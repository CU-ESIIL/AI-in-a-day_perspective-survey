#!/usr/bin/env python3
"""Recode TechSkill_Interest into compact, analysis-ready categories.

Usage:
    python categorize_techskill_interest.py [INPUT_CSV] [OUTPUT_CSV]
        [--long-output LONG_OUTPUT_CSV]

Input file:
    INPUT_CSV is the survey CSV to process. It must contain a column named
    ``TechSkill_Interest``. If omitted, the script reads
    ``01_tidied-responses_no-free-text.csv`` from the current directory.

Output file:
    OUTPUT_CSV is the path where the categorized CSV will be written. If
    omitted, the script writes
    ``01_tidied-responses_techskill-categories.csv`` in the current directory.
    An existing file at that path will be replaced. The output retains every
    input column and appends a generated ``Respondent_ID`` plus category and
    status columns.

Optional long-format file:
    Pass ``--long-output LONG_OUTPUT_CSV`` to also write one record for each
    unique substantive category selected by each respondent. This file retains
    the input columns, adds ``Respondent_ID``, and stores the single category
    represented by each record in ``TechSkill_Category``. Respondents with no
    substantive category remain in the regular output but have no long-format
    record. IDs are assigned from input row order as R0001, R0002, and so on.

Examples:
    python categorize_techskill_interest.py
    python categorize_techskill_interest.py survey.csv categorized-survey.csv
    python categorize_techskill_interest.py data/survey.csv results/categories.csv
    python categorize_techskill_interest.py --long-output techskills-long.csv
"""

import argparse
import csv
from collections import Counter
from pathlib import Path


FIELD = "TechSkill_Interest"
RESPONDENT_ID_FIELD = "Respondent_ID"
LONG_CATEGORY_FIELD = "TechSkill_Category"

CATEGORY_OPTIONS = {
    "Responsible AI": (
        "Responsible use of GenAI",
    ),
    "Prompting & assistants": (
        "Prompt Engineering",
        "Create custom GPTs, projects, or assistants",
        "Using a specialized AI coding assistant such as Codex or Claude Code",
    ),
    "Programming & APIs": (
        "Use GenAI through an API (e.g., accessing Gemini via its API through your IDE of choice)",
        "Write scripts that call GenAI models programmatically",
        "Develop AI-powered applications or tools",
    ),
    "Model access & infrastructure": (
        "Use open-source models",
        "Run a model locally on a personal computer",
        "Run models on institutional or cloud computing resources",
    ),
    "Customization & grounding": (
        "Build Retrieval-Augmented Generation (RAG) systems",
        "Fine-tune or customize GenAI models",
    ),
    "Integration & automation": (
        "Use GenAI tools to connect to external databases",
        "Use GenAI agents that can complete multi-step tasks",
        "Connect GenAI tools to external software",
    ),
}

STATUS_OPTIONS = {
    "Other": "Other",
    "Not sure": "Not sure",
}


def selected_options(value: str) -> set[str]:
    """Identify selections without splitting labels that contain commas."""
    selections = set()
    for options in CATEGORY_OPTIONS.values():
        selections.update(option for option in options if option in value)
    selections.update(option for option in STATUS_OPTIONS if option in value)
    return selections


def categorize(value: str) -> tuple[list[str], set[str]]:
    selections = selected_options(value)
    categories = [
        category
        for category, options in CATEGORY_OPTIONS.items()
        if any(option in selections for option in options)
    ]
    return categories, selections


def column_name(label: str) -> str:
    normalized = label.lower().replace("&", "and")
    return "TechSkill_" + "_".join(normalized.split())


def recode(
    input_path: Path,
    output_path: Path,
    long_output_path: Path | None = None,
) -> None:
    with input_path.open(newline="", encoding="utf-8-sig") as source:
        reader = csv.DictReader(source)
        if reader.fieldnames is None or FIELD not in reader.fieldnames:
            raise ValueError(f"Required field {FIELD!r} was not found")
        if RESPONDENT_ID_FIELD in reader.fieldnames:
            raise ValueError(
                f"Input already contains generated field {RESPONDENT_ID_FIELD!r}"
            )
        rows = list(reader)
        original_fields = reader.fieldnames

    category_columns = [column_name(category) for category in CATEGORY_OPTIONS]
    status_columns = [column_name(status) for status in STATUS_OPTIONS]
    output_fields = original_fields + [
        RESPONDENT_ID_FIELD,
        "TechSkill_Categories",
        *category_columns,
        *status_columns,
        "TechSkill_Missing",
    ]
    long_rows = []

    category_counts: Counter[str] = Counter()
    status_counts: Counter[str] = Counter()

    for row_number, row in enumerate(rows, start=2):
        value = (row.get(FIELD) or "").strip()
        categories, selections = categorize(value)
        row[RESPONDENT_ID_FIELD] = f"R{row_number - 1:04d}"

        recognized_text = value
        known_options = {
            option
            for options in CATEGORY_OPTIONS.values()
            for option in options
        } | set(STATUS_OPTIONS)
        for option in sorted(known_options, key=len, reverse=True):
            recognized_text = recognized_text.replace(option, "")
        if recognized_text.strip(", "):
            raise ValueError(
                f"Unrecognized TechSkill_Interest content on CSV row {row_number}: "
                f"{recognized_text.strip(', ')!r}"
            )

        row["TechSkill_Categories"] = "; ".join(categories)
        for category, output_column in zip(CATEGORY_OPTIONS, category_columns):
            selected = category in categories
            row[output_column] = int(selected)
            category_counts[category] += selected
        for status, output_column in zip(STATUS_OPTIONS, status_columns):
            selected = status in selections
            row[output_column] = int(selected)
            status_counts[status] += selected
        row["TechSkill_Missing"] = int(not value)
        status_counts["Missing"] += not value

        if long_output_path is not None:
            for category in categories:
                long_rows.append(
                    {
                        **{field: row[field] for field in original_fields},
                        RESPONDENT_ID_FIELD: row[RESPONDENT_ID_FIELD],
                        LONG_CATEGORY_FIELD: category,
                    }
                )

    with output_path.open("w", newline="", encoding="utf-8") as destination:
        writer = csv.DictWriter(destination, fieldnames=output_fields)
        writer.writeheader()
        writer.writerows(rows)

    if long_output_path is not None:
        long_fields = original_fields + [RESPONDENT_ID_FIELD, LONG_CATEGORY_FIELD]
        with long_output_path.open("w", newline="", encoding="utf-8") as destination:
            writer = csv.DictWriter(destination, fieldnames=long_fields)
            writer.writeheader()
            writer.writerows(long_rows)

    print(f"Wrote {len(rows)} rows to {output_path}")
    if long_output_path is not None:
        print(f"Wrote {len(long_rows)} rows to {long_output_path}")
    for category in CATEGORY_OPTIONS:
        print(f"{category}: {category_counts[category]}")
    for status in (*STATUS_OPTIONS, "Missing"):
        print(f"{status}: {status_counts[status]}")


def main() -> None:
    parser = argparse.ArgumentParser(
        description=__doc__,
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    parser.add_argument(
        "input",
        nargs="?",
        type=Path,
        default=Path("01_tidied-responses_no-free-text.csv"),
        help="input survey CSV (default: %(default)s)",
    )
    parser.add_argument(
        "output",
        nargs="?",
        type=Path,
        default=Path("01_tidied-responses_techskill-categories.csv"),
        help="output categorized CSV (default: %(default)s)",
    )
    parser.add_argument(
        "--long-output",
        type=Path,
        metavar="LONG_OUTPUT_CSV",
        help="also write one row per respondent and substantive category",
    )
    args = parser.parse_args()
    recode(args.input, args.output, args.long_output)


if __name__ == "__main__":
    main()