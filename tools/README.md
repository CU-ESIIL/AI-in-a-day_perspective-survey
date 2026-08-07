## AIIAD Custom Functions

Each script in this folder contains a function (of the same name as the file) that is useful in at least two other scripts used elsewhere in this repository.

**All scripts here _should_ be `source`-able!** To quickly load these functions into your R environment, copy/paste the following code chunk into the relevant script(s).

```
# Load all custom functions
purrr::walk(.x = dir(path = file.path("tools"), 
    pattern = "*.r", full.names = TRUE),
  .f = ~ source(file = .x))
```
