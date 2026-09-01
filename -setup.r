## -------------------------------------------- ##
# Survey Code Setup
## -------------------------------------------- ##
# Purpose
## Do generally-useful setup stuff (esp. if necessary for multiple downstream scripts)
## !!! NOTE: this script should be `source`-able !!!

# Clear environment/collect garbage
rm(list = ls()); gc()

# Make needed folder(s)
dir.create(path = file.path("data"), showWarnings = FALSE)
dir.create(path = file.path("graphs"), showWarnings = FALSE)

# End ----
