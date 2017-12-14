library("devtools")

revdep_check(recursive = TRUE)
revdep_check_save_summary()
revdep_check_print_problems()

print(readRDS(paste0(getwd(), "/revdep/checks.rds")))
