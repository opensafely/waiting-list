#########################################
# This script contains custom functions  
#########################################


##### Rounding and redaction #####
rounding <- function(vars) {
  case_when(vars == 0 ~ 0,
            vars > 7 ~ round(vars / 5) * 5)
}