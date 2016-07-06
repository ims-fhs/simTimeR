dict <- function() {
  dict_g2e <- setNames(
    c("Sun", "Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Jan", "Feb", "Mar",
      "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
    c("So", "Mo", "Di", "Mi", "Do", "Fr", "Sa", "Jan", "Feb", "Mar",
      "Apr", "Mai", "Jun", "Jul", "Aug", "Sep", "Okt", "Nov", "Dez"))

  dict_e2g <- setNames(
    c("So", "Mo", "Di", "Mi", "Do", "Fr", "Sa", "Jan", "Feb", "Mar", "Apr",
      "Mai", "Jun", "Jul", "Aug", "Sep", "Okt", "Nov", "Dez"),
    c("Sun", "Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Jan", "Feb", "Mar", "Apr",
      "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
  return(list(g2e = dict_g2e, e2g = dict_e2g))
}

int_dict <- function() {
  dict_int2e  <- setNames(
    c("Sun", "Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Jan", "Feb", "Mar",
      "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
    c("07", "01", "02", "03", "04", "05", "06", "01", "02", "03",
      "04", "05", "06", "07", "08", "09", "10", "11", "12"))

  dict_e2int <- setNames(
    c("07", "01", "02", "03", "04", "05", "06", "01", "02", "03",
      "04", "05", "06", "07", "08", "09", "10", "11", "12"),
    c("Sun", "Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Jan", "Feb", "Mar",
      "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
  return(list(e2int = dict_e2int, int2e = dict_int2e))
}

g2e <- function(ger_expr) {
  if (!exists("dict_g2e")) {
    # dictg_g2e <- dict()$g2e
    assign("dict_g2e", dict()$g2e, envir = .GlobalEnv)
  }
  res <- character(length(ger_expr))
  for (i in 1:length(ger_expr)) {
    split <- unlist(strsplit(ger_expr[i], split = ", "))
    assertthat::assert_that(all(split %in% names(dict_g2e)))
    if (length(split) == length(ger_expr[i])) {
      res[i] <- dict_g2e[split]
    } else {
      res[i] <- paste(dict_g2e[split], collapse = ", ")
    }
  }
  return(res)
}

e2g <- function(eng_expr) {
  if (!exists("dict_e2g")) {
    # dictg_e2g <- dict()$e2g
    assign("dict_e2g", dict()$e2g, envir = .GlobalEnv)
  }
  res <- character(length(eng_expr))
  for (i in 1:length(eng_expr)) {
    split <- unlist(strsplit(eng_expr[i], split = ", "))
    assertthat::assert_that(all(split %in% names(dict_e2g)))
    if (length(split) == length(eng_expr[i])) {
      res[i] <- dict_e2g[split]
    } else {
      res[i] <- paste(dict_e2g[split], collapse = ", ")
    }
  }
  return(res)
}



