fmt_pct_special <- function(x){
  if(!is.vector(x = x, mode = "numeric")){
    cli::cli_abort("Argument {.arg x} has to be a numeric vector")
  }
  if(any(x > 1, na.rm = TRUE)){
    cli::cli_abort("One or more values in {.arg x} are >1")
  }
  rlang::check_installed("scales", "to format numerical strings.")
  # allocate prefix and accuracy vectors
  prefix <- vector("character", length = length(x))
  accuracy <- vector("numeric", length = length(x))
  # we want to show <1% for every value <0.01, unless very close to 0.
  # To avoid cases of <0%, we need to change those values to something
  # close to 0.01.
  x[x < 0.01 & x > sqrt(.Machine$double.eps)] <- 0.009
  # Analog rounding close to 0 above
  x[x > 0.999 & 1 - x > sqrt(.Machine$double.eps)] <- 0.9991
  # Don't round percentages >99.5 to 100% automatically
  accuracy <- ifelse(x >= 0.995, 0.1, 1)
  # create prefixes for <1% and >99.9%
  prefix[x < 0.01 & x > sqrt(.Machine$double.eps)] <- "<"
  prefix[x > 0.999] <- ">"
  # remove prefix and change accuracy if x is VERY close to 100%
  prefix[1 - x <= sqrt(.Machine$double.eps)] <- ""
  accuracy[1 - x <= sqrt(.Machine$double.eps)] <- 1
  # catch NAs as scales will fail otherwise
  prefix[is.na(prefix)] <- ""
  accuracy[is.na(accuracy)] <- 1
  # now format x using scales
  scales::number(
    x,
    accuracy = accuracy,
    scale = 100,
    prefix = prefix,
    suffix = "%"
  )
}

saveRDS(fmt_pct_special, 'Simulation Backup/Functions/fmt_pct_special.rds')

gt_fmt_pct_special <- function(gt, columns, ...){
  gt::text_transform(
    gt,
    locations = gt::cells_body(columns = {{ columns }}),
    fn = function(x){
      # avoid "NAs introduced by coercion" warning
      x[x == "NA"] <- NA_character_
      x <- as.double(x)
      fmt_pct_special(x)
    }
  )
}

saveRDS(gt_fmt_pct_special, 'Simulation Backup/Functions/gt_fmt_pct_special.rds')
