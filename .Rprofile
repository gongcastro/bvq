if (interactive()) {
  suppressWarnings(require(usethis, quietly = TRUE))
  suppressWarnings(require(devtools, quietly = TRUE))

  cat("\014") # clear screen
  cli::cli_text("")
  cli::cli_alert_info(R.version$version.string)
  cli::cli_text("")

  # usethis options
  options(usethis.protocol = "ssh")
  options(usethis.full_name = "gongcastro")

  # bias against scientific notation
  options(scipen = 4)

}
