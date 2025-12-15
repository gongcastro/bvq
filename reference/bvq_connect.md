# Authenticate in Google and formr

This function tries to log in to the formr API with the user-provided
password (argument `password`) or retrieving it from the global
environment (`FORMR_PWD` in .Renviron)

## Usage

``` r
bvq_connect(google_email = NULL, password = NULL)
```

## Arguments

- google_email:

  E-mail used in Google Drive account. If `NULL` (default), it is
  assumed to be the same as `formr_email`.

- password:

  Character string with the password to formr (`NULL` by default).

## Value

Logical. `TRUE` if Google and formr authentication was successful,
`FALSE` if authentication of any of the two failed.

## Examples

``` r
if (FALSE) { # \dontrun{
bvq_connect()
} # }
```
