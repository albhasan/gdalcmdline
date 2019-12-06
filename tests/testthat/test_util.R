context("Utilitary functions.")
library(gdalcmdline)

test_that("parse_os_help parses OS's call", {
    gdalinfo_res <- c("", "FAILURE: No datasource specified.",
                      "Usage: gdalinfo [--help-general] [-mm] [-stats] [-hist] [-nogcp] [-nomd]",
                      "                [-norat] [-noct] [-nofl] [-checksum] [-proj4] [-mdd domain]*",
                      "                [-sd subdataset] datasetname")
    res <- parse_os_help(gdalinfo_res)
    expected <- tibble::tribble(
        ~key,          ~value,    ~minimum, ~maximum,
        "mm",          NA,               0,       1,
        "stats",       NA,               0,       1,
        "hist",        NA,               0,       1,
        "nogcp",       NA,               0,       1,
        "nomd",        NA,               0,       1,
        "norat",       NA,               0,       1,
        "noct",        NA,               0,       1,
        "nofl",        NA,               0,       1,
        "checksum",    NA,               0,       1,
        "mdd",         "domain",         0,     Inf,
        "sd",          "subdataset",     0,       1,
        "datasetname", NA,               1,       1
    ) %>%
    dplyr::mutate(value = as.character(value),
                  minimum = as.integer(minimum))

    testthat::expect_equal(res, expected)
})

