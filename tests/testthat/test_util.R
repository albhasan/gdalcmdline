context("Utilitary functions.")
library(gdalcmdline)

test_that("Parse gdalinfo 1.10.1", {
    gdalinfo_res <- c("", "FAILURE: No datasource specified.",
                      "Usage: gdalinfo [--help-general] [-mm] [-stats] [-hist] [-nogcp] [-nomd]",
                      "                [-norat] [-noct] [-nofl] [-checksum] [-proj4] [-mdd domain]*",
                      "                [-sd subdataset] datasetname")
    expected <- tibble::tribble(
        ~key,          ~value,    ~minimum, ~maximum,
        "--help-general", NA,               0,       1,
        "-mm",            NA,               0,       1,
        "-stats",         NA,               0,       1,
        "-hist",          NA,               0,       1,
        "-nogcp",         NA,               0,       1,
        "-nomd",          NA,               0,       1,
        "-norat",         NA,               0,       1,
        "-noct",          NA,               0,       1,
        "-nofl",          NA,               0,       1,
        "-checksum",      NA,               0,       1,
        "-proj4",         NA,               0,       1,
        "-mdd",           "domain",         0,     Inf,
        "-sd",            "subdataset",     0,       1,
        "datasetname",    NA,               1,       1
    ) %>%
    dplyr::mutate(value = as.character(value),
                  minimum = as.integer(minimum))
    testthat::expect_equal(parse_os_help(gdalinfo_res), expected)
})

test_that("Parse gdalwarp 1.10.1", {

    gdalwarp_res <- c(
        "",
        "FAILURE: No target filename specified.",
        "Usage: gdalwarp [--help-general] [--formats]",
        "    [-s_srs srs_def] [-t_srs srs_def] [-to \"NAME=VALUE\"]",
        "    [-order n | -tps | -rpc | -geoloc] [-et err_threshold]",
        "    [-refine_gcps tolerance [minimum_gcps]]",
        "    [-te xmin ymin xmax ymax] [-tr xres yres] [-tap] [-ts width height]",
        "    [-wo \"NAME=VALUE\"] [-ot Byte/Int16/...] [-wt Byte/Int16]",
        "    [-srcnodata \"value [value...]\"] [-dstnodata \"value [value...]\"] -dstalpha",
        "    [-r resampling_method] [-wm memory_in_mb] [-multi] [-q]",
        "    [-cutline datasource] [-cl layer] [-cwhere expression]",
        "    [-csql statement] [-cblend dist_in_pixels] [-crop_to_cutline]",
        "    [-of format] [-co \"NAME=VALUE\"]* [-overwrite]",
        "    [-nomd] [-cvmd meta_conflict_value]",
        "    srcfile* dstfile",
        "",
        "Available resampling methods:",
        "    near (default), bilinear, cubic, cubicspline, lanczos, average, mode."
    )
    expected <- tibble::tribble(
        ~key,              ~value,              ~minimum,  ~maximum,
        "--help-general",   NA,                       0,       1,
        "--formats",        NA,                       0,       1,
        "-s_srs",           "srs_def",                0,       1,
        "-t_srs",           "srs_def",                0,       1,
        "-to",              '\"NAME=VALUE\"',         0,       1,
        "-order",           "n",                      0,       1,
        "-et",              "err_threshold",          0,       1,
        "-te",              "xmin",                   0,       1,
        "-tr",              "xres",                   0,       1,
        "-tap",             NA,                       0,       1,
        "-ts",              "width",                  0,       1,
        "-wo",              '\"NAME=VALUE\"',         0,       1,
        "-ot",              "Byte/Int16/...",         0,       1,
        "-wt",              "Byte/Int16",             0,       1,
        "-r",               "resampling_method",      0,       1,
        "-wm",              "memory_in_mb",           0,       1,
        "-multi",           NA,                       0,       1,
        "-q",               NA,                       0,       1,
        "-cutline",         "datasource",             0,       1,
        "-cl",              "layer",                  0,       1,
        "-cwhere",          "expression",             0,       1,
        "-csql",            "statement",              0,       1,
        "-cblend",          "dist_in_pixels",         0,       1,
        "-crop_to_cutline", NA,                       0,       1,
        "-of",              "format",                 0,       1,
        "-co",              '\"NAME=VALUE\"',         0,     Inf,
        "-overwrite",       NA,                       0,       1,
        "-nomd",            NA,                       0,       1,
        "-cvmd",            "meta_conflict_value",    0,       1,
        "srcfile",          NA,                       1,     Inf,
        "dstfile",          NA,                       1,       1
    ) %>%
    dplyr::mutate(value = as.character(value),
                  minimum = as.integer(minimum))
    testthat::expect_equal(parse_os_help(gdalwarp_res), expected)
})

