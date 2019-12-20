#' @title Parse the arguments of a operating system call.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Parse the arguments of a operating system call.
#'
#' @param os_response A character. The operating system response to a call.
#' @return            A character
#' @export
#' @examples
#' \dontrun{
#' library(magrittr)
#' system2("gdalinfo", stdout = TRUE, stderr = TRUE) %>%
#'     parse_os_help()
#' }
parse_os_help <- function(os_response){
    value <- arg_raw <- keyvalue <- kv_split <- key <- minimum <- maximum <- NULL

    pat_empty <- "^(?![i\\s\\S])"
    pat_letters <- "[a-zA-Z]+"
    pat_no_squares <- "(\\[|\\]|\\*)"
    pat_option <- '\\[-+[a-zA-Z0-9-_`\\"=|/[.] ]+\\]\\*?'
    pat_star   <- "\\*"
    pat_whitespaces <- "\\s+"

    help_message <- os_response %>%
        stringr::str_c(collapse = ' ')

    id_to <- os_response %>%
        stringr::str_detect(pattern = pat_empty) %>%
        which()

    if (length(id_to) > 1) {
        id_to <- id_to %>%
            magrittr::extract(-1) %>%
            dplyr::first()
        help_message <- os_response %>%
            magrittr::extract(1:id_to) %>%
            stringr::str_c(collapse = ' ')
    }

    optionals <- help_message %>%
        stringr::str_extract_all(pattern = pat_option) %>%
        unlist() %>%
        tibble::enframe(name = NULL) %>%
        dplyr::rename(arg_raw = value) %>%
        dplyr::mutate(minimum = 0,
                      maximum = ifelse(stringr::str_detect(arg_raw, pattern = pat_star), Inf, 1),
                      keyvalue = stringr::str_remove_all(arg_raw, pattern = pat_no_squares),
                      kv_split = stringr::str_split(keyvalue, pattern = " "),
                      key   = vapply(kv_split, dplyr::first, character(1)),
                      value = vapply(kv_split, dplyr::nth, n = 2,  character(1))) %>%
        dplyr::select(key, value, minimum, maximum)

    last_flag <- help_message %>%
        stringr::str_locate_all(pattern = pat_option) %>%
        unlist() %>%
        dplyr::last()

    arguments <- help_message %>%
        stringr::str_sub(start = (last_flag + 1)) %>%
        trimws() %>%
        stringr::str_replace(pattern = pat_whitespaces, replacement = ' ') %>%
        stringr::str_split(pattern = ' ') %>%
        unlist() %>%
        tibble::enframe(name = NULL) %>%
        dplyr::rename(arg_raw = value) %>%
        dplyr::mutate(key = stringr::str_extract(arg_raw,
                                                 pattern = pat_letters),
                      value = NA,
                      minimum = 1,
                      maximum = ifelse(stringr::str_detect(arg_raw,
                                                           pattern = pat_star),
                                       Inf, 1)) %>%
        dplyr::select(key, value, minimum, maximum)

    optionals %>%
        dplyr::bind_rows(arguments) %>%
        dplyr::mutate(value = as.character(value),
                      minimum = as.integer(minimum)) %>%
        return()
}


#' @title Call the operating system.
#' @author Alber Sanchez, \email{alber.ipia@@inpe.br}
#' @description Call the operating system.
#'
#' @param command A length-one character. The command to call.
#' @param args    A character. The command's parameters.
#' @param stdout  A length-one character.
#' @param stderr  A length-one character.
#' @param dry_run A length-one logical. Print the function parameters and terminate.
#' @return A character
call_os <- function(command, args, stdout = "", stderr = "", dry_run = FALSE) {
    if (dry_run) {
        print(paste(c(command, args), collapse = " "))
        return(0)
    }

    tryCatch({
        system2(command = command, args = args, stdout = stdout, stderr = stderr)
    }, warning = function(msg) {
        paste(command, collapse = " ")
        paste(args, collapse = " ")
        paste(msg, collapse = " ")
    }, error = function(msg) {
        paste(command, collapse = " ")
        paste(args, collapse = " ")
        paste(msg, collapse = " ")
    })
}


.is_input_file_valid <- function(files){
    #TODO: This function can't handle HDF4 datasets.
    if(!any(grepl("HDF4_EOS", files))){
            if (any(vapply(files, is.null, logical(1))))
            return(FALSE)
        if (any(vapply(files, is.na, logical(1))))
            return(FALSE)
        if (!all(vapply(files, file.exists, logical(1))))
            return(FALSE)
    }else{
        stop("Cannot handle HDF4 datasets.")
    }
    return(TRUE)
}

.is_resolution_valid <- function(res){
    if (is.atomic(res))
        return(is.numeric(res) && length(res) == 2)
    if (is.list(res))
        return(vapply(res, .is_resolution_valid, logical(1)))
    return(FALSE)
}

.is_extent_valid <- function(ext){
    if (is.atomic(ext))
        return(is.numeric(ext) && length(ext) == 4)
    if (is.list(ext))
        return(vapply(ext, .is_extent_valid, logical(1)))
    return(FALSE)
}

