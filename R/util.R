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

