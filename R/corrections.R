#' Convert biological data
#'
#' @param labdat labdat file
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#'
#' @return converted values
#' @export
#'
#' @examples
convert_biocounts <- function(labdat){
  parm_list <- c("Blue Green Algae (x10^-3)",
                 "Crustaceans (x10^-3)",
                 "Flagellates (x10^-3)",
                 "Green Algae (x10^-3)",
                 "Nematodes (x10^-3)",
                 "Other (x10^-3)",
                 "Rotifers (x10^-3)")

  df <- labdat %>%
    mutate(result = ifelse(parameter %in% parm_list & result > 0,
                           result * 1000, result),
           result_flag = ifelse(grepl(" \\(x10\\^-3\\)", parameter),
                                "converted", result_flag),
           parameter = ifelse(parameter %in% parm_list,
                              gsub(" \\(x10\\^-3\\)", "", parameter),
                              as.character(parameter)))

  #df$result_flag[which(grepl(pattern = "\\(x10\\^-3\\)", x = df$parameter))] <- "converted"
  return(df)

}


#' Replace all values above and below detection limit
#'
#' @param labdat
#'
#' @return
#' @export
#'
#' @examples
replace_dl <- function(labdat){
  labdat <- convert_adl(labdat = labdat)
  labdat <- convert_bdl(labdat = labdat)
  labdat <- convert_ast(labdat = labdat)

  return(labdat)
}


#' Replace below detection limit values with
#'
#' @param labdat
#' @param replacement
#'
#' @return
#' @export
#'
#' @examples
convert_bdl <- function(labdat, replacement = "0"){
  labdat$result[which(grepl(pattern = "[<]", x = labdat$result))] <- replacement

  labdat$result_flag[which(grepl(pattern = "[<]",
                                 x = labdat$result_org))] <- "bdl"

  return(labdat)

}

#' Replace above detection limit values
#'
#' @param labdat
#' @param replacement
#'
#' @return
#' @export
#'
#' @examples
convert_adl <- function(labdat, replacement = NA){
  labdat$result[which(grepl(pattern = "[>]", x = labdat$result))] <- replacement
  labdat$result[which(grepl(pattern = "TNTC", x = labdat$result))] <- replacement

  labdat$result_flag[which(grepl(pattern = "[>]", x = labdat$result_org) |
                             grepl(pattern = "TNTC", x = labdat$result_org))] <- "adl"

  return(labdat)

}

convert_ast <- function(labdat){
  labdat$result <- gsub(pattern = "[*]", replacement = "", x = labdat$result)
  labdat$result_flag[which(grepl(pattern = "[*]", x = labdat$result_org))] <- "mod"

  return(labdat)

}
