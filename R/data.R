#' SimNetR-Network Data
#'
#' A dataset containing the transcriptional network inferred using SCENIC pipeline on murine TILs transcriptomics.

#'
#' @format A dataframe encoding the network in 5 columns:'Source','Interaction','Target','Weights' and 'String'; the latter includes the content of 'Source','Interaction' and 'Target' space separated.
#' @examples
#' network
"network"


#' SimNetR-Phenotypes Data
#'
#' A dataset containing several network stable states

#'
#' @format A dataframe containing network states, with nodes in raws and phenotypes in columns
#' @examples
#' phenotypes
"phenotypes"
