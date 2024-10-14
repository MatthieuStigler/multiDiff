#' Dataset from Gentzkow et al (2011)
#'
#' This is the replication dataset put by Reed (2019)
#'
#'
#' @name GentzkowData
#' @docType data
#' @format A data-frame
#' @keywords datasets
#' @references Reed, W. Robert, 2019, "Replication Data for: "GUEST BLOG at TRN: EIR - Heterogeneity in Two-Way Fixed Effects Models"", https://doi.org/10.7910/DVN/EGMLQG, Harvard Dataverse, V2, UNF:6:r9g/aJRpVkv2rxw3F3J7QQ==
#' @examples
#' library(multiDiff)
#' data(GentzkowData)
#' GentzkowData
#'
#' ## The FE from R is slightly different,
#' ## they use some strange formulation of fixed effects in their STATA areg code
#' library(lfe)
#' reg_FE_lfe <- felm(prestout ~numdailies|cnty90 + styr, data = GentzkowData)
#' reg_FE_lfe
"GentzkowData"


