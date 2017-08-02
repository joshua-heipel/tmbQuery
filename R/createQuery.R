#' Create a query vector.
#'
#' This function creates a vector with weights from the given querywords and
#' term-document matrix.
#' @param qtext, query-words (keywords) as string or vector with strings
#' @param terms, terms from a term-document matrix as vector with strings
#' @param stemming, determines if stemming should be applied to given keywords
#' @param language, language used for text stemming
#' @param FUN, function to apply weights to each term depending on the keywords
#' @param max.distance, maximum Levenshtein distance between a keyword and term
#' @param ..., other parameters inquired as by the function
#' @keywords create_query term_document_matrix text_mining
#' @export
#' @examples
#' createQuery("term", c("vector", "with", "terms"))
#' createQuery("term", c("vector", "with", "terms"), max.distance=1, partial=F)
#' createQuery("term", c("vector", "with", "terms"),
#'             FUN=function(x, y) rep(1, length(y)))

CreateQuery <- function(qtext, terms, stemming = TRUE, language = "german",
                        FUN = NA, max.distance = 0, ...) {
  # optional arguments (passed to adist): costs, fixed, partial, ignore.case

  # parse optional arguments
  opt_args <- modifyList(list(ignore.case = TRUE, partial = TRUE), list(...))

  # split input string by whitespace and transform to lower case
  keywords <- unlist(strsplit(qtext, " "))
  # stem keywords
  if (stemming) {
    keywords <- tm::stemDocument(keywords, language=language)
  }

  # default function to compare the similarity between keywords and given terms
  # (returns a single weight for each term)
  if (!is.function(FUN)) {
    FUN <- function(x, y) {
      dist <- do.call(adist, modifyList(opt_args, list(x = x, y = y)))
      dist <- apply(dist, 2, min)
      weights <- 1 - dist / nchar(y)
      if (max.distance >= 1) weights[dist > max.distance] <- 0
      if (max.distance < 1) weights[weights < 1 - max.distance] <- 0
      return(weights)
    }
  }

  values <- do.call(FUN, list(keywords, terms))

  # create query vector
  q_vec <- matrix(values, nrow = length(values))
  rownames(q_vec) <- terms
  colnames(q_vec) <- paste(toupper(qtext), collapse = " ")
  q_vec <- lsa::as.textmatrix(q_vec)
  return(q_vec)
}
