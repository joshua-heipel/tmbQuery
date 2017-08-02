#' Create a vector space model.
#'
#' This function creates a term document matrix from the given string vector.
#' @param text, string or vector with strings
#' @param ..., other parameters inquired as list "control" by the function
#' tm::TermDocumentMatrix()
#' @keywords vector_space_model term_document_matrix text_mining
#' @export
#' @examples
#' CreateVSM(c("first sentence", "second sentence", "one more"))

CreateVSM <- function(text, ...) {
  # available options: tokenize, tolower, language, removePunctuation,
  # removeNumbers, stopwords, stemming, dictionary, bounds, wordLengths,
  # weighting

  # set default options
  control <- list(tokenize = scan_tokenizer, tolower = TRUE)
  # parse optional arguments
  control <- modifyList(control, list(...))
  # create corpus
  corpus <- tm::VCorpus(tm::VectorSource(text))
  # create term-document matrix
  tdm <- tm::TermDocumentMatrix(corpus, control = control)
  return(tdm)
}
