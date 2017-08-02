#' Perform a latent semantic analysis.
#'
#' This function compares the similarity between a low rank approximation of a
#' term-document matrix and and a given query vector. (See e.g. "18 - Matrix
#' decompositions and latent semantic indexing" in "Manning, S.D. et al.
#' (2009): An Introduction to Information Retrieval. Cambridge University
#' Press." for matematical details:
#' https://nlp.stanford.edu/IR-book/pdf/irbookprint.pdf )
#' @param tdm, term-document matrix as returned by the function createVSM()
#' @param qvec, query vector as returned by the function createQuery()
#' @param spec, determines weighting used by the function tm::weightSMART()
#' @param dims, rank of the appoximation (integer value)
#' @keywords singular_value_decomposition latent_semantic_analysis
#' latent_semantic_indexing term_document_matrix text_mining
#' @export
#' @examples

LatentSemanticSearch <- function(tdm, qvec, spec="nnn",
                                 dims=lsa::dimcalc_share(), ...) {
  # available options: slope, pivot, alpha

  # weight term-document matrix
  tdm <- tm::weightSMART(tdm, spec = spec, control = list(...))
  # singular value decomposition
  SVD <- lsa::lsa(tdm, dims = dims)
  # compare query vector with rank-k approximation of the term-document matrix
  ranking <- (t(qvec) %*% SVD$tk * SVD$sk) %*% t(SVD$dk)

  return(as.numeric(ranking))
}
