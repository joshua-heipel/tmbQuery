% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/latentSemanticSearch.R
\name{LatentSemanticSearch}
\alias{LatentSemanticSearch}
\title{Perform a latent semantic analysis.}
\usage{
LatentSemanticSearch(tdm, qvec, spec = "nnn", dims = lsa::dimcalc_share(),
  ...)
}
\arguments{
\item{tdm, }{term-document matrix as returned by the function createVSM()}

\item{qvec, }{query vector as returned by the function createQuery()}

\item{spec, }{determines weighting used by the function tm::weightSMART()}

\item{dims, }{rank of the appoximation (integer value)}
}
\description{
This function compares the similarity between a low rank approximation of a
term-document matrix and and a given query vector. (See e.g. "18 - Matrix
decompositions and latent semantic indexing" in "Manning, S.D. et al.
(2009): An Introduction to Information Retrieval. Cambridge University
Press." for matematical details:
https://nlp.stanford.edu/IR-book/pdf/irbookprint.pdf )
}
\keyword{latent_semantic_analysis}
\keyword{latent_semantic_indexing}
\keyword{singular_value_decomposition}
\keyword{term_document_matrix}
\keyword{text_mining}
