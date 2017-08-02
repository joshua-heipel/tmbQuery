#' Split text into seperate blocks.
#'
#' This function allows you to seperate a string or a vector with strings into 
#' single sentences.
#' @param documents, string or vector with strings to split
#' @param keep, defines special characters to be kept before splitting
#' @param split, defines characters used to split the given string(s)
#' @keywords split text
#' @export
#' @examples
#' SplitDocs("Text: to be splitted into <single> blocks!!!")

SplitDocs <- function(documents, keep=".?!:;_-", split="[.:;!?]") {
  
  # remove double entries
  documents <- unique(documents)
  # remove all special characters
  documents <- gsub(paste("[^[:alnum:]", keep, "]", sep=""), " ", documents)
  # remove more than one whitespace
  documents <- gsub("[[:blank:]]+", " ", documents)
  # split documents into sentences
  sentences <- strsplit(documents, split)
  # remove empty sentences
  sentences <- sapply(sentences, function(x) x[grep("[[:alpha:]]", x)])
  return(sentences)
}