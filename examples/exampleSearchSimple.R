################################################################################
# Author: Joshua Heipel <joshua.heipel@gmx.de>
################################################################################

# LOAD PACKAGES ################################################################

# check and install packages (needs devtools to install via github)
# installed <- row.names(installed.packages())
# if (!"devtools" %in% installed) install.packages("devtools")
# library(devtools)
# 
# if (!"tamboRapi" %in% installed) install_github("tambora-org/tamboRapi")
# if (!"tmbQuery" %in% installed) install_github("joshua-heipel/tmbQuery")

library(tamboRapi)
library(tmbQuery)

# READ DATA FROM TAMBORA #######################################################

# reads data from project 1853 (See https://github.com/tambora-org/tamboRapi for
# details about Tambora and how to use the function tamboRapi::fromTambora
data <- fromTambora("g[pid]=1853")

# set keywords to search for
querywords <- c("Frost", "minus", "Temperatur", "kalt", "eisig")

# MAIN PROGRAMM ################################################################

# split documents into sentences (by punctuation)
sentences <- SplitDocs(data$text)

# create vector space model (term-document matrix) from all sentences
vsm <- CreateVSM(unlist(sentences), language="de", stemming=F)

# create query vector (function returns the similarity between all word pairs 
# based on the letters of them as vector)
q <- CreateQuery(querywords, row.names(vsm), stemming=F)

# search for sentences with semantic similarities to the given set of keywords
# ("spec" defines the weighting of the term-document matrix used by the function
# tm::weightSMART)
ranks <- LatentSemanticSearch(vsm, q, spec="nnn", dims=50)

# View the 10 most similar sentences with the given keywords
results <- cbind(unlist(sentences), round(ranks,2))
View(results[order(ranks, decreasing=T),][1:10,])
