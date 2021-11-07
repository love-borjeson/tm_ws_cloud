#clean slate...:
rm(list = ls())

library(udpipe)

Eng_model <- udpipe_download_model(language = "english") #Just specify the language of your data here.
#UDpipe have more than 60 languages available. Awesome.

Eng_model <- udpipe_load_model(Eng_model$file_model) #and load to be used in this session

testtext <- read.csv("SmallTest.csv", stringsAsFactors=FALSE)
testtext

z <- udpipe_annotate(Eng_model, x = testtext$text, doc_id = testtext$doc_id)
z <- as.data.frame(z)
View(z)
#Handles twitter better than Shakespeare, apparently.
rm(z)

x <- readRDS("annotated_jokes.rds") #pre-annotated our joke sub-sample to save time...

x$topic_level_id <- unique_identifier(x, fields = c("doc_id", "paragraph_id", "sentence_id"))

# Clean the text from punctuation and numbers, remove apostrophes and lowercase
library(textclean)
x$lemma <- strip(x$lemma, char.keep = c(), digit.remove = T,
                 apostrophe.remove = T, lower.case = T) 

## Keep keyword or just plain nouns, adj or adv
#REF http://hackage.haskell.org/package/rake:
#Rapid Automatic Keyword Extraction (RAKE) is an algorithm (well-known and widely used) to automatically extract keywords from documents.
#Keywords are sequences of one or more words that, together, provide a compact representation of content.
#But hey! Isn't that what topic modeling is supposed to do? True.
#We do it to reduce the dimensionality a bit by representing common bigrams as one word.
keyw_rake2 <- keywords_rake(x, 
                            term = "lemma", group = c("sentence_id"),
                            relevant = x$upos %in% c("NOUN", "ADJ", "VERB"),
                            ngram_max = 3, n_min = 5) #Relevant: nouns, adjectives and verbs. Optionally, include all open classes
#n_min may have to be increased when modeling on a larger corpus

keyw_rake2 <- subset(keyw_rake2, ngram > 1) #only keep keywords if they are > 1 word.

# Recode terms to keywords: c("New", "York") will be c("New York", NA)
x$term <- x$lemma #Let's model on lemmas if not kw.
x$term <- txt_recode_ngram(x$term, 
                           compound = keyw_rake2$keyword, ngram = keyw_rake2$ngram)

x$term <- ifelse(x$upos %in% c("NOUN", "ADJ", "VERB"), x$term,
                 ifelse(x$term %in% c(keyw_rake2$keyword), x$term, NA)) #Include lemmas of NOUN, ADJ, VERB and kw. Otherwise NA.
#Cf https://universaldependencies.org/u/pos/index.html

dtf <- document_term_frequencies(x, document = "doc_id", term = "term") #model on docs.
dtf$term <- gsub(" ", "_", dtf$term, fixed = TRUE) #Whitestrips to underscore
dtf$term <- gsub('\\b\\w{1,2}\\b','',dtf$term) #word lengths
dtf$term <- gsub('\\b\\w{150,}\\b','',dtf$term) #word lengths, longer since we have keywords
dtf$term <- gsub(" ", "", dtf$term, fixed = TRUE) #Whitestrips removal
dtf <- dtf[!(is.na(dtf$term) | dtf$term==""), ] #remove empty rows

# How do we get rid of overly common and uncommon words?
# TF-IDF = term frequency - (log scaled) inverse document frequency,
# can be used to filter away very infrequent words AND words frequent in many docs
# https://en.wikipedia.org/wiki/Tf%E2%80%93idf#Inverse_document_frequency
dgCM_dtm <- document_term_matrix(x = dtf) #make a non-zero dtm
dtm_tfidfVector <- as.data.frame(dtm_tfidf(dgCM_dtm)) #check out the vector
View(dtm_tfidfVector)
# Here we keep only the words that have tf-idf in the top 50% quantile
# There are other possibilities to filter according to tf-idf, see here 
# https://www.rdocumentation.org/packages/udpipe/versions/0.8.6/topics/dtm_remove_tfidf
dgCM_dtm <- dtm_remove_tfidf(dgCM_dtm, prob=0.5) 
saveRDS(dgCM_dtm, file = "dtmKW.rds")

library(topicmodels)
k = 24

#FOR REFERENCE!
#Suggested default settings of the sampler for a full-blown proper model
controlGibbs <- list(#alpha is the numeric prior for document-topic multinomial distribution,
  #i.e. smaller alpha means fewer topics per document.
  #Starting value for alpha is 50/k as suggested by Griffiths and Steyvers (2004).
  alpha = 50/k,                   
  estimate.beta = TRUE, #Save logarithmized parameters of the term distribution over topics.
  #Not a prior in 'topicmodels'! See 'delta' below.
  verbose = 0, #no information is printed during the algorithm
  save = 0, #no intermediate results are saved
  keep = 0, #the log-likelihood values are stored every 'keep' iteration. For diagnostic purposes.
  seed = list(5683, 123, 8972, 7, 9999), #seed needs to have the length nstart.
  nstart = 5, #no of independent runs
  best = TRUE, #only the best model over all runs with respect to the log-likelihood is returned.
  #Default is true. But read first:
  #http://cs.colorado.edu/~jbg/docs/2014_emnlp_howto_gibbs.pdf
  delta = 0.1, #numeric prior for topic-word multinomial distribution. 
  #The default 0.1 is suggested in Griffiths and Steyvers (2004).
  #Also, 'delta', ususally referred to as 'beta'. Yes. Confusing.
  #Decreasing 'Delta' ('beta'), e.g. from 0.1 to 0.001 will increase the granularity of the model.
  #If topics appear to be to general and hard to distinguish, manipulating 'delta' (lowering its value) 
  #could be a strategy. If topics are too particular, go the opposite way.
  iter = 2000, #>1000
  burnin = 5, #>200, to throwaway the first inaccurate samples. Default set to 0.
  thin = 2000) #Default = iter, that is to say we only need the stationary state of the last iteration.
  #setting iter=thin is sometimes disputed, optionally thin to have >10 samples.
  #Following Griffiths and Steyvers (2004).

#The priors above, especially alpha and delta (beta), are our guesses on properties of data.
#We typically inherit these guesses from someone  else, i.e. we use standard values suggested in a paper somewhere.
#Priors are nonetheless slippery since they can have large effect on what we "find": finding is thus to some extent "shaping".
#TM in other words, relies heavily on the integrity of the researcher. The quality of your TM thus relies on YOU!
#Transparency vis-a-vis your methodological choices are crucial.

#However, for the purpose of this lab..:
#a simpler life and and a simpler sampler, as before
controlGibbs <- list(seed = 5683, #what does this mean?
                     burnin = 200,
                     iter = 500)

model4 <- LDA(dgCM_dtm, k, method = "Gibbs", control = controlGibbs) #Model...
terms(model4,10) #Looks ok.
# To fine-tune and add a (short) stoplist, use LDAvis as before.
# Here we add words that we think don't add interpretability to our topics.
dgCM_dtm <- dtm_remove_terms(dgCM_dtm, terms = c("but", "only", "new"))  
#remodel.....
model4 <- LDA(dgCM_dtm, k, method = "Gibbs", control = controlGibbs) 
# and then visualize. 
# Notice how the size (and spread) of topics (in the MDS) is much more even than before.
# We can now say that the model is symmetric, which is a good thing.
topicmodels2LDAvis <- function(x, ...){
  post <- topicmodels::posterior(x)
  if (ncol(post[["topics"]]) < 3) stop("The model must contain > 2 topics")
  mat <- x@wordassignments
  LDAvis::createJSON(
    phi = post[["terms"]], 
    theta = post[["topics"]],
    vocab = colnames(post[["terms"]]),
    doc.length = slam::row_sums(mat, na.rm = TRUE),
    term.frequency = slam::col_sums(mat, na.rm = TRUE)
  )
}

library(LDAvis)
serVis(topicmodels2LDAvis(model4))
servr::daemon_stop(1)

#But what about K?

