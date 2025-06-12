
###### IMAGE ANALYSES #############
# THE AIM OF THE SCRIPT IS TO AGGREGATE ALL KIND OF IMAGE ANALYSES AND BRING THEM TOGHETHER 
# caveat, is that we still do not know the accuracy for each different kind of image processing,and for now we do not have all the elaborations 
## 1) I will need to reimport all the image descriptions, for all the different methodologies 
## 3) perform the sentiment on flicker descriptions
## 2) aggregate them geographycally at a data zone level. 

## PREPARING SENTIMENT 
## packages
library(tm)
library(XML)
library(jsonlite )
library(tm)
library(Matrix)  # For row_sums function
library(textmineR)
library(text2vec)
library(gridExtra)
library(grid)
library(textstem)
library(rgeos)
library(textstem)
library(leaflet)
library(sf)
library(geosphere)
library(tidytext)
library(vader)
library(wordcloud)
library(RColorBrewer)
library(tm)
library(topicmodels)
library(reticulate)
library(dplyr)
library(stringr)
library(sp)
library(spatial)
library(rgdal)
library(raster)
library(GGally)
library(betareg)
library(htmlwidgets)
library(magick)
library(tesseract)
library(exifr)
library(tm)
library(proxy)
library(stringdist)
library(NLP)
#library(textmineR)
library(SnowballC)
library(emoji)

library(reticulate)
library(rgdal)

### functions for sentiment 
### definingt he three function to work on the lapply
preprocess_corpus_stem <-function(text) {
  if (length(text) >= 1){
    # text<-c("person")
    # text<-c("tv")
    
    #text<-TW_obj[TW_obj$image_path %in% tweets_4_hash_clean1que[tweets_4_hash_clean1que$query=="climate change","Media_url"]  & TW_obj$method=="FasterRCNN" & TW_obj$score>=0.75,"label" ] 
    corpus<-Corpus(VectorSource(text))
    corpus <- tm_map(corpus, tolower)
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, removeNumbers)
    corpus <- tm_map(corpus, removeWords, stopwords("english"))
    corpus <- tm_map(corpus, stripWhitespace)
    
    corpus <- TermDocumentMatrix(corpus)
    corpus <- as.matrix(corpus)
    
    # Apply stemming to the terms in the matrix
    corpus <- corpus
    rownames(corpus) <- SnowballC::wordStem(rownames(corpus), "english")
    
    # Get the frequency of terms
    corpus <- rowSums(corpus)
    
    # Create a data frame with stemmed terms and their frequencies
    corpus <- data.frame(word = names(corpus), freq = corpus)
    
    # Aggregate term frequencies for duplicate terms
    # if (sum(duplicated( corpus$word)) >= 1){
    
    try(corpus <- aggregate(freq ~ word, corpus, sum), silent=T)
    #}else { 
    # Sort by frequency in decreasing order
    corpus <- corpus[order(corpus$freq, decreasing = TRUE), ]
    
    return(corpus)
  }else { print("Not enough images with labels for labels analyses")
    a=data.frame(freq=NULL,word=NULL )
    return(a)}
}



preprocess_corpus_stem_resnet <-function(text) {
  if (length(text) >= 1){
    # text<-c("person")
    # text<-c("tv")
    
    # Sample vector of strings
    #vector_of_strings <- c("apple, banana, cherry", "orange, grape, pineapple", "pear, kiwi, mango")
    
    # Split each string by commas and extract the first element
    corpus <- sapply(strsplit(text, ","), function(x) trimws(x[1]))
    
    #text<-TW_obj[TW_obj$image_path %in% tweets_4_hash_clean1que[tweets_4_hash_clean1que$query=="climate change","Media_url"]  & TW_obj$method=="FasterRCNN" & TW_obj$score>=0.75,"label" ] 
    corpus<-Corpus(VectorSource(corpus))
    corpus <- tm_map(corpus, tolower)
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, removeNumbers)
    corpus <- tm_map(corpus, removeWords, stopwords("english"))
    corpus <- tm_map(corpus, stripWhitespace)
    
    corpus <- TermDocumentMatrix(corpus)
    corpus <- as.matrix(corpus)
    
    # Apply stemming to the terms in the matrix
    corpus <- corpus
    rownames(corpus) <- SnowballC::wordStem(rownames(corpus), "english")
    
    # Get the frequency of terms
    corpus <- rowSums(corpus)
    
    # Create a data frame with stemmed terms and their frequencies
    corpus <- data.frame(word = names(corpus), freq = corpus)
    
    # Aggregate term frequencies for duplicate terms
    # if (sum(duplicated( corpus$word)) >= 1){
    
    try(corpus <- aggregate(freq ~ word, corpus, sum), silent=T)
    #}else { 
    # Sort by frequency in decreasing order
    corpus <- corpus[order(corpus$freq, decreasing = TRUE), ]
    
    return(corpus)
  }else { print("Not enough images with labels for labels analyses")
    a=data.frame(freq=NULL,word=NULL )
    return(a)}
}

## load objects with a new name
loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}
# Function to extract date from filename and compare with cutoff
filter_files_before_date <- function(filenames, cutoff_date) {
  filtered_files <- sapply(filenames, function(filename) {
    # Extract date from filename using regular expressions
    matches <- regmatches(filename, regexec("\\d{4}-\\d{2}-\\d{2}", filename))
    file_date <- as.Date(matches[[1]])
    
    # Return filename if date is before cutoff, else return NA
    if (!is.na(file_date) && file_date < cutoff_date) {
      return(filename)
    } else {
      return(NA)
    }
  })
  
  # Remove NAs and return filtered filenames
  return(filtered_files[!is.na(filtered_files)])
}
library(jsonlite)

parseJSONSafely <- function(json_str) {
  # Check for NA or empty strings and return NULL or any placeholder you prefer
  if(is.na(json_str) || json_str == "") {
    return(NULL)
  }
  
  # Replace single quotes with double quotes for valid JSON format
  json_str <- gsub("'", "\"", json_str)
  
  # Use tryCatch to attempt JSON parsing, returning NULL on error
  parsed <- tryCatch({
    fromJSON(json_str)
  }, error = function(e) {
    # Optionally, log or print the error and problematic string
    # cat("Error parsing JSON:", json_str, "\n")
    NULL  # Return NULL or any placeholder in case of parsing error
  })
  
  return(parsed)
}

# Apply the function to each element in the column
#tweets_4_hash_clean1que$FasterRCNN_Labelsmore_07 <- lapply(tweets_4_hash_clean1que$FasterRCNN_Labels, parseJSONSafely)


LDA_inf <- function(text) {
  #text<-tweets_4_hash_clean1que[  ,"full_text.x"]
  #text<-TW_obj[TW_obj$image_path %in% tweets_4_hash_clean1que[tweets_4_hash_clean1que$Data_Zone==dz ,"Media_url"] & TW_obj$method=="Caption_blip" ,"text" ]
  if (length(gsub("(https?://\\S+\\s*|&amp|\\n\\n|\\n|\\n\\n\\n)", "", text, perl = TRUE))>=3){
    embeddings <- CreateTcm(doc_vec =  gsub("(https?://\\S+\\s*|&amp|\\n\\n|\\n|\\n\\n\\n)", "", text, perl = TRUE),
                            skipgram_window = Inf, ### 0 count the number of words shared by document i and j, while inf count the number of documents in which j and i occur together
                            stem_lemma_function = function(x) SnowballC::wordStem(x, "english"),
                            stopword_vec = c(stopwords::stopwords("en"), stopwords::stopwords(source = "smart")),
                            verbose = F,
                            cpus = 10) ### putting that to 0 return a list of numbers
    #### chat suggested a skipgram_window of 5, as tweets are short and of low coherence. howver i think a inf value can be better. 0 is not working though
    # than your average DTM
    embeddings <- FitLdaModel(dtm = embeddings,
                              k = 50,### integer number of topics
                              iterations = 1000,
                              burnin = 200,
                              alpha = 0.1,
                              beta = 0.05,
                              optimize_alpha = TRUE,
                              calc_likelihood = TRUE,
                              calc_coherence = TRUE,
                              calc_r2 = TRUE,
                              cpus = 10)
    
    print(embeddings$r2)
    plot(embeddings$log_likelihood$iteration,embeddings$log_likelihood$log_likelihood)
    hist(embeddings$coherence, 
         col= "blue", 
         main = "Histogram of probabilistic coherence")
    # get top terms, no labels because we don't have bigrams
    embeddings$top_terms <- GetTopTerms(phi = embeddings$phi,
                                        M = 7)
    embeddings$prevalence <- colSums(embeddings$theta) / sum(embeddings$theta) * 100
    plot(embeddings$prevalence, embeddings$alpha, xlab = "prevalence", ylab = "alpha") ## prevalence should be correlated to alpha, here i do not see this
    
    
    # Create a summary table, similar to the above
    embeddings$summary <- data.frame(topic = rownames(embeddings$phi),
                                     coherence = round(embeddings$coherence, 3),
                                     prevalence = round(colSums(embeddings$theta), 2),
                                     top_terms = apply(embeddings$top_terms, 2, function(x){
                                       paste(x, collapse = ", ")
                                     }),
                                     stringsAsFactors = FALSE) #not hashtags but text analyses
    #return(embeddings$summary)
    # Sort the summary data frame by prevalence in descending order
    #sorted_summary <- embeddings$summary[order(embeddings$summary$prevalence, decreasing = TRUE), ]
    embeddings <- embeddings$summary[order(embeddings$summary$prevalence, decreasing = TRUE), ]
    
    return(embeddings)
    # a<-embeddings$summary
  }else { print("Not enough tweets for LDA")
    a=data.frame(coherence=NULL,prevalence=NULL,top_terms=NULL )
    return(a)}
}

LDA <- function(text) {
  #text<-tweets_4_hash_clean1que[ tweets_4_hash_clean1que$Data_Zone==dz ,"full_text.x"]
  #text<-TW_obj[TW_obj$image_path %in% tweets_4_hash_clean1que[tweets_4_hash_clean1que$Data_Zone==dz ,"Media_url"] & TW_obj$method=="Text_gpt2" ,"text" ]
  # text=tweets_4_hash_clean1que[ tweets_4_hash_clean1que$query %in% que[3,1],"full_text.x"]
  if (length(gsub("(https?://\\S+\\s*|&amp|\\n\\n|\\n|\\n\\n\\n)", "", text, perl = TRUE))>=3){
    embeddings <- CreateTcm(doc_vec =  gsub("(https?://\\S+\\s*|&amp|\\n\\n|\\n|\\n\\n\\n)", "", text, perl = TRUE),
                            skipgram_window = 5,#5, ### 0 count the number of words shared by document i and j, while inf count the number of documents in which j and i occur together
                            stem_lemma_function = function(x) SnowballC::wordStem(x, "english"),
                            stopword_vec = c(stopwords::stopwords("en"), stopwords::stopwords(source = "smart")),
                            verbose = F,
                            cpus = 10) ### putting that to 0 return a list of numbers
    #### chat suggested a skipgram_window of 5, as tweets are short and of low coherence. howver i think a inf value can be better. 0 is not working though
    # than your average DTM
    embeddings <- FitLdaModel(dtm = embeddings,
                              k = 50,### integer number of topics ## with this to 5, it worked way better
                              iterations = 10000,
                              burnin = 200,
                              alpha = 0.1,
                              beta = 0.05,
                              optimize_alpha = TRUE,
                              calc_likelihood = TRUE,
                              calc_coherence = TRUE,
                              calc_r2 = TRUE,
                              cpus = 10)
    print(embeddings$r2)
    plot(embeddings$log_likelihood$iteration,embeddings$log_likelihood$log_likelihood)
    hist(embeddings$coherence, 
         col= "blue", 
         main = "Histogram of probabilistic coherence")
    # get top terms, no labels because we don't have bigrams
    embeddings$top_terms <- GetTopTerms(phi = embeddings$phi,
                                        M = 7)
    embeddings$prevalence <- colSums(embeddings$theta) / sum(embeddings$theta) * 100
    plot(embeddings$prevalence, embeddings$alpha, xlab = "prevalence", ylab = "alpha") ## prevalence should be correlated to alpha, here i do not see this
    
    # Create a summary table, similar to the above
    embeddings$summary <- data.frame(topic = rownames(embeddings$phi),
                                     coherence = round(embeddings$coherence, 3),
                                     prevalence = round(colSums(embeddings$theta), 2),
                                     top_terms = apply(embeddings$top_terms, 2, function(x){
                                       paste(x, collapse = ", ")
                                     }),
                                     stringsAsFactors = FALSE)#not hashtags but text analyses
    ## this was, like that, i changed to save memory
    #sorted_summary <- embeddings$summary[order(embeddings$summary$prevalence, decreasing = TRUE), ]
    embeddings <- embeddings$summary[order(embeddings$summary$prevalence, decreasing = TRUE), ]
    
    return(embeddings)
    # a<-embeddings$summary
  }else { print("Not enough tweets for LDA")
    a=data.frame(coherence=NULL,prevalence=NULL,top_terms=NULL )
    return(a)}
}
process_corpus_words <- function(text) {
  #rm(corpus)
  #TW_text <- Corpus(VectorSource(gsub("(https?://\\S+\\s*|&amp|\\n\\n|\\n|\\n\\n\\n)", "", (a[a$query==q,"fulltext"]), perl = TRUE)))
  #text<-tweets_4_hash_clean1que[  ,"full_text.x"]
  if (length(gsub("(https?://\\S+\\s*|&amp|\\n\\n|\\n|\\n\\n\\n)", "", text, perl = TRUE))>=1){
    
    text <- xpathSApply(htmlParse(text, asText = TRUE), "//body//text()", xmlValue) ### this is a new addition i had to clean html like text that showed up as animal species names as cpriolusquote
    ## it all came out with the capreolus issue
    
    #text<-TW_obj[TW_obj$image_path %in% tweets_4_hash_clean1que[,"Media_url"] & TW_obj$method=="FasterRCNN" & TW_obj$score>=0.75,"label" ] 
    corpus<- Corpus(VectorSource(gsub("(https?://\\S+\\s*|&amp|\\n\\n|\\n|\\n\\n\\n)", "", text, perl = TRUE))) ## silenziate wrning to life a better life
    corpus <- tm_map(corpus, tolower) ## silenziate wrning to life a better life
    corpus <- tm_map(corpus, removePunctuation) ## silenziate wrning to life a better life
    corpus <- tm_map(corpus, removeNumbers)## silenziate wrning to life a better life
    corpus <- tm_map(corpus, removeWords, stopwords("english")) ## silenziate wrning to life a better life
    corpus <- tm_map(corpus, stripWhitespace)## silenziate wrning to life a better life
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "&amp;", replacement = "")
    
    # Extract individual words
    corpus <- unlist(strsplit(as.character(corpus), "\\s+"))
    
    # Remove empty and short words
    corpus <- corpus[nchar(corpus) > 1]
    
    # Remove non-alphabetic words
    corpus <- corpus[grepl("^[a-zA-Z]+$", corpus)]
    
    
    corpus <- TermDocumentMatrix (corpus)
    # Apply stemming to the terms in the matrix
    #dtm_stemmed <- mytextdata
    rownames(corpus) <- SnowballC::wordStem(rownames(corpus), "english")
    
    
    
    corpus <- as.matrix (corpus)
    corpus <- sort( rowSums( corpus ),decreasing=TRUE)
    corpus <- data.frame (word = names (corpus),freq=corpus)
    
    ## adding an if here if i have only one label 
    # ifelse(length(unique(corpus$word)==1)
    # Aggregate term frequencies for duplicate terms
    # if (sum(duplicated( corpus$word)) >= 1){
    
    #try(corpus <- aggregate(freq ~ word, corpus, sum), silent=T)
    corpus <- aggregate(freq ~ word, corpus, sum)
    #corpus <- corpus[order(corpus$freq, decreasing = TRUE), ]
    #corpus <- aggregate(freq ~ word, corpus, sum)
    # }else { 
    # Sort by frequency in decreasing order
    corpus <- corpus[order(corpus$freq, decreasing = TRUE), ] 
    #corpus <- sort( rowSums( corpus ),decreasing=TRUE)
    #sum(duplicated( corpus$word))
    set.seed( 412 )# for reproducibility
    return(corpus)
    
    gc(reset=T)
    #rm(corpus)
  }else { print("Not enough tweetsor blip/gpt for preprocess corpus")
    a=data.frame(freq=NULL,word=NULL )
    return(a) 
  }}





    ##### NOW FOR COMBINED QUERIES VERSION 4 to put in the Hub - pseudonymised #############
    ### these are with only the 'newer' posts after 2019
    setwd("C:/Users/lc365c/OneDrive - University of Glasgow/Desktop/Pipelines - DEF script/flicker")
    setwd("/Users/luigi.caopinna/Library/CloudStorage/OneDrive-UniversityofGlasgow/Desktop/Pipelines - DEF script/flicker")
    ## ON THE NEW DATASET 
    #### LOADING THE SECOND VERSION OF THE DATA 
    load("flick_all_SIMD_V2_14-08-2024.Rda") ## This data can not be uplpoaded for ethic purposes, it is not anonymised.
    ## here i need to rename it for the final thing
    flick_all_SIMD<-flick_all_V2
    rm(flick_all_V2)
    
    ### start checking names 
    View(sort (table(flick_all_SIMD$ownername)))
    
    ## there are very few active users, and one approach could be to just remove those names from the main tags. 
    ## however, these can still cause some names to show up in sparse regions of the SIMD map. 
    ## the orher approaches is to figure out a rule to remove names from tags, or to remove the tags overall.
    ### if we remove all the tags, we can indeed keep only those used to perform the original query.
    
    
    
    ## how many tweets are we talking about? 
    #### HANDLE THE TIME FRAME 
    library(lubridate)
    library(dplyr)
    library(st)
    library(sf)
    # Convert the 'datetaken' column to a POSIXct datetime object
    flick_all_SIMD$datetaken <- ymd_hms(flick_all_SIMD$datetaken)
    
    
    
    
    
    flick_all_SIMD$datetaken<-ymd_hms(flick_all_SIMD$datetaken)
    flick_all_SIMD$monthdayyear = format(flick_all_SIMD$datetaken, "%m-%d-%y")
    # Create separate columns for year, month, and minutes/hour
    flick_all_SIMD$year <- year(flick_all_SIMD$datetaken)
    table(flick_all_SIMD$year)
    sum(flick_all_SIMD$year>=2019 & !is.na(flick_all_SIMD$year))
    ### removing older posts 
    flick_all_SIMD<-flick_all_SIMD[flick_all_SIMD$year>=2019 & !is.na(flick_all_SIMD$year),]
    
    table(flick_all_SIMD$year)
    
    
    
    
    
    
    
    ##################### try to remove the username I can remove tags at all, but it may still remain in the title or other let's see what we can do
    
    ### apply this in the loop 
    # Function to clean tags based on username
    ## the idea is that if we get the ownername tolower and aggregated it, we will then grep it in all our values of title, tags, and descriptions
    remove_overlapping_tags <- function(username, tags) {
      clean_username <- gsub("[^a-z0-9]", "", tolower(username))  # Normalize username
      tag_list <- unlist(strsplit(tags, " "))                     # Split tags
      filtered <- tag_list[!grepl(clean_username, tag_list, ignore.case = TRUE)]  # Remove overlaps
      paste(filtered, collapse = " ")                             # Recombine
    }
    
    
    library(stringdist)
    

     ### this is a very good idea to be more bespoken 
    
     
    ## this is the function i was using, it seems to be working quite well, but I have not developed a clear way to actually see how this is working
    ## i trust my approach more, the issue is that i will need two functions to clean tags and non tags, or maybe i can squeeze all toghether
    mask_overlapping_tags <- function(username, tags, distance_threshold = 0.2, mask = "USER_TAG") {
      ## this is to give this a try with the original version of tags 
      # username<-flick_all_SIMD$ownername[1]
      # tags<-flick_all_SIMD$tags_original[1]
      # distance_threshold<-0.2
      # mask = "USER_TAG"
      # username<-"Ratters: Thanks for the Views and Favs:)"
      # tags<-"<i>© D a v e F o r b e s </i>\n\n<b>Engagement 700+</b>\n\nSeen in Milton of Campsie Scotland #wildlifeandnature #longtailedtit #nature #wildbirds"
      # username<-"Photos By Dave Forbes"
      
      ### after a medium in deep first visual check of the maps I found Ronnie barron as an username still appearing in some posts and aggregated maps. 
      ### that happened because the tag is RCB4J and the name is in fact Ronnie Barron. 
      clean_username <- gsub("[^a-z0-9]", "", tolower(username))  # Normalize username ## that is needed for the tags, since one can not use separate tags
      ## this works well with tags and descriptions too.
      
    ## an idea is to set a list of names which will be removed instead fo the cryptic nametag
      ### kryptic tags
      # these users used a tag in the hastags ownername field, and then put their real name in the text
      crypt_usernames<-c("RCB4J","johnmightycat1") ### these are the usernames which are cryptics and used instead of the real name 
      extended_usernames<-c("*****","*****") ### these are the real usernames used in the tags for example which correspond to the cryptic tags.  
      ## these are the real usernames, that should be in the same order of the criptic ones
      
      
      # an idea is to set a list of names which will be removed instead fo the cryptic nametag
      ### this will check if the clean username belongs to one of the cryptic ones and in case substitute it with the extention, based on the inputted list at the beginning. 
      #ifelse(sum(grepl(clean_username, crypt_usernames, ignore.case = TRUE))>0,clean_username=extended_usernames[grepl(clean_username, crypt_usernames, ignore.case = TRUE)], NA )
      
      matches <- grepl(clean_username, crypt_usernames, ignore.case = TRUE) ## first get the matches, so are the clean usernames overlapping with the cryptic ones, where there is the need to do a correction?
      
      ### for this code it is important that the two vectors are in the same order, so RCB4J will be substituted with first element on the other side so ronniebarron
      ## the substitution should be lower e tutto attaccato (withouth spaces)
      ## if there are overlaps then correct the cryptic username with the correct corresponding one, otherwise no changes
      if (any(matches)) {
        username <- extended_usernames[which(matches)]  # get the first matching extended name
        ## but i guess I also need to change the name of the cleaned version so clean username again
        clean_username <- gsub("[^a-z0-9]", "", tolower(username))  # Normalize username ## that is needed for the tags, since one can not use separate tags
        
      } else {
        clean_username <- clean_username
      }
      # Replace full name (case-insensitive) with placeholder, this is maybe the easiest but most important step to do
      #tags <- gsub(paste("(?i)\\b",username,"\\b", sep = ""), mask, tags, perl = TRUE)
      ## this gave some errors, so we putted a trtychath
      tryCatch({
        tags <- gsub(paste0("(?i)\\b", username, "\\b"), mask, tags, perl = TRUE)
      }, error = function(e) {
        cat("Error in gsub with username:", username, "\n")
      })
      
      tag_list <- unlist(strsplit(tags, " "))                     # Split tags but i need this after the tags are cleaned by the complete username name. Just because i then cleaned the names on this
      
      
      
      
      
      
      
      username_list <- unlist(strsplit(tolower(username), " "))                     # Split tags
      library(tm)  # for stopwords()
      ## however there are sometimes too many usertags with stopwords, which make me mask all stopwords, which then appear as a common usertag
      ## an idea could be to clean these usertags
      # Split username into words
      #username_list <- unlist(strsplit(tolower(username), " "))
      
      # Remove empty strings and stopwords fromt he usernames list
      username_list <- username_list[
        nchar(username_list) > 0 &   # keep non-empty strings
          !(username_list %in% stopwords("english"))  # remove common stopwords
      ]
      
      #
     
      
      # Compute distances for the clean aggregated username
      tag_distances <- stringdist(tolower(tag_list), clean_username, method = "jw")
      
      ## try to do this also with the usernames lists - here we can use a more stringent match
      #username_list_distances <- stringdist(tolower(tag_list), username_list, method = "jw")
     #username_list_distances <- stringdist(tolower(tag_list), "boulton", method = "jw")
      #min_dist <- numeric(length(tag_list))
      min_dist<-rep(1,length(tag_list)) ### i put a high number here, so i can subsitute with lower values if there are any
      for (i in username_list[]) { ## this is to compute for each element of the tag list the distance with the username list
        username_list_distances <- stringdist(tolower(tag_list), i, method = "jw")
        
        min_dist <- apply(as.data.frame(rbind(min_dist,username_list_distances)), 2, min) ## this is to compare each element of the owner name with each element 
        # of the tags or descriptions and compute a distance value. i then take only the lower, 
        # so the idea is to get the words of the tags or descriptions which are similar to at least one element of the ownername. Let's hope this is enough.
        
      }
      # min_dist is now an important vector as well, to clean the tag lists, or word list 
      
      # Identify which tags should be masked
      is_overlap <- grepl(clean_username, tag_list, ignore.case = TRUE) | tag_distances < distance_threshold | min_dist<0.1 ###
      ## i have already removed the perfect matches at the beginning, i am now cleaning the words which match with the cleaned aggregated usernames
      ## those that match with a difference of less than 0.1 and those that match with less than 0.1 of each of the single elements of the lsit
      
      # Replace matching tags with a mask
      tag_list[is_overlap] <- mask
      
      # Recombine into a single string
      paste(tag_list, collapse = " ")
    }
    
    
    
  
    ## that seems to be working for me, 
    # the assumption is though that the tag with personal informations is the same of the declared username, or one of the cryptic ones. 
    ## it would not clean the username if for example the declared username is "peppe" and the name in the tags is "giuseppe rossi" unless i defined peppe as a cryptic username for "giusepperossi"
    # but in order for this to happen, i need to have found this in the maps
  
    ###### Another function to mask overlapping tags Chat wrote this and it is all wrong. 
    library(stringdist)
    
  
    
      
    ############ in all these three cases i am doing an overwreiting the original column THAT CAN BE IMPORTANT FOR THE SAVED DATASET
    
    # Apply the function row-wise
    ## clean tags
    flick_all_SIMD$tags_original<-flick_all_SIMD$tags
    flick_all_SIMD$tags <- mapply(mask_overlapping_tags, flick_all_SIMD$ownername, flick_all_SIMD$tags)
    View(cbind(flick_all_SIMD$tags,flick_all_SIMD$tags_original, flick_all_SIMD$ownername))
    
    ## clean titles
    flick_all_SIMD$title_originla<-flick_all_SIMD$title
    flick_all_SIMD$title <- mapply(mask_overlapping_tags, flick_all_SIMD$ownername, flick_all_SIMD$title) ## with the upgrade of handling this will clear the tahs and cryptic tags from the descriptions as well
    ## so basically if the user tag is RNC4J the name will be cleaned using ronnie baronn name
    View(cbind(flick_all_SIMD$title,flick_all_SIMD$title_originla, flick_all_SIMD$ownername))
    
    ## clean post
    flick_all_SIMD$description_original<-flick_all_SIMD$description
    ### this new version should be able to also grep www. other than htmsl
   # flick_all_SIMD$description <- gsub(
  #    "(?i)(http[s]?://|www\\.)[^\\s\"<>]+",
  #    "URL_LINK_MASKED",
  #    flick_all_SIMD$description,
  #    perl = TRUE
  #  )## first remove the links, otherwise it get difficult to spot them
   
    # this previous version is not cleaning the rel= part of it and according to chat this new code will
    # First: mask actual URLs in href attributes and anywhere else
    flick_all_SIMD$description <- gsub(
      "(?i)(http[s]?://|www\\.)[^\\s\"<>]+",
      "URL_LINK_MASKED",
      flick_all_SIMD$description,
      perl = TRUE
    )
    
    # Second: mask visible URL-like text between HTML tags (e.g., <a>instagram.com/xxx</a>)
    flick_all_SIMD$description <- gsub(
      "([a-z0-9.-]+\\.[a-z]{2,}[^<]*)<",
      "URL_LINK_MASKED",
      flick_all_SIMD$description,
      perl = TRUE
    ) ## I have checked this a bit and it seems to be working to further cleaning some urls
    
     flick_all_SIMD$description <- mapply(mask_overlapping_tags, flick_all_SIMD$ownername, flick_all_SIMD$description )
    View(cbind(flick_all_SIMD$description,flick_all_SIMD$description_original, flick_all_SIMD$ownername))
    
    
     ### but i have also noticed some links here and there i would like to remove.
    
    
    #ddd<-gsub(" ?(f|ht)tp(s?)://(.*)[.][a-z]+", "URL_LINK_MASKED",flick_all_SIMD$description )
    ## this clean any reference to htlm
    #flick_all_SIMD$description <- gsub("(<a href=\")?(f|ht)tp(s?)://[^\\s\"']+", "URL_LINK_MASKED", flick_all_SIMD$description)
   
    
    View(cbind(flick_all_SIMD$description,flick_all_SIMD$description_original))
    ## i will not clean the title nor tags fiels since it appears there are not many connections there
    
    
    ### plus loading other stuff i will need
    library(readr)
    
    GCCcodes <- read_csv("GCCcodes.csv")
    View(GCCcodes)
    ## adding more data for the SIMD
    
    d<-"C:/Users/lc365c/OneDrive - University of Glasgow/Desktop/simd2020_withgeog/sc_dz_11.shp"
    #d<-"C:/Users/luigi/OneDrive - University of Glasgow/Desktop/simd2020_withgeog/sc_dz_11.shp"
    
    #SIMD<-raster::shapefile(d) ## this is working
    library(rgdal)
    library(htmlwidgets)
    library(sf)
    library(htmltools)
    library(leaflet)
    library(lubridate)
    SIMD <- readOGR(dsn = d)
    
    
    ## to select the Glasgow city region area i can go ahead like this 
    ### first check all posts have an associated region
    sum(table(flick_all_SIMD$Council_area))
    dim(flick_all_SIMD)
    ### GCR council areas: 1) "East Dunbartonshire",2) "Glasgow City",3) "East Renfrewshire", 4) "North Lanarkshire", 5) "South Lanarkshire", 6) "Inverclyde", 7) "West Dunbartonshire" , 8) "Renfrewshire"
    ## these should be 8 
    CGR_names<-c( "East Dunbartonshire","Glasgow City", "East Renfrewshire", "North Lanarkshire", "South Lanarkshire", "Inverclyde", "West Dunbartonshire" , "Renfrewshire")
### i need to be carefull the GCR loop may stop anytime, if i do not have at least one point in my GCR area. That should not be the case though since i had enough on the GCC area. 
    
    
    
    
    
    
    

    ### this work on the previous "pseudonymised" loop and with a novel word selection
    ### so i need to run the previous part of the code and not the loops, but also to run again the word selection
    API_vand<-c("vandalised", "vandal", "mural", "leftbehind", "disused","abandoned", "vandalism", "graffiti", "murals", "antisocial", "publicdisorder", "urbandecay", "crime", "criminality", "publicnuisance", "propertydamage")
    APIwords<-c( "tree","trees","biodiversityloss","nature","environment","protectnature","Naturephotography","flowers","wildflowers","bee","bees","bird","birds","restorebiodiversity","citizenscience","connectivity","BirdSpecies","mammalspecies","Birding","birdphotography","Habitat","wildlifephotography","wildlife","animals","garden","gardening","LocalNatureReserve","urbancorridors","HabitatMapping", ## WP2
                 "floodrisk","riverscapes","riverscape","riverlandscape","riverlandscapes","floodadaptation","flood","flooding", "tidal","riverclyde","tidalchange",  "sealevelrise", "sealevel" , "extremeweather", "floodwarning",
                 "UrbanCorridorParks", "FloodWaterStorage", "searise", "riverclyde", "riverkelvin",  #) ## WP1,#) ## WP1
                 "derelict", "vacant","land","pollutedlands","derelictland", "greenhousegas","trappollutants","greenhousegases","abandonedland","polluted","airquality","vacantland","contamination" ,"pollutant","environmentalcontamination", ###) ### WP3 
                 "Promotingactivetravel","activetravel","inclusivemobility","cycling", "cyclist", "cyclists", "bike", "bikesharing","wheeling" , "walking", "bus" , "tram" ,"busstops" , "subway","subwaystation", "train","trains", "trainstation", "tramstops", "thelimit", "20mileshour","20minuteneighbourhood","publictransport","mcgill","firstbus" ,"lowemissionzone", "lez", "nextbike", "mobility", "10minutesfromhome" , "pedestrian", "transportation","sustainabletravel", "bicycles", "bicycle", "cycle", "pedestrianised", "e-bike", "e-bicycle", "commuters", "commuting", "adaptivecycle", "walkability", "rail", "JMBtravel", "PVT", "stagecoach", "liveableneighbhourood", "LN", "citynetwork", "roadsafety", ### WP 4
                 "Sustainableenergy", "netzero", "netzerocarbon", "lowcarbon","lowcarbonenergy","cleanenergy" ,"CleanEnergyDemand", "renewables", "renewableenergy", "sustainableenergy","lowemissions" ,"loweremissions", "emissions" ,"carbonemissions","carbonneutral","energysolutions","ClimateCrisis", "cuttingemissions" ,"energy" , "energytransition", "heating","sustainableheating","sustainabletransport", "sustainable" ,"CarbonDioxideEmissions", ## WP5
                 "Sustainabledevelopmentgoal", "SDG", "SIMD","ScottishIndexofMultipleDeprivation", "systemthinking", ### WS1 words
                 "Glasgow" , "streetphotography" , "streetphotographers","glasgowstreetphotography","photooftheday", "PeopleMakeGlasgow","glasgowpeople", "photomapping", "glasgowwestend","westendglasgow" , "glasgowphotography" , "glasgowphotographer" ,"Glasgowcentral" , "glasgowstreetphotography", "photo", "communityspaces" , "Community","southsideglasgow", "glasgowsouthside", "glasgowstyle" ,"communityjustice", "collaborativecommunity", "Communitycollaboration", ### WS 2 words
                 "data" , "dataanalytics","datahub", ## WS3
                 "Urbanenvironmental", "ecologicalceiling", "climateresilience","climateadaptation" ,"glasgowlivinglab", "livinglab", "thrivingcities", "thrivingcityinitiative","c40cities", "greentransition","GALLANT","glasgowGALLANT", "GALLANTglasgow","sustainability" , "climatechange" ,"airquality", "environmentaljustice", ######) ## GALLANT wide
                 "Scotland", "Glasgowcityregion", "westernLowlands","GlasgowWest","Anderston", "westend","Govan","Govanhill" , "Strathbungo")## for the geographic API query
    
    ## these for the tags
    #vand <-paste("\\b(", paste("cicci", collapse = "|"), ")\\b", sep = "")
    
    vand <-paste("\\b(", paste(API_vand, collapse = "|"), ")\\b", sep = "")
    
    ## the only version is really tracking things that we think can be associated with the issues
    vand_only <-paste("\\b(", paste(API_vand[c(1,2,7,10,11:16)], collapse = "|"), ")\\b", sep = "")
    
    ## I WAS GETTING SOME ERRORS AS I NEEDED THE COMPLETE GREP
    WP2 <-paste("\\b(", paste(APIwords[1:29], collapse = "|"), ")\\b", sep = "")
    WP2_only <-paste("\\b(", paste(APIwords[c(1:3,9:13,17:20)], collapse = "|"), ")\\b", sep = "")
    
    #WP1 <- paste(APIwords[30:49], collapse = "|")
    WP1 <-paste("\\b(", paste(APIwords[30:49], collapse = "|"), ")\\b", sep = "")
    WP1_only <-paste("\\b(", paste(APIwords[c(30,36,37,43,44)], collapse = "|"), ")\\b", sep = "")
    
    #WP3 <- paste(APIwords[50:64], collapse = "|")
    WP3 <-paste("\\b(", paste(APIwords[50:64], collapse = "|"), ")\\b", sep = "")
    WP3_only <-paste("\\b(", paste(APIwords[c(50,53,54,58,61,64)], collapse = "|"), ")\\b", sep = "")
    
    #WP4 <- paste(APIwords[65:115], collapse = "|")
    WP4 <-paste("\\b(", paste(APIwords[65:115], collapse = "|"), ")\\b", sep = "")
    WP4_only <-paste("\\b(", paste(APIwords[c(65:74,92,97:103)], collapse = "|"), ")\\b", sep = "")
    
    #WP5 <- paste(APIwords[116:140], collapse = "|")
    WP5 <-paste("\\b(", paste(APIwords[116:140], collapse = "|"), ")\\b", sep = "")
    WP5_only <-paste("\\b(", paste(APIwords[c(116,117,121:130,133,137)], collapse = "|"), ")\\b", sep = "")
    
    
    #WS1 <- paste(APIwords[141:146], collapse = "|")
    WS1 <-paste("\\b(", paste(APIwords[141:146], collapse = "|"), ")\\b", sep = "")
    
    #WS2 <- paste(APIwords[147:168], collapse = "|")
    WS2 <-paste("\\b(", paste(APIwords[147:168], collapse = "|"), ")\\b", sep = "")
    
    #WS3 <- paste(API_words[169:], collapse = "|")
    #WIDE <- paste(APIwords[172:197], collapse = "|")
    WIDE <-paste("\\b(", paste(APIwords[172:197], collapse = "|"), ")\\b", sep = "")
    
    #ALL<- paste(APIwords[], collapse = "|")
    ALL <-paste("\\b(", paste(APIwords[], collapse = "|"), ")\\b", sep = "")
    wordAGGR<-as.data.frame(cbind(c(WP1,WP2,WP3,WP4,WP5,WP1_only,WP2_only,WP3_only,WP4_only,WP5_only,WS1,WS2,WIDE,ALL,vand,vand_only),c("WP1","WP2","WP3","WP4","WP5","WP1_only","WP2_only","WP3_only","WP4_only","WP5_only","WS1","WS2","WIDE","ALL","vand","vand_only")))
    ### for now the word selection is actually the same
    for (Q in wordAGGR$V2[] ) {
      #Q="WP2" ## trial 
      #Q="vand" ## trial 
      que<-wordAGGR[wordAGGR$V2==Q,"V1"]
      ## subsetting for the selected query 
      tweets_4_hash_clean1que<-flick_all_SIMD[ grepl(que, flick_all_SIMD$tags_original),] ### the modification is to do that with the original tags, since for firstbus, it may remove the word bus
      ## are there duplicates in the query?
      sum(duplicated(tweets_4_hash_clean1que$id)) ## why i do not have duplicates? these must have been screened out before?
      
      #save(tweets_4_hash_clean1que,file="only_trees_flicker.Rda")
      #write.csv(tweets_4_hash_clean1que,file="only_trees_flicker.csv")
      if (dim(tweets_4_hash_clean1que)[1]>0) {
        #que<-wordAGGR[wordAGGR$V2==Q,"V1"]
        #tweets_4_hash_clean1que<-tweets_4_hash_clean1que[!duplicated(tweets_4_hash_clean1que$id),]
        #tweets_4_hash_clean1que$que<-rep(Q, dim(tweets_4_hash_clean1que)[1])
        DZ<-unique(tweets_4_hash_clean1que$DataZone) ## not many for the trial query
        
        
        
        
        
        
        
        #### continue with the older code
        
        ### for CNN
        tweets_4_hash_clean1que$FasterRCNN_Labelsmore_07 <- lapply(tweets_4_hash_clean1que$FasterRCNN_Labels, parseJSONSafely)
        tweets_4_hash_clean1que$FasterRCNN_Scoressmore_07 <- lapply(tweets_4_hash_clean1que$FasterRCNN_Scores, parseJSONSafely)
        # Filter labels by scores
        tweets_4_hash_clean1que$FasterRCNN_Labelsmore_07 <- mapply(function(labels, scores) {
          labels[scores > 0.75]
        },  tweets_4_hash_clean1que$FasterRCNN_Labelsmore_07 , tweets_4_hash_clean1que$FasterRCNN_Scoressmore_07, SIMPLIFY = FALSE)
        ## for yolos
        # Convert character strings to lists
        tweets_4_hash_clean1que$YOLOS_Labelsmore_07 <- lapply(tweets_4_hash_clean1que$YOLOS_Labels, parseJSONSafely)
        tweets_4_hash_clean1que$YOLOS_Scoresmore_07 <- lapply(tweets_4_hash_clean1que$YOLOS_Scores, parseJSONSafely)
        # Filter labels by scores
        tweets_4_hash_clean1que$YOLOS_Labelsmore_07 <- mapply(function(labels, scores) {
          labels[scores > 0.75]
        },  tweets_4_hash_clean1que$YOLOS_Labelsmore_07 , tweets_4_hash_clean1que$YOLOS_Scoresmore_07, SIMPLIFY = FALSE)
        
        #dz="S01011874"
        # print(dz)
        #library(lubridate)
        tweets_4_hash_clean1que$datetaken<-ymd_hms(tweets_4_hash_clean1que$datetaken)
        tweets_4_hash_clean1que$monthdayyear = format(tweets_4_hash_clean1que$datetaken, "%m-%d-%y")
        ## modify Q for saving 
        sanitized_path <- gsub("[/:\"\\\\]", "_", Q)
        
        
        ### plotting the points ere 
        # Create a color palet891te with handmade bins.
        
        #library(leaflet)
        tweets_4_hash_clean1que$resc_sent<-tweets_4_hash_clean1que$H_desc_Positive-tweets_4_hash_clean1que$H_desc_Negative
        tweets_4_hash_clean1que$resc_sent_AI<-tweets_4_hash_clean1que$Positive-tweets_4_hash_clean1que$Negative
        
        
        
        
        
        #library(htmltools)
        # mybins <- c(seq(0,summary(tweets_4_hash_clean1que$resc_sent)[6],summary(tweets_4_hash_clean1que$resc_sent)[6]/5))
        mybins <- c(seq(-1,1,0.4))
        
        #mypalette <- colorBin( palette=rev("RdYlBu"), domain=tweets_4_hash_clean1que$resc_sent_AI, na.color="transparent", bins=mybins)
        mypalette <- colorBin( palette=("RdYlBu"), domain=tweets_4_hash_clean1que$resc_sent_AI, na.color="transparent", bins=-mybins, reverse = T)
        
        # Prepare the text for tooltips:
        mytext <- paste(
          "Data Zone: ", tweets_4_hash_clean1que$DataZone,"<br/>", 
          "Area: ", tweets_4_hash_clean1que$StdAreaKm2, "<br/>", 
          "SIMD Rank: ", tweets_4_hash_clean1que$SIMD2020v2_Rank, "<br/>", 
          "Population: ", ((tweets_4_hash_clean1que$TotPop2011)), "<br/>", 
          #"Nr.img: ", as.numeric(tweets_4_hash_clean1que$nr.image), "<br/>", 
          #"Nr.PUD: ", as.numeric(tweets_4_hash_clean1que$Photo_User_Day_un), "<br/>", 
          "post_title: ", (tweets_4_hash_clean1que$title), "<br/>", 
          
          "user_post: ", (tweets_4_hash_clean1que$description), "<br/>", 
          "Resc_HUM.sent: ", round(tweets_4_hash_clean1que$resc_sent, 2), "<br/>",
          "Pos_HUM.sent: ", round(tweets_4_hash_clean1que$H_desc_Positive, 2), "<br/>",
          "Neg_HUM.sent: ", round(tweets_4_hash_clean1que$H_desc_Negative, 2), "<br/>",
          "Neut_HUM.sent: ", round(tweets_4_hash_clean1que$H_desc_Neutral, 2), "<br/>", 
          "Img_URL: ", (tweets_4_hash_clean1que$url_l ), "<br/>", 
          
          "AI_img_descr: ", tweets_4_hash_clean1que$Caption_blip , "<br/>" ,
          "Resc_AI.sent: ", round(tweets_4_hash_clean1que$Positive-tweets_4_hash_clean1que$Negative, 2), "<br/>",
          "Pos_AI.sent: ", round(tweets_4_hash_clean1que$Positive, 2), "<br/>",
          "Neg_AI.sent: ", round(tweets_4_hash_clean1que$Negative, 2), "<br/>",
          "Neut_AI.sent: ", round(tweets_4_hash_clean1que$Neutral, 2), "<br/>", 
          "Tags: ", (tweets_4_hash_clean1que$tags), "<br/>", 
          #"Tags: ", SIMD_merg@data$tags_words_freq[1:4] , "<br/>" ,
          
          
          
          sep="") %>%
          lapply(htmltools::HTML)
        #library(sf)
        tweets_sf <- st_as_sf(tweets_4_hash_clean1que, coords = c("longitude", "latitude"), crs = 4326)
        
        # Final Map points?
        # Assuming tweets_sf is already converted to an sf object with points
        m <- leaflet(tweets_sf) %>%
          addTiles() %>%
          setView(lat = 55.8, lng = -4.3, zoom = 5) %>%
          addCircleMarkers(
            fillColor = ~mypalette(resc_sent_AI),
            stroke = TRUE,
            fillOpacity = 0.9,
            color = "white",
            weight = 0.3,
            popup = mytext,
            labelOptions = labelOptions(
              style = list("font-weight" = "normal", padding = "3px 8px"),
              textsize = "13px",
              direction = "auto"
            )
          ) %>%
          addLegend(
            pal = mypalette,
            values = ~resc_sent_AI,
            opacity = 0.9,
            title = "AI. Rescaled Sentiment",
            position = "bottomleft"
          )
        
        # Print the map
        m
        #library(htmlwidgets)
        saveWidget(m, file= paste("Data hub maps/_",sanitized_path,"pointmap_Flicker.html"))
        
        ######### points all, no tags 
        # Prepare the text for tooltips:
        mytext <- paste(
          "Data Zone: ", tweets_4_hash_clean1que$DataZone,"<br/>", 
          "Area: ", tweets_4_hash_clean1que$StdAreaKm2, "<br/>", 
          "SIMD Rank: ", tweets_4_hash_clean1que$SIMD2020v2_Rank, "<br/>", 
          "Population: ", ((tweets_4_hash_clean1que$TotPop2011)), "<br/>", 
          #"Nr.img: ", as.numeric(tweets_4_hash_clean1que$nr.image), "<br/>", 
          #"Nr.PUD: ", as.numeric(tweets_4_hash_clean1que$Photo_User_Day_un), "<br/>", 
          "post_title: ", (tweets_4_hash_clean1que$title), "<br/>", 
          
          "user_post: ", (tweets_4_hash_clean1que$description), "<br/>", 
          "Resc_HUM.sent: ", round(tweets_4_hash_clean1que$resc_sent, 2), "<br/>",
          "Pos_HUM.sent: ", round(tweets_4_hash_clean1que$H_desc_Positive, 2), "<br/>",
          "Neg_HUM.sent: ", round(tweets_4_hash_clean1que$H_desc_Negative, 2), "<br/>",
          "Neut_HUM.sent: ", round(tweets_4_hash_clean1que$H_desc_Neutral, 2), "<br/>", 
          "Img_URL: ", (tweets_4_hash_clean1que$url_l ), "<br/>", 
          
          "AI_img_descr: ", tweets_4_hash_clean1que$Caption_blip , "<br/>" ,
          "Resc_AI.sent: ", round(tweets_4_hash_clean1que$Positive-tweets_4_hash_clean1que$Negative, 2), "<br/>",
          "Pos_AI.sent: ", round(tweets_4_hash_clean1que$Positive, 2), "<br/>",
          "Neg_AI.sent: ", round(tweets_4_hash_clean1que$Negative, 2), "<br/>",
          "Neut_AI.sent: ", round(tweets_4_hash_clean1que$Neutral, 2), "<br/>", 
          #"Tags: ", (tweets_4_hash_clean1que$tags), "<br/>", ## in this run i remove the tags
          #"Tags: ", SIMD_merg@data$tags_words_freq[1:4] , "<br/>" ,
          
          
          
          sep="") %>%
          lapply(htmltools::HTML)
        #library(sf)
        tweets_sf <- st_as_sf(tweets_4_hash_clean1que, coords = c("longitude", "latitude"), crs = 4326)
        
        # Final Map points?
        # Assuming tweets_sf is already converted to an sf object with points
        m <- leaflet(tweets_sf) %>%
          addTiles() %>%
          setView(lat = 55.8, lng = -4.3, zoom = 5) %>%
          addCircleMarkers(
            fillColor = ~mypalette(resc_sent_AI),
            stroke = TRUE,
            fillOpacity = 0.9,
            color = "white",
            weight = 0.3,
            popup = mytext,
            labelOptions = labelOptions(
              style = list("font-weight" = "normal", padding = "3px 8px"),
              textsize = "13px",
              direction = "auto"
            )
          ) %>%
          addLegend(
            pal = mypalette,
            values = ~resc_sent_AI,
            opacity = 0.9,
            title = "AI. Rescaled Sentiment",
            position = "bottomleft"
          )
        
        # Print the map
        m
        #library(htmlwidgets)
        ##à you will need to create the folder here.
        saveWidget(m, file= paste("Data hubs no tags/_",sanitized_path,"pointmap_Flicker.html"))
        tweets_4_hash_clean1que_save<-tweets_4_hash_clean1que[, -which(colnames(tweets_4_hash_clean1que) %in% c("tags","tags_original"))]
        save( tweets_4_hash_clean1que, file=paste("Data hubs no tags/_",sanitized_path,"_pointmap.Rda",sep=""))
        
        
        
        
        ## the same, points but only for GCR area
        as<-tweets_4_hash_clean1que[ tweets_4_hash_clean1que$Council_area %in% CGR_names,]
        as$resc_sent<-as$H_desc_Positive-as$H_desc_Negative
        as$resc_sent_AI<-as$Positive-as$Negative
        
        ### points map for only GCC datazones
        #mybins <- c(seq(0,summary(as$Positive)[6],summary(as$Positive)[6]/8))
        mybins <- c(seq(-1,1,0.4))
        
        mypalette <- colorBin( palette=("RdYlBu"), domain=as$resc_sent_AI, na.color="transparent", bins=-mybins, reverse = T)
        
        # Prepare the text for tooltips:
        mytext <- paste(
          "Data Zone: ", as$DataZone,"<br/>", 
          "Area: ", as$StdAreaKm2, "<br/>", 
          "SIMD Rank: ", as$SIMD2020v2_Rank, "<br/>", 
          "Population: ", ((as$TotPop2011)), "<br/>", 
          #"Nr.img: ", as.numeric(as$nr.image), "<br/>", 
          #"Nr.PUD: ", as.numeric(as$Photo_User_Day_un), "<br/>", 
          "post_title: ", (as$title), "<br/>", 
          
          "user_post: ", (as$description), "<br/>", 
          "Resc_HUM.sent: ", round(as$resc_sent, 2), "<br/>",
          
          "Pos_HUM.sent: ", round(as$H_desc_Positive, 2), "<br/>",
          "Neg_HUM.sent: ", round(as$H_desc_Negative, 2), "<br/>",
          "Neut_HUM.sent: ", round(as$H_desc_Neutral, 2), "<br/>", 
          "Img_URL: ", (as$url_l ), "<br/>", 
          
          "AI_img_descr: ", as$Caption_blip , "<br/>" ,
          "Resc_AI.sent: ", round(as$Positive-as$Negative, 2), "<br/>",
          
          "Pos_AI.sent: ", round(as$Positive, 2), "<br/>",
          "Neg_AI.sent: ", round(as$Negative, 2), "<br/>",
          "Neut_AI.sent: ", round(as$Neutral, 2), "<br/>", 
          "Tags: ", (as$tags), "<br/>", 
          #"Tags: ", SIMD_merg@data$tags_words_freq[1:4] , "<br/>" ,
          
          
          
          sep="") %>%
          lapply(htmltools::HTML)
        library(sf)
        tweets_sf <- st_as_sf(as, coords = c("longitude", "latitude"), crs = 4326)
        
        # Final Map PUD
        # Assuming tweets_sf is already converted to an sf object with points
        m <- leaflet(tweets_sf) %>%
          addTiles() %>%
          setView(lat = 55.8, lng = -4.3, zoom = 5) %>%
          addCircleMarkers(
            fillColor = ~mypalette(resc_sent_AI),
            stroke = TRUE,
            fillOpacity = 0.9,
            color = "white",
            weight = 0.3,
            popup = mytext,
            labelOptions = labelOptions(
              style = list("font-weight" = "normal", padding = "3px 8px"),
              textsize = "13px",
              direction = "auto"
            )
          ) %>%
          addLegend(
            pal = mypalette,
            values = ~resc_sent_AI,
            opacity = 0.9,
            title = "AI. Rescaled Sentiment",
            position = "bottomleft"
          )
        
        # Print the map
        m
        
        library(htmlwidgets)
        saveWidget(m, file= paste("Data hub maps/_",sanitized_path,"_GCR_pointmap_Flicker.html"))
        
        
        
        ##### points GCR no tags 
        # Prepare the text for tooltips:
        mytext <- paste(
          "Data Zone: ", as$DataZone,"<br/>", 
          "Area: ", as$StdAreaKm2, "<br/>", 
          "SIMD Rank: ", as$SIMD2020v2_Rank, "<br/>", 
          "Population: ", ((as$TotPop2011)), "<br/>", 
          #"Nr.img: ", as.numeric(as$nr.image), "<br/>", 
          #"Nr.PUD: ", as.numeric(as$Photo_User_Day_un), "<br/>", 
          "post_title: ", (as$title), "<br/>", 
          
          "user_post: ", (as$description), "<br/>", 
          "Resc_HUM.sent: ", round(as$resc_sent, 2), "<br/>",
          
          "Pos_HUM.sent: ", round(as$H_desc_Positive, 2), "<br/>",
          "Neg_HUM.sent: ", round(as$H_desc_Negative, 2), "<br/>",
          "Neut_HUM.sent: ", round(as$H_desc_Neutral, 2), "<br/>", 
          "Img_URL: ", (as$url_l ), "<br/>", 
          
          "AI_img_descr: ", as$Caption_blip , "<br/>" ,
          "Resc_AI.sent: ", round(as$Positive-as$Negative, 2), "<br/>",
          
          "Pos_AI.sent: ", round(as$Positive, 2), "<br/>",
          "Neg_AI.sent: ", round(as$Negative, 2), "<br/>",
          "Neut_AI.sent: ", round(as$Neutral, 2), "<br/>", 
          #"Tags: ", (as$tags), "<br/>", ## removed this as well in this run
          #"Tags: ", SIMD_merg@data$tags_words_freq[1:4] , "<br/>" ,
          
          
          
          sep="") %>%
          lapply(htmltools::HTML)
        library(sf)
        tweets_sf <- st_as_sf(as, coords = c("longitude", "latitude"), crs = 4326)
        
        # Final Map PUD
        # Assuming tweets_sf is already converted to an sf object with points
        m <- leaflet(tweets_sf) %>%
          addTiles() %>%
          setView(lat = 55.8, lng = -4.3, zoom = 5) %>%
          addCircleMarkers(
            fillColor = ~mypalette(resc_sent_AI),
            stroke = TRUE,
            fillOpacity = 0.9,
            color = "white",
            weight = 0.3,
            popup = mytext,
            labelOptions = labelOptions(
              style = list("font-weight" = "normal", padding = "3px 8px"),
              textsize = "13px",
              direction = "auto"
            )
          ) %>%
          addLegend(
            pal = mypalette,
            values = ~resc_sent_AI,
            opacity = 0.9,
            title = "AI. Rescaled Sentiment",
            position = "bottomleft"
          )
        
        # Print the map
        m
        
        library(htmlwidgets)
        saveWidget(m, file= paste("Data hubs no tags/_",sanitized_path,"_GCRpointmap_Flicker.html"))
        as_save<-as[, -which(colnames(as) %in% c("tags","tags_original"))]
        save( as_save, file=paste("Data hubs no tags/_",sanitized_path,"_GCR_pointmap.Rda",sep=""))
        
        
        
        
        
        
        
        pud_results <- tweets_4_hash_clean1que %>%
          group_by(DataZone,ownername , monthdayyear) %>%
          summarise(
            Photo_User_Day = n_distinct(id),  # Replace 'photo_id' with your photo identifier column
            .groups = 'drop'  # Drop grouping for the next operations
          )
        ## these count the number of pictures each day in each datazone done by users. However, each of them should count as one. 
        pud_results$un_count<-rep(1,length(pud_results$DataZone))
        ## aggregate again 
        pud_results_un <- pud_results %>%
          group_by(DataZone) %>%
          summarise(
            Photo_User_Day_un = sum(un_count),
            names=list(ownername),
            days=list(monthdayyear)# Replace 'photo_id' with your photo identifier column
            # Drop grouping for the next operations
          )
        
        
        aggregated_df_topics <- tweets_4_hash_clean1que[  ,] %>%
          group_by(DataZone) %>%
          summarize(
            #que=first(que),
            mean_Resc_AI = mean(Positive-Negative),
            list_Resc_AI = list(Positive-Negative),
            
            mean_Positive_AI = mean(Positive),
            all_Positive_AI=list(Positive),
            mean_Negative_AI = mean(Negative),
            all_Negative_AI=list(Negative),
            mean_Neutral_AI = mean(Neutral),
            all_Neutral_AI=list(Neutral),
            
            
            mean_Resc_HUM = mean(H_desc_Positive-H_desc_Negative, na.rm = TRUE),
            list_Resc_HUM = list(H_desc_Positive-H_desc_Negative),
            
            mean_Positive_HUM = mean(H_desc_Positive, na.rm = TRUE),
            all_Positive_HUM=list(H_desc_Positive),
            mean_Negative_HUM = mean(H_desc_Negative, na.rm = TRUE),
            all_Negative_HUM=list(H_desc_Negative),
            mean_Neutral_HUM = mean(H_desc_Neutral, na.rm = TRUE),
            all_Neutral_HUM=list(H_desc_Neutral),
            
            TOT_views=sum(as.numeric(views)),
            flick_ID = list(id),
            ### add here id of the tweets 
            #      Nr.pos=sum(Sentiment==1),
            #       Nr.neg=sum(Sentiment==-1),
            ##        Nr.neut=sum(Sentiment==0),
            Nr.posts = length(na.omit(Neutral)),
            text_words=list(try(process_corpus_words(description)$word)),
            text_words_freq=list(try(process_corpus_words(description)$freq)),
            tags_words=list(try(process_corpus_words(tags)$word)),
            tags_words_freq=list(try(process_corpus_words(tags)$freq)),
            ## need to add the LDA for text
            
            ###THESE LDA WORKS BUT ARE LONG TO COMPUTE
            #LDA_inf_coherence_text=list(try(LDA_inf(description)$coherence)),
            #LDA_inf_prevalence_text=list(try(LDA_inf(description)$prevalence)),
            #LDA_inf_words_text=list(try(LDA_inf(description)$top_terms)),
            
            #LDA_NOinf_coherence_text=list(try(LDA(description)$coherence)),
            #LDA_NOinf_prevalence_text=list(try(LDA(description)$prevalence)),
            #LDA_NOinf_words_text=list(try(LDA(description)$top_terms)),
            
            nr.image=length(na.omit(url_l)),
            #      #nr.image_nomemes_elaborated=length(na.omit(TW_obj[TW_obj$image_path %in% Media_url & TW_obj$method=="Caption_blip" ,"text" ] )),
            #      #URL_nomemes=(na.omit(TW_obj[TW_obj$image_path %in% Media_url & TW_obj$method=="Caption_blip" ,"image_path" ] )),
            #      nr.image_nomemes_elaborated=length(na.omit(TW_obj[TW_obj$image_path %in% na.omit(Media_url) & TW_obj$method=="Caption_blip" ,"image_path" ] )),
            #      image_nomemes_URL=            list(na.omit(TW_obj[TW_obj$image_path %in% na.omit(Media_url) & TW_obj$method=="Caption_blip" ,"image_path" ] )),
            #      
            url = list(na.omit(url_l)),
            images_blipdescr_words=list(try(process_corpus_words (Caption_blip)$word)),
            images_blipdescr_freq=list(try(process_corpus_words(Caption_blip)$freq)),
            #images_blip_and_gptdescr_words=list(try(process_corpus_words ( c("Caption_blip",  "Text_gpt2") )$word)),
            #images_blip_and_gptdescr_freq=list(try(process_corpus_words(Caption_blip )$freq)),
            #      ## i have eliminated these previous, as it was not working, retrieved the all values 
            #      
            #      LDA_inf_coherence_blip=list(try(LDA_inf(Caption_blip)$coherence)),
            #      LDA_inf_prevalence_blip=list(try(LDA_inf(Caption_blip)$prevalence)),
            #      LDA_inf_words_blip=list(try(LDA_inf(Caption_blip)$top_terms)),
            #     
            #      LDA_NOinf_coherence_blip=list(try(LDA(TW_obj[TW_obj$image_path %in% Media_url & TW_obj$method=="Caption_blip" ,"text" ])$coherence)),
            #     LDA_NOinf_prevalence_blip=list(try(LDA(TW_obj[TW_obj$image_path %in% Media_url & TW_obj$method=="Caption_blip" ,"text" ])$prevalence)),
            #    LDA_NOinf_words_blip=list(try(LDA(TW_obj[TW_obj$image_path %in% Media_url & TW_obj$method=="Caption_blip" ,"text" ])$top_terms)),
            #    
            images_gptdescr_words=list(try(process_corpus_words (Text_gpt2 )$word)),
            images_gptdescr_freq=list(try(process_corpus_words(Text_gpt2 )$freq)),
            #    LDA_inf_coherence_gpt=list(try(LDA_inf(TW_obj[TW_obj$image_path %in% Media_url & TW_obj$method=="Text_gpt2" ,"text" ])$coherence)),
            #    LDA_inf_prevalence_gpt=list(try(LDA_inf(TW_obj[TW_obj$image_path %in% Media_url & TW_obj$method=="Text_gpt2" ,"text" ])$prevalence)),
            #    LDA_inf_words_gpt=list(try(LDA_inf(TW_obj[TW_obj$image_path %in% Media_url & TW_obj$method=="Text_gpt2" ,"text" ])$top_terms)),
            #    
            #    LDA_NOinf_coherence_gpt=list(try(LDA(TW_obj[TW_obj$image_path %in% Media_url & TW_obj$method=="Text_gpt2" ,"text" ])$coherence)),
            #    LDA_NOinf_prevalence_gpt=list(try(LDA(TW_obj[TW_obj$image_path %in% Media_url & TW_obj$method=="Text_gpt2" ,"text" ])$prevalence)),
            #    LDA_NOinf_words_gpt=list(try(LDA(TW_obj[TW_obj$image_path %in% Media_url & TW_obj$method=="Text_gpt2" ,"text" ])$top_terms)),
            #    
            #    
            images_labelsCNN_words=list(try(preprocess_corpus_stem (unlist(FasterRCNN_Labelsmore_07))$word)),
            images_labelsCNN_freq=list(try(preprocess_corpus_stem (unlist(FasterRCNN_Labelsmore_07))$freq)),
            
            images_labelsYOLOS_words=list(try(preprocess_corpus_stem (unlist(YOLOS_Labelsmore_07))$word)),
            images_labelsYOLOS_freq=list(try(preprocess_corpus_stem (unlist(YOLOS_Labelsmore_07))$freq)),
            #images_labelsYOLOS_words=list(try(preprocess_corpus_stem (unlist(tweets_4_hash_clean1que[ unlist(lapply(tweets_4_hash_clean1que$FasterRCNN_Scores, function(x) {
            # Remove non-numeric characters, brackets, and extra spaces, keeping only numbers, decimal points, and commas
            
            # images_labelsYOLOS_freq=list(try(preprocess_corpus_stem(unlist(tweets_4_hash_clean1que[YOLOS_Scores>=0.75 ,"FasterRCNN_Scores" ] )$freq))),
            
            
            # images_labelsYOLOS_words=list(try(preprocess_corpus_stem (TW_obj[TW_obj$image_path %in% Media_url & TW_obj$method=="YOLOS" & score>=0.75 ,"label" ] )$word)),
            #  images_labelsYOLOS_freq=list(try(preprocess_corpus_stem(TW_obj[TW_obj$image_path %in% Media_url & TW_obj$method=="YOLOS" & score>=0.75,"label" ] )$freq)),
            #    ## in the next version it would be better to analyse also resnet50 
            images_labelsresnet_words=list(try(preprocess_corpus_stem_resnet (ResNet50_Label )$word)),
            images_labelsresnet_freq=list(try(preprocess_corpus_stem_resnet(ResNet50_Label )$freq)),
            #  
            #"FasterRCNN_Labels","FasterRCNN_Scores","ResNet50_Label","ResNet50_Scores"
            Intermediate_Zone = first(Intermediate_Zone),
            Council_area = first(Council_area),
            Total_population = first(Total_population),
            Working_Age_population = first(Working_Age_population),
            SIMD2020v2_Rank = first(SIMD2020v2_Rank),
            SIMD_Percentile = first(SIMD_2020v2_Percentile),
            SIMD_Vigintile = first(SIMD2020v2_Vigintile),
            SIMD_Decile = first(SIMD2020v2_Decile),
            SIMD_Quintile = first(SIMD2020v2_Quintile),
            SIMD_Income_Domain_Rank = first(SIMD2020v2_Income_Domain_Rank),
            SIMD_Employment_Domain_Rank = first(SIMD2020_Employment_Domain_Rank),
            SIMD_Health_Domain_Rank = first(SIMD2020_Health_Domain_Rank),
            SIMD_Education_Domain_Rank = first(SIMD2020_Education_Domain_Rank),
            SIMD_Access_Domain_Rank = first(SIMD2020_Access_Domain_Rank),
            SIMD_Crime_Domain_Rank = first(SIMD2020_Crime_Domain_Rank),
            SIMD_Housing_Domain_Rank = first(SIMD2020_Housing_Domain_Rank),
            income_rate = first(income_rate),
            income_count = first(income_count),
            employment_rate = first(employment_rate),
            employment_count = first(employment_count),
            CIF = first(CIF),
            ALCOHOL = first(ALCOHOL),
            DRUG = first(DRUG),
            SMR = first(SMR),
            DEPRESS = first(DEPRESS),
            LBWT = first(LBWT),
            EMERG = first(EMERG),
            Attendance = first(Attendance),
            Attainment = first(Attainment),
            no_qualifications = first(no_qualifications),
            not_participating = first(not_participating),
            University = first(University),
            crime_count = first(crime_count),
            crime_rate = first(crime_rate),
            overcrowded_count = first(overcrowded_count),
            nocentralheating_count = first(nocentralheating_count),
            overcrowded_rate = first(overcrowded_rate),
            nocentralheating_rate = first(nocentralheating_rate),
            drive_petrol = first(drive_petrol),
            drive_GP = first(drive_GP),
            drive_post = first(drive_post),
            drive_primary = first(drive_primary),
            drive_retail = first(drive_retail),
            drive_secondary = first(drive_secondary),
            PT_GP = first(PT_GP),
            PT_post = first(PT_post),
            PT_retail = first(PT_retail),
            broadband = first(broadband)
          )
        
        
        
        
        sum(aggregated_df_topics$nr.image)
        ## these should match
        # SAVE the dataset 
        
        
        ### adding hte PUD now 
        # Perform a left join to add Photo_User_Day_un from pud_results_un to aggregated_df_topics
        aggregated_df_topics <- left_join(aggregated_df_topics, 
                                          pud_results_un[, c("DataZone", "Photo_User_Day_un")],
                                          by = "DataZone")    
        
        
        ## modify Q for saving 
        sanitized_path <- gsub("[/:\"\\\\]", "_", Q)
        
        save( aggregated_df_topics, file=paste("Data hub maps/_",sanitized_path,"_aggregated_df_topic_DATAZONES.Rda",sep=""))
        colnames(aggregated_df_topics)
        #### plotting the dataset
        
        
        ### adding data i need in the spatial object 
        SIMD_merg <- merge(SIMD, aggregated_df_topics, by.x = "DataZone", by.y = "DataZone")
        SIMD_merg@data$first_four_text_words <- vector("list", length = dim(SIMD_merg@data)[1])
        SIMD_merg@data$first_four_images_blipdescr_words <- vector("list", length = dim(SIMD_merg@data)[1])
        SIMD_merg@data$first_4wors_tags<- vector("list", length = dim(SIMD_merg@data)[1])
        #SIMD_merg@data$first_theme_4wors_LDA <- vector("list", length = dim(SIMD_merg@data)[1])
        #SIMD_merg@data$first_theme_4blipimg_LDA <- vector("list", length = dim(SIMD_merg@data)[1])
        
        #SIMD_merg@data$first_four_text_words<-rep(NA,4)
        #SIMD_merg@data$first_four_images_blipdescr_words<-NA
        
        ## taking only the selected four values for dysplaying 
        for (i in 1:dim(SIMD_merg@data)[1]) {
          ###   #i=892
          print(i)
          try(SIMD_merg@data$first_four_text_words[[i]]<-SIMD_merg@data$text_words[[i]][1:4],silent=F)
          try(SIMD_merg@data$first_four_images_blipdescr_words[[i]]<-SIMD_merg@data$images_blipdescr_words[[i]][1:4],silent=F)
          try(SIMD_merg@data$first_4wors_tags[[i]]<-SIMD_merg@data$tags_words[[i]][1:4],silent=F)
          #try(SIMD_merg@data$first_theme_4blipimg_LDA[[i]]<-SIMD_merg@data$LDA_NOinf_words_blip[[i]][1:2],silent=F)
          ## when i have the error message is actually one of the LDA that was stopped before performing.
          ## THESE ARE NOT MANY, AS IF THE LDA WAS NOT EVEN STARTING, I WILL NOT HAVE NULL BUT NA AND THE ERROR DO NOT APPEAR.
        }
        ## is this loop really necessary? YES!
        
        
        
        # Create a color palet891te with handmade bins.
        mybins <- c(seq(0,summary(SIMD_merg@data$Photo_User_Day_un)[6],summary(SIMD_merg@data$Photo_User_Day_un)[6]/8))
        mypalette <- colorBin( palette="YlOrBr", domain=SIMD_merg@data$Photo_User_Day_un, na.color="transparent", bins=mybins)
        # Prepare the text for tooltips:
        mytext <- paste(
          "Data Zone: ", SIMD_merg@data$DataZone,"<br/>", 
          "Area: ", round(as.numeric(SIMD_merg@data$StdAreaKm2), 1), "<br/>",
          "SIMD Rank: ", SIMD_merg@data$SIMD2020v2_Rank, "<br/>", 
          
          "Population: ", ((SIMD_merg@data$TotPop2011)), "<br/>", 
          "Nr. img.: ", as.numeric(SIMD_merg@data$nr.image), "<br/>", 
          "Nr. PUD: ", as.numeric(SIMD_merg@data$Photo_User_Day_un), "<br/>", 
          
          "Resc_AI.sent: ", round(SIMD_merg@data$mean_Resc_AI, 2), "<br/>",
          
          "Pos_AI.sent: ", round(SIMD_merg@data$mean_Positive_AI, 2), "<br/>",
          "Neg_AI.sent: ", round(SIMD_merg@data$mean_Negative_AI, 2), "<br/>",
          "Neut_AI.sent: ", round(SIMD_merg@data$mean_Neutral_AI, 2), "<br/>", 
          
          "Resc_HUM.sent: ", round(SIMD_merg@data$mean_Resc_HUM, 2), "<br/>",
          
          "Pos_HUM.sent: ", round(SIMD_merg@data$mean_Positive_HUM, 2), "<br/>",
          "Neg_HUM.sent: ", round(SIMD_merg@data$mean_Negative_HUM, 2), "<br/>",
          "Neut_HUM.sent: ", round(SIMD_merg@data$mean_Neutral_HUM, 2), "<br/>", 
          "Img_descr: ", (SIMD_merg@data[["first_four_text_words"]]), "<br/>", 
          "AI_img_descr: ", SIMD_merg@data[["first_four_images_blipdescr_words"]] , "<br/>" ,
          "Tags: ", (SIMD_merg@data$first_4wors_tags), "<br/>", 
          #"Tags: ", SIMD_merg@data$tags_words_freq[1:4] , "<br/>" ,
          
          
          
          sep="") %>%
          lapply(htmltools::HTML)
        # Final Map PUD
        m<- leaflet(SIMD_merg) %>% 
          addTiles()  %>% 
          setView( lat=55.8, lng=-4.3 , zoom=5) %>%
          addPolygons( 
            fillColor = ~mypalette(Photo_User_Day_un), 
            stroke=TRUE, 
            fillOpacity = 0.9, 
            color="white", 
            weight=0.3,
            label = mytext,
            labelOptions = labelOptions( 
              style = list("font-weight" = "normal", padding = "3px 8px"), 
              textsize = "13px", 
              direction = "auto"
            )
          ) %>%
          addLegend( pal=mypalette, values=~Photo_User_Day_un, opacity=0.9, title = "Nr.PUD", position = "bottomleft" )
        
        m  
        #saveWidget(m, file= paste("Flicker - query maps/_","GCC - vandal related PUD","_Data.html"))
        saveWidget(m, file= paste("Data hub maps/_",sanitized_path,"_Data zones map_Flicker.html"))
        
        
        
        
        
        
        #### all aggregated data hub no tags 
        # Prepare the text for tooltips:
        mytext <- paste(
          "Data Zone: ", SIMD_merg@data$DataZone,"<br/>", 
          "Area: ", round(as.numeric(SIMD_merg@data$StdAreaKm2), 1), "<br/>", 
          "SIMD Rank: ", SIMD_merg@data$SIMD2020v2_Rank, "<br/>", 
          
          "Population: ", ((SIMD_merg@data$TotPop2011)), "<br/>", 
          "Nr. img.: ", as.numeric(SIMD_merg@data$nr.image), "<br/>", 
          "Nr. PUD: ", as.numeric(SIMD_merg@data$Photo_User_Day_un), "<br/>", 
          "Post sent.: ", round(SIMD_merg@data$mean_Resc_HUM, 2), "<br/>",
          
          #"Pos_HUM.sent: ", round(SIMD_merg@data$mean_Positive_HUM, 2), "<br/>",
          #"Neg_HUM.sent: ", round(SIMD_merg@data$mean_Negative_HUM, 2), "<br/>",
          #"Neut_HUM.sent: ", round(SIMD_merg@data$mean_Neutral_HUM, 2), "<br/>", 
          
          "AI-capt. sent.: ", round(SIMD_merg@data$mean_Resc_AI, 2), "<br/>",
          
      #    "Pos_AI.sent: ", round(SIMD_merg@data$mean_Positive_AI, 2), "<br/>",
       #   "Neg_AI.sent: ", round(SIMD_merg@data$mean_Negative_AI, 2), "<br/>",
        #  "Neut_AI.sent: ", round(SIMD_merg@data$mean_Neutral_AI, 2), "<br/>", 
          
            #"Img_descr: ", (SIMD_merg@data[["first_four_text_words"]]), "<br/>", 
          "AI-capt. words: ", SIMD_merg@data[["first_four_images_blipdescr_words"]] , "<br/>" ,
          #"Tags: ", (SIMD_merg@data$first_4wors_tags), "<br/>", ## better not to save the tags in this case
          #"Tags: ", SIMD_merg@data$tags_words_freq[1:4] , "<br/>" ,
          
          
          
          sep="") %>%
          lapply(htmltools::HTML)
        # Final Map PUD
        m<- leaflet(SIMD_merg) %>% 
          addTiles()  %>% 
          setView( lat=55.8, lng=-4.3 , zoom=5) %>%
          addPolygons( 
            fillColor = ~mypalette(Photo_User_Day_un), 
            stroke=TRUE, 
            fillOpacity = 0.9, 
            color="white", 
            weight=0.3,
            label = mytext,
            labelOptions = labelOptions( 
              style = list("font-weight" = "normal", padding = "3px 8px"), 
              textsize = "13px", 
              direction = "auto"
            )
          ) %>%
          addLegend( pal=mypalette, values=~Photo_User_Day_un, opacity=0.9, title = "Nr.PUD", position = "bottomleft" )
        
        m  
        #saveWidget(m, file= paste("Flicker - query maps/_","GCC - vandal related PUD","_Data.html"))
        saveWidget(m, file= paste("Data hubs no tags/_",sanitized_path,"_Data zones map_Flicker.html"))
        
        
        
        
        ## REDOING TO CLOSE UP ON GCC REGION
        dim(aggregated_df_topics[ aggregated_df_topics$DataZone %in% GCCcodes$DataZone,])
        dim(aggregated_df_topics[ ,])
        ## for this case i am going to lose 578-48 regions, but it is fine because there was a too big area
        ### adding data i need in the spatial object 
        SIMD_merg <- merge(SIMD, aggregated_df_topics[ aggregated_df_topics$Council_area %in% CGR_names,], by.x = "DataZone", by.y = "DataZone")
        SIMD_merg@data$first_four_text_words <- vector("list", length = dim(SIMD_merg@data)[1])
        SIMD_merg@data$first_four_images_blipdescr_words <- vector("list", length = dim(SIMD_merg@data)[1])
        SIMD_merg@data$first_4wors_tags<- vector("list", length = dim(SIMD_merg@data)[1])
        #SIMD_merg@data$first_theme_4wors_LDA <- vector("list", length = dim(SIMD_merg@data)[1])
        #SIMD_merg@data$first_theme_4blipimg_LDA <- vector("list", length = dim(SIMD_merg@data)[1])
        
        #SIMD_merg@data$first_four_text_words<-rep(NA,4)
        #SIMD_merg@data$first_four_images_blipdescr_words<-NA
        
        ## taking only the selected four values for dysplaying 
        for (i in 1:dim(SIMD_merg@data)[1]) {
          ###   #i=892
          print(i)
          try(SIMD_merg@data$first_four_text_words[[i]]<-SIMD_merg@data$text_words[[i]][1:4],silent=F)
          try(SIMD_merg@data$first_four_images_blipdescr_words[[i]]<-SIMD_merg@data$images_blipdescr_words[[i]][1:4],silent=F)
          try(SIMD_merg@data$first_4wors_tags[[i]]<-SIMD_merg@data$tags_words[[i]][1:4],silent=F)
          #try(SIMD_merg@data$first_theme_4blipimg_LDA[[i]]<-SIMD_merg@data$LDA_NOinf_words_blip[[i]][1:2],silent=F)
          ## when i have the error message is actually one of the LDA that was stopped before performing.
          ## THESE ARE NOT MANY, AS IF THE LDA WAS NOT EVEN STARTING, I WILL NOT HAVE NULL BUT NA AND THE ERROR DO NOT APPEAR.
        }
        ## is this loop really necessary? YES
        
        
        
        # Create a color palet891te with handmade bins.
        mybins <- c(seq(0,summary(SIMD_merg@data$Photo_User_Day_un)[6],summary(SIMD_merg@data$Photo_User_Day_un)[6]/8))
        mypalette <- colorBin( palette="YlOrBr", domain=SIMD_merg@data$Photo_User_Day_un, na.color="transparent", bins=mybins)
        # Prepare the text for tooltips:
        mytext <- paste(
          "Data Zone: ", SIMD_merg@data$DataZone,"<br/>", 
          "Area: ", round(as.numeric(SIMD_merg@data$StdAreaKm2), 1), "<br/>", 
          
          "SIMD Rank: ", SIMD_merg@data$SIMD2020v2_Rank, "<br/>", 
          
          "Population: ", ((SIMD_merg@data$TotPop2011)), "<br/>", 
          "Nr. img.: ", as.numeric(SIMD_merg@data$nr.image), "<br/>", 
          "Nr. PUD: ", as.numeric(SIMD_merg@data$Photo_User_Day_un), "<br/>", 
            "Post sent.: ", round(SIMD_merg@data$mean_Resc_HUM, 2), "<br/>",
          
          #"Pos_HUM.sent: ", round(SIMD_merg@data$mean_Positive_HUM, 2), "<br/>",
          #"Neg_HUM.sent: ", round(SIMD_merg@data$mean_Negative_HUM, 2), "<br/>",
         # "Neut_HUM.sent: ", round(SIMD_merg@data$mean_Neutral_HUM, 2), "<br/>", 
          
          "AI-caption sent.: ", round(SIMD_merg@data$mean_Resc_AI, 2), "<br/>",
          
         # "Pos_AI.sent: ", round(SIMD_merg@data$mean_Positive_AI, 2), "<br/>",
        #  "Neg_AI.sent: ", round(SIMD_merg@data$mean_Negative_AI, 2), "<br/>",
         # "Neut_AI.sent: ", round(SIMD_merg@data$mean_Neutral_AI, 2), "<br/>", 
          
          
          "Post words: ", (SIMD_merg@data[["first_four_text_words"]]), "<br/>", 
          "AI-capt. words: ", SIMD_merg@data[["first_four_images_blipdescr_words"]] , "<br/>" ,
          "Tags: ", (SIMD_merg@data$first_4wors_tags), "<br/>", 
          #"Tags: ", SIMD_merg@data$tags_words_freq[1:4] , "<br/>" ,
          
          
          
          sep="") %>%
          lapply(htmltools::HTML)
        # Final Map PUD
        m<- leaflet(SIMD_merg) %>% 
          addTiles()  %>% 
          setView( lat=55.8, lng=-4.3 , zoom=5) %>%
          addPolygons( 
            fillColor = ~mypalette(Photo_User_Day_un), 
            stroke=TRUE, 
            fillOpacity = 0.9, 
            color="white", 
            weight=0.3,
            label = mytext,
            labelOptions = labelOptions( 
              style = list("font-weight" = "normal", padding = "3px 8px"), 
              textsize = "13px", 
              direction = "auto"
            )
          ) %>%
          addLegend( pal=mypalette, values=~Photo_User_Day_un, opacity=0.9, title = "Nr.PUD", position = "bottomleft" )
        
        m  
        #saveWidget(m, file= paste("Flicker - query maps/_","GCC - vandal related PUD only GCC","_Data.html"))
        saveWidget(m, file= paste("Data hub maps/_",sanitized_path,"_Data zones map_Flicker_GCR.html"))
        
        
        #### GCR no tags aggregated maps 
        # Prepare the text for tooltips:
        mytext <- paste(
          "Data Zone: ", SIMD_merg@data$DataZone,"<br/>", 
          "Area: ", round(as.numeric(SIMD_merg@data$StdAreaKm2), 1), "<br/>", 
          "SIMD Rank: ", SIMD_merg@data$SIMD2020v2_Rank, "<br/>", 
          
          "Population: ", ((SIMD_merg@data$TotPop2011)), "<br/>", 
          "Nr. img.: ", as.numeric(SIMD_merg@data$nr.image), "<br/>", 
          "Nr. PUD: ", as.numeric(SIMD_merg@data$Photo_User_Day_un), "<br/>", 
          "Post sent.: ", round(SIMD_merg@data$mean_Resc_HUM, 2), "<br/>",
          
          #"Pos_HUM.sent: ", round(SIMD_merg@data$mean_Positive_HUM, 2), "<br/>",
         # "Neg_HUM.sent: ", round(SIMD_merg@data$mean_Negative_HUM, 2), "<br/>",
          #"Neut_HUM.sent: ", round(SIMD_merg@data$mean_Neutral_HUM, 2), "<br/>", 
          
          "AI-capt. sent.: ", round(SIMD_merg@data$mean_Resc_AI, 2), "<br/>",
         # #
         # "Pos_AI.sent: ", round(SIMD_merg@data$mean_Positive_AI, 2), "<br/>",
         # "Neg_AI.sent: ", round(SIMD_merg@data$mean_Negative_AI, 2), "<br/>",
         # "Neut_AI.sent: ", round(SIMD_merg@data$mean_Neutral_AI, 2), "<br/>", 
          
          #  "Img_descr: ", (SIMD_merg@data[["first_four_text_words"]]), "<br/>", 
          "AI-capt. words: ", SIMD_merg@data[["first_four_images_blipdescr_words"]] , "<br/>" ,
          # "Tags: ", (SIMD_merg@data$first_4wors_tags), "<br/>", ## once again remove the tags
          #"Tags: ", SIMD_merg@data$tags_words_freq[1:4] , "<br/>" ,
          
          
          
          sep="") %>%
          lapply(htmltools::HTML)
        # Final Map PUD
        m<- leaflet(SIMD_merg) %>% 
          addTiles()  %>% 
          setView( lat=55.8, lng=-4.3 , zoom=5) %>%
          addPolygons( 
            fillColor = ~mypalette(Photo_User_Day_un), 
            stroke=TRUE, 
            fillOpacity = 0.9, 
            color="white", 
            weight=0.3,
            label = mytext,
            labelOptions = labelOptions( 
              style = list("font-weight" = "normal", padding = "3px 8px"), 
              textsize = "13px", 
              direction = "auto"
            )
          ) %>%
          addLegend( pal=mypalette, values=~Photo_User_Day_un, opacity=0.9, title = "Nr.PUD", position = "bottomleft" )
        
        m  
        #saveWidget(m, file= paste("Flicker - query maps/_","GCC - vandal related PUD only GCC","_Data.html"))
        saveWidget(m, file= paste("Data hubs no tags/_",sanitized_path,"_Data zones map_Flicker_GCR.html"))
        
        
        SIMD_merg_save<-SIMD_merg@data[, -which(colnames(SIMD_merg@data) %in% c("tags_words","tags_words_freq"))]
        
        save( SIMD_merg_save, file=paste("Data hubs no tags/_",sanitized_path,"_aggregated_df_topic_DATAZONES_GCR.Rda",sep=""))
        
        
        #file=paste("V_2 Twitter pipeline/New images results/Almost def images/",Q,"_aggregated_df_topics.Rda",sep="")
      }else{print(paste("not enough post for:", Q))}
    }
    
    
    
    
    
    
    ########### another loop to re-do the analyses, but exporting the data in a converted ESRI shapefile format #############
    # chat think it is possible. 
    library(rvest)
    library(jsonlite)
    library(sf)
    
    ################################################# After re-uploading the all things, I just need to 
    ### see if i can save this for the hub? 
    # Option A: Save as Shapefile (zipped, good for ArcGIS Online upload)
    st_write(tweets_4_hash_clean1que[,1:33], "Flicks_export.shp")
    st_write(tweets_4_hash_clean1que[,1:33], "Flicks_export", driver = "ESRI Shapefile")
    library(sf)
    
    # Create sf object from coordinates
    ## THIS WORKED TO SAVE THE POINTS 
    tweets_sf <- st_as_sf(tweets_4_hash_clean1que[,1:44], coords = c("longitude", "latitude"), crs = 4326) ## this worked for the points dataset and it is good. 
    st_write(tweets_sf, "Flicks_export", driver = "ESRI Shapefile") ## this worked quite well to export hte file in a format which is read by ESRI
    
    ## for some of these I just need to get the dataset clean of the list, particularly when I have aggregated at a datazone level, so
    
    ## I am not sure this will also work with shapefiles. withouth first putting this toghether with the SIMD. 
    ## another issue is that if i took there all the columns i then get an error, I think it is because there are lists there. 
    sapply(tweets_sf, class)
    # it may be datetaken which is not even a list column number 27 
    library(dplyr)
    
    
    # Export as shapefile
   # st_write(tweets_sf, "Flicks_export", driver = "ESRI Shapefile") ## this worked quite well to export hte file in a format which is read by ESRI
    
    # Create a zip archive
    #zip("Flicks_export.zip", files = list.files(pattern = "Flicks_export\\.(shp|shx|dbf|prj)$"))
    # Option B: Save as GeoJSON (widely supported and easy to use)
    #st_write(tweets_4_hash_clean1que[,1:33], "Flicks_export.geojson")
    
    ### THIS WORKED (WITHOUT SAVING ALL COLUMNS), BUT THEN IT ALL DOES NOT WORK IN THE LOOP SO I NEED A CHANGE AND IT PROBABLY RELATES TO THE LISTS COLUMNS AGAIN
    ###########################################
    
    ## which rows may i need now
  #  colnames(SIMD_merg@data) ## since we have aproblem with lists
  #  class(SIMD_merg@data$first_four_images_blipdescr_words)
   # View(SIMD_merg@data)
   #unlist( SIMD_merg@data$first_four_images_blipdescr_words[1:10])
    
   ### since i will want to also show the first four words, i may need to get rid of the lists, however  
  # a<-as.vector(c(unlist(SIMD_merg@data$first_four_images_blipdescr_words[900]))) # if i send this and get rid of the list, then i will still need to get only one value out of the 4, to have the same number of dimensions.
   ## for now i guess we can go ahead with the 4 columns approach, another idea is to transform the four values in a sentence but this may be difficult and useless since there will still need a convertion for numbers as well. 
  # class(a)
  # a<-paste(a[1],a[2], a[3], a[4], sep=" ")
   ## similarly with numbers
   #a<-as.vector(c(unlist(SIMD_merg@data$first_four_images_blipdescr_words [900]))) # if i send this and get rid of the list, then i will still need to get only one value out of the 4, to have the same number of dimensions.
   
   ### this worked TO SAVE THE shpefiles.!!
   # SIMD_merg_cahnged<-    st_as_sf(SIMD_merg[,c(1:9,93)], crs = 4326) # as soon as we put a list this change 

  #  class(SIMD_merg_cahnged)
    #st_write(SIMD_merg_cahnged[,], "Flicks_export_SIMD_MERGED_4.shp", driver = "ESRI Shapefile", delete_dsn = TRUE) ## this worked quite well to export hte file in a format which is read by ESRI
    ### 
    #st_write(SIMD_merg_cahnged, paste("shapes/_","/",sanitized_path,"Flicks_export_SIMD_MERGED.shp", sep=""), driver = "ESRI Shapefile", delete_dsn = TRUE)
    ## THEN I NEED TO PUT EVERYTHING INTO A FILE AND SAVE this can be uploaded to ArcGIS online.
    
    #### i first need to upload all the data from the previous loop ###########
    for (Q in wordAGGR$V2[] ) {
      #Q="WP2" ## trial 
      #Q="WP1_only" ## trial 
      que<-wordAGGR[wordAGGR$V2==Q,"V1"]
      ## subsetting for the selected query 
      tweets_4_hash_clean1que<-flick_all_SIMD[ grepl(que, flick_all_SIMD$tags_original),] ### the modification is to do that with the original tags, since for firstbus, it may remove the word bus
      ## are there duplicates in the query?
      sum(duplicated(tweets_4_hash_clean1que$id)) ## why i do not have duplicates? these must have been screened out before?
      
      #save(tweets_4_hash_clean1que,file="only_trees_flicker.Rda")
      #write.csv(tweets_4_hash_clean1que,file="only_trees_flicker.csv")
      if (dim(tweets_4_hash_clean1que)[1]>0) {
        #que<-wordAGGR[wordAGGR$V2==Q,"V1"]
        #tweets_4_hash_clean1que<-tweets_4_hash_clean1que[!duplicated(tweets_4_hash_clean1que$id),]
        #tweets_4_hash_clean1que$que<-rep(Q, dim(tweets_4_hash_clean1que)[1])
        DZ<-unique(tweets_4_hash_clean1que$DataZone) ## not many for the trial query
        
        
        
        
        
        
        
        #### continue with the older code
        
        ### for CNN
        tweets_4_hash_clean1que$FasterRCNN_Labelsmore_07 <- lapply(tweets_4_hash_clean1que$FasterRCNN_Labels, parseJSONSafely)
        tweets_4_hash_clean1que$FasterRCNN_Scoressmore_07 <- lapply(tweets_4_hash_clean1que$FasterRCNN_Scores, parseJSONSafely)
        # Filter labels by scores
        tweets_4_hash_clean1que$FasterRCNN_Labelsmore_07 <- mapply(function(labels, scores) {
          labels[scores > 0.75]
        },  tweets_4_hash_clean1que$FasterRCNN_Labelsmore_07 , tweets_4_hash_clean1que$FasterRCNN_Scoressmore_07, SIMPLIFY = FALSE)
        ## for yolos
        # Convert character strings to lists
        tweets_4_hash_clean1que$YOLOS_Labelsmore_07 <- lapply(tweets_4_hash_clean1que$YOLOS_Labels, parseJSONSafely)
        tweets_4_hash_clean1que$YOLOS_Scoresmore_07 <- lapply(tweets_4_hash_clean1que$YOLOS_Scores, parseJSONSafely)
        # Filter labels by scores
        tweets_4_hash_clean1que$YOLOS_Labelsmore_07 <- mapply(function(labels, scores) {
          labels[scores > 0.75]
        },  tweets_4_hash_clean1que$YOLOS_Labelsmore_07 , tweets_4_hash_clean1que$YOLOS_Scoresmore_07, SIMPLIFY = FALSE)
        
        #dz="S01011874"
        # print(dz)
        #library(lubridate)
        tweets_4_hash_clean1que$datetaken<-ymd_hms(tweets_4_hash_clean1que$datetaken)
        tweets_4_hash_clean1que$monthdayyear = format(tweets_4_hash_clean1que$datetaken, "%m-%d-%y")
        ## modify Q for saving 
        sanitized_path <- gsub("[/:\"\\\\]", "_", Q)
        
        
        ### plotting the points ere 
        # Create a color palet891te with handmade bins.
        
        #library(leaflet)
        tweets_4_hash_clean1que$resc_sent<-tweets_4_hash_clean1que$H_desc_Positive-tweets_4_hash_clean1que$H_desc_Negative
        tweets_4_hash_clean1que$resc_sent_AI<-tweets_4_hash_clean1que$Positive-tweets_4_hash_clean1que$Negative
        
        
        
        
        
        #library(htmltools)
        # mybins <- c(seq(0,summary(tweets_4_hash_clean1que$resc_sent)[6],summary(tweets_4_hash_clean1que$resc_sent)[6]/5))
       # mybins <- c(seq(-1,1,0.4))
        
        #mypalette <- colorBin( palette=rev("RdYlBu"), domain=tweets_4_hash_clean1que$resc_sent_AI, na.color="transparent", bins=mybins)
       # mypalette <- colorBin( palette=("RdYlBu"), domain=tweets_4_hash_clean1que$resc_sent_AI, na.color="transparent", bins=-mybins, reverse = T)
        
        # Prepare the text for tooltips:
        #mytext <- paste(
         # "Data Zone: ", tweets_4_hash_clean1que$DataZone,"<br/>", 
          #"Area: ", tweets_4_hash_clean1que$StdAreaKm2, "<br/>", 
          #"SIMD Rank: ", tweets_4_hash_clean1que$SIMD2020v2_Rank, "<br/>", 
          #"Population: ", ((tweets_4_hash_clean1que$TotPop2011)), "<br/>", 
          ##"Nr.img: ", as.numeric(tweets_4_hash_clean1que$nr.image), "<br/>", 
          ##"Nr.PUD: ", as.numeric(tweets_4_hash_clean1que$Photo_User_Day_un), "<br/>", 
          #"post_title: ", (tweets_4_hash_clean1que$title), "<br/>", 
          
        #  "user_post: ", (tweets_4_hash_clean1que$description), "<br/>", 
         # "Resc_HUM.sent: ", round(tweets_4_hash_clean1que$resc_sent, 2), "<br/>",
          #"Pos_HUM.sent: ", round(tweets_4_hash_clean1que$H_desc_Positive, 2), "<br/>",
          #"Neg_HUM.sent: ", round(tweets_4_hash_clean1que$H_desc_Negative, 2), "<br/>",
          #"Neut_HUM.sent: ", round(tweets_4_hash_clean1que$H_desc_Neutral, 2), "<br/>", 
          #"Img_URL: ", (tweets_4_hash_clean1que$url_l ), "<br/>", 
          #
          #"AI_img_descr: ", tweets_4_hash_clean1que$Caption_blip , "<br/>" ,
          #"Resc_AI.sent: ", round(tweets_4_hash_clean1que$Positive-tweets_4_hash_clean1que$Negative, 2), "<br/>",
          #"Pos_AI.sent: ", round(tweets_4_hash_clean1que$Positive, 2), "<br/>",
          #"Neg_AI.sent: ", round(tweets_4_hash_clean1que$Negative, 2), "<br/>",
          #"Neut_AI.sent: ", round(tweets_4_hash_clean1que$Neutral, 2), "<br/>", 
          #"Tags: ", (tweets_4_hash_clean1que$tags), "<br/>", 
          ##"Tags: ", SIMD_merg@data$tags_words_freq[1:4] , "<br/>" ,
          #
        #  
        #  
        #  sep="") %>%
        #  lapply(htmltools::HTML)
        ##library(sf)
        #tweets_sf <- st_as_sf(tweets_4_hash_clean1que, coords = c("longitude", "latitude"), crs = 4326)
      #  
      #  # Final Map points?
        # Assuming tweets_sf is already converted to an sf object with points
   #     m <- leaflet(tweets_sf) %>%
   #       addTiles() %>%
   #       setView(lat = 55.8, lng = -4.3, zoom = 5) %>%
   #       addCircleMarkers(
   #         fillColor = ~mypalette(resc_sent_AI),
   #         stroke = TRUE,
   #         fillOpacity = 0.9,
   #         color = "white",
  #          weight = 0.3,
  #          popup = mytext,
  #          labelOptions = labelOptions(
  #            style = list("font-weight" = "normal", padding = "3px 8px"),
  #            textsize = "13px",
  #            direction = "auto"
  #          )
  #        ) %>%
  #        addLegend(
  #          pal = mypalette,
  #          values = ~resc_sent_AI,
  #          opacity = 0.9,
  #          title = "AI. Rescaled Sentiment",
  #          position = "bottomleft"
  #        )
  #      
  #      # Print the map
  #      m
  #      #library(htmlwidgets)
  #      saveWidget(m, file= paste("Data hub maps/_",sanitized_path,"pointmap_Flicker.html"))
        
        ######### points all, no tags 
        # Prepare the text for tooltips:
   #     mytext <- paste(
    #      "Data Zone: ", tweets_4_hash_clean1que$DataZone,"<br/>", 
    #      "Area: ", tweets_4_hash_clean1que$StdAreaKm2, "<br/>", 
    #      "SIMD Rank: ", tweets_4_hash_clean1que$SIMD2020v2_Rank, "<br/>", 
    #      "Population: ", ((tweets_4_hash_clean1que$TotPop2011)), "<br/>", 
    #      #"Nr.img: ", as.numeric(tweets_4_hash_clean1que$nr.image), "<br/>", 
    #      #"Nr.PUD: ", as.numeric(tweets_4_hash_clean1que$Photo_User_Day_un), "<br/>", 
    #      "post_title: ", (tweets_4_hash_clean1que$title), "<br/>", 
    #      
    #      "user_post: ", (tweets_4_hash_clean1que$description), "<br/>", 
    #      "Resc_HUM.sent: ", round(tweets_4_hash_clean1que$resc_sent, 2), "<br/>",
    #      "Pos_HUM.sent: ", round(tweets_4_hash_clean1que$H_desc_Positive, 2), "<br/>",
    #      "Neg_HUM.sent: ", round(tweets_4_hash_clean1que$H_desc_Negative, 2), "<br/>",
    #      "Neut_HUM.sent: ", round(tweets_4_hash_clean1que$H_desc_Neutral, 2), "<br/>", 
    #      "Img_URL: ", (tweets_4_hash_clean1que$url_l ), "<br/>", 
    #      
    #      "AI_img_descr: ", tweets_4_hash_clean1que$Caption_blip , "<br/>" ,
    #      "Resc_AI.sent: ", round(tweets_4_hash_clean1que$Positive-tweets_4_hash_clean1que$Negative, 2), "<br/>",
    #      "Pos_AI.sent: ", round(tweets_4_hash_clean1que$Positive, 2), "<br/>",
    #      "Neg_AI.sent: ", round(tweets_4_hash_clean1que$Negative, 2), "<br/>",
    #      "Neut_AI.sent: ", round(tweets_4_hash_clean1que$Neutral, 2), "<br/>", 
    #      #"Tags: ", (tweets_4_hash_clean1que$tags), "<br/>", ## in this run i remove the tags
    #      #"Tags: ", SIMD_merg@data$tags_words_freq[1:4] , "<br/>" ,
    #      
    #      
    #      
    #      sep="") %>%
    #      lapply(htmltools::HTML)
    #    #library(sf)
    #    tweets_sf <- st_as_sf(tweets_4_hash_clean1que, coords = c("longitude", "latitude"), crs = 4326)
    #    
        # Final Map points?
        # Assuming tweets_sf is already converted to an sf object with points
    #    m <- leaflet(tweets_sf) %>%
    #      addTiles() %>%
    #      setView(lat = 55.8, lng = -4.3, zoom = 5) %>%
    #      addCircleMarkers(
    #        fillColor = ~mypalette(resc_sent_AI),
    ##        stroke = TRUE,
    #        fillOpacity = 0.9,
    #        color = "white",
    #        weight = 0.3,
    #        popup = mytext,
    #        labelOptions = labelOptions(
    #          style = list("font-weight" = "normal", padding = "3px 8px"),
    #          textsize = "13px",
    #          direction = "auto"
    #        )
    #      ) %>%
    #      addLegend(
    #        pal = mypalette,
    #        values = ~resc_sent_AI,
    #        opacity = 0.9,
    #        title = "AI. Rescaled Sentiment",
    #        position = "bottomleft"
    #      )
    #    
        # Print the map
    #    m
    #    #library(htmlwidgets)
    #    saveWidget(m, file= paste("Data hubs no tags/_",sanitized_path,"pointmap_Flicker.html"))
    #    tweets_4_hash_clean1que_save<-tweets_4_hash_clean1que[, -which(colnames(tweets_4_hash_clean1que) %in% c("tags","tags_original"))]
    #    save( tweets_4_hash_clean1que, file=paste("Data hubs no tags/_",sanitized_path,"_pointmap.Rda",sep=""))
    #    
        
        
        
        ## the same, points but only for GCR area
        as<-tweets_4_hash_clean1que[ tweets_4_hash_clean1que$Council_area %in% CGR_names,]
        as$resc_sent<-as$H_desc_Positive-as$H_desc_Negative
        as$resc_sent_AI<-as$Positive-as$Negative
        
        ### points map for only GCC datazones
        #mybins <- c(seq(0,summary(as$Positive)[6],summary(as$Positive)[6]/8))
    #    mybins <- c(seq(-1,1,0.4))
    #    
    #    mypalette <- colorBin( palette=("RdYlBu"), domain=as$resc_sent_AI, na.color="transparent", bins=-mybins, reverse = T)
    #    
    #    # Prepare the text for tooltips:
    #    mytext <- paste(
    #      "Data Zone: ", as$DataZone,"<br/>", 
   #       "Area: ", as$StdAreaKm2, "<br/>", 
  #        "SIMD Rank: ", as$SIMD2020v2_Rank, "<br/>", 
  #        "Population: ", ((as$TotPop2011)), "<br/>", 
  #        #"Nr.img: ", as.numeric(as$nr.image), "<br/>", 
  #        #"Nr.PUD: ", as.numeric(as$Photo_User_Day_un), "<br/>", 
  #        "post_title: ", (as$title), "<br/>", 
  #        
  #        "user_post: ", (as$description), "<br/>", 
  #        "Resc_HUM.sent: ", round(as$resc_sent, 2), "<br/>",
  #        
  #        "Pos_HUM.sent: ", round(as$H_desc_Positive, 2), "<br/>",
  #        "Neg_HUM.sent: ", round(as$H_desc_Negative, 2), "<br/>",
  #        "Neut_HUM.sent: ", round(as$H_desc_Neutral, 2), "<br/>", 
  #        "Img_URL: ", (as$url_l ), "<br/>", 
  #        
  #        "AI_img_descr: ", as$Caption_blip , "<br/>" ,
  #        "Resc_AI.sent: ", round(as$Positive-as$Negative, 2), "<br/>",
  #        
  #        "Pos_AI.sent: ", round(as$Positive, 2), "<br/>",
  #        "Neg_AI.sent: ", round(as$Negative, 2), "<br/>",
  #        "Neut_AI.sent: ", round(as$Neutral, 2), "<br/>", 
  #        "Tags: ", (as$tags), "<br/>", 
  #        #"Tags: ", SIMD_merg@data$tags_words_freq[1:4] , "<br/>" ,
  #        
  #        
  #        
  #        sep="") %>%
  #        lapply(htmltools::HTML)
  #      library(sf)
  #      tweets_sf <- st_as_sf(as, coords = c("longitude", "latitude"), crs = 4326)
        
        # Final Map PUD
        # Assuming tweets_sf is already converted to an sf object with points
  #      m <- leaflet(tweets_sf) %>%
  #        addTiles() %>%
  #        setView(lat = 55.8, lng = -4.3, zoom = 5) %>%
  #        addCircleMarkers(
  #          fillColor = ~mypalette(resc_sent_AI),
  #          stroke = TRUE,
  #          fillOpacity = 0.9,
  #          color = "white",
  #          weight = 0.3,
  #          popup = mytext,
  #          labelOptions = labelOptions(
  #            style = list("font-weight" = "normal", padding = "3px 8px"),
  #            textsize = "13px",
  #            direction = "auto"
  #          )
  #        ) %>%
  #        addLegend(
  #          pal = mypalette,
  #          values = ~resc_sent_AI,
  #          opacity = 0.9,
  #          title = "AI. Rescaled Sentiment",
  #          position = "bottomleft"
  #        )
  #      
        # Print the map
  #      m
  #      
  #      library(htmlwidgets)
  #      saveWidget(m, file= paste("Data hub maps/_",sanitized_path,"_GCR_pointmap_Flicker.html"))
        
        
        
        ##### points GCR no tags 
        # Prepare the text for tooltips:
  #      mytext <- paste(
  #        "Data Zone: ", as$DataZone,"<br/>", 
  #        "Area: ", as$StdAreaKm2, "<br/>", 
  #        "SIMD Rank: ", as$SIMD2020v2_Rank, "<br/>", 
  #        "Population: ", ((as$TotPop2011)), "<br/>", 
  #        #"Nr.img: ", as.numeric(as$nr.image), "<br/>", 
  #        #"Nr.PUD: ", as.numeric(as$Photo_User_Day_un), "<br/>", 
  #        "post_title: ", (as$title), "<br/>", 
  #        
  #        "user_post: ", (as$description), "<br/>", 
  #        "Resc_HUM.sent: ", round(as$resc_sent, 2), "<br/>",
  #        
  #        "Pos_HUM.sent: ", round(as$H_desc_Positive, 2), "<br/>",
  #        "Neg_HUM.sent: ", round(as$H_desc_Negative, 2), "<br/>",
  #        "Neut_HUM.sent: ", round(as$H_desc_Neutral, 2), "<br/>", 
  #        "Img_URL: ", (as$url_l ), "<br/>", 
  #        
  #        "AI_img_descr: ", as$Caption_blip , "<br/>" ,
  #        "Resc_AI.sent: ", round(as$Positive-as$Negative, 2), "<br/>",
  #        
  #        "Pos_AI.sent: ", round(as$Positive, 2), "<br/>",
  #        "Neg_AI.sent: ", round(as$Negative, 2), "<br/>",
  #        "Neut_AI.sent: ", round(as$Neutral, 2), "<br/>", 
  #        #"Tags: ", (as$tags), "<br/>", ## removed this as well in this run
  #        #"Tags: ", SIMD_merg@data$tags_words_freq[1:4] , "<br/>" ,
  #        
  #        
  #        
  #        sep="") %>%
  #        lapply(htmltools::HTML)
  #      library(sf)
  #      tweets_sf <- st_as_sf(as, coords = c("longitude", "latitude"), crs = 4326)
  #      
  #      # Final Map PUD
  #      # Assuming tweets_sf is already converted to an sf object with points
  #      m <- leaflet(tweets_sf) %>%
  #        addTiles() %>%
  #        setView(lat = 55.8, lng = -4.3, zoom = 5) %>%
 #         addCircleMarkers(
# #           fillColor = ~mypalette(resc_sent_AI),
   #         stroke = TRUE,
    #        fillOpacity = 0.9,
    #        color = "white",
    #        weight = 0.3,
    #        popup = mytext,
    #        labelOptions = labelOptions(
    #          style = list("font-weight" = "normal", padding = "3px 8px"),
    #          textsize = "13px",
    #          direction = "auto"
    #        )
    #      ) %>%
    #      addLegend(
    #        pal = mypalette,
    #        values = ~resc_sent_AI,
  #          opacity = 0.9,
  #          title = "AI. Rescaled Sentiment",
  #          position = "bottomleft"
  #        )
  #      
        # Print the map
  #      m
  #      
  #      library(htmlwidgets)
  #      saveWidget(m, file= paste("Data hubs no tags/_",sanitized_path,"_GCRpointmap_Flicker.html"))
  #      as_save<-as[, -which(colnames(as) %in% c("tags","tags_original"))]
#        save( as_save, file=paste("Data hubs no tags/_",sanitized_path,"_GCR_pointmap.Rda",sep=""))
        
        
        
        
        
        
        
        pud_results <- tweets_4_hash_clean1que %>%
          group_by(DataZone,ownername , monthdayyear) %>%
          summarise(
            Photo_User_Day = n_distinct(id),  # Replace 'photo_id' with your photo identifier column
            .groups = 'drop'  # Drop grouping for the next operations
          )
        ## these count the number of pictures each day in each datazone done by users. However, each of them should count as one. 
        pud_results$un_count<-rep(1,length(pud_results$DataZone))
        ## aggregate again 
        pud_results_un <- pud_results %>%
          group_by(DataZone) %>%
          summarise(
            Photo_User_Day_un = sum(un_count),
            names=list(ownername),
            days=list(monthdayyear)# Replace 'photo_id' with your photo identifier column
            # Drop grouping for the next operations
          )
        
        
        aggregated_df_topics <- tweets_4_hash_clean1que[  ,] %>%
          group_by(DataZone) %>%
          summarize(
            #que=first(que),
            mean_Resc_AI = mean(Positive-Negative),
            list_Resc_AI = list(Positive-Negative),
            
            mean_Positive_AI = mean(Positive),
            all_Positive_AI=list(Positive),
            mean_Negative_AI = mean(Negative),
            all_Negative_AI=list(Negative),
            mean_Neutral_AI = mean(Neutral),
            all_Neutral_AI=list(Neutral),
            
            
            mean_Resc_HUM = mean(H_desc_Positive-H_desc_Negative, na.rm = TRUE),
            list_Resc_HUM = list(H_desc_Positive-H_desc_Negative),
            
            mean_Positive_HUM = mean(H_desc_Positive, na.rm = TRUE),
            all_Positive_HUM=list(H_desc_Positive),
            mean_Negative_HUM = mean(H_desc_Negative, na.rm = TRUE),
            all_Negative_HUM=list(H_desc_Negative),
            mean_Neutral_HUM = mean(H_desc_Neutral, na.rm = TRUE),
            all_Neutral_HUM=list(H_desc_Neutral),
            
            TOT_views=sum(as.numeric(views)),
            flick_ID = list(id),
            ### add here id of the tweets 
            #      Nr.pos=sum(Sentiment==1),
            #       Nr.neg=sum(Sentiment==-1),
            ##        Nr.neut=sum(Sentiment==0),
            Nr.posts = length(na.omit(Neutral)),
            text_words=list(try(process_corpus_words(description)$word)),
            text_words_freq=list(try(process_corpus_words(description)$freq)),
            tags_words=list(try(process_corpus_words(tags)$word)),
            tags_words_freq=list(try(process_corpus_words(tags)$freq)),
            ## need to add the LDA for text
            
            ###THESE LDA WORKS BUT ARE LONG TO COMPUTE
            #LDA_inf_coherence_text=list(try(LDA_inf(description)$coherence)),
            #LDA_inf_prevalence_text=list(try(LDA_inf(description)$prevalence)),
            #LDA_inf_words_text=list(try(LDA_inf(description)$top_terms)),
            
            #LDA_NOinf_coherence_text=list(try(LDA(description)$coherence)),
            #LDA_NOinf_prevalence_text=list(try(LDA(description)$prevalence)),
            #LDA_NOinf_words_text=list(try(LDA(description)$top_terms)),
            
            nr.image=length(na.omit(url_l)),
            #      #nr.image_nomemes_elaborated=length(na.omit(TW_obj[TW_obj$image_path %in% Media_url & TW_obj$method=="Caption_blip" ,"text" ] )),
            #      #URL_nomemes=(na.omit(TW_obj[TW_obj$image_path %in% Media_url & TW_obj$method=="Caption_blip" ,"image_path" ] )),
            #      nr.image_nomemes_elaborated=length(na.omit(TW_obj[TW_obj$image_path %in% na.omit(Media_url) & TW_obj$method=="Caption_blip" ,"image_path" ] )),
            #      image_nomemes_URL=            list(na.omit(TW_obj[TW_obj$image_path %in% na.omit(Media_url) & TW_obj$method=="Caption_blip" ,"image_path" ] )),
            #      
            url = list(na.omit(url_l)),
            images_blipdescr_words=list(try(process_corpus_words (Caption_blip)$word)),
            images_blipdescr_freq=list(try(process_corpus_words(Caption_blip)$freq)),
            #images_blip_and_gptdescr_words=list(try(process_corpus_words ( c("Caption_blip",  "Text_gpt2") )$word)),
            #images_blip_and_gptdescr_freq=list(try(process_corpus_words(Caption_blip )$freq)),
            #      ## i have eliminated these previous, as it was not working, retrieved the all values 
            #      
            #      LDA_inf_coherence_blip=list(try(LDA_inf(Caption_blip)$coherence)),
            #      LDA_inf_prevalence_blip=list(try(LDA_inf(Caption_blip)$prevalence)),
            #      LDA_inf_words_blip=list(try(LDA_inf(Caption_blip)$top_terms)),
            #     
            #      LDA_NOinf_coherence_blip=list(try(LDA(TW_obj[TW_obj$image_path %in% Media_url & TW_obj$method=="Caption_blip" ,"text" ])$coherence)),
            #     LDA_NOinf_prevalence_blip=list(try(LDA(TW_obj[TW_obj$image_path %in% Media_url & TW_obj$method=="Caption_blip" ,"text" ])$prevalence)),
            #    LDA_NOinf_words_blip=list(try(LDA(TW_obj[TW_obj$image_path %in% Media_url & TW_obj$method=="Caption_blip" ,"text" ])$top_terms)),
            #    
            images_gptdescr_words=list(try(process_corpus_words (Text_gpt2 )$word)),
            images_gptdescr_freq=list(try(process_corpus_words(Text_gpt2 )$freq)),
            #    LDA_inf_coherence_gpt=list(try(LDA_inf(TW_obj[TW_obj$image_path %in% Media_url & TW_obj$method=="Text_gpt2" ,"text" ])$coherence)),
            #    LDA_inf_prevalence_gpt=list(try(LDA_inf(TW_obj[TW_obj$image_path %in% Media_url & TW_obj$method=="Text_gpt2" ,"text" ])$prevalence)),
            #    LDA_inf_words_gpt=list(try(LDA_inf(TW_obj[TW_obj$image_path %in% Media_url & TW_obj$method=="Text_gpt2" ,"text" ])$top_terms)),
            #    
            #    LDA_NOinf_coherence_gpt=list(try(LDA(TW_obj[TW_obj$image_path %in% Media_url & TW_obj$method=="Text_gpt2" ,"text" ])$coherence)),
            #    LDA_NOinf_prevalence_gpt=list(try(LDA(TW_obj[TW_obj$image_path %in% Media_url & TW_obj$method=="Text_gpt2" ,"text" ])$prevalence)),
            #    LDA_NOinf_words_gpt=list(try(LDA(TW_obj[TW_obj$image_path %in% Media_url & TW_obj$method=="Text_gpt2" ,"text" ])$top_terms)),
            #    
            #    
            images_labelsCNN_words=list(try(preprocess_corpus_stem (unlist(FasterRCNN_Labelsmore_07))$word)),
            images_labelsCNN_freq=list(try(preprocess_corpus_stem (unlist(FasterRCNN_Labelsmore_07))$freq)),
            
            images_labelsYOLOS_words=list(try(preprocess_corpus_stem (unlist(YOLOS_Labelsmore_07))$word)),
            images_labelsYOLOS_freq=list(try(preprocess_corpus_stem (unlist(YOLOS_Labelsmore_07))$freq)),
            #images_labelsYOLOS_words=list(try(preprocess_corpus_stem (unlist(tweets_4_hash_clean1que[ unlist(lapply(tweets_4_hash_clean1que$FasterRCNN_Scores, function(x) {
            # Remove non-numeric characters, brackets, and extra spaces, keeping only numbers, decimal points, and commas
            
            # images_labelsYOLOS_freq=list(try(preprocess_corpus_stem(unlist(tweets_4_hash_clean1que[YOLOS_Scores>=0.75 ,"FasterRCNN_Scores" ] )$freq))),
            
            
            # images_labelsYOLOS_words=list(try(preprocess_corpus_stem (TW_obj[TW_obj$image_path %in% Media_url & TW_obj$method=="YOLOS" & score>=0.75 ,"label" ] )$word)),
            #  images_labelsYOLOS_freq=list(try(preprocess_corpus_stem(TW_obj[TW_obj$image_path %in% Media_url & TW_obj$method=="YOLOS" & score>=0.75,"label" ] )$freq)),
            #    ## in the next version it would be better to analyse also resnet50 
            images_labelsresnet_words=list(try(preprocess_corpus_stem_resnet (ResNet50_Label )$word)),
            images_labelsresnet_freq=list(try(preprocess_corpus_stem_resnet(ResNet50_Label )$freq)),
            #  
            #"FasterRCNN_Labels","FasterRCNN_Scores","ResNet50_Label","ResNet50_Scores"
            Intermediate_Zone = first(Intermediate_Zone),
            Council_area = first(Council_area),
            Total_population = first(Total_population),
            Working_Age_population = first(Working_Age_population),
            SIMD2020v2_Rank = first(SIMD2020v2_Rank),
            SIMD_Percentile = first(SIMD_2020v2_Percentile),
            SIMD_Vigintile = first(SIMD2020v2_Vigintile),
            SIMD_Decile = first(SIMD2020v2_Decile),
            SIMD_Quintile = first(SIMD2020v2_Quintile),
            SIMD_Income_Domain_Rank = first(SIMD2020v2_Income_Domain_Rank),
            SIMD_Employment_Domain_Rank = first(SIMD2020_Employment_Domain_Rank),
            SIMD_Health_Domain_Rank = first(SIMD2020_Health_Domain_Rank),
            SIMD_Education_Domain_Rank = first(SIMD2020_Education_Domain_Rank),
            SIMD_Access_Domain_Rank = first(SIMD2020_Access_Domain_Rank),
            SIMD_Crime_Domain_Rank = first(SIMD2020_Crime_Domain_Rank),
            SIMD_Housing_Domain_Rank = first(SIMD2020_Housing_Domain_Rank),
            income_rate = first(income_rate),
            income_count = first(income_count),
            employment_rate = first(employment_rate),
            employment_count = first(employment_count),
            CIF = first(CIF),
            ALCOHOL = first(ALCOHOL),
            DRUG = first(DRUG),
            SMR = first(SMR),
            DEPRESS = first(DEPRESS),
            LBWT = first(LBWT),
            EMERG = first(EMERG),
            Attendance = first(Attendance),
            Attainment = first(Attainment),
            no_qualifications = first(no_qualifications),
            not_participating = first(not_participating),
            University = first(University),
            crime_count = first(crime_count),
            crime_rate = first(crime_rate),
            overcrowded_count = first(overcrowded_count),
            nocentralheating_count = first(nocentralheating_count),
            overcrowded_rate = first(overcrowded_rate),
            nocentralheating_rate = first(nocentralheating_rate),
            drive_petrol = first(drive_petrol),
            drive_GP = first(drive_GP),
            drive_post = first(drive_post),
            drive_primary = first(drive_primary),
            drive_retail = first(drive_retail),
            drive_secondary = first(drive_secondary),
            PT_GP = first(PT_GP),
            PT_post = first(PT_post),
            PT_retail = first(PT_retail),
            broadband = first(broadband)
          )
        
        
        
        
        sum(aggregated_df_topics$nr.image)
        ## these should match
        # SAVE the dataset 
        
        
        ### adding hte PUD now 
        # Perform a left join to add Photo_User_Day_un from pud_results_un to aggregated_df_topics
        aggregated_df_topics <- left_join(aggregated_df_topics, 
                                          pud_results_un[, c("DataZone", "Photo_User_Day_un")],
                                          by = "DataZone")    
        
        
        ## modify Q for saving 
        sanitized_path <- gsub("[/:\"\\\\]", "_", Q)
        
        #save( aggregated_df_topics, file=paste("Data hub maps/_",sanitized_path,"_aggregated_df_topic_DATAZONES.Rda",sep=""))
        #colnames(aggregated_df_topics)
        #### plotting the dataset
        
        
        ### adding data i need in the spatial object 
        SIMD_merg <- merge(SIMD, aggregated_df_topics, by.x = "DataZone", by.y = "DataZone")
     #   SIMD_merg@data$first_four_text_words <- vector("list", length = dim(SIMD_merg@data)[1])
        SIMD_merg@data$first_images_blipdescr_words <- vector("character", length = dim(SIMD_merg@data)[1])
        SIMD_merg@data$second_images_blipdescr_words <- vector("character", length = dim(SIMD_merg@data)[1])
        SIMD_merg@data$third_images_blipdescr_words <- vector("character", length = dim(SIMD_merg@data)[1])
        SIMD_merg@data$forth_images_blipdescr_words <- vector("character", length = dim(SIMD_merg@data)[1])
        
    #    SIMD_merg@data$first_4wors_tags<- vector("list", length = dim(SIMD_merg@data)[1])
        #SIMD_merg@data$first_theme_4wors_LDA <- vector("list", length = dim(SIMD_merg@data)[1])
        #SIMD_merg@data$first_theme_4blipimg_LDA <- vector("list", length = dim(SIMD_merg@data)[1])
        
        #SIMD_merg@data$first_four_text_words<-rep(NA,4)
        #SIMD_merg@data$first_four_images_blipdescr_words<-NA
        
        ## taking only the selected four values for dysplaying 
        for (i in 1:dim(SIMD_merg@data)[1]) {
          ###   #i=892
          print(i)
          #try(SIMD_merg@data$first_four_text_words[[i]]<-SIMD_merg@data$text_words[[i]][1:4],silent=F)
          try(SIMD_merg@data$first_images_blipdescr_words[i]<-SIMD_merg@data$images_blipdescr_words[[i]][1],silent=F) ## here the frequencies are ordered actually.
          try(SIMD_merg@data$second_images_blipdescr_words[i]<-SIMD_merg@data$images_blipdescr_words[[i]][2],silent=F) ## here the frequencies are ordered actually.
          try(SIMD_merg@data$third_images_blipdescr_words[i]<-SIMD_merg@data$images_blipdescr_words[[i]][3],silent=F) ## here the frequencies are ordered actually.
          try(SIMD_merg@data$forth_images_blipdescr_words[i]<-SIMD_merg@data$images_blipdescr_words[[i]][4],silent=F) ## here the frequencies are ordered actually.
          
           #try(SIMD_merg@data$first_4wors_tags[[i]]<-SIMD_merg@data$tags_words[[i]][1:4],silent=F)
          #try(SIMD_merg@data$first_theme_4blipimg_LDA[[i]]<-SIMD_merg@data$LDA_NOinf_words_blip[[i]][1:2],silent=F)
          ## when i have the error message is actually one of the LDA that was stopped before performing.
          ## THESE ARE NOT MANY, AS IF THE LDA WAS NOT EVEN STARTING, I WILL NOT HAVE NULL BUT NA AND THE ERROR DO NOT APPEAR.
        }
        ## is this loop really necessary? YES!
        
        
        
        # Create a color palet891te with handmade bins.
        #mybins <- c(seq(0,summary(SIMD_merg@data$Photo_User_Day_un)[6],summary(SIMD_merg@data$Photo_User_Day_un)[6]/8))
        #mypalette <- colorBin( palette="YlOrBr", domain=SIMD_merg@data$Photo_User_Day_un, na.color="transparent", bins=mybins)
        # Prepare the text for tooltips:
        #mytext <- paste(
        #  "Data Zone: ", SIMD_merg@data$DataZone,"<br/>", 
        #  "Area: ", round(as.numeric(SIMD_merg@data$StdAreaKm2), 1), "<br/>",
        #  "SIMD Rank: ", SIMD_merg@data$SIMD2020v2_Rank, "<br/>", 
        #  
        #  "Population: ", ((SIMD_merg@data$TotPop2011)), "<br/>", 
        #  "Nr. img.: ", as.numeric(SIMD_merg@data$nr.image), "<br/>", 
        #  "Nr. PUD: ", as.numeric(SIMD_merg@data$Photo_User_Day_un), "<br/>", 
        #  
        #  "Resc_AI.sent: ", round(SIMD_merg@data$mean_Resc_AI, 2), "<br/>",
        #  
        #  "Pos_AI.sent: ", round(SIMD_merg@data$mean_Positive_AI, 2), "<br/>",
        #  "Neg_AI.sent: ", round(SIMD_merg@data$mean_Negative_AI, 2), "<br/>",
        #  "Neut_AI.sent: ", round(SIMD_merg@data$mean_Neutral_AI, 2), "<br/>", 
        #  
        #  "Resc_HUM.sent: ", round(SIMD_merg@data$mean_Resc_HUM, 2), "<br/>",
        #  
        #  "Pos_HUM.sent: ", round(SIMD_merg@data$mean_Positive_HUM, 2), "<br/>",
        #  "Neg_HUM.sent: ", round(SIMD_merg@data$mean_Negative_HUM, 2), "<br/>",
        #  "Neut_HUM.sent: ", round(SIMD_merg@data$mean_Neutral_HUM, 2), "<br/>", 
        #  "Img_descr: ", (SIMD_merg@data[["first_four_text_words"]]), "<br/>", 
        #  "AI_img_descr: ", SIMD_merg@data[["first_four_images_blipdescr_words"]] , "<br/>" ,
        #  "Tags: ", (SIMD_merg@data$first_4wors_tags), "<br/>", 
        #  #"Tags: ", SIMD_merg@data$tags_words_freq[1:4] , "<br/>" ,
        #  
        #  
        #  
        #  sep="") %>%
        #  lapply(htmltools::HTML)
        ## Final Map PUD
        #m<- leaflet(SIMD_merg) %>% 
        #  addTiles()  %>% 
        #  setView( lat=55.8, lng=-4.3 , zoom=5) %>%
        #  addPolygons( 
        #    fillColor = ~mypalette(Photo_User_Day_un), 
        #    stroke=TRUE, 
        #    fillOpacity = 0.9, 
        #    color="white", 
        #    weight=0.3,
        #    label = mytext,
        #    labelOptions = labelOptions( 
        #      style = list("font-weight" = "normal", padding = "3px 8px"), 
        #      textsize = "13px", 
        #      direction = "auto"
        #    )
        #  ) %>%
        #  addLegend( pal=mypalette, values=~Photo_User_Day_un, opacity=0.9, title = "Nr.PUD", position = "bottomleft" )
      #  
     #   m  
    #    #saveWidget(m, file= paste("Flicker - query maps/_","GCC - vandal related PUD","_Data.html"))
  #      saveWidget(m, file= paste("Data hub maps/_",sanitized_path,"_Data zones map_Flicker.html"))
        
        
        
        
        
        
        #### all aggregated data hub no tags 
        # Prepare the text for tooltips:
      #  mytext <- paste(
      #    "Data Zone: ", SIMD_merg@data$DataZone,"<br/>", 
      #    "Area: ", round(as.numeric(SIMD_merg@data$StdAreaKm2), 1), "<br/>", 
      #    "SIMD Rank: ", SIMD_merg@data$SIMD2020v2_Rank, "<br/>", 
      #    
      #    "Population: ", ((SIMD_merg@data$TotPop2011)), "<br/>", 
      #    "Nr. img.: ", as.numeric(SIMD_merg@data$nr.image), "<br/>", 
      #    "Nr. PUD: ", as.numeric(SIMD_merg@data$Photo_User_Day_un), "<br/>", 
      #    "Post sent.: ", round(SIMD_merg@data$mean_Resc_HUM, 2), "<br/>",
      #    
      #    #"Pos_HUM.sent: ", round(SIMD_merg@data$mean_Positive_HUM, 2), "<br/>",
      #    #"Neg_HUM.sent: ", round(SIMD_merg@data$mean_Negative_HUM, 2), "<br/>",
      #    #"Neut_HUM.sent: ", round(SIMD_merg@data$mean_Neutral_HUM, 2), "<br/>", 
      #    
      #    "AI-capt. sent.: ", round(SIMD_merg@data$mean_Resc_AI, 2), "<br/>",
      #    
      #    #    "Pos_AI.sent: ", round(SIMD_merg@data$mean_Positive_AI, 2), "<br/>",
      #    #   "Neg_AI.sent: ", round(SIMD_merg@data$mean_Negative_AI, 2), "<br/>",
      #    #  "Neut_AI.sent: ", round(SIMD_merg@data$mean_Neutral_AI, 2), "<br/>", 
      #    
      #    #"Img_descr: ", (SIMD_merg@data[["first_four_text_words"]]), "<br/>", 
      #    "AI-capt. words: ", SIMD_merg@data[["first_four_images_blipdescr_words"]] , "<br/>" ,
      #    #"Tags: ", (SIMD_merg@data$first_4wors_tags), "<br/>", ## better not to save the tags in this case
      #    #"Tags: ", SIMD_merg@data$tags_words_freq[1:4] , "<br/>" ,
          
      #    
      #    
      #    sep="") %>%
      #    lapply(htmltools::HTML)
      #  # Final Map PUD
      #  m<- leaflet(SIMD_merg) %>% 
      #    addTiles()  %>% 
      #    setView( lat=55.8, lng=-4.3 , zoom=5) %>%
      #    addPolygons( 
      #      fillColor = ~mypalette(Photo_User_Day_un), 
      #      stroke=TRUE, 
      #      fillOpacity = 0.9, 
      #      color="white", 
      #      weight=0.3,
      #      label = mytext,
      #      labelOptions = labelOptions( 
      #        style = list("font-weight" = "normal", padding = "3px 8px"), 
      #        textsize = "13px", 
      #        direction = "auto"
      #      )
      #    ) %>%
      #    addLegend( pal=mypalette, values=~Photo_User_Day_un, opacity=0.9, title = "Nr.PUD", position = "bottomleft" )
      #  
      #  m  
      #  #saveWidget(m, file= paste("Flicker - query maps/_","GCC - vandal related PUD","_Data.html"))
      #  saveWidget(m, file= paste("shapes/_",sanitized_path,"_Data zones map_Flicker.html"))
        
        SIMD_merg_cahnged<-    st_as_sf(SIMD_merg[,c(1:10,18,26,28,33,93:97)], crs = 4326) # as soon as we put a list this change 
        library(sf)
        library(fs)     # for file system operations
        library(zip)    # for zipping files
        
        ## see the classes before saving 
        apply(SIMD_merg_cahnged,2,class)
         # Create the directory if it doesn't exist
        #dir_create(paste("shapes/_",sanitized_path,sep=""))
        st_write(SIMD_merg_cahnged, paste("shapes/_",sanitized_path,"Flicks_export_SIMD_MERGED.shp", sep=""), driver = "ESRI Shapefile", delete_dsn = TRUE)
        
        #class(SIMD_merg_cahnged)
       #try( st_write(SIMD_merg_cahnged, paste("shapes/_",sanitized_path,"/",sanitized_path,"Flicks_export_SIMD_MERGED.shp", sep=""), driver = "ESRI Shapefile", delete_dsn = TRUE) )## this worked quite well to export hte file in a format which is read by ESRI
        #try( st_write(SIMD_merg[,c(1:10,18,26,28,33,46:97)], "WP2_trialshp.shp", driver = "ESRI Shapefile", delete_dsn = TRUE) )## this worked quite well to export hte file in a format which is read by ESRI
        
        ## I did this conversion 
        SIMD_merg_cahnged$mean_Resc_AI<-as.character(round(as.numeric(SIMD_merg_cahnged$mean_Resc_AI),3))
        SIMD_merg_cahnged$mean_Resc_HUM<-as.character(round(as.numeric(SIMD_merg_cahnged$mean_Resc_HUM),3))
        str(SIMD_merg_cahnged)
        #SIMD_merg_cahnged$mean_Resc_AI <- round(as.numeric(SIMD_merg_cahnged$mean_Resc_AI) + 1e-6, 3)
        #SIMD_merg_cahnged$mean_Resc_HUM <- round(as.numeric(SIMD_merg_cahnged$mean_Resc_HUM) + 1e-6, 3)
        
        
        st_write(SIMD_merg_cahnged, paste("shapes/_",sanitized_path,"Flicks_export_SIMD_MERGED.geojson", sep=""), driver = "GeoJSON", delete_dsn = TRUE)
        
        
        ## REDOING TO CLOSE UP ON GCC REGION
        dim(aggregated_df_topics[ aggregated_df_topics$DataZone %in% GCCcodes$DataZone,])
        dim(aggregated_df_topics[ ,])
        ## for this case i am going to lose 578-48 regions, but it is fine because there was a too big area
        ### adding data i need in the spatial object 
        SIMD_merg <- merge(SIMD, aggregated_df_topics[ aggregated_df_topics$Council_area %in% CGR_names,], by.x = "DataZone", by.y = "DataZone")
        SIMD_merg@data$first_images_blipdescr_words <- vector("character", length = dim(SIMD_merg@data)[1])
        SIMD_merg@data$second_images_blipdescr_words <- vector("character", length = dim(SIMD_merg@data)[1])
        SIMD_merg@data$third_images_blipdescr_words <- vector("character", length = dim(SIMD_merg@data)[1])
        SIMD_merg@data$forth_images_blipdescr_words <- vector("character", length = dim(SIMD_merg@data)[1])
        
        #    SIMD_merg@data$first_4wors_tags<- vector("list", length = dim(SIMD_merg@data)[1])
        #SIMD_merg@data$first_theme_4wors_LDA <- vector("list", length = dim(SIMD_merg@data)[1])
        #SIMD_merg@data$first_theme_4blipimg_LDA <- vector("list", length = dim(SIMD_merg@data)[1])
        
        #SIMD_merg@data$first_four_text_words<-rep(NA,4)
        #SIMD_merg@data$first_four_images_blipdescr_words<-NA
        
        ## taking only the selected four values for dysplaying 
        for (i in 1:dim(SIMD_merg@data)[1]) {
          ###   #i=892
          print(i)
          #try(SIMD_merg@data$first_four_text_words[[i]]<-SIMD_merg@data$text_words[[i]][1:4],silent=F)
          try(SIMD_merg@data$first_images_blipdescr_words[i]<-SIMD_merg@data$images_blipdescr_words[[i]][1],silent=F) ## here the frequencies are ordered actually.
          try(SIMD_merg@data$second_images_blipdescr_words[i]<-SIMD_merg@data$images_blipdescr_words[[i]][2],silent=F) ## here the frequencies are ordered actually.
          try(SIMD_merg@data$third_images_blipdescr_words[i]<-SIMD_merg@data$images_blipdescr_words[[i]][3],silent=F) ## here the frequencies are ordered actually.
          try(SIMD_merg@data$forth_images_blipdescr_words[i]<-SIMD_merg@data$images_blipdescr_words[[i]][4],silent=F) ## here the frequencies are ordered actually.
          
          #try(SIMD_merg@data$first_4wors_tags[[i]]<-SIMD_merg@data$tags_words[[i]][1:4],silent=F)
          #try(SIMD_merg@data$first_theme_4blipimg_LDA[[i]]<-SIMD_merg@data$LDA_NOinf_words_blip[[i]][1:2],silent=F)
          ## when i have the error message is actually one of the LDA that was stopped before performing.
          ## THESE ARE NOT MANY, AS IF THE LDA WAS NOT EVEN STARTING, I WILL NOT HAVE NULL BUT NA AND THE ERROR DO NOT APPEAR.
        }
        SIMD_merg_cahnged<-    st_as_sf(SIMD_merg[,c(1:10,18,26,28,33,93:97)], crs = 4326) # as soon as we put a list this change 
        library(sf)
        library(fs)     # for file system operations
        library(zip)    # for zipping files
        
        # Create the directory if it doesn't exist
        #dir_create(paste("shapes/_",sanitized_path,sep=""))
        st_write(SIMD_merg_cahnged, paste("shapes_GCC/_",sanitized_path,"Flicks_export_SIMD_MERGED.shp", sep=""), driver = "ESRI Shapefile", delete_dsn = TRUE)
        
        #class(SIMD_merg_cahnged)
        #try( st_write(SIMD_merg_cahnged, paste("shapes/_",sanitized_path,"/",sanitized_path,"Flicks_export_SIMD_MERGED.shp", sep=""), driver = "ESRI Shapefile", delete_dsn = TRUE) )## this worked quite well to export hte file in a format which is read by ESRI
        #try( st_write(SIMD_merg[,c(1:10,18,26,28,33,46:97)], "WP2_trialshp.shp", driver = "ESRI Shapefile", delete_dsn = TRUE) )## this worked quite well to export hte file in a format which is read by ESRI
        ## I did this conversion 
        SIMD_merg_cahnged$mean_Resc_AI<-as.character(round(as.numeric(SIMD_merg_cahnged$mean_Resc_AI),3))
        SIMD_merg_cahnged$mean_Resc_HUM<-as.character(round(as.numeric(SIMD_merg_cahnged$mean_Resc_HUM),3))
        str(SIMD_merg_cahnged)
        
        st_write(SIMD_merg_cahnged,paste("shapes_GCC/_",sanitized_path,"Flicks_export_SIMD_MERGED.geojson", sep="") , driver = "GeoJSON", delete_dsn = TRUE)
        
        
        
        #file=paste("V_2 Twitter pipeline/New images results/Almost def images/",Q,"_aggregated_df_topics.Rda",sep="")
      }else{print(paste("not enough post for:", Q))}
    }

    ### how to work with this code, what it does is that it saves the geojason and shapefiles for the GCC only and not GCC only area. 
    ## the idea is that we have these two folders (i could not save in each folder the files directly because of some issues) however it is fair to say that both geojason and the shapefiles works.
    
    ## to make the upload of the shape in arcGIS onlyne though it should be a compress file which i have done by hand for now.
    
    
    
    
    
    