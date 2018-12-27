getwd()
setwd("C://Users//amrat//Dropbox//SASAnalytics2017//Data Mining Project( Bank Fraud Using R)//Fraud-Detection")
.libPaths("C://Users//amrat//R//win-library")


#Reading the text filrs
text_fraud <- read.csv("fraudfile_analysis_first_sort.csv", header = T, sep = ",",stringsAsFactors = F)
text_policy <- read.csv("policyfile_analysis_first_sort.csv", header = T, sep = ",",stringsAsFactors = F)
text_unauthorized <- read.csv("unauthorizedfile_analysis_first_sort.csv", header = T, sep = ",",stringsAsFactors = F)
text_feefile <- read.csv("feefile_analysis_first_sort.csv", header = T, sep = ",",stringsAsFactors = F)

library(ggplot2)
library(tm)
library(readr)
library(wordcloud)
library(plyr)
library(lubridate)

require(SnowballC)

wordcloud_diag<-function(data_df) {

text <- as.character(data_df$text)

sample<-sample(text,(length(text)))
corpus=Corpus(VectorSource(list(sample)))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeWords, stopwords('english'))
corpus <- tm_map(corpus, stemDocument)
dtm_up<-DocumentTermMatrix(VCorpus(VectorSource(corpus[[1]]$content)))

require(RSentiment)

freq_up <- colSums(as.matrix(dtm_up))

sentiments_up <- calculate_sentiment(names(freq_up))

sentiments_up<- cbind(sentiments_up,as.data.frame(freq_up))
head(sentiments_up)

sent_pos_up <- sentiments_up[sentiments_up$sentiment=='Positive',]
sent_neg_up <- sentiments_up[sentiments_up$sentiment=='Negative',]

cat("Negative Sentiments: ",sum(sent_neg_up$freq_up),"Positive Sentiments: ",sum(sent_pos_up$freq_up))

wordcloud(sent_neg_up$text,sent_neg_up$freq_up,min.freq = 5,random.order = FALSE,colors = brewer.pal(6,"Dark2"))

}

# To Build the wordclouds for different categories of complaints
wordcloud_diag(text_fraud)
wordcloud_diag(text_policy)
wordcloud_diag(text_feefile)
wordcloud_diag(text_unauthorized)

###To Build wordcloud for the combined data

text_combined <- read.csv("Clean_File.csv", header = T, sep = ",",stringsAsFactors = F)

text_combined$FEE<-NULL
text_combined$FRAUD<-NULL
text_combined$Policy<-NULL
text_combined$Unauthorized<-NULL
text_combined$ï..DataSet.Entry.ID<-NULL

text_combined<-as.character(text_combined$Text)



sample<-sample(text_combined,(length(text_combined)))

corpus=Corpus(VectorSource(list(sample)))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeWords, stopwords('english'))
corpus <- tm_map(corpus, stemDocument)
dtm<-DocumentTermMatrix(VCorpus(VectorSource(corpus[[1]]$content)))

removeSparse<-removeSparseTerms(dtm,0.997)#Drop the most infrequent terms

freq_up_c=colSums(as.matrix(removeSparse))

#Word Cloud
bag=as.matrix(freq_up_c)
bag=sort(rowSums(bag), decreasing = T)
bag.df = data.frame(word=names(bag), freq = bag)

wordcloud(words = bag.df$word, freq = bag.df$freq, min.freq = 200, max.words = 1500, random.order = FALSE, rot.per = 0.25,colors = brewer.pal(8,"Dark2"))

#generic function to get the word cloud for different reviews
#This method is used as of teh bag of  words approach to build a wordcloud

getWordFreq=function(textV) {
 
  sample<-sample(text_combined,(length(text_combined)))
  
  corpus=Corpus(VectorSource(list(sample)))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeWords, stopwords('english'))
  corpus <- tm_map(corpus, stemDocument)
  dtm<-DocumentTermMatrix(VCorpus(VectorSource(corpus[[1]]$content)))
  removeSparse<-removeSparseTerms(dtm,0.997)#Drop the most infrequent terms
  
  return(freq_up_c=colSums(as.matrix(removeSparse)))
  
   
}


#For Policy
word_freq_pol= getWordFreq(text_combined$Text[text_combined$Category=='policy'])

bag=as.matrix(word_freq_pol)
bag=sort(rowSums(bag), decreasing = T)
bag.df = data.frame(word=names(bag), freq = bag)

wordcloud(words = bag.df$word, freq = bag.df$freq, min.freq = 200, max.words = 1500, random.order = FALSE, rot.per = 0.25,colors = brewer.pal(8,"Dark2"),scale=c(0.5,3))

#For Unauthorized
word_freq_pol= getWordFreq(text_combined$Text[text_combined$Category=='Unauthorized'])

bag=as.matrix(word_freq_pol)
bag=sort(rowSums(bag), decreasing = T)
bag.df = data.frame(word=names(bag), freq = bag)

wordcloud(words = bag.df$word, freq = bag.df$freq, min.freq = 200, max.words = 1500, random.order = FALSE, rot.per = 0.25,colors = brewer.pal(8,"Dark2"),scale=c(0.5,3))

#For Fraud
word_freq_pol= getWordFreq(text_combined$Text[text_combined$Category=='fraud'])

bag=as.matrix(word_freq_pol)
bag=sort(rowSums(bag), decreasing = T)
bag.df = data.frame(word=names(bag), freq = bag)

wordcloud(words = bag.df$word, freq = bag.df$freq, min.freq = 200, max.words = 1500, random.order = FALSE, rot.per = 0.25,colors = brewer.pal(8,"Dark2"),scale=c(0.5,3))

#For FeeFile
word_freq_pol= getWordFreq(text_combined$Text[text_combined$Category=='fee'])

bag=as.matrix(word_freq_pol)
bag=sort(rowSums(bag), decreasing = T)
bag.df = data.frame(word=names(bag), freq = bag)

wordcloud(words = bag.df$word, freq = bag.df$freq, min.freq = 200, max.words = 1500, random.order = FALSE, rot.per = 0.25,colors = brewer.pal(8,"Dark2"),scale=c(0.5,3))



#############################################
# Contribution of Sentiments
#We specifically use NRC approach to get the sentiments here, from the combined data set. 

Sentiment_nrc<-function(data_df){

text <- as.character(data_df$text)

head(text)

corpus=Corpus(VectorSource(text))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, removeWords, stopwords('english'))
corpus <- tm_map(corpus, stemDocument)
dtm_up<-DocumentTermMatrix(VCorpus(VectorSource(corpus[[1]]$content)))


require(tidytext)
library(tidyr)
library(dplyr)


terms <- Terms(dtm_up)
head(terms)



data_tidy <- tidy(dtm_up) #COnverting in tidy format
head(data_tidy)

data_sentiments <- data_tidy %>%
  inner_join(get_sentiments("nrc"), by=c(term="word"))

head(data_sentiments)

#Words which contribute to positivity

data_sentiments%>%
  count(sentiment,term,wt=count)%>%
  ungroup() %>%
  filter(n>=1)%>%
  mutate(n=ifelse(sentiment=="negative",-n,n))%>%
  mutate(term=reorder(term,n))%>%
  ggplot(aes(term,n,fill=sentiment))+
  geom_bar(stat = "identity")+
  ylab("Contribution to sentiment")+
  coord_flip()
}

text_combined<-as.data.frame(text_combined)
Sentiment_nrc(text_combined)


#### We can notice here that most sentiments found in the complaints are of shock, negative,fear and very less are positive and
# joy, which makes us visualize that the overall polarity must be negative for the combined complaint data and hence we now calculate
# the overall polarity

##O#####3  verall Polarity
library(dplyr)
library(qdap)

##x=qdap::polarity(text_combined$Text)
##x
###save(x,file = "Overall_Polarity.Rda")
load("Overall_Polarity.Rda")
x


library(sentimentr)

############Polarity of different Categories of Complaints

#fraud_polarity<-polarity(text_fraud$text)
#save(fraud_polarity,file="fraud_polarity.Rda")
#policy_polarity<-polarity(text_policy$text)
#save(policy_polarity,file = "policy_polarity.Rda")
#fee_polarity<-polarity(text_feefile$text)
#save(fee_polarity,file = "fee_polarity.Rda")
#unauthorized<-polarity(text_unauthorized$text)
#save(unauthorized,file="unauthorized_polarity.Rda")

load("fraud_polarity.Rda")
load("policy_polarity.Rda")
load("fee_polarity.Rda")
load("unauthorized_polarity.Rda")


fraud_polarity
policy_polarity
fee_polarity
unauthorized




########################################################################################################
#######This is the exploratory research on the text data, so that we cn know whihc method or mldel should we apply
#We noticed that almpst all the 4 categories of data has lot of common words, hence it got toufgh for us to make 
#the data dictionary for all four of them and after we saw these results, we decided to go with supervised classification

#Reading the text filrs
text_fraud <- read.csv("fraudfile_analysis_first_sort.csv", header = T, sep = ",")
text_policy <- read.csv("policyfile_analysis_first_sort.csv", header = T, sep = ",")
text_unauthorized <- read.csv("unauthorizedfile_analysis_first_sort.csv", header = T, sep = ",")
text_feefile <- read.csv("feefile_analysis_first_sort.csv", header = T, sep = ",")

head(text_feefile)
head(text_policy)


class(text_fraud)
head(text$text)
#Files COntaining the complaints whihc are found true, so that we can create a data dictionary of the words specific to one 
#category of complaint
text_only_fraud <- subset(text_fraud, text_fraud$fraud==1)
head(text_only_fraud)
class(text_policy)
text_only_policy <- subset(text_policy, text_policy$policy==1)
head(text_only_policy)
text_only_unauthorized <- subset(text_unauthorized, text_unauthorized$unauthorized==1)

text_only_feefile <- subset(text_feefile, text_feefile$fee==1)
########################


#COnverting into one complan per line.

library(dplyr)

head(text_only_policy)
dim(text_only_fraud)
text_only_fraud_df <- data_frame(line = 1:843, text = text_only_fraud$text)

head(text_only_feefile)
head(text)
dim(text_only_feefile)
text_only_feefile_df <- data_frame(line = 1:1164, text = text_only_feefile$text)

dim(text_only_policy)
text_only_policy_df <- data_frame(line = 1:1027, text = text_only_policy$text)


dim(text_only_unauthorized)
text_only_unauthorized_df <- data_frame(line = 1:739, text = text_only_unauthorized$text)

##############################################
library(tidytext)

str(text_only_fraud_df)

#COnverting the factor into vector
text_only_fraud_df$text <- as.vector(text_only_fraud_df$text)
text_only_feefile_df$text<- as.vector(text_only_feefile_df$text)
text_only_policy_df$text <- as.vector(text_only_policy_df$text)
text_only_unauthorized_df$text<- as.vector(text_only_unauthorized_df$text)

###################################3

#COnverting into tokens
text_only_fraud_tokens<- text_only_fraud_df%>%
  unnest_tokens(output=word,input=text)

text_only_feefile_tokens<- text_only_feefile_df%>%
  unnest_tokens(output=word,input=text)

text_only_policy_tokens<- text_only_policy_df%>%
  unnest_tokens(output=word,input=text)

text_only_unauthorized_tokens<- text_only_unauthorized_df%>%
  unnest_tokens(output=word,input=text)

###################################

#Removing the Stop Words

data("stop_words")

head(text_only_fraud_tokens)

text_only_fraud_tokens<- text_only_fraud_tokens %>%
  anti_join(stop_words)

text_only_feefile_tokens<- text_only_feefile_tokens %>%
  anti_join(stop_words)

text_only_policy_tokens<- text_only_policy_tokens %>%
  anti_join(stop_words)

text_only_unauthorized_tokens<- text_only_unauthorized_tokens %>%
  anti_join(stop_words)

combinned_df <- data.frame(text_only_feefile_tokens,text_only_fraud_tokens,text_only_policy_tokens,text_only_unauthorized_tokens)

# creating the dataframe for the most common words
text_only_fraud_tokens <- count(text_only_fraud_tokens,word, sort = TRUE)
text_only_fraud_tokens



text_only_feefile_tokens <- count(text_only_feefile_tokens,  word, sort = TRUE) 
text_only_feefile_tokens

text_only_unauthorized_tokens <- count(text_only_unauthorized_tokens,word, sort = TRUE) 
text_only_unauthorized_tokens


text_only_policy_tokens <- count(text_only_policy_tokens,word, sort = TRUE) 
text_only_policy_tokens


##################
#Converting the counts into the dataframe and combining the complaints and there top counts

top_counts_fraud <- (text_only_fraud_tokens[1:500,])
top_counts_fraud


top_counts_policy <- (text_only_policy_tokens[1:500,])
top_counts_policy

top_counts_feefile <- (text_only_feefile_tokens[1:500,])
top_counts_feefile

top_counts_unauthorized <- (text_only_unauthorized_tokens[1:500,])
top_counts_unauthorized



#Detecting the common words in all the 4 vetors

a<- intersect(top_counts_fraud$word,top_counts_policy$word)

str(a)
b<- intersect(a,top_counts_feefile$word)

c<- intersect(b,top_counts_unauthorized$word)

length(c)

c <- as.data.frame(c)
c

#################
## Here we notice that out of 500 words, 340 words are common in all the four categories which makes no sense, if we remove them all to c
#create an unique data dictionary for all the four categories, it is not that good approacch in this case
#####################

combined_complaints <- data.frame(top_counts_fraud,top_counts_policy,top_counts_feefile,top_counts_unauthorized)
names(combined_complaints[1]) <-paste0("Fraud Words")
names(combined_complaints[2]) <-paste0("Frequency")
names(combined_complaints[3]) <- paste0("Policy Words")
names(combined_complaints[4]) <-paste0("Frequency")
names(combined_complaints[5]) <-paste0("feefile words")
names(combined_complaints[6]) <-paste0("Frequency")
names(combined_complaints[7]) <-paste0("Unauthorized words")
names(combined_complaints[8]) <-paste0("Frequency")

colnames(combined_complaints) <- c("Fraud Words","Frequency","Policy Words","Frequency","Feefile Words","Frequency","Unauthorized Words","Frequency")

#These are the combined category of complaintswith there frequency counts

head(combined_complaints)
###############################################################################################

####################
#Applying the classification tree model to do the supervised learning. We also try mutinominal logisitc regression here
#but due to such large number of predictors which are actually our words here, it couldnt give successful results
  
  #Reading the text files
  text_fraud <- read.csv("fraudfile_analysis_first_sort.csv", header = T, sep = ",",stringsAsFactors = F)
  text_policy <- read.csv("policyfile_analysis_first_sort.csv", header = T, sep = ",",stringsAsFactors = F)
  text_unauthorized <- read.csv("unauthorizedfile_analysis_first_sort.csv", header = T, sep = ",",stringsAsFactors = F)
  text_feefile <- read.csv("feefile_analysis_first_sort.csv", header = T, sep = ",",stringsAsFactors = F)
  
  getwd()
  

  ###Fitting Model Accuracy for the 4 Classification tree models we build for the 4 categories of complaints
  

  Accuracy_of_Models<-function(text_data,type){
    library(rpart)
    library(rpart.plot)
    library(caTools)
    
    set.seed(123)

  text_data$issue_type <- ifelse(text_data[,4] == 1 , type, "Not")
  text_data[,1]<-NULL
  text_data$tagger<-NULL
  
  names(text_data) <- c("text","label")
  levels(as.factor(text_data$label))
  

  ##### clean the text
  
  corpus <- Corpus(VectorSource(text_data$text))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeWords, stopwords('english'))
  corpus <- tm_map(corpus, stemDocument)
  

  ## obtain document term matrix
  myDtm <- DocumentTermMatrix(corpus)
  
  sparse = removeSparseTerms(myDtm, 0.99)

  # Convert to a data frame
  
  text_data_Sparse = as.data.frame(as.matrix(sparse))
  text_data_Sparse$label<-text_data$label
  
  documents <- data.frame(doc_id=row.names(text_data),text=text_data$text)
  
  n = nrow(text_data_Sparse)
  idx <- sample(n, n * .75)
  train = text_data_Sparse[idx,]
  test = text_data_Sparse[-idx,]
  
  text_CART = rpart(label ~ ., data=train, method="class")
  
  predictCART = predict(text_CART, newdata=test, type="class")
  
  
c<-table(test$label, predictCART)

d<-((c[1]+c[4])/(c[2]+c[3]+c[1]+c[4]))*100

e<-paste0("The accuracy of the model is: " )
f<-paste0(" Percentage!       ")

return(cat(e,d,f))

}
  
Accuracy_of_Models(text_fraud,"fraud")
Accuracy_of_Models(text_policy,"Policy")
Accuracy_of_Models(text_feefile,"fee")
Accuracy_of_Models(text_unauthorized,"unauthorized")

##############################################################
########Fitting the original data in the models
# Now we pull the original data and apply oyr classification tree model on the data

## Install the required package with:

## install.packages("RSocrata"), Pulling the total data 


library("RSocrata")


library(caTools)
library(e1071)
library(rpart)
library(rpart.plot)
library(wordcloud)
library(tm)
library(SnowballC)
library(ROCR)
library(RColorBrewer)
library(stringr)


#Complaints_data <- read.socrata(
#  "https://data.consumerfinance.gov/resource/jhzv-w97w.json?$where=date_received>='2015-03-01T00:00:00.000' AND complaint_what_happened IS NOT NULL",
#  app_token = "xUWI2P9RzzcZuUqGa9VURZrMU",
#  email     = "avatse@purdue.edu",
# password  = "Zsa@2019"
#)

#save(Complaints_data,file = "Complaint_data.Rda")


load("Complaint_data.Rda")




final_data<-Complaints_data[,3:4]
head(final_data)
names(final_data) <- c("Id","text")

########### Pre Processing of the final data, which are the original data
################################
# As the data is very large, so it takes time to run the below functions and to preprocess it to the format of training
#data set and hence we save the final results of preprocessing. If you want to run, feel free, Just it will take sometime.

##### clean the text

#corpus <- Corpus(VectorSource(final_data$text))
#corpus <- tm_map(corpus, removePunctuation)
#corpus <- tm_map(corpus, content_transformer(tolower))
#corpus <- tm_map(corpus, removeNumbers)
#corpus <- tm_map(corpus, stripWhitespace)
#corpus <- tm_map(corpus, removeWords, stopwords('english'))
#corpus <- tm_map(corpus, stemDocument)


## obtain document term matrix
#myDtm <- DocumentTermMatrix(corpus)
#findFreqTerms(myDtm, lowfreq=400)

#sparse = removeSparseTerms(myDtm, 0.99)
#sparse


# Convert to a data frame

#text_final_sparse = as.data.frame(as.matrix(sparse))
#save(sparse,"sparse.Rda")
#save(text_final_sparse,file = "text_final_sparse.Rda")

load("text_final_sparse.Rda")    

#documents <- data.frame(doc_id=row.names(final_data),text=final_data$text)








############ Creating a function, so that we can prepprocess the data for all the three models and we can then have the predictions
  # for the new original data set


  
  Predicting_Models<-function(text_data,type){
    library(rpart)
    library(rpart.plot)
    library(caTools)
    
    set.seed(123)
    
    text_data$issue_type <- ifelse(text_data[,4] == 1 , type, "Not")
    text_data[,1]<-NULL
    text_data$tagger<-NULL
    
    names(text_data) <- c("text","label")
    levels(as.factor(text_data$label))
    
    
    ##### clean the text
    
    corpus <- Corpus(VectorSource(text_data$text))
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, removeNumbers)
    corpus <- tm_map(corpus, stripWhitespace)
    corpus <- tm_map(corpus, removeWords, stopwords('english'))
    corpus <- tm_map(corpus, stemDocument)
    
    
    ## obtain document term matrix
    myDtm <- DocumentTermMatrix(corpus)
    
    sparse = removeSparseTerms(myDtm, 0.99)
    
    # Convert to a data frame
    
    text_data_Sparse = as.data.frame(as.matrix(sparse))
    text_data_Sparse$label<-text_data$label
    
    documents <- data.frame(doc_id=row.names(text_data),text=text_data$text)
    
    n = nrow(text_data_Sparse)
    idx <- sample(n, n * .75)
    train = text_data_Sparse[idx,]
    test = text_data_Sparse[-idx,]
    tr_ts<- list(c(train,test))
    
    return(train)
  
    }
  
  
  ##################
  ## HEre we fit the model to extract the fraud classification
  library(rpart)
  library(rpart.plot)
  
  x_1<-Predicting_Models(text_fraud,"fraud")
  common_cols <- intersect(colnames(x_1), colnames(text_final_sparse))
  length(common_cols)
  train_final<-subset(x_1,select = c(common_cols))
  train_final$label<-x_1$label
  
  final_text_sparse_train<-subset(text_final_sparse,select = (common_cols))
  
  dim(train_final)
  dim(final_text_sparse_train)
  
  fraud_CART = rpart(label ~ ., data=train_final, method="class")
  predictCART = predict(fraud_CART, newdata=final_text_sparse_train, type="class")
  
  head(predictCART)
  class(predictCART)
  predictCART<-as.data.frame(predictCART)
  c<-Complaints_data$complaint_id
  final_fraud<-data.frame(c,predictCART)
  head(final_fraud)
  names(final_fraud) <- c("Complaint Id","Fraud Categorization")
  head(final_fraud)
  
  save(final_fraud,file = "final_fraud.Rda")
  


########### ####
## ## HEre we fit the model to extract the Policy classification
  

library(rpart)
library(rpart.plot)

x_2<-Predicting_Models(text_policy,"Policy")
common_cols <- intersect(colnames(x_2), colnames(text_final_sparse))
length(common_cols)
train_final<-subset(x_2,select = c(common_cols))
train_final$label<-x_2$label

final_text_sparse_train<-subset(text_final_sparse,select = (common_cols))

dim(train_final)
dim(final_text_sparse_train)

policy_CART = rpart(label ~ ., data=train_final, method="class")
predictCART = predict(policy_CART, newdata=final_text_sparse_train, type="class")

head(predictCART)
class(predictCART)
predictCART<-as.data.frame(predictCART)
c<-Complaints_data$complaint_id
final_policy<-data.frame(c,predictCART)
head(final_policy)
names(final_policy) <- c("Complaint Id","Policy Categorization")
head(final_policy)

save(final_policy,file = "final_policy.Rda")

################################ 
######### HEre we fit the model to extract the Feefile classification



library(rpart)
library(rpart.plot)

x_3<-Predicting_Models(text_feefile,"fee")
common_cols <- intersect(colnames(x_3), colnames(text_final_sparse))
length(common_cols)
train_final<-subset(x_3,select = c(common_cols))
train_final$label<-x_3$label

final_text_sparse_train<-subset(text_final_sparse,select = (common_cols))

dim(train_final)
dim(final_text_sparse_train)

fee_CART = rpart(label ~ ., data=train_final, method="class")
predictCART = predict(fee_CART, newdata=final_text_sparse_train, type="class")

head(predictCART)
class(predictCART)
predictCART<-as.data.frame(predictCART)
c<-Complaints_data$complaint_id
final_fee<-data.frame(c,predictCART)
head(final_fee)
names(final_fee) <- c("Complaint Id","Fee Categorization")
head(final_fee)

save(final_fee,file = "final_fee.Rda")


################################
######HEre we fit the model to extract the Policy classification



library(rpart)
library(rpart.plot)

x_4<-Predicting_Models(text_unauthorized,"unauthorized")
common_cols <- intersect(colnames(x_4), colnames(text_final_sparse))
length(common_cols)
train_final<-subset(x_4,select = c(common_cols))
train_final$label<-x_4$label

final_text_sparse_train<-subset(text_final_sparse,select = (common_cols))

dim(train_final)
dim(final_text_sparse_train)

unauthorized_CART = rpart(label ~ ., data=train_final, method="class")
predictCART = predict(unauthorized_CART, newdata=final_text_sparse_train, type="class")


head(predictCART)
class(predictCART)
predictCART<-as.data.frame(predictCART)
c<-Complaints_data$complaint_id

final_unauthorized<-data.frame(c,predictCART)
head(final_unauthorized)
names(final_unauthorized) <- c("Complaint Id","Unauthorized Categorization")
head(final_unauthorized)

save(final_unauthorized,file = "final_unauthorized.Rda")




######################################
load("final_fraud.Rda")
load("final_policy.Rda")
load("final_fee.Rda")
load("final_unauthorized.Rda")

final_result<- data.frame(final_fraud,final_fee$`Fee Categorization`,final_policy$`Policy Categorization`,final_unauthorized$`Unauthorized Categorization`)
names(final_result)<-(c("Complaint_Id","Fraud","Fee","Policy","Unauthorized"))

(final_result[17:60,])
save(final_result,file="Submission_final_predictions")
write.csv(final_result,"Submission_final_predictions")


table(final_result$Fraud.Categorization)
table(final_result$final_fee..Fee.Categorization.)
table(final_result$final_policy..Policy.Categorization.)
table(final_result$final_unauthorized..Unauthorized.Categorization.)

final_result_1<-subset(final_result,final_result$Fraud.Categorization=="1"|final_result$final_fee..Fee.Categorization.=="1"|final_result$final_policy..Policy.Categorization.=="1"|final_result$final_unauthorized..Unauthorized.Categorization.=="1")

final_result_2<-subset(final_result,final_result$Fraud.Categorization=="0"&&final_result$final_fee..Fee.Categorization.=="0" && final_result$final_policy..Policy.Categorization.=="0"&&final_result$final_unauthorized..Unauthorized.Categorization.=="0")


head(final_result_1)
dim(final_result_1)

head(final_result_2)

final_result_combined<- rbind(final_result_1,final_result_2[1:30000,])

dim(final_result_combined)

write.csv(final_result_combined,"final_result_combined_done.csv")


