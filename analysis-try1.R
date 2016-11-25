getwd()
setwd("E:/RPI/DS 2016/Assignments/Project/")
list.files()

library(tm)
library(wordcloud)
library(rpart)
library(e1071)
library(rattle)
library(rpart.plot)
library(ggplot2)
library(ROCR)
library(caret)

df.bos <-
  read.csv(
    "Food_Establishment_Inspections_(converted).csv",
    header = TRUE,
    sep = ","
  )

df.chi <-
  read.csv("Food_Inspections_(converted).csv",
           header = TRUE,
           sep = ",")

######## Refactoring data ########
# Removing column X
df.bos <- subset(df.bos, select = -c(X))
df.chi <- subset(df.chi, select = -c(X))

# Reordering position of date column and changing data type.
refcols <- c("date")
df.bos <- df.bos[, c(refcols, setdiff(names(df.bos), refcols))]
df.chi <- df.chi[, c(refcols, setdiff(names(df.chi), refcols))]
df.bos$date <- as.Date(df.bos$date, format = "%m/%d/%Y")
df.chi$date <- as.Date(df.chi$date, format = "%m/%d/%Y")

summary(df.bos)
summary(df.chi)

# create dataframe from comments & result of boston data
comments.bos <- df.bos[, c("comment", "result")]
comments.bos$comment <- as.character(comments.bos$comment)
comments.bos <- comments.bos[(comments.bos$result==""), ]
sum(comments.bos$comment=="") # 19718 empty comments. dropping these in next line
comments.bos <- comments.bos[!(comments.bos$comment == ""), ]

# create dataframe from comments & result of chicago data
comments.chi <- df.chi[, c("comment", "result")]
comments.chi$comment <- as.character(comments.chi$comment)
sum(comments.chi$comment=="") # 5929 empty comments. dropping these in next line
comments.chi <- comments.chi[!(comments.chi$comment == ""), ]
# Fail                  No Entry      Not Ready    Out of Business      Pass 
# 146560 (29.512%)    974 (0.196%)   89 (0.0179%)   111 (0.02235%)   282291 (56.84%) 
# Pass w/ Conditions 
# 66586 (13.408%)

# Dropping factors other than Pass & Fail.
comments.chi <- comments.chi[!(comments.chi$result=="No Entry"), ] # 0.1083% of records in boston & chicago data put together
comments.chi <- comments.chi[!(comments.chi$result=="Not Ready"), ] # 0.00966% of records boston & chicago data put together
comments.chi <- comments.chi[!(comments.chi$result=="Out of Business"), ] # 0.0134% of records boston & chicago data put together
comments.chi <- comments.chi[!(comments.chi$result=="Pass w/ Conditions"), ] # 7.2645% of records boston & chicago data put together
comments.chi$result <- factor(comments.chi$result)

# Indexes for taking a randomly sampled subset from boston and chicago
# Taking a sample size of 50,000 from boston & chicago to get 100,000 records in the combined data
random.sample.subset.bos = sample(nrow(comments.bos), size = 50000, replace = FALSE)
random.sample.subset.chi = sample(nrow(comments.chi), size = 50000, replace = FALSE)
# Taking a sample size of 100,000 from boston & chicago to perform the analysis on them independently
random.sample.subset.bos.1 = sample(nrow(comments.bos), size = 100000, replace = FALSE)
random.sample.subset.chi.1 = sample(nrow(comments.chi), size = 100000, replace = FALSE)

summary(comments.bos[random.sample.subset.bos, ] )
summary(comments.chi[random.sample.subset.chi, ] )

# create dataframe from comments & result of all the data
comments <- rbind(comments.bos[random.sample.subset.bos, ], comments.chi[random.sample.subset.chi, ])

summary(comments)
# comment           result     
# Length:100000      Fail:45119  
# Class :character   Pass:54881  
# Mode  :character 

############ Corpus Creation #############
# Create corpus from comment column of comments dataframe. 
myCorpus <- Corpus(VectorSource(comments[, c("comment")]))
myCorpus <- tm_map(myCorpus, tolower)
myCorpus <- tm_map(myCorpus, removePunctuation)
myCorpus <- tm_map(myCorpus, stripWhitespace)
myCorpus <- tm_map(myCorpus, PlainTextDocument)
myCorpus <- tm_map(myCorpus, removeNumbers)
myCorpus <- tm_map(myCorpus, removeWords, stopwords('english'))
myCorpus.stemmed <- tm_map(myCorpus, stemDocument) #stemming the corpus and storing it separately.

# Corpus from only chicago comments.
myCorpus.chi <- Corpus(VectorSource(comments.chi[random.sample.subset.chi.1, c("comment")]))
myCorpus.chi <- tm_map(myCorpus.chi, tolower)
myCorpus.chi <- tm_map(myCorpus.chi, removePunctuation)
myCorpus.chi <- tm_map(myCorpus.chi, stripWhitespace)
myCorpus.chi <- tm_map(myCorpus.chi, PlainTextDocument)
myCorpus.chi <- tm_map(myCorpus.chi, removeNumbers)
myCorpus.chi <- tm_map(myCorpus.chi, removeWords, stopwords('english'))
myCorpus.chi.stemmed <- tm_map(myCorpus.chi, stemDocument) #stemming the corpus and storing it separately.

# Corpus from only the boston comments.
myCorpus.bos <- Corpus(VectorSource(comments.bos[random.sample.subset.bos.1, c("comment")]))
myCorpus.bos <- tm_map(myCorpus.bos, tolower)
myCorpus.bos <- tm_map(myCorpus.bos, removePunctuation)
myCorpus.bos <- tm_map(myCorpus.bos, stripWhitespace)
myCorpus.bos <- tm_map(myCorpus.bos, PlainTextDocument)
myCorpus.bos <- tm_map(myCorpus.bos, removeNumbers)
myCorpus.bos <- tm_map(myCorpus.bos, removeWords, stopwords('english'))
myCorpus.bos.stemmed <- tm_map(myCorpus.bos, stemDocument) #stemming the corpus and storing it separately.


########## Creating the Document Term Matrix & removing sparse Terms ###########
# Create document term matrix with weighting as the term factor from a stemmed and an unstemmed corpus. 
# Combined Data
dtm.tf <- DocumentTermMatrix(myCorpus, control = list(weighting = function(x) weightTf(x))) # Unstemmed
stemmed.dtm.tf <- DocumentTermMatrix(myCorpus.stemmed, control = list(weighting = function(x) weightTf(x))) # Stemmed
# Boston Data
dtm.tf.bos <- DocumentTermMatrix(myCorpus.bos, control = list(weighting = function(x) weightTf(x))) # Unstemmed
stemmed.dtm.tf.bos <- DocumentTermMatrix(myCorpus.bos.stemmed, control = list(weighting = function(x) weightTf(x))) # Stemmed
# Chicago Data
dtm.tf.chi <- DocumentTermMatrix(myCorpus.chi, control = list(weighting = function(x) weightTf(x))) # Unstemmed
stemmed.dtm.tf.chi <- DocumentTermMatrix(myCorpus.chi.stemmed, control = list(weighting = function(x) weightTf(x))) # Stemmed


# removeSparseTerms is to reduce the number of sparse terms in the Matrix. Done due to the highly sparse nature of the matrix.
# Combined unstemmed
sparseDTM.tf <- removeSparseTerms(dtm.tf, 0.96) # 0.96 means keep terms (words) occuring in 96% of comments
findFreqTerms(dtm.tf, lowfreq = 4500, highfreq = Inf) # Finding terms that occur atleast in 4600 comments.
Terms(sparseDTM.tf) # get terms in the sparseDTM.tf
intersect( findFreqTerms(dtm.tf, lowfreq = 4600, highfreq = Inf), 
           Terms(sparseDTM.tf)) # intersection of the results from findFreqTerms & Terms in sparseDTM.tf
setdiff( findFreqTerms(dtm.tf, lowfreq = 4600, highfreq = Inf), 
         Terms(sparseDTM.tf)) # set difference of the results from findFreqTerms & Terms in sparseDTM.tf

# Boston unstemmed
sparseDTM.tf.bos <- removeSparseTerms(dtm.tf.bos, 0.965) # 0.96 means keep terms (words) occuring in 96% of comments
findFreqTerms(dtm.tf.bos, lowfreq = 4600, highfreq = Inf) # Finding terms that occur atleast in 4600 comments.
Terms(sparseDTM.tf.bos) # get terms in the sparseDTM.tf
intersect( findFreqTerms(dtm.tf.bos, lowfreq = 4600, highfreq = Inf), 
           Terms(sparseDTM.tf.bos)) # intersection of the results from findFreqTerms & Terms in sparseDTM.tf
setdiff( findFreqTerms(dtm.tf.bos, lowfreq = 4600, highfreq = Inf), 
         Terms(sparseDTM.tf.bos)) # set difference of the results from findFreqTerms & Terms in sparseDTM.tf

# Chicago unstemmed
sparseDTM.tf.chi <- removeSparseTerms(dtm.tf.chi, 0.95) # 0.96 means keep terms (words) occuring in 96% of comments
findFreqTerms(dtm.tf.chi, lowfreq = 4600, highfreq = Inf) # Finding terms that occur atleast in 4600 comments.
Terms(sparseDTM.tf.chi) # get terms in the sparseDTM.tf
intersect( findFreqTerms(dtm.tf.chi, lowfreq = 4600, highfreq = Inf), 
           Terms(sparseDTM.tf.chi)) # intersection of the results from findFreqTerms & Terms in sparseDTM.tf
setdiff( findFreqTerms(dtm.tf.chi, lowfreq = 4600, highfreq = Inf), 
         Terms(sparseDTM.tf.chi)) # set difference of the results from findFreqTerms & Terms in sparseDTM.tf

# Combined stemmed
stemmed.sparseDTM.tf <- removeSparseTerms(stemmed.dtm.tf, 0.96) # 0.96 means keep terms (words) occuring in 96% of comments
findFreqTerms(stemmed.dtm.tf, lowfreq = 4600, highfreq = Inf) # Finding terms that occur atleast in 4600 comments.
Terms(stemmed.sparseDTM.tf) # get terms in stemmed.sparseDTM.tf
intersect( findFreqTerms(stemmed.dtm.tf, lowfreq = 4600, highfreq = Inf), 
          Terms(stemmed.sparseDTM.tf)) # intersection of the results from findFreqTerms & Terms in stemmed.sparseDTM.tf
setdiff( findFreqTerms(stemmed.dtm.tf, lowfreq = 4600, highfreq = Inf), 
          Terms(stemmed.sparseDTM.tf)) # set difference of the results from findFreqTerms & Terms in stemmed.sparseDTM.tf

# Boston stemmed
stemmed.sparseDTM.tf.bos <- removeSparseTerms(stemmed.dtm.tf.bos, 0.965) # 0.96 means keep terms (words) occuring in 96% of comments
findFreqTerms(stemmed.dtm.tf.bos, lowfreq = 4600, highfreq = Inf) # Finding terms that occur atleast in 4600 comments.
Terms(stemmed.sparseDTM.tf.bos) # get terms in stemmed.sparseDTM.tf
intersect( findFreqTerms(stemmed.dtm.tf.bos, lowfreq = 4600, highfreq = Inf), 
           Terms(stemmed.sparseDTM.tf.bos)) # intersection of the results from findFreqTerms & Terms in stemmed.sparseDTM.tf
setdiff( findFreqTerms(stemmed.dtm.tf.bos, lowfreq = 4600, highfreq = Inf), 
         Terms(stemmed.sparseDTM.tf.bos)) # set difference of the results from findFreqTerms & Terms in stemmed.sparseDTM.tf

# Chicago stemmed
stemmed.sparseDTM.tf.chi <- removeSparseTerms(stemmed.dtm.tf.chi, 0.95) # 0.96 means keep terms (words) occuring in 96% of comments
findFreqTerms(stemmed.dtm.tf.chi, lowfreq = 4600, highfreq = Inf) # Finding terms that occur atleast in 4600 comments.
Terms(stemmed.sparseDTM.tf.chi) # get terms in stemmed.sparseDTM.tf
intersect( findFreqTerms(stemmed.dtm.tf.chi, lowfreq = 4600, highfreq = Inf), 
           Terms(stemmed.sparseDTM.tf.chi)) # intersection of the results from findFreqTerms & Terms in stemmed.sparseDTM.tf
setdiff( findFreqTerms(stemmed.dtm.tf.chi, lowfreq = 4600, highfreq = Inf), 
         Terms(stemmed.sparseDTM.tf.chi)) # set difference of the results from findFreqTerms & Terms in stemmed.sparseDTM.tf

######## Data Frame Creation ########
# Create dataframe from the document term matrices created in the previous section. 
# Dataframes are created for both stemmed and unstemmed. The stemmed dataframe is prefixed with the word 'stemmed'
# Note: This might take a lot of time.
# Combined Data
sparseDTM.tf.df <- as.data.frame(as.matrix(sparseDTM.tf))
stemmed.sparseDTM.tf.df <- as.data.frame(as.matrix(stemmed.sparseDTM.tf))
summary(sparseDTM.tf.df)
summary(stemmed.sparseDTM.tf.df)
# Boston Data
sparseDTM.tf.df.bos <- as.data.frame(as.matrix(sparseDTM.tf.bos))
stemmed.sparseDTM.tf.df.bos <- as.data.frame(as.matrix(stemmed.sparseDTM.tf.bos))
summary(sparseDTM.tf.df.bos)
summary(stemmed.sparseDTM.tf.df.bos)
# Chicago Data
sparseDTM.tf.df.chi <- as.data.frame(as.matrix(sparseDTM.tf.chi))
stemmed.sparseDTM.tf.df.chi <- as.data.frame(as.matrix(stemmed.sparseDTM.tf.chi))
summary(sparseDTM.tf.df.chi)
summary(stemmed.sparseDTM.tf.df.chi)

# Join the document term matrix's dataframe with the results column from comments.
# This is done to make it easier to run the data through a model.
# Combined unstemmed
sparseDTM.tf.df = cbind(sparseDTM.tf.df, comments[ , c("result")], row.names = NULL)
colnames(sparseDTM.tf.df)[ncol(sparseDTM.tf.df)] <- "result_value" # change the column name
# Boston unstemmed
sparseDTM.tf.df.bos = cbind(sparseDTM.tf.df.bos, comments.bos[random.sample.subset.bos.1, c("result")], row.names = NULL)
colnames(sparseDTM.tf.df.bos)[ncol(sparseDTM.tf.df.bos)] <- "result_value" # change the column name
# Chicago unstemmed
sparseDTM.tf.df.chi = cbind(sparseDTM.tf.df.chi, comments.chi[random.sample.subset.chi.1, c("result")], row.names = NULL)
colnames(sparseDTM.tf.df.chi)[ncol(sparseDTM.tf.df.chi)] <- "result_value" # change the column name
# Combined stemmed
stemmed.sparseDTM.tf.df = cbind(stemmed.sparseDTM.tf.df, comments[ , c("result")], row.names = NULL)
colnames(stemmed.sparseDTM.tf.df)[ncol(stemmed.sparseDTM.tf.df)] <- "result_value" # change the column name
# Boston stemmed
stemmed.sparseDTM.tf.df.bos = cbind(stemmed.sparseDTM.tf.df.bos, comments.bos[random.sample.subset.bos.1, c("result")], row.names = NULL)
colnames(stemmed.sparseDTM.tf.df.bos)[ncol(stemmed.sparseDTM.tf.df.bos)] <- "result_value" # change the column name
# Chicago stemmed
stemmed.sparseDTM.tf.df.chi = cbind(stemmed.sparseDTM.tf.df.chi, comments.chi[random.sample.subset.chi.1, c("result")], row.names = NULL)
colnames(stemmed.sparseDTM.tf.df.chi)[ncol(stemmed.sparseDTM.tf.df.chi)] <- "result_value" # change the column name
# Just a bit of noodling around.
colSums(sparseDTM.tf.df[sparseDTM.tf.df$result_value == "Pass", 1:45] )
colSums(sparseDTM.tf.df[sparseDTM.tf.df$result_value == "Fail", 1:45] )
# csp <- colSums(sparseDTM.tf.df[sparseDTM.tf.df$result_value == "Pass", 1:45] )
# csf <- colSums(sparseDTM.tf.df[sparseDTM.tf.df$result_value == "Fail", 1:45] )
# csp.df = data.frame(x = Terms(sparseDTM.tf), y = csp)
# csf.df = data.frame(x = Terms(sparseDTM.tf), y = csf)
# ggplot(sparseDTM.tf.df, aes(x = colnames(sparseDTM.tf.df[ , 1:45 ]), fill = result_value)) + geom_bar(position = "dodge")
# colnames(sparseDTM.tfidf.df)[78] <- "result_value" # change the column name
# summary(sparseDTM.tfidf.df)

# Creating indexes for splitting the data. The split is 70% train and 30% test.
idx = sort(sample(nrow(sparseDTM.tf.df), nrow(sparseDTM.tf.df)*.7))
# Combined unstemmed
train.sparseDTM.tf.df <- sparseDTM.tf.df[idx, ] # Creating training set
test.sparseDTM.tf.df <- sparseDTM.tf.df[-idx, ] # Creating test set
# Boston unstemmed
train.sparseDTM.tf.df.bos <- sparseDTM.tf.df.bos[idx, ] # Creating training set
test.sparseDTM.tf.df.bos <- sparseDTM.tf.df.bos[-idx, ] # Creating test set
# Chicago unstemmed
train.sparseDTM.tf.df.chi <- sparseDTM.tf.df.chi[idx, ] # Creating training set
test.sparseDTM.tf.df.chi <- sparseDTM.tf.df.chi[-idx, ] # Creating test set
# Combined stemmed
train.stemmed.sparseDTM.tf.df <- stemmed.sparseDTM.tf.df[idx, ] # Creating training set
test.stemmed.sparseDTM.tf.df <- stemmed.sparseDTM.tf.df[-idx, ] # Creating test set
# Boston stemmed
train.stemmed.sparseDTM.tf.df.bos <- stemmed.sparseDTM.tf.df.bos[idx, ] # Creating training set
test.stemmed.sparseDTM.tf.df.bos <- stemmed.sparseDTM.tf.df.bos[-idx, ] # Creating test set
# Chicago stemmed
train.stemmed.sparseDTM.tf.df.chi <- stemmed.sparseDTM.tf.df.chi[idx, ] # Creating training set
test.stemmed.sparseDTM.tf.df.chi <- stemmed.sparseDTM.tf.df.chi[-idx, ] # Creating test set

######## Model Creation ########
# For the combined data
# Fitting training data to a decision tree model
# Had to choose an extremely low cp value otherwise I was getting only 1 node in the tree.
# This might be due to the sparse nature of the matrix.
# Unstemmed data
model.rpart <- rpart(result_value ~ ., data = train.sparseDTM.tf.df, method = "class", 
                     control=rpart.control(minsplit=1, minbucket=1, cp=0.000001) )
# Combined Stemmed Data
model.rpart.stemmed <- rpart(result_value ~ ., data = train.stemmed.sparseDTM.tf.df, method = "class", 
                             control=rpart.control(minsplit=1, minbucket=1, cp=0.000001) )
# Fitting training data to a Naive Bayesian classifier model
# Combined Unstemmed Data
model.nb <- naiveBayes(result_value ~ ., data = train.sparseDTM.tf.df)
# Combined Stemmed Data
model.nb.stemmed <- naiveBayes(result_value ~ ., data = train.stemmed.sparseDTM.tf.df)

# Boston Data
# Fitting training data to a decision tree model
# unstemmed
model.rpart.bos <- rpart(result_value ~ ., data = train.sparseDTM.tf.df.bos, method = "class", 
                     control=rpart.control(minsplit=1, minbucket=1, cp=0.000001) )
# stemmed
model.rpart.stemmed.bos <- rpart(result_value ~ ., data = train.stemmed.sparseDTM.tf.df.bos, method = "class", 
                             control=rpart.control(minsplit=1, minbucket=1, cp=0.000001) )
# Fitting training data to a Naive Bayesian classifier model
# unstemmed
model.nb.bos <- naiveBayes(result_value ~ ., data = train.sparseDTM.tf.df.bos)
# stemmed
model.nb.stemmed.bos <- naiveBayes(result_value ~ ., data = train.stemmed.sparseDTM.tf.df.bos)

# Chicago
# Fitting training data to a decision tree model
# unstemmed
model.rpart.chi <- rpart(result_value ~ ., data = train.sparseDTM.tf.df.chi, method = "class", 
                     control=rpart.control(minsplit=1, minbucket=1, cp=0.000001) )
# stemmed
model.rpart.stemmed.chi <- rpart(result_value ~ ., data = train.stemmed.sparseDTM.tf.df.chi, method = "class", 
                             control=rpart.control(minsplit=1, minbucket=1, cp=0.000001) )
# Fitting training data to a Naive Bayesian classifier model
# unstemmed
model.nb.chi <- naiveBayes(result_value ~ ., data = train.sparseDTM.tf.df.chi)
# stemmed
model.nb.stemmed.chi <- naiveBayes(result_value ~ ., data = train.stemmed.sparseDTM.tf.df.chi)

####### Predictions #######
# Predicting values using n-1 columns in the test set as the nth column is the actual value.
# stemmed predictions are suffixed with the word 'stemmed'
# Combined data predictions
mypred.rpart <- predict(model.rpart, test.sparseDTM.tf.df[ , 1:ncol(test.sparseDTM.tf.df) - 1], type = "class")
mypred.rpart.stemmed <- predict(model.rpart.stemmed, test.stemmed.sparseDTM.tf.df[ , 1:ncol(test.stemmed.sparseDTM.tf.df) - 1], type = "class")
mypred.nb <- predict(model.nb, test.sparseDTM.tf.df[ , 1:ncol(test.sparseDTM.tf.df) - 1] )
mypred.nb.stemmed <- predict(model.nb.stemmed, test.stemmed.sparseDTM.tf.df[ , 1:ncol(test.stemmed.sparseDTM.tf.df) - 1] )
# Boston
mypred.rpart.bos <- predict(model.rpart.bos, test.sparseDTM.tf.df.bos[ , 1:ncol(test.sparseDTM.tf.df.bos) - 1], type = "class")
mypred.rpart.stemmed.bos <- predict(model.rpart.stemmed.bos, test.stemmed.sparseDTM.tf.df.bos[ , 1:ncol(test.stemmed.sparseDTM.tf.df.bos) - 1], type = "class")
mypred.nb.bos <- predict(model.nb.bos, test.sparseDTM.tf.df.bos[ , 1:ncol(test.sparseDTM.tf.df.bos) - 1] )
mypred.nb.stemmed.bos <- predict(model.nb.stemmed.bos, test.stemmed.sparseDTM.tf.df.bos[ , 1:ncol(test.stemmed.sparseDTM.tf.df.bos) - 1] )
# Chicago
mypred.rpart.chi <- predict(model.rpart.chi, test.sparseDTM.tf.df.chi[ , 1:ncol(test.sparseDTM.tf.df.chi) - 1], type = "class")
mypred.rpart.stemmed.chi <- predict(model.rpart.stemmed.chi, test.stemmed.sparseDTM.tf.df.chi[ , 1:ncol(test.stemmed.sparseDTM.tf.df.chi) - 1], type = "class")
mypred.nb.chi <- predict(model.nb.chi, test.sparseDTM.tf.df.chi[ , 1:ncol(test.sparseDTM.tf.df.chi) - 1] )
mypred.nb.stemmed.chi <- predict(model.nb.stemmed.chi, test.stemmed.sparseDTM.tf.df.chi[ , 1:ncol(test.stemmed.sparseDTM.tf.df.chi) - 1] )


# Tabling the actual values vs the predicted values
# Combined
table(test.sparseDTM.tf.df[, c("result_value")], mypred.rpart)
table(test.stemmed.sparseDTM.tf.df[, c("result_value")], mypred.rpart.stemmed)
table(test.sparseDTM.tf.df[, c("result_value")], mypred.nb)
table(test.stemmed.sparseDTM.tf.df[, c("result_value")], mypred.nb.stemmed)
# Boston
table(test.sparseDTM.tf.df.bos[, c("result_value")], mypred.rpart.bos)
table(test.stemmed.sparseDTM.tf.df.bos[, c("result_value")], mypred.rpart.stemmed.bos)
table(test.sparseDTM.tf.df.bos[, c("result_value")], mypred.nb.bos)
table(test.stemmed.sparseDTM.tf.df.bos[, c("result_value")], mypred.nb.stemmed.bos)
# Chicago
table(test.sparseDTM.tf.df.chi[, c("result_value")], mypred.rpart.chi)
table(test.stemmed.sparseDTM.tf.df.chi[, c("result_value")], mypred.rpart.stemmed.chi)
table(test.sparseDTM.tf.df.chi[, c("result_value")], mypred.nb.chi)
table(test.stemmed.sparseDTM.tf.df.chi[, c("result_value")], mypred.nb.stemmed.chi)

####### Verification of Results ####### 
# Printing confusionMatrix values
# Combined data
confusionMatrix(table(test.sparseDTM.tf.df[, c("result_value")], mypred.rpart))
confusionMatrix(table(test.stemmed.sparseDTM.tf.df[, c("result_value")], mypred.rpart.stemmed))
confusionMatrix(table(test.sparseDTM.tf.df[, c("result_value")], mypred.nb))
confusionMatrix(table(test.stemmed.sparseDTM.tf.df[, c("result_value")], mypred.nb.stemmed))
# Boston
confusionMatrix(table(test.sparseDTM.tf.df.bos[, c("result_value")], mypred.rpart.bos))
confusionMatrix(table(test.stemmed.sparseDTM.tf.df.bos[, c("result_value")], mypred.rpart.stemmed.bos))
confusionMatrix(table(test.sparseDTM.tf.df.bos[, c("result_value")], mypred.nb.bos))
confusionMatrix(table(test.stemmed.sparseDTM.tf.df.bos[, c("result_value")], mypred.nb.stemmed.bos))
# Chicago
confusionMatrix(table(test.sparseDTM.tf.df.chi[, c("result_value")], mypred.rpart.chi))
confusionMatrix(table(test.stemmed.sparseDTM.tf.df.chi[, c("result_value")], mypred.rpart.stemmed.chi))
confusionMatrix(table(test.sparseDTM.tf.df.chi[, c("result_value")], mypred.nb.chi))
confusionMatrix(table(test.stemmed.sparseDTM.tf.df.chi[, c("result_value")], mypred.nb.stemmed.chi))

