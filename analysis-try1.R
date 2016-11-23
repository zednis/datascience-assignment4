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



# table of violations vs risk for boston
# tab.viol.risk.bos <- table(df.bos$violation, df.bos$risk)
# tab.viol.risk.bos <-
#   rbind(tab.viol.risk.bos, Total = colSums(tab.viol.risk.bos))
#
# table of violations vs risk for chicago
# tab.viol.risk.chi <- table(df.chi$violation, df.chi$risk)
# tab.viol.risk.chi <-
#   rbind(tab.viol.risk.chi, Total = colSums(tab.viol.risk.chi))

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

random.sample.subset.bos = sample(nrow(comments.bos), size = 50000, replace = FALSE)
random.sample.subset.chi = sample(nrow(comments.chi), size = 50000, replace = FALSE)

summary(comments.bos[random.sample.subset.bos, ] )
summary(comments.chi[random.sample.subset.chi, ] )

# create dataframe from comments & result of all the data
comments <- rbind(comments.bos[random.sample.subset.bos, ], comments.chi[random.sample.subset.chi, ])

summary(comments)
# comment           result     
# Length:100000      Fail:45119  
# Class :character   Pass:54881  
# Mode  :character 

# Create corpus from comment column of comments dataframe. 
myCorpus <- Corpus(VectorSource(comments[, c("comment")]))
myCorpus <- tm_map(myCorpus, tolower)
myCorpus <- tm_map(myCorpus, removePunctuation)
myCorpus <- tm_map(myCorpus, stripWhitespace)
myCorpus <- tm_map(myCorpus, PlainTextDocument)
myCorpus <- tm_map(myCorpus, removeNumbers)
myCorpus <- tm_map(myCorpus, removeWords, stopwords('english'))
myCorpus.stemmed <- tm_map(myCorpus, stemDocument) #stemming the corpus and storing it separately.

# Create document term matrix with weighting as the term factor-inverse document factor
# dtm.tfidf <- DocumentTermMatrix(myCorpus, control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE)))

# Create document term matrix with weighting as the term factor
dtm.tf <- DocumentTermMatrix(myCorpus, control = list(weighting = function(x) weightTf(x)))
stemmed.dtm.tf <- DocumentTermMatrix(myCorpus.stemmed, control = list(weighting = function(x) weightTf(x)))

# removeSparseTerms is to reduce the number of sparse terms in the Matrix. Done due to the highly sparse nature of the matrix.
# sparseDTM.tfidf <- removeSparseTerms(dtm.tfidf, 0.975) # 0.975 means keep terms (words) occuring in 97.5% of comments
sparseDTM.tf <- removeSparseTerms(dtm.tf, 0.96) # 0.96 means keep terms (words) occuring in 96% of comments
findFreqTerms(dtm.tf, lowfreq = 4500, highfreq = Inf) # Finding terms that occur atleast in 4600 comments.
Terms(sparseDTM.tf) # get terms in the sparseDTM.tf
intersect( findFreqTerms(dtm.tf, lowfreq = 4600, highfreq = Inf), 
           Terms(sparseDTM.tf)) # intersection of the results from findFreqTerms & Terms in sparseDTM.tf
setdiff( findFreqTerms(dtm.tf, lowfreq = 4600, highfreq = Inf), 
         Terms(sparseDTM.tf)) # set difference of the results from findFreqTerms & Terms in sparseDTM.tf



stemmed.sparseDTM.tf <- removeSparseTerms(stemmed.dtm.tf, 0.96) # 0.96 means keep terms (words) occuring in 96% of comments
findFreqTerms(stemmed.dtm.tf, lowfreq = 4600, highfreq = Inf) # Finding terms that occur atleast in 4600 comments.
Terms(stemmed.sparseDTM.tf) # get terms in stemmed.sparseDTM.tf
intersect( findFreqTerms(stemmed.dtm.tf, lowfreq = 4600, highfreq = Inf), 
          Terms(stemmed.sparseDTM.tf)) # intersection of the results from findFreqTerms & Terms in stemmed.sparseDTM.tf
setdiff( findFreqTerms(stemmed.dtm.tf, lowfreq = 4600, highfreq = Inf), 
          Terms(stemmed.sparseDTM.tf)) # set difference of the results from findFreqTerms & Terms in stemmed.sparseDTM.tf

# Create dataframe out of the data matrix. This takes a lot of time.
sparseDTM.tf.df <- as.data.frame(as.matrix(sparseDTM.tf))
stemmed.sparseDTM.tf.df <- as.data.frame(as.matrix(stemmed.sparseDTM.tf))
summary(sparseDTM.tf.df)
summary(stemmed.sparseDTM.tf.df)

# sparseDTM.tfidf.df <- as.data.frame(as.matrix(sparseDTM.tfidf))
# summary(sparseDTM.tfidf.df)
# str(sparseDTM.tfidf.df)
# Join DTM with results column from comments 
sparseDTM.tf.df = cbind(sparseDTM.tf.df, comments[ , c("result")], row.names = NULL)
colnames(sparseDTM.tf.df)[ncol(sparseDTM.tf.df)] <- "result_value" # change the column name

stemmed.sparseDTM.tf.df = cbind(stemmed.sparseDTM.tf.df, comments[ , c("result")], row.names = NULL)
colnames(stemmed.sparseDTM.tf.df)[ncol(stemmed.sparseDTM.tf.df)] <- "result_value" # change the column name

colSums(sparseDTM.tf.df[sparseDTM.tf.df$result_value == "Pass", 1:45] )
colSums(sparseDTM.tf.df[sparseDTM.tf.df$result_value == "Fail", 1:45] )
# csp <- colSums(sparseDTM.tf.df[sparseDTM.tf.df$result_value == "Pass", 1:45] )
# csf <- colSums(sparseDTM.tf.df[sparseDTM.tf.df$result_value == "Fail", 1:45] )
# csp.df = data.frame(x = Terms(sparseDTM.tf), y = csp)
# csf.df = data.frame(x = Terms(sparseDTM.tf), y = csf)

# ggplot(sparseDTM.tf.df, aes(x = colnames(sparseDTM.tf.df[ , 1:45 ]), fill = result_value)) + geom_bar(position = "dodge")
# colnames(sparseDTM.tfidf.df)[78] <- "result_value" # change the column name
# summary(sparseDTM.tfidf.df)

idx = sort(sample(nrow(sparseDTM.tf.df), nrow(sparseDTM.tf.df)*.7)) # indexes for splitting the data

train.sparseDTM.tf.df <- sparseDTM.tf.df[idx, ] # Creating training set
test.sparseDTM.tf.df <- sparseDTM.tf.df[-idx, ] # Creating test set

train.stemmed.sparseDTM.tf.df <- stemmed.sparseDTM.tf.df[idx, ] # Creating training set
test.stemmed.sparseDTM.tf.df <- stemmed.sparseDTM.tf.df[-idx, ] # Creating test set

# Need more testing here. First glance tells me that I seem to be doing something wrong.
# Fitting training data to a decision tree model 
model.rpart <- rpart(result_value ~ ., data = train.sparseDTM.tf.df, method = "class", 
                     control=rpart.control(minsplit=1, minbucket=1, cp=0.000001) )

model.rpart.stemmed <- rpart(result_value ~ ., data = train.stemmed.sparseDTM.tf.df, method = "class", 
                             control=rpart.control(minsplit=1, minbucket=1, cp=0.000001) )

model.nb <- naiveBayes(result_value ~ ., data = train.sparseDTM.tf.df)

# plot(model.rpart)
# text(model.rpart)
# prp(model.rpart)

# Predicting values.
mypred.rpart <- predict(model.rpart, test.sparseDTM.tf.df[ , 1:45], type = "class")
mypred.rpart.stemmed <- predict(model.rpart.stemmed, test.stemmed.sparseDTM.tf.df[ , 1:52], type = "class")
mypred.nb <- predict(model.nb, test.sparseDTM.tf.df[ , 1:45] )

table(test.sparseDTM.tf.df[, c("result_value")], mypred.rpart)
table(test.stemmed.sparseDTM.tf.df[, c("result_value")], mypred.rpart.stemmed)
table(test.sparseDTM.tf.df[, c("result_value")], mypred.nb)

summary(mypred.rpart)
summary(model.rpart)

confusionMatrix(table(test.sparseDTM.tf.df[, c("result_value")], mypred.rpart))
confusionMatrix(table(test.stemmed.sparseDTM.tf.df[, c("result_value")], mypred.rpart.stemmed))
confusionMatrix(table(test.sparseDTM.tf.df[, c("result_value")], mypred.nb))
# Fitting training data to a svm model 
# model.svm <- svm(result_value ~ ., data = train.sparseDTM.tfidf.df)
# Predicting values
# mypred.svm <- predict(model.svm, newdata = test.sparseDTM.tfidf.df, type = "class")
