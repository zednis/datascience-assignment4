getwd()
setwd("E:/RPI/DS 2016/Assignments/Project/")
list.files()

library(tm)
library(wordcloud)
library(rpart)
library(e1071)
library(rattle)
library(rpart.plot)

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

# summary(df.bos)
# summary(df.chi)

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
# comments.bos <- comments.bos[(comments.bos$result==""), ]
comments.chi <- comments.chi[(comments.chi$result==""), ]

# create dataframe from comments & result of chicago data
comments.chi <- df.chi[, c("comment", "result")]
comments.chi$comment <- as.character(comments.chi$comment)
comments.chi <- comments.chi[!(comments.chi$result=="No Entry"), ] # 0.1083% of records
comments.chi <- comments.chi[!(comments.chi$result=="Not Ready"), ] # 0.00966% of records
comments.chi <- comments.chi[!(comments.chi$result=="Out of Business"), ] # 0.0134% of records
comments.chi <- comments.chi[!(comments.chi$result=="Pass w/ Conditions"), ] # 7.2645% of records



# create dataframe from comments & result of all the data
comments <- rbind(df.bos[, c("comment", "result")], df.chi[, c("comment", "result")])
comments$comment <- as.character(comments$comment)

summary(comments)
#                 result      
# Fail              :379835  
# Pass              :472662  
# No Entry          :   997  
# Not Ready         :    89  
# Out of Business   :   124  
# Pass w/ Conditions: 66876

# Dropping rows other than Fail and Pass
comments <- comments[!(comments$result=="No Entry"), ] # 0.1083% of records
comments <- comments[!(comments$result=="Not Ready"), ] # 0.00966% of records
comments <- comments[!(comments$result=="Out of Business"), ] # 0.0134% of records
comments <- comments[!(comments$result=="Pass w/ Conditions"), ] # 7.2645% of records

# randomly subset data without replacement. Done to avoid memory issues. 
random.sample.subset = sample(nrow(comments), size = 100000, replace = FALSE)
summary(comments[random.sample.subset, ])

# Create corpus from randomly subset comments dataset
myCorpus <- Corpus(VectorSource(comments[random.sample.subset, c("comment")]))
myCorpus <- tm_map(myCorpus, tolower)
myCorpus <- tm_map(myCorpus, removePunctuation)
myCorpus <- tm_map(myCorpus, stripWhitespace)
myCorpus <- tm_map(myCorpus, PlainTextDocument)
myCorpus <- tm_map(myCorpus, removeWords, stopwords('english'))

# Create document term matrix with weighting as the term factor-inverse document factor
dtm.tfidf <- DocumentTermMatrix(myCorpus, control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE)))
# removeSparseTerms is to reduce the number of sparse terms in the Matrix. Done due to the highly sparse nature of the matrix.
sparseDTM.tfidf <- removeSparseTerms(dtm.tfidf, 0.975) #0.975 means keep terms (words) occuring in 97.5% of comments

# Create dataframe out of the data matrix. This takes a lot of time.
sparseDTM.tfidf.df <- as.data.frame(as.matrix(sparseDTM.tfidf))
# summary(sparseDTM.tfidf.df)
# str(sparseDTM.tfidf.df)
terms <- colnames(sparseDTM.tfidf.df) # saving the terms in a separate variable.

# Join DTM with corresponding results column from comments 
sparseDTM.tfidf.df = cbind(sparseDTM.tfidf.df, comments[random.sample.subset, c("result")])
colnames(sparseDTM.tfidf.df)[78] <- "result_value" # change the column name
# summary(sparseDTM.tfidf.df)

dt = sort(sample(nrow(sparseDTM.tfidf.df), nrow(sparseDTM.tfidf.df)*.7)) # indexes for splitting the data

train.sparseDTM.tfidf.df <- sparseDTM.tfidf.df[dt, ] # Creating training set
# This seems unnecessary, will refactor to remove this part later.
train.sparseDTM.tfidf.df$result_value <- comments[dt, c("result")] 
train.sparseDTM.tfidf.df <- train.sparseDTM.tfidf.df[ -c(79) ]
colnames(train.sparseDTM.tfidf.df)[78] <- "result_value"

# This seems unnecessary, will refactor to remove this part later.
test.sparseDTM.tfidf.df <- sparseDTM.tfidf.df[-dt, ]
test.sparseDTM.tfidf.df$result_value <- sparseDTM.tfidf.df[-dt, c("result") ]
test.sparseDTM.tfidf.df <- test.sparseDTM.tfidf.df[ -c(78) ]

# Need more testing here. First glance tells me that I seem to be doing something wrong.
# Fitting training data to a decision tree model 
model.rpart <- rpart(result_value ~ ., data = train.sparseDTM.tfidf.df, method = "class", 
                     control=rpart.control(minsplit=1, minbucket=1, cp=0.000001) )
plot(model.rpart)
text(model.rpart)
prp(model.rpart)

# Predicting values.
mypred.rpart <- predict(model.rpart, test.sparseDTM.tfidf.df, type = "class")
table(sparseDTM.tfidf.df[-dt, c("result_value") ], mypred.rpart)

# Fitting training data to a svm model 
# model.svm <- svm(result_value ~ ., data = train.sparseDTM.tfidf.df)
# Predicting values
# mypred.svm <- predict(model.svm, newdata = test.sparseDTM.tfidf.df, type = "class")
