source("C:/Users/jprasol/Documents/School/BizIntelligence/QTM7571.R")
library(gmodels)
library(class)
library(readr)

df <-read.csv("C:/Users/jprasol/Documents/School/BizIntelligence/evergreen_cleaned.csv") 
str(df)

#manage the data
df$url=NULL
df$urlid=NULL
df$boilerplate=NULL
df$alchemy_category=NULL
df$alchemy_category_score_clean=NULL  #swapped the names
df$is_news=NULL
df$news_front_page=NULL
df$framebased=NULL

#remove categorical variables because KNN won't use them
df$is_news_clean=NULL
df$news_front_page_clean=NULL
df$has_cat=NULL
df$alchemy_category_clean=NULL
str(df)
df$label=as.factor(df$label)

#partition the data
n=nrow(df)
trainingSize=round(n*0.75)
trainingCases=sample(n,trainingSize)  ##generates a dataframe of rowids
training = df[trainingCases,] ##pulls all the data associated with those rowids
test=df[-trainingCases,]

#model
predictions = kNN(label ~ ., training, test, k=9)
predictionsNoNorm = kNN(label ~ ., training, test, norm = F, k = 9)

str(df)

# evaluate performance
errorRate = sum(predictions != test$label)/nrow(test)
errorRateNoNorm = sum(predictionsNoNorm != test$label)/nrow(test)
CrossTable(predictions, test$label, expected=F, prop.r=F, prop.c=F, prop.t=F, prop.chisq=F)  # fancy table

# compare to benchmark
bench = benchmarkErrorRate(training$label, test$label)

# choose "best" value of k
k_best = kNNCrossVal(label ~ . , training)