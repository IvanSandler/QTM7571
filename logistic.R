#exponential growth - odds double for each increase.
#undo exponents.. -- logs undo exponents. Log of the odds is linear.
#we can use linear regression on the log-odds
#odds are a multipicative factor determined by the beta

#logistic regression only works on logical variables
#mult-nomial is an extension

#family=binomial(logit) -- what flavor of S shaped curve

#probability that we would observe this if there was no relationship between the independant variable
#interpretation of the P value. Probability of a negative statement.

#beta = As input increases by 1 unit, the odds that the target is true increases 
#by a multiplicative factor of E^beta
#you need to know this. 

# error rate = #errors/total population
# is it a good model -- always refer back to the benchmark

library(gmodels)
source("C:/Users/jprasol/Documents/School/BizIntelligence/QTM7571.R") #source

df <-read.csv("C:/Users/jprasol/Documents/School/BizIntelligence/evergreen_cleaned.csv") 
str(df)

#manage the data
#removing uncleaned or redundant fields
df$url=NULL
df$urlid=NULL
df$boilerplate=NULL
df$alchemy_category=NULL
df$alchemy_category_score_clean=NULL  #swapped the names
df$is_news=NULL
df$news_front_page=NULL
df$framebased=NULL

str(df)
#convert to factors
df$is_news_clean=as.factor(df$is_news_clean)
df$news_front_page_clean=as.factor(df$news_front_page_clean)
df$has_cat=as.factor(df$has_cat)
df$label=as.logical(df$label)

str(df)

#partition the data
n=nrow(df)
trainingSize=round(n*0.75)
trainingCases=sample(n,trainingSize)  ##generates a dataframe of rowids
training = df[trainingCases,] ##pulls all the data associated with those rowids
test=df[-trainingCases,]

#build the model
model = glm(label ~ ., data=training, family
            = binomial(logit))
summary(model)

#predict
predictions=predict(model, test, type="response")
predictTF =((predictions > 0.5))
obs=(test$label)


bench=benchmarkErrorRate(training$label,test$label)
error_rate=(sum(predictTF != obs)/nrow(test))*100


CrossTable(predictTF, obs, expected=F,
           prop.r=F, prop.c=F, prop.t=F, prop.chisq=F)
#crosstable is in Gmodel

##New Model with just the variables
model2 = glm(label ~ alchemy_category_clean + alchemy_category_score + commonlinkratio_3
                    + frameTagRatio + linkwordscore + non_markup_alphanum_characters
                    + numberOfLinks + spelling_errors_ratio
                    , data=training, family = binomial(logit))
summary(model2)

predictions2=predict(model2, test, type="response")
predictTF2 =((predictions2 > 0.5))
obs2=(test$label)


bench2=benchmarkErrorRate(training$label,test$label)
error_rate2=(sum(predictTF2 != obs2)/nrow(test))*100


CrossTable(predictTF, obs, expected=F,
           prop.r=F, prop.c=F, prop.t=F, prop.chisq=F)



#lift curve:
#up every time we observe a true, right when we have a false. Perfect would be an L
#grades us on how sure we were about it before we got along.
#sensitivity - given that the targets is true, what is the probability we got it right.


sensitivity = sum(obs==TRUE & predictTF==TRUE)/sum(obs==TRUE) ##probability of correctly predicting true
specificity = sum(obs==FALSE & predictTF==FALSE)/sum(obs==FALSE) ##probability of correctly predicting fals

#ROC chart  sensitivity vs. 1-specificity
#trying to give you an idea of how sensitivity and specifity change as you vary the cutoff probability.
ROCChart(obs,predictions)

liftChart(obs,predictions)
