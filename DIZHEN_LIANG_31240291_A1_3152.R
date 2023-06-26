install.packages("dplyr");
install.packages("ggplot2");
library(dplyr);
library(ggplot2);

rm(list=ls())
set.seed(31240291) #to make output reproducibale
setwd("C:/Users/DavidL/OneDrive/CS/FIT3152/A1")
covid <- read.csv("PsyCoronaBaselineExtract.csv", header =T)
covid<-covid[sample(nrow(covid),40000),]
head(covid)
dim(covid)
str(covid)
unique(covid$coded_country)
#Q1.(a)
#This data is a long format consisting of 64381 rows and 54 columns.
#There is a only one character type variable (text attribute) named coded_country, 
#the rest are all integer data type and each has multiple categorical variables
#There are a lot of NA (missing values) in multiple columns

#set arragement of plot
par(mfrow=c(1,1))
#select one example from each concept
cod_imp <- covid %>%
  select(affInsp, PLRAC19, disc02, jbInsec02, employstatus_10,
         PFS01, fail02, lifeSat, c19NormShould,trustGovState, edu,
         gender,age,c19ProSo01, c19ProSo02, c19ProSo03, c19ProSo04)

#boxplotting with text on x-axis in specific orientation
boxplot(cod_imp, las =2)
#title of boxplot
title("Boxplot of viewing value range one example from each concept")

summary(covid)

#For the concepts of Affect (affAnx, affBor, affCalm, affContent, affDepr, 
#affEnerg, affExc, affNerv, affExh, affInsp, and affRel), the range is from 1 
#(minimum) to 5 (maximum), with means ranging from 2.151 to 2.928 
#and medians ranging from 2 to 3.

#For the concept of Likelihood (PLRAC19 and PLRAEco), the range is 
#from 1 (minimum) to 8 (maximum), with means of 3.554 and 4.402 
#and medians of 4 for both.


#For the concepts of Social Discontent (disc01, disc02, and disc03), the range is 
#from -2 (minimum) to 2 (maximum), with means ranging from -0.4027 to 0.8355 
#and medians of 1 for all.


#For the concept of Job Insecurity (jbInsec01, jbInsec02, jbInsec03, and 
#jbInsec04), the range is from -2 (minimum) to 2 (maximum), 
#with means ranging from -0.982 to 0.56 and medians ranging from -2 to 0.
#For the concept of Employment Status (employstatus_1 to employstatus_10), 
#all values are 1 and there are no missing values.

#For the concept of Employment Status (employstatus_1 to employstatus_10), 
#all values are 1 and there are no missing values.

#For the concepts of Perceived Financial Strain (PFS01, PFS02, and PFS03), 
#the range is from -2 (minimum) to 2 (maximum), with means ranging 
#from -0.2513 to 0.5716 and medians ranging from 0 to 1.

#For the concept of Disempowerment (fail01, fail02, and fail03), the range 
#is from -2 (minimum) to 2 (maximum), with means ranging from -0.4099 to 0.3569 
#and medians ranging from -1 to 1.

#For the variable of Happy, the range is from 1 (minimum) to 10 (maximum), 
#with a mean of 6.333 and a median of 7.

#For the variable of Life Satisfaction (lifeSat), the range is from 1 (minimum) 
#to 6 (maximum), with a mean of 4.139 and a median of 4.

#For the concept of MLQ, the range is from -3 (minimum) to 3 (maximum), 
#with a mean of 0.8434 and a median of 1.


#For the concepts of Corona Community Injunctive norms 
#(c19NormShould and c19NormDo), the range is from -3 (minimum) to 3 (maximum), 
#with means of 2.002 and 1.298 and medians of 2 for both.

#For the concepts of Corona Community Injunctive norms 
#(c19IsStrict, c19IsPunish, and c19IsOrg),the range is from 1 (minimum) 
#to 6 (maximum), with means ranging from 3.499 to 4.121 and medians of 4 for all.

#For the concepts of Trust in Government Country(trustGovCtry and trustGovState), 
#the range is from 1 (minimum) to 5 (maximum), with means of 3.02 and 3.083 
#and medians of 3 for both.

#For the concept of Gender, the range is from 1 (minimum) to 3 (maximum), 
#with a mean of 1.389 and a median of 1.

#For the concept of Age, the range is from 1 (minimum) to 8 (maximum), 
#with a mean of 2.895 and a median of 3.

#For the concept of Education (edu), the range is from 1 (minimum) to 7 (maximum), 
#with a mean of 4.403 and a median of 5.

#For the concept of Coded Country (coded_country), it is a character variable 
#with a length of 64381.

#For the concepts of Covid-19 Pro-Social Behavior (c19ProSo01, c19ProSo02, and 
#c19ProSo03), the range is from -3 (minimum) to 3 (maximum), with means ranging 
#from 0.5434 to 0.9681 and medians of 1 for all.


#Q1.(b)
#since there are many NA in the dataset, the is.na would need to replace with
#them to mean of each column to have no effect to the dataset.
#However, for the binary categorcal variables should replace NA with 0
#since there are many categorical variables except coded_country, they might need
#factor function to be viewed as categorical variable in R
#since we have a focus country, it would be appropriate to group data that has 
#the same country into one, and then grow data from Germany as one
#after that, since we have focus country, it would be appropriate to grow rest 
#of the countries into one as a comparison

#Cleaning out all NAs
#replace NA with 0 in binary categorical variables, employment status variavles
covid[,21:30] <- lapply(covid[,21:30], function(x) {x[is.na(x)] <- 0;x})

# x is the column, treat x as a vector and us is.na to find NA,
#then replace NA with 0, last x to return the result.
#replace NA with columns(all before coded_country) corresponding mean values
covid[,1:(ncol(covid)-5)] <- lapply(covid[,1:(ncol(covid)-5)], function(x) 
  {x[is.na(x)] <- mean(x, na.rm = TRUE); x})

#character type column, can not implement is.na method (coded_country)
#covid[,(ncol(covid)-4)]

#replace NA with column (after coded_country) corresponding mean values
covid[,(ncol(covid)-3):ncol(covid)] <- lapply(covid[,(ncol(covid)-3):ncol(covid)]
                                              , function(x) {x[is.na(x)] <- mean(x, na.rm = TRUE); x})
str(covid)



#Q2.(a)

#data with just Germany
#germany_2 <- covid[which(covid$coded_country == "Germany"),]
#str(germany_2) #sliceing rows

germany = covid %>% filter(coded_country == "Germany")
others = covid %>% filter(coded_country != "Germany")

#no need na.rm since ,NA values are cleared in previous preocedures
germany %>% group_by(coded_country)%>%
  summarise(AC19PS1 = mean(c19ProSo01, na.rm=T), AC19PS2 = mean(c19ProSo02, na.rm=T), 
            AC19PS3 = mean(c19ProSo03, na.rm=T), AC19PS4 = mean(c19ProSo04, na.rm=T))

others %>% group_by(coded_country != "Germany")%>%
  summarise(AC19PS1 = mean(c19ProSo01, na.rm=T), AC19PS2 = mean(c19ProSo02, na.rm=T), 
            AC19PS3 = mean(c19ProSo03, na.rm=T), AC19PS4 = mean(c19ProSo04, na.rm=T))


#On average, Germany only has higher value in c19ProSo01, the rest are lower
#than other countries as a group, especially much lower in terms of AC19PS2 
#(Germany-0.162, others-0.689)

#t.test to see the differences between Germany and 
#other countries in all four response

#Germany vs Other Countries on c19ProSo Response
#Make columns to be called by columns names without calling name of data frame
attach(covid)
#null hypothesis, Germany'C19ProSo Response = Other Countries' C19ProSo Response
#alternative hypothesis: Germany'C19ProSo Response != Other Countries' C19ProSo
t.test(c19ProSo01[coded_country == "Germany"], c19ProSo01[coded_country != "Germany"]
       , "greater",conf.level = 0.95)

t.test(c19ProSo02[coded_country == "Germany"], c19ProSo02[coded_country != "Germany"]
       , "less",conf.level = 0.95)

t.test(c19ProSo03[coded_country == "Germany"], c19ProSo03[coded_country != "Germany"]
       , "less",conf.level = 0.95)

t.test(c19ProSo04[coded_country == "Germany"], c19ProSo04[coded_country != "Germany"]
       , "less",conf.level = 0.95)


#The p-value of 0.02 is less than 0.05, indicating strong evidence against the 
#null hypothesis that the means are equal and suggesting that there is a 
#significant difference between the average test scores of the two groups. 
#The 95% confidence interval for the difference between the means does not 
#contain 0, further supporting the conclusion that there is a significant 
#difference between the average test scores of the two groups.


#Q2.(b)
#c() function in R is used to combine values into a vector or list
#can be used to bind columns by binding multiple numbers in to a list
#C19PS1 = covid[,c(1:(ncol(covid)-5),(ncol(covid)-3))]
#str(C19PS1)
#C19PS1 = cbind(covid[,1:(ncol(covid)-5)],covid$c19ProSo01), cbind function
#cbind to bind columns, but would cause different p-values of all predictors,
#sicne c19ProSo01 is correlated with all other predictors

min_p <- function(p_va){
  # find the index of the predictor with the smallest p-value
  min_pvalue_index <- which.min(p_va[-1]) + 1
  
  # get the name of the predictor with the smallest p-value
  return(names(p_va)[min_pvalue_index])
}

#All predictors for all four reponses for germany
germany_p = germany[,1:(ncol(germany)-5)]

#p-vlaue < 0.05 to reject null hypothesis (that this predictor make not difference
# predicting the response, so conclude this predictor contribute to better predict
#response)

#fit to linear model 
#Corona ProSocial Behavior 1 with its predictor
PS1_fit_g = lm(formula = germany$c19ProSo01~., data = germany_p)
pvalues_1 <- summary(PS1_fit_g)$coefficients[, 4]
pvalues_1[pvalues_1 < 0.05]#treat it as list
min_p(pvalues_1)

#Corona ProSocial Behavioure 2 with its predictor
PS2_fit_g = lm(formula = germany$c19ProSo02~., data = germany_p)
pvalues_2 <- summary(PS2_fit_g)$coefficients[, 4]
pvalues_2[pvalues_2 < 0.05]#treat it as list
min_p(pvalues_2)

#Corona ProSocial Behavioure 3 with its predictor
PS3_fit_g = lm(formula = germany$c19ProSo03~., data = germany_p)
pvalues_3 <- summary(PS3_fit_g)$coefficients[, 4]
pvalues_3[pvalues_3 < 0.05]#treat it as list
min_p(pvalues_3)

#Corona ProSocial Behavioure 4 with its predictor
PS4_fit_g = lm(formula = germany$c19ProSo04~., data = germany_p)
pvalues_4 <- summary(PS4_fit_g)$coefficients[, 4]
pvalues_4[pvalues_4 < 0.05]#treat it as list
min_p(pvalues_4)

#the predictors that have p-values less than 0.05(enough to reject null hypothesis) 
#in all four fitted linear models
pvalues_1[pvalues_1 < 0.05 & pvalues_2 < 0.05 & pvalues_3 < 0.05 & pvalues_4 < 0.05]

#ploting linear model of Germany,
par(mfrow=c(2,2))
plot(PS1_fit_g)
plot(PS2_fit_g)
plot(PS3_fit_g)
plot(PS4_fit_g)
# residuals vs Fitted residuals(the difference between the observed and predicted values) 
#on the y-axis and the fitted values (the predicted values) on the x-axis. 
#This plot is used to check for non-linearity, unequal error variances, and outliers.

#Non-linearity: curved pattern in the Residuals vs Fitted plot

#Unequal error variances: If the error variances are not equal 
#(also known as heteroscedasticity), funnel shape in the Residuals vs Fitted plot. 
#This suggests that the assumption of constant variance is violated.

#Outliers: Outliers are observations that have large residuals (i.e., 
#they are far from the fitted line). In the Residuals vs Fitted plot, outliers
#will appear as points that are far from the horizontal line at zero residual.


#Normal Q-Q plot compares the quantiles of the standardized residuals to 
#the quantiles of a standard normal distribution. 
#This plot is used to check if the residuals are normally distributed.


#2(c) 
#Other Countries as a group
#All predictors for all four response for other countries
others_p = others[,1:(ncol(others)-5)]

#fit to linear model
#Corona ProSocial Behavior 1 with its predictor
PS1_fit = lm(formula = others$c19ProSo01~., data = others_p)
pvalues_1 <- summary(PS1_fit)$coefficients[, 4]
pvalues_1[pvalues_1 < 0.05]#treat it as list
min_p(pvalues_1)

#Corona ProSocial Behavioure 2 with its predictor
PS2_fit = lm(formula = others$c19ProSo02~., data = others_p)
pvalues_2 <- summary(PS2_fit)$coefficients[, 4]
pvalues_2[pvalues_2 < 0.05]#treat it as list
min_p(pvalues_2)

#Corona ProSocial Behavioure 3 with its predictor
PS3_fit = lm(formula = others$c19ProSo03~., data = others_p)
pvalues_3 <- summary(PS3_fit)$coefficients[, 4]
pvalues_3[pvalues_3 < 0.05]#treat it as list
min_p(pvalues_3)

#Corona ProSocial Behavioure 4 with its predictor
PS4_fit = lm(formula = others$c19ProSo04~., data = others_p)
pvalues_4 <- summary(PS4_fit)$coefficients[, 4]
pvalues_4[pvalues_4 < 0.05]#treat it as list
min_p(pvalues_4)

#the predictors that have p-values less than 0.05(very important predictors) 
#in all four fitted linear models
pvalues_1[pvalues_1 < 0.05 & pvalues_2 < 0.05 & pvalues_3 < 0.05 & pvalues_4 < 0.05]

#ploting linear model of other countries, 
par(mfrow=c(2,2))

plot(PS1_fit)
plot(PS2_fit)
plot(PS3_fit)
plot(PS4_fit)


#3(a)
#clustering algorithms such as k-means and hierarchical clustering operate on 
#individual observations and try to group similar observations into clusters. 
#If you have multiple observations for the same country, the algorithm would 
#treat each observation as a separate entity and could potentially assign 
#different observations for the same country to different clusters.
#By aggregating the data for each country before performing the clustering 
#analysis, you ensure that each country is represented by a single observation 
#in the data. This allows the clustering algorithm to group countries into 
#clusters based on their similarity in the chosen indicators.



#cbind return character matrix if there is chr type predictors
#use data.frame can retain their original name


#k-mean clustering to to cluster similiar contries
library(cluster)


covid_p = covid[,1:(ncol(others)-5)]

#fit to linear model
#Corona ProSocial Behavior 1 with its predictor
PS1_fit = lm(formula = covid$c19ProSo01~., data = covid_p)
pvalues_1 <- summary(PS1_fit)$coefficients[, 4]
min_p(pvalues_1)

#Corona ProSocial Behavioure 2 with its predictor
PS2_fit = lm(formula = covid$c19ProSo02~., data = covid_p)
pvalues_2 <- summary(PS2_fit)$coefficients[, 4]
min_p(pvalues_2)

#Corona ProSocial Behavioure 3 with its predictor
PS3_fit = lm(formula = covid$c19ProSo03~., data = covid_p)
pvalues_3 <- summary(PS3_fit)$coefficients[, 4]
min_p(pvalues_3)

#Corona ProSocial Behavioure 4 with its predictor
PS4_fit = lm(formula = covid$c19ProSo04~., data = covid_p)
pvalues_4 <- summary(PS4_fit)$coefficients[, 4]
min_p(pvalues_4)

#the predictors that have p-values less than 0.05(very important predictors) 
#in all four fitted linear models
pvalues_1[pvalues_1 < 0.001 & pvalues_2 < 0.001 & pvalues_3 < 0.001 & pvalues_4 < 0.001]


# Create imp by selecting most important predictors (pvalue < 0.001) in covid dataset
imp <- covid %>%
  select(coded_country, affInsp, disc02, jbInsec02, employstatus_10,
         fail01, fail02, lifeSat, c19NormShould,
         c19NormDo, c19IsOrg, trustGovState, edu,
         c19ProSo01, c19ProSo02, c19ProSo03, c19ProSo04)

#aggreagte by countries
#median_by_group <- aggregate(x ~ group, data = mydata, FUN = median)
csmall = aggregate(imp[,2:ncol(imp)],list(imp$coded_country),mean)
colnames(csmall)[1] = "coded_country"

#scaling to make all indicators have equal weight in the clustering algorithm
csmall[2:ncol(csmall)]<-scale(csmall[2:ncol(csmall)])

#choose optimal number of clusters (k) with average silhouette score
i_silhouette_score <- function(k) {
                      #start from 2 to avoid coded_country
  km <- kmeans(csmall[,2:ncol(csmall)], k, nstart = 50)#start from 50 cluster centrods
  #more starts to make clustering more stable
  ss <- silhouette(km$cluster, dist(csmall[,2:ncol(csmall)]))
  mean(ss[,3]) #mean of the third column of the silhouette scores
  #calculates the average silhouette width for all observations in the data
  #
  #R returns a matrix with three columns. The first column contains the cluster 
  #assignments for each observation, the second column contains the neighbor 
  #cluster (the second-best cluster assignment for each observation), and the 
  #third column contains the silhouette width for each observation.
}

k <- 2:20  #creates a vector k containing the values from 2 to 20
#sapply function to apply the i_silhouette_score function to each value of k
avg_sil <- sapply(k, i_silhouette_score) 
#retrieve k(number of clusters) that has highest average silhouette score
k[which.max(avg_sil)]
par(mfrow=c(1,1))

#create a line plot of the average silhouette scores against number of clusters
plot(k, type ='b', avg_sil, xlab='Number of clusters', ylab = 'Average Silhouette Score')
#Add text of number of clustet to every point
text(k, avg_sil, labels=k, cex=0.8)
title("Average Silhouette Score against Numebr of clusters (k)")


set.seed(31240291)
#fit with kmeans clustering with k = 4, number of centroids start from 20
zkfit = kmeans(csmall[2:ncol(csmall)], centers = 4, nstart = 20)

# Add cluster assignments to data frame
csmall$cluster <- zkfit$cluster

# Move column coded_country to the first column
csmall <- cbind(csmall$cluster, csmall[,setdiff(names(csmall), "cluster")])
#find out Germany in which cluster
csmall %>% filter(coded_country == "Germany") #cluster 3
#find out similar countries in the same cluster
sim_3 = csmall %>% filter(`csmall$cluster`==3)


#Q3(b)
#fit to linear model
#Corona ProSocial Behavior 1 with its predictor

#table can frequncies for unique values
                 #as.character to perserve character type otherwise 
country_count = table(as.character(covid$coded_country)) #changed to factor type

#change to dataframe to have separate column of countries and frequencies
cc_frame = as.data.frame(country_count)
#change back the type to character from factor for the following filtering
cc_frame$Var1 <- as.character(cc_frame$Var1)
str(cc_frame)


# subset dataframe to include only rows where Var1 has those countries in sim_3
sim_cc <- filter(cc_frame, Var1 %in% sim_3$coded_country)
   #filter is prioritized option ,since subset does not work

sim_cc <- sim_cc[order(sim_cc$Freq, decreasing = TRUE), ]


#choose USA, Spain and Greece, highest observations in similar countries
usa = covid %>% filter(coded_country == "United States of America")

usa_p = usa[,1:(ncol(usa)-5)]
#fit to linear model
#Corona ProSocial Behavior 1 with its predictor
PS1_fit_g = lm(formula = usa$c19ProSo01~., data = usa_p)
pvalues_1 <- summary(PS1_fit_g)$coefficients[, 4]
pvalues_1[pvalues_1 < 0.05]#treat it as list
min_p(pvalues_1)

#Corona ProSocial Behavioure 2 with its predictor
PS2_fit_g = lm(formula = usa$c19ProSo02~., data = usa_p)
pvalues_2 <- summary(PS2_fit_g)$coefficients[, 4]
pvalues_2[pvalues_2 < 0.05]#treat it as list
min_p(pvalues_2)

#Corona ProSocial Behavioure 3 with its predictor
PS3_fit_g = lm(formula = usa$c19ProSo03~., data = usa_p)
pvalues_3 <- summary(PS3_fit_g)$coefficients[, 4]
pvalues_3[pvalues_3 < 0.05]#treat it as list
min_p(pvalues_3)

#Corona ProSocial Behavioure 4 with its predictor
PS4_fit_g = lm(formula = usa$c19ProSo04~., data = usa_p)
pvalues_4 <- summary(PS4_fit_g)$coefficients[, 4]
pvalues_4[pvalues_4 < 0.05]#treat it as list
min_p(pvalues_4)

#the predictors that have p-values less than 0.05
#in all four fitted linear models
pvalues_1[pvalues_1 < 0.05 & pvalues_2 < 0.05 & pvalues_3 < 0.05 & pvalues_4 < 0.05]

#ploting linear model of USA, 
par(mfrow=c(2,2))

plot(PS1_fit_g)
plot(PS2_fit_g)
plot(PS3_fit_g)
plot(PS4_fit_g)



#Spain
spain = covid %>% filter(coded_country == "Spain")

spain_p = spain[,1:(ncol(usa)-5)]

#fit to linear model
#Corona ProSocial Behavior 1 with its predictor
PS1_fit_g = lm(formula = spain$c19ProSo01~., data = spain_p)
pvalues_1 <- summary(PS1_fit_g)$coefficients[, 4]
pvalues_1[pvalues_1 < 0.05]#treat it as list
min_p(pvalues_1)

#Corona ProSocial Behavioure 2 with its predictor
PS2_fit_g = lm(formula = spain$c19ProSo02~., data = spain_p)
pvalues_2 <- summary(PS2_fit_g)$coefficients[, 4]
pvalues_2[pvalues_2 < 0.05]#treat it as list
min_p(pvalues_2)

#Corona ProSocial Behavioure 3 with its predictor
PS3_fit_g = lm(formula = spain$c19ProSo03~., data = spain_p)
pvalues_3 <- summary(PS3_fit_g)$coefficients[, 4]
pvalues_3[pvalues_3 < 0.05]#treat it as list
min_p(pvalues_3)

#Corona ProSocial Behavioure 4 with its predictor
PS4_fit_g = lm(formula = spain$c19ProSo04~., data = spain_p)
pvalues_4 <- summary(PS4_fit_g)$coefficients[, 4]
pvalues_4[pvalues_4 < 0.05]#treat it as list
min_p(pvalues_4)

#the predictors that have p-values less than 0.001(very important predictors) 
#in all four fitted linear models
pvalues_1[pvalues_1 < 0.05 & pvalues_2 < 0.05 & pvalues_3 < 0.05 & pvalues_4 < 0.05]

#ploting linear model of Spain, 
par(mfrow=c(2,2))

plot(PS1_fit_g)
plot(PS2_fit_g)
plot(PS3_fit_g)
plot(PS4_fit_g)


#Greece
greece = covid %>% filter(coded_country == "Greece")

greece_p = greece[,1:(ncol(greece)-5)]

#fit to linear model
#Corona ProSocial Behavior 1 with its predictor
PS1_fit_g = lm(formula = greece$c19ProSo01~., data = greece_p)
pvalues_1 <- summary(PS1_fit_g)$coefficients[, 4]
pvalues_1[pvalues_1 < 0.05]#treat it as list
min_p(pvalues_1)

#Corona ProSocial Behavioure 2 with its predictor
PS2_fit_g = lm(formula = greece$c19ProSo02~., data = greece_p)
pvalues_2 <- summary(PS2_fit_g)$coefficients[, 4]
pvalues_2[pvalues_2 < 0.05]#treat it as list
min_p(pvalues_2)

#Corona ProSocial Behavioure 3 with its predictor
PS3_fit_g = lm(formula = greece$c19ProSo03~., data = greece_p)
pvalues_3 <- summary(PS3_fit_g)$coefficients[, 4]
pvalues_3[pvalues_3 < 0.05]#treat it as list
min_p(pvalues_3)

#Corona ProSocial Behavioure 4 with its predictor
PS4_fit_g = lm(formula = greece$c19ProSo04~., data = greece_p)
pvalues_4 <- summary(PS4_fit_g)$coefficients[, 4]
pvalues_4[pvalues_4 < 0.05]#treat it as list
min_p(pvalues_4)

#the predictors that have p-values less than 0.05(very important predictors) 
#in all four fitted linear models
pvalues_1[pvalues_1 < 0.05 & pvalues_2 < 0.05 & pvalues_3 < 0.05 & pvalues_4 < 0.05]

#ploting linear model of Greece, 
par(mfrow=c(2,2))

plot(PS1_fit_g)
plot(PS2_fit_g)
plot(PS3_fit_g)
plot(PS4_fit_g)
