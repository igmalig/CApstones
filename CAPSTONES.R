# Install packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(GGally)) install.packages("GGally", repos = "http://cran.us.r-project.org")
if(!require(forecast)) install.packages("forecast", repos = "http://cran.us.r-project.org")
if(!require(foreign)) install.packages("foreign", repos = "http://cran.us.r-project.org")
if(!require(haven)) install.packages("haven", repos = "http://cran.us.r-project.org")

# Enable packages
library(foreign) # Converting datasets to R format
library(forecast) # Forecasting functions for time series and linear models
library(ggplot2) # Elegant data visualization
library(GGally) # Extension of ggplot2
library(tidyverse)
library(haven)
library(caret) 

#Download datasets
engldata<- read_dta("https://github.com/igmalig/P/blob/master/engl.dta?raw=true")
community<- read_dta("https://github.com/igmalig/P/blob/master/comunit1.dta?raw=true")
childdata<- read_dta("https://github.com/igmalig/P/blob/master/child2.dta?raw=true")
cebudata<- read_dta("https://github.com/igmalig/P/blob/master/cebu.dta?raw=true")
childfile<- read_dta("https://github.com/igmalig/P/blob/master/cdietiq.dta?raw=true")
motherdata<- read_dta("https://github.com/igmalig/P/blob/master/mother.dta?raw=true")
mathdata<- read_dta("https://github.com/igmalig/P/blob/master/math.dta?raw=true")

set.seed(1)
#Display column names
colnames(community)

#Rename 2nd column to CURBRGY2
names(community)[2] <- "CURBRGY2"

#Set community1
community1 <- community[c(2,26,42,43,44,45,54,55,56,57)]
#2 CURBRGY 2 - Current Barangay
#26 density - POPULATION DENSITY PERSONS PER SQUARE KILOMETER
#42 compubel - Are there Complete Public Elementary Schools in the Barangay? If yes how many?
#43 macpubel - TOTAL NUMBER OF MALE STUDENTS IN THE COMPLETE PUBLIC ELEMENTARY SCHOOL(S)
#44 fecpubel - TOTAL NUMBER OF FEMALE STUDENTS IN THE COMPLETE PUBLIC ELEMENTARY SCHOOL(S)
#45 dicpel- How far in (km) is the brgy. center to the closest complete public school not found in the barangay?
#54 cprvtels - Are there Complete Private Elementary Schools Barangay? If yes how many?
#55 macprvte - TOTAL NUMBER OF MALE STUDENTS IN THE COMPLETE PRIVATE ELEMENTARY SCHOOL(S)
#56 fecprvte - TOTAL NUMBER OF FEMALE STUDENTS IN THE COMPLETE PRIVATE ELEMENTARY SCHOOL(S)
#57 dicprvte- HOW FAR (IN KM.) IS THE BRGY CENTER TO THE CLOSEST COMPLETE PRIVATE ELEMENTARY SCHOOL NOT FOUND IN BARANGAY?

count(community1,dicpel, wt = NULL, sort = FALSE)
count(community1,density, wt = NULL, sort = FALSE)

#Set math data
mathdata1 <- mathdata[c(1,14,15,80)]
#1 CURBRGY2 - CURRENT BARANGAY OF RESIDENCE OF CHILD
#2 SCHOLBA2 - BARANGAY IN WHICH SCHOOL IS LOCATED X
#14 HHNUM942 - CHILD'S 1991 HOUSEHOLD ID NUMBER
#15 WOMAN942 - 1994 WOMAN ID NUMBER
#16 CHLD CODE - CHILD's CODE 
#80 SCORE2 - SCORE OF CHILD IN MATHEMATICS ACHIEVEMENT TEST

#Set english data
engldata1 <- engldata[c(1,14,15,80)]
#1 CURBRGY2 - CURRENT BARANGAY OF RESIDENCE OF CHILD
#2 SCHOLBA2 - BARANGAY IN WHICH SCHOOL IS LOCATED X
#14 HHNUM942 - CHILD'S 1991 HOUSEHOLD ID NUMBER
#15 WOMAN942 - 1994 WOMAN ID NUMBER
#16 CHLDCODE - CHILD's CODE 
#80 SCORE2 - SCORE OF CHILD IN ENGLISH ACHIEVEMENT TEST

#Set cebu data
cebudata1 <- cebudata[c(1,14,15,50)]
#1 CURBRGY2 - CURRENT BARANGAY OF RESIDENCE OF CHILD
#2 SCHOLBA2 - BARANGAY IN WHICH SCHOOL IS LOCATED X
#3 SCHLCODE - CODED NAME OF SCHOOL X
#14 HHNUM942 - CHILD'S 1991 HOUSEHOLD ID NUMBER
#15 WOMAN942 - 1994 WOMAN ID NUMBER
#16 CHLDCODE - CHILD's CODE 
#50 SCORE 2 - SCORE OF CHILD IN CEBUANO READING ACHIEVEMENT TEST

#agechild <- (94-childfile$YRBIRTC2)
count(childfile,YRBIRTC2, wt = NULL,sort=FALSE)
childfile$CURSTRA2[childfile$CURSTRA2==1] <- 0
childfile$CURSTRA2[childfile$CURSTRA2==2] <- 1
childfile1 <- childfile[c(2,3,14,22,45,232)]
#childfile1 <- cbind(childfile1, agechild)
#2 CURBRGY2 - CURRENT BARANGAY
#3 CURSTRA2 - CURRENT STRATUM (SEE APPENDIX 1)
#14 HHNUM942 - 1994 HOUSEHOLD ID NUMBER
#20 YRBIRTC2 - YEAR OF BIRTH OF CHILD -> Determine Age  
#22 CHLNO942 - CHILD'S LINE NUMBER IN 1994
#45 MO1AB942 - NUMBER OF DAYS CHILD ABSENT IN MONTH1 (SY 1994-1995)
#232 IQSCORE - CHILD'S TOTAL SCORE FOR NONVERBAL INTELLIGENCE TEST
#agechild - age of child

#Merge the examination data with community1 data
mergemathdata <- merge(mathdata1,community1)
head(mergemathdata)
mergeengldata <- merge(engldata1,community1)
head(mergeengldata)
mergecebudata <- merge(cebudata1,community1)
head(mergecebudata)
mergechildfiledata <- merge(childfile1,community1)
head(mergechildfiledata)

#See age of child and rescale sex as 1 and 0 instead of 2 and 1
agechild <- (94-childdata$YRBIRTC2)
colnames(childdata)[1] <- "CURBRGY2"
childdata$SEXCHLD2[childdata$SEXCHLD2==1] <- 0
childdata$SEXCHLD2[childdata$SEXCHLD2==2] <- 1
childdata1 <- childdata[c(1,11,12,14,19,90,91,92,93,94,101,144)]
childdata1 <- cbind(childdata1,agechild)
#1 BASEBRGY to CURBRGY2 - BASE BARANGAY - Change name to CURBRGY2
#11 HHNUM942 - 1994 HOUSEHOLD ID NUMBER
#12 WOMAN942 - 1994 WOMAN ID NUMBER
#14 SEXCHLD2 - SEX OF INDEX CHILD 
#16 YRBIRTC2 - YEAR OF BIRTH OF INDEX CHILD X
#17 CHLDCODE - CHILD'S CODE X
#18 CHLNO942- LINE NUMBER OF INDEX CHILD (1994) X
#19 SCHOLIN2 - HAS INDEX CHILD EVER ATTENDED SCHOOL?
#90 TRAVHR2 - ON A REGULAR SCHOOLDAY, HOW MANY HOURS DOES INDEX CHILD SPEND TRAVELLING TO SCHOOL?
#91 WORKHR2 - ON A REGULAR SCHOOLDAY, HOW MANY HOURS DOES INDEX CHILD SPEND WORKING FOR PAY OR ON FARM OR FAMILY BUSINESS?
#92 CHOREHR2 - ON A REGULAR SCHOOLDAY, HOW MANY HOURS DOES INDEX CHILD SPEND HELPING WITH HOUSEHOLD CHORES?
#93 CARESIB2 - ON A REGULAR SCHOOLDAY, HOW MANY HOURS DOES INDEX CHILD SPEND CARING OF YOUNGER SIBLINGS?
#94 PLAYHR2 - ON A REGULAR SCHOOLDAY, HOW MANY HOURS DOES INDEX CHILD SPEND PLAYING?
#101 MISSEDA2 - IN THE PAST MONTH, HOW MANY DAYS HAS INDEX CHILD MISSED SCHOOL WHEN IT WAS IN SESSION?
#129 CAUSEH12 - CAUSE OF INDEX CHILD'S FIRST HOSPITALIZATION X
#144 ILLNESC2 - DOES INDEX CHILD HAVE ANY CHRONIC ILLNESS/DISABILITY?
#agechild

#Merge examination data with childdata
MERGEMATH <- merge(mergemathdata,childdata1)
MERGEENGL <- merge(mergeengldata,childdata1)
MERGECEBU <- merge(mergecebudata,childdata1)
MERGECHILDFILE <- merge(mergechildfiledata,childdata1)
count(MERGEENGL,SEXCHLD2, wt = NULL,sort=FALSE)

colnames(motherdata)[1] <- "CURBRGY2"
motherdata$CAREKID2[motherdata$CAREKID2==2] <- 0
motherdata$CAREKID2[motherdata$CAREKID2==3] <- 0
motherdata$CAREKID2[motherdata$CAREKID2==4] <- 0
motherdata$WITHSPO2[motherdata$WITHSPO2==3] <- 0
motherdata$WITHSPO2[motherdata$WITHSPO2==2] <- 0
motherdata$LIVTODA2 <- motherdata$LIVTODA2-1

colnames(motherdata)[32] <- "NUMSIBLINGS"

#Create motherdata1
motherdata1 <- motherdata[c(1,2,12,11,32,43,130)]
#1 BASEBRG2 to CURBRGY2 - BASELINE BARANGAY ID NUMBER
#11 HHNUM942 - 1994 HOUSEHOLD ID NUMBER
#14 LINENUM2 - LINE NUMBER OF MOTHER/CARETAKER X
#12 WOMAN942 - 1994 WOMAN ID NUMBER
#21 TYPEPA12 - HOW IS MOTHER DOING HER MAIN JOB? X
#16 MARSTAT2 - MARITAL STATUS OF MOTHER/CARETAKER X
#25 TYPEPA22 - HOW IS MOTHER PAID ON HER SECOND JOB? X
#32 LIVTODA2 - TOTAL NUMBER OF CHILDREN MOTHER HAVE GIVEN BIRTH TO WHO ARE STILL ALIVE TODAY
#47 WITHSPO2 - IS MOTHER CURRENTLY LIVING WITH HUSBAND NOW?
#130 CAREKID2 - CAN MOTHER STILL TAKE CARE OF THE CHILDREN?

sum(motherdata$CAREKID2)
count(motherdata,CAREKID2, wt = NULL, sort = FALSE)

#Merge mother data with the examination data
MERGEMATHFINAL <- merge(motherdata1,MERGEMATH)
MERGEENGLFINAL <- merge(motherdata1,MERGEENGL)
MERGECEBUFINAL <- merge(motherdata1,MERGECEBU)
MERGECHILDFILEFINAL <- merge(motherdata1,MERGECHILDFILE)

#Regression
mathreg <- lm(SCORE2 ~ SEXCHLD2 + CAREKID2 + agechild + ILLNESC2 + MISSEDA2  + NUMSIBLINGS + WITHSPO2 + TRAVHR2, data=MERGEMATHFINAL)
engreg <- lm(SCORE2 ~ SEXCHLD2 + CAREKID2 + agechild + ILLNESC2 + MISSEDA2  + NUMSIBLINGS + WITHSPO2 + TRAVHR2, data=MERGEENGLFINAL)
cebureg <- lm(SCORE2 ~ SEXCHLD2 + CAREKID2 + agechild + ILLNESC2 + MISSEDA2 +  NUMSIBLINGS + WITHSPO2 + TRAVHR2, data=MERGECEBUFINAL)
nonverbalreg <- lm(iqscore ~ SEXCHLD2 + CURSTRA2 + agechild + CAREKID2 + ILLNESC2 + MISSEDA2 + NUMSIBLINGS + WITHSPO2 + TRAVHR2, data=MERGECHILDFILEFINAL)

#Regression summary
summary(mathreg) 
summary(engreg)
summary(cebureg) 
summary(nonverbalreg) 

fitted(mathreg)
fitted(engreg)
fitted(cebureg)
fitted(nonverbalreg)

count(MERGECEBUFINAL,SCORE2, wt = NULL,sort=FALSE)

summary(child1234)

singlelinear <- lm(SCORE2~ dicprvte, data=MERGEMATHFINAL)
summary(singlelinear)
plot(singlelinear)


#Dependent Variables 
#SCORE2 - SCORE OF CHILD IN MATH/ENGLISH/CEBUANOREADING ACHIEVEMENT TEST
#IQSCORE - CHILD'S TOTAL SCORE FOR NONVERBAL INTELLIGENCE TEST
#Independent Variables: 
# compubel - Are there Complete Public Elementary Schools in the Barangay? If yes how many?
# dicpel- How far in (km) is the brgy. center to the closest complete public school not found in the barangay?
# cprvtels - Are there Complete Private Elementary Schools Barangay? If yes how many?
# dicprvte -  HOW FAR (IN KM.) IS THE BRGY CENTER TO THE CLOSEST COMPLETE PRIVATE ELEMENTARY SCHOOL NOT FOUND IN BARANGAY?
#SEXCHLD2 - Sex of Child
#CURSTRA2 - Urban or Rural Barangay 
#agechild - age of child during interview 
#CAREKID2 - Can mother take care of child (Yes=1, No=0, Yes,but has some difficulty=0.75, Yes,but has extreme difficultry =0.25)
#SCHOLIN2 - Has Index Child ever attended school? (All observations in the sample have attended school, this variable can be removed)
#ILLNESC2 - Does Index Child have any chronic illness/disability?
#MISSEDA2 - IN THE PAST MONTH, HOW MANY DAYS HAS INDEX CHILD MISSED SCHOOL WHEN IT WAS IN SESSION?
#LIVTODA2 - TOTAL NUMBER OF CHILDREN MOTHER HAVE GIVEN BIRTH TO WHO ARE STILL ALIVE TODAY
#WITHSPO2 - IS MOTHER CURRENTLY LIVING WITH HUSBAND NOW?

count(MERGECHILDFILEFINAL,iqscore, wt = NULL, sort = FALSE)
min(MERGEMATHFINAL$SCORE2)
plot(mathreg)

#Plots
ggplot(MERGECHILDFILEFINAL, aes(x = TRAVHR2, y = iqscore)) + 
  geom_point() + ggtitle("Non-Verbal Intelligence") +
  ylab("Test Score") + xlab("Travel Time to School") +
  stat_smooth(method = "lm", col = "red")

ggplot(MERGEMATHFINAL, aes(x = TRAVHR2, y = SCORE2)) + 
  geom_point() + ggtitle("Mathematics") +
  ylab("Test Score") + xlab("Travel Time to School") +
  stat_smooth(method = "lm", col = "red")

ggplot(MERGEENGLFINAL, aes(x = TRAVHR2, y = SCORE2)) + 
  geom_point() + ggtitle("English") +
  ylab("Test Score") + xlab("Travel Time to School") +
  stat_smooth(method = "lm", col = "red")

ggplot(MERGECEBUFINAL, aes(x = TRAVHR2, y = SCORE2)) + 
  geom_point() + ggtitle("Cebuano Reading") +
  ylab("Test Score") + xlab("Travel Time to School") +
  stat_smooth(method = "lm", col = "red")



#part 2
#Create dataframe for the prediction
IQset<- MERGECHILDFILEFINAL %>% select(HHNUM942, iqscore, TRAVHR2, dicpel, dicprvte)
IQset<- na.omit(IQset)

#Split IQset into train and test sets
ind <- createDataPartition(y = IQset$iqscore, times = 1, p = 0.1, list = FALSE) 
train<- IQset[-ind,]
test<- IQset[ind,]

#Establish rmse function
RMSE <- function(iqscore, predicting){   sqrt(mean((iqscore-predicting)^2,na.rm=TRUE)) } 

rmse_tracker <- data_frame() 
mu<- mean(train$iqscore)


#Check the plot of beat_t and beta_u
beta_t<- train %>% group_by(TRAVHR2) %>% summarize(b_t= mean(iqscore-mu))
beta_t %>% qplot(b_t, geom= "histogram", bins= 20, data=., color= I("black"))

beta_u<- train %>% left_join(beta_t, by='TRAVHR2') %>% group_by(dicpel) %>%
  summarize(b_u = mean(iqscore - mu - b_t)) 
beta_u %>% qplot(b_u, geom ="histogram", bins = 30, data = ., color = I("black")) 

#Show rmse value when predicting using only the mean
rmse1 <- RMSE(test$iqscore, mu)
rmse_tracker <- data_frame(method = "Mean", RMSE = rmse1)  

#rmse value for prediciton using mean and beta_t
rmse2 <- test %>%  left_join(beta_t, by='TRAVHR2') %>% mutate(pred = mu + b_t)  
model1 <- RMSE(test$iqscore,rmse2$pred) 
rmse_tracker <- bind_rows(rmse_tracker, data_frame(method="Mean + Beta_t", RMSE = model1 )) 
rmse_tracker   

#rmse value for prediciton using mean and beta_t and beta_u
rmse3 <- test %>%  left_join(beta_t, by='TRAVHR2') %>% left_join(beta_u, by='dicpel') %>% 
  mutate(pred = mu + b_t + b_u)   
# update rmse results   
model2 <- RMSE(test$iqscore,rmse3$pred) 
rmse_tracker <- bind_rows(rmse_tracker,data_frame(method="Mean + b_t + b_u",  RMSE = model2)) 
rmse_tracker   

#choose optimal lambda
lambdas <- seq(0, 10, 1)  
rmses <- sapply(lambdas, function(l){    mu <- mean(train$iqscore)    
b_t <- train %>% group_by(TRAVHR2) %>%summarize(b_t = sum(iqscore - mu)/(n()+l))    
b_u <- train %>% left_join(b_t, by="TRAVHR2") %>% group_by(dicpel) %>% 
  summarize(b_u = sum(iqscore - b_t - mu)/(n()+l))    
predicting <- test %>% left_join(b_t, by = "TRAVHR2") %>% left_join(b_u, by = "dicpel") %>% 
  mutate(pred = mu + b_t + b_u) %>% .$pred   
return(RMSE(test$iqscore,predicting)) })  
#To show an rmse-lambda plot that will help us visualize what value of lambda will be optimal   
qplot(lambdas, rmses)     
lambda <- lambdas[which.min(rmses)]
lambda   


#Regularisation with mean beta_t and beta_u
regular_beta_t<- train %>% group_by(TRAVHR2) %>%  summarize(b_t = sum(iqscore - mu)/(n()+lambda), n_i = n())


regular_beta_u <- train %>% left_join(regular_beta_t, by='TRAVHR2') %>% group_by(dicpel) %>% 
  summarize(b_u = sum(iqscore - mu - b_t)/(n()+lambda), n_u = n()) 

regular_predicting <- test %>% left_join(regular_beta_t, by='TRAVHR2') %>% 
  left_join(regular_beta_u, by='dicpel') %>% mutate(pred = mu + b_t + b_u) %>%  
  .$pred

model3 <- RMSE(test$iqscore,regular_predicting) 
rmse_tracker <- bind_rows(rmse_tracker, data_frame(method="Beta_t and Beta_u (regularized)",  RMSE = model3 )) 
rmse_tracker 



lambdas <- seq(0, 15, 1)  
rmses <- sapply(lambdas, function(l){    
  mu <- mean(train$iqscore)    
  b_t <- test %>% group_by(TRAVHR2) %>% summarize(b_t = sum(iqscore - mu)/(n()+l))   
  b_u <- test %>% left_join(b_t, by="TRAVHR2") %>%group_by(dicpel) %>% 
    summarize(b_u = sum(iqscore - b_t - mu)/(n()+l))    
  b_r <- test %>% left_join(b_t, by='TRAVHR2') %>% left_join(b_u, by='dicpel') %>% group_by(dicprvte) %>% 
    summarize(b_r= sum(iqscore - mu - b_t -b_u)/(n()+lambda), n_r = n())    
  
  predicting <- test %>% left_join(b_t, by='TRAVHR2') %>% left_join(b_u, by='dicpel') %>% 
    left_join(b_r, by = 'dicprvte') %>% 
    mutate(pred = mu + b_t + b_u + b_r) %>%  .$pred   
  return(RMSE(test$iqscore,predicting)) })  
qplot(lambdas, rmses) 


lambda_opt <- lambdas[which.min(rmses)]
lambda_opt  

#Regularisation including beta_r
regular_beta_t2 <-test %>% group_by(TRAVHR2) %>%  
  summarize(b_t = sum(iqscore - mu)/(n()+lambda_opt), n_i = n())  

regular_beta_u2 <- test %>% left_join(regular_beta_t2, by='TRAVHR2') %>% group_by(dicpel) %>% 
  summarize(b_u = sum(iqscore - mu - b_t)/(n()+lambda_opt), n_u = n())  

regular_beta_r <- test %>% left_join(regular_beta_t2, by='TRAVHR2') %>% 
  left_join(regular_beta_u2, by='dicpel') %>% group_by(dicprvte) %>% 
  summarize(b_r = sum(iqscore - mu - b_t - b_u)/(n()+lambda_opt), n_y = n())  

predicting <- test %>% left_join(regular_beta_t2, by='TRAVHR2') %>% 
  left_join(regular_beta_u2, by='dicpel') %>% left_join(regular_beta_r, by = 'dicprvte') %>% 
  mutate(pred = mu + b_t + b_u + b_r) %>% .$pred    

model4 <- RMSE(test$iqscore,predicting) 
rmse_tracker <- bind_rows(rmse_tracker, data_frame(method="Beta_t, beta_u, beta_r (regularized)",  
                                                   RMSE = model4 )) 
rmse_tracker