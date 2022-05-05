
# Inspired by 
# https://www.r-bloggers.com/2022/02/beginners-guide-to-machine-learning-in-r-with-step-by-step-tutorial/


# Obviously works best for tidy datasets where each row = an observation. But not
# useless even here. 

# Trying again for Creative works.

#----- HEART DISEASE PREDICTION
#
# dataset where target 1 or 0 means they got heart disease or not. 
#
library(tidyverse)
library(caret)
library(party)


dat <- read.csv("practice_data/heart.csv")
names(dat)[[1]] <- "age"

dat$target <- dplyr::recode(dat$target, `0` = 1L, `1` = 0L)

#clean the data and check for duplicates or missing values.

sapply(dat, function(x) table(is.na(x)))
# -> no missing values
table(duplicated(dat))
# -> one duplicated row which the next line removes
dat <- dat[!duplicated(dat),]

## Step 2: Visual inspection / descriptive statistics
# The are univariate distributions (one variable at a time)
dat %>% gather() %>%
        ggplot(aes(x=value)) + 
        geom_histogram(fill="steelblue", alpha=.7) +
        theme_minimal() +
        facet_wrap(~key, scales="free")

# plotting a correlation matrix, in order to a) check if we have features 
# that are highly correlated (which is problematic for some algorithms), and 
# b) get a first feeling about which features are correlated with the target 
# (heart disease) and which are not.

cormat <- cor(dat %>% keep(is.numeric))

cormat %>% as.data.frame %>% mutate(var2=rownames(.)) %>%
        pivot_longer(!var2, values_to = "value") %>%
        ggplot(aes(x=name,y=var2,fill=abs(value),label=round(value,2))) +
        geom_tile() + geom_label() + xlab("") + ylab("") +
        ggtitle("Correlation matrix of our predictors") +
        labs(fill="Correlation\n(absolute):")

# No problematically strong correlations between any of hte predictors. 
#
# Box plots to find bivariate relations between predictors and outcome.
# select only columns with numeric values, haven't yet converted categorial
# variables into factors. 
dat %>% select(-c(sex,cp,ca,thal,restecg,slope,exang,fbs)) %>%
        pivot_longer(!target, values_to = "value") %>%
        ggplot(aes(x=factor(target), y=value, fill=factor(target))) +
        geom_boxplot(outlier.shape = NA) + geom_jitter(size=.7, width=.1, alpha=.5) +
        scale_fill_manual(values=c("steelblue", "orangered1")) +
        labs(fill="Heart disease:") +
        theme_minimal() +
        facet_wrap(~name, scales="free")

# Use stacked barplots for the categorical data:
# 

dat %>% select(sex,cp,ca,thal,restecg,slope,exang,fbs,target) %>% 
        pivot_longer(!target, values_to = "value") %>%
        ggplot(aes(x=factor(value), fill=factor(target))) +
        scale_fill_manual(values=c("steelblue", "orangered1")) +
        geom_bar(position="fill", alpha=.7)+
        theme_minimal() +
        labs(fill="Heart disease:") +
        facet_wrap(~name, scales="free")

pred <- as.factor(ifelse(dat$sex==1,1,0))
confusionMatrix(pred,as.factor(dat$target))


#-----
#
#
preprocess_data <- function(df){
        
        #ROutliers are assigned the modal value
        df <- df %>% mutate(restecg = recode(restecg, `2`=1L),
                            thal = recode(thal, `0`=2L),
                            ca = recode(ca, `4`=0L))
        
        #Nominal variables
        nomvars <- c("cp", "ca", "thal", "restecg", "slope")
        df[,names(df) %in% nomvars] <- sapply(df[,names(df) %in% nomvars], as.character)
        dummies <- dummyVars(~ ., df)
        df <- predict(dummies, newdata = df) %>% as.data.frame
        
        #Age-standardized variables
        df$hr_age <- df$thalach / df$age
        df$chol_age <- df$chol / df$age
        
        
        #Oldpeak: Is there any ST depression
        df$st <- ifelse(df$oldpeak>0,1,0)
        
        return(df[,names(df)!="target"])
}

x_train <- preprocess_data(train)
x_test <- preprocess_data(test)

y_train <- factor(train[,"target"], levels=c(1,0))
y_test <- factor(test[,"target"], levels=c(1,0))
