library(tidyverse)
library(caret)
library(beepr)
library(mlbench)
library(MLmetrics)
# read in data and convert variables to factors, relevel
clean_props <- read_csv('data/PropsCleaned.csv') %>% 
  mutate(across(where(is_character), as_factor),
         across(c(TX_DIST, PT, SPT, N_GRP, LEA, IS_REMOD, BLTASSTORIES, HAS_ATTGAR,
                  HAS_DETGAR, HAS_BSMT, BSMT_ISFIN, KITCHENS), as_factor)) %>% 
  mutate(IMPQUALITY = relevel(IMPQUALITY, 'Below Average'),
         IMPCONDITIONTYPE = relevel(IMPCONDITIONTYPE, 'Average'))

# Some preprocessing to constrain numeric variables between 0 and 1
trans.01 <- preProcess(x=clean_props %>% select(-c(ACCOUNTNO, IMPROVED)), method='range',
                       rangeBounds = c(0,1))
clean_props <- predict(trans.01, newdata=clean_props) %>% 
  drop_na() %>% 
  filter(IMPROVED > 0)

### Variable Selection ###

# # define the control using a random forest selection function
# VScontrol <- rfeControl(functions=lmFuncs, method="cv", number=5)
# # run the RFE algorithm
# VSresults <- rfe(log(IMPROVED)~.,
#                data = clean_props %>% select(-ACCOUNTNO),
#                rfeControl=VScontrol,
#                sizes = c(5,10,15,20,30,40,50,70))
# beep()
# # summarize the results
# print(VSresults)
# # list the chosen features
# predictors(VSresults)
# # plot the results
# plot(VSresults, type=c("g", "o"))
# VSresults$fit
# select the variables deemed significant by our variable selection
# pred.props <- props.iv %>%
#   select(IMPROVED, ACCOUNTNO, BSMT_PCTCOMP, MLS_TOTAL_SQFT, TX_DIST.11, N_GRP.117,
#          ACTUAL_AGE, TOT_BATH, LEA.1060, BLTASSTORIES.2, ATTGAR, 
#          DETGAR, REMODPCT, BEDRMCNT)
  # mutate(BSMT_FIN = interaction(BSMT_ISFIN, BSMT_FIN),
  #        ATTGAR = interaction(HAS_ATTGAR, ATTGAR),
  #        DETGAR = interaction(HAS_DETGAR, DETGAR),
  #        REMODPCT = interaction(IS_REMOD, REMODPCT)) %>% 
  # select(-c(HAS_ATTGAR, HAS_DETGAR, BSMT_ISFIN, IS_REMOD))

#Create training set
props.train <- clean_props %>% sample_frac(.80)
#Create test set
props.test <- clean_props %>% anti_join(props.train, by = 'ACCOUNTNO')



# A helper function for testing how far off our predictions are
mape <- function(data, lev = NULL, model = NULL) {
  c(MAPE = MAPE(data$pred, data$obs))
}

# control <- trainControl(method="repeatedcv",
#                         number=5,
#                         repeats=3,
#                         summaryFunction = mape
#                         )
train_model <- function(alg = 'lm',
                        control = trainControl(method="repeatedcv",
                                                     number=5,
                                                     repeats=3,
                                                     summaryFunction = mape),
                        t.grid = expand.grid(n.trees = 150,
                                             interaction.depth = 5,
                                             shrinkage = .1,
                                             n.minobsinnode = 15
                        ))
  {
    model <- train(form=log(IMPROVED)~.,
                   data=props.train %>% select(-c(ACCOUNTNO)),
                   method=alg,
                   trControl=control
                   #, tuneGrid=t.grid
    )
    tibble(ACCOUNTNO=props.test$ACCOUNTNO,
                          Predicted=exp(predict(lin_mod, newdata=props.test %>%
                                                  select(-c(ACCOUNTNO,IMPROVED)))),
                          Actual=props.test$IMPROVED) %>% 
      write_csv("results.csv")
}
   

train_model(alg = 'lm')
results <- read_csv("results.csv")

# plot(model)
# model$bestTune
model$results



