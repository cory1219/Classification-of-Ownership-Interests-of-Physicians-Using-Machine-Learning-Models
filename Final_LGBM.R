library(tidyverse)
library(lubridate)
#library(summarytools) # for user-friendly html summaries of data
library(tidymodels)
library(magrittr)
library(reshape2)
library(batman)
library(Hmisc)
library(lightgbm)
#remotes::install_github("curso-r/treesnip")
library(treesnip)



options(dplyr.width = Inf, error=recover) # show all columns when printing to console
theme_set(theme_minimal()) # set ggplot theme for cleaner plotting
set.seed(2021) # in the AC, you'll be required to set a fixed random seed to make your work reproducible

#Preprocessing - basics for physicians
physicians <- read_csv("physicians.csv", col_types = cols(id = col_integer(), 
                                                          First_Name = col_skip(),
                                                          Middle_Name = col_skip(),
                                                          Last_Name = col_skip(),
                                                          Name_Suffix = col_skip(),
                                                          Zipcode = col_skip(),
                                                          Country = col_skip(),
                                                          Province =col_skip()))

physicians <- physicians %>% 
  separate(Primary_Specialty, into = c("Primary_Specialty_1", "Primary_Specialty_2", "Primary_Specialty_3"), sep = '\\|')
 

license <- dcast(melt(physicians,measure.vars = c("License_State_1", "License_State_2", "License_State_3", "License_State_4", "License_State_5"), na.rm = T), id ~ value, length)
license <- license[, !names(license) %in% c("DC", "FM", "GU", "PR")] #drop other than 50 states 
physicians<-merge(physicians, license)
physicians<- physicians[, !names(physicians) %in% c("License_State_1", "License_State_2", "License_State_3", "License_State_4", "License_State_5")]

physicians <- physicians %>% mutate(
  amount_licenses = AK+AL+AR+AZ+CA+CO+CT+DE+FL+GA+HI+IA+ID+IL+IN+KS+KY+LA+MA+MD+ME+MI+MN+MO+MS+MT+NC+ND+NE+NH+NJ+NM+NV+NY+OH+OK+OR+PA+RI+SC+SD+TN+TX+UT+VA+VT+WA+WI+WV+WY)


#############
#Preprocessing - basics for payment
company <- read_csv("companies.csv", col_types = cols(Company_ID = col_integer()))
payment <- read_csv("payments.csv", col_types = cols(Record_ID = col_integer(), 
                                                     Physician_ID = col_integer(),
                                                     Company_ID = col_integer(),
                                                     Number_of_Payments = col_integer(),
                                                     State_of_Travel = col_skip(),
                                                     Country_of_Travel = col_skip(),
                                                     Product_Category_1 = col_character(),
                                                     Product_Category_2 = col_character(),
                                                     Product_Category_3 = col_character()))
payment <- mutate(payment, Date = mdy(Date))
payment <- mutate(payment, across(.cols = c(Ownership_Indicator, Charity, Third_Party_Covered), .fns = to_logical))

payment <- payment %>% 
  mutate(Related_Product_Indicator = if_else(Related_Product_Indicator %in% "None", "No", Related_Product_Indicator))

table(payment$City_of_Travel)
payment <- mutate(payment, City_of_Travel = toupper(City_of_Travel))
table(payment$City_of_Travel)

payment <- payment %>% 
  mutate(Form_of_Payment_or_Transfer_of_Value = if_else(Form_of_Payment_or_Transfer_of_Value %in% c("Cash or cash equivalent", "In-kind items and services"), Form_of_Payment_or_Transfer_of_Value, "Any other ownership interest")) 

###########
#summary of basic preprocessing
summary(company)
glimpse(company)
summary(payment)
glimpse(payment)
summary(physicians)
glimpse(physicians)

###########

#payment %>% group_by(Physician_ID) %>% 
#  summarise(sum = sum(Total_Amount_of_Payment_USDollars)) %>% 
#  ggplot(aes(sum))+
#  geom_histogram()+
#  scale_x_log10()

#payment %>% group_by(Physician_ID) %>% 
#  summarise(mean = mean(Total_Amount_of_Payment_USDollars))%>% 
#  ggplot(aes(mean))+
#  geom_histogram()+
#  scale_x_log10()

###########
#Preprocessing-engineer variables of payment and move to physicians
company_list <- payment %>% 
  filter(Ownership_Indicator == T) %>% 
  group_by(Company_ID) %>% 
  summarise(count = n()) %>% 
  select(Company_ID) 

physicians <- payment %>% mutate(company = if_else(Company_ID %in% company_list$Company_ID, TRUE, FALSE)) %>%
  group_by(Physician_ID) %>% 
  summarise(company = sum(company, na.rm = T)) %>%   
  merge(physicians, by.x = "Physician_ID", by.y = "id")

#company_interest_unique
physicians <- payment %>% 
  mutate(company_interest_unique = if_else(Company_ID %in% company_list$Company_ID, TRUE, FALSE)) %>% 
  filter(company_interest_unique == TRUE) %>% 
  group_by(Physician_ID, Company_ID) %>% 
  summarise(company_interest_unique = sum(company_interest_unique, na.rm = T)) %>% 
  group_by(Physician_ID) %>% 
  summarise(company_interest_unique = n()) %>% 
  merge(physicians, all.y = T) %>% 
  mutate(company_interest_unique = if_else(is.na(company_interest_unique), 0, as.double(company_interest_unique)))

# Option 1: the amount of payment
physicians <- payment %>% group_by(Physician_ID) %>% 
  summarise(avg_payment = mean(Total_Amount_of_Payment_USDollars)) %>% 
  merge(physicians)

# Option 2: the amount of payment
physicians <- payment %>% mutate(group = as.numeric(cut2(payment$Total_Amount_of_Payment_USDollars, g=10))) %>% 
  group_by(Physician_ID) %>% 
  summarise(group = round(mean(group))) %>% 
  merge(physicians)


payment$difftime <- payment$Date - min(payment$Date)
physicians <- payment %>% group_by(Physician_ID) %>% 
  summarise(date = sum(difftime)) %>% 
  merge(physicians)

physicians <- payment %>% group_by(Physician_ID) %>% 
  summarise(Number_of_Payments = sum(Number_of_Payments)) %>% 
  merge(physicians)

physicians <- payment %>% dcast(...~Form_of_Payment_or_Transfer_of_Value, length) %>%
  group_by(Physician_ID) %>%
  summarise(In_kind_items_and_services = sum(`In-kind items and services`, na.rm = T), 
            Cash_or_cash_equivalent = sum(`Cash or cash equivalent`, na.rm = T), 
            Any_other_ownership_interest = sum(`Any other ownership interest`, na.rm = T)) %>% 
  merge(physicians)

physicians <- payment %>% dcast(...~Nature_of_Payment_or_Transfer_of_Value, length) %>%
  group_by(Physician_ID) %>%
  summarise(Food_and_Beverage = sum(`Food and Beverage`, na.rm = T), 
            Education = sum(Education, na.rm = T), 
            Consulting_Fee = sum(`Consulting Fee`, na.rm = T),
            Compensation_for_services = sum(`Compensation for services other than consulting, including serving as faculty or as a speaker at a venue other than a continuing education program`, na.rm = T),
            Gift = sum(Gift, na.rm = T),
            Entertainment = sum(Entertainment, na.rm = T),
            Travel_and_Lodging = sum(`Travel and Lodging`, na.rm = T),
            Compensation_for_noncertified = sum(`Compensation for serving as faculty or as a speaker for a non-accredited and noncertified continuing education program`, na.rm = T),
            Royalty_or_License = sum(`Royalty or License`, na.rm = T),
            Honoraria = sum(Honoraria, na.rm = T),
            Grant = sum(Grant, na.rm = T),
            Current_or_prospective_ownership = sum(`Current or prospective ownership or investment interest`, na.rm = T),
            Compensation_for_certified = sum(`Compensation for serving as faculty or as a speaker for an accredited or certified continuing education program`, na.rm = T),
            Charitable_Contribution = sum(`Charitable Contribution`, na.rm = T)) %>% 
  merge(physicians)

ownership_physician_id <- payment %>% filter(Ownership_Indicator == TRUE) %>% distinct(Physician_ID)
physicians <- physicians %>% 
  mutate(physician_ownership_interest = if_else(Physician_ID %in% ownership_physician_id$Physician_ID, TRUE, FALSE)) %>% 
  mutate(physician_ownership_interest = as.factor(physician_ownership_interest))

physicians <- payment %>% dcast(...~Third_Party_Recipient, length) %>%
  group_by(Physician_ID) %>%
  summarise(Entity = sum(Entity, na.rm = T), 
            Individual = sum(Individual, na.rm = T), 
            No_Third_Party_Payment = sum(`No Third Party Payment`, na.rm = T)) %>% 
  merge(physicians)

physicians <- payment %>% group_by(Physician_ID) %>%
  summarise(Charity = sum(Charity, na.rm = T)) %>% 
  merge(physicians)

text <- payment %>% 
  filter(Ownership_Indicator == T) %>% 
  group_by(Contextual_Information) %>% 
  summarise(count = n()) 
text <- text[!is.na(text$Contextual_Information),]

physicians <- payment %>% 
  mutate(Contextual_Information_log = if_else(Contextual_Information %in% text$Contextual_Information, TRUE, FALSE)) %>% 
  mutate(Contextual_Information_log = if_else(is.na(Contextual_Information), NA, Contextual_Information_log)) %>% 
  group_by(Physician_ID) %>% 
  summarise(Contextual_Information = sum(Contextual_Information_log, na.rm = T)) %>% 
  merge(physicians)

physicians <- payment %>% dcast(...~Related_Product_Indicator, length) %>%
  group_by(Physician_ID) %>%
  summarise(Combination = sum(Combination, na.rm = T), 
            Covered = sum(Covered, na.rm = T), 
            No = sum(No, na.rm = T),
            Non_Covered = sum(`Non-Covered`, na.rm = T),
            Yes = sum(Yes, na.rm = T)) %>% 
  merge(physicians)

#Option 1: Product_Type
physicians <- melt(payment, measure.vars = c("Product_Type_1", "Product_Type_2", "Product_Type_3"), na.rm = T) %>% 
  dcast(... ~ value, length) %>% 
  group_by(Physician_ID) %>% 
  summarise(Biological = sum(Biological, na.rm = T), 
            Device = sum(Device, na.rm = T), 
            Device_or_Medical_Supply = sum(`Device or Medical Supply`, na.rm = T),
            Drug = sum(Drug, na.rm = T),
            Drug_or_Biological = sum(`Drug or Biological`, na.rm = T),
            Medical_Supply = sum(`Medical Supply`, na.rm = T)) %>% 
  merge(physicians, all.y = T)

#Option 2: Product_Type
#payment <- payment %>% 
#  mutate(Product_Type_1 = if_else(Product_Type_1 == "Device" | Product_Type_1 == "Device or Medical Supply" | Product_Type_1 == "Medical Supply", 1, if_else(Product_Type_1 == "Drug" | Product_Type_1 == "Drug or Biological" | Product_Type_1 == "Biological", 2, 0)))
#payment %>% mutate(Product_Type_1 = as.numeric(Product_Type_1))
#payment$Product_Type_1 <- replace_na(payment$Product_Type_1,0)

#payment <- payment %>% 
#  mutate(Product_Type_2 = if_else(Product_Type_2 == "Device" | Product_Type_2 == "Device or Medical Supply" | Product_Type_2 == "Medical Supply", 1, if_else(Product_Type_2 == "Drug" | Product_Type_2 == "Drug or Biological" | Product_Type_2 == "Biological", 2, 0)))
#payment$Product_Type_2 <- replace_na(payment$Product_Type_2,0)

#payment <- payment %>% 
#  mutate(Product_Type_3 = if_else(Product_Type_3 == "Device" | Product_Type_3 == "Device or Medical Supply" | Product_Type_3 == "Medical Supply", 1, if_else(Product_Type_3 == "Drug" | Product_Type_3 == "Drug or Biological" | Product_Type_3 == "Biological", 2, 0)))
#payment$Product_Type_3 <- replace_na(payment$Product_Type_3,0)


#physicians <- physicians[-(46:95)]
physicians <- physicians %>% mutate(City = gsub(" ","_", City)) #Expression Unification


##########

train <- filter(physicians, set == "train")
test <- filter(physicians, set == "test")

###########


rec <- recipe(
  #specift predictors, target and data to enable code autocompletion
  physician_ownership_interest ~ ., data = train) %>% 
  # tell tidymodels that `id` is an ID and should not be used in any model
  update_role(Physician_ID, new_role = "ID") %>%
  step_mutate_at(Biological, Device, Device_or_Medical_Supply, Drug, Drug_or_Biological, Medical_Supply,
                 # this is a shorthand notation for (function(x) {replace_na(x,0)})
                 fn= ~replace_na(.,0)) %>%
  # determine what happens when a new nominal value is encountered in test data (which was missing from the trianing set)
  step_novel(all_nominal(), -has_role("ID"), -all_outcomes(), new_level="new") %>% 
  # impute all other nominal (character + factor) columns with the value "none"
  step_unknown(all_nominal(), -all_outcomes(), new_level = "none") %>%
  #dummy
  step_dummy(Primary_Specialty_1, Primary_Specialty_2, Primary_Specialty_3, State, one_hot = TRUE) %>% 
  # convert all strings to factors
  step_string2factor(all_nominal(), -all_outcomes(), -has_role("ID")) %>% 
  # combine low frequency factor levels
  step_other(all_nominal(), threshold = 0.01) %>%
  # remove no variance predictors which provide no predictive information 
  step_nzv(all_nominal()) %>%
  # remove constant columns
  step_zv(all_predictors()) %>% 
  prep()

proc_Train <- rec %>% bake(train)
proc_Test <- rec %>% bake(test)


#Method : Lightgbm

set.seed(2021)
folds_2 <- 
  proc_Train %>%  
  vfold_cv(v = 5, strata = physician_ownership_interest)
folds_2

lightgbm_model <- 
  boost_tree(
    trees = 1000,
    mtry = tune(),
    min_n = tune(),
    tree_depth = tune(),
    sample_size = tune(),
    learn_rate = tune(),
    loss_reduction = tune()
  ) %>%
  set_mode("classification") %>% 
  set_engine("lightgbm")

lightgbm_params <- parameters(
  finalize(mtry(), select(proc_Train,-physician_ownership_interest)),
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  learn_rate()
)

#lgbm_grid <- grid_max_entropy(
#  lightgbm_params,
#  size = 30
#)
#lgbm_grid
#change grid_random

lgbm_wf <-
  workflow() %>%
  add_model(lightgbm_model) %>%
  add_formula(physician_ownership_interest ~ .)

set.seed(2021)
lgbm_tuned <- tune_bayes(
  object = lgbm_wf,
  resamples = folds_2,
  param_info =lightgbm_params,
  iter = 30,
  metrics = metric_set(bal_accuracy),
  control = control_bayes(no_improve = 10, 
                          save_pred = T, verbose = T)
)

autoplot(lgbm_tuned)

lgbm_tuned %>%
  show_best(metric = "bal_accuracy" ,n = 10)

lgbm_best_params <-
  lgbm_tuned %>%
  select_best("bal_accuracy")

lgbm_model_final <-
  lightgbm_model%>%
  finalize_model(lgbm_best_params)

lightgbm_model
lgbm_model_final

set.seed(2021)
lgbmTrainFit <- lgbm_model_final %>% fit(physician_ownership_interest~., data=proc_Train)
lgbmTrainFit

set.seed(2021)
train_prediction <- lgbm_model_final %>%
  # fit the model on all the training data
  fit(
    formula = physician_ownership_interest ~ ., 
    data    = proc_Train
  ) %>%
  # predict the sale prices for the training data
  predict(new_data = proc_Train) %>%
  bind_cols(train)

lgbm_score_train <- 
  train_prediction %>%
  metrics(physician_ownership_interest, .pred_class) %>%
  mutate(.estimate = format(round(.estimate, 2), big.mark = ","))
knitr::kable(lgbm_score_train)


test_prediction <-
  lgbm_model_final %>%
  # fit the model on all the training data
  fit(
    formula = physician_ownership_interest ~ ., 
    data    = proc_Train
  ) %>%
  # predict the sale prices for the training data
  predict(new_data = proc_Test) %>%
  bind_cols(test)

test_prediction %>%
  yardstick::metrics(physician_ownership_interest, .pred_class) %>%
  mutate(.estimate = format(round(.estimate, 2), big.mark = ",")) %>%
  knitr::kable()

select(test_prediction, Physician_ID, .pred_class) %>% 
  mutate(.pred_class = if_else(.pred_class == TRUE, 1, 0)) %>% 
  write_csv("prediction.csv")

