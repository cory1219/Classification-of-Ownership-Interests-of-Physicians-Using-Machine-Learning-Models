library(tidyverse)
library(lubridate)
library(summarytools) # for user-friendly html summaries of data
library(ggmap) # for plotting data on a map
library(tidymodels)
library(magrittr)
library(reshape2)
install.packages("batman")
library(batman)


options(dplyr.width = Inf) # show all columns when printing to console
theme_set(theme_minimal()) # set ggplot theme for cleaner plotting
set.seed(2021) # in the AC, you'll be required to set a fixed random seed to make your work reproducible

physicians <- read_csv("~/Desktop/Cory/TUM Master/Business Analytics/Analytics Cup/Analytics Cup V1/training_data_v2_L8PXAxZ/physicians.csv", col_types = cols(id = col_integer(), 
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
#############3

company <- read_csv("~/Desktop/Cory/TUM Master/Business Analytics/Analytics Cup/Analytics Cup V1/training_data_v2_L8PXAxZ/companies.csv", col_types = cols(Company_ID = col_integer()))
payment <- read_csv("~/Desktop/Cory/TUM Master/Business Analytics/Analytics Cup/Analytics Cup V1/training_data_v2_L8PXAxZ/payments.csv", col_types = cols(Record_ID = col_integer(), 
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
### to melt product name or not
payment <- payment %>% 
  mutate(Related_Product_Indicator = if_else(Related_Product_Indicator %in% "None", "No", Related_Product_Indicator))

table(payment$City_of_Travel)
payment <- mutate(payment, City_of_Travel = toupper(City_of_Travel))
table(payment$City_of_Travel)

payment <- payment %>% 
  mutate(Form_of_Payment_or_Transfer_of_Value = if_else(Form_of_Payment_or_Transfer_of_Value %in% c("Cash or cash equivalent", "In-kind items and services"), Form_of_Payment_or_Transfer_of_Value, "Any other ownership interest")) 

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
payment$Company_ID <- as.integer(payment$Company_ID)

company_list <- payment %>% 
  filter(Ownership_Indicator == T) %>% 
  group_by(Company_ID) %>% 
  summarize(count = n()) %>% 
  select(Company_ID) 

physicians <- payment %>% mutate(company = if_else(Company_ID %in% company_list$Company_ID, TRUE, FALSE)) %>%
  group_by(Physician_ID) %>% 
  summarise(company = sum(company, na.rm = T)) %>%   
  merge(physicians, by.x = "Physician_ID", by.y = "id")

physicians <- payment %>% group_by(Physician_ID) %>% 
  summarise(avg_payment = mean(Total_Amount_of_Payment_USDollars)) %>% 
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

payment_physician_interest <- payment %>% group_by(Physician_ID) %>% 
  summarize(n = n(), sum = sum(Ownership_Indicator)) %>% 
  filter(sum > 0)
physicians <- physicians %>% 
  mutate(physician_ownership_interest = if_else(Physician_ID %in% payment_physician_interest$Physician_ID, TRUE, FALSE))

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



       
##########

train <- filter(physicians, set == "train")
test <- filter(physicians, set == "test")
###########


rec <- recipe(
  #specift predictors, target and data to enable code autocompletion
  physician_ownership_interest ~ ., data = train) %>% 
  # tell tidymodels that `id` is an ID and should not be used in any model
  update_role(Physician_ID, new_role = "ID") %>%
  step_dummy(Primary_Specialty_1, Primary_Specialty_2, Primary_Specialty_3, one_hot = TRUE) %>% 
  step_mutate_at(Biological, Device, Device_or_Medical_Supply, Drug, Drug_or_Biological, Medical_Supply,
                 # this is a shorthand notation for (function(x) {replace_na(x,0)})
                 fn= ~replace_na(.,0)) %>%
  # determine what happens when a new nominal value is encountered in test data (which was missing from the trianing set)
  step_novel(all_nominal(), -has_role("ID"), new_level="new") %>% 
  # impute all other nominal (character + factor) columns with the value "none"
  step_unknown(all_nominal(), new_level = "none") %>%
  # convert all strings to factors
  step_string2factor(all_nominal(), -all_outcomes(), -has_role("ID")) %>% 
  # remove constant columns
  step_zv(all_predictors()) 
  

rec %>% prep() %>% bake(test)

folds <- vfold_cv(train, v = 5)

training_workflow <- 
  # start with a blank workflow() object
  workflow() %>% 
  # add our preprocessing recipe
  add_recipe(rec) %>% 
  # add our model specification
  add_model(model)

training_workflow

cv_fits <- 
  training_workflow %>% 
  fit_resamples(folds,
                metrics = metric_set(yardstick::rmse, yardstick::mae)
  )


company %>% dfSummary %>% view(method = 'browser')
physicians %>% dfSummary %>% view(method = 'browser')
payment %>% dfSummary %>% view(method = 'browser')
train %>% dfSummary %>% view(method = 'browser')
test %>% dfSummary %>% view(method = 'browser')

# max expense outlier
# standardize?? 
#Hyperparameter ??

sum(grepl("ownership", payment$Contextual_Information))
sum(grepl("ownership interest", payment$Contextual_Information))
sum(grepl("interest", payment$Contextual_Information))
payment[grepl("ownership interest", payment$Contextual_Information),] %>%
  filter(Ownership_Indicator == T) %>% 
  summarise(n())
payment <- payment %>% 
  mutate(matchedWord_OI = if_else(grepl("ownership interest", Contextual_Information), TRUE, FALSE))






