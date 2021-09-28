setwd("C:/Users/Daniel Gutierrez/Desktop/R Practice/House Prices")
houseprices <- readr::read_csv("train.csv")
summary(houseprices)

library(tidyverse)
library(tidymodels)
library(caret)
library(corrplot)
library(gridExtra)
library(ggcorrplot)
library(mgcv)
library(xgboost)
library(Metrics)
theme_set(theme_bw())


## Identifying categories in indicator variables

house <- houseprices %>% 
  mutate(
         MSZoning = as.factor(MSZoning),
         Street = as.factor(Street),
         Alley = as.factor(Alley),
         LotShape = as.factor(LotShape),
         LandContour = as.factor(LandContour),
         Utilities = as.factor(Utilities),
         LotConfig = as.factor(LotConfig),
         LandSlope = as.factor(LandSlope),
         Neighborhood = as.factor(Neighborhood),
         Condition1 = as.factor(Condition1),
         Condition2 = as.factor(Condition2),
         BldgType = as.factor(BldgType),
         HouseStyle = as.factor(HouseStyle),
         OverallQual = as.factor(OverallQual),
         OverallCond = as.factor(OverallCond),
         RoofStyle = as.factor(RoofStyle),
         RoofMatl = as.factor(RoofMatl),
         Exterior1st = as.factor(Exterior1st),
         Exterior2nd = as.factor(Exterior2nd),
         MasVnrType = as.factor(MasVnrType),
         ExterQual = as.factor(ExterQual),
         ExterCond = as.factor(ExterCond),
         Foundation = as.factor(Foundation),
         BsmtQual = as.factor(BsmtQual),
         BsmtCond = as.factor(BsmtCond),
         BsmtExposure = as.factor(BsmtExposure),
         BsmtFinType1 = as.factor(BsmtFinType1),
         BsmtFinType2 = as.factor(BsmtFinType2),
         Heating = as.factor(Heating),
         HeatingQC = as.factor(HeatingQC),
         CentralAir = as.factor(CentralAir),
         Electrical = as.factor(Electrical),
         KitchenQual = as.factor(KitchenQual),
         Functional = as.factor(Functional),
         FireplaceQu = as.factor(FireplaceQu),
         GarageType = as.factor(GarageType),
         GarageFinish = as.factor(GarageFinish),
         GarageQual = as.factor(GarageQual),
         GarageCond = as.factor(GarageCond),
         PavedDrive = as.factor(PavedDrive),
         PoolQC = as.factor(PoolQC),
         Fence = as.factor(Fence),
         MiscFeature = as.factor(MiscFeature),
         MoSold = as.factor(MoSold),
         YrSold = as.factor(YrSold),
         SaleType = as.factor(SaleType),
         SaleCondition = as.factor(SaleCondition))

## For LotFrontage we can substitute the NA's for the mean value

house$LotFrontage[is.na(house$LotFrontage)] <- round(mean(house$LotFrontage, na.rm=TRUE))
summary(house$LotFrontage)

## For some variables the NA means None

na_to_none <- function(variables) {
  variable_char <- as.character(variables)
  variable_char[is.na(variable_char)] <- "None"
  variable_fac <- as.factor(variable_char)
  
  return(variable_fac)
}

factor_list <- c("Alley","BsmtQual", "BsmtCond", "BsmtExposure", "BsmtFinType1", 
                 "BsmtFinType2", "Electrical", "FireplaceQu",
                 "GarageType","GarageFinish","GarageQual","GarageCond","PoolQC",
                 "Fence","MiscFeature")

for (i in factor_list) {
  house[[i]] <- na_to_none(house[[i]])
}
summary(house)


colnames(house)[colSums(is.na(house)) > 0]

## For GarageYrBlt I assume a NA means No garage but we can visualize it 
garage <- house %>% 
  select(GarageType, GarageYrBlt)
  
DT::datatable(filter(garage, garage$GarageType == "None"))
str(house$GarageYrBlt)

house$GarageYrBlt[is.na(house$GarageYrBlt)] <- 0

## For MasVnrType we can do substitute the Na's by nones and for the area by 0's
house$MasVnrType[is.na(house$MasVnrType)] <- "None"
summary(house$MasVnrType)

house$MasVnrArea[is.na(house$MasVnrArea)] <- 0

## Now the dataset has no more NA's. We can proceed to EDA.

hist(house$SalePrice, breaks = 200) # There are some houses that were sold for more than $650,000
sum(house$SalePrice > 650000) # Two houses. We can drop these outliers
boxplot(house$SalePrice)

hist(house$LotArea, breaks = 100) # Are these big houses the expensive ones?
DT::datatable(filter(house, house$SalePrice > 650000)) # Yes, they are.


sl1 <- house %>% 
  ggplot(aes(LotArea, SalePrice))+
  geom_point(alpha = 0.3) +
  geom_smooth(method = "gam")+
  labs(subtitle = "Complete sample")

sl2 <- house %>% 
  filter(SalePrice < 650000 & LotArea < 20000) %>% 
  ggplot(aes(LotArea, SalePrice))+
  geom_point(alpha = 0.3) +
  geom_smooth(method = "gam")+
  labs(subtitle = "Subsetting with price < $650,000 and lot area < 20000 sqrft")
  
grid.arrange(sl1, sl2)

## If we consider the complete sample, the model will not capture the effects correctly.
## As we can see, the data is more concentrated to the left. In this subsample, the effects
## are non-linear. It is an important feature to consider when fitting a regression.

# Lets investigate the effects of different zones. 
plot(house$MSZoning) ## This seems like a problem
table(house$MSZoning) ## C and RH are less than 20 observations. There is not enough data for the zones.

## Area
zone <- house %>% 
  ggplot(aes(SalePrice, fill = MSZoning)) +
  geom_boxplot(outlier.alpha =0.5) +
  coord_flip() +
  labs(subtitle = "Zone")

neighborhood <- plot(house$Neighborhood)
neighborhood <- house %>% 
  ggplot(aes(SalePrice, fill = Neighborhood)) +
  geom_boxplot(outlier.alpha =0.5) +
  coord_flip() +
  labs(subtitle = "Neighborhood")

grid.arrange(zone, neighborhood)
## Both seem to affect the saleprice

## Features of the house
msssubclass <- house %>% 
  ggplot(aes(SalePrice, fill = MSSubClass)) +
  geom_boxplot(outlier.alpha =0.5) +
  coord_flip() +
  labs(subtitle = "Type of dwelling involved in the sale")

## Houses built in 1946 and newer(Groups: 20, 60, 120) were sold at a higher price than older.
## Among those, 2-story houses(60) are the more expensive. 

lotshape <- house %>% 
  ggplot(aes(SalePrice, fill = LotShape)) +
  geom_boxplot(outlier.alpha =0.5) +
  coord_flip() +
  labs(subtitle = "Lot Shape")

condition1 <- house %>% 
  ggplot(aes(SalePrice, fill = Condition1)) +
  geom_boxplot(outlier.alpha =0.5) +
  coord_flip() +
  labs(subtitle = "Condition 1")
condition2 <- house %>% 
  ggplot(aes(SalePrice, fill = Condition2)) +
  geom_boxplot(outlier.alpha =0.5) +
  coord_flip() +
  labs(subtitle = "Condition 2")


housestyle <- house %>% 
  ggplot(aes(SalePrice, fill = HouseStyle)) +
  geom_boxplot(outlier.alpha =0.5) +
  coord_flip() +
  labs(subtitle = "Style of dwelling")

grid.arrange(lotshape, condition1, condition2, housestyle) # Graph features
## Lot Shape seems to not affect the saleprice

overallqual <- house %>% 
  ggplot(aes(SalePrice, fill = OverallQual)) +
  geom_boxplot(outlier.alpha =0.5) +
  coord_flip() +
  labs(subtitle = "Rates the overall material and finish of the house")
  

overallcond <- house %>% 
  ggplot(aes(SalePrice, fill = OverallCond)) +
  geom_boxplot(outlier.alpha =0.5) +
  coord_flip() +
  labs(subtitle = "Rates the overall condition of the house")

extercond <- house %>% 
  ggplot(aes(SalePrice, fill = ExterCond)) +
  geom_boxplot(outlier.alpha =0.5) +
  coord_flip() +
  labs(subtitle = "Evaluates the present condition of the material on the exterior")

grid.arrange(overallqual, overallcond, extercond) # Condition of the house
#The median values of ExterCond seem equal.

baths <- house %>% 
  ggplot(aes(SalePrice, fill = as.factor(Baths))) +
  geom_boxplot(outlier.alpha =0.5) +
  coord_flip() +
  labs(subtitle = "Number of full bathrooms")

bedroom <- house %>% 
  ggplot(aes(SalePrice, fill = as.factor(BedroomAbvGr))) +
  geom_boxplot(outlier.alpha =0.5) +
  coord_flip()+
  labs(subtitle = "Bedrooms above grade")
  
garagety <- house %>% 
  ggplot(aes(SalePrice, fill = GarageType)) +
  geom_boxplot(outlier.alpha =0.5) +
  coord_flip()+
  labs(subtitle = "Garage type")

pool <- house %>% 
  ggplot(aes(SalePrice, fill = PoolQC)) +
  geom_boxplot(outlier.alpha =0.5) +
  coord_flip() +
  labs(subtitle = "Pool quality")

grid.arrange(baths, bedroom, garagety, pool) #Features 2 in the house

yrsold <- house %>% 
  ggplot(aes(SalePrice, fill = YrSold)) +
  geom_boxplot(outlier.alpha =0.5) +
  coord_flip() 
  
mosold <- house %>% 
  ggplot(aes(SalePrice, fill = MoSold)) +
  geom_boxplot(outlier.alpha =0.5) +
  coord_flip()

salecond <- house %>% 
  ggplot(aes(SalePrice, fill = SaleCondition)) +
  geom_boxplot(outlier.alpha =0.5) +
  coord_flip() +
  labs(subtitle = "Condition of sale")

house %>% 
  ggplot(aes(SalePrice, fill = Utilities)) +
  geom_boxplot(outlier.alpha =0.5) +
  coord_flip() +
  labs(subtitle = "Condition of sale")

grid.arrange(yrsold, mosold, salecond) # Conditions at the moment of sale
## All matter, special attention to Baths.

heating <- house %>% 
  ggplot(aes(SalePrice, fill = Heating)) +
  geom_boxplot(outlier.alpha =0.5) +
  coord_flip() +
  labs(subtitle = "Heating")

heatingQual <- house %>% 
  ggplot(aes(SalePrice, fill = HeatingQC)) +
  geom_boxplot(outlier.alpha =0.5) +
  coord_flip() +
  labs(subtitle = "Heating Quality")

centralair <- house %>% 
  ggplot(aes(SalePrice, fill = CentralAir)) +
  geom_boxplot(outlier.alpha =0.5) +
  coord_flip() +
  labs(subtitle = "Central Air")

electricalsystem <- house %>% 
  ggplot(aes(SalePrice, fill = Electrical)) +
  geom_boxplot(outlier.alpha =0.5) +
  coord_flip() +
  labs(subtitle = "Electrical system")

foundation <- house %>% 
  ggplot(aes(SalePrice, fill = Foundation)) +
  geom_boxplot(outlier.alpha =0.5) +
  coord_flip() +
  labs(subtitle = "Foundation")

landcontour <- house %>% 
  ggplot(aes(SalePrice, fill = LandContour)) +
  geom_boxplot(outlier.alpha =0.5) +
  coord_flip() +
  labs(subtitle = "Land Contour")

garagecars <- house %>% 
  ggplot(aes(SalePrice, fill = as.factor(GarageCars))) +
  geom_boxplot(outlier.alpha =0.5) +
  coord_flip() +
  labs(subtitle = "Garage Cars")

fence <- house %>% 
  ggplot(aes(SalePrice, fill = Fence)) +
  geom_boxplot(outlier.alpha =0.5) +
  coord_flip() +
  labs(subtitle = "Fence")

miscfeature <- house %>% 
  ggplot(aes(SalePrice, fill = MiscFeature)) +
  geom_boxplot(outlier.alpha =0.5) +
  coord_flip() +
  labs(subtitle = "Miscellaneous feature not covered in other categories")

grid.arrange(heating, heatingQual, centralair, electricalsystem, foundation, landcontour, garagecars,
             fence, miscfeature)

house %>% 
  filter(SalePrice < 650000 & LotArea < 20000) %>% 
  ggplot(aes(SalePrice,YearBuilt)) +
  geom_point() +
  geom_smooth(method = "gam")+
  coord_flip()

house %>% 
  ggplot(aes(x = reorder (Neighborhood, SalePrice, FUN = median), SalePrice, fill = Neighborhood))+
        geom_bar(alpha = 0.5, stat = "summary")+
        geom_hline(yintercept = 163000, linetype="dashed") +
  labs(x = "Median Sale Price", y = "Sale Price") +
  coord_flip()

# Plotting the correlation of SalePrice with the numerical variables

house$stFlrSF <- house$`1stFlrSF`
house$ndFlrSF <- house$`2ndFlrSF`
house$SsnPorch <- house$`3SsnPorch`

num_house <- house %>% 
  select(LotFrontage,  LotArea,  YearBuilt,  YearRemodAdd,  MasVnrArea,  BsmtFinSF1, BsmtFinSF2, BsmtUnfSF,  TotalBsmtSF,  stFlrSF,  ndFlrSF,  LowQualFinSF,  GrLivArea,  BsmtFullBath,  BsmtHalfBath, FullBath,  HalfBath,  BedroomAbvGr,  KitchenAbvGr,  TotRmsAbvGrd,  Fireplaces,  GarageYrBlt,  GarageCars,  GarageArea,  WoodDeckSF,  OpenPorchSF,  EnclosedPorch, SsnPorch,  ScreenPorch,  PoolArea,  MiscVal)
saleprice <- house$SalePrice
num_house <- as.data.frame(num_house)

corrhouse <- cor(saleprice, num_house)
corrhouse <- as.data.frame(corrhouse)

ggcorrplot(as.data.frame(as.matrix(corrhouse)), lab =TRUE, lab_size = 2.5,
           title = "Correlation Sale Price vs other numerical")

# Only Garage Area, Garage Cars, Fireplaces, TotRmsAbvGr, Full Bath, GrLiveArea, stFlrSf, TotalBsmtSf
# And maybe YearRemodAdd and YearBuilt might be considered for analysis7
# We could also add  all Halfbath and Bath together to get a better perspective of bathrooms.

house <- house %>% 
  mutate(Baths = 0.5*(HalfBath) + FullBath + 0.5*(BsmtHalfBath) + BsmtFullBath)

cor(house$SalePrice, house$Baths)
# Baths look good.

## Adding variables that seems to affect SalePrice

house$Remod <- ifelse(house$YearBuilt == house$YearRemodAdd, 0, 1)
house$Age <- as.numeric(house$YrSold) - house$YearRemodAdd
cor(house$Age, house$SalePrice)

house$New <- ifelse(house$YearBuilt == house$YrSold, 0, 1)
table(house$New)
cor(house$New, house$SalePrice)
house$Rich <- 0
house$Rich[house$Neighborhood %in% c("NridgHt", "NoRidge", "StoneBr")] <- 4
house$Rich[house$Neighborhood %in% c("Timber", "Somerst", "Veenker",
                                                 "Crawfor", "ClearCr", "CollgCr",
                                                 "Blmngtn", "NWAmes", "Gilbert",
                                                 "SawyerW")] <- 3
house$Rich[house$Neighborhood %in% c("Mitchel", "NPkVill", "NAmes",
                                                 "SWISU", "Blueste", "Sawyer",
                                                 "BrkSide", "Edwards", "OldTown")] <- 2
house$Rich[house$Neighborhood %in% c("BrDale", "IDOTRR", "MeadowV")] <- 1

### Here I created new variable for total porch area and then added it to the TotalSq
house$TotalPorchSq <- house$OpenPorchSF + house$EnclosedPorch + house$`3SsnPorch` + house$ScreenPorch

house$TotalSq <- house$GrLivArea + house$TotalBsmtSF + house$GarageArea + house$TotalPorchSq +
  house$MasVnrArea + house$`1stFlrSF` + house$`2ndFlrSF`
cor(house$TotalSq, house$SalePrice)

### Yes or No variables for each feature
house$Pool <- ifelse(house$PoolArea > 0, 1, 0)
house$FenceBinary <- ifelse(house$Fence == "None", 0, 1)
house$Fireplace <- ifelse(house$Fireplaces > 0, 1, 0)

### We also have to think about MSSubClass as it was out of the analysis because
### in the test data set it includes a new category for 150. 
### Also, analyzing the data LotFrontage is important but there are two outliers > 300
### KitchenQual seems to affect but KitchenAbvGr is strange
### RoofMtl also seem to affect but there are few observations for some categories
### HouseStyle does matter and GarageType too

house_ready <- house %>% 
  filter(LotFrontage < 300) %>% 
  select(-LotShape, -PoolArea, -ScreenPorch, - SsnPorch, -EnclosedPorch, -OpenPorchSF,
         -ndFlrSF,-BsmtUnfSF, -BsmtFinSF2, -HalfBath, -FullBath, -BsmtHalfBath, -BsmtFullBath,
         -`1stFlrSF`, `2ndFlrSF`,  - `3SsnPorch`, -Fence, -Fireplaces, -FireplaceQu, -Alley,
         -ExterQual, -ExterCond, -Foundation, -Heating, -MiscFeature,-MoSold, -SaleType,
         -Street, -LandContour, -RoofStyle, -Condition2,
         -RoofMatl, -PoolQC, -GarageQual, -SaleCondition, -GarageCond, -GarageFinish, -Electrical
         ,-BsmtFinType2, -BsmtFinType1, -PavedDrive, -Functional,
         -BldgType, -BsmtExposure, -GrLivArea, -TotalBsmtSF, -YearBuilt, -YearRemodAdd
         , -Neighborhood, -HeatingQC, -BsmtCond, -New, -GarageArea) %>% 
  mutate(SalePrice = log(SalePrice),
         YrSold = as.factor(YrSold), Rich = as.factor(Rich),
         Remod = as.factor(Remod))

str(house_ready)

linear1 <- lm(SalePrice~. - Id, data = house_ready)
summary(linear1)

house_ready %>% 
  ggplot(aes(predict, SalePrice)) +
  geom_point()

# The model looks good using only data that it has seen. Now lets try splitting the data set

set.seed(1234)
house_split <- house_ready %>% 
  initial_split()
house_train <- training(house_split)
house_test <- testing(house_split)

linear2 <- lm(SalePrice~. - Id - predict, data = house_train)

predict_trainlm <- predict(linear2)
house_train %>% 
  ggplot(aes(predict, SalePrice)) +
  geom_point() +
  geom_smooth()

# Still good
# RMSE?
rmse <- function(actual,predicted) {
  error = actual - predicted
  se = error^2
  mse = mean(se)
  rmse = sqrt(mse)
  return(rmse)
}


rmse(house_train$SalePrice, house_train$predict)

predict_testlm <- predict(linear2, newdata = house_test)
house_test %>% 
  ggplot(aes(predict, SalePrice)) +
  geom_point() +
  geom_smooth()

rmse(house_test$SalePrice, house_test$predict)

## Now, as we will use tree-based models we can take advantage of the algorithm feature
## To handle the missing values in the data. For this, we need the dataset without imputing missing
## values in the numerical variables. We have to set -1 as a numeric flag so the model can identify it 
## as NA's.

houseprices <- read.csv("train.csv", stringsAsFactors = FALSE) ### Original train data set
test_houseprices <- read.csv("test.csv", stringsAsFactors = FALSE) #### original test data set

SalePrice <- houseprices$SalePrice 
houseprices$SalePrice <- NULL

full_data <- rbind(houseprices,test_houseprices)

for(col in  1:80){
  if(class(full_data[,col])=="character"){
    new_col = full_data[,col]
    new_col[which(is.na(new_col),T)]="None"
    full_data[col] =  as.factor(new_col)
    
  }
}


#now the levels of train & test datasets will be same
# Separate out our train and test sets

houseprices <- full_data[1:nrow(houseprices),]
houseprices$SalePrice <- SalePrice  
test_houseprices <- full_data[(nrow(houseprices)+1):nrow(full_data),]

houseprices[is.na(houseprices)] <- -1 ### Numeric flag

### Now we have to create the new features for this data set too.

houseprices <- houseprices %>% 
  mutate(Baths = 0.5*(HalfBath) + FullBath + 0.5*(BsmtHalfBath) + BsmtFullBath)

houseprices$Remod <- ifelse(houseprices$YearBuilt == houseprices$YearRemodAdd, 0, 1)

houseprices$Age <- as.numeric(houseprices$YrSold) - houseprices$YearRemodAdd

houseprices$New <- ifelse(houseprices$YearBuilt == houseprices$YrSold, 0, 1)

houseprices$Rich <- 0
houseprices$Rich[houseprices$Neighborhood %in% c("NridgHt", "NoRidge", "StoneBr")] <- 4
houseprices$Rich[houseprices$Neighborhood %in% c("Timber", "Somerst", "Veenker",
                                     "Crawfor", "ClearCr", "CollgCr",
                                     "Blmngtn", "NWAmes", "Gilbert",
                                     "SawyerW")] <- 3
houseprices$Rich[houseprices$Neighborhood %in% c("Mitchel", "NPkVill", "NAmes",
                                     "SWISU", "Blueste", "Sawyer",
                                     "BrkSide", "Edwards", "OldTown")] <- 2
houseprices$Rich[houseprices$Neighborhood %in% c("BrDale", "IDOTRR", "MeadowV")] <- 1

houseprices$TotalPorchSq <- houseprices$OpenPorchSF + houseprices$EnclosedPorch + houseprices$X3SsnPorch + houseprices$ScreenPorch

houseprices$TotalSq <- houseprices$GrLivArea + houseprices$TotalBsmtSF + houseprices$X1stFlrSF + houseprices$GarageArea + houseprices$TotalPorchSq + houseprices$X2ndFlrSF

### Yes or No variables for each feature
houseprices$Pool <- ifelse(houseprices$PoolArea > 0, 1, 0)
houseprices$FenceBinary <- ifelse(houseprices$Fence == "None", 0, 1)
houseprices$Fireplace <- ifelse(houseprices$Fireplaces > 0, 1, 0)

## Now we have to drop the variables that we use to create the new features

houseprices <- houseprices %>% 
  select(-ScreenPorch, -EnclosedPorch, -OpenPorchSF, -X3SsnPorch,
         -HalfBath, -FullBath, -BsmtHalfBath, -BsmtFullBath,
         -X1stFlrSF, -X2ndFlrSF, -GrLivArea, -TotalBsmtSF, -YearBuilt, -YearRemodAdd,
         -Fence, -Fireplaces, -Neighborhood, -GarageArea, -TotalPorchSq,
         -MasVnrArea, -PoolArea, -New) %>% 
  mutate(YrSold = as.factor(YrSold), 
         Rich = as.factor(Rich),
         Remod = as.factor(Remod),
         Pool = as.factor(Pool),
         FenceBinary = as.factor(FenceBinary),
         Fireplace = as.factor(Fireplace))

## Random Forest
set.seed(1234)
house_split_tot <- houseprices %>% 
  initial_split()
houseprices_train <- training(house_split_tot)
houseprices_test <- testing(house_split_tot)


rf_spec <- rand_forest(mode = "regression",
                       trees = 500,
                       mtry = ) %>% 
  set_engine("ranger")

rf_spec

rf_fit <- rf_spec %>% 
  fit(SalePrice ~ . - Id,
      data = houseprices_train)

rf_fit

result_train <- rf_fit %>% 
  predict(new_data = houseprices_train) %>% 
  mutate(actual = houseprices_train$SalePrice)

result_train %>% 
  ggplot(aes(.pred, actual))+
  geom_point()
rmse(result_train$actual, result_train$.pred)

result_test <- rf_fit %>% 
  predict(new_data = houseprices_test) %>% 
  mutate(actual = houseprices_test$SalePrice)
result_test

result_test %>% 
  ggplot(aes(.pred, actual))+
  geom_point()

rmse(result_train$actual, result_train$.pred)
rmse(result_test$actual, result_test$.pred)


### RMSE for tets too high. Lets use cv

house_folds <- vfold_cv(houseprices_train) # This divides our house_train in 10 parts

rf_res <- fit_resamples(
  rf_spec, #The specification
  SalePrice ~ . - Id,
  house_folds, #Resamples
  control = control_resamples(save_pred = TRUE) #To fine tune the resampling process
)

rf_res %>% 
  collect_metrics()

rf_res %>% 
  unnest(.predictions) %>% 
  ggplot(aes(SalePrice, .pred, color = id)) +
  geom_abline(lty = 2, color = "black", size = 1) +
  geom_point(alpha = 0.5)



lm_spec <- linear_reg() %>% 
  set_engine( engine ="lm")

lm_spec

lm_fit <- lm_spec %>% 
  fit(SalePrice ~ . -Id,
      data = house_train)
lm_fit 

lm_res <- fit_resamples(
  lm_spec, #The specification
  SalePrice ~ . -Id,
  house_folds, #Resamples
  control = control_resamples(save_pred = TRUE) #To fine tune the resampling process
)

lm_res %>% 
  collect_metrics()
lm_res %>% 
  unnest(.predictions) %>% 
  ggplot(aes(SalePrice, .pred, color = id)) +
  geom_abline(lty = 2, color = "black", size = 1) +
  geom_point(alpha = 0.5)



## xgboost

control <- trainControl(method = "cv",  # cross validation
                       number = 10)     # 5-folds

# Create grid of tuning parameters

grid <- expand.grid(nrounds=c(100, 200, 400, 800),     # 3 different amounts of boosting rounds
                   max_depth= c(4, 6),           # 2 values for tree depth
                   eta=c(0.1, 0.05, 0.025),      # 3 values for learning rate
                   gamma= c(0.1), 
                   colsample_bytree = c(1), 
                   min_child_weight = c(1),
                   subsample=0.8)

xgb <-  train(SalePrice~. - Id,      
             data=houseprices_train,
             method="xgbTree",
             trControl=control, 
             tuneGrid=grid,
             maximize = FALSE)


xgb$results
xgb$bestTune
varImp(xgb)
varImp(xgb, scale = TRUE)$importance

houseprices_train$predict_boost <- predict(xgb, new_data = houseprices_train)
test_predictions <- predict(xgb, newdata = houseprices_test)
houseprices_test$predict_boost <- test_predictions


rmse(houseprices_train$SalePrice, houseprices_train$predict_boost)
rmse(houseprices_test$SalePrice, houseprices_test$predict_boost)
rmsle(houseprices_train$SalePrice, houseprices_train$predict_boost)
rmsle(houseprices_test$SalePrice, houseprices_test$predict_boost)

### Preparing test data
test_houseprices <- readr::read_csv("test.csv")

test_file <- test_houseprices %>% 
  mutate(MSZoning = as.factor(MSZoning),
         Street = as.factor(Street),
         Alley = as.factor(Alley),
         LotShape = as.factor(LotShape),
         LandContour = as.factor(LandContour),
         Utilities = as.factor(Utilities),
         LotConfig = as.factor(LotConfig),
         LandSlope = as.factor(LandSlope),
         Neighborhood = as.factor(Neighborhood),
         Condition1 = as.factor(Condition1),
         Condition2 = as.factor(Condition2),
         BldgType = as.factor(BldgType),
         HouseStyle = as.factor(HouseStyle),
         OverallQual = as.factor(OverallQual),
         OverallCond = as.factor(OverallCond),
         RoofStyle = as.factor(RoofStyle),
         RoofMatl = as.factor(RoofMatl),
         Exterior1st = as.factor(Exterior1st),
         Exterior2nd = as.factor(Exterior2nd),
         MasVnrType = as.factor(MasVnrType),
         ExterQual = as.factor(ExterQual),
         ExterCond = as.factor(ExterCond),
         Foundation = as.factor(Foundation),
         BsmtQual = as.factor(BsmtQual),
         BsmtCond = as.factor(BsmtCond),
         BsmtExposure = as.factor(BsmtExposure),
         BsmtFinType1 = as.factor(BsmtFinType1),
         BsmtFinType2 = as.factor(BsmtFinType2),
         Heating = as.factor(Heating),
         HeatingQC = as.factor(HeatingQC),
         CentralAir = as.factor(CentralAir),
         Electrical = as.factor(Electrical),
         KitchenQual = as.factor(KitchenQual),
         Functional = as.factor(Functional),
         FireplaceQu = as.factor(FireplaceQu),
         GarageType = as.factor(GarageType),
         GarageFinish = as.factor(GarageFinish),
         GarageQual = as.factor(GarageQual),
         GarageCond = as.factor(GarageCond),
         PavedDrive = as.factor(PavedDrive),
         PoolQC = as.factor(PoolQC),
         Fence = as.factor(Fence),
         MiscFeature = as.factor(MiscFeature),
         MoSold = as.factor(MoSold),
         YrSold = as.factor(YrSold),
         SaleType = as.factor(SaleType),
         SaleCondition = as.factor(SaleCondition),
         KitchenAbvGr = as.factor(KitchenAbvGr))
test_file$LotFrontage[is.na(test_file$LotFrontage)] <- round(mean(test_file$LotFrontage, na.rm=TRUE))
summary(test_file$LotFrontage)

test_file$GarageArea[is.na(test_file$GarageArea)] <- round(median(test_file$GarageArea, na.rm=TRUE))


table(test_file$Utilities)
summary(test_file$Utilities)
test_file$Utilities[is.na(test_file$Utilities)] <- "AllPub"
#test_file$MSSubClass <- ifelse(test_file_ready$MSSubClass == 150, 120, test_file_ready$MSSubClass)
#summary(test_file$MSSubClass)

for (i in factor_list) {
  test_file[[i]] <- na_to_none(test_file[[i]])
}
summary(test_file)

colnames(test_file)[colSums(is.na(test_file)) > 0]

## For GarageYrBlt I assume a NA means No garage but we can visualize it 
garage <- test_file %>% 
  select(GarageType, GarageYrBlt)

DT::datatable(filter(garage, garage$GarageType == "None"))
str(test_file$GarageYrBlt)


test_file$GarageYrBlt[is.na(test_file$GarageYrBlt)] <- 0

## For MasVnrType we can do substitute the Na's by nones and for the area by 0's
test_file$MasVnrType[is.na(test_file$MasVnrType)] <- "None"
summary(test_file$MasVnrType)


test_file$GarageCars[is.na(test_file$GarageCars)] <- 2


test_file$MasVnrArea[is.na(test_file$MasVnrArea)] <- 0

colnames(test_file)[colSums(is.na(test_file)) > 0]

## For MSZoning we use the most common zone RL
test_file$MSZoning[is.na(test_file$MSZoning)] <- "RL"

## Utilities is problematic. I think AllPub is the best solution 
summary(test_file$Utilities)

## Creating the Baths variables so we can remove all of the others.
test_file <- test_file %>% 
  mutate(Baths = 0.5*(HalfBath) + FullBath + 0.5*(BsmtHalfBath) + BsmtFullBath)

## Also we will remove all the Area variables and keep only totalarea.

## For Sale Type its only one observation missing so we can substitute it 
## with the most common WD
test_file$SaleType[is.na(test_file$SaleType)] <- "WD"

## Same with Functional and the rest of the categorical variables
test_file$Functional[is.na(test_file$Functional)] <- "Typ"
test_file$Exterior1st[is.na(test_file$Exterior1st)] <- "VinylSd"
test_file$Exterior2nd[is.na(test_file$Exterior2nd)] <- "VinylSd"
test_file$KitchenQual[is.na(test_file$KitchenQual)] <- "TA"


## For the numerical we substitute them for the mean or median value
test_file$BsmtFinSF1[is.na(test_file$BsmtFinSF1)] <- round(mean(test_file$BsmtFinSF1, na.rm=TRUE))
test_file$Baths[is.na(test_file$Baths)] <- median(test_file$Baths, na.rm=TRUE)
test_file$TotalBsmtSF[is.na(test_file$TotalBsmtSF)] <- round(mean(test_file$TotalBsmtSF, na.rm=TRUE))
test_file$GarageCars[is.na(test_file$GarageCars)] <- median(test_file$GarageCars, na.rm=TRUE)

test_file$stFlrSF <- test_file$`1stFlrSF`
test_file$ndFlrSF <- test_file$`2ndFlrSF`
test_file$SsnPorch <- test_file$`3SsnPorch`

################
test_file$Remod <- ifelse(test_file$YearBuilt == test_file$YearRemodAdd, 0, 1)
test_file$Age <- as.numeric(as.character(test_file$YrSold)) - test_file$YearBuilt
test_file$Age[is.na(test_file$Age)] <- round(mean(test_file$Age, na.rm=TRUE))
test_file$Age[test_file$Age <0] <- 0

test_file$New <- as.factor(ifelse(test_file$YearBuilt == test_file$YrSold, 0, 1))
table(test_file$New)
test_file$Rich <- 0
test_file$Rich[test_file$Neighborhood %in% c("NridgHt", "NoRidge", "StoneBr")] <- 4
test_file$Rich[test_file$Neighborhood %in% c("Timber", "Somerst", "Veenker",
                                             "Crawfor", "ClearCr", "CollgCr",
                                             "Blmngtn", "NWAmes", "Gilbert",
                                             "SawyerW")] <- 3
test_file$Rich[test_file$Neighborhood %in% c("Mitchel", "NPkVill", "NAmes",
                                             "SWISU", "Blueste", "Sawyer",
                                             "BrkSide", "Edwards", "OldTown")] <- 2
test_file$Rich[test_file$Neighborhood %in% c("BrDale", "IDOTRR", "MeadowV")] <- 1

table(test_file$Rich)

test_file$TotalSq <- test_file$GrLivArea + test_file$TotalBsmtSF


test_file$TotalPorchSq <- test_file$OpenPorchSF + test_file$EnclosedPorch + test_file$`3SsnPorch` + test_file$ScreenPorch

test_file$TotalSq <- test_file$GrLivArea + test_file$TotalBsmtSF + test_file$GarageArea + test_file$TotalPorchSq +
  test_file$MasVnrArea + test_file$`1stFlrSF` + test_file$`2ndFlrSF`

### Yes or No variables for each feature
test_file$Pool <- ifelse(test_file$PoolArea > 0, 1, 0)
test_file$FenceBinary <- ifelse(test_file$Fence == "None", 0, 1)
test_file$Fireplace <- ifelse(test_file$Fireplaces > 0, 1, 0)
summary(test_file)

test_file_ready <- test_file %>% 
  select(-LotShape, -PoolArea, -ScreenPorch, - SsnPorch, -EnclosedPorch, -OpenPorchSF,
         -ndFlrSF,-BsmtUnfSF, -BsmtFinSF2, -HalfBath, -FullBath, -BsmtHalfBath, -BsmtFullBath,
         -`1stFlrSF`, `2ndFlrSF`,  - `3SsnPorch`, -Fence, -Fireplaces, -FireplaceQu,
         -ExterQual, -ExterCond, -Foundation, -Heating, -MiscFeature,-MoSold, -SaleType,
         -LandContour, -RoofStyle, -Condition2,
         -RoofMatl, -PoolQC, -GarageQual, -SaleCondition, -GarageCond, -GarageFinish, -Electrical
         ,-BsmtFinType2, -BsmtFinType1, -PavedDrive, -Functional,
         -BldgType, -BsmtExposure, -GrLivArea, -TotalBsmtSF, -YearBuilt, -YearRemodAdd
         , -Neighborhood, -HeatingQC, -BsmtCond, -New, -GarageArea) %>% 
  mutate(YrSold = as.factor(YrSold), 
         Rich = as.factor(Rich),
         Remod = as.factor(Remod))


str(test_file_ready)
# test_file_ready <- test_file_ready %>%
#   mutate(LotArea = log(LotArea), MasVnrArea = ifelse(MasVnrArea == 0, 0, log(MasVnrArea)), BsmtFinSF1 = ifelse(BsmtFinSF1 == 0, 0, log(BsmtFinSF1)), `2ndFlrSF` = ifelse(`2ndFlrSF` == 0, 0, log(`2ndFlrSF`)),
#          TotRmsAbvGrd = log(TotRmsAbvGrd), Fireplaces = ifelse(Fireplaces == 0, 0, log(Fireplaces)), stFlrSF = log(stFlrSF), Baths = log(Baths), Age = ifelse(Age == 0, 0, log(Age)), TotalSq = log(TotalSq))
summary(test_file_ready)

### For tree-based models

test_houseprices[is.na(test_houseprices)] <- -1 ### Numeric flag

### Now we have to create the new features for this data set too.

test_houseprices <- test_houseprices %>% 
  mutate(Baths = 0.5*(HalfBath) + FullBath + 0.5*(BsmtHalfBath) + BsmtFullBath)

test_houseprices$Remod <- ifelse(test_houseprices$YearBuilt == test_houseprices$YearRemodAdd, 0, 1)

test_houseprices$Age <- as.numeric(test_houseprices$YrSold) - test_houseprices$YearRemodAdd

test_houseprices$New <- ifelse(test_houseprices$YearBuilt == test_houseprices$YrSold, 0, 1)

test_houseprices$Rich <- 0
test_houseprices$Rich[test_houseprices$Neighborhood %in% c("NridgHt", "NoRidge", "StoneBr")] <- 4
test_houseprices$Rich[test_houseprices$Neighborhood %in% c("Timber", "Somerst", "Veenker",
                                                 "Crawfor", "ClearCr", "CollgCr",
                                                 "Blmngtn", "NWAmes", "Gilbert",
                                                 "SawyerW")] <- 3
test_houseprices$Rich[test_houseprices$Neighborhood %in% c("Mitchel", "NPkVill", "NAmes",
                                                 "SWISU", "Blueste", "Sawyer",
                                                 "BrkSide", "Edwards", "OldTown")] <- 2
test_houseprices$Rich[test_houseprices$Neighborhood %in% c("BrDale", "IDOTRR", "MeadowV")] <- 1

test_houseprices$TotalPorchSq <- test_houseprices$OpenPorchSF + test_houseprices$EnclosedPorch + test_houseprices$X3SsnPorch + test_houseprices$ScreenPorch

test_houseprices$TotalSq <- test_houseprices$GrLivArea + test_houseprices$TotalBsmtSF + 
  test_houseprices$GarageArea + test_houseprices$TotalPorchSq + test_houseprices$MasVnrArea + test_houseprices$X1stFlrSF + test_houseprices$X2ndFlrSF

### Yes or No variables for each feature
test_houseprices$Pool <- ifelse(test_houseprices$PoolArea > 0, 1, 0)
test_houseprices$FenceBinary <- ifelse(test_houseprices$Fence == "None", 0, 1)
test_houseprices$Fireplace <- ifelse(test_houseprices$Fireplaces > 0, 1, 0)

# test_houseprices$Utilities <- as.factor(ifelse(test_houseprices$Utilities == "None", "NoSeWa", "AllPub"))
# test_houseprices$Exterior1st <- factor(ifelse(test_houseprices$Exterior1st == "None", "VinylSd", as.character(test_houseprices$Exterior1st)))
# test_houseprices$Exterior2nd <- factor(ifelse(test_houseprices$Exterior2nd == "None", "VinylSd", as.character(test_houseprices$Exterior2nd)))
# test_houseprices$KitchenQual <- factor(ifelse(test_houseprices$KitchenQual == "None", "TA", as.character(test_houseprices$KitchenQual)))
# test_houseprices$Functional <- factor(ifelse(test_houseprices$Functional == "None", "Typ", as.character(test_houseprices$Functional)))
# test_houseprices$SaleType <- factor(ifelse(test_houseprices$SaleType == "None", "WD", as.character(test_houseprices$SaleType)))

## Now we have to drop the variables that we use to create the new features

test_houseprices <- test_houseprices %>% 
  select(-ScreenPorch, -EnclosedPorch, -OpenPorchSF, -X3SsnPorch,
         -HalfBath, -FullBath, -BsmtHalfBath, -BsmtFullBath,
         -X1stFlrSF, -X2ndFlrSF, -GrLivArea, -TotalBsmtSF, -YearBuilt, -YearRemodAdd,
          -Fence, -Fireplaces, -Neighborhood, -GarageArea, -TotalPorchSq,
         -MasVnrArea, -PoolArea, -New) %>% 
  mutate(YrSold = as.factor(YrSold), 
         Rich = as.factor(Rich),
         Remod = as.factor(Remod),
         Pool = as.factor(Pool),
         FenceBinary = as.factor(FenceBinary),
         Fireplace = as.factor(Fireplace))

###################

test_houseprices$prediction_xgb <- predict(xgb, newdata = test_houseprices)
submission_xgb2 <- test_houseprices %>% 
  select(Id, prediction_xgb) 

colnames(submission_xgb2)[2] <- "SalePrice"


write.csv(submission_xgb2, "C:\\Users\\Daniel Gutierrez\\Desktop\\R Practice\\House Prices\\submission_xgb10.csv",
          row.names = FALSE)



house_test_ready$prediction_lm1 <- predict(linear1, newdata = house_test_ready)
submission_lm1 <- house_test_ready %>% 
  rename(SalePrice = prediction_lm1) %>% 
  select(Id, SalePrice) 
write.csv(submission_lm1, "C:\\Users\\Daniel Gutierrez\\Desktop\\R Practice\\House Prices\\submission_lm1.csv",
          row.names = FALSE)

submission_rf1 <- rf_fit %>% 
  predict(new_data = house_test_ready) %>% 
  mutate(Id = house_test_ready$Id,
         SalePrice = .pred) %>% 
  select(Id, SalePrice)
write.csv(submission_rf1, "C:\\Users\\Daniel Gutierrez\\Desktop\\R Practice\\House Prices\\submission_rf1.csv",
          row.names = FALSE)
