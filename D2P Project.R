library(dplyr)
library(tools)
library(faraway)
library(perturb)
library(leaps)
library(car)
library(caret)
library(ggplot2)
library(gridExtra)
library(jtools)
library(effects)
library(stringr)

##### DATA #####

survey_data = read.csv(file="~/Regression/D2P Project/Data/american_community_survey_nbrhd_2010_2014.csv", header=TRUE)
survey = survey_data %>% select(NBHD_NAME, TTL_POPULATION_ALL, PCT_HISPANIC, PCT_WHITE, MEDIAN_AGE_ALL, TTLPOP_25PLUS_EDU, 
                                LESS_THAN_HS_DIPLOMA_EDU, HSGRAD_OR_EQUIV_EDU, TTL_HOUSING_UNITS, VACANT_HU, RENTER_OCCUPIED_HU, 
                                OWNER_OCCUPIED_HU, FAMILY_HOUSEHOLDS,TTL_HOUSEHOLDS, MED_HH_INCOME, MED_GROSS_RENT, MEDIAN_HOME_VALUE, PCT_POVERTY)

character_data = read.csv(file="~/Regression/D2P Project/Data/NeighborhoodCharacteristicsCountData.csv", header=TRUE)
characteristics = character_data %>% select(NBHD_NAME, Area_km2, PoliceStations, GroceryStores, HealthClinics, TotalIndividualSchools)

foreclosure_data = read.csv(file="~/Regression/D2P Project/Data/Neighborhood Foreclosures 2003-2016.csv", header=TRUE)
foreclosures = foreclosure_data %>% rowwise() %>% 
  mutate(Avg_Foreclosures = mean(c(Foreclosures_2010, Foreclosures_2011, Foreclosures_2012, Foreclosures_2013, Foreclosures_2014))) %>%
  select(NBHD_NAME, Avg_Foreclosures)

crime_data = read.csv(file="~/Regression/D2P Project/Data/Crime Data.csv", header=TRUE)
#crime = crime_data %>% group_by(NEIGHBORHOOD_ID) %>% summarize(sum(IS_CRIME))

crime_data$REPORTED_DATE = word(as.character(crime_data$REPORTED_DATE), 1)
crime_data$REPORTED_DATE = as.Date(crime_data$REPORTED_DATE, "%m/%d/%Y")
crime_2014 = crime_data[crime_data$REPORTED_DATE >= as.Date("2014-01-01") & crime_data$REPORTED_DATE <= as.Date("2014-12-31"),]
crime_2015 = crime_data[crime_data$REPORTED_DATE >= as.Date("2015-01-01") & crime_data$REPORTED_DATE <= as.Date("2015-12-31"),]
crime_2016 = crime_data[crime_data$REPORTED_DATE >= as.Date("2016-01-01") & crime_data$REPORTED_DATE <= as.Date("2016-12-31"),]
crime_2017 = crime_data[crime_data$REPORTED_DATE >= as.Date("2017-01-01") & crime_data$REPORTED_DATE <= as.Date("2017-12-31"),]
crime_2018 = crime_data[crime_data$REPORTED_DATE >= as.Date("2018-01-01") & crime_data$REPORTED_DATE <= as.Date("2018-12-31"),]
crime_2019 = crime_data[crime_data$REPORTED_DATE >= as.Date("2019-01-01") & crime_data$REPORTED_DATE <= as.Date("2019-12-31"),]

crime_2014 = crime_2014 %>% group_by(NEIGHBORHOOD_ID) %>% summarize(crime2014=sum(IS_CRIME))
crime_2015 = crime_2015 %>% group_by(NEIGHBORHOOD_ID) %>% summarize(crime2015=sum(IS_CRIME))
crime_2016 = crime_2016 %>% group_by(NEIGHBORHOOD_ID) %>% summarize(crime2016=sum(IS_CRIME))
crime_2017 = crime_2017 %>% group_by(NEIGHBORHOOD_ID) %>% summarize(crime2017=sum(IS_CRIME))
crime_2018 = crime_2018 %>% group_by(NEIGHBORHOOD_ID) %>% summarize(crime2018=sum(IS_CRIME))

crime = merge(crime_2014, crime_2015)
crime = merge(crime, crime_2016)
crime = merge(crime, crime_2017)
crime = merge(crime, crime_2018)

crime = crime %>% rowwise() %>% 
  mutate(avg_crime = mean(c(crime2014, crime2015, crime2016, crime2017, crime2018))) %>%
  select(NEIGHBORHOOD_ID, avg_crime)

colnames(crime) = c("NBHD_NAME", "Crimes")
crime$NBHD_NAME = gsub("-", " ", crime$NBHD_NAME)
crime$NBHD_NAME = toTitleCase(crime$NBHD_NAME)
crime[which(crime$NBHD_NAME=="Dia"),1] = "DIA" 
crime[which(crime$NBHD_NAME=="Cbd"),1] = "CBD" 
crime[which(crime$NBHD_NAME=="College View South Platte"),1] = "College View - South Platte" 
crime[which(crime$NBHD_NAME=="Gateway Green Valley Ranch"),1] = "Gateway - Green Valley Ranch"
crime[which(crime$NBHD_NAME=="Cory Merrill"),1] = "Cory - Merrill"

housing_data = merge(survey, characteristics, by="NBHD_NAME")
housing_data = merge(housing_data, foreclosures, by="NBHD_NAME")
housing_data = merge(housing_data, crime, by="NBHD_NAME")
housing_data = housing_data[-which(housing_data$NBHD_NAME=="DIA"),] #remove DIA

housing_data = housing_data %>% 
  mutate(no_college = ((LESS_THAN_HS_DIPLOMA_EDU + HSGRAD_OR_EQUIV_EDU)/TTLPOP_25PLUS_EDU)*100) %>%
  mutate(renter = (RENTER_OCCUPIED_HU/TTL_HOUSING_UNITS)*100) %>%
  mutate(vacant = (VACANT_HU/TTL_HOUSING_UNITS)*100) %>%
  mutate(family = (FAMILY_HOUSEHOLDS/TTL_HOUSEHOLDS)*100) %>%
  mutate(income = (MED_HH_INCOME/1000)) %>%
  mutate(value = (MEDIAN_HOME_VALUE/10000)) %>%
  mutate(police = (PoliceStations/TTL_POPULATION_ALL)*10000) %>%
  mutate(clinics = (HealthClinics/TTL_POPULATION_ALL)*10000) %>%
  mutate(schools = (TotalIndividualSchools/TTL_POPULATION_ALL)*10000) %>%
  mutate(crimes = (Crimes/TTL_POPULATION_ALL)*100) %>%
  mutate(foreclosures = (Avg_Foreclosures/OWNER_OCCUPIED_HU)*1000)
housing_data = housing_data[-which(is.na(housing_data$value)),] #remove Kennedy and Sun Valley - NA home value

housing = housing_data %>%
  select(hispanic=PCT_HISPANIC, white=PCT_WHITE, age=MEDIAN_AGE_ALL, no_college, renter, vacant,
         family, income, rent=MED_GROSS_RENT, value, poverty=PCT_POVERTY, foreclosures, crimes, 
         police, clinics, schools, size=Area_km2)

##### PLOTS #####

# Histrograms

ggplot(housing, aes(x=rent)) + 
  geom_histogram(aes(y=..density..), col="black", fill="royalblue3") +
  geom_density(aes(y=..density..), size=1) + 
  labs(x="Rent", title="Distribution of Rental Prices", size=5) +
  theme_bw(base_size=15)

ggplot(housing, aes(rent)) + 
  geom_histogram(col="black", bins=20) + 
  labs(x="Rent", title="Median Rent") + 
  theme_bw(base_size=20)

ggplot(housing, aes(value*10)) + 
  geom_histogram(col="black", bins=20) + 
  labs(x="Value (in thousands)", title="Median Home Value") +
  theme_bw(base_size=20)

ggplot(housing, aes(poverty)) + 
  geom_histogram(col="black", bins=20) + 
  labs(x="Poverty (percentage)", title="Poverty") +
  theme_bw(base_size=20)

ggplot(housing, aes(income)) + 
  geom_histogram(col="black", bins=20) + 
  labs(x="Income (in thousands)", title="Median Household Income") +
  theme_bw(base_size=20)

ggplot(housing, aes(crimes/100)) + 
  geom_histogram(col="black", bins=20) + 
  labs(x="Crimes (per person)", title="Crime") +
  theme_bw(base_size=20)

ggplot(housing, aes(foreclosures/10)) + 
  geom_histogram(col="black", bins=20) + 
  labs(x="Foreclosures (percantage)", title="Foreclosures") +
  theme_bw(base_size=20)

# Bivariate plots

ggplot(housing, aes(x=value*10, y=rent)) + 
  geom_point(size=2) + 
  labs(x="Home Value (in thousands)", y="Rent", title="Rent vs Home Value") + 
  theme_bw(base_size=20) 

ggplot(housing, aes(x=foreclosures/10, y=rent)) + 
  geom_point(size=2) + 
  labs(x="Foreclosures (percentage)", y="Rent", title="Rent vs Foreclosures") +
  theme_bw(base_size=20)

ggplot(housing, aes(x=crimes/100, y=rent)) + 
  geom_point(size=2) + 
  labs(x="Crimes (per person)", y="Rent", title="Rent vs Crime") +
  theme_bw(base_size=20)

ggplot(housing, aes(x=poverty, y=rent)) + 
  geom_point(size=2) + 
  labs(x="Poverty (percentage)", y="Rent", title="Rent vs Poverty") +
  theme_bw(base_size=20)

ggplot(housing, aes(x=value*10, y=foreclosures/10)) + 
  geom_point(size=2) + 
  labs(x="Home Value (in thousands)", y="Foreclosures (percentage)", title="Foreclosures vs Home Value") +
  theme_bw(base_size=20)

ggplot(housing, aes(x=poverty, y=crimes/100)) + 
  geom_point(size=2) + 
  labs(x="Poverty (percentage)", y="Crimes (per person)", title="Crimes vs Poverty") +
  theme_bw(base_size=20)

##### LINEAR MODEL #####

lmod = lm(rent ~ ., data=housing)
sumary(lmod)

##### COLLINEARITY #####

vif(lmod)
colldiag(lmod, scale = FALSE, add.intercept = FALSE) 

lmod2 = update(lmod, . ~ . - no_college)
vif(lmod2)
colldiag(lmod2, scale = FALSE, add.intercept = FALSE) 

lmod3 = update(lmod2, . ~ . - white)
vif(lmod3)
colldiag(lmod3, scale = FALSE, add.intercept = FALSE) 

lmod4 = update(lmod3, . ~ . - income)
vif(lmod4)
colldiag(lmod4, scale = FALSE, add.intercept = FALSE) 

lmod5 = update(lmod4, . ~ . - family)
vif(lmod5)
colldiag(lmod5, scale = FALSE, add.intercept = FALSE) 

lmod6 = update(lmod5, . ~ . - police)
vif(lmod6)
colldiag(lmod6, scale = FALSE, add.intercept = FALSE) 

lmod7 = update(lmod6, . ~ . - clinics)
vif(lmod7)
colldiag(lmod7, scale = FALSE, add.intercept = FALSE)

##### VARIABLE SELECTION #####

housing = housing %>% select(rent, hispanic, age, renter, vacant, value, poverty, foreclosures, schools, crimes, size)
rownames(housing) = housing_data$NBHD_NAME

# AIC: p=7
b <- regsubsets(rent ~ ., data = housing, nvmax=30)
rs <- summary(b)
rs$which
p = 2:11
AIC = rs$bic + p * (2 - log(75))

ggplot() + aes(x=p, y=AIC) + 
  geom_point(size=3) + 
  labs(title="AIC") +
  theme_bw(base_size=25)

# Adjusted R-squared: p=8
ggplot() + aes(x=p, y=rs$adjr2) + 
  geom_point(size=3) + 
  labs(y=expression({R^2}[a]), title=expression(paste("Adjusted R"^"2"))) +
  theme_bw(base_size=25)
which.max(rs$adjr2)

# Mallows Cp: p=7
ggplot() + aes(x=p, y=rs$cp) + 
  geom_point(size=3) + 
  geom_abline(intercept=0, slope=1) +
  labs(y=expression(paste(C[p], " statistic")), title=expression(paste("Mallows ", C[p]))) +
  theme_bw(base_size=25)
which.min(rs$cp)

# Cross validation: p=7
cv_10fold = trainControl(method = "cv", number = 10) 
cv_loo = trainControl(method = "LOOCV")

f1 = rent ~ . #full model
f2 = rent ~ age + renter + value + poverty + foreclosures + crimes + size #p=8 
f3 = rent ~ renter + value + poverty + foreclosures + crimes + size #p=7

modela = train(f1, data = housing, trControl = cv_10fold, method = "lm")
modelb = train(f2, data = housing, trControl = cv_10fold, method = "lm")
modelc = train(f3, data = housing, trControl = cv_10fold, method = "lm")

print(modela) #full, 10-fold
print(modelb) #p=8, 10-fold
print(modelc) #p=7, 10-fold

modeld = train(f1, data = housing, trControl = cv_loo, method = "lm")
modele = train(f2, data = housing, trControl = cv_loo, method = "lm")
modelf = train(f3, data = housing, trControl = cv_loo, method = "lm")

print(modeld) #full, loo
print(modele) #p=8, loo
print(modelf) #p=7, loo

# Remove Variables
lmod8 = update(lmod7, . ~ . - vacant)
lmod9 = update(lmod8, . ~ . - schools)
lmod10 = update(lmod9, . ~ . - hispanic)
lmod11 = update(lmod10, . ~ . - age)
lmod12 = update(lmod11, . ~ . - size)

##### MODEL STRUCTURE #####

residualPlots(lmod12, main="Residual Plots")
marginalModelPlots(lmod12)
avPlots(lmod12)
crPlots(lmod12)
ceresPlots(lmod12)

# Transformations
lmod13 = lm(rent ~ renter + value + poverty + poly(foreclosures,2) + sqrt(crimes), data=housing) #R2=0.59

residualPlots(lmod13, main="Residual Plots with Transformations")
marginalModelPlots(lmod13)
avPlots(lmod13)
crPlots(lmod13)
ceresPlots(lmod13)

# High leverage points
influencePlot(lmod13, main="Influence Plot") 

housing2 = housing[-2,] #Auraria, R2=0.59
housing10 = housing[-10,] #CBD, R2=0.6
housing21 = housing[-21,] #Cory-Merrill, R2=0.6
housing22 = housing[-22,] #Country Club, R2=0.62
housing70 = housing[-70,] #Wellshire, R2=0.58

housing_updated = housing[-c(2,10,21,22,70),] #remove influence points, R2=0.67

lmod14 = lm(rent ~ renter + value + poverty + poly(foreclosures,2) + sqrt(crimes), data=housing_updated)
sumary(lmod14)

##### HYPOTHESIS TESTING #####

# Test whether crime is needed in the model
lms = summary(lmod13)
tobs = lms$coef[7,3]

nreps = 4000
tsim = numeric(nreps)
set.seed(123)
for (i in 1:nreps) {
  lmodp = lm(rent ~ renter + value + poverty + poly(foreclosures,2) + sample(sqrt(crimes)), housing)
  lmodps = summary(lmodp)
  tsim[i] = lmodps$coef[7,3]
}
mean(abs(tsim) >= abs(tobs))

hist(tsim, freq = FALSE)
abline(v = tobs)

##### PREDICTION #####

x <- model.matrix(lmod12)
x0 <- apply(x, 2, median) #median values of predictor variables

predict(lmod13, new=data.frame(t(x0))) #point estimate
predict(lmod13, new=data.frame(t(x0)), interval="prediction", level = 0.95) #prediction interval
predict(lmod13, new=data.frame(t(x0)), interval="confidence", level = 0.95) #confidence interval

##### RESULTS #####

# Save summary results
results = summary(lmod13)$coefficients
results = as.data.frame(results)
write.csv(file="Results.csv", x=results)

lm.fortified = fortify(lmod13)
lm.fortified$extreme = ifelse(abs(lm.fortified$`.resid`) > 400, 1, 0) # extreme values
r2 = format(summary(lmod13)$r.squared, digits=2)

# y vs yhat plot
plot1 = ggplot(data=lm.fortified, aes(x=.fitted, y=rent)) + 
  geom_point(size=2) + 
  theme_bw(base_size=20) + labs(x="Fitted", y="Actual", title="Actual vs Fitted") + 
  geom_abline(aes(intercept=0, slope=1))
  #annotate("text", label=paste("R^2: ", r2, sep=""), x=900, y=1700, parse=T, size=4)

# Residual plot with extreme values
plot2 = ggplot(lmod13, aes(x=.fitted, y=.resid)) + 
  geom_point(col="red", size=2) + 
  geom_hline(yintercept = 0) + 
  theme_bw() + labs(x="Fitted", y="Residuals", title="Residuals vs Fitted") + 
  geom_text(data = lm.fortified[lm.fortified$extreme == 1, ], 
            aes(label=rownames(lm.fortified[lm.fortified$extreme == 1, ]), x=.fitted, y=.resid), hjust="right", vjust=-0.5, size=3)

grid.arrange(plot1, plot2, ncol=2, nrow=1)

# Confidence interval plot
plot_summs(lmod13, scale=TRUE, col="blue")

# Effect size plots
par(mfrow = c(2, 2))
plot(Effect("crimes", lmod14))
plot(Effect("value", lmod14))
plot(Effect("foreclosures", lmod14))
