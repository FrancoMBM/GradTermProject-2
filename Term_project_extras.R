### Importing necessary libraries

library(ggplot2)
library(dbplyr)
library(tidyr)
library(tidyverse)
library(asbio)
library(data.table)
library(RSQLite)
library(zoo)
library(xts)
library(car)
library(emmeans)
setwd("C:/Users/franc/Desktop/BU_MS_ANALYTICS/1Semester/CS555/Term_project")

# Data from: https://www.redfin.com/news/data-center/, https://www.redfin.com/news/data-center-metrics-definitions/

### WARNING: Big file, if pc is not powerful start from line 100

# Opening the table:

df <- read.table(file = 'weekly_housing_market_data_most_recent.tsv', sep = '\t', header = TRUE)
df <- df[df$region_name %like% "MA", ]

df2 <- df[df$region_name %like% "MA", ]
df2 <- df[,c(3,4,5,6,7,8,10,18,22,38)]

drv <- dbDriver("SQLite")
con <- dbConnect(drv, dbname = "housing.db")


dbWriteTable(con, "housing", df2)


# Access from database to make things faster: tsv files contain hidden data which
# makes the things run slower


query <- function(region.id, density, county.name){
  q <- dbGetQuery(con, "SELECT region_type, period_begin,
                      period_end, duration, total_homes_sold, 
                      average_homes_sold, median_sale_price, median_days_to_close, 
                      average_new_listings FROM housing WHERE region_id == ?", region.id)
  
  q <- cbind(q, Pop.Density = rep(density, nrow(q)), Name = rep(county.name, nrow(q)))
  return(q)
}



# norfolk = region id == 1344

norfolk <- query(1344, 1536.8, "Norfolk" )


# suffolk = region id == 1346

suffolk <- query(1346, 6221.3, "Suffolk")

# middlesex = region id == 1342

middlesex <- query(1342, 1817.9, "Middlesex")


# essex = region id == 1338

essex <- query(1338, 914.2, "Essex")

# bristol = region id == 1336

bristol <- query(1336, 797.2, "Bristol")


# plymouth = region id == 1345

plymouth <- query(1345, 458.1, "Plymouth")

# combining all rows:

dataset <- rbind(norfolk, suffolk, middlesex, essex, bristol, plymouth)


# Consulted:  https://stackoverflow.com/questions/33322248/how-to-import-a-tsv-file
#             http://www.usa.com/rank/massachusetts-state--population-density--county-rank.htm
#             r-Documentation


# Saving final dataset:

data.table::fwrite(dataset, file = 'final.csv', sep = ',')

#write.table(df, file = 'Ma_House_prices.tsv', sep = '\t')


# Consulted: https://stackoverflow.com/questions/18587334/subset-data-to-contain-only-columns-whose-names-match-a-condition
#https://stackoverflow.com/questions/13043928/selecting-data-frame-rows-based-on-partial-string-match-in-a-column
#https://stackoverflow.com/questions/17108191/how-to-export-proper-tsv/17108345
# https://www.quora.com/How-do-you-load-a-TSV-file-into-R
#https://stackoverflow.com/questions/50021302/writing-small-dataframe-to-csv-creates-a-huge-file

# Clear the memory:

########################### Start from here:####################################

################ 1. Preliminary steps:##########################

df <- read.csv(file = 'final.csv', sep = ',', header = TRUE)


# a) Selecting columns of interest:

df <- df[,c(2,4,6,7,8,9,10,11)]

# b) Selecting years of interest (2017 to 2020):

# Note that the date column 'Period END' was removed and the date column 
# 'Period Begin' was kept. The later was chosen over the former since, 
# logically, it should be more likely that a deal would be closed before the 
# listing period end than after that listing period is over.


# Transforming date column to datetime object:

df$period_begin <- as.Date(df$period_begin)

df <- subset(df,format(df$period_begin,'%Y')=='2017'| 
               format(df$period_begin,'%Y')=='2018' | 
               format(df$period_begin,'%Y')=='2019' |
               format(df$period_begin,'%Y')=='2020')

# ordering by date:

df <- df[order(df$period_begin),]


# Creating a new columns date factor:

#df[df$period_begin >= '2020-01-01',]
#df[df$period_begin >= '2019-01-01' & df$period_begin < '2020-01-01' ,]


year <- function(date.column){
  
  year.factor <- c()
  
  for (i in 1:length(date.column)){
    
    if(date.column[i]  >= '2020-01-01'){
      
      year.factor[i] = '2020'
      
    }else if (date.column[i]  >= '2019-01-01') {
      year.factor[i] = '2019'
    }else if (date.column[i]  >= '2018-01-01') {
      year.factor[i] = '2018'
    }else{
      year.factor[i] = '2017'
    }
    
  }

  return(year.factor)
}

year.factor <- year(df$period_begin)

df <- cbind(df, Year = year.factor) 



# consulted :https://www.stat.berkeley.edu/~s133/dates.html
#           https://stat.ethz.ch/pipermail/r-help/2011-September/289364.html
#           https://stackoverflow.com/questions/6246159/how-to-sort-a-data-frame-by-date/6246186
#           https://www.ling.upenn.edu/~joseff/rstudy/week2.html
#           https://stackoverflow.com/questions/22235809/append-value-to-empty-vector-in-r/22235924
#           https://stackoverflow.com/questions/22235809/append-value-to-empty-vector-in-r/22235924


# transforming 'duration' 'Pop.Density', 'Name' and 'year' columns into a factor:

df$duration <- as.factor(df$duration)
df$Year <- as.factor(df$Year)
df$Name <- as.factor(df$Name)
df$Pop.Density <- as.factor(df$Pop.Density)


########################### 2. Data Exploration :###############################

attach(df)

summary(df)

# Check for NANs:

any(is.na(df)) 

# a) Distribution of numeric columns before dealing with general outliers:

h <- function(col, name){
  hist(col, xlab = sprintf("%s", name), 
                    main = sprintf('General distribution of %s', name),
       cex.main = 0.8, col = 'wheat3')
}


par(mfrow = c(2,2))

h(average_homes_sold, 'Average Homes Sold')
h(median_sale_price, 'Median Sale Price')
h(median_days_to_close, 'Median Days to Close')
h(average_new_listings, 'Average New Listings')



par(mfrow = c(1,1))



# Shapiro-Wilk Test for the distribution

shapiro.test(average_homes_sold)
shapiro.test(median_sale_price)
shapiro.test(median_days_to_close)
shapiro.test(average_new_listings)

# Checking for general outliers 


b <- function(col, name, y = '', main = 'General Boxplot of %s', graph = boxplot){
  graph(col, xlab = sprintf("%s", name), ylab = y,
       main = sprintf(main, name),
       cex.main = 0.8, col = 'wheat3')
}

par(mfrow = c(2,2))

b(average_homes_sold, 'Average Homes Sold')
b(median_sale_price, 'Median Sale Price')
b(median_days_to_close, 'Median Days to Close')
b(average_new_listings, 'Average New Listings')

par(mfrow = c(1,1))


# Dealing with general outliers: 

df$average_homes_sold[df$average_homes_sold < quantile(df$average_homes_sold, probs = 0.25) - 1.5*IQR(df$average_homes_sold) |
                          
                          df$average_homes_sold > quantile(df$average_homes_sold, probs = 0.75) + 1.5*IQR(df$average_homes_sold)] <- NA

df$median_sale_price[df$median_sale_price < quantile(df$median_sale_price, probs = 0.25) - 1.5*IQR(df$median_sale_price) |
                          
                          df$median_sale_price > quantile(df$median_sale_price, probs = 0.75) + 1.5*IQR(df$median_sale_price)] <- NA

df$median_days_to_close[df$median_days_to_close < quantile(df$median_days_to_close, probs = 0.25) - 1.5*IQR(df$median_days_to_close) |
                          
                          df$median_days_to_close > quantile(df$median_days_to_close, probs = 0.75) + 1.5*IQR(df$median_days_to_close)] <- NA


df$average_new_listings[df$average_new_listings < quantile(df$average_new_listings, probs = 0.25) - 1.5*IQR(df$average_new_listings) |
                          
                          df$average_new_listings > quantile(df$average_new_listings, probs = 0.75) + 1.5*IQR(df$average_new_listings)] <- NA

# consulted: (Stack Overflow and d8aninja )



df <- na.omit(df) 

detach(df)

attach(df)


# Note that this is a preliminary exploration of data. 
# other potential issues with outliers and distribution will be dealt with 
# by factor before performing a particular statistical analysis




############################# 3. Analytics :####################################

###############Question a:###################

# Are there any significant differences in Median House Sale Price
# between 'Average Homes Sold' and the years 2017, 2018 , 2019, 2020?.

# The variable population density will not be considered here since it derives
# from the variable county. This is done in order to avoid using two measurement
# for the same group unit (dependency)

# i : First, testing for normality and homogeneity of variances for the groups
# of the two factor variables(Year and County) after dealing with potential outliers:



##### Outliers: ######


# Name (county):

Norfolk <- median_sale_price[Name == 'Norfolk']
Suffolk <-median_sale_price[Name == 'Suffolk']
Essex <- median_sale_price[Name == 'Essex']
Middlesex <- median_sale_price[Name == 'Middlesex']
Bristol <- median_sale_price[Name == 'Bristol']
Plymouth <- median_sale_price[Name == 'Plymouth']

par(mfrow = c(2,3))

b(Norfolk, 'Norfolk', y = 'Median Sale Price', main = 'House sale price for %s')
b(Suffolk, 'Suffolk', y = 'Median Sale Price', main = 'House sale price for %s')
b(Essex, 'Essex', y = 'Median Sale Price', main = 'House sale price for %s')
b(Middlesex, 'Middlesex', y = 'Median Sale Price', main = 'House sale price for %s')
b(Bristol, 'Bristol', y = 'Median Sale Price', main = 'House sale price for %s')
b(Plymouth, 'Plymouth', y = 'Median Sale Price', main = 'House sale price for %s')



par(mfrow = c(1,1))

county.outliers <- c(boxplot(Norfolk)$out,boxplot(Suffolk)$out, boxplot(Essex)$out,
                     boxplot(Middlesex)$out, boxplot(Bristol)$out,boxplot(Plymouth)$out )

for (i in county.outliers){
  df$median_sale_price[df$median_sale_price == i] <- NA
  }
    



df <- na.omit(df)  

detach(df)
attach(df)


# Year: 

year2017 <- median_sale_price[period_begin < '2018-01-01']

year2018 <- median_sale_price[period_begin < '2019-01-01']

year2019 <- median_sale_price[period_begin < '2020-01-01']
  
year2020 <- median_sale_price[period_begin >= '2020-01-01']



par(mfrow = c(2,2))

b(year2019 , '2017', y = 'Median Sale Price', main = 'House sale price for %s')
b(year2019 , '2018', y = 'Median Sale Price', main = 'House sale price for %s')
b(year2019 , '2019', y = 'Median Sale Price', main = 'House sale price for %s')
b(year2020, '2020', y = 'Median Sale Price', main = 'House sale price for %s')

par(mfrow = c(1,1))




### Summary for years 2017 to 2020 ####


df2017 <- subset(df, Year == '2017')
df2018 <- subset(df, Year == '2018')
df2019 <- subset(df, Year == '2019')
df2020  <- subset(df, Year == '2020')



# Overall:

summary(df2017$median_sale_price)
summary(df2018$median_sale_price)
summary(df2019$median_sale_price)
summary(df2020$median_sale_price)


# Per county and year:

summary(subset(df2017$median_sale_price, Name == 'Norfolk'))
summary(subset(df2017$median_sale_price, Name == 'Suffolk'))
summary(subset(df2017$median_sale_price, Name == 'Essex'))
summary(subset(df2017$median_sale_price, Name == 'Middlesex'))
summary(subset(df2017$median_sale_price, Name == 'Bristol'))
summary(subset(df2017$median_sale_price, Name == 'Plymouth'))

summary(subset(df2018$median_sale_price, Name == 'Norfolk'))
summary(subset(df2018$median_sale_price, Name == 'Suffolk'))
summary(subset(df2018$median_sale_price, Name == 'Essex'))
summary(subset(df2018$median_sale_price, Name == 'Middlesex'))
summary(subset(df2018$median_sale_price, Name == 'Bristol'))
summary(subset(df2018$median_sale_price, Name == 'Plymouth'))



summary(subset(df2019$median_sale_price, Name == 'Norfolk'))
summary(subset(df2019$median_sale_price, Name == 'Suffolk'))
summary(subset(df2019$median_sale_price, Name == 'Essex'))
summary(subset(df2019$median_sale_price, Name == 'Middlesex'))
summary(subset(df2019$median_sale_price, Name == 'Bristol'))
summary(subset(df2019$median_sale_price, Name == 'Plymouth'))

summary(subset(df2020$median_sale_price, Name == 'Norfolk'))
summary(subset(df2020$median_sale_price, Name == 'Suffolk'))
summary(subset(df2020$median_sale_price, Name == 'Essex'))
summary(subset(df2020$median_sale_price, Name == 'Middlesex'))
summary(subset(df2020$median_sale_price, Name == 'Bristol'))
summary(subset(df2020$median_sale_price, Name == 'Plymouth'))

##### Normality #####:

# Name (county):


par(mfrow = c(2,3))

b(Norfolk, 'Norfolk', y = 'Frequency', main = 'House sale price distribution for %s', graph = hist)
b(Suffolk, 'Suffolk', y = 'Frequency', main = 'House sale price distribution for %s', graph = hist)
b(Essex, 'Essex', y = 'Frequency', main = 'House sale price distribution for %s', graph = hist)
b(Middlesex, 'Middlesex', y = 'Frequency', main = 'House sale price distribution for %s', graph = hist)
b(Bristol, 'Bristol', y = 'Frequency', main = 'House sale price distribution for %s', graph = hist)
b(Plymouth, 'Plymouth', y = 'Frequency', main = 'House sale price distribution for %s', graph = hist)

par(mfrow = c(1,1))

shapiro.test(Norfolk)
shapiro.test(Suffolk)
shapiro.test(Essex)
shapiro.test(Middlesex)
shapiro.test(Bristol)
shapiro.test(Plymouth)


# Year: 

par(mfrow = c(2,2))

b(year2017, '2017', y = 'Frequency', main = 'House sale price distribution for %s', graph = hist)
b(year2018, '2018', y = 'Frequency', main = 'House sale price distribution for %s', graph = hist)
b(year2019, '2019', y = 'Frequency', main = 'House sale price distribution for %s', graph = hist)
b(year2020, '2020', y = 'Frequency', main = 'House sale price distribution for %s', graph = hist)

par(mfrow = c(1,1))


shapiro.test(year2017)
shapiro.test(year2018)
shapiro.test(year2019)
shapiro.test(year2020)

#### Homogeneity of variances #####

library(lawstat)
library(car)

# County:

leveneTest(df$median_sale_price, Name)

# Year:

# years <- cbind(year2017, year2018, year2019, year2020)

leveneTest(df$median_sale_price, Year)

# consulted: https://www.rdocumentation.org/packages/lawstat/versions/3.2/topics/levene.test

##### Graphical comparisons# of name and year #####:

boxplot(df$median_sale_price ~ df$Name, col = 'wheat3',
        ylab = 'Median House Sale Price', xlab = 'County')
boxplot(df$median_sale_price ~ df$Year, col = 'wheat3',
        ylab = 'Median House Sale Price', xlab = 'Year')


df2017 <- subset(df, Year == '2017')
df2018  <- subset(df, Year == '2018')
df2019 <- subset(df, Year == '2019')
df2020  <- subset(df, Year == '2020')

par(mfrow = c(2,2))

boxplot(df2017$median_sale_price ~ df2017$Name, col = 'wheat3',
        ylab = 'Median House Sale Price', xlab = 'County', main = '2017')

boxplot(df2018$median_sale_price ~ df2018$Name, col = 'wheat3',
        ylab = 'Median House Sale Price', xlab = 'County', main = '2018')


boxplot(df2019$median_sale_price ~ df2019$Name, col = 'wheat3',
        ylab = 'Median House Sale Price', xlab = 'County', main = '2019')

boxplot(df2020$median_sale_price ~ df2020$Name, col = 'wheat3',
        ylab = 'Median House Sale Price', xlab = 'County', main = '2020')

par(mfrow = c(1,1))

nor <- subset(df, Name == 'Norfolk')
su <- subset(df, Name == 'Suffolk')
es <- subset(df, Name == 'Essex')
mid <- subset(df, Name == 'Middlesex')
bris <- subset(df, Name == 'Bristol')
ply <- subset(df, Name == 'Plymouth')



par(mfrow = c(2,3))

boxplot(nor$median_sale_price ~ nor$Year,col = 'wheat3', xlab = 'Year', ylab = 'Median House Sale Price', main = 'Norfolk')
boxplot(su$median_sale_price ~ su$Year,col = 'wheat3', xlab = 'Year', ylab = 'Median House Sale Price', main = 'Suffolk')
boxplot(es$median_sale_price ~ es$Year,col = 'wheat3', xlab = 'Year', ylab = 'Median House Sale Price', main = 'Essex')
boxplot(mid$median_sale_price ~ mid$Year,col = 'wheat3', xlab = 'Year', ylab = 'Median House Sale Price', main = 'Middlesex')
boxplot(bris$median_sale_price ~ bris$Year,col = 'wheat3', xlab = 'Year', ylab = 'Median House Sale Price', main = 'Bristol')
boxplot(ply$median_sale_price ~ ply$Year,col = 'wheat3', xlab = 'Year', ylab = 'Median House Sale Price', main = 'Plymouth')

par(mfrow = c(1,1))




#### Statistical Analyses: ####

# Since the assumptions for parametric tests are not met. The parametric
# test will be accompanied by a non-parametric counterpart. The results obtained
# from the non parametric version of the test will be given more consideration
# that the results obtained for the parametric tests:

# Formal hypothesis test for question a. Two way ANOVa:

# 1. Setting hypothesis and alpha level

# Alpha = 0.05


# Parametric Test:---------


# Factor: Name(county) after controlling for Year

#Ho: mu-Norfolk = mu-Suffolk = mu-Essex = mu-Middlesex = mu-Plymouth = mu-Bristol = 0

#H1: mu -i != mu-j (at least two of the counties present significant differences 

# in Median Sale House Price)

# Factor : Year (after controlling for county)

#Ho: mu-2017 = mu2018 = mu-2019 = mu2020

#H1: mu-some.year != mu-other.year
  


# Non-parametric Test:------




# Factor: Name(county) after controlling for Year


#Ho: population-Norfolk = population-Suffolk = population-Essex 
# = population-Middlesex = population-Plymouth = population-Bristol = 0

#H1: population -i != population-j 

# (at least two of the counties present significant differences 
# in "Median Sale House Price")

# Factor : Year (after controlling for county)

#Ho: population-2017 = population-2018 = population-2019 = population-2020

#H1: population-some.year != population-some.other.year





#Consulted: Biostatistical Analysis, Jerold H. Zar 196-197p


# 2. Selecting the appropriate test statistic

# Two way ANOVA between dependent variable median_sale_price and the factor
# variables Name(county) and Year:

# If interactions are found, a stratified one-way analysis of variance will be
# performed between the dependent variable median_sale_price and the factor 
# variables Name(county) and Year followed by a post-hoc paired t-test

# Stratified non parametric Kruskal-Wallis analysis of variance between the 
# dependent variable median_sale_price and the factor variables Name(county)
# and Year followed by a Multiple comparison test after Kruskal-Wallis
# using critical differences among groups (described in kruskalmc {pgirmess} 
# package and Discovering Statistics Using R. Andy Field, Jeremy Miles & 
# Zoe Fields)
# 
# # Testing for interactions:



model <- lm(median_sale_price ~ Name + Year + Name*Year, data = df)

interaction.plot(df$Name, df$Year, df$median_sale_price, col = 1:2, 
                 ylab = 'Median Sale Price', xlab = 'County Name', legend = T, 
                 trace.label = 'Year', main = 'Interaction Between Year and County')

summary(model)


# There appears to be an interaction between the 
# county Suffolk and the years 2018, 2019 and 2020. Also, an interaction between
# the counties Norfolk and Essex with the year 2020. Suffolk is the most populous
# county in this analysis, which makes it very important. Due to the interactions,
# A stratified ANOVA will be performed followed by a Pairwise t test with 
# Bonferroni correction

# 3. Setting the decision rule:

# if Calculated statistic of choice 
# is Greater or equal than the Critical value, reject Ho. Also, if p-value < alpha,
# Reject Ho

# Critical values:


# Parametric:--------

# For two way analysis:

qf(0.95, df1 = 23, df2 = 1816) 



# For stratified analysis Factor: county, stratus(sub-dataframe = year)

#2017

qf(0.95, df1 = 5, df2 = 478) 

# Post-Hoc:

qt(0.975, df = 478)


#2018

qf(0.95, df1 = 5, df2 = 502) 

# Post-Hoc:

qt(0.975, df = 502)

#2019

qf(0.95, df1 = 5, df2 = 478) 

# Post-Hoc:

qt(0.975, df = 478)

#2020

qf(0.95, df1 = 5, df2 = 358) 

# Post-Hoc:

qt(0.975, df = 358)

# For stratified analysis Factor: Year,  stratus(sub-dataframe = county)

#Norfolk

qf(0.95, df1 = 3, df2 = 282) 

# Post-Hoc:

qt(0.975, df = 282)

#Suffolk

qf(0.95, df1 = 3, df2 = 484) 

# Post-Hoc:

qt(0.975, df = 484)

#Essex

qf(0.95, df1 = 3, df2 = 290) 

# Post-Hoc:

qt(0.975, df = 290)

#Middlesex

qf(0.95, df1 = 3, df2 = 79) 

# Post-Hoc:

qt(0.975, df = 79)

#Bristol

qf(0.95, df1 = 3, df2 = 402) 

# Post-Hoc:

qt(0.975, df = 402)

#Plymouth

qf(0.95, df1 = 3, df2 = 279) 

# Post-Hoc:

qt(0.975, df = 279)

# Non-parametric:--------



# For stratified analysis Factor: county, stratus(sub-dataframe = year)

qchisq(0.95, df = 5) 

# Post-Hoc:

critical.difference <- function(N, ni, nj, k){
  c = qnorm(1-(0.05/(k*(k-1))))*(sqrt((N*(N+1))/12*(1/ni+1/nj)))
  return(c)
}


# Example:

critical.difference(N = nrow(df2017), ni = nrow(subset(df2017, Name == 'Suffolk')), 
                    nj = nrow(subset(df2017, Name == 'Norfolk')), k = 6)




# For stratified analysis Factor: Year,  stratus(sub-dataframe = county)

qchisq(0.95, df = 3) 

# Post-Hoc:

# Example:

critical.difference(N = nrow(nor), ni = nrow(subset(nor, Year == '2017')), 
                    nj = nrow(subset(nor, Year == '2018')), k = 4)





# Consulted: Biostatistical Analysis, Jerold H. Zar 223-226p

# 4. Computing test statistic:


# Parametric:--------


# Stratification by # Year:

#2017

a <- aov(median_sale_price ~ Name, data = df2017)
summary(a)

#2018

b <- aov(median_sale_price ~ Name, data = df2018)
summary(b)

#2019

c <- aov(median_sale_price ~ Name, data = df2019)
summary(c)

#2020
d <- aov(median_sale_price ~ Name, data = df2020)
summary(d)


#Pairwise:

pairwise.t.test(median_sale_price, Name, data = df2017, p.dj = 'bonferroni')
pairwise.t.test(median_sale_price, Name, data = df2018, p.dj = 'bonferroni')
pairwise.t.test(median_sale_price, Name, data = df2019, p.dj = 'bonferroni')
pairwise.t.test(median_sale_price, Name, data = df2020, p.dj = 'bonferroni')


# consulted: Lecture 9 and 10 CS555. Module 5 Video tutorial code review
#            R-Documentation Two-way Interaction Plot



# Stratification by #County#


#Norfolk

summary(aov(median_sale_price ~ Year, data = nor))


#Suffolk


summary(aov(median_sale_price ~ Year, data = su))

#Essex


summary(aov(median_sale_price ~ Year, data = es))

#Middlesex


summary(aov(median_sale_price ~ Year, data = mid))

#Bristol


summary(aov(median_sale_price ~ Year, data = bris))

#Plymouth


summary(aov(median_sale_price ~ Year, data = ply))


#Pairwise:

pairwise.t.test(median_sale_price, Year, data = nor, p.dj = 'bonferroni')
pairwise.t.test(median_sale_price, Year, data = su, p.dj = 'bonferroni')
pairwise.t.test(median_sale_price, Year, data = es, p.dj = 'bonferroni')
pairwise.t.test(median_sale_price, Year, data = mid, p.dj = 'bonferroni')
pairwise.t.test(median_sale_price, Year, data = bris, p.dj = 'bonferroni')
pairwise.t.test(median_sale_price, Year, data = ply, p.dj = 'bonferroni')

# Non-Parametric:--------


# Stratification by Year:




library(pgirmess) 

# 2017:

kruskal.test(median_sale_price ~ Name, data = df2017)

# Pairwise:

kruskalmc(median_sale_price ~ Name, data = df2017)


# 2018:

kruskal.test(median_sale_price ~ Name, data = df2018)

# Pairwise:


kruskalmc(median_sale_price ~ Name, data = df2018) 


# 2019:

kruskal.test(median_sale_price ~ Name, data = df2019)

# Pairwise:

kruskalmc(median_sale_price ~ Name, data = df2019) 



# 2020:

kruskal.test(median_sale_price ~ Name, data = df2020)


# Pairwise:


kruskalmc(median_sale_price ~ Name, data = df2020) 


# Consulted: Discovering Statistics Using R. Andy Field, Jeremy Miles, Zoe Field,
# 679-681p R-Documentation, pgirmess library: 
# Multiple comparison test after Kruskal-Wallis

# Stratification by #County#




#Norfolk

kruskal.test(median_sale_price ~ Year, data = nor)
kruskalmc(median_sale_price ~ Year, data = nor)

#Suffolk

kruskal.test(median_sale_price ~ Year, data = su)
kruskalmc(median_sale_price ~ Year, data = su)

#Essex

kruskal.test(median_sale_price ~ Year, data = es)
kruskalmc(median_sale_price ~ Year, data = es)

#Middlesex

kruskal.test(median_sale_price ~ Year, data = mid)
kruskalmc(median_sale_price ~ Year, data = mid)
#Bristol

kruskal.test(median_sale_price ~ Year, data = bris)
kruskalmc(median_sale_price ~ Year, data = bris)

#Plymouth

kruskal.test(median_sale_price ~ Year, data = ply)
kruskalmc(median_sale_price ~ Year, data = ply)

# 5. Conclusions:



########################### Question b #########################################

# How the rest of the numeric variables influence the Median Sale Prices 
# compared to counties,in the years 2017, 2018, 2019 and 2020 separately?

# Dealing with covariates outliers for years 2017, 2018, 2019 and 2020:

library(car)
library(emmeans)

# Parametric ----------


# 2017

# Re running normal ANOVA of median sale prices by County 

m2017 <- aov(median_sale_price ~ Name, data = df2017)
summary(m2017)

# Calculating ANCOVA:

# Adjusted for Average Homes Sold

Anova(lm(df2017$median_sale_price ~ df2017$Name + df2017$average_homes_sold), type = 3)
lsm <- lm(median_sale_price~Name + average_homes_sold, data = df2017)
emm_options(contrasts = c('contr.treatment', 'contr.poly'))
emmeans(lsm, specs = 'Name', contr = 'pairwise')

# source: Video Lectures CS555 Module 5

# Adjusted for Median Days to Close

Anova(lm(df2017$median_sale_price ~ df2017$Name + df2017$median_days_to_close), type = 3)
lsm <- lm(median_sale_price~Name + median_days_to_close, data = df2017)
emm_options(contrasts = c('contr.treatment', 'contr.poly'))
emmeans(lsm, specs = 'Name', contr = 'pairwise')


# Adjusted for Average New Listings

Anova(lm(df2017$median_sale_price ~ df2017$Name + df2017$average_new_listings), type = 3)
lsm <- lm(median_sale_price~Name + average_new_listings, data = df2017)
emm_options(contrasts = c('contr.treatment', 'contr.poly'))
emmeans(lsm, specs = 'Name', contr = 'pairwise')

# 2018


# Re running normal ANOVA of median sale prices by County 

m2018 <- aov(median_sale_price ~ Name, data = df2018)
summary(m2018)

# Calculating ANCOVA:

# Adjusted for Average Homes Sold

Anova(lm(df2018$median_sale_price ~ df2018$Name + df2018$average_homes_sold), type = 3)
lsm <- lm(median_sale_price~Name + average_homes_sold, data = df2018)
emm_options(contrasts = c('contr.treatment', 'contr.poly'))
emmeans(lsm, specs = 'Name', contr = 'pairwise')

# source: Video Lectures CS555 Module 5

# Adjusted for Median Days to Close

Anova(lm(df2018$median_sale_price ~ df2018$Name + df2018$median_days_to_close), type = 3)
lsm <- lm(median_sale_price~Name + median_days_to_close, data = df2018)
emm_options(contrasts = c('contr.treatment', 'contr.poly'))
emmeans(lsm, specs = 'Name', contr = 'pairwise')


# Adjusted for Average New Listings

Anova(lm(df2018$median_sale_price ~ df2018$Name + df2018$average_new_listings), type = 3)
lsm <- lm(median_sale_price~Name + average_new_listings, data = df2018)
emm_options(contrasts = c('contr.treatment', 'contr.poly'))
emmeans(lsm, specs = 'Name', contr = 'pairwise')





# 2019

# Re running normal ANOVA of median sale prices by County 

m2019 <- aov(median_sale_price ~ Name, data = df2019)
summary(m2019)

# Calculating ANCOVA:

# Adjusted for Average Homes Sold

Anova(lm(df2019$median_sale_price ~ df2019$Name + df2019$average_homes_sold), type = 3)
lsm <- lm(median_sale_price~Name + average_homes_sold, data = df2019)
emm_options(contrasts = c('contr.treatment', 'contr.poly'))
emmeans(lsm, specs = 'Name', contr = 'pairwise')

# source: Video Lectures CS555 Module 5

# Adjusted for Median Days to Close

Anova(lm(df2019$median_sale_price ~ df2019$Name + df2019$median_days_to_close), type = 3)
lsm <- lm(median_sale_price~Name + median_days_to_close, data = df2019)
emm_options(contrasts = c('contr.treatment', 'contr.poly'))
emmeans(lsm, specs = 'Name', contr = 'pairwise')


# Adjusted for Average New Listings

Anova(lm(df2019$median_sale_price ~ df2019$Name + df2019$average_new_listings), type = 3)
lsm <- lm(median_sale_price~Name + average_new_listings, data = df2019)
emm_options(contrasts = c('contr.treatment', 'contr.poly'))
emmeans(lsm, specs = 'Name', contr = 'pairwise')


# 2020

# Re running normal ANOVA of median sale prices by County 

m2020 <- aov(median_sale_price ~ Name, data = df2020)
summary(m2020)

# Calculating ANCOVA:

# Adjusted for Average Homes Sold

Anova(lm(df2020$median_sale_price ~ df2020$Name + df2020$average_homes_sold), type = 3)
lsm <- lm(median_sale_price~Name + average_homes_sold, data = df2020)
emm_options(contrasts = c('contr.treatment', 'contr.poly'))
emmeans(lsm, specs = 'Name', contr = 'pairwise')

# source: Video Lectures CS555 Module 5

# Adjusted for Median Days to Close

Anova(lm(df2020$median_sale_price ~ df2020$Name + df2020$median_days_to_close), type = 3)
lsm <- lm(median_sale_price~Name + median_days_to_close, data = df2020)
emm_options(contrasts = c('contr.treatment', 'contr.poly'))
emmeans(lsm, specs = 'Name', contr = 'pairwise')


# Adjusted for Average New Listings

Anova(lm(df2020$median_sale_price ~ df2020$Name + df2020$average_new_listings), type = 3)
lsm <- lm(median_sale_price~Name + average_new_listings, data = df2020)
emm_options(contrasts = c('contr.treatment', 'contr.poly'))
emmeans(lsm, specs = 'Name', contr = 'pairwise')





# Non-Parametric ----------



library(sm)

# Robust ANCOVA

# 2017

# Adjusted for Average Homes Sold
sm.ancova(x = df2017$average_homes_sold, y = df2017$median_sale_price, group = df2017$Name, model ='equal')
# Adjusted for Median Days to Close
sm.ancova(x = df2017$median_days_to_close, y = df2017$median_sale_price, group = df2017$Name, model ='equal')
# Adjusted for Average New Listings
sm.ancova(x = df2017$average_new_listings, y = df2017$median_sale_price, group = df2017$Name, model ='equal')

# 2018


# Adjusted for Average Homes Sold
sm.ancova(x = df2018$average_homes_sold, y = df2018$median_sale_price, group = df2018$Name, model ='equal')
# Adjusted for Median Days to Close
sm.ancova(x = df2018$median_days_to_close, y = df2018$median_sale_price, group = df2018$Name, model ='equal')
# Adjusted for Average New Listings
sm.ancova(x = df2018$average_new_listings, y = df2018$median_sale_price, group = df2018$Name, model ='equal')


# 2019

# Adjusted for Average Homes Sold
sm.ancova(x = df2019$average_homes_sold, y = df2017$median_sale_price, group = df2019$Name, model ='equal')
# Adjusted for Median Days to Close
sm.ancova(x = df2019$median_days_to_close, y = df2017$median_sale_price, group = df2019$Name, model ='equal')
# Adjusted for Average New Listings
sm.ancova(x = df2019$average_new_listings, y = df2017$median_sale_price, group = df2019$Name, model ='equal')



# 2020

# Adjusted for Average Homes Sold
sm.ancova(x = df2020$average_homes_sold, y = df2020$median_sale_price, group = df2020$Name, model ='equal')
# Adjusted for Median Days to Close
sm.ancova(x = df2020$median_days_to_close, y = df2020$median_sale_price, group = df2020$Name, model ='equal')
# Adjusted for Average New Listings
sm.ancova(x = df2020$average_new_listings, y = df2020$median_sale_price, group = df2020$Name, model ='equal')


# Consulted: http://lojze.lugos.si/~darja/software/r/library/sm/html/sm.ancova.html
#            Nonparametric analysis of covariance (sm.ancova {sm})
#            Non-parametric Analysis of Covariance Stuart G. Young and Adrian W. Bowman
#            Department of Statistics, University of Glasgow, Glasgow G12 8QW, Scotland




# c) Is there a pattern or association between Median Sale Price and time (Years
# 2019 and 2020)?

nor2017 <- subset(df2017,  Name == 'Norfolk')
su2017 <- subset(df2017,  Name == 'Suffolk')
es2017 <- subset(df2017,  Name == 'Essex')
mid2017 <- subset(df2017,  Name == 'Middlesex')
bris2017 <- subset(df2017,  Name == 'Bristol')
ply2017 <- subset(df2017,  Name == 'Plymouth')

nor2018 <- subset(df2018,  Name == 'Norfolk')
su2018 <- subset(df2018,  Name == 'Suffolk')
es2018 <- subset(df2018,  Name == 'Essex')
mid2018 <- subset(df2018,  Name == 'Middlesex')
bris2018 <- subset(df2018,  Name == 'Bristol')
ply2018 <- subset(df2018,  Name == 'Plymouth')





nor2019 <- subset(df2019,  Name == 'Norfolk')
su2019 <- subset(df2019,  Name == 'Suffolk')
es2019 <- subset(df2019,  Name == 'Essex')
mid2019 <- subset(df2019,  Name == 'Middlesex')
bris2019 <- subset(df2019,  Name == 'Bristol')
ply2019 <- subset(df2019,  Name == 'Plymouth')

nor2020 <- subset(df2020,  Name == 'Norfolk')
su2020 <- subset(df2020,  Name == 'Suffolk')
es2020 <- subset(df2020,  Name == 'Essex')
mid2020 <- subset(df2020,  Name == 'Middlesex')
bris2020 <- subset(df2020,  Name == 'Bristol')
ply2020 <- subset(df2020,  Name == 'Plymouth')



# Is there a relationship? between time and median sale price:


lp <- function(period, sale.price, y, main){
  plt = plot(period, sale.price, xlab = sprintf('Year %s', y), 
             ylab = 'Median Sale Price', col = 'blue', 
             main = main, pch = 16)
  return(plt)
}


# 2017



par(mfrow = c(2,3))

lp(nor2017$period_begin, nor2017$median_sale_price, '2017', main = 'Norfolk')
lp(su2017$period_begin, su2017$median_sale_price, '2017', main = 'Suffolk')
lp(es2017$period_begin, es2017$median_sale_price, '2017', main = 'Essex')
lp(mid2017$period_begin, mid2017$median_sale_price, '2017', main = 'Middlesex')
lp(bris2017$period_begin, bris2017$median_sale_price, '2017', main = 'Bristol')
lp(ply2017$period_begin, ply2017$median_sale_price, '2017', main = 'Plymouth')

par(mfrow = c(1,1))

par(mfrow = c(2,3))

lp(nor2018$period_begin, nor2018$median_sale_price, '2018', main = 'Norfolk')
lp(su2018$period_begin, su2018$median_sale_price, '2018', main = 'Suffolk')
lp(es2018$period_begin, es2018$median_sale_price, '2018', main = 'Essex')
lp(mid2018$period_begin, mid2018$median_sale_price, '2018', main = 'Middlesex')
lp(bris2018$period_begin, bris2018$median_sale_price, '2018', main = 'Bristol')
lp(ply2018$period_begin, ply2018$median_sale_price, '2018', main = 'Plymouth')

par(mfrow = c(1,1))


par(mfrow = c(2,3))

lp(nor2019$period_begin, nor2019$median_sale_price, '2019', main = 'Norfolk')
lp(su2019$period_begin, su2019$median_sale_price, '2019', main = 'Suffolk')
lp(es2019$period_begin, es2019$median_sale_price, '2019', main = 'Essex')
lp(mid2019$period_begin, mid2019$median_sale_price, '2019', main = 'Middlesex')
lp(bris2019$period_begin, bris2019$median_sale_price, '2019', main = 'Bristol')
lp(ply2019$period_begin, ply2019$median_sale_price, '2019', main = 'Plymouth')

par(mfrow = c(1,1))


par(mfrow = c(2,3))

lp(nor2020$period_begin, nor2020$median_sale_price, '2020', main = 'Norfolk')
lp(su2020$period_begin, su2020$median_sale_price, '2020', main = 'Suffolk')
lp(es2020$period_begin, es2020$median_sale_price, '2020', main = 'Essex')
lp(mid2020$period_begin, mid2020$median_sale_price, '2020', main = 'Middlesex')
lp(bris2020$period_begin, bris2020$median_sale_price, '2020', main = 'Bristol')
lp(ply2020$period_begin, ply2020$median_sale_price, '2020', main = 'Plymouth')

par(mfrow = c(1,1))





library(lmtest)


lm.nor2017 <- lm(nor2017$median_sale_price ~ nor2017$period_begin)
summary(lm.nor2017)

lm.su2017 <- lm(su2017$median_sale_price ~ su2017$period_begin)

summary(lm.su2017)
plot(residuals(lm.su2017))
dwtest(formula = lm.su2017, alternative = 'two.sided')
shapiro.test(residuals(lm.su2017))

lm.es2017 <- lm(es2017$median_sale_price ~ es2017$period_begin)

summary(lm.es2017)
plot(residuals(lm.es2017))
dwtest(formula = lm.es2017, alternative = 'two.sided')
shapiro.test(residuals(lm.es2017))

lm.mid2017 <-lm(mid2017$median_sale_price ~ mid2017$period_begin)

summary(lm.mid2017)
plot(residuals(lm.mid2017))
dwtest(formula = lm.mid2017, alternative = 'two.sided')
shapiro.test(residuals(lm.mid2017))

lm.bris2017 <-lm(bris2017$median_sale_price ~ bris2017$period_begin)
summary(lm.bris2017)
plot(residuals(lm.bris2017))
dwtest(formula = lm.bris2017, alternative = 'two.sided')
shapiro.test(residuals(lm.bris2017))



lm.ply2017 <- lm(ply2017$median_sale_price ~ ply2017$period_begin)

summary(lm.ply2017)
plot(residuals(lm.ply2017))
dwtest(formula = lm.ply2017, alternative = 'two.sided')
shapiro.test(residuals(lm.ply2017))





# 2018

lp(nor2018$period_begin, nor2018$median_sale_price, '2018')
lp(su2018$period_begin, su2018$median_sale_price, '2018')
lp(es2018$period_begin, es2018$median_sale_price, '2018')
lp(mid2018$period_begin, mid2018$median_sale_price, '2018')
lp(bris2018$period_begin, bris2018$median_sale_price, '2018')
lp(ply2018$period_begin, ply2018$median_sale_price, '2018')

library(lmtest)

lm.nor2018 <- lm(nor2018$median_sale_price ~ nor2018$period_begin)
summary(lm.nor2018)



lm.su2018 <- lm(su2018$median_sale_price ~ su2018$period_begin)

summary(lm.su2018)
plot(residuals(lm.su2018))
dwtest(formula = lm.su2018, alternative = 'two.sided')
shapiro.test(residuals(lm.su2018))


lm.es2018 <- lm(es2018$median_sale_price ~ es2018$period_begin)

summary(lm.es2018)
plot(residuals(lm.es2018))
dwtest(formula = lm.es2018, alternative = 'two.sided')
shapiro.test(residuals(lm.es2018))

lm.mid2018 <-lm(mid2018$median_sale_price ~ mid2018$period_begin)

summary(lm.mid2018)
plot(residuals(lm.mid2018))
dwtest(formula = lm.mid2018, alternative = 'two.sided')
shapiro.test(residuals(lm.mid2018))

lm.bris2018 <-lm(bris2018$median_sale_price ~ bris2018$period_begin)
summary(lm.bris2018)
plot(residuals(lm.bris2018))
dwtest(formula = lm.bris2018, alternative = 'two.sided')
shapiro.test(residuals(lm.bris2018))



lm.ply2018 <- lm(ply2018$median_sale_price ~ ply2018$period_begin)

summary(lm.ply2018)
plot(residuals(lm.ply2018))
dwtest(formula = lm.ply2018, alternative = 'two.sided')
shapiro.test(residuals(lm.ply2018))



lp(nor2019$period_begin, nor2019$median_sale_price, '2019')
lp(su2019$period_begin, su2019$median_sale_price, '2019')
lp(es2019$period_begin, es2019$median_sale_price, '2019')
lp(mid2019$period_begin, mid2019$median_sale_price, '2019')
lp(bris2019$period_begin, bris2019$median_sale_price, '2019')
lp(ply2019$period_begin, ply2019$median_sale_price, '2019')

library(lmtest)

lm.nor2019 <- lm(nor2019$median_sale_price ~ nor2019$period_begin)
summary(lm.nor2019)



lm.su2019 <- lm(su2019$median_sale_price ~ su2019$period_begin)

summary(lm.su2019)
plot(residuals(lm.su2019))
dwtest(formula = lm.su2019, alternative = 'two.sided')
shapiro.test(residuals(lm.su2019))


lm.es2019 <- lm(es2019$median_sale_price ~ es2019$period_begin)

summary(lm.es2019)
plot(residuals(lm.es2019))
dwtest(formula = lm.es2019, alternative = 'two.sided')
shapiro.test(residuals(lm.es2019))

lm.mid2019 <-lm(mid2019$median_sale_price ~ mid2019$period_begin)

summary(lm.mid2019)
plot(residuals(lm.mid2019))
dwtest(formula = lm.mid2019, alternative = 'two.sided')
shapiro.test(residuals(lm.mid2019))

lm.bris2019 <-lm(bris2019$median_sale_price ~ bris2019$period_begin)
summary(lm.bris2019)
plot(residuals(lm.bris2019))
dwtest(formula = lm.bris2019, alternative = 'two.sided')
shapiro.test(residuals(lm.bris2019))



lm.ply2019 <- lm(ply2019$median_sale_price ~ ply2019$period_begin)

summary(lm.ply2019)
plot(residuals(lm.ply2019))
dwtest(formula = lm.ply2019, alternative = 'two.sided')
shapiro.test(residuals(lm.ply2019))




lp(nor2020$period_begin, nor2020$median_sale_price, '2020')
lp(su2020$period_begin, su2020$median_sale_price, '2020')
lp(es2020$period_begin, es2020$median_sale_price, '2020')
lp(mid2020$period_begin, mid2020$median_sale_price, '2020')
lp(bris2020$period_begin, bris2020$median_sale_price, '2020')
lp(ply2020$period_begin, ply2020$median_sale_price, '2020')



lm.nor2020 <- lm(nor2020$median_sale_price ~ nor2020$period_begin)
summary(lm.nor2020)
plot(residuals(lm.nor2020))
dwtest(formula = lm.nor2020, alternative = 'two.sided')
shapiro.test(residuals(lm.nor2020))


lm.su2020 <- lm(su2020$median_sale_price ~ su2020$period_begin)

summary(lm.su2020)
plot(residuals(lm.nor2020))
dwtest(formula = lm.nor2020, alternative = 'two.sided')
shapiro.test(residuals(lm.nor2020))

lm.es2020 <- lm(es2020$median_sale_price ~ es2020$period_begin)

summary(lm.es2020)
plot(residuals(lm.es2020))
dwtest(formula = lm.es2020, alternative = 'two.sided')
shapiro.test(residuals(lm.es2020))

lm.mid2020 <-lm(mid2020$median_sale_price ~ mid2020$period_begin)

summary(lm.mid2020)
plot(residuals(lm.mid2020))
dwtest(formula = lm.mid2020, alternative = 'two.sided')
shapiro.test(residuals(lm.mid2020))

lm.bris2020 <-lm(bris2020$median_sale_price ~ bris2020$period_begin)
summary(lm.bris2020)
plot(residuals(lm.bris2020))
dwtest(formula = lm.bris2020, alternative = 'two.sided')
shapiro.test(residuals(lm.bris2020))



lm.ply2020 <- lm(ply2020$median_sale_price ~ ply2020$period_begin)

summary(lm.ply2020)
plot(residuals(lm.ply2020))
dwtest(formula = lm.ply2020, alternative = 'two.sided')
shapiro.test(residuals(lm.ply2020))


### Due to the complexity of the housing market, it is very difficult to identify
# an equation that explain the variability of Median Sale price through the years
# 2019 and 2020. Having said that, significant correlations were found for  


cor.test(as.numeric(nor2017$period_begin), nor2017$median_sale_price, method = 'spearman') # Non Significant

cor.test(as.numeric(su2017$period_begin), su2017$median_sale_price, method = 'spearman') # Significant

cor.test(as.numeric(es2017$period_begin), es2017$median_sale_price, method = 'spearman') # Significant

cor.test(as.numeric(mid2017$period_begin), mid2017$median_sale_price, method = 'spearman') # Significant

cor.test(as.numeric(bris2017$period_begin), bris2017$median_sale_price, method = 'spearman') # Significant

cor.test(as.numeric(ply2017$period_begin), ply2017$median_sale_price, method = 'spearman') # Non Significant


cor.test(as.numeric(nor2018$period_begin), nor2018$median_sale_price, method = 'spearman') # Non Significant

cor.test(as.numeric(su2018$period_begin), su2018$median_sale_price, method = 'spearman') # Significant

cor.test(as.numeric(es2018$period_begin), es2018$median_sale_price, method = 'spearman') # Non Significant

cor.test(as.numeric(mid2018$period_begin), mid2018$median_sale_price, method = 'spearman')# Non Significant

cor.test(as.numeric(bris2018$period_begin), bris2018$median_sale_price, method = 'spearman')# Significant

cor.test(as.numeric(ply2018$period_begin), ply2018$median_sale_price, method = 'spearman') # Non Significant


cor.test(as.numeric(nor2019$period_begin), nor2019$median_sale_price, method = 'spearman') # Non Significant

cor.test(as.numeric(su2019$period_begin), su2019$median_sale_price, method = 'spearman') # Significant

cor.test(as.numeric(es2019$period_begin), es2019$median_sale_price, method = 'spearman') # Non-Significant

cor.test(as.numeric(mid2019$period_begin), mid2019 $median_sale_price, method = 'spearman')# Significant

cor.test(as.numeric(bris2019$period_begin), bris2019$median_sale_price, method = 'spearman') # Significant

cor.test(as.numeric(ply2019$period_begin), ply2019$median_sale_price, method = 'spearman') # Significant



cor.test(as.numeric(nor2020$period_begin), nor2020$median_sale_price, method = 'spearman')# Significant

cor.test(as.numeric(su2020$period_begin), su2020$median_sale_price, method = 'spearman')# Non-Significant

cor.test(as.numeric(es2020$period_begin), es2020$median_sale_price, method = 'spearman')# Significant

cor.test(as.numeric(mid2020$period_begin), mid2020$median_sale_price, method = 'spearman')# Significant

cor.test(as.numeric(bris2020$period_begin), bris2020$median_sale_price, method = 'spearman')# Significant

cor.test(as.numeric(ply2020$period_begin), ply2020$median_sale_price, method = 'spearman')# Significant



# Average number of houses sold:




cor.test(as.numeric(nor2019$period_begin), nor2019$average_homes_sold, method = 'spearman')

cor.test(as.numeric(su2019$period_begin), su2019$average_homes_sold, method = 'spearman')

cor.test(as.numeric(es2019$period_begin), es2019$average_homes_sold, method = 'spearman')

cor.test(as.numeric(mid2019$period_begin), mid2019 $average_homes_sold, method = 'spearman')

cor.test(as.numeric(bris2019$period_begin), bris2019$average_homes_sold, method = 'spearman')

cor.test(as.numeric(ply2019$period_begin), ply2019$average_homes_sold, method = 'spearman')


cor.test(as.numeric(nor2020$period_begin), nor2020$average_homes_sold, method = 'spearman')

cor.test(as.numeric(su2020$period_begin), su2020$average_homes_sold, method = 'spearman')

cor.test(as.numeric(es2020$period_begin), es2020$average_homes_sold, method = 'spearman')

cor.test(as.numeric(mid2020$period_begin), mid2020$average_homes_sold, method = 'spearman')

cor.test(as.numeric(bris2020$period_begin), bris2020$average_homes_sold, method = 'spearman')

cor.test(as.numeric(ply2020$period_begin), ply2020$average_homes_sold, method = 'spearman')



# Average new listings:



cor.test(as.numeric(nor2019$period_begin), nor2019$average_new_listings, method = 'spearman')

cor.test(as.numeric(su2019$period_begin), su2019$average_new_listings, method = 'spearman')

cor.test(as.numeric(es2019$period_begin), es2019$average_new_listings, method = 'spearman')

cor.test(as.numeric(mid2019$period_begin), mid2019 $average_new_listings, method = 'spearman')

cor.test(as.numeric(bris2019$period_begin), bris2019$average_new_listings, method = 'spearman')

cor.test(as.numeric(ply2019$period_begin), ply2019$average_new_listings, method = 'spearman')


cor.test(as.numeric(nor2020$period_begin), nor2020$average_new_listings, method = 'spearman')

cor.test(as.numeric(su2020$period_begin), su2020$average_new_listings, method = 'spearman')

cor.test(as.numeric(es2020$period_begin), es2020$average_new_listings, method = 'spearman')

cor.test(as.numeric(mid2020$period_begin), mid2020$average_new_listings, method = 'spearman')

cor.test(as.numeric(bris2020$period_begin), bris2020$average_new_listings, method = 'spearman')

cor.test(as.numeric(ply2020$period_begin), ply2020$average_new_listings, method = 'spearman')






























































suff.2019 <- subset(df, Name == 'Suffolk' & format(df$period_begin,'%Y')=='2019')
suff.2019  <- suff.2019[order(suff.2019 $period_begin),]

suff.2020 <- subset(df, Name == 'Suffolk' & format(df$period_begin,'%Y')=='2020')
suff.2020  <- suff.2020[order(suff.2020 $period_begin),]



plot(suff.2019$period_begin , suff.2019$median_sale_price)

plot(suff.2020$period_begin , suff.2020$median_sale_price )


cor(as.numeric(suff.2019$period_begin) , suff.2019$median_sale_price)
cor(as.numeric(suff.2020$period_begin) , suff.2020$median_sale_price)



ply.2019 <- subset(df, Name == 'Plymouth' & format(df$period_begin,'%Y')=='2019')
ply.2019  <- ply.2019[order(ply.2019$period_begin),]
ply.2020 <- subset(df, Name == 'Plymouth' & format(df$period_begin,'%Y')=='2020')
ply.2020  <- ply.2020[order(ply.2020$period_begin),]

plot(ply.2019$period_begin , ply.2019$median_sale_price)

plot(as.numeric(ply.2020$period_begin) , ply.2020$median_sale_price)

cor(as.numeric(ply.2019$period_begin),  ply.2019$median_sale_price)
cor(as.numeric(ply.2020$period_begin),  ply.2020$median_sale_price)

# Stratification by #County#

#Norfolk

kruskal.test(average_new_listings ~ Year, data = nor)
kruskalmc(average_new_listings ~ Year, data = nor, probs = alpha.bonferroni.year) # for differences

#Suffolk

kruskal.test(average_new_listings ~ Year, data = su)
kruskalmc(average_new_listings ~ Year, data = su, probs = alpha.bonferroni.year)

#Essex

kruskal.test(average_new_listings ~ Year, data = es)
kruskalmc(average_new_listings ~ Year, data = es, probs = alpha.bonferroni.year)

#Middlesex

kruskal.test(average_new_listings ~ Year, data = mid)
kruskalmc(average_new_listings ~ Year, data = mid, probs = alpha.bonferroni.year)
#Bristol

kruskal.test(average_new_listings ~ Year, data = bris)
kruskalmc(average_new_listings ~ Year, data = bris, probs = alpha.bonferroni.year)

#Plymouth

kruskal.test(average_new_listings ~ Year, data = ply)
kruskalmc(average_new_listings ~ Year, data = ply, probs = alpha.bonferroni.year)


# consulted : https://www.stat.berkeley.edu/~s133/dates.html
#           https://stat.ethz.ch/pipermail/r-help/2011-September/289364.html
#           https://stackoverflow.com/questions/6246159/how-to-sort-a-data-frame-by-date/6246186
#           https://www.ling.upenn.edu/~joseff/rstudy/week2.html

