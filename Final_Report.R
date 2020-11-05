#Asma Karedia, Gunner West, Noumik Thadani, Som Jadhav
#Group39

#change this line to reflect the path to the csv file on your own computer
etfs=read.csv("\\Users\\somja\\Documents\\College\\STA 371G\\ETFs_Updated_1.csv",row.names=1)
#etfs=read.csv("/Users/asmakaredia/Downloads/ETFs_Updated_1.csv",row.names=1)
#etfs=read.csv("\\Users\\Gunner\\Documents\\UTexas\\Classes\\(STA 371G) Statistics and Modeling\\Project\\ETFs_Updated_1.csv",row.names=1)
#etfs=read.csv("/Users/Noumik/Downloads/ETFs_Updated_1.csv",row.names=1)

#get rid of ETFs that don't fit into any investment type category
etfs_cleaned=etfs[!(etfs$investment==""),]

#want etfs which have somewhat even distribution of investments (industry wise)
etfs_cleaned=etfs_cleaned[!(etfs_cleaned$financial_services>80 | etfs_cleaned$technology>80 | etfs_cleaned$energy>80 
                            | etfs_cleaned$industrials>80 | etfs_cleaned$healthcare>80 | etfs_cleaned$consumer_cyclical>80
                            | etfs_cleaned$basic_materials>80 | etfs_cleaned$real_estate>80 | etfs_cleaned$consumer_defensive>80
                            | etfs_cleaned$utilities>80 | etfs_cleaned$communication_services>80),]

#get rid of extraneous columns
etfs_cleaned<-etfs_cleaned[,-c(2,12,15,16,18,19,24)]

#get rows with missing data
etfs_na=etfs_cleaned[!complete.cases(etfs_cleaned), ]

#replace missing values for net_assets and fund_yield with mean of columns
etfs_cleaned$net_assets[is.na(etfs_cleaned$net_assets)]<-mean(etfs_cleaned$net_assets, na.rm=T)
etfs_cleaned$fund_yield[is.na(etfs_cleaned$fund_yield)]<-mean(etfs_cleaned$fund_yield, na.rm=T)

#remove 3 rows with NA for every column
etfs_cleaned=na.omit(etfs_cleaned)

#collinearity test for P/E, P/B, P/CF
pairs(~etfs_cleaned$price_book+etfs_cleaned$price_cashflow+etfs_cleaned$price_earnings)
cor(etfs_cleaned$price_book,etfs_cleaned$price_earnings,use="complete.obs")
cor(etfs_cleaned$price_earnings,etfs_cleaned$price_cashflow,use="complete.obs")

#collinearity check for portfolio stocks vs. sector/industry allocations
cor(etfs_cleaned$portfolio_stocks,etfs_cleaned$financial_services)
cor(etfs_cleaned$portfolio_stocks,etfs_cleaned$consumer_cyclical)
cor(etfs_cleaned$portfolio_stocks,etfs_cleaned$healthcare)
cor(etfs_cleaned$portfolio_stocks,etfs_cleaned$technology)
cor(etfs_cleaned$portfolio_stocks,etfs_cleaned$energy)
cor(etfs_cleaned$portfolio_stocks,etfs_cleaned$industrials)

#change column names
names(etfs_cleaned)[6]<-"expense_ratio"
names(etfs_cleaned)[7]<-"pct_stocks"
names(etfs_cleaned)[12]<-"finance"
names(etfs_cleaned)[16]<-"tech"
names(etfs_cleaned)[17]<-"return_5_yr"
names(etfs_cleaned)[18]<-"beta_5_yr"
View(etfs_cleaned)

#regsubsets
#install leaps
install.packages("leaps")
library(leaps)

plot(regsubsets(return_5_yr~net_assets+fund_yield+investment+expense_ratio+pct_stocks+price_earnings
                +consumer_cyclical+Inverse,data=etfs_cleaned),scale="adjr2",ylab="Adjusted R Squared")
dev.copy(png,'Regsubsets.png')
dev.off()

sum <- lm(return_5_yr ~ investment + pct_stocks + consumer_cyclical + Inverse , data=etfs_cleaned)
summary(sum)

#Outliers
#Categorical variables cannot be searched for outliers
#Investment and Inverse will remain as they are
#Consumer Cyclical Outliers
boxplot(etfs_cleaned$consumer_cyclical , xlab="Consumer Cyclical")
summary(etfs_cleaned$consumer_cyclical)
dev.copy(png,'consumerCyclical.png')
dev.off()

lessthan13CC <- subset(etfs_cleaned,etfs_cleaned$consumer_cyclical < 13)
summary(lessthan13CC$consumer_cyclical)
boxplot(lessthan13CC$consumer_cyclical , xlab="Consumer Cyclical Without Outliers")
dev.copy(png,'consumerCyclical-noOutliers.png')
dev.off()

#Percent Stocks Outliers
boxplot(etfs_cleaned$pct_stocks , xlab="Percent of portfolio in stocks")
summary(etfs_cleaned$pct_stocks)
dev.copy(png,'percentStocks.png')
dev.off()


#Graphical and Numerical Summaries
#Graphical and numerical summary for y variable
boxplot(etfs_cleaned$return_5_yr, xlab="5 Year Return")
hist(etfs_cleaned$return_5_yr, col="grey")
summary(etfs_cleaned$return_5_yr)
mean(etfs_cleaned$return_5_yr)
sd(etfs_cleaned$return_5_yr)

#Graphical and numerical summaries for x variables
#Investments
plot(etfs_cleaned$investment, xlab="Investment Types", ylab="Number of Funds")
summary(etfs_cleaned$investment)

#Consumer Cyclical
boxplot(etfs_cleaned$consumer_cyclical , xlab="Consumer Cyclical")
summary(etfs_cleaned$consumer_cyclical)
mean(etfs_cleaned$consumer_cyclical)
sd(etfs_cleaned$consumer_cyclical)

#Pct Stocks
hist(etfs_cleaned$pct_stocks , xlab="Percent of portfolio in stocks", col="grey")
summary(etfs_cleaned$pct_stocks)
mean(etfs_cleaned$pct_stocks)
sd(etfs_cleaned$pct_stocks)

#Inverse
plot(etfs_cleaned$Inverse, xlab="Inverse ETF")
summary(etfs_cleaned$Inverse)

#Graphical and numerical summaries for each y~x
#Investments
plot(etfs_cleaned$investment, etfs_cleaned$return_5_yr, xlab="Investment Types" , ylab="Net Assets")
investment <- lm(return_5_yr ~ investment , data = etfs_cleaned)
summary(investment)
plot(investment)

#Consumer Cyclical
plot(etfs_cleaned$consumer_cyclical, etfs_cleaned$return_5_yr, xlab="Consumer Cyclical" , ylab="Net Assets")
plot(lessthan13CC$consumer_cyclical,etfs_cleaned$return_5_yr)
cyclical <- lm(return_5_yr ~ consumer_cyclical , data = etfs_cleaned)
summary(cyclical)
plot(cyclical)

#Pct Stocks
plot(etfs_cleaned$pct_stocks, etfs_cleaned$return_5_yr, xlab="Percentage Stocks" , ylab="Net Assests")
stocks <- lm(return_5_yr ~ pct_stocks , data = etfs_cleaned)
summary(stocks)
plot(stocks)

#Inverse
plot(etfs_cleaned$Inverse)
plot(etfs_cleaned$Inverse, etfs_cleaned$return_5_yr, xlab="Inverse" , ylab="Net Assets")
inverse <- lm(return_5_yr ~ Inverse , data = etfs_cleaned)
summary(inverse)
plot(inverse)

#Multiple Regression Model
final.model <- lm(return_5_yr ~ investment + consumer_cyclical + pct_stocks + Inverse, data = etfs_cleaned)
summary(final.model)
plot(predict(final.model), residuals(final.model))
plot(final.model)

#Prediction Example
predict(final.model, list(investment="Growth", consumer_cyclical=8, pct_stocks=90, Inverse="No"))

#CI for coefficents 
model1 <- lm(return_5_yr ~ Inverse + pct_stocks + consumer_cyclical + investment , data = etfs_cleaned )
confint(model1)