#TEAM 1
#Leen Daqa
#Noor Yazeed
#Sana Murad
#Rand Alshoubaki

#uploading the library needed 
library(tidyverse)
library(e1071)
library(reshape)
library(pacman)
p_load('patchwork')
#importing data
df=read.csv("C:/Users/leend/Desktop/Data Visualization/Edit1.csv")

##############checking for nulls and data type
colnames(df)[1]<-"Quantity"

#Removing duplicates
df=distinct(df)

#fixing discrete
df$Price=as.numeric(df$Price)

#fixing categorical 
df$Holiday=as.factor(df$Holiday)
df$orderDate_quarter=as.factor(df$orderDate_quarter)
df$promotionDate_quarter=as.factor(df$promotionDate_quarter)

#Exploration
str(df)
summary(df)
unique(df$RoleDesc)
unique(df$EmpType)
unique(df$Customer_Type)
df_numeric
unique(df$WH)
unique(df$PaymentMethod)
unique(df$DEPARTMENT)
unique(df$PromotionType)

#created new data frame with only numeric variables for easier access
df_numeric=df[,unlist(lapply(df,is.numeric))]
col=colnames(df_numeric)
df_long=melt(df_numeric,value=c(col))
#box plot to check for outliers 
g=ggplot(data=df_long, aes(x=value))
g+geom_boxplot(fill = "#F5B7B1", color = "#CD5C5C")+facet_wrap(~variable,scales='free')+labs(title="Distrbution of numerical variables ")+
  (fun = "mean", colour = "red", size = 2, geom = "box")
##only quantity and city population have outliers 
g=ggplot(data=df_long,aes(x=value))
g+geom_histogram(bins=10,color="black", fill="lightpink")+facet_wrap(~variable,scales='free')+labs(title="Distrbution of numerical variables ")
#get skewness for numeric variables
skewness(df$Quantity) ##moderately right skewed
skewness(df$Price)    ##slightly right skewed,could be symmetrical
skewness(df$Rate)     ##slightly right skewed,could be symmetrical
skewness(df$customer_age)  ##slightly right skewed,could be symmetrical
skewness(df$City_Population) ##highly right skewed
skewness(df$City_Density.people.km2.)  ##moderately right skewed
skewness(df$employee_age)  ##slightly left skewed,could be symmetrical
skewness(df$employee_months_working) ##slightly right skewed,could be symmetrical
#Kurtosis
# measures how sharp the peak of a histogram is 
# if ~=3 the peak is sharp as the number goes further away
# from 3 the peak becomes flatter
kurtosis(df$Quantity) #far away from three there for it is flat
kurtosis(df$Price)   #it is flat no peak

kurtosis(df$Rate)   #it is flat no peak 
kurtosis(df$customer_age)  #it is flat no peak
kurtosis(df$City_Population) #it is flat no peak
kurtosis(df$City_Density.people.km2.) # flat
kurtosis(df$employee_age)  #it is flat no peak
kurtosis(df$employee_months_working) #it is flat no peak
#######################################
round(max(df$Quantity)) 
round(max(df$Price)) 
round(max(df$Rate)) 
round(max(df$customer_age))
round(max(df$City_Population)) 
round(max(df$City_Density.people.km2.)) 
round(max(df$employee_age))  
round(max(df$employee_months_working))
#####################################
round(mean(df$Quantity))
round(mean(df$Price))  
round(mean(df$Rate))  
round(mean(df$customer_age))  
round(mean(df$City_Population))
round(mean(df$City_Density.people.km2.))
round(mean(df$employee_age)) 
round(mean(df$employee_months_working))
##############################
round(min(df$Quantity))
round(min(df$Price))  
round(min(df$Rate))  
round(min(df$customer_age))  
round(min(df$City_Population))
round(min(df$City_Density.people.km2.))
round(min(df$employee_age)) 
round(min(df$employee_months_working))

######################################
#visualization
g=ggplot(data=df,aes(x=DEPARTMENT,y=Price,fill=DEPARTMENT))+
  geom_bar(stat='identity')+
  facet_wrap(~Holiday)+labs(title="relationship between Department , Price & Holiday",
                            x ="Department", y = "Price",caption = "Note  1 represent for Holiday while 0 represent the rest days of the year")+
  theme(legend.position="none")
plot(g)
####################################################

df_boxplot= df[c('Holiday', 'DEPARTMENT', 'Price')]
df_boxplot= as.data.frame(df_boxplot)
df_boxplot= melt(df_boxplot , id= c('DEPARTMENT','Holiday'), value=c('Price'))
pp = ggplot(data = df, aes(x='variable', y='value'))
pp + geom_boxplot()+ stat_summary(fun="mean")+facet_wrap(~variable )
df_boxplot

g=ggplot(data=df,aes(x='price',y='holiday'))+geom_boxplot()+facet_wrap(~DEPARTMENT)
plot(g)
##Women and kids sell the most in general, more sales on non-holidays
g=ggplot(data=df,aes(x=Price,fill=Customer_Type))+geom_histogram(bins=6)+facet_wrap(~Customer_Type)
plot(g)
##No correlation between price,rate and customer type
g=ggplot(data=df,aes(x=Price,y=City_Density.people.km2.))+geom_point()
plot(g)
##no correlation between price and city density
g=ggplot(data=df,aes(x=Price,y=Quantity))+geom_point(color='blue')+theme_classic()
plot(g)
##no correlation even when faceted on multiple features
g=ggplot(data= df,aes(x=Quantity,y=Rate,color=customer_gender))+
  geom_point()+theme_classic()
plot(g)
##no correlation between rate and quantity 
g1=ggplot(data=df,aes(x=orderDate_quarter,fill=PromotionType))+geom_bar(position=position_dodge())+ theme(legend.position="none")
plot(g1)
g2=ggplot(data=df,aes(x=orderDate_quarter,fill=PromotionType))+geom_bar()
plot(g2)
g1+g2
##not much difference in the number of promotions in each quarter the most 
# common promotion is giveaways
g1=ggplot(data=df,aes(x=Branch_Location,fill=customer_City))+geom_bar()
plot(g)
g2=ggplot(data=df,aes(x=Branch_Location,fill=customer_City))+geom_bar(position=position_dodge())
plot(g)
g1+g2
##most people use the online branch , most customers live in cities in the west
g3=ggplot(data=df,aes(x=Branch_Name,fill=PaymentMethod))+geom_bar()
plot(g3)
g4=ggplot(data=df,aes(x=Branch_Name,fill=PaymentMethod))+geom_bar(position=position_dodge())
plot(g4)
g3+g4
#most customers go to branch F, no one payment method is more commonly used than another
g4=ggplot(data=df,aes(x=Price,y=orderDate_quarter))+geom_bar(stat='identity',color="lightblue")+coord_flip()
plot(g4)

#the price is not affected by the quarter when it is was ordered
g5=ggplot(data=df,aes(x=Price,y=promotionDate_quarter))+geom_bar(stat='identity',color="lightblue")+coord_flip()
plot(g5)
g4+g5
#the price is not affected by the quarter when there was a promotion

g=ggplot(data=df,aes(x=Price,y=customer_City))+geom_bar(stat='identity',color="lightblue")+coord_flip()
plot(g)
##Prices are the highest in the West region while they are the cheapest in the South
install.packages("timetk")
g=ggplot(data=df,aes(x= PaymentMethod,fill=customer_City))+geom_bar()
plot(g)

##g=ggplot(data=df,aes(x=DEPARTMENT,y=customer_age,fill="lightpink"))+stat_summary(fun.data = mean_sdl,geom = "bar")+ theme(legend.position="none")
##plot(g)

df1=aggregate(Quantity~DEPARTMENT,df,mean)
g=ggplot(data=df1,aes(x="",y="Quantity",fill=DEPARTMENT,label=round(Quantity)))
g=g+geom_col()
g+coord_polar(theta='y',start=0)+geom_text(position=position_stack(vjust=0.5))


df1

library(lubridate)
library(timetk)

#months
#we want to see relationship between time and the paymentMethod
ts1.month=ts( data =(df$PaymentMethod),star=2021, frequency = 13)
ts1.month
plot(ts1.month)

################################################
df5= df[c('DEPARTMENT','customer_gender','Quantity')]
df5=as.data.frame(df5)

data5= melt(df5, id=c('DEPARTMENT','customer_gender'), value=c('Quantity'))
g=ggplot(data = data5, aes(x= "", y=value, fill=DEPARTMENT, label=round(Quantity))) +  
           geom_bar(stat='identity') + 
           coord_polar(theta = 'y', start=0)+
           geom_text(position = position_stack(vjust = 0.5))+
           facet_wrap(DEPARTMENT~customer_gender)
plot(g)
##########################################         
g1=ggplot(data=df,aes(x=PromotionType,y= City_Population))+geom_point()
plot(g1)
df
############################

gg = ggplot(data = df, aes(sample=DEPARTMENT))
gg+geom_qq()+ geom_qq_line()

#########ERROR BAR ############
se <- function(x)
{
  return (sd(x)/sqrt(length(x)))
  
}
mean_data = aggregate(Rate ~ DEPARTMENT, df, mean)
se_data = aggregate(Rate ~ DEPARTMENT, df, se)
error_data=cbind(mean_data, se_data['Rate'])
colnames(error_data)=c('DEPARTMENT', 'mean', 'se')
t= ggplot(error_data, aes(x=DEPARTMENT, y=mean, fill=DEPARTMENT)) 

t + geom_col() + 
  geom_errorbar(aes(ymin = mean-se, ymax= mean+se), width=0.9, color='black') + labs(title = 'Displaying Error Bar for Tax Rate according to each Department', x = 'Department', y = 'Tax Rate' ) + theme(legend.position="none")
########################################

###########PIE CHART###################
df1=aggregate(customer_age~DEPARTMENT,df,mean)
g=ggplot(data=df,aes(x="",y="customer_age",fill=DEPARTMENT,label=round(customer_age)))
g=g+geom_col()
g+coord_polar(theta='y',start=0)+geom_text(position=position_stack(vjust=0.5))
############################