#read the data 

library(ggplot2)
library(dplyr)
library(tidyverse) 
library(lubridate)
library(plotly)
library(knitr)

data <- read.csv("NYC_complaints.csv")
head(data)

#printing dimension of the dataset

glimpse(data)


#dropping columns that are not in use
df <- data[ -c(2,6,7,9,10,11,13,15,17,18,19,20,23,27,31,32,36) ]
head(df)

#renaming the columns for better understanding
names(df) <- c("ID","Borough","Date","Time","Crime Status","Jurisdiction","Level of offense", "Offense", "Premise" , "Report Date", " Suspect age", "Suspect race","Suspect sex", "Victim age", " Victim race", " Victim sex","Latitude", "Longtitude", "Cordinates")
head(df)

#printing dimension of the new dataset
glimpse(df)

#formatting date
df$Date <- as.Date(as.character(df$Date), format = "%m/%d/%y")
df$date2 <- df$Date
df <- separate(df, col = date2, into = c("year","month","day"), sep ="-")

# replacing empty cells with "NOT REPORTED" as information is unknown to assign value
df <- df%>%mutate_if(is.character, list(~na_if(.,"")))
df[is.na(df)]<- "NOT REPORTED"
head(df)

# crime in each borough
cf <- ggplot(df, aes(x = Borough, fill=as.factor(Borough))) + geom_bar(width=0.9, stat="count") + theme(legend.position="none") + coord_flip()
print(cf)



#count of crimes per month and per day

mf <- ggplot(df, aes(x = month, fill=as.factor(month))) + geom_bar(width=0.9, stat="count") + theme(legend.position= "none")

dff <- ggplot(df, aes(x = day, fill=as.factor(day))) + geom_bar(width=0.9, stat="count") + theme(legend.position= "none")

grid.arrange(dff, mf)

#count of crimes per month by borough
mb <- ggplot(df, aes(x = month, fill=as.factor(Borough))) + geom_bar(width=0.9, stat="count") + theme(legend.position= "right")

db <- ggplot(df, aes(x = day, fill=as.factor(Borough))) + geom_bar(width=0.9, stat="count") + theme(legend.position= "right")

grid.arrange(db, mb)


# complaints per borough

cnt <-pie(table(df$`Borough`))
print(cnt)

#type of crime category

lc <- pie(table(df$`Level of offense`))
print(lc)

# departments solving crime per month
pd<-ggplot(data=df,aes(x=`month`,fill=`Jurisdiction`))+geom_histogram(stat="count")+ scale_fill_discrete(name="DEPARTMENT")
print(pd)

# count of each crime
oc <- ggplot(df, aes(x=`Offense`))+geom_histogram(stat ="count")+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
print(oc)

# Number of victims in each age group
ag <- table(df$`Victim age`)
# We can see that around 14 age group values do not align with reality, but this is a very small inconsistency comapred to the whole data. 
# But what is concering is the 116573 unknown values(almost 1/3 of the data), this will affect the accuracy. 

# Age group most likely to be a victim
ag[ag==max(ag)]

# converting time column from character to time stamp
df$Time <- as.POSIXct(df$Time, format = "%H:%M:%S")
df$Time <- format(df$Time, "%H:%M:%S")

# crimes happening between 12am to 6am and the count
df1 <- df %>% filter(Time < "06:00:00" & Time > "00:00:00")
print(df1)
print(nrow(df1))

# percentage
print(nrow(df1)*100/nrow(df))

# Murders happening between 12 am to 6am 
df2 <- df1 %>% filter(df1$`Offense` == "MURDER & NON-NEGL. MANSLAUGHTER")
head(df2)

print(nrow(df2))

# Murders happening by age groups between 12 am to 6am
mg<-ggplot(df2, aes(x =`Victim age`, fill=as.factor(`Victim age`))) + geom_bar(width=0.9, stat="count") + theme(legend.position="none") + coord_flip()
print(mg)

# scatterplot of the above crime's cordinates:
x<- df2$Latitude
y<- df2$Longtitude
sp<-plot( x,y, main = "scatter plot", xlab = "latitude", ylab = "longitude", pch = 19)
print(sp)

# street crimes
df3 <- df %>% filter(df$`Premise` == "STREET")
print(df3)
print(nrow(df3))

# percent of street crime
print(nrow(df3)*100/nrow(df))

# histogram of street crimes by borough
sb<-ggplot(df3, aes(x = `Borough`, fill=as.factor(`Borough`))) + geom_bar(width=0.9, stat="count") + theme(legend.position="none") + coord_flip()
print(sb)

#street crimes indicating the race of the victim 
sr<- ggplot(data=df3,aes(x=`Borough`,fill=` Victim race`))+geom_histogram(stat="count")+ scale_fill_discrete(name="RACE")+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
print(sr)

