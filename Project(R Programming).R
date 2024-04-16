#Importing Libraries
library(dplyr)
library(ggplot2)
library(plotly)

#To check the current path
getwd()

#load the dataset
df = read.csv("insurance2.csv")
#In insurance file insuranceclaim column has 0 & 1 
#i.e 0 indicate customer Not Claiming the insurance
#    1 indicate customer Claiming the insurance

#To show the first 6 records
head(df)

#To show the last 6 records
tail(df)

#To show random records
sample_n(df,5)

#To check the number of rows and columns
dim(df)

#To show particular columns
head(df["insuranceclaim"])
head(df[c("age","insuranceclaim")])

#To show the record of above 40 age from given dataset
df[df$age>60,]

#To delete the column from existing dataset
#to create the copy of the existing dataset
df_copy = df
head(df_copy)
df_copy$sex=NULL
head(df_copy)

#To search the particular records from the dataset we use inbuilt function
#subset()
subset(df,age==25)
subset(df,children==4)

#Now use the inbuilt function of dplyr library
#1. select() : to select the particular columns from the given dataset
head(select(df,age))
head(select(df,age,sex,bmi,charges,insuranceclaim))

#To show all columns name from dataset
colnames(df)

#To show the columns start with particular alphabet using inbuilt function starts_with
head(select(df,starts_with("s")),10)

#To show the columns end with particualr alphabet using inbuilt function ends_with
head(select(df,ends_with("n")),10)

#2. filter() : to show those records according to given condition 
head(filter(df,age>60),10)
head(filter(df,charges>40000),10)

#To shows those record whose age is 30,40,50,60 using membership operator %in%
filter(df,age %in% c(30,40,50,60))

#To show those records whose age is 30 and gender is female(0)
filter(df,age==60 & sex==0)

#3. mutate() : to add new column into given dataset
head(df)
mutate(df,less_charges = charges-1000)   #Its temporary added column
head(mutate(df,less_charges = charges-1000),10)

#4. rename() : to change the columns existing name
head(rename(df,body.mass.index = bmi),10)   #but changes are temporary

#5. arrange() : this function is to arrange the records in ascending order according to particular column
arrange(df,age)   #but temporary
head(arrange(df,charges),20)   #but temporary
head(arrange(df,desc(charges)),20) #but temporary

#6. summarise() : this function perform the statistics calculation and gives the numerical column
summarise(df,Over.all.avg.charges = mean(charges))
summarise(df,median(charges))
summarise(df,Sum_charges = sum(charges),avg_charges = mean(charges),No.of.records = n())

#str() : to describe the structure of dataframe
str(df)

#7. group_by() : to check the number of records as per values
df %>% group_by(insuranceclaim)%>%summarise(No.of.records=n())

#duplicated() : to identify the duplicated elements on given dataset
duplicated(df)
#To check the number of duplicated rows in given dataset
sum(duplicated(df))

# distinct() : to removes the duplicated rows
df2 = distinct(df)
dim(df)
dim(df2)
#Cross check the duplicated values
sum(duplicated(df2))

head(df)


#Now using ggplot2 library visualize the dataset
#1. Create the bar graph to check the number of insuranceclaim
df_bar = df %>% group_by(insuranceclaim)%>%summarise(No.of.records=n())
df_bar
ggplot(data=df_bar ,mapping = aes(x=insuranceclaim,y=No.of.records))+ggtitle("Bar Graph")+
geom_bar(stat = "identity",fill="yellow")+geom_text(aes(label=No.of.records),vjust=-0.3,size=5)+
theme(plot.title = element_text(hjust=0.5))

ggplot(data=df_bar ,mapping = aes(x=insuranceclaim,y=No.of.records))+ggtitle("Bar Graph")+
geom_bar(stat = "identity",fill="yellow")+geom_text(aes(label=No.of.records),vjust=-0.5,size=5)+
theme(plot.title = element_text(hjust=0.5))+coord_flip()

#To check the number of children
df_bar2 = df %>% group_by(children)%>%summarise(No.of.records=n())
df_bar2
ggplot(data=df_bar2 ,mapping = aes(x=children,y=No.of.records))+ggtitle("Bar Graph")+
geom_bar(stat = "identity",fill="yellow")+geom_text(aes(label=No.of.records),vjust=-0.3,size=5)+
theme(plot.title = element_text(hjust=0.5))

ggplot(data=df_bar2 ,mapping = aes(x=children,y=No.of.records))+ggtitle("Bar Graph")+
  geom_bar(stat = "identity",fill="yellow")+geom_text(aes(label=No.of.records),vjust=-0.3,size=5)+
  theme(plot.title = element_text(hjust=0.5))+coord_flip()


#To check the number of region
df_bar3 = df %>% group_by(region) %>% summarise(No.of.records = n())
df_bar3
ggplot(data=df_bar3,mapping = aes(x=region,y=No.of.records))+ggtitle("Bar Graph")+
geom_bar(stat="identity",fill="yellow")+geom_text(aes(label=No.of.records),vjust=-0.3,size=5)+
theme(plot.title = element_text(hjust=0.5))


#2. create the box plot to check the outliers in the given dataset
ggplot(data=df,mapping = aes(x=age))+geom_boxplot()
ggplot(data=df,mapping = aes(x=bmi))+geom_boxplot()

ggplot(data=df,mapping = aes(x=bmi))+geom_boxplot()+ggtitle("Box Plot")+
theme(plot.title = element_text(hjust=0.5))

#Box plot with plotly library
bmi = df[["bmi"]]
plot_ly(data=df,x=bmi,type='box')


#3.To create the histogram 
ggplot(data = df,mapping = aes(x=children))+geom_histogram(col="red")

#data convert to vectors 
children = df[["children"]]
hist(children,col="red",border="blue")
head(df)
age = df[["age"]]
hist(age,col="red",border="yellow")


#4. To create the Scatter plot
ggplot(data=df,mapping=aes(x=charges,y=bmi))+geom_point(color="green",size=2)+
ggtitle("charges vs bmi")+xlab("Charges")+ylab("Body Mass Index")
#create the line 
ggplot(data=df,mapping=aes(x=charges,y=bmi))+geom_point(color="green",size=2)+
ggtitle("charges vs bmi")+xlab("Charges")+ylab("Body Mass Index")+
geom_line(color="red")


#5. To create the Line chart
ggplot(data=df,mapping = aes(x=charges,y=bmi))+ggtitle("Line Chart")+xlab("Charges")+
ylab("Body Mass Index")+geom_line(color="green")


#6. To create the Pie charts
pie = data.frame(df %>% group_by(children) %>% summarise(Total.num.of.child=n()))
pie 
ggplot(data=pie,mapping=aes(x=" ",y="Total.num.of.child",fill=children))+geom_col(color="black")+
geom_text(aes(label=Total.num.of.child),position=position_stack(vjust=0.5))+coord_polar("y")

#To convert the dataframe into vectors
Total.num.of.child = pie[["Total.num.of.child"]]
children = pie[["children"]]

pie(Total.num.of.child,labels=Total.num.of.child,col=c("red","green","yellow","blue","brown","purple"),main="Pie Chart")
legend("topright",legend=children,cex = 0.6,fill=rainbow(length(Total.num.of.child)),title="Number of Children")

pie(Total.num.of.child,labels=Total.num.of.child,col=rainbow(length(Total.num.of.child)),main="Pie Chart")
legend("topright",legend=children,title = "Number of Childrens",cex=0.6,fill=rainbow(length(Total.num.of.child)))



    