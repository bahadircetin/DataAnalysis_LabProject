#Seyit Bahadir Cetin Lab Project

#Reading data from file into a dataframe. 
book.data <- read.csv("prog_book.csv", header = TRUE)

#Clear the NA values to get clean data. 
clean.book.data <- na.omit(book.data)
library(dplyr)

#Selecting the columns that we are going to use by using select() function.
books.selected <- clean.book.data %>% select(Book_title, Type, Number_Of_Pages, Rating, Price) 

#Adding new rating column which converts ratings to over 100 points instead of 5 by using mutate() function.
books.selected <- books.selected %>% mutate(Rating_over_100=Rating*100/5)

#Adding new columns: max,average,min we will find max, average, and min price of every type of book by using group_by() and summarise() functions.
books.summarised <- books.selected %>% group_by(Type) %>% summarise(Max_price=max(Price),Avg_Price=mean(Price), Min_Price=min(Price))

#Arranging our data by Rating_over_100 and Price

books.arranged <- books.selected %>% arrange(Price, Rating_over_100)

#Showing the most 5 expensive and the cheapest programming books by using top_n() function.

books.selected %>% top_n(5, Price)

books.selected %>% top_n(5, desc(Price))


#Creating graphs by using ggplot2.
#Scatter plot
library(ggplot2)
graph1 <- ggplot(books.selected, aes(x =Price , y = Rating_over_100)) 
graph1+geom_point(aes(color = Type,size=Price))+ 
  geom_smooth(method="loess", se=F)+
  labs(subtitle="Price Vs Rating", 
       y="Rating", 
       x="Price of Book", 
       title="Scatterplot", 
       caption = "Source: Kaggle")


#Pie Chart
pie <- ggplot(books.selected, aes(x = "", fill = factor(Type))) + 
  geom_bar(width = 1) +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  labs(fill="Book Types", 
       x=NULL, 
       y=NULL, 
       title="Pie Chart of Book Types", 
       caption="Source: Kaggle")

pie + coord_polar(theta = "y", start=0)

#Box plot
library(ggthemes)
library(ggridges)
g <- ggplot(books.selected, aes(Price, Number_Of_Pages))
g + geom_boxplot(aes(fill=factor(Type))) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  ylim(c(50, 1000))+
  labs(title="Box plot", 
       subtitle="Number of pages grouped by cover type of book",
       caption="Source: Kaggle",
       x="Price",
       y="Number of Pages")

#Density Plot
ggplot(books.selected, aes(x = Price, y = Type, fill = Type)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")

#Seyit Bahadir Cetin Lab Project








