#load Libraries
library(gtools)
library(ggplot2)
library(sqldf)
library(packcircles)
library(ggplot2)
library(viridis)
library(hrbrthemes)
library(plotly)

#load data
wine_130k <- read.csv("C:\\Users\\Saum\\Desktop\\R Prog\\Project\\winemag-data-130k-v2\\winemag-data-130k-v2.csv", header=T, na.strings=c("","NA"))
wine_130k_snap=head(wine_130k,500)

wine_master1<-wine_130k
print(summary(wine_master1))
str(wine_master1)

wine<-wine_master1[!complete.cases(wine_master1),]




# Data summary

summary(wine)

# Distribution of Wine Reviews by Top 10 Countries
  
  #Take count of records for countries not equla to NA

  test<-sqldf("Select country,count(*) records           
  from wine
  where country is not NULL
  group by country")
  sqldf("select count(*) from wine where country is not NULL")
  (top_10_country<-head(sqldf("select country, records contri from test
                    order by records desc"),n=10))
  
  # Plot plotly graph for top 10 countries based on the count taken above 
  
  (p1 <- plot_ly(x = ~top_10_country$contri, y = ~reorder(top_10_country$country, top_10_country$contri),                             
                type = 'bar', orientation = 'h',
                marker = list(title="Top 10 countires (freq)",
                  color = 'rgba(50, 171, 96, 0.6)',
                              line = list(color = 'rgba(50, 171, 96, 1.0)', width = 1))) %>%
    
      layout(yaxis = list(showgrid = FALSE, showline = FALSE, showticklabels = TRUE, domain= c(0, 0.85),title=""),
           xaxis = list(zeroline = FALSE, showline = FALSE, showticklabels = TRUE, showgrid = TRUE,title=" Record Count")))
   
  
#top five and bottom 5 countries on the basis of average points
  #identify the top 5 countries
  top5_count_points<-head(sqldf("select country,avg(points) average_points
  from wine
  where country is not NULL
  group by country
  order by avg(points) desc"),n=5)
  
  #Plot the graph for top 5 using plotly
  (p2a <- plot_ly(x = ~top5_count_points$average_points, y = ~reorder(top5_count_points$country, top5_count_points$average_points),
                 type = 'bar', orientation = 'h',
                 marker = list(
                               color = top5_count_points$average_points
                               )) %>%
      
      layout(title="Top 5 countires (avg points)",
        yaxis = list(showgrid = FALSE, showline = FALSE, showticklabels = TRUE, domain= c(0, 0.85),title=""),
             xaxis = list(zeroline = FALSE, showline = FALSE, showticklabels = TRUE, showgrid = TRUE,title=" Average Points")))
  
  #identify the bottom 5 countries
  bottom5_count_points<-tail(sqldf("select country,avg(points) average_points
  from wine
  where country is not NULL
  group by country
  order by avg(points) desc"),n=5)
  #Plot graph for bottom 5 using plotly
  (p2b <- plot_ly(x = ~bottom5_count_points$average_points, y = ~reorder(bottom5_count_points$country, bottom5_count_points$average_points), name = 'Household savings, percentage of household disposable income',
                  type = 'bar', orientation = 'h',
                  marker = list(
                                color = bottom5_count_points$average_points
                                )) %>%
      
      layout(title="Bottom 5 countires (avg points)",yaxis = list(showgrid = FALSE, showline = FALSE, showticklabels = TRUE, domain= c(0, 0.85),title=""),
             xaxis = list(zeroline = FALSE, showline = FALSE, showticklabels = TRUE, showgrid = TRUE,title=" Average Points")))
  
# Top and bottom 5 countries on the basis of average price
  #identify the top 5 countries
  top5_count_price<-head(sqldf("select country,avg(price) average_price
  from wine
  where country is not NULL
  group by country
  order by avg(price) desc"),n=5)
  
  #Plot graph for top 5 countries using plotly
  (p2c <- plot_ly(x = ~top5_count_price$average_price, y = ~reorder(top5_count_price$country, top5_count_price$average_price),
                  type = 'bar', orientation = 'h',
                  marker = list(
                                color = top5_count_price$average_price)) %>%
      
      layout(title="Top 5 countires (avg price)",yaxis = list(showgrid = FALSE, showline = FALSE, showticklabels = TRUE, domain= c(0, 0.85),title=""),
             xaxis = list(zeroline = FALSE, showline = FALSE, showticklabels = TRUE, showgrid = TRUE,title=" Average Price")))
  
  #identify the bottom 5 countries
 bottom5_count_price<-tail(sqldf("select country,avg(price) average_price
  from wine
  where country is not NULL
  group by country
  order by avg(price) desc"),n=6)
 
 #Plot the graph for top 5 countries using plotly
  (p2d <- plot_ly(x = ~bottom5_count_price$average_price, y = ~reorder(bottom5_count_price$country, bottom5_count_price$average_price),
                  type = 'bar', orientation = 'h',
                  marker = list(
                                color = 'rgba(50, 171, 96, 0.6)',
                                line = list(color = 'rgba(50, 171, 96, 1.0)', width = 1))) %>%
      
      layout(title="Bottom 5 countires (avg price)",yaxis = list(showgrid = FALSE, showline = FALSE, showticklabels = TRUE, domain= c(0, 0.85),title=""),
             xaxis = list(zeroline = FALSE, showline = FALSE, showticklabels = TRUE, showgrid = TRUE,title=" Average Price")))
  

# Find correlation between price and rating (points) 

  
 (p3<- ggplot(wine, aes(points,price, color = points)) +
    geom_point(shape = 16, size = 5, show.legend = FALSE) +
    theme_minimal() +
    scale_color_gradient(low = "#0091ff", high = "#f0650e"))

# most rated and least rated wine
  #Find the most rated and least rated wine for each country
  (min_max_points<-sqldf("select country,max(points) Max_rated_wine,min(points) Least_rated_wine
  from wine
  where country is not NULL
  group by country"))
  
  par(mfrow=c(1,2), bg="grey")
  #Plot the graph for max rated wine for each country using plotly
  (p4a <- plot_ly(x = ~min_max_points$Max_rated_wine, y = ~reorder(min_max_points$country, min_max_points$Max_rated_wine), name = 'Maximum rated wine by country',
                 type = 'bar', orientation = 'h',
                 xlab=" ",
                 ylab=" ",
                 marker = list(color = min_max_points$Max_rated_wine
                               )) %>%
      layout(title="Maximum rated wine by company",
            yaxis = list(showgrid = FALSE, showline = FALSE, showticklabels = TRUE, domain= c(0, 0.85), title=" "),
             xaxis = list(zeroline = FALSE, showline = FALSE, showticklabels = TRUE, showgrid = TRUE,title="Maximum rated wine"))
  )
  
  #Plot the graph for least rated wine for each country using plotly
  (p4b <- plot_ly(x = ~min_max_points$Least_rated_wine, y = ~reorder(min_max_points$country, min_max_points$Least_rated_wine), name = 'Least rated wine by country',
                  type = 'bar', orientation = 'h',
                  xlab=" ",
                  ylab=" ",
                  marker = list(color = min_max_points$Least_rated_wine,
                                line = list(width = 1))) %>%
      layout(yaxis = list(showgrid = FALSE, showline = FALSE, showticklabels = TRUE, domain= c(0, 0.85), title=" "),
             xaxis = list(zeroline = FALSE, showline = FALSE, showticklabels = TRUE, showgrid = TRUE,title="Least rated wine"))
  )
  
#Create Tree map graph
    #load "treemap" library
    library(treemap)
    
  #Price tree grouped by country
    group <- wine$country
    value <- wine$price
    data <- data.frame(group,value)
    
   
    treemap(data,
            index="group",
            vSize="value",
            type="index"
    )
  
  #Data volume tree(recod count) grouped by variety
    #Find record count for each Variety
    variety_count<-sqldf("select Variety, count(*) count from wine where variety is not NULL group by variety")
    group2 <- variety_count$variety
    value2 <- variety_count$count
    data3 <- data.frame(group2,value2)
    
    
    treemap(data3,
            index="group2",
            vSize="value2",
            type="index"
    )
  
  # Price tree grouped by Country, Variety
    subgroup <- wine$variety
  
    (data2 <- data.frame(group,subgroup,value))
    
    # Custom labels:
    treemap(data2, index=c("group","subgroup"),     vSize="value", type="index",
            
            fontsize.labels=c(15,12),
            fontcolor.labels=c("white","orange"),
            fontface.labels=c(2,1),
            bg.labels=c("transparent"),
            align.labels=list(
              c("center", "center"),
              c("right", "bottom")
              ),
            overlap.labels=0.5,
            inflate.labels=F,
            )
# Bubblechart - Top 5 varieties based on average points, identify their country and average price  
  
  #find average points and price for each variety within each country
  Country_variety_metric<-head(sqldf("select Country, variety, avg(points) Points, avg(price) Price from wine 
                                     where country is not NULL AND Variety is not NULL
                                     group by Country,variety
                                     order by (avg(points)) desc"),n=5)
  
  #Plot bubble chart for the Country_variety_metric data frame
  ggplot(Country_variety_metric, aes(x=variety, y=Points, size = Price, fill=country)) +
      geom_point(alpha=0.5, shape=21, color="black")+
      scale_size(name="Price ($)")

# Bubblechart - Top 5 varieties based on average points, identify their province and average price  
  
  #find average points and price for each variety within each province
  Province_variety_metric<-head(sqldf("select variety, province, avg(points) Points, avg(price) Price from wine \
                                     where province is not NULL AND winery is not NULL
                                     group by variety,province
                                     order by (avg(points)) desc"),n=5)
  #Plot bubble chart for the Province_variety_metric data frame
  ggplot(Province_variety_metric, aes(x=variety, y=Points, size = Price, fill=province)) +
      geom_point(alpha=0.5, shape=21, color="black")+
      scale_size(name="Price ($)")
