# wines<-read.csv('winemag-data-130k-v2.csv',stringsAsFactors=F)
# common_words <- unlist(str_split(readLines("common_words_and_bigrams.txt"), pattern = ", "))
# common_words = as.list(common_words)
# 
# for (i in (1:length(wines$description))){
#   wines$description[i] = tolower(wines$description[i])
# }
# 
# wines$description=strsplit(wines$description, "\\, |\\,| ")
# #wines_test$description = toLower(wines_test$description)
# 
# common_word_filter <- function(x){
#   lst = as.list(x)[[1]]
#   for (i in (1:length(lst))){
#     ifelse(lst[[i]] %in% common_words, return(lst[[i]]), return(''))
#   }
# }
# 
# wines$description = sapply(wines$description, common_word_filter)



library(shiny)
library(shinydashboard)
library(dplyr)
library(data.table)
library(DT)
library(dqshiny)


#library(rsconnect)
#rsconnect::setAccountInfo(name='rahulmadhu', token='434A990AB46F10E0321CCDE9D5837EF4', secret='IHla+UTKa0q+u5J1D8V3WCDzPaqM3wU8XVE3dpdQ')
# wnames <- wines$title
# wines1<-read.csv('winemag-data-130k-v2.csv')
# 
# wines1 <- wines1[!(wines1$country == "Nan"), ]
# wines1 <- wines1[!(wines1$province == "Nan"), ]
# wines1$description<-as.character(wines1$description)
# wines1$price<-as.integer(wines1$price)
# wines1$points<-as.integer(wines1$points)
# cpv<-wines1[,c("title","description","country","province","variety","points","price")]
# cpv<-cpv[complete.cases(cpv),]


ui<-shinyUI({
  dashboardPage(skin = "red",
                dashboardHeader(title= "Wine Recommender"),
                dashboardSidebar(
                  sidebarMenu(
                    id="tabs",
                    menuItem("Aficionado", tabName = "af",icon = icon("wine-glass-alt")),
                    menuItem("Sommelier", tabName = "co",icon = icon("wine-glass")),
                    menuItem("Know more", tabName = "km",icon = icon("book-open")),
                    menuItem("About us", tabName = "au",icon = icon("users"))
                  )
                ),
                dashboardBody(
                  tags$head(tags$style(HTML('@import url("//fonts.googleapis.com/css?family=Lobster|Cabin:400,700");
        .main-header .logo {
        font-family: "Lobster", cursive;
        font-size: 25px;
        }
        .main-sidebar { 
        font-family: Verdana, Geneva, sans-serif;
        font-size: 18px;
        font-weight: bold;
                              }'))),
                  tabItems(
                    # First tab content
                    tabItem(tabName = "af",
                            box(
                              title = span(icon("heart"),HTML('&nbsp;'),"Select your favorite wines:"),
                              icon = icon("heart"),
                              status = "danger",
                              autocomplete_input("w1", "First Choice", wnames, value = "", max_options = 10),
                              autocomplete_input("w2", "Second Choice", wnames, value = "", max_options = 10),
                              autocomplete_input("w3", "Third Choice", wnames, value = "", max_options = 10),
                              autocomplete_input("w4", "Fourth Choice", wnames, value = "", max_options = 10),
                              autocomplete_input("w5", "Fifth Choice", wnames, value = "", max_options = 10)
                            ),
                            box(
                              status = "danger",
                              h1("You must try..",style="font-size:20px"),
                              verbatimTextOutput("aff")
                              #textOutput("aff")
                              #DT::dataTableOutput("aff")
                            )
                            
                    ),
                    
                    # Second tab content
                    tabItem(tabName = "co",
                            
                            fluidRow(
                              column(
                                width=3,
                                box(
                                  status = "danger",
                                  width=NULL,
                                  htmlOutput("country"),
                                  htmlOutput("province"),
                                  htmlOutput("variety")
                                ),
                                box(
                                  status = "danger",
                                  width=NULL,
                                  htmlOutput("price"),
                                  htmlOutput("points")
                                )
                                
                              ),
                              column(
                                width=8,
                                box(
                                  status = "danger",
                                  width = NULL, height="80%", DT::dataTableOutput("wine_recom")
                                )
                              )
                            )
                            
                            
                    ),
                    
                    
                    # third tab content
                    tabItem(tabName = "km",
                            fluidPage(
                              
                              box(
                                status = "danger",
                                textInput("t1","Name of the Wine:"),
                                actionButton("search","Search")
                              ),
                              box(
                                status = "danger",
                                h1("Description",style="font-size:20px"),
                                textOutput("d")
                              ),
                              
                              box(
                                status = "danger",
                                h1("Location Information",style="font-size:20px"),
                                h1("Country",style="font-size:15px;font-weight: bold"),
                                textOutput("c"),
                                h1("Province",style="font-size:15px;font-weight: bold"),
                                textOutput("pro"),
                                h1("Designation",style="font-size:15px;font-weight: bold"),
                                textOutput("des"),
                                h1("Winery",style="font-size:15px;font-weight: bold"),
                                textOutput("win")
                              )  ,
                              
                              infoBoxOutput("pr"),
                              infoBoxOutput("po"),
                              box(
                                background = "red",
                                h1("Variety",style="font-size:20px"),
                                textOutput("v")
                              )  
                            )
                    ),
                    
                    # About us tab content
                    tabItem(tabName = "au",
                            fluidPage(
                              h2(HTML("<br>Team CrosstRainers<br>"),style="font-size:25px"),
                              
                              column(2,
                                     img(src="https://i.postimg.cc/PvmTPFV5/deepankar.png", height='200', width='140'),
                                     h3("Deepankar Singh", align="center")
                              ),
                              column(2,
                                     img(src="https://i.postimg.cc/XpknWmy2/keerthi.png", height='200', width='140'),
                                     h3("Keerthi Pullela", align="center")
                              ),
                              column(2,
                                     img(src="https://i.postimg.cc/mPZBgjYT/rahul.png", height='200', width='140'),
                                     h3("Rahul Madhu", align="center")
                              ),
                              column(2,
                                     img(src="https://i.postimg.cc/JGLR9MtD/saumya.png", height='200', width='140'),
                                     h3("Saumya Bharti
                                                  ", align="center")
                              ),
                              
                              column(2,
                                     img(src="https://i.postimg.cc/4ntJLm4R/soujanya.png", height='200', width='140'),
                                     h3("Soujanya Samineni", align="center")
                              )
                              
                            ),
                            h2(HTML("<br><br>MS Business Analytics and Information Management students graduating in May 2020"),style="font-size:25px"),
                            h2("Krannert School of Management",style="font-size:25px"),
                            h2("Purdue University",style="font-size:25px")
                    )
                  )
                )
  )
  
})

server <- shinyServer(function(input, output,session) {
  
  
  #output for afficianado tab
  wine_list<- reactive({
    as.list(c(input$w1,input$w2,input$w3,input$w4,input$w5))
  })
  
  
  t2<-function(wine_list) {
    for (i in (1:length(wine_list))) {
      
      x<-(wines$description[wines$title==wine_list[i]])
      
      b<-wines%>% filter(wines$title!=wine_list)
      c<-(b$title[b$description==x[1]])
      
    }
    return(c[1:5])
  }
  
  
  output$aff <- renderPrint(
    print(t2(wine_list()))
  )
  # output$aff <- DT::renderDataTable(
  #   DT::datatable( 
  #     data = t1(wine_list())
  #   ) 
  # )
  
  
  #dynamic filters for Sommelier tab
  output$country <- renderUI({
    
    selectInput(
      inputId = "country", 
      label = "Country",
      choices = as.character(unique(cpv$country)),
      selected = "Portugal")
    
  })
  
  output$province <- renderUI({
    
    available <- cpv[cpv$country == input$country, "province"]
    
    selectInput(
      inputId = "province", 
      label = "Province",
      choices = unique(available),
      selected = unique(available)[1])
    
  })
  
  output$variety <- renderUI({
    
    available <- cpv[cpv$province == input$province, "variety"]
    
    selectInput(
      inputId = "variety", 
      label = "Variety",
      choices = unique(available),
      selected = unique(available)[1])
    
  })
  
  output$price <- renderUI({
    
    available2 <- cpv[cpv$variety == input$variety, "price"]
    
    sliderInput("price", "Maximum Price", min(available2), max(available2),
                value = available2[1])
    
  })
  
  output$points <- renderUI({
    
    available1 <- cpv[cpv$price <= input$price, "points"]
    
    sliderInput("points", "Maximum Points", min(available1), max(available1),
                value = available1[1])
    
  })
  
  #output for sommelier tab
  
  wine <- reactive({
    wines %>% filter (country == input$country) %>% filter (province == input$province) %>% filter(variety == input$variety) %>% filter(price < input$price) %>% filter(points < input$points)  
  })
  
  
  output$wine_recom <- DT::renderDataTable(
    DT::datatable( 
      data = wine() %>% select(title, price, points) %>% arrange(desc(points))
    ) 
  )
  
  #output for know more tab
  observeEvent( input$search,{
    
    a <- as.character(cpv[cpv$title == input$t1,"description"])
    b <- wines[wines$title == input$t1,"price"]
    c <- wines[wines$title == input$t1,"points"]
    d <- as.character(wines[wines$title == input$t1,"country"])
    e <- as.character(wines[wines$title == input$t1,"province"])
    f <- as.character(wines[wines$title == input$t1,"designation"])
    g <-as.character(wines[wines$title == input$t1,"winery"])
    h <- as.character(wines[wines$title == input$t1,"variety"])
    output$d <- renderText(a)
    output$c <- renderText(d)
    output$pro <- renderText(e)
    output$des <- renderText(f)
    output$win <- renderText(g)
    output$v <- renderText(h)
    
    output$po <- renderInfoBox({
      
      infoBox(value = c, title = 'Points', color = "yellow",icon = icon("award"))
    })
    
    output$pr <- renderInfoBox({
      
      infoBox(value = b, title = 'Price', color = "green",icon = icon("dollar-sign"))
    })
    
  })
  
  
})

shinyApp(ui = ui, server = server)
# 
# wine_list = c('Nicosia 2013 VulkÃ  Bianco  (Etna)','Quinta dos Avidagos 2011 Avidagos Red (Douro)')
#new_wine_list = as.data.frame(x=c('','','','',''))
# 
# t1<-function(wine_list) {
#   for (i in (1:length(wine_list))) {
#     
#     x<-(wines$description[wines$title==wine_list[i]])
#     
#     b<-wines%>% filter(wines$title!=wine_list)
#     c<-(b$title[b$description==x[1]])
#     
#   }
#   return(c[1:5])
# }
# 
# s<-c("Quinta dos Avidagos 2011 Avidagos Red (Douro)","Rainstorm 2013 Pinot Gris (Willamette Valley)","St. Julian 2013 Reserve Late Harvest Riesling (Lake Michigan Shore)","Sweet Cheeks 2012 Vintner's Reserve Wild Child Block Pinot Noir (Willamette Valley)","Tandem 2011 Ars In Vitro Tempranillo-Merlot (Navarra)")
# 
# t1(s)
