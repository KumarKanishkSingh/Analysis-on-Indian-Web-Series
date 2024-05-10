#libraries needed
library(shiny)
library(ggplot2)
library(reshape)
library(tidyverse)
library(plotly)
library(GGally)
library(shinythemes)

#loading the final data frame from the working directory
load("finaldata.Rdata")

#==================================================================================================

#defining ui part of the app
ui <- navbarPage("Visualizing Indian Web series Data",
                 
                 #selecting theme for the app
                 theme = shinytheme("sandstone"),
                 
                 #creating different section for the app 
                 #first section of the app
                 tabPanel("Overall Visualisations", 
                          
                          #section heading
                          titlePanel("Overall Visualisations"),
                          
                          #defining layout of the section
                          sidebarLayout(
                            
                            #sidebarpanel of inputs
                            sidebarPanel(
                              
                              #drop down box for choosing platforms
                              selectInput("platfrom",
                                          h3("Select Platform : "),
                                          choices = c("All", unique(data$Platform)),
                                          selected = "All" 
                              ),           
                              
                              #drop down box for selecting the field of comparison
                              selectInput("question",
                                          h3("View plots based on:"),
                                          choices = list("Select" = 1,
                                                         "Certificate of Web series" = 2, 
                                                         "Genre of Web series" = 3,
                                                         "Length of Web series" = 4)
                              ),
                              
                              #conditional panel when chosen the first field of comparison
                              conditionalPanel(condition = "input.question == 2",
                                               
                                               hr(),
                                               
                                               #radio input of mode of comparison
                                               radioButtons("radio2", label = h4("Show comparison with respect to:"),
                                                            choices = list("Ratings" = 1, "Votes" = 2), 
                                                            selected = 0),
                                               
                                               #conditional panel when first mode of comaprison is chosen
                                               conditionalPanel(condition = "input.radio2 == 1",
                                                                
                                                                hr(),
                                                                
                                                                #check box to show gender wise classification
                                                                checkboxInput(inputId = "mof1",
                                                                              label = h4("Compare with respect to gender"),
                                                                              value = FALSE)
                                                                
                                               ),
                                               
                                               #conditional panel when second mode of comaprison is chosen
                                               conditionalPanel(condition = "input.radio2 == 2",
                                                               
                                                                hr(),
                                                                
                                                                #check box to show gender wise classification
                                                                checkboxInput(inputId = "mof2",
                                                                              label = h4("Compare with respect to gender"),
                                                                              value = FALSE)
                                                                
                                               )
                                               
                              ),
                              
                              #conditional panel when chosen the second field of comparison
                              conditionalPanel(condition = "input.question == 3",
                                               
                                               hr(),
                                               
                                               #radio input of mode of comparison
                                               radioButtons("radio3", label = h4("Show comparison with respect to:"),
                                                            choices = list("Ratings" = 1, "Votes" = 2), 
                                                            selected = 0),
                                               
                                               #conditional panel when first mode of comaprison is chosen
                                               conditionalPanel(condition = "input.radio3 == 1",
                                                                
                                                                hr(),
                                                                
                                                                #check box to show gender wise classification
                                                                checkboxInput(inputId = "mof3",
                                                                              label = h4("Compare with respect to gender"),
                                                                              value = FALSE)
                                                                
                                               ),
                                               
                                               #conditional panel when second mode of comaprison is chosen
                                               conditionalPanel(condition = "input.radio3 == 2",
                                                                
                                                                hr(),
                                                                
                                                                #check box to show gender wise classification
                                                                checkboxInput(inputId = "mof4",
                                                                              label = h4("Compare with respect to gender"),
                                                                              value = FALSE)
                                                                
                                               )
                                               
                              ),
                              
                              #conditional panel when chosen the third field of comparison
                              conditionalPanel(condition = "input.question == 4",
                                               
                                               hr(),
                                               
                                               #radio input of mode of comparison
                                               radioButtons("radio4", label = h4("Show comparison with respect to:"),
                                                            choices = list("Ratings" = 1, "Votes" = 2), 
                                                            selected = 0),
                                               
                                               #conditional panel when first mode of comaprison is chosen
                                               conditionalPanel(condition = "input.radio4 == 1",
                                                                
                                                                hr(),
                                                                
                                                                #check box to show gender wise classification
                                                                checkboxInput(inputId = "mof5",
                                                                              label = h4("Compare with respect to gender"),
                                                                              value = FALSE)
                                                                
                                               ),
                                               
                                               #conditional panel when second mode of comaprison is chosen
                                               conditionalPanel(condition = "input.radio4 == 2",

                                                                hr(),
                                                                
                                                                #check box to show gender wise classification
                                                                checkboxInput(inputId = "mof6",
                                                                              label = h4("Compare with respect to gender"),
                                                                              value = FALSE)
                                                                
                                               )
                                               
                              )
                              
                              
                               
                              
                            ),
                            
                            #mainpanel for outputs
                            mainPanel(
                              
                              #conditional panel for showing plot of certificate category w.r.t ratings  
                              conditionalPanel(condition = "input.question == 2 & input.radio2 == 1",
                                               plotOutput("distPlot1")
                              ),
                              
                              #conditional panel for showing plot of certificate category w.r.t ratings with gender classification 
                              conditionalPanel(condition = "input.question == 2 & input.radio2 == 1 & input.mof1 == true",
                                               hr(),
                                               plotOutput("distPlot2")
                              ),
                              
                              #conditional panel for showing plot of certificate category w.r.t votes
                              conditionalPanel(condition = "input.question == 2 & input.radio2 == 2",
                                               plotOutput("distPlot3")
                              ),
                              
                              #conditional panel for showing plot of certificate category w.r.t votes with gender classification 
                              conditionalPanel(condition = "input.question == 2 & input.radio2 == 2 & input.mof2 == true",
                                               hr(),
                                               plotOutput("distPlot4")
                              ),
                              
                              #conditional panel for showing plot of genre w.r.t ratings
                              conditionalPanel(condition = "input.question == 3 & input.radio3 == 1",
                                               plotOutput("distPlot5")
                              ),
                              
                              #conditional panel for showing plot of genre w.r.t ratings with gender classification 
                              conditionalPanel(condition = "input.question == 3 & input.radio3 == 1 & input.mof3 == true",
                                               hr(),
                                               plotOutput("distPlot6")
                              ),
                              
                              #conditional panel for showing plot of genre w.r.t votes
                              conditionalPanel(condition = "input.question == 3 & input.radio3 == 2",
                                               plotOutput("distPlot7")
                              ),
                              
                              #conditional panel for showing plot of genre w.r.t votes with gender classification 
                              conditionalPanel(condition = "input.question == 3 & input.radio3 == 2 & input.mof4 == true",
                                               hr(),
                                               plotOutput("distPlot8")
                              ),
                              
                              #conditional panel for showing plot of length category w.r.t. ratings
                              conditionalPanel(condition = "input.question == 4 & input.radio4 == 1",
                                               plotOutput("distPlot9")
                              ),
                              
                              #conditional panel for showing plot of length category w.r.t. ratings with gender classification 
                              conditionalPanel(condition = "input.question == 4 & input.radio4 == 1 & input.mof5 == true",
                                               hr(),
                                               plotOutput("distPlot10")
                              ),
                              
                              #conditional panel for showing plot of length category w.r.t. votes
                              conditionalPanel(condition = "input.question == 4 & input.radio4 == 2",
                                               plotOutput("distPlot11")
                              ),
                              
                              #conditional panel for showing plot of length category w.r.t. votes with gender classification 
                              conditionalPanel(condition = "input.question == 4 & input.radio4 == 2 & input.mof6 == true",
                                               hr(),
                                               plotOutput("distPlot12")
                              )
                              
                              
                            )
                          )
                 ),
                 
                 #second section of the app
                 tabPanel("Platform Wise Comparison",
                      
                          #section heading
                          titlePanel("Platform Wise Comparison"),
                          
                          #defining layout of the section
                          sidebarLayout(
                            
                            #sidebarpanel of inputs
                            sidebarPanel(
                              
                              #drop down box to choose the field of comparison
                              selectInput("comptype",
                                          h3("Compare platforms with respect to :"),
                                          choices = list("Select" = 1,
                                                         "Runtime" = 2, 
                                                         "Age Group" = 3,
                                                         "Genre" = 4)  
                                          
                              ),
                              
                              #conditional panel when chosen the first field of comparison
                              conditionalPanel(condition = "input.comptype == 2",
                                               
                                               hr(),
                                               
                                               #checkbox for choosing platforms (multiple inputs)
                                               checkboxGroupInput("plats1", label = "Select Platforms :", 
                                                                  choices = unique(data$Platform),
                                                                  selected = "Alt Balaji")
                                               
                              ),
                              
                              #conditional panel when chosen the second field of comparison
                              conditionalPanel(condition = "input.comptype == 3",
                                               
                                               hr(),
                                               
                                               #checkbox for choosing platforms (multiple inputs)
                                               checkboxGroupInput("plats2", label = "Select Platforms :", 
                                                                  choices = unique(data$Platform),
                                                                  selected = "Alt Balaji"),
                                               
                              ),
                              
                              #conditional panel when chosen the third field of comparison
                              conditionalPanel(condition = "input.comptype == 4",
                                               
                                               hr(),
                                               
                                               #checkbox for choosing platforms (multiple inputs)
                                               checkboxGroupInput("plats3", label = "Select Platforms :", 
                                                                  choices = c("Alt Balaji", "Amazon Prime", "MX Player",    
                                                                              "Sony LIV", "Voot Select", "Zee5"),
                                                                  selected = "Alt Balaji"),
                              )
                              
                              
                            ),
                            
                            #mainpanel for outputs
                            mainPanel(
                              
                              #conditional panel for showing the boxplot to compare run time fo different platforms 
                              conditionalPanel(condition = "input.comptype == 2",
                                               plotOutput("distPlot13")
                              ),
                              
                              #conditional panel for showing percentage bar plot to compare age distribution of different platforms
                              conditionalPanel(condition = "input.comptype == 3",
                                               hr(),
                                               plotOutput("distPlot14")
                              ),
                              
                              #conditional panel for showing multiple donut chart to compare genre distribution of different platforms
                              conditionalPanel(condition = "input.comptype == 4",
                                               plotOutput("distPlot15")
                              )
                              
                              
                            )
                          )
                          
                 ),
                 
                 #third section of the app
                 tabPanel("Finding Association",
                          
                          #section heading
                          titlePanel("Finding Association"),
                          
                          #defining layout of the section
                          sidebarLayout(
                            
                            #sidebarpanel of inputs
                            sidebarPanel(
                              
                              #drop down box to choose the field of comparison
                              selectInput("asstype",
                                          h3("Choose the type of comparison :"),
                                          choices = list("Select" = 1,
                                                         "Ratings vs Votes" = 2,
                                                         "Runtime vs Votes and Ratings" = 3)  
                                          
                              ),
                              
                              #conditional panel when chosen the first field of comparison
                              conditionalPanel(condition = "input.asstype == 2",
                                               
                                               hr(),
                                               
                                               #checkbox for choosing platforms (multiple inputs)
                                               checkboxGroupInput("plats4", label = "Select Platfroms to compare :", 
                                                                  choices = c("Alt Balaji", "Hotstar", "MX Player",    
                                                                              "Sony LIV", "Voot Select", "Zee5", "TVF"),
                                                                  selected = "Alt Balaji"),
                                               
                                               #slider for selecting the amount of outlier to be trimmed
                                               sliderInput("slider", label = "Select percentage of outliers to be trimmed :",
                                                           min = 0, max = 30, value = 10)
                                               
                              ),
                              
                              #conditional panel when chosen the second field of comparison
                              conditionalPanel(condition = "input.asstype == 3",
                                               
                                               hr(),
                                               
                                               #checkbox for choosing platforms (multiple inputs)
                                               checkboxGroupInput("plats5", label = "Select Platfroms to compare :", 
                                                                  choices = c("Alt Balaji", "Hotstar", "MX Player",    
                                                                              "Sony LIV", "Voot Select", "Zee5", "TVF"),
                                                                  selected = "Alt Balaji")
                                               
                              )
                              
                              
                            ),
                            
                            #mainpanel for outputs
                            mainPanel(
                              
                              #conditional panel for showing scatterplot of votes vs ratings
                              conditionalPanel(condition = "input.asstype == 2",
                                               plotlyOutput("distPlot16")
                              ),
                              
                              #conditional panel for showing multiple variable scatterplot to compare run time with votes and ratings
                              conditionalPanel(condition = "input.asstype == 3",
                                               hr(),
                                               plotOutput("distPlot17")
                              )
                              
                              
                            )
                          )
                          
                 )
                                   
                                   
         )


#defining server part of the app
server <- function(input, output) {
  
  
  #plot for certificate, wrt ratings
  output$distPlot1 <- renderPlot({
    
    #refining data set according to the chosen platform
    if (input$platfrom == "All")
    {
      dat <- data
    }
    else 
    {
      dat <- subset(data, data$Platform %in% input$platfrom)
    }
    
    #removing rows with NA values
    dat <- na.omit(dat)
    
    #getting unique certificate categories
    cer <- unique(dat$`Certificate Category`)
    
    #initiating blank vector for scores of different certificate categories
    score <- numeric(length = length(cer))
    
    #assinging score to the certificate categories
    for (i in 1:length(cer))
    {
      
      a <- subset(dat, dat$`Certificate Category` == cer[i])
      
      score[i] <- mean(a$`Overall Rating`)
    }
    
    #defining dataframe for plotting using ggplot
    df1 <- data.frame(cer, score)
    
    #arranging the categories according to our desired arrangement
    f.df1 <- df1
    f.df1$cer <- factor(f.df1$cer,                                    
                        levels = c("Family and Kids", "Teenagers", "Adults"))
    
    #ggplot
    ggplot(data=f.df1, aes(x = cer, y = score)) +
      geom_bar(stat="identity", fill="darkorchid") + 
      ylim(0, 10) +
      labs(title = "Bar Diagram", 
           subtitle = "Comparing Ratings across Certificate Categories", 
           x = "Certificate Category", y = "Ratings") + 
      theme(plot.title = element_text(size = 20), 
            plot.subtitle = element_text(size = 15), 
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 13))
             
    
  })
  
  #plot for certificate, wrt ratings with Gender classification
  output$distPlot2 <- renderPlot({
    
    #refining data set according to the chosen platform
    if (input$platfrom == "All")
    {
      dat <- data
    }
    else 
    {
      dat <- subset(data, data$Platform %in% input$platfrom)
    }
    
    #removing rows with NA values
    dat <- na.omit(dat)
    
    #getting unique certificate categories
    cer <- unique(dat$`Certificate Category`)
    
    #initiating blank vector for scores of different certificate categories for males and females
    Male <- numeric(length = length(cer))
    Female <- numeric(length = length(cer))
    
    #assinging score to the certificate categories
    for (i in 1:length(cer))
    {
      
      a <- subset(dat, dat$`Certificate Category` == cer[i])
      Male[i] <- mean(a$`Male Rating`)
      Female[i] <- mean(a$`Female Rating`)
    }
    
    #defining dataframe for plotting using ggplot
    df2 <- data.frame(Male, Female, cer)
    
    #arranging the categories according to our desired arrangement
    f.df2 <- df2
    f.df2$cer <- factor(f.df2$cer,                                    
                        levels = c("Family and Kids", "Teenagers", "Adults"))
    
    #creating suitable data frame for comparing gender wise
    dum <- melt(f.df2, id.vars='cer')
    colnames(dum) <- c("cer", "Gender", "value")
    
    #ggplot
    ggplot(dum, aes(x=cer, y=value, fill=Gender)) +
      geom_bar(stat='identity', position='dodge')  +
      ylim(0, 10) +
      labs(title = "Side-by-side Bar Diagram", 
           subtitle = "Comparison of male and female ratings for different Certificate Categories", 
           x = "Certificate Category", y = "Ratings") + 
      theme(plot.title = element_text(size = 20), 
            plot.subtitle = element_text(size = 15), 
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 13),
            legend.title=element_text(size=14), 
            legend.text=element_text(size=13))
    
    
    
  })
  
  
  #plot for certificate, wrt votes
  output$distPlot3 <- renderPlot({
    
    #refining data set according to the chosen platform
    if (input$platfrom == "All")
    {
      dat <- data
    }
    else 
    {
      dat <- subset(data, data$Platform %in% input$platfrom)
    }
    
    #removing rows with NA values
    dat <- na.omit(dat)
    
    #getting unique certificate categories
    cer <- unique(dat$`Certificate Category`)
    
    #initiating blank vector for scores of different certificate categories
    score <- numeric(length = length(cer))
    
    #assinging score to the certificate categories
    for (i in 1:length(cer))
    {
      
      a <- subset(dat, dat$`Certificate Category` == cer[i])
      
      score[i] <- mean(a$`Overall Votes`)
    }
    
    #defining dataframe for plotting using ggplot
    df3 <- data.frame(cer, score)
    
    #arranging the categories according to our desired arrangement
    f.df3 <- df3
    f.df3$cer <- factor(f.df3$cer,                                    
                        levels = c("Family and Kids", "Teenagers", "Adults"))
    
    #ggplot
    ggplot(data=f.df3, aes(x = cer, y = score)) +
      geom_bar(stat="identity", fill="darkorchid") + 
      labs(title = "Bar Diagram", 
           subtitle = "Comparing Overall Votes across Certificate Categories", 
           x = "Certificate Category", y = "Overall Votes") + 
      theme(plot.title = element_text(size = 20), 
            plot.subtitle = element_text(size = 15), 
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 13))
    
  })
  
  
  #plot for certificate, wrt votes with Gender classification
  output$distPlot4 <- renderPlot({
    
    #refining data set according to the chosen platform
    if (input$platfrom == "All")
    {
      dat <- data
    }
    else 
    {
      dat <- subset(data, data$Platform %in% input$platfrom)
    }
    
    #removing rows with NA values
    dat <- na.omit(dat)
    
    #getting unique certificate categories
    cer <- unique(dat$`Certificate Category`)
    
    #initiating blank vector for scores of different certificate categories for males and females
    Male <- numeric(length = length(cer))
    Female <- numeric(length = length(cer))
    
    #assinging score to the certificate categories
    for (i in 1:length(cer))
    {
      
      a <- subset(dat, dat$`Certificate Category` == cer[i])
      Male[i] <- mean(a$`Male Votes`)
      Female[i] <- mean(a$`Female Votes`)
    }
    
    #defining dataframe for plotting using ggplot
    df4 <- data.frame(Male, Female, cer)
    
    #arranging the categories according to our desired arrangement
    f.df4 <- df4
    f.df4$cer <- factor(f.df4$cer,                                    
                        levels = c("Family and Kids", "Teenagers", "Adults"))
    
    #creating suitable data frame for comparing gender wise
    dum <- melt(f.df4, id.vars='cer')
    colnames(dum) <- c("cer", "Gender", "value")
    
    #ggplot
    ggplot(dum, aes(x=cer, y=value, fill=Gender)) +
      geom_bar(stat='identity', position='dodge')  + 
      labs(title = "Side-by-side Bar Diagram", 
           subtitle = "Comparison of male and female votes for different Certificate Categories", 
           x = "Certificate Category", y = "Overall Votes") + 
      theme(plot.title = element_text(size = 20), 
            plot.subtitle = element_text(size = 15), 
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 13),
            legend.title=element_text(size=14), 
            legend.text=element_text(size=13))
    
    
  })
  
  
  #plot for genre, wrt ratings
  output$distPlot5 <- renderPlot({
    
    #refining data set according to the chosen platform defining the limit for a genre to have appeared that many times to be shown in plot
    if (input$platfrom == "All")
    {
      data1 <- data
      limit <- 7
    }
    else if (input$platfrom %in% c("TVF", "Hotstar"))
    {
      data1 <- subset(data, data$Platform %in% input$platfrom)
      limit <- 0
    }
    else if (input$platfrom %in% "Amazon Prime")
    {
      data1 <- subset(data, data$Platform %in% input$platfrom)
      limit <- 1
    }
    else 
    {
      data1 <- subset(data, data$Platform %in% input$platfrom)
      limit <- 2
    }
    
    #removing rows with NA values
    data1 <- na.omit(data1)
    
    #splitting 'genre' columns to consider all genres of a webseries
    split.g <- str_split(data1$Genre, ",")
    
    #creating dataframe of all the genres and the webseries identification from where it comes
    f.vector <- vector()
    
    index <- 0
    for (i in 1:dim(data1)[1])
    {
      len <- length(split.g[[i]])
      for (j in 1:len){
        f.vector[index + 1] <- i
        f.vector[index + 2] <- str_trim(split.g[[i]][j])
        index = index + 2
      }
    }
    
    d.gen <- data.frame( f.vector[seq(2, length(f.vector), by = 2)],
                         as.numeric(f.vector[seq(1, length(f.vector), by = 2)]),
                         stringsAsFactors = F)
    colnames(d.gen) <- c("Genre", "Web_no")
    
    #finding unique genres in the above data frame
    u.gen <- unique(d.gen$Genre)
    
    #initiating blank vector for scores of different certificate categories
    score <- numeric(length = length(u.gen))
    
    #assinging score to the genres
    for (i in 1:length(u.gen))
    {
      index <- which(d.gen$Genre == u.gen[i])
      index <- d.gen$Web_no[index]
      score[i] <- mean(data1$`Overall Rating`[index])
    }
    
    #selecting only the genres which appears more than 'limit'  number of times
    sel.gen <- vector()
    f.score <- numeric()
    index <- 1
    
    for (i in 1:length(u.gen))
    {
      if (sum(d.gen$Genre == u.gen[i]) > limit)
      {
        sel.gen[index] <- u.gen[i]
        f.score[index] <- score[i]
        index <- index + 1
      }
    }
    
    #defining dataframe for plotting using ggplot
    df5 <- data.frame(sel.gen, f.score)
    
    #ggplot
    ggplot(data=df5, aes(x = sel.gen, y = f.score)) +
      geom_bar(stat="identity", fill="darkorchid") + 
      ylim(0, 10) +
      labs(title = "Bar Diagram", 
           subtitle = "Comparing Ratings across popular Genres", 
           x = "Genre", y = "Ratings") + 
      theme(plot.title = element_text(size = 20), 
            plot.subtitle = element_text(size = 15), 
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 13))
    
    
  })
  
  
  #plot for genre, wrt ratings with Gender classification
  output$distPlot6 <- renderPlot({
    
    #refining data set according to the chosen platform defining the limit for a genre to have appeared that many times to be shown in plot
    if (input$platfrom == "All")
    {
      data1 <- data
      limit <- 7
    }
    else if (input$platfrom %in% c("TVF", "Hotstar"))
    {
      data1 <- subset(data, data$Platform %in% input$platfrom)
      limit <- 0
    }
    else if (input$platfrom %in% "Amazon Prime")
    {
      data1 <- subset(data, data$Platform %in% input$platfrom)
      limit <- 1
    }
    else 
    {
      data1 <- subset(data, data$Platform %in% input$platfrom)
      limit <- 2
    }
    
    #removing rows with NA values
    data1 <- na.omit(data1)
    
    #splitting 'genre' columns to consider all genres of a webseries
    split.g <- str_split(data1$Genre, ",")
    
    #creating dataframe of all the genres and the webseries identification from where it comes
    f.vector <- vector()
    
    index <- 0
    for (i in 1:dim(data1)[1])
    {
      len <- length(split.g[[i]])
      for (j in 1:len){
        f.vector[index + 1] <- i
        f.vector[index + 2] <- str_trim(split.g[[i]][j])
        index = index + 2
      }
    }
    
    d.gen <- data.frame( f.vector[seq(2, length(f.vector), by = 2)],
                         as.numeric(f.vector[seq(1, length(f.vector), by = 2)]),
                         stringsAsFactors = F)
    colnames(d.gen) <- c("Genre", "Web_no")
    
    #finding unique genres in the above data frame
    u.gen <- unique(d.gen$Genre)
    
    #initiating blank vector for scores of different certificate categories for males and females
    score1 <- numeric(length = length(u.gen))
    score2 <- numeric(length = length(u.gen))
    
    #assinging score to the genres
    for (i in 1:length(u.gen))
    {
      index <- which(d.gen$Genre == u.gen[i])
      index <- d.gen$Web_no[index]
      score1[i] <- mean(data1$`Male Rating`[index])
      score2[i] <- mean(data1$`Female Rating`[index])
    }
    
    #selecting only the genres which appears more than 'limit'  number of times
    sel.gen <- vector()
    Male <- numeric()
    Female <- numeric()
    index <- 1
    
    for (i in 1:length(u.gen))
    {
      if (sum(d.gen$Genre == u.gen[i]) > limit)
      {
        sel.gen[index] <- u.gen[i]
        Male[index] <- score1[i]
        Female[index] <- score2[i]
        index <- index + 1
      }
    }
    
    #defining dataframe for plotting using ggplot
    df6 <- data.frame(Male, Female, sel.gen)
    
    #creating suitable data frame for comparing gender wise
    dum <- melt(df6, id.vars='sel.gen')
    colnames(dum) <- c("gen", "Gender", "value")
    
    #ggplot
    ggplot(dum, aes(x=gen, y=value, fill=Gender)) +
      geom_bar(stat='identity', position='dodge')  +
      ylim(0, 10) +
      labs(title = "Side-by-side Bar Diagram", 
           subtitle = "Comparison of male and female ratings across popular Genres", 
           x = "Genre", y = "Ratings") + 
      theme(plot.title = element_text(size = 20), 
            plot.subtitle = element_text(size = 15), 
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 13),
            legend.title=element_text(size=14), 
            legend.text=element_text(size=13))
    
    
  })
  
  
  #plot for genre, wrt votes
  output$distPlot7 <- renderPlot({
    
    #refining data set according to the chosen platform defining the limit for a genre to have appeared that many times to be shown in plot
    if (input$platfrom == "All")
    {
      data1 <- data
      limit <- 7
    }
    else if (input$platfrom %in% c("TVF", "Hotstar"))
    {
      data1 <- subset(data, data$Platform %in% input$platfrom)
      limit <- 0
    }
    else if (input$platfrom %in% "Amazon Prime")
    {
      data1 <- subset(data, data$Platform %in% input$platfrom)
      limit <- 1
    }
    else 
    {
      data1 <- subset(data, data$Platform %in% input$platfrom)
      limit <- 2
    }
    
    #removing rows with NA values
    data1 <- na.omit(data1)
    
    #splitting 'genre' columns to consider all genres of a webseries
    split.g <- str_split(data1$Genre, ",")
    
    #creating dataframe of all the genres and the webseries identification from where it comes
    f.vector <- vector()
    
    index <- 0
    for (i in 1:dim(data1)[1])
    {
      len <- length(split.g[[i]])
      for (j in 1:len){
        f.vector[index + 1] <- i
        f.vector[index + 2] <- str_trim(split.g[[i]][j])
        index = index + 2
      }
    }
    
    d.gen <- data.frame( f.vector[seq(2, length(f.vector), by = 2)],
                         as.numeric(f.vector[seq(1, length(f.vector), by = 2)]),
                         stringsAsFactors = F)
    colnames(d.gen) <- c("Genre", "Web_no")
    
    #finding unique genres in the above data frame
    u.gen <- unique(d.gen$Genre)
    
    #initiating blank vector for scores of different certificate categories
    score <- numeric(length = length(u.gen))
    
    #assinging score to the genres
    for (i in 1:length(u.gen))
    {
      index <- which(d.gen$Genre == u.gen[i])
      index <- d.gen$Web_no[index]
      score[i] <- mean(data1$`Overall Votes`[index])
    }
    
    #selecting only the genres which appears more than 'limit'  number of times
    sel.gen <- vector()
    f.score <- numeric()
    index <- 1
    
    for (i in 1:length(u.gen))
    {
      if (sum(d.gen$Genre == u.gen[i]) > limit)
      {
        sel.gen[index] <- u.gen[i]
        f.score[index] <- score[i]
        index <- index + 1
      }
    }
    
    #defining dataframe for plotting using ggplot
    df7 <- data.frame(sel.gen, f.score)
    
    #ggplot
    ggplot(data=df7, aes(x = sel.gen, y = f.score)) +
      geom_bar(stat="identity", fill="darkorchid") +
      labs(title = "Bar Diagram", 
           subtitle = "Comparing Overall Votes across popular Genres", 
           x = "Genre", y = "Votes") + 
      theme(plot.title = element_text(size = 20), 
            plot.subtitle = element_text(size = 15), 
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 13))
    
    
  })
  
  
  #plot for genre, wrt genre with Gender classification
  output$distPlot8 <- renderPlot({
    
    #refining data set according to the chosen platform and defining the limit for a genre to have appeared that many times to be shown in plot
    if (input$platfrom == "All")
    {
      data1 <- data
      limit <- 7
    }
    else if (input$platfrom %in% c("TVF", "Hotstar"))
    {
      data1 <- subset(data, data$Platform %in% input$platfrom)
      limit <- 0
    }
    else if (input$platfrom %in% "Amazon Prime")
    {
      data1 <- subset(data, data$Platform %in% input$platfrom)
      limit <- 1
    }
    else 
    {
      data1 <- subset(data, data$Platform %in% input$platfrom)
      limit <- 2
    }
    
    #removing rows with NA values
    data1 <- na.omit(data1)
    
    #splitting 'genre' columns to consider all genres of a webseries
    split.g <- str_split(data1$Genre, ",")
    
    #creating dataframe of all the genres and the webseries identification from where it comes
    f.vector <- vector()
    
    index <- 0
    for (i in 1:dim(data1)[1])
    {
      len <- length(split.g[[i]])
      for (j in 1:len){
        f.vector[index + 1] <- i
        f.vector[index + 2] <- str_trim(split.g[[i]][j])
        index = index + 2
      }
    }
    
    d.gen <- data.frame( f.vector[seq(2, length(f.vector), by = 2)],
                         as.numeric(f.vector[seq(1, length(f.vector), by = 2)]),
                         stringsAsFactors = F)
    colnames(d.gen) <- c("Genre", "Web_no")
    
    #finding unique genres in the above data frame
    u.gen <- unique(d.gen$Genre)
    
    #initiating blank vector for scores of different certificate categories for males and females
    score1 <- numeric(length = length(u.gen))
    score2 <- numeric(length = length(u.gen))
    
    #assinging score to the genres
    for (i in 1:length(u.gen))
    {
      index <- which(d.gen$Genre == u.gen[i])
      index <- d.gen$Web_no[index]
      score1[i] <- mean(data1$`Male Votes`[index])
      score2[i] <- mean(data1$`Female Votes`[index])
    }
    
    #selecting only the genres which appears more than 'limit'  number of times
    sel.gen <- vector()
    Male <- numeric()
    Female <- numeric()
    index <- 1
    
    for (i in 1:length(u.gen))
    {
      if (sum(d.gen$Genre == u.gen[i]) > limit)
      {
        sel.gen[index] <- u.gen[i]
        Male[index] <- score1[i]
        Female[index] <- score2[i]
        index <- index + 1
      }
    }
    
    #defining dataframe for plotting using ggplot
    df8 <- data.frame(Male, Female, sel.gen)
    
    #creating suitable data frame for comparing gender wise
    dum <- melt(df8, id.vars='sel.gen')
    colnames(dum) <- c("gen", "Gender", "value")
    
    #ggplot
    ggplot(dum, aes(x=gen, y=value, fill=Gender)) +
      geom_bar(stat='identity', position='dodge')  +
      labs(title = "Side-by-side Bar Diagram", 
           subtitle = "Comparison of male and female votes across popular Genres", 
           x = "Genre", y = "Votes") + 
      theme(plot.title = element_text(size = 20), 
            plot.subtitle = element_text(size = 15), 
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 13),
            legend.title=element_text(size=14), 
            legend.text=element_text(size=13))
    
    
    
  })
  
  
  #plot for runtime, wrt ratings
  output$distPlot9 <- renderPlot({
    
    #refining data set according to the chosen platform
    if (input$platfrom == "All")
    {
      dat <- data
    }
    else 
    {
      dat <- subset(data, data$Platform %in% input$platfrom)
    }
    
    #removing rows with NA values
    dat <- na.omit(dat)
    
    #getting unique length categories
    len <- unique(dat$`Length Category`)
    
    #initiating blank vector for scores of different length categories
    score <- numeric(length = length(len))
    
    #assigning score to the length categories
    for (i in 1:length(len))
    {
      
      a <- subset(dat, dat$`Length Category` == len[i])
      
      score[i] <- mean(a$`Overall Rating`)
    }
    
    #defining dataframe for plotting using ggplot
    df9 <- data.frame(len, score)
    
    #arranging the categories according to our desired arrangement
    f.df9 <- df9
    f.df9$len <- factor(f.df9$len,                                    
                        levels = c("Short", "Medium", "Long"))
    
    #ggplot
    ggplot(data=f.df9, aes(x = len, y = score)) +
      geom_bar(stat="identity", fill="darkorchid") + 
      ylim(0, 10) +
      labs(title = "Bar Diagram", 
           subtitle = "Comparing Ratings across Web series Length", 
           x = "Length Category", y = "Ratings") + 
      theme(plot.title = element_text(size = 20), 
            plot.subtitle = element_text(size = 15), 
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 13))
    
    
  })
  
  #plot for runtime, wrt ratings with Gender classification
  output$distPlot10 <- renderPlot({
    
    #refining data set according to the chosen platform
    if (input$platfrom == "All")
    {
      dat <- data
    }
    else 
    {
      dat <- subset(data, data$Platform %in% input$platfrom)
    }
    
    #removing rows with NA values
    dat <- na.omit(dat)
    
    #getting unique length categories
    len <- unique(dat$`Length Category`)
    
    #initiating blank vector for scores of different length categories for males and females
    Male <- numeric(length = length(len))
    Female <- numeric(length = length(len))
    
    #assigning score to the length categories
    for (i in 1:length(len))
    {
      
      a <- subset(dat, dat$`Length Category` == len[i])
      Male[i] <- mean(a$`Male Rating`)
      Female[i] <- mean(a$`Female Rating`)
    }
    
    #defining dataframe for plotting using ggplot
    df10 <- data.frame(Male, Female, len)
    
    #arranging the categories according to our desired arrangement
    f.df10 <- df10
    f.df10$len <- factor(f.df10$len,                                    
                        levels = c("Short", "Medium", "Long"))
    
    #creating suitable data frame for comparing gender wise
    dum <- melt(f.df10, id.vars='len')
    colnames(dum) <- c("len", "Gender", "value")
    
    #ggplot
    ggplot(dum, aes(x=len, y=value, fill=Gender)) +
      geom_bar(stat='identity', position='dodge')  +
      ylim(0, 10) +
      labs(title = "Side-by-side Bar Diagram", 
           subtitle = "Comparison of male and female ratings for different Lengths", 
           x = "Length Category", y = "Ratings") + 
      theme(plot.title = element_text(size = 20), 
            plot.subtitle = element_text(size = 15), 
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 13),
            legend.title=element_text(size=14), 
            legend.text=element_text(size=13))
    
    
    
  })
  
  
  #plot for runtime, wrt votes
  output$distPlot11 <- renderPlot({
    
    #refining data set according to the chosen platform
    if (input$platfrom == "All")
    {
      dat <- data
    }
    else 
    {
      dat <- subset(data, data$Platform %in% input$platfrom)
    }
    
    #removing rows with NA values
    dat <- na.omit(dat)
    
    #getting unique length categories
    len <- unique(dat$`Length Category`)
    
    #initiating blank vector for scores of different length categories
    score <- numeric(length = length(len))
    
    #assigning score to the length categories
    for (i in 1:length(len))
    {
      
      a <- subset(dat, dat$`Length Category` == len[i])
      
      score[i] <- mean(a$`Overall Votes`)
    }
    
    #defining dataframe for plotting using ggplot
    df11 <- data.frame(len, score)
    
    #arranging the categories according to our desired arrangement
    f.df11 <- df11
    f.df11$len <- factor(f.df11$len,                                    
                         levels = c("Short", "Medium", "Long"))
    
    #ggplot
    ggplot(data=f.df11, aes(x = len, y = score)) +
      geom_bar(stat="identity", fill="darkorchid") + 
      labs(title = "Bar Diagram", 
           subtitle = "Comparing Overall Votes across Web series Length", 
           x = "Length Category", y = "Overall Votes") + 
      theme(plot.title = element_text(size = 20), 
            plot.subtitle = element_text(size = 15), 
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 13))
    
  })
  
  
  #plot for runtime, wrt votes with Gender classification
  output$distPlot12 <- renderPlot({
    
    #refining data set according to the chosen platform
    if (input$platfrom == "All")
    {
      dat <- data
    }
    else 
    {
      dat <- subset(data, data$Platform %in% input$platfrom)
    }
    
    #removing rows with NA values
    dat <- na.omit(dat)
    
    #getting unique length categories
    len <- unique(dat$`Length Category`)
    
    #initiating blank vector for scores of different length categories for males and females
    Male <- numeric(length = length(len))
    Female <- numeric(length = length(len))
    
    #assigning score to the length categories
    for (i in 1:length(len))
    {
      
      a <- subset(dat, dat$`Length Category` == len[i])
      Male[i] <- mean(a$`Male Votes`)
      Female[i] <- mean(a$`Female Votes`)
    }
    
    #defining dataframe for plotting using ggplot
    df12 <- data.frame(Male, Female, len)
    
    #arranging the categories according to our desired arrangement
    f.df12 <- df12
    f.df12$len <- factor(f.df12$len,                                    
                         levels = c("Short", "Medium", "Long"))
    
    #creating suitable data frame for comparing gender wise
    dum <- melt(f.df12, id.vars='len')
    colnames(dum) <- c("len", "Gender", "value")
    
    #ggplot
    ggplot(dum, aes(x=len, y=value, fill=Gender)) +
      geom_bar(stat='identity', position='dodge')  + 
      labs(title = "Side-by-side Bar Diagram", 
           subtitle = "Comparison of male and female votes for different Lengths", 
           x = "Length Category", y = "Overall Votes") + 
      theme(plot.title = element_text(size = 20), 
            plot.subtitle = element_text(size = 15), 
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 13),
            legend.title = element_text(size = 14), 
            legend.text = element_text(size = 13))
    
  })
  
  #Boxplot of runtimes of different platforms
  output$distPlot13 <- renderPlot({
    
    #platforms input from sidebar panel
    plat = input$plats1
    
    #refining data set according to the chosen platform
    new <- subset(data , data$Platform %in% plat)
    
    #taking only platform and runtime column
    new <- data.frame(new$Platform, new$`Run Time`)
    colnames(new) <- c("Platform", "Run Time")
    
    #removing missing values
    new <- na.omit(new)
    
    #ggplot
    ggplot(data = new, aes(x = Platform, y = `Run Time`)) +
      geom_boxplot(fill = "darkturquoise")  + 
      labs(title = "Side-by-side Box plot", 
           subtitle = "Comparison of average runtime of different Platforms", 
           x = "Platforms", y = "Average run time (in min)") + 
      theme(plot.title = element_text(size = 20), 
            plot.subtitle = element_text(size = 15), 
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 13))
    
    
  })
  
  #percentage bar plot for comparing certificate categ over platforms
  output$distPlot14 <- renderPlot({
    
    #platforms input from sidebar panel
    plat <- input$plats2
    
    #defining list to hold data frames for each platform
    mt <- list()
    
    #creating data frame of each platform with certificate category and their count of appearance and saving them in 'mt' list
    for(i in 1:length(plat))
    {
      sub <- subset(data, data$Platform %in% plat[i])
      t <- table(sub$`Certificate Category`)
      
      df <- data.frame(plat[i],t)
      colnames(df) <- c("Platform", "Certificate Category", "Proportion")
      mt[[i]] <- df
    }
    
    #crating a single data frame with all the data frame of 'mt'
    final <- mt[[1]]
    
    if (length(plat) > 1) #if else to handle the situation when there is only one platform selected
    {
      for (i in 2:length(mt))
      {
        final <- rbind(final, mt[[i]])
      }
    }
    
    #ggplot
    ggplot(data = final, aes(x = Platform, y = Proportion, fill = `Certificate Category`)) +
      geom_bar(position="fill", stat="identity")  + 
      labs(title = "Percentage Bar plot", 
           subtitle = "Comparison of certificate category of different Platforms", 
           x = "Platforms") + 
      theme(plot.title = element_text(size = 20), 
            plot.subtitle = element_text(size = 15), 
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 13),
            legend.title = element_text(size = 14), 
            legend.text = element_text(size = 13))
    
  })
  
  #which genre works best in which platforms?
  output$distPlot15 <- renderPlot({
    
    #platforms input from sidebar panel
    plat <- input$plats3
    
    #defining list to hold data frames for each platform
    mt <- list()
    
    #creating data frame of each platform with platform, genre and frequency
    for (i in 1:length(plat))
    {
      new <- subset(data , data$Platform == plat[i])
      all_gen_plat <- strsplit(new$Genre , ",")
      vec = c()
      for(a in 1:length(all_gen_plat))
      {
        for(b in 1:length(all_gen_plat[[a]]))
        {
          vec = c(vec, all_gen_plat[[a]][b])
        }
      }
      
      vec <- gsub(" ","",vec)
      t <- table(vec)
      
      df <- data.frame(plat[i], t)
      colnames(df) <- c("Platform", "Genre", "Frequency")
      
      mt[[i]] <- df
      
    }
    
    #crating a single data frame with all the data frame of 'mt'
    final <- mt[[1]]
    
    if (length(plat) > 1)     #if else to handle the situation when there is only one platform selected
    {
      for (i in 2:length(mt))
      {
        final <- rbind(final, mt[[i]])
      }
    }
    
    #finding common genre of the selected platfroms
    com <- unique(final$Genre)
    
    for (i in 1:length(plat))
    {
      a <- subset(final, final$Platform == plat[i])
      ge <- a$Genre
      com <- intersect(com, ge)
    }
    
    #subsetting dataframe to the common genres
    b <- mt[[1]]
    final2 <- subset(b, b$Genre %in% com)
    
    if (length(plat) >1)        #if else to handle the situation when there is only one platform selected
    {
      for (i in 2:length(plat))
      {
        b <- mt[[i]]
        c <- subset(b, b$Genre %in% com)
        final2 <- rbind(final2, c)
      }
    }
    
    #ggplot
    ggplot(final2, aes(x = Platform, y = Frequency, fill = Genre)) +
      geom_bar(position="fill", stat="identity") +
      scale_fill_viridis_d() +
      coord_polar("y")  + 
      labs(title = "Multiple Donut Chart", 
           subtitle = "Comparison of Genre of different Platforms", 
           x = "Platforms", y = "") + 
      theme(plot.title = element_text(size = 20), 
            plot.subtitle = element_text(size = 15), 
            axis.title = element_text(size = 15),
            axis.text = element_text(size = 13),
            legend.title = element_text(size = 14), 
            legend.text = element_text(size = 13))
    
    
  })
  
  #plot with overall votes vs overall ratings
  output$distPlot16 <- renderPlotly({
    
    #platforms input from sidebar panel
    plat <- input$plats4
    
    #defining list to hold data frames for each platform
    new <- list()
    
    #creating data frame of each platform with name, platform, votes and ratings
    for ( i in 1:length(plat))
    {
      sub <- subset(data, data$Platform %in% plat[i])                                              #subsetting data w.r.t the chosen platforms
      df <- data.frame(sub$Name, sub$Platform, sub$`Overall Votes`, sub$`Overall Rating`)          #selecting required columns
      df <- na.omit(df)                                                                            #removinf missing values
      colnames(df) <- c("Name", "Platform","Overall Votes","Overall Rating")
      
      tr <- input$slider                                                                           #input of percentage of outlier to be trimmed
      tr <- tr/100
      tr <- tr/2
      
      df <- df%>% filter(between(`Overall Votes`,                                                  #trimming the outliers                                                
                                 quantile(`Overall Votes`, tr),
                                 quantile(`Overall Votes`, (1 - tr))))
      
      new[[i]] <- df
      
    }
    
    #crating a single data frame with all the data frame of 'mt'
    final <- new[[1]]
    
    if (length(plat) > 1)                #if else to handle the situation when there is only one platform selected
    {
      for (i in 2:length(plat))
      {
        final <- rbind(final,new[[i]])
      }
    }
    
    #removing missing values from the original dataset and stroing it 
    a <- na.omit(data)       
      
    #ggplot
    p <- ggplot() +
      geom_point(data = final, aes(x = `Overall Rating`, y = `Overall Votes`, color = Platform, label = Name)) +                                               #points w.r.t different platfroms
      geom_smooth(data = final, aes(x = `Overall Rating`, y = `Overall Votes`, color = Platform), method = "lm", formula = y ~ x, se = FALSE, size = 0.5) +    #regression lines of individual platforms
      geom_smooth(data = a, aes(x = `Overall Rating`, y = `Overall Votes`), method = "lm", formula = y ~ x, se = FALSE, color = "Black") +                     #overall regression line
      labs(title = "Scatterplot", 
           subtitle = "Overall Ratings vs Overall Votes", 
           x = "Overall Ratings", y = "Overall Votes",
           caption = "Note: The Black line represents the overall regression line.") + 
      theme(plot.title = element_text(size = 20), 
            plot.subtitle = element_text(size = 15), 
            axis.title = element_text(size = 15),
            axis.text = element_text(size = 13),
            legend.title = element_text(size = 14), 
            legend.text = element_text(size = 12),
            plot.caption = element_text(hjust = 0, size = 17))
    
    ggplotly(p) %>%                                                                                                                 #making it interactive
      layout(title = list(text = paste0("Scatterplot",                                                                              #code to add subtitle
                                        '<br>',
                                        '<sup>',
                                        "Overall Ratings vs Overall Votes",
                                        '</sup>'))) %>%
      layout(margin = list(l = 50, r = 50, b = 100, t = 50),                                                                        #code to add caption
             annotations = list(x = 1, y = -0.3, text = "Note: The Black line represents the overall regression line.",
                                xref='paper', yref='paper', showarrow = F, 
                                xanchor = 'right', yanchor = 'auto', xshift = -100, yshift = -26,
                                font = list(size = 20)))
    
  })
  
  #plot of runtime vs votas and ratings
  output$distPlot17 <- renderPlot({
    
    #platforms input from sidebar panel
    plat <- input$plats5
    
    #subsetting data w.r.t the chosen platforms
    dat <- subset(data, data$Platform %in% plat)
    
    #removing missing values
    a <- na.omit(dat)
    
    #creating data frame with four colums only
    a <- data.frame(a$`Run Time`, a$`Overall Rating`, a$`Overall Votes`, a$Platform)
    colnames(a) <- c("Run Time", "Overall Ratings", "Overall Votes", "Platform")
    
    #creating the plot
    ggpairs(a, columns = 1:3,
            aes(color = Platform,  
                alpha = 0.4), diag = list(continuous = "blankDiag"),
            title = "Comparing Runtime with Overall Votes and Overall Rating") +
      theme(plot.title = element_text(size = 20)) + theme_grey(base_size = 15)
    
  })
  
    
}

# Run the application 
shinyApp(ui = ui, server = server)
