library(shiny)
library(tidyverse)
library(tidytext)
library(stringr)
library(shinythemes)
library(textdata)

wrangledHP<-read_csv("wrangledHP.csv")

ui <- fluidPage(
    theme = shinytheme("superhero"),
    titlePanel("Sentiment Analysis of the Harry Potter Series"),
    p("In order to preform the analysis, I used data collected by Bradley Boehmke at the link below. I then used the sentiment libraries of 
      AFINN and Bing et al. in order to analize the mean sentiment of each chapter of each Harry Potter book. In oder to scale the data, 
      I divided AFINN mean by 5 as well as centered the data around 0 by subtracting the mean series sentiment. Choose which books and
      sentiment lexicon you wish to explore."),
    a(href ="https://github.com/bradleyboehmke/harrypotter","Harry Potter Data"),
    hr(),
    sidebarLayout(
        sidebarPanel(
            p("Please select at least one book and at least one sentiment lexicon."),
            checkboxGroupInput(inputId = "books", 
                               label = "Book select:",
                               choiceNames = c("Sorcerers Stone", "Chamber Of Secrets", "Prisoner Of Azkaban", "Goblet Of Fire", "Order Of The Phoenix", "Half Blood Prince", "Deathly Hallows"),
                               choiceValues = c("sorcerersStone", "chamberOfSecrets", "prisonerOfAzkaban", "gobletOfFire", "orderOfThePhoenix", "halfBloodPrince", "deathlyHallows"),
                               selected = "sorcerersStone"),
            
            checkboxGroupInput(inputId = "lex", 
                               label = "Lexicon select:",
                               choiceNames = c("AFINN", "Bing"),
                               choiceValues = c("Afinn", "Bing"),
                               selected = c("Afinn", "Bing")),
            actionButton(inputId = "button1", label = "Enter")),
        mainPanel(plotOutput(outputId = "plot1"))
    )
)

server <- function(input, output, session) {
    output$plot1 <- renderPlot({
        input$button1
        
        if (!is.null(isolate(input$books)) && !is.null(isolate(input$lex))){
            wrangledHP%>%
                filter(book == isolate(input$books))%>%
                group_by(book, chapter)%>%
                summarise(Afinn = (mean(value, na.rm = TRUE)+0.442)/5,
                          Bing = mean(sentiment, na.rm = TRUE)-0.317)%>% #This line is the scalling to center the sentiment around 0 and scale AFFIN back by 5
                pivot_longer(cols = c(Afinn,Bing), names_to = "Sentiment Lexicons")%>%
                filter(`Sentiment Lexicons` == isolate(input$lex))%>%
                ggplot(aes(x = chapter, 
                           y = value,
                           fill = `Sentiment Lexicons`))+
                geom_col(position="dodge2")+
                scale_fill_manual(values=c("#22A7F0", "#E2B13C"))+
                geom_smooth(aes(color = `Sentiment Lexicons`),
                            se=FALSE,
                            method = 'loess',
                            formula = y ~ x)+
                scale_color_manual(values=c("#317589", "#E08A1E"))+
                facet_wrap(~fct_relevel(book,"sorcerersStone","chamberOfSecrets","prisonerOfAzkaban","gobletOfFire","orderOfThePhoenix","halfBloodPrince","deathlyHallows"),
                           scales = "free_x",
                           strip.position = "bottom")+
                ylab("Scaled Sentiment")+
                xlab("Book Chapter")+
                theme_light()+
                theme(legend.position = "bottom")
            
        }
    })
}

shinyApp(ui, server)