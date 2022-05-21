# works.
# needs a lot more customazation
"https://shiny.rstudio.com/images/shiny-cheatsheet.pdf"

library(shiny);library(DT)
library(colourpicker);library(shinydashboard)
library(tidyverse);library(shinythemes)
library(rvest);library(wordcloud2)
library(shinyWidgets);library(glue)
library(htmltools)

t <- function(page,language="en",bg_color="white",
              word_color=c("#2f68d1 steelblue #0067e3"),shape="circle",
              min_freq=0,max_freq=NULL,
              remove_pattern="[:punct:]",
              remove_words=NULL,
              word_length=c(1,30)){

  page <- page %>% str_replace_all(" ","_") %>% str_to_title()

  # IF none mentioned, remove all these words:
  fr.es_remove=c("su fue una como o mas al Más ha para no sus elle los À ne À si e ser uso sin ce qui'il Apr Ã s comme cette a han ya e esto estan D'un este esta  ellas ellos lo son avec aux se por Ã y el il par sur sa ses qui que en es un con las del de la et le les en des a du dans sont ou est pour un au une pas ont")
  en_remove=c("he will who more have because into can hi her then so do about what been him but may being this its had has which there these such than due were their also is not or are if it be the an at from as after who they had by for that with were the of and in to a his on was")
  remove_words_null = str_to_title(unlist(str_split(c(fr.es_remove,en_remove),pattern=" ")))
  if(is.null(remove_words)){
    remove_words=remove_words_null
  } else {remove_words=c(str_to_title(unlist(str_split(remove_words,pattern=" "))),
                         remove_words_null )}
  # ELSE, remove the words mentioned.
  # Data mining from wikipedia
  web_link <- paste(c("https://"),language,c(".wikipedia.org/wiki/"),page,sep = "")

  raw_text=rvest::read_html(web_link) %>% rvest::html_nodes("p") %>%
    rvest::html_text()
  # Data cleanining
  clean.text <- str_to_title(str_remove_all(
    unlist(str_split(raw_text,pattern = " ")),
    pattern = remove_pattern))
  #
  words.freq <- data.frame(word=clean.text,Frequency=1,
                           Length=str_length(clean.text)) %>%
    group_by(word) %>% summarise(Frequency=sum(Frequency),
                                 Length=mean(Length)) %>%
    arrange(desc(Frequency)) %>% filter(!word %in%remove_words,
                                        Length>=word_length[1] & Length<=word_length[2])

  if(is.null(max_freq)){max_freq=max(words.freq$Frequency)}
  else {max_freq=max_freq}
  # Word cloud
  word_color <- unlist(str_split(word_color, " "))
  word_col <- sample(word_color,nrow(words.freq[words.freq$Frequency>=min_freq& words.freq$Frequency<=max_freq,]),T)
  cloud=wordcloud2::wordcloud2(words.freq[words.freq$Frequency>=min_freq& words.freq$Frequency<=max_freq,],
                               color = word_col,backgroundColor = bg_color,shape=shape)
  # Print cloud
  to_print <- list(
    ggcloud=cloud,
    table=words.freq[words.freq$Frequency>=min_freq & words.freq$Frequency<=max_freq,]
  )
  return(to_print)
}


# Define UI for application that draws a histogram
ui <- navbarPage(

  # aesthetics:
  title = "Wikipedia's Word Cloud",
  footer =   tags$h5(a("Email me more words to remove in different languages please.", href="mailto:yannco@campus.technion.ac.il"),align="left"),
  theme = shinytheme("cerulean"),

  setBackgroundColor(
    color = c("#c4c7f0", "#d4e1ff"),
    gradient = "radial",
    direction = "bottom"
  ),
  # end of aesthetics

  tabPanel("iWordCloud", #@@@PRIMARY@@@
           titlePanel(title=h3("Create a fully customizable word cloud!",align="center")),
           tabsetPanel(
             tabPanel(h4("Word Cloud"), # @@@SECONDARY@@@

                      sidebarLayout(
                        sidebarPanel(width = 3,
                                     # Basic features in the Word Cloud tab:
                                     textInput("pagew",
                                               "Value:",
                                               value = "Covid",
                                               placeholder = "e.g Machine learning"),

                                     selectInput("shapec","Shape",choices = c("circle","cardioid",
                                                                              "star","pentagon","diamond",
                                                                              "triangle",
                                                                              "triangle-forward")),

tags$hr(),
                                     # Advanced features in the Word Cloud tab:

                                     checkboxInput("advanced","Advanced",F),

                                     conditionalPanel("input.advanced",

                                                      radioButtons("lang","Select language",
                                                                   choices = c("English"="en",
                                                                               "Español"="es",
                                                                               "French"="fr",
                                                                               "Português"="pt",
                                                                               "Italiano"="it",
                                                                               "Deutsch"="de",
                                                                               "Dutch (nl)"="nl"),selected = "en"),
                                                      sliderInput("range",
                                                                  label = "Word Frequancy Range:",
                                                                  min = 1, max = 100, value = c(0, 100),step = 1),
                                                      sliderInput("wrdln",
                                                                  label = "Word Length Range:",
                                                                  min = 1, max = 30, value = c(1, 30),step = 1),

                                                      colourInput("col_bg","Background color",value = "gray"),
                                                      textInput("col_w","Word color(s)",value=c("#2f68d1 steelblue #0067e3"),
                                                                placeholder = "either as red or #ff1e00"),
                                                      textInput("rmwrds","Remove words:"),

                                                      a(h5("Color names in R"),href="http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf")


                                     ), # end of conditional
tags$hr(),
                                    br(), actionButton("go","Apply changes",
                                                  icon=icon("cloud"))



                        ), # end of sidebar PANEL
                        mainPanel(
                          uiOutput("distPlot")


                        ) # end of MAIN panel
                      ) # end of sidebar LAYOUT

             ), # end of tab 'word cloud' @@@SECONDARY@@@

             tabPanel(h4("Table"), # @@@SECONDARY@@@
                      sidebarLayout(
                        sidebarPanel(width = 3,
                                     downloadButton("downloadData", "Download table",
                                                    icon=icon("download"))
                        ),# end of sidebar PANEL
                        mainPanel(
                          dataTableOutput("dataf")

                        ) # end of MAIN panel
                      ) # end of sidebar LAYOUT

             ) # end of tab 'Table' @@@SECONDARY@@@


             # Closing UI:

           ))) # end of (1) tabset that contains both small sets
#                  (2) @@@PRIMARY@@@ tabpanel
#                  (3) entire UI




# Define server logic required to draw a histogram
server <- function(input, output,session) {

  wrd_cld <-   eventReactive(input$go,{

      t(
        page=input$pagew,
        language=input$lang,
        bg_color=input$col_bg,
        word_color=input$col_w,
        shape=input$shapec,
        min_freq=input$range[1],
        max_freq=input$range[2]+1,
        word_length = input$wrdln,
        remove_words=input$rmwrds
      )

  })

  output$distPlot <- renderUI({
    wrd_cld()[[1]]
  })

  output$dataf <- renderDataTable({
    wrd_cld()[[2]]
  })

  # Download data:
  output$downloadData <- downloadHandler(
    filename = function() {
      glue("{input$pagew} Word Frequency.csv")
    },
    content = function(file) {
      write.csv(wrd_cld()[[2]], file)
    }
  )


}

# Run the application
shinyApp(ui = ui, server = server)
