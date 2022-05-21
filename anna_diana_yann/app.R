# Necessary libraries to run the app
library(shiny);library(shinythemes)
library(DT);library(shinyWidgets)
library(babynames)
library(tidyverse)
library(glue)
library(BayesFactor)
library(effsize)
library(car)
library(pwr)
library(readxl)
options(DT.options = list(pageLength = 30))


{
  # Define UI for the application
  ui <- navbarPage(
    title = "Ad-Hominem experiment",
    footer = "Anna, Diana & Yann",
    theme = shinytheme("cerulean"),
    setBackgroundColor(
      color = "#d4e1ff"
    ),
    tags$h6("BGU Psychology // Experimental Methodology",align="center"),
    tabPanel("Data",
             titlePanel(title=h3("Real data from Ad-Hominem experiment",align="center")),
             # Inputs
             sidebarLayout(fluid = T,
                           sidebarPanel(width = 0,
                                        helpText("")


                           ), #closes panel

                           # Show a plot of the generated data:
                           mainPanel(width=12,
                                     dataTableOutput("random_data"),
                                     downloadButton("downloadData", "Download Data",
                                                    icon=icon("download"))
                           )
             )#closes layout
    ),#closes tabPanel "data"
    tabPanel("Analysis",
             titlePanel(title=h3("Experiment Analysis",align="left")),
             tabsetPanel(
               tabPanel(h4("T-Test"),

                        sidebarLayout(
                          sidebarPanel(width = 4,
                                       checkboxInput("bayest","Bayesian",F),
                                       tableOutput("grouped_tblt"),
                                       conditionalPanel("!input.bayest",
                                                        plotOutput("table",height="130px"),
                                                        checkboxInput("power","Statistical Power",F)),

                                       conditionalPanel("input.bayest",
                                                        numericInput("prior","Prior mu",
                                                                     value = 0,step = 0.1)),
                                       br(),
                                       h5("Assumptions:"),
                                       verbatimTextOutput("t_ass")
                          ),
                          mainPanel(width=8,
                                    verbatimTextOutput("ttest"),
                                    verbatimTextOutput("effect"),
                                    plotOutput("gg_t")

                          )
                        )
               ),
               tabPanel(h4("Regression"),
                        sidebarLayout(
                          sidebarPanel(width = 4,
                                       checkboxInput("bayesr","Bayesian",F),
                                       tableOutput("grouped_tblr"),
                                       br(),
                                       h5("Assumptions:"),
                                       verbatimTextOutput("norm_res"),
                                       verbatimTextOutput("homo")

                          ),
                          mainPanel(width=8,
                                    verbatimTextOutput("reg"),
                                    plotOutput("gg_r")
                          )
                        )),
               tabPanel(h4("Manipulation"),
                        splitLayout(cellWidths = 550,
                                    dataTableOutput("manipulation")))

             )#closes tabset aregument
    ),
    #this one closes "analysis" head-panel
    tabPanel("Ad Hominem",

             sidebarLayout(
sidebarPanel = NULL,
               mainPanel = mainPanel(width=12,
                         tags$img(height = 750, width = 980,src = "ad.png")

               )) #closes layout
    ) #this one closes "code" tabpanel

  )#this one closes the whole navBar

  # Define server logic required
}
server <- function(input, output) {

  mydata <- reactive({
     t <- read.csv(file.path(getwd(),"df.csv"))
     t[,-1]
  })

  # DATA:
  output$random_data <- renderDataTable({
    mydata()

  })
  # Download data:
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("experimentdata-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(mydata(), file)
    }
  )
  # Analysis:

  ## T- TEST ##

  output$grouped_tblt <- renderTable({
    if(!input$bayest){
      mydata() %>% group_by(evil) %>%
        summarise(Mean=mean(score),SD=sd(score),Items=n())
    }
  })
  diffrences <- reactive( mydata() %>%
                            group_by(subject,evil) %>% summarise(score=mean(score)) %>%
                            pivot_wider(names_from = evil,values_from = score) %>%
                            mutate(diff=`FALSE`-`TRUE`))

  output$t_ass <- renderPrint({
    if(!input$bayest){
      lapply(list(Differences=diffrences()$diff), FUN=shapiro.test)
    }

  })

  effectest <-  reactive({effsize::cohen.d(score~evil | Subject(subject),
                                           mydata(),paired=T,within=T) %>%
      suppressWarnings()})
  power <- reactive({
    pwr::pwr.t.test(n=nrow(mydata()),
                    d=effectest()$estimate,
                    sig.level=.05,
                    alternative="two.sided",
                    type = "paired")
  })

  output$ttest <- renderPrint({
    if(input$bayest)
    {BayesFactor::ttestBF(x = mydata()$score[mydata()$evil==F],
                          y = mydata()$score[mydata()$evil==T],
                          paired=TRUE,mu = input$prior)}
    else{lapply(X = list(Diffrences=diffrences()$diff),
                t.test,alternative="greater")}
  })
  output$effect <- renderPrint({
    if(!input$bayest & input$power){
      power()}
    else{ if(!input$bayest & !input$power){
      effectest()}}
  })

  output$gg_t <- renderPlot({
    if(!input$bayest){
      mydata() %>%
        ggplot(aes(score,fill=evil))+
        geom_bar(show.legend = T,position="dodge2")+
        theme_minimal()+
        scale_fill_manual(values = c("#56b1f7","#132b43"))+
        scale_y_continuous(breaks =NULL)+ylab("")+xlab("")+
        theme(legend.position = "bottom",
              legend.title = element_blank(),title = element_text(face="italic"),
              legend.key.size = unit(4,"mm"),legend.box.spacing = unit(-7,"mm"))+
        labs(caption = "Agreement score distribution by condition")
    }
  })
  output$table <- renderPlot({
    mydata() %>% group_by(evil) %>%
      summarise(score=mean(score)) %>%
      ggplot(aes(evil,score,fill=score,label=round(score,2)))+
      geom_col(show.legend = F)+
      geom_text(nudge_y = -0.5,color="white")+
      theme_void()
  })

  ## REGRESSION ##
  #Helper function:
  reg_model <- function(model,plot){
    SST = round(var(model$model[, 1]) * (length(model$model[,
                                                            1]) - 1), 3)
    SSRes = round(sum(resid(model)^2), 3)
    SSReg = round(SST - SSRes, 3)
    Rsq = round(SSReg/SST, 4)
    lm_m <- summary(model)
    adrsq = round(lm_m$adj.r.squared, 4)
    datatemp <- data.frame(SST = SST, SSRes = SSRes, SSReg = SSReg,
                           Rsq = Rsq, Adj.Rsq = adrsq)
    if(plot){
      print(datatemp %>% select(SSRes,SSReg) %>%
              pivot_longer(cols = everything()) %>%
              ggplot(aes(x="",y=value,fill=name,label=name))+theme_void()+
              geom_bar(stat="identity",color="white",show.legend = F)+
              coord_polar("y",start=0)+
              labs(subtitle =paste("R squared:",datatemp$Rsq))+
              theme(text = element_text(color="#56b1f7",face = "bold",size = 15))+
              scale_fill_manual(values = c("#56b1f7","#132b43"))
      )}


    return(datatemp %>% pivot_longer(cols=everything(),names_to = "Estimate"))
  }
  # Reactive event for regression models:
  bestBF <- eventReactive(input$bayesr,
                          BayesFactor::generalTestBF(score~subject+
                                                       statement+person+
                                                       evil,data=mydata()))

  react_lm <- reactive({lm(score~evil,mydata())})

  # Sidebar panel:
  output$grouped_tblr <- renderTable({
    if(!input$bayesr){
      reg_model(react_lm(),F)
    }
  })
  #assumptions:
  output$homo <- renderPrint({
    if(!input$bayesr){
      car::leveneTest(score~evil,mydata()) %>% suppressWarnings()
    }
  })
  output$norm_res <- renderPrint({
    if(input$bayesr){
      BayesFactor::lmBF(score~evil,mydata())
    } else {lapply(list(Model_Residuals=react_lm()$residuals),shapiro.test)}

  })
  # Main panel:
  output$reg <- renderPrint({
    if(input$bayesr){
      head(bestBF()/bestBF()[length(bestBF())])
    } else{
      react_lm() %>% summary()
    }
  })
  output$gg_r <- renderPlot({
    if(input$bayesr){
      NULL
    } else{
      reg_model(react_lm(),T)
    }
  })

  ## Manipulation ##

  output$manipulation <- renderDataTable({
    mydata()[,c(1,8:12)] %>% group_by(subject) %>% summarise(
      Hitler=mean(Hitler),Sadam=mean(Sadam),Binladen=mean(Binladen),
      Nasrala=mean(Nasrala),Arafat=mean(Arafat)
    ) %>% arrange(-Arafat)
  })



}

# Run the application
shinyApp(ui = ui, server = server)

