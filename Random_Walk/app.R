library(tidyverse)
library(glue)
library(shiny)
library(DT)
library(plotly)
# Create basic RW function
wonder <- function(n_steps,start,
                   step_probability=rep(1/4,4),
                   step_size=rep(1,4),
                   max_distance=Inf,
                   dot_size,dot_type,
                   dot_alpha){
  n=n_steps
  x <- rep(start[1],n)
  y <- rep(start[2],n)
  d <- rep(0,n)
  direction=rep("Origin",n)

  ###
  for(i in 2:n){
    val=sample(4,1,prob = step_probability)

    if(val==1){
      x[i] <- x[i-1]+step_size[1]
      y[i] <- y[i-1]
      direction[i]="East"
    } else{
      if(val==2){
        x[i] <- x[i-1]-step_size[2]
        y[i] <- y[i-1]
        direction[i]="West"
      } else{
        if(val==3){
          x[i] <- x[i-1]
          y[i] <- y[i-1]+step_size[3]
          direction[i]="North"
        } else{
          x[i] <- x[i-1]
          y[i] <- y[i-1]-step_size[4]
          direction[i]="South"
        }
      }
    }
    # Calculate distance from starting point:
    d[i] <- sqrt((x[i]-start[1])^2+(y[i]-start[2])^2)
    # Stop walking when reaching a certain distance:
    if(d[i]>max_distance){
      break
    }# end of break condition


  } # end of loop
  table = data.frame(step=1:i,x=x[1:i],
                     y=y[1:i],distance=d[1:i],
                     direction=direction[1:i])

  #Set plot limits
  lim <- list(y=c(min(y[1:i]),max(y[1:i])),
              x=c(min(x[1:i]),max(x[1:i])))

  geom <- geom_jitter(show.legend = F,size=d[1:i]*dot_size+0.1,alpha=dot_alpha)
  if(dot_type){
    geom <- geom_point(show.legend = F,size=d[1:i]*dot_size+0.1,alpha=dot_alpha)
  }
  gg_object <- table %>% ggplot(aes(x[1:i],y[1:i],
                                    color=step[1:i],
                                    text=paste0("(",x,",",y,")",
                                      "\nStep: ",step[1:i],
                                      "\nDistance: ",round(d[1:i],3)
                                               )),alpha=.7)+
    geom+
    geom_hline(yintercept = start[2],alpha=.42)+
    geom_vline(xintercept = start[1],alpha=.42)+
    coord_cartesian(lim[["x"]]*1.2,lim[["y"]]*1.2)+
    theme_minimal()+theme(axis.title = element_blank(),
                          title = element_text(face="bold",size=20,hjust = 0))
msg <- glue("Avg distance over {i} steps: {round(mean(d[1:i]),3)}")
  return(list(table,gg_object,msg))

} # end of function
# for gg print wonder[[2]]




# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("Random Walk"),
  tabPanel(title="Wonder around",
           tabsetPanel(
             tabPanel("Walk",
                      # Sidebar with a slider input for number of bins
                      sidebarLayout(
                        sidebarPanel(width = 3,
                                     numericInput("n",
                                                  "Number of steps:",
                                                  min = 10,step=100,
                                                  max = 1e6,
                                                  value = 1e3),
                                     textInput("startp","Origin point",value = "0,0",
                                               placeholder = "Initialize origin point"),
                                     numericInput(inputId = "max",
                                                  "Max distance allowed",value=100,
                                                  min=1,max=Inf,step=1),
                                     verbatimTextOutput("infosteps"),
                                     sliderInput("dot","Dot size",0,3,.1,0.05),
                                     sliderInput("alph","Transparency",0,1,.5,0.01),
                                     checkboxInput("ggeom","Diffrent dots"),
                                     actionButton("walk","Start Walking"),
                                     tags$hr(),
                                     checkboxInput("hack","Modify steps",F),
                                     conditionalPanel("input.hack",
                                                      sliderInput("nprob",
                                                                  "Probability to take a step North",
                                                                  0,1,1/4,0.01),
                                                      sliderInput("sprob",
                                                                  "Probability to take a step South",
                                                                  0,1,1/4,0.01),
                                                      sliderInput("wprob",
                                                                  "Probability to take a step West",
                                                                  0,1,1/4,0.01),
                                                      sliderInput("eprob",
                                                                  "Probability to take a step East",
                                                                  0,1,1/4,0.01),
                                                      tags$hr(),br(),
                                                      sliderInput("nsize","Size of step North",0,3,1,0.1),
                                                      sliderInput("ssize","Size of step South",0,3,1,0.1),
                                                      sliderInput("wsize","Size of step West",0,3,1,0.1),
                                                      sliderInput("esize","Size of step East",0,3,1,0.1))


                        ),

                        # Show a plot of the generated distribution
                        mainPanel(width = 9,
                                  plotlyOutput("ggobj"))

                      )),
             tabPanel("Follow each step",
                      splitLayout(
                        DT::dataTableOutput("table")
                      )
             )
           ))

)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
  plotit <- eventReactive(input$walk,{
    # set step prob and size
    steps_prob <- c(input$eprob,input$wprob,input$nprob,input$sprob)
    steps_size <- c(input$esize,input$wsize,input$nsize,input$ssize)
    # set starting point
    strt=unlist(str_split(input$startp,",")) %>% as.numeric()

    wonder(input$n,max_distance=input$max,start = c(strt[1],strt[2]),
           dot_size=input$dot,dot_type = input$ggeom,
           step_probability = steps_prob,
           step_size = steps_size,dot_alpha = input$alph)
  })

  observe({
    val <- input$n*10
    # Control the value, min, max, and step.
    # Step size is 2 when input value is even; 1 when value is odd.
    updateSliderInput(session, "max", value = val,
                      min=1,max=Inf,step=1)
  })
  output$ggobj <- renderPlotly({

    ggplotly(plotit()[[2]],tooltip = "text")

  })
  output$infosteps <- renderPrint({
    req(plotit())
    plotit()[[3]]
  })

  output$table <- DT::renderDataTable(plotit()[[1]])
}

# Run the application
shinyApp(ui = ui, server = server)
