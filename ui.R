library(shiny)
shinyUI(fluidPage(

titlePanel("Using R to simulate commmon statistic distributions and to validate the Central Limit Theorem (CLT) \n\n"),
br(),
hr(),

a("Documentation",target="_blank",href="https://sites.google.com/site/zhangj848012/document.pdf"),
br(),
a("Download Source Codes @ GitHub",target="_blank",href="https://github.com/zhangj5/CLT_simulation"),
hr(),
br(),
br(),

fluidRow(

column(2, selectInput("dis","Select distribution to simulate",selected="normal",
                      choices = c("normal","poisson","binomial","Bernoulli","negative binomial","uniform","exponential"),selectize = FALSE, size = 10)),
column(2, sliderInput("bins", "Number of bins:", min = 1, max = 50, value = 30)),
column(4, uiOutput("reactive_menu1")),
column(4, uiOutput("reactive_menu2"))
),


fluidRow(
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  column(4,plotOutput("Plot")),
  column(4,plotOutput("plot2")),
  column(4,plotOutput("plot3"))
)
))

