#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(e1071)
library(kernlab)
library(mlbench)
library(ggplot2)


#Linearly Separable data
set.seed(1234)
x <- matrix(rnorm(100*2),ncol=2)
y <- c(rep(1,50),rep(-1,50))

x[y==1,] =x[y==1] +4

#plot(x,col=y+3)

dfls <- data.frame(x=x,y=as.factor(y))


#Non-Linear (Radial works best)

x <- matrix(rnorm(100*2),ncol=2)
x[1:30,] = x[1:30,] +2
x[31:60,] = x[31:60,] - 2
y <- c(rep(1,60),rep(-1,40))
#plot(x,col=y+3)

dfnonli <- data.frame(x=x,y=as.factor(y))


# XOR pattern (simple)
p11<-cbind(rnorm(n=25,mean=1,sd=0.5),rnorm(n=25,mean=1,sd=0.5))
p12<-cbind(rnorm(n=25,mean=-1,sd=0.5),rnorm(n=25,mean=1,sd=0.5))
p13<-cbind(rnorm(n=25,mean=-1,sd=0.5),rnorm(n=25,mean=-1,sd=0.5))
p14<-cbind(rnorm(n=25,mean=1,sd=0.5),rnorm(n=25,mean=-1,sd=0.5))
t<-as.factor(c(rep(0,50),rep(1,50)))
dfxorsimp<-data.frame(x=rbind(p11,p13,p12,p14),y=t)


# XOR pattern (complex)
p21<-cbind(rnorm(n=25,mean=1,sd=1),rnorm(n=25,mean=1,sd=1))
p22<-cbind(rnorm(n=25,mean=-1,sd=1),rnorm(n=25,mean=1,sd=1))
p23<-cbind(rnorm(n=25,mean=-1,sd=1),rnorm(n=25,mean=-1,sd=1))
p24<-cbind(rnorm(n=25,mean=1,sd=1),rnorm(n=25,mean=-1,sd=1))
t<-as.factor(c(rep(0,50),rep(1,50)))
dfxorcomp<- data.frame(x=rbind(p21,p23,p22,p24),y=t)


#Concentric Circles
dfcon <- read.csv("circledata.csv")
dfcon$y <- as.factor(dfcon$y)


#Spiral
dfspi <- data.frame(mlbench.spirals(n=200,cycles=1.2,sd=.03))
names(dfspi) <- c("x.1","x.2","y")


mysvm <- function(dataset,cost,kernel,degree=3,sigma=.5,seed=99) {
  
  set.seed(seed)
  samp <- sample(nrow(dataset),.75*nrow(dataset))
  train <- dataset[samp,]
  test <- dataset[-samp,]
  
  if (kernel=="Linear") {
    model <- ksvm(y~.,train,C=cost,kernel="vanilladot",scaled=F)
  }
  
  else if (kernel=="Polynomial") {
    model <- ksvm(y~.,train,C=cost,kernel="polydot",kpar=list(degree=degree),scaled=F)
  }
  
  else if(kernel=="Radial") {
    model <- ksvm(y~.,train,C=cost,kernel="rbfdot",kpar=list(sigma=sigma),scaled=F)
  }
  
  else "Please Select correct kernel"
  
  p <- kernlab::plot(model,data=train)
  
  
  
  
  
  
  return(list(p=p,model=model))
}


# Define UI for application that draws a histogram
ui <- fluidPage(titlePanel("SVM Playground"),
                 
                 
                          sidebarLayout(
                            sidebarPanel(width=2,
                                         radioButtons("radio",label="Select Dataset",
                                                      choices = list("Linearly Separable" = 1 ,
                                                                     "Non-Linear" = 2,
                                                                     "Simple XOR" = 3,
                                                                     "Complex XOR"= 4,
                                                                     "Concentric Circles"= 5,
                                                                     "Spiral"= 6)),
                                         selectizeInput("kernelin",label="Select Kernel",
                                                        choices = list("Linear","Polynomial","Radial"),selected=NULL),
                                         selectizeInput("costin",label="Cost",choices=list(.001,.01,.1,1,5,10),selected=1),
                                         uiOutput("degree"),
                                         uiOutput("gamma"),
                                         actionButton("submit",label = "Run")
                                         
                                         
                                         
                            ),
                            
                            # Show a plot of the generated distribution
                            mainPanel(
                             
                            fluidRow(column(6,plotOutput("dataplot"),
                                            fluidRow(column(6,verbatimTextOutput("traintab")),
                                                     column(6,verbatimTextOutput("testtab")))),
                                     column(6,plotOutput("svmplot"),div(style="height:50px;"),
                                            fluidRow(column(6,textOutput("trainacc"))),fluidRow(column(6,textOutput("testacc")))))
                              
                              
                            )
                          )
                          
                          
                 
                 
                 
                 
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
  
  session$onSessionEnded(stopApp)
  
  output$dataplot <- renderPlot({
    
    if(input$radio == 1) df <- dfls
    else if (input$radio ==2) df <- dfnonli
    else if (input$radio ==3) df <- dfxorsimp
    else if (input$radio ==4) df <- dfxorcomp
    else if (input$radio ==5) df <- dfcon
    else if (input$radio ==6) df <- dfspi
    else "Please Select a dataset"
    
    ggplot(df,aes(x.1,x.2)) + geom_point(col=as.numeric(df$y),size=2.5)+
      theme_minimal()+xlab("x.1")+ylab("x.2") + ggtitle(" \n Dataset plot \n") + 
      theme(plot.title= element_text(hjust=.5,size=16,face="bold"))
    
    
    
  },width=350,height=350)
  
  
  output$degree <- renderUI({
    
    if (input$kernelin != "Polynomial")
      return()
    
    selectizeInput("degreein",label="Degree",
                   choices = list(1,2,3,4,5,10),selected=NULL)
  })
  
  
  output$gamma <- renderUI({
    
    if (input$kernelin != "Radial")
      return()
    
    selectizeInput("gammain",label="Gamma",
                   choices = list(.0001,.001,.01,2^-4,.01,2^-3,2^-2,.5,.1,1,2),selected=.5)
  })
  
  
  svmoutput <- eventReactive(input$submit,{
    
    if(input$radio == 1) df <- dfls
    else if (input$radio ==2) df <- dfnonli
    else if (input$radio ==3) df <- dfxorsimp
    else if (input$radio ==4) df <- dfxorcomp
    else if (input$radio ==5) df <- dfcon
    else if (input$radio ==6) df <- dfspi
    else "Please Select a dataset"
    
    cost <- as.numeric(input$costin)
    degree <- as.numeric(input$degreein)
    gamma <- as.numeric(input$gammain)
    
    
    
    out <- mysvm(df,cost=cost,kernel=input$kernelin,degree=degree,sigma=gamma)
    
    list(plotout=out$p,modelout=out$model)
    
     })
  
  output$svmplot <- renderPlot(svmoutput()$plotout,width = 500,height=450)
  
  metrics <- eventReactive(input$submit,{
    
    model <- svmoutput()$modelout
    
    
    if(input$radio == 1) df <- dfls
    else if (input$radio ==2) df <- dfnonli
    else if (input$radio ==3) df <- dfxorsimp
    else if (input$radio ==4) df <- dfxorcomp
    else if (input$radio ==5) df <- dfcon
    else if (input$radio ==6) df <- dfspi
    else "Please Select a dataset"
    
    set.seed(99)
    samp <- sample(nrow(df),.75*nrow(df))
    train <- df[samp,]
    test <- df[-samp,]
    
    trainpreds <- predict(model,train)
    traintab <- table(actual=train$y,trainPredictions=trainpreds)
    trainacc <- paste0("Accuracy on Train: ",round((sum(diag(traintab))/sum(traintab)) *100,2),"%")
    
    testpreds <- predict(model,test)
    testtab <- table(actual=test$y,testPredictions=testpreds)
    testacc <- paste0("Accuracy on Test: " ,round((sum(diag(testtab))/sum(testtab)) *100,2),"%")
    
  list(traintab=traintab,testtab=testtab,trainacc=trainacc,testacc=testacc)
    
    })
  
  
  output$trainacc <- renderText(metrics()$trainacc)
  output$testacc <- renderText(metrics()$testacc)
  output$traintab <- renderPrint(metrics()$traintab)
  output$testtab <- renderPrint(metrics()$testtab) 
}

# Run the application 
shinyApp(ui = ui, server = server)

