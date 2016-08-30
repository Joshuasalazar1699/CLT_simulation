library("shiny")
library('Rlab')
set.seed(1)
shinyServer(function(input, output) {

dis_type <- reactive({
 choice=as.character(input$dis)
 return(choice)
})

output$reactive_menu1 <- renderUI({
  if(dis_type()=="normal"){
  lt<-list(
  sliderInput("observe", "Observation", min=1,max=10000,value = 1000),
  sliderInput("mean", "Mean", min=1,max=1000,value = 30),
  sliderInput("sd", "SD", min=0,max=100,value = 10))
}

  if(dis_type()=="poisson"){
  lt<-list(
  sliderInput("observe", "Observation", min=1,max=10000,value = 1000),
   sliderInput("lamda", "Lamda", min=0,max=100, value = 10))
  }

if(dis_type()=="binomial"){
  lt<-list(sliderInput("observe", "Observation", min=1,max=10000,value = 1000),
  sliderInput("trial", "Trials", min=1,max=10000,value = 1000),
  sliderInput("pval", "P", min = 0, max = 1, value = 0.5))
}

if(dis_type()=="Bernoulli"){
    lt<-list(sliderInput("observe", "Observation", min=1,max=10000,value = 1000),
    sliderInput("pval", "P", min = 0, max = 1, value = 0.5))
 }
  
if(dis_type()=="negative binomial"){
  lt<-list(sliderInput("observe", "Observation", min=1,max=10000,value = 1000),
  sliderInput("size", "Dispersion", min = 0, max = 5, value = 0.5),
  sliderInput("pval", "P", min = 0, max = 1, value = 0.5))
}
  if(dis_type()=="uniform"){
  lt<-list(sliderInput("observe", "Observation", min=1,max=10000,value = 1000),
  sliderInput("min", "min", min = 0, max = 1000, value = 0),
  sliderInput("max", "max", min = 1, max = 1000, value = 1))
}
  
  if(dis_type()=="exponential"){
    lt<-list(sliderInput("observe", "Observation", min=1,max=10000,value = 1000),
    sliderInput("rate", "rate", min = 0.1, max = 10, value = 0.2))
}
return(lt)
})

output$reactive_menu2 <- renderUI({
  
  lt2<- list(sliderInput("sampling_size", "Sampling Size", min=0,max=100,value = 40),
             sliderInput("n_sim", "Simulation#", min=0,max=1000,value = 400))
  return(lt2)
})

sim2<-reactive({
  
  if(input$dis=="normal"){
    sim=rnorm(input$observe,input$mean,input$sd)
 }
  if(input$dis=="binomial"){
    sim=rbinom(input$observe,input$trial,input$pval)
 }
  if(input$dis=="poisson"){
    sim= rpois(input$observe,input$lamda)
  }
  
  if(input$dis=="Bernoulli"){
    sim= rbern(input$observe,input$pval)
  }
  
  if(input$dis=="negative binomial"){
    sim=rnbinom(input$observe,input$size,input$pval)
 }
  if(input$dis=="uniform"){
    sim=runif(input$observe,input$min,input$max)
  }
  
  if(input$dis=="exponential"){
    sim=rexp(input$observe,input$rate)
  }
  
  return(sim)
})

df2<-reactive({
  sim=sim2()
  mns=c()
  var1=c()
  for (i in 1 : input$n_sim) {
    sampling<-sample(length(sim),input$sampling_size,replace = T)
    mns = c(mns, mean(sim[sampling]))
    var1 = c(var1, var(sim[sampling]))
  }
  df<-data.frame(m=mns,v=var1)
  return(df)
})

output$Plot <- renderPlot({
sim=sim2()
if(input$dis=="normal"){
hist(sim,prob=TRUE,breaks=input$bins,xlab=paste0("mean=",mean(sim),"|var=",var(sim)),main=paste0(input$dis," distribution"))
curve(dnorm(x,input$mean,input$sd), col="blue", lwd=2,add=TRUE, yaxt="n")
abline(v=mean(sim),col="red",lwd=2)

 }
 if(input$dis=="binomial"){
hist(sim,prob=TRUE,breaks=input$bins,xlab=paste0("mean=",mean(sim),"|var=",var(sim)),main=paste0(input$dis," distribution"))
curve(dbinom(as.integer(x),input$observe,input$pval), col="blue", lwd=2,add=TRUE, yaxt="n")
abline(v=mean(sim),col="red",lwd=2)
 }
  
if(input$dis=="Bernoulli"){
  hist(sim,prob=TRUE,breaks=input$bins,xlab=paste0("mean=",mean(sim),"|var=",var(sim)),main=paste0(input$dis," distribution"))
  curve(dbern(as.integer(x),input$pval), col="blue", lwd=2,add=TRUE, yaxt="n")
  abline(v=mean(sim),col="red",lwd=2)
}


 if(input$dis=="poisson"){
hist(sim,prob=TRUE,breaks=input$bins,xlab=paste0("mean=",mean(sim),"|var=",var(sim)),main=paste0(input$dis," distribution"))
curve(dpois(as.integer(x),input$lamda), col="blue", lwd=2,add=TRUE, yaxt="n")
abline(v=mean(sim),col="red",lwd=2)
  }

 if(input$dis=="negative binomial"){
hist(sim,prob=TRUE,breaks=input$bins,xlab=paste0("mean=",mean(sim),"|var=",var(sim)),main=paste0(input$dis," distribution"))
curve(dnbinom(as.integer(x),input$size,input$pval), col="blue", lwd=2,add=TRUE, yaxt="n")
abline(v=mean(sim),col="red",lwd=2)
  }

 if(input$dis=="uniform"){
hist(sim,prob=TRUE,breaks=input$bins,xlab=paste0("mean=",mean(sim),"|var=",var(sim)),main=paste0(input$dis," distribution"))
curve(dunif(as.integer(x),input$min,input$max), col="blue", lwd=2,add=TRUE, yaxt="n")
abline(v=mean(sim),col="red",lwd=2)
  }
  
  if(input$dis=="exponential"){
hist(sim,prob=TRUE,breaks=input$bins,xlab=paste0("mean=",mean(sim),"|var=",var(sim)),main=paste0(input$dis," distribution"))
curve(dexp(as.integer(x),input$rate), col="blue", lwd=2,add=TRUE, yaxt="n")
abline(v=mean(sim),col="red",lwd=2)
    
  }

})

output$plot2 <- renderPlot({
df=df2()
df_mean=apply(df,2,mean)
df_var=apply(df,2,var)
hist(df$m,prob=TRUE,breaks=input$bins,xlab="mean of sample mean",main=paste0("mean of ",input$dis," samples"))

curve(dnorm(x,df_mean[1],sqrt(df_var[1])), col="red", lwd=2,add=TRUE, yaxt="n")
})

output$plot3 <- renderPlot({
 df=df2()
 df_mean=apply(df,2,mean)
 df_var=apply(df,2,var)
  hist(df$v,prob=TRUE,breaks=input$bins,xlab="variance of sample variance",main=paste0("variance of ",input$dis," samples"))
  
  curve(dnorm(x,df_mean[2],sqrt(df_var[2])), col="red", lwd=2,add=TRUE, yaxt="n")
})

})


