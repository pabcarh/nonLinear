#' non_linear_DT
#'
#' A special curve using specific parameters and also its modelling
#' 
#' If no estimated values of the parameters of the model must enable "modeling = TRUE", then include the initial values
#'
#' @param x numeric vector.
#' @param y numeric vector.
#' @param Error numeric vector; default is NULL.
#' @param yl numeric vector with two values.
#' @param xl numeric vector with two values.
#' @param yyl numeric vector with two values.
#' @param xyl numeric vector with two values.
#' @param Fyx a expression; default is 1.
#' @param Ival a list; default is NULL.
#' @param SEmodel numeric value; default is NULL.
#' @param dff numeric value.
#' @param dir a path.
#' @param show logical; default is FALSE.
#' @param ylab character a string
#' @param xlab character a string; default is 'temperature (°C)'.
#' @param variable character a string; default is 'Other' and you can choise 'DT' and "Fec"
#' @param modelling logical; default is FALSE.
#' @param yypos numeric vector; default is NULL.
#' @param yylab numeric vector; default is NULL.
#' @param width the width of the device; default is 12.
#' @param height the height of the device; default is 10.
#' @param cex.lab the height of the device; default is 1.
#' @param LTY numeric vector with two values; the style of the line for the curve and intervals (dots); default is c(1,3).
#' @param PCH numeric value; the style of the point; default is 19.
#' @param ColPoint character or numeric value; color of the point; default is "gray60".
#' @param TITLE character a string; default is NULL
#' @param Symbol character a string; default is NULL
#' @param FeatSymbol numeric vector with three values; the first value defines the X coordinate using a negative value, the second value defines the Y coordinate and the third value defines the size of the symbol;default is NULL
#' @param Symbol character a string; default is NULL
#' @param FeatSymbol numeric vector with three values; the first value defines the X coordinate using a positive value, the second value defines the Y coordinate and the third value defines the size of the text;default is NULL
#' @param Limit1 numeric vector with three values; for minimum x limit; the first value defines the X limit of main curve and the second and third value define the X limit of the confidence interval curve;default is NULL
#' @param Limit2 numeric vector with three values; for maximum x limit; the first value defines the X limit of main curve and the second and third value define the X limit of the confidence interval curve;default is NULL
#' @param Symbol character; to aggregate a plot or plots in another plot processed;posible values could be "ADD_0","ADD_i","ADD_f" and "No";default is NULL
#' @return character a string; default is 'finished!'.
#' @author Pablo Carhuapoma Ramos
#' @family example
#' @example inst/examples/ex_non_linear_DT.R
#' @export
non_linear_DT<-function(x,y,Error=NULL,yl,xl,yyl,xxl,ylab,xlab=expression(bold(paste("temperature (", degree, "C)"))),Fyx,Ival,SEmodel=NULL,DFF=NULL,dir,show=FALSE,variable="Other",modelling=FALSE,yypos=NULL,yylab=NULL,width=12,height=10,cex.lab=1,Cex.Axis=1,LTY=c(1,3),PCH=19,ColPoint="gray60",TITLE=NULL,Symbol=NULL,FeatSymbol=NULL,Text=NULL,FeatText=NULL,Limit1=NULL,Limit2=NULL,ADD="No")
{
  # modelling
  if(modelling==TRUE)
  {
    out <- nls(Fyx, start = Ival,trace = FALSE)
    SE<-(summary(out))[3]$sigma
    dff<-(summary(out))[4]$df[2]
    sal <- coef(out)
  }else
  {
    dff<-length(y)-length(Ival)
    sal <- Ival
  }
  
  for (i in names(sal))
  {
    temp <- sal[i]
    storage.mode(temp) <- "double"
    assign(i, temp)
  }
  
  switch(variable,
         DT={
           Y <- 1/exp(y)
           ff <- function(x){1/exp(eval(Fyx[[3]]))}
           SE<-sqrt(sum((Y-ff(x))^2)/(length(Y)-length(sal)))
           if(!is.null(SEmodel)){SE <- SEmodel}
           if(!is.null(DFF)){dff <- DFF}
           ffup <- function(x){1/(exp(eval(Fyx[[3]])+SE*qt(0.975,dff)))}
           fflo <- function(x){1/(exp(eval(Fyx[[3]])-SE*qt(0.975,dff)))}
         },
         Fec={
           Y <- exp(y)
           ff <- function(x){exp(eval(Fyx[[3]]))}
           SE<-sqrt(sum((log(Y)-log(ff(x)))^2)/(length(Y)-length(sal)))
           if(!is.null(SEmodel)){SE <- SEmodel}
           if(!is.null(DFF)){dff <- DFF}
           ffup <- function(x){exp(eval(Fyx[[3]])+SE*qt(0.975,dff))}
           fflo <- function(x){exp(eval(Fyx[[3]])-SE*qt(0.975,dff))}
         },
         Other={
           Y <- y
           ff <- function(x){eval(Fyx[[3]])}
           SE<-sqrt(sum((Y-ff(x))^2)/(length(Y)-length(sal)))
           if(!is.null(SEmodel)){SE <- SEmodel}
           if(!is.null(DFF)){dff <- DFF}
           ffup <- function(x){eval(Fyx[[3]])+SE*qt(0.975,dff)}
           fflo <- function(x){eval(Fyx[[3]])-SE*qt(0.975,dff)}
         },
         stop("Enter something that switches me!")
  )
  
  corrx2<-seq(xl[1],xl[2],xxl)
  corry2<-seq(yl[1],yl[2],yyl)
  
  if(!is.null(yypos) & !is.null(yylab))
  { 
    corry2<-yypos
  }else(yylab<-corry2)
  
  switch(ADD,
         ADD_0={
           show=FALSE
           cat("\n","add another graphic..","\n")
         },
         ADD_i={
           show=TRUE
           cat("\n","add another graphic..","\n")
         },
         ADD_f={
           show=TRUE
         },
         No={
           show=show
         },
         stop("Enter something that switches me!")
  )
    
  if(show==FALSE)
  {
    png(paste(dir,"/Plot_NonLinear.png",sep=""), width = width, height = height, units = 'in', res = 300) # para el grafico
  }
  par(mar=c(6.1, 8.1, 2.1, 3.1),xpd=TRUE,font.lab=2)
  #plot(x, Y, ylab=ylab, xlab=xlab, col="transparent", pch=19,axes=F,xlim=c(xl[1],xl[2]),ylim=c(yl[1],yl[2]),cex = 1.5, cex.axis=Cex.Axis,cex.sub=1.5,cex.lab=cex.lab,main=TITLE,font.lab=2)
  plot(x, Y, ylab="", xlab="", col="transparent", pch=PCH,axes=F,xlim=c(xl[1],xl[2]),ylim=c(yl[1],yl[2]),cex = 1.5, cex.axis=Cex.Axis,cex.sub=1.5,cex.lab=cex.lab,main=TITLE,font.lab=2)
  axis(1, corrx2,lwd=2,cex.axis = Cex.Axis)
  axis(2, corry2,labels = round(yylab,2),las=2,lwd=2,cex.axis = Cex.Axis)
  title(ylab=ylab, cex.lab = cex.lab,line = 4.5)
  title(xlab=xlab, cex.lab = cex.lab,line = 3.5)
  if(!is.null(Symbol))
  {
    legend("topleft", inset=c(FeatSymbol[1],FeatSymbol[2]), legend=Symbol, cex=FeatSymbol[3], bty="n")
  }
  for(i in 1:length(corry2)){lines(c(-1,max(corrx2)),c(corry2[i],corry2[i]),lty=3,lwd=2,col="gray80",type = "l")}
  curve(ff,add=TRUE,col="black",lwd=4,lty=LTY[1])
  curve(ffup,add=TRUE,col="gray30",lwd=3,lty=LTY[2])
  curve(fflo,add=TRUE,col="gray30",lwd=3,lty=LTY[2])
  if(!is.null(Limit1))
  {
    curve(ff,to = Limit1[1],add=TRUE,col="white",lwd=4.9)
    curve(ffup,to = Limit1[2],add=TRUE,col="white",lwd=3.9)
    curve(fflo,to = Limit1[3],add=TRUE,col="white",lwd=3.9)
  }
  if(!is.null(Limit2))
  {
    curve(ff,from = Limit2[1],add=TRUE,col="white",lwd=4.9)
    curve(ffup,from = Limit2[2],add=TRUE,col="white",lwd=3.9)
    curve(fflo,from = Limit2[3],add=TRUE,col="white",lwd=3.9)
  }  
  if(!is.null(Error))
  {
    F <- Y
    L <- F-Error
    U <- F+Error
    plotCI(x, F, ui=U, li=L, add=TRUE, col=ColPoint,scol="black", pch=PCH, cex=3,lwd=3)
  }else{points(x, Y,col="gray60",pch=19,lwd=3,cex=3)}
  points(x, Y,col="gray10",lwd=3,cex=3)
  box()
  if(!is.null(Text))
  {
    legend("topleft", inset=c(FeatText[1],FeatText[2]), legend=Text, cex=FeatText[3], bty="n")
  }
  
  switch(ADD,
         ADD_0={
           show=TRUE 
         },
         ADD_i={
           show=TRUE
         },
         ADD_f={
           show=FALSE
         },
         No={
           show=show
         },
         stop("Enter something that switches me!")
  )
  
  if(show==FALSE)
  {
    dev.off()  
  }
  
  if(show==FALSE)
  {
    sink(paste(dir,"/results.txt",sep=""))    
  }
  
  if(modelling==TRUE)
  {
    print(summary(out))
    Dfinal2=data.frame(x,y,Ajustado=round(predict(out),4));colnames(Dfinal2)[2]<-"Observado"
    print(Dfinal2)
  }else{
    Dfinal2=data.frame(x,y,Ajustado=round(ff(x),4));colnames(Dfinal2)[2]<-"Observado"
    print(Dfinal2)
  }
  
  if(show==FALSE)
  {
    sink()
  }
  
  return(print("finished!"))
}