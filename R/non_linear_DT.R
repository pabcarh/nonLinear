#' non_linear_DT
#'
#' A one liner.
#' 
#' Here more details
#'
#' @param x a vector
#' @param x a vector
#' @param Error a vector
#' @param yl a vector with two values
#' @param xl a vector with two values
#' @param Fyx a Fyx; default is 1
#' @param dir a path; default is 'D:/'
#' @return character a string; default is 'Hello, world!'
#' @author author
#' @family example
#' @example inst/examples/ex_non_linear_DT.R
#' @export
non_linear_DT<-function(x,y,Error,yl,xl,Fyx,dir)
{
  setwd(dir)
  # modelling
  out <- nls(Fyx, start = Ival,trace = FALSE)
  SE<-(summary(out))[3]$sigma
  dff<-(summary(out))[4]$df[2]
  
  sal <- coef(out)
  for (i in names(sal))
  {
    temp <- sal[i]
    storage.mode(temp) <- "double"
    assign(i, temp)
  }
  
  ff <- function(x){1/exp(eval(Fyx[[3]]))}
  ffup <- function(x){1/(exp(eval(Fyx[[3]])+SE*qt(0.975,dff)))}
  fflo <- function(x){1/(exp(eval(Fyx[[3]])-SE*qt(0.975,dff)))}
  
  corrx2<-seq(0,30,5)
  corry2<-seq(0,0.3,0.05)
  
  png("Plot_NonLinear.png", width = 12, height = 10, units = 'in', res = 300) # para el grafico
  plot(x, 1/exp(y), ylab="development rate (1/day)", xlab="temperature (°C)", col="transparent", pch=19,axes=F,xlim=c(0,30),ylim=c(0,0.3),cex = 1.5,cex.lab=1.5, cex.axis=1.5,cex.sub=1.5)
  axis(1, corrx2,lwd=2)
  axis(2, corry2,labels = round(corry2,2),las=2,lwd=2)
  for(i in 1:length(corry2)){lines(c(-1,max(corrx2)+5),c(corry2[i],corry2[i]),lty=3,lwd=2,col="gray80",type = "l")}
  curve(ff,add=TRUE,col="black",lwd=4)
  curve(ffup,add=TRUE,col="gray30",lwd=3,lty=3)
  curve(fflo,add=TRUE,col="gray30",lwd=3,lty=3)
  F <- 1/exp(y)
  L <- F-Error
  U <- F+Error
  plotCI(x, F, ui=U, li=L, add=TRUE, col="gray60",scol="black", pch=19, cex=3,lwd=3)
  points(x, 1/exp(y),col="gray10",lwd=3,cex=3)
  box()
  
  dev.off()
  
  sink("results.txt")
  print(summary(out))
  Dfinal2=data.frame(x,y,Ajustado=round(predict(out),4));colnames(Dfinal2)[2]<-"Observado"
  print(Dfinal2)
  sink()
}