
#PS2


#start creating the function
bendfordlaw <- function(x, returnm=TRUE, returnd=TRUE){
  #Create initial list with the input only
  output <- list(digits=x)
  #The lenght of the input
  n <- length(x)
  #If user ask for m
  if (returnm==TRUE) {
    #create m statistic
    m= max(x-log10(1+(1/(1:n))))
    #Create stars
    star=ifelse(m<0.851,"None",ifelse(m<0.967,"*", ifelse(m<1.212, "**",  "***")))
    #put both in a list
    m=list(m, star)
    #put in the output
    output$m=m
  }
  #If user ask for d
  if (returnd==TRUE) {
    d=sqrt(sum((x-log10(1+(1/(1:n))))^2))
    #Create stars    
    star=ifelse(d<0.851,"None",ifelse(d<0.967,"*", ifelse(d<1.212, "**",  "***")))
    #put both in a list    
    d=list(d, star)
    #put in the output
    output$d=d
  }
  return(output)
}

x <- c(0,0,0.1,0.1,0,0,0,0,0.8)
bendfordlaw(x, returnm=F)
