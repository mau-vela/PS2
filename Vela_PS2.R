
#PS2


#start creating the function
bendfordlaw <- function(x, returnm=TRUE, returnd=TRUE){
  m <- NULL
  d <- NULL
  n <- length(first_digits)
  if (returnm==TRUE) {
    m= max(x-log10(1+(1/(1:n))))
    star=ifelse(m<0.851,"NONE",ifelse(m<0.967,"*", ifelse(m<1.212, "**",  "***")))
    m=list(m, star)
  }
  if (returnd==TRUE) {
    d=sqrt(sum((x-log10(1+(1/(1:n))))^2))
    star=ifelse(d<0.851,"NONE",ifelse(d<0.967,"*", ifelse(d<1.212, "**",  "***")))
    d=list(d, star)
  }
  return(list(digits=x, m=m,d=d))
}



x <- c(0,0,0.1,0.1,0,0,0,0,0.8)
bendfordlaw(x, returnm=F)
