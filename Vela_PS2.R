
#PS2


x <- c(0,0,0,0,0,0,0,0,1)
thefunction <- function(x, returnm=TRUE, returnd=TRUE){
  m=NULL
  d=NULL
  if (returnm==TRUE) {
    m= max(x-log(1+(1/(1:9)), base=10))
    star=ifelse(m<0.851,"NONE",ifelse(m<0.967,"*", ifelse(m<1.212, "**",  "***")))
    m=list(m, star)

