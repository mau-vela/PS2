
#PS2
setwd("C:/Users/MauricioAndresVela/Documents/R/Clase/PS2")

#start creating the function
bendfordlaw <- function(x, returnm=TRUE, returnd=TRUE){
  #Create initial list with the argument
  output <- list(digits=x)
  #The lenght of the argument
  n <- length(x)
  #If user ask for m
  if (returnm==TRUE) {
    #create m statistic
    m= max(x-log10(1+(1/(1:n))))
    #Create stars based on the table
    star=ifelse(m<0.851," ",ifelse(m<0.967,"*", ifelse(m<1.212, "**",  "***")))
    #put both in a list
    m=list(m=m, star=star)
    #put in the output
    output$m=m
  }
  #If user ask for d
  if (returnd==TRUE) {
    d=sqrt(sum((x-log10(1+(1/(1:n))))^2))
    #Create stars    1.212 1.330 1.569
    star=ifelse(d<1.212," ",ifelse(d<1.330,"*", ifelse(d<1.569, "**",  "***")))
    #put both in a list    
    d=list(d=d, star=star)
    #put in the output
    output$d=d
  }
  return(output)
}

#Example
x <- c(0,0,0.1,0.1,0,0,0,0,0.8)
y <- bendfordlaw(x, returnm=F)

#Start creating print.benfords
print.benfords <- function(y){
  #Create empty lists
  statistics <- list()
  stars <- list()
  rownames <- list()
  #Check if the argument has m statistic
  if (!is.null(y[["m"]])){
      #Put in each list the correspoding value
      stars$m <- y$m$star
      statistics$m <- y$m$m
      #Create a rownames for latter use
      rownames$m <- "M statistic"
  }
  #Check if the argument has d statistic  
  if (!is.null(y[["d"]])){
    #Put in each list the correspoding value    
    stars$d <- y$d$star
    statistics$d <- y$d$d
    #Create a rownames for latter use    
    rownames$d <- "D statistic"    
  }  
  #Final output will create a dataframe based on the values in the lists
  output <- structure(list(statistics=statistics, stars=structure(stars, class="character")), 
                      .Names = c("Statistic", "Stars"), class = "data.frame", row.names =as.character(rownames))
  #Print the dataframe output
  print(output)
  #Put the significant values codes
  cat("--- \n Signif. codes:  '***' 0.01 '**' 0.05 '*' 0.1")
}

#Example
print.benfords(y)

#Create function to save print output to a csv file
tocsvbenfords <- function(y){
  #start capturing all from the console
  sink(file="benford.csv")
  print.benfords(y)
  sink()
}

#Create the file from the example
tocsvbenfords(y)





