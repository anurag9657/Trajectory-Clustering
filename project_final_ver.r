## 1. Setting up "trajcomp" package

#library(devtools)
#install_github("mwernerds/trajcomp/r-package/trajcomp",force = TRUE)

## 2. Setting up library, working directory and data

## 2.1 library
library(trajcomp)

## 2.2 Working directory
setwd("V:/Users/ahohl/Classes/spring17/ITCS8156")

## 2.3 Setting up data
data1 <- read.table('data_scaled.csv',sep=",", fill = TRUE, header=T)

data2 <- reshape(data1[,1:6],
               varying=2:6,
               direction="long", sep="")
data3 <- reshape(data1[,c(1, 7:11)],
                 varying=2:6,
                 direction="long", sep="")
data1 <- merge(data2, data3, by=c("NID", "id", "time"))
rm(data2, data3)


k <- 1
data <- data1[(5*k-4):(5*k), 4:5]
for (k in 2:4015) {
  data <- rbind(data, c(NaN, NaN))
  data <- rbind(data, data1[(5*k-4):(5*k), 4:5])
}

#subset the data
data4 <- data[1:12000,]
rm(data1)

# not actually elegant, but very clear for beginners:
cluster_to_color <- function(r)
{
  if (r == 13){
    return ("slategrey");
  }
  if (r == 12){
    return ("navy");
  }
  if (r == 11){
    return ("khaki1");
  }
  if (r == 10){
    return ("indianred");
  }
  if (r == 9){
    return ("deeppink");
  }
  if (r == 8){
    return ("brown");
  }
  if (r == 7){
    return("bisque");
  }
  if (r == 6){
    return ("purple");
  }
  if (r == 5){
    return ("orange");
  }
  if (r == 4){
    return ("yellow");
  }
  if (r == 3){
    return ("red");
  }
  if (r == 2){
    return("green");
  }
  if (r == 1){
    return ("blue");
  }
}

epsilon <- c(0.02, 0.025, 0.03, 0.035)
MinLns <- c(35,45,55)

library(plyr) # for efficient grouping using ddply

for (i in epsilon) {
  for (j in MinLns) {
    
    segs <- traclus(data4,i,j)
    
    png(paste0("myplot", i, "_", j,".png"), width=10, height=10, units="in", res=300)
    plot(data4,t="l",col="gray")
    ddply(segs,.(cluster),function(x){
      lines(c(x$x1,x$x2),c(x$y1,x$y2),col=cluster_to_color(x$cluster[1]));
    })
    
    
    print(i)
    print(j)
    
    dev.off() #only 129kb in siz
  }
}

