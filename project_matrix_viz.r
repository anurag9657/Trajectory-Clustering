#load matrix.csv
matrix <- read.csv("V:/Users/ahohl/Classes/spring17/ITCS8156/matrix.csv", header = TRUE, row.names = 1, sep = ",")

#put in matrix data structure
M <- as.matrix(matrix)

#spit out row and coulmn number of maximum co-occurrence pairs
which(M == max(M), arr.ind = TRUE)
#which(M > 10, arr.ind = TRUE)



#?
#M2 <- M[,which(!apply(M,2,FUN = function(x){all(x == 0)}))]
