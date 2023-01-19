# Week1 ESTDA
temp <- read.csv("Data/Temp_China.csv")

# conames():define the name of each colomn, and [4:ncol(temp)] means fron 4th to the last one colomns
colnames(temp)[4:ncol(temp)] <- as.character(c(1951:2002))

# Usage:paste("a character string you want to concatenate", a number you want to concatenate with the character, separator)
station<-paste("sta",1:nrow(temp),sep = "")
station


# define the colname derectly, but create the new column of station name and combine the new column and 'temp' table
temp<-cbind(station,temp)
temp

# convert temp dataset to matrix
temp_matrix<-data.matrix(temp[,5:ncol(temp)])



#  explore their general characteristics: the mean and the standard deviation
mu = mean(temp_matrix)
mu

# standard deviation

