library(corrplot)
library(RColorBrewer)
library(dplyr)
library(hash)
library(arules)
library(factoextra)

#The first step was made by hand: adding the ISO codes of each country on the 2 datasets to make the merge easier

data <- read.csv("~/Cours/MLDM/2ndsemester/DMKD/Project/world_ISO.csv")
duolingo <- read.csv("~/Cours/MLDM/2ndsemester/DMKD/Project/duolingo_ISO.csv")

duolingo$withoutEnglish <- duolingo$pop1_2023

duolingo <- within(duolingo, withoutEnglish[withoutEnglish == "English"] <- pop2_2023[withoutEnglish == "English"])

dataset <- data

#Creating 2 new columns to merge the 2 datasets
dataset$duolingo <- integer(nrow(dataset))
dataset$duolingowoEnglish <- integer(nrow(dataset))

# Verifying if the countries are correctly linked
for(i in 1:nrow(duolingo)){
  name <- duolingo[i, "country"]
  code <- duolingo[i, "ISO3"]
  new_name <- data[data$ISO == code,"Country"]
  if(name != new_name){
    print(i)
    print(name)
    print(new_name)
  }
}

#Since we have Duolingo information on 193 rows only, we will drop the rows for which we do not have this information, and insert the data for the others
rowstodrop = c()


for(i in 1:nrow(dataset)){
  code <- dataset[i, "ISO"]
  if(code %in% duolingo$ISO3){
    dataset[i, "duolingo"] <- duolingo[duolingo$ISO3 == code, "pop1_2023"]
    dataset[i, "duolingowoEnglish"] <- duolingo[duolingo$ISO3 == code, "withoutEnglish"]
  }
  else{
    rowstodrop <- c(rowstodrop, i)
  }
}

print(rowstodrop)

#We can also remove the columns that contain information that is not useful to our study
columnstodrop <- c(2,4,9,10,22)

dataset <- dataset[-rowstodrop, -columnstodrop]

#A lot of the columns are of type character because of the commas in the numbers
commas_columns = c(2,4,5,7,8,24,29)

for (col in commas_columns){
  dataset[, col] <- as.numeric(gsub(',', '', dataset[, col]))
}

#We can now do the same with the columns containing a percentage
percent_columns = c(3,9,12,15,16,22,25,26,27,28)

for (col in percent_columns){
  dataset[, col] <- as.numeric(gsub(',', '', substr(dataset[,col], 1, nchar(dataset[,col])-1)))
}

#And with those containing an amount of money
money_columns = c(13,14,20)

for (col in money_columns){
  dataset[, col] <- as.numeric(gsub(',', '', substr(dataset[,col], 2, nchar(dataset[,col]))))
}

#Creation of a dataset containing only the numerical values

non_numeric <- c(1,10,21,32,33)

numerical_dataset <- dataset[,-non_numeric]

#Replacing the missing values with the average of the column

for(i in 1:ncol(numerical_dataset)){
  numerical_dataset[,i][is.na(numerical_dataset[,i])] <- mean(numerical_dataset[,i], na.rm=TRUE)
}

#Doing and showing a PCA

pca <- prcomp(numerical_dataset, scale=TRUE)

pca$rotation

fviz_pca_biplot(pca, geom.ind = c("point"), select.var=list(cos2=0.5))

#Correlation between the different values

corrplot(cor(numerical_dataset), method = "color")

#Proportions of languages learned in Duolingo in 2023

palette <- brewer.pal(n_distinct(dataset$duolingowoEnglish), "Set3") 

pie(table(dataset$duolingo), col=palette)

pie(table(dataset$duolingowoEnglish), col=palette)

#Getting a list of the learned languages

languages <- c()

for(i in 1:nrow(dataset)){
  if(! dataset[i,"duolingowoEnglish"] %in% languages){
    languages <- c(languages, dataset[i, "duolingowoEnglish"])
  }
}

print(languages)

#One-hot encoding languages

dataset$English <- integer(nrow(dataset))
dataset$German <- integer(nrow(dataset))
dataset$French <- integer(nrow(dataset))
dataset$Spanish <- integer(nrow(dataset))
dataset$Italian <- integer(nrow(dataset))
dataset$Russian <- integer(nrow(dataset))
dataset$Korean <- integer(nrow(dataset))
dataset$Japanese <- integer(nrow(dataset))
dataset$Portuguese <- integer(nrow(dataset))
dataset$Chinese <- integer(nrow(dataset))
dataset$Hindi <- integer(nrow(dataset))
dataset$Hebrew <- integer(nrow(dataset))
dataset$Arabic <- integer(nrow(dataset))

dataset$duoGerman <- integer(nrow(dataset))
dataset$duoFrench <- integer(nrow(dataset))
dataset$duoSpanish <- integer(nrow(dataset))
dataset$duoItalian <- integer(nrow(dataset))
dataset$duoRussian <- integer(nrow(dataset))
dataset$duoKorean <- integer(nrow(dataset))
dataset$duoJapanese <- integer(nrow(dataset))
dataset$duoPortuguese <- integer(nrow(dataset))
dataset$duoChinese <- integer(nrow(dataset))
dataset$duoHindi <- integer(nrow(dataset))
dataset$duoHebrew <- integer(nrow(dataset))
dataset$duoArabic <- integer(nrow(dataset))

for(i in 1:nrow(dataset)){
  if( dataset[i,"Official.language"] %in% c("English", languages)){
    dataset[i,33+match(dataset[i,"Official.language"], c("English", languages))] <- 1
  }
  if( dataset[i,"duolingowoEnglish"] %in% languages){
    dataset[i,46+match(dataset[i,"duolingowoEnglish"], languages)] <- 1
  }
}

#Checking columns with missing values and replacing them with the mean

colSums(is.na(dataset))

for(i in 1:ncol(dataset)){
  dataset[,i][is.na(dataset[,i])] <- mean(dataset[,i], na.rm=TRUE)
}

colSums(is.na(dataset))

#Dropping non-numerical values to create a binary dataset that we will use to create transactions
binary_dataset <- dataset[, -c(1,10,21,32,33)]

for(j in 1:31){
  avg <- mean(binary_dataset[,j])
  for(i in 1:nrow(binary_dataset)){
    if(binary_dataset[i,j] <= avg){
      binary_dataset[i,j] <- 0
    }
    else{
      binary_dataset[i,j] <- 1
    }
  }
}

#Creating the transactions

trans_list <- list()
for(i in 1:nrow(binary_dataset)){
  trans <- c()
  for(j in 1:ncol(binary_dataset)){
    if(binary_dataset[i,j] == 1){
      trans <- c(trans, colnames(binary_dataset)[j])
    }
  }
  trans_list[[length(trans_list)+1]] <- trans
}

#Looking at the rules with sufficient support and confidence

rules <- apriori(trans_list, sup=0.2, conf=0.5)
inspect(rules)

#French is the only language we found results for

filtered_rules <- subset(rules, subset=rhs %pin% "duoFrench")

inspect(filtered_rules)

#Plotting a correlation matrix between our different values and  the learnt languages

#corrplot(cor(numerical_dataset, dataset[,47:58]), method = "color")
#corrplot(cor(dataset[,34:46], dataset[,47:58]), method = "color")