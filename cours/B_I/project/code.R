# 
# 
# fileName <- "/Users/dahdiarra/master2/cours/B_I/project/DBLP_Subset.txt"
# brute_data <- readChar(fileName, file.info(fileName)$size)
# 
# data_by_article <- unlist(strsplit(brute_data,"\n\n"))
# 
# readByArticle <- function(article){
#   value <- c("","","","","","","")
#    vect <- unlist(strsplit(article,"#"))
#    for (line in vect) {
#      identif = substring(line,1,1)
#      
#         if("i" == identif && substring(line,1,5) == "index"){
#           value[1] <- substring(line,6)
#         }
#         if("*"== identif){
#           value[2] <- substring(line,2)
#         }
#         if("@"== identif){
#           value[3] <- substring(line,2)
#         }
#         if("t" == identif){
#           value[4] <- substring(line,2)
#         }
#         if("c"== identif){
#           value[5] <- substring(line,2)
#         }
#         if("%"== identif){
#           value[6] <- paste(value[6],substring(line,2),sep = ",")
#         }
#         if("!"== identif){
#           value[7] <- substring(line,2)
#         }
#    }
#    
#   value[3] <-  str_replace_all(value[3],pattern = "\n",replacement = ",")
#    
#   return(unlist(str_remove_all(value,"\n")))
# }
# 
# mat <- lapply(data_by_article[1:20],FUN = readByArticle)
# 
# dat <- data.frame()
# 
# for(d in mat){
#  print((d))
#   break()
# }


heart_data <- read_csv("/Users/dahdiarra/master2/cours/R_Shiny/heart.csv")
# heart_data <- read_csv("./heart.csv")
categoryCol <- c("sex","cp", "fbs", "restecg","exang","slope","ca","thal","target")
qualitativeCol <- c("age","trestbps","chol","thalach","oldpeak")
heart_data[categoryCol] <- lapply(heart_data[categoryCol],FUN = factor)

essay <- function(val){
  print(val)
}
