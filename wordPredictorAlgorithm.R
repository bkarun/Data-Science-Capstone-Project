#############################################################################
### The code below has the algorithm for next word prediction used in the ###
### word predictor app. The program reads the Ngram frequencey data frames ##
### created in the Milestone report program and runs through the Petagrams,##
### Quadragrams, Trigrams and Bigrams in that order, to find matching     ###
### phrases and returns the subsequent words in the phrases. The last part ##
### tests the accuracy of the prediction using out of sample phrases       ##
### generated from the same corpora                                       ###
#############################################################################

library(stringi)
library(stringr)

#Read in the frequencey data frames

load('UnigramData.RData')
load('BigramData.RData')
load('TrigramData.RData')
load('QuadragramData.RData')
load('PentagramData.RData')

#Separate the last word to create the input phrase and output words separately for each Ngram

PentaPre <- gsub(" $", "", gsub('\\w+$', "", PentagramData[,1]))
PentaPost <- stri_extract_last_words(PentagramData[,1])
QuadraPre <- gsub(" $", "", gsub('\\w+$', "", QuadragramData[,1]))
QuadraPost <- stri_extract_last_words(QuadragramData[,1])
TriPre <- gsub(" $", "", gsub('\\w+$', "", TrigramData[,1]))
TriPost <- stri_extract_last_words(TrigramData[,1])
BiPre <- gsub(" $", "", gsub('\\w+$', "", BigramData[,1]))
BiPost <- stri_extract_last_words(BigramData[,1])


#Function to check for phrases in Pentagram
checkPenta <- function(string1, nxt= NA){
                    
                     i = 1
                     
                    while(numWords < 4 & i <= length(PentaPost)){
                       if(string1 == PentaPre[i]){
                         if(is.na(nxt)){ 
                           nxt <- PentaPost[i]
                          }
                         else{
                           nxt <- paste(nxt, PentaPost[i], sep = "    ")
                         }
                         numWords <<- numWords + 1
                       }
                      i = i + 1
                     
                    }
                    return(nxt)
                 }                       

#Function to check for phrase in Quadragram
checkQuadra <- function(string1, nxt = NA){
                  
                  i=1
                  while(numWords < 4 & i <= length(QuadraPost)){
                      if(string1 == QuadraPre[i]){
                        if(is.na(nxt)){
                          nxt = QuadraPost[i]
                          numWords <<- numWords + 1  
                        }
                        else if(!(QuadraPost[i] %in% str_trim(strsplit(nxt, "   ")[[1]]))){
                          nxt <- paste(nxt, QuadraPost[i], sep = "   ")
                          numWords <<- numWords + 1  
                        }
                      }
                    i= i + 1
                  }
                  return (nxt)  
}


#Function to check in Trigram
checkTri <- function(string1, nxt = NA){
                  
                
                  i = 1
                  while(numWords < 4 & i <= length(TriPost)){
                      if(string1 == TriPre[i]){
                        if(is.na(nxt)){
                          nxt = TriPost[i]
                          numWords <<- numWords + 1 
                        }
                        else if(!(TriPost[i] %in% str_trim(strsplit(nxt, "   ")[[1]]))) {
                         nxt   <- paste(nxt, TriPost[i], sep = "   ")  
                         numWords <<- numWords + 1 
                        }
                      
                      }
                  i = i + 1
                 }
             return (nxt)  
            }


#Function to check in Bigram
checkBi <- function(string1, nxt=NA){
                 
                 i = 1
                 while(numWords < 4 & i <= length(BiPost)){
                      if(string1 == BiPre[i]){
                        if(is.na(nxt)){
                          nxt = BiPost[i]
                          numWords <<- numWords + 1 
                          }
                        else if (!(BiPost[i] %in% str_trim(strsplit(nxt, "   ")[[1]]))){
                          nxt <- paste(nxt, BiPost[i], sep = "   ")
                          numWords <<- numWords + 1 
                        }
                           
                       }
                    i = i + 1
                }
                 return (nxt) 
}


#Putting it all together, word predictor function 
wordPredictor <- function(gstring){
numWords <<-  0
word <- "The   For   That   You"
gstring <- tolower(gstring)
gstring <- gsub("[[:punct:]]", "", gstring)
gstring <- gsub("\\s+"," ",gstring)
gstring <- gsub('[[:digit:]]+', '', gstring)
gstring <- gsub(" $", "", gstring)
glength = ifelse(is.na(gstring), 0,stri_count(gstring, regex="\\S+"))
gstring <- tolower(gstring)
if(glength >= 4){
       if(glength > 4){
         gstring = paste (word(gstring,-(4:1)), sep = "", collapse = " ")
       }
       word= checkPenta(gstring)
        print('Penta')
        print(word)
        print(numWords)
       if(numWords < 4){
         gstring = gsub("^ ", "", gsub('^\\w+', "", gstring))  
         word = checkQuadra(gstring, word)
         print('Quadra')
         print(word)
         print(numWords)
         if(numWords < 4){
           gstring = gsub("^ ", "", gsub('^\\w+', "", gstring)) 
           word = checkTri(gstring, word)
           print('Tri')
           print(word)
           print(numWords)
           if(numWords < 4) {
             gstring = gsub("^ ", "", gsub('^\\w+', "", gstring)) 
             word = checkBi(gstring, word)
             print('Bi')
             print(word)
             print(numWords)
           }         
          }
       }
    }else if (glength == 3){
     word = checkQuadra(gstring)
     if(numWords < 4){
        gstring = gsub("^ ", "", gsub('^\\w+', "", gstring)) 
        word = checkTri(gstring, word)
        if(numWords < 4){
          gstring = gsub("^ ", "", gsub('^\\w+', "", gstring)) 
          word = checkBi(gstring, word)
        }         
      }
  }else if (glength == 2){
    word = checkTri(gstring)
    if(numWords < 4){
      gstring = gsub("^ ", "", gsub('^\\w+', "", gstring)) 
      word = checkBi(gstring, word)
      }
  }else if (glength == 1){
    word = checkBi(gstring)
  }
return(word(word, 1))
}



###Test accuracy 

#Single word
sampleBi <- BigramData[sample(nrow(BigramData), 100), ]
sampleBiPre <- gsub(" $", "", gsub('\\w+$', "", sampleBi[,1]))
sampleBiPost <- stri_extract_last_words(sampleBi[,1])
sampleBiPredict <- vector()
for(i in 1:length(sampleBiPre)){
     sampleBiPredict[i]= wordPredictor(sampleBiPre[i])
   }
sum(ifelse(sampleBiPredict == sampleBiPost, 1, 0))

# Two word Phrases
sampleTri <- TrigramData[sample(nrow(TrigramData), 100), ]
sampleTriPre <- gsub(" $", "", gsub('\\w+$', "", sampleTri[,1]))
sampleTriPost <- stri_extract_last_words(sampleTri[,1])
sampleTriPredict <- vector()
for(i in 1:length(sampleTriPre)){
     sampleTriPredict[i]= wordPredictor(sampleTriPre[i])
   }
sum(ifelse(sampleTriPredict == sampleTriPost, 1, 0))




# Four word phrases

samplePenta <- PentagramData[sample(nrow(PentagramData), 100), ]
samplePentaPre <- gsub(" $", "", gsub('\\w+$', "", samplePenta[,1]))
samplePentaPost <- stri_extract_last_words(samplePenta[,1])
samplePentaPredict <- vector()
for(i in 1:length(samplePentaPre)){
  samplePentaPredict[i]= wordPredictor(samplePentaPre[i])
}
sum(ifelse(samplePentaPredict == samplePentaPost, 1, 0))







