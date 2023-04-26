library(readr)

Cleaned_Dataset_Source_Export_2 <- read_csv("C:/Users/Jeffrey/Box/Jeffrey Mai 1st Year Project (IT API admin)/Stats/Final Analysis Sheets/Cleaned_Dataset_Source_Export_2.csv")
FNSTETSimplified <- read_csv("C:/Users/Jeffrey/Box/Jeffrey Mai 1st Year Project (IT API admin)/Stats/Final Analysis Sheets/FNSTETSimplified.csv")
Cleaned_Dataset_Source_Export_2 <- read_csv("~/Library/CloudStorage/Box-Box/Jeffrey Mai 1st Year Project/Stats/Final Analysis Sheets/Cleaned_Dataset_Source_Export_2.csv")
FNSTETSimplified <- read_csv("~/Library/CloudStorage/Box-Box/Jeffrey Mai 1st Year Project/Stats/Final Analysis Sheets/FNSTETSimplified.csv")

Double_Cleaned_Export <- data.frame(File_Name = character(),
                                 Child_ID = integer(),
                                 Child_Age = integer(),
                                 Start_Time = integer(),
                                 End_Time = integer(),
                                 Infant_Voc = character(),
                                 Adult_Utterance_Direction = character(),
                                 Music = character(),
                                 Duration = double())

File_Name <- NA
Child_ID <- NA
Child_Age <- NA
Infant_Voc <- NA
Adult_Utterance_Direction <- NA
Music <- NA
Duration <- NA

for(x in 1:nrow(FNSTETSimplified)){
  
  Bound_Start_Time <- FNSTETSimplified$StartTimeSS[x]
  Bound_End_Time <- FNSTETSimplified$EndTimeSS[x]
  File_Name <- FNSTETSimplified$FileName[x] #get file name from FNSTET to compare
  
  for(j in 1:nrow(Cleaned_Dataset_Source_Export_2)){
   
    temp_Name <- Cleaned_Dataset_Source_Export_2$File[j]
    if(nchar(temp_Name) == 22){
      temp_Name <- substr(temp_Name, start = 1, stop = 11) #removes _Edited.eaf
    } else if(nchar(temp_Name) == 23){
      temp_Name <- substr(temp_Name, start = 1, stop = 12) #removes _Edited.eaf
    }
    
    Start_Time <- Cleaned_Dataset_Source_Export_2$BeginTime[j]
    End_Time <- Cleaned_Dataset_Source_Export_2$EndTime[j]
    
    Child_ID <- substr(temp_Name, start = 1, stop = 4)
    Child_Age <- substr(temp_Name, start = 6, stop = 9)
    
    if(as.numeric(Child_Age) == 0002){
      Child_Age <- 3
    } else if(as.numeric(Child_Age) == 0003){
      Child_Age <- 3
    } else if(as.numeric(Child_Age) == 0005){
      Child_Age <- 6
    } else if(as.numeric(Child_Age) == 0006){
      Child_Age <- 6
    } else if(as.numeric(Child_Age) == 0006){
      Child_Age <- 9
    } else if(as.numeric(Child_Age) == 0009){
      Child_Age <- 9
    } else if(as.numeric(Child_Age) == 0105){
      Child_Age <- 18
    } else if(as.numeric(Child_Age) == 0106){
      Child_Age <- 18
    }
    
    if(File_Name == temp_Name){
      if(Bound_Start_Time <= Cleaned_Dataset_Source_Export_2$BeginTime[j]  && Bound_End_Time >= Cleaned_Dataset_Source_Export_2$EndTime[j]){
        if(!is.na(Cleaned_Dataset_Source_Export_2$AdultUtteranceDirection[j])){
          if(Cleaned_Dataset_Source_Export_2$AdultUtteranceDirection[j] == "N"){
            if(Cleaned_Dataset_Source_Export_2$Music[j] == "LL"){
              Adult_Utterance_Direction <- "N"
              Music <- "LL"
              Duration <- Cleaned_Dataset_Source_Export_2$Duration[j]
            } else if(Cleaned_Dataset_Source_Export_2$Music[j] == "NL"){
              Adult_Utterance_Direction <- "N"
              Music <- "NL"
              Duration <- Cleaned_Dataset_Source_Export_2$Duration[j]
            } else if(Cleaned_Dataset_Source_Export_2$Music[j] == "NM"){
              Adult_Utterance_Direction <- "N"
              Music <- "NM"
              Duration <- Cleaned_Dataset_Source_Export_2$Duration[j]
            }
          }else if(Cleaned_Dataset_Source_Export_2$AdultUtteranceDirection[j] == "U"){ #covers all AU that has no clear direction
            if(Cleaned_Dataset_Source_Export_2$Music[j] == "LL"){
              Adult_Utterance_Direction <- "U"
              Music <- "LL"
              Duration <- Cleaned_Dataset_Source_Export_2$Duration[j]
            } else if(Cleaned_Dataset_Source_Export_2$Music[j] == "NL"){
              Adult_Utterance_Direction <- "U"
              Music <- "NL"
              Duration <- Cleaned_Dataset_Source_Export_2$Duration[j]
            } else if(Cleaned_Dataset_Source_Export_2$Music[j] == "NM"){
              Adult_Utterance_Direction <- "U"
              Music <- "NM"
              Duration <- Cleaned_Dataset_Source_Export_2$Duration[j]
            }
          }else if(Cleaned_Dataset_Source_Export_2$AdultUtteranceDirection[j] == "T"){ #covers all AU that is directed to infant
            if(Cleaned_Dataset_Source_Export_2$Music[j] == "LL"){
              Adult_Utterance_Direction <- "T"
              Music <- "LL"
              Duration <- Cleaned_Dataset_Source_Export_2$Duration[j]
            } else if(Cleaned_Dataset_Source_Export_2$Music[j] == "NL"){
              Adult_Utterance_Direction <- "T"
              Music <- "NL"
              Duration <- Cleaned_Dataset_Source_Export_2$Duration[j]
            } else if(Cleaned_Dataset_Source_Export_2$Music[j] == "NM"){
              Adult_Utterance_Direction <- "T"
              Music <- "NM"
              Duration <- Cleaned_Dataset_Source_Export_2$Duration[j]
            }
          }
        }else if(!is.na(Cleaned_Dataset_Source_Export_2$InfantVocType[j])){
          if(Cleaned_Dataset_Source_Export_2$InfantVocType[j] == "C"){
            Infant_Voc <- "C"
            Duration <- Cleaned_Dataset_Source_Export_2$Duration[j]
          } else if(Cleaned_Dataset_Source_Export_2$InfantVocType[j] == "X"){
            Infant_Voc <- "X"
            Duration <- Cleaned_Dataset_Source_Export_2$Duration[j]
          } else if(Cleaned_Dataset_Source_Export_2$InfantVocType[j] == "R"){
            Infant_Voc <- "R"
            Duration <- Cleaned_Dataset_Source_Export_2$Duration[j]
          } else if(Cleaned_Dataset_Source_Export_2$InfantVocType[j] == "L"){
            Infant_Voc <- "L"
            Duration <- Cleaned_Dataset_Source_Export_2$Duration[j]
          }
        }
        Double_Cleaned_Export[nrow(Double_Cleaned_Export) +1,] <- c(File_Name,
                                                              Child_ID,
                                                              Child_Age,
                                                              Start_Time,
                                                              End_Time,
                                                              Infant_Voc,
                                                              Adult_Utterance_Direction,
                                                              Music,
                                                              Duration)
    
        
        Child_ID <- NA
        Child_Age <- NA
        Infant_Voc <- NA
        Adult_Utterance_Direction <- NA
        Music <- NA
        Duration <- NA
      }
    }
  }
}

write.csv(Double_Cleaned_Export, "C:/Users/Jeffrey/Box/Jeffrey Mai 1st Year Project (IT API admin)/Stats/Final Analysis Sheets/Double_Cleaned_Export.csv")
