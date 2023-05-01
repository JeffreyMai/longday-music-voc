library(readr)

Double_Cleaned_Export <- read_csv("C:/Users/Jeffrey/Box/Jeffrey Mai 1st Year Project (IT API admin)/Stats/Final Analysis Sheets/Double_Cleaned_Export.csv")
FNSTETSimplified <- read_csv("C:/Users/Jeffrey/Box/Jeffrey Mai 1st Year Project (IT API admin)/Stats/Final Analysis Sheets/FNSTETSimplified.csv")
Descriptives_Table <- read_csv("C:/Users/Jeffrey/Box/Jeffrey Mai 1st Year Project (IT API admin)/Stats/Final Analysis Sheets/Descriptives_Table.csv")
Double_Cleaned_Export <- read_csv("~/Library/CloudStorage/Box-Box/Jeffrey Mai 1st Year Project/Stats/Final Analysis Sheets/Double_Cleaned_Export.csv")
FNSTETSimplified <- read_csv("~/Library/CloudStorage/Box-Box/Jeffrey Mai 1st Year Project/Stats/Final Analysis Sheets/FNSTETSimplified.csv")
Descriptives_Table <- read_csv("~/Library/CloudStorage/Box-Box/Jeffrey Mai 1st Year Project/Stats/Final Analysis Sheets/Descriptives_Table.csv")

#creating empty data frame
Descriptives_Table = data.frame(File_Name = character(),
                                Child_ID = integer(),
                                Child_Age = integer(),
                                Start_Time = double(),
                                End_Time = double(),
                                Child_C = double(),
                                Child_X = double(),
                                Child_R = double(),
                                Child_L = double(),
                                Child_C_X = double(),
                                Adult_Lyrical_N = double(),
                                Adult_Lyrical_U = double(),
                                Adult_Lyrical_T = double(),
                                Adult_NonLyrical_N = double(),
                                Adult_NonLyrical_U = double(),
                                Adult_NonLyrical_T = double(),
                                Adult_NoMusic_N = double(),
                                Adult_NoMusic_U = double(),
                                Adult_NoMusic_T = double(),
                                Child_C_log = double(),
                                Child_X_log = double(),
                                Child_R_log = double(),
                                Child_L_log = double(),
                                Child_C_X_log = double(),
                                Adult_Lyrical_N_log = double(),
                                Adult_Lyrical_U_log = double(),
                                Adult_Lyrical_T_log = double(),
                                Adult_NonLyrical_N_log = double(),
                                Adult_NonLyrical_U_log = double(),
                                Adult_NonLyrical_T_log = double(),
                                Adult_NoMusic_N_log = double(),
                                Adult_NoMusic_U_log = double(),
                                Adult_NoMusic_T_log = double(),
                                Child_C_count = double(),
                                Child_X_count = double(),
                                Child_R_count = double(),
                                Child_L_count = double(),
                                Child_C_X_count = double(),
                                Adult_Lyrical_N_count = double(),
                                Adult_Lyrical_U_count = double(),
                                Adult_Lyrical_T_count = double(),
                                Adult_NonLyrical_N_count = double(),
                                Adult_NonLyrical_U_count = double(),
                                Adult_NonLyrical_T_count = double(),
                                Adult_NoMusic_N_count = double(),
                                Adult_NoMusic_U_count = double(),
                                Adult_NoMusic_T_count = double())

#initializing all variables to start 
File_Name <- 0 #file name
temp_Name <- 0 #temporary file name holder
Child_ID <- 0 #child ID
Child_Age <- 0 #child age
Start_Time <- 0 #annotation start time
End_Time <- 0 #annotation end time
Child_C <- 0 #child canonical vocalization
Child_X <- 0 #child non-canonical vocalization
Child_R <- 0 #child cry reflexive utterance
Child_L <- 0 #child laugh reflexive utterance
Child_C_X <- 0 #child total vocalization
Adult_Lyrical_N <- 0 #adult lyrical NOT to child
Adult_Lyrical_U <- 0 #adult lyrical to unknown
Adult_Lyrical_T <- 0 #adult lyrical to child
Adult_NonLyrical_N <- 0 #adult non-lyrical NOT to child
Adult_NonLyrical_U <- 0 #adult non-lyrical to unknown
Adult_NonLyrical_T <- 0 #adult non-lyrical to child
Adult_NoMusic_N <- 0 #adult no music NOT to child
Adult_NoMusic_U <- 0 #adult no music to unknown
Adult_NoMusic_T <- 0 #adult no music to child
Child_C_log <- 0 #log of child canonical vocalization
Child_X_log <- 0 #log of child non-canonical vocalization
Child_R_log <- 0 #log of child cry reflexive utterance
Child_L_log <- 0 #log of child laugh reflexive utterance
Child_C_X_log <- 0 #log of child total vocalization
Adult_Lyrical_N_log <- 0 #log of adult lyrical NOT to child
Adult_Lyrical_U_log <- 0 #log of adult lyrical to unknown
Adult_Lyrical_T_log <- 0 #log of adult lyrical to child
Adult_NonLyrical_N_log <- 0 #log of adult non-lyrical NOT to child
Adult_NonLyrical_U_log <- 0 #log of adult non-lyrical to unknown
Adult_NonLyrical_T_log <- 0 #log of adult non-lyrical to child
Adult_NoMusic_N_log <- 0 #log of adult no music NOT to child
Adult_NoMusic_U_log <- 0 #log of adult no music to unknown
Adult_NoMusic_T_log <- 0 #log of adult no music to child
Child_C_count <- 0 #count of child canonical vocalization
Child_X_count <- 0 #count of child non-canonical vocalization
Child_R_count <- 0 #count of child cry reflexive utterance
Child_L_count <- 0 #count of child laugh reflexive utterance
Child_C_X_count <- 0 #count of child total vocalization
Adult_Lyrical_N_count <- 0 #count of adult lyrical NOT to child
Adult_Lyrical_U_count <- 0 #count of adult lyrical to unknown
Adult_Lyrical_T_count <- 0 #count of adult lyrical to child
Adult_NonLyrical_N_count <- 0 #count of adult non-lyrical NOT to child
Adult_NonLyrical_U_count <- 0 #count of adult non-lyrical to unknown
Adult_NonLyrical_T_count <- 0 #count of adult non-lyrical to child
Adult_NoMusic_N_count <- 0 #count of adult no music NOT to child
Adult_NoMusic_U_count <- 0 #count of adult no music to unknown
Adult_NoMusic_T_count <- 0 #count of adult no music to child


#x tracking for StartTimeEndTime sheet
#j tracking for Annotation sheet
for(x in 1:nrow(FNSTETSimplified)){ 
  
  if(File_Name != FNSTETSimplified$FileName[x]){ #reads simplified sheet for dataset to be in order
    File_Name <- FNSTETSimplified$FileName[x] #renames File_Name to current file
    Child_ID <- substr(File_Name, start = 1, stop = 4) #gets child ID 
    Child_Age <- substr(File_Name, start = 6, stop = 9) #gets child age
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
  }
  
  Start_Time <- FNSTETSimplified$StartTimeSS[x]
  End_Time <- FNSTETSimplified$EndTimeSS[x]
  
  for(j in 1:nrow(Double_Cleaned_Export)){ #goes through entire dataset export
    
    temp_Name <- Double_Cleaned_Export$File_Name[j] #get file name from dataset export
    
    if(nchar(temp_Name) == 22){
      temp_Name <- substr(temp_Name, start = 1, stop = 11) #removes _Edited.eaf
    } else if(nchar(temp_Name) == 23){
      temp_Name <- substr(temp_Name, start = 1, stop = 12) #removes _Edited.eaf
    }
    
    
    if(temp_Name == File_Name){
      if(Double_Cleaned_Export$Start_Time[j] >= Start_Time && Double_Cleaned_Export$End_Time[j] <= End_Time){ #scans for matching file name and start end times
        if(!is.na(Double_Cleaned_Export$Adult_Utterance_Direction[j])){
         if(Double_Cleaned_Export$Adult_Utterance_Direction[j] == "N"){ #covers all AU that is NOT directed to infant
           if(Double_Cleaned_Export$Music[j] == "LL"){
              Adult_Lyrical_N <- Adult_Lyrical_N + Double_Cleaned_Export$Duration[j]
              Adult_Lyrical_N_count <- Adult_Lyrical_N_count + 1
            } else if(Double_Cleaned_Export$Music[j] == "NL"){
              Adult_NonLyrical_N <- Adult_NonLyrical_N + Double_Cleaned_Export$Duration[j]
              Adult_NonLyrical_N_count <- Adult_NonLyrical_N_count + 1
            } else if(Double_Cleaned_Export$Music[j] == "NM"){
              Adult_NoMusic_N <- Adult_NoMusic_N + Double_Cleaned_Export$Duration[j]
              Adult_NoMusic_N_count <- Adult_NoMusic_N_count + 1
            }
          }else if(Double_Cleaned_Export$Adult_Utterance_Direction[j] == "U"){ #covers all AU that has no clear direction
            if(Double_Cleaned_Export$Music[j] == "LL"){
              Adult_Lyrical_U <- Adult_Lyrical_U + Double_Cleaned_Export$Duration[j]
              Adult_Lyrical_U_count <- Adult_Lyrical_U_count + 1
            } else if(Double_Cleaned_Export$Music[j] == "NL"){
              Adult_NonLyrical_U <- Adult_NonLyrical_U + Double_Cleaned_Export$Duration[j]
              Adult_NonLyrical_U_count <- Adult_NonLyrical_U_count + 1
            } else if(Double_Cleaned_Export$Music[j] == "NM"){
              Adult_NoMusic_U <- Adult_NoMusic_U + Double_Cleaned_Export$Duration[j]
              Adult_NoMusic_U_count <- Adult_NoMusic_U_count + 1
            }
          }else if(Double_Cleaned_Export$Adult_Utterance_Direction[j] == "T"){ #covers all AU that is directed to infant
            if(Double_Cleaned_Export$Music[j] == "LL"){
              Adult_Lyrical_T <- Adult_Lyrical_T + Double_Cleaned_Export$Duration[j]
              Adult_Lyrical_T_count <- Adult_Lyrical_T_count + 1
            } else if(Double_Cleaned_Export$Music[j] == "NL"){
              Adult_NonLyrical_T <- Adult_NonLyrical_T + Double_Cleaned_Export$Duration[j]
              Adult_NonLyrical_T_count <- Adult_NonLyrical_T_count + 1
            } else if(Double_Cleaned_Export$Music[j] == "NM"){
              Adult_NoMusic_T <- Adult_NoMusic_T + Double_Cleaned_Export$Duration[j]
              Adult_NoMusic_T_count <- Adult_NoMusic_T_count + 1
            }
          }
        }
        if(!is.na(Double_Cleaned_Export$Infant_Voc[j])){
          if(Double_Cleaned_Export$Infant_Voc[j] == "C"){
            Child_C <- Child_C + Double_Cleaned_Export$Duration[j]
            Child_C_X <- Child_C_X + Double_Cleaned_Export$Duration[j]
            Child_C_count <- Child_C_count + 1
            Child_C_X_count <- Child_C_X_count + 1
          } else if(Double_Cleaned_Export$Infant_Voc[j] == "X"){
            Child_X <- Child_X + Double_Cleaned_Export$Duration[j]
            Child_C_X <- Child_C_X + Double_Cleaned_Export$Duration[j]
            Child_X_count <- Child_X_count + 1
            Child_C_X_count <- Child_C_X_count + 1
          } else if(Double_Cleaned_Export$Infant_Voc[j] == "R"){
            Child_R <- Child_R + Double_Cleaned_Export$Duration[j]
            Child_R_count <- Child_R_count + 1
          } else if(Double_Cleaned_Export$Infant_Voc[j] == "L"){
            Child_L <- Child_L + Double_Cleaned_Export$Duration[j]
            Child_L_count <- Child_L_count + 1
        }
      }
        } 
      }else if(j == nrow(Double_Cleaned_Export)){ #prints the info for the 5 minute segment
        Child_C_log <- log(Child_C + 1)
        Child_X_log <- log(Child_X + 1)
        Child_R_log <- log(Child_R + 1)
        Child_L_log <- log(Child_L + 1)
        Child_C_X_log <- log(Child_C_X + 1)
        Adult_Lyrical_N_log <- log(Adult_Lyrical_N + 1)
        Adult_Lyrical_U_log <- log(Adult_Lyrical_U + 1)
        Adult_Lyrical_T_log <- log(Adult_Lyrical_T + 1)
        Adult_NonLyrical_N_log <- log(Adult_NonLyrical_N + 1)
        Adult_NonLyrical_U_log <- log(Adult_NonLyrical_U + 1)
        Adult_NonLyrical_T_log <- log(Adult_NonLyrical_T + 1)
        Adult_NoMusic_N_log <- log(Adult_NoMusic_N + 1)
        Adult_NoMusic_U_log <- log(Adult_NoMusic_U + 1)
        Adult_NoMusic_T_log <- log(Adult_NoMusic_T + 1)
        if(nchar(File_Name) == 22){
          File_Name <- substr(temp_Name, start = 1, stop = 11) #removes _Edited.eaf
        }else if(nchar(File_Name) == 23){
          File_Name <- substr(temp_Name, start = 1, stop = 12) #removes _Edited.eaf
        }
        Descriptives_Table[nrow(Descriptives_Table) +1,] <- c(File_Name,
                                                              Child_ID, 
                                                              Child_Age,
                                                              Start_Time,
                                                              End_Time,
                                                              Child_C,
                                                              Child_X,
                                                              Child_R, 
                                                              Child_L,
                                                              Child_C_X,
                                                              Adult_Lyrical_N, 
                                                              Adult_Lyrical_U, 
                                                              Adult_Lyrical_T, 
                                                              Adult_NonLyrical_N,
                                                              Adult_NonLyrical_U,
                                                              Adult_NonLyrical_T, 
                                                              Adult_NoMusic_N,
                                                              Adult_NoMusic_U,
                                                              Adult_NoMusic_T,
                                                              Child_C_log,
                                                              Child_X_log,
                                                              Child_R_log,
                                                              Child_L_log,
                                                              Child_C_X_log,
                                                              Adult_Lyrical_N_log,
                                                              Adult_Lyrical_U_log,
                                                              Adult_Lyrical_T_log,
                                                              Adult_NonLyrical_N_log,
                                                              Adult_NonLyrical_U_log,
                                                              Adult_NonLyrical_T_log,
                                                              Adult_NoMusic_N_log,
                                                              Adult_NoMusic_U_log,
                                                              Adult_NoMusic_T_log,
                                                              Child_C_count,
                                                              Child_X_count,
                                                              Child_R_count,
                                                              Child_L_count,
                                                              Child_C_X_count,
                                                              Adult_Lyrical_N_count,
                                                              Adult_Lyrical_U_count,
                                                              Adult_Lyrical_T_count,
                                                              Adult_NonLyrical_N_count,
                                                              Adult_NonLyrical_U_count,
                                                              Adult_NonLyrical_T_count,
                                                              Adult_NoMusic_N_count,
                                                              Adult_NoMusic_U_count,
                                                              Adult_NoMusic_T_count) #creates new row and inserts everything into Descriptives_Table
      #resets all variables for new subset
      File_Name <- 0
      Child_ID <- 0
      Child_Age <- 0
      Start_Time <- 0
      End_Time <- 0
      Child_C <- 0
      Child_X <- 0
      Child_R <- 0
      Child_L <- 0
      Child_C_X <- 0
      Adult_Lyrical_N <- 0
      Adult_Lyrical_U <- 0
      Adult_Lyrical_T <- 0
      Adult_NonLyrical_N <- 0
      Adult_NonLyrical_U <- 0
      Adult_NonLyrical_T <- 0
      Adult_NoMusic_N <- 0
      Adult_NoMusic_U <- 0
      Adult_NoMusic_T <- 0
      Child_C_log <- 0
      Child_X_log <- 0
      Child_R_log <- 0
      Child_L_log <- 0
      Child_C_X_log <- 0
      Adult_Lyrical_N_log <- 0
      Adult_Lyrical_U_log <- 0
      Adult_Lyrical_T_log <- 0
      Adult_NonLyrical_N_log <- 0
      Adult_NonLyrical_U_log <- 0
      Adult_NonLyrical_T_log <- 0
      Adult_NoMusic_N_log <- 0
      Adult_NoMusic_U_log <- 0
      Adult_NoMusic_T_log <- 0
      Child_C_count <- 0
      Child_X_count <- 0
      Child_R_count <- 0
      Child_L_count <- 0
      Child_C_X_count <- 0
      Adult_Lyrical_N_count <- 0
      Adult_Lyrical_U_count <- 0
      Adult_Lyrical_T_count <- 0
      Adult_NonLyrical_N_count <- 0
      Adult_NonLyrical_U_count <- 0
      Adult_NonLyrical_T_count <- 0
      Adult_NoMusic_N_count <- 0
      Adult_NoMusic_U_count <- 0
      Adult_NoMusic_T_count <- 0
      break
    }
  }
}

#Changes values from char to numeric format
for (i in 2:ncol(Descriptives_Table)){
  Descriptives_Table[,i] <- as.numeric(Descriptives_Table[,i])
}

write.csv(Descriptives_Table, "C:/Users/Jeffrey/Box/Jeffrey Mai 1st Year Project (IT API admin)/Stats/Final Analysis Sheets/Descriptives_Table.csv")
write.csv(Descriptives_Table, "~/Library/CloudStorage/Box-Box/Jeffrey Mai 1st Year Project/Stats/Final Analysis Sheets/Descriptives_Table.csv")

