library(tidyverse)
library(dplyr)
library(openxlsx)
df<-read.xlsx("R01_B_Data_Export_TD_newAOInames.xlsx")

newdata<-df[which(df$GazeEventType =='Fixation'),]
rm(df)
df<-newdata[grep(".png", newdata$MediaName), ]
rm(newdata)
newdata <- df[c(-1, -6:-12,-14:-17)]
rm(df)
df<-newdata
rm(newdata)

df2<-df[,1:2]
df2$aoi<-""
for (i in 1:nrow(df)) {
  #for (j in 7:(ncol(newdata2)-1)) {
  # if (newdata[i,j]==1 && !is.na(newdata[i,j])) {df2$aoi[i]<-colnames(newdata)[j]}
  if (identical(which(df[i,]==1),integer(0))){
    df2$aoi[i]<-NA}
  else {
    df2$aoi[i]<-colnames(df[min(which(df[i,]==1))])}
  #}
}
df2<-df[,1:5]

rm(df)
df2 %>% 
  filter(!is.na(aoi))->df3
df3$category<- gsub("[^a-zA-Z]", "", substr(df3 $aoi,5,7))
table(df3$category) # check whether it is A/B/C/X
df3$category[which(df3$category=='A')] <-'A'
df3$category[which(df3$category=='B')] <-'B'
df3$category[which(df3$category=='C')] <-'C'
df3$category[which(df3$category=='x')] <-'X'

rm(df2)
names(df3)[3] <- "Group"
df4<- df3 %>%
  group_by(ParticipantName,StudioTestName, Group,MediaName,FixationIndex,category) %>%
  summarize(n=n())%>%
  mutate(condt=sub(".png", "", MediaName))

df4 <- df4[c(-4)]

write.table(df4, "R01_B_DS.txt", sep="\t", row.names = FALSE)