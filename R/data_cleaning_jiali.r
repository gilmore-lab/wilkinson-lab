
# Please run the following 3 functions first.



# Function 1: extract condt from media (function)}
extractcondt.fun <- function(data){  

data$condt <- as.factor(substr(data$MediaName, 7,10))
levels(data$condt) <- c(levels(data$condt), "soc")
data$condt[data$condt == "soc." | data$condt == "soc_"] <- "soc"
data$condt[data$condt == "cloc" ] <- "cloc"
data$condt <- factor(data$condt)

return(data)
}



# Function 2: getting the vocabulary
extractvocab.fun <- function(data){
  
data$vocab <- as.factor(substr(data$fixedaoi, 13, (nchar(data$fixedaoi)-4)))


allboy <- grep("BOY",data$fixedaoi, value=TRUE, fixed = TRUE )
allsad <- grep("SAD",data$fixedaoi, value=TRUE, fixed = TRUE )
allhappy <- grep("HAPPY",data$fixedaoi, value=TRUE, fixed = TRUE )
allhot <- grep("HOT",data$fixedaoi, value=TRUE, fixed = TRUE )
allcold <- grep("COLD",data$fixedaoi, value=TRUE, fixed = TRUE )
allme <- grep("ME",data$fixedaoi, value=TRUE, fixed = TRUE )
allbaby <- grep("BABY",data$fixedaoi, value=TRUE, fixed = TRUE )
allgirl <- grep("GIRL",data$fixedaoi, value=TRUE, fixed = TRUE )
allslide <- grep("SLIDE",data$fixedaoi, value=TRUE, fixed = TRUE )
allball <- grep("BALL",data$fixedaoi, value=TRUE, fixed = TRUE )
allswing <- grep("SWING",data$fixedaoi, value=TRUE, fixed = TRUE )

allclimbing <- grep("CLIMBING",data$fixedaoi, value=TRUE, fixed = TRUE )
allclimibing <- grep("CLIMIBING",data$fixedaoi, value=TRUE, fixed = TRUE )

allswinging <- grep("SWINGING",data$fixedaoi, value=TRUE, fixed = TRUE )
allthrowing <- grep("THROWING",data$fixedaoi, value=TRUE, fixed = TRUE )
allsliding <- grep("SLIDING",data$fixedaoi, value=TRUE, fixed = TRUE )
allladder <- grep("LADDER",data$fixedaoi, value=TRUE, fixed = TRUE )

allmother <- grep("MOTHER",data$fixedaoi, value=TRUE, fixed = TRUE )

allall <- grep("ALL",data$fixedaoi, value=TRUE, fixed = TRUE )


levels(data$vocab) <- c(levels(data$vocab),
                        "ALL","BABY","BALL","BOY","CLIMBING","COLD","GIRL","HAPPY","HOT",
                        "LADDER","ME","SAD","SLIDE","SLIDING",
                        "SWING","SWINGING","THROWING")

data$vocab[is.na(data$fixedaoi) && complete.cases(data$fixedwhole)] <- "ALL"

data$vocab[data$fixedaoi %in% allall == TRUE] <- "ALL"
data$vocab[data$fixedaoi %in% allbaby == TRUE] <- "BABY"
data$vocab[data$fixedaoi %in% allmother == TRUE] <- "BABY"
data$vocab[data$fixedaoi %in% allball == TRUE] <- "BALL"
data$vocab[data$fixedaoi %in% allboy == TRUE] <- "BOY"
data$vocab[data$fixedaoi %in% allclimbing == TRUE] <- "CLIMBING"
data$vocab[data$fixedaoi %in% allclimibing == TRUE] <- "CLIMBING"
data$vocab[data$fixedaoi %in% allcold == TRUE] <- "COLD"
data$vocab[data$fixedaoi %in% allgirl == TRUE] <- "GIRL"
data$vocab[data$fixedaoi %in% allhappy == TRUE] <- "HAPPY"
data$vocab[data$fixedaoi %in% allhot == TRUE] <- "HOT"
data$vocab[data$fixedaoi %in% allladder == TRUE] <- "LADDER"
data$vocab[data$fixedaoi %in% allme == TRUE] <- "ME"
data$vocab[data$fixedaoi %in% allsad == TRUE] <- "SAD"
data$vocab[data$fixedaoi %in% allslide == TRUE] <- "SLIDE"
data$vocab[data$fixedaoi %in% allsliding == TRUE] <- "SLIDING"
data$vocab[data$fixedaoi %in% allswing == TRUE] <- "SWING"
data$vocab[data$fixedaoi %in% allswinging == TRUE] <- "SWINGING"
data$vocab[data$fixedaoi %in% allthrowing == TRUE] <- "THROWING"

data$vocab <- factor(data$vocab)

return(data)
}



# Function 3: get the fixed aoi, pictures being fixed, and their fixation counts:
fixduraoi.fun <- function(input_table) {

allAOI <- grep("AOI", colnames(input_table), value = TRUE, fixed = TRUE) 
allpic <- grep(".ALL", colnames(input_table),  value = TRUE,fixed = TRUE) 
partaoi <- allAOI[allAOI %in% allpic == FALSE]

# if an AOI is being fixated, put that AOI name (column) into a row called fixedaoi
input_table$fixedaoi = rep(NA, nrow(input_table))
for (xxxx in partaoi){
  input_table$fixedaoi[input_table[,xxxx]==1]<- xxxx
}
# if an ALL (whole picture) is being fixated, put that AOI name (column) into a row called fixedwhole
input_table$fixedwhole = rep(NA, nrow(input_table))
for (yyyy in allpic){
  input_table$fixedwhole[input_table[,yyyy] == 1] <- yyyy
}

# get the target from column fixedaoi.
input_table$target <- as.factor(substr(input_table$fixedaoi, 6, 7))
# create levels for the target: A (target), B(target_pair), C(distractor), D(white space) 
levels(input_table$target) <- c(levels(input_table$target), "A", "B","C","D")

# if there is an A in the target name, label this fixation with an "A".
levelA <- grep("A", input_table$target, value = TRUE, fixed = TRUE)
input_table$target[input_table$target %in% levelA == TRUE] <- "A"
levelB <- grep("B", input_table$target, value = TRUE, fixed = TRUE)
input_table$target[input_table$target %in% levelB == TRUE] <- "B"
levelC <- grep("C", input_table$target, value = TRUE, fixed = TRUE)
input_table$target[input_table$target %in% levelC == TRUE] <- "C"

# calculate the number of fixations on the whole picture.
# if there is a fixation on the whole picture, label it as 1 in the column countfixwhole
input_table$countfixwhole <- ifelse(complete.cases(input_table$fixedwhole), 1, NA)

input_table$target[is.na(input_table$target) & input_table$countfixwhole == 1] <- "D"
input_table$target <- factor(input_table$target)

# extract conditions from media name
input_table <- extractcondt.fun(input_table)
# calcualte number of fixations on the whole picture
input_table$countfix <- ifelse(complete.cases(input_table$fixedaoi), 1, NA)

# extract conditions from  fixedaoi
input_table <- extractvocab.fun(data = input_table)
# calcuate number of fixations on the aoi
input_table <- input_table[complete.cases(input_table$fixedwhole),]


return(input_table)
}


# Now begins the data cleaning:

# First, read in the data file. I worked on csv.
# The default output file is actually xlxs; pleae make sure the data file type is correct.

test <- read.csv(file = "WHICHEVER FILE YOU WORK ON", header = TRUE)


# Then, remove rows unrelated to the current project
# Keep rows with medianames of a ".png"
valid_rows <- grep(".png",test$MediaName,  fixed = TRUE )
test <- test[valid_rows,]

# Finally, run the function to get the fixed aoi:
test_result <- fixduraoi.fun(input_table = test)



