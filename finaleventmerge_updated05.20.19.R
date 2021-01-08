rm(list=ls())
library(tidyverse)
library(stringr)
setwd("/Volumes/NortonLab/SocialEEG/event_merge")

int_dir <- "/Volumes/NortonLab/SocialEEG/InteractFiles/"
mat_dir <- "./MATLAB Files/"


elists <- list.files(mat_dir)
elists <- elists[str_detect(elists, "elist.txt")]
core <- str_replace(elists, "_elist.txt", "")
subids <- unique(str_extract(core, "[SW][O2][CW][3]?_[0-9]{3,4}"))
contexts <- unique(str_extract(core, pattern = "[bfmpn].*"))
int_all <- list.files(int_dir) 
from_int <- int_all[str_detect(int_all, paste0(subids, collapse = "|"))]
from_int <- from_int[str_detect(from_int, ".csv")]


subid <- "SOC_800"
context <- "book"
## Change this next line only if you will be timelocking to something other than the '0' time marker
timelock <- 4


csv <- from_int[str_detect(from_int, paste0(subid, "_", context))]
if(length(csv) >= 2){
  stop("There are 2 files that are not named sufficiently differently. Is your context movie1 or movie? Did multiple people export this file from interact? Are the files from Matlab and Interact named correctly?")
}
x <- read.csv(paste0(int_dir , csv)) 

#Interact should not have identical start times for events. Occassionally, the first row will have an exact match
# with the 2nd row's Onset Time. In this case, we want to delete the first row
if(x$Onset_Time[1] == x$Onset_Time[2]){
  x <- x[-1,]
}

#create a short final event so we don't lose data at the end
x[(nrow(x) + 1),] <- x[nrow(x),]
x$Number[nrow(x)] <- nrow(x)
x$Onset_Time[nrow(x)] <- x$Offset_Time[nrow(x)]
x$Offset_Time[nrow(x)] <- x$Offset_Time[nrow(x)] + 1
x$Duration_Time[nrow(x)] <- x$Offset_Time[nrow(x)] - x$Onset_Time[nrow(x)]

#list of all possible output variables from Interact
varNames <- c("Onset_Time", "Offset_Time", "object_engagement", 
              "dyadic_engagement", "joint_engagement", "engagement", "other", 
              "experiment_interrupted", "parent_state", "child_state", 
              "frustration.child.", "object.p.", "object.c.", "object.parent.", "object.child.", 
              "supporting_person", "qualifying", "object", 
              "symbols.child.", "Transcript", "X")

#adds in variables that were missing/not used in interact
for(name in varNames){
  if(any(names(x) == name) == FALSE){
    v <- as.list(x)
    v[[name]] <- rep("", nrow(x))
    x <- as.data.frame(v)
  }
}

# #pre for-loop example
# if(any(names(x) == "engagement") == FALSE){
#   x$engagement <- ""
# }

################################
#### Create event codes ########
#### case_when is fancy ifelse##
################################

x$engagement <- case_when(x$joint_engagement == "coordinated" ~ 2, 
                          x$joint_engagement == "supported" ~ 3,
                          x$dyadic_engagement == "person engagement" ~ 1,
                          x$experiment_interrupted == "experiment interrupted" ~ 7,
                          x$object_engagement == "separate object engagement" ~ 4,
                          x$object_engagement == "parallel object engagement" ~ 5,
                          x$other == "other" ~ 6)

x$supporter <- case_when(x$supporting_person == "child" ~ 1,
                         x$supporting_person == "parent" ~ 2)

x$object <- case_when(x$object == "movie" ~	1,
                      x$object == "toy/object" ~ 2)

x$symbols  <- case_when(x$symbols.child. == "symbols" ~ 1,
                        x$symbols.child. == "without symbols" ~ 2)

x$frustration <-  case_when(x$frustration.child. == "positive/neutral(c)" ~ 2,
                            x$frustration.child. == "frustrated(c)" ~ 1,
                            x$frustration.child. == "positive/neutral" ~ 2,
                            x$frustration.child. == "frustrated" ~ 1)

x$qual_beh <- case_when(x$qualifying == "soothing" ~ 3, 
                        x$qualifying == "none" ~ 4,
                        x$qualifying == "child narration" ~ 2,
                        x$qualifying == "parent narration" ~ 1)

x$parent_st <- case_when(x$parent_state == "object engagement movie(p)" ~ 1,
                         x$parent_state == "object engagement toy/object(p)" ~ 2,
                         x$parent_state == "onlooking(p)" ~ 3, 
                         x$parent_state == "unengaged(p)" ~ 4, 
                         x$parent_state == "attempted joint engagement(p)" ~ 5)

x$child_st <- case_when(x$child_state == "object engagement movie(c)" ~ 1,
                        x$child_state == "object engagement toy/object(c)" ~ 2,
                        x$child_state == "onlooking(c)" ~ 3, 
                        x$child_state == "unengaged(c)" ~ 4, 
                        x$child_state == "attempted joint engagement(c)" ~ 5)

x$p_object <- case_when(x$object.p. == "movie(p)" ~	1,
                        x$object.p. == "toy/object(p)" ~ 2,
                        x$object.parent. == "movie(p)" ~	1,
                        x$object.parent. == "toy/object(p)" ~ 2)

x$c_object <- case_when(x$object.c. == "movie(c)" ~	1,
                        x$object.c. == "toy/object(c)" ~ 2,
                        x$object.child. == "movie(c)" ~	1,
                        x$object.child. == "toy/object(c)" ~ 2)

#actually paste each number of the event code togeter
x$ecode <- paste0(x$engagement, x$supporter, x$object, x$symbols, x$frustration, x$qual_beh, x$parent_st, x$child_st, x$p_object, x$c_object)
x$ecode <- gsub("NA", "0", x$ecode)
x$ecode <- as.numeric(x$ecode)

#select only the essential variables
x <- x %>%
  select(Onset_Time, Offset_Time, ecode)

########################
####epoching############ 
########################

z <- data.frame()
for(i in 1:nrow(x)){
  for(j in 1:floor(x$Offset_Time[i] - x$Onset_Time[i])){
    z[i,j] <- x$Onset_Time[i+1] - j
  }
}
z <- t(z)

xx <- vector("numeric")
for(i in 1:ncol(z)){
  x1 <- z[,i]
  xx <- c(x1, xx)
}
xxx <- xx[!is.na(xx)]
xxx <- data.frame(unname(sort(xxx, decreasing = F)))
names(xxx) <- c("time")

#"Cross" original dataframe with newly created epoch times, and keep only the ones that make sense
df <- x %>%
  crossing(xxx) %>%
  filter(time >= Onset_Time & time <= Offset_Time)

#clean up environment
rm(xx, xxx, x1, i, j, z, x, v)

###############################
#### Read in matlab elist #####
###############################
y <- read.table(paste0(mat_dir, paste0(subid, "_", context, "_elist.txt")))
#one column "b_flags" read in funky - added extra var "b_flag"
names(y) <- c("item",	 "bepoch", "ecode", "label", "onset", "diff", "dura",	"b_flags", "b_flag",  "a_flags", "enable", "bin")

#find 0 point from matlab
zero_point_mat <- y %>%
  filter(ecode == timelock)
zero_point_mat <- zero_point_mat$onset
zero_point_int <- min(df$time)

#write over the time variable by adding the zero point from matlab and subtracting the earliest time from interact
df$time <- df$time - zero_point_int + zero_point_mat

#merge to put into matlab format
z <- full_join(df, y, by = c("ecode", "time" = "onset")) %>%
  select(-Onset_Time, -Offset_Time, -b_flag)

#make it sort by time
z <- z[order(z$time),]

#recreate difference variable
z$diff <- round((z$time - lag(z$time)) *1000, 2) 

#mark as NA if its a transition epoch 
z$ecode <- ifelse(z$diff <= 1500 & z$diff > 1000 & z$item >= 6, NA, z$ecode)
#remove said transition epochs
z <- z[!is.na(z$ecode),] 



z$diff[1] <- NA

#add a bunch of variables that matlab outputs
z$item <- rownames(z)
z$bepoch <- 0
z$dura <- 0
z$b_flags <- "00000000"
z$a_flags <- "00000000"
z$enable <- "1    [       ]"
z$bin <- ""
z$onset <- round(z$time, 4)
z$label <- shQuote("", type = "cmd2")
z <- z %>%
  select(item, bepoch, ecode, label, onset, diff, dura,	b_flags, a_flags, enable, bin)

#reorder the events so they are in the correct sequence
z$item <- seq(1, nrow(z), 1)

#write the actual data without the weird header
write.table(z, file = paste0("/Volumes/NortonLab/SocialEEG/event_merge/MergedFiles/", subid, "_", context, "_main.txt"), quote = c(4), row.names = F, col.names = F, sep = "\t")

#substitute the nevents with new nevents number
fileName <- paste0(mat_dir, subid, "_", context, "_elist.txt")
non_edit_header <- readChar(fileName, nchars = 1140)
non_edit_header <- str_replace(non_edit_header, "nevents...................: [0-9].*", paste0("nevents...................: ",nrow(z)))

#write the weird header alone. merge using bash script
fileConn <- file(paste0("/Volumes/NortonLab/SocialEEG/event_merge/MergedFiles/", subid, "_", context, "_header.txt"))
writeLines(non_edit_header, fileConn)
close(fileConn)
