print("Isha Golakiya")
#essential libraries
library(FSA)
library(FSAdata)
library(magrittr)
library(dplyr)
library(tidyr)
library(plyr)
library(tidyverse)
library(plotly)
# Import the dataset
setwd("/Users/HP/Downloads")
bio <- read.csv("inchBio.csv")
bio
names(bio)

# Printing head, tail and structure of data set
headtail(bio)
tail(bio)
str(bio)

# making new object which count and list the species
counts<- aggregate(bio$species, by= list(bio$species), FUN= length)
colnames(counts) <- c("Species", "Count")
counts

# Displaying the 8 levels
levels(as.factor(bio$species))

# Creating tmp
temp <- table(bio$species)
temp

#Subset of species
tmp2 <- bio$species
head(tmp2,5)

# table w for species variable
w <- table(bio$species)
class(w)

t <- as.data.frame(w)
class(t)
t
#extracting and displaying frequency
t$Freq
#or
t[,2]

#table with bio species attribute
cspec <- table(bio$species)
cspec
class(cspec)

#table with percentage of record for each species
cSpecPct <- prop.table(cspec)*100 
cSpecPct
class(cSpecPct)

# converting into dataframe
u <- as.data.frame(cSpecPct)

# Bar plot  
barplot(cspec, main = "Fish Count", xlab = "COUNTS", col = "green", las= 2, cex.axis = 0.8, cex.lab = 0.5, horiz = TRUE, density = 60, cex = 0.6)

#Bar plot of cspecpct
barplot(cSpecPct/100, ylim= c(0,0.4), las=2, col.axis = "LightBlue", main = "Fish Relative Frequency")

#arranging in decending order
d <- arrange(t,desc(t$Freq))
t

colnames(d) <- c("Species", "RealFreq")
d

d <- mutate(d, cumfreq = cumsum(RealFreq), counts= t$Freq, cumcounts = cumsum(t$Freq))
d  
def_par <- as.data.frame(d)
d


#barplpot
pc = barplot(d$counts,
             width = 1,
             space = 0.15,
             border = NA,
             axes = FALSE,
             ylim = c(0, 3.05 * max(d$counts, na.rm = TRUE)),
             ylab = "Cummulative Counts",  cex.axis = 0.7,
             names.arg= d$Species,
             las = 2,
             main = "Species Pareto")

lines(pc, d$cumcounts, type = "b", cex = 0.7, pch = 19, col= "cyan4" )
box(col = "grey62")
axis(side = 2, at = c(0, d$cumcounts), las = 1, col.axis = "grey62", col = "grey62", cex.axis = 0.8)
axis(side = 4, at = c(0, d$cumcounts), labels = paste(c(0, round(d$cumfreq * 100)) ,"%",sep=""), 
     las = 1, col.axis = "cyan4", col = "cyan4", cex.axis = 0.8)

text(x = grconvertX(0.35, from = "npc"), # align to center of plot X axis
     y = grconvertY(0.9, from = "npc"), # align to center of plot Y axis
     labels = "Isha Golakiya", # our watermark
     cex = 2, font = 1, # large, bold font - hard to miss
     col = rgb(0, 0, 0, 0.2), # translucent (0.2 = 20%) red color
     srt = 0) # srt = angle of text: 45 degree angle to X axis

#scatterplot
p2 <- smoothScatter(bio$tl, bio$w,
                    ylab = "Total length",
                    xlab = "Weight",
                    main = "",
                    pch = 16,
                    col = "green")

