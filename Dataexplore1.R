#' ---
#' title : "DS Capstone Quiz 1"
#' author : B.F.C
#' date : "18/2/2013"
#' ---


# get the data :  data dir
datadir <- "projdata/final/en_US"

# open en_US.twitter.txt
# fname <- "en_US.twitter.txt"




# Question 2 - 3
# --------------

#  summarising function
sumfile <- function(fname) {
        # read lines
        lines <- readLines(file.path(datadir, fname))
        # Counting lines
        nlines <- length(lines)
        
        maxwidthline <- max(sapply(lines,
                   function(x) nchar(x)
        ))
        list(Filename = fname, Lines = nlines, Linewidth = maxwidthline)
}

# list of files
flist <- dir(datadir)

# Get results
sapply(flist, sumfile)

#               en_US.blogs.txt   en_US.news.txt   en_US.twitter.txt
# Filename  "en_US.blogs.txt" "en_US.news.txt" "en_US.twitter.txt"
# Lines     899288            77259            2360148
# Linewidth 40835             5760             213


# Question 4
# ----------
lovehateratio <-  function(fname) {
        lines <- readLines(file.path(datadir, fname))
        ln_love <- sum(grepl(pattern = "love",lines ))
        ln_hate <- sum(grepl(pattern = "hate",lines ))
        ratio = ln_love / ln_hate
        c(love = ln_love, hate = ln_hate, ratio = ratio)
}

lovehateratio("en_US.twitter.txt")

# love         hate        ratio 
# 90956.000000 22138.000000     4.108592 


# Question 5
# ----------

local({
        fname <- "en_US.twitter.txt"
        lines <- readLines(file.path(datadir, fname))
        list(
        Tweetbio = grep(pattern = "biostat",x = lines, value = TRUE),
        Howmanychess = sum(grepl(pattern = "A computer once beat me at chess, but it was no match for me at kickboxing",
                                 lines ))
        )
})


# $Tweetbio
# [1] "i know how you feel.. i have biostats on tuesday and i have yet to study =/"
# 
# $Howmanychess
# [1] 3



