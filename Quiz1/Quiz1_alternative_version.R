#' ---
#' title : "DS Capstone Quiz 1"
#' author : B.F.C
#' date created : "18/2/2013"
#' date modified : "23/2/2013" 
#' History : 
#'  "23/2/2013" : try to read one line at a time
#' ---


# get the data :  data dir
datadir <- "projdata/final/en_US"

# open en_US.twitter.txt
# fname <- "en_US.twitter.txt"



# code for reading consecutive blocks of lines
# (based on https://stackoverflow.com/questions/20410979/can-i-read-a-file-consecutively-without-rewinding-to-the-beginning-in-r/20411541#20411541)

read_by_lines <- function(filename, # the file path (string)
                          block_process = function(block_text, # the function to call on each  block of lines read: 
                                                   block_fields = c("Value"), 
                                                   block_size = block_size, 
                                                   block_length = block_size, 
                                                   block_index = 1) { # default behavior
                                  print(block_index)} , 
                          # Args : block_text = the block = a vector of lines,
                          # block_fields = the dataframe header if necessary
                          # block_size = the intended length of the block_text
                          # block_length = the real length of the block_text (lines may be missing), 
                          # block_index  = the block number (i = 3 == the 3rd block to be read )
                          combine_results = function(previous_results,new_results) { # function for combining block results
                                  previous_results + new_results},
                          initial_results = NULL,
                          block_size = 5, # read block_size lines at a time
                          read_header = FALSE # get header first ? (from line 1)
                          ) {
        f_cnxn <- file(filename, 'r') # open a new connection
        on.exit(close(f_cnxn))
        
        # initialization
        if (read_header) fields <- readLines(f_cnxn, n = 1)      # if necessary, read the header, which we might reuse for each block
        # (only necessary if the lines are part of a dataframe)
        block_index <- 0 # set the counter (maybe useful ?)
        previous_results <- initial_results
        
        # main reading loop
        repeat { # keep reading a block of lines until you reach the end of the cnxn.
                block_text <- readLines(f_cnxn, n = block_size) # read block
                block_length <- length(block_text)
                block_index <- block_index + 1
                if (block_length == 0) # if there's nothing left, leave the loop
                        break
                # process a block of text lines :  call the function 
                new_results <- block_process(block_text, 
                                             block_fields = fields,
                                             block_size = block_size,
                                             block_length = block_length,
                                             block_index = block_index )
                previous_results <- combine_results(previous_results, new_results) # update results
                
        }
        previous_results # return value
}
        

# example and test

# creating data, and write it in a temporary file

read_by_lines_test <- function(block_size) {
        
        temp.fpath <- tempfile() # create a temp file name for this demo
        d <- data.frame(one = letters[1:10], two = 1:10) # sample data, 10 rows. we'll read 'block_size' at a time
        write.csv(d, temp.fpath, row.names=FALSE) # write the sample data to file
        # force remove thrash when finished
        on.exit(expr = file.remove(temp.fpath))
        
        # call read_by_lines
        read_by_lines(temp.fpath, 
                      block_size, 
                      block_process = function(block_text, 
                                               block_fields = c("Value"), 
                                               block_size, 
                                               block_length, 
                                               block_index ) {
                              block <- read.csv(text=c(block_fields, block_text)) # process chunk with
                              cat(paste0("\nBlock number: ", block_index, "\n"))
                              cat(paste0("Lines in block: ", block_length, "\n"))
                              print(block)
                              # return
                              block_length
                      },
                      combine_results = function(previous_results,new_results){previous_results + new_results},
                      initial_results = 0,
                      read_header = TRUE
        )
}

# make a test
read_by_lines_test(block_size = 9)
# remove the testing function
rm(read_by_lines_test)






# Question 2 -  redone with incremential reading 
# --------------

#  summarising function
# sumfile <- function(fname) {
#         # read lines
#         lines <- readLines(file.path(datadir, fname))
#         # Counting lines
#         nlines <- length(lines)
#         
#         maxwidthline <- max(sapply(lines,
#                    function(x) nchar(x)
#         ))
#         list(Filename = fname, Lines = nlines, Linewidth = maxwidthline)
# }



sumfile <- function(fname) {
        # read lines
        lines_sum <- read_by_lines(filename = fname, 
                                   block_process = local( {
                                           nlines <- 0
                                           linewidth <- 0 #dummy values for initialization
                                           function(block_text, 
                                                    block_fields = c("Value"), 
                                                    block_size, 
                                                    block_length, 
                                                    block_index ) {
                                                   nlines <- block_length + nlines
                                                   linewidth <- max(linewidth, 
                                                                    max(sapply(block_text,
                                                                               function(x) nchar(x)) ) )
                                                   # return
                                                   c(nlines, linewidth)
                                           }
                                   }),
                                   combine_results = function(previous_results, new_results){
                                           c(previous_results[1] + new_results[1],
                                             max(previous_results[2], new_results[2]))
                                   },
                                   initial_results = c(0,0),
                                   block_size = 1000, # read block_size lines at a time
                                   read_header = FALSE # get header first ? (from line 1)
        )
        list(Filename = fname, Lines = lines_sum[1], Linewidth = lines_sum[2])
}

sumfile(file.path(datadir, "en_US.news.txt"))



# list of files
flist <- dir(datadir)

# Get results
sapply(sapply(flist, 
              function(fname) file.path(datadir, fname)),
       sumfile)

#               en_US.blogs.txt   en_US.news.txt   en_US.twitter.txt
# Filename  "en_US.blogs.txt" "en_US.news.txt" "en_US.twitter.txt"
# Lines     899288            77259            2360148
# Linewidth 40835             5760             213


# Question 4
# ----------
# lovehateratio <-  function(fname) {
#         lines <- readLines(file.path(datadir, fname))
#         ln_love <- sum(grepl(pattern = "love",lines ))
#         ln_hate <- sum(grepl(pattern = "hate",lines ))
#         ratio = ln_love / ln_hate
#         c(love = ln_love, hate = ln_hate, ratio = ratio)
# }


lovehateratio <-  function(fname) {
        lovehate <- read_by_lines(filename = fname, 
                                  block_process = local( {
                                          ln_love <- 0
                                          ln_hate <- 0 #dummy values for initialization
                                          function(block_text, 
                                                   block_fields = c("Value"), 
                                                   block_size, 
                                                   block_length, 
                                                   block_index ) {
                                                  ln_love <- sum(grepl(pattern = "love", block_text ))
                                                  ln_hate <- sum(grepl(pattern = "hate", block_text ))
                                                  # return
                                                  c(ln_love, ln_hate)
                                          }
                                  }),
                                  combine_results = function(previous_results, new_results){
                                          c(previous_results[1] + new_results[1],
                                            previous_results[2] + new_results[2])
                                  },
                                  initial_results = c(0,0),
                                  block_size = 1000, # read block_size lines at a time
                                  read_header = FALSE # get header first ? (from line 1)
        )
        c(love = lovehate[1], hate = lovehate[2],  ratio = lovehate[1] / lovehate[2])
}

lovehateratio(file.path(datadir,"en_US.twitter.txt"))

# love         hate        ratio 
# 90956.000000 22138.000000     4.108592 


# Question 5
# ----------

local({
        fname <- "en_US.twitter.txt"
        lines <- readLines(file.path(datadir, fname))
        anything <-  ""
        anything  <-  grep(pattern = "NOMATCH biostat", x = lines, value = TRUE)
        # str(anything),
        # is.null(anything),
        # anythingelse = sum(grepl(pattern = "NOMATCH biostat", lines )),
        Tweetbio <-  grep(pattern = "biostat", x = lines, value = TRUE)
        Howmanychess <-  sum(grepl(pattern = "A computer once beat me at chess, but it was no match for me at kickboxing",
                                   lines ))
        
        list( anything = anything,
              l_anything = length(anything),
              Tweetbio = Tweetbio,
              Howmanychess = Howmanychess
              # anythingpos = grep(pattern = "NOMATCH biostat", x = lines, value = FALSE)
        )
})



# new
local({
        fname <- "en_US.twitter.txt"
        chesspattern <- "A computer once beat me at chess, but it was no match for me at kickboxing"
        
        listres <- read_by_lines(filename = file.path(datadir, fname), 
                                 block_process = function(block_text, 
                                                          block_fields = c("Value"), 
                                                          block_size, 
                                                          block_length, 
                                                          block_index ) {
                                         biostat_match <- grep(pattern = "biostat", x =  block_text, value = TRUE)
                                         Howmanychess <- sum(grepl(pattern = chesspattern,
                                                                   block_text ))
                                         # return
                                         list(ifelse(length(biostat_match) != 0, biostat_match, "NO_MATCH"), # test necessary
                                              Howmanychess)
                                 },
                                 combine_results = function(previous_results, new_results){
                                         # if(length(new_results[[1]])  != 0) {message("MATCH FOUND")}
                                         list(ifelse(previous_results[[1]] == "NO_MATCH", 
                                                      new_results[[1]], previous_results[[1]]), 
                                               previous_results[[2]] + new_results[[2]])
                                 },
                                 initial_results = list("NO_MATCH", 0),
                                 block_size = 10000, # read block_size lines at a time
                                 read_header = FALSE # get header first ? (from line 1)
        )
        names(listres) <- c("Tweetbio", "Howmanychess")
        listres
})

# correct answer
# $Tweetbio
# [1] "i know how you feel.. i have biostats on tuesday and i have yet to study =/"
# 
# $Howmanychess
# [1] 3



