#' ---
#' title : "DS Capstone : Sampling the data
#' subtitle: "Reducing the size of the corpora"
#' author : B.F.C
#' date created : "18/2/2013"
#' date modified : "23/2/2013" 
#' History : 
#'  "23/2/2013" : try to read one line at a time
#' ---

library(pander)


#' Data folders and files
#' ============

#' originals
origdatadir <- "projdata/final/en_US"
flist <- dir(origdatadir)

sampdatadir <- "projdata/sampled/en_US"


# list of files
flist <- dir(origdatadir)


#' Using the functions defined for quiz 1 (not useful.... goto actual sampling 157)
#' ======================================


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




#  summarising function

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

# test
# sumfile(file.path(origdatadir, "en_US.news.txt"))






#' Determining file sizes
#' ----------------------

# calcuation
filesizes <- lapply(lapply(flist, 
                           function(fname) file.path(origdatadir, fname)),
                    sumfile)

# make a dataframe
df_filesizes <- data.frame(Fpath = character(0), 
                           Lines = integer(0),
                           Lwidth = integer(0),
                           stringsAsFactors = FALSE)
for (i in 1:3) {
        for (j in 1:3) {
                df_filesizes[i,j] <- filesizes[[i]][[j]]
        }
}

# anticipating the samplingsamplingnum
blocksize <- 100
samplingrate <- 0.25
samplingnum <- blocksize * samplingrate


df_filesizes$Numblocks <- df_filesizes$Lines %/% blocksize
df_filesizes$Numsamp <- df_filesizes$Numblocks * samplingnum + floor(df_filesizes$Lines %% blocksize / samplingrate)

df_filesizes <- cbind(data.frame(Fname = flist), df_filesizes) 



#' effectively sampling
#' ====================

# systematic sampling (rate = 1/4 by default)
sample_lines <- function(inputfilepath, # the input file path (string)
                         outputfilepath,
                         block_size = 4) {
        in_cnxn <- file(inputfilepath, 'r') # open a new connection

        out_cnxn <- file(outputfilepath, 'w')
        on.exit({close(in_cnxn)
                close(out_cnxn)})
        
        block_index <- 1
        repeat { # keep reading a block of lines until you reach the end of the cnxn.
                block_text <- readLines(in_cnxn, n = block_size) # read block
                block_length <- length(block_text)
                block_index <- block_index + 1
                if (block_length == 0) # if there's nothing left, leave the loop
                        break
                # process a block of text lines :  call the function 
                sline <- block_text[block_length]
                writeLines(out_cnxn,text = sline)
        }
        block_index
}

# test
sample_lines(inputfilepath = file.path("projdata", "Lorem.txt"), # the input file path (string)
             outputfilepath = file.path("projdata", "Outlorem.txt"),
             block_size = 4)


# actual sampling (rate = 1/8)
sapply(X = flist,
       FUN = function(fname, srate = 8){
               message(paste("Sampling", fname))
               sample_lines(inputfilepath = file.path(origdatadir, fname), # the input file path (string)
                            outputfilepath = file.path(sampdatadir, fname),
                            block_size = srate)
       } )


