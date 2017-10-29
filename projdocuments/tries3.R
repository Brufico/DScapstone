



# context-detection functions
# ============================

# getcontext returns the doc conversion context
# From Hadley's ggplot2 book:  Knowing conversion target
is_latex <- function() {
        identical(knitr::opts_knit$get("rmarkdown.pandoc.to"), "latex")
}

# html same
is_html <- function() {
        identical(knitr::opts_knit$get("rmarkdown.pandoc.to"), "html")
}

getcontext <- function(){
        if (is_latex()) {"latex"
        } else if (is_html()) {
                "html"
        } else {"other"}
}

# tries
# message(getcontext())



# text formatting functions
# ===========================

# centertext <- function(x){paste0("<center>", x, "</center>")}

xtag <- local({
        # list the valid contexts
        contexts <- c("latex", "html", "other")
        # list the valid string type
        strtype <- c("begin", "end")
        # default string
        defaultstring <- ""
        deffields <- c("context", "tagname", "stringtype", "stringdef")
        dimdefs <- length(context) * length(strtype)
        # create the (empty) definitions dataframe
        # tagdefs <- data.frame(context = character(),
        #                         tagname = character(),
        #                         stringtype= character() ,
        #                         stringdef = character(),
        #                         stringsAsFactors = FALSE)
        # create the (empty) definitions dataframe
        makeemptydefs <- function(){
                defs <- lapply(fields, function(field){character()} )
                names(defs) <- fields
                defs <- as.data.frame(defs, stringsAsFactors = FALSE)
                defs
        }

        makedefaultdefs <- function(){
                defs <- lapply(fields, function(field){character(dimdefs)} )
                names(defs) <- fields
                defs <- as.data.frame(defs, stringsAsFactors = FALSE)
                defs$context <- rep(contexts, length(strtype))
                defs$stringtype <- rep(strtype, length(contexts))
                for (i in seq_along(fields)[c(-1, -2)]){
                        defs[[i]] <- rep(defaultstring, dimdefs)
                }
                defs
        }
        # add a definition:
        # usage:
        # xtag$addef(tagname = "p",
        #            defslist = list(html = c(begin = "<p>", end = "</p>"),
        #                            latex = c(begin = "nothing",
        #                                      end = "something")
        #               ))



})

# try

dim = 3
fields <- c("a", "b", "c", "d")
lcols <- lapply(fields, function(name){character(dim)} )
names(lcols) <- fields
lcols <- as.data.frame(lcols, stringsAsFactors = FALSE )
lcols
str(lcols)

seq_along(fields)[c(-1, -2)]







