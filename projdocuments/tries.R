library(dplyr)

lx <- list(html = c(begin ="<center>", end = "</center>" ),
     latex =  c(begin = "{\\centering}", end = "}" ),
     other = c(begin = "", end = "" ))

names(lx)

lapply(names(lx),
      FUN = function(context){
              value <-lx[[context]]
              value
      }
      )
# --------------------------------------------------
xtag$defs <- data.frame(context = character(),
                        tagname = character(),
                        begin= character() ,
                        end = character(),
                        stringsAsFactors = FALSE)
#
# xtag$addef <- function(tagname, defsdf, existing = xtag$def){
#         n <- nrows(defs)
#         newdef <- data.frame(context = defs$context,
#                              tagname = rep(tagbname, n),
#                              begin = defs$begin,
#                              end = defs$end,
#                              stringsAsFactors = FALSE)
#         rbind(existing, newdef)
# }


xtag$contexts <- c("latex", "html", "other")

xtag$addef <- function(tagname, defslist, existing = xtag$def) {
        nr <- length(xtag$context)
        defsdefault <- data.frame(context = xtag$contexts,
                                  tagname = rep(tagname, nr),
                                  begin = rep("", nr),
                                  end = rep("", nr),
                                  stringsAsFactors = FALSE)
        lapply(names(defslist),
               FUN = function(contextname) {
                       if (contextname %in% xtag$contexts) {
                               rowdef <- c(contextname, tagname, defslist[[context]])
                               defsdefault[context == contextname, ] <- rowdef
                       } else {warning("non-standard tag context in definition")}
               }
        )
        newdef <- defsdefault
        rbind(existing, newdef)
}



xtag$addef(tagname = "p", defslist = list(html = c(begin = "<p>", end = "</p>")))





# example for p

defslist <- list(html = c(begin = "<p>", end = "</p>")) # not defined for the rest ==> use ""

# xtag$defaults <- c(begin = "", end = "")

xtag$contexts <- c("latex", "html", "other")
nr <- length(xtag$contexts)
tagname <- "p"
defsdefault <- data.frame(context = xtag$contexts,
                      tagname = rep(tagname, nr),
                      begin = rep("", nr),
                      end = rep("", nr),
                      stringsAsFactors = FALSE)

c("html", "p", defslist[["html"]])
defsdefault[defsdefault$context == "html",]
defsdefault[defsdefault$context == "html",] <- c("html", "p", defslist[["html"]])
defsdefault
