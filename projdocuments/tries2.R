


xtag$contexts <- c("latex", "html", "other")

xtag$addef <- function(tagname, defslist) {
        existingdefs = xtag$def
        nr <- length(xtag$context)
        # make a default definition dataframe
        defsdefault <- data.frame(context = xtag$contexts,
                                  tagname = rep(tagname, nr),
                                  begin = rep("", nr),
                                  end = rep("", nr),
                                  stringsAsFactors = FALSE)
        # input the given definition in the dataframe
        for (contextname in names(defslist)) {
                if (contextname %in% xtag$contexts) {
                        rowdef <- c(contextname, tagname, defslist[[contextname]])
                        defsdefault[defsdefault$context == contextname, ] <- rowdef
                } else {warning("non-standard tag context in definition")}
        }

        newdef <- defsdefault
        # return all the definitions
        rbind(existingdefs, newdef)
}



xtag$addef(tagname = "p", defslist = list(html = c(begin = "<p>", end = "</p>")))



