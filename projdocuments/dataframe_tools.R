# ==========================================================
# Dataframe tools
# Bruno Fischer Colonimos, 2017, october 29
# ==========================================================

inputdfvalue <- function(dframe, rowkeyvals, col, value){
        logicrow <- rep(TRUE, nrow(dframe))
        for (i in seq_along(rowkeyvals)){
                keyval <-  rowkeyvals[[i]]
                key <- keyval[1]
                val <- keyval[2]
                logicrow <- logicrow & dframe[[key]] == val
        }
        dframe[logicrow, col] <- value
        dframe
}


getdfvalue <- function(dframe, rowkeyvals, col){
        if (missing(col)) {col <- 1:ncol(dframe)}
        logicrow <- rep(TRUE, nrow(dframe))
        for (i in seq_along(rowkeyvals)){
                keyval <-  rowkeyvals[[i]]
                key <- keyval[1]
                val <- keyval[2]
                logicrow <- logicrow & (dframe[[key]] == val)
        }
        dframe[logicrow, col]

}


# ----------------------------------------------------------
# testing + examples
# ----------------------------------------------------------


# example
df <- data.frame( k1 = rep(c("u", "v", "w"),2),
                  k2 = rep(c("x", "y"), each = 3),
                  a = 1:6,
                  b = seq(10,60, by = 10),
                  c = seq(100,600, by = 100),
                  stringsAsFactors = FALSE )



getdfvalue(df, list(c("k1", "v"), c("k2", "x")))
getdfvalue(df, list(c("k1", "v"), c("k2", "y")),
           col=3:5 )
inputdfvalue(df, list(c("k1", "v"), c("k2", "x")),
             col="c",
             value = 9999)
inputdfvalue(df, list(c("k1", "u"), c("k2", "y")),
             col=c("b","c"),
             value = c(99,9999))
