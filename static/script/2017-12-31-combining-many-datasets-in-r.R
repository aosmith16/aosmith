library(here) # v. 0.1
library(stringr) # v. 1.2.0
library(purrr) # v. 0.2.3


( allfiles = list.files(path = here("static", "data"),
                        pattern = "AB.csv|ab.csv",
                        full.names = TRUE,
                        recursive = TRUE) )


( test = read.csv(allfiles[1], 
                skip = 6,
                header = FALSE,
                col.names = c("date", "temperature") ) )


( allnames = str_split( allfiles[1], pattern = "/", simplify = TRUE) )


allnames[, ncol(allnames) - 2]


test$block = allnames[, ncol(allnames) - 2]
test


test$site = allnames[, ncol(allnames) - 1]


allnames[, ncol(allnames)]


str_extract(allnames[, ncol(allnames)], pattern = "[0-9](?=\\.)")


test$plot = str_extract(allnames[, ncol(allnames)], pattern = "[0-9](?=\\.)")


str_sub(allnames[, ncol(allnames)], start = -6, end = -5)


test$logloc = toupper( str_sub(allnames[, ncol(allnames)], start = -6, end = -5) )

test


read_fun = function(path) {
     test = read.csv(path, 
                skip = 6,
                header = FALSE,
                col.names = c("date", "temperature") )
     allnames = str_split( path, pattern = "/", simplify = TRUE)
     test$block = allnames[, ncol(allnames) - 2]
     test$site = allnames[, ncol(allnames) - 1]
     test$plot = str_extract(allnames[, ncol(allnames)], pattern = "[0-9](?=\\.)")
     test$logloc = toupper( str_sub(allnames[, ncol(allnames)], start = -6, end = -5) )
     test
}


read_fun(allfiles[1])


( combined_dat = map_dfr(allfiles, read_fun) )

