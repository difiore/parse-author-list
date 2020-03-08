library(readr)
library(tidyverse)
d <- read_csv("~/Desktop/authors.csv")

d <- separate(d, name, into=c("a","b", "c","d","e"), sep = " ", remove = FALSE,
         convert = FALSE, extra = "warn", fill = "left")

d <- mutate(d, last=ifelse(!is.na(d) & (d=="de" | d=="Di" | d=="der"| d=="del" | d=="ter" | d=="van"| d=="von"| endsWith(e, '.') | e=="III" | e== "Jr."), paste0(d," ", e), e))
d <- mutate(d, d=ifelse(d=="de" | d=="Di" | d=="der" | d=="del"| d=="ter" | d=="van"| d=="von"| endsWith(e, '.') | e=="III" | e== "Jr.", NA, d))

d <- mutate(d, last=ifelse(!is.na(c) & c=="van", paste0(c," ",last),last))
d <- mutate(d, c=ifelse(c=="van", "", c))

d[["e"]] <- NULL

d <- mutate(d, d=ifelse(!is.na(d),
                          ifelse(!is.na(str_locate(d,"-")[,1]),
                                 str_replace(d,".*-.*", paste0(substring(d,1,1),".-", substring(d, str_locate(d,"-")[,1] + 1, str_locate(d,"-")[,1] + 1),".")),
                                 paste0(substring(d,1,1),".")),
                          d))

d <- mutate(d, c=ifelse(!is.na(c),
                          ifelse(!is.na(str_locate(c,"-")[,1]),
                                 str_replace(c,".*-.*", paste0(substring(c,1,1),".-", substring(c, str_locate(c,"-")[,1] + 1, str_locate(c,"-")[,1] + 1),".")),
                                 paste0(substring(c,1,1),".")),
                          c))

d <- mutate(d, b=ifelse(!is.na(b),
                          ifelse(!is.na(str_locate(b,"-")[,1]),
                                 str_replace(b,".*-.*", paste0(substring(b,1,1),".-", substring(b, str_locate(b,"-")[,1] + 1, str_locate(b,"-")[,1] + 1),".")),
                                 paste0(substring(b,1,1),".")),
                          b))

d <- mutate(d, a=ifelse(!is.na(a),
                          ifelse(!is.na(str_locate(a,"-")[,1]),
                                 str_replace(a,".*-.*", paste0(substring(a,1,1),".-", substring(a, str_locate(a,"-")[,1] + 1, str_locate(a,"-")[,1] + 1),".")),
                                 paste0(substring(a,1,1),".")),
                          a))

d <- mutate(d, first = paste0(ifelse(!is.na(a),a,""),ifelse(!is.na(b),b,""),ifelse(!is.na(c),c,""),ifelse(!is.na(d),d,"")))

d[["a"]] <- NULL
d[["b"]] <- NULL
d[["c"]] <- NULL
d[["d"]] <- NULL

d <- mutate(d, fullname=paste0(last, ", ", first))

p <- paste(d$fullname, sep=",", collapse=", ")
p
