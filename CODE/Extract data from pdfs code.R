install.packages("pdftools")
install.packages("tabulizer")
install.packages("rJava")


library(pdftools)
library(tabulizer)
library(tidyverse)
library(rJava)

#name pdf
pdfname <- "Official Statement 2019.pdf"


BondPDFRaw <- pdftools::pdf_text(str_c("./DATA/RAW/",pdfname))

# Get the text of the PDF
pdftools::pdf_info(str_c("./DATA/RAW/",pdfname))
                                 


# txt is a character vector where each page is a piece of the vector
# e.g., txt[1] is the text for page 1

txt <- pdftools::pdf_text(str_c("./DATA/RAW/",pdfname))
writeLines(txt[2])


#using tabulizer
Bondpdftables <- tabulizer::extract_tables(str_c("./DATA/RAW/",pdfname), 
                                           pages=2, output = "data.frame")
str(Bondpdftables)



#you can also import a scanned pdf that isn't machine readable
#more of that info is found in rladies/meetup-presentations_rtp github repo
https://github.com/rladies/meetup-presentations_rtp
