# TODO: Add comment
# 
# Author:  Brad
# File:    Clean_Raw_Filings.R
# Version: 1.0
# Date:    06.10.2014
# Purpose: Clean individual filings
###############################################################################

###############################################################################
# INITIAL SETUP;
cat("SECTION: INITIAL SETUP", "\n")
###############################################################################

# Clear workspace
rm(list = ls(all = TRUE))

# Limit History to not exceed 50 lines
Sys.setenv(R_HISTSIZE = 500)

repo <- c("http://cran.us.r-project.org")
options(repos = structure(repo))
options(install.packages.check.source = FALSE)
# String as factors is False -- used for read.csv
options(StringsAsFactors = FALSE)

# Default maxprint option
options(max.print = 500)
# options(max.print=99999)

# Memory limit
#memory.limit(size = 8183)

# Set location (1=HOME,2=WORK,3=LAPTOP,4=CORALSEA FROM HOME,5=CORALSEA FROM WORK,6=CORALSEA FROM LAPTOP)
Location <- 1

if (Location == 1) {
  #setwd("C:/Research_temp3/")
  #input_directory <- normalizePath("C:/Users/S.Brad/Dropbox/Research/Fund_Letters/Data",winslash="\\", mustWork=TRUE)
  output_directory <- normalizePath("F:/Research_temp3",winslash="\\", mustWork=TRUE)
  function_directory <- normalizePath("C:/Users/S.Brad/Dropbox/Research_Methods/R", winslash = "\\", mustWork = TRUE)
  #treetag_directory <- normalizePath("C:/TreeTagger",winslash="\\", mustWork=TRUE)    
  
} else if (Location == 2) {
  #setwd("C:/Research_temp3/")
  #input_directory <- normalizePath("C:/Users/bdaughdr/Dropbox/Research/Fund_Letters/Data",winslash="\\", mustWork=TRUE)
  output_directory <- normalizePath("C:/Research_temp3",winslash="\\", mustWork=TRUE)
  function_directory <- normalizePath("C:/Users/bdaughdr/Dropbox/Research_Methods/R",winslash="\\", mustWork=TRUE) 
  #treetag_directory <- normalizePath("C:/TreeTagger",winslash="\\", mustWork=TRUE)    
  
} else if (Location == 3) {
  #setwd("C:/Research_temp3/")
  #input_directory <- normalizePath("C:/Users/S.Brad/Dropbox/Research/Fund_Letters/Data",winslash="\\", mustWork=TRUE)
  output_directory <- normalizePath("C:/Research_temp3",winslash="\\", mustWork=TRUE)
  function_directory <- normalizePath("C:/Users/S.Brad/Dropbox/Research_Methods/R", winslash = "\\", mustWork = TRUE)
  #treetag_directory <- normalizePath("C:/TreeTagger",winslash="\\", mustWork=TRUE)  
  
} else if (Location == 4) {
  #setwd("//tsclient/C/Research_temp3/")
  #input_directory <- normalizePath("H:/Research/Mutual_Fund_Letters/Data", winslash = "\\", mustWork = TRUE)
  output_directory <- normalizePath("C:/Users/bdaughdr/Documents/Research_temp3",winslash="\\", mustWork=TRUE)
  function_directory <- normalizePath("//tsclient/C/Users/S.Brad/Dropbox/Research_Methods/R", winslash = "\\", mustWork = TRUE)
  #treetag_directory <- normalizePath("//tsclient/C/TreeTagger",winslash="\\", mustWork=TRUE)    
  
} else if (Location == 5) {
  #setwd("//tsclient/C/Research_temp3/")
  #input_directory <- normalizePath("H:/Research/Mutual_Fund_Letters/Data", winslash = "\\", mustWork = TRUE)
  output_directory <- normalizePath("C:/Users/bdaughdr/Documents/Research_temp3",winslash="\\", mustWork=TRUE)
  function_directory <- normalizePath("//tsclient/C/Users/bdaughdr/Dropbox/Research_Methods/R", winslash = "\\", mustWork = TRUE)
  #treetag_directory <- normalizePath("//tsclient/C/TreeTagger",winslash="\\", mustWork=TRUE)       
  
} else if (Location == 6) {
  #setwd("//tsclient/C/Research_temp2/")
  #input_directory <- normalizePath("H:/Research/Mutual_Fund_Letters/Data", winslash = "\\", mustWork = TRUE)
  output_directory <- normalizePath("C:/Research_temp3",winslash="\\", mustWork=TRUE)
  function_directory <- normalizePath("//tsclient/C/Users/S.Brad/Dropbox/Research_Methods/R", winslash = "\\", mustWork = TRUE)
  #treetag_directory <- normalizePath("//tsclient/C/TreeTagger",winslash="\\", mustWork=TRUE)         
  
} else {
  cat("ERROR ASSIGNING DIRECTORIES", "\n")
  
}
rm(Location)


###############################################################################
# FUNCTIONS;
cat("SECTION: FUNCTIONS", "\n")
###############################################################################

#source(file=paste(function_directory,"functions_db.R",sep="\\"),echo=FALSE)
#source(file=paste(function_directory,"functions_statistics.R",sep="\\"),echo=FALSE)
#source(file=paste(function_directory,"functions_text_analysis.R",sep="\\"),echo=FALSE)
source(file=paste(function_directory,"functions_text_parse.R",sep="\\"),echo=FALSE)
source(file=paste(function_directory,"functions_utilities.R",sep="\\"),echo=FALSE)

###############################################################################
# LIBRARIES;
cat("SECTION: LIBRARIES", "\n")
###############################################################################

#Load External Packages
external_packages <- c("data.table","gdata","plyr","reshape2","stringr","XML")
invisible(unlist(sapply(external_packages,load_external_packages, repo_str=repo, simplify=FALSE, USE.NAMES=FALSE)))
installed_packages <- list_installed_packages(external_packages)

rm(installed_packages,external_packages,repo)


###############################################################################
#PARAMETERS;
###############################################################################

#If using windows, set to "\\" - if mac (or unix), set to "/";
slash <- "\\"

#First year you want index files for:

#startyear <- 1993
startyear <- 2003

#Last year you want index files for:
#endyear <- 2004
endyear <- 2013

#First qtr you want index files for (usually 1):
startqtr <- 1

#Last qtr you want index files for (usually 4):
endqtr <- 4

downloadfolder <- "MF_Shareholder_Reports_N-SAR-B"

#The sub directory where the downloaded filings are
txtfolder <- "txt_extract"

#The sub directory where the combined filings will be located
txtfolder_clean <- "txt_clean"

#Output Folder
txtoutput <- "questions"

#The file that will contain the filings you want to download.
infile <- "filings_list_comb.csv"

yr_qtr_comb <- expand.grid(yr = seq(startyear, endyear, 1), qtr = seq(1, 4, 1))

yr_qtr_comb <- yr_qtr_comb[order(yr_qtr_comb[,"yr"],yr_qtr_comb[,"qtr"]),]
row.names(yr_qtr_comb) <- seq(nrow(yr_qtr_comb))

yr_qtr_comb[,"qtr"] <- ifelse((yr_qtr_comb[,"yr"]==startyear & yr_qtr_comb[,"qtr"] < startqtr),NA,yr_qtr_comb[,"qtr"])
yr_qtr_comb[,"qtr"] <- ifelse((yr_qtr_comb[,"yr"]==endyear & yr_qtr_comb[,"qtr"] > endqtr),NA,yr_qtr_comb[,"qtr"])

rm(startyear,startqtr,endyear,endqtr)

yr_qtr_comb <- yr_qtr_comb[(!is.na(yr_qtr_comb[,"qtr"])),]
row.names(yr_qtr_comb) <- seq(nrow(yr_qtr_comb))

yr_qtr_comb2 <- data.frame(yr_qtr_comb,yr_qtr=NA,stringsAsFactors=FALSE)

rm(yr_qtr_comb)

yr_qtr_comb2[,"yr_qtr"] <- paste(yr_qtr_comb2[,"yr"],yr_qtr_comb2[,"qtr"],sep="_")

#Check to see if output directory exists.  If not, create it.
create_directory(output_directory,remove=1)

#Check to see if download folder exists.  If not, create it.
download_folder_path <- paste(output_directory, downloadfolder, sep = slash, collapse = slash)  
create_directory(download_folder_path,remove=1)


###############################################################################
cat("Get list files \n")
###############################################################################

filings <- read.table(file=paste(download_folder_path,"\\",infile,sep=""), header = TRUE, na.strings="NA",stringsAsFactors=FALSE, 
                      sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")

filings2 <- data.frame(yr_qtr=NA,
                       filings,
                       stringsAsFactors=FALSE)

rm(filings)

filings2[,"yr_qtr"] <- paste(filings2[,"yr"],
                             filings2[,"qtr"],
                             sep="_")

filings2 <- filings2[,c(c("yr","qtr","yr_qtr"),
                        colnames(filings2[,!(colnames(filings2) %in% c("yr","qtr","yr_qtr"))]))]

filings_trim <- filings2[ filings2[,"yr_qtr"] %in% yr_qtr_comb2[,"yr_qtr"],]
row.names(filings_trim) <- seq(nrow(filings_trim))

rm(filings2,yr_qtr_comb2)

filings_trim2 <- data.frame(overall_id=NA,
                            filings_trim,
                            stringsAsFactors=FALSE)
filings_trim2[,"overall_id"] <- seq(1,nrow(filings_trim2),1)

rm(filings_trim)


###############################################################################
cat("Clean Files \n")
###############################################################################

#filings_info <- dlply(.data=filings_trim2[(filings_trim2[,"yr"]==2003 | filings_trim2[,"yr"]==2004 ),], .variables=c("yr"), .fun = function(x, path_output,subfolder,subfolder_output){
filings_info <- dlply(.data=filings_trim2, .variables=c("yr"), .fun = function(x, path_output,subfolder,subfolder_output){
  
  #  x <- filings_trim2[(filings_trim2[,"yr"]==2003),]
  #  x <- filings_trim2[(filings_trim2[,"yr"]==2004),]
  #  x <- filings_trim2[(filings_trim2[,"yr"]==2005),]
  #  x <- filings_trim2[(filings_trim2[,"yr"]==2006),]
  #  x <- filings_trim2[(filings_trim2[,"yr"]==2007),]
  #  x <- filings_trim2[(filings_trim2[,"yr"]==2008),]
  #  x <- filings_trim2[(filings_trim2[,"yr"]==2009),]
  #  x <- filings_trim2[(filings_trim2[,"yr"]==2010),]
  #  x <- filings_trim2[(filings_trim2[,"yr"]==2011),]
  #  x <- filings_trim2[(filings_trim2[,"yr"]==2012),]
  #  x <- filings_trim2[(filings_trim2[,"yr"]==2013),]
  
  #  path_output <- paste(output_directory,downloadfolder,sep=slash)
  #  subfolder <- txtfolder
  #  subfolder_output <- txtfolder_clean
  
  filings_trim2_short <- x[,!(colnames(x) %in% c("file_header","file_index_htm"))]
  
  yr <-  unique(x[,"yr"])
  
  cat("\n",yr,"\n")
  
  #Check to see if yr folder exists.  If not, create it.
  yr_folder_path <- paste(path_output, yr, sep = "\\", collapse = "\\")   
  create_directory(yr_folder_path,remove=1)
  
  sub_folder_path <- paste(yr_folder_path, subfolder, sep = "\\", collapse = "\\")   
  create_directory(sub_folder_path,remove=1)
  
  sub_folder_output_path <- paste(yr_folder_path, subfolder_output, sep = "\\", collapse = "\\")   
  create_directory(sub_folder_output_path,remove=1)
  
  #Get name of downloaded files
  downloaded_files <- data.frame(file=list.files(sub_folder_path),stringsAsFactors=FALSE)
  downloaded_files2 <- ddply(.data=downloaded_files, .variables=c("file"), .fun = function(x,folder){
    
    filepath <- paste(folder,x,sep="\\")
    output <- data.frame(filepath=filepath,file.info(filepath),stringsAsFactors=FALSE)
    
  }, folder=sub_folder_path, 
  .progress = "none", .inform = FALSE, .parallel = FALSE, .paropts = NULL, .id = NA)
  
  rm(downloaded_files)
  
  #downloaded_files2 <- downloaded_files2[order(downloaded_files2[,"filepath"]),]
  #downloaded_files2 <- downloaded_files2[order(downloaded_files2[,"size"]),]
  downloaded_files2 <- downloaded_files2[order(-downloaded_files2[,"size"]),]
  row.names(downloaded_files2) <- seq(nrow(downloaded_files2))
  
  downloaded_files3 <- data.frame(yr_id=NA,downloaded_files2,stringsAsFactors=FALSE)
  downloaded_files3[,"yr_id"] <- seq(1,nrow(downloaded_files3),1)
  
  rm(downloaded_files2)
  
  #output_files <- dlply(.data=downloaded_files3[1:3,], .variables=c("yr_id"), .fun = function(y,sub_folder_output_path){
  output_files <- dlply(.data=downloaded_files3[,], .variables=c("yr_id"), .fun = function(y,sub_folder_output_path){
    
    #  y <- downloaded_files3[(downloaded_files3[,"file"]=="0000089043-03-000004.csv"),]
    #  y <- downloaded_files3[(downloaded_files3[,"file"]=="0000355414-03-000145.csv"),]
    #  y <- downloaded_files3[(downloaded_files3[,"file"]=="0000355414-03-000365.csv"),]
    #  y <- downloaded_files3[(downloaded_files3[,"file"]=="0000898531-03-000424.csv"),]
    #  y <- downloaded_files3[(downloaded_files3[,"file"]=="0001119381-03-000003.csv"),]
    
    #  y <- downloaded_files3[(downloaded_files3[,"file"]=="0001119381-04-000004.csv"),]
    
    #  y <- downloaded_files3[(downloaded_files3[,"file"]=="0000799195-10-000004.csv"),]
    
    #  y <- downloaded_files3[(downloaded_files3[,"file"]=="0001092720-11-000010.csv"),] 
    
    #  y <- downloaded_files3[(downloaded_files3[,"file"]=="0000898432-13-001710.csv"),]
    #  y <- downloaded_files3[(downloaded_files3[,"file"]=="0000898432-13-001711.csv"),]
    #  y <- downloaded_files3[(downloaded_files3[,"file"]=="0000898432-13-001712.csv"),]
    
    #  y <- downloaded_files3[1,]
    
    # sub_folder_output_path <- sub_folder_output_path
    
    file <- unique(y[,"file"])
    filepath <- unique(y[,"filepath"])
    
    #file_out <- file
    #filepath_out <- paste(sub_folder_output_path,file_out,sep="\\")
    
    cat(file,"\n")
    
    filing <- read.table(file=filepath, header = TRUE, na.strings="",stringsAsFactors=FALSE, 
                         sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")
    
    filing_no_text0 <- filing[,!(colnames(filing) %in% c("TEXT"))]
    filing_no_text1 <- unique(filing_no_text0)
    filing_no_text2 <- data.frame(file=NA,filing_no_text1,stringsAsFactors=FALSE)
    filing_no_text2[,"file"] <- file
    rm(filing_no_text0,filing_no_text1)
    
    filing_text0 <- filing[,(colnames(filing) %in% c("DOCUMENT_INDEX","TEXT"))]
    filing_text1 <- data.frame(file=NA,filing_text0,stringsAsFactors=FALSE)
    filing_text1[,"file"] <- file
    rm(filing_text0)
    
    #filing_text1 <- data.frame(file=NA,filing,stringsAsFactors=FALSE)
    #filing_text1[,"file"] <- file
    
    rm(filing)
    
    #questions_all <- formatC(seq(0,137,1), width = 3, format = "d", flag = "0") 
    questions_all <- formatC(c(seq(0,137,1),999), width = 3, format = "d", flag = "0")
    
    
    #ONLY KEEP DOCUMENT_INDEX OF 1
    filing_text1_trim <- filing_text1
    #filing_text1_trim <- filing_text1_trim[filing_text1_trim[,"DOCUMENT_INDEX"]==1,]
    
    rm(filing_text1)
    
    
    #CHECK TO SEE IF TEXT IS IN RIGHT FORMATTING
    
    flag1 <- max(ifelse(grepl("FORM N-SAR", filing_text1_trim[,"TEXT"]), 1, 0))
    flag2 <- max(ifelse(grepl("SEMI-ANNUAL REPORT", filing_text1_trim[,"TEXT"]), 1, 0))
    flag3 <- max(ifelse(grepl("FOR REGISTERED INVESTMENT COMPANIES", filing_text1_trim[,"TEXT"]), 1, 0))
    flag4 <- max(ifelse(grepl("REPORT AS OF THE END OF SEMIANNUAL PERIOD", filing_text1_trim[,"TEXT"]), 1, 0))
    
    flag_sum <- flag1 + flag2 + flag3 + flag4
    
    rm(flag1,flag2,flag3,flag4)
    
    if (flag_sum > 0) {
      
      cat("BAD FILING", "\n")
      
      questions_not_present <- questions_all
      
    } else {
      
      #cat("GOOD FILING", "\n")
      
      
      #REMOVE PAGES
      
      #Remove space after beginning and before end of tags
      filing_text_page_clean <- filing_text1_trim
      filing_text_page_clean[,c("TEXT")] <- gsub("<\\s*(?!$)","<", filing_text_page_clean[,c("TEXT")],perl=TRUE)
      filing_text_page_clean[,c("TEXT")] <- gsub("</\\s*(?!$)","</", filing_text_page_clean[,c("TEXT")],perl=TRUE)
      filing_text_page_clean[,c("TEXT")] <- gsub("\\s*>",">", filing_text_page_clean[,c("TEXT")],perl=TRUE)
      
      rm(filing_text1_trim)
      
      for (i in 1:ncol(filing_text_page_clean))
      {
        filing_text_page_clean[,i] <- unknownToNA(filing_text_page_clean[,i], unknown=c("",".","n/a","na","<NA>","NA",NA,"null","NULL",NULL,"nan","NaN",NaN,
                                                                                        NA_integer_,"NA_integer_",NA_complex_,"NA_complex_",
                                                                                        NA_character_,"NA_character_",NA_real_,"NA_real_"),force=TRUE)
        filing_text_page_clean[,i] <- ifelse(is.na(filing_text_page_clean[,i]),"", filing_text_page_clean[,i])
      } 
      rm(i)
      
      #Add page ending tags
      filing_text_page_clean[,"TEXT"] <- ifelse(grepl("<PAGE>", filing_text_page_clean[,"TEXT"]), paste(filing_text_page_clean[,"TEXT"],"</PAGE>",sep=" "), filing_text_page_clean[,"TEXT"])
      
      
      tags_sep_page <- c("PAGE")
      filing_text_page_sep <- ddply(.data=filing_text_page_clean,  .variables=c("file","DOCUMENT_INDEX"),  .fun = function(x,bycol,xmlcol,tags){
        
        # x <- filing_text_page_clean[filing_text_page_clean[,"DOCUMENT_INDEX"]==1,]
        # bycol <- c("file","DOCUMENT_INDEX")
        # xmlcol <- "TEXT"
        # tags <- tags_sep_page
        
        file_temp <- unique(x[,"file"])
        index_temp <- unique(x[,"DOCUMENT_INDEX"])
        
        #x_temp <- data.frame(x,id=NA,stringsAsFactors=FALSE)
        #colnames(x_temp) <- c(colnames(x),"id")
        #x_temp[,"id"] <- seq(1,nrow(x_temp),1)
        
        x_temp_split <- split_by_tag(x,xmlcol,tags)
        x_temp_split[,xmlcol] <- ifelse(x_temp_split[,xmlcol]==""," ",x_temp_split[,xmlcol])
        
        x_temp_split <- data.table(x_temp_split, key = "id")
        sep_temp <- x_temp_split[, list(TEXT = unlist(strsplit(TEXT, '\n', fixed=TRUE))), by = id]
        
        rm(x_temp_split)
        
        sep_temp <- sep_temp[, expand_id := sequence(.N), by = "id"]
        sep_temp <- as.data.frame(sep_temp,stringsAsFactors=FALSE)
        sep_temp <- sep_temp[,c("id","expand_id",xmlcol)]
        
        sep_temp <- sep_temp[,!(colnames(sep_temp) %in% c("id","expand_id"))]
        sep_temp_out <- data.frame(file=NA,index=NA,sep_temp,stringsAsFactors=FALSE)
        rm(sep_temp)
        
        sep_temp_out[,"file"] <- file_temp
        sep_temp_out[,"index"] <- index_temp
        colnames(sep_temp_out) <- c(bycol,xmlcol)
        
        return(sep_temp_out)
        
      }, bycol=c("file","DOCUMENT_INDEX"),xmlcol="TEXT", tags=tags_sep_page,
      .progress = "none", .inform = FALSE, .drop = FALSE, .parallel = FALSE, .paropts = NULL)
      
      rm(filing_text_page_clean)
      
      filing_text_page_id <- data.frame(filing_text_page_sep,page_tag_temp=NA,stringsAsFactors=FALSE)
      colnames(filing_text_page_id)[match("page_tag_temp",names(filing_text_page_id))] <- "PAGE_INDEX"
      rm(filing_text_page_sep)
      
      filing_text_page_id[,"PAGE_INDEX"] <- ifelse(grepl("<PAGE", filing_text_page_id[,"TEXT"]), "<PAGE>", filing_text_page_id[,"PAGE_INDEX"])
      filing_text_page_id[,"PAGE_INDEX"] <- ifelse(grepl("</PAGE", filing_text_page_id[,"TEXT"]), "</PAGE>", filing_text_page_id[,"PAGE_INDEX"])
      
      index_page_temp <- llply(.data=tags_sep_page, create_tag_index,data=filing_text_page_id, tag_raw_col="PAGE_INDEX",
                               .progress = "none", .inform = FALSE,.parallel = FALSE, .paropts = NULL)
      index_page <- do.call(cbind, index_page_temp)
      index_page <- as.data.frame(index_page,stringsAsFactors=FALSE)
      colnames(index_page) <- paste(tags_sep_page,"INDEX",sep="_")
      colnames(index_page) <- gsub("-","_", colnames(index_page))
      rm(index_page_temp)
      
      filing_text_page_id[,"PAGE_INDEX"] <- index_page
      rm(index_page)
      
      if(c("PAGE_INDEX") %in% colnames(filing_text_page_id)) {
        
        filing_text_page_id_trim <- filing_text_page_id[(filing_text_page_id[,c("PAGE_INDEX")]==0),]
        
      } else {
        
        #cat("NO page SECTION","\n")
        
        filing_text_page_id_trim <- filing_text_page_id
        
      }
      rm(filing_text_page_id)
      
      filing_text_page_id_trim <- filing_text_page_id_trim[,!(colnames(filing_text_page_id_trim) %in% c("PAGE_INDEX"))]
      row.names(filing_text_page_id_trim) <- seq(nrow(filing_text_page_id_trim))
      
      
      #CLEAN TEXT
      
      filing_text_clean <- filing_text_page_id_trim
      
      #filing_text_clean[,"TEXT"] <- gsub(pattern="\\.{2,}", replacement="\\.", x=filing_text_clean[,"TEXT"])
      #filing_text_clean[,"TEXT"] <- gsub(pattern="\\.{2,}", replacement="\\.", x=filing_text_clean[,"TEXT"])
      #filing_text_clean[,"TEXT"] <- gsub(pattern="\\.{2,}", replacement="\\.", x=filing_text_clean[,"TEXT"])
      #filing_text_clean[,"TEXT"] <- gsub(pattern="\\.{2,}", replacement="\\.", x=filing_text_clean[,"TEXT"])
      #filing_text_clean[,"TEXT"] <- gsub(pattern="  ", replacement=" ", x=filing_text_clean[,"TEXT"])
      #filing_text_clean[,"TEXT"] <- gsub(pattern="  ", replacement=" ", x=filing_text_clean[,"TEXT"])
      #filing_text_clean[,"TEXT"] <- gsub(pattern="  ", replacement=" ", x=filing_text_clean[,"TEXT"])
      #filing_text_clean[,"TEXT"] <- gsub(pattern="  ", replacement=" ", x=filing_text_clean[,"TEXT"])
      filing_text_clean[,"TEXT"] <- gsub("^\\s+|\\s+$", "", filing_text_clean[,"TEXT"])
      
      rm(filing_text_page_id_trim)
      
      
      #REMOVE BLANK TEXT
      
      filing_text_trim <- filing_text_clean
      
      filing_text_trim <- filing_text_trim[!is.na(filing_text_trim[,"TEXT"]),]
      filing_text_trim <- filing_text_trim[!filing_text_trim[,"TEXT"]=="",]
      
      rm(filing_text_clean)
      
      
      #REMOVE FIRST WORDS WITH NO NUMBERS
      
      #       ###############################################################
      #       temp <- data.frame(TEXT=NA,stringsAsFactors=FALSE)
      #       temp[1,] <- "042 H00AA00   0"
      #       temp[2,] <- "043  00AA00      0"
      #       temp[3,] <- "048  000100  0.750"
      #       temp[4,] <- "048 A010100        0"
      #       temp[5,] <- "054 A00AA00 N"
      #       temp[6,] <- "072 Y000100       47"
      #       temp[7,] <- "072AA000100    12551"
      #       temp[8,] <- "072BB000100        0"
      #       temp[9,] <- "072 Z000300     -115"
      #       temp[10,] <- "072AA000300        0"
      #       temp[11,] <- "072BB000300      607"
      #       temp[12,] <- "072CC020300        0"
      #       temp[13,] <- "073 A010300   0.0000"
      #       temp[14,] <- "077 Q030000 N"
      #       temp[15,] <- "078  000000 N"
      #       temp[16,] <- "085 B00AA00 N"
      #       temp[17,] <- "000A 0 AA1  FEDERATED SERVICES COMPANY"
      #       temp[18,] <- "075 A000600        0"
      #       temp[19,] <- "076  000600     0.00"
      #       temp[20,] <- "000C 1 100  10"
      # 
      #       filing_text_trim_backup <-  filing_text_trim
      #       filing_text_trim <- temp
      #       ###############################################################
      
      
      filing_text_sig_title <-  data.frame(filing_text_trim,
                                           org_order=NA,
                                           TEXT_CLEAN=NA,
                                           item_number=NA,
                                           item_letter=NA,
                                           item_subnumber=NA,
                                           series_number=NA,
                                           repetition=NA,
                                           value=NA,
                                           error=NA,
                                           stringsAsFactors=FALSE)
      
      filing_text_sig_title <- filing_text_sig_title[,c("org_order",colnames(filing_text_sig_title[,!(colnames(filing_text_sig_title) %in% c("org_order"))]))]
      filing_text_sig_title[,"org_order"] <- seq(1,nrow(filing_text_sig_title))
      
      
      filing_text_sig_title[,"item_number"] <- ldply(filing_text_sig_title[,"TEXT"], function(z) {
        
        return(unlist(strsplit(z, split=" "))[1])
      })
      rm(filing_text_trim)
      
      filing_text_sig_title2 <- filing_text_sig_title
      #filing_text_sig_title2 <- filing_text_sig_title2[!(filing_text_sig_title2[,"item_number"]=="SIGNATURE"),]
      #filing_text_sig_title2 <- filing_text_sig_title2[!(filing_text_sig_title2[,"item_number"]=="TITLE"),]
      
      filing_text_sig_title2[,"item_letter"] <- nchar(filing_text_sig_title2[,"item_number"])
      filing_text_sig_title2[,"item_subnumber"] <- gsub("[^0-9]","",filing_text_sig_title2[,"item_number"])
      filing_text_sig_title2[,"series_number"] <- nchar(filing_text_sig_title2[,"item_subnumber"])
      
      rm(filing_text_sig_title)
      
      filing_text_sig_title3 <- filing_text_sig_title2[!(filing_text_sig_title2[,"series_number"]==0),]
      
      if(nrow(filing_text_sig_title3)==0){      
        
        filing_text_sig_title3 <- filing_text_sig_title2[1,]
        filing_text_sig_title3[,"TEXT"] <- paste("999__00",filing_text_sig_title3[,"org_order"],"00  TRASH FILINGS!!",sep="")
        filing_text_sig_title3[,"item_number"] <- NA
        filing_text_sig_title3[,"item_letter"] <- NA
        filing_text_sig_title3[,"item_subnumber"] <- NA
        filing_text_sig_title3[,"series_number"] <- NA
        filing_text_sig_title3[,"repetition"] <- NA
      }
       
      rm(filing_text_sig_title2)
      
      filing_text_sig_title4 <- filing_text_sig_title3
      filing_text_sig_title4[,"item_number"] <- substr(filing_text_sig_title4[,"TEXT"], 1, 3)
      filing_text_sig_title4[,"item_letter"] <- nchar(filing_text_sig_title4[,"item_number"])
      filing_text_sig_title4[,"item_subnumber"] <- gsub("[^0-9]","",filing_text_sig_title4[,"item_number"])
      filing_text_sig_title4[,"series_number"] <- nchar(filing_text_sig_title4[,"item_subnumber"])
      filing_text_sig_title4[,"repetition"] <- filing_text_sig_title4[,"item_letter"] - filing_text_sig_title4[,"series_number"] 
      
      rm(filing_text_sig_title3)
      
      filing_text_sig_title5 <- filing_text_sig_title4[filing_text_sig_title4[,"repetition"]==0,]
      filing_text_sig_title5 <- filing_text_sig_title5[rowSums(is.na(filing_text_sig_title5[,1:ncol(filing_text_sig_title5)]))<(ncol(filing_text_sig_title5)),]
      
      if(nrow(filing_text_sig_title5)==0){ 
        
        filing_text_sig_title5 <- filing_text_sig_title4[1,]
        filing_text_sig_title5[,"TEXT"] <- paste("999__00",filing_text_sig_title5[,"org_order"],"00  TRASH FILINGS!!",sep="")
        filing_text_sig_title5[,"item_number"] <- NA
        filing_text_sig_title5[,"item_letter"] <- NA
        filing_text_sig_title5[,"item_subnumber"] <- NA
        filing_text_sig_title5[,"series_number"] <- NA
        filing_text_sig_title5[,"repetition"] <- NA
      }
      
      rm(filing_text_sig_title4)
      
      filing_text_sig_title6 <- filing_text_sig_title5
      filing_text_sig_title6[,"item_letter"] <- NA
      filing_text_sig_title6[,"item_subnumber"] <- NA
      filing_text_sig_title6[,"series_number"] <- NA
      filing_text_sig_title6[,"repetition"] <- NA
      
      #row.names(filing_text_sig_title6) <- seq(nrow(filing_text_sig_title6))
      
      rm(filing_text_sig_title5)
      
      
      #EXPAND ID VARIABLES
      
      filing_text_expand_id <- filing_text_sig_title6
      
      #See if question doesn't have subpart
      filing_text_expand_id[,"TEXT_CLEAN"] <- filing_text_expand_id[,"TEXT"]
      
      
      #filing_text_expand_id[,"TEXT_CLEAN"] <- ifelse(grepl("[0-9]",substr(filing_text_expand_id[,"TEXT_CLEAN"], 5, 5)),
      #                                               paste(substr(filing_text_expand_id[,"TEXT_CLEAN"], 1, 4),substr(filing_text_expand_id[,"TEXT_CLEAN"], 5, nchar(filing_text_expand_id[,"TEXT_CLEAN"])),sep="0"),
      #                                               filing_text_expand_id[,"TEXT_CLEAN"])
      
      filing_text_expand_id[,"item_letter"] <- substr(filing_text_expand_id[,"TEXT_CLEAN"], 4, 5)
      filing_text_expand_id[,"item_subnumber"] <- substr(filing_text_expand_id[,"TEXT_CLEAN"], 6, 7)
      filing_text_expand_id[,"series_number"] <- substr(filing_text_expand_id[,"TEXT_CLEAN"], 8, 9)
      filing_text_expand_id[,"repetition"] <- substr(filing_text_expand_id[,"TEXT_CLEAN"], 10, 11)
      #filing_text_expand_id[,"value"] <- substr(filing_text_expand_id[,"TEXT_CLEAN"], 12, nchar(encodeString(filing_text_expand_id[,"TEXT_CLEAN"])))
      filing_text_expand_id[,"value"] <- substr(filing_text_expand_id[,"TEXT_CLEAN"], 12, nchar(filing_text_expand_id[,"TEXT_CLEAN"]))
      
      rm(filing_text_sig_title6)
      
      
      #CLEAN DATA
      
      filing_text_expand_id_clean <- filing_text_expand_id
      
      rm(filing_text_expand_id)
      
      filing_text_expand_id_clean[,"item_letter"] <- gsub("^\\s+|\\s+$", "", filing_text_expand_id_clean[,"item_letter"])
      filing_text_expand_id_clean[,"item_letter"] <- formatC( filing_text_expand_id_clean[,"item_letter"], width = 2, format = "s", flag = " ") 
      filing_text_expand_id_clean[,"item_letter"] <- gsub(" ", "_", filing_text_expand_id_clean[,"item_letter"])
      
      filing_text_expand_id_clean[,"item_subnumber"] <- gsub("^\\s+|\\s+$", "", filing_text_expand_id_clean[,"item_subnumber"])
      filing_text_expand_id_clean[,"item_subnumber"] <- formatC( filing_text_expand_id_clean[,"item_subnumber"], width = 2, format = "s", flag = " ") 
      filing_text_expand_id_clean[,"item_subnumber"] <- gsub(" ", "0", filing_text_expand_id_clean[,"item_subnumber"])
      
      filing_text_expand_id_clean[,"repetition"] <- gsub("^\\s+|\\s+$", "", filing_text_expand_id_clean[,"repetition"])
      filing_text_expand_id_clean[,"repetition"] <- formatC( filing_text_expand_id_clean[,"repetition"], width = 2, format = "s", flag = " ") 
      filing_text_expand_id_clean[,"repetition"] <- gsub(" ", "0", filing_text_expand_id_clean[,"repetition"])
      
      filing_text_expand_id_clean[,"value"] <- gsub("^\\s+|\\s+$", "", filing_text_expand_id_clean[,"value"])
      
      
      #FIND ERROR ROWS
      
      filing_text_expand_id_trim <- filing_text_expand_id_clean
      
      filing_text_expand_id_trim[,"TEXT_CLEAN"] <- substr(filing_text_expand_id_trim[,"TEXT_CLEAN"], 1, 11)
      
      filing_text_expand_id_trim[,"TEXT_CLEAN"] <- gsub(pattern="  ", replacement=" ", x=filing_text_expand_id_trim[,"TEXT_CLEAN"])
      filing_text_expand_id_trim[,"TEXT_CLEAN"] <- gsub(pattern="  ", replacement=" ", x=filing_text_expand_id_trim[,"TEXT_CLEAN"])
      filing_text_expand_id_trim[,"TEXT_CLEAN"] <- gsub(pattern=" ", replacement="", x=filing_text_expand_id_trim[,"TEXT_CLEAN"])
      filing_text_expand_id_trim[,"TEXT_CLEAN"] <- gsub(pattern=" ", replacement="", x=filing_text_expand_id_trim[,"TEXT_CLEAN"])
      filing_text_expand_id_trim[,"TEXT_CLEAN"] <- gsub("^\\s+|\\s+$", "", filing_text_expand_id_trim[,"TEXT_CLEAN"])
      
      filing_text_expand_id_trim[,"error"] <- ifelse((filing_text_expand_id_trim[,"item_number"]=="000" & nchar(filing_text_expand_id_trim[,"TEXT_CLEAN"])<9),1,0)
      
      filing_text_expand_id_trim[,"item_number"] <- ifelse(filing_text_expand_id_trim[,"error"]==1,"999",filing_text_expand_id_trim[,"item_number"])
      #filing_text_expand_id_trim[,"item_letter"] <- ifelse(filing_text_expand_id_trim[,"error"]==1,"__",filing_text_expand_id_trim[,"item_letter"])
      #filing_text_expand_id_trim[,"item_subnumber"] <- ifelse(filing_text_expand_id_trim[,"error"]==1,"00",filing_text_expand_id_trim[,"item_subnumber"])
      #filing_text_expand_id_trim[,"series_number"] <- ifelse(filing_text_expand_id_trim[,"error"]==1,filing_text_expand_id_trim[,"org_order"],filing_text_expand_id_trim[,"series_number"])
      #filing_text_expand_id_trim[,"repetition"] <- ifelse(filing_text_expand_id_trim[,"error"]==1,"00",filing_text_expand_id_trim[,"repetition"])
      #filing_text_expand_id_trim[,"value"] <- ifelse(filing_text_expand_id_trim[,"error"]==1,filing_text_expand_id_trim[,"TEXT"],filing_text_expand_id_trim[,"value"])
      
      #filing_text_expand_id_trim <- filing_text_expand_id_trim[order(filing_text_expand_id_trim[,"file"], filing_text_expand_id_trim[,"DOCUMENT_INDEX"],
      #                                                               filing_text_expand_id_trim[,"item_number"], filing_text_expand_id_trim[,"series_number"],
      #                                                               filing_text_expand_id_trim[,"repetition"], filing_text_expand_id_trim[,"item_letter"],
      #                                                               filing_text_expand_id_trim[,"item_subnumber"]),]
      
      #filing_text_expand_id_trim_order <- c("file","DOCUMENT_INDEX","item_number","item_letter","item_subnumber","series_number","repetition")
      filing_text_expand_id_trim_order <- c("file","DOCUMENT_INDEX","item_number","series_number","repetition","item_letter","item_subnumber")
      filing_text_expand_id_trim <- setorderv(data.table(filing_text_expand_id_trim), filing_text_expand_id_trim_order, rep(1,length(filing_text_expand_id_trim_order)))
      filing_text_expand_id_trim <- as.data.frame(filing_text_expand_id_trim,stringsAsFactors=FALSE)
      
      rm(filing_text_expand_id_clean)
      
      
      #SEQUENCE DATA
      
      filing_text_cast_temp <- filing_text_expand_id_trim[,!(colnames(filing_text_expand_id_trim) %in% c("TEXT","TEXT_CLEAN","error"))]
      
      rm(filing_text_expand_id_trim)
      
      filing_text_cast_id <- ddply(.data=filing_text_cast_temp,  .variables=filing_text_expand_id_trim_order, .fun = function(x){
        
        #filing_text_cast_id <- ddply(.data=filing_text_cast_temp,  .variables=c("file","DOCUMENT_INDEX","item_number","series_number","repetition", "item_letter","item_subnumber"), .fun = function(x){
        
        x_out <- data.frame(x,row_id=NA,stringsAsFactors=FALSE)
        x_out[,"row_id"] <- seq(1,nrow(x_out))
        
        return(x_out)
        
      }, .progress = "none", .inform = FALSE, .drop = TRUE, .parallel = FALSE, .paropts = NULL)
      
      rm(filing_text_cast_temp)
      
      
      #CAST DATA
      
      #     filing_text_questions <- dlply(.data=filing_text_cast_id, .variables=c("file","DOCUMENT_INDEX","item_number"), .fun = function(x){
      #       
      #       # x <- filing_text_cast_id[filing_text_cast_id[,"item_number"]=="000",]
      #       # x <- filing_text_cast_id[filing_text_cast_id[,"item_number"]=="001",]
      #       # x <- filing_text_cast_id[filing_text_cast_id[,"item_number"]=="002",]
      #       # x <- filing_text_cast_id[filing_text_cast_id[,"item_number"]=="008",]
      #       # x <- filing_text_cast_id[filing_text_cast_id[,"item_number"]=="010",]
      #       # x <- filing_text_cast_id[filing_text_cast_id[,"item_number"]=="071",]
      #       # x <- filing_text_cast_id[filing_text_cast_id[,"item_number"]=="072",]
      # 
      #       filing_text_questions_temp <- dcast(x, file + DOCUMENT_INDEX + item_number + series_number + repetition + row_id ~ item_number + item_letter + item_subnumber, value.var = "value")
      # 
      #       return(filing_text_questions_temp)
      #       
      #     },.progress = "none", .inform = FALSE, .drop = TRUE, .parallel = FALSE, .paropts = NULL)
      
      questions_present <- unique(filing_text_cast_id[,"item_number"])
      
      #test <- filing_text_cast_id
      #test[,"DOCUMENT_INDEX"] <- 1
      #test <- rbind(test,filing_text_cast_id)
      
      for (j in 1:length(questions_present))
      {
        #j <- 1
        
        cast_temp <- dcast(filing_text_cast_id[filing_text_cast_id[,"item_number"]==questions_present[j],], 
                           file + DOCUMENT_INDEX + item_number + series_number + repetition + row_id ~ item_number + item_letter + item_subnumber, value.var = "value")
        
        col_all <- colnames(cast_temp)
        col_no_change <- c("file","DOCUMENT_INDEX","item_number","series_number","repetition","row_id")
        col_change <- col_all[!(col_all %in% col_no_change)]
        
        colnames(cast_temp) <- c(col_no_change,paste("q",col_change,sep="_"))
        
        cast_temp2 <- merge(filing_no_text2, cast_temp, 
                            by.x=c("file","DOCUMENT_INDEX"), by.y=c("file","DOCUMENT_INDEX"), 
                            all.x=FALSE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"))
        
        rm(cast_temp)
        
        assign(paste("filing_text_q", questions_present[j], sep = ""), cast_temp2, envir = .GlobalEnv)
        
        rm(cast_temp2,col_all,col_no_change,col_change)
      }
      rm(j)
      
      questions_not_present <- questions_all[!(questions_all %in% questions_present)]
      
      rm(filing_text_cast_id,questions_present)
      
    }
    rm(flag_sum)
    
    
    #CREATE EMPTY QUESTIONS FOR THE ONES THAT AREN'T PRESENT
    
    for (l in 1:length(questions_not_present))
    {
      #l <- 1
      
      #cast_temp <- data.frame(file=file,DOCUMENT_INDEX=1,item_number=questions_not_present[l],stringsAsFactors=FALSE)
      cast_temp <- filing_no_text2[,c("file","DOCUMENT_INDEX","TYPE")]
      colnames(cast_temp)[match("TYPE",names(cast_temp))] <- "item_number"
      cast_temp[,"item_number"] <- questions_not_present[l]
      
      
      cast_temp2 <- merge(filing_no_text2, cast_temp, 
                          by.x=c("file","DOCUMENT_INDEX"), by.y=c("file","DOCUMENT_INDEX"), 
                          all.x=FALSE, all.y=FALSE, sort=FALSE, suffixes=c(".x",".y"))
      
      rm(cast_temp)
      
      assign(paste("filing_text_q", questions_not_present[l], sep = ""), 
             cast_temp2, 
             envir = .GlobalEnv)
      
      rm(cast_temp2)
    }
    rm(questions_not_present,l)
    
    str_comb <- paste0("filing_text_questions <- list(",paste("filing_text_q", questions_all, sep="", collapse = ","),")")
    expr_comb <- parse(text=str_comb)
    eval(expr_comb)
    rm(str_comb,expr_comb)
    
    str_rm <- paste0("rm(",paste("filing_text_q", questions_all, sep="", collapse = ","),",envir = .GlobalEnv)")
    expr_rm <- parse(text=str_rm)
    eval(expr_rm)
    rm(str_rm,expr_rm)
    
    rm(questions_all)
    rm(file,filepath)
    #rm(file_out,filepath_out)
    rm(filing_no_text2)
    
    
    
    return(filing_text_questions)
    
  },
  sub_folder_output_path=sub_folder_output_path,
  .progress = "none",.inform = FALSE, .drop = TRUE, .parallel = FALSE, .paropts = NULL)
  
  
  #questions_all <- formatC(seq(0,137,1), width = 3, format = "d", flag = "0") 
  questions_all <- formatC(c(seq(0,137,1),999), width = 3, format = "d", flag = "0")
  
  for (k in 1:length(questions_all))
  {
    # k <- 1
    # k <- 139
    
    #cat("Question Num", k, "\n") 
    #cat("Question ID", questions_all[k], "\n") 
    
    #output_files_comb0 <- sapply(output_files, "[", (as.numeric(questions_all[k])+1))
    output_files_comb0 <- sapply(output_files, "[", k)
    output_files_comb1 <- rbindlist(output_files_comb0,fill=TRUE,use.names=TRUE)
    output_files_comb  <- data.frame(yr=NA,output_files_comb1,stringsAsFactors=FALSE)
    output_files_comb[,"yr"] <- yr
    
    rm(output_files_comb0,output_files_comb1)
    
    output_files_comb_id_cols_possible <- c("yr","file","DOCUMENT_INDEX","TYPE","SEQUENCE","FILENAME","DESCRIPTION")
    
    output_files_comb_id_cols <- colnames(output_files_comb)[colnames(output_files_comb) %in% output_files_comb_id_cols_possible]
    
    output_files_comb_question_cols <- colnames(output_files_comb)[grep(questions_all[k], colnames(output_files_comb))]
    output_files_comb_question_cols <- sort(output_files_comb_question_cols)
    
    output_files_comb_other_cols <- colnames(output_files_comb)[!(colnames(output_files_comb) %in% c(output_files_comb_id_cols,output_files_comb_question_cols))]
    
    #output_files_comb <- output_files_comb[,c(output_files_comb_id_cols,colnames(output_files_comb[,!(colnames(output_files_comb) %in% c(output_files_comb_id_cols))]))]
    output_files_comb <- output_files_comb[,c(output_files_comb_id_cols,output_files_comb_other_cols,output_files_comb_question_cols)]
    
    rm(output_files_comb_id_cols_possible,output_files_comb_id_cols,output_files_comb_other_cols,output_files_comb_question_cols)
    
    assign(paste("filing_text_comb_q", questions_all[k], sep = ""), output_files_comb, envir = .GlobalEnv)
    
    write.table(output_files_comb,file=paste(sub_folder_output_path,"\\",paste("nsar_b_temp_q", questions_all[k], sep = ""),".csv",sep=""), append=FALSE, na="NA", 
                sep = ",", quote = TRUE,dec = ".",  qmethod = "double", col.names=TRUE, row.names = FALSE)
    
    rm(output_files_comb)
    
  }
  rm(k)
  
  file_str <- paste("filing_text_comb_q", questions_all, sep="", collapse = ",")
  
  str_comb <- paste0("filing_text_questions_comb <- list(",file_str,")")
  expr_comb <- parse(text=str_comb)
  eval(expr_comb)
  
  str_rm <- paste0("rm(",file_str,",envir = .GlobalEnv)")
  expr_rm <- parse(text=str_rm)
  eval(expr_rm)
  
  #rm(str_comb,expr_comb,str_rm,expr_rm)
  rm(file_str)
     
  rm(output_files)
  
  rm(yr,yr_folder_path,sub_folder_path,sub_folder_output_path,downloaded_files3)
  
  return(questions_all)
  
},
path_output=paste(output_directory,downloadfolder,sep=slash),
subfolder=txtfolder,subfolder_output=txtfolder_clean,
.progress = "text",.inform = TRUE, .drop = TRUE, .parallel = FALSE, .paropts = NULL)


###############################################################################
cat("Output Files \n")
###############################################################################

#Check to see if download folder exists.  If not, create it.
question_folder_path <- paste(output_directory, downloadfolder, txtoutput, sep = slash, collapse = slash)  
create_directory(question_folder_path,remove=1)

yrs <- unique(filings_trim2[,"yr"])

questions_all <- formatC(c(seq(0,137,1),999), width = 3, format = "d", flag = "0")

for (m in 1:length(questions_all))
{
  # m <- 1
  # m <- 139
  
  #cat("Question Num", m, "\n") 
  cat("Question ID", questions_all[m], "\n") 
  
  filings_info_comb <- ldply(.data=yrs, .fun = function(x,path_output,subfolder_output,question_str,question_id){
    
    # x <- 2003
    # path_output <- paste(output_directory,downloadfolder,sep=slash)
    # subfolder_output <- txtfolder_clean
    # question_str <- "nsar_b_temp_q"
    # question_id <- questions_all[m]
    
    yr <-  as.numeric(x)
    
    #cat("\n",yr,"\n")
    
    #Check to see if yr folder exists.  If not, create it.
    yr_folder_path <- paste(path_output, yr, sep = "\\", collapse = "\\")   
    #create_directory(yr_folder_path,remove=1)
    
    sub_folder_output_path <- paste(yr_folder_path, subfolder_output, sep = "\\", collapse = "\\")   
    #create_directory(sub_folder_output_path,remove=1)

    question_str_full <- paste(question_str,question_id,".csv",sep="")
    
    filings <- read.table(file=paste(sub_folder_output_path,"\\",question_str_full,sep=""), header = TRUE, na.strings="NA",stringsAsFactors=FALSE, 
                          sep = ",", quote = "\"",dec = ".", fill = TRUE, comment.char = "")
    
    rm(yr,yr_folder_path,sub_folder_output_path,question_str_full)
    
    return(filings)
    
  }, path_output=paste(output_directory,downloadfolder,sep=slash), subfolder_output=txtfolder_clean, 
  question_str="nsar_b_temp_q",question_id=questions_all[m], 
  .progress = "none", .inform = FALSE, .parallel = FALSE, .paropts = NULL, .id = NA)

  #filings_info_comb0 <- sapply(filings_info, "[", (as.numeric(questions_all[m])+1))
  #filings_info_comb0 <- sapply(filings_info, "[", m)
  #filings_info_comb1 <- rbindlist(filings_info_comb0,fill=TRUE,use.names=TRUE)
  #filings_info_comb  <- as.data.frame(filings_info_comb1,stringsAsFactors=FALSE) 
  #rm(filings_info_comb0,filings_info_comb1)
  
  
  ## ORDER COLUMNS
  
  filings_info_comb_id_cols_possible <- c("yr","file","DOCUMENT_INDEX","TYPE","SEQUENCE","FILENAME","DESCRIPTION")
  
  filings_info_comb_id_cols <- colnames(filings_info_comb)[colnames(filings_info_comb) %in% filings_info_comb_id_cols_possible]
  
  filings_info_comb_question_cols <- colnames(filings_info_comb)[grep(questions_all[m], colnames(filings_info_comb))]
  filings_info_comb_question_cols <- sort(filings_info_comb_question_cols)
  
  filings_info_comb_other_cols <- colnames(filings_info_comb)[!(colnames(filings_info_comb) %in% c(filings_info_comb_id_cols,filings_info_comb_question_cols))]
  
  #filings_info_comb <- filings_info_comb[,c(filings_info_comb_id_cols,colnames(filings_info_comb[,!(colnames(filings_info_comb) %in% c(filings_info_comb_id_cols))]))]
  filings_info_comb <- filings_info_comb[,c(filings_info_comb_id_cols,filings_info_comb_other_cols,filings_info_comb_question_cols)]
  
  rm(filings_info_comb_id_cols_possible,filings_info_comb_id_cols,filings_info_comb_other_cols,filings_info_comb_question_cols)
  
  
  ## FIX COLUMN TYPE
  
  col_temp <- "item_number"
  
  #if (col_temp %in% colnames(filings_info_comb)) { filings_info_comb[,col_temp] <- as.numeric(filings_info_comb[,col_temp]) }
  
  col_temp <- "repetition"
  
  #if (col_temp %in% colnames(filings_info_comb)) { filings_info_comb[,col_temp] <- as.numeric(filings_info_comb[,col_temp]) }
  
  col_temp <- "row_id"
  
  #if (col_temp %in% colnames(filings_info_comb)) { filings_info_comb[,col_temp] <- as.numeric(filings_info_comb[,col_temp]) }
  
  col_temp <- "item_subnumber"
  
  #if (col_temp %in% colnames(filings_info_comb)) { filings_info_comb[,col_temp] <- as.numeric(filings_info_comb[,col_temp]) }
  
  write.table(filings_info_comb,file=paste(question_folder_path,"\\",paste("nsar_b_q", questions_all[m], sep = ""),".csv",sep=""), append=FALSE, na="NA", 
              sep = ",", quote = TRUE,dec = ".",  qmethod = "double", col.names=TRUE, row.names = FALSE)
  
  rm(filings_info_comb,col_temp)
}
rm(m)



###############################################################################
cat("BAD Files \n")
###############################################################################

# aa <- ls()
# aa <- as.data.frame(aa,stringsAsFactors=FALSE)
# aa[,"flag"] <- ifelse(grepl("filing_text_q",aa[,"aa"]),1,0)
# bb <- aa[aa[,"flag"]==1,]
# 
# cc <- bb
# cc[,"flag"] <- seq(1:nrow(cc))
# 
# dd <- dlply(.data=cc,  .variables=c("aa"),  .fun = function(x){
#   
#   # x <- cc[1,]
#   
#   x_out <- get(x[,"aa"])
#   
#   x_cols_all <- colnames(x_out)
#   x_cols_good <- c("file","DOCUMENT_INDEX","TYPE","SEQUENCE","FILENAME","DESCRIPTION","item_number","series_number","repetition","row_id")
#   x_cols_bad <- x_cols_all[!(x_cols_all %in% x_cols_good)]
#   
#   colnames(x_out)[match(x_cols_bad,names(x_out))] <- "bad_question_name"
#   
#   x_out2 <- data.frame(bad_file_name=NA,x_out,stringsAsFactors=FALSE)
#   x_out2[,"bad_file_name"] <- x[,"aa"]
#   
#   assign(paste("filing_text_comb", x["flag"], sep = ""),  x_out2, envir = .GlobalEnv)
#   
#   return(x_out2)
#   
# }, .progress = "none", .inform = FALSE, .drop = TRUE, .parallel = FALSE, .paropts = NULL)
# 
# str_temp <- paste0("bad_list <- rbindlist(list(",paste("filing_text_comb",cc[,"flag"], sep="", collapse = ","),"),fill=TRUE)")
# expr_temp <- parse(text=str_temp)
# eval(expr_temp)
# 
# bad_list <- as.data.frame(bad_list,stringsAsFactors=FALSE)
# 
# str(bad_list)
# 
# #Check to see if download folder exists.  If not, create it.
# question_bad_folder_path <- paste(output_directory, downloadfolder, "questions_bad", sep = slash, collapse = slash)  
# create_directory(question_bad_folder_path,remove=1)
# 
# write.table(bad_list,file=paste(question_bad_folder_path,"\\","nsar_b_questions_bad_full",".csv",sep=""), append=FALSE, na="NA", 
#             sep = ",", quote = TRUE,dec = ".",  qmethod = "double", col.names=TRUE, row.names = FALSE)
# 
# 
# bad_list_u <- unique(bad_list[,c("file","DOCUMENT_INDEX","TYPE","SEQUENCE","FILENAME","DESCRIPTION","row_id")])
# 
# str(bad_list_u)
# 
# write.table(bad_list_u,file=paste(question_bad_folder_path,"\\","nsar_b_questions_bad_unique",".csv",sep=""), append=FALSE, na="NA", 
#             sep = ",", quote = TRUE,dec = ".",  qmethod = "double", col.names=TRUE, row.names = FALSE)

