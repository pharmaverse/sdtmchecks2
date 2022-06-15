#' @title Check for missing CMDECOD values
#'
#' @description This check looks for missing CMDECOD values
#'
#' @param CM Concomitant Medications SDTM dataset with variables USUBJID, CMTRT, CMDECOD
#' @param preproc An optional company specific preprocessing script
#' @param ... Other arguments passed to methods
#'
#' @return boolean value if check failed or passed with 'msg' attribute if the test failed
#'
#' @export
#'
#' @importFrom dplyr %>% filter select
#' @importFrom tidyselect any_of
#'
#' @author Lei Zhao, Stella Banjo (HackR 2021)
#'
#' @examples
#'
#' # test with sample data
#' 
#' CM <- data.frame(
#'  USUBJID = 1:5,
#'  DOMAIN = c(rep("CM", 5)),
#'  CMTRT = rep("DRUG TERM", 5),
#'  CMDECOD = rep("CODED DRUG TERM", 5),,
#'  CMSTDTC = 1:5,
#'  CMENDTC = 1:5,
#'  CMCAT = "CONCOMITANT MEDICATIONS,
#'  CMSPID = c("/F:CMPG-D:47155-R:1/L:5/AT:TEXT",
#'  "/F:CMPG-D:43355-R:2/L:5/AT:TEXT",
#'  "/F:CMPG-D:43155-R:3/L:5/AT:TEXT",
#'  "/F:CMPG-D:119232-R:97/L:5/AT:TEXT",
#'  "/F:CMPG-D:119232-R:90"),
#'  stringsAsFactors=FALSE
#' )
#'
#' check_cm_cmdecod(CM)
#'
#' CM$CMDECOD[1] = NA
#' CM$CMDECOD[2] = "NA"
#' CM$CMDECOD[3:5] = ""
#' check_cm_cmdecod(CM)
#' check_cm_cmdecod(CM,preproc=roche_derive_rave_row)
#'
#' CM$CMDECOD <- NULL
#' check_cm_cmdecod(CM)
#' 
#'
#'
#'

check_cm_cmdecod <- function(CM,preproc=identity,...){
  
  ###First check that required variables exist and return a message if they don't
  if(CM %lacks_any% c("USUBJID","CMTRT","CMDECOD")){
    
    fail(lacks_msg(CM, c("USUBJID","CMTRT","CMDECOD")))
    
  }else {
    
    #Apply company specific pre-processing function
    CM = preproc(CM,...)
    
    ### Subset domain to only records with missing coded term (CMDECOD)
    mydf <- CM %>%
      select(any_of(c("USUBJID", "RAVE", "CMSEQ","CMSTDTC","CMTRT","CMDECOD", "CMPRESP", "CMOCCUR"))) %>%
      filter(is_sas_na(CMDECOD))
    rownames(mydf)=NULL
    
    ###Print to report
    ### Return message if no records with missing CMDECOD
    if(nrow(mydf)==0){
      pass()
      ### Return subset dataframe if there are records with missing CMDECOD
    }else if(nrow(mydf)>0){
      fail(paste("CM has ",nrow(mydf)," record(s) with missing CMDECOD. ",
                 sep=""), mydf)
    }
  }
  
}
