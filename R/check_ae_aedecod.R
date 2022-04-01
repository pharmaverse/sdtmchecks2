#' @title Check for missing AEDECOD values
#'
#' @description This check looks for missing AEDECOD values
#'
#' @param AE Adverse Events SDTM dataset with variables USUBJID, AETERM, AEDECOD
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
#' @author Yinghui Miao, Stella Banjo(HackR 2021)
#'
#' @examples
#'
#' # test with sample data
#'
#' AE <- data.frame(
#'  USUBJID = 1:5,
#'  DOMAIN = c(rep("AE", 5)),
#'  AESEQ = 1:5,
#'  AESTDTC = 1:5,
#'  AETERM = 1:5,
#'  AEDECOD = 1:5,
#'  AESPID = c("/F:AEDE-D:47155-R:1/L:5/AT:INITIALEXTREME",
#'  "/F:AEDE-D:43355-R:2/L:5/AT:INITIALEXTREME",
#'  "/F:AEDE-D:43155-R:3/L:5/AT:INITIALEXTREME",
#'  "/F:TOXICITY_OTH-D:119232-R:97/L:5/AT:INITIALEXTREME",
#'  "/F:TOXICITY_OTH-D:119232-R:90"),
#'  stringsAsFactors = FALSE
#' )
#'
#' check_ae_aedecod(AE)
#'
#' AE$AEDECOD[1] = NA
#' AE$AEDECOD[2] = "NA"
#' AE$AEDECOD[3:5] = ""
#' check_ae_aedecod(AE)
#' check_ae_aedecod(AE,preproc=roche_derive_rave_row)
#'
#' AE$AEDECOD <- NULL
#' check_ae_aedecod(AE)
#' 
#'
#'
#'

check_ae_aedecod <- function(AE,preproc=identity,...){

  ###First check that required variables exist and return a message if they don't
  if(AE %lacks_any% c("USUBJID","AETERM","AEDECOD")){

    fail(lacks_msg(AE, c("USUBJID","AETERM","AEDECOD")))

  }else {

      #Apply company specific preprocessing function
      AE = preproc(AE,...)

      ### Subset AE to only records with missing AEDECOD
      mydf <- AE %>%
        select(any_of(c("USUBJID", "RAVE", "AESEQ","AESTDTC","AETERM","AEDECOD"))) %>%
        filter(is_sas_na(AEDECOD))
      rownames(mydf)=NULL

    ###Print to report
    ### Return message if no records with missing AEDECOD
    if(nrow(mydf)==0){
      pass()
    ### Return subset dataframe if there are records with missing AEDECOD
    }else if(nrow(mydf)>0){
        fail(paste("AE has ",nrow(mydf)," record(s) with missing AEDECOD. ",
                   sep=""), mydf)
    }
  }

}
