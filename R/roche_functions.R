#' @title Utility function to obtain Rave row
#'
#' @description This function derives the Rave row number from XXSPID
#'
#' @param dts dataset e.g. AE
#' @param domains domains you wish to identify a SPID variable from
#'
#' @return dataset with rave row number
#'
#' @export
#'
#' @author Stella Banjo (HackR 2021)
#'
#' @examples
#'
#' AE <- data.frame(
#'   STUDY = c(rep("GO29537", 6)),
#'   DOMAIN = c(rep("AE", 6)),
#'   USUBJID = c(rep("GO29537-278300-30221", 6)),
#'   AESEQ = c(1, 2, 3, 4, 5, 6),
#'   AETERM = c("ANEMIA", "ANEMIA", "ANOREXIA", "CONSTIPATION", "DIARRHEA", "FATIGUE"),
#'   AEDECOD = c("ANAEMIA", "ANAEMIA", "DECREASED APPETITE", "CONSTIPATION", "DIARRHOEA", "FATIGUE"),
#'   AESPID = c("/F:AEDE-D:22028232-R:13/L:13/AT:INITIALEXTREME",
#'              "/F:AEDE-D:22028232-R:16/L:16/AT:INITIALEXTREME",
#'              "/F:AEDE-D:22028232-R:2/L:2/AT:INITIALEXTREME",
#'              "/F:AEDE-D:22028232-R:19/L:19/AT:INITIALEXTREME",
#'              "/F:AEDE-D:22028232-R:5/L:5/AT:INITIALEXTREME",
#'              "/F:AEDE-D:22028232-R:20/L:20/AT:INITIALEXTREME"),
#'   AESTDTC = c("2015-08-12", "2015-09-02", "2015-07-02", "2015-08-19", "2015-07-17", "2015-08-12")
#' )
#'
#' roche_derive_rave_row(AE)
#'
#'
#' RS <- data.frame(
#'    USUBJID = c("1"),
#'    RSSPID = c("/F:EDT_RSP", "/F:MRSP1-D:23456-R:0", "/F:RRSP1-D:23456-R:0")
#'    )
#'
#' roche_derive_rave_row(RS)
#'
#' SS <- data.frame(
#'    USUBJID = c("1"),
#'    SSSPID = c("/F:EDT_SS", "/F:LTFU-D:23456-R:0", "/F:LTFU-D:23456-R:0", "123sdf",
#'    "/F:AEDE-D:22028232-R:20/L:20/AT:INITIALEXTREME")
#'    )
#'
#' roche_derive_rave_row(SS)
#'
#'


roche_derive_rave_row <- function(dts,domains=c("ae","cm")) {

    myvec <- paste0(toupper(unlist(domains)), "SPID")
    
    thevar=intersect(names(dts), myvec) #get --SPID variable of interest

    if(length(thevar)==1) { #Only create RAVE column if there is a single --SPID variable

        # Extract RAVE row number, finding the last occurrence of -R:
        RAVE_ROW <- ifelse(grepl("-R:", dts[[thevar]]),
                                sub("/.*-R:", "#", dts[[thevar]]),
                                "")
        RAVE_ROW <- sub("/.*", "", RAVE_ROW)

        # Extract RAVE form name
        RAVE_FORM <- ifelse(startsWith(as.character(dts[[thevar]]), "/F:")
                                & grepl("-D:", dts[[thevar]]),
                                 sub("-D:.*", "", substring(dts[[thevar]], 4)),
                                     "")

        dts[["RAVE"]] <- paste0(RAVE_FORM, RAVE_ROW)
        attr(dts[["RAVE"]], "label") <- "Rave Form"

        return(dts)

    }else(
        return(dts)
    )
}
