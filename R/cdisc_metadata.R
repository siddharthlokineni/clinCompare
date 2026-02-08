#' SDTM Metadata
#'
#' Returns metadata for SDTM domains following CDISC standards.
#' Provides information about required, expected, and permissible variables
#' for each SDTM domain.
#'
#' Variable definitions are based on the published CDISC SDTM Implementation
#' Guide. The canonical machine-readable source is the CDISC Library API
#' (\url{https://www.cdisc.org/cdisc-library}), which requires CDISC
#' membership. The metadata shipped with clinCompare is hand-curated from the
#' published IG specifications and should be cross-referenced with the
#' official CDISC Library for regulatory submissions.
#'
#' @param version Character string specifying the SDTM IG version.
#'   Supported values: "3.4" (default, based on SDTM v2.0),
#'   "3.3" (based on SDTM v1.7). Version "3.3" excludes domains
#'   introduced in v3.4 (GF, CP, BE, BS, SM, TD, TM).
#'
#' @return A named list where keys are SDTM domain codes and values are
#'   data.frames with columns:
#'   \describe{
#'     \item{variable}{Variable name (character)}
#'     \item{label}{Variable label/description (character)}
#'     \item{type}{Data type: "Char" for character or "Num" for numeric}
#'     \item{core}{Importance level: "Req" (Required), "Exp" (Expected),
#'       or "Perm" (Permissible)}
#'   }
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' sdtm_meta <- get_sdtm_metadata()
#' dm_vars <- sdtm_meta$DM
#' ae_vars <- sdtm_meta$AE
#'
#' # Use SDTM IG 3.3 metadata
#' sdtm_33 <- get_sdtm_metadata(version = "3.3")
#' }
get_sdtm_metadata <- function(version = "3.4") {
  version <- match.arg(version, choices = c("3.4", "3.3"))

  # Domains introduced in SDTM IG 3.4 (not in 3.3)
  v34_only_domains <- c("GF", "CP", "BE", "BS", "SM", "TD", "TM")

  all_domains <- .sdtm_all_domains()

  if (version == "3.3") {
    all_domains[v34_only_domains] <- NULL
  }

  return(all_domains)
}

#' Internal: Full SDTM Domain Definitions (IG 3.4 superset)
#' @keywords internal
#' @noRd
.sdtm_all_domains <- function() {
  list(
    DM = data.frame(
      variable = c(
        "STUDYID", "DOMAIN", "USUBJID", "SUBJID", "RFSTDTC", "RFENDTC",
        "SITEID", "AGE", "AGEU", "SEX", "RACE", "ETHNIC", "ARMCD", "ARM",
        "COUNTRY", "DMDTC", "DMDY", "BRTHDTC", "DTHDTC", "DTHFL",
        "ACTARMCD", "ACTARM", "RFXSTDTC", "RFXENDTC", "RFICDTC", "RFPENDTC"
      ),
      label = c(
        "Study Identifier",
        "Domain Abbreviation",
        "Unique Subject Identifier",
        "Subject Identifier for the Study",
        "Subject Reference Start Date/Time",
        "Subject Reference End Date/Time",
        "Study Site Identifier",
        "Age",
        "Age Units",
        "Sex",
        "Race",
        "Ethnicity",
        "Planned Arm Code",
        "Description of Planned Arm",
        "Country",
        "Date/Time of Collection",
        "Study Day of Collection",
        "Date/Time of Birth",
        "Date/Time of Death",
        "Subject Death Flag",
        "Actual Arm Code",
        "Description of Actual Arm",
        "Date/Time of First Study Treatment",
        "Date/Time of Last Study Treatment",
        "Date/Time of Informed Consent",
        "Date/Time of End of Participation"
      ),
      type = c(
        "Char", "Char", "Char", "Char", "Char", "Char",
        "Char", "Num", "Char", "Char", "Char", "Char", "Char", "Char",
        "Char", "Char", "Num", "Char", "Char", "Char",
        "Char", "Char", "Char", "Char", "Char", "Char"
      ),
      core = c(
        "Req", "Req", "Req", "Req", "Req", "Req",
        "Req", "Exp", "Exp", "Req", "Exp", "Exp", "Req", "Req",
        "Req", "Perm", "Perm", "Perm", "Perm", "Perm",
        "Req", "Req", "Exp", "Exp", "Perm", "Perm"
      ),
      stringsAsFactors = FALSE
    ),

    AE = data.frame(
      variable = c(
        "STUDYID", "DOMAIN", "USUBJID", "AESEQ", "AETERM", "AEDECOD",
        "AEBODSYS", "AESEV", "AESER", "AEACN", "AEREL", "AEOUT",
        "AESTDTC", "AEENDTC", "AESTDY", "AEENDY", "AESCAN", "AESCONG",
        "AESDISAB", "AESDTH", "AESHOSP", "AESLIFE", "AESMIE", "AECONTRT",
        "AETOXGR", "EPOCH", "AEENRF", "AESTRF"
      ),
      label = c(
        "Study Identifier",
        "Domain Abbreviation",
        "Unique Subject Identifier",
        "Adverse Event Sequence Number",
        "Reported Term for the Adverse Event",
        "Dictionary-Derived Term",
        "Body System or Organ Class",
        "Severity/Intensity",
        "Serious Event",
        "Action Taken with Study Treatment",
        "Causality",
        "Outcome of Adverse Event",
        "Start Date/Time of Adverse Event",
        "End Date/Time of Adverse Event",
        "Study Day of Start",
        "Study Day of End",
        "Involves Cancer",
        "Congenital Anomaly",
        "Persist or Signif Disability/Incapacity",
        "Results in Death",
        "Requires or Prolongs Hospitalization",
        "Is Life Threatening",
        "Other Medically Important Serious Event",
        "Concomitant or Additional Trtmnt Given",
        "Standard Toxicity Grade",
        "Epoch",
        "End Relative to Reference Period",
        "Start Relative to Reference Period"
      ),
      type = c(
        "Char", "Char", "Char", "Num", "Char", "Char",
        "Char", "Char", "Char", "Char", "Char", "Char",
        "Char", "Char", "Num", "Num", "Char", "Char",
        "Char", "Char", "Char", "Char", "Char", "Char",
        "Char", "Char", "Char", "Char"
      ),
      core = c(
        "Req", "Req", "Req", "Req", "Req", "Req",
        "Exp", "Exp", "Req", "Exp", "Exp", "Exp",
        "Exp", "Exp", "Perm", "Perm", "Perm", "Perm",
        "Perm", "Perm", "Perm", "Perm", "Perm", "Perm",
        "Perm", "Perm", "Perm", "Perm"
      ),
      stringsAsFactors = FALSE
    ),

    LB = data.frame(
      variable = c(
        "STUDYID", "DOMAIN", "USUBJID", "LBSEQ", "LBTESTCD", "LBTEST",
        "LBCAT", "LBORRES", "LBORRESU", "LBORNRLO", "LBORNRHI", "LBSTRESC",
        "LBSTRESN", "LBSTRESU", "LBSTNRLO", "LBSTNRHI", "LBNRIND", "LBSPEC",
        "LBMETHOD", "LBBLFL", "LBDTC", "LBDY", "VISITNUM", "VISIT",
        "EPOCH", "LBFAST", "LBTOX", "LBTOXGR"
      ),
      label = c(
        "Study Identifier",
        "Domain Abbreviation",
        "Unique Subject Identifier",
        "Laboratory Sequence Number",
        "Lab Test or Examination Short Name",
        "Lab Test or Examination Name",
        "Category for Lab Test",
        "Result or Finding in Original Units",
        "Original Units",
        "Reference Range Lower Limit-Orig Unit",
        "Reference Range Upper Limit-Orig Unit",
        "Character Result/Finding in Std Format",
        "Numeric Result/Finding in Standard Units",
        "Standard Units",
        "Reference Range Lower Limit-Std Units",
        "Reference Range Upper Limit-Std Units",
        "Reference Range Indicator",
        "Specimen Type",
        "Method of Test or Examination",
        "Baseline Flag",
        "Date/Time of Specimen Collection",
        "Study Day of Specimen Collection",
        "Visit Number",
        "Visit Name",
        "Epoch",
        "Fasting Status",
        "Toxicity",
        "Standard Toxicity Grade"
      ),
      type = c(
        "Char", "Char", "Char", "Num", "Char", "Char",
        "Char", "Char", "Char", "Char", "Char", "Char",
        "Num", "Char", "Num", "Num", "Char", "Char",
        "Char", "Char", "Char", "Num", "Num", "Char",
        "Char", "Char", "Char", "Char"
      ),
      core = c(
        "Req", "Req", "Req", "Req", "Req", "Req",
        "Exp", "Exp", "Exp", "Exp", "Exp", "Exp",
        "Exp", "Exp", "Exp", "Exp", "Exp", "Exp",
        "Perm", "Perm", "Exp", "Perm", "Exp", "Exp",
        "Perm", "Perm", "Perm", "Perm"
      ),
      stringsAsFactors = FALSE
    ),

    VS = data.frame(
      variable = c(
        "STUDYID", "DOMAIN", "USUBJID", "VSSEQ", "VSTESTCD", "VSTEST",
        "VSPOS", "VSORRES", "VSORRESU", "VSSTRESC", "VSSTRESN", "VSSTRESU",
        "VSBLFL", "VSDTC", "VSDY", "VISITNUM", "VISIT", "EPOCH"
      ),
      label = c(
        "Study Identifier",
        "Domain Abbreviation",
        "Unique Subject Identifier",
        "Vital Signs Sequence Number",
        "Vital Signs Test Short Name",
        "Vital Signs Test Name",
        "Vital Signs Position of Subject",
        "Result or Finding in Original Units",
        "Original Units",
        "Character Result/Finding in Std Format",
        "Numeric Result/Finding in Standard Units",
        "Standard Units",
        "Baseline Flag",
        "Date/Time of Measurements",
        "Study Day of Measurements",
        "Visit Number",
        "Visit Name",
        "Epoch"
      ),
      type = c(
        "Char", "Char", "Char", "Num", "Char", "Char",
        "Char", "Char", "Char", "Char", "Num", "Char",
        "Char", "Char", "Num", "Num", "Char", "Char"
      ),
      core = c(
        "Req", "Req", "Req", "Req", "Req", "Req",
        "Perm", "Exp", "Exp", "Exp", "Exp", "Exp",
        "Perm", "Exp", "Perm", "Exp", "Exp", "Perm"
      ),
      stringsAsFactors = FALSE
    ),

    EX = data.frame(
      variable = c(
        "STUDYID", "DOMAIN", "USUBJID", "EXSEQ", "EXTRT", "EXDOSE",
        "EXDOSU", "EXDOSFRM", "EXDOSFRQ", "EXROUTE", "EXSTDTC", "EXENDTC",
        "EXSTDY", "EXENDY", "EPOCH", "VISITNUM", "VISIT"
      ),
      label = c(
        "Study Identifier",
        "Domain Abbreviation",
        "Unique Subject Identifier",
        "Exposure Sequence Number",
        "Name of Treatment",
        "Dose per Administration",
        "Dose Units",
        "Dose Form",
        "Dosing Frequency per Interval",
        "Route of Administration",
        "Start Date/Time of Treatment",
        "End Date/Time of Treatment",
        "Study Day of Start of Treatment",
        "Study Day of End of Treatment",
        "Epoch",
        "Visit Number",
        "Visit Name"
      ),
      type = c(
        "Char", "Char", "Char", "Num", "Char", "Num",
        "Char", "Char", "Char", "Char", "Char", "Char",
        "Num", "Num", "Char", "Num", "Char"
      ),
      core = c(
        "Req", "Req", "Req", "Req", "Req", "Exp",
        "Exp", "Exp", "Perm", "Exp", "Exp", "Exp",
        "Perm", "Perm", "Perm", "Perm", "Perm"
      ),
      stringsAsFactors = FALSE
    ),

    CM = data.frame(
      variable = c(
        "STUDYID", "DOMAIN", "USUBJID", "CMSEQ", "CMTRT", "CMDECOD",
        "CMCAT", "CMDOSE", "CMDOSU", "CMDOSFRM", "CMROUTE", "CMSTDTC",
        "CMENDTC", "CMINDC", "CMCLAS", "CMCLASCD", "CMENRF", "CMSTRF", "EPOCH"
      ),
      label = c(
        "Study Identifier",
        "Domain Abbreviation",
        "Unique Subject Identifier",
        "Concomitant Medication Sequence Number",
        "Reported Name of Drug, Med, or Therapy",
        "Standardized Medication Name",
        "Category for Medication",
        "Dose per Administration",
        "Dose Units",
        "Dose Form",
        "Route of Administration",
        "Start Date/Time of Medication",
        "End Date/Time of Medication",
        "Indication",
        "Medication Class",
        "Medication Class Code",
        "End Relative to Reference Period",
        "Start Relative to Reference Period",
        "Epoch"
      ),
      type = c(
        "Char", "Char", "Char", "Num", "Char", "Char",
        "Char", "Num", "Char", "Char", "Char", "Char",
        "Char", "Char", "Char", "Char", "Char", "Char", "Char"
      ),
      core = c(
        "Req", "Req", "Req", "Req", "Req", "Exp",
        "Perm", "Perm", "Perm", "Perm", "Perm", "Exp",
        "Exp", "Perm", "Perm", "Perm", "Perm", "Perm", "Perm"
      ),
      stringsAsFactors = FALSE
    ),

    MH = data.frame(
      variable = c(
        "STUDYID", "DOMAIN", "USUBJID", "MHSEQ", "MHTERM", "MHDECOD",
        "MHBODSYS", "MHCAT", "MHSTDTC", "MHENDTC", "MHENRF", "MHSTRF"
      ),
      label = c(
        "Study Identifier",
        "Domain Abbreviation",
        "Unique Subject Identifier",
        "Medical History Sequence Number",
        "Reported Term for the Medical History",
        "Dictionary-Derived Term",
        "Body System or Organ Class",
        "Category for Medical History",
        "Start Date/Time of History",
        "End Date/Time of History",
        "End Relative to Reference Period",
        "Start Relative to Reference Period"
      ),
      type = c(
        "Char", "Char", "Char", "Num", "Char", "Char",
        "Char", "Char", "Char", "Char", "Char", "Char"
      ),
      core = c(
        "Req", "Req", "Req", "Req", "Req", "Exp",
        "Exp", "Perm", "Exp", "Perm", "Perm", "Perm"
      ),
      stringsAsFactors = FALSE
    ),

    DS = data.frame(
      variable = c(
        "STUDYID", "DOMAIN", "USUBJID", "DSSEQ", "DSTERM", "DSDECOD",
        "DSCAT", "DSSTDTC", "EPOCH", "DSSCAT"
      ),
      label = c(
        "Study Identifier",
        "Domain Abbreviation",
        "Unique Subject Identifier",
        "Disposition Sequence Number",
        "Reported Term for the Disposition Event",
        "Standardized Disposition Term",
        "Category for Disposition Event",
        "Start Date/Time of Disposition Event",
        "Epoch",
        "Subcategory for Disposition Event"
      ),
      type = c(
        "Char", "Char", "Char", "Num", "Char", "Char",
        "Char", "Char", "Char", "Char"
      ),
      core = c(
        "Req", "Req", "Req", "Req", "Req", "Req",
        "Exp", "Exp", "Perm", "Perm"
      ),
      stringsAsFactors = FALSE
    ),

    SV = data.frame(
      variable = c(
        "STUDYID", "DOMAIN", "USUBJID", "VISITNUM", "VISIT",
        "SVSTDTC", "SVENDTC", "EPOCH"
      ),
      label = c(
        "Study Identifier",
        "Domain Abbreviation",
        "Unique Subject Identifier",
        "Visit Number",
        "Visit Name",
        "Start Date/Time of Visit",
        "End Date/Time of Visit",
        "Epoch"
      ),
      type = c(
        "Char", "Char", "Char", "Num", "Char",
        "Char", "Char", "Char"
      ),
      core = c(
        "Req", "Req", "Req", "Req", "Exp",
        "Exp", "Exp", "Perm"
      ),
      stringsAsFactors = FALSE
    ),

    TA = data.frame(
      variable = c(
        "STUDYID", "DOMAIN", "ARMCD", "ARM", "TAESSION",
        "ETCD", "ELEMENT", "TABESSION", "TATRANS", "EPOCH"
      ),
      label = c(
        "Study Identifier",
        "Domain Abbreviation",
        "Coded Arm",
        "Description of Arm",
        "Planned Sequence Number within Arm",
        "Element Code",
        "Description of Element",
        "TABRANCH + Epoch",
        "Transition Rule",
        "Epoch"
      ),
      type = c(
        "Char", "Char", "Char", "Char", "Num",
        "Char", "Char", "Char", "Char", "Char"
      ),
      core = c(
        "Req", "Req", "Req", "Req", "Req",
        "Req", "Exp", "Perm", "Perm", "Req"
      ),
      stringsAsFactors = FALSE
    ),

    TE = data.frame(
      variable = c(
        "STUDYID", "DOMAIN", "ETCD", "ELEMENT", "TESTRL", "TEDUR"
      ),
      label = c(
        "Study Identifier",
        "Domain Abbreviation",
        "Element Code",
        "Description of Element",
        "Rule for End of Element",
        "Planned Duration of Element"
      ),
      type = c(
        "Char", "Char", "Char", "Char", "Char", "Char"
      ),
      core = c(
        "Req", "Req", "Req", "Req", "Perm", "Perm"
      ),
      stringsAsFactors = FALSE
    ),

    # --- Trial Design domains ---

    TI = data.frame(
      variable = c(
        "STUDYID", "DOMAIN", "IETESTCD", "IETEST", "IECAT",
        "IESCAT", "TIRL", "TIVERS"
      ),
      label = c(
        "Study Identifier",
        "Domain Abbreviation",
        "Incl/Excl Criterion Short Name",
        "Incl/Excl Criterion",
        "Incl/Excl Category",
        "Incl/Excl Subcategory",
        "Criterion Rule",
        "Protocol Criteria Versions"
      ),
      type = c(
        "Char", "Char", "Char", "Char", "Char",
        "Char", "Char", "Char"
      ),
      core = c(
        "Req", "Req", "Req", "Req", "Req",
        "Perm", "Perm", "Perm"
      ),
      stringsAsFactors = FALSE
    ),

    TS = data.frame(
      variable = c(
        "STUDYID", "DOMAIN", "TSSEQ", "TSGRPID", "TSPARMCD", "TSPARM",
        "TSVAL", "TSVALNF", "TSVALCD", "TSVCDREF", "TSVCDVER"
      ),
      label = c(
        "Study Identifier",
        "Domain Abbreviation",
        "Sequence Number",
        "Group ID",
        "Trial Summary Parameter Short Name",
        "Trial Summary Parameter",
        "Parameter Value",
        "Parameter Null Flavor",
        "Parameter Value Code",
        "Name of Reference Terminology",
        "Version of Reference Terminology"
      ),
      type = c(
        "Char", "Char", "Num", "Char", "Char", "Char",
        "Char", "Char", "Char", "Char", "Char"
      ),
      core = c(
        "Req", "Req", "Req", "Perm", "Req", "Req",
        "Req", "Perm", "Perm", "Perm", "Perm"
      ),
      stringsAsFactors = FALSE
    ),

    TV = data.frame(
      variable = c(
        "STUDYID", "DOMAIN", "VISITNUM", "VISIT", "TVSTRL",
        "TVCLRID", "ARMCD", "ARM", "EPOCH"
      ),
      label = c(
        "Study Identifier",
        "Domain Abbreviation",
        "Visit Number",
        "Visit Name",
        "Rule for Visit Start",
        "Trial Criteria ID",
        "Planned Arm Code",
        "Description of Planned Arm",
        "Epoch"
      ),
      type = c(
        "Char", "Char", "Num", "Char", "Char",
        "Char", "Char", "Char", "Char"
      ),
      core = c(
        "Req", "Req", "Req", "Req", "Perm",
        "Perm", "Req", "Req", "Perm"
      ),
      stringsAsFactors = FALSE
    ),

    # --- Special Purpose domains ---

    CO = data.frame(
      variable = c(
        "STUDYID", "DOMAIN", "RDOMAIN", "USUBJID", "COSEQ",
        "IDVAR", "IDVARVAL", "COREF", "CODTC", "COVAL", "COEVAL"
      ),
      label = c(
        "Study Identifier",
        "Domain Abbreviation",
        "Related Domain Abbreviation",
        "Unique Subject Identifier",
        "Sequence Number",
        "Identifying Variable",
        "Identifying Variable Value",
        "Comment Reference",
        "Date/Time of Comment",
        "Comment",
        "Evaluator"
      ),
      type = c(
        "Char", "Char", "Char", "Char", "Num",
        "Char", "Char", "Char", "Char", "Char", "Char"
      ),
      core = c(
        "Req", "Req", "Exp", "Exp", "Req",
        "Perm", "Perm", "Perm", "Perm", "Req", "Perm"
      ),
      stringsAsFactors = FALSE
    ),

    SE = data.frame(
      variable = c(
        "STUDYID", "DOMAIN", "USUBJID", "SESEQ", "ETCD",
        "ELEMENT", "SESTDTC", "SEENDTC", "TAESSION", "EPOCH",
        "SEUPDES"
      ),
      label = c(
        "Study Identifier",
        "Domain Abbreviation",
        "Unique Subject Identifier",
        "Sequence Number",
        "Element Code",
        "Description of Element",
        "Start Date/Time of Element",
        "End Date/Time of Element",
        "Planned Sequence Number within Arm",
        "Epoch",
        "Description of Unplanned Element"
      ),
      type = c(
        "Char", "Char", "Char", "Num", "Char",
        "Char", "Char", "Char", "Num", "Char",
        "Char"
      ),
      core = c(
        "Req", "Req", "Req", "Req", "Req",
        "Exp", "Exp", "Exp", "Perm", "Perm",
        "Perm"
      ),
      stringsAsFactors = FALSE
    ),

    # --- Events domains ---

    CE = data.frame(
      variable = c(
        "STUDYID", "DOMAIN", "USUBJID", "CESEQ", "CEGRPID",
        "CETERM", "CEDECOD", "CECAT", "CESCAT", "CEPRESP",
        "CEOCCUR", "CESTDTC", "CEENDTC", "CESTDY", "CEENDY",
        "EPOCH"
      ),
      label = c(
        "Study Identifier",
        "Domain Abbreviation",
        "Unique Subject Identifier",
        "Sequence Number",
        "Group ID",
        "Reported Term for Clinical Event",
        "Dictionary-Derived Term",
        "Category for Clinical Event",
        "Subcategory for Clinical Event",
        "Pre-Specified",
        "Occurrence",
        "Start Date/Time of Clinical Event",
        "End Date/Time of Clinical Event",
        "Study Day of Start",
        "Study Day of End",
        "Epoch"
      ),
      type = c(
        "Char", "Char", "Char", "Num", "Char",
        "Char", "Char", "Char", "Char", "Char",
        "Char", "Char", "Char", "Num", "Num",
        "Char"
      ),
      core = c(
        "Req", "Req", "Req", "Req", "Perm",
        "Req", "Exp", "Perm", "Perm", "Perm",
        "Perm", "Exp", "Exp", "Perm", "Perm",
        "Perm"
      ),
      stringsAsFactors = FALSE
    ),

    DV = data.frame(
      variable = c(
        "STUDYID", "DOMAIN", "USUBJID", "DVSEQ", "DVTERM",
        "DVDECOD", "DVCAT", "DVSCAT", "DVSTDTC", "DVENDTC",
        "DVSTDY", "DVENDY", "EPOCH"
      ),
      label = c(
        "Study Identifier",
        "Domain Abbreviation",
        "Unique Subject Identifier",
        "Sequence Number",
        "Protocol Deviation Term",
        "Dictionary-Derived Term",
        "Category for Protocol Deviation",
        "Subcategory for Protocol Deviation",
        "Start Date/Time of Deviation",
        "End Date/Time of Deviation",
        "Study Day of Start of Deviation",
        "Study Day of End of Deviation",
        "Epoch"
      ),
      type = c(
        "Char", "Char", "Char", "Num", "Char",
        "Char", "Char", "Char", "Char", "Char",
        "Num", "Num", "Char"
      ),
      core = c(
        "Req", "Req", "Req", "Req", "Req",
        "Perm", "Perm", "Perm", "Exp", "Perm",
        "Perm", "Perm", "Perm"
      ),
      stringsAsFactors = FALSE
    ),

    HO = data.frame(
      variable = c(
        "STUDYID", "DOMAIN", "USUBJID", "HOSEQ", "HOTERM",
        "HODECOD", "HOCAT", "HOSCAT", "HOPRESP", "HOOCCUR",
        "HOSTDTC", "HOENDTC", "HOSTDY", "HOENDY", "EPOCH"
      ),
      label = c(
        "Study Identifier",
        "Domain Abbreviation",
        "Unique Subject Identifier",
        "Sequence Number",
        "Reported Term for Healthcare Encounter",
        "Dictionary-Derived Term",
        "Category",
        "Subcategory",
        "Pre-Specified",
        "Occurrence",
        "Start Date/Time",
        "End Date/Time",
        "Study Day of Start",
        "Study Day of End",
        "Epoch"
      ),
      type = c(
        "Char", "Char", "Char", "Num", "Char",
        "Char", "Char", "Char", "Char", "Char",
        "Char", "Char", "Num", "Num", "Char"
      ),
      core = c(
        "Req", "Req", "Req", "Req", "Req",
        "Perm", "Perm", "Perm", "Perm", "Perm",
        "Exp", "Perm", "Perm", "Perm", "Perm"
      ),
      stringsAsFactors = FALSE
    ),

    # --- Interventions domains ---

    EC = data.frame(
      variable = c(
        "STUDYID", "DOMAIN", "USUBJID", "ECSEQ", "ECTRT",
        "ECDOSE", "ECDOSU", "ECDOSFRM", "ECDOSFRQ", "ECROUTE",
        "ECSTDTC", "ECENDTC", "ECSTDY", "ECENDY", "ECMOOD",
        "EPOCH", "VISITNUM", "VISIT"
      ),
      label = c(
        "Study Identifier",
        "Domain Abbreviation",
        "Unique Subject Identifier",
        "Sequence Number",
        "Name of Treatment",
        "Dose",
        "Dose Units",
        "Dose Form",
        "Dosing Frequency per Interval",
        "Route of Administration",
        "Start Date/Time of Treatment",
        "End Date/Time of Treatment",
        "Study Day of Start",
        "Study Day of End",
        "Record Mood",
        "Epoch",
        "Visit Number",
        "Visit Name"
      ),
      type = c(
        "Char", "Char", "Char", "Num", "Char",
        "Num", "Char", "Char", "Char", "Char",
        "Char", "Char", "Num", "Num", "Char",
        "Char", "Num", "Char"
      ),
      core = c(
        "Req", "Req", "Req", "Req", "Req",
        "Exp", "Exp", "Exp", "Perm", "Exp",
        "Exp", "Exp", "Perm", "Perm", "Perm",
        "Perm", "Perm", "Perm"
      ),
      stringsAsFactors = FALSE
    ),

    PR = data.frame(
      variable = c(
        "STUDYID", "DOMAIN", "USUBJID", "PRSEQ", "PRTRT",
        "PRDECOD", "PRCAT", "PRSCAT", "PRINDC", "PRSTDTC",
        "PRENDTC", "PRSTDY", "PRENDY", "EPOCH"
      ),
      label = c(
        "Study Identifier",
        "Domain Abbreviation",
        "Unique Subject Identifier",
        "Sequence Number",
        "Reported Name of Procedure",
        "Standardized Procedure Name",
        "Category for Procedure",
        "Subcategory for Procedure",
        "Procedure Indication",
        "Start Date/Time of Procedure",
        "End Date/Time of Procedure",
        "Study Day of Start",
        "Study Day of End",
        "Epoch"
      ),
      type = c(
        "Char", "Char", "Char", "Num", "Char",
        "Char", "Char", "Char", "Char", "Char",
        "Char", "Num", "Num", "Char"
      ),
      core = c(
        "Req", "Req", "Req", "Req", "Req",
        "Perm", "Perm", "Perm", "Perm", "Exp",
        "Perm", "Perm", "Perm", "Perm"
      ),
      stringsAsFactors = FALSE
    ),

    SU = data.frame(
      variable = c(
        "STUDYID", "DOMAIN", "USUBJID", "SUSEQ", "SUTRT",
        "SUDECOD", "SUCAT", "SUSCAT", "SUDOSE", "SUDOSU",
        "SUDOSFRQ", "SUROUTE", "SUSTDTC", "SUENDTC", "SUSTDY",
        "SUENDY", "SUENRF", "SUSTRF", "EPOCH"
      ),
      label = c(
        "Study Identifier",
        "Domain Abbreviation",
        "Unique Subject Identifier",
        "Sequence Number",
        "Reported Name of Substance",
        "Standardized Substance Name",
        "Category of Substance",
        "Subcategory of Substance",
        "Substance Use Consumption",
        "Consumption Units",
        "Use Frequency per Interval",
        "Route of Administration",
        "Start Date/Time",
        "End Date/Time",
        "Study Day of Start",
        "Study Day of End",
        "End Relative to Reference Period",
        "Start Relative to Reference Period",
        "Epoch"
      ),
      type = c(
        "Char", "Char", "Char", "Num", "Char",
        "Char", "Char", "Char", "Num", "Char",
        "Char", "Char", "Char", "Char", "Num",
        "Num", "Char", "Char", "Char"
      ),
      core = c(
        "Req", "Req", "Req", "Req", "Req",
        "Perm", "Perm", "Perm", "Perm", "Perm",
        "Perm", "Perm", "Exp", "Perm", "Perm",
        "Perm", "Perm", "Perm", "Perm"
      ),
      stringsAsFactors = FALSE
    ),

    # --- Findings domains ---

    DA = data.frame(
      variable = c(
        "STUDYID", "DOMAIN", "USUBJID", "DASEQ", "DATESTCD",
        "DATEST", "DACAT", "DASCAT", "DAORRES", "DAORRESU",
        "DASTRESC", "DASTRESN", "DASTRESU", "DADTC", "DADY",
        "EPOCH"
      ),
      label = c(
        "Study Identifier",
        "Domain Abbreviation",
        "Unique Subject Identifier",
        "Sequence Number",
        "Drug Accountability Test Short Name",
        "Drug Accountability Test Name",
        "Category",
        "Subcategory",
        "Result or Finding in Original Units",
        "Original Units",
        "Character Result/Finding in Std Format",
        "Numeric Result/Finding in Std Units",
        "Standard Units",
        "Date/Time of Collection",
        "Study Day of Collection",
        "Epoch"
      ),
      type = c(
        "Char", "Char", "Char", "Num", "Char",
        "Char", "Char", "Char", "Char", "Char",
        "Char", "Num", "Char", "Char", "Num",
        "Char"
      ),
      core = c(
        "Req", "Req", "Req", "Req", "Req",
        "Req", "Perm", "Perm", "Exp", "Exp",
        "Exp", "Exp", "Exp", "Exp", "Perm",
        "Perm"
      ),
      stringsAsFactors = FALSE
    ),

    DD = data.frame(
      variable = c(
        "STUDYID", "DOMAIN", "USUBJID", "DDSEQ", "DDTESTCD",
        "DDTEST", "DDORRES", "DDORRESU", "DDSTRESC", "DDSTRESN",
        "DDSTRESU", "DDDTC", "DDDY"
      ),
      label = c(
        "Study Identifier",
        "Domain Abbreviation",
        "Unique Subject Identifier",
        "Sequence Number",
        "Death Detail Assessment Short Name",
        "Death Detail Assessment Name",
        "Result or Finding in Original Units",
        "Original Units",
        "Character Result/Finding in Std Format",
        "Numeric Result/Finding in Std Units",
        "Standard Units",
        "Date/Time of Assessment",
        "Study Day of Assessment"
      ),
      type = c(
        "Char", "Char", "Char", "Num", "Char",
        "Char", "Char", "Char", "Char", "Num",
        "Char", "Char", "Num"
      ),
      core = c(
        "Req", "Req", "Req", "Req", "Req",
        "Req", "Exp", "Perm", "Exp", "Perm",
        "Perm", "Exp", "Perm"
      ),
      stringsAsFactors = FALSE
    ),

    EG = data.frame(
      variable = c(
        "STUDYID", "DOMAIN", "USUBJID", "EGSEQ", "EGTESTCD",
        "EGTEST", "EGCAT", "EGPOS", "EGORRES", "EGORRESU",
        "EGSTRESC", "EGSTRESN", "EGSTRESU", "EGBLFL", "EGDTC",
        "EGDY", "EGTPT", "EGTPTNUM", "EGELTM", "VISITNUM",
        "VISIT", "EPOCH"
      ),
      label = c(
        "Study Identifier",
        "Domain Abbreviation",
        "Unique Subject Identifier",
        "ECG Sequence Number",
        "ECG Test Short Name",
        "ECG Test Name",
        "Category for ECG",
        "ECG Position of Subject",
        "Result or Finding in Original Units",
        "Original Units",
        "Character Result/Finding in Std Format",
        "Numeric Result/Finding in Std Units",
        "Standard Units",
        "Baseline Flag",
        "Date/Time of ECG",
        "Study Day of ECG",
        "Planned Time Point Name",
        "Planned Time Point Number",
        "Planned Elapsed Time from Time Point Ref",
        "Visit Number",
        "Visit Name",
        "Epoch"
      ),
      type = c(
        "Char", "Char", "Char", "Num", "Char",
        "Char", "Char", "Char", "Char", "Char",
        "Char", "Num", "Char", "Char", "Char",
        "Num", "Char", "Num", "Char", "Num",
        "Char", "Char"
      ),
      core = c(
        "Req", "Req", "Req", "Req", "Req",
        "Req", "Perm", "Perm", "Exp", "Exp",
        "Exp", "Exp", "Exp", "Perm", "Exp",
        "Perm", "Perm", "Perm", "Perm", "Exp",
        "Exp", "Perm"
      ),
      stringsAsFactors = FALSE
    ),

    FA = data.frame(
      variable = c(
        "STUDYID", "DOMAIN", "USUBJID", "FASEQ", "FATESTCD",
        "FATEST", "FACAT", "FASCAT", "FAOBJ", "FAORRES",
        "FAORRESU", "FASTRESC", "FASTRESN", "FASTRESU", "FADTC",
        "FADY", "EPOCH", "FALNKID"
      ),
      label = c(
        "Study Identifier",
        "Domain Abbreviation",
        "Unique Subject Identifier",
        "Sequence Number",
        "Findings About Test Short Name",
        "Findings About Test Name",
        "Category",
        "Subcategory",
        "Object of the Observation",
        "Result or Finding in Original Units",
        "Original Units",
        "Character Result/Finding in Std Format",
        "Numeric Result/Finding in Std Units",
        "Standard Units",
        "Date/Time of Collection",
        "Study Day of Collection",
        "Epoch",
        "Link ID"
      ),
      type = c(
        "Char", "Char", "Char", "Num", "Char",
        "Char", "Char", "Char", "Char", "Char",
        "Char", "Char", "Num", "Char", "Char",
        "Num", "Char", "Char"
      ),
      core = c(
        "Req", "Req", "Req", "Req", "Req",
        "Req", "Perm", "Perm", "Req", "Exp",
        "Perm", "Exp", "Perm", "Perm", "Exp",
        "Perm", "Perm", "Perm"
      ),
      stringsAsFactors = FALSE
    ),

    IE = data.frame(
      variable = c(
        "STUDYID", "DOMAIN", "USUBJID", "IESEQ", "IETESTCD",
        "IETEST", "IECAT", "IESCAT", "IEORRES", "IESTRESC",
        "IEDTC", "IEDY", "EPOCH"
      ),
      label = c(
        "Study Identifier",
        "Domain Abbreviation",
        "Unique Subject Identifier",
        "Sequence Number",
        "Incl/Excl Criterion Short Name",
        "Incl/Excl Criterion",
        "Incl/Excl Category",
        "Incl/Excl Subcategory",
        "Incl/Excl Criterion Original Result",
        "Char Result/Finding in Std Format",
        "Date/Time of Assessment",
        "Study Day of Assessment",
        "Epoch"
      ),
      type = c(
        "Char", "Char", "Char", "Num", "Char",
        "Char", "Char", "Char", "Char", "Char",
        "Char", "Num", "Char"
      ),
      core = c(
        "Req", "Req", "Req", "Req", "Req",
        "Req", "Req", "Perm", "Exp", "Exp",
        "Exp", "Perm", "Perm"
      ),
      stringsAsFactors = FALSE
    ),

    MB = data.frame(
      variable = c(
        "STUDYID", "DOMAIN", "USUBJID", "MBSEQ", "MBTESTCD",
        "MBTEST", "MBCAT", "MBSCAT", "MBORRES", "MBORRESU",
        "MBSTRESC", "MBSTRESN", "MBSTRESU", "MBSPEC", "MBMETHOD",
        "MBBLFL", "MBDTC", "MBDY", "VISITNUM", "VISIT", "EPOCH"
      ),
      label = c(
        "Study Identifier",
        "Domain Abbreviation",
        "Unique Subject Identifier",
        "Sequence Number",
        "Microbiology Test Short Name",
        "Microbiology Test Name",
        "Category",
        "Subcategory",
        "Result in Original Units",
        "Original Units",
        "Character Result in Std Format",
        "Numeric Result in Std Units",
        "Standard Units",
        "Specimen Type",
        "Method of Test",
        "Baseline Flag",
        "Date/Time of Collection",
        "Study Day of Collection",
        "Visit Number",
        "Visit Name",
        "Epoch"
      ),
      type = c(
        "Char", "Char", "Char", "Num", "Char",
        "Char", "Char", "Char", "Char", "Char",
        "Char", "Num", "Char", "Char", "Char",
        "Char", "Char", "Num", "Num", "Char", "Char"
      ),
      core = c(
        "Req", "Req", "Req", "Req", "Req",
        "Req", "Perm", "Perm", "Exp", "Exp",
        "Exp", "Perm", "Perm", "Exp", "Perm",
        "Perm", "Exp", "Perm", "Exp", "Exp", "Perm"
      ),
      stringsAsFactors = FALSE
    ),

    MI = data.frame(
      variable = c(
        "STUDYID", "DOMAIN", "USUBJID", "MISEQ", "MITESTCD",
        "MITEST", "MICAT", "MISCAT", "MIORRES", "MIORRESU",
        "MISTRESC", "MISTRESN", "MISTRESU", "MISPEC", "MIMETHOD",
        "MIDTC", "MIDY", "EPOCH"
      ),
      label = c(
        "Study Identifier",
        "Domain Abbreviation",
        "Unique Subject Identifier",
        "Sequence Number",
        "Microscopic Test Short Name",
        "Microscopic Test Name",
        "Category",
        "Subcategory",
        "Result in Original Units",
        "Original Units",
        "Character Result in Std Format",
        "Numeric Result in Std Units",
        "Standard Units",
        "Specimen Type",
        "Method",
        "Date/Time of Collection",
        "Study Day of Collection",
        "Epoch"
      ),
      type = c(
        "Char", "Char", "Char", "Num", "Char",
        "Char", "Char", "Char", "Char", "Char",
        "Char", "Num", "Char", "Char", "Char",
        "Char", "Num", "Char"
      ),
      core = c(
        "Req", "Req", "Req", "Req", "Req",
        "Req", "Perm", "Perm", "Exp", "Perm",
        "Exp", "Perm", "Perm", "Exp", "Perm",
        "Exp", "Perm", "Perm"
      ),
      stringsAsFactors = FALSE
    ),

    MS = data.frame(
      variable = c(
        "STUDYID", "DOMAIN", "USUBJID", "MSSEQ", "MSTESTCD",
        "MSTEST", "MSCAT", "MSORRES", "MSORRESU", "MSSTRESC",
        "MSSTRESN", "MSSTRESU", "MSSPEC", "MSDTC", "MSDY",
        "EPOCH"
      ),
      label = c(
        "Study Identifier",
        "Domain Abbreviation",
        "Unique Subject Identifier",
        "Sequence Number",
        "Susceptibility Test Short Name",
        "Susceptibility Test Name",
        "Category",
        "Result in Original Units",
        "Original Units",
        "Character Result in Std Format",
        "Numeric Result in Std Units",
        "Standard Units",
        "Specimen Type",
        "Date/Time of Collection",
        "Study Day of Collection",
        "Epoch"
      ),
      type = c(
        "Char", "Char", "Char", "Num", "Char",
        "Char", "Char", "Char", "Char", "Char",
        "Num", "Char", "Char", "Char", "Num",
        "Char"
      ),
      core = c(
        "Req", "Req", "Req", "Req", "Req",
        "Req", "Perm", "Exp", "Exp", "Exp",
        "Perm", "Perm", "Exp", "Exp", "Perm",
        "Perm"
      ),
      stringsAsFactors = FALSE
    ),

    PC = data.frame(
      variable = c(
        "STUDYID", "DOMAIN", "USUBJID", "PCSEQ", "PCTESTCD",
        "PCTEST", "PCCAT", "PCORRES", "PCORRESU", "PCSTRESC",
        "PCSTRESN", "PCSTRESU", "PCSPEC", "PCMETHOD", "PCBLFL",
        "PCDTC", "PCDY", "PCTPT", "PCTPTNUM", "PCELTM",
        "VISITNUM", "VISIT", "EPOCH"
      ),
      label = c(
        "Study Identifier",
        "Domain Abbreviation",
        "Unique Subject Identifier",
        "Sequence Number",
        "PK Concentration Test Short Name",
        "PK Concentration Test Name",
        "Category",
        "Result in Original Units",
        "Original Units",
        "Character Result in Std Format",
        "Numeric Result in Std Units",
        "Standard Units",
        "Specimen Type",
        "Method",
        "Baseline Flag",
        "Date/Time of Specimen Collection",
        "Study Day of Collection",
        "Planned Time Point Name",
        "Planned Time Point Number",
        "Planned Elapsed Time from Time Point Ref",
        "Visit Number",
        "Visit Name",
        "Epoch"
      ),
      type = c(
        "Char", "Char", "Char", "Num", "Char",
        "Char", "Char", "Char", "Char", "Char",
        "Num", "Char", "Char", "Char", "Char",
        "Char", "Num", "Char", "Num", "Char",
        "Num", "Char", "Char"
      ),
      core = c(
        "Req", "Req", "Req", "Req", "Req",
        "Req", "Perm", "Exp", "Exp", "Exp",
        "Exp", "Exp", "Exp", "Perm", "Perm",
        "Exp", "Perm", "Perm", "Perm", "Perm",
        "Exp", "Exp", "Perm"
      ),
      stringsAsFactors = FALSE
    ),

    PE = data.frame(
      variable = c(
        "STUDYID", "DOMAIN", "USUBJID", "PESEQ", "PETESTCD",
        "PETEST", "PECAT", "PEBODSYS", "PEORRES", "PEORRESU",
        "PESTRESC", "PESTRESN", "PESTRESU", "PELOC", "PEDTC",
        "PEDY", "VISITNUM", "VISIT", "EPOCH"
      ),
      label = c(
        "Study Identifier",
        "Domain Abbreviation",
        "Unique Subject Identifier",
        "Sequence Number",
        "Physical Examination Short Name",
        "Physical Examination Test Name",
        "Category",
        "Body System or Organ Class",
        "Result or Finding in Original Units",
        "Original Units",
        "Character Result/Finding in Std Format",
        "Numeric Result/Finding in Std Units",
        "Standard Units",
        "Location Used for the Measurement",
        "Date/Time of Examination",
        "Study Day of Examination",
        "Visit Number",
        "Visit Name",
        "Epoch"
      ),
      type = c(
        "Char", "Char", "Char", "Num", "Char",
        "Char", "Char", "Char", "Char", "Char",
        "Char", "Num", "Char", "Char", "Char",
        "Num", "Num", "Char", "Char"
      ),
      core = c(
        "Req", "Req", "Req", "Req", "Req",
        "Req", "Perm", "Perm", "Exp", "Perm",
        "Exp", "Perm", "Perm", "Perm", "Exp",
        "Perm", "Exp", "Exp", "Perm"
      ),
      stringsAsFactors = FALSE
    ),

    PP = data.frame(
      variable = c(
        "STUDYID", "DOMAIN", "USUBJID", "PPSEQ", "PPGRPID",
        "PPTESTCD", "PPTEST", "PPCAT", "PPSCAT", "PPORRES",
        "PPORRESU", "PPSTRESC", "PPSTRESN", "PPSTRESU", "PPDTC",
        "PPDY", "PPSPEC", "PPRFTDTC", "VISITNUM", "VISIT", "EPOCH"
      ),
      label = c(
        "Study Identifier",
        "Domain Abbreviation",
        "Unique Subject Identifier",
        "Sequence Number",
        "Group ID",
        "PK Parameter Short Name",
        "PK Parameter Name",
        "Category",
        "Subcategory",
        "Result in Original Units",
        "Original Units",
        "Character Result in Std Format",
        "Numeric Result in Std Units",
        "Standard Units",
        "Date/Time",
        "Study Day",
        "Specimen Type",
        "Date/Time of Reference Point",
        "Visit Number",
        "Visit Name",
        "Epoch"
      ),
      type = c(
        "Char", "Char", "Char", "Num", "Char",
        "Char", "Char", "Char", "Char", "Char",
        "Char", "Char", "Num", "Char", "Char",
        "Num", "Char", "Char", "Num", "Char", "Char"
      ),
      core = c(
        "Req", "Req", "Req", "Req", "Perm",
        "Req", "Req", "Perm", "Perm", "Exp",
        "Exp", "Exp", "Exp", "Exp", "Perm",
        "Perm", "Perm", "Perm", "Perm", "Perm", "Perm"
      ),
      stringsAsFactors = FALSE
    ),

    QS = data.frame(
      variable = c(
        "STUDYID", "DOMAIN", "USUBJID", "QSSEQ", "QSTESTCD",
        "QSTEST", "QSCAT", "QSSCAT", "QSORRES", "QSORRESU",
        "QSSTRESC", "QSSTRESN", "QSSTRESU", "QSBLFL", "QSDTC",
        "QSDY", "QSTPT", "QSTPTNUM", "VISITNUM", "VISIT", "EPOCH",
        "QSEVAL", "QSEVLINT"
      ),
      label = c(
        "Study Identifier",
        "Domain Abbreviation",
        "Unique Subject Identifier",
        "Sequence Number",
        "Questionnaire Test Short Name",
        "Questionnaire Test Name",
        "Category of Questionnaire",
        "Subcategory of Questionnaire",
        "Result or Finding in Original Units",
        "Original Units",
        "Character Result/Finding in Std Format",
        "Numeric Result/Finding in Std Units",
        "Standard Units",
        "Baseline Flag",
        "Date/Time of Finding",
        "Study Day of Finding",
        "Planned Time Point Name",
        "Planned Time Point Number",
        "Visit Number",
        "Visit Name",
        "Epoch",
        "Evaluator",
        "Evaluation Interval"
      ),
      type = c(
        "Char", "Char", "Char", "Num", "Char",
        "Char", "Char", "Char", "Char", "Char",
        "Char", "Num", "Char", "Char", "Char",
        "Num", "Char", "Num", "Num", "Char", "Char",
        "Char", "Char"
      ),
      core = c(
        "Req", "Req", "Req", "Req", "Req",
        "Req", "Exp", "Perm", "Exp", "Perm",
        "Exp", "Perm", "Perm", "Perm", "Exp",
        "Perm", "Perm", "Perm", "Exp", "Exp", "Perm",
        "Perm", "Perm"
      ),
      stringsAsFactors = FALSE
    ),

    RS = data.frame(
      variable = c(
        "STUDYID", "DOMAIN", "USUBJID", "RSSEQ", "RSGRPID",
        "RSTESTCD", "RSTEST", "RSCAT", "RSSCAT", "RSORRES",
        "RSSTRESC", "RSEVAL", "RSDTC", "RSDY", "VISITNUM",
        "VISIT", "EPOCH"
      ),
      label = c(
        "Study Identifier",
        "Domain Abbreviation",
        "Unique Subject Identifier",
        "Sequence Number",
        "Group ID",
        "Disease Response Short Name",
        "Disease Response Test Name",
        "Category",
        "Subcategory",
        "Result or Finding in Original Units",
        "Character Result in Std Format",
        "Evaluator",
        "Date/Time of Assessment",
        "Study Day of Assessment",
        "Visit Number",
        "Visit Name",
        "Epoch"
      ),
      type = c(
        "Char", "Char", "Char", "Num", "Char",
        "Char", "Char", "Char", "Char", "Char",
        "Char", "Char", "Char", "Num", "Num",
        "Char", "Char"
      ),
      core = c(
        "Req", "Req", "Req", "Req", "Perm",
        "Req", "Req", "Exp", "Perm", "Exp",
        "Exp", "Perm", "Exp", "Perm", "Exp",
        "Exp", "Perm"
      ),
      stringsAsFactors = FALSE
    ),

    SC = data.frame(
      variable = c(
        "STUDYID", "DOMAIN", "USUBJID", "SCSEQ", "SCTESTCD",
        "SCTEST", "SCCAT", "SCSCAT", "SCORRES", "SCORRESU",
        "SCSTRESC", "SCSTRESN", "SCSTRESU", "SCDTC", "SCDY"
      ),
      label = c(
        "Study Identifier",
        "Domain Abbreviation",
        "Unique Subject Identifier",
        "Sequence Number",
        "Subject Characteristic Short Name",
        "Subject Characteristic",
        "Category",
        "Subcategory",
        "Result or Finding in Original Units",
        "Original Units",
        "Character Result in Std Format",
        "Numeric Result in Std Units",
        "Standard Units",
        "Date/Time of Collection",
        "Study Day of Collection"
      ),
      type = c(
        "Char", "Char", "Char", "Num", "Char",
        "Char", "Char", "Char", "Char", "Char",
        "Char", "Num", "Char", "Char", "Num"
      ),
      core = c(
        "Req", "Req", "Req", "Req", "Req",
        "Req", "Perm", "Perm", "Exp", "Perm",
        "Exp", "Perm", "Perm", "Exp", "Perm"
      ),
      stringsAsFactors = FALSE
    ),

    SS = data.frame(
      variable = c(
        "STUDYID", "DOMAIN", "USUBJID", "SSSEQ", "SSTESTCD",
        "SSTEST", "SSCAT", "SSSCAT", "SSORRES", "SSSTRESC",
        "SSDTC", "SSDY", "EPOCH"
      ),
      label = c(
        "Study Identifier",
        "Domain Abbreviation",
        "Unique Subject Identifier",
        "Sequence Number",
        "Subject Status Short Name",
        "Subject Status",
        "Category",
        "Subcategory",
        "Result or Finding in Original Units",
        "Character Result in Std Format",
        "Date/Time of Assessment",
        "Study Day of Assessment",
        "Epoch"
      ),
      type = c(
        "Char", "Char", "Char", "Num", "Char",
        "Char", "Char", "Char", "Char", "Char",
        "Char", "Num", "Char"
      ),
      core = c(
        "Req", "Req", "Req", "Req", "Req",
        "Req", "Perm", "Perm", "Exp", "Exp",
        "Exp", "Perm", "Perm"
      ),
      stringsAsFactors = FALSE
    ),

    TR = data.frame(
      variable = c(
        "STUDYID", "DOMAIN", "USUBJID", "TRSEQ", "TRGRPID",
        "TRLNKID", "TRTESTCD", "TRTEST", "TRORRES", "TRORRESU",
        "TRSTRESC", "TRSTRESN", "TRSTRESU", "TRMETHOD", "TREVAL",
        "TRDTC", "TRDY", "VISITNUM", "VISIT", "EPOCH"
      ),
      label = c(
        "Study Identifier",
        "Domain Abbreviation",
        "Unique Subject Identifier",
        "Sequence Number",
        "Group ID",
        "Link ID",
        "Tumor/Lesion Assessment Short Name",
        "Tumor/Lesion Assessment Test Name",
        "Result or Finding in Original Units",
        "Original Units",
        "Character Result in Std Format",
        "Numeric Result in Std Units",
        "Standard Units",
        "Method of Assessment",
        "Evaluator",
        "Date/Time of Assessment",
        "Study Day of Assessment",
        "Visit Number",
        "Visit Name",
        "Epoch"
      ),
      type = c(
        "Char", "Char", "Char", "Num", "Char",
        "Char", "Char", "Char", "Char", "Char",
        "Char", "Num", "Char", "Char", "Char",
        "Char", "Num", "Num", "Char", "Char"
      ),
      core = c(
        "Req", "Req", "Req", "Req", "Perm",
        "Exp", "Req", "Req", "Exp", "Exp",
        "Exp", "Perm", "Perm", "Perm", "Perm",
        "Exp", "Perm", "Exp", "Exp", "Perm"
      ),
      stringsAsFactors = FALSE
    ),

    TU = data.frame(
      variable = c(
        "STUDYID", "DOMAIN", "USUBJID", "TUSEQ", "TUGRPID",
        "TULNKID", "TUTESTCD", "TUTEST", "TUORRES", "TUSTRESC",
        "TULOC", "TULAT", "TUMETHOD", "TUEVAL", "TUDTC",
        "TUDY", "EPOCH"
      ),
      label = c(
        "Study Identifier",
        "Domain Abbreviation",
        "Unique Subject Identifier",
        "Sequence Number",
        "Group ID",
        "Link ID",
        "Tumor Identification Short Name",
        "Tumor Identification Test Name",
        "Result or Finding in Original Units",
        "Character Result in Std Format",
        "Location of Tumor/Lesion",
        "Laterality",
        "Method of Identification",
        "Evaluator",
        "Date/Time of Assessment",
        "Study Day of Assessment",
        "Epoch"
      ),
      type = c(
        "Char", "Char", "Char", "Num", "Char",
        "Char", "Char", "Char", "Char", "Char",
        "Char", "Char", "Char", "Char", "Char",
        "Num", "Char"
      ),
      core = c(
        "Req", "Req", "Req", "Req", "Perm",
        "Exp", "Req", "Req", "Exp", "Exp",
        "Exp", "Perm", "Perm", "Perm", "Exp",
        "Perm", "Perm"
      ),
      stringsAsFactors = FALSE
    ),

    # --- SDTM IG 3.4 new domains ---

    AG = data.frame(
      variable = c(
        "STUDYID", "DOMAIN", "USUBJID", "AGSEQ", "AGTRT",
        "AGDECOD", "AGCAT", "AGDOSE", "AGDOSU", "AGDOSFRM",
        "AGROUTE", "AGSTDTC", "AGENDTC", "AGSTDY", "AGENDY",
        "AGLNKID", "EPOCH"
      ),
      label = c(
        "Study Identifier", "Domain Abbreviation",
        "Unique Subject Identifier", "Sequence Number",
        "Reported Agent Name", "Standardized Agent Name",
        "Category", "Agent Dose", "Agent Dose Units",
        "Agent Dose Form", "Route of Administration",
        "Start Date/Time", "End Date/Time",
        "Study Day of Start", "Study Day of End",
        "Link ID", "Epoch"
      ),
      type = c(
        "Char", "Char", "Char", "Num", "Char",
        "Char", "Char", "Num", "Char", "Char",
        "Char", "Char", "Char", "Num", "Num",
        "Char", "Char"
      ),
      core = c(
        "Req", "Req", "Req", "Req", "Req",
        "Perm", "Perm", "Exp", "Exp", "Exp",
        "Exp", "Exp", "Perm", "Perm", "Perm",
        "Perm", "Perm"
      ),
      stringsAsFactors = FALSE
    ),

    BE = data.frame(
      variable = c(
        "STUDYID", "DOMAIN", "USUBJID", "BESEQ", "BETERM",
        "BEDECOD", "BECAT", "BESCAT", "BESTDTC", "BEENDTC",
        "BEDY", "EPOCH"
      ),
      label = c(
        "Study Identifier", "Domain Abbreviation",
        "Unique Subject Identifier", "Sequence Number",
        "Reported Term for Biospecimen Event",
        "Dictionary-Derived Term", "Category", "Subcategory",
        "Start Date/Time", "End Date/Time",
        "Study Day", "Epoch"
      ),
      type = c(
        "Char", "Char", "Char", "Num", "Char",
        "Char", "Char", "Char", "Char", "Char",
        "Num", "Char"
      ),
      core = c(
        "Req", "Req", "Req", "Req", "Req",
        "Perm", "Perm", "Perm", "Exp", "Perm",
        "Perm", "Perm"
      ),
      stringsAsFactors = FALSE
    ),

    BS = data.frame(
      variable = c(
        "STUDYID", "DOMAIN", "USUBJID", "BSSEQ", "BSTESTCD",
        "BSTEST", "BSCAT", "BSORRES", "BSORRESU", "BSSTRESC",
        "BSSTRESN", "BSSTRESU", "BSSPEC", "BSMETHOD", "BSDTC",
        "BSDY", "EPOCH"
      ),
      label = c(
        "Study Identifier", "Domain Abbreviation",
        "Unique Subject Identifier", "Sequence Number",
        "Biospecimen Test Short Name", "Biospecimen Test Name",
        "Category", "Result in Original Units", "Original Units",
        "Character Result in Std Format",
        "Numeric Result in Std Units", "Standard Units",
        "Specimen Type", "Method", "Date/Time of Collection",
        "Study Day of Collection", "Epoch"
      ),
      type = c(
        "Char", "Char", "Char", "Num", "Char",
        "Char", "Char", "Char", "Char", "Char",
        "Num", "Char", "Char", "Char", "Char",
        "Num", "Char"
      ),
      core = c(
        "Req", "Req", "Req", "Req", "Req",
        "Req", "Perm", "Exp", "Exp", "Exp",
        "Perm", "Perm", "Exp", "Perm", "Exp",
        "Perm", "Perm"
      ),
      stringsAsFactors = FALSE
    ),

    CP = data.frame(
      variable = c(
        "STUDYID", "DOMAIN", "USUBJID", "CPSEQ", "CPTESTCD",
        "CPTEST", "CPCAT", "CPSCAT", "CPORRES", "CPORRESU",
        "CPSTRESC", "CPSTRESN", "CPSTRESU", "CPSPEC", "CPMETHOD",
        "CPDTC", "CPDY", "EPOCH"
      ),
      label = c(
        "Study Identifier", "Domain Abbreviation",
        "Unique Subject Identifier", "Sequence Number",
        "Cell Phenotype Test Short Name",
        "Cell Phenotype Test Name", "Category", "Subcategory",
        "Result in Original Units", "Original Units",
        "Character Result in Std Format",
        "Numeric Result in Std Units", "Standard Units",
        "Specimen Type", "Method", "Date/Time of Collection",
        "Study Day of Collection", "Epoch"
      ),
      type = c(
        "Char", "Char", "Char", "Num", "Char",
        "Char", "Char", "Char", "Char", "Char",
        "Char", "Num", "Char", "Char", "Char",
        "Char", "Num", "Char"
      ),
      core = c(
        "Req", "Req", "Req", "Req", "Req",
        "Req", "Perm", "Perm", "Exp", "Exp",
        "Exp", "Perm", "Perm", "Exp", "Perm",
        "Exp", "Perm", "Perm"
      ),
      stringsAsFactors = FALSE
    ),

    GF = data.frame(
      variable = c(
        "STUDYID", "DOMAIN", "USUBJID", "GFSEQ", "GFTESTCD",
        "GFTEST", "GFCAT", "GFSCAT", "GFORRES", "GFORRESU",
        "GFSTRESC", "GFSTRESN", "GFSTRESU", "GFSPEC", "GFMETHOD",
        "GFDTC", "GFDY", "EPOCH"
      ),
      label = c(
        "Study Identifier", "Domain Abbreviation",
        "Unique Subject Identifier", "Sequence Number",
        "Genomics Findings Test Short Name",
        "Genomics Findings Test Name", "Category", "Subcategory",
        "Result in Original Units", "Original Units",
        "Character Result in Std Format",
        "Numeric Result in Std Units", "Standard Units",
        "Specimen Type", "Method", "Date/Time of Collection",
        "Study Day of Collection", "Epoch"
      ),
      type = c(
        "Char", "Char", "Char", "Num", "Char",
        "Char", "Char", "Char", "Char", "Char",
        "Char", "Num", "Char", "Char", "Char",
        "Char", "Num", "Char"
      ),
      core = c(
        "Req", "Req", "Req", "Req", "Req",
        "Req", "Perm", "Perm", "Exp", "Exp",
        "Exp", "Perm", "Perm", "Exp", "Perm",
        "Exp", "Perm", "Perm"
      ),
      stringsAsFactors = FALSE
    ),

    IS = data.frame(
      variable = c(
        "STUDYID", "DOMAIN", "USUBJID", "ISSEQ", "ISTESTCD",
        "ISTEST", "ISCAT", "ISORRES", "ISORRESU", "ISSTRESC",
        "ISSTRESN", "ISSTRESU", "ISSPEC", "ISMETHOD", "ISBLFL",
        "ISDTC", "ISDY", "VISITNUM", "VISIT", "EPOCH"
      ),
      label = c(
        "Study Identifier", "Domain Abbreviation",
        "Unique Subject Identifier", "Sequence Number",
        "Immunogenicity Test Short Name",
        "Immunogenicity Test Name", "Category",
        "Result in Original Units", "Original Units",
        "Character Result in Std Format",
        "Numeric Result in Std Units", "Standard Units",
        "Specimen Type", "Method", "Baseline Flag",
        "Date/Time of Collection", "Study Day of Collection",
        "Visit Number", "Visit Name", "Epoch"
      ),
      type = c(
        "Char", "Char", "Char", "Num", "Char",
        "Char", "Char", "Char", "Char", "Char",
        "Num", "Char", "Char", "Char", "Char",
        "Char", "Num", "Num", "Char", "Char"
      ),
      core = c(
        "Req", "Req", "Req", "Req", "Req",
        "Req", "Perm", "Exp", "Exp", "Exp",
        "Perm", "Perm", "Exp", "Perm", "Perm",
        "Exp", "Perm", "Exp", "Exp", "Perm"
      ),
      stringsAsFactors = FALSE
    ),

    ML = data.frame(
      variable = c(
        "STUDYID", "DOMAIN", "USUBJID", "MLSEQ", "MLTRT",
        "MLDECOD", "MLCAT", "MLSTDTC", "MLENDTC", "MLSTDY",
        "MLENDY", "EPOCH"
      ),
      label = c(
        "Study Identifier", "Domain Abbreviation",
        "Unique Subject Identifier", "Sequence Number",
        "Reported Name of Meal", "Standardized Meal Name",
        "Category", "Start Date/Time", "End Date/Time",
        "Study Day of Start", "Study Day of End", "Epoch"
      ),
      type = c(
        "Char", "Char", "Char", "Num", "Char",
        "Char", "Char", "Char", "Char", "Num",
        "Num", "Char"
      ),
      core = c(
        "Req", "Req", "Req", "Req", "Req",
        "Perm", "Perm", "Exp", "Perm", "Perm",
        "Perm", "Perm"
      ),
      stringsAsFactors = FALSE
    ),

    SM = data.frame(
      variable = c(
        "STUDYID", "DOMAIN", "USUBJID", "SMSEQ", "SMTESTCD",
        "SMTEST", "SMORRES", "SMSTRESC", "SMDTC", "SMDY",
        "EPOCH"
      ),
      label = c(
        "Study Identifier", "Domain Abbreviation",
        "Unique Subject Identifier", "Sequence Number",
        "Disease Milestone Short Name", "Disease Milestone Name",
        "Result in Original Units", "Character Result in Std Format",
        "Date/Time", "Study Day", "Epoch"
      ),
      type = c(
        "Char", "Char", "Char", "Num", "Char",
        "Char", "Char", "Char", "Char", "Num",
        "Char"
      ),
      core = c(
        "Req", "Req", "Req", "Req", "Req",
        "Req", "Exp", "Exp", "Exp", "Perm",
        "Perm"
      ),
      stringsAsFactors = FALSE
    ),

    TD = data.frame(
      variable = c(
        "STUDYID", "DOMAIN", "TDORDER", "TDANCVAR", "TDSTOFF",
        "TDTGTPAI", "TDMINPAI", "TDMAXPAI"
      ),
      label = c(
        "Study Identifier", "Domain Abbreviation",
        "Order of Disease Assessment", "Anchoring Variable",
        "Start Offset", "Target Number Per Assessment Interval",
        "Min Number Per Assessment Interval",
        "Max Number Per Assessment Interval"
      ),
      type = c(
        "Char", "Char", "Num", "Char", "Char",
        "Num", "Num", "Num"
      ),
      core = c(
        "Req", "Req", "Req", "Req", "Perm",
        "Perm", "Perm", "Perm"
      ),
      stringsAsFactors = FALSE
    ),

    TM = data.frame(
      variable = c(
        "STUDYID", "DOMAIN", "MIDSTYPE", "TMDEF", "TMRPT"
      ),
      label = c(
        "Study Identifier", "Domain Abbreviation",
        "Disease Milestone Type", "Disease Milestone Definition",
        "Disease Milestone Repetition Indicator"
      ),
      type = c("Char", "Char", "Char", "Char", "Char"),
      core = c("Req", "Req", "Req", "Req", "Perm"),
      stringsAsFactors = FALSE
    ),

    # --- Supplemental Qualifiers (generic template for all SUPP-- domains) ---

    SUPPQUAL = data.frame(
      variable = c(
        "STUDYID", "RDOMAIN", "USUBJID", "IDVAR", "IDVARVAL",
        "QNAM", "QLABEL", "QVAL", "QORIG", "QEVAL"
      ),
      label = c(
        "Study Identifier",
        "Related Domain Abbreviation",
        "Unique Subject Identifier",
        "Identifying Variable",
        "Identifying Variable Value",
        "Qualifier Variable Name",
        "Qualifier Variable Label",
        "Data Value",
        "Origin",
        "Evaluator"
      ),
      type = c(
        "Char", "Char", "Char", "Char", "Char",
        "Char", "Char", "Char", "Char", "Char"
      ),
      core = c(
        "Req", "Req", "Req", "Req", "Req",
        "Req", "Req", "Req", "Req", "Perm"
      ),
      stringsAsFactors = FALSE
    ),

    # --- Relationship dataset ---

    RELREC = data.frame(
      variable = c(
        "STUDYID", "RDOMAIN", "USUBJID", "IDVAR", "IDVARVAL",
        "RELTYPE", "RELID"
      ),
      label = c(
        "Study Identifier", "Related Domain Abbreviation",
        "Unique Subject Identifier", "Identifying Variable",
        "Identifying Variable Value", "Relationship Type",
        "Relationship Identifier"
      ),
      type = c(
        "Char", "Char", "Char", "Char", "Char",
        "Char", "Char"
      ),
      core = c(
        "Req", "Req", "Perm", "Req", "Req",
        "Req", "Req"
      ),
      stringsAsFactors = FALSE
    )
  )
}

#' ADaM Metadata
#'
#' Returns metadata for ADaM datasets following CDISC standards.
#' Provides information about required, conditional, and other variables
#' for each ADaM analysis dataset.
#'
#' Variable definitions are based on the published CDISC ADaM Implementation
#' Guide. The canonical machine-readable source is the CDISC Library API
#' (\url{https://www.cdisc.org/cdisc-library}), which requires CDISC
#' membership. The metadata shipped with clinCompare is hand-curated from the
#' published IG specifications.
#'
#' @param version Character string specifying the ADaM IG version.
#'   Supported values: "1.3" (default), "1.2", "1.1".
#'   All versions currently return the same variable definitions;
#'   the version is recorded for provenance tracking.
#'
#' @return A named list where keys are ADaM dataset names and values are
#'   data.frames with columns:
#'   \describe{
#'     \item{variable}{Variable name (character)}
#'     \item{label}{Variable label/description (character)}
#'     \item{type}{Data type: "Char" for character or "Num" for numeric}
#'     \item{core}{Importance level: "Req" (Required), "Cond" (Conditional)}
#'   }
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' adam_meta <- get_adam_metadata()
#' adsl_vars <- adam_meta$ADSL
#' adae_vars <- adam_meta$ADAE
#' }
get_adam_metadata <- function(version = "1.3") {
  version <- match.arg(version, choices = c("1.3", "1.2", "1.1"))
  list(
    ADSL = data.frame(
      variable = c(
        "STUDYID", "USUBJID", "SUBJID", "SITEID", "AGE", "AGEU", "AGEGR1",
        "AGEGR1N", "SEX", "RACE", "ETHNIC", "COUNTRY", "ARM", "ARMCD",
        "ACTARM", "ACTARMCD", "TRT01P", "TRT01PN", "TRT01A", "TRT01AN",
        "TRTSDT", "TRTEDT", "RFSTDTC", "RFENDTC", "SAFFL", "ITTFL", "EFFFL",
        "COMP8FL", "COMP16FL", "COMP24FL", "RANDFL", "RANDDT", "DTHDT",
        "DTHFL", "DTHDTC"
      ),
      label = c(
        "Study Identifier",
        "Unique Subject Identifier",
        "Subject Identifier",
        "Study Site Identifier",
        "Age",
        "Age Units",
        "Pooled Age Group 1",
        "Pooled Age Group 1 (N)",
        "Sex",
        "Race",
        "Ethnicity",
        "Country",
        "Description of Planned Arm",
        "Planned Arm Code",
        "Description of Actual Arm",
        "Actual Arm Code",
        "Planned Treatment for Period 01",
        "Planned Treatment for Period 01 (N)",
        "Actual Treatment for Period 01",
        "Actual Treatment for Period 01 (N)",
        "Date of First Exposure to Treatment",
        "Date of Last Exposure to Treatment",
        "Subject Reference Start Date/Time",
        "Subject Reference End Date/Time",
        "Safety Population Flag",
        "Intent-To-Treat Population Flag",
        "Efficacy Population Flag",
        "Completers of Week 8 Population Flag",
        "Completers of Week 16 Population Flag",
        "Completers of Week 24 Population Flag",
        "Randomized Population Flag",
        "Date of Randomization",
        "Date of Death",
        "Subject Death Flag",
        "Date/Time of Death"
      ),
      type = c(
        "Char", "Char", "Char", "Char", "Num", "Char", "Char",
        "Num", "Char", "Char", "Char", "Char", "Char", "Char",
        "Char", "Char", "Char", "Num", "Char", "Num",
        "Num", "Num", "Char", "Char", "Char", "Char", "Char",
        "Char", "Char", "Char", "Char", "Num", "Num",
        "Char", "Char"
      ),
      core = c(
        "Req", "Req", "Req", "Req", "Req", "Req", "Cond",
        "Cond", "Req", "Req", "Cond", "Cond", "Req", "Req",
        "Req", "Req", "Req", "Req", "Req", "Req",
        "Req", "Req", "Req", "Req", "Req", "Req", "Cond",
        "Cond", "Cond", "Cond", "Cond", "Cond", "Cond",
        "Cond", "Cond"
      ),
      stringsAsFactors = FALSE
    ),

    ADAE = data.frame(
      variable = c(
        "STUDYID", "USUBJID", "TRTA", "TRTAN", "AGE", "AGEGR1", "SEX", "RACE",
        "SAFFL", "AEBODSYS", "AEDECOD", "AETERM", "AESEV", "AESER", "AEREL",
        "AEACN", "AEOUT", "ASTDT", "AENDT", "ASTDTF", "AENDTF", "AESTDTC",
        "AEENDTC", "TRTEMFL", "AOCCFL", "AOCCSFL", "AOCCPFL", "CQ01NAM",
        "AOESSION"
      ),
      label = c(
        "Study Identifier",
        "Unique Subject Identifier",
        "Actual Treatment",
        "Actual Treatment (N)",
        "Age",
        "Pooled Age Group 1",
        "Sex",
        "Race",
        "Safety Population Flag",
        "Body System or Organ Class",
        "Dictionary-Derived Term",
        "Reported Term",
        "Severity/Intensity",
        "Serious Event",
        "Causality",
        "Action Taken",
        "Outcome",
        "Analysis Start Date",
        "Analysis End Date",
        "Analysis Start Date Imputation Flag",
        "Analysis End Date Imputation Flag",
        "Start Date/Time",
        "End Date/Time",
        "Treatment Emergent Analysis Flag",
        "1st Occurrence within Subject Flag",
        "1st Occurrence of SOC within Subject",
        "1st Occurrence of PT within Subject",
        "Customized Query 01 Name",
        "Occurrence with Subject Sequence"
      ),
      type = c(
        "Char", "Char", "Char", "Num", "Num", "Char", "Char", "Char",
        "Char", "Char", "Char", "Char", "Char", "Char", "Char",
        "Char", "Char", "Num", "Num", "Char", "Char", "Char",
        "Char", "Char", "Char", "Char", "Char", "Char",
        "Num"
      ),
      core = c(
        "Req", "Req", "Req", "Req", "Req", "Cond", "Req", "Req",
        "Req", "Req", "Req", "Req", "Cond", "Req", "Cond",
        "Cond", "Cond", "Req", "Cond", "Cond", "Cond", "Cond",
        "Cond", "Req", "Cond", "Cond", "Cond", "Cond",
        "Cond"
      ),
      stringsAsFactors = FALSE
    ),

    ADLB = data.frame(
      variable = c(
        "STUDYID", "USUBJID", "TRTA", "TRTAN", "PARAMCD", "PARAM", "PARAMN",
        "PARCAT1", "AVAL", "AVALC", "BASE", "BASEC", "CHG", "PCHG",
        "ABLFL", "ANRLO", "ANRHI", "A1LO", "A1HI", "ANRIND", "AVISIT",
        "AVISITN", "ADT", "ADY", "ATPT", "ATPTN", "DTYPE", "SAFFL",
        "ONTRTFL", "LBSEQ", "LBTESTCD"
      ),
      label = c(
        "Study Identifier",
        "Unique Subject Identifier",
        "Actual Treatment",
        "Actual Treatment (N)",
        "Parameter Code",
        "Parameter",
        "Parameter (N)",
        "Parameter Category 1",
        "Analysis Value",
        "Analysis Value (C)",
        "Baseline Value",
        "Baseline Value (C)",
        "Change from Baseline",
        "Percent Change from Baseline",
        "Baseline Record Flag",
        "Analysis Normal Range Lower Limit",
        "Analysis Normal Range Upper Limit",
        "Analysis Range 1 Lower Limit",
        "Analysis Range 1 Upper Limit",
        "Analysis Reference Range Indicator",
        "Analysis Visit",
        "Analysis Visit (N)",
        "Analysis Date",
        "Analysis Relative Day",
        "Analysis Timepoint",
        "Analysis Timepoint (N)",
        "Derivation Type",
        "Safety Population Flag",
        "On Treatment Record Flag",
        "Sequence Number",
        "Lab Test Code"
      ),
      type = c(
        "Char", "Char", "Char", "Num", "Char", "Char", "Num",
        "Char", "Num", "Char", "Num", "Char", "Num", "Num",
        "Char", "Num", "Num", "Num", "Num", "Char", "Char",
        "Num", "Num", "Num", "Char", "Num", "Char", "Char",
        "Char", "Num", "Char"
      ),
      core = c(
        "Req", "Req", "Req", "Req", "Req", "Req", "Cond",
        "Cond", "Req", "Cond", "Req", "Cond", "Cond", "Cond",
        "Req", "Cond", "Cond", "Cond", "Cond", "Cond", "Req",
        "Req", "Req", "Cond", "Cond", "Cond", "Cond", "Req",
        "Cond", "Cond", "Cond"
      ),
      stringsAsFactors = FALSE
    ),

    ADTTE = data.frame(
      variable = c(
        "STUDYID", "USUBJID", "PARAMCD", "PARAM", "PARAMN", "AVAL", "CNSR",
        "EVNTDESC", "CNSDTDSC", "STARTDT", "ADT", "SRCDOM", "SRCVAR",
        "SRCSEQ", "TRTA", "TRTAN", "AGE", "AGEGR1", "SEX", "RACE",
        "SAFFL", "ITTFL"
      ),
      label = c(
        "Study Identifier",
        "Unique Subject Identifier",
        "Parameter Code",
        "Parameter",
        "Parameter (N)",
        "Analysis Value",
        "Censor",
        "Event Description",
        "Censor Date Description",
        "Time-to-Event Origin Date",
        "Analysis Date",
        "Source Domain",
        "Source Variable",
        "Source Sequence Number",
        "Actual Treatment",
        "Actual Treatment (N)",
        "Age",
        "Pooled Age Group 1",
        "Sex",
        "Race",
        "Safety Population Flag",
        "Intent-To-Treat Population Flag"
      ),
      type = c(
        "Char", "Char", "Char", "Char", "Num", "Num", "Num",
        "Char", "Char", "Num", "Num", "Char", "Char",
        "Num", "Char", "Num", "Num", "Char", "Char", "Char",
        "Char", "Char"
      ),
      core = c(
        "Req", "Req", "Req", "Req", "Cond", "Req", "Req",
        "Cond", "Cond", "Req", "Req", "Cond", "Cond",
        "Cond", "Cond", "Cond", "Cond", "Cond", "Cond", "Cond",
        "Cond", "Cond"
      ),
      stringsAsFactors = FALSE
    ),

    ADEFF = data.frame(
      variable = c(
        "STUDYID", "USUBJID", "PARAMCD", "PARAM", "PARAMN", "AVAL", "AVALC",
        "BASE", "CHG", "PCHG", "ABLFL", "AVISIT", "AVISITN", "ADT", "ADY",
        "TRTA", "TRTAN", "SAFFL", "ITTFL", "EFFFL", "DTYPE", "ANL01FL"
      ),
      label = c(
        "Study Identifier",
        "Unique Subject Identifier",
        "Parameter Code",
        "Parameter",
        "Parameter (N)",
        "Analysis Value",
        "Analysis Value (C)",
        "Baseline Value",
        "Change from Baseline",
        "Percent Change from Baseline",
        "Baseline Record Flag",
        "Analysis Visit",
        "Analysis Visit (N)",
        "Analysis Date",
        "Analysis Relative Day",
        "Actual Treatment",
        "Actual Treatment (N)",
        "Safety Population Flag",
        "Intent-To-Treat Population Flag",
        "Efficacy Population Flag",
        "Derivation Type",
        "Analysis Flag 01"
      ),
      type = c(
        "Char", "Char", "Char", "Char", "Num", "Num", "Char",
        "Num", "Num", "Num", "Char", "Char", "Num", "Num", "Num",
        "Char", "Num", "Char", "Char", "Char", "Char", "Char"
      ),
      core = c(
        "Req", "Req", "Req", "Req", "Cond", "Req", "Cond",
        "Cond", "Cond", "Cond", "Req", "Req", "Req", "Req", "Cond",
        "Req", "Req", "Cond", "Cond", "Cond", "Cond", "Cond"
      ),
      stringsAsFactors = FALSE
    ),

    ADCM = data.frame(
      variable = c(
        "STUDYID", "USUBJID", "TRTA", "TRTAN", "CMTRT", "CMDECOD",
        "CMCAT", "CMINDC", "CMCLAS", "CMCLASCD", "ASTDT", "AENDT",
        "ASTDTF", "AENDTF", "ASTDY", "AENDY", "ADURN", "ADURU",
        "SAFFL", "ONTRTFL", "CMSEQ", "AGE", "SEX", "RACE"
      ),
      label = c(
        "Study Identifier",
        "Unique Subject Identifier",
        "Actual Treatment",
        "Actual Treatment (N)",
        "Reported Name of Drug",
        "Standardized Medication Name",
        "Category for Medication",
        "Indication",
        "Medication Class",
        "Medication Class Code",
        "Analysis Start Date",
        "Analysis End Date",
        "Analysis Start Date Imputation Flag",
        "Analysis End Date Imputation Flag",
        "Analysis Start Relative Day",
        "Analysis End Relative Day",
        "Analysis Duration (N)",
        "Analysis Duration Units",
        "Safety Population Flag",
        "On Treatment Record Flag",
        "Sequence Number",
        "Age",
        "Sex",
        "Race"
      ),
      type = c(
        "Char", "Char", "Char", "Num", "Char", "Char",
        "Char", "Char", "Char", "Char", "Num", "Num",
        "Char", "Char", "Num", "Num", "Num", "Char",
        "Char", "Char", "Num", "Num", "Char", "Char"
      ),
      core = c(
        "Req", "Req", "Req", "Req", "Req", "Cond",
        "Cond", "Cond", "Cond", "Cond", "Req", "Cond",
        "Cond", "Cond", "Cond", "Cond", "Cond", "Cond",
        "Req", "Cond", "Cond", "Cond", "Cond", "Cond"
      ),
      stringsAsFactors = FALSE
    ),

    ADVS = data.frame(
      variable = c(
        "STUDYID", "USUBJID", "TRTA", "TRTAN", "PARAMCD", "PARAM",
        "PARAMN", "PARCAT1", "AVAL", "AVALC", "BASE", "CHG", "PCHG",
        "ABLFL", "ANRLO", "ANRHI", "ANRIND", "AVISIT", "AVISITN",
        "ADT", "ADY", "ATPT", "ATPTN", "DTYPE", "SAFFL", "ONTRTFL",
        "ANL01FL", "VSSEQ", "VSTESTCD"
      ),
      label = c(
        "Study Identifier",
        "Unique Subject Identifier",
        "Actual Treatment",
        "Actual Treatment (N)",
        "Parameter Code",
        "Parameter",
        "Parameter (N)",
        "Parameter Category 1",
        "Analysis Value",
        "Analysis Value (C)",
        "Baseline Value",
        "Change from Baseline",
        "Percent Change from Baseline",
        "Baseline Record Flag",
        "Analysis Normal Range Lower Limit",
        "Analysis Normal Range Upper Limit",
        "Analysis Reference Range Indicator",
        "Analysis Visit",
        "Analysis Visit (N)",
        "Analysis Date",
        "Analysis Relative Day",
        "Analysis Timepoint",
        "Analysis Timepoint (N)",
        "Derivation Type",
        "Safety Population Flag",
        "On Treatment Record Flag",
        "Analysis Flag 01",
        "Sequence Number",
        "Vital Signs Test Code"
      ),
      type = c(
        "Char", "Char", "Char", "Num", "Char", "Char",
        "Num", "Char", "Num", "Char", "Num", "Num", "Num",
        "Char", "Num", "Num", "Char", "Char", "Num",
        "Num", "Num", "Char", "Num", "Char", "Char", "Char",
        "Char", "Num", "Char"
      ),
      core = c(
        "Req", "Req", "Req", "Req", "Req", "Req",
        "Cond", "Cond", "Req", "Cond", "Cond", "Cond", "Cond",
        "Req", "Cond", "Cond", "Cond", "Req", "Req",
        "Req", "Cond", "Cond", "Cond", "Cond", "Req", "Cond",
        "Cond", "Cond", "Cond"
      ),
      stringsAsFactors = FALSE
    ),

    ADEG = data.frame(
      variable = c(
        "STUDYID", "USUBJID", "TRTA", "TRTAN", "PARAMCD", "PARAM",
        "PARAMN", "PARCAT1", "AVAL", "AVALC", "BASE", "CHG", "PCHG",
        "ABLFL", "ANRLO", "ANRHI", "ANRIND", "AVISIT", "AVISITN",
        "ADT", "ADY", "ATPT", "ATPTN", "DTYPE", "SAFFL", "ONTRTFL",
        "EGSEQ", "EGTESTCD"
      ),
      label = c(
        "Study Identifier",
        "Unique Subject Identifier",
        "Actual Treatment",
        "Actual Treatment (N)",
        "Parameter Code",
        "Parameter",
        "Parameter (N)",
        "Parameter Category 1",
        "Analysis Value",
        "Analysis Value (C)",
        "Baseline Value",
        "Change from Baseline",
        "Percent Change from Baseline",
        "Baseline Record Flag",
        "Analysis Normal Range Lower Limit",
        "Analysis Normal Range Upper Limit",
        "Analysis Reference Range Indicator",
        "Analysis Visit",
        "Analysis Visit (N)",
        "Analysis Date",
        "Analysis Relative Day",
        "Analysis Timepoint",
        "Analysis Timepoint (N)",
        "Derivation Type",
        "Safety Population Flag",
        "On Treatment Record Flag",
        "Sequence Number",
        "ECG Test Code"
      ),
      type = c(
        "Char", "Char", "Char", "Num", "Char", "Char",
        "Num", "Char", "Num", "Char", "Num", "Num", "Num",
        "Char", "Num", "Num", "Char", "Char", "Num",
        "Num", "Num", "Char", "Num", "Char", "Char", "Char",
        "Num", "Char"
      ),
      core = c(
        "Req", "Req", "Req", "Req", "Req", "Req",
        "Cond", "Cond", "Req", "Cond", "Cond", "Cond", "Cond",
        "Req", "Cond", "Cond", "Cond", "Req", "Req",
        "Req", "Cond", "Cond", "Cond", "Cond", "Req", "Cond",
        "Cond", "Cond"
      ),
      stringsAsFactors = FALSE
    ),

    ADMH = data.frame(
      variable = c(
        "STUDYID", "USUBJID", "TRTA", "TRTAN", "MHTERM", "MHDECOD",
        "MHBODSYS", "MHCAT", "MHSCAT", "ASTDT", "AENDT", "ASTDTF",
        "AENDTF", "AREL", "SAFFL", "MHSEQ", "AGE", "SEX", "RACE"
      ),
      label = c(
        "Study Identifier",
        "Unique Subject Identifier",
        "Actual Treatment",
        "Actual Treatment (N)",
        "Reported Term for Medical History",
        "Dictionary-Derived Term",
        "Body System or Organ Class",
        "Category",
        "Subcategory",
        "Analysis Start Date",
        "Analysis End Date",
        "Analysis Start Date Imputation Flag",
        "Analysis End Date Imputation Flag",
        "Analysis Relatedness to Treatment",
        "Safety Population Flag",
        "Sequence Number",
        "Age",
        "Sex",
        "Race"
      ),
      type = c(
        "Char", "Char", "Char", "Num", "Char", "Char",
        "Char", "Char", "Char", "Num", "Num", "Char",
        "Char", "Char", "Char", "Num", "Num", "Char", "Char"
      ),
      core = c(
        "Req", "Req", "Req", "Req", "Req", "Cond",
        "Cond", "Cond", "Cond", "Cond", "Cond", "Cond",
        "Cond", "Cond", "Req", "Cond", "Cond", "Cond", "Cond"
      ),
      stringsAsFactors = FALSE
    ),

    ADEX = data.frame(
      variable = c(
        "STUDYID", "USUBJID", "TRTA", "TRTAN", "PARAMCD", "PARAM",
        "PARAMN", "PARCAT1", "AVAL", "AVALC", "ASTDT", "AENDT",
        "ASTDY", "AENDY", "ADURN", "ADURU", "ABLFL", "AVISIT",
        "AVISITN", "ADT", "SAFFL", "EXSEQ", "EXDOSE", "EXDOSU"
      ),
      label = c(
        "Study Identifier",
        "Unique Subject Identifier",
        "Actual Treatment",
        "Actual Treatment (N)",
        "Parameter Code",
        "Parameter",
        "Parameter (N)",
        "Parameter Category 1",
        "Analysis Value",
        "Analysis Value (C)",
        "Analysis Start Date",
        "Analysis End Date",
        "Analysis Start Relative Day",
        "Analysis End Relative Day",
        "Analysis Duration (N)",
        "Analysis Duration Units",
        "Baseline Record Flag",
        "Analysis Visit",
        "Analysis Visit (N)",
        "Analysis Date",
        "Safety Population Flag",
        "Sequence Number",
        "Dose per Administration",
        "Dose Units"
      ),
      type = c(
        "Char", "Char", "Char", "Num", "Char", "Char",
        "Num", "Char", "Num", "Char", "Num", "Num",
        "Num", "Num", "Num", "Char", "Char", "Char",
        "Num", "Num", "Char", "Num", "Num", "Char"
      ),
      core = c(
        "Req", "Req", "Req", "Req", "Req", "Req",
        "Cond", "Cond", "Req", "Cond", "Cond", "Cond",
        "Cond", "Cond", "Cond", "Cond", "Cond", "Cond",
        "Cond", "Cond", "Req", "Cond", "Cond", "Cond"
      ),
      stringsAsFactors = FALSE
    ),

    ADPC = data.frame(
      variable = c(
        "STUDYID", "USUBJID", "TRTA", "TRTAN", "PARAMCD", "PARAM",
        "PARAMN", "PARCAT1", "AVAL", "AVALC", "AVALU", "ABLFL",
        "AVISIT", "AVISITN", "ADT", "ATM", "ADTM", "ATPT", "ATPTN",
        "ARRLT", "NRRLT", "PCTPT", "PCTPTNUM", "PCSPEC", "SAFFL",
        "PKFL", "PPFL", "DTYPE", "AFRLT", "NFRLT"
      ),
      label = c(
        "Study Identifier",
        "Unique Subject Identifier",
        "Actual Treatment",
        "Actual Treatment (N)",
        "Parameter Code",
        "Parameter",
        "Parameter (N)",
        "Parameter Category 1",
        "Analysis Value",
        "Analysis Value (C)",
        "Analysis Value Unit",
        "Baseline Record Flag",
        "Analysis Visit",
        "Analysis Visit (N)",
        "Analysis Date",
        "Analysis Time",
        "Analysis Datetime",
        "Analysis Timepoint",
        "Analysis Timepoint (N)",
        "Actual Relative Time from Ref Dose",
        "Nominal Relative Time from Ref Dose",
        "Planned Time Point",
        "Planned Time Point (N)",
        "Specimen Material Type",
        "Safety Population Flag",
        "PK Concentration Flag",
        "PK Parameter Flag",
        "Derivation Type",
        "Actual Relative Time from First Dose",
        "Nominal Relative Time from First Dose"
      ),
      type = c(
        "Char", "Char", "Char", "Num", "Char", "Char",
        "Num", "Char", "Num", "Char", "Char", "Char",
        "Char", "Num", "Num", "Char", "Num", "Char", "Num",
        "Num", "Num", "Char", "Num", "Char", "Char",
        "Char", "Char", "Char", "Num", "Num"
      ),
      core = c(
        "Req", "Req", "Req", "Req", "Req", "Req",
        "Cond", "Cond", "Req", "Cond", "Cond", "Cond",
        "Cond", "Cond", "Cond", "Cond", "Cond", "Cond", "Cond",
        "Cond", "Cond", "Cond", "Cond", "Cond", "Req",
        "Cond", "Cond", "Cond", "Cond", "Cond"
      ),
      stringsAsFactors = FALSE
    ),

    ADPP = data.frame(
      variable = c(
        "STUDYID", "USUBJID", "TRTA", "TRTAN", "PARAMCD", "PARAM",
        "PARAMN", "PARCAT1", "AVAL", "AVALC", "AVALU", "ABLFL",
        "AVISIT", "AVISITN", "ADT", "SAFFL", "PKFL", "PPFL",
        "PPSPEC", "PPCAT", "PPRFDTC", "DTYPE"
      ),
      label = c(
        "Study Identifier",
        "Unique Subject Identifier",
        "Actual Treatment",
        "Actual Treatment (N)",
        "Parameter Code",
        "Parameter",
        "Parameter (N)",
        "Parameter Category 1",
        "Analysis Value",
        "Analysis Value (C)",
        "Analysis Value Unit",
        "Baseline Record Flag",
        "Analysis Visit",
        "Analysis Visit (N)",
        "Analysis Date",
        "Safety Population Flag",
        "PK Concentration Flag",
        "PK Parameter Flag",
        "Specimen Material Type",
        "Parameter Category",
        "Reference Dose Date/Time",
        "Derivation Type"
      ),
      type = c(
        "Char", "Char", "Char", "Num", "Char", "Char",
        "Num", "Char", "Num", "Char", "Char", "Char",
        "Char", "Num", "Num", "Char", "Char", "Char",
        "Char", "Char", "Char", "Char"
      ),
      core = c(
        "Req", "Req", "Req", "Req", "Req", "Req",
        "Cond", "Cond", "Req", "Cond", "Cond", "Cond",
        "Cond", "Cond", "Cond", "Req", "Cond", "Cond",
        "Cond", "Cond", "Cond", "Cond"
      ),
      stringsAsFactors = FALSE
    ),

    ADRS = data.frame(
      variable = c(
        "STUDYID", "USUBJID", "TRTA", "TRTAN", "PARAMCD", "PARAM",
        "PARAMN", "PARCAT1", "AVAL", "AVALC", "ABLFL", "BASE",
        "BASEC", "CHG", "PCHG", "AVISIT", "AVISITN", "ADT",
        "ADTF", "AENDT", "AENDTF", "DTYPE", "ANL01FL", "CRIT1",
        "CRIT1FL", "SAFFL", "ITTFL", "EFFFL", "RSSEQ"
      ),
      label = c(
        "Study Identifier",
        "Unique Subject Identifier",
        "Actual Treatment",
        "Actual Treatment (N)",
        "Parameter Code",
        "Parameter",
        "Parameter (N)",
        "Parameter Category 1",
        "Analysis Value",
        "Analysis Value (C)",
        "Baseline Record Flag",
        "Baseline Value",
        "Baseline Value (C)",
        "Change from Baseline",
        "Percent Change from Baseline",
        "Analysis Visit",
        "Analysis Visit (N)",
        "Analysis Date",
        "Analysis Date Imputation Flag",
        "Analysis End Date",
        "Analysis End Date Imputation Flag",
        "Derivation Type",
        "Analysis Record Flag 01",
        "Analysis Criterion 1",
        "Criterion 1 Evaluation Result Flag",
        "Safety Population Flag",
        "Intent-to-Treat Population Flag",
        "Efficacy Population Flag",
        "Sequence Number"
      ),
      type = c(
        "Char", "Char", "Char", "Num", "Char", "Char",
        "Num", "Char", "Num", "Char", "Char", "Num",
        "Char", "Num", "Num", "Char", "Num", "Num",
        "Char", "Num", "Char", "Char", "Char", "Char",
        "Char", "Char", "Char", "Char", "Num"
      ),
      core = c(
        "Req", "Req", "Req", "Req", "Req", "Req",
        "Cond", "Cond", "Req", "Cond", "Cond", "Cond",
        "Cond", "Cond", "Cond", "Cond", "Cond", "Cond",
        "Cond", "Cond", "Cond", "Cond", "Cond", "Cond",
        "Cond", "Req", "Req", "Cond", "Cond"
      ),
      stringsAsFactors = FALSE
    ),

    ADTR = data.frame(
      variable = c(
        "STUDYID", "USUBJID", "TRTA", "TRTAN", "PARAMCD", "PARAM",
        "PARAMN", "PARCAT1", "AVAL", "AVALC", "ABLFL", "BASE",
        "CHG", "PCHG", "AVISIT", "AVISITN", "ADT", "ADTF",
        "DTYPE", "ANL01FL", "SAFFL", "ITTFL", "EFFFL",
        "TRGRPID", "TRLNKID", "TRSEQ"
      ),
      label = c(
        "Study Identifier",
        "Unique Subject Identifier",
        "Actual Treatment",
        "Actual Treatment (N)",
        "Parameter Code",
        "Parameter",
        "Parameter (N)",
        "Parameter Category 1",
        "Analysis Value",
        "Analysis Value (C)",
        "Baseline Record Flag",
        "Baseline Value",
        "Change from Baseline",
        "Percent Change from Baseline",
        "Analysis Visit",
        "Analysis Visit (N)",
        "Analysis Date",
        "Analysis Date Imputation Flag",
        "Derivation Type",
        "Analysis Record Flag 01",
        "Safety Population Flag",
        "Intent-to-Treat Population Flag",
        "Efficacy Population Flag",
        "Tumor Group ID",
        "Tumor Link ID",
        "Sequence Number"
      ),
      type = c(
        "Char", "Char", "Char", "Num", "Char", "Char",
        "Num", "Char", "Num", "Char", "Char", "Num",
        "Num", "Num", "Char", "Num", "Num", "Char",
        "Char", "Char", "Char", "Char", "Char",
        "Char", "Char", "Num"
      ),
      core = c(
        "Req", "Req", "Req", "Req", "Req", "Req",
        "Cond", "Cond", "Req", "Cond", "Cond", "Cond",
        "Cond", "Cond", "Cond", "Cond", "Cond", "Cond",
        "Cond", "Cond", "Req", "Req", "Cond",
        "Cond", "Cond", "Cond"
      ),
      stringsAsFactors = FALSE
    )
  )
}


#' Extract CDISC Version from TS Domain
#'
#' @description
#' Reads a Trial Summary (TS) dataset and extracts the CDISC standard version
#' information. Looks for SDTM IG version (TSPARMCD = "SDTIGVER" or "CDISCVER")
#' and ADaM IG version (TSPARMCD = "ADAMIGVR") parameters.
#'
#' @param ts_data A data frame representing a TS (Trial Summary) domain.
#'   Must contain at minimum TSPARMCD and TSVAL columns.
#'
#' @return
#' A list containing:
#' \item{sdtm_ig_version}{Character: SDTM IG version (e.g., "3.4"), or NA}
#' \item{adam_ig_version}{Character: ADaM IG version (e.g., "1.3"), or NA}
#' \item{study_id}{Character: STUDYID from TS if available, or NA}
#' \item{protocol_title}{Character: Protocol title if available, or NA}
#' \item{version_note}{Character: Formatted note string for reports}
#'
#' @export
#' @examples
#' \dontrun{
#' ts <- data.frame(
#'   STUDYID = rep("STUDY001", 3),
#'   TSPARMCD = c("SDTIGVER", "ADAMIGVR", "TITLE"),
#'   TSPARM = c("SDTM IG Version", "ADaM IG Version", "Protocol Title"),
#'   TSVAL = c("3.4", "1.3", "My Phase 3 Trial"),
#'   stringsAsFactors = FALSE
#' )
#' version_info <- extract_cdisc_version(ts)
#' cat(version_info$version_note)
#' }
extract_cdisc_version <- function(ts_data) {
  if (!is.data.frame(ts_data)) {
    stop("ts_data must be a data frame", call. = FALSE)
  }

  # Normalize column names to uppercase for case-insensitive matching
  col_upper <- toupper(colnames(ts_data))

  # Check for required columns
  parmcd_col <- which(col_upper == "TSPARMCD")
  val_col <- which(col_upper == "TSVAL")

  if (length(parmcd_col) == 0 || length(val_col) == 0) {
    warning("TS data must contain TSPARMCD and TSVAL columns", call. = FALSE)
    return(list(
      sdtm_ig_version = NA_character_,
      adam_ig_version = NA_character_,
      study_id = NA_character_,
      protocol_title = NA_character_,
      version_note = ""
    ))
  }

  parmcd_col <- parmcd_col[1]
  val_col <- val_col[1]

  parmcds <- toupper(as.character(ts_data[[parmcd_col]]))
  vals <- as.character(ts_data[[val_col]])

  # Helper to look up a parameter value
  get_ts_val <- function(codes) {
    for (code in codes) {
      idx <- which(parmcds == code)
      if (length(idx) > 0) {
        return(vals[idx[1]])
      }
    }
    return(NA_character_)
  }

  sdtm_ver <- get_ts_val(c("SDTIGVER", "CDISCVER"))
  adam_ver <- get_ts_val(c("ADAMIGVR"))
  study_id <- get_ts_val(c("STUDYID"))
  protocol_title <- get_ts_val(c("TITLE"))

  # Also try STUDYID column directly if not found as a parameter
  studyid_col <- which(col_upper == "STUDYID")
  if (is.na(study_id) && length(studyid_col) > 0 && nrow(ts_data) > 0) {
    study_id <- as.character(ts_data[[studyid_col[1]]][1])
  }

  # Build version note
  parts <- character(0)
  if (!is.na(sdtm_ver)) {
    parts <- c(parts, paste0("SDTM IG ", sdtm_ver))
  }
  if (!is.na(adam_ver)) {
    parts <- c(parts, paste0("ADaM IG ", adam_ver))
  }

  if (length(parts) > 0) {
    version_note <- paste0("CDISC Version (from TS): ", paste(parts, collapse = ", "))
  } else {
    version_note <- ""
  }

  return(list(
    sdtm_ig_version = sdtm_ver,
    adam_ig_version = adam_ver,
    study_id = study_id,
    protocol_title = protocol_title,
    version_note = version_note
  ))
}
