#' SDTM Metadata
#'
#' Returns metadata for SDTM domains following CDISC standards (SDTM IG 3.4).
#' Provides information about required, expected, and permissible variables
#' for each SDTM domain.
#'
#' @return A named list where keys are SDTM domain codes and values are
#'   data.frames with columns:
#'   - variable: Variable name (character)
#'   - label: Variable label/description (character)
#'   - type: Data type - "Char" for character or "Num" for numeric
#'   - core: Importance level - "Req" (Required), "Exp" (Expected), or "Perm" (Permissible)
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' sdtm_meta <- get_sdtm_metadata()
#' dm_vars <- sdtm_meta$DM
#' ae_vars <- sdtm_meta$AE
#' }
get_sdtm_metadata <- function() {
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
    )
  )
}

#' ADaM Metadata
#'
#' Returns metadata for ADaM datasets following CDISC standards.
#' Provides information about required, conditional, and other variables
#' for each ADaM analysis dataset.
#'
#' @return A named list where keys are ADaM dataset names and values are
#'   data.frames with columns:
#'   - variable: Variable name (character)
#'   - label: Variable label/description (character)
#'   - type: Data type - "Char" for character or "Num" for numeric
#'   - core: Importance level - "Req" (Required), "Cond" (Conditional), or other
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' adam_meta <- get_adam_metadata()
#' adsl_vars <- adam_meta$ADSL
#' adae_vars <- adam_meta$ADAE
#' }
get_adam_metadata <- function() {
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
    )
  )
}
