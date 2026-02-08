================================================================================
SYNTHETIC SDTM CLINICAL TRIAL DATASETS
================================================================================
Study: CLIN-2025-042
Generated: 2025-02-08
Subjects: 500 (+ 3 new in v2)
Sites: 5 (SITE01-SITE05, USA/CAN/GBR)
Arms: TRT01 (Treatment A), TRT02 (Treatment B), PBO (Placebo)

================================================================================
DATASET DESCRIPTIONS
================================================================================

1. DM - DEMOGRAPHICS (dm_v1.csv, dm_v2.csv)
   Records: 500 (v1) → 503 (v2)
   Columns: 16 (v1) → 17 (v2)
   
   Required variables:
   - STUDYID: Study identifier (CLIN-2025-042)
   - DOMAIN: Domain indicator (DM)
   - USUBJID: Unique subject identifier
   - SUBJID: Subject number
   - RFSTDTC: Reference start date
   - RFENDTC: Reference end date
   - SITEID: Site identifier (SITE01-SITE05)
   - SEX: Biological sex (M/F)
   - AGE: Age in years (18-75)
   - AGEU: Age units (YEARS)
   - RACE: Race (WHITE, BLACK, ASIAN, OTHER)
   - ETHNIC: Ethnicity (NOT HISPANIC, HISPANIC) - v2 only
   - ARMCD: Arm code (TRT01, TRT02, PBO)
   - ARM: Arm description
   - COUNTRY: Country (USA, CAN, GBR)
   - ACTARMCD: Actual arm code
   - ACTARM: Actual arm description
   
   V2 Differences:
   - ETHNIC column added
   - 3 new subjects (SUBJID: NEW00, NEW01, NEW02)
   - 10 subjects had RACE corrected
   - 5 subjects had AGE corrected

2. AE - ADVERSE EVENTS (ae_v1.csv, ae_v2.csv)
   Records: 1495 (v1) → 1545 (v2)
   Avg AEs per subject: ~3
   
   Required variables:
   - STUDYID, DOMAIN, USUBJID
   - AESEQ: Adverse event sequence number
   - AETERM: Adverse event term
   - AEDECOD: Adverse event decoded term
   - AEBODSYS: Body system (8 systems)
   - AESEV: Severity (MILD, MODERATE, SEVERE)
   - AESER: Serious (Y/N)
   - AEACN: Action taken (NONE, DOSE REDUCED, DOSE INCREASED, etc.)
   - AEREL: Relationship to treatment
   - AEOUT: Outcome (RECOVERED, RECOVERING, etc.)
   - AESTDTC: Start date
   - AEENDTC: End date
   
   V2 Differences:
   - 50 new AE records added
   - 13 records had AESEV severity updated
   - 8 records had AEREL corrected

3. LB - LABORATORY (lb_v1.csv, lb_v2.csv)
   Records: 16000 (v1) → 16000 (v2)
   Expected: 500 subjects × 8 tests × 4 visits
   
   Required variables:
   - STUDYID, DOMAIN, USUBJID
   - LBSEQ: Lab test sequence
   - LBTESTCD: Test code (ALT, AST, BILI, CREAT, HGB, WBC, PLT, GLUC)
   - LBTEST: Test name
   - LBORRES: Original result
   - LBORRESU: Original units
   - LBSTRESN: Standardized numeric result
   - LBSTRESU: Standardized units
   - LBNRIND: Normal indicator (NORMAL, HIGH, LOW)
   - VISITNUM: Visit number (1-4)
   - VISIT: Visit name (Screening, Baseline, Week 4, Week 8)
   - LBDTC: Lab test date
   
   V2 Differences:
   - 100 records had LBSTRESN values updated
   - 17 records had LBNRIND corrected

4. VS - VITAL SIGNS (vs_v1.csv, vs_v2.csv)
   Records: 14000 (v1) → 14020 (v2)
   Expected: 500 subjects × 7 tests × 4 visits = 14000
   
   Required variables:
   - STUDYID, DOMAIN, USUBJID
   - VSSEQ: Vital sign sequence
   - VSTESTCD: Test code (SYSBP, DIABP, PULSE, TEMP, RESP, WEIGHT, HEIGHT)
   - VSTEST: Test name
   - VSORRES: Original result
   - VSORRESU: Original units
   - VSSTRESN: Standardized numeric result
   - VSSTRESU: Standardized units
   - VISITNUM: Visit number (1-4)
   - VISIT: Visit name
   - VSDTC: Vital sign date
   
   V2 Differences:
   - 20 new VS records added
   - 50 records had VSSTRESN values updated

5. EX - EXPOSURE (ex_v1.csv, ex_v2.csv)
   Records: 1500 (v1) → 1500 (v2)
   Expected: 500 subjects × 3 treatment visits
   
   Required variables:
   - STUDYID, DOMAIN, USUBJID
   - EXSEQ: Exposure sequence
   - EXTRT: Treatment name
   - EXDOSE: Dose value
   - EXDOSU: Dose units (mg)
   - EXDOSFRM: Dosage form (TABLET, CAPSULE)
   - EXROUTE: Route of administration (ORAL, IV)
   - EXSTDTC: Start date
   - EXENDTC: End date
   - VISITNUM: Visit number (2, 3, 4)
   - VISIT: Visit name
   - EPOCH: Study phase (TREATMENT)
   
   V2 Differences:
   - 11 records had EXDOSE corrected
   - 3 records had EXROUTE corrected

================================================================================
DATA CHARACTERISTICS
================================================================================

RANDOMIZATION:
- TRT01 (Treatment A): 40% of subjects
- TRT02 (Treatment B): 40% of subjects
- PBO (Placebo): 20% of subjects

SITES:
- SITE01: USA, 100 subjects
- SITE02: USA, 100 subjects
- SITE03: USA, 100 subjects
- SITE04: CAN, 100 subjects
- SITE05: GBR, 100 subjects

VISIT SCHEDULE:
1. Screening: Day -7
2. Baseline: Day 0
3. Week 4: Day 28
4. Week 8: Day 56

ADVERSE EVENTS:
19 different event terms across 8 body systems
Realistic severity and relationship distributions

LABORATORY TESTS:
8 test parameters with realistic ranges and normal/high/low distributions

VITAL SIGNS:
7 vital sign measurements per visit
Realistic physiological ranges

EXPOSURES:
Treatment doses:
- Treatment A: 100 mg TABLET ORAL
- Treatment B: 50 mg CAPSULE ORAL
- Placebo: 0 mg TABLET ORAL

================================================================================
REPRODUCIBILITY
================================================================================
Random seed: 42 (numpy.random.seed(42))
All datasets are reproducible and deterministic

================================================================================
USE CASES
================================================================================
These datasets are designed for testing and validating:
- Data comparison tools
- SDTM validation systems
- Clinical trial data processing pipelines
- Version reconciliation algorithms
- Data quality assessment tools

The intentional differences between v1 and v2 provide realistic scenarios
for identifying and tracking data changes across interim and final datasets.

================================================================================
