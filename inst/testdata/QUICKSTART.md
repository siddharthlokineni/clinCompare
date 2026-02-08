# Quick Start Guide - SDTM Test Datasets

## Overview
Two versions of realistic synthetic SDTM clinical trial datasets for **Study CLIN-2025-042** with intentional differences to test comparison and validation tools.

## Dataset Versions
- **v1**: Interim dataset (500 subjects, 33,495 records)
- **v2**: Final dataset (503 subjects, 33,568 records) - with corrections and new data

## Files Available

### Demographics (DM)
- `dm_v1.csv` - 500 subjects, 16 variables
- `dm_v2.csv` - 503 subjects, 17 variables (ETHNIC added)

### Adverse Events (AE)
- `ae_v1.csv` - 1,495 adverse events
- `ae_v2.csv` - 1,545 adverse events (+50 new)

### Laboratory (LB)
- `lb_v1.csv` - 16,000 lab test results
- `lb_v2.csv` - 16,000 lab test results (100 value corrections, 17 indicator corrections)

### Vital Signs (VS)
- `vs_v1.csv` - 14,000 vital sign measurements
- `vs_v2.csv` - 14,020 vital sign measurements (+20 new, 50 value updates)

### Exposure (EX)
- `ex_v1.csv` - 1,500 treatment exposures
- `ex_v2.csv` - 1,500 treatment exposures (11 dose corrections, 3 route corrections)

## Key Characteristics

### Study Design
- Study ID: CLIN-2025-042
- Subjects: 500 (+ 3 new in v2)
- Sites: 5 (SITE01-SITE05: 3 USA, 1 Canada, 1 GB)
- Arms: TRT01 (40%), TRT02 (40%), PBO (20%)

### Demographics
- Age: 18-75 years
- Sex: 52% M, 48% F
- Race: 65% White, 15% Black, 15% Asian, 5% Other
- Ethnicity: 85% Not Hispanic, 15% Hispanic

### Data Types
- All dates in ISO format (YYYY-MM-DD)
- Numeric values with 2 decimal places
- Standard SDTM variable naming conventions
- Realistic missing values (e.g., AEENDTC for ongoing events)

## Common Use Cases

### 1. Test Data Comparison Tools
Compare v1 and v2 to identify:
- New subjects (3 in DM)
- New records (50 AE, 20 VS)
- Updated values (100 LB, 50 VS, etc.)
- Corrected values (10 RACE, 5 AGE, etc.)
- New columns (ETHNIC in DM v2)

### 2. Data Validation
Check for:
- SDTM compliance
- Data type consistency
- Valid value ranges
- Referential integrity (USUBJID linkage)
- Date sequence validity

### 3. ETL Testing
Process datasets for:
- Data transformation pipelines
- Subject key management
- Visit scheduling
- Laboratory normal ranges
- Treatment assignment tracking

### 4. Reconciliation Tools
Practice reconciliation between:
- Interim (v1) and Final (v2) datasets
- Version tracking
- Change detection
- Audit trails

## Column Quick Reference

### All Files
- STUDYID (CLIN-2025-042)
- DOMAIN (DM, AE, LB, VS, EX)
- USUBJID (unique subject ID)

### Domain-Specific Key Variables

**DM**: SUBJID, RFSTDTC, RFENDTC, SITEID, SEX, AGE, RACE, ETHNIC*, ARMCD, ARM, COUNTRY
*v2 only

**AE**: AESEQ, AETERM, AEDECOD, AEBODSYS, AESEV, AESER, AEACN, AEREL, AEOUT, AESTDTC, AEENDTC

**LB**: LBSEQ, LBTESTCD, LBTEST, LBORRES, LBORRESU, LBSTRESN, LBSTRESU, LBNRIND, VISITNUM, VISIT, LBDTC

**VS**: VSSEQ, VSTESTCD, VSTEST, VSORRES, VSORRESU, VSSTRESN, VSSTRESU, VISITNUM, VISIT, VSDTC

**EX**: EXSEQ, EXTRT, EXDOSE, EXDOSU, EXDOSFRM, EXROUTE, EXSTDTC, EXENDTC, VISITNUM, VISIT, EPOCH

## Intentional Differences (v1 → v2)

| Domain | Change Type | Count | Details |
|--------|------------|-------|---------|
| DM | New Column | 1 | ETHNIC added |
| DM | New Subjects | 3 | SUBJID: NEW00, NEW01, NEW02 |
| DM | Corrected RACE | 10 | Data corrections |
| DM | Corrected AGE | 5 | Data corrections |
| AE | New Records | 50 | New AE observations |
| AE | Updated AESEV | 13 | Severity corrections |
| AE | Corrected AEREL | 8 | Relationship corrections |
| LB | Updated LBSTRESN | 100 | Result value updates |
| LB | Corrected LBNRIND | 17 | Normal indicator fixes |
| VS | New Records | 20 | New measurements |
| VS | Updated VSSTRESN | 50 | Result value updates |
| EX | Corrected EXDOSE | 11 | Dose adjustments |
| EX | Corrected EXROUTE | 3 | Route corrections |

## Sample Data

### Load in Python
```python
import pandas as pd

# Load demographics v1
dm = pd.read_csv('dm_v1.csv')
print(dm.head())
print(f"Subjects: {len(dm)}")
print(f"Sites: {dm['SITEID'].nunique()}")
print(f"Arms: {dm['ARMCD'].unique()}")
```

### Load in R
```r
library(readr)

# Load adverse events v1
ae <- read_csv('ae_v1.csv')
head(ae)
nrow(ae)  # Total AE records
unique(ae$AEBODSYS)  # Body systems
```

### Load in SQL
```sql
-- Create table from v1 data
CREATE TABLE dm_v1 AS
SELECT * FROM (
  LOAD DATA FROM 'dm_v1.csv' FORMAT CSV
)

-- Compare v1 and v2
SELECT COUNT(*) FROM dm_v2 
WHERE SUBJID NOT IN (SELECT SUBJID FROM dm_v1)
-- Returns: 3 (new subjects)
```

## Data Validation Checks

### Expected Ranges
- **Age**: 18-75 years
- **Lab Values**: Physiologically realistic (e.g., ALT 7-56 U/L)
- **Vital Signs**: Normal ranges (e.g., SYSBP 90-140 mmHg)
- **Doses**: 0-100 mg as defined

### Cardinality Checks
- DM: 1 row per subject
- AE: Multiple rows per subject (avg 3)
- LB: 500 × 8 tests × 4 visits = 16,000 rows
- VS: 500 × 7 params × 4 visits = 14,000 rows
- EX: 500 × 3 visits = 1,500 rows

### Referential Integrity
- All USUBJID in AE, LB, VS, EX must exist in DM
- All VISITNUM should be 1-4 (with EX having 2-4)
- All SITEID should be SITE01-SITE05

## Reproducibility
- Random seed: 42
- All values are deterministic
- Regenerating with same script produces identical data

## Support Files
- `README.txt` - Detailed variable descriptions
- `QUICKSTART.md` - This guide
- `GENERATION_SUMMARY.md` - Complete technical documentation

## Questions?
Refer to README.txt for detailed variable definitions and data specifications.
