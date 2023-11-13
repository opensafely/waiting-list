##################################################################
# Some covariates used in the study are created from codelists
# of clinical conditions or numerical values available on a
# patient's records. This script fetches all of the codelists 
# identified in codelists.txt from OpenCodelists.
####################################################################


# --- IMPORT STATEMENTS ---

## Import code building blocks from cohort extractor package
from databuilder.ehrql import codelist_from_csv

 
# CODELISTS 

### Medications
opioid_codes = codelist_from_csv(
    "codelists/user-anschaf-opioids-for-analgesia-dmd.csv",
    column = "code"
)

hi_opioid_codes = codelist_from_csv(
    "codelists/opensafely-high-dose-long-acting-opioids-openprescribing-dmd.csv",
    column = "code"
)
antidepressant_codes = codelist_from_csv(
    "codelists/user-anschaf-antidepressants-dmd.csv",
    column = "code"
)

gabapentinoid_codes = codelist_from_csv(
    "codelists/user-anschaf-gabapentinoids-dmd.csv",
    column = "code"
)

nsaid_codes = codelist_from_csv(
    "codelists/opensafely-nsaids-oral.csv",
    column = "code"
)

# Demographics
ethnicity_codes_16 = codelist_from_csv(
    "codelists/opensafely-ethnicity-snomed-0removed.csv",
    column="snomedcode",
    category_column="Grouping_16",
)

ethnicity_codes_6 = codelist_from_csv(
    "codelists/opensafely-ethnicity-snomed-0removed.csv",
    column="snomedcode",
    category_column="Grouping_6",
)

# Comorbidities

oth_ca_codes = codelist_from_csv(
  "codelists/opensafely-cancer-excluding-lung-and-haematological-snomed.csv",
  column = "id"
)

lung_ca_codes = codelist_from_csv(
  "codelists/opensafely-lung-cancer-snomed.csv",
  column = "id"
)

haem_ca_codes = codelist_from_csv(
  "codelists/opensafely-haematological-cancer-snomed.csv",
  column = "id"
)

cancer_codes = (
  oth_ca_codes +
  lung_ca_codes +
  haem_ca_codes
)

osteo_codes = codelist_from_csv(
    "codelists/opensafely-osteoarthritis.csv",
    column = "CTV3ID"
)

depress_gad_codes = codelist_from_csv(
    "codelists/ons-depression-and-generalised-anxiety-disorder-diagnoses-and-symptoms.csv",
    column = "code"
)

cardiac_codes = codelist_from_csv(
  "codelists/opensafely-chronic-cardiac-disease.csv",
  column = "CTV3ID"
)

ckd_codes = codelist_from_csv(
  "codelists/opensafely-chronic-kidney-disease-snomed.csv",
  column = "id"
)

liver_codes = codelist_from_csv(
  "codelists/opensafely-chronic-liver-disease.csv",
  column = "CTV3ID"
)

diabetes_codes = codelist_from_csv(
  "codelists/opensafely-diabetes.csv",
  column = "CTV3ID"
)

copd_codes = codelist_from_csv(
  "codelists/opensafely-chronic-respiratory-disease.csv",
  column = "CTV3ID"
)
