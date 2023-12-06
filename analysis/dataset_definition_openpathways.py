################################################################################
# This script defines and extracts relevant variables for people with a completed
# RTT pathway from May 2021 - Apr 2022 regardless of treatment type/specialty
################################################################################


from ehrql import create_dataset, case, when, days, years, minimum_of
from ehrql.tables.beta.tpp import (
    patients, 
    medications, 
    addresses,
    practice_registrations,
    clinical_events,
    wl_openpathways)

import codelists


dataset = create_dataset()


#### Waiting list variables ####

# WL data - exclude rows with missing dates/dates outside study period/end date before start date
openpathways = wl_openpathways.where(
        wl_openpathways.referral_to_treatment_period_start_date.is_not_null()
        & (wl_openpathways.week_ending_date == "2022-05-01")
        & wl_openpathways.waiting_list_type.is_in(["IRTT","ORTT","PTLO","PTLI","RTTO","RTTI"])
    )

# Number of RTT pathways per person
dataset.count_rtt_rows = openpathways.count_for_patient()
dataset.count_rtt_start_date = openpathways.referral_to_treatment_period_start_date.count_distinct_for_patient()
dataset.count_patient_id = openpathways.pseudo_patient_pathway_identifier.count_distinct_for_patient()
dataset.count_organisation_id = openpathways.pseudo_organisation_code_patient_pathway_identifier_issuer.count_distinct_for_patient()
dataset.count_referral_id = openpathways.pseudo_referral_identifier.count_distinct_for_patient()

# Latest waiting list
#   Sort by IDs and start date to identify unique RTT pathways
last_openpathways = openpathways.sort_by(
        openpathways.referral_to_treatment_period_start_date,
        openpathways.pseudo_referral_identifier,
        openpathways.pseudo_patient_pathway_identifier,
        openpathways.pseudo_organisation_code_patient_pathway_identifier_issuer
    ).last_for_patient()

# RTT waiting list start date and end date
dataset.rtt_start_date = last_openpathways.referral_to_treatment_period_start_date
dataset.wait_time = ("2022-05-01" - dataset.rtt_start_date).days

# Other relevant columns
dataset.treatment_function = last_openpathways.activity_treatment_function_code
dataset.waiting_list_type = last_openpathways.waiting_list_type
dataset.priority_type = last_openpathways.priority_type_code


#### Censoring dates ####

# Registered 6 months before WL start
registrations = practice_registrations.where(
        practice_registrations.start_date.is_on_or_before(dataset.rtt_start_date - days(182))
    ).for_patient_on(dataset.rtt_start_date)

dataset.reg_end_date = registrations.end_date
dataset.dod = patients.date_of_death
dataset.end_date = minimum_of(dataset.reg_end_date, dataset.dod, "2022-05-01")

# Flag if censored before May 2022
dataset.censor_before_study_end = dataset.end_date < "2022-05-01"


#### Demographics ####

dataset.age = patients.age_on(dataset.rtt_start_date)
dataset.age_group = case(
        when(dataset.age < 40).then("18-39"),
        when(dataset.age < 50).then("40-49"),
        when(dataset.age < 60).then("50-59"),
        when(dataset.age < 70).then("60-69"),
        when(dataset.age < 80).then("70-79"),
        when(dataset.age >= 80).then("80+"),
        default="Missing",
)
dataset.sex = patients.sex

# IMD decile
imd = addresses.for_patient_on(dataset.rtt_start_date).imd_rounded
dataset.imd10 = case(
        when((imd >= 0) & (imd < int(32844 * 1 / 10))).then("1 (most deprived)"),
        when(imd < int(32844 * 2 / 10)).then("2"),
        when(imd < int(32844 * 3 / 10)).then("3"),
        when(imd < int(32844 * 4 / 10)).then("4"),
        when(imd < int(32844 * 5 / 10)).then("5"),
        when(imd < int(32844 * 6 / 10)).then("6"),
        when(imd < int(32844 * 7 / 10)).then("7"),
        when(imd < int(32844 * 8 / 10)).then("8"),
        when(imd < int(32844 * 9 / 10)).then("9"),
        when(imd >= int(32844 * 9 / 10)).then("10 (least deprived)"),
        default="Unknown"
)

# Ethnicity 6 categories
ethnicity6 = clinical_events.where(
        clinical_events.snomedct_code.is_in(codelists.ethnicity_codes_6)
    ).where(
        clinical_events.date.is_on_or_before(dataset.rtt_start_date)
    ).sort_by(
        clinical_events.date
    ).last_for_patient().snomedct_code.to_category(codelists.ethnicity_codes_6)

dataset.ethnicity6 = case(
    when(ethnicity6 == "1").then("White"),
    when(ethnicity6 == "2").then("Mixed"),
    when(ethnicity6 == "3").then("South Asian"),
    when(ethnicity6 == "4").then("Black"),
    when(ethnicity6 == "5").then("Other"),
    when(ethnicity6 == "6").then("Not stated"),
    default="Unknown"
)

# Ethnicity 16 categories
ethnicity16 = clinical_events.where(
        clinical_events.snomedct_code.is_in(codelists.ethnicity_codes_16)
    ).where(
        clinical_events.date.is_on_or_before(dataset.rtt_start_date)
    ).sort_by(
        clinical_events.date
    ).last_for_patient().snomedct_code.to_category(codelists.ethnicity_codes_16)

dataset.ethnicity16 = case(
    when(ethnicity16 == "1").then("White - British"),
    when(ethnicity16 == "2").then("White - Irish"),
    when(ethnicity16 == "3").then("White - Other"),
    when(ethnicity16 == "4").then("Mixed - White/Black Caribbean"),
    when(ethnicity16 == "5").then("Mixed - White/Black African"),
    when(ethnicity16 == "6").then("Mixed - White/Asian"),
    when(ethnicity16 == "7").then("Mixed - Other"),
    when(ethnicity16 == "8").then("Asian or Asian British - Indian"),
    when(ethnicity16 == "9").then("Asian or Asian British - Pakistani"),
    when(ethnicity16 == "10").then("Asian or Asian British - Bangladeshi"),
    when(ethnicity16 == "11").then("Asian or Asian British - Other"),
    when(ethnicity16 == "12").then("Black - Caribbean"),    
    when(ethnicity16 == "13").then("Black - African"),
    when(ethnicity16 == "14").then("Black - Other"),
    when(ethnicity16 == "15").then("Other - Chinese"),
    when(ethnicity16 == "16").then("Other - Other"),
    default="Unknown"
)

dataset.region = practice_registrations.for_patient_on(dataset.rtt_start_date).practice_nuts1_region_name


#### Clinical characteristics ####

# Cancer diagnosis in past 5 years 
dataset.cancer = clinical_events.where(
        clinical_events.snomedct_code.is_in(codelists.cancer_codes)
    ).where(
        clinical_events.date.is_between_but_not_on(dataset.rtt_start_date - years(5), dataset.rtt_start_date)
    ).exists_for_patient()

comorbidities = ["diabetes","cardiac","copd","liver","ckd","osteoarthritis","depress_or_gad","ra"]
comorb_codes = {
    "diabetes": codelists.diabetes_codes,
    "cardiac": codelists.cardiac_codes,
    "copd": codelists.copd_codes,
    "liver": codelists.liver_codes,
    "ckd": codelists.ckd_codes,
    "osteoarthritis": codelists.osteoarthritis_codes,
    "depress_or_gad": codelists.depress_or_gad_codes,
    "ra": codelists.ra_codes,
    }


# Comorbidities in past 2 years

clin_events_2yrs = clinical_events.where(
        clinical_events.date.is_between_but_not_on(dataset.rtt_start_date - years(2), dataset.rtt_start_date)
    )

for comorb in comorbidities:
        
    if comorb in ["diabetes","cardiac","copd","liver","osteoarthritis","ra"]:

        ctv3_name = comorb
        ctv3_query = clin_events_2yrs.where(
                clin_events_2yrs.ctv3_code.is_in(comorb_codes[comorb])
            ).exists_for_patient()
        setattr(dataset, ctv3_name, ctv3_query)
    
    else:

        snomed_name = comorb
        snomed_query = clin_events_2yrs.where(
                clin_events_2yrs.snomedct_code.is_in(comorb_codes[comorb])
            ).exists_for_patient()
        setattr(dataset, snomed_name, snomed_query)


# ### TO ADD MORE? ###


# #### DEFINE POPULATION ####

dataset.define_population(
    (dataset.age >= 18) 
    & (dataset.age < 110)
    & dataset.sex.is_in(["male","female"])
    & dataset.end_date.is_after(dataset.rtt_start_date)
    & registrations.exists_for_patient()
    & last_openpathways.exists_for_patient()
)
