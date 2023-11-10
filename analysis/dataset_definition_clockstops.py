################################################################################
# This script defines and extracts relevant variables for people with a completed
# RTT pathway from May 2021 - May 2022 regardless of treatment type/specialty
################################################################################


from ehrql import create_dataset, case, when, days, years, minimum_of
from ehrql.tables.beta.tpp import (
    patients, 
    medications, 
    addresses,
    practice_registrations,
    clinical_events,
    wl_clockstops)

import codelists


dataset = create_dataset()


#### Waiting list variables ####

# WL data - exclude rows with missing dates/dates outside study period/end date before start date
clockstops = wl_clockstops.where(
        wl_clockstops.referral_to_treatment_period_end_date.is_on_or_between("2021-05-01", "2022-05-01")
        & wl_clockstops.referral_to_treatment_period_start_date.is_on_or_before(wl_clockstops.referral_to_treatment_period_end_date)
        & wl_clockstops.week_ending_date.is_on_or_between("2021-05-01", "2022-05-01")
        & wl_clockstops.waiting_list_type.is_in(["IRTT","ORTT","PTLO","PTLI","PLTI","RTTO","RTTI","PTL0","PTL1"])
    )

# Number of RTT pathways per person
dataset.count_rtt_rows = clockstops.count_for_patient()
dataset.count_rtt_start_date = clockstops.referral_to_treatment_period_start_date.count_distinct_for_patient()
dataset.count_patient_id = clockstops.pseudo_patient_pathway_identifier.count_distinct_for_patient()
dataset.count_organisation_id = clockstops.pseudo_organisation_code_patient_pathway_identifier_issuer.count_distinct_for_patient()
dataset.count_referral_id = clockstops.pseudo_referral_identifier.count_distinct_for_patient()

# Latest waiting list
#   Sort by IDs and start date to identify unique RTT pathways
last_clockstops = clockstops.sort_by(
        clockstops.referral_to_treatment_period_start_date,
        clockstops.pseudo_referral_identifier,
        clockstops.pseudo_patient_pathway_identifier,
        clockstops.pseudo_organisation_code_patient_pathway_identifier_issuer
    ).last_for_patient()

# RTT waiting list start date and end date
dataset.rtt_start_date = last_clockstops.referral_to_treatment_period_start_date
dataset.rtt_end_date = last_clockstops.referral_to_treatment_period_end_date
dataset.wait_time = (dataset.rtt_end_date - dataset.rtt_start_date).days

# Other relevant columns
dataset.treatment_function = last_clockstops.activity_treatment_function_code
dataset.waiting_list_type = last_clockstops.waiting_list_type
dataset.priority_type = last_clockstops.priority_type_code


#### Censoring dates ####

# Registered 6 months before WL start
registrations = practice_registrations.where(
        practice_registrations.start_date.is_on_or_before(dataset.rtt_start_date - days(182))
        & (practice_registrations.end_date.is_after(dataset.rtt_start_date) | practice_registrations.end_date.is_null())
    )

dataset.reg_end_date = registrations.sort_by(registrations.end_date).last_for_patient().end_date
dataset.dod = patients.date_of_death
dataset.end_date = minimum_of(dataset.reg_end_date, dataset.dod, dataset.rtt_end_date + days(182))

# Flag if censored before WL end date
dataset.censor_before_rtt_end = dataset.end_date < dataset.rtt_end_date

# Flag if censored before study end date (RTT end + 6 months)
dataset.censor_before_study_end = dataset.end_date < dataset.rtt_end_date + days(182)


#### Medicines data ####

# Number of prescriptions during waiting list (this time period is variable, will account for this later)
def count_med_wait(codelist):
    return medications.where(
            medications.dmd_code.is_in(codelist)
            & medications.date.is_on_or_between(dataset.rtt_start_date, minimum_of(dataset.end_date, dataset.rtt_end_date))
        ).count_for_patient()

# Number of prescriptions before waiting list
def count_med_pre(codelist):
    return medications.where(
            medications.dmd_code.is_in(codelist)
            & medications.date.is_on_or_between(dataset.rtt_start_date - days(182), dataset.rtt_start_date - days(1))
        ).count_for_patient()

# Number of prescriptions after waiting list 
def count_med_post(codelist):
    return medications.where(
            medications.dmd_code.is_in(codelist)
            & medications.date.is_on_or_between(dataset.rtt_end_date + days(1), minimum_of(dataset.rtt_end_date + days(182), dataset.end_date))
            & (dataset.end_date > dataset.rtt_end_date)
        ).count_for_patient()


# Any opioid
dataset.opioid_wait_count = count_med_wait(codelists.opioid_codes)
dataset.opioid_pre_count = count_med_pre(codelists.opioid_codes)
dataset.opioid_post_count = count_med_post(codelists.opioid_codes)

# High dose/long-acting opioids
dataset.hi_opioid_wait_count = count_med_wait(codelists.hi_opioid_codes)
dataset.hi_opioid_pre_count = count_med_pre(codelists.hi_opioid_codes)
dataset.hi_opioid_post_count = count_med_post(codelists.hi_opioid_codes)

# Gabapentinoids
dataset.gaba_wait_count = count_med_wait(codelists.gabapentinoid_codes)
dataset.gaba_pre_count = count_med_pre(codelists.gabapentinoid_codes)
dataset.gaba_post_count = count_med_post(codelists.gabapentinoid_codes)

# Antidepressant
dataset.ad_wait_count = count_med_wait(codelists.antidepressant_codes)
dataset.ad_pre_count = count_med_pre(codelists.antidepressant_codes)
dataset.ad_post_count = count_med_post(codelists.antidepressant_codes)

# NSAID
dataset.nsaid_wait_count = count_med_wait(codelists.nsaid_codes)
dataset.nsaid_pre_count = count_med_pre(codelists.nsaid_codes)
dataset.nsaid_post_count = count_med_post(codelists.nsaid_codes)


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
        clinical_events.date.is_on_or_between(dataset.rtt_start_date - years(5), dataset.rtt_start_date)
    ).exists_for_patient()

# All clinical events - past 6 months
clin_events_6mo = clinical_events.where(
        clinical_events.date.is_on_or_between(dataset.rtt_start_date - days(182), dataset.rtt_start_date)
    )

# Comorbidities in past 6 mos
dataset.diabetes = clin_events_6mo.where(
        clin_events_6mo.ctv3_code.is_in(codelists.diabetes_codes)
    ).exists_for_patient()

dataset.cardiac = clin_events_6mo.where(
        clin_events_6mo.ctv3_code.is_in(codelists.cardiac_codes)
    ).exists_for_patient()

dataset.copd = clin_events_6mo.where(
        clin_events_6mo.ctv3_code.is_in(codelists.copd_codes)
    ).exists_for_patient()

dataset.liver = clin_events_6mo.where(
        clin_events_6mo.ctv3_code.is_in(codelists.liver_codes)
    ).exists_for_patient()

dataset.ckd = clin_events_6mo.where(
        clin_events_6mo.snomedct_code.is_in(codelists.ckd_codes)
    ).exists_for_patient()

dataset.osteoarthritis = clin_events_6mo.where(
        clin_events_6mo.ctv3_code.is_in(codelists.osteo_codes)
    ).exists_for_patient()

dataset.depress_or_gad = clin_events_6mo.where(
        clin_events_6mo.snomedct_code.is_in(codelists.depress_gad_codes)
    ).exists_for_patient()


### TO ADD MORE? ###


#### DEFINE POPULATION ####

dataset.define_population(
    (dataset.age >= 18) 
    & (dataset.age < 110)
    & dataset.sex.is_in(["male","female"])
    & dataset.end_date.is_after(dataset.rtt_start_date)
    & registrations.exists_for_patient()
    & last_clockstops.exists_for_patient()
)
