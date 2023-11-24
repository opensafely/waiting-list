###########################################################
# This script creates weekly opioid prescribing rates
#   in the 6 months pre-waiting list, during waiting list and 
#   6 months post-waiting list for people with a completed RTT pathway
#   for orthopaedic surgery only
###########################################################

from ehrql import INTERVAL, Measures, weeks, days, minimum_of, years, when, case
from ehrql.tables.beta.tpp import (
    patients, 
    practice_registrations,
    medications,
    clinical_events,
    addresses,
    wl_clockstops)

import codelists

##########

from argparse import ArgumentParser

parser = ArgumentParser()
parser.add_argument("--codelist")

args = parser.parse_args()

codelist_name = args.codelist
codelist = getattr(codelists, codelist_name)

##########

# WL data - exclude rows with missing dates/dates outside study period/end date before start date
last_clockstops = wl_clockstops.where(
        wl_clockstops.referral_to_treatment_period_end_date.is_on_or_between("2021-05-01", "2022-04-30")
        & wl_clockstops.referral_to_treatment_period_start_date.is_on_or_before(wl_clockstops.referral_to_treatment_period_end_date)
        & wl_clockstops.week_ending_date.is_on_or_between("2021-05-01", "2022-04-30")
        & wl_clockstops.waiting_list_type.is_in(["IRTT","ORTT","PTLO","PTLI","PLTI","RTTO","RTTI","PTL0","PTL1"])
    ).sort_by(
        wl_clockstops.referral_to_treatment_period_start_date,
        wl_clockstops.pseudo_referral_identifier,
        wl_clockstops.pseudo_patient_pathway_identifier,
        wl_clockstops.pseudo_organisation_code_patient_pathway_identifier_issuer
    ).last_for_patient()

# Orthopaedic surgery 
ortho_surgery = last_clockstops.activity_treatment_function_code.is_in(["110","111","108","115"])

# RTT waiting list start date and end date
rtt_start_date = last_clockstops.referral_to_treatment_period_start_date
rtt_end_date = last_clockstops.referral_to_treatment_period_end_date

# Set artificial start/end date for running Measures
#   this is to standardise dates, as every person's 
#   start is different (and Measures works on calendar dates only)
tmp_date = "2000-01-01"

# All opioid prescriptions during study period
all_opioid_rx = medications.where(
                medications.dmd_code.is_in(codelists.opioid_codes)
                & medications.date.is_on_or_between(rtt_start_date - days(182), rtt_end_date + days(182))
            )

# Standardise Rx dates relative to RTT start date for prescribing during WL 
all_opioid_rx.tmp_wait_date = tmp_date + days((all_opioid_rx.date - rtt_start_date).days)

# Standardise Rx dates relative to RTT end date for post-WL prescribing
all_opioid_rx.tmp_post_date = tmp_date + days((all_opioid_rx.date - (rtt_end_date + days(1))).days)

# Standardise Rx dates relative to RTT start date for pre-WL prescribing (note: dates count backwards from start date)
all_opioid_rx.tmp_pre_date = tmp_date + days((all_opioid_rx.date - (rtt_start_date - days(182))).days)


### Grouping/stratification variables (Final list TBD) ###
prior_opioid_rx = all_opioid_rx.where(
        all_opioid_rx.date.is_on_or_between(rtt_start_date - days(182), rtt_start_date - days(1))
    ).exists_for_patient()


### Prescribing variables for numerator ####

# Num Rx during waiting list (up to 1 year)
count_opioid_wait = all_opioid_rx.where(
                all_opioid_rx.dmd_code.is_in(codelist)
                & all_opioid_rx.tmp_wait_date.is_on_or_between(INTERVAL.start_date, INTERVAL.end_date)
            ).count_for_patient()

# Num Rx post waiting list (up to 6 months)
count_opioid_post = all_opioid_rx.where(
                all_opioid_rx.dmd_code.is_in(codelist)
                & all_opioid_rx.tmp_post_date.is_on_or_between(INTERVAL.start_date, INTERVAL.end_date)
            ).count_for_patient()

# Num Rx pre waiting list (up to 6 months)
count_opioid_pre = all_opioid_rx.where(
                all_opioid_rx.dmd_code.is_in(codelist)
                & all_opioid_rx.tmp_pre_date.is_on_or_between(INTERVAL.start_date, INTERVAL.end_date)
            ).count_for_patient()


## Censoring date
registrations = practice_registrations.where(
        practice_registrations.start_date.is_on_or_before(rtt_start_date - days(182))
        & (practice_registrations.end_date.is_after(rtt_start_date) | practice_registrations.end_date.is_null())
    )

reg_end_date = registrations.sort_by(registrations.end_date).last_for_patient().end_date
end_date = minimum_of(reg_end_date, patients.date_of_death, rtt_end_date + days(182))

# Standardise end date relative to start dates
tmp_end_date_rtt_start = tmp_date + days((end_date - rtt_start_date).days)
tmp_end_date_rtt_end = tmp_date + days((end_date - rtt_end_date).days)
tmp_rtt_end = tmp_date + days((rtt_end_date - rtt_start_date).days)


## Cancer diagnosis in past 5 years 
cancer = clinical_events.where(
        clinical_events.snomedct_code.is_in(codelists.cancer_codes)
    ).where(
        clinical_events.date.is_on_or_between(rtt_start_date - years(5), rtt_start_date)
    ).exists_for_patient()

## Demographics


age = patients.age_on(rtt_start_date)
age_group = case(
        when(age < 40).then("18-39"),
        when(age < 50).then("40-49"),
        when(age < 60).then("50-59"),
        when(age < 70).then("60-69"),
        when(age < 80).then("70-79"),
        when(age >= 80).then("80+"),
        default="Missing",
)

sex = patients.sex

# IMD decile
imd = addresses.for_patient_on(rtt_start_date).imd_rounded
imd10 = case(
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
        clinical_events.date.is_on_or_before(rtt_start_date)
    ).sort_by(
        clinical_events.date
    ).last_for_patient().snomedct_code.to_category(codelists.ethnicity_codes_6)

ethnicity6 = case(
    when(ethnicity6 == "1").then("White"),
    when(ethnicity6 == "2").then("Mixed"),
    when(ethnicity6 == "3").then("South Asian"),
    when(ethnicity6 == "4").then("Black"),
    when(ethnicity6 == "5").then("Other"),
    when(ethnicity6 == "6").then("Not stated"),
    default="Unknown"
)


######


measures = Measures()

# Denominator = everyone on the waiting list with non-missing age/sex, 
#   and whose end date (i.e. death or deregistration) is after the end of the interval
#   and is waiting for orthopaedic surgery and no history of cancer
denominator = (        
        (patients.age_on(rtt_start_date) >= 18) 
        & (patients.age_on(rtt_start_date) < 110)
        & (patients.sex.is_in(["male","female"]))
        & last_clockstops.exists_for_patient()
        & registrations.exists_for_patient()
        & ortho_surgery
        & ~cancer
        & (end_date >= rtt_start_date)
    )

# Prescribing during WL
measures.define_measure(
    name="count_wait",
    numerator=count_opioid_wait,
    # Denominator = only include people whose RTT end date and study end date are after interval end date
    #   IOW, exclude people who are no longer on waiting list or have been censored
    denominator=denominator & (tmp_end_date_rtt_start > INTERVAL.end_date) & (tmp_rtt_end > INTERVAL.end_date),
    intervals=weeks(52).starting_on("2000-01-01")
    )

# Prescribing post WL
measures.define_measure(
    name="count_post",
    numerator=count_opioid_post,
    # Denominator = only include people whose RTT end date is after interval end date
    #   IOW, exclude people who have been censored
    denominator=denominator & (tmp_end_date_rtt_end > INTERVAL.end_date),
    intervals=weeks(26).starting_on("2000-01-01")
    )

# Prescribing pre WL
measures.define_measure(
    name="count_pre",
    numerator=count_opioid_pre,
    denominator=denominator,
    intervals=weeks(26).starting_on("2000-01-01")
    )



# Prescribing pre WL - stratified by prior opioid Rx
measures.define_measure(
    name="count_pre_prior",
    numerator=count_opioid_pre,
    denominator=denominator,
    intervals=weeks(26).starting_on("2000-01-01"),
    group_by={"prior_opioid_rx": prior_opioid_rx}
    )

# Prescribing during WL - stratified by prior opioid Rx
measures.define_measure(
    name="count_wait_prior",
    numerator=count_opioid_wait,
    denominator=denominator & (tmp_end_date_rtt_start > INTERVAL.end_date) & (tmp_rtt_end > INTERVAL.end_date),
    intervals=weeks(52).starting_on("2000-01-01"),
    group_by={"prior_opioid_rx": prior_opioid_rx}
    )

# Prescribing post WL - stratified by prior opioid Rx
measures.define_measure(
    name="count_post_prior",
    numerator=count_opioid_post,
    denominator=denominator & (tmp_end_date_rtt_end > INTERVAL.end_date),
    intervals=weeks(26).starting_on("2000-01-01"),
    group_by={"prior_opioid_rx": prior_opioid_rx}
    )


# Prescribing pre WL - stratified by age
measures.define_measure(
    name="count_pre_age",
    numerator=count_opioid_pre,
    denominator=denominator,
    intervals=weeks(26).starting_on("2000-01-01"),
    group_by={"age_group": age_group}
    )

# Prescribing during WL - stratified by age
measures.define_measure(
    name="count_wait_age",
    numerator=count_opioid_wait,
    denominator=denominator & (tmp_end_date_rtt_start > INTERVAL.end_date) & (tmp_rtt_end > INTERVAL.end_date),
    intervals=weeks(52).starting_on("2000-01-01"),
    group_by={"age_group": age_group}
    )

# Prescribing post WL - stratified by age
measures.define_measure(
    name="count_post_age",
    numerator=count_opioid_post,
    denominator=denominator & (tmp_end_date_rtt_end > INTERVAL.end_date),
    intervals=weeks(26).starting_on("2000-01-01"),
    group_by={"age_group": age_group}
    )


# Prescribing pre WL - stratified by imd
measures.define_measure(
    name="count_pre_imd",
    numerator=count_opioid_pre,
    denominator=denominator,
    intervals=weeks(26).starting_on("2000-01-01"),
    group_by={"imd_decile": imd10}
    )

# Prescribing during WL - stratified by prior opioid Rx
measures.define_measure(
    name="count_wait_imd",
    numerator=count_opioid_wait,
    denominator=denominator & (tmp_end_date_rtt_start > INTERVAL.end_date) & (tmp_rtt_end > INTERVAL.end_date),
    intervals=weeks(52).starting_on("2000-01-01"),
    group_by={"imd_decile": imd10}
    )

# Prescribing post WL - stratified by prior opioid Rx
measures.define_measure(
    name="count_post_imd",
    numerator=count_opioid_post,
    denominator=denominator & (tmp_end_date_rtt_end > INTERVAL.end_date),
    intervals=weeks(26).starting_on("2000-01-01"),
    group_by={"imd_decile": imd10}
    )

    
# Prescribing pre WL - stratified by ethnicity
measures.define_measure(
    name="count_pre_sex",
    numerator=count_opioid_pre,
    denominator=denominator,
    intervals=weeks(26).starting_on("2000-01-01"),
    group_by={"sex": sex}
    )

# Prescribing during WL - stratified by prior opioid Rx
measures.define_measure(
    name="count_wait_sex",
    numerator=count_opioid_wait,
    denominator=denominator & (tmp_end_date_rtt_start > INTERVAL.end_date) & (tmp_rtt_end > INTERVAL.end_date),
    intervals=weeks(52).starting_on("2000-01-01"),
    group_by={"sex": sex}
    )

# Prescribing post WL - stratified by prior opioid Rx
measures.define_measure(
    name="count_post_sex",
    numerator=count_opioid_post,
    denominator=denominator & (tmp_end_date_rtt_end > INTERVAL.end_date),
    intervals=weeks(26).starting_on("2000-01-01"),
    group_by={"sex": sex}
    )
