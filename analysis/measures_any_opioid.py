###########################################################
# This script creates weekly opioid prescribing rates
#   in the 6 months pre-waiting list, during waiting list and 
#   6 months post-waiting list for people with a completed RTT pathway
#   for orthopaedic surgery only
###########################################################

from ehrql import INTERVAL, Measures, weeks, days, minimum_of, case, when, years
from ehrql.tables.beta.tpp import (
    patients, 
    practice_registrations,
    medications,
    clinical_events,
    wl_clockstops)

import codelists


##########

# WL data - exclude rows with missing dates/dates outside study period/end date before start date
last_clockstops = wl_clockstops.where(
        wl_clockstops.referral_to_treatment_period_end_date.is_on_or_between("2021-05-01", "2022-05-01")
        & wl_clockstops.referral_to_treatment_period_start_date.is_on_or_before(wl_clockstops.referral_to_treatment_period_end_date)
        & wl_clockstops.week_ending_date.is_on_or_between("2021-05-01", "2022-05-01")
        & wl_clockstops.waiting_list_type.is_in(["IRTT","ORTT","PTLO","PTLI","PLTI","RTTO","RTTI","PTL0","PTL1"])
    ).sort_by(
        wl_clockstops.pseudo_patient_pathway_identifier,
        wl_clockstops.pseudo_organisation_code_patient_pathway_identifier_issuer,
        wl_clockstops.pseudo_referral_identifier,
        wl_clockstops.referral_to_treatment_period_start_date
    ).last_for_patient()

# Orthopaedic surgery 
ortho_surgery = last_clockstops.activity_treatment_function_code.is_in(["110"])

# RTT waiting list start date and end date
rtt_start_date = last_clockstops.referral_to_treatment_period_start_date
rtt_end_date = last_clockstops.referral_to_treatment_period_end_date

# Set artificial start/end date for running Measures
#   this is to standardise dates, as every person's 
#   start is different (and Measures works on calendar dates only)
tmp_rtt_start_date = "2000-01-01"
tmp_rtt_end_date = "2000-01-01"
tmp_study_start_date = "2000-01-01"

# All opioid prescriptions during study period
all_opioid_rx = medications.where(
                medications.dmd_code.is_in(codelists.opioid_codes)
                & medications.date.is_on_or_between(rtt_start_date - days(182), rtt_end_date + days(182))
            )

# Standardise Rx dates relative to RTT start date for prescribing during WL 
all_opioid_rx.tmp_wait_date = tmp_rtt_start_date + days((all_opioid_rx.date - rtt_start_date).days)

# Standardise Rx dates relative to RTT end date for post-WL prescribing
all_opioid_rx.tmp_post_date = tmp_rtt_end_date + days((all_opioid_rx.date - rtt_end_date).days)

# Standardise Rx dates relative to RTT start date for pre-WL prescribing (note: dates count backwards from start date)
all_opioid_rx.tmp_pre_date = tmp_study_start_date - days((all_opioid_rx.date - (rtt_start_date - days(182))).days)


### Grouping/stratification variables (Final list TBD) ###

prior_opioid_rx = all_opioid_rx.where(
        all_opioid_rx.date.is_on_or_between(rtt_start_date - days(182), rtt_start_date - days(1))
    ).exists_for_patient()



### Opioid variables ####

### All opioids

# Num opioid Rx during waiting list
count_opioid_wait = all_opioid_rx.where(
                all_opioid_rx.tmp_wait_date.is_on_or_between(INTERVAL.start_date, INTERVAL.end_date)
            ).count_for_patient()

# Num opioid Rx post waiting list (up to 6 months)
count_opioid_post = all_opioid_rx.where(
                all_opioid_rx.tmp_post_date.is_on_or_between(INTERVAL.start_date, INTERVAL.end_date)
            ).count_for_patient()

# Num opioid Rx pre waiting list (up to 6 months)
count_opioid_pre = all_opioid_rx.where(
                all_opioid_rx.tmp_pre_date.is_on_or_between(INTERVAL.start_date, INTERVAL.end_date)
            ).count_for_patient()


## Censoring date
registrations = practice_registrations.where(
        practice_registrations.start_date.is_on_or_before(rtt_start_date - days(182))
        & (practice_registrations.end_date.is_after(rtt_start_date) | practice_registrations.end_date.is_null())
    )

reg_end_date = registrations.sort_by(registrations.end_date).last_for_patient().end_date
end_date = minimum_of(reg_end_date, patients.date_of_death, rtt_end_date + days(182))

# Standardise end date to relative to start dates
tmp_end_date_rtt_start = tmp_rtt_start_date + days((end_date - rtt_start_date).days)
tmp_end_date_rtt_end = tmp_rtt_end_date + days((end_date - rtt_end_date).days)


# Cancer diagnosis in past 5 years 
cancer = clinical_events.where(
        clinical_events.snomedct_code.is_in(codelists.cancer_codes)
    ).where(
        clinical_events.date.is_on_or_between(rtt_start_date - years(5), rtt_start_date)
    ).exists_for_patient()


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
    )

## All opioids

# Prescribing during WL
measures.define_measure(
    name="count_opioid_wait",
    numerator=count_opioid_wait,
    # Denominator = only include people whose study end date is after interval end date 
    denominator=denominator & (tmp_end_date_rtt_start > INTERVAL.end_date),
    intervals=weeks(52).starting_on("2000-01-01")
    )

# Prescribing post WL
measures.define_measure(
    name="count_opioid_post",
    numerator=count_opioid_post,
    denominator=denominator & (tmp_end_date_rtt_end > INTERVAL.end_date),
    intervals=weeks(26).starting_on("2000-01-01")
    )

# Prescribing pre WL
measures.define_measure(
    name="count_opioid_pre",
    numerator=count_opioid_pre,
    denominator=denominator,
    intervals=weeks(26).starting_on("2000-01-01")
    )

# Prescribing during WL - stratified by prior opioid Rx
measures.define_measure(
    name="count_opioid_wait_prior",
    numerator=count_opioid_wait,
    denominator=denominator & (tmp_end_date_rtt_start > INTERVAL.end_date),
    intervals=weeks(52).starting_on("2000-01-01"),
    group_by={"prior_opioid_rx": prior_opioid_rx}
    )