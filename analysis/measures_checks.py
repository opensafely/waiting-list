###########################################################
# The purpose of this script is to check the distributions
# of records over time. 
# It counts the monthly number of records by RTT start date,
# RTT end date and week ending date.
# It also counts number of patients by RTT end date.
###########################################################

from ehrql import INTERVAL, Measures, months
from ehrql.tables.beta.tpp import (
    patients, 
    wl_clockstops)


##########

count_rtt_end_date = wl_clockstops.where(
        (wl_clockstops.referral_to_treatment_period_end_date >= INTERVAL.start_date)
        & (wl_clockstops.referral_to_treatment_period_end_date <= INTERVAL.end_date)
    ).count_for_patient()

count_rtt_end_date_ortho = wl_clockstops.where(
        (wl_clockstops.referral_to_treatment_period_end_date >= INTERVAL.start_date)
        & (wl_clockstops.referral_to_treatment_period_end_date <= INTERVAL.end_date)
        & (wl_clockstops.activity_treatment_function_code.is_in(["110"]))
    ).count_for_patient()

count_rtt_start_date = wl_clockstops.where(
        (wl_clockstops.referral_to_treatment_period_start_date >= INTERVAL.start_date)
        & (wl_clockstops.referral_to_treatment_period_start_date <= INTERVAL.end_date)
    ).count_for_patient()

count_week_ending_date = wl_clockstops.where(
        (wl_clockstops.week_ending_date >= INTERVAL.start_date)
        &  (wl_clockstops.week_ending_date >= INTERVAL.end_date)
    ).count_for_patient()

wl_type = wl_clockstops.waiting_list_type

######

measures = Measures()

denominator = (        
        (patients.age_on(INTERVAL.start_date) >= 0) 
        & (patients.age_on(INTERVAL.start_date) < 110)
        & (patients.sex.is_in(["male","female"]))
        & wl_clockstops.exists_for_patient()
    )

measures.define_defaults(
    denominator = denominator,
    intervals=months(13).starting_on("2021-05-01")
    )

measures.define_measure(
    name="end_date",
    numerator=count_rtt_end_date
    )

measures.define_measure(
    name="ortho_end_date",
    numerator=count_rtt_end_date_ortho
    )

measures.define_measure(
    name="start_date",
    numerator=count_rtt_start_date
    )

measures.define_measure(
    name="week_ending_date",
    numerator=count_week_ending_date
    )


