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
        wl_clockstops.referral_to_treatment_period_end_date.is_on_or_between(INTERVAL.start_date, INTERVAL.end_date)
    ).count_for_patient()

count_rtt_end_date_ortho = wl_clockstops.where(
        wl_clockstops.referral_to_treatment_period_end_date.is_on_or_between(INTERVAL.start_date, INTERVAL.end_date)
        & wl_clockstops.activity_treatment_function_code.is_in(["110"])
    ).count_for_patient()

count_admitted = wl_clockstops.where(
        wl_clockstops.referral_to_treatment_period_end_date.is_on_or_between(INTERVAL.start_date, INTERVAL.end_date)
        & wl_clockstops.waiting_list_type.is_in(["IRTT","PTLI","PLTI","RTTI","PTL1"])
    ).count_for_patient()

count_admitted_ortho = wl_clockstops.where(
        wl_clockstops.referral_to_treatment_period_end_date.is_on_or_between(INTERVAL.start_date, INTERVAL.end_date)
        & wl_clockstops.waiting_list_type.is_in(["IRTT","PTLI","PLTI","RTTI","PTL1"])
        & wl_clockstops.activity_treatment_function_code.is_in(["110"])
    ).count_for_patient()

count_not_admitted = wl_clockstops.where(
        wl_clockstops.referral_to_treatment_period_end_date.is_on_or_between(INTERVAL.start_date, INTERVAL.end_date)
        & wl_clockstops.waiting_list_type.is_in(["ORTT","PTLO","RTTO","PTL0"])
    ).count_for_patient()

count_not_admitted_ortho = wl_clockstops.where(
        wl_clockstops.referral_to_treatment_period_end_date.is_on_or_between(INTERVAL.start_date, INTERVAL.end_date)
        & wl_clockstops.waiting_list_type.is_in(["ORTT","PTLO","RTTO","PTL0"])
        & wl_clockstops.activity_treatment_function_code.is_in(["110"])
    ).count_for_patient()


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
    name="closed_total",
    numerator=count_rtt_end_date
    )

measures.define_measure(
    name="closed_ortho",
    numerator=count_rtt_end_date_ortho
    )

measures.define_measure(
    name="closed_admit_total",
    numerator=count_admitted
    )

measures.define_measure(
    name="closed_admit_ortho",
    numerator=count_admitted_ortho
    )

measures.define_measure(
    name="closed_not_admit_total",
    numerator=count_not_admitted
    )

measures.define_measure(
    name="closed_not_admit_ortho",
    numerator=count_not_admitted_ortho
    )



