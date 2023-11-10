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
        #& wl_clockstops.waiting_list_type.is_in(["IRTT","PTLI","PLTI","RTTI","PTL1","ORTT","PTLO","RTTO","PTL0"])
    ).pseudo_referral_identifier.count_distinct_for_patient()

count_rtt_end_date_ortho = wl_clockstops.where(
        wl_clockstops.referral_to_treatment_period_end_date.is_on_or_between(INTERVAL.start_date, INTERVAL.end_date)
        #& wl_clockstops.waiting_list_type.is_in(["IRTT","PTLI","PLTI","RTTI","PTL1","ORTT","PTLO","RTTO","PTL0"])
        & wl_clockstops.activity_treatment_function_code.is_in(["110","111","108","115"])
    ).pseudo_referral_identifier.count_distinct_for_patient()

count_admitted = wl_clockstops.where(
        wl_clockstops.referral_to_treatment_period_end_date.is_on_or_between(INTERVAL.start_date, INTERVAL.end_date)
        & wl_clockstops.waiting_list_type.is_in(["110","111","108","115"])
    ).pseudo_referral_identifier.count_distinct_for_patient()

count_admitted_ortho = wl_clockstops.where(
        wl_clockstops.referral_to_treatment_period_end_date.is_on_or_between(INTERVAL.start_date, INTERVAL.end_date)
        & wl_clockstops.waiting_list_type.is_in(["IRTT","PTLI","PLTI","RTTI","PTL1"])
        & wl_clockstops.activity_treatment_function_code.is_in(["110","111","108","115"])
    ).pseudo_referral_identifier.count_distinct_for_patient()

count_not_admitted = wl_clockstops.where(
        wl_clockstops.referral_to_treatment_period_end_date.is_on_or_between(INTERVAL.start_date, INTERVAL.end_date)
        & wl_clockstops.waiting_list_type.is_in(["ORTT","PTLO","RTTO","PTL0"])
    ).pseudo_referral_identifier.count_distinct_for_patient()

count_not_admitted_ortho = wl_clockstops.where(
        wl_clockstops.referral_to_treatment_period_end_date.is_on_or_between(INTERVAL.start_date, INTERVAL.end_date)
        & wl_clockstops.waiting_list_type.is_in(["ORTT","PTLO","RTTO","PTL0"])
        & wl_clockstops.activity_treatment_function_code.is_in(["110","111","108","115"])
    ).pseudo_referral_identifier.count_distinct_for_patient()


count_pat = wl_clockstops.where(
        wl_clockstops.referral_to_treatment_period_end_date.is_on_or_between(INTERVAL.start_date, INTERVAL.end_date)
    ).sort_by(
        wl_clockstops.referral_to_treatment_period_end_date
    ).first_for_patient().exists_for_patient()

treatment_function_code = wl_clockstops.where(
        wl_clockstops.referral_to_treatment_period_end_date.is_on_or_between(INTERVAL.start_date, INTERVAL.end_date)
    ).sort_by(
        wl_clockstops.referral_to_treatment_period_end_date
    ).first_for_patient().activity_treatment_function_code

waiting_list_type = wl_clockstops.where(
        wl_clockstops.referral_to_treatment_period_end_date.is_on_or_between(INTERVAL.start_date, INTERVAL.end_date)
    ).sort_by(
        wl_clockstops.referral_to_treatment_period_end_date
    ).first_for_patient().waiting_list_type

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
    name="closed_pat_by_trt",
    numerator=count_pat,
    group_by={"treatment_function": treatment_function_code}
    )

measures.define_measure(
    name="closed_pat_by_wl_type",
    numerator=count_pat,
    group_by={"wl_type": waiting_list_type}
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


