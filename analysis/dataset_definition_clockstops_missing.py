################################################################################
# This script identifies number of rows with missing end date
################################################################################


from ehrql import create_dataset
from ehrql.tables.tpp import (
    patients, 
    wl_clockstops)


dataset = create_dataset()

#### Waiting list variables ####

# WL data - exclude rows with missing dates/dates outside study period/end date before start date
null_dates = wl_clockstops.where(
        wl_clockstops.referral_to_treatment_period_end_date.is_null()
    )

not_null_dates = wl_clockstops.where(
        wl_clockstops.referral_to_treatment_period_end_date.is_not_null()
    )

dataset.count_null_ref_id = null_dates.pseudo_referral_identifier.count_distinct_for_patient()
dataset.count_not_null_ref_id = null_dates.pseudo_referral_identifier.count_distinct_for_patient()

dataset.define_population(patients.age_on("2021-05-01").is_not_null())

