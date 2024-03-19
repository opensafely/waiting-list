###########################################################
# The purpose of this script is to check the distributions
# of admissions by region
###########################################################

from ehrql import INTERVAL, Measures, months
from ehrql.tables.tpp import (
    patients, 
    practice_registrations,
    apcs)


##########

region = practice_registrations.for_patient_on(INTERVAL.start_date).practice_nuts1_region_name

admit_count = apcs.where(
        apcs.admission_date.is_during(INTERVAL)
    ).count_for_patient()

######

measures = Measures()
measures.configure_dummy_data(population_size=5000)

denominator = (        
        (patients.age_on(INTERVAL.start_date) >= 0) 
        & (patients.age_on(INTERVAL.start_date) < 110)
        & (patients.sex.is_in(["male","female"]))
    )

measures.define_measure(
    name = "admit_count",
    numerator = admit_count,
    denominator = denominator,
    intervals=months(36).starting_on("2020-01-01"),
    group_by={"region": region}
    )
