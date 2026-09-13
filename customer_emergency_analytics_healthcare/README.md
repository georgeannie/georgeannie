# Emergency Transport Decision Support (Healthcare Analytics)

**Business problem:** When a patient needs emergency transport, the choice between ground and air transport is time- and cost-sensitive — the wrong call either delays critical care or spends helicopter-level resources on a case that didn't need it. This project builds the data pipeline and a decision-support interface to inform that call using patient condition and geographic distance to the nearest appropriate facility.

**Approach:**
- Merged and cleaned multi-source incident, prehospital, and patient-ED records (interfacility transport data), including deduplication and completeness handling
- Built a distance function between incident and facility zip codes to quantify transport time as a decision input
- Engineered features onto the patient-ED table to support a predictive model of transport-mode need
- Delivered results through a Shiny interface so non-technical stakeholders (dispatch/clinical staff) could use the output directly, not just view a model score

**Stack:** R, Shiny

**Capstone project of applied analytics**. With more time would productionize it and allow on field ER reps to utilize it to determine the nearest facility based on patient condition