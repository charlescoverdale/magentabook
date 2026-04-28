# Build inst/extdata/icc_reference.csv
#
# Reference intra-class correlation coefficients for cluster-RCT
# planning across UK policy domains.
#
# Honesty rule: every row carries a `value_source` flag.
#   "table_quote"      = direct extraction of a specific row/value
#                        from a published table (cite table number)
#   "central_estimate" = researcher synthesis of a plausible central
#                        value within the published range, used as a
#                        practitioner default in the absence of
#                        domain-specific baseline data
#
# Sources:
# - Hedges & Hedberg (2007). Intraclass Correlation Values for
#   Planning Group-Randomized Trials in Education. Educational
#   Evaluation and Policy Analysis 29(1).
# - Adams, Gulliford, Ukoumunne, Eldridge, Chinn & Campbell (2004).
#   Patterns of intra-cluster correlation from primary care
#   research to inform study design and analysis. Statistics in
#   Medicine 23.
# - Campbell, Mollison & Grimshaw (2000). Cluster trials in
#   implementation research: estimation of intracluster correlation
#   coefficients and sample size. BMJ.
# - UK Department for Education / EEF guidance documents.
# - DWP, MHCLG, MoJ impact-evaluation reports.
#
# All current rows are central_estimate. To upgrade a row to
# table_quote, replace the value with the exact figure from the
# source table and set value_source = "table_quote" with the table
# number in the source field.

icc_reference <- data.frame(
  domain = c(
    "education", "education", "education", "education",
    "health", "health", "health",
    "employment", "employment",
    "local_government", "local_government",
    "criminal_justice", "criminal_justice",
    "housing"
  ),
  outcome = c(
    "attainment_primary", "attainment_secondary", "attendance", "attainment_classroom",
    "clinical_outcome", "patient_satisfaction", "bp_management",
    "job_entry", "wage",
    "service_use", "satisfaction",
    "reoffending", "case_disposal_time",
    "tenancy_sustainment"
  ),
  unit_of_clustering = c(
    "school", "school", "school", "classroom",
    "gp_practice", "gp_practice", "gp_practice",
    "jobcentre", "jobcentre",
    "local_authority", "local_authority",
    "probation_office", "court",
    "housing_association"
  ),
  icc_low     = c(0.10, 0.10, 0.05, 0.15, 0.01, 0.02, 0.02, 0.02, 0.03,
                  0.03, 0.05, 0.02, 0.05, 0.04),
  icc_central = c(0.15, 0.13, 0.08, 0.20, 0.05, 0.04, 0.03, 0.04, 0.06,
                  0.06, 0.08, 0.04, 0.10, 0.07),
  icc_high    = c(0.20, 0.18, 0.12, 0.25, 0.10, 0.08, 0.05, 0.08, 0.10,
                  0.10, 0.12, 0.07, 0.15, 0.12),
  value_source = "central_estimate",
  source = c(
    "Hedges & Hedberg (2007) Table 2 reports school-level ICCs for US reading/maths in the range 0.10-0.25; Adams et al. (2004) corroborate similar magnitudes for UK BPRS data",
    "Hedges & Hedberg (2007) Table 2; Tymms et al. UK NFER cohort data",
    "EEF guidance documents; UK Department for Education impact evaluations",
    "Hedges & Hedberg (2007) Table 2 (classroom-level)",
    "Adams et al. (2004) Statistics in Medicine 23 reports primary-care ICCs in the range 0.005-0.05; Campbell et al. (2000) BMJ corroborates",
    "Adams et al. (2004)",
    "Campbell et al. (2000)",
    "DWP impact evaluations (synthesis across multiple programmes)",
    "DWP impact evaluations",
    "MHCLG / Cabinet Office published evaluations",
    "MHCLG / Cabinet Office",
    "MoJ Justice Data Lab",
    "MoJ administrative data",
    "MHCLG impact evaluations"
  ),
  notes = c(
    "Pupil-level outcomes within primary schools; central value is researcher synthesis within the published range",
    "Pupil-level outcomes within secondary schools; central value is researcher synthesis",
    "Pupil-level attendance within schools; central value is researcher synthesis",
    "Pupil-level outcomes within classrooms; central value is researcher synthesis",
    "Patient-level outcomes within GP practices; central value is researcher synthesis",
    "Patient-level satisfaction within GP practices; central value is researcher synthesis",
    "Hypertension management within GP practices; central value is researcher synthesis",
    "Claimant-level outcomes within Jobcentre Plus offices; central value is researcher synthesis",
    "Claimant wage outcomes within Jobcentres; central value is researcher synthesis",
    "Resident-level outcomes within local authorities; central value is researcher synthesis",
    "Resident satisfaction within local authorities; central value is researcher synthesis",
    "Offender-level reoffending within probation areas; central value is researcher synthesis",
    "Case-level disposal time within courts; central value is researcher synthesis",
    "Tenant-level outcomes within housing associations; central value is researcher synthesis"
  ),
  stringsAsFactors = FALSE
)

utils::write.csv(
  icc_reference,
  file = "inst/extdata/icc_reference.csv",
  row.names = FALSE
)
