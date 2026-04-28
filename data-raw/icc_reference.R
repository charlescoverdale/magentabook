# Build inst/extdata/icc_reference.csv
#
# Source: Hedges & Hedberg (2007), "Intraclass Correlation Values for
# Planning Group-Randomized Trials in Education", Educational
# Evaluation and Policy Analysis 29(1); Adams et al. (2004),
# "Patterns of intra-cluster correlation from primary care research",
# Statistics in Medicine 23; Campbell et al. (2000) BMJ; UK
# Department for Education / EEF guidance; DWP and MHCLG impact
# evaluations; MoJ Justice Data Lab.
#
# All values are *reference* ICCs for planning purposes only. The
# specific evaluation context should compute its own ICC from
# baseline data wherever feasible.

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
  icc_low = c(
    0.10, 0.10, 0.05, 0.15,
    0.01, 0.02, 0.02,
    0.02, 0.03,
    0.03, 0.05,
    0.02, 0.05,
    0.04
  ),
  icc_central = c(
    0.15, 0.13, 0.08, 0.20,
    0.05, 0.04, 0.03,
    0.04, 0.06,
    0.06, 0.08,
    0.04, 0.10,
    0.07
  ),
  icc_high = c(
    0.20, 0.18, 0.12, 0.25,
    0.10, 0.08, 0.05,
    0.08, 0.10,
    0.10, 0.12,
    0.07, 0.15,
    0.12
  ),
  source = c(
    "Hedges & Hedberg (2007), US data; Adams et al. UK BPRS",
    "Hedges & Hedberg (2007); Tymms et al. UK",
    "EEF guidance; UK Department for Education",
    "Hedges & Hedberg (2007)",
    "Adams et al. (2004); Campbell et al. (2000)",
    "Adams et al. (2004)",
    "Campbell et al. (2000)",
    "DWP impact evaluations",
    "DWP impact evaluations",
    "MHCLG / Cabinet Office",
    "MHCLG / Cabinet Office",
    "MoJ Justice Data Lab",
    "MoJ administrative data",
    "MHCLG impact evaluations"
  ),
  notes = c(
    "Pupil-level outcomes within primary schools",
    "Pupil-level outcomes within secondary schools",
    "Pupil-level attendance within schools",
    "Pupil-level outcomes within classrooms",
    "Patient-level outcomes within GP practices",
    "Patient-level satisfaction within GP practices",
    "Hypertension management within GP practices",
    "Claimant-level outcomes within Jobcentre Plus offices",
    "Claimant wage outcomes within Jobcentres",
    "Resident-level outcomes within local authorities",
    "Resident satisfaction within local authorities",
    "Offender-level reoffending within probation areas",
    "Case-level disposal time within courts",
    "Tenant-level outcomes within housing associations"
  ),
  stringsAsFactors = FALSE
)

utils::write.csv(
  icc_reference,
  file = "inst/extdata/icc_reference.csv",
  row.names = FALSE
)
