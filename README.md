# Training Sign-off: Digital Workflow

Digital replacement for the paper-style sign-off forms in the Staff Training
Framework (Appendix A). Each staff member gets one YAML file per track,
committed to the team repository.

Reference: Training Sign-off Digital Workflow Specification v1.0

## Directory structure

```
docs/training/
├── templates/                    # Blank templates (one per track)
│   ├── core.yaml
│   ├── hydrology_fundamentals.yaml
│   ├── hydraulics_fundamentals.yaml
│   ├── data_coding_fundamentals.yaml
│   ├── cross_cutting.yaml
│   ├── hydrological_modelling.yaml
│   ├── hydraulic_modelling.yaml
│   ├── fmp_crossover.yaml
│   ├── r_development.yaml
│   └── python_ml.yaml
├── R/
│   └── training_audit.R          # Audit and onboarding functions
├── archive/                      # Original .docx forms (if migrating)
├── [staff_name]_[track]_[year].yaml  # Completed sign-offs live here
└── README.md                     # This file
```

## Quick start

### 1. Onboard a new starter

```r
source("R/training_audit.R")

# Create all track files for a new team member
onboard_new_starter(
  template_dir = "templates",
  staff_name   = "J. Smith",
  role         = "Flood Forecasting Officer",
  year         = 2026,
  tracks       = "all",
  output_dir   = "."
)
```

Example output:

```
── Onboarding J. Smith ──────────────────────────────────────────
✔ Created ./j_smith_core_2026.yaml
✔ Created ./j_smith_cross_cutting_2026.yaml
✔ Created ./j_smith_data_coding_fundamentals_2026.yaml
✔ Created ./j_smith_fmp_crossover_2026.yaml
✔ Created ./j_smith_hydraulic_modelling_2026.yaml
✔ Created ./j_smith_hydraulics_fundamentals_2026.yaml
✔ Created ./j_smith_hydrological_modelling_2026.yaml
✔ Created ./j_smith_hydrology_fundamentals_2026.yaml
✔ Created ./j_smith_python_ml_2026.yaml
✔ Created ./j_smith_r_development_2026.yaml
✔ Created 10 sign-off files
```

Or just the tracks they need:

```r
onboard_new_starter(
  template_dir = "templates",
  staff_name   = "J. Smith",
  role         = "Flood Forecasting Officer",
  year         = 2026,
  tracks       = c("core", "hydrology_fundamentals", "cross_cutting",
                    "r_development"),
  output_dir   = "."
)
```

```
── Onboarding J. Smith ──────────────────────────────────────────
✔ Created ./j_smith_core_2026.yaml
✔ Created ./j_smith_hydrology_fundamentals_2026.yaml
✔ Created ./j_smith_cross_cutting_2026.yaml
✔ Created ./j_smith_r_development_2026.yaml
✔ Created 4 sign-off files
```

### 2. Record completion

Open the staff member's YAML file and fill in the `completed` date and
`verified_by` name for each item as they finish it. Commit after each
update.

```yaml
  - id: core_01
    description: Read the Data & Digital Asset Governance Framework v1.3
    section: Governance framework
    completed: 2026-02-10       # <-- date completed
    verified_by: J. Coles       # <-- verifier name
    notes: null
```

### 3. Run the annual audit

```r
source("R/training_audit.R")

report <- training_audit(
  dir = ".",
  tier1_custodians = c("A. Jones", "B. Taylor")
)
```

Example console output:

```
ℹ Reading 5 sign-off files from .
! Tier 1 custodians with NO training files: "B. Taylor"
── Training Audit Summary ───────────────────────────────────────
ℹ 3 staff members across 4 tracks
ℹ 40 / 53 items completed (75.5%)
! 13 incomplete items across all staff
```

### 4. Inspect the report

#### `report$summary`

One row per person per track. Shows total items, how many are done, and
percentage complete.

```r
report$summary
```

```
     staff_name                  track year total_items completed verified incomplete pct_done
1:     A. Jones                   core 2026           9         9        9          0    100.0
2:     A. Jones         r_development 2026          13        13       13          0    100.0
3:     J. Smith                   core 2026           9         6        6          3     66.7
4:     J. Smith hydrology_fundamentals 2026           6         4        4          2     66.7
5:     P. Kumar         cross_cutting 2026          16         8        8          8     50.0
```

#### `report$incomplete`

Every item not yet completed, across all staff.

```r
report$incomplete
```

```
     staff_name                  track      id                                                      description                    section
1:     J. Smith                   core core_03 Can explain Tier 1, Tier 2, and Tier 3 classification ... Governance framework
2:     J. Smith                   core core_04 Understands the Tool Register and Model Register ...      Governance framework
3:     J. Smith                   core core_07 Can explain Q-FAIR and what each principle means ...       Data architecture
4:     J. Smith hydrology_fundamentals hydro_05 Can explain NSE, KGE, and FAR ...                        Data and metrics
5:     J. Smith hydrology_fundamentals hydro_06 Can name the main catchment descriptors ...               Data and metrics
6:     P. Kumar         cross_cutting  xcut_09 Can explain how poor condition propagates ...              Model condition and connections
7:     P. Kumar         cross_cutting  xcut_10 Understands what the Connection Register records ...       Model condition and connections
...
```

#### `report$tier1_check`

Tier 1 custodians who have gaps or no files at all. Empty table means
everyone is up to date.

```r
report$tier1_check
```

```
     staff_name          track gaps
1:    B. Taylor NO FILES FOUND   NA
```

If all Tier 1 custodians are complete the console shows:

```
✔ All Tier 1 custodians have complete training records on file.
```

### 5. Create a single sign-off file

For cases where you just need one file rather than the full onboarding
set:

```r
source("R/training_audit.R")

create_signoff(
  template_path = "templates/r_development.yaml",
  staff_name    = "P. Kumar",
  role          = "Data Engineer",
  year          = 2026,
  output_dir    = "."
)
```

```
✔ Created ./p_kumar_r_development_2026.yaml
```

## Dependencies

R packages: `data.table`, `yaml`, `cli`. All are available on CRAN and
in the team's renv lockfile.

## Governance

This workflow is governed by the Staff Training Framework v1.0 and the
Data and Digital Asset Governance Framework v1.3 (Section 12). YAML
files are version-controlled alongside the code they relate to.

The Steward is responsible for keeping sign-off files current and for
confirming that Tier 1 custodians have complete training before being
listed on the register.
