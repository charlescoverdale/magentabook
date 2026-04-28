## Submission summary

Resubmission of magentabook 0.1.0 after pre-test feedback from
2026-04-28. Three issues from the previous attempt have been fixed:

1. **Vignette ERROR on Windows**: `cost-effectiveness-with-greenbook.Rmd`
   has been rewritten so it does not call `greenbook` at evaluation
   time. The vignette now does all real computation with manual
   discount-factor arithmetic. The `greenbook` integration is shown
   as `eval = FALSE` code blocks for documentation only.

2. **`greenbook` in Suggests but not on a mainstream repository**:
   `greenbook` has been removed from Suggests. The vignette no longer
   needs it; documentation comments referencing the companion package
   remain (this is OK because they do not trigger CRAN's
   suggested-package check).

3. **Possibly mis-spelled words in DESCRIPTION**: author surnames
   (`Hussey`, `Hemming`, `Bernal`, `Cameron`, `Miller`, `Stuart`) are
   now wrapped in single quotes per CRAN convention. `logframe` has
   been changed to `log-frame`. The remaining `et`, `al.`, `pre`,
   `pre-treatment` are not mis-spelled in evaluation-methods context;
   `et al.` is the standard academic abbreviation, and `pre-treatment`
   appears in the technical sense of pre-treatment covariate balance.

## Test environments

* local macOS, R 4.5.x: 0 errors, 0 warnings, 0 notes
* GitHub `urlchecker::url_check()`: only the magentabook CRAN-status
  badge URL is flagged 404 (universally tolerated; resolves once the
  package is on CRAN)
* GitHub `spelling::spell_check_package()`: 0 errors

## R CMD check results

0 errors | 0 warnings | 0 notes (locally)

## Cross-validation

The arithmetic primitives are cross-validated against canonical
reference implementations on every R CMD check (when the optional
packages are installed): `pwr` for power and sample size,
`sandwich` for cluster-robust SEs, `swCRTdesign` for stepped-wedge
designs, `BCEA` for cost-effectiveness, and `cobalt` for balance
diagnostics. See the `tests/testthat/test-*-equivalence.R` files.

## Reverse dependencies

None (first submission).
