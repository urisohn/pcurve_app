the p-curve app runs at https://p-curve.com/app

It was written in R in 2017. In 2024 when responding to feedback I put it on github, thus the first commit is from 2024, starting with the app as available in version 4.06.
Future changes will be logged here.

## 4.11 (2026-09-17)
- Fixed half *p*-curve Stouffer when there are zero *p*<.025 results (all significant results in (.025, .05)). Previously R produced `NaN` (0/0), which crashed the PHP front-end on PHP 8+ (`round('NaN')`). Stouffer now returns `NA` in that case; PHP shows N/A for half tests instead of rounding NaN.
