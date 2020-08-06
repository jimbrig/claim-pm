# Claims - PM Future Enhancements:
#
# 1) DataPartition training sets for each model by response variables (use subset argument in train).
# 2) Holdout the "2015-11-30" claims as a predict set in train formula.
# 3) Outlier function within the modeling functions.
# 4) Use something other than stepAIC for logistic models feature selections
# 5) Add a probit option for logistic models? (usually dollar data uses probit and biological data uses logit) also cloglog.
# 6) Think of other possible predictor variables that are relevant and not redundant/better ways to extract information
#    from current predictors. 
# 7) Try other classification models (i.e. LDA, baysian, etc.)/regression models 
# 8) Make a data analysis and model diagnostics script for each model with relevant graphs, tables, etc.
# 9) Expand models to model past age 42 and include a tail estimate grouping together all future devt ages.
# 10) Show model assumptions and why we can assume they hold for each model.
# 11) Try other ways of cross validation other than repeatedcv, i.e. bootstrap, etc.
# 12) Look for non-linear relationships and interaction terms that could improve but not overcomplicate models.
# 13) Add provision for new claims (i.e. look at how many new claims come in at each devt and add an estimate for them)
# 14) a way to measure inflation and adjust for inflation/benefit changes. (e.g. linear model with response = paid_incr, predictor = fisccal_year)
# 15) incorporate inflation variation to quantify effect of inflation on future paid_incr
