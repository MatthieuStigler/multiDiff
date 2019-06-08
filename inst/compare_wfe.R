
example(wfe, echo=FALSE)
Data.obs <- as_tibble(Data.obs)
Data.obs

mod.did <- wfe(y~ tr+x1+x2, data = Data.obs, treat = "tr",
               unit.index = "unit", time.index = "time", method = "unit",
               qoi = "ate", estimator ="did", hetero.se=TRUE, auto.se=TRUE,
               White = TRUE, White.alpha = 0.05, verbose = TRUE)

## summarize the results
summary(mod.did)
