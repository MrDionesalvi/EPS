

matrix(c(54,41,70,17,12,51,3,5), nrow =4, ncol=2) ->fratture

chisq.test(fratture) -> chi2test
chi2test$expected
chi2test$observed
chi2test$residuals

####

dimissioni <- matrix(c(622,631,3997,4660), nrow =2, ncol=2)
chisq.test(dimissioni)
