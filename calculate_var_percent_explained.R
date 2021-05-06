af <- anova(iafm_full_model_99)
afss <- af$"Sum Sq"
print(cbind(af,PctExp=afss/sum(afss)*100))
