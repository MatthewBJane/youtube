
library(metafor)

head(dat.viechtbauer2021)


df <- escalc(data = dat.viechtbauer2021,
             measure = "RR",
             ai = xTi, n1i = nTi,
             ci = xCi, n2i = nCi,
             var.names = c("logRR","var_logRR"))

head(df)


mdl <- rma(data = df,
           yi = logRR,
           vi = var_logRR,
           method = "REML")

coef(summary(mdl))

forest(mdl,header = TRUE, refline = 0)

forest(mdl,transf = exp,header = TRUE, refline = 1)



mdl <- update(mdl, mods = ~ dose)

regplot(mdl, pi=TRUE, shade = FALSE, bg = "transparent")

regplot(mdl, pi=TRUE, transf = exp, shade = FALSE, bg = "transparent")
