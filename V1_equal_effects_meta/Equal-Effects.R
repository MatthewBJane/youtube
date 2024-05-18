
url_string = "https://raw.githubusercontent.com/MatthewBJane/youtube/main/V1_equal_effects_meta/vocab_data.csv"
df <- read.csv(url(url_string))

head(df)


library(metafor)

df_meta <- escalc(data = df,
                  measure = "MD",
                  m1i = mean1, sd1i = sd1, n1i = n1, # College
                  m2i = mean0, sd2i = sd0, n2i = n0, # Non-College
                  var.names=c("Di","vi"))

head(df_meta,4)


mdl <- rma(data = df_meta,
           yi = Di,
           vi = vi,
           method = "EE")

coef(summary(mdl))




# Expected variance of mean differences
weighted.mean(x = df_meta$vi,
              w = 1/df_meta$vi)

# Observed variance of mean differences
weighted.mean(x = (df_meta$Di-mdl$b[1])^2,
              w = 1/df_meta$vi)

# Test for sig difference
data.frame(Q = mdl$QE,
           p = mdl$QEp)
