### VITADEK SCENARIOS
### .. RData files
### 18/06/2018

## AGE CLASSES
age_cat <- c("(2,6]", "(6,10]", "(10,14]", "(14,17]", "(17,39]", "(39,64]")
nage <- length(age_cat)

## POPULATION DATA
pop_df <- readxl("../../01_data/population.xlsx")
pop_age_abs <-
  c(sum(pop_df[ 4: 7, 2:3]),
    sum(pop_df[ 8:11, 2:3]),
    sum(pop_df[12:15, 2:3]),
    sum(pop_df[16:18, 2:3]),
    sum(pop_df[19:40, 2:3]),
    sum(pop_df[41:65, 2:3]))
pop_age_rel <- prop.table(pop_age_abs)

## READ DATA
read_dta <-
function(g) {
  ## load RData file
  load(paste0("../../01_data/RDATA-20180607/", g))

  ## sort data frames by age
  HI_vitd  <- HI_vitd[order(HI_vitd$age), ]
  HI_enerc <- HI_enerc[order(HI_enerc$age), ]

  ## check if ages match
  if (!all(HI_vitd$age == HI_enerc$age))
    stop("Not all ages match in ", g)

  ## return data frames
  data.frame(age = HI_vitd$age,
             cat = cut(HI_vitd$age, c(2, 6, 10, 14, 17, 39, 64)),
             bsl = HI_vitd$hi,
             cns = HI_enerc$hi)
}

## DO FORTIFICATION
do_fort <-
function(x, f) {
  data.frame(AGE = x$cat, TOTAL = x$bsl + x$cns * f)
}

## make summary table
tab <-
function(m, rownames) {
  xfit <- do_fort(dta, m)
  prev <- evaluate_age(xfit)
  out <-
    cbind(P05 = with(xfit, tapply(TOTAL, AGE, quantile, 0.05)),
          P25 = with(xfit, tapply(TOTAL, AGE, quantile, 0.25)),
          P50 = with(xfit, tapply(TOTAL, AGE, median)),
          P95 = with(xfit, tapply(TOTAL, AGE, quantile, 0.95)),
          def = prev[, 1],
          exc = prev[, 2],
          EAR = ear,
          AI = lwr,
          UL = upr)
  rownames(out) <- rownames

  ## add weighted average
  out <-
    rbind(out,
          apply(out, 2, weighted.mean, w = pop_age_rel))
  rownames(out)[nage+1] <- "WAVG"

  ## return result
  return(out)
}

## format tables
Kable <-
function(...) {
  kable(format = "html", ...)%>%
    kable_styling(bootstrap_options = "striped",
                  full_width = F,
                  position = "left")
}


### OPTIMIZATION 1 / minimize deficiencies and excesses

## calculate deficiency/excess prevalence per age class
evaluate_age <-
function(x) {
  # define exc/def per individual
  DEF <- x$TOTAL < ear[x$AGE]
  EXC <- x$TOTAL > upr[x$AGE]

  # calculate prevalence per age class
  cbind(tapply(DEF, x$AGE, mean), tapply(EXC, x$AGE, mean))
}

## calculate deficiency/excess prevalence for total pop
evaluate_pop <-
function(x) {
  # weighted mean of age-specific def/exc
  eval <- evaluate_age(x)
  apply(eval, 2, weighted.mean, w = pop_age_rel)
}

## optimize fortification level across age classes
optim_age <-
function(x) {
  sum(evaluate_age(do_fort(dta, x)))
}

## optimize fortification level for total pop
optim_pop <-
function(x) {
  sum(evaluate_pop(do_fort(dta, x)))
}

## optimization plot (EAR/UL)
optim_plot <-
function(.f, .sc, .nage, .age_cat, .v) {
  col <- brewer.pal(.nage, "Set2")
  layout(matrix(c(1, 2), 2, 1, byrow = TRUE), heights = c(1, 4))

  ## .. plot overall deviance
  par(mar = c(0, 4, 0.5, 8))
  plot(.f, colSums(.sc),
       type = "l", axes = FALSE, ylim = c(0, 6),
       ylab = expression(delta+epsilon))
  box()
  axis(2, las = 1, at = c(0, 2, 4, 6))

  ## .. plot curves
  par(mar = c(4, 4, 0.5, 8))
  matplot(.f, t(.sc), ylim = c(0, 1),
          axes = FALSE, type = "l", col = col, lty = rep(1:2, each = .nage),
          xlab = "Level of fortification (µg/kcal)", ylab = "Prevalence")
  box()
  axis(1)
  ytck <- c(0, 0.2, 0.4, 0.6, 0.8, 1)
  axis(2, las = 1, at = ytck, lab = paste0(100*ytck,"%"))
  legend("topright", legend = c("%<EAR", "%>UL"), 
         inset = c(-0.23, 0), lty = 1:2, cex = .9, xpd = TRUE)
  legend("topright", legend = .age_cat, title = "age group",
         inset = c(-0.22, 0.2), lty = 1, cex = .9, col = col, xpd = TRUE)

  ## .. add optima
  abline(v = .v)
}

## main wrapper function
run <-
function(g) {
  ## write header
  cat(sprintf("<h2>%s</h2>\n", g))

  ## load simulations
  dta <<- read_dta(g)

  ## find optima
  fit_pop <- optimize(optim_pop, c(0, 1))
  fit_age <- optimize(optim_age, c(0, 1))

  ## simulate fortification levels
  f <- seq(0, ceiling(fit_pop$minimum), 0.01)
  sc <- sapply(f, function(x) evaluate_age(do_fort(dta, x)))
  sc_avg_def <-
    apply(sc[seq(1,nage), ], 2, weighted.mean, w = pop_age_rel)
  sc_avg_exc <-
    apply(sc[nage+seq(1,nage), ], 2, weighted.mean, w = pop_age_rel)

  ## make plot
  optim_plot(f, sc, nage, age_cat, fit_age$minimum)

  ## save plot
  tiff(sprintf("plots/%s.tiff", gsub(".RData", "", g)),
       width = 8, height = 6, res = 300, units = "in", compress = "lzw")
  optim_plot(f, sc, nage, age_cat, fit_age$minimum)
  dev.off()

  ## population estimate
  cat("\n  ")
  cat("<h4>Population</h4>")
  print(Kable(unlist(fit_pop)))
  print(Kable(tab(fit_pop$minimum, age_cat), digits = 3))

  ## age group estimate
  cat("\n  ")
  cat("<h4>Age specific</h4>")
  print(Kable(unlist(fit_age)))
  print(Kable(tab(fit_age$minimum, age_cat), digits = 3))

  ## final line break
  cat("\n  ")

  ## remove global objects
  rm(dta, envir = .GlobalEnv)

  ## return fit results
  return(c(unlist(fit_pop), unlist(fit_age)))
}


### OPTIMIZATION 2 / median to AI

## calculate deviance from acceptable intake
## .. deficiency : P50 as close to lwr as possible
dev_ai_age <-
function(x) {
  # calculate median per age group
  # abs value of difference (median-lwr)
  abs(tapply(x$TOTAL, x$AGE, median) - lwr)
}

dev_ai_pop <-
function(x) {
  # weighted mean of deviances
  dev_ai_age <- dev_ai_age(x)
  weighted.mean(dev_ai_age, w = pop_age_rel)
}

optim2_age <-
function(x) {
  # sum of deviances
  sum(dev_ai_age(do_fort(dta, x)))
}

optim2_pop <-
function(x) {
  # weighted mean of deviances
  dev_ai_pop(do_fort(dta, x))
}


## main wrapper function
run2 <-
function(g) {
  ## write header
  cat(sprintf("<h2>%s</h2>\n", g))

  ## load simulations
  dta <<- read_dta(g)

  ## find optima
  fit_pop <- optimize(optim2_pop, c(0, 1))
  fit_age <- optimize(optim2_age, c(0, 1))

  ## simulate fortification levels
  f <- seq(0, ceiling(fit_pop$minimum), 0.01)
  sc <- sapply(f, function(x) dev_ai_age(do_fort(dta, x)))
  dev_age <- apply(sc, 2, function(x) abs(x - lwr))
  dev_pop <- apply(dev_age, 2, weighted.mean, w = pop_age_rel)

  ## make plot
  col <- brewer.pal(nage, "Set2")
  par(mfrow = c(1,2))

  ## .. medians
  matplot(f, t(sc),
          las = 1, type = "l", col = col, lty = 1,
          xlab = "Level of fortification", ylab = "Median")
  legend("topleft", legend = age_cat, lty = 1, col = col, cex = .8)
  abline(h = 15)
  abline(v = fit_pop$minimum)
  abline(v = fit_age$minimum)

  ## .. deviances
  matplot(f, t(dev_age),
          las = 1, type = "l", col = col, lty = 1,
          xlab = "Level of fortification", ylab = "abs(P50-AI)")
  legend("topleft", legend = age_cat, lty = 1, col = col, cex = .8)
  lines(f, dev_pop, lty = 1, lwd = 2, col = "red")
  abline(v = fit_pop$minimum)
  abline(v = fit_age$minimum)

  ## print population estimate
  cat("\n  ")
  cat("<h4>Population</h4>")
  print(Kable(unlist(fit_pop)))
  print(Kable(tab(fit_pop$minimum, age_cat), digits = 3))

  ## print age group estimate
  cat("\n  ")
  cat("<h4>Age specific</h4>")
  print(Kable(unlist(fit_age)))
  print(Kable(tab(fit_age$minimum, age_cat), digits = 3))

  ## final line break
  cat("\n  ")

  ## remove global objects
  rm(dta, envir = .GlobalEnv)

  ## return fit results
  return(c(unlist(fit_pop), unlist(fit_age)))
}

##
##

