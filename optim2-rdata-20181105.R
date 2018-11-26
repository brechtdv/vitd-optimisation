#' ---
#' title: VITADEK SCENARIOS / RDATA
#' subtitle: OPTIMIZATION 2 / median to AI
#' output:
#'   html_document:
#'     toc: true
#' ---

## required packages
library(bd)
library(DT)
library(ggplot2)
library(haven)
library(kableExtra)
library(knitr)
library(RColorBrewer)

#+ warning=FALSE
## load helper functions
source("scenarios-helpers-rdata.R")

## define thresholds / 6 values
ear <- c(10, 10,  10,  10,  10,  10)
lwr <- c(15, 15,  15,  15,  15,  15)
upr <- c(50, 50, 100, 100, 100, 100)

## define food groups / general population
g <- dir("../../01_data/RDATA-20180607")

#+ warning=FALSE, results='asis', fig.width=10
out <- matrix(nrow = length(g), ncol = 4)
rownames(out) <- g
colnames(out) <- c("pop.fort", "pop.dev", "age.fort", "age.dev")
for (i in seq_along(g)) {
  out[i, ] <- run2(g[i])
}


#' # SUMMARY
datatable(out, options = list(pageLength = 100))


#' # PYRAMID PLOT
#+ warning=FALSE, fig.height=8

res <- numeric()

for (i in seq(nrow(out))) {
  ## food group
  g <- rownames(out)[i]

  ## read data
  dtai <- read_dta(g)

  ## combine baseline and fortification
  xfit <- do_fort(dtai, out[i, "age.fort"])
  dev <- dev_ai_age(xfit)
  res <- c(res, dev)
}

rownames(out) <- gsub(".RData", "", gsub(".*_", "", rownames(out)))

##
## AGE SPECIFIC
##

df <-
  data.frame(
    value = res,
    age = factor(rep(seq(nage), times = nrow(out))),
    group = rep(rownames(out), each = nage))

ord <- order(with(df, tapply(value, group, function(x) sum(abs(x)))))
df$group <- factor(df$group, levels(df$group)[rev(ord)])

p1 <-
ggplot(df, aes(y = value, x = group)) +
  geom_col(aes(fill = age)) +
  coord_flip() +
  theme_bw() +
  scale_y_continuous("Absolute deviance") +
  scale_x_discrete(NULL) +
  scale_fill_manual("Age group",
                    values = brewer.pal(nage, "Set2"),
                    labels = age_cat)
print(p1)

tiff("pyramid-optim2-age.tiff",
     width = 6, height = 8, units = "in", res = 300, compress = "lzw")
print(p1)
graphics.off()


##
## POPULATION WEIGHTED
##

df2 <- df
df2$value <- df2$value * pop_age_rel

ord <- order(with(df2, tapply(value, group, function(x) sum(abs(x)))))
df2$group <- factor(df2$group, levels(df2$group)[rev(ord)])

p2 <-
ggplot(df2, aes(y = value, x = group)) +
  geom_col(aes(fill = age)) +
  coord_flip() +
  theme_bw() +
  scale_y_continuous("Absolute deviance") +
  scale_x_discrete(NULL) +
  scale_fill_manual("Age group",
                    values = brewer.pal(nage, "Set2"),
                    labels = age_cat)
print(p2)

tiff("pyramid-optim2-pop.tiff",
     width = 6, height = 8, units = "in", res = 300, compress = "lzw")
print(p2)
graphics.off()

##
##

#' # R session info
sessionInfo()

#rmarkdown::render("optim2-rdata-20181105.R")
