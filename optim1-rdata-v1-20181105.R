#' ---
#' title: VITADEK SCENARIOS / RDATA
#' subtitle: OPTIMIZATION 1 / minimize deficiencies and excesses
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

#+ warning=FALSE, results='asis', fig.width=8
out <- matrix(nrow = length(g), ncol = 4)
rownames(out) <- g
colnames(out) <- c("pop.fort", "pop.dev", "age.fort", "age.dev")
for (i in seq_along(g)) {
  out[i, ] <- run(g[i])
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
  prev <- evaluate_age(xfit)
  res <- c(res, -prev[, 1], prev[, 2])
}

rownames(out) <- gsub(".RData", "", gsub(".*_", "", rownames(out)))

##
## AGE SPECIFIC
##

df <-
  data.frame(
    value = res,
    age = factor(rep(seq(nage), times = 2*nrow(out))),
    par = rep(rep(c("def", "exc"), each = nage), times = nrow(out)),
    group = rep(rownames(out), each = 2*nage))

ord <- order(with(df, tapply(value, group, function(x) sum(abs(x)))))
df$group <- factor(df$group, levels(df$group)[rev(ord)])

xrange <-
  c(floor(min(with(subset(df, par == "def"), tapply(value, group, sum)))),
    ceiling(max(with(subset(df, par == "exc"), tapply(value, group, sum)))))
xbreaks <- seq(xrange[1], xrange[2])

p1 <-
ggplot(df, aes(y = value, x = group, group = par)) +
  geom_col(aes(fill = age)) +
  coord_flip() +
  theme_bw() +
  geom_hline(yintercept = 0) +
  scale_x_discrete(NULL) +
  scale_y_continuous("",
                     breaks = xbreaks,
                     labels = abs(xbreaks),
                     limits = range(xbreaks),
                     expand = c(0, diff(xbreaks)[1]/10)) +
  scale_fill_manual("Age group",
                    values = brewer.pal(nage, "Set2"),
                    labels = age_cat)
print(p1)
grid::grid.text("% excess", x = unit(0.76, "npc"), y = unit(0.02, "npc"))
grid::grid.text("% inadequate", x = unit(0.395, "npc"), y = unit(0.02, "npc"))

tiff("pyramid-optim1-age.tiff",
     width = 6, height = 8, units = "in", res = 300, compress = "lzw")
print(p1)
grid::grid.text("% excess", x = unit(0.76, "npc"), y = unit(0.02, "npc"))
grid::grid.text("% inadequate", x = unit(0.395, "npc"), y = unit(0.02, "npc"))
dev.off()


##
## POPULATION WEIGHTED
##

df2 <- df
df2$value <- df2$value * pop_age_rel

ord <- order(with(df2, tapply(value, group, function(x) sum(abs(x)))))
df2$group <- factor(df2$group, levels(df2$group)[rev(ord)])

xrange <-
  c(floor(min(10*with(subset(df2, par == "def"), tapply(value, group, sum)))),
    ceiling(max(10*with(subset(df2, par == "exc"), tapply(value, group, sum)))))
xbreaks <- seq(xrange[1], xrange[2]) / 10

p2 <-
ggplot(df2, aes(y = value, x = group, group = par)) +
  geom_col(aes(fill = age)) +
  coord_flip() +
  theme_bw() +
  geom_hline(yintercept = 0) +
  scale_x_discrete(NULL) +
  scale_y_continuous("",
                     breaks = xbreaks,
                     labels = abs(xbreaks),
                     limits = range(xbreaks),
                     expand = c(0, diff(xbreaks)[1]/10)) +
  scale_fill_manual("Age group",
                    values = brewer.pal(nage, "Set2"),
                    labels = age_cat)
  
print(p2)
grid::grid.text("% excess", x = unit(0.8, "npc"), y = unit(0.02, "npc"))
grid::grid.text("% inadequate", x = unit(0.435, "npc"), y = unit(0.02, "npc"))

tiff("pyramid-optim1-pop.tiff",
     width = 6, height = 8, units = "in", res = 300, compress = "lzw")
print(p2)
grid::grid.text("% excess", x = unit(0.8, "npc"), y = unit(0.02, "npc"))
grid::grid.text("% inadequate", x = unit(0.435, "npc"), y = unit(0.02, "npc"))
dev.off()

##
##

#' # R session info
sessionInfo()

#rmarkdown::render("optim1-rdata-v1-20181105.R")