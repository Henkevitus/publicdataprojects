## --------------------------------------------------------------------------------------------------------------------------------------------
#| label: Loading Libraries
#| echo: false
library(Epi)
library(tidyverse)
library(survival)
library(survminer)


## --------------------------------------------------------------------------------------------------------------------------------------------
#| label: loading steno2 data
data("steno2")
data("st2clin")
data("st2alb")


## --------------------------------------------------------------------------------------------------------------------------------------------
#| label: Structure of data-sets
str(steno2)
str(st2clin)
str(st2alb)


## --------------------------------------------------------------------------------------------------------------------------------------------
#| label: unique ID testing further 1

# create a 1 for each time there is an ID
id_test1 <- steno2 |> count(id) 
head(id_test1)

# different numbers on those counts
id_test2 <- id_test1 |>  summarize(n=sum(n), idcount = max(id), sum = sum(id))
id_test2


## --------------------------------------------------------------------------------------------------------------------------------------------
#| label: unique ID testing further 2

st2alb |> count(id) |> summarize(n=sum(n), idcount = max(id), sum = sum(id))
st2clin |> count(id) |> summarize(n=sum(n), idcount = max(id), sum = sum(id))


## --------------------------------------------------------------------------------------------------------------------------------------------
#| label: missing in st2alb



## --------------------------------------------------------------------------------------------------------------------------------------------
#| label: missing from st2alb

anti_join(steno2, st2alb, by = "id")


## --------------------------------------------------------------------------------------------------------------------------------------------
#| label: hidden. no missing from st2clin
#| echo: false

anti_join(steno2, st2clin, by = "id")


## --------------------------------------------------------------------------------------------------------------------------------------------
#| label: steno2 demographics

summary(steno2)



## --------------------------------------------------------------------------------------------------------------------------------------------
#| label: first attempt, going over date vars
plot_histo_spam <- function(df,histovar) {
out <- ggplot(df) + 
  geom_histogram(mapping = aes( {{histovar}} ))

  return(out)
}

plot_histo_spam(steno2,doBth)
plot_histo_spam(steno2,doDM)
plot_histo_spam(steno2,doBase)



## --------------------------------------------------------------------------------------------------------------------------------------------
#| label: second attempt, going over date vars

plot_histospam_faceted <- function(data, prefix = "do") {
  data |> 
    select(starts_with(prefix)) |>             
    pivot_longer(everything()) |> # everything() turns varnames into a column, and their values into a separate column
    ggplot(aes(value)) +
      geom_histogram(colour = "white") +
      facet_wrap(~ name, scales = "free_x") +   # one panel per variable
      labs(x = NULL) 
}

plot_histospam_faceted(steno2)
plot_histospam_faceted(st2clin)
plot_histospam_faceted(st2alb)



## --------------------------------------------------------------------------------------------------------------------------------------------
#| label: histograms, faceted, sex

plot_histospam_faceted_sex <- function(data, prefix = "do") {
  data |> 
    select(sex, starts_with(prefix)) |>             
    pivot_longer(-sex, 
                 names_to = "name",
                 values_to = "value") |> 
    ggplot(aes(value, fill = sex)) +
      geom_histogram(
        position = "dodge",
        colour = "white") +
      facet_wrap(~ name, scales = "free_x") +   # one panel per variable
      labs(x = NULL) 
}

plot_histospam_faceted_sex(steno2)


## --------------------------------------------------------------------------------------------------------------------------------------------
#| label: histograms, faceted, allocation group

plot_histospam_faceted_allo <- function(data, prefix = "do") {
  data |> 
    select(allo, starts_with(prefix)) |>             
    pivot_longer(-allo, 
                 names_to = "name",
                 values_to = "value") |> 
    ggplot(aes(value, fill = allo)) +
      geom_histogram(
        position = "identity",
        alpha    = 0.5,
        colour = "white") +
      facet_wrap(~ name, scales = "free_x") +   # one panel per variable
      labs(x = NULL) 
}

plot_histospam_faceted_allo(steno2)


## --------------------------------------------------------------------------------------------------------------------------------------------
#| label: histograms, faceted, complicated

plot_histospam_faceted_allo_comp <- function(
    data,
    prefix      = "do",
    bins        = 30,
    overlay     = TRUE,     
    facet_rows  = FALSE      
) {
  data |> 
    select(allo, starts_with(prefix)) |>         
    pivot_longer(-allo, names_to = "variable",
                       values_to = "value") |>   
    ggplot(aes(value, fill = allo)) +
      geom_histogram(
        bins      = bins,
        position  = if (overlay) "identity" else "dodge",
        alpha     = if (overlay) 0.5 else 1,      # translucent if overlaid
        colour    = "white"
      ) +
      { if (facet_rows)
          facet_grid(rows = vars(allo), cols = vars(variable),
                     scales = "free_x")
        else
          facet_wrap(~ variable, scales = "free_x")
      } +
      labs(x = NULL, fill = "Sex") +
      theme_minimal()
}

plot_histospam_faceted_allo_comp(steno2, facet_rows = TRUE)



## --------------------------------------------------------------------------------------------------------------------------------------------
#| label: survival setting

steno2 <- steno2 |> 
  mutate(
    doExit = coalesce(doDth, doEnd),
    time_days = difftime(
      coalesce(steno2$doDth, steno2$doEnd), 
      steno2$doBase, 
      units='days'),
    time_yrs = as.numeric(time_days / 365.25),
    event_num = if_else(!is.na(doDth),1,0),
    event = factor(event_num, levels = c(0, 1), labels = c("Censored", "Dead"))
  )

glimpse(steno2)


## --------------------------------------------------------------------------------------------------------------------------------------------
#| label: swimplot followup simple
sp1 <- steno2 |> 
  mutate(id = fct_reorder(as.factor(id), time_yrs)) |>
  ggplot(aes(x = doBase, 
             xend = doExit,
             y = id)) +
  geom_segment(color = "grey") +
  geom_point(aes(x = doExit, shape = event)) +
  geom_point(aes(x = doDth, shape = event)) 
sp1


## --------------------------------------------------------------------------------------------------------------------------------------------
#| label: swimplot followup simple base is 0

sp2 <- steno2 |> 
  mutate(
    id = fct_reorder(as.factor(id), time_yrs),
    t0 = 0) |>
  ggplot(aes(x = t0, 
             xend = time_yrs,
             y = id)) +
  geom_segment(color = "grey") +
  geom_point(aes(x = time_yrs, shape = event, colour = event)) +
  theme_minimal()
sp2


## --------------------------------------------------------------------------------------------------------------------------------------------
#| label: swimplot different shapes for event
sp3 <- sp2 +
  scale_shape_manual(values = c(Censored = 17, Dead = 4)) +
  scale_colour_manual(values = c(Censored = "black", Dead = "red"))  
sp3


## --------------------------------------------------------------------------------------------------------------------------------------------
#| label: swimplot followup final

sp4 <- sp3 +
  labs(
    x = NULL, 
    y = "Subject sorted by follow-up", 
    title = "Follow-up per subject, years since baseline") +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    plot.background = element_blank(),
    panel.background = element_blank()
    )
sp4


## --------------------------------------------------------------------------------------------------------------------------------------------
#| label: surv fit

# Overall, by treatment allocation, subset into sex
km_os <- survfit(Surv(time_yrs, event_num) ~ 1, data = steno2)
km_allo <- survfit(Surv(time_yrs, event_num) ~ allo, data = steno2)
km_sex <- survfit(Surv(time_yrs, event_num) ~ sex, data = steno2)
km_allo_sex <- survfit(Surv(time_yrs, event_num) ~ allo + sex, data = steno2)

# steno2_m <- steno2 |> filter(sex == "M") 
# steno2_f <- steno2 |> filter(sex == "F") 
# km_allo_m <- survfit(Surv(time_yrs, event_num) ~ allo, data = steno2_m)
# km_allo_f <- survfit(Surv(time_yrs, event_num) ~ allo, data = steno2_f)

# Summarising, showing just the first 10 years
summary(km_os, times=c(0:10)) 
summary(km_allo, times=c(0:10)) 
summary(km_sex, times=c(0:10)) 
summary(km_allo_sex, times=c(0:10)) 


## --------------------------------------------------------------------------------------------------------------------------------------------
#| label: survplot KM

# Function to save some space
survplot_function <- function(fit,data){
  out <- ggsurvplot(
    fit, data = data,
    conf.int = TRUE,
    risk.table = TRUE,
    palette = "dark2",
    xlab = NULL, ylab = "Survival Probability",
    title = "Kaplan Meier Estimator for survival probability, in years"
    )
  
  return(out)
}

# Overall
survplot_function(fit_os,steno2)

# allo
survplot_function(fit_allo,steno2)

# sex
survplot_function(fit_sex,steno2)

# allo sex
survplot_function(fit_allo_sex,steno2)



## --------------------------------------------------------------------------------------------------------------------------------------------
#| label: survplot KM pretty, fewer

# allo
surv_p_allo_pretty <- ggsurvplot(
  fit = fit_allo, data = steno2,
  surv.median.line = "hv", # Add medians survival
  legend.title = "Allocation group",
  legend.labs = c("Intervention", "Conventional"),
  pval = TRUE,
  conf.int = TRUE,

  risk.table = TRUE,
  tables.height = 0.2,
  tables.theme = theme(axis.title.y = element_blank()), 
  
  palette = "dark2",
  ggtheme = theme_bw(),
  xlab = NULL, ylab = "Survival Probability",
  title = "Kaplan Meier Estimator for survival probability, in years"
  )

surv_p_allo_sex_pretty <- ggsurvplot(
  fit = fit_allo_sex, data = steno2,
  surv.median.line = "hv", # Add medians survival
  legend.title = "Allocation group, Sex",
  legend.labs = c("Int, F", "Int, M", "Conv, F", "Conv, M"),
  conf.int = TRUE,

  risk.table = TRUE,
  tables.height = 0.2,
  tables.theme = theme(axis.title.y = element_blank()), 
  
  palette = "dark2",
  ggtheme = theme_bw(),
  xlab = NULL, ylab = "Survival Probability",
  title = "Kaplan Meier Estimator for survival probability, in years"
  )

surv_p_allo_pretty
surv_p_allo_sex_pretty


## --------------------------------------------------------------------------------------------------------------------------------------------
#| label: Log-rank test
survdiff(Surv(time_yrs, event_num) ~ allo, data = steno2)
survdiff(Surv(time_yrs, event_num) ~ sex, data = steno2)


## --------------------------------------------------------------------------------------------------------------------------------------------
#| label: cox models

# Overall, by treatment allocation, subset into sex
cox_allo <- coxph(Surv(time_yrs, event_num) ~ allo, data = steno2)
cox_sex <- coxph(Surv(time_yrs, event_num) ~ sex, data = steno2)
cox_allo_sex <- coxph(Surv(time_yrs, event_num) ~ allo + sex, data = steno2)


## --------------------------------------------------------------------------------------------------------------------------------------------
#| label: observed versus estimated, loglog plot

plot(km_allo,
     col=1:2,
     lty=2,
     main='Observed and fitted survival')
lines(survfit(cox_allo, 
              newdata=data.frame(allo=c('Int','Conv'))),
      col=1:2)
legend('bottomleft',
       lty=rep(1:2,2),col=rep(1:2,each=2),
       legend=c('Int, fit','Int, obs','Conv, fit','Conv, obs'))

plot(km_allo,
     col=1:2,
     fun='cloglog',
     main='Log-log plot')
legend('topleft',lty=rep(1,2),col=1:2,legend=levels(steno2$allo))


## --------------------------------------------------------------------------------------------------------------------------------------------
#| label: summarising models

# list 
cox_models <- list(cox_allo,cox_sex,cox_allo_sex)
names(cox_models) <- paste0("model_", seq_along(cox_models))

# tidy 
tidy_cox_models <- map_dfr(cox_models, ~broom::tidy(.x, conf.int = TRUE))
tidy_cox_models
tidy_cox_models_exp <- map_dfr(cox_models, ~broom::tidy(.x, conf.int = TRUE, exponentiate = TRUE))
tidy_cox_models_exp


## --------------------------------------------------------------------------------------------------------------------------------------------
#| label: model estimates table

cox_grid_1 <- expand.grid(allo = c("Conv", "Int")) 
cox_grid_2 <- expand.grid(allo = c("Conv", "Int"), sex = c("M","F")) 
(broom::augment(cox_allo,newdata = cox_grid_1, type.predict = "risk"))
(broom::augment(cox_allo_sex,newdata = cox_grid_2, type.predict = "risk"))

