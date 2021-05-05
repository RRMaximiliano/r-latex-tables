
# Packages ----------------------------------------------------------------

library(tidyverse)
library(gtsummary)
library(modelsummary)
library(fixest)
library(gt)
library(haven)


# Load data ---------------------------------------------------------------

census <- read_dta("http://www.stata-press.com/data/r9/census.dta")

# Settings the models
models <- list(
	m1 = lm(death ~ marriage + pop, data = census),
	m2 = lm(death ~ popurban, data = census),
	m3 = lm(divorce ~ marriage + pop, data = census), 
	m4 = lm(divorce ~ marriage + pop + factor(region), data = census)
)

# First table
modelsummary(models, fmt = "%.2f", output = "latex") %>% 
	gsub("^.*?&","&", .) %>% 
	gsub("[\\\\][bottomrule][^ ]+$", "", .) %>% 
	cat(file = here::here("outputs", "tables", "table1.tex"))

# modelsummary(models, fmt = "%.2f", output = "gt") %>% 
# 	as_latex() %>% 
# 	as.character() %>%
# 	str_replace_all(., "longtable", "tabular") %>% 
# 	cat(file = here::here("outputs", "tables", "table1_reduced.tex"))

# Second table
cm <- c("marriage" = "Number of marriages",
		"pop"      = "Population",
		"popurban" = "Urban Population",
		"factor(region)2" = "North Central",
		"factor(region)3" = "South",
		"factor(region)4" = "West",
		"(Intercept)"     = "Constant")

modelsummary(models, fmt = "%.2f", coef_map = cm, output = "latex") %>% 
	gsub("^.*?&","&", .) %>% 
	gsub("[\\\\][bottomrule][^ ]+$", "", .) %>% 
	cat(file = here::here("outputs", "tables", "table2.tex"))

# Third table
names(models) <- c("Number of deaths (1)", "Number of deaths (2)", "Number of divorces (3)", "Number of divorces (4)")

modelsummary(models, fmt = "%.2f", coef_map = cm, output = "latex") %>% 
	gsub("^.*?&","&", .) %>% 
	gsub("[\\\\][bottomrule][^ ]+$", "", .) %>% 
	cat(file = here::here("outputs", "tables", "table3.tex"))

# Fourth table
modelsummary(models, fmt = "%.2f", coef_map = cm, stars = TRUE, estimate = "{estimate}{stars}", output = "latex") %>% 
	gsub("^.*?&","&", .) %>% 
	gsub("[\\\\][bottomrule][^ ]+$", "", .) %>% 
	cat(file = here::here("outputs", "tables", "table4.tex"))

# Fifth table
gm <- tibble::tribble(
	~raw,        ~clean,          ~fmt,
	"nobs",      "N",             0,
	"r.squared", "R-squared",     2)

modelsummary(models, fmt = "%.2f", coef_map = cm, stars = TRUE, estimate = "{estimate}{stars}", gof_map = gm, escape = FALSE, output = "latex") %>% 
	gsub("^.*?&","&", .) %>% 
	gsub("[\\\\][bottomrule][^ ]+$", "", .) %>% 
	cat(file = here::here("outputs", "tables", "table5.tex"))

# Sixth table
names(models) <- c("(1)", "(2)", "(3)", "(4)")

modelsummary(models, fmt = "%.2f", coef_map = cm, stars = TRUE, estimate = "{estimate}{stars}", gof_map = gm, output = "gt") %>% 
	tab_spanner(label = 'Number of deaths', columns = 2:3) %>% 
	tab_spanner(label = 'Number of divorces', columns = 4:5) %>% 
	as_latex() %>% 
	gsub("^.*?&","&", .) %>% 
	gsub("[\\\\][bottomrule][^ ]+$", "", .) %>% 
	cat(file = here::here("outputs", "tables", "table6.tex"))

# Seventh table
gm <- tibble::tribble(
	~raw,        ~clean,          ~fmt,
	"FE: region","Region FE",     0,
	"nobs",      "N",             0,
	"r.squared", "R-squared",     2)

cm <- c("marriage" = "Number of marriages",
		"pop"      = "Population",
		"popurban" = "Urban Population")

mod <- list()
mod[[1]] <- feols(death ~ marriage + pop, data = census)
mod[[2]] <- feols(death ~ popurban, data = census)
mod[[3]] <- feols(divorce ~ marriage + pop, data = census)
mod[[4]] <- feols(divorce ~ marriage + pop | region, data = census)
names(mod) <- c("(1)", "(2)", "(3)", "(4)")

modelsummary(mod, fmt = "%.2f", coef_map = cm, stars = TRUE, estimate = "{estimate}{stars}", gof_map = gm, output = "gt") %>% 
	tab_spanner(label = 'Number of deaths', columns = 2:3) %>% 
	tab_spanner(label = 'Number of divorces', columns = 4:5) %>% 
	as_latex() %>% 
	gsub("^.*?&","&", .) %>% 
	gsub("[\\\\][bottomrule][^ ]+$", "", .) %>% 
	cat(file = here::here("outputs", "tables", "table7.tex"))
