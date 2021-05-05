library(gtsummary)
library(tidyverse)


trial %>% 
	select(trt, age, grade) %>% 
	tbl_summary(by = "trt") %>% 
	add_p()

trial %>% 
	select(trt, age, grade) %>% 
	tbl_summary(
		by = trt,
		statistic = list(all_continuous() ~ "{mean} ({sd})",
						 all_categorical() ~ "{n} / {N} ({p}%)"),
		digits = all_continuous() ~ 2,
		label = grade ~ "Tumor Grade",
		missing_text = "(Missing)"
	)

trial %>%
	tbl_cross(
		row = stage,
		col = trt,
		percent = "cell"
	) %>% 
	as_gt() %>%
	gt::as_latex() %>% 
	gsub("^.*?&","&", .) %>% 
	gsub("\\\\[^ ]+$", "", .) %>% 
	cat(file = "table_example.tex")

