library(crssplot)
library(dplyr)
ofile <- "data-raw/sample_figures.pdf"

pdf(ofile, width = 9, height = 6)

# scens_plot_range() -----------------------------------------
print(scens_plot_range(ex_pe, "mead_dec_pe"))
print(scens_plot_range(ex_pe, "mead_dec_pe", y_lab = "feet"))
print(scens_plot_range(ex_pe, c("powell_dec_pe", "mead_dec_pe"),
                       facet_scales = "free_y"))
print(scens_plot_range(ex_pe, "mead_dec_pe", scenarios = "April ST CT"))
pc <- c("April ST CT" = "red", "April ST 2007 UCRC" = "black")
sl <- c("April ST CT" = "s1", "April ST 2007 UCRC" = "s2")
print(scens_plot_range(ex_pe, "powell_dec_pe", plot_colors = pc, 
                       scen_labels = sl, title = "PE", 
                       caption = "this is a caption"))
print(scens_plot_range(ex_pe, c("powell_dec_pe", "mead_dec_pe"), 
                       facet_scales = "free_y",
           facet_nrow = 2))
print(scens_plot_range(ex_pe, c("powell_dec_pe", "mead_dec_pe"), 
                       facet_scales = "free_y",
     facet_ncol = 2))

# scens_plot_probs() -----------------------------------------
print(scens_plot_probs(ex_pe, "mead_min_lt_1000"))
print(scens_plot_probs(ex_pe, "powell_wy_min_lt_3525", y_lab = "percent"))

print(scens_plot_probs(
  ex_pe, 
  c("mead_min_lt_1020", "powell_wy_min_lt_3490"), 
  facet_scales = "free_y"
))

print(scens_plot_probs(ex_pe, "mead_dec_lt_1025", scenarios = "April ST CT"))

pc <- c("April ST CT" = "red", "April ST 2007 UCRC" = "black")
sl <- c("April ST CT" = "s1", "April ST 2007 UCRC" = "s2")

print(scens_plot_probs(
  ex_pe, 
  "mead_dec_lt_1025", 
  plot_colors = pc, 
  scen_labels = sl,
  title = "Mead December < 1,025'", 
  caption = "this is a caption"
))

print(scens_plot_probs(
  ex_pe, 
  c("mead_dec_lt_1025", "mead_min_lt_1025"), 
  facet_scales = "free_y",
  facet_nrow = 2
))

print(scens_plot_probs(
  ex_pe, 
  c("mead_min_lt_1020", "mead_min_lt_1000"), 
  facet_scales = "fixed",
  facet_ncol = 2
))

# scens_plot_cloud() ------------------------------------------

h_mead <- read.csv("inst/extdata/HistMeadPE.csv")
h_powell <- read.csv("inst/extdata/HistPowellPE.csv")
hh <- h_mead
colnames(hh)[2] <- "mead_dec_pe"
hh$powell_dec_pe <- h_powell[,2]

pal <- c(
  "April ST 2007 UCRC" = "#138d75",
  "April ST CT" = "#f1c40f"
)

p1 <- scens_plot_cloud(ex_pe, "mead_dec_pe", historical = h_mead, 
                       legend_wrap = 20,
                       plot_colors = pal, y_lab = "feet", years = 2020:2026) +
  theme_cloud()

p2 <- scens_plot_cloud(ex_pe, c("powell_dec_pe", "mead_dec_pe"), 
                       historical = hh, legend_wrap = 20,
                       plot_colors = pal, y_lab = "feet", years = 2020:2026,
                       facet_scales = "free_y", fill_label = "ok then") +
  theme_cloud()

print(p1)
print(p2)

# vars_plot_probs() line -------------------------------------------------

vv <- c("mead_min_lt_1000", "mead_min_lt_1020", "powell_wy_min_lt_3490", 
        "powell_dec_lt_3525")
zz <- filter(ex_pe, Variable %in% vv)

gg <- vars_plot_probs(zz, "April ST CT")

v_names <- c("mead_min_lt_1000" = "Mead < 1,000' in Any Month", 
             "mead_min_lt_1020" = "Mead < 1,020' in Any Month", 
             "powell_wy_min_lt_3490" = "Powell < 3,490' in Any Month in the WY", 
             "powell_dec_lt_3525" = "Powell < 3,525' in December")

gg2 <- vars_plot_probs(zz, "April ST CT", years = 2021:2040, 
                       var_labels = v_names, legend_wrap = 10)

gg3 <- vars_plot_probs(zz, unique(zz$ScenarioGroup))

v_col <- c("red", "black")
names(v_col) <- vv[1:2]
gg4 <- vars_plot_probs(zz, "April ST CT", vars = vv[1:2], plot_colors = v_col)

print(gg)
print(gg2)
print(gg3)
print(gg4)

# vars_plot_probs() bar -------------------------------------------------

zz <- filter(ex_pe, Variable == "mead_dec_pe") %>%
  mutate(Shortage = case_when(
    Value <= 1075 & Value > 1050 ~ 1,
    Value <= 1050 & Value > 1025 ~ 2, 
    Value <= 1025 ~ 3,
    TRUE ~ 0
  )) %>%
  mutate(
    Short1 = if_else(Shortage == 1, 1, 0),
    Short2 = if_else(Shortage == 2, 1, 0),
    Short3 = if_else(Shortage == 3, 1, 0)
  ) %>%
  select(-Variable, -Value, -Shortage) %>%
  tidyr::pivot_longer(c("Short1", "Short2", "Short3"), names_to = "Variable",
                      values_to = "Value")

gg <- vars_plot_probs(zz, scenarios = "April ST CT", plot_type = "stacked bar")
gg2 <- vars_plot_probs(
  zz, 
  scenarios = c("April ST CT", "April ST 2007 UCRC"), 
  plot_type = "stacked bar"
)

vname <- c("Short1" = "Shortage 1", "Short2" = "Shortage 2", 
           "Short3" = "Shortage 3")

vcol <- c("Short1" = "grey80", "Short2" = "grey50", 
           "Short3" = "grey20")

gg3 <- vars_plot_probs(zz, scenarios = "April ST CT", plot_type = 2, 
                       var_labels = vname, plot_colors = vcol)

print(gg)
print(gg2)
print(gg3)

# vars_plot_heatmap() -------------------------------------------

gg <- vars_plot_heatmap(zz, scenarios = "April ST CT")
gg2 <- vars_plot_heatmap(zz, scenarios = "April ST CT", 
                         vars = c("Short2", "Short1", "Short3"))
gg3 <- vars_plot_heatmap(
  zz, 
  scenarios = unique(zz$ScenarioGroup), 
  years = 2020:2026, 
  y_lab = "shortage", 
  var_labels = vname,
  title = "ok", subtitle = "then", caption = "here and now", color_label = "prct"
)
vnames <- c(
  "Short1" = "a really really reallly really really reallyl long label", 
  "Short2" = "b", "Short3" = "c"
)

gg4 <- vars_plot_heatmap(
  zz, 
  scenarios = "April ST CT", 
  var_labels = vnames, 
  legend_wrap = 15, 
  vars = c("Short2", "Short1", "Short3")
)

print(gg)
print(gg2)
print(gg3)
print(gg4)

# var_plot_trace_scatter ----------------------------------
var_plot_trace_scatter(ex_pe, vars = "mead_dec_pe", years = 2021, scenarios = "April ST CT")

zz <- mutate(ex_pe, color_cat = case_when(
  Value > 1095 ~ "No concern",
  Value > 1076 ~ "Some concern",
  Value > 1074 ~ "Moderate concern",
  TRUE ~ "concern")
)

cc <- c("No concern" = "grey20", "Some concern" = "blue", 
        "Moderate concern" = "steelblue", "concern" = "red")

gg <- var_plot_trace_scatter(
  zz, 
  vars = "mead_dec_pe", 
  years = 2021, 
  scenarios = "April ST CT", 
  color_by = "color_cat"
)

gg2 <- var_plot_trace_scatter(
  zz, 
  vars = "mead_dec_pe", 
  years = 2021, 
  scenarios = "April ST CT", 
  color_by = "color_cat",
  plot_colors = cc
)

gg3 <- var_plot_trace_scatter(
  zz, 
  vars = "mead_dec_pe", 
  years = 2021, 
  scenarios = unique(zz$ScenarioGroup), 
  color_by = "color_cat",
  plot_colors = cc,
  title = "Mead end of 2021 elevation", subtitle = "colored by concern", 
  y_lab = "feet"
)

print(gg)
print(gg2)
print(gg3)

dev.off()