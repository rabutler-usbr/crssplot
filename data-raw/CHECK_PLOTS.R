library(crssplot)
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

dev.off()

