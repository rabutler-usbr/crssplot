ofile <- "data-raw/sample_figures.pdf"

pdf(ofile, width = 9, height = 6)
# scens_plot_range() -----------------------------------------
print(scens_plot_range(pe, "mead_dec_pe"))
print(scens_plot_range(pe, "mead_dec_pe", y_lab = "feet"))
print(scens_plot_range(pe, c("powell_dec_pe", "mead_dec_pe"),
                       facet_scales = "free_y"))
print(scens_plot_range(pe, "mead_dec_pe", scenarios = "April ST CT"))
pc <- c("April ST CT" = "red", "April ST 2007 UCRC" = "black")
sl <- c("April ST CT" = "s1", "April ST 2007 UCRC" = "s2")
print(scens_plot_range(pe, "powell_dec_pe", plot_colors = pc, scen_labels = sl,
     title = "PE", caption = "this is a caption"))
print(scens_plot_range(pe, c("powell_dec_pe", "mead_dec_pe"), 
                       facet_scales = "free_y",
           facet_nrow = 2))
print(scens_plot_range(pe, c("powell_dec_pe", "mead_dec_pe"), 
                       facet_scales = "free_y",
     facet_ncol = 2))
dev.off()

