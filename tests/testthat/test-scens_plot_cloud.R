test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

# scens_plot_cloud <- function(df, vars, historical = NULL, years = NULL, 
#                              scenarios = NULL, plot_colors = NULL, 
#                              scen_labels = NULL, ...)

h_mead <- read.csv("inst/extdata/HistMeadPE.csv")

pal <- c(
  "April ST 2007 UCRC" = "#138d75",
  "April ST CT" = "#f1c40f"
)

scens_plot_cloud(ex_pe, "mead_dec_pe", historical = h_mead, legend_wrap = 20,
                 plot_colors = pal, y_lab = "feet", years = 2020:2026) +
  theme_cloud()


tst <- data.frame(x = 1:10, mid = 1:10, low = 1:10 - 5, top = 1:10 + 6, cat = "yeah", stringsAsFactors = FALSE)
tst <- bind_rows(tst, data.frame(x = 1:10, mid = 1:10 - 2, cat = "nah", stringsAsFactors = FALSE))

gg <- ggplot(filter(tst, cat == "yeah"), aes(x)) +
  geom_ribbon(aes(ymin = low, ymax = top, fill = cat, color = cat), linetype = 2, alpha = 0.2) +
  geom_line(data = tst, aes(y = mid, color = cat))

ggfill <- ggplot(filter(tst, cat == "yeah"), aes(x)) +
  geom_ribbon(aes(ymin = low, ymax = top, fill = cat, color = cat), linetype = 2, alpha = 0.2)

ggline <- ggplot(tst, aes(x)) +
  geom_line(aes(y = mid, color = cat))


legfill <- ggplotGrob(ggfill)$grobs[[which(sapply(ggplotGrob(ggfill)$grobs, function(x) x$name) == "guide-box")]]
legline <- ggplotGrob(ggline)$grobs[[which(sapply(ggplotGrob(ggline)$grobs, function(x) x$name) == "guide-box")]]

l2 <- rbind(legfill, legline)

leg <- ggplotGrob(gg)$grobs[[which(sapply(ggplotGrob(gg)$grobs, function(x) x$name) == "guide-box")]]

i <- which(sapply(ggplotGrob(gg)$grobs, function(x) x$name) == "guide-box")

ggg <- ggplotGrob(gg)
gtable::gtable_add_grob(ggg$grobs[[i]], legfill, t = 3, l = 3)


cowplot::plot_grid(gg, legfill, rel_widths = c(2,.4))
