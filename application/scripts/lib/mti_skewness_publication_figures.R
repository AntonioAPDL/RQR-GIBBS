msu_width_figures <- function(cfg) {
  d <- msu_read(cfg, "primary_cells")
  oracle <- msu_read(cfg, "population_content_references")
  labels <- setNames(cfg$labels, cfg$law_order)
  laws <- rev(cfg$law_order)
  colors <- c(ecm = "#009E73", young_mathew = "#CC79A7", wilks = "black")
  offsets <- c(ecm = .22, young_mathew = 0, wilks = -.22)
  designs <- unique(d[, .(n, content)])[order(n, content)]
  groups <- list(small = designs[n <= 100], n500 = designs[n == 500], n1000 = designs[n == 1000])
  for (name in names(groups)) {
    selected <- groups[[name]]
    path <- file.path(cfg$figure_dir, paste0("width_ranges_", name, ".pdf"))
    grDevices::pdf(path, width = 6.5, height = 8.0, pointsize = 9.5, useDingbats = FALSE)
    tryCatch({
      graphics::layout(matrix(1:4, ncol = 1), heights = c(1, 1, 1, .14))
      graphics::par(mar = c(2.8, 12.0, 1.8, .6), mgp = c(1.65, .4, 0), las = 1,
        cex.axis = .92, cex.lab = .98, cex.main = 1.03)
      for (i in seq_len(nrow(selected))) {
        design <- selected[i]
        z <- d[n == design$n & content == design$content]
        reference <- oracle[content == design$content & dgp_id %in% cfg$law_order]
        limits <- range(c(z$width_q025, z$width_q975, z$mean_width, reference$width), finite = TRUE)
        limits <- limits * c(.9, 1.08)
        graphics::plot(NA, log = "x", xlim = limits, ylim = c(.5, length(laws) + .5),
          yaxt = "n", xlab = "Interval width (log scale)", ylab = "", bty = "l",
          main = sprintf("n = %d, c = %.2f", design$n, design$content))
        graphics::axis(2, at = seq_along(laws), labels = labels[laws], tick = FALSE, cex.axis = .91)
        graphics::abline(h = seq(.5, length(laws) + .5, 1), col = "grey93", lwd = .6)
        position <- match(reference$dgp_id, laws)
        graphics::segments(reference$width, position - .37, reference$width, position + .37,
          col = "grey35", lty = 2, lwd = 1)
        for (method_name in cfg$methods) {
          a <- z[method == method_name]
          y <- match(a$law_id, laws) + offsets[[method_name]]
          graphics::segments(a$width_q025, y, a$width_q975, y, col = colors[[method_name]], lwd = 1.45)
          graphics::points(a$mean_width, y, pch = 4, cex = .55, col = colors[[method_name]], lwd = 1)
        }
      }
      graphics::par(mar = c(0, 0, 0, 0)); graphics::plot.new()
      graphics::legend("center", c("MTI-ECM", "Young-Mathew", "Wilks", "Mean", "Population reference"),
        col = c(unname(colors), "grey20", "grey35"), lty = c(1, 1, 1, NA, 2),
        pch = c(NA, NA, NA, 4, NA), lwd = c(1.5, 1.5, 1.5, NA, 1), ncol = 5,
        bty = "n", cex = .88, x.intersp = .6, seg.len = 1.5)
    }, finally = grDevices::dev.off())
  }
  invisible(TRUE)
}
