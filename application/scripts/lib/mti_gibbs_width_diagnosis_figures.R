mwd_names <- c(normal = "Normal", student_t3 = "Student t (3 df)",
  asym_laplace_tau010 = "Asymmetric Laplace", two_piece_normal_1_12 = "Two-piece normal",
  exponential = "Exponential", gamma05 = "Gamma (shape 0.5)", beta18 = "Beta (1, 8)",
  lognormal_hard = "Log-normal", fct_tensile = "Tablet tensile strength")

mwd_title <- function(task) sprintf("%s; n = %d, c = %.2f, q = %.5f\n%s",
  mwd_names[[task$response_id]], task$n, task$content, task$q, task$case_id)

mwd_figures <- function(cases, summaries, samples, fits, original, output) {
  colors <- c("#0072B2", "#D55E00", "#009E73", "#CC79A7")
  grDevices::pdf(file.path(output, "prior_and_target_sensitivity.pdf"), width = 11, height = 8.5,
    useDingbats = FALSE)
  for (i in seq_len(nrow(cases))) {
    task <- cases[i, ]; y <- samples[[i]]; z <- summaries[summaries$case_id == task$case_id, ]
    p <- z[z$contrast == "prior", ]; p <- p[order(p$variance), ]
    par(mfrow = c(2, 2), mar = c(4.4, 4.8, 2.8, 1), oma = c(1.5, 0, 3, 0))
    x <- sqrt(p$variance)
    plot(x, p$width, log = "x", type = "b", pch = 4, xlab = "Prior standard deviation",
      ylab = "Interval width", ylim = range(0, p$width_q025, p$width_q975), main = "Prior sensitivity")
    arrows(x, p$width_q025, x, p$width_q975, angle = 90, code = 3, length = .04)
    abline(h = task$ecm_upper - task$ecm_lower, col = colors[2], lty = 2)
    legend("topleft", c("Posterior mean and 95% interval", "Archived ECM width"),
      col = c("black", colors[2]), lty = c(1, 2), pch = c(4, NA), cex = .75, bty = "n")
    matplot(x, cbind(p$lower, p$upper), log = "x", type = "b", lty = 1, pch = c(1, 2),
      col = colors[1:2], xlab = "Prior standard deviation", ylab = "Posterior mean endpoint",
      main = "Endpoint sensitivity")
    abline(h = range(y), col = "grey65", lty = 3)
    legend("bottomleft", c("Lower endpoint", "Upper endpoint", "Sample extremes"),
      col = c(colors[1:2], "grey65"), lty = c(1, 1, 3), bty = "n", cex = .8)
    other <- z[z$contrast != "prior" | z$variant == "prior_var_10000", ]
    labels <- c(prior_var_10000 = "Original target", tilt_zero = "Zero tilt",
      tilt_interior_midpoint = "Interior tilt", learning_rate_10 = "Learning rate 10",
      learning_rate_100 = "Learning rate 100", sample_centered_sample_scale = "Sample-scale prior")
    par(mar = c(4.4, 8, 2.8, 1))
    plot(other$width, seq_len(nrow(other)), xlim = range(0, other$width_q975),
      yaxt = "n", xlab = "Width", ylab = "", pch = 4, main = "Separate diagnostic contrasts")
    axis(2, at = seq_len(nrow(other)), labels = labels[other$variant], las = 1, cex.axis = .75)
    arrows(other$width_q025, seq_len(nrow(other)), other$width_q975, seq_len(nrow(other)),
      angle = 90, code = 3, length = .04)
    par(mar = c(4.4, 4.8, 2.8, 1))
    baseline <- z[z$variant == "prior_var_10000", ]
    plot(ecdf(y), xlim = range(y, baseline$lower, baseline$upper), do.points = FALSE,
      verticals = TRUE, xlab = "Response", ylab = "Empirical distribution function",
      main = "Location relative to the observations")
    abline(v = c(task$ecm_lower, task$ecm_upper), col = colors[2], lty = 2)
    abline(v = c(baseline$lower, baseline$upper), col = colors[1], lty = 1)
    legend("bottomright", c("ECM", "Gibbs mean endpoints"), col = colors[c(2, 1)],
      lty = c(2, 1), bty = "n", cex = .8)
    mtext(mwd_title(task), outer = TRUE, line = .4, cex = 1.05)
    mtext("Bars describe posterior width variation for one fixed sample, not repeated-sampling width variation.",
      outer = TRUE, side = 1, cex = .8)
  }
  dev.off()
  grDevices::pdf(file.path(output, "overdispersed_baseline_traces.pdf"), width = 11, height = 8.5,
    useDingbats = FALSE)
  for (i in seq_len(nrow(cases))) {
    task <- cases[i, ]; a <- mgp_array(fits[[paste(task$case_id, "prior_var_10000", sep = ":")]])
    old <- mgp_array(original[[i]])
    par(mfrow = c(2, 2), mar = c(4.2, 4.6, 2.3, 1), oma = c(1, 0, 3, 0))
    for (j in seq_len(4)) {
      matplot(a[, , j], type = "l", lty = 1, col = adjustcolor(colors, .65),
        xlab = "Retained iteration", ylab = dimnames(a)[[3]][j], main = "Four overdispersed chains")
      abline(h = mean(old[, , j]), lty = 2)
    }
    mtext(mwd_title(task), outer = TRUE, line = .4)
    mtext("Dashed line: original posterior mean. The diagnostic chains start independently at much wider values.",
      outer = TRUE, side = 1, cex = .8)
  }
  dev.off()
  invisible(TRUE)
}
