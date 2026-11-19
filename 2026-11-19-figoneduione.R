# Run with Rscript 2026-11-19-figoneduione.R [output_directory].
# Requires ggplot2, patchwork, svglite, and ragg.
library(ggplot2)
library(patchwork)

groups <- c("ZS11", "AzAz", "C2C2", "AzC2", "AzAzC2C2")
components <- data.frame(
  group = rep(groups, c(2, 1, 1, 2, 2)),
  part = rep(c("A", "C"), 4),
  rna = c(376118, 419961, 371847, 567426, 206073, 248925, 360635, 432426),
  h3k4me3 = c(388272, 460792, 358216, 561803, 108576, 129744, 339334, 414481)
)
totals <- data.frame(
  group = c("ZS11", "AzC2", "AzAzC2C2"), part = "A+C",
  rna = c(796080, 454997, 793060),
  h3k4me3 = c(849064, 238320, 753815)
)

part_order <- c("A", "C", "A+C")
ratios <- rbind(components, totals)
ratios$part <- factor(ratios$part, levels = part_order)
ratios$ratio <- with(ratios, rna / h3k4me3)
ratios$has_total <- ratios$group %in% totals$group
offsets <- c(A = -0.22, C = 0.22, "A+C" = 0)[as.character(ratios$part)]
ratios$x <- match(ratios$group, groups) + ifelse(ratios$has_total, offsets, 0)

centers <- match(components$group, groups)
bar_offset <- 0.22
amounts <- data.frame(
  x = c(centers - bar_offset, centers + bar_offset),
  part = factor(rep(components$part, 2), levels = c("A", "C")),
  amount = c(components$rna, components$h3k4me3) / 10000
)
assay_labels <- data.frame(
  x = c(seq_along(groups) - bar_offset, seq_along(groups) + bar_offset),
  label = rep(c("RNA", "H3K4me3"), each = length(groups))
)
material_labels <- data.frame(x = seq_along(groups), label = groups)

ink <- "#292929"
group_scale <- scale_x_continuous(
  breaks = seq_along(groups), labels = groups,
  limits = c(0.49, length(groups) + 0.51), expand = expansion(mult = 0)
)
common_theme <- theme_classic(base_size = 8, base_family = "Arial") +
  theme(
    text = element_text(color = ink),
    axis.text = element_text(color = ink),
    axis.text.x = element_text(size = 7.4, margin = margin(t = 4), lineheight = 1.05),
    axis.title.y = element_text(margin = margin(r = 5)),
    axis.line = element_line(color = ink, linewidth = 0.25),
    axis.ticks = element_line(color = ink, linewidth = 0.25),
    axis.ticks.x = element_blank(),
    axis.ticks.length = unit(1, "mm"),
    legend.position = "top",
    legend.justification = "left",
    legend.key.height = unit(3, "mm"),
    legend.key.width = unit(3.2, "mm"),
    legend.key.spacing.x = unit(2, "mm"),
    legend.margin = margin(0, 0, 3, 0),
    plot.margin = margin(4, 5, 5, 4)
  )

ratio_plot <- ggplot(ratios, aes(x, ratio)) +
  geom_hline(yintercept = 1, color = "#A7A7A7", linewidth = 0.24, linetype = "dashed") +
  geom_point(aes(shape = part, fill = part), color = ink, size = 2.05, stroke = 0.42) +
  geom_text(
    data = subset(ratios, part == "A+C" | !has_total),
    aes(label = sprintf("%.2f", ratio)),
    nudge_y = 0.12, vjust = 0, size = 7.4, size.unit = "pt",
    family = "Arial", color = ink
  ) +
  scale_shape_manual(
    values = c(A = 21, C = 21, "A+C" = 23),
    labels = c("A component", "C component", "A+C total")
  ) +
  scale_fill_manual(
    values = c(A = "white", C = "#B5B5B5", "A+C" = ink),
    labels = c("A component", "C component", "A+C total")
  ) +
  group_scale +
  scale_y_continuous(
    breaks = seq(0, 2, by = 0.5), labels = c("0", "0.5", "1.0", "1.5", "2.0"),
    limits = c(0, 2.22), expand = expansion(mult = 0)
  ) +
  labs(x = NULL, y = "RNA / H3K4me3", shape = NULL, fill = NULL) +
  common_theme

amount_plot <- ggplot(amounts, aes(x, amount, fill = part)) +
  geom_col(
    width = 0.32, position = position_stack(reverse = TRUE),
    color = ink, linewidth = 0.18
  ) +
  scale_fill_manual(
    values = c(A = "#B4A087", C = "#7893A5"),
    labels = c("A component", "C component")
  ) +
  group_scale +
  scale_y_continuous(
    breaks = seq(0, 100, by = 25), limits = c(0, 104), expand = expansion(mult = 0)
  ) +
  labs(x = NULL, y = "Reported amount (\u00d710\u2074)", fill = NULL) +
  common_theme +
  theme(axis.text.x = element_blank(), plot.margin = margin(4, 5, 0, 4))

amount_axis <- ggplot() +
  geom_text(
    data = assay_labels, aes(x, 0.74, label = label),
    family = "Arial", color = ink, size = 7.2, size.unit = "pt"
  ) +
  geom_text(
    data = material_labels, aes(x, 0.25, label = label),
    family = "Arial", color = ink, size = 7.4, size.unit = "pt"
  ) +
  group_scale +
  scale_y_continuous(limits = c(0, 1), expand = expansion(mult = 0)) +
  theme_void() +
  theme(plot.margin = margin(0, 5, 4, 4))

amount_figure <- wrap_plots(amount_plot, amount_axis, ncol = 1,
                            heights = grid::unit(c(1, 8.5), c("null", "mm")))

save_figure <- function(plot, stem, height_mm) {
  ggsave(paste0(stem, ".svg"), plot, device = svglite::svglite,
         width = 136, height = height_mm, units = "mm", bg = "white")
  ggsave(paste0(stem, ".png"), plot, device = ragg::agg_png,
         width = 136, height = height_mm, units = "mm", dpi = 300, bg = "white")
}

args <- commandArgs(trailingOnly = TRUE)
output_dir <- if (length(args) == 0L) "." else args[[1]]
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
save_figure(ratio_plot, file.path(output_dir, "rna_h3k4me3_ratios_materials_only"), 52)
save_figure(amount_figure, file.path(output_dir, "rna_h3k4me3_amounts_tall_soft_swapped"), 78)
