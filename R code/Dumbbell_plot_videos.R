> library(ggplot2)
> library(dplyr)
> library(tidyr)
> 
> # ----------------------------
> # 1. Data
> # ----------------------------
> df <- data.frame(
+     resource = c("Resource 1", "Resource 1", "Resource 1",
+                  "Resource 2", "Resource 2", "Resource 2",
+                  "Resource 3", "Resource 3", "Resource 3"),
+     measure = c("Relevance", "Usefulness", "Recommend",
+                 "Relevance", "Usefulness", "Recommend",
+                 "Relevance", "Usefulness", "Recommend"),
+     HCPs = c(4.60, 4.60, 5.59,
+              4.59, 4.61, 5.55,
+              4.69, 4.66, 5.69),
+     AC = c(5.47, 5.36, 5.25,
+            5.45, 5.46, 5.41,
+            5.83, 5.84, 5.60)
+ )
> 
> # Set factor order
> df$resource <- factor(df$resource, levels = c("Resource 1", "Resource 2", "Resource 3"))
> df$measure  <- factor(df$measure, levels = c("Relevance", "Usefulness", "Recommend"))
> 
> # Long format for points and labels
> df_long <- df %>%
+     pivot_longer(cols = c("HCPs", "AC"),
+                  names_to = "group",
+                  values_to = "score") %>%
+     mutate(
+         group = factor(group, levels = c("HCPs", "AC")),
+         label = sprintf("%.2f", score),
+         # For "Recommend" row: put HCPs label below, AC label above
+         # For other rows: put labels to the left (HCPs) and right (AC)
+         hjust_val = case_when(
+             measure == "Recommend" ~ 0.5,
+             group == "HCPs" ~ 1.3,    # Push further left
+             group == "AC" ~ -0.3      # Push further right
+         ),
+         vjust_val = case_when(
+             measure == "Recommend" & group == "HCPs" ~ 1.8,   # Below
+             measure == "Recommend" & group == "AC" ~ -0.8,    # Above
+             TRUE ~ 0.5
+         )
+     )
> 
> # Colorblind-friendly palette: blue and orange
> colors <- c("HCPs" = "#0072B2", "AC" = "#E69F00")
> 
> # ----------------------------
> # 2. Plot
> # ----------------------------
> ggplot() +
+     # Lines connecting HCPs and AC
+     geom_segment(
+         data = df,
+         aes(x = HCPs, xend = AC, y = measure, yend = measure),
+         color = "grey60",
+         linewidth = 1.2
+     ) +
+     # Points
+     geom_point(
+         data = df_long,
+         aes(x = score, y = measure, color = group),
+         size = 4
+     ) +
+     # Labels
+     geom_text(
+         data = df_long,
+         aes(x = score, y = measure, label = label, 
+             hjust = hjust_val, vjust = vjust_val, color = group),
+         size = 3.5,
+         fontface = "bold",
+         show.legend = FALSE
+     ) +
+     # Facet by resource
+     facet_wrap(~ resource, ncol = 1) +
+     # Scales
+     scale_x_continuous(
+         limits = c(4.0, 7.0),
+         breaks = seq(4, 7, 0.5),
+         expand = expansion(mult = c(0.02, 0.02))
+     ) +
+     scale_color_manual(values = colors) +
+     # Labels
+     labs(
+         title = "Average feedback scores by group and resource",
+         x = "Average score",
+         y = NULL,
+         color = NULL,
+         caption = "HCPs: n = 100; AC: n = 99"
+     ) +
+     # Theme
+     theme_minimal(base_size = 13) +
+     theme(
+         legend.position = "top",
+         panel.grid.major.y = element_blank(),
+         panel.grid.minor = element_blank(),
+         strip.text = element_text(face = "bold", size = 12),
+         plot.title = element_text(face = "bold", size = 14),
+         plot.caption = element_text(hjust = 1, size = 10, color = "grey40")
+     )
> 
> # Save the plot
> ggsave("dumbbell_plot_final.png", width = 8, height = 10, dpi = 300)
