# Summarize results
# Will break out into functions
#
outputs_files <- list.files(base_outputs_path, recursive = TRUE)
r_summary_paths <- grep("r_summary_table.csv", outputs_files, value = TRUE)
r_summary_paths <- grep("interim", r_summary_paths,
                        value = TRUE, invert = TRUE)

r_summaries <- list()
for (s_path in r_summary_paths) {
  split_path <- stringr::str_split(s_path, "/|\\\\")[[1]]
  df <- as.data.table(read.csv(file.path(base_outputs_path, s_path),
                               stringsAsFactors = FALSE, row.names = ))
  df[ , panel := toupper(gsub("panel_", "", split_path[1]))]
  df[ , wave := gsub("wave_", "", split_path[2])]
  df[ , wave_id := paste(panel, wave)]
  df[ , week := fcase(wave_id == "A 1", 1,
                      wave_id == "B 1", 2,
                      wave_id == "A 2", 3,
                      wave_id == "B 2", 4,
                      wave_id == "A 3", 5,
                      wave_id == "B 3", 6)]
  df <- df[, list(week, wave_id, contact, mean, ci_low, ci_high)]
  r_summaries[[length(r_summaries) + 1]] <- df
}

r_summary <- do.call("rbind", r_summaries)
r_summary <- r_summary[contact %in% c("all scaled", "phsyical scaled")]
r_summary <- r_summary[, wave_id := as.factor(wave_id)]

write.csv(r_summary, file.path(base_outputs_path, "combined", "r_summary.csv"),
          row.names = FALSE)

r0_errrorbar <- ggplot(r_summary,
                       aes(x = factor(week), y = as.numeric(mean), color = contact)) +
  geom_errorbar(aes(ymin = ci_low,
                    ymax = ci_high),
                width = 0.05,
                size  = 1,
                position = position_dodge(width = 0.9)) +
  geom_point(shape = 15, size  = 5, position = position_dodge(width = 0.9)) +
  xlab("Survey Week") +
  ylab("R0") +
  ggtitle("Mean R0 Estimates")

r0_errrorbar

ggsave(plot = r0_errrorbar,
       filename = file.path(base_outputs_path, "combined", "r_summary.png"))
