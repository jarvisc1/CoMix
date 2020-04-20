library(data.table)
library(ggplot2)
library(ggthemr)

ggthemr("fresh")

combined <- readRDS(file.path(base_outputs_path,
                              "combined", "clean_participants_counts.rds"))

combined$country_code
cnt_dt <- combined[, list(panel,
                          wave,
                          country_code,
                          part_id,
                          n_contacts,
                          n_cnt_work,
                          n_cnt_home,
                          n_cnt_not_home,
                          n_cnt_school,
                          n_cnt_inside,
                          n_cnt_outside)]

cnt_dt[ , panel := toupper(gsub("Panel ", "", panel))]
cnt_dt[ , wave := gsub("Wave ", "", wave)]
cnt_dt[ , wave_id := paste(panel, wave)]
cnt_dt[ , week := fcase(wave_id == "A 1", 1,
                        wave_id == "B 1", 2,
                        wave_id == "A 2", 3,
                        wave_id == "B 2", 4,
                        wave_id == "A 3", 5,
                        wave_id == "B 3", 6)]
cnt_dt[, week := as.factor(week)]

id_vars_part <- c("panel", "wave", "wave_id", "week", "country_code", "part_id")
id_vars <- c("panel", "wave", "wave_id", "week", "country_code")
measure_vars <- c("n_contacts", "n_cnt_work", "n_cnt_home","n_cnt_not_home",
                  "n_cnt_school", "n_cnt_inside", "n_cnt_outside")


cnt_dt <- melt(cnt_dt, id.vars = id_vars_part, measure.vars = measure_vars)
cnt_dt[, value := as.numeric(value)]
cnt_dt[ , variable := gsub("n_cnt_", "", variable)]
vlevs <- c("n_contacts", "home", "not_home", "work", "school", "inside", "outside")
vlabs <- c("All", "Within house", "Outside house", "Work", "School", "Indoors", "Outdoors")
cnt_dt[, variable := factor(variable,
                            levels = rev(vlevs),
                            labels = rev(vlabs))]

cnt_dt[order(part_id, week, variable)]

# mean_dt <- cnt_dt[, .(mean = mean(value)), by = c(id_vars, "variable")]
# mean_dt[order(variable, week), list(variable, week, mean)]

count_dt <- cnt_dt[ , .(
  count = .N), by = c(id_vars, "variable")
  ]

cnt_dt <- merge(cnt_dt, count_dt, by = c(id_vars, "variable"))
summ_dt <- cnt_dt[ , .(
  cnt_mean = mean(value),
  cnt_sd = sd(value),
  cnt_median = median(value),
  cnt_l_iqr = quantile(value, p = 0.25),
  cnt_u_iqr = quantile(value, p = 0.75),
  cnt_95_low = mean(value) - (1.96*sd(value)/sqrt(count)),
  cnt_95_high =  mean(value) + (1.96*sd(value)/sqrt(count)),
  cnt_min = min(value),
  cnt_max = max(value)
), by = c(id_vars, "variable")
]

summ_dt[order(variable, week), list(variable,
                                    week,
                                    cnt_mean,
                                    cnt_sd,
                                    cnt_median,
                                    cnt_l_iqr,
                                    cnt_u_iqr,
                                    cnt_ci_low,
                                    cnt_ci_high,
                                    cnt_min,
                                    cnt_max)]
#
# ggplot(cnt_dt, aes(y = value, x = week, fill = factor(week))) +
#   geom_boxplot(outlier.shape = " ") +
#   facet_wrap(vars(variable))
#
# ggplot(cnt_dt, aes(y = value, x = variable)) +
#   geom_boxplot(outlier.size = 0.3) +
#   scale_y_continuous(limits = c(0, 10))




# Errorbar version
# The negative value orders correctly
pos_width = -0.75
mean_contacts <- ggplot(summ_dt[variable != "School"],
                        aes(x = factor(variable), y = as.numeric(cnt_mean), color = week)) +
  geom_errorbar(aes(ymin = cnt_ci_low,
                    ymax = cnt_ci_high),
                width = 0.02,
                size  = 0.5,
                position = position_dodge(width = pos_width)) +
  geom_point(shape = 15, size  = 1.5, position = position_dodge(width = pos_width)) +
  xlab("Contact type") +
  ylab("Mean contacts") +
  labs(color = "Week") +
  theme_bw() +
  theme(
    legend.position = c(0.85, 0.65),
    # legend.title.align = "center",
    # legend.title = element_text(size = 9),
    # legend.text = element_text(size = 8),
    legend.box.background = element_blank(),
    legend.background = element_blank(),
    panel.grid.major.y = element_line(size = 0.4, linetype = 2),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  scale_y_continuous(
    breaks = seq(0, 50, 2),
    limits = c(min(summ_dt$cnt_ci_low), max(summ_dt$cnt_ci_high) + 0.5)
  ) + coord_flip()

mean_contacts

ggsave(plot = mean_contacts,
       filename = file.path(base_outputs_path, "combined", "mean_contacts_by_location.png"))

write.csv(summ_dt,
          file.path(base_outputs_path, "combined", "mean_contacts_by_location.csv"),
          row.names = FALSE)
