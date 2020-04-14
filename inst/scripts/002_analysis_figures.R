### Figure 1.

library(patchwork)
library(ggthemr)
library(socialmixr)
library(data.table)
ggthemr("fresh")
## Create Figure of Contact Matrices

source('R/ggplot_functions.R')
# source('r/functions/utility_functions.R')

h2020_cm <- readRDS(file.path(matrices_path, "comix_cm.rds"))
# h2020_cm <- h2020_cm[[1]]$matrix
h2020_cm_imputed <- readRDS(file.path(matrices_path, "comix_cm_imputed.rds"))
polymod_cm <- readRDS(file.path(matrices_path, "polymod_cm.rds"))


rowSums(polymod_cm)/rowSums(h2020_cm)
colnames(polymod_cm)

cmatrices <- list(
  "POLYMOD" = polymod_cm,
  "CoMix" = h2020_cm_imputed
)

## Transform data
cm_dt <- sm_to_dt_matrix(cmatrices)

cm_dt$contact_age

## Create the plot
matrix_plot <- gg_matrix(
  cm_dt,
  breaks = c(0, 2, 4, 6, 8),
  age_lab = age_labs
) +
  ggtitle("A")

matrix_plot



## Frequency of contacts


part <- readRDS(file.path(data_path, "clean_participants.rds"))
polymod_part <- readRDS(
  file.path(base_data_directory , "polymod_participants.rds"))

class(part$part_age_group)
class(polymod_part$part_age_group)

part_age <- part[ , .(study = "CoMix", .N), by = part_age_group]
polymod_age <- polymod_part[ , .(study = "POLYMOD", .N), by = part_age_group]

part_age[, per := N/nrow(part)]
polymod_age[, per := N/nrow(polymod_part)]

extra_under18_row <- polymod_age[part_age_group == "[0,18)",]
extra_under18_row$per <- extra_under18_row$N <- 0
extra_under18_row$study <- "CoMix"


comb_agef <- rbind(part_age, polymod_age, extra_under18_row)


age_compare_plot <- ggplot(comb_agef) +
  geom_col(aes(x = part_age_group, y = per, fill = study),
           position = "dodge") +
  labs(
    x="Age group",
    y="",
    fill="Study"
  ) +
  #scale_fill_manual(values=c("#e41a1c","#377eb8")) +
  scale_x_discrete(
    labels = c("0-17", age_labs[c(-1,-2)])
  ) +
  scale_fill_manual(
    values=c("#0D5257","#A7A8AA")
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 10L)) +
  coord_flip() +
  ggtitle("B") +
  geom_hline(yintercept = seq(0.,0.5, 0.05), col = "white") +
  theme(
    legend.position = c(0.85, 0.9),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank()
  )

age_compare_plot

## Number of contacts by age

contacts_part <- readRDS(file.path(data_path, "clean_contacts_part.rds"))
polymod_contacts_part <- readRDS(
  file.path(base_data_directory, "polymod_contacts_part.rds"))


h2020_avg <- contacts_part[ ,  .(study = "CoMix", N = .N),
                            by = .(part_id, part_age_group)]
polymod_avg <- polymod_contacts_part[ ,  .(study = "POLYMOD", N = .N) ,
                                      by = .(part_id, part_age_group)]

# Get mean and SD
h2020_avg_all <- h2020_avg[,
                           .(
                             mean_age = mean(N),
                             sd_age =  sd(N),
                             N = sum(N)
                           ),
                           by = .(study, part_age_group)]


polymod_avg_all <- polymod_avg[,
                               .(
                                 mean_age = mean(N),
                                 sd_age =  sd(N),
                                 N = sum(N)
                               ),
                               by = .(study, part_age_group)]


h2020_avg_all[, phys_contact := 0]

polymod_avg_all[, phys_contact := 0]


## Repeat for Physical
h2020_avg <- contacts_part[ ,  .(study = "CoMix", N = .N),
                            by = .(part_id, part_age_group, phys_contact)]
polymod_avg <- polymod_contacts_part[ ,  .(study = "POLYMOD", N = .N) ,
                                      by = .(part_id, part_age_group, phys_contact)]

h2020_avg_phys <- h2020_avg[phys_contact == 1,
                            .(
                              mean_age = mean(N),
                              sd_age =  sd(N),
                              N = sum(N)
                            ),
                            by = .(study, part_age_group)]


polymod_avg_phys <- polymod_avg[, .(
  mean_age = mean(N),
  sd_age =  sd(N) ,
  N = sum(N)
),
by = .(study, part_age_group)]

h2020_avg_phys[, phys_contact := 1]
polymod_avg_phys[, phys_contact := 1]


# Combine into one dataset
comb_avg <- rbind(
  h2020_avg_all,
  polymod_avg_all,
  h2020_avg_phys,
  polymod_avg_phys
)

comb_avg[, low_ci := mean_age - (1.96*sd_age/sqrt(N))]
comb_avg[, upp_ci := mean_age + (1.96*sd_age/sqrt(N))]

comb_avg
avg_contacts_plot <- ggplot(comb_avg,
                            aes(x = part_age_group,
                                y = mean_age,
                                group = interaction(study, phys_contact),
                                col = study)) +
  # geom_ribbon(aes(ymin = low_ci, ymax = upp_ci, fill = study),
  #             alpha = 0.2, col = "white") +
  geom_line(aes(linetype = as.factor(phys_contact))) +
  geom_point() +
  scale_color_manual(
    values=c("#0D5257","#A7A8AA")
  ) +
  scale_x_discrete(
    labels = c("0-17", age_labs[c(-1,-2)])
  ) +
  scale_y_continuous(
    breaks = seq(2, round(max(comb_avg$mean_age)), 2)
  ) +
  scale_linetype_manual(
    values = 1:2,
    labels = c(
      "All",
      "Physical"
    )
  ) +
  labs(
    x="Age group",
    y="Average contacts",
    group="Study",
    linetype=""
  ) +
  ggtitle("D") +
  theme(
    legend.position = c(0.75, 0.95),
    legend.box.background = element_blank(),
    legend.background = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  guides(colour = guide_legend(
    order = 1,
    title = "Study",
    override.aes = list(alpha = 1, size = 2),
    byrow = TRUE,
    direction = "horizontal",
    title.position = "left",
    label.position = "left", keywidth = 0.1
  ),
  linetype = guide_legend(
    "Contacts",
    byrow = TRUE,
    title.position = "left",
    direction = "horizontal"),
  fill = "none")

avg_contacts_plot

part <- readRDS(file.path(data_path, "clean_participants.rds"))
contacts <- readRDS(file.path(data_path, "clean_contacts.rds"))




mean(part$n_contacts)
max(part$n_contacts)
sd(part$n_contacts)


## C change in R plot

changes_inR <-  readRDS(file.path(matrices_path, "rds_eigen.rds"))


changes_inR <- changes_inR[type %in% c("All", "Physical"), ]

ggthemr("fresh")
r_plot <- ggplot(changes_inR) +
  geom_density(aes(newR, fill= type),
               alpha = 0.8, position = "identity") +
  labs(
    x = expression("Estimate of" ~ R[0]),
    y = "Density",
    title = "B") +
  scale_fill_manual(
    name="Type of Contact",
    values=c("#0D5257","#6d979a")
  ) +
  scale_x_continuous(
    limits = c(0.0, 1.25),
    breaks = c(0, 0.25, 0.5, 0.75, 1, 1.25)
  ) +
  geom_vline(xintercept = 1, col = "darkred", linetype = "dashed", alpha = 0.3) +
  theme(
    legend.position = c(1, 0.75),
    legend.box.background = element_blank(),
    legend.background = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank()
  )



r_plot
## Combine the plots
x1 <- (matrix_plot / r_plot )
x1

ggsave(filename = file.path(outputs_path, "figure1-panelb.png"),
       x1, width = 6, height = 6.5)









## FIGURE 2
##
# library(data.table)

part <- readRDS('data/clean_participants.rds')
part$weekday <- weekdays(part$date)

age_lab <- "Age of participant (years)"
gender_lab <- "Gender of participant"
hh_size_lab <- "Household size"
dow_lab <- "Day of week"


part[, hh_size := fcase(
  hh_size == 1, "1",
  hh_size == 2, "2",
  hh_size == 3, "3",
  hh_size == 4, "4",
  hh_size > 4, "4+"
)]

## Participant chracteristics
t_total <- data.table(Category = "Overall", Variables = "Overall", N_participants = nrow(part), order = 1)
t_ages <-  part[order(part_age_group) ,  .(N_participants = .N), by = part_age_group]
t_gender <-  part[order(part_gender) ,  .(N_participants = .N), by = part_gender]
t_hh_size <-  part[order(hh_size) ,  .(N_participants = .N), by = hh_size]
t_weekday <-  part[order(weekday) ,  .(N_participants = .N), by = weekday]


t_counts <- rbind(
  t_total,
  t_ages[, .(Category = age_lab,
             Variables = part_age_group,
             N_participants,
             order = 2)
         ],
  t_gender[, .(Category = gender_lab,
               Variables = part_gender,
               N_participants,
               order = 3)
           ],
  # Probably regroup to 6+ and missing
  t_hh_size[, .(Category = hh_size_lab,
                Variables = hh_size,
                N_participants,
                order = 4)
            ],
  t_weekday[, .(Category = dow_lab,
                Variables = weekday,
                N_participants,
                order = 5)
            ]
)


## Number of contacts per group.

part[, mean_contacts := as.numeric(n_contacts)]

## Calculate the mean and standard devation by the groupings



mean_contacts <- function(df = part,
                          category = "Overall",
                          var = "mean_contacts", by = NULL, filter = FALSE){
  # browser()
  x <- df[, .(
    Mean_contacts = mean(get(var)),
    SD_contacts = sd(get(var)),
    Median_contacts = median(get(var)),
    l_iqr = quantile(get(var), p = 0.25),
    u_iqr = quantile(get(var), p = 0.75)
  )
  ]
  x <- x[, .(Category = category,
             Variables = "Overall",
             Mean_contacts,
             SD_contacts,
             Median_contacts,
             l_iqr,
             u_iqr
  )
  ]
  if (!is.null(by)) {
    x <- df[order(get(by)), .(
      Mean_contacts = mean(get(var)),
      SD_contacts = sd(get(var)),
      Median_contacts = median(get(var)),
      l_iqr = quantile(get(var), p = 0.25),
      u_iqr = quantile(get(var), p = 0.75)
    ),
    by = get(by)
    ]

    x <- x[, .(Category = category,
               Variables = get,
               Mean_contacts,
               SD_contacts,
               Median_contacts,
               l_iqr,
               u_iqr
    )
    ]

  }
  x

}



## Comix contacts
t_overall <- mean_contacts(part, category = "Overall")
t_ages_contacts <- mean_contacts(part, by = "part_age_group", category = age_lab)
t_gender_contacts <-  mean_contacts(part, by = "part_gender", category = gender_lab)
t_hh_size_contacts <-  mean_contacts(part, by = "hh_size", category = hh_size_lab)
t_weekday_contacts <-  mean_contacts(part, by = "weekday", category = dow_lab)


# Combine the groups

t_contacts <- rbind(
  t_overall,
  t_ages_contacts,
  t_gender_contacts,
  t_hh_size_contacts,
  t_weekday_contacts

)

t_contacts

## POLYMOD contacts
polymod <- readRDS(file.path(base_data_path, "polymod_participants.rds"))
polymod_part_contacts <- readRDS(
  file.path(base_data_path, "polymod_contacts_part.rds"))

polymod_mean_contacts <-
  polymod_part_contacts[,  .(mean_contacts = mean(.N)), by = part_id]

polymod <- merge(polymod, polymod_mean_contacts, by = "part_id", all.x = TRUE)


polymod[, hh_size := fcase(
  hh_size == 1, "1",
  hh_size == 2, "2",
  hh_size == 3, "3",
  hh_size == 4, "4",
  hh_size > 4, "6+"
)]

polymod <- polymod[part_age_group != "[0,18)"]
polymod <- polymod[!is.na(mean_contacts)]
polymod

tp_overall <- mean_contacts(polymod, category = "Overall")
tp_ages_contacts <- mean_contacts(polymod, by = "part_age_group", category = age_lab)
tp_gender_contacts <-  mean_contacts(polymod, by = "part_gender", category = gender_lab)
tp_hh_size_contacts <-  mean_contacts(polymod, by = "hh_size", category = hh_size_lab)
#tp_weekday_contacts <-  mean_contacts(polymod, by = "weekday", category = dow_lab)

tp_gender_contacts[ ,Variables := fifelse(Variables == "F", "Female", "Male")]

tp_contacts <- rbind(
  tp_overall,
  tp_ages_contacts,
  tp_gender_contacts,
  tp_hh_size_contacts

)

tp_contacts
## Contacts compared to reference point.

tp_contacts[, SD_lab := fifelse(is.na(SD_contacts), "-", as.character(round(SD_contacts, 1)))]
tp_contacts[, pm_Contacts := paste0(round(Mean_contacts,1)," (", SD_lab, ")" )]
tp_contacts[, pm_Contacts_med := paste0(round(Mean_contacts,1)," (", l_iqr, ", ", u_iqr, ")")]

tp_contacts <- tp_contacts[, .(Category,
                               Variables,
                               pm_Contacts,
                               pm_Contacts_med)]


tp_contacts
t_contacts


table_two <- merge(t_counts, t_contacts, by = c("Category", "Variables"), all.x = TRUE)
table_two <- merge(table_two, tp_contacts, by = c("Category", "Variables"), all.x = TRUE)

table_two[, SD_lab := fifelse(is.na(SD_contacts), "-", as.character(round(SD_contacts, 1)))]
table_two[, Contacts := paste0(round(Mean_contacts,1)," (", SD_lab, ")" )]
table_two[, Contacts_med := paste0(round(Mean_contacts,1)," (", l_iqr, ", ", u_iqr, ")")]



table_two[, Value := fcase(
  Variables == "[18,20)", "18-19",
  Variables == "[20,30)", "20-29",
  Variables == "[30,40)", "30-39",
  Variables == "[40,50)", "40-49",
  Variables == "[50,60)", "50-59",
  Variables == "[60,70)", "60-69",
  Variables == "[70,120)", "70+"
)
]


table_two[, Value := fifelse(
  is.na(Value),
  as.character(Variables),
  Value)
  ]

table_two <- table_two[order(order), .(
  Category,
  Value,
  N_participants,
  Contacts,
  Contacts_med,
  pm_Contacts,
  pm_Contacts_med
)]
setcolorder(table_two, c(
  "Category",
  "Value",
  "N_participants",
  "Contacts",
  "Contacts_med"
)
)

table_two

## Write to csv for output into paper
write.csv(table_two,
          file = file.path(outputs_path, "table_two.csv"), row.names = FALSE)
