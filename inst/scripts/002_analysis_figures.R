### Figure 1.

library(patchwork)
library(ggthemr)
library(socialmixr)
library(data.table)
ggthemr("fresh")
## Create Figure of Contact Matrices

source('R/ggplot_functions.R')
# source('r/functions/utility_functions.R')

h2020_cm <- readRDS('inst/data/contact_matrices/comix_cm.rds')
# h2020_cm <- h2020_cm[[1]]$matrix
h2020_cm_imputed <- readRDS('inst/data/contact_matrices/comix_cm_imputed.rds')
polymod_cm <- readRDS('inst/data/contact_matrices/polymod_cm.rds')


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


part <- readRDS('inst/data/clean_participants.rds')
polymod_part <- readRDS('inst/data/polymod_participants.rds')

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
  scale_y_continuous(labels = scales::percent_format(accuracy = 10L))+
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

contacts_part <- readRDS('inst/data/clean_contacts_part.rds')
polymod_contacts_part <- readRDS('inst/data/polymod_contacts_part.rds')


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

part <- readRDS('inst/data/clean_participants.rds')
contacts <- readRDS('inst/data/clean_contacts.rds')




mean(part$n_contacts)
max(part$n_contacts)
sd(part$n_contacts)


## C change in R plot

changes_inR <-  readRDS("inst/data/contact_matrices/rds_eigen.rds")


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

ggsave(filename = "inst/outputs/figure1-panelb.png", x1, width = 6, height = 6.5)

