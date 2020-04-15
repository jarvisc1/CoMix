### Figure Contact matrices different settings.

## Create Figure of Contact Matrices

load(file.path(matrices_path, "comix_cms.RData"))


comix_cm <- readRDS(file.path(matrices_path, "comix_cm.rds"))
polymod_cm <- readRDS(file.path(matrices_path, "polymod_cm.rds"))
comix_cm_phys <- readRDS(file.path(matrices_path, "comix_cm_phys.rds"))
polymod_cm_phys <- readRDS(file.path(matrices_path, "polymod_cm_phys.rds"))

cmatrices <- list(
  "CoMix - All" = comix_cm,
  "POLYMOD - All" = polymod_cm,
  "CoMix - Physical" = comix_cm_phys,
  "POLYMOD - Physical" = polymod_cm_phys,
  "CoMix - Home" = comix_cm_home,
  "POLYMOD - Home" = polymod_cm_home,
  "CoMix - Work" = comix_cm_work,
  "POLYMOD - Work" = polymod_cm_work,
  "CoMix - Other" = comix_cm_other,
  "POLYMOD - Other" = polymod_cm_other,
  "CoMix - School" = comix_cm_school,
  "POLYMOD - School" = polymod_cm_school
)

cm_dt <- sm_to_dt_matrix(cmatrices)


cm_dt


gg_matrix <- function(dt, breaks = c(0,0.5, 1), age_lab = FALSE) {
  ct_age_unique <- unique(dt[,contact_age])

  gplot <- ggplot(dt,
                  aes(x = factor(participant_age,  ct_age_unique),
                      y = factor(contact_age,  ct_age_unique),
                      fill = contacts
                  )
  ) +
    facet_wrap(. ~ factor(study, levels = names(cmatrices)), ncol = 4, nrow = 3) +
    geom_tile() +
    labs(
      x = "Age of participant",
      y = "Age of contact",
      fill = "Contacts"
    ) +
    scale_fill_gradientn(
      colors = c("#0D5257","#00BF6F", "#FFB81C"),
      na.value = "#EEEEEE",
      values = c(0, 1, 3, 5, 12)/12,
      breaks =  breaks,
      limits = c(0, 9)

    )  +
    coord_fixed(ratio = 1, xlim = NULL,
                ylim = NULL, expand = FALSE, clip = "off")

  if(length(age_lab) > 2){
    print("Yes")
    gplot <- gplot +
      scale_x_discrete(
        labels = age_lab
      ) +
      scale_y_discrete(
        labels = age_lab
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  }
  gplot

}


x1 <- gg_matrix(cm_dt ,
          breaks = c(0, 2,4,6, 8),
          age_lab = age_labs
)

x1

ggsave(filename = file.path(outputs_path, "figure2.png"),
       x1, width = 8, height = 8)


