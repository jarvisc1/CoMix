eigen_all <- readRDS('inst/data/contact_matrices/eigen_all.rds')
eigen_phys <- readRDS('inst/data/contact_matrices/eigen_physical.rds')
eigen_all_scaled <- readRDS('inst/data/contact_matrices/eigen_all_polymod_scaled.rds')
eigen_phys_scaled <- readRDS('inst/data/contact_matrices/eigen_physical_polymod_scaled.rds')


R_mean <- 2.68
R_sd <- 0.57

type <- rep(c("All", "Physical", "All_scaled", "Phys_scaled"), each = boots)

eigens <- c(eigen_all, eigen_phys, eigen_all_scaled, eigen_phys_scaled)

eigen_df <- data.table(type, eigens)

previousR <- rnorm(nrow(eigen_df), mean = R_mean, sd = R_sd)

eigen_df <- data.table(type, eigens, previousR, newR = eigens*previousR)

saveRDS(eigen_df, file = "inst/data/contact_matrices/rds_eigen.rds")


eigen_df[,.(
  mean = mean(newR),
  lci = quantile(newR, probs = 0.025),
  uci = quantile(newR, probs = 0.975),
  eigen_mean = mean(eigens),
  eigen_lci = quantile(eigens, probs = 0.025),
  eigen_uci = quantile(eigens, probs = 0.975),
  eigen_lci = min(eigens),
  eigen_uci = max(eigens)
),
by = type
]


changes_inR <-  readRDS("inst/data/contact_matrices/rds_eigen.rds")

max_all <-  1/max(eigen_all)
max_phys <- 1/max(eigen_phys)

boots <- length(eigen_all)

ggplot(eigen_df) +
  geom_histogram(aes(max_all * eigens, fill = type, group = type),
                 bins = 100, position = "identity") +
  xlim(c(0,1)) +
  facet_wrap(.~type) +
  geom_vline(xintercept = 1) +
  scale_x_log10()


# 3.4?
# eigen_df[ , 3.4 * mean(eigens) , by = type]
# eigen_df[ , 3.4 * median(eigens) , by = type]


## Create R estimates table

eigen_summary_table <- data.table(
  contact = c("all", "physical"),
  mean = c(mean(eigen_all),
            mean(eigen_phys)),
  min = c(min(eigen_all),
           min(eigen_phys)),
  ci_low = c(quantile(eigen_all, probs = c(0.025, 0.975))[1],
             quantile(eigen_phys, probs = c(0.025, 0.975))[1]),
  ci_high = c(quantile(eigen_all, probs = c(0.025, 0.975))[2],
             quantile(eigen_phys, probs = c(0.025, 0.975))[2]),
  max = c(max(eigen_all),
          max(eigen_phys))
  )

eigen_summary_table

## Create R estimates table

all_scaled_mean <- mean(rnorm(boots,
                              mean = R_mean,
                              sd = R_sd) * eigen_all_scaled)
all_scaled_quantiles <- quantile(
  rnorm(boots, mean = R_mean, sd = R_sd) * eigen_all_scaled,
  probs = c(0.025, 0.975))

phys_scaled_mean <- mean(rnorm(boots,
                               mean = R_mean,
                               sd = R_sd) * eigen_phys_scaled)
phys_scaled_quantiles <- quantile(
  rnorm(boots, mean = R_mean, sd = R_sd) * eigen_phys_scaled,
  probs = c(0.025, 0.975))

R_summary_table <- data.table(
  contact = c("all", "physical", "all scaled", "phsyical scaled"),
  mean = c(mean(R_mean * eigen_all),
           mean(R_mean * eigen_phys)),
  ci_low = c(quantile(R_mean * eigen_all, probs = c(0.025, 0.975))[1],
             quantile(R_mean * eigen_phys, probs = c(0.025, 0.975))[1],
             all_scaled_quantiles[1],
             phys_scaled_quantiles[1]),
  ci_high = c(quantile(R_mean * eigen_all, probs = c(0.025, 0.975))[2],
              quantile(R_mean * eigen_phys, probs = c(0.025, 0.975))[2],
              all_scaled_quantiles[2],
              phys_scaled_quantiles[2])
)

R_summary_table
