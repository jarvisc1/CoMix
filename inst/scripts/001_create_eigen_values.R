eigen_all <- readRDS('inst/data/contact_matrices/eigen_all.rds')
eigen_phys <- readRDS('inst/data/contact_matrices/eigen_physical.rds')
eigen_all_scaled <- readRDS('inst/data/contact_matrices/eigen_all_polymod_scaled.rds')
eigen_phys_scaled <- readRDS('inst/data/contact_matrices/eigen_physical_polymod_scaled.rds')



type <- rep(c("All", "Physical", "All_scaled", "Phys_scaled"), each = boots)

eigens <- c(eigen_all, eigen_phys, eigen_all_scaled, eigen_phys_scaled)

eigen_df <- data.table(type, eigens)



previousR <- rnorm(nrow(eigen_df), mean = 2.6, sd = 0.54)

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

1/max(eigen_all)
1/max(eigen_phys)

boots <- length(eigen_all)



ggplot(eigen_df) +
  geom_histogram(aes(3.6*eigens, fill = type, group = type),
                 bins = 100, position = "identity") +
  xlim(c(0,1)) +
  facet_wrap(.~type) +
  geom_vline(xintercept = 1) +
  scale_x_log10()


eigen_df[ , 3.4*mean(eigens) , by = type]
eigen_df[ , 3.4*median(eigens) , by = type]


mean(2.4*eigen_all)
quantile(2.4*eigen_all, probs = c(0.025, 0.975))
mean(2.4*eigen_phys)
quantile(2.4*eigen_phys, probs = c(0.025, 0.975))

mean(2.4*eigen_phys)
quantile(2.4*eigen_phys, probs = c(0.025, 0.975))

mean(rnorm(boots, mean = 2.6, sd = 0.54)*eigen_all_scaled)
quantile(rnorm(boots, mean = 2.6, sd = 0.54)*eigen_all_scaled, probs = c(0.025, 0.975))
mean(2.4*eigen_phys_scaled)
quantile(2.4*eigen_phys_scaled, probs = c(0.025, 0.975))

mean(eigen_all)
min(eigen_all)
max(eigen_all)
mean(eigen_phys)
min(eigen_phys)
max(eigen_phys)

quantile(eigen_all, probs = c(0.025, 0.975))
mean(eigen_phys)
quantile(eigen_phys, probs = c(0.025, 0.975))

