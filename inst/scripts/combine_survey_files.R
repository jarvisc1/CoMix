## combined survey files

data_files <- list.files(base_data_path, recursive = TRUE)
part_paths <- grep("clean_participants", data_files, value = TRUE)
part_paths <- grep("interim", part_paths,
                   value = TRUE, invert = TRUE)
part_dts <- lapply(part_paths, function(part_path) {
  readRDS(file.path(base_data_path, part_path))
})

setdiff(names(part_dts[[2]]), names(part_dts[[1]]))
part_dts[[1]]$part_att_spread_others <- NA_character_
part_dts[[1]]$part_visit_indoor_event <- NA_character_
part_dts[[1]]$part_visit_outdoor_event <- NA_character_
part_dt <- do.call("rbind", part_dts)
table(part_dt$panel, part_dt$wave)
saveRDS(part_dt, file.path(base_outputs_path, "combined", "clean_participants.rds"))

# CONTACTS

data_files <- list.files(base_data_path, recursive = TRUE)
part_paths <- grep("clean_contacts.rds", data_files, value = TRUE)
part_paths <- grep("interim", part_paths,
                   value = TRUE, invert = TRUE)
part_paths
cont_dts <- lapply(part_paths, function(part_path) {
  readRDS(file.path(base_data_path, part_path))
})

setdiff(names(cont_dts[[2]]), names(cont_dts[[1]]))
cont_dt <- do.call("rbind", cont_dts)
table(cont_dt$panel, cont_dt$wave)

saveRDS(cont_dt, file.path(base_outputs_path, "combined", "clean_contacts.rds"))



data_files <- list.files(base_data_path, recursive = TRUE)
part_paths <- grep("clean_contacts_part.rds", data_files, value = TRUE)
part_paths <- grep("interim", part_paths,
                   value = TRUE, invert = TRUE)
part_paths
cont_dts <- lapply(part_paths, function(part_path) {
  readRDS(file.path(base_data_path, part_path))
})

setdiff(names(cont_dts[[2]]), names(cont_dts[[1]]))
cont_dts[[1]]$part_att_spread_others <- NA_character_
cont_dts[[1]]$part_visit_indoor_event <- NA_character_
cont_dts[[1]]$part_visit_outdoor_event <- NA_character_
cont_dt <- do.call("rbind", cont_dts)
table(cont_dt$panel.x, cont_dt$wave)
cont_dt$panel = cont_dt$panel.x
table(cont_dt$panel, cont_dt$wave)

saveRDS(cont_dt, file.path(base_outputs_path, "combined", "clean_contacts_part.rds"))
