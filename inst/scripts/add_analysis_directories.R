if (!file.exists(data_path)) {
  if(!file.exists(file.path(base_data_path, panel))) {
    dir.create(file.path(base_data_path, panel))
  }
  dir.create(file.path(base_data_path, panel, wave))
}

if (length(list.files(data_path)) == 0) {
  stop(paste("Add CoMix contact and participant data.tables to: ", data_path))
}

if (!file.exists(outputs_path)) {
  if(!file.exists(file.path(base_outputs_path, panel))) {
    dir.create(file.path(base_outputs_path, panel))
  }
  dir.create(file.path(base_outputs_path, panel, wave))
}

if (!file.exists(matrices_path)) dir.create(matrices_path)


if (!file.exists(online_matrices_path)) dir.create(online_matrices_path)

