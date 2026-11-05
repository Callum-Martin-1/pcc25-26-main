files <- Rs3tools::list_files_in_buckets("alpha-pcc2526")

folder = "~/MoJ Secondment/PCC Analysis/pcc25-26-main/Return forms"

for (s3_path in files$path) {
  local_path <- file.path(folder, basename(s3_path))
  Rs3tools::download_file_from_s3(
    s3_path = s3_path, 
    local_path = local_path, 
    overwrite = TRUE
  )
}



