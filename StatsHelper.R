raw_data_path <- "../tax-service-opendata/rsmp/xml"

raw_files <- list.files(raw_data_path, pattern = "*.zip", full.names = TRUE)
raw_files_count <- length(raw_files)
raw_files_count

raw_files_size_gb <- sum(file.size(raw_files)) / 2 ** 30
raw_files_size_gb

raw_files_uncompressed_size_tb <- sum(sapply(
  raw_files,
  function(path) {
    sum(unzip(path, list = TRUE)$Length)
})) / 2 ** 40
raw_files_uncompressed_size_tb
prettyNum(raw_files_uncompressed_size_tb, digits = 2)
