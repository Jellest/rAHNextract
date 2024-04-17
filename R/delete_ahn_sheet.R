#'@inheritParams ahn_area
#'@noRd
delete_ahn_sheet <- function(sheets) {
  for (sr in seq_along(sheets$kaartBlad)) {
    if (sheets[sr, "downloadedNow"] == TRUE) {
      file.remove(sheets[sr, "filePath"])
      print(paste("Removed", sheets[sr, "kaartBlad"], "successfully."))
    } else {
      warning(paste("Kaartblad", sheets[sr, "kaartBlad"], "already existed and was not downloaded now. Therefore it will not be removed.", sep = " "))
    }
  }
}