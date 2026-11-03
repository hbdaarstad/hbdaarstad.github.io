# Builds the web-sized copies of your gallery photos.
#
# How to add photos:
#   1. Drop the full-size photos into a folder in photos/originals/
#      (an existing one, or a new one you also add to photos/sections.yml).
#   2. Run this script in RStudio (Source button) from the project folder.
#   3. Render the site (quarto render) and push.
#
# For each original it makes:
#   photos/gallery/<section>/thumb/<date>_<name>.jpg   600px wide, shown in the grid
#   photos/gallery/<section>/full/<date>_<name>.jpg    2000px max, shown when clicked
# Photos are rotated correctly and stripped of metadata (including GPS location).
# Only new photos are processed, so re-running is quick. Web copies whose original
# was deleted are removed too, so deleting a photo = delete it from originals + re-run.
#
# Needs: install.packages("magick")

library(magick)

orig_root <- "photos/originals"
web_root  <- "photos/gallery"
if (!dir.exists(orig_root)) stop("Run this from the website project folder (the one with _quarto.yml).")

web_name <- function(path, img) {
  attrs <- image_attributes(img)
  d <- c(attrs$value[attrs$property == "exif:DateTimeOriginal"],
         attrs$value[attrs$property == "exif:DateTime"])[1]
  date <- if (is.na(d) || !nzchar(d)) "undated" else gsub(":", "-", substr(d, 1, 10))
  slug <- tolower(tools::file_path_sans_ext(basename(path)))
  slug <- gsub("^-|-$", "", gsub("[^a-z0-9]+", "-", slug))
  paste0(date, "_", slug, ".jpg")
}

made <- 0; keep <- character()
for (sec in list.dirs(orig_root, recursive = FALSE, full.names = FALSE)) {
  files <- list.files(file.path(orig_root, sec), pattern = "\\.(jpe?g|png|heic)$",
                      ignore.case = TRUE, full.names = TRUE)
  for (f in files) {
    img  <- image_read(f)
    name <- web_name(f, img)
    thumb <- file.path(web_root, sec, "thumb", name)
    full  <- file.path(web_root, sec, "full",  name)
    keep <- c(keep, thumb, full)
    if (file.exists(thumb) && file.exists(full)) next
    dir.create(dirname(thumb), recursive = TRUE, showWarnings = FALSE)
    dir.create(dirname(full),  recursive = TRUE, showWarnings = FALSE)
    img <- image_strip(image_orient(img))
    image_write(image_resize(img, "2000x2000>"), full,  format = "jpeg", quality = 82, flatten = TRUE)
    image_write(image_resize(img, "600x1200>"),  thumb, format = "jpeg", quality = 80, flatten = TRUE)
    made <- made + 1
    message("  added ", sec, "/", name)
  }
}

# remove web copies whose original is gone
existing <- list.files(web_root, pattern = "\\.jpg$", recursive = TRUE, full.names = TRUE)
orphans <- setdiff(existing, keep)
if (length(orphans)) { file.remove(orphans); message("Removed ", length(orphans), " old web copies.") }

# make sure the gallery page re-runs on the next render/preview
# (otherwise Quarto can reuse a cached copy from before these photos existed)
unlink(".quarto/_freeze/gallery", recursive = TRUE)
Sys.setFileTime("gallery.qmd", Sys.time())

message("Done: ", made, " new photo(s) processed.")
