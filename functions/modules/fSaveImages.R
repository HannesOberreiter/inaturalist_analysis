# simple function to save images
fSaveImages <- function(filename, currentplot, w = 7.5, h = 6) {
  ggsave(paste0("output/figs/", filename, ".pdf"), currentplot, width = w, height = h)
  ggsave(paste0("output/figs/", filename, ".png"), currentplot, width = w, height = h, dpi = 320)
}