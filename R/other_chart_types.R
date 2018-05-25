#Helper functions for other chart types

# Tables
plot_table <- function(data) {
  grid::grid.newpage()
  gridExtra::grid.table(data)
}

#Category Stripe
plot_category_stripe <- function(data, category) {
  ggplot(data, aes(x=rownames(data), y="categories", fill=category)) +
    geom_bin2d() +
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_blank())
}

#Image
plot_jpeg_image <- function(sourcejpeg) {
  jpeg::readJPEG(source=sourcejpeg)
}

plot_png_image <- function(sourcepng) {
  png::readPNG(source=sourcepng)
}

plot_pdf_image <- function(sourcepdf) {
  tm::readPDF(source=sourcepdf)
}

