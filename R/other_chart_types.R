#Helper functions for other chart types

# Tables
plot_table <- function(data) {
  grid::grid.newpage()
  gridExtra::grid.table(data)
}

#Category Stripe
plot_category_stripe <- function(data, category) {
  ggplot(data, aes(x=rownames(data), y="categories", fill=shQuote(data[[category]]))) +
    geom_bin2d() +
    theme(axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.x = element_blank(),
          panel.background = element_blank()) +
    guides(fill=guide_legend(title = category))
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

