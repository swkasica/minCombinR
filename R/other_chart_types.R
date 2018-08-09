#Helper functions for other chart types

# Tables
#x is rows, y is columns
plot_table <- function(data, flip_coord=FALSE, rownames=NA, x_limits=NA, y_limits=NA) {

  if (!is.na(rownames)) {
    data <- as.data.frame(data)
    rownames(data) <- data[ , rownames]
    data[ , rownames] <- NULL
  }

  if (flip_coord) {
    data <- t(data)
  }

  if(!is.na(x_limits)[1]) {
    #TODO: add the rows to the limits and reorder the rows here
  }

  if(!is.na(y_limits[1])) {
    #TODO!!!
  }

  gridExtra::tableGrob(data)
}

#Category Stripe
plot_category_stripe <- function(data, x, category, x_limits=NA) {
  gg_chart <- ggplot(data, aes_string(x=x, y=shQuote("categories"), fill=category)) +
    geom_bin2d() +
    theme(axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          # axis.text.x = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
          panel.background = element_blank(),
          legend.position = "none")
    # guides(fill=guide_legend(title = category))

  if(!is.na(x_limits)[1]) {
    gg_chart <- gg_chart + xlim(x_limits)
  }

  gg_chart
}

#Image

plot_image <- function(path) {
  cowplot::ggdraw() + cowplot::draw_image(path)

  # img_type <- strsplit(path, '[.]')[[1]]
  # if (img_type == "jpg" || img_type == "jpeg") {
  #   jpeg::readJPEG(source=sourcejpeg, ...)
  # }
  # else if (img_type == "png") {
  #   png::readPNG(source=sourcepng, ...)
  # }
  # else if (img_type == "pdf") {
  #   tm::readPDF(source=sourcepdf, ...)
  # }
}

# plot_jpeg_image <- function(sourcejpeg) {
#   jpeg::readJPEG(source=sourcejpeg)
# }
#
# plot_png_image <- function(sourcepng) {
#   png::readPNG(source=sourcepng)
# }
#
# plot_pdf_image <- function(sourcepdf) {
#   tm::readPDF(source=sourcepdf)
# }

