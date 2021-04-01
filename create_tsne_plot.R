# Any questions of suggestions? Please write me.

# Prerequisites_
# Import data set from https://github.com/MuseumofModernArt/collection
# Download images

library(Rtsne)
library(imager)
faktor = 500 #size of thumbnails
pathtoimages=""

# Create set with features from data frame
art_sub = subset(
  Artworks,
  select = c(
    ObjectID,
    BeginDate,
    EndDate,
    DateAcquired,
    Circumference..cm.,
    Depth..cm.,
    Diameter..cm.,
    Height..cm.,
    Length..cm.,
    Weight..kg.,
    Width..cm.,
    Duration..sec..
  )
)  

# Convert to numbers
art_sub$DateAcquired = as.numeric(substr(art_sub$DateAcquired, 1, 4))
art_sub$BeginDate = as.numeric(substr(art_sub$BeginDate, 2, 5))
art_sub$EndDate = as.numeric(substr(art_sub$EndDate, 2, 5))

# tsne with only first 1000
art_sub_use = art_sub_impute[1:1000, ]

# Crete t-SNE map
  tsne_out = Rtsne(
    art_sub_use[, 1:12],
    dims = 2,
    check_duplicates = FALSE,
    theta = 0.4
  )
  
# Create imageplot
  png(
    filename = paste0(format(Sys.time(), "%H%M%S"), "-tsne_MoMa.png"),
    width = 19200 / 20,
    height = 10800 / 20,
    pointsize = 24,
    bg = "white"
  )
  
# Create empty plot
  plot(   
    c(min(tsne_out$Y[, 1]), max(tsne_out$Y[, 1])),
    c(min(tsne_out$Y[, 1]), max(tsne_out$Y[, 1])),
    type = "n",
    axes = FALSE,
    xlab = "",
    ylab = "",
    main = "1.000 MoMA objects, t-sne imageplot (H. Klinke)",
    cex.main = 1
  )

# Place images
  for (i in (1:(nrow(tsne_out$Y) - 0))) {
    filename = paste(
      pathtoimages,    
      art_sub_use$ObjectID[i],
      ".jpg",
      sep = ""
    )
    
    if (file.exists(filename)) {
      image = load.image(filename)
    
      rasterImage(
        image,
        tsne_out$Y[i, 1],
        tsne_out$Y[i, 2],
        tsne_out$Y[i, 1] + dim(image)[1] / faktor,
        tsne_out$Y[i, 2] + dim(image)[2] / faktor * 2.5
      )
    } else  {
      message(i, ": no image found")
    }
  }
  dev.off()
 
