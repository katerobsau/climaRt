#crearing working directory
dir.create("examples")
setwd("examples") 

#Creating countdown .png files from 10 to "GO!"
png(file="example%02d.png", width=200, height=200)
for (i in c(10:1, "G0!")){
  plot.new()
  text(.5, .5, i, cex = 6)
}
dev.off()

# Converting .png files in one .gif image using ImageMagick
system("convert -delay 80 *.png example_1.gif")
# Remove .png files from working directory
file.remove(list.files(pattern=".png"))
