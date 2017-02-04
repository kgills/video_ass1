library(png)

original=readPNG("original.png")
orig_bright=original

# Add 50/256 to each pixel value
for (i in seq_len(dim(original)[1])) {
    for (j in seq_len(dim(original)[2])) {
        for (k in seq_len(dim(original)[3])) {

        	# Convert original image to 8-bit color
            # original[i,j,k] = as.integer(original[i,j,k] * 255)

            # Increase the brightness
            # orig_bright[i,j,k] = original[i,j,k]+50
            # if(orig_bright[i,j,k] > 255) {
            # 	orig_bright[i,j,k] = 255
            # }





            # Convert back to double
            # orig_bright[i,j,k] = as.double(orig_bright[i,j,k]/255)
            # original[i,j,k] = as.double(original[i,j,k]/255)



            # Increase the brightness
            orig_bright[i,j,k] = original[i,j,k]+(50/256)
            if(orig_bright[i,j,k] > 255) {
            	orig_bright[i,j,k] = 255
            }
            
        }
    }
}

# grid::grid.raster(original)
# grid::grid.raster(orig_brighter)