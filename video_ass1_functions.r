rgb_hsv <- function(rgb) {

    hsv = rgb
    for (i in seq_len(dim(rgb)[1])) {
        for (j in seq_len(dim(rgb)[2])) {

            rgb_max = max(rgb[i,j,])
            rgb_min = min(rgb[i,j,])
            rgb_delta = rgb_max - rgb_min

            # Calculate V
            if(rgb_max == 0) {
                hsv[i,j,1] = 0
                hsv[i,j,2] = 0
                hsv[i,j,3] = 0
                continue
            }
            hsv[i,j,3] = rgb_max

            # Calculate S
            hsv[i,j,2] = as.integer((rgb_delta/rgb_max) * 255)

            # Calculate H
            if(rgb_delta == 0) {
                hsv[i,j,1] = 0
            } else if(rgb[i,j,1] == rgb_max) {
                hsv[i,j,1] = as.integer(0 + 43 * (rgb[i,j,2] - rgb[i,j,3])/(rgb_delta))
            } else if(rgb[i,j,2] == rgb_max)  {
                hsv[i,j,1] = as.integer(85 + 43 * (rgb[i,j,3] - rgb[i,j,1])/(rgb_delta))
            } else {
                hsv[i,j,1] = as.integer(171 + 43 * (rgb[i,j,1] - rgb[i,j,2])/(rgb_delta))
            }

            # Wrap the values
            hsv[i,j,1] = hsv[i,j,1] %% 256 
            hsv[i,j,2] = hsv[i,j,2] %% 256 
            hsv[i,j,3] = hsv[i,j,3] %% 256 
        }
    }

    return(hsv)
}

float_8bit <- function(float) {
    for (i in seq_len(dim(float)[1])) {
        for (j in seq_len(dim(float)[2])) {
            for (k in seq_len(dim(float)[3])) {

                # Convert orig image to 8-bit color
                float[i,j,k] = as.integer(float[i,j,k] * 255)
            }
        }
    }

    return(float)
}

bit_float <- function(bit) {
    for (i in seq_len(dim(bit)[1])) {
        for (j in seq_len(dim(bit)[2])) {
            for (k in seq_len(dim(bit)[3])) {

                # Convert orig image to floating
                bit[i,j,k] = as.double(bit[i,j,k] / 255)
            }
        }
    }

    return(bit)
}

add_bright <- function(img, val) {
    for (i in seq_len(dim(img)[1])) {
        for (j in seq_len(dim(img)[2])) {
            for (k in seq_len(dim(img)[3])) {

                # Increase the brightness
                img[i,j,k] = img[i,j,k]+val
                if(img[i,j,k] > 255) {
                    img[i,j,k] = 255
                } 
            }
        }
    }

    return(img)
}

edge_red_rgb <- function(img, thresh, factor) {

    output = img

    x_vals = c()
    y_vals = c()

    # Go in the x diretion
    for (j in seq_len(dim(img)[2])) {
        for (i in seq_len(dim(img)[1])) {

            # Pixel is above red thresh and has much more red than other colors
            if((i > 3) && (i < (dim(img)[1] - 3)) && img[seq((i-3),(i+3)),j,1] > thresh &&
                img[seq((i-3),(i+3)),j,1] > (factor*img[seq((i-3),(i+3)),j,2]) && 
                img[seq((i-3),(i+3)),j,1] > (factor*img[seq((i-3),(i+3)),j,3])) {

                x_vals =c(x_vals, i)
            }
        }
    }  

    # Go in the y direction
    for (j in seq_len(dim(img)[1])) {
        for (i in seq_len(dim(img)[2])) {

            # Pixel is above red thresh and has much more red than other colors
            if((i > 3) && (i < (dim(img)[2] - 3)) && img[j, seq((i-3),(i+3)),1] > thresh &&
                img[j, seq((i-3),(i+3)),1] > (factor*img[j, seq((i-3),(i+3)),2]) && 
                img[j, seq((i-3),(i+3)),1] > (factor*img[j, seq((i-3),(i+3)),3])) {

                y_vals =c(y_vals, i)
            }
        }
    }

    # Remove the outliers
    x_vals = x_vals[!x_vals %in% boxplot.stats(x_vals)$out]
    y_vals = y_vals[!y_vals %in% boxplot.stats(y_vals)$out]

    # Figure out the average value or all of the changed pixels
    avg_x = mean(x_vals)
    avg_y = mean(y_vals)
    range_x = 2*IQR(x_vals)

    # Draw the lines
    output[seq(avg_x-(range_x/2), avg_x+(range_x/2)), seq(avg_y+range_x/2, avg_y+range_x/2+3),1] = 0
    output[seq(avg_x-(range_x/2), avg_x+(range_x/2)), seq(avg_y+range_x/2, avg_y+range_x/2+3),2] = 255
    output[seq(avg_x-(range_x/2), avg_x+(range_x/2)), seq(avg_y+range_x/2, avg_y+range_x/2+3),3] = 255

    output[seq(avg_x-(range_x/2), avg_x+(range_x/2)), seq(avg_y-range_x/2, avg_y-range_x/2-3),1] = 0
    output[seq(avg_x-(range_x/2), avg_x+(range_x/2)), seq(avg_y-range_x/2, avg_y-range_x/2-3),2] = 255
    output[seq(avg_x-(range_x/2), avg_x+(range_x/2)), seq(avg_y-range_x/2, avg_y-range_x/2-3),3] = 255

    output[seq(avg_x+(range_x/2),avg_x+(range_x/2)+3), seq(avg_y-range_x/2, avg_y+range_x/2),1] = 0
    output[seq(avg_x+(range_x/2),avg_x+(range_x/2)+3), seq(avg_y-range_x/2, avg_y+range_x/2),2] = 255
    output[seq(avg_x+(range_x/2),avg_x+(range_x/2)+3), seq(avg_y-range_x/2, avg_y+range_x/2),3] = 255

    output[seq(avg_x-(range_x/2),avg_x-(range_x/2)-3), seq(avg_y-range_x/2, avg_y+range_x/2),1] = 0
    output[seq(avg_x-(range_x/2),avg_x-(range_x/2)-3), seq(avg_y-range_x/2, avg_y+range_x/2),2] = 255
    output[seq(avg_x-(range_x/2),avg_x-(range_x/2)-3), seq(avg_y-range_x/2, avg_y+range_x/2),3] = 255

    return(output)
}

edge_red_hsv <- function(img, range) {

    output = img

    x_vals = c()
    y_vals = c()

    # Go in the x diretion
    for (j in seq_len(dim(img)[2])) {
        for (i in seq_len(dim(img)[1])) {

            # Pixel is in the hue range
            if((i > 3) && (i < (dim(img)[1] - 3)) && ((img[seq((i-3),(i+3)),j,1] < range) || 
                (img[seq((i-3),(i+3)),j,1] > 255 - range))) {

                x_vals =c(x_vals, i)
            }
        }
    }  

    # Go in the y direction
    for (j in seq_len(dim(img)[1])) {
        for (i in seq_len(dim(img)[2])) {

            # Pixel is in the hue range
            if((i > 3) && (i < (dim(img)[2] - 3)) && ((img[j, seq((i-3),(i+3)),1] < range) || 
                (img[j, seq((i-3),(i+3)),1] > 255 - range))) {

                y_vals =c(y_vals, i)
            }
        }
    }

    # Remove the outliers
    x_vals = x_vals[!x_vals %in% boxplot.stats(x_vals)$out]
    y_vals = y_vals[!y_vals %in% boxplot.stats(y_vals)$out]

    # Figure out the average value or all of the changed pixels
    avg_x = mean(x_vals)
    avg_y = mean(y_vals)
    range_x = 2*IQR(x_vals)

    # Draw the lines
    output[seq(avg_x-(range_x/2), avg_x+(range_x/2)), seq(avg_y+range_x/2, avg_y+range_x/2+3),1] = 0
    output[seq(avg_x-(range_x/2), avg_x+(range_x/2)), seq(avg_y+range_x/2, avg_y+range_x/2+3),2] = 255
    output[seq(avg_x-(range_x/2), avg_x+(range_x/2)), seq(avg_y+range_x/2, avg_y+range_x/2+3),3] = 255

    output[seq(avg_x-(range_x/2), avg_x+(range_x/2)), seq(avg_y-range_x/2, avg_y-range_x/2-3),1] = 0
    output[seq(avg_x-(range_x/2), avg_x+(range_x/2)), seq(avg_y-range_x/2, avg_y-range_x/2-3),2] = 255
    output[seq(avg_x-(range_x/2), avg_x+(range_x/2)), seq(avg_y-range_x/2, avg_y-range_x/2-3),3] = 255

    output[seq(avg_x+(range_x/2),avg_x+(range_x/2)+3), seq(avg_y-range_x/2, avg_y+range_x/2),1] = 0
    output[seq(avg_x+(range_x/2),avg_x+(range_x/2)+3), seq(avg_y-range_x/2, avg_y+range_x/2),2] = 255
    output[seq(avg_x+(range_x/2),avg_x+(range_x/2)+3), seq(avg_y-range_x/2, avg_y+range_x/2),3] = 255

    output[seq(avg_x-(range_x/2),avg_x-(range_x/2)-3), seq(avg_y-range_x/2, avg_y+range_x/2),1] = 0
    output[seq(avg_x-(range_x/2),avg_x-(range_x/2)-3), seq(avg_y-range_x/2, avg_y+range_x/2),2] = 255
    output[seq(avg_x-(range_x/2),avg_x-(range_x/2)-3), seq(avg_y-range_x/2, avg_y+range_x/2),3] = 255

    return(output)
}

print_bit <- function(img) {
    for (i in seq_len(dim(img)[1])) {
        for (j in seq_len(dim(img)[2])) {
            for (k in seq_len(dim(img)[3])) {

                # Convert orig image to floating
                img[i,j,k] = as.double(img[i,j,k] / 255)
            }
        }
    }

    grid::grid.raster(img)
}

swap_rgb <- function(img) {
    output = img
    for (i in seq_len(dim(img)[1])) {
        for (j in seq_len(dim(img)[2])) {

            # Convert orig image to floating
            output[i,j,1] = img[i,j,2]
            output[i,j,2] = img[i,j,1]
        }
    }

    return(output)
}