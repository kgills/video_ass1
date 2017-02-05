library(png)
source("video_ass1_functions.r")

# Read in the original image
orig=readPNG("original.png")

# Convert original image to 8-bit
orig = float_8bit(orig)

# # Create the bright image
orig50 = add_bright(orig, 50)

# Convert the images from RGB to HSV
origHSV = rgb_hsv(orig)
orig50HSV = rgb_hsv(orig50)

# Detect the red apple
orig_detect=edge_red_rgb(orig, 100, 1.75)
orig50_detect=edge_red_rgb(orig50, 150, 1.75)
origHSV_detect=edge_red_hsv(origHSV, 20)
orig50HSV_detect=edge_red_hsv(orig50HSV, 20)

# Swap the red and green
orig_swap = swap_rgb(orig)

# Save the images
png("orig_working.png")
print_bit(orig)
dev.off()

png("orig50.png")
print_bit(orig50)
dev.off()

png("origHSV.png")
print_bit(origHSV)
dev.off()

png("orig50HSV.png")
print_bit(orig50HSV)
dev.off()

png("orig_detect.png")
print_bit(orig_detect)
dev.off()

png("orig50_detect.png")
print_bit(orig50_detect)
dev.off()

png("origHSV_detect.png")
print_bit(origHSV_detect)
dev.off()

png("orig50HSV_detect.png")
print_bit(orig50HSV_detect)
dev.off()

png("orig_swap.png")
print_bit(orig_swap)
dev.off()
