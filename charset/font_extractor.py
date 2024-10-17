import os

import cv2
import numpy as np
import argparse

# Parse the command-line arguments
parser = argparse.ArgumentParser(description='Interactive grid editor for font extraction.')
parser.add_argument('image_path', type=str, help='Path to the input font screenshot image.')
args = parser.parse_args()

# Load the image
image_path = args.image_path
image = cv2.imread(image_path, cv2.IMREAD_GRAYSCALE)

if image is None:
    print(f"Error: Could not load the image at {image_path}. Please check the file path.")
    exit(1)

# Parameters for grid dimensions (initialize with defaults)
base_cell_width = 95
base_cell_height = 138

cell_width = base_cell_width
cell_height = base_cell_height
origin_x = 438
origin_y = 341

# Define reasonable maximum values for the trackbars, ensuring they are at least 1
max_width = 200
max_height = 200
max_origin_x = image.shape[1]
max_origin_y = image.shape[0]

initialized = False

# Function to draw the grid on the image
def draw_grid(img, cell_w, cell_h, orig_x, orig_y):
    img_with_grid = img.copy()

    h, w = img.shape

    # Draw vertical lines
    for x in np.arange(orig_x, orig_x + cell_w * 17, cell_w):
        cv2.line(img_with_grid, (round(x), 0), (round(x), h), (255, 0, 0), 1)

    # Draw horizontal lines
    for y in np.arange(orig_y, orig_y + cell_h * 7, cell_h):
        cv2.line(img_with_grid, (0, round(y)), (w, round(y)), (255, 0, 0), 1)

    return img_with_grid

# Callback function for the trackbars
def update_grid(val):
    global initialized
    if not initialized:
        return
    
    global cell_width, cell_height, origin_x, origin_y

    # Retrieve the trackbar positions
    cell_width = base_cell_width + cv2.getTrackbarPos('Cell Width', 'Grid Editor') / 10.0
    cell_height = base_cell_height + cv2.getTrackbarPos('Cell Height', 'Grid Editor') / 10.0
    origin_x = cv2.getTrackbarPos('Origin X', 'Grid Editor')
    origin_y = cv2.getTrackbarPos('Origin Y', 'Grid Editor')

    # Redraw the image with the updated grid
    img_with_grid = draw_grid(image, cell_width, cell_height, origin_x, origin_y)
    cv2.imshow('Grid Editor', img_with_grid)

# Create a window
cv2.namedWindow('Grid Editor')

# Create trackbars for grid parameters with reasonable ranges
cv2.createTrackbar('Cell Width', 'Grid Editor', cell_width, max_width, update_grid)
cv2.setTrackbarPos('Cell Width', 'Grid Editor', 11)
cv2.createTrackbar('Cell Height', 'Grid Editor', cell_height, max_height, update_grid)
cv2.setTrackbarPos('Cell Height', 'Grid Editor', 42)
cv2.createTrackbar('Origin X', 'Grid Editor', origin_x, max_origin_x, update_grid)
cv2.createTrackbar('Origin Y', 'Grid Editor', origin_y, max_origin_y, update_grid)
initialized = True

# Manually call update_grid once to initialize the display
update_grid(0)

# Display the initial grid
cv2.imshow('Grid Editor', draw_grid(image, cell_width, cell_height, origin_x, origin_y))

# Wait until user closes the window
cv2.waitKey(0)
cv2.destroyAllWindows()

# Function to slice the image into individual characters
def slice_image(img, cell_w, cell_h, orig_x, orig_y):
    characters = []

    for y in np.arange(orig_y, orig_y + cell_h * 6, cell_h):
        for x in np.arange(orig_x, orig_x + cell_w * 16, cell_w):
            char_img = img[y:y+cell_h, x:x+cell_w]
            characters.append(char_img)

    return characters

# Slice the image based on the final grid settings
characters = slice_image(image, round(cell_width), round(cell_height), origin_x, origin_y)

output_dir = os.path.splitext(image_path)[0]
if not os.path.exists(output_dir):
    os.makedirs(output_dir)

# Save the individual characters as separate images
for idx, char_img in enumerate(characters):
    cv2.imwrite(f'{output_dir}/character_{idx}.png', char_img)

print(f'wrote images to {output_dir}')
