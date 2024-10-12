import argparse
from PIL import Image, ImageDraw
import os

def convert_to_bitmap(image_path, size=(12, 10)):
    # Open the image
    img = Image.open(image_path)
    
    # Convert to grayscale
    img = img.convert('L')
    
    # Resize to the target size if it's not already
    img = img.resize(size, Image.NEAREST)
    
    # Convert to 1-bit color (black and white)
    bitmap_img = img.point(lambda p: p > 128 and 255)  # Threshold to binarize
    bitmap_img = bitmap_img.convert('1')  # Convert to 1-bit
    
    return bitmap_img

def create_composite_image(original_img, bitmap_img, pixel_width=10, pixel_height=12):
    # Create a new image wide enough to hold both the original and bitmap representations
    width, height = original_img.size
    new_width = width + (bitmap_img.width * pixel_width)
    new_height = max(height, bitmap_img.height * pixel_height)
    
    # Create the new composite image
    composite_img = Image.new('RGB', (new_width, new_height), (255, 255, 255))
    
    # Paste the original image on the left
    composite_img.paste(original_img, (0, 0))
    
    # Draw the bitmap representation using rectangles with the specified pixel aspect ratio
    draw = ImageDraw.Draw(composite_img)
    for y in range(bitmap_img.height):
        for x in range(bitmap_img.width):
            # Determine the color based on the bitmap pixel (0: black, 1: white)
            color = (0, 0, 0) if bitmap_img.getpixel((x, y)) == 0 else (255, 255, 255)
            # Draw the corresponding rectangle
            draw.rectangle(
                [
                    (width + x * pixel_width, y * pixel_height),
                    (width + (x + 1) * pixel_width, (y + 1) * pixel_height)
                ],
                fill=color
            )
    
    # Draw a grid over the bitmap representation
    for y in range(0, bitmap_img.height * pixel_height, pixel_height):
        draw.line([(width, y), (new_width, y)], fill=(0, 0, 0), width=1)
    for x in range(width, new_width, pixel_width):
        draw.line([(x, 0), (x, new_height)], fill=(0, 0, 0), width=1)
    
    return composite_img

def enlarge_image(image, factor=8):
    # Enlarge the image by repeating each pixel 'factor' times
    enlarged_image = image.resize(
        (image.width * factor, image.height * factor),
        Image.NEAREST
    )
    return enlarged_image

def process_images(image_paths, output_dir, pixel_width=8, pixel_height=14, enlargement_factor=8):
    if not os.path.exists(output_dir):
        os.makedirs(output_dir)

    font_image = Image.new('1', (12*16, 10*6), 0)
    glyph_no = 0

    for image_path in sorted(image_paths, key=lambda x: int(os.path.splitext(os.path.basename(x))[0].split('_')[1])):
        # Open the original image
        original_img = Image.open(image_path)
        
        # Convert to bitmap
        bitmap_img = convert_to_bitmap(image_path)

        # Paste bitmap into font_image
        font_image.paste(bitmap_img, (glyph_no % 16 * 12, glyph_no // 16 * 10))
        glyph_no += 1

        # Create the composite image
        composite_img = create_composite_image(original_img, bitmap_img, pixel_width=pixel_width, pixel_height=pixel_height)
        
        # Enlarge the composite image
        enlarged_composite_img = enlarge_image(composite_img, factor=enlargement_factor)
        
        # Create a meaningful output filename
        filename = os.path.basename(image_path)
        output_path = os.path.join(output_dir, f"{os.path.splitext(filename)[0]}_composite_enlarged.png")
        
        # Save the result
        enlarged_composite_img.save(output_path)
        print(f"Processed {image_path} -> {output_path}")

    font_image.save('bitmap.png')

def main():
    # Set up argument parser
    parser = argparse.ArgumentParser(description='Create composite images showing original and bitmap representations, enlarged by pixel repetition.')
    parser.add_argument('output_dir', type=str, help='Directory where output images will be saved')
    parser.add_argument('images', nargs='+', help='List of image file paths to process')
    
    # Parse arguments
    args = parser.parse_args()
    
    # Process images
    process_images(args.images, args.output_dir)

if __name__ == '__main__':
    main()
