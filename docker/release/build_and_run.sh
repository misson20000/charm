#!/bin/bash

# Define variables
DOCKER_IMAGE_NAME="charm-builder"
OUTPUT_DIR="./output"

# Step 1: Build the Docker image
echo "Building Docker image..."
docker build -t $DOCKER_IMAGE_NAME .

# Check if the build was successful
if [ $? -ne 0 ]; then
    echo "Docker image build failed."
    exit 1
fi

# Step 2: Create the output directory if it doesn't exist
if [ ! -d $OUTPUT_DIR ]; then
    mkdir -p $OUTPUT_DIR
fi

# Step 3: Run the Docker container to build the Rust project and output the build files
echo "Running Docker container..."
docker run --rm -v $(pwd)/output:/output $DOCKER_IMAGE_NAME

# Check if the container run was successful
if [ $? -ne 0 ]; then
    echo "Docker container run failed."
    exit 1
fi

echo "Build completed successfully. Output files are located in the 'output' directory."
