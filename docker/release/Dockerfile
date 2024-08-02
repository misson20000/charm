# Use the official Ubuntu 24.04 image as the base
FROM ubuntu:24.04

# Set environment variables to non-interactive to avoid prompts during apt-get install
ENV DEBIAN_FRONTEND=noninteractive

# Update and install required dependencies
RUN apt-get update && apt-get install -y \
    libgtk-4-dev \
    libgdk-pixbuf2.0-dev \
    libatk1.0-dev \
    libcairo2-dev \
    libjpeg8-dev \
    libpango1.0-dev \
    libgif-dev \
    libgraphene-1.0-dev \
    build-essential \
    g++ \
    curl \
    git \
    pkg-config \
    && rm -rf /var/lib/apt/lists/*

# Install Rust
RUN curl https://sh.rustup.rs -sSf | sh -s -- -y
ENV PATH="/root/.cargo/bin:${PATH}"

# Clone the GitHub repo
RUN git clone https://github.com/misson20000/charm.git /charm

# Set the working directory
WORKDIR /charm

# Build the repo using Cargo in release mode
RUN cargo build --release -F gtk

# Output the resulting build file
CMD ["sh", "-c", "cp -r /charm/target/* /output && ls /output"]

# Set the output directory
VOLUME ["/output"]