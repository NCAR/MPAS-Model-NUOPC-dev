FROM ubuntu:rolling

# Set environment variables
ENV SPACK_ROOT=/opt/spack \
    DEBIAN_FRONTEND=noninteractive

# Install system dependencies required by spack
RUN <<INSTALL_PACKAGES
    apt-get update
    apt-get install -yqq \
        build-essential \
        ca-certificates \
        curl \
        file \
        g++-14 \
        gcc-14 \
        gfortran-14 \
        git \
        gpg \
        gzip \
        libc6-dev \
        libcurl4-openssl-dev \
        libssl-dev \
        m4 \
        make \
        openssh-client \
        pkg-config \
        python3 \
        python3-dev \
        tar \
        unzip \
        wget \
        zlib1g-dev
    apt-get clean
INSTALL_PACKAGES

# Clone spack repository
RUN <<INSTALL_SPACK
    mkdir -p ${SPACK_ROOT}
    git clone --depth=2 --branch=releases/v1.1 https://github.com/spack/spack.git ${SPACK_ROOT}
INSTALL_SPACK

# Set up spack environment
ENV PATH="${SPACK_ROOT}/bin:${PATH}"

# Install MPAS dependencies with gcc 12
RUN <<SETUP_SPACK
    . ${SPACK_ROOT}/share/spack/setup-env.sh
    spack compiler find
    spack install \
        mpich@4.3%gcc \
        netcdf-c@4.9%gcc \
        netcdf-fortran@4.6%gcc \
        parallel-netcdf@1.14%gcc \
        parallelio@2.6%gcc \
        esmf@8.9%gcc
    spack clean --all
SETUP_SPACK

# Create environment
RUN <<SETUP_ENV
    mkdir -p /etc/profile.d
    mkdir -p /home/mpas-dev
SETUP_ENV

# Copy spack setup script
COPY --chmod=0755 mpas_spack.sh /etc/profile.d/mpas_spack.sh

# Copy README file
COPY --chmod=0644 README.md /home/mpas-dev/README.md

# Set bash as default shell
SHELL ["/bin/bash", "-c"]

# Set working directory
WORKDIR /home/mpas-dev

# Default command
CMD ["/bin/bash", "-l"]
