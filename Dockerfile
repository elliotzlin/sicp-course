FROM ubuntu:18.04

# Install build dependencies for compiling MIT Scheme:
#   - gcc
#   - libncurses-dev
#   - m4
#   - make
# And install some additional tools:
#   - curl
#   - emacs
#   - git
ARG APT_PACKAGES='\
  curl \
  emacs \
  gcc \
  libncurses-dev \
  m4 \
  make \
'

RUN apt-get update && apt-get install -y $APT_PACKAGES

# Install MIT Scheme
ARG MITSCM_VER=10.1.10
ARG MITSCM_ARCH=x86-64
ARG MITSCM_URL=http://ftp.gnu.org/gnu/mit-scheme/stable.pkg/${MITSCM_VER}/mit-scheme-${MITSCM_VER}-${MITSCM_ARCH}.tar.gz
RUN curl -s $MITSCM_URL | tar xvz && \
    cd mit-scheme-${MITSCM_VER}/src && \
    ./configure && \
    make && \
    make install

# Remove Scheme source code.
RUN rm -rf mit-scheme-${MITSCM_VER}

# Create a default non-root user for safety reasons. Probably best to
# not run everything as root at runtime.
ARG USERNAME=dev
ARG USERID=50000
RUN adduser --disabled-password --disabled-login --gecos "" -uid $USERID $USERNAME

# Switch to non-root user for safer runtime.
USER $USERNAME

# Start in home directory.
WORKDIR /home/$USERNAME

ENTRYPOINT ["/bin/bash"]
