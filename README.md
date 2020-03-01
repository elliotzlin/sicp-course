# sicp-course
A place to keep my files and track my progress as I work through SICP.

# 1. Build the Docker image
`docker build -t <image-name>:<tag> .`

# 2. Run a container, mounting in this directory.
`docker run --rm -it -v /path/to/this/dir:/home/dev/sicp --name=<container-name> <image-name>:<tag>`

# 3. Exiting
Just type `exit`.
