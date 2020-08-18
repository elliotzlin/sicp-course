# sicp-course
A place to keep my files and track my progress as I work through SICP.

# 1. Build the Docker image
`docker build -t <image-name>:<tag> .`

# 2. Run a container, mounting in this directory.
`docker run --rm -it -v /path/to/this/dir:/home/dev/sicp --detach-keys=ctrl-z,z --name=<container-name> <image-name>:<tag>`

Note I'm passing `--detach-keys` to the `docker run` command in order for
certain emacs key bindings to work. Or you can set this in your Docker
configs. Or you can just use a different text editor.

# 3. Exiting
Just type `exit`.
