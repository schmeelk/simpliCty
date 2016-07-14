#!/bin/bash
#
# Used to call the docker build function from command line with no CPU restrictions
# and also removes container images when build complete.

docker build --ulimit core=-1 -f Dockerfile --no-cache -t simplicty/buildenv .
