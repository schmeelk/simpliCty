############################################################
# Dockerfile to build LLVM 3.8.0 container images
# Based on Ubuntu
############################################################
FROM ubuntu16.04:LLVM3.8.0

MAINTAINER Rui Gu

RUN apt-get update && apt-get -y install build-essential subversion python2.7-dev libedit-dev libncurses5-dev cmake inotify-tools fdupes libxml2-dev swig wget

RUN mkdir -p ~/Workspace

RUN mkdir -p ~/Downloads

RUN cd ~/Downloads && wget http://llvm.org/releases/3.8.0/llvm-3.8.0.src.tar.xz

RUN cd ~/Downloads && wget http://llvm.org/releases/3.8.0/cfe-3.8.0.src.tar.xz

RUN cd ~/Downloads && tar -xf llvm-3.8.0.src.tar.xz -C ~/Workspace/

RUN cd ~/Downloads && tar -xf cfe-3.8.0.src.tar.xz -C ~/Workspace/llvm-3.8.0.src/tools/

RUN cd ~/Workspace/llvm-3.8.0.src/tools && mv cfe-3.8.0.src clang

RUN cd ~/Workspace/llvm-3.8.0.src && mkdir build

RUN cd ~/Workspace/llvm-3.8.0.src/build && cmake -DCMAKE_BUILD_TYPE=Debug ..

RUN cd ~/Workspace/llvm-3.8.0.src/build && make -j2

RUN cd ~/Workspace/llvm-3.8.0.src/build && make install
