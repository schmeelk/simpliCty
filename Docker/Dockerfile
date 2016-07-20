############################################################
# Dockerfile to build LLVM 3.7.1 container images
# Based on Ubuntu
############################################################
FROM ubuntu:16.04

MAINTAINER Rui Gu

# Build LLVM 3.7.1
RUN apt-get update && apt-get -y install build-essential subversion swig python2.7-dev libedit-dev libncurses5-dev cmake inotify-tools fdupes libxml2-dev swig wget vim sudo tmux ocaml ocaml-native-compilers ocaml-findlib oasis libpcre-ocaml-dev git opam

RUN mkdir -p ~/Workspace && mkdir -p ~/Downloads

RUN cd ~/Downloads && wget http://llvm.org/releases/3.7.1/llvm-3.7.1.src.tar.xz

RUN cd ~/Downloads && wget http://llvm.org/releases/3.7.1/cfe-3.7.1.src.tar.xz

RUN cd ~/Downloads && tar -xf llvm-3.7.1.src.tar.xz -C ~/Workspace/

RUN cd ~/Downloads && tar -xf cfe-3.7.1.src.tar.xz -C ~/Workspace/llvm-3.7.1.src/tools/

RUN cd ~/Workspace/llvm-3.7.1.src/tools && mv cfe-3.7.1.src clang

RUN cd ~/Workspace/llvm-3.7.1.src && mkdir build

RUN cd ~/Workspace/llvm-3.7.1.src/build && cmake -DCMAKE_BUILD_TYPE=Debug ..

RUN cd ~/Workspace/llvm-3.7.1.src/build && make -j8

RUN cd ~/Workspace/llvm-3.7.1.src/build && make install && make clean
