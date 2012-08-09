CC=g++
CFLAGS+=-g
LDFLAGS+=-lGL -lGLEW -lglfw -lIL -lILU -lILUT
INCLUDEFLAGS=-I/usr/include/eigen3
CFILES=*.cpp
OFILES=*.o
NAME=sim

all:
	$(CC) $(CFLAGS) $(INCLUDEFLAGS) $(LDFLAGS) -o $(NAME) $(CFILES)