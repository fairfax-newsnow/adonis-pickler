#!/bin/bash

echo "ls -l $SEMAPHORE_CACHE_DIR/"
ls -l $SEMAPHORE_CACHE_DIR/
# setup command -- restores the cache
if [ -e $SEMAPHORE_CACHE_DIR/.sbt ]
  then
  echo "ls -l $SEMAPHORE_CACHE_DIR/.sbt/"
  ls -l $SEMAPHORE_CACHE_DIR/.sbt/
  echo "ls -l $SEMAPHORE_CACHE_DIR/.sbt/boot/"
  ls -l $SEMAPHORE_CACHE_DIR/.sbt/boot/
  echo "---------------------------------------------------------"
  echo "Copying .sbt from $SEMAPHORE_CACHE_DIR/.sbt to $HOME/.sbt"
  cp -r $SEMAPHORE_CACHE_DIR/.sbt $HOME/.sbt
else
  echo "No .sbt cache found."
fi