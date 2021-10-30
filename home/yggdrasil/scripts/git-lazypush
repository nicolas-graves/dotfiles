#!/bin/sh

message=$1 # First parameter will be the commit message
currentBranch=$(git symbolic-ref --short -q HEAD) # Getting the current branch

if [ ! -z "$1" ] # checking if the commit message is present. If not then aborting.
then
  git add .
  git commit -m "$message"
  git push origin $currentBranch
else
  echo "Commit message is not provided"
fi
