# Interface to Google Drive in Racket

## Fred Martin
### April 22, 2017

# Overview
This set of code provides an interface to searching through one's Google Drive account. 
Its most important feature is that it provides a /folder-delimited search/.

The essential model of files in Google Drive is that they are in one big “pile.” So you can't directly find a file in a
given folder.

This code recursively collects all files within a given file 
