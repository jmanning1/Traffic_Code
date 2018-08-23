@echo off
rem This file decompresses and converts the tcd.bz2 file into a csv

rem TAKE BACKUP of bz2 file before running!!!

rem required if data on secondary drive

D:



rem Go to tcd.bz2 location

cd "D:\Documents\5872M-Dissertation\Data\Original\Auto"



rem bzip downloaded from https://www.midas-data.org.uk/ 
rem -d is decompression into a tcd file along with the files to be decompressed

@echo on

bzip2-102-x86-win32.exe -d *.tcd.bz2

@echo off


rem The TcdDecoder is downloaded from https://tcddecoder.sitedatatools.co.uk/
rem The first File location is the location of all tcd files --output-dir is the output location of the csv

rem @echo on

rem TcdDecoder.exe D:\Documents\5872M-Dissertation\Data\tcd\*tcd --output-dir D:\Documents\5872M-Dissertation\Data\csv_files



pause
