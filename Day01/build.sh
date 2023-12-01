# This script compiles the *txt.bas files into c64 format .bas files and creates a disk image that can then be loaded into a C64.
# Use LOAD"$",8 to load the disk directory then LOAD"DAY01PT{n}.PRG",8,1 to load each individual part.  
# RUN then runs the program, non-sped up emulation or original hardware takes around 15 mins for part 1 and 30 mins for part 2.
# (Download and install VICE to use these, then run the d64 disk image on your C64 emulator of choice.)
/Users/Nick/development/c64/Vice/bin/petcat -w2 -o day01_1_trebuchet.bas -- day01_1_trebuchet_txt.bas
/Users/Nick/development/c64/Vice/bin/petcat -w2 -o day01_2_trebuchet.bas -- day01_2_trebuchet_txt.bas
/Users/Nick/development/c64/Vice/bin/c1541 -format diskname,id d64 day01.d64 -attach day01.d64 -write day01_1_trebuchet.bas day01pt1.prg
/Users/Nick/development/c64/Vice/bin/c1541 -attach day01.d64 -write day01_2_trebuchet.bas day01pt2.prg
