# Advent of Code 2023 - Day 20 - Pulse Propagation
# Tcl/Tk

# Parse input file.
proc parseInputFile {inputFileName} {
    set inputFile [open $inputFileName "r"]
    # Create empty modules dictionary.
    set modules [dict create]
    # Now parse each line and store as a key value pair in modules.
    while {[gets $inputFile line] != -1} {        
        set line [string map {" " ""} $line]
        set tokens [split $line "->"]
        # Input is key (e.g. %a), value is list of desinations (e.g. b,c)
        set input [lindex $tokens 0]        
        set outputs [split [lindex $tokens 2] ","]
        dict set modules $input [list {*}$outputs]
    }    
    close $inputFile
    return $modules
}

set modules [parseInputFile "Day20InputExample1.txt"]
set broadcaster [dict get $modules "broadcaster"]
puts "[lindex $broadcaster 2]"
