# Advent of Code 2023 - Day 20 - Pulse Propagation
# Tcl/Tk

source "Queue.tcl"

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

# Given a pulse (1=high,0=low), destModule, modules and memory
# returns newPulse (1=high,0=low,-1=none), newDestModules and updatedMemory. 
proc send {pulse sourceModule destModule modules memory} {
    if {$pulse == 0} {
        set pulseStr "low"
    } else {
        set pulseStr "high"
    }        
    # Get rx module for part 2.
    set rxLowPulse 0
    set rxHighPulse 0
    if {$destModule == "rx" && $pulse == 0} {
        set rxLowPulse 1 
    } elseif {$destModule == "rx" && $pulse == 1} {
        set rxHighPulse 1 
    }
    # Flip-flop.
    if {[dict exists $modules "%$destModule"]} {        
        # Init newPulse.
        set newPulse 0
        # Get current flip-flop status from memory.
        set currentStatus "off"
        if {[dict exists $memory $destModule]} {
            set currentStatus [dict get $memory $destModule]    
        }
        # If a high pulse is received, set memory but do nothing.
        if {$pulse == 1} {
            #puts "Processing hi pulse, no action"
            return [list -1 {} $memory $rxLowPulse $rxHighPulse]
        # If a low pulse is received flip-flop.
        } else {
            #puts "Processing low pulse"
            if {$currentStatus == "on"} {
                set currentStatus "off"
                dict set memory $destModule $currentStatus
                set newPulse 0
            } else {
                set currentStatus "on"
                dict set memory $destModule $currentStatus
                set newPulse 1
            }
            set newDestModules [dict get $modules "%$destModule"]
            return [list $newPulse $newDestModules $memory $rxLowPulse $rxHighPulse]
        }        
    # Conjunction.
    } elseif {[dict exists $modules "&$destModule"]} {        
        # Firstly update memory.
        if {[dict exists $memory $destModule]} {
            set currentMem [dict get $memory $destModule]
        } else {
            set currentMem [dict create]  
        }       
        dict set currentMem $sourceModule $pulse
        dict set memory $destModule $currentMem        
        # Now if all are high pulses return a low pulse, else return a high pulse.
        set newPulse 0 
        foreach memPulse [dict values $currentMem] {
            if {$memPulse == 0} {
                set newPulse 1
                break
            }
        }        
        set newDestModules [dict get $modules "&$destModule"]        
        return [list $newPulse $newDestModules $memory $rxLowPulse $rxHighPulse]
    }
    return [list $pulse {} $memory $rxLowPulse $rxHighPulse]
}

# Now process a button push, this will run until the pulses queue is empty.
proc pushButton {modules memory} {
    # Init high and low pulses.
    set noLowPulses 1
    set noHighPulses 0
    set noRxLowPulses 0
    set noRxHighPulses 0
    # Create an init pulses queue.
    set pulses [createQueue]
    set broadcasterModule [dict get $modules broadcaster]
    foreach module $broadcasterModule {
        enqueue pulses [list "broadcaster" $module 0]
    }
    while {[llength $pulses] > 0} {
        # Get next module in queue.
        set nextModule [dequeue pulses]
        set pulse [lindex $nextModule 2]
        set destModule [lindex $nextModule 1]
        set sourceModule [lindex $nextModule 0]
        # Process module.
        if {$pulse == 1} {
            incr noHighPulses
        } else {
            incr noLowPulses
        }
        set result [send $pulse $sourceModule $destModule $modules $memory]
        set newPulse [lindex $result 0]
        set newDestModules [lindex $result 1]
        set newMemory [lindex $result 2]
        set rxlp [lindex $result 3]
        set rxhp [lindex $result 4]
        set noRxLowPulses [expr {$noRxLowPulses + $rxlp}]
        set noRxHighPulses [expr {$noRxHighPulses + $rxhp}]
        # Add new pulses to queue and update memory.
        foreach module $newDestModules {
            enqueue pulses [list $destModule $module $newPulse]
        }
        set memory $newMemory
    }
    return [list $memory $noLowPulses $noHighPulses $noRxLowPulses $noRxHighPulses]
}

# Now process a button push n times, this will run until the pulses queue is empty.
proc pushButtonNTimes {modules memory n} {
    set totalNoLowPulses 0
    set totalNoHighPulses 0
    for {set i 1} {$i <= $n} {incr i} {
        set result [pushButton $modules $memory]
        set newMemory [lindex $result 0]
        set noLowPulses [lindex $result 1]
        set noHighPulses [lindex $result 2]
        set totalNoLowPulses [expr {$totalNoLowPulses + $noLowPulses}]
        set totalNoHighPulses [expr {$totalNoHighPulses + $noHighPulses}]
        set memory $newMemory
    }
    return [list $memory $totalNoLowPulses $totalNoHighPulses]
}

# Try to find no pushes required to send a single low pulse to rx to switch on machine.
proc findNoPushesToSwitchOn {modules memory} {
    set i 0
    while {true} {
        set i [incr i]
        set result [pushButton $modules $memory]
        set newMemory [lindex $result 0]
        set noLowPulses [lindex $result 1]
        set noHighPulses [lindex $result 2]    
        set noRxLowPulses [lindex $result 3]
        set noRxHighPulses [lindex $result 4]
        if {$noRxLowPulses >= 1} {
            return $i
        }
        puts "$i: $noLowPulses, $noHighPulses, $noRxLowPulses, $noRxHighPulses" 
        set memory $newMemory
    }
}

# Initialise memory.
# A dictionary with key component: 
# For conjunction value a dict with value 0 for low and 1 for hi for all connected modules.
# For flip-flop value is either "on" or "off".
proc initMemory {modules} {
    set memory [dict create]
    dict for {key value} $modules {    
        if {[string match {*%*} $key]} {
            dict set memory [string range $key 1 end] "off"            
        } else {
            # Init all connections.
            dict set memory [string range $key 1 end] [dict create]
            dict for {key2 value2} $modules {
                if {[lsearch -exact $value2 [string range $key 1 end]] >= 0} {
                    set memKey [dict get $memory [string range $key 1 end]]
                    dict set memKey [string range $key2 1 end] 0
                    dict set memory [string range $key 1 end] $memKey
                }
            }
        }
    }
    return $memory
}

# Modules which will contain the layout.
set modules [parseInputFile "Day20Input.txt"]

# Part 1.
set memory [initMemory $modules]
set result [pushButtonNTimes $modules $memory 1000]
set pulseProduct [expr {[lindex $result 1] * [lindex $result 2]}]
puts "Part 1 answer: $pulseProduct"

# Part 2.
#set memory [initMemory $modules]
#set noPushes [findNoPushesToSwitchOn $modules $memory]
#puts "Part 2 answer: $noPushes"