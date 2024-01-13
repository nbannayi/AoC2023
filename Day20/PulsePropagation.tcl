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
    # Print route.
    set route "$sourceModule -$pulseStr-> $destModule"
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
            return [list -1 {} $memory $route]
        # If a low pulse is received flip-flop.
        } else {            
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
            return [list $newPulse $newDestModules $memory $route]
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
        return [list $newPulse $newDestModules $memory $route]
    }
    return [list $pulse {} $memory $route]
}

# Now process a button push, this will run until the pulses queue is empty.
proc pushButton {modules memory} {
    # Init high and low pulses.
    set noLowPulses 1
    set noHighPulses 0
    set routes {}
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
        set route [lindex $result 3]
        lappend routes $route
        # Add new pulses to queue and update memory.
        foreach module $newDestModules {
            enqueue pulses [list $destModule $module $newPulse]
        }
        set memory $newMemory
    }
    return [list $memory $noLowPulses $noHighPulses $routes]
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

# Push the button till we achieve the desired condition.  Return no. pushes.
proc pushButtonTillCondition {modules memory requiredRoute} {
    set obtainedRoutes {}
    set noPushes 0
    while {[lsearch $obtainedRoutes $requiredRoute] == -1} {
        set noPushes [incr noPushes]        
        set result [pushButton $modules $memory]
        set memory [lindex $result 0]        
        set obtainedRoutes [lindex $result 3]
    }
    return $noPushes
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

# Calculate the Lowest Common Multiple (LCM).
proc lcm {a b} {
    proc gcd {a b} {
        while {$b} {
            set temp $a
            set a $b
            set b [expr {$temp % $b}]
        }
        return $a
    }
    if {$a == 0 || $b == 0} {
        return 0
    }
    return [expr {abs($a * $b) / [gcd $a $b]}]
}

# Modules which will contain the layout.
set modules [parseInputFile "Day20Input.txt"]

# Display it all in Tk.

# Function to be called when the button is clicked
proc updateValues {modules} {
    # Part 1.
    set memory [initMemory $modules]
    set result [pushButtonNTimes $modules $memory 1000]
    set pulseProduct [expr {[lindex $result 1] * [lindex $result 2]}]
    .label1 configure -text "Part 1 answer: $pulseProduct"

    # Part 2.
    # Looking at the input need to find LCM of number of pushes such that:
    # jn -low-> hn, jl -low-> mp, qp -low-> xf, fb -low-> fz
    # -------------------------------------------------
    # Init memory and work out first conjunction point.
    set memory [initMemory $modules]
    set jnPushes [pushButtonTillCondition $modules $memory {jn -low-> hn}]
    # Init memory and work out second conjunction point.
    set memory [initMemory $modules]
    set jlPushes [pushButtonTillCondition $modules $memory {jl -low-> mp}]
    # Init memory and work out third conjunction point.
    set memory [initMemory $modules]
    set gpPushes [pushButtonTillCondition $modules $memory {gp -low-> xf}]
    # Init memory and work out fourth conjunction point.
    set memory [initMemory $modules]
    set fbPushes [pushButtonTillCondition $modules $memory {fb -low-> fz}]
    # Now LCM of all of these will mean low inputs fo all of these which will in turn
    # go through the remaining few steps to send a low pulse to rx.
    set lcm1 [lcm $jnPushes $jlPushes]
    set lcm2 [lcm $gpPushes $fbPushes]
    set totalPushes [lcm $lcm1 $lcm2]
    .label2 configure -text "Part 2 answer: $totalPushes"
}

# Create the main window
wm title . "Advent of Code 2023 - Day 20"
wm geometry . 400x200

# Create and place a label for "Pulse Propagation"
label .pulseLabel -text "Pulse Propagation"
pack .pulseLabel -side top -pady 5

# Create and place labels
label .label1 -text "Part 1 answer:"
label .label2 -text "Part 2 answer:"

pack .label1 -side top -pady 5
pack .label2 -side top -pady 5

# Create a button to trigger the updateValues function
button .button -text "Calculate answers" -command {updateValues $modules}
pack .button -side top -pady 10

# Run the Tkinter event loop
update