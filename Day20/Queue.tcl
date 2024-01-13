# Queue implementation for AoC 2023 day 20.

# Function to enqueue an element.
proc enqueue {queue element} {
    upvar 1 $queue queueVar
    lappend queueVar $element
}

# Function to dequeue an element.
proc dequeue {queue} {
    upvar 1 $queue queueVar
    set element [lindex $queueVar 0]
    set queueVar [lrange $queueVar 1 end]
    return $element
}

# Create a new queue.
proc createQueue {} {
    set queue {}
    return $queue
}

# Function to display the contents of the queue.
proc displayQueue {queue} {
    puts "Queue contents: $queue"
}