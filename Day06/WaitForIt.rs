// Advent of Code 2023, Day 06 - Wait For It.
// Rust.

// Race type for storing puzzle input.
#[derive(Debug)]
struct Race {
    time: u64,
    distance: u64
}

// Given a time and distance return the number of ways to win.
fn get_no_ways_to_win(time: u64, distance: u64) -> u64 {
    let mut count = 0u64;
    for t in 1u64..(time-1) {
        let run_distance = t*(time-t);
        if run_distance > distance {
            count += 1;
        }
    }
    return count;         
}

// Get margin of error for all races.
fn get_margin_of_error(races: &[Race]) -> u64 {
    let mut margin_of_error = 1u64;
    for race in races {
        margin_of_error *= get_no_ways_to_win(race.time, race.distance);        
    }
    return margin_of_error;
}

// Correct kerning to turn multiple races into one race.
fn correct_kerning (races: &[Race]) -> Race {
    let mut time_str = "".to_string();
    let mut distance_str = "".to_string();
    for race in races {
        time_str.push_str(&race.time.to_string());        
        distance_str.push_str(&race.distance.to_string());        
    }
    let time_value: u64 = time_str.parse().unwrap();
    let distance_value: u64 = distance_str.parse().unwrap();
    return Race {time: time_value, distance: distance_value};
}

fn main () {
    // Input data - so small created it manually.    
    let races =
        [
            // Redacted - enter manually.
            Race {time: 1, distance: 5}, 
            Race {time: 2, distance: 6}, 
            Race {time: 3, distance: 7},
            Race {time: 4, distance: 8}
        ];

    // Part 1.
    let margin_of_error = get_margin_of_error(&races);
    println!("Part 1 answer: {}", margin_of_error);

    // Part 2.
    let corrected_race = correct_kerning(&races);
    let no_ways_to_win = get_no_ways_to_win(corrected_race.time, corrected_race.distance);
    println!("Part 2 answer: {}", no_ways_to_win);
}