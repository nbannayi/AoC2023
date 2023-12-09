// Advent of Code 2023, Day 06 - Wait For It.
// Rust.

// Race type for storing puzzle input.
#[derive(Debug)]
struct Race {
    time: u32,
    distance: u32
}

// Given a time and distance return the number of ways to win.
fn get_no_ways_to_win(time: u32, distance: u32) -> u32 {
    let mut count = 0u32;
    for t in 1u32..(time-1) {
        let run_distance = t*(time-t);
        if run_distance > distance {
            count += 1;
        }
    }
    return count;         
}

// Get margin of error for all races.
fn get_margin_of_error(races: &[Race]) -> u32 {
    let mut margin_of_error = 1u32;
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
    let time_value: u32 = time_str.parse().unwrap();
    let distance_value: u32 = distance_str.parse().unwrap();
    let corrected_race = Race {time: time_value, distance: distance_value};
    return Race {time: time_value, distance: distance_value};
}

fn main () {
    // Input data - so small will create it manully for now.    
    let example_races = 
        [
            Race {time: 7,  distance: 9}, 
            Race {time: 15, distance: 40}, 
            Race {time: 30, distance: 200}
        ];
    /*
    let races =
        [
            Race {time: 46, distance: 358}, 
            Race {time: 68, distance: 1054}, 
            Race {time: 98, distance: 1807},
            Race {time: 66, distance: 1080}
        ];
    */

    // Part 1.
    let margin_of_error = get_margin_of_error(&example_races);
    println!("Part 1 answer: {}", margin_of_error);

    // Part 2 (naive, unlikely to work for full input...)
    let corrected_race = correct_kerning(&example_races);
    let no_ways_to_win = get_no_ways_to_win(corrected_race.time, corrected_race.distance);
    println!("Part 2 answer: {}", no_ways_to_win);
}