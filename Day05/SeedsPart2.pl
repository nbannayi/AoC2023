#!/usr/bin/perl
use strict;
use warnings;

# Adent of Code 2023 Day 05, Part 2 - If You Give A Seed A Fertilizer.
# Perl.

# Load raw input data into arrays of lines.
sub get_raw_input {
    open(DATA, "<$_[0]");
    my @lines = <DATA>;
    close(DATA);
    return @lines;
}

# Get seeds array directly from raw input.
sub get_seeds {
    my @input = @_;
    my @seeds = split(/ /,$input[0]);
    splice @seeds, 0, 1;
    return @seeds
}

# Laod a map based on name.
sub load_map {
    my ($input, $mapName) = @_;
    my $startLine = 0;
    my $endLine = 0;    
    for my $i (0..(scalar @$input)) {
        if (index(@$input[$i], $mapName) != -1) {
            $startLine = $i+1;
            last;
        }
    }    
    for my $j (($startLine+1)..(scalar @$input)) {
        if (@$input[$j] eq "\n") {
            $endLine = $j-1;
            last;
        }
    }         
    my @outputMappings = ();
    for my $i ($startLine..$endLine) {
        chomp(@$input[$i]); # Remove new line characters.
        my $mappingLine = @$input[$i];
        my @inputMappings = split(/ /, $mappingLine);
        $outputMappings[$i-$startLine][0] = $inputMappings[1];
        $outputMappings[$i-$startLine][1] = $inputMappings[1] + $inputMappings[2] - 1;
        $outputMappings[$i-$startLine][2] = $inputMappings[0];
        $outputMappings[$i-$startLine][3] = $inputMappings[0] + $inputMappings[2] - 1;        
    }
    return @outputMappings;
}

# Given an input and a map, find the output.
sub map_input_to_output {
    my($input, $mappings) = @_;
    my $output = $input;    
    foreach my $mapping (@$mappings) {
        my $sourceStart = @$mapping[2];
        my $sourceEnd   = @$mapping[3];
        my $destStart   = @$mapping[0];
        my $destEnd     = @$mapping[1];
        if ($input >= $sourceStart and $input <= $sourceEnd) {
            $output = $destStart + $input - $sourceStart;
            last;
        }
    }
    return $output;
}

# Returns 1 if seed is in one of the ranges, 0 otherwise.
sub is_seed_in_ranges {
    my ($input, $seed_ranges) = @_;
    for (my $i = 0; $i < (scalar @$seed_ranges); $i += 2) {
        my $seedLower = @$seed_ranges[$i];
        my $seedUpper = @$seed_ranges[$i] + @$seed_ranges[$i+1] - 1;
        my $found = 0;
        if ($input >= $seedLower and $input <= $seedUpper) {
            return 1;            
        }
    }
    return 0;
}

# Given a location and all maps, return 1 if a seed exists, 0 otherwise.
sub seed_exists_for_location {
    my ($location, 
        $seeds,
        $seedToSoilMap,     
        $soilToFertilizerMap,
        $fertilizerToWaterMap,
        $waterToLightMap,
        $lightToTemperatureMap,
        $temperatureToHumidityMap,
        $humidityToLocationMap) = @_;                                           
    my $output         = map_input_to_output($location, \@$humidityToLocationMap);
    $output            = map_input_to_output($output,   \@$temperatureToHumidityMap);
    $output            = map_input_to_output($output,   \@$lightToTemperatureMap);
    $output            = map_input_to_output($output,   \@$waterToLightMap);
    $output            = map_input_to_output($output,   \@$fertilizerToWaterMap);
    $output            = map_input_to_output($output,   \@$soilToFertilizerMap);
    my $potential_seed = map_input_to_output($output,   \@$seedToSoilMap);
    return is_seed_in_ranges($potential_seed, \@$seeds);
}

# Attempt to find the lowest location working backwards and starting from 1.
# If this doesn't work I'm a bit out of ideas...
sub find_lowest_location {
    my ($seeds,
        $seedToSoilMap,     
        $soilToFertilizerMap,
        $fertilizerToWaterMap,
        $waterToLightMap,
        $lightToTemperatureMap,
        $temperatureToHumidityMap,
        $humidityToLocationMap) = @_;   
    my $found = 0;
    my $potential_location = -1;
    while ($found == 0) {        
        $potential_location++;
        print("Tried ",$potential_location,"\n");
        $found = 
            seed_exists_for_location(
                $potential_location,
                \@$seeds,
                \@$seedToSoilMap, 
                \@$soilToFertilizerMap, 
                \@$fertilizerToWaterMap, 
                \@$waterToLightMap,
                \@$lightToTemperatureMap,
                \@$temperatureToHumidityMap,
                \@$humidityToLocationMap);   
    }    
    return $potential_location;
}

# Get raw input.
my $fileName = "Day05Input.txt";
my @input = get_raw_input($fileName);

# Parse all data and maps.
my @seeds                    = get_seeds(@input); # Directly load seed numbers.
# --
my @seedToSoilMap            = load_map(\@input,"seed-to-soil");
my @soilToFertilizerMap      = load_map(\@input,"soil-to-fertilizer");
my @fertilizerToWaterMap     = load_map(\@input,"fertilizer-to-water");
my @waterToLightMap          = load_map(\@input,"water-to-light");
my @lightToTemperatureMap    = load_map(\@input,"light-to-temperature");
my @temperatureToHumidityMap = load_map(\@input,"temperature-to-humidity");
my @humidityToLocationMap    = load_map(\@input,"humidity-to-location");

# Finally return lowest locations.
my $location =
    find_lowest_location(
        \@seeds,
        \@seedToSoilMap, 
        \@soilToFertilizerMap, 
        \@fertilizerToWaterMap, 
        \@waterToLightMap,
        \@lightToTemperatureMap,
        \@temperatureToHumidityMap,
        \@humidityToLocationMap);

print("Part 2 answer: ", $location,"\n");