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
sub get_seeds_direct {
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

# Given a location and all maps, return true if a seed exists, false otherwise.
sub seed_exists_for_location {
    my ($location, 
        $seeds,
        $seedToSoilMap,     
        $soilToFertilizerMap,
        $fertilizerToWaterMap,
        $waterToLightMap,
        $lightToTemperatureMap,
        $humidityToTemperatureMap,
        $humidityToLocationMap) = @_;                                           
    my $output         = map_input_to_output($location, \@$humidityToLocationMap);
    $output            = map_input_to_output($output,   \@$humidityToTemperatureMap);
    $output            = map_input_to_output($output,   \@$lightToTemperatureMap);
    $output            = map_input_to_output($output,   \@$waterToLightMap);
    $output            = map_input_to_output($output,   \@$fertilizerToWaterMap);
    $output            = map_input_to_output($output,   \@$soilToFertilizerMap);
    my $potential_seed = map_input_to_output($output,   \@$seedToSoilMap);
    print($potential_seed, "\n");
    return grep(/^$potential_seed$/, @$seeds);
}

# Get raw input.
my $fileName = "Day05InputExample.txt";
my @input = get_raw_input($fileName);

# Parse all data and maps.
my @seeds                    = get_seeds_direct(@input); # Directly load seed numbers.
# --
my @seedToSoilMap            = load_map(\@input,"seed-to-soil");
my @soilToFertilizerMap      = load_map(\@input,"soil-to-fertilizer");
my @fertilizerToWaterMap     = load_map(\@input,"fertilizer-to-water");
my @waterToLightMap          = load_map(\@input,"water-to-light");
my @lightToTemperatureMap    = load_map(\@input,"light-to-temperature");
my @temperatureToHumidityMap = load_map(\@input,"temperature-to-humidity");
my @humidityToLocationMap    = load_map(\@input,"humidity-to-location");

my $exists = 
    seed_exists_for_location(
        46,
        \@seeds,
        \@seedToSoilMap, 
        \@soilToFertilizerMap, 
        \@fertilizerToWaterMap, 
        \@waterToLightMap,
        \@lightToTemperatureMap,
        \@temperatureToHumidityMap,
        \@humidityToLocationMap);

print($exists, "\n")