#!/usr/bin/perl
use strict;
use warnings;
use List::Util qw(min);

# Adent of Code 2023 Day 05, Part 1 - If You Give A Seed A Fertilizer.
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
        my $sourceStart = @$mapping[0];
        my $sourceEnd   = @$mapping[1];
        my $destStart   = @$mapping[2];
        my $destEnd     = @$mapping[3];
        if ($input >= $sourceStart and $input <= $sourceEnd) {
            $output = $destStart + $input - $sourceStart;
            last;
        }
    }
    return $output;
}

# Given a seed and all maps return location.
sub find_seed_location {
    my ($seed, 
        $seedToSoilMap, 
        $soilToFertilizerMap, 
        $fertilizerToWaterMap, 
        $waterToLightMap,
        $lightToTemperatureMap,
        $temperatureToHumidityMap,
        $humidityToLocationMap) = @_;        
    my $output = map_input_to_output($seed,   \@$seedToSoilMap);
    $output    = map_input_to_output($output, \@$soilToFertilizerMap);
    $output    = map_input_to_output($output, \@$fertilizerToWaterMap);
    $output    = map_input_to_output($output, \@$waterToLightMap);
    $output    = map_input_to_output($output, \@$lightToTemperatureMap);
    $output    = map_input_to_output($output, \@$temperatureToHumidityMap);
    $output    = map_input_to_output($output, \@$humidityToLocationMap);
    return $output;    
}

# Given an array of seeds and the maps, return all corresponding locations.
sub find_seed_locations {
    my ($seeds, 
        $seedToSoilMap, 
        $soilToFertilizerMap, 
        $fertilizerToWaterMap, 
        $waterToLightMap,
        $lightToTemperatureMap,
        $temperatureToHumidityMap,
        $humidityToLocationMap) = @_;  
    my @locations = ();    
    foreach my $seed (@$seeds) {
        my $location = find_seed_location(
            $seed, 
            \@$seedToSoilMap, 
            \@$soilToFertilizerMap, 
            \@$fertilizerToWaterMap, 
            \@$waterToLightMap,
            \@$lightToTemperatureMap,
            \@$temperatureToHumidityMap,
            \@$humidityToLocationMap); 
        push(@locations, $location);
    }
    return @locations;
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

# Get all locations for part 1.
my @locations =
    find_seed_locations(
        \@seeds,
        \@seedToSoilMap, 
        \@soilToFertilizerMap, 
        \@fertilizerToWaterMap, 
        \@waterToLightMap,
        \@lightToTemperatureMap,
        \@temperatureToHumidityMap,
        \@humidityToLocationMap);

# Finally return minimum locations.
print("Part 1 answer: ", min(@locations),"\n");