#!/usr/bin/perl
use strict;
use warnings;

# Adent of Code 2023 Day 05 - If You Give A Seed A Fertilizer.
# Perl.

# Load raw input data into arrays of lines.
sub get_raw_input {
    open(DATA, "<$_[0]");
    my @lines = <DATA>;
    close(DATA);
    return @lines;
}

# Get seeds array from raw input.
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

# Display seeds list.
sub display_seeds {
    print "seeds: ";
    foreach my $item (@_) {
        chomp($item);
        print($item," ");
    }        
    print("\n");
}

# Display mappings array.
sub display_map {
    my($mappings, $mapName) = @_;
    print("\n",$mapName," map:\n");
    foreach my $item (@$mappings) {
        print(@$item[0]," ",@$item[1]," ",@$item[2]," ",@$item[3],"\n");
    }    
}

# Get raw input.
my $fileName = "Day05InputExample.txt";
my @input = get_raw_input($fileName);

# Parse all data and maps.
my @seeds = get_seeds(@input);

my @seedToSoilMap = load_map(\@input,"seed-to-soil");
my @soilToFertilizerMap = load_map(\@input,"soil-to-fertilizer");
my @fertilizerToWaterMap = load_map(\@input,"fertilizer-to-water");
my @waterToLightMap = load_map(\@input,"water-to-light");
my @lightToTemperaturerMap = load_map(\@input,"light-to-temperature");
my @temperatureToHumidityMap = load_map(\@input,"temperature-to-humidity");
my @humidityToLocationMap = load_map(\@input,"humidity-to-location");

# Display all data.
display_seeds(@seeds);

display_map(\@seedToSoilMap, "seed-to-soil");
display_map(\@soilToFertilizerMap, "soil-to-fertilizer");
display_map(\@fertilizerToWaterMap, "fertilizer-to-water");
display_map(\@waterToLightMap, "water-to-light");
display_map(\@lightToTemperaturerMap, "light-to-temperature");
display_map(\@temperatureToHumidityMap, "temperature-to-humidity");
display_map(\@humidityToLocationMap, "humidity-to-location");
