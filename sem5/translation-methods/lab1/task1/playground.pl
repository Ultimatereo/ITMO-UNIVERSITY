# It's all from https://perldoc.perl.org/5.30.0/perlintro
use strict;
print("Hello, world\n");
# This is a comment
my $animal = "camel";
my $answer = 42;
print("The animal is $animal\n");
print("The number is $answer\n");

my @animals = ("camel", "llama", "owl");
my @numbers = (23, 42, 69);
my @mixed   = ("camel", 42, 1.23);
print $animals[0];              # prints "camel"
print $animals[1];              # prints "llama"
print $mixed[$#mixed];       # last element, prints 1.23
@animals[0,1];                 # gives ("camel", "llama");
@animals[0..2];                # gives ("camel", "llama", "owl");
@animals[1..$#animals];        # gives all except the first element

my %fruit_color = ("apple", "red", "banana", "yellow");
my %fruit_colors = (
    apple  => "red",
    banana => "yellow",
);
print($fruit_colors{"apple"});           # gives "red"
my @fruits = keys %fruit_colors;
my @colors = values %fruit_colors;
print("\n@fruits\n@colors\n");

my $variables = {
    scalar  =>  {
                 description => "single item",
                 sigil => '$',
                },
    array   =>  {
                 description => "ordered list of items",
                 sigil => '@',
                },
    hash    =>  {
                 description => "key/value pairs",
                 sigil => '%',
                },
};

print "Scalars begin with a $variables->{'scalar'}->{'sigil'}\n";