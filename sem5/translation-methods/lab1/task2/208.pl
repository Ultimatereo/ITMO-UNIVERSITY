use strict;

while (<>) {
    s/\b0*(\d+)0\b/$1/g;
    print;
}
