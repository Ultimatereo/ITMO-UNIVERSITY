use strict;

while (<>) {
    s/\([^)]*\)/\(\)/g;
    print;
}
