use strict;
while (<>) {
	s/^(\W*)(\w+)(\W+)(\w+)(\W+)/$1$4$3$2$5/;
	print;
}
