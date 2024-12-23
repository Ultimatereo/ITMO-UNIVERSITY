use strict;
while (<>) {
	print if /^.*\([^\(\)]*\w[^\(\)]*\).*$/;
}