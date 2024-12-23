use strict;
while (<>) {
	print if /^(\w|\W)*cat(\w|\W)*cat(\w|\W)*$/;
}