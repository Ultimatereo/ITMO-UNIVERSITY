use strict;
while (<>) {
	print if /^\W*((\w)+\W+)*\d+(\W+(\w)+)*\W*$/;
}