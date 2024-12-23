use strict;
while (<>) {
	print if /^\W*((\w)+\W+)*cat(\W+(\w)+)*\W*$/;
}