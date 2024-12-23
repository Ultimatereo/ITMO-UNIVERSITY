use strict;

my $status = 0;

while (<>) {
	s/<[^>]*>//g;
    if ($status == 0) {
        if (/^\s*$/) {
            next;
        } else {
            s/^\s+//;
            s/\s+$//;
            s/(\s)\s+/$1/g;
            # print $output_fh $_;
            print $_;
            $status = 1;
        }
    } else {
        if (/^\s*$/) {
            $status = 2;
            next;
        } else {
            s/^\s+//;
            s/\s+$//;
            s/(\s)\s+/$1/g;
            if ($status == 2) {
                print "\n";
            }
            print "\n";
            print $_;
            $status = 1;
        }
    }
}
