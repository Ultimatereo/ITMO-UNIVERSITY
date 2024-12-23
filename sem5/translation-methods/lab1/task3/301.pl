use strict;

my $status = 0;

# my $output_file = 'output.txt';
# open my $output_fh, '>', $output_file or die "Cannot open $output_file: $!";

while (<>) {
	# if (/^STOP123$/) {
		# last;
	# }
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
            	# print $output_fh "\n";
                print "\n";
            }
            # print $output_fh "\n";
            # print $output_fh $_;
            print "\n";
            print $_;
            $status = 1;
        }
    }
}

# close $output_fh;
