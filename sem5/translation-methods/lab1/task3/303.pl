use strict;

my $html_content = "";
while (<>) {
	if (/^STOP123$/) {
		last;
	}
	$html_content = $html_content . $_;
}
# print $html_content . "\n";

my %unique_sites;
while ($html_content =~ 
	/<\s*a\s.*href\s*=\s*("\s*[^"'>]*\s*"|'\s*[^"'>]*\s*').*>/gi) {
    my $url = substr($1, 1, -1);;
    $url =~ s/^\s+|\s+$//g;
    if ($url =~ /^(([\w+\-]+:)?\/\/)(([\w.\-~]+(:[\w.\-~]+)?@)?(?<host>[\w.\-~]+)(\:\d+)?)([\/?#].*)?$/i) {
        my $site = $+{host};
        # print "$site\n";
        $unique_sites{$site} = 1;
    }
}

# print "\n\n";

foreach my $site (sort keys %unique_sites) {
	print "$site\n";
}