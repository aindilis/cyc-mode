#!/usr/bin/perl -w

use Net::Telnet;

$telnet = new Net::Telnet ( Timeout=>50,
			    Errmode=>'die',
			    Prompt => '/200 240490/');
$telnet->open(Host => '127.0.0.1',
	      Port => 3601);

$com = join (" ", @ARGV);
$com =~ s/%/\#\$/g;
$com =~ s/^/(/;
$com =~ s/$/)/;

print "$com\n";

print $telnet->cmd("$com (+ 235235 5255)");
$telnet->close;
