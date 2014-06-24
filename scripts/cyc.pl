#!/usr/bin/perl -w

use PerlLib::SwissArmyKnife;

use Net::Telnet;

$telnet = Net::Telnet->new
  (
   Timeout=>50,
   Errmode=>'die',
   Prompt => '/^200 /',
  );

$telnet->open
  (
   Host => '127.0.0.1',
   Port => 3601,
  );

my @res = $telnet->cmd('(all-instances #$Microtheory #$EverythingPSC)');
print Dumper({Res => \@res});
# $telnet->close;
