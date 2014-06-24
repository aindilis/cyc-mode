#!/usr/bin/perl

printheaders();

while (<>) {
    $input .= $_;
}

foreach $segment (split /^ *$/m,$input ) {
    $type = 0;
    %argt = ();
    %args = ();
    %argn = ();
    $returnvalue = "";
    $comment = "";
    if ($segment =~ /^function /m) {
	$line = $segment;
	$line =~ /function (\S+) : \(([^\)]*)\)(.*)/s;
	$function = $1;
	$args = $2;
	$segment = $3;
	$args =~ s/\n//g;
	$args =~ s/\s+/ /g;

	# deal with functions that end with a ?
	$printedrepresentation=$function;
	$function =~ s/\?/qm/;
	$function =~ s/\+/plus/;

	# parse args to determine which args are optional, etc.
	$iter = 0;
	$firstoarg = 0;
	foreach $arg (split / /,$args) {
	    ++$iter;
	    if ($arg =~ /\&optional/) {
		$firstoarg = $iter;
	    } else {
		$temp = $iter;
		
		$args{$iter - ($firstoarg > 0)} = $arg;
		$argn{$arg} = $iter - ($firstoarg > 0);
	    }
	}
	foreach $line (split /\n/, $segment) {
	    if ($line =~ /^(.*) must satisfy (.*)\.\s*$/) {
		++$type;
		$arg = $1;
		$ptype = $2;
		$ptypeprintedrepresentation = $ptype;
		$ptypeprintedrepresentation =~ s/\?/qm/g;
		$ptypeprintedrepresentation =~ s/\+/plus/g;
		$ptypeprintedrepresentation =~ s/[\(\)]//g;
		$ptypeprintedrepresentation =~ s/\s/-/g;
		$argt{$type} = $ptypeprintedrepresentation;
		$argpt{$type} = $ptype;
	    }
	    elsif ($line =~ /^Single value returned (satisfies|is a list of elements satisfying) (\S+)\.\s*/) {
		$returnvalue = $2;
	    }
	    else {
		$line =~ s/\s+$/ /;
		$comment .= $line;
	    }
	}
	# say its a function
	print "(find-or-create-constant \"SubLFunction-$function\")\n";
	print "(cyc-assert \'(#\$isa #\$SubLFunction-$function #\$SubLFunction) #\$SubLMicrotheoryMt)\n";
	    print "(cyc-assert \'(#\$printedRepresentation #\$SubLFunction-$function \"$printedrepresentation\") #\$SubLMicrotheoryMt)\n";
	    if ($returnvalue !~ /^$/) {
		# say its a type
		print "(find-or-create-constant \"SubLFunction-$returnvalue\")\n";
		print "(cyc-assert \'(#\$isa #\$SubLFunction-$returnvalue #\$SubLTypePredicate) #\$SubLMicrotheoryMt)\n";
		    print "(cyc-assert \'(#\$subLFunctionReturnTypePredicate #\$SubLFunction-$function #\$SubLFunction-$returnvalue) #\$SubLMicrotheoryMt)\n";
		} else {
#		    print "(cyc-assert \'(#\$subLFunctionReturnTypePredicate #\$SubLFunctionSymbol-$function #\$SubLFunction-NIL) #\$SubLMicrotheoryMt)\n";
		}
	if ($comment !~ /^$/) {
	    print "(cyc-assert '(#\$comment #\$SubLFunction-$function \"$comment\") #\$SubLMicrotheoryMt)\n";
	    }
	foreach $key (keys %argt) {
	    print "(find-or-create-constant \"SubLFunction-$argt{$key}\")\n";
	    print "(cyc-assert \'(#\$isa #\$SubLFunction-$argt{$key} #\$SubLTypePredicate) #\$SubLMicrotheoryMt)\n";
		print "(cyc-assert \'(#\$subLFunctionArgumentTypePredicate #\$SubLFunction-$function $key #\$SubLFunction-$argt{$key}) #\$SubLMicrotheoryMt)\n";
	    print "(cyc-assert \'(#\$printedRepresentation #\$SubLFunction-$argt{$key} \"$argpt{$key}\") #\$SubLMicrotheoryMt)\n";
		if (($key >= $firstoarg) && $firstoarg) {
		    print "(cyc-assert \'(#\$subLFunctionOptionalArgument #\$SubLFunction-$function $key) #\$SubLMicrotheoryMt)\n";
			
		    }
	}
	print "\n";
    }
}

sub printheaders () {
    print "(find-or-create-constant \"SubLMicrotheoryMt\")\n";
    print "(cyc-assert '(#\$isa #\$SubLMicrotheoryMt #\$Microtheory) #\$BaseKB)\n";
	print "(cyc-assert '(#\$genlMt #\$SubLMicrotheoryMt #\$BaseKB) #\$BaseKB)\n";
	print "(find-or-create-constant \"SubLFunction\")\n";
    print "(cyc-assert '(#\$isa #\$SubLFunction #\$Collection) #\$SubLMicrotheoryMt)\n";
	print "(find-or-create-constant \"SubLTypePredicate\")\n";
    print "(cyc-assert '(#\$isa #\$SubLTypePredicate #\$Collection) #\$SubLMicrotheoryMt)\n";
	print "(cyc-assert '(#\$genls #\$SubLTypePredicate #\$SubLFunction) #\$SubLMicrotheoryMt)\n";
	print "(find-or-create-constant \"printedRepresentation\")\n";
    print "(cyc-assert '(#\$isa #\$printedRepresentation #\$BinaryPredicate) #\$SubLMicrotheoryMt)\n";
	print "(cyc-assert '(#\$arity #\$printedRepresentation 2) #\$SubLMicrotheoryMt)\n";
	print "(cyc-assert '(#\$argIsa #\$printedRepresentation 1 #\$SubLFunction) #\$SubLMicrotheoryMt)\n";
	print "(cyc-assert '(#\$argIsa #\$printedRepresentation 2 #\$SubLString) #\$SubLMicrotheoryMt)\n";
	print "(find-or-create-constant \"subLFunctionReturnTypePredicate\")\n";
    print "(cyc-assert '(#\$isa #\$subLFunctionReturnTypePredicate #\$BinaryPredicate) #\$SubLMicrotheoryMt)\n";
	print "(cyc-assert '(#\$arity #\$subLFunctionReturnTypePredicate 2) #\$SubLMicrotheoryMt)\n";
	print "(cyc-assert '(#\$argIsa #\$subLFunctionReturnTypePredicate 1 #\$SubLFunction) #\$SubLMicrotheoryMt)\n";
	print "(cyc-assert '(#\$argIsa #\$subLFunctionReturnTypePredicate 2 #\$SubLTypePredicate) #\$SubLMicrotheoryMt)\n";
	print "(find-or-create-constant \"subLFunctionArgumentTypePredicate\")\n";
    print "(cyc-assert '(#\$isa #\$subLFunctionArgumentTypePredicate #\$TernaryPredicate) #\$SubLMicrotheoryMt)\n";
	print "(cyc-assert '(#\$arity #\$subLFunctionArgumentTypePredicate 3) #\$SubLMicrotheoryMt)\n";
	print "(cyc-assert '(#\$argIsa #\$subLFunctionArgumentTypePredicate 1 #\$SubLFunction) #\$SubLMicrotheoryMt)\n";
	print "(cyc-assert '(#\$argIsa #\$subLFunctionArgumentTypePredicate 2 #\$SubLPositiveInteger) #\$SubLMicrotheoryMt)\n";
	print "(cyc-assert '(#\$argIsa #\$subLFunctionArgumentTypePredicate 3 #\$SubLTypePredicate) #\$SubLMicrotheoryMt)\n";
	print "(find-or-create-constant \"subLFunctionOptionalArgument\")\n";
    print "(cyc-assert '(#\$isa #\$subLFunctionOptionalArgument #\$BinaryPredicate) #\$SubLMicrotheoryMt)\n";
	print "(cyc-assert '(#\$arity #\$subLFunctionOptionalArgument 2) #\$SubLMicrotheoryMt)\n";    
	print "(cyc-assert '(#\$argIsa #\$subLFunctionOptionalArgument 1 #\$SubLFunction) #\$SubLMicrotheoryMt)\n";
	print "(cyc-assert '(#\$argIsa #\$subLFunctionOptionalArgument 2 #\$SubLPositiveInteger) #\$SubLMicrotheoryMt)\n";
	print "\n";
}
