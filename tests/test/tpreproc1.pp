const
	C = -2;
	M2 = -2;
begin
	// Works.
	{$if C <> M2} {$error C <> M2} {$endif}

	// Error: Evaluating a conditional compiling expression
	// Error: Incompatible types: got "<erroneous type>" expected "Int64"
	// Error: Incompatible types: got "ShortInt" expected "<erroneous type>"
	// Error: Compile time expression: Wanted Boolean but got <erroneous type> at IF or ELSEIF
	{$if C <> -2} {$error C <> -2} {$endif}
	{$if C <> - 2} {$error C <> - 2} {$endif}
	{$if C <> -2.0} {$error C <> -2.0} {$endif}
	{$if C <> - 2.0} {$error C <> - 2.0} {$endif}
end.
