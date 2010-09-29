program tmacprocvar;

{$MODE MACPAS}

{Tests of different ways of handling functions in MW, THINK Pascal and FPC}

	type
		SInt8 = -128..127;
		Ptr = ^SInt8;
		ProcPtr = Ptr; {This is the definition of ProcPtr in Apples Univ Interfaces}

	procedure A;

	begin
		Writeln('Hello');
	end;

	procedure B (procedure X);
	begin
		X;
	end;

{$IFC UNDEFINED THINK_Pascal }
{ ** Not supported in THINK Pascal ** }

	type
		M = procedure;

	var
		n: M;

	procedure C (Y: M);
	begin
		Y;
	end;
{$ENDC}

	procedure D (Z: ProcPtr);
	begin
		Writeln(Ord(Z));
	end;

begin
	B(A);
	D(@A);
  {$IFC UNDEFINED THINK_Pascal }
  { ** Not supported in THINK Pascal ** }
	B(@A);
	n := nil;
	n := A;
	if nil <> n then
		C(n);
	C(A);
	C(@A);
  {$ENDC}
end.
