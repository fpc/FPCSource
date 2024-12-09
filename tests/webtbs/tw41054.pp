type
	myrec=record
		f1,f2,f3:byte;
	end;
	myenum=(
		asd=0
	);

const
	myarr:array [myenum] of myrec=(
		(f1:1;f2:2;f3:3)
	);

procedure foo;
var
	i:byte;
begin
	for i:=0 to 0 do
		writeln(myarr[myenum(i)].f2);
end;

begin
end.
