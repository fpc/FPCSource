{$mode objfpc}
program VariantTest;

uses variants;

var
    i: integer;
    laenge: integer;
    integerbuffer: integer;
    integerarray: variant;
    singlebuffer: single;
    singlearray: variant;
    error : boolean;
begin
    laenge := 20;
    integerarray := VarArrayCreate([1,laenge],varInteger);
    singlearray  := VarArrayCreate([1,laenge],varSingle);
    for i := 1 to laenge do
    begin
	integerbuffer := i;
	singlebuffer  := i;
	integerarray[i] := integerbuffer;
	singlearray[i]  := singlebuffer;
    end;

    writeln ('** Program VariantTest **');
    writeln;
    writeln ('integerarray singlearray');
    writeln;
    error:=false;
    for i := 1 to laenge do
    begin
	integerbuffer := integerarray[i];
	singlebuffer  := singlearray[i];
	writeln (integerbuffer:12, singlebuffer:12:7);
        error:=error or (integerbuffer<>singlebuffer);
    end;
    if error then
      halt(1)
    else
      writeln('ok');
end.
