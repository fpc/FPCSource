{ %OPT=-O2 }
{ Source provided for Free Pascal Bug Report 1932 }
{ Submitted by "Rob Kolstad" on  2002-04-15 }
{ e-mail: kolstad@ace.delos.com }
program breaksusingO2;
var
    i, ans:longint;
    data:array[1..2,0..1] of longint;
begin
    data[1,0] := 2; data[1,1] := 7;
    data[2,0] := 6; data[2,1] := 9;
    i:= 1;
    ans := 0;
    writeln ('calculated diff = ',data[i,1]-data[i+1,0]);
    if data[i,1]>data[i+1,0] then begin
        ans := data[i,1]-data[i+1,0];
        // I think that data[i+1,0] is being evaluated to 0...RK
    end;
    // ans should be 1
    writeln('ans should = 1, but ans=', ans);
    if ans <> 1 then
      halt(1);
end.
