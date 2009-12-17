program test;

uses sysutils;

 var
   a : array [Boolean,Boolean] of string;
   b : wordbool;
begin
  a[False,True] := 'True';
  a[False,False] := 'False';
  a[True,True] := 'True';
  a[True,False] := 'False';
  b := True;
  if a[false,b]<>'True' then
    halt(1);
end.
