{$mode objfpc}
uses sysUtils;

 type
      t = object
       f:integer;
       function m: AnsiString;
      end;

 function t.m: AnsiString;
 begin
  result:=IntToStr(f);
 end;

 var ti:t;

begin
 ti.f:=1; // no vmt for t - constructor call is not needed
 writeln(format('%s', [ti.m])); // this works
 writeln(format('%s, %s', [ti.m, ti.m])); // this does not - the same story with classes
end.
