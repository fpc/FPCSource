{$ifdef fpc}{$mode objfpc}{$endif}
uses sysutils;
 var S:String;
     par:array [0..1] of TVarRec; { array of const here is illegal ! }
begin
 writeln(format(S, par));
end.
