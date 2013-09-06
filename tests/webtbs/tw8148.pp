program ValVsArrayOfChar;

{$IFDEF FPC}
  {$mode delphi}
{$ENDIF}

procedure test(a: ansistring);
begin
end;


var
  Code : Integer;
  D : Double;
  s : Array[byte] of Char;
  s2 : Array[0..100] of Char;
begin
  s := '123';
  s2 := '123';
  test(s);
  Val(s, D, Code); // compiles only in delphi
  if (abs(d-123.0) > 0.00001) then
    halt(1);
  Val(PChar(@s), D, Code); // compiles in both delphi and FPC
  if (abs(d-123.0) > 0.00001) then
    halt(1);
  Val(s2, D, Code); // compiles only in delphi
  if (abs(d-123.0) > 0.00001) then
    halt(1);
  writeln('ok');
end.
