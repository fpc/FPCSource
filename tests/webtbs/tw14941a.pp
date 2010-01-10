program StringTest5;
{$ifdef fpc}
{$mode delphi}
{$endif}
{$V+}
var
   s    :String;

procedure P( s: OpenString);
begin
  writeln(s);
  if (high(s)<>255) or
     (s<>'12345') then
    halt(1);
end;

begin
  P('12345');
  s:='12345';
  p(s);
  {Won't compile.
  FPC or Turbo Pascal mode:  Internal error 200405241
  Delphi mode:  Signal 291.  Save files and restart IDE.  (Can't save.)}
end.
