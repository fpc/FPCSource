{ Old file: tbs0279.pp }
{ crash with ansistring and new(^ansistring)           OK 0.99.13 (PFV) }

{$H+}
Program AnsiTest;

Type
   PS=^String;

procedure test;
var
  P:PS;
Begin
  p:=New(PS);
  P^:='';
  P^:=P^+'BLAH';
  P^:=P^+' '+P^;
  Writeln(P^);
  Dispose(P);

  New(P);
  P^:='';
  P^:=P^+'BLAH';
  P^:=P^+' '+P^;
  Writeln(P^);
  Dispose(P);
end;

var
  membefore : longint;

begin
  membefore:=memavail;
  test;
  if membefore<>memavail then
    begin
      Writeln('Memory hole using pointers to ansi strings');
      Halt(1);
    end
  else
    Writeln('No memory hole with pointers to ansi strings');
end.

