{ Old file: tbs0279.pp }
{ crash with ansistring and new(^ansistring)           OK 0.99.13 (PFV) }

{$H+}
Program AnsiTest;
uses
  erroru;

Type
   PS=^String;
var
  mem : ptrint;


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

begin
  DoMem(mem);
  test;
  if DoMem(mem)<>0 then
    halt(1);
end.
