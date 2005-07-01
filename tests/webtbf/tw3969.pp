{ %fail }
{$Mode Delphi}
{$R-,X+,V-}
uses crt, tw3969u;

var
  myobj: testobj;

begin
  clrscr;
  myobj.init;
  writeln(myobj.getvar);
  myobj.setvar('antonio');
  myobj.myvar := 'pippo';
  writeln(myobj.myvar);
  myobj.done;
end.
