Program Example10;
uses Dos;

{ Program to demonstrate the FSearch function. }

var
  s : string;
begin
  s:=FSearch(ParamStr(1),GetEnv('PATH'));
  if s='' then
   WriteLn(ParamStr(1),' not Found in PATH')
  else
   Writeln(ParamStr(1),' Found in PATH at ',s);
end.
