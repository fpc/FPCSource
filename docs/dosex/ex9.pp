Program Example9;
uses Dos;

{ Program to demonstrate the GetFTime function. }

Function L0(w:word):string;
var
  s : string;
begin
  Str(w,s);
  if w<10 then
   L0:='0'+s
  else
   L0:=s;
end;

var
  f    : File;
  Time : Longint;
  DT   : DateTime;
begin
  if Paramcount>0 then
    Assign(f,ParamStr(1))
  else
    Assign(f,'ex9.pp' );
  Reset(f);
  GetFTime(f,Time);
  Close(f);
  UnPackTime(Time,DT);
  Write ('File ',ParamStr(1),' is last modified on ');
  Writeln (L0(DT.Month),'-',L0(DT.Day),'-',DT.Year,
           ' at ',L0(DT.Hour),':',L0(DT.Min));
end.
