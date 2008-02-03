{$mode objfpc}{$H+}
uses
  SysUtils;

var
  err : boolean;

procedure p;
 var
  AStr,AText: string;
   AValue: int64;
 begin
   //This goes wrong, notice the AStr input and output
   AValue:=1234567890;
   AStr := Format('%0.n',[double(1.0)*AValue]); //1.234.567.890
   AStr := Format('<font color="#ff0000">%s</font>',[AStr]);
   Writeln('Wrong:' +AStr); //Wrong: <font color="#ff0000"></font>????
   if AStr<>'<font color="#ff0000">1,234,567,890</font>' then
     err:=true;
   //This is Ok, notice the changed output AText
   AValue:=2134567890;
   AStr := Format('%0.n',[double(1.0)*AValue]); //2.134.567.890
   AText := Format('<font color="#ff0000">%s</font>',[AStr]);
   Writeln('Ok:' +AText); //Ok 2.134.567.890
   if Atext<>'<font color="#ff0000">2,134,567,890</font>' then
     err:=true;
end;

begin
  p;
  if err then
    halt(1);
end.
