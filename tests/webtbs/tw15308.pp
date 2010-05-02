program testformatfloat;

uses SysUtils;

const
   val_format: string = '0.0000E+000';
   
var
   input: extended;

begin
   decimalseparator:='.';
   input:=1.05e2;
   if (formatfloat(val_format,input)<>'1.0500E+002') then
     begin
       writeln(formatfloat(val_format,input));
       halt(1);
     end;
   input:=1.06e2;
   if (formatfloat(val_format,input)<>'1.0600E+002') then
     begin
       writeln(formatfloat(val_format,input));
       halt(2);
     end;
end.
