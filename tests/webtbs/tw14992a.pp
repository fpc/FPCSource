{ %target=win32,wince,win64 }
{ %needlibrary }
{ %opt=-gh }
{ %norun }
library dll1;

uses
  popuperr;

begin
  IsMultiThread:=True;
end.
