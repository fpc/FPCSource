{ %FAIL }
{ Old file: tbf0148.pp }
{ crash when setting function result of a declared but not yet implemented function in another function }

unit test;

interface

Function t(a: Byte): byte;
Function DoT(b: byte): Byte;

implementation

Function t(a: Byte): Byte;
var f: byte;
Begin
  DoT := f;
End;

Function DoT(b: byte): Byte;
Begin
End;

end.
