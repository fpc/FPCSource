{ %OPT=-CE }
{ %RESULT=207 }
{ Source provided for Free Pascal Bug Report 3160 }
{ Submitted by "Michalis Kamburelis" on  2004-06-12 }
{ e-mail: michalis@camelot.homedns.org }
var A:Extended;
begin
 { All lines below will raise runtime error 207
   (Invalid floating point operation, converted to EInvalidOp exception)
   under Win32.

   But under Linux they raise runtime error 216
   (General protection fault, converted to EAccessViolation). }
 A:=-3;  Writeln(Ln(A));
end.
