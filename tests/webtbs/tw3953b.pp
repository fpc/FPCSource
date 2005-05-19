{ Source provided for Free Pascal Bug Report 3953 }
{ Submitted by "Jesus Reyes A." on  2005-05-08 }
{ e-mail: jesusrmx@yahoo.com.mx }
program CompareNULLVars;
uses variants;
var
  A,B: Variant;
begin
  A := NULL;
  B := NULL;
  if A=B then
    WriteLn('A and B are equal')
  else
    begin
      WriteLn('A and B are NOT equal');
      halt(1);
    end;
end.
