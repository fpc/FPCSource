{ Source provided for Free Pascal Bug Report 3612 }
{ Submitted by "Alexey Barkovoy" on  2005-01-30 }
{ e-mail: clootie@ixbt.com }
var
  str: array[0..200] of WideChar;
  w: PWideChar;
  s: String;
begin
  str:= 'abcdefgh';
  w:= str;
  s:= w;
  WriteLn(s);
  w:= str + 2;
  s:= w;
  WriteLn(s); // will output "bcdefgh" instead of "cdefgh"
  if s<>'cdefgh' then
    halt(1);
end.
