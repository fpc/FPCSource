{ Source provided for Free Pascal Bug Report 3176 }
{ Submitted by "Marcel Martin" on  2004-06-21 }
{ e-mail: mm10@ellipsa.net }
program test;
{$MODE OBJFPC}
{$GOTO ON}

label DONE1;
var i : Longint;
begin
  i := 0;
  try
    if i < 0 then goto DONE1;
    i := 1;

    //...

DONE1:
  finally
    i := 2;
  end;
  if i<>2 then
    halt(1);
end.
//DONE1:
//    begin end; <- inserting this line fixes the problem
//  finally
