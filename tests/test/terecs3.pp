program terecs3;

{$mode delphi}
{$apptype console}

uses
  terecs_u1;

var
  F: TFoo;
begin
  F.F3 := 0;
  F.F4 := 1;
  if F.F3 <> 0 then
    halt(1);
  if F.F4 <> 1 then
    halt(2);
  if F.C <> 1 then
    halt(3);
  if F.Test(3) <> 4 then
    halt(4);
  if F.Test1(4) <> 5 then
    halt(5);
  if F.F5 <> 6 then
    halt(6);
  F.P3 := 7;
  if F.P3 <> 7 then
    halt(7);
  F.P5 := 8;
  if F.P5 <> 8 then
    halt(8);
  // test Self
  F.Test2;
  F.Test3;
  WriteLn('ok');
end.