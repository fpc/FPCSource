{ Source provided for Free Pascal Bug Report 2904 }
{ Submitted by "Alexey Barkovoy" on  2004-01-17 }
{ e-mail: clootie@ixbt.com }
program Project2;
{$mode objfpc}
{$goto on}
{$APPTYPE CONSOLE}
uses SysUtils;

var
  i: Integer;
label
  LFail;
begin
  try
LFail: // Cleanup
//  i:= i; //todo: Uncomment this line for FPC_1.9.2 BUG workaround !!!
  except
    on EOutOfMemory do i:= 1;
    else i:= 2;
  end;
end.
