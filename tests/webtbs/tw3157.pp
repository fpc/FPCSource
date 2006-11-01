{ Source provided for Free Pascal Bug Report 3157 }
{ Submitted by "Michalis Kamburelis" on  2004-06-11 }
{ e-mail: michalis@camelot.homedns.org }

{$mode objfpc}

uses SysUtils, Math;

var
  c:Single;
  temp_float:double;
  temp_int:Integer;
  notcaught: integer;
begin
 notcaught := 2;
 SetExceptionMask(GetExceptionMask - [exOverflow,exUnderflow,exPrecision]);
 try
  { cosh(800) =~ 1.36E+0347, this will fit in Extended but will
    not fit in Single or Double.
    So instruction below should raise Floating point overflow.
    But it does not (yet). }
  c:=cosh(800);  
 except
  on E:Exception do
    begin
      Writeln('Line "c:=..." raised ' +E.ClassName+ ': ' +E.Message);
      dec(notcaught);
    end;
 end;

 temp_float:=9/200;
 try
  { This innocent instruction will raise EOverflow exception.

    Note: if this will be changed to "Round(9/200)" then
    this whole program will run with no exception
    (I guess that it's because "Round(9/200)" will be calculated
    at compile-time). }
  temp_int:=Round(temp_float);
 except
  on E:Exception do
   begin
     Writeln('Line "temp_int:=..." raised ' +E.ClassName+ ': ' +E.Message);
     dec(notcaught);
   end;
 end;
 halt(notcaught);
end.
