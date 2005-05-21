{ this tests the pi routine, as an inline }
program tpi;

const
 PI_CONST = 3.1459;
 { the following expression also works on constants }
 PI_CONST_VALUE = pi;

 procedure fail;
  begin
    WriteLn('Failed!');
    halt(1);
  end;

var
 value : real;
 _result : boolean;
Begin
  Write('Pi() test...');
  _result := true;
  value:=pi;
  if trunc(value) <> trunc(PI_CONST) then
     _result := false;
  If trunc(Pi) <> trunc(PI_CONST) then
     _result := false;
  If trunc(Pi) <> trunc(PI_CONST_VALUE) then
     _result := false;
  if not _result then
     fail;
  WriteLn('Success!');
end.
