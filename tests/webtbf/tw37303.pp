{ %fail }
program gen_default;

{$mode delphi}

uses SysUtils;

type

  TBla = record
    Bla: Integer;
  end;

  //TBla = array[1..4] of Integer;

  //TBla = type Integer;


procedure SetValue<T>(out v: T; const aValue: T = Default(T));
begin
  v := aValue;
end;

procedure Test;
var
  Bla: TBla;
begin
  SetValue<TBla>(Bla);
  WriteLn(Format('Bla = (Bla: %d)', [Bla.Bla]));
  //WriteLn(Format('Bla = (%d, %d, %d, %d)', [Bla[1], Bla[2], Bla[3], Bla[4]]));
  //WriteLn('Bla = ', Bla);
end;

begin
  Test;
end.

