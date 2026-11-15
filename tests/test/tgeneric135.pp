{ in mode Delphi a "<" behind a generic's name is still a comparison where a class of that name is visible:
  the class type's name is a value, a class reference, which an overloaded "<" can take }
program tgeneric135;

{$mode delphi}

uses
  ugeneric135;

type
  TName<T> = class
  end;

  TProbe = record
    class operator LessThan(aLeft: TClass; aRight: TProbe): Boolean;
  end;

class operator TProbe.LessThan(aLeft: TClass; aRight: TProbe): Boolean;
begin
  Result := aLeft = ugeneric135.TName;
end;

var
  lProbe: TProbe;
begin
  if not (TName < lProbe) then
    Halt(1);
  WriteLn('ok');
end.
