program tw39981;

{$mode objfpc}{$H+}
{$ModeSwitch functionreferences}

type
  TNotifyProc = reference to procedure();

  TMyClass = class
  public
    procedure One;
    procedure Two;
  end;

{ TMyClass }

var
  OneCounter: Integer = 0;
  TwoCounter: Integer = 0;

procedure TMyClass.One;
begin
  Writeln('One');
  Inc(OneCounter);
end;

procedure TMyClass.Two;
begin
  Writeln('Two');
  Inc(TwoCounter);
end;

var
  One, Two: TNotifyProc;
  MyObject: TMyClass;
  HasError: Boolean = False;
begin
  MyObject := TMyClass.Create;
  One := @MyObject.One;
  Two := @MyObject.Two;
  One(); // writes out One - OK
  Two(); // writes out One - Error

  if One=Two then // yes, they are equal - Error
    HasError := True;
  if not ((OneCounter=1) and (TwoCounter=1)) then // Error: OneCounter=2, TwoCounter=0
    HasError := True;

  if HasError then
    Halt(1);
end.

