{%norun}
{$MODE OBJFPC}
uses typinfo;

type
  TMyRecord = record end;

  {$M+}
  TMyClass = class
  published
    procedure MyMethod(MyArgument: TMyRecord); virtual;
  end;
  {$M-}

procedure TMyClass.MyMethod(MyArgument: TMyRecord);
begin
end;

var
  X: PTypeInfo;
begin
  X := TypeInfo(@TMyClass.MyMethod);
end.
