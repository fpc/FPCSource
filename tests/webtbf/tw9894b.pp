{ %fail }

{$mode delphi}

unit tw9894b;

interface

Type
  TMyInteger = Class
    Value : Integer;
  end;

  TMyRec2 = record
    MyInteger : TMyInteger;
  end;

  TMyRec = record
    MyRec2 : TMyRec2;
  end;

  TMyClass = Class
    FMyRec : TMyRec;
  Private
    Procedure DoSomething;
    Property MyRec : TMyRec Read FMyRec;
  end;

Implementation

Procedure TMyClass.DoSomething;

begin
  MyRec.MyRec2.MyInteger:=TMyInteger(nil);
end;

end.
