{$mode delphi}

unit tw9894a;

interface

Type
  TMyInteger = Class
    Value : Integer;
  end;

  TMyRec = record
    MyInteger : TMyInteger;
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
  MyRec.MyInteger.Value:=3;
end;

end.
