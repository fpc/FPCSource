{$mode delphi}

unit tw9894;

interface

Type
  PMyInteger = ^TMyInteger;
  TMyInteger = record
    Value : Integer;
  end;

  TMyRec = record
    MyInteger : PMyInteger;
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
  MyRec.MyInteger^.Value:=3;
end;

end.
