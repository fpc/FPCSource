{ %norun }
{$mode objfpc}

type
  TMyRecord = record
    Data: pointer;
  end;

  TMyClass = class
  private
    FRef: TMyRecord;
  public
    property Ref: TMyRecord read FRef write FRef;
  end;

var
  MyClass1, MyClass2: TMyClass;

procedure test;
begin
  Move(MyClass1.Ref.Data^, MyClass2.Ref.Data^, 1);
end;


begin
end.
