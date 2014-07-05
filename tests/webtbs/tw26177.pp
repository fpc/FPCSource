program tw26177;

{$MODE DELPHI}
{$MODESWITCH TYPEHELPERS}

uses
  Classes;

type
  TInt32Helper = record helper for Int32
    procedure Foo(Sender: TObject);
  end;

var
  value: Int32 = 0;

procedure TInt32Helper.Foo(Sender: TObject);
begin
  value := Self;
end;

var
  i: Int32 = 10;
  m: TNotifyEvent;
begin
  m := i.Foo;
  // Data is equal 10 (!) but should be equal to @i
  //WriteLn(Int32(TMethod(m).Data));
  // TMethod(m).Data := @i; < workaround for bug
  try
    m(nil); // External SIGSEGV!
    if value <> 10 then
      Halt(2);
  except
    Halt(1);
  end;
end.

