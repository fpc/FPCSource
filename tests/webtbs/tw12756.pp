{$mode delphi}
{$M+}

type
  TFooR = object { put "record" here and it works. }
    Thing : integer;
  end;
  
  TFoo = class
  private
    fRecord : TFooR;
  published
    property Thing : integer read fRecord.Thing;
  end;

var
  fFoo : TFoo;
begin
  fFoo := TFoo.Create;
  fFoo.fRecord.Thing:=123;
  if (fFoo.Thing <> 123) then
    halt(1);
  fFoo.free;
end.
