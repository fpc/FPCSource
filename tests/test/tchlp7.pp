{%FAIL}

{ message methods are not allowed in mode ObjFPC }
program tchlp7;

{$mode objfpc}

type
  TMessage = record
    ID: LongWord;
  end;

  TObjectHelper = class helper for TObject
    procedure SomeMessage(var aMessage: TMessage); message 42;
  end;

procedure TObjectHelper.SomeMessage(var aMessage: TMessage);
begin

end;

begin

end.

