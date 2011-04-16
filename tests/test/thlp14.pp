{ %FAIL }

{ message methods are forbidden in mode ObjFPC }
program thlp14;

{$ifdef fpc}
  {$mode objfpc}
{$endif}

type
  TMessage = record
    ID: Word;
  end;

  TObjectHelper = class helper for TObject
    procedure Message(var aMessage: TMessage); message 42;
  end;

procedure TObjectHelper.Message(var aMessage: TMessage);
begin
end;

begin
end.

