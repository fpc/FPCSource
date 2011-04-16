{ %NORUN }

{ message methods are allowed in mode Delphi }
program thlp13;

{$ifdef fpc}
  {$mode delphi}
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
