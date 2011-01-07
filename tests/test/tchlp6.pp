{%NORUN}

{ message methods are allowed in mode Delphi }
program tchlp6;

{$ifdef fpc}
  {$mode delphi}
{$endif}

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

