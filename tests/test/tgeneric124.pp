{ %NORUN }

program tgeneric124;

{$mode delphi}

uses
  Classes;

type
  TNotifyEvent<T> = procedure(Sender: TObject; const item: T) of object;

var
  { this needs to find Classes.TNotifyEvent }
  n: TNotifyEvent;
begin

end.
