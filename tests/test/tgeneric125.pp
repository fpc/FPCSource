{ %NORUN }

program tgeneric125;

{$mode objfpc}

uses
  Classes;

type
  generic TNotifyEvent<T> = procedure(Sender: TObject; const item: T) of object;

var
  { this needs to find Classes.TNotifyEvent }
  n: TNotifyEvent;
begin

end.
