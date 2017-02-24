{ %norun }

{$mode objfpc}
{$h+}

unit tw31421a;

interface

type
  TMessageReceivedEvent = function (const Received: TObject): boolean of object;

  TMessageReceivedEventList = class
  private
    function Get(Index: Integer): TMessageReceivedEvent;
  public
    property MyItems[Index: Integer]: TMessageReceivedEvent read Get; default;
    procedure ExecuteAll(A: TMessageReceivedEvent; const Received: TObject);
  end;

implementation

{ TMessageReceivedEventList -------------------------------------------------- }

function TMessageReceivedEventList.Get(Index: Integer): TMessageReceivedEvent;
begin
  //Result := ...;
end;

procedure TMessageReceivedEventList.ExecuteAll(A: TMessageReceivedEvent; const Received: TObject);
var
  Handled: boolean;
begin
  Handled := false;
  Handled := MyItems[0](Received) or Handled;
end;

end.
