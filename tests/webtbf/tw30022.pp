{ %fail }

{ %target=darwin }

{$mode delphi}
{$modeswitch objectivec1}
{$modeswitch cblocks}

type
  treadabilityHandler = reference to procedure (l :longint); cdecl;

procedure setReadabilityHandler(ahandler: treadabilityHandler);
  begin
  end;

procedure MyHandler(l: longint);
begin
end;

begin
  setReadabilityHandler(@myhandler)
end.
