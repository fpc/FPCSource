{ %FAIL }
{ %EXPECTMSGS=5099 }

program tfuncref63;

{ the object a method is taken of for a function reference is captured: an open array parameter cannot be }

{$mode delphi}
{$modeswitch functionreferences}

type
  TProcRef = reference to procedure(aSize: LongInt);

  TSink = class
    procedure OnRef(aSize: LongInt);
  end;

procedure TSink.OnRef(aSize: LongInt);
begin
end;
function Make(const aSinks: array of TSink): TProcRef;
begin
  Result := aSinks[0].OnRef;
end;

var
  lSink: TSink;
begin
  lSink := TSink.Create;
  Make([lSink])(1);
  lSink.Free;
end.