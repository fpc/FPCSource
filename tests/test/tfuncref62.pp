{ %FAIL }
{ %EXPECTMSGS=5099 }

program tfuncref62;

{ the object a method is taken of for a function reference is captured: an out parameter cannot be }

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
function Make(out aSink: TSink): TProcRef;
begin
  aSink := TSink.Create;
  Result := aSink.OnRef;
end;

var
  lSink: TSink;
begin
  Make(lSink)(1);
  lSink.Free;
end.