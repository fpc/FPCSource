{ %FAIL }
{ %EXPECTMSGS=5099 }

program tfuncref64;

{ the object a method is taken of for a function reference is captured: the function result cannot be }

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
function Make(out aRef: TProcRef): TSink;
begin
  Result := TSink.Create;
  aRef := Result.OnRef;
end;

var
  lSink: TSink;
  lRef: TProcRef;
begin
  lSink := Make(lRef);
  lRef(1);
  lSink.Free;
end.