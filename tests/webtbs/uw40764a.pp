unit uw40764a;

{$ifdef fpc}
{$mode delphi}
{$endif}

interface


Type
  TFMXObject = class(TObject)
    procedure something; virtual;
    function FindStyleResource(const AStyleLookup: string; const AClone: Boolean = False): TFmxObject; overload; virtual;
  end;

  TBrushObject = class(TFMXObject)
    procedure something; override;
  end;

implementation

function TFMXObject.FindStyleResource(const AStyleLookup: string; const AClone: Boolean = False): TFmxObject;
begin
  Result:=Nil;
end;

procedure TFMXObject.something;

begin
  writeln('here')
end;

procedure TBrushObject.something;

begin
  inherited something;
  Writeln('here too')
end;

end.
  