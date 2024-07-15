unit tw40764;

{$ifdef fpc}
{$mode delphi}
{$endif}

interface

uses uw40764a,uw40764b;

Type
  
  TBrushFMXStyle = class(TFMXObject)
  end;
  
  TPresentedControl = class(TStyledControl)
    function FindStyleResource(const AStyleLookup: string; const AClone: Boolean = False): TFmxObject; overload; override;  
  end;

  TMyControl = class(TPresentedControl)
    Procedure DoTest;
  end;
  
implementation

  
function TPresentedControl.FindStyleResource(const AStyleLookup: string; const AClone: Boolean = False): TFmxObject;

begin
end;


procedure TMyControl.DoTest;

var
  B : Boolean;
  BrushObject : TBrushObject;

begin
  B:=FindStyleResource<TBrushObject>( 'foreground' , BrushObject );
end;

end.