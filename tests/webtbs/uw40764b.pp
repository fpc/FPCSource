unit uw40764b;

{$ifdef fpc}
{$mode delphi}
{$endif}

interface

uses uw40764a;

Type

  TStyledControl = class (tfmxobject)
    function FindStyleResource(const AStyleLookup: string; const Clone: Boolean = False): TFmxObject; overload; override;
    function FindStyleResource<T: TFmxObject>(const AStyleLookup: string; var AResource: T): Boolean; overload;
  end;

implementation

function TStyledControl.FindStyleResource(const AStyleLookup: string; const Clone: Boolean = False): TFmxObject; 

begin
  Result:=Nil;
end;

function TStyledControl.FindStyleResource<T>(const AStyleLookup: string; var AResource: T): Boolean;

begin
  Result:= aStyleLookup<>'';
  if Result then
    aResource:=T(TObject.Create)
  else
    aResource:=Nil;
end;

end.
