program SourceBug;

{$APPTYPE CONSOLE}

{$ifdef FPC}
{$MODE Delphi}
{$endif}

uses
  Variants,
  SysUtils;

type
  TSampleVariant = class(TInvokeableVariantType)
  protected
    {$ifndef FPC}
    function FixupIdent(const AText: string): string; override;
    {$endif}
  public
    procedure Clear(var V: TVarData); override;
    procedure Copy(var Dest: TVarData; const Source: TVarData; const Indirect: Boolean ); override;
    function GetProperty(var Dest: TVarData; const V: TVarData;
      const Name: string): Boolean; override;
    function SetProperty(var V: TVarData; const Name: string;
      const Value: TVarData): Boolean; override;
  end;

procedure TSampleVariant.Clear(var V: TVarData);
begin
  V.VType:=varEmpty;
end;

procedure TSampleVariant.Copy(var Dest: TVarData; const Source: TVarData; const Indirect: Boolean);
begin
  if Indirect and VarDataIsByRef(Source) then
    VarDataCopyNoInd(Dest, Source)
  else with Dest do
    VType:=Source.VType;
end;

{$ifndef FPC}
function TSampleVariant.FixupIdent(const AText: string): string;
begin
  result := AText; // we do not want any uppercase names
end;
{$endif}

function TSampleVariant.GetProperty(var Dest: TVarData; const V: TVarData;
  const Name: string): Boolean;
begin
  assert(V.VType=varType);
  if Name='AnyField' then begin
    variant(Dest) := V.VInt64;
    result := true;
  end else
    result := false;
end;

function TSampleVariant.SetProperty(var V: TVarData; const Name: string;
  const Value: TVarData): Boolean;
begin
  assert(V.VType=varType);
  if Name='AnyField' then begin
    PVarData(@V)^.VInt64 := variant(Value);
    result := true;
  end else
    result := false;
end;

var
  SampleVariant: TSampleVariant;
  v: Variant;
begin
  SampleVariant:=TSampleVariant.Create;
  v := null;
  TVarData(v).VType:=SampleVariant.VarType;
  v.AnyField := 100;
  if v.AnyField=100 then
    writeln('ok') else
    writeln('ERROR: v.AnyField=',v.AnyField);
  readln;
end.