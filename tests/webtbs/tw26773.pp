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
  if Name='IntField' then
    begin
      variant(Dest) := V.VInt64;
      result := true;
    end
  else if Name='FloatField' then
    begin
      variant(Dest) := V.VDouble;
      result := true;
    end
  else if Name='BoolField' then
    begin
      variant(Dest) := V.VBoolean;
      result := true;
    end
  else
    result := false;
end;

function TSampleVariant.SetProperty(var V: TVarData; const Name: string;
  const Value: TVarData): Boolean;
begin
  assert(V.VType=varType);
  if Name='IntField' then
    begin
      PVarData(@V)^.VInt64 := variant(Value);
      result := true;
    end
  else if Name='FloatField' then
    begin
      PVarData(@V)^.VDouble := variant(Value);
      result := true;
    end
  else if Name='BoolField' then
    begin
      PVarData(@V)^.VBoolean := variant(Value);
      result := true;
    end
  else
    result := false;
end;

var
  SampleVariant: TSampleVariant;
  v: Variant;

  GB1 : Byte;
  GS1 : Shortint;
  GW : Word;
  GL : longint;
  gsi : single;
  gd : double;
  gi64 : int64;
  gdate: tdatetime;
  gb: boolean;
begin
  SampleVariant:=TSampleVariant.Create;
  v := null;
  TVarData(v).VType:=SampleVariant.VarType;
  v.IntField := 100;
  if v.IntField<>100 then
    halt(1);

  gb1:=128;
  gs1:=127;
  gw:=32768;
  gl:=longint($b100dbad);
  gsi:=12345789.5;
  gd:=999991234889879.5;
  gi64:=$813245678901234;
  gdate:=now;
  gb:=false;

  v.IntField:=gb1;
  if v.IntField<>gb1 then
    halt(2);

  v.IntField:=gs1;
  if v.IntField<>gs1 then
    halt(3);

  v.IntField:=gw;
  if v.IntField<>gw then
    halt(4);

  v.IntField:=gl;
  if v.IntField<>gl then
    halt(5);

  v.FloatField:=gsi;
  if v.FloatField<>gsi then
    halt(6);

  v.FloatField:=gd;
  if v.FloatField<>gd then
    halt(7);

  v.IntField:=gi64;
  if v.IntField<>gi64 then
    halt(8);

  v.FloatField:=gdate;
  if v.FloatField<>gdate then
    halt(9);

  v.BoolField:=gb;
  if boolean(v.BoolField)<>gb then
    halt(10);

end.
