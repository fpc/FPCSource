program EmptyStringWriter;

{$mode objfpc}{$h+}

uses
  SysUtils, Classes;

type
  TMyComp = class(TComponent)
  public const
    cDefS = 'default';
  private
    fS: string;
    fT: string;
    fU: string;
    fSn: string;
    fTn: string;
    fUn: string;
    fSdef: string;
    function SStored: Boolean;
    function SnStored: Boolean;
    function SdefStored: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property S: string read fS write fS stored SStored nodefault;
    property T: string read fT write fT nodefault;
    property U: string read fU write fU;
    property Sn: string read fSn write fSn stored SnStored nodefault;
    property Tn: string read fTn write fTn nodefault;
    property Un: string read fUn write fUn;
    property Sdef: string read fSdef write fSdef stored SdefStored nodefault;
  end;

{ TMyComp }

constructor TMyComp.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fS := cDefS;
  fSn := cDefS;
  fSdef := cDefS;
end;

function TMyComp.SdefStored: Boolean;
begin
  Result := fSdef <> cDefS;
end;

function TMyComp.SnStored: Boolean;
begin
  Result := fSn <> cDefS;
end;

function TMyComp.SStored: Boolean;
begin
  Result := fS <> cDefS;
end;

const
  ExpectedOutput: RawByteString = #7#84#77#121#67#111#109#112#0#1#83#6#0#1#84#6#0#2#83#110#6#1#110#2#84#110#6#1#110#2#85#110#6#1#110#0#0;
var
  xStream: TStream;
  xWriter: TWriter;
  C: TMyComp;
  xReader: TReader;
  B: Byte;
  I: Integer;
begin
  xStream := TMemoryStream.Create;
  C := TMyComp.Create(nil);
  C.S := '';
  C.T := '';
  C.U := '';
  C.Sn := 'n';
  C.Tn := 'n';
  C.Un := 'n';
  //keep SDef to default value -> won't be streamed
  xWriter := TWriter.Create(xStream, 1024);
  xWriter.WriteComponent(C);
  C.Free;
  xWriter.Free;
  xStream.Position := 0;
  I := 1;
  while xStream.Read(B, 1) = 1 do
  begin
    if (I>Length(ExpectedOutput)) or (B<>Ord(ExpectedOutput[I])) then
      raise Exception.CreateFmt('Wrong output at character index: %d', [I]);
    Inc(I);
  end;
  xStream.Position := 0;
  C := TMyComp.Create(nil);
  xReader := TReader.Create(xStream, 1024);
  xReader.BeginReferences;
  xReader.ReadComponent(C);
  xReader.EndReferences;
  if C.S<>'' then
    raise Exception.Create('S invalid.');
  if C.T<>'' then
    raise Exception.Create('T invalid.');
  if C.U<>'' then
    raise Exception.Create('U invalid.');
  if C.Sn<>'n' then
    raise Exception.Create('Sn invalid.');
  if C.Tn<>'n' then
    raise Exception.Create('Tn invalid.');
  if C.Un<>'n' then
    raise Exception.Create('Un invalid.');
  if C.Sdef<>TMyComp.cDefS then
    raise Exception.Create('Un invalid.');
  C.Free;
  xReader.Free;
  xStream.Free;
end.

