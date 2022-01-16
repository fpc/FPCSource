program EmptyRealWriter;

{$mode objfpc}{$h+}

uses
  SysUtils, Classes, Math;

type
  TMyComp = class(TComponent)
  public const
    cDefS = Pi;
  private
    fS: Double;
    fT: Double;
    fU: Double;
    fSn: Double;
    fTn: Double;
    fUn: Double;
    fSdef: Double;
    function SStored: Boolean;
    function SnStored: Boolean;
    function SdefStored: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property S: Double read fS write fS stored SStored nodefault;
    property T: Double read fT write fT nodefault;    // stored implicitely True
    property U: Double read fU write fU;              // stored implicitely True, default=0
    property Sn: Double read fSn write fSn stored SnStored nodefault;
    property Tn: Double read fTn write fTn nodefault; // stored implicitely True
    property Un: Double read fUn write fUn;           // stored implicitely True, default=0
    property Sdef: Double read fSdef write fSdef stored SdefStored nodefault;
  end;
  TMyCompClass = class of TMyComp;

  TMyComp2 = class(TMyComp)
  published
    property S;
    property T;
    property U;
    property Sn;
    property Tn;
    property Un;
    property Sdef;
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
  Result := not SameValue(fSdef, cDefS);
end;

function TMyComp.SnStored: Boolean;
begin
  Result := not SameValue(fSn, cDefS);
end;

function TMyComp.SStored: Boolean;
begin
  Result := not SameValue(fS, cDefS);
end;

procedure CheckStringRead(const aReader: TBinaryObjectReader; const aExpectedValue: string);
var
  S: string;
begin
  S := aReader.ReadStr;
  if S<>aExpectedValue then
    raise Exception.CreateFmt('Reader error [''%s'' <> ''%s'']', [S, aExpectedValue]);
end;

procedure CheckFloatRead(const aReader: TBinaryObjectReader; const aExpectedValue: Double);
var
  F: Double;
begin
  if aReader.ReadInt8 <> Ord(vaExtended) then
    raise Exception.Create('Reader error: wrong property');
  F := aReader.ReadFloat;
  if not SameValue(F, aExpectedValue) then
    raise Exception.CreateFmt('Reader error [''%f'' <> ''%f'']', [F, aExpectedValue]);
end;

var
  xStream: TStream;
  xWriter: TWriter;
  Cl: TMyCompClass;
  C: TMyComp;
  xReader: TReader;
  xObjReader: TBinaryObjectReader;
  I: Integer;
begin
  for I := 0 to 1 do
  begin
    try
      case I of
        0: Cl := TMyComp;
        1: Cl := TMyComp2;
      end;
      xStream := TMemoryStream.Create;
      C := Cl.Create(nil);
      C.S := 0;
      C.T := 0;
      C.U := 0;
      C.Sn := 5;
      C.Tn := 5;
      C.Un := 5;
      //keep SDef to default value -> won't be streamed
      xWriter := TWriter.Create(xStream, 1024);
      xWriter.WriteComponent(C);
      C.Free;
      xWriter.Free;
      xStream.Position := 0;

      xObjReader := TBinaryObjectReader.Create(xStream, 1);
      CheckStringRead(xObjReader, Cl.ClassName);
      CheckStringRead(xObjReader, '');
      CheckStringRead(xObjReader, 'S');
      CheckFloatRead(xObjReader, 0);
      CheckStringRead(xObjReader, 'T');
      CheckFloatRead(xObjReader, 0);
      CheckStringRead(xObjReader, 'Sn');
      CheckFloatRead(xObjReader, 5);
      CheckStringRead(xObjReader, 'Tn');
      CheckFloatRead(xObjReader, 5);
      CheckStringRead(xObjReader, 'Un');
      CheckFloatRead(xObjReader, 5);
      if xObjReader.ReadInt16<>0 then
        raise Exception.Create('Too many properties were streamed');
      if xStream.Position <> xStream.Size then
        raise Exception.Create('Too many properties were streamed.');
      xObjReader.Free;
      xStream.Position := 0;

      C := Cl.Create(nil);
      xReader := TReader.Create(xStream, 1024);
      xReader.BeginReferences;
      xReader.ReadComponent(C);
      xReader.EndReferences;
      if not SameValue(C.S, 0) then
        raise Exception.Create('S invalid.');
      if not SameValue(C.T, 0) then
        raise Exception.Create('T invalid.');
      if not SameValue(C.U, 0) then
        raise Exception.Create('U invalid.');
      if not SameValue(C.Sn, 5) then
        raise Exception.Create('Sn invalid.');
      if not SameValue(C.Tn, 5) then
        raise Exception.Create('Tn invalid.');
      if not SameValue(C.Un, 5) then
        raise Exception.Create('Un invalid.');
      if not SameValue(C.Sdef, TMyComp.cDefS) then
        raise Exception.Create('Sdef invalid.');
      C.Free;
      xReader.Free;
      xStream.Free;
    except
      on E: Exception do
      begin
        Writeln('Class: ', Cl.ClassName);
        Writeln('Error: ', E.ClassName);
        WriteLn(E.Message);
        Halt(1);
      end;
    end;
  end;
end.

