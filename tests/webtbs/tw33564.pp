program EmptyRealWriter2;

{$mode objfpc}{$h+}

uses
  SysUtils,
  Classes,
  Math;

type
  TMyComp = class(TComponent)
  private
    fI: Integer;
    fL: Integer;
  public
    property I: Integer read fI write fI stored False default 1;
    property L: Integer read fL write fL stored True;
  end;

  TMyComp2 = class(TMyComp)
  published
    property I stored True;
    property L;
  end;

{ TMyComp }

procedure CheckStringRead(const aReader: TBinaryObjectReader; const aExpectedValue: string);
var
  S: string;
begin
  S := aReader.ReadStr;
  if S<>aExpectedValue then
    raise Exception.CreateFmt('Reader error [''%s'' <> ''%s'']', [S, aExpectedValue]);
end;

procedure CheckIntegerRead(const aReader: TBinaryObjectReader; const aExpectedValue: Integer);
var
  I: Integer;
  B: Byte;
begin
  aReader.Read(B, 1);
  case B of
    Ord(vaInt8): I := aReader.ReadInt8;
    Ord(vaInt16): I := aReader.ReadInt16;
    Ord(vaInt32): I := aReader.ReadInt32;
  else
    raise Exception.Create('Reader error: wrong property');
  end;

  if not SameValue(I, aExpectedValue) then
    raise Exception.CreateFmt('Reader error [''%f'' <> ''%f'']', [I, aExpectedValue]);
end;

var
  xStream: TStream;
  xWriter: TWriter;
  C: TMyComp2;
  xReader: TReader;
  xObjReader: TBinaryObjectReader;
begin
  try
    xStream := TMemoryStream.Create;
    C := TMyComp2.Create(nil);
    C.I := 1;

    xWriter := TWriter.Create(xStream, 1024);
    xWriter.WriteComponent(C);
    C.Free;
    xWriter.Free;
    xStream.Position := 0;

    xObjReader := TBinaryObjectReader.Create(xStream, 1);
    CheckStringRead(xObjReader, 'TMyComp2');
    CheckStringRead(xObjReader, '');
    CheckStringRead(xObjReader, 'L');
    CheckIntegerRead(xObjReader, 0);
    if xObjReader.ReadInt16<>0 then
      raise Exception.Create('Too many properties were streamed');
    if xStream.Position <> xStream.Size then
      raise Exception.CreateFmt('Too many properties were streamed %d <> %d.', [xStream.Position, xStream.Size]);
    xObjReader.Free;
    xStream.Position := 0;

    C := TMyComp2.Create(nil);
    xReader := TReader.Create(xStream, 1024);
    xReader.BeginReferences;
    xReader.ReadComponent(C);
    xReader.EndReferences;
    C.Free;
    xReader.Free;
    xStream.Free;
  except
    on E: Exception do
    begin
      Writeln('Error: ', E.ClassName);
      WriteLn(E.Message);
      Halt(1);
    end;
  end;
end.
