{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2022 by Michael van Canneyt and other members of the
    Free Pascal development team

    test report streamer

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit tcreportstreamer;

{$mode objfpc}{$H+}
{.$define writejson}
{.$define verbosedebug}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, fpcanvas, fpjson,
  fpreport,fpreportstreamer;

type

  TReportStreamTester = class(TTestCase)
  Private
    procedure FillBytes(S: TStream; AMax: Byte);
  protected
    FRD : TFPReportJSONStreamer;
    procedure SetUp; override;
    procedure TearDown; override;
  end;


  TTestReportDOM = class(TReportStreamTester)
  Public
    procedure TestStream(DoReset: Boolean);
  published
    procedure TestCreate;
    procedure TestAdd;
    procedure TestFind1;
    procedure TestFind2;
    procedure TestPush;
    procedure TestPop1;
    procedure TestStreamToHex;
    procedure TestStreamToHex2;
    procedure TestStreamEquals1;
    procedure TestStreamEquals2;
    procedure TestStreamEquals3;
    procedure TestStreamEquals4;
    procedure TestHexToStream;
    procedure TestWriteInteger1;
    procedure TestWriteString1;
    procedure TestWriteString2;
    procedure TestWriteBoolean1;
    procedure TestWriteBoolean2;
    procedure TestWriteFloat1;
    procedure TestWriteFloat2;
    procedure TestWriteFloat3;
    procedure TestWriteDateTime1;
    procedure TestWriteDateTime2;
    procedure TestWriteDateTime3;
    procedure TestWriteStream1;
    procedure TestWriteIntegerDiff1;
    procedure TestWriteIntegerDiff2;
    procedure TestWriteStringDiff1;
    procedure TestWriteStringDiff2;
    procedure TestWriteBooleanDiff1;
    procedure TestWriteBooleanDiff2;
    procedure TestWriteFloatDiff1;
    procedure TestWriteFloatDiff2;
    procedure TestWriteDateTimeDiff1;
    procedure TestWriteDateTimeDiff2;
    procedure TestWriteDateTimeDiff3;
    procedure TestWriteStreamDiff1;
    procedure TestWriteStreamDiff2;
    procedure TestWriteStreamDiff3;
    procedure TestReadInteger1;
    procedure TestReadInteger2;
    procedure TestReadInteger3;
    procedure TestReadString1;
    procedure TestReadString2;
    procedure TestReadString3;
    procedure TestReadDateTime1;
    procedure TestReadDateTime2;
    procedure TestReadDateTime3;
    procedure TestReadDateTime4;
    procedure TestReadDateTime5;
    procedure TestReadBoolean1;
    procedure TestReadBoolean2;
    procedure TestReadBoolean3;
    procedure TestReadBoolean4;
    procedure TestReadFloat1;
    procedure TestReadFloat2;
    procedure TestReadFloat3;
    procedure TestReadFloat4;
    procedure TestReadStream1;
    procedure TestReadStream2;
    procedure TestReadStream3;
    procedure TestALL;
  end;


  TTestReportFrameDom = Class(TReportStreamTester)
  private
    FF,F2 : TFPReportFrame;
    procedure FillFF;
  protected
    procedure Setup; override;
    procedure TearDown; override;
  published
    procedure TestWrite;
    procedure TestWriteDiff;
    procedure TestRead;
  end;


  TTestReportLayoutDom = Class(TReportStreamTester)
  private
    FL,F2 : TFPReportLayout;
    procedure FillFL;
  protected
    procedure Setup; override;
    procedure TearDown; override;
  published
    procedure TestWrite;
    procedure TestWriteDiff;
    procedure TestRead;
  end;


  TTestReportElementDOM =  Class(TReportStreamTester)
  private
    FE,F2 : TFPReportElement;
    procedure FillFE;
  protected
    procedure Setup; override;
    procedure TearDown; override;
  published
    procedure TestWrite1;
    procedure TestWriteDiff1;
    procedure TestWriteDiff2;
    procedure TestRead1;
    procedure TestRead2;
  end;


  TTestReportElementWithChildrenDOM = class(TReportStreamTester)
  private
    FE, F2: TFPReportElementWithChildren;
    procedure FillFE;
  protected
    procedure Setup; override;
    procedure TearDown; override;
  published
    procedure TestWrite;
    procedure TestRead;
  end;


  TTestReportPageHeader = class(TReportStreamTester)
  private
    FE, F2: TFPReportPageHeaderBand;
    procedure FillFE;
  protected
    procedure Setup; override;
    procedure TearDown; override;
  published
    procedure TestWrite;
    procedure TestWrite2;
    procedure TestRead;
  end;


implementation


{ TTestReportDOM }

procedure TTestReportDOM.TestStream(DoReset: Boolean);
Var
  S : TMemoryStream;
  B : Byte;
  T,H : String;
begin
  S:=TMemoryStream.Create;
  try
    FillBytes(S,255);
    S.Position:=0;
    T:=FRD.StreamToHex(S);
    AssertEquals('Stream position is zero',0,S.Position);
    AssertEquals('Correct number of bytes returned by streamtohex',512,Length(T));
    For B:=0 to 255 do
      begin
      H:=Copy(T,1,2);
      Delete(T,1,2);
      AssertEquals(Format('Correct value at position %d',[b]),H,HexStr(B,2));
      end;
  Finally
    S.Free;
  end;
end;

procedure TTestReportDOM.TestCreate;
begin
  AssertTrue('Failed on 1', Assigned(FRD.JSON));
end;

procedure TTestReportDOM.TestAdd;
var
  E1, E2: TJSONObject;
begin
  AssertTrue('Failed on 1', FRD is TFPReportJSONStreamer);
  E1 := FRD.JSON;
  E2 := TJSONObject(FRD.NewElement('MyElement'));
  AssertNotNull('NewElement returns result', E2);
  AssertSame('NewElement is child of current element', E2, E1.Find('MyElement'));
  AssertEquals('New element created with correct name', '{ "MyElement" : {} }', E1.AsJSON);
//  AssertSame('New element is current element',E2,FRD.CurrentElement);
end;

procedure TTestReportDOM.TestFind1;
var
  E1, E2, E3: TJSONObject;
begin
  AssertTrue('Failed on 1', FRD is TFPReportJSONStreamer);

  E1 := TJSONObject(FRD.NewElement('element1'));
  E2 := TJSONObject(FRD.NewElement('element2'));

  FRD.CurrentElement := E1;
  E3 := TJSONObject(FRD.FindChild('element2'));

  AssertEquals('Failed on 1', '{ "element1" : { "element2" : {} } }', FRD.JSON.AsJSON);
  AssertSame('Found element', E2, E3);
end;

procedure TTestReportDOM.TestFind2;
var
  E1, E2, E3: TJSONObject;
begin
  AssertTrue('Failed on 1', FRD is TFPReportJSONStreamer);

  E1 := TJSONObject(FRD.NewElement('element1'));
  E2 := TJSONObject(FRD.NewElement('element2'));

  FRD.CurrentElement := E1;
  E3 := TJSONObject(FRD.FindChild('element3'));

  AssertNull('NonExisting element is null', E3);
end;

procedure TTestReportDOM.TestPush;
var
  E1, E2: TJSONObject;
begin
  AssertTrue('Failed on 1', FRD is TFPReportJSONStreamer);

  E1 := TJSONObject(FRD.NewElement('element1'));
  AssertSame('Current element equals created', E1, FRD.CurrentElement);

  E2 := TJSONObject(FRD.NewElement('element2'));
  AssertEquals('New node pushed with correct name', '{ "element2" : {} }', E1.AsJSON);
end;

procedure TTestReportDOM.TestPop1;
var
  E1, E2, E3: TJSONObject;
begin
  AssertTrue('Failed on 1', FRD is TFPReportJSONStreamer);

  E1 := TJSONObject(FRD.NewElement('element1'));
  AssertSame('Failed on 1', E1, FRD.CurrentElement);

  E2 := TJSONObject(FRD.PushElement('element2'));
  AssertSame('Failed on 2', E2, FRD.CurrentElement);
  AssertEquals('Failed on 3', '{ "element2" : {} }', E1.AsJSON);
  E3 := FRD.CurrentElement;

  AssertSame('Failed on 4', E3, TJSONObject(FRD.PopElement));
end;

procedure TTestReportDOM.TestStreamToHex;
begin
  TestStream(True);
end;

procedure TTestReportDOM.TestStreamToHex2;
begin
  TestStream(False);
end;

procedure TTestReportDOM.TestStreamEquals1;
var
  S: TMemoryStream;
begin
  S := TMemoryStream.Create;
  try
    AssertEquals('Same stream always equal', True, FRD.StreamsEqual(S, S));
  finally
    S.Free;
  end;
end;

procedure TTestReportDOM.TestStreamEquals2;
var
  S1, S2: TMemoryStream;
begin
  S1 := TMemoryStream.Create;
  try
    FillBytes(S1, 255);
    S2 := TMemoryStream.Create;
    try
      FillBytes(S2, 255);
      AssertEquals('Same content always equal', True, FRD.StreamsEqual(S1, S2));
    finally
      S2.Free;
    end;
  finally
    S1.Free;
  end;
end;

procedure TTestReportDOM.TestStreamEquals3;
var
  S1, S2: TMemoryStream;
begin
  S1 := TMemoryStream.Create;
  try
    FillBytes(S1, 255);
    S2 := TMemoryStream.Create;
    try
      FillBytes(S2, 254);
      AssertEquals('Different sizes makes not equal', False, FRD.StreamsEqual(S1, S2));
    finally
      S2.Free;
    end;
  finally
    S1.Free;
  end;
end;

procedure TTestReportDOM.TestStreamEquals4;
var
  S1, S2: TMemoryStream;
  B: byte;
begin
  S1 := TMemoryStream.Create;
  try
    FillBytes(S1, 255);
    AssertEquals(0, S1.Seek(0, soFromBeginning));
    B := 10;
    S1.WriteBuffer(B, 1);
    B := 12;
    S1.Position := 0;
    S1.ReadBuffer(B, 1);
    AssertEquals(10, B);
    AssertEquals(256, S1.Size);
    S2 := TMemoryStream.Create;
    try
      FillBytes(S2, 255);
      AssertEquals('Different streams makes not equal', False, FRD.StreamsEqual(S1, S2));
    finally
      S2.Free;
    end;
  finally
    S1.Free;
  end;
end;

procedure TTestReportDOM.TestHexToStream;
var
  S: TMemoryStream;
  SS: TStringStream;
  H: string;
begin
  S := TMemoryStream.Create;
  try
    FillBytes(S, 255);
    H := FRD.StreamToHex(S);
    SS := FRD.HexToStringStream(H);
    try
      AssertEquals('Size of stream is OK', 256, SS.Size);
      AssertEquals('HexToStringStream OK', True, FRD.StreamsEqual(S, SS));
    finally
      SS.Free;
    end;
  finally
    S.Free;
  end;
end;

procedure TTestReportDOM.TestWriteInteger1;
var
  E: TJSONObject;
begin
  AssertTrue('Failed on 1', FRD is TFPReportJSONStreamer);

  E := TJSONObject(FRD.NewElement('MyElement'));
  FRD.WriteInteger('Int', 1);
  AssertSame('Current element not changed', E, FRD.CurrentElement);
  AssertEquals('Failed on 1', '{ "MyElement" : { "Int" : 1 } }', FRD.JSON.AsJSON);
end;

procedure TTestReportDOM.TestWriteString1;
var
  E: TJSONObject;
begin
  AssertTrue('Failed on 1', FRD is TFPReportJSONStreamer);
  E := TJSONObject(FRD.NewElement('MyElement'));
  FRD.WriteString('Str', 'Aloha');
  AssertSame('Current element not changed', E, FRD.CurrentElement);
  AssertEquals('Failed on 1', '{ "MyElement" : { "Str" : "Aloha" } }', FRD.JSON.AsJSON);
end;

procedure TTestReportDOM.TestWriteString2;
var
  E: TJSONObject;
begin
  AssertTrue('Failed on 1', FRD is TFPReportJSONStreamer);
  E := TJSONObject(FRD.NewElement('MyElement'));
  FRD.WriteString('Str', '');
  AssertSame('Current element not changed', E, FRD.CurrentElement);
  AssertEquals('Failed on 1', '{ "MyElement" : { "Str" : "" } }', FRD.JSON.AsJSON);
end;

procedure TTestReportDOM.TestWriteBoolean1;
var
  E: TJSONObject;
begin
  AssertTrue('Failed on 1', FRD is TFPReportJSONStreamer);
  E := TJSONObject(FRD.NewElement('MyElement'));
  FRD.WriteBoolean('Bool', True);
  AssertSame('Current element not changed', E, FRD.CurrentElement);
  AssertEquals('Failed on 1', '{ "MyElement" : { "Bool" : true } }', FRD.JSON.AsJSON);
end;

procedure TTestReportDOM.TestWriteBoolean2;
var
  E: TJSONObject;
begin
  AssertTrue('Failed on 1', FRD is TFPReportJSONStreamer);
  E := TJSONObject(FRD.NewElement('MyElement'));
  FRD.WriteBoolean('Bool', False);
  AssertSame('Current element not changed', E, FRD.CurrentElement);
  AssertEquals('Failed on 1', '{ "MyElement" : { "Bool" : false } }', FRD.JSON.AsJSON);
end;

procedure TTestReportDOM.TestWriteFloat1;
var
  E: TJSONObject;
begin
  AssertTrue('Failed on 1', FRD is TFPReportJSONStreamer);
  E := TJSONObject(FRD.NewElement('MyElement'));
  FRD.WriteFloat('Float', 1.23);
  AssertSame('Current element not changed', E, FRD.CurrentElement);
  AssertEquals('Failed on 1', 1.23, FRD.JSON.FindPath('MyElement.Float').AsFloat);
end;

procedure TTestReportDOM.TestWriteFloat2;
var
  E: TJSONObject;
begin
  AssertTrue('Failed on 1', FRD is TFPReportJSONStreamer);
  E := TJSONObject(FRD.NewElement('MyElement'));
  FRD.WriteFloat('Float', -1.23);
  AssertSame('Current element not changed', E, FRD.CurrentElement);
  AssertEquals('Failed on 1', -1.23, FRD.JSON.FindPath('MyElement.Float').AsFloat);
end;

procedure TTestReportDOM.TestWriteFloat3;
var
  E: TJSONObject;
  x: Extended;
begin
  AssertTrue('Failed on 1', FRD is TFPReportJSONStreamer);
  E := TJSONObject(FRD.NewElement('MyElement'));
  x := 0.0;
  FRD.WriteFloat('Float', x);
  AssertSame('Current element not changed', E, FRD.CurrentElement);
  AssertEquals('Failed on 1', 0.0, FRD.JSON.FindPath('MyElement.Float').AsFloat);
end;

procedure TTestReportDOM.TestWriteDateTime1;
var
  E: TJSONObject;
  D: TDateTime;
begin
  AssertTrue('Failed on 1', FRD is TFPReportJSONStreamer);
  E := TJSONObject(FRD.NewElement('MyElement'));
  D := EncodeDate(2008, 9, 18);
  FRD.WriteDateTime('Date', D);
  AssertSame('Current element not changed', E, FRD.CurrentElement);
  AssertEquals('Failed on 1', '{ "MyElement" : { "Date" : "20080918T000000" } }', FRD.JSON.AsJSON);
end;

procedure TTestReportDOM.TestWriteDateTime2;
var
  E: TJSONObject;
  D: TDateTime;
begin
  AssertTrue('Failed on 1', FRD is TFPReportJSONStreamer);
  E := TJSONObject(FRD.NewElement('MyElement'));
  D := EncodeDate(2008, 9, 18) + EncodeTime(11,03,55,123);
  FRD.WriteDateTime('Date', D);
  AssertSame('Current element not changed', E, FRD.CurrentElement);
  AssertEquals('Failed on 1', '{ "MyElement" : { "Date" : "20080918T110355" } }', FRD.JSON.AsJSON);
end;

procedure TTestReportDOM.TestWriteDateTime3;
var
  E: TJSONObject;
  D: TDateTime;
begin
  AssertTrue('Failed on 1', FRD is TFPReportJSONStreamer);
  E := TJSONObject(FRD.NewElement('MyElement'));
  D := EncodeTime(11,03,55,123);
  FRD.WriteDateTime('Date', D);
  AssertSame('Current element not changed', E, FRD.CurrentElement);
  AssertEquals('Failed on 1', '{ "MyElement" : { "Date" : "00000000T110355" } }', FRD.JSON.AsJSON);
end;

procedure TTestReportDOM.TestWriteStream1;
var
  E: TJSONObject;
  S: TMemoryStream;
begin
  AssertTrue('Failed on 1', FRD is TFPReportJSONStreamer);
  E := TJSONObject(FRD.NewElement('MyElement'));
  S := TMemoryStream.Create;
  try
    FillBytes(S, 15);
    FRD.WriteStream('Stream', S);
    AssertSame('Current element not changed', E, FRD.CurrentElement);
    AssertEquals('Failed on 1', '{ "MyElement" : { "Stream" : "000102030405060708090A0B0C0D0E0F" } }', FRD.JSON.AsJSON);
  finally
    FreeAndNil(S);
  end;
end;

procedure TTestReportDOM.TestWriteIntegerDiff1;
var
  E: TJSONObject;
begin
  AssertTrue('Failed on 1', FRD is TFPReportJSONStreamer);
  E := TJSONObject(FRD.NewElement('MyElement'));
  FRD.WriteIntegerDiff('Int', 1, 0);
  AssertSame('Current element not changed', E, FRD.CurrentElement);
  AssertEquals('Failed on 1', '{ "MyElement" : { "Int" : 1 } }', FRD.JSON.AsJSON);
end;

procedure TTestReportDOM.TestWriteIntegerDiff2;
var
  E: TJSONObject;
begin
  AssertTrue('Failed on 1', FRD is TFPReportJSONStreamer);
  E := TJSONObject(FRD.NewElement('MyElement'));
  FRD.WriteIntegerDiff('Int', 1, 1);
  AssertSame('Current element not changed', E, FRD.CurrentElement);
  AssertEquals('Failed on 1', '{ "MyElement" : {} }', FRD.JSON.AsJSON);
end;

procedure TTestReportDOM.TestWriteStringDiff1;
var
  E: TJSONObject;
begin
  AssertTrue('Failed on 1', FRD is TFPReportJSONStreamer);
  E := TJSONObject(FRD.NewElement('MyElement'));
  FRD.WriteStringDiff('Str', 'Aloha', 'mopa');
  AssertSame('Current element not changed', E, FRD.CurrentElement);
  AssertEquals('Failed on 1', '{ "MyElement" : { "Str" : "Aloha" } }', FRD.JSON.AsJSON);
end;

procedure TTestReportDOM.TestWriteStringDiff2;
var
  E: TJSONObject;
begin
  AssertTrue('Failed on 1', FRD is TFPReportJSONStreamer);
  E := TJSONObject(FRD.NewElement('MyElement'));
  FRD.WriteStringDiff('Str', 'Aloha', 'Aloha');
  AssertSame('Current element not changed', E, FRD.CurrentElement);
  AssertEquals('Failed on 1', '{ "MyElement" : {} }', FRD.JSON.AsJSON);
end;

procedure TTestReportDOM.TestWriteBooleanDiff1;
var
  E: TJSONObject;
begin
  AssertTrue('Failed on 1', FRD is TFPReportJSONStreamer);
  E := TJSONObject(FRD.NewElement('MyElement'));
  FRD.WriteBooleanDiff('Bool', True, False);
  AssertSame('Current element not changed', E, FRD.CurrentElement);
  AssertEquals('Failed on 1', '{ "MyElement" : { "Bool" : true } }', FRD.JSON.AsJSON);
end;

procedure TTestReportDOM.TestWriteBooleanDiff2;
var
  E: TJSONObject;
begin
  AssertTrue('Failed on 1', FRD is TFPReportJSONStreamer);
  E := TJSONObject(FRD.NewElement('MyElement'));
  FRD.WriteBooleanDiff('Bool', True, True);
  AssertSame('Current element not changed', E, FRD.CurrentElement);
  AssertEquals('Failed on 1', '{ "MyElement" : {} }', FRD.JSON.AsJSON);
end;

procedure TTestReportDOM.TestWriteFloatDiff1;
var
  E: TJSONObject;
begin
  AssertTrue('Failed on 1', FRD is TFPReportJSONStreamer);
  E := TJSONObject(FRD.NewElement('MyElement'));
  FRD.WriteFloatDiff('Float', 1.23, 1.24);
  AssertSame('Current element not changed', E, FRD.CurrentElement);
  AssertEquals('Failed on 1', 1.23, FRD.JSON.FindPath('MyElement.Float').AsFloat);
end;

procedure TTestReportDOM.TestWriteFloatDiff2;
var
  E: TJSONObject;
begin
  AssertTrue('Failed on 1', FRD is TFPReportJSONStreamer);
  E := TJSONObject(FRD.NewElement('MyElement'));
  FRD.WriteFloatDiff('Float', 1.23, 1.23);
  AssertSame('Current element not changed', E, FRD.CurrentElement);
  AssertNull('Failed on 1', FRD.JSON.FindPath('MyElement.Float'));
end;

procedure TTestReportDOM.TestWriteDateTimeDiff1;
var
  E: TJSONObject;
  D1, D2: TDateTime;
begin
  AssertTrue('Failed on 1', FRD is TFPReportJSONStreamer);
  E := TJSONObject(FRD.NewElement('MyElement'));
  D1 := EncodeDate(2008, 9, 18);
  D2 := EncodeDate(2001, 10, 28);
  FRD.WriteDateTimeDiff('Date', D1, D2);
  AssertSame('Current element not changed', E, FRD.CurrentElement);
  AssertEquals('Failed on 1', '{ "MyElement" : { "Date" : "20080918T000000" } }', FRD.JSON.AsJSON);
end;

procedure TTestReportDOM.TestWriteDateTimeDiff2;
var
  E: TJSONObject;
  D1: TDateTime;
begin
  AssertTrue('Failed on 1', FRD is TFPReportJSONStreamer);
  E := TJSONObject(FRD.NewElement('MyElement'));
  D1 := EncodeDate(2008, 9, 18) + EncodeTime(0,0,0,1);
  FRD.WriteDateTimeDiff('Date', D1, D1);
  AssertSame('Current element not changed', E, FRD.CurrentElement);
  AssertEquals('Failed on 1', '{ "MyElement" : {} }', FRD.JSON.AsJSON);
end;

procedure TTestReportDOM.TestWriteDateTimeDiff3;
var
  E: TJSONObject;
  D1: TDateTime;
begin
  AssertTrue('Failed on 1', FRD is TFPReportJSONStreamer);
  E := TJSONObject(FRD.NewElement('MyElement'));
  D1 := EncodeDate(2008, 9, 18);
  FRD.WriteDateTimeDiff('Date', D1, D1 + EncodeTime(0,0,0,1));
  AssertSame('Current element not changed', E, FRD.CurrentElement);
  AssertEquals('Failed on 1', '{ "MyElement" : { "Date" : "20080918T000000" } }', FRD.JSON.AsJSON);
end;

procedure TTestReportDOM.TestWriteStreamDiff1;
var
  E: TJSONObject;
  S: TMemoryStream;
begin
  AssertTrue('Failed on 1', FRD is TFPReportJSONStreamer);
  E := TJSONObject(FRD.NewElement('MyElement'));
  S := TMemoryStream.Create;
  try
    FillBytes(S, 15);
    FRD.WriteStreamDiff('Stream', S, S);
    AssertSame('Current element not changed', E, FRD.CurrentElement);
    AssertEquals('Failed on 1', '{ "MyElement" : {} }', FRD.JSON.AsJSON);
  finally
    FreeAndNil(S);
  end;
end;

procedure TTestReportDOM.TestWriteStreamDiff2;
var
  E: TJSONObject;
  S, T: TMemoryStream;
begin
  AssertTrue('Failed on 1', FRD is TFPReportJSONStreamer);
  E := TJSONObject(FRD.NewElement('MyElement'));
  S := TMemoryStream.Create;
  try
    FillBytes(S, 15);
    T := TMemoryStream.Create;
    try
      FillBytes(T, 15);
      FRD.WriteStreamDiff('Stream', S, T);
      AssertSame('Current element not changed', E, FRD.CurrentElement);
      AssertEquals('Failed on 1', '{ "MyElement" : {} }', FRD.JSON.AsJSON);
    finally
      FreeAndNil(T);
    end;
  finally
    FreeAndNil(S);
  end;
end;

procedure TTestReportDOM.TestWriteStreamDiff3;
var
  E: TJSONObject;
  S, T: TMemoryStream;
begin
  AssertTrue('Failed on 1', FRD is TFPReportJSONStreamer);
  E := TJSONObject(FRD.NewElement('MyElement'));
  S := TMemoryStream.Create;
  try
    FillBytes(S, 15);
    T := TMemoryStream.Create;
    try
      FillBytes(T, 16);
      FRD.WriteStreamDiff('Stream', S, T);
      AssertSame('Current element not changed', E, FRD.CurrentElement);
      AssertEquals('Failed on 1', '{ "MyElement" : { "Stream" : "000102030405060708090A0B0C0D0E0F" } }', FRD.JSON.AsJSON)
    finally
      FreeAndNil(T);
    end;
  finally
    FreeAndNil(S);
  end;
end;

procedure TTestReportDOM.TestReadInteger1;
var
  E: TJSONObject;
begin
  AssertTrue('Failed on 1', FRD is TFPReportJSONStreamer);
  E := TJSONObject(FRD.NewElement('MyElement'));
  FRD.WriteInteger('Int', 1);
  AssertSame('Current element not changed', E, FRD.CurrentElement);
  AssertEquals('Reading integer property', 1, FRD.ReadInteger('Int', -1));
  AssertSame('Current element not changed', E, FRD.CurrentElement);
end;

procedure TTestReportDOM.TestReadInteger2;
var
  E: TJSONObject;
begin
  AssertTrue('Failed on 1', FRD is TFPReportJSONStreamer);
  E := TJSONObject(FRD.NewElement('MyElement'));
  AssertEquals('Reading non-existent integer property', -1, FRD.ReadInteger('Int', -1));
  AssertEquals('Reading non-existent integer property', -2, FRD.ReadInteger('Int', -2));
end;

procedure TTestReportDOM.TestReadInteger3;
var
  E: TJSONObject;
begin
  AssertTrue('Failed on 1', FRD is TFPReportJSONStreamer);
  E := TJSONObject(FRD.NewElement('MyElement'));
  FRD.WriteString('Int', 'Aloha');
  AssertSame('Current element not changed', E, FRD.CurrentElement);
  AssertEquals('Reading wrongly typed integer property', -1, FRD.ReadInteger('Int', -1));
  AssertSame('Current element not changed', E, FRD.CurrentElement);
end;

procedure TTestReportDOM.TestReadString1;
var
  E: TJSONObject;
begin
  AssertTrue('Failed on 1', FRD is TFPReportJSONStreamer);
  E := TJSONObject(FRD.NewElement('MyElement'));
  FRD.WriteString('Str', 'Aloha');
  AssertSame('Current element not changed', E, FRD.CurrentElement);
  AssertEquals('Reading string property', 'Aloha', FRD.ReadString('Str', '(none)'));
  AssertSame('Current element not changed', E, FRD.CurrentElement);
end;

procedure TTestReportDOM.TestReadString2;
var
  E: TJSONObject;
begin
  AssertTrue('Failed on 1', FRD is TFPReportJSONStreamer);
  E := TJSONObject(FRD.NewElement('MyElement'));

  AssertSame('Current element not changed', E, FRD.CurrentElement);
  AssertEquals('Reading non-existent string property', '(none)', FRD.ReadString('Str', '(none)'));
  AssertSame('Current element not changed', E, FRD.CurrentElement);
end;

procedure TTestReportDOM.TestReadString3;
var
  E: TJSONObject;
begin
  AssertTrue('Failed on 1', FRD is TFPReportJSONStreamer);
  E := TJSONObject(FRD.NewElement('MyElement'));
  FRD.WriteInteger('Str', 1);
  AssertSame('Current element not changed', E, FRD.CurrentElement);
  AssertEquals('Reading wrongly typed string property', '(none)', FRD.ReadString('Str', '(none)'));
  AssertSame('Current element not changed', E, FRD.CurrentElement);
end;

procedure TTestReportDOM.TestReadDateTime1;
var
  E: TJSONObject;
  D: TDateTime;
begin
  AssertTrue('Failed on 1', FRD is TFPReportJSONStreamer);
  E := TJSONObject(FRD.NewElement('MyElement'));
  D := EncodeDate(2008, 9, 18);
  FRD.WriteDateTime('Date', D);
  AssertSame('Current element not changed', E, FRD.CurrentElement);
  AssertEquals('Reading date property', D, FRD.ReadDateTime('Date', D-1));
end;

procedure TTestReportDOM.TestReadDateTime2;
var
  E: TJSONObject;
  D: TDateTime;
begin
  AssertTrue('Failed on 1', FRD is TFPReportJSONStreamer);
  E := TJSONObject(FRD.NewElement('MyElement'));
  D := EncodeDate(2008, 9, 18) + EncodeTime(11, 3, 55, 123);
  FRD.WriteDateTime('Date', D);
  AssertSame('Current element not changed', E, FRD.CurrentElement);
  AssertEquals('Reading date property', D, FRD.ReadDateTime('Date', D-1));
end;

procedure TTestReportDOM.TestReadDateTime3;
var
  E: TJSONObject;
  D: TDateTime;
begin
  AssertTrue('Failed on 1', FRD is TFPReportJSONStreamer);
  E := TJSONObject(FRD.NewElement('MyElement'));
  D := EncodeTime(11, 3, 55, 123);

  AssertSame('Current element not changed', E, FRD.CurrentElement);
  AssertEquals('Reading non-existent time property', D, FRD.ReadDateTime('Date', D));
end;

procedure TTestReportDOM.TestReadDateTime4;
var
  E: TJSONObject;
  D: TDateTime;
begin
  AssertTrue('Failed on 1', FRD is TFPReportJSONStreamer);
  E := TJSONObject(FRD.NewElement('MyElement'));
  D := EncodeDate(2008, 9, 18);
  FRD.WriteString('Date', '20080918');
  AssertSame('Current element not changed', E, FRD.CurrentElement);
  AssertEquals('Reading date-only property', D, FRD.ReadDateTime('Date', D));
end;

procedure TTestReportDOM.TestReadDateTime5;
var
  E: TJSONObject;
  D: TDateTime;
begin
  AssertTrue('Failed on 1', FRD is TFPReportJSONStreamer);
  E := TJSONObject(FRD.NewElement('MyElement'));
  D := EncodeDate(2008, 9, 18);
  FRD.WriteDateTime('Date', D);
  AssertSame('Current element not changed', E, FRD.CurrentElement);
  AssertEquals('Reading date property', D, FRD.ReadDateTime('Date', D-1));
end;

procedure TTestReportDOM.TestReadBoolean1;
var
  E: TJSONObject;
begin
  AssertTrue('Failed on 1', FRD is TFPReportJSONStreamer);
  E := TJSONObject(FRD.NewElement('MyElement'));
  FRD.WriteBoolean('Bool', True);
  AssertSame('Current element not changed', E, FRD.CurrentElement);
  AssertEquals('Reading true boolean property', True, FRD.ReadBoolean('Bool', False));
  AssertSame('Current element not changed', E, FRD.CurrentElement);
end;

procedure TTestReportDOM.TestReadBoolean2;
var
  E: TJSONObject;
begin
  AssertTrue('Failed on 1', FRD is TFPReportJSONStreamer);
  E := TJSONObject(FRD.NewElement('MyElement'));
  FRD.WriteBoolean('Bool', False);
  AssertSame('Current element not changed', E, FRD.CurrentElement);
  AssertEquals('Reading true boolean property', False, FRD.ReadBoolean('Bool', True));
  AssertSame('Current element not changed', E, FRD.CurrentElement);
end;

procedure TTestReportDOM.TestReadBoolean3;
var
  E: TJSONObject;
begin
  AssertTrue('Failed on 1', FRD is TFPReportJSONStreamer);
  E := TJSONObject(FRD.NewElement('MyElement'));
  FRD.WriteString('Bool', 'Aloha');
  AssertSame('Current element not changed', E, FRD.CurrentElement);
  AssertEquals('Reading wrongly typed boolean property', False, FRD.ReadBoolean('Bool', False));
  AssertSame('Current element not changed', E, FRD.CurrentElement);
end;

procedure TTestReportDOM.TestReadBoolean4;
var
  E: TJSONObject;
begin
  AssertTrue('Failed on 1', FRD is TFPReportJSONStreamer);
  E := TJSONObject(FRD.NewElement('MyElement'));

  AssertSame('Current element not changed', E, FRD.CurrentElement);
  AssertEquals('Reading non-existant boolean property', False, FRD.ReadBoolean('Bool', False));
  AssertSame('Current element not changed', E, FRD.CurrentElement);
end;

procedure TTestReportDOM.TestReadFloat1;
var
  E: TJSONObject;
begin
  AssertTrue('Failed on 1', FRD is TFPReportJSONStreamer);
  E := TJSONObject(FRD.NewElement('MyElement'));
  FRD.WriteFloat('Float', 1.23);
  AssertSame('Current element not changed', E, FRD.CurrentElement);
  AssertEquals('Reading float property', 1.23, FRD.ReadFloat('Float', 2.34), 0.001);
  AssertSame('Current element not changed', E, FRD.CurrentElement);
end;

procedure TTestReportDOM.TestReadFloat2;
var
  E: TJSONObject;
begin
  AssertTrue('Failed on 1', FRD is TFPReportJSONStreamer);
  E := TJSONObject(FRD.NewElement('MyElement'));
  FRD.WriteString('Float', 'Aloha');
  AssertSame('Current element not changed', E, FRD.CurrentElement);
  AssertEquals('Reading wrongly typed float property', 2.34, FRD.ReadFloat('Float', 2.34), 0.001);
  AssertSame('Current element not changed', E, FRD.CurrentElement);
end;

procedure TTestReportDOM.TestReadFloat3;
var
  E: TJSONObject;
begin
  AssertTrue('Failed on 1', FRD is TFPReportJSONStreamer);
  E := TJSONObject(FRD.NewElement('MyElement'));

  AssertSame('Current element not changed', E, FRD.CurrentElement);
  AssertEquals('Reading non existant float property', 2.34, FRD.ReadFloat('Float', 2.34), 0.001);
  AssertSame('Current element not changed', E, FRD.CurrentElement);
end;

procedure TTestReportDOM.TestReadFloat4;
var
  E: TJSONObject;
begin
  AssertTrue('Failed on 1', FRD is TFPReportJSONStreamer);
  E := TJSONObject(FRD.NewElement('MyElement'));
  FRD.WriteInteger('Float', 1);
  AssertSame('Current element not changed', E, FRD.CurrentElement);
  AssertEquals('Reading integer formatted float property', 1.0, FRD.ReadFloat('Float', 2.34), 0.001);
  AssertSame('Current element not changed', E, FRD.CurrentElement);
end;

procedure TTestReportDOM.TestReadStream1;
var
  E: TJSONObject;
  S, T: TMemoryStream;
begin
  AssertTrue('Failed on 1', FRD is TFPReportJSONStreamer);
  E := TJSONObject(FRD.NewElement('MyElement'));
  S := TMemoryStream.Create;
  try
    FillBytes(S, 15);
    FRD.WriteStream('Stream', S);
    AssertSame('Current element not changed', E, FRD.CurrentElement);
    T := TMemoryStream.Create;
    try
      AssertEquals('Reading stream data', True, FRD.ReadStream('Stream', T));
      AssertEquals('Read stream equals written stream', True, FRD.StreamsEqual(S, T));
    finally
      T.Free;
    end;
  finally
    FreeAndNil(S);
  end;
end;

procedure TTestReportDOM.TestReadStream2;
var
  E: TJSONObject;
  T: TMemoryStream;
begin
  AssertTrue('Failed on 1', FRD is TFPReportJSONStreamer);
  E := TJSONObject(FRD.NewElement('MyElement'));

  FRD.WriteString('Stream', '');
  AssertSame('Current element not changed', E, FRD.CurrentElement);
  T := TMemoryStream.Create;
  try
    AssertEquals('Reading empty stream data', False, FRD.ReadStream('Stream', T));
    AssertEquals('Read stream is empty', 0, T.Size);
  finally
    T.Free;
  end;
end;

procedure TTestReportDOM.TestReadStream3;
var
  E: TJSONObject;
  T: TMemoryStream;
begin
  AssertTrue('Failed on 1', FRD is TFPReportJSONStreamer);
  E := TJSONObject(FRD.NewElement('MyElement'));
  AssertSame('Current element not changed', E, FRD.CurrentElement);
  T := TMemoryStream.Create;
  try
    AssertEquals('Reading non-existent stream data', False, FRD.ReadStream('Stream', T));
    AssertEquals('Read stream is empty', 0, T.Size);
  finally
    T.Free;
  end;
end;

procedure TTestReportDOM.TestALL;
var
  E: TJSONObject;
  rp: TFPReportPage;
begin
  AssertTrue('Failed on 1', FRD is TFPReportJSONStreamer);
  E := TJSONObject(FRD.NewElement('MyElement'));

  rp := TFPReportPage.Create(nil);
  try
    rp.WriteElement(FRD);
    {$ifdef verbosedebug}
    writeln('--------------');
    Writeln(FRD.JSON.AsJSON);
    writeln('--------------');
    {$endif}
    With FRD.JSON do
      begin
      AssertEquals('Failed on 1', '', FindPath('MyElement.Name').Asstring);
      AssertEquals('Failed on 2,',0.0, FindPath('MyElement.Layout.Top').AsFloat);
      AssertEquals('Failed on 3,',0.0, FindPath('MyElement.Layout.Left').AsFloat);
      AssertEquals('Failed on 4,',0.0, FindPath('MyElement.Layout.Height').AsFloat);
      AssertEquals('Failed on 5,',0.0, FindPath('MyElement.Layout.Width').AsFloat);
      end;
  finally
    rp.Free;
  end;
end;



{ ---------------------------------------------------------------------
  General routines
  ---------------------------------------------------------------------}

procedure TReportStreamTester.SetUp;
begin
  FRD := TFPReportJSONStreamer.Create(nil);
end;

procedure TReportStreamTester.TearDown;
begin
{$ifdef writejson}
  writeln(FRD.JSON.FormatJSON);
{$endif}
  FreeAndNil(FRD);
end;

procedure TReportStreamTester.FillBytes(S : TStream; AMax : Byte);
Var
  B : Byte;
begin
  For B:=0 to AMax do
    S.WriteBuffer(B,SizeOf(B));
end;


{ ---------------------------------------------------------------------
  Actual test routines
  ---------------------------------------------------------------------}



{ TTestReportFrameDom }

procedure TTestReportFrameDom.Setup;
begin
  inherited Setup;
  FF:=TFPReportframe.Create(Nil);
  F2:=TFPReportframe.Create(Nil);
end;

procedure TTestReportFrameDom.TearDown;
begin
  FreeAndNil(FF);
  FreeAndNil(F2);
  inherited TearDown;
end;

procedure TTestReportFrameDom.TestWrite;
var
  FDoc: TJSONObject;
begin
  FillFF;
  AssertTrue('Failed on 0', FRD is TFPReportJSONStreamer);
  FDoc := FRD.JSON;

  FF.WriteElement(FRD, nil);
  // compare via JSON directly
  AssertEquals('Failed on 1', 2, FDoc.Get('Width', 1));
  AssertEquals('Failed on 2', 'psDot', FDoc.Get('Pen', 'psSolid'));
  AssertEquals('Failed on 3', 'fsRoundedRect', FDoc.Get('Shape', 'fsNone'));
  AssertEquals('Failed on 4', 23, FDoc.Get('Color', 0));
  AssertEquals('Failed on 5', Integer([flTop,flBottom]), FDoc.Get('Lines', 0));
  // compare via streamer interface
  AssertEquals('Failed on 6', 2, FRD.ReadInteger('Width', 1));
  AssertEquals('Failed on 7', 'psDot', FRD.ReadString('Pen', 'psSolid'));
  AssertEquals('Failed on 8', 'fsRoundedRect', FRD.ReadString('Shape', 'fsNone'));
  AssertEquals('Failed on 9', 23, FRD.ReadInteger('Color', 0));
  AssertEquals('Failed on 10', Integer([flTop,flBottom]), FRD.ReadInteger('Lines', 0));
end;

procedure TTestReportFrameDom.TestWriteDiff;
var
  FDoc: TJSONObject;
begin
  FillFF;
  AssertTrue('Failed on 0', FRD is TFPReportJSONStreamer);
  FDoc := FRD.JSON;
  FF.WriteElement(FRD, F2);
  AssertEquals('Failed on 1', 2, FDoc.Get('Width', 0));
  AssertEquals('Failed on 2', 'psDot', FDoc.Get('Pen', 'psSolid'));
  AssertEquals('Failed on 3', 'fsRoundedRect', FDoc.Get('Shape', 'fsNone'));
  AssertEquals('Failed on 4', 23, FDoc.Get('Color', 0));
  AssertEquals('Failed on 5', Integer([flTop,flBottom]), FDoc.Get('Lines', 0));
end;

procedure TTestReportFrameDom.TestRead;
var
  FDoc: TJSONObject;
begin
  FillFF;
  AssertTrue('Failed on 0', FRD is TFPReportJSONStreamer);
  FDoc := FRD.JSON;
  FF.WriteElement(FRD, nil);
  F2.ReadElement(FRD);

  AssertEquals('Failed on 1', FF.Width, F2.Width);
  AssertEquals('Failed on 2', Ord(FF.Pen), Ord(F2.Pen));
  AssertEquals('Failed on 3', Ord(FF.Color), Ord(F2.Color));
  AssertEquals('Failed on 4', Ord(FF.Shape), Ord(F2.Shape));
  AssertEquals('Failed on 5', Integer(FF.Lines), Integer(F2.Lines));
end;

Procedure FillFrame(FF : TFPReportFrame);
begin
  FF.Width:=2;
  FF.Pen:=psDot;
  FF.Shape:=fsRoundedRect;
  FF.Color:=23;
  FF.Lines:=[flTop,flBottom];
end;

procedure TTestReportFrameDom.FillFF;
begin
  FillFrame(FF);
end;

{ TTestReportLayoutDom }

Procedure FillLayout(FL : TFPReportLayout);
begin
  FL.Top:=1.2;
  FL.Left:=3.4;
  FL.Width:=5.6;
  FL.Height:=7.8;
end;

procedure TTestReportLayoutDom.FillFL;
begin
  FillLayout(FL);
end;

procedure TTestReportLayoutDom.Setup;
begin
  inherited Setup;
  FL:=TFPReportLayout.Create(Nil);
  F2:=TFPReportLayout.Create(Nil);
end;

procedure TTestReportLayoutDom.TearDown;
begin
  FreeAndNil(FL);
  FreeAndNil(F2);
  inherited TearDown;
end;

procedure TTestReportLayoutDom.TestWrite;
var
  FDoc: TJSONObject;
begin
  FillFL;
  AssertTrue('Failed on 0', FRD is TFPReportJSONStreamer);
  FDoc := FRD.JSON;
  FL.WriteElement(FRD, nil);
  // compare json directly
  AssertEquals('Failed on 1', 1.2, FDoc.Get('Top', 0.0));
  AssertEquals('Failed on 2', 3.4, FDoc.Get('Left', 0.0));
  AssertEquals('Failed on 3', 5.6, FDoc.Get('Width', 0.0));
  AssertEquals('Failed en 4', 7.8, FDoc.Get('Height', 0.0));
  // compare via streamer interface
  AssertEquals('Failed on 5', 1.2, FRD.ReadFloat('Top', 0.0));
  AssertEquals('Failed on 6', 3.4, FRD.ReadFloat('Left', 0.0));
  AssertEquals('Failed on 7', 5.6, FRD.ReadFloat('Width', 0.0));
  AssertEquals('Failed en 8', 7.8, FRD.ReadFloat('Height', 0.0));
end;

procedure TTestReportLayoutDom.TestWriteDiff;
var
  FDoc: TJSONObject;
begin
  FillFL;
  AssertTrue('Failed on 0', FRD is TFPReportJSONStreamer);
  FDoc := FRD.JSON;
  FL.WriteElement(FRD, F2);
  AssertEquals('Failed on 1', 1.2, FDoc.Get('Top', 0.0));
  AssertEquals('Failed on 2', 3.4, FDoc.Get('Left', 0.0));
  AssertEquals('Failed on 3', 5.6, FDoc.Get('Width', 0.0));
  AssertEquals('Failed en 4', 7.8, FDoc.Get('Height', 0.0));
end;

procedure TTestReportLayoutDom.TestRead;
var
  FDoc: TJSONObject;
begin
  FillFL;
  AssertTrue('Failed on 0', FRD is TFPReportJSONStreamer);
  FDoc := FRD.JSON;
  FL.WriteElement(FRD, nil);
  F2.ReadElement(FRD);
  AssertEquals('Failed on 1', FL.Top, F2.Top);
  AssertEquals('Failed on 2', FL.Left, F2.Left);
  AssertEquals('Failed on 3', FL.Width, F2.Width);
  AssertEquals('Failed on 4', FL.Height, F2.Height);
end;

{ TTestReportElementDOM }

procedure TTestReportElementDOM.FillFE;
begin
  FillLayout(FE.Layout);
  FillFrame(FE.Frame);
end;

procedure TTestReportElementDOM.Setup;
begin
  inherited Setup;
  FE:=TFPReportElement.Create(Nil);
  F2:=TFPReportElement.Create(Nil);
//  FRD.JSON.Add('element');
end;

procedure TTestReportElementDOM.TearDown;
begin
  FreeAndNil(F2);
  FreeAndNil(FE);
  inherited TearDown;
end;

procedure TTestReportElementDOM.TestWrite1;
var
  E: TJSONObject;
begin
  FillFE;
  AssertTrue('Failed on 0', FRD is TFPReportJSONStreamer);
  FE.WriteElement(FRD, nil);

  E := TJSONObject(FRD.FindChild('Layout'));
  AssertNotNull('Failed on 1', E);
  FRD.PushElement(E);
  try
    AssertEquals('Failed on 2', 1.2, FRD.ReadFloat('Top', 0.0));
    AssertEquals('Failed on 3', 3.4, FRD.ReadFloat('Left', 0.0));
    AssertEquals('Failed on 4', 5.6, FRD.ReadFloat('Width', 0.0));
    AssertEquals('Failed en 5', 7.8, FRD.ReadFloat('Height', 0.0));
  finally
    FRD.PopElement;
  end;

  E := TJSONObject(FRD.FindChild('Frame'));
  AssertNotNull('Failed on 6', E);
  FRD.PushElement(E);
  try
    AssertEquals('Failed on 7', 2, FRD.ReadInteger('Width', 1));
    AssertEquals('Failed on 8', 'psDot', FRD.ReadString('Pen', 'psSolid'));
    AssertEquals('Failed on 9', 'fsRoundedRect', FRD.ReadString('Shape', 'fsNone'));
    AssertEquals('Failed on 10', 23, FRD.ReadInteger('Color', 0));
    AssertEquals('Failed on 11', Integer([flTop,flBottom]), FRD.ReadInteger('Lines', 0));
  finally
    FRD.PopElement;
  end;
end;

procedure TTestReportElementDOM.TestWriteDiff1;
var
  E: TJSONObject;
begin
  FillFE;
  AssertTrue('Failed on 0.1', FRD is TFPReportJSONStreamer);
  E := FRD.CurrentElement;
  FE.WriteElement(FRD, F2);
  AssertSame('Failed on 0.2', E, FRD.CurrentElement);

  E := TJSONObject(FRD.FindChild('Layout'));
  AssertNotNull('Failed on 1', E);
  FRD.PushElement(E);
  try
    AssertEquals('Failed on 2', 1.2, FRD.ReadFloat('Top', 0.0));
    AssertEquals('Failed on 3', 3.4, FRD.ReadFloat('Left', 0.0));
    AssertEquals('Failed on 4', 5.6, FRD.ReadFloat('Width', 0.0));
    AssertEquals('Failed en 5', 7.8, FRD.ReadFloat('Height', 0.0));
  finally
    FRD.PopElement;
  end;

  E := TJSONObject(FRD.FindChild('Frame'));
  AssertNotNull('Failed on 6', E);
  FRD.PushElement(E);
  try
    AssertEquals('Failed on 7', 2, FRD.ReadInteger('Width', 1));
    AssertEquals('Failed on 8', 'psDot', FRD.ReadString('Pen', 'psSolid'));
    AssertEquals('Failed on 9', 'fsRoundedRect', FRD.ReadString('Shape', 'fsNone'));
    AssertEquals('Failed on 10', 23, FRD.ReadInteger('Color', 0));
    AssertEquals('Failed on 11', Integer([flTop,flBottom]), FRD.ReadInteger('Lines', 0));
  finally
    FRD.PopElement;
  end;
end;

procedure TTestReportElementDOM.TestWriteDiff2;
var
  E: TJSONObject;
begin
  FillFE;
  AssertTrue('Failed on 0', FRD is TFPReportJSONStreamer);
  E := FRD.CurrentElement;
  F2.Frame.Assign(FE.Frame);
  FE.WriteElement(FRD, F2);
  AssertSame('Failed on 1', E, FRD.CurrentElement);

  E := TJSONObject(FRD.FindChild('Layout'));
  AssertNotNull('Failed on 2', E); // Layout was saved

  E := TJSONObject(FRD.FindChild('Frame'));
  AssertNull('Failed on 3', E);  // Frame was not saved
end;

procedure TTestReportElementDOM.TestRead1;
begin
  FillFE;
  AssertTrue('Failed on 0', FRD is TFPReportJSONStreamer);
  FE.WriteElement(FRD, nil);
  F2.ReadElement(FRD);

  AssertEquals('Failed on 1', True, FE.Layout.Equals(F2.Layout));
  AssertEquals('Failed on 2', True, FE.Frame.Equals(F2.Frame));
  AssertEquals('Failed on 3', True, FE.Equals(F2));

  F2.Visible := False;
  AssertEquals('Failed on 4', False, FE.Equals(F2));
end;

procedure TTestReportElementDOM.TestRead2;
begin
  FillFE;
  AssertTrue('Failed on 0', FRD is TFPReportJSONStreamer);

  F2.Frame.Assign(FE.Frame);

  // Only layout is written
  FE.WriteElement(FRD, F2);
  FreeAndNil(F2);
  F2 := TFPReportElement.Create(Nil);
  F2.ReadElement(FRD);

  AssertEquals('Failed on 1', True, FE.Layout.Equals(F2.Layout));
  AssertEquals('Failed on 2', False, FE.Frame.Equals(F2.Frame));
  AssertEquals('Failed on 3', False, FE.Equals(F2));
end;


{ TTestReportElementWithChildrenDOM }

procedure TTestReportElementWithChildrenDOM.FillFE;
var
  E: TFPReportElement;
begin
  FillLayout(FE.Layout);
  FillFrame(FE.Frame);

  // child 1
  E := TFPReportMemo.Create(FE);
  E.Name := 'Memo1';
  E.Visible := True;
  E.Layout.Left := 1;

  // child 2
  E := TFPReportMemo.Create(FE);
  E.Name := 'Memo2';
  E.Visible := False;
  E.Layout.Left := 2;
end;

procedure TTestReportElementWithChildrenDOM.Setup;
begin
  inherited Setup;
  FE := TFPReportElementWithChildren.Create(Nil);
  FE.Name := 'Component1';
  F2 := TFPReportElementWithChildren.Create(Nil);
  F2.Name := 'Component2';
end;

procedure TTestReportElementWithChildrenDOM.TearDown;
begin
  FreeAndNil(F2);
  FreeAndNil(FE);
  inherited TearDown;
end;

procedure TTestReportElementWithChildrenDOM.TestWrite;
var
  E: TJSONObject;
begin
  FillFE;
  AssertTrue('Failed on 0', FRD is TFPReportJSONStreamer);
  FE.WriteElement(FRD, nil);

  E := TJSONObject(FRD.FindChild('Layout'));
  AssertNotNull('Failed on 1', E);
  FRD.PushElement(E);
  try
    AssertEquals('Failed on 2', 1.2, FRD.ReadFloat('Top', 0.0));
    AssertEquals('Failed on 3', 3.4, FRD.ReadFloat('Left', 0.0));
    AssertEquals('Failed on 4', 5.6, FRD.ReadFloat('Width', 0.0));
    AssertEquals('Failed en 5', 7.8, FRD.ReadFloat('Height', 0.0));
  finally
    FRD.PopElement;
  end;

  E := TJSONObject(FRD.FindChild('Children'));
  AssertNotNull('Failed on 6', E);
  FRD.PushElement(E);
  try
    AssertEquals('Failed on 7', 2, FRD.ChildCount);
    // child 1
    E := TJSONObject(FRD.GetChild(0));
    FRD.PushElement(E);
    try
      AssertEquals('Failed on 8', True, FRD.ReadBoolean('Visible', False));
    finally
      FRD.PopElement;
    end;
    // child 2
    E := TJSONObject(FRD.GetChild(1));
    FRD.PushElement(E);
    try
      AssertEquals('Failed on 9', False, FRD.ReadBoolean('Visible', True));
    finally
      FRD.PopElement;
    end;
  finally
    FRD.PopElement;
  end;
end;

procedure TTestReportElementWithChildrenDOM.TestRead;
begin
  FillFE;
  AssertTrue('Failed on 0', FRD is TFPReportJSONStreamer);
  FE.WriteElement(FRD, nil);
  F2.ReadElement(FRD);

  AssertEquals('Failed on 1', True, FE.Layout.Equals(F2.Layout));
  AssertEquals('Failed on 2', True, FE.Frame.Equals(F2.Frame));
  AssertEquals('Failed on 3', True, FE.Equals(F2));

  F2.Visible := False;
  AssertEquals('Failed on 4', False, FE.Equals(F2));
end;

{ TTestReportPageHeader }

procedure TTestReportPageHeader.FillFE;
var
  E: TFPReportMemo;
begin
  FillLayout(FE.Layout);
  FillFrame(FE.Frame);

  // child 1
  E := TFPReportMemo.Create(FE);
  E.Name := 'Memo1';
  E.Visible := True;
  E.Layout.Left := 1;

  // child 2
  E := TFPReportMemo.Create(FE);
  E.Name := 'Memo2';
  E.Visible := False;
  E.Layout.Left := 2;
  E.TextAlignment.Horizontal := taCentered;
  E.TextAlignment.Vertical := tlCenter;
end;

procedure TTestReportPageHeader.Setup;
begin
  inherited Setup;
  FE := TFPReportPageHeaderBand.Create(Nil);
  FE.Name := 'Component1';
  F2 := TFPReportPageHeaderBand.Create(Nil);
  F2.Name := 'Component2';
end;

procedure TTestReportPageHeader.TearDown;
begin
  FreeAndNil(F2);
  FreeAndNil(FE);
  inherited TearDown;
end;

procedure TTestReportPageHeader.TestWrite;
var
  E: TJSONObject;
begin
  FillFE;
  AssertTrue('Failed on 0', FRD is TFPReportJSONStreamer);
  FE.WriteElement(FRD, nil);

  E := TJSONObject(FRD.FindChild('Layout'));
  AssertNotNull('Failed on 1', E);
  FRD.PushElement(E);
  try
    AssertEquals('Failed on 2', 1.2, FRD.ReadFloat('Top', 0.0));
    AssertEquals('Failed on 3', 3.4, FRD.ReadFloat('Left', 0.0));
    AssertEquals('Failed on 4', 5.6, FRD.ReadFloat('Width', 0.0));
    AssertEquals('Failed en 5', 7.8, FRD.ReadFloat('Height', 0.0));
  finally
    FRD.PopElement;
  end;

  E := TJSONObject(FRD.FindChild('Children'));
  AssertNotNull('Failed on 6', E);
  FRD.PushElement(E);
  try
    AssertEquals('Failed on 7', 2, FRD.ChildCount);
    // child 1
    E := TJSONObject(FRD.GetChild(0));
    FRD.PushElement(E);
    try
      AssertEquals('Failed on 8', True, FRD.ReadBoolean('Visible', False));
    finally
      FRD.PopElement;
    end;
    // child 2
    E := TJSONObject(FRD.GetChild(1));
    FRD.PushElement(E);
    try
      AssertEquals('Failed on 9', False, FRD.ReadBoolean('Visible', True));
    finally
      FRD.PopElement;
    end;
  finally
    FRD.PopElement;
  end;

  E := TJSONObject(FRD.FindChild('VisibleOnPage'));
  AssertNotNull('Failed on 10', E);
  AssertEquals('Failed on 11', 'vpAll', E.Value);
end;

procedure TTestReportPageHeader.TestWrite2;
var
  E: TJSONObject;
begin
  FillFE;
  FE.VisibleOnPage := vpNotOnFirst;
  AssertTrue('Failed on 1', FRD is TFPReportJSONStreamer);
  FE.WriteElement(FRD, nil);

  E := TJSONObject(FRD.FindChild('VisibleOnPage'));
  AssertNotNull('Failed on 2', E);
  AssertEquals('Failed on 3', 'vpNotOnFirst', E.Value);
end;

procedure TTestReportPageHeader.TestRead;
begin
  FillFE;
  FE.VisibleOnPage := vpNotOnFirst; // a non-default value
  AssertTrue('Failed on 1', FRD is TFPReportJSONStreamer);
  FE.WriteElement(FRD, nil);
  F2.ReadElement(FRD);

  AssertEquals('Failed on 2', True, FE.Layout.Equals(F2.Layout));
  AssertEquals('Failed on 3', True, FE.Frame.Equals(F2.Frame));
  AssertEquals('Failed on 4', True, FE.Equals(F2));

  F2.Visible := False;
  AssertEquals('Failed on 5', False, FE.Equals(F2));
end;


initialization
  RegisterTests(
      [
        TTestReportDOM,
        TTestReportFrameDom,
        TTestReportLayoutDom,
        TTestReportElementDOM,
        TTestReportElementWithChildrenDOM,
        TTestReportPageHeader
      ]);
end.

