(*
 * Test program for pascal HPack for http2
 *
 * This test code uses sample headers from https://github.com/http2jp/hpack-test-case
 * to test decoding of available samples and then reencode and decode again
 * using plain only, indexing only, huffman only, and both at same time.
 *
 * The JSON parsing adds around a 15% speed penalty.
 *
 *)

unit uhpacktest1;

{$mode objfpc}{$H+}

{$DEFINE QUIET}
{$DEFINE FULL_QUIET}

{$IFDEF FULL_QUIET}
  {$DEFINE QUIET}
{$ENDIF}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, uhpack, fpjson, jsonparser, jsonscanner;

type

  { THPackTestCaseCycle }

  THPackTestCaseCycle= class(TTestCase)
  private
    HPDecoder: THPackDecoder;
    HPIntfDecoderPlain: THPackDecoder;
    HPIntfDecoderPlainIndexed: THPackDecoder;
    HPIntfDecoderHuffman: THPackDecoder;
    HPIntfDecoderHuffmanIndexed: THPackDecoder;
    HPIntfEncoderPlain: THPackEncoder;
    HPIntfEncoderPlainIndexed: THPackEncoder;
    HPIntfEncoderHuffman: THPackEncoder;
    HPIntfEncoderHuffmanIndexed: THPackEncoder;
    SequenceCounter: integer;
    StoryCounter: integer;
    GroupsCounter: integer;
    WireBytes: integer;
    DecodedBytes: integer;
    procedure TestThisSequence(const aGroup: integer; const aStory: integer; const aJSon: TJSONData);
    procedure TestCaseStory(const aGroup: integer; const aStory: integer; const aJSon: TJSONData);
    procedure RunSampleHeadersTest;
  protected
    function  GetTestName: string; override;
  published
    procedure TestHookUp;
  end;

  { THPackTestDecoder }

  THPackTestDecoder= class(TTestCase)
  private
    HPDecoder: THPackDecoder;
    DummyDecoder: THPackDecoder;
    DummyEncoder: THPackEncoder;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure VerifyIncompleteIndexRead;
    procedure InvalidTableIndexZero;
    procedure IndexShiftOverflow;
    procedure DynamicTableSizeUpdate;
    procedure DynamicTableSizeUpdateRequired;
    procedure IllegalDynamicTableSizeUpdate;
    procedure MaxDynamicTableSizeSignOverflow;
    procedure ReduceMaxDynamicTableSize;
    procedure TooLargeDynamicTableSizeUpdate;
    procedure MissingDynamicTableSizeUpdate;
    procedure LiteralWithIncrementalIndexingWithEmptyName;
    procedure LiteralWithIncrementalIndexingCompleteEviction;
    procedure LiteralWithIncrementalIndexingWithLargeName;
    procedure LiteralWithIncrementalIndexingWithLargeValue;
    procedure LiteralWithoutIndexingWithEmptyName;
    procedure LiteralWithoutIndexingWithLargeName;
    procedure LiteralWithoutIndexingWithLargeValue;
    procedure LiteralNeverIndexedWithEmptyName;
    procedure LiteralNeverIndexedWithLargeName;
    procedure LiteralNeverIndexedWithLargeValue;
  end;

implementation

function HexToBinString(aHex: RawByteString): RawByteString;
var
  j: integer;
  t: integer;
begin
  t:=0;
  for j := 1 to Length(aHex) do begin
    if (aHex[j] in ['a'..'f','A'..'F','0'..'9']) then begin
      inc(t);
      if t<>j then begin
        aHex[t]:=aHex[j];
      end;
    end else begin
      if (aHex[j]<>#32) and (aHex[j]<>'-') then begin
        Raise Exception.Create('Internal: Invalid hex format character');
      end;
    end;
  end;
  if t<>j then SetLength(aHex,t);
  if t mod 2 <>0 then begin
    Raise Exception.Create('Internal: Invalid hex chars count (odd)');
  end;
  SetLength(Result,Length(aHex) div 2);
  HexToBin(@aHex[1],@Result[1],Length(Result));
end;

function BinStringToHex(const aBinString: string): string;
begin
  Result:='';
  SetLength(Result,Length(aBinString)*2);
  BinToHex(@aBinString[1],@Result[1],Length(aBinString));
end;

function ErrorHeader(const aString: string): string;
begin
  if Length(aString)<38 then begin
    Result:='**'+aString+StringOfChar('*',38-Length(aString));
  end else begin
    Result:='**'+aString+'**';
  end;
end;

{ THPackTestDecoder }

procedure THPackTestDecoder.SetUp;
begin
  //Setup 2 dummy encoder & decoder to avoid multiple
  //creation of internal tables. This should be fixed some
  //way in the future.
  DummyDecoder:=THPackDecoder.Create;
  DummyEncoder:=THPackEncoder.Create;
  inherited SetUp;
end;

procedure THPackTestDecoder.TearDown;
begin
  FreeAndNil(DummyEncoder);
  FreeAndNil(DummyDecoder);
  inherited TearDown;
end;

procedure THPackTestDecoder.VerifyIncompleteIndexRead;
var
  Data: TStringStream;
begin
  Data:=TStringStream.Create(HexToBinString('FFF0'));
  HPDecoder:=THPackDecoder.Create;
  try
    HPDecoder.Decode(Data);
    AssertEquals(Data.Size-Data.Position,1);
    HPDecoder.Decode(Data);
    AssertEquals(Data.Size-Data.Position,1);
  finally
    Data.Free;
    FreeAndNil(HPDecoder);
  end;
end;

procedure THPackTestDecoder.InvalidTableIndexZero;
begin
  HPDecoder:=THPackDecoder.Create;
  try
    try
      HPDecoder.Decode(HexToBinString('80'));
      FAIL('Exception missing');
    except
      on e: Exception do begin
        if not (e is THPACKException) then begin
          Raise;
        end;
      end;
    end;
  finally
    FreeAndNil(HPDecoder);
  end;
end;

procedure THPackTestDecoder.IndexShiftOverflow;
begin
  HPDecoder:=THPackDecoder.Create;
  try
    try
      HPDecoder.Decode(HexToBinString('FF8080808008'));
      FAIL('Exception missing');
    except
      on e: Exception do begin
        if not (e is THPACKException) then begin
          Raise;
        end;
      end;
    end;
  finally
    FreeAndNil(HPDecoder);
  end;
end;

procedure THPackTestDecoder.DynamicTableSizeUpdate;
begin
  HPDecoder:=THPackDecoder.Create;
  try
    HPDecoder.Decode(HexToBinString('20'));
    AssertEquals(0,HPDecoder.GetMaxHeaderTableSize);
    HPDecoder.Decode(HexToBinString('3FE11F'));
    assertEquals(4096, HPDecoder.GetMaxHeaderTableSize);
  finally
    FreeAndNil(HPDecoder);
  end;
end;

procedure THPackTestDecoder.DynamicTableSizeUpdateRequired;
begin
  HPDecoder:=THPackDecoder.Create;
  try
    HPDecoder.SetMaxHeaderTableSize(32);
    HPDecoder.Decode(HexToBinString('3F00'));
    assertEquals(31, HPDecoder.GetMaxHeaderTableSize);
  finally
    FreeAndNil(HPDecoder);
  end;
end;

procedure THPackTestDecoder.IllegalDynamicTableSizeUpdate;
begin
  HPDecoder:=THPackDecoder.Create;
  try
    try
      HPDecoder.Decode(HexToBinString('3FE21F'));
      FAIL('Exception missing');
    except
      on e: Exception do begin
        if not (e is THPACKException) then begin
          raise;
        end;
      end;
    end;
  finally
    FreeAndNil(HPDecoder);
  end;
end;

procedure THPackTestDecoder.MaxDynamicTableSizeSignOverflow;
begin
  HPDecoder:=THPackDecoder.Create;
  try
    try
      HPDecoder.Decode(HexToBinString('3FE1FFFFFF07'));
    except
      on e: Exception do begin
        if not (e is THPACKException) then begin
          raise;
        end;
      end;
    end;
  finally
    FreeAndNil(HPDecoder);
  end;
end;

procedure THPackTestDecoder.ReduceMaxDynamicTableSize;
begin
  HPDecoder:=THPackDecoder.Create;
  try
    HPDecoder.SetMaxHeaderTableSize(0);
    AssertEquals(0, HPDecoder.GetMaxHeaderTableSize());
    HPDecoder.Decode(HexToBinString('2081'));
    AssertEquals(0, HPDecoder.GetMaxHeaderTableSize());
  finally
    FreeAndNil(HPDecoder);
  end;
end;

procedure THPackTestDecoder.TooLargeDynamicTableSizeUpdate;
begin
  HPDecoder:=THPackDecoder.Create;
  try
    HPDecoder.SetMaxHeaderTableSize(0);
    AssertEquals(0, HPDecoder.GetMaxHeaderTableSize());
    try
      HPDecoder.Decode(HexToBinString('21'));
      FAIL('Exception missing');
    except
      on E:Exception do begin
        if not (e is THPACKException) then begin
          raise;
        end;
      end;
    end;
  finally
    FreeAndNil(HPDecoder);
  end;
end;

procedure THPackTestDecoder.MissingDynamicTableSizeUpdate;
begin
  HPDecoder:=THPackDecoder.Create;
  try
    HPDecoder.SetMaxHeaderTableSize(0);
    AssertEquals(0, HPDecoder.GetMaxHeaderTableSize());
    try
      HPDecoder.Decode(HexToBinString('81'));
      FAIL('Exception missing');
    except
      on E:Exception do begin
        if not (e is THPACKException) then begin
          raise;
        end;
      end;
    end;
  finally
    FreeAndNil(HPDecoder);
  end;
end;

procedure THPackTestDecoder.LiteralWithIncrementalIndexingWithEmptyName;
begin
  HPDecoder:=THPackDecoder.Create;
  try
    try
      HPDecoder.Decode(HexToBinString('000005')+'value');
      FAIL('Exception missing');
    except
      on E:Exception do begin
        if not (e is THPACKException) then begin
          raise;
        end;
      end;
    end;
  finally
    FreeAndNil(HPDecoder);
  end;
end;

procedure THPackTestDecoder.LiteralWithIncrementalIndexingCompleteEviction;
begin
  HPDecoder:=THPackDecoder.Create;
  try
    HPDecoder.Decode(HexToBinString('4004')+'name'+HexToBinString('05')+'value');
    AssertFalse(HPDecoder.EndHeaderBlockTruncated);
    HPDecoder.Decode(HexToBinString('417F811F')+StringOfChar('a',4096));
    AssertFalse(HPDecoder.EndHeaderBlockTruncated);
    HPDecoder.Decode(HexToBinString('4004')+'name'+ HexToBinString('05')+'value'+HexToBinString('BE'));
    AssertEquals('name',HPDecoder.DecodedHeaders[0]^.HeaderName);
    AssertEquals('value',HPDecoder.DecodedHeaders[0]^.HeaderValue);
    AssertEquals('name',HPDecoder.DecodedHeaders[1]^.HeaderName);
    AssertEquals('value',HPDecoder.DecodedHeaders[1]^.HeaderValue);
  finally
    FreeAndNil(HPDecoder);
  end;
end;

procedure THPackTestDecoder.LiteralWithIncrementalIndexingWithLargeName;
begin
  HPDecoder:=THPackDecoder.Create;
  try
    HPDecoder.Decode(HexToBinString('417F811F')+StringOfChar('a',16384)+HexToBinString('00'));
    // Verify header block is reported as truncated
    AssertTrue(HPDecoder.EndHeaderBlockTruncated);
    // Verify next header is inserted at index 62
    HPDecoder.Decode(HexToBinString('4004')+'name'+ HexToBinString('05')+'value'+HexToBinString('BE'));
    AssertEquals('name',HPDecoder.DecodedHeaders[0]^.HeaderName);
    AssertEquals('value',HPDecoder.DecodedHeaders[0]^.HeaderValue);
    AssertEquals('name',HPDecoder.DecodedHeaders[1]^.HeaderName);
    AssertEquals('value',HPDecoder.DecodedHeaders[1]^.HeaderValue);
  finally
    FreeAndNil(HPDecoder);
  end;
end;

procedure THPackTestDecoder.LiteralWithIncrementalIndexingWithLargeValue;
begin
  HPDecoder:=THPackDecoder.Create;
  try
    HPDecoder.Decode(HexToBinString('4004')+'name'+HexToBinString('7F813F')+StringOfChar('a',8192));
    // Verify header block is reported as truncated
    AssertTrue(HPDecoder.EndHeaderBlockTruncated);
    // Verify next header is inserted at index 62
    HPDecoder.Decode(HexToBinString('4004')+'name'+ HexToBinString('05')+'value'+HexToBinString('BE'));
    AssertEquals('name',HPDecoder.DecodedHeaders[0]^.HeaderName);
    AssertEquals('value',HPDecoder.DecodedHeaders[0]^.HeaderValue);
    AssertEquals('name',HPDecoder.DecodedHeaders[1]^.HeaderName);
    AssertEquals('value',HPDecoder.DecodedHeaders[1]^.HeaderValue);
  finally
    FreeAndNil(HPDecoder);
  end;
end;

procedure THPackTestDecoder.LiteralWithoutIndexingWithEmptyName;
begin
  HPDecoder:=THPackDecoder.Create;
  try
    try
      HPDecoder.Decode(HexToBinString('000005')+'value');
      FAIL('Exception missing');
    except
      on E:Exception do begin
        if not (e is THPACKException) then begin
          raise;
        end;
      end;
    end;
  finally
    FreeAndNil(HPDecoder);
  end;
end;

procedure THPackTestDecoder.LiteralWithoutIndexingWithLargeName;
begin
  HPDecoder:=THPackDecoder.Create;
  try
    HPDecoder.Decode(HexToBinString('007F817F')+StringOfChar('a',16384)+HexToBinString('00'));
    // Verify header block is reported as truncated
    AssertTrue(HPDecoder.EndHeaderBlockTruncated);
    try
      HPDecoder.Decode(HexToBinString('BE'));
      FAIL('Exception missing');
    except
      on E:Exception do begin
        if not (e is THPACKException) then begin
          raise;
        end;
      end;
    end;
  finally
    FreeAndNil(HPDecoder);
  end;
end;

procedure THPackTestDecoder.LiteralWithoutIndexingWithLargeValue;
begin
  HPDecoder:=THPackDecoder.Create;
  try
    HPDecoder.Decode(HexToBinString('0004')+'name'+HexToBinString('7F813F')+StringOfChar('a',8192));
    // Verify header block is reported as truncated
    AssertTrue(HPDecoder.EndHeaderBlockTruncated);
    try
      HPDecoder.Decode(HexToBinString('BE'));
      FAIL('Exception missing');
    except
      on E:Exception do begin
        if not (e is THPACKException) then begin
          raise;
        end;
      end;
    end;
  finally
    FreeAndNil(HPDecoder);
  end;
end;

procedure THPackTestDecoder.LiteralNeverIndexedWithEmptyName;
begin
  HPDecoder:=THPackDecoder.Create;
  try
    try
      HPDecoder.Decode(HexToBinString('100005')+'value');
      FAIL('Exception missing');
    except
      on E:Exception do begin
        if not (e is THPACKException) then begin
          raise;
        end;
      end;
    end;
  finally
    FreeAndNil(HPDecoder);
  end;
end;

procedure THPackTestDecoder.LiteralNeverIndexedWithLargeName;
begin
  HPDecoder:=THPackDecoder.Create;
  try
    HPDecoder.Decode(HexToBinString('107F817F')+StringOfChar('a',16384)+HexToBinString('00'));
    // Verify header block is reported as truncated
    AssertTrue(HPDecoder.EndHeaderBlockTruncated);
    try
      HPDecoder.Decode(HexToBinString('BE'));
      FAIL('Exception missing');
    except
      on E:Exception do begin
        if not (e is THPACKException) then begin
          raise;
        end;
      end;
    end;
  finally
    FreeAndNil(HPDecoder);
  end;
end;

procedure THPackTestDecoder.LiteralNeverIndexedWithLargeValue;
begin
  HPDecoder:=THPackDecoder.Create;
  try
    HPDecoder.Decode(HexToBinString('1004')+'name'+HexToBinString('7F813F')+StringOfChar('a',8192));
    // Verify header block is reported as truncated
    AssertTrue(HPDecoder.EndHeaderBlockTruncated);
    try
      HPDecoder.Decode(HexToBinString('BE'));
      FAIL('Exception missing');
    except
      on E:Exception do begin
        if not (e is THPACKException) then begin
          raise;
        end;
      end;
    end;
  finally
    FreeAndNil(HPDecoder);
  end;
end;

procedure THPackTestCaseCycle.TestHookUp;
begin
  RunSampleHeadersTest;
end;

function THPackTestCaseCycle.GetTestName: string;
begin
  Result:='Sample headers cycled';
end;

procedure THPackTestCaseCycle.TestThisSequence(const aGroup: integer; const aStory: integer; const aJSon: TJSONData);
var
  HeadersPath: TJSonData;
  HexWire: string;
  BinWire: RawByteString;
  BinWire2: RawByteString;
  Sequence: integer;
  ExpectedHeaders: THPackHeaderTextList;
  j, HeaderTableSize: integer;
  lName,lValue: string;
  TestPassed: integer;
  function GetInteger(const aPath: string; const aOptional: Boolean=false): integer;
  var
    tmp: TJSonData;
  begin
    tmp:=aJSon.FindPath(aPath);
    if Assigned(tmp) then begin
      Result:=tmp.AsInteger;
    end else begin
      if not aOptional then begin
        Raise Exception.Create('Missing '+aPath);
      end else begin
        Result:=-1;
      end;
    end;
  end;
  function GetString(const aPath: string): String;
  var
    tmp: TJSonData;
  begin
    tmp:=aJSon.FindPath(aPath);
    if Assigned(tmp) then begin
      Result:=tmp.AsString;
    end else begin
      Raise Exception.Create('Missing '+aPath);
    end;
  end;
  procedure GetHeadersPair(const aHeaders: TJSonData; out aName,aValue: string);
  var
    Enumerator: TBaseJSONEnumerator;
  begin
    aName:='';
    aValue:='';
    if aHeaders.Count<>1 then begin
      Raise Exception.Create('Unexpected headers count = '+aHeaders.AsJSON);
    end;
    Enumerator:=aHeaders.GetEnumerator;
    try
      if Assigned(Enumerator) then begin
        if Enumerator.MoveNext then begin
          aName:=Enumerator.Current.Key;
          aValue:=Enumerator.Current.Value.AsString;
          if Enumerator.MoveNext then begin
            Raise Exception.Create('Too many header parts, expected A=B');
          end;
          Exit;
        end;
      end;
      Raise Exception.Create('Unexpected reach');
    finally
      Enumerator.Free;
    end;
  end;

  function EncodeHeaders(const aEncoder: THPackEncoder; const aHeadersList: THPackHeaderTextList): String;
  var
    OutStream: TStringStream;
    j: integer;
  begin
    Result:='';
    OutStream:=TStringStream.Create('');
    try
      for j := 0 to Pred(aHeadersList.Count) do begin
        aEncoder.EncodeHeader(OutStream,aHeadersList[j]^.HeaderName,aHeadersList[j]^.HeaderValue,aHeadersList[j]^.IsSensitive);
      end;
      Result:=OutStream.DataString;
    finally
      FreeAndNil(OutStream);
    end;
  end;

begin
  TestPassed:=0;
  Sequence:=GetInteger('seqno');
  HexWire:=GetString('wire');
  HeaderTableSize:=GetInteger('header_table_size',true);
  if HeaderTableSize=-1 then begin
    HeaderTableSize:=HPACK_MAX_HEADER_TABLE_SIZE;
  end;
  if HeaderTableSize<>HPDecoder.GetMaxHeaderTableSize then begin
    {$IFNDEF QUIET}
    writeln('Max header table size changed from ',HPDecoder.GetMaxHeaderTableSize,' to ',HeaderTableSize);
    {$ENDIF}
    HPDecoder.SetMaxHeaderTableSize(HeaderTableSize);
  end;
  ExpectedHeaders:=THPackHeaderTextList.Create;
  {$IFNDEF QUIET}
  write('SEQ: ',aGroup,'-',aStory,'-',Sequence,#13);
  {$ENDIF}
  try
    HeadersPath:=aJSon.FindPath('headers');
    if not Assigned(HeadersPath) then begin
      Raise Exception.Create('Missing headers');
    end;
    for j := 0 to Pred(HeadersPath.Count) do begin
      GetHeadersPair(HeadersPath.Items[j],lName,lValue);
      ExpectedHeaders.Add(lName,lValue);
    end;
    BinWire:=HexToBinString(HexWire);
    HPDecoder.Decode(BinWire);
    if HPDecoder.EndHeaderBlockTruncated then begin
      raise Exception.Create('FAIL EndHeaderBlock');
    end;
    if HPDecoder.DecodedHeaders.Text<>ExpectedHeaders.Text then begin
      raise Exception.Create('Expected headers different than decoded ones.');
    end;

    TestPassed:=1;

    // Now reencode with our engine and decode again, result must be the same.
    BinWire2:=EncodeHeaders(HPIntfEncoderPlain,ExpectedHeaders);
    HPIntfDecoderPlain.Decode(BinWire2);
    if HPIntfDecoderPlain.EndHeaderBlockTruncated then begin
      raise Exception.Create('FAIL EndHeaderBlock REcoded (Plain).');
    end;
    if HPIntfDecoderPlain.DecodedHeaders.Text<>ExpectedHeaders.Text then begin
      raise Exception.Create('Expected headers different than REcoded ones (Plain).');
    end;

    TestPassed:=2;

    // Now reencode with our engine and decode again, result must be the same.
    BinWire2:=EncodeHeaders(HPIntfEncoderPlainIndexed,ExpectedHeaders);
    HPIntfDecoderPlainIndexed.Decode(BinWire2);
    if HPIntfDecoderPlainIndexed.EndHeaderBlockTruncated then begin
      raise Exception.Create('FAIL EndHeaderBlock REcoded (Plain & Indexed).');
    end;
    if HPIntfDecoderPlainIndexed.DecodedHeaders.Text<>ExpectedHeaders.Text then begin
      raise Exception.Create('Expected headers different than REcoded ones (Plain & Indexed).');
    end;

    TestPassed:=3;

    // Now reencode with our engine using huffman and decode again, result must be the same.
    BinWire2:=EncodeHeaders(HPIntfEncoderHuffman,ExpectedHeaders);
    HPIntfDecoderHuffman.Decode(BinWire2);
    if HPIntfDecoderHuffman.EndHeaderBlockTruncated then begin
      raise Exception.Create('FAIL EndHeaderBlock REcoded (Huffman).');
    end;
    if HPIntfDecoderHuffman.DecodedHeaders.Text<>ExpectedHeaders.Text then begin
      raise Exception.Create('Expected headers different than REcoded ones (Huffman).');
    end;

    TestPassed:=4;

    // Now reencode with our engine using huffman & indexed and decode again, result must be the same.
    BinWire2:=EncodeHeaders(HPIntfEncoderHuffmanIndexed,ExpectedHeaders);
    HPIntfDecoderHuffmanIndexed.Decode(BinWire2);
    if HPIntfDecoderHuffmanIndexed.EndHeaderBlockTruncated then begin
      raise Exception.Create('FAIL EndHeaderBlock REcoded (Huffman & Indexed).');
    end;
    if HPIntfDecoderHuffmanIndexed.DecodedHeaders.Text<>ExpectedHeaders.Text then begin
      raise Exception.Create('Expected headers different than REcoded ones (Huffman & Indexed).');
    end;
    inc(DecodedBytes,Length(HPIntfDecoderHuffmanIndexed.DecodedHeaders.Text));
    inc(WireBytes,Length(BinWire2));

    TestPassed:=1000;
  finally
    if TestPassed<1000 then begin
      {$IFNDEF FULL_QUIET}
      writeln(StdErr,ErrorHeader('TEST FAIL - Section passed '+inttostr(TestPassed)));
      writeln(StdErr,ErrorHeader('Expected headers'));
      writeln(StdErr,ExpectedHeaders.Text);
      writeln(StdErr,ErrorHeader('Got headers'));
      case TestPassed of
        0: writeln(StdErr,HPDecoder.DecodedHeaders.Text);
        1: writeln(StdErr,HPIntfDecoderPlain.DecodedHeaders.Text);
        2: writeln(StdErr,HPIntfDecoderPlainIndexed.DecodedHeaders.Text);
        3: writeln(StdErr,HPIntfDecoderHuffman.DecodedHeaders.Text);
        4: writeln(StdErr,HPIntfDecoderHuffmanIndexed.DecodedHeaders.Text);
      else
        writeln(StdErr,'Unknown decoder in use.');
      end;
      writeln(StdErr,ErrorHeader('Location'));
      writeln(StdErr,'SEQ: ',aGroup,'-',aStory,'-',Sequence);
      {$ENDIF}
    end else begin
      inc(SequenceCounter);
    end;
    ExpectedHeaders.Free;
  end;
end;

procedure THPackTestCaseCycle.TestCaseStory(const aGroup: integer; const aStory: integer;
  const aJSon: TJSONData);
var
  JSonData: TJSONData;
  CaseData: TJSonData;
  CaseCounter,Cases: integer;
  TestPass: Boolean;
begin
  TestPass:=false;
  JSonData:=ajSon.FindPath('description');
  if Assigned(JSonData) then begin
    {$IFNDEF QUIET}
    writeln(JSonData.AsString);
    {$ENDIF}
  end;
  JSonData:=ajSon.FindPath('cases');
  if Assigned(JSonData) then begin
    Cases:=JSonData.Count;
    {$IFNDEF QUIET}
    writeln('Sequences in case ',Cases);
    {$ENDIF}
    HPDecoder:=THPackDecoder.Create(HPACK_MAX_HEADER_SIZE,HPACK_MAX_HEADER_TABLE_SIZE);

    // This encoders, decoders are for cycle compress, decompress tests.
    HPIntfDecoderPlain:=THPackDecoder.Create(HPACK_MAX_HEADER_SIZE,HPACK_MAX_HEADER_TABLE_SIZE);
    HPIntfDecoderPlainIndexed:=THPackDecoder.Create(HPACK_MAX_HEADER_SIZE,HPACK_MAX_HEADER_TABLE_SIZE);
    HPIntfDecoderHuffman:=THPackDecoder.Create(HPACK_MAX_HEADER_SIZE,HPACK_MAX_HEADER_TABLE_SIZE);
    HPIntfDecoderHuffmanIndexed:=THPackDecoder.Create(HPACK_MAX_HEADER_SIZE,HPACK_MAX_HEADER_TABLE_SIZE);

    HPIntfEncoderPlain:=THPackEncoder.Create(HPACK_MAX_HEADER_TABLE_SIZE,false,false,true);
    HPIntfEncoderPlainIndexed:=THPackEncoder.Create(HPACK_MAX_HEADER_TABLE_SIZE,true,false,true);
    HPIntfEncoderHuffman:=THPackEncoder.Create(HPACK_MAX_HEADER_TABLE_SIZE,false,true,false);
    HPIntfEncoderHuffmanIndexed:=THPackEncoder.Create(HPACK_MAX_HEADER_TABLE_SIZE,true,true,false);
    try
      CaseCounter:=0;
      while CaseCounter<Cases do begin
        CaseData:=JSonData.Items[CaseCounter];
        TestThisSequence(aGroup,aStory,CaseData);
        inc(CaseCounter);
      end;
      TestPass:=true;
    finally
      if not TestPass then begin
        {$IFNDEF FULL_QUIET}
        writeln(StdErr,ErrorHeader('Sequence failed'));
        writeln(StdErr,'Seq expected: ',CaseCounter);
        {$ENDIF}
      end else begin
        inc(StoryCounter);
      end;
      FreeAndNil(HPDecoder);
      FreeAndNil(HPIntfDecoderPlain);
      FreeAndNil(HPIntfDecoderPlainIndexed);
      FreeAndNil(HPIntfDecoderHuffman);
      FreeAndNil(HPIntfDecoderHuffmanIndexed);
      FreeAndNil(HPIntfEncoderPlain);
      FreeAndNil(HPIntfEncoderPlainIndexed);
      FreeAndNil(HPIntfEncoderHuffman);
      FreeAndNil(HPIntfEncoderHuffmanIndexed);
    end;
  end;
end;

procedure THPackTestCaseCycle.RunSampleHeadersTest;
const
  TestCaseBase: string ='hpack-test-case-master'+PathDelim;
  TestCaseGroups: array [0..10] of string =
      (
      'go-hpack',
      'haskell-http2-linear',
      'haskell-http2-linear-huffman',
      'haskell-http2-naive',
      'haskell-http2-naive-huffman',
      'haskell-http2-static',
      'haskell-http2-static-huffman',
      'nghttp2',
      'nghttp2-16384-4096',
      'nghttp2-change-table-size',
      'node-http2-hpack'
      );
  TestCaseStoryMask: string ='story_%.2d.json';
var
  TheFile: string;
  JSonParser: TJSONParser;
  JSonData: TJSonData;
  MyStream: TFileStream;
  j: integer;
  FolderCounter: integer;
  FailCounter: Integer=0;
  ElapsedTime: QWord;
begin
  SequenceCounter:=0;
  StoryCounter:=0;
  GroupsCounter:=0;
  WireBytes:=0;
  DecodedBytes:=0;
  ElapsedTime:=GetTickCount64;
  FolderCounter:=0;
  while FolderCounter<=High(TestCaseGroups) do begin
    j:=0;
    while true do begin
      TheFile:=IncludeTrailingPathDelimiter(TestCaseBase)+IncludeTrailingPathDelimiter(TestCaseGroups[FolderCounter])+format(TestCaseStoryMask,[j]);
      if not FileExists(TheFile) then begin
        break;
      end;
      MyStream:=TFileStream.Create(TheFile,fmOpenRead or fmShareDenyWrite);
      JSonParser:=TJSONParser.Create(MyStream,[]);
      JSonData:=JSonParser.Parse;
      {$IFNDEF QUIET}
      writeln('Check story ',Thefile);
      {$ENDIF}
      try
        try
          TestCaseStory(FolderCounter,j,JSonData);
        finally
          FreeAndNil(JSonData);
          FreeAndNil(JSonParser);
          FreeAndNil(MyStream);
        end;
      except
        on e: exception do begin
          {$IFNDEF FULL_QUIET}
          writeln(StdErr,ErrorHeader('Story failed'));
          writeln(StdErr,TheFile);
          writeln(StdErr,ErrorHeader('Fail condition'));
          writeln(StdErr,e.Message);
          inc(FailCounter);
          {$ENDIF}
          break;
        end;
      end;
      inc(j);
    end;
    inc(GroupsCounter);
    inc(FolderCounter);
  end;
  ElapsedTime:=GetTickCount64-ElapsedTime;
  {$IFNDEF QUIET}
  writeln;
  writeln;
  {$ENDIF}
  {$IFNDEF FULL_QUIET}
  writeln(ErrorHeader('Summary'));
  writeln('Groups: ',GroupsCounter);
  writeln('Stories: ',StoryCounter);
  writeln('Sequences: ',SequenceCounter);
  writeln('Time: ',ElapsedTime/1000:1:3,' seconds.');
  writeln('Wire bytes / Decoded bytes: ',WireBytes,' / ',DecodedBytes);
  writeln('Compression ratio: ',WireBytes/DecodedBytes:1:3);
  writeln('Failed tests: ',FailCounter);
  {$ENDIF}
  if FailCounter>0 then begin
    Fail('Failed cycle tests: %d',[FailCounter]);
  end;
end;

initialization

  RegisterTest(THPackTestCaseCycle);
  RegisterTest(THPackTestDecoder);
end.

