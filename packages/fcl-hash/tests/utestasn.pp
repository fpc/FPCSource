unit utestasn;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, fpasn, fphashutils;

type

  { TTestASN }

  TTestASN = class(TTestCase)
  private
    FList: TStringList;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    // Type of item aIndex in the parse list
    function ItemType(aIndex: Integer): Integer;
    // Size of item aIndex in the parse list
    function ItemSize(aIndex: Integer): Integer;
  published
    procedure TestValidSequence;
    procedure TestFetchNegativeLength;
    procedure TestReadLenNegativeLength;
    procedure TestParseNegativeLength;
    procedure TestDebugNegativeLength;
    procedure TestBitStringWithoutContent;
    procedure TestTruncatedItem;
    procedure TestConstructedTooLong;
    procedure TestBitStringContent;
    procedure TestBitStringContentBounded;
    procedure TestBitStringNestingLimit;
    procedure TestDebugBitStringWithoutContent;
    procedure TestEncodeUInt;
    procedure TestOIDRoundTrip;
  end;

implementation

procedure TTestASN.SetUp;
begin
  FList:=TStringList.Create;
end;


procedure TTestASN.TearDown;
begin
  FreeAndNil(FList);
end;


function TTestASN.ItemType(aIndex: Integer): Integer;
var
  lSize: Integer;
begin
  ASNParse_GetItem(FList,aIndex,Result,lSize);
end;


function TTestASN.ItemSize(aIndex: Integer): Integer;
var
  lType: Integer;
begin
  ASNParse_GetItem(FList,aIndex,lType,Result);
end;


procedure TTestASN.TestValidSequence;
begin
  ASNParse(TBytes([$30,$07,$02,$01,$05,$04,$02,$AA,$BB]),FList);
  AssertEquals('Item count',3,FList.Count);
  AssertEquals('Sequence type',ASN1_SEQ,ItemType(0));
  AssertEquals('Sequence size',7,ItemSize(0));
  AssertEquals('Integer type',ASN1_INT,ItemType(1));
  AssertEquals('Integer value','5',FList[1]);
  AssertEquals('Octet string type',ASN1_OCTSTR,ItemType(2));
  AssertEquals('Octet string value','AABB',UpperCase(FList[2]));
end;


procedure TTestASN.TestFetchNegativeLength;
var
  lData: TBytes;
  lBuffer: PByte;
  lType, lSize: Int32;
begin
  lData:=TBytes([$05,$84,$FF,$FF,$FF,$FA]);
  lBuffer:=@lData[0];
  AssertFalse('Length above High(Int32) rejected',ASNFetch(lBuffer,@lData[0]+Length(lData),lType,lSize));
  lData:=TBytes([$04,$84,$7F,$FF,$FF,$FF]);
  lBuffer:=@lData[0];
  AssertTrue('Length High(Int32) accepted',ASNFetch(lBuffer,@lData[0]+Length(lData),lType,lSize));
  AssertEquals('Length High(Int32)',High(Int32),lSize);
end;


procedure TTestASN.TestReadLenNegativeLength;
var
  lData: TBytes;
  lBuffer: PByte;
  lRaised: Boolean;
begin
  lData:=TBytes([$84,$FF,$FF,$FF,$FA]);
  lBuffer:=@lData[0];
  lRaised:=False;
  try
    ASNReadLen(lBuffer,@lData[0]+Length(lData));
  except
    on EHashUtil do
      lRaised:=True;
  end;
  AssertTrue('Length above High(Int32) raises EHashUtil',lRaised);
  lData:=TBytes([$05]);
  lBuffer:=@lData[0]+1;
  lRaised:=False;
  try
    ASNReadLen(lBuffer,@lData[0]+Length(lData));
  except
    on EHashUtil do
      lRaised:=True;
  end;
  AssertTrue('Reading at the end of the buffer raises EHashUtil',lRaised);
end;


procedure TTestASN.TestParseNegativeLength;
begin
  ASNParse(TBytes([$05,$84,$FF,$FF,$FF,$FA]),FList);
  AssertEquals('NULL with length FFFFFFFA gives no items',0,FList.Count);
  ASNParse(TBytes([$04,$84,$FF,$FF,$FF,$FA]),FList);
  AssertEquals('Octet string with length FFFFFFFA gives no items',0,FList.Count);
  ASNParse(TBytes([$02,$01,$07,$30,$84,$FF,$FF,$FF,$FF,$05,$00]),FList);
  AssertEquals('Parsing stops at the sequence with length FFFFFFFF',1,FList.Count);
  AssertEquals('Integer before the sequence','7',FList[0]);
end;


procedure TTestASN.TestDebugNegativeLength;
var
  lRaised: Boolean;
begin
  lRaised:=False;
  try
    ASNDebug(TBytes([$05,$84,$FF,$FF,$FF,$FA]));
  except
    on EHashUtil do
      lRaised:=True;
  end;
  AssertTrue('ASNDebug raises EHashUtil for length FFFFFFFA',lRaised);
end;


procedure TTestASN.TestBitStringWithoutContent;
begin
  ASNParse(TBytes([$04,$01,$AA,$03,$00]),FList);
  AssertEquals('Only the octet string is parsed',1,FList.Count);
  AssertEquals('Octet string type',ASN1_OCTSTR,ItemType(0));
end;


procedure TTestASN.TestTruncatedItem;
begin
  ASNParse(TBytes([$02,$01,$07,$04,$05,$AA,$BB]),FList);
  AssertEquals('Parsing stops at the truncated octet string',1,FList.Count);
  AssertEquals('Integer before the truncated item','7',FList[0]);
end;


procedure TTestASN.TestConstructedTooLong;
begin
  ASNParse(TBytes([$02,$01,$07,$30,$05,$02,$01,$01]),FList);
  AssertEquals('Parsing stops at the sequence longer than the buffer',1,FList.Count);
  AssertEquals('Integer before the sequence','7',FList[0]);
end;


procedure TTestASN.TestBitStringContent;
begin
  // BIT STRING, 0 unused bits, content SEQUENCE { INTEGER 5 }, then NULL
  ASNParse(TBytes([$03,$06,$00,$30,$03,$02,$01,$05,$05,$00]),FList);
  AssertEquals('Item count',4,FList.Count);
  AssertEquals('Bit string',ASN1_BITSTR,ItemType(0));
  AssertEquals('Sequence inside the bit string',ASN1_SEQ,ItemType(1));
  AssertEquals('Integer inside the bit string',ASN1_INT,ItemType(2));
  AssertEquals('Integer value','5',FList[2]);
  AssertEquals('Null after the bit string',ASN1_NULL,ItemType(3));
end;


procedure TTestASN.TestBitStringContentBounded;
begin
  // The SEQUENCE inside the bit string claims 6 bytes, the bit string only has 4 left
  ASNParse(TBytes([$03,$06,$00,$30,$06,$02,$01,$05,$05,$00,$05,$00]),FList);
  AssertEquals('Parsing stops inside the bit string',1,FList.Count);
  AssertEquals('Bit string',ASN1_BITSTR,ItemType(0));
end;


procedure TTestASN.TestBitStringNestingLimit;

  // DER length bytes for aLength
  function EncodeLen(aLength: Integer): TBytes;
  begin
    if aLength<128 then
      Result:=TBytes([aLength])
    else if aLength<256 then
      Result:=TBytes([$81,aLength])
    else
      Result:=TBytes([$82,aLength shr 8,aLength and $FF]);
  end;

var
  lData, lSeq: TBytes;
  I, lLevels: Integer;
begin
  // 40 levels of BIT STRING { SEQUENCE { ... } } around INTEGER 1
  lLevels:=40;
  lData:=TBytes([$02,$01,$01]);
  for I:=1 to lLevels do
    begin
    lSeq:=Concat(TBytes([$30]),EncodeLen(Length(lData)),lData);
    lData:=Concat(TBytes([$03]),EncodeLen(Length(lSeq)+1),TBytes([$00]),lSeq);
    end;
  ASNParse(lData,FList);
  AssertTrue('Parsing stops before the innermost integer',FList.Count<2*lLevels+1);
  AssertTrue('Outer levels parsed',FList.Count>=2*32);
end;


procedure TTestASN.TestDebugBitStringWithoutContent;
var
  lRaised: Boolean;
begin
  lRaised:=False;
  try
    ASNDebug(TBytes([$03,$00]));
  except
    on EHashUtil do
      lRaised:=True;
  end;
  AssertTrue('ASNDebug raises EHashUtil for a bit string without unused-bits byte',lRaised);
end;


procedure TTestASN.TestEncodeUInt;

  // Hex of ASNEncodeUInt(aValue)
  function Encode(aValue: Integer): String;
  var
    lBytes: TBytes;
  begin
    lBytes:=[];
    ASNEncodeUInt(aValue,lBytes);
    Result:=UpperCase(BytesToHexStr(lBytes));
  end;

begin
  AssertEquals('0','00',Encode(0));
  AssertEquals('255','FF',Encode(255));
  AssertEquals('300','012C',Encode(300));
  AssertEquals('65536','010000',Encode(65536));
  AssertEquals('$12345678','12345678',Encode($12345678));
  AssertEquals('-1 as unsigned 32 bit','FFFFFFFF',Encode(-1));
end;


procedure TTestASN.TestOIDRoundTrip;
const
  OIDs: array[0..6] of AnsiString = ('0.9.2342.19200300.100.1.1', '1.2.840.113549.1.1.1', '2.5.4.3',
                                     '2.39.1', '2.40.1', '2.100.3', '2.999.3');
var
  I: Integer;
  Encoded, Decoded: AnsiString;
begin
  for I:=Low(OIDs) to High(OIDs) do
    begin
    Encoded:='';
    MibToId(OIDs[I],Encoded);
    Decoded:='stale';
    IdToMib(Encoded,Decoded);
    AssertEquals('String decoding of '+OIDs[I],OIDs[I],Decoded);
    AssertEquals('Buffer decoding of '+OIDs[I],OIDs[I],
                 IdToMib(PByte(Encoded),PByte(Encoded)+Length(Encoded)));
    end;
  Encoded:='';
  MibToId('2.999.3',Encoded);
  AssertEquals('Encoding of 2.999.3','883703',UpperCase(BytesToHexStr(Encoded)));
end;


initialization
  RegisterTest(TTestASN);
end.
