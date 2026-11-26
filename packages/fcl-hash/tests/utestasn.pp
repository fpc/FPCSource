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


initialization
  RegisterTest(TTestASN);
end.
