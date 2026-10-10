program tw40509;

{$mode objfpc}{$h+}

uses
  SysUtils, Classes, Variants;

type
  TCurrHolder = class(TComponent)
  private
    FValue: Currency;
  published
    // Currency value streamed as a published property
    property Value: Currency read FValue write FValue;
  end;

  TVariantHolder = class(TComponent)
  private
    FValue: Variant;
  published
    // Variant value streamed as vaCurrency when it holds a currency
    property Value: Variant read FValue write FValue;
  end;

const
  cValues: array[0..6] of Currency = (0, 1.5, -1.5, -0.0001, 123456.7891, -922337203685477.5807, 922337203685477.5807);
  // Last index of cValues that survives conversion to double
  cLastDouble = 4;

// Halts with aCode and writes aMsg when aOk is false
procedure Check(aCode: Integer; aOk: Boolean; const aMsg: string);

begin
  if not aOk then
    begin
    Writeln('check ',aCode,' failed: ',aMsg);
    Halt(aCode);
    end;
end;


// Writes aValue with TWriter and reads it back with TReader, checking the stream layout
procedure TestRoundTrip(aIndex: Integer; aValue: Currency);

var
  lStream: TMemoryStream;
  lWriter: TWriter;
  lReader: TReader;
  lBytes: array[0..8] of Byte;
  lRaw: Int64;
  i: Integer;

begin
  lStream:=TMemoryStream.Create;
  try
    lWriter:=TWriter.Create(lStream,4096);
    try
      lWriter.WriteCurrency(aValue);
    finally
      lWriter.Free;
    end;
    Check(10+aIndex,lStream.Size=9,'stream size for '+CurrToStr(aValue));
    lStream.Position:=0;
    lStream.ReadBuffer(lBytes,9);
    Check(20+aIndex,lBytes[0]=Ord(vaCurrency),'value type for '+CurrToStr(aValue));
    lRaw:=0;
    for i:=8 downto 1 do
      lRaw:=(lRaw shl 8) or lBytes[i];
    Check(30+aIndex,lRaw=PInt64(@aValue)^,'little endian scaled integer for '+CurrToStr(aValue));
    lStream.Position:=0;
    lReader:=TReader.Create(lStream,4096);
    try
      Check(40+aIndex,lReader.ReadCurrency=aValue,'read back '+CurrToStr(aValue));
    finally
      lReader.Free;
    end;
  finally
    lStream.Free;
  end;
end;


// Streams a component with a published currency property and reads it back
procedure TestComponent(aIndex: Integer; aValue: Currency);

var
  lStream: TMemoryStream;
  lSrc, lDest: TCurrHolder;

begin
  lStream:=TMemoryStream.Create;
  lSrc:=TCurrHolder.Create(nil);
  lDest:=TCurrHolder.Create(nil);
  try
    lSrc.Value:=aValue;
    lStream.WriteComponent(lSrc);
    lStream.Position:=0;
    lStream.ReadComponent(lDest);
    Check(50+aIndex,lDest.Value=aValue,'component property '+CurrToStr(aValue));
  finally
    lDest.Free;
    lSrc.Free;
    lStream.Free;
  end;
end;


// Skips and copies a vaCurrency value with TReader
procedure TestSkipCopy(aIndex: Integer; aValue: Currency);

var
  lStream, lCopy: TMemoryStream;
  lWriter: TWriter;
  lReader: TReader;

begin
  lStream:=TMemoryStream.Create;
  lCopy:=TMemoryStream.Create;
  try
    lWriter:=TWriter.Create(lStream,4096);
    try
      lWriter.WriteCurrency(aValue);
      lWriter.WriteInteger(42);
    finally
      lWriter.Free;
    end;
    lStream.Position:=0;
    lReader:=TReader.Create(lStream,4096);
    try
      lReader.SkipValue;
      Check(60+aIndex,lReader.ReadInteger=42,'value after skipped currency '+CurrToStr(aValue));
    finally
      lReader.Free;
    end;
    lStream.Position:=0;
    lReader:=TReader.Create(lStream,4096);
    try
      lWriter:=TWriter.Create(lCopy,4096);
      try
        lReader.CopyValue(lWriter);
      finally
        lWriter.Free;
      end;
    finally
      lReader.Free;
    end;
    Check(70+aIndex,(lCopy.Size=9) and (CompareByte(lCopy.Memory^,lStream.Memory^,9)=0),'copied currency '+CurrToStr(aValue));
  finally
    lCopy.Free;
    lStream.Free;
  end;
end;


// Converts a component with a currency variant to text and back
procedure TestText(aIndex: Integer; aValue: Currency);

var
  lBin, lText, lBin2: TMemoryStream;
  lSrc, lDest: TVariantHolder;
  lStr, lExpected: string;

begin
  lBin:=TMemoryStream.Create;
  lText:=TMemoryStream.Create;
  lBin2:=TMemoryStream.Create;
  lSrc:=TVariantHolder.Create(nil);
  lDest:=TVariantHolder.Create(nil);
  try
    lSrc.Value:=aValue;
    lBin.WriteComponent(lSrc);
    lBin.Position:=0;
    ObjectBinaryToText(lBin,lText);
    SetLength(lStr,lText.Size);
    Move(lText.Memory^,lStr[1],lText.Size);
    lExpected:='Value = '+IntToStr(PInt64(@aValue)^)+'c';
    Check(80+aIndex,Pos(lExpected,lStr)>0,'text contains '+lExpected+': '+lStr);
    lText.Position:=0;
    ObjectTextToBinary(lText,lBin2);
    Check(90+aIndex,(lBin2.Size=lBin.Size) and (CompareByte(lBin2.Memory^,lBin.Memory^,lBin.Size)=0),'binary from text equals original for '+CurrToStr(aValue));
    lBin2.Position:=0;
    lBin2.ReadComponent(lDest);
    Check(100+aIndex,(VarType(lDest.Value)=varCurrency) and (lDest.Value=aValue),'variant read back from text for '+CurrToStr(aValue));
  finally
    lDest.Free;
    lSrc.Free;
    lBin2.Free;
    lText.Free;
    lBin.Free;
  end;
end;


var
  i: Integer;

begin
  for i:=Low(cValues) to High(cValues) do
    begin
    TestRoundTrip(i,cValues[i]);
    if i<=cLastDouble then
      TestComponent(i,cValues[i]);
    TestSkipCopy(i,cValues[i]);
    TestText(i,cValues[i]);
    end;
  Writeln('ok');
end.
