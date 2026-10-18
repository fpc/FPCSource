program tdelphistream;

{$mode objfpc}{$H+}

uses
  sysutils, variants, classes;

const
  DelphiDouble = 21;
  DelphiWString = 18;
  DelphiInt64 = 19;
  DelphiInt8 = 2;
  BigQWord = QWord($F234567890ABCDEF);
  VarQWord = QWord($123456789A);

type
  TTestComp = class(TComponent)
  private
    FDoubleProp: Double;
    FSingleProp: Single;
    FUStr: UnicodeString;
    FU64: QWord;
    FI64: Int64;
    FV: Variant;
  published
    property DoubleProp: Double read FDoubleProp write FDoubleProp;
    property SingleProp: Single read FSingleProp write FSingleProp;
    property UStr: UnicodeString read FUStr write FUStr;
    property U64: QWord read FU64 write FU64;
    property I64: Int64 read FI64 write FI64;
    property V: Variant read FV write FV;
  end;


procedure Check(aCondition: Boolean; aCode: Integer; const aMsg: string);

begin
  if not aCondition then
    begin
      Writeln('Failed: ',aMsg);
      Halt(aCode);
    end;
end;


procedure PutByte(aStream: TStream; aValue: Byte);

begin
  aStream.WriteBuffer(aValue,1);
end;


procedure PutStr(aStream: TStream; const aValue: ShortString);

begin
  aStream.WriteBuffer(aValue[0],Length(aValue)+1);
end;


// Build a stream as written by Delphi, using vaDouble (21) for a double property.
function MakeDelphiStream: TMemoryStream;

const
  lUStr: UnicodeString = 'h'#$E9'llo';
var
  lDouble: Double;
  lLen: LongWord;
  lQ: QWord;
begin
  Result:=TMemoryStream.Create;
  Result.WriteBuffer(FilerSignature,4);
  PutStr(Result,'TTestComp');
  PutStr(Result,'Test');
  PutStr(Result,'DoubleProp');
  PutByte(Result,DelphiDouble);
  lDouble:=3.25;
  Result.WriteBuffer(lDouble,8);
  PutStr(Result,'SingleProp');
  PutByte(Result,DelphiDouble);
  lDouble:=-1.5;
  Result.WriteBuffer(lDouble,8);
  PutStr(Result,'UStr');
  PutByte(Result,DelphiWString);
  lLen:=Length(lUStr);
  Result.WriteBuffer(lLen,4);
  Result.WriteBuffer(lUStr[1],lLen*2);
  PutStr(Result,'U64');
  PutByte(Result,DelphiInt64);
  lQ:=BigQWord;
  Result.WriteBuffer(lQ,8);
  PutStr(Result,'I64');
  PutByte(Result,DelphiInt8);
  PutByte(Result,7);
  PutByte(Result,0);
  PutByte(Result,0);
  Result.Position:=0;
end;


// Return the value type byte that follows property name aName in aStream.
function ValueTypeAfter(aStream: TMemoryStream; const aName: ShortString): Integer;

var
  lData: PByte;
  i: Integer;
begin
  Result:=-1;
  lData:=aStream.Memory;
  for i:=0 to aStream.Size-Length(aName)-2 do
    if (lData[i]=Length(aName)) and CompareMem(@lData[i+1],@aName[1],Length(aName)) then
      Exit(lData[i+1+Length(aName)]);
end;


procedure ReadInto(aStream: TStream; aComp: TTestComp; aDelphi: Boolean);

var
  lReader: TReader;
begin
  aStream.Position:=0;
  lReader:=TReader.Create(aStream,4096);
  try
    lReader.DelphiCompatible:=aDelphi;
    lReader.ReadRootComponent(aComp);
  finally
    lReader.Free;
  end;
end;


procedure WriteFrom(aStream: TStream; aComp: TTestComp; aDelphi: Boolean);

var
  lWriter: TWriter;
begin
  lWriter:=TWriter.Create(aStream,4096);
  try
    lWriter.DelphiCompatible:=aDelphi;
    lWriter.WriteDescendent(aComp,nil);
  finally
    lWriter.Free;
  end;
end;


procedure TestReadDelphi;

var
  lStream: TMemoryStream;
  lComp: TTestComp;
  lFailed: Boolean;
begin
  lStream:=MakeDelphiStream;
  lComp:=TTestComp.Create(nil);
  try
    ReadInto(lStream,lComp,True);
    Check(lComp.DoubleProp=3.25,1,'Delphi vaDouble read into double property');
    Check(lComp.SingleProp=-1.5,2,'Delphi vaDouble read into single property');
    Check(lComp.UStr='h'#$E9'llo',3,'Delphi vaWString read into UnicodeString property');
    Check(lComp.U64=BigQWord,4,'Delphi vaInt64 read into QWord property');
    Check(lComp.I64=7,5,'Delphi vaInt8 read into Int64 property');
    lComp.Free;
    lComp:=TTestComp.Create(nil);
    lFailed:=False;
    try
      ReadInto(lStream,lComp,False);
    except
      lFailed:=True;
    end;
    Check(lFailed,6,'Delphi stream is rejected without DelphiCompatible');
  finally
    lComp.Free;
    lStream.Free;
  end;
end;


procedure TestSkipDelphiDouble;

var
  lStream: TMemoryStream;
  lReader: TReader;
  lDouble: Double;
begin
  lStream:=TMemoryStream.Create;
  try
    PutByte(lStream,DelphiDouble);
    lDouble:=1;
    lStream.WriteBuffer(lDouble,8);
    PutByte(lStream,DelphiInt8);
    PutByte(lStream,9);
    lStream.Position:=0;
    lReader:=TReader.Create(lStream,4096);
    try
      lReader.DelphiCompatible:=True;
      Check(lReader.NextValue=vaDouble,10,'Value type 21 is vaDouble in DelphiCompatible mode');
      lReader.SkipValue;
      Check(lReader.ReadInteger=9,11,'SkipValue skips a vaDouble value');
    finally
      lReader.Free;
    end;
  finally
    lStream.Free;
  end;
end;


procedure TestWriteDelphi;

var
  lStream: TMemoryStream;
  lSrc, lDst: TTestComp;
begin
  lStream:=TMemoryStream.Create;
  lSrc:=TTestComp.Create(nil);
  lDst:=TTestComp.Create(nil);
  try
    lSrc.Name:='Src';
    lSrc.DoubleProp:=2.5;
    lSrc.SingleProp:=0.75;
    lSrc.UStr:='w'#$263A'rld';
    lSrc.U64:=BigQWord;
    lSrc.I64:=-5;
    lSrc.V:=VarQWord;
    WriteFrom(lStream,lSrc,True);
    Check(CompareMem(lStream.Memory,@FilerSignature,4),20,'DelphiCompatible writer uses signature TPF0');
    Check(ValueTypeAfter(lStream,'UStr')=Ord(vaWString),21,'DelphiCompatible writer writes UnicodeString as vaWString');
    Check(ValueTypeAfter(lStream,'V')=Ord(vaInt64),22,'DelphiCompatible writer writes a QWord variant as vaInt64');
    ReadInto(lStream,lDst,True);
    Check(lDst.DoubleProp=2.5,23,'Double round trip in DelphiCompatible mode');
    Check(lDst.SingleProp=0.75,24,'Single round trip in DelphiCompatible mode');
    Check(lDst.UStr=lSrc.UStr,25,'UnicodeString round trip in DelphiCompatible mode');
    Check(lDst.U64=BigQWord,26,'QWord round trip in DelphiCompatible mode');
    Check(lDst.I64=-5,27,'Int64 round trip in DelphiCompatible mode');
    Check(lDst.V=Int64(VarQWord),28,'QWord variant round trip in DelphiCompatible mode');
  finally
    lDst.Free;
    lSrc.Free;
    lStream.Free;
  end;
end;


procedure TestWriteFPC;

var
  lStream: TMemoryStream;
  lSrc, lDst: TTestComp;
begin
  lStream:=TMemoryStream.Create;
  lSrc:=TTestComp.Create(nil);
  lDst:=TTestComp.Create(nil);
  try
    lSrc.Name:='Src';
    lSrc.UStr:='abc';
    lSrc.U64:=BigQWord;
    lSrc.V:=VarQWord;
    WriteFrom(lStream,lSrc,False);
    Check(ValueTypeAfter(lStream,'UStr')=Ord(vaUString),30,'Default writer writes UnicodeString as vaUString');
    Check(ValueTypeAfter(lStream,'V')=Ord(vaQWord),31,'Default writer writes a QWord variant as vaQWord');
    ReadInto(lStream,lDst,False);
    Check(lDst.UStr='abc',32,'UnicodeString round trip in default mode');
    Check(lDst.U64=BigQWord,33,'QWord round trip in default mode');
    Check(lDst.V=VarQWord,34,'QWord variant round trip in default mode');
  finally
    lDst.Free;
    lSrc.Free;
    lStream.Free;
  end;
end;


procedure TestWriteDouble(aDelphi: Boolean; aExpected: Integer; aCode: Integer);

var
  lStream: TMemoryStream;
  lWriter: TWriter;
  lReader: TReader;
begin
  lStream:=TMemoryStream.Create;
  try
    lWriter:=TWriter.Create(lStream,4096);
    try
      lWriter.DelphiCompatible:=aDelphi;
      lWriter.WriteDouble(1/3);
    finally
      lWriter.Free;
    end;
    Check(lStream.Size=9,aCode,'WriteDouble writes a value type and 8 bytes');
    Check(PByte(lStream.Memory)^=aExpected,aCode+1,'WriteDouble value type byte');
    lStream.Position:=0;
    lReader:=TReader.Create(lStream,4096);
    try
      lReader.DelphiCompatible:=aDelphi;
      Check(lReader.ReadDouble=Double(1/3),aCode+2,'ReadDouble reads the value written by WriteDouble');
    finally
      lReader.Free;
    end;
  finally
    lStream.Free;
  end;
end;


procedure TestGlobalDefault;

var
  lStream: TMemoryStream;
  lComp: TTestComp;
begin
  lStream:=MakeDelphiStream;
  lComp:=TTestComp.Create(nil);
  DelphiCompatibleStreams:=True;
  try
    lStream.ReadComponent(lComp);
    Check(lComp.DoubleProp=3.25,40,'DelphiCompatibleStreams applies to TStream.ReadComponent');
  finally
    DelphiCompatibleStreams:=False;
    lComp.Free;
    lStream.Free;
  end;
end;


begin
  TestReadDelphi;
  TestSkipDelphiDouble;
  TestWriteDelphi;
  TestWriteFPC;
  TestWriteDouble(True,DelphiDouble,50);
  TestWriteDouble(False,Ord(vaDouble),53);
  TestGlobalDefault;
  Writeln('ok');
end.
