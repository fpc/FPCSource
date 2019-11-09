unit utsyshelpers;

{$mode objfpc}{$h+}

interface

uses
  Classes, SysUtils, Math;

implementation

uses punit, utrtl;

Function TestByteHelper : String;

Const
  Value               = 123;
  ValueAsString       = '123';
  ValueAsHex          = '7B';
  ValueAsHexDig       = 4;
  ValueAsHexDigString = '007B';

Var
  V : Byte;
begin
  {$i tohelper.inc}
end;

Function TestShortIntHelper : String;

Const
  Value               = 123;
  ValueAsString       = '123';
  ValueAsHex          = '7B';
  ValueAsHexDig       = 4;
  ValueAsHexDigString = '007B';

Var
  V : ShortInt;
begin
  {$i tohelper.inc}
end;

Function TestNegShortIntHelper : String;

Const
  Value               = -123;
  ValueAsString       = '-123';
  ValueAsHex          = '85';
  ValueAsHexDig       = 4;
  ValueAsHexDigString = 'FF85';

Var
  V : ShortInt;
begin
  {$i tohelper.inc}
end;

Function TestWordHelper : String;

Const
  Value               = 1024;
  ValueAsString       = '1024';
  ValueAsHex          = '0400';
  ValueAsHexDig       = 6;
  ValueAsHexDigString = '000400';

Var
  V : Word;
begin
  {$i tohelper.inc}
end;

Function TestSmallintHelper : String;

Const
  Value               = 1024;
  ValueAsString       = '1024';
  ValueAsHex          = '0400';
  ValueAsHexDig       = 6;
  ValueAsHexDigString = '000400';

Var
  V : Smallint;
begin
  {$i tohelper.inc}
end;

Function TestNegSmallintHelper : String;

Const
  Value               = -1024;
  ValueAsString       = '-1024';
  ValueAsHex          = 'FC00';
  ValueAsHexDig       = 6;
  ValueAsHexDigString = 'FFFC00';

Var
  V : Smallint;
begin
  {$i tohelper.inc}
end;

Function TestCardinalHelper : String;

Const
  Value               = 131072;
  ValueAsString       = '131072';
  ValueAsHex          = '00020000';
  ValueAsHexDig       = 10;
  ValueAsHexDigString = '0000020000';

Var
  V : Cardinal;
begin
  {$i tohelper.inc}
end;

Function TestLongintHelper : String;

Const
  Value               = 131072;
  ValueAsString       = '131072';
  ValueAsHex          = '00020000';
  ValueAsHexDig       = 10;
  ValueAsHexDigString = '0000020000';

Var
  V : Longint;

begin
  {$i tohelper.inc}
end;

Function TestNegLongintHelper : String;

Const
  Value               = -131072;
  ValueAsString       = '-131072';
  ValueAsHex          = 'FFFE0000';
  ValueAsHexDig       = 10;
  ValueAsHexDigString = '00FFFE0000';

Var
  V : Longint;
begin
  {$i tohelper.inc}
end;

Function TestQWordHelper : String;

Const
  Value               = 17179869184; // 2^34
  ValueAsString       = '17179869184';
  ValueAsHex          = '0000000400000000';
  ValueAsHexDig       = 18;
  ValueAsHexDigString = '000000000400000000';

Var
  V : QWord;
begin
  {$i tohelper.inc}
end;

Function TestInt64Helper : String;

Const
  Value               = 17179869184; // 2^34
  ValueAsString       = '17179869184';
  ValueAsHex          = '0000000400000000';
  ValueAsHexDig       = 18;
  ValueAsHexDigString = '000000000400000000';

Var
  V : Int64;
begin
  {$i tohelper.inc}
end;

Function TestNegInt64Helper : String;

Const
  Value               = -17179869184; // 2^34
  ValueAsString       = '-17179869184';
  ValueAsHex          = 'FFFFFFFC00000000';
  ValueAsHexDig       = 18;
  ValueAsHexDigString = '00FFFFFFFC00000000';

Var
  V : Int64;
begin
  {$i tohelper.inc}
end;

Procedure GetGUID(out G : TGUID);

Var
  I : Integer;

begin
  G.Data1:=$DDCCBBAA;
  G.Data2:=$EEFF;
  G.Data3:=$CAAC;
  For I:=0 to 7 do
   G.Data4[i]:=(1 shl i) and $FF;
end;

Procedure EqualGUID(Msg : String;Expected,Actual : TGUID);

Var
  I : Integer;

begin
  AssertEquals(Msg+' D1 equal',Expected.D1,Actual.D1);
  AssertEquals(Msg+' D2 equal',Expected.D2,Actual.D2);
  AssertEquals(Msg+' D2 equal',Expected.D3,Actual.D3);
  For I:=0 to 7 do
   AssertEquals(Msg+' D4['+IntToStr(I)+'] equal',Expected.D4[i],Actual.D4[i]);
end;

Procedure EqualGUIDSwap(Msg : String;Expected,Actual : TGUID);

Var
  I : Integer;

begin
  AssertEquals(Msg+' D1 equal',SwapEndian(Expected.D1),Actual.D1);
  AssertEquals(Msg+' D2 equal',SwapEndian(Expected.D2),Actual.D2);
  AssertEquals(Msg+' D2 equal',SwapEndian(Expected.D3),Actual.D3);
  For I:=0 to 7 do
   AssertEquals(Msg+' D4['+IntToStr(I)+'] equal',Expected.D4[i],Actual.D4[i]);
end;

Function TestGUIDHelperCreateUntypedData : String;

Var
  Src,Dest : TGUID;

begin
  Result:='';
  GetGUID(Src);
  Dest:=TGUID.Create(Src, True);
  if CPUEndian = TEndian.Big then
    EqualGUID('BE CPU: Create(Data,True)',Src,Dest)
  else
    EqualGUIDSwap('LE CPU: Create(Data,True)',Src,Dest);
  Dest:=TGUID.Create(Src, False);
  if CPUEndian = TEndian.Big then
    EqualGUIDSwap('BE CPU: Create(Data,False)',Src,Dest)
  else
    EqualGUID('LE CPU : Create(Data,False)',Src,Dest);
end;

Function TestGUIDHelperCreateUntypedDataEndian : String;

Var
  Src,Dest : TGUID;

begin
  Result:='';
  GetGUID(Src);
  Dest:=TGUID.Create(Src, TEndian.Big);
  if CPUEndian = TEndian.Big then
    EqualGUID('BE CPU: Create(Data,True)',Src,Dest)
  else
    EqualGUIDSwap('LE CPU: Create(Data,True)',Src,Dest);
  Dest:=TGUID.Create(Src, TEndian.Little);
  if CPUEndian = TEndian.Big then
    EqualGUIDSwap('BE CPU: Create(Data,False)',Src,Dest)
  else
    EqualGUID('LE CPU : Create(Data,False)',Src,Dest);
end;

Function TestGUIDHelperCreateArrayOfByte : String;

Var
  Src,Dest : TGUID;
  SrcBytes : Array of byte;

begin
  Result:='';
  GetGUID(Src);
  SrcBytes:=[];
  SetLength(SrcBytes,SizeOf(TGUID));
  Move(Src,SrcBytes[0],SizeOf(TGUID));
  Dest:=TGUID.Create(SrcBytes[0], True);
  if CPUEndian = TEndian.Big then
    EqualGUID('BE CPU: Create(Data,True)',Src,Dest)
  else
    EqualGUIDSwap('LE CPU: Create(Data,True)',Src,Dest);
  Dest:=TGUID.Create(SrcBytes[0], False);
  if CPUEndian = TEndian.Big then
    EqualGUIDSwap('BE CPU: Create(Data,False)',Src,Dest)
  else
    EqualGUID('LE CPU : Create(Data,False)',Src,Dest);
end;

Function TestGUIDHelperCreateTBytes : String;

Var
  Src,Dest : TGUID;
  SrcBytes : TBytes;

begin
  Result:='';
  GetGUID(Src);
  SrcBytes:=[];
  SetLength(SrcBytes,SizeOf(TGUID));
  Move(Src,SrcBytes[0],SizeOf(TGUID));
  Dest:=TGUID.Create(SrcBytes, TEndian.Big);
  if CPUEndian = TEndian.Big then
    EqualGUID('BE CPU: Create(Data,True)',Src,Dest)
  else
    EqualGUIDSwap('LE CPU: Create(Data,True)',Src,Dest);
  Dest:=TGUID.Create(SrcBytes, TEndian.Little);
  if CPUEndian = TEndian.Big then
    EqualGUIDSwap('BE CPU: Create(Data,False)',Src,Dest)
  else
    EqualGUID('LE CPU : Create(Data,False)',Src,Dest);
end;

Function TestGUIDHelperCreateTBytesAtIndex : String;

Var
  Src,Dest : TGUID;
  SrcBytes : TBytes;

begin
  Result:='';
  GetGUID(Src);
  SrcBytes:=[];
  SetLength(SrcBytes,SizeOf(TGUID)*2);
  Move(Src,SrcBytes[4],SizeOf(TGUID));
  Dest:=TGUID.Create(SrcBytes, 4, TEndian.Big);
  if CPUEndian = TEndian.Big then
    EqualGUID('BE CPU: Create(Data,True)',Src,Dest)
  else
    EqualGUIDSwap('LE CPU: Create(Data,True)',Src,Dest);
  Dest:=TGUID.Create(SrcBytes, 4, TEndian.Little);
  if CPUEndian = TEndian.Big then
    EqualGUIDSwap('BE CPU: Create(Data,False)',Src,Dest)
  else
    EqualGUID('LE CPU : Create(Data,False)',Src,Dest);
end;

Function TestGUIDHelperCreateString : String;

Var
  Src,Dest : TGUID;
  SrcBytes : TBytes;

begin
  Result:='';
  GetGUID(Src);
  SrcBytes:=[];
  SetLength(SrcBytes,SizeOf(TGUID)*2);
  Move(Src,SrcBytes[4],SizeOf(TGUID));
  Dest:=TGUID.Create(GUIDToString(Src));
  EqualGUID('Check equals',Src,Dest);
end;

Function TestGUIDHelperCreateIntegerBytes : String;
// Class Function Create(A: Integer; B: SmallInt; C: SmallInt; const D: TBytes): TGUID; overload; static;

Var
  A,I : Integer;
  B,C : Smallint;
  D : TBytes;
  Dest : TGUID;

begin
  Result:='';
  A:=1;
  B:=2;
  C:=3;
  D:=Nil;
  SetLength(D,8);
  For I:=0 to 7 do
    D[i]:=4+I;
  Dest:=TGuid.Create(A,B,C,D);
  AssertEquals('D1',1,Dest.D1);
  AssertEquals('D2',2,Dest.D2);
  AssertEquals('D3',3,Dest.D3);
  For I:=0 to 7 do
    AssertEquals('D4['+IntToStr(i)+']',I+4,Dest.D4[i]);
end;

Function TestGUIDHelperCreateWords : String;
// Class Function Create(A: Cardinal; B: Word; C: Word; D, E, F, G, H, I, J, K: Byte): TGUID; overload; static;

Var
  A,I : Cardinal;
  B,C : Word;
  Dest : TGUID;

begin
  Result:='';
  A:=1;
  B:=Word($FFFE);
  C:=Word($FFFF);
  Dest:=TGuid.Create(A,B,C,4,5,6,7,8,9,10,11);
  AssertEquals('D1',1,Dest.D1);
  AssertEquals('D2',$FFFE,Dest.D2);
  AssertEquals('D3',$FFFF,Dest.D3);
  For I:=0 to 7 do
    AssertEquals('D4['+IntToStr(i)+']',I+4,Dest.D4[i]);
end;

Function TestGUIDHelperCreateInteger : String;
// Class Function Create(A: Integer; B: SmallInt; C: SmallInt; D, E, F, G, H, I, J, K: Byte): TGUID; overload; static;

Var
  A,I : Integer;
  B,C : Smallint;
  Dest : TGUID;

begin
  Result:='';
  A:=1;
  B:=Smallint($FFFE);
  C:=Smallint($FFFF);
  Dest:=TGuid.Create(A,B,C,4,5,6,7,8,9,10,11);
  AssertEquals('D1',1,Dest.D1);
  AssertEquals('D2',$FFFE,Dest.D2);
  AssertEquals('D3',$FFFF,Dest.D3);
  For I:=0 to 7 do
    AssertEquals('D4['+IntToStr(i)+']',I+4,Dest.D4[i]);
end;

Function TestGUIDHelperCreateNew : String;
// Class Function NewGuid: TGUID; static;

Var
  Src,Dest : TGuid;
  I,J : integer;

begin
  Result:='';
  // All we can do is check that you don't get the same GUID twice.
  Src:=TGuid.NewGuid;
  Dest:=TGuid.NewGuid;
  I:=0;
  Inc(I,Ord(Src.D1<>Dest.D1));
  Inc(I,Ord(Src.D2<>Dest.D2));
  Inc(I,Ord(Src.D3<>Dest.D3));
  For J:=0 to 7 do
    Inc(I,Ord(Src.D4[i]<>Dest.D4[i]));
  AssertTrue('D1<>D2',I>0);
end;

Function TestGUIDHelperToByteArray : String;

Var
  Src,Dest : TGuid;
  D : TBytes;

begin
  Result:='';
  // All we can do is check that you don't get the same GUID twice.
  Src:=TGuid.NewGuid;
  D:=Src.ToByteArray(CPUEndian);
  Dest:=TGUID.Create(D,CPUEndian);
  EqualGUID('Check equals',Src,Dest);
  if CPUEndian=TEndian.Big then
    Dest:=TGUID.Create(D,TEndian.Little)
  else
    Dest:=TGUID.Create(D,TEndian.Big);
  EqualGUIDSwap('Swapped, Check equals',Src,Dest);
end;

Function TestGUIDHelperToString: String;
// Function ToString: string;

Var
  Src : TGuid;
  S : String;
begin
  Result:='';
  CreateGUID(Src);
  S:=GuidToString(Src);
  AssertEquals('Equal',S,Src.ToString);
  Delete(S,1,1);
  Delete(S,Length(S),1);
  AssertEquals('Equal',S,Src.ToString(True));
end;

Function TestIsNanSingle : String;


var
  Value: Single;
  ExMask: TFPUExceptionMask;
  
  
begin  
  Result:='';
  ExMask := GetExceptionMask;
  try
    SetExceptionMask(ExMask + [exInvalidOp]);
    Value := Single.NaN;
    AssertEquals('Is Nan',True,Value.IsNan);
  finally
    SetExceptionMask(ExMask);
  end;  
end;    

Function TestIsNanDouble : String;


var
  Value: Double;
  ExMask: TFPUExceptionMask;
begin  
  Result:='';
  ExMask := GetExceptionMask;
  try
    SetExceptionMask(ExMask + [exInvalidOp]);
    Value := Double.NaN;
    AssertEquals('Is Nan',True,Value.IsNan);
  finally
    SetExceptionMask(ExMask);
  end;  
end;    


Function TestIsNanExtended : String;


var
  Value: Extended;
  ExMask: TFPUExceptionMask;
begin  
  Result:='';
  ExMask := GetExceptionMask;
  try
    SetExceptionMask(ExMask + [exInvalidOp]);
    Value := Extended.NaN;
    AssertEquals('Is Nan',True,Value.IsNan);
  finally
    SetExceptionMask(ExMask);
  end;  
end;    


Function TestByteSetBit : String;
var
  Index: TByteBitIndex;
  B: Byte;
const
  Expected: array[TByteBitIndex] of byte = ($01,$03,$07,$0F,$1F,$3F,$7F,$FF);
begin
  // writeln('TestByteSetBit Start');
  B := 0;
  for Index in TByteBitIndex do
  begin
    B.SetBit(Index);
    if B <> Expected[Index] then
      Exit('Bit '+IntToStr(Index)+': expected <'+IntToStr(Expected[Index])+'> got <'+IntToStr(B)+'>');
  end;
  // writeln('TestByteSetBit: OK');
end;

function TestByteToggleBit : string;
var
  Index: TByteBitIndex;
  B: Byte;
const
  Expected: array[TByteBitIndex] of byte = ($01,$03,$07,$0F,$1F,$3F,$7F,$FF);
begin
  // writeln('TestByteToggleBit Start');
  B := 0;
  for Index in TByteBitIndex do
  begin
    B.ToggleBit(Index);
    if B <> Expected[Index] then
      Exit('Bit '+IntToStr(Index)+': expected <'+IntToStr(Expected[Index])+'> got <'+IntToStr(B)+'>');
  end;
  // writeln('TestByteToggleBit: OK');
end;

Function TestByteClearBit : string;
var
  Index: TByteBitIndex;
  B: Byte;
const
  Expected: array[TByteBitIndex] of byte = ($FE,$FD,$FB,$F7,$EF,$DF,$BF,$7F);
begin
  // writeln('TestByteClearBit Start');
  for Index in TByteBitIndex do
  begin
    B := High(Byte);
    B.ClearBit(Index);
    if B <> Expected[Index] then
      Exit('Bit '+IntToStr(Index)+': expected <'+IntToStr(Expected[Index])+'> got <'+IntToStr(B)+'>');
  end;
  // writeln('TestByteClearBit: OK');
end;

Function TestByteTestBit : string;
var
  Index: TByteBitIndex;
  B: Byte;
const
  Expected: array[TByteBitIndex] of Boolean = (True,False,True,False,True,False,True,False);
begin
  // writeln('TestByteTestBit Start');
  B := $55;
  for Index in TByteBitIndex do
  begin
    if B.TestBit(Index) <> Expected[Index] then
      Exit('Bit '+IntToStr(Index)+': expected <'+BoolToStr(Expected[Index])+'> got <'+BoolToStr(B.TestBit(Index))+'>');
  end;
  // writeln('TestByteTestBit: OK');
end;


Function TestShortIntSetBit : string;
var
  Index: TShortIntBitIndex;
  S: ShortInt;
const
  Expected: array[TByteBitIndex] of ShortInt = (
    ShortInt($01),ShortInt($03),ShortInt($07),ShortInt($0F),
    ShortInt($1F),ShortInt($3F),ShortInt($7F),ShortInt($FF));
begin
  // writeln('TestShortIntSetBit Start');
  S := 0;
  for Index in TShortIntBitIndex do
  begin
    S.SetBit(Index);
    if S <> Expected[Index] then
      Exit('Bit '+IntToStr(Index)+': expected <'+IntToStr(Expected[Index])+'> got <'+IntToStr(S)+'>');
  end;
  // writeln('TestShortIntSetBit: OK');
end;

Function TestShortIntToggleBit : string;
var
  Index: TShortIntBitIndex;
  S: ShortInt;
const
  Expected: array[TByteBitIndex] of ShortInt = (
    ShortInt($01),ShortInt($03),ShortInt($07),ShortInt($0F),
    ShortInt($1F),ShortInt($3F),ShortInt($7F),ShortInt($FF));
begin
  // writeln('TestShortIntToggleBit Start');
  S := 0;
  for Index in TShortIntBitIndex do
  begin
    S.ToggleBit(Index);
    if S <> Expected[Index] then
      Exit('Bit '+IntToStr(Index)+': expected <'+IntToStr(Expected[Index])+'> got <'+IntToStr(S)+'>');
  end;
  // writeln('TestShortIntToggleBit: OK');
end;

Function TestShortIntClearBit : string;
var
  Index: TShortIntBitIndex;
  S: ShortInt;
const
  Expected: array[TByteBitIndex] of ShortInt = (
    ShortInt($FE),ShortInt($FD),ShortInt($FB),ShortInt($F7),
    ShortInt($EF),ShortInt($DF),ShortInt($BF),ShortInt($7F));
begin
  // writeln('TestShortIntClearBit Start');
  for Index in TShortIntBitIndex do
  begin
    S := ShortInt($FF);
    S.ClearBit(Index);// was Togglebit ?
    if S <> Expected[Index] then
      Exit('Bit '+IntToStr(Index)+': expected <'+IntToStr(Expected[Index])+'> got <'+IntToStr(S)+'>');
  end;
  // writeln('TestShortIntClearBit: OK');
end;

Function TestShortIntTestBit : string;
var
  Index: TShortIntBitIndex;
  S: ShortInt;
const
  Expected: array[TByteBitIndex] of Boolean = (True,False,True,False,True,False,True,False);
begin
  // writeln('TestShortIntTestBit Start');
  S := ShortInt($55);
  for Index in TShortIntBitIndex do
  begin
    if S.TestBit(Index) <> Expected[Index] then
      Exit('Bit '+IntToStr(Index)+': expected <'+BoolToStr(Expected[Index])+'> got <'+BoolToStr(S.TestBit(Index))+'>');
  end;
  // writeln('TestShortIntTestBit: OK');
end;


function TestWordSetBit : string;
var
  Index: TWordBitIndex;
  W: Word;
const
  Expected: array[TWordBitIndex] of Word = (
    $0001,$0003,$0007,$000F,$001F,$003F,$007F,$00FF,
    $01FF,$03FF,$07FF,$0FFF,$1FFF,$3FFF,$7FFF,$FFFF);
begin
  // writeln('TestWordSetBit Start');
  W := 0;
  for Index in TWordBitIndex do
  begin
    W.SetBit(Index);
    if W <> Expected[Index] then
      Exit('Bit '+IntToStr(Index)+': expected <'+IntToStr(Expected[Index])+'> got <'+IntToStr(W)+'>');
  end;
  // writeln('TestWordSetBit: OK');
end;


Function TestWordToggleBit : string;
var
  Index: TWordBitIndex;
  W: Word;
const
  Expected: array[TWordBitIndex] of Word = (
    $0001,$0003,$0007,$000F,$001F,$003F,$007F,$00FF,
    $01FF,$03FF,$07FF,$0FFF,$1FFF,$3FFF,$7FFF,$FFFF);
begin
  // writeln('TestWordToggleBit Start');
  W := 0;
  for Index in TWordBitIndex do
  begin
    W.ToggleBit(Index);
    if W <> Expected[Index] then
      Exit('Bit '+IntToStr(Index)+': expected <'+IntToStr(Expected[Index])+'> got <'+IntToStr(W)+'>');
  end;
  // writeln('TestWordToggleBit: OK');
end;


Function TestWordClearBit : String;
var
  Index: TWordBitIndex;
  W: Word;
const
  Expected: array[TWordBitIndex] of Word = (
    $FFFE,$FFFD,$FFFB,$FFF7,$FFEF,$FFDF,$FFBF,$FF7F,
    $FEFF,$FDFF,$FBFF,$F7FF,$EFFF,$DFFF,$BFFF,$7FFF);
begin
  // writeln('TestWordClearBit Start');
  for Index in TWordBitIndex do
  begin
    W := High(Word);
    W.ClearBit(Index);
    if W <> Expected[Index] then
      Exit('Bit '+IntToStr(Index)+': expected <'+IntToStr(Expected[Index])+'> got <'+IntToStr(W)+'>');
  end;
  // writeln('TestWordClearBit: OK');
end;

Function TestWordTestBit : string;
var
  Index: TWordBitIndex;
  W: Word;
const
  Expected: array[TWordBitIndex] of Boolean = (True,False,True,False,True,False,True,False,
                                               True,False,True,False,True,False,True,False);
begin
  // writeln('TestWordTestBit Start');
  W := $5555;
  for Index in TWordBitIndex do
  begin
    if W.TestBit(Index) <> Expected[Index] then
      Exit('Bit '+IntToStr(Index)+': expected <'+BoolToStr(Expected[Index])+'> got <'+BoolToStr(W.TestBit(Index))+'>');
  end;
  // writeln('TestWordTestBit: OK');
end;


Function TestSmallIntSetBit : String;
var
  Index: TSmallIntBitIndex;
  S: SmallInt;
const
  Expected: array[TSmallIntBitIndex] of SmallInt = (
    SmallInt($0001),SmallInt($0003),SmallInt($0007),SmallInt($000F),
    SmallInt($001F),SmallInt($003F),SmallInt($007F),SmallInt($00FF),
    SmallInt($01FF),SmallInt($03FF),SmallInt($07FF),SmallInt($0FFF),
    SmallInt($1FFF),SmallInt($3FFF),SmallInt($7FFF),SmallInt($FFFF));
begin
  // writeln('TestSmallIntSetBit Start');
  S := 0;
  for Index in TSmallIntBitIndex do
  begin
    S.SetBit(Index);
    if S <> Expected[Index] then
      Exit('Bit '+IntToStr(Index)+': expected <'+IntToStr(Expected[Index])+'> got <'+IntToStr(S)+'>');
  end;
  // writeln('TestSmallIntSetBit: OK');
end;


Function TestSmallIntToggleBit : String;
var
  Index: TSmallIntBitIndex;
  S: SmallInt;
const
  Expected: array[TSmallIntBitIndex] of SmallInt = (
    SmallInt($0001),SmallInt($0003),SmallInt($0007),SmallInt($000F),
    SmallInt($001F),SmallInt($003F),SmallInt($007F),SmallInt($00FF),
    SmallInt($01FF),SmallInt($03FF),SmallInt($07FF),SmallInt($0FFF),
    SmallInt($1FFF),SmallInt($3FFF),SmallInt($7FFF),SmallInt($FFFF));
begin
  // writeln('TestSmallIntToggleBit Start');
  S := 0;
  for Index in TSmallIntBitIndex do
  begin
    S.ToggleBit(Index);
    if S <> Expected[Index] then
      Exit('Bit '+IntToStr(Index)+': expected <'+IntToStr(Expected[Index])+'> got <'+IntToStr(S)+'>');
  end;
  // writeln('TestSmallIntToggleBit: OK');
end;


Function TestSmallIntClearBit : string;
var
  Index: TSmallIntBitIndex;
  S: SmallInt;
const
  Expected: array[TSmallIntBitIndex] of SmallInt = (
    SmallInt($FFFE),SmallInt($FFFD),SmallInt($FFFB),SmallInt($FFF7),
    SmallInt($FFEF),SmallInt($FFDF),SmallInt($FFBF),SmallInt($FF7F),
    SmallInt($FEFF),SmallInt($FDFF),SmallInt($FBFF),SmallInt($F7FF),
    SmallInt($EFFF),SmallInt($DFFF),SmallInt($BFFF),SmallInt($7FFF));
begin
  // writeln('TestSmallIntClearBit Start');
  for Index in TSmallIntBitIndex do
  begin
    S := SmallInt($FFFF);
    S.ClearBit(Index);
    if S <> Expected[Index] then
      Exit('Bit '+IntToStr(Index)+': expected <'+IntToStr(Expected[Index])+'> got <'+IntToStr(S)+'>');
  end;
  // writeln('TestSmallIntClearBit: OK');
end;


Function TestSmallIntTestBit : string;
var
  Index: TSmallIntBitIndex;
  S: SmallInt;
const
  Expected: array[TSmallIntBitIndex] of Boolean = (True,False,True,False,True,False,True,False,
                                                   True,False,True,False,True,False,True,False);
begin
  // writeln('TestSmallIntTestBit Start');
  S := SMallInt($5555);
  for Index in TSmallIntBitIndex do
  begin
    if S.TestBit(Index) <> Expected[Index] then
      Exit('Bit '+IntToStr(Index)+': expected <'+BoolToStr(Expected[Index])+'> got <'+BoolToStr(S.TestBit(Index))+'>');
  end;
  // writeln('TestSmallIntTestBit: OK');
end;


Function TestCardinalSetBit : string;
var
  Index: TCardinalBitIndex;
  C: Cardinal;
const
  Expected: array[TCardinalBitIndex] of Cardinal = (
    $00000001,$00000003,$00000007,$0000000F,
    $0000001F,$0000003F,$0000007F,$000000FF,
    $000001FF,$000003FF,$000007FF,$00000FFF,
    $00001FFF,$00003FFF,$00007FFF,$0000FFFF,
    $0001FFFF,$0003FFFF,$0007FFFF,$000FFFFF,
    $001FFFFF,$003FFFFF,$007FFFFF,$00FFFFFF,
    $01FFFFFF,$03FFFFFF,$07FFFFFF,$0FFFFFFF,
    $1FFFFFFF,$3FFFFFFF,$7FFFFFFF,$FFFFFFFF);
begin
  // writeln('TestCardinalSetBit Start');
  C := 0;
  for Index in TCardinalBitIndex do
  begin
    C.SetBit(Index);
    if C <> Expected[Index] then
      Exit('Bit '+IntToStr(Index)+': expected <'+IntToStr(Expected[Index])+'> got <'+IntToStr(C)+'>');
  end;
  // writeln('TestCardinalSetBit: OK');
end;


Function TestCardinalToggleBit : string;
var
  Index: TCardinalBitIndex;
  C: Cardinal;
const
  Expected: array[TCardinalBitIndex] of Cardinal = (
    $00000001,$00000003,$00000007,$0000000F,
    $0000001F,$0000003F,$0000007F,$000000FF,
    $000001FF,$000003FF,$000007FF,$00000FFF,
    $00001FFF,$00003FFF,$00007FFF,$0000FFFF,
    $0001FFFF,$0003FFFF,$0007FFFF,$000FFFFF,
    $001FFFFF,$003FFFFF,$007FFFFF,$00FFFFFF,
    $01FFFFFF,$03FFFFFF,$07FFFFFF,$0FFFFFFF,
    $1FFFFFFF,$3FFFFFFF,$7FFFFFFF,$FFFFFFFF);
begin
  // writeln('TestCardinalToggleBit Start');
  C := 0;
  for Index in TCardinalBitIndex do
  begin
    C.ToggleBit(Index);
    if C <> Expected[Index] then
      Exit('Bit '+IntToStr(Index)+': expected <'+IntToStr(Expected[Index])+'> got <'+IntToStr(C)+'>');
  end;
  // writeln('TestCardinalToggleBit: OK');
end;


Function TestCardinalClearBit : string;
var
  Index: TCardinalBitIndex;
  C: Cardinal;
const
  Expected: array[TCardinalBitIndex] of Cardinal = (
    $FFFFFFFE,$FFFFFFFD,$FFFFFFFB,$FFFFFFF7,
    $FFFFFFEF,$FFFFFFDF,$FFFFFFBF,$FFFFFF7F,
    $FFFFFEFF,$FFFFFDFF,$FFFFFBFF,$FFFFF7FF,
    $FFFFEFFF,$FFFFDFFF,$FFFFBFFF,$FFFF7FFF,
    $FFFEFFFF,$FFFDFFFF,$FFFBFFFF,$FFF7FFFF,
    $FFEFFFFF,$FFDFFFFF,$FFBFFFFF,$FF7FFFFF,
    $FEFFFFFF,$FDFFFFFF,$FBFFFFFF,$F7FFFFFF,
    $EFFFFFFF,$DFFFFFFF,$BFFFFFFF,$7FFFFFFF);
begin
  // writeln('TestCardinalClearBit Start');
  for Index in TCardinalBitIndex do
  begin
    C := High(Cardinal);
    C.ClearBit(Index);
    if C <> Expected[Index] then
      Exit('Bit '+IntToStr(Index)+': expected <'+IntToStr(Expected[Index])+'> got <'+IntToStr(C)+'>');
  end;
  // writeln('TestCardinalClearBit: OK');
end;

Function TestCardinalTestBit : string;
var
  Index: TCardinalBitIndex;
  C: Cardinal;
const
  Expected: array[TCardinalBitIndex] of Boolean = (
                                               True,False,True,False,True,False,True,False,
                                               True,False,True,False,True,False,True,False,
                                               True,False,True,False,True,False,True,False,
                                               True,False,True,False,True,False,True,False);
begin
  // writeln('TestCardinalTestBit Start');
  C := $55555555;
  for Index in TCardinalBitIndex do
  begin
    if C.TestBit(Index) <> Expected[Index] then
      Exit('Bit '+IntToStr(Index)+': expected <'+BoolToStr(Expected[Index])+'> got <'+BoolToStr(C.TestBit(Index))+'>');
  end;
  // writeln('TestCardinalTestBit: OK');
end;


Function TestLongintSetBit : string;

var
  Index: TLongintBitIndex;
  L: Longint;

const
  Expected: array[TLongintBitIndex] of Longint = (
    Longint($00000001),Longint($00000003),Longint($00000007),Longint($0000000F),
    Longint($0000001F),Longint($0000003F),Longint($0000007F),Longint($000000FF),
    Longint($000001FF),Longint($000003FF),Longint($000007FF),Longint($00000FFF),
    Longint($00001FFF),Longint($00003FFF),Longint($00007FFF),Longint($0000FFFF),
    Longint($0001FFFF),Longint($0003FFFF),Longint($0007FFFF),Longint($000FFFFF),
    Longint($001FFFFF),Longint($003FFFFF),Longint($007FFFFF),Longint($00FFFFFF),
    Longint($01FFFFFF),Longint($03FFFFFF),Longint($07FFFFFF),Longint($0FFFFFFF),
    Longint($1FFFFFFF),Longint($3FFFFFFF),Longint($7FFFFFFF),Longint($FFFFFFFF));

begin
  // writeln('TestLongintSetBit Start');
  L := 0;
  for Index in TLongintBitIndex do
  begin
    L.SetBit(Index);
    if L <> Expected[Index] then
      Exit('Bit '+IntToStr(Index)+': expected <'+IntToStr(Expected[Index])+'> got <'+IntToStr(L)+'>');
  end;
  // writeln('TestLongintSetBit: OK');
end;


Function TestLongintToggleBit : string;
var
  Index: TLongintBitIndex;
  L: Longint;
const
  Expected: array[TLongintBitIndex] of Longint = (
    Longint($00000001),Longint($00000003),Longint($00000007),Longint($0000000F),
    Longint($0000001F),Longint($0000003F),Longint($0000007F),Longint($000000FF),
    Longint($000001FF),Longint($000003FF),Longint($000007FF),Longint($00000FFF),
    Longint($00001FFF),Longint($00003FFF),Longint($00007FFF),Longint($0000FFFF),
    Longint($0001FFFF),Longint($0003FFFF),Longint($0007FFFF),Longint($000FFFFF),
    Longint($001FFFFF),Longint($003FFFFF),Longint($007FFFFF),Longint($00FFFFFF),
    Longint($01FFFFFF),Longint($03FFFFFF),Longint($07FFFFFF),Longint($0FFFFFFF),
    Longint($1FFFFFFF),Longint($3FFFFFFF),Longint($7FFFFFFF),Longint($FFFFFFFF));

begin
  // writeln('TestLongintToggleBit Start');
  L := 0;
  for Index in TLongintBitIndex do
  begin
    L.ToggleBit(Index);
    if L <> Expected[Index] then
      Exit('Bit '+IntToStr(Index)+': expected <'+IntToStr(Expected[Index])+'> got <'+IntToStr(L)+'>');
  end;
  // writeln('TestLongintToggleBit: OK');
end;


Function TestLongintClearBit : string;
var
  Index: TLongintBitIndex;
  L: Longint;
const
  Expected: array[TLongintBitIndex] of Longint = (
    Longint($FFFFFFFE),Longint($FFFFFFFD),Longint($FFFFFFFB),Longint($FFFFFFF7),
    Longint($FFFFFFEF),Longint($FFFFFFDF),Longint($FFFFFFBF),Longint($FFFFFF7F),
    Longint($FFFFFEFF),Longint($FFFFFDFF),Longint($FFFFFBFF),Longint($FFFFF7FF),
    Longint($FFFFEFFF),Longint($FFFFDFFF),Longint($FFFFBFFF),Longint($FFFF7FFF),
    Longint($FFFEFFFF),Longint($FFFDFFFF),Longint($FFFBFFFF),Longint($FFF7FFFF),
    Longint($FFEFFFFF),Longint($FFDFFFFF),Longint($FFBFFFFF),Longint($FF7FFFFF),
    Longint($FEFFFFFF),Longint($FDFFFFFF),Longint($FBFFFFFF),Longint($F7FFFFFF),
    Longint($EFFFFFFF),Longint($DFFFFFFF),Longint($BFFFFFFF),Longint($7FFFFFFF));

begin
  // writeln('TestLongintClearBit Start');
  for Index in TLongintBitIndex do
  begin
    L := Longint($FFFFFFFF);
    L.ClearBit(Index);
    if L <> Expected[Index] then
      Exit('Bit '+IntToStr(Index)+': expected <'+IntToStr(Expected[Index])+'> got <'+IntToStr(L)+'>');
  end;
  // writeln('TestLongintClearBit: OK');
end;


Function TestLongintTestBit : string;

var
  Index: TLongintBitIndex;
  L: Longint;
const
  Expected: array[TLongintBitIndex] of Boolean = (
                                               True,False,True,False,True,False,True,False,
                                               True,False,True,False,True,False,True,False,
                                               True,False,True,False,True,False,True,False,
                                               True,False,True,False,True,False,True,False);
begin
  // writeln('TestLongintTestBit Start');
  L := Longint($55555555);
  for Index in TLongintBitIndex do
  begin
    if L.TestBit(Index) <> Expected[Index] then
      Exit('Bit '+IntToStr(Index)+': expected <'+BoolToStr(Expected[Index])+'> got <'+BoolToStr(L.TestBit(Index))+'>');
  end;
  // writeln('TestLongintTestBit: OK');
end;



Function TestQWordSetBit : string;
var
  Index: TQWordBitIndex;
  Q: QWord;
const
  Expected: array[TQWordBitIndex] of QWord = (
    $0000000000000001,$0000000000000003,$0000000000000007,$000000000000000F,
    $000000000000001F,$000000000000003F,$000000000000007F,$00000000000000FF,
    $00000000000001FF,$00000000000003FF,$00000000000007FF,$0000000000000FFF,
    $0000000000001FFF,$0000000000003FFF,$0000000000007FFF,$000000000000FFFF,
    $000000000001FFFF,$000000000003FFFF,$000000000007FFFF,$00000000000FFFFF,
    $00000000001FFFFF,$00000000003FFFFF,$00000000007FFFFF,$0000000000FFFFFF,
    $0000000001FFFFFF,$0000000003FFFFFF,$0000000007FFFFFF,$000000000FFFFFFF,
    $000000001FFFFFFF,$000000003FFFFFFF,$000000007FFFFFFF,$00000000FFFFFFFF,
    $00000001FFFFFFFF,$00000003FFFFFFFF,$00000007FFFFFFFF,$0000000FFFFFFFFF,
    $0000001FFFFFFFFF,$0000003FFFFFFFFF,$0000007FFFFFFFFF,$000000FFFFFFFFFF,
    $000001FFFFFFFFFF,$000003FFFFFFFFFF,$000007FFFFFFFFFF,$00000FFFFFFFFFFF,
    $00001FFFFFFFFFFF,$00003FFFFFFFFFFF,$00007FFFFFFFFFFF,$0000FFFFFFFFFFFF,
    $0001FFFFFFFFFFFF,$0003FFFFFFFFFFFF,$0007FFFFFFFFFFFF,$000FFFFFFFFFFFFF,
    $001FFFFFFFFFFFFF,$003FFFFFFFFFFFFF,$007FFFFFFFFFFFFF,$00FFFFFFFFFFFFFF,
    $01FFFFFFFFFFFFFF,$03FFFFFFFFFFFFFF,$07FFFFFFFFFFFFFF,$0FFFFFFFFFFFFFFF,
    $1FFFFFFFFFFFFFFF,$3FFFFFFFFFFFFFFF,$7FFFFFFFFFFFFFFF,QWORD($FFFFFFFFFFFFFFFF));
begin
  // writeln('TestQWordSetBit Start');
  Q := 0;
  for Index in TQWordBitIndex do
  begin
    Q.SetBit(Index);
    if Q <> Expected[Index] then
      Exit('Bit '+IntToStr(Index)+': expected <'+IntToStr(Expected[Index])+'> got <'+IntToStr(Q)+'>');
  end;
  // writeln('TestQWordSetBit: OK');
end;


Function TestQWordToggleBit : string;
var
  Index: TQWordBitIndex;
  Q: QWord;
const
  Expected: array[TQWordBitIndex] of QWord = (
    $0000000000000001,$0000000000000003,$0000000000000007,$000000000000000F,
    $000000000000001F,$000000000000003F,$000000000000007F,$00000000000000FF,
    $00000000000001FF,$00000000000003FF,$00000000000007FF,$0000000000000FFF,
    $0000000000001FFF,$0000000000003FFF,$0000000000007FFF,$000000000000FFFF,
    $000000000001FFFF,$000000000003FFFF,$000000000007FFFF,$00000000000FFFFF,
    $00000000001FFFFF,$00000000003FFFFF,$00000000007FFFFF,$0000000000FFFFFF,
    $0000000001FFFFFF,$0000000003FFFFFF,$0000000007FFFFFF,$000000000FFFFFFF,
    $000000001FFFFFFF,$000000003FFFFFFF,$000000007FFFFFFF,$00000000FFFFFFFF,
    $00000001FFFFFFFF,$00000003FFFFFFFF,$00000007FFFFFFFF,$0000000FFFFFFFFF,
    $0000001FFFFFFFFF,$0000003FFFFFFFFF,$0000007FFFFFFFFF,$000000FFFFFFFFFF,
    $000001FFFFFFFFFF,$000003FFFFFFFFFF,$000007FFFFFFFFFF,$00000FFFFFFFFFFF,
    $00001FFFFFFFFFFF,$00003FFFFFFFFFFF,$00007FFFFFFFFFFF,$0000FFFFFFFFFFFF,
    $0001FFFFFFFFFFFF,$0003FFFFFFFFFFFF,$0007FFFFFFFFFFFF,$000FFFFFFFFFFFFF,
    $001FFFFFFFFFFFFF,$003FFFFFFFFFFFFF,$007FFFFFFFFFFFFF,$00FFFFFFFFFFFFFF,
    $01FFFFFFFFFFFFFF,$03FFFFFFFFFFFFFF,$07FFFFFFFFFFFFFF,$0FFFFFFFFFFFFFFF,
    $1FFFFFFFFFFFFFFF,$3FFFFFFFFFFFFFFF,$7FFFFFFFFFFFFFFF,QWORD($FFFFFFFFFFFFFFFF));
begin
  // writeln('TestQWordToggleBit Start');
  Q := 0;
  for Index in TQWordBitIndex do
  begin
    Q.ToggleBit(Index);
    if Q <> Expected[Index] then
      Exit('Bit '+IntToStr(Index)+': expected <'+IntToStr(Expected[Index])+'> got <'+IntToStr(Q)+'>');
  end;
  // writeln('TestQWordToggleBit: OK');
end;


Function TestQWordClearBit : string;
var
  Index: TQWordBitIndex;
  Q: QWord;
const
  Expected: array[TQWordBitIndex] of QWord = (
    QWord($FFFFFFFFFFFFFFFE),QWord($FFFFFFFFFFFFFFFD),QWord($FFFFFFFFFFFFFFFB),QWord($FFFFFFFFFFFFFFF7),
    QWord($FFFFFFFFFFFFFFEF),QWord($FFFFFFFFFFFFFFDF),QWord($FFFFFFFFFFFFFFBF),QWord($FFFFFFFFFFFFFF7F),
    QWord($FFFFFFFFFFFFFEFF),QWord($FFFFFFFFFFFFFDFF),QWord($FFFFFFFFFFFFFBFF),QWord($FFFFFFFFFFFFF7FF),
    QWord($FFFFFFFFFFFFEFFF),QWord($FFFFFFFFFFFFDFFF),QWord($FFFFFFFFFFFFBFFF),QWord($FFFFFFFFFFFF7FFF),
    QWord($FFFFFFFFFFFEFFFF),QWord($FFFFFFFFFFFDFFFF),QWord($FFFFFFFFFFFBFFFF),QWord($FFFFFFFFFFF7FFFF),
    QWord($FFFFFFFFFFEFFFFF),QWord($FFFFFFFFFFDFFFFF),QWord($FFFFFFFFFFBFFFFF),QWord($FFFFFFFFFF7FFFFF),
    QWord($FFFFFFFFFEFFFFFF),QWord($FFFFFFFFFDFFFFFF),QWord($FFFFFFFFFBFFFFFF),QWord($FFFFFFFFF7FFFFFF),
    QWord($FFFFFFFFEFFFFFFF),QWord($FFFFFFFFDFFFFFFF),QWord($FFFFFFFFBFFFFFFF),QWord($FFFFFFFF7FFFFFFF),
    QWord($FFFFFFFEFFFFFFFF),QWord($FFFFFFFDFFFFFFFF),QWord($FFFFFFFBFFFFFFFF),QWord($FFFFFFF7FFFFFFFF),
    QWord($FFFFFFEFFFFFFFFF),QWord($FFFFFFDFFFFFFFFF),QWord($FFFFFFBFFFFFFFFF),QWord($FFFFFF7FFFFFFFFF),
    QWord($FFFFFEFFFFFFFFFF),QWord($FFFFFDFFFFFFFFFF),QWord($FFFFFBFFFFFFFFFF),QWord($FFFFF7FFFFFFFFFF),
    QWord($FFFFEFFFFFFFFFFF),QWord($FFFFDFFFFFFFFFFF),QWord($FFFFBFFFFFFFFFFF),QWord($FFFF7FFFFFFFFFFF),
    QWord($FFFEFFFFFFFFFFFF),QWord($FFFDFFFFFFFFFFFF),QWord($FFFBFFFFFFFFFFFF),QWord($FFF7FFFFFFFFFFFF),
    QWord($FFEFFFFFFFFFFFFF),QWord($FFDFFFFFFFFFFFFF),QWord($FFBFFFFFFFFFFFFF),QWord($FF7FFFFFFFFFFFFF),
    QWord($FEFFFFFFFFFFFFFF),QWord($FDFFFFFFFFFFFFFF),QWord($FBFFFFFFFFFFFFFF),QWord($F7FFFFFFFFFFFFFF),
    QWord($EFFFFFFFFFFFFFFF),QWord($DFFFFFFFFFFFFFFF),QWord($BFFFFFFFFFFFFFFF),QWord($7FFFFFFFFFFFFFFF));
begin
  // writeln('TestQWordClearBit Start');
  for Index in TQWordBitIndex do
  begin
    Q := High(QWord);
    Q.ClearBit(Index);
    if Q <> Expected[Index] then
      Exit('Bit '+IntToStr(Index)+': expected <'+IntToStr(Expected[Index])+'> got <'+IntToStr(Q)+'>');
  end;
  // writeln('TestQWordClearBit: OK');
end;


Function TestQWordTestBit : string;
var
  Index: TQWordBitIndex;
  Q: QWord;
const
  Expected: array[TQWordBitIndex] of Boolean = (True,False,True,False,True,False,True,False,
                                                True,False,True,False,True,False,True,False,
                                                True,False,True,False,True,False,True,False,
                                                True,False,True,False,True,False,True,False,
                                                True,False,True,False,True,False,True,False,
                                                True,False,True,False,True,False,True,False,
                                                True,False,True,False,True,False,True,False,
                                                True,False,True,False,True,False,True,False);
begin
  // writeln('TestQWordTestBit Start');
  Q := $5555555555555555;
  for Index in TQWordBitIndex do
  begin
    if Q.TestBit(Index) <> Expected[Index] then
      Exit('Bit '+IntToStr(Index)+': expected <'+BoolToStr(Expected[Index])+'> got <'+BoolToStr(Q.TestBit(Index))+'>');
  end;
  // writeln('TestQWordTestBit: OK');
end;



Function TestInt64SetBit : string;
var
  Index: TInt64BitIndex;
  I64: Int64;
const
  Expected: array[TInt64BitIndex] of Int64 = (
    Int64($0000000000000001),Int64($0000000000000003),Int64($0000000000000007),Int64($000000000000000F),
    Int64($000000000000001F),Int64($000000000000003F),Int64($000000000000007F),Int64($00000000000000FF),
    Int64($00000000000001FF),Int64($00000000000003FF),Int64($00000000000007FF),Int64($0000000000000FFF),
    Int64($0000000000001FFF),Int64($0000000000003FFF),Int64($0000000000007FFF),Int64($000000000000FFFF),
    Int64($000000000001FFFF),Int64($000000000003FFFF),Int64($000000000007FFFF),Int64($00000000000FFFFF),
    Int64($00000000001FFFFF),Int64($00000000003FFFFF),Int64($00000000007FFFFF),Int64($0000000000FFFFFF),
    Int64($0000000001FFFFFF),Int64($0000000003FFFFFF),Int64($0000000007FFFFFF),Int64($000000000FFFFFFF),
    Int64($000000001FFFFFFF),Int64($000000003FFFFFFF),Int64($000000007FFFFFFF),Int64($00000000FFFFFFFF),
    Int64($00000001FFFFFFFF),Int64($00000003FFFFFFFF),Int64($00000007FFFFFFFF),Int64($0000000FFFFFFFFF),
    Int64($0000001FFFFFFFFF),Int64($0000003FFFFFFFFF),Int64($0000007FFFFFFFFF),Int64($000000FFFFFFFFFF),
    Int64($000001FFFFFFFFFF),Int64($000003FFFFFFFFFF),Int64($000007FFFFFFFFFF),Int64($00000FFFFFFFFFFF),
    Int64($00001FFFFFFFFFFF),Int64($00003FFFFFFFFFFF),Int64($00007FFFFFFFFFFF),Int64($0000FFFFFFFFFFFF),
    Int64($0001FFFFFFFFFFFF),Int64($0003FFFFFFFFFFFF),Int64($0007FFFFFFFFFFFF),Int64($000FFFFFFFFFFFFF),
    Int64($001FFFFFFFFFFFFF),Int64($003FFFFFFFFFFFFF),Int64($007FFFFFFFFFFFFF),Int64($00FFFFFFFFFFFFFF),
    Int64($01FFFFFFFFFFFFFF),Int64($03FFFFFFFFFFFFFF),Int64($07FFFFFFFFFFFFFF),Int64($0FFFFFFFFFFFFFFF),
    Int64($1FFFFFFFFFFFFFFF),Int64($3FFFFFFFFFFFFFFF),Int64($7FFFFFFFFFFFFFFF),Int64($FFFFFFFFFFFFFFFF));
begin
  // writeln('TestInt64SetBit Start');
  I64 := 0;
  for Index in TInt64BitIndex do
  begin
    I64.SetBit(Index);
    if I64 <> Expected[Index] then
      Exit('Bit '+IntToStr(Index)+': expected <'+IntToStr(Expected[Index])+'> got <'+IntToStr(I64)+'>');
  end;
  // writeln('TestInt64SetBit: OK');
end;


Function TestInt64ToggleBit : string;
var
  Index: TInt64BitIndex;
  I64: Int64;
const
  Expected: array[TInt64BitIndex] of Int64 = (
  Int64($0000000000000001),Int64($0000000000000003),Int64($0000000000000007),Int64($000000000000000F),
  Int64($000000000000001F),Int64($000000000000003F),Int64($000000000000007F),Int64($00000000000000FF),
  Int64($00000000000001FF),Int64($00000000000003FF),Int64($00000000000007FF),Int64($0000000000000FFF),
  Int64($0000000000001FFF),Int64($0000000000003FFF),Int64($0000000000007FFF),Int64($000000000000FFFF),
  Int64($000000000001FFFF),Int64($000000000003FFFF),Int64($000000000007FFFF),Int64($00000000000FFFFF),
  Int64($00000000001FFFFF),Int64($00000000003FFFFF),Int64($00000000007FFFFF),Int64($0000000000FFFFFF),
  Int64($0000000001FFFFFF),Int64($0000000003FFFFFF),Int64($0000000007FFFFFF),Int64($000000000FFFFFFF),
  Int64($000000001FFFFFFF),Int64($000000003FFFFFFF),Int64($000000007FFFFFFF),Int64($00000000FFFFFFFF),
  Int64($00000001FFFFFFFF),Int64($00000003FFFFFFFF),Int64($00000007FFFFFFFF),Int64($0000000FFFFFFFFF),
  Int64($0000001FFFFFFFFF),Int64($0000003FFFFFFFFF),Int64($0000007FFFFFFFFF),Int64($000000FFFFFFFFFF),
  Int64($000001FFFFFFFFFF),Int64($000003FFFFFFFFFF),Int64($000007FFFFFFFFFF),Int64($00000FFFFFFFFFFF),
  Int64($00001FFFFFFFFFFF),Int64($00003FFFFFFFFFFF),Int64($00007FFFFFFFFFFF),Int64($0000FFFFFFFFFFFF),
  Int64($0001FFFFFFFFFFFF),Int64($0003FFFFFFFFFFFF),Int64($0007FFFFFFFFFFFF),Int64($000FFFFFFFFFFFFF),
  Int64($001FFFFFFFFFFFFF),Int64($003FFFFFFFFFFFFF),Int64($007FFFFFFFFFFFFF),Int64($00FFFFFFFFFFFFFF),
  Int64($01FFFFFFFFFFFFFF),Int64($03FFFFFFFFFFFFFF),Int64($07FFFFFFFFFFFFFF),Int64($0FFFFFFFFFFFFFFF),
  Int64($1FFFFFFFFFFFFFFF),Int64($3FFFFFFFFFFFFFFF),Int64($7FFFFFFFFFFFFFFF),Int64($FFFFFFFFFFFFFFFF));
begin
  // writeln('TestInt64ToggleBit Start');
  I64 := 0;
  for Index in TInt64BitIndex do
  begin
    I64.ToggleBit(Index);
    if I64 <> Expected[Index] then
      Exit('Bit '+IntToStr(Index)+': expected <'+IntToStr(Expected[Index])+'> got <'+IntToStr(I64)+'>');
  end;
  // writeln('TestInt64ToggleBit: OK');
end;


Function TestInt64ClearBit : string;
var
  Index: TInt64BitIndex;
  I64: Int64;
const
  Expected: array[TInt64BitIndex] of Int64 = (
    Int64($FFFFFFFFFFFFFFFE),Int64($FFFFFFFFFFFFFFFD),Int64($FFFFFFFFFFFFFFFB),Int64($FFFFFFFFFFFFFFF7),
    Int64($FFFFFFFFFFFFFFEF),Int64($FFFFFFFFFFFFFFDF),Int64($FFFFFFFFFFFFFFBF),Int64($FFFFFFFFFFFFFF7F),
    Int64($FFFFFFFFFFFFFEFF),Int64($FFFFFFFFFFFFFDFF),Int64($FFFFFFFFFFFFFBFF),Int64($FFFFFFFFFFFFF7FF),
    Int64($FFFFFFFFFFFFEFFF),Int64($FFFFFFFFFFFFDFFF),Int64($FFFFFFFFFFFFBFFF),Int64($FFFFFFFFFFFF7FFF),
    Int64($FFFFFFFFFFFEFFFF),Int64($FFFFFFFFFFFDFFFF),Int64($FFFFFFFFFFFBFFFF),Int64($FFFFFFFFFFF7FFFF),
    Int64($FFFFFFFFFFEFFFFF),Int64($FFFFFFFFFFDFFFFF),Int64($FFFFFFFFFFBFFFFF),Int64($FFFFFFFFFF7FFFFF),
    Int64($FFFFFFFFFEFFFFFF),Int64($FFFFFFFFFDFFFFFF),Int64($FFFFFFFFFBFFFFFF),Int64($FFFFFFFFF7FFFFFF),
    Int64($FFFFFFFFEFFFFFFF),Int64($FFFFFFFFDFFFFFFF),Int64($FFFFFFFFBFFFFFFF),Int64($FFFFFFFF7FFFFFFF),
    Int64($FFFFFFFEFFFFFFFF),Int64($FFFFFFFDFFFFFFFF),Int64($FFFFFFFBFFFFFFFF),Int64($FFFFFFF7FFFFFFFF),
    Int64($FFFFFFEFFFFFFFFF),Int64($FFFFFFDFFFFFFFFF),Int64($FFFFFFBFFFFFFFFF),Int64($FFFFFF7FFFFFFFFF),
    Int64($FFFFFEFFFFFFFFFF),Int64($FFFFFDFFFFFFFFFF),Int64($FFFFFBFFFFFFFFFF),Int64($FFFFF7FFFFFFFFFF),
    Int64($FFFFEFFFFFFFFFFF),Int64($FFFFDFFFFFFFFFFF),Int64($FFFFBFFFFFFFFFFF),Int64($FFFF7FFFFFFFFFFF),
    Int64($FFFEFFFFFFFFFFFF),Int64($FFFDFFFFFFFFFFFF),Int64($FFFBFFFFFFFFFFFF),Int64($FFF7FFFFFFFFFFFF),
    Int64($FFEFFFFFFFFFFFFF),Int64($FFDFFFFFFFFFFFFF),Int64($FFBFFFFFFFFFFFFF),Int64($FF7FFFFFFFFFFFFF),
    Int64($FEFFFFFFFFFFFFFF),Int64($FDFFFFFFFFFFFFFF),Int64($FBFFFFFFFFFFFFFF),Int64($F7FFFFFFFFFFFFFF),
    Int64($EFFFFFFFFFFFFFFF),Int64($DFFFFFFFFFFFFFFF),Int64($BFFFFFFFFFFFFFFF),Int64($7FFFFFFFFFFFFFFF));
begin
  // writeln('TestInt64ClearBit Start');
  for Index in TInt64BitIndex do
  begin
    I64 := Int64($FFFFFFFFFFFFFFFF);
    I64.ClearBit(Index);
    if I64 <> Expected[Index] then
      Exit('Bit '+IntToStr(Index)+': expected <'+IntToStr(Expected[Index])+'> got <'+IntToStr(I64)+'>');
  end;
  // writeln('TestInt64ClearBit: OK');
end;


Function TestInt64TestBit : string;
var
  Index: TInt64BitIndex;
  I64: Int64;
const
  Expected: array[TInt64BitIndex] of Boolean = (True,False,True,False,True,False,True,False,
                                                True,False,True,False,True,False,True,False,
                                                True,False,True,False,True,False,True,False,
                                                True,False,True,False,True,False,True,False,
                                                True,False,True,False,True,False,True,False,
                                                True,False,True,False,True,False,True,False,
                                                True,False,True,False,True,False,True,False,
                                                True,False,True,False,True,False,True,False);
begin
  // writeln('TestInt64TestBit Start');
  I64 := Int64($5555555555555555);
  for Index in TInt64BitIndex do
  begin
    if I64.TestBit(Index) <> Expected[Index] then
      Exit('Bit '+IntToStr(Index)+': expected <'+BoolToStr(Expected[Index])+'> got <'+BoolToStr(I64.TestBit(Index))+'>');
  end;
  // writeln('TestInt64TestBit: OK');
end;

{$if SizeOf(NativeUint)=SizeOf(QWord)}
{$define IsQWord}
{$endif}
{$if SizeOf(NativeUint)=SizeOf(DWord)}
{$define IsDWord}
{$endif}
{$if SizeOf(NativeUint)=SizeOf(Word)}
{$define IsWord}
{$endif}



Function TestNativeUIntSetBit : string;
var
  Index: TNativeUIntBitIndex;
  NU: NativeUInt;
const
  {$ifdef IsQWord}
  Expected: array[TNativeUIntBitIndex] of NativeUInt = (
    $0000000000000001,$0000000000000003,$0000000000000007,$000000000000000F,
    $000000000000001F,$000000000000003F,$000000000000007F,$00000000000000FF,
    $00000000000001FF,$00000000000003FF,$00000000000007FF,$0000000000000FFF,
    $0000000000001FFF,$0000000000003FFF,$0000000000007FFF,$000000000000FFFF,
    $000000000001FFFF,$000000000003FFFF,$000000000007FFFF,$00000000000FFFFF,
    $00000000001FFFFF,$00000000003FFFFF,$00000000007FFFFF,$0000000000FFFFFF,
    $0000000001FFFFFF,$0000000003FFFFFF,$0000000007FFFFFF,$000000000FFFFFFF,
    $000000001FFFFFFF,$000000003FFFFFFF,$000000007FFFFFFF,$00000000FFFFFFFF,
    $00000001FFFFFFFF,$00000003FFFFFFFF,$00000007FFFFFFFF,$0000000FFFFFFFFF,
    $0000001FFFFFFFFF,$0000003FFFFFFFFF,$0000007FFFFFFFFF,$000000FFFFFFFFFF,
    $000001FFFFFFFFFF,$000003FFFFFFFFFF,$000007FFFFFFFFFF,$00000FFFFFFFFFFF,
    $00001FFFFFFFFFFF,$00003FFFFFFFFFFF,$00007FFFFFFFFFFF,$0000FFFFFFFFFFFF,
    $0001FFFFFFFFFFFF,$0003FFFFFFFFFFFF,$0007FFFFFFFFFFFF,$000FFFFFFFFFFFFF,
    $001FFFFFFFFFFFFF,$003FFFFFFFFFFFFF,$007FFFFFFFFFFFFF,$00FFFFFFFFFFFFFF,
    $01FFFFFFFFFFFFFF,$03FFFFFFFFFFFFFF,$07FFFFFFFFFFFFFF,$0FFFFFFFFFFFFFFF,
    $1FFFFFFFFFFFFFFF,$3FFFFFFFFFFFFFFF,$7FFFFFFFFFFFFFFF,NativeUInt($FFFFFFFFFFFFFFFF));
  {$endif}
  {$ifdef IsDWord}
  Expected: array[TNativeUIntBitIndex] of NativeUInt = (
    $00000001,$00000003,$00000007,$0000000F,
    $0000001F,$0000003F,$0000007F,$000000FF,
    $000001FF,$000003FF,$000007FF,$00000FFF,
    $00001FFF,$00003FFF,$00007FFF,$0000FFFF,
    $0001FFFF,$0003FFFF,$0007FFFF,$000FFFFF,
    $001FFFFF,$003FFFFF,$007FFFFF,$00FFFFFF,
    $01FFFFFF,$03FFFFFF,$07FFFFFF,$0FFFFFFF,
    $1FFFFFFF,$3FFFFFFF,$7FFFFFFF,$FFFFFFFF);
  {$endif}
  {$ifdef IsWord}
  Expected: array[TNativeUIntBitIndex] of NativeUInt = (
    $0001,$0003,$0007,$000F,
    $001F,$003F,$007F,$00FF,
    $01FF,$03FF,$07FF,$0FFF,
    $1FFF,$3FFF,$7FFF,$FFFF);
  {$endif}
begin
  // writeln('TestNativeUIntSetBit Start');
  NU := 0;
  for Index in TNativeUIntBitIndex do
  begin
    NU.SetBit(Index);
    if NU <> Expected[Index] then
      Exit('Bit '+IntToStr(Index)+': expected <'+IntToStr(Expected[Index])+'> got <'+IntToStr(NU)+'>');
  end;
  // writeln('TestNativeUIntSetBit: OK');
end;


Function TestNativeUIntToggleBit : string;
var
  Index: TNativeUIntBitIndex;
  NU: NativeUInt;
const
  {$ifdef IsQWord}
  Expected: array[TNativeUIntBitIndex] of NativeUInt = (
    $0000000000000001,$0000000000000003,$0000000000000007,$000000000000000F,
    $000000000000001F,$000000000000003F,$000000000000007F,$00000000000000FF,
    $00000000000001FF,$00000000000003FF,$00000000000007FF,$0000000000000FFF,
    $0000000000001FFF,$0000000000003FFF,$0000000000007FFF,$000000000000FFFF,
    $000000000001FFFF,$000000000003FFFF,$000000000007FFFF,$00000000000FFFFF,
    $00000000001FFFFF,$00000000003FFFFF,$00000000007FFFFF,$0000000000FFFFFF,
    $0000000001FFFFFF,$0000000003FFFFFF,$0000000007FFFFFF,$000000000FFFFFFF,
    $000000001FFFFFFF,$000000003FFFFFFF,$000000007FFFFFFF,$00000000FFFFFFFF,
    $00000001FFFFFFFF,$00000003FFFFFFFF,$00000007FFFFFFFF,$0000000FFFFFFFFF,
    $0000001FFFFFFFFF,$0000003FFFFFFFFF,$0000007FFFFFFFFF,$000000FFFFFFFFFF,
    $000001FFFFFFFFFF,$000003FFFFFFFFFF,$000007FFFFFFFFFF,$00000FFFFFFFFFFF,
    $00001FFFFFFFFFFF,$00003FFFFFFFFFFF,$00007FFFFFFFFFFF,$0000FFFFFFFFFFFF,
    $0001FFFFFFFFFFFF,$0003FFFFFFFFFFFF,$0007FFFFFFFFFFFF,$000FFFFFFFFFFFFF,
    $001FFFFFFFFFFFFF,$003FFFFFFFFFFFFF,$007FFFFFFFFFFFFF,$00FFFFFFFFFFFFFF,
    $01FFFFFFFFFFFFFF,$03FFFFFFFFFFFFFF,$07FFFFFFFFFFFFFF,$0FFFFFFFFFFFFFFF,
    $1FFFFFFFFFFFFFFF,$3FFFFFFFFFFFFFFF,$7FFFFFFFFFFFFFFF,NativeUInt($FFFFFFFFFFFFFFFF));
  {$endif}
  {$ifdef IsDWord}
  Expected: array[TNativeUIntBitIndex] of NativeUInt = (
    $00000001,$00000003,$00000007,$0000000F,
    $0000001F,$0000003F,$0000007F,$000000FF,
    $000001FF,$000003FF,$000007FF,$00000FFF,
    $00001FFF,$00003FFF,$00007FFF,$0000FFFF,
    $0001FFFF,$0003FFFF,$0007FFFF,$000FFFFF,
    $001FFFFF,$003FFFFF,$007FFFFF,$00FFFFFF,
    $01FFFFFF,$03FFFFFF,$07FFFFFF,$0FFFFFFF,
    $1FFFFFFF,$3FFFFFFF,$7FFFFFFF,$FFFFFFFF);
  {$endif}
  {$ifdef IsWord}
  Expected: array[TNativeUIntBitIndex] of NativeUInt = (
    $0001,$0003,$0007,$000F,
    $001F,$003F,$007F,$00FF,
    $01FF,$03FF,$07FF,$0FFF,
    $1FFF,$3FFF,$7FFF,$FFFF);
  {$endif}
begin
  // writeln('TestNativeUIntToggleBit Start');
  NU := 0;
  for Index in TNativeUIntBitIndex do
  begin
    NU.ToggleBit(Index);
    if NU <> Expected[Index] then
      Exit('Bit '+IntToStr(Index)+': expected <'+IntToStr(Expected[Index])+'> got <'+IntToStr(NU)+'>');
  end;
  // writeln('TestNativeUIntToggleBit: OK');
end;


Function TestNativeUIntClearBit : string;
var
  Index: TNativeUIntBitIndex;
  NU: NativeUInt;
const
  {$ifdef IsQWord}
  Expected: array[TNativeUIntBitIndex] of NativeUInt = (
    NativeUInt($FFFFFFFFFFFFFFFE),NativeUInt($FFFFFFFFFFFFFFFD),NativeUInt($FFFFFFFFFFFFFFFB),NativeUInt($FFFFFFFFFFFFFFF7),
    NativeUInt($FFFFFFFFFFFFFFEF),NativeUInt($FFFFFFFFFFFFFFDF),NativeUInt($FFFFFFFFFFFFFFBF),NativeUInt($FFFFFFFFFFFFFF7F),
    NativeUInt($FFFFFFFFFFFFFEFF),NativeUInt($FFFFFFFFFFFFFDFF),NativeUInt($FFFFFFFFFFFFFBFF),NativeUInt($FFFFFFFFFFFFF7FF),
    NativeUInt($FFFFFFFFFFFFEFFF),NativeUInt($FFFFFFFFFFFFDFFF),NativeUInt($FFFFFFFFFFFFBFFF),NativeUInt($FFFFFFFFFFFF7FFF),
    NativeUInt($FFFFFFFFFFFEFFFF),NativeUInt($FFFFFFFFFFFDFFFF),NativeUInt($FFFFFFFFFFFBFFFF),NativeUInt($FFFFFFFFFFF7FFFF),
    NativeUInt($FFFFFFFFFFEFFFFF),NativeUInt($FFFFFFFFFFDFFFFF),NativeUInt($FFFFFFFFFFBFFFFF),NativeUInt($FFFFFFFFFF7FFFFF),
    NativeUInt($FFFFFFFFFEFFFFFF),NativeUInt($FFFFFFFFFDFFFFFF),NativeUInt($FFFFFFFFFBFFFFFF),NativeUInt($FFFFFFFFF7FFFFFF),
    NativeUInt($FFFFFFFFEFFFFFFF),NativeUInt($FFFFFFFFDFFFFFFF),NativeUInt($FFFFFFFFBFFFFFFF),NativeUInt($FFFFFFFF7FFFFFFF),
    NativeUInt($FFFFFFFEFFFFFFFF),NativeUInt($FFFFFFFDFFFFFFFF),NativeUInt($FFFFFFFBFFFFFFFF),NativeUInt($FFFFFFF7FFFFFFFF),
    NativeUInt($FFFFFFEFFFFFFFFF),NativeUInt($FFFFFFDFFFFFFFFF),NativeUInt($FFFFFFBFFFFFFFFF),NativeUInt($FFFFFF7FFFFFFFFF),
    NativeUInt($FFFFFEFFFFFFFFFF),NativeUInt($FFFFFDFFFFFFFFFF),NativeUInt($FFFFFBFFFFFFFFFF),NativeUInt($FFFFF7FFFFFFFFFF),
    NativeUInt($FFFFEFFFFFFFFFFF),NativeUInt($FFFFDFFFFFFFFFFF),NativeUInt($FFFFBFFFFFFFFFFF),NativeUInt($FFFF7FFFFFFFFFFF),
    NativeUInt($FFFEFFFFFFFFFFFF),NativeUInt($FFFDFFFFFFFFFFFF),NativeUInt($FFFBFFFFFFFFFFFF),NativeUInt($FFF7FFFFFFFFFFFF),
    NativeUInt($FFEFFFFFFFFFFFFF),NativeUInt($FFDFFFFFFFFFFFFF),NativeUInt($FFBFFFFFFFFFFFFF),NativeUInt($FF7FFFFFFFFFFFFF),
    NativeUInt($FEFFFFFFFFFFFFFF),NativeUInt($FDFFFFFFFFFFFFFF),NativeUInt($FBFFFFFFFFFFFFFF),NativeUInt($F7FFFFFFFFFFFFFF),
    NativeUInt($EFFFFFFFFFFFFFFF),NativeUInt($DFFFFFFFFFFFFFFF),NativeUInt($BFFFFFFFFFFFFFFF),NativeUInt($7FFFFFFFFFFFFFFF));
  {$endif}
  {$ifdef IsDWord}
  Expected: array[TNativeUIntBitIndex] of NativeUInt = (
    NativeUInt($FFFFFFFE),NativeUInt($FFFFFFFD),NativeUInt($FFFFFFFB),NativeUInt($FFFFFFF7),
    NativeUInt($FFFFFFEF),NativeUInt($FFFFFFDF),NativeUInt($FFFFFFBF),NativeUInt($FFFFFF7F),
    NativeUInt($FFFFFEFF),NativeUInt($FFFFFDFF),NativeUInt($FFFFFBFF),NativeUInt($FFFFF7FF),
    NativeUInt($FFFFEFFF),NativeUInt($FFFFDFFF),NativeUInt($FFFFBFFF),NativeUInt($FFFF7FFF),
    NativeUInt($FFFEFFFF),NativeUInt($FFFDFFFF),NativeUInt($FFFBFFFF),NativeUInt($FFF7FFFF),
    NativeUInt($FFEFFFFF),NativeUInt($FFDFFFFF),NativeUInt($FFBFFFFF),NativeUInt($FF7FFFFF),
    NativeUInt($FEFFFFFF),NativeUInt($FDFFFFFF),NativeUInt($FBFFFFFF),NativeUInt($F7FFFFFF),
    NativeUInt($EFFFFFFF),NativeUInt($DFFFFFFF),NativeUInt($BFFFFFFF),NativeUInt($7FFFFFFF));
  {$endif}
  {$ifdef IsWord}
  Expected: array[TNativeUIntBitIndex] of NativeUInt = (
    NativeUInt($FFFE),NativeUInt($FFFD),NativeUInt($FFFB),NativeUInt($FFF7),
    NativeUInt($FFEF),NativeUInt($FFDF),NativeUInt($FFBF),NativeUInt($FF7F),
    NativeUInt($FEFF),NativeUInt($FDFF),NativeUInt($FBFF),NativeUInt($F7FF),
    NativeUInt($EFFF),NativeUInt($DFFF),NativeUInt($BFFF),NativeUInt($7FFF));
  {$endif}
begin
  // writeln('TestNativeUIntClearBit Start');
  for Index in TNativeUIntBitIndex do
  begin
    NU := High(NativeUInt);
    NU.ClearBit(Index);
    if NU <> Expected[Index] then
      Exit('Bit '+IntToStr(Index)+': expected <'+IntToStr(Expected[Index])+'> got <'+IntToStr(NU)+'>');
  end;
  // writeln('TestNativeUIntClearBit: OK');
end;


Function TestNativeUIntTestBit : string;
var
  Index: TNativeUIntBitIndex;
  NU: NativeUInt;
const
  {$ifdef IsQWord}
  Expected: array[TNativeUIntBitIndex] of Boolean = (True,False,True,False,True,False,True,False,
                                                True,False,True,False,True,False,True,False,
                                                True,False,True,False,True,False,True,False,
                                                True,False,True,False,True,False,True,False,
                                                True,False,True,False,True,False,True,False,
                                                True,False,True,False,True,False,True,False,
                                                True,False,True,False,True,False,True,False,
                                                True,False,True,False,True,False,True,False);
  {$endif}
  {$ifdef IsDWord}
  Expected: array[TNativeUIntBitIndex] of Boolean = (True,False,True,False,True,False,True,False,
                                                True,False,True,False,True,False,True,False,
                                                True,False,True,False,True,False,True,False,
                                                True,False,True,False,True,False,True,False);

  {$endif}
  {$ifdef IsWord}
  Expected: array[TNativeUIntBitIndex] of Boolean = (True,False,True,False,True,False,True,False,
                                                True,False,True,False,True,False,True,False);

  {$endif}
begin
  // writeln('TestNativeUIntTestBit Start');
  {$ifdef IsQWord}
  NU := $5555555555555555;
  {$endif}
  {$ifdef IsDWord}
  NU := $55555555;
  {$endif}
  {$ifdef IsWord}
  NU := $5555;
  {$endif}
  for Index in TNativeUIntBitIndex do
  begin
    if NU.TestBit(Index) <> Expected[Index] then
      Exit('Bit '+IntToStr(Index)+': expected <'+BoolToStr(Expected[Index])+'> got <'+BoolToStr(NU.TestBit(Index))+'>');
  end;
  // writeln('TestNativeUIntTestBit: OK');
end;

{$undef IsQword}
{$undef IsDword}
{$undef IsWord}

{$if SizeOf(NativeUint)=SizeOf(Int64)}
{$define IsInt64}
{$endif}
{$if SizeOf(NativeUint)=SizeOf(LongInt)}
{$define IsInt32}
{$endif}
{$if SizeOf(NativeUint)=SizeOf(SmallInt)}
{$define IsInt16}
{$endif}



Function TestNativeIntSetBit : string;
var
  Index: TNativeIntBitIndex;
  NI: NativeInt;
const
  {$ifdef IsInt64}
  Expected: array[TNativeIntBitIndex] of NativeInt = (
    $0000000000000001,$0000000000000003,$0000000000000007,$000000000000000F,
    $000000000000001F,$000000000000003F,$000000000000007F,$00000000000000FF,
    $00000000000001FF,$00000000000003FF,$00000000000007FF,$0000000000000FFF,
    $0000000000001FFF,$0000000000003FFF,$0000000000007FFF,$000000000000FFFF,
    $000000000001FFFF,$000000000003FFFF,$000000000007FFFF,$00000000000FFFFF,
    $00000000001FFFFF,$00000000003FFFFF,$00000000007FFFFF,$0000000000FFFFFF,
    $0000000001FFFFFF,$0000000003FFFFFF,$0000000007FFFFFF,$000000000FFFFFFF,
    $000000001FFFFFFF,$000000003FFFFFFF,$000000007FFFFFFF,$00000000FFFFFFFF,
    $00000001FFFFFFFF,$00000003FFFFFFFF,$00000007FFFFFFFF,$0000000FFFFFFFFF,
    $0000001FFFFFFFFF,$0000003FFFFFFFFF,$0000007FFFFFFFFF,$000000FFFFFFFFFF,
    $000001FFFFFFFFFF,$000003FFFFFFFFFF,$000007FFFFFFFFFF,$00000FFFFFFFFFFF,
    $00001FFFFFFFFFFF,$00003FFFFFFFFFFF,$00007FFFFFFFFFFF,$0000FFFFFFFFFFFF,
    $0001FFFFFFFFFFFF,$0003FFFFFFFFFFFF,$0007FFFFFFFFFFFF,$000FFFFFFFFFFFFF,
    $001FFFFFFFFFFFFF,$003FFFFFFFFFFFFF,$007FFFFFFFFFFFFF,$00FFFFFFFFFFFFFF,
    $01FFFFFFFFFFFFFF,$03FFFFFFFFFFFFFF,$07FFFFFFFFFFFFFF,$0FFFFFFFFFFFFFFF,
    $1FFFFFFFFFFFFFFF,$3FFFFFFFFFFFFFFF,$7FFFFFFFFFFFFFFF,NativeInt($FFFFFFFFFFFFFFFF));
  {$endif}
  {$ifdef IsInt32}
  Expected: array[TNativeIntBitIndex] of NativeInt = (
    $00000001,$00000003,$00000007,$0000000F,
    $0000001F,$0000003F,$0000007F,$000000FF,
    $000001FF,$000003FF,$000007FF,$00000FFF,
    $00001FFF,$00003FFF,$00007FFF,$0000FFFF,
    $0001FFFF,$0003FFFF,$0007FFFF,$000FFFFF,
    $001FFFFF,$003FFFFF,$007FFFFF,$00FFFFFF,
    $01FFFFFF,$03FFFFFF,$07FFFFFF,$0FFFFFFF,
    $1FFFFFFF,$3FFFFFFF,$7FFFFFFF,NativeInt($FFFFFFFF));
  {$endif}
  {$ifdef IsInt16}
  Expected: array[TNativeIntBitIndex] of NativeInt = (
    $0001,$0003,$0007,$000F,
    $001F,$003F,$007F,$00FF,
    $01FF,$03FF,$07FF,$0FFF,
    $1FFF,$3FFF,$7FFF,$FFFF);
  {$endif}
begin
  // writeln('TestNativeIntSetBit Start');
  NI := 0;
  for Index in TNativeIntBitIndex do
  begin
    NI.SetBit(Index);
    if NI <> Expected[Index] then
      Exit('Bit '+IntToStr(Index)+': expected <'+IntToStr(Expected[Index])+'> got <'+IntToStr(NI)+'>');
  end;
  // writeln('TestNativeIntSetBit: OK');
end;


Function TestNativeIntToggleBit : string;
var
  Index: TNativeIntBitIndex;
  NI: NativeInt;
const
  {$ifdef IsInt64}
  Expected: array[TNativeIntBitIndex] of NativeInt = (
    $0000000000000001,$0000000000000003,$0000000000000007,$000000000000000F,
    $000000000000001F,$000000000000003F,$000000000000007F,$00000000000000FF,
    $00000000000001FF,$00000000000003FF,$00000000000007FF,$0000000000000FFF,
    $0000000000001FFF,$0000000000003FFF,$0000000000007FFF,$000000000000FFFF,
    $000000000001FFFF,$000000000003FFFF,$000000000007FFFF,$00000000000FFFFF,
    $00000000001FFFFF,$00000000003FFFFF,$00000000007FFFFF,$0000000000FFFFFF,
    $0000000001FFFFFF,$0000000003FFFFFF,$0000000007FFFFFF,$000000000FFFFFFF,
    $000000001FFFFFFF,$000000003FFFFFFF,$000000007FFFFFFF,$00000000FFFFFFFF,
    $00000001FFFFFFFF,$00000003FFFFFFFF,$00000007FFFFFFFF,$0000000FFFFFFFFF,
    $0000001FFFFFFFFF,$0000003FFFFFFFFF,$0000007FFFFFFFFF,$000000FFFFFFFFFF,
    $000001FFFFFFFFFF,$000003FFFFFFFFFF,$000007FFFFFFFFFF,$00000FFFFFFFFFFF,
    $00001FFFFFFFFFFF,$00003FFFFFFFFFFF,$00007FFFFFFFFFFF,$0000FFFFFFFFFFFF,
    $0001FFFFFFFFFFFF,$0003FFFFFFFFFFFF,$0007FFFFFFFFFFFF,$000FFFFFFFFFFFFF,
    $001FFFFFFFFFFFFF,$003FFFFFFFFFFFFF,$007FFFFFFFFFFFFF,$00FFFFFFFFFFFFFF,
    $01FFFFFFFFFFFFFF,$03FFFFFFFFFFFFFF,$07FFFFFFFFFFFFFF,$0FFFFFFFFFFFFFFF,
    $1FFFFFFFFFFFFFFF,$3FFFFFFFFFFFFFFF,$7FFFFFFFFFFFFFFF,NativeInt($FFFFFFFFFFFFFFFF));
  {$endif}
  {$ifdef IsInt32}
  Expected: array[TNativeIntBitIndex] of NativeInt = (
    $00000001,$00000003,$00000007,$0000000F,
    $0000001F,$0000003F,$0000007F,$000000FF,
    $000001FF,$000003FF,$000007FF,$00000FFF,
    $00001FFF,$00003FFF,$00007FFF,$0000FFFF,
    $0001FFFF,$0003FFFF,$0007FFFF,$000FFFFF,
    $001FFFFF,$003FFFFF,$007FFFFF,$00FFFFFF,
    $01FFFFFF,$03FFFFFF,$07FFFFFF,$0FFFFFFF,
    $1FFFFFFF,$3FFFFFFF,$7FFFFFFF,NativeInt($FFFFFFFF));
  {$endif}
  {$ifdef IsInt16}
  Expected: array[TNativeIntBitIndex] of NativeInt = (
    $0001,$0003,$0007,$000F,
    $001F,$003F,$007F,$00FF,
    $01FF,$03FF,$07FF,$0FFF,
    $1FFF,$3FFF,$7FFF,$FFFF);
  {$endif}
begin
  // writeln('TestNativeIntToggleBit: OK');
  NI := 0;
  for Index in TNativeIntBitIndex do
  begin
    NI.ToggleBit(Index);
    if NI <> Expected[Index] then
      Exit('Bit '+IntToStr(Index)+': expected <'+IntToStr(Expected[Index])+'> got <'+IntToStr(NI)+'>');
  end;
  // writeln('TestNativeIntToggleBit: OK');
end;


Function TestNativeIntClearBit : string;
var
  Index: TNativeIntBitIndex;
  NI: NativeInt;
const
  {$ifdef IsInt64}
  Expected: array[TNativeIntBitIndex] of NativeInt = (
    NativeInt($FFFFFFFFFFFFFFFE),NativeInt($FFFFFFFFFFFFFFFD),NativeInt($FFFFFFFFFFFFFFFB),NativeInt($FFFFFFFFFFFFFFF7),
    NativeInt($FFFFFFFFFFFFFFEF),NativeInt($FFFFFFFFFFFFFFDF),NativeInt($FFFFFFFFFFFFFFBF),NativeInt($FFFFFFFFFFFFFF7F),
    NativeInt($FFFFFFFFFFFFFEFF),NativeInt($FFFFFFFFFFFFFDFF),NativeInt($FFFFFFFFFFFFFBFF),NativeInt($FFFFFFFFFFFFF7FF),
    NativeInt($FFFFFFFFFFFFEFFF),NativeInt($FFFFFFFFFFFFDFFF),NativeInt($FFFFFFFFFFFFBFFF),NativeInt($FFFFFFFFFFFF7FFF),
    NativeInt($FFFFFFFFFFFEFFFF),NativeInt($FFFFFFFFFFFDFFFF),NativeInt($FFFFFFFFFFFBFFFF),NativeInt($FFFFFFFFFFF7FFFF),
    NativeInt($FFFFFFFFFFEFFFFF),NativeInt($FFFFFFFFFFDFFFFF),NativeInt($FFFFFFFFFFBFFFFF),NativeInt($FFFFFFFFFF7FFFFF),
    NativeInt($FFFFFFFFFEFFFFFF),NativeInt($FFFFFFFFFDFFFFFF),NativeInt($FFFFFFFFFBFFFFFF),NativeInt($FFFFFFFFF7FFFFFF),
    NativeInt($FFFFFFFFEFFFFFFF),NativeInt($FFFFFFFFDFFFFFFF),NativeInt($FFFFFFFFBFFFFFFF),NativeInt($FFFFFFFF7FFFFFFF),
    NativeInt($FFFFFFFEFFFFFFFF),NativeInt($FFFFFFFDFFFFFFFF),NativeInt($FFFFFFFBFFFFFFFF),NativeInt($FFFFFFF7FFFFFFFF),
    NativeInt($FFFFFFEFFFFFFFFF),NativeInt($FFFFFFDFFFFFFFFF),NativeInt($FFFFFFBFFFFFFFFF),NativeInt($FFFFFF7FFFFFFFFF),
    NativeInt($FFFFFEFFFFFFFFFF),NativeInt($FFFFFDFFFFFFFFFF),NativeInt($FFFFFBFFFFFFFFFF),NativeInt($FFFFF7FFFFFFFFFF),
    NativeInt($FFFFEFFFFFFFFFFF),NativeInt($FFFFDFFFFFFFFFFF),NativeInt($FFFFBFFFFFFFFFFF),NativeInt($FFFF7FFFFFFFFFFF),
    NativeInt($FFFEFFFFFFFFFFFF),NativeInt($FFFDFFFFFFFFFFFF),NativeInt($FFFBFFFFFFFFFFFF),NativeInt($FFF7FFFFFFFFFFFF),
    NativeInt($FFEFFFFFFFFFFFFF),NativeInt($FFDFFFFFFFFFFFFF),NativeInt($FFBFFFFFFFFFFFFF),NativeInt($FF7FFFFFFFFFFFFF),
    NativeInt($FEFFFFFFFFFFFFFF),NativeInt($FDFFFFFFFFFFFFFF),NativeInt($FBFFFFFFFFFFFFFF),NativeInt($F7FFFFFFFFFFFFFF),
    NativeInt($EFFFFFFFFFFFFFFF),NativeInt($DFFFFFFFFFFFFFFF),NativeInt($BFFFFFFFFFFFFFFF),NativeInt($7FFFFFFFFFFFFFFF));
  {$endif}
  {$ifdef IsInt32}
  Expected: array[TNativeIntBitIndex] of NativeInt = (
    NativeInt($FFFFFFFE),NativeInt($FFFFFFFD),NativeInt($FFFFFFFB),NativeInt($FFFFFFF7),
    NativeInt($FFFFFFEF),NativeInt($FFFFFFDF),NativeInt($FFFFFFBF),NativeInt($FFFFFF7F),
    NativeInt($FFFFFEFF),NativeInt($FFFFFDFF),NativeInt($FFFFFBFF),NativeInt($FFFFF7FF),
    NativeInt($FFFFEFFF),NativeInt($FFFFDFFF),NativeInt($FFFFBFFF),NativeInt($FFFF7FFF),
    NativeInt($FFFEFFFF),NativeInt($FFFDFFFF),NativeInt($FFFBFFFF),NativeInt($FFF7FFFF),
    NativeInt($FFEFFFFF),NativeInt($FFDFFFFF),NativeInt($FFBFFFFF),NativeInt($FF7FFFFF),
    NativeInt($FEFFFFFF),NativeInt($FDFFFFFF),NativeInt($FBFFFFFF),NativeInt($F7FFFFFF),
    NativeInt($EFFFFFFF),NativeInt($DFFFFFFF),NativeInt($BFFFFFFF),NativeInt($7FFFFFFF));
  {$endif}
  {$ifdef IsInt16}
  Expected: array[TNativeIntBitIndex] of NativeInt = (
    NativeInt($FFFE),NativeInt($FFFD),NativeInt($FFFB),NativeInt($FFF7),
    NativeInt($FFEF),NativeInt($FFDF),NativeInt($FFBF),NativeInt($FF7F),
    NativeInt($FEFF),NativeInt($FDFF),NativeInt($FBFF),NativeInt($F7FF),
    NativeInt($EFFF),NativeInt($DFFF),NativeInt($BFFF),NativeInt($7FFF));
  {$endif}
begin
  // writeln('TestNativeIntClearBit Start');
  for Index in TNativeIntBitIndex do
  begin
    {$ifdef IsInt64}
    NI := NativeInt($FFFFFFFFFFFFFFFF);
    {$endif}
    {$ifdef IsInt32}
    NI := NativeInt($FFFFFFFF);
    {$endif}
    {$ifdef IsInt}
    Q := NativeInt($FFFFF);
    {$endif}
    NI.ClearBit(Index);
    if NI <> Expected[Index] then
      Exit('Bit '+IntToStr(Index)+': expected <'+IntToStr(Expected[Index])+'> got <'+IntToStr(NI)+'>');
  end;
  // writeln('TestNativeIntClearBit: OK');
end;


Function TestNativeIntTestBit : string;
var
  Index: TNativeIntBitIndex;
  NI : NativeInt;

const
  {$ifdef IsInt64}
  Expected: array[TNativeIntBitIndex] of Boolean = (True,False,True,False,True,False,True,False,
                                                True,False,True,False,True,False,True,False,
                                                True,False,True,False,True,False,True,False,
                                                True,False,True,False,True,False,True,False,
                                                True,False,True,False,True,False,True,False,
                                                True,False,True,False,True,False,True,False,
                                                True,False,True,False,True,False,True,False,
                                                True,False,True,False,True,False,True,False);
  {$endif}
  {$ifdef IsInt32}
  Expected: array[TNativeIntBitIndex] of Boolean = (True,False,True,False,True,False,True,False,
                                                True,False,True,False,True,False,True,False,
                                                True,False,True,False,True,False,True,False,
                                                True,False,True,False,True,False,True,False);

  {$endif}
  {$ifdef IsInt16}
  Expected: array[TNativeIntBitIndex] of Boolean = (True,False,True,False,True,False,True,False,
                                                True,False,True,False,True,False,True,False);

  {$endif}
begin
  // writeln('TestNativeIntTestBit Start');
  {$ifdef IsInt64}
  NI := NativeInt($5555555555555555);
  {$endif}
  {$ifdef IsInt32}
  NI := NativeInt($55555555);
  {$endif}
  {$ifdef IsInt16}
  NI := NativeInt($5555);
  {$endif}
  for Index in TNativeIntBitIndex do
  begin
    if NI.TestBit(Index) <> Expected[Index] then
      Exit('Bit '+IntToStr(Index)+': expected <'+BoolToStr(Expected[Index])+'> got <'+BoolToStr(NI.TestBit(Index))+'>');
  end;
  // writeln('TestNativeIntTestBit: OK');
end;

Procedure RegisterHelperTests;

Var
  P : PSuite;

begin
  P:=AddSuite('OrdinalHelpers',EnsureSuite('SysUtils'));
//  P:=AddSuite('OrdinalHelpers',Psuite(Nil){EnsureSuite('SysUtils')});
  AddTest('ByteHelper',@TestByteHelper,P);
  AddTest('ShortIntHelper',@TestShortIntHelper,P);
  AddTest('NegShortIntHelper',@TestNegShortIntHelper,P);
  AddTest('WordHelper',@TestWordHelper,P);
  AddTest('SmallIntHelper',@TestSmallIntHelper,P);
  AddTest('NegSmallIntHelper',@TestNegSmallIntHelper,P);
  AddTest('CardinalHelper',@TestCardinalHelper,P);
  AddTest('LongintHelper',@TestLongintHelper,P);
  AddTest('NegLongintHelper',@TestNegLongintHelper,P);
  AddTest('QWordHelper',@TestQWordHelper,P);
  AddTest('Int64Helper',@TestInt64Helper,P);
  AddTest('NegInt64Helper',@TestNegInt64Helper,P);
  AddTest('TestByteSetBit',@TestByteSetBit,P);
  AddTest('TestByteToggleBit',@TestByteToggleBit,P);
  AddTest('TestByteClearBit',@TestByteClearBit,P);
  AddTest('TestByteTestBit',@TestByteTestBit,P);
  AddTest('TestShortIntSetBit',@TestShortIntSetBit,P);
  AddTest('TestShortIntToggleBit',@TestShortIntToggleBit,P);
  AddTest('TestShortIntClearBit',@TestShortIntClearBit,P);
  AddTest('TestShortIntTestBit',@TestShortIntTestBit,P);
  AddTest('TestWordSetBit',@TestWordSetBit,P);
  AddTest('TestWordToggleBit',@TestWordToggleBit,P);
  AddTest('TestWordClearBit',@TestWordClearBit,P);
  AddTest('TestWordTestBit',@TestWordTestBit,P);
  AddTest('TestSmallIntSetBit',@TestSmallIntSetBit,P);
  AddTest('TestSmallIntToggleBit',@TestSmallIntToggleBit,P);
  AddTest('TestSmallIntClearBit',@TestSmallIntClearBit,P);
  AddTest('TestSmallIntTestBit',@TestSmallIntTestBit,P);
  AddTest('TestCardinalSetBit',@TestCardinalSetBit,P);
  AddTest('TestCardinalToggleBit',@TestCardinalToggleBit,P);
  AddTest('TestCardinalClearBit',@TestCardinalClearBit,P);
  AddTest('TestCardinalTestBit',@TestCardinalTestBit,P);
  AddTest('TestLongIntSetBit',@TestLongIntSetBit,P);
  AddTest('TestLongIntToggleBit',@TestLongIntToggleBit,P);
  AddTest('TestLongIntClearBit',@TestLongIntClearBit,P);
  AddTest('TestLongIntTestBit',@TestLongIntTestBit,P);
  AddTest('TestQWordSetBit',@TestQWordSetBit,P);
  AddTest('TestQWordToggleBit',@TestQWordToggleBit,P);
  AddTest('TestQWordClearBit',@TestQWordClearBit,P);
  AddTest('TestQWordTestBit',@TestQWordTestBit,P);
  AddTest('TestInt64SetBit',@TestInt64SetBit,P);
  AddTest('TestInt64ToggleBit',@TestInt64ToggleBit,P);
  AddTest('TestInt64ClearBit',@TestInt64ClearBit,P);
  AddTest('TestInt64TestBit',@TestInt64TestBit,P);
  AddTest('TestNativeUIntSetBit',@TestNativeUIntSetBit,P);
  AddTest('TestNativeUIntToggleBit',@TestNativeUIntToggleBit,P);
  AddTest('TestNativeUIntClearBit',@TestNativeUIntClearBit,P);
  AddTest('TestNativeUIntTestBit',@TestNativeUIntTestBit,P);
  AddTest('TestNativeIntSetBit',@TestNativeIntSetBit,P);
  AddTest('TestNativeIntToggleBit',@TestNativeIntToggleBit,P);
  AddTest('TestNativeIntClearBit',@TestNativeIntClearBit,P);
  AddTest('TestNativeIntTestBit',@TestNativeIntTestBit,P);

  P:=AddSuite('GUIDHelper',EnsureSuite('SysUtils'));
//  P:=AddSuite('GUIDHelper',Psuite(Nil){EnsureSuite('SysUtils')});
  AddTest('CreateUntypedData',@TestGUIDHelperCreateUntypedData,P);
  AddTest('CreateUntypedDataEndian',@TestGUIDHelperCreateUntypedDataEndian,P);
  AddTest('CreateArrayOfByte',@TestGUIDHelperCreateArrayOfByte,P);
  AddTest('CreateTBytes',@TestGUIDHelperCreateTBytes,P);
  AddTest('CreateTBytesAtIndex',@TestGUIDHelperCreateTBytesAtIndex,P);
  AddTest('CreateString',@TestGUIDHelperCreateString,P);
  AddTest('CreateIntegerBytes',@TestGUIDHelperCreateIntegerBytes,P);
  AddTest('CreateWords',@TestGUIDHelperCreateWords,P);
  AddTest('CreateInteger',@TestGUIDHelperCreateInteger,P);
  AddTest('CreateNew',@TestGUIDHelperCreateNew,P);
  AddTest('ToByteArray',@TestGUIDHelperToByteArray,P);
  AddTest('ToString',@TestGUIDHelperToString,P);
  P:=AddSuite('FloatHelper',EnsureSuite('SysUtils'));
  // Float tests
  AddTest('IsNanSingle',@TestIsNanSingle,P);
  AddTest('IsNanDouble',@TestIsNanDouble,P);
  AddTest('IsNanExtended',@TestIsNanExtended,P);


end;


initialization
  RegisterHelperTests;
end.

