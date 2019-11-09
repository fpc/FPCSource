{$IFNDEF SBUNICODE}
unit utstringbuild;
{$ENDIF}

{$mode objfpc}
{$h+}
{$IFDEF SBUNICODE}
{$modeswitch unicodestrings}
{$ENDIF}

interface

uses
  SysUtils;

implementation

uses punit, utrtl;

Type
{$IFDEF SBUNICODE}
  TCharArray = Array of WideChar;
  TStringBuilder = TUnicodeStringBuilder;
{$ENDIF}

  { TTestObject }

  TTestObject = Class(TObject)
  Public
    Function ToString: ansistring; override;
  end;

Var
  SB : TStringBuilder;

Procedure InitSB(ASB : TStringBuilder);

begin
  SB:=ASB;
  If (SB=Nil) then
    SB:=TStringBuilder.Create('');
end;

Procedure DoneSB;

begin
  FreeAndNil(SB);
end;

function TearDownSB: TTestString;
begin
  FreeAndNil(SB);
  Result:='';
end;

function SetupSB: TTestString;
begin
  Result:='';
  InitSB(Nil);
end;


Function TestCreateCapacity : TTestString;

begin
  Result:='';
  DoneSB;
  InitSB(TStringBuilder.Create(12));
  If not AssertEquals('Correct capacity',12,SB.Capacity) then exit;
  If not AssertEquals('Correct length',0,SB.Length) then exit;
  If not AssertEquals('Maxcapacity',MaxInt,SB.MaxCapacity) then exit;
end;

Function TestCreateCapacityMaxCapacity : TTestString;

begin
  Result:='';
  DoneSB;
  InitSB(TStringBuilder.Create(12,23));
  If not AssertEquals('Correct capacity',12,SB.Capacity) then exit;
  If not AssertEquals('Correct length',0,SB.Length) then exit;
  If not AssertEquals('Maxcapacity',23,SB.MaxCapacity) then exit;
end;

Function TestCreateCapacityMaxCapacityExceeded : TTestString;

begin
  Result:='';
  DoneSB;
  ExpectException('Capacity exceeds max capacity',ERangeError);
  InitSB(TStringBuilder.Create(23,12));
end;

Function TestCreateCapacityString : TTestString;

begin
  Result:='';
  DoneSB;
  InitSB(TStringBuilder.Create('123',23));
  If not AssertEquals('Correct capacity',23,SB.Capacity) then exit;
  If not AssertEquals('Correct length',3,SB.Length) then exit;
  If not AssertEquals('Char 0','1',SB.Chars[0]) then exit;
  If not AssertEquals('Char 1','2',SB.Chars[1]) then exit;
  If not AssertEquals('Char 2','3',SB.Chars[2]) then exit;
end;

Function TestCreateString : TTestString;

begin
  Result:='';
  DoneSB;
  InitSB(TStringBuilder.Create('123'));
  If not AssertEquals('Correct capacity',64,SB.Capacity) then exit;
  If not AssertEquals('Correct length',3,SB.Length) then exit;
  If not AssertEquals('Char 0','1',SB.Chars[0]) then exit;
  If not AssertEquals('Char 1','2',SB.Chars[1]) then exit;
  If not AssertEquals('Char 2','3',SB.Chars[2]) then exit;
end;

Function TestToString : TTestString;

begin
  Result:='';
  DoneSB;
  InitSB(TStringBuilder.Create('12345'));
  If not AssertEquals('Correct asstring','12345',SB.ToString) then exit;
  If not AssertEquals('Correct asstring','234',SB.ToString(1,3)) then exit;
end;

Function TestCreateStringIndexCount : TTestString;

begin
  Result:='';
  DoneSB;
  InitSB(TStringBuilder.Create('aaa1234bbb',3,4,33));
  If not AssertEquals('Correct capacity',33,SB.Capacity) then exit;
  If not AssertEquals('Correct length',4,SB.Length) then exit;
  If not AssertEquals('Char 0','1',SB.Chars[0]) then exit;
  If not AssertEquals('Char 1','2',SB.Chars[1]) then exit;
  If not AssertEquals('Char 2','3',SB.Chars[2]) then exit;
  If not AssertEquals('Char 2','4',SB.Chars[3]) then exit;
end;

function TestAppendString: TTestString;
begin
  Result:='';
  if Not AssertSame('Return self 1',SB,SB.Append(AnsiString('ABC'))) then exit;
  If not AssertEquals('Empty','ABC',SB.ToString) then exit;
  if Not AssertSame('Return self 2',SB,SB.Append(AnsiString('DEF'))) then exit;
  If not AssertEquals('After append','ABCDEF',SB.ToString) then exit;
  if Not AssertSame('Return self 3',SB,SB.Append(AnsiString('ZGHIJKLM'),1,3)) then exit;
  If not AssertEquals('After append','ABCDEFGHI',SB.ToString) then exit;
end;

function TestAppendArrayOfChar: TTestString;

Var
  A1,A2,A3 : TCharArray;

begin
  Result:='';
  A1:=TCharArray.Create('A','B','C');
  A2:=TCharArray.Create('D','E','F');
  A3:=TCharArray.Create('Z','G','H','I','J','K','L','M');
  if Not AssertSame('Return self 1',SB,SB.Append(A1)) then exit;
  If not AssertEquals('Empty','ABC',SB.ToString) then exit;
  if Not AssertSame('Return self 2',SB,SB.Append(A2)) then exit;
  If not AssertEquals('After append','ABCDEF',SB.ToString) then exit;
  if Not AssertSame('Return self 3',SB,SB.Append(A3,1,3)) then exit;
  If not AssertEquals('After append 2','ABCDEFGHI',SB.ToString) then exit;
  A3[1]:=#0;
  if Not AssertSame('Return self 4',SB,SB.Append(A3)) then exit;
  If not AssertEquals('After append 3, null char terminates','ABCDEFGHIZ',SB.ToString) then exit;
end;

function TestClear : TTestString;

begin
  Result:='';
  SB.Append('abc');
  If not AssertEquals('Not empty',3,SB.Length) then exit;
  SB.Clear;
  If not AssertEquals('Empty',0,SB.Length) then exit;
end;

function TestAppendFormat : TTestString;

begin
  Result:='';
  if Not AssertSame('Return self 1',SB,SB.Append('%d',[1])) then exit;
  If not AssertEquals('Correctly formatted','1',SB.ToString) then exit;
  if Not AssertSame('Return self 2',SB,SB.AppendFormat('%d',[2])) then exit;
  If not AssertEquals('Correctly formatted','12',SB.ToString) then exit;
end;

function TestAppendByte : TTestString;

Var
  B : Byte = 123;

begin
  Result:='';
  if Not AssertSame('Return self',SB,SB.Append(b)) then exit;
  If Not AssertEquals('Correctly transformed to string','123',SB.ToString) then exit;
end;

function TestAppendBoolean : TTestString;


begin
  Result:='';
  if Not AssertSame('Return self',SB,SB.Append(True)) then exit;
  If Not AssertEquals('Correctly transformed to string','True',SB.ToString) then exit;
end;

function TestAppendChar : TTestString;

Var
  C : Char = 'd';

begin
  Result:='';
  if Not AssertSame('Return self',SB,SB.Append(c)) then exit;
  If Not AssertEquals('Correctly transformed to string','d',SB.ToString) then exit;
end;

function TestAppendCurrency : TTestString;

Var
  C : Currency = 1.25;

begin
  Result:='';
  if Not AssertSame('Return self',SB,SB.Append(c)) then exit;
  If Not AssertEquals('Correctly transformed to string',CurrToStr(C),SB.ToString) then exit;
end;

function TestAppendDouble : TTestString;

Var
  C : Double = 1.25;

begin
  Result:='';
  if Not AssertSame('Return self',SB,SB.Append(c)) then exit;
  If Not AssertEquals('Correctly transformed to string',FloatToStr(C),SB.ToString) then exit;
end;

function TestAppendSingle : TTestString;

Var
  C : Single = 1.25;

begin
  Result:='';
  if Not AssertSame('Return self',SB,SB.Append(c)) then exit;
  If Not AssertEquals('Correctly transformed to string',FloatToStr(C),SB.ToString) then exit;
end;


function TestAppendSmallint : TTestString;

Var
  C : Smallint = 125;

begin
  Result:='';
  if Not AssertSame('Return self',SB,SB.Append(c)) then exit;
  If Not AssertEquals('Correctly transformed to string','125',SB.ToString) then exit;
end;

function TestAppendInteger : TTestString;

Var
  C : Integer = $FFFFFF;

begin
  Result:='';
  if Not AssertSame('Return self',SB,SB.Append(c)) then exit;
  If Not AssertEquals('Correctly transformed to string',IntToStr(C),SB.ToString) then exit;
end;

function TestAppendInt64 : TTestString;

Var
  C : Int64 = $FFFFFFFFFF;

begin
  Result:='';
  if Not AssertSame('Return self',SB,SB.Append(c)) then exit;
  If Not AssertEquals('Correctly transformed to string',IntToStr(C),SB.ToString) then exit;
end;

function TestAppendShortInt : TTestString;

Var
  C : ShortInt = $1F;

begin
  Result:='';
  if Not AssertSame('Return self',SB,SB.Append(c)) then exit;
  If Not AssertEquals('Correctly transformed to string',IntToStr($1F),SB.ToString) then exit;
end;

function TestAppendQWord : TTestString;

Var
  C : QWord = $FFFFFFFFFF;

begin
  Result:='';
  if Not AssertSame('Return self',SB,SB.Append(c)) then exit;
  If Not AssertEquals('Correctly transformed to string',IntToStr(C),SB.ToString) then exit;
end;

function TestAppendWord : TTestString;

Var
  C : Word = $FFFF;

begin
  Result:='';
  if Not AssertSame('Return self',SB,SB.Append(c)) then exit;
  If Not AssertEquals('Correctly transformed to string',IntToStr(C),SB.ToString) then exit;
end;

function TestAppendCardinal : TTestString;

Var
  C : Cardinal = $FFFFFF;

begin
  Result:='';
  if Not AssertSame('Return self',SB,SB.Append(c)) then exit;
  If Not AssertEquals('Correctly transformed to string',IntToStr(C),SB.ToString) then exit;
end;

function TestAppendCharRepeat : TTestString;

Var
  C : Char;

begin
  Result:='';
  C:='*';
  if Not AssertSame('Return self',SB,SB.Append(c,5)) then exit;
  If Not AssertEquals('Correctly transformed to string','*****',SB.ToString) then exit;
end;

function TestAppendPAnsiChar : TTestString;

Var
  C : Array[0..5] of AnsiChar;
  P : PAnsiChar;

begin
  Result:='';
  C[0]:='1';
  C[1]:='2';
  C[2]:='3';
  C[3]:='4';
  C[4]:='5';
  C[5]:=#0;
  P:=@C[0];
  if Not AssertSame('Return self',SB,SB.Append(P)) then exit;
  If Not AssertEquals('Correctly transformed to string','12345',SB.ToString) then exit;
end;

function TestAppendObject : TTestString;

Var
  C : TTestObject;

begin
  Result:='';
  C:=TTestObject.Create;
  try
    if Not AssertSame('Return self',SB,SB.Append(C)) then exit;
    If Not AssertEquals('Correctly transformed to string','some string',SB.ToString) then exit;
  finally
    C.free;
  end;
end;

function TestInsertByte : TTestString;

Var
  B : Byte = 123;

begin
  Result:='';
  SB.Append('abcdef');
  if Not AssertSame('Return self',SB,SB.Insert(3,b)) then exit;
  If Not AssertEquals('Correctly transformed to string','abc123def',SB.ToString) then exit;
end;

function TestInsertBoolean : TTestString;


begin
  Result:='';
  SB.Append('abcdef');
  if Not AssertSame('Return self',SB,SB.Insert(3,True)) then exit;
  If Not AssertEquals('Correctly transformed to string','abcTruedef',SB.ToString) then exit;
end;

function TestInsertChar : TTestString;

Var
  C : Char = 'q';

begin
  Result:='';
  SB.Append('abcdef');
  if Not AssertSame('Return self',SB,SB.Insert(3,c)) then exit;
  If Not AssertEquals('Correctly transformed to string','abcqdef',SB.ToString) then exit;
end;

function TestInsertCurrency : TTestString;

Var
  C : Currency = 1.25;

begin
  Result:='';
  SB.Append('abcdef');
  if Not AssertSame('Return self',SB,SB.Insert(3,c)) then exit;
  If Not AssertEquals('Correctly transformed to string','abc'+CurrToStr(C)+'def',SB.ToString) then exit;
end;

function TestInsertDouble : TTestString;

Var
  C : Double = 1.25;

begin
  Result:='';
  SB.Append('abcdef');
  if Not AssertSame('Return self',SB,SB.Insert(3,c)) then exit;
  If Not AssertEquals('Correctly transformed to string','abc'+FloatToStr(C)+'def',SB.ToString) then exit;
end;

function TestInsertSingle : TTestString;

Var
  C : Single = 1.25;

begin
  Result:='';
  SB.Append('abcdef');
  if Not AssertSame('Return self',SB,SB.Insert(3,c)) then exit;
  If Not AssertEquals('Correctly transformed to string','abc'+FloatToStr(C)+'def',SB.ToString) then exit;
end;


function TestInsertSmallint : TTestString;

Var
  C : Smallint = 125;

begin
  Result:='';
  SB.Append('abcdef');
  if Not AssertSame('Return self',SB,SB.Insert(3,c)) then exit;
  If Not AssertEquals('Correctly transformed to string','abc125def',SB.ToString) then exit;
end;

function TestInsertInteger : TTestString;

Var
  C : Integer = $FFFFFF;

begin
  Result:='';
  SB.Append('abcdef');
  if Not AssertSame('Return self',SB,SB.Insert(3,c)) then exit;
  If Not AssertEquals('Correctly transformed to string','abc'+IntToStr(C)+'def',SB.ToString) then exit;
end;

function TestInsertInt64 : TTestString;

Var
  C : Int64 = $FFFFFFFFFF;

begin
  Result:='';
  SB.Append('abcdef');
  if Not AssertSame('Return self',SB,SB.Insert(3,c)) then exit;
  If Not AssertEquals('Correctly transformed to string','abc'+IntToStr(C)+'def',SB.ToString) then exit;
end;

function TestInsertShortInt : TTestString;

Var
  C : ShortInt = $1F;

begin
  Result:='';
  SB.Append('abcdef');
  if Not AssertSame('Return self',SB,SB.Insert(3,c)) then exit;
  If Not AssertEquals('Correctly transformed to string','abc'+IntToStr($1F)+'def',SB.ToString) then exit;
end;

function TestInsertQWord : TTestString;

Var
  C : QWord = $FFFFFFFFFF;

begin
  Result:='';
  SB.Append('abcdef');
  if Not AssertSame('Return self',SB,SB.Insert(3,c)) then exit;
  If Not AssertEquals('Correctly transformed to string','abc'+IntToStr(C)+'def',SB.ToString) then exit;
end;

function TestInsertWord : TTestString;

Var
  C : Word = $FFFF;

begin
  Result:='';
  SB.Append('abcdef');
  if Not AssertSame('Return self',SB,SB.Insert(3,c)) then exit;
  If Not AssertEquals('Correctly transformed to string','abc'+IntToStr(C)+'def',SB.ToString) then exit;
end;

function TestInsertCardinal : TTestString;

Var
  C : Cardinal = $FFFFFF;

begin
  Result:='';
  SB.Append('abcdef');
  if Not AssertSame('Return self',SB,SB.Insert(3,c)) then exit;
  If Not AssertEquals('Correctly transformed to string','abc'+IntToStr(C)+'def',SB.ToString) then exit;
end;

function TestInsertCharRepeat : TTestString;

Var
  C : Char;

begin
  Result:='';
  C:='*';
  SB.Append('abcdef');
  if Not AssertSame('Return self',SB,SB.Insert(3,c,5)) then exit;
  If Not AssertEquals('Correctly transformed to string','abc*****def',SB.ToString) then exit;
end;

function TestInsertPAnsiChar : TTestString;

Var
  C : Array[0..5] of AnsiChar;
  P : PAnsiChar;

begin
  Result:='';
  C[0]:='1';
  C[1]:='2';
  C[2]:='3';
  C[3]:='4';
  C[4]:='5';
  C[5]:=#0;
  P:=@C[0];
  SB.Append('abcdef');
  if Not AssertSame('Return self',SB,SB.Insert(3,P)) then exit;
  If Not AssertEquals('Correctly transformed to string','abc12345def',SB.ToString) then exit;
end;

function TestInsertObject : TTestString;

Var
  C : TTestObject;

begin
  Result:='';
  SB.Append('abcdef');
  C:=TTestObject.Create;
  try
    if Not AssertSame('Return self',SB,SB.Insert(3,C)) then exit;
    If Not AssertEquals('Correctly transformed to string','abcsome stringdef',SB.ToString) then exit;
  finally
    C.free;
  end;
end;

function TestInsertIndexNegative: TTestString;
begin
  Result:='';
  SB.Append('abcdef');
  ExpectException('No negatice index allowed',ERangeError);
  SB.Insert(-3,'abc')
end;

function TestInsertIndexTooBig: TTestString;
begin
  Result:='';
  SB.Append('abcdef');
  ExpectException('Maximum index exceeded',ERangeError);
  SB.Insert(6,'abc');
end;

function TestInsertString: TTestString;
begin
  Result:='';
  SB.Append('abcdef');
  if Not AssertSame('Return self',SB,SB.Insert(3,'123')) then exit;
  If Not AssertEquals('Correctly transformed to string','abc123def',SB.ToString) then exit;
end;

function TestInsertArrayOfChar: TTestString;

Var
  A : TCharArray;

begin
  Result:='';
  A:=TCharArray.Create('1','2','3');
  SB.Append('abcdef');
  if Not AssertSame('Return self',SB,SB.Insert(3,A)) then exit;
  If Not AssertEquals('Correctly transformed to string','abc123def',SB.ToString) then exit;
end;

function TestInsertArrayOfCharIndexNegative: TTestString;
Var
  A : TCharArray;

begin
  Result:='';
  A:=TCharArray.Create('1','2','3');
  Result:='';
  SB.Append('abcdef');
  ExpectException('No negatice index allowed',ERangeError);
  SB.Insert(-3,A)
end;

function TestInsertArrayOfCharIndexTooBig: TTestString;

Var
  A : TCharArray;

begin
  Result:='';
  A:=TCharArray.Create('1','2','3');
  SB.Append('abcdef');
  ExpectException('Maximum index exceeded',ERangeError);
  SB.Insert(6,A);
end;

function TestAppendLineString: TTestString;

begin
  Result:='';
  SB.Append('abc');
  if Not AssertSame('Return self',SB,SB.AppendLine('def')) then exit;
  If Not AssertEquals('Correctly transformed to string','abcdef'+sLineBreak,SB.ToString) then exit;
end;

function TestAppendLine: TTestString;
begin
  Result:='';
  SB.Append('abc');
  if Not AssertSame('Return self',SB,SB.AppendLine()) then exit;
  If Not AssertEquals('Correctly transformed to string','abc'+sLineBreak,SB.ToString) then exit;
end;

function TestCopyTo: TTestString;

Var
  C : TCharArray;
  I : Integer;

begin
  C:=Default(TCharArray);
  Result:='';
  SB.Append('abcdef');
  SetLength(C,12);
  For I:=0 to 11 do
    C[I]:='a';
  SB.CopyTo(1,C,0,3);
  if not AssertEquals('Correct copy','bcda',C[0]+C[1]+C[2]+c[3]) then exit;
  SB.CopyTo(4,C,3,2);
  if not AssertEquals('Correct copy','bcdef',C[0]+C[1]+C[2]+c[3]+c[4]) then exit;
end;

function TestCopyToNegativeSourceIndex: TTestString;

Var
  C : TCharArray;
  I : Integer;

begin
  C:=Default(TCharArray);
  Result:='';
  SB.Append('abcdef');
  SetLength(C,12);
  For I:=0 to 11 do
    C[I]:='a';
  ExpectException('Cannot pass negative source index',ERangeError);
  SB.CopyTo(-1,C,0,3);
end;

function TestCopyToNegativeCount: TTestString;

Var
  C : TCharArray;
  I : Integer;

begin
  C:=Default(TCharArray);
  Result:='';
  SB.Append('abcdef');
  SetLength(C,12);
  For I:=0 to 11 do
    C[I]:='a';
  ExpectException('Cannot pass negative count',ERangeError);
  SB.CopyTo(0,C,0,-3);
end;

function TestCopyToWrongRange: TTestString;

Var
  C : TCharArray;
  I : Integer;

begin
  C:=Default(TCharArray);
  Result:='';
  SB.Append('abcdef');
  SetLength(C,12);
  For I:=0 to 11 do
    C[I]:='a';
  ExpectException('Cannot pass negative count',ERangeError);
  SB.CopyTo(0,C,7,6); // 7+6=13
end;

function TestCopyToMaxRange: TTestString;

Var
  C : TCharArray;
  I : Integer;

begin
  Result:='';
  C:=Default(TCharArray);
  SB.Append('abcdef');
  SetLength(C,12);
  For I:=0 to 11 do
    C[I]:='a';
  SB.CopyTo(0,C,7,5); // 7+5=12
  if not AssertEquals('Correct copy','abcde',C[7]+C[8]+C[9]+c[10]+c[11]) then exit;
end;

function TestEquals: TTestString;

Var
  B : TStringBuilder;

begin
  Result:='';
  SB.Append('abc');
  AssertFalse('Nil is unequal',SB.Equals(Nil));
  B:=TStringBuilder.Create('a');
  try
    AssertFalse('Unequal length is unequal',SB.Equals(B));
    FreeAndNil(B);
    B:=TStringBuilder.Create('ade');
    AssertFalse('Equal length, unequal chars is unequal',SB.Equals(B));
    FreeAndNil(B);
    B:=TStringBuilder.Create('abc');
    AssertTrue('Equal length, equal chars is equal',SB.Equals(B));
    B:=TStringBuilder.Create(3,6);
    B.Append('abc');
    AssertFalse('Equal length, equal chars, unequal maxcapacity is unequal',SB.Equals(B));
  finally
    B.Free;
  end;
end;

function TestCapacity: TTestString;

Var
  C : Integer;

begin
  Result:='';
  SB.Append('abc');
  C:=SB.Capacity+10;
  If not AssertEquals('Returns new capacity',C,SB.EnsureCapacity(C)) then exit;
  If not AssertEquals('Returns new capacity, less than returns old',C,SB.EnsureCapacity(C-20)) then exit;
  ExpectException('No negative capacity',ERangeError);
  SB.EnsureCapacity(-1);
end;

function TestCapacityNegative: TTestString;

begin
  Result:='';
  FreeAndNil(SB);
  SB:=TStringBuilder.Create(10,20);
  ExpectException('Capacity less than maxcapacity',ERangeError);
  SB.EnsureCapacity(30);
end;

function TestAppendExceedsMaxCapacity: TTestString;
begin
  Result:='';
  FreeAndNil(SB);
  SB:=TStringBuilder.Create(10,20);
  ExpectException('Capacity exceeds maxcapacity on add',ERangeError);
  SB.Append(StringOfChar('*',30));
end;

function TestInsertExceedsMaxCapacity: TTestString;

begin
  Result:='';
  FreeAndNil(SB);
  SB:=TStringBuilder.Create(10,20);
  SB.Append('abcdef');
  ExpectException('Capacity exceeds maxcapacity on add',ERangeError);
  SB.Insert(3,StringOfChar('*',30));
end;

function TestInsertEqualsMaxCapacity: TTestString;

begin
  Result:='';
  FreeAndNil(SB);
  SB:=TStringBuilder.Create(10,20);
  SB.Append('abcdef');
  SB.Insert(3,StringOfChar('*',14));
  If not AssertEquals('Correctly added','abc**************def',SB.ToString) then exit;
end;

function TestAppendEqualsMaxCapacity: TTestString;

begin
  Result:='';
  FreeAndNil(SB);
  SB:=TStringBuilder.Create(10,20);
  SB.Append('abcdef');
  SB.Append(StringOfChar('*',14));
  If not AssertEquals('Correctly added','abcdef**************',SB.ToString) then exit;
end;

function TestRemove: TTestString;

begin
  Result:='';
  SB.Append('abcdef');
  if Not AssertSame('Return self 1',SB,SB.Remove(2,2)) then exit;
  If not AssertEquals('Correctly removed 2,2','abef',SB.ToString) then exit;
  if Not AssertSame('Return self 2',SB,SB.Remove(0,1)) then exit;
  If not AssertEquals('Correctly removed 0,1','bef',SB.ToString) then exit;
  if Not AssertSame('Return self 3',SB,SB.Remove(0,0)) then exit;
  If not AssertEquals('Correctly removed nothing','bef',SB.ToString) then exit;
  ExpectException('Negative length',ERangeError);
  SB.Remove(0,-1);
end;

function TestRemoveNegativeIndex: TTestString;

begin
  Result:='';
  SB.Append('abcdef');
  ExpectException('Negative startindex',ERangeError);
  SB.Remove(-1,1);
end;

function TestRemoveIndexTooBig: TTestString;

begin
  Result:='';
  SB.Append('abcdef');
  ExpectException('Startindex too big',ERangeError);
  SB.Remove(6,1);
end;

function TestRemoveIndexPluslengthTooBig: TTestString;

begin
  Result:='';
  SB.Append('abcdef');
  ExpectException('Startindex+Length too big',ERangeError);
  SB.Remove(4,3);
end;

function TestReplaceChar : TTestString;

Var
  Cold,CNew : Char;

begin
  Result:='';
  SB.Append('abcaedefa');
  Cold:='a';
  CNew:='z';
  if Not AssertSame('Return self 1',SB,SB.Replace(Cold,CNew)) then exit;
  If not AssertEquals('Correctly replaced 2 instances','zbczedefz',SB.ToString) then exit;
  Cold:='e';
  CNew:='z';
  if Not AssertSame('Return self 1',SB,SB.Replace(Cold,CNew,4,2)) then exit;
  If not AssertEquals('Correctly replaced 1 instance, bounded','zbczzdefz',SB.ToString) then exit;
end;

function TestReplaceString : TTestString;

Var
  Cold,CNew : String;

begin
  Result:='';
  SB.Append('aabcaaedeafaa');
  Cold:='aa';
  CNew:='zz';
  if Not AssertSame('Return self 1',SB,SB.Replace(Cold,CNew)) then exit;
  If not AssertEquals('Correctly replaced 2 instances','zzbczzedeafzz',SB.ToString) then exit;
  Cold:='e';
  CNew:='';
  if Not AssertSame('Return self 1',SB,SB.Replace(Cold,CNew,6,2)) then exit;
  If not AssertEquals('Correctly replaced 1 instance, bounded','zzbczzdeafzz',SB.ToString) then exit;
  SB.Clear;
  SB.Append('zzbczzedeafzz');
  Cold:='e';
  CNew:='qqqq';
  if Not AssertSame('Return self 1',SB,SB.Replace(Cold,CNew,6,3)) then exit;
  If not AssertEquals('Correctly replaced 2 instances, bounded','zzbczzqqqqdqqqqafzz',SB.ToString) then exit;
end;

Procedure RegisterStringBuilderTests;

Var
  P : PSuite;

begin
{$IFDEF SBUNICODE}
  P:=AddSuite('UnicodeStringBuilder',EnsureSuite('SysUtils'));
{$ELSE}
  P:=AddSuite('AnsiStringBuilder',EnsureSuite('SysUtils'));
{$ENDIF}
  P^.Options:=[soSetupTearDownPerTest];
  P^.Setup:=@SetupSB;
  P^.Teardown:=@TearDownSB;
  AddTest('TestCreateCapacity',@TestCreateCapacity,P);
  AddTest('TestCreateCapacityMaxCapacity',@TestCreateCapacityMaxCapacity,P);
  AddTest('TestCreateCapacityMaxCapacityExceeded',@TestCreateCapacityMaxCapacityExceeded,P);
  AddTest('TestCreateCapacityString',@TestCreateCapacityString,P);
  AddTest('TestCreateString',@TestCreateString,P);
  AddTest('TestCreateStringIndexCount',@TestCreateStringIndexCount,P);
  AddTest('TestToString',@TestToString,P);
  AddTest('TestClear',@TestClear,P);
  AddTest('TestAppendString',@TestAppendString,P);
  AddTest('TestAppendArrayOfChar',@TestAppendArrayOfChar,P);
  AddTest('TestAppendFormat',@TestAppendFormat,P);
  AddTest('TestAppendExceedsMaxCapacity',@TestAppendExceedsMaxCapacity,P);
  AddTest('TestAppendEqualsMaxCapacity',@TestAppendEqualsMaxCapacity,P);
  AddTest('TestAppendByte',@TestAppendByte,P);
  AddTest('TestAppendSmallInt',@TestAppendSmallint,P);
  AddTest('TestAppendWord',@TestAppendWord,P);
  AddTest('TestAppendInteger',@TestAppendInteger,P);
  AddTest('TestAppendCardinal',@TestAppendCardinal,P);
  AddTest('TestAppendInt64',@TestAppendInt64,P);
  AddTest('TestAppendQWord',@TestAppendQWord,P);
  AddTest('TestAppendShortInt',@TestAppendShortInt,P);
  AddTest('TestAppendBoolean',@TestAppendBoolean,P);
  AddTest('TestAppendChar',@TestAppendChar,P);
  AddTest('TestAppendCharRepeat',@TestAppendCharRepeat,P);
  AddTest('TestAppendCurrency',@TestAppendCurrency,P);
  AddTest('TestAppendDouble',@TestAppendDouble,P);
  AddTest('TestAppendSingle',@TestAppendSingle,P);
  AddTest('TestAppendPansiChar',@TestAppendPansiChar,P);
  AddTest('TestAppendObject',@TestAppendObject,P);
  AddTest('TestAppendLine',@TestAppendLine,P);
  AddTest('TestAppendLineString',@TestAppendLineString,P);
  AddTest('TestInsertString',@TestInsertString,P);
  AddTest('TestInsertExceedsMaxCapacity',@TestInsertExceedsMaxCapacity,P);
  AddTest('TestInsertEqualsMaxCapacity',@TestInsertEqualsMaxCapacity,P);
  AddTest('TestInsertArrayOfChar',@TestInsertArrayOfChar,P);
  AddTest('TestInsertByte',@TestInsertByte,P);
  AddTest('TestInsertSmallInt',@TestInsertSmallint,P);
  AddTest('TestInsertWord',@TestInsertWord,P);
  AddTest('TestInsertInteger',@TestInsertInteger,P);
  AddTest('TestInsertCardinal',@TestInsertCardinal,P);
  AddTest('TestInsertInt64',@TestInsertInt64,P);
  AddTest('TestInsertQWord',@TestInsertQWord,P);
  AddTest('TestInsertShortInt',@TestInsertShortInt,P);
  AddTest('TestInsertBoolean',@TestInsertBoolean,P);
  AddTest('TestInsertChar',@TestInsertChar,P);
  AddTest('TestInsertCharRepeat',@TestInsertCharRepeat,P);
  AddTest('TestInsertCurrency',@TestInsertCurrency,P);
  AddTest('TestInsertDouble',@TestInsertDouble,P);
  AddTest('TestInsertSingle',@TestInsertSingle,P);
  AddTest('TestInsertPansiChar',@TestInsertPansiChar,P);
  AddTest('TestInsertObject',@TestInsertObject,P);
  AddTest('TestInsertIndexNegative',@TestInsertIndexNegative,P);
  AddTest('TestInsertIndexTooBig',@TestInsertIndexTooBig,P);
  AddTest('TestInsertArrayOfCharIndexNegative',@TestInsertArrayOfCharIndexNegative,P);
  AddTest('TestInsertArrayOfCharIndexTooBig',@TestInsertArrayOfCharIndexTooBig,P);
  AddTest('TestCopyTo',@TestCopyTo,P);
  AddTest('TestCopyToNegativeIndex',@TestCopyToNegativeSourceIndex,P);
  AddTest('TestCopyToNegativeCount',@TestCopyToNegativeCount,P);
  AddTest('TestCopyToWrongRange',@TestCopyToWrongRange,P);
  AddTest('TestCopyToMaxRange',@TestCopyToMaxRange,P);
  AddTest('TestEquals',@TestEquals,P);
  AddTest('TestEnsureCapacity',@TestCapacity,P);
  AddTest('TestEnsureCapacityNegative',@TestCapacityNegative,P);
  AddTest('TestRemove',@TestRemove,P);
  AddTest('TestRemoveNegativeIndex',@TestRemoveNegativeIndex,P);
  AddTest('TestRemoveIndexTooBig',@TestRemoveIndexTooBig,P);
  AddTest('TestRemoveIndexPlusLengthTooBig',@TestRemoveIndexPlusLengthTooBig,P);
  AddTest('TestReplaceChar',@TestReplaceChar,P);
  AddTest('TestReplaceString',@TestReplaceString,P);
{
Function Replace(const OldValue: string; const NewValue: string): TStringBuilder;
Function Replace(const OldValue: string; const NewValue: string; StartIndex: Integer; Count: Integer): TStringBuilder;

}
end;

{ TTestObject }

function TTestObject.ToString: ansistring;
begin
  Result:='some string';
end;

initialization
  RegisterStringBuilderTests;
end.

