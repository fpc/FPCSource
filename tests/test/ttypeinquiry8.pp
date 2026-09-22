{ Type casts with "type of <operand>(<expr>)". }
program ttypeinquiry8;

{$mode objfpc}{$H+}

uses
  typinfo;

type
  TA = class
    Value: longint;
  end;

  TB = class(TA)
  end;

  TEnum = (eA, eB, eC);

  PLongint = ^longint;

var
  GlobalByte: byte = 0;
  GlobalWord: word = 0;
  GlobalInt: longint = 0;
  GlobalCard: cardinal = 0;
  GlobalEnum: TEnum = eA;
  GlobalChar: ansichar = #0;
  GlobalPtr: PLongint = nil;
  GlobalA: TA = nil;
  GlobalB: TB = nil;

type
  { a distinct type, so a cast is really needed }
  TUniqueWord = type type of GlobalWord;

var
  UniqueVar: TUniqueWord;

procedure Check(Ok: boolean; Id: longint);
begin
  if not Ok then
    begin
      writeln('failed: ',Id);
      halt(Id);
    end;
end;

begin
  { truncating and widening ordinal casts }
  GlobalWord:=$1234;
  Check(type of GlobalByte(GlobalWord)=$34,1);
  Check(SizeOf(type of GlobalByte(GlobalWord))=1,2);
  GlobalByte:=$56;
  Check(type of GlobalWord(GlobalByte)=$56,3);
  Check(SizeOf(type of GlobalWord(GlobalByte))=2,4);

  { the result of a cast has the type of the operand }
  Check(TypeInfo(type of GlobalWord(GlobalByte))=TypeInfo(word),5);

  { signed <-> unsigned of the same size }
  GlobalInt:=-1;
  Check(type of GlobalCard(GlobalInt)=High(cardinal),10);
  GlobalCard:=High(cardinal);
  Check(type of GlobalInt(GlobalCard)=-1,11);

  { a cast is an lvalue when the sizes match }
  GlobalInt:=0;
  type of GlobalCard(GlobalInt):=7;
  Check(GlobalInt=7,20);

  { nested casts }
  GlobalWord:=$1234;
  Check(type of GlobalWord(type of GlobalByte(GlobalWord))=$34,30);

  { enumerations and chars }
  GlobalByte:=2;
  Check(type of GlobalEnum(GlobalByte)=eC,40);
  GlobalEnum:=eB;
  Check(type of GlobalByte(GlobalEnum)=1,41);
  GlobalByte:=65;
  Check(type of GlobalChar(GlobalByte)='A',42);
  GlobalChar:='B';
  Check(type of GlobalByte(GlobalChar)=66,43);

  { pointers }
  GlobalInt:=99;
  GlobalPtr:=@GlobalInt;
  Check(type of GlobalPtr(GlobalPtr)^=99,50);
  Check(type of GlobalPtr(Pointer(GlobalPtr))^=99,51);

  { classes }
  GlobalB:=TB.Create;
  GlobalB.Value:=5;
  GlobalA:=GlobalB;
  Check(type of GlobalB(GlobalA).Value=5,60);
  Check(type of GlobalA(GlobalB).Value=5,61);
  GlobalB.Free;

  { a distinct type really needs the cast }
  GlobalWord:=8;
  UniqueVar:=type of UniqueVar(GlobalWord);
  Check(UniqueVar=8,70);
  Check(TypeInfo(type of UniqueVar(GlobalWord))<>TypeInfo(word),71);

  { casts of constants; the cast behaves like a cast to the named type }
  Check(type of GlobalByte(200)=200,80);
  Check(SizeOf(type of GlobalInt(1))=SizeOf(longint),81);
  Check((type of GlobalInt(1) shl 40)=(longint(1) shl 40),82);
  Check(SizeOf(type of GlobalInt(1) shl 40)=SizeOf(longint(1) shl 40),83);

  writeln('ok');
end.
