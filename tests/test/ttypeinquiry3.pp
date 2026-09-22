{ "type of Foo(bar)" is either the result type of a function or a type cast,
  depending on what Foo is. Foo is never called. }
program ttypeinquiry3;

{$mode objfpc}{$H+}

uses
  typinfo;

type
  TFunc = function(x: byte): int64;

var
  CallCount: longint = 0;
  SomeByte: byte = 1;
  SomeWord: word = 2;
  SomeVar: word = 3;
  Fn: TFunc;

{ overloaded functions }
function Foo(b: byte): word; overload;
begin
  Inc(CallCount);
  Result:=b;
end;

function Foo(w: word): int64; overload;
begin
  Inc(CallCount);
  Result:=w;
end;

function ViaProcVar(x: byte): int64;
begin
  Inc(CallCount);
  Result:=x;
end;

procedure Check(Ok: boolean; Id: longint);
begin
  if not Ok then
    begin
      writeln('failed: ',Id);
      halt(Id);
    end;
end;

var
  { Foo is a function, so (SomeByte) is its parameter list and the overload
    taking a byte is chosen: the type is word }
  ResByte: type of Foo(SomeByte);
  { the overload taking a word is chosen: the type is int64 }
  ResWord: type of Foo(SomeWord);
  { the same through a procedural variable }
  ResProcVar: type of Fn(SomeByte);
  { SomeVar is a variable, so (SomeByte) is a type cast to word }
  CastVar: type of SomeVar;
begin
  Fn:=@ViaProcVar;
  ResByte:=0;
  ResWord:=0;
  ResProcVar:=0;
  CastVar:=0;

  { overload resolution ran, but neither overload was called }
  Check(CallCount=0,1);
  Check(TypeInfo(ResByte)=TypeInfo(word),2);
  Check(TypeInfo(ResWord)=TypeInfo(int64),3);
  Check(TypeInfo(ResProcVar)=TypeInfo(int64),4);
  Check(TypeInfo(CastVar)=TypeInfo(word),5);

  { the same in an expression }
  Check(SizeOf(type of Foo(SomeByte))=2,10);
  Check(SizeOf(type of Foo(SomeWord))=8,11);
  Check(SizeOf(type of Fn(SomeByte))=8,12);
  Check(CallCount=0,13);

  { "type of SomeVar(SomeByte)" is a type cast of SomeByte to word }
  SomeByte:=$12;
  Check(type of SomeVar(SomeByte)=$12,20);
  Check(SizeOf(type of SomeVar(SomeByte))=2,21);
  Check(CallCount=0,22);

  { the functions still work when they are really called }
  Check(Foo(SomeByte)=$12,30);
  Check(CallCount=1,31);
  Check(Foo(SomeWord)=2,32);
  Check(CallCount=2,33);
  Check(Fn(SomeByte)=$12,34);
  Check(CallCount=3,35);
  Check(SomeVar=3,36);

  writeln('ok');
end.
