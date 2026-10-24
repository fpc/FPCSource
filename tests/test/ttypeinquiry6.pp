{ "type of" inside generics: the operand is parsed when the generic is
  specialized, so it can use the type parameters. }
program ttypeinquiry6;

{$mode objfpc}{$H+}

type
  generic TBox<T> = class
  public
    Value: T;
    function Test: longint;
  end;

generic procedure Swap<T>(var a,b: T);
var
  tmp: type of a;
begin
  tmp:=a;
  a:=b;
  b:=tmp;
end;

function TBox.Test: longint;
var
  { the type of Self is the specialized class }
  me: type of Self;
  { the type of an expression using the type parameter }
  v: type of (Default(T));
begin
  me:=Self;
  if me<>Self then
    Result:=-1
  else
    Result:=SizeOf(v);
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
  x: longint = 1;
  y: longint = 2;
  bw: specialize TBox<word>;
  bi: specialize TBox<int64>;
  { a specialization with "type of" as the argument }
  bt: specialize TBox<type of x>;
begin
  specialize Swap<longint>(x,y);
  Check((x=2) and (y=1),1);

  bw:=specialize TBox<word>.Create;
  Check(bw.Test=SizeOf(word),2);
  bw.Free;

  bi:=specialize TBox<int64>.Create;
  Check(bi.Test=SizeOf(int64),3);
  bi.Free;

  bt:=specialize TBox<type of x>.Create;
  bt.Value:=7;
  Check(bt.Value=7,4);
  Check(SizeOf(bt.Value)=SizeOf(longint),5);
  bt.Free;

  writeln('ok');
end.
