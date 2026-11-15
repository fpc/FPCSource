{ Insert into a dynamic array whose source is a call with enough parameters to
  need the stack: the call must not overwrite an argument of the insertion that
  is already in place. }
program tdynarrinsert1;

{$mode objfpc}{$H+}
{$inline on}

type
  TBytes = array of Byte;
  TInts = array of LongInt;
  TStrs = array of AnsiString;

var
  errors: LongInt = 0;
  calls: LongInt = 0;

procedure Check(const aRow, aGot, aWant: AnsiString);
begin
  if aGot<>aWant then
    begin
      Writeln(aRow, ': got "', aGot, '" expected "', aWant, '"');
      Inc(errors);
    end;
end;

function Show(const a: TBytes): AnsiString;
var
  i: LongInt;
  s: AnsiString;
begin
  Result:='';
  for i:=0 to High(a) do
    begin
      Str(a[i], s);
      Result:=Result+s+' ';
    end;
end;

function ShowInts(const a: TInts): AnsiString;
var
  i: LongInt;
  s: AnsiString;
begin
  Result:='';
  for i:=0 to High(a) do
    begin
      Str(a[i], s);
      Result:=Result+s+' ';
    end;
end;

function ShowStrs(const a: TStrs): AnsiString;
var
  i: LongInt;
begin
  Result:='';
  for i:=0 to High(a) do
    Result:=Result+a[i]+' ';
end;

function Seq(aFirst, aCount: LongInt): TBytes;
var
  i: LongInt;
begin
  Result:=nil;
  SetLength(Result, aCount);
  for i:=0 to aCount-1 do
    Result[i]:=aFirst+i;
end;

{ nine parameters: some are passed on the stack on every target }
function Many(a1, a2, a3, a4, a5, a6, a7, a8, a9: PtrInt): TBytes;
begin
  Inc(calls);
  Result:=nil;
  SetLength(Result, 2);
  Result[0]:=a1+a2+a3+a4+a5;
  Result[1]:=a6+a7+a8+a9;
end;

{ nine parameters, with a result of another type than the elements of TBytes }
function ManySum(a1, a2, a3, a4, a5, a6, a7, a8, a9: PtrInt): LongInt;
begin
  Inc(calls);
  Result:=a1+a2+a3+a4+a5+a6+a7+a8+a9;
end;

procedure CopySame;
var
  b: TBytes;
begin
  b:=Seq(0, 6);
  Insert(Copy(b, 5, 1), b, 6);
  Check('copy of itself at the end', Show(b), '0 1 2 3 4 5 5 ');
  b:=Seq(0, 6);
  Insert(Copy(b, 0, 1), b, 0);
  Check('copy of itself at the start', Show(b), '0 0 1 2 3 4 5 ');
  b:=Seq(0, 6);
  Insert(Copy(b, 1, 3), b, 2);
  Check('copy of itself in the middle', Show(b), '0 1 1 2 3 2 3 4 5 ');
end;

procedure CopyOther;
var
  b, o, shared: TBytes;
begin
  o:=Seq(50, 3);
  b:=Seq(0, 6);
  Insert(Copy(o, 0, 1), b, 6);
  Check('copy of another, three arguments', Show(b), '0 1 2 3 4 5 50 ');
  b:=Seq(0, 6);
  Insert(Copy(o), b, 2);
  Check('copy of another, one argument', Show(b), '0 1 50 51 52 2 3 4 5 ');
  b:=Seq(0, 6);
  Insert(Copy(o, 1), b, 3);
  Check('copy of another, two arguments', Show(b), '0 1 2 51 52 3 4 5 ');
  b:=Seq(0, 6);
  Insert(Copy(Seq(100, 4), 1, 2), b, 1);
  Check('copy of a function result', Show(b), '0 101 102 1 2 3 4 5 ');
  { a destination that is not unique takes the other path of the helper }
  b:=Seq(0, 3);
  shared:=b;
  Insert(Copy(o, 0, 2), b, 1);
  Check('shared destination', Show(b), '0 50 51 1 2 ');
  Check('shared destination, the other reference', Show(shared), '0 1 2 ');
  b:=nil;
  Insert(Copy(o, 1, 2), b, 0);
  Check('empty destination', Show(b), '51 52 ');
end;

procedure CallWithStackParameters;
var
  b: TBytes;
begin
  calls:=0;
  b:=Seq(0, 3);
  Insert(Many(1, 2, 3, 4, 5, 6, 7, 8, 9), b, 1);
  Check('function with nine parameters', Show(b), '0 15 30 1 2 ');
  if calls<>1 then
    begin
      Writeln('function with nine parameters: called ', calls, ' times');
      Inc(errors);
    end;
  b:=Seq(0, 3);
  Insert([Many(1, 1, 1, 1, 1, 1, 1, 1, 1)[0], 7], b, 3);
  Check('array constructor holding such a call', Show(b), '0 1 2 5 7 ');
  b:=Seq(0, 3);
  Insert([ManySum(1, 1, 1, 1, 1, 1, 1, 1, 1), 7], b, 3);
  Check('array constructor of another element type holding such a call', Show(b), '0 1 2 9 7 ');
  b:=Seq(0, 3);
  Insert(Many(9, 8, 7, 6, 5, 4, 3, 2, 1)[1], b, 0);
  Check('one element from such a call', Show(b), '10 0 1 2 ');
end;

{ inlined, the result of these functions is the temp of the insertion that uses it,
  and their own insertion's temp is initialised from it }
function InlMake: TBytes; inline;
begin
  Result:=nil;
  SetLength(Result, 1);
  Result[0]:=1;
  Insert(Result, Result, 1);
end;

function InlMany(a1, a2, a3, a4, a5, a6, a7, a8, a9: PtrInt): TBytes; inline;
begin
  Result:=nil;
  SetLength(Result, 2);
  Result[0]:=a1+a2+a3+a4+a5;
  Result[1]:=a6+a7+a8+a9;
  Insert(Copy(Result, 0, 1), Result, 2);
end;

{ only compiled: the init code of the two temps refer to each other here, and the
  search for a call in them has to end }
procedure InlinedSource;
var
  b: TBytes;
begin
  b:=nil;
  Insert(InlMake(), b, 0);
  Insert(Copy(InlMake(), 0, 1), b, 1);
  Insert(InlMany(1, 2, 3, 4, 5, 6, 7, 8, 9), b, 0);
end;

procedure OtherElementTypes;
var
  i, io: TInts;
  s: TStrs;
  n: AnsiString;
begin
  SetLength(i, 3);
  i[0]:=1;
  i[1]:=2;
  i[2]:=3;
  Insert(Copy(i, 1, 2), i, 0);
  Check('integers', ShowInts(i), '2 3 1 2 3 ');
  io:=Copy(i, 0, 2);
  Insert(Copy(io, 0, 2), i, 5);
  Check('integers at the end', ShowInts(i), '2 3 1 2 3 2 3 ');
  { strings built at run time, so that their reference counts matter }
  SetLength(s, 2);
  Str(1, n);
  s[0]:='a'+n;
  Str(2, n);
  s[1]:='b'+n;
  Insert(Copy(s, 1, 1), s, 0);
  Check('strings', ShowStrs(s), 'b2 a1 b2 ');
  Insert(Copy(s, 0, 3), s, 3);
  Check('strings at the end', ShowStrs(s), 'b2 a1 b2 b2 a1 b2 ');
  s:=nil;
  Check('strings released', ShowStrs(s), '');
end;

begin
  CopySame;
  CopyOther;
  CallWithStackParameters;
  OtherElementTypes;
  if errors<0 then
    InlinedSource;
  if errors<>0 then
    Halt(1);
  Writeln('ok');
end.
