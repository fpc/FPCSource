{ %OPT=-O2 }
{ On x86_64 the peephole optimizer folded a 32-bit "add $c,%reg" or "sub $c,%reg"
  into a following 64-bit "lea" that reads the whole register: the add was removed
  without its constant reaching the reference, and the 32-bit wrap-around of the
  operation, and its clearing of the register's upper half, were lost }
program tb0769;

{$mode objfpc}
{$R-}{$Q-}

type
  PPtrs = ^TPtrs;
  TPtrs = array[0..15] of Pointer;

  TList = class
    Items: PPtrs;
    function Find(out aIndex: longint): boolean;
    function SuccOfOutResult: longint;
    procedure SuccOfOutLocal;
    procedure SuccOfVarParam(var aIndex: longint);
  end;

var
  Data: TPtrs;
  Seed: longint;
  First, Second: Pointer;
  Failed: boolean;

procedure Check(const aName: string; aGot, aExpected: PtrInt);
begin
  if aGot <> aExpected then
    begin
      writeln(aName, ': got ', aGot, ', expected ', aExpected);
      Failed := true;
    end;
end;

procedure CheckCard(const aName: string; aGot, aExpected: cardinal);
begin
  if aGot <> aExpected then
    begin
      writeln(aName, ': got ', aGot, ', expected ', aExpected);
      Failed := true;
    end;
end;

procedure CheckQuad(const aName: string; aGot, aExpected: qword);
begin
  if aGot <> aExpected then
    begin
      writeln(aName, ': got ', aGot, ', expected ', aExpected);
      Failed := true;
    end;
end;

function TList.Find(out aIndex: longint): boolean;
begin
  aIndex := Seed;
  Find := false;
end;

{ the index is the function result, received through an out parameter }
function TList.SuccOfOutResult: longint;
begin
  if not Find(Result) then
    begin
      First := @Items^[Result];
      Second := @Items^[Succ(Result)];
    end;
end;

{ the index is a local variable, received through an out parameter }
procedure TList.SuccOfOutLocal;
var
  lIndex: longint;
begin
  if not Find(lIndex) then
    begin
      First := @Items^[lIndex];
      Second := @Items^[Succ(lIndex)];
    end;
end;

{ the index is a var parameter }
procedure TList.SuccOfVarParam(var aIndex: longint);
begin
  First := @Items^[aIndex];
  Second := @Items^[Succ(aIndex)];
end;

{ the index is a var parameter, the array a global variable }
procedure SuccOfGlobal(var aIndex: longint);
begin
  First := @Data[aIndex];
  Second := @Data[Succ(aIndex)];
end;

{ a cardinal that wraps in 32 bits, added to a pointer }
function SuccWrap(p: PByte; var c: cardinal): PByte;
begin
  SuccWrap := p + Succ(c);
end;

function PredWrap(p: PByte; var c: cardinal): PByte;
begin
  PredWrap := p + Pred(c);
end;

{ the low half of a 64-bit sum: the register holds the whole sum before the
  32-bit increment or decrement clears its upper half }
function SuccOfSum(p: PByte; x, y: int64): PByte;
begin
  SuccOfSum := p + Succ(cardinal(x + y));
end;

function PredOfSum(p: PByte; x, y: int64): PByte;
begin
  PredOfSum := p + Pred(cardinal(x + y));
end;

{ folds the optimizer still makes, through the register itself: the constant
  must reach the base, the index or both, at the operation's width }
function SuccTimes9(var n: cardinal): cardinal;
begin
  SuccTimes9 := Succ(n) * 9;
end;

function PredTimes9(var n: cardinal): cardinal;
begin
  PredTimes9 := Pred(n) * 9;
end;

function SuccPlus(var n: cardinal; m: cardinal): cardinal;
begin
  SuccPlus := Succ(n) + m;
end;

function PredPlus(var n: cardinal; m: cardinal): cardinal;
begin
  PredPlus := Pred(n) + m;
end;

function SuccTimes9Q(var n: qword): qword;
begin
  SuccTimes9Q := Succ(n) * 9;
end;

function PredTimes9Q(var n: qword): qword;
begin
  PredTimes9Q := Pred(n) * 9;
end;

{ the same wrap-arounds as a scaled index }
function SuccWrapIndex(p: PPointer; var c: cardinal): PPointer;
begin
  SuccWrapIndex := @p[Succ(c)];
end;

function PredWrapIndex(p: PPointer; var c: cardinal): PPointer;
begin
  PredWrapIndex := @p[Pred(c)];
end;

var
  List: TList;
  Index: longint;
  Card: cardinal;
  Quad: qword;
  Base: PByte;
begin
  List := TList.Create;
  List.Items := @Data;
  Seed := 3;

  First := nil;
  Second := nil;
  List.SuccOfOutResult;
  Check('out result', PByte(Second) - PByte(First), SizeOf(Pointer));

  First := nil;
  Second := nil;
  List.SuccOfOutLocal;
  Check('out local', PByte(Second) - PByte(First), SizeOf(Pointer));

  First := nil;
  Second := nil;
  Index := 3;
  List.SuccOfVarParam(Index);
  Check('var parameter', PByte(Second) - PByte(First), SizeOf(Pointer));

  First := nil;
  Second := nil;
  Index := 3;
  SuccOfGlobal(Index);
  Check('global array', PByte(Second) - PByte(First), SizeOf(Pointer));
  List.Free;

  Base := PByte(@Data);
  Card := $FFFFFFFF;
  Check('succ wrap', SuccWrap(Base, Card) - Base, 0);
  Card := 0;
  Check('pred wrap', PredWrap(Base, Card) - Base, PtrInt(High(cardinal)));
  Check('succ of sum', SuccOfSum(Base, $500000001, 1) - Base, 3);
  Check('pred of sum', PredOfSum(Base, $500000001, 1) - Base, 1);
  Card := $FFFFFFFF;
  Check('succ wrap index', PByte(SuccWrapIndex(PPointer(Base), Card)) - Base, 0);
  Card := 0;
  Check('pred wrap index', PByte(PredWrapIndex(PPointer(Base), Card)) - Base,
    PtrInt(High(cardinal)) * SizeOf(Pointer));

  Card := 4;
  CheckCard('succ times 9', SuccTimes9(Card), 45);
  Card := $FFFFFFFF;
  CheckCard('succ times 9, wrapped', SuccTimes9(Card), 0);
  Card := 5;
  CheckCard('pred times 9', PredTimes9(Card), 36);
  Card := 0;
  CheckCard('pred times 9, wrapped', PredTimes9(Card), $FFFFFFF7);
  Card := 4;
  CheckCard('succ plus', SuccPlus(Card, 10), 15);
  CheckCard('pred plus', PredPlus(Card, 10), 13);
  Quad := 4;
  CheckQuad('succ times 9, 64-bit', SuccTimes9Q(Quad), 45);
  Quad := High(qword);
  CheckQuad('succ times 9, 64-bit wrapped', SuccTimes9Q(Quad), 0);
  Quad := 5;
  CheckQuad('pred times 9, 64-bit', PredTimes9Q(Quad), 36);
  Quad := 0;
  CheckQuad('pred times 9, 64-bit wrapped', PredTimes9Q(Quad), High(qword) - 8);

  if Failed then
    halt(1);
  writeln('ok');
end.
