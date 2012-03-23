{ Program to test Code generator secondadd()                 }
{ with longint values                                        }
{ FUNCTIONAL PRE-REQUISITES:                                 }
{   - assignments function correctly.                        }
{   - if statements function correctly.                      }
{   - subroutine calls function correctly.                   }

{$R-}
{$Q-}

procedure fail;
begin
  WriteLn('Failed!');
  halt(1);
end;


procedure LongintTestAdd;
var
 i: longint;
 j: longint;
 result : boolean;
begin
 Write('Longint + Longint test...');
 result := true;
 i:=0;
 j:=0;
 i := i + -10000;
 if i <> -10000 then
  result := false;
 j := 32767;
 i := i + j;
 if i <> 22767 then
  result := false;
 i := i + j + 50000;
 if i <> 105534 then
  result := false;
 i:=0;
 j:=10000;
 i:= i + j + j + i + j;
 if i <> 30000 then
  result := false;
 if not result then
  Fail
 else
  WriteLn('Success.');
end;


procedure LongintTestSub;
var
 i, j, k : longint;
 result : boolean;
begin
 Write('Longint - Longint test...');
 result := true;
 i:=100000;
 j:=54;
 k:=56;
 i:= i - 100;
 if i <> 99900 then
  result := false;
 i := i - j - k - 100;
 if i <> 99690 then
  result := false;
 i:=100;
 j:=1000;
 k:=100;
 i:= j - i - k;
 if i <> 800 then
  result := false;
 if not result then
  Fail
 else
  WriteLn('Success.');
end;


procedure LongintTestMul;
var
 i : longint;
 j : longint;
 k: longint;
 result: boolean;
begin
 Write('Longint * Longint test...');
 result := true;
 i:=0;
 j:=0;
 i:=i * 32;
 if i <> 0 then
   result := false;
 i:=10;
 i:=i * -16;
 if i <> -160 then
    result := false;
 j:=10000;
 i:=-10000;
 i:=i * j;
 if i <> -100000000 then
    result := false;
 i:=1;
 j:=10;
 k:=16;
 i := i * j * k;
 if i <> 160 then
    result := false;
 i := 1;
 j := 10;
 k := 16;
 i := i * 10 * j * i * j * 16 * k;
 if i <> 256000 then
    result := false;
 if not result then
  Fail
 else
  WriteLn('Success.');
end;

procedure LongintTestXor;
var
 i, j : Longint;
 result : boolean;
begin
 Write('Longint XOR Longint test...');
 result := true;
 i := 0;
 j := 0;
 i := i xor $1000001;
 if i <> $1000001 then
   result := false;
 i:=0;
 j:=$10000001;
 i:=i xor j;
 if i <> $10000001 then
   result := false;

 i := 0;
 j := $55555555;
 i := i xor j xor $AAAAAAAA;
 if i <> longint($FFFFFFFF) then
   result := false;
 if not result then
  Fail
 else
  WriteLn('Success.');
end;


procedure LongintTestOr;
var
 i,j : Longint;
 result : boolean;
Begin
 Write('Longint OR Longint test...');
 result := true;
 i := 0;
 j := 0;
 i := i or $1000001;
 if i <> $1000001 then
   result := false;
 i:=0;
 j:=$10000001;
 i:=i or j;
 if i <> $10000001 then
   result := false;

 i := 0;
 j := $55555555;
 i := i or j or longint($AAAAAAAA);
 if i <> longint($FFFFFFFF) then
   result := false;
 if not result then
  Fail
 else
  WriteLn('Success.');
end;



procedure LongintTestAnd;
var
 i,j : Longint;
 result : boolean;
Begin
 Write('Longint AND Longint test...');
 result := true;
 i := $1000001;
 j := 0;
 i := i and $1000001;
 if i <> $1000001 then
   result := false;
 i:=0;
 j:=$10000001;
 i:=i and j;
 if i <> 0 then
   result := false;

 i := longint($FFFFFFFF);
 j := $55555555;
 i := i and j;
 if i <> $55555555 then
   result := false;
 i := longint($FFFFFFFF);
 i := i and longint($AAAAAAAA);
 if i <> longint($AAAAAAAA) then
   result := false;

 i := 0;
 j := $55555555;
 i := i and j and longint($AAAAAAAA);
 if i <> 0 then
   result := false;
 if not result then
  Fail
 else
  WriteLn('Success.');
end;

procedure LongintTestEqual;
var
 i,j : Longint;
 result : boolean;
Begin
 Write('Longint = Longint test...');
 result := true;
 i := $1000001;
 j := 0;
 if i = 0 then
   result := false;
 if i = j then
  result := false;
 if j = i then
  result := false;
 if not result then
  Fail
 else
  WriteLn('Success.');
end;


procedure LongintTestNotEqual;
var
 i,j : Longint;
 result : boolean;
Begin
 Write('Longint <> Longint test...');
 result := true;
 i := $1000001;
 j := $1000001;
 if i <> $1000001 then
   result := false;
 if i <> j then
  result := false;
 if j <> i then
  result := false;
 if not result then
  Fail
 else
  WriteLn('Success.');
end;

procedure LongintTestLE;
var
 i, j: Longint;
 result : boolean;
begin
 Write('Longint <= Longint test...');
 result := true;
 i := -1;
 j := -2;
 if i <= j then
   result := false;
 i := -2;
 j := $FFFF;
 if i >= j then
   result := false;
 i := longint($FFFFFFFF);
 if i <= longint($FFFFFFFE) then
    result := false;
 j := longint($FFFFFFFF);
 if i <= j then
  begin
    if result then
      WriteLn('Success.')
    else
      Fail;
  end
 else
  Fail;
end;


procedure LongintTestGE;
var
 i, j: Longint;
 result : boolean;
begin
 Write('Longint >= Longint test...');
 result := true;
 i := longint($FFFFFFFE);
 j := longint($FFFFFFFF);
 if i >= j then
   result := false;
 i := longint($FFFFFFFE);
 j := longint($FFFFFFFF);
 if i > j then
   result := false;
 i := longint($FFFFFFFE);
 if i > longint($FFFFFFFE) then
    result := false;
 i := longint($FFFFFFFF);
 j := longint($FFFFFFFF);
 if i >= j then
  begin
    if result then
      WriteLn('Success.')
    else
      Fail;
  end
 else
  Fail;
end;



Begin
  { These should be tested first, since if they do not }
  { work, they will false all other results.           }
  LongintTestEqual;
  LongintTestNotEqual;
  LongintTestAdd;
  LongintTestMul;
  LongintTestOr;
  LongintTestAnd;
  LongintTestXor;
  LongintTestLe;
  LongintTestGe;
  LongintTestSub;
end.
