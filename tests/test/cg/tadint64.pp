{ Program to test Code generator secondadd()                 }
{ with int64 values                                        }
{ FUNCTIONAL PRE-REQUISITES:                                 }
{   - assignments function correctly.                        }
{   - if statements function correctly.                      }
{   - subroutine calls function correctly.                   }

procedure fail;
begin
  WriteLn('Failed!');
  halt(1);
end;


procedure int64TestAdd;
var
 i: int64;
 j: int64;
 result : boolean;
begin
 Write('int64 + int64 test...');
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


procedure int64TestSub;
var
 i, j, k : int64;
 result : boolean;
begin
 Write('int64 - int64 test...');
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
 j := 900 - i;
 if (j <> 100) then
   result := false;

 i := 1000000000;
 k := i;
 i := i * 10;
 j := 1000000000 - i;
 k := k - i;
 if j <> k then
   result := false;
 if j <> (1000000000-(int64(1000000000) * 10)) then
   result := false;
 j := (int64(1) shl 33);
 i := (int64(1) shl 34) - j;
 if (i <> (int64(1) shl 33)) then
   result := false;

 i := 1 - j;
 if (i <> (1-(int64(1) shl 33))) then
   result := false;

 i := 100000;
 i := i - 90000;
 if (i <> 10000) then
   result := false;

 if not result then
  Fail
 else
  WriteLn('Success.');
end;


procedure int64TestMul;
var
 i : int64;
 j : int64;
 k: int64;
 result: boolean;
begin
 Write('int64 * int64 test...');
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

procedure int64TestXor;
var
 i, j : int64;
 result : boolean;
begin
 Write('int64 XOR int64 test...');
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
 if i <> $FFFFFFFF then
   result := false;
 if not result then
  Fail
 else
  WriteLn('Success.');
end;


procedure int64TestOr;
var
 i,j : int64;
 result : boolean;
Begin
 Write('int64 OR int64 test...');
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
 i := i or j or $AAAAAAAA;
 if i <> $FFFFFFFF then
   result := false;
 if not result then
  Fail
 else
  WriteLn('Success.');
end;



procedure int64TestAnd;
var
 i,j : int64;
 result : boolean;
Begin
 Write('int64 AND int64 test...');
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

 i := $FFFFFFFF;
 j := $55555555;
 i := i and j;
 if i <> $55555555 then
   result := false;
 i := $FFFFFFFF;
 i := i and $AAAAAAAA;
 if i <> $AAAAAAAA then
   result := false;

 i := 0;
 j := $55555555;
 i := i and j and $AAAAAAAA;
 if i <> 0 then
   result := false;
 if not result then
  Fail
 else
  WriteLn('Success.');
end;

procedure int64TestEqual;
var
 i,j : int64;
 result : boolean;
Begin
 Write('int64 = int64 test...');
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


procedure int64TestNotEqual;
var
 i,j : int64;
 result : boolean;
Begin
 Write('int64 <> int64 test...');
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

procedure int64TestLE;
var
 i, j: int64;
 result : boolean;
begin
 Write('int64 <= int64 test...');
 result := true;
 i := -1;
 j := -2;
 if i <= j then
   result := false;
 i := -2;
 j := $FFFF;
 if i >= j then
   result := false;
 i := $FFFFFFFF;
 if i <= $FFFFFFFE then
    result := false;
 j := $FFFFFFFF;
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


procedure int64TestGE;
var
 i, j: int64;
 result : boolean;
begin
 Write('int64 >= int64 test...');
 result := true;
 i := $FFFFFFFE;
 j := $FFFFFFFF;
 if i >= j then
   result := false;
 i := $FFFFFFFE;
 j := $FFFFFFFF;
 if i > j then
   result := false;
 i := $FFFFFFFE;
 if i > $FFFFFFFE then
    result := false;
 i := $FFFFFFFF;
 j := $FFFFFFFF;
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

{ QWord testing }
procedure qwordTestAdd;
var
 i: qword;
 j: qword;
 result : boolean;
begin
 Write('qword + qword test...');
 result := true;
 i:=0;
 j:=0;
 i := i + 10000;
 if i <> 10000 then
  result := false;
 j := 32767;
 i := i + j;
 if i <> 42767 then
  result := false;
 i := i + j + 50000;
 if i <> 125534 then
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


procedure QwordTestSub;
var
 i, j, k : qword;
 result : boolean;
begin
 Write('qword - qword test...');
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
 j := 900 - i;
 if (j <> 100) then
   result := false;

 i := 1000000000;
 k := i;
 i := i * 10;
 { The next statement would create an overflow }
 {$Q-}
 j := 1000000000 - i;
 k := k - i;
 if j <> k then
   result := false;

 { Since qword variable<>negative constant is always false according to the 
   compiler (allowing it to optimize the if away) we need to do a preventive
   typecast to qword.}
 if j <> qword(1000000000-(qword(1000000000) * 10)) then
   result := false;
 j := (qword(1) shl 33);
 i := (qword(1) shl 34) - j;
 if (i <> (qword(1) shl 33)) then
   result := false;

 i := 1 - j;
 { Since qword variable<>negative constant is always false according to the 
   compiler (allowing it to optimize the if away) we need to do a preventive
   typecast to qword.}
 if i <> qword(1-(qword(1) shl 33)) then
   result := false;

 i := 100000;
 i := i - 90000;
 if (i <> 10000) then
   result := false;

 if not result then
  Fail
 else
  WriteLn('Success.');
end;


procedure QwordTestMul;
var
 i : qword;
 j : qword;
 k: qword;
 result: boolean;
begin
 Write('qword * qword test...');
 result := true;
 i:=0;
 j:=0;
 i:=i * 32;
 if i <> 0 then
   result := false;
 i:=10;
 i:=i * 16;
 if i <> 160 then
    result := false;
 j:=10000;
 i:=10000;
 i:=i * j;
 if i <> 100000000 then
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

procedure QwordTestXor;
var
 i, j : qword;
 result : boolean;
begin
 Write('qword XOR qword test...');
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
 if i <> $FFFFFFFF then
   result := false;
 if not result then
  Fail
 else
  WriteLn('Success.');
end;


procedure QwordTestOr;
var
 i,j : qword;
 result : boolean;
Begin
 Write('qword OR qword test...');
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
 i := i or j or $AAAAAAAA;
 if i <> $FFFFFFFF then
   result := false;
 if not result then
  Fail
 else
  WriteLn('Success.');
end;



procedure QwordTestAnd;
var
 i,j : qword;
 result : boolean;
Begin
 Write('qword AND qword test...');
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

 i := $FFFFFFFF;
 j := $55555555;
 i := i and j;
 if i <> $55555555 then
   result := false;
 i := $FFFFFFFF;
 i := i and $AAAAAAAA;
 if i <> $AAAAAAAA then
   result := false;

 i := 0;
 j := $55555555;
 i := i and j and $AAAAAAAA;
 if i <> 0 then
   result := false;
 if not result then
  Fail
 else
  WriteLn('Success.');
end;

procedure QwordTestEqual;
var
 i,j : qword;
 result : boolean;
Begin
 Write('qword = qword test...');
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


procedure QwordTestNotEqual;
var
 i,j : qword;
 result : boolean;
Begin
 Write('qword <> qword test...');
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

procedure QwordTestLE;
var
 i, j: qword;
 result : boolean;
begin
 Write('qword <= qword test...');
 result := true;
 i := 1;
 j := 2;
 if j <= i then
   result := false;
 i := 2;
 j := $FFFF;
 if i >= j then
   result := false;
 i := $FFFFFFFF;
 if i <= $FFFFFFFE then
    result := false;
 j := $FFFFFFFF;
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


procedure QwordTestGE;
var
 i, j: qword;
 result : boolean;
begin
 Write('qword >= qword test...');
 result := true;
 i := $FFFFFFFE;
 j := $FFFFFFFF;
 if i >= j then
   result := false;
 i := $FFFFFFFE;
 j := $FFFFFFFF;
 if i > j then
   result := false;
 i := $FFFFFFFE;
 if i > $FFFFFFFE then
    result := false;
 i := $FFFFFFFF;
 j := $FFFFFFFF;
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
  Int64TestEqual;
  Int64TestNotEqual;
  Int64TestAdd;
  Int64TestMul;
  Int64TestOr;
  Int64TestAnd;
  Int64TestXor;
  Int64TestLe;
  Int64TestGe;
  Int64TestSub;
  QwordTestEqual;
  QwordTestNotEqual;
  QwordTestAdd;
  QwordTestMul;
  QwordTestOr;
  QwordTestAnd;
  QwordTestXor;
  QwordTestLe;
  QwordTestGe;
  QwordTestSub;
end.
