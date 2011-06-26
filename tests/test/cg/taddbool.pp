{ Program to test Code generator secondadd()                 }
{ with boolean values.                                       }
{ FUNCTIONAL PRE-REQUISITES:                                 }
{   - assignments function correctly.                        }
{   - if statements function correctly.                      }
{   - subroutine calls function correctly.                   }
Program TAddBool;


{$IFDEF VER70}
TYPE
  cardinal = longint;
{$ENDIF}


procedure fail;
begin
  WriteLn('Failed!');
  halt(1);
end;

{ ---------------------------- BOOLEAN TEST ----------------------------- }
{                              secondadd()                                }
{ ----------------------------------------------------------------------- }

procedure BoolTestAnd;
var
 b1, b2: boolean;
 b161, b162: boolean16;
 b321, b322: boolean32;
 b641, b642: boolean64;
 bb1, bb2: bytebool;
 wb1, wb2: wordbool;
 lb1, lb2: longbool;
 result : boolean;
begin
 result := true;
 { BOOLEAN AND BOOLEAN }
 Write('boolean AND boolean test...');
 b1 := true;
 b2 := false;
 if b1 and b2 then
   result := false;
 if b2 then
   result := false;
 b1 := false;
 b2 := false;
 if b1 and b2 then
   result := false;

 b1 := b1 and b2;
 if b1 then
   result := false;
 if b1 and FALSE then
   result := false;
 b1 := true;
 b2 := true;
 if b1 and b2 then
  begin
    if result then
      WriteLn('Success.')
    else
      Fail;
  end
 else
   Fail;

 { BOOLEAN16 AND BOOLEAN16 }
 Write('boolean16 AND boolean16 test...');
 b161 := true;
 b162 := false;
 if b161 and b162 then
   result := false;
 if b162 then
   result := false;
 b161 := false;
 b162 := false;
 if b161 and b162 then
   result := false;

 b161 := b161 and b162;
 if b161 then
   result := false;
 if b161 and FALSE then
   result := false;
 b161 := true;
 b162 := true;
 if b161 and b162 then
  begin
    if result then
      WriteLn('Success.')
    else
      Fail;
  end
 else
   Fail;

 { BOOLEAN32 AND BOOLEAN32 }
 Write('boolean32 AND boolean32 test...');
 b321 := true;
 b322 := false;
 if b321 and b322 then
   result := false;
 if b322 then
   result := false;
 b321 := false;
 b322 := false;
 if b321 and b322 then
   result := false;

 b321 := b321 and b322;
 if b321 then
   result := false;
 if b321 and FALSE then
   result := false;
 b321 := true;
 b322 := true;
 if b321 and b322 then
  begin
    if result then
      WriteLn('Success.')
    else
      Fail;
  end
 else
   Fail;

 { BOOLEAN64 AND BOOLEAN64 }
 Write('boolean64 AND boolean64 test...');
 b641 := true;
 b642 := false;
 if b641 and b642 then
   result := false;
 if b642 then
   result := false;
 b641 := false;
 b642 := false;
 if b641 and b642 then
   result := false;

 b641 := b641 and b642;
 if b641 then
   result := false;
 if b641 and FALSE then
   result := false;
 b641 := true;
 b642 := true;
 if b641 and b642 then
  begin
    if result then
      WriteLn('Success.')
    else
      Fail;
  end
 else
   Fail;

 { BYTEBOOL AND BYTEBOOL }
 Write('bytebool AND bytebool test...');
 bb1 := true;
 bb2 := false;
 if bb1 and bb2 then
   result := false;
 if bb2 then
   result := false;
 bb1 := false;
 bb2 := false;
 if bb1 and bb2 then
   result := false;

 bb1 := bb1 and bb2;
 if bb1 then
   result := false;
 if bb1 and FALSE then
   result := false;
 bb1 := true;
 bb2 := true;
 if bb1 and bb2 then
  begin
    if result then
      WriteLn('Success.')
    else
      Fail;
  end
 else
   Fail;

 { WORDBOOL AND WORDBOOL }
 result := true;
 Write('wordbool AND wordbool test...');
 wb1 := true;
 wb2 := false;
 if wb1 and wb2 then
   result := false;
 if wb2 then
   result := false;
 wb1 := false;
 wb2 := false;
 if wb1 and wb2 then
   result := false;

 wb1 := wb1 and wb2;
 if wb1 then
   result := false;
 if wb1 and FALSE then
   result := false;

 wb1 := true;
 wb2 := true;
 if wb1 and wb2 then
  begin
    if result then
      WriteLn('Success.')
    else
      Fail;
  end
 else
   Fail;

 { LONGBOOL AND LONGBOOL }
 result := true;
 Write('longbool AND longbool test...');
 lb1 := true;
 lb2 := false;
 if lb1 and lb2 then
   result := false;
 if lb2 then
   result := false;
 lb1 := false;
 lb2 := false;
 if lb1 and lb2 then
   result := false;

 lb1 := lb1 and lb2;
 if lb1 then
   result := false;
 if lb1 and FALSE then
   result := false;

 lb1 := true;
 lb2 := true;
 if lb1 and lb2 then
  begin
    if result then
      WriteLn('Success.')
    else
      Fail;
  end
 else
   Fail;
end;


procedure BoolTestOr;
var
 b1, b2: boolean;
 b161, b162: boolean16;
 b321, b322: boolean32;
 b641, b642: boolean64;
 bb1, bb2: bytebool;
 wb1, wb2: wordbool;
 lb1, lb2: longbool;
 result : boolean;
begin
 result := false;
 { BOOLEAN OR BOOLEAN }
 Write('boolean OR boolean test...');
 b1 := true;
 b2 := false;
 if b1 or b2 then
   result := true;
 b1 := false;
 b2 := false;
 if b1 or b2 then
   result := false;

 b1 := b1 or b2;
 if b1 then
   result := false;
 if b1 or FALSE then
   result := false;


 b1 := true;
 b2 := true;
 if b1 or b2 then
  begin
    if result then
      WriteLn('Success.')
    else
      Fail;
  end
 else
   Fail;

 { BOOLEAN16 OR BOOLEAN16 }
 Write('boolean16 OR boolean16 test...');
 b161 := true;
 b162 := false;
 if b161 or b162 then
   result := true;
 b161 := false;
 b162 := false;
 if b161 or b162 then
   result := false;

 b161 := b161 or b162;
 if b161 then
   result := false;
 if b161 or FALSE then
   result := false;


 b161 := true;
 b162 := true;
 if b161 or b162 then
  begin
    if result then
      WriteLn('Success.')
    else
      Fail;
  end
 else
   Fail;

 { BOOLEAN32 OR BOOLEAN32 }
 Write('boolean32 OR boolean32 test...');
 b321 := true;
 b322 := false;
 if b321 or b322 then
   result := true;
 b321 := false;
 b322 := false;
 if b321 or b322 then
   result := false;

 b321 := b321 or b322;
 if b321 then
   result := false;
 if b321 or FALSE then
   result := false;


 b321 := true;
 b322 := true;
 if b321 or b322 then
  begin
    if result then
      WriteLn('Success.')
    else
      Fail;
  end
 else
   Fail;

 { BOOLEAN64 OR BOOLEAN64 }
 Write('boolean64 OR boolean64 test...');
 b641 := true;
 b642 := false;
 if b641 or b642 then
   result := true;
 b641 := false;
 b642 := false;
 if b641 or b642 then
   result := false;

 b641 := b641 or b642;
 if b641 then
   result := false;
 if b641 or FALSE then
   result := false;


 b641 := true;
 b642 := true;
 if b641 or b642 then
  begin
    if result then
      WriteLn('Success.')
    else
      Fail;
  end
 else
   Fail;

 { BYTEBOOL OR BYTEBOOL }
 Write('bytebool OR bytebool test...');
 bb1 := true;
 bb2 := false;
 if bb1 or bb2 then
   result := true;
 bb1 := false;
 bb2 := false;
 if bb1 or bb2 then
   result := false;

 bb1 := bb1 or bb2;
 if bb1 then
   result := false;
 if bb1 or FALSE then
   result := false;


 bb1 := true;
 bb2 := true;
 if bb1 or bb2 then
  begin
    if result then
      WriteLn('Success.')
    else
      Fail;
  end
 else
   Fail;

 { WORDBOOL OR WORDBOOL }
 result := false;
 Write('wordbool OR wordbool test...');
 wb1 := true;
 wb2 := false;
 if wb1 or wb2 then
   result := true;
 wb1 := false;
 wb2 := false;
 if wb1 or wb2 then
   result := false;

 wb1 := wb1 or wb2;
 if wb1 then
   result := false;
 if wb1 or FALSE then
   result := false;

 wb1 := true;
 wb2 := true;
 if wb1 or wb2 then
  begin
    if result then
      WriteLn('Success.')
    else
      Fail;
  end
 else
   Fail;

 { LONGBOOL OR LONGBOOL }
 result := false;
 Write('longbool OR longbool test...');
 lb1 := true;
 lb2 := false;
 if lb1 or lb2 then
   result := true;
 if lb2 then
   result := false;
 lb1 := false;
 lb2 := false;
 if lb1 or lb2 then
   result := false;

 lb1 := lb1 or lb2;
 if lb1 then
   result := false;
 if lb1 or FALSE then
   result := false;

 lb1 := true;
 lb2 := true;
 if lb1 or lb2 then
  begin
    if result then
      WriteLn('Success.')
    else
      Fail;
  end
 else
   Fail;
end;


Procedure BoolTestXor;
var
 b1, b2: boolean;
 b161, b162: boolean16;
 b321, b322: boolean32;
 b641, b642: boolean64;
 bb1, bb2: bytebool;
 wb1, wb2: wordbool;
 lb1, lb2: longbool;
 result : boolean;
begin
 result := false;
 { BOOLEAN XOR BOOLEAN }
 Write('boolean XOR boolean test...');
 b1 := true;
 b2 := false;
 if b1 xor b2 then
   result := true;
 b1 := false;
 b2 := false;
 if b1 xor b2 then
   result := false;

 b1 := b1 xor b2;
 if b1 then
   result := false;
 if b1 xor FALSE then
   result := false;


 b1 := true;
 b2 := true;
 if b1 xor b2 then
  begin
     Fail;
  end
 else
  begin
    if result then
      WriteLn('Success.')
    else
      Fail;
  end;

  { BOOLEAN16 XOR BOOLEAN16 }
  Write('boolean16 XOR boolean16 test...');
  b161 := true;
  b162 := false;
  if b161 xor b162 then
    result := true;
  b161 := false;
  b162 := false;
  if b161 xor b162 then
    result := false;

  b161 := b161 xor b162;
  if b161 then
    result := false;
  if b161 xor FALSE then
    result := false;


  b161 := true;
  b162 := true;
  if b161 xor b162 then
   begin
      Fail;
   end
  else
   begin
     if result then
       WriteLn('Success.')
     else
       Fail;
   end;

  { BOOLEAN32 XOR BOOLEAN32 }
  Write('boolean32 XOR boolean32 test...');
  b321 := true;
  b322 := false;
  if b321 xor b322 then
    result := true;
  b321 := false;
  b322 := false;
  if b321 xor b322 then
    result := false;

  b321 := b321 xor b322;
  if b321 then
    result := false;
  if b321 xor FALSE then
    result := false;


  b321 := true;
  b322 := true;
  if b321 xor b322 then
   begin
      Fail;
   end
  else
   begin
     if result then
       WriteLn('Success.')
     else
       Fail;
   end;

  { BOOLEAN64 XOR BOOLEAN64 }
  Write('boolean64 XOR boolean64 test...');
  b641 := true;
  b642 := false;
  if b641 xor b642 then
    result := true;
  b641 := false;
  b642 := false;
  if b641 xor b642 then
    result := false;

  b641 := b641 xor b642;
  if b641 then
    result := false;
  if b641 xor FALSE then
    result := false;


  b641 := true;
  b642 := true;
  if b641 xor b642 then
   begin
      Fail;
   end
  else
   begin
     if result then
       WriteLn('Success.')
     else
       Fail;
   end;

 { BYTEBOOL XOR BYTEBOOL }
 Write('bytebool XOR bytebool test...');
 bb1 := true;
 bb2 := false;
 if bb1 xor bb2 then
   result := true;
 bb1 := false;
 bb2 := false;
 if bb1 xor bb2 then
   result := false;

 bb1 := bb1 xor bb2;
 if bb1 then
   result := false;
 if bb1 xor FALSE then
   result := false;


 bb1 := true;
 bb2 := true;
 if bb1 xor bb2 then
  begin
     Fail;
  end
 else
  begin
    if result then
      WriteLn('Success.')
    else
      Fail;
  end;

 { WORDBOOL XOR WORDBOOL }
 result := false;
 Write('wordbool XOR wordbool test...');
 wb1 := true;
 wb2 := false;
 if wb1 xor wb2 then
   result := true;
 wb1 := false;
 wb2 := false;
 if wb1 xor wb2 then
   result := false;

 wb1 := wb1 xor wb2;
 if wb1 then
   result := false;
 if wb1 xor FALSE then
   result := false;

 wb1 := true;
 wb2 := true;
 if wb1 xor wb2 then
  begin
      Fail;
  end
 else
   begin
    if result then
      WriteLn('Success.')
    else
      Fail;
   end;

 { LONGBOOL XOR LONGBOOL }
 result := false;
 Write('longbool XOR longbool test...');
 lb1 := true;
 lb2 := false;
 if lb1 xor lb2 then
   result := true;
 if lb2 then
   result := false;
 lb1 := false;
 lb2 := false;
 if lb1 xor lb2 then
   result := false;

 lb1 := lb1 xor lb2;
 if lb1 then
   result := false;
 if lb1 xor FALSE then
   result := false;

 lb1 := true;
 lb2 := true;
 if lb1 xor lb2 then
  begin
      Fail;
  end
 else
   begin
     if result then
       WriteLn('Success.')
     else
       Fail;
   end;
end;

Procedure BoolTestEqual;
var
 b1, b2, b3: boolean;
 b161, b162, b163: boolean16;
 b321, b322, b323: boolean32;
 b641, b642, b643: boolean64;
 bb1, bb2, bb3: bytebool;
 wb1, wb2, wb3: wordbool;
 lb1, lb2, lb3: longbool;
 result : boolean;
 values : longint;
Begin
 values := $02020202;

 { BOOLEAN = BOOLEAN }
 result := true;
 Write('boolean = boolean test...');
 b1 := true;
 b2 := true;
 b3 := false;
 b1 := (b1 = b2) and (b2 and false);
 if b1 then
   result := false;
 b1 := true;
 b2 := true;
 b3 := false;
 b1 := (b1 = b2) and (b2 and true);
 if not b1 then
   result := false;
 if b1 = b2 then
  begin
    if result then
      WriteLn('Success.')
    else
      Fail;
  end
 else
   Fail;

 { BOOLEAN16 = BOOLEAN16 }
 result := true;
 Write('boolean16 = boolean16 test...');
 b161 := true;
 b162 := true;
 b163 := false;
 b161 := (b161 = b162) and (b162 and false);
 if b161 then
   result := false;
 b161 := true;
 b162 := true;
 b163 := false;
 b161 := (b161 = b162) and (b162 and true);
 if not b161 then
   result := false;
 if b161 = b162 then
  begin
    if result then
      WriteLn('Success.')
    else
      Fail;
  end
 else
   Fail;

 { BOOLEAN32 = BOOLEAN32 }
 result := true;
 Write('boolean32 = boolean32 test...');
 b321 := true;
 b322 := true;
 b323 := false;
 b321 := (b321 = b322) and (b322 and false);
 if b321 then
   result := false;
 b321 := true;
 b322 := true;
 b323 := false;
 b321 := (b321 = b322) and (b322 and true);
 if not b321 then
   result := false;
 if b321 = b322 then
  begin
    if result then
      WriteLn('Success.')
    else
      Fail;
  end
 else
   Fail;

 { BOOLEAN64 = BOOLEAN64 }
 result := true;
 Write('boolean64 = boolean64 test...');
 b641 := true;
 b642 := true;
 b643 := false;
 b641 := (b641 = b642) and (b642 and false);
 if b641 then
   result := false;
 b641 := true;
 b642 := true;
 b643 := false;
 b641 := (b641 = b642) and (b642 and true);
 if not b641 then
   result := false;
 if b641 = b642 then
  begin
    if result then
      WriteLn('Success.')
    else
      Fail;
  end
 else
   Fail;

 { BYTEBOOL = BYTEBOOL }
 result := true;
 Write('bytebool = bytebool test...');
 bb1 := true;
 bb2 := true;
 bb3 := false;
 bb1 := (bb1 = bb2) and (bb2 and false);
 if bb1 then
   result := false;
 bb1 := true;
 bb2 := true;
 bb3 := false;
 bb1 := (bb1 = bb2) and (bb2 and true);
 if not bb1 then
   result := false;
 if bb1 = bb2 then
  begin
    if result then
      WriteLn('Success.')
    else
      Fail;
  end
 else
   Fail;

 { WORDBOOL = WORDBOOL }
 result := true;
 Write('wordbool = wordbool test...');
 wb1 := true;
 wb2 := true;
 wb3 := false;
 wb1 := (wb1 = wb2) and (wb2 and false);
 if wb1 then
   result := false;
 wb1 := true;
 wb2 := true;
 wb3 := false;
 wb1 := (wb1 = wb2) and (wb2 and true);
 if not wb1 then
   result := false;
 if wb1 = wb2 then
  begin
    if result then
      WriteLn('Success.')
    else
      Fail;
  end
 else
   Fail;
 Write('wordbool conversion to boolean...');
 result := TRUE;
 move(values,lb1,sizeof(lb1));
 if lb1 <> TRUE then
    result := false;
 if result then
    WriteLn('Success.')
 else
    Fail;

 { LONGBOOL = LONGBOOL }
 result := true;
 Write('longbool = longbool test...');
 lb1 := true;
 lb2 := true;
 lb3 := false;
 lb1 := (lb1 = lb2) and (lb2 and false);
 if lb1 then
   result := false;
 lb1 := true;
 lb2 := true;
 lb3 := false;
 lb1 := (lb1 = lb2) and (lb2 and true);
 if not lb1 then
   result := false;
 if lb1 = lb2 then
  begin
    if result then
      WriteLn('Success.')
    else
      Fail;
  end
 else
   Fail;
 Write('longbool conversion to boolean...');
 result := TRUE;
 move(values,lb1,sizeof(lb1));
 if lb1 <> TRUE then
    result := false;
 if result then
    WriteLn('Success.')
 else
    Fail;
end;


Procedure BoolTestNotEqual;
var
 b1, b2, b3: boolean;
 b161, b162, b163: boolean16;
 b321, b322, b323: boolean32;
 b641, b642, b643: boolean64;
 bb1, bb2, bb3: bytebool;
 wb1, wb2, wb3: wordbool;
 lb1, lb2, lb3: longbool;
 result : boolean;
Begin
 { BOOLEAN <> BOOLEAN }
 result := true;
 Write('boolean <> boolean test...');
 b1 := true;
 b2 := true;
 b3 := false;
 b1 := (b1 <> b2) and (b2 <> false);
 if b1 then
   result := false;
 b1 := true;
 b2 := true;
 b3 := false;
 b1 := (b1 <> b2) and (b2 <> true);
 if b1 then
   result := false;
 b1 := false;
 b2 := false;
 if b1 <> b2 then
  begin
      Fail;
  end
 else
  begin
   if result then
     WriteLn('Success.')
   else
     Fail;
  end;

 { BOOLEAN16 <> BOOLEAN16 }
 result := true;
 Write('boolean16 <> boolean16 test...');
 b161 := true;
 b162 := true;
 b163 := false;
 b161 := (b161 <> b162) and (b162 <> false);
 if b161 then
   result := false;
 b161 := true;
 b162 := true;
 b163 := false;
 b161 := (b161 <> b162) and (b162 <> true);
 if b161 then
   result := false;
 b161 := false;
 b162 := false;
 if b161 <> b162 then
  begin
      Fail;
  end
 else
  begin
   if result then
     WriteLn('Success.')
   else
     Fail;
  end;

 { BOOLEAN32 <> BOOLEAN32 }
 result := true;
 Write('boolean32 <> boolean32 test...');
 b321 := true;
 b322 := true;
 b323 := false;
 b321 := (b321 <> b322) and (b322 <> false);
 if b321 then
   result := false;
 b321 := true;
 b322 := true;
 b323 := false;
 b321 := (b321 <> b322) and (b322 <> true);
 if b321 then
   result := false;
 b321 := false;
 b322 := false;
 if b321 <> b322 then
  begin
      Fail;
  end
 else
  begin
   if result then
     WriteLn('Success.')
   else
     Fail;
  end;

 { BOOLEAN64 <> BOOLEAN64 }
 result := true;
 Write('boolean64 <> boolean64 test...');
 b641 := true;
 b642 := true;
 b643 := false;
 b641 := (b641 <> b642) and (b642 <> false);
 if b641 then
   result := false;
 b641 := true;
 b642 := true;
 b643 := false;
 b641 := (b641 <> b642) and (b642 <> true);
 if b641 then
   result := false;
 b641 := false;
 b642 := false;
 if b641 <> b642 then
  begin
      Fail;
  end
 else
  begin
   if result then
     WriteLn('Success.')
   else
     Fail;
  end;

 { BYTEBOOL <> BYTEBOOL }
 result := true;
 Write('bytebool <> bytebool test...');
 bb1 := true;
 bb2 := true;
 bb3 := false;
 bb1 := (bb1 <> bb2) and (bb2 <> false);
 if bb1 then
   result := false;
 bb1 := true;
 bb2 := true;
 bb3 := false;
 bb1 := (bb1 <> bb2) and (bb2 <> true);
 if bb1 then
   result := false;
 bb1 := false;
 bb2 := false;
 if bb1 <> bb2 then
  begin
      Fail;
  end
 else
  begin
   if result then
     WriteLn('Success.')
   else
     Fail;
  end;

 { WORDBOOL <> WORDBOOL }
 result := true;
 Write('wordbool <> wordbool test...');
 wb1 := true;
 wb2 := true;
 wb3 := false;
 wb1 := (wb1 <> wb2) and (wb2 <> false);
 if wb1 then
   result := false;
 wb1 := true;
 wb2 := true;
 wb3 := false;
 wb1 := (wb1 <> wb2) and (wb2 <> true);
 if wb1 then
   result := false;
 wb1 := false;
 wb2 := false;
 if wb1 <> wb2 then
  begin
      Fail;
  end
 else
  begin
   if result then
     WriteLn('Success.')
   else
     Fail;
  end;

 { LONGBOOL <> LONGBOOL }
 result := true;
 Write('longbool <> longbool test...');
 lb1 := true;
 lb2 := true;
 lb3 := false;
 lb1 := (lb1 <> lb2) and (lb2 <> false);
 if lb1 then
   result := false;
 lb1 := true;
 lb2 := true;
 lb3 := false;
 lb1 := (lb1 <> lb2) and (lb2 <> true);
 if lb1 then
   result := false;
 lb1 := false;
 lb2 := false;
 if lb1 <> lb2 then
  begin
      Fail;
  end
 else
  begin
   if result then
     WriteLn('Success.')
   else
     Fail;
  end;

end;

Procedure BoolLessThen;
var
 bb1, bb2: boolean;
 wb1, wb2: wordbool;
 lb1, lb2: longbool;
Begin
 {!!!!!!!!!!!}
end;


Procedure BoolGreaterThen;
var
 bb1, bb2: boolean;
 wb1, wb2: wordbool;
 lb1, lb2: longbool;
Begin
 {!!!!!!!!!!!!}
End;







Begin
  BoolTestAnd;
  BoolTestOr;
  BoolTestXor;
  BoolTestEqual;
  BoolTestNotEqual;
end.
