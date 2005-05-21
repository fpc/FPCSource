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
 bb1, bb2: boolean;
 wb1, wb2: wordbool;
 lb1, lb2: longbool;
 result : boolean;
begin
 result := true;
 { BOOLEAN AND BOOLEAN }
 Write('boolean AND boolean test...');
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
 bb1, bb2: boolean;
 wb1, wb2: wordbool;
 lb1, lb2: longbool;
 result : boolean;
begin
 result := false;
 { BOOLEAN AND BOOLEAN }
 Write('boolean OR boolean test...');
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

 { WORDBOOL AND WORDBOOL }
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

 { LONGBOOL AND LONGBOOL }
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
 bb1, bb2: boolean;
 wb1, wb2: wordbool;
 lb1, lb2: longbool;
 result : boolean;
begin
 result := false;
 { BOOLEAN XOR BOOLEAN }
 Write('boolean XOR boolean test...');
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
 bb1, bb2, bb3: boolean;
 wb1, wb2, wb3: wordbool;
 lb1, lb2, lb3: longbool;
 result : boolean;
 values : longint;
Begin
 values := $02020202;
 { BOOLEAN = BOOLEAN }
 result := true;
 Write('boolean = boolean test...');
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
 bb1, bb2, bb3: boolean;
 wb1, wb2, wb3: wordbool;
 lb1, lb2, lb3: longbool;
 result : boolean;
Begin
 { BOOLEAN <> BOOLEAN }
 result := true;
 Write('boolean <> boolean test...');
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
