Program fpcbugcheck;

{$mode macpas} { for | }

Var
  FrobCounter : Integer;

const
  ok: boolean = true;

Function Frob : Boolean;
begin
  Inc(FrobCounter);
  Frob := False;
end;

{
  This program tests whether "Frob or Frob" is contracted to a single
  "Frob" call, in different contexts:
    - without shortcut evaluation (no contraction allowed)
    - with shortcut evaluation enabled
    - using the MacPas "|" instead of "or"
  It prints the number of Frob calls for each variant.
}

{
  The second test this program does is related to the first one,
  it exercises a bug in fpc deeming all compares with string constants
  to be equal at the stage of compilation the elision of duplicate expression
  in bools is performed.

  This bug has been active since the introduction of the boolean shortcut
  optimization (r14714), but as that optimization only applies to the
  explicit shortcut form (in form of the MacPas operators), that bug
  didn't show.
}

{$B+}
Procedure CountFrobsBoolFull;
begin
  FrobCounter := 0;
  if Frob or Frob then
    begin
      WriteLn('Complete: Huh');
      ok:=false;
    end
  else
    begin
      WriteLn('Complete: Frobbed ', FrobCounter, ' times');
      if FrobCounter<>2 then
         ok:=false;
    end;
end;

{$B-}
Procedure CountFrobsBoolShort;
begin
  FrobCounter := 0;
  if Frob or Frob then
    begin
      WriteLn('Short: Huh');
      ok:=false;
    end
  else
    begin
      WriteLn('Short: Frobbed ', FrobCounter, ' times');
      if FrobCounter<>2 then
        ok:=false;
    end;
end;

Procedure CountFrobsMac;
begin
  FrobCounter := 0;
  if Frob | Frob then
    begin
      WriteLn('Mac: Huh');
      ok:=false;
    end
  else
    begin
      WriteLn('Mac: Frobbed ', FrobCounter, ' times');
      if FrobCounter<>2 then
        ok:=false;
    end;
end;

var
  test: String;
{$B-}
begin
  ok:=true;
  { test conditions for application of boolean contraction }
  CountFrobsBoolFull;
  CountFrobsBoolShort;
  CountFrobsMac;
  { test for faulty boolean contraction }
  test:='b';
  if (test='a') | (test='b') then
    test := 'OK'
  else
    ok:=false;
  if test <> 'OK' then WriteLn('Mac: fpc failed!');
  test:='b';
  if (test='a') or (test='b') then
    test := 'OK'
  else
    ok:=false;
  if test <> 'OK' then WriteLn('Short: fpc failed!');
  if not ok then
    halt(1);
end.
