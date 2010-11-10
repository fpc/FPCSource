// A basic test for TCustomVariantType creation/registration

{$ifdef fpc}{$mode objfpc}{$h+}{$endif}

uses Variants, SysUtils;

type
  TTest = class(TCustomVariantType)
    procedure Clear(var V: TVarData); override;
    procedure Copy(var Dest: TVarData; const Source: TVarData; const Indirect: Boolean); override;
  end;

procedure TTest.Clear(var V: TVarData);
begin
  SimplisticClear(V);
end;

procedure TTest.Copy(var Dest: TVarData; const Source: TVarData; const Indirect: Boolean);
begin
  SimplisticCopy(Dest, Source, Indirect);
end;


var
  cv, cv1: TCustomVariantType;
  code: Integer;
  Flag: Boolean;

begin
  Code := 0;
  { Test #1. Create a TCustomVariantType, it should receive VarType=$10F }
  cv := TTest.Create;
  writeln('first vartype=', cv.VarType);
  if cv.VarType <> $10F then
    Code := Code or 1;

  { Test #2. Try RequestedVarType that is too low, must be rejected. }
  Flag := False;
  try
    TTest.Create($10E);
  except
    on E: Exception do
    begin
      writeln('Test 2: ', E.Message);
      if E is EVariantError then
        Flag := True;
    end;
  end;
  if not Flag then
    Code := Code or 2;

  { Test #3. Try RequestedVarType that is too high, must be rejected. }
  Flag := False;
  try
    TTest.Create($1000);
  except
    on E: Exception do
    begin
      writeln('Test 3: ', E.Message);
      if E is EVariantError then
        Flag := True;
    end;
  end;
  if not Flag then
    Code := Code or 4;

  { Test #4. Try RequestVarType=$10F, must be rejected because this slot was occupied in test #1 }
  Flag := False;
  try
    TTest.Create($10F);
  except
    on E: Exception do
    begin
      writeln('Test 4: ', E.Message);
      if E is EVariantError then
        Flag := True;
    end;
  end;
  if not Flag then
    Code := Code or 8;

  { Test #5. Verify that our test type can be found VarType... }
  cv1 := nil;
  if (not FindCustomVariantType($10F, cv1)) or (cv1 <> cv) then
    Code := Code or 16;

  { Test #6. ... and by name (case-insensitive) }
  cv1 := nil;
  if (not FindCustomVariantType('TtEsT', cv1)) or (cv1 <> cv) then
    Code := Code or 32;

  { Test #7. Ok, now free cv and try again. The slot must remain occupied... }
  cv.Free;
  Flag := False;
  try
    TTest.Create($10F);
  except
    on E: Exception do
    begin
      writeln('Test 7: ', E.Message);
      if E is EVariantError then
        Flag := True;
    end;
  end;
  if not Flag then
    Code := Code or 64;

  { Test #8. ...but the type should no longer be found. }
  cv1 := nil;
  if FindCustomVariantType($10F, cv1) then
    Code := Code or 128;

  { Test #9. also by name }
  cv1 := nil;
  if FindCustomVariantType('TtEsT', cv1) then
    Code := Code or 256;

  { Test #10. Request a valid slot, should succeed }
  cv := TTest.Create($110);
  if cv.VarType <> $110 then
    Code := Code or 512;
    
  { Test #11. Now creating another customVariantType should skip the occupied slot.
    Delphi 7 fails this test miserably. }
  try
    cv1 := TTest.Create;
    if cv1.VarType <> $111 then
      Code := Code or 1024;
  except
    Code := Code or 2048;
  end;

  if Code <> 0 then
    writeln('Errors: ', Code);
  Halt(Code);

end.