{ %OPT=-Os -O1 -MDelphi }

{ Tests erroneous CMOV optimisation under x86 }

program tw40122;

uses
  Math;

function VerifyAccessRange(var nFrom, nTo: Integer; const Size: Integer): Integer;
begin
  Result:=0;
  nFrom:=Min(nFrom,nTo);
  nTo:=Max(nFrom,nTo);
  if (nFrom<=0) then
  begin
    nFrom:=1;
    Result:=Size;
  end;
  if (nTo>Size) then
  begin
    nTo:=Size;
    Result:=Size;
  end;
  WriteLn('- Output Range: nfrom = ',nfrom, '; nto = ', nto);
end;

function CheckLimits(L, H, S: Integer): Boolean;
var
  ns, ne, nSize: Integer;
begin
  Result := False;
  ns := L; ne := H;
  WriteLn('Testing: ', L, '..', H, ' -> 1..', S, '...');
  nSize := VerifyAccessRange(ns,ne,S);
  WriteLn('Output value (0 or Size): ', nSize);
 
  if (nSize <> 0) and (nSize <> S) then
  begin
    WriteLn('- FAIL: Size was modified');
    Exit;
  end;

  WriteLn('- Final result: ', ns, '..', ne);
  
  if (L <= 0) then
  begin
    if (ns <= 0) then
    begin
      WriteLn('- FAIL: Low range wasn''t adjusted');
      Exit;
    end
    else if (ns <> 1) then
    begin
      WriteLn('- FAIL: Low range wasn''t equal to 1');
      Exit;
    end;
  end
  else
  begin
    if (ns <> L) then
    begin
      WriteLn('- FAIL: Low range should not have been adjusted');
      Exit;
    end;
  end;
    
  if (H > S) then
  begin
    if (ne > S) then
    begin
      WriteLn('- FAIL: High range wasn''t adjusted');
      Exit;
    end
    else if (ne <> S) then
    begin
      WriteLn('- FAIL: High range wasn''t equal to ', S);
      Exit;
    end;
  end
  else
  begin
    if (ne <> H) then
    begin
      WriteLn('- FAIL: High range should not have been adjusted');
      Exit;
    end;
  end;

  WriteLn('- Pass');
  Result := True;
end;

const
  LArray: array[0..3] of Integer = (1,     -10,   1,     -10);
  RArray: array[0..3] of Integer = (71299, 30000, 30000, 71299);
  SArray: array[0..3] of Integer = (65535, 65535, 65535, 65535);

var
  X: Integer;
begin
  for X := 0 to 3 do
    if not CheckLimits(LArray[X], RArray[X], SArray[X]) then
      Halt(1);
      
  WriteLn('ok');
end.