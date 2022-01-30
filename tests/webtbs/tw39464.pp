{ %norun }
program Test;

{$Mode ObjFPC}

type
  TTestRec = packed record
    Empty: packed record end;
  end;

function GetEmptyPtr(R: TTestRec): Pointer;
begin
  Result := @R.Empty;
end;
    
begin
end.
