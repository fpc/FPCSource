{$MODE OBJFPC }
type
  TestRec = record
    fString  : AnsiString;
    fInt1    : Longint;
    fInt2    : Longint;
    fRetAddr : Longint;
  end;

function GetGroupInfo: TestRec;
begin
  fillchar(Result, Sizeof(Result), 0);
  Result.fRetAddr := 0;
end;

function SelectGroup: TestRec;
begin
  Result := GetGroupInfo;
end;

begin
  SelectGroup;
end.
