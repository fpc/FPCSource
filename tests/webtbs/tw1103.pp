{$ifdef fpc}{$MODE OBJFPC }{$endif}
type
  TestRec = record
    fString  : AnsiString;
    fInt1    : Longint;
    fInt2    : Longint;
    fRetAddr : Longint;
  end;

function GetGroupInfo: TestRec;
begin
//  fillchar(Result, Sizeof(Result), 0);
  Result.fRetAddr := 0;
end;

function SelectGroup: TestRec;
begin
  Result := GetGroupInfo;
end;

procedure p;
begin
  SelectGroup;
end;

begin
  p;
end.
