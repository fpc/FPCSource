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
  Result.fString := 'Test';
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

procedure destroystack;
var
  s : string;
  i : longint;
begin
  for i:=0 to 255 do
   s[i]:=#$90;   
end;  

begin
//  destroystack;
  p;
end.
