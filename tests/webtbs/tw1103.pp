{$ifdef fpc}{$MODE OBJFPC }{$endif}
type
  PTestRec = ^TestRec;
  TestRec = record
    fString  : AnsiString;
    fInt1    : Longint;
    fInt2    : Longint;
    fRetAddr : Longint;
  end;

function GetGroupInfoP: PTestRec;
var
  s : string;
begin
  new(Result);
  s:=' Wr';
  Result^.fString := 'Test' + s;
  Result^.fRetAddr := 0;
end;

function GetGroupInfo: TestRec;
var
  s : string;
begin
  s:=' Wr';
  Result.fString := 'Test' + s;
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
  s : shortstring;
  p : pchar;
  i : longint;
begin
  for i:=0 to 255 do
   s[i]:=#$90;
  getmem(p,sizeof(TestRec));
  for i:=0 to sizeof(TestRec)-1 do
   p[i]:=#$ff;
  freemem(p);
end;

var
  p1 : PTestRec;
begin
  destroystack;
  p;
  p1:=GetGroupInfoP;
  dispose(p1);
end.
