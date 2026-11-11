{ combining char arrays of nested statement expressions }
{$Mode Delphi}

procedure Check(const Actual, Expected: string; Code: integer);
begin
  if Actual<>Expected then
  begin
    writeln('Error ',Code,': expected "',Expected,'", got "',Actual,'"');
    Halt(Code);
  end;
end;

const
  StrConst1 = '12';
  StrConst2 = '12';
  StrConst3 = '1';
  WCharConst: WideChar = 'w';
var
  b, c: Boolean;
  s: string;
  ws: widestring;
begin
  b:=false;
  c:=true;

  // equal length constants and empty string
  s:=if b then StrConst1 else if b then StrConst2 else '';
  Check(s,'',1);
  s:=if b then StrConst1 else if c then StrConst2 else '';
  Check(s,'12',2);
  s:=if c then StrConst1 else if b then StrConst2 else '';
  Check(s,'12',3);

  // later branch shorter
  s:=if b then StrConst1 else if c then StrConst3 else '';
  Check(s,'1',4);
  s:=if b then StrConst1 else if b then StrConst3 else 'xy';
  Check(s,'xy',5);

  // char and empty string
  s:=if b then 'x' else '';
  Check(s,'',13);
  s:=if c then 'x' else '';
  Check(s,'x',14);
  s:=if b then '' else 'x';
  Check(s,'x',15);
  s:=if b then StrConst3 else '';
  Check(s,'',16);
  ws:=if c then WCharConst else '';
  if ws<>'w' then
    Halt(17);
  ws:=if c then '' else WCharConst;
  if ws<>'' then
    Halt(18);

  // two nested non-constant char arrays of different lengths
  s:=if b then (if c then 'ab' else 'c') else (if c then 'xyz' else 'q');
  Check(s,'xyz',6);
  s:=if c then (if c then 'ab' else 'c') else (if c then 'xyz' else 'q');
  Check(s,'ab',7);
  s:=if b then (if c then 'xyz' else 'q') else (if c then 'ab' else 'c');
  Check(s,'ab',8);
  s:=if c then (if c then 'xyz' else 'q') else (if c then 'ab' else 'c');
  Check(s,'xyz',9);

  // case expression with nested if
  s:=case b of true: StrConst1; else if c then StrConst2 else '' end;
  Check(s,'12',10);
  s:=case c of true: StrConst1; else if c then StrConst3 else '' end;
  Check(s,'12',11);
  s:=case b of true: 'ab'; else if c then 'x' else 'yz' end;
  Check(s,'x',12);

  writeln('ok');
end.
