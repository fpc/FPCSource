{ Old file: tbs0291.pp }
{ @procvar in tp mode bugss                             OK 0.99.13 (PFV) }

{$mode tp}

function ReturnString: string;
begin
  ReturnString := 'A string';
end;

procedure AcceptString(S: string);
begin
  WriteLn('Got: ', S);
end;

type
  TStringFunc = function: string;

const
  SF: TStringFunc = ReturnString;
var
  S2: TStringFunc;
begin
  @S2:=@ReturnString;
  AcceptString(ReturnString);
  AcceptString(SF);
  AcceptString(S2);
end.
