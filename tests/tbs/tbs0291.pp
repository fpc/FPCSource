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
