{$inline on}

function leftstr(const s: string; l: integer): ansistring; inline;
var
  i: longint;
begin
  i:=1;
  while i<length(s) do
    begin
      if s[i]=' ' then
        exit(copy(s,1,i-1));
      inc(i);
    end;
  leftstr:=s;
end;

var
  Line: String;
begin
  Line := 'astring2     ';
  case LeftStr(Line,1) of
    'astring1':
      halt(1);
    'astring2': // comment this line and everything works
       halt(0);
     else
       halt(2);
  end;
end.

