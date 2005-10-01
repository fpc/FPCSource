{$mode tp}

procedure display(s:string);
var L:longint;
begin
  for l:=1 to length(s) do
  begin
    case s[l]of
    '^':begin
          INC(L);
          case s[l] of
            'M':writeln;
          else write('^'=s[l]);
          end;
        end;
    else write(s[l]);
    end;
  end;
end;

begin
DISPLAY('^MHello Line One^M');
end.
