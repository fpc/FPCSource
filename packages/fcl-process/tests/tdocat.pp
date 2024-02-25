{%norun}
{%neededafter}

program docat;

Procedure ReadAndWrite(var O : Text);

var
  S : AnsiString;

begin
  While not EOF(O) do
    begin
    Readln(O,S);
    Writeln(S);
    end;
end;

var
  F : Text;

begin
  if ParamStr(1)<>'' then
    begin
    Assign(F,ParamStr(1));
    Reset(F);
    ReadAndWrite(F);
    end
  else
    ReadAndWrite(INput);
end.

