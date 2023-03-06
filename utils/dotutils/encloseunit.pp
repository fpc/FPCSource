
{$mode objfpc}
{$H+}

var
  S : String;
  
begin  
  Writeln('{$IFNDEF FPC_DOTTEDUNITS}');
  While not EOF do
    begin
    Readln(S);
    Writeln(S);
    end;
  Writeln('{$ENDIF FPC_DOTTEDUNITS}');
end.
