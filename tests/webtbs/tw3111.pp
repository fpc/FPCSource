var
  Start : Cardinal;
  Run,Text: PWideChar;
  RunLen: Cardinal;

begin
  Start:=100;
  Run:=nil;
  Text:=Run+10;
  Start:=Start+Run-Text;
  if Start<>90 then
    begin
      writeln('error');
      halt(1);
    end;
end.
