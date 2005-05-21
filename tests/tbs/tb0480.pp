{$ifdef fpc}{$mode delphi}{$endif}

procedure Test(const s1, s2: PAnsiChar);
begin
  Writeln(s1);
  Writeln(s2);
  if ansistring(s1)<>ansistring(s2) then
    begin
      writeln('Error');
      halt(1);
    end;
end;

var
  S: AnsiString;
  P: PAnsiChar;
begin
  S := 'Test';
  P := PAnsiChar(S);
  Test(PAnsiChar('String:'+S+';'), PAnsiChar('String:'+S+';'));
  Test(PAnsiChar('String:'+P+';'), PAnsiChar('String:'+P+';'));
end.
