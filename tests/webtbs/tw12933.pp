{$codepage cp866}

{ warning: this test will terminate successfully when run on systems that do
  not support the character used below in the current code page, even if the
  used compiler is buggy. On other systems, the test will properly fail if
  the compiler is buggy.
}

{$ifdef unix}
uses
  cwstring;
{$endif}
var
  s, s2: ansistring;
  ws, ws2, ws3: widestring;
begin
    s := '£';
    writeln(s);

    ws := s;
    writeln(ws);

    ws2 := '££';
    writeln(ws2);
    s2:=ws2;
    ws2:=s2;

    ws3 := '£';
    writeln(ws3);
    s2:=ws3;
    ws3:=s2;

    delete(ws2,1,1);

    if (ws<>ws2) or
       (ws<>ws3) then
      halt(1);
end.
