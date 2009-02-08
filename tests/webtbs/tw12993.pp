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
  s: ansistring;
  ws, ws3: widestring;
begin
    // must not be converted
    s := '£';
    if (length(s)<>1) or
       (s[1]<> #163) then
      halt(1);

    ws := '££';
    writeln(ws);
    s:=ws;
    ws:=s;

    ws3 := '£';
    if ws3[1]<>ws[1] then
      halt(2);
    writeln(ws3);
    s:=ws3;
    ws3:=s;

    delete(ws,1,1);

    if (ws<>ws3) then
      halt(3);
end.
