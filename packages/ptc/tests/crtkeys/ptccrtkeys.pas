program ptccrtkeys;
uses
  ptccrt, ptcgraph;
var
  Gd, Gm: Integer;
  Ch, Ex: Char;
  Done: Boolean;
begin
  Gd := VGA;
  Gm := VGAHi;
  InitGraph(Gd, Gm, '');
  Writeln('startup KeyMode (press ''m'' to switch): ', KeyMode);
  Done := False;
  repeat
    Ch := ReadKey;
    if Ch = #0 then
      Ex := ReadKey
    else
      Ex := #0;
    Writeln(Ord(Ch), ' ', Ord(Ex));
    if Ch = 'm' then
    begin
      if KeyMode <> High(KeyMode) then
        KeyMode := Succ(KeyMode)
      else
        KeyMode := Low(KeyMode);
      Writeln('KeyMode: ', KeyMode);
    end;
    if Ch = 'q' then
      Done := True;
  until Done;
  CloseGraph;
end.
