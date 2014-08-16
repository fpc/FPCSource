{ %target=win32,win64 }
{ %needlibrary }


uses
  ulib2a, ulib2b;
{ Checks that the two functions with the same exported name 'p'
  are each loaded correctly. }
procedure p(var a : dword);external 'tlib1a' name 'p';
procedure p2(var a : dword);external 'tlib1b' name 'p';

var
  a : dword;
begin
  a:=0;
  p(a);
  if a <> 1 then
    halt(1);
  a:=0;
  p2(a);
  if a <> 2 then
    begin
      if a=1 then
        writeln('Error: Calling tlib1a library p function again instead ',
          'of tlib1b p function.');
      halt(2);
    end;

  ulib2b.p(a);
  if a <> 1 then
    halt(1);
  a:=0;
  ulib2b.p2(a);
  if a <> 2 then
    begin
      if a=1 then
        writeln('Error: Calling tlib1a library p function via ulib2b unit again instead ',
          'of tlib1b p function.');
      halt(2);
    end;

  ulib2a.p(a);
  if a <> 1 then
    halt(1);
  a:=0;
  ulib2a.p2(a);
  if a <> 2 then
    begin
      if a=1 then
        writeln('Error: Calling tlib1a library p function via ulib2a unit again instead ',
          'of tlib1b p function.');
      halt(2);
    end;

  ulib2a.p3(a);
  if a <> 1 then
    begin
      if a=1 then
        writeln('Error: Calling tlib1a library p function via ulib2a unit again instead ',
          'of tlib1b p function.');
      halt(2);
    end;

  writeln('ok');
end.
