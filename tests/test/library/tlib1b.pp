{ %skiptarget=go32v2 }
{ %needlibrary }
{$goto on}


{ Checks that the two functions with the same exported name 'p'
  are each loaded correctly. }
uses
  {$ifdef unix}dl,{$endif unix}sysutils;

{$ifdef darwin}
{$linklib tlib1a}
{$linklib tlib1a2}
{$endif darwin}

procedure p(var a : dword);external 'tlib1a' name 'p';
procedure p2(var a : dword);external 'tlib1a2' name 'p';

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
          'of tlib1a2 p function.');
      halt(2);
    end;

  writeln('ok');
end.
