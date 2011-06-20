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
  p2(a);
  if a <> 2 then
    halt(2);

  writeln('ok');
end.
