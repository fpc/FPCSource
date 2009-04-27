{ %norun }
{ %recompile }

{$inline on}

program BothScreens3D;

uses
  tw13596a;

begin
  gluPerspective(70, 256.0 / 192.0, 0.1, 100);
end.

