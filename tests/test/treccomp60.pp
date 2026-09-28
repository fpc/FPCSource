{ %RECOMPILE }
{ Record composition: a composing record declared in a unit, loaded from the ppu,
  composed members are accessible in a program without modeswitch RecordComposition }
program record_compose_test;

{$Mode ObjFPC}{$H+}

uses
  ureccomp60;

var
  c: TComposed;
begin
  WriteLn('@c:         ', IntPtr(@c));
  WriteLn('@c.C:       ', IntPtr(@c.C));
  WriteLn('@c.child.C: ', IntPtr(@c.child.C));
  if (@c.C=@c.child.C) then
  begin
    WriteLn('ok');
    halt(0);
  end;
  halt(1);
end.
