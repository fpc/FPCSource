{ %cpu=x86_64,i386,powerpc,powerpc64}
{ %skiptarget=linux,freebsd,netbsd,openbsd,win32,os2,emx,go32v2}

{ should actually only skip i386-variants of win32/linux/.. for now, but that can't be specified }

{ test can only work correctly (for now) on targets with 16-byte aligned stacks }

program tw15582;

{$MODE OBJFPC}{$H+}

{$codealign varmin=16}
{$codealign localmin=16}

uses
  uw15582;

var g1,g2,g3 : byte;
    g4 : integer;
    g5 : byte;
    g6 : array[0..39] of double;

procedure l;
var l1,l2,l3 : byte;
    l4 : integer;
    l5 : byte;
    l6 : array[0..39] of double;

begin
  check('l1',@l1);
  check('l2',@l2);
  check('l3',@l3);
  check('l4',@l4);
  check('l5',@l5);
  check('l6',@l6);
end;


begin
  check('g1',@g1);
  check('g2',@g2);
  check('g3',@g3);
  check('g4',@g4);
  check('g5',@g5);
  check('g6',@g6);
  l;
  l_unit('main ');
  l_unit_nostackframe;
  writeln(n_checks,' tests. ',n_failed,' failed');
  if n_failed > 0 then 
    halt(1); 
end.
