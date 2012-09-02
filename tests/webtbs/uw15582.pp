unit uw15582;

{$MODE OBJFPC}{$H+}


{$codealign varmin=16}
{$codealign localmin=16}

interface

var 
  n_checks : integer = 0;
  n_failed : integer = 0;

procedure check(const v : string;p : pointer);
procedure l_unit(const pfx : string);
procedure l_unit_nostackframe;

implementation

var g1,g2,g3 : byte;
    g4 : integer;
    g5 : byte;
    g6 : array[0..39] of double;

procedure check(const v : string;p : pointer);
begin
  inc(n_checks);
  if (ptruint(p) and ptruint(-16)) <> ptruint(p) then begin
    writeln('Wrong aligned: "',v,'" : ',hexstr(p));
    inc(n_failed);
  end;
end;


procedure l_unit(const pfx : string);
var l1,l2,l3 : byte;
    l4 : integer;
    l5 : byte;
    l6 : array[0..39] of double;


begin
  check(pfx+'l_unit1',@l1);
  check(pfx+'l_unit2',@l2);
  check(pfx+'l_unit3',@l3);
  check(pfx+'l_unit4',@l4);
  check(pfx+'l_unit5',@l5);
  check(pfx+'l_unit6',@l6);
end;

procedure l_unit_nostackframe;
var
  b1, b2: byte;
begin
  inc(n_checks);
  if (ptruint(@b1) and ptruint(15)) <> 0 then
    inc(n_failed);
  inc(n_checks);
  if (ptruint(@b2) and ptruint(15)) <> 0 then
    inc(n_failed);
end;

initialization
  check('g_unit1',@g1);
  check('g_unit2',@g2);
  check('g_unit3',@g3);
  check('g_unit4',@g4);
  check('g_unit5',@g5);
  check('g_unit6',@g6);
  l_unit('ca_unit.initialization ');
end.
