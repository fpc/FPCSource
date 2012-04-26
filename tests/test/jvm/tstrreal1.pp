
program tstrreal1;

uses
  {$ifdef java}jdk15{$else}androidr14{$endif};

{$macro on}
{$define write:=JLSystem.fout.print}
{$define writeln:=JLSystem.fout.println}

const
  s: array[0..16] of string[13] =
    ('99999.900000',
     '99999.990000',
     '99999.999000',
     '99999.999900',
     '99999.999990',
     '99999.999999',
     '100000.000000',
     '100000.000000',
     '100000.000000',
     '100000.000000',
     '100000.000000',
     '100000.000000',
     '100000.000000',
     '100000.000000',
     '100000.000000',
     '100000.000000',
     '100000.000000');

var
  e,e2,e3: double;
  s2: string;
  c: longint;

begin
  e := 100000.0;
  e2 := 0.1;
  c := 0;
  repeat
    e3 := e-e2;
    str(e3:0:6,s2);
    writeln(s2);
    if s2 <> s[c] then
      begin
        write('  Error, should be '); writeln(s[c]);
        halt(1);
      end;
    e2 := e2 /10.0;
    inc(c);
  until e2 < 1e-17;
end.
