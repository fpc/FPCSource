{ from GPC testsuite (martin3v.pas) }

{$bitpacking on}
Unit uprec7;
Interface

Type TRecord = packed Record
                 a, b, c, d, e, f, g, h, i: Boolean
               end;

Var ARecord:TRecord = (a: False; b: True; c: True; d: True; e: False;
                       f: True; g: False; h: False; i: True);
    s: Integer;

Implementation

Begin
  s := SizeOf (TRecord)
end.
