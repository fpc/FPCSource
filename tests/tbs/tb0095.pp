{ Old file: tbs0113.pp }
{ point initialization problems                         OK 0.99.1 (PM/FK) }

program test;

type pRecord = ^aRecord;
     aRecord = record
                     next : pRecord;
                     a, b, c : integer;
               end;

const rec1 : aRecord = (next : nil; a : 10; b : 20; c : 30);
      rec2 : aRecord = (next : @rec1; a : 20; b : 30; c : 40);

begin
end.
