{ %opt=-gw3  }
program tlongname4;
(* Test manglednames
   Actual test code goes into unit, to trigger global labels

   Issue #42125
   This test failed only with debug info
*)
uses uverylongunitname4_123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345;
var Items:TItems;
begin
 Fill(Items);
 writeln(length(Items));
end.
