{ %opt=-gw  }
program tlongname1;
(* Test manglednames
   Actual test code goes into unit, to trigger global labels

   Issue #42125
   This test failed with or without debug info
*)

uses uverylongunitname1_123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345;

begin

end.

