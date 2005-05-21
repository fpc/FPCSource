{ Old file: tbs0273.pp }
{ small array pushing to array of char procedure is wrong OK 0.99.13 (PFV) }

Program CharArr;

Var CharArray : Array[1..4] Of Char;

    S : String;

Begin
 CharArray:='BUG?';
 S:=CharArray;
 WriteLn(S);         { * This is O.K. * }
 WriteLn(CharArray); { * GENERAL PROTECTION FAULT. * }
 if CharArray<>'BUG?' then
   begin
     Writeln('Error comparing charaay to constant string');
     Halt(1);
   end;
End.
