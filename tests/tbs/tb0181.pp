{ Old file: tbs0214.pp }
{ bugs for static methods                               OK 0.99.11 (PM) }

Program SttcTest;
{ Note: I've cut a lot out of this program, it did originally have
        constructors, destructors and instanced objects, but this
        is the minimum required to produce the problem, and I think
        that this should work, unless I've misunderstood the use of
        the static keyword. }
Type
   TObjectType1 = Object
      Procedure Setup; static;
      Procedure Weird; static;
   End;

Procedure TObjectType1.Setup;
   Begin
   End;

Procedure TObjectType1.Weird;
   Begin
   End;

Begin
   TObjectType1.Setup;
   TObjectType1.Weird;
   TObjectType1.Weird; // GPFs before exiting "Weird"
   Writeln('THE END.');
End.
