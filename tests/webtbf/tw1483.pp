{ %fail }

Type pBug=^tBug;
     tBug=Object
       Private
        A:Longint;
        Go:Procedure Of Object;
        Procedure Go1;
       Public
        Constructor Init;
     End;

Constructor tBug.Init;
Begin
   A:=10;
   Go:=Go1; { <-- It's wring, it should        }
            {     be Go:=@Go1; but compiler    }
            {     says it's ok, and the program}
            {     even runs... }
End;

Procedure tBug.Go1;
Begin
   WriteLn(A);
End;

Var Bug:pBug;

Begin
   Bug:=New(pBug,Init);
   Bug^.Go;
End.
