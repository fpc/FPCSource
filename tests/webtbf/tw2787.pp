{ %fail }

{$mode objfpc}

Type tClass1=Class
       Public
        Constructor Init;Virtual;
     End;

Type tClass2=Class(tClass1)
       Public
        Constructor Init;Override;Virtual;Overload;
     End;

Constructor tClass1.Init;
Begin
   Inherited Create;
End;

Constructor tClass2.Init;
Begin
   Inherited Init;
End;

Begin
End.
