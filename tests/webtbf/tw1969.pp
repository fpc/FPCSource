{ %fail }

{ Source provided for Free Pascal Bug Report 1969 }
{ Submitted by "Sergey Kosarevsky" on  2002-05-13 }
{ e-mail: netsurfer@au.ru }
Type tDemo=Object
        Static:Longint;
        Constructor Init;
        Procedure Run(Var Msg); Message 1;   // But this is an Object, not a Class !!! How can a make use of this declaration ?
     End;

Type tDemo1=Object
        Static:Longint;
        Constructor Init;
        Procedure Run; Dynamic;   // I caouldn't find this keyword in documentation
     End;

Constructor tDemo.Init;
Begin
End;

Constructor tDemo1.Init;
Begin
End;

Procedure tDemo.Run(Var Msg);
Begin
End;

Procedure tDemo1.Run;
Begin
End;

Begin
End.
