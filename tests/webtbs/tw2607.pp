{ %version=1.1 }
{ %opt=-vh }

{ Source provided for Free Pascal Bug Report 2607 }
{ Submitted by "Will" on  2003-07-27 }
{ e-mail: oohaynotihs@yahoo.com }
Program AbstractFunctionTest;

Type
        pAbstractObject = ^AbstractObject;
        AbstractObject  = Object
        Public

        Constructor Init;
        Destructor Done;
        Procedure Test; Virtual; Abstract;
        End;

Constructor AbstractObject.Init;
Begin
End;

Destructor AbstractObject.Done;
Begin
End;

Var
        Test    : ^AbstractObject;

Begin
        Test    := New(pAbstractObject, Init); {Obviously, this will cause a compiler error}
End.
