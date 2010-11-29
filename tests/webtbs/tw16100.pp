{$mode objfpc}
{$interfaces corba}
type
MyInterface = interface
end;

generic MyGenInterface<_T> = interface
    procedure MyProc(x:_T);
end;

MyGenInterface_Pointer = specialize MyGenInterface<Pointer>;

MyClass = class(MyInterface,MyGenInterface_Pointer)
    procedure MyProc(x:Pointer);
end;

procedure MyClass.MyProc(x:Pointer);
begin
end;

begin
end.
