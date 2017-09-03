program tb0631;

{$MODE DELPHI}

uses
  typinfo;

type
  {$M+}
  IFoo = interface
  ['{6AE439A1-06AA-460A-9CEB-71A1FD1BCFFB}']
    procedure SetFoo(a: pointer);
    property Foo: pointer write SetFoo;
  end;

begin
  if PInterfaceData(TypInfo.GetTypeData(TypeInfo(IFoo)))^.PropertyTable^.Prop[0]^.PropType 
    <> TypeInfo(Pointer) 
  then
    halt(1);
  WriteLn('ok');
end.

