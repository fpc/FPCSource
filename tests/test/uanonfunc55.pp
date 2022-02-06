unit uanonfunc55;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$modeswitch functionreferences}
{$modeswitch anonymousfunctions}

interface

type
  generic TFunc<T> = reference to function: T;

generic function Foo<T>: specialize TFunc<T>;

implementation

generic function Foo<T>: specialize TFunc<T>;
begin
  Result := function: T
            begin
              Result := Default(T);
            end;
end;

end.

