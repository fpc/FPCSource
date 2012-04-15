{ %NORUN }

{$MODE OBJFPC} { -*- text -*- }
program tw19498;

type
   generic TFoo1 <T> = class
    type
     TFoo2 = class 
        constructor Create(Owner: specialize TFoo1<T>);
     end;
   end;

constructor TFoo1.TFoo2.Create(Owner: specialize TFoo1<T>);
begin
end;

type
   TIntegerFoo1 = specialize TFoo1<Integer>;

var 
   Foo1: TIntegerFoo1;
   Foo2: TIntegerFoo1.TFoo2; 

begin
end.

