{ %NORUN }

{$MODE OBJFPC} { -*- text -*- }
program tw19511;

type
   generic TFoo<X> = class
   end;
   generic TBar<Y> = class
    type
     TIntegerSpecializedFoo = specialize TFoo<Integer>;
     TSelfSpecializedFoo = specialize TFoo<TBar>;
    function SelfTest(): TBar; // returns a TBar<Y>
      TSpecializedBar = specialize TBar<Y>; // this rightly would not compile since TBar here refers to the specialized TBar<Y>
   end;

function TBar.SelfTest(): TBar;
begin
   Result := Self;
end;

begin
end. 

