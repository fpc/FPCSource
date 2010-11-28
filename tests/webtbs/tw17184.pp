{$MODE OBJFPC} { -*- text -*- }
program Test;

type
   generic Test1<T> = class end;
   generic Test2<T> = class
    type
      Test3 = specialize Test1<T>;
   end;
   Test4 = specialize Test2<Integer>;

begin
end.