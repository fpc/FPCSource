program tw18620;
{$mode Delphi}

{ in delphi mode ^T in the var block of class/record/object should not create
  a forward definition which must be resolved after the type section end      }

type 
  C = class
    type 
      T = integer;
    var 
      V: ^T;
  end;

begin
end.