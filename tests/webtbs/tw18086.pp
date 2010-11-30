program tw18086;

{$mode delphi}

type
  TFoo1 = class; //Error: Type "TFoo1" is not completely defined

  TFoo2 = class //it compiles if TFoo2 is removed
  type
    TFoo3 = class
    end;
  end;

  TFoo1 = class
  end;

begin
end.

