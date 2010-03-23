{ %norun }
program tw16083;

{$ifdef fpc}
  {$mode delphi}
{$endif}

type
  TClass1 = class
  public type
    TNestedClass = class
    end;

    TNestedClass2 = class(TNestedClass)
    end;
  end;

  TClass3 = class(TClass1.TNestedClass)
  end;

begin
end.