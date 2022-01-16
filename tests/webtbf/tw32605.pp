{ %fail }

{$ifdef fpc}
{$mode delphi}
{$endif}

program InlineClass;

  type
    TAncestor = class
      public
        procedure TestMethod; virtual;
    end;

    TDerived = class(TAncestor)
      public
        procedure TestMethod; override;
    end;

procedure TAncestor.TestMethod; inline; // Virtual method with an 'inline' hint.
begin
  WriteLn('Ancestor Method');
end;

procedure TDerived.TestMethod;
begin
  WriteLn('Derived Method');
end;

var
  TestClass: TAncestor;
begin
  TestClass := TDerived.Create;
  try
    TestClass.TestMethod; // <-- TAncestor.TestMethod is called instead of TDerived.TestMethod
  finally
    TestClass.Free;
  end;
end.
