{ %FAIL}
program tclass11b;

{$ifdef fpc}
  {$mode delphi}
{$endif}
{$apptype console}

type
  TSomeClass = class
  strict private
    type
      TPrivateNestedClass = class
      public
        procedure DoSomething;
      end;
  public
    type
      TNestedClass = class
      public
        procedure DoSomething;
      end;
    class procedure Test; virtual;
  end;

  TDescendant = class(TSomeClass)
  public
    class procedure Test; override;
  end;

procedure TSomeClass.TPrivateNestedClass.DoSomething;
begin
  WriteLn('TSomeClass.TPrivateNestedClass.DoSomething: ok');
end;

procedure TSomeClass.TNestedClass.DoSomething;
begin
  WriteLn('TSomeClass.TNestedClass.DoSomething: ok');
end;

class procedure TSomeClass.Test;
var
  P: TPrivateNestedClass;
  N: TNestedClass;
begin
  P := TPrivateNestedClass.Create;
  P.DoSomething;
  P.Free;
  N := TNestedClass.Create;
  N.DoSomething;
  N.Free;
end;

class procedure TDescendant.Test;
var
  P: TPrivateNestedClass;
  N: TNestedClass;
begin
  P := TPrivateNestedClass.Create;
  P.DoSomething;
  P.Free;
  N := TNestedClass.Create;
  N.DoSomething;
  N.Free;
end;

begin
  TSomeClass.Test;
  TDescendant.Test;
end.
