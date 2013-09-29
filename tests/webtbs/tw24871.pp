program tw24871;

{$mode delphi}
{$APPTYPE CONSOLE}

type
  TRec = record
    class procedure Foo(Self: TObject); static;
  end;

  { TClass }

  TClass = class
    class procedure Foo(Self: TObject); static;
  end;

{ TRec }

class procedure TRec.Foo(Self: TObject);

  procedure Foo1;

    procedure Foo2;
    begin
      Self.ClassName;
    end;

  begin
    Self.ClassName;
  end;

begin
  Self.ClassName;
end;

{ TClass }

class procedure TClass.Foo(Self: TObject);

  procedure Foo1;

    procedure Foo2;
    begin
      Self.ClassName;
    end;

  begin
    Self.ClassName;
  end;

begin
  Self.ClassName;
end;

begin
end.

