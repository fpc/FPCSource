{ %FAIL }

program tw26193;

{$mode delphi}

type
  TA<T> = class
    function Foo: Boolean; virtual; abstract;
  end;

  TB<T> = class(TA<byte>)
    // Missing (!) error: There is no method in an ancestor class to be overridden: "Foo;"
    procedure Foo; override;
  end;

procedure TB<T>.Foo;
begin
end;

begin
end.

