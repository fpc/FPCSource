{ %NORUN }

program tw24458;

{$mode delphi}

type
  TA<T: class> = class
  public
    procedure Foo(const AValue: T);
  end;

procedure TA<T>.Foo(const AValue: T);
begin
  AValue.Free; // Error: identifier idents no member "Free"
end; 

begin

end.
