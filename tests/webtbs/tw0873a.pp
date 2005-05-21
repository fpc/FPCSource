{$mode objfpc}

// Compile it using the Delphi extensions
// directive.

type
    TObjectB = class
      private
        procedure SetValue(v: integer);
      public
        fx: integer;
        Constructor Create;
        Destructor Destroy;
        property x: integer read fx write SetValue;
    end;

var
    Obj: TObjectB;

Constructor TObjectB.Create;
begin
    fx := 0;
end;

Destructor TObjectB.Destroy;
begin
end;

procedure TObjectB.SetValue(v: integer);
begin
    fx := v + 2;
end;

begin
    writeln('This will be printed');
    Obj := TObjectB.Create;
    writeln('This won''t.');
end.
