{$mode objfpc}

// Compile it using the Delphi extensions
// directive.

type
    TObject = class
      private
        procedure SetValue(v: integer);
      public
        fx: integer;
        Constructor Create;
        Destructor Destroy;
        property x: integer read fx write SetValue;
    end;

var
    Obj: TObject;

Constructor TObject.Create;
begin
    fx := 0;
end;

Destructor TObject.Destroy;
begin
end;

procedure TObject.SetValue(v: integer);
begin
    fx := v + 2;
end;

begin
    writeln('This will be printed');
    Obj := TObject.Create;
    writeln('This won''t.');
end.
