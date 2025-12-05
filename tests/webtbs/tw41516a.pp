{ %NORUN }

program tw41516a;{$Mode objfpc}
type

  TFoo = class
    procedure       Foo2;
  end;

  generic TGen<T2: TObject> = class
    procedure       Foo2;
  end;

  TWrap1 = class
  public type
    generic TTest4<T2: TObject> = class
      procedure       Foo2;
    end;

  public
    procedure         P1;
  end;

var
  gTest4:  TWrap1.specialize TTest4<TObject>;

{ TFoo }

procedure TFoo.Foo2;
var
  v2:  TWrap1.specialize TTest4<TObject>;
begin
end;

procedure TGen.Foo2;
var
  v2:  TWrap1.specialize TTest4<TObject>;
begin
end;

// TWRAP1

procedure TWrap1.TTest4.Foo2;
var     // V2: project1.lpr(50,32) Error: Parameters or result types cannot contain local type definitions. Use a separate type definition in a type block.
  v2:  TWrap1.specialize TTest4<TObject>;
  v3:  specialize TTest4<TObject>;
type
  x2 =  TWrap1.specialize TTest4<TObject>;
  x3 =  specialize TTest4<TObject>;
begin
end;

procedure TWrap1.P1;
var        // V2: project1.lpr(50,32) Error: Parameters or result types cannot contain local type definitions. Use a separate type definition in a type block.
  v2:  TWrap1.specialize TTest4<TObject>;
  v3:  specialize TTest4<TObject>;
type
  x2 =  TWrap1.specialize TTest4<TObject>;
  x3 =  specialize TTest4<TObject>;
begin
end;

begin
end.

