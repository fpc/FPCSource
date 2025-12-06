{ %NORUN }

program tw41516b;{$Mode delphi}
type

  TFoo = class
    procedure       Foo2;
  end;

  TGen<T2: TObject> = class
    procedure       Foo2;
  end;

  TWrap1 = class
  public type
    TTest4<T2: TObject> = class
      procedure       Foo2;
    end;

  public
    procedure         P1;
  end;

var
  gTest4:  TWrap1.TTest4<TObject>;

{ TFoo }

procedure TFoo.Foo2;
var
  v2:  TWrap1.TTest4<TObject>;
begin
end;

procedure TGen<T2>.Foo2;
var
  v2:  TWrap1.TTest4<TObject>;
begin
end;

// TWRAP1

procedure TWrap1.TTest4<T2>.Foo2;
var     // V2: project1.lpr(50,32) Error: Parameters or result types cannot contain local type definitions. Use a separate type definition in a type block.
  v2:  TWrap1.TTest4<TObject>;
  v3:  TTest4<TObject>;
type
  x2 =  TWrap1.TTest4<TObject>;
  x3 =  TTest4<TObject>;
begin
end;

procedure TWrap1.P1;
var        // V2: project1.lpr(50,32) Error: Parameters or result types cannot contain local type definitions. Use a separate type definition in a type block.
  v2:  TWrap1.TTest4<TObject>;
  v3:  TTest4<TObject>;
type
  x2 =  TWrap1.TTest4<TObject>;
  x3 =  TTest4<TObject>;
begin
end;

begin
end.

