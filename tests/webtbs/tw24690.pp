unit tw24690;

{$mode objfpc}{$H+}

interface

type
  generic TMyGenericType<_T> = class
  public
  end;

  { TMyClass }

  generic TMyClass<_T> = class
  public
    type
      TMyGenericTypeSpec = specialize TMyGenericType<_T>;

  public
    procedure Test;
  end;

  { TMyClass2 }

  generic TMyClass2<_T> = class
  public
    type
      TMyClassSpec = specialize TMyClass<Integer>;

  public
    procedure Test2;
  end;

implementation

{ TMyClass2 }

procedure TMyClass2.Test2;
var
  Enum: TMyClassSpec.TMyGenericTypeSpec; //Error: Error in type definition
begin
  Enum := TMyClassSpec.TMyGenericTypeSpec.Create;
  Enum.Destroy;
end;

{ TMyClass }

procedure TMyClass.Test;
begin

end;


end.

