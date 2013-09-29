{ %NORUN }

program tw21051;

{$mode Delphi}{$H+}

type
  TCustomInner<T> = class abstract
  protected
    function SomeMethod: T; virtual; abstract;
  end;

  TContainer<T> = class
  public
    function GetInner: TCustomInner<T>;
  end;

  TInner<T> = class(TCustomInner<T>)
  private
    FContainer: TContainer<T>;
  protected
    function SomeMethod: T; override;
  public
    constructor Create(AContainer: TContainer<T>);
  end;


function TContainer<T>.GetInner: TCustomInner<T>;
type
  InnerClass = TInner<T>;
begin
  Result := InnerClass.Create(Self);
end;

function TInner<T>.SomeMethod: T;
begin

end;

constructor TInner<T>.Create(AContainer: TContainer<T>);
begin
  FContainer := AContainer;
end;

procedure Test;
var
  C: TContainer<string>;
begin
  C := TContainer<string>.Create;
end;

begin
  Test;
end.
