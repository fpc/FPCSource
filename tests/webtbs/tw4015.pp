{ Source provided for Free Pascal Bug Report 4015 }
{ Submitted by "Radoslaw Stachowiak" on  2005-05-25 }
{ e-mail: zenek_tm@tenbit.pl }
unit tw4015;
{$mode objfpc}
{$inline on}
{$H+}
interface

type
  TComponent = class;

  { TComponentContainer }
  TComponentContainer = class
  private
    acount: integer;
    components: array of TComponent;
    function fGet(ind: integer): TComponent; inline;
  public
    constructor Create;
    destructor Destroy; override;
    property Component[ind: integer]: TComponent read fGet; default;
    property Count: integer read acount;
  end;


  { TComponent }
  TComponent = class
  protected
    achildren: TComponentContainer;
  public
    constructor Create(const parent: TComponent);
    destructor Destroy; override;
  end;

implementation

{ TComponentContainer }
function TComponentContainer.fGet(ind: integer): TComponent; inline;
begin
  if (ind>=acount) or (ind<0) then
    result := nil
  else
    result := components[ind];
end;


constructor TComponentContainer.Create;
begin
  inherited Create;

  acount:=0;
  SetLength(components, 10);
end;

destructor TComponentContainer.Destroy;
begin
  inherited Destroy;
end;

{ TComponent }
constructor TComponent.Create(const parent: TComponent);
begin
  inherited Create;
  achildren := TComponentContainer.Create;
end;

destructor TComponent.Destroy;
var
  i: integer;
begin
  for i:=0 to achildren.Count-1 do
  begin
    achildren[i].Free(); //Internal Error 200108231
    {if above line is changed to (var c: TComponent):
     c := achildren[i]; //Internal Error 200108231
     c.Free();
     }
  end;
  achildren.Free;
  inherited Destroy;
end;

end.
