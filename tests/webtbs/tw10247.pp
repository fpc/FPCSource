{$mode objfpc}{$h+}
uses classes, sysutils;
type
  generic TNode<T> = class
  public
    type
      TAlias = T;
      PT = ^T;
  private
    var
      Data: T;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  generic TContainer<T> = class
  public
    type
      TTNode = specialize TNode<T>;
  private
    var
      Data: TTNode;
  public
    constructor Create;
    destructor Destroy; override;

    function GetAddr: TTNode.PT;
    procedure SetV(v: TTNode.TAlias);
  end;

constructor TNode.Create;
begin
end;

destructor TNode.Destroy;
begin
  inherited Destroy;
end;

constructor TContainer.Create;
begin
  Data:=TTNode.Create;
end;

destructor TContainer.Destroy;
begin
  Data.Free;
        inherited Destroy;
end;

function TContainer.GetAddr: TTNode.PT;
begin
        result := @Data.Data;
end;


procedure TContainer.SetV(v: TTNode.TAlias);
begin
  Data.Data:=v;
end;

type
  TStringContainer=specialize TContainer<String>;
var
  c : TStringContainer;
begin
  c:=TStringContainer.Create;
  c.SetV('abc');
  Writeln(HexStr(c.GetAddr));
end.
