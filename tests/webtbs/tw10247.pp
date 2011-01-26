{$mode objfpc}{$h+}
uses classes, sysutils;
type
  generic TNode<T> = class
  public
    type
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
    procedure SetV(v: TTNode.T);
  end;

constructor TNode<T>.Create;
begin
end;

destructor TNode<T>.Destroy;
begin
  inherited Destroy;
end;

constructor TContainer<T>.Create;
begin
  Data:=TTNode.Create;
end;

destructor TContainer<T>.Destroy;
begin
  Data.Free;
        inherited Destroy;
end;

function TContainer<T>.GetAddr: TTNode.PT;
begin
        result := @Data.Data;
end;


procedure TContainer<T>.SetV(v: TTNode.T);
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
