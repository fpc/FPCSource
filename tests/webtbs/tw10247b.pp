{$mode objfpc}{$h+}
type
  generic TNode<T> = class
  public
    type
      PT = T;
  private
    var
      Data: T;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TTNodeLongint = specialize TNode<Longint>;

  TTNodeString = specialize TNode<String>;

constructor TNode.Create;
begin
end;

destructor TNode.Destroy;
begin
  inherited Destroy;
end;


function GetIntNode: TTNodeLongint.T;
begin
  result := 10;
end;


function GetStringNode: TTNodeString.PT;
begin
  result := 'abc';
end;

begin
  writeln(GetIntNode);
  writeln(GetStringNode);
end.

