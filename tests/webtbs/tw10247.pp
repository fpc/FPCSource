{$mode objfpc}{$h+}
uses classes, sysutils;
type
        generic TNode<T> = class
        type public
                PT = ^T;
        var private
                Data: T;
        public
                constructor Create;
                destructor Destroy; override;
        end;

        generic TContainer<T> = class
        type public
                TTNode = specialize TNode<T>;
        var
        private
                Data: TTNode;
        public
                constructor Create;
                destructor Destroy; override;

                function GetAddr: TTNode.PT;
                procedure SetV(v: TTNode.T);
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


procedure TContainer.SetV(v: TTNode.T);
begin
  Data.Data:=v;
end;

type
  TStringContainer=specialize TContainer<String>;
var
  c : TStringContainer;
begin
  c:=TStringContainer.Create;
  c.Set('abc');
  Writeln(HexStr(c.Get));
end.
