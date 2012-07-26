{$mode objfpc}

unit uw22495;

interface

uses
  DOM;

type
  TNode = class;

  INode = interface
  ['{DCE90E41-4D14-4BAE-9AFC-BA10FF643EA0}']
    function GetChildNodes: TNode;           // if you delete this line, then the error disappears
  end;

  TNode = class(TInterfacedObject, INode)
  protected
    function GetChildNodes: TNode;
    function xxx:TDomNode;                    // if you do either of the next points, then the error disappears:
                                              // - exchange this line with the next one, or
                                              // - delete this procedure (along with its body), or
                                              // - make this procedure public
    function getOwnerDocument: TDOMDocument;  // if you delete this procedure, then the error disappears
  public
  end;

implementation

{ TNode }

function TNode.GetChildNodes: TNode;
begin
  Result:=nil;
end;

function TNode.xxx:TDomNode;
begin
  Result:=nil;
end;

function TNode.getOwnerDocument:TDOMDocument;
begin
  Result:=nil;
end;

end.

