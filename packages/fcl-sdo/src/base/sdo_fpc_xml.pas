{
    This file is part of the Free Pascal Class Library SDO Implementation
    Copyright (c) 2012 by Inoussa OUEDRAOGO
    Free Pascal development team

    This unit implements XML DOM handling utilities

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$INCLUDE sdo_global.inc}
unit sdo_fpc_xml;

interface

uses
  Classes, SysUtils, DOM;

type

  { TDOMNodeSelectListImp }

  TDOMNodeSelectListImp = class(TDOMNodeList)
  private
    FFilter: DOMString;
    FUseFilter: Boolean;
  protected
    procedure BuildList(); override;
  public
    constructor Create(ANode: TDOMNode; const AFilter: DOMString);
  end;

  function FilterList(const ANode : TDOMNode; const ANodeName : DOMString) : TDOMNodeList ;
  function SelectSingleNode(
    const AXPathExpression : DOMString;
    const AContextNode     : TDOMNode;
    const AErrorIfMore     : Boolean
  ) : TDOMNode;


  function GetNodeItemsCount(const ANode : TDOMNode): Integer;
  function GetNodeListCount(ANodeList : TDOMNodeList) : Integer ;overload;{$IFDEF USE_INLINE}inline;{$ENDIF}
  function GetNodeListCount(ANodeList : TDOMNamedNodeMap) : Integer ;overload;{$IFDEF USE_INLINE}inline;{$ENDIF}
  procedure ReleaseDomNode(ADomNode : TDOMNode);overload;{$IFDEF USE_INLINE}inline;{$ENDIF}
  procedure ReleaseDomNode(ADomNode : TDOMNodeList);overload;{$IFDEF USE_INLINE}inline;{$ENDIF}
  procedure ReleaseDomNode(ADomNode : TDOMNamedNodeMap);overload;{$IFDEF USE_INLINE}inline;{$ENDIF}
  function CreateDoc() : TXMLDocument ;{$IFDEF USE_INLINE}inline;{$ENDIF}
  function FindNode(ANode : TDOMNode;const ANodeName : string) : TDOMNode;{$IFDEF USE_INLINE}inline;{$ENDIF}

  function NodeToBuffer(ANode : TDOMNode):string ;
  
resourcestring
  SERR_NoNodeXpathExpression = 'This XPath expression does not correspond to node(s) : %s.';
  SERR_XpathExpectingOneNode = 'Xpath expression expecting a single node while got %d node : %s.';

implementation
uses
  XMLWrite, xpath;

function GetNodeItemsCount(const ANode : TDOMNode): Integer;
var
  n: TDOMNode;
begin
  n := ANode.FirstChild;
  Result := 0;
  while Assigned(n) do
  begin
    Inc(Result);
    n := n.NextSibling;
  end;
end;

function GetNodeListCount(ANodeList : TDOMNodeList) : Integer ;overload;
begin
  Result := ANodeList.Count;
end;

function GetNodeListCount(ANodeList : TDOMNamedNodeMap) : Integer ;overload;
begin
  Result := ANodeList.Length;
end;

procedure ReleaseDomNode(ADomNode : TDOMNode);overload;
begin
  ADomNode.Free();
end;

procedure ReleaseDomNode(ADomNode : TDOMNodeList);overload;
begin
{ $IFNDEF TDOMNodeList_RELEASE_NOT_FREE}
  ADomNode.Free();
{ $ENDIF}
end;

procedure ReleaseDomNode(ADomNode : TDOMNamedNodeMap);overload;
begin
  ADomNode.Free();
end;

function CreateDoc() : TXMLDocument ;
begin
  Result := TXMLDocument.Create();
end;

function FindNode(ANode : TDOMNode;const ANodeName : string) : TDOMNode;{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  Result := ANode.FindNode(ANodeName);
end;

function NodeToBuffer(ANode : TDOMNode):string ;
var
  locStream : TStringStream;
begin
  locStream := TStringStream.Create('');
  try
    WriteXML(ANode,locStream);
    Result := locStream.DataString;
  finally
    locStream.Free();
  end;
end;

function FilterList(const ANode : TDOMNode; const ANodeName : DOMString) : TDOMNodeList ;
begin
  Result := TDOMNodeSelectListImp.Create(ANode,ANodeName);
end;

function SelectSingleNode(
  const AXPathExpression : DOMString;
  const AContextNode     : TDOMNode;
  const AErrorIfMore     : Boolean
) : TDOMNode;
var
  xp_res : TXPathVariable;
  ns : TNodeSet;
begin
  Result := nil;
  xp_res := EvaluateXPathExpression(AXPathExpression,AContextNode);
  if ( xp_res <> nil ) then begin
    if not xp_res.InheritsFrom(TXPathNodeSetVariable) then
      raise Exception.CreateFmt(SERR_NoNodeXpathExpression,[AXPathExpression]);
    ns := xp_res.AsNodeSet;
    if ( ns <> nil ) and ( ns.Count > 0 ) then begin
      if AErrorIfMore and ( ns.Count > 1 ) then
        raise Exception.CreateFmt(SERR_XpathExpectingOneNode,[ns.Count,AXPathExpression]);
      Result := TDOMNode(ns[0]);
    end;
  end;
end;

{ TDOMNodeSelectListImp }

type
  TDOMNodeCracked = class(TDOMNode);
procedure TDOMNodeSelectListImp.BuildList();
var
  Child: TDOMNode;
begin
  FList.Clear;
  FRevision := TDOMNodeCracked(FNode).GetRevision();

  Child := FNode.FirstChild;
  while ( Child <> nil ) do begin
    if ( Child.NodeType = ELEMENT_NODE ) and
       ( ( not FUseFilter ) or ( TDOMElement(Child).TagName = FFilter ) )
    then begin
      FList.Add(Child);
    end;
    Child := Child.NextSibling
  end;
end;

constructor TDOMNodeSelectListImp.Create(ANode: TDOMNode; const AFilter: DOMString);
begin
  inherited Create(ANode);
  FFilter := AFilter;
  FUseFilter := ( FFilter <> '*' );
end;

end.
