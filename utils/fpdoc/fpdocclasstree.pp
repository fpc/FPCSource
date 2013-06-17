unit fpdocclasstree;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DOM, pastree;

Type
  TClassTreeBuilder = Class
  Private
    FClassTree : TXMLDocument;
    FTreeStart : TDomElement;
    FObjectKind : TPasObjKind;
    FPackage: TPasPackage;
    FParentObject : TPasClassType;
  Protected
    function LookForElement(PE: TDomElement; AElement: TPasElement; NoPath : Boolean): TDomNode;
    function NodeMatch(N: TDomNode; AElement: TPasElement; NoPath : Boolean): Boolean;
    Function AddToClassTree(AElement : TPasElement; ACount : Integer) : TDomElement;
  Public
    Constructor Create(APackage : TPasPackage; AObjectKind : TPasObjKind = okClass);
    Destructor Destroy; override;
    Function BuildTree(AObjects : TStringList) : Integer;
    Property ClassTree : TXMLDocument Read FClassTree;
  end;

implementation

constructor TClassTreeBuilder.Create(APackage : TPasPackage;
  AObjectKind: TPasObjKind);
Var
  N : TDomNode;
begin
  FCLassTree:=TXMLDocument.Create;
  FPackage:=APAckage;
  FObjectKind:=AObjectKind;
  Case FObjectkind of
    okObject    : FParentObject:=TPasClassType.Create('System.TObject',FPackage);
    okClass     : FParentObject:=TPasClassType.Create('System.TObject',FPackage);
    okInterface : FParentObject:=TPasClassType.Create('System.IInterface',FPackage);
  end;
  FParentObject.ObjKind:=FObjectKind;
  FTreeStart:=FClassTree.CreateElement('TObject');
  FTreeStart['unit']:='System';
  ClassTree.AppendChild(FTreeStart);
end;

destructor TClassTreeBuilder.Destroy;
begin
  FreeAndNil(FClassTree);
  Inherited;
end;
Function TClassTreeBuilder.BuildTree(AObjects : TStringList) : Integer;

Var
  I : Integer;
  PC : TPasClassType;

begin
  Result:=0;
  AObjects.Sorted:=True;
  For I:=0 to AObjects.Count-1 do
    begin
    PC:=TPasClassType(AObjects.Objects[i]);
    If (PC.ObjKind=FObjectKind) and Not PC.IsForward then
      begin
      AddToClassTree(PC as TPasElement,Result)
      end;
    end;
end;

Function TClassTreeBuilder.NodeMatch(N : TDomNode; AElement : TPasElement; NoPath : Boolean) : Boolean;

Var
  PN,S : String;

begin
  Result:=(N.NodeType=ELEMENT_NODE);
  if Result then
    begin
    S:=N.NodeName;
    if NoPath then
      Begin
      Result:= (CompareText(S,AElement.Name)=0);
      end
    else
      begin
      IF Assigned(Aelement.GetModule) then
        PN:=Aelement.GetModule.PackageName
      else
        PN:=FPackage.Name;
      S:=PN+'.'+TDomElement(N)['unit']+'.'+S;
      Result:= (CompareText(S,AElement.PathName)=0);
      end;
   end;
end;

Function TClassTreeBuilder.LookForElement(PE : TDomElement; AElement : TPasElement; NoPath : boolean) : TDomNode;

Var
  N : TDomNode;

begin
  Result:=PE;
  While (Result<>Nil) and Not NodeMatch(Result,AElement,NoPath) do
    Result:=Result.NextSibling;
  If (Result=Nil) then
    if  Assigned(PE) then
      begin
      N:=PE.FirstChild;
      While (Result=Nil) and (N<>Nil) do
        begin
        if (N.NodeType=ELEMENT_NODE) then
          begin
          Result:=LookForElement(N as TDomElement,AElement,NoPath);
          end;
        N:=N.NextSibling;
        end;
      end;
end;

Function TClassTreeBuilder.AddToClassTree(AElement : TPasElement; ACount : Integer) : TDomElement;
// there are several codepaths that use uninitialized variables. (N,PE)
// I initialized them to nil to at least make failures deterministic.
Var
  PC : TPasClassType;
  PE : TDomElement;
  M : TPasModule;
  N : TDomNode;
  PF : String;

begin
  PF:=StringOfChar(' ',ACount);
  Result:=Nil; N:=Nil;PE:=NIL;
  If (AElement=Nil) then
    begin
    Result:=FTreeStart;
    Exit;
    end
  else If (AElement is TPasUnresolvedTypeRef) then
    begin
    N:=LookForElement(FTreeStart,AElement,True);
    If (N=Nil) then
      begin
      PE:=FTreeStart;
      end
    end
  else If (AElement is TPasClassType) then
    begin
    if (AElement=FParentObject) then
      Result:=FTreeStart
    else
      begin
      PC:=AElement as TPasClassType;
      PE:=AddToClassTree(PC.AncestorType,ACount+1);
      if PE=Nil then
        PE:=FTreeStart;
      N:=LookForElement(PE,PC,False);
      end
    end;
  If (N<>Nil) then
    Result:=N as TDomElement
  else
    begin // N=NIL, PE might be nil.
    Inc(ACount);
    Result:=FClassTree.CreateElement(AElement.Name);
    If Not (AElement is TPasUnresolvedTypeRef) then
      begin
      M:=AElement.GetModule;
      if Assigned(M) then
        Result['unit']:=M.Name;
      end;
      if assigned(PE) then  // if not assigned, probably needs to be
			    // assigned to something else.
        PE.AppendChild(Result);
    end;
end;

end.

