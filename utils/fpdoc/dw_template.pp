{

    FPDoc  -  Free Pascal Documentation Tool
    Copyright (C) 2005 by Michael Van Canneyt

    * Template output generator

    See the file COPYING, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{
  Usage: change the constants below. Do a search&replace where TTemplateWriter
  is changed to TMyFormatWriter (replace MyFormat with whatever you need)
  and fill in all methods.

  If your format is linear (i.e. not some hyperlinked format, split in several
  output files), you should take the dw_lintmpl template instead. It will take
  care of all needed structure.

}
{$mode objfpc}
{$H+}
unit dw_template;

interface

uses
  Classes, SysUtils, DGlobals, dWriter, pastree, dom;

Const
  { Change this into the name of your writer}
  TemplateName = 'template';
  { Comprehensible description goes here:}
  STemplateUsageWriterDescr = 'Writes output in template format';
  { Extension for the template }
  TTemplateExtension = '.tpl';

Type
  { TTemplateWriter }

  TTemplateWriter = Class(TFPDocWriter)
    ModuleName,             // Current module name
    PackageName: String;    // Package name
    FStream : TStream;      // Current output stream.
    Module: TPasModule;     // Current module.
  Protected
    // Writing support.
    { various routines to Write to stream. }
    procedure Write(const s: String);
    procedure WriteF(const s: String; const Args: array of const);
    procedure WriteLn(const s: String);
    procedure WriteLnF(const s: String; const Args: array of const);
    { Replace current write stream with S, return current stream. }
    Function  PushWriteContext(S : TStream) : TStream;
    { Replace current write stream with S }
    Procedure PopWriteContext(S : TSTream);
    { make sure text is interpretable: escape all special characters in some way.}
    Function  EscapeText(const s : String) : String;
    { Sort a list of TPasElements }
    procedure SortElementList(List : TList);
    { Writes a description node to a stream, returns the result as a string }
    Function  GetDescrString(AContext: TPasElement; DescrNode: TDOMElement) : String;
    { Determine value for constant }
    function  ConstValue(ConstDecl: TPasConst): String; virtual;
    { Write the contents of an example file }
    Procedure WriteExampleFile(FN : String); virtual;
    { Write all example files, found in ADocNode. }
    procedure WriteExample(ADocNode: TDocNode);
    { Convert a TPasElement to a valid label for this backend }
    function ElementToLabel(AElement : TPasElement) : String;
  Public
    Constructor Create(APackage: TPasPackage; AEngine: TFPDocEngine); override;
    procedure WriteDoc; override;
    // Documentation writing methods.
    // Package
    Procedure WritePackagePage;
    // Topic refs
    Procedure ProcessTopics(DocNode : TDocNode; Subs : Boolean);
    // Single topic page.
    Procedure WriteTopicPage(Parent,Node : TDocNode);
    // Process a Module (=unit)
    procedure ProcessModule(AModule: TPasModule);
    // Write Unit description.
    procedure WriteUnitPage(AModule: TPasModule);
    // Do all resource strings in a unit.
    procedure WriteUnitResourceStrings(ASection: TPasSection);
    // Do all constants in a unit.
    procedure WriteUnitConsts(ASection: TPasSection);
    // Do all types in a unit.
    procedure WriteUnitTypes(ASection: TPasSection);
    // Do all variables in a unit.
    procedure WriteUnitVars(ASection: TPasSection);
    // Do all classes in a unit.
    procedure WriteUnitClasses(ASection: TPasSection);
    // Do all functions and procedures in a unit.
    procedure WriteUnitFunctionsAndProcedures(ASection: TPasSection);
    // Write elements of an enumeration type
    procedure WriteEnumElements(TypeDecl : TPasEnumType);
    // Write class documentation
    procedure WriteClassPage(ClassDecl: TPasClassType);
    // Write procedure/function documentation
    procedure WriteProcedurePage(ProcDecl: TPasProcedureBase);
    // Write procedure/function arguments documentation
    procedure AppendProcedureArgsSection(Element: TPasProcedureType);
    // Write function result documentation
    procedure AppendFunctionResultSection(Element: TPasFunctionType);
    // Write class property  documentation
    procedure WritePropertyPage(PropDecl: TPasProperty);
    // To be Overriden from fpdocwriter;
    procedure DescrWriteText(const AText: DOMString); override;
    procedure DescrBeginBold; override;
    procedure DescrEndBold; override;
    procedure DescrBeginItalic; override;
    procedure DescrEndItalic; override;
    procedure DescrBeginEmph; override;
    procedure DescrEndEmph; override;
    procedure DescrWriteFileEl(const AText: DOMString); override;
    procedure DescrWriteKeywordEl(const AText: DOMString); override;
    procedure DescrWriteVarEl(const AText: DOMString); override;
    procedure DescrBeginLink(const AId: DOMString); override;
    procedure DescrEndLink; override;
    procedure DescrWriteLinebreak; override;
    procedure DescrBeginParagraph; override;
    procedure DescrBeginCode(HasBorder: Boolean; const AHighlighterName: String); override;
    procedure DescrWriteCodeLine(const ALine: String); override;
    procedure DescrEndCode; override;
    procedure DescrEndParagraph; override;
    procedure DescrBeginOrderedList; override;
    procedure DescrEndOrderedList; override;
    procedure DescrBeginUnorderedList; override;
    procedure DescrEndUnorderedList; override;
    procedure DescrBeginDefinitionList; override;
    procedure DescrEndDefinitionList; override;
    procedure DescrBeginListItem; override;
    procedure DescrEndListItem; override;
    procedure DescrBeginDefinitionTerm; override;
    procedure DescrEndDefinitionTerm; override;
    procedure DescrBeginDefinitionEntry; override;
    procedure DescrEndDefinitionEntry; override;
    procedure DescrBeginSectionTitle; override;
    procedure DescrBeginSectionBody; override;
    procedure DescrEndSection; override;
    procedure DescrBeginRemark; override;
    procedure DescrEndRemark; override;
    procedure DescrBeginTable(ColCount: Integer; HasBorder: Boolean); override;
    procedure DescrEndTable; override;
    procedure DescrBeginTableCaption; override;
    procedure DescrEndTableCaption; override;
    procedure DescrBeginTableHeadRow; override;
    procedure DescrEndTableHeadRow; override;
    procedure DescrBeginTableRow; override;
    procedure DescrEndTableRow; override;
    procedure DescrBeginTableCell; override;
    procedure DescrEndTableCell; override;
    // Handle options.
    Function InterPretOption(Const Cmd,Arg : String) : boolean; override;
    // Provide feedback about usage of this backend.
    Class procedure Usage(List: TStrings); override;
    // For info only. See linear writer for an example.
    Class Function FileNameExtension : String;virtual;
  end;

implementation

{ TTemplateWriter }

constructor TTemplateWriter.Create(APackage: TPasPackage; AEngine: TFPDocEngine);

  procedure AddLabel(AElement: TPasElement);
  begin
    Engine.AddLink(AElement.PathName, ElementToLabel(AElement));
  end;

  procedure AddList(AElement: TPasElement; AList: TList);
  var
    i: Integer;
  begin
    for i := 0 to AList.Count - 1 do
      AddLabel(TPasElement(AList[i]));
  end;

  procedure AddTopicPages(AElement: TPasElement);

  var
    PreviousTopic,
    TopicElement : TTopicElement;
    DocNode,
    TopicNode : TDocNode;

  begin
    DocNode:=Engine.FindDocNode(AElement);
    If not Assigned(DocNode) then
      exit;
    TopicNode:=DocNode.FirstChild;
    PreviousTopic:=Nil;
    While Assigned(TopicNode) do
      begin
      If TopicNode.TopicNode then
        begin
        TopicElement:=TTopicElement.Create(TopicNode.Name,AElement);
        Topics.Add(TopicElement);
        TopicElement.TopicNode:=TopicNode;
        TopicElement.Previous:=PreviousTopic;
        If Assigned(PreviousTopic) then
          PreviousTopic.Next:=TopicElement;
        PreviousTopic:=TopicElement;
        if AElement is TTopicElement then
          TTopicElement(AElement).SubTopics.Add(TopicElement);
        Engine.AddLink(TopicElement.PathName, ElementToLabel(TopicElement));
        if AElement is TTopicElement then
          TTopicElement(AElement).SubTopics.Add(TopicElement)
        else // Only one level of recursion.
          AddTopicPages(TopicElement);
        end;
      TopicNode:=TopicNode.NextSibling;
      end;
  end;

  procedure ScanModule(AModule: TPasModule);
  var
    i, j, k: Integer;
    ClassEl: TPasClassType;
    FPEl, AncestorMemberEl: TPasElement;
    DocNode: TDocNode;
    DidAutolink: Boolean;
  begin
    AddLabel(AModule);
    AddTopicPages(AModule);
    with AModule do
    begin
      AddList(AModule, InterfaceSection.ResStrings);
      AddList(AModule, InterfaceSection.Consts);
      AddList(AModule, InterfaceSection.Types);
      if InterfaceSection.Classes.Count > 0 then
      begin
        for i := 0 to InterfaceSection.Classes.Count - 1 do
        begin
          ClassEl := TPasClassType(InterfaceSection.Classes[i]);
          AddLabel(ClassEl);

          for j := 0 to ClassEl.Members.Count - 1 do
          begin
            FPEl := TPasElement(ClassEl.Members[j]);
            if ((FPEl.Visibility = visPrivate) and Engine.HidePrivate) or
              ((FPEl.Visibility = visProtected) and Engine.HideProtected) then
              continue;

            DocNode := Engine.FindDocNode(FPEl);
            if not Assigned(DocNode) then
            begin
              DidAutolink := False;
              if Assigned(ClassEl.AncestorType) and
                (ClassEl.AncestorType.ClassType = TPasClassType) then
              begin
                for k := 0 to TPasClassType(ClassEl.AncestorType).Members.Count - 1 do
                begin
                  AncestorMemberEl :=
                    TPasElement(TPasClassType(ClassEl.AncestorType).Members[k]);
                  if AncestorMemberEl.Name = FPEl.Name then
                  begin
                    DocNode := Engine.FindDocNode(AncestorMemberEl);
                    if Assigned(DocNode) then
                    begin
                      DidAutolink := True;
                      Engine.AddLink(FPEl.PathName,
                        Engine.FindAbsoluteLink(AncestorMemberEl.PathName));
                      break;
                    end;
                  end;
                end;
              end;
              if not DidAutolink then
                AddLabel(FPEl);
            end else
              AddLabel(FPEl);
          end;
        end;
      end;
      AddList(AModule, InterfaceSection.Functions);
      AddList(AModule, InterfaceSection.Variables);
    end;
  end;

var
  i: Integer;
begin
  inherited;
  if Length(Package.Name) > 1 then
    AddTopicPages(Package);
  for i := 0 to Package.Modules.Count - 1 do
    ScanModule(TPasModule(Package.Modules[i]));
end;

{ ---------------------------------------------------------------------
  Writing support
  ---------------------------------------------------------------------}

Function TTemplateWriter.PushWriteContext(S : TStream) : TStream;

begin
  Result:=FStream;
  FStream:=S;
end;

Procedure TTemplateWriter.PopWriteContext(S : TSTream);

begin
  FStream:=S;
end;

function TTemplateWriter.EscapeText(const s: String): String;
begin
  Result:=S;
end;

procedure TTemplateWriter.Write(const s: String);

Var
  L : Integer;

begin
  L:=Length(S);
  If (L>0) then
    FStream.Write(PChar(S)^,L);
end;


procedure TTemplateWriter.WriteF(const s: String; const Args: array of const);
begin
  Write(Format(S,Args));
end;

procedure TTemplateWriter.WriteLn(const s: String);

begin
  Write(S);
  Write(LineEnding);
end;

procedure TTemplateWriter.WriteLnF(const s: String; const Args: array of const);
begin
  Write(Format(S,Args));
  Write(LineEnding);
end;

procedure TTemplateWriter.DescrWriteText(const AText: DOMString);

begin
  Self.Write(EscapeText(AText));
end;

procedure TTemplateWriter.DescrBeginBold;
begin
end;

procedure TTemplateWriter.DescrEndBold;
begin
end;

procedure TTemplateWriter.DescrBeginItalic;
begin
end;

procedure TTemplateWriter.DescrEndItalic;
begin
end;

procedure TTemplateWriter.DescrBeginEmph;
begin
end;

procedure TTemplateWriter.DescrEndEmph;
begin
end;

procedure TTemplateWriter.DescrWriteFileEl(const AText: DOMString);

begin
end;

procedure TTemplateWriter.DescrWriteKeywordEl(const AText: DOMString);

begin
end;

procedure TTemplateWriter.DescrWriteVarEl(const AText: DOMString);

begin
end;

procedure TTemplateWriter.DescrBeginLink(const AId: DOMString);
begin
end;

procedure TTemplateWriter.DescrEndLink;
begin
end;

procedure TTemplateWriter.DescrWriteLinebreak;
begin
end;

procedure TTemplateWriter.DescrBeginParagraph;
begin
end;

procedure TTemplateWriter.DescrBeginCode(HasBorder: Boolean;
  const AHighlighterName: String);
begin
end;

procedure TTemplateWriter.DescrWriteCodeLine(const ALine: String);
begin
  Writeln(ALine);
  DescrWriteLinebreak;
end;

procedure TTemplateWriter.DescrEndCode;
begin
end;

procedure TTemplateWriter.DescrEndParagraph;
begin
end;

procedure TTemplateWriter.DescrBeginOrderedList;
begin
end;

procedure TTemplateWriter.DescrEndOrderedList;
begin
end;

procedure TTemplateWriter.DescrBeginUnorderedList;
begin
end;

procedure TTemplateWriter.DescrEndUnorderedList;
begin
end;

procedure TTemplateWriter.DescrBeginDefinitionList;
begin
end;

procedure TTemplateWriter.DescrEndDefinitionList;
begin
end;

procedure TTemplateWriter.DescrBeginListItem;
begin
end;

procedure TTemplateWriter.DescrEndListItem;
begin
end;

procedure TTemplateWriter.DescrBeginDefinitionTerm;
begin
end;

procedure TTemplateWriter.DescrEndDefinitionTerm;
begin
end;

procedure TTemplateWriter.DescrBeginDefinitionEntry;
begin
end;

procedure TTemplateWriter.DescrEndDefinitionEntry;
begin
end;

procedure TTemplateWriter.DescrBeginSectionTitle;
begin
end;

procedure TTemplateWriter.DescrBeginSectionBody;
begin
end;

procedure TTemplateWriter.DescrEndSection;
begin
end;

procedure TTemplateWriter.DescrBeginRemark;
begin
end;

procedure TTemplateWriter.DescrEndRemark;
begin
end;

procedure TTemplateWriter.DescrBeginTable(ColCount: Integer; HasBorder: Boolean);
begin
end;

procedure TTemplateWriter.DescrEndTable;
begin
end;

procedure TTemplateWriter.DescrBeginTableCaption;
begin
end;

procedure TTemplateWriter.DescrEndTableCaption;
begin
end;

procedure TTemplateWriter.DescrBeginTableHeadRow;
begin
end;

procedure TTemplateWriter.DescrEndTableHeadRow;
begin
end;

procedure TTemplateWriter.DescrBeginTableRow;
begin
end;

procedure TTemplateWriter.DescrEndTableRow;
begin
end;

procedure TTemplateWriter.DescrBeginTableCell;
begin
end;

procedure TTemplateWriter.DescrEndTableCell;
begin
end;

function TTemplateWriter.InterPretOption(const Cmd, Arg: String): boolean;
begin
  Result:=False;
end;


Function TTemplateWriter.GetDescrString(AContext: TPasElement; DescrNode: TDOMElement) : String;

Var
  S : TStringStream;
  F : TStream;

begin
  Result:='';
  if Assigned(DescrNode) then
    begin
    S:=TStringStream.Create('');
    Try
      F:=PushWriteContext(S);
      Try
        ConvertDescr(AContext, DescrNode, False);
        Result:=S.DataString;
      FInally
        PopWriteContext(F);
      end;
    finally
      S.FRee;
    end;
    end;
end;

function TTemplateWriter.ConstValue(ConstDecl: TPasConst): String;
begin
  if Assigned(ConstDecl) then
    Result := ConstDecl.ClassName
  else
    Result := '<nil>';
end;


procedure TTemplateWriter.WriteExample(ADocNode: TDocNode);

var
  Example: TDOMElement;
  S : string;

begin
  // Template, change as needed.
  S:='';
  if Assigned(ADocNode) then
    begin
    Example := ADocNode.FirstExample;
    If Assigned(Example) then
      begin
      while Assigned(Example) do
        begin
        s:=Engine.GetExampleFileName(Example);
        if (s<>'') then
          WriteExampleFile(S);
        DescrEndParaGraph;
        Repeat
          Example := TDomElement(Example.NextSibling);
        until (Example=Nil) or ((Example.NodeType=ELEMENT_NODE) and (Example.NodeName='example'));
        end;
      end;
    end;
end;

procedure TTemplateWriter.WriteExampleFile(FN : String);

Var
  L : TStringList;
  I : Integer;

begin
  // Template, change as needed.
  If (FN<>'') and FileExists(FN) then
    begin
    L:=TStringList.Create;
    Try
      L.LoadFromFile(FN);
      For I:=0 to L.Count-1 do
        DescrWriteCodeLine(L[i]);
    finally
      L.Free;
    end;
    end;
end;

{ ---------------------------------------------------------------------
  Actual man page writing
  ---------------------------------------------------------------------}

procedure TTemplateWriter.WriteDoc;

var
  i : Integer;
  L : TstringList;
  DocNode : TDocNode;

begin
  PackageName := LowerCase(Copy(Package.Name, 2, 255));
  WritePackagePage;
  L:=TStringList.Create;
  Try
    // Sort modules.
    For I:=0 to Package.Modules.Count-1 do
      L.AddObject(TPasModule(Package.Modules[i]).Name,TPasModule(Package.Modules[i]));
    L.Sorted:=True;
    for i:=0 to L.Count - 1 do
      ProcessModule(TPasModule(L.Objects[i]));
  Finally
    L.Free;
  end;
end;


{ ---------------------------------------------------------------------
  Package man page
  ---------------------------------------------------------------------}


Procedure TTemplateWriter.WritePackagePage;

var
  DocNode: TDocNode;
  L : TStringList;

begin
  DocNode:=Engine.FindDocNode(Package);
  // Write topics
  ProcessTopics(DocNode,True);
end;

{ ---------------------------------------------------------------------
  Topic support
  ---------------------------------------------------------------------}


Procedure TTemplateWriter.ProcessTopics(DocNode : TDocNode; Subs : Boolean);

Var
  Node,SubNode : TDocNode;

begin
  If Not Assigned(DocNode) then
    Exit;
  Node:=DocNode.FirstChild;
  While Assigned(Node) do
    begin
    If Node.TopicNode then
      begin
      WriteTopicPage(DocNode,Node);
      If Subs then
        begin
        SubNode:=DocNode.FirstChild;
        While Assigned(SubNode) do
        If SubNode.TopicNode then
           WriteTopicPage(Node,SubNode);
        end;
      end;
    Node:=Node.NextSibling;
    end;
end;


Procedure TTemplateWriter.WriteTopicPage(Parent,Node : TDocNode);

Var
  Element : TTopicElement;

begin
  Element:=FindTopicElement(Node);
  If Not Assigned(Element) then
    Exit;
  // Write topic here
end;


{ ---------------------------------------------------------------------
  Module man pages
  ---------------------------------------------------------------------}

procedure TTemplateWriter.ProcessModule(AModule : TPasModule);

begin
  With AModule do
    begin
    Module:=AModule;
    ModuleName:=Name;
    With InterfaceSection do
      begin
      SortElementList(Declarations);
      SortElementList(Types);
      SortElementList(Consts);
      SortElementList(Classes);
      SortElementList(Functions);
      SortElementList(Variables);
      SortElementList(ResStrings);
      end;
    WriteUnitPage(AModule);
    WriteUnitResourceStrings(InterfaceSection);
    WriteUnitConsts(InterfaceSection);
    WriteUnitTypes(InterfaceSection);
    WriteUnitVars(InterfaceSection);
    WriteUnitFunctionsAndProcedures(InterfaceSection);
    WriteUnitClasses(InterfaceSection);
    end;
end;

procedure TTemplateWriter.WriteUnitPage(AModule : TPasModule);

Var
  DocNode : TDocNode;

begin
  DocNode:=Engine.FindDocNode(AModule);
  // Write unit stuff here.
end;


{ ---------------------------------------------------------------------
  Classes man pages
  ---------------------------------------------------------------------}

procedure TTemplateWriter.WriteUnitClasses(ASection: TPasSection);

var
  i: Integer;

begin
  if (ASection.Classes.Count > 0) then
    begin
    for i := 0 to ASection.Classes.Count - 1 do
      WriteClassPage(TPasClassType(ASection.Classes[i]));
    end;
end;

procedure TTemplateWriter.WriteClassPage(ClassDecl: TPasClassType);

var
  DocNode: TDocNode;

begin
  DocNode:=Engine.FindDocNode(ClassDecl);
  // Write class here.
end;


{ ---------------------------------------------------------------------
  Resource strings man page
  ---------------------------------------------------------------------}

procedure TTemplateWriter.WriteUnitResourceStrings(ASection: TPasSection);

var
  I : Integer;
  ResStrDecl : TPasResString;
  DocNode: TDocNode;

begin
  for i := 0 to ASection.ResStrings.Count - 1 do
    begin
    ResStrDecl := TPasResString(ASection.ResStrings[i]);
    DocNode:=Engine.FindDocNode(ResStrDecl);
    { Write docu for resource string.}
    end;
end;

{ ---------------------------------------------------------------------
  Constants man page
  ---------------------------------------------------------------------}

procedure TTemplateWriter.WriteUnitConsts(ASection: TPasSection);

var
  i: Integer;
  ConstDecl: TPasConst;
  DocNode: TDocNode;

begin
  for i := 0 to ASection.Consts.Count - 1 do
    begin
    ConstDecl := TPasConst(ASection.Consts[i]);
    DocNode:=Engine.FindDocNode(ConstDecl);
    { Write docu for constant }
    end;
end;

{ ---------------------------------------------------------------------
  Types man page
  ---------------------------------------------------------------------}

procedure TTemplateWriter.WriteUnitTypes(ASection: TPasSection);

var
  i: Integer;
  TypeDecl: TPasType;
  DocNode : TDocNode;

begin
  for i := 0 to ASection.Types.Count - 1 do
    begin
    TypeDecl := TPasType(ASection.Types[i]);
    DocNode:=Engine.FindDocNode(TypeDecl);
    { Write docu for type }
    end;
end;

procedure TTemplateWriter.WriteEnumElements(TypeDecl : TPasEnumType);

Var
  EV : TPasEnumValue;
  I : Integer;
  DocNode : TDocNode;

begin
  With TypeDecl do
    begin
    SortElementList(Values);
    For I:=0 to Values.Count-1 do
      begin
      EV:=TPasEnumValue(Values[i]);
      DocNode := Engine.FindDocNode(EV);
      { write docu per enumeration constant as needed }
      end;
    end;
end;

{ ---------------------------------------------------------------------
  Variables man page
  ---------------------------------------------------------------------}

procedure TTemplateWriter.WriteUnitVars(ASection: TPasSection);

var
  VarDecl: TPasVariable;
  i: Integer;
  DocNode : TDocNode;

begin
  for i := 0 to ASection.Variables.Count - 1 do
    begin
    VarDecl := TPasVariable(ASection.Variables[i]);
    DocNode:=Engine.FindDocNode(VarDecl);
    { Write docu for variable }
    end;
end;


{ ---------------------------------------------------------------------
  Procedure/Function/Method man page
  ---------------------------------------------------------------------}

procedure TTemplateWriter.WriteUnitFunctionsAndProcedures(ASection: TPasSection);

var
  i       : Integer;

begin
  // Pages for all identifiers.
  for i := 0 to ASection.Functions.Count - 1 do
    WriteProcedurePage(TPasProcedure(ASection.Functions[i]));
end;

procedure TTemplateWriter.WriteProcedurePage(ProcDecl : TPasProcedureBase);

var
  DocNode: TDocNode;
  OP : TPasOverloadedProc;
  i : integer;
  D,N : String;
begin
  DocNode := Engine.FindDocNode(ProcDecl);
  // Write arguments, if any.
  AppendProcedureArgsSection(TPasProcedure(ProcDecl).ProcType);
  If ProcDecl is TPasFunction then
    AppendProcedureArgsSection(TPasFunction(ProcDecl).ProcType);
  // Examples, is present
  WriteExample(DocNode);
end;

procedure TTemplateWriter.AppendProcedureArgsSection(Element: TPasProcedureType);

var
  DocNode: TDocNode;
  i: Integer;
  Arg: TPasArgument;

begin
  If Not Assigned(Element) then
    exit;
  for i := 0 to Element.Args.Count - 1 do
    begin
    Arg := TPasArgument(Element.Args[i]);
    DocNode:=Engine.FindDocNode(Arg);
    // Write docu for argument.
    end;
end;

procedure TTemplateWriter.AppendFunctionResultSection(Element: TPasFunctionType);

Var
  ResultEl: TPasResultElement;
  DocNode: TDocNode;

begin
  If Not Assigned(Element) then
    exit;
  ResultEl := TPasFunctionType(Element).ResultEl;
  DocNode := Engine.FindDocNode(ResultEl);
  // Write docu for result.
end;



{ ---------------------------------------------------------------------
  Property man page
  ---------------------------------------------------------------------}

procedure TTemplateWriter.WritePropertyPage(PropDecl : TPasProperty);

var
  DocNode: TDocNode;
  N,D: String;

begin
  DocNode := Engine.FindDocNode(PropDecl);
  // Write docu for property.
end;


Function CompareElements(P1,P2 : Pointer) : Integer;

begin
  Result:=CompareText(TPasElement(P1).Name,TPasElement(P2).Name);
end;

procedure TTemplateWriter.SortElementList(List : TList);

begin
  List.Sort(@CompareElements);
end;

function TTemplateWriter.FileNameExtension: String;
begin
  Result:=TTemplateExtension;
end;

Class procedure TTemplateWriter.Usage(List: TStrings);
begin
end;

function TTemplateWriter.ElementToLabel(AElement : TPasElement) : String;

begin
  // Convert AElement to a valid label for cross-referencing.
end;


initialization
  // Do not localize.
  RegisterWriter(TTemplateWriter,TemplateName,STemplateUsageWriterDescr);
finalization
  UnRegisterWriter(TemplateName);
end.
