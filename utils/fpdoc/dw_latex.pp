{
    $Id$

    FPDoc  -  Free Pascal Documentation Tool
    Copyright (C) 2000 - 2003 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    * LaTeX output generator

    See the file COPYING, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit dw_LaTeX;

{$MODE objfpc}
{$H+}

interface

uses DOM, dGlobals, PasTree;

const
  LateXHighLight: Boolean = False;

procedure CreateLaTeXDocForPackage(APackage: TPasPackage; AEngine: TFPDocEngine);


implementation

uses SysUtils, Classes, dWriter;

type
  TLabelType = (ltConst,ltVar,ltType,ltFunction,ltProcedure,ltClass,
                ltChapter,ltSection,ltSubsection,
                ltTable,ltFigure);
  
  TLaTeXWriter = class(TFPDocWriter)
  protected
    f: Text;
    FLink: String;
    Package: TPasPackage;
    PackageName: String;
    Module: TPasModule;
    ModuleName: String;
    FTableCount : Integer;
    TableRowStartFlag, TableCaptionWritten: Boolean;

    function GetLabel(AElement: TPasElement): String;

    procedure Write(const s: String);
    procedure WriteF(const s: String; const Args: array of const);
    procedure WriteLn(const s: String);
    procedure WriteLnF(const s: String; const Args: array of const);
    // Tex functions
    procedure WriteLabel(El: TPasElement);
    procedure WriteLabel(const s: String);
    procedure WriteIndex(El: TPasElement);
    procedure WriteIndex(const s: String);
    procedure StartListing(Frames: Boolean; const name: String);
    procedure StartListing(Frames: Boolean);
    procedure EndListing;
    Function  EscapeTex(S : String) : String;
    Function  StripTex(S : String) : String;
    
    procedure WriteCommentLine;
    procedure WriteComment(Comment : String);
    procedure StartSection(SectionName : String; SectionLabel : String);
    procedure StartSection(SectionName : String);
    procedure StartSubSection(SubSectionName : String; SubSectionLabel : String);
    procedure StartSubSection(SubSectionName : String);
    procedure StartChapter(ChapterName : String; ChapterLabel : String);
    procedure StartChapter(ChapterName : String);
    // Description node conversion
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
    procedure WriteDescr(Element: TPasElement);
    procedure WriteDescr(AContext: TPasElement; DescrNode: TDOMElement);
    function ConstValue(ConstDecl: TPasConst): String;
    procedure ProcessSection(ASection: TPasSection);
    // Documentation writing methods.
    procedure WriteResourceStrings(ASection: TPasSection);
    procedure WriteUnitOverview(ASection: TPasSection);
    procedure WriteVarsConstsTypes(ASection: TPasSection);
    procedure WriteConsts(ASection: TPasSection);
    procedure WriteTypes(ASection: TPasSection);
    procedure WriteEnumElements(TypeDecl : TPasEnumType);
    procedure WriteVars(ASection: TPasSection);
    procedure WriteFunctionsAndProcedures(ASection: TPasSection);
    procedure WriteProcedure(ProcDecl: TPasProcedureBase);
    procedure WriteClasses(ASection: TPasSection);
    procedure WriteClassDecl(ClassDecl: TPasClassType);
    procedure WriteClassMethodOverview(ClassDecl: TPasClassType);
    procedure WriteClassPropertyOverview(ClassDecl: TPasClassType);
    procedure WriteProperty(PropDecl: TPasProperty);
    procedure WriteExample(ADocNode: TDocNode);
    procedure WriteSeeAlso(ADocNode: TDocNode);
    procedure SortElementList(List : TList);
    Function  ShowMember(M : TPasElement) : boolean;
  public
    constructor Create(APackage: TPasPackage; AEngine: TFPDocEngine);
    procedure WriteDoc;
  end;



constructor TLaTeXWriter.Create(APackage: TPasPackage; AEngine: TFPDocEngine);

  procedure AddLabel(AElement: TPasElement);
  begin
    Engine.AddLink(AElement.PathName, GetLabel(AElement));
  end;

  procedure AddList(AElement: TPasElement; AList: TList);
  var
    i: Integer;
  begin
    for i := 0 to AList.Count - 1 do
      AddLabel(TPasElement(AList[i]));
  end;

  procedure ScanModule(AModule: TPasModule);
  var
    i, j, k: Integer;
    s: String;
    ClassEl: TPasClassType;
    FPEl, AncestorMemberEl: TPasElement;
    DocNode: TDocNode;
    DidAutolink: Boolean;
  begin
    AddLabel(AModule);
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
  inherited Create(AEngine);
  Package := APackage;

  { Allocate labels for all elements for which we are going to create
    documentation. This is needed for links to work correctly. }

  // Allocate label for the package itself, if a name is given (i.e. <> '#')
  if Length(Package.Name) > 1 then
    AddLabel(Package);

  for i := 0 to Package.Modules.Count - 1 do
    ScanModule(TPasModule(Package.Modules[i]));
end;

procedure TLaTeXWriter.WriteDoc;
var
  i: Integer;
begin
  PackageName := LowerCase(Copy(Package.Name, 2, 255));
  Assign(f,  Engine.Output + PathDelim + PackageName + '.tex');
  Rewrite(f);
  try
    WriteLn('% This file has been created automatically by FPDoc,');
    WriteLn('% (c) 2000-2003 by Areca Systems GmbH / Sebastian Guenther (sg@freepascal.org)');
    for i := 0 to Package.Modules.Count - 1 do
    begin
      Module := TPasModule(Package.Modules[i]);
      ModuleName := LowerCase(Module.Name);
      WriteLn('');
      WriteLnF('\chapter{%s}', [Format(SDocUnitTitle, [Module.Name])]);
      WriteLabel(Module);
      ProcessSection(Module.InterfaceSection);
    end;
  finally
    Close(f);
  end;
end;

function TLaTeXWriter.GetLabel(AElement: TPasElement): String;
var
  i: Integer;
begin
  if AElement.ClassType = TPasUnresolvedTypeRef then
    Result := Engine.ResolveLink(Module, AElement.Name)
  else
  begin
    Result := AElement.PathName;
    Result := LowerCase(Copy(Result, 2, Length(Result) - 1));
  end;
  for i := 1 to Length(Result) do
    if Result[i] = '.' then
      Result[i] := ':';
end;

procedure TLaTeXWriter.Write(const s: String);
begin
  System.Write(f, s);
end;

procedure TLaTeXWriter.WriteF(const s: String; const Args: array of const);
begin
  System.Write(f, Format(s, Args));
end;

procedure TLaTeXWriter.WriteLn(const s: String);
begin
  System.WriteLn(f, s);
end;

procedure TLaTeXWriter.WriteLnF(const s: String; const Args: array of const);
begin
  System.WriteLn(f, Format(s, Args));
end;

Function TLatexWriter.EscapeTex(S : String) : String;

var
  i: Integer;

begin
  SetLength(Result, 0);
  for i := 1 to Length(S) do
    case S[i] of
      '&','{','}','#','_','$','%':		// Escape these characters
        Result := Result + '\' + S[i];
      '~','^':
        Result := Result + '\'+S[i]+' ';
      '\': 
        Result:=Result+'$\backslash$'  
      else
        Result := Result + S[i];
    end;
end;

Function TLatexWriter.StripTex(S : String) : String;

var
  I,L: Integer;

begin
  SetLength(Result, 0);
  for i := 1 to Length(S) do
    If not (S[i] in ['&','{','}','#','_','$','%','''','~','^', '\']) then
      Result := Result + S[i];
end;

procedure TLaTeXWriter.DescrWriteText(const AText: DOMString);

begin
  Write(EscapeTex(AText));
end;

procedure TLaTeXWriter.DescrBeginBold;
begin
  Write('\textbf{');
end;

procedure TLaTeXWriter.DescrEndBold;
begin
  Write('}');
end;

procedure TLaTeXWriter.DescrBeginItalic;
begin
  Write('\textit{');
end;

procedure TLaTeXWriter.DescrEndItalic;
begin
  Write('}');
end;

procedure TLaTeXWriter.DescrBeginEmph;
begin
  Write('\emph{');
end;

procedure TLaTeXWriter.DescrEndEmph;
begin
  Write('}');
end;

procedure TLaTeXWriter.DescrWriteFileEl(const AText: DOMString);
begin
  Write('\file{');
  DescrWriteText(AText);
  Write('}');
end;

procedure TLaTeXWriter.DescrWriteKeywordEl(const AText: DOMString);
begin
  Write('\textbf{\\ttfamily ');
  DescrWriteText(AText);
  Write('}');
end;

procedure TLaTeXWriter.DescrWriteVarEl(const AText: DOMString);
begin
  Write('\var{');
  DescrWriteText(AText);
  Write('}');
end;

procedure TLaTeXWriter.DescrBeginLink(const AId: DOMString);
var
  i: Integer;
begin
  FLink := Engine.ResolveLink(Module, AId);
//  System.WriteLn('Link "', AId, '" => ', FLink);
end;

procedure TLaTeXWriter.DescrEndLink;
begin
  WriteF(' (\pageref{%s})',[StripTex(Flink)]);
end;

procedure TLaTeXWriter.DescrWriteLinebreak;
begin
  WriteLn('\\');
end;

procedure TLaTeXWriter.DescrBeginParagraph;
begin
  // Do nothing
end;

procedure TLaTeXWriter.DescrEndParagraph;
begin
  WriteLn('');
  WriteLn('');
end;

procedure TLaTeXWriter.DescrBeginCode(HasBorder: Boolean;
  const AHighlighterName: String);
begin
  StartListing(HasBorder);
end;

procedure TLaTeXWriter.DescrWriteCodeLine(const ALine: String);
begin
  WriteLn(ALine);
end;

procedure TLaTeXWriter.DescrEndCode;
begin
  EndListing
end;

procedure TLaTeXWriter.DescrBeginOrderedList;
begin
  WriteLn('\begin{enumerate}');
end;

procedure TLaTeXWriter.DescrEndOrderedList;
begin
  WriteLn('\end{enumerate}');
end;

procedure TLaTeXWriter.DescrBeginUnorderedList;
begin
  WriteLn('\begin{itemize}');
end;

procedure TLaTeXWriter.DescrEndUnorderedList;
begin
  WriteLn('\end{itemize}');
end;

procedure TLaTeXWriter.DescrBeginDefinitionList;
begin
  WriteLn('\begin{description');
end;

procedure TLaTeXWriter.DescrEndDefinitionList;
begin
  WriteLn('\end{description}');
end;

procedure TLaTeXWriter.DescrBeginListItem;
begin
  Write('\item ');
end;

procedure TLaTeXWriter.DescrEndListItem;
begin
  WriteLn('');
end;

procedure TLaTeXWriter.DescrBeginDefinitionTerm;
begin
  Write('\item[');
end;

procedure TLaTeXWriter.DescrEndDefinitionTerm;
begin
  WriteLn(']');
end;

procedure TLaTeXWriter.DescrBeginDefinitionEntry;
begin
  // Do nothing
end;

procedure TLaTeXWriter.DescrEndDefinitionEntry;
begin
  WriteLn('');
end;

procedure TLaTeXWriter.DescrBeginSectionTitle;
begin
  Write('\subsection{');
end;

procedure TLaTeXWriter.DescrBeginSectionBody;
begin
  WriteLn('}');
end;

procedure TLaTeXWriter.DescrEndSection;
begin
  // Do noting
end;

procedure TLaTeXWriter.DescrBeginRemark;
begin
  WriteLn('\begin{remark}');
end;

procedure TLaTeXWriter.DescrEndRemark;
begin
  WriteLn('\end{remark}');
end;

procedure TLaTeXWriter.DescrBeginTable(ColCount: Integer; HasBorder: Boolean);
var
  i: Integer;
begin
  // !!!: How do we set the border?
  Write('\begin{FPCltable}{');
  for i := 1 to ColCount do
    Write('l');  
  write('}{');
  TableCaptionWritten:=False;
end;

procedure TLaTeXWriter.DescrEndTable;
begin
  WriteLn('\end{FPCltable}');
end;

procedure TLaTeXWriter.DescrBeginTableCaption;
begin
  // Do nothing.
end;

procedure TLaTeXWriter.DescrEndTableCaption;
begin
  Write('}{table');
  Inc(FTableCount);
  Write(IntToStr(FTableCount));
  Writeln('}');
  TableCaptionWritten := True;
end;

procedure TLaTeXWriter.DescrBeginTableHeadRow;
begin
  if not TableCaptionWritten then
    DescrEndTableCaption;
  TableRowStartFlag := True;
end;

procedure TLaTeXWriter.DescrEndTableHeadRow;
begin
  WriteLn('\\ \hline');
end;

procedure TLaTeXWriter.DescrBeginTableRow;
begin
  if not TableCaptionWritten then
    DescrEndTableCaption;
  TableRowStartFlag := True;
end;

procedure TLaTeXWriter.DescrEndTableRow;
begin
  WriteLn('\\');
end;

procedure TLaTeXWriter.DescrBeginTableCell;
begin
  if TableRowStartFlag then
    TableRowStartFlag := False
  else
    Write(' & ');
end;

procedure TLaTeXWriter.DescrEndTableCell;
begin
  // Do nothing
end;


procedure TLaTeXWriter.WriteDescr(Element: TPasElement);
var
  DocNode: TDocNode;
begin
  DocNode := Engine.FindDocNode(Element);
  if Assigned(DocNode) then
    begin
    if not IsDescrNodeEmpty(DocNode.Descr) then
      WriteDescr(Element, DocNode.Descr)
    else if not IsDescrNodeEmpty(DocNode.ShortDescr) then
      WriteDescr(Element, DocNode.ShortDescr);
    end;
end;

procedure TLaTeXWriter.WriteDescr(AContext: TPasElement; DescrNode: TDOMElement);
begin
  if Assigned(DescrNode) then
    ConvertDescr(AContext, DescrNode, False);
end;

function TLaTeXWriter.ConstValue(ConstDecl: TPasConst): String;
begin
  if Assigned(ConstDecl) then
    Result := ConstDecl.ClassName
  else
    Result := '<nil>';
end;

procedure TLaTexWriter.WriteUnitOverview(ASection: TPasSection);
var
  i: Integer;
  UnitRef: TPasType;
  DocNode: TDocNode;
begin
  if ASection.UsesList.Count > 0 then
  begin
    WriteLnF('\section{%s}', [SDocUsedUnits]);
    WriteLnF('\begin{FPCltable}{lr}{%s}{%s:0units}',
      [Format(SDocUsedUnitsByUnitXY, [Module.Name]), ModuleName]);
    WriteLn('Name & Page \\ \hline');
    for i := 0 to ASection.UsesList.Count - 1 do
    begin
      UnitRef := TPasType(ASection.UsesList[i]);
      WriteLnF('%s\index{unit!%s} & \pageref{%s} \\',
        [UnitRef.Name, UnitRef.Name, StripTex(GetLabel(UnitRef))]);
    end;
    WriteLn('\end{FPCltable}');
  end;
  DocNode := Engine.FindDocNode(ASection.Parent);
  if Assigned(DocNode) and not IsDescrNodeEmpty(DocNode.Descr) then
  begin
    WriteLnF('\section{%s}', [EscapeTex(SDocOverview)]);
    WriteDescr(ASection.Parent, DocNode.Descr);
    Writeln('');
  end;
end;

procedure TLaTeXWriter.WriteResourceStrings(ASection: TPasSection);
var
  ResStrDecl: TPasResString;
  i: Integer;
begin
  if ASection.ResStrings.Count > 0 then
  begin
    StartSubSection(SDocResStrings,ModuleName+'ResStrings');
    for i := 0 to ASection.ResStrings.Count - 1 do
    begin
      ResStrDecl := TPasResString(ASection.ResStrings[i]);
      StartListing(false, '');
      Writeln(ResStrDecl.GetDeclaration(True));
      EndListing;
      WriteLabel(ResStrDecl);
      WriteIndex(ResStrDecl);
      WriteDescr(ResStrDecl);
      Writeln('');
    end;
  end;
end;

procedure TLaTeXWriter.WriteConsts(ASection: TPasSection);
var
  i: Integer;
  ConstDecl: TPasConst;
begin
  if ASection.Consts.Count > 0 then
  begin
    WriteLnF('\subsection{%s}\label{suse:%sConstants}',
      [EscapeTex(SDocConstants), EscapeTex(ModuleName)]);
    for i := 0 to ASection.Consts.Count - 1 do
    begin
      ConstDecl := TPasConst(ASection.Consts[i]);
      StartListing(False);
      WriteLn(EscapeTex(ConstDecl.GetDeclaration(True)));
      EndListing;
      WriteLabel(ConstDecl);  
      WriteIndex(ConstDecl);
      WriteDescr(ConstDecl);
    end;
  end;
end;

procedure TLaTeXWriter.WriteEnumElements(TypeDecl : TPasEnumType);

Var
  EV : TPasEnumValue;
  I : Integer;
  DocNode : TDocNode;
  
begin
  With TypeDecl do
    begin
    SortElementList(Values);
    DescrBeginTable(2,True);
    DescrBeginTableCaption;
      Writeln(EscapeTex(Format(SDocValuesForEnum,[TypeDecl.Name])));
    DescrEndTableCaption;
    DescrBeginTableHeadRow;
      DescrBeginTableCell;
        Writeln(EscapeTex(SDocValue));
      DescrEndTableCell;
      DescrBeginTableCell;
        Writeln(EscapeTex(SDocExplanation));
      DescrEndTableCell;
    DescrEndTableHeadRow;
    For I:=0 to Values.Count-1 do
      begin
      EV:=TPasEnumValue(Values[i]);
      DescrBeginTableRow;
        DescrBeginTableCell;
          Writeln(EscapeTex(EV.Name));
        DescrEndTableCell;
        DescrBeginTableCell;
          DocNode := Engine.FindDocNode(EV);
          if Assigned(DocNode) and (not IsDescrNodeEmpty(DocNode.ShortDescr)) then
            WriteDescr(EV,DocNode.ShortDescr);
        DescrEndTableCell;
      DescrEndTableRow;  
      end;
    DescrEndTable;
    end;  
end;

procedure TLaTeXWriter.WriteTypes(ASection: TPasSection);
var
  i: Integer;
  TypeDecl: TPasType;
begin
  if ASection.Types.Count > 0 then
  begin
    StartSubSection(SDocTypes,ModuleName+'Types');
    for i := 0 to ASection.Types.Count - 1 do
    begin
      TypeDecl := TPasType(ASection.Types[i]);
      StartListing(False);
      Writeln(EscapeTex(TypeDecl.GetDeclaration(True)));
      EndListing;
      WriteLabel(TypeDecl);
      WriteIndex(TypeDecl);
      If TypeDecl is TPasEnumType then
        begin
        WriteENumElements(TypeDecl as TPasEnumType);
        end;
      WriteDescr(TypeDecl);
    end;
  end;
end;

procedure TLaTeXWriter.WriteVars(ASection: TPasSection);
var                        
  VarDecl: TPasVariable;
  i: Integer;
begin
  if ASection.Variables.Count > 0 then
  begin
    StartSubsection(SDocVariables,ModuleName+'Variables');
    for i := 0 to ASection.Variables.Count - 1 do
    begin
      VarDecl := TPasVariable(ASection.Variables[i]);
      StartListing(False);
      WriteLn(EscapeTex(VarDecl.GetDeclaration(True)));
      EndListing;
      WriteLabel(VarDecl);
      WriteIndex(VarDecl);
      WriteDescr(VarDecl);
    end;
  end;
end;

procedure TLaTeXWriter.WriteVarsConstsTypes(ASection: TPasSection);
begin
  With Asection do
    if (Consts.Count > 0) or 
       (Types.Count > 0) or
       (Variables.Count > 0) or
       (ResStrings.Count>0) then
      begin
      StartSection(SDocConstsTypesVars, ModuleName+'ConstsTypesVars');
      WriteResourceStrings(ASection);
      WriteConsts(ASection);
      WriteTypes(ASection);
      WriteVars(ASection);
      end;
end;

const 
  SVisibility: array[TPasMemberVisibility] of string = 
       ('Default', 'Private', 'Protected', 'Public',
      'Published', 'Automated');

procedure TLatexWriter.WriteProcedure(ProcDecl : TPasProcedureBase);
var
  DocNode: TDocNode;
  OP : TPasOverloadedProc;
  i : integer; 
begin
  With ProcDecl do
    begin
    if Not (Assigned(Parent) and Parent.InheritsFrom(TPasClassType)) then
      begin
      StartSubSection(Name);
      WriteLabel(ProcDecl);
      WriteIndex(ProcDecl);
      end
    else
      begin // Parent assigned and hence method.
      StartSubSection(Parent.Name+'.'+Name);
      WriteLabel(ProcDecl);
      WriteIndex(Parent.Name+'.'+Name);
      end;  
    Writeln('\begin{FPCList}');
    DocNode := Engine.FindDocNode(ProcDecl);
    if Assigned(DocNode) and Assigned(DocNode.ShortDescr) then
      begin
      Writeln('\Synopsis');
      WriteDescr(ProcDecl, DocNode.ShortDescr);
      end;
    Writeln('\Declaration ');  
    StartListing(False);
    if ClassType = TPasOverloadedProc then
      begin
      OP:=TPasOverloadedProc(ProcDecl);
      for i := 0 to OP.Overloads.Count - 1 do
        begin
        WriteLn(TPasProcedure(OP.Overloads[i]).GetDeclaration(True));
        end;
      end
    else
      WriteLn(GetDeclaration(True));
    EndListing;
    If Assigned(Parent) then
      begin
      Writeln('\Visibility');
      Writeln(VisibilityNames[Visibility])
      end;
    if Assigned(DocNode) and Assigned(DocNode.Descr) then
      begin
      Writeln('\Description');
      WriteDescr(ProcDecl);
      end;
    if Assigned(DocNode) and Assigned(DocNode.ErrorsDoc) then   
      begin
      Writeln('\Errors');
      WriteDescr(ProcDecl, DocNode.ErrorsDoc);
      end;
    WriteSeeAlso(DocNode);
    Writeln('\end{FPCList}');
    WriteExample(DocNode);
    end;
end;

procedure TLaTeXWriter.WriteFunctionsAndProcedures(ASection: TPasSection);
var
  i: Integer;
begin
  if ASection.Functions.Count > 0 then
    begin
    StartSection(SDocProceduresAndFunctions,ModuleName+'Functions');
    for i := 0 to ASection.Functions.Count - 1 do
      WriteProcedure(TPasProcedureBase(ASection.Functions[i]));
    end;
end;

procedure TlatexWriter.WriteExample(ADocNode: TDocNode);
var
  Example: TDOMElement;
begin
  if Assigned(ADocNode) then
  begin
    Example := ADocNode.FirstExample;
    while Assigned(Example) do
    begin
      WritelnF('\FPCexample{%s}', [Engine.GetExampleFileName(Example)]);
      if Assigned(Example.NextSibling) then
        WriteLn('');
      Example := TDomElement(Example.NextSibling);   
    end;
  end;
end;

procedure TLateXWriter.WriteSeeAlso(ADocNode: TDocNode);
var
  Node: TDOMNode;
  s: String;
begin
  if Assigned(ADocNode) and Assigned(ADocNode.SeeAlso) and
    Assigned(ADocNode.SeeAlso.FirstChild) then
  begin
    Writeln('\SeeAlso');
    Node := ADocNode.SeeAlso.FirstChild;
    while Assigned(Node) do
    begin 
      if (Node.NodeType = ELEMENT_NODE) and 
        (Node.NodeName = 'link') then
      begin
        S:=TDomElement(Node)['id'];
        DescrBeginLink(S);
        Writeln(S);
        DescrEndLink();
        if Assigned(Node.NextSibling) Then
          Writeln(',');
      end;  
      Node:=Node.NextSibling;  
    end;
  end;   
end;

procedure TLaTeXWriter.WriteClasses(ASection: TPasSection);
var
  i: Integer;
begin
  if (ASection.Classes.Count > 0) then
  begin
    for i := 0 to ASection.Classes.Count - 1 do
      WriteClassDecl(TPasClassType(ASection.Classes[i]));
  end;

end;

procedure TLaTeXWriter.ProcessSection(ASection: TPasSection);
begin
  With ASection do
    begin
    SortElementList(UsesList);
    SortElementList(Declarations);
    SortElementList(ResStrings);
    SortElementList(Types);
    SortElementList(Consts);
    SortElementList(Classes);
    SortElementList(Functions);
    SortElementList(Variables);
    end;
  WriteUnitOverView(ASection);
  WriteVarsConstsTypes(ASection);
  WriteFunctionsAndProcedures(ASection);
  WriteClasses(ASection);
end;

Function TLatexWriter.ShowMember(M : TPasElement) : boolean;

begin
  Result:=not ((M.Visibility=visPrivate) and Engine.HidePrivate);
  If Result then
    Result:=Not ((M.Visibility=visProtected) and Engine.HideProtected)
end;

procedure TLatexWriter.WriteClassMethodOverview(ClassDecl : TPasClassType);
var
  Member: TPasElement;
  i, j: Integer;
  s: String;
  Arg: TPasArgument;
  DocNode: TDocNode;
  List : TStringList;
  
begin
  List:=TStringList.Create;
  Try
    List.Sorted:=True;
    for i := 0 to ClassDecl.Members.Count - 1 do
      begin
      Member := TPasElement(ClassDecl.Members[i]);
      With Member do 
        if InheritsFrom(TPasProcedureBase) and ShowMember(Member) then
      List.AddObject(Member.Name,Member);
      end;
    If List.Count>0 then
      begin
      StartSubSection(SDocMethodOverview);
      WriteLabel(GetLabel(ClassDecl) + ':Methods');
      WriteLn('\begin{tabularx}{\textwidth}{llX}');
      WriteLnF('%s & %s & %s \\ \hline',  [EscapeTex(SDocPage), EscapeTex(SDocMethod), EscapeTex(SDocDescription)]);
      For I:=0 to List.Count-1 do
        begin
        Member:=TPasElement(List.Objects[i]);
        DocNode := Engine.FindDocNode(Member);
        WriteF('\pageref{%s} & %s & ',[StripTex(GetLabel(Member)), EscapeTex(Member.Name)]);
        if Assigned(DocNode) and Assigned(DocNode.ShortDescr) then
          WriteDescr(Member, DocNode.ShortDescr);
        WriteLn('\\');
        end;
      WriteLn('\hline');
      WriteLn('\end{tabularx}');
      end;
  Finally
    List.Free;
  end;  
end;

procedure TLatexWriter.WriteClassPropertyOverview(ClassDecl : TPasClassType);
var
  Member: TPasElement;
  i, j: Integer;
  s: String;
  Arg: TPasArgument;
  DocNode: TDocNode;
  List : TStringList;
  
begin
  // Write property overview
  List:=TStringList.Create;
  Try
    List.Sorted:=True;
    for i := 0 to ClassDecl.Members.Count - 1 do
      begin
      Member := TPasElement(ClassDecl.Members[i]);
      With Member do
        if InheritsFrom(TPasProperty) and SHowMember(Member) then
          List.AddObject(Member.Name,Member)
      end;
    If (List.Count>0) then  
      begin
      StartSubSection(SDocPropertyOverview);
      WriteLabel(GetLabel(ClassDecl) + ':Properties');
      WriteLn('\begin{tabularx}{\textwidth}{lllX}');
      WriteLnF('%s & %s & %s & %s \\ \hline',
        [EscapeTex(SDocPage), EscapeTex(SDocProperty), EscapeTex(SDocAccess), EscapeTex(SDocDescription)]);
      For I:=0 to List.Count-1 do
        begin  
        Member:=TPasElement(List.objects[i]);
        WriteF('\pageref{%s} & %s & ', [StripTex(GetLabel(Member)), EscapeTex(Member.Name)]);
        setlength(S,0);
        if Length(TPasProperty(Member).ReadAccessorName) > 0 then
          s := s + 'r';
        if Length(TPasProperty(Member).WriteAccessorName) > 0 then
          s := s + 'w';
        if Length(TPasProperty(Member).StoredAccessorName) > 0 then
          s := s + 's';
        Write(s + ' & ');
        DocNode := Engine.FindDocNode(Member);
        if Assigned(DocNode) and Assigned(DocNode.ShortDescr) then
          WriteDescr(Member, DocNode.ShortDescr);
        WriteLn('\\');
        end;
      WriteLn('\hline');
      WriteLn('\end{tabularx}');
      end;
  Finally
    List.Free;
  end;      
end;


procedure TLaTeXWriter.WriteClassDecl(ClassDecl: TPasClassType);
var
  DocNode: TDocNode;
  Vis: TPasMemberVisibilities;
  Member: TPasElement;
  i: Integer;
begin
  StartSection(ClassDecl.Name);
  WriteLabel(ClassDecl);
  WriteIndex(ClassDecl);
  DocNode := Engine.FindDocNode(ClassDecl);
  if Assigned(DocNode) and ((not IsDescrNodeEmpty(DocNode.Descr)) or
    (not IsDescrNodeEmpty(DocNode.ShortDescr))) then
  begin
    StartSubSection(SDocDescription);
    WriteDescr(ClassDecl);
  end;

  // Write method overview
  WriteClassMethodOverView(ClassDecl);
  // Write Property Overview;
  WriteClassPropertyOverView(ClassDecl);

  // Write method & property descriptions
  
  // Determine visibilities 

  Vis := AllVisibilities;
  if Engine.HidePrivate then
    Exclude(Vis,visPrivate);
  if Engine.HideProtected then
    Exclude(Vis,visProtected);

  for i := 0 to ClassDecl.Members.Count - 1 do
    begin
    Member := TPasElement(ClassDecl.Members[i]);
    if ((Member.InheritsFrom(TPasProcedureBase)) and
        (Member.Visibility in Vis)) then
      WriteProcedure(TPasProcedureBase(Member));
    end;

  // properties.

  for i := 0 to ClassDecl.Members.Count - 1 do
    begin
    Member := TPasElement(ClassDecl.Members[i]);
    if ((Member.InheritsFrom(TPasProperty)) and
        (Member.Visibility in Vis)) then
      WriteProperty(TPasProperty(Member));
    end;

end;

procedure TLaTexWriter.WriteProperty(PropDecl : TPasProperty);
var
  DocNode: TDocNode;
  S: String;
begin
  With PropDecl do
    begin
    StartSubSection(Parent.Name+'.'+Name);
    WriteLabel(PropDecl);
    WriteIndex(Parent.Name+'.'+Name);
    Writeln('\begin{FPCList}');
    DocNode := Engine.FindDocNode(PropDecl);
    if Assigned(DocNode) and Assigned(DocNode.ShortDescr) then
      begin
      Writeln('\Synopsis');
      WriteDescr(PropDecl, DocNode.ShortDescr);
      end;
    Writeln('\Declaration ');  
    StartListing(False);
    WriteLn('Property '+GetDeclaration(True));
    EndListing;
    If Assigned(Parent) then
      begin
      Writeln('\Visibility');
      Writeln(VisibilityNames[Visibility])
      end;
    Writeln('\Access');
    Setlength(S,0);
    If Length(ReadAccessorName) > 0 then
      S:='Read';
    if Length(WriteAccessorName) > 0 then
      begin
      If S<>'' then
        S:=S+',';
      S:=S+'Write';
      end;
    Writeln(S);  
    if Assigned(DocNode) and Assigned(DocNode.Descr) then
      begin
      Writeln('\Description');
      WriteDescr(PropDecl);
      end;
    if Assigned(DocNode) and Assigned(DocNode.ErrorsDoc) then   
      begin
      Writeln('\Errors');
      WriteDescr(PropDecl, DocNode.ErrorsDoc);
      end;
    WriteSeeAlso(DocNode);
    Writeln('\end{FPCList}');
    WriteExample(DocNode);
    end;
end;

Function CompareElements(P1,P2 : Pointer) : Integer;

begin
  Result:=CompareText(TPasElement(P1).Name,TPasElement(P2).Name);
end;

procedure TLaTeXWriter.SortElementList(List : TList);

begin
  List.Sort(@CompareElements)
end;


procedure TLaTeXWriter.WriteLabel(El: TPasElement);
begin
  WriteLabel(GetLabel(El));
end;

procedure TLaTeXWriter.WriteLabel(const s: String);
begin
  WriteLnF('\label{%s}', [LowerCase(StripTex(s))]);
end;

procedure TLaTeXWriter.WriteIndex(El : TPasElement);
begin
  WriteIndex(El.Name);
end;

procedure TLaTeXWriter.WriteIndex(const s : String);
begin
  Write('\index{');
  Write(EscapeTex(s));
  Writeln('}');
end;

procedure TLaTeXWriter.StartListing(Frames: Boolean; const name: String);
begin
  if Not LaTexHighLight then
    Writeln('\begin{verbatim}')
  else  
    if Frames then
      Writelnf('\begin{lstlisting}{%s}',[StripTex(Name)])
    else  
      Writelnf('\begin{lstlisting}[frame=]{%s}',[StripTex(Name)]);
end;

procedure TLaTeXWriter.StartListing(Frames : Boolean);
begin
  StartListing(Frames,'');
end;

procedure TLaTeXWriter.EndListing;
begin
  If LaTexHighLight then
    Writeln('\end{lstlisting}')
  else  
    Writeln('\end{verbatim}')
end;

procedure TLatexWriter.WriteCommentLine;
const
  CommentLine = 
    '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%';
begin
  Writeln(CommentLine);
end;

procedure TLatexWriter.WriteComment(Comment : String);
begin
  Write('% ');
  Writeln(Comment);
end;


procedure TLatexWriter.StartSection(SectionName : String; SectionLabel : String);
begin
  StartSection(SectionName);
  WriteLabel(SectionLabel);
end;

procedure TLatexWriter.StartSection(SectionName : String);
begin
  Writeln('');
  WriteCommentLine;
  WriteComment(SectionName);
  Writeln('\section{'+EscapeTex(SectionName)+'}');
end;

procedure TLatexWriter.StartSubSection(SubSectionName : String; SubSectionLabel : String);
begin
  StartSubSection(SubSectionName);
  WriteLabel(SubsectionLabel);
end;

procedure TLatexWriter.StartSubSection(SubSectionName : String);
begin
  Writeln('');
  WriteComment(SubsectionName);
  Writeln('\subsection{'+EscapeTex(SubSectionName)+'}');
end;

procedure TLatexWriter.StartChapter(ChapterName : String; ChapterLabel : String);
begin
  StartChapter(ChapterName);
  WriteLabel(ChapterLabel);
end;

procedure TLatexWriter.StartChapter(ChapterName : String);
begin
  Writeln('');
  WriteCommentLine;
  WriteComment(ChapterName);
  WriteCommentLine;
  Writeln('\chapter{'+EscapeTex(ChapterName)+'}');
end;

procedure CreateLaTeXDocForPackage(APackage: TPasPackage; AEngine: TFPDocEngine);
var
  Writer: TLaTeXWriter;
begin
  Writer := TLaTeXWriter.Create(APackage, AEngine);
  try
    Writer.WriteDoc;
  finally
    Writer.Free;
  end;
end;


end.


{
  $Log$
  Revision 1.2  2003-03-18 01:11:51  michael
  + Some fixes to deal with illegal tex characters

  Revision 1.1  2003/03/17 23:03:20  michael
  + Initial import in CVS

  Revision 1.13  2003/03/13 22:02:13  sg
  * New version with many bugfixes and our own parser (now independent of the
    compiler source)

  Revision 1.12  2002/10/20 22:49:31  michael
  + Sorted all overviews. Added table with enumeration values for enumerated types.

  Revision 1.11  2002/05/24 00:13:22  sg
  * much improved new version, including many linking and output fixes

  Revision 1.10  2002/03/12 10:58:36  sg
  * reworked linking engine and internal structure

  Revision 1.9  2002/01/20 11:19:55  michael
  + Added link attribute and property to TFPElement

  Revision 1.8  2002/01/08 13:00:06  michael
  + Added correct array handling and syntax highlighting is now optional

  Revision 1.7  2002/01/08 08:22:40  michael
  + Implemented latex writer

  Revision 1.6  2001/12/17 14:41:42  michael
  + Split out of latex writer

  Revision 1.5  2001/12/17 13:41:18  jonas
    * OsPathSeparator -> PathDelim
}