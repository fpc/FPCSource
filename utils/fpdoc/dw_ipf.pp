{

    FPDoc  -  Free Pascal Documentation Tool
    Copyright (C) 2000 - 2003 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    * IPF output generator

    See the file COPYING, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

unit dw_IPF;

{$MODE objfpc}
{$H+}

interface

uses SysUtils, Classes, dWriter, DOM, dGlobals, PasTree;

const
  IPFHighLight : Boolean = False;
  IPFExtension   : String = '.ipf';

type
  TLabelType = (ltConst,ltVar,ltType,ltFunction,ltProcedure,ltClass,
                ltChapter,ltSection,ltSubsection,
                ltTable,ltFigure);

  { TIPFWriter }

  TIPFWriter = class(TFPDocWriter)
  protected
    f: Text;
    FLink: String;
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
//    procedure StartSection(SectionName : String);
    procedure StartSubSection(SubSectionName : String; SubSectionLabel : String);
//    procedure StartSubSection(SubSectionName : String);
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
    constructor Create(APackage: TPasPackage; AEngine: TFPDocEngine); override;
    procedure WriteDoc; override;
  end;


implementation





constructor TIPFWriter.Create(APackage: TPasPackage; AEngine: TFPDocEngine);

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
  inherited ;

  { Allocate labels for all elements for which we are going to create
    documentation. This is needed for links to work correctly. }

  // Allocate label for the package itself, if a name is given (i.e. <> '#')
  if Length(Package.Name) > 1 then
    AddLabel(Package);

  for i := 0 to Package.Modules.Count - 1 do
    ScanModule(TPasModule(Package.Modules[i]));
end;

procedure TIPFWriter.WriteDoc;
var
  i : Integer;

begin
  PackageName := LowerCase(Copy(Package.Name, 2, 255));
  If (Engine.OutPut='') then
    Engine.Output:=PackageName+IPFExtension;
  Assign(f, Engine.Output);
  Rewrite(f);
  try
    WriteLn('.* This file has been created automatically by FPDoc,');
    WriteLn('.* (c) 2000-2003 by Areca Systems GmbH / Sebastian Guenther (sg@freepascal.org)');
    for i := 0 to Package.Modules.Count - 1 do
    begin
      Module := TPasModule(Package.Modules[i]);
      ModuleName := LowerCase(Module.Name);
      WriteLn('');
      Write(':h1 name=');
      WriteLabel(Module);
      WriteLnF('.%s', [EscapeTex(Format(SDocUnitTitle, [Module.Name]))]);
      ProcessSection(Module.InterfaceSection);
    end;
  finally
    Close(f);
  end;
end;

function TIPFWriter.GetLabel(AElement: TPasElement): String;
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
      Result[i] := '_';
end;

procedure TIPFWriter.Write(const s: String);
begin
  System.Write(f, s);
end;

procedure TIPFWriter.WriteF(const s: String; const Args: array of const);
begin
  System.Write(f, Format(s, Args));
end;

procedure TIPFWriter.WriteLn(const s: String);
begin
  System.WriteLn(f, s);
end;

procedure TIPFWriter.WriteLnF(const s: String; const Args: array of const);
begin
  System.WriteLn(f, Format(s, Args));
end;

Function TIPFWriter.EscapeTex(S : String) : String;

var
  i: Integer;

begin
  SetLength(Result, 0);
  for i := 1 to Length(S) do
    case S[i] of
      '.':              // Escape these characters
        Result := Result + '&per.';
      ':':
        Result := Result + '&colon.';
      ',':
        Result := Result + '&comma.';
      '&':
        Result := Result + '&amp.';
(*
³&amp.               ³ampersand           ³&                   ³
³&and.               ³and                 ³^                   ³
³&apos.              ³apostrophe          ³'                   ³
³&asterisk.          ³asterisk            ³*                   ³
³&atsign.            ³at sign             ³@                   ³
³&bslash., &bsl.     ³back slash          ³\                   ³
³&caret.             ³caret symbol        ³^                   ³
³&cdq.               ³close double quote  ³"                   ³
³&csq.               ³close single quote  ³'                   ³
³&comma.             ³comma               ³,                   ³
³&colon.             ³colon               ³:                   ³
³&dash.              ³dash                ³-                   ³
³&degree., &deg.     ³degree              ³ø                   ³
³&dollar.            ³dollar sign         ³$                   ³
³&dot.               ³dot                 ³ú                   ³
³&darrow.            ³down arrow          ³                   ³
³&emdash.            ³em dash             ³-                   ³
³&endash.            ³en dash             ³-                   ³
³&eq., &equals.,     ³equal sign          ³=                   ³
³&eqsym.             ³                    ³                    ³
³&xclm., &xclam.     ³exclamation point   ³!                   ³
³&gtsym., &gt.       ³greater than        ³>                   ³
³&house.             ³house               ³                   ³
³&hyphen.            ³hyphen              ³-                   ³
³&larrow.            ³left arrow          ³                   ³
³&lahead.            ³left arrowhead      ³                   ³
³&lbrace., &lbrc.    ³left brace          ³{                   ³
³&lbracket. &lbrk.   ³left bracket        ³[                   ³
³&lpar. , &lparen.   ³left parenthesis    ³(                   ³
³&mdash.             ³em dash             ³-                   ³
³&minus.             ³minus sign          ³-                   ³
³&ndash.             ³en dash             ³-                   ³
³&numsign.           ³number sign         ³#                   ³
³&odq.               ³open double quote   ³"                   ³
³&osq.               ³open single quote   ³`                   ³
³&percent.           ³percent             ³%                   ³
³&per.               ³period              ³.                   ³
³&plus.              ³plus sign           ³+                   ³
³&rbrace., &rbrc.    ³right brace         ³}                   ³
³&rbracket., &rbrk.  ³right bracket       ³]                   ³
³&rpar., &rparen.    ³right parenthesis   ³)                   ³
³&slash., &slr.      ³slash               ³/                   ³
³&splitvbar.         ³split vertical bar  ³|                   ³
³                    ³(piping symbol)     ³                    ³
³&sqbul.             ³square bullet       ³þ                   ³
³&tilde.             ³tilde               ³~                   ³
³&us.                ³underscore          ³_                   ³
*)
      else
        Result := Result + S[i];
    end;
end;

Function TIPFWriter.StripTex(S : String) : String;

var
  I,L: Integer;

begin
  Result:=S;
//  SetLength(Result, 0);
//  for i := 1 to Length(S) do
//    If not (S[i] in ['&','{','}','#','_','$','%','''','~','^', '\']) then
//      Result := Result + S[i];
end;

procedure TIPFWriter.DescrWriteText(const AText: DOMString);

begin
  Write(EscapeTex(AText));
end;

procedure TIPFWriter.DescrBeginBold;
begin
  Write(':hp2.');
end;

procedure TIPFWriter.DescrEndBold;
begin
  WriteLn(':ehp2.');
end;

procedure TIPFWriter.DescrBeginItalic;
begin
  Write(':hp1.');
end;

procedure TIPFWriter.DescrEndItalic;
begin
  WriteLn(':ehp1.');
end;

procedure TIPFWriter.DescrBeginEmph;
begin
  Write(':hp2.');
end;

procedure TIPFWriter.DescrEndEmph;
begin
  Write(':ehp2.');
end;

procedure TIPFWriter.DescrWriteFileEl(const AText: DOMString);
begin
  Write(':hp2.');
  DescrWriteText(AText);
  Write(':ehp2.');
end;

procedure TIPFWriter.DescrWriteKeywordEl(const AText: DOMString);
begin
  Write(':hp2.');
  DescrWriteText(AText);
  Write(':ehp2.');
end;

procedure TIPFWriter.DescrWriteVarEl(const AText: DOMString);
begin
  Write(':hp2.');
  DescrWriteText(AText);
  Write(':ehp2.');
end;

procedure TIPFWriter.DescrBeginLink(const AId: DOMString);
var
  i: Integer;
begin
  FLink := Engine.ResolveLink(Module, AId);
  While pos(':',flink)>0 do flink[pos(':',flink)]:='_';
//  System.WriteLn('Link "', AId, '" => ', FLink);
  WriteF(':link reftype=hd refid=%s.', [flink]);
end;

procedure TIPFWriter.DescrEndLink;
begin
  Write(':elink.');
end;

procedure TIPFWriter.DescrWriteLinebreak;
begin
  WriteLn('.br');
end;

procedure TIPFWriter.DescrBeginParagraph;
begin
  WriteLn(':p.');
  // Do nothing
end;

procedure TIPFWriter.DescrEndParagraph;
begin
  WriteLn('');
  WriteLn('');
end;

procedure TIPFWriter.DescrBeginCode(HasBorder: Boolean;
  const AHighlighterName: String);
begin
  StartListing(HasBorder);
end;

procedure TIPFWriter.DescrWriteCodeLine(const ALine: String);
begin
  WriteLn(EscapeTex(ALine));
end;

procedure TIPFWriter.DescrEndCode;
begin
  EndListing
end;

procedure TIPFWriter.DescrBeginOrderedList;
begin
  WriteLn(':ol.');
end;

procedure TIPFWriter.DescrEndOrderedList;
begin
  WriteLn(':eol.');
end;

procedure TIPFWriter.DescrBeginUnorderedList;
begin
  WriteLn(':ul.');
end;

procedure TIPFWriter.DescrEndUnorderedList;
begin
  WriteLn(':eul.');
end;

procedure TIPFWriter.DescrBeginDefinitionList;
begin
  WriteLn(':dl.');
end;

procedure TIPFWriter.DescrEndDefinitionList;
begin
  WriteLn(':edl.');
end;

procedure TIPFWriter.DescrBeginListItem;
begin
  Write(':li.');
end;

procedure TIPFWriter.DescrEndListItem;
begin
  WriteLn('');
end;

procedure TIPFWriter.DescrBeginDefinitionTerm;
begin
  Write(':li.');
end;

procedure TIPFWriter.DescrEndDefinitionTerm;
begin
  WriteLn('');
end;

procedure TIPFWriter.DescrBeginDefinitionEntry;
begin
  WriteLn('');
  // Do nothing
end;

procedure TIPFWriter.DescrEndDefinitionEntry;
begin
  WriteLn('');
end;

procedure TIPFWriter.DescrBeginSectionTitle;
begin
  Write(':h3.');
end;

procedure TIPFWriter.DescrBeginSectionBody;
begin
  WriteLn('');
end;

procedure TIPFWriter.DescrEndSection;
begin
  WriteLn('');
  // Do noting
end;

procedure TIPFWriter.DescrBeginRemark;
begin
  WriteLn(':note.');
end;

procedure TIPFWriter.DescrEndRemark;
begin
  WriteLn('');
end;

procedure TIPFWriter.DescrBeginTable(ColCount: Integer; HasBorder: Boolean);
begin
  // !!!: How do we set the border?
//  for i := 1 to ColCount do
//    Write('l');
//  write('}{');
  TableCaptionWritten:=False;
end;

procedure TIPFWriter.DescrEndTable;
begin
  WriteLn(':etable.');
end;

procedure TIPFWriter.DescrBeginTableCaption;
begin
  // Do nothing.
end;

procedure TIPFWriter.DescrEndTableCaption;
begin
  Write('');
//  Inc(FTableCount);
//  Write(IntToStr(FTableCount));
//  Writeln('}');
  TableCaptionWritten := True;
  Write(':table cols=''30 50''.');
end;

procedure TIPFWriter.DescrBeginTableHeadRow;
begin
  if not TableCaptionWritten then
    DescrEndTableCaption;
  TableRowStartFlag := True;
  WriteLn(':row.:c.');
end;

procedure TIPFWriter.DescrEndTableHeadRow;
begin
  WriteLn('');
end;

procedure TIPFWriter.DescrBeginTableRow;
begin
  if not TableCaptionWritten then
    DescrEndTableCaption;
  TableRowStartFlag := True;
  WriteLn(':row.:c.');
end;

procedure TIPFWriter.DescrEndTableRow;
begin
end;

procedure TIPFWriter.DescrBeginTableCell;
begin
  if TableRowStartFlag then
    TableRowStartFlag := False
  else
    WriteLn(':c.');
end;

procedure TIPFWriter.DescrEndTableCell;
begin
  WriteLn('');
  // Do nothing
end;


function TIPFWriter.ConstValue(ConstDecl: TPasConst): String;
begin
  if Assigned(ConstDecl) then
    Result := ConstDecl.ClassName
  else
    Result := '<nil>';
end;

procedure TIPFWriter.WriteUnitOverview(ASection: TPasSection);
var
  i: Integer;
  UnitRef: TPasType;
  DocNode: TDocNode;
begin
  if ASection.UsesList.Count > 0 then
  begin
    WriteLnF(':h2.%s', [SDocUsedUnits]);
    WriteLn(':ol.');
    for i := 0 to ASection.UsesList.Count - 1 do
    begin
      UnitRef := TPasType(ASection.UsesList[i]);
      WriteLnF(':li.%s', [UnitRef.Name]);
    end;
    WriteLn(':eol.');
  end;
  DocNode := Engine.FindDocNode(ASection.Parent);
  if Assigned(DocNode) and not IsDescrNodeEmpty(DocNode.Descr) then
  begin
    WriteLnF(':h2.%s', [EscapeTex(SDocOverview)]);
    WriteDescr(ASection.Parent, DocNode.Descr);
    Writeln('');
  end;
end;

procedure TIPFWriter.WriteResourceStrings(ASection: TPasSection);
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

procedure TIPFWriter.WriteConsts(ASection: TPasSection);
var
  i: Integer;
  ConstDecl: TPasConst;
begin
  if ASection.Consts.Count > 0 then
  begin
    WriteLnF(':h3 name=suse_%sconstants.%s', [EscapeTex(ModuleName), EscapeTex(SDocConstants)]);
    for i := 0 to ASection.Consts.Count - 1 do
    begin
      ConstDecl := TPasConst(ASection.Consts[i]);
      StartListing(False);
      WriteLn(EscapeTex(ConstDecl.GetDeclaration(True)));
      EndListing;
//      WriteLabel(ConstDecl);
//      WriteIndex(ConstDecl);
      WriteDescr(ConstDecl);
    end;
  end;
end;

procedure TIPFWriter.WriteEnumElements(TypeDecl : TPasEnumType);

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

procedure TIPFWriter.WriteTypes(ASection: TPasSection);
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
      WriteLn(':h4 name='+GetLabel(TypeDecl)+'.');
//      WriteLn(':hdref refid='+GetLabel(TypeDecl)+'.');
//      WriteLabel(TypeDecl);
//      WriteIndex(TypeDecl);
      StartListing(False);
      Writeln(EscapeTex(TypeDecl.GetDeclaration(True)));
      EndListing;
      If TypeDecl is TPasEnumType then
        begin
        WriteENumElements(TypeDecl as TPasEnumType);
        end;
      WriteDescr(TypeDecl);
    end;
  end;
end;

procedure TIPFWriter.WriteVars(ASection: TPasSection);
var
  VarDecl: TPasVariable;
  i: Integer;
begin
  if ASection.Variables.Count > 0 then
  begin
    StartSubsection(SDocVariables,ModuleName+'Variables');
    for i := 0 to ASection.Variables.Count - 1 do
    begin
//      WriteIndex(VarDecl);
      VarDecl := TPasVariable(ASection.Variables[i]);
      WriteLn(':h4 name='+GetLabel(VarDecl)+'.');
      StartListing(False);
      WriteLn(EscapeTex(VarDecl.GetDeclaration(True)));
      EndListing;
      WriteDescr(VarDecl);
    end;
  end;
end;

procedure TIPFWriter.WriteVarsConstsTypes(ASection: TPasSection);
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

procedure TIPFWriter.WriteProcedure(ProcDecl : TPasProcedureBase);
var
  DocNode: TDocNode;
  OP : TPasOverloadedProc;
  i : integer;
begin
  With ProcDecl do
    begin
    if Not (Assigned(Parent) and Parent.InheritsFrom(TPasClassType)) then
      begin
      StartSubSection(Name, GetLabel(ProcDecl));
//      WriteLabel(ProcDecl);
//      WriteIndex(ProcDecl);
      end
    else
      begin // Parent assigned and hence method.
      StartSubSection(Parent.Name+'&per.'+Name, GetLabel(ProcDecl));
//      WriteLabel(ProcDecl);
//      WriteIndex(Parent.Name+'.'+Name);
      end;
//    Writeln('\begin{FPCList}');
    DocNode := Engine.FindDocNode(ProcDecl);
    if Assigned(DocNode) and Assigned(DocNode.ShortDescr) then
      begin
      Writeln(':hp2.Synopsis:ehp2.&colon. ');
      WriteDescr(ProcDecl, DocNode.ShortDescr);
      WriteLn('');
      WriteLn('.br');
      end;
    Writeln(':hp2.Declaration:ehp2.&colon. ');
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
    WriteLn('');
    WriteLn('.br');
    If Assigned(Parent) then
      begin
      Writeln(':hp2.Visibility:ehp2.&colon. ');
      Writeln(VisibilityNames[Visibility]);
      WriteLn('');
      WriteLn('.br');
      end;
    if Assigned(DocNode) and Assigned(DocNode.Descr) then
      begin
      Writeln(':hp2.Description:ehp2.&colon. ');
      WriteDescr(ProcDecl);
      WriteLn('');
      WriteLn('.br');
      end;
    if Assigned(DocNode) and Assigned(DocNode.ErrorsDoc) then
      begin
      Writeln(':hp2.Errors:ehp2.&colon.');
      WriteDescr(ProcDecl, DocNode.ErrorsDoc);
      WriteLn('');
      WriteLn('.br');
      end;
    WriteSeeAlso(DocNode);
    WriteLn('');
    WriteLn('.br');
//    Writeln('\end{FPCList}');
    WriteExample(DocNode);
    end;
end;

procedure TIPFWriter.WriteFunctionsAndProcedures(ASection: TPasSection);
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

procedure TIPFWriter.WriteExample(ADocNode: TDocNode);
var
  Example: TDOMElement;
begin
  if Assigned(ADocNode) then
  begin
    Example := ADocNode.FirstExample;
    while Assigned(Example) do
    begin
      WritelnF(':xmp.%s:exmp.', [EscapeTex(Engine.GetExampleFileName(Example))]);
      if Assigned(Example.NextSibling) then
        WriteLn('');
      Example := TDomElement(Example.NextSibling);
    end;
  end;
end;

procedure TIPFWriter.WriteSeeAlso(ADocNode: TDocNode);
var
  Node: TDOMNode;
  s: String;
begin
  if Assigned(ADocNode) and Assigned(ADocNode.SeeAlso) and
    Assigned(ADocNode.SeeAlso.FirstChild) then
  begin
    Writeln(':hp2.SeeAlso:ehp2.');
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

procedure TIPFWriter.WriteClasses(ASection: TPasSection);
var
  i: Integer;
begin
  if (ASection.Classes.Count > 0) then
  begin
    for i := 0 to ASection.Classes.Count - 1 do
      WriteClassDecl(TPasClassType(ASection.Classes[i]));
  end;

end;

procedure TIPFWriter.ProcessSection(ASection: TPasSection);
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

Function TIPFWriter.ShowMember(M : TPasElement) : boolean;

begin
  Result:=not ((M.Visibility=visPrivate) and Engine.HidePrivate);
  If Result then
    Result:=Not ((M.Visibility=visProtected) and Engine.HideProtected)
end;

procedure TIPFWriter.WriteClassMethodOverview(ClassDecl : TPasClassType);
var
  Member: TPasElement;
  i: Integer;
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
      StartSubSection(SDocMethodOverview, GetLabel(ClassDecl) + ':Methods');
//      WriteLabel();
      WriteLn(':parml.');
//      WriteLnF('%s & %s & %s \\ \hline',  [EscapeTex(SDocPage), EscapeTex(SDocMethod), EscapeTex(SDocDescription)]);
      For I:=0 to List.Count-1 do
        begin
        Member:=TPasElement(List.Objects[i]);
        DocNode := Engine.FindDocNode(Member);
        WriteF(':pt.:link reftype=hd refid=%s.%s:elink.:pd.',[StripTex(GetLabel(Member)), EscapeTex(Member.Name)]);
        if Assigned(DocNode) and Assigned(DocNode.ShortDescr) then
          WriteDescr(Member, DocNode.ShortDescr);
        WriteLn('');
        WriteLn('.br');
        end;
      WriteLn(':eparml.');
//      WriteLn('\end{tabularx}');
      end;
  Finally
    List.Free;
  end;
end;

procedure TIPFWriter.WriteClassPropertyOverview(ClassDecl : TPasClassType);
var
  Member: TPasElement;
  i: Integer;
  s: String;
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
      StartSubSection(SDocPropertyOverview, GetLabel(ClassDecl) + ':Properties');
//      WriteLabel(GetLabel(ClassDecl) + ':Properties');
      WriteLn(':parml.');
//      WriteLn('\begin{tabularx}{\textwidth}{lllX}');
//      WriteLnF('%s & %s & %s & %s \\ \hline',
//        [EscapeTex(SDocPage), EscapeTex(SDocProperty), EscapeTex(SDocAccess), EscapeTex(SDocDescription)]);
      For I:=0 to List.Count-1 do
        begin
        Member:=TPasElement(List.objects[i]);
        WriteF(':pt.:link reftype=hd refid=%s.%s:elink.:pd.',[StripTex(GetLabel(Member)), EscapeTex(Member.Name)]);
        setlength(S,0);
        if Length(TPasProperty(Member).ReadAccessorName) > 0 then
          s := s + 'r';
        if Length(TPasProperty(Member).WriteAccessorName) > 0 then
          s := s + 'w';
        if Length(TPasProperty(Member).StoredAccessorName) > 0 then
          s := s + 's';
//        Write(s + ' & ');
        DocNode := Engine.FindDocNode(Member);
        if Assigned(DocNode) and Assigned(DocNode.ShortDescr) then
          WriteDescr(Member, DocNode.ShortDescr);
        WriteLn('');
        WriteLn('.br');
        end;
      WriteLn(':eparml.');
      end;
  Finally
    List.Free;
  end;
end;


procedure TIPFWriter.WriteClassDecl(ClassDecl: TPasClassType);
var
  DocNode: TDocNode;
  Vis: TPasMemberVisibilities;
  Member: TPasElement;
  i: Integer;
begin
  StartSection(ClassDecl.Name, GetLabel(ClassDecl));
//  WriteLabel(ClassDecl);
//  WriteIndex(ClassDecl);
  DocNode := Engine.FindDocNode(ClassDecl);
  if Assigned(DocNode) and ((not IsDescrNodeEmpty(DocNode.Descr)) or
    (not IsDescrNodeEmpty(DocNode.ShortDescr))) then
  begin
//    StartSubSection(SDocDescription, GetLabel(ClassDecl) + ':Description');
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

procedure TIPFWriter.WriteProperty(PropDecl : TPasProperty);
var
  DocNode: TDocNode;
  S: String;
begin
  With PropDecl do
    begin
    StartSubSection(Parent.Name+'&per.'+Name, GetLabel(PropDecl));
//    WriteLabel(PropDecl);
//    WriteIndex(Parent.Name+'.'+Name);
//    Writeln('\begin{FPCList}');
    DocNode := Engine.FindDocNode(PropDecl);
    if Assigned(DocNode) and Assigned(DocNode.ShortDescr) then
      begin
      Writeln(':hp2.Synopsis:ehp2.&colon. ');
      WriteDescr(PropDecl, DocNode.ShortDescr);
      WriteLn('');
      WriteLn('.br');
      end;
    Writeln(':hp2.Declaration:ehp2.&colon. ');
    StartListing(False);
    WriteLn('Property '+GetDeclaration(True));
    EndListing;
    WriteLn('');
    WriteLn('.br');
    If Assigned(Parent) then
      begin
      Writeln(':hp2.Visibility:ehp2.&colon. ');
      Writeln(VisibilityNames[Visibility]);
      WriteLn('');
      WriteLn('.br');
      end;
    Writeln(':hp2.Access:ehp2.&colon.');
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
    WriteLn('');
    WriteLn('.br');
    if Assigned(DocNode) and Assigned(DocNode.Descr) then
      begin
      Writeln(':hp2.Description:ehp2.&colon.');
      WriteDescr(PropDecl);
      WriteLn('');
      WriteLn('.br');
      end;
    if Assigned(DocNode) and Assigned(DocNode.ErrorsDoc) then
      begin
      Writeln(':hp2.Errors:ehp2.&colon. ');
      WriteDescr(PropDecl, DocNode.ErrorsDoc);
      WriteLn('');
      WriteLn('.br');
      end;
    WriteSeeAlso(DocNode);
    WriteLn('');
      WriteLn('.br');
//    Writeln('\end{FPCList}');
    WriteExample(DocNode);
    end;
end;

Function CompareElements(P1,P2 : Pointer) : Integer;

begin
  Result:=CompareText(TPasElement(P1).Name,TPasElement(P2).Name);
end;

procedure TIPFWriter.SortElementList(List : TList);

begin
  List.Sort(@CompareElements)
end;


procedure TIPFWriter.WriteLabel(El: TPasElement);
begin
  WriteLabel(GetLabel(El));
end;

procedure TIPFWriter.WriteLabel(const s: String);
var
  x: String;
begin
  X:=s;
  While pos(':',x)>0 do x[pos(':',x)]:='_';
  WriteF('%s', [LowerCase(StripTex(x))]);
end;

procedure TIPFWriter.WriteIndex(El : TPasElement);
begin
  WriteIndex(El.Name);
end;

procedure TIPFWriter.WriteIndex(const s : String);
begin
//  Write('\index{');
//  Write(EscapeTex(s));
//  Writeln('}');
end;

procedure TIPFWriter.StartListing(Frames: Boolean; const name: String);
begin
  Writeln(':xmp.')
end;

procedure TIPFWriter.StartListing(Frames : Boolean);
begin
  StartListing(Frames,'');
end;

procedure TIPFWriter.EndListing;
begin
  Writeln(':exmp.')
end;

procedure TIPFWriter.WriteCommentLine;
const
  CommentLine =
    '.* %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%';
begin
  WriteLn('');
  Writeln(CommentLine);
end;

procedure TIPFWriter.WriteComment(Comment : String);
begin
//  Write('.* ');
//  Writeln(Comment);
end;


procedure TIPFWriter.StartSection(SectionName : String; SectionLabel : String);
begin
//  StartSection(SectionName);
  WriteCommentLine;
  WriteComment(SectionName);
  Write(':h2 name=');
  WriteLabel(SectionLabel);
  WriteLn('.'+EscapeTex(SectionName));
end;

//procedure TIPFWriter.StartSection(SectionName : String);
//begin
//end;

procedure TIPFWriter.StartSubSection(SubSectionName : String; SubSectionLabel : String);
begin
  Writeln('');
  WriteComment(SubsectionName);
  Write(':h3 name=');
  WriteLabel(SubsectionLabel);
  WriteLn('.'+{EscapeTex(}SubSectionName{)});
end;

//procedure TIPFWriter.StartSubSection(SubSectionName : String);
//begin
//end;

procedure TIPFWriter.StartChapter(ChapterName : String; ChapterLabel : String);
begin
  StartChapter(ChapterName);
  WriteLabel(ChapterLabel);
end;

procedure TIPFWriter.StartChapter(ChapterName : String);
begin
  Writeln('');
  WriteCommentLine;
  WriteComment(ChapterName);
  WriteCommentLine;
  Writeln(':h1.'+{EscapeTex(}ChapterName{)});
end;



initialization
  // Do not localize.
  RegisterWriter(TIPFWriter,'ipf','IPF output.');
finalization
  UnRegisterWriter('ipf');
end.
