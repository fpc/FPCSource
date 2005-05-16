{
    $Id: dw_lintmpl.pp,v 1.2 2005/02/14 17:13:39 peter Exp $

    FPDoc  -  Free Pascal Documentation Tool
    Copyright (C) 2000 - 2003 by
    Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    * Linear output generator template

    See the file COPYING, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{
  This file implements an almost empty template for generating linear documentation.

  Usage: change the constants below. Do a search&replace where TTemplateWriter
  is changed to TMyFormatWriter (replace MyFormat with whatever you need)
  and fill in all methods.

  If your format is  some hyperlinked format, split in several output files,
  you should take the dw_template.pp template instead.

}

{$mode objfpc}
{$H+}
unit dw_lintmpl;

interface

uses DOM, dGlobals, PasTree;

const
  { Change this into the name of your writer}
  TemplateName = 'template';
  { Comprehensible description goes here:}
  STemplateUsageWriterDescr = 'Writes output in template format';
  { Extension for the template }
  TTemplateExtension = '.tpl';


implementation

uses SysUtils, Classes, dwLinear, dwriter;


Type
 { TTemplateWriter }
  TTemplateWriter = class(TLinearWriter)
  protected
    FLink: String;
    FTableCount : Integer;
    FInVerbatim : Boolean;
    Inlist,
    TableRowStartFlag,
    TableCaptionWritten: Boolean;
    // Linear documentation methods overrides;
    procedure WriteLabel(Const S : String); override;
    procedure WriteIndex(Const S : String); override;
    Procedure WriteExampleFile(FN : String); override;
    Procedure StartProcedure; override;
    Procedure EndProcedure; override;
    Procedure StartProperty; override;
    Procedure EndProperty; override;
    Procedure StartSynopsis; override;
    Procedure StartDeclaration; override;
    Procedure StartVisibility; override;
    Procedure StartDescription; override;
    Procedure StartAccess; override;
    Procedure StartErrors; override;
    Procedure StartSeealso; override;
    Procedure EndSeealso; override;
    procedure StartUnitOverview(AModuleName,AModuleLabel : String);override;
    procedure WriteUnitEntry(UnitRef : TPasType); override;
    Procedure EndUnitOverview; override;
    function  GetLabel(AElement: TPasElement): String; override;
    procedure StartListing(Frames: Boolean; const name: String); override;
    procedure EndListing; override;
    Function  EscapeText(S : String) : String; override;
    Function  StripText(S : String) : String; override;
    procedure WriteCommentLine; override;
    procedure WriteComment(Comment : String);override;
    procedure StartSection(SectionName : String);override;
    procedure StartSubSection(SubSectionName : String);override;
    procedure StartSubSubSection(SubSubSectionName : String);override;
    procedure StartChapter(ChapterName : String); override;
    procedure StartOverview(WithAccess : Boolean); override;
    procedure EndOverview; override;
    procedure WriteOverviewMember(ALabel,AName,Access,ADescr : String); override;
    procedure WriteOverviewMember(ALabel,AName,ADescr : String); override;
    Class Function FileNameExtension : String; override;
    // Description node conversion. Overrides for TFPDocWriter.
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
    // TFPDocWriter class methods
    Function InterPretOption(Const Cmd,Arg : String) : boolean; override;
    Class procedure Usage(List: TStrings); override;
  end;

{ TFPDocWriter overrides }


procedure TTemplateWriter.DescrBeginBold;
begin
  { Start bold output }
end;

procedure TTemplateWriter.DescrEndBold;
begin
  { End bold output }
end;

procedure TTemplateWriter.DescrBeginItalic;
begin
  { Start italic output }
end;

procedure TTemplateWriter.DescrEndItalic;
begin
  { End italic output }
end;

procedure TTemplateWriter.DescrBeginEmph;
begin
  { Start emphasized output }
end;

procedure TTemplateWriter.DescrEndEmph;
begin
  { End emphasized output }
end;

procedure TTemplateWriter.DescrWriteFileEl(const AText: DOMString);
begin
  { format as file name }
end;

procedure TTemplateWriter.DescrWriteKeywordEl(const AText: DOMString);
begin
  { Format as keyword }
end;

procedure TTemplateWriter.DescrWriteVarEl(const AText: DOMString);
begin
  { Format as variable }
end;

procedure TTemplateWriter.DescrBeginLink(const AId: DOMString);
begin
  { Start link to label ID - links are never nested.}
end;

procedure TTemplateWriter.DescrEndLink;
begin
  { End link to label ID}
end;

procedure TTemplateWriter.DescrWriteLinebreak;
begin
  { Start a new line. }
end;

procedure TTemplateWriter.DescrBeginParagraph;
begin
  { Start a new paragraph }
end;

procedure TTemplateWriter.DescrEndParagraph;
begin
  { End current paragraph }
end;

procedure TTemplateWriter.DescrBeginCode(HasBorder: Boolean;
  const AHighlighterName: String);
begin
  { Start block of code }
end;

procedure TTemplateWriter.DescrWriteCodeLine(const ALine: String);
begin
  { Write line of code }
end;

procedure TTemplateWriter.DescrEndCode;
begin
  { End block of code }
end;

procedure TTemplateWriter.DescrBeginOrderedList;
begin
  {  Start numbered list }
end;

procedure TTemplateWriter.DescrEndOrderedList;
begin
  {  End numbered list }
end;

procedure TTemplateWriter.DescrBeginUnorderedList;
begin
  {  Start bulleted list }
end;

procedure TTemplateWriter.DescrEndUnorderedList;
begin
  {  End bulleted list }
end;

procedure TTemplateWriter.DescrBeginDefinitionList;
begin
  {  Start definition list }
end;

procedure TTemplateWriter.DescrEndDefinitionList;
begin
  {  End definition list }
end;

procedure TTemplateWriter.DescrBeginListItem;
begin
  {  Start list item (both bulleted/numbered) }
end;

procedure TTemplateWriter.DescrEndListItem;
begin
  {  End list item (both bulleted/numbered) }
end;

procedure TTemplateWriter.DescrBeginDefinitionTerm;
begin
  {  Start definition term }
end;

procedure TTemplateWriter.DescrEndDefinitionTerm;
begin
  {  End definition term }
end;

procedure TTemplateWriter.DescrBeginDefinitionEntry;
begin
  {  start definition explanation }
end;

procedure TTemplateWriter.DescrEndDefinitionEntry;
begin
  {  End definition explanation }
end;

procedure TTemplateWriter.DescrBeginSectionTitle;
begin
  {  Start section title }
end;

procedure TTemplateWriter.DescrBeginSectionBody;
begin
  {  Start section body }
end;

procedure TTemplateWriter.DescrEndSection;
begin
  {  End section body }
end;

procedure TTemplateWriter.DescrBeginRemark;
begin
  {  Start remark paragraph }
end;

procedure TTemplateWriter.DescrEndRemark;
begin
  {  End remark paragraph }
end;

procedure TTemplateWriter.DescrBeginTable(ColCount: Integer; HasBorder: Boolean);

begin
  {  Start table with ColCount columns, and with border }
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

{ TLinearWriter overrides}

{ Treat backend-specific options }
Function TTemplateWriter.InterPretOption(Const Cmd,Arg : String) : boolean;

begin
  Result:=False;
end;

Class procedure TTemplateWriter.Usage(List: TStrings);

begin
  // Add options to list. Eelement I is option, element i+1 is explanation.
end;


function TTemplateWriter.FileNameExtension: String;
begin
  Result:=TTemplateExtension;
end;


function TTemplateWriter.GetLabel(AElement: TPasElement): String;

begin
end;


Function TTemplateWriter.EscapeText(S : String) : String;

begin
  Result:=S;
end;

Function TTemplateWriter.StripText(S : String) : String;

begin
  Result:=S;
end;


procedure TTemplateWriter.WriteLabel(const s: String);
begin
end;

procedure TTemplateWriter.WriteIndex(const s : String);
begin
end;

procedure TTemplateWriter.StartListing(Frames: Boolean; const name: String);
begin
end;

procedure TTemplateWriter.EndListing;
begin
end;

procedure TTemplateWriter.WriteCommentLine;

begin
end;

procedure TTemplateWriter.WriteComment(Comment : String);
begin
end;

procedure TTemplateWriter.StartChapter(ChapterName : String);
begin
end;

procedure TTemplateWriter.StartSection(SectionName : String);
begin
end;

procedure TTemplateWriter.StartSubSection(SubSectionName : String);
begin
end;

procedure TTemplateWriter.StartSubSubSection(SubSubSectionName : String);
begin
end;

Procedure TTemplateWriter.StartProcedure;

begin
end;

Procedure TTemplateWriter.StartSynopsis;

begin
end;

Procedure TTemplateWriter.StartDeclaration;

begin
end;

Procedure TTemplateWriter.StartVisibility;

begin
end;

Procedure TTemplateWriter.StartDescription;

begin
end;

Procedure TTemplateWriter.StartErrors;

begin
end;

Procedure TTemplateWriter.StartAccess;

begin
end;

Procedure TTemplateWriter.EndProcedure;

begin
end;
Procedure TTemplateWriter.StartProperty;

begin
end;

Procedure TTemplateWriter.EndProperty;

begin
end;

procedure TTemplateWriter.WriteExampleFile(FN : String);

begin
end;

procedure TTemplateWriter.StartOverview(WithAccess : Boolean);

begin
{
  If With access then it is a property overview.
  Otherwise it is a method/function overview.
  If tabular output is generated, the preferred output order is:
  With access:
  Col 1 : Page reference
  Col 2 : Property Name
  Col 3 : Accessibility (r/w)
  Col 4 : Description
  Without access:
  Col 1 : Page reference
  Col 2 : Method name
  Col 3 : Description
  (See the two WriteOverviewMember functions)
}
end;

procedure TTemplateWriter.EndOverview;

begin
  { End of overview }
end;

procedure TTemplateWriter.WriteOverviewMember(ALabel,AName,Access,ADescr : String);

begin
  { Write one entry in property overview:
  ALabel : Label, as returned by GetLabel
  AName  : Property name
  Access : Property acces (r/w/a)
  Descr  : Description
  }
end;

procedure TTemplateWriter.WriteOverviewMember(ALabel,AName,ADescr : String);

begin
  { Write one entry in method overview:
  ALabel : Label, as returned by GetLabel
  AName  : Method name
  Descr  : Description
  }
end;


Procedure TTemplateWriter.StartSeeAlso;

begin
end;

procedure TTemplateWriter.EndSeealso;
begin
end;

procedure TTemplateWriter.StartUnitOverview(AModuleName,AModuleLabel : String);

begin
  { Start of unit overview.
    AModuleName : Name of current unit.
    AModuleLabel : Label name of current unit.
  }
end;

procedure TTemplateWriter.WriteUnitEntry(UnitRef : TPasType);

begin
  { Write one unit entry }
end;

procedure TTemplateWriter.EndUnitOverview;

begin
  { end of unit overview }
end;


initialization
  // Do not localize templatename
  RegisterWriter(TTemplateWriter,TemplateName,STemplateUsageWriterDescr);
finalization
  UnRegisterWriter(TemplateName);
end.


{
  $Log: dw_lintmpl.pp,v $
  Revision 1.2  2005/02/14 17:13:39  peter
    * truncate log

  Revision 1.1  2005/01/15 20:46:34  michael
    + Initial implementation of linear template.

  Revision 1.9  2005/01/12 21:11:41  michael
  + New structure for writers. Implemented TXT writer

  Revision 1.8  2005/01/09 15:59:50  michael
  + Split out latex writer to linear and latex writer

  Revision 1.7  2004/11/15 18:01:16  michael
  + Example fixes, and more escape seqences

  Revision 1.6  2004/07/23 23:39:48  michael
  + Some fixes in verbatim writing

  Revision 1.5  2004/06/06 10:53:02  michael
  + Added Topic support

  Revision 1.4  2003/03/18 19:28:44  michael
  + Some changes to output handling, more suitable for tex output

  Revision 1.3  2003/03/18 19:12:29  michael
  + More EscapeText calls needed

  Revision 1.2  2003/03/18 01:11:51  michael
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
