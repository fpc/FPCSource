{
    $Id$
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit whtml;

{$ifdef fpc}
{$mode objfpc}
{$endif}

interface

uses wformat,Classes,SysUtils;

Type
  THTMLWriter=Class(TFormattingWriter)
  Public
    Constructor Create (AStream : TStream); override;
    Procedure TagStart(Const Name, Attrs : String);
    Procedure TagEnd(Const Name : String);
    Function EscapeText (AText : String) : String; override;
    Procedure DocumentStart(Const Title : String); override;
    Procedure DocumentEnd; override;
    Procedure HeaderStart(Alevel : Integer); override;
    Procedure HeaderEnd(Alevel : Integer); override;
    Procedure ParagraphStart; override;
    Procedure ParagraphEnd; override;
    Procedure LineBreak; override;
    Procedure Rule; override;
    Procedure BoldStart; override;
    Procedure BoldEnd;override;
    Procedure ItalicStart;override;
    Procedure ItalicEnd;override;
    Procedure UnderlineStart;override;
    Procedure UnderlineEnd;override;
    Procedure PreformatStart; override;
    Procedure PreformatEnd; override;
    Procedure TableStart( NoCols: Integer; Border : Boolean); override;
    Procedure TableEnd; override;
    Procedure RowStart; override;
    Procedure RowEnd; override;
    Procedure CellStart; override;
    Procedure CellEnd; override;
    Procedure HeaderCellStart; override;
    Procedure HeaderCellEnd; override;
    Procedure ListStart(ListType : TListType); override;
    Procedure ListEnd(ListType : TListType); override;
    Procedure ListItemStart; override;
    Procedure ListItemEnd; override;
    Procedure DefinitionItem(Const Aname,AText : String); override;
  end;

Const
  ListTags : Array[TListType] of string[2] = ('OL','UL','DL');

implementation

{ THTMLWriter }

procedure THTMLWriter.BoldEnd;
begin
  TagEnd('B');
end;

procedure THTMLWriter.BoldStart;
begin
  TagStart('B','');
end;

procedure THTMLWriter.CellEnd;
begin
  TagEnd('TD');
end;

procedure THTMLWriter.CellStart;
begin
  TagStart('TD','');
end;

constructor THTMLWriter.Create(AStream: TStream);
begin
  inherited;
end;

procedure THTMLWriter.DefinitionItem(const Aname, AText: String);
begin
  TagStart('DT','');
  Write(Aname);
  TagEnd('DT');
  TagStart('DD','');
  Write(AText);
  TagEnd('DD');
end;

procedure THTMLWriter.DocumentEnd;
begin
  TagEnd('BODY');
  TagEnd('HTML');
end;

procedure THTMLWriter.DocumentStart(const Title: String);
begin
  inherited;
  TagStart('HTML','');
  TagStart('TITLE','');
  Write(Title);
  TagEnd('TITLE');
  TagStart('BODY','');
end;

function THTMLWriter.EscapeText(AText: String): String;
begin
  // replace by a more sensitive method.
  Result:=StringReplace(AText,'&','&amp',[rfReplaceAll]);
  Result:=StringReplace(Result,'<','&lt',[rfReplaceAll]);
  Result:=StringReplace(Result,'>','&gt',[rfReplaceAll]);
  Result:=StringReplace(Result,#10,'<BR>',[rfreplaceAll]);
end;

procedure THTMLWriter.HeaderCellEnd;
begin
  TagEnd('TH');
end;

procedure THTMLWriter.HeaderCellStart;
begin
  TagStart('TH','');
end;

procedure THTMLWriter.HeaderEnd(Alevel: Integer);
begin
  TagEnd(Format('H%d',[ALevel]));
end;

procedure THTMLWriter.HeaderStart(Alevel: Integer);
begin
  TagStart(Format('H%d',[ALevel]),'');
end;

procedure THTMLWriter.ItalicEnd;
begin
  TagEnd('I');
end;

procedure THTMLWriter.ItalicStart;
begin
  TagStart('I','');
end;

procedure THTMLWriter.LineBreak;
begin
  TagStart('BR','');
end;

procedure THTMLWriter.ListEnd(ListType: TListType);
begin
  TagEnd(ListTags[ListType]);
end;


procedure THTMLWriter.ListItemEnd;
begin
  TagEnd('LI');

end;

procedure THTMLWriter.ListItemStart;
begin
  TagStart('LI','');
end;

procedure THTMLWriter.ListStart(ListType: TListType);
begin
  TagEnd(ListTags[ListType]);
end;

procedure THTMLWriter.ParagraphEnd;
begin
  TagEnd('P')
end;

procedure THTMLWriter.ParagraphStart;
begin
  TagStart('P','')
end;

procedure THTMLWriter.PreformatEnd;
begin
  TagEnd('PRE')
end;

procedure THTMLWriter.PreformatStart;
begin
  TagStart('PRE','');
end;

procedure THTMLWriter.RowEnd;
begin
  TagEnd('TR')
end;

procedure THTMLWriter.RowStart;
begin
  TagStart('TR','')
end;

procedure THTMLWriter.Rule;
begin
  TagStart('HR','');
end;

procedure THTMLWriter.TableStart(NoCols: Integer; Border: Boolean);

Var
  Attr : string;
begin
  if Border then
    Attr:='BORDER=1'
  else
    Attr:='';
  TagStart('TABLE',Attr);
end;

procedure THTMLWriter.TableEnd;

begin
  TagEnd('TABLE');
end;

procedure THTMLWriter.TagEnd(const Name : String);
begin
  Dump('</'+Name+'>');
end;

procedure THTMLWriter.TagStart(const Name, Attrs: String);
begin
  Dump('<'+Name);
  If Attrs<>'' then
    begin
    Dump(' ');
    Dump(Attrs);
    end;
  Dump('>');
end;

procedure THTMLWriter.UnderlineEnd;
begin
  TagEnd('U');
end;

procedure THTMLWriter.UnderlineStart;
begin
  TagStart('U','');
end;

end.
{
  $Log$
  Revision 1.1  2003-10-01 20:49:29  michael
  + Initial implementation

}
