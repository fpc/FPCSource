{
  $Id$

  GTK (demo) implementation for shedit
  Copyright (C) 1999  Sebastian Guenther (sguenther@gmx.de)

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
}

{$MODE objfpc}
{$H+}

program GTKDemo;
uses
  SysUtils, Classes,
  Doc_text, shedit, sh_pas, sh_xml,
  GDK, GTK, GtkSHEdit;

type
  TGtkSHTextEdit = class(TGtkSHEdit)
  public
    constructor Create(ADoc: TTextDoc);
  end;

  TGtkSHPasEdit = class(TGtkSHEdit)
  public
    constructor Create(ADoc: TTextDoc);
  end;

  TGtkSHXMLEdit = class(TGtkSHEdit)
  public
    constructor Create(ADoc: TTextDoc);
  end;

constructor TGtkSHTextEdit.Create(ADoc: TTextDoc);
var
  e: TSHTextEdit;
begin
  inherited Create;
  e := TSHTextEdit.Create(ADoc, Self);
  SetEdit(e);
end;

constructor TGtkSHPasEdit.Create(ADoc: TTextDoc);
var
  e: TSHPasEdit;
begin
  inherited Create;
  e := TSHPasEdit.Create(ADoc, Self);
  SetEdit(e);

  e.shSymbol     := AddSHStyle('Symbol',        colBrown,       colDefault, fsNormal);
  e.shKeyword    := AddSHStyle('Keyword',       colBlack,       colDefault, fsBold);
  e.shComment    := AddSHStyle('Comment',       colDarkCyan,    colDefault, fsItalics);
  e.shDirective  := AddSHStyle('Directive',     colRed,         colDefault, fsItalics);
  e.shNumbers    := AddSHStyle('Numbers',       colDarkMagenta, colDefault, fsNormal);
  e.shCharacters := AddSHStyle('Characters',    colDarkBlue,    colDefault, fsNormal);
  e.shStrings    := AddSHStyle('Strings',       colBlue,        colDefault, fsNormal);
  e.shAssembler  := AddSHStyle('Assembler',     colDarkGreen,   colDefault, fsNormal);
end;

constructor TGtkSHXMLEdit.Create(ADoc: TTextDoc);
var
  e: TSHXMLEdit;
begin
  inherited Create;
  e := TSHXMLEdit.Create(ADoc, Self);
  SetEdit(e);

  e.shTag        := AddSHStyle('Tag',           colBlack,       colDefault, fsBold);
  e.shTagName    := AddSHStyle('Tag Name',      colBlack,       colDefault, fsBold);
  e.shDefTagName := AddSHStyle('Definition Tag Name', colDarkGreen, colDefault, fsBold);
  e.shArgName    := AddSHStyle('Argument Name', colBrown,       colDefault, fsNormal);
  e.shString     := AddSHStyle('String',        colBlue,        colDefault, fsNormal);
  e.shReference  := AddSHStyle('Reference',     colDarkMagenta, colDefault, fsNormal);
  e.shInvalid    := AddSHStyle('Invalid',       colRed,         colDefault, fsNormal);
  e.shComment    := AddSHStyle('Comment',       colDarkCyan,    colDefault, fsItalics);
  e.shCDATA      := AddSHStyle('CDATA',         colDarkGreen,   colDefault, fsNormal);
end;


var
  MainWindow, Notebook: PGtkWidget;
  Pages: array[0..2] of TGtkSHEdit;
  PasDoc, XMLDoc: TTextDoc;

procedure OnMainWindowDestroyed; cdecl;
begin
  gtk_main_quit;
end;

begin

  gtk_init(@argc, @argv);

  // Create main window
  MainWindow := gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_widget_set_usize(MainWindow, 600, 400);
  gtk_window_set_title(PGtkWindow(MainWindow), 'FPC SHEdit GTK Demo');
  gtk_signal_connect(PGtkObject(MainWindow), 'destroy', GTK_SIGNAL_FUNC(@OnMainWindowDestroyed), nil);

  // Set up documents
  PasDoc := TTextDoc.Create;
  PasDoc.LoadFromFile('gtkdemo.pp');
  XMLDoc := TTextDoc.Create;
  XMLDoc.LoadFromFile('gtkdemo.pp');

  // Create notebook pages (editor widgets)
  Pages[0] := TGtkSHPasEdit.Create(PasDoc);
  Pages[1] := TGtkSHXMLEdit.Create(XMLDoc);
  Pages[2] := TGtkSHTextEdit.Create(PasDoc);

  // Create notebook
  Notebook := gtk_notebook_new;
  gtk_notebook_append_page(PGtkNotebook(Notebook), Pages[0].Widget, gtk_label_new('Pascal'));
  gtk_notebook_append_page(PGtkNotebook(Notebook), Pages[1].Widget, gtk_label_new('XML'));
  gtk_notebook_append_page(PGtkNotebook(Notebook), Pages[2].Widget, gtk_label_new('Text'));
  gtk_container_add(PGtkContainer(MainWindow), Notebook);
  gtk_widget_show(Notebook);
  gtk_widget_show(MainWindow);
  Pages[0].SetFocus;
  gtk_main;
end.
{
  $Log$
  Revision 1.6  1999-12-22 22:28:08  peter
    * updates for cursor setting
    * button press event works

  Revision 1.5  1999/12/08 01:03:15  peter
    * changes so redrawing and walking with the cursor finally works
      correct

  Revision 1.4  1999/11/15 21:47:36  peter
    * first working keypress things

  Revision 1.3  1999/11/14 21:32:55  peter
    * fixes to get it working without crashes

}
