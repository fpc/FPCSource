{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2005 Karoly Balogh

    MUI helper functions for MorphOS/PowerPC

    Based on work of Nils Sjoholm member of the Amiga RTL
    development team.

    MorphOS port was done on a free Pegasos II/G4 machine
    provided by Genesi S.a.r.l. <www.genesi.lu>

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit muihelper;

interface

uses intuition, mui, doslib, utility;


const
  NoFrame          = MUIV_Frame_None;
  ButtonFrame      = MUIV_Frame_Button;
  ImageButtonFrame = MUIV_Frame_ImageButton;
  TextFrame        = MUIV_Frame_Text;
  StringFrame      = MUIV_Frame_String;
  ReadListFrame    = MUIV_Frame_ReadList;
  InputListFrame   = MUIV_Frame_InputList;
  PropFrame        = MUIV_Frame_Prop;
  SliderFrame      = MUIV_Frame_Slider;
  GaugeFrame       = MUIV_Frame_Gauge;
  VirtualFrame     = MUIV_Frame_Virtual;
  GroupFrame       = MUIV_Frame_Group;

const
  Child          = MUIA_Group_Child;
  SubWindow      = MUIA_Application_Window;
  WindowContents = MUIA_Window_RootObject;


// Creates a MUI application
function MH_Application(tags: array of LongWord): pObject_;
function MH_Application(var app; tags: array of LongWord): pObject_;

// Creates a MUI window
function MH_Window(tags: array of LongWord): pObject_;
function MH_Window(var win; tags: array of LongWord): pObject_;

// Creates a MUI button
function MH_MakeButton(blabel: pchar): pObject_;
function MH_MakeButton(var button; blabel: pchar): pObject_;

// Creates a MUI HBar
function MH_MakeHBar(space: longword): pObject_;
function MH_MakeHBar(var hbar; space: longword): pObject_;

// Creates MUI V/HGroup
function MH_VGroup(tags: array of LongWord): pObject_;
function MH_VGroup(frame: longword; tags: array of LongWord): pObject_;
function MH_VGroup(title: PChar; tags: array of LongWord): pObject_;
function MH_HGroup(tags: array of LongWord): pObject_;
function MH_HGroup(frame: longword; tags: array of LongWord): pObject_;
function MH_HGroup(title: PChar; tags: array of LongWord): pObject_;

// Creates MUI Col/RowGroup
function MH_ColGroup(cols: longword; tags: array of LongWord): pObject_;
function MH_ColGroup(cols: longword; frame: longword; tags: array of LongWord): pObject_;
function MH_ColGroup(cols: longword; title: PChar; tags: array of LongWord): pObject_;
function MH_RowGroup(rows: longword; tags: array of LongWord): pObject_;
function MH_RowGroup(rows: longword; frame: longword; tags: array of LongWord): pObject_;
function MH_RowGroup(rows: longword; title: PChar; tags: array of LongWord): pObject_;


// Creates a MUI Text area
function MH_Text(contents: PChar): pObject_;
function MH_Text(contents: PChar; tags: array of LongWord): pObject_;
function MH_Text(var text_; contents: PChar): pObject_;
function MH_Text(var text_; contents: PChar; tags: array of LongWord): pObject_;


implementation


// Creates a MUI application
// ************************************************************************
function MH_Application(tags: array of LongWord): pObject_;
begin
  MH_Application:=MUI_NewObject(MUIC_Application, tags);
end;

function MH_Application(var app; tags: array of LongWord): pObject_;
begin
  pObject_(app):=MUI_NewObject(MUIC_Application, tags);
  MH_Application:=pObject_(app);
end;


// Creates a MUI window
// ************************************************************************
function MH_Window(tags: array of LongWord): pObject_;
begin
  MH_Window:=MUI_NewObject(MUIC_Window, tags);
end;

function MH_Window(var win; tags: array of LongWord): pObject_;
begin
  pObject_(win):=MUI_NewObject(MUIC_Window, tags);
  MH_Window:=pObject_(win);
end;


// Creates a MUI button
// ************************************************************************
function MH_MakeButton(blabel: pchar): pObject_;
begin
 MH_MakeButton:=MUI_MakeObject(MUIO_Button, [DWord(blabel)]);
end;

function MH_MakeButton(var button; blabel: pchar): pObject_;
begin
 pObject_(button):=MUI_MakeObject(MUIO_Button, [DWord(blabel)]);
 MH_MakeButton:=pObject_(button);
end;


// Creates a MUI HBar
// ************************************************************************
function MH_MakeHBar(space: longword): pObject_;
begin
 MH_MakeHBar:=MUI_MakeObject(MUIO_HBar, [space]);
end;

function MH_MakeHBar(var hbar; space: longword): pObject_;
begin
 pObject_(hbar):=MUI_MakeObject(MUIO_HBar, [space]);
 MH_MakeHBar:=pObject_(hbar);
end;


// Creates a MUI VGroup
// ************************************************************************
function MH_VGroup(tags: array of LongWord): pObject_;
begin
  MH_VGroup:=MUI_NewObject(MUIC_Group, tags);
end;

function MH_VGroup(frame: longword; tags: array of LongWord): pObject_;
begin
  MH_VGroup:=MUI_NewObject(MUIC_Group, [ MUIA_Frame, frame, TAG_MORE, DWord(@tags) ] );
end;

function MH_VGroup(title: PChar; tags: array of LongWord): pObject_;
begin
  MH_VGroup:=MUI_NewObject(MUIC_Group, [ MUIA_Frame,      MUIV_Frame_Group, 
                                         MUIA_FrameTitle, longword(title), 
                                         MUIA_Background, MUII_GroupBack,
                                         TAG_MORE, DWord(@tags) ]);
end;


// Creates a MUI HGroup
// ************************************************************************
function MH_HGroup(tags: array of LongWord): pObject_;
begin
  MH_HGroup:=MUI_NewObject(MUIC_Group, [ MUIA_Group_Horiz, MUI_TRUE, TAG_MORE, DWord(@tags) ]);
end;

function MH_HGroup(frame: longword; tags: array of LongWord): pObject_;
begin
  MH_HGroup:=MUI_NewObject(MUIC_Group, [ MUIA_Group_Horiz, MUI_TRUE, 
                                         MUIA_Frame,       frame, 
                                         TAG_MORE, DWord(@tags) ] );
end;

function MH_HGroup(title: PChar; tags: array of LongWord): pObject_;
begin
  MH_HGroup:=MUI_NewObject(MUIC_Group, [ MUIA_Group_Horiz, MUI_TRUE,
                                         MUIA_Frame,       MUIV_Frame_Group, 
                                         MUIA_FrameTitle,  longword(title), 
                                         MUIA_Background,  MUII_GroupBack,
                                         TAG_MORE, DWord(@tags) ]);
end;


// Creates MUI ColGroup
// ************************************************************************
function MH_ColGroup(cols: longword; tags: array of LongWord): pObject_;
begin
  MH_ColGroup:=MUI_NewObject(MUIC_Group, [ MUIA_Group_Columns, cols, TAG_MORE, DWord(@tags) ]);
end;

function MH_ColGroup(cols: longword; frame: longword; tags: array of LongWord): pObject_;
begin
  MH_ColGroup:=MUI_NewObject(MUIC_Group, [ MUIA_Group_Columns, cols, 
                                           MUIA_Frame,         frame, 
                                           TAG_MORE, DWord(@tags) ]);
end;

function MH_ColGroup(cols: longword; title: PChar; tags: array of LongWord): pObject_;
begin
  MH_ColGroup:=MUI_NewObject(MUIC_Group, [ MUIA_Group_Columns, cols,
                                           MUIA_Frame,         MUIV_Frame_Group, 
                                           MUIA_FrameTitle,    longword(title), 
                                           MUIA_Background,    MUII_GroupBack,
                                           TAG_MORE, DWord(@tags) ]);
end;


// Creates MUI RowGroup
// ************************************************************************
function MH_RowGroup(rows: longword; tags: array of LongWord): pObject_;
begin
  MH_RowGroup:=MUI_NewObject(MUIC_Group, [ MUIA_Group_Rows, rows, TAG_MORE, DWord(@tags) ]);
end;

function MH_RowGroup(rows: longword; frame: longword; tags: array of LongWord): pObject_;
begin
  MH_RowGroup:=MUI_NewObject(MUIC_Group, [ MUIA_Group_Rows, rows, 
                                           MUIA_Frame,      frame, 
                                           TAG_MORE, DWord(@tags) ]);
end;

function MH_RowGroup(rows: longword; title: PChar; tags: array of LongWord): pObject_;
begin
  MH_RowGroup:=MUI_NewObject(MUIC_Group, [ MUIA_Group_Rows, rows,
                                           MUIA_Frame,      MUIV_Frame_Group, 
                                           MUIA_FrameTitle, longword(title), 
                                           MUIA_Background, MUII_GroupBack,
                                           TAG_MORE, DWord(@tags) ]);
end;


// Creates a MUI text area
// ************************************************************************
function MH_Text(contents: PChar): pObject_;
begin
 MH_Text:=MUI_NewObject(MUIC_Text,[ MUIA_Text_Contents, DWord(contents), TAG_DONE ]);
end;

function MH_Text(contents: PChar; tags: array of LongWord): pObject_;
begin
 MH_Text:=MUI_NewObject(MUIC_Text,[ MUIA_Text_Contents, DWord(contents), 
                                       TAG_MORE, DWord(@tags) ]);
end;

function MH_Text(var text_; contents: PChar): pObject_;
begin
 pObject_(text_):=MUI_NewObject(MUIC_Text,[ MUIA_Text_Contents, DWord(contents), TAG_DONE ]);
 MH_Text:=pObject_(text_);
end;

function MH_Text(var text_; contents: PChar; tags: array of LongWord): pObject_;
begin
 pObject_(text_):=MUI_NewObject(MUIC_Text,[ MUIA_Text_Contents, DWord(contents), 
                                      TAG_MORE, DWord(@tags) ]);
 MH_Text:=pObject_(text_);
end;


end.
