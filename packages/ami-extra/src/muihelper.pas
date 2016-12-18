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

uses intuition, mui, amigados, utility;


const
  MUI_TRUE  = 1;
  MUI_FALSE = 0;

  // Frame Types
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

// Creates a MUI menu strip
function MH_Menustrip(const Tags: array of PtrUInt): PObject_;
function MH_Menustrip(var MenuStrip; const Tags: array of PtrUInt): PObject_;
// Creates a MUI menu
function MH_Menu(const Tags: array of PtrUInt): PObject_;
function MH_Menu(var Menu; const Tags: array of PtrUInt): PObject_;
function MH_Menu(Name: PChar; const Tags: array of PtrUInt): PObject_;
function MH_Menu(var Menu; Name: PChar; const Tags: array of PtrUInt): PObject_;
// Creates a MUI menuitem
function MH_Menuitem(const Tags: array of PtrUInt): PObject_;
function MH_Menuitem(var Menuitem; const Tags: array of PtrUInt): PObject_;
// Creates a MUI window
function MH_Window(const Tags: array of PtrUInt): PObject_;
function MH_Window(var Win; const Tags: array of PtrUInt): PObject_;
// Creates a MUI image
function MH_Image(const Tags: array of PtrUInt): PObject_;
function MH_Image(var Image; const Tags: array of PtrUInt): PObject_;
// Creates a MUI bitmap
function MH_Bitmap(const Tags: array of PtrUInt): PObject_;
function MH_Bitmap(var Bitmap; const Tags: array of PtrUInt): PObject_;
// Creates a MUI BodyChunk
function MH_BodyChunk(const Tags: array of PtrUInt): PObject_;
function MH_BodyChunk(var BodyChunk; const Tags: array of PtrUInt): PObject_;
// Creates a MUI Notify
function MH_Notify(const Tags: array of PtrUInt): PObject_;
function MH_Notify(var Notify; const Tags: array of PtrUInt): PObject_;
// Creates a MUI application
function MH_Application(const Tags: array of PtrUInt): PObject_;
function MH_Application(var App; const Tags: array of PtrUInt): PObject_;
// Creates a MUI Text area
function MH_Text(Contents: PChar): PObject_;
function MH_Text(Contents: PChar; const Tags: array of PtrUInt): PObject_;
function MH_Text(var Text_; Contents: PChar): PObject_;
function MH_Text(var Text_; Contents: PChar; const Tags: array of PtrUInt): PObject_;
// Creates a MUI Rectangle
function MH_Rectangle(const Tags: array of PtrUInt): PObject_;
function MH_Rectangle(var Rectangle; const Tags: array of PtrUInt): PObject_;
function MH_Rectangle(Frame: LongWord; const Tags: array of PtrUInt): PObject_;
function MH_Rectangle(var Rectangle; Frame: LongWord; const Tags: array of PtrUInt): PObject_;
// Creates a MUI Balance
function MH_Balance(const Tags: array of PtrUInt): PObject_;
function MH_Balance(var Balance; const Tags: array of PtrUInt): PObject_;
// Creates a MUI List
function MH_List(const Tags: array of PtrUInt): PObject_;
function MH_List(var List; const Tags: array of PtrUInt): PObject_;
// Creates a MUI Prop
function MH_Prop(const Tags: array of PtrUInt): PObject_;
function MH_Prop(var Prop; const Tags: array of PtrUInt): PObject_;
// Creates a MUI String
function MH_String(const Tags: array of PtrUInt): PObject_;
function MH_String(var NString; const Tags: array of PtrUInt): PObject_;
// Creates a MUI Scrollbar
function MH_Scrollbar(const Tags: array of PtrUInt): PObject_;
function MH_Scrollbar(var Scrollbar; const Tags: array of PtrUInt): PObject_;
// Creates a MUI Listview
function MH_Listview(const Tags: array of PtrUInt): PObject_;
function MH_Listview(var Listview; const Tags: array of PtrUInt): PObject_;
// Creates a MUI Radio
function MH_Radio(const Tags: array of PtrUInt): PObject_;
function MH_Radio(var Radio; const Tags: array of PtrUInt): PObject_;
// Creates a MUI Volumelist
function MH_Volumelist(const Tags: array of PtrUInt): PObject_;
function MH_Volumelist(var Volumelist; const Tags: array of PtrUInt): PObject_;
// Creates a MUI Floattext
function MH_Floattext(const Tags: array of PtrUInt): PObject_;
function MH_Floattext(var Floattext; const Tags: array of PtrUInt): PObject_;
// Creates a MUI Dirlist
function MH_Dirlist(const Tags: array of PtrUInt): PObject_;
function MH_Dirlist(var Dirlist; const Tags: array of PtrUInt): PObject_;
// Creates a MUI Cycle
function MH_Cycle(const Tags: array of PtrUInt): PObject_;
function MH_Cycle(var Cycle; const Tags: array of PtrUInt): PObject_;
// Creates a MUI Gauge
function MH_Gauge(const Tags: array of PtrUInt): PObject_;
function MH_Gauge(var Gauge; const Tags: array of PtrUInt): PObject_;
// Creates a MUI Scale
function MH_Scale(const Tags: array of PtrUInt): PObject_;
function MH_Scale(var Scale; const Tags: array of PtrUInt): PObject_;
// Creates a MUI Numeric
function MH_Numeric(const Tags: array of PtrUInt): PObject_;
function MH_Numeric(var Numeric; const Tags: array of PtrUInt): PObject_;
// Creates a MUI Slider
function MH_Slider(const Tags: array of PtrUInt): PObject_;
function MH_Slider(var Slider; const Tags: array of PtrUInt): PObject_;
// Creates a MUI Numericbutton
function MH_Numericbutton(const Tags: array of PtrUInt): PObject_;
function MH_Numericbutton(var Numericbutton; const Tags: array of PtrUInt): PObject_;
// Creates a MUI Knob
function MH_Knob(const Tags: array of PtrUInt): PObject_;
function MH_Knob(var Knob; const Tags: array of PtrUInt): PObject_;
// Creates a MUI Levelmeter
function MH_Levelmeter(const Tags: array of PtrUInt): PObject_;
function MH_Levelmeter(var Levelmeter; const Tags: array of PtrUInt): PObject_;
// Creates a MUI Boopsimeter
function MH_Boopsi(const Tags: array of PtrUInt): PObject_;
function MH_Boopsi(var Boopsi; const Tags: array of PtrUInt): PObject_;
// Creates a MUI Colorfield
function MH_Colorfield(const Tags: array of PtrUInt): PObject_;
function MH_Colorfield(var Colorfield; const Tags: array of PtrUInt): PObject_;
// Creates a MUI Penadjust
function MH_Penadjust(const Tags: array of PtrUInt): PObject_;
function MH_Penadjust(var Penadjust; const Tags: array of PtrUInt): PObject_;
// Creates a MUI Coloradjust
function MH_Coloradjust(const Tags: array of PtrUInt): PObject_;
function MH_Coloradjust(var Coloradjust; const Tags: array of PtrUInt): PObject_;
// Creates a MUI Palette
function MH_Palette(const Tags: array of PtrUInt): PObject_;
function MH_Palette(var Palette; const Tags: array of PtrUInt): PObject_;
// Creates a MUI Group
function MH_Group(const Tags: array of PtrUInt): PObject_;
function MH_Group(var Group; const Tags: array of PtrUInt): PObject_;
// Creates a MUI Register
function MH_Register(const Tags: array of PtrUInt): PObject_;
function MH_Register(var Register; const Tags: array of PtrUInt): PObject_;
// Creates a MUI Virtgroup
function MH_Virtgroup(const Tags: array of PtrUInt): PObject_;
function MH_Virtgroup(var Virtgroup; const Tags: array of PtrUInt): PObject_;
// Creates a MUI Scrollgroup
function MH_Scrollgroup(const Tags: array of PtrUInt): PObject_;
function MH_Scrollgroup(var Scrollgroup; const Tags: array of PtrUInt): PObject_;
// Creates a MUI Popstring
function MH_Popstring(const Tags: array of PtrUInt): PObject_;
function MH_Popstring(var Popstring; const Tags: array of PtrUInt): PObject_;
// Creates a MUI Popobject
function MH_Popobject(const Tags: array of PtrUInt): PObject_;
function MH_Popobject(var Popobject; const Tags: array of PtrUInt): PObject_;
// Creates a MUI Poplist
function MH_Poplist(const Tags: array of PtrUInt): PObject_;
function MH_Poplist(var Poplist; const Tags: array of PtrUInt): PObject_;
// Creates a MUI Popasl
function MH_Popasl(const Tags: array of PtrUInt): PObject_;
function MH_Popasl(var Popasl; const Tags: array of PtrUInt): PObject_;
// Creates a MUI Pendisplay
function MH_Pendisplay(const Tags: array of PtrUInt): PObject_;
function MH_Pendisplay(var Pendisplay; const Tags: array of PtrUInt): PObject_;
// Creates a MUI Poppen
function MH_Poppen(const Tags: array of PtrUInt): PObject_;
function MH_Poppen(var Poppen; const Tags: array of PtrUInt): PObject_;
// Creates a MUI Aboutmui
function MH_Aboutmui(const Tags: array of PtrUInt): PObject_;
function MH_Aboutmui(var Aboutmui; const Tags: array of PtrUInt): PObject_;
// Creates a MUI Scrmodelist
function MH_Scrmodelist(const Tags: array of PtrUInt): PObject_;
function MH_Scrmodelist(var Scrmodelist; const Tags: array of PtrUInt): PObject_;
// Creates MUI V/HGroup
function MH_VGroup(const Tags: array of PtrUInt): pObject_;
function MH_VGroup(Frame: LongWord; const Tags: array of PtrUInt): PObject_;
function MH_VGroup(Title: PChar; const Tags: array of PtrUInt): PObject_;
function MH_HGroup(const Tags: array of PtrUInt): PObject_;
function MH_HGroup(Frame: LongWord; const Tags: array of PtrUInt): PObject_;
function MH_HGroup(Title: PChar; const Tags: array of PtrUInt): PObject_;
// Creates MUI Col/RowGroup
function MH_ColGroup(Cols: LongWord; const Tags: array of PtrUInt): PObject_;
function MH_ColGroup(Cols: LongWord; Frame: Longword; const Tags: array of PtrUInt): PObject_;
function MH_ColGroup(Cols: LongWord; Title: PChar; const Tags: array of PtrUInt): PObject_;
function MH_RowGroup(Rows: LongWord; const Tags: array of PtrUInt): PObject_;
function MH_RowGroup(Rows: LongWord; Frame: LongWord; const Tags: array of PtrUInt): PObject_;
function MH_RowGroup(Rows: LongWord; Title: PChar; const Tags: array of PtrUInt): PObject_;
// Creates a MUI Pagegroup
function MH_Pagegroup(const Tags: array of PtrUInt): PObject_;
function MH_Pagegroup(var Pagegroup; const Tags: array of PtrUInt): PObject_;
// Creates MUI Virtual V/HGroup
function MH_VGroupV(const Tags: array of PtrUInt): PObject_;
function MH_HGroupV(const Tags: array of PtrUInt): PObject_;
// Creates MUI Virtual ColGroup
function MH_ColGroupV(Cols: LongWord; const Tags: array of PtrUInt): PObject_;
function MH_RowGroupV(Rows: LongWord; const Tags: array of PtrUInt): PObject_;
// Creates a MUI Virtual Pagegroup
function MH_PagegroupV(const Tags: array of PtrUInt): PObject_;
function MH_PagegroupV(var Pagegroup; const Tags: array of PtrUInt): PObject_;
// Creates a MUI RegisterGroup
function MH_RegisterGroup(Titles: PPChar; const Tags: array of PtrUInt): PObject_;
function MH_RegisterGroup(var RegisterGroup; Titles: PPChar; const Tags: array of PtrUInt): PObject_;
//Spacing Macros
function MH_HVSpace: PObject_;
function MH_HSpace(x: LongWord): PObject_;
function MH_VSpace(x: LongWord): PObject_;
function MH_HCenter(Obj: PObject_): PObject_;
function MH_VCenter(Obj: PObject_): PObject_;
// Creates a MUI button
function MH_Button(BLabel: PChar): PObject_;
function MH_Button(var Button; BLabel: PChar): PObject_;
function MH_SimpleButton(BLabel: PChar): PObject_;
function MH_SimpleButton(var Button; BLabel: PChar): PObject_;
// Creates a MUI PopButton
function MH_PopButton(Img: PChar): PObject_;
function MH_PopButton(var PopButton; img: PChar): PObject_;
// Creates a MUI label
function MH_Label(BLabel: PChar): PObject_;
function MH_Label(var OLabel; BLabel: PChar): PObject_;
// Creates a MUI label with single Frame border
function MH_Label1(BLabel: PChar): PObject_;
function MH_Label1(var OLabel; BLabel: PChar): PObject_;
// Creates a MUI label with double Frame border
function MH_Label2(BLabel: PChar): PObject_;
function MH_Label2(var OLabel; BLabel: PChar): PObject_;
// Creates a MUI left aligned label
function MH_LLabel(BLabel: PChar): PObject_;
function MH_LLabel(var OLabel; BLabel: PChar): PObject_;
// Creates a MUI left aligned label with single Frame border
function MH_LLabel1(BLabel: PChar): PObject_;
function MH_LLabel1(var OLabel; BLabel: PChar): PObject_;
// Creates a MUI left aligned label with double Frame border
function MH_LLabel2(BLabel: PChar): PObject_;
function MH_LLabel2(var OLabel; BLabel: PChar): PObject_;
// Creates a MUI centered label
function MH_CLabel(BLabel: PChar): PObject_;
function MH_CLabel(var OLabel; BLabel: PChar): PObject_;
// Creates a MUI centered label with single Frame border
function MH_CLabel1(BLabel: PChar): PObject_;
function MH_CLabel1(var OLabel; BLabel: PChar): PObject_;
// Creates a MUI centered label with double Frame border
function MH_CLabel2(BLabel: PChar): PObject_;
function MH_CLabel2(var OLabel; BLabel: PChar): PObject_;
// Creates a MUI label
function MH_FreeLabel(BLabel: PChar): PObject_;
function MH_FreeLabel(var OLabel; BLabel: PChar): PObject_;
// Creates a MUI label with single Frame border
function MH_FreeLabel1(BLabel: PChar): PObject_;
function MH_FreeLabel1(var OLabel; BLabel: PChar): PObject_;
// Creates a MUI label with double Frame border
function MH_FreeLabel2(BLabel: PChar): PObject_;
function MH_FreeLabel2(var OLabel; BLabel: PChar): PObject_;
// Creates a MUI left aligned label
function MH_FreeLLabel(BLabel: PChar): PObject_;
function MH_FreeLLabel(var OLabel; BLabel: PChar): PObject_;
// Creates a MUI left aligned label with single Frame border
function MH_FreeLLabel1(BLabel: PChar): PObject_;
function MH_FreeLLabel1(var OLabel; BLabel: PChar): PObject_;
// Creates a MUI left aligned label with double Frame border
function MH_FreeLLabel2(BLabel: PChar): PObject_;
function MH_FreeLLabel2(var OLabel; BLabel: PChar): PObject_;
// Creates a MUI centered label
function MH_FreeCLabel(BLabel: PChar): PObject_;
function MH_FreeCLabel(var OLabel; BLabel: PChar): PObject_;
// Creates a MUI centered label with single Frame border
function MH_FreeCLabel1(BLabel: PChar): PObject_;
function MH_FreeCLabel1(var OLabel; BLabel: PChar): PObject_;
// Creates a MUI centered label with double Frame border
function MH_FreeCLabel2(BLabel: PChar): PObject_;
function MH_FreeCLabel2(var OLabel; BLabel: PChar): PObject_;
// Creates a MUI HBar
function MH_HBar(Space: LongWord): PObject_;
function MH_HBar(var HBar; Space: LongWord): PObject_;
// Creates a MUI VBar
function MH_VBar(Space: LongWord): PObject_;
function MH_VBar(var VBar; Space: LongWord): PObject_;

function MAKE_ID(c1, c2, c3, c4: char): LongWord; inline;

procedure MH_Set(Obj: PObject_; Tag, Data: PtrUInt);
function MH_Get(Obj: PObject_; Tag: PtrUInt): PtrUInt;
procedure MH_SetMutex(Obj: PObject_; n: Integer);
procedure MH_SetCycle(Obj: PObject_; n: Integer);
procedure MH_SetString(Obj: PObject_; s: PChar);
procedure MH_SetCheckmark(Obj: PObject_; b: Boolean);
procedure MH_SetSlider(Obj: PObject_; l: LongInt);

implementation

function MAKE_ID(c1, c2, c3, c4: char): LongWord; inline;
begin
  MAKE_ID := (LongWord(Ord(c1)) shl 24) or
             (LongWord(Ord(c2)) shl 16) or
             (LongWord(Ord(c3)) shl 8) or
             (LongWord(Ord(c4)));
end;

procedure MH_Set(Obj: PObject_; Tag, Data: PtrUInt);
begin
  SetAttrs(Obj, [Tag, Data, TAG_END]);
end;

function MH_Get(Obj: PObject_; Tag: PtrUInt): PtrUInt;
begin
  GetAttr(Tag, Obj, MH_Get);
end;

procedure MH_SetMutex(Obj: PObject_; n: Integer);
begin
  MH_Set(Obj, MUIA_Radio_Active, n);
end;

procedure MH_SetCycle(Obj: PObject_; n: Integer);
begin
  MH_Set(Obj, MUIA_Cycle_Active, n);
end;

procedure MH_SetString(Obj: PObject_; s: PChar);
begin
  MH_Set(Obj, MUIA_String_Contents, AsTag(s));
end;

procedure MH_SetCheckmark(Obj: PObject_; b: Boolean);
begin
  if b then
    MH_Set(Obj, MUIA_Selected, MUI_TRUE)
  else
    MH_Set(Obj, MUIA_Selected, MUI_FALSE);
end;

procedure MH_SetSlider(Obj: PObject_; l: LongInt);
begin
  MH_Set(Obj, MUIA_Numeric_Value, AsTag(l));
end;

// Creates a MUI menu strip
// ************************************************************************
function MH_Menustrip(const Tags: array of PtrUInt): PObject_;
begin
  MH_Menustrip := MUI_NewObject(MUIC_Menustrip, Tags);
end;

function MH_Menustrip(var MenuStrip; const Tags: array of PtrUInt): PObject_;
begin
  PObject_(MenuStrip) := MUI_NewObject(MUIC_Menustrip, Tags);
  MH_Menustrip := PObject_(MenuStrip);
end;

// Creates a MUI menu
// ************************************************************************
function MH_Menu(const Tags: array of PtrUInt): PObject_;
begin
  MH_Menu := MUI_NewObject(MUIC_Menu, Tags);
end;

function MH_Menu(var Menu; const Tags: array of PtrUInt): PObject_;
begin
  PObject_(Menu) := MUI_NewObject(MUIC_Menu, Tags);
  MH_Menu := PObject_(Menu);
end;

function MH_Menu(Name: PChar; const Tags: array of PtrUInt): PObject_;
begin
  MH_Menu := MUI_NewObject(MUIC_Menu, [MUIA_Menu_Title, AsTag(Name), TAG_MORE, AsTag(@Tags)]);
end;

function MH_Menu(var Menu; Name: PChar; const Tags: array of PtrUInt): PObject_;
begin
  PObject_(Menu) := MUI_NewObject(MUIC_Menu, [MUIA_Menu_Title, AsTag(Name), TAG_MORE, AsTag(@Tags)]);
  MH_Menu := PObject_(Menu);
end;

// Creates a MUI menuitem
// ************************************************************************
function MH_Menuitem(const Tags: array of PtrUInt): PObject_;
begin
  MH_Menuitem := MUI_NewObject(MUIC_Menuitem, Tags);
end;

function MH_Menuitem(var Menuitem; const Tags: array of PtrUInt): PObject_;
begin
  PObject_(Menuitem) := MUI_NewObject(MUIC_Menuitem, Tags);
  MH_Menuitem := PObject_(Menuitem);
end;

// Creates a MUI window
// ************************************************************************
function MH_Window(const Tags: array of PtrUInt): PObject_;
begin
  MH_Window := MUI_NewObject(MUIC_Window, Tags);
end;

function MH_Window(var Win; const Tags: array of PtrUInt): PObject_;
begin
  PObject_(Win) := MUI_NewObject(MUIC_Window, Tags);
  MH_Window := PObject_(Win);
end;

// Creates a MUI image
// ************************************************************************
function MH_Image(const Tags: array of PtrUInt): PObject_;
begin
  MH_Image := MUI_NewObject(MUIC_Image, Tags);
end;

function MH_Image(var Image; const Tags: array of PtrUInt): PObject_;
begin
  PObject_(Image) := MUI_NewObject(MUIC_Image, Tags);
  MH_Image := PObject_(Image);
end;

// Creates a MUI Bitmap
// ************************************************************************
function MH_Bitmap(const Tags: array of PtrUInt): PObject_;
begin
  MH_Bitmap := MUI_NewObject(MUIC_Bitmap, Tags);
end;

function MH_Bitmap(var Bitmap; const Tags: array of PtrUInt): PObject_;
begin
  PObject_(Bitmap) := MUI_NewObject(MUIC_Bitmap, Tags);
  MH_Bitmap := PObject_(Bitmap);
end;

// Creates a MUI BodyChunk
// ************************************************************************
function MH_BodyChunk(const Tags: array of PtrUInt): PObject_;
begin
  MH_BodyChunk := MUI_NewObject(MUIC_BodyChunk, Tags);
end;

function MH_BodyChunk(var BodyChunk; const Tags: array of PtrUInt): PObject_;
begin
  PObject_(BodyChunk) := MUI_NewObject(MUIC_BodyChunk, Tags);
  MH_BodyChunk := PObject_(BodyChunk);
end;

// Creates a MUI Notify
// ************************************************************************
function MH_Notify(const Tags: array of PtrUInt): PObject_;
begin
  MH_Notify := MUI_NewObject(MUIC_Notify, Tags);
end;

function MH_Notify(var Notify; const Tags: array of PtrUInt): PObject_;
begin
  PObject_(Notify) := MUI_NewObject(MUIC_Notify, Tags);
  MH_Notify := PObject_(Notify);
end;

// Creates a MUI application
// ************************************************************************
function MH_Application(const Tags: array of PtrUInt): PObject_;
begin
  MH_Application := MUI_NewObject(MUIC_Application, Tags);
end;

function MH_Application(var App; const Tags: array of PtrUInt): PObject_;
begin
  PObject_(App) := MUI_NewObject(MUIC_Application, Tags);
  MH_Application := PObject_(App);
end;

// Creates a MUI text area
// ************************************************************************
function MH_Text(Contents: PChar): PObject_;
begin
  MH_Text := MUI_NewObject(MUIC_Text,[MUIA_Text_Contents, AsTag(Contents), TAG_DONE]);
end;

function MH_Text(Contents: PChar; const Tags: array of PtrUInt): PObject_;
begin
  MH_Text := MUI_NewObject(MUIC_Text,[
    MUIA_Text_Contents, AsTag(Contents),
    TAG_MORE,           AsTag(@Tags)]);
end;

function MH_Text(var Text_; Contents: PChar): PObject_;
begin
  PObject_(Text_) := MUI_NewObject(MUIC_Text,[MUIA_Text_Contents, AsTag(Contents), TAG_DONE]);
  MH_Text := PObject_(Text_);
end;

function MH_Text(var Text_; Contents: PChar; const Tags: array of PtrUInt): PObject_;
begin
  PObject_(Text_) := MUI_NewObject(MUIC_Text, [
    MUIA_Text_Contents, AsTag(Contents),
    TAG_MORE,           AsTag(@Tags)]);
  MH_Text := PObject_(Text_);
end;

// Creates a MUI Rectangle
// ************************************************************************
function MH_Rectangle(const Tags: array of PtrUInt): PObject_;
begin
  MH_Rectangle := MUI_NewObject(MUIC_Rectangle, Tags);
end;

function MH_Rectangle(var Rectangle; const Tags: array of PtrUInt): PObject_;
begin
 PObject_(Rectangle) := MUI_NewObject(MUIC_Rectangle, Tags);
 MH_Rectangle := PObject_(Rectangle);
end;

function MH_Rectangle(Frame: LongWord; const Tags: array of PtrUInt): PObject_;
begin
  MH_Rectangle := MUI_NewObject(MUIC_Rectangle, [MUIA_Frame, Frame, TAG_MORE, AsTag(@Tags)]);
end;

function MH_Rectangle(var Rectangle; Frame: LongWord; const Tags: array of PtrUInt): PObject_;
begin
 PObject_(Rectangle) := MUI_NewObject(MUIC_Rectangle, [MUIA_Frame, Frame, TAG_MORE, AsTag(@Tags)]);
 MH_Rectangle := PObject_(Rectangle);
end;

// Creates a MUI Balance
// ************************************************************************
function MH_Balance(const Tags: array of PtrUInt): PObject_;
begin
  MH_Balance := MUI_NewObject(MUIC_Balance, Tags);
end;

function MH_Balance(var Balance; const Tags: array of PtrUInt): PObject_;
begin
 PObject_(Balance) := MUI_NewObject(MUIC_Balance, Tags);
 MH_Balance := PObject_(Balance);
end;

// Creates a MUI List
// ************************************************************************
function MH_List(const Tags: array of PtrUInt): PObject_;
begin
  MH_List := MUI_NewObject(MUIC_List, Tags);
end;

function MH_List(var List; const Tags: array of PtrUInt): PObject_;
begin
 PObject_(List) := MUI_NewObject(MUIC_List, Tags);
 MH_List := PObject_(List);
end;

// Creates a MUI Prop
// ************************************************************************
function MH_Prop(const Tags: array of PtrUInt): PObject_;
begin
  MH_Prop := MUI_NewObject(MUIC_Prop, Tags);
end;

function MH_Prop(var Prop; const Tags: array of PtrUInt): PObject_;
begin
 PObject_(Prop) := MUI_NewObject(MUIC_Prop, Tags);
 MH_Prop := PObject_(Prop);
end;

// Creates a MUI String
// ************************************************************************
function MH_String(const Tags: array of PtrUInt): PObject_;
begin
  MH_String := MUI_NewObject(MUIC_String, Tags);
end;

function MH_String(var NString; const Tags: array of PtrUInt): PObject_;
begin
 PObject_(NString) := MUI_NewObject(MUIC_String, Tags);
 MH_String := PObject_(NString);
end;

// Creates a MUI Scrollbar
// ************************************************************************
function MH_Scrollbar(const Tags: array of PtrUInt): PObject_;
begin
  MH_Scrollbar := MUI_NewObject(MUIC_Scrollbar, Tags);
end;

function MH_Scrollbar(var Scrollbar; const Tags: array of PtrUInt): PObject_;
begin
 PObject_(Scrollbar) := MUI_NewObject(MUIC_Scrollbar, Tags);
 MH_Scrollbar := PObject_(Scrollbar);
end;

// Creates a MUI Listview
// ************************************************************************
function MH_Listview(const Tags: array of PtrUInt): PObject_;
begin
  MH_Listview := MUI_NewObject(MUIC_Listview, Tags);
end;

function MH_Listview(var Listview; const Tags: array of PtrUInt): PObject_;
begin
 PObject_(Listview) := MUI_NewObject(MUIC_Listview, Tags);
 MH_Listview := PObject_(Listview);
end;

// Creates a MUI Radio
// ************************************************************************
function MH_Radio(const Tags: array of PtrUInt): PObject_;
begin
  MH_Radio := MUI_NewObject(MUIC_Radio, Tags);
end;

function MH_Radio(var Radio; const Tags: array of PtrUInt): PObject_;
begin
 PObject_(Radio) := MUI_NewObject(MUIC_Radio, Tags);
 MH_Radio := PObject_(Radio);
end;

// Creates a MUI Volumelist
// ************************************************************************
function MH_Volumelist(const Tags: array of PtrUInt): PObject_;
begin
  MH_Volumelist := MUI_NewObject(MUIC_Volumelist, Tags);
end;

function MH_Volumelist(var Volumelist; const Tags: array of PtrUInt): PObject_;
begin
 PObject_(Volumelist) := MUI_NewObject(MUIC_Volumelist, Tags);
 MH_Volumelist := PObject_(Volumelist);
end;

// Creates a MUI Floattext
// ************************************************************************
function MH_Floattext(const Tags: array of PtrUInt): PObject_;
begin
  MH_Floattext := MUI_NewObject(MUIC_Floattext, Tags);
end;

function MH_Floattext(var Floattext; const Tags: array of PtrUInt): PObject_;
begin
 PObject_(Floattext) := MUI_NewObject(MUIC_Floattext, Tags);
 MH_Floattext := PObject_(Floattext);
end;

// Creates a MUI Dirlist
// ************************************************************************
function MH_Dirlist(const Tags: array of PtrUInt): PObject_;
begin
  MH_Dirlist := MUI_NewObject(MUIC_Dirlist, Tags);
end;

function MH_Dirlist(var Dirlist; const Tags: array of PtrUInt): PObject_;
begin
 PObject_(Dirlist) := MUI_NewObject(MUIC_Dirlist, Tags);
 MH_Dirlist := PObject_(Dirlist);
end;

// Creates a MUI Cycle
// ************************************************************************
function MH_Cycle(const Tags: array of PtrUInt): PObject_;
begin
  MH_Cycle := MUI_NewObject(MUIC_Cycle, Tags);
end;

function MH_Cycle(var Cycle; const Tags: array of PtrUInt): PObject_;
begin
 PObject_(Cycle) := MUI_NewObject(MUIC_Cycle, Tags);
 MH_Cycle := PObject_(Cycle);
end;

// Creates a MUI Gauge
// ************************************************************************
function MH_Gauge(const Tags: array of PtrUInt): PObject_;
begin
  MH_Gauge := MUI_NewObject(MUIC_Gauge, Tags);
end;

function MH_Gauge(var Gauge; const Tags: array of PtrUInt): PObject_;
begin
 PObject_(Gauge) := MUI_NewObject(MUIC_Gauge, Tags);
 MH_Gauge := PObject_(Gauge);
end;

// Creates a MUI Scale
// ************************************************************************
function MH_Scale(const Tags: array of PtrUInt): PObject_;
begin
  MH_Scale := MUI_NewObject(MUIC_Scale, Tags);
end;

function MH_Scale(var Scale; const Tags: array of PtrUInt): PObject_;
begin
 PObject_(Scale) := MUI_NewObject(MUIC_Scale, Tags);
 MH_Scale := PObject_(Scale);
end;

// Creates a MUI Numeric
// ************************************************************************
function MH_Numeric(const Tags: array of PtrUInt): PObject_;
begin
  MH_Numeric := MUI_NewObject(MUIC_Numeric, Tags);
end;

function MH_Numeric(var Numeric; const Tags: array of PtrUInt): PObject_;
begin
 PObject_(Numeric) := MUI_NewObject(MUIC_Numeric, Tags);
 MH_Numeric := PObject_(Numeric);
end;

// Creates a MUI Slider
// ************************************************************************
function MH_Slider(const Tags: array of PtrUInt): PObject_;
begin
  MH_Slider := MUI_NewObject(MUIC_Slider, Tags);
end;

function MH_Slider(var Slider; const Tags: array of PtrUInt): PObject_;
begin
 PObject_(Slider) := MUI_NewObject(MUIC_Slider, Tags);
 MH_Slider := PObject_(Slider);
end;

// Creates a MUI Numericbutton
// ************************************************************************
function MH_Numericbutton(const Tags: array of PtrUInt): PObject_;
begin
  MH_Numericbutton := MUI_NewObject(MUIC_Numericbutton, Tags);
end;

function MH_Numericbutton(var Numericbutton; const Tags: array of PtrUInt): PObject_;
begin
 PObject_(Numericbutton) := MUI_NewObject(MUIC_Numericbutton, Tags);
 MH_Numericbutton := PObject_(Numericbutton);
end;

// Creates a MUI Knob
// ************************************************************************
function MH_Knob(const Tags: array of PtrUInt): PObject_;
begin
  MH_Knob := MUI_NewObject(MUIC_Knob, Tags);
end;

function MH_Knob(var Knob; const Tags: array of PtrUInt): PObject_;
begin
 PObject_(Knob) := MUI_NewObject(MUIC_Knob, Tags);
 MH_Knob := PObject_(Knob);
end;

// Creates a MUI Levelmeter
// ************************************************************************
function MH_Levelmeter(const Tags: array of PtrUInt): PObject_;
begin
  MH_Levelmeter := MUI_NewObject(MUIC_Levelmeter, Tags);
end;

function MH_Levelmeter(var Levelmeter; const Tags: array of PtrUInt): PObject_;
begin
 PObject_(Levelmeter) := MUI_NewObject(MUIC_Levelmeter, Tags);
 MH_Levelmeter := PObject_(Levelmeter);
end;

// Creates a MUI Boopsi
// ************************************************************************
function MH_Boopsi(const Tags: array of PtrUInt): PObject_;
begin
  MH_Boopsi := MUI_NewObject(MUIC_Boopsi, Tags);
end;

function MH_Boopsi(var Boopsi; const Tags: array of PtrUInt): PObject_;
begin
 PObject_(Boopsi) := MUI_NewObject(MUIC_Boopsi, Tags);
 MH_Boopsi := PObject_(Boopsi);
end;

// Creates a MUI Colorfield
// ************************************************************************
function MH_Colorfield(const Tags: array of PtrUInt): PObject_;
begin
  MH_Colorfield := MUI_NewObject(MUIC_Colorfield, Tags);
end;

function MH_Colorfield(var Colorfield; const Tags: array of PtrUInt): PObject_;
begin
 PObject_(Colorfield) := MUI_NewObject(MUIC_Colorfield, Tags);
 MH_Colorfield := PObject_(Colorfield);
end;

// Creates a MUI Penadjust
// ************************************************************************
function MH_Penadjust(const Tags: array of PtrUInt): PObject_;
begin
  MH_Penadjust := MUI_NewObject(MUIC_Penadjust, Tags);
end;

function MH_Penadjust(var Penadjust; const Tags: array of PtrUInt): PObject_;
begin
 PObject_(Penadjust) := MUI_NewObject(MUIC_Penadjust, Tags);
 MH_Penadjust := PObject_(Penadjust);
end;

// Creates a MUI Coloradjust
// ************************************************************************
function MH_Coloradjust(const Tags: array of PtrUInt): PObject_;
begin
  MH_Coloradjust := MUI_NewObject(MUIC_Coloradjust, Tags);
end;

function MH_Coloradjust(var Coloradjust; const Tags: array of PtrUInt): PObject_;
begin
 PObject_(Coloradjust) := MUI_NewObject(MUIC_Coloradjust, Tags);
 MH_Coloradjust := PObject_(Coloradjust);
end;

// Creates a MUI Palette
// ************************************************************************
function MH_Palette(const Tags: array of PtrUInt): PObject_;
begin
  MH_Palette := MUI_NewObject(MUIC_Palette, Tags);
end;

function MH_Palette(var Palette; const Tags: array of PtrUInt): PObject_;
begin
 PObject_(Palette) := MUI_NewObject(MUIC_Palette, Tags);
 MH_Palette := PObject_(Palette);
end;

// Creates a MUI Group
// ************************************************************************
function MH_Group(const Tags: array of PtrUInt): PObject_;
begin
  MH_Group := MUI_NewObject(MUIC_Group, Tags);
end;

function MH_Group(var Group; const Tags: array of PtrUInt): PObject_;
begin
 PObject_(Group) := MUI_NewObject(MUIC_Group, Tags);
 MH_Group := PObject_(Group);
end;

// Creates a MUI Register
// ************************************************************************
function MH_Register(const Tags: array of PtrUInt): PObject_;
begin
  MH_Register := MUI_NewObject(MUIC_Register, Tags);
end;

function MH_Register(var Register; const Tags: array of PtrUInt): PObject_;
begin
 PObject_(Register) := MUI_NewObject(MUIC_Register, Tags);
 MH_Register := PObject_(Register);
end;

// Creates a MUI Virtgroup
// ************************************************************************
function MH_Virtgroup(const Tags: array of PtrUInt): PObject_;
begin
  MH_Virtgroup := MUI_NewObject(MUIC_Virtgroup, Tags);
end;

function MH_Virtgroup(var Virtgroup; const Tags: array of PtrUInt): PObject_;
begin
 PObject_(Virtgroup) := MUI_NewObject(MUIC_Virtgroup, Tags);
 MH_Virtgroup := PObject_(Virtgroup);
end;

// Creates a MUI Scrollgroup
// ************************************************************************
function MH_Scrollgroup(const Tags: array of PtrUInt): PObject_;
begin
  MH_Scrollgroup := MUI_NewObject(MUIC_Scrollgroup, Tags);
end;

function MH_Scrollgroup(var Scrollgroup; const Tags: array of PtrUInt): PObject_;
begin
 PObject_(Scrollgroup) := MUI_NewObject(MUIC_Scrollgroup, Tags);
 MH_Scrollgroup := PObject_(Scrollgroup);
end;

// Creates a MUI Popstring
// ************************************************************************
function MH_Popstring(const Tags: array of PtrUInt): PObject_;
begin
  MH_Popstring := MUI_NewObject(MUIC_Popstring, Tags);
end;

function MH_Popstring(var Popstring; const Tags: array of PtrUInt): PObject_;
begin
 PObject_(Popstring) := MUI_NewObject(MUIC_Popstring, Tags);
 MH_Popstring := PObject_(Popstring);
end;

// Creates a MUI Popobject
// ************************************************************************
function MH_Popobject(const Tags: array of PtrUInt): PObject_;
begin
  MH_Popobject := MUI_NewObject(MUIC_Popobject, Tags);
end;

function MH_Popobject(var Popobject; const Tags: array of PtrUInt): PObject_;
begin
 PObject_(Popobject) := MUI_NewObject(MUIC_Popobject, Tags);
 MH_Popobject := PObject_(Popobject);
end;

// Creates a MUI Poplist
// ************************************************************************
function MH_Poplist(const Tags: array of PtrUInt): PObject_;
begin
  MH_Poplist := MUI_NewObject(MUIC_Poplist, Tags);
end;

function MH_Poplist(var Poplist; const Tags: array of PtrUInt): PObject_;
begin
 PObject_(Poplist) := MUI_NewObject(MUIC_Poplist, Tags);
 MH_Poplist := PObject_(Poplist);
end;

// Creates a MUI Popasl
// ************************************************************************
function MH_Popasl(const Tags: array of PtrUInt): PObject_;
begin
  MH_Popasl := MUI_NewObject(MUIC_Popasl, Tags);
end;

function MH_Popasl(var Popasl; const Tags: array of PtrUInt): PObject_;
begin
 PObject_(Popasl) := MUI_NewObject(MUIC_Popasl, Tags);
 MH_Popasl := PObject_(Popasl);
end;

// Creates a MUI Pendisplay
// ************************************************************************
function MH_Pendisplay(const Tags: array of PtrUInt): PObject_;
begin
  MH_Pendisplay := MUI_NewObject(MUIC_Pendisplay, Tags);
end;

function MH_Pendisplay(var Pendisplay; const Tags: array of PtrUInt): PObject_;
begin
 PObject_(Pendisplay) := MUI_NewObject(MUIC_Pendisplay, Tags);
 MH_Pendisplay := PObject_(Pendisplay);
end;

// Creates a MUI Poppen
// ************************************************************************
function MH_Poppen(const Tags: array of PtrUInt): PObject_;
begin
  MH_Poppen := MUI_NewObject(MUIC_Poppen, Tags);
end;

function MH_Poppen(var Poppen; const Tags: array of PtrUInt): PObject_;
begin
 PObject_(Poppen) := MUI_NewObject(MUIC_Poppen, Tags);
 MH_Poppen := PObject_(Poppen);
end;

// Creates a MUI Aboutmui
// ************************************************************************
function MH_Aboutmui(const Tags: array of PtrUInt): PObject_;
begin
  MH_Aboutmui := MUI_NewObject(MUIC_Aboutmui, Tags);
end;

function MH_Aboutmui(var Aboutmui; const Tags: array of PtrUInt): PObject_;
begin
 PObject_(Aboutmui) := MUI_NewObject(MUIC_Aboutmui, Tags);
 MH_Aboutmui := PObject_(Aboutmui);
end;

// Creates a MUI Scrmodelist
// ************************************************************************
function MH_Scrmodelist(const Tags: array of PtrUInt): PObject_;
begin
  MH_Scrmodelist := MUI_NewObject(MUIC_Scrmodelist, Tags);
end;

function MH_Scrmodelist(var Scrmodelist; const Tags: array of PtrUInt): PObject_;
begin
 PObject_(Scrmodelist) := MUI_NewObject(MUIC_Scrmodelist, Tags);
 MH_Scrmodelist := PObject_(Scrmodelist);
end;

// Creates a MUI VGroup
// ************************************************************************
function MH_VGroup(const Tags: array of PtrUInt): PObject_;
begin
  MH_VGroup := MUI_NewObject(MUIC_Group, Tags);
end;

function MH_VGroup(Frame: LongWord; const Tags: array of PtrUInt): PObject_;
begin
  MH_VGroup := MUI_NewObject(MUIC_Group, [MUIA_Frame, Frame, TAG_MORE, AsTag(@Tags)]);
end;

function MH_VGroup(Title: PChar; const Tags: array of PtrUInt): PObject_;
begin
  MH_VGroup := MUI_NewObject(MUIC_Group, [
    MUIA_Frame,      MUIV_Frame_Group,
    MUIA_FrameTitle, AsTag(Title),
    MUIA_Background, MUII_GroupBack,
    TAG_MORE,        AsTag(@Tags)]);
end;

// Creates a MUI HGroup
// ************************************************************************
function MH_HGroup(const Tags: array of PtrUInt): PObject_;
begin
  MH_HGroup := MUI_NewObject(MUIC_Group, [MUIA_Group_Horiz, MUI_TRUE, TAG_MORE, AsTag(@Tags)]);
end;

function MH_HGroup(Frame: LongWord; const Tags: array of PtrUInt): PObject_;
begin
  MH_HGroup := MUI_NewObject(MUIC_Group, [
    MUIA_Group_Horiz, MUI_TRUE,
    MUIA_Frame,       Frame,
    TAG_MORE,         AsTag(@Tags)]);
end;

function MH_HGroup(Title: PChar; const Tags: array of PtrUInt): PObject_;
begin
  MH_HGroup := MUI_NewObject(MUIC_Group, [
    MUIA_Group_Horiz, MUI_TRUE,
    MUIA_Frame,       MUIV_Frame_Group,
    MUIA_FrameTitle,  AsTag(Title),
    MUIA_Background,  MUII_GroupBack,
    TAG_MORE,         AsTag(@Tags)]);
end;

// Creates MUI ColGroup
// ************************************************************************
function MH_ColGroup(Cols: LongWord; const Tags: array of PtrUInt): PObject_;
begin
  MH_ColGroup := MUI_NewObject(MUIC_Group, [MUIA_Group_Columns, Cols, TAG_MORE, AsTag(@Tags)]);
end;

function MH_ColGroup(Cols: LongWord; Frame: LongWord; const Tags: array of PtrUInt): PObject_;
begin
  MH_ColGroup := MUI_NewObject(MUIC_Group, [
    MUIA_Group_Columns, Cols,
    MUIA_Frame,         Frame,
    TAG_MORE,           AsTag(@Tags)]);
end;

function MH_ColGroup(Cols: LongWord; Title: PChar; const Tags: array of PtrUInt): PObject_;
begin
  MH_ColGroup := MUI_NewObject(MUIC_Group, [
    MUIA_Group_Columns, Cols,
    MUIA_Frame,         MUIV_Frame_Group,
    MUIA_FrameTitle,    AsTag(Title),
    MUIA_Background,    MUII_GroupBack,
    TAG_MORE,           AsTag(@Tags)]);
end;

// Creates MUI RowGroup
// ************************************************************************
function MH_RowGroup(Rows: LongWord; const Tags: array of PtrUInt): PObject_;
begin
  MH_RowGroup := MUI_NewObject(MUIC_Group, [MUIA_Group_Rows, Rows, TAG_MORE, AsTag(@Tags)]);
end;

function MH_RowGroup(Rows: LongWord; Frame: LongWord; const Tags: array of PtrUInt): PObject_;
begin
  MH_RowGroup := MUI_NewObject(MUIC_Group, [
    MUIA_Group_Rows, Rows,
    MUIA_Frame,      Frame,
    TAG_MORE,        AsTag(@Tags)]);
end;

function MH_RowGroup(Rows: LongWord; Title: PChar; const Tags: array of PtrUInt): PObject_;
begin
  MH_RowGroup := MUI_NewObject(MUIC_Group, [
    MUIA_Group_Rows, Rows,
    MUIA_Frame,      MUIV_Frame_Group,
    MUIA_FrameTitle, AsTag(Title),
    MUIA_Background, MUII_GroupBack,
    TAG_MORE,        AsTag(@Tags)]);
end;

// Creates a MUI Pagegroup
// ************************************************************************
function MH_Pagegroup(const Tags: array of PtrUInt): PObject_;
begin
  MH_Pagegroup := MUI_NewObject(MUIC_Group, [
    MUIA_Group_PageMode, MUI_TRUE,
    TAG_MORE,            AsTag(@Tags)]);
end;

function MH_Pagegroup(var Pagegroup; const Tags: array of PtrUInt): PObject_;
begin
  PObject_(Pagegroup) := MUI_NewObject(MUIC_Group, [
    MUIA_Group_PageMode, MUI_TRUE,
    TAG_MORE,            AsTag(@Tags)]);
  MH_Pagegroup := PObject_(Pagegroup);
end;

// Creates MUI V/HGroupV
// ************************************************************************
function MH_VGroupV(const Tags: array of PtrUInt): PObject_;
begin
  MH_VGroupV := MUI_NewObject(MUIC_Virtgroup, Tags);
end;

function MH_HGroupV(const Tags: array of PtrUInt): PObject_;
begin
  MH_HGroupV := MUI_NewObject(MUIC_Virtgroup, [
    MUIA_Group_Horiz, MUI_TRUE,
    TAG_MORE,         AsTag(@Tags)]);
end;

// Creates MUI Virtual ColGroup
// ************************************************************************
function MH_ColGroupV(Cols: LongWord; const Tags: array of PtrUInt): PObject_;
begin
  MH_ColGroupV := MUI_NewObject(MUIC_Virtgroup, [
    MUIA_Group_Columns, Cols,
    TAG_MORE,           AsTag(@Tags)]);
end;

function MH_RowGroupV(Rows: LongWord; const Tags: array of PtrUInt): PObject_;
begin
  MH_RowGroupV := MUI_NewObject(MUIC_Virtgroup, [
    MUIA_Group_Rows, Rows,
    TAG_MORE,        AsTag(@Tags)]);
end;

// Creates a MUI Virtual Pagegroup
// ************************************************************************
function MH_PagegroupV(const Tags: array of PtrUInt): PObject_;
begin
  MH_PagegroupV := MUI_NewObject(MUIC_Virtgroup, [
    MUIA_Group_PageMode, MUI_TRUE,
    TAG_MORE,            AsTag(@Tags)]);
end;

function MH_PagegroupV(var Pagegroup; const Tags: array of PtrUInt): PObject_;
begin
  PObject_(Pagegroup) := MUI_NewObject(MUIC_Virtgroup, [
    MUIA_Group_PageMode, MUI_TRUE,
    TAG_MORE,            AsTag(@Tags)]);
  MH_PagegroupV := PObject_(Pagegroup);
end;

// Creates a MUI RegisterGroup
// ************************************************************************
function MH_RegisterGroup(Titles: PPChar; const Tags: array of PtrUInt): PObject_;
begin
  MH_RegisterGroup := MUI_NewObject(MUIC_Register, [
    MUIA_Register_Titles, AsTag(Titles),
    TAG_MORE,             AsTag(@Tags)]);
end;

function MH_RegisterGroup(var RegisterGroup; Titles: PPChar; const Tags: array of PtrUInt): PObject_;
begin
  PObject_(RegisterGroup) := MUI_NewObject(MUIC_Register, [
    MUIA_Register_Titles, AsTag(Titles),
    TAG_MORE,             AsTag(@Tags)]);
  MH_RegisterGroup := PObject_(RegisterGroup);
end;

// Spacing Macros
// ************************************************************************
function MH_HVSpace: PObject_;
begin
  MH_HVSpace := MUI_NewObject(MUIC_Rectangle, [TAG_DONE]);
end;

function MH_HSpace(x: LongWord): PObject_;
begin
  MH_HSpace := MUI_MakeObject(MUIO_HSpace, [x]);
end;

function MH_VSpace(x: LongWord): PObject_;
begin
  MH_VSpace := MUI_MakeObject(MUIO_VSpace, [x]);
end;

function MH_HCenter(Obj: PObject_): PObject_;
begin
  MH_HCenter := MH_HGroup([
    MUIA_Group_Spacing, 0,
    Child,              AsTag(MH_HSpace(0)),
    Child,              AsTag(Obj),
    Child,              AsTag(MH_HSpace(0)),
    TAG_END]);
end;

function MH_VCenter(Obj: PObject_): PObject_;
begin
  MH_VCenter := MH_VGroup([
    MUIA_Group_Spacing, 0,
    Child,              AsTag(MH_VSpace(0)),
    Child,              AsTag(Obj),
    Child,              AsTag(MH_VSpace(0)),
    TAG_END]);
end;

// Creates a MUI button
// ************************************************************************
function MH_Button(BLabel: PChar): PObject_;
begin
  MH_Button := MUI_MakeObject(MUIO_Button, [PtrUInt(BLabel)]);
end;

function MH_Button(var Button; BLabel: PChar): PObject_;
begin
  PObject_(Button) := MUI_MakeObject(MUIO_Button, [PtrUInt(BLabel)]);
  MH_Button := PObject_(Button);
end;

function MH_SimpleButton(BLabel: PChar): PObject_; inline;
begin
  MH_SimpleButton := MH_Button(BLabel);
end;

function MH_SimpleButton(var Button; BLabel: PChar): PObject_;
begin
  MH_SimpleButton := MH_Button(Button, BLabel);
end;

// Creates a MUI PopButton
// ************************************************************************
function MH_PopButton(Img: PChar): PObject_;
begin
  MH_PopButton := MUI_MakeObject(MUIO_PopButton, [AsTag(Img)]);
end;

function MH_PopButton(var PopButton; img: PChar): PObject_;
begin
  PObject_(PopButton) := MUI_MakeObject(MUIO_PopButton, [AsTag(Img)]);
  MH_PopButton := PObject_(PopButton);
end;

// Creates a MUI Label
// ************************************************************************
function MH_Label(BLabel: PChar): PObject_;
begin
  MH_Label := MUI_MakeObject(MUIO_Label, [PtrUInt(BLabel), 0]);
end;

function MH_Label(var OLabel; BLabel: PChar): PObject_;
begin
  PObject_(OLabel) := MUI_MakeObject(MUIO_Label, [PtrUInt(BLabel), 0]);
  MH_Label := PObject_(OLabel);
end;

// Creates a MUI label with single Frame border
// ************************************************************************
function MH_Label1(BLabel: PChar): PObject_;
begin
  MH_Label1 := MUI_MakeObject(MUIO_Label, [PtrUInt(BLabel), MUIO_Label_SingleFrame]);
end;

function MH_Label1(var OLabel; BLabel: PChar): PObject_;
begin
  PObject_(OLabel) := MUI_MakeObject(MUIO_Label, [PtrUInt(BLabel), MUIO_Label_SingleFrame]);
  MH_Label1 := PObject_(OLabel);
end;

// Creates a MUI label with double Frame border
// ************************************************************************
function MH_Label2(BLabel: PChar): PObject_;
begin
  MH_Label2 := MUI_MakeObject(MUIO_Label, [PtrUInt(BLabel), MUIO_Label_DoubleFrame]);
end;

function MH_Label2(var OLabel; BLabel: PChar): PObject_;
begin
  PObject_(OLabel) := MUI_MakeObject(MUIO_Label, [PtrUInt(BLabel), MUIO_Label_DoubleFrame]);
  MH_Label2 := PObject_(OLabel);
end;

// Creates a MUI left aligned label
// ************************************************************************
function MH_LLabel(BLabel: PChar): PObject_;
begin
  MH_LLabel := MUI_MakeObject(MUIO_Label, [PtrUInt(BLabel), MUIO_Label_LeftAligned]);
end;

function MH_LLabel(var OLabel; BLabel: PChar): PObject_;
begin
  PObject_(OLabel) := MUI_MakeObject(MUIO_Label, [PtrUInt(BLabel), MUIO_Label_LeftAligned]);
  MH_LLabel := PObject_(OLabel);
end;

// Creates a MUI left aligned label with single Frame border
// ************************************************************************
function MH_LLabel1(BLabel: PChar): PObject_;
begin
  MH_LLabel1 := MUI_MakeObject(MUIO_Label, [PtrUInt(BLabel), MUIO_Label_LeftAligned or MUIO_Label_SingleFrame]);
end;

function MH_LLabel1(var OLabel; BLabel: PChar): PObject_;
begin
  PObject_(OLabel) := MUI_MakeObject(MUIO_Label, [PtrUInt(BLabel), MUIO_Label_LeftAligned or MUIO_Label_SingleFrame]);
  MH_LLabel1 := PObject_(OLabel);
end;

// Creates a MUI left aligned label with double Frame border
// ************************************************************************
function MH_LLabel2(BLabel: PChar): PObject_;
begin
  MH_LLabel2 := MUI_MakeObject(MUIO_Label, [PtrUInt(BLabel), MUIO_Label_LeftAligned or MUIO_Label_DoubleFrame]);
end;

function MH_LLabel2(var OLabel; BLabel: PChar): PObject_;
begin
  PObject_(OLabel) := MUI_MakeObject(MUIO_Label, [PtrUInt(BLabel), MUIO_Label_LeftAligned or MUIO_Label_DoubleFrame]);
  MH_LLabel2 := PObject_(OLabel);
end;

// Creates a MUI centered label
// ************************************************************************
function MH_CLabel(BLabel: PChar): PObject_;
begin
  MH_CLabel := MUI_MakeObject(MUIO_Label, [PtrUInt(BLabel), MUIO_Label_Centered]);
end;

function MH_CLabel(var OLabel; BLabel: PChar): PObject_;
begin
  PObject_(OLabel) := MUI_MakeObject(MUIO_Label, [PtrUInt(BLabel), MUIO_Label_Centered]);
  MH_CLabel := PObject_(OLabel);
end;

// Creates a MUI centered label with single Frame border
// ************************************************************************
function MH_CLabel1(BLabel: PChar): PObject_;
begin
  MH_CLabel1 := MUI_MakeObject(MUIO_Label, [PtrUInt(BLabel), MUIO_Label_Centered or MUIO_Label_SingleFrame]);
end;

function MH_CLabel1(var OLabel; BLabel: PChar): PObject_;
begin
  PObject_(OLabel) := MUI_MakeObject(MUIO_Label, [PtrUInt(BLabel), MUIO_Label_Centered or MUIO_Label_SingleFrame]);
  MH_CLabel1 := PObject_(OLabel);
end;

// Creates a MUI centered label with double Frame border
// ************************************************************************
function MH_CLabel2(BLabel: PChar): PObject_;
begin
  MH_CLabel2 := MUI_MakeObject(MUIO_Label, [PtrUInt(BLabel), MUIO_Label_Centered or MUIO_Label_DoubleFrame]);
end;

function MH_CLabel2(var OLabel; BLabel: PChar): PObject_;
begin
  PObject_(OLabel) := MUI_MakeObject(MUIO_Label, [PtrUInt(BLabel), MUIO_Label_Centered or MUIO_Label_DoubleFrame]);
  MH_CLabel2 := PObject_(OLabel);
end;

// Creates a MUI Label
// ************************************************************************
function MH_FreeLabel(BLabel: PChar): PObject_;
begin
  MH_FreeLabel := MUI_MakeObject(MUIO_Label, [PtrUInt(BLabel), MUIO_Label_FreeVert]);
end;

function MH_FreeLabel(var OLabel; BLabel: PChar): PObject_;
begin
  PObject_(OLabel) := MUI_MakeObject(MUIO_Label, [PtrUInt(BLabel), MUIO_Label_FreeVert]);
  MH_FreeLabel := PObject_(OLabel);
end;

// Creates a MUI label with single Frame border
// ************************************************************************
function MH_FreeLabel1(BLabel: PChar): PObject_;
begin
  MH_FreeLabel1 := MUI_MakeObject(MUIO_Label, [PtrUInt(BLabel), MUIO_Label_FreeVert or MUIO_Label_SingleFrame]);
end;

function MH_FreeLabel1(var OLabel; BLabel: PChar): PObject_;
begin
  PObject_(OLabel) := MUI_MakeObject(MUIO_Label, [PtrUInt(BLabel), MUIO_Label_FreeVert or MUIO_Label_SingleFrame]);
  MH_FreeLabel1 := PObject_(OLabel);
end;

// Creates a MUI label with double Frame border
// ************************************************************************
function MH_FreeLabel2(BLabel: PChar): PObject_;
begin
  MH_FreeLabel2 := MUI_MakeObject(MUIO_Label, [PtrUInt(BLabel), MUIO_Label_FreeVert or MUIO_Label_DoubleFrame]);
end;

function MH_FreeLabel2(var OLabel; BLabel: PChar): PObject_;
begin
  PObject_(OLabel) := MUI_MakeObject(MUIO_Label, [PtrUInt(BLabel), MUIO_Label_FreeVert or MUIO_Label_DoubleFrame]);
  MH_FreeLabel2 := PObject_(OLabel);
end;

// Creates a MUI left aligned label
// ************************************************************************
function MH_FreeLLabel(BLabel: PChar): PObject_;
begin
  MH_FreeLLabel := MUI_MakeObject(MUIO_Label, [PtrUInt(BLabel), MUIO_Label_FreeVert or MUIO_Label_LeftAligned]);
end;

function MH_FreeLLabel(var OLabel; BLabel: PChar): PObject_;
begin
  PObject_(OLabel) := MUI_MakeObject(MUIO_Label, [PtrUInt(BLabel), MUIO_Label_FreeVert or MUIO_Label_LeftAligned]);
  MH_FreeLLabel := PObject_(OLabel);
end;

// Creates a MUI left aligned label with single Frame border
// ************************************************************************
function MH_FreeLLabel1(BLabel: PChar): PObject_;
begin
  MH_FreeLLabel1 := MUI_MakeObject(MUIO_Label, [PtrUInt(BLabel), MUIO_Label_FreeVert or MUIO_Label_LeftAligned or MUIO_Label_SingleFrame]);
end;

function MH_FreeLLabel1(var OLabel; BLabel: PChar): PObject_;
begin
  PObject_(OLabel) := MUI_MakeObject(MUIO_Label, [PtrUInt(BLabel), MUIO_Label_FreeVert or MUIO_Label_LeftAligned or MUIO_Label_SingleFrame]);
  MH_FreeLLabel1 := PObject_(OLabel);
end;

// Creates a MUI left aligned label with double Frame border
// ************************************************************************
function MH_FreeLLabel2(BLabel: PChar): PObject_;
begin
  MH_FreeLLabel2 := MUI_MakeObject(MUIO_Label, [PtrUInt(BLabel), MUIO_Label_FreeVert or MUIO_Label_LeftAligned or MUIO_Label_DoubleFrame]);
end;

function MH_FreeLLabel2(var OLabel; BLabel: PChar): PObject_;
begin
  PObject_(OLabel) := MUI_MakeObject(MUIO_Label, [PtrUInt(BLabel), MUIO_Label_FreeVert or MUIO_Label_LeftAligned or MUIO_Label_DoubleFrame]);
  MH_FreeLLabel2 := PObject_(OLabel);
end;

// Creates a MUI centered label
// ************************************************************************
function MH_FreeCLabel(BLabel: PChar): PObject_;
begin
  MH_FreeCLabel := MUI_MakeObject(MUIO_Label, [PtrUInt(BLabel), MUIO_Label_FreeVert or MUIO_Label_Centered]);
end;

function MH_FreeCLabel(var OLabel; BLabel: PChar): PObject_;
begin
  PObject_(OLabel) := MUI_MakeObject(MUIO_Label, [PtrUInt(BLabel), MUIO_Label_FreeVert or MUIO_Label_Centered]);
  MH_FreeCLabel := PObject_(OLabel);
end;

// Creates a MUI centered label with single Frame border
// ************************************************************************
function MH_FreeCLabel1(BLabel: PChar): PObject_;
begin
  MH_FreeCLabel1 := MUI_MakeObject(MUIO_Label, [PtrUInt(BLabel), MUIO_Label_FreeVert or MUIO_Label_Centered or MUIO_Label_SingleFrame]);
end;

function MH_FreeCLabel1(var OLabel; BLabel: PChar): PObject_;
begin
  PObject_(OLabel) := MUI_MakeObject(MUIO_Label, [PtrUInt(BLabel), MUIO_Label_FreeVert or MUIO_Label_Centered or MUIO_Label_SingleFrame]);
  MH_FreeCLabel1 := PObject_(OLabel);
end;

// Creates a MUI centered label with double Frame border
// ************************************************************************
function MH_FreeCLabel2(BLabel: PChar): PObject_;
begin
  MH_FreeCLabel2 := MUI_MakeObject(MUIO_Label, [PtrUInt(BLabel), MUIO_Label_FreeVert or MUIO_Label_Centered or MUIO_Label_DoubleFrame]);
end;

function MH_FreeCLabel2(var OLabel; BLabel: PChar): PObject_;
begin
  PObject_(OLabel) := MUI_MakeObject(MUIO_Label, [PtrUInt(BLabel), MUIO_Label_FreeVert or MUIO_Label_Centered or MUIO_Label_DoubleFrame]);
  MH_FreeCLabel2 := PObject_(OLabel);
end;

// Creates a MUI HBar
// ************************************************************************
function MH_HBar(Space: LongWord): PObject_;
begin
 MH_HBar := MUI_MakeObject(MUIO_HBar, [Space]);
end;

function MH_HBar(var HBar; Space: LongWord): PObject_;
begin
 PObject_(HBar) := MUI_MakeObject(MUIO_HBar, [Space]);
 MH_HBar := PObject_(HBar);
end;

// Creates a MUI VBar
// ************************************************************************
function MH_VBar(Space: LongWord): PObject_;
begin
 MH_VBar := MUI_MakeObject(MUIO_VBar, [Space]);
end;

function MH_VBar(var VBar; Space: LongWord): PObject_;
begin
 PObject_(VBar) := MUI_MakeObject(MUIO_VBar, [Space]);
 MH_VBar := PObject_(VBar);
end;

end.
