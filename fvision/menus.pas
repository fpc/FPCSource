{ $Id$ }
{********[ SOURCE FILE OF GRAPHICAL FREE VISION ]**********}
{                                                          }
{   System independent GRAPHICAL clone of MENUS.PAS        }
{                                                          }
{   Interface Copyright (c) 1992 Borland International     }
{                                                          }
{   Copyright (c) 1996, 1997, 1998, 1999 by Leon de Boer   }
{   ldeboer@attglobal.net  - primary e-mail addr           }
{   ldeboer@starwon.com.au - backup e-mail addr            }
{                                                          }
{****************[ THIS CODE IS FREEWARE ]*****************}
{                                                          }
{     This sourcecode is released for the purpose to       }
{   promote the pascal language on all platforms. You may  }
{   redistribute it and/or modify with the following       }
{   DISCLAIMER.                                            }
{                                                          }
{     This SOURCE CODE is distributed "AS IS" WITHOUT      }
{   WARRANTIES AS TO PERFORMANCE OF MERCHANTABILITY OR     }
{   ANY OTHER WARRANTIES WHETHER EXPRESSED OR IMPLIED.     }
{                                                          }
{*****************[ SUPPORTED PLATFORMS ]******************}
{     16 and 32 Bit compilers                              }
{        DOS      - Turbo Pascal 7.0 +      (16 Bit)       }
{        DPMI     - Turbo Pascal 7.0 +      (16 Bit)       }
{                 - FPC 0.9912+ (GO32V2)    (32 Bit)       }
{        WINDOWS  - Turbo Pascal 7.0 +      (16 Bit)       }
{                 - Delphi 1.0+             (16 Bit)       }
{        WIN95/NT - Delphi 2.0+             (32 Bit)       }
{                 - Virtual Pascal 2.0+     (32 Bit)       }
{                 - Speedsoft Sybil 2.0+    (32 Bit)       }
{                 - FPC 0.9912+             (32 Bit)       }
{        OS2      - Virtual Pascal 1.0+     (32 Bit)       }
{                 - Speed pascal 1.0+       (32 Bit)       }
{                                                          }
{******************[ REVISION HISTORY ]********************}
{  Version  Date        Fix                                }
{  -------  ---------   ---------------------------------  }
{  1.00     12 Jun 96   Initial DOS/DPMI code released.    }
{  1.10     18 Oct 97   Code converted to GUI & TEXT mode. }
{  1.20     18 Jul 97   Windows conversion added.          }
{  1.30     29 Aug 97   Platform.inc sort added.           }
{  1.40     22 Oct 97   Delphi3 32 bit code added.         }
{  1.50     05 May 98   Virtual pascal 2.0 code added.     }
{  1.60     11 May 98   Clean up and ensure error checks.  }
{  1.70     15 May 98   Documentation & format completed.  }
{  1.80     30 Sep 99   Complete recheck preformed         }
{  1.81     03 Nov 99   FPC windows support added          }
{  1.90     26 Nov 99   Graphics stuff moved to GFVGraph   }
{**********************************************************}

UNIT Menus;

{<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
                                  INTERFACE
{<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}

{====Include file to sort compiler platform out =====================}
{$I Platform.inc}
{====================================================================}

{==== Compiler directives ===========================================}

{$IFNDEF PPC_FPC}{ FPC doesn't support these switches }
  {$F-} { Near calls are okay }
  {$A+} { Word Align Data }
  {$B-} { Allow short circuit boolean evaluations }
  {$O+} { This unit may be overlaid }
  {$G+} { 286 Code optimization - if you're on an 8088 get a real computer }
  {$P-} { Normal string variables }
  {$N-} { No 80x87 code generation }
  {$E+} { Emulation is on }
{$ENDIF}

{$X+} { Extended syntax is ok }
{$R-} { Disable range checking }
{$S-} { Disable Stack Checking }
{$I-} { Disable IO Checking }
{$Q-} { Disable Overflow Checking }
{$V-} { Turn off strict VAR strings }
{====================================================================}

USES
   {$IFDEF OS_WINDOWS}                                { WIN/NT CODE }
     {$IFNDEF PPC_SPEED}                              { NON SPEED COMPILER }
       {$IFDEF PPC_FPC}                               { FPC WINDOWS COMPILER }
       Windows,                                       { Standard unit }
       {$ELSE}                                        { OTHER COMPILERS }
       WinTypes,WinProcs,                             { Standard units }
       {$ENDIF}
     {$ELSE}                                          { SPEEDSOFT COMPILER }
       WinBase, WinDef,                               { Standard units }
     {$ENDIF}
   {$ENDIF}

   GFVGraph,                                          { GFV standard unit }
   Objects, Drivers, Views;                           { GFV standard units }

{***************************************************************************}
{                              PUBLIC CONSTANTS                             }
{***************************************************************************}

{---------------------------------------------------------------------------}
{                               COLOUR PALETTES                             }
{---------------------------------------------------------------------------}
CONST
   CMenuView   = #2#3#4#5#6#7;                        { Menu colours }
   CStatusLine = #2#3#4#5#6#7;                        { Statusline colours }

{***************************************************************************}
{                            RECORD DEFINITIONS                             }
{***************************************************************************}
TYPE
   TMenuStr = String[31];                             { Menu string }

   PMenu = ^TMenu;                                    { Pointer to menu }

{---------------------------------------------------------------------------}
{                              TMenuItem RECORD                             }
{---------------------------------------------------------------------------}
   PMenuItem = ^TMenuItem;
   TMenuItem = PACKED RECORD
     Next: PMenuItem;                                 { Next menu item }
     Name: PString;                                   { Menu item name }
     Command: Word;                                   { Menu item command }
     Disabled: Boolean;                               { Menu item state }
     KeyCode: Word;                                   { Menu item keycode }
     HelpCtx: Word;                                   { Menu item help ctx }
     Case Integer Of
       0: (Param: PString);
       1: (SubMenu: PMenu);
   END;

{---------------------------------------------------------------------------}
{                                TMenu RECORD                               }
{---------------------------------------------------------------------------}
   TMenu = PACKED RECORD
     Items: PMenuItem;                                { Menu item list }
     Default: PMenuItem;                              { Default menu }
   END;

{---------------------------------------------------------------------------}
{                             TStatusItem RECORD                            }
{---------------------------------------------------------------------------}
TYPE
   PStatusItem = ^TStatusItem;
   TStatusItem = PACKED RECORD
     Next: PStatusItem;                               { Next status item }
     Text: PString;                                   { Text of status item }
     KeyCode: Word;                                   { Keycode of item }
     Command: Word;                                   { Command of item }
   END;

{---------------------------------------------------------------------------}
{                             TStatusDef RECORD                             }
{---------------------------------------------------------------------------}
TYPE
   PStatusDef = ^TStatusDef;
   TStatusDef = PACKED RECORD
     Next: PStatusDef;                                { Next status defined }
     Min, Max: Word;                                  { Range of item }
     Items: PStatusItem;                              { Item list }
   END;

{***************************************************************************}
{                            OBJECT DEFINITIONS                             }
{***************************************************************************}

{---------------------------------------------------------------------------}
{                TMenuView OBJECT - MENU VIEW ANCESTOR OBJECT               }
{---------------------------------------------------------------------------}
TYPE
   PMenuView = ^TMenuView;
   TMenuView = OBJECT (TView)
         ParentMenu: PMenuView;                       { Parent menu }
         Menu      : PMenu;                           { Menu item list }
         Current   : PMenuItem;                       { Current menu item }
         OldItem   : PMenuItem;                       { Old item for draws }
      CONSTRUCTOR Init (Var Bounds: TRect);
      CONSTRUCTOR Load (Var S: TStream);
      FUNCTION Execute: Word; Virtual;
      FUNCTION GetHelpCtx: Word; Virtual;
      FUNCTION GetPalette: PPalette; Virtual;
      FUNCTION FindItem (Ch: Char): PMenuItem;
      FUNCTION HotKey (KeyCode: Word): PMenuItem;
      FUNCTION NewSubView (Var Bounds: TRect; AMenu: PMenu;
        AParentMenu: PMenuView): PMenuView; Virtual;
      PROCEDURE Store (Var S: TStream);
      PROCEDURE HandleEvent (Var Event: TEvent); Virtual;
      PROCEDURE GetItemRect (Item: PMenuItem; Var R: TRect); Virtual;
      private
      PROCEDURE GetItemRectX (Item: PMenuItem; Var R: TRect); Virtual;
   END;

{---------------------------------------------------------------------------}
{                    TMenuBar OBJECT - MENU BAR OBJECT                      }
{---------------------------------------------------------------------------}
TYPE
   TMenuBar = OBJECT (TMenuView)
      CONSTRUCTOR Init (Var Bounds: TRect; AMenu: PMenu);
      DESTRUCTOR Done; Virtual;
      PROCEDURE Draw; Virtual;
      PROCEDURE DrawBackGround; Virtual;
      private
      PROCEDURE GetItemRectX (Item: PMenuItem; Var R: TRect); Virtual;
   END;
   PMenuBar = ^TMenuBar;

{---------------------------------------------------------------------------}
{                   TMenuBox OBJECT - BOXED MENU OBJECT                     }
{---------------------------------------------------------------------------}
TYPE
   TMenuBox = OBJECT (TMenuView)
      CONSTRUCTOR Init (Var Bounds: TRect; AMenu: PMenu;
        AParentMenu: PMenuView);
      PROCEDURE Draw; Virtual;
      PROCEDURE DrawBackGround; Virtual;
      private
      PROCEDURE GetItemRectX (Item: PMenuItem; Var R: TRect); Virtual;
   END;
   PMenuBox = ^TMenuBox;

{---------------------------------------------------------------------------}
{                  TMenuPopUp OBJECT - POPUP MENU OBJECT                    }
{---------------------------------------------------------------------------}
TYPE
   TMenuPopup = OBJECT (TMenuBox)
      CONSTRUCTOR Init (Var Bounds: TRect; AMenu: PMenu);
      DESTRUCTOR Done; Virtual;
      PROCEDURE HandleEvent (Var Event: TEvent); Virtual;
   END;
   PMenuPopup = ^TMenuPopup;

{---------------------------------------------------------------------------}
{                    TStatusLine OBJECT - STATUS LINE OBJECT                }
{---------------------------------------------------------------------------}
TYPE
   TStatusLine = OBJECT (TView)
         Items: PStatusItem;                          { Status line items }
         Defs : PStatusDef;                           { Status line default }
      CONSTRUCTOR Init (Var Bounds: TRect; ADefs: PStatusDef);
      CONSTRUCTOR Load (Var S: TStream);
      DESTRUCTOR Done; Virtual;
      FUNCTION GetPalette: PPalette; Virtual;
      FUNCTION Hint (AHelpCtx: Word): String; Virtual;
      PROCEDURE Draw; Virtual;
      PROCEDURE Update; Virtual;
      PROCEDURE Store (Var S: TStream);
      PROCEDURE HandleEvent (Var Event: TEvent); Virtual;
      PRIVATE
      PROCEDURE FindItems;
      PROCEDURE DrawSelect (Selected: PStatusItem);
   END;
   PStatusLine = ^TStatusLine;

{***************************************************************************}
{                            INTERFACE ROUTINES                             }
{***************************************************************************}

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                           MENU INTERFACE ROUTINES                         }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{-NewMenu------------------------------------------------------------
Allocates and returns a pointer to a new TMenu record. Sets the Items
and Default fields of the record to the value given by the parameter.
An error creating will return a nil pointer.
14May98 LdB
---------------------------------------------------------------------}
FUNCTION NewMenu (Items: PMenuItem): PMenu;

{-DisposeMenu--------------------------------------------------------
Disposes of all the elements of the specified menu (and all submenus).
14May98 LdB
---------------------------------------------------------------------}
PROCEDURE DisposeMenu (Menu: PMenu);

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                             MENU ITEM ROUTINES                            }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{-NewLine------------------------------------------------------------
Allocates and returns a pointer to a new TMenuItem record that
represents a separator line in a menu box.
An error creating will return a nil pointer.
14May98 LdB
---------------------------------------------------------------------}
FUNCTION NewLine (Next: PMenuItem): PMenuItem;

{-NewItem------------------------------------------------------------
Allocates and returns a pointer to a new TMenuItem record that
represents a menu item (using NewStr to allocate the Name and Param).
An error creating will return a nil pointer.
14May98 LdB
---------------------------------------------------------------------}
FUNCTION NewItem (Name, Param: TMenuStr; KeyCode: Word; Command: Word;
  AHelpCtx: Word; Next: PMenuItem): PMenuItem;

{-NewSubMenu---------------------------------------------------------
Allocates and returns a pointer to a new TMenuItem record, which
represents a submenu (using NewStr to allocate the Name).
An error creating will return a nil pointer.
14May98 LdB
---------------------------------------------------------------------}
FUNCTION NewSubMenu (Name: TMenuStr; AHelpCtx: Word; SubMenu: PMenu;
  Next: PMenuItem): PMenuItem;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                          STATUS INTERFACE ROUTINES                        }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{-NewStatusDef-------------------------------------------------------
Allocates and returns a pointer to a new TStatusDef record initialized
with the given parameter values. Calls to NewStatusDef can be nested.
An error creating will return a nil pointer.
15May98 LdB
---------------------------------------------------------------------}
FUNCTION NewStatusDef (AMin, AMax: Word; AItems: PStatusItem;
  ANext: PStatusDef): PStatusDef;

{-NewStatusKey-------------------------------------------------------
Allocates and returns a pointer to a new TStatusItem record initialized
with the given parameter values (using NewStr to allocate the Text).
An error in creating will return a nil pointer.
15May98 LdB
---------------------------------------------------------------------}
FUNCTION NewStatusKey (AText: String; AKeyCode: Word; ACommand: Word;
  ANext: PStatusItem): PStatusItem;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                           OBJECT REGISTER ROUTINES                        }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{-RegisterMenus-------------------------------------------------------
Calls RegisterType for each of the object types defined in this unit.
15May98 LdB
---------------------------------------------------------------------}
PROCEDURE RegisterMenus;

{***************************************************************************}
{                           OBJECT REGISTRATION                             }
{***************************************************************************}

{---------------------------------------------------------------------------}
{                        TMenuBar STREAM REGISTRATION                       }
{---------------------------------------------------------------------------}
CONST
   RMenuBar: TStreamRec = (
     ObjType: 40;                                     { Register id = 40 }
     {$IFDEF BP_VMTLink}                              { BP style VMT link }
     VmtLink: Ofs(TypeOf(TMenuBar)^);
     {$ELSE}                                          { Alt style VMT link }
     VmtLink: TypeOf(TMenuBar);
     {$ENDIF}
     Load:    @TMenuBar.Load;                         { Object load method }
     Store:   @TMenuBar.Store                         { Object store method }
   );

{---------------------------------------------------------------------------}
{                        TMenuBox STREAM REGISTRATION                       }
{---------------------------------------------------------------------------}
CONST
   RMenuBox: TStreamRec = (
     ObjType: 41;                                     { Register id = 41 }
     {$IFDEF BP_VMTLink}                              { BP style VMT link }
     VmtLink: Ofs(TypeOf(TMenuBox)^);
     {$ELSE}                                          { Alt style VMT link }
     VmtLink: TypeOf(TMenuBox);
     {$ENDIF}
     Load:    @TMenuBox.Load;                         { Object load method }
     Store:   @TMenuBox.Store                         { Object store method }
   );

{---------------------------------------------------------------------------}
{                      TStatusLine STREAM REGISTRATION                      }
{---------------------------------------------------------------------------}
CONST
   RStatusLine: TStreamRec = (
     ObjType: 42;                                     { Register id = 42 }
     {$IFDEF BP_VMTLink}                              { BP style VMT link }
     VmtLink: Ofs(TypeOf(TStatusLine)^);
     {$ELSE}                                          { Alt style VMT link }
     VmtLink: TypeOf(TStatusLine);
     {$ENDIF}
     Load:    @TStatusLine.Load;                      { Object load method }
     Store:   @TStatusLine.Store                      { Object store method }
   );

{---------------------------------------------------------------------------}
{                       TMenuPopup STREAM REGISTRATION                      }
{---------------------------------------------------------------------------}
CONST
   RMenuPopup: TStreamRec = (
     ObjType: 43;                                     { Register id = 43 }
     {$IFDEF BP_VMTLink}                              { BP style VMT link }
     VmtLink: Ofs(TypeOf(TMenuPopup)^);
     {$ELSE}                                          { Alt style VMT link }
     VmtLink: TypeOf(TMenuPopup);
     {$ENDIF}
     Load:    @TMenuPopup.Load;                       { Object load method }
     Store:   @TMenuPopup.Store                       { Object store method }
   );

{***************************************************************************}
{                        INITIALIZED PUBLIC VARIABLES                       }
{***************************************************************************}

{---------------------------------------------------------------------------}
{                       INITIALIZED PUBLIC VARIABLES                        }
{---------------------------------------------------------------------------}
CONST
   AdvancedMenus: Boolean = False;                    { Advanced menus }

{<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
                                IMPLEMENTATION
{<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>}
{$ifndef GRAPH_API}
USES
  Video;
{$endif not GRAPH_API}

CONST
  SubMenuChar : array[boolean] of char = ('>',#16);
  { SubMenuChar is the character displayed at right of submenu }

{***************************************************************************}
{                               OBJECT METHODS                              }
{***************************************************************************}

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                          TMenuView OBJECT METHODS                         }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{--TMenuView----------------------------------------------------------------}
{  Init -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 08May98 LdB              }
{---------------------------------------------------------------------------}
CONSTRUCTOR TMenuView.Init (Var Bounds: TRect);
BEGIN
   Inherited Init(Bounds);                            { Call ancestor }
   EventMask := EventMask OR evBroadcast;             { See broadcast events }
END;

{--TMenuView----------------------------------------------------------------}
{  Load -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 30Sep99 LdB              }
{---------------------------------------------------------------------------}
CONSTRUCTOR TMenuView.Load (Var S: TStream);

   FUNCTION DoLoadMenu: PMenu;
   VAR Tok: Byte; Item: PMenuItem; Last: ^PMenuItem; Menu: PMenu;
   BEGIN
     New(Menu);                                       { Create new menu }
     Last := @Menu^.Items;                            { Start on first item }
     Item := Nil;                                     { Clear pointer }
     S.Read(Tok, 1);                                  { Read token }
     While (Tok <> 0) Do Begin
       New(Item);                                     { Create new item }
       Last^ := Item;                                 { First part of chain }
       If (Item <> Nil) Then Begin                    { Check item valid }
         Last := @Item^.Next;                         { Complete chain }
         With Item^ Do Begin
           Name := S.ReadStr;                         { Read menu name }
           S.Read(Command, 2);                        { Menu item command }
           S.Read(Disabled, 1);                       { Menu item state }
           S.Read(KeyCode, 2);                        { Menu item keycode }
           S.Read(HelpCtx, 2);                        { Menu item help ctx }
           If (Name <> Nil) Then
             If Command = 0 Then
{$ifdef PPC_FPC}
               SubMenu := DoLoadMenu()                  { Load submenu }
{$else not PPC_FPC}
               SubMenu := DoLoadMenu                  { Load submenu }
{$endif not PPC_FPC}
                 Else Param := S.ReadStr;             { Read param string }
         End;
       End;
       S.Read(Tok, 1);                                { Read token }
     End;
     Last^ := Nil;                                    { List complete }
     Menu^.Default := Menu^.Items;                    { Set menu default }
     DoLoadMenu := Menu;                              { Return menu }
   End;

BEGIN
   Inherited Load(S);                                 { Call ancestor }
   Menu := DoLoadMenu;                                { Load menu items }
END;

{--TMenuView----------------------------------------------------------------}
{  Execute -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 30Sep99 LdB           }
{---------------------------------------------------------------------------}
FUNCTION TMenuView.Execute: Word;
TYPE MenuAction = (DoNothing, DoSelect, DoReturn);
VAR AutoSelect: Boolean; Action: MenuAction; Ch: Char; Res: Word; R: TRect;
  ItemShown, P: PMenuItem; Target: PMenuView; E: TEvent; MouseActive: Boolean;

   PROCEDURE TrackMouse;
   VAR Mouse: TPoint; R: TRect;
   BEGIN
     Mouse.X := E.Where.X - RawOrigin.X;              { Local x position }
     Mouse.Y := E.Where.Y - RawoRigin.Y;              { Local y position }
     Current := Menu^.Items;                          { Start with current }
     While (Current <> Nil) Do Begin
       GetItemRectX(Current, R);                       { Get item rectangle }
       If R.Contains(Mouse) Then Begin                { Contains mouse }
         MouseActive := True;                         { Return true }
         Exit;                                        { Then exit }
       End;
       Current := Current^.Next;                      { Try next item }
     End;
   END;

   PROCEDURE TrackKey (FindNext: Boolean);

       PROCEDURE NextItem;
       BEGIN
         Current := Current^.Next;                    { Move to next item }
         If (Current = Nil) Then
           Current := Menu^.Items;                    { Return first menu }
       END;

       PROCEDURE PrevItem;
       VAR P: PMenuItem;
       BEGIN
         P := Current;                                { Start on current }
         If (P = Menu^.Items) Then P := Nil;          { Check if at start }
         Repeat NextItem Until Current^.Next = P;     { Prev item found }
       END;

   BEGIN
     If (Current <> Nil) Then                         { Current view valid }
       Repeat
         If FindNext Then NextItem Else PrevItem;     { Find next/prev item }
       Until (Current^.Name <> Nil);                  { Until we have name }
   END;

   FUNCTION MouseInOwner: Boolean;
   VAR Mouse: TPoint; R: TRect;
   BEGIN
     MouseInOwner := False;                           { Preset false }
     If (ParentMenu <> Nil) AND (ParentMenu^.Size.Y = 1)
     Then Begin                                       { Valid parent menu }
       Mouse.X := E.Where.X - ParentMenu^.RawOrigin.X;{ Local x position }
       Mouse.Y := E.Where.Y - ParentMenu^.RawOrigin.Y;{ Local y position }
       ParentMenu^.GetItemRectX(ParentMenu^.Current,R);{ Get item rect }
       MouseInOwner := R.Contains(Mouse);             { Return result }
     End;
   END;

   FUNCTION MouseInMenus: Boolean;
   VAR P: PMenuView;
   BEGIN
     P := ParentMenu;                                 { Parent menu }
     While (P <> Nil) AND NOT P^.MouseInView(E.Where)
       Do P := P^.ParentMenu;                         { Check next menu }
     MouseInMenus := (P <> Nil);                      { Return result }
   END;

   FUNCTION TopMenu: PMenuView;
   VAR P: PMenuView;
   BEGIN
     P := @Self;                                      { Start with self }
     While (P^.ParentMenu <> Nil) Do
       P := P^.ParentMenu;                            { Check next menu }
     TopMenu := P;                                    { Top menu }
   END;

BEGIN
   AutoSelect := False;                               { Clear select flag }
   MouseActive := False;                              { Clear mouse flag }
   Res := 0;                                          { Clear result }
   ItemShown := Nil;                                  { Clear item pointer }
   If (Menu <> Nil) Then Current := Menu^.Default     { Set current item }
     Else Current := Nil;                             { No menu = no current }
   Repeat
     Action := DoNothing;                             { Clear action flag }
     GetEvent(E);                                     { Get next event }
     Case E.What Of
       evMouseDown: If MouseInView(E.Where)           { Mouse in us }
         OR MouseInOwner Then Begin                   { Mouse in owner area }
           TrackMouse;                                { Track the mouse }
           If (Size.Y = 1) Then AutoSelect := True;   { Set select flag }
         End Else Action := DoReturn;                 { Set return action }
       evMouseUp: Begin
           TrackMouse;                                { Track the mouse }
           If MouseInOwner Then                       { Mouse in owner }
             Current := Menu^.Default                 { Set as current }
           Else If (Current <> Nil) AND
           (Current^.Name <> Nil) Then
             Action := DoSelect                       { Set select action }
           Else If MouseActive OR MouseInView(E.Where)
           Then Action := DoReturn                    { Set return action }
           Else Begin
             Current := Menu^.Default;                { Set current item }
             If (Current = Nil) Then
               Current := Menu^.Items;                { Select first item }
             Action := DoNothing;                     { Do nothing action }
           End;
         End;
       evMouseMove: If (E.Buttons <> 0) Then Begin    { Mouse moved }
           TrackMouse;                                { Track the mouse }
           If NOT (MouseInView(E.Where) OR MouseInOwner)
           AND MouseInMenus Then Action := DoReturn;  { Set return action }
         End;
       evKeyDown:
         Case CtrlToArrow(E.KeyCode) Of               { Check arrow keys }
           kbUp, kbDown: If (Size.Y <> 1) Then
             TrackKey(CtrlToArrow(E.KeyCode) = kbDown){ Track keyboard }
             Else If (E.KeyCode = kbDown) Then        { Down arrow }
             AutoSelect := True;                      { Select item }
           kbLeft, kbRight: If (ParentMenu = Nil) Then
             TrackKey(CtrlToArrow(E.KeyCode)=kbRight) { Track keyboard }
             Else Action := DoReturn;                 { Set return action }
           kbHome, kbEnd: If (Size.Y <> 1) Then Begin
               Current := Menu^.Items;                { Set to first item }
               If (E.KeyCode = kbEnd) Then            { If the 'end' key }
                 TrackKey(False);                     { Move to last item }
             End;
           kbEnter: Begin
               If Size.Y = 1 Then AutoSelect := True; { Select item }
               Action := DoSelect;                    { Return the item }
             End;
           kbEsc: Begin
               Action := DoReturn;                    { Set return action }
               If (ParentMenu = Nil) OR
               (ParentMenu^.Size.Y <> 1) Then         { Check parent }
                 ClearEvent(E);                       { Kill the event }
             End;
           Else Target := @Self;                      { Set target as self }
           Ch := GetAltChar(E.KeyCode);
           If (Ch = #0) Then Ch := E.CharCode Else
             Target := TopMenu;                       { Target is top menu }
           P := Target^.FindItem(Ch);                 { Check for item }
           If (P = Nil) Then Begin
             P := TopMenu^.HotKey(E.KeyCode);         { Check for hot key }
             If (P <> Nil) AND                        { Item valid }
             CommandEnabled(P^.Command) Then Begin    { Command enabled }
               Res := P^.Command;                     { Set return command }
               Action := DoReturn;                    { Set return action }
             End
           End Else If Target = @Self Then Begin
             If Size.Y = 1 Then AutoSelect := True;   { Set auto select }
             Action := DoSelect;                      { Select item }
             Current := P;                            { Set current item }
           End Else If (ParentMenu <> Target) OR
           (ParentMenu^.Current <> P) Then            { Item different }
              Action := DoReturn;                     { Set return action }
         End;
       evCommand: If (E.Command = cmMenu) Then Begin  { Menu command }
           AutoSelect := False;                       { Dont select item }
           If (ParentMenu <> Nil) Then
             Action := DoReturn;                      { Set return action }
         End Else Action := DoReturn;                 { Set return action }
     End;
     If (ItemShown <> Current) Then Begin             { New current item }
       OldItem := ItemShown;                          { Hold old item }
       ItemShown := Current;                          { Hold new item }
       SetDrawMask(vdFocus OR vdInner);               { Set the draw mask }
       DrawView;                                      { Redraw the items }
       OldItem := Nil;                                { Clear old item }
     End;
     If (Action = DoSelect) OR ((Action = DoNothing)
     AND AutoSelect) Then                             { Item is selecting }
       If (Current <> Nil) Then With Current^ Do      { Current item valid }
         If (Name <> Nil) Then                        { Item has a name }
           If (Command = 0) Then Begin                { Has no command }
             If (E.What AND (evMouseDown+evMouseMove) <> 0)
               Then PutEvent(E);                      { Put event on queue }
             GetItemRectX(Current, R);                 { Get area of item }
             R.A.X := R.A.X DIV FontWidth + Origin.X; { Left start point }
             R.A.Y := R.B.Y DIV FontHeight + Origin.Y;{ Top start point }
             R.B.X := Owner^.Size.X;                  { X screen area left }
             R.B.Y := Owner^.Size.Y;                  { Y screen area left }
             Target := TopMenu^.NewSubView(R, SubMenu,
               @Self);                                { Create drop menu }
             Res := Owner^.ExecView(Target);          { Execute dropped view }
             Dispose(Target, Done);                   { Dispose drop view }
           End Else If Action = DoSelect Then
             Res := Command;                          { Return result }
     If (Res <> 0) AND CommandEnabled(Res)            { Check command }
     Then Begin
       Action := DoReturn;                            { Return command }
       ClearEvent(E);                                 { Clear the event }
     End Else Res := 0;                               { Clear result }
   Until (Action = DoReturn);
   If (E.What <> evNothing) Then
     If (ParentMenu <> Nil) OR (E.What = evCommand)   { Check event type }
       Then PutEvent(E);                              { Put event on queue }
   If (Current <> Nil) Then Begin
     Menu^.Default := Current;                        { Set new default }
     Current := Nil;                                  { Clear current }
     DrawView;                                        { Redraw the view }
   End;
   Execute := Res;                                    { Return result }
END;

{--TMenuView----------------------------------------------------------------}
{  GetHelpCtx -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 08May98 LdB        }
{---------------------------------------------------------------------------}
FUNCTION TMenuView.GetHelpCtx: Word;
VAR C: PMenuView;
BEGIN
   C := @Self;                                        { Start at self }
   While (C <> Nil) AND ((C^.Current = Nil) OR
   (C^.Current^.HelpCtx = hcNoContext) OR             { Has no context }
   (C^.Current^.Name = Nil)) Do C := C^.ParentMenu;   { Parent menu context }
   If (C<>Nil) Then GetHelpCtx := C^.Current^.HelpCtx { Current context }
     Else GetHelpCtx := hcNoContext;                  { No help context }
END;

{--TMenuView----------------------------------------------------------------}
{  GetPalette -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 15May98 LdB        }
{---------------------------------------------------------------------------}
FUNCTION TMenuView.GetPalette: PPalette;
{$IFDEF PPC_DELPHI3}                                  { DELPHI3+ COMPILER }
CONST P: String = CMenuView;                          { Possible huge string }
{$ELSE}                                               { OTHER COMPILERS }
CONST P: String[Length(CMenuView)] = CMenuView;       { Always normal string }
{$ENDIF}
BEGIN
   GetPalette := @P;                                  { Return palette }
END;

{--TMenuView----------------------------------------------------------------}
{  FindItem -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 11May98 LdB          }
{---------------------------------------------------------------------------}
FUNCTION TMenuView.FindItem (Ch: Char): PMenuItem;
VAR I: Integer; P: PMenuItem;
BEGIN
   Ch := UpCase(Ch);                                  { Upper case of char }
   P := Menu^.Items;                                  { First menu item }
   While (P <> Nil) Do Begin                          { While item valid }
     If (P^.Name <> Nil) AND (NOT P^.Disabled)        { Valid enabled cmd }
     Then Begin
       I := Pos('~', P^.Name^);                       { Scan for highlight }
       If (I <> 0) AND (Ch = UpCase(P^.Name^[I+1]))   { Hotkey char found }
       Then Begin
         FindItem := P;                               { Return item }
         Exit;                                        { Now exit }
       End;
     End;
     P := P^.Next;                                    { Next item }
   End;
   FindItem := Nil;                                   { No item found }
END;

{--TMenuView----------------------------------------------------------------}
{  HotKey -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 11May98 LdB            }
{---------------------------------------------------------------------------}
FUNCTION TMenuView.HotKey (KeyCode: Word): PMenuItem;

   FUNCTION FindHotKey (P: PMenuItem): PMenuItem;
   VAR T: PMenuItem;
   BEGIN
     While (P <> Nil) Do Begin                        { While item valid }
       If (P^.Name <> Nil) Then                       { If valid name }
         If (P^.Command = 0) Then Begin               { Valid command }
           T := FindHotKey(P^.SubMenu^.Items);        { Search for hot key }
           If (T <> Nil) Then Begin
             FindHotKey := T;                         { Return hotkey }
             Exit;                                    { Now exit }
           End;
         End Else If NOT P^.Disabled AND              { Hotkey is enabled }
         (P^.KeyCode <> kbNoKey) AND                  { Valid keycode }
         (P^.KeyCode = KeyCode) Then Begin            { Key matches request }
           FindHotKey := P;                           { Return hotkey code }
           Exit;                                      { Exit }
         End;
         P := P^.Next;                                { Next item }
     End;
     FindHotKey := Nil;                               { No item found }
   END;

BEGIN
   HotKey := FindHotKey(Menu^.Items);                 { Hot key function }
END;

{--TMenuView----------------------------------------------------------------}
{  NewSubView -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 11May98 LdB        }
{---------------------------------------------------------------------------}
FUNCTION TMenuView.NewSubView (Var Bounds: TRect; AMenu: PMenu;
  AParentMenu: PMenuView): PMenuView;
BEGIN
   NewSubView := New(PMenuBox, Init(Bounds, AMenu,
     AParentMenu));                                   { Create a menu box }
END;

{--TMenuView----------------------------------------------------------------}
{  Store -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 11May98 LdB             }
{---------------------------------------------------------------------------}
PROCEDURE TMenuView.Store (Var S: TStream);

   PROCEDURE DoStoreMenu (Menu: PMenu);
   VAR Item: PMenuItem; Tok: Byte;
   BEGIN
     Tok := $FF;                                      { Preset max count }
     Item := Menu^.Items;                             { Start first item }
     While (Item <> Nil) Do Begin
       With Item^ Do Begin
         S.Write(Tok, 1);                             { Write tok value }
         S.WriteStr(Name);                            { Write item name }
         S.Write(Command, 2);                         { Menu item command }
         S.Write(Disabled, 1);                        { Menu item state }
         S.Write(KeyCode, 2);                         { Menu item keycode }
         S.Write(HelpCtx, 2);                         { Menu item help ctx }
         If (Name <> Nil) Then
           If Command = 0 Then DoStoreMenu(SubMenu)
           Else S.WriteStr(Param);                    { Write parameter }
       End;
       Item := Item^.Next;                            { Next item }
     End;
     Tok := 0;                                        { Clear tok count }
     S.Write(Tok, 1);                                 { Write tok value }
   END;

BEGIN
   TView.Store(S);                                    { TView.Store called }
   DoStoreMenu(Menu);                                 { Store menu items }
END;

{--TMenuView----------------------------------------------------------------}
{  HandleEvent -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 08May98 LdB       }
{---------------------------------------------------------------------------}
PROCEDURE TMenuView.HandleEvent (Var Event: TEvent);
VAR CallDraw: Boolean; P: PMenuItem;

   PROCEDURE UpdateMenu (Menu: PMenu);
   VAR P: PMenuItem; CommandState: Boolean;
   BEGIN
     P := Menu^.Items;                                { Start on first item }
     While (P <> Nil) Do Begin
       If (P^.Name <> Nil) Then                       { Valid name }
       If (P^.Command = 0) Then UpdateMenu(P^.SubMenu){ Update menu }
       Else Begin
         CommandState := CommandEnabled(P^.Command);  { Menu item state }
         If (P^.Disabled = CommandState) Then Begin
           P^.Disabled := NOT CommandState;           { Disable item }
           CallDraw := True;                          { Must draw }
         End;
       End;
       P := P^.Next;                                  { Next item }
     End;
   END;

   PROCEDURE DoSelect;
   BEGIN
     PutEvent(Event);                                 { Put event on queue }
     Event.Command := Owner^.ExecView(@Self);         { Execute view }
     If (Event.Command <> 0) AND
     CommandEnabled(Event.Command) Then Begin
       Event.What := evCommand;                       { Command event }
       Event.InfoPtr := Nil;                          { Clear info ptr }
       PutEvent(Event);                               { Put event on queue }
     End;
     ClearEvent(Event);                               { Clear the event }
   END;

BEGIN
   If (Menu <> Nil) Then
     Case Event.What Of
       evMouseDown: DoSelect;                         { Select menu item }
       evKeyDown:
         If (FindItem(GetAltChar(Event.KeyCode)) <> Nil)
         Then DoSelect Else Begin                     { Select menu item }
           P := HotKey(Event.KeyCode);                { Check for hotkey }
           If (P <> Nil) AND
           (CommandEnabled(P^.Command)) Then Begin
             Event.What := evCommand;                 { Command event }
             Event.Command := P^.Command;             { Set command event }
             Event.InfoPtr := Nil;                    { Clear info ptr }
             PutEvent(Event);                         { Put event on queue }
             ClearEvent(Event);                       { Clear the event }
           End;
         End;
       evCommand:
         If Event.Command = cmMenu Then DoSelect;     { Select menu item }
       evBroadcast:
         If (Event.Command = cmCommandSetChanged)     { Commands changed }
         Then Begin
           CallDraw := False;                         { Preset no redraw }
           UpdateMenu(Menu);                          { Update menu }
           If CallDraw Then DrawView;                 { Redraw if needed }
         End;
     End;
END;

{--TMenuView----------------------------------------------------------------}
{  GetItemRectX -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 08May98 LdB       }
{---------------------------------------------------------------------------}
PROCEDURE TMenuView.GetItemRectX (Item: PMenuItem; Var R: TRect);
BEGIN                                                 { Abstract method }
END;

{--TMenuView----------------------------------------------------------------}
{  GetItemRect -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 08May98 LdB       }
{---------------------------------------------------------------------------}
PROCEDURE TMenuView.GetItemRect (Item: PMenuItem; Var R: TRect);
BEGIN
  GetItemRectX(Item,R);
  R.A.X:=R.A.X div SysFontWidth;
  R.A.Y:=R.A.Y div SysFontHeight;
  R.B.X:=R.B.X div SysFontWidth;
  R.B.Y:=R.B.Y div SysFontHeight;
END;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                        TMenuBar OBJECT METHODS                            }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{--TMenuBar-----------------------------------------------------------------}
{  Init -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 08May98 LdB              }
{---------------------------------------------------------------------------}
CONSTRUCTOR TMenuBar.Init (Var Bounds: TRect; AMenu: PMenu);
BEGIN
   Inherited Init(Bounds);                            { Call ancestor }
   GrowMode := gfGrowHiX;                             { Set grow mode }
   Menu := AMenu;                                     { Hold menu item }
   Options := Options OR ofPreProcess;                { Preprocessing view }
END;

{--TMenuBar-----------------------------------------------------------------}
{  Done -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 08May98 LdB              }
{---------------------------------------------------------------------------}
DESTRUCTOR TMenuBar.Done;
BEGIN
   If (Menu <> Nil) Then DisposeMenu(Menu);           { Dispose menu items }
   Inherited Done;                                    { Call ancestor }
END;

{--TMenuBar-----------------------------------------------------------------}
{  Draw -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 08May98 LdB              }
{---------------------------------------------------------------------------}
PROCEDURE TMenuBar.Draw;
VAR I, J, K, L, CNormal, CSelect, CNormDisabled, CSelDisabled, Color: Word;
    P: PMenuItem; B: TDrawBuffer;
BEGIN
   CNormal := GetColor($0301);                        { Normal colour }
   CSelect := GetColor($0604);                        { Select colour }
   CNormDisabled := GetColor($0202);                  { Disabled colour }
   CSelDisabled := GetColor($0505);                   { Select disabled }
   If (Menu <> Nil) Then Begin                        { Valid menu }
     I := 0;                                          { Set start position }
     P := Menu^.Items;                                { First item }
     While (P <> Nil) Do Begin
       If (P^.Name <> Nil) Then Begin                 { Name valid }
         J := CStrLen(P^.Name^) + 2;                  { Name string length }
         If (P = OldItem) OR (P = Current) Then Begin { Matches a state }
           If P^.Disabled Then Begin
             If (P = Current) Then
               Color := CSelDisabled                  { Select disabled }
               Else Color := CNormDisabled            { Normal disabled }
           End Else Begin
             If (P = Current) Then Color := CSelect   { Select colour }
               Else Color := CNormal;                 { Normal colour }
           End;
           MoveCStr(B, ' '+P^.Name^+' ', Color);      { Name to buffer }
           WriteBuf(I, 0, J, 1, B);                   { Write the string }
           K := I*FontWidth;                          { X start position }
           L := K + TextWidth(' '+P^.Name^+' ');      { X end position }
           If AdvancedMenus Then Begin
             GraphLine(K, 0, L, 0, White);            { Redraw top line }
             GraphLine(K, FontHeight-1, L,
               FontHeight-1, DarkGray);               { Redraw lower line }
           End;
         End;
         Inc(I, J);                                   { Advance position }
       End;
       P := P^.Next;                                  { Next item }
     End;
   End;
END;

{--TMenuBar-----------------------------------------------------------------}
{  DrawBackGround -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 08May98 LdB    }
{---------------------------------------------------------------------------}
PROCEDURE TMenuBar.DrawBackGround;
VAR I, J, CNormal, CSelect, CNormDisabled, CSelDisabled, Color: Word;
    P: PMenuItem; B: TDrawBuffer;
BEGIN
   Inherited DrawBackGround;                          { Call ancestor }
   CNormal := GetColor($0301);                        { Normal colour }
   CSelect := GetColor($0604);                        { Select colour }
   CNormDisabled := GetColor($0202);                  { Disabled colour }
   CSelDisabled := GetColor($0505);                   { Select disabled }
   If (Menu <> Nil) Then Begin                        { Valid menu }
     I := 0;                                          { Set start position }
     P := Menu^.Items;                                { First item }
     While (P <> Nil) Do Begin
       If (P^.Name <> Nil) Then Begin                 { Name valid }
         If P^.Disabled Then Begin
           If (P = Current) Then Color := CSelDisabled{ Select disabled }
             Else Color := CNormDisabled              { Normal disabled }
         End Else Begin
           If (P = Current) Then Color := CSelect     { Select colour }
             Else Color := CNormal;                   { Normal colour }
         End;
         J := CStrLen(P^.Name^) + 2;                  { Length of string }
         MoveCStr(B, ' '+P^.Name^+' ', Color);        { Name to buffer }
         WriteBuf(I, 0, J, 1, B);                     { Write the string }
         Inc(I, J);                                   { Advance position }
       End;
       P := P^.Next;                                  { Next item }
     End;
   End;
   If AdvancedMenus Then BiColorRectangle(0, 0,
     RawSize.X, RawSize.Y, White, DarkGray, False);   { Draw 3d effect }
END;

{--TMenuBar-----------------------------------------------------------------}
{  GetItemRectX -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 08May98 LdB      }
{---------------------------------------------------------------------------}
PROCEDURE TMenuBar.GetItemRectX (Item: PMenuItem; Var R: TRect);
VAR I: Integer; P: PMenuItem;
BEGIN
   I := 0;                                            { Preset to zero }
   R.Assign(0, 0, 0, FontHeight);                     { Initial rect size }
   P := Menu^.Items;                                  { First item }
   While (P <> Nil) Do Begin                          { While valid item }
     R.A.X := I*FontWidth;                            { Move area along }
     If (P^.Name <> Nil) Then Begin                   { Valid name }
       R.B.X := R.A.X+TextWidth(' ' + P^.Name^ + ' ');{ Add text width  }
       I := I + CStrLen(P^.Name^) + 2;                { Add item length }
     End Else R.B.X := R.A.X;
     If (P = Item) Then break;                        { Requested item found }
     P := P^.Next;                                    { Next item }
   End;
END;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                          TMenuBox OBJECT METHODS                          }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{--TMenuBox-----------------------------------------------------------------}
{  Init -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 11May98 LdB              }
{---------------------------------------------------------------------------}
CONSTRUCTOR TMenuBox.Init (Var Bounds: TRect; AMenu: PMenu;
  AParentMenu: PMenuView);
VAR W, H, L: Integer; S: String; P: PMenuItem; R: TRect;
BEGIN
   W := 0;                                            { Clear initial width }
   H := 2;                                            { Set initial height }
   If (AMenu <> Nil) Then Begin                       { Valid menu }
     P := AMenu^.Items;                               { Start on first item }
     While (P <> Nil) Do Begin                        { If item valid }
       If (P^.Name <> Nil) Then Begin                 { Check for name }
         S := ' ' + P^.Name^ + ' ';                   { Transfer string }
         If (P^.Command <> 0) AND (P^.Param <> Nil)
           Then S := S + ' - ' + P^.Param^;           { Add any parameter }
       End;
       L := TextWidth(S);                             { Width of string }
       If (L > W) Then W := L;                        { Hold maximum }
       Inc(H);                                        { Inc count of items }
       P := P^.Next;                                  { Move to next item }
     End;
   End;
   W := 5 + (W DIV FontWidth);                        { Longest text width }
   R.Copy(Bounds);                                    { Copy the bounds }
   If (R.A.X + W < R.B.X) Then R.B.X := R.A.X + W     { Shorten if possible }
     Else R.A.X := R.B.X - W;                         { Insufficent space }
   R.B.X := R.A.X + W;
   If (R.A.Y + H < R.B.Y) Then R.B.Y := R.A.Y + H     { Shorten if possible }
     Else R.A.Y := R.B.Y - H;                         { Insufficent height }
   Inherited Init(R);                                 { Call ancestor }
   State := State OR sfShadow;                        { Set shadow state }
   Options := Options OR ofPreProcess;                { View pre processes }
   if TextModeGFV then
     Options := Options OR ofFramed;
   Menu := AMenu;                                     { Hold menu }
   ParentMenu := AParentMenu;                         { Hold parent }
END;

{--TMenuBox-----------------------------------------------------------------}
{  Draw -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 11May98 LdB              }
{---------------------------------------------------------------------------}
PROCEDURE TMenuBox.Draw;
VAR CNormal, CSelect, CSelectDisabled, CDisabled, Color: Word; Index, Tx, Ty, Y: Integer;
    S: String; P: PMenuItem; B: TDrawBuffer;
Type
   FrameLineType = (UpperLine,NormalLine,SeparationLine,LowerLine);
   FrameLineChars = Array[0..2] of char;
Const
   FrameLines : Array[FrameLineType] of FrameLineChars =
     ('ÚÄ¿','³ ³','ÃÄ´','ÀÄÙ');
  Procedure CreateBorder(LineType : FrameLineType);
  Begin
    MoveChar(B, ' ', CNormal, 1);
    MoveChar(B[1], FrameLines[LineType][0], CNormal, 1);
    MoveChar(B[2], FrameLines[LineType][1], Color, Size.X-4);
    MoveChar(B[Size.X-2], FrameLines[LineType][2], CNormal, 1);
    MoveChar(B[Size.X-1], ' ', CNormal, 1);
  End;


BEGIN
   CNormal := GetColor($0301);                        { Normal colour }
   CSelect := GetColor($0604);                        { Selected colour }
   CDisabled := GetColor($0202);                      { Disabled colour }
   CSelectDisabled := GetColor($0505);                { Selected, but disabled }
   If TextModeGFV then
     Begin
       Color := CNormal;                              { Normal colour }
       CreateBorder(UpperLine);
       WriteBuf(0, 0, Size.X, 1, B);                  { Write the line }
     End;
   Y := 1;
   If (Menu <> Nil) Then Begin                        { We have a menu }
     P := Menu^.Items;                                { Start on first }
     While (P <> Nil) Do Begin                        { Valid menu item }
       Color := CNormal;                              { Normal colour }
       If (P^.Name <> Nil) Then Begin                 { Item has text }
         If P^.Disabled Then
           begin
             if (P = Current) then
               Color := CSelectDisabled
             else
               Color := CDisabled; { Is item disabled }
           end
         else
           If (P = Current) Then Color := CSelect;    { Select colour }
         If TextModeGFV or UseFixedFont then
           Begin
             If Not TextModeGFV then
               MoveChar(B, ' ', Color, Size.X);    { Clear buffer }
             If TextModeGFV  then
               CreateBorder(NormalLine);
             Index:=2;
           End
         Else
           Begin
             MoveChar(B, ' ', Color, Size.X-4);    { Clear buffer }
             Index:=0;
           End;
         S := ' ' + P^.Name^ + ' ';                   { Menu string }
         MoveCStr(B[Index], S, Color);                { Transfer string }
        if P^.Command = 0 then
          MoveChar(B[Size.X - 4],SubMenuChar[LowAscii],
            Byte(Color), 1) else
         If (P^.Command <> 0) AND(P^.Param <> Nil)
         Then Begin
           if TextModeGFV or UseFixedFont then
            MoveCStr(B[Size.X - 3 - Length(P^.Param^)], P^.Param^, Color)  { Add param chars }
           else
            S := S + ' - ' + P^.Param^;                { Add to string }
         End;
         If (OldItem = Nil) OR (OldItem = P) OR
         (Current = P) Then Begin                     { We need to fix draw }
           If TextModeGFV or UseFixedFont then
             Begin
               if TextModeGFV then
                 WriteBuf(0, Y, Size.X, 1, B)             { Write the whole line }
               else
                 WriteBuf(1, Y, Size.X-2, 1, B[1]);
             end
           Else
             WriteBuf(2, Y, CStrLen(S), 1, B);          { Write the line }
           If (P = Current) Then Begin                { Selected item }
             Tx := 2 * FontWidth;                     { X offset }
             Ty := Y * FontHeight;                    { Y offset }
             BicolorRectangle(Tx, Ty, Tx + TextWidth(S)
               - 1, Ty + FontHeight - 1, White,
               DarkGray, False);                      { Draw higlight box }
           End;
         End;
       End Else Begin { no text NewLine }
         Color := CNormal;                              { Normal colour }
         If TextModeGFV then
           Begin
             CreateBorder(SeparationLine);
             WriteBuf(0, Y, Size.X, 1, B);                { Write the line }
           End;
       End;
       Inc(Y);                                        { Next line down }
       P := P^.Next;                                  { fetch next item }
     End;
   End;
   If TextModeGFV then
     Begin
       Color := CNormal;                              { Normal colour }
       CreateBorder(LowerLine);
       WriteBuf(0, Size.Y-1, Size.X, 1, B);                  { Write the line }
     End;
END;

{--TMenuBox-----------------------------------------------------------------}
{  DrawBackGround -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 11May98 LdB    }
{---------------------------------------------------------------------------}
PROCEDURE TMenuBox.DrawBackGround;
VAR X, Y, Y2: Integer; P : PMenuItem;
BEGIN
   Y2 := FontHeight DIV 2;                            { Intra offset }
   Y := FontHeight;                                   { Initial position }
   X := 3*FontWidth;                                  { 2 offset }
   Inherited DrawBackGround;                          { Call ancestor }
   If (Menu <> Nil) Then Begin                        { We have a menu }
     P := Menu^.Items;                                { Start on first }
     While (P <> Nil) Do Begin
       If (P^.Name = Nil) Then                        { Item has no string }
         BiColorRectangle(X, Y+Y2, RawSize.X-X,
           Y+Y2+1, White, DarkGray, True);            { Draw 3d line effect }
       Inc(Y, FontHeight);                            { Down one line }
       P := P^.Next;                                  { Next item now }
     End;
   End;
   BiColorRectangle(3, 3, RawSize.X-3, RawSize.Y-3,
     White, DarkGray, False);                         { Draw 3d effect }
   BiColorRectangle(5, 5, RawSize.X-5, RawSize.Y-5,
    White, DarkGray, True);                           { Draw 3d effect }
   BiColorRectangle(0, 0, RawSize.X, RawSize.Y,
     White, DarkGray, False);                         { Draw 3d effect }
END;

{--TMenuBox-----------------------------------------------------------------}
{  GetItemRectX -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 11May98 LdB      }
{---------------------------------------------------------------------------}
PROCEDURE TMenuBox.GetItemRectX (Item: PMenuItem; Var R: TRect);
VAR X, Y: Integer; P: PMenuItem;
BEGIN
   Y := FontHeight;                                   { Initial y position }
   P := Menu^.Items;                                  { Initial item }
   While (P <> Item) Do Begin                         { Valid item }
     Inc(Y, FontHeight);                              { Inc position }
     P := P^.Next;                                    { Next item }
   End;
   X := 2 * FontWidth;                                { Left/Right margin }
   R.Assign(X, Y, RawSize.X - X, Y + FontHeight);     { Assign area }
END;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                        TMenuPopUp OBJECT METHODS                          }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{--TMenuPopUp---------------------------------------------------------------}
{  Init -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 15May98 LdB              }
{---------------------------------------------------------------------------}
CONSTRUCTOR TMenuPopup.Init (Var Bounds: TRect; AMenu: PMenu);
BEGIN
   Inherited Init(Bounds, AMenu, Nil);                { Call ancestor }
END;

{--TMenuPopUp---------------------------------------------------------------}
{  Done -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 15May98 LdB              }
{---------------------------------------------------------------------------}
DESTRUCTOR TMenuPopup.Done;
BEGIN
   If (Menu <> Nil) Then DisposeMenu(Menu);           { Dispose menu items }
   Inherited Done;                                    { Call ancestor }
END;

{--TMenuPopUp---------------------------------------------------------------}
{  HandleEvent -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 15May98 LdB       }
{---------------------------------------------------------------------------}
PROCEDURE TMenuPopup.HandleEvent (Var Event: TEvent);
VAR P: PMenuItem;
BEGIN
   Case Event.What Of
     evKeyDown: Begin
         P := FindItem(GetCtrlChar(Event.KeyCode));   { Find the item }
         If (P = Nil) Then P := HotKey(Event.KeyCode);{ Try hot key }
         If (P <> Nil) AND (CommandEnabled(P^.Command))
         Then Begin                                   { Command valid }
           Event.What := evCommand;                   { Command event }
           Event.Command := P^.Command;               { Set command value }
           Event.InfoPtr := Nil;                      { Clear info ptr }
           PutEvent(Event);                           { Put event on queue }
           ClearEvent(Event);                         { Clear the event }
         End Else If (GetAltChar(Event.KeyCode) <> #0)
           Then ClearEvent(Event);                    { Clear the event }
       End;
   End;
   Inherited HandleEvent(Event);                      { Call ancestor }
END;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                        TStatusLine OBJECT METHODS                         }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{--TStatusLine--------------------------------------------------------------}
{  Init -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 11May98 LdB              }
{---------------------------------------------------------------------------}
CONSTRUCTOR TStatusLine.Init (Var Bounds: TRect; ADefs: PStatusDef);
BEGIN
   Inherited Init(Bounds);                            { Call ancestor }
   Options := Options OR ofPreProcess;                { Pre processing view }
   EventMask := EventMask OR evBroadcast;             { See broadcasts }
   GrowMode := gfGrowLoY + gfGrowHiX + gfGrowHiY;     { Set grow modes }
   Defs := ADefs;                                     { Set default items }
   FindItems;                                         { Find the items }
END;

{--TStatusLine--------------------------------------------------------------}
{  Load -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 11May98 LdB              }
{---------------------------------------------------------------------------}
CONSTRUCTOR TStatusLine.Load (Var S: TStream);

   FUNCTION DoLoadStatusItems: PStatusItem;
   VAR Count: Integer; Cur, First: PStatusItem; Last: ^PStatusItem;
   BEGIN
     Cur := Nil;                                      { Preset nil }
     Last := @First;                                  { Start on first item }
     S.Read(Count, 2);                                { Read count }
     While (Count > 0) Do Begin
       New(Cur);                                      { New status item }
       Last^ := Cur;                                  { First chain part }
       If (Cur <> Nil) Then Begin                     { Check pointer valid }
         Last := @Cur^.Next;                          { Chain complete }
         Cur^.Text := S.ReadStr;                      { Read item text }
         S.Read(Cur^.KeyCode, 2);                     { Keycode of item }
         S.Read(Cur^.Command, 2);                     { Command of item }
       End;
       Dec(Count);                                    { One item loaded }
     End;
     Last^ := Nil;                                    { Now chain end }
     DoLoadStatusItems := First;                      { Return the list }
   END;

   FUNCTION DoLoadStatusDefs: PStatusDef;
   VAR Count: Integer; Cur, First: PStatusDef; Last: ^PStatusDef;
   BEGIN
     Last := @First;                                  { Start on first }
     S.Read(Count, 2);                                { Read item count }
     While (Count > 0) Do Begin
       New(Cur);                                      { New status def }
       Last^ := Cur;                                  { First part of chain }
       If (Cur <> Nil) Then Begin                     { Check pointer valid }
         Last := @Cur^.Next;                          { Chain complete }
         S.Read(Cur^.Min, 2);                         { Read min data }
         S.Read(Cur^.Max, 2);                         { Read max data }
         Cur^.Items := DoLoadStatusItems;             { Set pointer }
       End;
       Dec(Count);                                    { One item loaded }
     End;
     Last^ := Nil;                                    { Now chain ends }
     DoLoadStatusDefs := First;                       { Return item list }
   END;

BEGIN
   Inherited Load(S);                                 { Call ancestor }
   Defs := DoLoadStatusDefs;                          { Retreive items }
   FindItems;                                         { Find the items }
END;

{--TStatusLine--------------------------------------------------------------}
{  Done -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 11May98 LdB              }
{---------------------------------------------------------------------------}
DESTRUCTOR TStatusLine.Done;
VAR T: PStatusDef;

   PROCEDURE DisposeItems (Item: PStatusItem);
   VAR T: PStatusItem;
   BEGIN
     While (Item <> Nil) Do Begin                     { Item to dispose }
       T := Item;                                     { Hold pointer }
       Item := Item^.Next;                            { Move down chain }
       DisposeStr(T^.Text);                           { Dispose string }
       Dispose(T);                                    { Dispose item }
     End;
   END;

BEGIN
   While (Defs <> Nil) Do Begin
     T := Defs;                                       { Hold pointer }
     Defs := Defs^.Next;                              { Move down chain }
     DisposeItems(T^.Items);                          { Dispose the item }
     Dispose(T);                                      { Dispose status item }
   End;
   Inherited Done;                                    { Call ancestor }
END;


{--TStatusLine--------------------------------------------------------------}
{  GetPalette -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 11May98 LdB        }
{---------------------------------------------------------------------------}
FUNCTION TStatusLine.GetPalette: PPalette;
{$IFDEF PPC_DELPHI3}                                  { DELPHI3+ COMPILER }
CONST P: String = CStatusLine;                        { Possible huge string }
{$ELSE}                                               { OTHER COMPILERS }
CONST P: String[Length(CStatusLine)] = CStatusLine;   { Always normal string }
{$ENDIF}
BEGIN
   GetPalette := @P;                                  { Return palette }
END;

{--TStatusLine--------------------------------------------------------------}
{  Hint -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 11May98 LdB              }
{---------------------------------------------------------------------------}
FUNCTION TStatusLine.Hint (AHelpCtx: Word): String;
BEGIN
   Hint := '';                                        { Return nothing }
END;

{--TStatusLine--------------------------------------------------------------}
{  Draw -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 11May98 LdB              }
{---------------------------------------------------------------------------}
PROCEDURE TStatusLine.Draw;
BEGIN
   DrawSelect(Nil);                                   { Call draw select }
END;

{--TStatusLine--------------------------------------------------------------}
{  Update -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 15May98 LdB            }
{---------------------------------------------------------------------------}
PROCEDURE TStatusLine.Update;
VAR H: Word; P: PView;
BEGIN
   P := TopView;                                      { Get topmost view }
   If (P <> Nil) Then H := P^.GetHelpCtx Else         { Top views context }
     H := hcNoContext;                                { No context }
   If (HelpCtx <> H) Then Begin                       { Differs from last }
     HelpCtx := H;                                    { Hold new context }
     FindItems;                                       { Find the item }
     DrawView;                                        { Redraw the view }
   End;
END;

{--TStatusLine--------------------------------------------------------------}
{  Store -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 11May98 LdB             }
{---------------------------------------------------------------------------}
PROCEDURE TStatusLine.Store (Var S: TStream);

   PROCEDURE DoStoreStatusItems (Cur: PStatusItem);
   VAR Count: Integer; T: PStatusItem;
   BEGIN
     Count := 0;                                      { Clear count }
     T := Cur;                                        { Start on current }
     While (T <> Nil) Do Begin
       Inc(Count);                                    { Count items }
       T := T^.Next;                                  { Next item }
     End;
     S.Write(Count, 2);                               { Write item count }
     While (Cur <> Nil) Do Begin
       S.WriteStr(Cur^.Text);                         { Store item text }
       S.Write(Cur^.KeyCode, 2);                      { Keycode of item }
       S.Write(Cur^.Command, 2);                      { Command of item }
       Cur := Cur^.Next;                              { Move to next item }
     End;
   END;

   PROCEDURE DoStoreStatusDefs (Cur: PStatusDef);
   VAR Count: Integer; T: PStatusDef;
   BEGIN
     Count := 0;                                      { Clear count }
     T := Cur;                                        { Current status item }
     While (T <> Nil) Do Begin
       Inc(Count);                                    { Count items }
       T := T^.Next                                   { Next item }
     End;
     S.Write(Count, 2);                               { Write item count }
     While (Cur <> Nil) Do Begin
       With Cur^ Do Begin
         S.Write(Cur^.Min, 2);                        { Write min data }
         S.Write(Cur^.Max, 2);                        { Write max data }
         DoStoreStatusItems(Items);                   { Store the items }
       End;
       Cur := Cur^.Next;                              { Next status item }
     End;
   END;

BEGIN
   TView.Store(S);                                    { TView.Store called }
   DoStoreStatusDefs(Defs);                           { Store status items }
END;

{--TStatusLine--------------------------------------------------------------}
{  HandleEvent -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 11May98 LdB       }
{---------------------------------------------------------------------------}
PROCEDURE TStatusLine.HandleEvent (Var Event: TEvent);
VAR Mouse: TPoint; T, Tt: PStatusItem;

   FUNCTION ItemMouseIsIn: PStatusItem;
   VAR X, Xi: Word; T: PStatusItem;
   BEGIN
     ItemMouseIsIn := Nil;                            { Preset fail }
     If (Mouse.Y < 0) OR (Mouse.Y > FontHeight)       { Outside view height }
       Then Exit;                                     { Not in view exit }
     X := 0;                                          { Zero x position }
     T := Items;                                      { Start at first item }
     While (T <> Nil) Do Begin                        { While item valid }
       If (T^.Text <> Nil) Then Begin                 { Check valid text }
         Xi := X;                                     { Hold initial x value }
         X := Xi + TextWidth(' ' + T^.Text^ + ' ');   { Add text width }
         If (Mouse.X >= Xi) AND (Mouse.X < X)
         Then Begin
           ItemMouseIsIn := T;                        { Selected item }
           Exit;                                      { Now exit }
         End;
       End;
       T := T^.Next;                                  { Next item }
     End;
   END;

BEGIN
   Inherited HandleEvent(Event);                      { Call ancestor }
   Case Event.What Of
     evMouseDown: Begin
         T := Nil;                                    { Preset ptr to nil }
         Repeat
           Mouse.X := Event.Where.X - RawOrigin.X;    { Local x position }
           Mouse.Y := Event.Where.Y - RawOrigin.Y;    { Local y position }
           Tt := ItemMouseIsIn;                       { Find selected item }
           If (T <> Tt) Then                          { Item has changed }
             DrawSelect(Tt);                          { Draw new item }
           T := Tt                                    { Transfer item }
         Until NOT MouseEvent(Event, evMouseMove);    { Mouse stopped moving }
         If (T <> Nil) AND CommandEnabled(T^.Command) { Check cmd enabled }
         Then Begin
           Event.What := evCommand;                   { Command event }
           Event.Command := T^.Command;               { Set command value }
           Event.InfoPtr := Nil;                      { No info ptr }
           PutEvent(Event);                           { Put event on queue }
         End;
         ClearEvent(Event);                           { Clear the event }
         DrawSelect(Nil);                             { Clear the highlight }
       End;
     evKeyDown: Begin                                 { Key down event }
         T := Items;                                  { Start on first item }
         While (T <> Nil) Do Begin                    { For each valid item }
           If (Event.KeyCode = T^.KeyCode) AND        { Check for hot key }
           CommandEnabled(T^.Command) Then Begin      { Check cmd enabled }
             Event.What := evCommand;                 { Change to command }
             Event.Command := T^.Command;             { Set command value }
             Event.InfoPtr := Nil;                    { Clear info ptr }
             Exit;                                    { Now exit }
           End;
           T := T^.Next;                              { Next item }
         End;
       End;
     evBroadcast:
       If (Event.Command = cmCommandSetChanged) Then  { Command set change }
         DrawView;                                    { Redraw view }
   End;
END;

{***************************************************************************}
{                    TStatusLine OBJECT PRIVATE METHODS                     }
{***************************************************************************}

{--TStatusLine--------------------------------------------------------------}
{  FindItems -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 11May98 LdB         }
{---------------------------------------------------------------------------}
PROCEDURE TStatusLine.FindItems;
VAR P: PStatusDef;
BEGIN
   P := Defs;                                         { First status item }
   While (P <> Nil) AND ((HelpCtx < P^.Min) OR
   (HelpCtx > P^.Max)) Do P := P^.Next;               { Find status item }
   If (P = Nil) Then Items := Nil Else
     Items := P^.Items;                               { Return found item }
END;

{--TStatusLine--------------------------------------------------------------}
{  DrawSelect -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 11May98 LdB        }
{---------------------------------------------------------------------------}
PROCEDURE TStatusLine.DrawSelect (Selected: PStatusItem);
VAR I, L: Integer; Color, CSelect, CNormal, CSelDisabled, CNormDisabled: Word;
    HintBuf: String; B: TDrawBuffer; T: PStatusItem;
BEGIN
   CNormal := GetColor($0301);                        { Normal colour }
   CSelect := GetColor($0604);                        { Select colour }
   CNormDisabled := GetColor($0202);                  { Disabled colour }
   CSelDisabled := GetColor($0505);                   { Select disabled }
   MoveChar(B, ' ', Byte(CNormal), 80);               { Clear the buffer }
   T := Items;                                        { First item }
   I := 0;                                            { Clear the count }
   L := 0;
   While (T <> Nil) Do Begin                          { While valid item }
     If (T^.Text <> Nil) Then Begin                   { While valid text }
       L := CStrLen(' '+T^.Text^+' ');                { Text length }
       If CommandEnabled(T^.Command) Then Begin       { Command enabled }
         If T = Selected Then Color := CSelect        { Selected colour }
           Else Color := CNormal                      { Normal colour }
       End Else
         If T = Selected Then Color := CSelDisabled   { Selected disabled }
           Else Color := CNormDisabled;               { Disabled colour }
       MoveCStr(B[I], ' '+T^.Text^+' ', Color);       { Move text to buf }
       Inc(I, L);                                     { Advance position }
     End;
     T := T^.Next;                                    { Next item }
   End;
   HintBuf := Hint(HelpCtx);                          { Get hint string }
   If (HintBuf <> '') Then Begin                      { Hint present }
     {$IFNDEF OS_WINDOWS}
     MoveChar(B[I], #179, Byte(CNormal), 1);          { '|' char to buffer }
     {$ELSE}
     MoveChar(B[I], #166, Byte(CNormal), 1);          { '|' char to buffer }
     {$ENDIF}
     Inc(I, 2);                                       { Move along }
     MoveStr(B[I], HintBuf, Byte(CNormal));           { Move hint to buffer }
     I := I + Length(HintBuf);                        { Hint length }
   End;
   WriteLine(0, 0, I, 1, B);                          { Write the buffer }
   If AdvancedMenus Then BicolorRectangle(0, 0,
     RawSize.X, RawSize.Y, White, DarkGray, False);   { Add 3d effect }
END;

{***************************************************************************}
{                            INTERFACE ROUTINES                             }
{***************************************************************************}

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                           MENU INTERFACE ROUTINES                         }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{---------------------------------------------------------------------------}
{  NewMenu -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 14May98 LdB           }
{---------------------------------------------------------------------------}
FUNCTION NewMenu (Items: PMenuItem): PMenu;
VAR P: PMenu;
BEGIN
   New(P);                                            { Create new menu }
   If (P <> Nil) Then Begin                           { Check valid pointer }
     P^.Items := Items;                               { Hold item list }
     P^.Default := Items;                             { Set default item }
   End;
   NewMenu := P;                                      { Return menu }
END;

{---------------------------------------------------------------------------}
{  DisposeMenu -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 14May98 LdB       }
{---------------------------------------------------------------------------}
PROCEDURE DisposeMenu (Menu: PMenu);
VAR P, Q: PMenuItem;
BEGIN
   If (Menu <> Nil) Then Begin                        { Valid menu item }
     P := Menu^.Items;                                { First item in list }
     While (P <> Nil) Do Begin                        { Item is valid }
       If (P^.Name <> Nil) Then Begin                 { Valid name pointer }
         DisposeStr(P^.Name);                         { Dispose of name }
         If (P^.Command <> 0) Then
           DisposeStr(P^.Param) Else                  { Dispose parameter }
           DisposeMenu(P^.SubMenu);                   { Dispose submenu }
       End;
       Q := P;                                        { Hold pointer }
       P := P^.Next;                                  { Move to next item }
       Dispose(Q);                                    { Dispose of item }
     End;
     Dispose(Menu);                                   { Dispose of menu }
   End;
END;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                             MENU ITEM ROUTINES                            }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{---------------------------------------------------------------------------}
{  NewLine -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 14May98 LdB           }
{---------------------------------------------------------------------------}
FUNCTION NewLine (Next: PMenuItem): PMenuItem;
VAR P: PMenuItem;
BEGIN
   New(P);                                            { Allocate memory }
   If (P <> Nil) Then Begin                           { Check valid pointer }
     P^.Next := Next;                                 { Hold next menu item }
     P^.Name := Nil;                                  { Clear name ptr }
     P^.HelpCtx := hcNoContext;                       { Clear help context }
   End;
   NewLine := P;                                      { Return new line }
END;

{---------------------------------------------------------------------------}
{  NewItem -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 14May98 LdB           }
{---------------------------------------------------------------------------}
FUNCTION NewItem (Name, Param: TMenuStr; KeyCode: Word; Command: Word;
  AHelpCtx: Word; Next: PMenuItem): PMenuItem;
VAR P: PMenuItem; R: TRect; T: PView;
BEGIN
   If (Name <> '') AND (Command <> 0) Then Begin
     New(P);                                          { Allocate memory }
     If (P <> Nil) Then Begin                         { Check valid pointer }
       P^.Next := Next;                               { Hold next item }
       P^.Name := NewStr(Name);                       { Hold item name }
       P^.Command := Command;                         { Hold item command }
       R.Assign(1, 1, 10, 10);                        { Random assignment }
       T := New(PView, Init(R));                      { Create a view }
       If (T <> Nil) Then Begin
         P^.Disabled := NOT T^.CommandEnabled(Command);
         Dispose(T, Done);                            { Dispose of view }
       End Else P^.Disabled := True;
       P^.KeyCode := KeyCode;                         { Hold item keycode }
       P^.HelpCtx := AHelpCtx;                        { Hold help context }
       P^.Param := NewStr(Param);                     { Hold parameter }
     End;
     NewItem := P;                                    { Return item }
   End Else NewItem := Next;                          { Move forward }
END;

{---------------------------------------------------------------------------}
{  NewSubMenu -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 14May98 LdB        }
{---------------------------------------------------------------------------}
FUNCTION NewSubMenu (Name: TMenuStr; AHelpCtx: Word; SubMenu: PMenu;
  Next: PMenuItem): PMenuItem;
VAR P: PMenuItem;
BEGIN
   If (Name <> '') AND (SubMenu <> Nil) Then Begin
     New(P);                                          { Allocate memory }
     If (P <> Nil) Then Begin                         { Check valid pointer }
       P^.Next := Next;                               { Hold next item }
       P^.Name := NewStr(Name);                       { Hold submenu name }
       P^.Command := 0;                               { Clear item command }
       P^.Disabled := False;                          { Item not disabled }
       P^.HelpCtx := AHelpCtx;                        { Set help context }
       P^.SubMenu := SubMenu;                         { Hold next submenu }
     End;
     NewSubMenu := P;                                 { Return submenu }
   End Else NewSubMenu := Next;                       { Return next item }
END;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                          STATUS INTERFACE ROUTINES                        }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{---------------------------------------------------------------------------}
{  NewStatusDef -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 15May98 LdB      }
{---------------------------------------------------------------------------}
FUNCTION NewStatusDef (AMin, AMax: Word; AItems: PStatusItem;
ANext:PStatusDef): PStatusDef;
VAR T: PStatusDef;
BEGIN
   New(T);                                            { Allocate memory }
   If (T <> Nil) Then Begin                           { Check valid pointer }
     T^.Next := ANext;                                { Set next item }
     T^.Min := AMin;                                  { Hold min value }
     T^.Max := AMax;                                  { Hold max value }
     T^.Items := AItems;                              { Hold item list }
   End;
   NewStatusDef := T;                                 { Return status }
END;

{---------------------------------------------------------------------------}
{  NewStatusKey -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 15May98 LdB      }
{---------------------------------------------------------------------------}
FUNCTION NewStatusKey (AText: String; AKeyCode: Word; ACommand: Word;
  ANext: PStatusItem): PStatusItem;
VAR T: PStatusItem;
BEGIN
   New(T);                                            { Allocate memory }
   If (T <> Nil) Then Begin                           { Check valid pointer }
     T^.Text := NewStr(AText);                        { Hold text string }
     T^.KeyCode := AKeyCode;                          { Hold keycode }
     T^.Command := ACommand;                          { Hold command }
     T^.Next := ANext;                                { Pointer to next }
   End;
   NewStatusKey := T;                                 { Return status item }
END;

{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}
{                           OBJECT REGISTER ROUTINES                        }
{+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++}

{---------------------------------------------------------------------------}
{  RegisterMenus -> Platforms DOS/DPMI/WIN/NT/OS2 - Updated 15May98 LdB     }
{---------------------------------------------------------------------------}
PROCEDURE RegisterMenus;
BEGIN
   RegisterType(RMenuBar);                            { Register bar menu }
   RegisterType(RMenuBox);                            { Register menu box }
   RegisterType(RStatusLine);                         { Register status line }
   RegisterType(RMenuPopup);                          { Register popup menu }
END;

END.
{
 $Log$
 Revision 1.15  2002-09-07 15:06:37  peter
   * old logs removed and tabs fixed

 Revision 1.14  2002/06/10 18:41:26  pierre
  + add Submenu recognition sign

 Revision 1.13  2002/05/30 06:58:28  pierre
  * fix grpah related menubar draw issues

 Revision 1.12  2002/05/29 19:36:52  pierre
  * fix UseFixedFont related code

 Revision 1.11  2002/05/21 10:53:25  pierre
  * fix graphical separation lines

}
