{
    This file is part of the Free Pascal run time library.

    A file in Amiga system run time library.
    Copyright (c) 1998-2003 by Nils Sjoholm
    member of the Amiga RTL development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{
    History:
    Added overlay functions for Pchar->Strings, functions
    and procedures.
    14 Jul 2000.

    Changed tWBArg.wa_Lock from a pointer to a longint.
    15 Aug 2000.

    Fixed tDiskObject.
    Member tDiskObject.do_CurrentX was defined as a pointer,
    should be longint.
    17 Aug 2000.
 
    Added functions and procedures with array of const.
    For use with fpc 1.0.7. Thay are in systemvartags.
    05 Nov 2002.

    Removed amigaoverlays, use smartlink instead.
    05 Nov 2002.

    Added the defines use_amiga_smartlink and
    use_auto_openlib. Implemented autoopening of
    the library.
    14 Jan 2003.

    nils.sjoholm@mailbox.swipnet.se
}

{$I useamigasmartlink.inc}
{$ifdef use_amiga_smartlink}
   {$smartlink on}
{$endif use_amiga_smartlink}

unit workbench;

INTERFACE

uses exec,amigados,utility, intuition;



Type

    pWBArg = ^tWBArg;
    tWBArg = record
        wa_Lock         : longint;      { a lock descriptor }
        wa_Name         : STRPTR;       { a string relative to that lock }
    end;

    WBArgList = Array [1..100] of tWBArg; { Only 1..smNumArgs are valid }
    pWBArgList = ^WBArgList;


    pWBStartup = ^tWBStartup;
    tWBStartup = record
        sm_Message      : tMessage;      { a standard message structure }
        sm_Process      : pMsgPort;   { the process descriptor for you }
        sm_Segment      : BPTR;         { a descriptor for your code }
        sm_NumArgs      : Longint;      { the number of elements in ArgList }
        sm_ToolWindow   : STRPTR;       { description of window }
        sm_ArgList      : pWBArgList; { the arguments themselves }
    end;


Const

    WBDISK              = 1;
    WBDRAWER            = 2;
    WBTOOL              = 3;
    WBPROJECT           = 4;
    WBGARBAGE           = 5;
    WBDEVICE            = 6;
    WBKICK              = 7;
    WBAPPICON           = 8;

Type

    pOldDrawerData = ^tOldDrawerData;
    tOldDrawerData = record
        dd_NewWindow    : tNewWindow;    { args to open window }
        dd_CurrentX     : Longint;      { current x coordinate of origin }
        dd_CurrentY     : Longint;      { current y coordinate of origin }
    end;

Const

{ the amount of DrawerData actually written to disk }

    OLDDRAWERDATAFILESIZE  = 56;  { sizeof(OldDrawerData) }

Type
    pDrawerData = ^tDrawerData;
    tDrawerData = record
        dd_NewWindow    : tNewWindow;    { args to open window }
        dd_CurrentX     : Longint;      { current x coordinate of origin }
        dd_CurrentY     : Longint;      { current y coordinate of origin }
        dd_Flags        : Longint;      { flags for drawer }
        dd_ViewModes    : Word;        { view mode for drawer }
    end;

Const

{ the amount of DrawerData actually written to disk }

    DRAWERDATAFILESIZE  = 62;  { sizeof(DrawerData) }


Type

    pDiskObject = ^tDiskObject;
    tDiskObject = record
        do_Magic        : Word;        { a magic number at the start of the file }
        do_Version      : Word;        { a version number, so we can change it }
        do_Gadget       : tGadget;       { a copy of in core gadget }
        do_Type         : Byte;
        do_DefaultTool  : STRPTR;
        do_ToolTypes    : Pointer;
        do_CurrentX     : Longint;
        do_CurrentY     : Longint;
        do_DrawerData   : pDrawerData;
        do_ToolWindow   : STRPTR;       { only applies to tools }
        do_StackSize    : Longint;      { only applies to tools }
    end;

Const

    WB_DISKMAGIC        = $e310;        { a magic number, not easily impersonated }
    WB_DISKVERSION      = 1;            { our current version number }
    WB_DISKREVISION     = 1;            { our current revision number }
  {I only use the lower 8 bits of Gadget.UserData for the revision # }
    WB_DISKREVISIONMASK = 255;
Type
    pFreeList = ^tFreeList;
    tFreeList = record
        fl_NumFree      : Integer;
        fl_MemList      : tList;
    end;

Const

{ each message that comes into the WorkBenchPort must have a type field
 * in the preceeding short.  These are the defines for this type
 }

    MTYPE_PSTD          = 1;    { a "standard Potion" message }
    MTYPE_TOOLEXIT      = 2;    { exit message from our tools }
    MTYPE_DISKCHANGE    = 3;    { dos telling us of a disk change }
    MTYPE_TIMER         = 4;    { we got a timer tick }
    MTYPE_CLOSEDOWN     = 5;    { <unimplemented> }
    MTYPE_IOPROC        = 6;    { <unimplemented> }
    MTYPE_APPWINDOW     = 7;    {     msg from an app window }
    MTYPE_APPICON       = 8;    {     msg from an app icon }
    MTYPE_APPMENUITEM   = 9;    {     msg from an app menuitem }
    MTYPE_COPYEXIT      = 10;   {     exit msg from copy process }
    MTYPE_ICONPUT       = 11;   {     msg from PutDiskObject in icon.library }


{ workbench does different complement modes for its gadgets.
 * It supports separate images, complement mode, and backfill mode.
 * The first two are identical to intuitions GADGIMAGE and GADGHCOMP.
 * backfill is similar to GADGHCOMP, but the region outside of the
 * image (which normally would be color three when complemented)
 * is flood-filled to color zero.
 }

    GFLG_GADGBACKFILL   = $0001;
    GADGBACKFILL        = $0001;   { an old synonym }

{ if an icon does not really live anywhere, set its current position
 * to here
 }

    NO_ICON_POSITION    = $80000000;

{    If you find am_Version >= AM_VERSION, you know this structure has
 * at least the fields defined in this version of the include file
 }
 AM_VERSION   =   1;

Type
   pAppMessage = ^tAppMessage;
   tAppMessage = record
    am_Message       : tMessage;            {    standard message structure }
    am_Type          : Word;              {    message type }
    am_UserData      : ULONG;            {    application specific }
    am_ID            : ULONG;            {    application definable ID }
    am_NumArgs       : ULONG;            {    # of elements in arglist }
    am_ArgList       : pWBArgList;       {    the arguements themselves }
    am_Version       : Word;              {    will be AM_VERSION }
    am_Class         : Word;              {    message class }
    am_MouseX        : Integer;              {    mouse x position of event }
    am_MouseY        : Integer;              {    mouse y position of event }
    am_Seconds       : ULONG;            {    current system clock time }
    am_Micros        : ULONG;            {    current system clock time }
    am_Reserved      : Array[0..7] of ULONG;       {    avoid recompilation }
   END;

{* types of app messages *}
const
    AMTYPE_APPWINDOW   = 7;    {* app window message    *}
    AMTYPE_APPICON     = 8;    {* app icon message  *}
    AMTYPE_APPMENUITEM = 9;    {* app menu item message *}

{
 * The following structures are private.  These are just stub
 * structures for code compatibility...
 }
type

 tAppWindow = record
   aw_PRIVATE : Pointer;
 END;
 pAppWindow = ^tAppWindow;

 tAppIcon = record
   ai_PRIVATE : Pointer;
 END;
 pAppIcon = ^tAppIcon;

 tAppMenuItem = record
   ami_PRIVATE : Pointer;
 END;
 pAppMenuItem = ^tAppMenuItem;


CONST
    WORKBENCHNAME : PChar  = 'workbench.library';

VAR
    WorkbenchBase : pLibrary;

FUNCTION AddAppIconA(id : ULONG; userdata : ULONG; text_ : pCHAR; msgport : pMsgPort; lock : pFileLock; diskobj : pDiskObject; taglist : pTagItem) : pAppIcon;
FUNCTION AddAppMenuItemA(id : ULONG; userdata : ULONG; text_ : pCHAR; msgport : pMsgPort; taglist : pTagItem) : pAppMenuItem;
FUNCTION AddAppWindowA(id : ULONG; userdata : ULONG; window : pWindow; msgport : pMsgPort; taglist : pTagItem) : pAppWindow;
FUNCTION RemoveAppIcon(appIcon : pAppIcon) : BOOLEAN;
FUNCTION RemoveAppMenuItem(appMenuItem : pAppMenuItem) : BOOLEAN;
FUNCTION RemoveAppWindow(appWindow : pAppWindow) : BOOLEAN;
PROCEDURE WBInfo(lock : BPTR; name : pCHAR; screen : pScreen);

FUNCTION AddAppIconA(id : ULONG; userdata : ULONG; text_ : string; msgport : pMsgPort; lock : pFileLock; diskobj : pDiskObject; taglist : pTagItem) : pAppIcon;
FUNCTION AddAppMenuItemA(id : ULONG; userdata : ULONG; text_ : string; msgport : pMsgPort; taglist : pTagItem) : pAppMenuItem;
PROCEDURE WBInfo(lock : BPTR; name : string; screen : pScreen);

IMPLEMENTATION

uses pastoc,msgbox;

FUNCTION AddAppIconA(id : ULONG; userdata : ULONG; text_ : pCHAR; msgport : pMsgPort; lock : pFileLock; diskobj : pDiskObject; taglist : pTagItem) : pAppIcon;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVE.L  id,D0
    MOVE.L  userdata,D1
    MOVEA.L text_,A0
    MOVEA.L msgport,A1
    MOVEA.L lock,A2
    MOVEA.L diskobj,A3
    MOVEA.L taglist,A4
    MOVEA.L WorkbenchBase,A6
    JSR -060(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

FUNCTION AddAppMenuItemA(id : ULONG; userdata : ULONG; text_ : pCHAR; msgport : pMsgPort; taglist : pTagItem) : pAppMenuItem;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVE.L  id,D0
    MOVE.L  userdata,D1
    MOVEA.L text_,A0
    MOVEA.L msgport,A1
    MOVEA.L taglist,A2
    MOVEA.L WorkbenchBase,A6
    JSR -072(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

FUNCTION AddAppWindowA(id : ULONG; userdata : ULONG; window : pWindow; msgport : pMsgPort; taglist : pTagItem) : pAppWindow;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVE.L  id,D0
    MOVE.L  userdata,D1
    MOVEA.L window,A0
    MOVEA.L msgport,A1
    MOVEA.L taglist,A2
    MOVEA.L WorkbenchBase,A6
    JSR -048(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

FUNCTION RemoveAppIcon(appIcon : pAppIcon) : BOOLEAN;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L appIcon,A0
    MOVEA.L WorkbenchBase,A6
    JSR -066(A6)
    MOVEA.L (A7)+,A6
    TST.W   D0
    BEQ.B   @end
    MOVEQ   #1,D0
  @end: MOVE.B  D0,@RESULT
  END;
END;

FUNCTION RemoveAppMenuItem(appMenuItem : pAppMenuItem) : BOOLEAN;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L appMenuItem,A0
    MOVEA.L WorkbenchBase,A6
    JSR -078(A6)
    MOVEA.L (A7)+,A6
    TST.W   D0
    BEQ.B   @end
    MOVEQ   #1,D0
  @end: MOVE.B  D0,@RESULT
  END;
END;

FUNCTION RemoveAppWindow(appWindow : pAppWindow) : BOOLEAN;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L appWindow,A0
    MOVEA.L WorkbenchBase,A6
    JSR -054(A6)
    MOVEA.L (A7)+,A6
    TST.W   D0
    BEQ.B   @end
    MOVEQ   #1,D0
  @end: MOVE.B  D0,@RESULT
  END;
END;

PROCEDURE WBInfo(lock : BPTR; name : pCHAR; screen : pScreen);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L lock,A0
    MOVEA.L name,A1
    MOVEA.L screen,A2
    MOVEA.L WorkbenchBase,A6
    JSR -090(A6)
    MOVEA.L (A7)+,A6
  END;
END;

FUNCTION AddAppIconA(id : ULONG; userdata : ULONG; text_ : string; msgport : pMsgPort; lock : pFileLock; diskobj : pDiskObject; taglist : pTagItem) : pAppIcon;
begin
       AddAppIconA := AddAppIconA(id,userdata,pas2c(text_),msgport,lock,diskobj,taglist);
end;

FUNCTION AddAppMenuItemA(id : ULONG; userdata : ULONG; text_ : string; msgport : pMsgPort; taglist : pTagItem) : pAppMenuItem;
begin
       AddAppMenuItemA := AddAppMenuItemA(id,userdata,pas2c(text_),msgport,taglist);
end;

PROCEDURE WBInfo(lock : BPTR; name : string; screen : pScreen);
begin
       WBInfo(lock,pas2c(name),screen);
end;

{$I useautoopenlib.inc}
{$ifdef use_auto_openlib}
  {$Info Compiling autoopening of workbench.library}

var
    wb_exit : Pointer;

procedure ClosewbLibrary;
begin
    ExitProc := wb_exit;
    if WorkbenchBase <> nil then begin
        CloseLibrary(WorkbenchBase);
        WorkbenchBase := nil;
    end;
end;

const
    { Change VERSION and LIBVERSION to proper values }

    VERSION : string[2] = '0';
    LIBVERSION : Cardinal = 0;

begin
    WorkbenchBase := nil;
    WorkbenchBase := OpenLibrary(WORKBENCHNAME,LIBVERSION);
    if WorkbenchBase <> nil then begin
        wb_exit := ExitProc;
        ExitProc := @ClosewbLibrary
    end else begin
        MessageBox('FPC Pascal Error',
        'Can''t open wb.library version ' + VERSION + #10 +
        'Deallocating resources and closing down',
        'Oops');
        halt(20);
    end;

{$else}
   {$Warning No autoopening of workbench.library compiled}
   {$Info Make sure you open workbench.library yourself}
{$endif use_auto_openlib}

END. (* UNIT WB *)

{
  $Log$
  Revision 1.3  2003-01-14 18:46:04  nils
  * added defines use_amia_smartlink and use_auto_openlib

  * implemented autoopening of library

  Revision 1.2  2002/11/19 18:47:48  nils
    * update check internal log

}

  

