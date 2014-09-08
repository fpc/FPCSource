{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2014 by Free Pascal development team

    workbnech.library functions

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit Workbench;

{$MODE OBJFPC} {$H+}
{$PACKRECORDS C}


Interface

Uses
   exec, AmigaDos, Utility, Intuition, AGraphics;

  // NOTE:
  // - unit based on AROS ABIv0 sources d.d. 16-oct-2013
  // - workbench/icon.h is not present in this unit. Instead it is present in
  //   unit icon.
  // - It is uncertain if v45 additions are aros specific.
  // - No full ABIv1 diff was done yet (only one item was done for future
  //   reference. Ergo, this unit is not ABIv1 compatible yet.

// ###### workbench/startup.h ###############################################


type
  PWBArg = ^TWBArg;
  TWBArg = record
    wa_Lock: BPTR;       // A lock descriptor.
    wa_Name: PChar;      // A string relative to that lock.
  end;

// ###### <freepascal> ######################################################

  PWBArgList = ^TWBArgList;
  TWBArgList = array[1..100] of TWBArg; // Only 1..smNumArgs are valid

// ###### </freepascal> #####################################################

  PWBStartup = ^TWBStartup;
  TWBStartup = record
    sm_Message: TMessage;    // A standard message structure.
    sm_Process: PMsgPort;    // The process descriptor for you.
    sm_Segment: BPTR;        // A descriptor for your code.
    sm_NumArgs: LongInt;     // The number of elements in ArgList.
    sm_ToolWindow: PChar;    // Description of window.
    sm_ArgList: PWBArgList;  // The arguments themselves
  end;

// ###### workbench/workbench.h #############################################

const
  WORKBENCHNAME: PChar = 'workbench.library';  // Workbench library name.

type
  POldDrawerData = ^TOldDrawerData;
  TOldDrawerData = record
    dd_NewWindow: TNewWindow;  // Args to open window.
    dd_CurrentX: LongInt;      // Current x coordinate of origin.
    dd_CurrentY: LongInt;      // Current y coordinate of origin.
  end;

const
  OLDDRAWERDATAFILESIZE = SizeOf(TOldDrawerData);  // Amount of DrawerData actually written to disk.

type
  PDrawerData = ^TDrawerData;
  TDrawerData = record
     dd_NewWindow: TNewWindow; // Args to open window.
     dd_CurrentX: LongInt;     // Current x coordinate of origin.
     dd_CurrentY: LongInt;     // Current y coordinate of origin.
     dd_Flags: LongWord;       // Flags for drawer.
     dd_ViewModes: Word;       // View mode for drawer.
  end;

const
  DRAWERDATAFILESIZE = SizeOf(TDrawerData);  // Amount of DrawerData actually written to disk.

  // Definitions for dd_ViewModes
  DDVM_BYDEFAULT = 0;   // Default (inherit parent's view mode).
  DDVM_BYICON    = 1;   // View as icons.
  DDVM_BYNAME    = 2;   // View as text, sorted by name.
  DDVM_BYDATE    = 3;   // View as text, sorted by date.
  DDVM_BYSIZE    = 4;   // View as text, sorted by size.
  DDVM_BYTYPE    = 5;   // View as text, sorted by type.

  // Definitions for dd_Flags
  DDFLAGS_SHOWDEFAULT = 0;  // Fefault (show only icons).
  DDFLAGS_SHOWICONS   = 1;  // Show only icons.
  DDFLAGS_SHOWALL     = 2;  // Show all files.

type
  PDiskObject = ^TDiskObject;
  TDiskObject = record
    do_Magic: Word;              // A magic number at the start of the file.
    do_Version: Word;            // A version number, so we can change it.
    do_Gadget: TGadget;          // A copy of in core gadget.
    do_type: Word;
    do_DefaultTool: STRPTR;
    do_Tooltypes: PPChar;
    do_CurrentX: LongInt;
    do_CurrentY: LongInt;
    do_DrawerData: PDrawerData;
    do_ToolWindow: STRPTR;       // Only applies to tools.
    do_StackSize: LongInt;       // Only applies to tools.
  end;

const
  WBDISK    = 1;
  WBDRAWER  = 2;
  WBTOOL    = 3;
  WBPROJECT = 4;
  WBGARBAGE = 5;
  WBDEVICE  = 6;
  WBKICK    = 7;
  WBAPPICON = 8;

  WB_DISKVERSION      = 1;                  // Current version number.
  WB_DISKREVISION     = 1;                  // Current revision number.
  WB_DISKREVISIONMASK = $FF;                // Only use the lower 8 bits of Gadget.UserData for the revision #.

  WB_DISKMAGIC = $E310;              // A magic number, not easily impersonated.

type
  PFreeList = ^TFreeList;
  TFreeList = record
    fl_NumFree: SmallInt;
    fl_MemList: TList;
  end;

const
  //
  // workbench does different complement modes for its gadgets.
  // It supports separate images, complement mode, and backfill mode.
  // The first two are identical to intuitions GADGIMAGE and GADGHCOMP.
  // backfill is similar to GADGHCOMP, but the region outside of the
  // image (which normally would be color three when complemented)
  // is flood-filled to color zero.
  //
  GFLG_GADGBACKFILL = $0001;
  // GADGBACKFILL = $0001;   // an old synonym
  NO_ICON_POSITION  = $80000000;  // If an icon does not really live anywhere, set its current position to here.

type
  PAppMessage = ^TAppMessage;
  TAppMessage = record
    am_Message: TMessage;             // Standard message structure.
    am_type: Word;              // Message type.
    am_UserData: IPTR;               // Application specific.
    {$IFDEF AROS_ABIv1}
    am_ID: IPTR;               // Application definable ID.
    {$ELSE}
    am_ID: LongWord;              // Application definable ID.
    {$ENDIF}
    am_NumArgs: LongInt;               // # of elements in arglist.
    am_ArgList: PWBArgList;           // The arguements themselves.
    am_Version: Word;              // Will be AM_VERSION.
    am_Class: Word;              // Message class.
    am_MouseX: SmallInt;               // Mouse x position of event.
    am_MouseY: SmallInt;               // Mouse y position of event.
    am_Seconds: LongWord;              // Current system clock time.
    am_Micros: LongWord;              // Current system clock time.
    am_Reserved: array[0..7] of LongWord;  // Avoid recompilation.
  end;

const
  //
  // If you find am_Version >= AM_VERSION, you know this structure has
  // at least the fields defined in this version of the include file
  //
  AM_VERSION =  1;  // Definition for am_Version.

  // Definitions for member am_type of structure TAppMessage.
  AMTYPE_APPWINDOW     =  7;  // App window message.
  AMTYPE_APPICON       =  8;  // App icon message.
  AMTYPE_APPMENUITEM   =  9;  // App menu item message.
  AMTYPE_APPWINDOWZONE = 10;  // App window drop zone message.

  // Definitions for member am_Class of structure TAppMessage for AppIcon messages (V44)
  AMCLASSICON_Open        =  0;  // The "Open" menu item was invoked, the icon got double-clicked or an icon got dropped on it.
  AMCLASSICON_Copy        =  1;  // The "Copy" menu item was invoked.
  AMCLASSICON_Rename      =  2;  // The "Rename" menu item was invoked.
  AMCLASSICON_Information =  3;  // The "Information" menu item was invoked.
  AMCLASSICON_Snapshot    =  4;  // The "Snapshot" menu item was invoked.
  AMCLASSICON_UnSnapshot  =  5;  // The "UnSnapshot" menu item was invoked.
  AMCLASSICON_LeaveOut    =  6;  // The "Leave Out" menu item was invoked.
  AMCLASSICON_PutAway     =  7;  // The "Put Away" menu item was invoked.
  AMCLASSICON_Delete      =  8;  // The "Delete" menu item was invoked.
  AMCLASSICON_FormatDisk  =  9;  // The "Format Disk" menu item was invoked.
  AMCLASSICON_EmptyTrash  = 10;  // The "Empty Trash" menu item was invoked.
  AMCLASSICON_Selected    = 11;  // The icon is now selected.
  AMCLASSICON_Unselected  = 12;  // The icon is now unselected.

type
  // The message your AppIcon rendering hook gets invoked with.
  PAppIconRenderMsg = ^TAppIconRenderMsg;
  TAppIconRenderMsg = record
    arm_RastPort: PRastPort; // RastPort to render into.
    arm_Icon: PDiskObject;   // The icon to be rendered.
    arm_Label: STRPTR;       // The icon label txt.
    arm_Tags: PTagItem;      // Further tags to be passed on to DrawIconStateA().
    arm_Left: SmallInt;      // \ Rendering origin, not taking the
    arm_Top: SmallInt;       // / button border into account.
    arm_Width: SmallInt;     // \ Limit your rendering to
    arm_Height: SmallInt;    // / this area.
    arm_State: LongWord;     // IDS_SELECTED, IDS_NORMAL, etc.
  end;

  // The message your drop zone hook gets invoked with. }
  PAppWindowDropZoneMsg = ^TAppWindowDropZoneMsg;
  TAppWindowDropZoneMsg = record
    adzm_RastPort: PRastPort; // RastPort to render into.
    adzm_DropZoneBox: TIBox;  // Limit your rendering to this area.
    adzm_ID: LongWord;        // \ These come from straight
    adzm_UserData: IPTR;      // / from AddAppWindowDropZoneA().
    adzm_Action: LongInt;     // See below for a list of actions.
  end;

const
  // definitions for adzm_Action
  ADZMACTION_Enter = 0;
  ADZMACTION_Leave = 1;

type
  // The message your icon selection change hook is invoked with. }
  PIconSelectMsg = ^TIconSelectMsg;
  TIconSelectMsg = record
    ism_Length: LongWord;      // Size of this data structure (in bytes).
    ism_Drawer: BPTR;          // Lock on the drawer this object resides in, NULL for Workbench backdrop (devices).
    ism_Name: STRPTR;          // Name of the object in question.
    ism_type: Word;            // One of WBDISK, WBDRAWER, WBTOOL, WBPROJECT, WBGARBAGE, WBDEVICE, WBKICK or WBAPPICON.
    ism_Selected: LongBool;    // TRUE if currently selected, FALSE otherwise.
    ism_Tags: PTagItem;        // Pointer to the list of tag items passed to ChangeWorkbenchSelectionA().
    ism_DrawerWindow: PWindow; // Pointer to the window attached to this icon, if the icon is a drawer-like object.
    ism_ParentWindow: PWindow; // Pointer to the window the icon resides in.
    ism_Left: SmallInt;        // Position and size of the icon; note that the icon may not entirely reside within the visible bounds of the parent window.
    ism_Top: SmallInt;
    ism_Width: SmallInt;
    ism_Height: SmallInt;
  end;

const
  // These are the values your hook code can return.
  ISMACTION_Unselect = 0; // Unselect the icon.
  ISMACTION_Select   = 1; // Select the icon.
  ISMACTION_Ignore   = 2; // Do not change the selection state.
  ISMACTION_Stop     = 3; // Do not invoke the hook code again, leave the icon as it is.

type
  // The messages your copy hook is invoked with.
  PCopyBeginMsg = ^TCopyBeginMsg;
  TCopyBeginMsg = record
    cbm_Length: LongWord;        // Size of this data structure in bytes.
    cbm_Action: LongInt;         // Will be set to CPACTION_Begin (see below).
    cbm_SourceDrawer: BPTR;      // A lock on the source drawer.
    cbm_DestinationDrawer: BPTR; // A lock on the destination drawer.
  end;

  PCopyDataMsg = ^TCopyDataMsg;
  TCopyDataMsg = record
    cdm_Length: LongWord;        // Size of this data structure in bytes.
    cdm_Action: LongInt;         // Will be set to CPACTION_Copy (see below).
    cdm_SourceLock: BPTR;        // A lock on the parent directory of the source file/drawer.
    cdm_SourceName: STRPTR;      // The name of the source file or drawer.
    cdm_DestinationLock: BPTR;   // A lock on the parent directory of the destination file/drawer.
    cdm_DestinationName: STRPTR; // The name of the destination file/drawer.
                                 // This may or may not match the name of the source file/drawer in case the
                                 // data is to be copied under a different name. For example, this is the case
                                 // with the Workbench "Copy" command which creates duplicates of file/drawers by
                                 // prefixing the duplicate's name with "Copy_XXX_of".
    cdm_DestinationX: LongInt;   // When the icon corresponding to the destination is written to disk, this
    cdm_DestinationY: LongInt;   // is the position (put into its DiskObject->do_CurrentX/DiskObject->do_CurrentY.
  end;                           // fields) it should be placed at.

  PCopyEndMsg = ^TCopyEndMsg;
  TCopyEndMsg = record
    cem_Length: LongWord; // Size of this data structure in bytes.
    cem_Action: LongInt;  // Will be set to CPACTION_End (see below).
  end;

const
  CPACTION_Begin = 0;
  CPACTION_Copy  = 1; // This message arrives for each file or drawer to be copied.
  CPACTION_End   = 2; // This message arrives when all files/drawers have been copied.

type
  // The messages your delete hook is invoked with.
  PDeleteBeginMsg = ^TDeleteBeginMsg;
  TDeleteBeginMsg = record
    dbm_Length: LongWord; // Size of this data structure in bytes.
    dbm_Action: LongInt;  // Will be set to either DLACTION_BeginDiscard or DLACTION_BeginEmptyTrash (see below).
  end;

  PDeleteDataMsg = ^TDeleteDataMsg;
  TDeleteDataMsg = record
    ddm_Length: LongWord; // Size of this data structure in bytes.
    ddm_Action: LongInt;  // Will be set to either DLACTION_DeleteContents or DLACTION_DeleteObject (see below).
    ddm_Lock: BPTR;       // A Lock on the parent directory of the object whose contents or which itself should be deleted.
    ddm_Name: STRPTR;     // The name of the object whose contents or which itself should be deleted.
  end;

  PDeleteEndMsg = ^TDeleteEndMsg;
  TDeleteEndMsg = record
    dem_Length: LongWord; // Size of this data structure in bytes.
    dem_Action: LongInt;  // Will be set to DLACTION_End (see below).
  end;


const
  DLACTION_BeginDiscard    = 0;
  DLACTION_BeginEmptyTrash = 1; // This indicates that the following delete operations are intended to empty the trashcan.
  DLACTION_DeleteContents  = 3; // This indicates that the object described by lock and name refers to a drawer; you should empty its contents but  DO NOT  delete the drawer itself!
  DLACTION_DeleteObject    = 4; // This indicates that the object described by lock and name should be deleted; this could be a file or an empty drawer.
  DLACTION_End             = 5; // This indicates that the deletion process is finished.

type
  // The message your setup/cleanup hook gets invoked with.
  PSetupCleanupHookMsg = ^TSetupCleanupHookMsg;
  TSetupCleanupHookMsg = record
    schm_Length: LongWord;
    schm_State: LongInt;
  end;

const
  SCHMSTATE_TryCleanup = 0; // Workbench will attempt to shut down now.
  SCHMSTATE_Cleanup    = 1; // Workbench will really shut down now.
  SCHMSTATE_Setup      = 2; // Workbench is operational again or could not be shut down.

type
  // The messages your text input hook is invoked with.
  PTextInputMsg = ^TTextInputMsg;
  TTextInputMsg = record
    tim_Length: LongWord; // Size of this data structure in bytes.
    tim_Action: LongInt;  // One of the TIACTION_... values listed below.
    tim_Prompt: STRPTR;   // The Workbench suggested result, depending on what kind of input is requested (as indicated by the tim_Action member).
  end;

const
  TIACTION_Rename        = 0; // A file or drawer is to be renamed.
  TIACTION_RelabelVolume = 1; // A volume is to be relabeled.
  TIACTION_NewDrawer     = 2; // A new drawer is to be created.
  TIACTION_Execute       = 3; // A program or script is to be executed.

type
  //
  // The following structures are private.  These are just stub
  // structures for code compatibility...
  //
  {
  pAppWindow = ^tAppWindow;
  tAppWindow = record
    aw_PRIVATE: Pointer;
  end;

  pAppWindowDropZone = ^tAppWindowDropZone;
  tAppWindowDropZone = record
    awdz_PRIVATE: pointer;
  end;

  pAppIcon = ^tAppIcon;
  tAppIcon = record
    ai_PRIVATE: Pointer;
  end;

  pAppMenuItem = ^tAppMenuItem;
  tAppMenuItem = record
    ami_PRIVATE: Pointer;
  end;
  }
  (* not in aros
  PAppMenu = ^tAppMenu;
  tAppMenu = record
    am_PRIVATE: pointer;
  end;
  *)
  // Aros versions:

  PAppWindow = ^TAppWindow;
  TAppWindow = record
  end;

  PAppWindowDropZone = ^TAppWindowDropZone;
  TAppWindowDropZone = record
  end;

  PAppIcon = ^TAppIcon;
  TAppIcon = record
  end;

  PAppMenuItem = ^TAppMenuItem;
  TAppMenuItem = record
  end;


const
  (* WBA_Dummy = TAG_USER + $A000; *) {$WARNING AROS: constant WBA_Dummy is renamed to WBA_BASE}
  WBA_BASE = TAG_USER + $A000;
  // Tags for use with AddAppIconA()
  // The different menu items the AppIcon responds to (BOOL).
  WBAPPICONA_SupportsOpen        = WBA_BASE +  1; // AppIcon responds to the "Open" menu item (LongBool).
  WBAPPICONA_SupportsCopy        = WBA_BASE +  2; // AppIcon responds to the "Copy" menu item (LongBool).
  WBAPPICONA_SupportsRename      = WBA_BASE +  3; // AppIcon responds to the "Rename" menu item (LongBool).
  WBAPPICONA_SupportsInformation = WBA_BASE +  4; // AppIcon responds to the "Information" menu item (LongBool).
  WBAPPICONA_SupportsSnapshot    = WBA_BASE +  5; // AppIcon responds to the "Snapshot" menu item (LongBool).
  WBAPPICONA_SupportsUnSnapshot  = WBA_BASE +  6; // AppIcon responds to the "UnSnapshot" menu item (LongBool).
  WBAPPICONA_SupportsLeaveOut    = WBA_BASE +  7; // AppIcon responds to the "LeaveOut" menu item (LongBool).
  WBAPPICONA_SupportsPutAway     = WBA_BASE +  8; // AppIcon responds to the "PutAway" menu item (LongBool).
  WBAPPICONA_SupportsDelete      = WBA_BASE +  9; // AppIcon responds to the "Delete" menu item (LongBool).
  WBAPPICONA_SupportsFormatDisk  = WBA_BASE + 10; // AppIcon responds to the "FormatDisk" menu item (LongBool).
  WBAPPICONA_SupportsEmptyTrash  = WBA_BASE + 11; // AppIcon responds to the "EmptyTrash" menu item (LongBool).

  WBAPPICONA_PropagatePosition   = WBA_BASE + 12; // AppIcon position should be propagated back to original DiskObject (LongBool).
  WBAPPICONA_RenderHook          = WBA_BASE + 13; // Callback hook to be invoked when rendering this icon (pHook).
  WBAPPICONA_NotifySelectState   = WBA_BASE + 14; // AppIcon wants to be notified when its select state changes (LongBool).

  // Tags for use with AddAppMenuA()
  WBAPPMENUA_CommandKeyString    = WBA_BASE + 15; // Command key string for this AppMenu (STRPTR).
  WBAPPMENUA_GetKey              = WBA_BASE + 65; // Item to be added should get sub menu items attached to; make room for it, then return the key to use later for attaching the items (A_PUL
  WBAPPMENUA_UseKey              = WBA_BASE + 66; // This item should be attached to a sub menu; the key provided refers to the sub menu it should be attached to (LongWord).
  WBAPPMENUA_GetTitleKey         = WBA_BASE + 77; // Item to be added is in fact a new menu title; make room for it, then return the key to use later for attaching the items (??? ULONG ???).

  // Tags for use with OpenWorkbenchObjectA()
  WBOPENA_ArgLock                = WBA_BASE + 16; // Corresponds to the wa_Lock member of a struct WBArg.
  WBOPENA_ArgName                = WBA_BASE + 17; // Corresponds to the wa_Name member of a struct WBArg.
  WBOPENA_Show                   = WBA_BASE + 75; // When opening a drawer, show all files or only icons? This must be one out of DDFLAGS_SHOWICONS, or DDFLAGS_SHOWALL; (Byte); (V45)
  WBOPENA_ViewBy                 = WBA_BASE + 76; // When opening a drawer, view the contents by icon, name, date, size or type? This must be one out of DDVM_BYICON, DDVM_BYNAME, DDVM_BYDATE, DDVM_BYSIZE or DDVM_BYTYPE; (UBYTE); (V45)

  // Tags for use with WorkbenchControlA()
  WBCTRLA_IsOpen                 = WBA_BASE + 18; // Check if the named drawer is currently open (PLongInt).
  WBCTRLA_DuplicateSearchPath    = WBA_BASE + 19; // Create a duplicate of the Workbench private search path list (PBPTR).
  WBCTRLA_FreeSearchPath         = WBA_BASE + 20; // Free the duplicated search path list (BPTR).
  WBCTRLA_GetDefaultStackSize    = WBA_BASE + 21; // Get the default stack size for launching programs with (PLongWord).
  WBCTRLA_SetDefaultStackSize    = WBA_BASE + 22; // Set the default stack size for launching programs with (LongWord).
  WBCTRLA_RedrawAppIcon          = WBA_BASE + 23; // Cause an AppIcon to be redrawn (pAppIcon).
  WBCTRLA_GetProgramList         = WBA_BASE + 24; // Get a list of currently running Workbench programs (pList).
  WBCTRLA_FreeProgramList        = WBA_BASE + 25; // Release the list of currently running Workbench programs (pList).
  WBCTRLA_GetSelectedIconList    = WBA_BASE + 36; // Get a list of currently selected icons (pList).
  WBCTRLA_FreeSelectedIconList   = WBA_BASE + 37; // Release the list of currently selected icons (pList).
  WBCTRLA_GetOpenDrawerList      = WBA_BASE + 38; // Get a list of currently open drawers (pList).
  WBCTRLA_FreeOpenDrawerList     = WBA_BASE + 39; // Release the list of currently open icons (pList).
  WBCTRLA_GetHiddenDeviceList    = WBA_BASE + 42; // Get the list of hidden devices (pList).

  WBCTRLA_FreeHiddenDeviceList   = WBA_BASE + 43; // Release the list of hidden devices (pList).
  WBCTRLA_AddHiddenDeviceName    = WBA_BASE + 44; // Add the name of a device which Workbench should never try to read a disk icon from (STRPTR).
  WBCTRLA_RemoveHiddenDeviceName = WBA_BASE + 45; // Remove a name from list of hidden devices (STRPTR).
  WBCTRLA_GettypeRestartTime     = WBA_BASE + 47; // Get the number of seconds that have to pass before typing the next character in a drawer window will restart with a new file name (PLongWord).
  WBCTRLA_SettypeRestartTime     = WBA_BASE + 48; // Set the number of seconds that have to pass before typing the next character in a drawer window will restart with a new file name (LongWord).
  WBCTRLA_GetCopyHook            = WBA_BASE + 69; // Obtain the hook that will be invoked when Workbench starts to copy files and data (pHook); (V45)
  WBCTRLA_SetCopyHook            = WBA_BASE + 70; // Install the hook that will be invoked when Workbench starts to copy files and data (pHook); (V45)
  WBCTRLA_GetDeleteHook          = WBA_BASE + 71; // Obtain the hook that will be invoked when Workbench discards files and drawers or empties the trashcan (pHook);  (V45).
  WBCTRLA_SetDeleteHook          = WBA_BASE + 72; // Install the hook that will be invoked when Workbench discards  files and drawers or empties the trashcan (pHook); (V45).
  WBCTRLA_GetTextInputHook       = WBA_BASE + 73; // Obtain the hook that will be invoked when Workbench requests that the user enters text, such as when a file is to be renamed  or a new drawer is to be created (pHook); (V45)
  WBCTRLA_SetTextInputHook       = WBA_BASE + 74; // Install the hook that will be invoked when Workbench requests that the user enters text, such as when a file is to be renamed or a new drawer is to be created (pHook); (V45)
  WBCTRLA_AddSetupCleanupHook    = WBA_BASE + 78; // Add a hook that will be invoked when Workbench is about to shut down (cleanup), and when Workbench has returned to operational state (setup) (pHook); (V45)
  WBCTRLA_RemSetupCleanupHook    = WBA_BASE + 79; // Remove a hook that has been installed with the WBCTRLA_AddSetupCleanupHook tag (pHook); (V45)

  // Tags for use with AddAppWindowDropZoneA()
  WBDZA_Left      = WBA_BASE + 26; // Zone left edge (SmallInt)
  WBDZA_RelRight  = WBA_BASE + 27; // Zone left edge, if relative to the right edge of the window (SmallInt)
  WBDZA_Top       = WBA_BASE + 28; // Zone top edge (SmallInt)
  WBDZA_RelBottom = WBA_BASE + 29; // Zone top edge, if relative to the bottom edge of the window (SmallInt)
  WBDZA_Width     = WBA_BASE + 30; // Zone width (SmallInt)
  WBDZA_RelWidth  = WBA_BASE + 31; // Zone width, if relative to the window width (SmallInt)
  WBDZA_Height    = WBA_BASE + 32; // Zone height (SmallInt)
  WBDZA_RelHeight = WBA_BASE + 33; // Zone height, if relative to the window height (SmallInt)
  WBDZA_Box       = WBA_BASE + 34; // Zone position and size (pIBox).
  WBDZA_Hook      = WBA_BASE + 35; // Hook to invoke when the mouse enters or leave a drop zone (pHook).

  // Reserved tags; don't use!  }
  WBA_Reserved1  = WBA_BASE + 40;
  WBA_Reserved2  = WBA_BASE + 41;
  WBA_Reserved3  = WBA_BASE + 46;
  WBA_Reserved4  = WBA_BASE + 49;
  WBA_Reserved5  = WBA_BASE + 50;
  WBA_Reserved6  = WBA_BASE + 51;
  WBA_Reserved7  = WBA_BASE + 52;
  WBA_Reserved8  = WBA_BASE + 53;
  WBA_Reserved9  = WBA_BASE + 54;
  WBA_Reserved10 = WBA_BASE + 55;
  WBA_Reserved11 = WBA_BASE + 56;
  WBA_Reserved12 = WBA_BASE + 57;
  WBA_Reserved13 = WBA_BASE + 58;
  WBA_Reserved14 = WBA_BASE + 59;
  WBA_Reserved15 = WBA_BASE + 60;
  WBA_Reserved16 = WBA_BASE + 61;
  WBA_Reserved17 = WBA_BASE + 62;
  WBA_Reserved18 = WBA_BASE + 63;
  WBA_Reserved19 = WBA_BASE + 64;

  // Last Tag
  WBA_LAST_TAG = WBA_BASE + 64;

  // Parameters for the UpdateWorkbench() function.
  UPDATEWB_ObjectRemoved = 0; // Object has been deleted.
  UPDATEWB_ObjectAdded   = 1; // Object is new or has changed.

(* left behind: not used in aros, classic only
const

{ each message that comes into the WorkBenchPort must have a type field
 * in the preceeding short.  These are the defines for this type
 }

  MTYPE_PSTD        = 1;    { a "standard Potion" message }
  MTYPE_TOOLEXIT    = 2;    { exit message from our tools }
  MTYPE_DISKCHANGE  = 3;    { dos telling us of a disk change }
  MTYPE_TIMER       = 4;    { we got a timer tick }
  MTYPE_CLOSEDOWN   = 5;    { <unimplemented> }
  MTYPE_IOPROC      = 6;    { <unimplemented> }
  MTYPE_APPWINDOW   = 7;    {     msg from an app window }
  MTYPE_APPICON     = 8;    {     msg from an app icon }
  MTYPE_APPMENUITEM = 9;    {     msg from an app menuitem }
  MTYPE_COPYEXIT    = 10;   {     exit msg from copy process }
  MTYPE_ICONPUT     = 11;   {     msg from PutDiskObject in icon.library }
*)

// ###### workbench/handler.h ###############################################
type
  TWBHM_type =
  (
    WBHM_TYPE_SHOW,  // Open all windows.
    WBHM_TYPE_HIDE,  // Close all windows.
    WBHM_TYPE_OPEN,  // Open a drawer.
    WBHM_TYPE_UPDATE // Update an object.
  );

  PWBHandlerMessage = ^TWBHandlerMessage;
  TWBHandlerMessage = record
    wbhm_Message: TMessage; // Standard message structure.
    wbhm_type: TWBHM_type;  // type of message.
    case integer of
    0 :
    (
      Open: record
        OpenName: STRPTR;   // Name of the drawer.
      end;
    );
    1 :
    (
      Update: record
        UpdateName: STRPTR;  // Mame of the object.
        Updatetype: LongInt; // type of object (WBDRAWER, WBPROJECT, ...).
      end;
    );
  end;


var
  WorkbenchBase: pLibrary;

{ v36 }
function UpdateWorkbench(const Name: STRPTR; Lock: BPTR; Action: LongInt): LongBool; syscall WorkbenchBase 5;
function QuoteWorkbench(StringNum: LongWord): LongBool; syscall WorkbenchBase 6; unimplemented;
function StartWorkbench(Flag: LongWord; Ptr: APTR): LongBool; syscall WorkbenchBase 7;
function AddAppWindowA(ID: LongWord; UserData: LongWord; Window: PWindow; MsgPort: PMsgPort; TagList: PTagItem): PAppWindow;  syscall WorkbenchBase 8;
function RemoveAppWindow(AppWindow: PAppWindow): LongBool;  syscall WorkbenchBase 9;
function AddAppIconA(ID: LongWord; UserData: LongWord; Text_: PChar; MsgPort: PMsgPort; Lock: BPTR; DiskObj: PDiskObject; TagList: PTagItem): PAppIcon; syscall WorkbenchBase 10;
function RemoveAppIcon(AppIcon: PAppIcon): LongBool; syscall WorkbenchBase 11;
function AddAppMenuItemA(ID: LongWord; UserData: LongWord; Text_: APTR; MsgPort: PMsgPort; TagList: PTagItem): PAppMenuItem; syscall WorkbenchBase 12;
function RemoveAppMenuItem(AppMenuItem: PAppMenuItem): LongBool; syscall WorkbenchBase 13;
{ v39 }
function WBConfig(Unk1: LongWord; Unk2: LongWord): LongBool; syscall WorkbenchBase 14; unimplemented;
function WBInfo(Lock: BPTR; const Name: STRPTR; Screen: PScreen): LongBool; syscall WorkbenchBase 15;
{ v44 }
function OpenWorkbenchObjectA(Name: STRPTR; Tags: PTagItem): LongBool; syscall WorkbenchBase 16;
function CloseWorkbenchObjectA(Name: STRPTR; Tags: PTagItem): LongBool; syscall WorkbenchBase 17; unimplemented;
function WorkbenchControlA(Name: STRPTR; Tags: PTagItem): LongBool; syscall WorkbenchBase 18;
function AddAppWindowDropZoneA(Aw: PAppWindow; ID: LongWord; UserData: LongWord; Tags: PTagItem): PAppWindowDropZone; syscall WorkbenchBase 19;
function RemoveAppWindowDropZone(Aw: PAppWindow; DropZone: PAppWindowDropZone): LongBool; syscall WorkbenchBase 20;
function ChangeWorkbenchSelectionA(Name: STRPTR; Hook: PHook; Tags: PTagItem): LongBool; syscall WorkbenchBase 21; unimplemented;
function MakeWorkbenchObjectVisibleA(Name: STRPTR; Tags: PTagItem): LongBool; syscall WorkbenchBase 22; unimplemented;
{ v45, AROS only ? }
function RegisterWorkbench(MessagePort: PMsgPort): LongBool; syscall WorkbenchBase 23;
function UnregisterWorkbench(MessagePort: PMsgPort): LongBool; syscall WorkbenchBase 24;
function UpdateWorkbenchObjectA(Name: STRPTR; Type_: LongInt; Tags: PTagItem): LongBool; syscall WorkbenchBase 25;
function SendAppWindowMessage(Win: PWindow; NumFiles: LongWord; Files: PPChar; Class_: Word; MouseX: SmallInt; MouseY: SmallInt; Seconds: LongWord; Micros: LongWord): LongBool; syscall WorkbenchBase 26;
function GetNextAppIcon(LastDiskObj: PDiskObject; Text_: PChar): PDiskObject; syscall WorkbenchBase 27;

// varargs versions:
function AddAppIcon(ID: LongWord; UserData: LongWord; Text_: PChar; MsgPort: PMsgPort; Lock: BPTR; DiskObj: PDiskObject; const Tags: array of const): PAppIcon;
function AddAppMenuItem(ID: LongWord; UserData: LongWord; Text_: APTR; MsgPort: PMsgPort; const Tags: array of const): PAppMenuItem;
function AddAppWindow(ID: LongWord; UserData: LongWord; Window: PWindow; MsgPort: PMsgPort; const Tags: array of const): PAppWindow;
function AddAppWindowDropZone(Aw: PAppWindow; ID: LongWord; UserData: LongWord; const Tags: array of const): PAppWindowDropZone;
function CloseWorkbenchObject(Name: STRPTR; const Tags: array of const): LongBool; unimplemented;
function MakeWorkbenchObjectVisible(Name: STRPTR; const Tags: array of const): LongBool; unimplemented;
function OpenWorkbenchObject(Name: STRPTR; const Tags: array of const): LongBool;
function WorkbenchControl(Name: STRPTR; const Tags: array of const): LongBool;

implementation

uses
  TagsArray;


// varargs versions:
function AddAppIcon(ID: LongWord; UserData: LongWord; Text_: PChar; MsgPort: PMsgPort; Lock: BPTR; DiskObj: PDiskObject; const Tags: array of const): PAppIcon;
begin
  AddAppIcon := AddAppIconA(ID, UserData, Text_, MsgPort, Lock, DiskObj, ReadInTags(Tags));
end;


function AddAppMenuItem(ID: LongWord; UserData: LongWord; Text_: APTR; MsgPort: PMsgPort;  const Tags: array of const): PAppMenuItem;
begin
  AddAppMenuItem := AddAppMenuItemA(ID, UserData, Text_, MsgPort, ReadInTags(Tags));
end;


function AddAppWindow(ID: LongWord; UserData: LongWord; Window: PWindow; MsgPort: PMsgPort;  const Tags: array of const): PAppWindow;
begin
  AddAppWindow := AddAppWindowA(ID, UserData, Window, MsgPort, ReadInTags(Tags));
end;


function AddAppWindowDropZone(Aw: PAppWindow; ID: LongWord; UserData: LongWord;  const Tags: array of const): PAppWindowDropZone;
begin
  AddAppWindowDropZone := AddAppWindowDropZoneA(Aw, ID, UserData, ReadInTags(Tags));
end;


function CloseWorkbenchObject(Name: STRPTR;  const Tags: array of const): LongBool;
begin
  CloseWorkbenchObject := CloseWorkbenchObjectA(Name, ReadInTags(Tags));
end;


function MakeWorkbenchObjectVisible(Name: STRPTR;  const Tags: array of const): LongBool;
begin
  MakeWorkbenchObjectVisible := MakeWorkbenchObjectVisibleA(Name, ReadInTags(Tags));
end;


function OpenWorkbenchObject(Name: STRPTR;  const Tags: array of const): LongBool;
begin
  OpenWorkbenchObject := OpenWorkbenchObjectA(Name, ReadInTags(Tags));
end;


function WorkbenchControl(Name: STRPTR;  const Tags: array of const): LongBool;
begin
  WorkbenchControl := WorkbenchControlA(Name, ReadInTags(Tags));
end;


Initialization
  WorkbenchBase := OpenLibrary(WORKBENCHNAME,0);
Finalization
  CloseLibrary(WorkbenchBase);
end.
