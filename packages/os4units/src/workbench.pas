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
{$PACKRECORDS C}

Interface

Uses
   exec, AmigaDos, Utility, Intuition, AGraphics;

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
    do_type: Byte;
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
    am_Message: TMessage;       // Standard message structure.
    am_type: Word;              // Message type.
    am_UserData: LongWord;      // Application specific.
    am_ID: LongWord;            // Application definable ID.
    am_NumArgs: LongInt;        // # of elements in arglist.
    am_ArgList: PWBArgList;     // The arguements themselves.
    am_Version: Word;           // Will be AM_VERSION.
    am_Class: Word;             // Message class.
    am_MouseX: SmallInt;        // Mouse x position of event.
    am_MouseY: SmallInt;        // Mouse y position of event.
    am_Seconds: LongWord;       // Current system clock time.
    am_Micros: LongWord;        // Current system clock time.
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
    adzm_UserData: LongWord;  // / from AddAppWindowDropZoneA().
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
  PAppWindow = ^TAppWindow;
  TAppWindow = record
    aw_PRIVATE: pointer;
  end;

  PAppMenu = ^TAppMenu;
  TAppMenu = record
    am_PRIVATE: pointer;
  end;

  PAppWindowDropZone = ^TAppWindowDropZone;
  TAppWindowDropZone = record
    awdz_PRIVATE: pointer;
  end;

  PAppIcon = ^TAppIcon;
  TAppIcon = record
    ai_PRIVATE: Pointer;
  end;

  PAppMenuItem = ^TAppMenuItem;
  TAppMenuItem = record
    ami_PRIVATE: Pointer;
  end;


const
  WBA_Dummy = TAG_USER + $A000;
  // Tags for use with AddAppIconA()
  // The different menu items the AppIcon responds to (BOOL).
  WBAPPICONA_SupportsOpen        = WBA_Dummy +  1; // AppIcon responds to the "Open" menu item (LongBool).
  WBAPPICONA_SupportsCopy        = WBA_Dummy +  2; // AppIcon responds to the "Copy" menu item (LongBool).
  WBAPPICONA_SupportsRename      = WBA_Dummy +  3; // AppIcon responds to the "Rename" menu item (LongBool).
  WBAPPICONA_SupportsInformation = WBA_Dummy +  4; // AppIcon responds to the "Information" menu item (LongBool).
  WBAPPICONA_SupportsSnapshot    = WBA_Dummy +  5; // AppIcon responds to the "Snapshot" menu item (LongBool).
  WBAPPICONA_SupportsUnSnapshot  = WBA_Dummy +  6; // AppIcon responds to the "UnSnapshot" menu item (LongBool).
  WBAPPICONA_SupportsLeaveOut    = WBA_Dummy +  7; // AppIcon responds to the "LeaveOut" menu item (LongBool).
  WBAPPICONA_SupportsPutAway     = WBA_Dummy +  8; // AppIcon responds to the "PutAway" menu item (LongBool).
  WBAPPICONA_SupportsDelete      = WBA_Dummy +  9; // AppIcon responds to the "Delete" menu item (LongBool).
  WBAPPICONA_SupportsFormatDisk  = WBA_Dummy + 10; // AppIcon responds to the "FormatDisk" menu item (LongBool).
  WBAPPICONA_SupportsEmptyTrash  = WBA_Dummy + 11; // AppIcon responds to the "EmptyTrash" menu item (LongBool).

  WBAPPICONA_PropagatePosition   = WBA_Dummy + 12; // AppIcon position should be propagated back to original DiskObject (LongBool).
  WBAPPICONA_RenderHook          = WBA_Dummy + 13; // Callback hook to be invoked when rendering this icon (pHook).
  WBAPPICONA_NotifySelectState   = WBA_Dummy + 14; // AppIcon wants to be notified when its select state changes (LongBool).

  // Tags for use with AddAppMenuA()
  WBAPPMENUA_CommandKeyString    = WBA_Dummy + 15; // Command key string for this AppMenu (STRPTR).
  WBAPPMENUA_GetKey              = WBA_Dummy + 65; // Item to be added should get sub menu items attached to; make room for it, then return the key to use later for attaching the items (A_PUL
  WBAPPMENUA_UseKey              = WBA_Dummy + 66; // This item should be attached to a sub menu; the key provided refers to the sub menu it should be attached to (LongWord).
  WBAPPMENUA_GetTitleKey         = WBA_Dummy + 77; // Item to be added is in fact a new menu title; make room for it, then return the key to use later for attaching the items (??? ULONG ???).

  // Tags for use with OpenWorkbenchObjectA()
  WBOPENA_ArgLock                = WBA_Dummy + 16; // Corresponds to the wa_Lock member of a struct WBArg.
  WBOPENA_ArgName                = WBA_Dummy + 17; // Corresponds to the wa_Name member of a struct WBArg.
  WBOPENA_Show                   = WBA_Dummy + 75; // When opening a drawer, show all files or only icons? This must be one out of DDFLAGS_SHOWICONS, or DDFLAGS_SHOWALL; (Byte); (V45)
  WBOPENA_ViewBy                 = WBA_Dummy + 76; // When opening a drawer, view the contents by icon, name, date, size or type? This must be one out of DDVM_BYICON, DDVM_BYNAME, DDVM_BYDATE, DDVM_BYSIZE or DDVM_BYTYPE; (UBYTE); (V45)

  // Tags for use with WorkbenchControlA()
  WBCTRLA_IsOpen                 = WBA_Dummy + 18; // Check if the named drawer is currently open (PLongInt).
  WBCTRLA_DuplicateSearchPath    = WBA_Dummy + 19; // Create a duplicate of the Workbench private search path list (PBPTR).
  WBCTRLA_FreeSearchPath         = WBA_Dummy + 20; // Free the duplicated search path list (BPTR).
  WBCTRLA_GetDefaultStackSize    = WBA_Dummy + 21; // Get the default stack size for launching programs with (PLongWord).
  WBCTRLA_SetDefaultStackSize    = WBA_Dummy + 22; // Set the default stack size for launching programs with (LongWord).
  WBCTRLA_RedrawAppIcon          = WBA_Dummy + 23; // Cause an AppIcon to be redrawn (pAppIcon).
  WBCTRLA_GetProgramList         = WBA_Dummy + 24; // Get a list of currently running Workbench programs (pList).
  WBCTRLA_FreeProgramList        = WBA_Dummy + 25; // Release the list of currently running Workbench programs (pList).
  WBCTRLA_GetSelectedIconList    = WBA_Dummy + 36; // Get a list of currently selected icons (pList).
  WBCTRLA_FreeSelectedIconList   = WBA_Dummy + 37; // Release the list of currently selected icons (pList).
  WBCTRLA_GetOpenDrawerList      = WBA_Dummy + 38; // Get a list of currently open drawers (pList).
  WBCTRLA_FreeOpenDrawerList     = WBA_Dummy + 39; // Release the list of currently open icons (pList).
  WBCTRLA_GetHiddenDeviceList    = WBA_Dummy + 42; // Get the list of hidden devices (pList).

  WBCTRLA_FreeHiddenDeviceList   = WBA_Dummy + 43; // Release the list of hidden devices (pList).
  WBCTRLA_AddHiddenDeviceName    = WBA_Dummy + 44; // Add the name of a device which Workbench should never try to read a disk icon from (STRPTR).
  WBCTRLA_RemoveHiddenDeviceName = WBA_Dummy + 45; // Remove a name from list of hidden devices (STRPTR).
  WBCTRLA_GettypeRestartTime     = WBA_Dummy + 47; // Get the number of seconds that have to pass before typing the next character in a drawer window will restart with a new file name (PLongWord).
  WBCTRLA_SettypeRestartTime     = WBA_Dummy + 48; // Set the number of seconds that have to pass before typing the next character in a drawer window will restart with a new file name (LongWord).
  WBCTRLA_GetCopyHook            = WBA_Dummy + 69; // Obtain the hook that will be invoked when Workbench starts to copy files and data (pHook); (V45)
  WBCTRLA_SetCopyHook            = WBA_Dummy + 70; // Install the hook that will be invoked when Workbench starts to copy files and data (pHook); (V45)
  WBCTRLA_GetDeleteHook          = WBA_Dummy + 71; // Obtain the hook that will be invoked when Workbench discards files and drawers or empties the trashcan (pHook);  (V45).
  WBCTRLA_SetDeleteHook          = WBA_Dummy + 72; // Install the hook that will be invoked when Workbench discards  files and drawers or empties the trashcan (pHook); (V45).
  WBCTRLA_GetTextInputHook       = WBA_Dummy + 73; // Obtain the hook that will be invoked when Workbench requests that the user enters text, such as when a file is to be renamed  or a new drawer is to be created (pHook); (V45)
  WBCTRLA_SetTextInputHook       = WBA_Dummy + 74; // Install the hook that will be invoked when Workbench requests that the user enters text, such as when a file is to be renamed or a new drawer is to be created (pHook); (V45)
  WBCTRLA_AddSetupCleanupHook    = WBA_Dummy + 78; // Add a hook that will be invoked when Workbench is about to shut down (cleanup), and when Workbench has returned to operational state (setup) (pHook); (V45)
  WBCTRLA_RemSetupCleanupHook    = WBA_Dummy + 79; // Remove a hook that has been installed with the WBCTRLA_AddSetupCleanupHook tag (pHook); (V45)

  // Tags for use with AddAppWindowDropZoneA()
  WBDZA_Left      = WBA_Dummy + 26; // Zone left edge (SmallInt)
  WBDZA_RelRight  = WBA_Dummy + 27; // Zone left edge, if relative to the right edge of the window (SmallInt)
  WBDZA_Top       = WBA_Dummy + 28; // Zone top edge (SmallInt)
  WBDZA_RelBottom = WBA_Dummy + 29; // Zone top edge, if relative to the bottom edge of the window (SmallInt)
  WBDZA_Width     = WBA_Dummy + 30; // Zone width (SmallInt)
  WBDZA_RelWidth  = WBA_Dummy + 31; // Zone width, if relative to the window width (SmallInt)
  WBDZA_Height    = WBA_Dummy + 32; // Zone height (SmallInt)
  WBDZA_RelHeight = WBA_Dummy + 33; // Zone height, if relative to the window height (SmallInt)
  WBDZA_Box       = WBA_Dummy + 34; // Zone position and size (pIBox).
  WBDZA_Hook      = WBA_Dummy + 35; // Hook to invoke when the mouse enters or leave a drop zone (pHook).

  // Reserved tags; don't use!  }
  WBA_Reserved1  = WBA_Dummy + 40;
  WBA_Reserved2  = WBA_Dummy + 41;
  WBA_Reserved3  = WBA_Dummy + 46;
  WBA_Reserved4  = WBA_Dummy + 49;
  WBA_Reserved5  = WBA_Dummy + 50;
  WBA_Reserved6  = WBA_Dummy + 51;
  WBA_Reserved7  = WBA_Dummy + 52;
  WBA_Reserved8  = WBA_Dummy + 53;
  WBA_Reserved9  = WBA_Dummy + 54;
  WBA_Reserved10 = WBA_Dummy + 55;
  WBA_Reserved11 = WBA_Dummy + 56;
  WBA_Reserved12 = WBA_Dummy + 57;
  WBA_Reserved13 = WBA_Dummy + 58;
  WBA_Reserved14 = WBA_Dummy + 59;
  WBA_Reserved15 = WBA_Dummy + 60;
  WBA_Reserved16 = WBA_Dummy + 61;
  WBA_Reserved17 = WBA_Dummy + 62;
  WBA_Reserved18 = WBA_Dummy + 63;
  WBA_Reserved19 = WBA_Dummy + 64;

  // Last Tag
  WBA_LAST_TAG = WBA_Dummy + 64;

  // Parameters for the UpdateWorkbench() function.
  UPDATEWB_ObjectRemoved = 0; // Object has been deleted.
  UPDATEWB_ObjectAdded   = 1; // Object is new or has changed.

const

// each message that comes into the WorkBenchPort must have a type field in the preceeding short.  These are the defines for this type

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
  WorkbenchBase: PLibrary;
  IWorkbench: PInterface;

function WBObtain(): LongWord; syscall IWorkbench 60;
function WBRelease(): LongWord; syscall IWorkbench 64;
procedure WBExpunge(); syscall IWorkbench 68;
function WBClone(): PInterface; syscall IWorkbench 72;
procedure UpdateWorkbench(const Name: STRPTR; Lock: BPTR; Action: LongInt); syscall IWorkbench 76;
// 80 private
// 84 private
function AddAppWindowA(ID: LongWord; UserData: LongWord; Window: PWindow; MsgPort: PMsgPort; TagList: PTagItem): PAppWindow;  syscall IWorkbench 88;
// 92 AddAppWindow
function RemoveAppWindow(AppWindow: PAppWindow): LongBool;  syscall IWorkbench 96;
function AddAppIconA(ID: LongWord; UserData: LongWord; Text_: PChar; MsgPort: PMsgPort; Lock: BPTR; DiskObj: PDiskObject; TagList: PTagItem): PAppIcon; syscall IWorkbench 100;
// 104 AddAppIcon
function RemoveAppIcon(AppIcon: PAppIcon): LongBool; syscall IWorkbench 108;
function AddAppMenuItemA(ID: LongWord; UserData: LongWord; Text_: APTR; MsgPort: PMsgPort; TagList: PTagItem): PAppMenuItem; syscall IWorkbench 112;
// 116
function RemoveAppMenuItem(AppMenuItem: PAppMenuItem): LongBool; syscall IWorkbench 120;
// 124 private
function WBInfo(Lock: BPTR; const Name: STRPTR; Screen: PScreen): LongBool; syscall IWorkbench 128;
function OpenWorkbenchObjectA(Name: STRPTR; Tags: PTagItem): LongBool; syscall IWorkbench 132;
// 136 OpenWorkbenchObject
function CloseWorkbenchObjectA(Name: STRPTR; Tags: PTagItem): LongBool; syscall IWorkbench 140;
// 144 CloseWorkbenchObject
function WorkbenchControlA(Name: STRPTR; Tags: PTagItem): LongBool; syscall IWorkbench 148;
// 152 WorkbenchControl
function AddAppWindowDropZoneA(Aw: PAppWindow; ID: LongWord; UserData: LongWord; Tags: PTagItem): PAppWindowDropZone; syscall IWorkbench 156;
// 160 AddAppWindowDropZone
function RemoveAppWindowDropZone(Aw: PAppWindow; DropZone: PAppWindowDropZone): LongBool; syscall IWorkbench 164;
function ChangeWorkbenchSelectionA(Name: STRPTR; Hook: PHook; Tags: PTagItem): LongBool; syscall IWorkbench 168;
// 172 ChangeWorkbenchSelection
function MakeWorkbenchObjectVisibleA(Name: STRPTR; Tags: PTagItem): LongBool; syscall IWorkbench 176;
// 180 MakeWorkbenchObjectVisible
function WhichWorkbenchObjectA(Window: PWindow; X, Y: LongInt; Tags: PTagItem): LongBool; syscall IWorkbench 184;
// 188 WhichWorkbenchObject
// 192 private

// varargs versions:
function AddAppIcon(ID: LongWord; UserData: LongWord; Text_: PChar; MsgPort: PMsgPort; Lock: BPTR; DiskObj: PDiskObject; const Tags: array of PtrUInt): PAppIcon;
function AddAppMenuItem(ID: LongWord; UserData: LongWord; Text_: APTR; MsgPort: PMsgPort; const Tags: array of PtrUInt): PAppMenuItem;
function AddAppWindow(ID: LongWord; UserData: LongWord; Window: PWindow; MsgPort: PMsgPort; const Tags: array of PtrUInt): PAppWindow;
function AddAppWindowDropZone(Aw: PAppWindow; ID: LongWord; UserData: LongWord; const Tags: array of PtrUInt): PAppWindowDropZone;
function CloseWorkbenchObject(Name: STRPTR; const Tags: array of PtrUInt): LongBool; unimplemented;
function MakeWorkbenchObjectVisible(Name: STRPTR; const Tags: array of PtrUInt): LongBool; unimplemented;
function OpenWorkbenchObject(Name: STRPTR; const Tags: array of PtrUInt): LongBool;
function WorkbenchControl(Name: STRPTR; const Tags: array of PtrUInt): LongBool;

implementation


// varargs versions:
function AddAppIcon(ID: LongWord; UserData: LongWord; Text_: PChar; MsgPort: PMsgPort; Lock: BPTR; DiskObj: PDiskObject; const Tags: array of PtrUInt): PAppIcon; inline;
begin
  AddAppIcon := AddAppIconA(ID, UserData, Text_, MsgPort, Lock, DiskObj, @Tags);
end;


function AddAppMenuItem(ID: LongWord; UserData: LongWord; Text_: APTR; MsgPort: PMsgPort;  const Tags: array of PtrUInt): PAppMenuItem; inline;
begin
  AddAppMenuItem := AddAppMenuItemA(ID, UserData, Text_, MsgPort, @Tags);
end;


function AddAppWindow(ID: LongWord; UserData: LongWord; Window: PWindow; MsgPort: PMsgPort;  const Tags: array of PtrUInt): PAppWindow; inline;
begin
  AddAppWindow := AddAppWindowA(ID, UserData, Window, MsgPort, @Tags);
end;


function AddAppWindowDropZone(Aw: PAppWindow; ID: LongWord; UserData: LongWord;  const Tags: array of PtrUInt): PAppWindowDropZone; inline;
begin
  AddAppWindowDropZone := AddAppWindowDropZoneA(Aw, ID, UserData, @Tags);
end;


function CloseWorkbenchObject(Name: STRPTR;  const Tags: array of PtrUInt): LongBool; inline;
begin
  CloseWorkbenchObject := CloseWorkbenchObjectA(Name, @Tags);
end;


function MakeWorkbenchObjectVisible(Name: STRPTR;  const Tags: array of PtrUInt): LongBool; inline;
begin
  MakeWorkbenchObjectVisible := MakeWorkbenchObjectVisibleA(Name, @Tags);
end;


function OpenWorkbenchObject(Name: STRPTR;  const Tags: array of PtrUInt): LongBool; inline;
begin
  OpenWorkbenchObject := OpenWorkbenchObjectA(Name, @Tags);
end;


function WorkbenchControl(Name: STRPTR;  const Tags: array of PtrUInt): LongBool; inline;
begin
  WorkbenchControl := WorkbenchControlA(Name, @Tags);
end;


Initialization
  WorkbenchBase := OpenLibrary(WORKBENCHNAME,0);
  if Assigned(WorkbenchBase) then
    IWorkbench := GetInterface(WorkbenchBase, 'main', 1, nil);
finalization
  if Assigned(IWorkbench) then
    DropInterface(IWorkbench);
  if Assigned(WorkbenchBase) then
    CloseLibrary(WorkbenchBase);
end.
