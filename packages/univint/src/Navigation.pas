{
     File:       Navigation.p
 
     Contains:   Navigation Services Interfaces
 
     Version:    Technology: Navigation 3.0
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  © 1996-2002 by Apple Computer, Inc., all rights reserved
 
     Bugs?:      For bug reports, consult the following page on
                 the World Wide Web:
 
                     http://www.freepascal.org/bugs.html
 
}


{
    Modified for use with Free Pascal
    Version 210
    Please report any bugs to <gpc@microbizz.nl>
}

{$mode macpas}
{$packenum 1}
{$macro on}
{$inline on}
{$calling mwpascal}

unit Navigation;
interface
{$setc UNIVERSAL_INTERFACES_VERSION := $0342}
{$setc GAP_INTERFACES_VERSION := $0210}

{$ifc not defined USE_CFSTR_CONSTANT_MACROS}
    {$setc USE_CFSTR_CONSTANT_MACROS := TRUE}
{$endc}

{$ifc defined CPUPOWERPC and defined CPUI386}
	{$error Conflicting initial definitions for CPUPOWERPC and CPUI386}
{$endc}
{$ifc defined FPC_BIG_ENDIAN and defined FPC_LITTLE_ENDIAN}
	{$error Conflicting initial definitions for FPC_BIG_ENDIAN and FPC_LITTLE_ENDIAN}
{$endc}

{$ifc not defined __ppc__ and defined CPUPOWERPC}
	{$setc __ppc__ := 1}
{$elsec}
	{$setc __ppc__ := 0}
{$endc}
{$ifc not defined __i386__ and defined CPUI386}
	{$setc __i386__ := 1}
{$elsec}
	{$setc __i386__ := 0}
{$endc}

{$ifc defined __ppc__ and __ppc__ and defined __i386__ and __i386__}
	{$error Conflicting definitions for __ppc__ and __i386__}
{$endc}

{$ifc defined __ppc__ and __ppc__}
	{$setc TARGET_CPU_PPC := TRUE}
	{$setc TARGET_CPU_X86 := FALSE}
{$elifc defined __i386__ and __i386__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_X86 := TRUE}
{$elsec}
	{$error Neither __ppc__ nor __i386__ is defined.}
{$endc}
{$setc TARGET_CPU_PPC_64 := FALSE}

{$ifc defined FPC_BIG_ENDIAN}
	{$setc TARGET_RT_BIG_ENDIAN := TRUE}
	{$setc TARGET_RT_LITTLE_ENDIAN := FALSE}
{$elifc defined FPC_LITTLE_ENDIAN}
	{$setc TARGET_RT_BIG_ENDIAN := FALSE}
	{$setc TARGET_RT_LITTLE_ENDIAN := TRUE}
{$elsec}
	{$error Neither FPC_BIG_ENDIAN nor FPC_LITTLE_ENDIAN are defined.}
{$endc}
{$setc ACCESSOR_CALLS_ARE_FUNCTIONS := TRUE}
{$setc CALL_NOT_IN_CARBON := FALSE}
{$setc OLDROUTINENAMES := FALSE}
{$setc OPAQUE_TOOLBOX_STRUCTS := TRUE}
{$setc OPAQUE_UPP_TYPES := TRUE}
{$setc OTCARBONAPPLICATION := TRUE}
{$setc OTKERNEL := FALSE}
{$setc PM_USE_SESSION_APIS := TRUE}
{$setc TARGET_API_MAC_CARBON := TRUE}
{$setc TARGET_API_MAC_OS8 := FALSE}
{$setc TARGET_API_MAC_OSX := TRUE}
{$setc TARGET_CARBON := TRUE}
{$setc TARGET_CPU_68K := FALSE}
{$setc TARGET_CPU_MIPS := FALSE}
{$setc TARGET_CPU_SPARC := FALSE}
{$setc TARGET_OS_MAC := TRUE}
{$setc TARGET_OS_UNIX := FALSE}
{$setc TARGET_OS_WIN32 := FALSE}
{$setc TARGET_RT_MAC_68881 := FALSE}
{$setc TARGET_RT_MAC_CFM := FALSE}
{$setc TARGET_RT_MAC_MACHO := TRUE}
{$setc TYPED_FUNCTION_POINTERS := TRUE}
{$setc TYPE_BOOL := FALSE}
{$setc TYPE_EXTENDED := FALSE}
{$setc TYPE_LONGLONG := TRUE}
uses MacTypes,AEDataModel,CFBase,Quickdraw,Finder,Events,AppleEvents,Translation,MacWindows,CodeFragments,MacErrors,CFArray,CFString;
{#I+}


{$ALIGN MAC68K}


type
	NavAskSaveChangesAction 	= UInt32;
const
																{  input action codes for NavAskSaveChanges()  }
	kNavSaveChangesClosingDocument = 1;
	kNavSaveChangesQuittingApplication = 2;
	kNavSaveChangesOther		= 0;


type
	NavAskSaveChangesResult 	= UInt32;
const
																{  result codes for NavAskSaveChanges()  }
	kNavAskSaveChangesSave		= 1;
	kNavAskSaveChangesCancel	= 2;
	kNavAskSaveChangesDontSave	= 3;


type
	NavAskDiscardChangesResult 	= UInt32;
const
																{  result codes for NavAskDiscardChanges()  }
	kNavAskDiscardChanges		= 1;
	kNavAskDiscardChangesCancel	= 2;


type
	NavFilterModes 				= SInt16;
const
																{  which elements are being filtered for objects:  }
	kNavFilteringBrowserList	= 0;
	kNavFilteringFavorites		= 1;
	kNavFilteringRecents		= 2;
	kNavFilteringShortCutVolumes = 3;
	kNavFilteringLocationPopup	= 4;							{  for v1.1 or greater  }


	kNavFileOrFolderVersion		= 1;


type
	NavFileOrFolderInfoPtr = ^NavFileOrFolderInfo;
	NavFileOrFolderInfo = record
		version:				UInt16;
		isFolder:				boolean;
		visible:				boolean;
		creationDate:			UInt32;
		modificationDate:		UInt32;
		case SInt16 of
		0: (
			locked:				boolean;								{  file is locked  }
			resourceOpen:		boolean;								{  resource fork is opened  }
			dataOpen:			boolean;								{  data fork is opened  }
			reserved1:			boolean;
			dataSize:			UInt32;									{  size of the data fork  }
			resourceSize:		UInt32;									{  size of the resource fork  }
			finderInfo:			FInfo;									{  more file info:  }
			finderXInfo:		FXInfo;
		   );
		1: (
			shareable:			boolean;
			sharePoint:			boolean;
			mounted:			boolean;
			readable:			boolean;
			writeable:			boolean;
			reserved2:			boolean;
			numberOfFiles:		UInt32;
			finderDInfo:		DInfo;
			finderDXInfo:		DXInfo;
			folderType:			OSType;									{  package type, For struct version >= 1  }
			folderCreator:		OSType;									{  package creator, For struct version >= 1  }
			reserved3:			packed array [0..205] of char;
		   );
	end;

	NavEventDataInfoPtr = ^NavEventDataInfo;
	NavEventDataInfo = record
		case SInt16 of
		0: (
			event:				EventRecordPtr;							{  for event processing  }
			);
		1: (
			param:				Ptr;									{  points to event specific data  }
			);
	end;

	NavEventDataPtr = ^NavEventData;
	NavEventData = record
		eventDataParms:			NavEventDataInfo;						{  the event data  }
		itemHit:				SInt16;									{  the dialog item number, for v1.1 or greater  }
	end;


	{
	 *  NavDialogRef
	 *  
	 *  Summary:
	 *    Opaque Navigation Services dialog identifier
	 *  
	 *  Discussion:
	 *    A NavDialogRef is an opaque reference to an instance of a
	 *    Navigation Services dialog. A new NavDialogRef is returned from
	 *    any of the NavCreate*Dialog functions and is later disposed with
	 *    the NavDialogDispose function. NavDialogRef is the new name for
	 *    the NavContext type, and thus when a client's event proc is
	 *    called, the value of the NavCBRec.context field is the same as
	 *    the NavDialogRef returned from the corresponding
	 *    NavCreate*Dialog. A NavDialogRef is distinct from, and is not
	 *    interchangable with, a Dialog Manager DialogRef.
	 	}
	NavDialogRef    = ^SInt32; { an opaque 32-bit type }
	NavDialogRefPtr = ^NavDialogRef;  { when a var xx:NavDialogRef parameter can be nil, it is changed to xx: NavDialogRefPtr }
{$ifc CALL_NOT_IN_CARBON}
	{	 NavContext is the old name for NavDialogRef 	}
	NavContext							= NavDialogRef;
{$endc}  {CALL_NOT_IN_CARBON}


	{
	 *  NavUserAction
	 *  
	 *  Summary:
	 *    Indicates an action taken by the user
	 *  
	 *  Discussion:
	 *    When the user clicks a button at the bottom of a Nav Services
	 *    dialog (or makes an equivalent mouse or key gesture), a
	 *    kNavCBUserAction event is sent to the client's event proc
	 *    indicating which action was taken. Often, the action also
	 *    dismisses the dialog. User action events are only generated when
	 *    using dialogs created from a NavCreate*Dialog function. In the
	 *    special case of a modeless GetFile dialog (supported only on Mac
	 *    OS X), the user can option-click on the open button to keep the
	 *    dialog from being dismissed, but the kNavCBUserAction event is
	 *    sent so the client can get the reply record and open the selected
	 *    files.
	 	}
	NavUserAction 				= UInt32;
const
	kNavUserActionNone			= 0;
	kNavUserActionCancel		= 1;
	kNavUserActionOpen			= 2;
	kNavUserActionSaveAs		= 3;
	kNavUserActionChoose		= 4;
	kNavUserActionNewFolder		= 5;
	kNavUserActionSaveChanges	= 6;
	kNavUserActionDontSaveChanges = 7;
	kNavUserActionDiscardChanges = 8;
	kNavUserActionReviewDocuments = 9;
	kNavUserActionDiscardDocuments = 10;


	kNavCBRecVersion			= 1;


	{
	 *  NavCBRec
	 *  
	 *  Summary:
	 *    A structure passed to event and preview callbacks
	 *  
	 *  Discussion:
	 *    The NavCBRec structure is passed to the client's event proc or
	 *    custom preview proc. It provides information that is specific to
	 *    each event type. New for Carbon: the userAction field.
	 	}

type
	NavCBRecPtr = ^NavCBRec;
	NavCBRec = record
		version:				UInt16;
		context:				NavDialogRef;
		window:					WindowRef;
		customRect:				Rect;
		previewRect:			Rect;
		eventData:				NavEventData;
		userAction:				NavUserAction;
		reserved:				packed array [0..217] of char;
	end;


	{
	 *  NavEventCallbackMessage
	 *  
	 *  Summary:
	 *    Identifies the message type being sent to the client's event proc
	 	}
	NavEventCallbackMessage 	= SInt32;
const
	kNavCBEvent					= 0;
	kNavCBCustomize				= 1;
	kNavCBStart					= 2;
	kNavCBTerminate				= 3;
	kNavCBAdjustRect			= 4;
	kNavCBNewLocation			= 5;
	kNavCBShowDesktop			= 6;
	kNavCBSelectEntry			= 7;
	kNavCBPopupMenuSelect		= 8;
	kNavCBAccept				= 9;
	kNavCBCancel				= 10;
	kNavCBAdjustPreview			= 11;
	kNavCBUserAction			= 12;
	kNavCBOpenSelection			= $80000000;


type
	NavCallBackUserData					= Ptr;
	{	 for events and customization: 	}
{$ifc TYPED_FUNCTION_POINTERS}
	NavEventProcPtr = procedure(callBackSelector: NavEventCallbackMessage; callBackParms: NavCBRecPtr; callBackUD: UnivPtr);
{$elsec}
	NavEventProcPtr = ProcPtr;
{$endc}

	{	 for preview support: 	}
{$ifc TYPED_FUNCTION_POINTERS}
	NavPreviewProcPtr = function(callBackParms: NavCBRecPtr; callBackUD: UnivPtr): boolean;
{$elsec}
	NavPreviewProcPtr = ProcPtr;
{$endc}

	{	 filtering callback information: 	}
{$ifc TYPED_FUNCTION_POINTERS}
	NavObjectFilterProcPtr = function(var theItem: AEDesc; info: NavFileOrFolderInfoPtr; callBackUD: UnivPtr; filterMode: NavFilterModes): boolean;
{$elsec}
	NavObjectFilterProcPtr = ProcPtr;
{$endc}

{$ifc OPAQUE_UPP_TYPES}
	NavEventUPP = ^SInt32; { an opaque UPP }
{$elsec}
	NavEventUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	NavPreviewUPP = ^SInt32; { an opaque UPP }
{$elsec}
	NavPreviewUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	NavObjectFilterUPP = ^SInt32; { an opaque UPP }
{$elsec}
	NavObjectFilterUPP = UniversalProcPtr;
{$endc}	

const
	uppNavEventProcInfo = $00000FC0;
	uppNavPreviewProcInfo = $000003D0;
	uppNavObjectFilterProcInfo = $00002FD0;
	{
	 *  NewNavEventUPP()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   available as macro/inline
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function NewNavEventUPP(userRoutine: NavEventProcPtr): NavEventUPP; external name '_NewNavEventUPP'; { old name was NewNavEventProc }
{
 *  NewNavPreviewUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewNavPreviewUPP(userRoutine: NavPreviewProcPtr): NavPreviewUPP; external name '_NewNavPreviewUPP'; { old name was NewNavPreviewProc }
{
 *  NewNavObjectFilterUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewNavObjectFilterUPP(userRoutine: NavObjectFilterProcPtr): NavObjectFilterUPP; external name '_NewNavObjectFilterUPP'; { old name was NewNavObjectFilterProc }
{
 *  DisposeNavEventUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeNavEventUPP(userUPP: NavEventUPP); external name '_DisposeNavEventUPP';
{
 *  DisposeNavPreviewUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeNavPreviewUPP(userUPP: NavPreviewUPP); external name '_DisposeNavPreviewUPP';
{
 *  DisposeNavObjectFilterUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeNavObjectFilterUPP(userUPP: NavObjectFilterUPP); external name '_DisposeNavObjectFilterUPP';
{
 *  InvokeNavEventUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure InvokeNavEventUPP(callBackSelector: NavEventCallbackMessage; callBackParms: NavCBRecPtr; callBackUD: UnivPtr; userRoutine: NavEventUPP); external name '_InvokeNavEventUPP'; { old name was CallNavEventProc }
{
 *  InvokeNavPreviewUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeNavPreviewUPP(callBackParms: NavCBRecPtr; callBackUD: UnivPtr; userRoutine: NavPreviewUPP): boolean; external name '_InvokeNavPreviewUPP'; { old name was CallNavPreviewProc }
{
 *  InvokeNavObjectFilterUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeNavObjectFilterUPP(var theItem: AEDesc; info: UnivPtr; callBackUD: UnivPtr; filterMode: NavFilterModes; userRoutine: NavObjectFilterUPP): boolean; external name '_InvokeNavObjectFilterUPP'; { old name was CallNavObjectFilterProc }
type
	NavCustomControlMessage 	= SInt32;
const
	kNavCtlShowDesktop			= 0;							{     show desktop,           parms = nil  }
	kNavCtlSortBy				= 1;							{     sort key field,       parms->NavSortKeyField  }
	kNavCtlSortOrder			= 2;							{     sort order,              parms->NavSortOrder  }
	kNavCtlScrollHome			= 3;							{     scroll list home,       parms = nil  }
	kNavCtlScrollEnd			= 4;							{     scroll list end,      parms = nil  }
	kNavCtlPageUp				= 5;							{     page list up,          parms = nil  }
	kNavCtlPageDown				= 6;							{     page list down,          parms = nil  }
	kNavCtlGetLocation			= 7;							{     get current location,   parms<-AEDesc*  }
	kNavCtlSetLocation			= 8;							{     set current location,   parms->AEDesc*  }
	kNavCtlGetSelection			= 9;							{     get current selection,     parms<-AEDescList*  }
	kNavCtlSetSelection			= 10;							{     set current selection,     parms->AEDescList*  }
	kNavCtlShowSelection		= 11;							{     make selection visible,       parms = nil  }
	kNavCtlOpenSelection		= 12;							{     open view of selection,       parms = nil  }
	kNavCtlEjectVolume			= 13;							{     eject volume,          parms->vRefNum  }
	kNavCtlNewFolder			= 14;							{     create a new folder,     parms->StringPtr  }
	kNavCtlCancel				= 15;							{     cancel dialog,          parms = nil  }
	kNavCtlAccept				= 16;							{     accept dialog default,     parms = nil  }
	kNavCtlIsPreviewShowing		= 17;							{     query preview status,   parms<-Boolean  }
	kNavCtlAddControl			= 18;							{   add one control to dialog,    parms->ControlHandle  }
	kNavCtlAddControlList		= 19;							{     add control list to dialog,    parms->Handle (DITL rsrc)  }
	kNavCtlGetFirstControlID	= 20;							{     get 1st control ID,         parms<-UInt16  }
	kNavCtlSelectCustomType		= 21;							{     select a custom menu item  parms->NavMenuItemSpec*  }
	kNavCtlSelectAllType		= 22;							{   select an "All" menu item parms->SInt16  }
	kNavCtlGetEditFileName		= 23;							{     get save dlog's file name  parms<-StringPtr  }
	kNavCtlSetEditFileName		= 24;							{     set save dlog's file name  parms->StringPtr  }
	kNavCtlSelectEditFileName	= 25;							{     select save dlog file name parms->ControlEditTextSelectionRec*, v1.1 or greater  }
	kNavCtlBrowserSelectAll		= 26;							{   re-scan the browser list  parms = nil, v2.0 or greater  }
	kNavCtlGotoParent			= 27;							{   navigate to parent         parms = nil, v2.0 or greater  }
	kNavCtlSetActionState		= 28;							{   restrict navigation      parms->NavActionState (flags), v2.0 or greater  }
	kNavCtlBrowserRedraw		= 29;							{   rescan browser list      parms = nil, v2.0 or greater  }
	kNavCtlTerminate			= 30;							{   terminate/dismiss dialog  parms = nil, v2.0 or greater  }


type
	NavActionState 				= UInt32;
const
	kNavNormalState				= $00000000;					{  normal/default state  }
	kNavDontOpenState			= $00000001;					{  disallow opening files/folders  }
	kNavDontSaveState			= $00000002;					{  disallow saving files  }
	kNavDontChooseState			= $00000004;					{  disallow choosing objects  }
	kNavDontNewFolderState		= $00000010;					{  disallow creating new folders  }


type
	NavPopupMenuItem 			= UInt16;
const
	kNavAllKnownFiles			= 0;
	kNavAllReadableFiles		= 1;
	kNavAllFiles				= 2;


type
	NavSortKeyField 			= UInt16;
const
	kNavSortNameField			= 0;
	kNavSortDateField			= 1;


type
	NavSortOrder 				= UInt16;
const
	kNavSortAscending			= 0;
	kNavSortDescending			= 1;


type
	NavDialogOptionFlags 		= UInt32;
const
	kNavDefaultNavDlogOptions	= $000000E4;					{  use defaults for all the options  }
	kNavNoTypePopup				= $00000001;					{  don't show file type/extension popup on Open/Save  }
	kNavDontAutoTranslate		= $00000002;					{  don't automatically translate on Open  }
	kNavDontAddTranslateItems	= $00000004;					{  don't add translation choices on Open/Save  }
	kNavAllFilesInPopup			= $00000010;					{  "All Files" menu item in the type popup on Open  }
	kNavAllowStationery			= $00000020;					{  allow saving of stationery files  }
	kNavAllowPreviews			= $00000040;					{  allow preview to show  }
	kNavAllowMultipleFiles		= $00000080;					{  allow multiple items to be selected  }
	kNavAllowInvisibleFiles		= $00000100;					{  allow invisible items to be shown  }
	kNavDontResolveAliases		= $00000200;					{  don't resolve aliases  }
	kNavSelectDefaultLocation	= $00000400;					{  make the default location the browser selection  }
	kNavSelectAllReadableItem	= $00000800;					{  make the dialog select "All Readable Documents" on open  }
	kNavSupportPackages			= $00001000;					{  recognize file system packages, v2.0 or greater  }
	kNavAllowOpenPackages		= $00002000;					{  allow opening of packages, v2.0 or greater  }
	kNavDontAddRecents			= $00004000;					{  don't add chosen objects to the recents list, v2.0 or greater  }
	kNavDontUseCustomFrame		= $00008000;					{  don't draw the custom area bevel frame, v2.0 or greater  }
	kNavDontConfirmReplacement	= $00010000;					{  don't show the "Replace File?" alert on save conflict, v3.0 or greater  }
	kNavPreserveSaveFileExtension = $00020000;					{  extension in default file name is preserved and initially hidden, v3.1 or greater  }


type
	NavTranslationOptions 		= UInt32;
const
	kNavTranslateInPlace		= 0;							{     translate in place, replacing translation source file (default for Save)  }
	kNavTranslateCopy			= 1;							{     translate to a copy of the source file (default for Open)  }


	kNavMenuItemSpecVersion		= 0;


type
	NavMenuItemSpecPtr = ^NavMenuItemSpec;
	NavMenuItemSpec = record
		version:				UInt16;
		menuCreator:			OSType;
		menuType:				OSType;
		menuItemName:			Str255;
		reserved:				packed array [0..244] of char;
	end;

	NavMenuItemSpecArray				= array [0..0] of NavMenuItemSpec;
	NavMenuItemSpecArrayPtr				= ^NavMenuItemSpecArray;
	NavMenuItemSpecArrayHandle			= ^NavMenuItemSpecArrayPtr;
	NavMenuItemSpecHandle				= NavMenuItemSpecArrayHandle;

const
	kNavGenericSignature		= FourCharCode('****');


type
	NavTypeListPtr = ^NavTypeList;
	NavTypeList = record
		componentSignature:		OSType_fix;
		reserved:				SInt16;
		osTypeCount:			SInt16;
		osType:					array [0..0] of OSType_fix;
	end;

	NavTypeListHandle					= ^NavTypeListPtr;

const
	kNavDialogOptionsVersion	= 0;


type
	NavDialogOptionsPtr = ^NavDialogOptions;
	NavDialogOptions = record
		version:				UInt16;
		dialogOptionFlags:		NavDialogOptionFlags;					{  option flags for affecting the dialog's behavior  }
		location:				Point;									{  top-left location of the dialog, or (-1,-1) for default position  }
		clientName:				Str255;
		windowTitle:			Str255;
		actionButtonLabel:		Str255;									{  label of the default button (or null string for default)  }
		cancelButtonLabel:		Str255;									{  label of the cancel button (or null string for default)  }
		savedFileName:			Str255;									{  default name for text box in NavPutFile (or null string for default)  }
		message:				Str255;									{  custom message prompt (or null string for default)  }
		preferenceKey:			UInt32;									{  a key for to managing preferences for using multiple utility dialogs  }
		popupExtension:			NavMenuItemSpecArrayHandle;				{  extended popup menu items, an array of NavMenuItemSpecs  }
		reserved:				packed array [0..493] of char;
	end;


const
	kNavReplyRecordVersion		= 2;


	{
	 *  NavReplyRecord
	 *  
	 *  Summary:
	 *    A structure describing the results of a Nav Services dialog
	 *  
	 *  Discussion:
	 *    A reply record is the result of a Nav Services file dialog. Using
	 *    the older API, which is always modal, the client passes the
	 *    address of a reply record when invoking the dialog. In the Carbon
	 *    API, dialogs may also be window modal or modeless, so the client
	 *    requests the reply record by calling NavDialogGetReply when a
	 *    kNavCBUserAction event is received. Either way, a reply record
	 *    should be disposed of using NavDisposeReply.
	 	}

type
	NavReplyRecordPtr = ^NavReplyRecord;
	NavReplyRecord = record
		version:				UInt16;
		validRecord:			boolean;
		replacing:				boolean;
		isStationery:			boolean;
		translationNeeded:		boolean;
		selection:				AEDescList;
		keyScript:				ScriptCode;
		fileTranslation:		FileTranslationSpecArrayHandle;
		reserved1:				UInt32;
		saveFileName:			CFStringRef;
		saveFileExtensionHidden: boolean;
		reserved2:				SInt8;
		reserved:				packed array [0..224] of char;
	end;

	{
	 *  NavLoad()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in NavigationLib 1.0 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         not available
	 	}
function NavLoad: OSErr; external name '_NavLoad';

{
 *  NavUnload()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in NavigationLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         not available
 }
function NavUnload: OSErr; external name '_NavUnload';

{
 *  NavLibraryVersion()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in NavigationLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NavLibraryVersion: UInt32; external name '_NavLibraryVersion';

{
 *  NavGetDefaultDialogOptions()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in NavigationLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NavGetDefaultDialogOptions(var dialogOptions: NavDialogOptions): OSErr; external name '_NavGetDefaultDialogOptions';


{
 *  NavGetFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in NavigationLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NavGetFile(defaultLocation: AEDescPtr; var reply: NavReplyRecord; dialogOptions: NavDialogOptionsPtr; eventProc: NavEventUPP; previewProc: NavPreviewUPP; filterProc: NavObjectFilterUPP; typeList: NavTypeListHandle; callBackUD: UnivPtr): OSErr; external name '_NavGetFile';

{
 *  NavPutFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in NavigationLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NavPutFile(defaultLocation: AEDescPtr; var reply: NavReplyRecord; dialogOptions: NavDialogOptionsPtr; eventProc: NavEventUPP; fileType: OSType; fileCreator: OSType; callBackUD: UnivPtr): OSErr; external name '_NavPutFile';

{
 *  NavAskSaveChanges()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in NavigationLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NavAskSaveChanges(var dialogOptions: NavDialogOptions; action: NavAskSaveChangesAction; var reply: NavAskSaveChangesResult; eventProc: NavEventUPP; callBackUD: UnivPtr): OSErr; external name '_NavAskSaveChanges';

{
 *  NavCustomAskSaveChanges()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in NavigationLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NavCustomAskSaveChanges(var dialogOptions: NavDialogOptions; var reply: NavAskSaveChangesResult; eventProc: NavEventUPP; callBackUD: UnivPtr): OSErr; external name '_NavCustomAskSaveChanges';

{
 *  NavAskDiscardChanges()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in NavigationLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NavAskDiscardChanges(var dialogOptions: NavDialogOptions; var reply: NavAskDiscardChangesResult; eventProc: NavEventUPP; callBackUD: UnivPtr): OSErr; external name '_NavAskDiscardChanges';

{
 *  NavChooseFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in NavigationLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NavChooseFile(defaultLocation: AEDescPtr; var reply: NavReplyRecord; dialogOptions: NavDialogOptionsPtr; eventProc: NavEventUPP; previewProc: NavPreviewUPP; filterProc: NavObjectFilterUPP; typeList: NavTypeListHandle; callBackUD: UnivPtr): OSErr; external name '_NavChooseFile';

{
 *  NavChooseFolder()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in NavigationLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NavChooseFolder(defaultLocation: AEDescPtr; var reply: NavReplyRecord; dialogOptions: NavDialogOptionsPtr; eventProc: NavEventUPP; filterProc: NavObjectFilterUPP; callBackUD: UnivPtr): OSErr; external name '_NavChooseFolder';

{
 *  NavChooseVolume()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in NavigationLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NavChooseVolume(defaultSelection: AEDescPtr; var reply: NavReplyRecord; dialogOptions: NavDialogOptionsPtr; eventProc: NavEventUPP; filterProc: NavObjectFilterUPP; callBackUD: UnivPtr): OSErr; external name '_NavChooseVolume';

{
 *  NavChooseObject()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in NavigationLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NavChooseObject(defaultLocation: AEDescPtr; var reply: NavReplyRecord; dialogOptions: NavDialogOptionsPtr; eventProc: NavEventUPP; filterProc: NavObjectFilterUPP; callBackUD: UnivPtr): OSErr; external name '_NavChooseObject';

{
 *  NavNewFolder()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in NavigationLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NavNewFolder(defaultLocation: AEDescPtr; var reply: NavReplyRecord; dialogOptions: NavDialogOptionsPtr; eventProc: NavEventUPP; callBackUD: UnivPtr): OSErr; external name '_NavNewFolder';

{
 *  NavTranslateFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in NavigationLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NavTranslateFile(var reply: NavReplyRecord; howToTranslate: NavTranslationOptions): OSErr; external name '_NavTranslateFile';

{
 *  NavCompleteSave()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in NavigationLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NavCompleteSave(var reply: NavReplyRecord; howToTranslate: NavTranslationOptions): OSErr; external name '_NavCompleteSave';

{
 *  NavCustomControl()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in NavigationLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NavCustomControl(dialog: NavDialogRef; selector: NavCustomControlMessage; parms: UnivPtr): OSErr; external name '_NavCustomControl';

{
 *  NavCreatePreview()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in NavigationLib 2.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NavCreatePreview(var theObject: AEDesc; previewDataType: OSType; previewData: UnivPtr; previewDataSize: Size): OSErr; external name '_NavCreatePreview';

{
 *  NavDisposeReply()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in NavigationLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NavDisposeReply(var reply: NavReplyRecord): OSErr; external name '_NavDisposeReply';

{
 *  NavServicesCanRun()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in NavigationLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         not available
 }
function NavServicesCanRun: boolean; external name '_NavServicesCanRun';


{$ifc TARGET_RT_MAC_CFM}
{
        NavServicesAvailable() is a macro available only in C/C++.
        To get the same functionality from pascal or assembly, you need
        to test if NavigationLib functions are not NULL and call NavServicesCanRun()
        which will test if NavServices is properly installed.  For instance:

            gNavServicesAvailable = FALSE;
            IF @NavLibraryVersion <> kUnresolvedCFragSymbolAddress THEN
                gNavServicesAvailable = NavServicesCanRun;
            end

}
{$elsec}
  {$ifc TARGET_RT_MAC_MACHO}
{ Navigation is always available on OS X }
  {$elsec}
{  NavServicesAvailable() is implemented in Navigation.o for classic 68K clients }
  {$ifc CALL_NOT_IN_CARBON}
{
 *  NavServicesAvailable()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function NavServicesAvailable: boolean; external name '_NavServicesAvailable';

  {$endc}  {CALL_NOT_IN_CARBON}
  {$endc}
{$endc}

{ Carbon API }
{ Includes support for Unicode and long file names (where available). }


const
	kNavDialogCreationOptionsVersion = 0;


	{
	 *  NavDialogCreationOptions
	 *  
	 *  Summary:
	 *    Options used to control the appearance and operation of a Nav
	 *    Services dialog
	 *  
	 *  Discussion:
	 *    NavDialogCreationOptions is a preferred replacement for
	 *    NavDialogOptions. The new structure uses CFStrings in place of
	 *    Pascal strings, and adds fields for setting the dialog modality
	 *    and the parent window (for sheet dialogs). A
	 *    NavDialogCreationOptions structure can be initialized using
	 *    NavDialogGetDefaultCreationOptions. Each of the NavCreate*Dialog
	 *    functions accepts a pointer to the client's
	 *    NavDialogCreationOptions structure.
	 	}

type
	NavDialogCreationOptionsPtr = ^NavDialogCreationOptions;
	NavDialogCreationOptions = record
		version:				UInt16;
		optionFlags:			NavDialogOptionFlags;
		location:				Point;
		clientName:				CFStringRef;
		windowTitle:			CFStringRef;
		actionButtonLabel:		CFStringRef;
		cancelButtonLabel:		CFStringRef;
		saveFileName:			CFStringRef;
		message:				CFStringRef;
		preferenceKey:			UInt32;
		popupExtension:			CFArrayRef;
		modality:				WindowModality;
		parentWindow:			WindowRef;
		reserved:				packed array [0..15] of char;
	end;

	{
	 *  NavGetDefaultDialogCreationOptions()
	 *  
	 *  Summary:
	 *    Initialize the input structure to default values
	 *  
	 *  Discussion:
	 *    Provided as a convenience to obtain the preferred default options
	 *    for use in creating any Nav Services dialog.
	 *  
	 *  Parameters:
	 *    
	 *    outOptions:
	 *      A pointer to the client-allocated options structure to
	 *      initialize
	 *  
	 *  Result:
	 *    A status code
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   not available
	 *    CarbonLib:        in CarbonLib 1.1 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function NavGetDefaultDialogCreationOptions(var outOptions: NavDialogCreationOptions): OSStatus; external name '_NavGetDefaultDialogCreationOptions';


{
 *  NavCreateGetFileDialog()
 *  
 *  Summary:
 *    Create a GetFile dialog
 *  
 *  Discussion:
 *    Use this function to create a dialog designed for opening
 *    document files. This function replaces NavGetFile, allowing new
 *    window modalities, and adding Unicode support. Upon successful
 *    creation, the dialog is not visible. Present and run the dialog
 *    with NavDialogRun. After the dialog is complete, dispose of it
 *    with NavDialogDispose.
 *  
 *  Parameters:
 *    
 *    inOptions:
 *      Options controlling the appearance and behavior of the dialog
 *    
 *    inTypeList:
 *      A creator signature and list of file types to show in the
 *      dialog file browser. If NULL, show all files.
 *    
 *    inEventProc:
 *      The UPP for the client's event callack, or NULL for no event
 *      callback
 *    
 *    inPreviewProc:
 *      The UPP for the client's custom file preview callback, or NULL
 *      for standard previews
 *    
 *    inFilterProc:
 *      The UPP for the client's custom filter callback, or NULL for no
 *      custom file filtering
 *    
 *    inClientData:
 *      A client-defined context value passed to all callback functions
 *    
 *    outDialog:
 *      Upon successful completion, a reference to the created dialog
 *  
 *  Result:
 *    A status code
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NavCreateGetFileDialog(inOptions: {Const}NavDialogCreationOptionsPtr; inTypeList: NavTypeListHandle; inEventProc: NavEventUPP; inPreviewProc: NavPreviewUPP; inFilterProc: NavObjectFilterUPP; inClientData: UnivPtr; var outDialog: NavDialogRef): OSStatus; external name '_NavCreateGetFileDialog';


{
 *  NavCreatePutFileDialog()
 *  
 *  Summary:
 *    Create a PutFile dialog
 *  
 *  Discussion:
 *    Use this function to create a dialog designed for setting the
 *    name and location of a document file prior to saving. This
 *    function replaces NavPutFile, allowing new window modalities, and
 *    adding Unicode support. Upon successful creation, the dialog is
 *    not visible. Present and run the dialog with NavDialogRun. After
 *    the dialog is complete, dispose of it with NavDialogDispose.
 *  
 *  Parameters:
 *    
 *    inOptions:
 *      Options controlling the appearance and behavior of the dialog
 *    
 *    inFileType:
 *      The type of the file to be saved. This parameter is used in
 *      conjunction with the inFileCreator parameter to look up the
 *      kind string for the Format popup menu, and to drive the
 *      identification of translation options.
 *    
 *    inFileCreator:
 *      The creator signature of the file to be saved (see inFileType
 *      parameter)
 *    
 *    inEventProc:
 *      The UPP for the client's event callack, or NULL for no event
 *      callback
 *    
 *    inClientData:
 *      A client-defined context value passed to all callback functions
 *    
 *    outDialog:
 *      Upon successful completion, a reference to the created dialog
 *  
 *  Result:
 *    A status code
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NavCreatePutFileDialog(inOptions: {Const}NavDialogCreationOptionsPtr; inFileType: OSType; inFileCreator: OSType; inEventProc: NavEventUPP; inClientData: UnivPtr; var outDialog: NavDialogRef): OSStatus; external name '_NavCreatePutFileDialog';


{
 *  NavCreateAskReviewDocumentsDialog()
 *  
 *  Summary:
 *    Create an AskReviewDocumentsDialog dialog
 *  
 *  Discussion:
 *    Use this function to create a dialog which tells the user how
 *    many unsaved documents there are, and asks the user to start
 *    reviewing the documents, don't save any documents, or cancel.
 *    This dialog is appropriate to use when an application is quitting
 *    and there is more than one unsaved document. It is supported only
 *    on Mac OS X because the HI guidelines for earlier versions of Mac
 *    OS do not include this dialog as part of the application quit
 *    sequence. Upon successful creation, the dialog is not visible.
 *    Present and run the dialog with NavDialogRun. After the dialog is
 *    complete, dispose of it with NavDialogDispose. Upon dismissal of
 *    the dialog, this dialog's user action will be set to one of the
 *    following: kNavUserActionReviewDocuments,
 *    kNavUserActionDiscardDocuments, or kNavUserActionCancel.
 *  
 *  Parameters:
 *    
 *    inOptions:
 *      Options controlling the appearance and behavior of the dialog
 *    
 *    inDocumentCount:
 *      Indicates the number of documents needing review. This number
 *      appears in the text presented to the user. If for any reason
 *      the total number of unsaved documents is unknown, specify 0,
 *      and an ambiguous message will appear. Do not specifiy 1, since
 *      the HI guidelines call for this alert only when there is more
 *      than one document to be reviewed.
 *    
 *    inEventProc:
 *      The UPP for the client's event callack, or NULL for no event
 *      callback
 *    
 *    inClientData:
 *      A client-defined context value passed to all callback functions
 *    
 *    outDialog:
 *      Upon successful completion, a reference to the created dialog
 *  
 *  Result:
 *    A status code
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.1 and later
 *    Mac OS X:         in version 10.1 and later
 }
function NavCreateAskReviewDocumentsDialog(const (*var*) inOptions: NavDialogCreationOptions; inDocumentCount: UInt32; inEventProc: NavEventUPP; inClientData: UnivPtr; var outDialog: NavDialogRef): OSStatus; external name '_NavCreateAskReviewDocumentsDialog';


{
 *  NavCreateAskSaveChangesDialog()
 *  
 *  Summary:
 *    Create an AskSaveChanges dialog
 *  
 *  Discussion:
 *    Use this function to create a dialog which asks the user to save,
 *    don't save or cancel closing a document with unsaved changes.
 *    This function replaces NavAskSaveChanges and
 *    NavCustomAskSaveChanges, allowing new window modalities, and
 *    adding Unicode support. Upon successful creation, the dialog is
 *    not visible. Present and run the dialog with NavDialogRun. After
 *    the dialog is complete, dispose of it with NavDialogDispose. To
 *    provide a customized message for the alert, specify an non-NULL
 *    message value in the options structure.
 *  
 *  Parameters:
 *    
 *    inOptions:
 *      Options controlling the appearance and behavior of the dialog
 *    
 *    inAction:
 *      Indicates this usage context for this dialog: closing a
 *      document or quitting an application. This setting affects the
 *      message text displayed to the user.
 *    
 *    inEventProc:
 *      The UPP for the client's event callack, or NULL for no event
 *      callback
 *    
 *    inClientData:
 *      A client-defined context value passed to all callback functions
 *    
 *    outDialog:
 *      Upon successful completion, a reference to the created dialog
 *  
 *  Result:
 *    A status code
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NavCreateAskSaveChangesDialog(const (*var*) inOptions: NavDialogCreationOptions; inAction: NavAskSaveChangesAction; inEventProc: NavEventUPP; inClientData: UnivPtr; var outDialog: NavDialogRef): OSStatus; external name '_NavCreateAskSaveChangesDialog';


{
 *  NavCreateAskDiscardChangesDialog()
 *  
 *  Summary:
 *    Create an AskDiscardChanges dialog
 *  
 *  Discussion:
 *    Use this function to create a dialog which asks the user to
 *    discard changes to a document or cancel. This is most often use
 *    when the user wants to revert a a document to the last saved
 *    revision. This function replaces NavAskDiscardChanges, allowing
 *    new window modalities, and adding Unicode support. Upon
 *    successful creation, the dialog is not visible. Present and run
 *    the dialog with NavDialogRun. After the dialog is complete,
 *    dispose of it with NavDialogDispose.
 *  
 *  Parameters:
 *    
 *    inOptions:
 *      Options controlling the appearance and behavior of the dialog
 *    
 *    inEventProc:
 *      The UPP for the client's event callack, or NULL for no event
 *      callback
 *    
 *    inClientData:
 *      A client-defined context value passed to all callback functions
 *    
 *    outDialog:
 *      Upon successful completion, a reference to the created dialog
 *  
 *  Result:
 *    A status code
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NavCreateAskDiscardChangesDialog(const (*var*) inOptions: NavDialogCreationOptions; inEventProc: NavEventUPP; inClientData: UnivPtr; var outDialog: NavDialogRef): OSStatus; external name '_NavCreateAskDiscardChangesDialog';


{
 *  NavCreateChooseFileDialog()
 *  
 *  Summary:
 *    Create a ChooseFile dialog
 *  
 *  Discussion:
 *    Use this function to create a dialog designed for selecting one
 *    file as the target of an operation. A ChooseFile dialog is a
 *    simple version a GetFile dialog. This function replaces
 *    NavChooseFile, allowing new window modalities, and adding Unicode
 *    support. Upon successful creation, the dialog is not visible.
 *    Present and run the dialog with NavDialogRun. After the dialog is
 *    complete, dispose of it with NavDialogDispose.
 *  
 *  Parameters:
 *    
 *    inOptions:
 *      Options controlling the appearance and behavior of the dialog
 *    
 *    inTypeList:
 *      A creator signature and list of file types to show in the
 *      dialog file browser. If NULL, show all files.
 *    
 *    inEventProc:
 *      The UPP for the client's event callack, or NULL for no event
 *      callback
 *    
 *    inPreviewProc:
 *      The UPP for the client's custom file preview callback, or NULL
 *      for standard previews
 *    
 *    inFilterProc:
 *      The UPP for the client's custom filter callback, or NULL for no
 *      custom file filtering
 *    
 *    inClientData:
 *      A client-defined context value passed to all callback functions
 *    
 *    outDialog:
 *      Upon successful completion, a reference to the created dialog
 *  
 *  Result:
 *    A status code
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NavCreateChooseFileDialog(inOptions: {Const}NavDialogCreationOptionsPtr; inTypeList: NavTypeListHandle; inEventProc: NavEventUPP; inPreviewProc: NavPreviewUPP; inFilterProc: NavObjectFilterUPP; inClientData: UnivPtr; var outDialog: NavDialogRef): OSStatus; external name '_NavCreateChooseFileDialog';


{
 *  NavCreateChooseFolderDialog()
 *  
 *  Summary:
 *    Create a ChooseFolder dialog
 *  
 *  Discussion:
 *    Use this function to create a dialog designed for selecting a
 *    folder as the target of an operation. This function replaces
 *    NavChooseFolder, allowing new window modalities, and adding
 *    Unicode support. Upon successful creation, the dialog is not
 *    visible. Present and run the dialog with NavDialogRun. After the
 *    dialog is complete, dispose of it with NavDialogDispose.
 *  
 *  Parameters:
 *    
 *    inOptions:
 *      Options controlling the appearance and behavior of the dialog
 *    
 *    inEventProc:
 *      The UPP for the client's event callack, or NULL for no event
 *      callback
 *    
 *    inFilterProc:
 *      The UPP for the client's custom filter callback, or NULL for no
 *      custom file filtering
 *    
 *    inClientData:
 *      A client-defined context value passed to all callback functions
 *    
 *    outDialog:
 *      Upon successful completion, a reference to the created dialog
 *  
 *  Result:
 *    A status code
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NavCreateChooseFolderDialog(inOptions: {Const}NavDialogCreationOptionsPtr; inEventProc: NavEventUPP; inFilterProc: NavObjectFilterUPP; inClientData: UnivPtr; var outDialog: NavDialogRef): OSStatus; external name '_NavCreateChooseFolderDialog';


{
 *  NavCreateChooseVolumeDialog()
 *  
 *  Summary:
 *    Create a ChooseVolume dialog
 *  
 *  Discussion:
 *    Use this function to create a dialog designed for selecting a
 *    volume as the target of an operation. This function replaces
 *    NavChooseVolume, allowing new window modalities, and adding
 *    Unicode support. Upon successful creation, the dialog is not
 *    visible. Present and run the dialog with NavDialogRun. After the
 *    dialog is complete, dispose of it with NavDialogDispose.
 *  
 *  Parameters:
 *    
 *    inOptions:
 *      Options controlling the appearance and behavior of the dialog
 *    
 *    inEventProc:
 *      The UPP for the client's event callack, or NULL for no event
 *      callback
 *    
 *    inFilterProc:
 *      The UPP for the client's custom filter callback, or NULL for no
 *      custom file filtering
 *    
 *    inClientData:
 *      A client-defined context value passed to all callback functions
 *    
 *    outDialog:
 *      Upon successful completion, a reference to the created dialog
 *  
 *  Result:
 *    A status code
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NavCreateChooseVolumeDialog(inOptions: {Const}NavDialogCreationOptionsPtr; inEventProc: NavEventUPP; inFilterProc: NavObjectFilterUPP; inClientData: UnivPtr; var outDialog: NavDialogRef): OSStatus; external name '_NavCreateChooseVolumeDialog';


{
 *  NavCreateChooseObjectDialog()
 *  
 *  Summary:
 *    Create a ChooseObject dialog
 *  
 *  Discussion:
 *    Use this function to create a dialog designed for selecting a
 *    file, folder, or volume as the target of an operation. This
 *    function replaces NavChooseObject, allowing new window
 *    modalities, and adding Unicode support. Upon successful creation,
 *    the dialog is not visible. Present and run the dialog with
 *    NavDialogRun. After the dialog is complete, dispose of it with
 *    NavDialogDispose.
 *  
 *  Parameters:
 *    
 *    inOptions:
 *      Options controlling the appearance and behavior of the dialog
 *    
 *    inEventProc:
 *      The UPP for the client's event callack, or NULL for no event
 *      callback
 *    
 *    inPreviewProc:
 *      The UPP for the client's custom file preview callback, or NULL
 *      for standard previews
 *    
 *    inFilterProc:
 *      The UPP for the client's custom filter callback, or NULL for no
 *      custom file filtering
 *    
 *    inClientData:
 *      A client-defined context value passed to all callback functions
 *    
 *    outDialog:
 *      Upon successful completion, a reference to the created dialog
 *  
 *  Result:
 *    A status code
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NavCreateChooseObjectDialog(inOptions: {Const}NavDialogCreationOptionsPtr; inEventProc: NavEventUPP; inPreviewProc: NavPreviewUPP; inFilterProc: NavObjectFilterUPP; inClientData: UnivPtr; var outDialog: NavDialogRef): OSStatus; external name '_NavCreateChooseObjectDialog';


{
 *  NavCreateNewFolderDialog()
 *  
 *  Summary:
 *    Create a NewFolder dialog
 *  
 *  Discussion:
 *    Use this function to create a dialog designed for creating a new
 *    folder. Nav Services creates the folder as specified by the user
 *    and returns a reference to the folder in the selection field of
 *    the reply record. This function replaces NavNewFolder, allowing
 *    new window modalities, and adding Unicode support. Upon
 *    successful creation, the dialog is not visible. Present and run
 *    the dialog with NavDialogRun. After the dialog is complete,
 *    dispose of it with NavDialogDispose.
 *  
 *  Parameters:
 *    
 *    inOptions:
 *      Options controlling the appearance and behavior of the dialog
 *    
 *    inEventProc:
 *      The UPP for the client's event callack, or NULL for no event
 *      callback
 *    
 *    inClientData:
 *      A client-defined context value passed to all callback functions
 *    
 *    outDialog:
 *      Upon successful completion, a reference to the created dialog
 *  
 *  Result:
 *    A status code
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NavCreateNewFolderDialog(inOptions: {Const}NavDialogCreationOptionsPtr; inEventProc: NavEventUPP; inClientData: UnivPtr; var outDialog: NavDialogRef): OSStatus; external name '_NavCreateNewFolderDialog';


{
 *  NavDialogRun()
 *  
 *  Summary:
 *    Show and run a Nav Services dialog
 *  
 *  Discussion:
 *    After a dialog is created with a NavCreate*Dialog function, the
 *    client can modify the dialog target folder or save file name
 *    using NavCustomControl with the appropriate selectors. The dialog
 *    is presented to the user by calling NavDialogRun. If the dialog
 *    is system modal or application modal (kWindowModalitySystemModal,
 *    kWindowModalityAppModal), NavDialogRun does not return until the
 *    dialog has been dismissed. If the dialog is modeless or window
 *    modal (kWindowModalityNone, kWindowModalityWindowModal),
 *    NavDialogRun shows the dialog and returns immediately. In order
 *    to know when the dialog has been dismissed, the client must watch
 *    for the kNavCBUserAction event sent to the client event proc.
 *    Note that on Mac OS 9 and earlier, all dialogs are modal, even if
 *    a modeless or window modal dialog is requested. However, the
 *    kNavCBUserAction event is still sent to the event proc, so it's
 *    possible to use a single programming model on OS 9 and OS X
 *    provided the client assumes NavDialogRun returns immediately
 *    after showing the dialog.
 *  
 *  Parameters:
 *    
 *    inDialog:
 *      The dialog to run
 *  
 *  Result:
 *    A status code
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NavDialogRun(inDialog: NavDialogRef): OSStatus; external name '_NavDialogRun';


{
 *  NavDialogDispose()
 *  
 *  Summary:
 *    Dispose of a Nav Services dialog
 *  
 *  Discussion:
 *    Call this function when completely finished with a Nav Services
 *    dialog. After calling NavDialogDispose, the dialog reference is
 *    no longer valid. NavDialogDispose is safe to call from within a
 *    callback to the client's Nav Services event proc.
 *  
 *  Parameters:
 *    
 *    inDialog:
 *      The dialog to dispose
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure NavDialogDispose(inDialog: NavDialogRef); external name '_NavDialogDispose';


{
 *  NavDialogGetWindow()
 *  
 *  Summary:
 *    Return the window in which a Nav Services dialog appears
 *  
 *  Discussion:
 *    Note that a valid NavDialogRef may not have a window until
 *    NavDialogRun has been called. If no window exists for the dialog,
 *    NavDialogGetWindow returns NULL.
 *  
 *  Parameters:
 *    
 *    inDialog:
 *      Which dialog
 *  
 *  Result:
 *    The window reference
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NavDialogGetWindow(inDialog: NavDialogRef): WindowRef; external name '_NavDialogGetWindow';


{
 *  NavDialogGetUserAction()
 *  
 *  Summary:
 *    Return the current user action taken by the user
 *  
 *  Discussion:
 *    A user action occurs when the user dismisses the dialog or
 *    otherwise does something generating a reply record that the
 *    client needs to act upon. If the user has not taken such an
 *    action, NavDialogGetUserAction returns kNavUserActionNone. If the
 *    dialog is terminated using the NavCustomControl selector
 *    kNavCtlTerminate, the final user action is kNavUserActionNone.
 *    For file dialogs, if the final user action is not
 *    kNavUserActionCancel, then there is a valid reply record which
 *    can be obtained with NavDialogGetReply. Although the user action
 *    is sent to the client event proc as a kNavCBUserAction event,
 *    this function is provided as a convenience for clients of modal
 *    dialogs who may find it easier to get the user action immediately
 *    after NavDialogRun returns.
 *  
 *  Parameters:
 *    
 *    inDialog:
 *      Which dialog
 *  
 *  Result:
 *    The user action
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NavDialogGetUserAction(inDialog: NavDialogRef): NavUserAction; external name '_NavDialogGetUserAction';


{
 *  NavDialogGetReply()
 *  
 *  Summary:
 *    Fill in the provided reply record with the results of a user
 *    action such as kNavUserActionOpen, kNavUserActionSaveAs, or
 *    kNavUserActionChoose.
 *  
 *  Discussion:
 *    Call this function when a file dialog receives a user action
 *    other that implies an item or items to open, save, etc. Upon
 *    successful completion, the reply record describes the item(s)
 *    that the client needs to act upon. The reply record should later
 *    be disposed of with NavDisposeReply.
 *  
 *  Parameters:
 *    
 *    inDialog:
 *      Which dialog
 *    
 *    outReply:
 *      A pointer to the client-allocated reply record to be filled in
 *  
 *  Result:
 *    A status code
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NavDialogGetReply(inDialog: NavDialogRef; var outReply: NavReplyRecord): OSStatus; external name '_NavDialogGetReply';


{
 *  NavDialogGetSaveFileName()
 *  
 *  Summary:
 *    Return the current value of the file name to be saved in a
 *    PutFile dialog
 *  
 *  Discussion:
 *    This function can be called at any time on a valid PutFile dialog
 *    to obtain the current value of the save file name. This function
 *    is a Unicode-based replacement for the kNavCtlGetEditFileName
 *    NavCustomControl selector. On Mac OS X, the full file name is
 *    returned, including any extenison that may be hidden from the
 *    user.
 *  
 *  Parameters:
 *    
 *    inPutFileDialog:
 *      Which dialog
 *  
 *  Result:
 *    The save file name as a CFStringRef. The string is immutable. The
 *    client should retain the string if the reference is to be held
 *    beyond the life of the dialog (standard CF retain/release
 *    semantics).
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NavDialogGetSaveFileName(inPutFileDialog: NavDialogRef): CFStringRef; external name '_NavDialogGetSaveFileName';


{
 *  NavDialogSetSaveFileName()
 *  
 *  Summary:
 *    Set the current value of the file name to be saved in a PutFile
 *    dialog
 *  
 *  Discussion:
 *    This function can be called at any time to set the current save
 *    file name. Use it to set an initial name before calling
 *    NavDialogRun or to change the file name dynamically while a
 *    dialog is running. This function is a Unicode-based replacement
 *    for the kNavCtlSetEditFileName NavCustomControl selector.
 *  
 *  Parameters:
 *    
 *    inPutFileDialog:
 *      Which PutFile dialog
 *    
 *    inFileName:
 *      The file name to use
 *  
 *  Result:
 *    A status code
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NavDialogSetSaveFileName(inPutFileDialog: NavDialogRef; inFileName: CFStringRef): OSStatus; external name '_NavDialogSetSaveFileName';


{
 *  NavDialogGetSaveFileExtensionHidden()
 *  
 *  Summary:
 *    Get the current state of the extension hiding in a PutFile dialog
 *  
 *  Discussion:
 *    This function can be called at any time to determine if a PutFile
 *    dialog is hiding the file extesion (if any) of the file to be
 *    saved.
 *  
 *  Parameters:
 *    
 *    inPutFileDialog:
 *      Which PutFile dialog
 *  
 *  Result:
 *    True if the extension is hidden, false if the extension is
 *    visible or there is no extension.
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.1 and later
 *    Mac OS X:         in version 10.1 and later
 }
function NavDialogGetSaveFileExtensionHidden(inPutFileDialog: NavDialogRef): boolean; external name '_NavDialogGetSaveFileExtensionHidden';


{
 *  NavDialogSetSaveFileExtensionHidden()
 *  
 *  Summary:
 *    Set the current state of the extension hiding in a PutFile dialog
 *  
 *  Discussion:
 *    This function can be called at any time to hide or show the
 *    extension of the file to be saved in a PutFile dialog. If the
 *    current file name has no extension, then hiding the extension has
 *    no effect.
 *  
 *  Parameters:
 *    
 *    inPutFileDialog:
 *      Which PutFile dialog
 *    
 *    inHidden:
 *      The new value for the hidden extension state
 *  
 *  Result:
 *    A status code
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available in CarbonLib 1.x, is available on Mac OS X version 10.1 and later
 *    Mac OS X:         in version 10.1 and later
 }
function NavDialogSetSaveFileExtensionHidden(inPutFileDialog: NavDialogRef; inHidden: boolean): OSStatus; external name '_NavDialogSetSaveFileExtensionHidden';

{$ALIGN MAC68K}


end.
