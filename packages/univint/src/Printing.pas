{
     File:       Printing.p
 
     Contains:   Print Manager Interfaces.
 
     Version:    Technology: System 7.5
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  © 1985-2002 by Apple Computer, Inc., all rights reserved
 
     Bugs?:      For bug reports, consult the following page on
                 the World Wide Web:
 
                     http://www.freepascal.org/bugs.html
 
}


{
    Modified for use with Free Pascal
    Version 200
    Please report any bugs to <gpc@microbizz.nl>
}

{$mode macpas}
{$packenum 1}
{$macro on}
{$inline on}
{$CALLING MWPASCAL}

unit Printing;
interface
{$setc UNIVERSAL_INTERFACES_VERSION := $0342}
{$setc GAP_INTERFACES_VERSION := $0200}

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
uses MacTypes,MacErrors,Quickdraw,Dialogs;


{$ALIGN MAC68K}


const
	kFirstPageMin				= 1;							{  min value for first page  }
	kLastPageMax				= 32767;						{  max value for last page  }

	iPFMaxPgs					= 128;
	iPrPgFract					= 120;							{ Page scale factor. ptPgSize (below) is in units of 1/iPrPgFract }
	iPrPgFst					= 1;							{ Page range constants }
	iPrPgMax					= 9999;
	iPrRelease					= 3;							{ Current version number of the code. }
	iPrSavPFil					= -1;
	iPrAbort					= $0080;
	iPrDevCtl					= 7;							{ The PrDevCtl Proc's ctl number }
	lPrReset					= $00010000;					{ The PrDevCtl Proc's CParam for reset }
	lPrLineFeed					= $00030000;
	lPrLFStd					= $0003FFFF;					{ The PrDevCtl Proc's CParam for std paper advance }
	lPrLFSixth					= $0003FFFF;
	lPrPageEnd					= $00020000;					{ The PrDevCtl Proc's CParam for end page }
	lPrDocOpen					= $00010000;
	lPrPageOpen					= $00040000;
	lPrPageClose				= $00020000;
	lPrDocClose					= $00050000;
	iFMgrCtl					= 8;							{ The FMgr's Tail-hook Proc's ctl number }
	iMscCtl						= 9;							{ The FMgr's Tail-hook Proc's ctl number }
	iPvtCtl						= 10;							{ The FMgr's Tail-hook Proc's ctl number }

	{	  Error Codes moved to Errors.(hap) 	}
	pPrGlobals					= $00000944;					{ The PrVars lo mem area: }
	bDraftLoop					= 0;
	bSpoolLoop					= 1;
	bUser1Loop					= 2;
	bUser2Loop					= 3;
	fNewRunBit					= 2;
	fHiResOK					= 3;
	fWeOpenedRF					= 4;							{ Driver constants  }
	iPrBitsCtl					= 4;
	lScreenBits					= 0;
	lPaintBits					= 1;
	lHiScreenBits				= $00000002;					{ The Bitmap Print Proc's Screen Bitmap param }
	lHiPaintBits				= $00000003;					{ The Bitmap Print Proc's Paint [sq pix] param }
	iPrIOCtl					= 5;
	iPrEvtCtl					= 6;							{ The PrEvent Proc's ctl number }
	lPrEvtAll					= $0002FFFD;					{ The PrEvent Proc's CParam for the entire screen }
	lPrEvtTop					= $0001FFFD;					{ The PrEvent Proc's CParam for the top folder }
	iPrDrvrRef					= -3;

	getRslDataOp				= 4;
	setRslOp					= 5;
	draftBitsOp					= 6;
	noDraftBitsOp				= 7;
	getRotnOp					= 8;
	NoSuchRsl					= 1;
	OpNotImpl					= 2;							{ the driver doesn't support this opcode }
	RgType1						= 1;


type
	TFeed 						= SInt8;
const
	feedCut						= 0;
	feedFanfold					= 1;
	feedMechCut					= 2;
	feedOther					= 3;


type
	TScan 						= SInt8;
const
	scanTB						= 0;
	scanBT						= 1;
	scanLR						= 2;
	scanRL						= 3;

	{	 A Rect Ptr 	}

type
	TPRect								= ^Rect;
	TPRectPtr 							= ^TPRect;
{$ifc TYPED_FUNCTION_POINTERS}
	PrIdleProcPtr = procedure;
{$elsec}
	PrIdleProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	PItemProcPtr = procedure(theDialog: DialogRef; item: SInt16);
{$elsec}
	PItemProcPtr = ProcPtr;
{$endc}

{$ifc OPAQUE_UPP_TYPES}
	PrIdleUPP = ^SInt32; { an opaque UPP }
{$elsec}
	PrIdleUPP = UniversalProcPtr;
{$endc}	
{$ifc OPAQUE_UPP_TYPES}
	PItemUPP = ^SInt32; { an opaque UPP }
{$elsec}
	PItemUPP = UniversalProcPtr;
{$endc}	

const
	uppPrIdleProcInfo = $00000000;
	uppPItemProcInfo = $000002C0;
{$ifc CALL_NOT_IN_CARBON}
	{
	 *  NewPrIdleUPP()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   available as macro/inline
	 *    CarbonLib:        not available
	 *    Mac OS X:         not available
	 	}
function NewPrIdleUPP(userRoutine: PrIdleProcPtr): PrIdleUPP; external name '_NewPrIdleUPP'; { old name was NewPrIdleProc }
{
 *  NewPItemUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function NewPItemUPP(userRoutine: PItemProcPtr): PItemUPP; external name '_NewPItemUPP'; { old name was NewPItemProc }
{
 *  DisposePrIdleUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure DisposePrIdleUPP(userUPP: PrIdleUPP); external name '_DisposePrIdleUPP';
{
 *  DisposePItemUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure DisposePItemUPP(userUPP: PItemUPP); external name '_DisposePItemUPP';
{
 *  InvokePrIdleUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure InvokePrIdleUPP(userRoutine: PrIdleUPP); external name '_InvokePrIdleUPP'; { old name was CallPrIdleProc }
{
 *  InvokePItemUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure InvokePItemUPP(theDialog: DialogRef; item: SInt16; userRoutine: PItemUPP); external name '_InvokePItemUPP'; { old name was CallPItemProc }
{$endc}  {CALL_NOT_IN_CARBON}

{$ifc NOT OPAQUE_TOOLBOX_STRUCTS}

type
	TPrPortPtr = ^TPrPort;
	TPrPort = record
		gPort:					GrafPort;								{ The Printer's graf port. }
		gProcs:					QDProcs;								{ ..and its procs }
		lGParam1:				SInt32;								{ 16 bytes for private parameter storage. }
		lGParam2:				SInt32;
		lGParam3:				SInt32;
		lGParam4:				SInt32;
		fOurPtr:				boolean;								{ Whether the PrPort allocation was done by us. }
		fOurBits:				boolean;								{ Whether the BitMap allocation was done by us. }
	end;

	TPPrPort							= ^TPrPort;
{$elsec}

type
	TPPrPort    = ^SInt32; { an opaque 32-bit type }
	TPPrPortPtr = ^TPPrPort;  { when a var xx:TPPrPort parameter can be nil, it is changed to xx: TPPrPortPtr }
{$endc}

	TPPrPortRef							= TPPrPort;
	{	 Printing Graf Port. All printer imaging, whether spooling, banding, etc, happens "thru" a GrafPort.
	  This is the "PrPeek" record. 	}
	TPrInfoPtr = ^TPrInfo;
	TPrInfo = record
		iDev:					SInt16;								{ Font mgr/QuickDraw device code }
		iVRes:					SInt16;								{ Resolution of device, in device coordinates }
		iHRes:					SInt16;								{ ..note: V before H => compatable with Point. }
		rPage:					Rect;									{ The page (printable) rectangle in device coordinates. }
	end;

	TPPrInfo							= ^TPrInfo;
	{	 Print Info Record: The parameters needed for page composition. 	}
	TPrStlPtr = ^TPrStl;
	TPrStl = record
		wDev:					SInt16;
		iPageV:					SInt16;
		iPageH:					SInt16;
		bPort:					SInt8;
		feed:					SInt8;
	end;

	TPPrStl								= ^TPrStl;
	TPrXInfoPtr = ^TPrXInfo;
	TPrXInfo = record
		iRowBytes:				SInt16;
		iBandV:					SInt16;
		iBandH:					SInt16;
		iDevBytes:				SInt16;
		iBands:					SInt16;
		bPatScale:				SInt8;
		bUlThick:				SInt8;
		bUlOffset:				SInt8;
		bUlShadow:				SInt8;
		scan:					SInt8;
		bXInfoX:				SInt8;
	end;

	TPPrXInfo							= ^TPrXInfo;
	TPrJobPtr = ^TPrJob;
	TPrJob = record
		iFstPage:				SInt16;								{ Page Range. }
		iLstPage:				SInt16;
		iCopies:				SInt16;								{ No. copies. }
		bJDocLoop:				SInt8;									{ The Doc style: Draft, Spool, .., and .. }
		fFromUsr:				boolean;								{ Printing from an User's App (not PrApp) flag }
		pIdleProc:				PrIdleUPP;								{ The Proc called while waiting on IO etc. }
		pFileName:				StringPtr;								{ Spool File Name: NIL for default. }
		iFileVol:				SInt16;								{ Spool File vol, set to 0 initially }
		bFileVers:				SInt8;									{ Spool File version, set to 0 initially }
		bJobX:					SInt8;									{ An eXtra byte. }
	end;

	TPPrJob								= ^TPrJob;
	{	 Print Job: Print "form" for a single print request. 	}
	TPrFlag1Ptr = ^TPrFlag1;
	TPrFlag1 = packed record
		f15:					boolean;
		f14:					boolean;
		f13:					boolean;
		f12:					boolean;
		f11:					boolean;
		f10:					boolean;
		f9:						boolean;
		f8:						boolean;
		f7:						boolean;
		f6:						boolean;
		f5:						boolean;
		f4:						boolean;
		f3:						boolean;
		f2:						boolean;
		fLstPgFst:				boolean;
		fUserScale:				boolean;
	end;

	TPrintPtr = ^TPrint;
	TPrint = record
		iPrVersion:				SInt16;								{ (2) Printing software version }
		prInfo:					TPrInfo;								{ (14) the PrInfo data associated with the current style. }
		rPaper:					Rect;									{ (8) The paper rectangle [offset from rPage] }
		prStl:					TPrStl;									{ (8)  This print request's style. }
		prInfoPT:				TPrInfo;								{ (14)  Print Time Imaging metrics }
		prXInfo:				TPrXInfo;								{ (16)  Print-time (expanded) Print info record. }
		prJob:					TPrJob;									{ (20) The Print Job request (82)  Total of the above; 120-82 = 38 bytes needed to fill 120 }
		case SInt16 of
		0: (
			printX:				array [1..19] of SInt16;
			);
		1: (
			prFlag1:			TPrFlag1;
			iZoomMin:			SInt16;
			iZoomMax:			SInt16;
			hDocName:			StringHandle;
		   );
	end;

	TPPrint								= ^TPrint;
	THPrint								= ^TPPrint;
	TPrStatusPtr = ^TPrStatus;
	TPrStatus = record
		iTotPages:				SInt16;								{ Total pages in Print File. }
		iCurPage:				SInt16;								{ Current page number }
		iTotCopies:				SInt16;								{ Total copies requested }
		iCurCopy:				SInt16;								{ Current copy number }
		iTotBands:				SInt16;								{ Total bands per page. }
		iCurBand:				SInt16;								{ Current band number }
		fPgDirty:				boolean;								{ True if current page has been written to. }
		fImaging:				boolean;								{ Set while in band's DrawPic call. }
		hPrint:					THPrint;								{ Handle to the active Printer record }
		pPrPort:				TPPrPort;								{ Ptr to the active PrPort }
		hPic:					PicHandle;								{ Handle to the active Picture }
	end;

	TPPrStatus							= ^TPrStatus;
	TPPrStatusRef						= TPPrStatus;

	{	 Print Status: Print information during printing. 	}
	TPfPgDirPtr = ^TPfPgDir;
	TPfPgDir = record
		iPages:					SInt16;
		iPgPos:					array [0..128] of SInt32;				{ array [0..iPfMaxPgs] of SInt32 }
	end;

	TPPfPgDir							= ^TPfPgDir;
	THPfPgDir							= ^TPPfPgDir;
	{	 PicFile = a TPfHeader followed by n QuickDraw Pics (whose PicSize is invalid!) 	}
	{	 This is the Printing Dialog Record. Only used by folks appending their own
	   DITLs to the print dialogs.  Print Dialog: The Dialog Stream object. 	}
{$ifc NOT OPAQUE_TOOLBOX_STRUCTS}
	TPrDlgPtr = ^TPrDlg;
	TPrDlg = record
		Dlg:					DialogRecord;							{ The Dialog window }
		pFltrProc:				ModalFilterUPP;							{ The Filter Proc. }
		pItemProc:				PItemUPP;								{ The Item evaluating proc. }
		hPrintUsr:				THPrint;								{ The user's print record. }
		fDoIt:					boolean;
		fDone:					boolean;
		lUser1:					SInt32;								{ Four longs for apps to hang global data. }
		lUser2:					SInt32;								{ Plus more stuff needed by the particular }
		lUser3:					SInt32;								{ printing dialog. }
		lUser4:					SInt32;
	end;

	TPPrDlg								= ^TPrDlg;
{$elsec}
	TPPrDlg    = ^SInt32; { an opaque 32-bit type }
	TPPrDlgPtr = ^TPPrDlg;  { when a var xx:TPPrDlg parameter can be nil, it is changed to xx: TPPrDlgPtr }
{$endc}

	TPPrDlgRef							= TPPrDlg;
{$ifc TYPED_FUNCTION_POINTERS}
	PDlgInitProcPtr = function(hPrint: THPrint): TPPrDlgRef;
{$elsec}
	PDlgInitProcPtr = ProcPtr;
{$endc}

{$ifc OPAQUE_UPP_TYPES}
	PDlgInitUPP = ^SInt32; { an opaque UPP }
{$elsec}
	PDlgInitUPP = UniversalProcPtr;
{$endc}	

const
	uppPDlgInitProcInfo = $000000F0;
{$ifc CALL_NOT_IN_CARBON}
	{
	 *  NewPDlgInitUPP()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   available as macro/inline
	 *    CarbonLib:        not available
	 *    Mac OS X:         not available
	 	}
function NewPDlgInitUPP(userRoutine: PDlgInitProcPtr): PDlgInitUPP; external name '_NewPDlgInitUPP'; { old name was NewPDlgInitProc }
{
 *  DisposePDlgInitUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure DisposePDlgInitUPP(userUPP: PDlgInitUPP); external name '_DisposePDlgInitUPP';
{
 *  InvokePDlgInitUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function InvokePDlgInitUPP(hPrint: THPrint; userRoutine: PDlgInitUPP): TPPrDlgRef; external name '_InvokePDlgInitUPP'; { old name was CallPDlgInitProc }
{$endc}  {CALL_NOT_IN_CARBON}


type
	TGnlDataPtr = ^TGnlData;
	TGnlData = record
		iOpCode:				SInt16;
		iError:					SInt16;
		lReserved:				SInt32;								{ more fields here depending on call }
	end;

	TRslRgPtr = ^TRslRg;
	TRslRg = record
		iMin:					SInt16;
		iMax:					SInt16;
	end;

	TRslRecPtr = ^TRslRec;
	TRslRec = record
		iXRsl:					SInt16;
		iYRsl:					SInt16;
	end;

	TGetRslBlkPtr = ^TGetRslBlk;
	TGetRslBlk = record
		iOpCode:				SInt16;
		iError:					SInt16;
		lReserved:				SInt32;
		iRgType:				SInt16;
		xRslRg:					TRslRg;
		yRslRg:					TRslRg;
		iRslRecCnt:				SInt16;
		rgRslRec:				array [1..27] of TRslRec;
	end;

	TSetRslBlkPtr = ^TSetRslBlk;
	TSetRslBlk = record
		iOpCode:				SInt16;
		iError:					SInt16;
		lReserved:				SInt32;
		hPrint:					THPrint;
		iXRsl:					SInt16;
		iYRsl:					SInt16;
	end;

	TDftBitsBlkPtr = ^TDftBitsBlk;
	TDftBitsBlk = record
		iOpCode:				SInt16;
		iError:					SInt16;
		lReserved:				SInt32;
		hPrint:					THPrint;
	end;

	TGetRotnBlkPtr = ^TGetRotnBlk;
	TGetRotnBlk = record
		iOpCode:				SInt16;
		iError:					SInt16;
		lReserved:				SInt32;
		hPrint:					THPrint;
		fLandscape:				boolean;
		bXtra:					SInt8;
	end;

{$ifc CALL_NOT_IN_CARBON}
	{
	 *  PrPurge()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
	 *    CarbonLib:        not available
	 *    Mac OS X:         not available
	 	}
procedure PrPurge; external name '_PrPurge';
{
 *  PrNoPurge()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure PrNoPurge; external name '_PrNoPurge';
{
 *  PrOpen()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure PrOpen; external name '_PrOpen';
{
 *  PrClose()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure PrClose; external name '_PrClose';
{
 *  PrintDefault()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure PrintDefault(hPrint: THPrint); external name '_PrintDefault';
{
 *  PrValidate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PrValidate(hPrint: THPrint): boolean; external name '_PrValidate';
{
 *  PrStlDialog()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PrStlDialog(hPrint: THPrint): boolean; external name '_PrStlDialog';
{
 *  PrJobDialog()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PrJobDialog(hPrint: THPrint): boolean; external name '_PrJobDialog';
{
 *  PrStlInit()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PrStlInit(hPrint: THPrint): TPPrDlgRef; external name '_PrStlInit';
{
 *  PrJobInit()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PrJobInit(hPrint: THPrint): TPPrDlgRef; external name '_PrJobInit';
{
 *  PrJobMerge()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure PrJobMerge(hPrintSrc: THPrint; hPrintDst: THPrint); external name '_PrJobMerge';
{
 *  PrDlgMain()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PrDlgMain(hPrint: THPrint; pDlgInit: PDlgInitUPP): boolean; external name '_PrDlgMain';
{
 *  PrOpenDoc()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PrOpenDoc(hPrint: THPrint; pPrPort: TPPrPort; pIOBuf: Ptr): TPPrPort; external name '_PrOpenDoc';
{
 *  PrCloseDoc()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure PrCloseDoc(pPrPort: TPPrPort); external name '_PrCloseDoc';
{
 *  PrOpenPage()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure PrOpenPage(pPrPort: TPPrPort; pPageFrame: TPRect); external name '_PrOpenPage';
{
 *  PrClosePage()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure PrClosePage(pPrPort: TPPrPort); external name '_PrClosePage';
{
 *  PrPicFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure PrPicFile(hPrint: THPrint; pPrPort: TPPrPort; pIOBuf: Ptr; pDevBuf: Ptr; prStatus: TPPrStatus); external name '_PrPicFile';
{
 *  PrError()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PrError: SInt16; external name '_PrError';
{
 *  PrSetError()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure PrSetError(iErr: SInt16); external name '_PrSetError';
{$endc}  {CALL_NOT_IN_CARBON}

{$ifc CALL_NOT_IN_CARBON}
{
 *  PrGeneral()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure PrGeneral(pData: Ptr); external name '_PrGeneral';
{$endc}  {CALL_NOT_IN_CARBON}

{$ifc CALL_NOT_IN_CARBON}
{
 *  PrDrvrOpen()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure PrDrvrOpen; external name '_PrDrvrOpen';
{
 *  PrDrvrClose()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure PrDrvrClose; external name '_PrDrvrClose';
{
 *  PrCtlCall()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure PrCtlCall(iWhichCtl: SInt16; lParam1: SInt32; lParam2: SInt32; lParam3: SInt32); external name '_PrCtlCall';
{
 *  PrDrvrDCE()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PrDrvrDCE: Handle; external name '_PrDrvrDCE';
{
 *  PrDrvrVers()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PrDrvrVers: SInt16; external name '_PrDrvrVers';
{
 *  PrLoadDriver()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PrLoadDriver: SInt16; external name '_PrLoadDriver';
{$endc}  {CALL_NOT_IN_CARBON}

{$ALIGN MAC68K}


end.
