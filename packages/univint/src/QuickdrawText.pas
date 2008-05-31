{
     File:       QuickdrawText.p
 
     Contains:   Quickdraw Text Interfaces.
 
     Version:    Technology: Mac OS 8.5
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  © 1983-2002 by Apple Computer, Inc., all rights reserved.
 
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

unit QuickdrawText;
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
uses MacTypes,MixedMode,IntlResources;


{$ALIGN MAC68K}

{ new CGrafPort bottleneck ("newProc2") function, used in Unicode Text drawing }
{
 *  StandardGlyphs()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QuickDrawText 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function StandardGlyphs(dataStream: UnivPtr; size: ByteCount): OSStatus; external name '_StandardGlyphs';


const
																{  CharToPixel directions  }
	leftCaret					= 0;							{ Place caret for left block }
	rightCaret					= -1;							{ Place caret for right block }
	kHilite						= 1;							{ Direction is SysDirection }

	smLeftCaret					= 0;							{ Place caret for left block - obsolete  }
	smRightCaret				= -1;							{ Place caret for right block - obsolete  }
	smHilite					= 1;							{ Direction is TESysJust - obsolete  }

																{ Constants for styleRunPosition argument in PortionLine, DrawJustified, MeasureJustified, CharToPixel, and PixelToChar. }
	onlyStyleRun				= 0;							{  This is the only style run on the line  }
	leftStyleRun				= 1;							{  This is leftmost of multiple style runs on the line  }
	rightStyleRun				= 2;							{  This is rightmost of multiple style runs on the line  }
	middleStyleRun				= 3;							{  There are multiple style runs on the line and this is neither the leftmost nor the rightmost.  }
	smOnlyStyleRun				= 0;							{  obsolete  }
	smLeftStyleRun				= 1;							{  obsolete  }
	smRightStyleRun				= 2;							{  obsolete  }
	smMiddleStyleRun			= 3;							{  obsolete  }

	{	 type for styleRunPosition parameter in PixelToChar etc. 	}

type
	JustStyleCode						= SInt16;
	{	 Type for truncWhere parameter in TruncString, TruncText 	}
	TruncCode							= SInt16;

const
																{  Constants for truncWhere argument in TruncString and TruncText  }
	truncEnd					= 0;							{  Truncate at end  }
	truncMiddle					= $4000;						{  Truncate in middle  }
	smTruncEnd					= 0;							{  Truncate at end - obsolete  }
	smTruncMiddle				= $4000;						{  Truncate in middle - obsolete  }

																{  Constants for TruncString and TruncText results  }
	notTruncated				= 0;							{  No truncation was necessary  }
	truncated					= 1;							{  Truncation performed  }
	truncErr					= -1;							{  General error  }
	smNotTruncated				= 0;							{  No truncation was necessary - obsolete  }
	smTruncated					= 1;							{  Truncation performed   - obsolete  }
	smTruncErr					= -1;							{  General error - obsolete  }


type
	StyledLineBreakCode					= SInt8;

const
	smBreakWord					= 0;
	smBreakChar					= 1;
	smBreakOverflow				= 2;

	{	QuickTime3.0	}
																{  Constants for txFlags (which used to be the pad field after txFace)  }
	tfAntiAlias					= $01;
	tfUnicode					= $02;


type
	FontInfoPtr = ^FontInfo;
	FontInfo = record
		ascent:					SInt16;
		descent:				SInt16;
		widMax:					SInt16;
		leading:				SInt16;
	end;

	FormatOrder							= array [0..0] of SInt16;
	FormatOrderPtr						= ^FormatOrder;
	{	 FormatStatus was moved to TextUtils.i 	}
	{	 OffsetTable moved to IntlResources.i 	}

{$ifc TYPED_FUNCTION_POINTERS}
	StyleRunDirectionProcPtr = function(styleRunIndex: SInt16; dirParam: UnivPtr): boolean;
{$elsec}
	StyleRunDirectionProcPtr = ProcPtr;
{$endc}

{$ifc OPAQUE_UPP_TYPES}
	StyleRunDirectionUPP = ^SInt32; { an opaque UPP }
{$elsec}
	StyleRunDirectionUPP = UniversalProcPtr;
{$endc}	

const
	uppStyleRunDirectionProcInfo = $00000390;
	{
	 *  NewStyleRunDirectionUPP()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   available as macro/inline
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function NewStyleRunDirectionUPP(userRoutine: StyleRunDirectionProcPtr): StyleRunDirectionUPP; external name '_NewStyleRunDirectionUPP'; { old name was NewStyleRunDirectionProc }
{
 *  DisposeStyleRunDirectionUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeStyleRunDirectionUPP(userUPP: StyleRunDirectionUPP); external name '_DisposeStyleRunDirectionUPP';
{
 *  InvokeStyleRunDirectionUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeStyleRunDirectionUPP(styleRunIndex: SInt16; dirParam: UnivPtr; userRoutine: StyleRunDirectionUPP): boolean; external name '_InvokeStyleRunDirectionUPP'; { old name was CallStyleRunDirectionProc }
{$ifc CALL_NOT_IN_CARBON}
{
 *  Pixel2Char()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function Pixel2Char(textBuf: Ptr; textLen: SInt16; slop: SInt16; pixelWidth: SInt16; var leadingEdge: boolean): SInt16; external name '_Pixel2Char';
{
 *  Char2Pixel()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function Char2Pixel(textBuf: Ptr; textLen: SInt16; slop: SInt16; offset: SInt16; direction: SInt16): SInt16; external name '_Char2Pixel';
{$endc}  {CALL_NOT_IN_CARBON}

{
 *  PixelToChar()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PixelToChar(textBuf: Ptr; textLength: SInt32; slop: Fixed; pixelWidth: Fixed; var leadingEdge: boolean; var widthRemaining: Fixed; styleRunPosition: JustStyleCode; numer: Point; denom: Point): SInt16; external name '_PixelToChar';
{
 *  CharToPixel()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function CharToPixel(textBuf: Ptr; textLength: SInt32; slop: Fixed; offset: SInt32; direction: SInt16; styleRunPosition: JustStyleCode; numer: Point; denom: Point): SInt16; external name '_CharToPixel';
{
 *  DrawJustified()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DrawJustified(textPtr: Ptr; textLength: SInt32; slop: Fixed; styleRunPosition: JustStyleCode; numer: Point; denom: Point); external name '_DrawJustified';
{
 *  MeasureJustified()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure MeasureJustified(textPtr: Ptr; textLength: SInt32; slop: Fixed; charLocs: Ptr; styleRunPosition: JustStyleCode; numer: Point; denom: Point); external name '_MeasureJustified';
{
 *  PortionLine()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function PortionLine(textPtr: Ptr; textLen: SInt32; styleRunPosition: JustStyleCode; numer: Point; denom: Point): Fixed; external name '_PortionLine';
{
 *  HiliteText()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure HiliteText(textPtr: Ptr; textLength: SInt16; firstOffset: SInt16; secondOffset: SInt16; var offsets: OffsetTable); external name '_HiliteText';
{$ifc CALL_NOT_IN_CARBON}
{
 *  DrawJust()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure DrawJust(textPtr: Ptr; textLength: SInt16; slop: SInt16); external name '_DrawJust';
{
 *  MeasureJust()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure MeasureJust(textPtr: Ptr; textLength: SInt16; slop: SInt16; charLocs: Ptr); external name '_MeasureJust';
{
 *  PortionText()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function PortionText(textPtr: Ptr; textLength: SInt32): Fixed; external name '_PortionText';
{$endc}  {CALL_NOT_IN_CARBON}

{
 *  VisibleLength()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function VisibleLength(textPtr: Ptr; textLength: SInt32): SInt32; external name '_VisibleLength';
{
 *  GetFormatOrder()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure GetFormatOrder(ordering: FormatOrderPtr; firstFormat: SInt16; lastFormat: SInt16; lineRight: boolean; rlDirProc: StyleRunDirectionUPP; dirParam: Ptr); external name '_GetFormatOrder';
{
 *  TextFont()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure TextFont(font: SInt16); external name '_TextFont';
{
 *  TextFace()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure TextFace(face: StyleParameter); external name '_TextFace';
{
 *  TextMode()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure TextMode(mode: SInt16); external name '_TextMode';
{
 *  TextSize()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure TextSize(size: SInt16); external name '_TextSize';
{
 *  SpaceExtra()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure SpaceExtra(extra: Fixed); external name '_SpaceExtra';
{
 *  DrawChar()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DrawChar(ch: CharParameter); external name '_DrawChar';
{
 *  DrawString()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DrawString(const (*var*) s: Str255); external name '_DrawString';
{
 *  [Mac]DrawText()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DrawText(textBuf: UnivPtr; firstByte: SInt16; byteCount: SInt16); external name '_DrawText';
{
 *  CharWidth()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function CharWidth(ch: CharParameter): SInt16; external name '_CharWidth';
{
 *  StringWidth()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function StringWidth(const (*var*) s: Str255): SInt16; external name '_StringWidth';
{
 *  TextWidth()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function TextWidth(textBuf: UnivPtr; firstByte: SInt16; byteCount: SInt16): SInt16; external name '_TextWidth';
{
 *  MeasureText()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure MeasureText(count: SInt16; textAddr: UnivPtr; charLocs: UnivPtr); external name '_MeasureText';
{
 *  GetFontInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure GetFontInfo(var info: FontInfo); external name '_GetFontInfo';
{
 *  CharExtra()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure CharExtra(extra: Fixed); external name '_CharExtra';
{
 *  StdText()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure StdText(count: SInt16; textAddr: UnivPtr; numer: Point; denom: Point); external name '_StdText';
{
 *  StdTxMeas()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function StdTxMeas(byteCount: SInt16; textAddr: UnivPtr; var numer: Point; var denom: Point; var info: FontInfo): SInt16; external name '_StdTxMeas';
{
 *  StyledLineBreak()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function StyledLineBreak(textPtr: Ptr; textLen: SInt32; textStart: SInt32; textEnd: SInt32; flags: SInt32; var textWidth: Fixed; var textOffset: SInt32): StyledLineBreakCode; external name '_StyledLineBreak';
{
 *  TruncString()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function TruncString(width: SInt16; var theString: Str255; truncWhere: TruncCode): SInt16; external name '_TruncString';
{
 *  TruncText()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function TruncText(width: SInt16; textPtr: Ptr; var length: SInt16; truncWhere: TruncCode): SInt16; external name '_TruncText';
{$ifc OLDROUTINENAMES}
{$ifc CALL_NOT_IN_CARBON}
{
 *  NPixel2Char()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function NPixel2Char(textBuf: Ptr; textLength: SInt32; slop: Fixed; pixelWidth: Fixed; var leadingEdge: boolean; var widthRemaining: Fixed; styleRunPosition: JustStyleCode; numer: Point; denom: Point): SInt16; external name '_NPixel2Char';
{
 *  NChar2Pixel()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function NChar2Pixel(textBuf: Ptr; textLength: SInt32; slop: Fixed; offset: SInt32; direction: SInt16; styleRunPosition: JustStyleCode; numer: Point; denom: Point): SInt16; external name '_NChar2Pixel';
{
 *  NDrawJust()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure NDrawJust(textPtr: Ptr; textLength: SInt32; slop: Fixed; styleRunPosition: JustStyleCode; numer: Point; denom: Point); external name '_NDrawJust';
{
 *  NMeasureJust()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
procedure NMeasureJust(textPtr: Ptr; textLength: SInt32; slop: Fixed; charLocs: Ptr; styleRunPosition: JustStyleCode; numer: Point; denom: Point); external name '_NMeasureJust';
{
 *  NPortionText()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        not available
 *    Mac OS X:         not available
 }
function NPortionText(textPtr: Ptr; textLen: SInt32; styleRunPosition: JustStyleCode; numer: Point; denom: Point): Fixed; external name '_NPortionText';
{$endc}  {CALL_NOT_IN_CARBON}
{$endc}  {OLDROUTINENAMES}

{$ALIGN MAC68K}


end.
