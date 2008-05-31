{
     File:       HTMLRendering.p
 
     Contains:   HTML Rendering Library Interfaces.
 
     Version:    Technology: 1.0
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  © 1999-2002 by Apple Computer, Inc., all rights reserved
 
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

unit HTMLRendering;
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
uses MacTypes,CFBase,Quickdraw,Events,Files,CodeFragments,Controls,CFData,CFString,CFURL;

{$ALIGN MAC68K}


type
	HRReference    = ^SInt32; { an opaque 32-bit type }
	HRReferencePtr = ^HRReference;  { when a var xx:HRReference parameter can be nil, it is changed to xx: HRReferencePtr }
	{
	 *  HRGetHTMLRenderingLibVersion()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in HTMLRenderingLib 1.0 and later
	 *    CarbonLib:        in CarbonLib 1.1 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function HRGetHTMLRenderingLibVersion(var returnVers: NumVersion): OSStatus; external name '_HRGetHTMLRenderingLibVersion';

{$ifc TARGET_RT_MAC_CFM}
{$elsec}
  {$ifc TARGET_RT_MAC_MACHO}
  {$endc}
{$endc}


const
	kHRRendererHTML32Type		= FourCharCode('ht32');						{  HTML 3.2  }


	{
	 *  HRNewReference()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in HTMLRenderingLib 1.0 and later
	 *    CarbonLib:        in CarbonLib 1.1 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function HRNewReference(var hrRef: HRReference; rendererType: OSType; grafPtr_: GrafPtr): OSStatus; external name '_HRNewReference';

{
 *  HRNewReferenceInWindow()
 *  
 *  Discussion:
 *    Use this API from  a Carbon App. All the contrrols created by the
 *    HTML renderer will be embedded in the root control of the window
 *    specified by the window ref.
 *  
 *  Parameters:
 *    
 *    hrRef:
 *      Pointer to the new reference created and returned by the
 *      renderer.
 *    
 *    rendererType:
 *      Type of the renderer e.g. kHRRendererHTML32Type. Only this type
 *      is supported for now.
 *    
 *    inWindowRef:
 *      Reference to the window for which rendering area will be
 *      specified.
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
function HRNewReferenceInWindow(var hrRef: HRReference; rendererType: OSType; inWindowRef: WindowRef): OSStatus; external name '_HRNewReferenceInWindow';

{
 *  HRDisposeReference()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in HTMLRenderingLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function HRDisposeReference(hrRef: HRReference): OSStatus; external name '_HRDisposeReference';


{
 *  HRFreeMemory()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in HTMLRenderingLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function HRFreeMemory(inBytesNeeded: Size): SInt32; external name '_HRFreeMemory';


{ System level notifications }
{
 *  HRScreenConfigurationChanged()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in HTMLRenderingLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure HRScreenConfigurationChanged; external name '_HRScreenConfigurationChanged';

{
 *  HRIsHREvent()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in HTMLRenderingLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function HRIsHREvent(const (*var*) eventRecord_: EventRecord): boolean; external name '_HRIsHREvent';


{ Drawing }
{
 *  HRSetGrafPtr()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in HTMLRenderingLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function HRSetGrafPtr(hrRef: HRReference; grafPtr_: GrafPtr): OSStatus; external name '_HRSetGrafPtr';

{
 *  HRSetWindowRef()
 *  
 *  Discussion:
 *    Use this API from  a Carbon App. All the contrrols created by the
 *    HTML renderer will be moved in the root control of the window
 *    specified by the window ref. All the drawing will now happen in
 *    the specified window.
 *  
 *  Parameters:
 *    
 *    hrRef:
 *      Reference to the renderer object.
 *    
 *    windowRef:
 *      new Reference to the window to be attached to the above hrRef.
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
function HRSetWindowRef(hrRef: HRReference; windowRef_: WindowRef): OSStatus; external name '_HRSetWindowRef';

{
 *  HRSetEmbeddingControl()
 *  
 *  Discussion:
 *    Use this API to tell the HTML Renderer to embed all the controls
 *    it has created so far and the new controls it creates after this
 *    call to be embedded in the given control. Useful if you wish to
 *    have an HTML displayed with in your dialog. e.g. Software Update
 *    needs this.
 *  
 *  Parameters:
 *    
 *    hrRef:
 *      Reference to the renderer object.
 *    
 *    controlRef:
 *      all the future controls created by renderer are embeded in this
 *      controlRef.
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
function HRSetEmbeddingControl(hrRef: HRReference; controlRef_: ControlRef): OSStatus; external name '_HRSetEmbeddingControl';

{
 *  HRActivate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in HTMLRenderingLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function HRActivate(hrRef: HRReference): OSStatus; external name '_HRActivate';

{
 *  HRDeactivate()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in HTMLRenderingLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function HRDeactivate(hrRef: HRReference): OSStatus; external name '_HRDeactivate';

{
 *  HRDraw()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in HTMLRenderingLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function HRDraw(hrRef: HRReference; updateRgnH: RgnHandle): OSStatus; external name '_HRDraw';

{
 *  HRDrawInPort()
 *  
 *  Discussion:
 *    Use this API from  a Carbon App.  All the drawing will now happen
 *    in the specified port. This is the API you want to use to draw in
 *    an offscreen port, for example when printing. You could also use
 *    this API to draw in an on screen port.
 *  
 *  Parameters:
 *    
 *    hrRef:
 *      Reference to the renderer object.
 *    
 *    updateRgnH:
 *      Region to be updated.
 *    
 *    grafPtr:
 *      A graf pointer to render HTML into.
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
function HRDrawInPort(hrRef: HRReference; updateRgnH: RgnHandle; grafPtr: CGrafPtr): OSStatus; external name '_HRDrawInPort';

{
 *  HRSetRenderingRect()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in HTMLRenderingLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function HRSetRenderingRect(hrRef: HRReference; const (*var*) renderingRect: Rect): OSStatus; external name '_HRSetRenderingRect';

{
 *  HRGetRenderedImageSize()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in HTMLRenderingLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function HRGetRenderedImageSize(hrRef: HRReference; var renderingSize: Point): OSStatus; external name '_HRGetRenderedImageSize';

{
 *  HRGetRenderedImageSize32()
 *  
 *  Discussion:
 *    Use this API when the rendered image could have coordinates
 *    larger than what SInt16 can hold.
 *  
 *  Parameters:
 *    
 *    hrRef:
 *      Reference to the renderer object.
 *    
 *    height:
 *      Height of the image is returned in this parameter.
 *    
 *    width:
 *      Width of the image is returned in this parameter.
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
function HRGetRenderedImageSize32(hrRef: HRReference; var height: UInt32; var width: UInt32): OSStatus; external name '_HRGetRenderedImageSize32';

{
 *  HRScrollToLocation()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in HTMLRenderingLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function HRScrollToLocation(hrRef: HRReference; var location: Point): OSStatus; external name '_HRScrollToLocation';

{
 *  HRScrollToImageLocation32()
 *  
 *  Discussion:
 *    Use this API when specifying location to scroll to. Location is
 *    specified in image space.
 *  
 *  Parameters:
 *    
 *    hrRef:
 *      Reference to the renderer object.
 *    
 *    h:
 *      Horizontal location.
 *    
 *    v:
 *      Vertical location.
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
function HRScrollToImageLocation32(hrRef: HRReference; h: SInt32; v: SInt32): OSStatus; external name '_HRScrollToImageLocation32';

{
 *  HRForceQuickdraw()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in HTMLRenderingLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function HRForceQuickdraw(hrRef: HRReference; forceQuickdraw: boolean): OSStatus; external name '_HRForceQuickdraw';


type
	HRScrollbarState 			= SInt16;
const
	eHRScrollbarOn				= 0;
	eHRScrollbarOff				= 1;
	eHRScrollbarAuto			= 2;

	{
	 *  HRSetScrollbarState()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in HTMLRenderingLib 1.0 and later
	 *    CarbonLib:        in CarbonLib 1.1 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function HRSetScrollbarState(hrRef: HRReference; hScrollbarState: HRScrollbarState; vScrollbarState: HRScrollbarState): OSStatus; external name '_HRSetScrollbarState';

{
 *  HRSetDrawBorder()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in HTMLRenderingLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function HRSetDrawBorder(hrRef: HRReference; drawBorder: boolean): OSStatus; external name '_HRSetDrawBorder';

{
 *  HRSetGrowboxCutout()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in HTMLRenderingLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function HRSetGrowboxCutout(hrRef: HRReference; allowCutout: boolean): OSStatus; external name '_HRSetGrowboxCutout';

{ Navigation }
{
 *  HRGoToFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in HTMLRenderingLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function HRGoToFile(hrRef: HRReference; const (*var*) fsspec_: FSSpec; addToHistory: boolean; forceRefresh: boolean): OSStatus; external name '_HRGoToFile';

{
 *  HRGoToURL()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in HTMLRenderingLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function HRGoToURL(hrRef: HRReference; url: ConstCStringPtr; addToHistory: boolean; forceRefresh: boolean): OSStatus; external name '_HRGoToURL';

{
 *  HRGoToAnchor()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in HTMLRenderingLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function HRGoToAnchor(hrRef: HRReference; anchorName: ConstCStringPtr): OSStatus; external name '_HRGoToAnchor';

{
 *  HRGoToPtr()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in HTMLRenderingLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function HRGoToPtr(hrRef: HRReference; buffer: CStringPtr; bufferSize: UInt32; addToHistory: boolean; forceRefresh: boolean): OSStatus; external name '_HRGoToPtr';

{
 *  HRGoToFSRef()
 *  
 *  Discussion:
 *    Use these API from  a Carbon App instead of using HRGoToFile,
 *    HRGoToURL, HRGoToAnchor and HRGoToPtr. These APIs are same in
 *    behavior with their old counter parts. The only difference is
 *    that they take FSRef, CFURLRef, CFString, and CFData as
 *    parameters.
 *  
 *  Parameters:
 *    
 *    hrRef:
 *      Reference to the renderer object.
 *    
 *    fref:
 *      Reference to HTML file that is be opened and rendered.
 *    
 *    addToHistory:
 *      true if this file URL should be added to history.
 *    
 *    forceRefresh:
 *      true if the rendering area should be refreshed.
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
function HRGoToFSRef(hrRef: HRReference; const (*var*) fref: FSRef; addToHistory: boolean; forceRefresh: boolean): OSStatus; external name '_HRGoToFSRef';

{
 *  HRGoToCFURL()
 *  
 *  Discussion:
 *    Use these API from  a Carbon App instead of using HRGoToFile,
 *    HRGoToURL, HRGoToAnchor and HRGoToPtr. These APIs are same in
 *    behavior with their old counter parts. The only difference is
 *    that they take FSRef, CFURLRef, CFString, and CFData as
 *    parameters.
 *  
 *  Parameters:
 *    
 *    hrRef:
 *      Reference to the renderer object.
 *    
 *    url:
 *      Reference to url that is be rendered.
 *    
 *    addToHistory:
 *      true if this URL should be added to history.
 *    
 *    forceRefresh:
 *      true if the rendering area should be refreshed.
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
function HRGoToCFURL(hrRef: HRReference; url: CFURLRef; addToHistory: boolean; forceRefresh: boolean): OSStatus; external name '_HRGoToCFURL';

{
 *  HRGoToAnchorCFString()
 *  
 *  Discussion:
 *    Use these API from  a Carbon App instead of using HRGoToFile,
 *    HRGoToURL, HRGoToAnchor and HRGoToPtr. These APIs are same in
 *    behavior with their old counter parts. The only difference is
 *    that they take FSRef, CFURLRef, CFString, and CFData as
 *    parameters.
 *  
 *  Parameters:
 *    
 *    hrRef:
 *      Reference to the renderer object.
 *    
 *    anchorName:
 *      Name of the anchor to be displayed.
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
function HRGoToAnchorCFString(hrRef: HRReference; anchorName: CFStringRef): OSStatus; external name '_HRGoToAnchorCFString';

{
 *  HRGoToData()
 *  
 *  Discussion:
 *    Use these API from  a Carbon App instead of using HRGoToFile,
 *    HRGoToURL, HRGoToAnchor and HRGoToPtr. These APIs are same in
 *    behavior with their old counter parts. The only difference is
 *    that they take FSRef, CFURLRef, CFString, and CFData as
 *    parameters.
 *  
 *  Parameters:
 *    
 *    hrRef:
 *      Reference to the renderer object.
 *    
 *    data:
 *      Reference to data in the memory that is be rendered.
 *    
 *    addToHistory:
 *      true if this file URL should be added to history.
 *    
 *    forceRefresh:
 *      true if the rendering area should be refreshed.
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
function HRGoToData(hrRef: HRReference; data: CFDataRef; addToHistory: boolean; forceRefresh: boolean): OSStatus; external name '_HRGoToData';

{ Accessors }
{ either file url or url of <base> tag }
{
 *  HRGetRootURL()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in HTMLRenderingLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function HRGetRootURL(hrRef: HRReference; rootURLH: Handle): OSStatus; external name '_HRGetRootURL';

{ url of <base> tag }
{
 *  HRGetBaseURL()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in HTMLRenderingLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function HRGetBaseURL(hrRef: HRReference; baseURLH: Handle): OSStatus; external name '_HRGetBaseURL';

{ file url }
{
 *  HRGetHTMLURL()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in HTMLRenderingLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function HRGetHTMLURL(hrRef: HRReference; HTMLURLH: Handle): OSStatus; external name '_HRGetHTMLURL';

{
 *  HRGetTitle()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in HTMLRenderingLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function HRGetTitle(hrRef: HRReference; title: StringPtr): OSStatus; external name '_HRGetTitle';

{
 *  HRGetHTMLFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in HTMLRenderingLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function HRGetHTMLFile(hrRef: HRReference; var fsspec_: FSSpec): OSStatus; external name '_HRGetHTMLFile';


{
 *  HRGetRootURLAsCFString()
 *  
 *  Discussion:
 *    Use these API from  a Carbon App instead of using HRGetRootURL,
 *    HRGetBaseURL, HRGetHTMLURL, HRGetTitle and HRGetHTMLFile. These
 *    APIs are same in behavior with their old counter parts. The only
 *    difference is that they take CFString, CFURLRef, and FSRef as
 *    parameters.
 *  
 *  Parameters:
 *    
 *    hrRef:
 *      Reference to the renderer object.
 *    
 *    rootString:
 *      Get CFString equivalent for the root url.
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
function HRGetRootURLAsCFString(hrRef: HRReference; var rootString: CFStringRef): OSStatus; external name '_HRGetRootURLAsCFString';

{
 *  HRGetBaseURLAsCFString()
 *  
 *  Discussion:
 *    Use these API from  a Carbon App instead of using HRGetRootURL,
 *    HRGetBaseURL, HRGetHTMLURL, HRGetTitle and HRGetHTMLFile. These
 *    APIs are same in behavior with their old counter parts. The only
 *    difference is that they take CFString, CFURLRef, and FSRef as
 *    parameters.
 *  
 *  Parameters:
 *    
 *    hrRef:
 *      Reference to the renderer object.
 *    
 *    baseString:
 *      Get CFString equivalent for the base url.
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
function HRGetBaseURLAsCFString(hrRef: HRReference; var baseString: CFStringRef): OSStatus; external name '_HRGetBaseURLAsCFString';

{
 *  HRGetHTMLURLAsCFURL()
 *  
 *  Discussion:
 *    Use these API from  a Carbon App instead of using HRGetRootURL,
 *    HRGetBaseURL, HRGetHTMLURL, HRGetTitle and HRGetHTMLFile. These
 *    APIs are same in behavior with their old counter parts. The only
 *    difference is that they take CFString, CFURLRef, and FSRef as
 *    parameters.
 *  
 *  Parameters:
 *    
 *    hrRef:
 *      Reference to the renderer object.
 *    
 *    theURL:
 *      Get currently displayed HTML as a CFURL.
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
function HRGetHTMLURLAsCFURL(hrRef: HRReference; var theURL: CFURLRef): OSStatus; external name '_HRGetHTMLURLAsCFURL';

{
 *  HRGetTitleAsCFString()
 *  
 *  Discussion:
 *    Use these API from  a Carbon App instead of using HRGetRootURL,
 *    HRGetBaseURL, HRGetHTMLURL, HRGetTitle and HRGetHTMLFile. These
 *    APIs are same in behavior with their old counter parts. The only
 *    difference is that they take CFString, CFURLRef, and FSRef as
 *    parameters.
 *  
 *  Parameters:
 *    
 *    hrRef:
 *      Reference to the renderer object.
 *    
 *    title:
 *      Get title of the currently displayed HTML as a CFString.
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
function HRGetTitleAsCFString(hrRef: HRReference; var title: CFStringRef): OSStatus; external name '_HRGetTitleAsCFString';

{
 *  HRGetHTMLFileAsFSRef()
 *  
 *  Discussion:
 *    Use these API from  a Carbon App instead of using HRGetRootURL,
 *    HRGetBaseURL, HRGetHTMLURL, HRGetTitle and HRGetHTMLFile. These
 *    APIs are same in behavior with their old counter parts. The only
 *    difference is that they take CFString, CFURLRef, and FSRef as
 *    parameters.
 *  
 *  Parameters:
 *    
 *    hrRef:
 *      Reference to the renderer object.
 *    
 *    fref:
 *      Get currently displayed HTML as a FSRef.
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
function HRGetHTMLFileAsFSRef(hrRef: HRReference; var fref: FSRef): OSStatus; external name '_HRGetHTMLFileAsFSRef';

{ Utilities }
{
 *  HRUtilCreateFullURL()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in HTMLRenderingLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function HRUtilCreateFullURL(rootURL: ConstCStringPtr; linkURL: ConstCStringPtr; fullURLH: Handle): OSStatus; external name '_HRUtilCreateFullURL';

{
 *  HRUtilGetFSSpecFromURL()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in HTMLRenderingLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function HRUtilGetFSSpecFromURL(rootURL: ConstCStringPtr; linkURL: ConstCStringPtr; var destSpec: FSSpec): OSStatus; external name '_HRUtilGetFSSpecFromURL';

{ urlHandle should be valid on input }
{
 *  HRUtilGetURLFromFSSpec()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in HTMLRenderingLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function HRUtilGetURLFromFSSpec(const (*var*) fsspec_: FSSpec; urlHandle: Handle): OSStatus; external name '_HRUtilGetURLFromFSSpec';


{
 *  HRUtilCreateFullCFURL()
 *  
 *  Discussion:
 *    Use these API from  a Carbon App instead of using
 *    HRUtilCreateFullURL, HRUtilGetFSSpecFromURL,
 *    HRUtilGetURLFromFSSpec. These APIs are same in behavior with
 *    their old counter parts. The only difference is that they take
 *    CFURLRef, and FSRef as parameters.
 *  
 *  Parameters:
 *    
 *    rootString:
 *      a CFString for the root.
 *    
 *    linkString:
 *      a CFString for a partial link.
 *    
 *    url:
 *      Fully qualified URL is returned after attaching a link string
 *      to the root.
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
function HRUtilCreateFullCFURL(rootString: CFStringRef; linkString: CFStringRef; var url: CFURLRef): OSStatus; external name '_HRUtilCreateFullCFURL';

{
 *  HRUtilGetFSRefFromURL()
 *  
 *  Discussion:
 *    Use these API from  a Carbon App instead of using
 *    HRUtilCreateFullURL, HRUtilGetFSSpecFromURL,
 *    HRUtilGetURLFromFSSpec. These APIs are same in behavior with
 *    their old counter parts. The only difference is that they take
 *    CFURLRef, and FSRef as parameters.
 *  
 *  Parameters:
 *    
 *    rootString:
 *      a CFString for the root.
 *    
 *    linkString:
 *      a CFString for a partial link.
 *    
 *    destRef:
 *      File reference is returned for the complete path created after
 *      attaching link string to the root. If File does not exist,
 *      fnfErr is returned as a function result.
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
function HRUtilGetFSRefFromURL(rootString: CFStringRef; linkString: CFStringRef; var destRef: FSRef): OSStatus; external name '_HRUtilGetFSRefFromURL';

{
 *  HRUtilGetURLFromFSRef()
 *  
 *  Discussion:
 *    Use these API from  a Carbon App instead of using
 *    HRUtilCreateFullURL, HRUtilGetFSSpecFromURL,
 *    HRUtilGetURLFromFSSpec. These APIs are same in behavior with
 *    their old counter parts. The only difference is that they take
 *    CFURLRef, and FSRef as parameters.
 *  
 *  Parameters:
 *    
 *    fileRef:
 *      Refernce to a file whose URL is desired.
 *    
 *    url:
 *      a fully qualified URL is returned in this parameter. The
 *      returned URL gives the path of the file specified in the above
 *      parameter.
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
function HRUtilGetURLFromFSRef(const (*var*) fileRef: FSRef; var url: CFURLRef): OSStatus; external name '_HRUtilGetURLFromFSRef';

{
    Visited links

    If you register a function here, it will be called to determine
    whether or not the given URL has been visited. It should return
    true if the URL has been visited.
    
    In addition to the URLs that the application may add to the list
    of visited links, it should also add URLs that the user clicks
    on. These URLs can be caught by the "add URL to history" callback
    below.
 }

type
{$ifc TYPED_FUNCTION_POINTERS}
	HRWasURLVisitedProcPtr = function(url: ConstCStringPtr; refCon: UnivPtr): boolean;
{$elsec}
	HRWasURLVisitedProcPtr = ProcPtr;
{$endc}

{$ifc OPAQUE_UPP_TYPES}
	HRWasURLVisitedUPP = ^SInt32; { an opaque UPP }
{$elsec}
	HRWasURLVisitedUPP = UniversalProcPtr;
{$endc}	
	{
	 *  HRRegisterWasURLVisitedUPP()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in HTMLRenderingLib 1.0 and later
	 *    CarbonLib:        in CarbonLib 1.1 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
procedure HRRegisterWasURLVisitedUPP(inWasURLVisitedUPP: HRWasURLVisitedUPP; hrRef: HRReference; inRefCon: UnivPtr); external name '_HRRegisterWasURLVisitedUPP';

{
 *  HRUnregisterWasURLVisitedUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in HTMLRenderingLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure HRUnregisterWasURLVisitedUPP(hrRef: HRReference); external name '_HRUnregisterWasURLVisitedUPP';

{
    Use these API from  a Carbon App instead of using HRRegisterWasURLVisitedUPP, HRUnregisterWasURLVisitedUPP. 
    These APIs are same in behavior with their old counter parts. The only difference is that they take 
    CFURLRef as parameters.
        
}

type
{$ifc TYPED_FUNCTION_POINTERS}
	HRWasCFURLVisitedProcPtr = function(url: CFURLRef; refCon: UnivPtr): boolean;
{$elsec}
	HRWasCFURLVisitedProcPtr = ProcPtr;
{$endc}

{$ifc OPAQUE_UPP_TYPES}
	HRWasCFURLVisitedUPP = ^SInt32; { an opaque UPP }
{$elsec}
	HRWasCFURLVisitedUPP = HRWasCFURLVisitedProcPtr;
{$endc}	
	{
	 *  HRRegisterWasCFURLVisitedUPP()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   not available
	 *    CarbonLib:        in CarbonLib 1.3 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
procedure HRRegisterWasCFURLVisitedUPP(inWasCFURLVisitedUPP: HRWasCFURLVisitedUPP; hrRef: HRReference; inRefCon: UnivPtr); external name '_HRRegisterWasCFURLVisitedUPP';

{
 *  HRUnregisterWasCFURLVisitedUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure HRUnregisterWasCFURLVisitedUPP(hrRef: HRReference); external name '_HRUnregisterWasCFURLVisitedUPP';


{
    New URL

    If you register a function here, it will be called every time
    the renderer is going to display a new URL. A few examples of how
    you might use this include...
    
        (a) maintaining a history of URLs
        (b) maintainging a list of visited links
        (c) setting a window title based on the new URL
}

type
{$ifc TYPED_FUNCTION_POINTERS}
	HRNewURLProcPtr = function(url: ConstCStringPtr; targetFrame: ConstCStringPtr; addToHistory: boolean; refCon: UnivPtr): OSStatus;
{$elsec}
	HRNewURLProcPtr = ProcPtr;
{$endc}

{$ifc OPAQUE_UPP_TYPES}
	HRNewURLUPP = ^SInt32; { an opaque UPP }
{$elsec}
	HRNewURLUPP = UniversalProcPtr;
{$endc}	
	{
	 *  HRRegisterNewURLUPP()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in HTMLRenderingLib 1.0 and later
	 *    CarbonLib:        in CarbonLib 1.1 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
procedure HRRegisterNewURLUPP(inNewURLUPP: HRNewURLUPP; hrRef: HRReference; inRefCon: UnivPtr); external name '_HRRegisterNewURLUPP';

{
 *  HRUnregisterNewURLUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in HTMLRenderingLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure HRUnregisterNewURLUPP(hrRef: HRReference); external name '_HRUnregisterNewURLUPP';


{ 
    Use these API from  a Carbon App instead of using HRRegisterNewURLUPP, HRUnregisterNewURLUPP. 
    These APIs are same in behavior with their old counter parts. The only difference is that they take 
    CFURLRef as parameters.
}

type
{$ifc TYPED_FUNCTION_POINTERS}
	HRNewCFURLProcPtr = function(url: CFURLRef; targetString: CFStringRef; addToHistory: boolean; refCon: UnivPtr): OSStatus;
{$elsec}
	HRNewCFURLProcPtr = ProcPtr;
{$endc}

{$ifc OPAQUE_UPP_TYPES}
	HRNewCFURLUPP = ^SInt32; { an opaque UPP }
{$elsec}
	HRNewCFURLUPP = HRNewCFURLProcPtr;
{$endc}	
	{
	 *  HRRegisterNewCFURLUPP()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   not available
	 *    CarbonLib:        in CarbonLib 1.3 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
procedure HRRegisterNewCFURLUPP(inURLUPP: HRNewCFURLUPP; hrRef: HRReference; inRefCon: UnivPtr); external name '_HRRegisterNewCFURLUPP';

{
 *  HRUnregisterNewCFURLUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure HRUnregisterNewCFURLUPP(hrRef: HRReference); external name '_HRUnregisterNewCFURLUPP';


{
    URL to FSSpec function

    If you register a function here, it will be called every time
    the renderer is going to locate a file. The function will be
    passed an enum indicating the type of file being asked for.
 }

type
	URLSourceType 				= UInt16;
const
	kHRLookingForHTMLSource		= 1;
	kHRLookingForImage			= 2;
	kHRLookingForEmbedded		= 3;
	kHRLookingForImageMap		= 4;
	kHRLookingForFrame			= 5;


type
{$ifc TYPED_FUNCTION_POINTERS}
	HRURLToFSSpecProcPtr = function(rootURL: ConstCStringPtr; linkURL: ConstCStringPtr; var fsspec_: FSSpec; urlSourceType_: URLSourceType; refCon: UnivPtr): OSStatus;
{$elsec}
	HRURLToFSSpecProcPtr = ProcPtr;
{$endc}

{$ifc OPAQUE_UPP_TYPES}
	HRURLToFSSpecUPP = ^SInt32; { an opaque UPP }
{$elsec}
	HRURLToFSSpecUPP = UniversalProcPtr;
{$endc}	
	{
	 *  HRRegisterURLToFSSpecUPP()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in HTMLRenderingLib 1.0 and later
	 *    CarbonLib:        in CarbonLib 1.1 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
procedure HRRegisterURLToFSSpecUPP(inURLToFSSpecUPP: HRURLToFSSpecUPP; hrRef: HRReference; inRefCon: UnivPtr); external name '_HRRegisterURLToFSSpecUPP';

{
 *  HRUnregisterURLToFSSpecUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in HTMLRenderingLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure HRUnregisterURLToFSSpecUPP(hrRef: HRReference); external name '_HRUnregisterURLToFSSpecUPP';


{ 
    Use these API from  a Carbon App instead of using HRRegisterURLToFSSpecUPP, HRUnregisterURLToFSSpecUPP. 
    These APIs are same in behavior with their old counter parts. The only difference is that they take 
    FSRef as parameters.
}

type
{$ifc TYPED_FUNCTION_POINTERS}
	HRURLToFSRefProcPtr = function(rootString: CFStringRef; linkString: CFStringRef; var fref: FSRef; urlSourceType_: URLSourceType; refCon: UnivPtr): OSStatus;
{$elsec}
	HRURLToFSRefProcPtr = ProcPtr;
{$endc}

{$ifc OPAQUE_UPP_TYPES}
	HRURLToFSRefUPP = ^SInt32; { an opaque UPP }
{$elsec}
	HRURLToFSRefUPP = HRURLToFSRefProcPtr;
{$endc}	
	{
	 *  HRRegisterURLToFSRefUPP()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   not available
	 *    CarbonLib:        in CarbonLib 1.3 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
procedure HRRegisterURLToFSRefUPP(inURLToFSRefUPP: HRURLToFSRefUPP; hrRef: HRReference; inRefCon: UnivPtr); external name '_HRRegisterURLToFSRefUPP';

{
 *  HRUnregisterURLToFSRefUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure HRUnregisterURLToFSRefUPP(hrRef: HRReference); external name '_HRUnregisterURLToFSRefUPP';


const
	uppHRWasURLVisitedProcInfo = $000003D0;
	uppHRWasCFURLVisitedProcInfo = $000003D0;
	uppHRNewURLProcInfo = $000037F0;
	uppHRNewCFURLProcInfo = $000037F0;
	uppHRURLToFSSpecProcInfo = $0000EFF0;
	uppHRURLToFSRefProcInfo = $0000EFF0;
	{
	 *  NewHRWasURLVisitedUPP()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   available as macro/inline
	 *    CarbonLib:        in CarbonLib 1.1 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function NewHRWasURLVisitedUPP(userRoutine: HRWasURLVisitedProcPtr): HRWasURLVisitedUPP; external name '_NewHRWasURLVisitedUPP'; { old name was NewHRWasURLVisitedProc }
{
 *  NewHRWasCFURLVisitedUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewHRWasCFURLVisitedUPP(userRoutine: HRWasCFURLVisitedProcPtr): HRWasCFURLVisitedUPP; external name '_NewHRWasCFURLVisitedUPP';
{
 *  NewHRNewURLUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewHRNewURLUPP(userRoutine: HRNewURLProcPtr): HRNewURLUPP; external name '_NewHRNewURLUPP'; { old name was NewHRNewURLProc }
{
 *  NewHRNewCFURLUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewHRNewCFURLUPP(userRoutine: HRNewCFURLProcPtr): HRNewCFURLUPP; external name '_NewHRNewCFURLUPP';
{
 *  NewHRURLToFSSpecUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewHRURLToFSSpecUPP(userRoutine: HRURLToFSSpecProcPtr): HRURLToFSSpecUPP; external name '_NewHRURLToFSSpecUPP'; { old name was NewHRURLToFSSpecProc }
{
 *  NewHRURLToFSRefUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewHRURLToFSRefUPP(userRoutine: HRURLToFSRefProcPtr): HRURLToFSRefUPP; external name '_NewHRURLToFSRefUPP';
{
 *  DisposeHRWasURLVisitedUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeHRWasURLVisitedUPP(userUPP: HRWasURLVisitedUPP); external name '_DisposeHRWasURLVisitedUPP';
{
 *  DisposeHRWasCFURLVisitedUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeHRWasCFURLVisitedUPP(userUPP: HRWasCFURLVisitedUPP); external name '_DisposeHRWasCFURLVisitedUPP';
{
 *  DisposeHRNewURLUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeHRNewURLUPP(userUPP: HRNewURLUPP); external name '_DisposeHRNewURLUPP';
{
 *  DisposeHRNewCFURLUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeHRNewCFURLUPP(userUPP: HRNewCFURLUPP); external name '_DisposeHRNewCFURLUPP';
{
 *  DisposeHRURLToFSSpecUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeHRURLToFSSpecUPP(userUPP: HRURLToFSSpecUPP); external name '_DisposeHRURLToFSSpecUPP';
{
 *  DisposeHRURLToFSRefUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeHRURLToFSRefUPP(userUPP: HRURLToFSRefUPP); external name '_DisposeHRURLToFSRefUPP';
{
 *  InvokeHRWasURLVisitedUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeHRWasURLVisitedUPP(url: ConstCStringPtr; refCon: UnivPtr; userRoutine: HRWasURLVisitedUPP): boolean; external name '_InvokeHRWasURLVisitedUPP'; { old name was CallHRWasURLVisitedProc }
{
 *  InvokeHRWasCFURLVisitedUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeHRWasCFURLVisitedUPP(url: CFURLRef; refCon: UnivPtr; userRoutine: HRWasCFURLVisitedUPP): boolean; external name '_InvokeHRWasCFURLVisitedUPP';
{
 *  InvokeHRNewURLUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeHRNewURLUPP(url: ConstCStringPtr; targetFrame: ConstCStringPtr; addToHistory: boolean; refCon: UnivPtr; userRoutine: HRNewURLUPP): OSStatus; external name '_InvokeHRNewURLUPP'; { old name was CallHRNewURLProc }
{
 *  InvokeHRNewCFURLUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeHRNewCFURLUPP(url: CFURLRef; targetString: CFStringRef; addToHistory: boolean; refCon: UnivPtr; userRoutine: HRNewCFURLUPP): OSStatus; external name '_InvokeHRNewCFURLUPP';
{
 *  InvokeHRURLToFSSpecUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeHRURLToFSSpecUPP(rootURL: ConstCStringPtr; linkURL: ConstCStringPtr; var fsspec_: FSSpec; urlSourceType_: URLSourceType; refCon: UnivPtr; userRoutine: HRURLToFSSpecUPP): OSStatus; external name '_InvokeHRURLToFSSpecUPP'; { old name was CallHRURLToFSSpecProc }
{
 *  InvokeHRURLToFSRefUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.3 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeHRURLToFSRefUPP(rootString: CFStringRef; linkString: CFStringRef; var fref: FSRef; urlSourceType_: URLSourceType; refCon: UnivPtr; userRoutine: HRURLToFSRefUPP): OSStatus; external name '_InvokeHRURLToFSRefUPP';
{$ALIGN MAC68K}


end.
