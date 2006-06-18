{
     File:       QTSMovie.p
 
     Contains:   QuickTime Interfaces.
 
     Version:    Universal Interfaces 3.4.2
 
     Copyright:  © 1990-2002 by Apple Computer, Inc., all rights reserved
 
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

unit QTSMovie;
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
uses MacTypes,Components,Movies,QuickTimeStreaming;


{$ALIGN MAC68K}


const
	kQTSStreamMediaType			= $7374726D (* 'strm' *);


type
	QTSSampleDescriptionPtr = ^QTSSampleDescription;
	QTSSampleDescription = record
		descSize:				SInt32;
		dataFormat:				SInt32;
		resvd1:					SInt32;								{  set to 0 }
		resvd2:					SInt16;								{  set to 0 }
		dataRefIndex:			SInt16;
		version:				UInt32;
		resvd3:					UInt32;									{  set to 0 }
		flags:					SInt32;
																		{  qt atoms follow: }
																		{       long size, long type, some data }
																		{       repeat as necessary }
	end;

	QTSSampleDescriptionHandle			= ^QTSSampleDescriptionPtr;

const
	kQTSSampleDescriptionVersion1 = 1;

	kQTSDefaultMediaTimeScale	= 600;

	{  sample description flags }
	kQTSSampleDescPassSampleDataAsHandleFlag = $00000001;


	{	============================================================================
	        Stream Media Handler
	============================================================================	}
	{	-----------------------------------------
	    Info Selectors
	-----------------------------------------	}
	{	 all indexes start at 1 	}

	kQTSMediaPresentationInfo	= $70726573 (* 'pres' *);						{  QTSMediaPresentationParams*  }
	kQTSMediaNotificationInfo	= $6E6F7469 (* 'noti' *);						{  QTSMediaNotificationParams*  }
	kQTSMediaTotalDataRateInfo	= $64747274 (* 'dtrt' *);						{  UInt32*, bits/sec  }
	kQTSMediaLostPercentInfo	= $6C737063 (* 'lspc' *);						{  Fixed*  }
	kQTSMediaNumStreamsInfo		= $6E737472 (* 'nstr' *);						{  UInt32*  }
	kQTSMediaIndSampleDescriptionInfo = $69736463 (* 'isdc' *);					{  QTSMediaIndSampleDescriptionParams*  }


type
	QTSMediaPresentationParamsPtr = ^QTSMediaPresentationParams;
	QTSMediaPresentationParams = record
		presentationID:			QTSPresentation;
	end;

	QTSMediaNotificationParamsPtr = ^QTSMediaNotificationParams;
	QTSMediaNotificationParams = record
		notificationProc:		QTSNotificationUPP;
		notificationRefCon:		Ptr;
		flags:					SInt32;
	end;

	QTSMediaIndSampleDescriptionParamsPtr = ^QTSMediaIndSampleDescriptionParams;
	QTSMediaIndSampleDescriptionParams = record
		index:					SInt32;
		returnedMediaType:		OSType;
		returnedSampleDescription: SampleDescriptionHandle;
	end;

	{	-----------------------------------------
	    QTS Media Handler Selectors
	-----------------------------------------	}

const
	kQTSMediaSetInfoSelect		= $0100;
	kQTSMediaGetInfoSelect		= $0101;
	kQTSMediaSetIndStreamInfoSelect = $0102;
	kQTSMediaGetIndStreamInfoSelect = $0103;

	{	-----------------------------------------
	    QTS Media Handler functions
	-----------------------------------------	}
	{
	 *  QTSMediaSetInfo()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
	 *    CarbonLib:        in CarbonLib 1.1 and later
	 *    Mac OS X:         in version 10.0 and later
	 *    Windows:          in QTSClient.lib 4.0 and later
	 	}
function QTSMediaSetInfo(mh: MediaHandler; inSelector: OSType; ioParams: UnivPtr): ComponentResult; external name '_QTSMediaSetInfo';
{
 *  QTSMediaGetInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function QTSMediaGetInfo(mh: MediaHandler; inSelector: OSType; ioParams: UnivPtr): ComponentResult; external name '_QTSMediaGetInfo';
{
 *  QTSMediaSetIndStreamInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function QTSMediaSetIndStreamInfo(mh: MediaHandler; inIndex: SInt32; inSelector: OSType; ioParams: UnivPtr): ComponentResult; external name '_QTSMediaSetIndStreamInfo';
{
 *  QTSMediaGetIndStreamInfo()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in QTStreamLib 4.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 *    Windows:          in QTSClient.lib 4.0 and later
 }
function QTSMediaGetIndStreamInfo(mh: MediaHandler; inIndex: SInt32; inSelector: OSType; ioParams: UnivPtr): ComponentResult; external name '_QTSMediaGetIndStreamInfo';
{============================================================================
        Hint Media Handler
============================================================================}

const
	kQTSHintMediaType			= $68696E74 (* 'hint' *);

	kQTSHintTrackReference		= $68696E74 (* 'hint' *);


{$ALIGN MAC68K}


end.
