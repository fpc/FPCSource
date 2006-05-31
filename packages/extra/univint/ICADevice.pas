{
     File:       ICADevice.p
 
     Contains:   Image Capture device definitions.  This file is included
 
     Version:    Technology: 1.0
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  © 2000-2002 by Apple Computer, Inc., all rights reserved.
 
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

unit ICADevice;
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
uses MacTypes,ICAApplication;


{$ALIGN MAC68K}

{ 
--------------- Completion Procs --------------- 
}
{
   
   NOTE: the parameter for the completion proc (ICDHeader*) has to be casted to the appropriate type
   e.g. (ICD_BuildObjectChildrenPB*), ...
   
}

type
	ICDHeaderPtr = ^ICDHeader;
{$ifc TYPED_FUNCTION_POINTERS}
	ICDCompletion = procedure(pb: ICDHeaderPtr);
{$elsec}
	ICDCompletion = ProcPtr;
{$endc}

	{	 
	--------------- ICDHeader --------------- 
		}
	ICDHeader = record
		err:					OSErr;									{  -->  }
		refcon:					UInt32;									{  <--  }
	end;

	{	
	--------------- Object parameter blocks ---------------
		}
	ICD_NewObjectPBPtr = ^ICD_NewObjectPB;
	ICD_NewObjectPB = record
		header:					ICDHeader;
		parentObject:			ICAObject;								{  <--  }
		objectInfo:				ICAObjectInfo;							{  <--  }
		objct:					ICAObject;								{  -->  }
	end;

	ICD_DisposeObjectPBPtr = ^ICD_DisposeObjectPB;
	ICD_DisposeObjectPB = record
		header:					ICDHeader;
		objct:					ICAObject;								{  <--  }
	end;

	{	
	--------------- Property parameter blocks ---------------
		}
	ICD_NewPropertyPBPtr = ^ICD_NewPropertyPB;
	ICD_NewPropertyPB = record
		header:					ICDHeader;
		objct:					ICAObject;								{  <--  }
		propertyInfo:			ICAPropertyInfo;						{  <--  }
		proprty:				ICAProperty;							{  -->  }
	end;

	ICD_DisposePropertyPBPtr = ^ICD_DisposePropertyPB;
	ICD_DisposePropertyPB = record
		header:					ICDHeader;
		proprty:				ICAProperty;							{  <--  }
	end;

	{
	   
	   NOTE: for all APIs - pass NULL as completion parameter to make a synchronous call 
	   
	}

	{	 
	--------------- Object utilities for device libraries --------------- 
		}
	{
	 *  ICDNewObject()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in ImageCaptureLib 1.0 and later
	 *    CarbonLib:        in CarbonLib 1.1 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function ICDNewObject(var pb: ICD_NewObjectPB; completion: ICDCompletion): OSErr; external name '_ICDNewObject';

{
 *  ICDDisposeObject()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in ImageCaptureLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ICDDisposeObject(var pb: ICD_DisposeObjectPB; completion: ICDCompletion): OSErr; external name '_ICDDisposeObject';

{
 *  ICDNewProperty()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in ImageCaptureLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ICDNewProperty(var pb: ICD_NewPropertyPB; completion: ICDCompletion): OSErr; external name '_ICDNewProperty';

{
 *  ICDDisposeProperty()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in ImageCaptureLib 1.0 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ICDDisposeProperty(var pb: ICD_DisposePropertyPB; completion: ICDCompletion): OSErr; external name '_ICDDisposeProperty';


{$ALIGN MAC68K}


end.
