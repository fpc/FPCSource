{
     File:       CMScriptingPlugin.p
 
     Contains:   ColorSync Scripting Plugin API
 
     Version:    Technology: ColorSync 2.5
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  © 1998-2002 by Apple Computer, Inc., all rights reserved.
 
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

unit CMScriptingPlugin;
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
uses MacTypes,CMTypes,AEDataModel,Files,CMApplication;


{$ALIGN MAC68K}


const
																{  ColorSync Scripting AppleEvent Errors  }
	cmspInvalidImageFile		= -4220;						{  Plugin cannot handle this image file type  }
	cmspInvalidImageSpace		= -4221;						{  Plugin cannot create an image file of this colorspace  }
	cmspInvalidProfileEmbed		= -4222;						{  Specific invalid profile errors  }
	cmspInvalidProfileSource	= -4223;
	cmspInvalidProfileDest		= -4224;
	cmspInvalidProfileProof		= -4225;
	cmspInvalidProfileLink		= -4226;


	{	*** embedFlags field  ***	}
	{	 reserved for future use: currently 0 	}

	{	*** matchFlags field  ***	}
	cmspFavorEmbeddedMask		= $00000001;					{  if bit 0 is 0 then use srcProf profile, if 1 then use profile embedded in image if present }


	{	*** scripting plugin entry points  ***	}

type
{$ifc TYPED_FUNCTION_POINTERS}
	ValidateImageProcPtr = function(const (*var*) spec: FSSpec): CMError;
{$elsec}
	ValidateImageProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	GetImageSpaceProcPtr = function(const (*var*) spec: FSSpec; var space: OSType): CMError;
{$elsec}
	GetImageSpaceProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	ValidateSpaceProcPtr = function(const (*var*) spec: FSSpec; var space: OSType): CMError;
{$elsec}
	ValidateSpaceProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	EmbedImageProcPtr = function(const (*var*) specFrom: FSSpec; const (*var*) specInto: FSSpec; embedProf: CMProfileRef; embedFlags: UInt32): CMError;
{$elsec}
	EmbedImageProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	UnembedImageProcPtr = function(const (*var*) specFrom: FSSpec; const (*var*) specInto: FSSpec): CMError;
{$elsec}
	UnembedImageProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	MatchImageProcPtr = function(const (*var*) specFrom: FSSpec; const (*var*) specInto: FSSpec; qual: UInt32; srcIntent: UInt32; srcProf: CMProfileRef; dstProf: CMProfileRef; prfProf: CMProfileRef; matchFlags: UInt32): CMError;
{$elsec}
	MatchImageProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	CountImageProfilesProcPtr = function(const (*var*) spec: FSSpec; var count: UInt32): CMError;
{$elsec}
	CountImageProfilesProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	GetIndImageProfileProcPtr = function(const (*var*) spec: FSSpec; index: UInt32; var prof: CMProfileRef): CMError;
{$elsec}
	GetIndImageProfileProcPtr = ProcPtr;
{$endc}

{$ifc TYPED_FUNCTION_POINTERS}
	SetIndImageProfileProcPtr = function(const (*var*) specFrom: FSSpec; const (*var*) specInto: FSSpec; index: UInt32; prof: CMProfileRef; embedFlags: UInt32): CMError;
{$elsec}
	SetIndImageProfileProcPtr = ProcPtr;
{$endc}

	{	*** CSScriptingLib API  ***	}

	{
	 *  CMValidImage()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in CSScriptingLib 2.6 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in 3.0 and later
	 	}
function CMValidImage(const (*var*) spec: FSSpec): CMError; external name '_CMValidImage';

{
 *  CMGetImageSpace()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in CSScriptingLib 2.6 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in 3.0 and later
 }
function CMGetImageSpace(const (*var*) spec: FSSpec; var space: OSType): CMError; external name '_CMGetImageSpace';

{
 *  CMEmbedImage()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in CSScriptingLib 2.6 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in 3.0 and later
 }
function CMEmbedImage(const (*var*) specFrom: FSSpec; const (*var*) specInto: FSSpec; repl: boolean; embProf: CMProfileRef): CMError; external name '_CMEmbedImage';

{
 *  CMUnembedImage()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in CSScriptingLib 2.6 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in 3.0 and later
 }
function CMUnembedImage(const (*var*) specFrom: FSSpec; const (*var*) specInto: FSSpec; repl: boolean): CMError; external name '_CMUnembedImage';

{
 *  CMMatchImage()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in CSScriptingLib 2.6 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in 3.0 and later
 }
function CMMatchImage(const (*var*) specFrom: FSSpec; const (*var*) specInto: FSSpec; repl: boolean; qual: UInt32; srcProf: CMProfileRef; srcIntent: UInt32; dstProf: CMProfileRef): CMError; external name '_CMMatchImage';

{
 *  CMProofImage()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in CSScriptingLib 2.6 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in 3.0 and later
 }
function CMProofImage(const (*var*) specFrom: FSSpec; const (*var*) specInto: FSSpec; repl: boolean; qual: UInt32; srcProf: CMProfileRef; srcIntent: UInt32; dstProf: CMProfileRef; prfProf: CMProfileRef): CMError; external name '_CMProofImage';

{
 *  CMLinkImage()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in CSScriptingLib 2.6 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in 3.0 and later
 }
function CMLinkImage(const (*var*) specFrom: FSSpec; const (*var*) specInto: FSSpec; repl: boolean; qual: UInt32; lnkProf: CMProfileRef; lnkIntent: UInt32): CMError; external name '_CMLinkImage';

{
 *  CMCountImageProfiles()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in CSScriptingLib 2.6 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in 3.0 and later
 }
function CMCountImageProfiles(const (*var*) spec: FSSpec; var count: UInt32): CMError; external name '_CMCountImageProfiles';

{
 *  CMGetIndImageProfile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in CSScriptingLib 2.6 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in 3.0 and later
 }
function CMGetIndImageProfile(const (*var*) spec: FSSpec; index: UInt32; var prof: CMProfileRef): CMError; external name '_CMGetIndImageProfile';

{
 *  CMSetIndImageProfile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in CSScriptingLib 2.6 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in 3.0 and later
 }
function CMSetIndImageProfile(const (*var*) specFrom: FSSpec; const (*var*) specInto: FSSpec; repl: boolean; index: UInt32; prof: CMProfileRef): CMError; external name '_CMSetIndImageProfile';

{$ALIGN MAC68K}


end.
