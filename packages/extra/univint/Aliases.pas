{
     File:       Aliases.p
 
     Contains:   Alias Manager Interfaces.
 
     Version:    Technology: Mac OS 8.1
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  © 1989-2002 by Apple Computer, Inc., all rights reserved
 
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

unit Aliases;
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
uses MacTypes,Files;


{$ALIGN MAC68K}


const
	rAliasType					= $616C6973 (* 'alis' *);						{  Aliases are stored as resources of this type  }

																{  define alias resolution action rules mask  }
	kARMMountVol				= $00000001;					{  mount the volume automatically  }
	kARMNoUI					= $00000002;					{  no user interface allowed during resolution  }
	kARMMultVols				= $00000008;					{  search on multiple volumes  }
	kARMSearch					= $00000100;					{  search quickly  }
	kARMSearchMore				= $00000200;					{  search further  }
	kARMSearchRelFirst			= $00000400;					{  search target on a relative path first  }

																{  define alias record information types  }
	asiZoneName					= -3;							{  get zone name  }
	asiServerName				= -2;							{  get server name  }
	asiVolumeName				= -1;							{  get volume name  }
	asiAliasName				= 0;							{  get aliased file/folder/volume name  }
	asiParentName				= 1;							{  get parent folder name  }

	{	 ResolveAliasFileWithMountFlags options 	}
	kResolveAliasFileNoUI		= $00000001;					{  no user interaction during resolution  }

	{	 define the alias record that will be the blackbox for the caller 	}

type
	AliasRecordPtr = ^AliasRecord;
	AliasRecord = record
		userType:				OSType;									{  appl stored type like creator type  }
		aliasSize:				UInt16;									{  alias record size in bytes, for appl usage  }
	end;

	AliasPtr							= ^AliasRecord;
	AliasHandle							= ^AliasPtr;
	{	 alias record information type 	}
	AliasInfoType						= SInt16;
	{
	 *  NewAlias()
	 *  
	 *  Summary:
	 *    create a new alias between fromFile and target, returns alias
	 *    record handle
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function NewAlias(fromFile: {Const}FSSpecPtr; const (*var*) target: FSSpec; var alias: AliasHandle): OSErr; external name '_NewAlias';
{
 *  NewAliasMinimal()
 *  
 *  Summary:
 *    create a minimal new alias for a target and return alias record
 *    handle
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewAliasMinimal(const (*var*) target: FSSpec; var alias: AliasHandle): OSErr; external name '_NewAliasMinimal';
{
 *  NewAliasMinimalFromFullPath()
 *  
 *  Summary:
 *    create a minimal new alias from a target fullpath (optional zone
 *    and server name) and return alias record handle
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function NewAliasMinimalFromFullPath(fullPathLength: SInt16; fullPath: UnivPtr; const (*var*) zoneName: Str32; const (*var*) serverName: Str31; var alias: AliasHandle): OSErr; external name '_NewAliasMinimalFromFullPath';
{
 *  ResolveAlias()
 *  
 *  Summary:
 *    given an alias handle and fromFile, resolve the alias, update the
 *    alias record and return aliased filename and wasChanged flag.
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ResolveAlias(fromFile: {Const}FSSpecPtr; alias: AliasHandle; var target: FSSpec; var wasChanged: boolean): OSErr; external name '_ResolveAlias';
{
 *  GetAliasInfo()
 *  
 *  Summary:
 *    given an alias handle and an index specifying requested alias
 *    information type, return the information from alias record as a
 *    string.
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function GetAliasInfo(alias: AliasHandle; index: AliasInfoType; var theString: Str63): OSErr; external name '_GetAliasInfo';
{
 *  IsAliasFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function IsAliasFile(const (*var*) fileFSSpec: FSSpec; var aliasFileFlag: boolean; var folderFlag: boolean): OSErr; external name '_IsAliasFile';
{
 *  ResolveAliasWithMountFlags()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 8.5 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ResolveAliasWithMountFlags(fromFile: {Const}FSSpecPtr; alias: AliasHandle; var target: FSSpec; var wasChanged: boolean; mountFlags: UInt32): OSErr; external name '_ResolveAliasWithMountFlags';
{
 *  ResolveAliasFile()
 *  
 *  Summary:
 *    Given a file spec, return target file spec if input file spec is
 *    an alias. It resolves the entire alias chain or one step of the
 *    chain.  It returns info about whether the target is a folder or
 *    file; and whether the input file spec was an alias or not.
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ResolveAliasFile(var theSpec: FSSpec; resolveAliasChains: boolean; var targetIsFolder: boolean; var wasAliased: boolean): OSErr; external name '_ResolveAliasFile';
{
 *  ResolveAliasFileWithMountFlags()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ResolveAliasFileWithMountFlags(var theSpec: FSSpec; resolveAliasChains: boolean; var targetIsFolder: boolean; var wasAliased: boolean; mountFlags: UInt32): OSErr; external name '_ResolveAliasFileWithMountFlags';
{
 *  FollowFinderAlias()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FollowFinderAlias(fromFile: {Const}FSSpecPtr; alias: AliasHandle; logon: boolean; var target: FSSpec; var wasChanged: boolean): OSErr; external name '_FollowFinderAlias';
{ 
   Low Level Routines 
}
{
 *  UpdateAlias()
 *  
 *  Summary:
 *    given a fromFile-target pair and an alias handle, update the
 *    alias record pointed to by alias handle to represent target as
 *    the new alias.
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function UpdateAlias(fromFile: {Const}FSSpecPtr; const (*var*) target: FSSpec; alias: AliasHandle; var wasChanged: boolean): OSErr; external name '_UpdateAlias';
type
{$ifc TYPED_FUNCTION_POINTERS}
	AliasFilterProcPtr = function(cpbPtr: CInfoPBPtr; var quitFlag: boolean; myDataPtr: Ptr): boolean;
{$elsec}
	AliasFilterProcPtr = ProcPtr;
{$endc}

{$ifc OPAQUE_UPP_TYPES}
	AliasFilterUPP = ^SInt32; { an opaque UPP }
{$elsec}
	AliasFilterUPP = UniversalProcPtr;
{$endc}	

const
	uppAliasFilterProcInfo = $00000FD0;
	{
	 *  NewAliasFilterUPP()
	 *  
	 *  Availability:
	 *    Non-Carbon CFM:   available as macro/inline
	 *    CarbonLib:        in CarbonLib 1.0 and later
	 *    Mac OS X:         in version 10.0 and later
	 	}
function NewAliasFilterUPP(userRoutine: AliasFilterProcPtr): AliasFilterUPP; external name '_NewAliasFilterUPP'; { old name was NewAliasFilterProc }
{
 *  DisposeAliasFilterUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
procedure DisposeAliasFilterUPP(userUPP: AliasFilterUPP); external name '_DisposeAliasFilterUPP';
{
 *  InvokeAliasFilterUPP()
 *  
 *  Availability:
 *    Non-Carbon CFM:   available as macro/inline
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function InvokeAliasFilterUPP(cpbPtr: CInfoPBPtr; var quitFlag: boolean; myDataPtr: Ptr; userRoutine: AliasFilterUPP): boolean; external name '_InvokeAliasFilterUPP'; { old name was CallAliasFilterProc }
{
 *  MatchAlias()
 *  
 *  Summary:
 *    Given an alias handle and fromFile, match the alias and return
 *    aliased filename(s) and needsUpdate flag
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 7.1 and later
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function MatchAlias(fromFile: {Const}FSSpecPtr; rulesMask: UInt32; alias: AliasHandle; var aliasCount: SInt16; aliasList: FSSpecArrayPtr; var needsUpdate: boolean; aliasFilter: AliasFilterUPP; yourDataPtr: UnivPtr): OSErr; external name '_MatchAlias';
{
 *  ResolveAliasFileWithMountFlagsNoUI()
 *  
 *  Summary:
 *    variation on ResolveAliasFile that does not prompt user with a
 *    dialog
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function ResolveAliasFileWithMountFlagsNoUI(var theSpec: FSSpec; resolveAliasChains: boolean; var targetIsFolder: boolean; var wasAliased: boolean; mountFlags: UInt32): OSErr; external name '_ResolveAliasFileWithMountFlagsNoUI';

{
 *  MatchAliasNoUI()
 *  
 *  Summary:
 *    variation on MatchAlias that does not prompt user with a dialog
 *  
 *  Availability:
 *    Non-Carbon CFM:   not available
 *    CarbonLib:        in CarbonLib 1.0 and later
 *    Mac OS X:         in version 10.0 and later
 }
function MatchAliasNoUI(fromFile: {Const}FSSpecPtr; rulesMask: UInt32; alias: AliasHandle; var aliasCount: SInt16; aliasList: FSSpecArrayPtr; var needsUpdate: boolean; aliasFilter: AliasFilterUPP; yourDataPtr: UnivPtr): OSErr; external name '_MatchAliasNoUI';

{
 *  FSNewAlias()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.1 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FSNewAlias(fromFile: {Const}FSRefPtr; const (*var*) target: FSRef; var inAlias: AliasHandle): OSErr; external name '_FSNewAlias';
{
 *  FSNewAliasMinimal()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.1 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FSNewAliasMinimal(const (*var*) target: FSRef; var inAlias: AliasHandle): OSErr; external name '_FSNewAliasMinimal';
{
 *  FSIsAliasFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.1 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FSIsAliasFile(const (*var*) fileRef: FSRef; var aliasFileFlag: boolean; var folderFlag: boolean): OSErr; external name '_FSIsAliasFile';
{
 *  FSResolveAliasWithMountFlags()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.1 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FSResolveAliasWithMountFlags(fromFile: {Const}FSRefPtr; inAlias: AliasHandle; var target: FSRef; var wasChanged: boolean; mountFlags: UInt32): OSErr; external name '_FSResolveAliasWithMountFlags';
{
 *  FSResolveAlias()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.1 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FSResolveAlias(fromFile: {Const}FSRefPtr; alias: AliasHandle; var target: FSRef; var wasChanged: boolean): OSErr; external name '_FSResolveAlias';
{
 *  FSResolveAliasFileWithMountFlags()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.1 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FSResolveAliasFileWithMountFlags(var theRef: FSRef; resolveAliasChains: boolean; var targetIsFolder: boolean; var wasAliased: boolean; mountFlags: UInt32): OSErr; external name '_FSResolveAliasFileWithMountFlags';
{
 *  FSResolveAliasFile()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.1 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FSResolveAliasFile(var theRef: FSRef; resolveAliasChains: boolean; var targetIsFolder: boolean; var wasAliased: boolean): OSErr; external name '_FSResolveAliasFile';
{
 *  FSFollowFinderAlias()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.1 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FSFollowFinderAlias(fromFile: FSRefPtr; alias: AliasHandle; logon: boolean; var target: FSRef; var wasChanged: boolean): OSErr; external name '_FSFollowFinderAlias';
{
 *  FSUpdateAlias()
 *  
 *  Availability:
 *    Non-Carbon CFM:   in InterfaceLib 9.1 and later
 *    CarbonLib:        in CarbonLib 1.1 and later
 *    Mac OS X:         in version 10.0 and later
 }
function FSUpdateAlias(fromFile: {Const}FSRefPtr; const (*var*) target: FSRef; alias: AliasHandle; var wasChanged: boolean): OSErr; external name '_FSUpdateAlias';
{$ALIGN MAC68K}


end.
