{
	Copyright (c) 2000-2003, Apple, Inc. All rights reserved.
}
{	  Pascal Translation:  Peter N Lewis, <peter@stairways.com.au>, 2004 }


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

unit MacOSXPosix;
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
uses MacTypes;
{$ALIGN MAC68K}

type
	{ sys/types }
	u_quad_t = UInt64;	{ quads }
	quad_t = SInt64;
	qaddr_t = ^quad_t;
	
	caddr_t = Ptr;	{ core address }
	daddr_t = SInt32;	{ disk address }
	dev_t = SInt32;		{ device number }
	fixpt_t = UInt32;	{ fixed point number }
	gid_t = UInt32;		{ group id }
	gid_t_ptr = ^gid_t;
	in_addr_t = UInt32;	{ base type for internet address }
	in_port_t = UInt16;
	ino_t = UInt32;		{ inode number }
	key_t = SInt32;		{ IPC key (for Sys V IPC) }
	mode_t = UInt16;		{ permissions }
	nlink_t = UInt16;	{ link count }
	off_t = quad_t;		{ file offset }
	pid_t = SInt32;		{ process id }
	rlim_t = quad_t;		{ resource limit }
	segsz_t = SInt32;	{ segment size }
	swblk_t = SInt32;	{ swap offset }
	uid_t = UInt32;		{ user id }
	uid_t_ptr = ^uid_t;
	useconds_t = UInt32;	{ microseconds (unsigned) }
	mach_port_name_t = UInt32; { represents a name of a port right }
	mach_port_t = UInt32; { reference added or deleted to a port right }

	type
		sockaddr = packed record
			sin_len: UInt8;
			sin_family: UInt8;
			sa_data: packed array[0..13] of UInt8;
		end;
		sockaddr_ptr = ^sockaddr;
		sockaddr_in = packed record
			sin_len: UInt8;
			sin_family: UInt8;
			sin_port: UInt16;
			sin_addr: UInt32;
			sin_zero: packed array[0..7] of UInt8;
		end;
		sockaddr_in_ptr = ^sockaddr_in;
	
end.
