{
 * Copyright (c) 2004, 2010 Apple Inc. All rights reserved.
 *
 * @APPLE_LICENSE_HEADER_START@
 * 
 * The contents of this file constitute Original Code as defined in and
 * are subject to the Apple Public Source License Version 1.1 (the
 * "License").  You may not use this file except in compliance with the
 * License.  Please obtain a copy of the License at
 * http://www.apple.com/publicsource and read it before using this file.
 * 
 * This Original Code and all software distributed under the License are
 * distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, EITHER
 * EXPRESS OR IMPLIED, AND APPLE HEREBY DISCLAIMS ALL SUCH WARRANTIES,
 * INCLUDING WITHOUT LIMITATION, ANY WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE OR NON-INFRINGEMENT.  Please see the
 * License for the specific language governing rights and limitations
 * under the License.
 * 
 * @APPLE_LICENSE_HEADER_END@
 }
{
    Modified for use with Free Pascal
    Version 308
    Please report any bugs to <gpc@microbizz.nl>
}

{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}
{$mode macpas}
{$modeswitch cblocks}
{$packenum 1}
{$macro on}
{$inline on}
{$calling mwpascal}

unit acl;
interface
{$setc UNIVERSAL_INTERFACES_VERSION := $0400}
{$setc GAP_INTERFACES_VERSION := $0308}

{$ifc not defined USE_CFSTR_CONSTANT_MACROS}
    {$setc USE_CFSTR_CONSTANT_MACROS := TRUE}
{$endc}

{$ifc defined CPUPOWERPC and defined CPUI386}
	{$error Conflicting initial definitions for CPUPOWERPC and CPUI386}
{$endc}
{$ifc defined FPC_BIG_ENDIAN and defined FPC_LITTLE_ENDIAN}
	{$error Conflicting initial definitions for FPC_BIG_ENDIAN and FPC_LITTLE_ENDIAN}
{$endc}

{$ifc not defined __ppc__ and defined CPUPOWERPC32}
	{$setc __ppc__ := 1}
{$elsec}
	{$setc __ppc__ := 0}
{$endc}
{$ifc not defined __ppc64__ and defined CPUPOWERPC64}
	{$setc __ppc64__ := 1}
{$elsec}
	{$setc __ppc64__ := 0}
{$endc}
{$ifc not defined __i386__ and defined CPUI386}
	{$setc __i386__ := 1}
{$elsec}
	{$setc __i386__ := 0}
{$endc}
{$ifc not defined __x86_64__ and defined CPUX86_64}
	{$setc __x86_64__ := 1}
{$elsec}
	{$setc __x86_64__ := 0}
{$endc}
{$ifc not defined __arm__ and defined CPUARM}
	{$setc __arm__ := 1}
{$elsec}
	{$setc __arm__ := 0}
{$endc}
{$ifc not defined __arm64__ and defined CPUAARCH64}
  {$setc __arm64__ := 1}
{$elsec}
  {$setc __arm64__ := 0}
{$endc}

{$ifc defined cpu64}
  {$setc __LP64__ := 1}
{$elsec}
  {$setc __LP64__ := 0}
{$endc}


{$ifc defined __ppc__ and __ppc__ and defined __i386__ and __i386__}
	{$error Conflicting definitions for __ppc__ and __i386__}
{$endc}

{$ifc defined __ppc__ and __ppc__}
	{$setc TARGET_CPU_PPC := TRUE}
	{$setc TARGET_CPU_PPC64 := FALSE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := FALSE}
	{$setc TARGET_CPU_ARM64 := FALSE}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
	{$setc TARGET_OS_EMBEDDED := FALSE}
{$elifc defined __ppc64__ and __ppc64__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := TRUE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := FALSE}
	{$setc TARGET_CPU_ARM64 := FALSE}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
	{$setc TARGET_OS_EMBEDDED := FALSE}
{$elifc defined __i386__ and __i386__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := FALSE}
	{$setc TARGET_CPU_X86 := TRUE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := FALSE}
	{$setc TARGET_CPU_ARM64 := FALSE}
{$ifc defined iphonesim}
 	{$setc TARGET_OS_MAC := FALSE}
	{$setc TARGET_OS_IPHONE := TRUE}
	{$setc TARGET_IPHONE_SIMULATOR := TRUE}
{$elsec}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
{$endc}
	{$setc TARGET_OS_EMBEDDED := FALSE}
{$elifc defined __x86_64__ and __x86_64__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := FALSE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := TRUE}
	{$setc TARGET_CPU_ARM := FALSE}
	{$setc TARGET_CPU_ARM64 := FALSE}
{$ifc defined iphonesim}
 	{$setc TARGET_OS_MAC := FALSE}
	{$setc TARGET_OS_IPHONE := TRUE}
	{$setc TARGET_IPHONE_SIMULATOR := TRUE}
{$elsec}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
{$endc}
	{$setc TARGET_OS_EMBEDDED := FALSE}
{$elifc defined __arm__ and __arm__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := FALSE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := TRUE}
	{$setc TARGET_CPU_ARM64 := FALSE}
	{$setc TARGET_OS_MAC := FALSE}
	{$setc TARGET_OS_IPHONE := TRUE}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
	{$setc TARGET_OS_EMBEDDED := TRUE}
{$elifc defined __arm64__ and __arm64__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_PPC64 := FALSE}
	{$setc TARGET_CPU_X86 := FALSE}
	{$setc TARGET_CPU_X86_64 := FALSE}
	{$setc TARGET_CPU_ARM := FALSE}
	{$setc TARGET_CPU_ARM64 := TRUE}
{$ifc defined ios}
	{$setc TARGET_OS_MAC := FALSE}
	{$setc TARGET_OS_IPHONE := TRUE}
	{$setc TARGET_OS_EMBEDDED := TRUE}
{$elsec}
	{$setc TARGET_OS_MAC := TRUE}
	{$setc TARGET_OS_IPHONE := FALSE}
	{$setc TARGET_OS_EMBEDDED := FALSE}
{$endc}
	{$setc TARGET_IPHONE_SIMULATOR := FALSE}
{$elsec}
	{$error __ppc__ nor __ppc64__ nor __i386__ nor __x86_64__ nor __arm__ nor __arm64__ is defined.}
{$endc}

{$ifc defined __LP64__ and __LP64__ }
  {$setc TARGET_CPU_64 := TRUE}
{$elsec}
  {$setc TARGET_CPU_64 := FALSE}
{$endc}

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
{$setc TARGET_OS_UNIX := FALSE}
{$setc TARGET_OS_WIN32 := FALSE}
{$setc TARGET_RT_MAC_68881 := FALSE}
{$setc TARGET_RT_MAC_CFM := FALSE}
{$setc TARGET_RT_MAC_MACHO := TRUE}
{$setc TYPED_FUNCTION_POINTERS := TRUE}
{$setc TYPE_BOOL := FALSE}
{$setc TYPE_EXTENDED := FALSE}
{$setc TYPE_LONGLONG := TRUE}
uses MacTypes,MacOSXPosix;
{$endc} {not MACOSALLINCLUDE}

{$ALIGN POWER}


const __DARWIN_ACL_READ_DATA = 1 shl 1;
const __DARWIN_ACL_LIST_DIRECTORY = __DARWIN_ACL_READ_DATA;
const __DARWIN_ACL_WRITE_DATA = 1 shl 2;
const __DARWIN_ACL_ADD_FILE = __DARWIN_ACL_WRITE_DATA;
const __DARWIN_ACL_EXECUTE = 1 shl 3;
const __DARWIN_ACL_SEARCH = __DARWIN_ACL_EXECUTE;
const __DARWIN_ACL_DELETE = 1 shl 4;
const __DARWIN_ACL_APPEND_DATA = 1 shl 5;
const __DARWIN_ACL_ADD_SUBDIRECTORY = __DARWIN_ACL_APPEND_DATA;
const __DARWIN_ACL_DELETE_CHILD = 1 shl 6;
const __DARWIN_ACL_READ_ATTRIBUTES = 1 shl 7;
const __DARWIN_ACL_WRITE_ATTRIBUTES = 1 shl 8;
const __DARWIN_ACL_READ_EXTATTRIBUTES = 1 shl 9;
const __DARWIN_ACL_WRITE_EXTATTRIBUTES = 1 shl 10;
const __DARWIN_ACL_READ_SECURITY = 1 shl 11;
const __DARWIN_ACL_WRITE_SECURITY = 1 shl 12;
const __DARWIN_ACL_CHANGE_OWNER = 1 shl 13;

const
	__DARWIN_ACL_EXTENDED_ALLOW = 1;
const
	__DARWIN_ACL_EXTENDED_DENY = 2;

const __DARWIN_ACL_ENTRY_INHERITED = 1 shl 4;
const __DARWIN_ACL_ENTRY_FILE_INHERIT = 1 shl 5;
const __DARWIN_ACL_ENTRY_DIRECTORY_INHERIT = 1 shl 6;
const __DARWIN_ACL_ENTRY_LIMIT_INHERIT = 1 shl 7;
const __DARWIN_ACL_ENTRY_ONLY_INHERIT = 1 shl 8;
const __DARWIN_ACL_FLAG_NO_INHERIT = 1 shl 17;

{
 * Implementation constants.
 *
 * The ACL_TYPE_EXTENDED binary format permits 169 entries plus
 * the ACL header in a page.  Give ourselves some room to grow;
 * this limit is arbitrary.
 }
const
	ACL_MAX_ENTRIES = 128;

{ 23.2.2 Individual object access permissions - nonstandard }
type
  acl_perm_t = SInt32;
const
	ACL_READ_DATA		= __DARWIN_ACL_READ_DATA;
	ACL_LIST_DIRECTORY	= __DARWIN_ACL_LIST_DIRECTORY;
	ACL_WRITE_DATA		= __DARWIN_ACL_WRITE_DATA;
	ACL_ADD_FILE		= __DARWIN_ACL_ADD_FILE;
	ACL_EXECUTE		= __DARWIN_ACL_EXECUTE;
	ACL_SEARCH		= __DARWIN_ACL_SEARCH;
	ACL_DELETE		= __DARWIN_ACL_DELETE;
	ACL_APPEND_DATA		= __DARWIN_ACL_APPEND_DATA;
	ACL_ADD_SUBDIRECTORY	= __DARWIN_ACL_ADD_SUBDIRECTORY;
	ACL_DELETE_CHILD	= __DARWIN_ACL_DELETE_CHILD;
	ACL_READ_ATTRIBUTES	= __DARWIN_ACL_READ_ATTRIBUTES;
	ACL_WRITE_ATTRIBUTES	= __DARWIN_ACL_WRITE_ATTRIBUTES;
	ACL_READ_EXTATTRIBUTES	= __DARWIN_ACL_READ_EXTATTRIBUTES;
	ACL_WRITE_EXTATTRIBUTES	= __DARWIN_ACL_WRITE_EXTATTRIBUTES;
	ACL_READ_SECURITY	= __DARWIN_ACL_READ_SECURITY;
	ACL_WRITE_SECURITY	= __DARWIN_ACL_WRITE_SECURITY;
	ACL_CHANGE_OWNER	= __DARWIN_ACL_CHANGE_OWNER;


{ 23.2.5 ACL entry tag type bits - nonstandard }
type
  acl_tag_t = SInt32;
const
	ACL_UNDEFINED_TAG	= 0;
	ACL_EXTENDED_ALLOW	= __DARWIN_ACL_EXTENDED_ALLOW;
	ACL_EXTENDED_DENY	= __DARWIN_ACL_EXTENDED_DENY;

{ 23.2.6 Individual ACL types }
type
  acl_type_t = SInt32;
const
	ACL_TYPE_EXTENDED	= $00000100;
{ Posix 1003.1e types - not supported }
	ACL_TYPE_ACCESS         = $00000000;
	ACL_TYPE_DEFAULT        = $00000001;
{ The following types are defined on FreeBSD/Linux - not supported }
	ACL_TYPE_AFS            = $00000002;
	ACL_TYPE_CODA           = $00000003;
	ACL_TYPE_NTFS           = $00000004;
	ACL_TYPE_NWFS           = $00000005;

{ 23.2.7 ACL qualifier constants }

const ACL_UNDEFINED_ID = nil;	{ XXX ? }

{ 23.2.8 ACL Entry Constants }
type
  acl_entry_id_t = SInt32;
const
	ACL_FIRST_ENTRY		= 0;
	ACL_NEXT_ENTRY		= -1;
	ACL_LAST_ENTRY		= -2;

{ nonstandard ACL / entry flags }
type
  acl_flag_t = SInt32;
const
	ACL_FLAG_DEFER_INHERIT		= (1  shl  0);	{ tentative }
	ACL_FLAG_NO_INHERIT		= __DARWIN_ACL_FLAG_NO_INHERIT;
	ACL_ENTRY_INHERITED		= __DARWIN_ACL_ENTRY_INHERITED;
	ACL_ENTRY_FILE_INHERIT		= __DARWIN_ACL_ENTRY_FILE_INHERIT;
	ACL_ENTRY_DIRECTORY_INHERIT	= __DARWIN_ACL_ENTRY_DIRECTORY_INHERIT;
	ACL_ENTRY_LIMIT_INHERIT		= __DARWIN_ACL_ENTRY_LIMIT_INHERIT;
	ACL_ENTRY_ONLY_INHERIT		= __DARWIN_ACL_ENTRY_ONLY_INHERIT;

{ "External" ACL types }
type
	acl_t = ^_acl; { an opaque type }
	_acl = record end;
	acl_entry_t = ^_acl_entry; { an opaque type }
	_acl_entry = record end;
	acl_permset_t = ^_acl_permset; { an opaque type }
	_acl_permset = record end;
	acl_flagset_t = ^_acl_flagset; { an opaque type }
	_acl_flagset = record end;

type
	acl_permset_mask_t = UInt64;

{ 23.1.6.1 ACL Storage Management }
function acl_dup( acl: acl_t ): acl_t; external name '_acl_dup';
function acl_free( obj_p: UnivPtr ): SInt32; external name '_acl_free';
function acl_init( count: SInt32 ): acl_t; external name '_acl_init';

{ 23.1.6.2 (1) ACL Entry manipulation }
function acl_copy_entry( dest_d: acl_entry_t; src_d: acl_entry_t ): SInt32; external name '_acl_copy_entry';
function acl_create_entry( var acl_p: acl_t; var entry_p: acl_entry_t ): SInt32; external name '_acl_create_entry';
function acl_create_entry_np( var acl_p: acl_t; var entry_p: acl_entry_t; entry_index: SInt32 ): SInt32; external name '_acl_create_entry_np';
function acl_delete_entry( acl: acl_t; entry_d: acl_entry_t ): SInt32; external name '_acl_delete_entry';
function acl_get_entry( acl: acl_t; entry_id: SInt32; var entry_p: acl_entry_t ): SInt32; external name '_acl_get_entry';
function acl_valid( acl: acl_t ): SInt32; external name '_acl_valid';
function acl_valid_fd_np( fd: SInt32; typ: acl_type_t; acl: acl_t ): SInt32; external name '_acl_valid_fd_np';
function acl_valid_file_np( path: ConstCStringPtr; typ: acl_type_t; acl: acl_t ): SInt32; external name '_acl_valid_file_np';
function acl_valid_link_np( path: ConstCStringPtr; typ: acl_type_t; acl: acl_t ): SInt32; external name '_acl_valid_link_np';

{ 23.1.6.2 (2) Manipulate permissions within an ACL entry }
function acl_add_perm( permset_d: acl_permset_t; perm: acl_perm_t ): SInt32; external name '_acl_add_perm';
function acl_calc_mask( var acl_p: acl_t ): SInt32; external name '_acl_calc_mask';	{ not supported }
function acl_clear_perms( permset_d: acl_permset_t ): SInt32; external name '_acl_clear_perms';
function acl_delete_perm( permset_d: acl_permset_t; perm: acl_perm_t ): SInt32; external name '_acl_delete_perm';
function acl_get_perm_np( permset_d: acl_permset_t; perm: acl_perm_t ): SInt32; external name '_acl_get_perm_np';
function acl_get_permset( entry_d: acl_entry_t; var permset_p: acl_permset_t ): SInt32; external name '_acl_get_permset';
function acl_set_permset( entry_d: acl_entry_t; permset_d: acl_permset_t ): SInt32; external name '_acl_set_permset';

{ nonstandard - manipulate permissions within an ACL entry using bitmasks }
function acl_maximal_permset_mask_np( var mask_p: acl_permset_mask_t ): SInt32; external name '_acl_maximal_permset_mask_np';
(* __OSX_AVAILABLE_STARTING(__MAC_10_7, __IPHONE_4_3) *)
function acl_get_permset_mask_np( entry_d: acl_entry_t; var mask_p: acl_permset_mask_t ): SInt32; external name '_acl_get_permset_mask_np';
(* __OSX_AVAILABLE_STARTING(__MAC_10_7, __IPHONE_4_3) *)
function acl_set_permset_mask_np( entry_d: acl_entry_t; mask: acl_permset_mask_t ): SInt32; external name '_acl_set_permset_mask_np';
(* __OSX_AVAILABLE_STARTING(__MAC_10_7, __IPHONE_4_3) *)

{ nonstandard - manipulate flags on ACLs and entries }
function acl_add_flag_np( flagset_d: acl_flagset_t; flag: acl_flag_t ): SInt32; external name '_acl_add_flag_np';
function acl_clear_flags_np( flagset_d: acl_flagset_t ): SInt32; external name '_acl_clear_flags_np';
function acl_delete_flag_np( flagset_d: acl_flagset_t; flag: acl_flag_t ): SInt32; external name '_acl_delete_flag_np';
function acl_get_flag_np( flagset_d: acl_flagset_t; flag: acl_flag_t ): SInt32; external name '_acl_get_flag_np';
function acl_get_flagset_np( obj_p: UnivPtr; var flagset_p: acl_flagset_t ): SInt32; external name '_acl_get_flagset_np';
function acl_set_flagset_np( obj_p: UnivPtr; flagset_d: acl_flagset_t ): SInt32; external name '_acl_set_flagset_np';

{ 23.1.6.2 (3) Manipulate ACL entry tag type and qualifier }
function acl_get_qualifier( entry_d: acl_entry_t ): UnivPtr; external name '_acl_get_qualifier';
function acl_get_tag_type( entry_d: acl_entry_t; var tag_type_p: acl_tag_t ): SInt32; external name '_acl_get_tag_type';
function acl_set_qualifier( entry_d: acl_entry_t; tag_qualifier_p: {const} UnivPtr ): SInt32; external name '_acl_set_qualifier';
function acl_set_tag_type( entry_d: acl_entry_t; tag_type: acl_tag_t ): SInt32; external name '_acl_set_tag_type';

{ 23.1.6.3 ACL manipulation on an Object }
function acl_delete_def_file( path_p: ConstCStringPtr ): SInt32; external name '_acl_delete_def_file'; { not supported }
function acl_get_fd( fd: SInt32 ): acl_t; external name '_acl_get_fd';
function acl_get_fd_np( fd: SInt32; typ: acl_type_t ): acl_t; external name '_acl_get_fd_np';
function acl_get_file( path_p: ConstCStringPtr; typ: acl_type_t ): acl_t; external name '_acl_get_file';
function acl_get_link_np( path_p: ConstCStringPtr; typ: acl_type_t ): acl_t; external name '_acl_get_link_np';
function acl_set_fd( fd: SInt32; acl: acl_t ): SInt32; external name '_acl_set_fd';
function acl_set_fd_np( fd: SInt32; acl: acl_t; acl_type: acl_type_t ): SInt32; external name '_acl_set_fd_np';
function acl_set_file( path_p: ConstCStringPtr; typ: acl_type_t; acl: acl_t ): SInt32; external name '_acl_set_file';
function acl_set_link_np( path_p: ConstCStringPtr; typ: acl_type_t; acl: acl_t ): SInt32; external name '_acl_set_link_np';

{ 23.1.6.4 ACL Format translation }
function acl_copy_ext( buf_p: UnivPtr; acl: acl_t; size: ssize_t ): ssize_t; external name '_acl_copy_ext';
function acl_copy_ext_native( buf_p: UnivPtr; acl: acl_t; size: ssize_t ): ssize_t; external name '_acl_copy_ext_native';
function acl_copy_int( buf_p: {const} UnivPtr ): acl_t; external name '_acl_copy_int';
function acl_copy_int_native( buf_p: {const} UnivPtr ): acl_t; external name '_acl_copy_int_native';
function acl_from_text( buf_p: ConstCStringPtr ): acl_t; external name '_acl_from_text';
function acl_size( acl: acl_t ): ssize_t; external name '_acl_size';
function acl_to_text( acl: acl_t; var len_p: ssize_t ): CStringPtr; external name '_acl_to_text';
{$ifc not defined MACOSALLINCLUDE or not MACOSALLINCLUDE}

end.
{$endc} {not MACOSALLINCLUDE}
