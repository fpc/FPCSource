{ Copyright 2000-2005 The Apache Software Foundation or its licensors, as
 * applicable.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 }

{
 * @file apr_file_info.h
 * @brief APR File Information
 }

{#include "apr.h"
#include "apr_user.h"
#include "apr_pools.h"
#include "apr_tables.h"
#include "apr_time.h"
#include "apr_errno.h"

#if APR_HAVE_SYS_UIO_H
#include <sys/uio.h>
#endif}

{
 * @defgroup apr_file_info File Information
 * @ingroup APR 
 }

{ Many applications use the type member to determine the
 * existance of a file or initialization of the file info,
 * so the APR_NOFILE value must be distinct from APR_UNKFILE.
 }

{ apr_filetype_e values for the filetype member of the
 * apr_file_info_t structure
 * @warning: Not all of the filetypes below can be determined.
 * For example, a given platform might not correctly report 
 * a socket descriptor as APR_SOCK if that type isn't 
 * well-identified on that platform.  In such cases where
 * a filetype exists but cannot be described by the recognized
 * flags below, the filetype will be APR_UNKFILE.  If the
 * filetype member is not determined, the type will be APR_NOFILE.
 }

type
  apr_filetype_e = (
    APR_NOFILE = 0,     {< no file type determined }
    APR_REG,            {< a regular file }
    APR_DIR,            {< a directory }
    APR_CHR,            {< a character device }
    APR_BLK,            {< a block device }
    APR_PIPE,           {< a FIFO / pipe }
    APR_LNK,            {< a symbolic link }
    APR_SOCK,           {< a [unix domain] socket }
    APR_UNKFILE = 127   {< a file of some other unknown type }
  );

{
 * @defgroup apr_file_permissions File Permissions flags 
 * @
 }

const
  APR_FPROT_USETID     = $8000; {< Set user id }
  APR_FPROT_UREAD      = $0400; {< Read by user }
  APR_FPROT_UWRITE     = $0200; {< Write by user }
  APR_FPROT_UEXECUTE   = $0100; {< Execute by user }

  APR_FPROT_GSETID     = $4000; {< Set group id }
  APR_FPROT_GREAD      = $0040; {< Read by group }
  APR_FPROT_GWRITE     = $0020; {< Write by group }
  APR_FPROT_GEXECUTE   = $0010; {< Execute by group }

  APR_FPROT_WSTICKY    = $2000; {< Sticky bit }
  APR_FPROT_WREAD      = $0004; {< Read by others }
  APR_FPROT_WWRITE     = $0002; {< Write by others }
  APR_FPROT_WEXECUTE   = $0001; {< Execute by others }

  APR_FPROT_OS_DEFAULT = $0FFF; {< use OS's default permissions }

{ additional permission flags for apr_file_copy  and apr_file_append }
  APR_FPROT_FILE_SOURCE_PERMS = $1000; {< Copy source file's permissions }

{ backcompat }
  APR_USETID    = APR_FPROT_USETID;     {< @deprecated @see APR_FPROT_USETID     }
  APR_UREAD     = APR_FPROT_UREAD;      {< @deprecated @see APR_FPROT_UREAD      }
  APR_UWRITE    = APR_FPROT_UWRITE;     {< @deprecated @see APR_FPROT_UWRITE     }
  APR_UEXECUTE  = APR_FPROT_UEXECUTE;   {< @deprecated @see APR_FPROT_UEXECUTE   }
  APR_GSETID    = APR_FPROT_GSETID;     {< @deprecated @see APR_FPROT_GSETID     }
  APR_GREAD     = APR_FPROT_GREAD;      {< @deprecated @see APR_FPROT_GREAD      }
  APR_GWRITE    = APR_FPROT_GWRITE;     {< @deprecated @see APR_FPROT_GWRITE     }
  APR_GEXECUTE  = APR_FPROT_GEXECUTE;   {< @deprecated @see APR_FPROT_GEXECUTE   }
  APR_WSTICKY   = APR_FPROT_WSTICKY;    {< @deprecated @see APR_FPROT_WSTICKY    }
  APR_WREAD     = APR_FPROT_WREAD;      {< @deprecated @see APR_FPROT_WREAD      }
  APR_WWRITE    = APR_FPROT_WWRITE;     {< @deprecated @see APR_FPROT_WWRITE     }
  APR_WEXECUTE  = APR_FPROT_WEXECUTE;   {< @deprecated @see APR_FPROT_WEXECUTE   }
  APR_OS_DEFAULT= APR_FPROT_OS_DEFAULT; {< @deprecated @see APR_FPROT_OS_DEFAULT }
  APR_FILE_SOURCE_PERMS = APR_FPROT_FILE_SOURCE_PERMS; {< @deprecated @see APR_FPROT_FILE_SOURCE_PERMS }

{
 * Structure for referencing directories.
 }
type
  apr_dir_t = record end;
  Papr_dir_t = ^apr_dir_t;
  PPapr_dir_t = ^Papr_dir_t;
  
{
 * Structure for determining file permissions.
 }
  apr_fileperms_t = apr_int32_t;

{$if defined(WINDOWS) or defined(NETWARE)}
{
 * Structure for determining the inode of the file.
 }
  apr_ino_t = apr_uint64_t;
{
 * Structure for determining the device the file is on.
 }
  apr_dev_t = apr_uint32_t;
{$else}
{ The inode of the file. }
  apr_ino_t = ino_t;
{
 * Structure for determining the device the file is on.
 }
  apr_dev_t = dev_t;
{$endif}

{
 * @defgroup apr_file_stat Stat Functions
 * @
 }

const
  APR_FINFO_LINK =  $00000001; {< Stat the link not the file itself if it is a link }
  APR_FINFO_MTIME = $00000010; {< Modification Time }
  APR_FINFO_CTIME = $00000020; {< Creation Time }
  APR_FINFO_ATIME = $00000040; {< Access Time }
  APR_FINFO_SIZE =  $00000100; {< Size of the file }
  APR_FINFO_CSIZE = $00000200; {< Storage size consumed by the file }
  APR_FINFO_DEV =   $00001000; {< Device }
  APR_FINFO_INODE = $00002000; {< Inode }
  APR_FINFO_NLINK = $00004000; {< Number of links }
  APR_FINFO_TYPE =  $00008000; {< Type }
  APR_FINFO_USER =  $00010000; {< User }
  APR_FINFO_GROUP = $00020000; {< Group }
  APR_FINFO_UPROT = $00100000; {< User protection bits }
  APR_FINFO_GPROT = $00200000; {< Group protection bits }
  APR_FINFO_WPROT = $00400000; {< World protection bits }
  APR_FINFO_ICASE = $01000000; {< if dev is case insensitive }
  APR_FINFO_NAME =  $02000000; {< ->name in proper case }

  APR_FINFO_MIN =   $00008170; {< type, mtime, ctime, atime, size }
  APR_FINFO_IDENT = $00003000; {< dev and inode }
  APR_FINFO_OWNER = $00030000; {< user and group }
  APR_FINFO_PROT =  $00700000; {<  all protections }
  APR_FINFO_NORM =  $0073b170; {<  an atomic unix apr_stat() }
  APR_FINFO_DIRENT = $02000000; {<  an atomic unix apr_dir_read() }

{
 * The file information structure.  This is analogous to the POSIX
 * stat structure.
 }
type
  apr_finfo_t = record
    { Allocates memory and closes lingering handles in the specified pool }
    pool: Papr_pool_t;
    { The bitmask describing valid fields of this apr_finfo_t structure
     *  including all available 'wanted' fields and potentially more }
    valid: apr_int32_t;
    { The access permissions of the file.  Mimics Unix access rights. }
    protection: apr_fileperms_t;
    { The type of file.  One of APR_REG, APR_DIR, APR_CHR, APR_BLK, APR_PIPE,
     * APR_LNK or APR_SOCK.  If the type is undetermined, the value is APR_NOFILE.
     * If the type cannot be determined, the value is APR_UNKFILE.
     }
    filetype: apr_filetype_e;
    { The user id that owns the file }
    user: apr_uid_t;
    { The group id that owns the file }
    group: apr_gid_t;
    { The inode of the file. }
    inode: apr_ino_t;
    { The id of the device the file is on. }
    device: apr_dev_t;
    { The number of hard links to the file. }
    nlink: apr_int32_t;
    { The size of the file }
    size: apr_off_t;
    { The storage size consumed by the file }
    csize: apr_off_t;
    { The time the file was last accessed }
    atime: apr_time_t;
    { The time the file was last modified }
    mtime: apr_time_t;
    { The time the file was last changed }
    ctime: apr_time_t;
    { The pathname of the file (possibly unrooted) }
    fname: PChar;
    { The file's name (no path) in filesystem case }
    name: PChar;
    { The file's handle, if accessed (can be submitted to apr_duphandle) }
    filehand: Papr_file_t;
  end;
  
  Papr_finfo_t = ^apr_finfo_t;

{
 * get the specified file's stats.  The file is specified by filename, 
 * instead of using a pre-opened file.
 * @param finfo Where to store the information about the file, which is
 * never touched if the call fails.
 * @param fname The name of the file to stat.
 * @param wanted The desired apr_finfo_t fields, as a bit flag of APR_FINFO_
                 values 
 * @param pool the pool to use to allocate the new file.
 *
 * @note If @c APR_INCOMPLETE is returned all the fields in @a finfo may
 *       not be filled in, and you need to check the @c finfo->valid bitmask
 *       to verify that what you're looking for is there.
 }
function apr_stat(finfo: Papr_finfo_t; const fname: PChar;
 wanted: apr_int32_t; pool: Papr_pool_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_stat' + LibSuff16;

{
 * @defgroup apr_dir Directory Manipulation Functions
 }

{
 * Open the specified directory.
 * @param new_dir The opened directory descriptor.
 * @param dirname The full path to the directory (use / on all systems)
 * @param pool The pool to use.
 }
function apr_dir_open(new_dir: PPapr_dir_t; const dirname: PChar;
 pool: Papr_pool_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_dir_open' + LibSuff12;

{
 * close the specified directory. 
 * @param thedir the directory descriptor to close.
 }
function apr_dir_close(thedir: Papr_dir_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_dir_close' + LibSuff4;

{
 * Read the next entry from the specified directory. 
 * @param finfo the file info structure and filled in by apr_dir_read
 * @param wanted The desired apr_finfo_t fields, as a bit flag of APR_FINFO_
                 values 
 * @param thedir the directory descriptor returned from apr_dir_open
 * @remark No ordering is guaranteed for the entries read.
 *
 * @note If @c APR_INCOMPLETE is returned all the fields in @a finfo may
 *       not be filled in, and you need to check the @c finfo->valid bitmask
 *       to verify that what you're looking for is there.
 }
function apr_dir_read(finfo: Papr_finfo_t; wanted: apr_int32_t;
 thedir: Papr_dir_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_dir_read' + LibSuff12;

{
 * Rewind the directory to the first entry.
 * @param thedir the directory descriptor to rewind.
 }
function apr_dir_rewind(thedir: Papr_dir_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_dir_rewind' + LibSuff4;

{
 * @defgroup apr_filepath Filepath Manipulation Functions
 }

const
  { Cause apr_filepath_merge to fail if addpath is above rootpath }
  APR_FILEPATH_NOTABOVEROOT =  $01;

{ internal: Only meaningful with APR_FILEPATH_NOTABOVEROOT }
  APR_FILEPATH_SECUREROOTTEST =$02;

{ Cause apr_filepath_merge to fail if addpath is above rootpath,
 * even given a rootpath /foo/bar and an addpath ../bar/bash
 }
  APR_FILEPATH_SECUREROOT =    $03;

{ Fail apr_filepath_merge if the merged path is relative }
  APR_FILEPATH_NOTRELATIVE =   $04;

{ Fail apr_filepath_merge if the merged path is absolute }
  APR_FILEPATH_NOTABSOLUTE =   $08;

{ Return the file system's native path format (e.g. path delimiters
 * of ':' on MacOS9, '\' on Win32, etc.) }
  APR_FILEPATH_NATIVE =        $10;

{ Resolve the true case of existing directories and file elements
 * of addpath, (resolving any aliases on Win32) and append a proper 
 * trailing slash if a directory
 }
  APR_FILEPATH_TRUENAME =      $20;

{
 * Extract the rootpath from the given filepath
 * @param rootpath the root file path returned with APR_SUCCESS or APR_EINCOMPLETE
 * @param filepath the pathname to parse for its root component
 * @param flags the desired rules to apply, from
 * <PRE>
 *      APR_FILEPATH_NATIVE    Use native path seperators (e.g. '\' on Win32)
 *      APR_FILEPATH_TRUENAME  Tests that the root exists, and makes it proper
 * </PRE>
 * @param p the pool to allocate the new path string from
 * @remark on return, filepath points to the first non-root character in the
 * given filepath.  In the simplest example, given a filepath of "/foo", 
 * returns the rootpath of "/" and filepath points at "foo".  This is far 
 * more complex on other platforms, which will canonicalize the root form
 * to a consistant format, given the APR_FILEPATH_TRUENAME flag, and also
 * test for the validity of that root (e.g., that a drive d:/ or network 
 * share //machine/foovol/). 
 * The function returns APR_ERELATIVE if filepath isn't rooted (an
 * error), APR_EINCOMPLETE if the root path is ambigious (but potentially
 * legitimate, e.g. "/" on Windows is incomplete because it doesn't specify
 * the drive letter), or APR_EBADPATH if the root is simply invalid.
 * APR_SUCCESS is returned if filepath is an absolute path.
 }
function apr_filepath_root(const rootpath, filepath: PPChar;
 flags: apr_int32_t; p: Papr_pool_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_filepath_root' + LibSuff16;

{
 * Merge additional file path onto the previously processed rootpath
 * @param newpath the merged paths returned
 * @param rootpath the root file path (NULL uses the current working path)
 * @param addpath the path to add to the root path
 * @param flags the desired APR_FILEPATH_ rules to apply when merging
 * @param p the pool to allocate the new path string from
 * @remark if the flag APR_FILEPATH_TRUENAME is given, and the addpath 
 * contains wildcard characters ('*', '?') on platforms that don't support 
 * such characters within filenames, the paths will be merged, but the 
 * result code will be APR_EPATHWILD, and all further segments will not
 * reflect the true filenames including the wildcard and following segments.
 }
function apr_filepath_merge(newpath: PPChar; const rootpath, addpath: PPChar;
 flags: apr_int32_t; p: Papr_pool_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_filepath_merge' + LibSuff20;

{
 * Split a search path into separate components
 * @param pathelts the returned components of the search path
 * @param liststr the search path (e.g., <tt>getenv("PATH")</tt>)
 * @param p the pool to allocate the array and path components from
 * @remark empty path componenta do not become part of @a pathelts.
 * @remark the path separator in @a liststr is system specific;
 * e.g., ':' on Unix, ';' on Windows, etc.
 }
function apr_filepath_list_split(pathelts: PPapr_array_header_t;
 const liststr: PChar; p: Papr_pool_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_filepath_list_split' + LibSuff12;

{
 * Merge a list of search path components into a single search path
 * @param liststr the returned search path; may be NULL if @a pathelts is empty
 * @param pathelts the components of the search path
 * @param p the pool to allocate the search path from
 * @remark emtpy strings in the source array are ignored.
 * @remark the path separator in @a liststr is system specific;
 * e.g., ':' on Unix, ';' on Windows, etc.
 }
function apr_filepath_list_merge(liststr: PPChar;
 pathelts: Papr_array_header_t; p: Papr_pool_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_filepath_list_merge' + LibSuff12;

{
 * Return the default file path (for relative file names)
 * @param path the default path string returned
 * @param flags optional flag APR_FILEPATH_NATIVE to retrieve the
 *              default file path in os-native format.
 * @param p the pool to allocate the default path string from
 }
function apr_filepath_get(path: PPChar; flags: apr_int32_t;
 p: Papr_pool_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_filepath_get' + LibSuff12;

{
 * Set the default file path (for relative file names)
 * @param path the default path returned
 * @param p the pool to allocate any working storage
 }
function apr_filepath_set(const path: PChar; p: Papr_pool_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_filepath_set' + LibSuff8;

const
  { The FilePath character encoding is unknown }
  APR_FILEPATH_ENCODING_UNKNOWN = 0;

  { The FilePath character encoding is locale-dependent }
  APR_FILEPATH_ENCODING_LOCALE =  1;

  { The FilePath character encoding is UTF-8 }
  APR_FILEPATH_ENCODING_UTF8 =    2;

{
 * Determine the encoding used internally by the FilePath functions
 * @param style points to a variable which receives the encoding style flag
 * @param p the pool to allocate any working storage
 * @remark Use @c apr_os_locale_encoding and/or @c apr_os_default_encoding
 * to get the name of the path encoding if it's not UTF-8.
 }
function apr_filepath_encoding(style: PInteger; p: Papr_pool_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_filepath_encoding' + LibSuff8;

