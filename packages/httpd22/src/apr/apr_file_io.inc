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
 * @file apr_file_io.h
 * @brief APR File I/O Handling
 }

{#include "apr.h"
#include "apr_pools.h"
#include "apr_time.h"
#include "apr_errno.h"}
{.$include apr_file_info.inc}
{#include "apr_inherit.h"}

//#define APR_WANT_STDIO          {< for SEEK_* }
//#define APR_WANT_IOVEC          {< for apr_file_writev }
//#include "apr_want.h"

{
 * @defgroup apr_file_io File I/O Handling Functions
 * @ingroup APR 
 }

{
 * @defgroup apr_file_open_flags File Open Flags/Routines
 }

{ Note to implementors: Values in the range 0x00100000--0x80000000
   are reserved for platform-specific values. }

const
  APR_FOPEN_READ      = $00001;  {< Open the file for reading }
  APR_FOPEN_WRITE     = $00002;  {< Open the file for writing }
  APR_FOPEN_CREATE    = $00004;  {< Create the file if not there }
  APR_FOPEN_APPEND    = $00008;  {< Append to the end of the file }
  APR_FOPEN_TRUNCATE  = $00010;  {< Open the file and truncate
                                         to 0 length }
  APR_FOPEN_BINARY    = $00020;  {< Open the file in binary mode }
  APR_FOPEN_EXCL      = $00040;  {< Open should fail if APR_CREATE
                                         and file exists. }
  APR_FOPEN_BUFFERED  = $00080;  {< Open the file for buffered I/O }
  APR_FOPEN_DELONCLOSE= $00100;  {< Delete the file after close }
  APR_FOPEN_XTHREAD   = $00200;  {< Platform dependent tag to open
                                         the file for use across multiple
                                         threads }
  APR_FOPEN_SHARELOCK = $00400;  {< Platform dependent support for
                                         higher level locked read/write
                                         access to support writes across
                                         process/machines }
  APR_FOPEN_NOCLEANUP = $00800;  {< Do not register a cleanup
                                         when the file is opened }
  APR_FOPEN_SENDFILE_ENABLED = $01000; {< Advisory flag that this
                                             file should support
                                             apr_socket_sendfile operation }
  APR_FOPEN_LARGEFILE = $04000;  {< Platform dependent flag to enable
                                         large file support; WARNING see
                                         below. }
{ backcompat }
  APR_READ            = APR_FOPEN_READ;       {< @deprecated @see APR_FOPEN_READ }
  APR_WRITE           = APR_FOPEN_WRITE;      {< @deprecated @see APR_FOPEN_WRITE }
  APR_CREATE          = APR_FOPEN_CREATE;     {< @deprecated @see APR_FOPEN_CREATE }
  APR_APPEND          = APR_FOPEN_APPEND;     {< @deprecated @see APR_FOPEN_APPEND }
  APR_TRUNCATE        = APR_FOPEN_TRUNCATE;   {< @deprecated @see APR_FOPEN_TRUNCATE }
  APR_BINARY          = APR_FOPEN_BINARY;     {< @deprecated @see APR_FOPEN_BINARY }
  APR_EXCL            = APR_FOPEN_EXCL;       {< @deprecated @see APR_FOPEN_EXCL }
  APR_BUFFERED        = APR_FOPEN_BUFFERED;   {< @deprecated @see APR_FOPEN_BUFFERED }
  APR_DELONCLOSE      = APR_FOPEN_DELONCLOSE; {< @deprecated @see APR_FOPEN_DELONCLOSE }
  APR_XTHREAD         = APR_FOPEN_XTHREAD;    {< @deprecated @see APR_FOPEN_XTHREAD }
  APR_SHARELOCK       = APR_FOPEN_SHARELOCK;  {< @deprecated @see APR_FOPEN_SHARELOCK }
  APR_FILE_NOCLEANUP  = APR_FOPEN_NOCLEANUP;  {< @deprecated @see APR_FOPEN_NOCLEANUP }
  APR_SENDFILE_ENABLED= APR_FOPEN_SENDFILE_ENABLED; {< @deprecated @see APR_FOPEN_SENDFILE_ENABLED }
  APR_LARGEFILE       = APR_FOPEN_LARGEFILE;  {< @deprecated @see APR_FOPEN_LARGEFILE }

{ @warning The APR_LARGEFILE flag only has effect on some platforms
 * where sizeof(apr_off_t) == 4.  Where implemented, it allows opening
 * and writing to a file which exceeds the size which can be
 * represented by apr_off_t (2 gigabytes).  When a file's size does
 * exceed 2Gb, apr_file_info_get() will fail with an error on the
 * descriptor, likewise apr_stat()/apr_lstat() will fail on the
 * filename.  apr_dir_read() will fail with APR_INCOMPLETE on a
 * directory entry for a large file depending on the particular
 * APR_FINFO_* flags.  Generally, it is not recommended to use this
 * flag. }

{
 * @defgroup apr_file_seek_flags File Seek Flags
 }

{
 * @defgroup apr_file_attrs_set_flags File Attribute Flags
 }

{ flags for apr_file_attrs_set }
  APR_FILE_ATTR_READONLY =  $01;          {< File is read-only }
  APR_FILE_ATTR_EXECUTABLE =$02;          {< File is executable }
  APR_FILE_ATTR_HIDDEN =    $04;          {< File is hidden }

{
 * @defgroup apr_file_writev(_full) max iovec size
 }
{$ifdef DOXYGEN}
  APR_MAX_IOVEC_SIZE = 1024;                {< System dependent maximum
                                                    size of an iovec array }
{#elif defined(IOV_MAX)
#define APR_MAX_IOVEC_SIZE IOV_MAX
#elif defined(MAX_IOVEC)
#define APR_MAX_IOVEC_SIZE MAX_IOVEC}
{$else}
  APR_MAX_IOVEC_SIZE = 1024;
{$endif}

{ File attributes }
type
  apr_fileattrs_t = apr_uint32_t;

{ Type to pass as whence argument to apr_file_seek. }
  apr_seek_where_t = Integer;

{
 * Structure for referencing files.
 }
  apr_file_t = record end;
//  Papr_file_t = ^apr_file_t;
  PPapr_file_t = ^Papr_file_t;

{ File lock types/flags }
{
 * @defgroup apr_file_lock_types File Lock Types
 }

const
  APR_FLOCK_SHARED =       1;       {< Shared lock. More than one process
                                           or thread can hold a shared lock
                                           at any given time. Essentially,
                                           this is a "read lock", preventing
                                           writers from establishing an
                                           exclusive lock. }
  APR_FLOCK_EXCLUSIVE =    2;       {< Exclusive lock. Only one process
                                           may hold an exclusive lock at any
                                           given time. This is analogous to
                                           a "write lock". }

  APR_FLOCK_TYPEMASK =     $000F;  {< mask to extract lock type }
  APR_FLOCK_NONBLOCK =     $0010;  {< do not block while acquiring the
                                           file lock }
{
 * Open the specified file.
 * @param newf The opened file descriptor.
 * @param fname The full path to the file (using / on all systems)
 * @param flag Or'ed value of:
 * <PRE>
 *         APR_READ              open for reading
 *         APR_WRITE             open for writing
 *         APR_CREATE            create the file if not there
 *         APR_APPEND            file ptr is set to end prior to all writes
 *         APR_TRUNCATE          set length to zero if file exists
 *         APR_BINARY            not a text file (This flag is ignored on 
 *                               UNIX because it has no meaning)
 *         APR_BUFFERED          buffer the data.  Default is non-buffered
 *         APR_EXCL              return error if APR_CREATE and file exists
 *         APR_DELONCLOSE        delete the file after closing.
 *         APR_XTHREAD           Platform dependent tag to open the file
 *                               for use across multiple threads
 *         APR_SHARELOCK         Platform dependent support for higher
 *                               level locked read/write access to support
 *                               writes across process/machines
 *         APR_FILE_NOCLEANUP    Do not register a cleanup with the pool 
 *                               passed in on the <EM>pool</EM> argument (see below).
 *                               The apr_os_file_t handle in apr_file_t will not
 *                               be closed when the pool is destroyed.
 *         APR_SENDFILE_ENABLED  Open with appropriate platform semantics
 *                               for sendfile operations.  Advisory only,
 *                               apr_socket_sendfile does not check this flag.
 * </PRE>
 * @param perm Access permissions for file.
 * @param pool The pool to use.
 * @remark If perm is APR_OS_DEFAULT and the file is being created, appropriate 
 *      default permissions will be used.  *arg1 must point to a valid file_t, 
 *      or NULL (in which case it will be allocated)
 }
function apr_file_open(newf: PPapr_file_t; const fname: PChar;
 flag: apr_int32_t; perm: apr_fileperms_t;
 pool: Papr_pool_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_file_open' + LibSuff20;

{
 * Close the specified file.
 * @param file The file descriptor to close.
 }
function apr_file_close(file_: Papr_file_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_file_close' + LibSuff4;

{
 * delete the specified file.
 * @param path The full path to the file (using / on all systems)
 * @param pool The pool to use.
 * @remark If the file is open, it won't be removed until all instances are closed.
 }
function apr_file_remove(const path: PChar; pool: Papr_pool_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_file_remove' + LibSuff8;

{
 * rename the specified file.
 * @param from_path The full path to the original file (using / on all systems)
 * @param to_path The full path to the new file (using / on all systems)
 * @param pool The pool to use.
 * @warning If a file exists at the new location, then it will be overwritten.  
 *      Moving files or directories across devices may not be possible.
 }
function apr_file_rename(const from_path, to_path: PChar; pool: Papr_pool_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_file_rename' + LibSuff12;

{
 * copy the specified file to another file.
 * @param from_path The full path to the original file (using / on all systems)
 * @param to_path The full path to the new file (using / on all systems)
 * @param perms Access permissions for the new file if it is created.
 *     In place of the usual or'd combination of file permissions, the
 *     value APR_FILE_SOURCE_PERMS may be given, in which case the source
 *     file's permissions are copied.
 * @param pool The pool to use.
 * @remark The new file does not need to exist, it will be created if required.
 * @warning If the new file already exists, its contents will be overwritten.
 }
function apr_file_copy(const from_path, to_path: PChar;
 perms: apr_fileperms_t; pool: Papr_pool_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_file_copy' + LibSuff16;

{
 * append the specified file to another file.
 * @param from_path The full path to the source file (using / on all systems)
 * @param to_path The full path to the destination file (using / on all systems)
 * @param perms Access permissions for the destination file if it is created.
 *     In place of the usual or'd combination of file permissions, the
 *     value APR_FILE_SOURCE_PERMS may be given, in which case the source
 *     file's permissions are copied.
 * @param pool The pool to use.
 * @remark The new file does not need to exist, it will be created if required.
 }
function apr_file_append(const from_path, to_path: PChar;
 perms: apr_fileperms_t; pool: Papr_pool_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_file_append' + LibSuff16;

{
 * Are we at the end of the file
 * @param fptr The apr file we are testing.
 * @remark Returns APR_EOF if we are at the end of file, APR_SUCCESS otherwise.
 }
function apr_file_eof(fptr: Papr_file_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_file_eof' + LibSuff4;

{
 * open standard error as an apr file pointer.
 * @param thefile The apr file to use as stderr.
 * @param pool The pool to allocate the file out of.
 * 
 * @remark The only reason that the apr_file_open_std* functions exist
 * is that you may not always have a stderr/out/in on Windows.  This
 * is generally a problem with newer versions of Windows and services.
 * 
 * The other problem is that the C library functions generally work
 * differently on Windows and Unix.  So, by using apr_file_open_std*
 * functions, you can get a handle to an APR struct that works with
 * the APR functions which are supposed to work identically on all
 * platforms.
 }
function apr_file_open_stderr(thefile: PPapr_file_t;
 pool: Papr_pool_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_file_open_stderr' + LibSuff8;

{
 * open standard output as an apr file pointer.
 * @param thefile The apr file to use as stdout.
 * @param pool The pool to allocate the file out of.
 *
 * @remark See remarks for apr_file_open_stdout.
 }
function apr_file_open_stdout(thefile: PPapr_file_t;
 pool: Papr_pool_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_file_open_stdout' + LibSuff8;

{
 * open standard input as an apr file pointer.
 * @param thefile The apr file to use as stdin.
 * @param pool The pool to allocate the file out of.
 *
 * @remark See remarks for apr_file_open_stdout.
 }
function apr_file_open_stdin(thefile: PPapr_file_t;
 pool: Papr_pool_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_file_open_stdin' + LibSuff8;

{
 * Read data from the specified file.
 * @param thefile The file descriptor to read from.
 * @param buf The buffer to store the data to.
 * @param nbytes On entry, the number of bytes to read; on exit, the number
 * of bytes read.
 *
 * @remark apr_file_read will read up to the specified number of
 * bytes, but never more.  If there isn't enough data to fill that
 * number of bytes, all of the available data is read.  The third
 * argument is modified to reflect the number of bytes read.  If a
 * char was put back into the stream via ungetc, it will be the first
 * character returned.
 *
 * @remark It is not possible for both bytes to be read and an APR_EOF
 * or other error to be returned.  APR_EINTR is never returned.
 }
function apr_file_read(thefile: Papr_file_t; buf: Pointer;
 nbytes: Papr_size_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_file_read' + LibSuff12;

{
 * Write data to the specified file.
 * @param thefile The file descriptor to write to.
 * @param buf The buffer which contains the data.
 * @param nbytes On entry, the number of bytes to write; on exit, the number
 *               of bytes written.
 *
 * @remark apr_file_write will write up to the specified number of
 * bytes, but never more.  If the OS cannot write that many bytes, it
 * will write as many as it can.  The third argument is modified to
 * reflect the * number of bytes written.
 *
 * @remark It is possible for both bytes to be written and an error to
 * be returned.  APR_EINTR is never returned.
 }
function apr_file_write(thefile: Papr_file_t; buf: Pointer;
 nbytes: Papr_size_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_file_write' + LibSuff12;

{
 * Write data from iovec array to the specified file.
 * @param thefile The file descriptor to write to.
 * @param vec The array from which to get the data to write to the file.
 * @param nvec The number of elements in the struct iovec array. This must
 *             be smaller than APR_MAX_IOVEC_SIZE.  If it isn't, the function
 *             will fail with APR_EINVAL.
 * @param nbytes The number of bytes written.
 *
 * @remark It is possible for both bytes to be written and an error to
 * be returned.  APR_EINTR is never returned.
 *
 * @remark apr_file_writev is available even if the underlying
 * operating system doesn't provide writev().
 }
function apr_file_writev(thefile: Papr_file_t; const vec: Piovec;
 nvec: apr_size_t; nbytes: Papr_size_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_file_writev' + LibSuff16;

{
 * Read data from the specified file, ensuring that the buffer is filled
 * before returning.
 * @param thefile The file descriptor to read from.
 * @param buf The buffer to store the data to.
 * @param nbytes The number of bytes to read.
 * @param bytes_read If non-NULL, this will contain the number of bytes read.
 *
 * @remark apr_file_read will read up to the specified number of
 * bytes, but never more.  If there isn't enough data to fill that
 * number of bytes, then the process/thread will block until it is
 * available or EOF is reached.  If a char was put back into the
 * stream via ungetc, it will be the first character returned.
 *
 * @remark It is possible for both bytes to be read and an error to be
 * returned.  And if *bytes_read is less than nbytes, an accompanying
 * error is _always_ returned.
 *
 * @remark APR_EINTR is never returned.
 }
function apr_file_read_full(thefile: Papr_file_t; buf: Pointer;
 nbytes: apr_size_t; bytes_read: Papr_size_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_file_read_full' + LibSuff16;

{
 * Write data to the specified file, ensuring that all of the data is
 * written before returning.
 * @param thefile The file descriptor to write to.
 * @param buf The buffer which contains the data.
 * @param nbytes The number of bytes to write.
 * @param bytes_written If non-NULL, set to the number of bytes written.
 *
 * @remark apr_file_write will write up to the specified number of
 * bytes, but never more.  If the OS cannot write that many bytes, the
 * process/thread will block until they can be written. Exceptional
 * error such as "out of space" or "pipe closed" will terminate with
 * an error.
 *
 * @remark It is possible for both bytes to be written and an error to
 * be returned.  And if *bytes_written is less than nbytes, an
 * accompanying error is _always_ returned.
 *
 * @remark APR_EINTR is never returned.
 }
function apr_file_write_full(thefile: Papr_file_t; buf: Pointer;
 nbytes: apr_size_t; bytes_written: Papr_size_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_file_write_full' + LibSuff16;

{
 * Write data from iovec array to the specified file, ensuring that all of the
 * data is written before returning.
 * @param thefile The file descriptor to write to.
 * @param vec The array from which to get the data to write to the file.
 * @param nvec The number of elements in the struct iovec array. This must
 *             be smaller than APR_MAX_IOVEC_SIZE.  If it isn't, the function
 *             will fail with APR_EINVAL.
 * @param nbytes The number of bytes written.
 *
 * @remark apr_file_writev_full is available even if the underlying
 * operating system doesn't provide writev().
 }
function apr_file_writev_full(thefile: Papr_file_t; const vec: Piovec;
 nvec: apr_size_t; nbytes: Papr_size_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_file_writev_full' + LibSuff16;

{
 * Write a character into the specified file.
 * @param ch The character to write.
 * @param thefile The file descriptor to write to
 }
function apr_file_putc(ch: Char; thefile: Papr_file_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_file_putc' + LibSuff8;

{
 * get a character from the specified file.
 * @param ch The character to read into
 * @param thefile The file descriptor to read from
 }
function apr_file_getc(ch: PChar; thefile: Papr_file_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_file_getc' + LibSuff8;

{
 * put a character back onto a specified stream.
 * @param ch The character to write.
 * @param thefile The file descriptor to write to
 }
function apr_file_ungetc(ch: Char; thefile: PPapr_file_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_file_ungetc' + LibSuff8;

{
 * Get a string from a specified file.
 * @param str The buffer to store the string in. 
 * @param len The length of the string
 * @param thefile The file descriptor to read from
 * @remark The buffer will be '\0'-terminated if any characters are stored.
 }
function apr_file_gets(str: PChar; len: Integer;
 thefile: Papr_file_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_file_gets' + LibSuff12;

{
 * Put the string into a specified file.
 * @param str The string to write. 
 * @param thefile The file descriptor to write to
 }
function apr_file_puts(const str: PChar; thefile: Papr_file_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_file_puts' + LibSuff8;

{
 * Flush the file's buffer.
 * @param thefile The file descriptor to flush
 }
function apr_file_flush(thefile: PPapr_file_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_file_flush' + LibSuff4;

{
 * duplicate the specified file descriptor.
 * @param new_file The structure to duplicate into. 
 * @param old_file The file to duplicate.
 * @param p The pool to use for the new file.
 * @remark *new_file must point to a valid apr_file_t, or point to NULL
 }
function apr_file_dup(new_file: PPapr_file_t; old_file: PPapr_file_t;
 p: Papr_pool_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_file_dup' + LibSuff12;

{
 * duplicate the specified file descriptor and close the original
 * @param new_file The old file that is to be closed and reused
 * @param old_file The file to duplicate
 * @param p        The pool to use for the new file
 *
 * @remark new_file MUST point at a valid apr_file_t. It cannot be NULL
 }
function apr_file_dup2(new_file: PPapr_file_t; old_file: PPapr_file_t;
 p: Papr_pool_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_file_dup2' + LibSuff12;

{
 * move the specified file descriptor to a new pool
 * @param new_file Pointer in which to return the new apr_file_t
 * @param old_file The file to move
 * @param p        The pool to which the descriptor is to be moved
 * @remark Unlike apr_file_dup2(), this function doesn't do an
 *         OS dup() operation on the underlying descriptor; it just
 *         moves the descriptor's apr_file_t wrapper to a new pool.
 * @remark The new pool need not be an ancestor of old_file's pool.
 * @remark After calling this function, old_file may not be used
 }
function apr_file_setaside(new_file: PPapr_file_t; old_file: PPapr_file_t;
 p: Papr_pool_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_file_setaside' + LibSuff12;

{
 * Move the read/write file offset to a specified byte within a file.
 * @param thefile The file descriptor
 * @param where How to move the pointer, one of:
 * <PRE>
 *            APR_SET  --  set the offset to offset
 *            APR_CUR  --  add the offset to the current position 
 *            APR_END  --  add the offset to the current file size 
 * </PRE>
 * @param offset The offset to move the pointer to.
 * @remark The third argument is modified to be the offset the pointer
          was actually moved to.
 }
function apr_file_seek(thefile: Papr_file_t;
 where: apr_seek_where_t; offset: Papr_off_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_file_seek' + LibSuff12;

{
 * Create an anonymous pipe.
 * @param in The file descriptor to use as input to the pipe.
 * @param out The file descriptor to use as output from the pipe.
 * @param pool The pool to operate on.
 }
function apr_file_pipe_create(in_: PPapr_file_t; out_: PPapr_file_t;
 pool: Papr_pool_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_file_pipe_create' + LibSuff12;

{
 * Create a named pipe.
 * @param filename The filename of the named pipe
 * @param perm The permissions for the newly created pipe.
 * @param pool The pool to operate on.
 }
function apr_file_namedpipe_create(const filename: PChar;
 perm: apr_fileperms_t; pool: Papr_pool_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_file_namedpipe_create' + LibSuff12;

{
 * Get the timeout value for a pipe or manipulate the blocking state.
 * @param thepipe The pipe we are getting a timeout for.
 * @param timeout The current timeout value in microseconds. 
 }
function apr_file_pipe_timeout_get(thepipe: Papr_file_t;
 timeout: Papr_interval_time_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_file_pipe_timeout_get' + LibSuff8;

{
 * Set the timeout value for a pipe or manipulate the blocking state.
 * @param thepipe The pipe we are setting a timeout on.
 * @param timeout The timeout value in microseconds.  Values < 0 mean wait 
 *        forever, 0 means do not wait at all.
 }
function apr_file_pipe_timeout_set(thepipe: Papr_file_t;
 timeout: apr_interval_time_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_file_pipe_timeout_set' + LibSuff12;

{ file (un)locking functions. }

{
 * Establish a lock on the specified, open file. The lock may be advisory
 * or mandatory, at the discretion of the platform. The lock applies to
 * the file as a whole, rather than a specific range. Locks are established
 * on a per-thread/process basis; a second lock by the same thread will not
 * block.
 * @param thefile The file to lock.
 * @param type The type of lock to establish on the file.
 }
function apr_file_lock(thefile: Papr_file_t; type_: Integer): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_file_lock' + LibSuff8;

{
 * Remove any outstanding locks on the file.
 * @param thefile The file to unlock.
 }
function apr_file_unlock(thefile: Papr_file_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_file_unlock' + LibSuff4;

{accessor and general file_io functions. }

{
 * return the file name of the current file.
 * @param new_path The path of the file.  
 * @param thefile The currently open file.
 }
function apr_file_name_get(const newpath: PPChar; thefile: Papr_file_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_file_name_get' + LibSuff8;

{
 * Return the data associated with the current file.
 * @param data The user data associated with the file.  
 * @param key The key to use for retreiving data associated with this file.
 * @param file The currently open file.
 }
function apr_file_data_get(data: PPointer; const key: PChar;
 file_: Papr_file_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_file_data_get' + LibSuff12;

{
 * Set the data associated with the current file.
 * @param file The currently open file.
 * @param data The user data to associate with the file.  
 * @param key The key to use for assocaiteing data with the file.
 * @param cleanup The cleanup routine to use when the file is destroyed.
 }
//function apr_file_data_set(ch: Char; thefile: PPapr_file_t): apr_status_t;
// {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
// external LibAPR name LibNamePrefix + 'apr_file_data_set' + LibSuff4;
//APR_DECLARE(apr_status_t) (apr_file_t *file, void *data,
//                                           const char *key,
//                                           apr_status_t (*cleanup)(void *));

{
 * Write a string to a file using a printf format.
 * @param fptr The file to write to.
 * @param format The format string
 * @param ... The values to substitute in the format string
 * @return The number of bytes written
 }
function apr_file_printf(fptr: Papr_file_t; const format: PChar;
 othres: array of const): Integer;
 cdecl; external LibAPR name 'apr_file_printf';

{
 * set the specified file's permission bits.
 * @param fname The file (name) to apply the permissions to.
 * @param perms The permission bits to apply to the file.
 * @warning Some platforms may not be able to apply all of the available 
 *      permission bits; APR_INCOMPLETE will be returned if some permissions 
 *      are specified which could not be set.
 *
 *      Platforms which do not implement this feature will return APR_ENOTIMPL.
 }
function apr_file_perms_set(const fname: PChar;
 perms: apr_fileperms_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_file_perms_set' + LibSuff8;

{
 * Set attributes of the specified file.
 * @param fname The full path to the file (using / on all systems)
 * @param attributes Or'd combination of
 * <PRE>
 *            APR_FILE_ATTR_READONLY   - make the file readonly
 *            APR_FILE_ATTR_EXECUTABLE - make the file executable
 *            APR_FILE_ATTR_HIDDEN     - make the file hidden
 * </PRE>
 * @param attr_mask Mask of valid bits in attributes.
 * @param pool the pool to use.
 * @remark This function should be used in preference to explict manipulation
 *      of the file permissions, because the operations to provide these
 *      attributes are platform specific and may involve more than simply
 *      setting permission bits.
 * @warning Platforms which do not implement this feature will return
 *      APR_ENOTIMPL.
 }
function apr_file_attrs_set(const fname: PChar;
 attributes, attr_mask: apr_fileattrs_t;
 pool: Papr_pool_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_file_attrs_set' + LibSuff16;

{
 * Set the mtime of the specified file.
 * @param fname The full path to the file (using / on all systems)
 * @param mtime The mtime to apply to the file.
 * @param pool The pool to use.
 * @warning Platforms which do not implement this feature will return
 *      APR_ENOTIMPL.
 }
function apr_file_mtime_set(const fname: PChar;
 mtime: apr_time_t; pool: Papr_pool_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_file_mtime_set' + LibSuff16;

{
 * Create a new directory on the file system.
 * @param path the path for the directory to be created.  (use / on all systems)
 * @param perm Permissions for the new direcoty.
 * @param pool the pool to use.
 }
function apr_dir_make(const path: PChar; perm: apr_fileperms_t;
 pool: Papr_pool_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_dir_make' + LibSuff12;

{ Creates a new directory on the file system, but behaves like
 * 'mkdir -p'. Creates intermediate directories as required. No error
 * will be reported if PATH already exists.
 * @param path the path for the directory to be created.  (use / on all systems)
 * @param perm Permissions for the new direcoty.
 * @param pool the pool to use.
 }
function apr_dir_make_recursive(const path: PChar;
 perm: apr_fileperms_t; pool: Papr_pool_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_dir_make_recursive' + LibSuff12;

{
 * Remove directory from the file system.
 * @param path the path for the directory to be removed.  (use / on all systems)
 * @param pool the pool to use.
 }
function apr_dir_remove(const path: PChar; pool: Papr_pool_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_dir_remove' + LibSuff8;

{
 * get the specified file's stats.
 * @param finfo Where to store the information about the file.
 * @param wanted The desired apr_finfo_t fields, as a bit flag of APR_FINFO_ values 
 * @param thefile The file to get information about.
 }
function apr_file_info_get(finfo: Papr_finfo_t;
 wanted: apr_int32_t; thefile: Papr_file_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_file_info_get' + LibSuff12;

{
 * Truncate the file's length to the specified offset
 * @param fp The file to truncate
 * @param offset The offset to truncate to.
 }
function apr_file_trunc(fp: Papr_file_t; offset: apr_off_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_file_trunc' + LibSuff12;

{
 * Retrieve the flags that were passed into apr_file_open()
 * when the file was opened.
 * @return apr_int32_t the flags
 }
function apr_file_flags_get(f: Papr_file_t): apr_int32_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_file_flags_get' + LibSuff4;

{
 * Get the pool used by the file.
 }
{APR_POOL_DECLARE_ACCESSOR(file);
}
{
 * Set a file to be inherited by child processes.
 *
 }
{APR_DECLARE_INHERIT_SET(file);
}

{
 * Unset a file from being inherited by child processes.
 }
{APR_DECLARE_INHERIT_UNSET(file);
}

{
 * Open a temporary file
 * @param fp The apr file to use as a temporary file.
 * @param templ The template to use when creating a temp file.
 * @param flags The flags to open the file with. If this is zero,
 *              the file is opened with 
 *              APR_CREATE | APR_READ | APR_WRITE | APR_EXCL | APR_DELONCLOSE
 * @param p The pool to allocate the file out of.
 * @remark   
 * This function  generates  a unique temporary file name from template.  
 * The last six characters of template must be XXXXXX and these are replaced 
 * with a string that makes the filename unique. Since it will  be  modified,
 * template must not be a string constant, but should be declared as a character
 * array.  
 *
 }
function apr_file_mktemp(fp: PPapr_file_t; templ: PChar;
 flags: apr_int32_t; p: Papr_pool_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_file_mktemp' + LibSuff16;

{
 * Find an existing directory suitable as a temporary storage location.
 * @param temp_dir The temp directory.
 * @param p The pool to use for any necessary allocations.
 * @remark   
 * This function uses an algorithm to search for a directory that an
 * an application can use for temporary storage.  Once such a
 * directory is found, that location is cached by the library.  Thus,
 * callers only pay the cost of this algorithm once if that one time
 * is successful.
 *
 }
function apr_temp_dir_get(const temp_dir: PPChar; p: Papr_pool_t): apr_status_t;
 {$IFDEF WINDOWS} stdcall; {$ELSE} cdecl; {$ENDIF}
 external LibAPR name LibNamePrefix + 'apr_temp_dir_get' + LibSuff8;

