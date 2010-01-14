{
FUSE: Filesystem in Userspace -- Free Pascal bindings.

Copyright (C) 2001-2007  Miklos Szeredi <miklos@szeredi.hu>
Copyright (C) 2008 Danny Milosavljevic <danny_milo@yahoo.com>
Copyright (C) 2009 Michael A. Green <mgreen@emixode.com>

This program is free software; you can redistribute it and/or modify it under
the terms of the GNU Lesser General Public License as published by the Free
Software Foundation; either version 2 of the License, or (at your option) any
later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A
PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public License along
with this program; if not, write to the Free Software Foundation, Inc.,
51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
}
unit fuse;

{$ASSERTIONS ON}
{$MACRO ON}
{$MODE OBJFPC}
{$PACKRECORDS C}

{$ifndef FUSE_USE_VERSION}
  {$define FUSE_USE_VERSION := 26}
{$endif}

{$if (fuse_use_version < 26)}
  {$error Only FUSE Library 2.6 or better is supported}
{$endif}

{$if defined(fs32bit)}
  {$error FUSE expects sizeof(off_t)=8 (64-bits). Your libc does not support this}
{$endif}

{ TODO: Add low level functions }

interface

uses BaseUNIX, UNIXtype;

const
  FUSE_ROOT_ID = 1;                 // The node ID of the root inode

  { STAT Values }
  ST_RDONLY = 1;                    // Mount read-only.           - f_flag
  ST_NOSUID = 2;                    // Ignore suid and sgid bits. - f_flag
  ST_NODEV = 4;                     // Disallow access to device special files.
  ST_NOEXEC = 8;                    // Disallow program execution.
  ST_SYNCHRONOUS = 16;              // Writes are synced at once.
  ST_MANDLOCK = 64;                 // Allow mandatory locks on an FS.
  ST_WRITE = 128;                   // Write on file/directory/symlink.
  ST_APPEND = 256;                  // Append-only file.
  ST_IMMUTABLE = 512;               // Immutable file.
  ST_NOATIME = 1024;                // Do not update access times.
  ST_NODIRATIME = 2048;             // Do not update directory access times.
  ST_RELATIME = 4096;               // Update atime relative to mtime/ctime.

  { File Info Flags (see TFuseFileInfo)

    FUSE_FI_NONE        - No Flags Set
    FUSE_FI_DIRECT_IO   - Can be filled in by open, to use direct I/O on this
                          file. Introduced in version 2.4
    FUSE_FI_KEEP_CACHE  - Can be filled in by open, to indicate, that cached
                          file data need not be invalidated. Introduced in
                          version 2.4
    FUSE_FI_FLUSH       - Indicates a flush operation. Set in flush operation,
                          also maybe set in highlevel lock operation and
                          lowlevel release operation. Introduced in version 2.6
    FUSE_FI_NONSEEKABLE - Can be filled in by open, to indicate that the file is
                          not seekable. Introduced in version 2.9
  }
  FUSE_FI_NONE = 0;
  FUSE_FI_DIRECT_IO = 1;
  FUSE_FI_KEEP_CACHE = 2;
  FUSE_FI_FLUSH = 4;
  {$if (fuse_use_version >= 29)}
  FUSE_FI_NONSEEKABLE = 8
  {$endif}

  { Operation Flags (see TFuseOpereation)

    FUSE_OP_NONE       - No Flags Set
    FUSE_OP_NULLPATHOK - Flag indicating, that the filesystem can accept a NULL
                         path as the first argument for the following
                         operations:
                         read, write, flush, release, fsync, readdir,
                         releasedir, fsyncdir, ftruncate, fgetattr and lock
  }
  {$if (fuse_use_version >= 28)}
  FUSE_OP_NONE = 0;
  FUSE_OP_NULLPATHOK = $1;
  {$endif}

  { Capability Flags (see TFuseConnInfo.Capable and TFuseConnInfo.Want)

    FUSE_CAP_NONE           - No capability bits set
    FUSE_CAP_ASYNC_READ     - filesystem supports asynchronous read requests
    FUSE_CAP_POSIX_LOCKS    - filesystem supports "remote" locking
    FUSE_CAP_ATOMIC_O_TRUNC - filesystem handles the O_TRUNC open flag
    FUSE_CAP_EXPORT_SUPPORT - filesystem handles lookups of "." and ".."
    FUSE_CAP_BIG_WRITES     - filesystem can handle write size larger than 4kB
    FUSE_CAP_DONT_MASK      - don't apply umask to file mode on create
                              operations
  }
  FUSE_CAP_NONE = 0;
  FUSE_CAP_ASYNC_READ = 1;
  FUSE_CAP_POSIX_LOCKS = 2;
  FUSE_CAP_ATOMIC_O_TRUNC = 8;
  FUSE_CAP_EXPORT_SUPPORT = 16;
  FUSE_CAP_BIG_WRITES = 32;
  FUSE_CAP_DONT_MASK = 64;

  { Ioctl Flags (see TFuseOperations.ioctl)

    FUSE_IOCTL_COMPAT       - 32bit compat ioctl on 64bit machine
    FUSE_IOCTL_UNRESTRICTED - not restricted to well-formed ioctls, retry
                              allowed
    FUSE_IOCTL_RETRY        - retry with new iovecs
    FUSE_IOCTL_MAX_IOV      - maximum of in_iovecs + out_iovecs

    Introduced in version 2.8
  }
  {$if (fuse_use_version >= 28)}
  FUSE_IOCTL_NONE = 0;
  FUSE_IOCTL_COMPAT = 1;
  FUSE_IOCTL_UNRESTRICTED = 2;
  FUSE_IOCTL_RETRY = 4;
  FUSE_IOCTL_MAX_IOV = 256;
  {$endif}

type
  { Common Type Defines }
  PFuse = type pointer;
  PFuseSession = type pointer;
  PFuseChan = type pointer;
  PPFuseChan = ^PFuseChan;
  PFuseRequest = type pointer;
  PFuseCmd = type pointer;
  {$if (fuse_use_version >= 28)}
  PFusePollHandle = type pointer;
  {$endif}

  __fsblkcnt64_t = cuint64;
  __fsfilcnt64_t = cuint64;
  fsblkcnt_t = __fsblkcnt64_t; // 64-bit (LFS)
  fsfilcnt_t = __fsfilcnt64_t; // 64-bit (LFS)

  { VFS File System information structure

    <sys/statvfs.h>
  }
  TStatVFS = record
    f_bsize : culong;                // File system block size
    f_frsize : culong;               // Fudamental file system block size
    f_blocks,                        // Total number of blocks on file system in
                                     // units of f_frsize.
    f_bfree,                         // Total number of free blocks.
    f_bavail : fsblkcnt_t;           // Number of free blocks available to
                                     // non-privileged process.
    f_files,                         // Total number of file serial numbers.
    f_ffree,                         // Total number of free file serial numbers
    f_favail : fsfilcnt_t;           // Number of file serial numbers available
                                     // to non-privileged process.
    f_fsid : culong;                 // File system ID.
    __f_unused : cint;               // Unused
    f_flag : culong;                 // Bit mask of f_flag values.
    f_namemax : culong;              // Maximum filename length.
    __f_spare : array[0..5] of cint; // Spare
  end;
  PStatVFS = ^TStatVFS;

  TStatVFS64 = TStatVFS; PStatVFS64 = ^TStatVFS;

  { Information about open files }
  TFuseFileInfo = record
    flags : cint;                    // Open flags. Available in open() and
                                     // release()
    fh_old : culong deprecated;      // Old file handle, don't use
    writepage : cint;                // In case of a write oprtation indicates
                                     // if this was caused by a writepage
    fi_flags : cuint;                // See FUSE_FI_  flags
    fh : cuint64;                    // File handle. May be filled in by
                                     // filesystem in open().
                                     // Available in all other file operations
    lock_owner : cuint64;            // Lock owner id. Available in locking
                                     // operations and flush
  end;
  PFuseFileInfo = ^TFuseFileInfo;

  { Connection information, passed to the ->init() method

    Some of the elements are read-write, these can be changed to indicate the
    value requested by the filesystem.  The requested value must usually be
    smaller than the indicated value.
  }
  TFuseConnInfo = record
    ProtoMajor : cunsigned;       // Major version of the protocol (read-only)
    ProtoMinor : cunsigned;       // Minor version of the protocol (read-only)
    AsyncRead : cunsigned;        // Is asynchronous read supported (read-write)
    MaxWrite : cunsigned;         // Maximum size of the write buffer
    MaxReadahead : cunsigned;     // Maximum readahead
    Capable : cunsigned;          // Capability flags, that the kernel supports
    Want : cunsigned;             // Capability flags, that the filesystem wants
                                  // to enable. See FUSE_CAP_ flags
    Reserved : array [0..24] of cunsigned; // For future use.
  end;
  PFuseConnInfo = ^TFuseConnInfo;

  { Time Tuple for utimens() }
  TFuseTimeTuple = record
    AccessedTime,
    ModifiedTime : timespec;
  end;
  PFuseTimeTuple = ^TFuseTimeTuple;

  { Function to add an entry in a readdir() operation

    @param aBuffer the buffer passed to the readdir() operation
    @param aName the file name of the directory entry
    @param aStat file attributes, can be NULL
    @param aFileOffset offset of the next entry or zero
    @return 1 if buffer is full, zero otherwise
  }
  TFuseFillDir = function(aBuffer : pointer; const aName : PChar; const aStat : PStat; aFileOffset : TOff) : cint; cdecl;

  { Used by deprecated getdir() method }
  TFuseDirfil = function(aHandle : pointer; const aName : PChar; aType : cint; aIno : TIno) : cint deprecated; cdecl;

  { The file system operations:

    Most of these should work very similarly to the well known UNIX file system
    operations.  A major exception is that instead of returning an error in
    'errno', the operation should return the negated error value (-errno)
    directly.

    All methods are optional, but some are essential for a useful filesystem
    (e.g. getattr).  Open, flush, release, fsync, opendir, releasedir, fsyncdir,
    access, create, ftruncate, fgetattr, lock, init and destroy are special
    purpose methods, without which a full featured filesystem can still be
    implemented.

    Almost all operations take a path which can be of any length.

    Changed in fuse 2.8.0 (regardless of API version)
    Previously, paths were limited to a length of PATH_MAX.

    See http://fuse.sourceforge.net/wiki/ for more information.
  }
  TFuseOperations = record
    { Get file attributes.

      Similar to stat(). The 'st_dev' and 'st_blksize' fields are ignored.
      The 'st_ino' field is ignored except if the 'use_ino' mount option is given.
    }
    getattr : function(const aName : PChar; var aStat : TStat) : cint; cdecl;

    { Read the target of a symbolic link

      The buffer should be filled with a null terminated string. The buffer size
      argument includes the space for the terminating null character. If the
      linkname is too long to fit in the buffer, it should be truncated.
      The return value should be 0 for success.
    }
    readlink : function(const aName : PChar; aLinksToName : PChar; aLinksToNameSize: TSize) : cint; cdecl;

    { Deprecated, use readdir() instead }
    getdir : function(const aName : PChar; aDirectoryHandle : pointer; aDirfilFunc : TFuseDirfil) : cint deprecated; cdecl;

    { Create a file node

      This is called for creation of all non-directory, non-symlink nodes.If the
      filesystem defines a create() method, then for regular files that will be
      called instead
    }
    mknod : function(const aName : PChar; aMode : TMode; aDevice : TDev) : cint; cdecl;

    { Create a directory

      Note that the mode argument may not have the type specification bits set,
      i.e. S_ISDIR(mode) can be false. To obtain the correct directory type bits
      use  mode|S_IFDIR
    }
    mkdir : function(const aDirectoryName : PChar; aMode : TMode) : cint; cdecl;

    { Remove a file }
    unlink : function(const aName : PChar) : cint; cdecl;

    { Remove a directory }
    rmdir : function(const aName : PChar) : cint; cdecl;

    { Create a symbolic link }
    symlink : function(const aLinksToName, aName : PChar) : cint; cdecl;

    { Rename a file }
    rename : function(const aName, aNewName : PChar) : cint; cdecl;

    { Create a hard link to a file }
    link : function(const aLinksToName, aName : PChar) : cint; cdecl;

    { Change the permission bits of a file }
    chmod : function(const aName : PChar; aMode : TMode) : cint; cdecl;

    { Change the owner and group of a file }
    chown : function(const aName : PChar; aUID : TUid; aGID : TGid) : cint; cdecl;

    { Change the size of a file }
    truncate : function(const aName : PChar; aNewSize : TOff) : cint; cdecl;

    { Change the access and/or modification times of a file

      Deprecated, use utimens() instead.
    }
    utime : function(aName : PChar; aTime : Putimbuf) : cint deprecated; cdecl;

    { File open operation

      No creation (O_CREAT, O_EXCL) and by default also no truncation (O_TRUNC)
      flags will be passed to open(). If an application specifies O_TRUNC, fuse
      first calls truncate() and then open(). Only if 'atomic_o_trunc' has been
      specified and kernel version is 2.6.24 or later, O_TRUNC is passed on to
      open.

      Unless the 'default_permissions' mount option is given, open should check
      if the operation is permitted for the given flags. Optionally open may
      also return an arbitrary filehandle in the fuse_file_info structure, which
      will be passed to all file operations.

      Changed in version 2.2
    }
    open : function(const aName : PChar; aFileInfo : PFuseFileInfo) : cint; cdecl;

    { Read data from an open file

      Read should return exactly the number of bytes requested except on EOF or
      error, otherwise the rest of the data will be substituted with zeroes. An
      exception to this is when the 'direct_io' mount option is specified, in
      which case the return value of the read system call will reflect the
      return value of this operation.

      Changed in version 2.2
    }
    read : function(const aName : PChar; aBuffer : pointer; aBufferSize : TSize; aFileOffset : TOff; aFileInfo : PFuseFileInfo) : cint; cdecl;

    { Write data to an open file

      Write should return exactly the number of bytes requested except on error.
      An exception to this is when the 'direct_io' mount option is specified
      (see read operation).

      Changed in version 2.2
    }
    write : function(const aName : PChar; const aBuffer : Pointer; aBufferSize : TSize; aFileOffset : TOff; aFileInfo : PFuseFileInfo) : cint; cdecl;

    { Get file system statistics

      The 'f_frsize', 'f_favail', 'f_fsid' and 'f_flag' fields are ignored

      Replaced 'struct statfs' parameter with 'struct statvfs' in version 2.5
    }
    statfs : function(const aName : PChar; aStatVFS : PStatVFS) : cint; cdecl;

    { Possibly flush cached data

      BIG NOTE: This is not equivalent to fsync(). It's not a request to sync
      dirty data.

      Flush is called on each close() of a file descriptor. So if a filesystem
      wants to return write errors in close() and the file has cached dirty
      data, this is a good place to write back data and return any errors. Since
      many applications ignore close() errors this is not always useful.

      NOTE: The flush() method may be called more than once for each open().
      This happens if more than one file descriptor refers to an opened file due
      to dup(), dup2() or fork() calls. It is not possible to determine if a
      flush is final, so each flush should be treated equally.  Multiple
      write-flush sequences are relatively rare, so this shouldn't be a problem.

      Filesystems shouldn't assume that flush will always be called after some
      writes, or that if will be called at all.

      Changed in version 2.2
    }
    flush : function(const aName : PChar; aFileInfo : PFuseFileInfo) : cint; cdecl;

    { Release an open file

      Release is called when there are no more references to an open file: all
      file descriptors are closed and all memory mappings are unmapped.

      For every open() call there will be exactly one release() call with the
      same flags and file descriptor. It is possible to have a file opened more
      than once, in which case only the last release will mean, that no more
      reads/writes will happen on the file. The return value of release is
      ignored.

      Changed in version 2.2
    }
    release : function(const aName : PChar; aFileInfo : PFuseFileInfo) : cint; cdecl;

    { Synchronize file contents

      If the datasync parameter is non-zero, then only the user data should be
      flushed, not the meta data.

      Changed in version 2.2
    }
    fsync : function(const aName : PChar; aDataSync : cint; aFileInfo : PFuseFileInfo) : cint; cdecl;

    { Set Extended Attributes }
    setxattr : function(const aName, aKey, aValue : PChar; aValueSize : TSize; Flags : cint) : cint; cdecl;

    { Get Extended Attributes }
    getxattr : function(const aName, aKey : PChar; aValue : PChar; aValueSize : TSize) : cint; cdecl;

    { List Extended Attributes }
    listxattr : function(const aName : PChar; aList : PChar; aListSize : TSize) : cint; cdecl;

    { Remove Extended Attributes }
    removexattr : function(const aName, aKey : PChar) : cint; cdecl;

    { Open directory

      Unless the 'default_permissions' mount option is given, this method should
      check if opendir is permitted for this directory. Optionally opendir may
      also return an arbitrary filehandle in the fuse_file_info structure, which
      will be passed to readdir, closedir and fsyncdir.

      Introduced in version 2.3
    }
    opendir : function(const aName : PChar; aFileInfo : PFuseFileInfo) : cint; cdecl;

    { Read directory

      This supersedes the old getdir() interface. New applications should use
      this.

      The filesystem may choose between two modes of operation:

      1) The readdir implementation ignores the offset parameter, and passes
      zero to the filler function's offset.  The filler function will not
      return '1' (unless an error happens), so the whole directory is read in a
      single readdir operation.  This works just like the old getdir() method.

      2) The readdir implementation keeps track of the offsets of the directory
      entries. It uses the offset parameter and always passes non-zero offset to
      the filler function. When the buffer is full (or an error happens) the
      filler function will return '1'.

      Introduced in version 2.3
    }
    readdir : function(const aName : PChar; aBuffer : pointer; aFillDirFunc : TFuseFillDir; aFileOffset : TOff; aFileInfo : PFuseFileInfo) : cint; cdecl;

    { Release directory

      Introduced in version 2.3
    }
    releasedir : function(const aName : PChar; aFileInfo : PFuseFileInfo) : cint; cdecl;

    { Synchronize directory contents

      If the datasync parameter is non-zero, then only the user data should be
      flushed, not the meta data

      Introduced in version 2.3
    }
    fsyncdir : function(const aName : PChar; aDataSync : Integer; aFileInfo : PFuseFileInfo) : cint; cdecl;

    { Initialize filesystem

      The return value will passed in the private_data field of fuse_context to
      all file operations and as a parameter to the destroy() method.

      Introduced in version 2.3
      Changed in version 2.6
    }
    init : function(var aConnectionInfo : TFuseConnInfo) : pointer; cdecl;

    { Clean up filesystem

      Called on filesystem exit.

      Introduced in version 2.3
    }
    destroy : procedure(aUserData : pointer); cdecl;

    { Check file access permissions

      This will be called for the access() system call. If the
      'default_permissions' mount option is given, this method is not called.

      This method is not called under Linux kernel versions 2.4.x

      Introduced in version 2.5
    }
    access : function(const aName : PChar; aMode : cint) : cint; cdecl;

    { Create and open a file

      If the file does not exist, first create it with the specified mode, and
      then open it.

      If this method is not implemented or under Linux kernel versions earlier
      than 2.6.15, the mknod() and open() methods will be called instead.

      Introduced in version 2.5
    }
    create : function(const aName : PChar; aMode : TMode; aFileInfo : PFuseFileInfo) : cint; cdecl;

    { Change the size of an open file

      This method is called instead of the truncate() method if the truncation
      was invoked from an ftruncate() system call.

      If this method is not implemented or under Linux kernel versions earlier
      than 2.6.15, the truncate() method will be called instead.

      Introduced in version 2.5
    }
    ftruncate : function(const aName : PChar; aSize : TOff; aFileInfo : PFuseFileInfo) : cint; cdecl;

    { Get attributes from an open file

      This method is called instead of the getattr() method if the file
      information is available.

      Currently this is only called after the create() method if that is
      implemented (see above). Later it may be called for invocations of fstat()
      too.

      Introduced in version 2.5
    }
    fgetattr : function(constaName : PChar; aOutStat : PStat; PFileInfo : PFuseFileInfo) : cint; cdecl;

    { Perform POSIX file locking operation

      The cmd argument will be either F_GETLK, F_SETLK or F_SETLKW.

      For the meaning of fields in 'struct flock' see the man page for fcntl(2).
      The l_whence field will always be set to SEEK_SET.

      For checking lock ownership, the 'fuse_file_info->owner'argument must be
      used.

      For F_GETLK operation, the library will first check currently held locks,
      and if a conflicting lock is found it will return information without
      calling this method. This ensures, that for local locks the l_pid field is
      correctly filled in. The results may not be accurate in case of race
      conditions and inthe presence of hard links, but it's unlikly that an
      application would rely on accurate GETLK results in these cases. If a
      conflicting lock is not found, this method will be called, and the
      filesystem may fill out l_pid by a meaningful value, or it may leave this
      field zero.

      or F_SETLK and F_SETLKW the l_pid field will be set to the pid of the
      process performing the locking operation.

      Note: if this method is not implemented, the kernel will still allow file
      locking to work locally. Hence it is only interesting for network
      filesystems and similar.

      Introduced in version 2.6
    }
    lock : function(const aName : PChar; aFileInfo : PFuseFileInfo; aCMD : cint; var aLock : FLock) : cint; cdecl;

    { Change the access and modification times of a file with nanosecond
      resolution

      Introduced in version 2.6
    }
    utimens : function(const aName : PChar; const aTime : TFuseTimeTuple) : cint; cdecl;

    { Map block index within file to block index within device

      Note: This makes sense only for block device backed filesystems mounted
      with the 'blkdev' option

      Introduced in version 2.6
    }
    bmap : function(const aName : PChar; aBlockSize : TSize; aIndex : cuint64) : cint; cdecl;

  {$if (fuse_use_version >= 28)}
    { See FUSE_OP_ flags }
    operation_flags : cuint; // 32-bits!

    { Ioctl

      flags will have FUSE_IOCTL_COMPAT set for 32bit ioctls in 64bit
      environment. The size and direction of data is determined by _IOC_*()
      decoding of cmd. For _IOC_NONE, data will be NULL, for _IOC_WRITE data is
      out area, for _IOC_READ in area and if both are set in/out area. In all
      non-NULL cases, the area is of _IOC_SIZE(cmd) bytes.

      Introduced in version 2.8
    }
    ioctl : function(const aName : PChar; aCmd : cint; aArg : pointer; aFileInfo : PFuseFileInfo; aFlags : cuint; aData : pointer) : cint; cdecl;

    { Poll for IO readiness events

      Note: If ph is non-NULL, the client should notify when IO readiness events
      occur by calling fuse_notify_poll() with the specified ph.

      Regardless of the number of times poll with a non-NULL ph is received,
      single notification is enough to clear all. Notifying more times incurs
      overhead but doesn't harm correctness.

      The callee is responsible for destroying ph with fuse_pollhandle_destroy()
      when no longer in use.

      Introduced in version 2.8
    }
    poll : function(const aName : PChar; aFileInfo : PFuseFileInfo; aPH : PFusePollHandle; aREventSP : pcunsigned) : cint; cdecl;
  {$endif}
  end;
  PFuseOperations = ^TFuseOperations;

  { Extra context that may be needed by some filesystems

    The uid, gid and pid fields are not filled in case of a writepage operation.
  }
  TFuseContext = record
    fuse : PFuse;           // Pointer to the fuse object
    uid : TUid;             // User ID of the calling process
    gid : TGid;             // Group ID of the calling process
    pid : TPid;             // Thread ID of the calling process
    UserData : pointer;     // Private filesystem data (private_data)
    umask : TMode;          // Umask of the calling process (introduced in version 2.8)
  end;
  PFuseContext = ^TFuseContext;

  { Argument list }
  TFuseArgs = record
    argc : cint;       // Argument count
    argv : PPChar;     // Argument vector. NULL termiated
    allocated : cint;  // Is 'argv' allocated?
  end;
  PFuseArgs = ^TFuseArgs;

  { Function type used to process commands

    Will be deprecated in 3.0 API (see fuse_loop_mt_proc)
  }
  TFuseProcessor = procedure(aFuse : PFuse; aFuseCmd : PFuseCmd; aData : pointer); deprecated;

{ -----------------------------------------------------------
  Common FUSE functions
  ----------------------------------------------------------- }

{ Create a FUSE mountpoint

  Returns a control file descriptor suitable for passing tofuse_new()

  @param aMountpoint the mount point path
  @param aFuseArgs argument vector
  @return the communication channel on success, NULL on failure
}
function fuse_mount(const aMountpoint : PChar; aFuseArgs : PFuseArgs) : PFuseChan; cdecl;

{ Umount a FUSE mountpoint

  @param aMountpoint the mount point path
  @param aFuseChan the communication channel
}
procedure fuse_unmount(const mountpoint : PChar; aFuseChan : PFuseChan); cdecl;

{ Parse common options

  The following options are parsed:

    '-f'	    foreground
    '-d' '-odebug'  foreground, but keep the debug option
    '-s'	    single threaded
    '-h' '--help'   help
    '-ho'	    help without header
    '-ofsname=..'   file system name, if not present, then set to the program
 		    name

  All parameters may be NULL

  @param aFuseArgs argument vector
  @param aMountpoint the returned mountpoint, should be freed after use
  @param aMultithreaded set to 1 unless the '-s' option is present
  @param aForeground set to 1 if one of the relevant options is present
  @return 0 on success, -1 on failure
}
function fuse_parse_cmdline(aFuseArgs : PFuseArgs; var aMountpoint : PChar; var aMultithreaded : cint; var aForeground : cint) : cint; cdecl;

{ Go into the background

  @param foreground if true, stay in the foreground
  @return 0 on success, -1 on failure
}
function fuse_daemonize(aForeground : cint) : cint; cdecl;

{ Get the version of the library

  @return the version
}
function fuse_version : cint; cdecl;

{ Destroy poll handle

  @param ph the poll handle
}
{$if (fuse_use_version >= 28)}
procedure fuse_pollhandle_destroy(aFusePollHandle : PFusePollHandle); cdecl;
{$endif}

{ -----------------------------------------------------------
  Signal handling
  ----------------------------------------------------------- }

{ Exit session on HUP, TERM and INT signals and ignore PIPE signal

  Stores session in a global variable. May only be called once per process until
  fuse_remove_signal_handlers() is called.

  @param aSession the session to exit
  @return 0 on success, -1 on failure
}
function fuse_set_signal_handlers(aSession : PFuseSession) : cint; cdecl;

{ Restore default signal handlers

  Resets global session.  After this fuse_set_signal_handlers() may be called
  again.

  @param aSession the same session as given in fuse_set_signal_handlers()
}
procedure fuse_remove_signal_handlers(aSession : PFuseSession); cdecl;

{ *** *** *** }

{ Main function of FUSE

  This is for the lazy. This is all that has to be called from the main
  program block.

  This function does the following:
    - parses command line options (-d -s and -h)
    - passes relevant mount options to the fuse_mount()
    - installs signal handlers for INT, HUP, TERM and PIPE
    - registers an exit handler to unmount the filesystem on program exit
    - creates a fuse handle
    - registers the operations
    - calls either the single-threaded or the multi-threaded event loop

  @param aArgc the argument counter passed to the main program block
  @param aArgv the argument vector passed to the main program block
  @param aFuseOperations the file system operation
  @param aUserData user data supplied in the context during the init() method
  @return 0 on success, nonzero on failure
}
function fuse_main(aArgC : cint; aArgV: PPChar; aFuseOperations : PFuseOperations; aFuseOperationsSize : TSize; aUserData : pointer) : cint;

{ -----------------------------------------------------------
  More detailed API
  ----------------------------------------------------------- }

{ Create a new FUSE filesystem.

  @param aFuseChan the communication channel
  @param aFuseArgs argument vector
  @param aFuseOperations the filesystem operations
  @param aFuseOperationsSize the size of the fuse_operations structure
  @param aUserData user data supplied in the context during the init() method
  @return the created FUSE handle
}
function fuse_new(aFuseChan : PFuseChan; aFuseArgs : PFuseArgs; aFuseOperations : PFuseOperations; aFuseOperationsSize : TSize; aUserData : pointer) : PFuse; cdecl;

{ Destroy the FUSE handle.

  The communication channel attached to the handle is also destroyed.

  NOTE: This function does not unmount the filesystem. If this is needed, call
  fuse_unmount() before calling this function.

  @param aFuse the FUSE handle
}
procedure fuse_destroy(aFuse : PFuse); cdecl;

{ FUSE event loop.

  Requests from the kernel are processed, and the appropriate operations are
  called.

  @param aFuse the FUSE handle
  @return 0 if no error occurred, -1 otherwise
}
function fuse_loop(aFuse : PFuse): cint; cdecl;

{ Exit from event loop

  @param aFuse the FUSE handle
}
procedure fuse_exit(aFuse : PFuse); cdecl;

{ FUSE event loop with multiple threads

  Requests from the kernel are processed, and the appropriate operations are
  called. Request are processed in parallel by distributing them between
  multiple threads.

  Calling this function requires the pthreads library to be linked to the
  application.

  @param aFuse the FUSE handle
  @return 0 if no error occurred, -1 otherwise
}
function fuse_loop_mt(aFuse : PFuse) : cint; cdecl;

{ Get the current context

  The context is only valid for the duration of a filesystem operation, and thus
  must not be stored and used later.

  @return the context
}
function fuse_get_context : PFuseContext; cdecl;

{ Get the current supplementary group IDs for the current request

  Currently unsupported and will return -ENOSYS.
}
function fuse_getgroups(aSize : cint; aList : array of TGid) : cint;

{ Check if the current request has already been interrupted

  @return 1 if the request has been interrupted, 0 otherwise
}
function fuse_interrupted : cint; cdecl;

{ Obsolete, doesn't do anything

  @return -EINVAL
}
function fuse_invalidate(aFuse : PFuse; const aPath : PChar) : cint; deprecated;

{ Deprecated, don't use }
function fuse_is_lib_option(const aOpt : PChar) : cint; deprecated;

{ -----------------------------------------------------------
  Advanced API for event handling, don't worry about this...
  -----------------------------------------------------------

  NOTE: the following functions are deprecated, and will be removed
  from the 3.0 API.  Use the lowlevel session functions instead
}

{ This is the part of fuse_main() before the event loop }
function fuse_setup(aArgC : cint; aArgV : PPChar; aFuseOperations : PFuseOperations; aFuseOperationsSize : TSize; aMountpoint : pointer; aMultithreaded : pcint; aUserData : pointer) : PFuse; cdecl; deprecated;

{ This is the part of fuse_main() after the event loop }
procedure fuse_teardown(aFuse : PFuse; aMountpoint : pointer); cdecl; deprecated;

{ Read a single command.  If none are read, return NULL }
function fuse_read_cmd(aFuse : PFuse) : PFuseCmd; cdecl; deprecated;

{ Process a single command }
procedure fuse_process_cmd(aFuse : PFuse; aCmd : PFuseCmd); cdecl; deprecated;

{ Multi threaded event loop, which calls the custom command processor function }
function fuse_loop_mt_proc(aFuse : PFuse; aFuseProcessor : TFuseProcessor; aData : pointer) : cint; cdecl; deprecated;

{ Return the exited flag, which indicates if fuse_exit() has been called }
function fuse_exited(aFuse : PFuse) : cint; cdecl; deprecated;

{ This function is obsolete and implemented as a no-op }
procedure fuse_set_getcontext_func(aFuseContext : pointer); cdecl; deprecated;

{ Get session from fuse object }
function fuse_get_session(aFuse : PFuse) : PFuseSession; cdecl; deprecated;

{ ---------------------------------------------------------------------------- }

implementation

const
  {$if defined(linux)}
  FUSELIBFile = 'libfuse.so.2';
  {$else}
     {$error Unsupported Target}
  {$endif}

{ --- Common --- }
function fuse_mount(const aMountpoint : PChar; aFuseArgs : PFuseArgs) : PFuseChan; cdecl; external FUSELIBFile;
procedure fuse_unmount(const mountpoint : PChar; aFuseChan : PFuseChan); cdecl; external FUSELIBFile;
function fuse_parse_cmdline(aFuseArgs : PFuseArgs; var aMountpoint : PChar; var aMultithreaded : cint; var aForeground : cint) : cint; cdecl; external FUSELIBFile;
function fuse_daemonize(aForeground : cint) : cint; cdecl; external FUSELIBFile;
function fuse_version : cint; cdecl; external FUSELIBFile;
{$if (fuse_use_version >= 28)}
procedure fuse_pollhandle_destroy(aFusePollHandle : PFusePollHandle); cdecl; external FUSELIBFile;
{$endif}
function fuse_set_signal_handlers(aSession : PFuseSession) : cint; cdecl; external FUSELIBFile;
procedure fuse_remove_signal_handlers(aSession : PFuseSession); cdecl; external FUSELIBFile;

{ --- Main --- }
function fuse_main_real(aArgC : cint; aArgV: PPChar; aFuseOperations : PFuseOperations; aFuseOperationsSize : TSize; aUserData : pointer) : cint; cdecl; external FUSELIBFile;
function fuse_main(aArgC : cint; aArgV: PPChar; aFuseOperations : PFuseOperations; aFuseOperationsSize : TSize; aUserData : pointer) : cint;
begin
  Result := fuse_main_real(aArgC, aArgV, aFuseOperations, aFuseOperationsSize, aUserData);
end;

function fuse_new(aFuseChan : PFuseChan; aFuseArgs : PFuseArgs; aFuseOperations : PFuseOperations; aFuseOperationsSize : TSize; aUserData : pointer) : PFuse; cdecl; external FUSELIBFile;
procedure fuse_destroy(aFuse : PFuse); cdecl; external FUSELIBFile;
function fuse_loop(aFuse : PFuse): cint; cdecl; external FUSELIBFile;
procedure fuse_exit(aFuse : PFuse); cdecl; external FUSELIBFile;
function fuse_loop_mt(aFuse : PFuse) : cint; cdecl; external FUSELIBFile;
function fuse_get_context : PFuseContext; cdecl; external FUSELIBFile;

function fuse_getgroups(aSize : cint; aList : array of TGid) : cint;
begin
  Result := -ESysENOSYS;
end;

function fuse_interrupted : cint; cdecl; external FUSELIBFile;

function fuse_invalidate(aFuse : PFuse; const aPath : PChar) : cint;
begin
  Result := -ESysEINVAL;
end;

function fuse_is_lib_option(const aOpt : PChar) : cint;
begin
  Result := -ESysEINVAL;
end;

{ --- Deprecated --- }
function fuse_setup(aArgC : cint; aArgV : PPChar; aFuseOperations : PFuseOperations; aFuseOperationsSize : TSize; aMountpoint : pointer; aMultithreaded : Pcint; aUserData : pointer) : PFuse; cdecl; external FUSELIBFile;
procedure fuse_teardown(aFuse : PFuse; aMountpoint : pointer); cdecl; external FUSELIBFile;
function fuse_read_cmd(aFuse : PFuse) : PFuseCmd; cdecl; external FUSELIBFile;
procedure fuse_process_cmd(aFuse : PFuse; aCmd : PFuseCmd); cdecl; external FUSELIBFile;
function fuse_loop_mt_proc(aFuse : PFuse; aFuseProcessor : TFuseProcessor; aData : pointer) : cint; cdecl; external FUSELIBFile;
function fuse_exited(aFuse : PFuse) : cint; cdecl; external FUSELIBFile;
procedure fuse_set_getcontext_func(aFuseContext : pointer); cdecl; external FUSELIBFile;
function fuse_get_session(aFuse : PFuse) : PFuseSession; cdecl; external FUSELIBFile;


initialization
  assert(sizeof(off_t)=8, 'Assertion Failed - FUSE expects 64 bit file offsets (_FILE_OFFSET_BITS 64)');

end.

