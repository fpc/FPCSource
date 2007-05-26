{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Jonas Maebe,
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$ifndef FPC_USE_LIBC} // kernel record

  Stat = packed record  
    case byte of
      0:
        (dev        : qword    deprecated;
        __pad0      : array[0..3] of byte deprecated;
        __ino       : cardinal deprecated;
        mode        : cardinal deprecated;
        nlink       : cardinal deprecated;
        uid	    : cardinal deprecated;
        gid         : cardinal deprecated;
        rdev        : qword deprecated;
        __pad3      : array[0..3] of byte deprecated;
        size        : qword    deprecated;
        blksize     : cardinal deprecated;
        blocks      : qword    deprecated;
        atime	    : cardinal deprecated;
        atime_nsec  : cardinal deprecated;
        mtime	    : cardinal deprecated; 
        mtime_nsec  : cardinal deprecated;
        ctime	    : cardinal deprecated;
        ctime_nsec  : cardinal deprecated;
        ino         : qword    deprecated);
      1:		// Unix typing will be reintroduced.
        (st_dev   : qword;
        __pad0_   : array[0..3] of byte;
        __st_ino_,
        st_mode,
        st_nlink,
        st_uid,
        st_gid    : cardinal;
        st_rdev   : qword;
        __pad3_   : array[0..3] of byte;
        st_size   : qword;
        st_blksize: cardinal;
        st_blocks : qword;
        st_atime,
        st_atime_nsec,
        st_mtime,
        st_mtime_nsec,
        st_ctime,
        st_ctime_nsec : cardinal;
        st_ino    : qword);
  end;

{$else}

{$packrecords C}
  Stat =  Record
    case byte of
      0:
                              // kernel <->libc
        (dev    	 : dev_t    deprecated;
        pad1    	 : word     deprecated;
{$ifdef fs32bit}
        ino     	 : ino_t    deprecated;
{$else}
        __ino   	 : ino_t    deprecated;
{$endif}
        mode    	 : mode_t   deprecated;
        nlink   	 : nlink_t  deprecated;
        uid     	 : uid_t    deprecated;
        gid     	 : gid_t    deprecated;
        rdev    	 : dev_t    deprecated;
        pad2    	 : word     deprecated;
        size    	 : off_t    deprecated;
        blksize 	 : blksize_t deprecated;
{$ifdef fs32bit}
        blocks  	 : blkcnt_t deprecated;
{$else}    
        blocks  	 : blkcnt64_t deprecated;
{$endif}    
        atime		 : cardinal deprecated;
        atime_nsec 	 : cardinal deprecated;
        mtime 		 : cardinal deprecated;
        mtime_nsec 	 : cardinal deprecated;
        ctime 		 : cardinal deprecated;
        ctime_nsec 	 : cardinal deprecated;
{$ifdef fs32bit}
        unused4		 : cardinal deprecated;
	unused5		 : cardinal deprecated;
{$else}    
        ino     	 : ino64_t  deprecated;
{$endif});
      1:
                              // kernel <->libc
        (st_dev    : dev_t;
        pad1_      : word;
{$ifdef fs32bit}
        st_ino     : ino_t;
{$else}
        __st_ino   : ino_t;
{$endif}    
        st_mode    : mode_t;
        st_nlink   : nlink_t;
        st_uid     : uid_t;
        st_gid     : gid_t;
        st_rdev    : dev_t;
        pad2_       : word;
        st_size    : off_t;
        st_blksize : blksize_t;
{$ifdef fs32bit}
        st_blocks  : blkcnt_t;
{$else}    
        st_blocks  : blkcnt64_t;
{$endif}    
        st_atime,
        st_atime_nsec,
        st_mtime,
        st_mtime_nsec,
        st_ctime,
        st_ctime_nsec : culong;
{$ifdef fs32bit}
        unused4_, unused5_: culong;
{$else}    
        st_ino     : ino64_t;
{$endif});
  end;


{$endif}

