{
    Copyright (c) 2002 by Carl Eric Codere


    Implements QNX system calls and types


    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.


    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.


    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
    MA 02110-1301, USA.


 ****************************************************************************
}
const
  _FSTYPSZ = 16;


type
  fsblkcnt_t = int64;
  fsfilcnt_t = int64;
  
  statvfs_t = packed record
    f_bsize : cardinal;                {* fundamental file system block size *}
    f_frsize : cardinal;               {* fragment size *}
    f_blocks : fsblkcnt_t;             {* total blocks of f_frsize on fs *} 
    f_bfree : fsblkcnt_t;              {* total free blocks of f_frsize *}   
    f_bavail : fsblkcnt_t;             {* free blocks avail to non-superuser *}
    f_files : fsfilcnt_t;              {* total file nodes (inodes) *}
    f_free : fsfilcnt_t;               {* total free file nodes *}
    f_favail : fsfilcnt_t;             {* free nodes avail to non-superuser *}
    f_fsid :  cardinal;                {* file system id (dev for now) *}
    f_basetype : array[0.._FSTYPSZ-1] of char; {* target fs type name null terminated *}
    f_flag : cardinal;                 {* bit-mask of flags *} 
    f_namemax : cardinal;              {* maximum file name length *}
    f_filler : array[1..21] of cardinal; {* reserved for future expansion *} 
  end;  
  
function sys_statvfs(const path: pchar; var buf : statvfs_t): cint; cdecl; external name 'statvfs';  

