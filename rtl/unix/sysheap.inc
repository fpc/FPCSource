{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2001 by Free Pascal development team

    This file implements all the base types and limits required
    for a minimal POSIX compliant subset required to port the compiler
    to a new OS.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$ifndef HAS_SYSOSALLOC}
{$define HAS_SYSOSALLOC}

function SysOSAlloc(size: ptruint): pointer;
begin
  result:=Fpmmap(nil,Size,3,MAP_PRIVATE+MAP_ANONYMOUS,-1,0);
  if result=pointer(-1) then
    result:=nil
  else
    seterrno(0);
end;
{$endif HAS_SYSOSALLOC}


{$ifndef HAS_SYSOSFREE}
{$define HAS_SYSOSFREE}

procedure SysOSFree(p: pointer; size: ptruint);
begin
  fpmunmap(p, size);
end;
{$endif HAS_SYSOSFREE}

{$ifdef FPC_SYSTEM_HAS_MREMAP}
const
  MREMAP_MAYMOVE = 1;

{$define FPC_SYSTEM_HAS_SYSOSREALLOC}
function SysOSRealloc(p: pointer;oldsize,newsize: ptruint): pointer;
begin
  result:=Fpmremap(p,oldsize,newsize,MREMAP_MAYMOVE,nil);
  if result=pointer(-1) then
    result:=nil
  else
    seterrno(0);
end;

{$endif FPC_SYSTEM_HAS_MREMAP}
