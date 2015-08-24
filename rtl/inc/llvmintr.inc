{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2015 Jonas Maebe

    This file contains declarations for LLVM intrinsics

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


procedure llvm_memcpy64(dest, source: pointer; len: qword; align: cardinal; isvolatile: boolean); compilerproc; external name 'llvm.memcpy.p0i8.p0i8.i64';

