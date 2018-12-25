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


procedure llvm_memcpy64(dest, source: pointer; len: qword; align: cardinal; isvolatile: LLVMBool1); compilerproc; external name 'llvm.memcpy.p0i8.p0i8.i64';

function llvm_frameaddress(level: longint): pointer; compilerproc; external name 'llvm.frameaddress';

procedure llvm_lifetime_start(size: int64; ptr: pointer); compilerproc; external name 'llvm.lifetime.start';
procedure llvm_lifetime_end(size: int64; ptr: pointer); compilerproc; external name 'llvm.lifetime.end';

function llvm_sqrt_f32(val: single): single; compilerproc; external name 'llvm.sqrt.f32';
function llvm_sqrt_f64(val: double): double; compilerproc; external name 'llvm.sqrt.f64';
{$ifdef SUPPORT_EXTENDED}
function llvm_sqrt_f80(val: extended): extended; compilerproc; external name 'llvm.sqrt.f80';
{$endif}
{$ifdef SUPPORT_FLOAT128}
function llvm_sqrt_f128(val: float128): float128; compilerproc; external name 'llvm.sqrt.f128';
{$endif}

function llvm_fma_f32(a, b, c: single): single; compilerproc; external name 'llvm.fma.f32';
function llvm_fma_f64(a, b, c: double): double; compilerproc; external name 'llvm.fma.f64';
{$ifdef SUPPORT_EXTENDED}
function llvm_fma_f80(a, b, c: extended): extended; compilerproc; external name 'llvm.fma.f80';
{$endif}
{$ifdef SUPPORT_FLOAT128}
function llvm_fma_f128(a, b, c: float128): float128; compilerproc; external name 'llvm.fma.f128';
{$endif}
