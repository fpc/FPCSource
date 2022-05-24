{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2022 by Nikolay Nikolov

    This unit contains some WebAssembly-specific routines

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit WebAssembly;

interface

procedure AtomicFence; inline;

implementation

{$I cpuh.inc}

procedure AtomicFence; inline;
begin
  fpc_wasm32_atomic_fence;
end;

end.
