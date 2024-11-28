{
    This file is part of the Free Component Library

    Webassembly Timer API - shared info with pas2js hosting implementation.
    Copyright (c) 2024 by Michael Van Canneyt michael@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit wasm.timer.shared;

{$mode ObjFPC}{$H+}

interface

Type
  TWasmTimerID = Longint;

const
  TimerExportName  = 'timer';
  TimerFN_Allocate = 'allocate_timer';
  TimerFN_DeAllocate = 'deallocate_timer';


implementation

end.

