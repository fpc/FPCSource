{
    Copyright (c) 1998-2022 by the Free Pascal development team

    Raspberry Pi 3 startup code for aarch64-embedded targets

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
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************
}
{$IFNDEF FPC_DOTTEDUNITS}
unit raspi3;
{$ENDIF FPC_DOTTEDUNITS}

{$goto on}
{$INLINE ON}

interface

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses
    EmbeddedApi.ConsoleIO, EmbeddedApi.mmio, EmbeddedApi.mailbox, EmbeddedApi.raspiuart, EmbeddedApi.gpio;
{$ELSE FPC_DOTTEDUNITS}
uses
    consoleio, mmio, mailbox, raspiuart, gpio;
{$ENDIF FPC_DOTTEDUNITS}

procedure _FPC_haltproc; assembler; nostackframe; public name '_haltproc';
asm
.Lhalt:
    wfe
    b .Lhalt
end;

function RaspiWrite(ACh: AnsiChar; AUserData: pointer): boolean;
begin
    UARTPuts(PeripheralBase, ACh);

    RaspiWrite := true;
end;

function RaspiRead(var ACh: AnsiChar; AUserData: pointer): boolean;
begin
    ACh := UARTGet(PeripheralBase);

    RaspiRead := true;
end;

{$ifndef CUSTOM_ENTRY}
procedure PASCALMAIN; external name 'PASCALMAIN';

var
    _stack_top: record end; external name '_stack_top';
    _bss_start: record end; external name '_bss_start';
    _bss_size: record end; external name '_bss_size';

{ This start makes sure we only execute on core 0 - the others will halt }
procedure _FPC_start; assembler; nostackframe;
label
    _start;
asm
    .init
    .align  16
    .globl  _start
_start:
    // find core, only enable 0
    mrs     x1, mpidr_el1
    and     x1, x1, #3
    // cpu id > 0, stop
    cbz     x1, .L2
.L1:
    wfe
    b      .L1

.L2:
    // set up the stack
    ldr    x0, .L_stack_top
    mov    sp, x0

    // clear BSS here
    ldr    x1, .L_bss_start
    ldr    w2, .L_bss_size
.L3:
    cbz    w2, .L4
    str    xzr, [x1], #8
    sub    w2, w2, #1
    cbnz   w2, .L3

.L4:
    bl     PASCALMAIN
    bl     _FPC_haltproc
.L_stack_top:
    .word  _stack_top
.L_bss_start:
    .word  _bss_start
.L_bss_size:
    .word  _bss_size
    .text
end;
{$endif CUSTOM_ENTRY}

begin
    UARTInit(PeripheralBase);

    OpenIO(Input, @RaspiWrite, @RaspiRead, fmInput, nil);
    OpenIO(Output, @RaspiWrite, @RaspiRead, fmOutput, nil);
    OpenIO(ErrOutput, @RaspiWrite, @RaspiRead, fmOutput, nil);
    OpenIO(StdOut, @RaspiWrite, @RaspiRead, fmOutput, nil);
    OpenIO(StdErr, @RaspiWrite, @RaspiRead, fmOutput, nil);
end.
