{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    Processor dependent part of strings.pp, not shared with
    sysutils unit.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{$ifndef FPC_UNIT_HAS_STRPCOPY}
{$define FPC_UNIT_HAS_STRPCOPY}
function strpcopy(d : pchar;const s : string) : pchar;assembler;
var
  saveesi,saveedi : longint;
asm
        movl    %edi,saveedi
        movl    %esi,saveesi
        cld
{$ifdef REGCALL}
        movl    %eax,%edi          // load destination address
        movl    %edx,%esi          // Load Source adress
{$else}
        movl    s,%esi          // Load Source adress
        movl    d,%edi          // load destination address
{$endif}
        movzbl  (%esi),%ecx        // load length in ECX
        incl    %esi
        rep
        movsb
        movb    $0,(%edi)
{$ifndef REGCALL}
        movl    d,%eax       // return value to EAX
{$endif}
        movl    saveedi,%edi
        movl    saveesi,%esi
end;
{$endif FPC_UNIT_HAS_STRPCOPY}

