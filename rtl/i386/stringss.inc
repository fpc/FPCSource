{
    $Id$
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


{$define FPC_UNIT_HAS_STRPAS}
function strpas(p : pchar) : string;
{$i strpas.inc}

{$define FPC_UNIT_HAS_STRPCOPY}
function strpcopy(d : pchar;const s : string) : pchar;assembler;
var
  saveesi,saveedi : longint;
asm
	movl	%edi,saveedi
	movl    %esi,saveesi
        cld
        movl    s,%esi          // Load Source adress
        movl    d,%edi          // load destination address
        movzbl  (%esi),%ecx        // load length in ECX
        incl    %esi
        rep
        movsb
        movb    $0,(%edi)
        movl    d,%eax       // return value to EAX
	movl	saveedi,%edi
	movl    saveesi,%esi
end;

{
  $Log$
  Revision 1.9  2003-09-08 18:21:37  peter
    * save edi,esi,ebx

  Revision 1.8  2003/07/07 20:22:05  peter
    * generic string routines added

  Revision 1.7  2002/09/07 16:01:19  peter
    * old logs removed and tabs fixed

}
