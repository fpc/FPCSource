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

{
  $Log$
  Revision 1.13  2004-11-21 16:33:55  peter
    * external fixes

  Revision 1.12  2004/05/01 15:26:33  jonas
    * use some more string routines from libc if FPC_USE_LIBC is used

  Revision 1.11  2003/11/19 16:58:44  peter
    * make strpas assembler function

  Revision 1.10  2003/11/11 21:08:17  peter
    * REGCALL define added

  Revision 1.9  2003/09/08 18:21:37  peter
    * save edi,esi,ebx

  Revision 1.8  2003/07/07 20:22:05  peter
    * generic string routines added

  Revision 1.7  2002/09/07 16:01:19  peter
    * old logs removed and tabs fixed

}
