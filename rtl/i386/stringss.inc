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

function strpas(p : pchar) : string;
{$i strpas.inc}

function strpcopy(d : pchar;const s : string) : pchar;assembler;
asm
        pushl   %esi            // Save ESI
        cld
        movl    s,%esi          // Load Source adress
        movl    d,%edi          // load destination address
        movzbl  (%esi),%ecx        // load length in ECX
        incl    %esi
        rep
        movsb
        movb    $0,(%edi)
        movl    d,%eax       // return value to EAX
        popl    %esi
end ['EDI','EAX','ECX'];

{
  $Log$
  Revision 1.6  2001-03-05 17:10:04  jonas
    * moved implementations of strlen and strpas to separate include files
      (they were duplicated in i386.inc and strings.inc/stringss.inc)
    * strpas supports 'nil' pchars again (returns an empty string)
      (both merged)

  Revision 1.5  2001/03/04 17:31:35  jonas
    * fixed all implementations of strpas

  Revision 1.4  2001/03/04 12:42:18  jonas
    * fixed strpas (was limited to 254 chars) and made it overall slightly faster

  Revision 1.3  2001/02/10 16:08:46  jonas
    * fixed non-working alignment code

  Revision 1.2  2000/07/13 11:33:42  michael
  + removed logs

}
