{
    $Id$
    Copyright (c) 1993-98 by Florian Klaempfl

    This unit implements the code generator for the DEC Alpha

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
unit cgcpu;

  interface

    uses
       cgobj,aasm,cpuasm,cpubase;

    type
       pcgalpha = ^tcgalpha;

       tcgalpha = object(tcg)
          procedure a_push_reg(list : paasmoutput;r : tregister);virtual;
          procedure a_call_name(list : paasmoutput;const s : string;
            offset : longint);virtual;

          procedure a_load_const8_ref(list : paasmoutput;b : byte;const ref : treference);virtual;
          procedure a_load_const16_ref(list : paasmoutput;w : word;const ref : treference);virtual;
          procedure a_load_const32_ref(list : paasmoutput;l : longint;const ref : treference);virtual;
          procedure a_load_const64_ref(list : paasmoutput;q : qword;const ref : treference);virtual;

          procedure g_stackframe_entry(list : paasmoutput;localsize : longint);virtual;
          constructor init;
       end;

  implementation

    uses
       globtype,globals;

    constructor tcgalpha.init;

      begin
         inherited init;
      end;

    procedure tcgalpha.g_stackframe_entry(list : paasmoutput;localsize : longint);

      begin
         {
         if localsize<>0 then
           begin
              if (cs_littlesize in aktglobalswitches) and (localsize<=65535) then
                list^.insert(new(pai386,op_const_const(A_ENTER,S_NO,localsize,0)))
              else
                begin
                   list^.concat(new(pai386,op_reg(A_PUSH,S_L,R_EBP)));
                   list^.concat(new(pai386,op_reg_reg(A_MOV,S_L,R_ESP,R_EBP)));
                   list^.concat(new(pai386,op_const_reg(A_SUB,S_L,localsize,R_ESP)));
                end;
             end
         else
           begin
              list^.concat(new(pai386,op_reg(A_PUSH,S_L,R_EBP)));
              list^.concat(new(pai386,op_reg_reg(A_MOV,S_L,R_ESP,R_EBP)));
           end;
         }
         abstract;
       end;

     procedure tcgalpha.a_call_name(list : paasmoutput;const s : string;
       offset : longint);

       begin
          { list^.concat(new(pai386,op_sym(A_CALL,S_NO,newasmsymbol(s)))); }
          {!!!!!!!!!1 offset is ignored }
          abstract;
       end;

     procedure tcgalpha.a_push_reg(list : paasmoutput;r : tregister);

       begin
          { list^.concat(new(pai386,op_reg(A_PUSH,regsize(r),r))); }
          abstract;
       end;

     procedure tcgalpha.a_load_const8_ref(list : paasmoutput;b : byte;const ref : treference);

       begin
          abstract;
       end;

     procedure tcgalpha.a_load_const16_ref(list : paasmoutput;w : word;const ref : treference);

       begin
          abstract;
       end;

     procedure tcgalpha.a_load_const32_ref(list : paasmoutput;l : longint;const ref : treference);

       begin
          abstract;
       end;

     procedure tcgalpha.a_load_const64_ref(list : paasmoutput;q : qword;const ref : treference);

       begin
          abstract;
       end;

end.
{
  $Log$
  Revision 1.2  1999-08-04 00:24:00  florian
    * renamed i386asm and i386base to cpuasm and cpubase

  Revision 1.1  1999/08/03 22:39:46  florian
    * initial revision

  Revision 1.2  1999/08/01 23:19:59  florian
    + make a new makefile using the old compiler makefile

  Revision 1.1  1999/08/01 23:11:24  florian
    + renamed ot tp cgcpu.pas

  Revision 1.1  1999/08/01 22:08:26  florian
    * reorganisation of directory structure

  Revision 1.3  1999/08/01 18:22:31  florian
   * made it again compilable

  Revision 1.2  1999/01/23 23:29:43  florian
    * first running version of the new code generator
    * when compiling exceptions under Linux fixed

  Revision 1.1  1998/12/15 22:17:02  florian
    * first version

}
