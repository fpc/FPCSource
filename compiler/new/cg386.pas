{
    $Id$
    Copyright (c) 1993-98 by Florian Klaempfl

    This unit implements the code generator for the i386

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
unit cg386;

  interface

    uses
       cgobj,aasm
{$i cpuunit.inc}
       ;

    type
       pcg386 = ^tcg386;

       tcg386 = object(tcg)
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

    constructor tcg386.init;

      begin
         inherited init;
      end;

    procedure tcg386.g_stackframe_entry(list : paasmoutput;localsize : longint);

      begin
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
       end;

     procedure tcg386.a_call_name(list : paasmoutput;const s : string;
       offset : longint);

       begin
          list^.concat(new(pai386,op_sym(A_CALL,S_NO,newasmsymbol(s))));
          {!!!!!!!!!1 offset is ignored }
       end;

     procedure tcg386.a_push_reg(list : paasmoutput;r : tregister);

       begin
          list^.concat(new(pai386,op_reg(A_PUSH,regsize(r),r)));
       end;

     procedure tcg386.a_load_const8_ref(list : paasmoutput;b : byte;const ref : treference);

       begin
          abstract;
       end;

     procedure tcg386.a_load_const16_ref(list : paasmoutput;w : word;const ref : treference);

       begin
          abstract;
       end;

     procedure tcg386.a_load_const32_ref(list : paasmoutput;l : longint;const ref : treference);

       begin
          abstract;
       end;

     procedure tcg386.a_load_const64_ref(list : paasmoutput;q : qword;const ref : treference);

       begin
          abstract;
       end;

end.
{
  $Log$
  Revision 1.3  1999-08-01 18:22:31  florian
   * made it again compilable

  Revision 1.2  1999/01/23 23:29:43  florian
    * first running version of the new code generator
    * when compiling exceptions under Linux fixed

  Revision 1.1  1998/12/15 22:17:02  florian
    * first version

}