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
       cgobj;

    type
       pcg386 = ^tcg386;

       tcg386 = object(tcg)
          procedure a_push_reg(list : paasmoutput;r : tregister);virtual;
          procedure a_call_name(list : paasmoutput;const s : string;
            offset : longint);virtual;

          procedure a_load_const8_ref(list : paasmoutput;b : byte;ref : treference);virtual;
          procedure a_load_const16_ref(list : paasmoutput;w : word;ref : treference);virtual;
          procedure a_load_const32_ref(list : paasmoutput;l : longint;ref : treference);virtual;
          procedure a_load_const64_ref(list : paasmoutput;{ q : qword; }ref : treference);virtual;

          procedure g_stackframe_entry(list : paasmoutput;localsize : longint);
       end;

  implementation

    procedure tcg386.g_stackframe_entry(list : paasmoutput;localsize : longint);

      begin
         if localsize<>0 then
           begin
              if (cs_littlesize in aktglobalswitches) and (stackframe<=65535) then
                list^.insert(new(pai386,op_const_const(A_ENTER,S_NO,stackframe,0)))
              else
                begin
                   g_stackframe_entry(list,stackframe);
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
          list^.concat(new(pai386,op_csymbol(A_CALL,S_NO,newcsymbol(s,offset))));
       end;

end.
{
  $Log$
  Revision 1.1  1998-12-15 22:17:02  florian
    * first version

}