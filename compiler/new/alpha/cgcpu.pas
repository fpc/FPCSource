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
          procedure g_exite_entry(list : paasmoutput;localsize : longint);virtual;
          procedure g_exitcode(list : paasmoutput;parasize : longint;
            nostackframe,inlined : boolean);
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
      With List^ do 
        begin
        concat(new(paialpha,op_reg_ref(A_LDGP,Global_pointer,new_reference(R_27,0)));
        concat(new(paialpha,op_reg_ref(A_LDA,Stack_Pointer,new_reference(Stack_pointer,-LocalSize))));
        If LocalSize<>0 then
          concat(new(paiframe,Init(Global_pinter,LocalSize,R27,0)));
        // Always generate a frame pointer.
        concat(new(paiframe,op_reg_reg_reg(A_BIS,Stackpointer,Stack_pointer,Frame_pointer)))
        end;
    end;

    procedure g_exitcode(list : paasmoutput;parasize : longint; nostackframe,inlined : boolean);
 
    begin
      With List^ do
        begin
        // Restore stack pointer from frame pointer
        Concat (new(paialpha,op_reg_reg_reg(A_BIS,Frame_Pointer,Frame_Pointer,Stack_Pointer)));
        // Restore previous stack position
        Concat (new(paialpha,op_reg_const_reg(A_ADDQ,Stack_Pointer,Parasize,Stack_pointer)));
        // return...
        Concat (new(paialpha,op_reg_ref_const(A_RET,Stack_pointer,new_reference(Return_pointer,0),1)));
        // end directive
        Concat (new(paiend,init(''));
        end;
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
  Revision 1.3  1999-08-05 15:50:32  michael
  * more changes

  Revision 1.2  1999/08/04 00:24:00  florian
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
