{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

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
   cgbase,cgobj,aasm,cpuasm,cpubase,cpuinfo;

type
pcgalpha = ^tcgalpha;
tcgalpha = object(tcg)
  procedure a_push_reg(list : paasmoutput;r : tregister);virtual;
  procedure a_call_name(list : paasmoutput;const s : string;
    offset : longint);virtual;
  procedure a_load_const_reg(list : paasmoutput;size : tcgsize;a : aword;register : tregister);virtual;
  procedure a_load_reg_ref(list : paasmoutput;size : tcgsize;register : tregister;const ref : treference);virtual;
  procedure a_load_ref_reg(list : paasmoutput;size : tcgsize;const ref : treference;register : tregister);virtual;
  procedure a_load_reg_reg(list : paasmoutput;size : tcgsize;reg1,reg2 : tregister);virtual;
  procedure a_cmp_reg_const_label(list : paasmoutput;size : tcgsize;cmp_op : topcmp;a : aword;
    reg : tregister;  l : pasmlabel);virtual;
  procedure a_cmp_reg_reg_label(list : paasmoutput;size : tcgsize;cmp_op : topcmp;reg1,reg2 : tregister;l : pasmlabel);
  procedure a_cmp_reg_ref_label(list : paasmoutput;size : tcgsize;cmp_op : topcmp;reg : tregister;l : pasmlabel);
  procedure a_cmp_ref_const_label(list : paasmoutput;size : tcgsize;cmp_op : topcmp;a : aword;
    reg : tregister; l : pasmlabel);
  procedure a_loadaddress_ref_reg(list : paasmoutput;const ref : treference;r : tregister);virtual;
  procedure g_stackframe_entry(list : paasmoutput;localsize : longint);virtual;
  procedure g_maybe_loadself(list : paasmoutput);virtual;
  procedure g_restore_frame_pointer(list : paasmoutput);virtual;
  procedure g_push_exception_value_reg(list : paasmoutput;reg : tregister);virtual;
  procedure g_push_exception_value_const(list : paasmoutput;reg : tregister);virtual;
  procedure g_pop_exception_value_reg(list : paasmoutput;reg : tregister);virtual;
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
    concat(new(paicpu,op_reg_ref(A_LDGP,Global_pointer,new_reference(R_27,0))));
    concat(new(paicpu,op_reg_ref(A_LDA,Stack_Pointer,new_reference(Stack_pointer,-LocalSize))));
    If LocalSize<>0 then
      concat(new(paiframe,Init(Global_pointer,LocalSize,R_27,0)));
    { Always generate a frame pointer. }
    concat(new(paicpu,op_reg_reg_reg(A_BIS,Stack_pointer,Stack_pointer,Frame_pointer)))
    end;
end;

procedure g_exitcode(list : paasmoutput;parasize : longint; nostackframe,inlined : boolean);

begin
  With List^ do
    begin
    { Restore stack pointer from frame pointer }
    Concat (new(paicpu,op_reg_reg_reg(A_BIS,Frame_Pointer,Frame_Pointer,Stack_Pointer)));
    { Restore previous stack position}
    Concat (new(paicpu,op_reg_const_reg(A_ADDQ,Stack_Pointer,Parasize,Stack_pointer)));
    { return... }
    Concat (new(paicpu,op_reg_ref_const(A_RET,Stack_pointer,new_reference(Return_pointer,0),1)));
    { end directive
    Concat (new(paiend,init(''));
    }
    end;
end;

procedure tcgalpha.a_call_name(list : paasmoutput;const s : string;  offset : longint);

  begin
     { list^.concat(new(paicpu,op_sym(A_CALL,S_NO,newasmsymbol(s)))); }
     {!!!!!!!!!1 offset is ignored }
     abstract;
  end;

procedure tcgalpha.a_push_reg(list : paasmoutput;r : tregister);

  begin
     { list^.concat(new(paicpu,op_reg(A_PUSH,regsize(r),r))); }
     abstract;
  end;


procedure tcgalpha.a_load_const_reg(list : paasmoutput;size : tcgsize;a : aword;register : tregister);

begin
end;


procedure tcgalpha.a_load_reg_ref(list : paasmoutput;size : tcgsize;register : tregister;const ref : treference);

begin
end;


procedure tcgalpha.a_load_ref_reg(list : paasmoutput;size : tcgsize;const ref : treference;register : tregister);

begin
end;


procedure tcgalpha.a_load_reg_reg(list : paasmoutput;size : tcgsize;reg1,reg2 : tregister);

begin
end;


procedure tcgalpha.a_cmp_reg_const_label(list : paasmoutput;size : tcgsize;cmp_op : topcmp;a : aword;reg : tregister;
  l : pasmlabel);

begin
end;


procedure tcgalpha.a_cmp_reg_reg_label(list : paasmoutput;size : tcgsize;cmp_op : topcmp;reg1,reg2 : tregister;l : pasmlabel);

begin
end;


procedure tcgalpha.a_cmp_reg_ref_label(list : paasmoutput;size : tcgsize;cmp_op : topcmp;reg : tregister;l : pasmlabel);

begin
end;


procedure tcgalpha.a_cmp_ref_const_label(list : paasmoutput;size : tcgsize;cmp_op : topcmp;a : aword;
  reg : tregister; l : pasmlabel);

begin
end;


procedure tcgalpha.a_loadaddress_ref_reg(list : paasmoutput;const ref : treference;r : tregister);

begin
end;


procedure tcgalpha.g_maybe_loadself(list : paasmoutput);

begin
end;


procedure tcgalpha.g_restore_frame_pointer(list : paasmoutput);

begin
end;


procedure tcgalpha.g_push_exception_value_reg(list : paasmoutput;reg : tregister);

begin
end;


procedure tcgalpha.g_push_exception_value_const(list : paasmoutput;reg : tregister);

begin
end;


procedure tcgalpha.g_pop_exception_value_reg(list : paasmoutput;reg : tregister);

begin
end;


end.
{
  $Log$
  Revision 1.2  2002-09-07 15:25:10  peter
    * old logs removed and tabs fixed

  Revision 1.1  2002/08/18 09:06:54  florian
    * alpha files moved compiler/alpha

}
