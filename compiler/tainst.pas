{
    $Id$
    Copyright (c) 1998-2002 by Michael Van Canneyt

    Contains a generic assembler instruction object;

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
Unit tainst;

{$i fpcdefs.inc}

interface

    Uses
      cpuinfo,cpubase,aasm,cclasses;

    Type
      tairegalloc = class(tai)
         allocation : boolean;
         reg        : tregister;
         constructor alloc(r : tregister);
         constructor dealloc(r : tregister);
      end;

      taicpu_abstract = class(tai)
        condition : TAsmCond;
        ops       : longint;
        oper      : array[0..max_operands-1] of toper;
        opcode    : tasmop;
{$ifdef x86}
        segprefix : tregister;
{$endif x86}
        is_jmp    : boolean; { is this instruction a jump? (needed for optimizer) }
        Constructor Create(op : tasmop);
        Destructor Destroy;override;
        function getcopy:tlinkedlistitem;override;
        procedure loadconst(opidx:longint;l:aword);
        procedure loadsymbol(opidx:longint;s:tasmsymbol;sofs:longint);
        procedure loadref(opidx:longint;const r:treference);
        procedure loadreg(opidx:longint;r:tregister);
        procedure loadoper(opidx:longint;o:toper);
        procedure SetCondition(const c:TAsmCond);
      end;

      { alignment for operator }
      tai_align_abstract = class(tai)
         buf       : array[0..63] of char; { buf used for fill }
         aligntype : byte;   { 1 = no align, 2 = word align, 4 = dword align }
         fillsize  : byte;   { real size to fill }
         fillop    : byte;   { value to fill with - optional }
         use_op    : boolean;
         constructor Create(b:byte);
         constructor Create_op(b: byte; _op: byte);
         function getfillbuf:pchar;virtual;
      end;


implementation

    uses
      verbose;


{*****************************************************************************
                                 TaiRegAlloc
*****************************************************************************}

    constructor tairegalloc.alloc(r : tregister);
      begin
        inherited create;
        typ:=ait_regalloc;
        allocation:=true;
        reg:=r;
      end;


    constructor tairegalloc.dealloc(r : tregister);
      begin
        inherited create;
        typ:=ait_regalloc;
        allocation:=false;
        reg:=r;
      end;


{*****************************************************************************
                               TaiInstruction
*****************************************************************************}

    constructor taicpu_abstract.Create(op : tasmop);

      begin
         inherited create;
         typ:=ait_instruction;
         is_jmp:=false;
         opcode:=op;
         ops:=0;
         fillchar(condition,sizeof(condition),0);
         fillchar(oper,sizeof(oper),0);
      end;



    destructor taicpu_abstract.Destroy;

      var
        i : longint;
      begin
        for i:=0 to ops-1 do
        case oper[i].typ of
          top_ref:
            dispose(oper[i].ref);
          top_symbol:
            dec(tasmsymbol(oper[i].sym).refs);
        end;
        inherited destroy;
      end;



{ ---------------------------------------------------------------------
    Loading of operands.
  ---------------------------------------------------------------------}



    procedure taicpu_abstract.loadconst(opidx:longint;l:aword);
      begin
        if opidx>=ops then
         ops:=opidx+1;
        with oper[opidx] do
         begin
           if typ=top_ref then
            dispose(ref);
           val:=l;
           typ:=top_const;
         end;
      end;



    procedure taicpu_abstract.loadsymbol(opidx:longint;s:tasmsymbol;sofs:longint);
      begin
        if not assigned(s) then
         internalerror(200204251);
        if opidx>=ops then
         ops:=opidx+1;
        with oper[opidx] do
         begin
           if typ=top_ref then
            dispose(ref);
           sym:=s;
           symofs:=sofs;
           typ:=top_symbol;
         end;
        inc(s.refs);
      end;



    procedure taicpu_abstract.loadref(opidx:longint;const r:treference);
      begin
        if opidx>=ops then
         ops:=opidx+1;
        with oper[opidx] do
         begin
           if typ<>top_ref then
            new(ref);
           ref^:=r;
{$ifdef x86}
           { We allow this exception for i386, since overloading this would be
             too much of a a speed penalty}
           if not(ref^.segment in [R_DS,R_NO]) then
            segprefix:=ref^.segment;
{$endif x86}
           typ:=top_ref;
           { mark symbol as used }
           if assigned(ref^.symbol) then
             inc(ref^.symbol.refs);
         end;
      end;



    procedure taicpu_abstract.loadreg(opidx:longint;r:tregister);
      begin
        if opidx>=ops then
         ops:=opidx+1;
        with oper[opidx] do
         begin
           if typ=top_ref then
            dispose(ref);
           reg:=r;
           typ:=top_reg;
         end;
      end;



    procedure taicpu_abstract.loadoper(opidx:longint;o:toper);
      begin
        if opidx>=ops then
         ops:=opidx+1;
        if oper[opidx].typ=top_ref then
         dispose(oper[opidx].ref);
        oper[opidx]:=o;
        { copy also the reference }
        if oper[opidx].typ=top_ref then
         begin
           new(oper[opidx].ref);
           oper[opidx].ref^:=o.ref^;
         end;
      end;


{ ---------------------------------------------------------------------
    Miscellaneous methods.
  ---------------------------------------------------------------------}

    procedure taicpu_abstract.SetCondition(const c:TAsmCond);
      begin
         condition:=c;
      end;


    Function taicpu_abstract.getcopy:tlinkedlistitem;
      var
        i : longint;
        p : tlinkedlistitem;
      begin
        p:=inherited getcopy;
        { make a copy of the references }
        for i:=1 to ops do
         if (taicpu_abstract(p).oper[i-1].typ=top_ref) then
          begin
            new(taicpu_abstract(p).oper[i-1].ref);
            taicpu_abstract(p).oper[i-1].ref^:=oper[i-1].ref^;
          end;
        getcopy:=p;
      end;

{****************************************************************************
                              tai_align_abstract
 ****************************************************************************}

     constructor tai_align_abstract.Create(b: byte);
       begin
          inherited Create;
          typ:=ait_align;
          if b in [1,2,4,8,16,32] then
            aligntype := b
          else
            aligntype := 1;
          fillsize:=0;
          fillop:=0;
          use_op:=false;
       end;


     constructor tai_align_abstract.Create_op(b: byte; _op: byte);
       begin
          inherited Create;
          typ:=ait_align;
          if b in [1,2,4,8,16,32] then
            aligntype := b
          else
            aligntype := 1;
          fillsize:=0;
          fillop:=_op;
          use_op:=true;
          fillchar(buf,sizeof(buf),_op)
       end;


     function tai_align_abstract.getfillbuf:pchar;
       begin
         getfillbuf:=@buf;
       end;

end.

{
  $Log$
  Revision 1.11  2002-07-04 20:43:02  florian
    * first x86-64 patches

  Revision 1.10  2002/07/01 18:46:29  peter
    * internal linker
    * reorganized aasm layer

  Revision 1.9  2002/05/18 13:34:21  peter
    * readded missing revisions

  Revision 1.7  2002/05/14 19:34:52  peter
    * removed old logs and updated copyright year

  Revision 1.6  2002/05/14 17:28:09  peter
    * synchronized cpubase between powerpc and i386
    * moved more tables from cpubase to cpuasm
    * tai_align_abstract moved to tainst, cpuasm must define
      the tai_align class now, which may be empty

  Revision 1.5  2002/04/25 20:16:39  peter
    * moved more routines from cga/n386util

  Revision 1.4  2002/04/02 17:11:32  peter
    * tlocation,treference update
    * LOC_CONSTANT added for better constant handling
    * secondadd splitted in multiple routines
    * location_force_reg added for loading a location to a register
      of a specified size
    * secondassignment parses now first the right and then the left node
      (this is compatible with Kylix). This saves a lot of push/pop especially
      with string operations
    * adapted some routines to use the new cg methods

}