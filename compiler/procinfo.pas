{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

    Information about the current procedure that is being compiled

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
unit procinfo;

{$i fpcdefs.inc}

  interface

    uses
      { common }
      cclasses,
      { global }
      globtype,globals,verbose,
      { symtable }
      symconst,symtype,symdef,symsym,
      { aasm }
      cpubase,cpuinfo,cgbase,
      aasmbase,aasmtai
      ;


    type
      tprocinfoflag=(
        {# procedure uses asm }
        pi_uses_asm,
        {# procedure does a call }
        pi_do_call,
        {# procedure has a try statement = no register optimization }
        pi_uses_exceptions,
        {# procedure is declared as @var(assembler), don't optimize}
        pi_is_assembler,
        {# procedure contains data which needs to be finalized }
        pi_needs_implicit_finally
      );
      tprocinfoflags=set of tprocinfoflag;

    type
       {# This object gives information on the current routine being
          compiled.
       }
       tprocinfo = class(tlinkedlistitem)
          { pointer to parent in nested procedures }
          parent : tprocinfo;
          {# the definition of the routine itself }
          procdef : tprocdef;
          { file location of begin of procedure }
          entrypos  : tfileposinfo;
          { file location of end of procedure }
          exitpos   : tfileposinfo;
          { local switches at begin of procedure }
          entryswitches : tlocalswitches;
          { local switches at end of procedure }
          exitswitches  : tlocalswitches;

          { Size of the parameters on the stack }
          para_stack_size : longint;

          {# some collected informations about the procedure
             see pi_xxxx constants above
          }
          flags : tprocinfoflags;

          { register used as frame pointer }
          framepointer : tregister;

          { Holds the reference used to store alll saved registers. }
          save_regs_ref : treference;

          { label to leave the sub routine }
          aktexitlabel : tasmlabel;

          {# The code for the routine itself, excluding entry and
             exit code. This is a linked list of tai classes.
          }
          aktproccode : taasmoutput;
          { Data (like jump tables) that belongs to this routine }
          aktlocaldata : taasmoutput;

          constructor create(aparent:tprocinfo);virtual;
          destructor destroy;override;

          { Allocate framepointer so it can not be used by the
            register allocator }
          procedure allocate_framepointer_reg;virtual;

          procedure allocate_push_parasize(size:longint);virtual;

          function calc_stackframe_size:longint;virtual;

          { Set the address of the first temp, can be used to allocate
            space for pushing parameters }
          procedure set_first_temp_offset;virtual;

          { Generate parameter information }
          procedure generate_parameter_info;virtual;

          { This procedure is called after the pass 1 of the subroutine body is done.
            Here the address fix ups to generate code for the body must be done.
          }
          {procedure after_pass1;virtual;}
       end;

       pregvarinfo = ^tregvarinfo;
       tregvarinfo = record
          regvars : array[1..maxvarregs] of tvarsym;
          regvars_para : array[1..maxvarregs] of boolean;
          regvars_refs : array[1..maxvarregs] of longint;

          fpuregvars : array[1..maxfpuvarregs] of tvarsym;
          fpuregvars_para : array[1..maxfpuvarregs] of boolean;
          fpuregvars_refs : array[1..maxfpuvarregs] of longint;
       end;

       tcprocinfo = class of tprocinfo;

    var
       cprocinfo : tcprocinfo;
       { information about the current sub routine being parsed (@var(pprocinfo))}
       current_procinfo : tprocinfo;

       { save the size of pushed parameter, needed for aligning }
       pushedparasize : longint;


implementation

     uses
        cutils,systems,
        cresstr,
        tgobj,rgobj,
        defutil,
        fmodule
        ,symbase,paramgr
        ;


{****************************************************************************
                                 TProcInfo
****************************************************************************}

    constructor tprocinfo.create(aparent:tprocinfo);
      begin
        parent:=aparent;
        procdef:=nil;
        para_stack_size:=0;
        flags:=[];
        framepointer:=NR_FRAME_POINTER_REG;
        { asmlists }
        aktproccode:=Taasmoutput.Create;
        aktlocaldata:=Taasmoutput.Create;
        reference_reset(save_regs_ref);
        { labels }
        objectlibrary.getlabel(aktexitlabel);
      end;


    destructor tprocinfo.destroy;
      begin
         aktproccode.free;
         aktlocaldata.free;
      end;


    procedure tprocinfo.allocate_framepointer_reg;
      begin
        if framepointer=NR_FRAME_POINTER_REG then
          begin
            { Make sure the register allocator won't allocate registers
              into ebp }
            include(rg.used_in_proc_int,RS_FRAME_POINTER_REG);
            exclude(rg.unusedregsint,RS_FRAME_POINTER_REG);
          end;
      end;


    procedure tprocinfo.allocate_push_parasize(size:longint);
      begin
      end;


    function tprocinfo.calc_stackframe_size:longint;
      begin
        result:=Align(tg.direction*tg.lasttemp,aktalignment.localalignmin);
      end;


    procedure tprocinfo.set_first_temp_offset;
      begin
      end;


    procedure tprocinfo.generate_parameter_info;
      begin
        { generate callee paraloc register info, it returns the size that
          is allocated on the stack }
        para_stack_size:=paramanager.create_paraloc_info(procdef,calleeside);
      end;


end.
{
  $Log$
  Revision 1.2  2003-10-03 22:00:33  peter
    * parameter alignment fixes

  Revision 1.1  2003/10/01 20:34:49  peter
    * procinfo unit contains tprocinfo
    * cginfo renamed to cgbase
    * moved cgmessage to verbose
    * fixed ppc and sparc compiles

}
