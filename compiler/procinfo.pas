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
      cpubase,cpuinfo,cgbase,cgutils,
      aasmbase,aasmtai
      ;

    const
      inherited_inlining_flags : tprocinfoflags = [pi_do_call];


    type
       {# This object gives information on the current routine being
          compiled.
       }
       tprocinfo = class(tlinkedlistitem)
          { pointer to parent in nested procedures }
          parent : tprocinfo;
          {# the definition of the routine itself }
          procdef : tprocdef;
          { procinfo of the main procedure that is inlining
            the current function, only used in tcgcallnode.inlined_pass2 }
          inlining_procinfo : tprocinfo;
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

          { Offset of temp after para/local are allocated }
          tempstart : longint;

          {# some collected informations about the procedure
             see pi_xxxx constants above
          }
          flags : tprocinfoflags;

          { register used as frame pointer }
          framepointer : tregister;

          { register containing currently the got }
          got : tregister;
          gotlabel : tasmlabel;

          { Holds the reference used to store all saved registers. }
          save_regs_ref : treference;

          { label to leave the sub routine }
          aktexitlabel : tasmlabel;

          {# The code for the routine itself, excluding entry and
             exit code. This is a linked list of tai classes.
          }
          aktproccode : taasmoutput;
          { Data (like jump tables) that belongs to this routine }
          aktlocaldata : taasmoutput;

          { max. of space need for parameters }
          maxpushedparasize : aint;

          constructor create(aparent:tprocinfo);virtual;
          destructor destroy;override;

          procedure allocate_push_parasize(size:longint);virtual;

          function calc_stackframe_size:longint;virtual;

          { Set the address of the first temp, can be used to allocate
            space for pushing parameters }
          procedure set_first_temp_offset;virtual;

          { Generate parameter information }
          procedure generate_parameter_info;virtual;
       end;
       tcprocinfo = class of tprocinfo;

    var
       cprocinfo : tcprocinfo;
       { information about the current sub routine being parsed (@var(pprocinfo))}
       current_procinfo : tprocinfo;


implementation

     uses
        cutils,systems,
        tgobj,cgobj,
        paramgr
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
        maxpushedparasize:=0;
        { asmlists }
        aktproccode:=Taasmoutput.Create;
        aktlocaldata:=Taasmoutput.Create;
        reference_reset(save_regs_ref);
        { labels }
        objectlibrary.getlabel(aktexitlabel);
        objectlibrary.getlabel(gotlabel);
      end;


    destructor tprocinfo.destroy;
      begin
         aktproccode.free;
         aktlocaldata.free;
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
  Revision 1.17  2004-10-31 21:45:03  peter
    * generic tlocation
    * move tlocation to cgutils

  Revision 1.16  2004/06/20 08:55:30  florian
    * logs truncated

  Revision 1.15  2004/06/16 20:07:09  florian
    * dwarf branch merged

  Revision 1.14.2.2  2004/05/03 20:18:52  peter
    * fixes for tprintf

  Revision 1.14.2.1  2004/04/27 18:18:26  peter
    * aword -> aint

  Revision 1.14  2004/03/02 17:32:12  florian
    * make cycle fixed
    + pic support for darwin
    + support of importing vars from shared libs on darwin implemented

  Revision 1.13  2004/02/27 10:21:05  florian
    * top_symbol killed
    + refaddr to treference added
    + refsymbol to treference added
    * top_local stuff moved to an extra record to save memory
    + aint introduced
    * tppufile.get/putint64/aint implemented

}
