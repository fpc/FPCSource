{
    $Id$
    Copyright (c) 2002 by Florian Klaempfl

    This unit contains the CPU specific part of tprocinfo

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
unit cpupi;

{$i fpcdefs.inc}

interface

  uses
    cutils,
    cgbase,cpuinfo,
    psub;

  type
    TSparcProcInfo=class(tcgprocinfo)
    private
      maxpushedparasize : longint;
    public
      constructor create(aparent:tprocinfo);override;
      procedure allocate_push_parasize(size:longint);override;
      function calc_stackframe_size:longint;override;
    end;

implementation

    uses
      globtype,systems,
      tgobj,paramgr,symconst,symsym;

    constructor tsparcprocinfo.create(aparent:tprocinfo);
      begin
        inherited create(aparent);
        maxpushedparasize:=0;
      end;


    procedure tsparcprocinfo.allocate_push_parasize(size:longint);
      begin
        if size>maxpushedparasize then
          maxpushedparasize:=size;
      end;


    function TSparcProcInfo.calc_stackframe_size:longint;
      var
        savearea : longint;
      begin
        { ABI requires at least space to save 6 arguments }
        savearea:=procdef.parast.address_fixup+max(maxpushedparasize,6*4);
        {
          Stackframe layout:
          %fp
            <locals>
            <temp>
            <arguments for calling>
            <return pointer for calling>
            <register window save area for calling>
          %sp
        }
        result:=Align(tg.lasttemp+savearea,4);
      end;


begin
  cprocinfo:=TSparcProcInfo;
end.
{
  $Log$
  Revision 1.20  2003-09-03 15:55:01  peter
    * NEWRA branch merged

  Revision 1.19.2.1  2003/09/01 21:02:55  peter
    * sparc updates for new tregister

  Revision 1.19  2003/08/20 17:48:49  peter
    * fixed stackalloc to not allocate localst.datasize twice
    * order of stackalloc code fixed for implicit init/final

  Revision 1.18  2003/07/06 17:58:22  peter
    * framepointer fixes for sparc
    * parent framepointer code more generic

  Revision 1.17  2003/06/13 21:19:32  peter
    * current_procdef removed, use current_procinfo.procdef instead

  Revision 1.16  2003/05/30 23:57:08  peter
    * more sparc cleanup
    * accumulator removed, splitted in function_return_reg (called) and
      function_result_reg (caller)

  Revision 1.15  2003/05/23 22:33:48  florian
    * fix some small flaws which prevent sparc linux system unit from compiling
    * some reformatting done

  Revision 1.14  2003/04/27 11:21:36  peter
    * aktprocdef renamed to current_procinfo.procdef
    * procinfo renamed to current_procinfo
    * procinfo will now be stored in current_module so it can be
      cleaned up properly
    * gen_main_procsym changed to create_main_proc and release_main_proc
      to also generate a tprocinfo structure
    * fixed unit implicit initfinal

  Revision 1.13  2003/04/27 07:48:05  peter
    * updated for removed lexlevel

  Revision 1.12  2003/02/06 22:36:55  mazen
  * fixing bug related to errornous program main entry stack frame

  Revision 1.11  2003/01/05 21:32:35  mazen
  * fixing several bugs compiling the RTL

  Revision 1.10  2002/12/24 21:30:20  mazen
  - some writeln(s) removed in compiler
  + many files added to RTL
  * some errors fixed in RTL

  Revision 1.9  2002/12/21 23:21:47  mazen
  + added support for the shift nodes
  + added debug output on screen with -an command line option

  Revision 1.8  2002/11/17 17:49:09  mazen
  + return_result_reg and FUNCTION_RESULT_REG are now used, in all plateforms, to pass functions result between called function and its caller. See the explanation of each one

  Revision 1.7  2002/11/14 21:42:08  mazen
  * fixing return value variable address

  Revision 1.6  2002/11/10 19:07:46  mazen
  * SPARC calling mechanism almost OK (as in GCC./mppcsparc )

  Revision 1.5  2002/11/03 20:22:40  mazen
  * parameter handling updated

  Revision 1.4  2002/10/20 19:01:38  mazen
  + op_raddr_reg and op_caddr_reg added to fix functions prologue

  Revision 1.3  2002/10/10 15:10:39  mazen
  * Internal error fixed, but usually i386 parameter model used

  Revision 1.2  2002/08/29 11:02:36  mazen
  added support for SPARC processors

  Revision 1.1  2002/08/23 10:08:28  mazen
  *** empty log message ***

  Revision 1.2  2002/08/18 20:06:30  peter
    * inlining is now also allowed in interface
    * renamed write/load to ppuwrite/ppuload
    * tnode storing in ppu
    * nld,ncon,nbas are already updated for storing in ppu

  Revision 1.1  2002/08/17 09:23:49  florian
    * first part of procinfo rewrite
}
