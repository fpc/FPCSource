{*****************************************************************************
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

 ****************************************************************************}
{ This unit contains the CPU specific part of tprocinfo. }
unit cpupi;
{$INCLUDE fpcdefs.inc}
interface
uses
        cutils,
        cgbase,cpuinfo;
type
  TSparcProcInfo=class(TProcInfo)
    {overall size of allocated stack space, currently this is used for the
    PowerPC only}
    LocalSize:aword;
    {max of space need for parameters, currently used by the PowerPC port only}
    maxpushedparasize:aword;
    constructor create(aparent:tprocinfo);override;
{According the the SPARC ABI the standard stack frame must include :
  *  16 word save for the in and local registers in case of overflow/underflow.
this save area always must exist at the %o6+0,
  *  software conventions requires space for the aggregate return value pointer, even if the word is not used,
  *  althogh the first six words of arguments reside in registers, the standard
stack frame reserves space for them. Arguments beond the sixth reside on the
stack as in the Intel architecture,
  * other areas depend on the compiler and the code being compiled. The
standard calling sequence does not define a maximum stack frame size, nor does
it restrict how a language system uses the "unspecified" areas of the standard
stack frame.}
    procedure after_header;override;
    procedure after_pass1;override;
  end;
implementation
uses
        tgobj,paramgr,symsym,systems;

constructor TSparcprocinfo.create(aparent:tprocinfo);
        begin
                inherited create(aparent);
                maxpushedparasize:=0;
                LocalSize:=(16+1)*4;
        {First 16 words are in the frame are used to save registers in case of a
    register overflow/underflow.The 17th word is used to save the address of
    the variable which will receive the return value of the called function}
//    Return_Offset:=16*4;
        end;

    procedure TSparcprocinfo.after_header;
      begin
        { this value is necessary for nested procedures }
        if assigned(procdef.localst) then
          procdef.localst.address_fixup:=align(procdef.parast.address_fixup+procdef.parast.datasize,16);
      end;

procedure TSparcProcInfo.after_pass1;
        begin
    with ProcDef do
      begin
            {Reserve the stack for copying parameters passed into registers. By
        default we reserve space for the 6 input registers if the function had
        less parameters. Otherwise, we allocate data sizeof parameters}
        if parast.datasize>6*4
        then
          localst.address_fixup:=parast.address_fixup+parast.datasize
        else
          procdef.localst.address_fixup:=parast.address_fixup+6*4;
                    firsttemp_offset:=localst.address_fixup+localst.datasize;
        with tg do
          begin
                        SetFirstTemp(firsttemp_offset);
                        //LastTemp:=firsttemp_offset;
          end;
      end;
        end;
begin
  cprocinfo:=TSparcProcInfo;
end.
{
  $Log$
  Revision 1.14  2003-04-27 11:21:36  peter
    * aktprocdef renamed to current_procdef
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
  + return_result_reg and function_result_reg are now used, in all plateforms, to pass functions result between called function and its caller. See the explanation of each one

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
