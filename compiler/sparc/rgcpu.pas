{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

    This unit implements the SPARC specific class for the register
    allocator

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
unit rgcpu;

{$i fpcdefs.inc}

interface

    uses
      cpubase,
      cpuinfo,
      aasmcpu,
      aasmtai,
      cclasses,globtype,
      cginfo,cgbase,aasmbase,rgobj;

    type
      trgcpu=class(trgobj)
{$ifndef NEWRA}
      private
        UsedParaRegs: TSupRegSet;
      public
        function GetExplicitRegisterInt(list:taasmoutput;Reg:Tnewregister):tregister;override;
        procedure UngetregisterInt(list:taasmoutput;Reg:tregister);override;
{$endif NEWRA}
        function GetRegisterFpu(list:TAasmOutput;size:Tcgsize):TRegister;override;
        procedure UngetRegisterFpu(list:taasmoutput;reg:tregister;size:TCGsize);override;
        procedure ClearTempGen;override;
      end;


implementation

    uses
      cgobj,verbose;


{$ifndef NEWRA}
    function TRgCpu.GetExplicitRegisterInt(list:TAasmOutput;reg:TNewRegister):TRegister;
      begin
        if ((reg shr 8) in [RS_O0..RS_O7,RS_I0..RS_I7]) then
          begin
            if (reg shr 8) in UsedParaRegs then
              InternalError(2003060701)
              ;
            include(UsedParaRegs,reg shr 8);
            result.enum:=R_INTREGISTER;
            result.number:=reg;
            cg.a_reg_alloc(list,result);
          end
        else
          result:=inherited GetExplicitRegisterInt(list,reg);
      end;


    procedure trgcpu.UngetRegisterInt(list:taasmoutput;reg:tregister);
      begin
        if reg.enum<>R_INTREGISTER then
          internalerror(200302191);
        if ((reg.number shr 8) in [RS_O0..RS_O7,RS_I0..RS_I7]) then
          begin
            if not((reg.number shr 8) in usedpararegs) then
              internalerror(2003060702)
              ;
            exclude(usedpararegs,reg.number shr 8);
            cg.a_reg_dealloc(list,reg);
          end
        else
          inherited ungetregisterint(list,reg);
      end;
{$endif NEWRA}


    function TRgCpu.GetRegisterFpu(list:TAasmOutput;size:Tcgsize):TRegister;
      var
        i: Toldregister;
        r: Tregister;
      begin
        for i:=firstsavefpureg to lastsavefpureg do
         begin
            if (i in unusedregsfpu) and
               (
                (size=OS_F32) or
                (not odd(ord(i)-ord(R_F0)))
               ) then
              begin
                 exclude(unusedregsfpu,i);
                 dec(countunusedregsfpu);
                 r.enum:=i;
                 list.concat(tai_regalloc.alloc(r));
                 result := r;
                 { double need 2 FPU registers }
                 if size=OS_F64 then
                   begin
                     r.enum:=succ(i);
                     exclude(unusedregsfpu,r.enum);
                     dec(countunusedregsfpu);
                     list.concat(tai_regalloc.alloc(r));
                   end;
                 exit;
              end;
         end;
        internalerror(10);
      end;


    procedure trgcpu.UngetRegisterFpu(list:taasmoutput;reg:tregister;size:TCGsize);
      var
        r : tregister;
      begin
        { double need 2 FPU registers }
        if (size=OS_F64) then
          begin
            { Only even FP registers are allowed }
            if (odd(ord(reg.enum)-ord(R_F0))) then
              internalerror(200306101);
            r:=reg;
            r.enum:=succ(r.enum);
            inc(countunusedregsfpu);
            include(unusedregsfpu,r.enum);
            list.concat(tai_regalloc.dealloc(r));
          end;
        inc(countunusedregsfpu);
        include(unusedregsfpu,reg.enum);
        list.concat(tai_regalloc.dealloc(reg));
      end;


    procedure trgcpu.cleartempgen;
      begin
        inherited cleartempgen;
{$ifndef NEWRA}
        usedpararegs:=[];
{$endif NEWRA}
      end;

begin
  rg := trgcpu.create(24); {24 registers.}
end.
{
  $Log$
  Revision 1.16  2003-08-11 21:18:20  peter
    * start of sparc support for newra

  Revision 1.15  2003/07/02 22:18:04  peter
    * paraloc splitted in callerparaloc,calleeparaloc
    * sparc calling convention updates

  Revision 1.14  2003/06/17 16:36:11  peter
    * freeintparaloc added

  Revision 1.13  2003/06/12 22:47:52  mazen
  - unused temp var r removed in GetExplicitRegisterInt function
  * some case added for var and fauncions naming

  Revision 1.12  2003/06/12 21:11:44  peter
    * updates like the powerpc

  Revision 1.11  2003/06/01 21:38:07  peter
    * getregisterfpu size parameter added
    * op_const_reg size parameter added
    * sparc updates

  Revision 1.10  2003/05/31 01:00:51  peter
    * register fixes

  Revision 1.9  2003/04/22 10:09:35  daniel
    + Implemented the actual register allocator
    + Scratch registers unavailable when new register allocator used
    + maybe_save/maybe_restore unavailable when new register allocator used

  Revision 1.8  2003/03/15 22:51:58  mazen
  * remaking sparc rtl compile

  Revision 1.7  2003/03/10 21:59:54  mazen
  * fixing index overflow in handling new registers arrays.

  Revision 1.6  2003/02/19 22:00:17  daniel
    * Code generator converted to new register notation
    - Horribily outdated todo.txt removed

  Revision 1.5  2003/01/08 18:43:58  daniel
   * Tregister changed into a record

  Revision 1.4  2002/10/13 21:46:07  mazen
  * assembler output format fixed

  Revision 1.3  2002/10/12 19:03:23  mazen
  * Get/Unget expilit registers to be re-examined

}
