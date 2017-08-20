{
    Copyright (c) 1998-2002 by Florian Klaempfl

    Calling conventions for the SPARC

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
 *****************************************************************************}
unit sppara;

{$i fpcdefs.inc}

interface

    uses
      globtype,
      cclasses,
      aasmtai,aasmdata,
      cpubase,cpuinfo,
      symconst,symbase,symsym,symtype,symdef,paramgr,parabase,cgbase,cgutils;

    type
      tsparcparamanager=class(TParaManager)
        function  get_volatile_registers_int(calloption : tproccalloption):TCpuRegisterSet;override;
        function  get_volatile_registers_fpu(calloption : tproccalloption):TCpuRegisterSet;override;
        function  create_paraloc_info(p : TAbstractProcDef; side: tcallercallee):longint;override;
        function  create_varargs_paraloc_info(p : TAbstractProcDef; varargspara:tvarargsparalist):longint;override;
        procedure create_paraloc_info_intern(p : tabstractprocdef; side: tcallercallee; paras: tparalist;
                                             var curintreg: longint; curfloatreg: tsuperregister; var cur_stack_offset: aword);virtual;abstract;
      end;

    type
      tparasupregs = array[0..5] of tsuperregister;
      pparasupregs = ^tparasupregs;
    const
      paraoutsupregs : tparasupregs = (RS_O0,RS_O1,RS_O2,RS_O3,RS_O4,RS_O5);
      parainsupregs  : tparasupregs = (RS_I0,RS_I1,RS_I2,RS_I3,RS_I4,RS_I5);

implementation

    uses
      cutils,verbose,systems,
      defutil,
      cgobj;

    function tsparcparamanager.get_volatile_registers_int(calloption : tproccalloption):TCpuRegisterSet;
      begin
        result:=[RS_G1,RS_O0,RS_O1,RS_O2,RS_O3,RS_O4,RS_O5,RS_O6,RS_O7];
      end;


    function tsparcparamanager.get_volatile_registers_fpu(calloption : tproccalloption):TCpuRegisterSet;
      begin
        result:=[RS_F0..RS_F31];
      end;


    function tsparcparamanager.create_varargs_paraloc_info(p : tabstractprocdef; varargspara:tvarargsparalist):longint;
      var
        curintreg : LongInt;
        curfloatreg : TSuperRegister;
        cur_stack_offset : aword;
      begin
        curintreg:=0;
        curfloatreg:=RS_F0;
        cur_stack_offset:=0;
        { calculate the registers for the normal parameters }
        create_paraloc_info_intern(p,callerside,p.paras,curintreg,curfloatreg,cur_stack_offset);
        { append the varargs }
        create_paraloc_info_intern(p,callerside,varargspara,curintreg,curfloatreg,cur_stack_offset);
        result:=cur_stack_offset;
      end;


    function tsparcparamanager.create_paraloc_info(p : tabstractprocdef; side: tcallercallee):longint;
      var
        curintreg : LongInt;
        curfloatreg : TSuperRegister;
        cur_stack_offset : aword;
      begin
        curintreg:=0;
        curfloatreg:=RS_F0;
        cur_stack_offset:=0;
        create_paraloc_info_intern(p,side,p.paras,curintreg,curfloatreg,cur_stack_offset);
        { Create Function result paraloc }
        create_funcretloc_info(p,side);
        { We need to return the size allocated on the stack }
        result:=cur_stack_offset;
      end;


end.
