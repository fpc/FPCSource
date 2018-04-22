{
    Copyright (c) 1998-2002,2012 by Florian Klaempfl, Jonas Maebe

    This unit implements the LLVM-specific temp. generator

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

unit tgllvm;

{$i fpcdefs.inc}

  interface

    uses
      cclasses,
      globals,globtype,
      symtype,
      cpubase,cpuinfo,cgbase,cgutils,
      aasmbase,aasmtai,aasmdata,
      tgobj;

    type

      { LLVM temp manager: in LLVM, you allocate every temp separately using
        the "alloca" instrinsic. Every such temp is a separate stack slot, but
        can be turned into a regvar (or be decomposed) by LLVM. To avoid
        problems with turning stack slots into regvars, we don't allocate one
        big blob of memory that we manage ourselves using the regular temp
        manager. Instead, we just allocate a new "stack pointer register"
        (R_TEMPREGISTER) every time we need a new temp. This allows for having
        the generic code generator modify the offset without interfering with
        our ability to determine which temp the reference points to.

        Temps are currently not reused, but that should probably be added in
        the future (except if adding liveness information for the temps enables
        llvm to do so by itself and we don't run out of temp registers).
      }

      { ttgllvm }

      ttgllvm = class(ttgobj)
       protected
        procedure alloctemp(list: TAsmList; size: asizeint; alignment: shortint; temptype: ttemptype; def: tdef; fini: boolean; out ref: treference); override;
        procedure gethltempintern(list: TAsmList; def: tdef; alignment: shortint; forcesize: asizeint; temptype: ttemptype; out ref: treference);
       public
        alloclist: tasmlist;

        constructor create; override;
        destructor destroy; override;
        procedure setfirsttemp(l: asizeint); override;
        procedure temp_to_ref(p: ptemprecord; out ref: treference); override;
        procedure getlocal(list: TAsmList; size: asizeint; alignment: shortint; def: tdef; var ref: treference); override;
        procedure gethltemp(list: TAsmList; def: tdef; forcesize: asizeint; temptype: ttemptype; out ref: treference); override;
      end;


  var
    orgtgclass: ttgobjclass;

implementation

    uses
       cutils,
       systems,verbose,
       procinfo,
       llvmbase,aasmllvm,
       symconst,symdef,
       cgobj
       ;


    { ttgllvm }

    procedure ttgllvm.alloctemp(list: TAsmList; size: asizeint; alignment: shortint; temptype: ttemptype; def: tdef; fini: boolean; out ref: treference);
      var
        tl: ptemprecord;
        reg: tregister;
        oldfileinfo: tfileposinfo;
      begin
        reg:=cg.gettempregister(list);
        new(tl);

        tl^.temptype:=temptype;
        tl^.def:=def;
        tl^.fini:=fini;
        tl^.alignment:=alignment;
        tl^.pos:=getsupreg(reg);
        tl^.size:=size;
        tl^.next:=templist;
        tl^.nextfree:=nil;
        templist:=tl;
        temp_to_ref(tl,ref);
        list.concat(tai_tempalloc.alloc(tl^.pos,tl^.size));
        { TODO: add llvm.lifetime.start() for this allocation and afterwards
            llvm.lifetime.end() for freetemp (if the llvm version supports it) }
        inc(lasttemp);
        { allocation for the temp -- should have lineinfo of the start of the
          routine }
        if assigned(current_procinfo) then
          begin
            oldfileinfo:=current_filepos;
            current_filepos:=current_procinfo.procdef.fileinfo;
          end
        else
          { avoid uninitialised warning later }
          oldfileinfo.line:=0;
        alloclist.concat(taillvm.op_ref_size(la_alloca,ref,def));
        if assigned(current_procinfo) then
          current_filepos:=oldfileinfo;
      end;

    procedure ttgllvm.gethltempintern(list: TAsmList; def: tdef; alignment: shortint; forcesize: asizeint; temptype: ttemptype; out ref: treference);
      begin
        { empty array (can happen for arrayconstructors) -> don't request the
          size, as that will internalerror }
        if (def.typ=arraydef) and
           (tarraydef(def).highrange<tarraydef(def).lowrange) then
          alloctemp(list,0,alignment,temptype,def,false,ref)
        else
          alloctemp(list,def.size,alignment,temptype,def,false,ref);
      end;


    procedure ttgllvm.temp_to_ref(p: ptemprecord; out ref: treference);
      var
        temppos: treftemppos;
      begin
        { on the LLVM target, every temp is independent and encoded via a
          separate temp register whose superregister number is stored in p^.pos }
        temppos.val:=p^.pos;
        reference_reset_base(ref,newreg(R_TEMPREGISTER,p^.pos,R_SUBWHOLE),0,temppos,p^.alignment,[]);
      end;


    constructor ttgllvm.create;
      begin
        inherited create;
        direction:=1;
        alloclist:=TAsmList.create;
      end;


    destructor ttgllvm.destroy;
      begin
        alloclist.free;
        inherited destroy;
      end;


    procedure ttgllvm.setfirsttemp(l: asizeint);
      begin
        firsttemp:=l;
        lasttemp:=l;
      end;


    procedure ttgllvm.getlocal(list: TAsmList; size: asizeint; alignment: shortint; def: tdef; var ref: treference);
      begin
        alignment:=used_align(alignment,current_settings.alignment.localalignmin,current_settings.alignment.localalignmax);
        gethltempintern(list,def,alignment,size,tt_persistent,ref);
      end;


    procedure ttgllvm.gethltemp(list: TAsmList; def: tdef; forcesize: asizeint; temptype: ttemptype; out ref: treference);
      begin
        gethltempintern(list,def,def.alignment,forcesize,tt_persistent,ref);
      end;


begin
  orgtgclass:=tgobjclass;
  tgobjclass:=ttgllvm;
end.
