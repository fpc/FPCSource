{
    Copyright (c) 2015 by Jonas Maebe

    This unit implements llvm support for some basic nodes

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
unit nllvmbas;

{$i fpcdefs.inc}

interface

    uses
       globtype,cclasses,
       aasmbase,aasmtai,aasmdata,
       nbas,ncgbas,
       symsym;

    type
       tllvmasmnode = class(tcgasmnode)
        protected
         { map a tasmsymbol to an index in fsymboldata }
         fsymbollookup: THashSet;
         { the LLVM symbolic inline assembly operands to which the ones in the
           source code are mapped }
         fsymboldata: tfplist;
         function getllvmasmopindexforsym(sym: tabstractnormalvarsym): longint;
         function getllvmasmparasym(sym: tabstractnormalvarsym): tasmsymbol;
         procedure ResolveRef(const filepos: tfileposinfo; var op: toper); override;
        public
         constructor create(p : TAsmList); override;
         destructor destroy; override;
         procedure pass_generate_code; override;
       end;

      tllvmtempinfoaccessor = class(ttempinfoaccessor)
       protected
        class procedure settempinfoflags(tempinfo: ptempinfo; const flags: ttempinfoflags); override;
      end;

       tllvmtempcreatenode = class(tcgtempcreatenode)
          procedure pass_generate_code;override;
       end;

  implementation

    uses
      verbose,cutils,
      cgbase,cgutils,paramgr,
      symconst,symdef,procinfo,
      node,
      cpubase,llvmbase,aasmllvm
      ;

{*****************************************************************************
                             TLLVMASMNODE
*****************************************************************************}

    function tllvmasmnode.getllvmasmopindexforsym(sym: tabstractnormalvarsym): longint;
      var
        key: record
          sym: pointer;
        end;
        res: PHashSetItem;
        callpara: pllvmcallpara;
      begin
        key.sym:=sym;
        res:=fsymbollookup.FindOrAdd(@key,sizeof(key));
        if not assigned(res^.Data) then
          begin
            new(callpara);
            callpara^.def:=cpointerdef.getreusable(sym.vardef);
            if (sym.typ=paravarsym) and
               paramanager.push_addr_param(sym.varspez,sym.vardef,current_procinfo.procdef.proccalloption) then
              callpara^.def:=cpointerdef.getreusable(callpara^.def);
            callpara^.sret:=false;
            callpara^.byval:=false;
            callpara^.valueext:=lve_none;
            callpara^.loc:=LOC_REGISTER;
            { address must be a temp register }
            if (sym.localloc.loc<>LOC_REFERENCE) or
               (sym.localloc.reference.base=NR_NO) or
               (sym.localloc.reference.index<>NR_NO) or
               (sym.localloc.reference.offset<>0) or
               assigned(sym.localloc.reference.symbol) then
              internalerror(2016111001);
            callpara^.reg:=sym.localloc.reference.base;
            fsymboldata.add(callpara);
            ptruint(res^.Data):=fsymboldata.count-1;
          end;
        result:=longint(ptruint(res^.Data));
      end;


    function tllvmasmnode.getllvmasmparasym(sym: tabstractnormalvarsym): tasmsymbol;
      begin
        { these have to be transformed from ^nr into into $nr; we use ^ because
          we also have to double all other occurrences of '$' in the assembly
          code, and we can't differentiate between these and other '$'s in
          agllvm }
        result:=current_asmdata.RefAsmSymbol('^'+tostr(getllvmasmopindexforsym(sym)),AT_DATA,false);
      end;


    procedure tllvmasmnode.ResolveRef(const filepos: tfileposinfo; var op: toper);
      var
        sym: tabstractnormalvarsym;
        ref: treference;
        sofs: pint;
        indexreg : tregister;
        getoffset: boolean;
{$ifdef x86}
        scale : byte;
{$endif x86}
      begin
        { pure assembler routines are handled by the regular code generator }
        if po_assembler in current_procinfo.procdef.procoptions then
          begin
            inherited;
            exit;
          end;
        { translate all symbolic references to "parameters" of the llvm
          assembler statements }
        case op.typ of
          top_local:
            begin
              sofs:=op.localoper^.localsymofs;
              indexreg:=op.localoper^.localindexreg;
{$ifdef x86}
              scale:=op.localoper^.localscale;
{$endif x86}
              getoffset:=op.localoper^.localgetoffset;
              sym:=tabstractnormalvarsym(op.localoper^.localsym);
              dispose(op.localoper);
              case sym.localloc.loc of
                LOC_REFERENCE:
                  begin
                    if getoffset then
                      begin
                        { todo: print proper error. You cannot get the offset
                          of a local variable since it may be in a register
                          outside the assembler block with llvm }
                        internalerror(2016102001);
                      end
                    else
                      begin
                        op.typ:=top_ref;
                        new(op.ref);
                        reference_reset_symbol(op.ref^,getllvmasmparasym(sym),sofs,
                          newalignment(sym.localloc.reference.alignment,sofs),[]);
                        op.ref^.index:=indexreg;
{$ifdef x86}
                        op.ref^.scalefactor:=scale;
{$endif x86}
                      end;
                  end
                else
                  { all locals accessed from assembler are forced into memory
                    by FPC }
                  internalerror(2016101506);
              end;
            end;
        end;
      end;


    constructor tllvmasmnode.create(p: TAsmList);
      begin
        inherited;
      end;


    destructor tllvmasmnode.destroy;
      begin
        { normally already freed in pass_generate_code, but in case an error
          occurred that may not have happened }
        fsymboldata.free;
        fsymbollookup.free;
        inherited;
      end;


    procedure tllvmasmnode.pass_generate_code;
      var
        oldasmlist: tasmlist;
        asmai: tai;
      begin
        oldasmlist:=nil;
        if not(po_assembler in current_procinfo.procdef.procoptions) and
           not(nf_get_asm_position in flags) then
          begin
            { store the assembler code in a separate list, so we can make it
              the argument of an asmblock instruction }
            oldasmlist:=current_asmdata.CurrAsmList;
            current_asmdata.CurrAsmList:=tasmlist.create;
            { record relation between parameters and replaced local assembler
              operands }
            fsymboldata:=tfplist.create;
            fsymbollookup:=THashSet.Create(8,True,False);
          end;
        inherited;
        if not(po_assembler in current_procinfo.procdef.procoptions) and
           not(nf_get_asm_position in flags) then
          begin
            asmai:=taillvm.asm_paras(current_asmdata.CurrAsmList,fsymboldata);
            fsymboldata:=nil;
            fsymbollookup.free;
            fsymbollookup:=nil;
            oldasmlist.concat(asmai);
            current_asmdata.CurrAsmList:=oldasmlist;
          end;
      end;

{*****************************************************************************
                          TLLVMTEMPINFOACCESSOR
*****************************************************************************}

    class procedure tllvmtempinfoaccessor.settempinfoflags(tempinfo: ptempinfo; const flags: ttempinfoflags);
        begin
          { it is not possible to typecast between e.g. an integer and a record
            in a register, which is a problem if such a typecast is performed on
            an lvalue (since we then have to store it first to a temp in memory,
            which means we no longer have an lvalue).

            Disable regvars altogether since LLVM will put the values in registers
            anyway if possible/useful. }
          inherited settempinfoflags(tempinfo,flags-[ti_may_be_in_reg]);
        end;


{*****************************************************************************
                          TTEMPCREATENODE
*****************************************************************************}

    procedure tllvmtempcreatenode.pass_generate_code;
      begin
        inherited;

        { if a temp is in a register and we never assign anything to it (e.g.
          because it's the register for an inlined function result that never
          gets assigned a value), then llvm will be confused the first time
          we try to read from it (since it's never been defined) -> always
          immediately assign undef to such registers }
        if tempinfo^.location.loc in [LOC_REGISTER,LOC_CREGISTER,LOC_FPUREGISTER,
             LOC_CFPUREGISTER,LOC_MMREGISTER,LOC_CMMREGISTER] then
          current_asmdata.CurrAsmList.concat(
            taillvm.op_reg_size_undef(la_bitcast,tempinfo^.location.register,tempinfo^.typedef)
          );
      end;


begin
   casmnode:=tllvmasmnode;
   ctempinfoaccessor:=tllvmtempinfoaccessor;
   ctempcreatenode:=tllvmtempcreatenode;
end.
