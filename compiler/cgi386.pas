{
    $Id$
    Copyright (c) 1993-98 by Florian Klaempfl

    This unit generates i386 (or better) assembler from the parse tree

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
{$ifdef TP}
  {$E+,F+,N+,D+,L-,Y+}
{$endif}
unit cgi386;
interface

uses
  tree;

{ produces assembler for the expression in variable p }
{ and produces an assembler node at the end           }
procedure generatecode(var p : ptree);

{ produces the actual code }
function do_secondpass(var p : ptree) : boolean;
procedure secondpass(var p : ptree);


{$ifdef test_dest_loc}

const
  { used to avoid temporary assignments }
  dest_loc_known : boolean = false;
  in_dest_loc    : boolean = false;
  dest_loc_tree  : ptree = nil;

var
  dest_loc : tlocation;

procedure mov_reg_to_dest(p : ptree; s : topsize; reg : tregister);

{$endif test_dest_loc}


implementation

   uses
     verbose,cobjects,systems,globals,files,
     symtable,types,aasm,i386,
     pass_1,hcodegen,tgeni386,cgai386
{$ifdef GDB}
     ,gdb
{$endif}
{$ifdef TP}
     ,cgi3862
{$endif}
     ;

    const
       never_copy_const_param : boolean = false;

{$ifdef test_dest_loc}
       procedure mov_reg_to_dest(p : ptree; s : topsize; reg : tregister);

         begin
            if (dest_loc.loc=LOC_CREGISTER) or (dest_loc.loc=LOC_REGISTER) then
              begin
                emit_reg_reg(A_MOV,s,reg,dest_loc.register);
                p^.location:=dest_loc;
                in_dest_loc:=true;
              end
            else
            if (dest_loc.loc=LOC_REFERENCE) or (dest_loc.loc=LOC_MEM) then
              begin
                exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,s,reg,newreference(dest_loc.reference))));
                p^.location:=dest_loc;
                in_dest_loc:=true;
              end
            else
              internalerror(20080);
         end;

{$endif test_dest_loc}

     const
       bytes2Sxx:array[1..4] of Topsize=(S_B,S_W,S_NO,S_L);

    procedure message(const t : tmsgconst);

      var
         olderrorcount : longint;

      begin
         if not(codegenerror) then
           begin
              olderrorcount:=errorcount;
              verbose.Message(t);
              codegenerror:=olderrorcount<>errorcount;
           end;
      end;

    procedure message1(const t : tmsgconst;const s : string);

      var
         olderrorcount : longint;

      begin
         if not(codegenerror) then
           begin
              olderrorcount:=errorcount;
              verbose.Message1(t,s);
              codegenerror:=olderrorcount<>errorcount;
           end;
      end;

    procedure message2(const t : tmsgconst;const s1,s2 : string);

      var
         olderrorcount : longint;

      begin
         if not(codegenerror) then
           begin
              olderrorcount:=errorcount;
              verbose.Message2(t,s1,s2);
              codegenerror:=olderrorcount<>errorcount;
           end;
      end;

    procedure message3(const t : tmsgconst;const s1,s2,s3 : string);

      var
         olderrorcount : longint;

      begin
         if not(codegenerror) then
           begin
              olderrorcount:=errorcount;
              verbose.Message3(t,s1,s2,s3);
              codegenerror:=olderrorcount<>errorcount;
           end;
      end;

    type
       secondpassproc = procedure(var p : ptree);

    procedure seconderror(var p : ptree);

      begin
         p^.error:=true;
         codegenerror:=true;
      end;

    var
       { this is for open arrays and strings        }
       { but be careful, this data is in the        }
       { generated code destroyed quick, and also   }
       { the next call of secondload destroys this  }
       { data                                       }
       { So be careful using the informations       }
       { provided by this variables                 }
       highframepointer : tregister;
       highoffset : longint;

{$ifndef TP}

{$I cgi386ad.inc}

{$endif TP}

    procedure secondload(var p : ptree);

      var
         hregister : tregister;
         symtabletype : tsymtabletype;
         i : longint;
         hp : preference;

      begin
         simple_loadn:=true;
         reset_reference(p^.location.reference);
         case p^.symtableentry^.typ of
              { this is only for toasm and toaddr }
              absolutesym :
                 begin
                    stringdispose(p^.location.reference.symbol);
                    if (pabsolutesym(p^.symtableentry)^.abstyp=toaddr) then
                     begin
                       if pabsolutesym(p^.symtableentry)^.absseg then
                        p^.location.reference.segment:=R_FS;
                       p^.location.reference.offset:=pabsolutesym(p^.symtableentry)^.address;
                     end
                    else
                     p^.location.reference.symbol:=stringdup(p^.symtableentry^.mangledname);
                    maybe_concat_external(p^.symtableentry^.owner,p^.symtableentry^.mangledname);
                 end;
              varsym :
                 begin
                    hregister:=R_NO;
                    symtabletype:=p^.symtable^.symtabletype;
                    { in case it is a register variable: }
                    if pvarsym(p^.symtableentry)^.reg<>R_NO then
                      begin
                         p^.location.loc:=LOC_CREGISTER;
                         p^.location.register:=pvarsym(p^.symtableentry)^.reg;
                         unused:=unused-[pvarsym(p^.symtableentry)^.reg];
                      end
                    else
                      begin
                         { first handle local and temporary variables }
                         if (symtabletype=parasymtable) or
                            (symtabletype=inlinelocalsymtable) or
                            (symtabletype=inlineparasymtable) or
                            (symtabletype=localsymtable) then
                           begin
                              p^.location.reference.base:=procinfo.framepointer;
                              p^.location.reference.offset:=pvarsym(p^.symtableentry)^.address;
                              if (symtabletype=localsymtable) or (symtabletype=inlinelocalsymtable) then
                                p^.location.reference.offset:=-p^.location.reference.offset;
                              if (symtabletype=parasymtable) or (symtabletype=inlineparasymtable) then
                                inc(p^.location.reference.offset,p^.symtable^.call_offset);
                              if (lexlevel>(p^.symtable^.symtablelevel)) then
                                begin
                                   hregister:=getregister32;

                                   { make a reference }
                                   hp:=new_reference(procinfo.framepointer,
                                     procinfo.framepointer_offset);
                                   
                                   exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,hp,hregister)));

                                   simple_loadn:=false;
                                   i:=lexlevel-1;
                                   while i>(p^.symtable^.symtablelevel) do
                                     begin
                                        { make a reference }
                                        hp:=new_reference(hregister,8);
                                        exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,hp,hregister)));
                                        dec(i);
                                     end;
                                   p^.location.reference.base:=hregister;
                                end;
                           end
                         else
                           case symtabletype of
                              unitsymtable,globalsymtable,
                              staticsymtable : begin
                                                  stringdispose(p^.location.reference.symbol);
                                                  p^.location.reference.symbol:=stringdup(p^.symtableentry^.mangledname);
                                                  if symtabletype=unitsymtable then
                                                    concat_external(p^.symtableentry^.mangledname,EXT_NEAR);
                                               end;
                              objectsymtable : begin
                                                  if (pvarsym(p^.symtableentry)^.properties and sp_static)<>0 then
                                                    begin
                                                       stringdispose(p^.location.reference.symbol);
                                                       p^.location.reference.symbol:=stringdup(p^.symtableentry^.mangledname);
                                                       if p^.symtable^.defowner^.owner^.symtabletype=unitsymtable then
                                                         concat_external(p^.symtableentry^.mangledname,EXT_NEAR);
                                                    end
                                                  else
                                                    begin
                                                       p^.location.reference.base:=R_ESI;
                                                       p^.location.reference.offset:=pvarsym(p^.symtableentry)^.address;
                                                    end;
                                               end;
                              withsymtable:
                                begin
                                   hregister:=getregister32;
                                   p^.location.reference.base:=hregister;
                                   { make a reference }
                                   { symtable datasize field
                                     contains the offset of the temp
                                     stored }
                                   hp:=new_reference(procinfo.framepointer,
                                     p^.symtable^.datasize);

                                   exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,hp,hregister)));

                                   p^.location.reference.offset:=
                                     pvarsym(p^.symtableentry)^.address;
                                end;
                           end;
                         { in case call by reference, then calculate: }
                         if (pvarsym(p^.symtableentry)^.varspez=vs_var) or
                            ((pvarsym(p^.symtableentry)^.varspez=vs_const) and
                             dont_copy_const_param(pvarsym(p^.symtableentry)^.definition)) or
                             { call by value open arrays are also indirect addressed }
                             is_open_array(pvarsym(p^.symtableentry)^.definition) then
                           begin
                              simple_loadn:=false;
                              if hregister=R_NO then
                                hregister:=getregister32;
                              if (p^.location.reference.base=procinfo.framepointer) then
                                begin
                                   highframepointer:=p^.location.reference.base;
                                   highoffset:=p^.location.reference.offset;
                                end
                              else
                                begin
                                   highframepointer:=R_EDI;
                                   highoffset:=p^.location.reference.offset;
                                   exprasmlist^.concat(new(pai386,op_reg_reg(A_MOV,S_L,
                                     p^.location.reference.base,R_EDI)));
                                end;
                              exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,newreference(p^.location.reference),
                                hregister)));
                              clear_reference(p^.location.reference);
                              p^.location.reference.base:=hregister;
                          end;
                         {
                         if (pvarsym(p^.symtableentry)^.definition^.deftype=objectdef) and
                           ((pobjectdef(pvarsym(p^.symtableentry)^.definition)^.options and oois_class)<>0) then
                           begin
                              simple_loadn:=false;
                              if hregister=R_NO then
                                hregister:=getregister32;
                              exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,newreference(p^.location.reference),
                                hregister)));
                              clear_reference(p^.location.reference);
                              p^.location.reference.base:=hregister;
                           end;
                         }
                      end;
                 end;
              procsym:
                 begin
                    {!!!!! Be aware, work on virtual methods too }
                    stringdispose(p^.location.reference.symbol);
                    p^.location.reference.symbol:=
                      stringdup(pprocsym(p^.symtableentry)^.definition^.mangledname);
                    maybe_concat_external(p^.symtable,p^.symtableentry^.mangledname);
                 end;
              typedconstsym :
                 begin
                    stringdispose(p^.location.reference.symbol);
                    p^.location.reference.symbol:=stringdup(p^.symtableentry^.mangledname);
                    maybe_concat_external(p^.symtable,p^.symtableentry^.mangledname);
                 end;
              else internalerror(4);
         end;
      end;

    procedure secondmoddiv(var p : ptree);

      var
         hreg1 : tregister;
         pushed,popeax,popedx : boolean;
         power : longint;
         hl : plabel;

      begin
         secondpass(p^.left);
         set_location(p^.location,p^.left^.location);
         pushed:=maybe_push(p^.right^.registers32,p);
         secondpass(p^.right);
         if pushed then restore(p);

         { put numerator in register }
         if p^.left^.location.loc<>LOC_REGISTER then
           begin
              if p^.left^.location.loc=LOC_CREGISTER then
                begin
                  hreg1:=getregister32;
                  emit_reg_reg(A_MOV,S_L,p^.left^.location.register,hreg1);
                end
              else
                begin
                  del_reference(p^.left^.location.reference);
                  hreg1:=getregister32;
                  exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,newreference(p^.left^.location.reference),
                    hreg1)));
                end;
              p^.left^.location.loc:=LOC_REGISTER;
              p^.left^.location.register:=hreg1;
           end
         else hreg1:=p^.left^.location.register;

           if (p^.treetype=divn) and (p^.right^.treetype=ordconstn) and
               ispowerof2(p^.right^.value,power) then
             begin
                 exprasmlist^.concat(new(pai386,op_reg_reg(A_OR,S_L,hreg1,hreg1)));
                 getlabel(hl);
                 emitl(A_JNS,hl);
                 if power=1 then
                    exprasmlist^.concat(new(pai386,op_reg(A_INC,S_L,hreg1)))
                 else exprasmlist^.concat(new(pai386,op_const_reg(A_ADD,S_L,p^.right^.value-1,hreg1)));

                 emitl(A_LABEL,hl);
                 exprasmlist^.concat(new(pai386,op_const_reg(A_SAR,S_L,power,hreg1)));
             end
           else
             begin
                 { bring denominator to EDI }
                 { EDI is always free, it's }
                 { only used for temporary  }
                 { purposes                 }
                 if (p^.right^.location.loc<>LOC_REGISTER) and
                     (p^.right^.location.loc<>LOC_CREGISTER) then
                    begin
                       del_reference(p^.right^.location.reference);
                       p^.left^.location.loc:=LOC_REGISTER;
                       exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,newreference(p^.right^.location.reference),R_EDI)));
                end
              else
                begin
                   ungetregister32(p^.right^.location.register);
                   emit_reg_reg(A_MOV,S_L,p^.right^.location.register,R_EDI);
                end;
              popedx:=false;
              popeax:=false;
              if hreg1=R_EDX then
                begin
                       if not(R_EAX in unused) then
                     begin
                        exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,R_EAX)));
                        popeax:=true;
                     end;
                   emit_reg_reg(A_MOV,S_L,R_EDX,R_EAX);
                end
                 else
                begin
                   if not(R_EDX in unused) then
                     begin
                              exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,R_EDX)));
                        popedx:=true;
                     end;
                   if hreg1<>R_EAX then
                     begin
                        if not(R_EAX in unused) then
                          begin
                             exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,R_EAX)));
                             popeax:=true;
                          end;
                        emit_reg_reg(A_MOV,S_L,hreg1,R_EAX);
                     end;
                end;
              exprasmlist^.concat(new(pai386,op_none(A_CLTD,S_NO)));
                 exprasmlist^.concat(new(pai386,op_reg(A_IDIV,S_L,R_EDI)));
                 if p^.treetype=divn then
                begin
                   { if result register is busy then copy }
                   if popeax then
                     begin
                        if hreg1=R_EAX then
                          internalerror(112);
                        emit_reg_reg(A_MOV,S_L,R_EAX,hreg1)
                     end
                   else
                          if hreg1<>R_EAX then
                       emit_reg_reg(A_MOV,S_L,R_EAX,hreg1);
                end
              else
                emit_reg_reg(A_MOV,S_L,R_EDX,hreg1);
              if popeax then
                exprasmlist^.concat(new(pai386,op_reg(A_POP,S_L,R_EAX)));
              if popedx then
                    exprasmlist^.concat(new(pai386,op_reg(A_POP,S_L,R_EDX)));
             end;
           { this registers are always used when div/mod are present }
         usedinproc:=usedinproc or ($80 shr byte(R_EAX));
         usedinproc:=usedinproc or ($80 shr byte(R_EDX));
         p^.location.loc:=LOC_REGISTER;
         p^.location.register:=hreg1;
      end;

    procedure secondshlshr(var p : ptree);

      var
         hregister1,hregister2,hregister3 : tregister;
         pushed,popecx : boolean;
         op : tasmop;

      begin
         popecx:=false;

         secondpass(p^.left);
         pushed:=maybe_push(p^.right^.registers32,p);
         secondpass(p^.right);
         if pushed then restore(p);

         { load left operators in a register }
         if p^.left^.location.loc<>LOC_REGISTER then
           begin
              if p^.left^.location.loc=LOC_CREGISTER then
                begin
                   hregister1:=getregister32;
                   emit_reg_reg(A_MOV,S_L,p^.left^.location.register,
                     hregister1);
                end
              else
                begin
                   del_reference(p^.left^.location.reference);
                   hregister1:=getregister32;
                   exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,newreference(p^.left^.location.reference),
                     hregister1)));
                end;
           end
           else hregister1:=p^.left^.location.register;

         { determine operator }
         if p^.treetype=shln then
           op:=A_SHL
         else
           op:=A_SHR;

         { shifting by a constant directly decode: }
         if (p^.right^.treetype=ordconstn) then
           begin
                 exprasmlist^.concat(new(pai386,op_const_reg(op,S_L,p^.right^.location.reference.offset and 31,
                hregister1)));
              p^.location.loc:=LOC_REGISTER;
              p^.location.register:=hregister1;
           end
         else
           begin
              { load right operators in a register }
              if p^.right^.location.loc<>LOC_REGISTER then
                begin
                       if p^.right^.location.loc=LOC_CREGISTER then
                     begin
                              hregister2:=getregister32;
                        emit_reg_reg(A_MOV,S_L,p^.right^.location.register,
                          hregister2);
                     end
                   else
                     begin
                        del_reference(p^.right^.location.reference);
                        hregister2:=getregister32;
                        exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,newreference(p^.right^.location.reference),
                          hregister2)));
                     end;
                end
              else hregister2:=p^.right^.location.register;

                 { left operator is already in a register }
              { hence are both in a register }
              { is it in the case ECX ? }
              if (hregister1=R_ECX) then
                begin
                   { then only swap }
                   emit_reg_reg(A_XCHG,S_L,hregister1,
                     hregister2);

                   hregister3:=hregister1;
                   hregister1:=hregister2;
                   hregister2:=hregister3;
                end
              { if second operator not in ECX ? }
              else if (hregister2<>R_ECX) then
                begin
                   { ECX not occupied then swap with right register }
                   if R_ECX in unused then
                     begin
                        emit_reg_reg(A_MOV,S_L,hregister2,R_ECX);
                        ungetregister32(hregister2);
                          end
                       else
                     begin
                        { else save ECX and then copy it }
                        popecx:=true;
                        exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,R_ECX)));
                        emit_reg_reg(A_MOV,S_L,hregister2,R_ECX);
                        ungetregister32(hregister2);
                     end;
                end;
              { right operand is in ECX }
              emit_reg_reg(op,S_L,R_CL,hregister1);
              { maybe ECX back }
              if popecx then
                exprasmlist^.concat(new(pai386,op_reg(A_POP,S_L,R_ECX)));
              p^.location.register:=hregister1;
             end;
         { this register is always used when shl/shr are present }
         usedinproc:=usedinproc or ($80 shr byte(R_ECX));
      end;

    procedure secondrealconst(var p : ptree);

      var
         hp1 : pai;
         lastlabel : plabel;
         found : boolean;
      begin
         clear_reference(p^.location.reference);
         lastlabel:=nil;
         found:=false;
         { const already used ? }
         if p^.labnumber=-1 then
           begin
              { tries to found an old entry }
              hp1:=pai(consts^.first);
              while assigned(hp1) do
                begin
                   if hp1^.typ=ait_label then
                     lastlabel:=pai_label(hp1)^.l
                   else
                     begin
                        if (hp1^.typ=p^.realtyp) and (lastlabel<>nil) then
                          begin
                             if ((p^.realtyp=ait_real_64bit) and (pai_double(hp1)^.value=p^.valued)) or
                               ((p^.realtyp=ait_real_extended) and (pai_extended(hp1)^.value=p^.valued)) or
                               ((p^.realtyp=ait_real_32bit) and (pai_single(hp1)^.value=p^.valued)) then
                               begin
                                  { found! }
                                  p^.labnumber:=lastlabel^.nb;
                                  break;
                               end;
                          end;
                        lastlabel:=nil;
                     end;
                   hp1:=pai(hp1^.next);
                end;
              { :-(, we must generate a new entry }
              if p^.labnumber=-1 then
                begin
                   getlabel(lastlabel);
                   p^.labnumber:=lastlabel^.nb;
                   concat_constlabel(lastlabel,constreal);
                   case p^.realtyp of
                     ait_real_64bit : consts^.concat(new(pai_double,init(p^.valued)));
                     ait_real_32bit : consts^.concat(new(pai_single,init(p^.valued)));
                  ait_real_extended : consts^.concat(new(pai_extended,init(p^.valued)));
                   else
                     internalerror(10120);
                   end;
                end;
           end;
         stringdispose(p^.location.reference.symbol);
         if assigned(lastlabel) then
           p^.location.reference.symbol:=stringdup(constlabel2str(lastlabel,constreal))
         else
           p^.location.reference.symbol:=stringdup(constlabelnb2str(p^.labnumber,constreal));
      end;

    procedure secondfixconst(var p : ptree);

      begin
         { an fix comma const. behaves as a memory reference }
         p^.location.loc:=LOC_MEM;
         p^.location.reference.isintvalue:=true;
         p^.location.reference.offset:=p^.valuef;
      end;

    procedure secondordconst(var p : ptree);

      begin
         { an integer const. behaves as a memory reference }
         p^.location.loc:=LOC_MEM;
         p^.location.reference.isintvalue:=true;
         p^.location.reference.offset:=p^.value;
      end;

    procedure secondniln(var p : ptree);

      begin
         p^.location.loc:=LOC_MEM;
         p^.location.reference.isintvalue:=true;
         p^.location.reference.offset:=0;
      end;

    procedure secondstringconst(var p : ptree);

      var
         hp1 : pai;
         lastlabel : plabel;
         pc : pchar;
         same_string : boolean;
         i : word;

      begin
         clear_reference(p^.location.reference);
         lastlabel:=nil;
         { const already used ? }
         if p^.labstrnumber=-1 then
           begin
              { tries to found an old entry }
              hp1:=pai(consts^.first);
              while assigned(hp1) do
                begin
                   if hp1^.typ=ait_label then
                     lastlabel:=pai_label(hp1)^.l
                   else
                     begin
                        if (hp1^.typ=ait_string) and (lastlabel<>nil) and
                          (pai_string(hp1)^.len=length(p^.values^)+2) then
                          begin
                             same_string:=true;
{$ifndef UseAnsiString}
                             for i:=1 to length(p^.values^) do
                               if pai_string(hp1)^.str[i]<>p^.values^[i] then
{$else}
                             for i:=0 to p^.length do
                               if pai_string(hp1)^.str[i]<>p^.values[i] then
{$endif}
                                 begin
                                    same_string:=false;
                                    break;
                                 end;
                             if same_string then
                               begin
                                  { found! }
                                  p^.labstrnumber:=lastlabel^.nb;
                                  break;
                               end;
                          end;
                        lastlabel:=nil;
                     end;
                   hp1:=pai(hp1^.next);
                end;
              { :-(, we must generate a new entry }
              if p^.labstrnumber=-1 then
                begin
                   getlabel(lastlabel);
                   p^.labstrnumber:=lastlabel^.nb;
{$ifndef UseAnsiString}
                   getmem(pc,length(p^.values^)+3);
                   move(p^.values^,pc^,length(p^.values^)+1);
                   pc[length(p^.values^)+1]:=#0;
{$else UseAnsiString}
                   pc:=getpcharcopy(p);
{$endif UseAnsiString}

                   concat_constlabel(lastlabel,conststring);
{$ifdef UseAnsiString}
  {$ifdef debug}
                   consts^.concat(new(pai_asm_comment,init('Header of ansistring')));
  {$endif debug}
                   consts^.concat(new(pai_const,init_32bit(p^.length)));
                   consts^.concat(new(pai_const,init_32bit(p^.length)));
                   consts^.concat(new(pai_const,init_32bit(-1)));
                   { to overcome this problem we set the length explicitly }
                   { with the ending null char }
                   consts^.concat(new(pai_string,init_length_pchar(pc,p^.length+1)));
{$else UseAnsiString}
                   { we still will have a problem if there is a #0 inside the pchar }
                   consts^.concat(new(pai_string,init_length_pchar(pc,length(p^.values^)+2)));
{$endif UseAnsiString}
                end;
           end;
         stringdispose(p^.location.reference.symbol);
         if assigned(lastlabel) then
           p^.location.reference.symbol:=stringdup(constlabel2str(lastlabel,conststring))
         else
           p^.location.reference.symbol:=stringdup(constlabelnb2str(p^.labnumber,conststring));
         p^.location.loc := LOC_MEM;
      end;

    procedure secondumminus(var p : ptree);

{$ifdef SUPPORT_MMX}
      procedure do_mmx_neg;

        var
           op : tasmop;

        begin
           p^.location.loc:=LOC_MMXREGISTER;
           if cs_mmx_saturation in aktswitches then
             case mmx_type(p^.resulttype) of
                mmxs8bit:
                  op:=A_PSUBSB;
                mmxu8bit:
                  op:=A_PSUBUSB;
                mmxs16bit,mmxfixed16:
                  op:=A_PSUBSW;
                mmxu16bit:
                  op:=A_PSUBUSW;
             end
           else
             case mmx_type(p^.resulttype) of
                mmxs8bit,mmxu8bit:
                  op:=A_PSUBB;
                mmxs16bit,mmxu16bit,mmxfixed16:
                  op:=A_PSUBW;
                mmxs32bit,mmxu32bit:
                  op:=A_PSUBD;
             end;
           emit_reg_reg(op,S_NO,p^.location.register,R_MM7);
           emit_reg_reg(A_MOVQ,S_NO,R_MM7,p^.location.register);
        end;
{$endif}

      begin
         secondpass(p^.left);
         p^.location.loc:=LOC_REGISTER;
         case p^.left^.location.loc of
            LOC_REGISTER:
              begin
                 p^.location.register:=p^.left^.location.register;
                 exprasmlist^.concat(new(pai386,op_reg(A_NEG,S_L,p^.location.register)));
              end;
            LOC_CREGISTER:
              begin
                 p^.location.register:=getregister32;
                 emit_reg_reg(A_MOV,S_L,p^.location.register,
                   p^.location.register);
                 exprasmlist^.concat(new(pai386,op_reg(A_NEG,S_L,p^.location.register)));
              end;
{$ifdef SUPPORT_MMX}
            LOC_MMXREGISTER:
              begin
                 p^.location:=p^.left^.location;
                 emit_reg_reg(A_PXOR,S_NO,R_MM7,R_MM7);
                 do_mmx_neg;
              end;
            LOC_CMMXREGISTER:
              begin
                 p^.location.register:=getregistermmx;
                 emit_reg_reg(A_PXOR,S_NO,R_MM7,R_MM7);
                 emit_reg_reg(A_MOVQ,S_NO,p^.left^.location.register,
                   p^.location.register);
                 do_mmx_neg;
              end;
{$endif SUPPORT_MMX}
            LOC_REFERENCE,LOC_MEM:
                           begin
                              del_reference(p^.left^.location.reference);
                              if (p^.left^.resulttype^.deftype=floatdef) and
                                 (pfloatdef(p^.left^.resulttype)^.typ<>f32bit) then
                                begin
                                   p^.location.loc:=LOC_FPU;
                                   floatload(pfloatdef(p^.left^.resulttype)^.typ,
                                     p^.left^.location.reference);
                                   exprasmlist^.concat(new(pai386,op_none(A_FCHS,S_NO)));
                                end
{$ifdef SUPPORT_MMX}
                              else if (cs_mmx in aktswitches) and is_mmx_able_array(p^.left^.resulttype) then
                                begin
                                   p^.location.register:=getregistermmx;
                                   emit_reg_reg(A_PXOR,S_NO,R_MM7,R_MM7);
                                   exprasmlist^.concat(new(pai386,op_ref_reg(A_MOVQ,S_NO,
                                     newreference(p^.left^.location.reference),
                                     p^.location.register)));
                                   do_mmx_neg;
                                end
{$endif SUPPORT_MMX}
                              else
                                begin
                                   p^.location.register:=getregister32;
                                   exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,
                                     newreference(p^.left^.location.reference),
                                     p^.location.register)));
                                   exprasmlist^.concat(new(pai386,op_reg(A_NEG,S_L,p^.location.register)));
                                end;
                           end;
            LOC_FPU:
              begin
                 p^.location.loc:=LOC_FPU;
                 exprasmlist^.concat(new(pai386,op_none(A_FCHS,S_NO)));
              end;
         end;
{ Here was a problem...            }
{ Operand to be negated always     }
{ seems to be converted to signed  }
{ 32-bit before doing neg!!        }
{ So this is useless...            }
{         emitoverflowcheck(p);}
      end;

    procedure secondaddr(var p : ptree);

      begin
         secondpass(p^.left);
         p^.location.loc:=LOC_REGISTER;
         del_reference(p^.left^.location.reference);
         p^.location.register:=getregister32;
         {@ on a procvar means returning an address to the procedure that
           is stored in it.}
         { yes but p^.left^.symtableentry can be nil
           for example on @self !! }
         { symtableentry can be also invalid, if left is no tree node }
         if (p^.left^.treetype=loadn) and
           assigned(p^.left^.symtableentry) and
           (p^.left^.symtableentry^.typ=varsym) and
           (pvarsym(p^.left^.symtableentry)^.definition^.deftype=procvardef) then
           exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,
             newreference(p^.left^.location.reference),
             p^.location.register)))
         else
           exprasmlist^.concat(new(pai386,op_ref_reg(A_LEA,S_L,
             newreference(p^.left^.location.reference),
             p^.location.register)));
           { for use of other segments }
           if p^.left^.location.reference.segment<>R_DEFAULT_SEG then
             p^.location.segment:=p^.left^.location.reference.segment;
      end;

    procedure seconddoubleaddr(var p : ptree);

      begin
         secondpass(p^.left);
         p^.location.loc:=LOC_REGISTER;
         del_reference(p^.left^.location.reference);
         p^.location.register:=getregister32;
         exprasmlist^.concat(new(pai386,op_ref_reg(A_LEA,S_L,
         newreference(p^.left^.location.reference),
           p^.location.register)));
      end;

    procedure secondnot(var p : ptree);

      const
         flagsinvers : array[F_E..F_BE] of tresflags =
            (F_NE,F_E,F_LE,F_GE,F_L,F_G,F_NC,F_C,
             F_A,F_AE,F_B,F_BE);

      var
         hl : plabel;

      begin
         if (p^.resulttype^.deftype=orddef) and
            (porddef(p^.resulttype)^.typ=bool8bit) then
              begin
                 case p^.location.loc of
                    LOC_JUMP : begin
                                  hl:=truelabel;
                                  truelabel:=falselabel;
                                  falselabel:=hl;
                                  secondpass(p^.left);
                                  maketojumpbool(p^.left);
                                  hl:=truelabel;
                                  truelabel:=falselabel;
                                  falselabel:=hl;
                               end;
                    LOC_FLAGS : begin
                                   secondpass(p^.left);
                                   p^.location.resflags:=flagsinvers[p^.left^.location.resflags];
                                end;
                    LOC_REGISTER : begin
                                      secondpass(p^.left);
                                      p^.location.register:=p^.left^.location.register;
                                      exprasmlist^.concat(new(pai386,op_const_reg(A_XOR,S_B,1,p^.location.register)));
                                   end;
                    LOC_CREGISTER : begin
                                       secondpass(p^.left);
                                       p^.location.loc:=LOC_REGISTER;
                                       p^.location.register:=reg32toreg8(getregister32);
                                       emit_reg_reg(A_MOV,S_B,p^.left^.location.register,
                                         p^.location.register);
                                       exprasmlist^.concat(new(pai386,op_const_reg(A_XOR,S_B,1,p^.location.register)));
                                    end;
                    LOC_REFERENCE,LOC_MEM : begin
                                              secondpass(p^.left);
                                              del_reference(p^.left^.location.reference);
                                              p^.location.loc:=LOC_REGISTER;
                                              p^.location.register:=reg32toreg8(getregister32);
                                              if p^.left^.location.loc=LOC_CREGISTER then
                                                emit_reg_reg(A_MOV,S_B,p^.left^.location.register,
                                                   p^.location.register)
                                              else
                                                exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_B,
                                              newreference(p^.left^.location.reference),
                                                p^.location.register)));
                                              exprasmlist^.concat(new(pai386,op_const_reg(A_XOR,S_B,1,p^.location.register)));
                                           end;
                 end;
              end
{$ifdef SUPPORT_MMX}
            else if (cs_mmx in aktswitches) and is_mmx_able_array(p^.left^.resulttype) then
              begin
                 secondpass(p^.left);
                 p^.location.loc:=LOC_MMXREGISTER;
                 { prepare EDI }
                 exprasmlist^.concat(new(pai386,op_const_reg(A_MOV,S_L,$ffffffff,R_EDI)));
                 { load operand }
                 case p^.left^.location.loc of
                    LOC_MMXREGISTER:
                      p^.location:=p^.left^.location;
                    LOC_CMMXREGISTER:
                      begin
                         p^.location.register:=getregistermmx;
                         emit_reg_reg(A_MOVQ,S_NO,p^.left^.location.register,
                           p^.location.register);
                      end;
                    LOC_REFERENCE,LOC_MEM:
                      begin
                         del_reference(p^.left^.location.reference);
                         p^.location.register:=getregistermmx;
                         exprasmlist^.concat(new(pai386,op_ref_reg(A_MOVQ,S_NO,
                           newreference(p^.left^.location.reference),
                           p^.location.register)));
                      end;
                 end;
                 { load mask }
                 emit_reg_reg(A_MOV,S_D,R_EDI,R_MM7);
                 { lower 32 bit }
                 emit_reg_reg(A_PXOR,S_D,R_MM7,p^.location.register);
                 { shift mask }
                 exprasmlist^.concat(new(pai386,op_const_reg(A_PSLLQ,S_NO,
                   32,R_MM7)));
                 { higher 32 bit }
                 emit_reg_reg(A_PXOR,S_D,R_MM7,p^.location.register);
              end
{$endif SUPPORT_MMX}
            else
              begin
                secondpass(p^.left);
                p^.location.loc:=LOC_REGISTER;

                case p^.left^.location.loc of
                   LOC_REGISTER : begin
                                     p^.location.register:=p^.left^.location.register;
                                     exprasmlist^.concat(new(pai386,op_reg(A_NOT,S_L,p^.location.register)));
                                  end;
                   LOC_CREGISTER : begin
                                     p^.location.register:=getregister32;
                                     emit_reg_reg(A_MOV,S_L,p^.left^.location.register,
                                       p^.location.register);
                                     exprasmlist^.concat(new(pai386,op_reg(A_NOT,S_L,p^.location.register)));
                                   end;
                   LOC_REFERENCE,LOC_MEM :
                                  begin
                                     del_reference(p^.left^.location.reference);
                                     p^.location.register:=getregister32;
                                     exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,
                                       newreference(p^.left^.location.reference),
                                       p^.location.register)));
                                     exprasmlist^.concat(new(pai386,op_reg(A_NOT,S_L,p^.location.register)));
                                  end;
                end;
                {if  p^.left^.location.loc=loc_register then
                  p^.location.register:=p^.left^.location.register
                else
                  begin
                     del_locref(p^.left^.location);
                     p^.location.register:=getregister32;
                     exprasmlist^.concat(new(pai386,op_loc_reg(A_MOV,S_L,
                       p^.left^.location,
                       p^.location.register)));
                  end;
                exprasmlist^.concat(new(pai386,op_reg(A_NOT,S_L,p^.location.register)));}

             end;
      end;

    procedure secondnothing(var p : ptree);

      begin
      end;

    procedure secondderef(var p : ptree);

      var
         hr : tregister;

      begin
         secondpass(p^.left);
         clear_reference(p^.location.reference);
         case p^.left^.location.loc of
            LOC_REGISTER:
              p^.location.reference.base:=p^.left^.location.register;
            LOC_CREGISTER:
              begin
                 { ... and reserve one for the pointer }
                 hr:=getregister32;
                 emit_reg_reg(A_MOV,S_L,p^.left^.location.register,hr);
                 p^.location.reference.base:=hr;
              end;
            else
              begin
                 { free register }
                 del_reference(p^.left^.location.reference);

                 { ...and reserve one for the pointer }
                 hr:=getregister32;
                 exprasmlist^.concat(new(pai386,op_ref_reg(
                   A_MOV,S_L,newreference(p^.left^.location.reference),
                   hr)));
                 p^.location.reference.base:=hr;
              end;
         end;
      end;

    procedure secondvecn(var p : ptree);

      var
         pushed : boolean;
         ind,hr : tregister;
         _p : ptree;

       function get_mul_size:longint;
         begin
           if p^.memindex then
             get_mul_size:=1
           else
             get_mul_size:=p^.resulttype^.size;
         end;


      procedure calc_emit_mul;

        var
           l1,l2 : longint;

          begin
           l1:=get_mul_size;
           case l1 of
              1,2,4,8 : p^.location.reference.scalefactor:=l1;
           else
                begin
                   if ispowerof2(l1,l2) then
                     exprasmlist^.concat(new(pai386,op_const_reg(A_SHL,S_L,l2,ind)))
                   else
                     exprasmlist^.concat(new(pai386,op_const_reg(A_IMUL,S_L,l1,ind)));
                end;
           end;
        end;

      var
         extraoffset : longint;
           t : ptree;
           hp : preference;
           tai:Pai386;

      begin
         secondpass(p^.left);
         set_location(p^.location,p^.left^.location);

         { in ansistrings S[1] is pchar(S)[0] !! }
         if is_ansistring(p^.left^.resulttype) then
           dec(p^.location.reference.offset);
         { offset can only differ from 0 if arraydef }
         if p^.left^.resulttype^.deftype=arraydef then
           dec(p^.location.reference.offset,
               get_mul_size*parraydef(p^.left^.resulttype)^.lowrange);
         if p^.right^.treetype=ordconstn then
           begin
              { offset can only differ from 0 if arraydef }
              if (p^.left^.resulttype^.deftype=arraydef) then
                begin
                   if not(is_open_array(p^.left^.resulttype)) then
                         begin
                        if (p^.right^.value>parraydef(p^.left^.resulttype)^.highrange) or
                           (p^.right^.value<parraydef(p^.left^.resulttype)^.lowrange) then
                          Message(parser_e_range_check_error);

                        dec(p^.left^.location.reference.offset,
                            get_mul_size*parraydef(p^.left^.resulttype)^.lowrange);
                         end
                   else
                     begin
                        { range checking for open arrays }
                     end;
                end;
              inc(p^.left^.location.reference.offset,
                  get_mul_size*p^.right^.value);
              if p^.memseg then
                p^.left^.location.reference.segment:=R_FS;
              p^.left^.resulttype:=p^.resulttype;
              disposetree(p^.right);
              _p:=p^.left;
              putnode(p);
              p:=_p;
           end
         else
           begin
                 { quick hack, to overcome Delphi 2 }
              if (cs_maxoptimieren in aktswitches) and
                (p^.left^.resulttype^.deftype=arraydef) then
                begin
                   extraoffset:=0;
                   if (p^.right^.treetype=addn) then
                     begin
                        if p^.right^.right^.treetype=ordconstn then
                          begin
                             extraoffset:=p^.right^.right^.value;
                             t:=p^.right^.left;
                             putnode(p^.right);
                             putnode(p^.right^.right);
                             p^.right:=t
                          end
                        else if p^.right^.left^.treetype=ordconstn then
                          begin
                             extraoffset:=p^.right^.left^.value;
                             t:=p^.right^.right;
                                           putnode(p^.right);
                             putnode(p^.right^.left);
                             p^.right:=t
                          end;
                     end
                   else if (p^.right^.treetype=subn) then
                     begin
                                    if p^.right^.right^.treetype=ordconstn then
                          begin
                             extraoffset:=p^.right^.right^.value;
                             t:=p^.right^.left;
                             putnode(p^.right);
                             putnode(p^.right^.right);
                             p^.right:=t
                          end
                        else if p^.right^.left^.treetype=ordconstn then
                          begin
                             extraoffset:=p^.right^.left^.value;
                                           t:=p^.right^.right;
                             putnode(p^.right);
                             putnode(p^.right^.left);
                             p^.right:=t
                          end;
                     end;
                   inc(p^.location.reference.offset,
                       get_mul_size*extraoffset);
                end;
              { calculate from left to right }
              if (p^.location.loc<>LOC_REFERENCE) and
                 (p^.location.loc<>LOC_MEM) then
                Message(cg_e_illegal_expression);
              pushed:=maybe_push(p^.right^.registers32,p);
              secondpass(p^.right);
              if pushed then restore(p);
                case p^.right^.location.loc of
                   LOC_REGISTER:
                     begin
                        ind:=p^.right^.location.register;
                        case p^.right^.resulttype^.size of
                           1:
                             begin
                                hr:=reg8toreg32(ind);
                                emit_reg_reg(A_MOVZX,S_BL,ind,hr);
                                ind:=hr;
                             end;
                           2:
                             begin
                                hr:=reg16toreg32(ind);
                                emit_reg_reg(A_MOVZX,S_WL,ind,hr);
                                 ind:=hr;
                             end;
                        end;
                     end;
                   LOC_CREGISTER:
                     begin
                        ind:=getregister32;
                        case p^.right^.resulttype^.size of
                           1:
                             emit_reg_reg(A_MOVZX,S_BL,p^.right^.location.register,ind);
                           2:
                             emit_reg_reg(A_MOVZX,S_WL,p^.right^.location.register,ind);
                           4:
                             emit_reg_reg(A_MOV,S_L,p^.right^.location.register,ind);
                        end;
                     end;
                   LOC_FLAGS:
                     begin
                        ind:=getregister32;
                        exprasmlist^.concat(new(pai386,op_reg(flag_2_set[p^.right^.location.resflags],S_NO,reg32toreg8(ind))));
                        emit_reg_reg(A_MOVZX,S_BL,reg32toreg8(ind),ind);
                     end
                   else
                      begin
                         del_reference(p^.right^.location.reference);
                         ind:=getregister32;
                         { Booleans are stored in an 8 bit memory location, so
                           the use of MOVL is not correct }
                         case p^.right^.resulttype^.size of
                           1:
                             tai:=new(pai386,op_ref_reg(A_MOVZX,S_BL,newreference(p^.right^.location.reference),ind));
                           2:
                             tai:=new(Pai386,op_ref_reg(A_MOVZX,S_WL,newreference(p^.right^.location.reference),ind));
                           4:
                             tai:=new(Pai386,op_ref_reg(A_MOV,S_L,newreference(p^.right^.location.reference),ind));
                         end;
                         exprasmlist^.concat(tai);
                      end;
                end;
              { produce possible range check code: }
              if cs_rangechecking in aktswitches  then
                begin
                   if p^.left^.resulttype^.deftype=arraydef then
                     begin
                        hp:=new_reference(R_NO,0);
                        parraydef(p^.left^.resulttype)^.genrangecheck;
                        hp^.symbol:=stringdup('R_'+tostr(parraydef(p^.left^.resulttype)^.rangenr));
                        exprasmlist^.concat(new(pai386,op_reg_ref(A_BOUND,S_L,ind,hp)));
                     end;
                end;
              if p^.location.reference.index=R_NO then
                begin
                   p^.location.reference.index:=ind;
                   calc_emit_mul;
                end
              else
                begin
                   if p^.location.reference.base=R_NO then
                     begin
                        case p^.location.reference.scalefactor of
                           2 : exprasmlist^.concat(new(pai386,op_const_reg(A_SHL,S_L,1,p^.location.reference.index)));
                           4 : exprasmlist^.concat(new(pai386,op_const_reg(A_SHL,S_L,2,p^.location.reference.index)));
                           8 : exprasmlist^.concat(new(pai386,op_const_reg(A_SHL,S_L,3,p^.location.reference.index)));
                        end;
                        calc_emit_mul;
                        p^.location.reference.base:=p^.location.reference.index;
                        p^.location.reference.index:=ind;
                     end
                   else
                     begin
                        exprasmlist^.concat(new(pai386,op_ref_reg(
                          A_LEA,S_L,newreference(p^.location.reference),
                          p^.location.reference.index)));
                        ungetregister32(p^.location.reference.base);
                        { the symbol offset is loaded,               }
                        { so release the symbol name and set symbol  }
                        { to nil                                     }
                        stringdispose(p^.location.reference.symbol);
                        p^.location.reference.offset:=0;
                        calc_emit_mul;
                        p^.location.reference.base:=p^.location.reference.index;
                        p^.location.reference.index:=ind;
                     end;
                end;
             if p^.memseg then
               p^.location.reference.segment:=R_FS;
           end;
      end;

    { *************** Converting Types **************** }

    { produces if necessary rangecheckcode }

     procedure maybe_rangechecking(p : ptree;p2,p1 : pdef);

       var
          hp : preference;
          hregister : tregister;
          neglabel,poslabel : plabel;
          is_register : boolean;

      begin
         { convert from p2 to p1 }
         { range check from enums is not made yet !!}
         { and its probably not easy }
         if (p1^.deftype<>orddef) or (p2^.deftype<>orddef) then
           exit;
         { range checking is different for u32bit }
         { lets try to generate it allways }
         if (cs_rangechecking in aktswitches)  and
           { with $R+ explicit type conversations in TP aren't range checked! }
           (not(p^.explizit) or not(cs_tp_compatible in aktswitches)) and
           ((porddef(p1)^.von>porddef(p2)^.von) or
           (porddef(p1)^.bis<porddef(p2)^.bis) or
           (porddef(p1)^.typ=u32bit) or
           (porddef(p2)^.typ=u32bit)) then
           begin
              porddef(p1)^.genrangecheck;
              is_register:=(p^.left^.location.loc=LOC_REGISTER) or
                (p^.left^.location.loc=LOC_CREGISTER);
              if porddef(p2)^.typ=u8bit then
                begin
                   if is_register then
                     exprasmlist^.concat(new(pai386,op_reg_reg(A_MOVZX,S_BL,p^.left^.location.register,R_EDI)))
                   else
                     exprasmlist^.concat(new(pai386,op_ref_reg(A_MOVZX,S_BL,newreference(p^.left^.location.reference),R_EDI)));
                   hregister:=R_EDI;
                end
              else if porddef(p2)^.typ=s8bit then
                begin
                   if is_register then
                     exprasmlist^.concat(new(pai386,op_reg_reg(A_MOVSX,S_BL,p^.left^.location.register,R_EDI)))
                   else
                     exprasmlist^.concat(new(pai386,op_ref_reg(A_MOVSX,S_BL,newreference(p^.left^.location.reference),R_EDI)));
                   hregister:=R_EDI;
                end
              { rangechecking for u32bit ?? !!!!!!}
              { lets try }
              else if (porddef(p2)^.typ=s32bit) or (porddef(p2)^.typ=u32bit)  then
                begin
                   if is_register then
                     hregister:=p^.location.register
                   else
                     begin
                        exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,newreference(p^.left^.location.reference),R_EDI)));
                        hregister:=R_EDI;
                     end;
                end
              else if porddef(p2)^.typ=u16bit then
                begin
                   if is_register then
                     exprasmlist^.concat(new(pai386,op_reg_reg(A_MOVZX,S_WL,p^.left^.location.register,R_EDI)))
                   else
                     exprasmlist^.concat(new(pai386,op_ref_reg(A_MOVZX,S_WL,newreference(p^.left^.location.reference),R_EDI)));
                   hregister:=R_EDI;
                end
              else if porddef(p2)^.typ=s16bit then
                begin
                   if is_register then
                     exprasmlist^.concat(new(pai386,op_reg_reg(A_MOVSX,S_WL,p^.left^.location.register,R_EDI)))
                   else
                     exprasmlist^.concat(new(pai386,op_ref_reg(A_MOVSX,S_WL,newreference(p^.left^.location.reference),R_EDI)));
                   hregister:=R_EDI;
                end
              else internalerror(6);
              hp:=new_reference(R_NO,0);
              hp^.symbol:=stringdup('R_'+tostr(porddef(p1)^.rangenr));
              if porddef(p1)^.von>porddef(p1)^.bis then
                begin
                   getlabel(neglabel);
                   getlabel(poslabel);
                   exprasmlist^.concat(new(pai386,op_reg_reg(A_OR,S_L,hregister,hregister)));
                   emitl(A_JL,neglabel);
                end;
              exprasmlist^.concat(new(pai386,op_reg_ref(A_BOUND,S_L,hregister,hp)));
              if porddef(p1)^.von>porddef(p1)^.bis then
                begin
                   hp:=new_reference(R_NO,0);
                   hp^.symbol:=stringdup('R_'+tostr(porddef(p1)^.rangenr+1));
                   emitl(A_JMP,poslabel);
                   emitl(A_LABEL,neglabel);
                   exprasmlist^.concat(new(pai386,op_reg_ref(A_BOUND,S_L,hregister,hp)));
                   emitl(A_LABEL,poslabel);
                end;

           end;
      end;

     type
        tsecondconvproc = procedure(p,hp : ptree;convtyp : tconverttype);

    procedure second_nothing(p,hp : ptree;convtyp : tconverttype);

      begin
      end;

    procedure second_only_rangecheck(p,hp : ptree;convtyp : tconverttype);

      begin
         maybe_rangechecking(p,hp^.resulttype,p^.resulttype);
      end;

    procedure second_bigger(p,hp : ptree;convtyp : tconverttype);

      var
         hregister : tregister;
         opsize : topsize;
         op : tasmop;
         is_register : boolean;

      begin
           is_register:=p^.left^.location.loc=LOC_REGISTER;
           if not(is_register) and (p^.left^.location.loc<>LOC_CREGISTER) then
             begin
                del_reference(p^.left^.location.reference);
                { we can do this here as we need no temp inside second_bigger }
                ungetiftemp(p^.left^.location.reference);
             end;
         { this is wrong !!!
         gives me movl (%eax),%eax
         for the length(string !!!
         use only for constant values }
           {Constanst cannot be loaded into registers using MOVZX!}
           if (p^.left^.location.loc<>LOC_MEM) or (not p^.left^.location.reference.isintvalue) then
                case convtyp of
                    tc_u8bit_2_s32bit,tc_u8bit_2_u32bit :
                      begin
                          if is_register then
                            hregister:=reg8toreg32(p^.left^.location.register)
                          else hregister:=getregister32;
                          op:=A_MOVZX;
                          opsize:=S_BL;
                      end;
                    { here what do we do for negative values ? }
                    tc_s8bit_2_s32bit,tc_s8bit_2_u32bit :
                      begin
                          if is_register then
                            hregister:=reg8toreg32(p^.left^.location.register)
                          else hregister:=getregister32;
                          op:=A_MOVSX;
                          opsize:=S_BL;
                      end;
                    tc_u16bit_2_s32bit,tc_u16bit_2_u32bit :
                      begin
                          if is_register then
                            hregister:=reg16toreg32(p^.left^.location.register)
                          else hregister:=getregister32;
                          op:=A_MOVZX;
                          opsize:=S_WL;
                      end;
                    tc_s16bit_2_s32bit,tc_s16bit_2_u32bit :
                      begin
                          if is_register then
                            hregister:=reg16toreg32(p^.left^.location.register)
                          else hregister:=getregister32;
                          op:=A_MOVSX;
                          opsize:=S_WL;
                      end;
                    tc_s8bit_2_u16bit,
                    tc_u8bit_2_s16bit,
                    tc_u8bit_2_u16bit :
                      begin
                          if is_register then
                            hregister:=reg8toreg16(p^.left^.location.register)
                          else hregister:=reg32toreg16(getregister32);
                          op:=A_MOVZX;
                          opsize:=S_BW;
                      end;
                    tc_s8bit_2_s16bit :
                      begin
                          if is_register then
                            hregister:=reg8toreg16(p^.left^.location.register)
                          else hregister:=reg32toreg16(getregister32);
                          op:=A_MOVSX;
                          opsize:=S_BW;
                      end;
                end
           else
                case convtyp of
                    tc_u8bit_2_s32bit,
                    tc_s8bit_2_s32bit,
                    tc_u16bit_2_s32bit,
                    tc_s16bit_2_s32bit,
                    tc_u8bit_2_u32bit,
                    tc_s8bit_2_u32bit,
                    tc_u16bit_2_u32bit,
                    tc_s16bit_2_u32bit:
                      begin
                         hregister:=getregister32;
                         op:=A_MOV;
                         opsize:=S_L;
                      end;
                    tc_s8bit_2_u16bit,
                    tc_s8bit_2_s16bit,
                    tc_u8bit_2_s16bit,
                    tc_u8bit_2_u16bit:
                      begin
                         hregister:=reg32toreg16(getregister32);
                         op:=A_MOV;
                         opsize:=S_W;
                     end;
                end;
           if is_register then
             begin
                 emit_reg_reg(op,opsize,p^.left^.location.register,hregister);
             end
           else
             begin
                 if p^.left^.location.loc=LOC_CREGISTER then
                    emit_reg_reg(op,opsize,p^.left^.location.register,hregister)
                 else exprasmlist^.concat(new(pai386,op_ref_reg(op,opsize,
                    newreference(p^.left^.location.reference),hregister)));
             end;
           p^.location.loc:=LOC_REGISTER;
           p^.location.register:=hregister;
           maybe_rangechecking(p,p^.left^.resulttype,p^.resulttype);
       end;

    procedure second_string_string(p,hp : ptree;convtyp : tconverttype);

      begin
{$ifdef UseAnsiString}
         if is_ansistring(p^.resulttype) and not is_ansistring(p^.left^.resulttype) then
           begin
              { call shortstring to ansistring conversion }
              { result is in register }
              del_reference(p^.left^.location.reference);
              {!!!!
              copyshortstringtoansistring(p^.location,p^.left^.location.reference,pstringdef(p^.resulttype)^.len);
              }
              ungetiftemp(p^.left^.location.reference);
           end
         else if not is_ansistring(p^.resulttype) and is_ansistring(p^.left^.resulttype) then
           begin
              { call ansistring to shortstring conversion }
              { result is in mem }
              stringdispose(p^.location.reference.symbol);
              gettempofsizereference(p^.resulttype^.size,p^.location.reference);
              if p^.left^.location.loc in [LOC_MEM,LOC_REFERENCE] then
                del_reference(p^.left^.location.reference);
              copyansistringtoshortstring(p^.location.reference,p^.left^.location.reference,pstringdef(p^.resulttype)^.len);
              ungetiftemp(p^.left^.location.reference);
           end
         else
{$endif UseAnsiString}
           begin
              stringdispose(p^.location.reference.symbol);
              gettempofsizereference(p^.resulttype^.size,p^.location.reference);
              del_reference(p^.left^.location.reference);
              copystring(p^.location.reference,p^.left^.location.reference,pstringdef(p^.resulttype)^.len);
              ungetiftemp(p^.left^.location.reference);
           end;
      end;

    procedure second_cstring_charpointer(p,hp : ptree;convtyp : tconverttype);

      begin
         p^.location.loc:=LOC_REGISTER;
         p^.location.register:=getregister32;
         inc(p^.left^.location.reference.offset);
           exprasmlist^.concat(new(pai386,op_ref_reg(A_LEA,S_L,newreference(p^.left^.location.reference),
             p^.location.register)));
      end;

    procedure second_string_chararray(p,hp : ptree;convtyp : tconverttype);

      begin
         inc(p^.location.reference.offset);
      end;

    procedure second_array_to_pointer(p,hp : ptree;convtyp : tconverttype);

      begin
         del_reference(p^.left^.location.reference);
         p^.location.loc:=LOC_REGISTER;
         p^.location.register:=getregister32;
         exprasmlist^.concat(new(pai386,op_ref_reg(A_LEA,S_L,newreference(p^.left^.location.reference),
           p^.location.register)));
      end;

    procedure second_pointer_to_array(p,hp : ptree;convtyp : tconverttype);

      begin
         p^.location.loc:=LOC_REFERENCE;
         clear_reference(p^.location.reference);
         if p^.left^.location.loc=LOC_REGISTER then
           p^.location.reference.base:=p^.left^.location.register
         else
           begin
              if p^.left^.location.loc=LOC_CREGISTER then
                begin
                   p^.location.reference.base:=getregister32;
                   emit_reg_reg(A_MOV,S_L,p^.left^.location.register,
                     p^.location.reference.base);
                end
              else
                begin
                   del_reference(p^.left^.location.reference);
                   p^.location.reference.base:=getregister32;
                   exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,newreference(p^.left^.location.reference),
                     p^.location.reference.base)));
                end;
           end;
      end;

    { generates the code for the type conversion from an array of char }
    { to a string                                                        }
    procedure second_chararray_to_string(p,hp : ptree;convtyp : tconverttype);

      var
         l : longint;

      begin
         { this is a type conversion which copies the data, so we can't }
         { return a reference                                             }
         p^.location.loc:=LOC_MEM;

         { first get the memory for the string }
         stringdispose(p^.location.reference.symbol);
         gettempofsizereference(256,p^.location.reference);

         { calc the length of the array }
         l:=parraydef(p^.left^.resulttype)^.highrange-
           parraydef(p^.left^.resulttype)^.lowrange+1;

         if l>255 then
           Message(sym_e_type_mismatch);

         { write the length }
             exprasmlist^.concat(new(pai386,op_const_ref(A_MOV,S_B,l,
               newreference(p^.location.reference))));

         { copy to first char of string }
         inc(p^.location.reference.offset);

         { generates the copy code      }
         { and we need the source never }
         concatcopy(p^.left^.location.reference,p^.location.reference,l,true);

         { correct the string location }
         dec(p^.location.reference.offset);
      end;

    procedure second_char_to_string(p,hp : ptree;convtyp : tconverttype);

      begin
         stringdispose(p^.location.reference.symbol);
         gettempofsizereference(256,p^.location.reference);
      { call loadstring with correct left and right }
         p^.right:=p^.left;
         p^.left:=p;
         loadstring(p);
         p^.left:=nil; { reset left tree, which is empty }
      end;

    procedure second_int_real(p,hp : ptree;convtyp : tconverttype);

      var
         r : preference;

      begin
         if (p^.left^.location.loc=LOC_REGISTER) or
            (p^.left^.location.loc=LOC_CREGISTER) then
           begin
              case porddef(p^.left^.resulttype)^.typ of
                 s8bit : exprasmlist^.concat(new(pai386,op_reg_reg(A_MOVSX,S_BL,p^.left^.location.register,R_EDI)));
                 u8bit : exprasmlist^.concat(new(pai386,op_reg_reg(A_MOVZX,S_BL,p^.left^.location.register,R_EDI)));
                 s16bit : exprasmlist^.concat(new(pai386,op_reg_reg(A_MOVSX,S_WL,p^.left^.location.register,R_EDI)));
                 u16bit : exprasmlist^.concat(new(pai386,op_reg_reg(A_MOVZX,S_WL,p^.left^.location.register,R_EDI)));
                 u32bit,s32bit : exprasmlist^.concat(new(pai386,op_reg_reg(A_MOV,S_L,p^.left^.location.register,R_EDI)));
                 {!!!! u32bit }
              end;
              ungetregister(p^.left^.location.register);
           end
         else
           begin
              r:=newreference(p^.left^.location.reference);
              case porddef(p^.left^.resulttype)^.typ of
                 s8bit : exprasmlist^.concat(new(pai386,op_ref_reg(A_MOVSX,S_BL,r,R_EDI)));
                 u8bit : exprasmlist^.concat(new(pai386,op_ref_reg(A_MOVZX,S_BL,r,R_EDI)));
                 s16bit : exprasmlist^.concat(new(pai386,op_ref_reg(A_MOVSX,S_WL,r,R_EDI)));
                 u16bit : exprasmlist^.concat(new(pai386,op_ref_reg(A_MOVZX,S_WL,r,R_EDI)));
                 u32bit,s32bit : exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,r,R_EDI)));
                 {!!!! u32bit }
              end;
              del_reference(p^.left^.location.reference);
              ungetiftemp(p^.left^.location.reference);
         end;
          if porddef(p^.left^.resulttype)^.typ=u32bit then
            push_int(0);
          exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,R_EDI)));
          r:=new_reference(R_ESP,0);
         { for u32bit a solution is to push $0 and to load a
         comp }
          if porddef(p^.left^.resulttype)^.typ=u32bit then
            exprasmlist^.concat(new(pai386,op_ref(A_FILD,S_IQ,r)))
          else
            exprasmlist^.concat(new(pai386,op_ref(A_FILD,S_IL,r)));

         { better than an add on all processors }
         if porddef(p^.left^.resulttype)^.typ=u32bit then
           exprasmlist^.concat(new(pai386,op_const_reg(A_ADD,S_L,8,R_ESP)))
         else
           exprasmlist^.concat(new(pai386,op_reg(A_POP,S_L,R_EDI)));

         p^.location.loc:=LOC_FPU;
      end;

    procedure second_real_fix(p,hp : ptree;convtyp : tconverttype);

      var
         {hs : string;}
         rreg : tregister;
         ref : treference;

      begin
         { real must be on fpu stack }
         if (p^.left^.location.loc<>LOC_FPU) then
           exprasmlist^.concat(new(pai386,op_ref(A_FLD,S_FL,newreference(p^.left^.location.reference))));
         push_int($1f3f);
         push_int(65536);
         reset_reference(ref);
         ref.base:=R_ESP;

         exprasmlist^.concat(new(pai386,op_ref(A_FIMUL,S_IL,newreference(ref))));

         ref.offset:=4;
         exprasmlist^.concat(new(pai386,op_ref(A_FSTCW,S_L,newreference(ref))));

         ref.offset:=6;
         exprasmlist^.concat(new(pai386,op_ref(A_FLDCW,S_L,newreference(ref))));

         ref.offset:=0;
         exprasmlist^.concat(new(pai386,op_ref(A_FISTP,S_IL,newreference(ref))));

         ref.offset:=4;
         exprasmlist^.concat(new(pai386,op_ref(A_FLDCW,S_L,newreference(ref))));

         rreg:=getregister32;
         exprasmlist^.concat(new(pai386,op_reg(A_POP,S_L,rreg)));
         { better than an add on all processors }
         exprasmlist^.concat(new(pai386,op_reg(A_POP,S_L,R_EDI)));

         p^.location.loc:=LOC_REGISTER;
         p^.location.register:=rreg;
      end;

    procedure second_float_float(p,hp : ptree;convtyp : tconverttype);

      begin
         case p^.left^.location.loc of
            LOC_FPU : ;
            LOC_MEM,
            LOC_REFERENCE:
              begin
                 floatload(pfloatdef(p^.left^.resulttype)^.typ,
                   p^.left^.location.reference);
                 { we have to free the reference }
                 del_reference(p^.left^.location.reference);
              end;
         end;
         p^.location.loc:=LOC_FPU;
      end;

    procedure second_fix_real(p,hp : ptree;convtyp : tconverttype);

    var popeax,popebx,popecx,popedx : boolean;
        startreg : tregister;
        hl : plabel;
        r : treference;

      begin
         if (p^.left^.location.loc=LOC_REGISTER) or
            (p^.left^.location.loc=LOC_CREGISTER) then
           begin
              startreg:=p^.left^.location.register;
              ungetregister(startreg);
              popeax:=(startreg<>R_EAX) and not (R_EAX in unused);
              if popeax then
                exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,R_EAX)));
              { mov eax,eax is removed by emit_reg_reg }
              emit_reg_reg(A_MOV,S_L,startreg,R_EAX);
           end
         else
           begin
              exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,newreference(
                p^.left^.location.reference),R_EAX)));
              del_reference(p^.left^.location.reference);
              startreg:=R_NO;
           end;

         popebx:=(startreg<>R_EBX) and not (R_EBX in unused);
         if popebx then
           exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,R_EBX)));

         popecx:=(startreg<>R_ECX) and not (R_ECX in unused);
         if popecx then
           exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,R_ECX)));

         popedx:=(startreg<>R_EDX) and not (R_EDX in unused);
         if popedx then
           exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,R_EDX)));

         exprasmlist^.concat(new(pai386,op_none(A_CDQ,S_NO)));
         emit_reg_reg(A_XOR,S_L,R_EDX,R_EAX);
         emit_reg_reg(A_MOV,S_L,R_EAX,R_EBX);
         emit_reg_reg(A_SUB,S_L,R_EDX,R_EAX);
         getlabel(hl);
         emitl(A_JZ,hl);
         exprasmlist^.concat(new(pai386,op_const_reg(A_RCL,S_L,1,R_EBX)));
         emit_reg_reg(A_BSR,S_L,R_EAX,R_EDX);
         exprasmlist^.concat(new(pai386,op_const_reg(A_MOV,S_B,32,R_CL)));
         emit_reg_reg(A_SUB,S_B,R_DL,R_CL);
         emit_reg_reg(A_SHL,S_L,R_CL,R_EAX);
         exprasmlist^.concat(new(pai386,op_const_reg(A_ADD,S_W,1007,R_DX)));
         exprasmlist^.concat(new(pai386,op_const_reg(A_SHL,S_W,5,R_DX)));
         exprasmlist^.concat(new(pai386,op_const_reg_reg(A_SHLD,S_W,11,R_DX,R_BX)));
         exprasmlist^.concat(new(pai386,op_const_reg_reg(A_SHLD,S_W,20,R_EAX,R_EBX)));

         exprasmlist^.concat(new(pai386,op_const_reg(A_SHL,S_L,20,R_EAX)));
         emitl(A_LABEL,hl);
         { better than an add on all processors }
         exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,R_EBX)));
         exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,R_EAX)));

         reset_reference(r);
         r.base:=R_ESP;
         exprasmlist^.concat(new(pai386,op_ref(A_FLD,S_FL,newreference(r))));
         exprasmlist^.concat(new(pai386,op_const_reg(A_ADD,S_L,8,R_ESP)));
         if popedx then
           exprasmlist^.concat(new(pai386,op_reg(A_POP,S_L,R_EDX)));
         if popecx then
           exprasmlist^.concat(new(pai386,op_reg(A_POP,S_L,R_ECX)));
         if popebx then
           exprasmlist^.concat(new(pai386,op_reg(A_POP,S_L,R_EBX)));
         if popeax then
           exprasmlist^.concat(new(pai386,op_reg(A_POP,S_L,R_EAX)));

         p^.location.loc:=LOC_FPU;
      end;

    procedure second_int_fix(p,hp : ptree;convtyp : tconverttype);

      var
         {hs : string;}
         hregister : tregister;

      begin
         if (p^.left^.location.loc=LOC_REGISTER) then
           hregister:=p^.left^.location.register
         else if (p^.left^.location.loc=LOC_CREGISTER) then
           hregister:=getregister32
         else
           begin
              del_reference(p^.left^.location.reference);
              hregister:=getregister32;
              case porddef(p^.left^.resulttype)^.typ of
                s8bit : exprasmlist^.concat(new(pai386,op_ref_reg(A_MOVSX,S_BL,newreference(p^.left^.location.reference),
                  hregister)));
                u8bit : exprasmlist^.concat(new(pai386,op_ref_reg(A_MOVZX,S_BL,newreference(p^.left^.location.reference),
                  hregister)));
                s16bit : exprasmlist^.concat(new(pai386,op_ref_reg(A_MOVSX,S_WL,newreference(p^.left^.location.reference),
                  hregister)));
                u16bit : exprasmlist^.concat(new(pai386,op_ref_reg(A_MOVZX,S_WL,newreference(p^.left^.location.reference),
                  hregister)));
                u32bit,s32bit : exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,newreference(p^.left^.location.reference),
                  hregister)));
                {!!!! u32bit }
              end;
           end;
         exprasmlist^.concat(new(pai386,op_const_reg(A_SHL,S_L,16,hregister)));

         p^.location.loc:=LOC_REGISTER;
         p^.location.register:=hregister;
      end;

    procedure second_smaller(p,hp : ptree;convtyp : tconverttype);

      var
         hregister,destregister : tregister;
         ref : boolean;
         hpp : preference;

      begin
         ref:=false;
         { problems with enums !! }
         if (cs_rangechecking in aktswitches)  and
           { with $R+ explicit type conversations in TP aren't range checked! }
           (not(p^.explizit) or not(cs_tp_compatible in aktswitches)) and
           (p^.resulttype^.deftype=orddef) and
           (hp^.resulttype^.deftype=orddef) then
           begin
              if porddef(hp^.resulttype)^.typ=u32bit then
                begin
                   { when doing range checking for u32bit, we have some trouble }
                   { because BOUND assumes signed values                        }
                   { first, we check if the values is greater than 2^31:        }
                   { the u32bit rangenr contains the appropriate rangenr        }
                   porddef(hp^.resulttype)^.genrangecheck;
                   hregister:=R_EDI;
                   if (p^.location.loc=LOC_REGISTER) or
                      (p^.location.loc=LOC_CREGISTER) then
                     hregister:=p^.location.register
                   else
                     exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,
                       newreference(p^.location.reference),R_EDI)));
                   hpp:=new_reference(R_NO,0);
                   hpp^.symbol:=stringdup('R_'+tostr(porddef(hp^.resulttype)^.rangenr));
                   exprasmlist^.concat(new(pai386,op_reg_ref(A_BOUND,S_L,hregister,hpp)));

                   { then we do a normal range check }
                   porddef(p^.resulttype)^.genrangecheck;
                   hpp:=new_reference(R_NO,0);
                   hpp^.symbol:=stringdup('R_'+tostr(porddef(p^.resulttype)^.rangenr));
                   exprasmlist^.concat(new(pai386,op_reg_ref(A_BOUND,S_L,hregister,hpp)));
                end
              else
                if ((porddef(p^.resulttype)^.von>porddef(hp^.resulttype)^.von) or
                (porddef(p^.resulttype)^.bis<porddef(hp^.resulttype)^.bis)) then
                begin
                   porddef(p^.resulttype)^.genrangecheck;
                   { per default the var is copied to EDI }
                   hregister:=R_EDI;
                   if porddef(hp^.resulttype)^.typ=s32bit then
                     begin
                        if (p^.location.loc=LOC_REGISTER) or
                           (p^.location.loc=LOC_CREGISTER) then
                          hregister:=p^.location.register
                        else
                          exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,newreference(p^.location.reference),R_EDI)));
                     end
                   else if porddef(hp^.resulttype)^.typ=u16bit then
                     begin
                        if (p^.location.loc=LOC_REGISTER) or
                           (p^.location.loc=LOC_CREGISTER) then
                          exprasmlist^.concat(new(pai386,op_reg_reg(A_MOVZX,S_WL,p^.location.register,R_EDI)))
                        else
                          exprasmlist^.concat(new(pai386,op_ref_reg(A_MOVZX,S_WL,newreference(p^.location.reference),R_EDI)));
                     end
                   else if porddef(hp^.resulttype)^.typ=s16bit then
                     begin
                        if (p^.location.loc=LOC_REGISTER) or
                           (p^.location.loc=LOC_CREGISTER) then
                          exprasmlist^.concat(new(pai386,op_reg_reg(A_MOVSX,S_WL,p^.location.register,R_EDI)))
                        else
                          exprasmlist^.concat(new(pai386,op_ref_reg(A_MOVSX,S_WL,newreference(p^.location.reference),R_EDI)));
                     end
                   else internalerror(6);
                   hpp:=new_reference(R_NO,0);
                   hpp^.symbol:=stringdup('R_'+tostr(porddef(p^.resulttype)^.rangenr));
                   exprasmlist^.concat(new(pai386,op_reg_ref(A_BOUND,S_L,hregister,hpp)));
                   (*
                   if (p^.location.loc=LOC_REGISTER) or
                      (p^.location.loc=LOC_CREGISTER) then
                     begin
                        destregister:=p^.left^.location.register;
                        case convtyp of
                           tc_s32bit_2_s8bit,
                           tc_s32bit_2_u8bit:
                             destregister:=reg32toreg8(destregister);
                           tc_s32bit_2_s16bit,
                           tc_s32bit_2_u16bit:
                             destregister:=reg32toreg16(destregister);
                           { this was false because destregister is allways a 32bitreg }
                           tc_s16bit_2_s8bit,
                           tc_s16bit_2_u8bit,
                           tc_u16bit_2_s8bit,
                           tc_u16bit_2_u8bit:
                             destregister:=reg32toreg8(destregister);
                        end;
                   p^.location.register:=destregister;
                   exit;
                   *)
                end;
           end;
         { p^.location.loc is already set! }
         if (p^.location.loc=LOC_REGISTER) or
           (p^.location.loc=LOC_CREGISTER) then
           begin
              destregister:=p^.left^.location.register;
              case convtyp of
                 tc_s32bit_2_s8bit,
                 tc_s32bit_2_u8bit:
                   destregister:=reg32toreg8(destregister);
                 tc_s32bit_2_s16bit,
                 tc_s32bit_2_u16bit:
                   destregister:=reg32toreg16(destregister);
                 tc_s16bit_2_s8bit,
                 tc_s16bit_2_u8bit,
                 tc_u16bit_2_s8bit,
                 tc_u16bit_2_u8bit:
                   destregister:=reg16toreg8(destregister);
              end;
              p^.location.register:=destregister;
           end;
      end;

     procedure second_proc_to_procvar(p,hp : ptree;convtyp : tconverttype);

     begin
          secondpass(hp);
          p^.location.loc:=LOC_REGISTER;
          del_reference(hp^.location.reference);
          p^.location.register:=getregister32;
          exprasmlist^.concat(new(pai386,op_ref_reg(A_LEA,S_L,
           newreference(hp^.location.reference),p^.location.register)));
     end;

     procedure second_bool_to_byte(p,hp : ptree;convtyp : tconverttype);

      var
         oldtruelabel,oldfalselabel,hlabel : plabel;

     begin
         oldtruelabel:=truelabel;
         oldfalselabel:=falselabel;
         getlabel(truelabel);
         getlabel(falselabel);
          secondpass(hp);
          p^.location.loc:=LOC_REGISTER;
          del_reference(hp^.location.reference);
          p^.location.register:=reg32toreg8(getregister32);
          case hp^.location.loc of
            LOC_MEM,LOC_REFERENCE :
              exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_B,
                newreference(hp^.location.reference),p^.location.register)));
            LOC_REGISTER,LOC_CREGISTER :
              exprasmlist^.concat(new(pai386,op_reg_reg(A_MOV,S_B,
                hp^.location.register,p^.location.register)));
           LOC_FLAGS:
              begin
                 exprasmlist^.concat(new(pai386,op_reg(flag_2_set[hp^.location.resflags],S_NO,
                   p^.location.register)))
              end;
           LOC_JUMP:
             begin
                getlabel(hlabel);
                emitl(A_LABEL,truelabel);
                exprasmlist^.concat(new(pai386,op_const_reg(A_MOV,S_B,
                  1,p^.location.register)));
                emitl(A_JMP,hlabel);
                emitl(A_LABEL,falselabel);
                exprasmlist^.concat(new(pai386,op_reg_reg(A_XOR,S_B,
                  p^.location.register,
                  p^.location.register)));
                emitl(A_LABEL,hlabel);
             end;
          else
            internalerror(10060);
          end;
         truelabel:=oldtruelabel;
         falselabel:=oldfalselabel;
     end;

    procedure secondtypeconv(var p : ptree);

      const
         secondconvert : array[tc_u8bit_2_s32bit..tc_cchar_charpointer] of
           tsecondconvproc = (second_bigger,second_only_rangecheck,
           second_bigger,second_bigger,second_bigger,
           second_smaller,second_smaller,
           second_smaller,second_string_string,
           second_cstring_charpointer,second_string_chararray,
           second_array_to_pointer,second_pointer_to_array,
           second_char_to_string,second_bigger,
           second_bigger,second_bigger,
           second_smaller,second_smaller,
           second_smaller,second_smaller,
           second_bigger,second_smaller,
           second_only_rangecheck,second_bigger,
           second_bigger,second_bigger,
           second_bigger,second_only_rangecheck,
           second_smaller,second_smaller,
           second_smaller,second_smaller,
           second_int_real,second_real_fix,
           second_fix_real,second_int_fix,second_float_float,
           second_chararray_to_string,second_bool_to_byte,
           second_proc_to_procvar,
           { is constant char to pchar, is done by firstpass }
           second_nothing);

      begin
         { this isn't good coding, I think tc_bool_2_u8bit, shouldn't be }
         { type conversion (FK)                                        }

         { this is necessary, because second_bool_byte, have to change   }
         { true- and false label before calling secondpass               }
         if p^.convtyp<>tc_bool_2_u8bit then
           begin
              secondpass(p^.left);
              set_location(p^.location,p^.left^.location);
           end;
         if (p^.convtyp<>tc_equal) and (p^.convtyp<>tc_not_possible) then
           {the second argument only is for maybe_range_checking !}
           secondconvert[p^.convtyp](p,p^.left,p^.convtyp)
      end;


    procedure secondassignment(var p : ptree);

      var
         opsize : topsize;
         otlabel,hlabel,oflabel : plabel;
         hregister : tregister;
         loc : tloc;

      begin
         otlabel:=truelabel;
         oflabel:=falselabel;
         getlabel(truelabel);
         getlabel(falselabel);
         { calculate left sides }
         if not(p^.concat_string) then
           secondpass(p^.left);

         if codegenerror then
           exit;

         case p^.left^.location.loc of
            LOC_REFERENCE : begin
                              { in case left operator uses to register }
                              { but to few are free then LEA }
                              if (p^.left^.location.reference.base<>R_NO) and
                                 (p^.left^.location.reference.index<>R_NO) and
                                 (usablereg32<p^.right^.registers32) then
                                begin
                                   del_reference(p^.left^.location.reference);
                                   hregister:=getregister32;
                                   exprasmlist^.concat(new(pai386,op_ref_reg(A_LEA,S_L,newreference(
                                     p^.left^.location.reference),
                                     hregister)));
                                   clear_reference(p^.left^.location.reference);
                                   p^.left^.location.reference.base:=hregister;
                                   p^.left^.location.reference.index:=R_NO;
                                end;
                              loc:=LOC_REFERENCE;
                           end;
            LOC_CREGISTER:
              loc:=LOC_CREGISTER;
            LOC_MMXREGISTER:
              loc:=LOC_MMXREGISTER;
            LOC_CMMXREGISTER:
              loc:=LOC_CMMXREGISTER;
            else
               begin
                  Message(cg_e_illegal_expression);
                  exit;
               end;
         end;
         { lets try to optimize this (PM)             }
         { define a dest_loc that is the location      }
         { and a ptree to verify that it is the right }
         { place to insert it                         }
{$ifdef test_dest_loc}
         if (aktexprlevel<4) then
           begin
              dest_loc_known:=true;
              dest_loc:=p^.left^.location;
              dest_loc_tree:=p^.right;
           end;
{$endif test_dest_loc}

         if (p^.right^.treetype=realconstn) then
           begin
              if p^.left^.resulttype^.deftype=floatdef then
                begin
                   case pfloatdef(p^.left^.resulttype)^.typ of
                     s32real : p^.right^.realtyp:=ait_real_32bit;
                     s64real : p^.right^.realtyp:=ait_real_64bit;
                     s80real : p^.right^.realtyp:=ait_real_extended;
                     { what about f32bit and s64bit }
                     end;
                end;
           end;
         secondpass(p^.right);

         if codegenerror then
           exit;

{$ifdef test_dest_loc}
         dest_loc_known:=false;
         if in_dest_loc then
           begin
              truelabel:=otlabel;
              falselabel:=oflabel;
              in_dest_loc:=false;
              exit;
           end;
{$endif test_dest_loc}
         if p^.left^.resulttype^.deftype=stringdef then
           begin
{$ifdef UseAnsiString}
              if is_ansistring(p^.left^.resulttype) then
                begin
                  { the source and destinations are released
                    in loadansistring, because an ansi string can
                    also be in a register
                  }
                  loadansistring(p);
                end
              else
{$endif UseAnsiString}
              if not (p^.concat_string) then
                begin
                  { we do not need destination anymore }
                  del_reference(p^.left^.location.reference);
                  del_reference(p^.right^.location.reference);
                  loadstring(p);
                  ungetiftemp(p^.right^.location.reference);
                end
              else
                begin
                  { its the only thing we have to do }
                  del_reference(p^.right^.location.reference);
                end
           end
        else case p^.right^.location.loc of
            LOC_REFERENCE,
            LOC_MEM : begin
                         { handle ordinal constants trimmed }
                         if (p^.right^.treetype in [ordconstn,fixconstn]) or
                            (loc=LOC_CREGISTER) then
                           begin
                              case p^.left^.resulttype^.size of
                                 1 : opsize:=S_B;
                                 2 : opsize:=S_W;
                                 4 : opsize:=S_L;
                              end;
                              if loc=LOC_CREGISTER then
                                exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,opsize,
                                  newreference(p^.right^.location.reference),
                                  p^.left^.location.register)))
                              else
                                exprasmlist^.concat(new(pai386,op_const_ref(A_MOV,opsize,
                                  p^.right^.location.reference.offset,
                                  newreference(p^.left^.location.reference))));
                              {exprasmlist^.concat(new(pai386,op_const_loc(A_MOV,opsize,
                                  p^.right^.location.reference.offset,
                                  p^.left^.location)));}
                           end
                         else
                           begin
                              concatcopy(p^.right^.location.reference,
                                p^.left^.location.reference,p^.left^.resulttype^.size,false);
                              ungetiftemp(p^.right^.location.reference);
                           end;
                      end;
{$ifdef SUPPORT_MMX}
            LOC_CMMXREGISTER,
            LOC_MMXREGISTER:
              begin
                 if loc=LOC_CMMXREGISTER then
                   exprasmlist^.concat(new(pai386,op_reg_reg(A_MOVQ,S_NO,
                   p^.right^.location.register,p^.left^.location.register)))
                 else
                   exprasmlist^.concat(new(pai386,op_reg_ref(A_MOVQ,S_NO,
                     p^.right^.location.register,newreference(p^.left^.location.reference))));
              end;
{$endif SUPPORT_MMX}
            LOC_REGISTER,
            LOC_CREGISTER : begin
                              case p^.right^.resulttype^.size of
                                 1 : opsize:=S_B;
                                 2 : opsize:=S_W;
                                 4 : opsize:=S_L;
                              end;
                              { simplified with op_reg_loc         }
                              if loc=LOC_CREGISTER then
                                exprasmlist^.concat(new(pai386,op_reg_reg(A_MOV,opsize,
                                  p^.right^.location.register,
                                  p^.left^.location.register)))
                              else
                                exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,opsize,
                                  p^.right^.location.register,
                                  newreference(p^.left^.location.reference))));
                              {exprasmlist^.concat(new(pai386,op_reg_loc(A_MOV,opsize,
                                  p^.right^.location.register,
                                  p^.left^.location)));             }

                           end;
            LOC_FPU : begin
                              if loc<>LOC_REFERENCE then
                                internalerror(10010)
                              else
                                floatstore(pfloatdef(p^.left^.resulttype)^.typ,
                                  p^.left^.location.reference);
                           end;
            LOC_JUMP     : begin
                              getlabel(hlabel);
                              emitl(A_LABEL,truelabel);
                              if loc=LOC_CREGISTER then
                                exprasmlist^.concat(new(pai386,op_const_reg(A_MOV,S_B,
                                  1,p^.left^.location.register)))
                              else
                                exprasmlist^.concat(new(pai386,op_const_ref(A_MOV,S_B,
                                  1,newreference(p^.left^.location.reference))));
                              {exprasmlist^.concat(new(pai386,op_const_loc(A_MOV,S_B,
                                  1,p^.left^.location)));}
                              emitl(A_JMP,hlabel);
                              emitl(A_LABEL,falselabel);
                              if loc=LOC_CREGISTER then
                                exprasmlist^.concat(new(pai386,op_reg_reg(A_XOR,S_B,
                                  p^.left^.location.register,
                                  p^.left^.location.register)))
                              else
                                exprasmlist^.concat(new(pai386,op_const_ref(A_MOV,S_B,
                                  0,newreference(p^.left^.location.reference))));
                              emitl(A_LABEL,hlabel);
                           end;
            LOC_FLAGS    : begin
                              if loc=LOC_CREGISTER then
                                exprasmlist^.concat(new(pai386,op_reg(flag_2_set[p^.right^.location.resflags],S_NO,
                                  p^.left^.location.register)))
                              else
                                exprasmlist^.concat(new(pai386,op_ref(flag_2_set[p^.right^.location.resflags],S_NO,
                                  newreference(p^.left^.location.reference))));
                           end;
         end;
         truelabel:=otlabel;
         falselabel:=oflabel;
      end;



    { save the size of pushed parameter }
    var
       pushedparasize : longint;

    procedure secondcallparan(var p : ptree;defcoll : pdefcoll;
                push_from_left_to_right : boolean
                ;inlined : boolean;
                para_offset : longint
                );

      procedure maybe_push_open_array_high;

        var
           r : preference;

        begin
           { open array ? }
           { defcoll^.data can be nil for read/write }
           if assigned(defcoll^.data) and
             is_open_array(defcoll^.data) then
             begin
                inc(pushedparasize,4);
                { push high }
                if is_open_array(p^.left^.resulttype) then
                  begin
                     r:=new_reference(highframepointer,highoffset+4);
                     if inlined then
                       begin
                          exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,r,R_EDI)));
                          r:=new_reference(procinfo.framepointer,para_offset-pushedparasize);
                          exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_L,R_EDI,r)));
                       end
                     else
                     exprasmlist^.concat(new(pai386,op_ref(A_PUSH,S_L,r)));
                  end
                else
                     if inlined then
                       begin
                          r:=new_reference(procinfo.framepointer,para_offset-pushedparasize);
                          exprasmlist^.concat(new(pai386,op_const_ref(A_MOV,S_L,
                            parraydef(p^.left^.resulttype)^.highrange-
                            parraydef(p^.left^.resulttype)^.lowrange,r)));
                       end
                     else
                  push_int(parraydef(p^.left^.resulttype)^.highrange-
                           parraydef(p^.left^.resulttype)^.lowrange);
             end;
        end;

      var
         size : longint;
         stackref : treference;
         otlabel,hlabel,oflabel : plabel;


         { temporary variables: }
         tempdeftype : tdeftype;
         tempreference : treference;
         r : preference;
         s : topsize;
         op : tasmop;

      begin
         { push from left to right if specified }
         if push_from_left_to_right and assigned(p^.right) then
           secondcallparan(p^.right,defcoll^.next,push_from_left_to_right
             ,inlined,para_offset
           );
         otlabel:=truelabel;
         oflabel:=falselabel;
         getlabel(truelabel);
         getlabel(falselabel);
         secondpass(p^.left);
         { in codegen.handleread.. defcoll^.data is set to nil }
         if assigned(defcoll^.data) and
           (defcoll^.data^.deftype=formaldef) then
           begin
              { allow @var }
              inc(pushedparasize,4);
              if p^.left^.treetype=addrn then
                begin
                   { always a register }
                     if inlined then
                       begin
                          r:=new_reference(procinfo.framepointer,para_offset-pushedparasize);
                          exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_L,
                            p^.left^.location.register,r)));
                       end
                     else
                   exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,p^.left^.location.register)));
                   ungetregister32(p^.left^.location.register);
                end
              else
                begin
                   if (p^.left^.location.loc<>LOC_REFERENCE) and
                      (p^.left^.location.loc<>LOC_MEM) then
                     Message(sym_e_type_mismatch)
                   else
                     begin
                     if inlined then
                       begin
                          exprasmlist^.concat(new(pai386,op_ref_reg(A_LEA,S_L,
                            newreference(p^.left^.location.reference),R_EDI)));
                          r:=new_reference(procinfo.framepointer,para_offset-pushedparasize);
                          exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_L,
                            R_EDI,r)));
                       end
                     else
                        emitpushreferenceaddr(p^.left^.location.reference);
                        del_reference(p^.left^.location.reference);
                     end;
                end;
           end
         { handle call by reference parameter }
         else if (defcoll^.paratyp=vs_var) then
           begin
              if (p^.left^.location.loc<>LOC_REFERENCE) then
                Message(cg_e_var_must_be_reference);
              maybe_push_open_array_high;
              inc(pushedparasize,4);
                     if inlined then
                       begin
                          exprasmlist^.concat(new(pai386,op_ref_reg(A_LEA,S_L,
                            newreference(p^.left^.location.reference),R_EDI)));
                          r:=new_reference(procinfo.framepointer,para_offset-pushedparasize);
                          exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_L,
                            R_EDI,r)));
                       end
                     else
              emitpushreferenceaddr(p^.left^.location.reference);
              del_reference(p^.left^.location.reference);
           end
         else
           begin
              tempdeftype:=p^.resulttype^.deftype;
              if tempdeftype=filedef then
               Message(cg_e_file_must_call_by_reference);
              if (defcoll^.paratyp=vs_const) and
                 dont_copy_const_param(p^.resulttype) then
                begin
                   maybe_push_open_array_high;
                   inc(pushedparasize,4);
                     if inlined then
                       begin
                          exprasmlist^.concat(new(pai386,op_ref_reg(A_LEA,S_L,
                            newreference(p^.left^.location.reference),R_EDI)));
                          r:=new_reference(procinfo.framepointer,para_offset-pushedparasize);
                          exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_L,
                            R_EDI,r)));
                       end
                     else
                     emitpushreferenceaddr(p^.left^.location.reference);
                   del_reference(p^.left^.location.reference);
                end
              else
                case p^.left^.location.loc of
                   LOC_REGISTER,
                   LOC_CREGISTER:
                     begin
                        case p^.left^.location.register of
                           R_EAX,R_EBX,R_ECX,R_EDX,R_ESI,
                           R_EDI,R_ESP,R_EBP :
                             begin
                                inc(pushedparasize,4);
                     if inlined then
                       begin
                          r:=new_reference(procinfo.framepointer,para_offset-pushedparasize);
                          exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_L,
                            p^.left^.location.register,r)));
                       end
                     else
                                exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,p^.left^.location.register)));
                                ungetregister32(p^.left^.location.register);
                             end;
                           R_AX,R_BX,R_CX,R_DX,R_SI,R_DI:
                             begin
                                 inc(pushedparasize,2);
                     if inlined then
                       begin
                          r:=new_reference(procinfo.framepointer,para_offset-pushedparasize);
                          exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_W,
                            p^.left^.location.register,r)));
                       end
                     else
                                 exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_W,p^.left^.location.register)));
                                 ungetregister32(reg16toreg32(p^.left^.location.register));
                              end;
                           R_AL,R_BL,R_CL,R_DL:
                             begin
                                inc(pushedparasize,2);
                                { we must push always 16 bit }
                     if inlined then
                       begin
                          r:=new_reference(procinfo.framepointer,para_offset-pushedparasize);
                          exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_L,
                            reg8toreg16(p^.left^.location.register),r)));
                       end
                     else
                                exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_W,
                                  reg8toreg16(p^.left^.location.register))));
                                ungetregister32(reg8toreg32(p^.left^.location.register));
                             end;
                        end;
                     end;
                   LOC_FPU:
                     begin
                        size:=pfloatdef(p^.left^.resulttype)^.size;
                        inc(pushedparasize,size); { must be before for inlined }
                        if not inlined then
                        exprasmlist^.concat(new(pai386,op_const_reg(A_SUB,S_L,size,R_ESP)));
                        r:=new_reference(R_ESP,0);
                        floatstoreops(pfloatdef(p^.left^.resulttype)^.typ,op,s);
                        { this is the easiest case for inlined !! }
                        if inlined then
                          begin
                             r^.base:=procinfo.framepointer;
                             r^.offset:=para_offset-pushedparasize;
                          end;
                        exprasmlist^.concat(new(pai386,op_ref(op,s,r)));
                     end;
                   LOC_REFERENCE,LOC_MEM:
                     begin
                        tempreference:=p^.left^.location.reference;
                        del_reference(p^.left^.location.reference);
                        case p^.resulttype^.deftype of
                           orddef :
                             begin
                                case porddef(p^.resulttype)^.typ of
                                  s32bit,u32bit :
                                    begin
                                       inc(pushedparasize,4);
                                       if inlined then
                                         begin
                                            exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,
                                              newreference(tempreference),R_EDI)));
                                            r:=new_reference(procinfo.framepointer,para_offset-pushedparasize);
                                            exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_L,
                                            R_EDI,r)));
                                         end
                                       else
                                         emit_push_mem(tempreference);
                                    end;
                                  s8bit,u8bit,uchar,bool8bit,s16bit,u16bit :
                                    begin
                                      inc(pushedparasize,2);
                                      if inlined then
                                        begin
                                           exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_W,
                                             newreference(tempreference),R_DI)));
                                           r:=new_reference(procinfo.framepointer,para_offset-pushedparasize);
                                           exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_W,
                                           R_DI,r)));
                                        end
                                      else
                                        exprasmlist^.concat(new(pai386,op_ref(A_PUSH,S_W,
                                          newreference(tempreference))));
                                    end;
                                end;
                             end;
                           floatdef :
                           begin
                             case pfloatdef(p^.resulttype)^.typ of
                               f32bit,
                               s32real :
                                 begin
                                    inc(pushedparasize,4);
                                    if inlined then
                                      begin
                                         exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,
                                           newreference(tempreference),R_EDI)));
                                         r:=new_reference(procinfo.framepointer,para_offset-pushedparasize);
                                         exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_L,
                                           R_EDI,r)));
                                      end
                                    else
                                      emit_push_mem(tempreference);
                                              end;
                                            s64real,
                                            s64bit : begin
                                                         inc(pushedparasize,4);
                                                         inc(tempreference.offset,4);
                     if inlined then
                       begin
                          exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,
                            newreference(tempreference),R_EDI)));
                          r:=new_reference(procinfo.framepointer,para_offset-pushedparasize);
                          exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_L,
                            R_EDI,r)));
                       end
                     else
                                                         emit_push_mem(tempreference);
                                                         inc(pushedparasize,4);
                                                         dec(tempreference.offset,4);
                     if inlined then
                       begin
                          exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,
                            newreference(tempreference),R_EDI)));
                          r:=new_reference(procinfo.framepointer,para_offset-pushedparasize);
                          exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_L,
                            R_EDI,r)));
                       end
                     else
                                                         emit_push_mem(tempreference);
                                                      end;
                                            s80real : begin
                                                         inc(pushedparasize,4);
                                                         inc(tempreference.offset,6);
                     if inlined then
                       begin
                          exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,
                            newreference(tempreference),R_EDI)));
                          r:=new_reference(procinfo.framepointer,para_offset-pushedparasize);
                          exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_L,
                            R_EDI,r)));
                       end
                     else
                                                         emit_push_mem(tempreference);
                                                         dec(tempreference.offset,4);
                                                         inc(pushedparasize,4);
                     if inlined then
                       begin
                          exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,
                            newreference(tempreference),R_EDI)));
                          r:=new_reference(procinfo.framepointer,para_offset-pushedparasize);
                          exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_L,
                            R_EDI,r)));
                       end
                     else
                                                         emit_push_mem(tempreference);
                                                         dec(tempreference.offset,2);
                                                         inc(pushedparasize,2);
                     if inlined then
                       begin
                          exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_W,
                            newreference(tempreference),R_DI)));
                          r:=new_reference(procinfo.framepointer,para_offset-pushedparasize);
                          exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_W,
                            R_DI,r)));
                       end
                     else
                                                         exprasmlist^.concat(new(pai386,op_ref(A_PUSH,S_W,
                                                           newreference(tempreference))));
                                                      end;
                                         end;
                                      end;
                           pointerdef,procvardef,
                           enumdef,classrefdef:
                             begin
                                inc(pushedparasize,4);
                     if inlined then
                       begin
                          exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,
                            newreference(tempreference),R_EDI)));
                          r:=new_reference(procinfo.framepointer,para_offset-pushedparasize);
                          exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_L,
                            R_EDI,r)));
                       end
                     else
                                emit_push_mem(tempreference);
                             end;
                           arraydef,recorddef,stringdef,setdef,objectdef :
                             begin
                                { small set ? }
                                if ((p^.resulttype^.deftype=setdef) and
                                  (psetdef(p^.resulttype)^.settype=smallset)) then
                                  begin
                                     inc(pushedparasize,4);
                                     if inlined then
                                       begin
                                          r:=new_reference(procinfo.framepointer,para_offset-pushedparasize);
                                          concatcopy(tempreference,r^,4,false);
                                       end
                                     else
                                     emit_push_mem(tempreference);
                                  end
                                { call by value open array ? }
                                else if (p^.resulttype^.deftype=arraydef) and
                                  assigned(defcoll^.data) and
                                  is_open_array(defcoll^.data) then
                                  begin
                                     { first, push high }
                                     maybe_push_open_array_high;
                                     inc(pushedparasize,4);
                     if inlined then
                       begin
                          exprasmlist^.concat(new(pai386,op_ref_reg(A_LEA,S_L,
                            newreference(p^.left^.location.reference),R_EDI)));
                          r:=new_reference(procinfo.framepointer,para_offset-pushedparasize);
                          exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_L,
                            R_EDI,r)));
                       end
                     else
                                     emitpushreferenceaddr(p^.left^.location.reference);
                                  end
                                else
                                  begin

                                     size:=p^.resulttype^.size;

                                     { Alignment }
                                     {
                                     if (size>=4) and ((size and 3)<>0) then
                                       inc(size,4-(size and 3))
                                     else if (size>=2) and ((size and 1)<>0) then
                                       inc(size,2-(size and 1))
                                     else
                                     if size=1 then size:=2;
                                     }
                                     { create stack space }
                                     if not inlined then
                                       exprasmlist^.concat(new(pai386,op_const_reg(A_SUB,S_L,size,R_ESP)));
                                     inc(pushedparasize,size);
                                     { create stack reference }
                                     stackref.symbol := nil;
                                     if not inlined then
                                       begin
                                          clear_reference(stackref);
                                          stackref.base:=R_ESP;
                                       end
                                     else
                                       begin
                                          clear_reference(stackref);
                                          stackref.base:=procinfo.framepointer;
                                          stackref.offset:=para_offset-pushedparasize;
                                       end;
                                     { produce copy }
                                     if p^.resulttype^.deftype=stringdef then
                                       begin
                                          copystring(stackref,p^.left^.location.reference,
                                            pstringdef(p^.resulttype)^.len);
                                       end
                                     else
                                       begin
                                          concatcopy(p^.left^.location.reference,
                                          stackref,p^.resulttype^.size,true);
                                       end;
                                  end;
                             end;
                           else Message(cg_e_illegal_expression);
                        end;
                     end;
                   LOC_JUMP:
                     begin
                        getlabel(hlabel);
                        inc(pushedparasize,2);
                        emitl(A_LABEL,truelabel);
                     if inlined then
                       begin
                          r:=new_reference(procinfo.framepointer,para_offset-pushedparasize);
                          exprasmlist^.concat(new(pai386,op_const_ref(A_MOV,S_W,
                            1,r)));
                       end
                     else
                        exprasmlist^.concat(new(pai386,op_const(A_PUSH,S_W,1)));
                        emitl(A_JMP,hlabel);
                        emitl(A_LABEL,falselabel);
                     if inlined then
                       begin
                          r:=new_reference(procinfo.framepointer,para_offset-pushedparasize);
                          exprasmlist^.concat(new(pai386,op_const_ref(A_MOV,S_W,
                            0,r)));
                       end
                     else
                        exprasmlist^.concat(new(pai386,op_const(A_PUSH,S_W,0)));
                        emitl(A_LABEL,hlabel);
                     end;
                   LOC_FLAGS:
                     begin
                        if not(R_EAX in unused) then
                          exprasmlist^.concat(new(pai386,op_reg_reg(A_MOV,S_L,R_EAX,R_EDI)));

                        { clear full EAX is faster }
                        { but dont you set the equal flag ? }
                        {exprasmlist^.concat(new(pai386,op_reg_reg(A_XOR,S_L,R_EAX,R_EAX)));}
                        exprasmlist^.concat(new(pai386,op_reg(flag_2_set[p^.left^.location.resflags],S_NO,
                          R_AL)));
                        exprasmlist^.concat(new(pai386,op_reg_reg(A_MOVZX,S_BW,R_AL,R_AX)));
                        {exprasmlist^.concat(new(pai386,op_reg_reg(A_XOR,S_L,R_EAX,R_EAX)));}
                        inc(pushedparasize,2);
                     if inlined then
                       begin
                          r:=new_reference(procinfo.framepointer,para_offset-pushedparasize);
                          exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_W,
                            R_AX,r)));
                       end
                     else
                        exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_W,R_AX)));
                        if not(R_EAX in unused) then
                          exprasmlist^.concat(new(pai386,op_reg_reg(A_MOV,S_L,R_EDI,R_EAX)));
                     end;
{$ifdef SUPPORT_MMX}
                   LOC_MMXREGISTER,
                   LOC_CMMXREGISTER:
                     begin
                        inc(pushedparasize,8); { was missing !!! (PM) }
                        exprasmlist^.concat(new(pai386,op_const_reg(
                          A_SUB,S_L,8,R_ESP)));
                     if inlined then
                       begin
                          r:=new_reference(procinfo.framepointer,para_offset-pushedparasize);
                          exprasmlist^.concat(new(pai386,op_reg_ref(A_MOVQ,S_NO,
                            p^.left^.location.register,r)));
                       end
                     else
                        begin
                           r:=new_reference(R_ESP,0);
                           exprasmlist^.concat(new(pai386,op_reg_ref(
                          A_MOVQ,S_NO,p^.left^.location.register,r)));
                     end;
{$endif SUPPORT_MMX}
                end;
           end;
         truelabel:=otlabel;
         falselabel:=oflabel;
         { push from right to left }
         if not push_from_left_to_right and assigned(p^.right) then
           secondcallparan(p^.right,defcoll^.next,push_from_left_to_right
             ,inlined,para_offset
           );
      end;

    procedure secondcalln(var p : ptree);

      var
         unusedregisters : tregisterset;
         pushed : tpushed;
         funcretref : treference;
         hregister : tregister;
         oldpushedparasize : longint;
         { true if ESI must be loaded again after the subroutine }
         loadesi : boolean;
         { true if a virtual method must be called directly }
         no_virtual_call : boolean;
         { true if we produce a con- or destrutor in a call }
         is_con_or_destructor : boolean;
         { true if a constructor is called again }
         extended_new : boolean;
         { adress returned from an I/O-error }
         iolabel : plabel;
         { lexlevel count }
         i : longint;
         { help reference pointer }
         r : preference;
         pp,params : ptree;
         inlined : boolean;
         inlinecode : ptree;
         para_offset : longint;
         { instruction for alignement correction }
         corr : pai386;
         { we must pop this size also after !! }
         must_pop : boolean;
         pop_size : longint;

      label
         dont_call;

      begin
         extended_new:=false;
         iolabel:=nil;
         inlinecode:=nil;
         inlined:=false;
         loadesi:=true;
         no_virtual_call:=false;
         unusedregisters:=unused;

         if not assigned(p^.procdefinition) then
          exit;
         if (p^.procdefinition^.options and poinline)<>0 then
           begin
              inlined:=true;
              inlinecode:=p^.right;
              { set it to the same lexical level }
              p^.procdefinition^.parast^.symtablelevel:=
                aktprocsym^.definition^.parast^.symtablelevel;
              if assigned(p^.left) then
                inlinecode^.para_offset:=
                  gettempofsizepersistant(inlinecode^.para_size);
              p^.procdefinition^.parast^.call_offset:=
                inlinecode^.para_offset;
{$ifdef extdebug}
             Comment(V_debug,
               'inlined parasymtable is at offset '
               +tostr(p^.procdefinition^.parast^.call_offset));
             exprasmlist^.concat(new(pai_asm_comment,init(
               strpnew('inlined parasymtable is at offset '
               +tostr(p^.procdefinition^.parast^.call_offset)))));
{$endif extdebug}
              p^.right:=nil;
              { disable further inlining of the same proc
                in the args }
              p^.procdefinition^.options:=p^.procdefinition^.options and (not poinline);
           end;
         { only if no proc var }
         if not(assigned(p^.right)) then
           is_con_or_destructor:=((p^.procdefinition^.options and poconstructor)<>0)
             or ((p^.procdefinition^.options and podestructor)<>0);
         { proc variables destroy all registers }
         if (p^.right=nil) and
            { virtual methods too }
            ((p^.procdefinition^.options and povirtualmethod)=0) then
           begin
              if ((p^.procdefinition^.options and poiocheck)<>0)
                and (cs_iocheck in aktswitches) then
                begin
                   getlabel(iolabel);
                   emitl(A_LABEL,iolabel);
                end
              else iolabel:=nil;

              { save all used registers }
              pushusedregisters(pushed,p^.procdefinition^.usedregisters);

              { give used registers through }
              usedinproc:=usedinproc or p^.procdefinition^.usedregisters;
           end
         else
           begin
              pushusedregisters(pushed,$ff);
              usedinproc:=$ff;
              { no IO check for methods and procedure variables }
              iolabel:=nil;
           end;

         { generate the code for the parameter and push them }
         oldpushedparasize:=pushedparasize;
         pushedparasize:=0;
         corr:=new(pai386,op_const_reg(A_SUB,S_L,0,R_ESP));
         exprasmlist^.concat(corr);
         if (p^.resulttype<>pdef(voiddef)) and
            ret_in_param(p^.resulttype) then
           begin
              funcretref.symbol:=nil;
{$ifdef test_dest_loc}
              if dest_loc_known and (dest_loc_tree=p) and
                 (dest_loc.loc in [LOC_REFERENCE,LOC_MEM]) then
                begin
                   funcretref:=dest_loc.reference;
                   if assigned(dest_loc.reference.symbol) then
                     funcretref.symbol:=stringdup(dest_loc.reference.symbol^);
                   in_dest_loc:=true;
                end
              else
{$endif test_dest_loc}
                if inlined then
                  begin
                     reset_reference(funcretref);
                     funcretref.offset:=gettempofsizepersistant(p^.procdefinition^.retdef^.size);
                     funcretref.base:=procinfo.framepointer;
                  end
                else
                  gettempofsizereference(p^.procdefinition^.retdef^.size,funcretref);
           end;
         if assigned(p^.left) then
           begin
              pushedparasize:=0;
              { be found elsewhere }
              if inlined then
                para_offset:=p^.procdefinition^.parast^.call_offset+
                  p^.procdefinition^.parast^.datasize
              else
                para_offset:=0;
              if assigned(p^.right) then
                secondcallparan(p^.left,pprocvardef(p^.right^.resulttype)^.para1,
                  (p^.procdefinition^.options and poleftright)<>0
                  ,inlined,
                  para_offset
                  )
              else
                secondcallparan(p^.left,p^.procdefinition^.para1,
                  (p^.procdefinition^.options and poleftright)<>0
                   ,inlined,
                  para_offset
                  );
           end;
         params:=p^.left;
         p^.left:=nil;
         if inlined then
           inlinecode^.retoffset:=gettempofsizepersistant(4);
         if ret_in_param(p^.resulttype) then
           begin
              inc(pushedparasize,4);
              if inlined then
                begin
                   exprasmlist^.concat(new(pai386,op_ref_reg(A_LEA,S_L,
                     newreference(funcretref),R_EDI)));
                   r:=new_reference(procinfo.framepointer,inlinecode^.retoffset);
                   exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_L,
                     R_EDI,r)));
                end
              else
                emitpushreferenceaddr(funcretref);
           end;
         { procedure variable ? }
         if (p^.right=nil) then
           begin
              { overloaded operator have no symtable }
              { push self }
              if assigned(p^.symtable) and
                (p^.symtable^.symtabletype=withsymtable) then
                begin
                   { dirty trick to avoid the secondcall below }
                   p^.methodpointer:=genzeronode(callparan);
                   p^.methodpointer^.location.loc:=LOC_REGISTER;
                   p^.methodpointer^.location.register:=R_ESI;
                   { make a reference }
                   new(r);
                   reset_reference(r^);
                   r^.offset:=p^.symtable^.datasize;
                   r^.base:=procinfo.framepointer;
                   exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,r,R_ESI)));
                end;

              { push self }
              if assigned(p^.symtable) and
                ((p^.symtable^.symtabletype=objectsymtable) or
                (p^.symtable^.symtabletype=withsymtable)) then
                begin
                   if assigned(p^.methodpointer) then
                     begin
                        {
                        if p^.methodpointer^.resulttype=classrefdef then
                          begin
                              two possibilities:
                               1. constructor
                               2. class method

                          end
                        else }
                          begin
                             case p^.methodpointer^.treetype of
                               typen:
                                 begin
                                    { direct call to inherited method }
                                    if (p^.procdefinition^.options and poabstractmethod)<>0 then
                                      begin
                                         Message(cg_e_cant_call_abstract_method);
                                         goto dont_call;
                                      end;
                                    { generate no virtual call }
                                    no_virtual_call:=true;

                                    if (p^.symtableprocentry^.properties and sp_static)<>0 then
                                      begin
                                         { well lets put the VMT address directly into ESI }
                                         { it is kind of dirty but that is the simplest    }
                                         { way to accept virtual static functions (PM)     }
                                         loadesi:=true;
                                         exprasmlist^.concat(new(pai386,op_csymbol_reg(A_MOV,S_L,
                                           newcsymbol(pobjectdef(p^.methodpointer^.resulttype)^.vmt_mangledname,0),R_ESI)));
                                         maybe_concat_external(pobjectdef(p^.methodpointer^.resulttype)^.owner,
                                           pobjectdef(p^.methodpointer^.resulttype)^.vmt_mangledname);
                                         exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,R_ESI)));
                                      end
                                    else
                                      { this is a member call, so ESI isn't modfied }
                                      loadesi:=false;
                                    if not(is_con_or_destructor and
                                      pobjectdef(p^.methodpointer^.resulttype)^.isclass and
                                        assigned(aktprocsym) and
                                        ((aktprocsym^.definition^.options and
                                        (poconstructor or podestructor))<>0)) then
                                      exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,R_ESI)));
                                    { if an inherited con- or destructor should be  }
                                    { called in a con- or destructor then a warning }
                                    { will be made                                  }
                                    { con- and destructors need a pointer to the vmt }
                                    if is_con_or_destructor and
                                    not(pobjectdef(p^.methodpointer^.resulttype)^.isclass) and
                                    assigned(aktprocsym) then
                                      begin
                                         if not ((aktprocsym^.definition^.options
                                           and (poconstructor or podestructor))<>0) then

                                          Message(cg_w_member_cd_call_from_method);
                                      end;
                                    if is_con_or_destructor then
                                      push_int(0)
                                 end;
                               hnewn:
                                 begin
                                    { extended syntax of new }
                                    { ESI must be zero }
                                    exprasmlist^.concat(new(pai386,op_reg_reg(A_XOR,S_L,R_ESI,R_ESI)));
                                    exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,R_ESI)));
                                    { insert the vmt }
                                    exprasmlist^.concat(new(pai386,op_csymbol(A_PUSH,S_L,
                                    newcsymbol(pobjectdef(p^.methodpointer^.resulttype)^.vmt_mangledname,0))));
                                    maybe_concat_external(pobjectdef(p^.methodpointer^.resulttype)^.owner,
                                      pobjectdef(p^.methodpointer^.resulttype)^.vmt_mangledname);
                                    extended_new:=true;
                                 end;
                               hdisposen:
                                 begin
                                    secondpass(p^.methodpointer);

                                    { destructor with extended syntax called from dispose }
                                    { hdisposen always deliver LOC_REFERENCE              }
                                    exprasmlist^.concat(new(pai386,op_ref_reg(A_LEA,S_L,
                                      newreference(p^.methodpointer^.location.reference),R_ESI)));
                                    del_reference(p^.methodpointer^.location.reference);
                                    exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,R_ESI)));
                                    exprasmlist^.concat(new(pai386,op_csymbol(A_PUSH,S_L,
                                    newcsymbol(pobjectdef(p^.methodpointer^.resulttype)^.vmt_mangledname,0))));
                                    maybe_concat_external(pobjectdef(p^.methodpointer^.resulttype)^.owner,
                                      pobjectdef(p^.methodpointer^.resulttype)^.vmt_mangledname);
                                 end;
                               else
                                 begin
                                    { call to an instance member }
                                    if (p^.symtable^.symtabletype<>withsymtable) then
                                      begin
                                         secondpass(p^.methodpointer);
                                         case p^.methodpointer^.location.loc of
                                            LOC_REGISTER:
                                              begin
                                                 ungetregister32(p^.methodpointer^.location.register);
                                                 emit_reg_reg(A_MOV,S_L,p^.methodpointer^.location.register,R_ESI);
                                              end;
                                            else
                                              begin
                                                 if (p^.methodpointer^.resulttype^.deftype=objectdef) and
                                                   pobjectdef(p^.methodpointer^.resulttype)^.isclass then
                                                   exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,
                                                     newreference(p^.methodpointer^.location.reference),R_ESI)))
                                                 else
                                                   exprasmlist^.concat(new(pai386,op_ref_reg(A_LEA,S_L,
                                                     newreference(p^.methodpointer^.location.reference),R_ESI)));
                                                 del_reference(p^.methodpointer^.location.reference);
                                              end;
                                         end;
                                      end;
                                    { when calling a class method, we have
                                      to load ESI with the VMT !
                                      But that's wrong, if we call a class method via self
                                    }
                                    if ((p^.procdefinition^.options and poclassmethod)<>0)
                                       and not(p^.methodpointer^.treetype=selfn) then
                                      begin
                                         { class method needs current VMT }
                                         new(r);
                                         reset_reference(r^);
                                         r^.base:=R_ESI;
                                         exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,r,R_ESI)));
                                      end;

                                    { direct call to class constructor, don't allocate memory }
                                    if is_con_or_destructor and (p^.methodpointer^.resulttype^.deftype=objectdef) and
                                      (pobjectdef(p^.methodpointer^.resulttype)^.isclass) then
                                      exprasmlist^.concat(new(pai386,op_const(A_PUSH,S_L,0)))
                                    else
                                      exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,R_ESI)));
                                    if is_con_or_destructor then
                                      begin
                                         { classes don't get a VMT pointer pushed }
                                         if (p^.methodpointer^.resulttype^.deftype=objectdef) and
                                           not(pobjectdef(p^.methodpointer^.resulttype)^.isclass) then
                                           begin
                                              if ((p^.procdefinition^.options and poconstructor)<>0) then
                                                begin
                                                   { it's no bad idea, to insert the VMT }
                                                   exprasmlist^.concat(new(pai386,op_csymbol(A_PUSH,S_L,
                                                     newcsymbol(pobjectdef(p^.methodpointer^.resulttype)^.vmt_mangledname,
                                                     0))));
                                                   maybe_concat_external(pobjectdef(p^.methodpointer^.resulttype)^.owner,
                                                     pobjectdef(p^.methodpointer^.resulttype)^.vmt_mangledname);
                                                end
                                              { destructors haven't to dispose the instance, if this is }
                                              { a direct call                                           }
                                              else
                                                push_int(0);
                                           end;
                                      end;
                                 end;
                             end;
                          end;
                     end
                   else
                     begin
                        if ((p^.procdefinition^.options and poclassmethod)<>0) and
                          not(
                            assigned(aktprocsym) and
                            ((aktprocsym^.definition^.options and poclassmethod)<>0)
                          ) then
                          begin
                             { class method needs current VMT }
                             new(r);
                             reset_reference(r^);
                             r^.base:=R_ESI;
                             exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,r,R_ESI)));
                          end
                        else
                          begin
                             { member call, ESI isn't modified }
                             loadesi:=false;
                          end;
                        exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,R_ESI)));
                        { but a con- or destructor here would probably almost }
                        { always be placed wrong }
                        if is_con_or_destructor then
                          begin
                             Message(cg_w_member_cd_call_from_method);
                             push_int(0);
                          end;
                     end;
                end;

              { push base pointer ?}
              if (lexlevel>1) and assigned(pprocdef(p^.procdefinition)^.parast) and
                ((p^.procdefinition^.parast^.symtablelevel)>2) then
                begin
                   { if we call a nested function in a method, we must      }
                   { push also SELF!                                        }
                   { THAT'S NOT TRUE, we have to load ESI via frame pointer }
                   { access                                                 }
                   {
                     begin
                        loadesi:=false;
                        exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,R_ESI)));
                     end;
                   }
                   if lexlevel=(p^.procdefinition^.parast^.symtablelevel) then
                     begin
                        new(r);
                        reset_reference(r^);
                        r^.offset:=procinfo.framepointer_offset;
                        r^.base:=procinfo.framepointer;
                        exprasmlist^.concat(new(pai386,op_ref(A_PUSH,S_L,r)))
                     end
                     { this is only true if the difference is one !!
                       but it cannot be more !! }
                   else if (lexlevel=p^.procdefinition^.parast^.symtablelevel-1) then
                     begin
                        exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,procinfo.framepointer)))
                     end
                   else if (lexlevel>p^.procdefinition^.parast^.symtablelevel) then
                     begin
                        hregister:=getregister32;
                        new(r);
                        reset_reference(r^);
                        r^.offset:=procinfo.framepointer_offset;
                        r^.base:=procinfo.framepointer;
                        exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,r,hregister)));
                        for i:=(p^.procdefinition^.parast^.symtablelevel) to lexlevel-1 do
                          begin
                             new(r);
                             reset_reference(r^);
                             {we should get the correct frame_pointer_offset at each level
                             how can we do this !!! }
                             r^.offset:=procinfo.framepointer_offset;
                             r^.base:=hregister;
                             exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,r,hregister)));
                          end;
                        exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,hregister)));
                        ungetregister32(hregister);
                     end
                   else
                     internalerror(25000);
                end;

              { exported methods should be never called direct }
              if (p^.procdefinition^.options and poexports)<>0 then
                Message(cg_e_dont_call_exported_direct);

              if (not inlined) and ((pushedparasize mod 4)<>0) then
                begin
                   corr^.op1:=pointer(4-(pushedparasize mod 4));
                   must_pop:=true;
                   pop_size:=4-(pushedparasize mod 4);
                end
              else
                begin
                   exprasmlist^.remove(corr);
                   must_pop:=false;
                   pop_size:=0;
                end;

              if ((p^.procdefinition^.options and povirtualmethod)<>0) and
                 not(no_virtual_call) then
                begin
                   { static functions contain the vmt_address in ESI }
                   { also class methods                              }
                   if assigned(aktprocsym) then
                     begin
                       if ((aktprocsym^.properties and sp_static)<>0) or
                        ((aktprocsym^.definition^.options and poclassmethod)<>0) or
                        ((p^.procdefinition^.options and postaticmethod)<>0) or
                        ((p^.procdefinition^.options and poconstructor)<>0) or
                        { ESI is loaded earlier }
                        ((p^.procdefinition^.options and poclassmethod)<>0)then
                         begin
                            new(r);
                            reset_reference(r^);
                            r^.base:=R_ESI;
                         end
                       else
                         begin
                            new(r);
                            reset_reference(r^);
                            r^.base:=R_ESI;
                            exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,r,R_EDI)));
                            new(r);
                            reset_reference(r^);
                            r^.base:=R_EDI;
                         end;
                     end
                   else
                     { aktprocsym should be assigned, also in main program }
                     internalerror(12345);
                   {
                     begin
                       new(r);
                       reset_reference(r^);
                       r^.base:=R_ESI;
                       exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,r,R_EDI)));
                       new(r);
                       reset_reference(r^);
                       r^.base:=R_EDI;
                     end;
                   }
                   if p^.procdefinition^.extnumber=-1 then
                        internalerror($Da);
                   r^.offset:=p^.procdefinition^.extnumber*4+12;
                   if (cs_rangechecking in aktswitches) then
                     begin
                        exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,r^.base)));
                        emitcall('CHECK_OBJECT',true);
                     end;
                   exprasmlist^.concat(new(pai386,op_ref(A_CALL,S_NO,r)));
                end
              else if not inlined then
                emitcall(p^.procdefinition^.mangledname,
                  (p^.symtableproc^.symtabletype=unitsymtable) or
                  ((p^.symtableproc^.symtabletype=objectsymtable) and
                  (pobjectdef(p^.symtableproc^.defowner)^.owner^.symtabletype=unitsymtable)))
              else { inlined proc }
                { inlined code is in inlinecode }
                begin
                   secondpass(inlinecode);
                   { set poinline again }
                   p^.procdefinition^.options:=p^.procdefinition^.options or poinline;
                   { free the args }
                   ungetpersistanttemp(p^.procdefinition^.parast^.call_offset,
                     p^.procdefinition^.parast^.datasize);
                end;
              if (not inlined) and ((p^.procdefinition^.options and poclearstack)<>0) then
                begin
                   { consider the alignment with the rest (PM) }
                   pushedparasize:=pushedparasize+pop_size;
                   must_pop:=false;
                   if pushedparasize=4 then
                     { better than an add on all processors }
                     exprasmlist^.concat(new(pai386,op_reg(A_POP,S_L,R_EDI)))
                   { the pentium has two pipes and pop reg is pairable }
                   { but the registers must be different!              }
                   else if (pushedparasize=8) and
                     not(cs_littlesize in aktswitches) and
                     (opt_processors=pentium) and
                     (procinfo._class=nil) then
                       begin
                          exprasmlist^.concat(new(pai386,op_reg(A_POP,S_L,R_EDI)));
                          exprasmlist^.concat(new(pai386,op_reg(A_POP,S_L,R_ESI)));
                       end
                   else exprasmlist^.concat(new(pai386,op_const_reg(A_ADD,S_L,pushedparasize,R_ESP)));
                end;
           end
         else
           begin
              if (pushedparasize mod 4)<>0 then
                begin
                   corr^.op1:=pointer(4-(pushedparasize mod 4));
                   must_pop:=true;
                   pop_size:=4-(pushedparasize mod 4);
                end
              else
                begin
                   exprasmlist^.remove(corr);
                   must_pop:=false;
                   pop_size:=0;
                end;
              secondpass(p^.right);
              { method pointer ? }
              if (p^.procdefinition^.options and pomethodpointer)<>0 then
                begin
                   { method pointer can't be in a register }
                   inc(p^.right^.location.reference.offset,4);
                   { push self pointer }
                   exprasmlist^.concat(new(pai386,op_ref(A_PUSH,S_L,newreference(p^.right^.location.reference))));
                   del_reference(p^.right^.location.reference);
                   dec(p^.right^.location.reference.offset,4);
                end;
              case p^.right^.location.loc of
                 LOC_REGISTER,LOC_CREGISTER:
                    begin
                        exprasmlist^.concat(new(pai386,op_reg(A_CALL,S_NO,p^.right^.location.register)));
                        ungetregister32(p^.right^.location.register);
                    end
                 else
                    exprasmlist^.concat(new(pai386,op_ref(A_CALL,S_NO,newreference(p^.right^.location.reference))));
                    del_reference(p^.right^.location.reference);
              end;


             end;
      dont_call:
         pushedparasize:=oldpushedparasize;
         unused:=unusedregisters;

         { handle function results }
         { structured results are easy to handle.... }
         { needed also when result_no_used !! }
         if (p^.resulttype<>pdef(voiddef)) and ret_in_param(p^.resulttype) then
           begin
              p^.location.loc:=LOC_MEM;
              stringdispose(p^.location.reference.symbol);
              p^.location.reference:=funcretref;
           end;
         if (p^.resulttype<>pdef(voiddef)) and p^.return_value_used then
           begin
                 { a contructor could be a function with boolean result }
              if (p^.right=nil) and
                 ((p^.procdefinition^.options and poconstructor)<>0) and
                 { quick'n'dirty check if it is a class or an object }
                 (p^.resulttype^.deftype=orddef) then
                begin
                   p^.location.loc:=LOC_FLAGS;
                   p^.location.resflags:=F_NE;
                   if extended_new then
                     begin
{$ifdef test_dest_loc}
                        if dest_loc_known and (dest_loc_tree=p) then
                          mov_reg_to_dest(p,S_L,R_EAX)
                        else
{$endif test_dest_loc}
                          begin
                             hregister:=getregister32;
                             emit_reg_reg(A_MOV,S_L,R_EAX,hregister);
                             p^.location.register:=hregister;
                          end;
                     end;
                end
               { structed results are easy to handle.... }
              else if ret_in_param(p^.resulttype) then
                begin
                   {p^.location.loc:=LOC_MEM;
                   stringdispose(p^.location.reference.symbol);
                   p^.location.reference:=funcretref;
                   already done above (PM) }
                end
              else
                begin
                   if (p^.resulttype^.deftype=orddef) then
                     begin
                        p^.location.loc:=LOC_REGISTER;
                        case porddef(p^.resulttype)^.typ of
                          s32bit,u32bit :
                            begin
{$ifdef test_dest_loc}
                               if dest_loc_known and (dest_loc_tree=p) then
                                 mov_reg_to_dest(p,S_L,R_EAX)
                               else
{$endif test_dest_loc}
                                 begin
                                    hregister:=getregister32;
                                    emit_reg_reg(A_MOV,S_L,R_EAX,hregister);
                                    p^.location.register:=hregister;
                                 end;
                            end;
                          uchar,u8bit,bool8bit,s8bit :
                                  begin
{$ifdef test_dest_loc}
                                     if dest_loc_known and (dest_loc_tree=p) then
                                       mov_reg_to_dest(p,S_B,R_AL)
                                     else
{$endif test_dest_loc}
                                       begin
                                          hregister:=getregister32;
                                          emit_reg_reg(A_MOV,S_B,R_AL,reg32toreg8(hregister));
                                          p^.location.register:=reg32toreg8(hregister);
                                       end;
                                  end;
                                s16bit,u16bit :
                                  begin
{$ifdef test_dest_loc}
                                     if dest_loc_known and (dest_loc_tree=p) then
                                       mov_reg_to_dest(p,S_W,R_AX)
                                     else
{$endif test_dest_loc}
                                       begin
                                          hregister:=getregister32;
                                          emit_reg_reg(A_MOV,S_W,R_AX,reg32toreg16(hregister));
                                          p^.location.register:=reg32toreg16(hregister);
                                       end;
                                  end;
                             else internalerror(7);
                              end

                          end
                       else if (p^.resulttype^.deftype=floatdef) then
                           case pfloatdef(p^.resulttype)^.typ of
                                 f32bit : begin
                                             p^.location.loc:=LOC_REGISTER;
{$ifdef test_dest_loc}
                                             if dest_loc_known and (dest_loc_tree=p) then
                                               mov_reg_to_dest(p,S_L,R_EAX)
                                             else
{$endif test_dest_loc}
                                               begin
                                                  hregister:=getregister32;
                                                  emit_reg_reg(A_MOV,S_L,R_EAX,hregister);
                                                  p^.location.register:=hregister;
                                               end;
                                          end;
                                 else
                                     p^.location.loc:=LOC_FPU;
                           end
                       else
                          begin
                              p^.location.loc:=LOC_REGISTER;
{$ifdef test_dest_loc}
                              if dest_loc_known and (dest_loc_tree=p) then
                                mov_reg_to_dest(p,S_L,R_EAX)
                              else
{$endif test_dest_loc}
                                begin
                                    hregister:=getregister32;
                                    emit_reg_reg(A_MOV,S_L,R_EAX,hregister);
                                    p^.location.register:=hregister;
                                end;
                          end;
                end;
           end;

         { perhaps i/o check ? }
         if iolabel<>nil then
           begin
              exprasmlist^.concat(new(pai386,op_csymbol(A_PUSH,S_L,newcsymbol(lab2str(iolabel),0))));
              { this was wrong, probably an error due to diff3
                emitcall(p^.procdefinition^.mangledname);}
              emitcall('IOCHECK',true);
           end;
         { this should be optimized (PM) }
         if must_pop then
           exprasmlist^.concat(new(pai386,op_const_reg(A_ADD,S_L,pop_size,R_ESP)));
         { restore registers }
         popusedregisters(pushed);

         { at last, restore instance pointer (SELF) }
         if loadesi then
           maybe_loadesi;
         pp:=params;
         while assigned(pp) do
           begin
              if assigned(pp^.left) then
                if (pp^.left^.location.loc=LOC_REFERENCE) or
                  (pp^.left^.location.loc=LOC_MEM) then
                  ungetiftemp(pp^.left^.location.reference);
              pp:=pp^.right;
           end;
         if inlined then
           ungetpersistanttemp(inlinecode^.retoffset,4);
         disposetree(params);
         
         { from now on the result can be freed normally }
         if inlined and ret_in_param(p^.resulttype) then
           persistanttemptonormal(funcretref.offset);

         { if return value is not used }
         if (not p^.return_value_used) and (p^.resulttype<>pdef(voiddef)) then
           begin
              if p^.location.loc in [LOC_MEM,LOC_REFERENCE] then
                { release unused temp }
                ungetiftemp(p^.location.reference)
              else if p^.location.loc=LOC_FPU then
                { release FPU stack }
                exprasmlist^.concat(new(pai386,op_none(A_FDECSTP,S_NO)));
           end;
      end;

    { reverts the parameter list }
    var nb_para : integer;

    function reversparameter(p : ptree) : ptree;

       var
         hp1,hp2 : ptree;

      begin
         hp1:=nil;
         nb_para := 0;
         while assigned(p) do
           begin
              { pull out }
              hp2:=p;
              p:=p^.right;
              inc(nb_para);
              { pull in }
              hp2^.right:=hp1;
              hp1:=hp2;
           end;
         reversparameter:=hp1;
       end;

    procedure secondinline(var p : ptree);
     const     in2size:array[in_inc_byte..in_dec_dword] of Topsize=
                         (S_B,S_W,S_L,S_B,S_W,S_L);
               in2instr:array[in_inc_byte..in_dec_dword] of Tasmop=
                         (A_INC,A_INC,A_INC,A_DEC,A_DEC,A_DEC);
               ad2instr:array[in_inc_byte..in_dec_dword] of Tasmop=
                         (A_ADD,A_ADD,A_ADD,A_SUB,A_SUB,A_SUB);
            { tfloattype = (f32bit,s32real,s64real,s80real,s64bit); }
            float_name: array[tfloattype] of string[8]=
                ('FIXED','SINGLE','REAL','EXTENDED','COMP','FIXED16');
       var
         aktfile : treference;
         ft : tfiletype;
         opsize : topsize;
         asmop : tasmop;
         pushed : tpushed;

      { produces code for READ(LN) and WRITE(LN) }

      procedure handlereadwrite(doread,callwriteln : boolean);

        procedure loadstream;

          const     io:array[0..1] of string[7]=('_OUTPUT','_INPUT');
          var     r : preference;

            begin
                 new(r);
                 reset_reference(r^);
                 r^.symbol:=stringdup('U_'+upper(target_info.system_unit)+io[byte(doread)]);
                 if assem_need_external_list and
                   not (cs_compilesystem in aktswitches) then
                 concat_external(r^.symbol^,EXT_NEAR);

                 exprasmlist^.concat(new(pai386,op_ref_reg(A_LEA,S_L,r,R_EDI)))
            end;

        var
             node,hp : ptree;
            typedtyp,pararesult : pdef;
           doflush,has_length : boolean;
           dummycoll : tdefcoll;
           iolabel : plabel;
           npara : longint;

        begin
           { I/O check }
           if cs_iocheck in aktswitches then
                begin
                getlabel(iolabel);
                emitl(A_LABEL,iolabel);
             end
           else iolabel:=nil;
           { no automatic call from flush }
           doflush:=false;
           { for write of real with the length specified }
           has_length:=false;
           hp:=nil;
           { reserve temporary pointer to data variable }
             aktfile.symbol:=nil;
           gettempofsizereference(4,aktfile);
           { first state text data }
           ft:=ft_text;
           { and state a parameter ? }
           if p^.left=nil then
             begin
                { state screen address}
                doflush:=true;
                { the following instructions are for "writeln;" }
                loadstream;
                { save @Dateivarible in temporary variable }
                exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_L,R_EDI,newreference(aktfile))));
             end
           else
             begin
                { revers paramters }
                node:=reversparameter(p^.left);

                p^.left := node;
                npara := nb_para;
                { calculate data variable }
                { is first parameter a file type ? }
                if node^.left^.resulttype^.deftype=filedef then
                  begin
                     ft:=pfiledef(node^.left^.resulttype)^.filetype;
                     if ft=ft_typed then
                       typedtyp:=pfiledef(node^.left^.resulttype)^.typed_as;
                     secondpass(node^.left);
                     if codegenerror then
                       exit;

                     { save reference in temporary variables }                     { reference in tempor„re Variable retten }
                     if node^.left^.location.loc<>LOC_REFERENCE then
                       begin
                          Message(cg_e_illegal_expression);
                          exit;
                       end;

                     exprasmlist^.concat(new(pai386,op_ref_reg(A_LEA,S_L,newreference(node^.left^.location.reference),R_EDI)));

                     { skip to the next parameter }
                     node:=node^.right;
                  end
                else
                  begin
                     { if we write to stdout/in then flush after the write(ln) }
                     doflush:=true;
                     loadstream;
                  end;

                    { save @Dateivarible in temporary variable }
                exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_L,R_EDI,newreference(aktfile))));
                if doread then
                  { parameter by READ gives call by reference }
                  dummycoll.paratyp:=vs_var
                { an WRITE Call by "Const" }
                else dummycoll.paratyp:=vs_const;

                { because of secondcallparan, which otherwise attaches }
                if ft=ft_typed then
                  begin
                     { this is to avoid copy of simple const parameters }
                     dummycoll.data:=new(pformaldef,init);
                  end
                else
                  { I think, this isn't a good solution (FK) }
                  dummycoll.data:=nil;

                while assigned(node) do
                  begin
                     pushusedregisters(pushed,$ff);
                     hp:=node;
                     node:=node^.right;
                     hp^.right:=nil;
                     if hp^.is_colon_para then
                       Message(parser_e_illegal_colon_qualifier);
                     if ft=ft_typed then
                       never_copy_const_param:=true;
                     secondcallparan(hp,@dummycoll,false
                       ,false,0
                     );
                     if ft=ft_typed then
                       never_copy_const_param:=false;
                     hp^.right:=node;
                          if codegenerror then
                       exit;

                     emit_push_mem(aktfile);
                     if (ft=ft_typed) then
                       begin
                          { OK let's try this }
                          { first we must only allow the right type }
                            { we have to call blockread or blockwrite }
                                   { but the real problem is that            }
                            { reset and rewrite should have set       }
                            { the type size                           }
                                   { as recordsize for that file !!!!        }
                            { how can we make that                    }
                            { I think that is only possible by adding }
                            { reset and rewrite to the inline list a call        }
                                   { allways read only one record by element }
                            push_int(typedtyp^.size);
                            if doread then
                              emitcall('TYPED_READ',true)
                            else
                              emitcall('TYPED_WRITE',true)
                          {!!!!!!!}
                       end
                     else
                       begin
                          { save current position }
                          pararesult:=hp^.left^.resulttype;
                          { handle possible field width  }
                          { of course only for write(ln) }
                          if not doread then
                               begin
                               { handle total width parameter }
                               if assigned(node) and node^.is_colon_para then
                                 begin
                                    hp:=node;
                                    node:=node^.right;
                                    hp^.right:=nil;
                                    secondcallparan(hp,@dummycoll,false
                                      ,false,0
                                    );
                                    hp^.right:=node;
                                    if codegenerror then
                                      exit;
                                    has_length:=true;
                                 end
                               else
                                 if pararesult^.deftype<>floatdef then
                                   push_int(0)
                                 else
                                  push_int(-32767);
                              { a second colon para for a float ? }
                              if assigned(node) and node^.is_colon_para then
                                begin
                                    hp:=node;
                                    node:=node^.right;
                                    hp^.right:=nil;
                                    secondcallparan(hp,@dummycoll,false
                                      ,false,0
                                    );
                                    hp^.right:=node;
                                    if pararesult^.deftype<>floatdef then
                                      Message(parser_e_illegal_colon_qualifier);
                                    if codegenerror then
                                      exit;
                                end
                              else
                                begin
                                if pararesult^.deftype=floatdef then
                                    push_int(-1);
                                end
                              end;
                          case pararesult^.deftype of
                             stringdef:
                               begin
                                  if doread then
                                    emitcall('READ_TEXT_STRING',true)
                                  else
                                    begin
                                      emitcall('WRITE_TEXT_STRING',true);
                                      {ungetiftemp(hp^.left^.location.reference);}
                                    end;
                               end;
                                    pointerdef : begin
                                                        if is_equal(ppointerdef(pararesult)^.definition,cchardef) then
                                                          begin
                                                              if doread then
                                                                 emitcall('READ_TEXT_PCHAR_AS_POINTER',true)
                                                              else
                                                                 emitcall('WRITE_TEXT_PCHAR_AS_POINTER',true);
                                                          end
                                                        else
                                                         Message(parser_e_illegal_parameter_list);
                                                    end;
                                    arraydef : begin
                                                     if (parraydef(pararesult)^.lowrange=0)
                                                        and is_equal(parraydef(pararesult)^.definition,cchardef) then
                                                        begin
                                                            if doread then
                                                                 emitcall('READ_TEXT_PCHAR_AS_ARRAY',true)
                                                            else
                                                                 emitcall('WRITE_TEXT_PCHAR_AS_ARRAY',true);
                                                        end
                                                     else
                                                      Message(parser_e_illegal_parameter_list);
                                                  end;

                             floatdef:
                               begin
                                  if doread then
                                    emitcall('READ_TEXT_'+float_name[pfloatdef(pararesult)^.typ],true)
                                  else
                                    emitcall('WRITE_TEXT_'+float_name[pfloatdef(pararesult)^.typ],true);
                               end;
                                    orddef : begin
                                                     case porddef(pararesult)^.typ of
                                                         u8bit : if doread then
                                                                       emitcall('READ_TEXT_BYTE',true);
                                                         s8bit : if doread then
                                                                       emitcall('READ_TEXT_SHORTINT',true);
                                                         u16bit : if doread then
                                                                       emitcall('READ_TEXT_WORD',true);
                                                         s16bit : if doread then
                                                                       emitcall('READ_TEXT_INTEGER',true);
                                                         s32bit : if doread then
                                                                       emitcall('READ_TEXT_LONGINT',true)
                                                                    else
                                                                       emitcall('WRITE_TEXT_LONGINT',true);
                                                         u32bit : if doread then
                                                                       emitcall('READ_TEXT_CARDINAL',true)
                                                                    else
                                                                       emitcall('WRITE_TEXT_CARDINAL',true);
                                                         uchar : if doread then
                                                                       emitcall('READ_TEXT_CHAR',true)
                                                                    else
                                                                       emitcall('WRITE_TEXT_CHAR',true);
                                                         bool8bit : if  doread then
                                                                       { emitcall('READ_TEXT_BOOLEAN',true) }
                                                                       Message(parser_e_illegal_parameter_list)
                                                                    else
                                                                       emitcall('WRITE_TEXT_BOOLEAN',true);
                                                         else Message(parser_e_illegal_parameter_list);
                                                         end;
                                                     end;
                                    else Message(parser_e_illegal_parameter_list);
                                end;
                            end;
                          { load ESI in methods again }
                          popusedregisters(pushed);
                          maybe_loadesi;
                  end;
             end;
           if callwriteln then
             begin
                pushusedregisters(pushed,$ff);
                emit_push_mem(aktfile);
                { pushexceptlabel; }
                if ft<>ft_text then
                  Message(parser_e_illegal_parameter_list)                                    ;
                emitcall('WRITELN_TEXT',true);
                popusedregisters(pushed);
                maybe_loadesi;
             end;
           if doflush and not(doread) then
             begin
                pushusedregisters(pushed,$ff);
                { pushexceptlabel; }
                emitcall('FLUSH_STDOUT',true);
                popusedregisters(pushed);
                maybe_loadesi;
             end;
           if iolabel<>nil then
             begin
                { registers are saved in the procedure }
                exprasmlist^.concat(new(pai386,op_csymbol(A_PUSH,S_L,newcsymbol(lab2str(iolabel),0))));
                emitcall('IOCHECK',true);
             end;
           ungetiftemp(aktfile);
           if assigned(p^.left) then
             begin
                p^.left:=reversparameter(p^.left);
                    if npara<>nb_para then
                     Message(cg_f_internal_error_in_secondinline);
                    hp:=p^.left;
                    while assigned(hp) do
                  begin
                     if assigned(hp^.left) then
                       if (hp^.left^.location.loc=LOC_REFERENCE) or
                         (hp^.left^.location.loc=LOC_MEM) then
                         ungetiftemp(hp^.left^.location.reference);
                     hp:=hp^.right;
                  end;
            end;
        end;

      procedure handle_str;

        var
           hp,node : ptree;
           dummycoll : tdefcoll;
           is_real,has_length : boolean;

          begin
           pushusedregisters(pushed,$ff);
           node:=p^.left;
           is_real:=false;
           has_length:=false;
           while assigned(node^.right) do node:=node^.right;
           { if a real parameter somewhere then call REALSTR }
           if (node^.left^.resulttype^.deftype=floatdef) then
             is_real:=true;

           node:=p^.left;
           { we have at least two args }
           { with at max 2 colon_para in between }

           { first arg longint or float }
           hp:=node;
           node:=node^.right;
           hp^.right:=nil;
           dummycoll.data:=hp^.resulttype;
           { string arg }

           dummycoll.paratyp:=vs_var;
           secondcallparan(hp,@dummycoll,false
             ,false,0
             );
           if codegenerror then
             exit;

           dummycoll.paratyp:=vs_const;
           { second arg }
           hp:=node;
           node:=node^.right;
           hp^.right:=nil;
           { frac  para }
           if hp^.is_colon_para and assigned(node) and
              node^.is_colon_para then
             begin
                dummycoll.data:=hp^.resulttype;
                secondcallparan(hp,@dummycoll,false
                  ,false,0
                  );
                if codegenerror then
                  exit;
                hp:=node;
                node:=node^.right;
                hp^.right:=nil;
                has_length:=true;
             end
           else
             if is_real then
             push_int(-1);

           { third arg, length only if is_real }
           if hp^.is_colon_para then
             begin
                dummycoll.data:=hp^.resulttype;
                secondcallparan(hp,@dummycoll,false
                  ,false,0
                  );
                if codegenerror then
                  exit;
                hp:=node;
                node:=node^.right;
                hp^.right:=nil;
             end
           else
             if is_real then
               push_int(-32767)
             else
               push_int(-1);

           { last arg longint or real }
           secondcallparan(hp,@dummycoll,false
             ,false,0
             );
           if codegenerror then
             exit;

           if is_real then
             emitcall('STR_'+float_name[pfloatdef(hp^.resulttype)^.typ],true)
           else if porddef(hp^.resulttype)^.typ=u32bit then
             emitcall('STR_CARDINAL',true)
           else
             emitcall('STR_LONGINT',true);
           popusedregisters(pushed);
        end;

      var
         r : preference;
         l : longint;
         ispushed : boolean;
         hregister : tregister;

      begin
         case p^.inlinenumber of
            in_lo_word,
            in_hi_word :
              begin
                 secondpass(p^.left);
                 p^.location.loc:=LOC_REGISTER;
                 if p^.left^.location.loc<>LOC_REGISTER then
                   begin
                     if p^.left^.location.loc=LOC_CREGISTER then
                       begin
                          p^.location.register:=reg32toreg16(getregister32);
                          emit_reg_reg(A_MOV,S_W,p^.left^.location.register,
                            p^.location.register);
                       end
                     else
                       begin
                          del_reference(p^.left^.location.reference);
                          p^.location.register:=reg32toreg16(getregister32);
                          exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_W,newreference(p^.left^.location.reference),
                            p^.location.register)));
                       end;
                   end
                 else p^.location.register:=p^.left^.location.register;
                 if p^.inlinenumber=in_hi_word then
                   exprasmlist^.concat(new(pai386,op_const_reg(A_SHR,S_W,8,p^.location.register)));
                 p^.location.register:=reg16toreg8(p^.location.register);
              end;
            in_high_x :
              begin
                 if is_open_array(p^.left^.resulttype) then
                   begin
                      secondpass(p^.left);
                      del_reference(p^.left^.location.reference);
                      p^.location.register:=getregister32;
                      new(r);
                      reset_reference(r^);
                      r^.base:=highframepointer;
                      r^.offset:=highoffset+4;
                      exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,
                        r,p^.location.register)));
                   end
              end;
            in_sizeof_x,
            in_typeof_x :
              begin
                 { for both cases load vmt }
                 if p^.left^.treetype=typen then
                   begin
                      p^.location.register:=getregister32;
                      exprasmlist^.concat(new(pai386,op_csymbol_reg(A_MOV,
                        S_L,newcsymbol(pobjectdef(p^.left^.resulttype)^.vmt_mangledname,0),
                        p^.location.register)));
                   end
                 else
                   begin
                      secondpass(p^.left);
                      del_reference(p^.left^.location.reference);
                      p^.location.loc:=LOC_REGISTER;
                      p^.location.register:=getregister32;
                      { load VMT pointer }
                      exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,
                      newreference(p^.left^.location.reference),
                        p^.location.register)));
                   end;
                 { in sizeof load size }
                 if p^.inlinenumber=in_sizeof_x then
                   begin
                      new(r);
                      reset_reference(r^);
                      r^.base:=p^.location.register;
                      exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,r,
                        p^.location.register)));
                   end;
              end;
            in_lo_long,
            in_hi_long :
              begin
                 secondpass(p^.left);
                 p^.location.loc:=LOC_REGISTER;
                 if p^.left^.location.loc<>LOC_REGISTER then
                   begin
                      if p^.left^.location.loc=LOC_CREGISTER then
                        begin
                           p^.location.register:=getregister32;
                           emit_reg_reg(A_MOV,S_L,p^.left^.location.register,
                             p^.location.register);
                        end
                      else
                        begin
                           del_reference(p^.left^.location.reference);
                           p^.location.register:=getregister32;
                           exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,newreference(p^.left^.location.reference),
                             p^.location.register)));
                        end;
                   end
                 else p^.location.register:=p^.left^.location.register;
                 if p^.inlinenumber=in_hi_long then
                   exprasmlist^.concat(new(pai386,op_const_reg(A_SHR,S_L,16,p^.location.register)));
                 p^.location.register:=reg32toreg16(p^.location.register);
              end;
{***CHARBUG}
{We can now comment them out, as they are handled as typecast.
 Saves an incredible amount of 8 bytes code.
 I'am not lucky about this, because it's _not_ a type cast (FK) }
{              in_ord_char,
               in_chr_byte,}
{***}
            in_length_string :
              begin
                 secondpass(p^.left);
                 set_location(p^.location,p^.left^.location);
                 { length in ansi strings is at offset -8 }
{$ifdef UseAnsiString}
                 if is_ansistring(p^.left^.resulttype) then
                   dec(p^.location.reference.offset,8);
{$endif UseAnsiString}
              end;
            in_pred_x,
            in_succ_x:
              begin
                 secondpass(p^.left);
                 if p^.inlinenumber=in_pred_x then
                   asmop:=A_DEC
                 else
                   asmop:=A_INC;
                 case p^.resulttype^.size of
                   4 : opsize:=S_L;
                   2 : opsize:=S_W;
                   1 : opsize:=S_B;
                 else
                   internalerror(10080);
                 end;
                 p^.location.loc:=LOC_REGISTER;
                 if p^.left^.location.loc<>LOC_REGISTER then
                   begin
                      p^.location.register:=getregister32;
                      if (p^.resulttype^.size=2) then
                        p^.location.register:=reg32toreg16(p^.location.register);
                      if (p^.resulttype^.size=1) then
                        p^.location.register:=reg32toreg8(p^.location.register);
                      if p^.left^.location.loc=LOC_CREGISTER then
                        emit_reg_reg(A_MOV,opsize,p^.left^.location.register,
                          p^.location.register)
                      else
                      if p^.left^.location.loc=LOC_FLAGS then
                        exprasmlist^.concat(new(pai386,op_reg(flag_2_set[p^.left^.location.resflags],S_NO,
                                  p^.location.register)))
                      else
                        begin
                           del_reference(p^.left^.location.reference);
                           exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,opsize,newreference(p^.left^.location.reference),
                             p^.location.register)));
                        end;
                   end
                 else p^.location.register:=p^.left^.location.register;
                 exprasmlist^.concat(new(pai386,op_reg(asmop,opsize,
                   p^.location.register)))
                 { here we should insert bounds check ? }
                 { and direct call to bounds will crash the program }
                 { if we are at the limit }
                 { we could also simply say that pred(first)=first and succ(last)=last }
                 { could this be usefull I don't think so (PM)
                 emitoverflowcheck;}
              end;
            in_inc_byte..in_dec_dword:
              begin
                 secondpass(p^.left);
                 if cs_check_overflow in aktswitches then
                   begin
                   { SINCE THE CARRY FLAG IS NEVER SET BY DEC/INC, we must use  }
                   { ADD and SUB to check for overflow for unsigned operations. }
                     exprasmlist^.concat(new(pai386,op_const_ref(ad2instr[p^.inlinenumber],
                       in2size[p^.inlinenumber],1,newreference(p^.left^.location.reference))));
                     emitoverflowcheck(p^.left);
                   end
                 else
                 exprasmlist^.concat(new(pai386,op_ref(in2instr[p^.inlinenumber],
                   in2size[p^.inlinenumber],newreference(p^.left^.location.reference))));
              end;
            in_assigned_x :
              begin
                 secondpass(p^.left^.left);
                 p^.location.loc:=LOC_FLAGS;
                 if (p^.left^.left^.location.loc=LOC_REGISTER) or
                    (p^.left^.left^.location.loc=LOC_CREGISTER) then
                   begin
                      exprasmlist^.concat(new(pai386,op_reg_reg(A_OR,S_L,
                        p^.left^.left^.location.register,
                        p^.left^.left^.location.register)));
                      ungetregister32(p^.left^.left^.location.register);
                   end
                 else
                   begin
                      exprasmlist^.concat(new(pai386,op_const_ref(A_CMP,S_L,0,
                        newreference(p^.left^.left^.location.reference))));
                      del_reference(p^.left^.left^.location.reference);
                   end;
                 p^.location.resflags:=F_NE;
              end;
             in_reset_typedfile,in_rewrite_typedfile :
               begin
                  pushusedregisters(pushed,$ff);
                  exprasmlist^.concat(new(pai386,op_const(A_PUSH,S_L,pfiledef(p^.left^.resulttype)^.typed_as^.size)));
                  secondload(p^.left);
                  emitpushreferenceaddr(p^.left^.location.reference);
                  if p^.inlinenumber=in_reset_typedfile then
                    emitcall('RESET_TYPED',true)
                  else
                    emitcall('REWRITE_TYPED',true);
                  popusedregisters(pushed);
               end;
            in_write_x :
              handlereadwrite(false,false);
            in_writeln_x :
              handlereadwrite(false,true);
            in_read_x :
              handlereadwrite(true,false);
            in_readln_x :
              begin
                handlereadwrite(true,false);
                pushusedregisters(pushed,$ff);
                emit_push_mem(aktfile);
                { pushexceptlabel; }
                if ft<>ft_text then
                  Message(parser_e_illegal_parameter_list);
                emitcall('READLN_TEXT',true);
                popusedregisters(pushed);
                maybe_loadesi;
              end;
            in_str_x_string :
              begin
                 handle_str;
                 maybe_loadesi;
              end;
            in_include_x_y,
            in_exclude_x_y:
              begin
                 secondpass(p^.left^.left);
                 if p^.left^.right^.left^.treetype=ordconstn then
                   begin
                      { calculate bit position }
                      l:=1 shl (p^.left^.right^.left^.value mod 32);

                      { determine operator }
                      if p^.inlinenumber=in_include_x_y then
                        asmop:=A_OR
                      else
                        begin
                           asmop:=A_AND;
                           l:=not(l);
                        end;
                      if (p^.left^.left^.location.loc=LOC_REFERENCE) then
                        begin
                           inc(p^.left^.left^.location.reference.offset,(p^.left^.right^.left^.value div 32)*4);
                           exprasmlist^.concat(new(pai386,op_const_ref(asmop,S_L,
                             l,newreference(p^.left^.left^.location.reference))));
                           del_reference(p^.left^.left^.location.reference);
                        end
                      else
                        { LOC_CREGISTER }
                        exprasmlist^.concat(new(pai386,op_const_reg(asmop,S_L,
                          l,p^.left^.left^.location.register)));
                   end
                 else
                   begin
                      { generate code for the element to set }
                      ispushed:=maybe_push(p^.left^.right^.left^.registers32,p^.left^.left);
                      secondpass(p^.left^.right^.left);
                      if ispushed then
                        restore(p^.left^.left);
                      { determine asm operator }
                      if p^.inlinenumber=in_include_x_y then
                        asmop:=A_BTS
                      else
                        asmop:=A_BTR;
                      if psetdef(p^.left^.resulttype)^.settype=smallset then
                        begin
                           if p^.left^.right^.left^.location.loc in
                             [LOC_CREGISTER,LOC_REGISTER] then
                             hregister:=p^.left^.right^.left^.location.register
                           else
                             begin
                                hregister:=R_EDI;
                                exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,
                                  newreference(p^.left^.right^.left^.location.reference),
                                  R_EDI)));
                             end;
                          if (p^.left^.left^.location.loc=LOC_REFERENCE) then
                            exprasmlist^.concat(new(pai386,op_reg_ref(asmop,S_L,R_EDI,
                              newreference(p^.left^.right^.left^.location.reference))))
                          else
                            exprasmlist^.concat(new(pai386,op_reg_reg(asmop,S_L,R_EDI,
                              p^.left^.right^.left^.location.register)));
                        end
                      else
                        begin
                        end;
                   end;
              end;
            else internalerror(9);
         end;
      end;

    procedure secondsubscriptn(var p : ptree);

      var
         hr : tregister;

      begin
         secondpass(p^.left);

         if codegenerror then
             exit;
         { classes must be dereferenced implicit }
         if (p^.left^.resulttype^.deftype=objectdef) and
           pobjectdef(p^.left^.resulttype)^.isclass then
           begin
             clear_reference(p^.location.reference);
             case p^.left^.location.loc of
                LOC_REGISTER:
                  p^.location.reference.base:=p^.left^.location.register;
                LOC_CREGISTER:
                  begin
                     { ... and reserve one for the pointer }
                     hr:=getregister32;
                     emit_reg_reg(A_MOV,S_L,p^.left^.location.register,hr);
                       p^.location.reference.base:=hr;
                  end;
                else
                  begin
                     { free register }
                     del_reference(p^.left^.location.reference);

                     { ... and reserve one for the pointer }
                     hr:=getregister32;
                     exprasmlist^.concat(new(pai386,op_ref_reg(
                       A_MOV,S_L,newreference(p^.left^.location.reference),
                       hr)));
                     p^.location.reference.base:=hr;
                  end;
             end;
           end
         else
           set_location(p^.location,p^.left^.location);

         inc(p^.location.reference.offset,p^.vs^.address);
      end;

    procedure secondselfn(var p : ptree);

      begin
         clear_reference(p^.location.reference);
         if (p^.resulttype^.deftype=classrefdef) or
           ((p^.resulttype^.deftype=objectdef)
             and pobjectdef(p^.resulttype)^.isclass
           ) then
           p^.location.register:=R_ESI
         else
           p^.location.reference.base:=R_ESI;
      end;

    procedure secondhdisposen(var p : ptree);

      begin
         secondpass(p^.left);

         if codegenerror then
           exit;
         clear_reference(p^.location.reference);
         case p^.left^.location.loc of
            LOC_REGISTER,
            LOC_CREGISTER:
              begin
                 p^.location.reference.index:=getregister32;
                 exprasmlist^.concat(new(pai386,op_reg_reg(A_MOV,S_L,
                   p^.left^.location.register,
                   p^.location.reference.index)));
              end;
            LOC_MEM,LOC_REFERENCE :
              begin
                 del_reference(p^.left^.location.reference);
                 p^.location.reference.index:=getregister32;
                 exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,newreference(p^.left^.location.reference),
                   p^.location.reference.index)));
              end;
         end;
      end;

    procedure secondhnewn(var p : ptree);

      begin
       end;

    procedure secondnewn(var p : ptree);

      begin
         secondpass(p^.left);

           if codegenerror then
           exit;

         p^.location.register:=p^.left^.location.register;
      end;

    procedure secondsimplenewdispose(var p : ptree);

      var
         pushed : tpushed;
      begin
         secondpass(p^.left);
         if codegenerror then
           exit;

         pushusedregisters(pushed,$ff);
         { determines the size of the mem block }
         push_int(ppointerdef(p^.left^.resulttype)^.definition^.size);

         { push pointer adress }
         case p^.left^.location.loc of
            LOC_CREGISTER : exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,
              p^.left^.location.register)));
            LOC_REFERENCE : emitpushreferenceaddr(p^.left^.location.reference);

         end;

         { call the mem handling procedures }
         case p^.treetype of
           simpledisposen:
             emitcall('FREEMEM',true);
           simplenewn:
             emitcall('GETMEM',true);
         end;

         popusedregisters(pushed);
           { may be load ESI }
           maybe_loadesi;
       end;

     { copies p a set element on the stack }

     procedure pushsetelement(var p : ptree);

      var
         hr : tregister;

      begin
           { copy the element on the stack, slightly complicated }
         case p^.location.loc of
               LOC_REGISTER,
            LOC_CREGISTER : begin
                              hr:=p^.location.register;
                              case hr of
                                 R_EAX,R_EBX,R_ECX,R_EDX,R_EDI,R_ESI,R_ESP :
                                   begin
                                      ungetregister32(hr);
                                      exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_W,reg32toreg16(hr))));
                                   end;
                                 R_AX,R_BX,R_CX,R_DX,R_DI,R_SI,R_SP :
                                   begin
                                      ungetregister32(reg16toreg32(hr));
                                      exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_W,hr)));
                                   end;
                                 R_AL,R_BL,R_CL,R_DL :
                                   begin
                                      ungetregister32(reg8toreg32(hr));
                                      exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_W,reg8toreg16(hr))));
                                   end;
                              end;
                           end;
            else
               begin
                  exprasmlist^.concat(new(pai386,op_ref(A_PUSH,S_W,newreference(p^.location.reference))));
                  del_reference(p^.location.reference);
               end;
         end;
      end;

    procedure secondsetcons(var p : ptree);

      var
         l : plabel;
         i : longint;
         hp : ptree;
         href,sref : treference;
         hr : tregister;

      begin
         { this should be reimplemented for smallsets }
         { differently  (PM) }
         { produce constant part }
{$ifdef TestSmallSet}
         if psetdef(p^.resulttype)=smallset then
           begin
              smallsetvalue:=(p^.constset^[3]*256)+p^.constset^[2];
              smallsetvalue:=((smallset*256+p^.constset^[1])*256+p^.constset^[1];
              {consts^.concat(new(pai_const,init_32bit(smallsetvalue)));}
              hr:=getregister32;
              exprasmlist^.concat(new(pai386,op_const_reg(A_MOV,S_L,
                smallsetvalue,hr)));
              hp:=p^.left;
              if assigned(hp) then
                begin
                   while assigned(hp) do
                     begin
                        secondpass(hp^.left);
                        if codegenerror then
                          exit;
                        case hp^.left^.location.loc of
                          LOC_MEM,LOC_REFERENCE :
                            exprasmlist^.concat(new(pai386,op_ref_reg(A_BTS,S_L,
                              newreference(p^.left^.location.reference),hr)));
                          LOC_REGISTER,LOC_CREGISTER :
                            exprasmlist^.concat(new(pai386,op_reg_reg(A_BTS,S_L,
                              p^.left^.location.register,hr)));
                          else
                            internalerror(10567);
                          end
                        hp:=hp^.right;
                     end;
                end;
              p^.location.loc:=LOC_REGISTER;
              p^.location.register:=hr;
           end
         else
{$endif TestSmallSet}
           begin
         href.symbol := Nil;
         clear_reference(href);
         getlabel(l);
         stringdispose(p^.location.reference.symbol);
         href.symbol:=stringdup(constlabel2str(l,constseta));
         concat_constlabel(l,constseta);
           for i:=0 to 31 do
             consts^.concat(new(pai_const,init_8bit(p^.constset^[i])));
         hp:=p^.left;
         if assigned(hp) then
           begin
              sref.symbol:=nil;
              gettempofsizereference(32,sref);
                concatcopy(href,sref,32,false);
              while assigned(hp) do
                begin
                   secondpass(hp^.left);
                   if codegenerror then
                     exit;

                   pushsetelement(hp^.left);
                   emitpushreferenceaddr(sref);
                   { register is save in subroutine }
                   emitcall('SET_SET_BYTE',true);
                   hp:=hp^.right;
                end;
              p^.location.reference:=sref;
           end
         else p^.location.reference:=href;
         end;
      end;

    { could be built into secondadd but it }
    { should be easy to read }
    procedure secondin(var p : ptree);


       type    Tsetpart=record
                    range:boolean;      {Part is a range.}
                    start,stop:byte;    {Start/stop when range; Stop=element
                                              when an element.}
               end;

       var
           pushed,ranges : boolean;
           hr,pleftreg : tregister;
           opsize : topsize;
           setparts:array[1..8] of Tsetpart;
           i,numparts:byte;
           href,href2:Treference;
           l,l2 : plabel;


               function analizeset(Aset:Pconstset):boolean;

               var  compares,maxcompares:word;
                    i:byte;

               type byteset=set of byte;

               begin
                    analizeset:=false;
                    ranges:=false;
                    numparts:=0;
                    compares:=0;
                    {Lots of comparisions take a lot of time, so do not allow
                     too much comparisions. 8 comparisions are, however, still
                     smalller than emitting the set.}
                    maxcompares:=5;
                    if cs_littlesize in aktswitches then
                         maxcompares:=8;
                    for i:=0 to 255 do
                         if i in byteset(Aset^) then
                              begin
                                   if (numparts=0) or
                                    (i<>setparts[numparts].stop+1) then
                                        begin
                                             {Set element is a separate element.}
                                             inc(compares);
                                             if compares>maxcompares then
                                                  exit;
                                             inc(numparts);
                                             setparts[numparts].range:=false;
                                             setparts[numparts].stop:=i;
                                        end
                                    else
                                        {Set element is part of a range.}
                                        if not setparts[numparts].range then
                                             begin
                                                  {Transform an element into a range.}
                                                  setparts[numparts].range:=true;
                                                  setparts[numparts].start:=
                                                   setparts[numparts].stop;
                                                  setparts[numparts].stop:=i;
                                                  inc(compares);
                                                  if compares>maxcompares then
                                                       exit;
                                             end
                                        else
                                             begin
                                                  {Extend a range.}
                                                  setparts[numparts].stop:=i;
                                                  {A range of two elements can better
                                                   be checked as two separate ones.
                                                   When extending a range, our range
                                                   becomes larger than two elements.}
                                                  ranges:=true;
                                             end;
                              end;
                    analizeset:=true;
               end;

       begin
           if psetdef(p^.right^.resulttype)^.settype=smallset then
             begin
                 if p^.left^.treetype=ordconstn then
                    begin
                       { only compulsory }
                       secondpass(p^.left);
                            secondpass(p^.right);
                       if codegenerror then
                          exit;
                       p^.location.resflags:=F_NE;
                       case p^.right^.location.loc of
                          LOC_REGISTER,LOC_CREGISTER:
                            begin
                               exprasmlist^.concat(new(pai386,op_const_reg(
                                 A_TEST,S_L,1 shl (p^.left^.value and 31),
                                 p^.right^.location.register)));
                               ungetregister32(p^.right^.location.register);
                            end
                          else
                            begin
                               exprasmlist^.concat(new(pai386,op_const_ref(A_TEST,S_L,1 shl (p^.left^.value and 31),
                                 newreference(p^.right^.location.reference))));
                               del_reference(p^.right^.location.reference);
                            end;
                       end;
                    end
                 else
                    begin
                       { calculate both operators }
                       { the complex one first }
                       firstcomplex(p);
                       secondpass(p^.left);
                       { are too few registers free? }
                       pushed:=maybe_push(p^.right^.registers32,p^.left);
                       secondpass(p^.right);
                       if pushed then
                          restore(p^.left);
                       { of course not commutative }
                       if p^.swaped then
                              swaptree(p);
                       case p^.left^.location.loc of
                         LOC_REGISTER,
                         LOC_CREGISTER:
                           begin
                              hr:=p^.left^.location.register;
                              case p^.left^.location.register of
                                 R_AX,R_BX,R_CX,R_DX,R_DI,R_SI,R_SP :
                                    begin
                                        hr:=reg16toreg32(p^.left^.location.register);
                                        ungetregister32(hr);
                                        exprasmlist^.concat(new(pai386,op_reg_reg(A_MOVZX,S_WL,
                                          p^.left^.location.register,hr)));
                                    end;
                                 R_AL,R_BL,R_CL,R_DL :
                                    begin
                                        hr:=reg8toreg32(p^.left^.location.register);
                                        ungetregister32(hr);
                                        exprasmlist^.concat(new(pai386,op_reg_reg(A_MOVZX,S_BL,
                                          p^.left^.location.register,hr)));
                                    end;
                              end;
                           end;
                         else
                             begin
                                 { the set element isn't never samller than a byte  }
                                 { and because it's a small set we need only 5 bits }
                                 { but 8 bits are eaiser to load                    }
                                 exprasmlist^.concat(new(pai386,op_ref_reg(A_MOVZX,S_BL,
                                   newreference(p^.left^.location.reference),R_EDI)));
                                 hr:=R_EDI;
                                 del_reference(p^.left^.location.reference);
                             end;
                       end;
                       case p^.right^.location.loc of
                         LOC_REGISTER,
                         LOC_CREGISTER:
                           exprasmlist^.concat(new(pai386,op_reg_reg(A_BT,S_L,hr,
                             p^.right^.location.register)));
                         else
                            begin
                               exprasmlist^.concat(new(pai386,op_reg_ref(A_BT,S_L,hr,
                                 newreference(p^.right^.location.reference))));
                                        del_reference(p^.right^.location.reference);
                            end;
                       end;
                       p^.location.loc:=LOC_FLAGS;
                       p^.location.resflags:=F_C;
                    end;
             end
           else
             begin
                 if p^.left^.treetype=ordconstn then
                    begin
                       { only compulsory }
                       secondpass(p^.left);
                       secondpass(p^.right);
                       if codegenerror then
                          exit;
                       p^.location.resflags:=F_NE;
                       inc(p^.right^.location.reference.offset,p^.left^.value shr 3);
                       exprasmlist^.concat(new(pai386,op_const_ref(A_TEST,S_B,1 shl (p^.left^.value and 7),
                          newreference(p^.right^.location.reference))));
                       del_reference(p^.right^.location.reference);
                    end
                 else
                    begin
                       if (p^.right^.treetype=setconstrn) and
                         analizeset(p^.right^.constset) then
                         begin
                            {It gives us advantage to check for the set elements
                             separately instead of using the SET_IN_BYTE procedure.
                             To do: Build in support for LOC_JUMP.}
                            secondpass(p^.left);
                            {We won't do a second pass on p^.right, because
                             this will emit the constant set.}
                            {If register is used, use only lower 8 bits}
                            if p^.left^.location.loc in [LOC_REGISTER,LOC_CREGISTER] then
                             begin
                               pleftreg:=p^.left^.location.register;
                               if pleftreg in [R_AL..R_DH] then
                                 begin
                                    exprasmlist^.concat(new(pai386,op_const_reg(
                                      A_AND,S_B,255,pleftreg)));
                                    opsize:=S_B;
                                 end
                               else
                                 begin
                                    exprasmlist^.concat(new(pai386,op_const_reg(
                                      A_AND,S_L,255,pleftreg)));
                                    if pleftreg in [R_EAX..R_EDI] then
                                      opsize:=S_L
                                    else
                                      opsize:=S_W;
                                 end;
                             end;
                            {Get a label to jump to the end.}
                            p^.location.loc:=LOC_FLAGS;
                            {It's better to use the zero flag when there are
                             no ranges.}
                            if ranges then
                              p^.location.resflags:=F_C
                            else
                              p^.location.resflags:=F_E;
                            href.symbol := nil;
                            clear_reference(href);
                            getlabel(l);
                            href.symbol:=stringdup(lab2str(l));
                            for i:=1 to numparts do
                              if setparts[i].range then
                                begin
                                   {Check if left is in a range.}
                                   {Get a label to jump over the check.}
                                   href2.symbol := nil;
                                   clear_reference(href2);
                                   getlabel(l2);
                                   href.symbol:=stringdup(lab2str(l2));
                                   if setparts[i].start=setparts[i].stop-1 then
                                     begin
                                        case p^.left^.location.loc of
                                           LOC_REGISTER,
                                           LOC_CREGISTER :
                                             exprasmlist^.concat(new(pai386,op_const_reg(A_CMP,opsize,
                                               setparts[i].start,pleftreg)));
                                           else
                                             exprasmlist^.concat(new(pai386,op_const_ref(A_CMP,S_B,
                                               setparts[i].start,newreference(p^.left^.location.reference))));
                                        end;
                                        {Result should be in carry flag when ranges are used.}
                                        if ranges then
                                          exprasmlist^.concat(new(pai386,op_none(A_STC,S_NO)));
                                        {If found, jump to end.}
                                        emitl(A_JE,l);
                                        case p^.left^.location.loc of
                                           LOC_REGISTER,
                                           LOC_CREGISTER:
                                             exprasmlist^.concat(new(pai386,op_const_reg(A_CMP,opsize,
                                               setparts[i].stop,pleftreg)));
                                           else
                                             exprasmlist^.concat(new(pai386,op_const_ref(A_CMP,S_B,
                                               setparts[i].stop,newreference(p^.left^.location.reference))));
                                        end;
                                        {Result should be in carry flag when ranges are used.}
                                        if ranges then
                                          exprasmlist^.concat(new(pai386,op_none(A_STC,S_NO)));
                                        {If found, jump to end.}
                                        emitl(A_JE,l);
                                     end
                                   else
                                     begin
                                        if setparts[i].start<>0 then
                                          begin
                                             { We only check for the lower bound if it is > 0, because
                                             set elements lower than 0 do nt exist.}
                                             case p^.left^.location.loc of
                                               LOC_REGISTER,
                                               LOC_CREGISTER :
                                                 exprasmlist^.concat(new(pai386,op_const_reg(A_CMP,opsize,
                                                 setparts[i].start,pleftreg)));
                                               else
                                                 exprasmlist^.concat(new(pai386,op_const_ref(A_CMP,S_B,
                                               setparts[i].start,newreference(p^.left^.location.reference))));
                                             end;
                                             {If lower, jump to next check.}
                                             emitl(A_JB,l2);
                                          end;
                                        if setparts[i].stop<>255 then
                                          begin
                                             { We only check for the high bound if it is < 255, because
                                               set elements higher than 255 do nt exist.}
                                             case p^.left^.location.loc of
                                               LOC_REGISTER,
                                               LOC_CREGISTER :
                                                 exprasmlist^.concat(new(pai386,op_const_reg(A_CMP,opsize,
                                                   setparts[i].stop+1,pleftreg)));
                                               else
                                                 exprasmlist^.concat(new(pai386,op_const_ref(A_CMP,S_B,
                                                   setparts[i].stop+1,newreference(p^.left^.location.reference))));
                                             end;
                                             {If higher, element is in set.}
                                             emitl(A_JB,l);
                                          end;
                                      end;
                                   {Emit the jump over label.}
                                   exprasmlist^.concat(new(pai_label,init(l2)));
                                end
                              else
                                begin
                                   {Emit code to check if left is an element.}
                                   case p^.left^.location.loc of
                                      LOC_REGISTER,
                                      LOC_CREGISTER:
                                        exprasmlist^.concat(new(pai386,op_const_reg(A_CMP,opsize,
                                          setparts[i].stop,pleftreg)));
                                      else
                                        exprasmlist^.concat(new(pai386,op_const_ref(A_CMP,S_B,
                                          setparts[i].stop,newreference(p^.left^.location.reference))));
                                   end;
                                   {Result should be in carry flag when ranges are used.}
                                   if ranges then
                                     exprasmlist^.concat(new(pai386,op_none(A_STC,S_NO)));
                                   {If found, jump to end.}
                                   emitl(A_JE,l);
                                end;
                            if ranges then
                              exprasmlist^.concat(new(pai386,op_none(A_CLC,S_NO)));
                            {To compensate for not doing a second pass.}
                            stringdispose(p^.right^.location.reference.symbol);
                            {Now place the end label.}
                            exprasmlist^.concat(new(pai_label,init(l)));
                            case p^.left^.location.loc of
                               LOC_REGISTER,
                               LOC_CREGISTER:
                                 ungetregister32(pleftreg);
                               else
                                 del_reference(p^.left^.location.reference);
                            end;
                         end
                       else
                         begin
                            { calculate both operators }
                            { the complex one first }
                            firstcomplex(p);
                            secondpass(p^.left);
                            { are too few registers free? }
                            pushed:=maybe_push(p^.right^.registers32,p);
                            secondpass(p^.right);
                            if pushed then restore(p);
                            { of course not commutative }
                            if p^.swaped then
                              swaptree(p);
                            pushsetelement(p^.left);
                            emitpushreferenceaddr(p^.right^.location.reference);
                            del_reference(p^.right^.location.reference);
                            { registers need not be save. that happens in SET_IN_BYTE }
                            { (EDI is changed) }
                            emitcall('SET_IN_BYTE',true);
                            { ungetiftemp(p^.right^.location.reference); }
                            p^.location.loc:=LOC_FLAGS;
                            p^.location.resflags:=F_C;
                         end;
                    end;
                end;
       end;
{***}

    procedure secondstatement(var p : ptree);
      var
         hp : ptree;

      begin
         hp:=p;
         while assigned(hp) do
           begin
              { assignments could be distance optimized }
              if assigned(hp^.right) then
                begin
                   cleartempgen;
                   secondpass(hp^.right);
                   (* if (hp^.right^.resulttype<>pdef(voiddef)) then
                     if hp^.right^.location.loc in [LOC_MEM,LOC_REFERENCE] then
                     { release unused temp }
                       ungetiftemp(hp^.right^.location.reference)
                     else if hp^.right^.location.loc=LOC_FPU then
                     { release FPU stack }
                       exprasmlist^.concat(new(pai386,op_none(A_FDECSTP,S_NO)));
                     All done in secondcalln now (PM) *)
                end;
              hp:=hp^.left;
           end;
      end;


    procedure secondblockn(var p : ptree);

      begin
         { do second pass on left node }
         if assigned(p^.left) then
           secondpass(p^.left);
      end;

 
    procedure second_while_repeatn(var p : ptree);

      var
         l1,l2,l3,oldclabel,oldblabel : plabel;
         otlabel,oflabel : plabel;

      begin
         getlabel(l1);
         getlabel(l2);
         { arrange continue and breaklabels: }
         oldclabel:=aktcontinuelabel;
         oldblabel:=aktbreaklabel;
         if p^.treetype=repeatn then
           begin
              emitl(A_LABEL,l1);
              aktcontinuelabel:=l1;
              aktbreaklabel:=l2;
              cleartempgen;
              if assigned(p^.right) then
                secondpass(p^.right);

              otlabel:=truelabel;
              oflabel:=falselabel;
              truelabel:=l2;
              falselabel:=l1;
              cleartempgen;
              secondpass(p^.left);
              maketojumpbool(p^.left);
              emitl(A_LABEL,l2);
              truelabel:=otlabel;
              falselabel:=oflabel;
           end
         else
           begin
              { handling code at the end as it is much more efficient }
              emitl(A_JMP,l2);

              emitl(A_LABEL,l1);
              cleartempgen;

              getlabel(l3);
              aktcontinuelabel:=l2;
              aktbreaklabel:=l3;

              if assigned(p^.right) then
                secondpass(p^.right);

              emitl(A_LABEL,l2);
              otlabel:=truelabel;
              oflabel:=falselabel;
              truelabel:=l1;
              falselabel:=l3;
              cleartempgen;
              secondpass(p^.left);
              maketojumpbool(p^.left);

              emitl(A_LABEL,l3);
              truelabel:=otlabel;
              falselabel:=oflabel;
           end;
         aktcontinuelabel:=oldclabel;
         aktbreaklabel:=oldblabel;
      end;

    procedure secondifn(var p : ptree);

      var
         hl,otlabel,oflabel : plabel;

      begin
         otlabel:=truelabel;
         oflabel:=falselabel;
         getlabel(truelabel);
         getlabel(falselabel);
         cleartempgen;
         secondpass(p^.left);
         maketojumpbool(p^.left);
         if assigned(p^.right) then
           begin
              emitl(A_LABEL,truelabel);
              cleartempgen;
              secondpass(p^.right);
           end;
         if assigned(p^.t1) then
               begin
              if assigned(p^.right) then
                        begin
                   getlabel(hl);
                   emitl(A_JMP,hl);
                end;
              emitl(A_LABEL,falselabel);
              cleartempgen;
              secondpass(p^.t1);
              if assigned(p^.right) then
                emitl(A_LABEL,hl);
           end
         else
           emitl(A_LABEL,falselabel);
         if not(assigned(p^.right)) then
           emitl(A_LABEL,truelabel);
         truelabel:=otlabel;
         falselabel:=oflabel;
      end;

    procedure secondbreakn(var p : ptree);

      begin
         if aktbreaklabel<>nil then
           emitl(A_JMP,aktbreaklabel)
         else
           Message(cg_e_break_not_allowed);
      end;

    procedure secondcontinuen(var p : ptree);

      begin
         if aktcontinuelabel<>nil then
           emitl(A_JMP,aktcontinuelabel)
         else
           Message(cg_e_continue_not_allowed);
      end;

    procedure secondfor(var p : ptree);

      var
         l3,oldclabel,oldblabel : plabel;
         omitfirstcomp,temptovalue : boolean;
         hs : byte;
         temp1 : treference;
         hop : tasmop;
         cmpreg,cmp32 : tregister;
         opsize : topsize;
         count_var_is_signed : boolean;

      begin
         oldclabel:=aktcontinuelabel;
         oldblabel:=aktbreaklabel;
         getlabel(aktcontinuelabel);
         getlabel(aktbreaklabel);
         getlabel(l3);

         { could we spare the first comparison ? }
             omitfirstcomp:=false;
         if p^.right^.treetype=ordconstn then
           if p^.left^.right^.treetype=ordconstn then
             omitfirstcomp:=(p^.backward and (p^.left^.right^.value>=p^.right^.value))
               or (not(p^.backward) and (p^.left^.right^.value<=p^.right^.value));

         { only calculate reference }
         cleartempgen;
         secondpass(p^.t2);
         if not(simple_loadn) then
          Message(cg_e_illegal_count_var);

         { produce start assignment }
         cleartempgen;
         secondpass(p^.left);
         count_var_is_signed:=is_signed(porddef(p^.t2^.resulttype));
             hs:=p^.t2^.resulttype^.size;
         cmp32:=getregister32;
             case hs of
            1 : begin
                   opsize:=S_B;
                   cmpreg:=reg32toreg8(cmp32);
                end;
            2 : begin
                   opsize:=S_W;
                   cmpreg:=reg32toreg16(cmp32);
                end;
            4 : begin
                   opsize:=S_L;
                   cmpreg:=cmp32;
                end;
         end;
         cleartempgen;
             secondpass(p^.right);
         { calculate pointer value and check if changeable and if so }
         { load into temporary variable                              }
         if p^.right^.treetype<>ordconstn then
           begin
              temp1.symbol:=nil;
              gettempofsizereference(hs,temp1);
              temptovalue:=true;
              if (p^.right^.location.loc=LOC_REGISTER) or
                 (p^.right^.location.loc=LOC_CREGISTER) then
                begin
                   exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,opsize,p^.right^.location.register,
                      newreference(temp1))));
                 end
              else
                 concatcopy(p^.right^.location.reference,temp1,hs,false);
           end
         else temptovalue:=false;

         if temptovalue then
             begin
              if p^.t2^.location.loc=LOC_CREGISTER then
                begin
                   exprasmlist^.concat(new(pai386,op_ref_reg(A_CMP,opsize,newreference(temp1),
                     p^.t2^.location.register)));
                end
              else
                begin
                   exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,opsize,newreference(p^.t2^.location.reference),
                     cmpreg)));
                   exprasmlist^.concat(new(pai386,op_ref_reg(A_CMP,opsize,newreference(temp1),
                     cmpreg)));
                end;
           end
         else
             begin
              if not(omitfirstcomp) then
                begin
                   if p^.t2^.location.loc=LOC_CREGISTER then
                     exprasmlist^.concat(new(pai386,op_const_reg(A_CMP,opsize,p^.right^.value,
                       p^.t2^.location.register)))
                   else
                     exprasmlist^.concat(new(pai386,op_const_ref(A_CMP,opsize,p^.right^.value,
                 newreference(p^.t2^.location.reference))));
                end;
           end;
         if p^.backward then
           if count_var_is_signed then
             hop:=A_JL
           else hop:=A_JB
         else
           if count_var_is_signed then
             hop:=A_JG
            else hop:=A_JA;

             if not(omitfirstcomp) or temptovalue then
           emitl(hop,aktbreaklabel);

         emitl(A_LABEL,l3);

         { help register must not be in instruction block }
         cleartempgen;
         if assigned(p^.t1) then
           secondpass(p^.t1);

         emitl(A_LABEL,aktcontinuelabel);

         { makes no problems there }
         cleartempgen;

         { demand help register again }
         cmp32:=getregister32;
         case hs of
            1 : begin
                   opsize:=S_B;
                   cmpreg:=reg32toreg8(cmp32);
                end;
            2 : begin
                   opsize:=S_W;
                   cmpreg:=reg32toreg16(cmp32);
                end;
            4 : opsize:=S_L;
         end;

          { produce comparison and the corresponding }
         { jump                                     }
         if temptovalue then
           begin
              if p^.t2^.location.loc=LOC_CREGISTER then
                begin
                   exprasmlist^.concat(new(pai386,op_ref_reg(A_CMP,opsize,newreference(temp1),
                     p^.t2^.location.register)));
                end
              else
                begin
                   exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,opsize,newreference(p^.t2^.location.reference),
                     cmpreg)));
                   exprasmlist^.concat(new(pai386,op_ref_reg(A_CMP,opsize,newreference(temp1),
                     cmpreg)));
                    end;
           end
         else
           begin
              if p^.t2^.location.loc=LOC_CREGISTER then
                exprasmlist^.concat(new(pai386,op_const_reg(A_CMP,opsize,p^.right^.value,
                  p^.t2^.location.register)))
              else
                 exprasmlist^.concat(new(pai386,op_const_ref(A_CMP,opsize,p^.right^.value,
                   newreference(p^.t2^.location.reference))));
           end;
         if p^.backward then
           if count_var_is_signed then
             hop:=A_JLE
           else
             hop :=A_JBE
          else
            if count_var_is_signed then
              hop:=A_JGE
            else
                hop:=A_JAE;
         emitl(hop,aktbreaklabel);
         { according to count direction DEC or INC... }
         { must be after the test because of 0to 255 for bytes !! }
         if p^.backward then
           hop:=A_DEC
         else hop:=A_INC;

         if p^.t2^.location.loc=LOC_CREGISTER then
           exprasmlist^.concat(new(pai386,op_reg(hop,opsize,p^.t2^.location.register)))
         else
             exprasmlist^.concat(new(pai386,op_ref(hop,opsize,newreference(p^.t2^.location.reference))));
         emitl(A_JMP,l3);

           { this is the break label: }
         emitl(A_LABEL,aktbreaklabel);
         ungetregister32(cmp32);

         if temptovalue then
           ungetiftemp(temp1);

         aktcontinuelabel:=oldclabel;
         aktbreaklabel:=oldblabel;
      end;

{    var
       hs : string; }

    procedure secondexitn(var p : ptree);

      var
         is_mem : boolean;
         {op : tasmop;
         s : topsize;}
         otlabel,oflabel : plabel;

      label
         do_jmp;

      begin
         if assigned(p^.left) then
           begin
              otlabel:=truelabel;
              oflabel:=falselabel;
              getlabel(truelabel);
              getlabel(falselabel);
              secondpass(p^.left);
              case p^.left^.location.loc of
                 LOC_FPU : goto do_jmp;
                 LOC_MEM,LOC_REFERENCE : is_mem:=true;
                 LOC_CREGISTER,
                 LOC_REGISTER : is_mem:=false;
                     LOC_FLAGS : begin
                                exprasmlist^.concat(new(pai386,op_reg(flag_2_set[p^.right^.location.resflags],S_NO,R_AL)));
                                        goto do_jmp;
                             end;
                 LOC_JUMP : begin
                                      emitl(A_LABEL,truelabel);
                               exprasmlist^.concat(new(pai386,op_const_reg(A_MOV,S_B,1,R_AL)));
                               emitl(A_JMP,aktexit2label);
                               exprasmlist^.concat(new(pai386,op_reg_reg(A_XOR,S_B,R_AL,R_AL)));
                               goto do_jmp;
                            end;
                 else internalerror(2001);
              end;
                 if (procinfo.retdef^.deftype=orddef) then
                begin
                   case porddef(procinfo.retdef)^.typ of
                      s32bit,u32bit : if is_mem then
                                                  exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,
                                          newreference(p^.left^.location.reference),R_EAX)))
                                      else
                                        emit_reg_reg(A_MOV,S_L,
                                          p^.left^.location.register,R_EAX);
                           u8bit,s8bit,uchar,bool8bit : if is_mem then
                                        exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_B,
                                          newreference(p^.left^.location.reference),R_AL)))
                                      else
                                        emit_reg_reg(A_MOV,S_B,
                                          p^.left^.location.register,R_AL);
                      s16bit,u16bit : if is_mem then
                                        exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_W,
                                          newreference(p^.left^.location.reference),R_AX)))
                                      else
                                        emit_reg_reg(A_MOV,S_W,
                                                    p^.left^.location.register,R_AX);
                   end;
                end
                  else
                     if (procinfo.retdef^.deftype in
                          [pointerdef,enumdef,procvardef]) then
                       begin
                           if is_mem then
                              exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,
                                newreference(p^.left^.location.reference),R_EAX)))
                           else
                              exprasmlist^.concat(new(pai386,op_reg_reg(A_MOV,S_L,
                                p^.left^.location.register,R_EAX)));
                       end
                 else
                    if (procinfo.retdef^.deftype=floatdef) then
                      begin
                          if pfloatdef(procinfo.retdef)^.typ=f32bit then
                            begin
                                if is_mem then
                                   exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,
                              newreference(p^.left^.location.reference),R_EAX)))
                          else
                            emit_reg_reg(A_MOV,S_L,
                              p^.left^.location.register,R_EAX);
                       end
                     else
                       if is_mem then
                         floatload(pfloatdef(procinfo.retdef)^.typ,p^.left^.location.reference);
                end;
do_jmp:
              truelabel:=otlabel;
              falselabel:=oflabel;
              emitl(A_JMP,aktexit2label);
           end
         else
           begin
              emitl(A_JMP,aktexitlabel);
           end;
       end;

    procedure secondgoto(var p : ptree);

       begin
         emitl(A_JMP,p^.labelnr);
       end;

    procedure secondlabel(var p : ptree);

      begin
         emitl(A_LABEL,p^.labelnr);
         cleartempgen;
         secondpass(p^.left);
      end;

    procedure secondasm(var p : ptree);

      begin
         exprasmlist^.concatlist(p^.p_asm);
       end;

    procedure secondcase(var p : ptree);

      var
         with_sign : boolean;
         opsize : topsize;
         jmp_gt,jmp_le,jmp_lee : tasmop;
         hp : ptree;
         { register with case expression }
         hregister : tregister;
         endlabel,elselabel : plabel;

         { true, if we can omit the range check of the jump table }
         jumptable_no_range : boolean;
         { where to put the jump table }
         jumpsegment : paasmoutput;

      procedure gentreejmp(p : pcaserecord);

        var
           lesslabel,greaterlabel : plabel;

       begin
         emitl(A_LABEL,p^._at);
         { calculate labels for left and right }
         if (p^.less=nil) then
           lesslabel:=elselabel
         else
           lesslabel:=p^.less^._at;
         if (p^.greater=nil) then
           greaterlabel:=elselabel
         else
           greaterlabel:=p^.greater^._at;
           { calculate labels for left and right }
         { no range label: }
         if p^._low=p^._high then
           begin
              exprasmlist^.concat(new(pai386,op_const_reg(A_CMP,opsize,p^._low,hregister)));
              if greaterlabel=lesslabel then
                begin
                   emitl(A_JNE,lesslabel);
                end
              else
                begin
                   emitl(jmp_le,lesslabel);
                   emitl(jmp_gt,greaterlabel);
                end;
              emitl(A_JMP,p^.statement);
           end
         else
           begin
              exprasmlist^.concat(new(pai386,op_const_reg(A_CMP,opsize,p^._low,hregister)));
              emitl(jmp_le,lesslabel);
                exprasmlist^.concat(new(pai386,op_const_reg(A_CMP,opsize,p^._high,hregister)));
              emitl(jmp_gt,greaterlabel);
              emitl(A_JMP,p^.statement);
           end;
          if assigned(p^.less) then
           gentreejmp(p^.less);
          if assigned(p^.greater) then
           gentreejmp(p^.greater);
      end;

      procedure genlinearlist(hp : pcaserecord);

        var
           first : boolean;
           last : longint;
           {helplabel : longint;}

        procedure genitem(t : pcaserecord);

          begin
             if assigned(t^.less) then
               genitem(t^.less);
             if t^._low=t^._high then
               begin
                  if t^._low-last=1 then
                    exprasmlist^.concat(new(pai386,op_reg(A_DEC,opsize,hregister)))
                  else if t^._low-last=0 then
                    exprasmlist^.concat(new(pai386,op_reg_reg(A_OR,opsize,hregister,hregister)))
                  else
                    exprasmlist^.concat(new(pai386,op_const_reg(A_SUB,opsize,t^._low-last,hregister)));
                  last:=t^._low;

                  emitl(A_JZ,t^.statement);
               end
             else
               begin
                  { it begins with the smallest label, if the value }
                  { is even smaller then jump immediately to the    }
                  { ELSE-label                                      }
                  if first then
                    begin
                       if t^._low-1=1 then
                         exprasmlist^.concat(new(pai386,op_reg(A_DEC,opsize,
                           hregister)))
                       else if t^._low-1=0 then
                         exprasmlist^.concat(new(pai386,op_reg_reg(A_OR,opsize,
                           hregister,hregister)))
                       else
                         exprasmlist^.concat(new(pai386,op_const_reg(A_SUB,opsize,
                           t^._low-1,hregister)));
                       { work around: if the lower range=0 and we
                         do the subtraction we have to take care
                         of the sign!
                       }
                       if t^._low=0 then
                         emitl(A_JLE,elselabel)
                       else
                         emitl(jmp_lee,elselabel);
                    end
                  { if there is no unused label between the last and the }
                  { present label then the lower limit can be checked    }
                  { immediately. else check the range in between:        }
                  else if (t^._low-last>1)then
                    begin
                       if t^._low-last-1=1 then
                         exprasmlist^.concat(new(pai386,op_reg(A_DEC,opsize,hregister)))
                       else
                         exprasmlist^.concat(new(pai386,op_const_reg(A_SUB,opsize,t^._low-last-1,hregister)));
                       emitl(jmp_lee,elselabel);
                    end;
                  exprasmlist^.concat(new(pai386,op_const_reg(A_SUB,opsize,t^._high-t^._low+1,hregister)));
                  emitl(jmp_lee,t^.statement);

                  last:=t^._high;
               end;
             first:=false;
             if assigned(t^.greater) then
               genitem(t^.greater);
          end;

        var
           hr : tregister;

          begin
             { case register is modified by the list evalution }
           if (p^.left^.location.loc=LOC_CREGISTER) then
             begin
                hr:=getregister32;
                case opsize of
                   S_B : hregister:=reg32toreg8(hr);
                   S_W : hregister:=reg32toreg16(hr);
                   S_L : hregister:=hr;
                end;
             end;
           last:=0;
           first:=true;
           genitem(hp);
           emitl(A_JMP,elselabel);
        end;

      procedure genjumptable(hp : pcaserecord;min_,max_ : longint);

        var
           table : plabel;
           last : longint;
           hr : preference;

        procedure genitem(t : pcaserecord);

          var
             i : longint;

          begin
             if assigned(t^.less) then
               genitem(t^.less);
             { fill possible hole }
             for i:=last+1 to t^._low-1 do
               jumpsegment^.concat(new(pai_const,init_symbol(strpnew(lab2str
                 (elselabel)))));
             for i:=t^._low to t^._high do
               jumpsegment^.concat(new(pai_const,init_symbol(strpnew(lab2str
                    (t^.statement)))));
              last:=t^._high;
             if assigned(t^.greater) then
               genitem(t^.greater);
            end;

          begin
           if not(jumptable_no_range) then
             begin
                exprasmlist^.concat(new(pai386,op_const_reg(A_CMP,opsize,min_,hregister)));
                { case expr less than min_ => goto elselabel }
                emitl(jmp_le,elselabel);
                exprasmlist^.concat(new(pai386,op_const_reg(A_CMP,opsize,max_,hregister)));
                emitl(jmp_gt,elselabel);
             end;
           getlabel(table);
           { extend with sign }
           if opsize=S_W then
             begin
                exprasmlist^.concat(new(pai386,op_reg_reg(A_MOVZX,S_WL,hregister,
                  reg16toreg32(hregister))));
                hregister:=reg16toreg32(hregister);
             end
           else if opsize=S_B then
             begin
                exprasmlist^.concat(new(pai386,op_reg_reg(A_MOVZX,S_BL,hregister,
                  reg8toreg32(hregister))));
                hregister:=reg8toreg32(hregister);
             end;
           new(hr);
           reset_reference(hr^);
           hr^.symbol:=stringdup(lab2str(table));
           hr^.offset:=(-min_)*4;
           hr^.index:=hregister;
           hr^.scalefactor:=4;
           exprasmlist^.concat(new(pai386,op_ref(A_JMP,S_NO,hr)));
           { !!!!! generate tables
             if not(cs_littlesize in aktswitches^ ) then
             jumpsegment^.concat(new(pai386,op_const(A_ALIGN,S_NO,4)));
           }
           jumpsegment^.concat(new(pai_label,init(table)));
             last:=min_;
           genitem(hp);
             { !!!!!!!
           if not(cs_littlesize in aktswitches^ ) then
             exprasmlist^.concat(new(pai386,op_const(A_ALIGN,S_NO,4)));
           }
        end;

      var
         lv,hv,min_label,max_label,labels : longint;
         max_linear_list : longint;

      begin
         getlabel(endlabel);
         getlabel(elselabel);
         if smartlink then
           jumpsegment:=procinfo.aktlocaldata
         else
           jumpsegment:=datasegment;
         with_sign:=is_signed(p^.left^.resulttype);
         if with_sign then
           begin
              jmp_gt:=A_JG;
              jmp_le:=A_JL;
              jmp_lee:=A_JLE;
           end
         else
            begin
              jmp_gt:=A_JA;
              jmp_le:=A_JB;
              jmp_lee:=A_JBE;
           end;
         cleartempgen;
         secondpass(p^.left);
         { determines the size of the operand }
         { determines the size of the operand }
         opsize:=bytes2Sxx[p^.left^.resulttype^.size];
         { copy the case expression to a register }
         { copy the case expression to a register }
         case p^.left^.location.loc of
            LOC_REGISTER,
            LOC_CREGISTER:
              hregister:=p^.left^.location.register;
            LOC_MEM,LOC_REFERENCE : begin
                                       del_reference(p^.left^.location.reference);
                                           hregister:=getregister32;
                                       case opsize of
                                          S_B : hregister:=reg32toreg8(hregister);
                                          S_W : hregister:=reg32toreg16(hregister);
                                       end;
                                       exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,opsize,newreference(
                                         p^.left^.location.reference),hregister)));
                                    end;
            else internalerror(2002);
         end;
         { now generate the jumps }
           if cs_optimize in aktswitches then
           begin
              { procedures are empirically passed on }
              { consumption can also be calculated   }
              { but does it pay on the different     }
              { processors?                          }
              { moreover can the size only be appro- }
              { ximated as it is not known if rel8,  }
              { rel16 or rel32 jumps are used        }
              min_label:=case_get_min(p^.nodes);
              max_label:=case_get_max(p^.nodes);
              labels:=case_count_labels(p^.nodes);
              { can we omit the range check of the jump table }
              getrange(p^.left^.resulttype,lv,hv);
              jumptable_no_range:=(lv=min_label) and (hv=max_label);

              { optimize for size ? }
              if cs_littlesize in aktswitches  then
                begin
                   if (labels<=2) or ((max_label-min_label)>3*labels) then
                  { a linear list is always smaller than a jump tree }
                     genlinearlist(p^.nodes)
                   else
                          { if the labels less or more a continuum then }
                          genjumptable(p^.nodes,min_label,max_label);
                end
              else
                begin
                   if jumptable_no_range then
                     max_linear_list:=4
                   else
                     max_linear_list:=2;
                   { a jump table crashes the pipeline! }
                   if opt_processors=i486 then
                     inc(max_linear_list,3);
                       if opt_processors=pentium then
                     inc(max_linear_list,6);
                   if opt_processors>=pentiumpro then
                     inc(max_linear_list,9);

                   if (labels<=max_linear_list) then
                     genlinearlist(p^.nodes)
                   else
                     begin
                        if ((max_label-min_label)>4*labels) then
                          begin
                             if labels>16 then
                               gentreejmp(p^.nodes)
                             else
                               genlinearlist(p^.nodes);
                          end
                        else
                          genjumptable(p^.nodes,min_label,max_label);
                     end;
                end;
             end
           else
           { it's always not bad }
           genlinearlist(p^.nodes);

         { now generate the instructions }
           hp:=p^.right;
         while assigned(hp) do
           begin
              cleartempgen;
              secondpass(hp^.right);
              emitl(A_JMP,endlabel);
              hp:=hp^.left;
           end;
         emitl(A_LABEL,elselabel);
         { ...and the else block }
         if assigned(p^.elseblock) then
             begin
              cleartempgen;
              secondpass(p^.elseblock);
           end;
         emitl(A_LABEL,endlabel);
      end;

    { generates the code for a raise statement }
    procedure secondraise(var p : ptree);

      var
         a : plabel;

      begin
         if assigned(p^.left) then
           begin
              { generate the address }
              if assigned(p^.right) then
                begin
                   secondpass(p^.right);
                       if codegenerror then
                          exit;
                end
              else
                        begin
                   getlabel(a);
                           emitl(A_LABEL,a);
                   exprasmlist^.concat(new(pai386,
                     op_csymbol(A_PUSH,S_L,newcsymbol(lab2str(a),0))));
                end;
              secondpass(p^.left);
              if codegenerror then
                exit;

              case p^.left^.location.loc of
                 LOC_MEM,LOC_REFERENCE : emitpushreferenceaddr(p^.left^.location.reference);
                 LOC_CREGISTER,LOC_REGISTER : exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,
                       p^.left^.location.register)));
                 else Message(sym_e_type_mismatch);
              end;
                 emitcall('DO_RAISE',true);
             end
           else
             emitcall('DO_RERAISE',true);
       end;

     procedure secondtryexcept(var p : ptree);

      begin
      end;

    procedure secondtryfinally(var p : ptree);

      begin
      end;

     procedure secondfail(var p : ptree);

      var hp : preference;

      begin
         {if procinfo.exceptions then
           aktproccode.concat(gennasmrec(CALL,S_NO,'HELP_DESTRUCTOR_E'))
         else }
         { we should know if the constructor is called with a new or not,
         how can we do that ???
         exprasmlist^.concat(new(pai386,op_csymbol(A_CALL,S_NO,newcsymbol('HELP_DESTRUCTOR',0))));
         }
         exprasmlist^.concat(new(pai386,op_reg_reg(A_XOR,S_L,R_ESI,R_ESI)));
         { also reset to zero in the stack }
         new(hp);
         reset_reference(hp^);
         hp^.offset:=procinfo.ESI_offset;
         hp^.base:=procinfo.framepointer;
         exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_L,R_ESI,hp)));
         exprasmlist^.concat(new(pai_labeled,init(A_JMP,quickexitlabel)));
      end;

     procedure secondwith(var p : ptree);

        var
            ref : treference;
          symtable : psymtable;
          i : longint;

      begin
         if assigned(p^.left) then
            begin
               secondpass(p^.left);
               ref.symbol:=nil;
               gettempofsizereference(4,ref);
               exprasmlist^.concat(new(pai386,op_ref_reg(A_LEA,S_L,
                 newreference(p^.left^.location.reference),R_EDI)));
               exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_L,
                 R_EDI,newreference(ref))));
               del_reference(p^.left^.location.reference);
               { the offset relative to (%ebp) is only needed here! }
               symtable:=p^.withsymtable;
               for i:=1 to p^.tablecount do
                 begin
                    symtable^.datasize:=ref.offset;
                    symtable:=symtable^.next;
                 end;

               { p^.right can be optimize out !!! }
               if p^.right<>nil then
                 secondpass(p^.right);
               { clear some stuff }
               ungetiftemp(ref);
            end;
       end;

    { implementation not complete yet }

    var addr_correction : longint;

    procedure correct_address(p : psym);

      begin
         if p^.typ=varsym then
           begin
             inc(pvarsym(p)^.address,addr_correction);
{$ifdef extdebug}
             Comment(V_debug,pvarsym(p)^.name+' is at offset -'
               +tostr(pvarsym(p)^.address));
             exprasmlist^.concat(new(pai_asm_comment,init(
               strpnew(pvarsym(p)^.name+' is at offset -'
               +tostr(pvarsym(p)^.address)))));
{$endif extdebug}
           end;
      end;

    procedure secondprocinline(var p : ptree);

       var st : psymtable;
           oldprocsym : pprocsym;
           para_size : longint;
           oldprocinfo : tprocinfo;
           { just dummies for genentrycode }
           nostackframe,make_global : boolean;
           proc_names : tstringcontainer;
           inlineentrycode,inlineexitcode : paasmoutput;
           oldexitlabel,oldexit2label,oldquickexitlabel:Plabel;
       begin
          oldexitlabel:=aktexitlabel;
          oldexit2label:=aktexit2label;
          oldquickexitlabel:=quickexitlabel;
          getlabel(aktexitlabel);
          getlabel(aktexit2label);
          oldprocsym:=aktprocsym;
          oldprocinfo:=procinfo;
          { set the return value }
          procinfo.retdef:=p^.inlineprocdef^.retdef;
          procinfo.retoffset:=p^.retoffset;
          { arg space has been filled by the parent secondcall }
          st:=p^.inlineprocdef^.localst;
              { set it to the same lexical level }
          st^.symtablelevel:=
            oldprocsym^.definition^.localst^.symtablelevel;
          if st^.datasize>0 then
            st^.call_offset:=gettempofsizepersistant(st^.datasize);
{$ifdef extdebug}
             Comment(V_debug,'local symtable is at offset '
               +tostr(st^.call_offset));
          exprasmlist^.concat(new(pai_asm_comment,init(
          strpnew('local symtable is at offset '
               +tostr(st^.call_offset)))));
{$endif extdebug}
          addr_correction:=-st^.call_offset-st^.datasize;
          st^.foreach(correct_address);
{$ifdef extdebug}
          exprasmlist^.concat(new(pai_asm_comment,init('Start of inlined proc')));
{$endif extdebug}
          { takes care of local data initialization }
          inlineentrycode:=new(paasmoutput,init);
          inlineexitcode:=new(paasmoutput,init);
          proc_names.init;
          para_size:=p^.para_size;
          genentrycode(inlineentrycode,proc_names,make_global,
           0,para_size,nostackframe,true);
          exprasmlist^.concatlist(inlineentrycode);
          secondpass(p^.left);
          genexitcode(inlineexitcode,0,false,true);
          exprasmlist^.concatlist(inlineexitcode);
{$ifdef extdebug}
          exprasmlist^.concat(new(pai_asm_comment,init('End of inlined proc')));
{$endif extdebug}
          {we can free the local data now }
          if st^.datasize>0 then
            ungetpersistanttemp(st^.call_offset,st^.datasize);
          { set the real address again }
          addr_correction:=-addr_correction;
          st^.foreach(correct_address);
          aktprocsym:=oldprocsym;
          aktexitlabel:=oldexitlabel;
          aktexit2label:=oldexit2label;
          quickexitlabel:=oldquickexitlabel;
          procinfo:=oldprocinfo;
       end;


     procedure secondpass(var p : ptree);

       const
           procedures : array[ttreetyp] of secondpassproc =
               (secondadd,secondadd,secondadd,secondmoddiv,secondadd,
                secondmoddiv,secondassignment,secondload,secondnothing,
                secondadd,secondadd,secondadd,secondadd,
                secondadd,secondadd,secondin,secondadd,
                secondadd,secondshlshr,secondshlshr,secondadd,
               secondadd,secondsubscriptn,secondderef,secondaddr,
             seconddoubleaddr,
             secondordconst,secondtypeconv,secondcalln,secondnothing,
             secondrealconst,secondfixconst,secondumminus,
             secondasm,secondvecn,
             secondstringconst,secondfuncret,secondselfn,
             secondnot,secondinline,secondniln,seconderror,
             secondnothing,secondhnewn,secondhdisposen,secondnewn,
             secondsimplenewdispose,secondnothing,secondsetcons,secondblockn,
             secondstatement,secondnothing,secondifn,secondbreakn,
             secondcontinuen,second_while_repeatn,second_while_repeatn,secondfor,
             secondexitn,secondwith,secondcase,secondlabel,
             secondgoto,secondsimplenewdispose,secondtryexcept,secondraise,
             secondnothing,secondtryfinally,secondis,secondas,seconderror,
             secondfail,secondadd,secondprocinline,
             secondnothing,secondloadvmt);
      var
         oldcodegenerror : boolean;
         oldswitches : Tcswitches;
         oldis : pinputfile;
         oldnr : longint;

      begin
         oldcodegenerror:=codegenerror;
         oldswitches:=aktswitches;
         oldis:=current_module^.current_inputfile;
         oldnr:=current_module^.current_inputfile^.line_no;

         codegenerror:=false;
         current_module^.current_inputfile:=
           pinputfile(current_module^.sourcefiles.get_file(p^.fileinfo.fileindex));
         current_module^.current_inputfile^.line_no:=p^.fileinfo.line;
         aktswitches:=p^.pragmas;
         if not(p^.error) then
           begin
              procedures[p^.treetype](p);
              p^.error:=codegenerror;
              codegenerror:=codegenerror or oldcodegenerror;
           end
         else
           codegenerror:=true;
         aktswitches:=oldswitches;
         current_module^.current_inputfile:=oldis;
         current_module^.current_inputfile^.line_no:=oldnr;
      end;

    function do_secondpass(var p : ptree) : boolean;

      begin
         codegenerror:=false;
         if not(p^.error) then
           secondpass(p);
         do_secondpass:=codegenerror;
      end;

    var
       regvars : array[1..maxvarregs] of pvarsym;
       regvars_para : array[1..maxvarregs] of boolean;
       regvars_refs : array[1..maxvarregs] of longint;
       parasym : boolean;

    procedure searchregvars(p : psym);

      var
         i,j,k : longint;

      begin
         if (p^.typ=varsym) and (pvarsym(p)^.regable) then
           begin
              { walk through all momentary register variables }
              for i:=1 to maxvarregs do
                begin
                   { free register ? }
                   if regvars[i]=nil then
                     begin
                        regvars[i]:=pvarsym(p);
                        regvars_para[i]:=parasym;
                        break;
                     end;
                   { else throw out a variable ? }
                       j:=pvarsym(p)^.refs;
                   { parameter get a less value }
                   if parasym then
                     begin
                        if cs_littlesize in aktswitches  then
                          dec(j,1)
                        else
                          dec(j,100);
                     end;
                   if (j>regvars_refs[i]) and (j>0) then
                     begin
                        for k:=maxvarregs-1 downto i do
                          begin
                             regvars[k+1]:=regvars[k];
                             regvars_para[k+1]:=regvars_para[k];
                          end;
                        { calc the new refs
                        pvarsym(p)^.refs:=j; }
                        regvars[i]:=pvarsym(p);
                        regvars_para[i]:=parasym;
                        regvars_refs[i]:=j;
                        break;
                     end;
                end;
           end;
      end;

    procedure generatecode(var p : ptree);

      var
           { *pass modifies with every node aktlinenr and current_module^.current_inputfile, }
         { to constantly contain the right line numbers             }
           oldis : pinputfile;
         oldnr,i : longint;
         regsize : topsize;
         regi : tregister;
          hr : preference;

       label
         nextreg;

      begin
         cleartempgen;
         oldis:=current_module^.current_inputfile;
         oldnr:=current_module^.current_inputfile^.line_no;
         { when size optimization only count occurrence }
         if cs_littlesize in aktswitches then
           t_times:=1
         else
           { reference for repetition is 100 }
           t_times:=100;
         { clear register count }
{$ifdef SUPPORT_MMX}
         for regi:=R_EAX to R_MM6 do
           begin
              reg_pushes[regi]:=0;
              is_reg_var[regi]:=false;
           end;
{$else SUPPORT_MMX}
         for regi:=R_EAX to R_EDI do
           begin
              reg_pushes[regi]:=0;
              is_reg_var[regi]:=false;
           end;
{$endif SUPPORT_MMX}
         use_esp_stackframe:=false;

         if not(do_firstpass(p)) then
           begin
              { max. optimizations     }
              { only if no asm is used }
              if (cs_maxoptimieren in aktswitches) and
                ((procinfo.flags and pi_uses_asm)=0) then
                begin
                   { can we omit the stack frame ? }
                   { conditions:
                     1. procedure (not main block)
                     2. no constructor or destructor
                     3. no call to other procedures
                     4. no interrupt handler
                   }
                   if assigned(aktprocsym) then
                     begin
                       if (aktprocsym^.definition^.options and
                        poconstructor+podestructor{+poinline}+pointerrupt=0) and
                        ((procinfo.flags and pi_do_call)=0) and (lexlevel>1) then
                       begin
                         { use ESP as frame pointer }
                         procinfo.framepointer:=R_ESP;
                         use_esp_stackframe:=true;

                         { calc parameter distance new }
                         dec(procinfo.framepointer_offset,4);
                         dec(procinfo.ESI_offset,4);

                         { is this correct ???}
                         { retoffset can be negativ for results in eax !! }
                         { the value should be decreased only if positive }
                         if procinfo.retoffset>=0 then
                           dec(procinfo.retoffset,4);

                         dec(procinfo.call_offset,4);
                         aktprocsym^.definition^.parast^.call_offset:=procinfo.call_offset;
                       end;
                     end;
                   if (p^.registers32<4) then
                       begin
                        for i:=1 to maxvarregs do
                          regvars[i]:=nil;
                        parasym:=false;
{$ifdef tp}
                        symtablestack^.foreach(searchregvars);
{$else}
                        symtablestack^.foreach(@searchregvars);
{$endif}
                        { copy parameter into a register ? }
                        parasym:=true;
{$ifdef tp}
                        symtablestack^.next^.foreach(searchregvars);
{$else}
                        symtablestack^.next^.foreach(@searchregvars);
{$endif}

                        { hold needed registers free }
                        for i:=maxvarregs downto maxvarregs-p^.registers32+1 do
                          regvars[i]:=nil;
                        { now assign register }
                        for i:=1 to maxvarregs-p^.registers32 do
                          begin
                             if assigned(regvars[i]) then
                               begin
                                  { it is nonsens, to copy the variable to }
                                  { a register because we need then much   }
                                  { pushes ?                               }
                                  if reg_pushes[varregs[i]]>=regvars[i]^.refs then
                                    begin
                                       regvars[i]:=nil;
                                       goto nextreg;
                                    end;

                                  { register is no longer available for }
                                  { expressions                         }
                                  { search the register which is the most }
                                  { unused                                }
                                  usableregs:=usableregs-[varregs[i]];
                                  is_reg_var[varregs[i]]:=true;
                                  dec(c_usableregs);

                                  { possibly no 32 bit register are needed }
                                  if  (regvars[i]^.definition^.deftype=orddef) and
                                      (
                                       (porddef(regvars[i]^.definition)^.typ=bool8bit) or
                                       (porddef(regvars[i]^.definition)^.typ=uchar) or
                                       (porddef(regvars[i]^.definition)^.typ=u8bit) or
                                       (porddef(regvars[i]^.definition)^.typ=s8bit)
                                      ) then
                                    begin
                                       regvars[i]^.reg:=reg32toreg8(varregs[i]);
                                       regsize:=S_B;
                                    end
                                  else if  (regvars[i]^.definition^.deftype=orddef) and
                                      (
                                       (porddef(regvars[i]^.definition)^.typ=u16bit) or
                                       (porddef(regvars[i]^.definition)^.typ=s16bit)
                                      ) then
                                    begin
                                       regvars[i]^.reg:=reg32toreg16(varregs[i]);
                                       regsize:=S_W;
                                    end
                                  else
                                    begin
                                       regvars[i]^.reg:=varregs[i];
                                       regsize:=S_L;
                                    end;
                                  { parameter must be load }
                                  if regvars_para[i] then
                                    begin
                                       { procinfo is there actual,      }
                                       { because we can't never be in a }
                                       { nested procedure               }
                                       { when loading parameter to reg  }
                                       new(hr);
                                       reset_reference(hr^);
                                       hr^.offset:=pvarsym(regvars[i])^.address+procinfo.call_offset;
                                       hr^.base:=procinfo.framepointer;
                                       procinfo.aktentrycode^.concat(new(pai386,op_ref_reg(A_MOV,regsize,
                                         hr,regvars[i]^.reg)));
                                       unused:=unused - [regvars[i]^.reg];
                                    end;
                                  { procedure uses this register }
                                  usedinproc:=usedinproc or ($80 shr byte(varregs[i]));
                               end;
                             nextreg:
                               { dummy }
                               regsize:=S_W;
                          end;
                        if (verbosity and v_debug)=v_debug then
                          begin
                             for i:=1 to maxvarregs do
                               begin
                                  if assigned(regvars[i]) then
                                   Message3(cg_d_register_weight,reg2str(regvars[i]^.reg),
                                           tostr(regvars[i]^.refs),regvars[i]^.name);
                               end;
                          end;
                     end;
                end;
              do_secondpass(p);

{$ifdef StoreFPULevel}
              if assigned(aktprocsym) then
                aktprocsym^.fpu_used:=p^.registersfpu;
{$endif StoreFPULevel}
              { all registers can be used again }
              usableregs:=[R_EAX,R_EBX,R_ECX,R_EDX];
{$ifdef SUPPORT_MMX}
              usableregs:=usableregs+[R_MM0..R_MM6];
{$endif SUPPORT_MMX}
              c_usableregs:=4;
           end;
         procinfo.aktproccode^.concatlist(exprasmlist);

         current_module^.current_inputfile:=oldis;
         current_module^.current_inputfile^.line_no:=oldnr;
      end;

end.
{
  $Log$
  Revision 1.24  1998-05-20 09:42:33  pierre
    + UseTokenInfo now default
    * unit in interface uses and implementation uses gives error now
    * only one error for unknown symbol (uses lastsymknown boolean)
      the problem came from the label code !
    + first inlined procedures and function work
      (warning there might be allowed cases were the result is still wrong !!)
    * UseBrower updated gives a global list of all position of all used symbols
      with switch -gb

  Revision 1.23  1998/05/12 10:46:58  peter
    * moved printstatus to verb_def
    + V_Normal which is between V_Error and V_Warning and doesn't have a
      prefix like error: warning: and is included in V_Default
    * fixed some messages
    * first time parameter scan is only for -v and -T
    - removed old style messages

  Revision 1.22  1998/05/07 00:17:00  peter
    * smartlinking for sets
    + consts labels are now concated/generated in hcodegen
    * moved some cpu code to cga and some none cpu depended code from cga
      to tree and hcodegen and cleanup of hcodegen
    * assembling .. output reduced for smartlinking ;)

  Revision 1.21  1998/05/06 08:38:36  pierre
    * better position info with UseTokenInfo
      UseTokenInfo greatly simplified
    + added check for changed tree after first time firstpass
      (if we could remove all the cases were it happen
      we could skip all firstpass if firstpasscount > 1)
      Only with ExtDebug

  Revision 1.20  1998/05/01 16:38:44  florian
    * handling of private and protected fixed
    + change_keywords_to_tp implemented to remove
      keywords which aren't supported by tp
    * break and continue are now symbols of the system unit
    + widestring, longstring and ansistring type released

  Revision 1.19  1998/04/30 15:59:39  pierre
    * GDB works again better :
      correct type info in one pass
    + UseTokenInfo for better source position
    * fixed one remaining bug in scanner for line counts
    * several little fixes

  Revision 1.18  1998/04/29 10:33:48  pierre
    + added some code for ansistring (not complete nor working yet)
    * corrected operator overloading
    * corrected nasm output
    + started inline procedures
    + added starstarn : use ** for exponentiation (^ gave problems)
    + started UseTokenInfo cond to get accurate positions

  Revision 1.17  1998/04/27 23:10:27  peter
    + new scanner
    * $makelib -> if smartlink
    * small filename fixes pmodule.setfilename
    * moved import from files.pas -> import.pas

  Revision 1.16  1998/04/23 21:52:08  florian
    * fixes of Jonas applied

  Revision 1.15  1998/04/22 21:06:49  florian
    * last fixes before the release:
      - veryyyy slow firstcall fixed

  Revision 1.14  1998/04/21 10:16:47  peter
    * patches from strasbourg
    * objects is not used anymore in the fpc compiled version

  Revision 1.13  1998/04/14 23:27:02  florian
    + exclude/include with constant second parameter added

  Revision 1.12  1998/04/13 21:15:41  florian
    * error handling of pass_1 and cgi386 fixed
    * the following bugs fixed: 0117, 0118, 0119 and 0129, 0122 was already
      fixed, verified

  Revision 1.11  1998/04/13 08:42:51  florian
    * call by reference and call by value open arrays fixed

  Revision 1.10  1998/04/12 22:39:43  florian
    * problem with read access to properties solved
    * correct handling of hidding methods via virtual (COM)
    * correct result type of constructor calls (COM), the resulttype
      depends now on the type of the class reference

  Revision 1.9  1998/04/10 21:36:55  florian
    + some stuff to support method pointers (procedure of object) added
      (declaration, parameter handling)

  Revision 1.8  1998/04/09 22:16:33  florian
    * problem with previous REGALLOC solved
    * improved property support

  Revision 1.7  1998/04/09 14:28:05  jonas
    + basic k6 and 6x86 optimizing support (-O7 and -O8)

  Revision 1.6  1998/04/08 11:34:20  peter
    * nasm works (linux only tested)

  Revision 1.5  1998/04/07 22:45:04  florian
    * bug0092, bug0115 and bug0121 fixed
    + packed object/class/array

  Revision 1.4  1998/04/07 13:19:42  pierre
    * bugfixes for reset_gdb_info
      in MEM parsing for go32v2
      better external symbol creation
      support for rhgdb.exe (lowercase file names)
}
