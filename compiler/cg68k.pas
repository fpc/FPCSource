{
    $Id$
    Copyright (c) 1993,98 by Florian Klaempfl, Carl Eric Codere

    This unit generates 68000 (or better) assembler from the parse tree

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
{$ifdef tp}
{$E+,F+,N+,D+,L+,Y+}
{$endif}

{---------------------------------------------------------------------------}
{ LEFT TO DO IN CG68k AND CG68k2                                            }
{---------------------------------------------------------------------------}
{  o Test and correct problems with extended support.                       }
{  o Optimize secondmoddiv when doing a constant modulo.                    }
{  o Add emulation support for Cardinal under MC68000.                      }
{---------------------------------------------------------------------------}

unit cg68k;


{***************************************************************************}
interface
{***************************************************************************}

uses    objects,verbose,cobjects,systems,globals,tree,
        symtable,types,strings,pass_1,hcodegen,temp_gen,
        aasm,m68k,tgen68k,files,cga68k,cg68k2,link
{$ifdef GDB}
        ,gdb
{$endif}
        ;       
{ produces assembler for the expression in variable p }
{ and produces an assembler node at the end           }
procedure generatecode(var p : ptree);


{ produces the actual code }
function do_secondpass(var p : ptree) : boolean;

procedure secondpass(var p : ptree);

{$ifdef test_dest_loc}
const   { used to avoid temporary assignments }
        dest_loc_known : boolean = false;
        in_dest_loc : boolean = false;
        dest_loc_tree : ptree = nil;

var dest_loc : tlocation;

procedure mov_reg_to_dest(p : ptree; s : topsize; reg : tregister);

{$endif test_dest_loc}



{***************************************************************************}
implementation
{***************************************************************************}

  uses
    scanner;
    
    const
       never_copy_const_param : boolean = false;
       bytes2Sxx:array[1..4] of Topsize=(S_B,S_W,S_NO,S_L);
        { used to avoid temporary assignments }
        dest_loc_known : boolean = false;
        in_dest_loc : boolean = false;
        dest_loc_tree : ptree = nil;



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
       dest_loc : tlocation;

        procedure mov_reg_to_dest(p : ptree; s : topsize; reg : tregister);

          begin
             if (dest_loc.loc=LOC_CREGISTER) or (dest_loc.loc=LOC_REGISTER) then
               begin
                 emit_reg_reg(A_MOVE,s,reg,dest_loc.register);
                 p^.location:=dest_loc;
                 in_dest_loc:=true;
               end
             else
             if (dest_loc.loc=LOC_REFERENCE) or (dest_loc.loc=LOC_MEM) then
               begin
                 exprasmlist^.concat(new(pai68k,op_reg_ref(A_MOVE,s,reg,newreference(dest_loc.reference))));
                 p^.location:=dest_loc;
                 in_dest_loc:=true;
               end
             else
               internalerror(20080);
          end;




    procedure error(const t : tmsgconst);

      begin
         if not(codegenerror) then
           verbose.Message(t);
         codegenerror:=true;
      end;

    type
       secondpassproc = procedure(var p : ptree);

    procedure seconderror(var p : ptree);

      begin
         p^.error:=true;
         codegenerror:=true;
      end;

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
                end;
              hp:=hp^.left;
           end;
      end;

    procedure secondload(var p : ptree);

      var
         hregister : tregister;
         i : longint;
         symtabletype: tsymtabletype;
         hp : preference;

      begin
         simple_loadn:=true;
         reset_reference(p^.location.reference);
         case p^.symtableentry^.typ of
              { this is only for toasm and toaddr }
              absolutesym :
                 begin
                    stringdispose(p^.location.reference.symbol);
                    p^.location.reference.symbol:=stringdup(p^.symtableentry^.mangledname);
                    if p^.symtableentry^.owner^.symtabletype=unitsymtable then
                      concat_external(p^.symtableentry^.mangledname,EXT_NEAR);
                 end;
              varsym :
                 begin
                    hregister:=R_NO;
                    symtabletype:=p^.symtable^.symtabletype;
                    { in case it is a register variable: }
                    { we simply set the location to the  }
                    { correct register.                  }
                    if pvarsym(p^.symtableentry)^.reg<>R_NO then
                      begin
                         p^.location.loc:=LOC_CREGISTER;
                         p^.location.register:=pvarsym(p^.symtableentry)^.reg;
                         unused:=unused-[pvarsym(p^.symtableentry)^.reg];
                      end
                    else
                      begin
                         { --------------------- LOCAL AND TEMP VARIABLES ------------- }
                         if (symtabletype=parasymtable) or (symtabletype=localsymtable) then
                           begin

                              p^.location.reference.base:=procinfo.framepointer;
                              p^.location.reference.offset:=pvarsym(p^.symtableentry)^.address;

                              if (symtabletype=localsymtable) then
                                p^.location.reference.offset:=-p^.location.reference.offset;

                              if (symtabletype=parasymtable) then
                                inc(p^.location.reference.offset,p^.symtable^.call_offset);

                              if (lexlevel>(p^.symtable^.symtablelevel)) then
                                begin
                                   hregister:=getaddressreg;

                                   { make a reference }
                                   new(hp);
                                   reset_reference(hp^);
                                   hp^.offset:=procinfo.framepointer_offset;
                                   hp^.base:=procinfo.framepointer;

                                   exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,S_L,hp,hregister)));

                                   simple_loadn:=false;
                                   i:=lexlevel-1;
                                   while i>(p^.symtable^.symtablelevel) do
                                     begin
                                        { make a reference }
                                        new(hp);
                                        reset_reference(hp^);
                                        hp^.offset:=8;
                                        hp^.base:=hregister;

                                        exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,S_L,hp,hregister)));
                                        dec(i);
                                     end;
                                   p^.location.reference.base:=hregister;
                                end;
                           end
                         { --------------------- END OF LOCAL AND TEMP VARS ---------------- }
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
                                                  p^.location.reference.base:=R_A5;
                                                  p^.location.reference.offset:=pvarsym(p^.symtableentry)^.address;
                                               end;
                                               end;
                              withsymtable :   begin
                                                  hregister:=getaddressreg;
                                                  p^.location.reference.base:=hregister;
                                                  { make a reference }
                                                  new(hp);
                                                  reset_reference(hp^);
                                                  hp^.offset:=p^.symtable^.datasize;
                                                  hp^.base:=procinfo.framepointer;

                                                  exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,S_L,hp,hregister)));

                                                  p^.location.reference.offset:=
                                                    pvarsym(p^.symtableentry)^.address;
                                               end;
                           end;

                         { in case call by reference, then calculate: }
                         if (pvarsym(p^.symtableentry)^.varspez=vs_var) or
                            ((pvarsym(p^.symtableentry)^.varspez=vs_const) and
                             dont_copy_const_param(pvarsym(p^.symtableentry)^.definition)) then
                           begin
                              simple_loadn:=false;
                              if hregister=R_NO then
                                hregister:=getaddressreg;
                              { ADDED FOR OPEN ARRAY SUPPORT. }
                              if (p^.location.reference.base=procinfo.framepointer) then
                                begin
                                   highframepointer:=p^.location.reference.base;
                                   highoffset:=p^.location.reference.offset;
                                end
                              else
                                begin
                                   highframepointer:=R_A1;
                                   highoffset:=p^.location.reference.offset;
                                   exprasmlist^.concat(new(pai68k,op_reg_reg(A_MOVE,S_L,
                                     p^.location.reference.base,R_A1)));
                                end;
                              exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,S_L,newreference(p^.location.reference),
                                hregister)));
                              { END ADDITION }
                              clear_reference(p^.location.reference);
                              p^.location.reference.base:=hregister;
                          end;
                         { should be dereferenced later (FK)
                         if (pvarsym(p^.symtableentry)^.definition^.deftype=objectdef) and
                           ((pobjectdef(pvarsym(p^.symtableentry)^.definition)^.options and oois_class)<>0) then
                           begin
                              simple_loadn:=false;
                              if hregister=R_NO then
                                hregister:=getaddressreg;
                              exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,S_L,newreference(p^.location.reference),
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
                    if p^.symtable^.symtabletype=unitsymtable then
                    concat_external(p^.symtableentry^.mangledname,EXT_NEAR);
                 end;
              typedconstsym :
                 begin
                    stringdispose(p^.location.reference.symbol);
                    p^.location.reference.symbol:=stringdup(p^.symtableentry^.mangledname);
                    if p^.symtable^.symtabletype=unitsymtable then
                    concat_external(p^.symtableentry^.mangledname,EXT_NEAR);
                 end;
              else internalerror(4);
         end;
      end;


    { D0 and D1 used as temp (ok)   }
    procedure secondmoddiv(var p : ptree);

      var
         hreg1 : tregister;
         power : longint;
         hl : plabel;
         reg: tregister;
         pushed: boolean;
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
                  emit_reg_reg(A_MOVE,S_L,p^.left^.location.register,hreg1);
                end
              else
                begin
                  del_reference(p^.left^.location.reference);
                  hreg1:=getregister32;
                  exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,S_L,newreference(p^.left^.location.reference),
                    hreg1)));
                end;
              p^.left^.location.loc:=LOC_REGISTER;
              p^.left^.location.register:=hreg1;
           end
         else hreg1:=p^.left^.location.register;

         if (p^.treetype=divn) and (p^.right^.treetype=ordconstn) and
            ispowerof2(p^.right^.value,power) then
           begin
              exprasmlist^.concat(new(pai68k, op_reg(A_TST, S_L, hreg1)));
              getlabel(hl);
              emitl(A_BPL,hl);
              if (power = 1) then
                 exprasmlist^.concat(new(pai68k, op_const_reg(A_ADDQ, S_L,1, hreg1)));
              if (p^.right^.value-1) < 9 then
                 exprasmlist^.concat(new(pai68k, op_const_reg(A_ADDQ, S_L,p^.right^.value-1, hreg1)))
              else
                 exprasmlist^.concat(new(pai68k, op_const_reg(A_ADD, S_L,p^.right^.value-1, hreg1)));
              emitl(A_LABEL, hl);
              if (power > 0) and (power < 9) then
                 exprasmlist^.concat(new(pai68k, op_const_reg(A_ASR, S_L,power, hreg1)))
              else
               begin
                  exprasmlist^.concat(new(pai68k, op_const_reg(A_MOVE,S_L,power, R_D0)));
                  exprasmlist^.concat(new(pai68k, op_reg_reg(A_ASR,S_L,R_D0, hreg1)));
               end;
           end
         else
           begin
              { bring denominator to D1 }
              { D1 is always free, it's }
              { only used for temporary  }
              { purposes                 }
              if (p^.right^.location.loc<>LOC_REGISTER) and
                 (p^.right^.location.loc<>LOC_CREGISTER) then
                 begin
                   del_reference(p^.right^.location.reference);
                   p^.left^.location.loc:=LOC_REGISTER;
                   exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,S_L,newreference(p^.right^.location.reference),R_D1)));
                end
             else
              begin
                   ungetregister32(p^.right^.location.register);
                   emit_reg_reg(A_MOVE,S_L,p^.right^.location.register,R_D1);
              end;

              { on entering this section D1 should contain the divisor }

              if (aktoptprocessor
                = MC68020) then
              begin
                 if (p^.treetype = modn) then
                 Begin
                   reg := getregister32;
                   exprasmlist^.concat(new(pai68k,op_reg(A_CLR,S_L,reg)));
                   getlabel(hl);
                   { here what we do is prepare the high register with the     }
                   { correct sign. i.e we clear it, check if the low dword reg }
                   { which will participate in the division is signed, if so we}
                   { we extend the sign to the high doword register by inverting }
                   { all the bits.                                             }
                   exprasmlist^.concat(new(pai68k,op_reg(A_TST,S_L,hreg1)));
                   emitl(A_BPL,hl);
                   exprasmlist^.concat(new(pai68k,op_reg(A_NOT,S_L,reg)));
                   emitl(A_LABEL,hl);
                   { reg:hreg1 / d1 }
                   exprasmlist^.concat(new(pai68k,op_reg_reg_reg(A_DIVSL,S_L,R_D1,reg,hreg1)));
                   { hreg1 already contains quotient }
                   { looking for remainder }
                   exprasmlist^.concat(new(pai68k,op_reg_reg(A_MOVE,S_L,reg,hreg1)));
                   ungetregister32(reg);
                 end
                 else
                 { simple division... }
                 Begin
                   { reg:hreg1 / d1 }
                   exprasmlist^.concat(new(pai68k,op_reg_reg(A_DIVS,S_L,R_D1,hreg1)));
                 end;
              end
              else { MC68000 operations }
                 begin
                     { put numerator in d0 }
                     emit_reg_reg(A_MOVE,S_L,hreg1,R_D0);
                     { operation to perform on entry to both }
                     { routines...  d0/d1                    }
                     { return result in d0                   }
                     if p^.treetype = divn then
                       emitcall('LONGDIV',true)
                     else
                       emitcall('LONGMOD',true);
                     emit_reg_reg(A_MOVE,S_L,R_D0,hreg1);
              end; { endif }
         end;
         { this registers are always used when div/mod are present }
         usedinproc:=usedinproc or ($800 shr word(R_D1));
         usedinproc:=usedinproc or ($800 shr word(R_D0));
         p^.location.loc:=LOC_REGISTER;
         p^.location.register:=hreg1;
      end;


    { D6 used as scratch (ok) }
    procedure secondshlshr(var p : ptree);

      var
         hregister1,hregister2,hregister3 : tregister;
         op : tasmop;
         pushed : boolean;
      begin

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
                   emit_reg_reg(A_MOVE,S_L,p^.left^.location.register,
                     hregister1);
                end
              else
                begin
                   del_reference(p^.left^.location.reference);
                   hregister1:=getregister32;
                   exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,S_L,newreference(p^.left^.location.reference),
                     hregister1)));
                end;
           end
         else hregister1:=p^.left^.location.register;

         { determine operator }
         if p^.treetype=shln then
           op:=A_LSL
         else
           op:=A_LSR;

         { shifting by a constant directly decode: }
         if (p^.right^.treetype=ordconstn) then
           begin
             if (p^.right^.location.reference.offset and 31 > 0) and (p^.right^.location.reference.offset and 31 < 9) then
                 exprasmlist^.concat(new(pai68k,op_const_reg(op,S_L,p^.right^.location.reference.offset and 31,
                   hregister1)))
             else
               begin
                 exprasmlist^.concat(new(pai68k,op_const_reg(A_MOVE,S_L,p^.right^.location.reference.offset and 31,
                   R_D6)));
                 exprasmlist^.concat(new(pai68k,op_reg_reg(op,S_L,R_D6,hregister1)));
               end;
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
                        emit_reg_reg(A_MOVE,S_L,p^.right^.location.register,
                          hregister2);
                     end
                   else
                     begin
                        del_reference(p^.right^.location.reference);
                        hregister2:=getregister32;
                        exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,S_L,newreference(p^.right^.location.reference),
                          hregister2)));
                     end;
                end
              else hregister2:=p^.right^.location.register;


              emit_reg_reg(op,S_L,hregister2,hregister1);
              p^.location.register:=hregister1;
           end;
         { this register is always used when shl/shr are present }
         usedinproc:=usedinproc or ($800 shr byte(R_D6));
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
                             { Florian this caused a internalerror(10)=> no free reg !! }
                             {if ((p^.realtyp=ait_real_64bit) and (pai_double(hp1)^.value=p^.valued)) or
                               ((p^.realtyp=ait_real_80bit) and (pai_extended(hp1)^.value=p^.valued)) or
                               ((p^.realtyp=ait_real_32bit) and (pai_single(hp1)^.value=p^.valued)) then }
                             if ((p^.realtyp=ait_real_64bit) and (pai_double(hp1)^.value=p^.valued)) then
                               found:=true;
                             if ((p^.realtyp=ait_real_32bit) and (pai_single(hp1)^.value=p^.valued)) then
                               found:=true;
                             if ((p^.realtyp=ait_real_extended) and (pai_extended(hp1)^.value=p^.valued)) then
                               found:=true;
                             if found then
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
                   case p^.realtyp of
                     ait_real_64bit : consts^.insert(new(pai_double,init(p^.valued)));
                     ait_real_32bit : consts^.insert(new(pai_single,init(p^.valued)));
                     ait_real_extended : consts^.insert(new(pai_extended,init(p^.valued)));
                     else
                       internalerror(10120);
                     end;
                   consts^.insert(new(pai_label,init(lastlabel)));
                end;
           end;
         stringdispose(p^.location.reference.symbol);
         p^.location.reference.symbol:=stringdup(lab2str(lastlabel));
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
                             for i:=1 to length(p^.values^) do
                               if pai_string(hp1)^.str[i]<>p^.values^[i] then
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
                   getmem(pc,length(p^.values^)+3);
                   move(p^.values^,pc^,length(p^.values^)+1);
                   pc[length(p^.values^)+1]:=#0;
                   { we still will have a problem if there is a #0 inside the pchar }
                   consts^.insert(new(pai_string,init_pchar(pc)));
                   { to overcome this problem we set the length explicitly }
                   { with the ending null char }
                   pai_string(consts^.first)^.len:=length(p^.values^)+2;
                   consts^.insert(new(pai_label,init(lastlabel)));
                end;
           end;
         stringdispose(p^.location.reference.symbol);
         p^.location.reference.symbol:=stringdup(lab2str(lastlabel));
         p^.location.loc := LOC_MEM;
      end;

    procedure secondumminus(var p : ptree);

      begin
         secondpass(p^.left);
         p^.location.loc:=LOC_REGISTER;
         case p^.left^.location.loc of
            LOC_REGISTER : begin
                              p^.location.register:=p^.left^.location.register;
                              exprasmlist^.concat(new(pai68k,op_reg(A_NEG,S_L,p^.location.register)));
                           end;
            LOC_CREGISTER : begin
                               p^.location.register:=getregister32;
                               emit_reg_reg(A_MOVE,S_L,p^.location.register,
                                 p^.location.register);
                               exprasmlist^.concat(new(pai68k,op_reg(A_NEG,S_L,p^.location.register)));
                            end;
            LOC_REFERENCE,LOC_MEM :
                           begin
                              del_reference(p^.left^.location.reference);
                              { change sign of a floating point  }
                              { in the case of emulation, get    }
                              { a free register, and change sign }
                              { manually.                        }
                              { otherwise simply load into an FPU}
                              { register.                        }
                              if (p^.left^.resulttype^.deftype=floatdef) and
                                 (pfloatdef(p^.left^.resulttype)^.typ<>f32bit) then
                                begin
                                   { move to FPU }
                                   floatload(pfloatdef(p^.left^.resulttype)^.typ,
                                     p^.left^.location.reference,p^.location);
                                   if (cs_fp_emulation) in aktswitches then
                                       { if in emulation mode change sign manually }
                                       exprasmlist^.concat(new(pai68k,op_const_reg(A_BCHG,S_L,31,
                                          p^.location.fpureg)))
                                   else
                                       exprasmlist^.concat(new(pai68k,op_reg(A_FNEG,S_FX,
                                          p^.location.fpureg)));
                                end
                              else
                                begin
                                   p^.location.register:=getregister32;
                                   exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,S_L,
                                     newreference(p^.left^.location.reference),
                                     p^.location.register)));
                                   exprasmlist^.concat(new(pai68k,op_reg(A_NEG,S_L,p^.location.register)));
                                end;
                           end;
            LOC_FPU : begin
                              p^.location.loc:=LOC_FPU;
                              p^.location.fpureg := p^.left^.location.fpureg;
                              if (cs_fp_emulation) in aktswitches then
                                  exprasmlist^.concat(new(pai68k,op_const_reg(A_BCHG,S_L,31,p^.location.fpureg)))
                              else
                                 exprasmlist^.concat(new(pai68k,op_reg(A_FNEG,S_FX,p^.location.fpureg)));
                           end;
         end;
{         emitoverflowcheck;}
      end;

    { use of A6 is required only temp (ok) }
    procedure secondaddr(var p : ptree);

      begin
         secondpass(p^.left);
         p^.location.loc:=LOC_REGISTER;
         p^.location.register:=getregister32;
         {@ on a procvar means returning an address to the procedure that
          is stored in it.}
       { yes but p^.left^.symtableentry can be nil
       for example on @self !! }
         { symtableentry can be also invalid, if left is no tree node }
         if (p^.left^.treetype=loadn) and
          assigned(p^.left^.symtableentry) and
            (p^.left^.symtableentry^.typ=varsym) and
          (Pvarsym(p^.left^.symtableentry)^.definition^.deftype=
           procvardef) then
            exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,S_L,
             newreference(p^.left^.location.reference),
             p^.location.register)))
         else
           begin
            exprasmlist^.concat(new(pai68k,op_ref_reg(A_LEA,S_L,
             newreference(p^.left^.location.reference),R_A0)));
            exprasmlist^.concat(new(pai68k,op_reg_reg(A_MOVE,S_L,
             R_A0,p^.location.register)));
           end;
         { for use of other segments }
         { if p^.left^.location.reference.segment<>R_DEFAULT_SEG then
             p^.location.segment:=p^.left^.location.reference.segment;
         }
         del_reference(p^.left^.location.reference);
      end;

    { register a6 used as scratch }
    procedure seconddoubleaddr(var p : ptree);

      begin
         secondpass(p^.left);
         p^.location.loc:=LOC_REGISTER;
         del_reference(p^.left^.location.reference);
         p^.location.register:=getregister32;
         exprasmlist^.concat(new(pai68k,op_ref_reg(A_LEA,S_L,
          newreference(p^.left^.location.reference),R_A0)));
         exprasmlist^.concat(new(pai68k,op_reg_reg(A_MOVE,S_L,
          R_A0,p^.location.register)));
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
                                      exprasmlist^.concat(new(pai68k,op_const_reg(A_EOR,S_B,1,p^.location.register)));
                                   end;
                    LOC_CREGISTER : begin
                                       secondpass(p^.left);
                                       p^.location.loc:=LOC_REGISTER;
                                       p^.location.register:=getregister32;
                                       emit_reg_reg(A_MOVE,S_B,p^.left^.location.register,
                                         p^.location.register);
                                       exprasmlist^.concat(new(pai68k,op_const_reg(A_EOR,S_B,1,p^.location.register)));
                                    end;
                    LOC_REFERENCE,LOC_MEM : begin
                                              secondpass(p^.left);
                                              del_reference(p^.left^.location.reference);
                                              p^.location.loc:=LOC_REGISTER;
                                              p^.location.register:=getregister32;
                                              if p^.left^.location.loc=LOC_CREGISTER then
                                                emit_reg_reg(A_MOVE,S_B,p^.left^.location.register,
                                                   p^.location.register)
                                              else
                                                exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,S_B,
                                              newreference(p^.left^.location.reference),
                                                p^.location.register)));
                                              exprasmlist^.concat(new(pai68k,op_const_reg(A_EOR,S_B,1,p^.location.register)));
                                           end;
                 end;
              end
            else
              begin
                secondpass(p^.left);
                p^.location.loc:=LOC_REGISTER;

                case p^.left^.location.loc of
                   LOC_REGISTER : begin
                                     p^.location.register:=p^.left^.location.register;
                                     exprasmlist^.concat(new(pai68k,op_reg(A_NOT,S_L,p^.location.register)));
                                  end;
                   LOC_CREGISTER : begin
                                     p^.location.register:=getregister32;
                                     emit_reg_reg(A_MOVE,S_L,p^.left^.location.register,
                                       p^.location.register);
                                     exprasmlist^.concat(new(pai68k,op_reg(A_NOT,S_L,p^.location.register)));
                                   end;
                   LOC_REFERENCE,LOC_MEM :
                                  begin
                                     del_reference(p^.left^.location.reference);
                                     p^.location.register:=getregister32;
                                     exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,S_L,
                                       newreference(p^.left^.location.reference),
                                       p^.location.register)));
                                     exprasmlist^.concat(new(pai68k,op_reg(A_NOT,S_L,p^.location.register)));
                                  end;
                end;
                {if  p^.left^.location.loc=loc_register then
                  p^.location.register:=p^.left^.location.register
                else
                  begin
                     del_locref(p^.left^.location);
                     p^.location.register:=getregister32;
                     exprasmlist^.concat(new(pai68k,op_loc_reg(A_MOV,S_L,
                       p^.left^.location,
                       p^.location.register)));
                  end;
                exprasmlist^.concat(new(pai68k,op_reg(A_NOT,S_L,p^.location.register)));}

             end;
      end;

    procedure secondnothing(var p : ptree);

      begin
      end;

    procedure secondassignment(var p : ptree);

      var
         opsize : topsize;
         withresult : boolean;
         otlabel,hlabel,oflabel : plabel;
         hregister : tregister;
         loc : tloc;

      begin
         otlabel:=truelabel;
         oflabel:=falselabel;
         getlabel(truelabel);
         getlabel(falselabel);
         withresult:=false;
         { calculate left sides }
         secondpass(p^.left);
         case p^.left^.location.loc of
            LOC_REFERENCE : begin
                              { in case left operator uses too many registers }
                              { but to few are free then LEA                  }
                              if (p^.left^.location.reference.base<>R_NO) and
                                 (p^.left^.location.reference.index<>R_NO) and
                                 (usablereg32<p^.right^.registers32) then
                                begin
                                   del_reference(p^.left^.location.reference);
                                   hregister:=getaddressreg;
                                   exprasmlist^.concat(new(pai68k,op_ref_reg(A_LEA,S_L,newreference(
                                     p^.left^.location.reference),
                                     hregister)));
                                   clear_reference(p^.left^.location.reference);
                                   p^.left^.location.reference.base:=hregister;
                                   p^.left^.location.reference.index:=R_NO;
                                end;
                              loc:=LOC_REFERENCE;
                           end;
            LOC_CREGISTER : loc:=LOC_CREGISTER;
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
             { we do not need destination anymore }
             del_reference(p^.left^.location.reference);
             { only source if withresult is set }
             if not(withresult) then
               del_reference(p^.right^.location.reference);
             loadstring(p);
             ungetiftemp(p^.right^.location.reference);
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
                                exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,opsize,
                                  newreference(p^.right^.location.reference),
                                  p^.left^.location.register)))
                              else
                                exprasmlist^.concat(new(pai68k,op_const_ref(A_MOVE,opsize,
                                  p^.right^.location.reference.offset,
                                  newreference(p^.left^.location.reference))));
                              {exprasmlist^.concat(new(pai68k,op_const_loc(A_MOV,opsize,
                                  p^.right^.location.reference.offset,
                                  p^.left^.location)));}
                           end
                         else
                           begin
                              concatcopy(p^.right^.location.reference,
                                p^.left^.location.reference,p^.left^.resulttype^.size,
                                withresult);
                              ungetiftemp(p^.right^.location.reference);
                           end;
                      end;
            LOC_REGISTER,
            LOC_CREGISTER : begin
                              case p^.right^.resulttype^.size of
                                 1 : opsize:=S_B;
                                 2 : opsize:=S_W;
                                 4 : opsize:=S_L;
                              end;
                              { simplified with op_reg_loc         }
                              if loc=LOC_CREGISTER then
                                exprasmlist^.concat(new(pai68k,op_reg_reg(A_MOVE,opsize,
                                  p^.right^.location.register,
                                  p^.left^.location.register)))
                              else
                                exprasmlist^.concat(new(pai68k,op_reg_ref(A_MOVE,opsize,
                                  p^.right^.location.register,
                                  newreference(p^.left^.location.reference))));
                              {exprasmlist^.concat(new(pai68k,op_reg_loc(A_MOV,opsize,
                                  p^.right^.location.register,
                                  p^.left^.location)));             }

                           end;
            LOC_FPU : begin
                              if loc<>LOC_REFERENCE then
                                internalerror(10010)
                              else
                                floatstore(pfloatdef(p^.left^.resulttype)^.typ,
                                  p^.right^.location,p^.left^.location.reference);
                      end;
            LOC_JUMP     : begin
                              getlabel(hlabel);
                              emitl(A_LABEL,truelabel);
                              if loc=LOC_CREGISTER then
                                exprasmlist^.concat(new(pai68k,op_const_reg(A_MOVE,S_B,
                                  1,p^.left^.location.register)))
                              else
                                exprasmlist^.concat(new(pai68k,op_const_ref(A_MOVE,S_B,
                                  1,newreference(p^.left^.location.reference))));
                              {exprasmlist^.concat(new(pai68k,op_const_loc(A_MOV,S_B,
                                  1,p^.left^.location)));}
                              emitl(A_JMP,hlabel);
                              emitl(A_LABEL,falselabel);
                              if loc=LOC_CREGISTER then
                                exprasmlist^.concat(new(pai68k,op_reg(A_CLR,S_B,
                                  p^.left^.location.register)))
                              else
                                exprasmlist^.concat(new(pai68k,op_const_ref(A_MOVE,S_B,
                                  0,newreference(p^.left^.location.reference))));
                              emitl(A_LABEL,hlabel);
                           end;
            LOC_FLAGS    : begin
                              if loc=LOC_CREGISTER then
                               begin
                                exprasmlist^.concat(new(pai68k,op_reg(flag_2_set[p^.right^.location.resflags],S_B,
                                  p^.left^.location.register)));
                                exprasmlist^.concat(new(pai68k,op_reg(A_NEG,S_B,p^.left^.location.register)));
                               end
                              else
                               begin
                                 exprasmlist^.concat(new(pai68k,op_ref(flag_2_set[p^.right^.location.resflags],S_B,
                                    newreference(p^.left^.location.reference))));
                                 exprasmlist^.concat(new(pai68k,op_ref(A_NEG,S_B,newreference(p^.left^.location.reference))));
                               end;

                           end;
         end;
         truelabel:=otlabel;
         falselabel:=oflabel;
      end;

    procedure secondderef(var p : ptree);

      var
         hr : tregister;

      begin
         secondpass(p^.left);
         clear_reference(p^.location.reference);
         case p^.left^.location.loc of
            LOC_REGISTER : Begin
                             hr := getaddressreg;
                             emit_reg_reg(A_MOVE,S_L,p^.left^.location.register,hr);
                             p^.location.reference.base:=hr;
                             ungetregister(p^.left^.location.register);
                           end;
            LOC_CREGISTER : begin
                               { ... and reserve one for the pointer }
                               hr:=getaddressreg;
                               emit_reg_reg(A_MOVE,S_L,p^.left^.location.register,hr);
                                      p^.location.reference.base:=hr;
                               { LOC_REGISTER indicates that this is a
                               variable register which should not be freed. }
{                               ungetregister(p^.left^.location.register); }
                            end;
            else
              begin
                 { free register }
                 del_reference(p^.left^.location.reference);

                 { ...and reserve one for the pointer }
                 hr:=getaddressreg;
                 exprasmlist^.concat(new(pai68k,op_ref_reg(
                   A_MOVE,S_L,newreference(p^.left^.location.reference),
                   hr)));
                 p^.location.reference.base:=hr;
              end;
         end;
      end;

    { used D0, D1 as scratch (ok) }
    { arrays ...                  }
    { Sets up the array and string }
    { references .                 }
    procedure secondvecn(var p : ptree);

      var
         pushed : boolean;
         ind : tregister;
         _p : ptree;

      procedure calc_emit_mul;

        var
           l1,l2 : longint;

        begin
           l1:=p^.resulttype^.size;
           case l1 of
              1     : p^.location.reference.scalefactor:=l1;
              2 : exprasmlist^.concat(new(pai68k,op_const_reg(A_LSL,S_L,1,ind)));
              4 : exprasmlist^.concat(new(pai68k,op_const_reg(A_LSL,S_L,2,ind)));
              8 : exprasmlist^.concat(new(pai68k,op_const_reg(A_LSL,S_L,3,ind)));
           else
             begin
               if ispowerof2(l1,l2) then
                 exprasmlist^.concat(new(pai68k,op_const_reg(A_LSL,S_L,l2,ind)))
                   else
                 begin
                   { use normal MC68000 signed multiply }
                   if (l1 >= -32768) and (l1 <= 32767) then
                     exprasmlist^.concat(new(pai68k,op_const_reg(A_MULS,S_W,l1,ind)))
                   else
                   { use long MC68020 long multiply }
                   if (aktoptprocessor
                    = MC68020) then
                     exprasmlist^.concat(new(pai68k,op_const_reg(A_MULS,S_L,l1,ind)))
                   else
                   { MC68000 long multiply }
                     begin
                       exprasmlist^.concat(new(pai68k,op_const_reg(A_MOVE,S_L,l1,R_D0)));
                       exprasmlist^.concat(new(pai68k,op_reg_reg(A_MOVE,S_L,ind,R_D1)));
                       emitcall('LONGMUL',true);
                       exprasmlist^.concat(new(pai68k,op_reg_reg(A_MOVE,S_L,R_D0,ind)));
                     end;
                 end;
             end; { else case }
            end; { end case }
        end; { calc_emit_mul }

      var
       extraoffset : longint;
         t : ptree;
         hp : preference;
         tai:pai68k;
       reg: tregister;

      begin
         secondpass(p^.left);
         { RESULT IS IN p^.location.reference }
         set_location(p^.location,p^.left^.location);

         { offset can only differ from 0 if arraydef }
         if p^.left^.resulttype^.deftype=arraydef then
           dec(p^.location.reference.offset,
             p^.resulttype^.size*
             parraydef(p^.left^.resulttype)^.lowrange);

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
                        p^.resulttype^.size*parraydef(p^.left^.resulttype)^.lowrange);
                    end
                   else
                     begin
                        { range checking for open arrays }
                     end;
                  end;
              inc(p^.left^.location.reference.offset,
                 p^.right^.value*p^.resulttype^.size);
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
                     p^.resulttype^.size*extraoffset);
                end;
              { calculate from left to right }
              if (p^.location.loc<>LOC_REFERENCE) and
                 (p^.location.loc<>LOC_MEM) then
                Message(cg_e_illegal_expression);

              pushed:=maybe_push(p^.right^.registers32,p);
              secondpass(p^.right);
              if pushed then restore(p);
                 case p^.right^.location.loc of
                LOC_REGISTER : begin
                                 ind:=p^.right^.location.register;
                                 case p^.right^.resulttype^.size of
                                 1: exprasmlist^.concat(new(pai68k,op_const_reg(A_AND,S_L,
                                      $ff,ind)));
                                 2: exprasmlist^.concat(new(pai68k,op_const_reg(A_AND,S_L,
                                      $ffff,ind)));
                                 end;
                               end;

                LOC_CREGISTER : begin
                                       ind:=getregister32;
                                   emit_reg_reg(A_MOVE,S_L,p^.right^.location.register,ind);
                                   case p^.right^.resulttype^.size of
                                   1: exprasmlist^.concat(new(pai68k,op_const_reg(A_AND,S_L,
                                      $ff,ind)));
                                   2: exprasmlist^.concat(new(pai68k,op_const_reg(A_AND,S_L,
                                      $ffff,ind)));
                                end;
                                end;
                   LOC_FLAGS:
                     begin
                        ind:=getregister32;
                        exprasmlist^.concat(new(pai68k,op_reg(flag_2_set[p^.right^.location.resflags],S_B,ind)));
                        exprasmlist^.concat(new(pai68k,op_const_reg(A_AND,S_L,$ff,ind)));
                     end
                else { else outer case }
                   begin
                      del_reference(p^.right^.location.reference);
                           ind:=getregister32;

                      exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,S_L,
                        newreference(p^.right^.location.reference),ind)));

                           {Booleans are stored in an 8 bit memory location, so
                           the use of MOVL is not correct.}
                      case p^.right^.resulttype^.size of
                        1: exprasmlist^.concat(new(pai68k,op_const_reg(A_AND,S_L,
                          $ff,ind)));
                        2: exprasmlist^.concat(new(pai68k,op_const_reg(A_AND,S_L,
                          $ffff,ind)));
                      end; { end case }
                      end; { end else begin }
              end;

              { produce possible range check code: }
              if cs_rangechecking in aktswitches  then
                begin
                   if p^.left^.resulttype^.deftype=arraydef then
                     begin
                        new(hp);
                        reset_reference(hp^);
                        parraydef(p^.left^.resulttype)^.genrangecheck;
                        hp^.symbol:=stringdup('R_'+tostr(parraydef(p^.left^.resulttype)^.rangenr));
                        emit_bounds_check(hp^,ind);
                     end;
                end;

           { ------------------------ HANDLE INDEXING ----------------------- }
           { In Motorola 680x0 mode, displacement can only be of 64K max.     }
           { Therefore instead of doing a direct displacement, we must first  }
           { load the new address into an address register. Therefore the     }
           { symbol is not used.                                              }
           if assigned(p^.location.reference.symbol) then
           begin
              if p^.location.reference.base <> R_NO then
               Message(cg_f_secondvecn_base_defined_twice);
              p^.location.reference.base:=getaddressreg;
              exprasmlist^.concat(new(pai68k,op_csymbol_reg(A_LEA,S_L,newcsymbol(p^.location.reference.symbol^,0),
                p^.location.reference.base)));
              stringdispose(p^.location.reference.symbol);
           end;

              if (p^.location.reference.index=R_NO) then
                begin
                   p^.location.reference.index:=ind;
                   calc_emit_mul;
               { here we must check for the offset      }
               { and if out of bounds for the motorola  }
               { eg: out of signed d8 then reload index }
               { with correct value.                    }
               if p^.location.reference.offset > 127 then
               begin
                  exprasmlist^.concat(new(pai68k,op_const_reg(A_ADD,S_L,p^.location.reference.offset,ind)));
                  p^.location.reference.offset := 0;
               end
               else
               if p^.location.reference.offset < -128 then
               begin
                  exprasmlist^.concat(new(pai68k,op_const_reg(A_SUB,S_L,-p^.location.reference.offset,ind)));
                  p^.location.reference.offset := 0;
               end;
                end
              else
                begin
                   if p^.location.reference.base=R_NO then
                      begin
                          case p^.location.reference.scalefactor of
                       2 : exprasmlist^.concat(new(pai68k,op_const_reg(A_LSL,S_L,1,p^.location.reference.index)));
                       4 : exprasmlist^.concat(new(pai68k,op_const_reg(A_LSL,S_L,2,p^.location.reference.index)));
                       8 : exprasmlist^.concat(new(pai68k,op_const_reg(A_LSL,S_L,3,p^.location.reference.index)));
                       end;
                          calc_emit_mul;

                    { we must use address register to put index in base }
                    { compare with cgi386.pas                           }

                    reg := getaddressreg;
                    p^.location.reference.base := reg;

                    emit_reg_reg(A_MOVE,S_L,p^.location.reference.index,reg);
                    ungetregister(p^.location.reference.index);

                    p^.location.reference.index:=ind;
                 end
               else
                 begin
                    reg := getaddressreg;
                    exprasmlist^.concat(new(pai68k,op_ref_reg(
                      A_LEA,S_L,newreference(p^.location.reference),
                      reg)));

                    ungetregister(p^.location.reference.base);
                    { the symbol offset is loaded,               }
                    { so release the symbol name and set symbol  }
                    { to nil                                     }
                    stringdispose(p^.location.reference.symbol);
                    p^.location.reference.offset:=0;
                    calc_emit_mul;
                    p^.location.reference.base:=reg;
                    ungetregister32(p^.location.reference.index);
                    p^.location.reference.index:=ind;
                 end;
               end;
           end;
      end;

    { *************** Converting Types **************** }

    { produces if necessary rangecheckcode }

    procedure maybe_rangechecking(p : ptree;p2,p1 : pdef);

      var
         hp : preference;
       hregister : tregister;
       neglabel,poslabel : plabel;

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
             ((porddef(p1)^.low>porddef(p2)^.low) or
             (porddef(p1)^.high<porddef(p2)^.high) or
             (porddef(p1)^.typ=u32bit) or
             (porddef(p2)^.typ=u32bit)) then
           begin
              porddef(p1)^.genrangecheck;
              if porddef(p2)^.typ=u8bit then
                begin
                   if (p^.location.loc=LOC_REGISTER) or
                      (p^.location.loc=LOC_CREGISTER) then
                     begin
                         exprasmlist^.concat(new(pai68k,op_reg_reg(A_MOVE,S_B,p^.location.register,R_D6)));
                         exprasmlist^.concat(new(pai68k,op_const_reg(A_AND,S_L,$FF,R_D6)));
                     end
                   else
                     begin
                         exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,S_B,newreference(p^.location.reference),R_D6)));
                         exprasmlist^.concat(new(pai68k,op_const_reg(A_AND,S_L,$FF,R_D6)));
                     end;
                   hregister:=R_D6;
                end
              else if porddef(p2)^.typ=s8bit then
                begin
                   if (p^.location.loc=LOC_REGISTER) or
                      (p^.location.loc=LOC_CREGISTER) then
                     begin
                         exprasmlist^.concat(new(pai68k,op_reg_reg(A_MOVE,S_B,p^.location.register,R_D6)));
                         { byte to long }
                         if aktoptprocessor = MC68020 then
                             exprasmlist^.concat(new(pai68k,op_reg(A_EXTB,S_L,R_D6)))
                         else
                           begin
                             exprasmlist^.concat(new(pai68k,op_reg(A_EXT,S_W,R_D6)));
                             exprasmlist^.concat(new(pai68k,op_reg(A_EXT,S_L,R_D6)));
                           end;
                     end
                   else
                     begin
                         exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,S_B,newreference(p^.location.reference),R_D6)));
                         { byte to long }
                         if aktoptprocessor = MC68020 then
                             exprasmlist^.concat(new(pai68k,op_reg(A_EXTB,S_L,R_D6)))
                         else
                           begin
                             exprasmlist^.concat(new(pai68k,op_reg(A_EXT,S_W,R_D6)));
                             exprasmlist^.concat(new(pai68k,op_reg(A_EXT,S_L,R_D6)));
                           end;
                     end; { end outermost else }
                   hregister:=R_D6;
                end
               { rangechecking for u32bit ?? !!!!!!}
               { lets try }
               else if (porddef(p2)^.typ=s32bit) or (porddef(p2)^.typ=u32bit)  then
                begin
                   if (p^.location.loc=LOC_REGISTER) or
                      (p^.location.loc=LOC_CREGISTER) then
                     hregister:=p^.location.register
                   else
                     begin
                        exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,S_L,newreference(p^.location.reference),R_D6)));
                        hregister:=R_D6;
                     end;
                end
              { rangechecking for u32bit ?? !!!!!!}
              else if porddef(p2)^.typ=u16bit then
                begin
                   if (p^.location.loc=LOC_REGISTER) or
                      (p^.location.loc=LOC_CREGISTER) then
                     exprasmlist^.concat(new(pai68k,op_reg_reg(A_MOVE,S_W,p^.location.register,R_D6)))
                   else
                     exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,S_W,newreference(p^.location.reference),R_D6)));
                   { unisgned extend }
                   exprasmlist^.concat(new(pai68k,op_const_reg(A_AND,S_L,$FFFF,R_D6)));
                   hregister:=R_D6;
                end
              else if porddef(p2)^.typ=s16bit then
                begin
                   if (p^.location.loc=LOC_REGISTER) or
                      (p^.location.loc=LOC_CREGISTER) then
                     exprasmlist^.concat(new(pai68k,op_reg_reg(A_MOVE,S_W,p^.location.register,R_D6)))
                   else
                     exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,S_W,newreference(p^.location.reference),R_D6)));
                   { sign extend }
                   exprasmlist^.concat(new(pai68k,op_reg(A_EXT,S_L,R_D6)));
                   hregister:=R_D6;
                end
              else internalerror(6);
              new(hp);
              reset_reference(hp^);
              hp^.symbol:=stringdup('R_'+tostr(porddef(p1)^.rangenr));
              if porddef(p1)^.low>porddef(p1)^.high then
                begin
                   getlabel(neglabel);
                   getlabel(poslabel);
                   exprasmlist^.concat(new(pai68k,op_reg(A_TST,S_L,hregister)));
                   emitl(A_BLT,neglabel);
                end;
              emit_bounds_check(hp^,hregister);
              if porddef(p1)^.low>porddef(p1)^.high then
                begin
                   new(hp);
                   reset_reference(hp^);
                   hp^.symbol:=stringdup('R_'+tostr(porddef(p1)^.rangenr+1));
                   emitl(A_JMP,poslabel);
                   emitl(A_LABEL,neglabel);
                   emit_bounds_check(hp^,hregister);
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
                     tc_u8bit_2_s32bit,
                tc_u8bit_2_u32bit,
                tc_s8bit_2_u32bit,
                tc_s8bit_2_s16bit,
                tc_s8bit_2_s32bit,
                tc_u8bit_2_u16bit,
                tc_s8bit_2_u16bit,
                tc_u8bit_2_s16bit: begin
                                    if is_register then
                                      hregister := p^.left^.location.register
                                    else
                                      hregister := getregister32;
                                    if is_register then
                                      emit_reg_reg(A_MOVE,S_B,p^.left^.location.register, hregister)
                                    else
                                    begin
                                      if p^.left^.location.loc = LOC_CREGISTER then
                                        emit_reg_reg(A_MOVE,S_B,p^.left^.location.register,hregister)
                                      else
                                        exprasmlist^.concat(new(pai68k, op_ref_reg(A_MOVE,S_B,
                                         newreference(P^.left^.location.reference), hregister)));
                                    end;

                                    case convtyp of
                                      tc_u8bit_2_s32bit,
                                      tc_u8bit_2_u32bit:
                                                   exprasmlist^.concat(new(pai68k, op_const_reg(
                                                   A_AND,S_L,$FF,hregister)));
                                      tc_s8bit_2_u32bit,
                                      tc_s8bit_2_s32bit:
                                                  begin
                                                    if aktoptprocessor = MC68020 then
                                                      exprasmlist^.concat(new(pai68k,op_reg
                                                        (A_EXTB,S_L,hregister)))
                                                    else { else if aktoptprocessor }
                                                    begin
                                                    { byte to word }
                                                      exprasmlist^.concat(new(pai68k,op_reg
                                                        (A_EXT,S_W,hregister)));
                                                    { word to long }
                                                      exprasmlist^.concat(new(pai68k,op_reg
                                                        (A_EXT,S_L,hregister)));
                                                    end;
                                                  end;
                                      tc_s8bit_2_u16bit,
                                      tc_u8bit_2_s16bit,
                                      tc_u8bit_2_u16bit:
                                                  exprasmlist^.concat(new(pai68k, op_const_reg(
                                                                A_AND,S_W,$FF,hregister)));

                                      tc_s8bit_2_s16bit:
                                                  exprasmlist^.concat(new(pai68k, op_reg(
                                                                A_EXT, S_W, hregister)));

                                    end; { inner case }
                                   end;
                tc_u16bit_2_u32bit,
                tc_u16bit_2_s32bit,
                tc_s16bit_2_u32bit,
                tc_s16bit_2_s32bit: begin
                                     if is_register then
                                       hregister := p^.left^.location.register
                                     else
                                       hregister := getregister32;
                                     if is_register then
                                       emit_reg_reg(A_MOVE,S_W,p^.left^.location.register, hregister)
                                     else
                                     begin
                                       if p^.left^.location.loc = LOC_CREGISTER then
                                         emit_reg_reg(A_MOVE,S_W,p^.left^.location.register,hregister)
                                       else
                                         exprasmlist^.concat(new(pai68k, op_ref_reg(A_MOVE,S_W,
                                           newreference(P^.left^.location.reference), hregister)));
                                     end;
                                     if (convtyp = tc_u16bit_2_s32bit) or
                                        (convtyp = tc_u16bit_2_u32bit) then
                                         exprasmlist^.concat(new(pai68k, op_const_reg(
                                           A_AND, S_L, $ffff, hregister)))
                                     else { tc_s16bit_2_s32bit }
                                          { tc_s16bit_2_u32bit }
                                         exprasmlist^.concat(new(pai68k, op_reg(A_EXT,S_L,
                                           hregister)));
                                    end;
             end { end case }
         else
         begin
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
                        op:=A_MOVE;
                        opsize:=S_L;
                    end;
                tc_s8bit_2_u16bit,
                tc_s8bit_2_s16bit,
                tc_u8bit_2_s16bit,
                tc_u8bit_2_u16bit:
                    begin
                        hregister:=getregister32;
                        op:=A_MOVE;
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
                 else exprasmlist^.concat(new(pai68k,op_ref_reg(op,opsize,
                     newreference(p^.left^.location.reference),hregister)));
              end;
         end; { end elseif }

         p^.location.loc:=LOC_REGISTER;
         p^.location.register:=hregister;
         maybe_rangechecking(p,p^.left^.resulttype,p^.resulttype);
      end;



    procedure second_string_string(p,hp : ptree;convtyp : tconverttype);

      var
         pushedregs : tpushed;

      begin
         stringdispose(p^.location.reference.symbol);
         gettempofsizereference(p^.resulttype^.size,p^.location.reference);
         del_reference(p^.left^.location.reference);
         copystring(p^.location.reference,p^.left^.location.reference,pstringdef(p^.resulttype)^.len);
         ungetiftemp(p^.left^.location.reference);
      end;

    procedure second_cstring_charpointer(p,hp : ptree;convtyp : tconverttype);

      begin
         p^.location.loc:=LOC_REGISTER;
         p^.location.register:=getregister32;
         inc(p^.left^.location.reference.offset);
         exprasmlist^.concat(new(pai68k,op_ref_reg(A_LEA,S_L,newreference(p^.left^.location.reference),
           R_A0)));
         emit_reg_reg(A_MOVE, S_L, R_A0, p^.location.register);
      end;

    procedure second_cchar_charpointer(p,hp : ptree;convtyp : tconverttype);

      begin
         {!!!!}
         p^.location.loc:=LOC_REGISTER;
         p^.location.register:=getregister32;
         inc(p^.left^.location.reference.offset);
         exprasmlist^.concat(new(pai68k,op_ref_reg(A_LEA,S_L,newreference(p^.left^.location.reference),
           R_A0)));
         emit_reg_reg(A_MOVE, S_L, R_A0, p^.location.register);
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
         exprasmlist^.concat(new(pai68k,op_ref_reg(A_LEA,S_L,newreference(p^.left^.location.reference),
           R_A0)));
         emit_reg_reg(A_MOVE,S_L,R_A0, P^.location.register);
      end;

    procedure second_pointer_to_array(p,hp : ptree;convtyp : tconverttype);

      var
       reg: tregister;
      begin
         p^.location.loc:=LOC_REFERENCE;
         clear_reference(p^.location.reference);
         { here, after doing some arithmetic on the pointer }
         { we put it back in an address register            }
         if p^.left^.location.loc=LOC_REGISTER then
         begin
           reg := getaddressreg;
           { move the pointer in a data register back into }
           { an address register.                          }
           emit_reg_reg(A_MOVE, S_L, p^.left^.location.register,reg);

           p^.location.reference.base:=reg;
           ungetregister32(p^.left^.location.register);
         end
         else
           begin
              if p^.left^.location.loc=LOC_CREGISTER then
                begin
                   p^.location.reference.base:=getaddressreg;
                   emit_reg_reg(A_MOVE,S_L,p^.left^.location.register,
                     p^.location.reference.base);
                end
              else
                begin
                   del_reference(p^.left^.location.reference);
                   p^.location.reference.base:=getaddressreg;
                   exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,S_L,newreference(p^.left^.location.reference),
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
           exprasmlist^.concat(new(pai68k,op_const_ref(A_MOVE,S_B,l,
             newreference(p^.location.reference))));

         { copy to first char of string }
         inc(p^.location.reference.offset);

         { generates the copy code      }
         { and we need the source never }
         concatcopy(p^.left^.location.reference,p^.location.reference,l,true);

         { correct the string location }
         dec(p^.location.reference.offset);
      end;


(*    procedure second_char_to_string(p,hp : ptree;convtyp : tconverttype);

      begin
         stringdispose(p^.location.reference.symbol);
         gettempofsizereference(256,p^.location.reference);
         { is it a char const ? }
         if p^.left^.treetype=ordconstn then
           exprasmlist^.concat(new(pai68k,op_const_ref(A_MOVE,S_W,p^.left^.value*256+1,newreference(p^.location.reference))))
         else
           begin
              { not so elegant (goes better with extra register     }
              { Here the conversion is done in one shot             }
              { i.e we convert to a string with a single word which }
              { will be stored, the length followed by the char     }
              { This is of course, endian specific.                 }
              if (p^.left^.location.loc=LOC_REGISTER) or
                 (p^.left^.location.loc=LOC_CREGISTER) then
                begin
                   exprasmlist^.concat(new(pai68k,op_reg_reg(A_MOVE,S_B,p^.left^.location.register,R_D6)));
                   exprasmlist^.concat(new(pai68k, op_const_reg(A_AND, S_W, $FF, R_D6)));
                   ungetregister32(p^.left^.location.register);
                end
              else
                begin
                   exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,S_B,newreference(p^.left^.location.reference),R_D6)));
                   exprasmlist^.concat(new(pai68k, op_const_reg(A_AND, S_W, $FF, R_D6)));
                   del_reference(p^.left^.location.reference);
                end;
              if (aktoptprocessor = MC68020) then
              { alignment is not a problem on the 68020 and higher processors }
                Begin
                  { add length of string to word }
                  exprasmlist^.concat(new(pai68k,op_const_reg(A_OR,S_W,$0100,R_D6)));
                  { put back into mem ...        }
                  exprasmlist^.concat(new(pai68k,op_reg_ref(A_MOVE,S_W,R_D6,newreference(p^.location.reference))));
                end
              else
                Begin
                 { alignment can cause problems }
                  { add length of string to ref }
                  exprasmlist^.concat(new(pai68k,op_const_ref(A_MOVE,S_B,1,newreference(p^.location.reference))));
                  if abs(p^.location.reference.offset) >= 1 then
                    Begin
                      { temporarily decrease offset }
                      Inc(p^.location.reference.offset);
                      exprasmlist^.concat(new(pai68k,op_reg_ref(A_MOVE,S_B,R_D6,newreference(p^.location.reference))));
                      Dec(p^.location.reference.offset);
                      { restore offset }
                    end
                  else
                   Begin
                     Comment(V_Debug,'SecondChar2String() internal error.');
                     internalerror(34);
                  end;
                end;
           end;
      end;*)

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
        reg:tregister;
      begin
        emitloadord2reg(p^.left^.location, porddef(p^.left^.resulttype), R_D6, true);
        ungetiftemp(p^.left^.location.reference);
        if porddef(p^.left^.resulttype)^.typ=u32bit then
           push_int(0);

        emit_reg_reg(A_MOVE, S_L, R_D6, R_SPPUSH);
        new(r);
        reset_reference(r^);
        r^.base := R_SP;
        { no emulation }
{           for u32bit a solution would be to push $0 and to load a
+          comp
+           if porddef(p^.left^.resulttype)^.typ=u32bit then
+             exprasmlist^.concat(new(pai386,op_ref(A_FILD,S_IQ,r)))
+           else}
          p^.location.loc := LOC_FPU;
          { get floating point register. }
          if (cs_fp_emulation in aktswitches) then
          begin
            p^.location.fpureg := getregister32;
            exprasmlist^.concat(new(pai68k, op_ref_reg(A_MOVE, S_L, r, R_D0)));
            emitcall('LONG2SINGLE',true);
            emit_reg_reg(A_MOVE,S_L,R_D0,p^.location.fpureg);
          end
          else
          begin
            p^.location.fpureg := getfloatreg;
            exprasmlist^.concat(new(pai68k, op_ref_reg(A_FMOVE, S_L, r, p^.location.fpureg)))
          end;
        if porddef(p^.left^.resulttype)^.typ=u32bit then
           exprasmlist^.concat(new(pai68k,op_const_reg(A_ADD,S_L,8,R_SP)))
        else
        { restore the stack to the previous address }
           exprasmlist^.concat(new(pai68k, op_const_reg(A_ADDQ, S_L, 4, R_SP)));
      end;


    procedure second_real_fix(p,hp : ptree;convtyp : tconverttype);

      var
         {hs : string;}
         rreg : tregister;
         ref : treference;

      begin
         rreg:=getregister32;
         { Are we in a LOC_FPU, if not then use scratch registers }
         { instead of allocating reserved registers.              }
         if (p^.left^.location.loc<>LOC_FPU) then
         begin
           if (cs_fp_emulation in aktswitches) then
           begin
             exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,S_L,newreference(p^.left^.location.reference),R_D0)));
             exprasmlist^.concat(new(pai68k,op_const_reg(A_MOVE,S_L,65536,R_D1)));
             emitcall('LONGMUL',true);
             emit_reg_reg(A_MOVE,S_L,R_D0,rreg);
           end
           else
           begin
             exprasmlist^.concat(new(pai68k,op_ref_reg(A_FMOVE,S_L,newreference(p^.left^.location.reference),R_FP0)));
             exprasmlist^.concat(new(pai68k,op_const_reg(A_FMUL,S_L,65536,R_FP0)));
             exprasmlist^.concat(new(pai68k,op_reg_reg(A_FMOVE,S_L,R_FP0,rreg)));
           end;
         end
         else
         begin
           if (cs_fp_emulation in aktswitches) then
           begin
             exprasmlist^.concat(new(pai68k,op_reg_reg(A_MOVE,S_L,p^.left^.location.fpureg,R_D0)));
             exprasmlist^.concat(new(pai68k,op_const_reg(A_MOVE,S_L,65536,R_D1)));
             emitcall('LONGMUL',true);
             emit_reg_reg(A_MOVE,S_L,R_D0,rreg);
           end
           else
           begin
             exprasmlist^.concat(new(pai68k,op_const_reg(A_FMUL,S_L,65536,p^.left^.location.fpureg)));
             exprasmlist^.concat(new(pai68k,op_reg_reg(A_FMOVE,S_L,p^.left^.location.fpureg,rreg)));
           end;
         end;
         p^.location.loc:=LOC_REGISTER;
         p^.location.register:=rreg;
      end;


    procedure second_float_float(p,hp : ptree;convtyp : tconverttype);

      begin
         case p^.left^.location.loc of
            LOC_FPU :  begin
                         { reload }
                         p^.location.loc := LOC_FPU;
                         p^.location.fpureg := p^.left^.location.fpureg;
                       end;
            LOC_MEM,
            LOC_REFERENCE : floatload(pfloatdef(p^.left^.resulttype)^.typ,
                              p^.left^.location.reference,p^.location);
         end;
{ ALREADY HANDLED BY FLOATLOAD      }
{         p^.location.loc:=LOC_FPU; }
      end;

    procedure second_fix_real(p,hp : ptree;convtyp : tconverttype);

    var
        startreg : tregister;
        hl : plabel;
        r : treference;
        reg1: tregister;
        hl1,hl2,hl3,hl4,hl5,hl6,hl7,hl8,hl9: plabel;
      begin
         if (p^.left^.location.loc=LOC_REGISTER) or
            (p^.left^.location.loc=LOC_CREGISTER) then
           begin
              startreg:=p^.left^.location.register;
              ungetregister(startreg);
              { move d0,d0 is removed by emit_reg_reg }
              emit_reg_reg(A_MOVE,S_L,startreg,R_D0);
           end
         else
           begin
              exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,S_L,newreference(
                p^.left^.location.reference),R_D0)));
              del_reference(p^.left^.location.reference);
              startreg:=R_NO;
           end;

         reg1 := getregister32;

         { Motorola 68000 equivalent of CDQ     }
         { we choose d1:d0 pair for quad word   }
         exprasmlist^.concat(new(pai68k,op_reg(A_TST,S_L,R_D0)));
         getlabel(hl1);
         emitl(A_BPL,hl1);
         { we copy all bits (-ve number) }
         exprasmlist^.concat(new(pai68k,op_const_reg(A_MOVE,S_L,$ffffffff,R_D1)));
         getlabel(hl2);
         emitl(A_BRA,hl2);
         emitl(A_LABEL,hl1);
         exprasmlist^.concat(new(pai68k,op_reg(A_CLR,S_L,R_D0)));
         emitl(A_LABEL,hl2);
         { end CDQ }

         exprasmlist^.concat(new(pai68k,op_reg_reg(A_EOR,S_L,R_D1,R_D0)));
         exprasmlist^.concat(new(pai68k,op_reg_reg(A_MOVE,S_L,R_D0,reg1)));
         getlabel(hl3);
         emitl(A_BEQ,hl3);

         { Motorola 68000 equivalent of RCL    }
         getlabel(hl4);
         emitl(A_BCC,hl4);
         exprasmlist^.concat(new(pai68k,op_const_reg(A_LSL,S_L,1,reg1)));
         exprasmlist^.concat(new(pai68k,op_const_reg(A_OR,S_L,1,reg1)));
         getlabel(hl5);
         emitl(A_BRA,hl5);
         emitl(A_LABEL,hl4);
         exprasmlist^.concat(new(pai68k,op_const_reg(A_LSL,S_L,1,reg1)));
         emitl(A_LABEL,hl5);
         { end RCL }

         { Motorola 68000 equivalent of BSR }
         { save register }
         exprasmlist^.concat(new(pai68k,op_reg_reg(A_MOVE,S_L,R_D0,R_D6)));
         exprasmlist^.concat(new(pai68k,op_const_reg(A_MOVE,S_B,31,R_D0)));
         getlabel(hl6);
         emitl(A_LABEL,hl6);
         exprasmlist^.concat(new(pai68k,op_reg_reg(A_BTST,S_L,R_D0,R_D1)));
         getlabel(hl7);
         emitl(A_BNE,hl7);
         exprasmlist^.concat(new(pai68k,op_const_reg(A_SUBQ,S_B,1,R_D0)));
         emitl(A_BPL,hl6);
         { restore register }
         exprasmlist^.concat(new(pai68k,op_reg_reg(A_MOVE,S_L,R_D6,R_D0)));
         emitl(A_LABEL,hl7);
         { end BSR }

         exprasmlist^.concat(new(pai68k,op_const_reg(A_MOVE,S_B,32,R_D6)));
         exprasmlist^.concat(new(pai68k,op_reg_reg(A_SUB,S_B,R_D1,R_D6)));
         exprasmlist^.concat(new(pai68k,op_reg_reg(A_LSL,S_L,R_D6,R_D0)));
         exprasmlist^.concat(new(pai68k,op_const_reg(A_ADD,S_W,1007,R_D1)));
         exprasmlist^.concat(new(pai68k,op_const_reg(A_LSL,S_L,5,R_D1)));

         { Motorola 68000 equivalent of SHLD }
         exprasmlist^.concat(new(pai68k,op_const_reg(A_MOVE,S_W,11,R_D6)));
         { save register }
         exprasmlist^.concat(new(pai68k,op_reg_reg(A_MOVE,S_L,R_D1,R_A0)));
         getlabel(hl8);
         emitl(A_LABEL,hl8);
         exprasmlist^.concat(new(pai68k,op_const_reg(A_ROXL,S_W,1,R_D1)));
         exprasmlist^.concat(new(pai68k,op_const_reg(A_ROXL,S_W,1,reg1)));
         exprasmlist^.concat(new(pai68k,op_const_reg(A_SUBQ,S_B,1,R_D6)));
         emitl(A_BNE,hl8);
         { restore register }
         exprasmlist^.concat(new(pai68k,op_reg_reg(A_MOVE,S_L,R_A0,R_D1)));
         { end Motorola equivalent of SHLD }

         { Motorola 68000 equivalent of SHLD }
         exprasmlist^.concat(new(pai68k,op_const_reg(A_MOVE,S_W,20,R_D6)));
         { save register }
         exprasmlist^.concat(new(pai68k,op_reg_reg(A_MOVE,S_L,R_D0,R_A0)));
         getlabel(hl9);
         emitl(A_LABEL,hl9);
         exprasmlist^.concat(new(pai68k,op_const_reg(A_ROXL,S_W,1,R_D0)));
         exprasmlist^.concat(new(pai68k,op_const_reg(A_ROXL,S_W,1,reg1)));
         exprasmlist^.concat(new(pai68k,op_const_reg(A_SUBQ,S_B,1,R_D6)));
         emitl(A_BNE,hl9);
         { restore register }
         exprasmlist^.concat(new(pai68k,op_reg_reg(A_MOVE,S_L,R_A0,R_D0)));
         { end Motorola equivalent of SHLD }

         exprasmlist^.concat(new(pai68k,op_const_reg(A_MOVE,S_B,20,R_D6)));
         exprasmlist^.concat(new(pai68k,op_reg_reg(A_SUB,S_L,R_D6,R_D0)));
         emitl(A_LABEL, hl3);

         { create temp values and put on stack }
         exprasmlist^.concat(new(pai68k,op_reg_reg(A_MOVE,S_L,reg1,R_SPPUSH)));
         exprasmlist^.concat(new(pai68k,op_reg_reg(A_MOVE,S_L,R_D0,R_SPPUSH)));


         reset_reference(r);
         r.base:=R_SP;

         if (cs_fp_emulation in aktswitches) then
         begin
           p^.location.loc:=LOC_FPU;
           p^.location.fpureg := getregister32;
           exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,S_L,newreference(r),
             p^.left^.location.fpureg)))
         end
         else
         begin
           p^.location.loc:=LOC_FPU;
           p^.location.fpureg := getfloatreg;
           exprasmlist^.concat(new(pai68k,op_ref_reg(A_FMOVE,S_L,newreference(r),
               p^.left^.location.fpureg)))
         end;
         { clear temporary space }
         exprasmlist^.concat(new(pai68k,op_const_reg(A_ADDQ,S_L,8,R_SP)));
         ungetregister32(reg1);
{ Alreadu handled above...          }
{         p^.location.loc:=LOC_FPU; }
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
                s8bit : begin
                           exprasmlist^.concat(new(pai68k, op_ref_reg(A_MOVE,S_B,
                              newreference(p^.left^.location.reference),hregister)));
                           if aktoptprocessor = MC68020 then
                              exprasmlist^.concat(new(pai68k, op_reg(A_EXTB,S_L,hregister)))
                           else
                            begin
                              exprasmlist^.concat(new(pai68k, op_reg(A_EXT,S_W,hregister)));
                              exprasmlist^.concat(new(pai68k, op_reg(A_EXT,S_L,hregister)));
                            end;
                        end;
                u8bit : begin
                          exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,S_B,newreference(p^.left^.location.reference),
                            hregister)));
                          exprasmlist^.concat(new(pai68k,op_const_reg(A_AND,S_L,$ff,hregister)));
                        end;
                s16bit :begin
                         exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,S_W,newreference(p^.left^.location.reference),
                           hregister)));
                          exprasmlist^.concat(new(pai68k,op_reg(A_EXT,S_L,hregister)));
                        end;
                u16bit : begin
                            exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,S_W,newreference(p^.left^.location.reference),
                               hregister)));
                            exprasmlist^.concat(new(pai68k,op_const_reg(A_AND,S_L,$ffff,hregister)));
                         end;
                s32bit,u32bit : exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,S_L,newreference(p^.left^.location.reference),
                  hregister)));
                {!!!! u32bit }
              end;
           end;
         exprasmlist^.concat(new(pai68k,op_const_reg(A_MOVEQ,S_L,16,R_D1)));
         exprasmlist^.concat(new(pai68k,op_reg_reg(A_LSL,S_L,R_D1,hregister)));

         p^.location.loc:=LOC_REGISTER;
         p^.location.register:=hregister;
      end;

    procedure second_smaller(p,hp : ptree;convtyp : tconverttype);

      var
         hregister,destregister : tregister;
         {opsize : topsize;}
         ref : boolean;
         hpp : preference;

      begin
         { !!!!!!!! Rangechecking }
         ref:=false;
         { problems with enums !! }
         if (cs_rangechecking in aktswitches)  and
           { with $R+ explicit type conversations in TP aren't range checked! }
           (not(p^.explizit) or not(cs_tp_compatible in aktswitches)) and
           (p^.resulttype^.deftype=orddef) and
           (hp^.resulttype^.deftype=orddef) and
           ((porddef(p^.resulttype)^.low>porddef(hp^.resulttype)^.low) or
           (porddef(p^.resulttype)^.high<porddef(hp^.resulttype)^.high)) then
           begin
              porddef(p^.resulttype)^.genrangecheck;
              if porddef(hp^.resulttype)^.typ=s32bit then
                begin
                   if (p^.location.loc=LOC_REGISTER) or
                      (p^.location.loc=LOC_CREGISTER) then
                     hregister:=p^.location.register
                   else
                     begin
                        hregister:=getregister32;
                        exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,S_L,newreference(p^.location.reference),hregister)));
                     end;
                end
              { rangechecking for u32bit ?? !!!!!!}
              else if porddef(hp^.resulttype)^.typ=u16bit then
                begin
                   hregister:=getregister32;
                   if (p^.location.loc=LOC_REGISTER) or
                      (p^.location.loc=LOC_CREGISTER) then
                   begin
                     exprasmlist^.concat(new(pai68k,op_reg_reg(A_MOVE,S_W,p^.location.register,hregister)));
                   end
                   else
                     exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,S_W,newreference(p^.location.reference),hregister)));
                   { clear unused bits  i.e unsigned extend}
                   exprasmlist^.concat(new(pai68k,op_const_reg(A_AND,S_L, $FFFF, hregister)));
                end
              else if porddef(hp^.resulttype)^.typ=s16bit then
                begin
                   hregister:=getregister32;
                   if (p^.location.loc=LOC_REGISTER) or
                      (p^.location.loc=LOC_CREGISTER) then
                     exprasmlist^.concat(new(pai68k,op_reg_reg(A_MOVE,S_W,p^.location.register,hregister)))
                   else
                     exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,S_W,newreference(p^.location.reference),hregister)));
                   { sign extend }
                   exprasmlist^.concat(new(pai68k,op_reg(A_EXT, S_L, hregister)));
                end
              else internalerror(6);
              new(hpp);
              reset_reference(hpp^);
              hpp^.symbol:=stringdup('R_'+tostr(porddef(p^.resulttype)^.rangenr));


              emit_bounds_check(hpp^, hregister);

              p^.location.loc:=LOC_REGISTER;
              p^.location.register:=hregister;
              exit;
           end;
         if (p^.left^.location.loc=LOC_REGISTER) or
           (p^.left^.location.loc=LOC_CREGISTER) then
           begin
{ handled by secondpas by called routine ??? }
{              p^.location.loc:=p^.left^.location.loc; }
              p^.location.register:=p^.left^.location.register;
           end;
      end;

    procedure second_proc_to_procvar(p,hp : ptree;convtyp : tconverttype);far;

    begin
        secondpass(hp);
        p^.location.loc:=LOC_REGISTER;
        del_reference(hp^.location.reference);
        p^.location.register:=getregister32;
        exprasmlist^.concat(new(pai68k,op_ref_reg(A_LEA,S_L,
         newreference(hp^.location.reference),R_A0)));

        emit_reg_reg(A_MOVE, S_L, R_A0, P^.location.register);
    end;

   procedure second_bool_to_int(p,hp : ptree;convtyp : tconverttype);

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
        p^.location.register:=getregister32;
        case hp^.location.loc of
          LOC_MEM,LOC_REFERENCE :
            exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,S_B,
              newreference(hp^.location.reference),p^.location.register)));
          LOC_REGISTER,LOC_CREGISTER :
            exprasmlist^.concat(new(pai68k,op_reg_reg(A_MOVE,S_B,
              hp^.location.register,p^.location.register)));
           LOC_FLAGS:
            begin
               exprasmlist^.concat(new(pai68k,op_reg(flag_2_set[hp^.location.resflags],S_NO,
                 p^.location.register)))
            end;
           LOC_JUMP:
             begin
                getlabel(hlabel);
                emitl(A_LABEL,truelabel);
                exprasmlist^.concat(new(pai68k,op_const_reg(A_MOVE,S_B,
                  1,p^.location.register)));
                emitl(A_JMP,hlabel);
                emitl(A_LABEL,falselabel);
                exprasmlist^.concat(new(pai68k,op_reg(A_CLR,S_B,p^.location.register)));
                emitl(A_LABEL,hlabel);
             end;
        else
          internalerror(10060);
        end;
         truelabel:=oldtruelabel;
         falselabel:=oldfalselabel;
     end;

   procedure second_int_to_bool(p,hp : ptree;convtyp : tconverttype);
     begin
      { !!!!!!!!!!!!!!! }
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
                              second_bool_to_int,second_int_to_bool,
                              second_chararray_to_string,
                              second_proc_to_procvar,
                              { is constant char to pchar, is done by firstpass }
                              second_nothing);

      begin
         { this isn't good coding, I think tc_bool_2_u8bit, shouldn't be }
         { type conversion (FK)                                        }

         { this is necessary, because second_bool_byte, have to change   }
         { true- and false label before calling secondpass               }
         if p^.convtyp<>tc_bool_2_int then
         begin
           secondpass(p^.left);
           set_location(p^.location,p^.left^.location);
         end;
         if p^.convtyp<>tc_equal then
           {the second argument only is for maybe_range_checking !}
           secondconvert[p^.convtyp](p,p^.left,p^.convtyp)
      end;

    { save the size of pushed parameter }
    var
       pushedparasize : longint;

    procedure secondcallparan(var p : ptree;defcoll : pdefcoll;
                push_from_left_to_right : boolean);

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
           secondcallparan(p^.right,defcoll^.next,push_from_left_to_right);
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
              if p^.left^.treetype=addrn then
                begin
                   { allways a register }
                   exprasmlist^.concat(new(pai68k,op_reg_reg(A_MOVE,S_L,p^.left^.location.register,R_SPPUSH)));
                   ungetregister32(p^.left^.location.register);
                end
              else
                begin
                   if (p^.left^.location.loc<>LOC_REFERENCE) and
                      (p^.left^.location.loc<>LOC_MEM) then
                     Message(sym_e_type_mismatch)
                   else
                     begin
                        emitpushreferenceaddr(p^.left^.location.reference);
                        del_reference(p^.left^.location.reference);
                     end;
                end;
              inc(pushedparasize,4);
           end
         { handle call by reference parameter }
         else if (defcoll^.paratyp=vs_var) then
           begin
              if (p^.left^.location.loc<>LOC_REFERENCE) then
                Message(cg_e_var_must_be_reference);
              { open array ? }
              { defcoll^.data can be nil for read/write }
              if assigned(defcoll^.data) and
                is_open_array(defcoll^.data) then
                begin
                   { push high }
                   if is_open_array(p^.left^.resulttype) then
                     begin
                        new(r);
                        reset_reference(r^);
                        r^.base:=highframepointer;
                        r^.offset:=highoffset+4;
                        exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,S_L,r,R_SPPUSH)));
                     end
                   else
                     push_int(parraydef(p^.left^.resulttype)^.highrange-
                              parraydef(p^.left^.resulttype)^.lowrange);
                   inc(pushedparasize,4);
                end;
              emitpushreferenceaddr(p^.left^.location.reference);
              del_reference(p^.left^.location.reference);
              inc(pushedparasize,4);
           end
         else
           begin
              tempdeftype:=p^.resulttype^.deftype;
              if tempdeftype=filedef then
                Message(cg_e_file_must_call_by_reference);
              if (defcoll^.paratyp=vs_const) and
                 dont_copy_const_param(p^.resulttype) then
                begin
                   emitpushreferenceaddr(p^.left^.location.reference);
                   del_reference(p^.left^.location.reference);
                   inc(pushedparasize,4);
                end
              else
                case p^.left^.location.loc of
                   LOC_REGISTER,
                   LOC_CREGISTER : begin
                                             exprasmlist^.concat(new(pai68k,op_reg_reg(A_MOVE,S_L,
                                                 p^.left^.location.register,R_SPPUSH)));
                                             inc(pushedparasize,4);
                                             ungetregister32(p^.left^.location.register);
                                     end;
                   LOC_FPU : begin
                                        size:=pfloatdef(p^.left^.resulttype)^.size;
                                        inc(pushedparasize,size);
                                        exprasmlist^.concat(new(pai68k,op_const_reg(A_SUBQ,S_L,size,R_SP)));
                                        new(r);
                                        reset_reference(r^);
                                        r^.base:=R_SP;
                                        s:=getfloatsize(pfloatdef(p^.left^.resulttype)^.typ);
                                        if (cs_fp_emulation in aktswitches) then
                                        begin
                                          { when in emulation mode... }
                                          { only single supported!!!  }
                                          exprasmlist^.concat(new(pai68k,op_reg_ref(A_MOVE,S_L,
                                             p^.left^.location.fpureg,r)));
                                        end
                                        else
                                          { convert back from extended to normal type }
                                          exprasmlist^.concat(new(pai68k,op_reg_ref(A_FMOVE,s,
                                             p^.left^.location.fpureg,r)));
                                     end;
                   LOC_REFERENCE,LOC_MEM :
                               begin
                                  tempreference:=p^.left^.location.reference;
                                  del_reference(p^.left^.location.reference);
                                  case p^.resulttype^.deftype of
                                     orddef : begin
                                                   case porddef(p^.resulttype)^.typ of
                                                      s32bit,u32bit :
                                                        begin
                                                           emit_push_mem(tempreference);
                                                           inc(pushedparasize,4);
                                                        end;
                                                      s8bit,u8bit,uchar,bool8bit,s16bit,u16bit : begin
                                                          exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,S_W,
                                                            newreference(tempreference),R_SPPUSH)));
                                                          inc(pushedparasize,2);
                                                      end;
                                                    end;
                                              end;
                                     floatdef : begin
                                                   case pfloatdef(p^.resulttype)^.typ of
                                                      f32bit,
                                                      s32real :
                                                        begin
                                                           emit_push_mem(tempreference);
                                                           inc(pushedparasize,4);
                                                        end;
                                                      s64real:
                                                      {s64bit }
                                                                begin
                                                                   inc(tempreference.offset,4);
                                                                   emit_push_mem(tempreference);
                                                                   dec(tempreference.offset,4);
                                                                   emit_push_mem(tempreference);
                                                                   inc(pushedparasize,8);
                                                                end;
{$ifdef use48}
                                                      s48real : begin
                                                                end;
{$endif}
                                                      s80real : begin
                                                                    Message(cg_f_extended_cg68k_not_supported);
{                                                                   inc(tempreference.offset,6);
                                                                   emit_push_mem(tempreference);
                                                                   dec(tempreference.offset,4);
                                                                   emit_push_mem(tempreference);
                                                                   dec(tempreference.offset,2);
                                                                   exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,S_W,
                                                                     newreference(tempreference),R_SPPUSH)));
                                                                   inc(pushedparasize,extended_size);}
                                                                end;
                                                   end;
                                                end;
                                     pointerdef,procvardef,
                                         enumdef,classrefdef:  begin
                                                      emit_push_mem(tempreference);
                                                      inc(pushedparasize,4);
                                                   end;
                                     arraydef,recorddef,stringdef,setdef,objectdef :
                                                begin
                                                   if ((p^.resulttype^.deftype=setdef) and
                                                     (psetdef(p^.resulttype)^.settype=smallset)) then
                                                     begin
                                                        emit_push_mem(tempreference);
                                                        inc(pushedparasize,4);
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
                                                        if (size > 0) and (size < 9) then
                                                            exprasmlist^.concat(new(pai68k,op_const_reg(A_SUBQ,S_L,size,R_SP)))
                                                        else
                                                            exprasmlist^.concat(new(pai68k,op_const_reg(A_SUBA,
                                                              S_L,size,R_SP)));
                                                        inc(pushedparasize,size);
                                                        { create stack reference }
                                                        stackref.symbol := nil;
                                                        clear_reference(stackref);
                                                        stackref.base:=R_SP;
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
                 LOC_JUMP     : begin
                                   getlabel(hlabel);
                                   inc(pushedparasize,2);
                                   emitl(A_LABEL,truelabel);
                                   exprasmlist^.concat(new(pai68k,op_const_reg(A_MOVE,S_W,1,R_SPPUSH)));
                                   emitl(A_JMP,hlabel);
                                   emitl(A_LABEL,falselabel);
                                   exprasmlist^.concat(new(pai68k,op_const_reg(A_MOVE,S_W,0,R_SPPUSH)));
                                   emitl(A_LABEL,hlabel);
                                end;
                 LOC_FLAGS    : begin
                                   exprasmlist^.concat(new(pai68k,op_reg(flag_2_set[p^.left^.location.resflags],S_B,
                                     R_D0)));
                                   exprasmlist^.concat(new(pai68k,op_reg(A_NEG, S_B, R_D0)));
                                   exprasmlist^.concat(new(pai68k,op_const_reg(A_AND,S_W,$ff, R_D0)));
                                   inc(pushedparasize,2);
                                   exprasmlist^.concat(new(pai68k,op_reg_reg(A_MOVE,S_W,R_D0,R_SPPUSH)));
                                end;
                end;
           end;
         truelabel:=otlabel;
         falselabel:=oflabel;
         { push from right to left }
         if not push_from_left_to_right and assigned(p^.right) then
           secondcallparan(p^.right,defcoll^.next,push_from_left_to_right);
      end;

    procedure secondcalln(var p : ptree);

      var
         unusedregisters : tregisterset;
         pushed : tpushed;
         funcretref : treference;
         hregister : tregister;
         oldpushedparasize : longint;
         { true if a5 must be loaded again after the subroutine }
         loada5 : boolean;
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
         { temp register allocation }
         reg: tregister;
         { help reference pointer }
         ref: preference;

      label
         dont_call;

      begin
         extended_new:=false;
         iolabel:=nil;
         loada5:=true;
         no_virtual_call:=false;
         unusedregisters:=unused;
         if not assigned(p^.procdefinition) then
           exit;
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
              pushusedregisters(pushed,$ffff);
              usedinproc:=$ffff;

              { no IO check for methods and procedure variables }
              iolabel:=nil;
           end;

         { generate the code for the parameter and push them }
         oldpushedparasize:=pushedparasize;
         pushedparasize:=0;
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
              gettempofsizereference(p^.procdefinition^.retdef^.size,funcretref);
           end;
         if assigned(p^.left) then
           begin
              pushedparasize:=0;
              { be found elsewhere }
              if assigned(p^.right) then
                secondcallparan(p^.left,pprocvardef(p^.right^.resulttype)^.para1,
                  (p^.procdefinition^.options and poleftright)<>0)
              else
                secondcallparan(p^.left,p^.procdefinition^.para1,
                  (p^.procdefinition^.options and poleftright)<>0);
           end;
         params:=p^.left;
         p^.left:=nil;
         if ret_in_param(p^.resulttype) then
           begin
              emitpushreferenceaddr(funcretref);
              inc(pushedparasize,4);
           end;
         { overloaded operator have no symtable }
         if (p^.right=nil) then
           begin
              { push self }
              if assigned(p^.symtable) and
                (p^.symtable^.symtabletype=withsymtable) then
                begin
                   { dirty trick to avoid the secondcall below }
                   p^.methodpointer:=genzeronode(callparan);
                   p^.methodpointer^.location.loc:=LOC_REGISTER;
                   p^.methodpointer^.location.register:=R_A5;
                   { make a reference }
                   new(r);
                   reset_reference(r^);
                   r^.offset:=p^.symtable^.datasize;
                   r^.base:=procinfo.framepointer;
                   exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,S_L,r,R_A5)));
                end;

              { push self }
              if assigned(p^.symtable) and
                ((p^.symtable^.symtabletype=objectsymtable) or
                (p^.symtable^.symtabletype=withsymtable)) then
                begin
                   if assigned(p^.methodpointer) then
                     begin
                        case p^.methodpointer^.treetype of
                           typen : begin
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
                                    { well lets put the VMT address directly into a5 }
                                    { it is kind of dirty but that is the simplest    }
                                    { way to accept virtual static functions (PM)     }
                                    loada5:=true;
                                    exprasmlist^.concat(new(pai68k,op_csymbol_reg(A_MOVE,S_L,
                                      newcsymbol(pobjectdef(p^.methodpointer^.resulttype)^.vmt_mangledname,0),R_A5)));
                                    concat_external(pobjectdef(p^.methodpointer^.resulttype)^.vmt_mangledname,EXT_NEAR);
                                    exprasmlist^.concat(new(pai68k,op_reg_reg(A_MOVE,S_L,R_A5,R_SPPUSH)));
                                 end
                               else

                                  { this is a member call, so A5 isn't modfied }
                                  loada5:=false;

                               if not(is_con_or_destructor and
                                  pobjectdef(p^.methodpointer^.resulttype)^.isclass and
                                  assigned(aktprocsym) and
                                  ((aktprocsym^.definition^.options and
                                  (poconstructor or podestructor))<>0)) then
                                        exprasmlist^.concat(new(pai68k,op_reg_reg(A_MOVE,S_L,R_A5,R_SPPUSH)));
                                 { if an inherited con- or destructor should be  }
                                 { called in a con- or destructor then a warning }
                                 { will be made                                  }
                                 { con- and destructors need a pointer to the vmt }
                                 if is_con_or_destructor and
                                   ((pobjectdef(p^.methodpointer^.resulttype)^.options and oois_class)=0) and
                                   assigned(aktprocsym) then
                                   begin
                                    if not ((aktprocsym^.definition^.options
                                      and (poconstructor or podestructor))<>0) then
                                        Message(cg_w_member_cd_call_from_method);
                                   end;
                                      { con- and destructors need a pointer to the vmt }
                                      if is_con_or_destructor then
                                        begin
                                           { classes need the mem ! }
                                           if ((pobjectdef(p^.methodpointer^.resulttype)^.options and

                                            oois_class)=0) then
                                             push_int(0)
                                           else
                                               begin
                                                  exprasmlist^.concat(new(pai68k,op_csymbol_reg(A_MOVE,
                                                   S_L,newcsymbol(pobjectdef(p^.methodpointer^.
                                                   resulttype)^.vmt_mangledname,0),R_SPPUSH)));
                                                   concat_external(pobjectdef(p^.methodpointer^.resulttype)^.
                                                  vmt_mangledname,EXT_NEAR);
                                               end;
                                        end;
                                   end;
                           hnewn : begin
                                     { extended syntax of new }
                                     { A5 must be zero }
                                     exprasmlist^.concat(new(pai68k,op_const_reg(A_MOVE,S_L,0,R_A5)));
                                     emit_reg_reg(A_MOVE,S_L,R_A5, R_SPPUSH);
                                     { insert the vmt }
                                     exprasmlist^.concat(new(pai68k,op_csymbol_reg(A_MOVE,S_L,
                                       newcsymbol(pobjectdef(p^.methodpointer^.resulttype)^.vmt_mangledname,0),R_SPPUSH)));
                                     concat_external(pobjectdef(p^.methodpointer^.resulttype)^.vmt_mangledname,EXT_NEAR);
                                              extended_new:=true;
                                  end;
                           hdisposen : begin
                                          secondpass(p^.methodpointer);

                                          { destructor with extended syntax called from dispose }
                                          { hdisposen always deliver LOC_REFRENZ }
                                          exprasmlist^.concat(new(pai68k,op_ref_reg(A_LEA,S_L,
                                            newreference(p^.methodpointer^.location.reference),R_A5)));
                                          del_reference(p^.methodpointer^.location.reference);
                                          exprasmlist^.concat(new(pai68k,op_reg_reg(A_MOVE,S_L,R_A5,R_SPPUSH)));
                                          exprasmlist^.concat(new(pai68k,op_csymbol_reg(A_MOVE,S_L,
                                            newcsymbol(pobjectdef
                                               (p^.methodpointer^.resulttype)^.vmt_mangledname,0),R_SPPUSH)));
                                          concat_external(pobjectdef(p^.methodpointer^.resulttype)^.vmt_mangledname,EXT_NEAR);
                                       end;
                           else
                             begin
                                { call to a instance member }
                                if (p^.symtable^.symtabletype<>withsymtable) then
                                  begin
                                     secondpass(p^.methodpointer);


                                     case p^.methodpointer^.location.loc of
                                        LOC_REGISTER :
                                           begin
                                             ungetregister32(p^.methodpointer^.location.register);
                                             emit_reg_reg(A_MOVE,S_L,p^.methodpointer^.location.register,R_A5);
                                           end;
                                        else
                                           begin
                                                 if (p^.methodpointer^.resulttype^.deftype=objectdef) and
                                                   pobjectdef(p^.methodpointer^.resulttype)^.isclass then
                                                   exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,S_L,
                                                     newreference(p^.methodpointer^.location.reference),R_A5)))
                                                 else
                                                   exprasmlist^.concat(new(pai68k,op_ref_reg(A_LEA,S_L,
                                                     newreference(p^.methodpointer^.location.reference),R_A5)));

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
                                         r^.base:=R_A5;
                                         exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,S_L,r,R_A5)));
                                      end;

                                   exprasmlist^.concat(new(pai68k,op_reg_reg(A_MOVE,S_L,R_A5,R_SPPUSH)));
                                   if is_con_or_destructor then
                                   begin
                                         { classes don't get a VMT pointer pushed }
                                         if (p^.methodpointer^.resulttype^.deftype=objectdef) and
                                           not(pobjectdef(p^.methodpointer^.resulttype)^.isclass) then
                                           begin

                                            if ((p^.procdefinition^.options and poconstructor)<>0) then
                                              begin
                                               { it's no bad idea, to insert the VMT }
                                                      exprasmlist^.concat(new(pai68k,op_csymbol_reg(A_MOVE,S_L,
                                               newcsymbol(pobjectdef(
                                                 p^.methodpointer^.resulttype)^.vmt_mangledname,0),R_SPPUSH)));
                                               concat_external(pobjectdef(
                                                 p^.methodpointer^.resulttype)^.vmt_mangledname,EXT_NEAR);
                                              end
                                            { destructors haven't to dispose the instance, if this is }
                                            { a direct call                                           }
                                            else
                                              push_int(0);
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
                             r^.base:=R_A5;
                             exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,S_L,r,R_A5)));
                          end
                        else
                          begin
                             { member call, A5 isn't modified }
                             loada5:=false;
                          end;
                        exprasmlist^.concat(new(pai68k,op_reg_reg(A_MOVE,S_L,R_A5,R_SPPUSH)));
            { but a con- or destructor here would probably almost }
                        { always be placed wrong }
                        if is_con_or_destructor then
                          begin
                             Message(cg_w_member_cd_call_from_method);
                             { not insert VMT pointer }                             { VMT-Zeiger nicht eintragen }
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
                        exprasmlist^.concat(new(pai68k,op_reg(A_PUSH,S_L,R_ESI)));
                     end;
                   }
                   if lexlevel=(p^.procdefinition^.parast^.symtablelevel) then
                     begin
                        new(r);
                        reset_reference(r^);
                        r^.offset:=procinfo.framepointer_offset;
                        r^.base:=procinfo.framepointer;
                        exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,S_L,r,R_SPPUSH)))
                     end
                     { this is only true if the difference is one !!
                       but it cannot be more !! }
                   else if lexlevel=(p^.procdefinition^.parast^.symtablelevel)-1 then
                     begin
                        exprasmlist^.concat(new(pai68k,op_reg_reg(A_MOVE,S_L,procinfo.framepointer,R_SPPUSH)))
                     end
                   else if lexlevel>(p^.procdefinition^.parast^.symtablelevel) then
                     begin
                        hregister:=getaddressreg;
                        new(r);
                        reset_reference(r^);
                        r^.offset:=procinfo.framepointer_offset;
                        r^.base:=procinfo.framepointer;
                        exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,S_L,r,hregister)));
                        for i:=(p^.procdefinition^.parast^.symtablelevel) to lexlevel-1 do
                          begin
                             new(r);
                             reset_reference(r^);
                             {we should get the correct frame_pointer_offset at each level
                             how can we do this !!! }
                             r^.offset:=procinfo.framepointer_offset;
                             r^.base:=hregister;
                             exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,S_L,r,hregister)));
                          end;
                        exprasmlist^.concat(new(pai68k,op_reg_reg(A_MOVE,S_L,hregister,R_SPPUSH)));
                        ungetregister32(hregister);
                     end
                   else
                     internalerror(25000);
                end;

              { exported methods should be never called direct }
              if (p^.procdefinition^.options and poexports)<>0 then
               Message(cg_e_dont_call_exported_direct);

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
                        { A5 is already loaded  }
                        ((p^.procdefinition^.options and poclassmethod)<>0)then
                         begin
                            new(r);
                            reset_reference(r^);
                            r^.base:=R_a5;
                         end
                       else
                         begin
                            new(r);
                            reset_reference(r^);
                            r^.base:=R_a5;
                            exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,S_L,r,R_a0)));
                            new(r);
                            reset_reference(r^);
                            r^.base:=R_a0;
                         end;
                     end
                   else
                     begin
                       new(r);
                       reset_reference(r^);
                         r^.base:=R_a5;
                       exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,S_L,r,R_a0)));
                       new(r);
                       reset_reference(r^);
                       r^.base:=R_a0;
                     end;
                  if p^.procdefinition^.extnumber=-1 then
                        internalerror($Da);
                  r^.offset:=p^.procdefinition^.extnumber*4+12;
                  if (cs_rangechecking in aktswitches) then
                    begin
                        exprasmlist^.concat(new(pai68k,op_reg_reg(A_MOVE,S_L,r^.base,R_SPPUSH)));
                        emitcall('CHECK_OBJECT',true);
                    end;
                  exprasmlist^.concat(new(pai68k,op_ref(A_JSR,S_NO,r)));
                end
              else
                emitcall(p^.procdefinition^.mangledname,
                  p^.symtableproc^.symtabletype=unitsymtable);
              if ((p^.procdefinition^.options and poclearstack)<>0) then
                begin
                   if (pushedparasize > 0) and (pushedparasize < 9) then
                     { restore the stack, to its initial value }
                     exprasmlist^.concat(new(pai68k,op_const_reg(A_ADDQ,S_L,pushedparasize,R_SP)))
                   else
                     { restore the stack, to its initial value }
                     exprasmlist^.concat(new(pai68k,op_const_reg(A_ADDA,S_L,pushedparasize,R_SP)));
                end;
           end
         else
           begin
              secondpass(p^.right);
              case p^.right^.location.loc of
                 LOC_REGISTER,
                 LOC_CREGISTER : begin
                                   if p^.right^.location.register in [R_D0..R_D7] then
                                    begin
                                       reg := getaddressreg;
                                       emit_reg_reg(A_MOVE,S_L,p^.right^.location.register,reg);
                                       new(ref);
                                       reset_reference(ref^);
                                       ref^.base := reg;
                                       exprasmlist^.concat(new(pai68k,op_ref(A_JSR,S_NO,ref)));
                                       ungetregister(reg);
                                    end
                                   else
                                    begin
                                        new(ref);
                                        reset_reference(ref^);
                                        ref^.base := p^.right^.location.register;
                                        exprasmlist^.concat(new(pai68k,op_ref(A_JSR,S_NO,ref)));
                                    end;
                                   ungetregister32(p^.right^.location.register);
                                end
                 else
                    begin
                      if assigned(p^.right^.location.reference.symbol) then
                      { Here we have a symbolic name to the routine, so solve  }
                      { problem by loading the address first, and then emitting }
                      { the call.                                              }
                       begin
                         exprasmlist^.concat(new(pai68k,op_ref_reg(A_LEA,S_L,
                           newreference(p^.right^.location.reference),R_A1)));
                         new(ref);
                         reset_reference(ref^);
                         ref^.base := R_A1;
                         exprasmlist^.concat(new(pai68k,op_ref(A_JSR,S_NO,newreference(ref^))));
                       end
                       else
                         exprasmlist^.concat(new(pai68k,op_ref(A_JSR,S_NO,newreference(p^.right^.location.reference))));
                       del_reference(p^.right^.location.reference);
                    end;
              end;
           end;
      dont_call:
         pushedparasize:=oldpushedparasize;
         unused:=unusedregisters;

         { handle function results }
         if p^.resulttype<>pdef(voiddef) then
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
                               hregister:=getregister32;
                               emit_reg_reg(A_MOVE,S_L,R_D0,hregister);
                               p^.location.register:=hregister;
                     end;
                end
              { structed results are easy to handle.... }
              else if ret_in_param(p^.resulttype) then
                begin
                   p^.location.loc:=LOC_MEM;
                   stringdispose(p^.location.reference.symbol);
                   p^.location.reference:=funcretref;
                end
              else
                begin
                   if (p^.resulttype^.deftype=orddef) then
                     begin
                        p^.location.loc:=LOC_REGISTER;
                  case porddef(p^.resulttype)^.typ of
                     s32bit,u32bit :
                        begin
                             hregister:=getregister32;
                             emit_reg_reg(A_MOVE,S_L,R_D0,hregister);
                             p^.location.register:=hregister;
                        end;
                     uchar,u8bit,bool8bit,s8bit :
                        begin
                            hregister:=getregister32;
                            emit_reg_reg(A_MOVE,S_B,R_D0,hregister);
                            p^.location.register:=hregister;
                        end;
                     s16bit,u16bit :
                       begin
                           hregister:=getregister32;
                           emit_reg_reg(A_MOVE,S_L,R_D0,hregister);
                           p^.location.register:=hregister;
                       end;
                           else internalerror(7);
                        end
                     end
                   else if (p^.resulttype^.deftype=floatdef) then
                      case pfloatdef(p^.resulttype)^.typ of
                           f32bit :
                              begin
                                p^.location.loc:=LOC_REGISTER;
                                hregister:=getregister32;
                                emit_reg_reg(A_MOVE,S_L,R_D0,hregister);
                                p^.location.register:=hregister;
                      end;
                     s32real,s64bit,s64real,s80real: begin
                                              if cs_fp_emulation in aktswitches then
                                              begin
                                                p^.location.loc:=LOC_FPU;
                                                      hregister:=getregister32;
                                                emit_reg_reg(A_MOVE,S_L,R_D0,hregister);
                                                p^.location.fpureg:=hregister;
                                              end
                                              else
                                              begin
                                                { TRUE FPU mode }
                                                p^.location.loc:=LOC_FPU;
                                                { on exit of function result in R_FP0 }
                                                p^.location.fpureg:=R_FP0;
                                              end;
                                             end;
                           else
                      begin
                              p^.location.loc:=LOC_FPU;
                              p^.location.fpureg:=R_FP0;
                      end;
             end {end case }
       else
        begin
            p^.location.loc:=LOC_REGISTER;
            hregister:=getregister32;
            emit_reg_reg(A_MOVE,S_L,R_D0,hregister);
            p^.location.register:=hregister;
                end;
           end;
         end;
         { perhaps i/o check ? }
         if iolabel<>nil then
           begin
              exprasmlist^.concat(new(pai68k,op_csymbol_reg(A_MOVE,S_L,newcsymbol(lab2str(iolabel),0),R_SPPUSH)));
              { this was wrong, probably an error due to diff3
              emitcall(p^.procdefinition^.mangledname);}
              emitcall('IOCHECK',true);
           end;

         { restore registers }
         popusedregisters(pushed);

         { at last, restore instance pointer (SELF) }
         if loada5 then
           maybe_loada5;
         pp:=params;
         while assigned(pp) do
           begin
             if assigned(pp^.left) then
               if (pp^.left^.location.loc=LOC_REFERENCE) or
                 (pp^.left^.location.loc=LOC_MEM) then
                 ungetiftemp(pp^.left^.location.reference);
               pp:=pp^.right;
           end;
         disposetree(params);
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


    procedure secondloadvmt(var p : ptree);

      begin
         p^.location.loc:=LOC_REGISTER;
         p^.location.register:=getregister32;
         exprasmlist^.concat(new(pai68k,op_csymbol_reg(A_MOVE,
            S_L,newcsymbol(pobjectdef(pclassrefdef(p^.resulttype)^.definition)^.vmt_mangledname,0),
            p^.location.register)));
      end;


    procedure secondinline(var p : ptree);
    const   in2size:array[in_inc_byte..in_dec_dword] of Topsize=
                    (S_B,S_W,S_L,S_B,S_W,S_L);
            in2instr:array[in_inc_byte..in_dec_dword] of Tasmop=
                    (A_ADDQ,A_ADDQ,A_ADDQ,A_SUBQ,A_SUBQ,A_SUBQ);
            { tfloattype = (f32bit,s32real,s64real,s80real,s64bit); }
            float_name: array[tfloattype] of string[8]=
             {   ('FIXED','SINGLE','REAL','EXTENDED','COMP','FIXED'); }
             {  Since we only support the REAL (SINGLE IEEE) FLOAT    }
             {  type, here is what we do...                           }
                ('FIXED','REAL','REAL','REAL','COMP','FIXED');
      var
         opsize: topsize;
         asmop: tasmop;

         aktfile : treference;
         ft : tfiletype;
         pushed : tpushed;
         dummycoll : tdefcoll;

      { produces code for READ(LN) and WRITE(LN) }

      procedure handlereadwrite(doread,callwriteln : boolean);

        procedure loadstream;

          const io:array[0..1] of string[7]=('_OUTPUT','_INPUT');
          var     r : preference;

          begin
              new(r);
              reset_reference(r^);
              r^.symbol:=stringdup('U_'+upper(target_info.system_unit)+io[byte(doread)]);
           if not (cs_compilesystem in aktswitches) then
                 concat_external(r^.symbol^,EXT_NEAR);

              exprasmlist^.concat(new(pai68k,op_ref_reg(A_LEA,S_L,r,R_A0)))
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
                exprasmlist^.concat(new(pai68k,op_reg_ref(A_MOVE,S_L,R_A0,newreference(aktfile))));
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

                     exprasmlist^.concat(new(pai68k,op_ref_reg(A_LEA,S_L,newreference(node^.left^.location.reference),R_A0)));

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
                exprasmlist^.concat(new(pai68k,op_reg_ref(A_MOVE,S_L,R_A0,newreference(aktfile))));
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
                     { use var for write also }
                     { avoids problems with const passed by value }
                     { but will not accept untyped const }
                     { dummycoll.paratyp:=vs_var; }
                  end
                else
                  { I think, this isn't a good solution (FK) }
                  dummycoll.data:=nil;

                while assigned(node) do
                  begin
                     pushusedregisters(pushed,$ffff);
                     hp:=node;
                          node:=node^.right;
                     hp^.right:=nil;
                     if hp^.is_colon_para then
                       Message(parser_e_illegal_colon_qualifier);
                     if hp^.is_colon_para then
                       Message(parser_e_illegal_colon_qualifier);
                     if ft=ft_typed then
                       never_copy_const_param:=true;
                     secondcallparan(hp,@dummycoll,false);
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
                                             secondcallparan(hp,@dummycoll,false);
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
                                    secondcallparan(hp,@dummycoll,false);
                                             hp^.right:=node;
                                    if pararesult^.deftype<>floatdef then
                                     Message(parser_e_illegal_colon_qualifier);
                                    if codegenerror then
                                      exit;
                              end
                                     else
                              begin
                                if hp^.left^.resulttype^.deftype=floatdef then
                                  push_int(-1);
                              end;
                            end;
                          case pararesult^.deftype of
                              stringdef : begin
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
                                             else Message(parser_e_illegal_parameter_list);
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
                                           else Message(parser_e_illegal_parameter_list);
                                        end;
                      floatdef : begin
                                      if doread then
                                          emitcall('READ_TEXT_REAL',true)
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
                     { load A5 in methods again }
                     popusedregisters(pushed);
                     maybe_loada5;
                  end;
             end;
           if callwriteln then
             begin
                pushusedregisters(pushed,$ffff);
                emit_push_mem(aktfile);
                { pushexceptlabel; }
                if ft<>ft_text then
                  Message(parser_e_illegal_parameter_list);
                     emitcall('WRITELN_TEXT',true);
                     popusedregisters(pushed);
                     maybe_loada5;
                 end;
           if doflush and not(doread) then
             begin
               pushusedregisters(pushed,$ffff);
               { pushexceptlabel; }
               emitcall('FLUSH_STDOUT',true);
               popusedregisters(pushed);
               maybe_loada5;
             end;
           if iolabel<>nil then
             begin
                { registers are saved in the procedure }
                exprasmlist^.concat(new(pai68k,op_csymbol_reg(A_MOVE,S_L,newcsymbol(lab2str(iolabel),0),R_SPPUSH)));
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
           hp,node,lentree,paratree : ptree;
           dummycoll : tdefcoll;
           is_real,has_length : boolean;
           real_type : byte;

          begin
           pushusedregisters(pushed,$ffff);
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
           secondcallparan(hp,@dummycoll,false);
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
                secondcallparan(hp,@dummycoll,false);
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
                secondcallparan(hp,@dummycoll,false);
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
           secondcallparan(hp,@dummycoll,false);
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

      begin
         case p^.inlinenumber of
            in_lo_word,
            in_hi_word : begin
                       secondpass(p^.left);
                       p^.location.loc:=LOC_REGISTER;
                       if p^.left^.location.loc<>LOC_REGISTER then
                         begin
                            if p^.left^.location.loc=LOC_CREGISTER then
                              begin
                                 p^.location.register:=getregister32;
                                 emit_reg_reg(A_MOVE,S_W,p^.left^.location.register,
                                   p^.location.register);
                              end
                            else
                              begin
                                 del_reference(p^.left^.location.reference);
                                 p^.location.register:=getregister32;
                                 exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,S_W,
                                  newreference(p^.left^.location.reference),
                                  p^.location.register)));
                              end;
                         end
                       else p^.location.register:=p^.left^.location.register;
                       if p^.inlinenumber=in_hi_word then
                         exprasmlist^.concat(new(pai68k,op_const_reg(A_LSR,S_W,8,p^.location.register)));
                       p^.location.register:=p^.location.register;
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
                      exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,S_L,
                        r,p^.location.register)));
                   end
              end;
          in_sizeof_x,
          in_typeof_x:
                begin
                   { load vmt }
                   if p^.left^.treetype=typen then
                     begin
                      p^.location.register:=getregister32;
                      exprasmlist^.concat(new(pai68k,op_csymbol_reg(A_MOVE,
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
                        exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,S_L,
                          newreference(p^.left^.location.reference),
                          p^.location.register)));
                     end;

                   { in sizeof load size }
                   if p^.inlinenumber=in_sizeof_x then
                     begin
                        new(r);
                        reset_reference(r^);
                        { load the address in A0 }
                        { because now supposedly p^.location.register is an }
                        { address.                                          }
                        emit_reg_reg(A_MOVE, S_L, p^.location.register, R_A0);
            r^.base:=R_A0;
                        exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,S_L,r,
                          p^.location.register)));
                     end;
                end;
            in_lo_long,
            in_hi_long : begin
                       secondpass(p^.left);
                       p^.location.loc:=LOC_REGISTER;
                       if p^.left^.location.loc<>LOC_REGISTER then
                         begin
                            if p^.left^.location.loc=LOC_CREGISTER then
                              begin
                                 p^.location.register:=getregister32;
                                 emit_reg_reg(A_MOVE,S_L,p^.left^.location.register,
                                   p^.location.register);
                              end
                            else
                              begin
                                 del_reference(p^.left^.location.reference);
                                 p^.location.register:=getregister32;
                                 exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,S_L,
                                  newreference(p^.left^.location.reference),
                                  p^.location.register)));
                              end;
                         end
                       else p^.location.register:=p^.left^.location.register;
                       if p^.inlinenumber=in_hi_long then
                         begin
                           exprasmlist^.concat(new(pai68k,op_const_reg(A_MOVEQ, S_L, 16, R_D1)));
                           exprasmlist^.concat(new(pai68k,op_reg_reg(A_LSR,S_L,R_D1,p^.location.register)));
                         end;
                       p^.location.register:=p^.location.register;
                    end;
{We can now comment them out, as they are handled as typecast.
 Saves an incredible amount of 8 bytes code.
  I'am not lucky about this, because it's _not_ a type cast (FK) }
{           in_ord_char,
            in_chr_byte,}
            in_length_string : begin
                       secondpass(p^.left);
                       set_location(p^.location,p^.left^.location);
                    end;
            in_inc_byte..in_dec_dword:
                begin
                    secondpass(p^.left);
                    exprasmlist^.concat(new(pai68k,op_const_ref(in2instr[p^.inlinenumber],
                     in2size[p^.inlinenumber],1,newreference(p^.left^.location.reference))));
                     emitoverflowcheck(p^.left);
                end;
            in_pred_x,
            in_succ_x:
              begin
                 secondpass(p^.left);
                 if p^.inlinenumber=in_pred_x then
                   asmop:=A_SUB
                 else
                   asmop:=A_ADD;
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
                      if p^.left^.location.loc=LOC_CREGISTER then
                        emit_reg_reg(A_MOVE,opsize,p^.left^.location.register,
                          p^.location.register)
                      else
                      if p^.left^.location.loc=LOC_FLAGS then
                        exprasmlist^.concat(new(pai68k,op_reg(flag_2_set[p^.left^.location.resflags],S_NO,
                                  p^.location.register)))
                      else
                        begin
                           del_reference(p^.left^.location.reference);
                           exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,opsize,newreference(p^.left^.location.reference),
                             p^.location.register)));
                        end;
                   end
                 else p^.location.register:=p^.left^.location.register;
                 exprasmlist^.concat(new(pai68k,op_reg(asmop,opsize,
                   p^.location.register)))
                 { here we should insert bounds check ? }
                 { and direct call to bounds will crash the program }
                 { if we are at the limit }
                 { we could also simply say that pred(first)=first and succ(last)=last }
                 { could this be usefull I don't think so (PM)
                 emitoverflowcheck;}
              end;

        in_assigned_x:
              begin
         secondpass(p^.left^.left);
         p^.location.loc:=LOC_FLAGS;
         if (p^.left^.left^.location.loc=LOC_REGISTER) or
           (p^.left^.left^.location.loc=LOC_CREGISTER) then
           begin
              exprasmlist^.concat(new(pai68k,op_reg(A_TST,S_L,
                p^.left^.left^.location.register)));
              ungetregister32(p^.left^.left^.location.register);
           end
         else
           begin
              exprasmlist^.concat(new(pai68k,op_ref(A_TST,S_L,
              newreference(p^.left^.left^.location.reference))));
              del_reference(p^.left^.left^.location.reference);
           end;
         p^.location.resflags:=F_NE;
          end;
             in_reset_typedfile,in_rewrite_typedfile :
               begin
                  pushusedregisters(pushed,$ffff);
                  exprasmlist^.concat(new(pai68k,op_const_reg(A_MOVE,S_L,
                    pfiledef(p^.left^.resulttype)^.typed_as^.size,R_SPPUSH)));
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
                   pushusedregisters(pushed,$ffff);
                   emit_push_mem(aktfile);
                   { pushexceptlabel; }
                   if ft<>ft_text then
                     Message(parser_e_illegal_parameter_list);
                       emitcall('READLN_TEXT',true);
                       popusedregisters(pushed);
                   maybe_loada5;
                end;
            in_str_x_string : begin
                                 handle_str;
                                 maybe_loada5;
                              end;
            else internalerror(9);
         end;
      end;

    procedure secondsubscriptn(var p : ptree);
      var
       hr: tregister;

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
                  begin
                     { move it to an address register...}
                     hr:=getaddressreg;
                     emit_reg_reg(A_MOVE,S_L,p^.left^.location.register,hr);
                     p^.location.reference.base:=hr;
                     { free register }
                     ungetregister(p^.left^.location.register);
                  end;
                LOC_CREGISTER:
                  begin
                     { ... and reserve one for the pointer }
                     hr:=getaddressreg;
                     emit_reg_reg(A_MOVE,S_L,p^.left^.location.register,hr);
                       p^.location.reference.base:=hr;
                  end;
                else
                  begin
                     { free register }
                     del_reference(p^.left^.location.reference);

                     { ... and reserve one for the pointer }
                     hr:=getaddressreg;
                     exprasmlist^.concat(new(pai68k,op_ref_reg(
                       A_MOVE,S_L,newreference(p^.left^.location.reference),
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
         p^.location.reference.base:=R_A5;
      end;

    procedure secondhdisposen(var p : ptree);

      begin
         secondpass(p^.left);

         if codegenerror then
           exit;
         clear_reference(p^.location.reference);
         case p^.left^.location.loc of
            LOC_REGISTER,
            LOC_CREGISTER : begin
                               p^.location.reference.index:=getregister32;
                               exprasmlist^.concat(new(pai68k,op_reg_reg(A_MOVE,S_L,
                                 p^.left^.location.register,
                                 p^.location.reference.index)));
                            end;
            LOC_MEM,LOC_REFERENCE :
                            begin
                               del_reference(p^.left^.location.reference);
                               p^.location.reference.index:=getregister32;
                               exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,S_L,newreference(p^.left^.location.reference),
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

         pushusedregisters(pushed,$ffff);
         { determines the size of the mem block }
         push_int(ppointerdef(p^.left^.resulttype)^.definition^.size);

         { push pointer adress }
         case p^.left^.location.loc of
            LOC_CREGISTER : exprasmlist^.concat(new(pai68k,op_reg_reg(A_MOVE,S_L,
              p^.left^.location.register,R_SPPUSH)));
            LOC_REFERENCE : emitpushreferenceaddr(p^.left^.location.reference);

         end;

         { call the mem handling procedures }
         case p^.treetype of
            simpledisposen :
                emitcall('FREEMEM',true);
            simplenewn :
                emitcall('GETMEM',true);
         end;

       popusedregisters(pushed);
         { may be load ESI }
         maybe_loada5;
      end;


    procedure secondsetcons(var p : ptree);

      var
         l : plabel;
         i,smallsetvalue : longint;
         hp : ptree;
         href,sref : treference;
         hl1,hl2: plabel;

      begin
         { this should be reimplemented for smallsets }
         { differently  (PM) }
         { produce constant part }
         href.symbol := Nil;
         clear_reference(href);
         getlabel(l);
         href.symbol:=stringdup(lab2str(l));
         stringdispose(p^.location.reference.symbol);
         datasegment^.concat(new(pai_label,init(l)));
           {if psetdef(p^.resulttype)=smallset then
           begin
              smallsetvalue:=(p^.constset^[3]*256)+p^.constset^[2];
              smallsetvalue:=((smallset*256+p^.constset^[1])*256+p^.constset^[1];
              datasegment^.concat(new(pai_const,init_32bit(smallsetvalue)));
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
                         register is save in subroutine
                        emitcall('SET_SET_BYTE',true);
                        hp:=hp^.right;
                     end;
                   p^.location.reference:=sref;
                end
              else p^.location.reference:=href;
           end
         else    }
           begin
           for i:=0 to 31 do
             datasegment^.concat(new(pai_const,init_8bit(p^.constset^[i])));
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
                   { here we must set the flags manually  }
                   { on returne from the routine, because }
                   { falgs are corrupt when restoring the }
                   { stack                                }
                   exprasmlist^.concat(new(pai68k,op_reg(A_TST,S_B,R_D0)));
                   getlabel(hl1);
                   emitl(A_BEQ,hl1);
                   exprasmlist^.concat(new(pai68k,op_const_reg(A_AND,S_B,
                       $fe,R_CCR)));
                   getlabel(hl2);
                   emitl(A_BRA,hl2);
                   emitl(A_LABEL,hl1);
                   exprasmlist^.concat(new(pai68k,op_const_reg(A_OR,S_B,
                       $01,R_CCR)));
                   emitl(A_LABEL,hl2);
                   hp:=hp^.right;
                end;
              p^.location.reference:=sref;
           end
         else p^.location.reference:=href;
         end;
      end;


    procedure secondcontinuen(var p : ptree);

      begin
         if aktcontinuelabel<>nil then
           emitl(A_JMP,aktcontinuelabel)
         else
           Message(cg_e_continue_not_allowed);
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
                                exprasmlist^.concat(new(pai68k,op_reg(flag_2_set[p^.right^.location.resflags],S_B,R_D0)));
                                exprasmlist^.concat(new(pai68k,op_reg(A_NEG, S_B, R_D0)));
                                goto do_jmp;
                             end;
                 LOC_JUMP : begin
                               emitl(A_LABEL,truelabel);
                               exprasmlist^.concat(new(pai68k,op_const_reg(A_MOVE,S_B,1,R_D0)));
                               emitl(A_JMP,aktexit2label);
                               exprasmlist^.concat(new(pai68k,op_reg(A_CLR,S_B,R_D0)));
                               goto do_jmp;
                            end;
                 else internalerror(2001);
              end;
              if (procinfo.retdef^.deftype=orddef) then
                begin
                   case porddef(procinfo.retdef)^.typ of
                      s32bit,u32bit : if is_mem then
                                        exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,S_L,
                                          newreference(p^.left^.location.reference),R_D0)))
                                      else
                                        emit_reg_reg(A_MOVE,S_L,
                                          p^.left^.location.register,R_D0);
                      u8bit,s8bit,uchar,bool8bit : if is_mem then
                                        exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,S_B,
                                          newreference(p^.left^.location.reference),R_D0)))
                                      else
                                        emit_reg_reg(A_MOVE,S_B,
                                          p^.left^.location.register,R_D0);
                      s16bit,u16bit : if is_mem then
                                        exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,S_W,
                                          newreference(p^.left^.location.reference),R_D0)))
                                      else
                                        emit_reg_reg(A_MOVE,S_W,
                                          p^.left^.location.register,R_D0);
                   end;
                end
               else
                 if (procinfo.retdef^.deftype in
                     [pointerdef,enumdef,procvardef]) then
                   begin
                      if is_mem then
                        exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,S_L,
                          newreference(p^.left^.location.reference),R_D0)))
                      else
                        exprasmlist^.concat(new(pai68k,op_reg_reg(A_MOVE,S_L,
                          p^.left^.location.register,R_D0)));
                   end
              else
                if (procinfo.retdef^.deftype=floatdef) then
            { floating point return values .... }
            { single are returned in d0         }
                  begin
                     if (pfloatdef(procinfo.retdef)^.typ=f32bit) or
                   (pfloatdef(procinfo.retdef)^.typ=s32real) then
                       begin
                          if is_mem then
                            exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,S_L,
                              newreference(p^.left^.location.reference),R_D0)))
                    else
                    begin
                      if pfloatdef(procinfo.retdef)^.typ=f32bit then
                        emit_reg_reg(A_MOVE,S_L,p^.left^.location.register,R_D0)
                      else
                      begin
                        { single values are in the floating point registers }
                        if cs_fp_emulation in aktswitches then
                          emit_reg_reg(A_MOVE,S_L,p^.left^.location.fpureg,R_D0)
                        else
                          exprasmlist^.concat(
                             new(pai68k,op_reg_reg(A_FMOVE,S_FS,p^.left^.location.fpureg,R_D0)));
                      end;
                    end;
                  end
                  else
                    { this is only possible in real non emulation mode }
                    { LOC_MEM,LOC_REFERENCE }
                    if is_mem then
                    begin
                          exprasmlist^.concat(new(pai68k,op_ref_reg(A_FMOVE,
                             getfloatsize(pfloatdef(procinfo.retdef)^.typ),newreference(p^.left^.location.reference),R_FP0)));
                    end
                    else
                    { LOC_FPU }
                    begin
                          { convert from extended to correct type }
                          { when storing                          }
                          exprasmlist^.concat(new(pai68k,op_reg_reg(A_FMOVE,
                             getfloatsize(pfloatdef(procinfo.retdef)^.typ),p^.left^.location.fpureg,R_FP0)));
                    end;
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
              exprasmlist^.concat(new(pai68k,op_const_reg(A_CMP,opsize,p^._low,hregister)));
              if greaterlabel=lesslabel then
                begin
                   emitl(A_BNE,lesslabel);
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
              exprasmlist^.concat(new(pai68k,op_const_reg(A_CMP,opsize,p^._low,hregister)));
              emitl(jmp_le,lesslabel);
              exprasmlist^.concat(new(pai68k,op_const_reg(A_CMP,opsize,p^._high,hregister)));
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

        procedure genitem(t : pcaserecord);

          begin
             if assigned(t^.less) then
               genitem(t^.less);
             if t^._low=t^._high then
               begin
                  if (t^._low-last > 0) and (t^._low-last < 9) then
                     exprasmlist^.concat(new(pai68k,op_const_reg(A_SUBQ,opsize,t^._low-last,hregister)))
                  else
                  if (t^._low-last = 0) then
                     exprasmlist^.concat(new(pai68k,op_reg(A_TST,opsize,hregister)))
                  else
                     exprasmlist^.concat(new(pai68k,op_const_reg(A_SUB,opsize,t^._low-last,hregister)));
                  last:=t^._low;

                  emitl(A_BEQ,t^.statement);
               end
             else
               begin
                  { it begins with the smallest label, if the value }
                  { is even smaller then jump immediately to the    }
                  { ELSE-label                                      }
                  if first then
                    begin
                       if (t^._low-1 > 0) and (t^._low < 9) then
                          exprasmlist^.concat(new(pai68k,op_const_reg(A_SUBQ,opsize,t^._low-1,hregister)))
                       else
                       if t^._low-1=0 then
                         exprasmlist^.concat(new(pai68k,op_reg(A_TST,opsize,hregister)))
                       else
                         exprasmlist^.concat(new(pai68k,op_const_reg(A_SUB,opsize,t^._low-1,hregister)));
                       if t^._low = 0 then
                          emitl(A_BLE,elselabel)
                       else
                          emitl(jmp_lee,elselabel);
                    end
                  { if there is no unused label between the last and the }
                  { present label then the lower limit can be checked    }
                  { immediately. else check the range in between:        }
                  else if (t^._low-last>1)then

                    begin
                       if ((t^._low-last-1) > 0) and ((t^._low-last-1) < 9) then
                         exprasmlist^.concat(new(pai68k,op_const_reg(A_SUBQ,opsize,t^._low-last-1,hregister)))
                       else
                         exprasmlist^.concat(new(pai68k,op_const_reg(A_SUB,opsize,t^._low-last-1,hregister)));
                       emitl(jmp_lee,elselabel);
                    end;
                  exprasmlist^.concat(new(pai68k,op_const_reg(A_SUB,opsize,t^._high-t^._low+1,hregister)));
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
               datasegment^.concat(new(pai_const,init_symbol(strpnew(lab2str
                 (elselabel)))));
             for i:=t^._low to t^._high do
               datasegment^.concat(new(pai_const,init_symbol(strpnew(lab2str
                (t^.statement)))));
              last:=t^._high;
             if assigned(t^.greater) then
               genitem(t^.greater);
          end;

        begin
           if not(jumptable_no_range) then
             begin
                exprasmlist^.concat(new(pai68k,op_const_reg(A_CMP,opsize,min_,hregister)));
                { case expr less than min_ => goto elselabel }
                emitl(jmp_le,elselabel);
                exprasmlist^.concat(new(pai68k,op_const_reg(A_CMP,opsize,max_,hregister)));
                emitl(jmp_gt,elselabel);
             end;
           getlabel(table);
           { extend with sign }
           if opsize=S_W then
             begin
                { word to long - unsigned }
                exprasmlist^.concat(new(pai68k,op_const_reg(A_AND,S_L,$ffff,hregister)));
             end
           else if opsize=S_B then
             begin
                { byte to long - unsigned }
                exprasmlist^.concat(new(pai68k,op_const_reg(A_AND,S_L,$ff,hregister)));
             end;
           new(hr);
           reset_reference(hr^);
           hr^.symbol:=stringdup(lab2str(table));
           hr^.offset:=(-min_)*4;

           { add scalefactor *4 to index }
           exprasmlist^.concat(new(pai68k,op_const_reg(A_LSL,S_L,2,hregister)));
{           hr^.scalefactor:=4; }
           hr^.base:=getaddressreg;
           emit_reg_reg(A_MOVE,S_L,hregister,hr^.base);
           exprasmlist^.concat(new(pai68k,op_ref(A_JMP,S_NO,hr)));
{          if not(cs_littlesize in aktswitches^ ) then
             datasegment^.concat(new(pai68k,op_const(A_ALIGN,S_NO,4))); }
           datasegment^.concat(new(pai_label,init(table)));
             last:=min_;
           genitem(hp);
           if hr^.base <> R_NO then ungetregister(hr^.base);
           { !!!!!!!
           if not(cs_littlesize in aktswitches^ ) then
             exprasmlist^.concat(new(pai68k,op_const(A_ALIGN,S_NO,4)));
           }
        end;

      var
         lv,hv,min_label,max_label,labels : longint;
         max_linear_list : longint;

      begin
         getlabel(endlabel);
         getlabel(elselabel);
         with_sign:=is_signed(p^.left^.resulttype);
         if with_sign then
           begin
              jmp_gt:=A_BGT;
              jmp_le:=A_BLT;
              jmp_lee:=A_BLE;
           end
         else
           begin
              jmp_gt:=A_BHI;
              jmp_le:=A_BCS;
              jmp_lee:=A_BLS;
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
            LOC_CREGISTER : hregister:=p^.left^.location.register;
            LOC_MEM,LOC_REFERENCE : begin
                                       del_reference(p^.left^.location.reference);
                                           hregister:=getregister32;
                                       exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,opsize,newreference(
                                         p^.left^.location.reference),hregister)));
                                    end;
            else internalerror(2002);
         end;
         { now generate the jumps }
         if cs_optimize in aktswitches  then
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
         { ... and the else block }
         if assigned(p^.elseblock) then
           begin
              cleartempgen;
              secondpass(p^.elseblock);
           end;
         emitl(A_LABEL,endlabel);
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
         exprasmlist^.concat(new(pai68k,op_csymbol(A_CALL,S_NO,newcsymbol('HELP_DESTRUCTOR',0))));
         }
         exprasmlist^.concat(new(pai68k,op_reg(A_CLR,S_L,R_A5)));
         { also reset to zero in the stack }
         new(hp);
         reset_reference(hp^);
         hp^.offset:=procinfo.ESI_offset;
         hp^.base:=procinfo.framepointer;
         exprasmlist^.concat(new(pai68k,op_reg_ref(A_MOVE,S_L,R_A5,hp)));
         exprasmlist^.concat(new(pai_labeled,init(A_JMP,quickexitlabel)));
      end;

    procedure secondas(var p : ptree);

      var
         pushed : tpushed;

      begin
         set_location(p^.location,p^.left^.location);
         { save all used registers }
         pushusedregisters(pushed,$ffff);
         { push the vmt of the class }
         exprasmlist^.concat(new(pai68k,op_csymbol_reg(A_MOVE,
           S_L,newcsymbol(pobjectdef(p^.right^.resulttype)^.vmt_mangledname,0),R_SPPUSH)));
         concat_external(pobjectdef(p^.right^.resulttype)^.vmt_mangledname,EXT_NEAR);
         emitpushreferenceaddr(p^.location.reference);
          emitcall('DO_AS',true);
         popusedregisters(pushed);
      end;

    procedure secondis(var p : ptree);

      var
         pushed : tpushed;

      begin
         { save all used registers }
         pushusedregisters(pushed,$ffff);
         secondpass(p^.left);
         p^.location.loc:=LOC_FLAGS;
         p^.location.resflags:=F_NE;

         { push instance to check: }
         case p^.left^.location.loc of
            LOC_REGISTER,LOC_CREGISTER:
              begin
                 exprasmlist^.concat(new(pai68k,op_reg_reg(A_MOVE,
                   S_L,p^.left^.location.register,R_SPPUSH)));
                 ungetregister32(p^.left^.location.register);
              end;
            LOC_MEM,LOC_REFERENCE:
              begin
                 exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,
                   S_L,newreference(p^.left^.location.reference),R_SPPUSH)));
                 del_reference(p^.left^.location.reference);
              end;
            else internalerror(100);
         end;

         { generate type checking }
         secondpass(p^.right);
         case p^.right^.location.loc of
            LOC_REGISTER,LOC_CREGISTER:
              begin
                 exprasmlist^.concat(new(pai68k,op_reg_reg(A_MOVE,
                   S_L,p^.right^.location.register,R_SPPUSH)));
                 ungetregister32(p^.right^.location.register);
              end;
            LOC_MEM,LOC_REFERENCE:
              begin
                 exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,
                   S_L,newreference(p^.right^.location.reference),R_SPPUSH)));
                 del_reference(p^.right^.location.reference);
              end;
            else internalerror(100);
         end;
         emitcall('DO_IS',true);
         exprasmlist^.concat(new(pai68k,op_reg(A_TST,S_B,R_D0)));
         popusedregisters(pushed);
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
               exprasmlist^.concat(new(pai68k,op_ref_reg(A_LEA,S_L,
                 newreference(p^.left^.location.reference),R_A0)));
               exprasmlist^.concat(new(pai68k,op_reg_ref(A_MOVE,S_L,
                 R_A0,newreference(ref))));
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

procedure secondprocinline(var p:ptree);
begin
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
         oldpos : tfileposinfo;

      begin
         oldcodegenerror:=codegenerror;
         oldswitches:=aktswitches;
         get_cur_file_pos(oldpos);

         codegenerror:=false;
         set_cur_file_pos(p^.fileinfo);
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
         set_cur_file_pos(oldpos);
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
         if (p^.typ=varsym) and ((pvarsym(p)^.var_options and vo_regable)<>0) then
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
         for regi:=R_D0 to R_A6 do
           begin
              reg_pushes[regi]:=0;
              is_reg_var[regi]:=false;
           end;

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
                      if (aktprocsym^.definition^.options and poconstructor+podestructor+poinline+pointerrupt=0) and
                       ((procinfo.flags and pi_do_call)=0) and (lexlevel>1) then
                       begin
                          { use ESP as frame pointer }
                          procinfo.framepointer:=R_SP;
                          use_esp_stackframe:=true;

                          { calc parameter distance new }
                          dec(procinfo.framepointer_offset,4);
                          dec(procinfo.ESI_offset,4);

                          dec(procinfo.retoffset,4);

                          dec(procinfo.call_offset,4);
                          aktprocsym^.definition^.parast^.call_offset:=procinfo.call_offset;
                       end;
                          end; { endif assigned }
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
                        for i:=1 to maxvarregs do
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
                                       regvars[i]^.reg:=varregs[i];
                                       regsize:=S_B;
                                    end
                                  else if  (regvars[i]^.definition^.deftype=orddef) and
                                      (
                                       (porddef(regvars[i]^.definition)^.typ=u16bit) or
                                       (porddef(regvars[i]^.definition)^.typ=s16bit)
                                      ) then
                                    begin
                                       regvars[i]^.reg:=varregs[i];
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
                                       procinfo.aktentrycode^.concat(new(pai68k,op_ref_reg(A_MOVE,regsize,
                                         hr,regvars[i]^.reg)));
                                       unused:=unused - [regvars[i]^.reg];
                                    end;
                                  { procedure uses this register }
                                  usedinproc:=usedinproc or ($800 shr word(varregs[i]));
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

              { all registers can be used again }
              { contains both information on Address registers and data registers }
              { even if they are allocated separately.                            }
              usableregs:=[R_D0,R_D1,R_D2,R_D3,R_D4,R_D5,R_D6,R_D7,R_A0,R_A1,R_A2,R_A3,R_A4,
                  R_FP0,R_FP1,R_FP2,R_FP3,R_FP4,R_FP5,R_FP6,R_FP7];
              c_usableregs:=4;
           end;
         procinfo.aktproccode^.concatlist(exprasmlist);

         current_module^.current_inputfile:=oldis;
         current_module^.current_inputfile^.line_no:=oldnr;
      end;

end.


{
  $Log$
  Revision 1.8  1998-06-12 10:32:22  pierre
    * column problem hopefully solved
    + C vars declaration changed

  Revision 1.7  1998/06/09 16:01:36  pierre
    + added procedure directive parsing for procvars
      (accepted are popstack cdecl and pascal)
    + added C vars with the following syntax
      var C calias 'true_c_name';(can be followed by external)
      reason is that you must add the Cprefix

      which is target dependent

  Revision 1.6  1998/06/08 13:13:36  pierre
    + temporary variables now in temp_gen.pas unit
      because it is processor independent
    * mppc68k.bat modified to undefine i386 and support_mmx
      (which are defaults for i386)

  Revision 1.5  1998/06/04 23:51:34  peter
    * m68k compiles
    + .def file creation moved to gendef.pas so it could also be used
      for win32

  Revision 1.4  1998/04/29 10:33:44  pierre
    + added some code for ansistring (not complete nor working yet)
    * corrected operator overloading
    * corrected nasm output
    + started inline procedures
    + added starstarn : use ** for exponentiation (^ gave problems)
    + started UseTokenInfo cond to get accurate positions

  Revision 1.3  1998/04/07 22:45:03  florian
    * bug0092, bug0115 and bug0121 fixed
    + packed object/class/array

  Revision 1.2  1998/03/28 23:09:54  florian
    * secondin bugfix (m68k and i386)
    * overflow checking bugfix (m68k and i386) -- pretty useless in
      secondadd, since everything is done using 32-bit
    * loading pointer to routines hopefully fixed (m68k)
    * flags problem with calls to RTL internal routines fixed (still strcmp
      to fix) (m68k)
    * #ELSE was still incorrect (didn't take care of the previous level)
    * problem with filenames in the command line solved
    * problem with mangledname solved
    * linking name problem solved (was case insensitive)
    * double id problem and potential crash solved
    * stop after first error
    * and=>test problem removed
    * correct read for all float types
    * 2 sigsegv fixes and a cosmetic fix for Internal Error
    * push/pop is now correct optimized (=> mov (%esp),reg)

  Revision 1.1.1.1  1998/03/25 11:18:16  root
  * Restored version

  Revision 1.51  1998/03/22 12:45:37  florian
    * changes of Carl-Eric to m68k target commit:
      - wrong nodes because of the new string cg in intel, I had to create
        this under m68k also ... had to work it out to fix potential alignment
        problems --> this removes the crash of the m68k compiler.
      - added absolute addressing in m68k assembler (required for Amiga startup)
      - fixed alignment problems (because of byte return values, alignment
        would not be always valid) -- is this ok if i change the offset if odd in
        setfirsttemp ?? -- it seems ok...

  Revision 1.50  2036/02/07 09:29:32  florian
    * patch of Carl applied

  Revision 1.49  1998/03/10 16:27:36  pierre
    * better line info in stabs debug
    * symtabletype and lexlevel separated into two fields of tsymtable
    + ifdef MAKELIB for direct library output, not complete
    + ifdef CHAINPROCSYMS for overloaded seach across units, not fully
      working
    + ifdef TESTFUNCRET for setting func result in underfunction, not
      working

  Revision 1.48  1998/03/10 15:25:31  carl
    + put back $L switch for debugging

  Revision 1.47  1998/03/10 04:19:24  carl
    - removed string:=char optimization because would give A LOT of
  register problems

  Revision 1.46  1998/03/10 01:17:15  peter
    * all files have the same header
    * messages are fully implemented, EXTDEBUG uses Comment()
    + AG... files for the Assembler generation

  Revision 1.45  1998/03/09 10:44:33  peter
    + string='', string<>'', string:='', string:=char optimizes (the first 2
      were already in cg68k2)

  Revision 1.44  1998/03/06 00:51:57  peter
    * replaced all old messages from errore.msg, only ExtDebug and some
      Comment() calls are left
    * fixed options.pas

  Revision 1.43  1998/03/05 04:37:46  carl
    + small optimization

  Revision 1.42  1998/03/03 04:13:31  carl
    - removed generate_xxxx and put them in cga68k

  Revision 1.41  1998/03/03 01:08:17  florian
    * bug0105 and bug0106 problem solved

  Revision 1.40  1998/03/02 16:25:25  carl
    * bugfix #95

  Revision 1.39  1998/03/02 01:48:11  peter
    * renamed target_DOS to target_GO32V1
    + new verbose system, merged old errors and verbose units into one new
      verbose.pas, so errors.pas is obsolete

  Revision 1.38  1998/02/25 02:36:29  carl
    * small bugfix with range checking

  Revision 1.37  1998/02/24 16:49:48  peter
    * stackframe ommiting generated 'ret $-4'
    + timer.pp bp7 version
    * innr.inc are now the same files

  Revision 1.36  1998/02/24 16:42:49  carl
    + reinstated __EXIT

  Revision 1.35  1998/02/23 02:56:38  carl
    * bugfix of writing real type values qith m68k target

  Revision 1.34  1998/02/22 23:03:05  peter
    * renamed msource->mainsource and name->unitname
    * optimized filename handling, filename is not seperate anymore with
      path+name+ext, this saves stackspace and a lot of fsplit()'s
    * recompiling of some units in libraries fixed
    * shared libraries are working again
    + $LINKLIB <lib> to support automatic linking to libraries
    + libraries are saved/read from the ppufile, also allows more libraries
      per ppufile

  Revision 1.33  1998/02/22 18:50:12  carl
    * bugfix of stupid diffs!!!!! Recursive crash fix!

  Revision 1.30  1998/02/19 12:22:29  daniel
  * Optimized a statement that did pain to my eyes.

  Revision 1.29  1998/02/17 21:20:31  peter
    + Script unit
    + __EXIT is called again to exit a program
    - target_info.link/assembler calls
    * linking works again for dos
    * optimized a few filehandling functions
    * fixed stabs generation for procedures

  Revision 1.28  1998/02/15 21:16:04  peter
    * all assembler outputs supported by assemblerobject
    * cleanup with assembleroutputs, better .ascii generation
    * help_constructor/destructor are now added to the externals
    - generation of asmresponse is not outputformat depended

  Revision 1.27  1998/02/14 05:06:47  carl
    + now works with TP with overlays

  Revision 1.26  1998/02/14 01:45:06  peter
    * more fixes
    - pmode target is removed
    - search_as_ld is removed, this is done in the link.pas/assemble.pas
    + findexe() to search for an executable (linker,assembler,binder)

  Revision 1.25  1998/02/13 10:34:40  daniel
  * Made Motorola version compilable.
  * Fixed optimizer

  Revision 1.24  1998/02/12 11:49:45  daniel
  Yes! Finally! After three retries, my patch!

  Changes:

  Complete rewrite of psub.pas.
  Added support for DLL's.
  Compiler requires less memory.
  Platform units for each platform.

  Revision 1.23  1998/02/07 18:00:45  carl
    * bugfix in secondin (from Peter Vreman a while ago)

  Revision 1.21  1998/02/05 00:58:05  carl
    + secondas and secondis now work as expected.
    - moved secondas to cg68k2, otherwise problems with symbols

  Revision 1.20  1998/02/01 19:38:41  florian
    * bug0029 fixed, Carl please check it !!!

  Revision 1.19  1998/01/24 21:05:41  carl
    * nested comment bugfix

  Revision 1.18  1998/01/24 00:37:47  florian
    * small fix for DOM

  Revision 1.17  1998/01/21 21:29:46  florian
    * some fixes for Delphi classes

  Revision 1.16  1998/01/20 23:51:59  carl
    * bugfix 74 (FINAL, Pierre's one was incomplete under BP)

  Revision 1.15  1998/01/19 10:25:21  pierre
    * bug in object function call in main program or unit init fixed

  Revision 1.14  1998/01/16 22:34:23  michael
  * Changed 'conversation' to 'conversion'. Waayyy too much chatting going on
    in this compiler :)

  Revision 1.13  1998/01/16 02:18:25  carl
    * second_char_to_string align problem fix (N/A for MC68020 target)

  Revision 1.12  1998/01/13 23:11:02  florian
    + class methods

  Revision 1.11  1998/01/11 03:36:14  carl
  * fixed indexing problem with stack
  * reference on stack bugfix
  * second_bigger sign extension bugfix
  * array scaling bugfix
  * secondderef bugfix
  * bugfix with MOVEQ opcode
  * bugfix of linear list generation

  Revision 1.6  1997/12/10 23:07:12  florian
  * bugs fixed: 12,38 (also m68k),39,40,41
  + warning if a system unit is without -Us compiled
  + warning if a method is virtual and private (was an error)
  * some indentions changed
  + factor does a better error recovering (omit some crashes)
  + problem with @type(x) removed (crashed the compiler)

  Revision 1.5  1997/12/09 13:28:48  carl
  + added s80 real (will presently stop the compiler though)
  + renamed some stuff
  * some bugfixes (can't remember what exactly..)

  Revision 1.4  1997/12/05 14:51:09  carl
  * bugfix of secondfor
      cmpreg was never initialized.
      one of the jump conditionals was wrong (downto would not work)

  Revision 1.3  1997/12/04 14:47:05  carl
  + updated tov09...

  Revision 1.2  1997/11/28 18:14:20  pierre
   working version with several bug fixes

  Revision 1.1.1.1  1997/11/27 08:32:51  michael
  FPC Compiler CVS start


  Pre-CVS log:

  CEC   Carl-Eric Codere
  FK     Florian Klaempfl
  PM    Pierre Muller
  +      feature added
  -      removed
  *      bug fixed or changed

  History (started with version 0.9.0):
      23th october 1996:
         + some emit calls replaced (FK)
      24th october 1996:
         * for bug fixed (FK)
      26th october 1996:
         * english comments (FK)
       5th november 1996:
         * new init and terminate code (FK)

      ...... some items missed

      19th september 1997:
         * a call to a function procedure a;[ C ]; doesn't crash the stack
           furthermore (FK)
      22th september 1997:
         * stack layout for nested procedures in methods modified:
           ESI is no more pushed (must be loaded via framepointer) (FK)

      27th september 1997:
        + Start of conversion to motorola MC68000 (CEC)
      29th september 1997:
        + Updated to version 0.9.4 of Intel code generator (CEC)
      3th october 1997:
        + function second_bool_to_byte for ord(boolean) (PM)
      4th october 1997: (CEC)
         + first compilation
      5th octover 1997:
          check floating point negate when i can test everything,
            to see if it makes any sense , according SINGLE_NEG from
            sozobon, it does not.??
      8th october 1997:
        + ord(x) support (FK)
        + some stuff for typed file support (FK)
      9 october 1997:
        + converted code to motorola for v096 (CEC)
     18 october 1997:
        +* removed bugs relating to floating point condition codes. (CEC).
           (in secondadd).
        + had to put secondadd in another routine to compile in tp. (CEC).
        + updated second_bool_to_byte,secondtypeconv and secondinline, secondvecn to v097 (CEC)
        + updated secondload and secondstringconst (merging duplicate strings),secondfor to v95/v97 (CEC).
        + finally converted second_fix_real (very difficult and untested!). (CEC)
     23 october 1997:
        * bugfix of address register in usableregs set. (They were not defined...) (CEC).
     24 october 1997:
        * bugfix of scalefactor, allowed unrolled using lsl. (CEC).
   27th october 1997:
       + now all general purpose registers are in the unused list, so this fixes problems
         regarding pushing registers (such as d0) which were actually never used. (CEC)
       + added secondin (FK) (all credit goes to him).
       + converted second_real_fix thanks to Daniel Mantione for the information
         he gave me on the fixed format. Thanks to W. Metzenthen who did WMEmu
         (which in turn gave me information on the control word of the intel fpu). (CEC)
   23rd november 1997:
       + changed second_int_real to apply correct calling conventions of rtl.
   26th november 1997:
       + changed secondmoddiv to apply correct calling conventions of rtl
          and also optimized it a bit.

}

