{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

    Generate m68k assembler for load/assignment nodes

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
unit cg68kld;
interface

    uses
      tree,cpubase;

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

    procedure secondload(var p : ptree);
    procedure secondassignment(var p : ptree);
    procedure secondfuncret(var p : ptree);
    procedure secondarrayconstruct(var p : ptree);


implementation

    uses
      cobjects,verbose,globals,symconst,
      symtable,aasm,types,
      hcodegen,temp_gen,pass_2,
      cga68k,tgen68k;


{*****************************************************************************
                             SecondLoad
*****************************************************************************}

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

                              p^.location.reference.base:=procinfo^.framepointer;
                              p^.location.reference.offset:=pvarsym(p^.symtableentry)^.address;

                              if (symtabletype=localsymtable) then
                                p^.location.reference.offset:=-p^.location.reference.offset;

                              if (symtabletype in [localsymtable,inlinelocalsymtable]) then
                                p^.location.reference.offset:=-p^.location.reference.offset;

                              if (lexlevel>(p^.symtable^.symtablelevel)) then
                                begin
                                   hregister:=getaddressreg;

                                   { make a reference }
                                   new(hp);
                                   reset_reference(hp^);
                                   hp^.offset:=procinfo^.framepointer_offset;
                                   hp^.base:=procinfo^.framepointer;

                                   exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,S_L,hp,hregister)));

                                   simple_loadn:=false;
                                   i:=lexlevel-1;
                                   while i>(p^.symtable^.symtablelevel) do
                                     begin
                                        { make a reference }
                                        new(hp);
                                        reset_reference(hp^);
                                        hp^.offset:=8;
                                        hp^.base:=hregister;

                                        exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,S_L,hp,hregister)));
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
                                               end;
                              objectsymtable : begin
                                                  if sp_static in pvarsym(p^.symtableentry)^.symoptions then
                                                    begin
                                                       stringdispose(p^.location.reference.symbol);
                                                       p^.location.reference.symbol:=stringdup(p^.symtableentry^.mangledname);
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
                                                  hp^.base:=procinfo^.framepointer;

                                                  exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,S_L,hp,hregister)));

                                                  p^.location.reference.offset:=
                                                    pvarsym(p^.symtableentry)^.address;
                                               end;
                           end;

                         { in case call by reference, then calculate: }
                         if (pvarsym(p^.symtableentry)^.varspez=vs_var) or
                            is_open_array(pvarsym(p^.symtableentry)^.definition) or
                            is_array_of_const(pvarsym(p^.symtableentry)^.definition) or
                            ((pvarsym(p^.symtableentry)^.varspez=vs_const) and
                             push_addr_param(pvarsym(p^.symtableentry)^.definition)) then
                           begin
                              simple_loadn:=false;
                              if hregister=R_NO then
                                hregister:=getaddressreg;
                              { ADDED FOR OPEN ARRAY SUPPORT. }
                              if (p^.location.reference.base=procinfo^.framepointer) then
                                begin
                                   highframepointer:=p^.location.reference.base;
                                   highoffset:=p^.location.reference.offset;
                                end
                              else
                                begin
                                   highframepointer:=R_A1;
                                   highoffset:=p^.location.reference.offset;
                                   exprasmlist^.concat(new(paicpu,op_reg_reg(A_MOVE,S_L,
                                     p^.location.reference.base,R_A1)));
                                end;
                              exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,S_L,newreference(p^.location.reference),
                                hregister)));
                              { END ADDITION }
                              clear_reference(p^.location.reference);
                              p^.location.reference.base:=hregister;
                          end;
                         { should be dereferenced later (FK)
                         if (pvarsym(p^.symtableentry)^.definition^.deftype=objectdef) and
                           ((pobjectdef(pvarsym(p^.symtableentry)^.definition)^.options and oo_is_class)<>0) then
                           begin
                              simple_loadn:=false;
                              if hregister=R_NO then
                                hregister:=getaddressreg;
                              exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,S_L,newreference(p^.location.reference),
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
                 end;
              typedconstsym :
                 begin
                    stringdispose(p^.location.reference.symbol);
                    p^.location.reference.symbol:=stringdup(p^.symtableentry^.mangledname);
                 end;
              else internalerror(4);
         end;
      end;


{*****************************************************************************
                             SecondAssignment
*****************************************************************************}

    procedure secondassignment(var p : ptree);

      var
         opsize : topsize;
         withresult : boolean;
         otlabel,hlabel,oflabel : pasmlabel;
         hregister : tregister;
         loc : tloc;
         pushed : boolean;

      begin
         otlabel:=truelabel;
         oflabel:=falselabel;
         getlabel(truelabel);
         getlabel(falselabel);
         withresult:=false;
         { calculate left sides }
         secondpass(p^.left);
         if codegenerror then
           exit;
         loc:=p^.left^.location.loc;
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

         pushed:=maybe_push(p^.right^.registers32,p^.left);
         secondpass(p^.right);
         if pushed then restore(p^.left);

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
                                exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,opsize,
                                  newreference(p^.right^.location.reference),
                                  p^.left^.location.register)))
                              else
                                exprasmlist^.concat(new(paicpu,op_const_ref(A_MOVE,opsize,
                                  p^.right^.location.reference.offset,
                                  newreference(p^.left^.location.reference))));
                              {exprasmlist^.concat(new(paicpu,op_const_loc(A_MOV,opsize,
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
                                exprasmlist^.concat(new(paicpu,op_reg_reg(A_MOVE,opsize,
                                  p^.right^.location.register,
                                  p^.left^.location.register)))
                              else
                                exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOVE,opsize,
                                  p^.right^.location.register,
                                  newreference(p^.left^.location.reference))));
                              {exprasmlist^.concat(new(paicpu,op_reg_loc(A_MOV,opsize,
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
                                exprasmlist^.concat(new(paicpu,op_const_reg(A_MOVE,S_B,
                                  1,p^.left^.location.register)))
                              else
                                exprasmlist^.concat(new(paicpu,op_const_ref(A_MOVE,S_B,
                                  1,newreference(p^.left^.location.reference))));
                              {exprasmlist^.concat(new(paicpu,op_const_loc(A_MOV,S_B,
                                  1,p^.left^.location)));}
                              emitl(A_JMP,hlabel);
                              emitl(A_LABEL,falselabel);
                              if loc=LOC_CREGISTER then
                                exprasmlist^.concat(new(paicpu,op_reg(A_CLR,S_B,
                                  p^.left^.location.register)))
                              else
                                exprasmlist^.concat(new(paicpu,op_const_ref(A_MOVE,S_B,
                                  0,newreference(p^.left^.location.reference))));
                              emitl(A_LABEL,hlabel);
                           end;
            LOC_FLAGS    : begin
                              if loc=LOC_CREGISTER then
                               begin
                                exprasmlist^.concat(new(paicpu,op_reg(flag_2_set[p^.right^.location.resflags],S_B,
                                  p^.left^.location.register)));
                                exprasmlist^.concat(new(paicpu,op_reg(A_NEG,S_B,p^.left^.location.register)));
                               end
                              else
                               begin
                                 exprasmlist^.concat(new(paicpu,op_ref(flag_2_set[p^.right^.location.resflags],S_B,
                                    newreference(p^.left^.location.reference))));
                                 exprasmlist^.concat(new(paicpu,op_ref(A_NEG,S_B,newreference(p^.left^.location.reference))));
                               end;

                           end;
         end;
         truelabel:=otlabel;
         falselabel:=oflabel;
      end;


{*****************************************************************************
                             SecondFuncRetN
*****************************************************************************}

    procedure secondfuncret(var p : ptree);
      var
         hr : tregister;
         hp : preference;
         pp : pprocinfo;
         hr_valid : boolean;
      begin
         clear_reference(p^.location.reference);
         hr_valid:=false;
{ !!!!!!! }

         if @procinfo<>pprocinfo(p^.funcretprocinfo) then
           begin
              hr:=getaddressreg;
              hr_valid:=true;
              hp:=new_reference(procinfo^.framepointer,
                procinfo^.framepointer_offset);
              exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,S_L,hp,hr)));

              pp:=procinfo^.parent;
              { walk up the stack frame }
              while pp<>pprocinfo(p^.funcretprocinfo) do
                begin
                   hp:=new_reference(hr,
                     pp^.framepointer_offset);
                   exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,S_L,hp,hr)));
                   pp:=pp^.parent;
                end;
              p^.location.reference.base:=hr;
           end
         else
           p^.location.reference.base:=procinfo^.framepointer;
         p^.location.reference.offset:=procinfo^.retoffset;
         if ret_in_param(p^.retdef) then
           begin
              if not hr_valid then
                { this was wrong !! PM }
                hr:=getaddressreg;
              exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,S_L,newreference(p^.location.reference),hr)));
              p^.location.reference.base:=hr;
              p^.location.reference.offset:=0;
           end;
      end;

{*****************************************************************************
                           SecondArrayConstruct
*****************************************************************************}

      const
        vtInteger    = 0;
        vtBoolean    = 1;
        vtChar       = 2;
        vtExtended   = 3;
        vtString     = 4;
        vtPointer    = 5;
        vtPChar      = 6;
        vtObject     = 7;
        vtClass      = 8;
        vtWideChar   = 9;
        vtPWideChar  = 10;
        vtAnsiString = 11;
        vtCurrency   = 12;
        vtVariant    = 13;
        vtInterface  = 14;
        vtWideString = 15;
        vtInt64      = 16;

    procedure secondarrayconstruct(var p : ptree);
      begin
      end;

end.
{
  $Log$
  Revision 1.2  2000-07-13 11:32:37  michael
  + removed logs

}
