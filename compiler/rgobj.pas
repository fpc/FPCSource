{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

    This unit implements the base class for the register allocator

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

{$i fpcdefs.inc}

{# @abstract(Abstract register allocator unit)
   This unit contains services to allocate, free
   references and registers which are used by
   the code generator.
}

{*******************************************************************************

(applies to new register allocator)

Register allocator introduction.

Free Pascal uses a Chaitin style register allocator similair to the one
described in the book "Modern compiler implementation in C" by Andrew W. Appel.,
published by Cambridge University Press.

Reading this book is recommended for a complete understanding. Here is a small
introduction.

The code generator thinks it has an infinite amount of registers. Our processorhas a limited amount of registers. Therefore we must reduce the amount of
registers until there are less enough to fit into the processors registers.

Registers can interfere or not interfere. If two imaginary registers interfere
they cannot be placed into the same psysical register. Reduction of registers
is done by:

- "coalescing" Two registers that do not interfere are combined
   into one register.
- "spilling" A register is changed into a memory location and the generated
   code is modified to use the memory location instead of the register.

Register allocation is a graph colouring problem. Each register is a colour, and
if two registers interfere there is a connection between them in the graph.

In addition to the imaginary registers in the code generator, the psysical
CPU registers are also present in this graph. This allows us to make
interferences between imaginary registers and cpu registers. This is very
usefull for describing archtectural constraints, like for example that
the div instruction modifies edx, so variables that are in use at that time
cannot be stored into edx. This can be modelled by making edx interfere
with those variables.

Graph colouring is an NP complete problem. Therefore we use an approximation
that pushes registers to colour on to a stack. This is done in the "simplify"
procedure.

*******************************************************************************}


unit rgobj;

  interface

    uses
      cutils, cpubase,
      cpuinfo,
      aasmbase,aasmtai,aasmcpu,
      cclasses,globtype,cginfo,cgbase,node
{$ifdef delphi}
      ,dmisc
{$endif}
      ;


    const ALL_REGISTERS=[firstreg..lastreg];
          ALL_INTREGISTERS=[first_supreg..last_supreg]-[RS_STACK_POINTER_REG];

    type


       regvarother_longintarray = array[firstreg..lastreg] of longint;
       regvarother_booleanarray = array[firstreg..lastreg] of boolean;
       regvarint_longintarray = array[first_supreg..last_supreg] of longint;
       regvarint_ptreearray = array[first_supreg..last_supreg] of tnode;

       tpushedsavedloc = record
         case byte of
           0: (pushed: boolean);
           1: (ofs: longint);
       end;

       tpushedsavedother = array[firstreg..lastreg] of tpushedsavedloc;
{$ifndef newra}
       Tpushedsavedint = array[first_supreg..last_supreg] of Tpushedsavedloc;
{$endif}

      Tinterferencebitmap=array[Tsuperregister] of set of Tsuperregister;
      Tinterferenceadjlist=array[Tsuperregister] of Pstring;
      Tinterferencegraph=record
        bitmap:Tinterferencebitmap;
        adjlist:Tinterferenceadjlist;
      end;
      Pinterferencegraph=^Tinterferencegraph;

      Tmovelist=record
        count:cardinal;
        data:array[0..$ffff] of Tlinkedlistitem;
      end;
      Pmovelist=^Tmovelist;

      {In the register allocator we keep track of move instructions.
       These instructions are moved between five linked lists. There
       is also a linked list per register to keep track about the moves
       it is associated with. Because we need to determine quickly in
       which of the five lists it is we add anu enumeradtion to each
       move instruction.}

      Tmoveset=(ms_coalesced_moves,ms_constrained_moves,ms_frozen_moves,
                ms_worklist_moves,ms_active_moves);
      Tmoveins=class(Tlinkedlistitem)
        moveset:Tmoveset;
      { $ifdef ra_debug}
        x,y:Tsuperregister;
      { $endif}
        instruction:Taicpu;
      end;

       {#
          This class implements the abstract register allocator
          It is used by the code generator to allocate and free
          registers which might be valid across nodes. It also
          contains utility routines related to registers.

          Some of the methods in this class should be overriden
          by cpu-specific implementations.
       }
       trgobj = class
          { The "usableregsxxx" contain all registers of type "xxx" that }
          { aren't currently allocated to a regvar. The "unusedregsxxx"  }
          { contain all registers of type "xxx" that aren't currenly     }
          { allocated                                                    }
          lastintreg,maxintreg:Tsuperregister;
          unusedregsint,usableregsint:Tsupregset;
          unusedregsaddr,usableregsaddr:Tsupregset;
          unusedregsfpu,usableregsfpu : tregisterset;
          unusedregsmm,usableregsmm : tregisterset;
          { these counters contain the number of elements in the }
          { unusedregsxxx/usableregsxxx sets                     }
{$ifndef newra}
          countunusedregsint,
          countunusedregsaddr,
{$endif}
          countunusedregsfpu,
          countunusedregsmm : byte;
          countusableregsint,
          countusableregsaddr,
          countusableregsfpu,
          countusableregsmm : byte;

          { Contains the registers which are really used by the proc itself.
            It doesn't take care of registers used by called procedures
          }
          usedbyproc,
          usedinproc : tregisterset;
{$ifdef newra}
          savedbyproc,
{$else}
          usedintbyproc,
{$endif}
          usedaddrbyproc,
          usedintinproc,
          usedaddrinproc:Tsupregset;

          reg_pushes_other : regvarother_longintarray;
{$ifndef newra}
          reg_pushes_int : regvarint_longintarray;
{$endif}
          is_reg_var_other : regvarother_booleanarray;
          is_reg_var_int:Tsupregset;
          regvar_loaded_other: regvarother_booleanarray;
          regvar_loaded_int: Tsupregset;
{$ifdef newra}
          colour:array[Tsuperregister] of Tsuperregister;
          spillednodes:string;
{$endif}

          { tries to hold the amount of times which the current tree is processed  }
          t_times: longint;

          constructor create(Acpu_registers:byte);

          {# Allocate a general purpose register

             An internalerror will be generated if there
             is no more free registers which can be allocated
          }
          function getregisterint(list:Taasmoutput;size:Tcgsize):Tregister;{$ifndef newra}virtual;{$endif}
{$ifdef newra}
          procedure add_constraints(reg:Tnewregister);virtual;

          {# Allocate an ABT register

             An internalerror will be generated if there
             is no more free registers which can be allocated

             An explanantion of abt registers can be found near the implementation.
          }
          function getabtregisterint(list:Taasmoutput;size:Tcgsize):Tregister;
{$endif}

          {# Free a general purpose register

             @param(r register to free)

          }
          procedure ungetregisterint(list: taasmoutput; r : tregister); virtual;

          {# Allocate a floating point register

             An internalerror will be generated if there
             is no more free registers which can be allocated
          }
          function getregisterfpu(list: taasmoutput;size:Tcgsize) : tregister; virtual;
          {# Free a floating point register

             @param(r register to free)

          }
          procedure ungetregisterfpu(list: taasmoutput; r : tregister); virtual;

          function getregistermm(list: taasmoutput) : tregister; virtual;
          procedure ungetregistermm(list: taasmoutput; r : tregister); virtual;

          {# Allocate an address register.

             Address registers are the only registers which can
             be used as a base register in references (treference).
             On most cpu's this is the same as a general purpose
             register.

             An internalerror will be generated if there
             is no more free registers which can be allocated
          }
          function getaddressregister(list:Taasmoutput):Tregister;virtual;
          procedure ungetaddressregister(list: taasmoutput; r: tregister); virtual;
          {# Verify if the specified register is an address or
             general purpose register. Returns TRUE if @var(reg)
             is an adress register.

             This routine should only be used to check on
             general purpose or address register. It will
             not work on multimedia or floating point
             registers

             @param(reg register to verify)
          }
          function isaddressregister(reg: tregister): boolean; virtual;

          {# Tries to allocate the passed register, if possible

             @param(r specific register to allocate)
          }
          function getexplicitregisterint(list:Taasmoutput;r:Tnewregister):Tregister;virtual;
          {# Tries to allocate the passed fpu register, if possible

             @param(r specific register to allocate)
          }
          function getexplicitregisterfpu(list : taasmoutput; r : Toldregister) : tregister;virtual;

          {# Deallocate any kind of register }
          procedure ungetregister(list: taasmoutput; r : tregister); virtual;

          {# Deallocate all registers which are allocated
             in the specified reference. On most systems,
             this will free the base and index registers
             of the specified reference.

             @param(ref reference which must have its registers freed)
          }
          procedure ungetreference(list: taasmoutput; const ref : treference); virtual;

          {# Reset the register allocator information (usable registers etc).
             Please note that it is mortal sins to call cleartempgen during
             graph colouring (that is between prepare_colouring and
             epilogue_colouring).
          }

          procedure cleartempgen;virtual;

          {# Convert a register to a specified register size, and return that register size }
          function makeregsize(reg: tregister; size: tcgsize): tregister; virtual;


          {# saves register variables (restoring happens automatically) }
{$ifndef newra}
          procedure saveintregvars(list:Taasmoutput;const s:Tsupregset);
{$endif}
          procedure saveotherregvars(list:Taasmoutput;const s:Tregisterset);

          {# Saves in temporary references (allocated via the temp. allocator)
             the registers defined in @var(s). The registers are only saved
             if they are currently in use, otherwise they are left as is.

             On processors which have instructions which manipulate the stack,
             this routine should be overriden for performance reasons.

             @param(list)   List to add the instruction to
             @param(saved)  Array of saved register information
             @param(s)      Registers which might require saving
          }
{$ifndef newra}
          procedure saveusedintregisters(list:Taasmoutput;
                                         var saved:Tpushedsavedint;
                                         const s:Tsupregset);virtual;
{$endif}
          procedure saveusedotherregisters(list:Taasmoutput;
                                           var saved:Tpushedsavedother;
                                           const s:Tregisterset);virtual;
          {# Restores the registers which were saved with a call
             to @var(saveusedregisters).

             On processors which have instructions which manipulate the stack,
             this routine should be overriden for performance reasons.
          }
{$ifndef newra}
          procedure restoreusedintregisters(list:Taasmoutput;
                                            const saved:Tpushedsavedint);virtual;
{$endif}
          procedure restoreusedotherregisters(list:Taasmoutput;
                                              const saved:Tpushedsavedother);virtual;

          { used when deciding which registers to use for regvars }
{$ifndef newra}
          procedure incrementintregisterpushed(const s:Tsupregset);
{$endif}
          procedure incrementotherregisterpushed(const s: tregisterset);
          procedure clearregistercount;
          procedure resetusableregisters;virtual;

          procedure makeregvarint(reg:Tsuperregister);
          procedure makeregvarother(reg:Tregister);

          procedure saveStateForInline(var state: pointer);virtual;
          procedure restoreStateAfterInline(var state: pointer);virtual;

          procedure saveUnusedState(var state: pointer);virtual;
          procedure restoreUnusedState(var state: pointer);virtual;
{$ifdef newra}
{$ifdef ra_debug}
          procedure writegraph;
{$endif}
          procedure add_move_instruction(instr:Taicpu);
          procedure prepare_colouring;
          procedure epilogue_colouring;
          procedure colour_registers;
          function spill_registers(list:Taasmoutput;const regs_to_spill:string):boolean;
{$endif newra}
       protected
          cpu_registers:byte;
{$ifdef newra}
          igraph:Tinterferencegraph;
          degree:array[0..255] of byte;
          alias:array[Tsuperregister] of Tsuperregister;
          simplifyworklist,freezeworklist,spillworklist:string;
          coalescednodes:string;
          selectstack:string;
          abtlist:string;
          movelist:array[Tsuperregister] of Pmovelist;
          worklist_moves,active_moves,frozen_moves,
          coalesced_moves,constrained_moves:Tlinkedlist;
{$endif}
          { the following two contain the common (generic) code for all }
          { get- and ungetregisterxxx functions/procedures              }
          function getregistergen(list: taasmoutput; const lowreg, highreg: Toldregister;
              var unusedregs:Tregisterset;var countunusedregs:byte): tregister;
          function getregistergenint(list:Taasmoutput;subreg:Tsubregister;
                                     const lowreg,highreg:Tsuperregister;
                                     var fusedinproc,{$ifndef newra}fusedbyproc,{$endif}unusedregs:Tsupregset
                                     {$ifndef newra};var countunusedregs:byte{$endif}):Tregister;
          procedure ungetregistergen(list: taasmoutput; const r: tregister;
              const usableregs:tregisterset;var unusedregs: tregisterset; var countunusedregs: byte);
          procedure ungetregistergenint(list:taasmoutput;const r:Tregister;
                                        const usableregs:Tsupregset;
                                        var unusedregs:Tsupregset
                                        {$ifndef newra};var countunusedregs:byte{$endif});
{$ifdef newra}
          procedure getregisterintinline(list:Taasmoutput;position:Tai;subreg:Tsubregister;var result:Tregister);
          procedure ungetregisterintinline(list:Taasmoutput;position:Tai;const r:Tregister);
{$endif}
{$ifdef TEMPREGDEBUG}
         reg_user   : regvar_ptreearray;
         reg_releaser : regvar_ptreearray;
{$endif TEMPREGDEBUG}

{$ifdef TEMPREGDEBUG}
          procedure testregisters;
{$endif TEMPREGDEBUGx}
{$ifdef newra}
         procedure add_edge(u,v:Tsuperregister);
         procedure add_edges_used(u:Tsuperregister);
         procedure add_to_movelist(u:Tsuperregister;data:Tlinkedlistitem);
         function move_related(n:Tsuperregister):boolean;
         procedure make_work_list;
         procedure enable_moves(n:Tsuperregister);
         procedure decrement_degree(m:Tsuperregister);
         procedure simplify;

         function get_alias(n:Tsuperregister):Tsuperregister;
         procedure add_worklist(u:Tsuperregister);
         function adjacent_ok(u,v:Tsuperregister):boolean;
         function conservative(u,v:Tsuperregister):boolean;
         procedure combine(u,v:Tsuperregister);
         procedure coalesce;
         procedure freeze_moves(u:Tsuperregister);
         procedure freeze;
         procedure select_spill;
         procedure assign_colours;
         procedure clear_interferences(u:Tsuperregister);
{$endif}
       end;

     const
       {# This value is used in tsaved. If the array value is equal
          to this, then this means that this register is not used.
       }
       reg_not_saved = $7fffffff;

     var
       {# This is the class instance used to access the register allocator class }
       rg: trgobj;

     { trerefence handling }

     {# Clear to zero a treference }
     procedure reference_reset(var ref : treference);
     procedure reference_reset_old(var ref : treference);
     {# Clear to zero a treference, and set is base address
        to base register.
     }
     procedure reference_reset_base(var ref : treference;base : tregister;offset : longint);
     procedure reference_reset_symbol(var ref : treference;sym : tasmsymbol;offset : longint);
     procedure reference_release(list: taasmoutput; const ref : treference);
     { This routine verifies if two references are the same, and
        if so, returns TRUE, otherwise returns false.
     }
     function references_equal(sref : treference;dref : treference) : boolean;

     { tlocation handling }
     procedure location_reset(var l : tlocation;lt:TCGLoc;lsize:TCGSize);
     procedure location_release(list: taasmoutput; const l : tlocation);
     procedure location_freetemp(list: taasmoutput; const l : tlocation);
     procedure location_copy(var destloc:tlocation; const sourceloc : tlocation);
     procedure location_swap(var destloc,sourceloc : tlocation);

    type
      psavedstate = ^tsavedstate;
      tsavedstate = record
        unusedregsint,usableregsint : Tsupregset;
        unusedregsaddr,usableregsaddr : Tsupregset;
        unusedregsfpu,usableregsfpu : tregisterset;
        unusedregsmm,usableregsmm : tregisterset;
{$ifndef newra}
        countunusedregsint,
        countunusedregsaddr,
{$endif}
        countunusedregsfpu,
        countunusedregsmm : byte;
        countusableregsint,
        countusableregsfpu,
        countusableregsmm : byte;
        { contains the registers which are really used by the proc itself }
        usedbyproc,
        usedinproc : tregisterset;
        reg_pushes_other : regvarother_longintarray;
        reg_pushes_int : regvarint_longintarray;
        is_reg_var_other : regvarother_booleanarray;
        is_reg_var_int : Tsupregset;
        regvar_loaded_other: regvarother_booleanarray;
        regvar_loaded_int: Tsupregset;
{$ifdef TEMPREGDEBUG}
         reg_user   : regvar_ptreearray;
         reg_releaser : regvar_ptreearray;
{$endif TEMPREGDEBUG}
      end;

      punusedstate = ^tunusedstate;
      tunusedstate = record
        unusedregsint : Tsupregset;
        unusedregsaddr : Tsupregset;
        unusedregsfpu : tregisterset;
        unusedregsmm : tregisterset;
{$ifndef newra}
        countunusedregsint,
        countunusedregsaddr,
{$endif}
        countunusedregsfpu,
        countunusedregsmm : byte;
      end;

  implementation

    uses
       systems,
       globals,verbose,
       cgobj,tgobj,regvars;

    constructor Trgobj.create(Acpu_registers:byte);

     begin
       usedinproc := [];
       usedbyproc:=[];
       t_times := 0;
       resetusableregisters;
       lastintreg:=0;
       maxintreg:=first_imreg;
       cpu_registers:=Acpu_registers;
{$ifdef TEMPREGDEBUG}
       fillchar(reg_user,sizeof(reg_user),0);
       fillchar(reg_releaser,sizeof(reg_releaser),0);
{$endif TEMPREGDEBUG}
{$ifdef newra}
       fillchar(igraph,sizeof(igraph),0);
       fillchar(degree,sizeof(degree),0);
       fillchar(movelist,sizeof(movelist),0);
       worklist_moves:=Tlinkedlist.create;
       abtlist:='';
{$endif}
     end;


    function trgobj.getregistergen(list: taasmoutput; const lowreg, highreg: Toldregister;
        var unusedregs: tregisterset; var countunusedregs: byte): tregister;
      var
        i: Toldregister;
        r: Tregister;
      begin
         for i:=lowreg to highreg do
           begin
              if i in unusedregs then
                begin
                   exclude(unusedregs,i);
                   include(usedinproc,i);
                   include(usedbyproc,i);
                   dec(countunusedregs);
                   r.enum:=i;
                   list.concat(tai_regalloc.alloc(r));
                   result := r;
                   exit;
                end;
           end;
         internalerror(10);
      end;

    function Trgobj.getregistergenint(list:Taasmoutput;
                                      subreg:Tsubregister;
                                      const lowreg,highreg:Tsuperregister;
                                      var fusedinproc,{$ifndef newra}fusedbyproc,{$endif}unusedregs:Tsupregset
                                      {$ifndef newra};var countunusedregs:byte{$endif}):Tregister;

{$ifdef powerpc}
{$ifndef newra}
{$define reuseregs}
{$endif newra}
{$endif powerpc}

    var i:Tsuperregister;
        r:Tregister;

    begin
{$ifdef reuseregs}
      i := lowreg;
      lastintreg := highreg;
{$else reuseregs}
      if not (lastintreg in [lowreg..highreg]) then
        lastintreg:=lowreg;
      i:=lastintreg;
{$endif reuseregs}
      repeat
        if i=highreg then
          i:=lowreg
        else
          inc(i);
        if (i in unusedregs) {$ifdef newra} and (pos(char(i),abtlist)=0) {$endif} then
          begin
            exclude(unusedregs,i);
            include(fusedinproc,i);
          {$ifndef newra}
            include(fusedbyproc,i);
            dec(countunusedregs);
          {$endif}
            r.enum:=R_INTREGISTER;
            r.number:=i shl 8 or subreg;
            list.concat(Tai_regalloc.alloc(r));
            result:=r;
            lastintreg:=i;
            if i>maxintreg then
              maxintreg:=i;
          {$ifdef newra}
            add_edges_used(i);
          {$endif}
            exit;
          end;
      until i=lastintreg;
      internalerror(10);
    end;


    procedure trgobj.ungetregistergen(list: taasmoutput; const r: tregister;
        const usableregs: tregisterset; var unusedregs: tregisterset; var countunusedregs: byte);
      begin
         if r.enum>lastreg then
            internalerror(2003010801);
         { takes much time }
         if not(r.enum in usableregs) then
           exit;
{$ifdef TEMPREGDEBUG}
         if (r.enum in unusedregs) then
{$ifdef EXTTEMPREGDEBUG}
           begin
             Comment(V_Debug,'register freed twice '+std_reg2str[r.enum]);
             testregisters32;
             exit;
           end
{$else EXTTEMPREGDEBUG}
           exit
{$endif EXTTEMPREGDEBUG}
         else
{$endif TEMPREGDEBUG}
          inc(countunusedregs);
        include(unusedregs,r.enum);
        list.concat(tai_regalloc.dealloc(r));
      end;

    procedure trgobj.ungetregistergenint(list:taasmoutput;const r:Tregister;
                                         const usableregs:Tsupregset;
                                         var unusedregs:Tsupregset
                                         {$ifndef newra};var countunusedregs:byte{$endif});

    var supreg:Tsuperregister;

    begin
      if r.enum<=lastreg then
        internalerror(2003010803);
      supreg:=r.number shr 8;
      { takes much time }
{$ifndef newra}
      if not(supreg in usableregs) then
        exit;
{$endif}
{$ifdef TEMPREGDEBUG}
         if (supreg in unusedregs) then
{$ifdef EXTTEMPREGDEBUG}
           begin
             comment(v_debug,'register freed twice '+supreg_name(supreg));
             testregisters32
             exit;
           end
{$else EXTTEMPREGDEBUG}
           exit
{$endif EXTTEMPREGDEBUG}
         else
{$endif TEMPREGDEBUG}
          {$ifndef newra}inc(countunusedregs){$endif};
        include(unusedregs,supreg);
        list.concat(tai_regalloc.dealloc(r));
{$ifdef newra}
        add_edges_used(supreg);
{$endif newra}
      end;


    function trgobj.getregisterint(list:taasmoutput;size:Tcgsize):Tregister;

    var subreg:Tsubregister;

    begin
{$ifndef newra}
      if countunusedregsint=0 then
        internalerror(10);
  {$ifdef TEMPREGDEBUG}
      if curptree^^.usableregs-countunusedregsint>curptree^^.registers32 then
        internalerror(10);
  {$endif TEMPREGDEBUG}
  {$ifdef EXTTEMPREGDEBUG}
      if curptree^^.usableregs-countunusedregsint>curptree^^.reallyusedregs then
        curptree^^.reallyusedregs:=curptree^^.usableregs-countunusedregsint;
  {$endif EXTTEMPREGDEBUG}
{$endif}
      subreg:=cgsize2subreg(size);
      result:=getregistergenint(list,
                                subreg,
{$ifdef newra}
                                first_imreg,
                                last_imreg,
{$else}
                                first_supreg,
                                last_supreg,
                                usedintbyproc,
{$endif}
                                usedintinproc,
                                unusedregsint{$ifndef newra},
                                countunusedregsint{$endif});
{$ifdef TEMPREGDEBUG}
      reg_user[result]:=curptree^;
      testregisters32;
{$endif TEMPREGDEBUG}
{$ifdef newra}
      add_constraints(getregisterint.number);
{$endif}
    end;

{$ifdef newra}
    procedure Trgobj.add_constraints(reg:Tnewregister);

    begin
    end;
{$endif}

    procedure trgobj.ungetregisterint(list : taasmoutput; r : tregister);

      begin
         ungetregistergenint(list,r,usableregsint,unusedregsint{$ifndef newra},
           countunusedregsint{$endif});
{$ifdef TEMPREGDEBUG}
        reg_releaser[r]:=curptree^;
        testregisters32;
{$endif TEMPREGDEBUG}
      end;


    { tries to allocate the passed register, if possible }
    function trgobj.getexplicitregisterint(list:Taasmoutput;r:Tnewregister):Tregister;

    var r2:Tregister;

    begin
      if (r shr 8) in unusedregsint then
        begin
{$ifndef newra}
          dec(countunusedregsint);
{$ifdef TEMPREGDEBUG}
          if curptree^^.usableregs-countunusedregsint>curptree^^.registers32 then
            internalerror(10);
          reg_user[r shr 8]:=curptree^;
{$endif TEMPREGDEBUG}
{$endif newra}
          exclude(unusedregsint,r shr 8);
          include(usedintinproc,r shr 8);
        {$ifndef newra}
          include(usedintbyproc,r shr 8);
        {$endif}
          r2.enum:=R_INTREGISTER;
          r2.number:=r;
          list.concat(tai_regalloc.alloc(r2));
{$ifdef TEMPREGDEBUG}
          testregisters32;
{$endif TEMPREGDEBUG}
         end
       else
         internalerror(200301103);
       getexplicitregisterint:=r2;
    end;


    { tries to allocate the passed register, if possible }
    function trgobj.getexplicitregisterfpu(list : taasmoutput; r : Toldregister) : tregister;

    var r2:Tregister;

      begin
         if r in unusedregsfpu then
           begin
              dec(countunusedregsfpu);
{$ifdef TEMPREGDEBUG}
              if curptree^^.usableregs-countunusedregsint>curptree^^.registers32 then
                internalerror(10);
              reg_user[r]:=curptree^;
{$endif TEMPREGDEBUG}
              exclude(unusedregsfpu,r);
              include(usedinproc,r);
              include(usedbyproc,r);
              r2.enum:=r;
              list.concat(tai_regalloc.alloc(r2));
              getexplicitregisterfpu:=r2;
{$ifdef TEMPREGDEBUG}
              testregisters32;
{$endif TEMPREGDEBUG}
           end
         else
{$warning Size for FPU reg is maybe not correct}
           getexplicitregisterfpu:=getregisterfpu(list,OS_F32);
      end;

    function trgobj.getregisterfpu(list: taasmoutput;size:Tcgsize) : tregister;

      begin
        if countunusedregsfpu=0 then
          internalerror(10);
        result := getregistergen(list,firstsavefpureg,lastsavefpureg,
          unusedregsfpu,countunusedregsfpu);
      end;


    procedure trgobj.ungetregisterfpu(list : taasmoutput; r : tregister);

      begin
         ungetregistergen(list,r,usableregsfpu,unusedregsfpu,
           countunusedregsfpu);
      end;


    function trgobj.getregistermm(list: taasmoutput) : tregister;
      begin
        if countunusedregsmm=0 then
           internalerror(10);
       result := getregistergen(list,firstsavemmreg,lastsavemmreg,
                   unusedregsmm,countunusedregsmm);
      end;


    procedure trgobj.ungetregistermm(list: taasmoutput; r: tregister);
      begin
       ungetregistergen(list,r,usableregsmm,unusedregsmm,
         countunusedregsmm);
      end;


    function trgobj.getaddressregister(list:Taasmoutput): tregister;
      begin
        {An address register is OS_INT per definition.}
        result := getregisterint(list,OS_INT);
      end;


    procedure trgobj.ungetaddressregister(list: taasmoutput; r: tregister);
      begin
        ungetregisterint(list,r);
      end;


    function trgobj.isaddressregister(reg: tregister): boolean;
      begin
        if reg.number<>0 then; { remove warning }
        result := true;
      end;


    procedure trgobj.ungetregister(list: taasmoutput; r : tregister);

      begin
         if r.enum=R_NO then
           exit;
         if r.enum>lastreg then
          internalerror(200301081);
         if r.enum in intregs then
           ungetregisterint(list,r)
         else if r.enum in fpuregs then
           ungetregisterfpu(list,r)
         else if r.enum in mmregs then
           ungetregistermm(list,r)
         else if r.enum in addrregs then
           ungetaddressregister(list,r)
         else internalerror(2002070602);
      end;


    procedure Trgobj.cleartempgen;

   {$ifdef newra}
    var i:Tsuperregister;
   {$endif newra}

    begin
    {$ifndef newra}
      countunusedregsint:=countusableregsint;
    {$endif}
      countunusedregsfpu:=countusableregsfpu;
      countunusedregsmm:=countusableregsmm;
      lastintreg:=0;
      maxintreg:=first_imreg;
   {$ifdef newra}
      unusedregsint:=[0..255];
   {$else}
      unusedregsint:=usableregsint;
   {$endif}
      unusedregsfpu:=usableregsfpu;
      unusedregsmm:=usableregsmm;
   {$ifdef newra}
      savedbyproc:=[];
      for i:=low(Tsuperregister) to high(Tsuperregister) do
        begin
          if igraph.adjlist[i]<>nil then
            dispose(igraph.adjlist[i]);
          if movelist[i]<>nil then
            dispose(movelist[i]);
        end;
      fillchar(movelist,sizeof(movelist),0);
      fillchar(igraph,sizeof(igraph),0);
      fillchar(degree,sizeof(degree),0);
      worklist_moves.clear;
      abtlist:='';
   {$endif}
    end;


    procedure trgobj.ungetreference(list : taasmoutput; const ref : treference);

      begin
         if ref.base.number<>NR_NO then
           ungetregisterint(list,ref.base);
       if ref.index.number<>NR_NO then
           ungetregisterint(list,ref.index);
      end;

{$ifndef newra}
    procedure trgobj.saveintregvars(list:Taasmoutput;const s:Tsupregset);

    var r:Tsuperregister;
        hr: tregister;
    begin
      if not(cs_regalloc in aktglobalswitches) then
        exit;
      for r:=firstsaveintreg to lastsaveintreg do
        if (r in is_reg_var_int) and
           (r in s) then
          begin
            hr.number:=r shl 8;
            hr.enum:=R_INTREGISTER;
            store_regvar(list,hr);
          end;
    end;
{$endif}

    procedure trgobj.saveotherregvars(list: taasmoutput; const s: tregisterset);
      var
        r: Tregister;
      begin
        if not(cs_regalloc in aktglobalswitches) then
          exit;
        if firstsavefpureg <> R_NO then
          for r.enum := firstsavefpureg to lastsavefpureg do
            if is_reg_var_other[r.enum] and
               (r.enum in s) then
              store_regvar(list,r);
        if firstsavemmreg <> R_NO then
          for r.enum := firstsavemmreg to lastsavemmreg do
            if is_reg_var_other[r.enum] and
               (r.enum in s) then
              store_regvar(list,r);
      end;

{$ifndef newra}
    procedure trgobj.saveusedintregisters(list:Taasmoutput;
                                          var saved:Tpushedsavedint;
                                          const s:Tsupregset);

    var r:Tsuperregister;
        r2:Tregister;
        hr : treference;

    begin
      usedintinproc:=usedintinproc+s;
      for r:=firstsaveintreg to lastsaveintreg do
        begin
          saved[r].ofs:=reg_not_saved;
          { if the register is used by the calling subroutine and if }
          { it's not a regvar (those are handled separately)         }
          if not (r in is_reg_var_int) and
             (r in s) and
             { and is present in use }
             not(r in unusedregsint) then
            begin
              { then save it }
              tg.GetTemp(list,sizeof(aword),tt_persistent,hr);
              saved[r].ofs:=hr.offset;
              r2.enum:=R_INTREGISTER;
              r2.number:=r shl 8 or R_SUBWHOLE;
              cg.a_load_reg_ref(list,OS_INT,OS_INT,r2,hr);
              cg.a_reg_dealloc(list,r2);
              include(unusedregsint,r);
              inc(countunusedregsint);
            end;
        end;
{$ifdef TEMPREGDEBUG}
      testregisters32;
{$endif TEMPREGDEBUG}
    end;
{$endif}

    procedure trgobj.saveusedotherregisters(list: taasmoutput;
        var saved : tpushedsavedother; const s: tregisterset);

      var
         r : tregister;
         hr : treference;

      begin
        usedinproc:=usedinproc + s;

        { don't try to save the fpu registers if not desired (e.g. for }
        { the 80x86)                                                   }
        if firstsavefpureg <> R_NO then
          for r.enum:=firstsavefpureg to lastsavefpureg do
            begin
              saved[r.enum].ofs:=reg_not_saved;
              { if the register is used by the calling subroutine and if }
              { it's not a regvar (those are handled separately)         }
              if not is_reg_var_other[r.enum] and
                 (r.enum in s) and
                 { and is present in use }
                 not(r.enum in unusedregsfpu) then
                begin
                  { then save it }
                  tg.GetTemp(list,extended_size,tt_persistent,hr);
                  saved[r.enum].ofs:=hr.offset;
                  cg.a_loadfpu_reg_ref(list,OS_FLOAT,r,hr);
                  cg.a_reg_dealloc(list,r);
                  include(unusedregsfpu,r.enum);
                  inc(countunusedregsfpu);
                end;
            end;

        { don't save the vector registers if there's no support for them }
        if firstsavemmreg <> R_NO then
          for r.enum:=firstsavemmreg to lastsavemmreg do
            begin
              saved[r.enum].ofs:=reg_not_saved;
              { if the register is in use and if it's not a regvar (those }
              { are handled separately), save it                          }
              if not is_reg_var_other[r.enum] and
                 (r.enum in s) and
                 { and is present in use }
                 not(r.enum in unusedregsmm) then
                begin
                  { then save it }
                  tg.GetTemp(list,mmreg_size,tt_persistent,hr);
                  saved[r.enum].ofs:=hr.offset;
                  cg.a_loadmm_reg_ref(list,r,hr);
                  cg.a_reg_dealloc(list,r);
                  include(unusedregsmm,r.enum);
                  inc(countunusedregsmm);
               end;
            end;
{$ifdef TEMPREGDEBUG}
        testregisters32;
{$endif TEMPREGDEBUG}
      end;

{$ifndef newra}
    procedure trgobj.restoreusedintregisters(list:Taasmoutput;
                                             const saved:Tpushedsavedint);

    var r:Tsuperregister;
        r2:Tregister;
        hr:Treference;

      begin
        for r:=lastsaveintreg downto firstsaveintreg do
          begin
            if saved[r].ofs <> reg_not_saved then
              begin
                r2.enum:=R_INTREGISTER;
                r2.number:=NR_FRAME_POINTER_REG;
                reference_reset_base(hr,r2,saved[r].ofs);
                r2.enum:=R_INTREGISTER;
                r2.number:=r shl 8 or R_SUBWHOLE;
                cg.a_reg_alloc(list,r2);
                cg.a_load_ref_reg(list,OS_INT,OS_INT,hr,r2);
                if not (r in unusedregsint) then
                  { internalerror(10)
                    in n386cal we always save/restore the reg *state*
                    using save/restoreunusedstate -> the current state
                    may not be real (JM) }
                else
                  begin
                    dec(countunusedregsint);
                    exclude(unusedregsint,r);
                  end;
                tg.UnGetTemp(list,hr);
              end;
          end;
{$ifdef TEMPREGDEBUG}
        testregisters32;
{$endif TEMPREGDEBUG}
      end;
{$endif}

    procedure trgobj.restoreusedotherregisters(list : taasmoutput;
        const saved : tpushedsavedother);

      var
         r,r2 : tregister;
         hr : treference;

      begin
        if firstsavemmreg <> R_NO then
          for r.enum:=lastsavemmreg downto firstsavemmreg do
            begin
              if saved[r.enum].ofs <> reg_not_saved then
                begin
                  r2.enum:=R_INTREGISTER;
                  r2.number:=NR_FRAME_POINTER_REG;
                  reference_reset_base(hr,r2,saved[r.enum].ofs);
                  cg.a_reg_alloc(list,r);
                  cg.a_loadmm_ref_reg(list,hr,r);
                  if not (r.enum in unusedregsmm) then
                    { internalerror(10)
                      in n386cal we always save/restore the reg *state*
                      using save/restoreunusedstate -> the current state
                      may not be real (JM) }
                  else
                    begin
                      dec(countunusedregsmm);
                      exclude(unusedregsmm,r.enum);
                    end;
                  tg.UnGetTemp(list,hr);
                end;
            end;

        if firstsavefpureg <> R_NO then
          for r.enum:=lastsavefpureg downto firstsavefpureg do
            begin
              if saved[r.enum].ofs <> reg_not_saved then
                begin
                  r2.enum:=R_INTREGISTER;
                  r2.number:=NR_FRAME_POINTER_REG;
                  reference_reset_base(hr,r2,saved[r.enum].ofs);
                  cg.a_reg_alloc(list,r);
                  cg.a_loadfpu_ref_reg(list,OS_FLOAT,hr,r);
                  if not (r.enum in unusedregsfpu) then
                    { internalerror(10)
                      in n386cal we always save/restore the reg *state*
                      using save/restoreunusedstate -> the current state
                      may not be real (JM) }
                  else
                    begin
                      dec(countunusedregsfpu);
                      exclude(unusedregsfpu,r.enum);
                    end;
                  tg.UnGetTemp(list,hr);
                end;
            end;
{$ifdef TEMPREGDEBUG}
        testregisters32;
{$endif TEMPREGDEBUG}
      end;

{$ifndef newra}
    procedure trgobj.incrementintregisterpushed(const s:Tsupregset);

    var regi:Tsuperregister;

    begin
{$ifdef i386}
      for regi:=firstsaveintreg to lastsaveintreg do
        begin
          if (regi in s) then
            inc(reg_pushes_int[regi],t_times*2);
        end;
{$endif i386}
    end;
{$endif}

    procedure trgobj.incrementotherregisterpushed(const s:Tregisterset);

      var
         regi : Toldregister;

      begin
{$ifdef i386}
         if firstsavefpureg <> R_NO then
           for regi:=firstsavefpureg to lastsavefpureg do
             begin
                if (regi in s) then
                  inc(reg_pushes_other[regi],t_times*2);
             end;
         if firstsavemmreg <> R_NO then
           for regi:=firstsavemmreg to lastsavemmreg do
             begin
                if (regi in s) then
                  inc(reg_pushes_other[regi],t_times*2);
             end;
{$endif i386}
      end;


    procedure trgobj.clearregistercount;

      begin
      {$ifndef newra}
        fillchar(reg_pushes_int,sizeof(reg_pushes_int),0);
      {$endif}
        fillchar(reg_pushes_other,sizeof(reg_pushes_other),0);
{ifndef i386}
        { all used registers will have to be saved at the start and restored }
        { at the end, but otoh regpara's do not have to be saved to memory   }
        { at the start (there is a move from regpara to regvar most of the   }
        { time though) -> set cost to 100+20                                 }
      {$ifndef newra}
        filldword(reg_pushes_int[firstsaveintreg],lastsaveintreg-firstsaveintreg+1,120);
      {$endif}
        filldword(reg_pushes_other[firstsavefpureg],ord(lastsavefpureg)-ord(firstsavefpureg)+1,120);
{endif not i386}
        fillchar(is_reg_var_other,sizeof(is_reg_var_other),false);
        is_reg_var_int:=[];
        fillchar(regvar_loaded_other,sizeof(regvar_loaded_other),false);
        regvar_loaded_int:=[];
      end;


    procedure trgobj.resetusableregisters;

      begin
        { initialize fields with constant values from cpubase }
        countusableregsint := cpubase.c_countusableregsint;
        countusableregsfpu := cpubase.c_countusableregsfpu;
        countusableregsmm := cpubase.c_countusableregsmm;
        usableregsint := cpubase.usableregsint;
        usableregsfpu := cpubase.usableregsfpu;
        usableregsmm := cpubase.usableregsmm;
        clearregistercount;
      end;


    procedure trgobj.makeregvarint(reg:Tsuperregister);
    begin
      dec(countusableregsint);
    {$ifndef newra}
      dec(countunusedregsint);
    {$endif}
      exclude(usableregsint,reg);
      exclude(unusedregsint,reg);
      include(is_reg_var_int,reg);
{$ifndef i386}
      include(usedintbyproc,reg);
{$endif not i386}
    end;

    procedure trgobj.makeregvarother(reg: tregister);
      begin
        if reg.enum>lastreg then
          internalerror(200301081);
        if reg.enum in intregs then
          internalerror(200301151)
        else if reg.enum in fpuregs then
          begin
             dec(countusableregsfpu);
             dec(countunusedregsfpu);
             exclude(usableregsfpu,reg.enum);
             exclude(unusedregsfpu,reg.enum);
{$ifndef i386}
             include(usedbyproc,reg.enum);
{$endif not i386}
          end
        else if reg.enum in mmregs then
          begin
             dec(countusableregsmm);
             dec(countunusedregsmm);
             exclude(usableregsmm,reg.enum);
             exclude(unusedregsmm,reg.enum);
{$ifndef i386}
             include(usedbyproc,reg.enum);
{$endif not i386}
          end;
        is_reg_var_other[reg.enum]:=true;
      end;


{$ifdef TEMPREGDEBUG}
    procedure trgobj.testregisters;
      var
        r: tregister;
        test : byte;
      begin
        test:=0;
        for r := firstsaveintreg to lastsaveintreg do
          inc(test,ord(r in unusedregsint));
        if test<>countunusedregsint then
          internalerror(10);
      end;
{$endif TEMPREGDEBUG}


    procedure trgobj.saveStateForInline(var state: pointer);
      begin
        new(psavedstate(state));
        psavedstate(state)^.unusedregsint := unusedregsint;
        psavedstate(state)^.usableregsint := usableregsint;
        psavedstate(state)^.unusedregsfpu := unusedregsfpu;
        psavedstate(state)^.usableregsfpu := usableregsfpu;
        psavedstate(state)^.unusedregsmm := unusedregsmm;
        psavedstate(state)^.usableregsmm := usableregsmm;
      {$ifndef newra}
        psavedstate(state)^.countunusedregsint := countunusedregsint;
      {$endif}
        psavedstate(state)^.countunusedregsfpu := countunusedregsfpu;
        psavedstate(state)^.countunusedregsmm := countunusedregsmm;
        psavedstate(state)^.countusableregsint := countusableregsint;
        psavedstate(state)^.countusableregsfpu := countusableregsfpu;
        psavedstate(state)^.countusableregsmm := countusableregsmm;
        psavedstate(state)^.usedinproc := usedinproc;
        psavedstate(state)^.usedbyproc := usedbyproc;
      {$ifndef newra}
        psavedstate(state)^.reg_pushes_int := reg_pushes_int;
      {$endif}
        psavedstate(state)^.reg_pushes_other := reg_pushes_other;
        psavedstate(state)^.is_reg_var_int := is_reg_var_int;
        psavedstate(state)^.is_reg_var_other := is_reg_var_other;
        psavedstate(state)^.regvar_loaded_int := regvar_loaded_int;
        psavedstate(state)^.regvar_loaded_other := regvar_loaded_other;
{$ifdef TEMPREGDEBUG}
        psavedstate(state)^.reg_user := reg_user;
        psavedstate(state)^.reg_releaser := reg_releaser;
{$endif TEMPREGDEBUG}
      end;


    procedure trgobj.restoreStateAfterInline(var state: pointer);
      begin
        unusedregsint := psavedstate(state)^.unusedregsint;
        usableregsint := psavedstate(state)^.usableregsint;
        unusedregsfpu := psavedstate(state)^.unusedregsfpu;
        usableregsfpu := psavedstate(state)^.usableregsfpu;
        unusedregsmm := psavedstate(state)^.unusedregsmm;
        usableregsmm := psavedstate(state)^.usableregsmm;
      {$ifndef newra}
        countunusedregsint := psavedstate(state)^.countunusedregsint;
      {$endif}
        countunusedregsfpu := psavedstate(state)^.countunusedregsfpu;
        countunusedregsmm := psavedstate(state)^.countunusedregsmm;
        countusableregsint := psavedstate(state)^.countusableregsint;
        countusableregsfpu := psavedstate(state)^.countusableregsfpu;
        countusableregsmm := psavedstate(state)^.countusableregsmm;
        usedinproc := psavedstate(state)^.usedinproc;
        usedbyproc := psavedstate(state)^.usedbyproc;
      {$ifndef newra}
        reg_pushes_int := psavedstate(state)^.reg_pushes_int;
      {$endif}
        reg_pushes_other := psavedstate(state)^.reg_pushes_other;
        is_reg_var_int := psavedstate(state)^.is_reg_var_int;
        is_reg_var_other := psavedstate(state)^.is_reg_var_other;
        regvar_loaded_other := psavedstate(state)^.regvar_loaded_other;
        regvar_loaded_int := psavedstate(state)^.regvar_loaded_int;
{$ifdef TEMPREGDEBUG}
        reg_user := psavedstate(state)^.reg_user;
        reg_releaser := psavedstate(state)^.reg_releaser;
{$endif TEMPREGDEBUG}
        dispose(psavedstate(state));
        state := nil;
      end;


    procedure trgobj.saveUnusedState(var state: pointer);
      begin
        new(punusedstate(state));
        punusedstate(state)^.unusedregsint := unusedregsint;
        punusedstate(state)^.unusedregsfpu := unusedregsfpu;
        punusedstate(state)^.unusedregsmm := unusedregsmm;
      {$ifndef newra}
        punusedstate(state)^.countunusedregsint := countunusedregsint;
      {$endif}
        punusedstate(state)^.countunusedregsfpu := countunusedregsfpu;
        punusedstate(state)^.countunusedregsmm := countunusedregsmm;
      end;


    procedure trgobj.restoreUnusedState(var state: pointer);
      begin
        unusedregsint := punusedstate(state)^.unusedregsint;
        unusedregsfpu := punusedstate(state)^.unusedregsfpu;
        unusedregsmm := punusedstate(state)^.unusedregsmm;
      {$ifndef newra}
        countunusedregsint := punusedstate(state)^.countunusedregsint;
      {$endif}
        countunusedregsfpu := punusedstate(state)^.countunusedregsfpu;
        countunusedregsmm := punusedstate(state)^.countunusedregsmm;
        dispose(punusedstate(state));
        state := nil;
      end;

{$ifdef newra}
    procedure Trgobj.add_edge(u,v:Tsuperregister);

    {This procedure will add an edge to the virtual interference graph.}

      procedure addadj(u,v:Tsuperregister);

      begin
        if igraph.adjlist[u]=nil then
          begin
            getmem(igraph.adjlist[u],16);
            igraph.adjlist[u]^:='';
          end
        else if (length(igraph.adjlist[u]^) and 15)=15 then
          reallocmem(igraph.adjlist[u],length(igraph.adjlist[u]^)+16);
        igraph.adjlist[u]^:=igraph.adjlist[u]^+char(v);
      end;

    begin
      if (u<>v) and not(v in igraph.bitmap[u]) then
        begin
          include(igraph.bitmap[u],v);
          include(igraph.bitmap[v],u);
          {Precoloured nodes are not stored in the interference graph.}
          if not(u in [first_supreg..last_supreg]) then
            begin
              addadj(u,v);
              inc(degree[u]);
            end;
          if not(v in [first_supreg..last_supreg]) then
            begin
              addadj(v,u);
              inc(degree[v]);
            end;
        end;
    end;

    procedure Trgobj.add_edges_used(u:Tsuperregister);

    var i:Tsuperregister;

    begin
      for i:=1 to maxintreg do
        if not(i in unusedregsint) then
          add_edge(u,i);
    end;

{$ifdef ra_debug}
    procedure Trgobj.writegraph;

    {This procedure writes out the current interference graph in the
    register allocator.}


    var f:text;
        i,j:Tsuperregister;

    begin
      assign(f,'igraph'+char(48+random(10))+char(48+random(10)));
      rewrite(f);
      writeln(f,'Interference graph');
      writeln(f);
      write(f,'    ');
      for i:=0 to 15 do
        for j:=0 to 15 do
          write(f,hexstr(i,1));
      writeln(f);
      write(f,'    ');
      for i:=0 to 15 do
        write(f,'0123456789ABCDEF');
      writeln(f);
      for i:=0 to 255 do
        begin
          write(f,hexstr(i,2):4);
          for j:=0 to 255 do
            if j in igraph.bitmap[i] then
              write(f,'*')
            else
              write(f,'-');
          writeln(f);
        end;
      close(f);
    end;
{$endif}

    procedure Trgobj.add_to_movelist(u:Tsuperregister;data:Tlinkedlistitem);

    begin
      if movelist[u]=nil then
        begin
          getmem(movelist[u],64);
          movelist[u]^.count:=0;
        end
      else if (movelist[u]^.count and 15)=15 then
        reallocmem(movelist[u],(movelist[u]^.count+1)*4+64);
      movelist[u]^.data[movelist[u]^.count]:=data;
      inc(movelist[u]^.count);
    end;

    procedure Trgobj.add_move_instruction(instr:Taicpu);

    {This procedure notifies a certain as a move instruction so the
     register allocator can try to eliminate it.}

    var i:Tmoveins;
        ssupreg,dsupreg:Tsuperregister;

    begin
      i:=Tmoveins.create;
      i.moveset:=ms_worklist_moves;
      i.instruction:=instr;
      worklist_moves.insert(i);
      ssupreg:=instr.oper[0].reg.number shr 8;
      add_to_movelist(ssupreg,i);
      dsupreg:=instr.oper[1].reg.number shr 8;
      if ssupreg<>dsupreg then
        {Avoid adding the same move instruction twice to a single register.}
        add_to_movelist(dsupreg,i);
      i.x:=ssupreg;
      i.y:=dsupreg;
    end;

    function Trgobj.move_related(n:Tsuperregister):boolean;

    var i:cardinal;

    begin
      move_related:=false;
      if movelist[n]<>nil then
        begin
          for i:=0 to movelist[n]^.count-1 do
            if Tmoveins(movelist[n]^.data[i]).moveset in
               [ms_worklist_moves,ms_active_moves] then
              begin
                move_related:=true;
                break;
              end;
        end;
    end;

    procedure Trgobj.make_work_list;

    var n:Tsuperregister;

    begin
      {If we have 7 cpu registers, and the degree of a node is 7, we cannot
       assign it to any of the registers, thus it is significant.}
      for n:=first_imreg to maxintreg do
        if degree[n]>=cpu_registers then
          spillworklist:=spillworklist+char(n)
        else if move_related(n) then
          freezeworklist:=freezeworklist+char(n)
        else
          simplifyworklist:=simplifyworklist+char(n);
    end;

    procedure Trgobj.prepare_colouring;

    begin
      make_work_list;
      active_moves:=Tlinkedlist.create;
      frozen_moves:=Tlinkedlist.create;
      coalesced_moves:=Tlinkedlist.create;
      constrained_moves:=Tlinkedlist.create;
      fillchar(alias,sizeof(alias),0);
      coalescednodes:='';
      selectstack:='';
    end;

    procedure Trgobj.enable_moves(n:Tsuperregister);

    var m:Tlinkedlistitem;
        i:cardinal;

    begin
      if movelist[n]<>nil then
        for i:=0 to movelist[n]^.count-1 do
          begin
            m:=movelist[n]^.data[i];
            if Tmoveins(m).moveset in [ms_worklist_moves,ms_active_moves] then
              begin
                if Tmoveins(m).moveset=ms_active_moves then
                  begin
                    {Move m from the set active_moves to the set worklist_moves.}
                    active_moves.remove(m);
                    Tmoveins(m).moveset:=ms_worklist_moves;
                    worklist_moves.concat(m);
                  end;
              end;
          end;
    end;

    procedure Trgobj.decrement_degree(m:Tsuperregister);

    var adj:Pstring;
        d:byte;
        i,p:byte;
        n:char;

    begin
      d:=degree[m];
      dec(degree[m]);
      if d=cpu_registers then
        begin
          {Enable moves for m.}
          enable_moves(m);
          {Enable moves for adjacent.}
          adj:=igraph.adjlist[m];
          if adj<>nil then
            for i:=1 to length(adj^) do
              begin
                n:=adj^[i];
                if (pos(n,selectstack) or pos(n,coalescednodes))=0 then
                  enable_moves(Tsuperregister(n));
              end;
          {Remove the node from the spillworklist.}
          p:=pos(char(m),spillworklist);
          if p=0 then
            internalerror(200305301); {must be found}
          if length(spillworklist)>1 then
            spillworklist[p]:=spillworklist[length(spillworklist)];
          dec(spillworklist[0]);

          if move_related(m) then
            freezeworklist:=freezeworklist+char(m)
          else
            simplifyworklist:=simplifyworklist+char(m);
        end;
    end;

    procedure Trgobj.simplify;

    var adj:Pstring;
        i,min,p:byte;
        m:char;
        n:Tsuperregister;

    begin
      {We the element with the least interferences out of the
       simplifyworklist.}
      min:=$ff;
      p:=1;
      for i:=1 to length(simplifyworklist) do
        begin
          adj:=igraph.adjlist[Tsuperregister(simplifyworklist[i])];
          if adj=nil then
            begin
              min:=0;
              break;  {We won't find smaller ones.}
            end
          else
            if length(adj^)<min then
              begin
                min:=length(adj^);
                if min=0 then
                  break;  {We won't find smaller ones.}
                p:=i;
              end;
        end;
      n:=Tsuperregister(simplifyworklist[p]);
      delete(simplifyworklist,p,1);

      {Push it on the selectstack.}
      selectstack:=selectstack+char(n);
      adj:=igraph.adjlist[n];
      if adj<>nil then
        for i:=1 to length(adj^) do
          begin
            m:=adj^[i];
            if (pos(m,selectstack) or pos(m,coalescednodes))=0 then
               decrement_degree(Tsuperregister(m));
          end;
    end;

    function Trgobj.get_alias(n:Tsuperregister):Tsuperregister;

    begin
      while pos(char(n),coalescednodes)<>0 do
        n:=alias[n];
      get_alias:=n;
    end;

    procedure Trgobj.add_worklist(u:Tsuperregister);

    begin
      if not(u in [first_supreg..last_supreg]) and not move_related(u) and
         (degree[u]<cpu_registers) then
        begin
          delete(freezeworklist,pos(char(u),freezeworklist),1);
          simplifyworklist:=simplifyworklist+char(u);
        end;
    end;

    function Trgobj.adjacent_ok(u,v:Tsuperregister):boolean;

    {Check wether u and v should be coalesced. u is precoloured.}

      function ok(t,r:Tsuperregister):boolean;

      begin
        ok:=(degree[t]<cpu_registers) or
            (t in [first_supreg..last_supreg]) or
            (r in igraph.bitmap[t]);
      end;

    var adj:Pstring;
        i:byte;
        t:char;

    begin
      adjacent_ok:=true;
      adj:=igraph.adjlist[v];
      if adj<>nil then
        for i:=1 to length(adj^) do
          begin
            t:=adj^[i];
            if (pos(t,selectstack) or pos(t,coalescednodes))=0 then
              if not ok(Tsuperregister(t),u) then
                begin
                  adjacent_ok:=false;
                  break;
                end;
          end;
    end;

    function Trgobj.conservative(u,v:Tsuperregister):boolean;

    var adj:Pstring;
        done:set of char; {To prevent that we count nodes twice.}
        i,k:byte;
        n:char;

    begin
      k:=0;
      done:=[];
      adj:=igraph.adjlist[u];
      if adj<>nil then
        for i:=1 to length(adj^) do
          begin
            n:=adj^[i];
            if (pos(n,selectstack) or pos(n,coalescednodes))=0 then
              begin
                include(done,n);
                if degree[Tsuperregister(n)]>=cpu_registers then
                  inc(k);
              end;
          end;
      adj:=igraph.adjlist[v];
      if adj<>nil then
        for i:=1 to length(adj^) do
          begin
            n:=adj^[i];
            if ((pos(n,selectstack) or pos(n,coalescednodes))=0) and
               not (n in done) and
               (degree[Tsuperregister(n)]>=cpu_registers) then
              inc(k);
         end;
      conservative:=(k<cpu_registers);
    end;

    procedure Trgobj.combine(u,v:Tsuperregister);

    var add:boolean;
        adj:Pstring;
        i,p:byte;
        n,o:cardinal;
        t:char;

    begin
      p:=pos(char(v),freezeworklist);
      if p<>0 then
        delete(freezeworklist,p,1)
      else
        delete(spillworklist,pos(char(v),spillworklist),1);
      coalescednodes:=coalescednodes+char(v);
      alias[v]:=u;

      {Combine both movelists. Since the movelists are sets, only add
       elements that are not already present.}
      for n:=0 to movelist[v]^.count-1 do
        begin
          add:=true;
          for o:=0 to movelist[u]^.count-1 do
            if movelist[u]^.data[o]=movelist[v]^.data[n] then
              begin
                add:=false;
                break;
              end;
          if add then
            add_to_movelist(u,movelist[v]^.data[n]);
        end;
      enable_moves(v);

      adj:=igraph.adjlist[v];
      if adj<>nil then
        for i:=1 to length(adj^) do
          begin
            t:=adj^[i];
            if (pos(t,selectstack) or pos(t,coalescednodes))=0 then
              begin
                decrement_degree(Tsuperregister(t));
                add_edge(Tsuperregister(t),u);
              end;
          end;
      p:=pos(char(u),freezeworklist);
      if (degree[u]>=cpu_registers) and (p<>0) then
        begin
          delete(freezeworklist,p,1);
          spillworklist:=spillworklist+char(u);
        end;
    end;

    procedure Trgobj.coalesce;

    var m:Tmoveins;
        x,y,u,v:Tsuperregister;

    begin
      m:=Tmoveins(worklist_moves.getfirst);
      x:=get_alias(m.instruction.oper[0].reg.number shr 8);
      y:=get_alias(m.instruction.oper[1].reg.number shr 8);
      if y in [first_supreg..last_supreg] then
        begin
          u:=y;
          v:=x;
        end
      else
        begin
          u:=x;
          v:=y;
        end;
      if (u=v) then
        begin
          m.moveset:=ms_coalesced_moves;  {Already coalesced.}
          coalesced_moves.insert(m);
          add_worklist(u);
        end
      {Do u and v interfere? In that case the move is constrained. Two
       precoloured nodes interfere allways. If v is precoloured, by the above
       code u is precoloured, thus interference...}
      else if (v in [first_supreg..last_supreg]) or (u in igraph.bitmap[v]) then
        begin
          m.moveset:=ms_constrained_moves;  {Cannot coalesce yet...}
          constrained_moves.insert(m);
          add_worklist(u);
          add_worklist(v);
        end
      {Next test: is it possible and a good idea to coalesce??}
      else if ((u in [first_supreg..last_supreg]) and adjacent_ok(u,v)) or
              (not(u in [first_supreg..last_supreg]) and conservative(u,v)) then
        begin
          m.moveset:=ms_coalesced_moves;  {Move coalesced!}
          coalesced_moves.insert(m);
          combine(u,v);
          add_worklist(u);
        end
      else
        begin
          m.moveset:=ms_active_moves;
          active_moves.insert(m);
        end;
    end;

    procedure Trgobj.freeze_moves(u:Tsuperregister);

    var i:cardinal;
        m:Tlinkedlistitem;
        v,x,y:Tsuperregister;

    begin
      if movelist[u]<>nil then
        for i:=0 to movelist[u]^.count-1 do
          begin
            m:=movelist[u]^.data[i];
            if Tmoveins(m).moveset in [ms_worklist_moves,ms_active_moves] then
              begin
                x:=Tmoveins(m).instruction.oper[0].reg.number shr 8;
                y:=Tmoveins(m).instruction.oper[1].reg.number shr 8;
                if get_alias(y)=get_alias(u) then
                  v:=get_alias(x)
                else
                  v:=get_alias(y);
                {Move m from active_moves/worklist_moves to frozen_moves.}
                if Tmoveins(m).moveset=ms_active_moves then
                  active_moves.remove(m)
                else
                  worklist_moves.remove(m);
                Tmoveins(m).moveset:=ms_frozen_moves;
                frozen_moves.insert(m);

                if not(move_related(v)) and (degree[v]<cpu_registers) then
                  begin
                    delete(freezeworklist,pos(char(v),freezeworklist),1);
                    simplifyworklist:=simplifyworklist+char(v);
                  end;
              end;
          end;
    end;

    procedure Trgobj.freeze;

    var n:Tsuperregister;

    begin
      {We need to take a random element out of the freezeworklist. We take
       the last element. Dirty code!}
      n:=Tsuperregister(freezeworklist[byte(freezeworklist[0])]);
      dec(freezeworklist[0]);
      {Add it to the simplifyworklist.}
      simplifyworklist:=simplifyworklist+char(n);
      freeze_moves(n);
    end;

    procedure Trgobj.select_spill;

    var n:char;

    begin
      {This code is WAY too naive. We need not to select just a register, but
       the register that is used the least...}
      n:=spillworklist[byte(spillworklist[0])];
      dec(spillworklist[0]);
      simplifyworklist:=simplifyworklist+n;
      freeze_moves(Tsuperregister(n));
    end;

    procedure Trgobj.assign_colours;

    {Assign_colours assigns the actual colours to the registers.}

    var adj:Pstring;
        i,j,k:byte;
        n,a:Tsuperregister;
        adj_colours,colourednodes:set of Tsuperregister;
        w:char;

    begin
      spillednodes:='';
      {Colour the cpu registers...}
      colourednodes:=[first_supreg..last_supreg];
      for i:=first_supreg to last_supreg do
        colour[i]:=i;
      {Now colour the imaginary registers on the select-stack.}
      for i:=length(selectstack) downto 1 do
        begin
          n:=Tsuperregister(selectstack[i]);
          {Create a list of colours that we cannot assign to n.}
          adj_colours:=[];
          adj:=igraph.adjlist[n];
          if adj<>nil then
            for j:=1 to length(adj^) do
              begin
                w:=adj^[j];
                a:=get_alias(Tsuperregister(w));
                if a in colourednodes then
                  include(adj_colours,colour[a]);
              end;
          {Assume a spill by default...}
          spillednodes:=spillednodes+char(n);
          {Search for a colour not in this list.}
          for k:=1 to cpu_registers do
            if not(k in adj_colours) then
              begin
                colour[n]:=k;
                dec(spillednodes[0]);  {Colour found: no spill.}
                include(colourednodes,n);
                if n in usedintinproc then
                  include(usedintinproc,k);
                break;
              end;
        end;
      {Finally colour the nodes that were coalesced.}
      for i:=1 to length(coalescednodes) do
        begin
          n:=Tsuperregister(coalescednodes[i]);
          k:=get_alias(n);
          colour[n]:=colour[k];
          if n in usedintinproc then
            include(usedintinproc,colour[k]);
        end;
    {$ifdef ra_debug}
      for i:=first_imreg to maxintreg do
        writeln(i:4,'   ',colour[i]:4)
    {$endif}
    end;

    procedure Trgobj.colour_registers;

    begin
      repeat
        if length(simplifyworklist)<>0 then
          simplify
        else if not(worklist_moves.empty) then
          coalesce
        else if length(freezeworklist)<>0 then
          freeze
        else if length(spillworklist)<>0 then
          select_spill;
      until (length(simplifyworklist) or
             byte(not(worklist_moves.empty)) or
             length(freezeworklist) or
             length(spillworklist)
            )=0;
      assign_colours;
    end;

    procedure Trgobj.epilogue_colouring;

{
      procedure move_to_worklist_moves(list:Tlinkedlist);

      var p:Tlinkedlistitem;

      begin
        p:=list.first;
        while p<>nil do
          begin
            Tmoveins(p).moveset:=ms_worklist_moves;
            p:=p.next;
          end;
        worklist_moves.concatlist(list);
      end;
}

    var i:Tsuperregister;

    begin
      worklist_moves.clear;
{$ifdef Principle_wrong_by_definition}
      {Move everything back to worklist_moves.}
      move_to_worklist_moves(active_moves);
      move_to_worklist_moves(frozen_moves);
      move_to_worklist_moves(coalesced_moves);
      move_to_worklist_moves(constrained_moves);
{$endif}
      active_moves.destroy;
      active_moves:=nil;
      frozen_moves.destroy;
      frozen_moves:=nil;
      coalesced_moves.destroy;
      coalesced_moves:=nil;
      constrained_moves.destroy;
      constrained_moves:=nil;
      for i:=0 to 255 do
        if movelist[i]<>nil then
          begin
            dispose(movelist[i]);
            movelist[i]:=0;
          end;
    end;


    procedure Trgobj.clear_interferences(u:Tsuperregister);

    {Remove node u from the interference graph and remove all collected
     move instructions it is associated with.}

    var i:byte;
        j,k,count:cardinal;
        v:Tsuperregister;
        m,n:Tmoveins;

    begin
      if igraph.adjlist[u]<>nil then
        begin
          for i:=1 to length(igraph.adjlist[u]^) do
            begin
              v:=Tsuperregister(igraph.adjlist[u]^[i]);
              {Remove (u,v) and (v,u) from bitmap.}
              exclude(igraph.bitmap[u],v);
              exclude(igraph.bitmap[v],u);
              {Remove (v,u) from adjacency list.}
              if igraph.adjlist[v]<>nil then
                begin
                  delete(igraph.adjlist[v]^,pos(char(v),igraph.adjlist[v]^),1);
                  if length(igraph.adjlist[v]^)=0 then
                    begin
                      dispose(igraph.adjlist[v]);
                      igraph.adjlist[v]:=nil;
                    end;
                end;
            end;
          {Remove ( u,* ) from adjacency list.}
          dispose(igraph.adjlist[u]);
          igraph.adjlist[u]:=nil;
        end;
{$ifdef Principle_wrong_by_definition}
      {Now remove the moves.}
      if movelist[u]<>nil then
        begin
          for j:=0 to movelist[u]^.count-1 do
            begin
              m:=Tmoveins(movelist[u]^.data[j]);
              {Get the other register of the move instruction.}
              v:=m.instruction.oper[0].reg.number shr 8;
              if v=u then
                v:=m.instruction.oper[1].reg.number shr 8;
              repeat
                repeat
                  if (u<>v) and (movelist[v]<>nil) then
                    begin
                      {Remove the move from it's movelist.}
                      count:=movelist[v]^.count-1;
                      for k:=0 to count do
                        if m=movelist[v]^.data[k] then
                          begin
                            if k<>count then
                              movelist[v]^.data[k]:=movelist[v]^.data[count];
                            dec(movelist[v]^.count);
                            if count=0 then
                              begin
                                dispose(movelist[v]);
                                movelist[v]:=nil;
                              end;
                            break;
                          end;
                    end;
                  {The complexity is enourmous: the register might have been
                   coalesced. In that case it's movelists have been added to
                   it's coalescing alias. (DM)}
                  v:=alias[v];
                until v=0;
                {And also register u might have been coalesced.}
                u:=alias[u];
              until u=0;

              case m.moveset of
                ms_coalesced_moves:
                  coalesced_moves.remove(m);
                ms_constrained_moves:
                  constrained_moves.remove(m);
                ms_frozen_moves:
                  frozen_moves.remove(m);
                ms_worklist_moves:
                  worklist_moves.remove(m);
                ms_active_moves:
                  active_moves.remove(m);
              end;
            end;
          dispose(movelist[u]);
          movelist[u]:=nil;
        end;
{$endif}
    end;

    procedure Trgobj.getregisterintinline(list:Taasmoutput;position:Tai;subreg:Tsubregister;var result:Tregister);

    var i:Tsuperregister;
        r:Tregister;

    begin
      if not (lastintreg in [first_imreg..last_imreg]) then
        lastintreg:=first_imreg;
      i:=lastintreg;
      repeat
        if i=last_imreg then
          i:=first_imreg
        else
          inc(i);
        if (i in unusedregsint) and (pos(char(i),abtlist)=0) then
          begin
            exclude(unusedregsint,i);
            include(usedintinproc,i);
            r.enum:=R_INTREGISTER;
            r.number:=i shl 8 or subreg;
            if position=nil then
              list.insert(Tai_regalloc.alloc(r))
            else
              list.insertafter(Tai_regalloc.alloc(r),position);
            result:=r;
            lastintreg:=i;
            if i>maxintreg then
              maxintreg:=i;
            add_edges_used(i);
            add_constraints(result.number);
            exit;
          end;
      until i=lastintreg;
      internalerror(10);
    end;

    {In some cases we can get in big trouble. See this example:

     ; register reg23d released
     ; register eax allocated
     ; register ebx allocated
     ; register ecx allocated
     ; register edx allocated
     ; register esi allocated
     ; register edi allocated
     call [reg23d]

    This code is ok, *except* when reg23d is spilled. In that case the
    spilled would introduce a help register which can never get
    allocated to a real register because it interferes with all of them.

    To solve this we introduce the ABT ("avoid big trouble :)" ) registers.

    If you allocate an ABT register you get a register that has less
    than cpu_register interferences and will not be allocated ever again
    by the normal register get procedures. In other words it is for sure it
    will never get spilled.}

    function Trgobj.getabtregisterint(list:Taasmoutput;size:Tcgsize):Tregister;

    var i:Tsuperregister;
        r:Tregister;
        found:boolean;

    begin
      if not (lastintreg in [first_imreg..last_imreg]) then
        lastintreg:=first_imreg;
      found:=false;
      for i:=1 to length(abtlist) do
        if Tsuperregister(abtlist[i]) in unusedregsint then
          begin
            found:=true;
            break;
          end;
      i:=lastintreg;
      repeat
        if i=last_imreg then
          i:=first_imreg
        else
          inc(i);
        if (i in unusedregsint) and ((igraph.adjlist[i]=nil) or (length(igraph.adjlist[i]^)<cpu_registers)) then
          begin
            found:=true;
            break;
          end;
      until i=lastintreg;
      if found then
        begin
          exclude(unusedregsint,i);
          include(usedintinproc,i);
          r.enum:=R_INTREGISTER;
          r.number:=i shl 8 or cgsize2subreg(size);
          list.concat(Tai_regalloc.alloc(r));
          getabtregisterint:=r;
          lastintreg:=i;
          if i>maxintreg then
            maxintreg:=i;
          add_edges_used(i);
          if pos(char(i),abtlist)=0 then
            abtlist:=abtlist+char(i);
        end
      else
        internalerror(10);
{$ifdef newra}
      add_constraints(getabtregisterint.number);
{$endif}
    end;

    procedure Trgobj.ungetregisterintinline(list:Taasmoutput;position:Tai;const r:Tregister);

    var supreg:Tsuperregister;

    begin
      if r.enum<=lastreg then
        internalerror(2003010803);
      supreg:=r.number shr 8;
      { takes much time }
      include(unusedregsint,supreg);
      if position=nil then
        list.insert(Tai_regalloc.dealloc(r))
      else
        list.insertafter(Tai_regalloc.dealloc(r),position);
      add_edges_used(supreg);
    end;

    function Trgobj.spill_registers(list:Taasmoutput;const regs_to_spill:string):boolean;

    {Returns true if any help registers have been used.}

    var i:byte;
        r:Tsuperregister;
        p,q:Tai;
        regs_to_spill_set:Tsupregset;
        spill_temps:^Tspill_temp_list;

    begin
      spill_registers:=false;
      unusedregsint:=[0..255];
      fillchar(degree,sizeof(degree),0);
      if current_procinfo.framepointer.number=NR_FRAME_POINTER_REG then
        {Make sure the register allocator won't allocate registers into ebp.}
        exclude(rg.unusedregsint,RS_FRAME_POINTER_REG);
      new(spill_temps);
      fillchar(spill_temps^,sizeof(spill_temps^),0);
      regs_to_spill_set:=[];
      for i:=1 to length(regs_to_spill) do
        begin
          {Alternative representation.}
          include(regs_to_spill_set,Tsuperregister(regs_to_spill[i]));
          {Clear all interferences of the spilled register.}
          clear_interferences(Tsuperregister(regs_to_spill[i]));
          {Get a temp for the spilled register.}
          tg.gettemp(list,4,tt_noreuse,spill_temps^[Tsuperregister(regs_to_spill[i])]);
        end;
      p:=Tai(list.first);
      while assigned(p) do
        begin
          case p.typ of
            ait_regalloc:
              begin
                {A register allocation of a spilled register can be removed.}
                if (Tai_regalloc(p).reg.number shr 8) in regs_to_spill_set then
                  begin
                    q:=p;
                    p:=Tai(p.next);
                    list.remove(q);
                    continue;
                  end
                else
                  if Tai_regalloc(p).allocation then
                    exclude(unusedregsint,Tai_regalloc(p).reg.number shr 8)
                  else
                    include(unusedregsint,Tai_regalloc(p).reg.number shr 8);
              end;
            ait_instruction:
              begin
                if Taicpu_abstract(p).spill_registers(list,@getregisterintinline,
                                                      @ungetregisterintinline,
                                                      regs_to_spill_set,
                                                      unusedregsint,
                                                      spill_temps^) then
                  spill_registers:=true;
                if Taicpu_abstract(p).is_move then
                  add_move_instruction(Taicpu(p));
              end;
          end;
          p:=Tai(p.next);
        end;
      for i:=1 to length(regs_to_spill) do
        begin
          tg.ungettemp(list,spill_temps^[Tsuperregister(regs_to_spill[i])]);
        end;
      dispose(spill_temps);
    end;
{$endif newra}

{****************************************************************************
                                  TReference
****************************************************************************}

    procedure reference_reset(var ref : treference);
      begin
        FillChar(ref,sizeof(treference),0);
        ref.base.enum:=R_INTREGISTER;
        ref.index.enum:=R_INTREGISTER;
      {$ifdef i386}
        ref.segment.enum:=R_INTREGISTER;
      {$endif}
      end;

    procedure reference_reset_old(var ref : treference);
      begin
        FillChar(ref,sizeof(treference),0);
      end;


    procedure reference_reset_base(var ref : treference;base : tregister;offset : longint);
      begin
        reference_reset(ref);
        ref.base:=base;
        ref.offset:=offset;
      end;


    procedure reference_reset_symbol(var ref : treference;sym : tasmsymbol;offset : longint);
      begin
        reference_reset(ref);
        ref.symbol:=sym;
        ref.offset:=offset;
      end;


    procedure reference_release(list: taasmoutput; const ref : treference);
      begin
        rg.ungetreference(list,ref);
      end;


    function references_equal(sref : treference;dref : treference):boolean;
      begin
        references_equal:=CompareByte(sref,dref,sizeof(treference))=0;
      end;

 { on most processors , this routine does nothing, overriden currently  }
 { only by 80x86 processor.                                             }
 function trgobj.makeregsize(reg: tregister; size: tcgsize): tregister;
   begin
     makeregsize := reg;
   end;



{****************************************************************************
                                  TLocation
****************************************************************************}

    procedure location_reset(var l : tlocation;lt:TCGLoc;lsize:TCGSize);
      begin
        FillChar(l,sizeof(tlocation),0);
        l.loc:=lt;
        l.size:=lsize;
        case l.loc of
          LOC_REGISTER,LOC_CREGISTER:
            begin
              l.register.enum:=R_INTREGISTER;
              l.registerhigh.enum:=R_INTREGISTER;
            end;
          LOC_REFERENCE,LOC_CREFERENCE:
            begin
              l.reference.base.enum:=R_INTREGISTER;
              l.reference.index.enum:=R_INTREGISTER;
            {$ifdef i386}
              l.reference.segment.enum:=R_INTREGISTER;
            {$endif}
            end;
        end;
      end;


    procedure location_release(list: taasmoutput; const l : tlocation);
      begin
        case l.loc of
          LOC_REGISTER,LOC_CREGISTER :
            begin
              rg.ungetregisterint(list,l.register);
              if l.size in [OS_64,OS_S64] then
               rg.ungetregisterint(list,l.registerhigh);
            end;
          LOC_CREFERENCE,LOC_REFERENCE :
            rg.ungetreference(list, l.reference);
        end;
      end;


    procedure location_freetemp(list:taasmoutput; const l : tlocation);
      begin
        if (l.loc in [LOC_REFERENCE,LOC_CREFERENCE]) then
         tg.ungetiftemp(list,l.reference);
      end;


    procedure location_copy(var destloc:tlocation; const sourceloc : tlocation);
      begin
        destloc:=sourceloc;
      end;


    procedure location_swap(var destloc,sourceloc : tlocation);
      var
        swapl : tlocation;
      begin
        swapl := destloc;
        destloc := sourceloc;
        sourceloc := swapl;
      end;


initialization
   ;
finalization
  rg.free;
end.

{
  $Log$
  Revision 1.51  2003-06-09 14:54:26  jonas
    * (de)allocation of registers for parameters is now performed properly
      (and checked on the ppc)
    - removed obsolete allocation of all parameter registers at the start
      of a procedure (and deallocation at the end)

  Revision 1.50  2003/06/03 21:11:09  peter
    * cg.a_load_* get a from and to size specifier
    * makeregsize only accepts newregister
    * i386 uses generic tcgnotnode,tcgunaryminus

  Revision 1.49  2003/06/03 13:01:59  daniel
    * Register allocator finished

  Revision 1.48  2003/06/01 21:38:06  peter
    * getregisterfpu size parameter added
    * op_const_reg size parameter added
    * sparc updates

  Revision 1.47  2003/05/31 20:31:11  jonas
    * set inital costs of assigning a variable to a register to 120 for
      non-i386, because the used register must be store to memory at the
      start and loaded again at the end

  Revision 1.46  2003/05/30 18:55:21  jonas
    * fixed several regvar related bugs for non-i386. make cycle with -Or now
      works for ppc

  Revision 1.45  2003/05/30 12:36:13  jonas
    * use as little different registers on the ppc until newra is released,
      since every used register must be saved

  Revision 1.44  2003/05/17 13:30:08  jonas
    * changed tt_persistant to tt_persistent :)
    * tempcreatenode now doesn't accept a boolean anymore for persistent
      temps, but a ttemptype, so you can also create ansistring temps etc

  Revision 1.43  2003/05/16 14:33:31  peter
    * regvar fixes

  Revision 1.42  2003/04/26 20:03:49  daniel
    * Bug fix in simplify

  Revision 1.41  2003/04/25 20:59:35  peter
    * removed funcretn,funcretsym, function result is now in varsym
      and aliases for result and function name are added using absolutesym
    * vs_hidden parameter for funcret passed in parameter
    * vs_hidden fixes
    * writenode changed to printnode and released from extdebug
    * -vp option added to generate a tree.log with the nodetree
    * nicer printnode for statements, callnode

  Revision 1.40  2003/04/25 08:25:26  daniel
    * Ifdefs around a lot of calls to cleartempgen
    * Fixed registers that are allocated but not freed in several nodes
    * Tweak to register allocator to cause less spills
    * 8-bit registers now interfere with esi,edi and ebp
      Compiler can now compile rtl successfully when using new register
      allocator

  Revision 1.39  2003/04/23 20:23:06  peter
    * compile fix for no-newra

  Revision 1.38  2003/04/23 14:42:07  daniel
    * Further register allocator work. Compiler now smaller with new
      allocator than without.
    * Somebody forgot to adjust ppu version number

  Revision 1.37  2003/04/22 23:50:23  peter
    * firstpass uses expectloc
    * checks if there are differences between the expectloc and
      location.loc from secondpass in EXTDEBUG

  Revision 1.36  2003/04/22 10:09:35  daniel
    + Implemented the actual register allocator
    + Scratch registers unavailable when new register allocator used
    + maybe_save/maybe_restore unavailable when new register allocator used

  Revision 1.35  2003/04/21 19:16:49  peter
    * count address regs separate

  Revision 1.34  2003/04/17 16:48:21  daniel
    * Added some code to keep track of move instructions in register
      allocator

  Revision 1.33  2003/04/17 07:50:24  daniel
    * Some work on interference graph construction

  Revision 1.32  2003/03/28 19:16:57  peter
    * generic constructor working for i386
    * remove fixed self register
    * esi added as address register for i386

  Revision 1.31  2003/03/11 21:46:24  jonas
    * lots of new regallocator fixes, both in generic and ppc-specific code
      (ppc compiler still can't compile the linux system unit though)

  Revision 1.30  2003/03/09 21:18:59  olle
    + added cutils to the uses clause

  Revision 1.29  2003/03/08 20:36:41  daniel
    + Added newra version of Ti386shlshrnode
    + Added interference graph construction code

  Revision 1.28  2003/03/08 13:59:16  daniel
    * Work to handle new register notation in ag386nsm
    + Added newra version of Ti386moddivnode

  Revision 1.27  2003/03/08 10:53:48  daniel
    * Created newra version of secondmul in n386add.pas

  Revision 1.26  2003/03/08 08:59:07  daniel
    + $define newra will enable new register allocator
    + getregisterint will return imaginary registers with $newra
    + -sr switch added, will skip register allocation so you can see
      the direct output of the code generator before register allocation

  Revision 1.25  2003/02/26 20:50:45  daniel
    * Fixed ungetreference

  Revision 1.24  2003/02/19 22:39:56  daniel
    * Fixed a few issues

  Revision 1.23  2003/02/19 22:00:14  daniel
    * Code generator converted to new register notation
    - Horribily outdated todo.txt removed

  Revision 1.22  2003/02/02 19:25:54  carl
    * Several bugfixes for m68k target (register alloc., opcode emission)
    + VIS target
    + Generic add more complete (still not verified)

  Revision 1.21  2003/01/08 18:43:57  daniel
   * Tregister changed into a record

  Revision 1.20  2002/10/05 12:43:28  carl
    * fixes for Delphi 6 compilation
     (warning : Some features do not work under Delphi)

  Revision 1.19  2002/08/23 16:14:49  peter
    * tempgen cleanup
    * tt_noreuse temp type added that will be used in genentrycode

  Revision 1.18  2002/08/17 22:09:47  florian
    * result type handling in tcgcal.pass_2 overhauled
    * better tnode.dowrite
    * some ppc stuff fixed

  Revision 1.17  2002/08/17 09:23:42  florian
    * first part of procinfo rewrite

  Revision 1.16  2002/08/06 20:55:23  florian
    * first part of ppc calling conventions fix

  Revision 1.15  2002/08/05 18:27:48  carl
    + more more more documentation
    + first version include/exclude (can't test though, not enough scratch for i386 :()...

  Revision 1.14  2002/08/04 19:06:41  carl
    + added generic exception support (still does not work!)
    + more documentation

  Revision 1.13  2002/07/07 09:52:32  florian
    * powerpc target fixed, very simple units can be compiled
    * some basic stuff for better callparanode handling, far from being finished

  Revision 1.12  2002/07/01 18:46:26  peter
    * internal linker
    * reorganized aasm layer

  Revision 1.11  2002/05/18 13:34:17  peter
    * readded missing revisions

  Revision 1.10  2002/05/16 19:46:44  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.8  2002/04/21 15:23:03  carl
  + makeregsize
  + changeregsize is now a local routine

  Revision 1.7  2002/04/20 21:32:25  carl
  + generic FPC_CHECKPOINTER
  + first parameter offset in stack now portable
  * rename some constants
  + move some cpu stuff to other units
  - remove unused constents
  * fix stacksize for some targets
  * fix generic size problems which depend now on EXTEND_SIZE constant

  Revision 1.6  2002/04/15 19:03:31  carl
  + reg2str -> std_reg2str()

  Revision 1.5  2002/04/06 18:13:01  jonas
    * several powerpc-related additions and fixes

  Revision 1.4  2002/04/04 19:06:04  peter
    * removed unused units
    * use tlocation.size in cg.a_*loc*() routines

  Revision 1.3  2002/04/02 17:11:29  peter
    * tlocation,treference update
    * LOC_CONSTANT added for better constant handling
    * secondadd splitted in multiple routines
    * location_force_reg added for loading a location to a register
      of a specified size
    * secondassignment parses now first the right and then the left node
      (this is compatible with Kylix). This saves a lot of push/pop especially
      with string operations
    * adapted some routines to use the new cg methods

  Revision 1.2  2002/04/01 19:24:25  jonas
    * fixed different parameter name in interface and implementation
      declaration of a method (only 1.0.x detected this)

  Revision 1.1  2002/03/31 20:26:36  jonas
    + a_loadfpu_* and a_loadmm_* methods in tcg
    * register allocation is now handled by a class and is mostly processor
      independent (+rgobj.pas and i386/rgcpu.pas)
    * temp allocation is now handled by a class (+tgobj.pas, -i386\tgcpu.pas)
    * some small improvements and fixes to the optimizer
    * some register allocation fixes
    * some fpuvaroffset fixes in the unary minus node
    * push/popusedregisters is now called rg.save/restoreusedregisters and
      (for i386) uses temps instead of push/pop's when using -Op3 (that code is
      also better optimizable)
    * fixed and optimized register saving/restoring for new/dispose nodes
    * LOC_FPU locations now also require their "register" field to be set to
      R_ST, not R_ST0 (the latter is used for LOC_CFPUREGISTER locations only)
    - list field removed of the tnode class because it's not used currently
      and can cause hard-to-find bugs

}
