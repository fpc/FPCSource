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

The code generator thinks it has an infinite amount of registers. Our processor
has a limited amount of registers. Therefore we must reduce the amount of
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
usefull for describing archtectural constrains, like for example that
the div instruction modifies edx, so variables that are in use at that time
cannot be stored into edx. This can be modelled by making edx interfere
with those variables.

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

    type


       regvar_longintarray = array[firstreg..lastreg] of longint;
       regvarint_longintarray = array[first_supreg..last_supreg] of longint;
       regvar_booleanarray = array[firstreg..lastreg] of boolean;
       regvar_ptreearray = array[firstreg..lastreg] of tnode;

       tpushedsavedloc = record
         case byte of
           0: (pushed: boolean);
           1: (ofs: longint);
       end;

       tpushedsaved = array[firstreg..lastreg] of tpushedsavedloc;
       Tpushedsavedint = array[first_supreg..last_supreg] of Tpushedsavedloc;

      Tinterferencebitmap=array[Tsuperregister] of set of Tsuperregister;
      Tinterferenceadjlist=array[Tsuperregister] of Pstring;
      Tinterferencegraph=record
        bitmap:Tinterferencebitmap;
        adjlist:Tinterferenceadjlist;
      end;
      Pinterferencegraph=^Tinterferencegraph;

      Tmoveset=(ms_coalesced_moves,ms_constrained_moves,ms_frozen_moves,
                ms_worklist_moves,ms_activemoves);
      Tmoveins=class(Tlinkedlistitem)
        moveset:Tmoveset;
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
          lastintreg:Tsuperregister;
          unusedregsint,usableregsint:Tsupregset;
          unusedregsaddr,usableregsaddr:Tsupregset;
          unusedregsfpu,usableregsfpu : tregisterset;
          unusedregsmm,usableregsmm : tregisterset;
          { these counters contain the number of elements in the }
          { unusedregsxxx/usableregsxxx sets                     }
          countunusedregsint,
          countunusedregsaddr,
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
          usedintbyproc,
          usedaddrbyproc,
          usedintinproc,
          usedaddrinproc:Tsupregset;

          reg_pushes : regvar_longintarray;
          reg_pushes_int : regvarint_longintarray;
          is_reg_var : regvar_booleanarray;
          is_reg_var_int:Tsupregset;
          regvar_loaded: regvar_booleanarray;
          regvar_loaded_int: Tsupregset;


          { tries to hold the amount of times which the current tree is processed  }
          t_times: longint;

          constructor create;

          {# Allocate a general purpose register

             An internalerror will be generated if there
             is no more free registers which can be allocated
          }
          function getregisterint(list:Taasmoutput;size:Tcgsize):Tregister;virtual;
          {# Free a general purpose register

             @param(r register to free)

          }
          procedure ungetregisterint(list: taasmoutput; r : tregister); virtual;

          {# Allocate a floating point register

             An internalerror will be generated if there
             is no more free registers which can be allocated
          }
          function getregisterfpu(list: taasmoutput) : tregister; virtual;
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
          function getexplicitregisterfpu(list : taasmoutput; r : Toldregister) : tregister;

          {# Deallocate any kind of register }
          procedure ungetregister(list: taasmoutput; r : tregister); virtual;

          {# Deallocate all registers which are allocated
             in the specified reference. On most systems,
             this will free the base and index registers
             of the specified reference.

             @param(ref reference which must have its registers freed)
          }
          procedure ungetreference(list: taasmoutput; const ref : treference); virtual;

          {# Reset the register allocator information (usable registers etc) }
          procedure cleartempgen;virtual;

          {# Convert a register to a specified register size, and return that register size }
          function makeregsize(reg: tregister; size: tcgsize): tregister; virtual;


          {# saves register variables (restoring happens automatically) }
          procedure saveintregvars(list:Taasmoutput;const s:Tsupregset);
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
          procedure saveusedintregisters(list:Taasmoutput;
                                         var saved:Tpushedsavedint;
                                         const s:Tsupregset);virtual;
          procedure saveusedotherregisters(list:Taasmoutput;
                                           var saved:Tpushedsaved;
                                           const s:Tregisterset);virtual;
          {# Restores the registers which were saved with a call
             to @var(saveusedregisters).

             On processors which have instructions which manipulate the stack,
             this routine should be overriden for performance reasons.
          }
          procedure restoreusedintregisters(list:Taasmoutput;
                                            const saved:Tpushedsavedint);virtual;
          procedure restoreusedotherregisters(list:Taasmoutput;
                                              const saved:Tpushedsaved);virtual;

          { used when deciding which registers to use for regvars }
          procedure incrementintregisterpushed(const s:Tsupregset);
          procedure incrementotherregisterpushed(const s: tregisterset);
          procedure clearregistercount;
          procedure resetusableregisters;virtual;

          procedure makeregvarint(reg:Tnewregister);
          procedure makeregvarother(reg:Tregister);

          procedure saveStateForInline(var state: pointer);virtual;
          procedure restoreStateAfterInline(var state: pointer);virtual;

          procedure saveUnusedState(var state: pointer);virtual;
          procedure restoreUnusedState(var state: pointer);virtual;
{$ifdef newra}
          procedure writegraph;
{$endif}
       protected
{$ifdef newra}
          igraph:Tinterferencegraph;
          movelist:array[Tsuperregister] of Tlinkedlist;
          worklistmoves:Tlinkedlist;
{$endif}
          { the following two contain the common (generic) code for all }
          { get- and ungetregisterxxx functions/procedures              }
          function getregistergen(list: taasmoutput; const lowreg, highreg: Toldregister;
              var unusedregs:Tregisterset; var countunusedregs: byte): tregister;
          function getregistergenint(list:Taasmoutput;subreg:Tsubregister;
                                     const lowreg,highreg:Tsuperregister;
                                     var fusedinproc,fusedbyproc,unusedregs:Tsupregset;
                                     var countunusedregs:byte):Tregister;
          procedure ungetregistergen(list: taasmoutput; const r: tregister;
              const usableregs: tregisterset; var unusedregs: tregisterset; var countunusedregs: byte);
          procedure ungetregistergenint(list:taasmoutput;const r:Tregister;
                                        const usableregs:Tsupregset;
                                        var unusedregs:Tsupregset;
                                        var countunusedregs:byte);
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
     procedure location_reset(var l : tlocation;lt:TLoc;lsize:TCGSize);
     procedure location_release(list: taasmoutput; const l : tlocation);
     procedure location_freetemp(list: taasmoutput; const l : tlocation);
     procedure location_copy(var destloc,sourceloc : tlocation);
     procedure location_swap(var destloc,sourceloc : tlocation);

    type
      psavedstate = ^tsavedstate;
      tsavedstate = record
        unusedregsint,usableregsint : Tsupregset;
        unusedregsaddr,usableregsaddr : Tsupregset;
        unusedregsfpu,usableregsfpu : tregisterset;
        unusedregsmm,usableregsmm : tregisterset;
        countunusedregsint,
        countunusedregsaddr,
        countunusedregsfpu,
        countunusedregsmm : byte;
        countusableregsint,
        countusableregsfpu,
        countusableregsmm : byte;
        { contains the registers which are really used by the proc itself }
        usedbyproc,
        usedinproc : tregisterset;
        reg_pushes : regvar_longintarray;
        is_reg_var : regvar_booleanarray;
        is_reg_var_int : Tsupregset;
        regvar_loaded: regvar_booleanarray;
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
        countunusedregsint,
        countunusedregsaddr,
        countunusedregsfpu,
        countunusedregsmm : byte;
      end;

  implementation

    uses
       systems,
       globals,verbose,
       cgobj,tgobj,regvars;

    constructor trgobj.create;

     begin
       usedinproc := [];
       usedbyproc:=[];
       t_times := 0;
       resetusableregisters;
       lastintreg:=0;
{$ifdef TEMPREGDEBUG}
       fillchar(reg_user,sizeof(reg_user),0);
       fillchar(reg_releaser,sizeof(reg_releaser),0);
{$endif TEMPREGDEBUG}
{$ifdef newra}
       fillchar(igraph,sizeof(igraph),0);
       fillchar(movelist,sizeof(movelist),0);
       worklistmoves.create;
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
                                      var fusedinproc,fusedbyproc,unusedregs:Tsupregset;
                                      var countunusedregs:byte):Tregister;

    var i:Tsuperregister;
        r:Tregister;

    begin
      if not (lastintreg in [lowreg..highreg]) then
        lastintreg:=lowreg;
      i:=lastintreg;
      repeat
        if i=highreg then
          i:=lowreg
        else
          inc(i);
        if i in unusedregs then
          begin
            exclude(unusedregs,i);
            include(fusedinproc,i);
            include(fusedbyproc,i);
            dec(countunusedregs);
            r.enum:=R_INTREGISTER;
            r.number:=i shl 8 or subreg;
            list.concat(Tai_regalloc.alloc(r));
            result:=r;
            lastintreg:=i;
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
                                         var unusedregs:Tsupregset;
                                         var countunusedregs:byte);

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
             testregisters32;
             exit;
           end
{$else EXTTEMPREGDEBUG}
           exit
{$endif EXTTEMPREGDEBUG}
         else
{$endif TEMPREGDEBUG}
          inc(countunusedregs);
        include(unusedregs,supreg);
        list.concat(tai_regalloc.dealloc(r));
{$ifdef newra}
        add_edges_used(supreg);
{$endif newra}
      end;


    function trgobj.getregisterint(list:taasmoutput;size:Tcgsize):Tregister;

    var subreg:Tsubregister;

    begin
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
      subreg:=cgsize2subreg(size);
      result:=getregistergenint(list,
                                subreg,
{$ifdef newra}
                                first_imreg,
                                last_imreg,
{$else}
                                first_supreg,
                                last_supreg,
{$endif}
                                usedintbyproc,
                                usedintinproc,
                                unusedregsint,
                                countunusedregsint);
{$ifdef TEMPREGDEBUG}
      reg_user[result]:=curptree^;
      testregisters32;
{$endif TEMPREGDEBUG}
    end;


    procedure trgobj.ungetregisterint(list : taasmoutput; r : tregister);

      begin
         ungetregistergenint(list,r,usableregsint,unusedregsint,
           countunusedregsint);
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
          dec(countunusedregsint);
{$ifdef TEMPREGDEBUG}
          if curptree^^.usableregs-countunusedregsint>curptree^^.registers32 then
            internalerror(10);
          reg_user[r shr 8]:=curptree^;
{$endif TEMPREGDEBUG}
          exclude(unusedregsint,r shr 8);
          include(usedintinproc,r shr 8);
          include(usedintbyproc,r shr 8);
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
           getexplicitregisterfpu:=getregisterfpu(list);
      end;

    function trgobj.getregisterfpu(list: taasmoutput) : tregister;

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
      countunusedregsint:=countusableregsint;
      countunusedregsfpu:=countusableregsfpu;
      countunusedregsmm:=countusableregsmm;
   {$ifdef newra}
      unusedregsint:=[0..255];
   {$else}
      unusedregsint:=usableregsint;
   {$endif}
      unusedregsfpu:=usableregsfpu;
      unusedregsmm:=usableregsmm;
   {$ifdef newra}
      for i:=low(Tsuperregister) to high(Tsuperregister) do
        begin
          if igraph.adjlist[i]<>nil then
            dispose(igraph.adjlist[i]);
          if movelist[i]<>nil then
            movelist[i].destroy;
        end;
      fillchar(igraph,sizeof(igraph),0);
      worklistmoves.destroy;
   {$endif}
    end;


    procedure trgobj.ungetreference(list : taasmoutput; const ref : treference);

      begin
         if ref.base.number<>NR_NO then
           ungetregisterint(list,ref.base);
       if ref.index.number<>NR_NO then
           ungetregisterint(list,ref.index);
      end;


    procedure trgobj.saveintregvars(list:Taasmoutput;const s:Tsupregset);

    var r:Tsuperregister;

    begin
      if not(cs_regalloc in aktglobalswitches) then
        exit;
      for r:=firstsaveintreg to lastsaveintreg do
        if (r in is_reg_var_int) and
           (r in s) then
          store_regvar_int(list,r);
    end;

    procedure trgobj.saveotherregvars(list: taasmoutput; const s: tregisterset);
      var
        r: Tregister;
      begin
        if not(cs_regalloc in aktglobalswitches) then
          exit;
        if firstsavefpureg <> R_NO then
          for r.enum := firstsavefpureg to lastsavefpureg do
            if is_reg_var[r.enum] and
               (r.enum in s) then
              store_regvar(list,r);
        if firstsavemmreg <> R_NO then
          for r.enum := firstsavemmreg to lastsavemmreg do
            if is_reg_var[r.enum] and
               (r.enum in s) then
              store_regvar(list,r);
      end;


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
              tg.GetTemp(list,sizeof(aword),tt_persistant,hr);
              saved[r].ofs:=hr.offset;
              r2.enum:=R_INTREGISTER;
              r2.number:=r shl 8 or R_SUBWHOLE;
              cg.a_load_reg_ref(list,OS_INT,r2,hr);
              cg.a_reg_dealloc(list,r2);
              include(unusedregsint,r);
              inc(countunusedregsint);
            end;
        end;
{$ifdef TEMPREGDEBUG}
      testregisters32;
{$endif TEMPREGDEBUG}
    end;

    procedure trgobj.saveusedotherregisters(list: taasmoutput;
        var saved : tpushedsaved; const s: tregisterset);

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
              if not is_reg_var[r.enum] and
                 (r.enum in s) and
                 { and is present in use }
                 not(r.enum in unusedregsfpu) then
                begin
                  { then save it }
                  tg.GetTemp(list,extended_size,tt_persistant,hr);
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
              if not is_reg_var[r.enum] and
                 (r.enum in s) and
                 { and is present in use }
                 not(r.enum in unusedregsmm) then
                begin
                  { then save it }
                  tg.GetTemp(list,mmreg_size,tt_persistant,hr);
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
                cg.a_load_ref_reg(list,OS_INT,hr,r2);
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

    procedure trgobj.restoreusedotherregisters(list : taasmoutput;
        const saved : tpushedsaved);

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


    procedure trgobj.incrementintregisterpushed(const s:Tsupregset);

    var regi:Tsuperregister;

    begin
      for regi:=firstsaveintreg to lastsaveintreg do
        begin
          if (regi in s) then
            inc(reg_pushes_int[regi],t_times*2);
        end;
    end;

    procedure trgobj.incrementotherregisterpushed(const s:Tregisterset);

      var
         regi : Toldregister;

      begin
         if firstsavefpureg <> R_NO then
           for regi:=firstsavefpureg to lastsavefpureg do
             begin
                if (regi in s) then
                  inc(reg_pushes[regi],t_times*2);
             end;
         if firstsavemmreg <> R_NO then
           for regi:=firstsavemmreg to lastsavemmreg do
             begin
                if (regi in s) then
                  inc(reg_pushes[regi],t_times*2);
             end;
      end;


    procedure trgobj.clearregistercount;

      begin
        fillchar(reg_pushes,sizeof(reg_pushes),0);
        fillchar(is_reg_var,sizeof(is_reg_var),false);
        is_reg_var_int:=[];
        fillchar(regvar_loaded,sizeof(regvar_loaded),false);
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


    procedure trgobj.makeregvarint(reg:Tnewregister);

    var supreg:Tsuperregister;

    begin
      supreg:=reg shr 8;
      dec(countusableregsint);
      dec(countunusedregsint);
      exclude(usableregsint,reg);
      exclude(unusedregsint,reg);
      include(is_reg_var_int,supreg);
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
          end
        else if reg.enum in mmregs then
          begin
             dec(countusableregsmm);
             dec(countunusedregsmm);
             exclude(usableregsmm,reg.enum);
             exclude(unusedregsmm,reg.enum);
          end;
        is_reg_var[reg.enum]:=true;
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
        psavedstate(state)^.countunusedregsint := countunusedregsint;
        psavedstate(state)^.countunusedregsfpu := countunusedregsfpu;
        psavedstate(state)^.countunusedregsmm := countunusedregsmm;
        psavedstate(state)^.countusableregsint := countusableregsint;
        psavedstate(state)^.countusableregsfpu := countusableregsfpu;
        psavedstate(state)^.countusableregsmm := countusableregsmm;
        psavedstate(state)^.usedinproc := usedinproc;
        psavedstate(state)^.usedbyproc := usedbyproc;
        psavedstate(state)^.reg_pushes := reg_pushes;
        psavedstate(state)^.is_reg_var := is_reg_var;
        psavedstate(state)^.is_reg_var_int := is_reg_var_int;
        psavedstate(state)^.regvar_loaded := regvar_loaded;
        psavedstate(state)^.regvar_loaded_int := regvar_loaded_int;
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
        countunusedregsint := psavedstate(state)^.countunusedregsint;
        countunusedregsfpu := psavedstate(state)^.countunusedregsfpu;
        countunusedregsmm := psavedstate(state)^.countunusedregsmm;
        countusableregsint := psavedstate(state)^.countusableregsint;
        countusableregsfpu := psavedstate(state)^.countusableregsfpu;
        countusableregsmm := psavedstate(state)^.countusableregsmm;
        usedinproc := psavedstate(state)^.usedinproc;
        usedbyproc := psavedstate(state)^.usedbyproc;
        reg_pushes := psavedstate(state)^.reg_pushes;
        is_reg_var := psavedstate(state)^.is_reg_var;
        is_reg_var_int := psavedstate(state)^.is_reg_var_int;
        regvar_loaded := psavedstate(state)^.regvar_loaded;
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
        punusedstate(state)^.countunusedregsint := countunusedregsint;
        punusedstate(state)^.countunusedregsfpu := countunusedregsfpu;
        punusedstate(state)^.countunusedregsmm := countunusedregsmm;
      end;


    procedure trgobj.restoreUnusedState(var state: pointer);
      begin
        unusedregsint := punusedstate(state)^.unusedregsint;
        unusedregsfpu := punusedstate(state)^.unusedregsfpu;
        unusedregsmm := punusedstate(state)^.unusedregsmm;
        countunusedregsint := punusedstate(state)^.countunusedregsint;
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
            addadj(u,v);
          if not(v in [first_supreg..last_supreg]) then
            addadj(v,u);
        end;
    end;

    procedure Trgobj.add_edges_used(u:Tsuperregister);

    var i:Tsuperregister;

    begin
      for i:=1 to 255 do
        if not(i in unusedregsint) then
          add_edge(u,i);
    end;

    procedure Trgobj.writegraph;

    var f:text;
        i,j:Tsuperregister;

    begin
      assign(f,'igraph');
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

    procedure location_reset(var l : tlocation;lt:TLoc;lsize:TCGSize);
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


    procedure location_copy(var destloc,sourceloc : tlocation);
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
  Revision 1.35  2003-04-21 19:16:49  peter
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
