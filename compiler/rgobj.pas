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

{ Allow duplicate allocations, can be used to get the .s file written }
{ $define ALLOWDUPREG}

{#******************************************************************************

@abstract(Abstract register allocator unit)

Register allocator introduction.

Free Pascal uses a Chaitin style register allocator. We use a variant similair
to the one described in the book "Modern compiler implementation in C" by
Andrew W. Appel., published by Cambridge University Press.

The register allocator that is described by Appel uses a much improved way
of register coalescing, called "iterated register coalescing". Instead
of doing coalescing as a prepass to the register allocation, the coalescing
is done inside the register allocator. This has the advantage that the
register allocator can coalesce very aggresively without introducing spills.

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
usefull for describing archtectural constraints, like for example that
the div instruction modifies edx, so variables that are in use at that time
cannot be stored into edx. This can be modelled by making edx interfere
with those variables.

Graph colouring is an NP complete problem. Therefore we use an approximation
that pushes registers to colour on to a stack. This is done in the "simplify"
procedure.

The register allocator first checks which registers are a candidate for
coalescing.

*******************************************************************************}


unit rgobj;

  interface

    uses
      cutils, cpubase,
      aasmbase,aasmtai,aasmcpu,
      cclasses,globtype,cgbase,node,
{$ifdef delphi}
      dmisc,
{$endif}
      cpuinfo
      ;


    const
      ALL_OTHERREGISTERS=[low(tregisterindex)..high(tregisterindex)];

    type
       regvarother_longintarray = array[tregisterindex] of longint;
       regvarother_booleanarray = array[tregisterindex] of boolean;
       regvarint_longintarray = array[first_int_supreg..last_int_supreg] of longint;
       regvarint_ptreearray = array[first_int_supreg..last_int_supreg] of tnode;

       tpushedsavedloc = record
         case byte of
           0: (pushed: boolean);
           1: (ofs: longint);
       end;

      tpushedsavedother = array[tregisterindex] of tpushedsavedloc;

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


      {#------------------------------------------------------------------

      This class implements the abstract register allocator. It is used by the
      code generator to allocate and free registers which might be valid across
      nodes. It also contains utility routines related to registers.

      Some of the methods in this class should be overriden
      by cpu-specific implementations.

      --------------------------------------------------------------------}
      trgobj=class
        preserved_by_proc,used_in_proc:Tsuperregisterset;
        is_reg_var:Tsuperregisterset; {old regvars}
        reg_var_loaded:Tsuperregisterset; {old regvars}
        {# The register allocator assigns each register a colour.}
        colour:array[Tsuperregister] of Tsuperregister;
        spillednodes:string;

        constructor create(Acpu_registers:byte;
                           Aregtype:Tregistertype;
                           Adefaultsub:Tsubregister;
                           const Ausable:string;
                           Afirst_imaginary:Tsuperregister;
                           Apreserved_by_proc:Tsuperregisterset);
        destructor destroy;override;
        {# Allocate a register. An internalerror will be generated if there is
         no more free registers which can be allocated.}
        function getregister(list:Taasmoutput;subreg:Tsubregister):Tregister;
        procedure add_constraints(reg:Tregister);virtual;
        {# Allocate an ABT register. An internalerror will be generated if there
         are no more free registers that can be allocated. An explanantion of
         abt registers can be found near the implementation.}
        function getabtregister(list:Taasmoutput;subreg:Tsubregister):Tregister;
        {# Get the register specified.}
        procedure getexplicitregister(list:Taasmoutput;r:Tregister);
        {# Get multiple registers specified.}
        procedure allocexplicitregisters(list:Taasmoutput;r:Tsuperregisterset);
        {# Free multiple registers specified.}
        procedure deallocexplicitregisters(list:Taasmoutput;r:Tsuperregisterset);
        {# Deallocate any kind of register }
        procedure ungetregister(list:Taasmoutput;r:Tregister);virtual;

        {# Do the register allocation.}
        procedure do_register_allocation(list:Taasmoutput;headertai:tai);

{        procedure resetusableregisters;virtual;}

{        procedure makeregvar(reg:Tsuperregister);}

{$ifdef EXTDEBUG}
        procedure writegraph(loopidx:longint);
{$endif EXTDEBUG}
        procedure add_move_instruction(instr:Taicpu);
        {# Prepare the register colouring.}
        procedure prepare_colouring;
        {# Clean up after register colouring.}
        procedure epilogue_colouring;
        {# Colour the registers; that is do the register allocation.}
        procedure colour_registers;
        {# Spills certain registers in the specified assembler list.}
        function  spill_registers(list:Taasmoutput;headertai:tai;const regs_to_spill:string):boolean;
        {# Adds an interference edge.}
        procedure add_edge(u,v:Tsuperregister);
      protected
        regtype:Tregistertype;
        { default subregister used }
        defaultsub:tsubregister;
        {# First imaginary register.}
        first_imaginary,
        {# Last register allocated.}
        lastused,
        {# Highest register allocated until now.}
        maxreg:Tsuperregister;
        usable_registers:string[32];
        unusedregs,usableregs:Tsuperregisterset;
        countusableregs:byte; {old regvars}
        cpu_registers:byte;
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
        procedure getregisterinline(list:Taasmoutput;position:Tai;subreg:Tsubregister;var result:Tregister);
        procedure ungetregisterinline(list:Taasmoutput;position:Tai;r:Tregister);
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
      end;

    const {# This value is used in tsaved. If the array value is equal
             to this, then this means that this register is not used.}
            reg_not_saved = $7fffffff;
            first_reg = 0;
            last_reg = 254;
            maxspillingcounter = 20;




implementation

    uses
       systems,
       globals,verbose,tgobj,procinfo;

{******************************************************************************
                                trgobj
******************************************************************************}

    constructor trgobj.create(Acpu_registers:byte;
                                          Aregtype:Tregistertype;
                                          Adefaultsub:Tsubregister;
                                          const Ausable:string;
                                          Afirst_imaginary:Tsuperregister;
                                          Apreserved_by_proc:Tsuperregisterset);

    var i:Tsuperregister;

    begin
      used_in_proc:=[];
{      resetusableregisters;}
      lastused:=0;
      maxreg:=Afirst_imaginary;
      first_imaginary:=Afirst_imaginary;
      cpu_registers:=Acpu_registers;
      regtype:=Aregtype;
      defaultsub:=Adefaultsub;
      unusedregs:=[first_reg..last_reg]; { 255 (RS_INVALID) can't be used }
    {$ifdef powerpc}
      preserved_by_proc:=[RS_R13..RS_R31];
    {$else powerpc}
      preserved_by_proc:=[];
    {$endif powerpc}
      fillchar(igraph,sizeof(igraph),0);
      fillchar(degree,sizeof(degree),0);
      {Precoloured nodes should have an infinite degree, which we can approach
       by 255.}
      for i:=0 to first_imaginary-1 do
        degree[i]:=255;
      fillchar(movelist,sizeof(movelist),0);
      worklist_moves:=Tlinkedlist.create;
      usable_registers:=Ausable;
      abtlist:='';
      fillchar(colour,sizeof(colour),RS_INVALID);
   end;

    destructor trgobj.destroy;

    var i:Tsuperregister;

    begin
      for i:=low(Tsuperregister) to high(Tsuperregister) do
        begin
          if igraph.adjlist[i]<>nil then
            dispose(igraph.adjlist[i]);
          if movelist[i]<>nil then
            dispose(movelist[i]);
        end;
      worklist_moves.free;
    end;


    function trgobj.getregister(list:Taasmoutput;subreg:Tsubregister):Tregister;
    var i,p:Tsuperregister;
        r:Tregister;
        min : byte;
        adj : pstring;
    begin
      if maxreg<last_reg then
        begin
          inc(maxreg);
          p:=maxreg;
          min:=0;
        end
      else
        begin
          min:=$ff;
          p:=first_imaginary;
          for i:=first_imaginary to maxreg do
           if (i in unusedregs) and
              (pos(char(i),abtlist)=0) then
            begin
              adj:=igraph.adjlist[Tsuperregister(i)];
              if adj=nil then
                begin
                  p:=i;
                  min:=0;
                  break;  {We won't find smaller ones.}
                end
              else
                if length(adj^)<min then
                  begin
                    p:=i;
                    min:=length(adj^);
                    if min=0 then
                      break;  {We won't find smaller ones.}
                  end;
            end;

           if min=$ff then
             begin
{$ifdef ALLOWDUPREG}
               result:=newreg(regtype,RS_INVALID,subreg);
               exit;
{$else}
               internalerror(10);
{$endif}
             end;
        end;

       exclude(unusedregs,p);
       include(used_in_proc,p);
       r:=newreg(regtype,p,subreg);
       list.concat(Tai_regalloc.alloc(r));
       add_edges_used(p);
       add_constraints(r);
       result:=r;
    end;


    procedure trgobj.ungetregister(list:Taasmoutput;r:Tregister);

    var supreg:Tsuperregister;

    begin
      supreg:=getsupreg(r);
      if not (supreg in unusedregs) then
        begin
          include(unusedregs,supreg);
          list.concat(Tai_regalloc.dealloc(r));
          add_edges_used(supreg);
          add_constraints(r);
        end;
    end;


    procedure trgobj.getexplicitregister(list:Taasmoutput;r:Tregister);

    var supreg:Tsuperregister;

    begin
      supreg:=getsupreg(r);
      if supreg in unusedregs then
        begin
          exclude(unusedregs,supreg);
          include(used_in_proc,supreg);
          list.concat(Tai_regalloc.alloc(r));
          add_edges_used(supreg);
          add_constraints(r);
         end
       else
{$ifndef ALLOWDUPREG}
         internalerror(200301103)
{$endif ALLOWDUPREG}
         ;
    end;


    procedure trgobj.allocexplicitregisters(list:Taasmoutput;r:Tsuperregisterset);

    var reg:Tregister;
        i:Tsuperregister;

    begin
      if unusedregs*r=r then
        begin
          unusedregs:=unusedregs-r;
          used_in_proc:=used_in_proc+r;
          for i:=0 to first_imaginary-1 do
            if i in r then
              begin
                add_edges_used(i);
                reg:=newreg(regtype,i,R_SUBWHOLE);
                list.concat(Tai_regalloc.alloc(reg));
              end;
         end
       else
{$ifndef ALLOWDUPREG}
         internalerror(200305061)
{$endif ALLOWDUPREG}
         ;
    end;

    procedure trgobj.deallocexplicitregisters(list:Taasmoutput;r:Tsuperregisterset);

    var reg:Tregister;
        i:Tsuperregister;

    begin
      if unusedregs*r=[] then
        begin
          unusedregs:=unusedregs+r;
          for i:=first_imaginary-1 downto 0 do
            if i in r then
              begin
                reg:=newreg(regtype,i,R_SUBWHOLE);
                list.concat(Tai_regalloc.dealloc(reg));
              end;
         end
       else
         internalerror(200305061);
    end;


    procedure trgobj.do_register_allocation(list:Taasmoutput;headertai:tai);

    var spillingcounter:byte;
        fastspill:boolean;

    begin
      {Do register allocation.}
      spillingcounter:=0;
      repeat
        prepare_colouring;
        colour_registers;
        epilogue_colouring;
        fastspill:=true;
        if spillednodes<>'' then
          begin
            inc(spillingcounter);
            if spillingcounter>maxspillingcounter then
              internalerror(200309041);
            fastspill:=spill_registers(list,headertai,spillednodes);
          end;
      until (spillednodes='') or not fastspill;
    end;


    procedure trgobj.add_constraints(reg:Tregister);

    begin
    end;


    procedure trgobj.add_edge(u,v:Tsuperregister);

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
          if not(u in [first_int_supreg..last_int_supreg]) then
            begin
              addadj(u,v);
              inc(degree[u]);
            end;
          if not(v in [first_int_supreg..last_int_supreg]) then
            begin
              addadj(v,u);
              inc(degree[v]);
            end;
        end;
    end;


    procedure trgobj.add_edges_used(u:Tsuperregister);

    var i:Tsuperregister;

    begin
      for i:=0 to maxreg do
        if not(i in unusedregs) then
          add_edge(u,i);
    end;

{$ifdef EXTDEBUG}
    procedure trgobj.writegraph(loopidx:longint);

    {This procedure writes out the current interference graph in the
    register allocator.}


    var f:text;
        i,j:Tsuperregister;

    begin
      assign(f,'igraph'+tostr(loopidx));
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
{$endif EXTDEBUG}

    procedure trgobj.add_to_movelist(u:Tsuperregister;data:Tlinkedlistitem);

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

    procedure trgobj.add_move_instruction(instr:Taicpu);

    {This procedure notifies a certain as a move instruction so the
     register allocator can try to eliminate it.}

    var i:Tmoveins;
        ssupreg,dsupreg:Tsuperregister;

    begin
      i:=Tmoveins.create;
      i.moveset:=ms_worklist_moves;
      i.instruction:=instr;
      worklist_moves.insert(i);
      ssupreg:=getsupreg(instr.oper[O_MOV_SOURCE].reg);
      add_to_movelist(ssupreg,i);
      dsupreg:=getsupreg(instr.oper[O_MOV_DEST].reg);
      if ssupreg<>dsupreg then
        {Avoid adding the same move instruction twice to a single register.}
        add_to_movelist(dsupreg,i);
      i.x:=ssupreg;
      i.y:=dsupreg;
    end;

    function trgobj.move_related(n:Tsuperregister):boolean;

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

    procedure trgobj.make_work_list;

    var n:Tsuperregister;

    begin
      {If we have 7 cpu registers, and the degree of a node is 7, we cannot
       assign it to any of the registers, thus it is significant.}
      for n:=first_imaginary to maxreg do
        if degree[n]>=cpu_registers then
          spillworklist:=spillworklist+char(n)
        else if move_related(n) then
          freezeworklist:=freezeworklist+char(n)
        else
          simplifyworklist:=simplifyworklist+char(n);
    end;

    procedure trgobj.prepare_colouring;

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

    procedure trgobj.enable_moves(n:Tsuperregister);

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

    procedure trgobj.decrement_degree(m:Tsuperregister);

    var adj:Pstring;
        d:byte;
        i,p:byte;
        n:char;

    begin
      d:=degree[m];
      if degree[m]>0 then
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

    procedure trgobj.simplify;

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
      if length(simplifyworklist)>1 then
        simplifyworklist[p]:=simplifyworklist[length(simplifyworklist)];
      dec(simplifyworklist[0]);

      {Push it on the selectstack.}
      selectstack:=selectstack+char(n);
      adj:=igraph.adjlist[n];
      if adj<>nil then
        for i:=1 to length(adj^) do
          begin
            m:=adj^[i];
            if ((pos(m,selectstack) or pos(m,coalescednodes))=0) and
                not (Tsuperregister(m) in [first_int_supreg..last_int_supreg]) then
              decrement_degree(Tsuperregister(m));
          end;
    end;

    function trgobj.get_alias(n:Tsuperregister):Tsuperregister;

    begin
      while pos(char(n),coalescednodes)<>0 do
        n:=alias[n];
      get_alias:=n;
    end;

    procedure trgobj.add_worklist(u:Tsuperregister);

    var p:byte;

    begin
      if not(u in [first_int_supreg..last_int_supreg]) and
         not move_related(u) and
         (degree[u]<cpu_registers) then
        begin
          p:=pos(char(u),freezeworklist);
          if p=0 then
            internalerror(200308161); {must be found}
          if length(freezeworklist)>1 then
            freezeworklist[p]:=freezeworklist[length(freezeworklist)];
          dec(freezeworklist[0]);
          simplifyworklist:=simplifyworklist+char(u);
        end;
    end;

    function trgobj.adjacent_ok(u,v:Tsuperregister):boolean;

    {Check wether u and v should be coalesced. u is precoloured.}

      function ok(t,r:Tsuperregister):boolean;

      begin
        ok:=(degree[t]<cpu_registers) or
            (t in [first_int_supreg..last_int_supreg]) or
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

    function trgobj.conservative(u,v:Tsuperregister):boolean;

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

    procedure trgobj.combine(u,v:Tsuperregister);

    var add:boolean;
        adj:Pstring;
        i,p:byte;
        n,o:cardinal;
        t:char;
        decrement:boolean;

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
      if assigned(movelist[v]) then
        begin
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
        end;

      adj:=igraph.adjlist[v];
      if adj<>nil then
        for i:=1 to length(adj^) do
          begin
            t:=adj^[i];
            if (pos(t,selectstack) or pos(t,coalescednodes))=0 then
              begin
                decrement:=(Tsuperregister(t)<>u) and not(u in igraph.bitmap[Tsuperregister(t)]);
                add_edge(Tsuperregister(t),u);
                {Do not call decrement_degree because it might move nodes between
                 lists while the degree does not change (add_edge will increase it).
                 Instead, we will decrement manually. (Only if the degree has been
                 increased.)}
                if decrement and not (Tsuperregister(t) in [first_int_supreg..last_int_supreg])
                   and (degree[Tsuperregister(t)]>0) then
                  dec(degree[Tsuperregister(t)]);
              end;
          end;
      p:=pos(char(u),freezeworklist);
      if (degree[u]>=cpu_registers) and (p<>0) then
        begin
          delete(freezeworklist,p,1);
          spillworklist:=spillworklist+char(u);
        end;
    end;

    procedure trgobj.coalesce;

    var m:Tmoveins;
        x,y,u,v:Tsuperregister;

    begin
      m:=Tmoveins(worklist_moves.getfirst);
      x:=get_alias(getsupreg(m.instruction.oper[0].reg));
      y:=get_alias(getsupreg(m.instruction.oper[1].reg));
      if y in [first_int_supreg..last_int_supreg] then
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
      else if (v in [first_int_supreg..last_int_supreg]) or (u in igraph.bitmap[v]) then
        begin
          m.moveset:=ms_constrained_moves;  {Cannot coalesce yet...}
          constrained_moves.insert(m);
          add_worklist(u);
          add_worklist(v);
        end
      {Next test: is it possible and a good idea to coalesce??}
      else if ((u in [first_int_supreg..last_int_supreg]) and adjacent_ok(u,v)) or
              (not(u in [first_int_supreg..last_int_supreg]) and conservative(u,v)) then
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

    procedure trgobj.freeze_moves(u:Tsuperregister);

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
                x:=getsupreg(Tmoveins(m).instruction.oper[0].reg);
                y:=getsupreg(Tmoveins(m).instruction.oper[1].reg);
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

                if not(v in [first_int_supreg..last_int_supreg]) and
                   not(move_related(v)) and
                   (degree[v]<cpu_registers) then
                  begin
                    delete(freezeworklist,pos(char(v),freezeworklist),1);
                    simplifyworklist:=simplifyworklist+char(v);
                  end;
              end;
          end;
    end;

    procedure trgobj.freeze;

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

    procedure trgobj.select_spill;

    var n:char;

    begin
      {This code is WAY too naive. We need not to select just a register, but
       the register that is used the least...}
      n:=spillworklist[byte(spillworklist[0])];
      dec(spillworklist[0]);
      simplifyworklist:=simplifyworklist+n;
      freeze_moves(Tsuperregister(n));
    end;

    procedure trgobj.assign_colours;

    {Assign_colours assigns the actual colours to the registers.}

    var adj:Pstring;
        i,j,k:byte;
        n,a,c:Tsuperregister;
        adj_colours,colourednodes:set of Tsuperregister;
        w:char;

    begin
      spillednodes:='';
      {Reset colours}
      for i:=0 to maxreg do
        colour[i]:=i;
      {Colour the cpu registers...}
      colourednodes:=[first_int_supreg..last_int_supreg];
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
          include(adj_colours,RS_STACK_POINTER_REG);
          {Assume a spill by default...}
          spillednodes:=spillednodes+char(n);
          {Search for a colour not in this list.}
          for k:=1 to length(usable_registers) do
            begin
              c:=Tsuperregister(usable_registers[k]);
              if not(c in adj_colours) then
                begin
                  colour[n]:=c;
                  dec(spillednodes[0]);  {Colour found: no spill.}
                  include(colourednodes,n);
                  if n in used_in_proc then
                    include(used_in_proc,c);
                  break;
                end;
            end;
        end;
      {Finally colour the nodes that were coalesced.}
      for i:=1 to length(coalescednodes) do
        begin
          n:=Tsuperregister(coalescednodes[i]);
          k:=get_alias(n);
          colour[n]:=colour[k];
          if n in used_in_proc then
            include(used_in_proc,colour[k]);
        end;
{$ifdef ra_debug}
      if aktfilepos.line=2502 then
        begin
          writeln('colourlist ',length(freezeworklist));
          for i:=0 to maxintreg do
            writeln(i:4,'   ',colour[i]:4)
        end;
{$endif ra_debug}
    end;

    procedure trgobj.colour_registers;

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
      until (length(simplifyworklist)=0) and
            worklist_moves.empty and
            (length(freezeworklist)=0) and
            (length(spillworklist)=0);
      assign_colours;
    end;

    procedure trgobj.epilogue_colouring;

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
{$endif Principle_wrong_by_definition}
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


    procedure trgobj.clear_interferences(u:Tsuperregister);

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
{$endif Principle_wrong_by_definition}
    end;

    procedure trgobj.getregisterinline(list:Taasmoutput;
                  position:Tai;subreg:Tsubregister;var result:Tregister);
    var min,p,i:Tsuperregister;
        r:Tregister;
        adj:Pstring;
    begin
      if maxreg<last_reg then
        begin
          inc(maxreg);
          p:=maxreg;
          min:=0;
        end
      else
        begin
          min:=$ff;
          p:=first_imaginary;
          for i:=first_imaginary to maxreg do
           if (i in unusedregs) and
              (pos(char(i),abtlist)=0) and
              (pos(char(i),spillednodes)=0) then
            begin
              adj:=igraph.adjlist[Tsuperregister(i)];
              if adj=nil then
                begin
                  p:=i;
                  min:=0;
                  break;  {We won't find smaller ones.}
                end
              else
                if length(adj^)<min then
                  begin
                    p:=i;
                    min:=length(adj^);
                    if min=0 then
                      break;  {We won't find smaller ones.}
                  end;
            end;

           if min=$ff then
             begin
{$ifdef ALLOWDUPREG}
               result:=newreg(regtype,RS_INVALID,subreg);
               exit;
{$else}
               internalerror(10);
{$endif}
             end;
        end;

{$ifdef ra_debug}
       writeln('Spilling temp: ',p,' min ',min);
{$endif ra_debug}

       exclude(unusedregs,p);
       include(used_in_proc,p);
       r:=newreg(regtype,p,subreg);
       if position=nil then
         list.insert(Tai_regalloc.alloc(r))
       else
         list.insertafter(Tai_regalloc.alloc(r),position);
       add_edges_used(p);
       add_constraints(r);
       result:=r;
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

    function trgobj.getabtregister(list:Taasmoutput;subreg:tsubregister):Tregister;

    var p,i:Tsuperregister;
        r:Tregister;
        found:boolean;
        min : byte;
        adj:Pstring;

    begin
      min:=$ff;
      for i:=1 to length(abtlist) do
        if Tsuperregister(abtlist[i]) in unusedregs then
          begin
            p:=tsuperregister(abtlist[i]);
            min:=0;
            break;
          end;

      if min>0 then
        begin
          if maxreg<last_reg then
            begin
              inc(maxreg);
              p:=maxreg;
              min:=0;
            end
          else
            begin
              p:=first_imaginary;
              for i:=first_imaginary to maxreg do
               if (i in unusedregs) and
                  ((igraph.adjlist[i]=nil) or
                   (length(igraph.adjlist[i]^)<cpu_registers)) then
                begin
                  adj:=igraph.adjlist[i];
                  if adj=nil then
                    begin
                      p:=i;
                      min:=0;
                      break;  {We won't find smaller ones.}
                    end
                  else
                    if length(adj^)<min then
                      begin
                        p:=i;
                        min:=length(adj^);
                        if min=0 then
                          break;  {We won't find smaller ones.}
                      end;
                end;
            end;

           if min=$ff then
             begin
{$ifdef ALLOWDUPREG}
               result:=newreg(regtype,RS_INVALID,cgsize2subreg(size));
               exit;
{$else}
               internalerror(10);
{$endif}
             end;
         end;

       exclude(unusedregs,p);
       include(used_in_proc,p);
       r:=newreg(regtype,p,subreg);
       list.concat(Tai_regalloc.alloc(r));
       result:=r;
       add_edges_used(p);
       add_constraints(r);
       if pos(char(p),abtlist)=0 then
         abtlist:=abtlist+char(p);
    end;


    procedure trgobj.ungetregisterinline(list:Taasmoutput;
                position:Tai;r:Tregister);

    var supreg:Tsuperregister;

    begin
      supreg:=getsupreg(r);
      include(unusedregs,supreg);
      if position=nil then
        list.insert(Tai_regalloc.dealloc(r))
      else
        list.insertafter(Tai_regalloc.dealloc(r),position);
      add_edges_used(supreg);
      add_constraints(r);
    end;

    function trgobj.spill_registers(list:Taasmoutput;headertai:tai;const regs_to_spill:string):boolean;

    {Returns true if any help registers have been used.}

    var i:byte;
        p,q:Tai;
        regs_to_spill_set:Tsuperregisterset;
        spill_temps:^Tspill_temp_list;
        supreg : tsuperregister;
        templist : taasmoutput;
    begin
      spill_registers:=false;
      unusedregs:=[first_reg..last_reg];
      fillchar(degree,sizeof(degree),0);
      {Precoloured nodes should have an infinite degree, which we can approach
       by 255.}
      for i:=first_int_supreg to last_int_supreg do
        degree[i]:=255;
{      exclude(unusedregs,RS_STACK_POINTER_REG);}
      if current_procinfo.framepointer=NR_FRAME_POINTER_REG then
        {Make sure the register allocator won't allocate registers into ebp.}
        exclude(unusedregs,RS_FRAME_POINTER_REG);
      new(spill_temps);
      fillchar(spill_temps^,sizeof(spill_temps^),0);
      regs_to_spill_set:=[];
      { Allocate temps and insert in front of the list }
      templist:=taasmoutput.create;
      for i:=1 to length(regs_to_spill) do
        begin
          {Alternative representation.}
          include(regs_to_spill_set,Tsuperregister(regs_to_spill[i]));
          {Clear all interferences of the spilled register.}
          clear_interferences(Tsuperregister(regs_to_spill[i]));
          {Get a temp for the spilled register}
          tg.gettemp(templist,4,tt_noreuse,spill_temps^[Tsuperregister(regs_to_spill[i])]);
        end;
      list.insertlistafter(headertai,templist);
      templist.free;
      { Walk through all instructions, we can start with the headertai,
        because before the header tai is only symbols }
      p:=headertai;
      while assigned(p) do
        begin
          case p.typ of
            ait_regalloc:
              begin
                {A register allocation of a spilled register can be removed.}
                supreg:=getsupreg(Tai_regalloc(p).reg);
                if supreg in regs_to_spill_set then
                  begin
                    q:=p;
                    p:=Tai(p.next);
                    list.remove(q);
                    continue;
                  end
                else
                  if Tai_regalloc(p).allocation then
                    exclude(unusedregs,supreg)
                  else
                    include(unusedregs,supreg);
              end;
            ait_instruction:
              begin
                aktfilepos:=Taicpu_abstract(p).fileinfo;
                if Taicpu_abstract(p).spill_registers(list,@getregisterinline,
                                                      @ungetregisterinline,
                                                      regs_to_spill_set,
                                                      unusedregs,
                                                      spill_temps^) then
                  spill_registers:=true;
                if Taicpu_abstract(p).is_move then
                  add_move_instruction(Taicpu(p));
              end;
          end;
          p:=Tai(p.next);
        end;
      aktfilepos:=current_procinfo.exitpos;
      for i:=1 to length(regs_to_spill) do
        tg.ungettemp(list,spill_temps^[Tsuperregister(regs_to_spill[i])]);
      dispose(spill_temps);
    end;

end.

{
  $Log$
  Revision 1.83  2003-10-10 17:48:14  peter
    * old trgobj moved to x86/rgcpu and renamed to trgx86fpu
    * tregisteralloctor renamed to trgobj
    * removed rgobj from a lot of units
    * moved location_* and reference_* to cgobj
    * first things for mmx register allocation

  Revision 1.82  2003/10/09 21:31:37  daniel
    * Register allocator splitted, ans abstract now

  Revision 1.81  2003/10/01 20:34:49  peter
    * procinfo unit contains tprocinfo
    * cginfo renamed to cgbase
    * moved cgmessage to verbose
    * fixed ppc and sparc compiles

  Revision 1.80  2003/09/30 19:54:42  peter
    * reuse registers with the least conflicts

  Revision 1.79  2003/09/29 20:58:56  peter
    * optimized releasing of registers

  Revision 1.78  2003/09/28 13:41:12  peter
    * return reg 255 when allowdupreg is defined

  Revision 1.77  2003/09/25 16:19:32  peter
    * fix filepositions
    * insert spill temp allocations at the start of the proc

  Revision 1.76  2003/09/16 16:17:01  peter
    * varspez in calls to push_addr_param

  Revision 1.75  2003/09/12 19:07:42  daniel
    * Fixed fast spilling functionality by re-adding the code that initializes
      precoloured nodes to degree 255. I would like to play hangman on the one
      who removed that code.

  Revision 1.74  2003/09/11 11:54:59  florian
    * improved arm code generation
    * move some protected and private field around
    * the temp. register for register parameters/arguments are now released
      before the move to the parameter register is done. This improves
      the code in a lot of cases.

  Revision 1.73  2003/09/09 20:59:27  daniel
    * Adding register allocation order

  Revision 1.72  2003/09/09 15:55:44  peter
    * use register with least interferences in spillregister

  Revision 1.71  2003/09/07 22:09:35  peter
    * preparations for different default calling conventions
    * various RA fixes

  Revision 1.70  2003/09/03 21:06:45  peter
    * fixes for FPU register allocation

  Revision 1.69  2003/09/03 15:55:01  peter
    * NEWRA branch merged

  Revision 1.68  2003/09/03 11:18:37  florian
    * fixed arm concatcopy
    + arm support in the common compiler sources added
    * moved some generic cg code around
    + tfputype added
    * ...

  Revision 1.67.2.5  2003/08/31 20:44:07  peter
    * fixed getexplicitregisterint tregister value

  Revision 1.67.2.4  2003/08/31 20:40:50  daniel
    * Fixed add_edges_used

  Revision 1.67.2.3  2003/08/29 17:28:59  peter
    * next batch of updates

  Revision 1.67.2.2  2003/08/28 18:35:08  peter
    * tregister changed to cardinal

  Revision 1.67.2.1  2003/08/27 19:55:54  peter
    * first tregister patch

  Revision 1.67  2003/08/23 10:46:21  daniel
    * Register allocator bugfix for h2pas

  Revision 1.66  2003/08/17 16:59:20  jonas
    * fixed regvars so they work with newra (at least for ppc)
    * fixed some volatile register bugs
    + -dnotranslation option for -dnewra, which causes the registers not to
      be translated from virtual to normal registers. Requires support in
      the assembler writer as well, which is only implemented in aggas/
      agppcgas currently

  Revision 1.65  2003/08/17 14:32:48  daniel
    * Precoloured nodes now have an infinite degree approached with 255,
      like they should.

  Revision 1.64  2003/08/17 08:48:02  daniel
   * Another register allocator bug fixed.
   * cpu_registers set to 6 for i386

  Revision 1.63  2003/08/09 18:56:54  daniel
    * cs_regalloc renamed to cs_regvars to avoid confusion with register
      allocator
    * Some preventive changes to i386 spillinh code

  Revision 1.62  2003/08/03 14:09:50  daniel
    * Fixed a register allocator bug
    * Figured out why -dnewra generates superfluous "mov reg1,reg2"
      statements: changes in location_force. These moves are now no longer
      constrained so they are optimized away.

  Revision 1.61  2003/07/21 13:32:39  jonas
    * add_edges_used() is now also called for registers allocated with
      getexplicitregisterint()
    * writing the intereference graph is now only done with -dradebug2 and
      the created files are now called "igraph.<module_name>"

  Revision 1.60  2003/07/06 15:31:21  daniel
    * Fixed register allocator. *Lots* of fixes.

  Revision 1.59  2003/07/06 15:00:47  jonas
    * fixed my previous completely broken commit. It's not perfect though,
      registers > last_int_supreg and < max_intreg may still be "translated"

  Revision 1.58  2003/07/06 14:45:05  jonas
    * support integer registers that are not managed by newra (ie. don't
      translate register numbers that fall outside the range
      first_int_supreg..last_int_supreg)

  Revision 1.57  2003/07/02 22:18:04  peter
    * paraloc splitted in callerparaloc,calleeparaloc
    * sparc calling convention updates

  Revision 1.56  2003/06/17 16:34:44  jonas
    * lots of newra fixes (need getfuncretparaloc implementation for i386)!
    * renamed all_intregisters to volatile_intregisters and made it
      processor dependent

  Revision 1.55  2003/06/14 14:53:50  jonas
    * fixed newra cycle for x86
    * added constants for indicating source and destination operands of the
      "move reg,reg" instruction to aasmcpu (and use those in rgobj)

  Revision 1.54  2003/06/13 21:19:31  peter
    * current_procdef removed, use current_procinfo.procdef instead

  Revision 1.53  2003/06/12 21:11:10  peter
    * ungetregisterfpu gets size parameter

  Revision 1.52  2003/06/12 16:43:07  peter
    * newra compiles for sparc

  Revision 1.51  2003/06/09 14:54:26  jonas
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
