{
    $Id$
    Copyright (c) 1998-2000 by Michael Van Canneyt

    Contains a generic assembler instruction object;

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
Unit tainst;

interface

  Uses aasm,cpubase,cpuinfo,cobjects;

Type

pairegalloc = ^tairegalloc;
tairegalloc = object(tai)
   allocation : boolean;
   reg        : tregister;
   constructor alloc(r : tregister);
   constructor dealloc(r : tregister);
end;

painstruction = ^tainstruction;
tainstruction = object(tai)
  is_jmp    : boolean; { is this instruction a jump? (needed for optimizer) }
  opcode    : tasmop;
  condition : TAsmCond;
  ops       : longint;
  oper      : array[0..max_operands-1] of toper;
{$ifdef i386}
  segprefix : tregister;
{$endif}           
  Constructor init(op : tasmop);
  Destructor Done;virtual;
  function getcopy:plinkedlist_item;virtual;
  procedure loadconst(opidx:longint;l:longint);
  procedure loadsymbol(opidx:longint;s:pasmsymbol;sofs:longint);
  procedure loadref(opidx:longint;p:preference);
  procedure loadreg(opidx:longint;r:tregister);
  procedure loadoper(opidx:longint;o:toper);
  procedure SetCondition(c:TAsmCond);
  end;

implementation

{*****************************************************************************
                                 TaiRegAlloc
*****************************************************************************}

constructor tairegalloc.alloc(r : tregister);
  begin
    inherited init;
    typ:=ait_regalloc;
    allocation:=true;
    reg:=r;
  end;


constructor tairegalloc.dealloc(r : tregister);
  begin
    inherited init;
    typ:=ait_regalloc;
    allocation:=false;
    reg:=r;
  end;

{ ---------------------------------------------------------------------
    TaInstruction Constructor/Destructor
  ---------------------------------------------------------------------}



Constructor tainstruction.init(op : tasmop);

begin
   inherited init;
   typ:=ait_instruction;
   is_jmp:=false;
   opcode:=op;
   ops:=0;
   fillchar(condition,sizeof(condition),0);
   fillchar(oper,sizeof(oper),0);
end;



Destructor Tainstruction.Done;

Var i : longint;

begin
  for i:=1 to ops do
  if (oper[i-1].typ=top_ref) then
    dispose(oper[i-1].ref);
  inherited done;
end;



{ ---------------------------------------------------------------------
    Loading of operands.
  ---------------------------------------------------------------------}



procedure tainstruction.loadconst(opidx:longint;l:longint);

begin
  if opidx>=ops then
   ops:=opidx+1;
  with oper[opidx] do
   begin
     if typ=top_ref then
      disposereference(ref);
     val:=l;
     typ:=top_const;
   end;
end;



procedure tainstruction.loadsymbol(opidx:longint;s:pasmsymbol;sofs:longint);
begin
  if opidx>=ops then
   ops:=opidx+1;
  with oper[opidx] do
   begin
     if typ=top_ref then
      disposereference(ref);
     sym:=s;
     symofs:=sofs;
     typ:=top_symbol;
   end;
  { Mark the symbol as used }
  if assigned(s) then
   inc(s^.refs);
end;



procedure tainstruction.loadref(opidx:longint;p:preference);
begin
  if opidx>=ops then
   ops:=opidx+1;
  with oper[opidx] do
   begin
     if typ=top_ref then
      disposereference(ref);
     if p^.is_immediate then
       begin
         val:=p^.offset;
         disposereference(p);
         typ:=top_const;
       end
     else
       begin
         ref:=p;
{ We allow this exception for i386, since overloading this would be
  too much of a a speed penalty}
{$ifdef i386}
         if not(ref^.segment in [R_DS,R_NO]) then
           segprefix:=ref^.segment;
{$endif}
         typ:=top_ref;
         { mark symbol as used }
         if assigned(ref^.symbol) then
           inc(ref^.symbol^.refs);
       end;
   end;
end;



procedure tainstruction.loadreg(opidx:longint;r:tregister);
begin
  if opidx>=ops then
   ops:=opidx+1;
  with oper[opidx] do
   begin
     if typ=top_ref then
      disposereference(ref);
     reg:=r;
     typ:=top_reg;
   end;
end;



procedure tainstruction.loadoper(opidx:longint;o:toper);
begin
  if opidx>=ops then
   ops:=opidx+1;
  if oper[opidx].typ=top_ref then
    disposereference(oper[opidx].ref);
  oper[opidx]:=o;
  { copy also the reference }
  if oper[opidx].typ=top_ref then
   oper[opidx].ref:=newreference(o.ref^);
end;


{ ---------------------------------------------------------------------
    Miscellaneous methods.
  ---------------------------------------------------------------------}

procedure tainstruction.SetCondition(c:TAsmCond);
  begin
     condition:=c;
  end;


Function tainstruction.getcopy:plinkedlist_item;

var
  i : longint;
  p : plinkedlist_item;
begin
  p:=inherited getcopy;
  { make a copy of the references }
  for i:=1 to ops do
   if (painstruction(p)^.oper[i-1].typ=top_ref) then
    begin
      new(painstruction(p)^.oper[i-1].ref);
      painstruction(p)^.oper[i-1].ref^:=oper[i-1].ref^;
    end;
  getcopy:=p;
end;

end.

{
  $Log$
  Revision 1.6  2000-01-07 01:14:54  peter
    * updated copyright to 2000

  Revision 1.5  1999/09/10 18:48:11  florian
    * some bug fixes (e.g. must_be_valid and procinfo.funcret_is_valid)
    * most things for stored properties fixed

  Revision 1.4  1999/09/03 13:10:11  jonas
    * condition is now zeroed using fillchar\n    because on powerpc it's a record now

  Revision 1.3  1999/08/26 14:52:59  jonas
    * added segprefix field for i386 in tainstruction object

  Revision 1.2  1999/08/06 16:38:37  jonas
    * declared getcopy virtual, since it's already declared as such
      in cobjects.pas (FPC doesn't error on that, TP does)

  Revision 1.1  1999/08/06 16:04:05  michael
  + introduced tainstruction

}
