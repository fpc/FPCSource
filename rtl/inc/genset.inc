{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2001 by the Free Pascal development team

    Include file with set operations called by the compiler

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

 TYPE
   { TNormalSet = array[0..31] of byte;}
    TNormalSet = array[0..7] of longint;

{$ifndef FPC_SYSTEM_HAS_FPC_SET_LOAD_SMALL}
{ Error No pascal version of FPC_SET_LOAD_SMALL}
 { THIS DEPENDS ON THE ENDIAN OF THE ARCHITECTURE!
   Not anymore PM}

procedure do_load_small(p : pointer;l:longint);[public,alias:'FPC_SET_LOAD_SMALL'];
 {
  load a normal set p from a smallset l
 }
 begin
   Fillchar(p^,SizeOf(TNormalSet),#0);
   TNormalSet(p^)[0] := l;
 end;
{$endif FPC_SYSTEM_HAS_FPC_SET_LOAD_SMALL}


{$ifndef FPC_SYSTEM_HAS_FPC_SET_CREATE_ELEMENT}
 procedure do_create_element(p : pointer;b : byte);[public,alias:'FPC_SET_CREATE_ELEMENT'];
 {
  create a new set in p from an element b
 }
 begin
   Fillchar(p^,SizeOf(TNormalSet),#0);
   TNormalSet(p^)[b div 32] := 1 shl (b mod 32);
 end;
{$endif FPC_SYSTEM_HAS_FPC_SET_CREATE_ELEMENT}

{$ifndef FPC_SYSTEM_HAS_FPC_SET_SET_BYTE}
 procedure do_set_byte(p : pointer;b : byte);[public,alias:'FPC_SET_SET_BYTE'];
 {
  add the element b to the set pointed by p
 }
  var
   c: longint;
  begin
    c := TNormalSet(p^)[b div 32];
    c := (1 shl (b mod 32)) or c;
    TNormalSet(p^)[b div 32] := c;
  end;
{$endif FPC_SYSTEM_HAS_FPC_SET_SET_BYTE}


{$ifndef FPC_SYSTEM_HAS_FPC_SET_UNSET_BYTE}
 procedure do_unset_byte(p : pointer;b : byte);[public,alias:'FPC_SET_UNSET_BYTE'];
 {
   suppresses the element b to the set pointed by p
   used for exclude(set,element)
 }
  var
   c: longint;
  begin
    c := TNormalSet(p^)[b div 32];
    c := c and not (1 shl (b mod 32));
    TNormalSet(p^)[b div 32] := c;
  end;
{$endif FPC_SYSTEM_HAS_FPC_SET_UNSET_BYTE}


{$ifndef FPC_SYSTEM_HAS_FPC_SET_SET_RANGE}
 procedure do_set_range(p : pointer;l,h : byte);[public,alias:'FPC_SET_SET_RANGE'];
 {
  bad implementation, but it's very seldom used
 }
  var
   i: integer;
   c: longint;
  begin
    for i:=l to h do
      begin
        c := TNormalSet(p^)[i div 32];
        c := (1 shl (i mod 32)) or c;
        TNormalSet(p^)[i div 32] := c;
      end;
  end;
{$endif}


{$ifndef FPC_SYSTEM_HAS_FPC_SET_IN_BYTE}

{ saveregisters is a bit of overkill, but this routine should save all registers }
{ and it should be overriden for each platform and be written in assembler       }
{ by saving all required registers.                                              }
 function do_in_byte(p : pointer;b : byte):boolean;[public,alias:'FPC_SET_IN_BYTE'];saveregisters;
 {
   tests if the element b is in the set p the carryflag is set if it present
 }
  var
    c: longint;
  begin
    c := TNormalSet(p^)[b div 32];
    if ((1 shl (b mod 32)) and c) <> 0 then
     do_in_byte := TRUE
    else
     do_in_byte := FALSE;
  end;
{$endif}


{$ifndef FPC_SYSTEM_HAS_FPC_SET_ADD_SETS}
 procedure do_add_sets(set1,set2,dest : pointer);[public,alias:'FPC_SET_ADD_SETS'];
 {
   adds set1 and set2 into set dest
 }
  var
    i: integer;
   begin
     for i:=0 to 7 do
       TnormalSet(dest^)[i] := TNormalSet(set1^)[i] or TNormalSet(set2^)[i];
   end;
{$endif}


{$ifndef FPC_SYSTEM_HAS_FPC_SET_MUL_SETS}
 procedure do_mul_sets(set1,set2,dest:pointer);[public,alias:'FPC_SET_MUL_SETS'];
 {
   multiplies (takes common elements of) set1 and set2 result put in dest
 }
   var
    i: integer;
   begin
     for i:=0 to 7 do
       TnormalSet(dest^)[i] := TNormalSet(set1^)[i] and TNormalSet(set2^)[i];
   end;
{$endif}


{$ifndef FPC_SYSTEM_HAS_FPC_SET_SUB_SETS}
 procedure do_sub_sets(set1,set2,dest:pointer);[public,alias:'FPC_SET_SUB_SETS'];
 {
  computes the diff from set1 to set2 result in dest
 }
   var
    i: integer;
   begin
     for i:=0 to 7 do
       TnormalSet(dest^)[i] := TNormalSet(set1^)[i] and not TNormalSet(set2^)[i];
   end;
{$endif}


{$ifndef FPC_SYSTEM_HAS_FPC_SET_SYMDIF_SETS}
 procedure do_symdif_sets(set1,set2,dest:pointer);[public,alias:'FPC_SET_SYMDIF_SETS'];
 {
   computes the symetric diff from set1 to set2 result in dest
 }
   var
    i: integer;
   begin
     for i:=0 to 7 do
       TnormalSet(dest^)[i] := TNormalSet(set1^)[i] xor TNormalSet(set2^)[i];
   end;
{$endif}

{$ifndef FPC_SYSTEM_HAS_FPC_SET_COMP_SETS}
{ saveregisters is a bit of overkill, but this routine should save all registers }
{ and it should be overriden for each platform and be written in assembler       }
{ by saving all required registers.                                              }
 function do_comp_sets(set1,set2 : pointer):boolean;[public,alias:'FPC_SET_COMP_SETS'];saveregisters;
 {
  compares set1 and set2 zeroflag is set if they are equal
 }
   var
    i: integer;
   begin
     do_comp_sets := false;
     for i:=0 to 7 do
       if TNormalSet(set1^)[i] <> TNormalSet(set2^)[i] then
         exit;
     do_comp_sets := true;
   end;
{$endif}



{$ifndef FPC_SYSTEM_HAS_FPC_SET_CONTAINS_SET}
{ saveregisters is a bit of overkill, but this routine should save all registers }
{ and it should be overriden for each platform and be written in assembler       }
{ by saving all required registers.                                              }
 function do_contains_sets(set1,set2 : pointer):boolean;[public,alias:'FPC_SET_CONTAINS_SETS'];saveregisters;
 {
  on exit, zero flag is set if set1 <= set2 (set2 contains set1)
 }
 var
  i : integer;
 begin
   do_contains_sets := false;
   for i:=0 to 7 do
     if (TNormalSet(set1^)[i] and TNormalSet(set2^)[i]) <> TNormalSet(set1^)[i] then
       exit;
   do_contains_sets := true;
 end;
{$endif}

{
  $Log$
  Revision 1.4  2001-06-27 21:37:38  peter
    * v10 merges

  Revision 1.3  2001/05/18 22:59:59  peter
    * merged fixes branch fixes

  Revision 1.2  2001/05/09 19:57:07  peter
  *** empty log message ***

}

