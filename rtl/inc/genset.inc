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
   TNormalSet = array[0..31] of byte;

{$ifndef FPC_SYSTEM_HAS_FPC_SET_LOAD_SMALL}
{$Error No pascal version of FPC_SET_LOAD_SMALL}
 { THIS DEPENDS ON THE ENDIAN OF THE ARCHITECTURE! }

{ procedure do_load_small(p : pointer;l:longint);[public,alias:'FPC_SET_LOAD_SMALL'];}
 {
  load a normal set p from a smallset l
 }
{ begin
   for i:=0 to 3 do
    TNormalSet(p^)[i] := l shr (8*i);
   RunError(255);
 end;}
{$endif FPC_SYSTEM_HAS_FPC_SET_LOAD_SMALL}


{$ifndef FPC_SYSTEM_HAS_FPC_SET_CREATE_ELEMENT}
 procedure do_create_element(p : pointer;b : byte);[public,alias:'FPC_SET_CREATE_ELEMENT'];
 {
  create a new set in p from an element b
 }
 begin
   Fillchar(p^,32,#0);
   TNormalSet(p^)[b div 8] := 1 shl (b mod 8);
 end;
{$endif FPC_SYSTEM_HAS_FPC_SET_CREATE_ELEMENT}

{$ifndef FPC_SYSTEM_HAS_FPC_SET_SET_BYTE}
 procedure do_set_byte(p : pointer;b : byte);[public,alias:'FPC_SET_SET_BYTE'];
 {
  add the element b to the set pointed by p
 }
  var
   c: byte;
  begin
    c := TNormalSet(p^)[b div 8];
    c := (1 shl (b mod 8)) or c;
    TNormalSet(p^)[b div 8] := c;
  end;
{$endif FPC_SYSTEM_HAS_FPC_SET_SET_BYTE}


{$ifndef FPC_SYSTEM_HAS_FPC_SET_UNSET_BYTE}
 procedure do_unset_byte(p : pointer;b : byte);[public,alias:'FPC_SET_UNSET_BYTE'];
 {
   suppresses the element b to the set pointed by p
   used for exclude(set,element)
 }
  var
   c: byte;
  begin
    c := TNormalSet(p^)[b div 8];
    c := c and not (1 shl (b mod 8));
    TNormalSet(p^)[b div 8] := c;
  end;
{$endif FPC_SYSTEM_HAS_FPC_SET_UNSET_BYTE}


{$ifndef FPC_SYSTEM_HAS_FPC_SET_SET_RANGE}
 procedure do_set_range(p : pointer;l,h : byte);[public,alias:'FPC_SET_SET_RANGE'];
 {
  bad implementation, but it's very seldom used
 }
  var
   i: integer;
   c: byte;
  begin
    for i:=l to h do
      begin
        c := TNormalSet(p^)[i div 8];
        c := (1 shl (i mod 8)) or c;
        TNormalSet(p^)[i div 8] := c;
      end;
  end;
{$endif}


{$ifndef FPC_SYSTEM_HAS_FPC_SET_IN_BYTE}
 function do_in_byte(p : pointer;b : byte):boolean;[public,alias:'FPC_SET_IN_BYTE'];
 {
   tests if the element b is in the set p the carryflag is set if it present
 }
  var
    c: byte;
  begin
    c := TNormalSet(p^)[b div 8];
    if ((1 shl (b mod 8)) and c) <> 0 then
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
     for i:=0 to 31 do
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
     for i:=0 to 31 do
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
     for i:=0 to 31 do
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
     for i:=0 to 31 do
       TnormalSet(dest^)[i] := TNormalSet(set1^)[i] xor TNormalSet(set2^)[i];
   end;
{$endif}

{$ifndef FPC_SYSTEM_HAS_FPC_SET_COMP_SETS}
 function do_comp_sets(set1,set2 : pointer):boolean;[public,alias:'FPC_SET_COMP_SETS'];
 {
  compares set1 and set2 zeroflag is set if they are equal
 }
   var
    i: integer;
   begin
     do_comp_sets := false;
     for i:=0 to 31 do
       if TNormalSet(set1^)[i] <> TNormalSet(set2^)[i] then
         exit;
     do_comp_sets := true;
   end;
{$endif}



{$ifndef FPC_SYSTEM_HAS_FPC_SET_CONTAINS_SET}
 function do_contains_sets(set1,set2 : pointer):boolean;[public,alias:'FPC_SET_CONTAINS_SETS'];
 {
  on exit, zero flag is set if set1 <= set2 (set2 contains set1)
 }
 var
  i : integer;
 begin
   do_contains_sets := false;
   for i:=0 to 31 do
     if (TNormalSet(set1^)[i] and TNormalSet(set2^)[i]) <> TNormalSet(set1^)[i] then
       exit;
   do_contains_sets := true;
 end;
{$endif}

{
  $Log$
  Revision 1.2  2001-05-09 19:57:07  peter
  *** empty log message ***

}

