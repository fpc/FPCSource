{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2001 by the Free Pascal development team

    Include file with set operations called by the compiler

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$ifndef FPC_SYSTEM_HAS_FPC_SET_LOAD_SMALL}
{ Error No pascal version of FPC_SET_LOAD_SMALL}
 { THIS DEPENDS ON THE ENDIAN OF THE ARCHITECTURE!
   Not anymore PM}

function fpc_set_load_small(l: fpc_small_set): fpc_normal_set; [public,alias:'FPC_SET_LOAD_SMALL']; {$ifdef hascompilerproc} compilerproc; {$endif}
 {
  load a normal set p from a smallset l
 }
 begin
   fpc_set_load_small[0] := l;
   FillDWord(fpc_set_load_small[1],7,0);
 end;
{$endif FPC_SYSTEM_HAS_FPC_SET_LOAD_SMALL}


{$ifndef FPC_SYSTEM_HAS_FPC_SET_CREATE_ELEMENT}
function fpc_set_create_element(b : byte): fpc_normal_set;[public,alias:'FPC_SET_CREATE_ELEMENT']; {$ifdef hascompilerproc} compilerproc; {$endif}
 {
  create a new set in p from an element b
 }
 begin
   FillDWord(fpc_set_create_element,SizeOf(fpc_set_create_element) div 4,0);
   fpc_set_create_element[b div 32] := 1 shl (b mod 32);
 end;
{$endif FPC_SYSTEM_HAS_FPC_SET_CREATE_ELEMENT}

{$ifndef FPC_SYSTEM_HAS_FPC_SET_SET_BYTE}

{$ifdef hascompilerproc}
 function fpc_set_set_byte(const source: fpc_normal_set; b : byte): fpc_normal_set; compilerproc;
 {
  add the element b to the set "source"
 }
  var
   c: longint;
  begin
    move(source,fpc_set_set_byte,sizeof(source));
    c := fpc_set_set_byte[b div 32];
    c := (1 shl (b mod 32)) or c;
    fpc_set_set_byte[b div 32] := c;
  end;
{$else hascompilerproc}
 procedure do_set_byte(p : pointer;b : byte);[public,alias:'FPC_SET_SET_BYTE'];
 {
  add the element b to the set pointed by p
 }
  var
   c: longint;
  begin
    c := fpc_normal_set(p^)[b div 32];
    c := (1 shl (b mod 32)) or c;
    fpc_normal_set(p^)[b div 32] := c;
  end;
{$endif hascompilerproc}
{$endif FPC_SYSTEM_HAS_FPC_SET_SET_BYTE}


{$ifndef FPC_SYSTEM_HAS_FPC_SET_UNSET_BYTE}

{$ifdef hascompilerproc}
function fpc_set_unset_byte(const source: fpc_normal_set; b : byte): fpc_normal_set; compilerproc;
 {
   suppresses the element b to the set pointed by p
   used for exclude(set,element)
 }
  var
   c: longint;
  begin
    move(source,fpc_set_unset_byte,sizeof(source));
    c := fpc_set_unset_byte[b div 32];
    c := c and not (1 shl (b mod 32));
    fpc_set_unset_byte[b div 32] := c;
  end;
{$else hascompilerproc}
procedure do_unset_byte(p : pointer;b : byte);[public,alias:'FPC_SET_UNSET_BYTE'];
 {
   suppresses the element b to the set pointed by p
   used for exclude(set,element)
 }
  var
   c: longint;
  begin
    c := fpc_normal_set(p^)[b div 32];
    c := c and not (1 shl (b mod 32));
    fpc_normal_set(p^)[b div 32] := c;
  end;
{$endif hascompilerproc}
{$endif FPC_SYSTEM_HAS_FPC_SET_UNSET_BYTE}


{$ifndef FPC_SYSTEM_HAS_FPC_SET_SET_RANGE}
{$ifdef hascompilerproc}
 function fpc_set_set_range(const orgset: fpc_normal_set; l,h : byte): fpc_normal_set; compilerproc;
 {
   adds the range [l..h] to the set orgset
 }
  var
   i: integer;
   c: longint;
  begin
    move(orgset,fpc_set_set_range,sizeof(orgset));
    for i:=l to h do
      begin
        c := fpc_set_set_range[i div 32];
        c := (1 shl (i mod 32)) or c;
        fpc_set_set_range[i div 32] := c;
      end;
  end;
{$else hascompilerproc}
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
        c := fpc_normal_set(p^)[i div 32];
        c := (1 shl (i mod 32)) or c;
        fpc_normal_set(p^)[i div 32] := c;
      end;
  end;
{$endif hascompilerproc}
{$endif ndef FPC_SYSTEM_HAS_FPC_SET_SET_RANGE}


{$ifndef FPC_SYSTEM_HAS_FPC_SET_IN_BYTE}

 function fpc_set_in_byte(const p: fpc_normal_set; b: byte): boolean; [public,alias:'FPC_SET_IN_BYTE']; {$ifdef hascompilerproc} compilerproc; {$else} {$ifndef NOSAVEREGISTERS}saveregisters;{$endif} {$endif}
 {
   tests if the element b is in the set p the carryflag is set if it present
 }
  begin
    fpc_set_in_byte := (p[b div 32] and (1 shl (b mod 32))) <> 0;
  end;
{$endif}


{$ifndef FPC_SYSTEM_HAS_FPC_SET_ADD_SETS}
{$ifdef hascompilerproc}
 function fpc_set_add_sets(const set1,set2: fpc_normal_set): fpc_normal_set;[public,alias:'FPC_SET_ADD_SETS']; compilerproc;
 var
   dest: fpc_normal_set absolute fpc_set_add_sets;
{$else hascompilerproc}
 procedure do_add_sets(const set1,set2: fpc_normal_Set; var dest : fpc_normal_set);[public,alias:'FPC_SET_ADD_SETS'];
{$endif hascompilerproc}
 {
   adds set1 and set2 into set dest
 }
  var
    i: integer;
   begin
     for i:=0 to 7 do
       dest[i] := set1[i] or set2[i];
   end;
{$endif}


{$ifndef FPC_SYSTEM_HAS_FPC_SET_MUL_SETS}
{$ifdef hascompilerproc}
 function fpc_set_mul_sets(const set1,set2: fpc_normal_set): fpc_normal_set;[public,alias:'FPC_SET_MUL_SETS']; compilerproc;
 var
   dest: fpc_normal_set absolute fpc_set_mul_sets;
{$else hascompilerproc}
 procedure do_mul_sets(const set1,set2: fpc_normal_set; var dest: fpc_normal_set);[public,alias:'FPC_SET_MUL_SETS'];
{$endif hascompilerproc}
 {
   multiplies (takes common elements of) set1 and set2 result put in dest
 }
   var
    i: integer;
   begin
     for i:=0 to 7 do
       dest[i] := set1[i] and set2[i];
   end;
{$endif}


{$ifndef FPC_SYSTEM_HAS_FPC_SET_SUB_SETS}
{$ifdef hascompilerproc}
 function fpc_set_sub_sets(const set1,set2: fpc_normal_set): fpc_normal_set;[public,alias:'FPC_SET_SUB_SETS']; compilerproc;
 var
   dest: fpc_normal_set absolute fpc_set_sub_sets;
{$else hascompilerproc}
 procedure do_sub_sets(const set1,set2: fpc_normal_set; var dest: fpc_normal_set);[public,alias:'FPC_SET_SUB_SETS'];
{$endif hascompilerproc}
 {
  computes the diff from set1 to set2 result in dest
 }
   var
    i: integer;
   begin
     for i:=0 to 7 do
       dest[i] := set1[i] and not set2[i];
   end;
{$endif}


{$ifndef FPC_SYSTEM_HAS_FPC_SET_SYMDIF_SETS}
{$ifdef hascompilerproc}
 function fpc_set_symdif_sets(const set1,set2: fpc_normal_set): fpc_normal_set;[public,alias:'FPC_SET_SYMDIF_SETS']; compilerproc;
 var
   dest: fpc_normal_set absolute fpc_set_symdif_sets;
{$else hascompilerproc}
 procedure do_symdif_sets(const set1,set2: fpc_normal_set; var dest: fpc_normal_set);[public,alias:'FPC_SET_SYMDIF_SETS'];
{$endif hascompilerproc}
 {
   computes the symetric diff from set1 to set2 result in dest
 }
   var
    i: integer;
   begin
     for i:=0 to 7 do
       dest[i] := set1[i] xor set2[i];
   end;
{$endif}

{$ifndef FPC_SYSTEM_HAS_FPC_SET_COMP_SETS}
 function fpc_set_comp_sets(const set1,set2 : fpc_normal_set):boolean;[public,alias:'FPC_SET_COMP_SETS'];{$ifdef hascompilerproc} compilerproc; {$else} {$ifndef NOSAVEREGISTERS}saveregisters;{$endif} {$endif}
 {
  compares set1 and set2 zeroflag is set if they are equal
 }
   var
    i: integer;
   begin
     fpc_set_comp_sets:= false;
     for i:=0 to 7 do
       if set1[i] <> set2[i] then
         exit;
     fpc_set_comp_sets:= true;
   end;
{$endif}



{$ifndef FPC_SYSTEM_HAS_FPC_SET_CONTAINS_SET}
 function fpc_set_contains_sets(const set1,set2 : fpc_normal_set):boolean;[public,alias:'FPC_SET_CONTAINS_SETS'];{$ifdef hascompilerproc} compilerproc; {$else} saveregisters; {$endif}
 {
  on exit, zero flag is set if set1 <= set2 (set2 contains set1)
 }
 var
  i : integer;
 begin
   fpc_set_contains_sets:= false;
   for i:=0 to 7 do
     if (set1[i] and not set2[i]) <> 0 then
       exit;
   fpc_set_contains_sets:= true;
 end;
{$endif}


