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


{****************************************************************************
                                 Var sets
 ****************************************************************************}

const
  maxsetsize = 32;

{$ifndef FPC_SYSTEM_HAS_FPC_VARSET_LOAD_SMALL}
{
  convert sets
}
{$ifdef FPC_SETBASE_USED}
procedure fpc_varset_load(const l;sourcesize : longint;var dest;size,srcminusdstbase : ptrint); compilerproc;
  var
    srcptr, dstptr: pointer;
  begin
    srcptr:=@l;
    dstptr:=@dest;
    { going from a higher base to a lower base, e.g.
      src: 001f0000, base=2,size=4 -> 0000001f0000 in base 0
      dstr in base = 1 (-> srcminusdstbase = 1) -> to
      00001f0000, base=1 -> need to prepend "srcminusdstbase" zero bytes
    }
    if (srcminusdstbase>0) then
      begin
        { fill the skipped part with 0 }
        fillchar(dstptr^,srcminusdstbase,0);
        inc(dstptr,srcminusdstbase);
        dec(size,srcminusdstbase);
      end
    else if (srcminusdstbase<0) then
      begin
        { inc/dec switched since srcminusdstbase < 0 }
        dec(srcptr,srcminusdstbase);
        inc(sourcesize,srcminusdstbase);
      end;

    if sourcesize>size then
      sourcesize:=size;
    move(srcptr^,dstptr^,sourcesize);
    { fill the  leftover (if any) with 0 }
    FillChar((dstptr+sourcesize)^,size-sourcesize,0);
  end;

{$else FPC_SETBASE_USED}

procedure fpc_varset_load(const l;sourcesize : longint;var dest;size : ptrint); compilerproc;
  begin
    if sourcesize>size then
      sourcesize:=size;
    move(l,plongint(@dest)^,sourcesize);
    FillChar((@dest+sourcesize)^,size-sourcesize,0);
  end;
{$endif FPC_SETBASE_USED}

{$endif ndef FPC_SYSTEM_HAS_FPC_SET_LOAD_SMALL}


{$ifndef FPC_SYSTEM_HAS_FPC_VARSET_CREATE_ELEMENT}
{
  create a new set in p from an element b
}
procedure fpc_varset_create_element(b,size : ptrint; var data); compilerproc;
  type
    tbsetarray = bitpacked array[0..high(sizeint)-1] of 0..1;
  begin
    FillChar(data,size,0);
    tbsetarray(data)[b]:=1;
  end;
{$endif ndef FPC_SYSTEM_HAS_FPC_VARSET_CREATE_ELEMENT}


{$ifndef FPC_SYSTEM_HAS_FPC_VARSET_SET_BYTE}
{
  add the element b to the set "source"
}
procedure fpc_varset_set(const source;var dest; b,size : ptrint); compilerproc;
  type
    tbsetarray = bitpacked array[0..high(sizeint)-1] of 0..1;
  begin
    move(source,dest,size);
    tbsetarray(dest)[b]:=1;
  end;
{$endif ndef FPC_SYSTEM_HAS_FPC_VARSET_SET_BYTE}


{$ifndef FPC_SYSTEM_HAS_FPC_VARSET_UNSET_BYTE}
{
   suppresses the element b to the set pointed by p
   used for exclude(set,element)
}
procedure fpc_varset_unset(const source;var dest; b,size : ptrint); compilerproc;
  type
    tbsetarray = bitpacked array[0..high(sizeint)-1] of 0..1;
  begin
    move(source,dest,size);
    tbsetarray(dest)[b]:=0;
  end;
{$endif ndef FPC_SYSTEM_HAS_FPC_VARSET_UNSET_BYTE}


{$ifndef FPC_SYSTEM_HAS_FPC_VARSET_SET_RANGE}
{
  adds the range [l..h] to the set orgset
}
procedure fpc_varset_set_range(const orgset; var dest;l,h,size : ptrint); compilerproc;
  var
    bp : pbyte;
    nbits,partbits,partbytes : sizeint;
  begin
    nbits:=h-l+1;
    if nbits<=0 then
      exit;
    if @orgset<>@dest then
      move(orgset,dest,size);
    bp:=pbyte(@dest)+l shr 3;
    partbits:=-l and 7;
    if partbits<>0 then { Head. }
      if partbits>=nbits then
        begin
          bp^:=bp^ or {$ifdef endian_little} (1 shl nbits-1) shl 8 shr partbits {$else} (1 shl nbits-1) shl partbits shr nbits {$endif};
          exit;
        end
      else
        begin
          bp^:=bp^ or {$ifdef endian_little} byte($FF00 shr partbits) {$else} (1 shl partbits-1) {$endif};
          inc(bp);
          nbits:=nbits-partbits;
        end;
    partbytes:=nbits shr 3;
    FillChar(bp^,partbytes,$FF); { Full bytes. }
    bp:=bp+partbytes;
    nbits:=nbits and 7;
    if nbits<>0 then { Tail. }
      bp^:=bp^ or {$ifdef endian_little} (1 shl nbits-1) {$else} byte($FF00 shr nbits) {$endif};
  end;
{$endif ndef FPC_SYSTEM_HAS_FPC_VARSET_SET_RANGE}


{$ifndef FPC_SYSTEM_HAS_FPC_VARSET_ADD_SETS}
{
  adds set1 and set2 into set dest
}
procedure fpc_varset_add_sets(const set1,set2; var dest;size : ptrint); compilerproc;
  type
    tbytearray = array[0..maxsetsize-1] of byte;
  begin
    if (size>=sizeof(PtrUint))
{$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
      and ((PtrUint(@set1) or PtrUint(@set2) or PtrUint(@dest) or PtrUint(size)) and (sizeof(PtrUint)-1)=0)
{$endif}
    then
      begin
        { Work in PtrUints from the end. }
        size:=size-sizeof(PtrUint);
        repeat
          PPtrUint(pointer(@dest)+size)^:=PPtrUint(pointer(@set1)+size)^ or PPtrUint(pointer(@set2)+size)^;
          size:=size-sizeof(PtrUint);
        until size<=0;
        { Head, overlapping in non-existing cases of size = sizeof(PtrUint) or size mod sizeof(PtrUint) <> 0.
          “Or” is idempotent, so dest = set1 or set2 does not matter. }
        PPtrUint(@dest)^:=PPtrUint(@set1)^ or PPtrUint(@set2)^;
        exit;
      end;
    repeat
      dec(size);
      tbytearray(dest)[size]:=tbytearray(set1)[size] or tbytearray(set2)[size];
    until size=0;
  end;
{$endif ndef FPC_SYSTEM_HAS_FPC_VARSET_ADD_SETS}


{$ifndef FPC_SYSTEM_HAS_FPC_VARSET_MUL_SETS}
{
  multiplies (takes common elements of) set1 and set2 result put in dest
}
procedure fpc_varset_mul_sets(const set1,set2; var dest;size : ptrint); compilerproc;
  type
    tbytearray = array[0..maxsetsize-1] of byte;
  begin
    { fpc_varset_add_sets with 'or' instead of 'and'. }
    if (size>=sizeof(PtrUint))
{$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
      and ((PtrUint(@set1) or PtrUint(@set2) or PtrUint(@dest) or PtrUint(size)) and (sizeof(PtrUint)-1)=0)
{$endif}
    then
      begin
        size:=size-sizeof(PtrUint);
        repeat
          PPtrUint(pointer(@dest)+size)^:=PPtrUint(pointer(@set1)+size)^ and PPtrUint(pointer(@set2)+size)^;
          size:=size-sizeof(PtrUint);
        until size<=0;
        PPtrUint(@dest)^:=PPtrUint(@set1)^ and PPtrUint(@set2)^;
        exit;
      end;
    repeat
      dec(size);
      tbytearray(dest)[size]:=tbytearray(set1)[size] and tbytearray(set2)[size];
    until size=0;
  end;
{$endif ndef FPC_SYSTEM_HAS_FPC_VARSET_MUL_SETS}


{$ifndef FPC_SYSTEM_HAS_FPC_VARSET_SUB_SETS}
{
  computes the diff from set1 to set2 result in dest
}
procedure fpc_varset_sub_sets(const set1,set2; var dest;size : ptrint); compilerproc;
  type
    tbytearray = array[0..maxsetsize-1] of byte;
  var
    headval : ptruint;
  begin
    if (size>=sizeof(PtrUint))
{$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
      and ((PtrUint(@set1) or PtrUint(@set2) or PtrUint(@dest) or PtrUint(size)) and (sizeof(PtrUint)-1)=0)
{$endif}
    then
      begin
        { Head, overlapping in non-existing cases of size = sizeof(PtrUint) or size mod sizeof(PtrUint) <> 0.
          “And not” is not idempotent, so head must be calculated in advance to work correctly when, in this non-existing case, dest = set1 or set2. }
        headval:=PPtrUint(@set1)^ and not PPtrUint(@set2)^;
        { Work in PtrUints from the end. }
        size:=size-sizeof(PtrUint);
        repeat
          PPtrUint(pointer(@dest)+size)^:=PPtrUint(pointer(@set1)+size)^ and not PPtrUint(pointer(@set2)+size)^;
          size:=size-sizeof(PtrUint);
        until size<=0;
        PPtrUint(@dest)^:=headval;
        exit;
      end;
    repeat
      dec(size);
      tbytearray(dest)[size]:=tbytearray(set1)[size] and not tbytearray(set2)[size];
    until size=0;
  end;
{$endif ndef FPC_SYSTEM_HAS_FPC_VARSET_SUB_SETS}


{$ifndef FPC_SYSTEM_HAS_FPC_VARSET_SYMDIF_SETS}
{
   computes the symetric diff from set1 to set2 result in dest
}
procedure fpc_varset_symdif_sets(const set1,set2; var dest;size : ptrint); compilerproc;
  type
    tbytearray = array[0..maxsetsize-1] of byte;
  var
    headval : ptruint;
  begin
    { fpc_varset_sub_sets with 'xor' instead of 'and not'. }
    if (size>=sizeof(PtrUint))
{$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
      and ((PtrUint(@set1) or PtrUint(@set2) or PtrUint(@dest) or PtrUint(size)) and (sizeof(PtrUint)-1)=0)
{$endif}
    then
      begin
        headval:=PPtrUint(@set1)^ xor PPtrUint(@set2)^;
        size:=size-sizeof(PtrUint);
        repeat
          PPtrUint(pointer(@dest)+size)^:=PPtrUint(pointer(@set1)+size)^ xor PPtrUint(pointer(@set2)+size)^;
          size:=size-sizeof(PtrUint);
        until size<=0;
        PPtrUint(@dest)^:=headval;
        exit;
      end;
    repeat
      dec(size);
      tbytearray(dest)[size]:=tbytearray(set1)[size] xor tbytearray(set2)[size];
    until size=0;
  end;
{$endif ndef FPC_SYSTEM_HAS_FPC_VARSET_SYMDIF_SETS}


{$ifndef FPC_SYSTEM_HAS_FPC_VARSET_COMP_SETS}
{
  compares set1 and set2 zeroflag is set if they are equal
}
function fpc_varset_comp_sets(const set1,set2;size : ptrint):boolean; compilerproc;
  begin
    result:=CompareByte(set1,set2,size)=0;
  end;
{$endif ndef FPC_SYSTEM_HAS_FPC_VARSET_COMP_SETS}


{$ifndef FPC_SYSTEM_HAS_FPC_VARSET_CONTAINS_SET}
{
  on exit, zero flag is set if set1 <= set2 (set2 contains set1)
}
function fpc_varset_contains_sets(const set1,set2;size : ptrint):boolean; compilerproc;
  var
    set1p,set2p,set1tail : pointer;
  begin
    result:=false;
    set1p:=@set1;
    set2p:=@set2;
    { Should scan left to right because first bits are more likely to differ. }
    if (size>=sizeof(PtrUint))
{$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
      and ((PtrUint(@set1) or PtrUint(@set2) or PtrUint(size)) and (sizeof(PtrUint)-1)=0)
{$endif}
    then
      begin
        set1tail:=set1p+size-sizeof(PtrUint);
        repeat
          if PPtrUint(set1p)^ and not PPtrUint(set2p)^<>0 then
            exit;
          inc(set1p,sizeof(PtrUint));
          inc(set2p,sizeof(PtrUint));
        until set1p>=set1tail;
        dec(set2p,set1p-set1tail); { set2p = “set2tail” }
        exit(PPtrUint(set1tail)^ and not PPtrUint(set2p)^=0);
      end;
    set1tail:=set1p+size;
    repeat
      if pbyte(set1p)^ and not pbyte(set2p)^<>0 then
        exit;
      inc(set1p);
      inc(set2p);
    until set1p=set1tail;
    result:=true;
  end;
{$endif ndef FPC_SYSTEM_HAS_FPC_VARSET_CONTAINS_SET}
