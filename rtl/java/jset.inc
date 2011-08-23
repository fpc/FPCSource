{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2011 by Jonas Maebe,
    members of the Free Pascal development team.

    This file implements support infrastructure for sets under the JVM

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

   function FpcBitSet.clone: JLObject;
     begin
       result:=inherited;
     end;


  function FpcBitSet.add(elem: jint): FpcBitSet;
    begin
      &set(elem);
      result:=self;
    end;


  function FpcBitSet.addAll(s: FpcBitSet): FpcBitSet;
    begin
      &or(s);
      result:=self;
    end;


  function FpcBitSet.remove(elem: jint): FpcBitSet;
    begin
      clear(elem);
      result:=self;
    end;


  function FpcBitSet.removeAll(s: FpcBitSet): FpcBitSet;
    begin
      andnot(s);
      result:=self;
    end;


  function FpcBitSet.retainAll(s: FpcBitSet): FpcBitSet;
    begin
      &and(s);
      result:=self;
    end;


  function FpcBitSet.contains(elem: jint): boolean;
    begin
      result:=get(elem);
    end;


  function FpcBitSet.containsAll(s: FpcBitSet): boolean;
    var
      tmp: FpcBitSet;
    begin
      tmp:=FpcBitSet(clone);
      tmp.&and(s);
      result:=tmp.equals(s);
    end;


  function FpcBitSet.symdif(s: FpcBitSet): FpcBitSet;
    begin
      s.&xor(s);
      result:=self;
    end;


  class function FpcBitSet.range(start, stop: jint): FpcBitSet;
    begin
      result:=FpcBitSet.create(stop);
      result.&set(start,stop+1);
    end;


  class function FpcBitSet.&of(elem: jint): FpcBitSet;
    begin
      result:=FpcBitSet.create(elem);
      result.&set(elem);
    end;


  procedure fpc_bitset_copy(const src: FpcBitSet; dst: FpcBitSet); compilerproc;
    begin
      dst.clear();
      dst.&or(src);
    end;


  procedure fpc_enumset_copy(const src: JUEnumSet; dst: JUEnumSet); compilerproc;
    begin
      dst.clear();
      dst.addAll(src);
    end;


  function fpc_enumset_symdif(const set1, set2: JUEnumSet): JUEnumSet; compilerproc;
    var
      tmp: JUEnumSet;
    begin
      { (set1 + set 2) - (set1 * set2) }
      result:=JUEnumSet(set1.clone);
      result.addAll(set2);
      tmp:=JUEnumSet(set1.clone);
      tmp.retainAll(set2);
      result.removeAll(tmp);
    end;


  function fpc_bitset_from_string(const s: unicodestring): FpcBitSet; compilerproc;
    var
      i, bits: longint;
      wc: word;
    begin
      { all bits are encoded in the string characters }
      result:=FpcBitSet.Create(cardinal(length(s)+15) div 16);
      for i:=1 to length(s) do
        begin
          wc:=word(s[i]);
          if wc=0 then
            continue;
          for bits:=15 downto 0 do
            if (wc and (1 shl bits)) <> 0 then
              result.&set((i-1)*16+15-bits);
        end;
    end;


  function fpc_enumset_from_string(dummy: FpcEnumValueObtainable; const s: unicodestring): JUEnumSet; compilerproc;
    var
      i, bits: longint;
      wc: word;
    begin
      { all bits are encoded in the string characters }
      result:=JUEnumSet.noneOf(JLObject(dummy).getClass);
      for i:=1 to length(s) do
        begin
          wc:=word(s[i]);
          if wc=0 then
            continue;
          for bits:=15 downto 0 do
            if (wc and (1 shl bits)) <> 0 then
              result.add(dummy.fpcGenericValueOf((i-1)*16+15-bits));
        end;
    end;


  function fpc_enumset_to_int(const s: JUEnumSet; setbase, setsize: longint): jint; compilerproc;
    var
      it: JUIterator;
      ele: FpcEnumValueObtainable;
      val: longint;
    begin
      it:=s.iterator;
      result:=0;
      setsize:=setsize*8;
      while it.hasNext do
        begin
          ele:=FpcEnumValueObtainable(it.next);
          val:=ele.fpcOrdinal-setbase;
          result:=result or (1 shl (setsize-val));
        end;
    end;


  function fpc_enumset_to_long(const s: JUEnumSet; setbase, setsize: longint): jlong; compilerproc;
    var
      it: JUIterator;
      ele: FpcEnumValueObtainable;
      val: longint;
    begin
      it:=s.iterator;
      result:=0;
      setsize:=setsize*8;
      while it.hasNext do
        begin
          ele:=FpcEnumValueObtainable(it.next);
          val:=ele.fpcOrdinal-setbase;
          result:=result or (1 shl (setsize-val));
        end;
    end;


  function fpc_bitset_to_int(const s: FpcBitSet; setbase, setsize: longint): jint; compilerproc;
    var
      i, val: longint;
    begin
      result:=0;
      setsize:=setsize*8;
      i:=s.nextSetBit(0);
      while i>=0 do
        begin
          val:=i-setbase;
          result:=result or (1 shl (setsize-val));
          i:=s.nextSetBit(i+1);
        end;
    end;


  function fpc_bitset_to_long(const s: FpcBitSet; setbase, setsize: longint): jlong; compilerproc;
    var
      i, val: longint;
    begin
      result:=0;
      setsize:=setsize*8;
      i:=s.nextSetBit(0);
      while i>=0 do
        begin
          val:=i-setbase;
          result:=result or (1 shl (setsize-val));
          i:=s.nextSetBit(i+1);
        end;
    end;


  function fpc_int_to_bitset(const val: jint; setbase, setsize: jint): FpcBitSet; compilerproc;
    var
      i, setval: jint;
    begin
      result:=FpcBitSet.create;
      if val<>0 then
        begin
          setsize:=setsize*8;
          for i:=0 to setsize-1 do
            if (val and (jint(1) shl (setsize-i)))<>0 then
              result.&set(i+setbase);
        end;
    end;


function fpc_long_to_bitset(const val: jint; setbase, setsize: jint): FpcBitSet; compilerproc;
  var
    i, setval: jint;
  begin
    result:=FpcBitSet.create;
    if val<>0 then
      begin
        setsize:=setsize*8;
        for i:=0 to setsize-1 do
          if (val and (jlong(1) shl (setsize-i)))<>0 then
            result.&set(i+setbase);
      end;
  end;


  function fpc_enumset_to_bitset(const val: JUEnumSet; fromsetbase, tosetbase: jint): FpcBitSet; compilerproc;
    var
      it: JUIterator;
      ele: FpcEnumValueObtainable;
      i: longint;
    begin
      result:=FpcBitSet.Create;
      it:=val.iterator;
      while it.hasNext do
        begin
          ele:=FpcEnumValueObtainable(it.next);
          i:=ele.fpcOrdinal-fromsetbase;
          result.&set(i+tosetbase);
        end;
    end;


  function fpc_bitset_to_bitset(const s: FpcBitSet; fromsetbase, tosetbase: jint): FpcBitSet; compilerproc;
    var
      i, val: longint;
    begin
      result:=FpcBitSet.create;
      i:=s.nextSetBit(0);
      while i>=0 do
        begin
          val:=i-fromsetbase;
          result.&set(val+tosetbase);
          i:=s.nextSetBit(i+1);
        end;
    end;


