{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2011 by Jonas Maebe
    member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

procedure fpc_initialize_array_jstring_intern(arr: TJObjectArray; normalarrdim: longint); external name 'fpc_initialize_array_unicodestring';

procedure fpc_initialize_array_unicodestring(arr: TJObjectArray; normalarrdim: longint);compilerproc;
  var
    i: longint;
  begin
    if normalarrdim > 0 then
      begin
        for i:=low(arr) to high(arr) do
          fpc_initialize_array_jstring_intern(TJObjectArray(arr[i]),normalarrdim-1);
      end
    else
      begin
        for i:=low(arr) to high(arr) do
          unicodestring(arr[i]):='';
      end;
  end;


procedure fpc_initialize_array_ansistring_intern(arr: TJObjectArray; normalarrdim: longint); external name 'fpc_initialize_array_ansistring';

procedure fpc_initialize_array_ansistring(arr: TJObjectArray; normalarrdim: longint);compilerproc;
  var
    i: longint;
  begin
    if normalarrdim > 0 then
      begin
        for i:=low(arr) to high(arr) do
          fpc_initialize_array_ansistring_intern(TJObjectArray(arr[i]),normalarrdim-1);
      end
    else
      begin
        for i:=low(arr) to high(arr) do
          ansistring(arr[i]):='';
      end;
  end;


procedure fpc_initialize_array_dynarr_intern(arr: TJObjectArray; normalarrdim: longint); external name 'fpc_initialize_array_dynarr';

procedure fpc_initialize_array_dynarr(arr: TJObjectArray; normalarrdim: longint);compilerproc;
  var
    i: longint;
  begin
    if normalarrdim > 0 then
      begin
        for i:=low(arr) to high(arr) do
          fpc_initialize_array_dynarr_intern(TJObjectArray(arr[i]),normalarrdim-1);
      end
    else
      begin
        for i:=low(arr) to high(arr) do
          arr[i]:=nil;
      end;
  end;

procedure fpc_initialize_array_record_intern(arr: TJObjectArray; normalarrdim: longint; inst: FpcBaseRecordType); external name 'fpc_initialize_array_record';

procedure fpc_initialize_array_record(arr: TJObjectArray; normalarrdim: longint; inst: FpcBaseRecordType);compilerproc;
  var
    i: longint;
  begin
    if normalarrdim > 0 then
      begin
        for i:=low(arr) to high(arr) do
          fpc_initialize_array_record_intern(TJObjectArray(arr[i]),normalarrdim-1,inst);
      end
    else
      begin
        for i:=low(arr) to high(arr) do
          arr[i]:=inst.clone;
      end;
  end;


procedure fpc_initialize_array_procvar_intern(arr: TJObjectArray; normalarrdim: longint; inst: FpcBaseProcVarType); external name 'fpc_initialize_array_procvar';

procedure fpc_initialize_array_procvar(arr: TJObjectArray; normalarrdim: longint; inst: FpcBaseProcVarType);compilerproc;
  var
    i: longint;
  begin
    if normalarrdim > 0 then
      begin
        for i:=low(arr) to high(arr) do
          fpc_initialize_array_procvar_intern(TJObjectArray(arr[i]),normalarrdim-1,inst);
      end
    else
      begin
        for i:=low(arr) to high(arr) do
          arr[i]:=inst.clone;
      end;
  end;


{ exactly the same as fpc_initialize_array_record, but can't use generic
  routine because of Java clonable design :( (except by rtti/invoke, but that's
  not particularly fast either) }
procedure fpc_initialize_array_bitset_intern(arr: TJObjectArray; normalarrdim: longint; inst: FpcBitSet); external name 'fpc_initialize_array_bitset';

procedure fpc_initialize_array_bitset(arr: TJObjectArray; normalarrdim: longint; inst: FpcBitSet);compilerproc;
  var
    i: longint;
  begin
    if normalarrdim > 0 then
      begin
        for i:=low(arr) to high(arr) do
          fpc_initialize_array_bitset_intern(TJObjectArray(arr[i]),normalarrdim-1,inst);
      end
    else
      begin
        for i:=low(arr) to high(arr) do
          arr[i]:=inst.clone;
      end;
  end;


{ idem }
procedure fpc_initialize_array_enumset_intern(arr: TJObjectArray; normalarrdim: longint; inst: JUEnumSet); external name 'fpc_initialize_array_enumset';

procedure fpc_initialize_array_enumset(arr: TJObjectArray; normalarrdim: longint; inst: JUEnumSet);compilerproc;
  var
    i: longint;
  begin
    if normalarrdim > 0 then
      begin
        for i:=low(arr) to high(arr) do
          fpc_initialize_array_enumset_intern(TJObjectArray(arr[i]),normalarrdim-1,inst);
      end
    else
      begin
        for i:=low(arr) to high(arr) do
          arr[i]:=inst.clone;
      end;
  end;


{ initialize entire array with the same object, without making copies. Used for
  initialization with enum instance }
procedure fpc_initialize_array_object_intern(arr: TJObjectArray; normalarrdim: longint; inst: JLObject); external name 'fpc_initialize_array_object';

procedure fpc_initialize_array_object(arr: TJObjectArray; normalarrdim: longint; inst: JLObject);compilerproc;
  var
    i: longint;
  begin
    if normalarrdim > 0 then
      begin
        for i:=low(arr) to high(arr) do
          fpc_initialize_array_object_intern(TJObjectArray(arr[i]),normalarrdim-1,inst);
      end
    else
      begin
        for i:=low(arr) to high(arr) do
          arr[i]:=inst;
      end;
  end;


procedure fpc_initialize_array_shortstring_intern(arr: TJObjectArray; normalarrdim: longint; maxlen: byte); external name 'fpc_initialize_array_shortstring';

procedure fpc_initialize_array_shortstring(arr: TJObjectArray; normalarrdim: longint; maxlen: byte);compilerproc;
  var
    i: longint;
  begin
    if normalarrdim > 0 then
      begin
        for i:=low(arr) to high(arr) do
          fpc_initialize_array_shortstring_intern(TJObjectArray(arr[i]),normalarrdim-1,maxlen);
      end
    else
      begin
        for i:=low(arr) to high(arr) do
          arr[i]:=ShortstringClass.CreateEmpty(maxlen);
      end;
  end;

