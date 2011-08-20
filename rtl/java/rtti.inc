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

