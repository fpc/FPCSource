(*
 * Copyright 2006-2008 The FLWOR Foundation.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *)

{$mode objfpc}{$H+}
{$PACKRECORDS C}
{$MACRO ON}

interface

uses
  Sysutils,ctypes, dynlibs, xqc;

{$IFDEF UNIX}
  {$DEFINE extdecl:=cdecl}
  const
    zorbalib = 'libzorba_simplestore.'+sharedsuffix;
    zorbavlib = zorbalib+'.0.9.9';
{$ENDIF}
{$IFDEF WINDOWS}
  {$DEFINE extdecl:=stdcall}
  const
    zorbalib = 'zorba_simplestore.dll';
    zorbavlib = zorbalib;
{$ENDIF}

{$IFDEF LOAD_DYNAMICALLY}
  {$DEFINE D}
{$ELSE}
  {$DEFINE S}
{$ENDIF}

{.$i zorba_config.inc}
{$i zorba_options.inc}


(**
 * The zorba_implementation function creates a new ::XQC_Implementation object.
 * Thereby, the Zorba processor is initialized.
 * The user is responsible for freeing the object by calling the free() function
 * of the XQC_Implementation struct.
 *
 * \param store A pointer to the store that is being used by the Zorba instance that is created
 *              by this call.
 * \param[out] impl The newly created XQC_Implementation object.
 *
 * \retval ::XQC_NO_ERROR
 * \retval ::XQP0019_INTERNAL_ERROR
 *)
{$IFDEF S}function{$ELSE}var{$ENDIF}zorba_implementation{$IFDEF D}: function{$ENDIF}(out impl: XQC_Implementation; store: Pointer): XQUERY_ERROR; extdecl;{$IFDEF S}external zorbalib;{$ENDIF}


(* simplestorec.h *)
{$IFDEF S}function{$ELSE}var{$ENDIF}create_simple_store{$IFDEF D}: function{$ENDIF}: Pointer; extdecl;{$IFDEF S}external zorbalib;{$ENDIF}
{$IFDEF S}procedure{$ELSE}var{$ENDIF}shutdown_simple_store{$IFDEF D}: procedure{$ENDIF}(store: Pointer); extdecl;{$IFDEF S}external zorbalib;{$ENDIF}



{$IFDEF LOAD_DYNAMICALLY}
function InitializeZorba(const LibraryName: String = ''): Integer;
function TryInitializeZorba(const LibraryName: string = ''): Integer;
function ReleaseZorba: Integer;

var
  ZorbaLibraryHandle: TLibHandle;
{$ENDIF LOAD_DYNAMICALLY}

implementation

{$IFDEF LOAD_DYNAMICALLY}

ResourceString
  SErrDefaultsFailed = 'Can not load default Zorba clients ("%s" or "%s"). Check your installation.';
  SErrLoadFailed     = 'Can not load Zorba client library "%s". Check your installation.';
  SErrAlreadyLoaded  = 'Zorba interface already initialized from library %s.';

var
  RefCount : integer;
  LoadedLibrary : String;

Function TryInitializeZorba(Const LibraryName : String) : integer;

begin
  Result := 0;
  if (RefCount=0) then
    begin
    ZorbaLibraryHandle:=LoadLibrary(LibraryName);
    if (ZorbaLibraryHandle=nilhandle) then
      Exit;
    inc(RefCount);
    LoadedLibrary:=LibraryName;    
    pointer(create_simple_store)   :=GetProcedureAddress(ZorbaLibraryHandle,'create_simple_store');
    pointer(shutdown_simple_store) :=GetProcedureAddress(ZorbaLibraryHandle,'shutdown_simple_store');
    pointer(zorba_implementation)  :=GetProcedureAddress(ZorbaLibraryHandle,'zorba_implementation');
    pointer(Zorba_CompilerHints_default)    :=GetProcedureAddress(ZorbaLibraryHandle,'Zorba_CompilerHints_default');
    pointer(Zorba_SerializerOptions_default):=GetProcedureAddress(ZorbaLibraryHandle,'Zorba_SerializerOptions_default');
    pointer(Zorba_SerializerOptions_free)   :=GetProcedureAddress(ZorbaLibraryHandle,'Zorba_SerializerOptions_free');
    pointer(Zorba_SerializerOptions_set)    :=GetProcedureAddress(ZorbaLibraryHandle,'Zorba_SerializerOptions_set');
   end
  else
    inc(RefCount);
  Result := RefCount;
end;

function InitializeZorba(const LibraryName: String): Integer;
begin
  Result := TryInitializeZorba( LibraryName);
  If Result = 0 then
    Raise EInOutError.CreateFmt(SErrLoadFailed,[LibraryName])
  else If (LibraryName<>LoadedLibrary) then
    begin
      Dec(RefCount);
      Result := RefCount;
      Raise EInOUtError.CreateFmt(SErrAlreadyLoaded,[LoadedLibrary]);
    end;
end;

function ReleaseZorba: Integer;
begin
 if RefCount>1 then
    Dec(RefCount)
  else if UnloadLibrary(ZorbaLibraryHandle) then
    begin
      Dec(RefCount);
      ZorbaLibraryHandle := NilHandle;
      LoadedLibrary:='';
    end;
end;

{$ENDIF}
end.
