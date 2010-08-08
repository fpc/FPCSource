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
  ctypes, dynlibs, xqc;

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
  ZorbaLibrary: TLibHandler;
{$ENDIF LOAD_DYNAMICALLY}

implementation

{$IFDEF LOAD_DYNAMICALLY}
const
  zorba_symbols: array[0..6] of TLibSymbol = (
    (pvar:@create_simple_store; name:'create_simple_store'; weak:false),
    (pvar:@shutdown_simple_store; name:'shutdown_simple_store'; weak:false),
    (pvar:@zorba_implementation; name:'zorba_implementation'; weak:false),
    (pvar:@Zorba_CompilerHints_default; name:'Zorba_CompilerHints_default'; weak:false),
    (pvar:@Zorba_SerializerOptions_default; name:'Zorba_SerializerOptions_default'; weak:false),
    (pvar:@Zorba_SerializerOptions_free; name:'Zorba_SerializerOptions_free'; weak:false),
    (pvar:@Zorba_SerializerOptions_set; name:'Zorba_SerializerOptions_set'; weak:false)
  );

function TryInitializeZorba(const LibraryName: string): Integer;
begin
  Result := TryInitializeLibrary(ZorbaLibrary, LibraryName);
end;

function InitializeZorba(const LibraryName: String): Integer;
begin
  Result := InitializeLibrary(ZorbaLibrary, LibraryName);
end;

function ReleaseZorba: Integer;
begin
  Result := ReleaseLibrary(ZorbaLibrary);
end;

initialization
  ZorbaLibrary := LibraryHandler('zorba', [zorbalib,zorbavlib], @zorba_symbols, Length(zorba_symbols));
{$ENDIF}

end.
