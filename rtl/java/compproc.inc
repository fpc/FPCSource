{
    This file is part of the Free Pascal Run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    This file contains the declarations of internal compiler helper
    routines. That means you can *NOT* call these directly, as they may
    be changed or even removed at any time. The only reason they are
    included in the interface of the system unit, is so that the
    compiler doesn't need special code to access their parameter
    list information etc.

    Note that due to the "compilerproc" directive, it isn't even possible
    to use these routines in your programs.

    See the File COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$ifdef FPC_HAS_FEATURE_RTTI}
Procedure fpc_Initialize (Data,TypeInfo : pointer); compilerproc;
Procedure fpc_finalize (Data,TypeInfo: Pointer); compilerproc;
Procedure fpc_Addref (Data,TypeInfo : Pointer); compilerproc;
Procedure fpc_DecRef (Data,TypeInfo : Pointer);  compilerproc;
procedure fpc_initialize_array(data,typeinfo : pointer;count : SizeInt); compilerproc;
procedure fpc_finalize_array(data,typeinfo : pointer;count : SizeInt); compilerproc;
procedure fpc_addref_array(data,typeinfo: pointer; count: SizeInt); compilerproc;
procedure fpc_decref_array(data,typeinfo: pointer; count: sizeint); compilerproc;
Function fpc_Copy (Src, Dest, TypeInfo : Pointer) : SizeInt; compilerproc;
Procedure fpc_Copy_proc (Src, Dest, TypeInfo : Pointer); compilerproc; inline;
{$endif FPC_HAS_FEATURE_RTTI}
{ normalarrdim contains the number of dimensions
  a regular array, if any, that contains these unicodestrings. E.g.:
   type
     tarr = array[1..10,2..9] of unicodestring;

   procedure test(out arr: array of tarr);
    -> normalarrdim will be 2

  Initialises with nil rather than with empty arrays, because there does not
  appear to be a generic way to pass an iniitialised (empty) array object and
  then clone it for every array position, except for slow serialization (array
  instances are clonable in Java, but they don't inherit from a base class other
  than java.lang.Object (in which clone is protected) and they only implement
  the formal interfaces Clonable and Serializeable (which don't expose any
  particular methods). This means that we cannot cast arrays to a generic class
  type that supports cloning (except if we add support for calling methods on
  dynamic array types, and add an extra parameter to determine the first
  level elements types of the array) }
procedure fpc_initialize_array_dynarr(arr: TJObjectArray; normalarrdim: longint);compilerproc;
procedure fpc_initialize_array_record(arr: TJObjectArray; normalarrdim: longint; inst: FpcBaseRecordType);compilerproc;

