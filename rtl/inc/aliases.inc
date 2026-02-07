{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2000 by Florian Klaempfl
    member of the Free Pascal development team.

    This file contains external definitions (which map to aliases
    of functions which are later implemented) so that they can
    be called before their implementation is known. We can't use
    forward definitions, because there's a bug which causes all
    sorts of trouble if you you first declare a procedure as
    forward, then call it and then implement it using an
    "external name 'bla'" where 'bla' is a public alias of a
    procedure defined after the call to the forward defined
    procedure.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
}

{ export for internal usage }
Procedure int_Finalize (Data,TypeInfo: Pointer); [external name 'FPC_FINALIZE'];
Procedure int_Addref (Data,TypeInfo : Pointer); [external name 'FPC_ADDREF'];
Procedure int_Initialize (Data,TypeInfo: Pointer); [external name 'FPC_INITIALIZE'];
procedure int_InitializeArray(data,typeinfo : pointer;count : SizeInt); [external name 'FPC_INITIALIZE_ARRAY'];
procedure int_FinalizeArray(data,typeinfo : pointer;count : SizeInt); [external name 'FPC_FINALIZE_ARRAY'];
procedure int_AddRefArray(data,typeinfo : pointer;count : SizeInt); [external name 'FPC_ADDREF_ARRAY'];
Function int_Copy(Src, Dest, TypeInfo : Pointer) : SizeInt;[external name 'FPC_COPY'];
{$ifdef FPC_MANAGED_MOVE}
Function int_Copy_with_move_semantics(Src, Dest, TypeInfo : Pointer) : SizeInt;[external name 'FPC_COPY_WITH_MOVE_SEMANTICS'];
{$endif}

{$if defined(FPC_HAS_FEATURE_RTTI) and not defined(cpujvm)}
procedure FinalizeRecordFields(Data:Pointer;Ri:PRecordInfoInit); forward;
{$endif FPC_HAS_FEATURE_RTTI and not cpujvm}
