{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2014 by Maciej Izak

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$IF DEFINED(CPU8) OR DEFINED(CPU16)}
{ disable the extended RTTI for the RTL/FCL on these targets; the user can of
  course still enable it for their own classes if needed (and published
  properties and methods are accessible using the legacy RTTI as well) }
{$DEFINE SMALLRTTI}
{$ENDIF}

{$PUSH}
{$MINENUMSIZE 1   this saves a lot of memory }

type
  { If you change one of the following enumeration types you have also to
    change the compiler and unit typeinfo in an appropriate way!
    Also if you add managed types you'll need to update tkManagedTypes in
    rtti.inc }
  TTypeKind = (tkUnknown,tkInteger,tkChar,tkEnumeration,tkFloat,
              tkSet,tkMethod,tkSString,tkLString,tkAString,
              tkWString,tkVariant,tkArray,tkRecord,tkInterface,
              tkClass,tkObject,tkWChar,tkBool,tkInt64,tkQWord,
              tkDynArray,tkInterfaceRaw,tkProcVar,tkUString,tkUChar,
              tkHelper,tkFile,tkClassRef,tkPointer);

  TVisibilityClass = (vcPrivate, vcProtected, vcPublic, vcPublished);
  TVisibilityClasses = set of TVisibilityClass;
 
 {$MINENUMSIZE DEFAULT}
 
const
{$IFNDEF SMALLRTTI}
  DefaultFieldRttiVisibility = [vcPrivate..vcPublished];
  DefaultMethodRttiVisibility = [vcPublic..vcPublished];
  DefaultPropertyRttiVisibility = [vcPublic..vcPublished];
{$ELSE SMALLRTTI}
  DefaultFieldRttiVisibility = [];
  DefaultMethodRttiVisibility = [];
  DefaultPropertyRttiVisibility = [];
{$ENDIF SMALLRTTI}
 

{$POP}

const
  { some Delphi-compatible aliases }
  tkAnsiChar = tkChar;
  tkWideChar = tkWChar;
  tkShortString = tkSString;
  tkAnsiString = tkAString;
  tkWideString = tkWString;
  tkUnicodeString = tkUString;

{$ifdef FPC_HAS_FEATURE_RTTI}
procedure InitializeArray(p, typeInfo: Pointer; count: SizeInt);
procedure FinalizeArray(p, typeInfo: Pointer; count: SizeInt);
procedure CopyArray(dest, source, typeInfo: Pointer; count: SizeInt);
{$endif FPC_HAS_FEATURE_RTTI}


