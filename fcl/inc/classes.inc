{
    $Id$
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1998 by Michael Van Canneyt and Florian Klaempfl

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{**********************************************************************
 *       Class implementations are in separate files.                 *
 **********************************************************************}

{
 Include all message strings

 Add a language with IFDEF LANG_NAME
 just befor the final ELSE. This way English will always be the default.
}

{$IFDEF LANG_GERMAN}
{$i constsg.inc}
{$ELSE}
{$i constse.inc}
{$ENDIF}

{ Utility routines }
{$i util.inc}

{ TBits implementation }
{$i bits.inc}

{ TReader implementation }
{$i reader.inc}

{ TWriter implementation }
{$i writer.inc}

{ TFiler implementation }
{$i filer.inc}

{ All streams implementations: }
{ Tstreams THandleStream TFileStream TResourcseStreams TStringStream }
{ TCustomMemoryStream TMemoryStream }
{$i streams.inc}

{ TParser implementation}
{$i parser.inc}

{ TCollection and TCollectionItem implementations }
{$i collect.inc}

{ TList and TThreadList implementations }
{$i lists.inc}

{ TStrings and TStringList implementations }
{$i strings.inc}

{ TThread implementation }
{$i thread.inc}

{ TPersistent implementation }
{$i persist.inc }

{ TComponent implementation }
{$i compon.inc}

{**********************************************************************
 *       Miscellaneous procedures and functions                       *
 **********************************************************************}

{ Point and rectangle constructors }

function Point(AX, AY: Integer): TPoint;

begin
  with Result do
  begin
    X := AX;
    Y := AY;
  end;
end;


function SmallPoint(AX, AY: SmallInt): TSmallPoint;

begin
  with Result do
  begin
    X := AX;
    Y := AY;
  end;
end;


function Rect(ALeft, ATop, ARight, ABottom: Integer): TRect;

begin
  with Result do
  begin
    Left := ALeft;
    Top := ATop;
    Right := ARight;
    Bottom := ABottom;
  end;
end;


function Bounds(ALeft, ATop, AWidth, AHeight: Integer): TRect;

begin
  with Result do
  begin
    Left := ALeft;
    Top := ATop;
    Right := ALeft + AWidth;
    Bottom :=  ATop + AHeight;
  end;
end;



{ Class registration routines }

procedure RegisterClass(AClass: TPersistentClass);

begin
end;


procedure RegisterClasses(AClasses: array of TPersistentClass);

begin
end;


procedure RegisterClassAlias(AClass: TPersistentClass; const Alias: string);

begin
end;


procedure UnRegisterClass(AClass: TPersistentClass);

begin
end;


procedure UnRegisterClasses(AClasses: array of TPersistentClass);

begin
end;


procedure UnRegisterModuleClasses(Module: HMODULE);

begin
end;


function FindClass(const ClassName: string): TPersistentClass;

begin
end;


function GetClass(const ClassName: string): TPersistentClass;

begin
end;



{ Component registration routines }

procedure RegisterComponents(const Page: string;
  ComponentClasses: array of TComponentClass);

begin
end;


procedure RegisterNoIcon(ComponentClasses: array of TComponentClass);

begin
end;


procedure RegisterNonActiveX(ComponentClasses: array of TComponentClass;
  AxRegType: TActiveXRegType);

begin
end;




{ Object filing routines }

procedure RegisterIntegerConsts(IntegerType: Pointer; IdentToInt: TIdentToInt;
  IntToIdent: TIntToIdent);

begin
end;


function IdentToInt(const Ident: string; var Int: Longint; const Map: array of TIdentMapEntry): Boolean;

begin
end;


function IntToIdent(Int: Longint; var Ident: string; const Map: array of TIdentMapEntry): Boolean;

begin
end;


function InitInheritedComponent(Instance: TComponent; RootAncestor: TClass): Boolean;

begin
end;


function InitComponentRes(const ResName: string; Instance: TComponent): Boolean;

begin
end;


function ReadComponentRes(const ResName: string; Instance: TComponent): TComponent;

begin
end;


function ReadComponentResEx(HInstance: THandle; const ResName: string): TComponent;

begin
end;


function ReadComponentResFile(const FileName: string; Instance: TComponent): TComponent;

begin
end;


procedure WriteComponentResFile(const FileName: string; Instance: TComponent);

begin
end;



procedure GlobalFixupReferences;

begin
end;


procedure GetFixupReferenceNames(Root: TComponent; Names: TStrings);

begin
end;


procedure GetFixupInstanceNames(Root: TComponent;
  const ReferenceRootName: string; Names: TStrings);

begin
end;


procedure RedirectFixupReferences(Root: TComponent; const OldRootName,
  NewRootName: string);

begin
end;


procedure RemoveFixupReferences(Root: TComponent; const RootName: string);

begin
end;


procedure RemoveFixups(Instance: TPersistent);

begin
end;



procedure BeginGlobalLoading;

begin
end;


procedure NotifyGlobalLoading;

begin
end;


procedure EndGlobalLoading;

begin
end;



function CollectionsEqual(C1, C2: TCollection): Boolean;

begin
end;



{ Object conversion routines }

procedure ObjectBinaryToText(Input, Output: TStream);

begin
end;


procedure ObjectTextToBinary(Input, Output: TStream);

begin
end;


procedure ObjectResourceToText(Input, Output: TStream);

begin
end;


procedure ObjectTextToResource(Input, Output: TStream);

begin
end;



{ Utility routines }

function LineStart(Buffer, BufPos: PChar): PChar;

begin
end;




{
  $Log$
  Revision 1.7  1998-10-30 14:52:48  michael
  + Added format in interface
  + Some errors in parser fixed, it uses exceptions now
  + Strings now has no more syntax errors.

  Revision 1.6  1998/10/29 12:47:14  michael
  + Implementation of tpoint by WYB

  Revision 1.5  1998/09/23 08:41:55  michael
  Added switch to select language

  Revision 1.4  1998/09/23 07:47:40  michael
  + Some changes by TSE

  Revision 1.3  1998/06/03 15:10:20  michael
  + added include of util.inc

  Revision 1.2  1998/05/04 14:30:11  michael
  * Split file according to Class; implemented dummys for all methods, so unit compiles.

  Revision 1.9  1998/05/04 12:16:01  florian
    + Initial revisions after making a new directory structure

  Revision 1.8  1998/05/04 11:20:13  florian
    + Write* and Read* methods to TStream added
    * small problems solved

  Revision 1.7  1998/05/04 09:39:51  michael
  + Started implementation of TList

  Revision 1.6  1998/05/01 22:17:19  florian
    + TBits implemented
    + TStream partial implemented

  Revision 1.5  1998/05/01 17:53:12  florian
    * now it compiles with FPC

  Revision 1.4  1998/04/28 11:47:00  florian
    * more adaptions to FPC

  Revision 1.3  1998/04/27 12:55:57  florian
    + uses objpas added

  Revision 1.2  1998/04/27 09:09:49  michael
  + Added log at the end

}

{
  $Log$
  Revision 1.7  1998-10-30 14:52:48  michael
  + Added format in interface
  + Some errors in parser fixed, it uses exceptions now
  + Strings now has no more syntax errors.

  Revision 1.6  1998/10/29 12:47:14  michael
  + Implementation of tpoint by WYB

  Revision 1.5  1998/09/23 08:41:55  michael
  Added switch to select language

  Revision 1.4  1998/09/23 07:47:40  michael
  + Some changes by TSE

  Revision 1.3  1998/06/03 15:10:20  michael
  + added include of util.inc

  Revision 1.2  1998/05/04 14:30:11  michael
  * Split file according to Class; implemented dummys for all methods, so unit compiles.

  Revision 1.1  1998/05/04 12:16:01  florian
    + Initial revisions after making a new directory structure

}
