{
  $Id$
}
unit xresource;
interface

uses
  xlib;

{$LinkLib C}
{$LinkLib X11}

{***************************************************************
 * Quark Management
 ***************************************************************}

type
  TXrmQuark     = Longint;
  PXrmQuark     = ^TXrmQuark;
  TXrmQuarkList = PXrmQuark;
  TXrmString    = Pchar;

Const
  NULLQUARK = 0 ;
  NULLSTRING = 0;

{ find quark for string, create new quark if none already exists }
Function  XrmStringToQuark( p1 : pchar ) : TXrmQuark;cdecl;external;
Function  XrmPermStringToQuark( p1 : pchar ) : TXrmQuark;cdecl;external;
{ find string for quark }
Function  XrmQuarkToString( p1 : TXrmQuark) : TXrmString;cdecl;external;
Function  XrmUniqueQuark : TXrmQuark;cdecl;external;

{
#define XrmStringsEqual(a1, a2) (strcmp(a1, a2) == 0)
}


{***************************************************************
 * Conversion of Strings to Lists
 ***************************************************************}

type
  TXrmBinding = longint;
  TXrmBindingList = ^longint;
Const
  XrmBindTightly = 0;
  XrmBindLoosely = 1;

procedure XrmStringToQuarkList(
    p1 : pchar  { string };
    p2 : TXrmQuarkList  { quarks_return }
);cdecl;external;

procedure XrmStringToBindingQuarkList(
    p1 : pchar  { string };
    p2 : TXrmBindingList        { bindings_return };
    p3 : TXrmQuarkList  { quarks_return }
);cdecl;external;


{***************************************************************
 * Name and Class lists.
 ***************************************************************}

type
  TXrmName = TXrmQuark;
  TXrmNameList = TXrmQuarkList ;
{
#define XrmNameToString(name)           XrmQuarkToString(name)
#define XrmStringToName(string)         XrmStringToQuark(string)
#define XrmStringToNameList(str, name)  XrmStringToQuarkList(str, name)
}
 TXrmClass = TXrmQuark;
 TXrmClassList = TXrmQuarkList;
{
#define XrmClassToString(c_class)       XrmQuarkToString(c_class)
#define XrmStringToClass(c_class)       XrmStringToQuark(c_class)
#define XrmStringToClassList(str,c_class) XrmStringToQuarkList(str, c_class)
}


{***************************************************************
 * Resource Representation Types and Values
 ***************************************************************}

type
  TXrmRepresentation = TXrmQuark    ;
  PXrmRepresentation = ^TXrmRepresentation;
{
#define XrmStringToRepresentation(string)   XrmStringToQuark(string)
#define XrmRepresentationToString(type)   XrmQuarkToString(type)
}

type
  TXrmValue = record
    size : word;
    theaddr : TXPointer;
  end;
  PXrmValue = ^TXrmValue;
  TXrmValuePtr = PXrmValue;


{***************************************************************
 * Resource Manager Functions
 ***************************************************************}
{
typedef struct _XrmHashBucketRec *XrmHashBucket;
typedef XrmHashBucket *XrmHashTable;
typedef XrmHashTable XrmSearchList[];
typedef struct _XrmHashBucketRec *XrmDatabase;
}
Type
  TXrmHashBucket = pointer;
  TXrmHashTable = ^TXrmHashBucket;
  TXrmSearchList = ^TXrmHashTable;
  TXrmDatabase = pointer;
  PXrmDatabase = ^TXrmDatabase;

procedure XrmDestroyDatabase(
    p1 : TXrmDatabase           { database }
);cdecl;external;

Procedure XrmQPutResource(
    p1 :  PXrmDatabase  { database };
    p2 : TXrmBindingList        { bindings };
    p3 : TXrmQuarkList  { quarks };
    p4 : TXrmRepresentation     { type };
    p5 : PXrmValue              { value }
);cdecl;external;

Procedure  XrmPutResource(
    p1 : PXrmDatabase   { database };
    p2 : pchar  { specifier };
    p3 : pchar  { type };
    p4 : PXrmValue              { value }
);cdecl;external;

Procedure  XrmQPutStringResource(
    p1 : PXrmDatabase   { database };
    p2 : TXrmBindingList      { bindings };
    p3 : TXrmQuarkList  { quarks };
    p4 : Pchar  { value }
);cdecl;external;

Procedure XrmPutStringResource(
    P1 : PXrmDatabase   { database };
    p2,p3 : pchar
);cdecl;external;

Procedure XrmPutLineResource(
    p1 : PXrmDatabase   { database };
    p2 : pchar  { line }
);cdecl;external;

Function XrmQGetResource(
    p1 : PXrmDatabase           { database };
    p2 : TXrmNameList           { quark_name };
    p3 : TXrmClassList  { quark_class };
    p4 : PXrmRepresentation     { quark_type_return };
    p5 : PXrmValue              { value_return }
) : Integer;cdecl;external;

Function XrmGetResource(
    p1 : TXrmDatabase           { database };
    p2 : pchar  { str_name };
    p3 : pchar  { str_class };
    p4 : ppchar         { str_type_return };
    p5 : PXrmValue              { value_return }
) : Tbool;cdecl;external;

Function XrmQGetSearchList(
    p1 : TXrmDatabase           { database };
    p2 : TXrmNameList           { names };
    p3 : TXrmClassList  { classes };
    p4 : TXrmSearchList { list_return };
    p5 : integer                        { list_length }
) : Tbool;cdecl;external;

Function XrmQGetSearchResource(
    P1 : TXrmSearchList { list };
    p2 : TXrmName               { name };
    p3 : TXrmClass              { class };
    p4 : PXrmRepresentation     { type_return };
    p5 : PXrmValue              { value_return }
) : TBool;cdecl;external;

{***************************************************************
 *
 * Resource Database Management
 *
 ***************************************************************}

procedure XrmSetDatabase(
    p1 : PDisplay               { display };
    p2 : TXrmDatabase           { database }
);cdecl;external;

Function  XrmGetDatabase(
    p1 : PDisplay               { display }
) : TXrmDatabase;cdecl;external;

Function XrmGetFileDatabase(
    p1 : pchar  { filename }
) : TXrmDatabase ;cdecl;external;

Function XrmCombineFileDatabase(
    p1 : pchar  { filename };
    p2 : PXrmDatabase   { target };
    p3 : TBool          { override }
) : TStatus ;cdecl;external;

function XrmGetStringDatabase(
    p1 : pchar  { data }  {  null terminated string }
) : TXrmDatabase ;cdecl;external;

Procedure XrmPutFileDatabase(
    p1 : TXrmDatabase           { database };
    p2: pchar   { filename }
);cdecl;external;

Procedure XrmMergeDatabases(
    p1 : TXrmDatabase           { source_db };
    p2 : PXrmDatabase   { target_db }
);cdecl;external;

procedure XrmCombineDatabase(
    p1 : TXrmDatabase           { source_db };
    p2 : PXrmDatabase   { target_db };
    p3 : TBool          { override }
);cdecl;external;

const
 XrmEnumAllLevels = 0;
 XrmEnumOneLevel  = 1;
(*
Function XrmEnumerateDatabase(
    XrmDatabase         { db };
    XrmNameList         { name_prefix };
    XrmClassList        { class_prefix };
    int                 { mode };
    Bool *(#if NeedNestedPrototypes
             XrmDatabase*       { db };
             XrmBindingList     { bindings };
             XrmQuarkList       { quarks };
             XrmRepresentation* { type };
             XrmValue*          { value };
             XPointer           { closure }

             )          { proc };
    XPointer            { closure }
) : TBool;
*)

Function XrmLocaleOfDatabase(
    P1 : TXrmDatabase   { database }
) : Pchar;cdecl;external;


{***************************************************************
 *
 * Command line option mapping to resource entries
 *
 ***************************************************************}

Const
    XrmoptionNoArg = 0; { Value is specified in OptionDescRec.value         }
    XrmoptionIsArg = 1;     { Value is the option string itself             }
    XrmoptionStickyArg = 2; { Value is characters immediately following option }
    XrmoptionSepArg = 3;    { Value is next argument in argv                }
    XrmoptionResArg = 4;        { Resource and value in next argument in argv      }
    XrmoptionSkipArg = 5;   { Ignore this option and the next argument in argv }
    XrmoptionSkipLine = 6;  { Ignore this option and the rest of argv       }
    XrmoptionSkipNArgs = 7;     { Ignore this option and the next
                           OptionDescRes.value arguments in argv }
Type TXrmOptionKind = Integer;

TXrmOptionDescRec = record
    option : pchar;         { Option abbreviation in argv           }
    specifier : pchar;     { Resource specifier             }
    argKind : TXrmOptionKind;       { Which style of option it is           }
    value : TXpointer;      { Value to provide if XrmoptionNoArg   }
end;
PXrmOptionDescRec= ^TXrmOptionDescRec;
TXrmOptionDescList = PXrmOptionDescRec;


Procedure XrmParseCommand(
    p1 : TXrmDatabase   { database };
    p2 : TXrmOptionDescList     { table };
    p3 : integer                        { table_count };
    p4 : pchar  { name };
    p5 : Pointer                { argc_in_out };
    p6 : ppchar         { argv_in_out }
);cdecl;external;

Implementation

end.
{
  $Log$
  Revision 1.3  2000-02-27 14:39:54  peter
    * added explicit linklib c

  Revision 1.2  2000/02/27 13:11:31  peter
    * cleanup, removed warnings
    * external decls moved from implementation to interface

}
