unit xresource;
interface
uses
  x,xlib;

{$LinkLib c}
{$LinkLib X11}

{
  Automatically converted by H2Pas 0.99.15 from xresource.h
  The following command line parameters were used:
    -p
    -T
    -S
    -d
    -c
    xresource.h
}

{$PACKRECORDS C}


function Xpermalloc(para1:dword):Pchar;cdecl;external;
type

   PXrmQuark = ^TXrmQuark;
   TXrmQuark = longint;
   TXrmQuarkList = PXrmQuark;
   PXrmQuarkList = ^TXrmQuarkList;
function NULLQUARK : TXrmQuark;

type

   PXrmString = ^TXrmString;
   TXrmString = char;
function NULLSTRING : TXrmString;

function XrmStringToQuark(para1:Pchar):TXrmQuark;cdecl;external;
function XrmPermStringToQuark(para1:Pchar):TXrmQuark;cdecl;external;
function XrmQuarkToString(para1:TXrmQuark):TXrmString;cdecl;external;
function XrmUniqueQuark:TXrmQuark;cdecl;external;
{$ifdef MACROS}
function XrmStringsEqual(a1,a2 : longint) : longint;
{$endif MACROS}

type

   PXrmBinding = ^TXrmBinding;
   TXrmBinding = (XrmBindTightly,XrmBindLoosely);
   TXrmBindingList = PXrmBinding;
   PXrmBindingList = ^TXrmBindingList;

procedure XrmStringToQuarkList(para1:Pchar; para2:TXrmQuarkList);cdecl;external;
procedure XrmStringToBindingQuarkList(para1:Pchar; para2:TXrmBindingList; para3:TXrmQuarkList);cdecl;external;
type

   PXrmName = ^TXrmName;
   TXrmName = TXrmQuark;

   PXrmNameList = ^TXrmNameList;
   TXrmNameList = TXrmQuarkList;
{$ifdef MACROS}
function XrmNameToString(name : longint) : longint;

function XrmStringToName(_string : longint) : longint;

function XrmStringToNameList(str,name : longint) : longint;
{$endif MACROS}

type

   PXrmClass = ^TXrmClass;
   TXrmClass = TXrmQuark;

   PXrmClassList = ^TXrmClassList;
   TXrmClassList = TXrmQuarkList;
{$ifdef MACROS}
function XrmClassToString(c_class : longint) : longint;

function XrmStringToClass(c_class : longint) : longint;

function XrmStringToClassList(str,c_class : longint) : longint;
{$endif MACROS}
type

   PXrmRepresentation = ^TXrmRepresentation;
   TXrmRepresentation = TXrmQuark;
{$ifdef MACROS}
function XrmStringToRepresentation(_string : longint) : longint;

function XrmRepresentationToString(_type : longint) : longint;
{$endif MACROS}

type

   PXrmValue = ^TXrmValue;
   TXrmValue = record
        size : dword;
        addr : TXPointer;
     end;
   TXrmValuePtr = PXrmValue;
   PXrmValuePtr = ^TXrmValuePtr;

   PXrmHashBucketRec = ^TXrmHashBucketRec;
   TXrmHashBucketRec = record
     end;
   TXrmHashBucket = PXrmHashBucketRec;
   PXrmHashBucket = ^TXrmHashBucket;

   PXrmHashTable = ^TXrmHashTable;
   TXrmHashTable = TXrmHashBucket;

   TXrmDatabase = PXrmHashBucketRec;
   PXrmDatabase = ^TXrmDatabase;

procedure XrmDestroyDatabase(para1:TXrmDatabase);cdecl;external;
procedure XrmQPutResource(para1:PXrmDatabase; para2:TXrmBindingList; para3:TXrmQuarkList; para4:TXrmRepresentation; para5:PXrmValue);cdecl;external;
procedure XrmPutResource(para1:PXrmDatabase; para2:Pchar; para3:Pchar; para4:PXrmValue);cdecl;external;
procedure XrmQPutStringResource(para1:PXrmDatabase; para2:TXrmBindingList; para3:TXrmQuarkList; para4:Pchar);cdecl;external;
procedure XrmPutStringResource(para1:PXrmDatabase; para2:Pchar; para3:Pchar);cdecl;external;
procedure XrmPutLineResource(para1:PXrmDatabase; para2:Pchar);cdecl;external;
function XrmQGetResource(para1:TXrmDatabase; para2:TXrmNameList; para3:TXrmClassList; para4:PXrmRepresentation; para5:PXrmValue):TBool;cdecl;external;
function XrmGetResource(para1:TXrmDatabase; para2:Pchar; para3:Pchar; para4:PPchar; para5:PXrmValue):TBool;cdecl;external;
{ There is no definition of TXrmSearchList }
{function XrmQGetSearchList(para1:TXrmDatabase; para2:TXrmNameList; para3:TXrmClassList; para4:TXrmSearchList; para5:longint):TBool;cdecl;external;
function XrmQGetSearchResource(para1:TXrmSearchList; para2:TXrmName; para3:TXrmClass; para4:PXrmRepresentation; para5:PXrmValue):TBool;cdecl;external;}
procedure XrmSetDatabase(para1:PDisplay; para2:TXrmDatabase);cdecl;external;
function XrmGetDatabase(para1:PDisplay):TXrmDatabase;cdecl;external;
function XrmGetFileDatabase(para1:Pchar):TXrmDatabase;cdecl;external;
function XrmCombineFileDatabase(para1:Pchar; para2:PXrmDatabase; para3:TBool):TStatus;cdecl;external;
function XrmGetStringDatabase(para1:Pchar):TXrmDatabase;cdecl;external;
procedure XrmPutFileDatabase(para1:TXrmDatabase; para2:Pchar);cdecl;external;
procedure XrmMergeDatabases(para1:TXrmDatabase; para2:PXrmDatabase);cdecl;external;
procedure XrmCombineDatabase(para1:TXrmDatabase; para2:PXrmDatabase; para3:TBool);cdecl;external;

const
   XrmEnumAllLevels = 0;
   XrmEnumOneLevel = 1;
type
  funcbool=function :TBool;
function XrmEnumerateDatabase(para1:TXrmDatabase; para2:TXrmNameList; para3:TXrmClassList; para4:longint; para5:funcbool;
           para6:TXPointer):TBool;cdecl;external;
function XrmLocaleOfDatabase(para1:TXrmDatabase):Pchar;cdecl;external;
type

   PXrmOptionKind = ^TXrmOptionKind;
   TXrmOptionKind = (XrmoptionNoArg,XrmoptionIsArg,XrmoptionStickyArg,
     XrmoptionSepArg,XrmoptionResArg,XrmoptionSkipArg,
     XrmoptionSkipLine,XrmoptionSkipNArgs);

   PXrmOptionDescRec = ^TXrmOptionDescRec;
   TXrmOptionDescRec = record
        option : Pchar;
        specifier : Pchar;
        argKind : TXrmOptionKind;
        value : TXPointer;
     end;
   TXrmOptionDescList = PXrmOptionDescRec;
   PXrmOptionDescList = ^TXrmOptionDescList;

procedure XrmParseCommand(para1:PXrmDatabase; para2:TXrmOptionDescList; para3:longint; para4:Pchar; para5:Plongint;
            para6:PPchar);cdecl;external;

implementation

function NULLQUARK : TXrmQuark;
  begin
     NULLQUARK:=TXrmQuark(0);
  end;

function NULLSTRING : TXrmString;
  begin
     NULLSTRING:=TXrmString(0);
  end;

{$ifdef MACROS}
function XrmStringsEqual(a1,a2 : longint) : longint;
begin
   XrmStringsEqual:=(strcmp(a1,a2)) = 0;
end;

function XrmNameToString(name : longint) : longint;
begin
   XrmNameToString:=XrmQuarkToString(name);
end;

function XrmStringToName(string : longint) : longint;
begin
   XrmStringToName:=XrmStringToQuark(_string);
end;

function XrmStringToNameList(str,name : longint) : longint;
begin
   XrmStringToNameList:=XrmStringToQuarkList(str,name);
end;

function XrmClassToString(c_class : longint) : longint;
begin
   XrmClassToString:=XrmQuarkToString(c_class);
end;

function XrmStringToClass(c_class : longint) : longint;
begin
   XrmStringToClass:=XrmStringToQuark(c_class);
end;

function XrmStringToClassList(str,c_class : longint) : longint;
begin
   XrmStringToClassList:=XrmStringToQuarkList(str,c_class);
end;

function XrmStringToRepresentation(_string : longint) : longint;
begin
   XrmStringToRepresentation:=XrmStringToQuark(_string);
end;

function XrmRepresentationToString(_type : longint) : longint;
begin
   XrmRepresentationToString:=XrmQuarkToString(_type);
end;
{$endif MACROS}

end.
