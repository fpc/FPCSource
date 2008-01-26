unit xresource; {$DEFINE MACRO}
interface
uses
  x,xlib {$IFDEF MACROS}, strings{$ENDIF};

{$ifndef os2}
  {$LinkLib c}
  {$LinkLib X11}
const
  libX11='X11';
{$else}
const
  libX11='X11';
{$endif}

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


function Xpermalloc(para1:dword):Pchar;cdecl;external libX11;
type

   PXrmQuark = ^TXrmQuark;
   TXrmQuark = longint;
   TXrmQuarkList = PXrmQuark;
   PXrmQuarkList = ^TXrmQuarkList;
function NULLQUARK : TXrmQuark;

type

   PXrmString = ^TXrmString;
   TXrmString = ^char;

function NULLSTRING : TXrmString;

function XrmStringToQuark(para1:Pchar):TXrmQuark;cdecl;external libX11;
function XrmPermStringToQuark(para1:Pchar):TXrmQuark;cdecl;external libX11;
function XrmQuarkToString(para1:TXrmQuark):TXrmString;cdecl;external libX11;
function XrmUniqueQuark:TXrmQuark;cdecl;external libX11;
{$ifdef MACROS}
function XrmStringsEqual(a1,a2 : pchar) : boolean;
{$endif MACROS}

type

   PXrmBinding = ^TXrmBinding;
   TXrmBinding = (XrmBindTightly,XrmBindLoosely);
   TXrmBindingList = PXrmBinding;
   PXrmBindingList = ^TXrmBindingList;

procedure XrmStringToQuarkList(para1:Pchar; para2:TXrmQuarkList);cdecl;external libX11;
procedure XrmStringToBindingQuarkList(para1:Pchar; para2:TXrmBindingList; para3:TXrmQuarkList);cdecl;external libX11;
type

   PXrmName = ^TXrmName;
   TXrmName = TXrmQuark;

   PXrmNameList = ^TXrmNameList;
   TXrmNameList = TXrmQuarkList;
{$ifdef MACROS}
function XrmNameToString(name : longint) : TXrmString;

function XrmStringToName(_string : Pchar) : longint;

procedure XrmStringToNameList(str:Pchar; name : PXrmQuark);
{$endif MACROS}

type

   PXrmClass = ^TXrmClass;
   TXrmClass = TXrmQuark;

   PXrmClassList = ^TXrmClassList;
   TXrmClassList = TXrmQuarkList;
{$ifdef MACROS}
function XrmClassToString(c_class : longint) : TXrmString;

function XrmStringToClass(c_class : Pchar) : longint;

procedure XrmStringToClassList(str:Pchar; c_class : PXrmQuark);
{$endif MACROS}
type

   PXrmRepresentation = ^TXrmRepresentation;
   TXrmRepresentation = TXrmQuark;
{$ifdef MACROS}
function XrmStringToRepresentation(_string : Pchar) : longint;

function XrmRepresentationToString(_type : longint) : TXrmString;
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
   TXrmHashTable = ^TXrmHashBucket;

   TXrmDatabase = PXrmHashBucketRec;
   PXrmDatabase = ^TXrmDatabase;

procedure XrmDestroyDatabase(para1:TXrmDatabase);cdecl;external libX11;
procedure XrmQPutResource(para1:PXrmDatabase; para2:TXrmBindingList; para3:TXrmQuarkList; para4:TXrmRepresentation; para5:PXrmValue);cdecl;external libX11;
procedure XrmPutResource(para1:PXrmDatabase; para2:Pchar; para3:Pchar; para4:PXrmValue);cdecl;external libX11;
procedure XrmQPutStringResource(para1:PXrmDatabase; para2:TXrmBindingList; para3:TXrmQuarkList; para4:Pchar);cdecl;external libX11;
procedure XrmPutStringResource(para1:PXrmDatabase; para2:Pchar; para3:Pchar);cdecl;external libX11;
procedure XrmPutLineResource(para1:PXrmDatabase; para2:Pchar);cdecl;external libX11;
function XrmQGetResource(para1:TXrmDatabase; para2:TXrmNameList; para3:TXrmClassList; para4:PXrmRepresentation; para5:PXrmValue):TBool;cdecl;external libX11;
function XrmGetResource(para1:TXrmDatabase; para2:Pchar; para3:Pchar; para4:PPchar; para5:PXrmValue):TBool;cdecl;external libX11;
{ There is no definition of TXrmSearchList }
{function XrmQGetSearchList(para1:TXrmDatabase; para2:TXrmNameList; para3:TXrmClassList; para4:TXrmSearchList; para5:longint):TBool;cdecl;external libX11;
function XrmQGetSearchResource(para1:TXrmSearchList; para2:TXrmName; para3:TXrmClass; para4:PXrmRepresentation; para5:PXrmValue):TBool;cdecl;external libX11;}
procedure XrmSetDatabase(para1:PDisplay; para2:TXrmDatabase);cdecl;external libX11;
function XrmGetDatabase(para1:PDisplay):TXrmDatabase;cdecl;external libX11;
function XrmGetFileDatabase(para1:Pchar):TXrmDatabase;cdecl;external libX11;
function XrmCombineFileDatabase(para1:Pchar; para2:PXrmDatabase; para3:TBool):TStatus;cdecl;external libX11;
function XrmGetStringDatabase(para1:Pchar):TXrmDatabase;cdecl;external libX11;
procedure XrmPutFileDatabase(para1:TXrmDatabase; para2:Pchar);cdecl;external libX11;
procedure XrmMergeDatabases(para1:TXrmDatabase; para2:PXrmDatabase);cdecl;external libX11;
procedure XrmCombineDatabase(para1:TXrmDatabase; para2:PXrmDatabase; para3:TBool);cdecl;external libX11;

const
   XrmEnumAllLevels = 0;
   XrmEnumOneLevel = 1;
type
  funcbool=function :TBool;
function XrmEnumerateDatabase(para1:TXrmDatabase; para2:TXrmNameList; para3:TXrmClassList; para4:longint; para5:funcbool;
           para6:TXPointer):TBool;cdecl;external libX11;
function XrmLocaleOfDatabase(para1:TXrmDatabase):Pchar;cdecl;external libX11;
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
            para6:PPchar);cdecl;external libX11;

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
function XrmStringsEqual(a1,a2 : pchar) : boolean;
begin
   XrmStringsEqual:=(strcomp(a1,a2)) = 0;
end;

function XrmNameToString(name : longint) : TXrmString;
begin
   XrmNameToString:=XrmQuarkToString(name);
end;

function XrmStringToName(_string : Pchar) : longint;
begin
   XrmStringToName:=XrmStringToQuark(_string);
end;

procedure XrmStringToNameList(str:Pchar; name : PXrmQuark);
begin
   XrmStringToQuarkList(str,name);
end;

function XrmClassToString(c_class : longint) : TXrmString;
begin
   XrmClassToString:=XrmQuarkToString(c_class);
end;

function XrmStringToClass(c_class : Pchar) : longint;
begin
   XrmStringToClass:=XrmStringToQuark(c_class);
end;

procedure XrmStringToClassList(str:Pchar; c_class : PXrmQuark);
begin
   XrmStringToQuarkList(str,c_class);
end;

function XrmStringToRepresentation(_string : Pchar) : longint;
begin
   XrmStringToRepresentation:=XrmStringToQuark(_string);
end;

function XrmRepresentationToString(_type : longint) : TXrmString;
begin
   XrmRepresentationToString:=XrmQuarkToString(_type);
end;
{$endif MACROS}

end.
