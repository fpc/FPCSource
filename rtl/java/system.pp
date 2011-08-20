{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2006 by Florian Klaempfl
    member of the Free Pascal development team.

    System unit for embedded systems

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

Unit system;

{$namespace org.freepascal.rtl}

{*****************************************************************************}
                                    interface
{*****************************************************************************}

{$define FPC_IS_SYSTEM}

{$I-,Q-,H-,R-,V-,P+}
{$implicitexceptions off}
{$mode objfpc}

{$undef FPC_HAS_FEATURE_ANSISTRINGS}
{$undef FPC_HAS_FEATURE_TEXTIO}
{$undef FPC_HAS_FEATURE_VARIANTS}
{$undef FPC_HAS_FEATURE_CLASSES}
{$undef FPC_HAS_FEATURE_EXCEPTIONS}
{$undef FPC_HAS_FEATURE_OBJECTS}
{$undef FPC_HAS_FEATURE_RTTI}
{$undef FPC_HAS_FEATURE_FILEIO}
{$undef FPC_INCLUDE_SOFTWARE_INT64_TO_DOUBLE}

Type
  { The compiler has all integer types defined internally. Here
    we define only aliases }
  DWord    = LongWord;
  Cardinal = LongWord;
  Integer  = SmallInt;
  UInt64   = QWord;
  SizeInt  = Longint;
  SizeUInt = Longint;
  PtrInt   = Longint;
  PtrUInt  = Longint;

  ValReal = Double;

  AnsiChar    = Char;
  UnicodeChar = WideChar;

  { map comp to int64, }
  Comp = Int64;

  HResult = type longint;

  { Java primitive types }
  jboolean = boolean;
  jbyte = shortint;
  jshort = smallint;
  jint = longint;
  jlong = int64;
  jchar = widechar;
  jfloat = single;
  jdouble = double;

  Arr1jboolean = array of jboolean;
  Arr1jbyte = array of jbyte;
  Arr1jshort = array of jshort;
  Arr1jint = array of jint;
  Arr1jlong = array of jlong;
  Arr1jchar = array of jchar;
  Arr1jfloat = array of jfloat;
  Arr1jdouble = array of jdouble;

  Arr2jboolean = array of Arr1jboolean;
  Arr2jbyte = array of Arr1jbyte;
  Arr2jshort = array of Arr1jshort;
  Arr2jint = array of Arr1jint;
  Arr2jlong = array of Arr1jlong;
  Arr2jchar = array of Arr1jchar;
  Arr2jfloat = array of Arr1jfloat;
  Arr2jdouble = array of Arr1jdouble;

  Arr3jboolean = array of Arr2jboolean;
  Arr3jbyte = array of Arr2jbyte;
  Arr3jshort = array of Arr2jshort;
  Arr3jint = array of Arr2jint;
  Arr3jlong = array of Arr2jlong;
  Arr3jchar = array of Arr2jchar;
  Arr3jfloat = array of Arr2jfloat;
  Arr3jdouble = array of Arr2jdouble;

const
{ max. values for longint and int}
  maxLongint  = $7fffffff;
  maxSmallint = 32767;

  maxint   = maxsmallint;


{ Java base class type }
{$i java_sysh.inc}
{$i java_sys.inc}

type
  TObject = class(JLObject)
   strict private
    DestructorCalled: Boolean;
   public
    procedure Free;
    destructor Destroy; virtual;
    procedure finalize; override;
  end;

{$i innr.inc}
{$i jmathh.inc}
{$i jrech.inc}
{$i sstringh.inc}
{$i jdynarrh.inc}
{$i astringh.inc}

{$ifndef nounsupported}
type
  tmethod = record
    code: jlobject;
  end;

const
   vtInteger       = 0;
   vtBoolean       = 1;
   vtChar          = 2;
{$ifndef FPUNONE}
   vtExtended      = 3;
{$endif}
   vtString        = 4;
   vtPointer       = 5;
   vtPChar         = 6;
   vtObject        = 7;
   vtClass         = 8;
   vtWideChar      = 9;
   vtPWideChar     = 10;
   vtAnsiString    = 11;
   vtCurrency      = 12;
   vtVariant       = 13;
   vtInterface     = 14;
   vtWideString    = 15;
   vtInt64         = 16;
   vtQWord         = 17;
   vtUnicodeString = 18;

type
  TVarRec = record
     case VType : sizeint of
{$ifdef ENDIAN_BIG}
       vtInteger       : ({$IFDEF CPU64}integerdummy1 : Longint;{$ENDIF CPU64}VInteger: Longint);
       vtBoolean       : ({$IFDEF CPU64}booldummy : Longint;{$ENDIF CPU64}booldummy1,booldummy2,booldummy3: byte; VBoolean: Boolean);
       vtChar          : ({$IFDEF CPU64}chardummy : Longint;{$ENDIF CPU64}chardummy1,chardummy2,chardummy3: byte; VChar: Char);
       vtWideChar      : ({$IFDEF CPU64}widechardummy : Longint;{$ENDIF CPU64}wchardummy1,VWideChar: WideChar);
{$else ENDIAN_BIG}
       vtInteger       : (VInteger: Longint);
       vtBoolean       : (VBoolean: Boolean);
       vtChar          : (VChar: Char);
       vtWideChar      : (VWideChar: WideChar);
{$endif ENDIAN_BIG}
//       vtString        : (VString: PShortString);
//       vtPointer       : (VPointer: Pointer);
///       vtPChar         : (VPChar: PChar);
       vtObject        : (VObject: TObject);
//       vtClass         : (VClass: TClass);
//       vtPWideChar     : (VPWideChar: PWideChar);
       vtAnsiString    : (VAnsiString: JLObject);
       vtCurrency      : (VCurrency: Currency);
//       vtVariant       : (VVariant: PVariant);
       vtInterface     : (VInterface: JLObject);
       vtWideString    : (VWideString: JLString);
       vtInt64         : (VInt64: Int64);
       vtUnicodeString : (VUnicodeString: JLString);
       vtQWord         : (VQWord: QWord);
   end;

{$endif}

Function  lo(i : Integer) : byte;  [INTERNPROC: fpc_in_lo_Word];
Function  lo(w : Word) : byte;     [INTERNPROC: fpc_in_lo_Word];
Function  lo(l : Longint) : Word;  [INTERNPROC: fpc_in_lo_long];
Function  lo(l : DWord) : Word;    [INTERNPROC: fpc_in_lo_long];
Function  lo(i : Int64) : DWord;   [INTERNPROC: fpc_in_lo_qword];
Function  lo(q : QWord) : DWord;   [INTERNPROC: fpc_in_lo_qword];
Function  hi(i : Integer) : byte;  [INTERNPROC: fpc_in_hi_Word];
Function  hi(w : Word) : byte;     [INTERNPROC: fpc_in_hi_Word];
Function  hi(l : Longint) : Word;  [INTERNPROC: fpc_in_hi_long];
Function  hi(l : DWord) : Word;    [INTERNPROC: fpc_in_hi_long];
Function  hi(i : Int64) : DWord;   [INTERNPROC: fpc_in_hi_qword];
Function  hi(q : QWord) : DWord;   [INTERNPROC: fpc_in_hi_qword];

Function chr(b : byte) : AnsiChar;      [INTERNPROC: fpc_in_chr_byte];

function RorByte(Const AValue : Byte): Byte;[internproc:fpc_in_ror_x];
function RorByte(Const AValue : Byte;Dist : Byte): Byte;[internproc:fpc_in_ror_x_x];

function RolByte(Const AValue : Byte): Byte;[internproc:fpc_in_rol_x];
function RolByte(Const AValue : Byte;Dist : Byte): Byte;[internproc:fpc_in_rol_x_x];

function RorWord(Const AValue : Word): Word;[internproc:fpc_in_ror_x];
function RorWord(Const AValue : Word;Dist : Byte): Word;[internproc:fpc_in_ror_x_x];

function RolWord(Const AValue : Word): Word;[internproc:fpc_in_rol_x];
function RolWord(Const AValue : Word;Dist : Byte): Word;[internproc:fpc_in_rol_x_x];

function RorDWord(Const AValue : DWord): DWord;[internproc:fpc_in_ror_x];
function RorDWord(Const AValue : DWord;Dist : Byte): DWord;[internproc:fpc_in_ror_x_x];

function RolDWord(Const AValue : DWord): DWord;[internproc:fpc_in_rol_x];
function RolDWord(Const AValue : DWord;Dist : Byte): DWord;[internproc:fpc_in_rol_x_x];

function RorQWord(Const AValue : QWord): QWord;[internproc:fpc_in_ror_x];
function RorQWord(Const AValue : QWord;Dist : Byte): QWord;[internproc:fpc_in_ror_x_x];

function RolQWord(Const AValue : QWord): QWord;[internproc:fpc_in_rol_x];
function RolQWord(Const AValue : QWord;Dist : Byte): QWord;[internproc:fpc_in_rol_x_x];

function SarShortint(Const AValue : Shortint): Shortint;[internproc:fpc_in_sar_x];
function SarShortint(Const AValue : Shortint;Shift : Byte): Shortint;[internproc:fpc_in_sar_x_y];

function SarSmallint(Const AValue : Smallint): Smallint;[internproc:fpc_in_sar_x];
function SarSmallint(Const AValue : Smallint;Shift : Byte): Smallint;[internproc:fpc_in_sar_x_y];

function SarLongint(Const AValue : Longint): Longint;[internproc:fpc_in_sar_x];
function SarLongint(Const AValue : Longint;Shift : Byte): Longint;[internproc:fpc_in_sar_x_y];

function SarInt64(Const AValue : Int64): Int64;[internproc:fpc_in_sar_x];
function SarInt64(Const AValue : Int64;Shift : Byte): Int64;[internproc:fpc_in_sar_x_y];


{$i compproc.inc}

{$i ustringh.inc}

{*****************************************************************************}
                                 implementation
{*****************************************************************************}

{i jdynarr.inc}
{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2011 by Jonas Maebe
    member of the Free Pascal development team.

    This file implements the helper routines for dyn. Arrays in FPC

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
}


function min(a,b : longint) : longint;
  begin
     if a<=b then
       min:=a
     else
       min:=b;
  end;


{$i sstrings.inc}
{$i astrings.inc}
{$i ustrings.inc}
{$i rtti.inc}
{$i jrec.inc}
{$i jint64.inc}

{ copying helpers }

procedure fpc_copy_shallow_array(src, dst: JLObject; srcstart: jint = -1; srccopylen: jint = -1);
  var
    srclen, dstlen: jint;
  begin
    if assigned(src) then
      srclen:=JLRArray.getLength(src)
    else
      srclen:=0;
    if assigned(dst) then
      dstlen:=JLRArray.getLength(dst)
    else
      dstlen:=0;
    if srcstart=-1 then
      srcstart:=0
    else if srcstart>=srclen then
      exit;
    if srccopylen=-1 then
      srccopylen:=srclen
    else if srcstart+srccopylen>srclen then
      srccopylen:=srclen-srcstart;
    { causes exception in JLSystem.arraycopy }
    if (srccopylen=0) or
       (dstlen=0) then
      exit;
    JLSystem.arraycopy(src,srcstart,dst,0,min(srccopylen,dstlen));
  end;


procedure fpc_copy_jrecord_array(src, dst: TJRecordArray; srcstart: jint = -1; srccopylen: jint = -1);
  var
    i: longint;
    srclen, dstlen: jint;
  begin
    srclen:=length(src);
    dstlen:=length(dst);
    if srcstart=-1 then
      srcstart:=0
    else if srcstart>=srclen then
      exit;
    if srccopylen=-1 then
      srccopylen:=srclen
    else if srcstart+srccopylen>srclen then
      srccopylen:=srclen-srcstart;
    { no arraycopy, have to clone each element }
    for i:=0 to min(srccopylen,dstlen)-1 do
      dst[i]:=FpcBaseRecordType(src[srcstart+i].clone);
  end;


procedure fpc_copy_jshortstring_array(src, dst: TShortstringArray; srcstart: jint = -1; srccopylen: jint = -1);
  var
    i: longint;
    srclen, dstlen: jint;
  begin
    srclen:=length(src);
    dstlen:=length(dst);
    if srcstart=-1 then
      srcstart:=0
    else if srcstart>=srclen then
      exit;
    if srccopylen=-1 then
      srccopylen:=srclen
    else if srcstart+srccopylen>srclen then
      srccopylen:=srclen-srcstart;
    { no arraycopy, have to clone each element }
    for i:=0 to min(srccopylen,dstlen)-1 do
      dst[i]:=ShortstringClass(src[srcstart+i].clone);
  end;


{ 1-dimensional setlength routines }

function fpc_setlength_dynarr_generic(aorg, anew: JLObject; deepcopy: boolean; docopy: boolean = true): JLObject;
  var
    orglen, newlen: jint;
  begin
    orglen:=0;
    newlen:=0;
    if not deepcopy then
      begin
        if assigned(aorg) then
          orglen:=JLRArray.getLength(aorg)
        else
          orglen:=0;
        if assigned(anew) then
          newlen:=JLRArray.getLength(anew)
        else
          newlen:=0;
      end;
    if deepcopy or
       (orglen<>newlen) then
      begin
        if docopy then
          fpc_copy_shallow_array(aorg,anew);
        result:=anew
      end
    else
      result:=aorg;
  end;


function fpc_setlength_dynarr_jrecord(aorg, anew: TJRecordArray; deepcopy: boolean): TJRecordArray;
  begin
    if deepcopy or
       (length(aorg)<>length(anew)) then
      begin
        fpc_copy_jrecord_array(aorg,anew);
        result:=anew
      end
    else
      result:=aorg;
  end;


function fpc_setlength_dynarr_jshortstring(aorg, anew: TShortstringArray; deepcopy: boolean): TShortstringArray;
  begin
    if deepcopy or
       (length(aorg)<>length(anew)) then
      begin
        fpc_copy_jshortstring_array(aorg,anew);
        result:=anew
      end
    else
      result:=aorg;
  end;


{ multi-dimensional setlength routine }
function fpc_setlength_dynarr_multidim(aorg, anew: TJObjectArray; deepcopy: boolean; ndim: longint; eletype: jchar): TJObjectArray;
  var
    partdone,
    i: longint;

  begin
    { resize the current dimension; no need to copy the subarrays of the old
      array, as the subarrays will be (re-)initialised immediately below }
    { the srcstart/srccopylen always refers to the first dimension (since copy()
      performs a shallow copy of a dynamic array }
    result:=TJObjectArray(fpc_setlength_dynarr_generic(JLObject(aorg),JLObject(anew),deepcopy,false));
    { if aorg was empty, there's nothing else to do since result will now
      contain anew, of which all other dimensions are already initialised
      correctly since there are no aorg elements to copy }
    if not assigned(aorg) and
       not deepcopy then
      exit;
    partdone:=min(high(result),high(aorg));
    { ndim must be >=2 when this routine is called, since it has to return
      an array of java.lang.Object! (arrays are also objects, but primitive
      types are not) }
    if ndim=2 then
      begin
        { final dimension -> copy the primitive arrays }
        case eletype of
          FPCJDynArrTypeRecord:
            begin
              for i:=low(result) to partdone do
                result[i]:=JLObject(fpc_setlength_dynarr_jrecord(TJRecordArray(aorg[i]),TJRecordArray(anew[i]),deepcopy));
              for i:=succ(partdone) to high(result) do
                result[i]:=JLObject(fpc_setlength_dynarr_jrecord(nil,TJRecordArray(anew[i]),deepcopy));
            end;
          FPCJDynArrTypeShortstring:
            begin
              for i:=low(result) to partdone do
                result[i]:=JLObject(fpc_setlength_dynarr_jshortstring(TShortstringArray(aorg[i]),TShortstringArray(anew[i]),deepcopy));
              for i:=succ(partdone) to high(result) do
                result[i]:=JLObject(fpc_setlength_dynarr_jshortstring(nil,TShortstringArray(anew[i]),deepcopy));
            end;
          else
            begin
              for i:=low(result) to partdone do
                result[i]:=fpc_setlength_dynarr_generic(aorg[i],anew[i],deepcopy);
              for i:=succ(partdone) to high(result) do
                result[i]:=fpc_setlength_dynarr_generic(nil,anew[i],deepcopy);
            end;
        end;
      end
    else
      begin
        { recursively handle the next dimension }
        for i:=low(result) to partdone do
          result[i]:=JLObject(fpc_setlength_dynarr_multidim(TJObjectArray(aorg[i]),TJObjectArray(anew[i]),deepcopy,pred(ndim),eletype));
        for i:=succ(partdone) to high(result) do
          result[i]:=JLObject(fpc_setlength_dynarr_multidim(nil,TJObjectArray(anew[i]),deepcopy,pred(ndim),eletype));
      end;
  end;


function fpc_dynarray_copy(src: JLObject; start, len: longint; ndim: longint; eletype: jchar): JLObject;
  var
    i: longint;
    srclen: longint;
  begin
    if not assigned(src) then
      begin
        result:=nil;
        exit;
      end;
    srclen:=JLRArray.getLength(src);
    if (start=-1) and
       (len=-1) then
      begin
        len:=srclen;
        start:=0;
      end
    else if (start+len>srclen) then
      len:=srclen-start+1;
    result:=JLRArray.newInstance(src.getClass.getComponentType,len);
    if ndim=1 then
      begin
        case eletype of
          FPCJDynArrTypeRecord:
            fpc_copy_jrecord_array(TJRecordArray(src),TJRecordArray(result),start,len);
          FPCJDynArrTypeShortstring:
            fpc_copy_jshortstring_array(TShortstringArray(src),TShortstringArray(result),start,len);
          else
            fpc_copy_shallow_array(src,result,start,len);
        end
      end
    else
      begin
        for i:=0 to len-1 do
          TJObjectArray(result)[i]:=fpc_dynarray_copy(TJObjectArray(src)[start+i],-1,-1,ndim-1,eletype);
      end;
  end;


{i jdynarr.inc end}



{*****************************************************************************
                       Misc. System Dependent Functions
*****************************************************************************}

  procedure TObject.Free;
    begin
      if not DestructorCalled then
        begin
          DestructorCalled:=true;
          Destroy;
        end;
    end;


  destructor TObject.Destroy;
    begin
    end;


  procedure TObject.Finalize;
    begin
      Free;
    end;

{*****************************************************************************
                         SystemUnit Initialization
*****************************************************************************}

end.

