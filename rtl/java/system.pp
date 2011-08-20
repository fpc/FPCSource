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

{*****************************************************************************}
                                    interface
{*****************************************************************************}

{$define FPC_IS_SYSTEM}

{$I-,Q-,H-,R-,V-}
{$implicitexceptions off}
{$mode objfpc}

{$undef FPC_HAS_FEATURE_RTTI}

Type
  { The compiler has all integer types defined internally. Here
    we define only aliases }
  DWord    = LongWord;
  Cardinal = LongWord;
  Integer  = SmallInt;
  UInt64   = QWord;
  
  ValReal = Double;
  
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
  TObject = JLObject;

{$i innr.inc}
{$i jmathh.inc}
{$i jdynarrh.inc}

{$i compproc.inc}

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

{$i rtti.inc}

function min(a,b : longint) : longint;
  begin
     if a<=b then
       min:=a
     else
       min:=b;
  end;

{ copying helpers }

{ also for booleans }
procedure fpc_copy_jbyte_array(src, dst: TJByteArray);
  var
    i: longint;
  begin
    for i:=0 to min(high(src),high(dst)) do
      dst[i]:=src[i];
  end;


procedure fpc_copy_jshort_array(src, dst: TJShortArray);
  var
    i: longint;
  begin
    for i:=0 to min(high(src),high(dst)) do
      dst[i]:=src[i];
  end;


procedure fpc_copy_jint_array(src, dst: TJIntArray);
  var
    i: longint;
  begin
    for i:=0 to min(high(src),high(dst)) do
      dst[i]:=src[i];
  end;


procedure fpc_copy_jlong_array(src, dst: TJLongArray);
  var
    i: longint;
  begin
    for i:=0 to min(high(src),high(dst)) do
      dst[i]:=src[i];
  end;


procedure fpc_copy_jchar_array(src, dst: TJCharArray);
  var
    i: longint;
  begin
    for i:=0 to min(high(src),high(dst)) do
      dst[i]:=src[i];
  end;


procedure fpc_copy_jfloat_array(src, dst: TJFloatArray);
  var
    i: longint;
  begin
    for i:=0 to min(high(src),high(dst)) do
      dst[i]:=src[i];
  end;


procedure fpc_copy_jdouble_array(src, dst: TJDoubleArray);
  var
    i: longint;
  begin
    for i:=0 to min(high(src),high(dst)) do
      dst[i]:=src[i];
  end;


procedure fpc_copy_jobject_array(src, dst: TJObjectArray);
  var
    i: longint;
  begin
    for i:=0 to min(high(src),high(dst)) do
      dst[i]:=src[i];
  end;


{ 1-dimensional setlength routines }

function fpc_setlength_dynarr_jbyte(aorg, anew: TJByteArray; deepcopy: boolean): TJByteArray;
  begin
    if deepcopy or
       (length(aorg)<>length(anew)) then
      begin
        fpc_copy_jbyte_array(aorg,anew);
        result:=anew
      end
    else
      result:=aorg;
  end;


function fpc_setlength_dynarr_jshort(aorg, anew: TJShortArray; deepcopy: boolean): TJShortArray;
  begin
    if deepcopy or
       (length(aorg)<>length(anew)) then
      begin
        fpc_copy_jshort_array(aorg,anew);
        result:=anew
      end
    else
      result:=aorg;
  end;


function fpc_setlength_dynarr_jint(aorg, anew: TJIntArray; deepcopy: boolean): TJIntArray;
  begin
    if deepcopy or
       (length(aorg)<>length(anew)) then
      begin
        fpc_copy_jint_array(aorg,anew);
        result:=anew
      end
    else
      result:=aorg;
  end;


function fpc_setlength_dynarr_jlong(aorg, anew: TJLongArray; deepcopy: boolean): TJLongArray;
  begin
    if deepcopy or
       (length(aorg)<>length(anew)) then
      begin
        fpc_copy_jlong_array(aorg,anew);
        result:=anew
      end
    else
      result:=aorg;
  end;


function fpc_setlength_dynarr_jchar(aorg, anew: TJCharArray; deepcopy: boolean): TJCharArray;
  begin
    if deepcopy or
       (length(aorg)<>length(anew)) then
      begin
        fpc_copy_jchar_array(aorg,anew);
        result:=anew
      end
    else
      result:=aorg;
  end;


function fpc_setlength_dynarr_jfloat(aorg, anew: TJFloatArray; deepcopy: boolean): TJFloatArray;
  begin
    if deepcopy or
       (length(aorg)<>length(anew)) then
      begin
        fpc_copy_jfloat_array(aorg,anew);
        result:=anew
      end
    else
      result:=aorg;
  end;


function fpc_setlength_dynarr_jdouble(aorg, anew: TJDoubleArray; deepcopy: boolean): TJDoubleArray;
  begin
    if deepcopy or
       (length(aorg)<>length(anew)) then
      begin
        fpc_copy_jdouble_array(aorg,anew);
        result:=anew
      end
    else
      result:=aorg;
  end;


function fpc_setlength_dynarr_jobject(aorg, anew: TJObjectArray; deepcopy: boolean; docopy : boolean = true): TJObjectArray;
  begin
    if deepcopy or
       (length(aorg)<>length(anew)) then
      begin
        if docopy then
          fpc_copy_jobject_array(aorg,anew);
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
    result:=fpc_setlength_dynarr_jobject(aorg,anew,deepcopy,false);
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
          FPCJDynArrTypeJByte:
            begin
              for i:=low(result) to partdone do
                result[i]:=TObject(fpc_setlength_dynarr_jbyte(TJByteArray(aorg[i]),TJByteArray(anew[i]),deepcopy));
              for i:=succ(partdone) to high(result) do
                result[i]:=TObject(fpc_setlength_dynarr_jbyte(nil,TJByteArray(anew[i]),deepcopy));
            end;
          FPCJDynArrTypeJShort:
            begin
              for i:=low(result) to partdone do
                result[i]:=TObject(fpc_setlength_dynarr_jshort(TJShortArray(aorg[i]),TJShortArray(anew[i]),deepcopy));
              for i:=succ(partdone) to high(result) do
                result[i]:=TObject(fpc_setlength_dynarr_jshort(nil,TJShortArray(anew[i]),deepcopy));
            end;
          FPCJDynArrTypeJInt:
            begin
              for i:=low(result) to partdone do
                result[i]:=TObject(fpc_setlength_dynarr_jint(TJIntArray(aorg[i]),TJIntArray(anew[i]),deepcopy));
              for i:=succ(partdone) to high(result) do
                result[i]:=TObject(fpc_setlength_dynarr_jint(nil,TJIntArray(anew[i]),deepcopy));
            end;
          FPCJDynArrTypeJLong:
            begin
              for i:=low(result) to partdone do
                result[i]:=TObject(fpc_setlength_dynarr_jlong(TJLongArray(aorg[i]),TJLongArray(anew[i]),deepcopy));
              for i:=succ(partdone) to high(result) do
                result[i]:=TObject(fpc_setlength_dynarr_jlong(nil,TJLongArray(anew[i]),deepcopy));
            end;
          FPCJDynArrTypeJChar:
            begin
              for i:=low(result) to partdone do
                result[i]:=TObject(fpc_setlength_dynarr_jchar(TJCharArray(aorg[i]),TJCharArray(anew[i]),deepcopy));
              for i:=succ(partdone) to high(result) do
                result[i]:=TObject(fpc_setlength_dynarr_jchar(nil,TJCharArray(anew[i]),deepcopy));
            end;
          FPCJDynArrTypeJFloat:
            begin
              for i:=low(result) to partdone do
                result[i]:=TObject(fpc_setlength_dynarr_jfloat(TJFloatArray(aorg[i]),TJFloatArray(anew[i]),deepcopy));
              for i:=succ(partdone) to high(result) do
                result[i]:=TObject(fpc_setlength_dynarr_jfloat(nil,TJFloatArray(anew[i]),deepcopy));
            end;
          FPCJDynArrTypeJDouble:
            begin
              for i:=low(result) to partdone do
                result[i]:=TObject(fpc_setlength_dynarr_jdouble(TJDoubleArray(aorg[i]),TJDoubleArray(anew[i]),deepcopy));
              for i:=succ(partdone) to high(result) do
                result[i]:=TObject(fpc_setlength_dynarr_jdouble(nil,TJDoubleArray(anew[i]),deepcopy));
            end;
          FPCJDynArrTypeJObject:
            begin
              for i:=low(result) to partdone do
                result[i]:=TObject(fpc_setlength_dynarr_jobject(TJObjectArray(aorg[i]),TJObjectArray(anew[i]),deepcopy,true));
              for i:=succ(partdone) to high(result) do
                result[i]:=TObject(fpc_setlength_dynarr_jobject(nil,TJObjectArray(anew[i]),deepcopy,true));
            end;
        end;
      end
    else
      begin
        { recursively handle the next dimension }
        for i:=low(result) to partdone do
          result[i]:=TObject(fpc_setlength_dynarr_multidim(TJObjectArray(aorg[i]),TJObjectArray(anew[i]),deepcopy,pred(ndim),eletype));
        for i:=succ(partdone) to high(result) do
          result[i]:=TObject(fpc_setlength_dynarr_multidim(nil,TJObjectArray(anew[i]),deepcopy,pred(ndim),eletype));
      end;
  end;



{i jdynarr.inc end}



{*****************************************************************************
                       Misc. System Dependent Functions
*****************************************************************************}

{*****************************************************************************
                         SystemUnit Initialization
*****************************************************************************}

end.
