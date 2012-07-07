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
      src[srcstart+i].fpcDeepCopy(dst[i]);
  end;


procedure fpc_copy_jenumset_array(src, dst: TJEnumSetArray; srcstart: jint = -1; srccopylen: jint = -1);
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
      begin
        dst[i].clear;
        dst[i].addAll(src[srcstart+i]);
      end;
  end;


procedure fpc_copy_jbitset_array(src, dst: TJBitSetArray; srcstart: jint = -1; srccopylen: jint = -1);
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
      begin
        dst[i].clear;
        dst[i].addAll(src[srcstart+i]);
      end;
  end;


procedure fpc_copy_jprocvar_array(src, dst: TJProcVarArray; srcstart: jint = -1; srccopylen: jint = -1);
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
      src[srcstart+i].fpcDeepCopy(dst[i]);
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
      pshortstring(src[srcstart+i])^:=pshortstring(dst[i])^;
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


function fpc_setlength_dynarr_jenumset(aorg, anew: TJEnumSetArray; deepcopy: boolean): TJEnumSetArray;
  begin
    if deepcopy or
       (length(aorg)<>length(anew)) then
      begin
        fpc_copy_jenumset_array(aorg,anew);
        result:=anew
      end
    else
      result:=aorg;
  end;


function fpc_setlength_dynarr_jbitset(aorg, anew: TJBitSetArray; deepcopy: boolean): TJBitSetArray;
  begin
    if deepcopy or
       (length(aorg)<>length(anew)) then
      begin
        fpc_copy_jbitset_array(aorg,anew);
        result:=anew
      end
    else
      result:=aorg;
  end;


function fpc_setlength_dynarr_jprocvar(aorg, anew: TJProcVarArray; deepcopy: boolean): TJProcVarArray;
  begin
    if deepcopy or
       (length(aorg)<>length(anew)) then
      begin
        fpc_copy_jprocvar_array(aorg,anew);
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
          FPCJDynArrTypeEnumSet:
            begin
              for i:=low(result) to partdone do
                result[i]:=JLObject(fpc_setlength_dynarr_jenumset(TJEnumSetArray(aorg[i]),TJEnumSetArray(anew[i]),deepcopy));
              for i:=succ(partdone) to high(result) do
                result[i]:=JLObject(fpc_setlength_dynarr_jenumset(nil,TJEnumSetArray(anew[i]),deepcopy));
            end;
          FPCJDynArrTypeBitSet:
            begin
              for i:=low(result) to partdone do
                result[i]:=JLObject(fpc_setlength_dynarr_jbitset(TJBitSetArray(aorg[i]),TJBitSetArray(anew[i]),deepcopy));
              for i:=succ(partdone) to high(result) do
                result[i]:=JLObject(fpc_setlength_dynarr_jbitset(nil,TJBitSetArray(anew[i]),deepcopy));
            end;
          FPCJDynArrTypeProcVar:
            begin
              for i:=low(result) to partdone do
                result[i]:=JLObject(fpc_setlength_dynarr_jprocvar(TJProcVarArray(aorg[i]),TJProcVarArray(anew[i]),deepcopy));
              for i:=succ(partdone) to high(result) do
                result[i]:=JLObject(fpc_setlength_dynarr_jprocvar(nil,TJProcVarArray(anew[i]),deepcopy));
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
            begin
              for i:=0 to len-1 do
                TJObjectArray(result)[i]:=FpcBaseRecordType(TJObjectArray(src)[i]).clone;
            end;
          FPCJDynArrTypeEnumSet:
            begin
              for i:=0 to len-1 do
                TJObjectArray(result)[i]:=JUEnumSet(TJObjectArray(src)[i]).clone;
            end;
          FPCJDynArrTypeBitSet:
            begin
              for i:=0 to len-1 do
                TJObjectArray(result)[i]:=FpcBitSet(TJObjectArray(src)[i]).clone;
            end;
          FPCJDynArrTypeProcvar:
            begin
              for i:=0 to len-1 do
                TJObjectArray(result)[i]:=FpcBaseProcVarType(TJObjectArray(src)[i]).clone;
            end;
          FPCJDynArrTypeShortstring:
            begin
              for i:=0 to len-1 do
                TJObjectArray(result)[i]:=ShortStringClass(TJObjectArray(src)[i]).clone;
            end;
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

