{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team.

    Processor independent implementation for the system unit
    (adapted for intel i386.inc file)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{****************************************************************************
                               Primitives
****************************************************************************}
type
  pstring = ^shortstring;

{$ifndef FPC_SYSTEM_HAS_MOVE}
procedure Move(const source;var dest;count:SizeInt);[public, alias: 'FPC_MOVE'];
var
  aligncount : sizeint;
  pdest,psrc,pend : pbyte;
begin
  if (@dest=@source) or (count<=0) then
    exit;
  if (@dest<@source) or (@source+count<@dest) then
    begin
      { Forward Move }
      psrc:=@source;
      pdest:=@dest;
      if (Count>4*sizeof(ptruint)-11)
{$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
        and ((PtrUInt(pdest) and (sizeof(PtrUInt)-1))=(PtrUInt(psrc) and (sizeof(PtrUInt)-1)))
{$endif FPC_REQUIRES_PROPER_ALIGNMENT}
        then
        begin
          { Align on native pointer size }
          aligncount:=(PtrUInt(pdest) and (sizeof(PtrUInt)-1));
          dec(count,aligncount);
          pend:=psrc+aligncount;
          while psrc<pend do
            begin
              pdest^:=psrc^;
              inc(pdest);
              inc(psrc);
            end;
          { use sizeuint typecast to force shr optimization }
          pptruint(pend):=pptruint(psrc)+(sizeuint(count) div sizeof(ptruint));
          while psrc<pend do
            begin
              pptruint(pdest)^:=pptruint(psrc)^;
              inc(pptruint(pdest));
              inc(pptruint(psrc));
            end;
          count:=count and (sizeof(PtrUInt)-1);
        end;
      pend:=psrc+count;
      while psrc<pend do
        begin
          pdest^:=psrc^;
          inc(pdest);
          inc(psrc);
        end;
    end
  else
    begin
      { Backward Move }
      psrc:=@source+count;
      pdest:=@dest+count;
      if (Count>4*sizeof(ptruint)-11)
{$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
        and ((PtrUInt(pdest) and (sizeof(PtrUInt)-1))=(PtrUInt(psrc) and (sizeof(PtrUInt)-1)))
{$endif FPC_REQUIRES_PROPER_ALIGNMENT}
        then
        begin
          { Align on native pointer size }
          aligncount:=(PtrUInt(pdest) and (sizeof(PtrUInt)-1));
          dec(count,aligncount);
          pend:=psrc-aligncount;
          while psrc>pend do
            begin
              dec(pdest);
              dec(psrc);
              pdest^:=psrc^;
            end;
          { use sizeuint typecast to force shr optimization }
          pptruint(pend):=pptruint(psrc)-(sizeuint(count) div sizeof(ptruint));
          while psrc>pend do
            begin
              dec(pptruint(pdest));
              dec(pptruint(psrc));
              pptruint(pdest)^:=pptruint(psrc)^;
            end;
          count:=count and (sizeof(PtrUInt)-1);
        end;
      pend:=psrc-count;
      while psrc>pend do
        begin
          dec(pdest);
          dec(psrc);
          pdest^:=psrc^;
        end;
    end;
end;
{$endif not FPC_SYSTEM_HAS_MOVE}


{$ifndef FPC_SYSTEM_HAS_FILLCHAR}
Procedure FillChar(var x;count:SizeInt;value:byte);
var
  pdest,pend : pbyte;
  v : ptruint;
begin
  if count <= 0 then
    exit;
  pdest:=@x;
  if Count>4*sizeof(ptruint)-1 then
    begin
      v:=(value shl 8) or value;
      v:=(v shl 16) or v;
      if sizeof(ptruint)=8 then
        v:=(v shl 32) or v;
      { Align on native pointer size }
      pend:=pbyte(align(pdest,sizeof(PtrUInt)));
      dec(count,pend-pdest);
      while pdest<pend do
        begin
          pdest^:=value;
          inc(pdest);
        end;
      { use sizeuint typecast to force shr optimization }
      pptruint(pend):=pptruint(pdest)+(sizeuint(count) div sizeof(ptruint));
      while pdest<pend do
        begin
          pptruint(pdest)^:=v;
          inc(pptruint(pdest));
        end;
      count:=count and (sizeof(ptruint)-1);
    end;
  pend:=pdest+count;
  while pdest<pend do
    begin
      pdest^:=value;
      inc(pdest);
    end;
end;
{$endif FPC_SYSTEM_HAS_FILLCHAR}


{$ifndef FPC_SYSTEM_HAS_FILLWORD}
procedure fillword(var x;count : SizeInt;value : word);
var
  aligncount : sizeint;
  pdest,pend : pword;
  v : ptruint;
begin
  if count <= 0 then
    exit;
  pdest:=@x;
  if Count>4*sizeof(ptruint)-1 then
    begin
      v:=(value shl 16) or value;
{$ifdef CPU64}
      v:=(v shl 32) or v;
{$endif CPU64}
      { Align on native pointer size }
      aligncount:=(PtrUInt(pdest) and (sizeof(PtrUInt)-1)) shr 1;
      dec(count,aligncount);
      pend:=pdest+aligncount;
      while pdest<pend do
        begin
          pdest^:=value;
          inc(pdest);
        end;
      { use sizeuint typecast to force shr optimization }
      pptruint(pend):=pptruint(pdest)+((sizeuint(count)*2) div sizeof(ptruint));
      while pdest<pend do
        begin
          pptruint(pdest)^:=v;
          inc(pptruint(pdest));
        end;
      count:=((count*2) and (sizeof(ptruint)-1)) shr 1;
    end;
  pend:=pdest+count;
  while pdest<pend do
    begin
      pdest^:=value;
      inc(pdest);
    end;
end;
{$endif not FPC_SYSTEM_HAS_FILLWORD}


{$ifndef FPC_SYSTEM_HAS_FILLDWORD}
procedure filldword(var x;count : SizeInt;value : dword);
var
  aligncount : sizeint;
  pdest,pend : pdword;
  v : ptruint;
begin
  if count <= 0 then
    exit;
  pdest:=@x;
  if Count>4*sizeof(ptruint)-1 then
    begin
      v:=value;
{$ifdef CPU64}
      v:=(v shl 32) or v;
{$endif CPU64}
      { Align on native pointer size }
      aligncount:=(PtrUInt(pdest) and (sizeof(PtrUInt)-1)) shr 2;
      dec(count,aligncount);
      pend:=pdest+aligncount;
      while pdest<pend do
        begin
          pdest^:=value;
          inc(pdest);
        end;
      { use sizeuint typecast to force shr optimization }
      pptruint(pend):=pptruint(pdest)+((sizeuint(count)*4) div sizeof(ptruint));
      while pdest<pend do
        begin
          pptruint(pdest)^:=v;
          inc(pptruint(pdest));
        end;
      count:=((count*4) and (sizeof(ptruint)-1)) shr 2;
    end;
  pend:=pdest+count;
  while pdest<pend do
    begin
      pdest^:=value;
      inc(pdest);
    end;
end;
{$endif FPC_SYSTEM_HAS_FILLDWORD}


{$ifndef FPC_SYSTEM_HAS_INDEXBYTE}
function IndexByte(Const buf;len:SizeInt;b:byte):SizeInt;
var
  psrc,pend : pbyte;
begin
  psrc:=@buf;
  { simulate assembler implementations behaviour, which is expected }
  { fpc_pchar_to_ansistr in astrings.inc                            }
  if (len < 0) or
     (psrc+len < psrc) then
    pend:=pbyte(high(PtrUInt)-sizeof(byte))
  else
    pend:=psrc+len;
  while (psrc<pend) do
    begin
      if psrc^=b then
        begin
          result:=psrc-pbyte(@buf);
          exit;
        end;
      inc(psrc);
    end;
  result:=-1;
end;
{$endif not FPC_SYSTEM_HAS_INDEXBYTE}


{$ifndef FPC_SYSTEM_HAS_INDEXWORD}
function Indexword(Const buf;len:SizeInt;b:word):SizeInt;
var
  psrc,pend : pword;
begin
  psrc:=@buf;
  { simulate assembler implementations behaviour, which is expected }
  { fpc_pchar_to_ansistr in astrings.inc                            }
  if (len < 0) or
     { is this ever true? }
     (len > high(PtrInt)) or
     (psrc+len < psrc) then
    pend:=pword(high(PtrUInt)-sizeof(word))
  else
    pend:=psrc+len;
{$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
  if (ptruint(psrc) mod 2)<>0 then
    while psrc<pend do
      begin
        if unaligned(psrc^)=b then
          begin
            result:=psrc-pword(@buf);
            exit;
          end;
        inc(psrc);
      end
  else
{$endif FPC_REQUIRES_PROPER_ALIGNMENT}
    while psrc<pend do
      begin
        if psrc^=b then
          begin
            result:=psrc-pword(@buf);
            exit;
          end;
        inc(psrc);
      end;
  result:=-1;
end;
{$endif not FPC_SYSTEM_HAS_INDEXWORD}


{$ifndef FPC_SYSTEM_HAS_INDEXDWORD}
function IndexDWord(Const buf;len:SizeInt;b:DWord):SizeInt;
var
  psrc,pend : pdword;
begin
  psrc:=@buf;
  { simulate assembler implementations behaviour, which is expected }
  { fpc_pchar_to_ansistr in astrings.inc                            }
  if (len < 0) or
     (len > high(PtrInt) div 2) or
     (psrc+len < psrc) then
    pend:=pdword(high(PtrUInt)-sizeof(dword))
  else
    pend:=psrc+len;
{$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
  if (ptruint(psrc) mod 4)<>0 then
    while psrc<pend do
      begin
        if unaligned(psrc^)=b then
          begin
            result:=psrc-pdword(@buf);
            exit;
          end;
        inc(psrc);
      end
  else
{$endif FPC_REQUIRES_PROPER_ALIGNMENT}
    while psrc<pend do
      begin
        if psrc^=b then
          begin
            result:=psrc-pdword(@buf);
            exit;
          end;
        inc(psrc);
      end;
  result:=-1;
end;
{$endif not FPC_SYSTEM_HAS_INDEXDWORD}


{$ifndef FPC_SYSTEM_HAS_COMPAREBYTE}
function CompareByte(Const buf1,buf2;len:SizeInt):SizeInt;
var
  aligncount : sizeint;
  psrc,pdest,pend : pbyte;
  b : ptrint;
begin
  b:=0;
  psrc:=@buf1;
  pdest:=@buf2;
  if (len>4*sizeof(ptruint)-1)
{$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
    and ((PtrUInt(pdest) and (sizeof(PtrUInt)-1))=(PtrUInt(psrc) and (sizeof(PtrUInt)-1)))
{$endif FPC_REQUIRES_PROPER_ALIGNMENT}
    then
    begin
      { Align on native pointer size }
      aligncount:=(sizeof(PtrUInt)-(PtrUInt(pdest) and (sizeof(PtrUInt)-1))) and (sizeof(PtrUInt)-1);
      dec(len,aligncount);
      pend:=psrc+aligncount;
      while psrc<pend do
        begin
          b:=(ptrint(psrc^)-ptrint(pdest^));
          if b<>0 then
            begin
              if b<0 then
                exit(-1)
              else
                exit(1);
            end;
          inc(pdest);
          inc(psrc);
        end;
      { use sizeuint typecast to force shr optimization }
      pptruint(pend):=pptruint(psrc)+(sizeuint(len) div sizeof(ptruint));
      len:=len and (sizeof(PtrUInt)-1);
      while psrc<pend do
        begin
          b:=(pptrint(psrc)^-pptrint(pdest)^);
          if b<>0 then
            begin
              len:=sizeof(ptruint);
              break;
            end;
          inc(pptruint(pdest));
          inc(pptruint(psrc));
        end;
    end;
  if (psrc+len >= psrc) then
    pend:=psrc+len
  else
    pend:=pbyte(high(ptruint)-1);
  while psrc<pend do
    begin
      b:=(ptrint(psrc^)-ptrint(pdest^));
      if b<>0 then
        begin
          if b<0 then
            exit(-1)
          else
            exit(1);
        end;
      inc(pdest);
      inc(psrc);
    end;
  result:=0;
end;
{$endif not FPC_SYSTEM_HAS_COMPAREBYTE}


{$ifndef FPC_SYSTEM_HAS_COMPAREWORD}
function CompareWord(Const buf1,buf2;len:SizeInt):SizeInt;
var
  aligncount : sizeint;
  psrc,pdest,pend : pword;
  b : ptrint;
begin
  b:=0;
  psrc:=@buf1;
  pdest:=@buf2;
  if (len>4*sizeof(ptruint)-1)
{$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
    and ((PtrUInt(pdest) and (sizeof(PtrUInt)-1))=(PtrUInt(psrc) and (sizeof(PtrUInt)-1)))
    and (((PtrUInt(pdest) and 1) or (PtrUInt(psrc) and 1))=0)
{$endif FPC_REQUIRES_PROPER_ALIGNMENT}
    then
     begin
      { Align on native pointer size }
      aligncount:=((sizeof(PtrUInt)-(PtrUInt(pdest) and (sizeof(PtrUInt)-1))) and (sizeof(PtrUInt)-1)) shr 1;
      dec(len,aligncount);
      pend:=psrc+aligncount;
      while psrc<pend do
        begin
          b:=(ptrint(psrc^)-ptrint(pdest^));
          if b<>0 then
            begin
              if b<0 then
                exit(-1)
              else
                exit(1);
            end;
          inc(pdest);
          inc(psrc);
        end;
      { use sizeuint typecast to force shr optimization }
      pptruint(pend):=pptruint(psrc)+(sizeuint(len)*2 div sizeof(ptruint));
      len:=((len*2) and (sizeof(PtrUInt)-1)) shr 1;
      while psrc<pend do
        begin
          b:=(pptrint(psrc)^-pptrint(pdest)^);
          if b<>0 then
            begin
              len:=sizeof(ptruint) shr 1;
              break;
            end;
          inc(pptruint(pdest));
          inc(pptruint(psrc));
        end;
    end;
  if (len <= high(ptrint)) and
     (psrc+len >= psrc) then
    pend:=psrc+len
  else
    pend:=pword(high(ptruint)-2);
{$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
  if ((PtrUInt(pdest) and 1) or (PtrUInt(psrc) and 1))<>0 then
    while psrc<pend do
      begin
        b:=(ptrint(unaligned(psrc^))-ptrint(unaligned(pdest^)));
        if b<>0 then
          begin
            if b<0 then
              exit(-1)
            else
              exit(1);
          end;
        inc(pdest);
        inc(psrc);
      end
  else
{$endif FPC_REQUIRES_PROPER_ALIGNMENT}
    while psrc<pend do
      begin
        b:=(ptrint(psrc^)-ptrint(pdest^));
        if b<>0 then
          begin
            if b<0 then
              exit(-1)
            else
              exit(1);
          end;
        inc(pdest);
        inc(psrc);
      end;
  result:=0;
end;
{$endif not FPC_SYSTEM_HAS_COMPAREWORD}


{$ifndef FPC_SYSTEM_HAS_COMPAREDWORD}
function CompareDWord(Const buf1,buf2;len:SizeInt):SizeInt;
var
  aligncount : sizeint;
  psrc,pdest,pend : pdword;
begin
  psrc:=@buf1;
  pdest:=@buf2;
  if (len>4*sizeof(ptruint)-11)
{$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
    and (((PtrUInt(pdest) and 3) or (PtrUInt(psrc) and 3))=0)
{$endif FPC_REQUIRES_PROPER_ALIGNMENT}
    then
    begin
      { Align on native pointer size }
      aligncount:=((sizeof(PtrUInt)-(PtrUInt(pdest) and (sizeof(PtrUInt)-1))) and (sizeof(PtrUInt)-1)) shr 2;
      dec(len,aligncount);
      pend:=psrc+aligncount;
      while psrc<pend do
        begin
          if psrc^<>pdest^ then
            if psrc^>pdest^ then
              exit(1)
            else
              exit(-1);
          inc(pdest);
          inc(psrc);
        end;
      { use sizeuint typecast to force shr optimization }
      pptruint(pend):=pptruint(psrc)+(sizeuint(len)*4 div sizeof(ptruint));
      len:=((len*4) and (sizeof(PtrUInt)-1)) shr 2;
      while psrc<pend do
        begin
          if pptrint(psrc)^<>pptrint(pdest)^ then
            begin
              len:=sizeof(ptruint) shr 2;
              break;
            end;
          inc(pptruint(pdest));
          inc(pptruint(psrc));
        end;
    end;
  if (len <= high(ptrint) div 2) and
     (psrc+len >= psrc) then
    pend:=psrc+len
  else
    pend:=pdword(high(ptruint)-4);
{$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
  if ((PtrUInt(pdest) and 3) or (PtrUInt(psrc) and 3))<>0 then
    while psrc<pend do
      begin
        if unaligned(psrc^)<>unaligned(pdest^) then
          if unaligned(psrc^)>unaligned(pdest^) then
            exit(1)
          else
            exit(-1);
        inc(pdest);
        inc(psrc);
      end
  else
{$endif FPC_REQUIRES_PROPER_ALIGNMENT}
    while psrc<pend do
      begin
        if psrc^<>pdest^ then
          if psrc^>pdest^ then
            exit(1)
          else
            exit(-1);
        inc(pdest);
        inc(psrc);
      end;
  result:=0;
end;
{$endif ndef FPC_SYSTEM_HAS_COMPAREDWORD}


{$ifndef FPC_SYSTEM_HAS_MOVECHAR0}
procedure MoveChar0(Const buf1;var buf2;len:SizeInt);
var
  I : SizeInt;
begin
  if Len = 0 then
    exit;
  I:=IndexByte(Buf1,Len,0);
  if I<>-1 then
    Move(Buf1,Buf2,I)
  else
    Move(Buf1,Buf2,len);
end;
{$endif ndef FPC_SYSTEM_HAS_MOVECHAR0}


{$ifndef FPC_SYSTEM_HAS_INDEXCHAR0}
function IndexChar0(Const buf;len:SizeInt;b:Char):SizeInt;
var
  psrc,pend : pbyte;
begin
  psrc:=@buf;
  { simulate assembler implementations behaviour, which is expected }
  { fpc_pchar_to_ansistr in astrings.inc                            }
  if (len < 0) then
    pend:=pbyte(high(PtrUInt)-sizeof(byte))
  else
    pend:=psrc+len;
  while (psrc<pend) and (psrc^<>0) do
    begin
      if (psrc^=byte(b)) then
        begin
          result:=psrc-pbyte(@buf);
          exit;
        end;
      inc(psrc);
    end;
  result:=-1;
end;
{$endif ndef FPC_SYSTEM_HAS_INDEXCHAR0}


{$ifndef FPC_SYSTEM_HAS_COMPARECHAR0}
function CompareChar0(Const buf1,buf2;len:SizeInt):SizeInt;
var
  psrc,pdest,pend : pbyte;
  b : ptrint;
begin
  b:=0;
  psrc:=@buf1;
  pdest:=@buf2;
  pend:=psrc+len;
  while psrc<pend do
    begin
      b:=(ptrint(psrc^)-ptrint(pdest^));
      if b<0 then
        exit(-1)
      else if b>0 then
        exit(1);
      if (psrc^=0) or (pdest^=0) then
        exit(0);
      inc(pdest);
      inc(psrc);
    end;
  result:=0;
end;
{$endif not FPC_SYSTEM_HAS_COMPARECHAR0}


{****************************************************************************
                              Object Helpers
****************************************************************************}

{$ifdef FPC_HAS_FEATURE_OBJECTS}

{$ifndef FPC_SYSTEM_HAS_FPC_HELP_CONSTRUCTOR}
{ Note: _vmt will be reset to -1 when memory is allocated,
  this is needed for fpc_help_fail }
function fpc_help_constructor(_self:pointer;var _vmt:pointer;_vmt_pos:cardinal):pointer;[public,alias:'FPC_HELP_CONSTRUCTOR'];compilerproc;
type
  ppointer = ^pointer;
  pvmt = ^tvmt;
  tvmt=packed record
    size,msize:ptruint;
    parent:pointer;
  end;
var
  vmtcopy : pointer;
begin
  { Inherited call? }
  if _vmt=nil then
    begin
      fpc_help_constructor:=_self;
      exit;
    end;
  vmtcopy:=_vmt;

  if (_self=nil) and
     (pvmt(_vmt)^.size>0) then
    begin
      getmem(_self,pvmt(_vmt)^.size);
      { reset vmt needed for fail }
      _vmt:=pointer(-1);
    end;
  if _self<>nil then
    begin
      fillchar(_self^,pvmt(vmtcopy)^.size,#0);
      ppointer(_self+_vmt_pos)^:=vmtcopy;
    end;
  fpc_help_constructor:=_self;
end;
{$endif FPC_SYSTEM_HAS_FPC_HELP_CONSTRUCTOR}


{$ifndef FPC_SYSTEM_HAS_FPC_HELP_DESTRUCTOR}
{ Note: _self will not be reset, the compiler has to generate the reset }
procedure fpc_help_destructor(_self,_vmt:pointer;vmt_pos:cardinal);[public,alias:'FPC_HELP_DESTRUCTOR'];  compilerproc;
type
  ppointer = ^pointer;
  pvmt = ^tvmt;
  tvmt = packed record
    size,msize : ptruint;
    parent : pointer;
  end;
begin
   { already released? }
   if (_self=nil) or
      (_vmt=nil) or
      (ppointer(_self+vmt_pos)^=nil) then
     exit;
   if (pvmt(ppointer(_self+vmt_pos)^)^.size=0) or
      (pvmt(ppointer(_self+vmt_pos)^)^.size+pvmt(ppointer(_self+vmt_pos)^)^.msize<>0) then
     RunError(210);
   { reset vmt to nil for protection }
   ppointer(_self+vmt_pos)^:=nil;
   freemem(_self);
end;
{$endif FPC_SYSTEM_HAS_FPC_HELP_DESTRUCTOR}


{$ifndef FPC_SYSTEM_HAS_FPC_HELP_FAIL}
{ Note: _self will not be reset, the compiler has to generate the reset }
procedure fpc_help_fail(_self:pointer;var _vmt:pointer;vmt_pos:cardinal);[public,alias:'FPC_HELP_FAIL'];compilerproc;
begin
   if (_self=nil) or (_vmt=nil) then
     exit;
   { vmt=$ffffffff when memory was allocated }
   if ptruint(_vmt)=high(ptruint) then
     begin
       if (_self=nil) or (ppointer(_self+vmt_pos)^=nil) then
         HandleError(210)
       else
         begin
           ppointer(_self+vmt_pos)^:=nil;
           freemem(_self);
           { reset _vmt to nil so it will not be freed a
             second time }
           _vmt:=nil;
         end;
     end
   else
     ppointer(_self+vmt_pos)^:=nil;
end;
{$endif FPC_SYSTEM_HAS_FPC_HELP_FAIL}

{$ifndef FPC_SYSTEM_HAS_FPC_CHECK_OBJECT}
procedure fpc_check_object(_vmt : pointer); [public,alias:'FPC_CHECK_OBJECT'];  compilerproc;
type
  pvmt = ^tvmt;
  tvmt = packed record
    size,msize : ptruint;
    parent : pointer;
  end;
begin
  if (_vmt=nil) or
     (pvmt(_vmt)^.size=0) or
     (pvmt(_vmt)^.size+pvmt(_vmt)^.msize<>0) then
    RunError(210);
end;

{$endif ndef FPC_SYSTEM_HAS_FPC_CHECK_OBJECT}


{$ifndef FPC_SYSTEM_HAS_FPC_CHECK_OBJECT_EXT}
{ checks for a correct vmt pointer }
{ deeper check to see if the current object is }
{ really related to the true }
procedure fpc_check_object_ext(vmt, expvmt : pointer); [public,alias:'FPC_CHECK_OBJECT_EXT']; compilerproc;
type
  pvmt = ^tvmt;
  tvmt = packed record
    size,msize : ptruint;
    parent : pointer;
  end;
begin
   if (vmt=nil) or
      (pvmt(vmt)^.size=0) or
      (pvmt(vmt)^.size+pvmt(vmt)^.msize<>0) then
        RunError(210);
   while assigned(vmt) do
     if vmt=expvmt then
       exit
     else
       vmt:=pvmt(vmt)^.parent;
   RunError(219);
end;
{$endif not FPC_SYSTEM_HAS_FPC_CHECK_OBJECT_EXT}


{$endif FPC_HAS_FEATURE_OBJECTS}

{****************************************************************************
                                 String
****************************************************************************}

{$ifndef FPC_SYSTEM_HAS_FPC_SHORTSTR_ASSIGN}

{$ifndef FPC_STRTOSHORTSTRINGPROC}
function fpc_shortstr_to_shortstr(len:longint;const sstr:shortstring): shortstring;[public,alias:'FPC_SHORTSTR_TO_SHORTSTR']; compilerproc;
var
  slen : byte;
begin
  slen:=length(sstr);
  if slen<len then
    len:=slen;
  move(sstr[0],result[0],len+1);
  if slen>len then
    result[0]:=chr(len);
end;
{$else FPC_STRTOSHORTSTRINGPROC}
procedure fpc_shortstr_to_shortstr(out res:shortstring; const sstr: shortstring);[public,alias:'FPC_SHORTSTR_TO_SHORTSTR']; compilerproc;
var
  slen : byte;
begin
  slen:=length(sstr);
  if slen>high(res) then
    slen:=high(res);
  move(sstr[0],res[0],slen+1);
  res[0]:=chr(slen);
end;
{$endif FPC_STRTOSHORTSTRINGPROC}

procedure fpc_shortstr_assign(len:longint;sstr,dstr:pointer);[public,alias:'FPC_SHORTSTR_ASSIGN']; {$ifdef HAS_COMPILER_PROC} compilerproc; {$endif}
var
  slen : byte;
type
  pstring = ^string;
begin
  slen:=length(pstring(sstr)^);
  if slen<len then
    len:=slen;
  move(sstr^,dstr^,len+1);
  if slen>len then
    pchar(dstr)^:=chr(len);
end;

{$endif ndef FPC_SYSTEM_HAS_FPC_SHORTSTR_ASSIGN}

{$ifndef FPC_SYSTEM_HAS_FPC_SHORTSTR_CONCAT}

{$ifndef STR_CONCAT_PROCS}

function fpc_shortstr_concat(const s1,s2:shortstring): shortstring;[public,alias:'FPC_SHORTSTR_CONCAT']; compilerproc;
var
  s1l, s2l : byte;
begin
  s1l:=length(s1);
  s2l:=length(s2);
  if s1l+s2l>255 then
    s2l:=255-s1l;
  move(s1[1],fpc_shortstr_concat[1],s1l);
  move(s2[1],fpc_shortstr_concat[s1l+1],s2l);
  fpc_shortstr_concat[0]:=chr(s1l+s2l);
end;

{$else STR_CONCAT_PROCS}

procedure fpc_shortstr_concat(var dests:shortstring;const s1,s2:shortstring);compilerproc;
var
  s1l, s2l : longint;
begin
  s1l:=length(s1);
  s2l:=length(s2);
  if s1l+s2l>high(dests) then
    s2l:=high(dests)-s1l;
  if @dests=@s1 then
    move(s2[1],dests[s1l+1],s2l)
  else
    if @dests=@s2 then
      begin
        move(dests[1],dests[s1l+1],s2l);
        move(s1[1],dests[1],s1l);
      end
  else
    begin
      move(s1[1],dests[1],s1l);
      move(s2[1],dests[s1l+1],s2l);
    end;
  dests[0]:=chr(s1l+s2l);
end;

procedure fpc_shortstr_concat_multi(var dests:shortstring;const sarr:array of pshortstring);compilerproc;
var
  s2l : byte;
  LowStart,i,
  Len : longint;
  pc : pchar;
  needtemp : boolean;
  tmpstr  : shortstring;
  p,pdest  : pshortstring;
begin
  if high(sarr)=0 then
    begin
      DestS:='';
      exit;
    end;
  lowstart:=low(sarr);
  if Pointer(@DestS)=Pointer(sarr[lowstart]) then
    inc(lowstart);
  { Check for another reuse, then we can't use
    the append optimization and need to use a temp }
  needtemp:=false;
  for i:=lowstart to high(sarr) do
    begin
      if Pointer(@DestS)=Pointer(sarr[i]) then
        begin
          needtemp:=true;
          break;
        end;
    end;
  if needtemp then
    begin
      lowstart:=low(sarr);
      tmpstr:='';
      pdest:=@tmpstr
    end
  else
    begin
      { Start with empty DestS if we start with concatting
        the first array element }
      if lowstart=low(sarr) then
        DestS:='';
      pdest:=@DestS;
    end;
  { Concat all strings, except the string we already
    copied in DestS }
  Len:=length(pdest^);
  pc:=@pdest^[1+Length(pdest^)];
  for i:=lowstart to high(sarr) do
    begin
      p:=sarr[i];
      if assigned(p) then
        begin
          s2l:=length(p^);
          if Len+s2l>high(dests) then
            s2l:=high(dests)-Len;
          Move(p^[1],pc^,s2l);
          inc(pc,s2l);
          inc(Len,s2l);
        end;
    end;
  pdest^[0]:=Chr(Len);
  if needtemp then
    DestS:=TmpStr;
end;

{$endif STR_CONCAT_PROCS}

{$endif ndef FPC_SYSTEM_HAS_FPC_SHORTSTR_CONCAT}


{$ifndef FPC_SYSTEM_HAS_FPC_SHORTSTR_APPEND_SHORTSTR}
procedure fpc_shortstr_append_shortstr(var s1:shortstring;const s2:shortstring);compilerproc;
    [public,alias:'FPC_SHORTSTR_APPEND_SHORTSTR'];
var
  s1l, s2l : integer;
begin
  s1l:=length(s1);
  s2l:=length(s2);
  if s1l+s2l>high(s1) then
    s2l:=high(s1)-s1l;
  move(s2[1],s1[s1l+1],s2l);
  s1[0]:=chr(s1l+s2l);
end;
{$endif ndef FPC_SYSTEM_HAS_FPC_SHORTSTR_APPEND_SHORTSTR}


{$ifndef FPC_SYSTEM_HAS_FPC_SHORTSTR_COMPARE}
function fpc_shortstr_compare(const left,right:shortstring) : longint;[public,alias:'FPC_SHORTSTR_COMPARE']; compilerproc;
var
   s1,s2,max,i : byte;
   d : longint;
begin
  s1:=length(left);
  s2:=length(right);
  if s1<s2 then
    max:=s1
  else
    max:=s2;
  for i:=1 to max do
    begin
     d:=byte(left[i])-byte(right[i]);
     if d>0 then
       exit(1)
     else if d<0 then
       exit(-1);
    end;
  if s1>s2 then
    exit(1)
  else if s1<s2 then
    exit(-1)
  else
    exit(0);
end;
{$endif ndef FPC_SYSTEM_HAS_FPC_SHORTSTR_COMPARE}


{$ifndef FPC_SYSTEM_HAS_FPC_SHORTSTR_COMPARE_EQUAL}
function fpc_shortstr_compare_equal(const left,right:shortstring): longint; [public,alias:'FPC_SHORTSTR_COMPARE_EQUAL']; compilerproc;
begin
  Result := longint(left[0]) - longint(right[0]);
  if Result = 0 then
    Result := CompareByte(left[1],right[1], longint(left[0]));
end;
{$endif ndef FPC_SYSTEM_HAS_FPC_SHORTSTR_COMPARE_EQUAL}


{$ifndef FPC_SYSTEM_HAS_FPC_PCHAR_TO_SHORTSTR}

{$ifndef FPC_STRTOSHORTSTRINGPROC}
function fpc_pchar_to_shortstr(p:pchar):shortstring;[public,alias:'FPC_PCHAR_TO_SHORTSTR']; compilerproc;
var
  l : longint;
  s: shortstring;
begin
  if p=nil then
    l:=0
  else
    l:=strlen(p);
  if l>255 then
    l:=255;
  if l>0 then
    move(p^,s[1],l);
  s[0]:=chr(l);
  fpc_pchar_to_shortstr := s;
end;

{$else FPC_STRTOSHORTSTRINGPROC}

procedure fpc_pchar_to_shortstr(out res : shortstring;p:pchar);[public,alias:'FPC_PCHAR_TO_SHORTSTR']; compilerproc;
var
  l : longint;
  s: shortstring;
begin
  if p=nil then
    l:=0
  else
    l:=strlen(p);
  if l>high(res) then
    l:=high(res);
  if l>0 then
    move(p^,s[1],l);
  s[0]:=chr(l);
  res:=s;
end;

{$endif FPC_STRTOSHORTSTRINGPROC}

{$endif ndef FPC_SYSTEM_HAS_FPC_PCHAR_TO_SHORTSTR}


{$ifndef FPC_STRTOSHORTSTRINGPROC}

{ also define alias which can be used inside the system unit }
function fpc_pchar_to_shortstr(p:pchar):shortstring;[external name 'FPC_PCHAR_TO_SHORTSTR'];

{$else FPC_STRTOSHORTSTRINGPROC}

{ also define alias which can be used inside the system unit }
procedure fpc_pchar_to_shortstr(out res : shortstring;p:pchar);[external name 'FPC_PCHAR_TO_SHORTSTR'];

{$endif FPC_STRTOSHORTSTRINGPROC}

function strpas(p:pchar):shortstring;{$ifdef SYSTEMINLINE}inline;{$endif}
  begin
{$ifndef FPC_STRTOSHORTSTRINGPROC}
    result:=fpc_pchar_to_shortstr(p);
{$else FPC_STRTOSHORTSTRINGPROC}
    fpc_pchar_to_shortstr(result,p);
{$endif FPC_STRTOSHORTSTRINGPROC}
  end;

{$ifndef FPC_SYSTEM_HAS_FPC_CHARARRAY_TO_SHORTSTR}

{$ifndef FPC_STRTOSHORTSTRINGPROC}
function fpc_chararray_to_shortstr(const arr: array of char; zerobased: boolean = true):shortstring;[public,alias:'FPC_CHARARRAY_TO_SHORTSTR']; compilerproc;
var
 l: longint;
 index: longint;
 len: byte;
begin
  l := high(arr)+1;
  if l>=256 then
    l:=255
  else if l<0 then
    l:=0;
  if (zerobased) then
    begin
      index:=IndexByte(arr[0],l,0);
      if (index < 0) then
        len := l
      else
        len := index;
    end
  else
    len := l;
  move(arr[0],fpc_chararray_to_shortstr[1],len);
  fpc_chararray_to_shortstr[0]:=chr(len);
end;
{$else FPC_STRTOSHORTSTRINGPROC}
procedure fpc_chararray_to_shortstr(out res : shortstring;const arr: array of char; zerobased: boolean = true);[public,alias:'FPC_CHARARRAY_TO_SHORTSTR']; compilerproc;
var
 l: longint;
 index: longint;
 len: byte;
begin
  l:=high(arr)+1;
  if l>=high(res)+1 then
    l:=high(res)
  else if l<0 then
    l:=0;
  if zerobased then
    begin
      index:=IndexByte(arr[0],l,0);
      if index<0 then
        len:=l
      else
        len:=index;
    end
  else
    len:=l;
  move(arr[0],res[1],len);
  res[0]:=chr(len);
end;
{$endif FPC_STRTOSHORTSTRINGPROC}

{$endif ndef FPC_SYSTEM_HAS_FPC_CHARARRAY_TO_SHORTSTR}


{$ifndef FPC_SYSTEM_HAS_FPC_SHORTSTR_TO_CHARARRAY}

{$ifndef FPC_STRTOCHARARRAYPROC}

{ inside the compiler, the resulttype is modified to that of the actual }
{ chararray we're converting to (JM)                                    }
function fpc_shortstr_to_chararray(arraysize: longint; const src: ShortString): fpc_big_chararray;[public,alias: 'FPC_SHORTSTR_TO_CHARARRAY']; compilerproc;
var
  len: longint;
begin
  len := length(src);
  if len > arraysize then
    len := arraysize;
{$r-}
  { make sure we don't access char 1 if length is 0 (JM) }
  if len > 0 then
    move(src[1],fpc_shortstr_to_chararray[0],len);
  fillchar(fpc_shortstr_to_chararray[len],arraysize-len,0);
{$ifdef RangeCheckWasOn}
{$r+}
{$endif}
end;

{$else ndef FPC_STRTOCHARARRAYPROC}

procedure fpc_shortstr_to_chararray(out res: array of char; const src: ShortString); compilerproc;
var
  len: longint;
begin
  len := length(src);
  if len > length(res) then
    len := length(res);
{$r-}
  { make sure we don't access char 1 if length is 0 (JM) }
  if len > 0 then
    move(src[1],res[0],len);
  fillchar(res[len],length(res)-len,0);
{$ifdef RangeCheckWasOn}
{$r+}
{$endif}
end;

{$endif ndef FPC_STRTOCHARARRAYPROC}

{$endif FPC_SYSTEM_HAS_FPC_SHORTSTR_TO_CHARARRAY}

{$ifndef FPC_SYSTEM_HAS_FPC_PCHAR_LENGTH}

function fpc_pchar_length(p:pchar):longint;[public,alias:'FPC_PCHAR_LENGTH']; compilerproc;
var i : longint;
begin
  i:=0;
  if assigned(p) then
    while p[i]<>#0 do
      inc(i);
  exit(i);
end;

{$endif ndef FPC_SYSTEM_HAS_FPC_PCHAR_LENGTH}

{$ifndef FPC_SYSTEM_HAS_FPC_PWIDECHAR_LENGTH}

function fpc_pwidechar_length(p:pwidechar):longint;[public,alias:'FPC_PWIDECHAR_LENGTH']; compilerproc;
var i : longint;
begin
  i:=0;
  while p[i]<>#0 do inc(i);
  exit(i);
end;

{$endif ndef FPC_SYSTEM_HAS_FPC_PWIDECHAR_LENGTH}

{****************************************************************************
                       Caller/StackFrame Helpers
****************************************************************************}

{$ifndef FPC_SYSTEM_HAS_GET_FRAME}
{_$error Get_frame must be defined for each processor }
{$endif ndef FPC_SYSTEM_HAS_GET_FRAME}

{$ifndef FPC_SYSTEM_HAS_GET_CALLER_ADDR}
{_$error Get_caller_addr must be defined for each processor }
{$endif ndef FPC_SYSTEM_HAS_GET_CALLER_ADDR}

{$ifndef FPC_SYSTEM_HAS_GET_CALLER_FRAME}
{_$error Get_caller_frame must be defined for each processor }
{$endif ndef FPC_SYSTEM_HAS_GET_CALLER_FRAME}

{****************************************************************************
                                 Math
****************************************************************************}

{****************************************************************************
                          Software longint/dword division
****************************************************************************}
{$ifdef FPC_INCLUDE_SOFTWARE_MOD_DIV}

function count_leading_zeros_32bit(l : longint) : longint;
  var
    i : longint;
  begin
     for i:=0 to 31 do
       begin
          if (l and (longint($80000000) shr i))<>0 then
            begin
               result:=i;
               exit;
            end;
       end;
     result:=i;
  end;


{$ifndef FPC_SYSTEM_HAS_DIV_DWORD}
function fpc_div_dword(n,z : dword) : dword; [public,alias: 'FPC_DIV_DWORD']; compilerproc;
  var
     shift,lzz,lzn : longint;
  begin
     result:=0;
     if n=0 then
       HandleErrorFrame(200,get_frame);
     lzz:=count_leading_zeros_32bit(z);
     lzn:=count_leading_zeros_32bit(n);
     { if the denominator contains less zeros
       then the numerator
       the d is greater than the n }
     if lzn<lzz then
       exit;
     shift:=lzn-lzz;
     n:=n shl shift;
     repeat
       if z>=n then
         begin
            z:=z-n;
            result:=result+dword(1 shl shift);
         end;
       dec(shift);
       n:=n shr 1;
     until shift<0;
  end;
{$endif FPC_SYSTEM_HAS_DIV_DWORD}


{$ifndef FPC_SYSTEM_HAS_MOD_DWORD}
function fpc_mod_dword(n,z : dword) : dword; [public,alias: 'FPC_MOD_DWORD']; compilerproc;
  var
     shift,lzz,lzn : longint;
  begin
    result:=0;
    if n=0 then
      HandleErrorFrame(200,get_frame);
    lzz:=count_leading_zeros_32bit(z);
    lzn:=count_leading_zeros_32bit(n);
    { if the denominator contains less zeros
      then the numerator
      the d is greater than the n }
    if lzn<lzz then
      begin
         result:=z;
         exit;
      end;
    shift:=lzn-lzz;
    n:=n shl shift;
    repeat
      if z>=n then
        z:=z-n;
      dec(shift);
      n:=n shr 1;
    until shift<0;
    result:=z;
  end;
{$endif FPC_SYSTEM_HAS_MOD_DWORD}


{$ifndef FPC_SYSTEM_HAS_DIV_LONGINT}
function fpc_div_longint(n,z : longint) : longint; [public,alias: 'FPC_DIV_LONGINT']; compilerproc;
  var
     sign : boolean;
     d1,d2 : dword;
  begin
     if n=0 then
       HandleErrorFrame(200,get_frame);
     sign:=false;
     if z<0 then
       begin
          sign:=not(sign);
          d1:=dword(-z);
       end
     else
       d1:=z;
     if n<0 then
       begin
          sign:=not(sign);
          d2:=dword(-n);
       end
     else
       d2:=n;

     { the div is coded by the compiler as call to divdword }
     if sign then
       result:=-(d1 div d2)
     else
       result:=d1 div d2;
  end;
{$endif FPC_SYSTEM_HAS_DIV_LONGINT}


{$ifndef FPC_SYSTEM_HAS_MOD_LONGINT}
function fpc_mod_longint(n,z : longint) : longint; [public,alias: 'FPC_MOD_LONGINT']; compilerproc;
  var
     signed : boolean;
     r,nq,zq : dword;
  begin
     if n=0 then
       HandleErrorFrame(200,get_frame);
     nq:=abs(n);

     if z<0 then
       begin
          zq:=dword(-z);
          signed:=true;
       end
     else
       begin
         zq:=z;
         signed:=false;
       end;

     r:=zq mod nq;
     if signed then
       result:=-longint(r)
     else
       result:=r;
  end;
{$endif FPC_SYSTEM_HAS_MOD_LONGINT}

{$endif FPC_INCLUDE_SOFTWARE_MOD_DIV}


{****************************************************************************}

{$ifndef FPC_SYSTEM_HAS_ABS_LONGINT}
function abs(l:longint):longint;{$ifdef SYSTEMINLINE}inline;{$endif}
begin
   if l<0 then
     abs:=-l
   else
     abs:=l;
end;

{$endif not FPC_SYSTEM_HAS_ABS_LONGINT}

{$ifndef FPC_SYSTEM_HAS_ODD_LONGINT}

function odd(l:longint):boolean;{$ifdef SYSTEMINLINE}inline;{$endif}
begin
   odd:=boolean(l and 1);
end;

{$endif ndef FPC_SYSTEM_HAS_ODD_LONGINT}

{$ifndef FPC_SYSTEM_HAS_ODD_LONGWORD}

function odd(l:longword):boolean;{$ifdef SYSTEMINLINE}inline;{$endif}
begin
   odd:=boolean(l and 1);
end;

{$endif ndef FPC_SYSTEM_HAS_ODD_LONGWORD}


{$ifndef FPC_SYSTEM_HAS_ODD_INT64}

function odd(l:int64):boolean;{$ifdef SYSTEMINLINE}inline;{$endif}
begin
   odd:=boolean(longint(l) and 1);
end;

{$endif ndef FPC_SYSTEM_HAS_ODD_INT64}

{$ifndef FPC_SYSTEM_HAS_ODD_QWORD}

function odd(l:qword):boolean;{$ifdef SYSTEMINLINE}inline;{$endif}
begin
   odd:=boolean(longint(l) and 1);
end;

{$endif ndef FPC_SYSTEM_HAS_ODD_QWORD}

{$ifndef FPC_SYSTEM_HAS_SQR_LONGINT}

function sqr(l:longint):longint;{$ifdef SYSTEMINLINE}inline;{$endif}
begin
   sqr:=l*l;
end;

{$endif ndef FPC_SYSTEM_HAS_SQR_LONGINT}


{$ifndef FPC_SYSTEM_HAS_ABS_INT64}

function abs(l: Int64): Int64;{$ifdef SYSTEMINLINE}inline;{$endif}
begin
  if l < 0 then
    abs := -l
  else
    abs := l;
end;

{$endif ndef FPC_SYSTEM_HAS_ABS_INT64}


{$ifndef FPC_SYSTEM_HAS_SQR_INT64}

function sqr(l: Int64): Int64;{$ifdef SYSTEMINLINE}inline;{$endif}
begin
  sqr := l*l;
end;

{$endif ndef FPC_SYSTEM_HAS_SQR_INT64}


{$ifndef FPC_SYSTEM_HAS_SQR_QWORD}

function sqr(l: QWord): QWord;{$ifdef SYSTEMINLINE}inline;{$endif}
begin
  sqr := l*l;
end;

{$endif ndef FPC_SYSTEM_HAS_SQR_INT64}

{$ifdef CPU16}
{$ifndef FPC_SYSTEM_HAS_DECLOCKED_SMALLINT}
function declocked(var l:smallint):boolean;
  begin
    Dec(l);
    declocked:=(l=0);
  end;
{$endif FPC_SYSTEM_HAS_DECLOCKED_SMALLINT}
{$endif CPU16}

{$ifndef FPC_SYSTEM_HAS_DECLOCKED_LONGINT}
function declocked(var l:longint):boolean;
  begin
    Dec(l);
    declocked:=(l=0);
  end;
{$endif FPC_SYSTEM_HAS_DECLOCKED_LONGINT}


{$ifndef FPC_SYSTEM_HAS_DECLOCKED_INT64}
function declocked(var l:int64):boolean;
  begin
    Dec(l);
    declocked:=(l=0);
  end;
{$endif FPC_SYSTEM_HAS_DECLOCKED_INT64}


{$ifdef CPU16}
{$ifndef FPC_SYSTEM_HAS_INCLOCKED_SMALLINT}
procedure inclocked(var l:smallint);
  begin
    Inc(l);
  end;
{$endif FPC_SYSTEM_HAS_INCLOCKED_SMALLINT}
{$endif CPU16}

{$ifndef FPC_SYSTEM_HAS_INCLOCKED_LONGINT}
procedure inclocked(var l:longint);
  begin
    Inc(l);
  end;
{$endif FPC_SYSTEM_HAS_INCLOCKED_LONGINT}


{$ifndef FPC_SYSTEM_HAS_INCLOCKED_INT64}
procedure inclocked(var l:int64);
  begin
    Inc(l);
  end;
{$endif FPC_SYSTEM_HAS_INCLOCKED_INT64}


{$ifndef FPC_SYSTEM_HAS_SPTR}
{_$error Sptr must be defined for each processor }
{$endif ndef FPC_SYSTEM_HAS_SPTR}



function align(addr : PtrUInt;alignment : PtrUInt) : PtrUInt;{$ifdef SYSTEMINLINE}inline;{$endif}
  var
    tmp: PtrUInt;
  begin
    tmp:=addr+alignment-1;
    result:=tmp-(tmp mod alignment)
  end;


function align(addr : Pointer;alignment : PtrUInt) : Pointer;{$ifdef SYSTEMINLINE}inline;{$endif}
  var
    tmp: PtrUInt;
  begin
    tmp:=PtrUInt(addr)+alignment-1;
    result:=pointer(tmp-(tmp mod alignment));
  end;


{****************************************************************************
                                 Str()
****************************************************************************}

{$ifndef FPC_SYSTEM_HAS_INT_STR_LONGINT}

procedure int_str(l:longint;out s:string);
var
  m,m1 : longword;
  pcstart,
  pc2start,
  pc,pc2 : pchar;
  hs : string[32];
  overflow : longint;
begin
  pc2start:=@s[1];
  pc2:=pc2start;
  if (l<0) then
    begin
      pc2^:='-';
      inc(pc2);
      m:=longword(-l);
    end
  else
    m:=longword(l);
  pcstart:=pchar(@hs[0]);
  pc:=pcstart;
  repeat
    m1:=m div 10;
    inc(pc);
    pc^:=char(m-(m1*10)+byte('0'));
    m:=m1;
  until m=0;
  overflow:=(pc-pcstart)+(pc2-pc2start)-high(s);
  if overflow>0 then
    inc(pcstart,overflow);
  while (pc>pcstart) do
    begin
      pc2^:=pc^;
      inc(pc2);
      dec(pc);
    end;
  s[0]:=char(pc2-pc2start);
end;

{$endif ndef FPC_SYSTEM_HAS_INT_STR_LONGINT}

{$ifndef FPC_SYSTEM_HAS_INT_STR_LONGWORD}

procedure int_str(l:longword;out s:string);
var
  m1 : longword;
  pcstart,
  pc2start,
  pc,pc2 : pchar;
  hs : string[32];
  overflow : longint;
begin
  pc2start:=@s[1];
  pc2:=pc2start;
  pcstart:=pchar(@hs[0]);
  pc:=pcstart;
  repeat
    inc(pc);
    m1:=l div 10;
    pc^:=char(l-(m1*10)+byte('0'));
    l:=m1;
  until l=0;
  overflow:=(pc-pcstart)-high(s);
  if overflow>0 then
    inc(pcstart,overflow);
  while (pc>pcstart) do
    begin
      pc2^:=pc^;
      inc(pc2);
      dec(pc);
    end;
  s[0]:=char(pc2-pc2start);
end;

{$endif ndef FPC_SYSTEM_HAS_INT_STR_LONGWORD}

{$ifndef FPC_SYSTEM_HAS_INT_STR_INT64}

procedure int_str(l:int64;out s:string);
var
  m,m1 : qword;
  pcstart,
  pc2start,
  pc,pc2 : pchar;
  hs : string[32];
  overflow : longint;
begin
  pc2start:=@s[1];
  pc2:=pc2start;
  if (l<0) then
    begin
      pc2^:='-';
      inc(pc2);
      m:=qword(-l);
    end
  else
    m:=qword(l);
  pcstart:=pchar(@hs[0]);
  pc:=pcstart;
  repeat
    m1:=m div 10;
    inc(pc);
    pc^:=char(m-(m1*10)+byte('0'));
    m:=m1;
  until m=0;
  overflow:=(pc-pcstart)+(pc2-pc2start)-high(s);
  if overflow>0 then
    inc(pcstart,overflow);
  while (pc>pcstart) do
    begin
      pc2^:=pc^;
      inc(pc2);
      dec(pc);
    end;
  s[0]:=char(pc2-pc2start);
end;

{$endif ndef FPC_SYSTEM_HAS_INT_STR_INT64}

{$ifndef FPC_SYSTEM_HAS_INT_STR_QWORD}

procedure int_str(l:qword;out s:string);
var
  m1 : qword;
  pcstart,
  pc2start,
  pc,pc2 : pchar;
  hs : string[64];
  overflow : longint;
begin
  pc2start:=@s[1];
  pc2:=pc2start;
  pcstart:=pchar(@hs[0]);
  pc:=pcstart;
  repeat
    inc(pc);
    m1:=l div 10;
    pc^:=char(l-(m1*10)+byte('0'));
    l:=m1;
  until l=0;
  overflow:=(pc-pcstart)-high(s);
  if overflow>0 then
    inc(pcstart,overflow);
  while (pc>pcstart) do
    begin
      pc2^:=pc^;
      inc(pc2);
      dec(pc);
    end;
  s[0]:=char(pc2-pc2start);
end;

{$endif ndef FPC_SYSTEM_HAS_INT_STR_QWORD}

{$ifndef FPUNONE}
{$ifndef FPC_SYSTEM_HAS_SYSRESETFPU}

procedure SysResetFpu;{$ifdef SYSTEMINLINE}inline;{$endif}
begin
  softfloat_exception_flags:=0;
end;

{$endif FPC_SYSTEM_HAS_SYSRESETFPU}

{$ifndef FPC_SYSTEM_HAS_SYSINITFPU}

procedure SysInitFpu;{$ifdef SYSTEMINLINE}inline;{$endif}
begin
  softfloat_exception_mask:=float_flag_underflow or float_flag_inexact or float_flag_denormal;
end;

{$endif FPC_SYSTEM_HAS_SYSINITFPU}
{$endif}

{$ifndef FPC_SYSTEM_HAS_SWAPENDIAN}
function SwapEndian(const AValue: SmallInt): SmallInt;
  begin
    { the extra Word type cast is necessary because the "AValue shr 8" }
    { is turned into "longint(AValue) shr 8", so if AValue < 0 then    }
    { the sign bits from the upper 16 bits are shifted in rather than  }
    { zeroes.                                                          }
    Result := SmallInt((Word(AValue) shr 8) or (Word(AValue) shl 8));
  end;


function SwapEndian(const AValue: Word): Word;
  begin
    Result := Word((AValue shr 8) or (AValue shl 8));
  end;


function SwapEndian(const AValue: LongInt): LongInt;
  begin
    Result := (AValue shl 24)
           or ((AValue and $0000FF00) shl 8)
           or ((AValue and $00FF0000) shr 8)
           or (AValue shr 24);
  end;


function SwapEndian(const AValue: DWord): DWord;
  begin
    Result := (AValue shl 24)
           or ((AValue and $0000FF00) shl 8)
           or ((AValue and $00FF0000) shr 8)
           or (AValue shr 24);
  end;


function SwapEndian(const AValue: Int64): Int64;
  begin
    Result := (AValue shl 56)
           or ((AValue and $000000000000FF00) shl 40)
           or ((AValue and $0000000000FF0000) shl 24)
           or ((AValue and $00000000FF000000) shl 8)
           or ((AValue and $000000FF00000000) shr 8)
           or ((AValue and $0000FF0000000000) shr 24)
           or ((AValue and $00FF000000000000) shr 40)
           or (AValue shr 56);
  end;


function SwapEndian(const AValue: QWord): QWord;
  begin
    Result := (AValue shl 56)
           or ((AValue and $000000000000FF00) shl 40)
           or ((AValue and $0000000000FF0000) shl 24)
           or ((AValue and $00000000FF000000) shl 8)
           or ((AValue and $000000FF00000000) shr 8)
           or ((AValue and $0000FF0000000000) shr 24)
           or ((AValue and $00FF000000000000) shr 40)
           or (AValue shr 56);
  end;
{$endif FPC_SYSTEM_HAS_SWAPENDIAN}

function BEtoN(const AValue: SmallInt): SmallInt;{$ifdef SYSTEMINLINE}inline;{$endif}
  begin
    {$IFDEF ENDIAN_BIG}
      Result := AValue;
    {$ELSE}
      Result := SwapEndian(AValue);
    {$ENDIF}
  end;


function BEtoN(const AValue: Word): Word;{$ifdef SYSTEMINLINE}inline;{$endif}
  begin
    {$IFDEF ENDIAN_BIG}
      Result := AValue;
    {$ELSE}
      Result := SwapEndian(AValue);
    {$ENDIF}
  end;


function BEtoN(const AValue: LongInt): LongInt;{$ifdef SYSTEMINLINE}inline;{$endif}
  begin
    {$IFDEF ENDIAN_BIG}
      Result := AValue;
    {$ELSE}
      Result := SwapEndian(AValue);
    {$ENDIF}
  end;


function BEtoN(const AValue: DWord): DWord;{$ifdef SYSTEMINLINE}inline;{$endif}
  begin
    {$IFDEF ENDIAN_BIG}
      Result := AValue;
    {$ELSE}
      Result := SwapEndian(AValue);
    {$ENDIF}
  end;


function BEtoN(const AValue: Int64): Int64;{$ifdef SYSTEMINLINE}inline;{$endif}
  begin
    {$IFDEF ENDIAN_BIG}
      Result := AValue;
    {$ELSE}
      Result := SwapEndian(AValue);
    {$ENDIF}
  end;


function BEtoN(const AValue: QWord): QWord;{$ifdef SYSTEMINLINE}inline;{$endif}
  begin
    {$IFDEF ENDIAN_BIG}
      Result := AValue;
    {$ELSE}
      Result := SwapEndian(AValue);
    {$ENDIF}
  end;



function LEtoN(const AValue: SmallInt): SmallInt;{$ifdef SYSTEMINLINE}inline;{$endif}
  begin
    {$IFDEF ENDIAN_LITTLE}
      Result := AValue;
    {$ELSE}
      Result := SwapEndian(AValue);
    {$ENDIF}
  end;


function LEtoN(const AValue: Word): Word;{$ifdef SYSTEMINLINE}inline;{$endif}
  begin
    {$IFDEF ENDIAN_LITTLE}
      Result := AValue;
    {$ELSE}
      Result := SwapEndian(AValue);
    {$ENDIF}
  end;


function LEtoN(const AValue: LongInt): LongInt;{$ifdef SYSTEMINLINE}inline;{$endif}
  begin
    {$IFDEF ENDIAN_LITTLE}
      Result := AValue;
    {$ELSE}
      Result := SwapEndian(AValue);
    {$ENDIF}
  end;


function LEtoN(const AValue: DWord): DWord;{$ifdef SYSTEMINLINE}inline;{$endif}
  begin
    {$IFDEF ENDIAN_LITTLE}
      Result := AValue;
    {$ELSE}
      Result := SwapEndian(AValue);
    {$ENDIF}
  end;


function LEtoN(const AValue: Int64): Int64;{$ifdef SYSTEMINLINE}inline;{$endif}
  begin
    {$IFDEF ENDIAN_LITTLE}
      Result := AValue;
    {$ELSE}
      Result := SwapEndian(AValue);
    {$ENDIF}
  end;


function LEtoN(const AValue: QWord): QWord;{$ifdef SYSTEMINLINE}inline;{$endif}
  begin
    {$IFDEF ENDIAN_LITTLE}
      Result := AValue;
    {$ELSE}
      Result := SwapEndian(AValue);
    {$ENDIF}
  end;



function NtoBE(const AValue: SmallInt): SmallInt;{$ifdef SYSTEMINLINE}inline;{$endif}
  begin
    {$IFDEF ENDIAN_BIG}
      Result := AValue;
    {$ELSE}
      Result := SwapEndian(AValue);
    {$ENDIF}
  end;


function NtoBE(const AValue: Word): Word;{$ifdef SYSTEMINLINE}inline;{$endif}
  begin
    {$IFDEF ENDIAN_BIG}
      Result := AValue;
    {$ELSE}
      Result := SwapEndian(AValue);
    {$ENDIF}
  end;


function NtoBE(const AValue: LongInt): LongInt;{$ifdef SYSTEMINLINE}inline;{$endif}
  begin
    {$IFDEF ENDIAN_BIG}
      Result := AValue;
    {$ELSE}
      Result := SwapEndian(AValue);
    {$ENDIF}
  end;


function NtoBE(const AValue: DWord): DWord;{$ifdef SYSTEMINLINE}inline;{$endif}
  begin
    {$IFDEF ENDIAN_BIG}
      Result := AValue;
    {$ELSE}
      Result := SwapEndian(AValue);
    {$ENDIF}
  end;


function NtoBE(const AValue: Int64): Int64;{$ifdef SYSTEMINLINE}inline;{$endif}
  begin
    {$IFDEF ENDIAN_BIG}
      Result := AValue;
    {$ELSE}
      Result := SwapEndian(AValue);
    {$ENDIF}
  end;


function NtoBE(const AValue: QWord): QWord;{$ifdef SYSTEMINLINE}inline;{$endif}
  begin
    {$IFDEF ENDIAN_BIG}
      Result := AValue;
    {$ELSE}
      Result := SwapEndian(AValue);
    {$ENDIF}
  end;


function NtoLE(const AValue: SmallInt): SmallInt;{$ifdef SYSTEMINLINE}inline;{$endif}
  begin
    {$IFDEF ENDIAN_LITTLE}
      Result := AValue;
    {$ELSE}
      Result := SwapEndian(AValue);
    {$ENDIF}
  end;


function NtoLE(const AValue: Word): Word;{$ifdef SYSTEMINLINE}inline;{$endif}
  begin
    {$IFDEF ENDIAN_LITTLE}
      Result := AValue;
    {$ELSE}
      Result := SwapEndian(AValue);
    {$ENDIF}
  end;


function NtoLE(const AValue: LongInt): LongInt;{$ifdef SYSTEMINLINE}inline;{$endif}
  begin
    {$IFDEF ENDIAN_LITTLE}
      Result := AValue;
    {$ELSE}
      Result := SwapEndian(AValue);
    {$ENDIF}
  end;


function NtoLE(const AValue: DWord): DWord;{$ifdef SYSTEMINLINE}inline;{$endif}
  begin
    {$IFDEF ENDIAN_LITTLE}
      Result := AValue;
    {$ELSE}
      Result := SwapEndian(AValue);
    {$ENDIF}
  end;


function NtoLE(const AValue: Int64): Int64;{$ifdef SYSTEMINLINE}inline;{$endif}
  begin
    {$IFDEF ENDIAN_LITTLE}
      Result := AValue;
    {$ELSE}
      Result := SwapEndian(AValue);
    {$ENDIF}
  end;


function NtoLE(const AValue: QWord): QWord;{$ifdef SYSTEMINLINE}inline;{$endif}
  begin
    {$IFDEF ENDIAN_LITTLE}
      Result := AValue;
    {$ELSE}
      Result := SwapEndian(AValue);
    {$ENDIF}
  end;

{$ifndef FPC_SYSTEM_HAS_MEM_BARRIER}

procedure ReadBarrier;{$ifdef SYSTEMINLINE}inline;{$endif}
begin
end;

procedure ReadDependencyBarrier;{$ifdef SYSTEMINLINE}inline;{$endif}
begin
end;

procedure ReadWriteBarrier;{$ifdef SYSTEMINLINE}inline;{$endif}
begin
end;

procedure WriteBarrier;{$ifdef SYSTEMINLINE}inline;{$endif}
begin
end;

{$endif FPC_SYSTEM_HAS_MEM_BARRIER}

{$ifndef FPC_HAS_INTERNAL_ROX_BYTE}
{$ifndef FPC_SYSTEM_HAS_ROX_BYTE}

function Ror(Const AValue : Byte): Byte;{$ifdef SYSTEMINLINE}inline;{$endif}
  begin
    Result:=(AValue shr 1) or (AValue shl 7);
  end;
  

function Ror(Const AValue : Byte;Dist : Byte): Byte;{$ifdef SYSTEMINLINE}inline;{$endif}
  begin
    Dist:=Dist and 7;
    Result:=(AValue shr Dist) or (AValue shl (8-Dist));
  end;


function Rol(Const AValue : Byte): Byte;{$ifdef SYSTEMINLINE}inline;{$endif}
  begin
    Result:=(AValue shl 1) or (AValue shr 7);
  end;


function Rol(Const AValue : Byte;Dist : Byte): Byte;{$ifdef SYSTEMINLINE}inline;{$endif}
  begin
    Dist:=Dist and 7;
    Result:=(AValue shl Dist) or (AValue shr (8-Dist));
  end;

{$endif FPC_SYSTEM_HAS_ROX_BYTE}
{$endif FPC_HAS_INTERNAL_ROX_BYTE}

{$ifndef FPC_HAS_INTERNAL_ROX_WORD}
{$ifndef FPC_SYSTEM_HAS_ROX_WORD}

function Ror(Const AValue : Word): Word;{$ifdef SYSTEMINLINE}inline;{$endif}
  begin
    Result:=(AValue shr 1) or (AValue shl 15);
  end;

  
function Ror(Const AValue : Word;Dist : Byte): Word;{$ifdef SYSTEMINLINE}inline;{$endif}
  begin
    Dist:=Dist and 15;
    Result:=(AValue shr Dist) or (AValue shl (16-Dist));
  end;


function Rol(Const AValue : Word): Word;{$ifdef SYSTEMINLINE}inline;{$endif}
  begin
    Result:=(AValue shl 1) or (AValue shr 15);
  end;


function Rol(Const AValue : Word;Dist : Byte): Word;{$ifdef SYSTEMINLINE}inline;{$endif}
  begin
    Dist:=Dist and 15;
    Result:=(AValue shl Dist) or (AValue shr (16-Dist));
  end;

{$endif FPC_SYSTEM_HAS_ROX_WORD}
{$endif FPC_HAS_INTERNAL_ROX_WORD}

{$ifndef FPC_HAS_INTERNAL_ROX_DWORD}
{$ifndef FPC_SYSTEM_HAS_ROX_DWORD}

function Ror(Const AValue : DWord): DWord;{$ifdef SYSTEMINLINE}inline;{$endif}
  begin
    Result:=(AValue shr 1) or (AValue shl 31);
  end;


function Ror(Const AValue : DWord;Dist : Byte): DWord;{$ifdef SYSTEMINLINE}inline;{$endif}
  begin
    Dist:=Dist and 31;
    Result:=(AValue shr Dist) or (AValue shl (32-Dist));
  end;


function Rol(Const AValue : DWord): DWord;{$ifdef SYSTEMINLINE}inline;{$endif}
  begin
    Result:=(AValue shl 1) or (AValue shr 31);
  end;


function Rol(Const AValue : DWord;Dist : Byte): DWord;{$ifdef SYSTEMINLINE}inline;{$endif}
  begin
    Dist:=Dist and 31;
    Result:=(AValue shl Dist) or (AValue shr (32-Dist));
  end;

{$endif FPC_SYSTEM_HAS_ROX_DWORD}
{$endif FPC_HAS_INTERNAL_ROX_DWORD}

{$ifndef FPC_HAS_INTERNAL_ROX_QWORD}
{$ifndef FPC_SYSTEM_HAS_ROX_QWORD}

function Ror(Const AValue : QWord): QWord;{$ifdef SYSTEMINLINE}inline;{$endif}
  begin
    Result:=(AValue shr 1) or (AValue shl 63);
  end;


function Ror(Const AValue : QWord;Dist : Byte): QWord;{$ifdef SYSTEMINLINE}inline;{$endif}
  begin
    Dist:=Dist and 63;
    Result:=(AValue shr Dist) or (AValue shl (64-Dist));
  end;


function Rol(Const AValue : QWord): QWord;{$ifdef SYSTEMINLINE}inline;{$endif}
  begin
    Result:=(AValue shl 1) or (AValue shr 63);
  end;


function Rol(Const AValue : QWord;Dist : Byte): QWord;{$ifdef SYSTEMINLINE}inline;{$endif}
  begin
    Dist:=Dist and 63;
    Result:=(AValue shl Dist) or (AValue shr (64-Dist));
  end;

{$endif FPC_SYSTEM_HAS_ROX_QWORD}
{$endif FPC_HAS_INTERNAL_ROX_QWORD}

