{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Michael Van Canneyt,
    member of the Free Pascal development team.

    This file implements AnsiStrings for FPC

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ This will release some functions for special shortstring support }
{ define EXTRAANSISHORT}


{$ifndef FPC_ANSISTRING_TYPE_DEFINED}
{$define FPC_ANSISTRING_TYPE_DEFINED}
{
  This file contains the implementation of the AnsiString type,
  and all things that are needed for it.
  AnsiString is defined as a 'silent' pansichar :
  a pansichar that points to (S= SizeOf(SizeInt), R= (if CPU64 then SizeOf(Longint) else SizeOf(SizeInt))):

  @-S-R-4 : Code page indicator.
  @-S-R-2 : Character size (2 bytes)
  @-S-R   : Reference count (R bytes)
  @-S  : SizeInt for size;
  @    : String + Terminating #0;
  PAnsiChar(Ansistring) is a valid typecast.
  So AS[i] is converted to the address @AS+i-1.

  Constants should be assigned a reference count of -1
  Meaning that they can't be disposed of.
}

Type
  PAnsiRec = ^TAnsiRec;
  TAnsiRec = Record
    CodePage    : TSystemCodePage;
    ElementSize : Word;
{$if not defined(VER3_2)}
  {$ifdef CPU64}	
    Ref         : Longint;
  {$else}
    Ref         : SizeInt;
  {$endif}
{$else}
  {$ifdef CPU64}	
    { align fields  }
	Dummy       : DWord;
  {$endif CPU64}
    Ref         : SizeInt;
{$endif}
    Len         : SizeInt;
  end;

Const
  AnsiFirstOff = SizeOf(TAnsiRec);
{$endif FPC_ANSISTRING_TYPE_DEFINED}

{****************************************************************************
                    Internal functions, not in interface.
****************************************************************************}

{$ifndef FPC_HAS_TRANSLATEPLACEHOLDERCP}
{$define FPC_HAS_TRANSLATEPLACEHOLDERCP}
function TranslatePlaceholderCP(cp: TSystemCodePage): TSystemCodePage; {$ifdef SYSTEMINLINE}inline;{$endif}
begin
  TranslatePlaceholderCP:=cp;
  case cp of
    CP_OEMCP,
    CP_ACP:
      TranslatePlaceholderCP:=DefaultSystemCodePage;
  end;
end;
{$endif FPC_HAS_TRANSLATEPLACEHOLDERCP}


{$ifndef FPC_HAS_PCHAR_ANSISTR_INTERN_CHARMOVE}
{$define FPC_HAS_PCHAR_ANSISTR_INTERN_CHARMOVE}
procedure fpc_pchar_ansistr_intern_charmove(const src: pansichar; const srcindex: sizeint; var dst: rawbytestring; const dstindex, len: sizeint); rtlproc;{$ifdef SYSTEMINLINE}inline;{$endif}
begin
  move(src[srcindex],pbyte(pointer(dst))[dstindex],len);
end;
{$endif FPC_HAS_PCHAR_ANSISTR_INTERN_CHARMOVE}


{$ifndef FPC_HAS_PCHAR_PCHAR_INTERN_CHARMOVE}
{$define FPC_HAS_PCHAR_PCHAR_INTERN_CHARMOVE}
procedure fpc_pchar_pchar_intern_charmove(const src: pansichar; const srcindex: sizeint; const dst: pansichar; const dstindex, len: sizeint); rtlproc; {$ifdef SYSTEMINLINE}inline;{$endif}
begin
  move(src[srcindex],dst[dstindex],len);
end;
{$endif FPC_HAS_PCHAR_PCHAR_INTERN_CHARMOVE}


{$ifndef FPC_HAS_SHORTSTR_ANSISTR_INTERN_CHARMOVE}
{$define FPC_HAS_SHORTSTR_ANSISTR_INTERN_CHARMOVE}
procedure fpc_shortstr_ansistr_intern_charmove(const src: shortstring; const srcindex: sizeint; var dst: rawbytestring; const dstindex, len: sizeint); rtlproc; {$ifdef SYSTEMINLINE}inline;{$endif}
begin
  move(src[srcindex],pbyte(pointer(dst))[dstindex],len);
end;
{$endif FPC_HAS_SHORTSTR_ANSISTR_INTERN_CHARMOVE}



{$ifndef FPC_SYSTEM_HAS_ANSISTR_DECR_REF}
{$define FPC_SYSTEM_HAS_ANSISTR_DECR_REF}
Procedure fpc_ansistr_decr_ref (Var S : Pointer); [Public,Alias:'FPC_ANSISTR_DECR_REF'];  compilerproc;
{
  Decreases the ReferenceCount of a non constant ansistring;
  If the reference count is zero, deallocate the string;
}
Var
  p: pointer;
Begin
  p:=S;
  If p=Nil then
    exit;
  s:=nil;
  If (PAnsiRec(p-AnsiFirstOff)^.ref=1) or { Shortcut declocked on ref = 1. }
     (PAnsiRec(p-AnsiFirstOff)^.ref>0) { ref = -1 is constant string. }
     and declocked(PAnsiRec(p-AnsiFirstOff)^.ref) then
    FreeMem(p-AnsiFirstOff);
end;
{$endif FPC_SYSTEM_HAS_ANSISTR_DECR_REF}

{ also define alias for internal use in the system unit }
Procedure fpc_ansistr_decr_ref (Var S : Pointer); [external name 'FPC_ANSISTR_DECR_REF'];


{$ifndef FPC_SYSTEM_HAS_ANSISTR_INCR_REF}
{$define FPC_SYSTEM_HAS_ANSISTR_INCR_REF}
Procedure fpc_AnsiStr_Incr_Ref (S : Pointer); [Public,Alias:'FPC_ANSISTR_INCR_REF'];  compilerproc; inline;
Begin
  If (S<>Nil) and (PAnsiRec(S-AnsiFirstOff)^.Ref>0) then
    inclocked(PAnsiRec(S-AnsiFirstOff)^.Ref);
end;
{$endif FPC_SYSTEM_HAS_ANSISTR_DECR_REF}

{ also define alias which can be used inside the system unit }
Procedure fpc_AnsiStr_Incr_Ref (S : Pointer); [external name 'FPC_ANSISTR_INCR_REF'];


{$ifndef FPC_HAS_ANSISTR_ASSIGN}
{$define FPC_HAS_ANSISTR_ASSIGN}
Procedure fpc_AnsiStr_Assign (Var DestS : Pointer;S2 : Pointer);[Public,Alias:'FPC_ANSISTR_ASSIGN'];  compilerproc;
{
  Assigns S2 to S1 (S1:=S2), taking in account reference counts.
}
var
  old : Pointer;
begin
  old:=DestS;
  if old=S2 then
    exit;
  DestS:=S2; { Doing this early frees the DestS register. }
  { Inlined incr_ref(S2). }
  If (S2<>nil) and (PAnsiRec(S2-AnsiFirstOff)^.Ref>0) then
    inclocked(PAnsiRec(S2-AnsiFirstOff)^.Ref);
  { Inlined decr_ref(old). }
  If (old<>nil) and
     ((PAnsiRec(old-AnsiFirstOff)^.ref=1) or { Shortcut declocked on ref = 1. }
      (PAnsiRec(old-AnsiFirstOff)^.ref>0) { ref = -1 is constant string. }
      and declocked(PAnsiRec(old-AnsiFirstOff)^.ref)) then
    FreeMem(old-AnsiFirstOff);
end;
{$endif FPC_HAS_ANSISTR_ASSIGN}


{ alias for internal use }
Procedure fpc_AnsiStr_Assign (Var S1 : Pointer;S2 : Pointer);[external name 'FPC_ANSISTR_ASSIGN'];

{$ifndef FPC_HAS_ANSISTR_CONCAT_COMPLEX}
{$define FPC_HAS_ANSISTR_CONCAT_COMPLEX}
{ keeps implicit try..finally block out from primary control flow }
procedure ansistr_concat_complex(var DestS: RawByteString; const S1,S2: RawByteString; cp: TSystemCodePage);
var
  U: UnicodeString;
begin
  U:=UnicodeString(S1)+UnicodeString(S2);
  widestringmanager.Unicode2AnsiMoveProc(PUnicodeChar(Pointer(U)),DestS,cp,Length(U));
end;
{$endif FPC_HAS_ANSISTR_CONCAT_COMPLEX}

{$ifndef FPC_HAS_ANSISTR_CONCAT}
{$define FPC_HAS_ANSISTR_CONCAT}
procedure fpc_AnsiStr_Concat (var DestS:RawByteString;const S1,S2 : RawByteString;cp : TSystemCodePage); compilerproc;
Var
  S1Len, S2Len: SizeInt;
  S1CP, S2CP, DestCP: TSystemCodePage;
  OldDestP, NewDestP, RealDestP, Src : Pointer;
begin
  DestCP:=cp;
  if DestCp=CP_NONE then
    DestCP:=DefaultSystemCodePage;
  DestCP:=TranslatePlaceholderCP(DestCP);
  if (Pointer(S1)=nil) or (Pointer(S2)=nil) then
    begin
      Src:=Pointer(S1);
      if Src=nil then
        Src:=Pointer(S2); { Src = the only nonempty string, if any. }
      DestS:=RawByteString(Src); { ...And maybe convert :) }
      if (Src<>nil) and (cp<>CP_NONE) then
        begin
          S1CP:=PAnsiRec(Src-AnsiFirstOff)^.CodePage;
          if (DestCP<>S1CP) and (cp<>S1CP) and { Attempt to skip TranslatePlaceholderCP. }
            (DestCP<>TranslatePlaceholderCP(S1CP)) then
            SetCodePage(DestS,DestCP,True);
        end;
      exit;
    end;
  S1CP:=PAnsiRec(Pointer(S1)-AnsiFirstOff)^.CodePage;
  S2CP:=PAnsiRec(Pointer(S2)-AnsiFirstOff)^.CodePage;
  { Attempt to skip TranslatePlaceholderCPs: codepages are usually already equal to cp or DestCP. }
  if (DestCP<>S1CP) and (cp<>S1CP) or (cp<>S2CP) and (DestCP<>S2CP) then
    begin
      S1CP:=TranslatePlaceholderCP(S1CP);
      { if codepages are different then concat using unicodestring;
        but if the result is rawbytestring and both strings have the same code page, keep that code page }
      if (S1CP<>TranslatePlaceholderCP(S2CP)) or (cp<>CP_NONE) and (DestCP<>S1CP) then
        begin
          ansistr_concat_complex(DestS,S1,S2,DestCP);
          exit;
        end;
      if cp=CP_NONE then
        DestCP:=S1CP;
    end;
  S1Len:=PAnsiRec(Pointer(S1)-AnsiFirstOff)^.Len;
  S2Len:=PAnsiRec(Pointer(S2)-AnsiFirstOff)^.Len;
  OldDestP:=Pointer(DestS);
  { Reallocate when possible; in the hope this will reuse the chunk more often than do a redundant copy. }
  if Assigned(OldDestP) and (PAnsiRec(OldDestP-AnsiFirstOff)^.Ref=1) then
    begin
      RealDestP:=OldDestP-AnsiFirstOff;
      NewDestP:=ReallocMem(RealDestP,AnsiFirstOff+1+S1Len+S2Len)+AnsiFirstOff;
      { Copy S2 first, as in the case of OldDestP = Pointer(S2) it must be copied first and in other cases the order does not matter. }
      Src:=Pointer(S2);
      if Src=OldDestP then
        Src:=NewDestP;
      Move(Src^,PAnsiChar(NewDestP)[S1Len],S2Len);
      if OldDestP<>Pointer(S1) then { Not an append, need to copy S1? }
        Move(Pointer(S1)^,NewDestP^,S1Len);
    end
  else
    begin
      NewDestP:=GetMem(S1Len+S2Len+(AnsiFirstOff+1))+AnsiFirstOff;
      PAnsiRec(NewDestP-AnsiFirstOff)^.ElementSize:=1;
      PAnsiRec(NewDestP-AnsiFirstOff)^.Ref:=1;
      Move(Pointer(S1)^,NewDestP^,S1Len);
      Move(Pointer(S2)^,PAnsiChar(NewDestP)[S1Len],S2Len);
      fpc_ansistr_decr_ref(Pointer(DestS));
    end;
  PAnsiChar(NewDestP)[S1Len+S2Len]:=#0;
  PAnsiRec(NewDestP-AnsiFirstOff)^.CodePage:=DestCP;
  PAnsiRec(NewDestP-AnsiFirstOff)^.Len:=S1Len+S2Len;
  Pointer(DestS):=NewDestP;
end;
{$endif FPC_HAS_ANSISTR_CONCAT}


{$ifndef FPC_HAS_ANSISTR_CONCAT_MULTI}
{$define FPC_HAS_ANSISTR_CONCAT_MULTI}
procedure AnsiStr_Concat_multi_complex(var DestS:RawByteString;const sarr:array of RawByteString;cp:TSystemCodePage);
var
  i : ObjpasInt;
  U : UnicodeString;
begin
  U:='';
  for i:=0 to high(sarr) do
    if (Length(sarr[i])<>0) then
      U:=U+UnicodeString(sarr[i]);
  DestS:='';
  widestringmanager.Unicode2AnsiMoveProc(PUnicodeChar(Pointer(U)),DestS,cp,Length(U));
end;

procedure fpc_AnsiStr_Concat_multi (var DestS:RawByteString;const sarr:array of RawByteString;cp : TSystemCodePage); compilerproc;
Var
  lowstart,i,Size,NewLen : SizeInt;
  p,pc,olddestp,newdestp,realdestp : pointer;
  DestCP,tmpCP,tmpCPuntrans,ithCP : TSystemCodePage;
begin
  DestCP:=cp;
  if DestCp=CP_NONE then
    DestCP:=DefaultSystemCodePage;
  lowstart:=low(sarr);
  { skip empty strings }
  while (lowstart<=high(sarr)) and
        (Length(sarr[lowstart])=0) do
    inc(lowstart);
  if lowstart>high(sarr) then
    begin
      DestS:=''; { All source strings empty }
      exit;
    end;
  tmpCPuntrans:=DestCP;
  DestCP:=TranslatePlaceholderCP(DestCP);
  tmpCP:=DestCP;
  NewLen:=0;
  for i:=lowstart to high(sarr) do
    begin
      p:=pointer(sarr[i]);
      if not assigned(p) then
        continue;
      inc(NewLen,PAnsiRec(p-AnsiFirstOff)^.Len);
      ithCP:=PAnsiRec(p-AnsiFirstOff)^.CodePage;
      if (ithCP=tmpCP) or (ithCP=tmpCPuntrans) then { Attempt to skip TranslatePlaceholderCP. }
        continue;
      ithCP:=TranslatePlaceholderCP(ithCP);
      if ithCP=tmpCP then
        continue;
      { On the first iteration, tmpCP = DestCP (to reuse the checks above), and mismatches just adjust tmpCP instead of falling back to concat_multi_complex.
        Without further mismatches, the concatenated contents will be converted to DestCP by the final SetCodePage. }
      if i<>lowstart then
        begin
          if cp=CP_NONE then
            DestCP:=DefaultSystemCodePage; { Revert “DestCP:=tmpCP” below (no-op if it was never performed). }
          AnsiStr_Concat_multi_complex(DestS,sarr,DestCP);
          exit;
        end;
      tmpCPuntrans:=ithCP; { Isn’t really “untrans(lated)” from now on, just neutralizes the “ithCP=tmpCPuntrans” check. }
      tmpCP:=ithCP;
      { if the result is rawbytestring and all strings have the same code page, keep that code page.
        Gets reverted back to DefaultSystemCodePage in the concat_multi_complex branch. (This way the check is kept out of the common path.) }
      if cp=CP_NONE then
        DestCP:=tmpCP;
    end;
  { In the case of the only nonempty string, either return it directly (if SetCodePage has nothing to do) or skip 1 allocation. }
  if NewLen=PAnsiRec(Pointer(sarr[lowstart])-AnsiFirstOff)^.Len then
    DestS:=sarr[lowstart]
  else
    begin
      olddestp:=pointer(dests);
      { Reallocate when possible; in the hope this will reuse the chunk more often than do a redundant copy. }
      if assigned(olddestp) and (PAnsiRec(olddestp-AnsiFirstOff)^.Ref=1) then
        begin
          realdestp:=olddestp-AnsiFirstOff;
          newdestp:=ReallocMem(realdestp,AnsiFirstOff+1+NewLen)+AnsiFirstOff;
          { First string can be skipped if appending. }
          if olddestp=pointer(sarr[lowstart]) then
            inc(lowstart);
        end
      else
        begin
          { Create new string. }
          olddestp:=nil; { This case is distinguished as "not assigned(olddestp)". Also prevents "if p=olddestp" in the loop below shared with the ReallocMem branch. }
          newdestp:=GetMem(NewLen+(AnsiFirstOff+1))+AnsiFirstOff;
          PAnsiRec(newdestp-AnsiFirstOff)^.ElementSize:=1;
          PAnsiRec(newdestp-AnsiFirstOff)^.Ref:=1;
        end;
      { Copy strings from last to the first, so that possible occurences of DestS could read from the beginning of the reallocated DestS. }
      pc:=newdestp+NewLen;
      PByte(pc)^:=0; { Conveniently write null terminator. }
      for i:=high(sarr) downto lowstart do
        begin
          p:=pointer(sarr[i]);
          if not assigned(p) then
            continue;
          if p=olddestp then
            { DestS occured among pieces in the ReallocMem case! Use the new pointer. Its header still conveniently contains old DestS length. }
            p:=newdestp;
          Size:=PAnsiRec(p-AnsiFirstOff)^.Len;
          dec(pc,size);
          Move(p^,pc^,Size);
        end;
      PAnsiRec(newdestp-AnsiFirstOff)^.CodePage:=tmpCP;
      PAnsiRec(newdestp-AnsiFirstOff)^.Len:=NewLen; { Careful, loop above relies on the old Len in the newdestp header. }
      if not assigned(olddestp) then
        fpc_AnsiStr_Decr_Ref(pointer(DestS));
      Pointer(DestS):=newdestp;
    end;
  { SetCodePage does the conversion (or at least uniquifying) if DestCP is not exactly the code page stored in the string header. Avoid if possible. }
  if DestCP<>tmpCP then
    SetCodePage(DestS,DestCP,True);
end;
{$endif FPC_HAS_ANSISTR_CONCAT_MULTI}

{$ifdef EXTRAANSISHORT}
Procedure fpc_AnsiStr_ShortStr_Concat (Var S1: AnsiString; Var S2 : ShortString);
{
  Concatenates a Ansi with a short string; : S2 + S2
}
Var
  Size,Location : SizeInt;
begin
  Size:=Length(S2);
  Location:=Length(S1);
  If Size=0 then
    exit;
  { Setlength takes case of uniqueness
    and alllocated memory. We need to use length,
    to take into account possibility of S1=Nil }
  SetLength (S1,Size+Length(S1));
  Move (S2[1],Pointer(Pointer(S1)+Location)^,Size);
  PByte( Pointer(S1)+length(S1) )^:=0; { Terminating Zero }
end;
{$endif EXTRAANSISHORT}

{$ifndef FPC_HAS_ANSISTR_TO_ANSISTR}
{$define FPC_HAS_ANSISTR_TO_ANSISTR}
Function fpc_AnsiStr_To_AnsiStr (const S : RawByteString;cp : TSystemCodePage): RawByteString; [Public, alias: 'FPC_ANSISTR_TO_ANSISTR']; compilerproc;
{
  Converts an AnsiString to an AnsiString taking code pages into care
}
Var
  Size : SizeInt;
  temp : UnicodeString;
  orgcp: TSystemCodePage;
begin
  result:='';
  Size:=Length(S);
  if Size>0 then
    begin
      cp:=TranslatePlaceholderCP(cp);
      orgcp:=TranslatePlaceholderCP(StringCodePage(S));
      if (orgcp=cp) or (orgcp=CP_NONE) then
        begin
          SetLength(result,Size);
          Move(S[1],result[1],Size);
          PAnsiRec(Pointer(result)-AnsiFirstOff)^.CodePage:=cp;
        end
      else
        begin
          temp:=UnicodeString(S);
          Size:=Length(temp);
          widestringmanager.Unicode2AnsiMoveProc(PUnicodeChar(Pointer(temp)),result,cp,Size);
        end;
    end;
end;

Function fpc_AnsiStr_To_AnsiStr (const S : RawByteString;cp : TSystemCodePage): RawByteString; [external name 'FPC_ANSISTR_TO_ANSISTR'];
{$endif FPC_HAS_ANSISTR_TO_ANSISTR}

{$ifndef FPC_HAS_ANSISTR_TO_SHORTSTR}
{$define FPC_HAS_ANSISTR_TO_SHORTSTR}
procedure fpc_AnsiStr_To_ShortStr (out res: shortstring; const S2 : RawByteString);[Public, alias: 'FPC_ANSISTR_TO_SHORTSTR'];  compilerproc;
{
  Converts a AnsiString to a ShortString;
}
Var
  Size : SizeInt;
begin
  if Length(S2)=0 then
   res:=''
  else
   begin
     Size:=Length(S2);
     If Size>high(res) then
      Size:=high(res);
     byte(res[0]):=byte(Size);
     Move (S2[1],res[1],Size);
   end;
end;
{$endif FPC_HAS_ANSISTR_TO_SHORTSTR}



{$ifndef FPC_HAS_SHORTSTR_TO_ANSISTR}
{$define FPC_HAS_SHORTSTR_TO_ANSISTR}
Function fpc_ShortStr_To_AnsiStr (Const S2 : ShortString;cp : TSystemCodePage): RawByteString; compilerproc;
{
  Converts a ShortString to a AnsiString;
}
Var
  Size : SizeInt;
begin
  Size:=Length(S2);
  Setlength(fpc_ShortStr_To_AnsiStr,Size);
  if Size>0 then
    begin
      fpc_shortstr_ansistr_intern_charmove(S2,1,fpc_ShortStr_To_AnsiStr,0,Size);
      SetCodePage(fpc_ShortStr_To_AnsiStr,TranslatePlaceholderCP(cp),False);
    end
end;
{$endif FPC_HAS_SHORTSTR_TO_ANSISTR}


{$ifndef FPC_HAS_CHAR_TO_ANSISTR}
{$define FPC_HAS_CHAR_TO_ANSISTR}
Function fpc_Char_To_AnsiStr(const c : AnsiChar;cp : TSystemCodePage): RawByteString; compilerproc;
{
  Converts a AnsiChar to a AnsiString;
}
begin
  Setlength (fpc_Char_To_AnsiStr,1);
  PAnsiChar(fpc_Char_To_AnsiStr)^:=c;
  { Terminating Zero already set by SetLength above }
  SetCodePage(fpc_Char_To_AnsiStr,TranslatePlaceholderCP(cp),False);
end;
{$endif FPC_HAS_CHAR_TO_ANSISTR}


{$ifndef FPC_HAS_PCHAR_TO_ANSISTR}
{$define FPC_HAS_PCHAR_TO_ANSISTR}
Function fpc_PChar_To_AnsiStr(const p : PAnsiChar;cp : TSystemCodePage): RawByteString; compilerproc;
Var
  L : SizeInt;
begin
  if (not assigned(p)) or (p[0]=#0) Then
    L := 0
  else
    L:=IndexChar(p^,-1,#0);
  SetLength(fpc_PChar_To_AnsiStr,L);
  if L > 0 then
    begin
      fpc_pchar_ansistr_intern_charmove(p,0,fpc_PChar_To_AnsiStr,0,L);
      SetCodePage(fpc_PChar_To_AnsiStr,TranslatePlaceholderCP(cp),False);
    end;
end;
{$endif FPC_HAS_PCHAR_TO_ANSISTR}


{$ifndef FPC_HAS_CHARARRAY_TO_ANSISTR}
{$define FPC_HAS_CHARARRAY_TO_ANSISTR}
Function fpc_CharArray_To_AnsiStr(const arr: array of ansichar; cp : TSystemCodePage;zerobased: boolean = true): RawByteString; compilerproc;
var
  i  : SizeInt;
begin
  if (zerobased) then
    begin
      if (arr[0]=#0) Then
        i := 0
      else
      begin
        i:=IndexChar(arr,high(arr)+1,#0);
        if i = -1 then
          i := high(arr)+1;
      end;
    end
  else
    i := high(arr)+1;
  SetLength(fpc_CharArray_To_AnsiStr,i);
  if i > 0 then
    begin
      fpc_pchar_ansistr_intern_charmove(pansichar(@arr),0,fpc_CharArray_To_AnsiStr,0,i);
      SetCodePage(fpc_CharArray_To_AnsiStr,TranslatePlaceholderCP(cp),False);
    end;
end;
{$endif FPC_HAS_ANSISTR_TO_CHARARRAY}


{$ifndef FPC_HAS_ANSISTR_TO_CHARARRAY}
{$define FPC_HAS_ANSISTR_TO_CHARARRAY}
procedure  fpc_ansistr_to_chararray(out res: array of AnsiChar; const src: RawByteString); compilerproc;
var
  len: SizeInt;
begin
  len := length(src);
  if len > length(res) then
    len := length(res);
{$push}{$r-}
  { make sure we don't try to access element 1 of the ansistring if it's nil }
  if len > 0 then
    move(src[1],res[0],len);
  fillchar(res[len],length(res)-len,0);
{$pop}
end;
{$endif FPC_HAS_ANSISTR_TO_CHARARRAY}


{$ifndef FPC_HAS_ANSISTR_COMPARE}
{$define FPC_HAS_ANSISTR_COMPARE}

Function fpc_utf8_Compare(const S1,S2 : RawByteString): SizeInt;

var
  r1,r2 : RawByteString;
begin
  r1:=S1;
  r2:=S2;
  //convert them to utf8 then compare
  SetCodePage(r1,65001);
  SetCodePage(r2,65001);
  Result:=fpc_AnsiStr_Compare(r1,r2);
end;

Function fpc_AnsiStr_Compare(const S1,S2 : RawByteString): SizeInt;[Public,Alias : 'FPC_ANSISTR_COMPARE'];  compilerproc;
{
  Compares 2 AnsiStrings;
  The result is
   <0 if S1<S2
   0 if S1=S2
   >0 if S1>S2
}
Var
  Len1,Len2,CmpLen : SizeInt;
begin
  if (pointer(S1)=pointer(S2)) or (pointer(S1)=nil) or (pointer(S2)=nil) then
    exit(ord(pointer(S1)<>nil)-ord(pointer(S2)<>nil));
  if TranslatePlaceholderCP(PAnsiRec(Pointer(S1)-AnsiFirstOff)^.CodePage)<>TranslatePlaceholderCP(PAnsiRec(Pointer(S2)-AnsiFirstOff)^.CodePage) then
    exit(fpc_utf8_compare(s1,s2));
  Len1:=PAnsiRec(Pointer(S1)-AnsiFirstOff)^.Len;
  Len2:=PAnsiRec(Pointer(S2)-AnsiFirstOff)^.Len;
  CmpLen:=Len1;
  If Len1>Len2 then
    CmpLen:=Len2;
  result:=CompareByte(S1[1],S2[1],CmpLen);
  if result=0 then
    result:=Len1-Len2;
end;
{$endif FPC_HAS_ANSISTR_COMPARE}


{$ifndef FPC_HAS_ANSISTR_COMPARE_EQUAL}
{$define FPC_HAS_ANSISTR_COMPARE_EQUAL}

Function fpc_utf8_Compare_equal(Const S1,S2 : RawByteString): SizeInt;

Var
  r1,r2 : RawByteString;
  L1,L2 : SizeInt;
begin
  r1:=S1;
  r2:=S2;
  //convert them to utf8 then compare
  SetCodePage(r1,65001);
  SetCodePage(r2,65001);
  L1:=Length(r1);
  L2:=Length(r2);
  Result:=L1-L2;
  if Result = 0 then
    if L1>0 then
      result:=CompareByte(r1[1],r2[1],L1);
end;

Function fpc_AnsiStr_Compare_equal(const S1,S2 : RawByteString): SizeInt;[Public,Alias : 'FPC_ANSISTR_COMPARE_EQUAL'];  compilerproc;
{
  Compares 2 AnsiStrings for equality/inequality only;
  The result is
   0 if S1=S2
   <>0 if S1<>S2
}
Var
  MaxI : SizeInt;
begin
  { don't compare strings if one of them is empty }
  if (pointer(S1)=pointer(S2)) or (pointer(S1)=nil) or (pointer(S2)=nil) then
    Exit(ord(pointer(S1)<>pointer(S2)));
  if TranslatePlaceholderCP(PAnsiRec(Pointer(S1)-AnsiFirstOff)^.CodePage)<>TranslatePlaceholderCP(PAnsiRec(Pointer(S2)-AnsiFirstOff)^.CodePage) then
    Exit(fpc_utf8_Compare_equal(S1,S2));
  Maxi:=PAnsiRec(Pointer(S1)-AnsiFirstOff)^.Len;
  Result:=Maxi-PAnsiRec(Pointer(S2)-AnsiFirstOff)^.Len;
  if Result=0 then
    result:=CompareByte(S1[1],S2[1],MaxI);
end;
{$endif FPC_HAS_ANSISTR_COMPARE_EQUAL}

{$ifndef FPC_HAS_ANSISTR_RANGECHECK}
{$define FPC_HAS_ANSISTR_RANGECHECK}
Procedure fpc_AnsiStr_RangeCheck(p: Pointer; index: SizeInt);[Public,Alias : 'FPC_ANSISTR_RANGECHECK'];  compilerproc;
begin
  if (p=nil) or (index>PAnsiRec(p-AnsiFirstOff)^.Len) or (Index<1) then
    HandleErrorAddrFrameInd(201,get_pc_addr,get_frame);
end;

Procedure fpc_AnsiStr_ZeroBased_RangeCheck(p: Pointer; index: SizeInt);[Public,Alias : 'FPC_ANSISTR_ZEROBASED_RANGECHECK'];  compilerproc;
begin
  if (p=nil) or (index>=PAnsiRec(p-AnsiFirstOff)^.Len) or (Index<0) then
    HandleErrorAddrFrameInd(201,get_pc_addr,get_frame);
end;
{$endif FPC_HAS_ANSISTR_RANGECHECK}

{$ifndef FPC_HAS_ANSISTR_SETLENGTH}
{$define FPC_HAS_ANSISTR_SETLENGTH}
Procedure fpc_AnsiStr_SetLength (Var S : RawByteString; l : SizeInt;cp : TSystemCodePage);[Public,Alias : 'FPC_ANSISTR_SETLENGTH'];  compilerproc;
{
  Sets The length of string S to L.
  Makes sure S is unique, and contains enough room.
}
Var
  sp,oldsp,realsp : Pointer;
  lens, lena : SizeInt;
begin
  if l<=0 then { length=0, deallocate the string }
    begin
      fpc_ansistr_decr_ref (Pointer(S));
      exit;
    end;
  sp:=Pointer(S);
  if (sp<>nil) and (PAnsiRec(sp-AnsiFirstOff)^.Ref=1) then
    begin
      lens:=MemSize(sp-AnsiFirstOff);
      lena:=L+(AnsiFirstOff+1);
      { allow shrinking string if that saves at least half of current size }
      if (lena>lens) or (lena+32<=SizeInt(SizeUint(lens) div 2)) then
        begin
          realsp:=sp-AnsiFirstOff; { Avoid taking sp address. }
          sp:=reallocmem(realsp,lena)+AnsiFirstOff;
        end;
    end
  else
    begin
      { Reallocation is needed... }
      oldsp:=sp;
      sp:=GetMem(L+(AnsiFirstOff+1))+AnsiFirstOff;
      PAnsiRec(sp-AnsiFirstOff)^.ElementSize:=1;
      PAnsiRec(sp-AnsiFirstOff)^.Ref:=1;
      if oldsp=nil then
        PAnsiRec(sp-AnsiFirstOff)^.CodePage:=TranslatePlaceholderCP(cp)
      else
        begin
          PAnsiRec(sp-AnsiFirstOff)^.CodePage:=PAnsiRec(oldsp-AnsiFirstOff)^.CodePage;
          lens:=PAnsiRec(oldsp-AnsiFirstOff)^.Len;
          if l<lens then
            lens:=l;
          Move(oldsp^,sp^,lens);
          fpc_ansistr_decr_ref(Pointer(s));
        end;
    end;
  { Null-terminate. }
  PByte(sp+l)^:=0;
  PAnsiRec(sp-AnsiFirstOff)^.Len:=l;
  Pointer(S):=sp;
end;
{$endif FPC_HAS_ANSISTR_SETLENGTH}


{$ifdef EXTRAANSISHORT}
Function fpc_AnsiStr_ShortStr_Compare (Var S1 : Pointer; Var S2 : ShortString): SizeInt;  compilerproc;
{
  Compares a AnsiString with a ShortString;
  The result is
   <0 if S1<S2
   0 if S1=S2
   >0 if S1>S2
}
Var
  i,MaxI,Temp : SizeInt;
begin
  Temp:=0;
  i:=0;
  MaxI:=Length(AnsiString(S1));
  if MaxI>byte(S2[0]) then
    MaxI:=Byte(S2[0]);
  While (i<MaxI) and (Temp=0) do
   begin
     Temp:= PByte(S1+I)^ - Byte(S2[i+1]);
     inc(i);
   end;
  AnsiStr_ShortStr_Compare:=Temp;
end;
{$endif EXTRAANSISHORT}


{*****************************************************************************
                     Public functions, In interface.
*****************************************************************************}

{$ifndef FPC_SYSTEM_HAS_TRUELY_ANSISTR_UNIQUE}
{$define FPC_SYSTEM_HAS_TRUELY_ANSISTR_UNIQUE}
function fpc_truely_ansistr_unique(Var S : Pointer): Pointer;
Var
  Sp : Pointer;
  FullSize : SizeInt;
begin
  Sp:=S;
  FullSize:=PAnsiRec(Sp-AnsiFirstOff)^.len+(AnsiFirstOff+1);
  result:=GetMem(FullSize)+AnsiFirstOff;
  Move ((Sp-AnsiFirstOff)^,(result-AnsiFirstOff)^,FullSize); { Copy everything including header and #0, only refcount needs to be adjusted. }
  PAnsiRec(result-AnsiFirstOff)^.Ref:=1;
  fpc_ansistr_decr_ref (S);  { Thread safe }
  S:=result;
end;
{$endif FPC_SYSTEM_HAS_TRUELY_ANSISTR_UNIQUE}


Function fpc_ansistr_Unique_func(Var S : RawByteString): Pointer; external name 'FPC_ANSISTR_UNIQUE';


Procedure UniqueString(var S : RawByteString);rtlproc;{$ifdef SYSTEMINLINE}inline;{$endif}
  begin
    fpc_ansistr_Unique_func(S);
  end;


{$ifndef FPC_SYSTEM_HAS_ANSISTR_UNIQUE}
{$define FPC_SYSTEM_HAS_ANSISTR_UNIQUE}
// MV: inline the basic checks for case that S is already unique.
// Rest is too complex to inline, so factor that out as a call.
Function fpc_ansistr_Unique(Var S : Pointer): Pointer; [Public,Alias : 'FPC_ANSISTR_UNIQUE']; compilerproc; inline;
{
  Make sure reference count of S is 1,
  using copy-on-write semantics.
}
begin
  pointer(result) := pointer(s);
  if (result<>Nil) and (PAnsiRec(result-AnsiFirstOff)^.Ref<>1) then
    result:=fpc_truely_ansistr_unique(s);
end;
{$endif FPC_SYSTEM_HAS_ANSISTR_UNIQUE}

{$ifndef FPC_HAS_ANSISTR_COPY}
{$define FPC_HAS_ANSISTR_COPY}
Function Fpc_Ansistr_Copy(Const S : RawByteString; Index,Size : SizeInt): RawByteString;compilerproc;
var
  Lim : SizeInt;
  ResultAddress,PhysResultAddress : Pointer;
begin
  if Index < 1 then
    Index := 1;
  dec(index);
  Lim:=Length(S)-Index; { Cannot overflow as both Length(S) and Index are non-negative. }
  if Size>Lim then
    Size:=Lim;
  if (Size>0) and Assigned(Pointer(Result)) and (PAnsiRec(Pointer(Result)-AnsiFirstOff)^.Ref=1) then
   begin
     PhysResultAddress:=Pointer(Result)-AnsiFirstOff;
     ResultAddress:=ReallocMem(PhysResultAddress,Size+(AnsiFirstOff+1))+AnsiFirstOff;
   end
  else
   begin
     fpc_ansistr_decr_ref(Pointer(Result));
     if Size<=0 then
       exit;
     ResultAddress:=GetMem(Size+(AnsiFirstOff+1))+AnsiFirstOff;
   end;
  Pointer(Result):=ResultAddress;
  PAnsiRec(ResultAddress-AnsiFirstOff)^.CodePage:=PAnsiRec(Pointer(S)-AnsiFirstOff)^.CodePage;
  PAnsiRec(ResultAddress-AnsiFirstOff)^.ElementSize:=1;
  PAnsiRec(ResultAddress-AnsiFirstOff)^.Ref:=1;
  PAnsiRec(ResultAddress-AnsiFirstOff)^.Len:=Size;
  Move(Pointer(Pointer(S)+index)^,ResultAddress^,Size);
  PByte(ResultAddress)[Size]:=0;
end;
{$endif FPC_HAS_ANSISTR_COPY}


{$ifndef FPC_HAS_POS_SHORTSTR_ANSISTR}
{$define FPC_HAS_POS_SHORTSTR_ANSISTR}
Function Pos(Const Substr : ShortString; Const Source : RawByteString; Offset : Sizeint = 1) : SizeInt;
var
  nsource : SizeInt;
begin
  nsource:=Length(Source);
  result:=0;
  dec(Offset);
  if SizeUint(Offset)<SizeUint(nsource) then { (Offset >= 0) and (Offset < nsource) }
    result:=MemPos(PByte(@Substr[1]),length(Substr),PByte(Source)+Offset,nsource-Offset)+1;
  if result>0 then
    inc(result,Offset);
end;
{$endif FPC_HAS_POS_SHORTSTR_ANSISTR}


{$ifndef FPC_HAS_POS_ANSISTR_ANSISTR}
{$define FPC_HAS_POS_ANSISTR_ANSISTR}
Function Pos(Const Substr : RawByteString; Const Source : RawByteString; Offset : Sizeint = 1) : SizeInt;
var
  nsource : SizeInt;
begin
  nsource:=Length(Source);
  result:=0;
  dec(Offset);
  if SizeUint(Offset)<SizeUint(nsource) then { (Offset >= 0) and (Offset < nsource) }
    result:=MemPos(PByte(Substr),length(Substr),PByte(Source)+Offset,nsource-Offset)+1;
  if result>0 then
    inc(result,Offset);
end;
{$endif FPC_HAS_POS_ANSISTR_ANSISTR}


{$ifndef FPC_HAS_POS_ANSICHAR_ANSISTR}
{$define FPC_HAS_POS_ANSICHAR_ANSISTR}
{ Faster version for a AnsiChar alone. Must be implemented because   }
{ pos(c: AnsiChar; const s: shortstring) also exists, so otherwise   }
{ using pos(AnsiChar,pansichar) will always call the shortstring version }
{ (exact match for first argument), also with $h+ (JM)           }
Function Pos(c : AnsiChar; Const s : RawByteString; Offset : Sizeint = 1) : SizeInt;
var
  ns,idx: SizeInt;
begin
  pos:=0;
  ns:=length(s);
  if (Offset>0) and (Offset<=ns) then
  begin
    idx:=IndexByte(s[Offset],ns-Offset+1,byte(c));
    if idx>=0 then
      pos:=Offset+idx;
  end;
end;
{$endif FPC_HAS_POS_ANSICHAR_ANSISTR}


{$ifndef FPUNONE}
Function fpc_Val_Real_AnsiStr(Const S : RawByteString; out Code : ValSInt): ValReal; [public, alias:'FPC_VAL_REAL_ANSISTR']; compilerproc;
Var
  SS : ShortString;
begin
  fpc_Val_Real_AnsiStr := 0;
  if length(S) > 255 then
    code := 256
  else
    begin
      SS := S;
      Val(SS,fpc_Val_Real_AnsiStr,code);
    end;
end;
{$endif}


Function fpc_Val_Currency_AnsiStr(Const S : RawByteString; out Code : ValSInt): Currency; [public, alias:'FPC_VAL_CURRENCY_ANSISTR']; compilerproc;
Var
  SS : ShortString;
begin
  if length(S) > 255 then
    begin
      fpc_Val_Currency_AnsiStr := 0;
      code := 256;
    end
  else
    begin
      SS := S;
      Val(SS,fpc_Val_Currency_AnsiStr,code);
    end;
end;


Function fpc_Val_UInt_AnsiStr ({$ifndef VER3_2}DestSize: SizeInt;{$endif VER3_2} Const S : RawByteString; out Code : ValSInt): ValUInt; [public, alias:'FPC_VAL_UINT_ANSISTR']; compilerproc;
Var
  SS : ShortString;
begin
  fpc_Val_UInt_AnsiStr := 0;
  if length(S) > 255 then
    code := 256
  else
    begin
      SS := S;
      Val(SS,fpc_Val_UInt_AnsiStr,code);
    end;
end;


Function fpc_Val_SInt_AnsiStr (DestSize: SizeInt; Const S : RawByteString; out Code : ValSInt): ValSInt; [public, alias:'FPC_VAL_SINT_ANSISTR']; compilerproc;
Var
  SS : ShortString;
begin
  fpc_Val_SInt_AnsiStr:=0;
  if length(S)>255 then
    code:=256
  else
    begin
       SS := S;
       fpc_Val_SInt_AnsiStr := int_Val_SInt_ShortStr(DestSize,SS,Code);
    end;
end;


{$ifndef CPU64}

Function fpc_Val_qword_AnsiStr (Const S : RawByteString; out Code : ValSInt): qword; [public, alias:'FPC_VAL_QWORD_ANSISTR']; compilerproc;
Var
  SS : ShortString;
begin
  fpc_Val_qword_AnsiStr:=0;
  if length(S)>255 then
    code:=256
  else
    begin
       SS := S;
       Val(SS,fpc_Val_qword_AnsiStr,Code);
    end;
end;


Function fpc_Val_int64_AnsiStr (Const S : RawByteString; out Code : ValSInt): Int64; [public, alias:'FPC_VAL_INT64_ANSISTR']; compilerproc;
Var
  SS : ShortString;
begin
  fpc_Val_int64_AnsiStr:=0;
  if length(S)>255 then
    code:=256
  else
    begin
       SS := s;
       Val(SS,fpc_Val_int64_AnsiStr,Code);
    end;
end;

{$endif CPU64}


{$if defined(CPU16) or defined(CPU8)}
Function fpc_Val_longword_AnsiStr (Const S : RawByteString; out Code : ValSInt): longword; [public, alias:'FPC_VAL_LONGWORD_ANSISTR']; compilerproc;
Var
  SS : ShortString;
begin
  fpc_Val_longword_AnsiStr:=0;
  if length(S)>255 then
    code:=256
  else
    begin
       SS := S;
       Val(SS,fpc_Val_longword_AnsiStr,Code);
    end;
end;


Function fpc_Val_longint_AnsiStr (Const S : RawByteString; out Code : ValSInt): LongInt; [public, alias:'FPC_VAL_LONGINT_ANSISTR']; compilerproc;
Var
  SS : ShortString;
begin
  fpc_Val_longint_AnsiStr:=0;
  if length(S)>255 then
    code:=256
  else
    begin
       SS := s;
       Val(SS,fpc_Val_longint_AnsiStr,Code);
    end;
end;


Function fpc_Val_word_AnsiStr (Const S : RawByteString; out Code : ValSInt): word; [public, alias:'FPC_VAL_WORD_ANSISTR']; compilerproc;
Var
  SS : ShortString;
begin
  fpc_Val_word_AnsiStr:=0;
  if length(S)>255 then
    code:=256
  else
    begin
       SS := S;
       Val(SS,fpc_Val_word_AnsiStr,Code);
    end;
end;


Function fpc_Val_smallint_AnsiStr (Const S : RawByteString; out Code : ValSInt): smallint; [public, alias:'FPC_VAL_SMALLINT_ANSISTR']; compilerproc;
Var
  SS : ShortString;
begin
  fpc_Val_smallint_AnsiStr:=0;
  if length(S)>255 then
    code:=256
  else
    begin
       SS := s;
       Val(SS,fpc_Val_smallint_AnsiStr,Code);
    end;
end;
{$endif CPU16 or CPU8}


{$if defined(CPU8)}
Function fpc_Val_word_AnsiStr (Const S : RawByteString; out Code : ValSInt): longword; [public, alias:'FPC_VAL_WORD_ANSISTR']; compilerproc;
Var
  SS : ShortString;
begin
  fpc_Val_longword_AnsiStr:=0;
  if length(S)>255 then
    code:=256
  else
    begin
       SS := S;
       Val(SS,fpc_Val_longword_AnsiStr,Code);
    end;
end;


Function fpc_Val_smallint_AnsiStr (Const S : RawByteString; out Code : ValSInt): LongInt; [public, alias:'FPC_VAL_SMALLINT_ANSISTR']; compilerproc;
Var
  SS : ShortString;
begin
  fpc_Val_longint_AnsiStr:=0;
  if length(S)>255 then
    code:=256
  else
    begin
       SS := s;
       Val(SS,fpc_Val_longint_AnsiStr,Code);
    end;
end;
{$endif CPU8}


{$ifndef FPUNONE}
procedure fpc_AnsiStr_Float(d : ValReal;len,fr,rt : SizeInt;out s : RawByteString;cp : TSystemCodePage);[public,alias:'FPC_ANSISTR_FLOAT']; compilerproc; inline;
var
  ss: ShortString;
begin
  str_real(len,fr,d,treal_type(rt),ss);
  s:=ss;
  SetCodePage(s,cp,false);
end;
{$endif}

{$ifndef FPC_STR_ENUM_INTERN}
procedure fpc_ansistr_enum(ordinal,len:sizeint;typinfo,ord2strindex:pointer;out s:RawByteString;cp : TSystemCodePage);[public,alias:'FPC_ANSISTR_ENUM'];compilerproc; inline;

var ss:shortstring;

begin
  fpc_shortstr_enum(ordinal,len,typinfo,ord2strindex,ss);
  s:=ss;
  SetCodePage(s,cp,false);
end;
{$endif FPC_STR_ENUM_INTERN}


procedure fpc_ansistr_bool(b : boolean;len:sizeint;out s:RawByteString;cp : TSystemCodePage);[public,alias:'FPC_ANSISTR_BOOL'];compilerproc; inline;
var
  ss:shortstring;
begin
  fpc_shortstr_bool(b,len,ss);
  s:=ss;
  SetCodePage(s,cp,false);
end;


{$ifndef FPC_STR_ENUM_INTERN}
function fpc_val_enum_ansistr(str2ordindex:pointer;const s:RawByteString;out code:valsint):longint; [public, alias:'FPC_VAL_ENUM_ANSISTR']; compilerproc;

begin
  fpc_val_enum_ansistr:=fpc_val_enum_shortstr(str2ordindex,s,code);
end;
{$endif FPC_STR_ENUM_INTERN}


procedure fpc_AnsiStr_Currency(c : currency;len,fr : SizeInt;out s : RawByteString;cp : TSystemCodePage);[public,alias:'FPC_ANSISTR_CURRENCY']; compilerproc; inline;
var
  ss: ShortString;
begin
  str(c:len:fr,ss);
  s:=ss;
  SetCodePage(s,cp,false);
end;

Procedure fpc_AnsiStr_UInt(v : ValUInt;Len : SizeInt; out S : RawByteString;cp : TSystemCodePage);[Public,Alias : 'FPC_ANSISTR_VALUINT']; compilerproc; inline;
Var
  SS : ShortString;
begin
  str(v:Len,SS);
  S:=SS;
  SetCodePage(s,cp,false);
end;



Procedure fpc_AnsiStr_SInt(v : ValSInt;Len : SizeInt; out S : RawByteString;cp : TSystemCodePage);[Public,Alias : 'FPC_ANSISTR_VALSINT']; compilerproc; inline;
Var
  SS : ShortString;
begin
  str (v:Len,SS);
  S:=SS;
  SetCodePage(s,cp,false);
end;


{$ifndef CPU64}

Procedure fpc_AnsiStr_QWord(v : QWord;Len : SizeInt; out S : RawByteString;cp : TSystemCodePage);[Public,Alias : 'FPC_ANSISTR_QWORD']; compilerproc; inline;
Var
  SS : ShortString;
begin
  str(v:Len,SS);
  S:=SS;
  SetCodePage(s,cp,false);
end;

Procedure fpc_AnsiStr_Int64(v : Int64; Len : SizeInt; out S : RawByteString;cp : TSystemCodePage);[Public,Alias : 'FPC_ANSISTR_INT64']; compilerproc; inline;
Var
  SS : ShortString;
begin
  str (v:Len,SS);
  S:=SS;
  SetCodePage(s,cp,false);
end;

{$endif CPU64}

{$if defined(CPU16) or defined(CPU8)}
Procedure fpc_AnsiStr_LongWord(v : LongWord;Len : SizeInt; out S : RawByteString;cp : TSystemCodePage);[Public,Alias : 'FPC_ANSISTR_LONGWORD']; compilerproc; inline;
Var
  SS : ShortString;
begin
  str(v:Len,SS);
  S:=SS;
  SetCodePage(s,cp,false);
end;

Procedure fpc_AnsiStr_LongInt(v : LongInt; Len : SizeInt; out S : RawByteString;cp : TSystemCodePage);[Public,Alias : 'FPC_ANSISTR_LONGINT']; compilerproc; inline;
Var
  SS : ShortString;
begin
  str (v:Len,SS);
  S:=SS;
  SetCodePage(s,cp,false);
end;

Procedure fpc_AnsiStr_Word(v : Word;Len : SizeInt; out S : RawByteString;cp : TSystemCodePage);[Public,Alias : 'FPC_ANSISTR_WORD']; compilerproc; inline;
Var
  SS : ShortString;
begin
  str(v:Len,SS);
  S:=SS;
  SetCodePage(s,cp,false);
end;

Procedure fpc_AnsiStr_SmallInt(v : SmallInt; Len : SizeInt; out S : RawByteString;cp : TSystemCodePage);[Public,Alias : 'FPC_ANSISTR_SMALLINT']; compilerproc; inline;
Var
  SS : ShortString;
begin
  str (v:Len,SS);
  S:=SS;
  SetCodePage(s,cp,false);
end;
{$endif CPU16 or CPU8}

Procedure fpc_ansistr_delete(Var S : RawByteString; Index,Size: SizeInt);
Var
  LS : SizeInt;
begin
  ls:=Length(S);
  If (Index>LS) or (Index<=0) or (Size<=0) then
    exit;
  UniqueString(S);
  If (Size>LS-Index) then   // Size+Index gives overflow ??
     Size:=LS-Index+1;
  If (Size<=LS-Index) then
    begin
      Dec(Index);
      fpc_pchar_ansistr_intern_charmove(PAnsiChar(S),Index+Size,S,Index,LS-Index-Size+1);
    end;
  Setlength(S,LS-Size);
end;


Procedure fpc_ansistr_insert(Const Source : RawByteString; Var S : RawByteString; Index : SizeInt);
var
  LS,LSource : SizeInt;
{$ifdef jvm}
  Temp : RawByteString;
{$else}
  selfinsert : boolean;
  srcp : PAnsiChar;
{$endif}
begin
  If Source='' then
   exit;
  if S='' then
    begin
      S:=Source;
      exit;
    end;
  LSource:={$ifdef jvm}Length(Source){$else}PAnsiRec(Pointer(Source)-AnsiFirstOff)^.Len{$endif};
  LS:={$ifdef jvm}Length(S){$else}PAnsiRec(Pointer(S)-AnsiFirstOff)^.Len{$endif};
  if index < 1 then
   index := 1;
  Dec(Index);
  if index > LS then
   index := LS;
{$ifdef jvm}
  SetLength(Temp,LSource+LS);
  SetCodePage(Temp,StringCodePage(S),false);
  If Index>0 then
    fpc_pchar_ansistr_intern_charmove(PAnsiChar(S),0,Temp,0,Index);
  fpc_pchar_ansistr_intern_charmove(PAnsiChar(Source),0,Temp,Index,LSource);
  If (LS-Index)>0 then
    fpc_pchar_ansistr_intern_charmove(PAnsiChar(S),Index,Temp,LSource+Index,LS-Index);
  S:=Temp;
{$else}
  selfinsert:=Pointer(Source)=Pointer(S);
  SetLength(S,LS+LSource);
  Move(PAnsiChar(Pointer(S))[Index],PAnsiChar(Pointer(S))[Index+LSource],(LS-Index)*SizeOf(AnsiChar));
  srcp:=Pointer(Source);
  if selfinsert then
    srcp:=Pointer(S);
  Move(srcp^,PAnsiChar(Pointer(S))[Index],LSource*SizeOf(AnsiChar));
{$endif}
end;


{$ifndef FPC_HAS_ANSISTR_OF_CHAR}
{$define FPC_HAS_ANSISTR_OF_CHAR}
Function StringOfChar(c : Ansichar;l : SizeInt) : AnsiString;
begin
  SetLength(StringOfChar,l);
  FillChar(Pointer(StringOfChar)^,Length(StringOfChar),c);
end;
{$endif FPC_HAS_ANSISTR_OF_CHAR}


Procedure fpc_setstring_ansistr_pansichar(out S : RawByteString; Buf : PAnsiChar; Len : SizeInt; cp: TSystemCodePage); rtlproc; compilerproc;
begin
  SetLength(S,Len);
  SetCodePage(S,cp,false);
  If (Buf<>Nil) then
    fpc_pchar_ansistr_intern_charmove(Buf,0,S,0,Len);
end;


Procedure fpc_setstring_ansistr_pwidechar(out S : RawByteString; Buf : PWideChar; Len : SizeInt; cp: TSystemCodePage); rtlproc; compilerproc;
begin
  cp:=TranslatePlaceholderCP(cp);
  if (Buf<>nil) and (Len>0) then
    widestringmanager.Wide2AnsiMoveProc(Buf,S,cp,Len)
  else
    begin
      SetLength(S, Len);
      SetCodePage(S,cp,false);
    end;
end;


{$ifndef FPC_HAS_UPCASE_ANSISTR}
{$define FPC_HAS_UPCASE_ANSISTR}
function upcase(const s : ansistring) : ansistring;
var
  i : SizeInt;
begin
  Setlength(result,length(s));
  for i := 1 to length (s) do
    result[i] := upcase(s[i]);
end;
{$endif FPC_HAS_UPCASE_ANSISTR}


{$ifndef FPC_HAS_LOWERCASE_ANSISTR}
{$define FPC_HAS_LOWERCASE_ANSISTR}
function lowercase(const s : ansistring) : ansistring;
var
  i : SizeInt;
begin
  Setlength(result,length(s));
  for i := 1 to length (s) do
    result[i] := lowercase(s[i]);
end;
{$endif FPC_HAS_LOWERCASE_ANSISTR}


{$ifndef FPC_HAS_ANSISTR_STRINGCODEPAGE}
{$define FPC_HAS_ANSISTR_STRINGCODEPAGE}
function StringCodePage(const S: RawByteString): TSystemCodePage; overload;
  begin
    if assigned(Pointer(S)) then
      Result:=PAnsiRec(pointer(S)-AnsiFirstOff)^.CodePage
    else
      Result:=DefaultSystemCodePage;
  end;
{$endif FPC_HAS_ANSISTR_STRINGCODEPAGE}


{$ifndef FPC_HAS_ANSISTR_STRINGELEMENTSIZE}
{$define FPC_HAS_ANSISTR_STRINGELEMENTSIZE}
function StringElementSize(const S: RawByteString): Word; overload;
  begin
    if assigned(Pointer(S)) then
      Result:=PAnsiRec(pointer(S)-AnsiFirstOff)^.ElementSize
    else
      Result:=SizeOf(AnsiChar);
  end;
{$endif FPC_HAS_ANSISTR_STRINGELEMENTSIZE}


{$ifndef FPC_HAS_ANSISTR_STRINGREFCOUNT}
{$define FPC_HAS_ANSISTR_STRINGREFCOUNT}
function StringRefCount(const S: RawByteString): SizeInt; overload;
  begin
    if assigned(Pointer(S)) then
      Result:=PAnsiRec(pointer(S)-AnsiFirstOff)^.Ref
    else
      Result:=0;
  end;
{$endif FPC_HAS_ANSISTR_STRINGREFCOUNT}


{$ifndef FPC_HAS_ANSISTR_SETCODEPAGE}
{$define FPC_HAS_ANSISTR_SETCODEPAGE}
procedure InternalSetCodePage(var s : RawByteString; CodePage : TSystemCodePage; Convert : Boolean = True);
  begin
    if Convert then
      begin
        s:=fpc_AnsiStr_To_AnsiStr(s,CodePage);
      end
    else
      begin
        UniqueString(s);
        PAnsiRec(pointer(s)-AnsiFirstOff)^.CodePage:=CodePage;
      end;
  end;


{ use this wrapper for the simple case to avoid the generation of a temp. ansistring which causes
  extra exception frames }
procedure SetCodePage(var s : RawByteString; CodePage : TSystemCodePage; Convert : Boolean = True);
  var
    OrgCodePage,
    TranslatedCodePage,
    TranslatedCurrentCodePage: TSystemCodePage;
  begin
    if Length(S)=0 then
      exit;
    { if the codepage are identical, we don't have to do anything (even if the
      string has multiple references) }
    OrgCodePage:=PAnsiRec(pointer(S)-AnsiFirstOff)^.CodePage;
    if OrgCodePage=CodePage then
      exit;
    { if we're just replacing a placeholder code page with its actual value or
      vice versa, we don't have to perform any conversion }
    TranslatedCurrentCodePage:=TranslatePlaceholderCP(OrgCodePage);
    TranslatedCodePage:=TranslatePlaceholderCP(CodePage);
    Convert:=Convert and
      (TranslatedCurrentCodePage<>TranslatedCodePage);
    if not Convert and (PAnsiRec(pointer(S)-AnsiFirstOff)^.Ref=1) then
      PAnsiRec(pointer(S)-AnsiFirstOff)^.CodePage:=CodePage
    else
      InternalSetCodePage(S,CodePage,Convert);
  end;
{$endif FPC_HAS_ANSISTR_SETCODEPAGE}


procedure SetMultiByteConversionCodePage(CodePage: TSystemCodePage);
  begin
    DefaultSystemCodePage:=CodePage;
  end;


procedure SetMultiByteFileSystemCodePage(CodePage: TSystemCodePage);
  begin
    DefaultFileSystemCodePage:=CodePage;
  end;


procedure SetMultiByteRTLFileSystemCodePage(CodePage: TSystemCodePage);
  begin
    DefaultRTLFileSystemCodePage:=CodePage;
  end;

