unit fpwidestring;

{$mode objfpc}

interface
uses
  unicodedata;

{$i rtldefs.inc}

  function SetActiveCollation(const AName : TCollationName) : Boolean;
  function SetActiveCollation(const ACollation : PUCA_DataBook) : Boolean;
  function GetActiveCollation() : PUCA_DataBook;

var
  DefaultCollationName : TCollationName = '';

implementation
uses
{$ifdef MSWINDOWS}
  Windows,
{$endif MSWINDOWS}
{$ifdef Unix}
  unixcp,
{$endif}
  charset;
  
procedure fpc_rangeerror; [external name 'FPC_RANGEERROR'];
{$ifdef MSWINDOWS}
  function GetACP:UINT; external 'kernel32' name 'GetACP';
{$endif MSWINDOWS}

const
  IgnoreInvalidSequenceFlag = True;
var
  OldManager : TUnicodeStringManager;

{$ifdef FPC_HAS_FEATURE_THREADING}
ThreadVar
{$else FPC_HAS_FEATURE_THREADING}
Var
{$endif FPC_HAS_FEATURE_THREADING}
  current_DefaultSystemCodePage : TSystemCodePage;
  current_Map : punicodemap;
  current_Collation : PUCA_DataBook;

function SetActiveCollation(const ACollation : PUCA_DataBook) : Boolean;
begin
  Result := (ACollation <> nil);
  if Result then
    current_Collation := ACollation;
end;

function SetActiveCollation(const AName : TCollationName) : Boolean;
var
  c : PUCA_DataBook;
begin
  c:=FindCollation(AName);
  Result := (c <> nil);
  if Result then
    Result := SetActiveCollation(c);
end;

function GetActiveCollation() : PUCA_DataBook;
begin
  Result := current_Collation;
end;

{procedure error_CpNotFound(ACodePage:TSystemCodePage);
begin
  System.error(reCodesetConversion);
end;}

procedure InitThread;
var
  c : PUCA_DataBook;
begin
  current_DefaultSystemCodePage:=DefaultSystemCodePage;
  current_Map:=getmap(current_DefaultSystemCodePage);
  c:=nil;
  if (DefaultCollationName<>'') then
    c:=FindCollation(DefaultCollationName);
  if (c=nil) and (GetCollationCount()>0) then
    c:=FindCollation(0);
  current_Collation:=c;
end;

procedure FiniThread;
begin
  current_Map:=nil;
end;

function FindMap(const cp: TSystemCodePage): punicodemap;inline;
begin
  if (cp=DefaultSystemCodePage) then
    begin
      { update current_Map in case the DefaultSystemCodePage has been changed }
      if (current_DefaultSystemCodePage<>DefaultSystemCodePage) or not Assigned(current_Map) then
        begin
          FiniThread;
          InitThread;
        end;
      Result:=current_Map;
    end
  else
    Result:=getmap(cp);
end;

{ return value:
  -1 if incomplete or invalid code point
  0 if NULL character,
  > 0 if that's the length in bytes of the code point }
function UTF8CodePointLength(const Str: PAnsiChar; MaxLookAead: PtrInt): Ptrint;
{... taken from ustrings.inc}
var
  p: PByte;
  TempBYTE: Byte;
  CharLen: SizeUint;
  LookAhead: SizeUInt;
  UC: SizeUInt;
begin
  if (Str=nil) then
    exit(0);
  p:=PByte(Str);
  if (p^=0) then
    exit(0);

  p:=PByte(Str);
  if (p^ and $80) = 0 then //One character US-ASCII,
    exit(1);
  TempByte:=p^;
  CharLen:=0;
  while (TempByte and $80)<>0 do
    begin
      TempByte:=(TempByte shl 1) and $FE;
      Inc(CharLen);
    end;
  //Test for the "CharLen" conforms UTF-8 string
  //This means the 10xxxxxx pattern.
  if SizeUInt(CharLen-1)>MaxLookAead then //Insuficient chars in string to decode UTF-8 array
    exit(-1);
  for LookAhead := 1 to CharLen-1 do
    begin
      if ((p[LookAhead] and $80)<>$80) or
         ((p[LookAhead] and $40)<>$00)
      then
        begin
          //Invalid UTF-8 sequence, fallback.
          exit(-1);
        end;
    end;

  Result:=CharLen;
  case CharLen of
    1:  begin
          //Not valid UTF-8 sequence
          Result:=-1;
        end;
    2:  begin
          //Two bytes UTF, convert it
          UC:=(p^ and $1F) shl 6;
          UC:=UC or (p[1] and $3F);
          if UC <= $7F then
            begin
              //Invalid UTF sequence.
              Result:=-1;
            end;
        end;
    3:  begin
          //Three bytes, convert it to unicode
          UC:= (p^ and $0F) shl 12;
          UC:= UC or ((p[1] and $3F) shl 6);
          UC:= UC or ((p[2] and $3F));
          if (UC <= $7FF) or (UC >= $FFFE) or ((UC >= $D800) and (UC <= $DFFF)) then
            begin
              //Invalid UTF-8 sequence
              Result:=-1;
            End;
        end;
    4:  begin
          //Four bytes, convert it to two unicode characters
          UC:= (p^ and $07) shl 18;
          UC:= UC or ((p[1] and $3F) shl 12);
          UC:= UC or ((p[2] and $3F) shl 6);
          UC:= UC or ((p[3] and $3F));
          if (UC < $10000) or (UC > $10FFFF) then
            begin
              Result:=-1;
            end
        end;
    5,6,7:  begin
              //Invalid UTF8 to unicode conversion,
              //mask it as invalid UNICODE too.
              Result:=-1;
            end;
  end;
end;

{ return value:
  -1 if incomplete or invalid code point
  0 if NULL character,
  > 0 if that's the length in bytes of the code point }
function CodePointLength(const Str: PAnsiChar; MaxLookAead: PtrInt): PtrInt;
var
  p : PByte;
begin
  if (current_DefaultSystemCodePage=CP_UTF8) then
    exit(UTF8CodePointLength(Str,MaxLookAead));

  if (Str=nil) then
    exit(0);
  p:=PByte(Str);
  if (p^=0) then
    exit(0);
  if (current_Map=nil) then
    exit(1);

  if (p^>current_Map^.lastchar) then
    exit(-1);
  case current_Map^.map[p^].flag of
    umf_undefined : Result:=-1;
    umf_leadbyte  :
      begin
        if (MaxLookAead>0) then
          Result:=2
        else
          Result:=-1;
      end;
    else
      Result:=1;
  end;
end;

procedure Unicode2AnsiMove(source:punicodechar;var dest:RawByteString;cp : TSystemCodePage;len:SizeInt);
var
  locSource : punicodechar;
  locMap : punicodemap;
  destBuffer : PAnsiChar;
  destLen,actualLen, i : SizeInt;
  blockLen : SizeInt;
begin
  if (len=0) then
    begin
      SetLength(dest,0);
      exit;
    end;

  if (cp=CP_UTF8) then
    begin
      destLen:=UnicodeToUtf8(nil,High(SizeUInt),source,len);
      SetLength(dest,destLen);
      UnicodeToUtf8(@dest[1],destLen,source,len);
      SetCodePage(dest,cp,False);
      exit;
    end;
  if (cp=CP_UTF16) then
    begin
      destLen:=len*SizeOf(UnicodeChar);
      SetLength(dest,destLen);
      Move(source^,dest[1],destLen);
      SetCodePage(dest,cp,False);
      exit;
    end;

  locMap:=FindMap(cp);
  if (locMap=nil) then
    begin
      DefaultUnicode2AnsiMove(source,dest,DefaultSystemCodePage,len);
      exit;
    end;
    
  destLen:=3*len;
  SetLength(dest,destLen);
  destBuffer:=@dest[1];
  actualLen:=0;
  locSource:=source;
  for i:=1 to len do
    begin
      blockLen:=getascii(tunicodechar(locSource^),locMap,destBuffer,(destLen-actualLen));
      if (blockLen<0) then
        begin
          destLen:=destLen + 3*(1+len-i);
          SetLength(dest,destLen);
          destBuffer:=@dest[1];
          blockLen:=getascii(tunicodechar(locSource^),locMap,destBuffer,(destLen-actualLen));
        end;
      Inc(destBuffer,blockLen);
      actualLen:=actualLen+blockLen;
      Inc(locSource);
    end;
  if (actualLen<>Length(dest)) then
    SetLength(dest,actualLen);
  if (Length(dest)>0) then
    SetCodePage(dest,cp,False);
end;

procedure Ansi2UnicodeMove(source:PAnsiChar; cp:TSystemCodePage; var dest:UnicodeString; len:SizeInt);
var
  locMap : punicodemap;
  destLen : SizeInt;
begin
  if (len<=0) then
    begin
      SetLength(dest,0);
      exit;
    end;

  if (cp=CP_UTF8) then
    begin
      destLen:=Utf8ToUnicode(nil,source,len);
      if destLen > 0 then
        SetLength(dest,destLen-1)
      else
        SetLength(dest,0);
      Utf8ToUnicode(@dest[1],source,len);
      exit;
    end;
  if (cp=CP_UTF16) then
    begin
      //what if (len mod 2) > 0 ?
      destLen:=len div SizeOf(UnicodeChar);
      SetLength(dest,destLen);
      Move(source^,dest[1],(destLen*SizeOf(UnicodeChar)));
      exit;
    end;

  locMap:=FindMap(cp);
  if (locMap=nil) then
    begin
      DefaultAnsi2UnicodeMove(source,DefaultSystemCodePage,dest,len);
      exit;
    end;

  destLen:=getunicode(source,len,locMap,nil);   
  SetLength(dest,destLen);
  getunicode(source,len,locMap,tunicodestring(@dest[1]));   
end;

{$ifdef MSWINDOWS}
procedure Ansi2WideMove(source:PAnsiChar; cp:TSystemCodePage; var dest:WideString; len:SizeInt);
var
  locMap : punicodemap;
  destLen : SizeInt;
begin
  if (len<=0) then
    begin
      SetLength(dest,0);
      exit;
    end;

  locMap:=FindMap(cp);
  if (locMap=nil) then
    begin
      DefaultAnsi2WideMove(source,DefaultSystemCodePage,dest,len);
      exit;
    end;

  destLen:=getunicode(source,len,locMap,nil);   
  SetLength(dest,destLen);
  getunicode(source,len,locMap,tunicodestring(@dest[1])); 
end;
{$endif MSWINDOWS}

function UpperUnicodeString(const S: UnicodeString): UnicodeString;
begin
  if (UnicodeToUpper(S,IgnoreInvalidSequenceFlag,Result) <> 0) then
    system.error(reRangeError);
end;

function UpperWideString(const S: WideString): WideString;
var
  u : UnicodeString;
begin
  u:=s;
  Result:=UpperUnicodeString(u);
end;

function LowerUnicodeString(const S: UnicodeString): UnicodeString;
begin
  if (UnicodeToLower(S,IgnoreInvalidSequenceFlag,Result) <> 0) then
    system.error(reRangeError);
end;

function LowerWideString(const S: WideString): WideString;
var
  u : UnicodeString;
begin
  u:=s;
  Result:=LowerUnicodeString(u);
end;


function CompareUnicodeStringUCA(p1,p2:PUnicodeChar; l1,l2:PtrInt) : PtrInt;inline;
begin
  Result := IncrementalCompareString(p1,l1,p2,l2,current_Collation);
end;

function CompareUnicodeString(p1,p2:PUnicodeChar; l1,l2:PtrInt) : PtrInt;inline;
begin
  if (Pointer(p1)=Pointer(p2)) then
    exit(0);
  if (l1=0) then
    exit(-l2);
  if (l2=0) then
    exit(l1);
  Result := CompareUnicodeStringUCA(p1,p2,l1,l2);
end;

function CompareUnicodeString(const s1, s2 : UnicodeString) : PtrInt;
begin
  if (current_Collation=nil) then
    exit(OldManager.CompareUnicodeStringProc(s1,s2));
  Result:=CompareUnicodeString(
            PUnicodeChar(Pointer(s1)),
            PUnicodeChar(Pointer(s2)),
            Length(s1),Length(s2)
          );
end;

function CompareWideString(const s1, s2 : WideString) : PtrInt;
begin
  if (current_Collation=nil) then
    exit(OldManager.CompareWideStringProc(s1,s2));
  Result:=CompareUnicodeString(
            PUnicodeChar(Pointer(s1)),
            PUnicodeChar(Pointer(s2)),
            Length(s1),Length(s2)
          );
end;

function CompareTextUnicodeString(const s1, s2 : UnicodeString) : PtrInt;
begin
  Result:=CompareUnicodeString(UpperUnicodeString(s1),UpperUnicodeString(s2));
end;

function CompareTextWideString(const s1, s2 : WideString) : PtrInt;
begin
  Result:=CompareWideString(UpperWideString(s1),UpperWideString(s2));
end;

procedure EnsureAnsiLen(var S: AnsiString; const len: SizeInt); inline;
begin
  if (len>length(s)) then
    if (length(s) < 10*256) then
      setlength(s,length(s)+10)
    else
      setlength(s,length(s)+length(s) shr 8);
end;

procedure ConcatCharToAnsiStr(const c: AnsiChar; var S: AnsiString; var index: SizeInt);
begin
  EnsureAnsiLen(s,index);
  pansichar(@s[index])^:=c;
  inc(index);
end;

function UpperAnsiString(const s : ansistring) : ansistring;
var
  p        : PAnsiChar;
  i, slen,
  resindex : SizeInt;
  mblen    : SizeInt;
  us,usl   : UnicodeString;
  locMap   : punicodemap;
  ulen,k,
  aalen,ai : SizeInt;
  aa       : array[0..8] of AnsiChar;
begin
  if (Length(s)=0) then
    exit('');
  if (DefaultSystemCodePage=CP_UTF8) then 
    begin
      //convert to UnicodeString,uppercase,convert back to utf8
      ulen:=Utf8ToUnicode(nil,@s[1],Length(s));
      if ulen>0 then
        SetLength(us,ulen-1);
      Utf8ToUnicode(@us[1],@s[1],Length(s));
      us:=UpperUnicodeString(us);
      
      ulen:=Length(us);
      slen:=UnicodeToUtf8(nil,0,@us[1],ulen);
      SetLength(Result,slen);
      UnicodeToUtf8(@Result[1],slen,@us[1],ulen);
      exit;    
    end;

  locMap:=FindMap(DefaultSystemCodePage);
  if (locMap=nil) then
    exit(System.UpCase(s));

  SetLength(us,2);
  p:=@s[1];
  slen:=length(s);
  SetLength(result,slen+10);
  i:=1;
  resindex:=1;
  while (i<=slen) do
    begin
      mblen:=CodePointLength(p,slen-i);
      if (mblen<=0) then
        begin
          ConcatCharToAnsiStr(p^,result,resindex);
          mblen:=1;
        end
      else
        begin
          SetLength(us,2);
          ulen:=getunicode(p,mblen,locMap,@us[1]);
          if (Length(us)<>ulen) then
            SetLength(us,ulen);
          usl:=UpperUnicodeString(us);
          for k:=1 to Length(usl) do
            begin
              aalen:=getascii(tunicodechar(us[k]),locMap,@aa[Low(aa)],Length(aa));
              for ai:=0 to aalen-1 do
                ConcatCharToAnsiStr(aa[ai],result,resindex);
            end;
        end;
      Inc(p,mblen);
    end;
  SetLength(result,resindex-1);
end;

function LowerAnsiString(const s : ansistring) : ansistring;
var
  p        : PAnsiChar;
  i, slen,
  resindex : SizeInt;
  mblen    : SizeInt;
  us,usl   : UnicodeString;
  locMap   : punicodemap;
  ulen,k,
  aalen,ai : SizeInt;
  aa       : array[0..8] of AnsiChar;
begin
  if (Length(s)=0) then
    exit('');
  if (DefaultSystemCodePage=CP_UTF8) then 
    begin
      //convert to UnicodeString,lowercase,convert back to utf8
      ulen:=Utf8ToUnicode(nil,@s[1],Length(s));
      if ulen>0 then
        SetLength(us,ulen-1);
      Utf8ToUnicode(@us[1],@s[1],Length(s));
      us:=LowerUnicodeString(us);
      
      ulen:=Length(us);
      slen:=UnicodeToUtf8(nil,0,@us[1],ulen);
      SetLength(Result,slen);
      UnicodeToUtf8(@Result[1],slen,@us[1],ulen);
      exit;    
    end;
  locMap:=FindMap(DefaultSystemCodePage);
  if (locMap=nil) then
    exit(System.LowerCase(s));

  SetLength(us,2);
  p:=@s[1];
  slen:=length(s);
  SetLength(result,slen+10);
  i:=1;
  resindex:=1;
  while (i<=slen) do
    begin
      mblen:=CodePointLength(p,slen-i);
      if (mblen<=0) then
        begin
          ConcatCharToAnsiStr(p^,result,resindex);
          mblen:=1;
        end
      else
        begin
          SetLength(us,2);
          ulen:=getunicode(p,mblen,locMap,@us[1]);
          if (Length(us)<>ulen) then
            SetLength(us,ulen);
          usl:=LowerUnicodeString(us);
          for k:=1 to Length(usl) do
            begin
              aalen:=getascii(tunicodechar(us[k]),locMap,@aa[Low(aa)],Length(aa));
              for ai:=0 to aalen-1 do
                ConcatCharToAnsiStr(aa[ai],result,resindex);
            end;
        end;
      Inc(p,mblen);
    end;
  SetLength(result,resindex-1);
end;

procedure ansi2pchar(const s: ansistring; const orgp: pansichar; out p: pansichar);
var
  newlen: sizeint;
begin
  newlen:=length(s);
  if newlen>strlen(orgp) then
    fpc_rangeerror;
  p:=orgp;
  if (newlen>0) then
    move(s[1],p[0],newlen);
  p[newlen]:=#0;
end;

function AnsiStrLower(Str: PAnsiChar): PAnsiChar;
var
  temp: ansistring;
begin
  temp:=LowerAnsiString(str);
  ansi2pchar(temp,str,result);
end;

function AnsiStrUpper(Str: PAnsiChar): PAnsiChar;
var
  temp: ansistring;
begin
  temp:=UpperAnsiString(str);
  ansi2pchar(temp,str,result);
end;

function CharLengthPChar(const Str: PAnsiChar): PtrInt;
var
  len:PtrInt;
  nextlen: ptrint;
  s: PAnsiChar;
begin
  Result:=0;
  if (Str=nil) or (Byte(Str^)=0) then
    exit;
  s:=str;
  len:=strlen(s);
  repeat
    nextlen:=CodePointLength(s,len);
    { skip invalid/incomplete sequences }
    if (nextlen<0) then
      nextlen:=1;
    Inc(result,nextlen);
    Inc(s,nextlen);
    Dec(len,nextlen);
  until (nextlen=0);
end;

function InternalCompareStrAnsiString(
  const S1, S2     : PAnsiChar;
  const Len1, Len2 : PtrUInt
) : PtrInt;inline;
var
  a, b : UnicodeString;
begin
  a := '';
  Ansi2UnicodeMove(S1,DefaultSystemCodePage,a,Len1);
  b := '';
  Ansi2UnicodeMove(S2,DefaultSystemCodePage,b,Len2);
  Result := CompareUnicodeString(a,b);
end;

function StrLCompAnsiString(S1, S2: PAnsiChar; MaxLen: PtrUInt): PtrInt;
begin
  if (MaxLen=0) then
    exit(0);
  Result := InternalCompareStrAnsiString(S1,S2,MaxLen,MaxLen);
end;

function CompareStrAnsiString(const S1, S2: ansistring): PtrInt;
var
  l1, l2 : PtrInt;
begin
  if (Pointer(S1)=Pointer(S2)) then
    exit(0);
  l1:=Length(S1);
  l2:=Length(S2);
  if (l1=0) then begin
    if (l2=0) then
      exit(0);
    exit(-l2);
  end;
  if (l2=0) then
    exit(-l1);
  Result := InternalCompareStrAnsiString(@S1[1],@S2[2],l1,l2);
end;

function CompareTextAnsiString(const S1, S2: ansistring): PtrInt;
var
  a,b : ansistring;
begin
  a:=UpperAnsistring(s1);
  b:=UpperAnsistring(s2);
  Result:=CompareStrAnsiString(a,b);
end;

function StrCompAnsiString(S1, S2: PChar): PtrInt;
var
  l1,l2 : PtrInt;
begin
  l1:=strlen(S1);
  l2:=strlen(S2);
  Result := InternalCompareStrAnsiString(S1,S2,l1,l2);
end;

function StrLICompAnsiString(S1, S2: PChar; MaxLen: PtrUInt): PtrInt;
var
  a, b: ansistring;
begin
  if (MaxLen=0) then
    exit(0);
  SetLength(a,MaxLen);
  Move(s1^,a[1],MaxLen);
  SetLength(b,MaxLen);
  Move(s2^,b[1],MaxLen);
  Result:=CompareTextAnsiString(a,b);
end;

function StrICompAnsiString(S1, S2: PChar): PtrInt;
begin
  Result:=CompareTextAnsiString(ansistring(s1),ansistring(s2));
end;

function StrLowerAnsiString(Str: PChar): PChar;
var
  temp: ansistring;
begin
  temp:=LowerAnsiString(str);
  ansi2pchar(temp,str,result);
end;

function StrUpperAnsiString(Str: PChar): PChar;
var
  temp: ansistring;
begin
  temp:=UpperAnsiString(str);
  ansi2pchar(temp,str,result);
end;

//------------------------------------------------------------------------------
procedure SetPascalWideStringManager();
var
  locWideStringManager : TUnicodeStringManager;
begin
  OldManager := widestringmanager;
  locWideStringManager:=widestringmanager;
  With locWideStringManager do
    begin
      Wide2AnsiMoveProc:=@Unicode2AnsiMove;
{$ifndef FPC_WIDESTRING_EQUAL_UNICODESTRING}
      Ansi2WideMoveProc:=@Ansi2WideMove;

      UpperWideStringProc:=@UpperWideString;
      LowerWideStringProc:=@LowerWideString;

      CompareWideStringProc:=@CompareWideString;
      CompareTextWideStringProc:=@CompareTextWideString;
{$else FPC_WIDESTRING_EQUAL_UNICODESTRING}
      Ansi2WideMoveProc:=@Ansi2UnicodeMove;

      UpperWideStringProc:=@UpperUnicodeString;
      LowerWideStringProc:=@LowerUnicodeString;

      CompareWideStringProc:=@CompareUnicodeString;
      CompareTextWideStringProc:=@CompareTextUnicodeString;
{$endif FPC_WIDESTRING_EQUAL_UNICODESTRING}

      CharLengthPCharProc:=@CharLengthPChar;
      CodePointLengthProc:=@CodePointLength;

      UpperAnsiStringProc:=@UpperAnsiString;
      LowerAnsiStringProc:=@LowerAnsiString;
      CompareStrAnsiStringProc:=@CompareStrAnsiString;
      CompareTextAnsiStringProc:=@CompareTextAnsiString;
      StrCompAnsiStringProc:=@StrCompAnsiString;
      StrICompAnsiStringProc:=@StrICompAnsiString;
      StrLCompAnsiStringProc:=@StrLCompAnsiString;
      StrLICompAnsiStringProc:=@StrLICompAnsiString;
      StrLowerAnsiStringProc:=@StrLowerAnsiString;
      StrUpperAnsiStringProc:=@StrUpperAnsiString;
      ThreadInitProc:=@InitThread;
      ThreadFiniProc:=@FiniThread;
      { Unicode }
      Unicode2AnsiMoveProc:=@Unicode2AnsiMove;
      Ansi2UnicodeMoveProc:=@Ansi2UnicodeMove;
      UpperUnicodeStringProc:=@UpperUnicodeString;
      LowerUnicodeStringProc:=@LowerUnicodeString;
      CompareUnicodeStringProc:=@CompareUnicodeString;
      CompareTextUnicodeStringProc:=@CompareTextUnicodeString;
    end;
  SetUnicodeStringManager(locWideStringManager);

  DefaultUnicodeCodePage:=CP_UTF16;
{$ifdef MSWINDOWS}
  DefaultSystemCodePage:=GetACP();
{$ELSE MSWINDOWS}
 {$ifdef UNIX}
  DefaultSystemCodePage:=GetSystemCodepage;
  if (DefaultSystemCodePage = CP_NONE) then
    DefaultSystemCodePage:=CP_UTF8;
  {$ifdef FPCRTL_FILESYSTEM_UTF8}
  DefaultFileSystemCodePage:=CP_UTF8;
  {$else}
  DefaultFileSystemCodePage:=DefaultSystemCodepage;
  {$endif}
  DefaultRTLFileSystemCodePage:=DefaultFileSystemCodePage;
 {$ELSE UNIX}
  if Assigned (WideStringManager.GetStandardCodePageProc) then
   DefaultSystemCodePage := WideStringManager.GetStandardCodePageProc (scpAnsi)
  else
   DefaultSystemCodePage := CP_NONE;
  DefaultFileSystemCodePage := DefaultSystemCodePage;
  DefaultRTLFileSystemCodePage := DefaultSystemCodePage;
 {$endif UNIX}
{$endif MSWINDOWS}
end;


initialization
  current_Collation := nil;
  SetPascalWideStringManager();
  InitThread();

finalization
  FiniThread();

end.
