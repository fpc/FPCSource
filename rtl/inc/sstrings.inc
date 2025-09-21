{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{****************************************************************************
                    subroutines for string handling
****************************************************************************}

{$ifndef FPC_HAS_SHORTSTR_SETLENGTH}
{$define FPC_HAS_SHORTSTR_SETLENGTH}
procedure fpc_Shortstr_SetLength(var s:shortstring;len:SizeInt);[Public,Alias : 'FPC_SHORTSTR_SETLENGTH']; compilerproc;
begin
  if len<0 then
    len:=0;
  if len>high(s) then
    len:=high(s);
  s[0]:=chr(len);
end;
{$endif FPC_HAS_SHORTSTR_SETLENGTH}


{$ifndef FPC_HAS_SHORTSTR_COPY}
{$define FPC_HAS_SHORTSTR_COPY}
function fpc_shortstr_copy(const s : shortstring;index : SizeInt;count : SizeInt): shortstring;compilerproc;
begin
  if count<0 then
   count:=0;
  if index>1 then
   dec(index)
  else
   index:=0;
  if index>length(s) then
   count:=0
  else
   if count>length(s)-index then
    count:=length(s)-index;
  fpc_shortstr_Copy[0]:=chr(Count);
  fpc_shortstr_shortstr_intern_charmove(s,Index+1,fpc_shortstr_Copy,1,Count);
end;
{$endif FPC_HAS_SHORTSTR_COPY}


{$ifndef FPC_HAS_SHORTSTR_DELETE}
{$define FPC_HAS_SHORTSTR_DELETE}
procedure fpc_shortstr_delete(var s : shortstring;index : SizeInt;count : SizeInt);
begin
  if index<=0 then
     exit;
  if (Index<=Length(s)) and (Count>0) then
   begin
     if Count>length(s)-Index then
      Count:=length(s)-Index+1;
     s[0]:=Chr(length(s)-Count);
     if Index<=Length(s) then
      fpc_shortstr_shortstr_intern_charmove(s,Index+Count,s,Index,Length(s)-Index+1);
   end;
end;
{$endif FPC_HAS_SHORTSTR_DELETE}


{$ifndef FPC_HAS_SHORTSTR_INSERT}
{$define FPC_HAS_SHORTSTR_INSERT}
procedure fpc_shortstr_insert(const source : shortstring;var s : shortstring;index : SizeInt);
var
  cut,srclen,indexlen : SizeInt;
begin
  if index<1 then
   index:=1;
  if index>length(s) then
   begin
     index:=length(s)+1;
     if index>high(s) then
      exit;
   end;
  indexlen:=Length(s)-Index+1;
  srclen:=length(Source);
  if sizeInt(length(source))+sizeint(length(s))>=sizeof(s) then
   begin
     cut:=sizeInt(length(source))+sizeint(length(s))-sizeof(s)+1;
     if cut>indexlen then
      begin
        dec(srclen,cut-indexlen);
        indexlen:=0;
      end
     else
      dec(indexlen,cut);
   end;
  fpc_shortstr_shortstr_intern_charmove(s,Index,s,Index+srclen,indexlen);
  fpc_shortstr_shortstr_intern_charmove(Source,1,s,Index,srclen);
  s[0]:=chr(index+srclen+indexlen-1);
end;
{$endif FPC_HAS_SHORTSTR_INSERT}


{$ifndef FPC_HAS_SHORTSTR_INSERT_CHAR}
{$define FPC_HAS_SHORTSTR_INSERT_CHAR}
procedure fpc_shortstr_insert_char(source : AnsiChar;var s : shortstring;index : SizeInt);
var
  indexlen : SizeInt;
begin
  if index<1 then
   index:=1;
  if index>length(s) then
   begin
     index:=length(s)+1;
     if index>high(s) then
      exit;
   end;
  indexlen:=Length(s)-Index+1;
  if (sizeint(length(s))+1=sizeof(s)) and (indexlen>0) then
   dec(indexlen);
  fpc_shortstr_shortstr_intern_charmove(s,Index,s,Index+1,indexlen);
  s[Index]:=Source;
  s[0]:=chr(index+indexlen);
end;
{$endif FPC_HAS_SHORTSTR_INSERT_CHAR}


{$ifndef FPC_HAS_SHORTSTR_POS_SHORTSTR}
{$define FPC_HAS_SHORTSTR_POS_SHORTSTR}
function pos(const substr : shortstring;const s : shortstring; Offset : Sizeint = 1):SizeInt;
var
  i,MaxLen,d : SizeInt;
begin
  Pos:=0;
  if (Length(SubStr)>0) and (Offset>0) and (Offset<=Length(S)) then
   begin
     MaxLen:=sizeint(Length(s))-Length(SubStr)+1;
     i:=Offset;
     while (i<=MaxLen) do
      begin
        d:=IndexByte(s[i],MaxLen-i+1,byte(substr[1]));
        if d<0 then
          exit;
        if (CompareByte(Substr[1],s[i+d],Length(SubStr))=0) then
          exit(i+d);
        i:=i+d+1;
      end;
   end;
end;
{$endif FPC_HAS_SHORTSTR_POS_SHORTSTR}


{$ifndef FPC_HAS_SHORTSTR_POS_CHAR}
{$define FPC_HAS_SHORTSTR_POS_CHAR}
{Faster when looking for a single AnsiChar...}
function pos(c:ansichar;const s:shortstring; Offset : Sizeint = 1 ):SizeInt;
var
  idx : SizeInt;
begin
  Pos:=0;
  if (Offset<1) or (Offset>Length(S)) then
    exit;
  idx:=IndexByte(s[Offset],length(s)-Offset+1,byte(c));
  if idx>=0 then
    Pos:=Offset+idx;
end;
{$endif FPC_HAS_SHORTSTR_POS_CHAR}


function fpc_char_copy(c:ansichar;index : SizeInt;count : SizeInt): shortstring;compilerproc;
begin
  if (index=1) and (Count>0) then
   fpc_char_Copy:=c
  else
   fpc_char_Copy:='';
end;

function pos(const substr : shortstring;c:Ansichar;  Offset : Sizeint = 1): SizeInt;
begin
  if (length(substr)=1) and (substr[1]=c) and (Offset=1) then
   Pos:=1
  else
   Pos:=0;
end;


{$if not defined(FPC_UPCASE_CHAR) or not defined(FPC_LOWERCASE_CHAR)}
{$ifdef IBM_CHAR_SET}
const
  UpCaseTbl : shortstring[7]=#154#142#153#144#128#143#165;
  LoCaseTbl : shortstring[7]=#129#132#148#130#135#134#164;
{$endif}
{$endif}

{$ifndef FPC_UPCASE_CHAR}
{$define FPC_UPCASE_CHAR}
function upcase(c : Ansichar) : Ansichar;
{$IFDEF IBM_CHAR_SET}
var
  i : ObjpasInt;
{$ENDIF}
begin
  if (c in ['a'..'z']) then
    upcase:=AnsiChar(byte(c)-32)
  else
{$IFDEF IBM_CHAR_SET}
    begin
      i:=Pos(c,LoCaseTbl);
      if i>0 then
       upcase:=UpCaseTbl[i]
      else
       upcase:=c;
    end;
{$ELSE}
   upcase:=c;
{$ENDIF}
end;
{$endif FPC_UPCASE_CHAR}


{$ifndef FPC_UPCASE_SHORTSTR}
{$define FPC_UPCASE_SHORTSTR}
function upcase(const s : shortstring) : shortstring;
var
  i : ObjpasInt;
begin
  upcase[0]:=s[0];
  for i := 1 to length (s) do
    upcase[i] := upcase (s[i]);
end;
{$endif FPC_UPCASE_SHORTSTR}


{$ifndef FPC_LOWERCASE_CHAR}
{$define FPC_LOWERCASE_CHAR}
function lowercase(c : AnsiChar) : AnsiChar;overload;
{$IFDEF IBM_CHAR_SET}
var
  i : ObjpasInt;
{$ENDIF}
begin
  if (c in ['A'..'Z']) then
   lowercase:=AnsiChar(byte(c)+32)
  else
{$IFDEF IBM_CHAR_SET}
   begin
     i:=Pos(c,UpCaseTbl);
     if i>0 then
      lowercase:=LoCaseTbl[i]
     else
      lowercase:=c;
   end;
 {$ELSE}
   lowercase:=c;
 {$ENDIF}
end;
{$endif FPC_LOWERCASE_CHAR}


{$ifndef FPC_LOWERCASE_SHORTSTR}
{$define FPC_LOWERCASE_SHORTSTR}
function lowercase(const s : shortstring) : shortstring; overload;
var
  i : ObjpasInt;
begin
  lowercase [0]:=s[0];
  for i:=1 to length(s) do
   lowercase[i]:=lowercase (s[i]);
end;
{$endif FPC_LOWERCASE_SHORTSTR}

const
  HexTbl : array[0..15] of AnsiChar='0123456789ABCDEF';

function hexstr(val : longint;cnt : byte) : shortstring;
var
  i : ObjpasInt;
begin
  hexstr[0]:=AnsiChar(cnt);
  for i:=cnt downto 1 do
   begin
     hexstr[i]:=hextbl[val and $f];
     val:=val shr 4;
   end;
end;

function octstr(val : longint;cnt : byte) : shortstring;
var
  i : ObjpasInt;
begin
  octstr[0]:=AnsiChar(cnt);
  for i:=cnt downto 1 do
   begin
     octstr[i]:=hextbl[val and 7];
     val:=val shr 3;
   end;
end;


function binstr(val : longint;cnt : byte) : shortstring;
var
  i : ObjpasInt;
begin
  binstr[0]:=AnsiChar(cnt);
  for i:=cnt downto 1 do
   begin
     binstr[i]:=AnsiChar(48+val and 1);
     val:=val shr 1;
   end;
end;


function hexstr(val : int64;cnt : byte) : shortstring;
var
  i : ObjpasInt;
begin
  hexstr[0]:=AnsiChar(cnt);
  for i:=cnt downto 1 do
   begin
     hexstr[i]:=hextbl[val and $f];
     val:=val shr 4;
   end;
end;


function octstr(val : int64;cnt : byte) : shortstring;
var
  i : ObjpasInt;
begin
  octstr[0]:=AnsiChar(cnt);
  for i:=cnt downto 1 do
   begin
     octstr[i]:=hextbl[val and 7];
     val:=val shr 3;
   end;
end;


function binstr(val : int64;cnt : byte) : shortstring;
var
  i : ObjpasInt;
begin
  binstr[0]:=AnsiChar(cnt);
  for i:=cnt downto 1 do
   begin
     binstr[i]:=AnsiChar(48+val and 1);
     val:=val shr 1;
   end;
end;

{$ifndef FPC_HAS_QWORD_HEX_SHORTSTR}
{$define FPC_HAS_QWORD_HEX_SHORTSTR}
Function  hexStr(Val:qword;cnt:byte):shortstring;
begin
  hexStr:=hexStr(int64(Val),cnt);
end;
{$endif FPC_HAS_QWORD_HEX_SHORTSTR}


{$ifndef FPC_HAS_QWORD_OCT_SHORTSTR}
{$define FPC_HAS_QWORD_OCT_SHORTSTR}
Function  OctStr(Val:qword;cnt:byte):shortstring;
begin
  OctStr:=OctStr(int64(Val),cnt);
end;
{$endif FPC_HAS_QWORD_OCT_SHORTSTR}


{$ifndef FPC_HAS_QWORD_BIN_SHORTSTR}
{$define FPC_HAS_QWORD_BIN_SHORTSTR}
Function  binStr(Val:qword;cnt:byte):shortstring;
begin
  binStr:=binStr(int64(Val),cnt);
end;
{$endif FPC_HAS_QWORD_BIN_SHORTSTR}


{$ifndef FPC_HAS_HEXSTR_POINTER_SHORTSTR}
{$define FPC_HAS_HEXSTR_POINTER_SHORTSTR}
function hexstr(val : pointer) : shortstring;
var
  i : ObjpasInt;
  v : ptruint;
begin
  v:=ptruint(val);
  hexstr[0]:=chr(sizeof(pointer)*2);
  for i:=sizeof(pointer)*2 downto 1 do
   begin
     hexstr[i]:=hextbl[v and $f];
     v:=v shr 4;
   end;
end;
{$endif FPC_HAS_HEXSTR_POINTER_SHORTSTR}


{$ifndef FPC_HAS_SPACE_SHORTSTR}
{$define FPC_HAS_SPACE_SHORTSTR}
function space (b : byte): shortstring;
begin
  space[0] := chr(b);
  FillChar (Space[1],b,' ');
end;
{$endif FPC_HAS_SPACE_SHORTSTR}

{*****************************************************************************
                              Str() Helpers
*****************************************************************************}

procedure fpc_shortstr_SInt(v : valSInt;len : SizeInt;out s : shortstring);[public,alias:'FPC_SHORTSTR_SINT']; compilerproc;
begin
  int_str(v,s);
  if length(s)<len then
    s:=space(len-length(s))+s;
end;


procedure fpc_shortstr_UInt(v : valUInt;len : SizeInt;out s : shortstring);[public,alias:'FPC_SHORTSTR_UINT']; compilerproc;
begin
  int_str_unsigned(v,s);
  if length(s)<len then
    s:=space(len-length(s))+s;
end;

{$ifndef CPU64}

procedure fpc_shortstr_qword(v : qword;len : SizeInt;out s : shortstring);[public,alias:'FPC_SHORTSTR_QWORD']; compilerproc;
begin
  int_str_unsigned(v,s);
  if length(s)<len then
    s:=space(len-length(s))+s;
end;


procedure fpc_shortstr_int64(v : int64;len : SizeInt;out s : shortstring);[public,alias:'FPC_SHORTSTR_INT64'];  compilerproc;
begin
  int_str(v,s);
  if length(s)<len then
    s:=space(len-length(s))+s;
end;

{$endif CPU64}

{$if defined(CPU16) or defined(CPU8)}
procedure fpc_shortstr_longword(v : longword;len : SizeInt;out s : shortstring);[public,alias:'FPC_SHORTSTR_LONGWORD']; compilerproc;
begin
  int_str_unsigned(v,s);
  if length(s)<len then
    s:=space(len-length(s))+s;
end;


procedure fpc_shortstr_longint(v : longint;len : SizeInt;out s : shortstring);[public,alias:'FPC_SHORTSTR_LONGINT'];  compilerproc;
begin
  int_str(v,s);
  if length(s)<len then
    s:=space(len-length(s))+s;
end;


procedure fpc_shortstr_word(v : word;len : SizeInt;out s : shortstring);[public,alias:'FPC_SHORTSTR_WORD']; compilerproc;
begin
  int_str_unsigned(v,s);
  if length(s)<len then
    s:=space(len-length(s))+s;
end;


procedure fpc_shortstr_smallint(v : smallint;len : SizeInt;out s : shortstring);[public,alias:'FPC_SHORTSTR_SMALLINT'];  compilerproc;
begin
  int_str(v,s);
  if length(s)<len then
    s:=space(len-length(s))+s;
end;
{$endif CPU16 or CPU8}


{ fpc_shortstr_sInt must appear before this file is included, because }
{ it's used inside real2str.inc and otherwise the searching via the      }
{ compilerproc name will fail (JM)                                       }

{$ifndef FPUNONE}
{$I flt_conv.inc}
{$endif}

{$ifndef FPUNONE}
procedure fpc_shortstr_float(d : ValReal;len,fr,rt : SizeInt;out s : shortstring);[public,alias:'FPC_SHORTSTR_FLOAT']; compilerproc;
begin
  str_real(len,fr,d,treal_type(rt),s);
end;
{$endif}

{$ifndef FPC_STR_ENUM_INTERN}
function fpc_shortstr_enum_intern(ordinal,len:sizeint;typinfo,ord2strindex:pointer;out s:shortstring): longint;
{$ifndef FPC_HAS_FEATURE_RTTI}
begin
  int_str(ordinal,s);
  if length(s)<len then
    s:=space(len-length(s))+s;
end;
{$else with RTTI feature}
{ The following contains the TTypeInfo/TTypeData records from typinfo.pp
  specialized for the tkEnumeration case (and stripped of unused things). }
type
  PPstring=^Pstring;

  Penum_typeinfo=^Tenum_typeinfo;
  Tenum_typeinfo={$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}packed{$endif}record
    kind:TTypeKind; { always tkEnumeration }
    num_chars:byte;
    chars:array[0..0] of AnsiChar; { variable length with size of num_chars }
  end;

{$push}
{$packrecords c}
  Penum_typedata=^Tenum_typedata;
  Tenum_typedata={$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}packed{$endif}record
{$if declared(TRttiDataCommon)}
    Common: TRttiDataCommon;
{$endif}
    case TTypeKind of
      tkInt64,tkQWord,tkInteger,tkChar,tkEnumeration,tkBool,tkWChar: (
         OrdType : Byte;
         case TTypeKind of
            tkInteger,tkChar,tkEnumeration,tkBool,tkWChar: (
              MinValue,MaxValue : Longint;
              case TTypeKind of
                tkEnumeration: (
                  BaseTypeRef : pointer
                  );
            {tkBool with OrdType=otSQWord }
            tkInt64:
              (MinInt64Value, MaxInt64Value: Int64);
            {tkBool with OrdType=otUQWord }
            tkQWord:
              (MinQWordValue, MaxQWordValue: QWord);
         );
    );
    { more data here, but not needed }
  end;

  { Pascal data types for the ordinal enum value to string table. It consists of a header
    that indicates what type of data the table stores, either a direct lookup table (when
    o = lookup) or a set of ordered (ordinal value, string) tuples (when o = search). }

  { A single entry in the set of ordered tuples }
  Psearch_data=^Tsearch_data;
  Tsearch_data={$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}packed{$endif}record
    value:longint;
    name:Pstring;
  end;

  Penum_ord_to_string=^Tenum_ord_to_string;
  Tenum_ord_to_string={$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}packed{$endif}record
    o:(lookup,search);
    case integer of
      0: (lookup_data:array[0..0] of Pstring);
      1: (num_entries:longint;
          search_data:array[0..0] of Tsearch_data);
  end;
{$pop}
var
  enum_o2s : Penum_ord_to_string;
  header:Penum_typeinfo;
  body:Penum_typedata;

  res:Pshortstring;
  sorted_data:Psearch_data;
  spaces,i,m,h,l:longint;

begin
  { set default return value }
  fpc_shortstr_enum_intern:=107;

  enum_o2s:=Penum_ord_to_string(ord2strindex);
  { depending on the type of table in ord2strindex retrieve the data }
  if (enum_o2s^.o=lookup) then
    begin
      { direct lookup table }
      header:=Penum_typeinfo(typinfo);
      { calculate address of enum rtti body: add the actual size of the
        enum_rtti_header, and then align. Use an alignment of 1 (which
        does nothing) in case FPC_REQUIRES_PROPER_ALIGNMENT is not set
        to avoid the need for an if in this situation }

      body:=Penum_typedata(aligntoqword(pointer(header) + 2 * sizeof(byte) { kind, num_chars } + header^.num_chars));

      with body^ do
        begin
          { Bounds check for the ordinal value for this enum }
          if (ordinal<minvalue) or (ordinal>maxvalue) then
            exit;
          { make the ordinal index for lookup zero-based }
          dec(ordinal,minvalue);
        end;
      { temporarily disable range checking because of the access to the array[0..0]
        member of Tenum_ord_to_string_lookup }
{$push}{$R-}
        res:=enum_o2s^.lookup_data[ordinal];
{$pop}
        if (not assigned(res)) then
          exit;
        s:=res^;
    end
  else
    begin
      { The compiler did generate a sorted array of (ordvalue,Pstring) tuples }
      sorted_data:=@enum_o2s^.search_data;
      { Use a binary search to get the string }
      l:=0;
      { temporarily disable range checking because of the access to the array[0..0]
        member of Tenum_ord_to_string_search }
{$push}{$R-}
      h:=enum_o2s^.num_entries-1;
      repeat
        m:=(l+h) div 2;
        if ordinal>sorted_data[m].value then
          l:=m+1
        else if ordinal<sorted_data[m].value then
          h:=m-1
        else
          break;
        if l>h then
          exit; { Ordinal value not found? Exit }
      until false;
{$pop}
      s:=sorted_data[m].name^;
    end;

  { Pad the string with spaces if necessary }
  if (len>length(s)) then
    begin
      spaces:=len-length(s);
      for i:=1 to spaces do
        s[length(s)+i]:=' ';
      inc(byte(s[0]),spaces);
    end;
  fpc_shortstr_enum_intern:=0;
end;
{$endif with RTTI feature}

procedure fpc_shortstr_enum(ordinal,len:sizeint;typinfo,ord2strindex:pointer;out s:shortstring);[public,alias:'FPC_SHORTSTR_ENUM'];compilerproc;
var
  res: longint;
begin
  res:=fpc_shortstr_enum_intern(ordinal,len,typinfo,ord2strindex,s);
  if (res<>0) then
    runerror(107);
end;

{ also define alias for internal use in the system unit }
procedure fpc_shortstr_enum(ordinal,len:sizeint;typinfo,ord2strindex:pointer;out s:shortstring);external name 'FPC_SHORTSTR_ENUM';
{$endif FPC_SHORTSTR_ENUM_INTERN}


procedure fpc_shortstr_bool(b : boolean;len:sizeint;out s:shortstring);[public,alias:'FPC_SHORTSTR_BOOL'];compilerproc;
begin
  if b then
    s:='TRUE'
  else
    s:='FALSE';
  if length(s)<len then
    s:=space(len-length(s))+s;
end;

{ also define alias for internal use in the system unit }
procedure fpc_shortstr_bool(b : boolean;len:sizeint;out s:shortstring);external {$ifndef cpujvm}name 'FPC_SHORTSTR_BOOL'{$endif};

procedure fpc_shortstr_currency({$ifdef cpujvm}constref{$endif} c : currency; len,f : SizeInt; out s : shortstring);[public,alias:'FPC_SHORTSTR_CURRENCY']; compilerproc;
const
  MinLen = 8; { Minimal string length in scientific format }
var
  buf : array[1..19] of AnsiChar;
  i,j,k,reslen,tlen,sign,r,point : ObjpasInt;
  ic : qword;
begin
  fillchar(buf,length(buf),'0');
  { default value for length is -32767 }
  if len=-32767 then
    len:=25;
  if PInt64(@c)^ >= 0 then
    begin
      ic:=QWord(PInt64(@c)^);
      sign:=0;
    end
  else
    begin
      sign:=1;
      ic:=QWord(-PInt64(@c)^);
    end;
  { converting to integer string }
  tlen:=0;
  repeat
    Inc(tlen);
    buf[tlen]:=Chr(ic mod 10 + $30);
    ic:=ic div 10;
  until ic = 0;
  { calculating:
     reslen - length of result string,
     r      - rounding or appending zeroes,
     point  - place of decimal point        }
  reslen:=tlen;
  if f <> 0 then
    Inc(reslen); { adding decimal point length }
  if f < 0 then
    begin
      { scientific format }
      Inc(reslen,5); { adding length of sign and exponent }
      if len < MinLen then
        len:=MinLen;
      r:=reslen-len;
      if reslen < len then
        reslen:=len;
      if r > 0 then
        begin
          reslen:=len;
          point:=tlen - r;
        end
      else
        point:=tlen;
    end
  else
    begin
      { fixed format }
      Inc(reslen, sign);
      { prepending fractional part with zeroes }
      while tlen < 5 do
        begin
          Inc(reslen);
          Inc(tlen);
          buf[tlen]:='0';
        end;
      { Currency have 4 digits in fractional part }
      r:=4 - f;
      point:=f;
      if point <> 0 then
        begin
          if point > 4 then
            point:=4;
          Inc(point);
        end;
      Dec(reslen,r);
    end;
  { rounding string if r > 0 }
  if r > 0 then
    begin
      k := 0;
      i := r+2;
      if i > tlen then
         i := tlen+1;
      if buf[i-2] >= '5' then
         begin
           if buf[i-1] < '9' then
             buf[i-1] := chr(ord(buf[i-1])+1)
           else
             begin
               buf[i-1] := '0';
               k := 1;
             end;
         end;
      If (k=1) and (buf[i-1]='0') then
	    begin
		  { 1.9996 rounded to two decimal digits after the decimal separator must result in
		    2.00, i.e. the rounding is propagated
		  }
          while buf[i]='9' do
		    begin
			  buf[i]:='0';
     		  inc(i);
		    end;
		  buf[i]:=chr(Ord(buf[i])+1);
		  { did we add another digit? This happens when rounding
		    e.g. 99.9996 to two decimal digits after the decimal separator which should result in
			100.00
		  }
		  if i>tlen then
		    begin
			  inc(reslen);
			  inc(tlen);
			end;
		end;		
    end;
  { preparing result string }
  if reslen<len then
    reslen:=len;
  if reslen>High(s) then
    begin
      if r < 0 then
        Inc(r, reslen - High(s));
      reslen:=High(s);
    end;
  SetLength(s,reslen);
  j:=reslen;
  if f<0 then
    begin
      { writing power of 10 part }
      if PInt64(@c)^ = 0 then
        k:=0
      else
        k:=tlen-5;
      if k >= 0 then
        s[j-2]:='+'
      else
        begin
          s[j-2]:='-';
          k:=-k;
        end;
      s[j]:=Chr(k mod 10 + $30);
      Dec(j);
      s[j]:=Chr(k div 10 + $30);
      Dec(j,2);
      s[j]:='E';
      Dec(j);
    end;
  { writing extra zeroes if r < 0 }
  while r < 0 do
    begin
      s[j]:='0';
      Dec(j);
      Inc(r);
    end;
  { writing digits and decimal point }
  for i:=r + 1 to tlen do
    begin
      Dec(point);
      if point = 0 then
        begin
          s[j]:='.';
          Dec(j);
        end;
      s[j]:=buf[i];
      Dec(j);
    end;
  { writing sign }
  if sign = 1 then
    begin
      s[j]:='-';
      Dec(j);
    end;
  { writing spaces }
  while j > 0 do
    begin
      s[j]:=' ';
      Dec(j);
    end;
end;

{
   Array Of AnsiChar Str() helpers
}

procedure fpc_chararray_sint(v : valsint;len : SizeInt;out a:array of AnsiChar);compilerproc;
var
  ss : shortstring;
  maxlen : SizeInt;
begin
  int_str(v,ss);
  if length(ss)<len then
    ss:=space(len-length(ss))+ss;
  if length(ss)<high(a)+1 then
    maxlen:=length(ss)
  else
    maxlen:=high(a)+1;
  fpc_shortstr_chararray_intern_charmove(ss,a,maxlen);
end;


procedure fpc_chararray_uint(v : valuint;len : SizeInt;out a : array of AnsiChar);compilerproc;
var
  ss : shortstring;
  maxlen : SizeInt;
begin
  int_str_unsigned(v,ss);
  if length(ss)<len then
    ss:=space(len-length(ss))+ss;
  if length(ss)<high(a)+1 then
    maxlen:=length(ss)
  else
    maxlen:=high(a)+1;
  fpc_shortstr_chararray_intern_charmove(ss,a,maxlen);
end;


{$ifndef CPU64}

procedure fpc_chararray_qword(v : qword;len : SizeInt;out a : array of AnsiChar);compilerproc;
{$ifdef EXCLUDE_COMPLEX_PROCS}
begin
  runerror(219);
end;
{$else EXCLUDE_COMPLEX_PROCS}
var
  ss : shortstring;
  maxlen : SizeInt;
begin
  int_str_unsigned(v,ss);
  if length(ss)<len then
    ss:=space(len-length(ss))+ss;
  if length(ss)<high(a)+1 then
    maxlen:=length(ss)
  else
    maxlen:=high(a)+1;
  fpc_shortstr_chararray_intern_charmove(ss,a,maxlen);
end;
{$endif EXCLUDE_COMPLEX_PROCS}


procedure fpc_chararray_int64(v : int64;len : SizeInt;out a : array of AnsiChar);compilerproc;
{$ifdef EXCLUDE_COMPLEX_PROCS}
begin
  runerror(219);
end;
{$else EXCLUDE_COMPLEX_PROCS}
var
  ss : shortstring;
  maxlen : SizeInt;
begin
  int_str(v,ss);
  if length(ss)<len then
    ss:=space(len-length(ss))+ss;
  if length(ss)<high(a)+1 then
    maxlen:=length(ss)
  else
    maxlen:=high(a)+1;
  fpc_shortstr_chararray_intern_charmove(ss,a,maxlen);
end;
{$endif EXCLUDE_COMPLEX_PROCS}

{$endif CPU64}


{$if defined(CPU16) or defined(CPU8)}

procedure fpc_chararray_longword(v : longword;len : SizeInt;out a : array of AnsiChar);compilerproc;
var
  ss : shortstring;
  maxlen : SizeInt;
begin
  int_str_unsigned(v,ss);
  if length(ss)<len then
    ss:=space(len-length(ss))+ss;
  if length(ss)<high(a)+1 then
    maxlen:=length(ss)
  else
    maxlen:=high(a)+1;
  fpc_shortstr_chararray_intern_charmove(ss,a,maxlen);
end;


procedure fpc_chararray_longint(v : longint;len : SizeInt;out a : array of AnsiChar);compilerproc;
var
  ss : shortstring;
  maxlen : SizeInt;
begin
  int_str(v,ss);
  if length(ss)<len then
    ss:=space(len-length(ss))+ss;
  if length(ss)<high(a)+1 then
    maxlen:=length(ss)
  else
    maxlen:=high(a)+1;
  fpc_shortstr_chararray_intern_charmove(ss,a,maxlen);
end;


procedure fpc_chararray_word(v : word;len : SizeInt;out a : array of AnsiChar);compilerproc;
var
  ss : shortstring;
  maxlen : SizeInt;
begin
  int_str_unsigned(v,ss);
  if length(ss)<len then
    ss:=space(len-length(ss))+ss;
  if length(ss)<high(a)+1 then
    maxlen:=length(ss)
  else
    maxlen:=high(a)+1;
  fpc_shortstr_chararray_intern_charmove(ss,a,maxlen);
end;


procedure fpc_chararray_smallint(v : smallint;len : SizeInt;out a : array of AnsiChar);compilerproc;
var
  ss : shortstring;
  maxlen : SizeInt;
begin
  int_str(v,ss);
  if length(ss)<len then
    ss:=space(len-length(ss))+ss;
  if length(ss)<high(a)+1 then
    maxlen:=length(ss)
  else
    maxlen:=high(a)+1;
  fpc_shortstr_chararray_intern_charmove(ss,a,maxlen);
end;

{$endif CPU16 or CPU8}


{$ifndef FPUNONE}
procedure fpc_chararray_Float(d : ValReal;len,fr,rt : SizeInt;out a : array of AnsiChar);compilerproc;
var
  ss : shortstring;
  maxlen : SizeInt;
begin
  str_real(len,fr,d,treal_type(rt),ss);
  if length(ss)<high(a)+1 then
    maxlen:=length(ss)
  else
    maxlen:=high(a)+1;
  fpc_shortstr_chararray_intern_charmove(ss,a,maxlen);
end;
{$endif}

{$ifndef FPC_STR_ENUM_INTERN}
{ currently, the avr code generator fails on this procedure, so we disable it,
  this is not a good solution but fixing compilation of this procedure for
  avr is hard, requires significant changes to the register allocator to take
  care of different register classes }
procedure fpc_chararray_enum(ordinal,len:sizeint;typinfo,ord2strindex:pointer;out a : array of AnsiChar);compilerproc;
var
  ss : shortstring;
  maxlen : SizeInt;
begin
{$ifdef EXCLUDE_COMPLEX_PROCS}
  runerror(219);
{$else EXCLUDE_COMPLEX_PROCS}
  fpc_shortstr_enum(ordinal,len,typinfo,ord2strindex,ss);
  if length(ss)<high(a)+1 then
    maxlen:=length(ss)
  else
    maxlen:=high(a)+1;
  fpc_shortstr_chararray_intern_charmove(ss,a,maxlen);
{$endif EXCLUDE_COMPLEX_PROCS}
end;
{$endif not FPC_STR_ENUM_INTERN}

procedure fpc_chararray_bool(b : boolean;len:sizeint;out a : array of AnsiChar);compilerproc;
var
  ss : shortstring;
  maxlen : SizeInt;
begin
  fpc_shortstr_bool(b,len,ss);
  if length(ss)<high(a)+1 then
    maxlen:=length(ss)
  else
    maxlen:=high(a)+1;
  fpc_shortstr_chararray_intern_charmove(ss,a,maxlen);
end;


{$ifndef FPC_HAS_CHARARRAY_CURRENCY}
{$define FPC_HAS_CHARARRAY_CURRENCY}
procedure fpc_chararray_Currency(c : Currency;len,fr : SizeInt;out a : array of AnsiChar);compilerproc;
{$ifdef EXCLUDE_COMPLEX_PROCS}
begin
  runerror(217);
end;
{$else EXCLUDE_COMPLEX_PROCS}
var
  ss : shortstring;
  maxlen : SizeInt;
begin
  str(c:len:fr,ss);
  if length(ss)<high(a)+1 then
    maxlen:=length(ss)
  else
    maxlen:=high(a)+1;
  fpc_shortstr_chararray_intern_charmove(ss,a,maxlen);
end;
{$endif EXCLUDE_COMPLEX_PROCS}
{$endif FPC_HAS_CHARARRAY_CURRENCY}

{*****************************************************************************
                           Val() Functions
*****************************************************************************}

type
  ValCommon=record
    negative:boolean;
    base,baseIndex:byte; { baseIndex is flattened base for lookups: 0 — base 10, 1 — base 16, 2 — base 2, 3 — base 8. }
    minusPos:SizeInt;
  end;

Function InitVal(const s:shortstring;out vc:ValCommon):ValSInt;
var
  ns : SizeInt;
begin
  result:=1;
  vc.negative:=false;
  vc.base:=10;
  vc.baseIndex:=0;
  ns:=length(s);
{Skip Spaces and Tab}
  while (result<=ns) and (s[result] in [' ',#9]) do
   inc(result);
{Sign}
  if result<=ns then
   case s[result] of
    '-' : begin
            vc.negative:=true;
            vc.minusPos:=result;
            inc(result);
          end;
    '+' : inc(result);
   end;
{Base}
  if result<=ns then
   case s[result] of
    '$',
    'X',
    'x' : begin
            vc.base:=16;
            vc.baseIndex:=1;
            inc(result);
          end;
    '%' : begin
            vc.base:=2;
            vc.baseIndex:=2;
            inc(result);
          end;
    '&' : begin
            vc.base:=8;
            vc.baseIndex:=3;
            inc(result);
          end;
    '0' : if (result<ns) and (s[result+1] in ['x', 'X']) then
          begin
            vc.base:=16;
            vc.baseIndex:=1;
            inc(result, 2);
          end;
   end;
  { strip leading zeros }
  while (result<ns) and (s[result]='0') and (s[result+1]<>#0) do
    inc(result);
end;

const
  ValData: record
    ValueArray: array[0..ord('f')-ord('0')] of byte;
    MaxDigits: array[0 .. 2 { unsigned / signed positive / signed negative }, 0 .. 3 { base index }, 0 .. 3 { Bsr(DestSize) }] of byte;
  end =
  (
    ValueArray:
    (
      0,1,2,3,4,5,6,7,8,9,$FF,$FF,$FF,$FF,$FF,$FF,$FF,10,11,12,13,14,15,
      $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,
      10,11,12,13,14,15
    );

    { If VAL input has exactly this many digits (without leading zeros), then it may overflow.
      If it has more digits, it definitely overflows.
      If it has less, it definitely doesn’t overflow. }
    MaxDigits:
    (
      ( { unsigned }
        (3, 5, 10, 20), { base 10: 255 / 65535 / 4_294_967_295 / 18_446_744_073_709_551_615 }
        (2, 4, 8, 16), { base 16: $FF / $FFFF / $FFFF_FFFF / $FFFF_FFFF_FFFF_FFFF }
        (8, 16, 32, 64), { base 2: %1111_1111 / %1111_1111_1111_1111 / %1111_1111_1111_1111_1111_1111_1111_1111 / %1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111 }
        (3, 6, 11, 22) { base 8: &377 / &17_7777 / &377_7777_7777 / &17_7777_7777_7777_7777_7777 }
      ),
      ( { signed positive }
        (3, 5, 10, 19), { base 10: 127 / 32767 / 2_147_483_647 / 9_223_372_036_854_775_807 }
        (2, 4, 8, 16), { base 16: $7F / $7FFF / $7FFF_FFFF / $7FFF_FFFF_FFFF_FFFF }
        (7, 15, 31, 63), { base 2: %111_1111 / %111_1111_1111_1111 / %111_1111_1111_1111_1111_1111_1111_1111 / %111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111 }
        (3, 5, 11, 21) { base 8: &177 / &7_7777 / &177_7777_7777 / &7_7777_7777_7777_7777_7777 }
      ),
      ( { signed negative }
        (3, 5, 10, 19), { base 10: 128 / 32768 / 2_147_483_648 / 9_223_372_036_854_775_808 }
        (2, 4, 8, 16), { base 16: $80 / $8000 / $8000_0000 / $8000_0000_0000_0000 }
        (8, 16, 32, 64), { base 2: %1000_0000 / %1000_0000_0000_0000 / %1000_0000_0000_0000_0000_0000_0000_0000 / %1000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000 }
        (3, 6, 11, 22) { base 8: &200 / &10_0000 / &200_0000_0000 / &10_0000_0000_0000_0000_0000 }
      )
    )
  );

type
  ValNonZeroBase = 1 .. ord(High(ValCommon.base));
  ValNonZeroDestSize = 1 .. ord(High(uint32));

Function fpc_Val_SInt_ShortStr(DestSize: SizeInt; Const S: ShortString; out Code: ValSInt): ValSInt; [public, alias:'FPC_VAL_SINT_SHORTSTR']; compilerproc;
var
  sp,ns : SizeInt;
  u : SizeUInt;
  digitsLeft,sh : ALUSint;
  temp, prev, lim: ValUInt;
  vc: ValCommon;
begin
  fpc_Val_SInt_ShortStr:=0;
  sp:=InitVal(s,vc);
  ns:=length(s);
  if (sp>ns) or (s[sp]=#0) then
   begin
     Code:=sp;
     exit;
   end;

  digitsLeft:=ValData.MaxDigits[ord((vc.base=10) or vc.negative)+ord(vc.negative), vc.baseIndex, BsrDWord(ValNonZeroDestSize(DestSize))];
  Temp:=0;
  prev:=0;
  repeat
    u:=SizeUint(ord(s[sp])-ord('0'));
    if u>=length(ValData.ValueArray) then
      break;
    u:=ValData.ValueArray[u];
    If u>=vc.base then
      break;
    dec(digitsLeft);
    if digitsLeft<0 then
      break;
    prev:=Temp;
    Temp:=Temp*ValUInt(vc.base)+u;
    inc(sp);
  until sp>ns;

  if digitsLeft<=0 then { Maybe an overflow. If so, step 1 digit back. }
   begin
     if sp<=ns then { If the loop was stopped not by sp>ns check... }
       u:=ValData.ValueArray[ord(s[sp-1])-ord('0')]; { Then recover previous u. }
     lim:=High(lim) shr (bitsizeof(lim)-8*DestSize);
     if (vc.base=10) or vc.negative then
       lim:=lim shr 1+Ord(vc.negative); { Convert to signed limit. }
     if prev>ValUInt(lim-u) div ValNonZeroBase(vc.base) then
       dec(sp); { Overflow. Step 1 digit back. }
   end;

  if (sp<=ns) and (s[sp]<>#0) then
   begin
     Code:=sp;
     exit;
   end;

  Code:=0;
  fpc_Val_SInt_ShortStr := ValSInt(Temp);
  If vc.Negative Then
    fpc_Val_SInt_ShortStr := -fpc_Val_SInt_ShortStr;

  If Not(vc.Negative) and (vc.base<>10) and (DestSize<sizeof(fpc_Val_SInt_ShortStr)) Then
   begin
     {sign extend the result to allow proper range checking}
     sh:=bitsizeof(fpc_Val_SInt_ShortStr)-8*DestSize;
     fpc_Val_SInt_ShortStr:=
{$if sizeof(ValSint)=sizeof(int64)} SarInt64
{$elseif sizeof(ValSint)=sizeof(int32)} SarLongint
{$elseif sizeof(ValSint)=sizeof(int16)} SarSmallint
{$else} {$error unknown ValSint size}
{$endif} (fpc_Val_SInt_ShortStr shl sh, sh);
    End;
end;


{$ifndef FPC_HAS_INT_VAL_SINT_SHORTSTR}
{$define FPC_HAS_INT_VAL_SINT_SHORTSTR}
{ we need this for fpc_Val_SInt_Ansistr and fpc_Val_SInt_WideStr because }
{ we have to pass the DestSize parameter on (JM)                         }
Function int_Val_SInt_ShortStr(DestSize: SizeInt; Const S: ShortString; out Code: ValSInt): ValSInt; [external name 'FPC_VAL_SINT_SHORTSTR'];
{$endif FPC_HAS_INT_VAL_SINT_SHORTSTR}


Function fpc_Val_UInt_Shortstr({$ifndef VER3_2}DestSize: SizeInt;{$endif VER3_2} Const S: ShortString; out Code: ValSInt): ValUInt; [public, alias:'FPC_VAL_UINT_SHORTSTR']; compilerproc;
var
  sp,ns : SizeInt;
  u : SizeUInt;
  digitsLeft : ALUSint;
  prev: ValUInt;
  vc: ValCommon;
begin
  fpc_Val_UInt_Shortstr:=0;
  sp:=InitVal(s,vc);
  ns:=length(s);
  If vc.negative or (sp>ns) or (s[sp]=#0) Then
    begin
      if vc.negative then sp:=vc.minusPos;
      Code:=sp;
      Exit;
    end;

  digitsLeft:=ValData.MaxDigits[0, vc.baseIndex, BsrDWord(ValNonZeroDestSize({$ifndef VER3_2}DestSize{$else}sizeof(fpc_Val_UInt_Shortstr){$endif}))];
  prev:=0;
  repeat
    u:=SizeUint(ord(s[sp])-ord('0'));
    if u>=length(ValData.ValueArray) then
      break;
    u:=ValData.ValueArray[u];
    If u>=vc.base then
      break;
    dec(digitsLeft);
    if digitsLeft<0 then
      break;
    prev:=fpc_Val_UInt_Shortstr;
    fpc_Val_UInt_Shortstr:=fpc_Val_UInt_Shortstr*ValUInt(vc.base)+u;
    inc(sp);
  until sp>ns;

  if digitsLeft<=0 then { Maybe an overflow. If so, step 1 digit back. }
   begin
     if sp<=ns then { If the loop was stopped not by sp>ns check... }
       u:=ValData.ValueArray[ord(s[sp-1])-ord('0')]; { Then recover previous u. }
     if prev>ValUInt(High(result) {$ifndef VER3_2} shr (bitsizeof(result)-8*DestSize) {$endif}-u) div ValNonZeroBase(vc.base) then
       dec(sp);
   end;

  code:=0;
  if (sp<=ns) and (s[sp]<>#0) then
   begin
     Code:=sp;
     fpc_Val_UInt_Shortstr:=0;
   end;
end;


{$ifndef CPU64}

  Function fpc_val_int64_shortstr(Const S: ShortString; out Code: ValSInt): Int64; [public, alias:'FPC_VAL_INT64_SHORTSTR']; compilerproc;

  var  u : sizeuint;
       sp,ns : sizeint;
       digitsLeft : ALUSint;
       temp, prev, lim : qword;
       vc : ValCommon;

  begin
  {$ifdef EXCLUDE_COMPLEX_PROCS}
    runerror(219);
  {$else EXCLUDE_COMPLEX_PROCS}
    fpc_val_int64_shortstr := 0;
    sp:=InitVal(s,vc);
    ns:=length(s);
    if (sp>ns) or (s[sp]=#0) then
     begin
       Code:=sp;
       exit;
     end;

    digitsLeft:=ValData.MaxDigits[ord((vc.base=10) or vc.negative)+ord(vc.negative), vc.baseIndex, 3];
    Temp:=0;
    prev:=0;
    repeat
      u:=SizeUint(ord(s[sp])-ord('0'));
      if u>=length(ValData.ValueArray) then
        break;
      u:=ValData.ValueArray[u];
      If u>=vc.base then
        break;
      dec(digitsLeft);
      if digitsLeft<0 then
        break;
      prev:=Temp;
      Temp:=Temp*vc.base+u;
      inc(sp);
    until sp>ns;

    if digitsLeft<=0 then { Maybe an overflow. If so, step 1 digit back. }
     begin
       if sp<=ns then { If the loop was stopped not by sp>ns check... }
         u:=ValData.ValueArray[ord(s[sp-1])-ord('0')]; { Then recover previous u. }
       lim:=High(lim);
       if (vc.base=10) or vc.negative then
        begin
          lim:=uint64(High(int64));
          if vc.negative then { lim:=uint64(High(int64))+ord(vc.negative) triggers #41148. }
            lim:=uint64(Low(int64));
        end;
       if prev>uint64(lim-u) div ValNonZeroBase(vc.base) then
         dec(sp); { Overflow. Step 1 digit back. }
     end;

    if (sp<=ns) and (s[sp]<>#0) then
     begin
       Code:=sp;
       exit;
     end;

    code:=0;
    fpc_val_int64_shortstr:=int64(Temp);
    if vc.negative then
      fpc_val_int64_shortstr:=-fpc_val_int64_shortstr;
  {$endif EXCLUDE_COMPLEX_PROCS}
  end;

  Function fpc_val_qword_shortstr(Const S: ShortString; out Code: ValSInt): QWord; [public, alias:'FPC_VAL_QWORD_SHORTSTR']; compilerproc;
  var
    sp,ns : SizeInt;
    u : SizeUInt;
    digitsLeft : ALUSint;
    prev: qword;
    vc: ValCommon;
  begin
    fpc_val_qword_shortstr:=0;
    sp:=InitVal(s,vc);
    ns:=length(s);
    If vc.negative or (sp>ns) or (s[sp]=#0) Then
      begin
        if vc.negative then sp:=vc.minusPos;
        Code:=sp;
        Exit;
      end;

    prev:=0;
    digitsLeft:=ValData.MaxDigits[0, vc.baseIndex, 3];
    repeat
      u:=SizeUint(ord(s[sp])-ord('0'));
      if u>=length(ValData.ValueArray) then
        break;
      u:=ValData.ValueArray[u];
      If u>=vc.base then
        break;
      dec(digitsLeft);
      if digitsLeft<0 then
        break;
      prev:=fpc_val_qword_shortstr;
      fpc_val_qword_shortstr:=fpc_val_qword_shortstr*vc.base+u;
      inc(sp);
    until sp>ns;

    if digitsLeft<=0 then { Maybe an overflow. If so, step 1 digit back. }
     begin
       if sp<=ns then { If the loop was stopped not by sp>ns check... }
         u:=ValData.ValueArray[ord(s[sp-1])-ord('0')]; { Then recover previous u. }
       if prev>qword(High(qword)-u) div ValNonZeroBase(vc.base) then
         dec(sp);
     end;

    code:=0;
    if (sp<=ns) and (s[sp]<>#0) then
     begin
       Code:=sp;
       fpc_val_qword_shortstr:=0;
     end;
  end;
{$endif CPU64}

{$if defined(CPU16) or defined(CPU8)}
  Function fpc_val_longint_shortstr(Const S: ShortString; out Code: ValSInt): LongInt; [public, alias:'FPC_VAL_LONGINT_SHORTSTR']; compilerproc;

  var  u, temp, prev, maxprevvalue, maxnewvalue : longword;
       vc : ValCommon;

  const maxlongint=longword($7fffffff);
        maxlongword=longword($ffffffff);

  begin
    fpc_val_longint_shortstr := 0;
    Temp:=0;
    Code:=InitVal(s,vc);
    if (Code>length(s)) or (s[Code]=#0) then
      exit;
    maxprevvalue := maxlongword div vc.base;
    if (vc.base = 10) then
      maxnewvalue := maxlongint + ord(vc.negative)
    else
      maxnewvalue := maxlongword;

    while Code<=Length(s) do
     begin
       u:=16;
       case s[code] of
         '0'..'f' : u:=ValData.ValueArray[ord(S[Code])-ord('0')];
         #0 : break;
       else
         ;
       end;

       Prev:=Temp;
       Temp:=Temp*longword(vc.base);
       If (u >= vc.base) or
         (longword(maxnewvalue-u) < temp) or
         (prev > maxprevvalue) Then
         Begin
           fpc_val_longint_shortstr := 0;
           Exit;
         End;
       Temp:=Temp+u;
       inc(code);
     end;
    code:=0;
    fpc_val_longint_shortstr:=longint(Temp);
    If vc.Negative Then
      fpc_val_longint_shortstr:=-fpc_val_longint_shortstr;
  end;


  Function fpc_val_longword_shortstr(Const S: ShortString; out Code: ValSInt): LongWord; [public, alias:'FPC_VAL_LONGWORD_SHORTSTR']; compilerproc;

  var  u, prev: LongWord;
       vc: ValCommon;

  const UpperLimit=High(longword);

  begin
    fpc_val_longword_shortstr:=0;
    Code:=InitVal(s,vc);
    If vc.Negative or (Code>length(s)) or (s[Code]=#0) Then
      Exit;
    while Code<=Length(s) do
     begin
       u:=16;
       case s[code] of
         '0'..'f' : u:=ValData.ValueArray[ord(S[Code])-ord('0')];
         #0 : break;
       else
         ;
       end;

       If (u>=vc.base) or
          (LongWord(UpperLimit-u) div LongWord(vc.Base)<fpc_val_longword_shortstr) then
        begin
          fpc_val_longword_shortstr:=0;
          exit;
        end;
       fpc_val_longword_shortstr:=fpc_val_longword_shortstr*vc.base + u;
       inc(code);
     end;
    code := 0;
  end;


  Function fpc_val_smallint_shortstr(Const S: ShortString; out Code: ValSInt): SmallInt; [public, alias:'FPC_VAL_SMALLINT_SHORTSTR']; compilerproc;

  var  u, temp, prev, maxprevvalue : word;
       UnsignedUpperLimit: ValUInt;
       vc: ValCommon;
  begin
    fpc_val_smallint_shortstr := 0;
    Temp:=0;
    Code:=InitVal(s,vc);
    if (vc.base=10) or vc.negative then
      UnsignedUpperLimit := Word(High(SmallInt))+Ord(vc.negative)
    else
      UnsignedUpperLimit := High(Word);
    if (Code>length(s)) or (s[Code]=#0) then
     exit;
    maxprevvalue := High(Word) div vc.base;

    while Code<=Length(s) do
     begin
       u:=16;
       case s[code] of
         '0'..'f' : u:=ValData.ValueArray[ord(S[Code])-ord('0')];
         #0 : break;
       else
         ;
       end;

       Prev:=Temp;
       Temp:=Temp*longword(vc.base);
       If (u >= vc.base) or
        (prev > maxPrevValue) or
        ((Temp)>(UnsignedUpperLimit-u)) Then
       Begin
         fpc_val_smallint_shortstr := 0;
         Exit
       End;
       Temp:=Temp+u;
       inc(code);
     end;
    code:=0;
    fpc_val_smallint_shortstr:=SmallInt(Temp);
    If vc.Negative Then
      fpc_val_smallint_shortstr:=-fpc_val_smallint_shortstr;
  end;


  Function fpc_val_word_shortstr(Const S: ShortString; out Code: ValSInt): Word; [public, alias:'FPC_VAL_WORD_SHORTSTR']; compilerproc;

  var  u, prev: word;
       vc: ValCommon;

  const UpperLimit=High(Word);  //this preserves 3.2 (and earlier) behaviour

  begin
    fpc_val_word_shortstr:=0;
    Code:=InitVal(s,vc);
    If vc.Negative or (Code>length(s)) or (s[Code]=#0) Then
      begin
        if vc.Negative then Code:=vc.minusPos;
        Exit;
      end;
    while Code<=Length(s) do
     begin
       u:=16;
       case s[code] of
         '0'..'f' : u:=ValData.ValueArray[ord(S[Code])-ord('0')];
         #0 : break;
       else
         ;
       end;

       If (u>=vc.base) or
          (Word(UpperLimit-u) div Word(vc.Base)<fpc_val_word_shortstr) then
        begin
          fpc_val_word_shortstr:=0;
          exit;
        end;
       fpc_val_word_shortstr:=fpc_val_word_shortstr*vc.base + u;
       inc(code);
     end;
    code := 0;
  end;
{$endif CPU16 or CPU8}

{$ifndef FPUNONE}
Function fpc_Val_Real_ShortStr(const s : shortstring; out Code : ValSInt): ValReal; [public, alias:'FPC_VAL_REAL_SHORTSTR']; compilerproc;
begin
    fpc_Val_Real_ShortStr := val_real( s, code );
end;
{$endif FPUNONE}

{$ifndef FPC_STR_ENUM_INTERN}
function fpc_val_enum_shortstr(str2ordindex:pointer;const s:shortstring;out code:valsint):longint; [public, alias:'FPC_VAL_ENUM_SHORTSTR']; compilerproc;

type  Psorted_array=^Tsorted_array;
      Tsorted_array={$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}packed{$endif}record
        o:longint;
        s:Pstring;
      end;

      Pstring_to_ord=^Tstring_to_ord;
      Tstring_to_ord={$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}packed{$endif}record
        count:longint;
        data:array[0..0] of Tsorted_array;
      end;

var l,r,l2,r2,m,sp,isp:SizeInt;
    c:char;

begin
  {Val for numbers accepts spaces at the start, so lets do the same
   for enums. Skip spaces at the start of the string.}
  sp:=1;
  while (sp<=length(s)) and (s[sp]=' ')  do
    inc(sp);

  { Let input be “abd” and sorted names be: _hm a aa ab aba abb abc abd ac ad b c
    Start:                                  L                                    ┘R (R points PAST the last item in the range.)
    After iteration 0 (“a” analyzed):           L                            ┘R
    After iteration 1 (“ab” analyzed):               L                 ┘R
    After iteration 2 (“abd” analyzed):                             L  ┘R }
  l:=0;
  r:=Pstring_to_ord(str2ordindex)^.count;
  dec(sp); { sp/isp are incremented at the beginning of the loop so that 'continue's advance sp/isp. }
  isp:=0; { isp is the position without spaces. }
  repeat
    inc(sp);
    if sp>length(s) then
      break;
    inc(isp);
    c:=UpCase(s[sp]);
    { Among all strings beginning with, say, ‘ab’, the ‘ab’ itself will be the first.
      So after this check, “isp ≤ length(any string in the range)” is guaranteed. }
    if isp>length(Psorted_array(Pstring_to_ord(str2ordindex)^.data)[l].s^) then
      begin
        inc(l);
        if l=r then
          break;
      end;
    if UpCase(Psorted_array(Pstring_to_ord(str2ordindex)^.data)[l].s^[isp])=c then { Shortcut: L may be already correct (enums often have common prefixes). }
      begin
        if l+1=r then { Shortcut: the only string left (enums often have different suffixes). }
          continue;
      end
    else
      begin
        r2:=r; { Search for new L. }
        repeat
          m:=SizeUint(l+r2) div 2;
          if UpCase(Psorted_array(Pstring_to_ord(str2ordindex)^.data)[m].s^[isp])<c then
            l:=m+1
          else
            r2:=m;
        until l=r2;
        if l=r then
          break;
      end;
    if UpCase(Psorted_array(Pstring_to_ord(str2ordindex)^.data)[r-1].s^[isp])=c then { Shortcut: R−1 may be already correct. }
      continue;
    l2:=l; { Search for new R. }
    repeat
      m:=SizeUint(l2+r) div 2;
      if UpCase(Psorted_array(Pstring_to_ord(str2ordindex)^.data)[m].s^[isp])<=c then
        l2:=m+1
      else
        r:=m;
    until l2=r;
    if l=r then { Better not to make it the loop condition, or ‘continue’s may jump to it instead of the beginning. }
      break;
  until false;
  if (l<r) and (isp=length(Psorted_array(Pstring_to_ord(str2ordindex)^.data)[l].s^)) then
    begin
      code:=0;
      exit(Psorted_array(Pstring_to_ord(str2ordindex)^.data)[l].o);
    end;
  code:=sp;
  result:=-1; { Formally undefined, but −1 is very likely the invalid value prone to crashing, which is better than accidentally working. }
end;

{Redeclare fpc_val_enum_shortstr for internal use in the system unit.}
function fpc_val_enum_shortstr(str2ordindex:pointer;const s:shortstring;out code:valsint):longint;external name 'FPC_VAL_ENUM_SHORTSTR';
{$endif FPC_STR_ENUM_INTERN}

function fpc_Val_Currency_ShortStr(const s : shortstring; out Code : ValSInt): currency; [public, alias:'FPC_VAL_CURRENCY_SHORTSTR']; compilerproc;
{$ifdef EXCLUDE_COMPLEX_PROCS}
begin
  runerror(217);
end;
{$else EXCLUDE_COMPLEX_PROCS}
const
  MinInt64 : Int64  =-$8000000000000000;
  MinInt64Edge : Int64 = (-$8000000000000000 + 10) div 10;
var
  { to enable taking the address on the JVM target }
  res : array[0..0] of Int64;
  i,j,power,sign,len : longint;
  FracOverflow : boolean;
begin
  fpc_Val_Currency_ShortStr:=0;
  res[0]:=0;
  len:=Length(s);
  Code:=1;
  sign:=-1;
  power:=0;
  while True do
    if Code > len then
      exit
    else
      if s[Code] in [' ', #9] then
        Inc(Code)
      else
        break;
  { Read sign }
  case s[Code] of
   '+' : begin
           Inc(Code);
         end;
   '-' : begin
           sign:=+1;
           Inc(Code);
         end;
  end;
  { Read digits }
  FracOverflow:=False;
  i:=0;
  while Code <= len do
    begin
      case s[Code] of
        '0'..'9':
          begin
            j:=Ord(s[code])-Ord('0');
            { check overflow }
            if (res[0] >= MinInt64Edge) or (res[0] >= (MinInt64 + j) div 10) then
              begin
                res[0]:=res[0]*10 - j;
                Inc(i);
              end
            else
              if power = 0 then
                { exit if integer part overflow }
                exit
              else
                begin
                  if not FracOverflow and (j >= 5) and (res[0] > MinInt64) then
                    { round if first digit of fractional part overflow }
                    Dec(res[0]);
                  FracOverflow:=True;
                end;
          end;
        '.':
          begin
            if power = 0 then
              begin
                power:=1;
                i:=0;
              end
            else
              exit;
          end;
        else
          break;
      end;
      Inc(Code);
    end;
  if (i = 0) and (power = 0) then
    exit;
  if power <> 0 then
    power:=i;
  power:=4 - power;
  { Exponent? }
  if Code <= len then
    if s[Code] in ['E', 'e'] then
      begin
        Inc(Code);
        if Code > len then
          exit;
        i:=1;
        case s[Code] of
          '+':
            Inc(Code);
          '-':
            begin
              i:=-1;
              Inc(Code);
            end;
        end;
        { read exponent }
        j:=0;
        while Code <= len do
          if s[Code] in ['0'..'9'] then
            begin
              if j > 4951 then
                exit;
              j:=j*10 + (Ord(s[code])-Ord('0'));
              Inc(Code);
            end
          else
            exit;
        power:=power + j*i;
      end
    else
      exit;

  if power > 0 then
    begin
      for i:=1 to power do
        if res[0] >= MinInt64 div 10 then
          res[0]:=res[0]*10
        else
          exit;
    end
  else
    for i:=1 to -power do
      begin
        if res[0] >= MinInt64 + 5 then
          Dec(res[0], 5);
        res[0]:=res[0] div 10;
      end;

  if sign <> 1 then
    if res[0] > MinInt64 then
      res[0]:=res[0]*sign
    else
      exit;

  fpc_Val_Currency_ShortStr:=PCurrency(@res[0])^;
  Code:=0;
end;
{$endif EXCLUDE_COMPLEX_PROCS}


{$ifndef FPC_HAS_SETSTRING_SHORTSTR}
{$define FPC_HAS_SETSTRING_SHORTSTR}
Procedure fpc_setstring_shortstr(Out S : Shortstring; Buf : PAnsiChar; Len : SizeInt); compilerproc;
begin
  If Len > High(S) then
    Len := High(S);
  SetLength(S,Len);
  If Buf<>Nil then
    begin
      Move (Buf[0],S[1],Len);
    end;
end;
{$endif FPC_HAS_SETSTRING_SHORTSTR}

{$ifndef FPC_HAS_COMPARETEXT_SHORTSTR}
{$define FPC_HAS_COMPARETEXT_SHORTSTR}
function ShortCompareText(const S1, S2: shortstring): SizeInt;
var
  c1, c2: Byte;
  i: SizeInt;
  L1, L2, Count: SizeInt;
  P1, P2: PAnsiChar;
begin
  L1 := Length(S1);
  L2 := Length(S2);
  if L1 > L2 then
    Count := L2
  else
    Count := L1;
  i := 0;
  P1 := @S1[1];
  P2 := @S2[1];
  while i < count do
  begin
    c1 := byte(p1^);
    c2 := byte(p2^);
    if c1 <> c2 then
    begin
      if c1 in [97..122] then
        Dec(c1, 32);
      if c2 in [97..122] then
        Dec(c2, 32);
      if c1 <> c2 then
        Break;
    end;
    Inc(P1); Inc(P2); Inc(I);
  end;
  if i < count then
    ShortCompareText := c1 - c2
  else
    ShortCompareText := L1 - L2;
end;
{$endif FPC_HAS_COMPARETEXT_SHORTSTR}
