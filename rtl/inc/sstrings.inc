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

procedure fpc_Shortstr_SetLength(var s:shortstring;len:SizeInt);[Public,Alias : 'FPC_SHORTSTR_SETLENGTH']; compilerproc;
begin
  if Len>255 then
   Len:=255;
  s[0]:=chr(len);
end;

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
  Move(s[Index+1],fpc_shortstr_Copy[1],Count);
end;


procedure delete(var s : shortstring;index : SizeInt;count : SizeInt);
begin
  if index<=0 then
     exit;
  if (Index<=Length(s)) and (Count>0) then
   begin
     if Count>length(s)-Index then
      Count:=length(s)-Index+1;
     s[0]:=Chr(length(s)-Count);
     if Index<=Length(s) then
      Move(s[Index+Count],s[Index],Length(s)-Index+1);
   end;
end;


procedure insert(const source : shortstring;var s : shortstring;index : SizeInt);
var
  cut,srclen,indexlen : SizeInt;
begin
  if index<1 then
   index:=1;
  if index>length(s) then
   index:=length(s)+1;
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
  move(s[Index],s[Index+srclen],indexlen);
  move(Source[1],s[Index],srclen);
  s[0]:=chr(index+srclen+indexlen-1);
end;


procedure insert(source : Char;var s : shortstring;index : SizeInt);
var
  indexlen : SizeInt;
begin
  if index<1 then
   index:=1;
  if index>length(s) then
   index:=length(s)+1;
  indexlen:=Length(s)-Index+1;
  if (sizeint(length(s))+1=sizeof(s)) and (indexlen>0) then
   dec(indexlen);
  move(s[Index],s[Index+1],indexlen);
  s[Index]:=Source;
  s[0]:=chr(index+indexlen);
end;


function pos(const substr : shortstring;const s : shortstring):SizeInt;
var
  i,MaxLen : SizeInt;
  pc : pchar;
begin
  Pos:=0;
  if Length(SubStr)>0 then
   begin
     MaxLen:=sizeint(Length(s))-Length(SubStr);
     i:=0;
     pc:=@s[1];
     while (i<=MaxLen) do
      begin
        inc(i);
        if (SubStr[1]=pc^) and
           (CompareChar(Substr[1],pc^,Length(SubStr))=0) then
         begin
           Pos:=i;
           exit;
         end;
        inc(pc);
      end;
   end;
end;


{Faster when looking for a single char...}
function pos(c:char;const s:shortstring):SizeInt;
var
  i : SizeInt;
  pc : pchar;
begin
  pc:=@s[1];
  for i:=1 to length(s) do
   begin
     if pc^=c then
      begin
        pos:=i;
        exit;
      end;
     inc(pc);
   end;
  pos:=0;
end;


function fpc_char_copy(c:char;index : SizeInt;count : SizeInt): shortstring;compilerproc;
begin
  if (index=1) and (Count>0) then
   fpc_char_Copy:=c
  else
   fpc_char_Copy:='';
end;

function pos(const substr : shortstring;c:char): SizeInt;
begin
  if (length(substr)=1) and (substr[1]=c) then
   Pos:=1
  else
   Pos:=0;
end;


{$ifdef IBM_CHAR_SET}
const
  UpCaseTbl : shortstring[7]=#154#142#153#144#128#143#165;
  LoCaseTbl : shortstring[7]=#129#132#148#130#135#134#164;
{$endif}

function upcase(c : char) : char;
{$IFDEF IBM_CHAR_SET}
var
  i : longint;
{$ENDIF}
begin
  if (c in ['a'..'z']) then
    upcase:=char(byte(c)-32)
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


function upcase(const s : shortstring) : shortstring;
var
  i : longint;
begin
  upcase[0]:=s[0];
  for i := 1 to length (s) do
    upcase[i] := upcase (s[i]);
end;


function lowercase(c : char) : char;overload;
{$IFDEF IBM_CHAR_SET}
var
  i : longint;
{$ENDIF}
begin
  if (c in ['A'..'Z']) then
   lowercase:=char(byte(c)+32)
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


function lowercase(const s : shortstring) : shortstring; overload;
var
  i : longint;
begin
  lowercase [0]:=s[0];
  for i:=1 to length(s) do
   lowercase[i]:=lowercase (s[i]);
end;


const
  HexTbl : array[0..15] of char='0123456789ABCDEF';

function hexstr(val : longint;cnt : byte) : shortstring;
var
  i : longint;
begin
  hexstr[0]:=char(cnt);
  for i:=cnt downto 1 do
   begin
     hexstr[i]:=hextbl[val and $f];
     val:=val shr 4;
   end;
end;

function octstr(val : longint;cnt : byte) : shortstring;
var
  i : longint;
begin
  octstr[0]:=char(cnt);
  for i:=cnt downto 1 do
   begin
     octstr[i]:=hextbl[val and 7];
     val:=val shr 3;
   end;
end;


function binstr(val : longint;cnt : byte) : shortstring;
var
  i : longint;
begin
  binstr[0]:=char(cnt);
  for i:=cnt downto 1 do
   begin
     binstr[i]:=char(48+val and 1);
     val:=val shr 1;
   end;
end;


function hexstr(val : int64;cnt : byte) : shortstring;
var
  i : longint;
begin
  hexstr[0]:=char(cnt);
  for i:=cnt downto 1 do
   begin
     hexstr[i]:=hextbl[val and $f];
     val:=val shr 4;
   end;
end;


function octstr(val : int64;cnt : byte) : shortstring;
var
  i : longint;
begin
  octstr[0]:=char(cnt);
  for i:=cnt downto 1 do
   begin
     octstr[i]:=hextbl[val and 7];
     val:=val shr 3;
   end;
end;


function binstr(val : int64;cnt : byte) : shortstring;
var
  i : longint;
begin
  binstr[0]:=char(cnt);
  for i:=cnt downto 1 do
   begin
     binstr[i]:=char(48+val and 1);
     val:=val shr 1;
   end;
end;


Function  hexStr(Val:qword;cnt:byte):shortstring;
begin
  hexStr:=hexStr(int64(Val),cnt);
end;


Function  OctStr(Val:qword;cnt:byte):shortstring;
begin
  OctStr:=OctStr(int64(Val),cnt);
end;


Function  binStr(Val:qword;cnt:byte):shortstring;
begin
  binStr:=binStr(int64(Val),cnt);
end;


function hexstr(val : pointer) : shortstring;
var
  i : longint;
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


function space (b : byte): shortstring;
begin
  space[0] := chr(b);
  FillChar (Space[1],b,' ');
end;


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
  int_str(v,s);
  if length(s)<len then
    s:=space(len-length(s))+s;
end;

{$ifndef CPU64}

procedure fpc_shortstr_qword(v : qword;len : SizeInt;out s : shortstring);[public,alias:'FPC_SHORTSTR_QWORD']; compilerproc;
begin
  int_str(v,s);
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


{ fpc_shortstr_sInt must appear before this file is included, because }
{ it's used inside real2str.inc and otherwise the searching via the      }
{ compilerproc name will fail (JM)                                       }

{$ifndef FPUNONE}
{$I real2str.inc}
{$endif}

{$ifndef FPUNONE}
procedure fpc_shortstr_float(d : ValReal;len,fr,rt : SizeInt;out s : shortstring);[public,alias:'FPC_SHORTSTR_FLOAT']; compilerproc;
begin
  str_real(len,fr,d,treal_type(rt),s);
end;
{$endif}

function fpc_shortstr_enum_intern(ordinal,len:sizeint;typinfo,ord2strindex:pointer;out s:shortstring): longint;

{ The following contains the TTypeInfo/TTypeData records from typinfo.pp
  specialized for the tkEnumeration case (and stripped of unused things). }
type
  PPstring=^Pstring;

  Penum_typeinfo=^Tenum_typeinfo;
  Tenum_typeinfo={$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}packed{$endif}record
    kind:byte; { always tkEnumeration }
    num_chars:byte;
    chars:array[0..0] of char; { variable length with size of num_chars }
  end;

  Penum_typedata=^Tenum_typedata;
  Tenum_typedata={$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}packed{$endif}record
    ordtype:byte;
    { this seemingly extraneous inner record is here for alignment purposes, so
      that its data gets aligned properly (if FPC_REQUIRES_PROPER_ALIGNMENT is
      set }
    inner: {$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}packed{$endif}record
      minvalue,maxvalue:longint;
      basetype:pointer; { required for alignment }
    end;
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
      body:=Penum_typedata(align(ptruint(header) + 2 * sizeof(byte) { kind, num_chars } + header^.num_chars,
        {$ifndef FPC_REQUIRES_PROPER_ALIGNMENT} 1 {$else} sizeof(pointer) {$endif}));
      with (body^.inner) do
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
procedure fpc_shortstr_bool(b : boolean;len:sizeint;out s:shortstring);external name 'FPC_SHORTSTR_BOOL';


procedure fpc_shortstr_currency(c : currency; len,f : SizeInt; out s : shortstring);[public,alias:'FPC_SHORTSTR_CURRENCY']; compilerproc;
const
  MinLen = 8; { Minimal string length in scientific format }
var
  buf : array[1..19] of char;
  i,j,k,reslen,tlen,sign,r,point : longint;
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
      i:=1;
      k:=0;
      for j:=0 to r do
        begin
          if (k=1) and (buf[i]='9') then
            buf[i]:='0'
          else
            begin
            buf[i]:=chr(ord(buf[i]) + k);
            if buf[i] >= '5' then
              k:=1
            else
              k:=0;
            end;
          Inc(i);
          if i>tlen  then
            break;
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
		  if i>reslen then
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
   Array Of Char Str() helpers
}

procedure fpc_chararray_sint(v : valsint;len : SizeInt;out a:array of char);compilerproc;
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
  move(ss[1],pchar(@a)^,maxlen);
end;


procedure fpc_chararray_uint(v : valuint;len : SizeInt;out a : array of char);compilerproc;
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
  move(ss[1],pchar(@a)^,maxlen);
end;


{$ifndef CPU64}

procedure fpc_chararray_qword(v : qword;len : SizeInt;out a : array of char);compilerproc;
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
  move(ss[1],pchar(@a)^,maxlen);
end;


procedure fpc_chararray_int64(v : int64;len : SizeInt;out a : array of char);compilerproc;
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
  move(ss[1],pchar(@a)^,maxlen);
end;

{$endif CPU64}


{$ifndef FPUNONE}
procedure fpc_chararray_Float(d : ValReal;len,fr,rt : SizeInt;out a : array of char);compilerproc;
var
  ss : shortstring;
  maxlen : SizeInt;
begin
  str_real(len,fr,d,treal_type(rt),ss);
  if length(ss)<high(a)+1 then
    maxlen:=length(ss)
  else
    maxlen:=high(a)+1;
  move(ss[1],pchar(@a)^,maxlen);
end;
{$endif}


procedure fpc_chararray_enum(ordinal,len:sizeint;typinfo,ord2strindex:pointer;out a : array of char);compilerproc;
var
  ss : shortstring;
  maxlen : SizeInt;
begin
  fpc_shortstr_enum(ordinal,len,typinfo,ord2strindex,ss);
  if length(ss)<high(a)+1 then
    maxlen:=length(ss)
  else
    maxlen:=high(a)+1;
  move(ss[1],pchar(@a)^,maxlen);
end;


procedure fpc_chararray_bool(b : boolean;len:sizeint;out a : array of char);compilerproc;
var
  ss : shortstring;
  maxlen : SizeInt;
begin
  fpc_shortstr_bool(b,len,ss);
  if length(ss)<high(a)+1 then
    maxlen:=length(ss)
  else
    maxlen:=high(a)+1;
  move(ss[1],pchar(@a)^,maxlen);
end;


{$ifdef FPC_HAS_STR_CURRENCY}
procedure fpc_chararray_Currency(c : Currency;len,fr : SizeInt;out a : array of char);compilerproc;
var
  ss : shortstring;
  maxlen : SizeInt;
begin
  str(c:len:fr,ss);
  if length(ss)<high(a)+1 then
    maxlen:=length(ss)
  else
    maxlen:=high(a)+1;
  move(ss[1],pchar(@a)^,maxlen);
end;
{$endif FPC_HAS_STR_CURRENCY}

{*****************************************************************************
                           Val() Functions
*****************************************************************************}

Function InitVal(const s:shortstring;out negativ:boolean;out base:byte):ValSInt;
var
  Code : SizeInt;
begin
  code:=1;
  negativ:=false;
  base:=10;
  if length(s)=0 then
    begin
      InitVal:=code;
      Exit;
    end;
{Skip Spaces and Tab}
  while (code<=length(s)) and (s[code] in [' ',#9]) do
   inc(code);
{Sign}
  case s[code] of
   '-' : begin
           negativ:=true;
           inc(code);
         end;
   '+' : inc(code);
  end;
{Base}
  if code<=length(s) then
   begin
     case s[code] of
      '$',
      'X',
      'x' : begin
              base:=16;
              inc(code);
            end;
      '%' : begin
              base:=2;
              inc(code);
            end;
      '&' : begin
              Base:=8;
              inc(code);
            end;
      '0' : begin
              if (code < length(s)) and (s[code+1] in ['x', 'X']) then
              begin
                inc(code, 2);
                base := 16;
              end;
            end;
     end;
  end;
  { strip leading zeros }
  while ((code < length(s)) and (s[code] = '0')) do begin
    inc(code);
  end;
  InitVal:=code;
end;


Function fpc_Val_SInt_ShortStr(DestSize: SizeInt; Const S: ShortString; out Code: ValSInt): ValSInt; [public, alias:'FPC_VAL_SINT_SHORTSTR']; compilerproc;
var
  temp, prev, maxPrevValue, maxNewValue: ValUInt;
  base,u : byte;
  negative : boolean;
begin
  fpc_Val_SInt_ShortStr := 0;
  Temp:=0;
  Code:=InitVal(s,negative,base);
  if Code>length(s) then
   exit;
  if (s[Code]=#0) then
    begin
      if (Code>1) and (s[Code-1]='0') then
        Code:=0;
      exit;
    end;
  maxPrevValue := ValUInt(MaxUIntValue) div ValUInt(Base);
  if (base = 10) then
    maxNewValue := MaxSIntValue + ord(negative)
  else
    maxNewValue := MaxUIntValue;
  while Code<=Length(s) do
   begin
     case s[Code] of
       '0'..'9' : u:=Ord(S[Code])-Ord('0');
       'A'..'F' : u:=Ord(S[Code])-(Ord('A')-10);
       'a'..'f' : u:=Ord(S[Code])-(Ord('a')-10);
       #0 : break;
     else
      u:=16;
     end;
     Prev := Temp;
     Temp := Temp*ValUInt(base);
     If (u >= base) or
        (ValUInt(maxNewValue-u) < Temp) or
        (prev > maxPrevValue) Then
       Begin
         fpc_Val_SInt_ShortStr := 0;
         Exit
       End;
     Temp:=Temp+u;
     inc(code);
   end;
  code := 0;
  fpc_Val_SInt_ShortStr := ValSInt(Temp);
  If Negative Then
    fpc_Val_SInt_ShortStr := -fpc_Val_SInt_ShortStr;
  If Not(Negative) and (base <> 10) Then
   {sign extend the result to allow proper range checking}
    Case DestSize of
      1: fpc_Val_SInt_ShortStr := shortint(fpc_Val_SInt_ShortStr);
      2: fpc_Val_SInt_ShortStr := smallint(fpc_Val_SInt_ShortStr);
{$ifdef cpu64}
      4: fpc_Val_SInt_ShortStr := longint(fpc_Val_SInt_ShortStr);
{$endif cpu64}
    End;
end;

{ we need this for fpc_Val_SInt_Ansistr and fpc_Val_SInt_WideStr because }
{ we have to pass the DestSize parameter on (JM)                         }
Function int_Val_SInt_ShortStr(DestSize: SizeInt; Const S: ShortString; out Code: ValSInt): ValSInt; [external name 'FPC_VAL_SINT_SHORTSTR'];


Function fpc_Val_UInt_Shortstr(Const S: ShortString; out Code: ValSInt): ValUInt; [public, alias:'FPC_VAL_UINT_SHORTSTR']; compilerproc;
var
  prev : ValUInt;
  base,u : byte;
  negative : boolean;
begin
  fpc_Val_UInt_Shortstr:=0;
  Code:=InitVal(s,negative,base);
  If Negative or (Code>length(s)) Then
    Exit;
  if (s[Code]=#0) then
    begin
      if (Code>1) and (s[Code-1]='0') then
        Code:=0;
      exit;
    end;
  while Code<=Length(s) do
   begin
     case s[Code] of
       '0'..'9' : u:=Ord(S[Code])-Ord('0');
       'A'..'F' : u:=Ord(S[Code])-(Ord('A')-10);
       'a'..'f' : u:=Ord(S[Code])-(Ord('a')-10);
       #0 : break;
     else
      u:=16;
     end;
     prev := fpc_Val_UInt_Shortstr;
     If (u>=base) or
        (ValUInt(MaxUIntValue-u) div ValUInt(Base)<prev) then
      begin
        fpc_Val_UInt_Shortstr:=0;
        exit;
      end;
     fpc_Val_UInt_Shortstr:=fpc_Val_UInt_Shortstr*ValUInt(base) + u;
     inc(code);
   end;
  code := 0;
end;


{$ifndef CPU64}

  Function fpc_val_int64_shortstr(Const S: ShortString; out Code: ValSInt): Int64; [public, alias:'FPC_VAL_INT64_SHORTSTR']; compilerproc;

  var  u, temp, prev, maxprevvalue, maxnewvalue : qword;
       base : byte;
       negative : boolean;

  const maxint64=qword($7fffffffffffffff);
        maxqword=qword($ffffffffffffffff);

  begin
    fpc_val_int64_shortstr := 0;
    Temp:=0;
    Code:=InitVal(s,negative,base);
    if Code>length(s) then
     exit;
    if (s[Code]=#0) then
      begin
        if (Code>1) and (s[Code-1]='0') then
          Code:=0;
        exit;
      end;
    maxprevvalue := maxqword div base;
    if (base = 10) then
      maxnewvalue := maxint64 + ord(negative)
    else
      maxnewvalue := maxqword;

    while Code<=Length(s) do
     begin
       case s[Code] of
         '0'..'9' : u:=Ord(S[Code])-Ord('0');
         'A'..'F' : u:=Ord(S[Code])-(Ord('A')-10);
         'a'..'f' : u:=Ord(S[Code])-(Ord('a')-10);
         #0 : break;
       else
        u:=16;
       end;
       Prev:=Temp;
       Temp:=Temp*qword(base);
     If (u >= base) or
        (qword(maxnewvalue-u) < temp) or
        (prev > maxprevvalue) Then
       Begin
         fpc_val_int64_shortstr := 0;
         Exit
       End;
       Temp:=Temp+u;
       inc(code);
     end;
    code:=0;
    fpc_val_int64_shortstr:=int64(Temp);
    If Negative Then
      fpc_val_int64_shortstr:=-fpc_val_int64_shortstr;
  end;


  Function fpc_val_qword_shortstr(Const S: ShortString; out Code: ValSInt): QWord; [public, alias:'FPC_VAL_QWORD_SHORTSTR']; compilerproc;

  var  u, prev: QWord;
       base : byte;
       negative : boolean;

  const maxqword=qword($ffffffffffffffff);

  begin
    fpc_val_qword_shortstr:=0;
    Code:=InitVal(s,negative,base);
    If Negative or (Code>length(s)) Then
      Exit;
    if (s[Code]=#0) then
      begin
        if (Code>1) and (s[Code-1]='0') then
          Code:=0;
        exit;
      end;
    while Code<=Length(s) do
     begin
       case s[Code] of
         '0'..'9' : u:=Ord(S[Code])-Ord('0');
         'A'..'F' : u:=Ord(S[Code])-(Ord('A')-10);
         'a'..'f' : u:=Ord(S[Code])-(Ord('a')-10);
         #0 : break;
       else
        u:=16;
       end;
       prev := fpc_val_qword_shortstr;
       If (u>=base) or
         ((QWord(maxqword-u) div QWord(base))<prev) then
         Begin
           fpc_val_qword_shortstr := 0;
           Exit
         End;
       fpc_val_qword_shortstr:=fpc_val_qword_shortstr*QWord(base) + u;
       inc(code);
     end;
    code := 0;
  end;

{$endif CPU64}

{$ifndef FPUNONE}
const
{$ifdef FPC_HAS_TYPE_EXTENDED}
  valmaxexpnorm=4932;
{$else}
{$ifdef FPC_HAS_TYPE_DOUBLE}
  valmaxexpnorm=308;
{$else}
{$ifdef FPC_HAS_TYPE_SINGLE}
  valmaxexpnorm=38;
{$else}
{$error Unknown floating point precision }
{$endif}
{$endif}
{$endif}
{$endif}

{$ifndef FPUNONE}

(******************

   Derived from: ".\Free Pascal\source\rtl\inc\genmath.inc"

   Origin: "fast 10^n routine"
     function FPower10(val: Extended; Power: Longint): Extended;

   Changes:
    > adapted to "ValReal", so float can be single/double/extended
    > slightly changed arrays [redundant 58+2 float constants gone away]
    > added some checks etc..

   Notes:
    > denormalization and overflow should go smooth if corresponding
      FPU exceptions are masked [no external care needed by now]
    > adaption to real48 and real128 is not hard if one needed

 ******************)
//
   function mul_by_power10(x:ValReal;power:integer):ValReal;
//
// result:=X*(10^power)
//
// Routine achieves result with no more than 3 floating point mul/div's.
// Up to ABS(power)=31, only 1 floating point mul/div is needed.
//
// Limitations:
//  for ValReal=extended : power=-5119..+5119
//  for ValReal=double   : power=-319..+319
//  for ValReal=single   : power=-63..+63
//
// If "power" is beyond this limits, routine gives up and returns 0/+INF/-INF.
// This is not generally correct, but should be ok when routine is used only
// as "VAL"-helper, since "x" exponent is reasonably close to 0 in this case.
//
//==================================
{$IF DECLARED(C_HIGH_EXPBITS_5TO8)}
  {$ERROR C_HIGH_EXPBITS_5TO8 declared somewhere in scope}
{$ENDIF}

{$IF DECLARED(C_HIGH_EXPBITS_9ANDUP)}
  {$ERROR C_HIGH_EXPBITS_9ANDUP declared somewhere in scope}
{$ENDIF}

{$IF SIZEOF(ValReal)=10}
//==================================
// assuming "type ValReal=extended;"
//
type
  TValRealRec = packed record
    case byte of
    0: (
      {$ifndef ENDIAN_BIG}
        mant_lo, mant_hi : longword; expsign : word // little-endian/default
      {$else}
        expsign : word; mant_hi, mant_lo : longword // big-endian
      {$endif}
       );
    1: (v : extended);
  end;

const
 {$ifndef ENDIAN_BIG}
  // little-endian/default
  C_INFTYP:TValRealRec=(mant_lo:0;mant_hi:$80000000;expsign:$7FFF); //+INF
  C_INFTYM:TValRealRec=(mant_lo:0;mant_hi:$80000000;expsign:$FFFF); //-INF
 {$else}
  // big-endian
  C_INFTYP:TValRealRec=(expsign:$7FFF;mant_hi:$80000000;mant_lo:0); //+INF
  C_INFTYM:TValRealRec=(expsign:$FFFF;mant_hi:$80000000;mant_lo:0); //-INF
 {$endif}

  C_MAX_POWER = 5119;

  C_HIGH_EXPBITS_5TO8   = 15;
  C_HIGH_EXPBITS_9ANDUP = 9;

{$ELSEIF SIZEOF(ValReal)=8}
//==================================
// assuming "type ValReal=double;"
//
type
  TValRealRec = packed record
    case byte of
    0: (
      {$ifndef ENDIAN_BIG}
        raw_lo, raw_hi : longword // little-endian/default
      {$else}
        raw_hi, raw_lo : longword // big-endian
      {$endif}
       );
    1: (v : double);
  end;

const
 {$ifndef ENDIAN_BIG}
  // little-endian/default
  C_INFTYP:TValRealRec=(raw_lo:0;raw_hi:$7FF00000); //+INF
  C_INFTYM:TValRealRec=(raw_lo:0;raw_hi:$FFF00000); //-INF
 {$else}
  // big-endian
  C_INFTYP:TValRealRec=(raw_hi:$7FF00000;raw_lo:0); //+INF
  C_INFTYM:TValRealRec=(raw_hi:$FFF00000;raw_lo:0); //-INF
 {$endif}

  C_MAX_POWER = 319;

  C_HIGH_EXPBITS_5TO8 = 9;

{$ELSEIF SIZEOF(ValReal)=4}
//==================================
// assuming "type ValReal=single;"
//
type
  TValRealRec = packed record
    case byte of
    0: (raw : longword);
    1: (v : single);
  end;

const
  C_INFTYP:TValRealRec=(raw:$7F800000); //+INF
  C_INFTYM:TValRealRec=(raw:$FF800000); //-INF

  C_MAX_POWER = 63;

{$ELSE}
//==================================
// assuming "ValReal=?"
//
  {$ERROR Unsupported ValReal type}
{$ENDIF}

//==================================
const
  mul_expbits_0_to_4:packed array[0..31]of ValReal=(
   1E0,  1E1,  1E2,  1E3,
   1E4,  1E5,  1E6,  1E7,
   1E8,  1E9,  1E10, 1E11,
   1E12, 1E13, 1E14, 1E15,
   1E16, 1E17, 1E18, 1E19,
   1E20, 1E21, 1E22, 1E23,
   1E24, 1E25, 1E26, 1E27,
   1E28, 1E29, 1E30, 1E31);

{$IF DECLARED(C_HIGH_EXPBITS_5TO8)}
  mul_expbits_5_to_8:packed array[1..C_HIGH_EXPBITS_5TO8]of ValReal=(
   1E32,  1E64,  1E96,  1E128,
   1E160, 1E192, 1E224, 1E256, 1E288
  {$IF DECLARED(C_HIGH_EXPBITS_9ANDUP)},
   1E320, 1E352, 1E384, 1E416, 1E448, 1E480
  {$ENDIF});
{$ELSE}
  mul_expbits_5_to_8:ValReal=1E32;
{$ENDIF}

{$IF DECLARED(C_HIGH_EXPBITS_9ANDUP)}
  mul_expbits_9_and_up:packed array[1..C_HIGH_EXPBITS_9ANDUP]of ValReal=(
   1E512,  1E1024, 1E1536, 1E2048,
   1E2560, 1E3072, 1E3584, 1E4096,
   1E4608);
{$ENDIF}

begin
  if power=0 then mul_by_power10:=x else
  if power<-C_MAX_POWER then mul_by_power10:=0 else
  if power>C_MAX_POWER then
   if x<0 then mul_by_power10:=C_INFTYM.v else
   if x>0 then mul_by_power10:=C_INFTYP.v else mul_by_power10:=0
  else
   if power<0 then
    begin
     power:=-power;
     mul_by_power10:=x/mul_expbits_0_to_4[power and $1F];
     power:=(power shr 5);
     if power=0 then exit;
    {$IF DECLARED(C_HIGH_EXPBITS_5TO8)}
     if power and $F<>0 then
      mul_by_power10:=
       mul_by_power10/mul_expbits_5_to_8[power and $F];
    {$ELSE} // "single", power<>0, so always div
     mul_by_power10:=mul_by_power10/mul_expbits_5_to_8;
    {$ENDIF}
    {$IF DECLARED(C_HIGH_EXPBITS_9ANDUP)}
     power:=(power shr 4);
     if power<>0 then
      mul_by_power10:=
       mul_by_power10/mul_expbits_9_and_up[power];
    {$ENDIF}
    end
   else
    begin
     mul_by_power10:=x*mul_expbits_0_to_4[power and $1F];
     power:=(power shr 5);
     if power=0 then exit;
    {$IF DECLARED(C_HIGH_EXPBITS_5TO8)}
     if power and $F<>0 then
      mul_by_power10:=
       mul_by_power10*mul_expbits_5_to_8[power and $F];
    {$ELSE} // "single", power<>0, so always mul
     mul_by_power10:=mul_by_power10*mul_expbits_5_to_8;
    {$ENDIF}
    {$IF DECLARED(C_HIGH_EXPBITS_9ANDUP)}
     power:=(power shr 4);
     if power<>0 then
      mul_by_power10:=
       mul_by_power10*mul_expbits_9_and_up[power];
    {$ENDIF}
    end;
end;

Function fpc_Val_Real_ShortStr(const s : shortstring; out Code : ValSInt): ValReal; [public, alias:'FPC_VAL_REAL_SHORTSTR']; compilerproc;
var
  sign : valreal;
  esign,
  exponent,
  decpoint,i : SizeInt;
  flags      : byte;
begin
  fpc_Val_Real_ShortStr:=0.0;
  code:=1;
  exponent:=0;
  decpoint:=0;
  esign:=1;
  flags:=0;
  sign:=1;
  while (code<=length(s)) and (s[code] in [' ',#9]) do
    inc(code);
  if code<=length(s) then
    case s[code] of
     '+' : inc(code);
     '-' : begin
             sign:=-1;
             inc(code);
           end;
    end;
  while (Code<=Length(s)) and (s[code] in ['0'..'9']) do
    begin
   { Read integer part }
      flags:=flags or 1;
      fpc_Val_Real_ShortStr:=fpc_Val_Real_ShortStr*10+(ord(s[code])-ord('0'));
      inc(code);
    end;
{ Decimal ? }
  if (length(s)>=code) and (s[code]='.') then
    begin
      inc(code);
      while (length(s)>=code) and (s[code] in ['0'..'9']) do
        begin
           { Read fractional part. }
          flags:=flags or 2;
          fpc_Val_Real_ShortStr:=fpc_Val_Real_ShortStr*10+(ord(s[code])-ord('0'));
          inc(decpoint);
          inc(code);
        end;
   end;
 { Again, read integer and fractional part}
  if flags=0 then
    begin
      fpc_Val_Real_ShortStr:=0.0;
      exit;
    end;
 { Exponent ? }
  if (length(s)>=code) and (s[code] in ['e','E']) then
    begin
      inc(code);
      if Length(s) >= code then
        if s[code]='+' then
          inc(code)
        else
          if s[code]='-' then
           begin
             esign:=-1;
             inc(code);
           end;
      if (length(s)<code) or not(s[code] in ['0'..'9']) then
        begin
          fpc_Val_Real_ShortStr:=0.0;
          exit;
        end;
      while (length(s)>=code) and (s[code] in ['0'..'9']) do
        begin
          exponent:=exponent*10;
          exponent:=exponent+ord(s[code])-ord('0');
          inc(code);
        end;
    end;
{ adjust exponent based on decimal point }
  if esign>0 then
    begin
      dec(exponent,decpoint);
      if (exponent<0) then
        begin
          esign:=-1;
          exponent:=-exponent;
        end
    end
  else
    inc(exponent,decpoint);
{ evaluate sign }
{ (before exponent, because the exponent may turn it into a denormal) }
  fpc_Val_Real_ShortStr:=fpc_Val_Real_ShortStr*sign;

{ Calculate Exponent }
fpc_Val_Real_ShortStr:=mul_by_power10(fpc_Val_Real_ShortStr,exponent*esign);

{ Not all characters are read ? }
  if length(s)>=code then
    begin
      fpc_Val_Real_ShortStr:=0.0;
      exit;
    end;
{ success ! }
  code:=0;
end;
{$endif}

function fpc_val_enum_shortstr(str2ordindex:pointer;const s:shortstring;out code:valsint):longint; [public, alias:'FPC_VAL_ENUM_SHORTSTR']; compilerproc;

    function string_compare(const s1,s2:shortstring):sizeint;

    {We cannot use the > and < operators to compare a string here, because we if the string is
     not found in the enum, we need to return the position of error in "code". Code equals the
     highest matching character of all string compares, which is only known inside the string
     comparison.}

    var i,l:byte;
        c1,c2:char;

    begin
      l:=length(s1);
      if length(s1)>length(s2) then
        l:=length(s2);
      i:=1;
      while i<=l do
        begin
          c1:=s1[i];
          c2:=s2[i];
          if c1<>c2 then
            break;
          inc(i);
        end;
      if i>code then
        code:=i;
      if i<=l then
        string_compare:=byte(c1)-byte(c2)
      else
        string_compare:=length(s1)-length(s2);
    end;

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

var l,h,m:cardinal;
    c:sizeint;
    sorted_array:^Tsorted_array;
    spaces:byte;
    t:shortstring;

begin
  {Val for numbers accepts spaces at the start, so lets do the same
   for enums. Skip spaces at the start of the string.}
  spaces:=1;
  code:=1;
  while (spaces<=length(s)) and (s[spaces]=' ')  do
    inc(spaces);
  t:=upcase(copy(s,spaces,255));
  sorted_array:=pointer(@Pstring_to_ord(str2ordindex)^.data);
  {Use a binary search to get the string.}
  l:=1;
  h:=Pstring_to_ord(str2ordindex)^.count;
  repeat
    m:=(l+h) div 2;
    c:=string_compare(t,upcase(sorted_array[m-1].s^));
    if c>0 then
      l:=m+1
    else if c<0 then
      h:=m-1
    else
      break;
    if l>h then
      begin
        {Not found...}
        inc(code,spaces-1); {Add skipped spaces again.}
        {The result of val in case of error is undefined, don't assign a function result.}
        exit;
      end;
  until false;
  code:=0;
  fpc_val_enum_shortstr:=sorted_array[m-1].o;
end;

{Redeclare fpc_val_enum_shortstr for internal use in the system unit.}
function fpc_val_enum_shortstr(str2ordindex:pointer;const s:shortstring;out code:valsint):longint;external name 'FPC_VAL_ENUM_SHORTSTR';

function fpc_Val_Currency_ShortStr(const s : shortstring; out Code : ValSInt): currency; [public, alias:'FPC_VAL_CURRENCY_SHORTSTR']; compilerproc;
const
  MaxInt64 : Int64  = $7FFFFFFFFFFFFFFF;
  Int64Edge : Int64 = ($7FFFFFFFFFFFFFFF - 10) div 10;
  Int64Edge2 : Int64 = $7FFFFFFFFFFFFFFF div 10;
var
  res : Int64;
  i,j,power,sign,len : longint;
  FracOverflow : boolean;
begin
  fpc_Val_Currency_ShortStr:=0;
  res:=0;
  len:=Length(s);
  Code:=1;
  sign:=1;
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
   '+' : Inc(Code);
   '-' : begin
           sign:=-1;
           inc(code);
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
            if (res <= Int64Edge) or (res <= (MaxInt64 - j) div 10) then
              begin
                res:=res*10 + j;
                Inc(i);
              end
            else
              if power = 0 then
                { exit if integer part overflow }
                exit
              else
                begin
                  if not FracOverflow and (j >= 5) and (res < MaxInt64) then
                    { round if first digit of fractional part overflow }
                    Inc(res);
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
        if res <= Int64Edge2 then
          res:=res*10
        else
          exit;
    end
  else
    for i:=1 to -power do
      begin
        if res <= MaxInt64 - 5 then
          Inc(res, 5);
        res:=res div 10;
      end;
  res:=res*sign;
  fpc_Val_Currency_ShortStr:=PCurrency(@res)^;
  Code:=0;
end;


Procedure SetString (Out S : Shortstring; Buf : PChar; Len : SizeInt);
begin
  If Len > High(S) then
    Len := High(S);
  SetLength(S,Len);
  If Buf<>Nil then
    begin
      Move (Buf[0],S[1],Len);
    end;
end;

function ShortCompareText(const S1, S2: shortstring): SizeInt;
var
  c1, c2: Byte;
  i: Integer;
  L1, L2, Count: SizeInt;
  P1, P2: PChar;
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


