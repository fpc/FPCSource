{
    $Id$
    Copyright (C) 1998-2002 by Florian Klaempfl

    This unit implements some support functions

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published
    by the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


****************************************************************************
}
{# This unit contains some generic support functions which are used
   in the different parts of the compiler.
}
unit cutils;

{$i defines.inc}

interface

{$ifdef delphi}
    type
       dword = cardinal;
       qword = int64;
{$endif}

    type
       pstring = ^string;

    {# Returns the minimal value between @var(a) and @var(b) }
    function min(a,b : longint) : longint;
    {# Returns the maximum value between @var(a) and @var(b) }
    function max(a,b : longint) : longint;
    {# Returns the value in @var(x) swapped to different endian }
    function SwapLong(x : longint): longint;
    {# Returns the value in @va(x) swapped to different endian }
    function SwapWord(x : word): word;
    {# Return value @var(i) aligned on @var(a) boundary }
    function align(i,a:longint):longint;

    function used_align(varalign,minalign,maxalign:longint):longint;
    function size_2_align(len : longint) : longint;
    procedure Replace(var s:string;s1:string;const s2:string);
    procedure ReplaceCase(var s:string;const s1,s2:string);
    function upper(const s : string) : string;
    function lower(const s : string) : string;
    function trimbspace(const s:string):string;
    function trimspace(const s:string):string;
    function GetToken(var s:string;endchar:char):string;
    procedure uppervar(var s : string);
    function hexstr(val : cardinal;cnt : byte) : string;
    function tostru(i:cardinal) : string;
    function tostr(i : longint) : string;
    function int64tostr(i : int64) : string;
    function tostr_with_plus(i : longint) : string;
    procedure valint(S : string;var V : longint;var code : integer);
    {# Returns true if the string s is a number }
    function is_number(const s : string) : boolean;
    {# Returns true if value is a power of 2, the actual
       exponent value is returned in power.
    }
    function ispowerof2(value : int64;var power : longint) : boolean;
    function maybequoted(const s:string):string;
    function CompareText(S1, S2: string): longint;

    { releases the string p and assignes nil to p }
    { if p=nil then freemem isn't called          }
    procedure stringdispose(var p : pstring);


    { allocates mem for a copy of s, copies s to this mem and returns }
    { a pointer to this mem                                           }
    function stringdup(const s : string) : pstring;

    {# Allocates memory for the string @var(s) and copies s as zero
       terminated string to that allocated memory and returns a pointer
       to that mem
    }
    function  strpnew(const s : string) : pchar;
    procedure strdispose(var p : pchar);

    {# makes the character @var(c) lowercase, with spanish, french and german
       character set
    }
    function lowercase(c : char) : char;

    { makes zero terminated string to a pascal string }
    { the data in p is modified and p is returned     }
    function pchar2pstring(p : pchar) : pstring;

    { ambivalent to pchar2pstring }
    function pstring2pchar(p : pstring) : pchar;

{ Speed/Hash value }
    Function GetSpeedValue(Const s:String):cardinal;

{ Ansistring (pchar+length) support }
procedure ansistringdispose(var p : pchar;length : longint);
function compareansistrings(p1,p2 : pchar;length1,length2 : longint) : longint;
function concatansistrings(p1,p2 : pchar;length1,length2 : longint) : pchar;

{*****************************************************************************
                                 File Functions
*****************************************************************************}

    function DeleteFile(const fn:string):boolean;


implementation

uses
{$ifdef delphi}
  sysutils
{$else}
  strings
{$endif}
  ;

    var
      uppertbl,
      lowertbl  : array[char] of char;


    function min(a,b : longint) : longint;
    {
      return the minimal of a and b
    }
      begin
         if a>b then
           min:=b
         else
           min:=a;
      end;


    function max(a,b : longint) : longint;
    {
      return the maximum of a and b
    }
      begin
         if a<b then
           max:=b
         else
           max:=a;
      end;


    Function SwapLong(x : longint): longint;
      var
        y : word;
        z : word;
      Begin
        y := (x shr 16) and $FFFF;
        y := ((y shl 8) and $FFFF) or ((y shr 8) and $ff);
        z := x and $FFFF;
        z := ((z shl 8) and $FFFF) or ((z shr 8) and $ff);
        SwapLong := (longint(z) shl 16) or longint(y);
      End;


    Function SwapWord(x : word): word;
      var
        z : byte;
      Begin
        z := (x shr 8) and $ff;
        x := x and $ff;
        x := (x shl 8);
        SwapWord := x or z;
      End;


    function align(i,a:longint):longint;
    {
      return value <i> aligned <a> boundary
    }
      begin
        { for 0 and 1 no aligning is needed }
        if a<=1 then
         align:=i
        else
         align:=((i+a-1) div a) * a;
      end;


    function size_2_align(len : longint) : longint;
      begin
         if len>16 then
           size_2_align:=32
         else if len>8 then
           size_2_align:=16
         else if len>4 then
           size_2_align:=8
         else if len>2 then
           size_2_align:=4
         else if len>1 then
           size_2_align:=2
         else
           size_2_align:=1;
      end;


    function used_align(varalign,minalign,maxalign:longint):longint;
      begin
        { varalign  : minimum alignment required for the variable
          minalign  : Minimum alignment of this structure, 0 = undefined
          maxalign  : Maximum alignment of this structure, 0 = undefined }
        if (minalign>0) and
           (varalign<minalign) then
         used_align:=minalign
        else
         begin
           if (maxalign>0) and
              (varalign>maxalign) then
            used_align:=maxalign
           else
            used_align:=varalign;
         end;
      end;


    procedure Replace(var s:string;s1:string;const s2:string);
      var
         last,
         i  : longint;
      begin
        s1:=upper(s1);
        last:=0;
        repeat
          i:=pos(s1,upper(s));
          if i=last then
           i:=0;
          if (i>0) then
           begin
             Delete(s,i,length(s1));
             Insert(s2,s,i);
             last:=i;
           end;
        until (i=0);
      end;


    procedure ReplaceCase(var s:string;const s1,s2:string);
      var
         last,
         i  : longint;
      begin
        last:=0;
        repeat
          i:=pos(s1,s);
          if i=last then
           i:=0;
          if (i>0) then
           begin
             Delete(s,i,length(s1));
             Insert(s2,s,i);
             last:=i;
           end;
        until (i=0);
      end;


    function upper(const s : string) : string;
    {
      return uppercased string of s
    }
      var
        i  : longint;
      begin
        for i:=1 to length(s) do
          upper[i]:=uppertbl[s[i]];
        upper[0]:=s[0];
      end;


    function lower(const s : string) : string;
    {
      return lowercased string of s
    }
      var
        i : longint;
      begin
        for i:=1 to length(s) do
          lower[i]:=lowertbl[s[i]];
        lower[0]:=s[0];
      end;


    procedure uppervar(var s : string);
    {
      uppercase string s
    }
      var
         i : longint;
      begin
         for i:=1 to length(s) do
          s[i]:=uppertbl[s[i]];
      end;


    procedure initupperlower;
      var
        c : char;
      begin
        for c:=#0 to #255 do
         begin
           lowertbl[c]:=c;
           uppertbl[c]:=c;
           case c of
             'A'..'Z' :
               lowertbl[c]:=char(byte(c)+32);
             'a'..'z' :
               uppertbl[c]:=char(byte(c)-32);
           end;
         end;
      end;


    function hexstr(val : cardinal;cnt : byte) : string;
      const
        HexTbl : array[0..15] of char='0123456789ABCDEF';
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


   function tostru(i:cardinal):string;
   {
     return string of value i, but for cardinals
   }
      var
        hs : string;
      begin
        str(i,hs);
        tostru:=hs;
      end;


   function trimbspace(const s:string):string;
   {
     return s with all leading spaces and tabs removed
   }
     var
       i,j : longint;
     begin
       j:=1;
       i:=length(s);
       while (j<i) and (s[j] in [#9,' ']) do
        inc(j);
       trimbspace:=Copy(s,j,i-j+1);
     end;



   function trimspace(const s:string):string;
   {
     return s with all leading and ending spaces and tabs removed
   }
     var
       i,j : longint;
     begin
       i:=length(s);
       while (i>0) and (s[i] in [#9,' ']) do
        dec(i);
       j:=1;
       while (j<i) and (s[j] in [#9,' ']) do
        inc(j);
       trimspace:=Copy(s,j,i-j+1);
     end;


    function GetToken(var s:string;endchar:char):string;
      var
        i : longint;
      begin
        s:=TrimSpace(s);
        i:=pos(EndChar,s);
        if i=0 then
         begin
           GetToken:=s;
           s:='';
         end
        else
         begin
           GetToken:=Copy(s,1,i-1);
           Delete(s,1,i);
         end;
      end;


   function tostr(i : longint) : string;
   {
     return string of value i
   }
     var
        hs : string;
     begin
        str(i,hs);
        tostr:=hs;
     end;


   function int64tostr(i : int64) : string;
   {
     return string of value i
   }
     var
        hs : string;
     begin
        str(i,hs);
        int64tostr:=hs;
     end;


   function tostr_with_plus(i : longint) : string;
   {
     return string of value i, but always include a + when i>=0
   }
     var
        hs : string;
     begin
        str(i,hs);
        if i>=0 then
          tostr_with_plus:='+'+hs
        else
          tostr_with_plus:=hs;
     end;


    procedure valint(S : string;var V : longint;var code : integer);
    {
      val() with support for octal, which is not supported under tp7
    }
{$ifndef FPC}
      var
        vs : longint;
        c  : byte;
      begin
        if s[1]='%' then
          begin
             vs:=0;
             longint(v):=0;
             for c:=2 to length(s) do
               begin
                  if s[c]='0' then
                    vs:=vs shl 1
                  else
                  if s[c]='1' then
                    vs:=vs shl 1+1
                  else
                    begin
                      code:=c;
                      exit;
                    end;
               end;
             code:=0;
             longint(v):=vs;
          end
        else
         system.val(S,V,code);
      end;
{$else not FPC}
      begin
         system.val(S,V,code);
      end;
{$endif not FPC}


    function is_number(const s : string) : boolean;
    {
      is string a correct number ?
    }
      var
         w : integer;
         l : longint;
      begin
         valint(s,l,w);
         is_number:=(w=0);
      end;


    function ispowerof2(value : int64;var power : longint) : boolean;
    {
      return if value is a power of 2. And if correct return the power
    }
      var
         hl : int64;
         i : longint;
      begin
         if value and (value - 1) <> 0 then
           begin
             ispowerof2 := false;
             exit
           end;
         hl:=1;
         ispowerof2:=true;
         for i:=0 to 63 do
           begin
              if hl=value then
                begin
                   power:=i;
                   exit;
                end;
              hl:=hl shl 1;
           end;
         ispowerof2:=false;
      end;


    function maybequoted(const s:string):string;
      var
        s1 : string;
        i  : integer;
      begin
        if (pos('"',s)>0) then
         begin
           s1:='"';
           for i:=1 to length(s) do
            begin
              if s[i]='"' then
               s1:=s1+'\"'
              else
               s1:=s1+s[i];
            end;
           maybequoted:=s1+'"';
         end
        else if (pos(' ',s)>0) then
         maybequoted:='"'+s+'"'
        else
         maybequoted:=s;
      end;


    function pchar2pstring(p : pchar) : pstring;
      var
         w,i : longint;
      begin
         w:=strlen(p);
         for i:=w-1 downto 0 do
           p[i+1]:=p[i];
         p[0]:=chr(w);
         pchar2pstring:=pstring(p);
      end;


    function pstring2pchar(p : pstring) : pchar;
      var
         w,i : longint;
      begin
         w:=length(p^);
         for i:=1 to w do
           p^[i-1]:=p^[i];
         p^[w]:=#0;
         pstring2pchar:=pchar(p);
      end;


    function lowercase(c : char) : char;
       begin
          case c of
             #65..#90 : c := chr(ord (c) + 32);
             #154 : c:=#129;  { german }
             #142 : c:=#132;  { german }
             #153 : c:=#148;  { german }
             #144 : c:=#130;  { french }
             #128 : c:=#135;  { french }
             #143 : c:=#134;  { swedish/norge (?) }
             #165 : c:=#164;  { spanish }
             #228 : c:=#229;  { greek }
             #226 : c:=#231;  { greek }
             #232 : c:=#227;  { greek }
          end;
          lowercase := c;
       end;


    function strpnew(const s : string) : pchar;
      var
         p : pchar;
      begin
         getmem(p,length(s)+1);
         strpcopy(p,s);
         strpnew:=p;
      end;


    procedure strdispose(var p : pchar);
      begin
        if assigned(p) then
         begin
           freemem(p,strlen(p)+1);
           p:=nil;
         end;
      end;


    procedure stringdispose(var p : pstring);
      begin
         if assigned(p) then
           freemem(p,length(p^)+1);
         p:=nil;
      end;


    function stringdup(const s : string) : pstring;
      var
         p : pstring;
      begin
         getmem(p,length(s)+1);
         p^:=s;
         stringdup:=p;
      end;


    function CompareText(S1, S2: string): longint;
      begin
        UpperVar(S1);
        UpperVar(S2);
        if S1<S2 then
         CompareText:=-1
        else
         if S1>S2 then
          CompareText:= 1
        else
         CompareText:=0;
      end;


{*****************************************************************************
                               GetSpeedValue
*****************************************************************************}

{$ifdef ver1_0}
  {$R-}
{$endif}

    var
      Crc32Tbl : array[0..255] of cardinal;

    procedure MakeCRC32Tbl;
      var
        crc : cardinal;
        i,n : integer;
      begin
        for i:=0 to 255 do
         begin
           crc:=i;
           for n:=1 to 8 do
            if odd(longint(crc)) then
             crc:=cardinal(crc shr 1) xor cardinal($edb88320)
            else
             crc:=cardinal(crc shr 1);
           Crc32Tbl[i]:=crc;
         end;
      end;


    Function GetSpeedValue(Const s:String):cardinal;
      var
        i : integer;
        InitCrc : cardinal;
      begin
        if Crc32Tbl[1]=0 then
         MakeCrc32Tbl;
        InitCrc:=cardinal($ffffffff);
        for i:=1 to Length(s) do
         InitCrc:=Crc32Tbl[byte(InitCrc) xor ord(s[i])] xor (InitCrc shr 8);
        GetSpeedValue:=InitCrc;
      end;


{*****************************************************************************
                               Ansistring (PChar+Length)
*****************************************************************************}

    procedure ansistringdispose(var p : pchar;length : longint);
      begin
         if assigned(p) then
           freemem(p,length+1);
         p:=nil;
      end;


    { enable ansistring comparison }
    { 0 means equal }
    { 1 means p1 > p2 }
    { -1 means p1 < p2 }
    function compareansistrings(p1,p2 : pchar;length1,length2 :  longint) : longint;
      var
         i,j : longint;
      begin
         compareansistrings:=0;
         j:=min(length1,length2);
         i:=0;
         while (i<j) do
          begin
            if p1[i]>p2[i] then
             begin
               compareansistrings:=1;
               exit;
             end
            else
             if p1[i]<p2[i] then
              begin
                compareansistrings:=-1;
                exit;
              end;
            inc(i);
          end;
         if length1>length2 then
          compareansistrings:=1
         else
          if length1<length2 then
           compareansistrings:=-1;
      end;


    function concatansistrings(p1,p2 : pchar;length1,length2 : longint) : pchar;
      var
         p : pchar;
      begin
         getmem(p,length1+length2+1);
         move(p1[0],p[0],length1);
         move(p2[0],p[length1],length2+1);
         concatansistrings:=p;
      end;


{*****************************************************************************
                                 File Functions
*****************************************************************************}

    function DeleteFile(const fn:string):boolean;
      var
        f : file;
      begin
        {$I-}
         assign(f,fn);
         erase(f);
        {$I-}
        DeleteFile:=(IOResult=0);
      end;


initialization
  initupperlower;
end.
{
  $Log$
  Revision 1.15  2002-05-14 19:34:40  peter
    * removed old logs and updated copyright year

  Revision 1.14  2002/04/12 17:16:35  carl
  + more documentation of basic unit

}

