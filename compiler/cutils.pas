{
    $Id$
    Copyright (C) 1998-2000 by Florian Klaempfl

    This unit implements some support functions

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
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

    function min(a,b : longint) : longint;
    function max(a,b : longint) : longint;
    function align(i,a:longint):longint;
    function align_from_size(datasize:longint;length:longint):longint;
    procedure Replace(var s:string;s1:string;const s2:string);
    procedure ReplaceCase(var s:string;const s1,s2:string);
    function upper(const s : string) : string;
    function lower(const s : string) : string;
    function trimspace(const s:string):string;
    procedure uppervar(var s : string);
    function hexstr(val : longint;cnt : byte) : string;
    function tostru(i:cardinal) : string;
    function tostr(i : longint) : string;
    function tostr_with_plus(i : longint) : string;
    procedure valint(S : string;var V : longint;var code : integer);
    function is_number(const s : string) : boolean;
    function ispowerof2(value : longint;var power : longint) : boolean;

    { releases the string p and assignes nil to p }
    { if p=nil then freemem isn't called          }
    procedure stringdispose(var p : pstring);


    { allocates mem for a copy of s, copies s to this mem and returns }
    { a pointer to this mem                                           }
    function stringdup(const s : string) : pstring;

    { allocates memory for s and copies s as zero terminated string
      to that mem and returns a pointer to that mem }
    function  strpnew(const s : string) : pchar;
    procedure strdispose(var p : pchar);

    { makes a char lowercase, with spanish, french and german char set }
    function lowercase(c : char) : char;

    { makes zero terminated string to a pascal string }
    { the data in p is modified and p is returned     }
    function pchar2pstring(p : pchar) : pstring;

    { ambivalent to pchar2pstring }
    function pstring2pchar(p : pstring) : pchar;

{ Speed/Hash value }
function getspeedvalue(const s : string) : longint;

{ Ansistring (pchar+length) support }
procedure ansistringdispose(var p : pchar;length : longint);
function compareansistrings(p1,p2 : pchar;length1,length2 : longint) : longint;
function concatansistrings(p1,p2 : pchar;length1,length2 : longint) : pchar;


implementation

uses
{$ifdef delphi}
  sysutils
{$else}
  strings
{$endif}
  ;

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

    function align_from_size(datasize:longint;length:longint):longint;

    {Increases the datasize with the required alignment; i.e. on pentium
     words should be aligned word; and dwords should be aligned dword.
     So for a word (len=2), datasize is increased to the nearest multiple
     of 2, and for len=4, datasize is increased to the nearest multiple of
     4.}

    var data_align:word;

    begin
        {$IFDEF I386}
        if length>2 then
            data_align:=4
        else if length>1 then
            data_align:=2
        else
            data_align:=1;
        {$ENDIF}
        {$IFDEF M68K}
        data_align:=2;
        {$ENDIF}
        align_from_size:=(datasize+data_align-1) and not(data_align-1);
    end;


    function align(i,a:longint):longint;
    {
      return value <i> aligned <a> boundary
    }
      begin
        { for 0 and 1 no aligning is needed }
        if a<=1 then
         align:=i
        else
         align:=(i+a-1) and not(a-1);
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
          if s[i] in ['a'..'z'] then
           upper[i]:=char(byte(s[i])-32)
          else
           upper[i]:=s[i];
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
          if s[i] in ['A'..'Z'] then
           lower[i]:=char(byte(s[i])+32)
          else
           lower[i]:=s[i];
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
          if s[i] in ['a'..'z'] then
           s[i]:=char(byte(s[i])-32);
      end;

    function hexstr(val : longint;cnt : byte) : string;
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


    function ispowerof2(value : longint;var power : longint) : boolean;
    {
      return if value is a power of 2. And if correct return the power
    }
      var
         hl : longint;
         i : longint;
      begin
         hl:=1;
         ispowerof2:=true;
         for i:=0 to 31 do
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



{*****************************************************************************
                               GetSpeedValue
*****************************************************************************}

var
  Crc32Tbl : array[0..255] of longint;

procedure MakeCRC32Tbl;
var
  crc : longint;
  i,n : byte;
begin
  for i:=0 to 255 do
   begin
     crc:=i;
     for n:=1 to 8 do
      if odd(crc) then
       crc:=(crc shr 1) xor longint($edb88320)
      else
       crc:=crc shr 1;
     Crc32Tbl[i]:=crc;
   end;
end;


{$ifopt R+}
  {$define Range_check_on}
{$endif opt R+}

{$R- needed here }
{CRC 32}
Function GetSpeedValue(Const s:String):longint;
var
  i,InitCrc : longint;
begin
  if Crc32Tbl[1]=0 then
   MakeCrc32Tbl;
  InitCrc:=-1;
  for i:=1 to Length(s) do
   InitCrc:=Crc32Tbl[byte(InitCrc) xor ord(s[i])] xor (InitCrc shr 8);
  GetSpeedValue:=InitCrc;
end;

{$ifdef Range_check_on}
  {$R+}
  {$undef Range_check_on}
{$endif Range_check_on}


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
    function compareansistrings(p1,p2 : pchar;length1,length2 : longint) : longint;
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




end.
{
  $Log$
  Revision 1.2  2000-09-24 15:06:14  peter
    * use defines.inc

  Revision 1.1  2000/08/27 16:11:50  peter
    * moved some util functions from globals,cobjects to cutils
    * splitted files into finput,fmodule

}
