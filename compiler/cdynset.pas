{
    Dynamic set

    Copyright (c) 2007-2026 by Florian Klaempfl

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
unit cdynset;

{$i fpcdefs.inc}

  interface

    type
      { byte i contains the elements 8*i..8*i+7, the bits within a byte are
        ordered like in a native set of the host, so the memory layout is the
        same as the one of a native set }
      TDynSet = array of byte;
      PDynSet = ^TDynSet;

    { expand s so that it can hold at least maxelement }
    procedure DynSetExpand(var s : TDynSet;maxelement : integer);

    { add e to s }
    procedure DynSetInclude(var s : TDynSet;e : integer);

    { add s to d }
    procedure DynSetIncludeSet(var d : TDynSet;const s : TDynSet);

    { remove s from d }
    procedure DynSetExcludeSet(var d : TDynSet;const s : TDynSet);

    { remove e from s }
    procedure DynSetExclude(var s : TDynSet;e : integer);

    { test if s contains e }
    function DynSetIn(const s : TDynSet;e : integer) : boolean;

    { d:=s1+s2; }
    procedure DynSetUnion(var d : TDynSet;const s1,s2 : TDynSet);

    { s1+s2 }
    function DynSetUnionF(const s1,s2 : TDynSet) : TDynSet;

    { d:=s1*s2; }
    procedure DynSetIntersect(var d : TDynSet;const s1,s2 : TDynSet);

    { s1*s2 }
    function DynSetIntersectF(const s1,s2 : TDynSet) : TDynSet;

    { d:=s1-s2; }
    procedure DynSetDiff(var d : TDynSet;const s1,s2 : TDynSet);

    { s1-s2 }
    function DynSetDiffF(const s1,s2 : TDynSet) : TDynSet;

    { d:=s1 >< s2; }
    procedure DynSetSymDif(var d : TDynSet;const s1,s2 : TDynSet);

    { s1 >< s2 }
    function DynSetSymDifF(const s1,s2 : TDynSet) : TDynSet;

    { s1<>s2 }
    function DynSetNotEqual(const s1,s2 : TDynSet) : boolean;

    { s1=s2 }
    function DynSetEqual(const s1,s2 : TDynSet) : boolean;

    { s1<=s2 (s1 is subset of s2) }
    function DynSetSubset(const s1,s2 : TDynSet) : boolean;

    { test if s is empty }
    function DynSetIsEmpty(const s : TDynSet) : boolean;

    { return an empty set }
    function DynSetEmpty : TDynSet;

    { return a deep copy of s }
    function DynSetCopy(const s : TDynSet) : TDynSet;

    { output DynSet }
    procedure PrintDynSet(var f : text;s : TDynSet);

  implementation

    uses
      cutils;

    { mask of element e within its byte }
    function DynSetMask(e : integer) : byte; inline;
      begin
{$ifdef FPC_BIG_ENDIAN}
        result:=$80 shr (e mod 8);
{$else FPC_BIG_ENDIAN}
        result:=1 shl (e mod 8);
{$endif FPC_BIG_ENDIAN}
      end;


    procedure DynSetExpand(var s : TDynSet;maxelement : integer);
      var
        needed, oldlen: integer;
      begin
        needed:=(maxelement div 8)+1;
        oldlen:=length(s);
        if needed>oldlen then
          begin
            setlength(s,needed);
            fillchar(s[oldlen],needed-oldlen,0);
          end;
      end;


    procedure DynSetInclude(var s : TDynSet;e : integer);
      var
        e8 : Integer;
      begin
        e8:=e div 8;
        if e8>high(s) then
          SetLength(s,e8+1);
        s[e8]:=s[e8] or DynSetMask(e);
      end;


    procedure DynSetIncludeSet(var d : TDynSet;const s : TDynSet);
      var
        i : integer;
      begin
        if length(s)>length(d) then
          SetLength(d,length(s));
        for i:=0 to high(s) do
          d[i]:=d[i] or s[i];
      end;


    procedure DynSetExcludeSet(var d : TDynSet;const s : TDynSet);
      var
        i : integer;
      begin
        if length(s)>length(d) then
          SetLength(d,length(s));
        for i:=0 to high(s) do
          d[i]:=d[i] and not(s[i]);
      end;


    procedure DynSetExclude(var s : TDynSet;e : integer);
      var
        e8 : Integer;
      begin
        e8:=e div 8;
        if e8<=high(s) then
          s[e8]:=s[e8] and not(DynSetMask(e));
      end;


    function DynSetIn(const s : TDynSet;e : integer) : boolean;
      var
        e8 : Integer;
      begin
        e8:=e div 8;
        if (e<0) or (e8>high(s)) then
          result:=false
        else
          result:=(s[e8] and DynSetMask(e))<>0;
      end;


    procedure DynSetUnion(var d : TDynSet;const s1,s2 : TDynSet);
      var
        i : integer;
      begin
        SetLength(d,max(Length(s1),Length(s2)));
        for i:=0 to min(high(s1),high(s2)) do
          d[i]:=s1[i] or s2[i];
        if high(s1)<high(s2) then
          for i:=high(s1)+1 to high(s2) do
            d[i]:=s2[i]
        else
          for i:=high(s2)+1 to high(s1) do
            d[i]:=s1[i];
      end;


    function DynSetUnionF(const s1,s2 : TDynSet) : TDynSet;
      begin
        result:=nil;
        DynSetUnion(result,s1,s2);
      end;


    procedure DynSetIntersect(var d : TDynSet;const s1,s2 : TDynSet);
      var
        i : integer;
      begin
        SetLength(d,min(Length(s1),Length(s2)));
        for i:=0 to high(d) do
          d[i]:=s1[i] and s2[i];
      end;


    function DynSetIntersectF(const s1,s2 : TDynSet) : TDynSet;
      begin
        result:=nil;
        DynSetIntersect(result,s1,s2);
      end;


    procedure DynSetDiff(var d : TDynSet;const s1,s2 : TDynSet);
      var
        i : integer;
      begin
        SetLength(d,length(s1));
        for i:=0 to high(d) do
          if i>high(s2) then
            d[i]:=s1[i]
          else
            d[i]:=s1[i] and not(s2[i]);
      end;


    function DynSetDiffF(const s1,s2 : TDynSet) : TDynSet;
      begin
        result:=nil;
        DynSetDiff(result,s1,s2);
      end;


    procedure DynSetSymDif(var d : TDynSet;const s1,s2 : TDynSet);
      var
        i, minlen : integer;
      begin
        SetLength(d,max(Length(s1),Length(s2)));
        minlen:=min(length(s1),length(s2));
        for i:=0 to minlen-1 do
          d[i]:=s1[i] xor s2[i];
        for i:=minlen to length(s1)-1 do
          d[i]:=s1[i];
        for i:=minlen to length(s2)-1 do
          d[i]:=s2[i];
      end;


    function DynSetSymDifF(const s1,s2 : TDynSet) : TDynSet;
      begin
        result:=nil;
        DynSetSymDif(result,s1,s2);
      end;


    function DynSetNotEqual(const s1,s2 : TDynSet) : boolean;
      begin
        result:=not DynSetEqual(s1,s2);
      end;


    function DynSetEqual(const s1,s2 : TDynSet) : boolean;
      var
        i, minlen : integer;
      begin
        minlen:=min(length(s1),length(s2));
        for i:=0 to minlen-1 do
          if s1[i]<>s2[i] then
            begin
              result:=false;
              exit;
            end;
        { check that trailing bytes in the longer array are all zero }
        for i:=minlen to length(s1)-1 do
          if s1[i]<>0 then
            begin
              result:=false;
              exit;
            end;
        for i:=minlen to length(s2)-1 do
          if s2[i]<>0 then
            begin
              result:=false;
              exit;
            end;
        result:=true;
      end;


    function DynSetSubset(const s1,s2 : TDynSet) : boolean;
      var
        i: integer;
      begin
        for i:=0 to length(s1)-1 do
          begin
            if i<length(s2) then
              begin
                if (s1[i] and not s2[i])<>0 then
                  begin
                    result:=false;
                    exit;
                  end;
              end
            else
              begin
                if s1[i]<>0 then
                  begin
                    result:=false;
                    exit;
                  end;
              end;
          end;
        result:=true;
      end;


    function DynSetIsEmpty(const s : TDynSet) : boolean;
      var
        i: integer;
      begin
        for i:=0 to length(s)-1 do
          if s[i]<>0 then
            begin
              result:=false;
              exit;
            end;
        result:=true;
      end;


    function DynSetEmpty : TDynSet;
      begin
        result:=nil;
      end;


    function DynSetCopy(const s : TDynSet) : TDynSet;
      begin
        result:=nil;
        setlength(result,length(s));
        if length(s)>0 then
          move(s[0],result[0],length(s));
      end;


    procedure PrintDynSet(var f : text;s : TDynSet);
      var
        i : integer;
        first : boolean;
      begin
        first:=true;
        for i:=0 to Length(s)*8 do
          begin
            if DynSetIn(s,i) then
              begin
                if not(first) then
                  write(f,',');
                write(f,i);
                first:=false;
              end;
          end;
      end;


end.
