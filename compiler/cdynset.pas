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
      TDynSet = array of byte;
      PDynSet = ^TDynSet;

    { add e to s }
    procedure DynSetInclude(var s : TDynSet;e : integer);

    { add s to d }
    procedure DynSetIncludeSet(var d : TDynSet;const s : TDynSet);

    { remove s to d }
    procedure DynSetExcludeSet(var d : TDynSet;const s : TDynSet);

    { remove e from s }
    procedure DynSetExclude(var s : TDynSet;e : integer);

    { test if s contains e }
    function DynSetIn(const s : TDynSet;e : integer) : boolean;

    { d:=s1+s2; }
    procedure DynSetUnion(var d : TDynSet;const s1,s2 : TDynSet);

    { d:=s1*s2; }
    procedure DynSetIntersect(var d : TDynSet;const s1,s2 : TDynSet);

    { d:=s1-s2; }
    procedure DynSetDiff(var d : TDynSet;const s1,s2 : TDynSet);

    { s1<>s2; }
    function DynSetNotEqual(const s1,s2 : TDynSet) : boolean;

    { output DynSet }
    procedure PrintDynSet(var f : text;s : TDynSet);

  implementation

    uses
      cutils;

    procedure DynSetInclude(var s : tDynset;e : integer);
      var
        e8 : Integer;
      begin
        e8:=e div 8;
        if e8>high(s) then
          SetLength(s,e8+1);
        s[e8]:=s[e8] or (1 shl (e mod 8));
      end;


    procedure DynSetIncludeSet(var d : tDynset;const s : tDynset);
      var
        i : integer;
      begin
        if length(s)>length(d) then
          SetLength(d,length(s));
        for i:=0 to high(s) do
          d[i]:=d[i] or s[i];
      end;


    procedure DynSetExcludeSet(var d : tDynset;const s : tDynset);
      var
        i : integer;
      begin
        if length(s)>length(d) then
          SetLength(d,length(s));
        for i:=0 to high(s) do
          d[i]:=d[i] and not(s[i]);
      end;


    procedure DynSetExclude(var s : tDynset;e : integer);
      var
        e8 : Integer;
      begin
        e8:=e div 8;
        if e8<=high(s) then
          s[e8]:=s[e8] and not(1 shl (e mod 8));
      end;


    function DynSetIn(const s : tDynset;e : integer) : boolean;
      var
        e8 : Integer;
      begin
        e8:=e div 8;
        if e8<=high(s) then
          result:=(s[e8] and (1 shl (e mod 8)))<>0
        else
          result:=false;
      end;


    procedure DynSetUnion(var d : tDynset;const s1,s2 : tDynset);
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


    procedure DynSetIntersect(var d : tDynset;const s1,s2 : tDynset);
      var
        i : integer;
      begin
        SetLength(d,min(Length(s1),Length(s2)));
        for i:=0 to high(d) do
          d[i]:=s1[i] and s2[i];
      end;


    procedure DynSetDiff(var d : tDynset;const s1,s2 : tDynset);
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


    function DynSetNotEqual(const s1,s2 : tDynset) : boolean;
      var
        i : integer;
      begin
        result:=true;
        { one set could be larger than the other }
        if length(s1)>length(s2) then
          begin
            for i:=0 to high(s2) do
              if s1[i]<>s2[i] then
                exit;
            { check remaining part being zero }
            for i:=length(s2) to high(s1) do
              if s1[i]<>0 then
                exit;
          end
        else
          begin
            for i:=0 to high(s1) do
              if s1[i]<>s2[i] then
                exit;
            { check remaining part being zero }
            for i:=length(s1) to high(s2) do
              if s2[i]<>0 then
                exit;
          end;
        result:=false;
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

