{
    Basic node optimizer stuff

    Copyright (c) 2007 by Florian Klaempfl

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
unit optbase;

{$i fpcdefs.inc}

  interface

    uses
      globtype;

    type
      { this should maybe replaced by a spare set,
        using a dyn. array makes assignments cheap }
      tdfaset = array of byte;
      PDFASet = ^TDFASet;

      toptinfo = record
        { index of the current node inside the dfa sets, aword(-1) if no entry }
        index : aword;
        def : tdfaset;
        use : tdfaset;
        life : tdfaset;
      end;

      poptinfo = ^toptinfo;

    { basic set operations for dfa sets }

    { add e to s }
    procedure DFASetInclude(var s : tdfaset;e : integer);

    { add s to d }
    procedure DFASetIncludeSet(var d : tdfaset;const s : tdfaset);

    { remove e from s }
    procedure DFASetExclude(var s : tdfaset;e : integer);

    { test if s contains e }
    function DFASetIn(const s : tdfaset;e : integer) : boolean;

    { d:=s1+s2; }
    procedure DFASetUnion(var d : tdfaset;const s1,s2 : tdfaset);

    { d:=s1*s2; }
    procedure DFASetIntersect(var d : tdfaset;const s1,s2 : tdfaset);

    { d:=s1-s2; }
    procedure DFASetDiff(var d : tdfaset;const s1,s2 : tdfaset);

    { s1<>s2; }
    function DFASetNotEqual(const s1,s2 : tdfaset) : boolean;

    { output DFA set }
    procedure PrintDFASet(var f : text;s : TDFASet);

  implementation

    uses
      cutils;

    procedure DFASetInclude(var s : tdfaset;e : integer);
      var
        i,
        oldhigh,
        e8 : Integer;
      begin
        e8:=e div 8;
        if e8>high(s) then
          begin
            oldhigh:=high(s);
            SetLength(s,e8+1);
            for i:=oldhigh+1 to high(s) do
              s[i]:=0;
          end;
        s[e8]:=s[e8] or (1 shl (e mod 8));
      end;


    procedure DFASetIncludeSet(var d : tdfaset;const s : tdfaset);
      var
        i : integer;
      begin
        if length(s)>length(d) then
          SetLength(d,length(s));
        for i:=0 to high(s) do
          d[i]:=d[i] or s[i];
      end;


    procedure DFASetExclude(var s : tdfaset;e : integer);
      var
        e8 : Integer;
      begin
        e8:=e div 8;
        if e8>high(s) then
          SetLength(s,e8+1);
        s[e8]:=s[e8] and not(1 shl (e mod 8));
      end;


    function DFASetIn(const s : tdfaset;e : integer) : boolean;
      var
        e8 : Integer;
      begin
        result:=false;
        e8:=e div 8;
        if e8>high(s) then
          exit;
        result:=(s[e8] and (1 shl (e mod 8)))<>0;
      end;


    procedure DFASetUnion(var d : tdfaset;const s1,s2 : tdfaset);
      var
        i : integer;
      begin
        SetLength(d,max(Length(s1),Length(s2)));
        for i:=0 to high(s1) do
          d[i]:=s1[i];
        for i:=0 to high(s2) do
          d[i]:=d[i] or s2[i];
      end;


    procedure DFASetIntersect(var d : tdfaset;const s1,s2 : tdfaset);
      var
        i : integer;
      begin
        SetLength(d,min(Length(s1),Length(s2)));
        for i:=0 to min(high(s1),high(s2)) do
          d[i]:=s1[i] and s2[i];
      end;


    procedure DFASetDiff(var d : tdfaset;const s1,s2 : tdfaset);
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


    function DFASetNotEqual(const s1,s2 : tdfaset) : boolean;
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


    procedure PrintDFASet(var f : text;s : TDFASet);
      var
        i : integer;
        first : boolean;
      begin
        first:=true;
        for i:=0 to Length(s)*8 do
          begin
            if DFASetIn(s,i) then
              begin
                if not(first) then
                  write(f,',');
                write(f,i);
                first:=false;
              end;
          end;
      end;


end.
