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

      toptinfo = record
        { index of the current node inside the dfa sets, aword(-1) if no entry }
        index : aword;
        def : tdfaset;
        use : tdfaset;
        life : tdfaset;
      end;

      poptinfo = ^toptinfo;

    { basic set operations for dfa sets }
    procedure TDFASetInclude(var s : tdfaset;e : integer);
    procedure TDFASetExclude(var s : tdfaset;e : integer);
    function TDFASetIn(const s : tdfaset;e : integer) : boolean;
    procedure TDFASetUnion(var d : tdfaset;const s1,s2 : tdfaset);
    procedure TDFASetIntersect(var d : tdfaset;const s1,s2 : tdfaset);
    procedure TDFASetDiff(var d : tdfaset;const s1,s2 : tdfaset);
    function DFASetNotEqual(const s1,s2 : tdfaset) : boolean;

  implementation

    uses
      cutils;

    procedure TDFASetInclude(var s : tdfaset;e : integer);
      var
        e8 : Integer;
      begin
        e8:=e div 8;
        if e8>high(s) then
          SetLength(s,e8+1);
        s[e8]:=s[e8] or (1 shl (e mod 8));
      end;


    procedure TDFASetExclude(var s : tdfaset;e : integer);
      var
        e8 : Integer;
      begin
        e8:=e div 8;
        if e8>high(s) then
          SetLength(s,e8+1);
        s[e8]:=s[e8] and not(1 shl (e mod 8));
      end;


    function TDFASetIn(const s : tdfaset;e : integer) : boolean;
      var
        e8 : Integer;
      begin
        result:=false;
        e8:=e div 8;
        if e8>high(s) then
          exit;
        result:=(s[e8] and (1 shl (e mod 8)))<>0;
      end;


    procedure TDFASetUnion(var d : tdfaset;const s1,s2 : tdfaset);
      var
        i : integer;
      begin
        SetLength(d,max(Length(s1),Length(s2)));
        for i:=0 to high(s1) do
          d[i]:=s1[i];
        for i:=0 to high(s2) do
          d[i]:=d[i] or s2[i];
      end;


    procedure TDFASetIntersect(var d : tdfaset;const s1,s2 : tdfaset);
      var
        i : integer;
      begin
        SetLength(d,min(Length(s1),Length(s2)));
        for i:=0 to min(high(s1),high(s2)) do
          d[i]:=s1[i] and s2[i];
      end;


    procedure TDFASetDiff(var d : tdfaset;const s1,s2 : tdfaset);
      var
        i : integer;
      begin
        SetLength(d,min(Length(s1),Length(s2)));
        for i:=0 to min(high(s1),high(s2)) do
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

end.
