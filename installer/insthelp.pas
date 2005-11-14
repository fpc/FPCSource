{
    Helper routines for installer

    This file is part of the Free Pascal installer.

    Copyright (c) 1993-2005 by Florian Klaempfl
    member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit insthelp;

  interface

    function RTrim(const S: string): string;
    function LTrim(const S: string): string;
    function Trim(const S: string): string;
    function CompareText(S1, S2: string): integer;
    function ExtOf(const S: string): string;
    function DirAndNameOf(const S: string): string;
    function DirOf(const S: string): string;

  implementation

    uses
      dos;

    function RTrim(const S: string): string;
      var
        i : longint;
      begin
        i:=length(s);
        while (i>0) and (s[i]=' ') do
         dec(i);
        RTrim:=Copy(s,1,i);
      end;

    function LTrim(const S: string): string;
      var
        i : longint;
      begin
        i:=1;
        while (i<length(s)) and (s[i]=' ') do
         inc(i);
        LTrim:=Copy(s,i,255);
      end;

    function Trim(const S: string): string;
      begin
        Trim:=RTrim(LTrim(S));
      end;

    function CompareText(S1, S2: string): integer;
      var R: integer;
      begin
        S1:=Upcase(S1);
        S2:=Upcase(S2);
        if S1<S2 then R:=-1 else
        if S1>S2 then R:= 1 else
        R:=0;
        CompareText:=R;
      end;

    function ExtOf(const S: string): string;
      var D: DirStr; E: ExtStr; N: NameStr;
      begin
        FSplit(S,D,N,E);
        ExtOf:=E;
      end;

    function DirAndNameOf(const S: string): string;
      var D: DirStr; E: ExtStr; N: NameStr;
      begin
        FSplit(S,D,N,E);
        DirAndNameOf:=D+N;
      end;

    function DirOf(const S: string): string;
      var D: DirStr; E: ExtStr; N: NameStr;
      begin
        FSplit(S,D,N,E);
        DirOf:=D;
      end;

  end.
