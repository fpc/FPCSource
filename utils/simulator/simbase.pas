{
    This file is part of the Free Pascal simulator environment
    Copyright (c) 1999-2000 by Florian Klaempfl

    This unit implemements some helper routines

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$N+}
{$H-}
unit simbase;

  interface
{$ifdef Delphi}
    uses
       dmisc;
{$else Delphi}
    uses
       dos;
{$endif Delphi}

    { global types }
    type
       { tindex must be at least of type integer }
       tindex = integer;
{$ifndef FPC}
       int64 = comp;
       qword = comp;
{$endif FPC}
       dword = longint;
       tdword = array[0..3] of byte;

       pbyte = ^byte;
       pword = ^word;
       pdword = ^dword;
       pqword = ^qword;

       tqwordrec = record
          case tindex of
             1 : (low32,high32 : dword);
             2 : (bytes : array[0..7] of byte);
             3 : (words : array[0..3] of word);
       end;

       oword = array[0..7] of word;

       towordrec = record
          case tindex of
             1 : (bytes : array[0..15] of byte);
             2 : (words : array[0..7] of word);
             3 : (low64,high64 : qword);
       end;

    function hexstr(val : longint;cnt : byte) : string;
    function qword2str(q : qword) : string;
    function realtime : double;

    var
       stopsim : procedure;

  implementation

    function hexstr(val : longint;cnt : byte) : string;

       const
          HexTbl : array[0..15] of char='0123456789ABCDEF';

       var
         i : tindex;

       begin
          hexstr[0]:=char(cnt);
          for i:=cnt downto 1 do
            begin
               hexstr[i]:=hextbl[val and $f];
               val:=val shr 4;
            end;
       end;

    function qword2str(q : qword) : string;

      begin
         qword2str:=hexstr(tqwordrec(q).high32,8)+hexstr(tqwordrec(q).low32,8);
      end;

    function realtime : double;

      var
         h,m,s,s100 : word;

      begin
         gettime(h,m,s,s100);
         realtime:=h*3600+m*60+s+s100/100.0;
      end;

    procedure _stopsim;{$ifdef TP}far;{$endif TP}

      begin
         writeln('Simulation stopped');
         halt(1);
      end;

begin
   {$ifdef FPC}
   stopsim:=@_stopsim;
   {$else FPC}
   stopsim:=_stopsim;
   {$endif FPC}
end.
