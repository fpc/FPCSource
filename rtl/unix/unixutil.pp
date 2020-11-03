{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2013 by the Free Pascal development team

    DO NOT ADD ROUTINES TO THIS FILE!
    THE ROUTINES IN THIS FILE ARE INTERNAL AND NOT FOR END USER USAGE!

    Background: This unit contains leftovers from the unix restructure that
    shouldn't be in the interface of unit baseunix/unix, but are needed
    in these units. (at the time routines were still being moved
    from baseunix to unix, and unit baseunix couldn't depend on unix) 
    
    The routines are fairly OS independent but can't move to
    OS independent because the lowlevel units baseunix/unix depend
    on them. If they need to be generally accessable, copy these
    functions to a general purpose, OS independent, supportable unit.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit unixutil;

interface

uses BaseUnix;

Function StringToPPChar(S: PChar;ReserveEntries:integer):ppchar;
Function StringToPPChar(Var S:RawByteString;ReserveEntries:integer):ppchar;
function ArrayStringToPPchar(const S:Array of RawByteString;reserveentries:Longint):ppchar; // const ?

implementation

function ArrayStringToPPchar(const S:Array of RawByteString;reserveentries:Longint):ppchar; // const ?
// Extra allocate reserveentries pchar's at the beginning (default param=0 after 1.0.x ?)
// Note: for internal use by skilled programmers only
// if "s" goes out of scope in the parent procedure, the pointer is dangling.

var p   : ppchar;
    i   : LongInt;
begin
  if High(s)<Low(s) Then Exit(NIL);
  Getmem(p,sizeof(pchar)*(high(s)-low(s)+ReserveEntries+2));  // one more for NIL, one more
                                              // for cmd
  if p=nil then
    begin
      {$ifdef xunix}
      fpseterrno(ESysEnomem);
      {$endif}
      exit(NIL);
    end;
  for i:=low(s) to high(s) do
     p[i+Reserveentries]:=pchar(s[i]);
  p[high(s)+1+Reserveentries]:=nil;
  ArrayStringToPPchar:=p;
end;

Function StringToPPChar(Var S:RawByteString;ReserveEntries:integer):ppchar;
{
  Create a PPChar to structure of pchars which are the arguments specified
  in the string S. Especially useful for creating an ArgV for Exec-calls
}

begin
  StringToPPChar:=StringToPPChar(PChar(S),ReserveEntries);
end;

Function StringToPPChar(S: PChar;ReserveEntries:integer):ppchar;

var
  i,nr  : longint;
  Buf : ^char;
  p   : ppchar;

begin
  buf:=s;
  nr:=1;
  while (buf^<>#0) do                   // count nr of args
   begin
     while (buf^ in [' ',#9,#10]) do    // Kill separators.
      inc(buf);
     inc(nr);
     if buf^='"' Then                   // quotes argument?
      begin
        inc(buf);
        while not (buf^ in [#0,'"']) do // then end of argument is end of string or next quote
         inc(buf);
        if buf^='"' then                // skip closing quote.
          inc(buf);
      end
     else
       begin                            // else std
         while not (buf^ in [' ',#0,#9,#10]) do
           inc(buf);
       end;
   end;
  getmem(p,(ReserveEntries+nr)*sizeof(pchar));
  StringToPPChar:=p;
  if p=nil then
   begin
     {$ifdef xunix}
     fpseterrno(ESysEnomem);
     {$endif}
     exit;
   end;
  for i:=1 to ReserveEntries do inc(p); // skip empty slots
  buf:=s;
  while (buf^<>#0) do
   begin
     while (buf^ in [' ',#9,#10]) do    // Kill separators.
      begin
       buf^:=#0;
       inc(buf);
      end;
     if buf^='"' Then                   // quotes argument?
      begin
        inc(buf);
        p^:=buf;
        inc(p);
        p^:=nil;
        while not (buf^ in [#0,'"']) do // then end of argument is end of string or next quote
         inc(buf);
        if buf^='"' then                // skip closing quote.
          begin
            buf^:=#0;
            inc(buf);
          end;
      end
     else
       begin
        p^:=buf;
        inc(p);
        p^:=nil;
         while not (buf^ in [' ',#0,#9,#10]) do
           inc(buf);
       end;
   end;
end;

end.
