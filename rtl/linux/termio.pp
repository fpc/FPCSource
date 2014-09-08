{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Peter Vreman
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This file contains the termios interface.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit termio;

interface
{$inline on}

Uses BaseUnix;          // load base unix typing

// load types + consts

{$i termios.inc}

// load default prototypes from unix dir.

{$i termiosh.inc}

implementation

// load implementation for prototypes from current dir.
{$i termiosproc.inc}

{We can implement ttyname more efficiently using proc than by including the
 generic ttyname.inc file.}

function TTYName(Handle:cint):string;

{ Return the name of the current tty described by handle f.
  returns empty string in case of an error.}

var s:string[32];
    t:string[64];

begin
  ttyname:='';
  if isatty(handle)=1 then
    begin
      str(handle,s);
      t:='/proc/self/fd/'+s+#0;
      ttyname[0]:=char(fpreadlink(@t[1],@ttyname[1],255));
    end;
end;

function TTYName(var F:Text):string;{$ifndef ver2_0}inline;{$endif}
{
  Idem as previous, only now for text variables;
}
begin
  TTYName:=TTYName(textrec(f).handle);
end;

end.
