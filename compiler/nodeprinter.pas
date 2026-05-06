{
    Copyright (c) 2026 by Nikolay Nikolov

    Helper object for debug printing of nodes

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
unit nodeprinter;

{$i fpcdefs.inc}

interface

    const
       printnodespacing = '   ';

    type
      pnodeprinter = ^tnodeprinter;

      { tnodeprinter }

      tnodeprinter = object
        t:ptext;
        { indention used when writing a node tree to the screen }
        printnodeindention : string;

        constructor Init(var atext: text);
        destructor Done;

        { Node dumping support functions }
        procedure printnodeindent; inline;
        procedure printnodeunindent; inline;
      end;


implementation

    { tnodeprinter }

    constructor tnodeprinter.Init(var atext: text);
      begin
        printnodeindention:='';
        t:=@atext;
      end;

    destructor tnodeprinter.Done;
      begin
        t:=nil;
      end;

    procedure tnodeprinter.printnodeindent; inline;
      begin
        printnodeindention:=printnodeindention+printnodespacing;
      end;

    procedure tnodeprinter.printnodeunindent; inline;
      begin
        delete(printnodeindention,1,length(printnodespacing));
      end;


end.

