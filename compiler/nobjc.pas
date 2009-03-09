{
    Copyright (c) 2009 by Jonas Maebe

    This unit implements Objective-C nodes

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
{ @abstract(This unit implements Objective-C nodes)
  This unit contains various nodes to implement Objective-Pascal and to
  interface with the Objective-C runtime.
}

unit nobjc;

{$i fpcdefs.inc}

interface

uses
  node;

type
  tobjcselectornode = class(tunarynode)
   public
    constructor create(formethod: tnode);
    function pass_typecheck:tnode;override;
    function pass_1 : tnode;override;
  end;
  tobjcselectornodeclass = class of tobjcselectornode;

var
  cobjcselectornode : tobjcselectornodeclass;

implementation

uses
  sysutils,
  cclasses,
  verbose,pass_1,
  defutil,symtable,symdef,symconst,
  ncnv,ncon,ncal,nmem;


{*****************************************************************************
                            TOBJCSELECTORNODE
*****************************************************************************}

constructor tobjcselectornode.create(formethod: tnode);
  begin
    inherited create(objcselectorn,formethod);
  end;


function validselectorname(value_str: pchar; len: longint): boolean;
  var
    i         : longint;
    gotcolon  : boolean;
begin
  result:=false;
  { empty name is not allowed }
  if (len=0) then
    exit;

  gotcolon:=false;

  { if the first character is a colon, all of them must be colons }
  if (value_str[0] = ':') then
    begin
      for i:=1 to len-1 do
        if (value_str[i]<>':') then
          exit;
    end
  else
    begin
      { no special characters other than ':'
        (already checked character 0, so start checking from 1)
      }
      for i:=1 to len-1 do
        if (value_str[i] = ':') then
          gotcolon:=true
        else if not(value_str[i] in ['_','A'..'Z','a'..'z','0'..'9',':']) then
          exit;

      { if there is at least one colon, the final character must
        also be a colon (in case it's only one character that is
        a colon, this was already checked before the above loop)
      }
      if gotcolon and
         (value_str[len-1] <> ':') then
        exit;
    end;


  result:=true;
end;


function tobjcselectornode.pass_typecheck: tnode;
  begin
    result:=nil;
    typecheckpass(left);
    { argument can be
       a) an objc method
       b) a pchar, zero-based chararray or ansistring
    }
    case left.nodetype of
      loadn:
        begin
          if (left.resultdef.typ=procdef) and
             (po_objc in tprocdef(left.resultdef).procoptions) then
            begin
              { ok }
            end
          else
            CGMessage1(type_e_expected_objc_method_but_got,left.resultdef.typename);
        end;
      stringconstn:
        begin
          if not validselectorname(tstringconstnode(left).value_str,
                                   tstringconstnode(left).len) then
            begin
              CGMessage(type_e_invalid_objc_selector_name);
              exit;
            end;
        end
      else
        CGMessage(type_e_expected_objc_method);
    end;
    resultdef:=search_system_type('SEL').typedef;
  end;


function tobjcselectornode.pass_1: tnode;
  begin
    result:=nil;
  end;

end.

