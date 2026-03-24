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
  compilerbase,
  node;

type
  tobjcselectornode = class(tunarynode)
   public
    constructor create(formethod: tnode;acompiler:TCompilerBase);
    function pass_typecheck: tnode;override;
    function pass_1: tnode;override;
  end;
  tobjcselectornodeclass = class of tobjcselectornode;

  tobjcprotocolnode = class(tunarynode)
   public
    constructor create(forprotocol: tnode;acompiler:TCompilerBase);
    function pass_typecheck: tnode;override;
    function pass_1: tnode;override;
  end;
  tobjcprotocolnodeclass = class of tobjcprotocolnode;

var
  cobjcselectornode : tobjcselectornodeclass;
  cobjcprotocolnode : tobjcprotocolnodeclass;

implementation

uses
  globtype,globals,
  verbose,pass_1,compiler,
  symdef,symconst,
  ncon,ncal,
  objcutil,
  cgbase;


{*****************************************************************************
                            TOBJCSELECTORNODE
*****************************************************************************}

constructor tobjcselectornode.create(formethod: tnode;acompiler:TCompilerBase);
  begin
    inherited create(objcselectorn,formethod,acompiler);
  end;


function tobjcselectornode.pass_typecheck: tnode;
  var
    len: longint;
    s: shortstring;
  begin
    s:='';
    if not(m_objectivec1 in compiler.globals.current_settings.modeswitches) then
      compiler.verbose.Message(parser_f_modeswitch_objc_required);
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
            compiler.verbose.CGMessage1(type_e_expected_objc_method_but_got,left.resultdef.typename);
        end;
      stringconstn:
        begin
          if not compiler.objcutil.objcvalidselectorname(tstringconstnode(left).asconstpchar,
                                                         tstringconstnode(left).len) then
            begin
              len:=tstringconstnode(left).len;
              if (len>255) then
                len:=255;
              setlength(s,len);
              if len>0 then
                move(tstringconstnode(left).valueas[0],s[1],len);
              compiler.verbose.CGMessage1(type_e_invalid_objc_selector_name,s);
              exit;
            end;
        end
      else
        compiler.verbose.CGMessage(type_e_expected_objc_method);
    end;
    resultdef:=objc_seltype;
  end;


function tobjcselectornode.pass_1: tnode;
  begin
    result:=nil;
    expectloc:=LOC_CREFERENCE;
  end;


{*****************************************************************************
                            TOBJPROTOCOLNODE
*****************************************************************************}

constructor tobjcprotocolnode.create(forprotocol: tnode;acompiler:TCompilerBase);
  begin
    inherited create(objcprotocoln,forprotocol,acompiler);
  end;


function tobjcprotocolnode.pass_typecheck: tnode;
  begin
    if not(m_objectivec1 in compiler.globals.current_settings.modeswitches) then
      compiler.verbose.Message(parser_f_modeswitch_objc_required);
    result:=nil;
    typecheckpass(left);
    if (left.nodetype<>typen) then
      compiler.verbose.MessagePos(left.fileinfo,type_e_type_id_expected)
    else if not is_objcprotocol(left.resultdef) then
      compiler.verbose.MessagePos2(left.fileinfo,type_e_incompatible_types,left.resultdef.typename,'ObjCProtocol');
    resultdef:=objc_protocoltype;
  end;


function tobjcprotocolnode.pass_1: tnode;
  begin
    result:=compiler.ccallnode_internresfromunit('OBJC','OBJC_GETPROTOCOL',
      compiler.ccallparanode(compiler.cstringconstnode_str(tobjectdef(left.resultdef).objextname^),nil),
      resultdef
    );
    typecheckpass(result);
  end;


end.

