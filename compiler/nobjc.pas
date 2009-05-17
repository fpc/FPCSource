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

  tobjcmessagesendnode = class(tunarynode)
   public
    constructor create(forcall: tnode);
    function pass_typecheck: tnode;override;
    function pass_1: tnode;override;
  end;
  tobjcmessagesendnodeclass = class of tobjcmessagesendnode;

var
  cobjcselectornode : tobjcselectornodeclass;
  cobjcmessagesendnode : tobjcmessagesendnodeclass;

implementation

uses
  sysutils,
  globtype,cclasses,
  verbose,pass_1,
  defutil,
  symtype,symtable,symdef,symconst,symsym,
  paramgr,
  nbas,nld,ncnv,ncon,ncal,nmem,
  objcutil,
  cgbase;


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
    resultdef:=objc_seltype;
  end;


function tobjcselectornode.pass_1: tnode;
  begin
    result:=nil;
    expectloc:=LOC_CREFERENCE;
  end;

{*****************************************************************************
                          TOBJCMESSAGESENDNODE
*****************************************************************************}

constructor tobjcmessagesendnode.create(forcall: tnode);
  begin
    if (forcall.nodetype<>calln) then
      internalerror(2009032502);
    { typecheck pass (and pass1) must already have run on the call node,
      because pass1 of the callnode creates this node
    }
    inherited create(objcmessagesendn,forcall);
  end;


function tobjcmessagesendnode.pass_typecheck: tnode;
  begin
    { typecheckpass of left has already run, see constructor }
    resultdef:=left.resultdef;
    result:=nil;
    expectloc:=left.expectloc;
  end;


function tobjcmessagesendnode.pass_1: tnode;
  var
    msgsendname: string;
    newparas,
    lastpara: tcallparanode;
    block: tblocknode;
    statements: tstatementnode;
    temp: ttempcreatenode;
    objcsupertype: tdef;
    field: tfieldvarsym;
  begin
    { pass1 of left has already run, see constructor }

    { default behaviour: call objc_msgSend and friends;
      ppc64 and x86_64 for Mac OS X have to override this as they
      call messages via an indirect function call similar to
      dynamically linked functions, ARM maybe as well (not checked)

      Which variant of objc_msgSend is used depends on the
      result type, and on whether or not it's an inherited call.
    }

    { record returned via implicit pointer }
    if paramanager.ret_in_param(left.resultdef,tcallnode(left).procdefinition.proccalloption) then
      if not(cnf_inherited in tcallnode(left).callnodeflags) then
        msgsendname:='OBJC_MSGSEND_STRET'
      else
        msgsendname:='OBJC_MSGSENDSUPER_STRET'
{$ifdef i386}
    { special case for fpu results on i386 for non-inherited calls }
    else if (left.resultdef.typ=floatdef) and
            not(cnf_inherited in tcallnode(left).callnodeflags) then
      msgsendname:='OBJC_MSGSENF_FPRET'
{$endif}
    { default }
    else if not(cnf_inherited in tcallnode(left).callnodeflags) then
      msgsendname:='OBJC_MSGSEND'
    else
      msgsendname:='OBJC_MSGSENDSUPER';

    newparas:=tcallparanode(tcallnode(left).left);
    { parameters are chained from right to left, and we have to insert the two
      first parameters (self and selector) -> walk to the end of the chain
    }
    lastpara:=newparas;
    while assigned(lastpara.right) do
      lastpara:=tcallparanode(lastpara.right);
    { Handle self }
    { 1) If we're calling a class method, use a class ref.  }
    if (po_classmethod in tcallnode(left).procdefinition.procoptions) and
       ((tcallnode(left).methodpointer.nodetype=typen) or
        (tcallnode(left).methodpointer.resultdef.typ<>classrefdef)) then
      begin
        tcallnode(left).methodpointer:=cloadvmtaddrnode.create(tcallnode(left).methodpointer);
        firstpass(tcallnode(left).methodpointer);
      end;
    { 2) convert parameter to id to match objc_MsgSend* signatures }
    inserttypeconv_internal(tcallnode(left).methodpointer,objc_idtype);
    { in case of sending a message to a superclass, self is a pointer to
      and objc_super record
    }
    if (cnf_inherited in tcallnode(left).callnodeflags) then
      begin
         block:=internalstatements(statements);
         objcsupertype:=search_named_unit_globaltype('OBJC1','OBJC_SUPER').typedef;
         if (objcsupertype.typ<>recorddef) then
           internalerror(2009032901);
         { temp for the for the objc_super record }
         temp:=ctempcreatenode.create(objcsupertype,objcsupertype.size,tt_persistent,false);
         addstatement(statements,temp);
         { initialize objc_super record: first the destination object instance }
         field:=tfieldvarsym(trecorddef(objcsupertype).symtable.find('RECEIVER'));
         if not assigned(field) then
           internalerror(2009032902);
         addstatement(statements,
           cassignmentnode.create(
             csubscriptnode.create(field,ctemprefnode.create(temp)),
             tcallnode(left).methodpointer
           )
         );
         { and secondly, the destination class type }
         field:=tfieldvarsym(trecorddef(objcsupertype).symtable.find('_CLASS'));
         if not assigned(field) then
           internalerror(2009032903);
         addstatement(statements,
           cassignmentnode.create(
             csubscriptnode.create(field,ctemprefnode.create(temp)),
             objcsuperclassnode(tobjectdef(tcallnode(left).methodpointer.resultdef))
           )
         );
         { result of this block is the address of this temp }
         addstatement(statements,caddrnode.create_internal(ctemprefnode.create(temp)));
         { replace the method pointer with the address of this temp }
         tcallnode(left).methodpointer:=block;
         typecheckpass(block);
      end;
    lastpara.right:=ccallparanode.create(tcallnode(left).methodpointer,nil);
    { insert selector }
    lastpara.right:=ccallparanode.create(
      cobjcselectornode.create(
        cstringconstnode.createstr(tprocdef(tcallnode(left).procdefinition).messageinf.str^)),
     lastpara.right);
    { parameters are reused -> make sure they don't get freed }
    tcallnode(left).left:=nil;
    { methodpointer is also reused }
    tcallnode(left).methodpointer:=nil;
    { and now the call to the Objective-C rtl }
    result:=ccallnode.createinternresfromunit('OBJC1',msgsendname,newparas,left.resultdef);

    if (cnf_inherited in tcallnode(left).callnodeflags) then
      begin
        { free the objc_super temp after the call. We cannout use
          ctempdeletenode.create_normal_temp before the call, because then
          the temp will be released while evaluating the parameters, and thus
          may be reused while evaluating another parameter
        }
        block:=internalstatements(statements);
        addstatement(statements,result);
        addstatement(statements,ctempdeletenode.create(temp));
        typecheckpass(block);
        result:=block;
     end;
  end;


begin
  cobjcmessagesendnode:=tobjcmessagesendnode;
end.

