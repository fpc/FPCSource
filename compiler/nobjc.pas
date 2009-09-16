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
    function pass_typecheck: tnode;override;
    function pass_1: tnode;override;
  end;
  tobjcselectornodeclass = class of tobjcselectornode;

  tobjcprotocolnode = class(tunarynode)
   public
    constructor create(forprotocol: tnode);
    function pass_typecheck: tnode;override;
    function pass_1: tnode;override;
  end;
  tobjcprotocolnodeclass = class of tobjcprotocolnode;

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
  cobjcprotocolnode : tobjcprotocolnodeclass;

implementation

uses
  sysutils,
  globtype,cclasses,
  verbose,pass_1,
  defutil,
  symtype,symtable,symdef,symconst,symsym,
  paramgr,
  nutils,
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


function tobjcselectornode.pass_typecheck: tnode;
  var
    len: longint;
    s: shortstring;
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
          if not objcvalidselectorname(tstringconstnode(left).value_str,
                                       tstringconstnode(left).len) then
            begin
              len:=tstringconstnode(left).len;
              if (len>255) then
                len:=255;
              setlength(s,len);
              move(tstringconstnode(left).value_str^,s[1],len);
              CGMessage1(type_e_invalid_objc_selector_name,s);
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
                            TOBJPROTOCOLNODE
*****************************************************************************}

constructor tobjcprotocolnode.create(forprotocol: tnode);
  begin
    inherited create(objcprotocoln,forprotocol);
  end;


function tobjcprotocolnode.pass_typecheck: tnode;
  begin
    result:=nil;
    typecheckpass(left);
    if (left.nodetype<>typen) then
      MessagePos(left.fileinfo,type_e_type_id_expected)
    else if not is_objcprotocol(left.resultdef) then
      MessagePos2(left.fileinfo,type_e_incompatible_types,left.resultdef.typename,'ObjCProtocol');
    resultdef:=objc_protocoltype;
  end;


function tobjcprotocolnode.pass_1: tnode;
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
    { typecheck pass must already have run on the call node,
      because pass1 of the callnode creates this node right
      at the beginning
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
    para: tcallparanode;
    block,
    selftree  : tnode;
    statements: tstatementnode;
    temp,
    tempresult: ttempcreatenode;
    objcsupertype: tdef;
    field: tfieldvarsym;
    selfpara,
    msgselpara,
    respara,

    prerespara,
    prevpara: tcallparanode;
  begin
    { typecheckpass of left has already run, see constructor }

    { default behaviour: call objc_msgSend and friends;
      ppc64 and x86_64 for Mac OS X have to override this as they
      call messages via an indirect function call similar to
      dynamically linked functions, ARM maybe as well (not checked)

      Which variant of objc_msgSend is used depends on the
      result type, and on whether or not it's an inherited call.
    }

    tempresult:=nil;
    newparas:=tcallparanode(tcallnode(left).left);
    { Find the self and msgsel parameters.  }
    para:=newparas;
    selfpara:=nil;
    msgselpara:=nil;
    respara:=nil;
    prevpara:=nil;
    while assigned(para) do
      begin
        if (vo_is_self in para.parasym.varoptions) then
          selfpara:=para
        else if (vo_is_msgsel in para.parasym.varoptions) then
          msgselpara:=para
        else if (vo_is_funcret in para.parasym.varoptions) then
          begin
            prerespara:=prevpara;
            respara:=para;
          end;
        prevpara:=para;
        para:=tcallparanode(para.right);
      end;
    if not assigned(selfpara) then
      internalerror(2009051801);
    if not assigned(msgselpara) then
      internalerror(2009051802);

    { record returned via implicit pointer }
    if paramanager.ret_in_param(left.resultdef,tcallnode(left).procdefinition.proccalloption) then
      begin
        if not assigned(respara) then
          internalerror(2009091101);
        { Since the result parameter is also hidden in the routine we'll
          call now, it will be inserted again by the callnode. So we have to
          remove the old one, otherwise we'll have two result parameters.
        }
        if (tcallparanode(respara).left.nodetype<>nothingn) then
          internalerror(2009091102);
        if assigned(prerespara) then
          tcallparanode(prerespara).right:=tcallparanode(respara).right
        else
          begin
            tcallnode(left).left:=tcallparanode(respara).right;
            newparas:=tcallparanode(tcallnode(left).left);
          end;
        tcallparanode(respara).right:=nil;
        respara.free;
        if not(cnf_inherited in tcallnode(left).callnodeflags) then
          msgsendname:='OBJC_MSGSEND_STRET'
        else
          msgsendname:='OBJC_MSGSENDSUPER_STRET'
      end
{$ifdef i386}
    { special case for fpu results on i386 for non-inherited calls }
    else if (left.resultdef.typ=floatdef) and
            not(cnf_inherited in tcallnode(left).callnodeflags) then
      msgsendname:='OBJC_MSGSEND_FPRET'
{$endif}
    { default }
    else if not(cnf_inherited in tcallnode(left).callnodeflags) then
      msgsendname:='OBJC_MSGSEND'
    else
      msgsendname:='OBJC_MSGSENDSUPER';


    { Handle self }
    { 1) in case of sending a message to a superclass, self is a pointer to
         an objc_super record
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
         { initialize objc_super record }
         selftree:=load_self_node;

         { we can call an inherited class static/method from a regular method
           -> self node must change from instance pointer to vmt pointer)
         }
         if (po_classmethod in tcallnode(left).procdefinition.procoptions) and
            (selftree.resultdef.typ<>classrefdef) then
           begin
             selftree:=cloadvmtaddrnode.create(selftree);
             typecheckpass(selftree);
           end;
         field:=tfieldvarsym(trecorddef(objcsupertype).symtable.find('RECEIVER'));
         if not assigned(field) then
           internalerror(2009032902);
        { first the destination object/class instance }
         addstatement(statements,
           cassignmentnode.create(
             csubscriptnode.create(field,ctemprefnode.create(temp)),
             selftree
           )
         );
         { and secondly, the class type in which the selector must be looked
           up (the parent class in case of an instance method, the parent's
           metaclass in case of a class method) }
         field:=tfieldvarsym(trecorddef(objcsupertype).symtable.find('_CLASS'));
         if not assigned(field) then
           internalerror(2009032903);
         addstatement(statements,
           cassignmentnode.create(
             csubscriptnode.create(field,ctemprefnode.create(temp)),
             objcsuperclassnode(selftree.resultdef)
           )
         );
         { result of this block is the address of this temp }
         addstatement(statements,caddrnode.create_internal(ctemprefnode.create(temp)));
         { replace the method pointer with the address of this temp }
         tcallnode(left).methodpointer.free;
         tcallnode(left).methodpointer:=block;
         typecheckpass(block);
      end
    else
    { 2) regular call (not inherited) }
      begin
        { a) If we're calling a class method, use a class ref.  }
        if (po_classmethod in tcallnode(left).procdefinition.procoptions) and
           ((tcallnode(left).methodpointer.nodetype=typen) or
            (tcallnode(left).methodpointer.resultdef.typ<>classrefdef)) then
          begin
            tcallnode(left).methodpointer:=cloadvmtaddrnode.create(tcallnode(left).methodpointer);
            firstpass(tcallnode(left).methodpointer);
          end;
        { b) convert methodpointer parameter to match objc_MsgSend* signatures }
        inserttypeconv_internal(tcallnode(left).methodpointer,objc_idtype);
      end;
    { replace self parameter }
    selfpara.left.free;
    selfpara.left:=tcallnode(left).methodpointer;
    { replace selector parameter }
    msgselpara.left.free;
    msgselpara.left:=
      cobjcselectornode.create(
       cstringconstnode.createstr(tprocdef(tcallnode(left).procdefinition).messageinf.str^)
      );
    { parameters are reused -> make sure they don't get freed }
    tcallnode(left).left:=nil;
    { methodpointer is also reused }
    tcallnode(left).methodpointer:=nil;
    { and now the call to the Objective-C rtl }
    result:=ccallnode.createinternresfromunit('OBJC1',msgsendname,newparas,left.resultdef);
    { record whether or not the function result is used (remains
      the same for the new call).
    }
    if not(cnf_return_value_used in tcallnode(left).callnodeflags) then
      exclude(tcallnode(result).callnodeflags,cnf_return_value_used);
    { in case an explicit function result was specified, keep it }
    tcallnode(result).funcretnode:=tcallnode(left).funcretnode;
    tcallnode(left).funcretnode:=nil;
    { keep variable paras }
    tcallnode(result).varargsparas:=tcallnode(left).varargsparas;
    tcallnode(left).varargsparas:=nil;

    if (cnf_inherited in tcallnode(left).callnodeflags) then
      begin
        block:=internalstatements(statements);
        { temp for the result of the inherited call }
        if not is_void(left.resultdef) and
           (cnf_return_value_used in tcallnode(left).callnodeflags) then
           begin
             tempresult:=ctempcreatenode.create(left.resultdef,left.resultdef.size,tt_persistent,true);
             addstatement(statements,tempresult);
           end;

        { make sure we return the result, if any }
        if not assigned(tempresult) then
          addstatement(statements,result)
        else
          addstatement(statements,
            cassignmentnode.create(ctemprefnode.create(tempresult),result));
        { free the objc_super temp after the call. We cannot use
          ctempdeletenode.create_normal_temp before the call, because then
          the temp will be released while evaluating the parameters, and thus
          may be reused while evaluating another parameter
        }
        addstatement(statements,ctempdeletenode.create(temp));
        if assigned(tempresult) then
          begin
            { mark the result temp as "free after next use" and return it }
            addstatement(statements,
              ctempdeletenode.create_normal_temp(tempresult));
            addstatement(statements,ctemprefnode.create(tempresult));
          end;
        typecheckpass(block);
        result:=block;
     end;
  end;

begin
  cobjcmessagesendnode:=tobjcmessagesendnode;
end.

