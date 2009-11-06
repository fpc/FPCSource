{
    Copyright (c) 1998-2002 by Florian Klaempfl

    Type checking and register allocation for nodes that influence
    the flow

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
unit nflw;

{$i fpcdefs.inc}

interface

    uses
      cclasses,
      node,cpubase,
      symnot,
      symtype,symbase,symdef,symsym,
      optloop;

    type
       { flags used by loop nodes }
       tloopflag = (
         { set if it is a for ... downto ... do loop }
         lnf_backward,
         { Do we need to parse childs to set var state? }
         lnf_varstate,
         { Do a test at the begin of the loop?}
         lnf_testatbegin,
         { Negate the loop test? }
         lnf_checknegate,
         { Should the value of the loop variable on exit be correct. }
         lnf_dont_mind_loopvar_on_exit);
       tloopflags = set of tloopflag;

    const
         { loop flags which must match to consider loop nodes equal regarding the flags }
         loopflagsequal = [lnf_backward];

    type
       tlabelnode = class;

       tloopnode = class(tbinarynode)
          t1,t2 : tnode;
          loopflags : tloopflags;
          constructor create(tt : tnodetype;l,r,_t1,_t2 : tnode);virtual;
          destructor destroy;override;
          function dogetcopy : tnode;override;
          constructor ppuload(t:tnodetype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure buildderefimpl;override;
          procedure derefimpl;override;
          procedure insertintolist(l : tnodelist);override;
          procedure printnodetree(var t:text);override;
          function docompare(p: tnode): boolean; override;
       end;

       twhilerepeatnode = class(tloopnode)
          constructor create(l,r:Tnode;tab,cn:boolean);virtual;reintroduce;
          function pass_typecheck:tnode;override;
          function pass_1 : tnode;override;
{$ifdef state_tracking}
          function track_state_pass(exec_known:boolean):boolean;override;
{$endif}
       end;
       twhilerepeatnodeclass = class of twhilerepeatnode;

       tifnode = class(tloopnode)
          constructor create(l,r,_t1 : tnode);virtual;reintroduce;
          function pass_typecheck:tnode;override;
          function pass_1 : tnode;override;
          function simplify : tnode;override;
         private
          function internalsimplify(warn: boolean) : tnode;
       end;
       tifnodeclass = class of tifnode;

       tfornode = class(tloopnode)
          { if count isn divisable by unrolls then
            the for loop must jump to this label to get the correct
            number of executions }
          entrylabel : tnode;
          loopvar_notid:cardinal;
          constructor create(l,r,_t1,_t2 : tnode;back : boolean);virtual;reintroduce;
          procedure loop_var_access(not_type:Tnotification_flag;symbol:Tsym);
          function pass_typecheck:tnode;override;
          function pass_1 : tnode;override;
          function simplify : tnode;override;
       end;
       tfornodeclass = class of tfornode;

       texitnode = class(tunarynode)
          constructor create(l:tnode);virtual;
          constructor ppuload(t:tnodetype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          function pass_typecheck:tnode;override;
          function pass_1 : tnode;override;
       end;
       texitnodeclass = class of texitnode;

       tbreaknode = class(tnode)
          constructor create;virtual;
          function pass_typecheck:tnode;override;
          function pass_1 : tnode;override;
       end;
       tbreaknodeclass = class of tbreaknode;

       tcontinuenode = class(tnode)
          constructor create;virtual;
          function pass_typecheck:tnode;override;
          function pass_1 : tnode;override;
       end;
       tcontinuenodeclass = class of tcontinuenode;

       tgotonode = class(tnode)
       private
          labelnodeidx : longint;
       public
          labelsym : tlabelsym;
          labelnode : tlabelnode;
          exceptionblock : integer;
          constructor create(p : tlabelsym);virtual;
          constructor ppuload(t:tnodetype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure buildderefimpl;override;
          procedure derefimpl;override;
          procedure resolveppuidx;override;
          function dogetcopy : tnode;override;
          function pass_typecheck:tnode;override;
          function pass_1 : tnode;override;
          function docompare(p: tnode): boolean; override;
       end;
       tgotonodeclass = class of tgotonode;

       tlabelnode = class(tunarynode)
          exceptionblock : integer;
          { when copying trees, this points to the newly created copy of a label }
          copiedto : tlabelnode;
          labsym : tlabelsym;
          constructor create(l:tnode;alabsym:tlabelsym);virtual;
          destructor destroy;override;
          constructor ppuload(t:tnodetype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure buildderefimpl;override;
          procedure derefimpl;override;
          function dogetcopy : tnode;override;
          function pass_typecheck:tnode;override;
          function pass_1 : tnode;override;
          function docompare(p: tnode): boolean; override;
       end;
       tlabelnodeclass = class of tlabelnode;

       traisenode = class(ttertiarynode)
          constructor create(l,taddr,tframe:tnode);virtual;
          function pass_typecheck:tnode;override;
          function pass_1 : tnode;override;
       end;
       traisenodeclass = class of traisenode;

       ttryexceptnode = class(tloopnode)
          constructor create(l,r,_t1 : tnode);virtual;reintroduce;
          function pass_typecheck:tnode;override;
          function pass_1 : tnode;override;
       end;
       ttryexceptnodeclass = class of ttryexceptnode;

       ttryfinallynode = class(tloopnode)
          implicitframe : boolean;
          constructor create(l,r:tnode);virtual;reintroduce;
          constructor create_implicit(l,r,_t1:tnode);virtual;
          function pass_typecheck:tnode;override;
          function pass_1 : tnode;override;
          function simplify: tnode;override;
       end;
       ttryfinallynodeclass = class of ttryfinallynode;

       tonnode = class(tbinarynode)
          excepTSymtable : TSymtable;
          excepttype : tobjectdef;
          constructor create(l,r:tnode);virtual;
          destructor destroy;override;
          constructor ppuload(t:tnodetype;ppufile:tcompilerppufile);override;
          function pass_typecheck:tnode;override;
          function pass_1 : tnode;override;
          function dogetcopy : tnode;override;
          function docompare(p: tnode): boolean; override;
       end;
       tonnodeclass = class of tonnode;

    var
       cwhilerepeatnode : twhilerepeatnodeclass;
       cifnode : tifnodeclass;
       cfornode : tfornodeclass;
       cexitnode : texitnodeclass;
       cbreaknode : tbreaknodeclass;
       ccontinuenode : tcontinuenodeclass;
       cgotonode : tgotonodeclass;
       clabelnode : tlabelnodeclass;
       craisenode : traisenodeclass;
       ctryexceptnode : ttryexceptnodeclass;
       ctryfinallynode : ttryfinallynodeclass;
       connode : tonnodeclass;

// for-in loop helpers
function create_type_for_in_loop(hloopvar, hloopbody, expr: tnode): tnode;
function create_string_for_in_loop(hloopvar, hloopbody, expr: tnode): tnode;
function create_array_for_in_loop(hloopvar, hloopbody, expr: tnode): tnode;
function create_set_for_in_loop(hloopvar, hloopbody, expr: tnode): tnode;
function create_enumerator_for_in_loop(hloopvar, hloopbody, expr: tnode;
   enumerator_get, enumerator_move: tprocdef; enumerator_current: tpropertysym): tnode;
function create_for_in_loop(hloopvar, hloopbody, expr: tnode): tnode;

implementation

    uses
      globtype,systems,constexp,
      cutils,verbose,globals,
      symconst,symtable,paramgr,defcmp,defutil,htypechk,pass_1,
      ncal,nadd,ncon,nmem,nld,ncnv,nbas,cgobj,nutils,ninl,nset,
    {$ifdef state_tracking}
      nstate,
    {$endif}
      cgbase,procinfo
      ;


// for-in loop helpers

function create_type_for_in_loop(hloopvar, hloopbody, expr: tnode): tnode;
begin
  result:=cfornode.create(hloopvar,
       cinlinenode.create(in_low_x,false,expr.getcopy),
       cinlinenode.create(in_high_x,false,expr.getcopy),
       hloopbody,
       false);
end;

function create_string_for_in_loop(hloopvar, hloopbody, expr: tnode): tnode;
var
  loopstatement, loopbodystatement: tstatementnode;
  loopvar, stringvar: ttempcreatenode;
  stringindex, loopbody, forloopnode: tnode;
begin
  { result is a block of statements }
  result:=internalstatements(loopstatement);

  { create a temp variable for expression }
  stringvar := ctempcreatenode.create(
    expr.resultdef,
    expr.resultdef.size,
    tt_persistent,true);

  addstatement(loopstatement,stringvar);
  addstatement(loopstatement,cassignmentnode.create(ctemprefnode.create(stringvar),expr.getcopy));

  { create a loop counter: signed integer with size of string length }
  loopvar := ctempcreatenode.create(
    sinttype,
    sinttype.size,
    tt_persistent,true);

  addstatement(loopstatement,loopvar);

  stringindex:=ctemprefnode.create(loopvar);

  loopbody:=internalstatements(loopbodystatement);
  // for-in loop variable := string_expression[index]
  addstatement(loopbodystatement,
      cassignmentnode.create(hloopvar, cvecnode.create(ctemprefnode.create(stringvar),stringindex)));

  { add the actual statement to the loop }
  addstatement(loopbodystatement,hloopbody);

  forloopnode:=cfornode.create(ctemprefnode.create(loopvar),
     genintconstnode(1),
     cinlinenode.create(in_length_x,false,ctemprefnode.create(stringvar)),
     loopbody,
     false);

  addstatement(loopstatement,forloopnode);
  { free the loop counter }
  addstatement(loopstatement,ctempdeletenode.create(loopvar));
  { free the temp variable for expression }
  addstatement(loopstatement,ctempdeletenode.create(stringvar));
end;

function create_array_for_in_loop(hloopvar, hloopbody, expr: tnode): tnode;
var
  loopstatement, loopbodystatement: tstatementnode;
  loopvar, arrayvar: ttempcreatenode;
  arrayindex, lowbound, highbound, loopbody, forloopnode: tnode;
  is_string: boolean;
begin
  { result is a block of statements }
  result:=internalstatements(loopstatement);

  is_string := ado_IsConstString in tarraydef(expr.resultdef).arrayoptions;

  if (node_complexity(expr) > 1) and not is_open_array(expr.resultdef) then
  begin
    { create a temp variable for expression }
    arrayvar := ctempcreatenode.create(
      expr.resultdef,
      expr.resultdef.size,
      tt_persistent,true);

    if is_string then
    begin
      lowbound:=genintconstnode(1);
      highbound:=cinlinenode.create(in_length_x,false,ctemprefnode.create(arrayvar))
    end
    else
    begin
      lowbound:=cinlinenode.create(in_low_x,false,ctemprefnode.create(arrayvar));
      highbound:=cinlinenode.create(in_high_x,false,ctemprefnode.create(arrayvar));
    end;

    addstatement(loopstatement,arrayvar);
    addstatement(loopstatement,cassignmentnode.create(ctemprefnode.create(arrayvar),expr.getcopy));
  end
  else
  begin
    arrayvar:=nil;
    if is_string then
    begin
      lowbound:=genintconstnode(1);
      highbound:=cinlinenode.create(in_length_x,false,expr.getcopy);
    end
    else
    begin
      lowbound:=cinlinenode.create(in_low_x,false,expr.getcopy);
      highbound:=cinlinenode.create(in_high_x,false,expr.getcopy);
    end;
  end;

  { create a loop counter }
  loopvar := ctempcreatenode.create(
    tarraydef(expr.resultdef).rangedef,
    tarraydef(expr.resultdef).rangedef.size,
    tt_persistent,true);

  addstatement(loopstatement,loopvar);

  arrayindex:=ctemprefnode.create(loopvar);

  loopbody:=internalstatements(loopbodystatement);
  // for-in loop variable := array_expression[index]
  if assigned(arrayvar) then
    addstatement(loopbodystatement,
        cassignmentnode.create(hloopvar,cvecnode.create(ctemprefnode.create(arrayvar),arrayindex)))
  else
    addstatement(loopbodystatement,
        cassignmentnode.create(hloopvar,cvecnode.create(expr.getcopy,arrayindex)));

  { add the actual statement to the loop }
  addstatement(loopbodystatement,hloopbody);

  forloopnode:=cfornode.create(ctemprefnode.create(loopvar),
     lowbound,
     highbound,
     loopbody,
     false);

  addstatement(loopstatement,forloopnode);
  { free the loop counter }
  addstatement(loopstatement,ctempdeletenode.create(loopvar));
  { free the temp variable for expression if needed }
  if arrayvar<>nil then
    addstatement(loopstatement,ctempdeletenode.create(arrayvar));
end;

function create_set_for_in_loop(hloopvar, hloopbody, expr: tnode): tnode;
var
  loopstatement, loopbodystatement: tstatementnode;
  loopvar, setvar: ttempcreatenode;
  loopbody, forloopnode: tnode;
begin
  // first check is set is empty and if it so then skip other processing
  if not Assigned(tsetdef(expr.resultdef).elementdef) then
  begin
    result:=cnothingnode.create;
    // free unused nodes
    hloopvar.free;
    hloopbody.free;
    exit;
  end;
  { result is a block of statements }
  result:=internalstatements(loopstatement);

  { create a temp variable for expression }
  setvar := ctempcreatenode.create(
    expr.resultdef,
    expr.resultdef.size,
    tt_persistent,true);

  addstatement(loopstatement,setvar);
  addstatement(loopstatement,cassignmentnode.create(ctemprefnode.create(setvar),expr.getcopy));

  { create a loop counter }
  loopvar := ctempcreatenode.create(
    tsetdef(expr.resultdef).elementdef,
    tsetdef(expr.resultdef).elementdef.size,
    tt_persistent,true);

  addstatement(loopstatement,loopvar);

  // if loopvar in set then
  // begin
  //   hloopvar := loopvar
  //   for-in loop body
  // end

  loopbody:=cifnode.create(
        cinnode.create(ctemprefnode.create(loopvar),ctemprefnode.create(setvar)),
        internalstatements(loopbodystatement),
        nil
  );

  addstatement(loopbodystatement,cassignmentnode.create(hloopvar,ctemprefnode.create(loopvar)));
  { add the actual statement to the loop }
  addstatement(loopbodystatement,hloopbody);

  forloopnode:=cfornode.create(ctemprefnode.create(loopvar),
     cinlinenode.create(in_low_x,false,ctemprefnode.create(setvar)),
     cinlinenode.create(in_high_x,false,ctemprefnode.create(setvar)),
     loopbody,
     false);

  addstatement(loopstatement,forloopnode);
  { free the loop counter }
  addstatement(loopstatement,ctempdeletenode.create(loopvar));
  { free the temp variable for expression }
  addstatement(loopstatement,ctempdeletenode.create(setvar));
end;

function create_enumerator_for_in_loop(hloopvar, hloopbody, expr: tnode;
   enumerator_get, enumerator_move: tprocdef; enumerator_current: tpropertysym): tnode;
var
  loopstatement, loopbodystatement: tstatementnode;
  enumvar: ttempcreatenode;
  loopbody, whileloopnode,
  enum_get, enum_move, enum_current, enum_get_params: tnode;
  propaccesslist: tpropaccesslist;
  enumerator_is_class: boolean;
  enumerator_destructor: tprocdef;
begin
  { result is a block of statements }
  result:=internalstatements(loopstatement);

  enumerator_is_class := is_class(enumerator_get.returndef);

  { create a temp variable for enumerator }
  enumvar := ctempcreatenode.create(
    enumerator_get.returndef,
    enumerator_get.returndef.size,
    tt_persistent,true);

  addstatement(loopstatement,enumvar);

  if enumerator_get.proctypeoption=potype_operator then
  begin
    enum_get_params:=ccallparanode.create(expr.getcopy,nil);
    enum_get:=ccallnode.create(enum_get_params, tprocsym(enumerator_get.procsym), nil, nil, []);
    tcallnode(enum_get).procdefinition:=enumerator_get;
    addsymref(enumerator_get.procsym);
  end
  else
    enum_get:=ccallnode.create(nil, tprocsym(enumerator_get.procsym), enumerator_get.owner, expr.getcopy, []);

  addstatement(loopstatement,
    cassignmentnode.create(
      ctemprefnode.create(enumvar),
      enum_get
    ));

  loopbody:=internalstatements(loopbodystatement);
  { for-in loop variable := enumerator.current }
  if getpropaccesslist(enumerator_current,palt_read,propaccesslist) then
  begin
     case propaccesslist.firstsym^.sym.typ of
       fieldvarsym :
         begin
            { generate access code }
            enum_current:=ctemprefnode.create(enumvar);
            propaccesslist_to_node(enum_current,enumerator_current.owner,propaccesslist);
            include(enum_current.flags,nf_isproperty);
         end;
       procsym :
         begin
            { generate the method call }
            enum_current:=ccallnode.create(nil,tprocsym(propaccesslist.firstsym^.sym),enumerator_current.owner,ctemprefnode.create(enumvar),[]);
            include(enum_current.flags,nf_isproperty);
         end
       else
         begin
            enum_current:=cerrornode.create;
            Message(type_e_mismatch);
         end;
    end;
  end
  else
    enum_current:=cerrornode.create;

  addstatement(loopbodystatement,
      cassignmentnode.create(hloopvar, enum_current));

  { add the actual statement to the loop }
  addstatement(loopbodystatement,hloopbody);

  enum_move:=ccallnode.create(nil, tprocsym(enumerator_move.procsym), enumerator_move.owner, ctemprefnode.create(enumvar), []);
  whileloopnode:=cwhilerepeatnode.create(enum_move,loopbody,true,false);

  if enumerator_is_class then
  begin
    { insert a try-finally and call the destructor for the enumerator in the finally section }
    enumerator_destructor:=tobjectdef(enumerator_get.returndef).Finddestructor;
    if assigned(enumerator_destructor) then
    begin
      whileloopnode:=ctryfinallynode.create(
        whileloopnode, // try node
        ccallnode.create(nil,tprocsym(enumerator_destructor.procsym), // finally node
          enumerator_destructor.procsym.owner,ctemprefnode.create(enumvar),[]));
    end;
    { if getenumerator <> nil then do the loop }
    whileloopnode:=cifnode.create(
      caddnode.create(unequaln, ctemprefnode.create(enumvar), cnilnode.create),
      whileloopnode,
      nil
      );
  end;

  addstatement(loopstatement, whileloopnode);

  if is_object(enumerator_get.returndef) then
  begin
    // call the object destructor too
    enumerator_destructor:=tobjectdef(enumerator_get.returndef).Finddestructor;
    if assigned(enumerator_destructor) then
    begin
      addstatement(loopstatement,
        ccallnode.create(nil,tprocsym(enumerator_destructor.procsym),
            enumerator_destructor.procsym.owner,ctemprefnode.create(enumvar),[]));
    end;
  end;

  { free the temp variable for enumerator }
  addstatement(loopstatement,ctempdeletenode.create(enumvar));
end;

function create_for_in_loop(hloopvar, hloopbody, expr: tnode): tnode;
var
  pd, movenext: tprocdef;
  current: tpropertysym;
  storefilepos: tfileposinfo;
begin
  storefilepos:=current_filepos;
  current_filepos:=hloopvar.fileinfo;
  if expr.nodetype=typen then
  begin
    if (expr.resultdef.typ=enumdef) and tenumdef(expr.resultdef).has_jumps then
    begin
      result:=cerrornode.create;
      hloopvar.free;
      hloopbody.free;
      Message1(parser_e_for_in_loop_cannot_be_used_for_the_type,expr.resultdef.typename);
    end
    else 
      result:=create_type_for_in_loop(hloopvar, hloopbody, expr);
  end
  else
  begin
    { loop is made for an expression }
    // search for operator first
    pd:=search_enumerator_operator(expr.resultdef);
    // if there is no operator then search for class/object enumerator method
    if (pd=nil) and (expr.resultdef.typ=objectdef) then
      pd:=tobjectdef(expr.resultdef).search_enumerator_get;
    if pd<>nil then
    begin
      // seach movenext and current symbols
      movenext:=tobjectdef(pd.returndef).search_enumerator_move;
      if movenext = nil then
      begin
        result:=cerrornode.create;
        hloopvar.free;
        hloopbody.free;
        Message1(sym_e_no_enumerator_move,pd.returndef.GetTypeName);
      end
      else
      begin
        current:=tpropertysym(tobjectdef(pd.returndef).search_enumerator_current);
        if current = nil then
        begin
          result:=cerrornode.create;
          hloopvar.free;
          hloopbody.free;
          Message1(sym_e_no_enumerator_current,pd.returndef.GetTypeName);
        end
        else
          result:=create_enumerator_for_in_loop(hloopvar, hloopbody, expr, pd, movenext, current);
      end;
    end
    else
    begin
      case expr.resultdef.typ of
        stringdef: result:=create_string_for_in_loop(hloopvar, hloopbody, expr);
        arraydef: result:=create_array_for_in_loop(hloopvar, hloopbody, expr);
        setdef: result:=create_set_for_in_loop(hloopvar, hloopbody, expr);
      else
        begin
          result:=cerrornode.create;
          hloopvar.free;
          hloopbody.free;
          Message1(sym_e_no_enumerator,expr.resultdef.GetTypeName);
        end;
      end;
    end;
  end;
  current_filepos:=storefilepos;
end;

{****************************************************************************
                                 TLOOPNODE
*****************************************************************************}

    constructor tloopnode.create(tt : tnodetype;l,r,_t1,_t2 : tnode);

      begin
         inherited create(tt,l,r);
         t1:=_t1;
         t2:=_t2;
         fileinfo:=l.fileinfo;
      end;

    destructor tloopnode.destroy;

      begin
         t1.free;
         t2.free;
         inherited destroy;
      end;


    constructor tloopnode.ppuload(t:tnodetype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
        t1:=ppuloadnode(ppufile);
        t2:=ppuloadnode(ppufile);
      end;


    procedure tloopnode.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        ppuwritenode(ppufile,t1);
        ppuwritenode(ppufile,t2);
      end;


    procedure tloopnode.buildderefimpl;
      begin
        inherited buildderefimpl;
        if assigned(t1) then
          t1.buildderefimpl;
        if assigned(t2) then
          t2.buildderefimpl;
      end;


    procedure tloopnode.derefimpl;
      begin
        inherited derefimpl;
        if assigned(t1) then
          t1.derefimpl;
        if assigned(t2) then
          t2.derefimpl;
      end;


    function tloopnode.dogetcopy : tnode;

      var
         p : tloopnode;

      begin
         p:=tloopnode(inherited dogetcopy);
         if assigned(t1) then
           p.t1:=t1.dogetcopy
         else
           p.t1:=nil;
         if assigned(t2) then
           p.t2:=t2.dogetcopy
         else
           p.t2:=nil;
         p.loopflags:=loopflags;
         dogetcopy:=p;
      end;

    procedure tloopnode.insertintolist(l : tnodelist);

      begin
      end;


    procedure tloopnode.printnodetree(var t:text);
      begin
        write(t,printnodeindention,'(');
        printnodeindent;
        printnodeinfo(t);
        writeln(t);
        printnode(t,left);
        printnode(t,right);
        printnode(t,t1);
        printnode(t,t2);
        printnodeunindent;
        writeln(t,printnodeindention,')');
      end;


    function tloopnode.docompare(p: tnode): boolean;
      begin
        docompare :=
          inherited docompare(p) and
          (loopflags*loopflagsequal=tloopnode(p).loopflags*loopflagsequal) and
          t1.isequal(tloopnode(p).t1) and
          t2.isequal(tloopnode(p).t2);
      end;

{****************************************************************************
                               TWHILEREPEATNODE
*****************************************************************************}

    constructor Twhilerepeatnode.create(l,r:Tnode;tab,cn:boolean);
      begin
          inherited create(whilerepeatn,l,r,nil,nil);
          if tab then
              include(loopflags, lnf_testatbegin);
          if cn then
              include(loopflags,lnf_checknegate);
      end;

    function twhilerepeatnode.pass_typecheck:tnode;
      var
         t:Tunarynode;
      begin
         result:=nil;
         resultdef:=voidtype;

         typecheckpass(left);

         { tp procvar support }
         maybe_call_procvar(left,true);

         {A not node can be removed.}
         if left.nodetype=notn then
           begin
             t:=Tunarynode(left);
             left:=Tunarynode(left).left;
             t.left:=nil;
             t.destroy;
             {Symdif operator, in case you are wondering:}
             loopflags:=loopflags >< [lnf_checknegate];
           end;
         { loop instruction }
         if assigned(right) then
           typecheckpass(right);
         set_varstate(left,vs_read,[vsf_must_be_valid]);
         if codegenerror then
           exit;

         if not is_boolean(left.resultdef) then
           begin
             if left.resultdef.typ=variantdef then
               inserttypeconv(left,booltype)
             else
               CGMessage1(type_e_boolean_expr_expected,left.resultdef.typename);
           end;

         { Give warnings for code that will never be executed for
           while false do }
         if (lnf_testatbegin in loopflags) and
            (left.nodetype=ordconstn) and
            (tordconstnode(left).value.uvalue=0) and
            assigned(right) then
           CGMessagePos(right.fileinfo,cg_w_unreachable_code);
      end;


{$ifdef prefetchnext}
    type
      passignmentquery = ^tassignmentquery;
      tassignmentquery = record
        towhat: tnode;
        source: tassignmentnode;
        statementcount: cardinal;
      end;

    function checkassignment(var n: tnode; arg: pointer): foreachnoderesult;
      var
        query: passignmentquery absolute arg;
        temp, prederef: tnode;
      begin
        result := fen_norecurse_false;
        if (n.nodetype in [assignn,inlinen,forn,calln,whilerepeatn,casen,ifn]) then
          inc(query^.statementcount);
        { make sure there's something else in the loop besides going to the }
        { next item                                                         }
        if (query^.statementcount > 1) and
           (n.nodetype = assignn) then
          begin
            { skip type conversions of assignment target }
            temp := tassignmentnode(n).left;
            while (temp.nodetype = typeconvn) do
              temp := ttypeconvnode(temp).left;

            { assignment to x of the while assigned(x) check? }
            if not(temp.isequal(query^.towhat)) then
              exit;

            { right hand side of assignment dereferenced field of }
            { x? (no derefn in case of class)                     }
            temp := tassignmentnode(n).right;
            while (temp.nodetype = typeconvn) do
              temp := ttypeconvnode(temp).left;
            if (temp.nodetype <> subscriptn) then
              exit;

            prederef := tsubscriptnode(temp).left;
            temp := prederef;
            while (temp.nodetype = typeconvn) do
              temp := ttypeconvnode(temp).left;

            { see tests/test/prefetch1.pp }
            if (temp.nodetype = derefn) then
              temp := tderefnode(temp).left
            else
              temp := prederef;

            if temp.isequal(query^.towhat) then
              begin
                query^.source := tassignmentnode(n);
                result := fen_norecurse_true;
               end
          end
        { don't check nodes which can't contain an assignment or whose }
        { final assignment can vary a lot                              }
        else if not(n.nodetype in [calln,inlinen,casen,whilerepeatn,forn]) then
          result := fen_false;
      end;


    function findassignment(where: tnode; towhat: tnode): tassignmentnode;
      var
        query: tassignmentquery;
      begin
        query.towhat := towhat;
        query.source := nil;
        query.statementcount := 0;
        if foreachnodestatic(where,@checkassignment,@query) then
          result := query.source
        else
           result := nil;
      end;
{$endif prefetchnext}


    function twhilerepeatnode.pass_1 : tnode;
{$ifdef prefetchnext}
      var
         runnernode, prefetchcode: tnode;
         assignmentnode: tassignmentnode;
         prefetchstatements: tstatementnode;
{$endif prefetchnext}
      begin
         result:=nil;
         expectloc:=LOC_VOID;

         firstpass(left);
         if codegenerror then
           exit;

         { loop instruction }
         if assigned(right) then
           begin
              firstpass(right);
              if codegenerror then
                exit;
           end;

{$ifdef prefetchnext}
         { do at the end so all complex typeconversions are already }
         { converted to calln's                                     }
         if (cs_opt_level1 in current_settings.optimizerswitches) and
            (lnf_testatbegin in loopflags) then
           begin
             { get first component of the while check }
             runnernode := left;
             while (runnernode.nodetype in [andn,orn,notn,xorn,typeconvn]) do
               runnernode := tunarynode(runnernode).left;
             { is it an assigned(x) check? }
             if ((runnernode.nodetype = inlinen) and
                 (tinlinenode(runnernode).inlinenumber = in_assigned_x)) or
                ((runnernode.nodetype = unequaln) and
                 (taddnode(runnernode).right.nodetype = niln)) then
               begin
                 runnernode := tunarynode(runnernode).left;
                 { in case of in_assigned_x, there's a callparan in between }
                 if (runnernode.nodetype = callparan) then
                   runnernode := tcallparanode(runnernode).left;
                 while (runnernode.nodetype = typeconvn) do
                   runnernode := ttypeconvnode(runnernode).left;
                 { is there an "x := x(^).somefield"? }
                 assignmentnode := findassignment(right,runnernode);
                 if assigned(assignmentnode) then
                   begin
                     prefetchcode := internalstatements(prefetchstatements);
                     addstatement(prefetchstatements,geninlinenode(in_prefetch_var,false,
                       cderefnode.create(ctypeconvnode.create(assignmentnode.right.getcopy,voidpointertype))));
                     addstatement(prefetchstatements,right);
                     right := prefetchcode;
                     typecheckpass(right);
                   end;
               end;
           end;
{$endif prefetchnext}
      end;

{$ifdef state_tracking}
    function Twhilerepeatnode.track_state_pass(exec_known:boolean):boolean;

    var condition:Tnode;
        code:Tnode;
        done:boolean;
        value:boolean;
        change:boolean;
        firsttest:boolean;
        factval:Tnode;

    begin
        track_state_pass:=false;
        done:=false;
        firsttest:=true;
        {For repeat until statements, first do a pass through the code.}
        if not(lnf_testatbegin in flags) then
            begin
                code:=right.getcopy;
                if code.track_state_pass(exec_known) then
                    track_state_pass:=true;
                code.destroy;
            end;
        repeat
            condition:=left.getcopy;
            code:=right.getcopy;
            change:=condition.track_state_pass(exec_known);
            factval:=aktstate.find_fact(left);
            if factval<>nil then
                begin
                    condition.destroy;
                    condition:=factval.getcopy;
                    change:=true;
                end;
            if change then
                begin
                    track_state_pass:=true;
                    {Force new resultdef pass.}
                    condition.resultdef:=nil;
                    do_typecheckpass(condition);
                end;
            if is_constboolnode(condition) then
                begin
                    {Try to turn a while loop into a repeat loop.}
                    if firsttest then
                        exclude(flags,testatbegin);
                    value:=(Tordconstnode(condition).value<>0) xor checknegate;
                    if value then
                        begin
                            if code.track_state_pass(exec_known) then
                                track_state_pass:=true;
                        end
                    else
                        done:=true;
                end
            else
                begin
                    {Remove any modified variables from the state.}
                    code.track_state_pass(false);
                    done:=true;
                end;
            code.destroy;
            condition.destroy;
            firsttest:=false;
        until done;
        {The loop condition is also known, for example:
         while i<10 do
            begin
                ...
            end;

         When the loop is done, we do know that i<10 = false.
        }
        condition:=left.getcopy;
        if condition.track_state_pass(exec_known) then
            begin
                track_state_pass:=true;
                {Force new resultdef pass.}
                condition.resultdef:=nil;
                do_typecheckpass(condition);
            end;
        if not is_constboolnode(condition) then
            aktstate.store_fact(condition,
             cordconstnode.create(byte(checknegate),booltype,true))
        else
            condition.destroy;
    end;
{$endif}

{*****************************************************************************
                               TIFNODE
*****************************************************************************}

    constructor tifnode.create(l,r,_t1 : tnode);
      begin
         inherited create(ifn,l,r,_t1,nil);
      end;


    function tifnode.internalsimplify(warn: boolean) : tnode;
      begin
        result:=nil;
        { optimize constant expressions }
        if (left.nodetype=ordconstn) then
          begin
             if tordconstnode(left).value.uvalue=1 then
               begin
                  if assigned(right) then
                    result:=right
                  else
                    result:=cnothingnode.create;
                  right:=nil;
                  if warn and assigned(t1) then
                    CGMessagePos(t1.fileinfo,cg_w_unreachable_code);
               end
             else
               begin
                  if assigned(t1) then
                    result:=t1
                  else
                    result:=cnothingnode.create;
                  t1:=nil;
                  if warn and assigned(right) then
                    CGMessagePos(right.fileinfo,cg_w_unreachable_code);
               end;
          end;
      end;


    function tifnode.simplify : tnode;
      begin
        result:=internalsimplify(false);
      end;


    function tifnode.pass_typecheck:tnode;
      begin
         result:=nil;
         resultdef:=voidtype;

         typecheckpass(left);

         { tp procvar support }
         maybe_call_procvar(left,true);

         { if path }
         if assigned(right) then
           typecheckpass(right);
         { else path }
         if assigned(t1) then
           typecheckpass(t1);
         set_varstate(left,vs_read,[vsf_must_be_valid]);
         if codegenerror then
           exit;

         if not is_boolean(left.resultdef) then
           begin
             if left.resultdef.typ=variantdef then
               inserttypeconv(left,booltype)
             else
               Message1(type_e_boolean_expr_expected,left.resultdef.typename);
           end;
         result:=internalsimplify(true);
      end;


    function tifnode.pass_1 : tnode;
      begin
         result:=nil;
         expectloc:=LOC_VOID;
         firstpass(left);

         { if path }
         if assigned(right) then
           firstpass(right);

         { else path }
         if assigned(t1) then
           firstpass(t1);

         { leave if we've got an error in one of the paths }

         if codegenerror then
           exit;
      end;


{*****************************************************************************
                              TFORNODE
*****************************************************************************}

    constructor tfornode.create(l,r,_t1,_t2 : tnode;back : boolean);

      begin
         inherited create(forn,l,r,_t1,_t2);
         if back then
           include(loopflags,lnf_backward);
         include(loopflags,lnf_testatbegin);
      end;

    procedure Tfornode.loop_var_access(not_type:Tnotification_flag;
                                       symbol:Tsym);

    begin
      {If there is a read access, the value of the loop counter is important;
       at the end of the loop the loop variable should contain the value it
       had in the last iteration.}
      if not_type=vn_onwrite then
        begin
          writeln('Loopvar does not matter on exit');
        end
      else
        begin
          exclude(loopflags,lnf_dont_mind_loopvar_on_exit);
          writeln('Loopvar does matter on exit');
        end;
      Tabstractvarsym(symbol).unregister_notification(loopvar_notid);
    end;


    function tfornode.simplify : tnode;
      begin
        result:=nil;
        if (t1.nodetype=ordconstn) and
           (right.nodetype=ordconstn) and
           (
            (
             (lnf_backward in loopflags) and
             (tordconstnode(right).value<tordconstnode(t1).value)
            ) or
            (
              not(lnf_backward in loopflags) and
              (tordconstnode(right).value>tordconstnode(t1).value)
            )
           ) then
        result:=cnothingnode.create;
      end;


    function tfornode.pass_typecheck:tnode;
      var
        res : tnode;
      begin
         result:=nil;
         resultdef:=voidtype;

         { process the loopvar, from and to, varstates are already set }
         typecheckpass(left);
         typecheckpass(right);
         typecheckpass(t1);

         set_varstate(left,vs_written,[]);

         { loop unrolling }
         if cs_opt_loopunroll in current_settings.optimizerswitches then
           begin
             res:=unroll_loop(self);
             if assigned(res) then
               begin
                 typecheckpass(res);
                 result:=res;
                 exit;
               end;
           end;

         { Can we spare the first comparision? }
         if (t1.nodetype=ordconstn) and
            (right.nodetype=ordconstn) and
            (
             (
              (lnf_backward in loopflags) and
              (Tordconstnode(right).value>=Tordconstnode(t1).value)
             ) or
             (
               not(lnf_backward in loopflags) and
               (Tordconstnode(right).value<=Tordconstnode(t1).value)
             )
            ) then
           exclude(loopflags,lnf_testatbegin);

         { Make sure that the loop var and the
           from and to values are compatible types }
         check_ranges(right.fileinfo,right,left.resultdef);
         inserttypeconv(right,left.resultdef);

         check_ranges(t1.fileinfo,t1,left.resultdef);
         inserttypeconv(t1,left.resultdef);

         if assigned(t2) then
           typecheckpass(t2);
      end;


    function tfornode.pass_1 : tnode;
      begin
         result:=nil;
         expectloc:=LOC_VOID;

         firstpass(left);
         firstpass(right);
         firstpass(t1);

         if assigned(t2) then
          begin
            firstpass(t2);
            if codegenerror then
             exit;
          end;
      end;


{*****************************************************************************
                             TEXITNODE
*****************************************************************************}

    constructor texitnode.create(l:tnode);
      begin
        inherited create(exitn,l);
      end;


    constructor texitnode.ppuload(t:tnodetype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
      end;


    procedure texitnode.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
      end;


    function texitnode.pass_typecheck:tnode;
      begin
        result:=nil;
        if assigned(left) then
          begin
            { add assignment to funcretsym }
            inserttypeconv(left,current_procinfo.procdef.returndef);
            left:=cassignmentnode.create(
                cloadnode.create(current_procinfo.procdef.funcretsym,current_procinfo.procdef.funcretsym.owner),
                left);
            typecheckpass(left);
            set_varstate(left,vs_read,[vsf_must_be_valid]);
          end;
        resultdef:=voidtype;
      end;


    function texitnode.pass_1 : tnode;
      begin
         result:=nil;
         expectloc:=LOC_VOID;
         if assigned(left) then
           begin
              firstpass(left);
              if codegenerror then
               exit;
           end;
      end;


{*****************************************************************************
                             TBREAKNODE
*****************************************************************************}

    constructor tbreaknode.create;

      begin
        inherited create(breakn);
      end;


    function tbreaknode.pass_typecheck:tnode;
      begin
        result:=nil;
        resultdef:=voidtype;
      end;


    function tbreaknode.pass_1 : tnode;
      begin
        result:=nil;
        expectloc:=LOC_VOID;
      end;


{*****************************************************************************
                             TCONTINUENODE
*****************************************************************************}

    constructor tcontinuenode.create;
      begin
        inherited create(continuen);
      end;


    function tcontinuenode.pass_typecheck:tnode;
      begin
        result:=nil;
        resultdef:=voidtype;
      end;


    function tcontinuenode.pass_1 : tnode;
      begin
        result:=nil;
        expectloc:=LOC_VOID;
      end;


{*****************************************************************************
                             TGOTONODE
*****************************************************************************}

    constructor tgotonode.create(p : tlabelsym);
      begin
        inherited create(goton);
        exceptionblock:=current_exceptblock;
        labelnode:=nil;
        labelsym:=p;
      end;


    constructor tgotonode.ppuload(t:tnodetype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
        labelnodeidx:=ppufile.getlongint;
        exceptionblock:=ppufile.getbyte;
      end;


    procedure tgotonode.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        labelnodeidx:=labelnode.ppuidx;
        ppufile.putlongint(labelnodeidx);
        ppufile.putbyte(exceptionblock);
      end;


    procedure tgotonode.buildderefimpl;
      begin
        inherited buildderefimpl;
      end;


    procedure tgotonode.derefimpl;
      begin
        inherited derefimpl;
      end;


    procedure tgotonode.resolveppuidx;
      begin
        labelnode:=tlabelnode(nodeppuidxget(labelnodeidx));
        if labelnode.nodetype<>labeln then
          internalerror(200809021);
      end;


    function tgotonode.pass_typecheck:tnode;
      begin
        result:=nil;
        resultdef:=voidtype;
      end;


    function tgotonode.pass_1 : tnode;
      begin
        result:=nil;
        expectloc:=LOC_VOID;
        include(current_procinfo.flags,pi_has_goto);

        { The labelnode can already be set when
          this node was copied }
        if not assigned(labelnode) then
          begin
            if assigned(labelsym.code) then
              labelnode:=tlabelnode(labelsym.code)
            else
              CGMessage1(cg_e_goto_label_not_found,labelsym.realname);
          end;

        { check if we don't mess with exception blocks }
        if assigned(labelnode) and
           (exceptionblock<>labelnode.exceptionblock) then
          CGMessage(cg_e_goto_inout_of_exception_block);
      end;


   function tgotonode.dogetcopy : tnode;
     var
       p : tgotonode;
     begin
        p:=tgotonode(inherited dogetcopy);
        p.exceptionblock:=exceptionblock;

        { generate labelnode if not done yet }
        if not(assigned(labelnode)) then
          begin
            if assigned(labelsym) and assigned(labelsym.code) then
              labelnode:=tlabelnode(labelsym.code)
          end;

        p.labelsym:=labelsym;
        if assigned(labelnode) then
          p.labelnode:=tlabelnode(labelnode.dogetcopy)
        else
          begin
            { don't trigger IE when there was already an error, i.e. the
              label is not defined. See tw11763 (PFV) }
            if errorcount=0 then
              internalerror(200610291);
          end;
        result:=p;
     end;


    function tgotonode.docompare(p: tnode): boolean;
      begin
        docompare := false;
      end;


{*****************************************************************************
                             TLABELNODE
*****************************************************************************}

    constructor tlabelnode.create(l:tnode;alabsym:tlabelsym);
      begin
        inherited create(labeln,l);
        exceptionblock:=current_exceptblock;
        labsym:=alabsym;
        { Register labelnode in labelsym }
        labsym.code:=self;
      end;


    constructor tlabelnode.ppuload(t:tnodetype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
        exceptionblock:=ppufile.getbyte;
      end;


    destructor tlabelnode.destroy;
      begin
        { Remove reference in labelsym, this is to prevent
          goto's to this label }
        if assigned(labsym) and (labsym.code=pointer(self)) then
          labsym.code:=nil;
        inherited destroy;
      end;


    procedure tlabelnode.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        ppufile.putbyte(exceptionblock);
      end;


    procedure tlabelnode.buildderefimpl;
      begin
        inherited buildderefimpl;
      end;


    procedure tlabelnode.derefimpl;
      begin
        inherited derefimpl;
      end;


    function tlabelnode.pass_typecheck:tnode;
      begin
        result:=nil;
        { left could still be unassigned }
        if assigned(left) then
         typecheckpass(left);
        resultdef:=voidtype;
      end;


    function tlabelnode.pass_1 : tnode;
      begin
         result:=nil;
         expectloc:=LOC_VOID;
         if assigned(left) then
           firstpass(left);
      end;


   function tlabelnode.dogetcopy : tnode;
     begin
        if not(assigned(copiedto)) then
          copiedto:=tlabelnode(inherited dogetcopy);
        copiedto.exceptionblock:=exceptionblock;

        result:=copiedto;
     end;


    function tlabelnode.docompare(p: tnode): boolean;
      begin
        docompare := false;
      end;


{*****************************************************************************
                            TRAISENODE
*****************************************************************************}

    constructor traisenode.create(l,taddr,tframe:tnode);
      begin
         inherited create(raisen,l,taddr,tframe);
      end;


    function traisenode.pass_typecheck:tnode;
      begin
         result:=nil;
         resultdef:=voidtype;
         if assigned(left) then
           begin
             { first para must be a _class_ }
             typecheckpass(left);
             set_varstate(left,vs_read,[vsf_must_be_valid]);
             if codegenerror then
              exit;
             if not(is_class(left.resultdef)) then
               CGMessage1(type_e_class_type_expected,left.resultdef.typename);
             { insert needed typeconvs for addr,frame }
             if assigned(right) then
               begin
                 { addr }
                 typecheckpass(right);
                 inserttypeconv(right,voidpointertype);
                 { frame }
                 if assigned(third) then
                  begin
                    typecheckpass(third);
                    inserttypeconv(third,voidpointertype);
                  end;
               end;
           end;
      end;


    function traisenode.pass_1 : tnode;
      begin
         result:=nil;
         include(current_procinfo.flags,pi_do_call);
         expectloc:=LOC_VOID;
         if assigned(left) then
           begin
              { first para must be a _class_ }
              firstpass(left);
              { insert needed typeconvs for addr,frame }
              if assigned(right) then
               begin
                 { addr }
                 firstpass(right);
                 { frame }
                 if assigned(third) then
                  firstpass(third);
               end;
           end;
      end;


{*****************************************************************************
                             TTRYEXCEPTNODE
*****************************************************************************}

    constructor ttryexceptnode.create(l,r,_t1 : tnode);
      begin
         inherited create(tryexceptn,l,r,_t1,nil);
      end;


    function ttryexceptnode.pass_typecheck:tnode;
      begin
         result:=nil;
         typecheckpass(left);
         { on statements }
         if assigned(right) then
           typecheckpass(right);
         { else block }
         if assigned(t1) then
           typecheckpass(t1);
         resultdef:=voidtype;
      end;


    function ttryexceptnode.pass_1 : tnode;
      begin
         result:=nil;
         include(current_procinfo.flags,pi_do_call);
         expectloc:=LOC_VOID;
         firstpass(left);
         { on statements }
         if assigned(right) then
           firstpass(right);
         { else block }
         if assigned(t1) then
           firstpass(t1);
      end;


{*****************************************************************************
                           TTRYFINALLYNODE
*****************************************************************************}

    constructor ttryfinallynode.create(l,r:tnode);
      begin
        inherited create(tryfinallyn,l,r,nil,nil);
        implicitframe:=false;
      end;


    constructor ttryfinallynode.create_implicit(l,r,_t1:tnode);
      begin
        inherited create(tryfinallyn,l,r,_t1,nil);
        implicitframe:=true;
      end;


    function ttryfinallynode.pass_typecheck:tnode;
      begin
         result:=nil;
         include(current_procinfo.flags,pi_do_call);
         resultdef:=voidtype;

         typecheckpass(left);
         // "try block" is "used"? (JM)
         set_varstate(left,vs_readwritten,[vsf_must_be_valid]);

         typecheckpass(right);
         // "except block" is "used"? (JM)
         set_varstate(right,vs_readwritten,[vsf_must_be_valid]);

         { special finally block only executed when there was an exception }
         if assigned(t1) then
           begin
             typecheckpass(t1);
             // "finally block" is "used"? (JM)
             set_varstate(t1,vs_readwritten,[vsf_must_be_valid]);
           end;
      end;


    function ttryfinallynode.pass_1 : tnode;
      begin
         result:=nil;
         expectloc:=LOC_VOID;
         firstpass(left);

         firstpass(right);

         if assigned(t1) then
           firstpass(t1);
      end;


   function ttryfinallynode.simplify: tnode;
     begin
       result:=nil;
       { if the try contains no code, we can kill
         the try and except and return only the
         finally part }
       if has_no_code(left) then
         begin
           result:=right;
           right:=nil;
         end;
     end;


{*****************************************************************************
                                TONNODE
*****************************************************************************}

    constructor tonnode.create(l,r:tnode);
      begin
         inherited create(onn,l,r);
         excepTSymtable:=nil;
         excepttype:=nil;
      end;


    destructor tonnode.destroy;
      begin
        { copied nodes don't need to release the symtable }
        if assigned(excepTSymtable) then
         excepTSymtable.free;
        inherited destroy;
      end;


    constructor tonnode.ppuload(t:tnodetype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
        excepTSymtable:=nil;
        excepttype:=nil;
      end;


    function tonnode.dogetcopy : tnode;
      var
         n : tonnode;
      begin
         n:=tonnode(inherited dogetcopy);
         if assigned(exceptsymtable) then
           n.exceptsymtable:=exceptsymtable.getcopy
         else
           n.exceptsymtable:=nil;
         n.excepttype:=excepttype;
         result:=n;
      end;


    function tonnode.pass_typecheck:tnode;
      begin
         result:=nil;
         resultdef:=voidtype;
         if not(is_class(excepttype)) then
           CGMessage1(type_e_class_type_expected,excepttype.typename);
         if assigned(left) then
           typecheckpass(left);
         if assigned(right) then
           typecheckpass(right);
      end;


    function tonnode.pass_1 : tnode;
      begin
         result:=nil;
         include(current_procinfo.flags,pi_do_call);
         expectloc:=LOC_VOID;
         if assigned(left) then
           firstpass(left);

         if assigned(right) then
           firstpass(right);
      end;


    function tonnode.docompare(p: tnode): boolean;
      begin
        docompare := false;
      end;


begin
   cwhilerepeatnode:=twhilerepeatnode;
   cifnode:=tifnode;
   cfornode:=tfornode;
   cexitnode:=texitnode;
   cgotonode:=tgotonode;
   clabelnode:=tlabelnode;
   craisenode:=traisenode;
   ctryexceptnode:=ttryexceptnode;
   ctryfinallynode:=ttryfinallynode;
   connode:=tonnode;
end.
