{
    Copyright (c) 2009-2010 by Jonas Maebe

    This unit implements some Objective-C helper routines at the node tree
    level.

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

{$i fpcdefs.inc}

unit objcutil;

interface

    uses
      node,
      symtype,symdef;

    { Check whether a string contains a syntactically valid selector name.  }
    function objcvalidselectorname(value_str: pchar; len: longint): boolean;

    { Generate a node loading the superclass structure necessary to call
      an inherited Objective-C method.  }
    function objcsuperclassnode(def: tdef): tnode;

    { Encode a method's parameters and result type into the format used by the
      run time (for generating protocol and class rtti).  }
    function objcencodemethod(pd: tprocdef): ansistring;

    { Exports all assembler symbols related to the obj-c class }
    procedure exportobjcclass(def: tobjectdef);

implementation

    uses
      globtype,
      cutils,cclasses,
      pass_1,
      verbose,systems,
      symtable,symconst,symsym,
      objcdef,
      defutil,paramgr,
      nbas,nmem,ncal,nld,ncon,ncnv,
      export;


{******************************************************************
                       validselectorname
*******************************************************************}

function objcvalidselectorname(value_str: pchar; len: longint): boolean;
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
      }
      for i:=0 to len-1 do
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

{******************************************************************
                       objcsuperclassnode
*******************************************************************}

    function objcloadbasefield(n: tnode; const fieldname: string): tnode;
      var
        vs         : tsym;
      begin
        result:=cderefnode.create(ctypeconvnode.create_internal(n,objc_idtype));
        vs:=tsym(tabstractrecorddef(objc_objecttype).symtable.Find(fieldname));
        if not assigned(vs) or
           (vs.typ<>fieldvarsym) then
          internalerror(200911301);
        result:=csubscriptnode.create(vs,result);
      end;


    function objcsuperclassnode(def: tdef): tnode;
      var
        para       : tcallparanode;
      begin
        { only valid for Objective-C classes and classrefs }
        if not is_objcclass(def) and
           not is_objcclassref(def) then
          internalerror(2009090901);
        { Can be done a lot more efficiently with direct symbol accesses, but
          requires extra node types. Maybe later. }
        if is_objcclassref(def) then
          begin
            if (oo_is_classhelper in tobjectdef(tclassrefdef(def).pointeddef).objectoptions) then
              begin
                { in case we are in a category method, we need the metaclass of the
                  superclass class extended by this category (= metaclass of superclass of superclass)
                  for the fragile abi, and the metaclass of the superclass for the non-fragile ABI }
{$if defined(onlymacosx10_6) or defined(arm) or defined(aarch64)}
                { NOTE: those send2 methods are only available on Mac OS X 10.6 and later!
                    (but also on all iPhone SDK revisions we support) }
                if (target_info.system in systems_objc_nfabi) then
                  result:=cloadvmtaddrnode.create(ctypenode.create(tobjectdef(tclassrefdef(def).pointeddef).childof))
                else
{$endif onlymacosx10_6 or arm aarch64}
                  result:=cloadvmtaddrnode.create(ctypenode.create(tobjectdef(tclassrefdef(def).pointeddef).childof.childof));
                tloadvmtaddrnode(result).forcall:=true;
                if target_info.system<>system_aarch64_darwin then
                  result:=objcloadbasefield(result,'ISA')
                else
                  result:=cloadvmtaddrnode.create(result);
                typecheckpass(result);
                { we're done }
                exit;
              end
            else
              begin
                { otherwise we need the superclass of the metaclass }
                para:=ccallparanode.create(cstringconstnode.createstr(tobjectdef(tclassrefdef(def).pointeddef).objextname^),nil);
                result:=ccallnode.createinternfromunit('OBJC','OBJC_GETMETACLASS',para);
              end
          end
        else
          begin
            if not(oo_is_classhelper in tobjectdef(def).objectoptions) then
              result:=cloadvmtaddrnode.create(ctypenode.create(def))
            else
              result:=cloadvmtaddrnode.create(ctypenode.create(tobjectdef(def).childof));
            tloadvmtaddrnode(result).forcall:=true;
          end;

{$if defined(onlymacosx10_6) or defined(arm) or defined(aarch64)}
        { For the non-fragile ABI, the superclass send2 method itself loads the
          superclass. For the fragile ABI, we have to do this ourselves.

          NOTE: those send2 methods are only available on Mac OS X 10.6 and later!
            (but also on all iPhone SDK revisions we support) }
        if not(target_info.system in systems_objc_nfabi) then
{$endif onlymacosx10_6 or arm or aarch64}
          result:=objcloadbasefield(result,'SUPERCLASS');
        typecheckpass(result);
      end;


{******************************************************************
                          Type encoding
*******************************************************************}

    function objcparasize(vs: tparavarsym): ptrint;
      begin
        result:=vs.paraloc[callerside].intsize;
        { In Objective-C, all ordinal types are widened to at least the
          size of the C "int" type. Assume __LP64__/4 byte ints for now. }
        if is_ordinal(vs.vardef) and
           (result<4) then
          result:=4;
      end;


    function objcencodemethod(pd: tprocdef): ansistring;
      var
        parasize,
        totalsize: aint;
        vs: tparavarsym;
        i: longint;
        temp: ansistring;
        founderror: tdef;
      begin
        result:='';
        totalsize:=0;
        pd.init_paraloc_info(callerside);
{$if defined(powerpc) and defined(dummy)}
        { Disabled, because neither Clang nor gcc does this, and the ObjC
          runtime contains an explicit fix to detect this error.  }

        { On ppc, the callee is responsible for removing the hidden function
          result parameter from the stack, so it has to know. On i386, it's
          the caller that does this.  }
        if (pd.returndef<>voidtype) and
            paramgr.ret_in_param(pd.returndef,pocall_cdecl) then
          inc(totalsize,sizeof(pint));
{$endif}
        for i:=0 to pd.paras.count-1 do
          begin
            vs:=tparavarsym(pd.paras[i]);
            if (vo_is_funcret in vs.varoptions) then
              continue;
            { objcaddencodedtype always assumes a value parameter, so add
              a pointer indirection for var/out parameters.  }
            if not paramanager.push_addr_param(vs_value,vs.vardef,pocall_cdecl) and
               (vs.varspez in [vs_var,vs_out,vs_constref]) then
              result:=result+'^';
            { Add the parameter type.  }
            if not objcaddencodedtype(vs.vardef,ris_initial,false,result,founderror) then
              { should be checked earlier on }
              internalerror(2009081701);
            { And the total size of the parameters coming before this one
              (i.e., the "offset" of this parameter).  }
            result:=result+tostr(totalsize);
            { Update the total parameter size }
            parasize:=objcparasize(vs);
            inc(totalsize,parasize);
          end;
        { Prepend the total parameter size.  }
        result:=tostr(totalsize)+result;
        { And the type of the function result (void in case of a procedure).  }
        temp:='';
        if not objcaddencodedtype(pd.returndef,ris_initial,false,temp,founderror) then
          internalerror(2009081801);
        result:=temp+result;
      end;


{******************************************************************
                    ObjC class exporting
*******************************************************************}

    procedure exportobjcclassfields(objccls: tobjectdef);
    var
      i: longint;
      vf: tfieldvarsym;
      prefix: string;
    begin
      prefix:=target_info.cprefix+'OBJC_IVAR_$_'+objccls.objextname^+'.';
      for i:=0 to objccls.symtable.SymList.Count-1 do
        if tsym(objccls.symtable.SymList[i]).typ=fieldvarsym then
          begin
            vf:=tfieldvarsym(objccls.symtable.SymList[i]);
            { TODO: package visibility (private_extern) -- must not be exported
               either}
            if not(vf.visibility in [vis_private,vis_strictprivate]) then
              exportname(prefix+vf.RealName,0);
          end;
    end;


    procedure exportobjcclass(def: tobjectdef);
      begin
        if (target_info.system in systems_objc_nfabi) then
          begin
            { export class and metaclass symbols }
            exportname(def.rtti_mangledname(objcclassrtti),0);
            exportname(def.rtti_mangledname(objcmetartti),0);
            { export public/protected instance variable offset symbols }
            exportobjcclassfields(def);
          end
        else
          begin
             { export the class symbol }
             exportname('.objc_class_name_'+def.objextname^,0);
          end;
      end;

end.
