{
    Copyright (c) 2008 by Jonas Maebe

    Virtual methods optimizations (devirtualization)

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
unit optvirt;

{$i fpcdefs.inc}

  interface

    uses
      globtype,
      cclasses,
      symtype,symdef,
      wpobase;

    type
       { node in an inheritance tree, contains a link to the parent type (if any) and to all
        child types
      }
      tinheritancetreenode = class
       private
        fdef: tobjectdef;
        fparent: tinheritancetreenode;
        fchilds: tfpobjectlist;
        fcalledvmtmethods: tbitset;
        finstantiated: boolean;

        function getchild(index: longint): tinheritancetreenode;
       public
        constructor create(_parent: tinheritancetreenode; _def: tobjectdef; _instantiated: boolean);
        { destroys both this node and all of its siblings }
        destructor destroy; override;
        function  childcount: longint;
        function  haschilds: boolean;
        property  childs[index: longint]: tinheritancetreenode read getchild;
        property  parent: tinheritancetreenode read fparent;
        property  def: tobjectdef read fdef;
        property  instantiated: boolean read finstantiated write finstantiated;
        { if def is not yet a child of this node, add it. In all cases, return node containing
          this def (either new or existing one
        }
        function  maybeaddchild(_def: tobjectdef; _instantiated: boolean): tinheritancetreenode;
        function  findchild(_def: tobjectdef): tinheritancetreenode;
      end;


      tinheritancetreecallback = procedure(node: tinheritancetreenode; arg: pointer) of object;

      tinheritancetree = class
       private
        { just a regular node with parent = nil }
        froots: tinheritancetreenode;

        classrefdefs: tfpobjectlist;

        procedure foreachnodefromroot(root: tinheritancetreenode; proctocall: tinheritancetreecallback; arg: pointer);
        function registerinstantiatedobjectdefrecursive(def: tobjectdef; instantiated: boolean): tinheritancetreenode;
        procedure markvmethods(node: tinheritancetreenode; p: pointer);
        procedure printobjectvmtinfo(node: tinheritancetreenode; arg: pointer);
        procedure addcalledvmtentries(node: tinheritancetreenode; arg: pointer);

        function  getnodefordef(def: tobjectdef): tinheritancetreenode;
       public
        constructor create;
        destructor destroy; override;
        { adds an objectdef (the def itself, and all of its parents that do not yet exist) to
          the tree, and returns the leaf node
        }
        procedure registerinstantiatedobjdef(def: tdef);
        procedure registerinstantiatedclassrefdef(def: tdef);
        procedure registercalledvmtentries(entries: tcalledvmtentries);
        procedure checkforclassrefinheritance(def: tdef);
        procedure foreachnode(proctocall: tinheritancetreecallback; arg: pointer);
        procedure foreachleafnode(proctocall: tinheritancetreecallback; arg: pointer);
        procedure optimizevirtualmethods;
        procedure printvmtinfo;
      end;


      { devirtualisation information for a class }

      tclassdevirtinfo = class(tfphashobject)
       private
        { array (indexed by vmt entry nr) of replacement statically callable method names }
        fstaticmethodnames: tfplist;
        { is this class instantiated by the program? }
        finstantiated: boolean;
        function isstaticvmtentry(vmtindex: longint; out replacementname: pshortstring): boolean;
       public
        constructor create(hashobjectlist:tfphashobjectlist;const n: shortstring; instantiated: boolean);
        destructor destroy; override;

        property instantiated: boolean read finstantiated;

        procedure addstaticmethod(vmtindex: longint; const replacementname: shortstring);
      end;


      { devirtualisation information for all classes in a unit }

      tunitdevirtinfo = class(tfphashobject)
       private
        { hashtable of classes }
        fclasses: tfphashobjectlist;
       public
        constructor create(hashobjectlist:tfphashobjectlist;const n: shortstring);reintroduce;
        destructor destroy; override;

        function addclass(const n: shortstring; instantiated: boolean): tclassdevirtinfo;
        function findclass(const n: shortstring): tclassdevirtinfo;
      end;

      { devirtualisation information for all units in a program }

      { tprogdevirtinfo }

      tprogdevirtinfo = class(twpodevirtualisationhandler)
       private
        { hashtable of tunitdevirtinfo (which contain tclassdevirtinfo) }
        funits: tfphashobjectlist;

        procedure converttreenode(node: tinheritancetreenode; arg: pointer);
        function addunitifnew(const n: shortstring): tunitdevirtinfo;
        function findunit(const n: shortstring): tunitdevirtinfo;
        function getstaticname(forvmtentry: boolean; objdef, procdef: tdef; out staticname: TSymStr): boolean;
        procedure documentformat(writer: twposectionwriterintf);
       public
        constructor create; override;
        destructor destroy; override;

        class function getwpotype: twpotype; override;
        class function generatesinfoforwposwitches: twpoptimizerswitches; override;
        class function performswpoforswitches: twpoptimizerswitches; override;
        class function sectionname: shortstring; override;

        { information collection }
        procedure constructfromcompilerstate; override;
        procedure storewpofilesection(writer: twposectionwriterintf); override;

        { information providing }
        procedure loadfromwpofilesection(reader: twposectionreaderintf); override;
        function staticnameforcallingvirtualmethod(objdef, procdef: tdef; out staticname: TSymStr): boolean; override;
        function staticnameforvmtentry(objdef, procdef: tdef; out staticname: TSymStr): boolean; override;

      end;


  implementation

    uses
      cutils,
      fmodule,
      symconst,
      symbase,
      defcmp,
      verbose;

    const
      DEVIRT_SECTION_NAME = 'contextinsensitive_devirtualization';

   { *************************** tinheritancetreenode ************************* }
    
    constructor tinheritancetreenode.create(_parent: tinheritancetreenode; _def: tobjectdef; _instantiated: boolean);
      begin
        fparent:=_parent;
        fdef:=_def;
        finstantiated:=_instantiated;
        if assigned(_def) then
          fcalledvmtmethods:=tbitset.create(_def.vmtentries.count);
      end;


    destructor tinheritancetreenode.destroy;
      begin
        { fchilds owns its members, so it will free them too }
        fchilds.free;
        fcalledvmtmethods.free;
        inherited destroy;
      end;


    function tinheritancetreenode.childcount: longint;
      begin
        if assigned(fchilds) then
          result:=fchilds.count
        else
          result:=0;
      end;


    function tinheritancetreenode.haschilds: boolean;
      begin
        result:=assigned(fchilds)
      end;


    function tinheritancetreenode.getchild(index: longint): tinheritancetreenode;
      begin
        result:=tinheritancetreenode(fchilds[index]);
      end;


    function tinheritancetreenode.maybeaddchild(_def: tobjectdef; _instantiated: boolean): tinheritancetreenode;
      begin
        { sanity check }
        if assigned(_def.childof) then 
          begin
            if (_def.childof<>def) then
              internalerror(2008092201);
          end
        else if assigned(fparent) then
          internalerror(2008092202);

        if not assigned(fchilds) then
          fchilds:=tfpobjectlist.create(true);
        { def already a child -> return }
        result:=findchild(_def);
        if assigned(result) then
          result.finstantiated:=result.finstantiated or _instantiated
        else
          begin
            { not found, add new child }
            result:=tinheritancetreenode.create(self,_def,_instantiated);
            fchilds.add(result);
          end;
      end;


    function tinheritancetreenode.findchild(_def: tobjectdef): tinheritancetreenode;
      var
        i: longint;
      begin
        result:=nil;
        if assigned(fchilds) then
          for i := 0 to fchilds.count-1 do
            if (tinheritancetreenode(fchilds[i]).def=_def) then
              begin
                result:=tinheritancetreenode(fchilds[i]);
                break;
              end;
      end;

    { *************************** tinheritancetree ************************* }

    constructor tinheritancetree.create;
      begin
        froots:=tinheritancetreenode.create(nil,nil,false);
        classrefdefs:=tfpobjectlist.create(false);
      end;


    destructor tinheritancetree.destroy;
      begin
        froots.free;
        classrefdefs.free;
        inherited destroy;
      end;
      

    function tinheritancetree.registerinstantiatedobjectdefrecursive(def: tobjectdef; instantiated: boolean): tinheritancetreenode;
      begin
        if assigned(def.childof) then
          begin
            { recursively add parent, of which we have no info about whether or not it is
              instantiated at this point -> default to false (will be overridden by "true"
              if this class is instantioted, since then registerinstantiatedobjdef() will
              be called for this class as well)
            }
            result:=registerinstantiatedobjectdefrecursive(def.childof,false);
            { and add ourselves to the parent }
            result:=result.maybeaddchild(def,instantiated);
          end
        else
          { add ourselves to the roots }
          result:=froots.maybeaddchild(def,instantiated);
      end;


    procedure tinheritancetree.registerinstantiatedobjdef(def: tdef);
      begin
        { add the def }
        if (def.typ=objectdef) then
          registerinstantiatedobjectdefrecursive(tobjectdef(def),true)
        else
          internalerror(2008092401);
      end;


    procedure tinheritancetree.registerinstantiatedclassrefdef(def: tdef);
      begin
        { queue for later checking (these are the objectdefs
          to which the classrefdefs point) }
        if (def.typ=objectdef) then
          classrefdefs.add(def)
        else
          internalerror(2008101401);
      end;


    function tinheritancetree.getnodefordef(def: tobjectdef): tinheritancetreenode;
      begin
        if assigned(def.childof) then
          begin
            result:=getnodefordef(def.childof);
            if assigned(result) then
              result:=result.findchild(def);
          end
        else
          result:=froots.findchild(def);
      end;


    procedure tinheritancetree.registercalledvmtentries(entries: tcalledvmtentries);
      var
        node: tinheritancetreenode;
      begin
        node:=getnodefordef(tobjectdef(entries.objdef));
        { it's possible that no instance of this class or its descendants are
          instantiated
        }
        if not assigned(node) then
          exit;
        { now mark these methods as (potentially) called for this type and for
          all of its descendants
        }
        addcalledvmtentries(node,entries.calledentries);
        foreachnodefromroot(node,@addcalledvmtentries,entries.calledentries);
      end;


   procedure tinheritancetree.checkforclassrefinheritance(def: tdef);
     var
       i: longint;
     begin
       if (def.typ=objectdef) then
         begin
{$ifdef debug_devirt}
           write('   Checking for classrefdef inheritance of ',def.typename);
{$endif debug_devirt}
           for i:=0 to classrefdefs.count-1 do
             if def_is_related(tobjectdef(def),tobjectdef(classrefdefs[i])) then
               begin
{$ifdef debug_devirt}
                 writeln('... Found: inherits from Class Of ',tobjectdef(classrefdefs[i]).typename);
{$endif debug_devirt}
                 registerinstantiatedobjdef(def);
                 exit;
               end;
{$ifdef debug_devirt}
           writeln('... Not found!');
{$endif debug_devirt}
         end;
     end;


    procedure tinheritancetree.foreachnodefromroot(root: tinheritancetreenode; proctocall: tinheritancetreecallback; arg: pointer);
        
      procedure process(const node: tinheritancetreenode);
        var
         i: longint;
        begin
          for i:=0 to node.childcount-1 do
            if node.childs[i].haschilds then
              begin
                proctocall(node.childs[i],arg);
                process(node.childs[i])
              end
            else
              proctocall(node.childs[i],arg);
        end;
        
      begin
        process(root);
      end;


    procedure tinheritancetree.foreachnode(proctocall: tinheritancetreecallback; arg: pointer);
      begin
        foreachnodefromroot(froots,proctocall,arg);
      end;


    procedure tinheritancetree.foreachleafnode(proctocall: tinheritancetreecallback; arg: pointer);

      procedure process(const node: tinheritancetreenode);
        var
         i: longint;
        begin
          for i:=0 to node.childcount-1 do
            if node.childs[i].haschilds then
              process(node.childs[i])
            else
              proctocall(node.childs[i],arg);
        end;
        
      begin
        process(froots);
      end;


    procedure tinheritancetree.markvmethods(node: tinheritancetreenode; p: pointer);
      var
        currnode: tinheritancetreenode;
        pd: tprocdef;
        i: longint;
        makeallvirtual: boolean;
      begin
        {$IFDEF DEBUG_DEVIRT}
        writeln('processing leaf node ',node.def.typename);
        {$ENDIF}
        { todo: also process interfaces (ImplementedInterfaces) }
        if (node.def.vmtentries.count=0) then
          exit;
        { process all vmt entries for this class/object }
        for i:=0 to node.def.vmtentries.count-1 do
          begin
            currnode:=node;
            { extra tprocdef(tobject(..)) typecasts so that -CR can catch
              errors in case the vmtentries are not properly (re)deref'd }
            pd:=tprocdef(tobject(pvmtentry(currnode.def.vmtentries[i])^.procdef));
            { abstract methods cannot be called directly }
            if (po_abstractmethod in pd.procoptions) then
              continue;
            {$IFDEF DEBUG_DEVIRT}
            writeln('  method ',pd.typename);
            {$ENDIF}
            { Now mark all virtual methods static that are the same in parent
              classes as in this instantiated child class (only instantiated
              classes can be leaf nodes, since only instantiated classes were
              added to the tree).
              If a first child does not override a parent method while a
              a second one does, the first will mark it as statically
              callable, but the second will set it to not statically callable.
              In the opposite situation, the first will mark it as not
              statically callable and the second will leave it alone.
            }
            makeallvirtual:=false;
            repeat
              if { stop when this method does not exist in a parent }
                 (currnode.def.vmtentries.count<=i) then
                break;
              
              if not assigned(currnode.def.vmcallstaticinfo) then
                currnode.def.vmcallstaticinfo:=allocmem(currnode.def.vmtentries.count*sizeof(tvmcallstatic));
              { if this method cannot be called, we can just mark it as
                unreachable. This will cause its static name to be set to
                FPC_ABSTRACTERROR later on. Exception: published methods are
                always reachable (via RTTI).
              }
              if (pd.visibility<>vis_published) and
                 not(currnode.fcalledvmtmethods.isset(i)) then
                begin
                  currnode.def.vmcallstaticinfo^[i]:=vmcs_unreachable;
                  currnode:=currnode.parent;
                end
              { same procdef as in all instantiated childs? (yes or don't know) }
              else if (currnode.def.vmcallstaticinfo^[i] in [vmcs_default,vmcs_yes]) then
                begin
                  { methods in uninstantiated classes can be made static if
                    they are the same in all instantiated derived classes
                  }
                  if ((pvmtentry(currnode.def.vmtentries[i])^.procdef=pd) or
                      (not currnode.instantiated and
                       (currnode.def.vmcallstaticinfo^[i]=vmcs_default))) and
                      not makeallvirtual then
                    begin
                      {$IFDEF DEBUG_DEVIRT}
                      writeln('    marking as static for ',currnode.def.typename);
                      {$ENDIF}
                      currnode.def.vmcallstaticinfo^[i]:=vmcs_yes;
                      { this is in case of a non-instantiated parent of an instantiated child:
                        the method declared in the child will always be called here
                      }
                      pvmtentry(currnode.def.vmtentries[i])^.procdef:=pd;
                    end
                  else
                    begin
                      {$IFDEF DEBUG_DEVIRT}
                      writeln('    marking as non-static for ',currnode.def.typename);
                      {$ENDIF}
                      { this vmt entry must also remain virtual for all parents }
                      makeallvirtual:=true;
                      currnode.def.vmcallstaticinfo^[i]:=vmcs_no;
                    end;
                  currnode:=currnode.parent;
                end
              else if (currnode.def.vmcallstaticinfo^[i]=vmcs_no) then
                begin
                  {$IFDEF DEBUG_DEVIRT}
                  writeln('    not processing parents, already non-static for ',currnode.def.typename);
                  {$ENDIF}
                  { parents are already set to vmcs_no, so no need to continue }
                  currnode:=nil;
                end
              else
                currnode:=currnode.parent;
            until not assigned(currnode) or
                  not assigned(currnode.def);
          end;
      end;


    procedure tinheritancetree.optimizevirtualmethods;
      begin
        foreachleafnode(@markvmethods,nil);
      end;


    procedure tinheritancetree.printobjectvmtinfo(node: tinheritancetreenode; arg: pointer);
      var
        i,
        totaldevirtualised,
        totalvirtual,
        totalunreachable: ptrint;
      begin
        totaldevirtualised:=0;
        totalvirtual:=0;
        totalunreachable:=0;
        writeln(node.def.typename);
        if (node.def.vmtentries.count=0) then
          begin
            writeln('  No virtual methods!');
            exit;
          end;
        for i:=0 to node.def.vmtentries.count-1 do
          if (po_virtualmethod in pvmtentry(node.def.vmtentries[i])^.procdef.procoptions) then
            begin
              inc(totalvirtual);
              if (node.def.vmcallstaticinfo^[i]=vmcs_yes) then
                begin
                  inc(totaldevirtualised);
                  writeln('  Devirtualised: ',pvmtentry(node.def.vmtentries[i])^.procdef.typename);
                end
              else if (node.def.vmcallstaticinfo^[i]=vmcs_unreachable) then
                begin
                  inc(totalunreachable);
                  writeln('   Unreachable: ',pvmtentry(node.def.vmtentries[i])^.procdef.typename);
                end;
            end;
        writeln('Total devirtualised/unreachable/all: ',totaldevirtualised,'/',totalunreachable,'/',totalvirtual);
        writeln;
      end;


    procedure tinheritancetree.addcalledvmtentries(node: tinheritancetreenode; arg: pointer);
      var
        vmtentries: tbitset absolute arg;
      begin
        node.fcalledvmtmethods.addset(vmtentries);
      end;


    procedure tinheritancetree.printvmtinfo;
      begin
        foreachnode(@printobjectvmtinfo,nil);
      end;


    { helper routines: decompose an object & procdef combo into a unitname, class name and vmtentry number
      (unit name where the objectdef is declared, class name of the objectdef, vmtentry number of the
       procdef -- procdef does not necessarily belong to objectdef, it may also belong to a descendant
       or parent). classprefix is set in case of nested classes.
    }

    procedure defunitclassname(objdef: tobjectdef; out unitname, classname: pshortstring; out classprefix: shortstring);
      const
        mainprogname: string[2] = 'P$';
      var
        mainsymtab,
        objparentsymtab : tsymtable;
      begin
        objparentsymtab:=objdef.symtable;
        mainsymtab:=objparentsymtab.defowner.owner;
        classprefix:='';
        while mainsymtab.symtabletype in [recordsymtable,objectsymtable,localsymtable] do
          begin
            classprefix:=mainsymtab.name^+'.'+classprefix;
            mainsymtab:=mainsymtab.defowner.owner;
          end;
        { main symtable must be static or global }
        if not(mainsymtab.symtabletype in [staticsymtable,globalsymtable]) then
         internalerror(200204177);
        if (TSymtable(main_module.localsymtable)=mainsymtab) and
            (not main_module.is_unit) then
           { same convention as for mangled names }
          unitname:=@mainprogname
        else
          unitname:=mainsymtab.name;
        classname:=tobjectdef(objparentsymtab.defowner).objname;
      end;


    procedure defsdecompose(objdef: tobjectdef; procdef: tprocdef; out unitname, classname: pshortstring; out classprefix: shortstring; out vmtentry: longint);
      begin
        defunitclassname(objdef,unitname,classname,classprefix);
        vmtentry:=procdef.extnumber;
        { if it's $ffff, this is not a valid virtual method }
        if (vmtentry=$ffff) then
          internalerror(2008100509);
      end;


   { tclassdevirtinfo }

    constructor tclassdevirtinfo.create(hashobjectlist:tfphashobjectlist;const n: shortstring; instantiated: boolean);
      begin
        inherited create(hashobjectlist,n);
        finstantiated:=instantiated;
        fstaticmethodnames:=tfplist.create;
      end;

    destructor tclassdevirtinfo.destroy;
      var
        i: longint;
      begin
        for i:=0 to fstaticmethodnames.count-1 do
          if assigned(fstaticmethodnames[i]) then
            freemem(fstaticmethodnames[i]);
        fstaticmethodnames.free;
        inherited destroy;
      end;

    procedure tclassdevirtinfo.addstaticmethod(vmtindex: longint;
      const replacementname: shortstring);
      begin
        if (vmtindex>=fstaticmethodnames.count) then
          fstaticmethodnames.Count:=vmtindex+10;
        fstaticmethodnames[vmtindex]:=stringdup(replacementname);
      end;

    function tclassdevirtinfo.isstaticvmtentry(vmtindex: longint; out
      replacementname: pshortstring): boolean;
      begin
         result:=false;
         if (vmtindex>=fstaticmethodnames.count) then
           exit;

         replacementname:=fstaticmethodnames[vmtindex];
         result:=assigned(replacementname);
      end;

    { tunitdevirtinfo }

    constructor tunitdevirtinfo.create(hashobjectlist:tfphashobjectlist;const n: shortstring);
      begin
        inherited create(hashobjectlist,n);
        fclasses:=tfphashobjectlist.create(true);
      end;

    destructor tunitdevirtinfo.destroy;
      begin
        fclasses.free;
        inherited destroy;
      end;

    function tunitdevirtinfo.addclass(const n: shortstring; instantiated: boolean): tclassdevirtinfo;
      begin
        result:=findclass(n);
        { can't have two classes with the same name in a single unit }
        if assigned(result) then
          internalerror(2008100501);
        result:=tclassdevirtinfo.create(fclasses,n,instantiated);
      end;

    function tunitdevirtinfo.findclass(const n: shortstring): tclassdevirtinfo;
      begin
        result:=tclassdevirtinfo(fclasses.find(n));
      end;


    { tprogdevirtinfo }

    procedure tprogdevirtinfo.converttreenode(node: tinheritancetreenode; arg: pointer);
      var
        i: longint;
        classprefix: shortstring;
        unitid, classid: pshortstring;
        unitdevirtinfo: tunitdevirtinfo;
        classdevirtinfo: tclassdevirtinfo;
      begin
        if (not node.instantiated) and
           (node.def.vmtentries.count=0) then
          exit;
        { always add a class entry for an instantiated class, so we can
          fill the vmt's of non-instantiated classes with calls to
          FPC_ABSTRACTERROR during the optimisation phase
        }
        defunitclassname(node.def,unitid,classid,classprefix);
        unitdevirtinfo:=addunitifnew(unitid^);
        classdevirtinfo:=unitdevirtinfo.addclass(classprefix+classid^,node.instantiated);
        if (node.def.vmtentries.count=0) then
          exit;
        for i:=0 to node.def.vmtentries.count-1 do
          if (po_virtualmethod in pvmtentry(node.def.vmtentries[i])^.procdef.procoptions) then
            case node.def.vmcallstaticinfo^[i] of
              vmcs_yes:
                begin
                  { add info about devirtualised vmt entry }
                  classdevirtinfo.addstaticmethod(i,pvmtentry(node.def.vmtentries[i])^.procdef.mangledname);
                end;
              vmcs_unreachable:
                begin
                  { static reference to FPC_ABSTRACTERROR }
                  classdevirtinfo.addstaticmethod(i,'FPC_ABSTRACTERROR');
                end;
              else
                ;
            end;
      end;


    constructor tprogdevirtinfo.create;
      begin
        inherited create;
      end;


    destructor tprogdevirtinfo.destroy;
      begin
        funits.free;
        inherited destroy;
      end;


    class function tprogdevirtinfo.getwpotype: twpotype;
      begin
        result:=wpo_devirtualization_context_insensitive;
      end;


    class function tprogdevirtinfo.generatesinfoforwposwitches: twpoptimizerswitches;
      begin
        result:=[cs_wpo_devirtualize_calls,cs_wpo_optimize_vmts];
      end;


    class function tprogdevirtinfo.performswpoforswitches: twpoptimizerswitches;
      begin
        result:=[cs_wpo_devirtualize_calls,cs_wpo_optimize_vmts];
      end;


    class function tprogdevirtinfo.sectionname: shortstring;
      begin
        result:=DEVIRT_SECTION_NAME;
      end;


    procedure tprogdevirtinfo.constructfromcompilerstate;
      var
        hp: tmodule;
        i: longint;
        inheritancetree: tinheritancetree;
      begin
         { register all instantiated class/object types }
         hp:=tmodule(loaded_units.first);
         while assigned(hp) do
          begin
            if assigned(hp.wpoinfo.createdobjtypes) then
              for i:=0 to hp.wpoinfo.createdobjtypes.count-1 do
                tdef(hp.wpoinfo.createdobjtypes[i]).register_created_object_type;
            if assigned(hp.wpoinfo.createdclassrefobjtypes) then
              for i:=0 to hp.wpoinfo.createdclassrefobjtypes.count-1 do
                tobjectdef(hp.wpoinfo.createdclassrefobjtypes[i]).register_created_classref_type;
            if assigned(hp.wpoinfo.maybecreatedbyclassrefdeftypes) then
              for i:=0 to hp.wpoinfo.maybecreatedbyclassrefdeftypes.count-1 do
                tobjectdef(hp.wpoinfo.maybecreatedbyclassrefdeftypes[i]).register_maybe_created_object_type;
            hp:=tmodule(hp.next);
          end;
         inheritancetree:=tinheritancetree.create;

         { add all constructed class/object types to the tree }
{$IFDEF DEBUG_DEVIRT}
         writeln('constructed object/class/classreftypes in ',current_module.realmodulename^);
{$ENDIF}
         for i := 0 to current_module.wpoinfo.createdobjtypes.count-1 do
           begin
             inheritancetree.registerinstantiatedobjdef(tdef(current_module.wpoinfo.createdobjtypes[i]));
{$IFDEF DEBUG_DEVIRT}
             write('  ',tdef(current_module.wpoinfo.createdobjtypes[i]).GetTypeName);
{$ENDIF}
             case tdef(current_module.wpoinfo.createdobjtypes[i]).typ of
               objectdef:
                 case tobjectdef(current_module.wpoinfo.createdobjtypes[i]).objecttype of
                   odt_object:
{$IFDEF DEBUG_DEVIRT}
                     writeln(' (object)')
{$ENDIF}
                     ;
                   odt_class:
{$IFDEF DEBUG_DEVIRT}
                     writeln(' (class)')
{$ENDIF}
                     ;
                   else
                     internalerror(2008092101);
                 end;
               else
                 internalerror(2008092102);
             end;
           end;

         { register all instantiated classrefdefs with the tree }
         for i := 0 to current_module.wpoinfo.createdclassrefobjtypes.count-1 do
           begin
             inheritancetree.registerinstantiatedclassrefdef(tdef(current_module.wpoinfo.createdclassrefobjtypes[i]));
{$IFDEF DEBUG_DEVIRT}
             write('  Class Of ',tdef(current_module.wpoinfo.createdclassrefobjtypes[i]).GetTypeName);
{$ENDIF}
             case tdef(current_module.wpoinfo.createdclassrefobjtypes[i]).typ of
               objectdef:
{$IFDEF DEBUG_DEVIRT}
                 writeln(' (classrefdef)')
{$ENDIF}
                 ;
               else
                 internalerror(2008101101);
             end;
           end;


         { now add all objectdefs that are referred somewhere (via a
           loadvmtaddr node) and that are derived from an instantiated
           classrefdef to the tree (as they can, in theory, all
           be instantiated as well)
         }
         for i := 0 to current_module.wpoinfo.maybecreatedbyclassrefdeftypes.count-1 do
           begin
             inheritancetree.checkforclassrefinheritance(tdef(current_module.wpoinfo.maybecreatedbyclassrefdeftypes[i]));
{$IFDEF DEBUG_DEVIRT}
             write('  Class Of ',tdef(current_module.wpoinfo.maybecreatedbyclassrefdeftypes[i]).GetTypeName);
{$ENDIF}
             case tdef(current_module.wpoinfo.maybecreatedbyclassrefdeftypes[i]).typ of
               objectdef:
{$IFDEF DEBUG_DEVIRT}
                 writeln(' (classrefdef)')
{$ENDIF}
                 ;
               else
                 internalerror(2008101106);
             end;
           end;

         { add info about called virtual methods }
         hp:=tmodule(loaded_units.first);
         while assigned(hp) do
          begin
            if assigned(hp.wpoinfo.calledvmtentries) then
              for i:=0 to hp.wpoinfo.calledvmtentries.count-1 do
                inheritancetree.registercalledvmtentries(tcalledvmtentries(hp.wpoinfo.calledvmtentries[i]));
            hp:=tmodule(hp.next);
          end;


         inheritancetree.optimizevirtualmethods;
{$ifdef DEBUG_DEVIRT}
         inheritancetree.printvmtinfo;
{$endif DEBUG_DEVIRT}
         inheritancetree.foreachnode(@converttreenode,nil);
         inheritancetree.free;
      end;


    function tprogdevirtinfo.addunitifnew(const n: shortstring): tunitdevirtinfo;
      begin
        if assigned(funits) then
          result:=findunit(n)
        else
          begin
            funits:=tfphashobjectlist.create;
            result:=nil;
          end;
        if not assigned(result) then
          begin
            result:=tunitdevirtinfo.create(funits,n);
          end;
      end;


    function tprogdevirtinfo.findunit(const n: shortstring): tunitdevirtinfo;
      begin
        result:=tunitdevirtinfo(funits.find(n));
      end;


    procedure tprogdevirtinfo.loadfromwpofilesection(reader: twposectionreaderintf);
      var
        unitid,
        classid,
        vmtentryname: string;
        vmttype: string[15];
        vmtentrynrstr: string[7];
        classinstantiated: string[1];
        vmtentry, error: longint;
        unitdevirtinfo: tunitdevirtinfo;
        classdevirtinfo: tclassdevirtinfo;
        instantiated: boolean;
      begin
        { format:
            # unitname^
            unit1^
            # classname&
            class1&
            # instantiated?
            1
            # vmt type (base or some interface)
            basevmt
            # vmt entry nr
            0
            # name of routine to call instead
            staticvmtentryforslot0
            5
            staticvmtentryforslot5
            intfvmt1
            0
            staticvmtentryforslot0

            # non-instantiated class (but if we encounter a variable of this
            # type, we can optimise class to vmtentry 1)
            class2&
            0
            basevmt
            1
            staticvmtentryforslot1

            # instantiated class without optimisable virtual methods
            class3&
            1

            unit2^
            1
            class3&
            ...

            currently, only basevmt is supported (no interfaces yet)
        }
        { could be empty if no classes or so }
        if not reader.sectiongetnextline(unitid) then
          exit;
        repeat
          if (unitid='') or
             (unitid[length(unitid)]<>'^') then
            internalerror(2008100502);
          { cut off the trailing ^ }
          setlength(unitid,length(unitid)-1);
          unitdevirtinfo:=addunitifnew(unitid);
          { now read classes }
          if not reader.sectiongetnextline(classid) then
            internalerror(2008100505);
          repeat
            if (classid='') or
               (classid[length(classid)]<>'&') then
              internalerror(2008100503);
            { instantiated? }
            if not reader.sectiongetnextline(classinstantiated) then
              internalerror(2008101901);
            instantiated:=classinstantiated='1';
            { cut off the trailing & }
            setlength(classid,length(classid)-1);
            classdevirtinfo:=unitdevirtinfo.addclass(classid,instantiated);
            { last class could be an instantiated class without any
               optimisable methods. }
            if not reader.sectiongetnextline(vmttype) then
              exit;
            { any optimisable virtual methods? }
            if (vmttype<>'') then
              begin
                { interface info is not yet supported }
                if (vmttype<>'basevmt') then
                  internalerror(2008100507);
                { read all vmt entries for this class }
                while reader.sectiongetnextline(vmtentrynrstr) and
                      (vmtentrynrstr<>'') do
                  begin
                    val(vmtentrynrstr,vmtentry,error);
                    if (error<>0) then
                      internalerror(2008100504);
                    if not reader.sectiongetnextline(vmtentryname) or
                       (vmtentryname='') then
                      internalerror(2008100508);
                    classdevirtinfo.addstaticmethod(vmtentry,vmtentryname);
                  end;
              end;
            { end of section -> exit }
            if not(reader.sectiongetnextline(classid)) then
              exit;
          until (classid='') or
                (classid[length(classid)]='^');
          { next unit, or error }
          unitid:=classid;
        until false;
      end;


    procedure tprogdevirtinfo.documentformat(writer: twposectionwriterintf);
      begin
        writer.sectionputline('# section format:');
        writer.sectionputline('# unit1^');
        writer.sectionputline('# class1&                ; classname&');
        writer.sectionputline('# 1                      ; instantiated or not');
        writer.sectionputline('# basevmt                ; vmt type (base or some interface)');
        writer.sectionputline('# # vmt entry nr');
        writer.sectionputline('# 0                      ; vmt entry nr');
        writer.sectionputline('# staticvmtentryforslot0 ; name or routine to call instead');
        writer.sectionputline('# 5');
        writer.sectionputline('# staticvmtentryforslot5');
        writer.sectionputline('# intfvmt1');
        writer.sectionputline('# 0');
        writer.sectionputline('# staticvmtentryforslot0');
        writer.sectionputline('#');
        writer.sectionputline('# class2&');
        writer.sectionputline('# 0                      ; non-instantiated class (can be variables of this type, e.g. TObject)');
        writer.sectionputline('# basevmt');
        writer.sectionputline('# 1');
        writer.sectionputline('# staticvmtentryforslot1');
        writer.sectionputline('#');
        writer.sectionputline('# class3&                ; instantiated class without optimisable virtual methods');
        writer.sectionputline('# 1');
        writer.sectionputline('#');
        writer.sectionputline('# unit2^');
        writer.sectionputline('# 1');
        writer.sectionputline('# class3&');
        writer.sectionputline('# ...');
        writer.sectionputline('#');
        writer.sectionputline('# currently, only basevmt is supported (no interfaces yet)');
        writer.sectionputline('#');
      end;


    procedure tprogdevirtinfo.storewpofilesection(writer: twposectionwriterintf);
      var
        unitcount,
        classcount,
        vmtentrycount: longint;
        unitdevirtinfo: tunitdevirtinfo;
        classdevirtinfo: tclassdevirtinfo;
        first: boolean;
      begin
        writer.startsection(DEVIRT_SECTION_NAME);
        { if there are no optimised virtual methods, we have stored no info }
        if not assigned(funits) then
          exit;
        documentformat(writer);
        for unitcount:=0 to funits.count-1 do
          begin
            unitdevirtinfo:=tunitdevirtinfo(funits[unitcount]);
            writer.sectionputline(unitdevirtinfo.name+'^');
            for classcount:=0 to unitdevirtinfo.fclasses.count-1 do
              begin
                classdevirtinfo:=tclassdevirtinfo(tunitdevirtinfo(funits[unitcount]).fclasses[classcount]);
                writer.sectionputline(classdevirtinfo.name+'&');
                writer.sectionputline(tostr(ord(classdevirtinfo.instantiated)));
                first:=true;
                for vmtentrycount:=0 to classdevirtinfo.fstaticmethodnames.count-1 do
                  if assigned(classdevirtinfo.fstaticmethodnames[vmtentrycount]) then
                    begin
                      if first then
                        begin
                          writer.sectionputline('basevmt');
                          first:=false;
                        end;
                      writer.sectionputline(tostr(vmtentrycount));
                      writer.sectionputline(pshortstring(classdevirtinfo.fstaticmethodnames[vmtentrycount])^);
                    end;
                writer.sectionputline('');
              end;
          end;
      end;


    function tprogdevirtinfo.getstaticname(forvmtentry: boolean; objdef, procdef: tdef; out staticname: TSymStr): boolean;
      var
        unitid,
        classid,
        newname: pshortstring;
        unitdevirtinfo: tunitdevirtinfo;
        classdevirtinfo: tclassdevirtinfo;
        vmtentry: longint;
        realobjdef: tobjectdef;
        classprefix: shortstring;
      begin
         { if we don't have any devirtualisation info, exit }
         if not assigned(funits) then
           begin
             result:=false;
             exit
           end;
         { class methods are in the regular vmt, so we can handle classrefs
           the same way as plain objectdefs
         }
         if (objdef.typ=classrefdef) then
           realobjdef:=tobjectdef(tclassrefdef(objdef).pointeddef)
         else if (objdef.typ=objectdef) and
            (tobjectdef(objdef).objecttype in [odt_class,odt_object]) then
           realobjdef:=tobjectdef(objdef)
         else
           begin
             { we don't support interfaces yet }
             result:=false;
             exit;
           end;

         { if it's for a vmtentry of an objdef and the objdef is
           not instantiated, then we can fill the vmt with pointers
           to FPC_ABSTRACTERROR, except for published methods
           (these can be called via rtti, so always have to point
            to the original method)
         }
         if forvmtentry and
            (tprocdef(procdef).visibility=vis_published) then
           begin
             result:=false;
             exit;
           end;

         { get the component names for the class/procdef combo }
         defsdecompose(realobjdef,tprocdef(procdef),unitid,classid,classprefix,vmtentry);

         { If we don't have information about a particular unit/class/method,
           it means that such class cannot be instantiated. So if we are
           looking up information for a vmt entry, we can always safely return
           FPC_ABSTRACTERROR if we do not find anything, unless it's a
           published method (but those are handled already above) or a
           class method (can be called even if the class is not instantiated).
         }
         result:=
           forvmtentry and
           not(po_classmethod in tprocdef(procdef).procoptions);
         staticname:='FPC_ABSTRACTERROR';

         { do we have any info for this unit? }
         unitdevirtinfo:=findunit(unitid^);
         if not assigned(unitdevirtinfo) then
           exit;
         { and for this class? }
         classdevirtinfo:=unitdevirtinfo.findclass(classprefix+classid^);
         if not assigned(classdevirtinfo) then
           exit;
         if forvmtentry and
            (objdef.typ=objectdef) and
            not classdevirtinfo.instantiated and
            { virtual class methods can be called even if the class is not instantiated }
            not(po_classmethod in tprocdef(procdef).procoptions) then
           begin
             { already set above
               staticname:='FPC_ABSTRACTERROR';
             }
             result:=true;
           end
         else
           begin
             { now check whether it can be devirtualised, and if so to what }
             result:=classdevirtinfo.isstaticvmtentry(vmtentry,newname);
             if result then
               staticname:=newname^;
           end;
      end;



    function tprogdevirtinfo.staticnameforcallingvirtualmethod(objdef, procdef: tdef; out staticname: TSymStr): boolean;
      begin
        result:=getstaticname(false,objdef,procdef,staticname);
      end;


    function tprogdevirtinfo.staticnameforvmtentry(objdef, procdef: tdef; out staticname: TSymStr): boolean;
      begin
        result:=getstaticname(true,objdef,procdef,staticname);
      end;

end.
