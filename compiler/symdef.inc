{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl, Pierre Muller

    Symbol table implementation for the definitions

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

{****************************************************************************
                     TDEF (base class for definitions)
****************************************************************************}

    const
       { if you change one of the following contants, }
       { you have also to change the typinfo unit     }
       { and the rtl/i386,template/rttip.inc files    }
       tkUnknown       = 0;
       tkInteger       = 1;
       tkChar          = 2;
       tkEnumeration   = 3;
       tkFloat         = 4;
       tkSet           = 5;
       tkMethod        = 6;
       tkSString       = 7;
       tkString        = tkSString;
       tkLString       = 8;
       tkAString       = 9;
       tkWString       = 10;
       tkVariant       = 11;
       tkArray         = 12;
       tkRecord        = 13;
       tkInterface     = 14;
       tkClass         = 15;
       tkObject        = 16;
       tkWChar         = 17;
       tkBool          = 18;

       otSByte         = 0;
       otUByte         = 1;
       otSWord         = 2;
       otUWord         = 3;
       otSLong         = 4;
       otULong         = 5;

       ftSingle        = 0;
       ftDouble        = 1;
       ftExtended      = 2;
       ftComp          = 3;
       ftCurr          = 4;
       ftFixed16       = 5;
       ftFixed32       = 6;

       mkProcedure     = 0;
       mkFunction      = 1;
       mkConstructor   = 2;
       mkDestructor    = 3;
       mkClassProcedure= 4;
       mkClassFunction = 5;

       pfvar           = 1;
       pfConst         = 2;
       pfArray         = 4;
       pfAddress       = 8;
       pfReference     = 16;
       pfOut           = 32;


    constructor tdef.init;
      begin
         inherited init;
         deftype:=abstractdef;
         owner := nil;
         typesym := nil;
         savesize := 0;
         if registerdef then
           symtablestack^.registerdef(@self);
         has_rtti:=false;
         has_inittable:=false;
{$ifdef GDB}
         is_def_stab_written := false;
         globalnb := 0;
{$endif GDB}
         if assigned(lastglobaldef) then
           begin
              lastglobaldef^.nextglobal := @self;
              previousglobal:=lastglobaldef;
           end
         else
           begin
              firstglobaldef := @self;
              previousglobal := nil;
           end;
         lastglobaldef := @self;
         nextglobal := nil;
      end;


    constructor tdef.load;
      begin
         deftype:=abstractdef;
         next := nil;
         owner := nil;
         has_rtti:=false;
         has_inittable:=false;
{$ifdef GDB}
         is_def_stab_written := false;
         globalnb := 0;
{$endif GDB}
         if assigned(lastglobaldef) then
           begin
              lastglobaldef^.nextglobal := @self;
              previousglobal:=lastglobaldef;
           end
         else
           begin
              firstglobaldef := @self;
              previousglobal:=nil;
           end;
         lastglobaldef := @self;
         nextglobal := nil;
      { load }
         indexnr:=readword;
         typesym:=ptypesym(readsymref);
      end;


    destructor tdef.done;
      begin
         { first element  ? }
         if not(assigned(previousglobal)) then
           begin
              firstglobaldef := nextglobal;
              if assigned(firstglobaldef) then
                firstglobaldef^.previousglobal:=nil;
           end
         else
           begin
              { remove reference in the element before }
              previousglobal^.nextglobal:=nextglobal;
           end;
         { last element ? }
         if not(assigned(nextglobal)) then
           begin
              lastglobaldef := previousglobal;
              if assigned(lastglobaldef) then
                lastglobaldef^.nextglobal:=nil;
           end
         else
           nextglobal^.previousglobal:=previousglobal;
         previousglobal:=nil;
         nextglobal:=nil;
{$ifdef SYNONYM}
         while assigned(typesym) do
           begin
              typesym^.restype.setdef(nil);
              typesym:=typesym^.synonym;
           end;
{$endif}
      end;

    { used for enumdef because the symbols are
      inserted in the owner symtable }
    procedure tdef.correct_owner_symtable;
      var
         st : psymtable;
      begin
         if assigned(owner) and
            (owner^.symtabletype in [recordsymtable,objectsymtable]) then
           begin
              owner^.defindex^.deleteindex(@self);
              st:=owner;
              while (st^.symtabletype in [recordsymtable,objectsymtable]) do
                st:=st^.next;
              st^.registerdef(@self);
           end;
      end;


    function tdef.typename:string;
      begin
        if assigned(typesym) then
         typename:=Upper(typesym^.name)
        else
         typename:=gettypename;
      end;

    function tdef.gettypename : string;

      begin
         gettypename:='<unknown type>'
      end;

    function tdef.is_in_current : boolean;
      var
        p : psymtable;
      begin
         p:=owner;
         is_in_current:=false;
         while assigned(p) do
           begin
              if (p=current_module^.globalsymtable) or (p=current_module^.localsymtable)
                 or (p^.symtabletype in [globalsymtable,staticsymtable]) then
                begin
                   is_in_current:=true;
                   exit;
                end
              else if p^.symtabletype in [localsymtable,parasymtable,objectsymtable] then
                begin
                  if assigned(p^.defowner) then
                    p:=pobjectdef(p^.defowner)^.owner
                  else
                    exit;
                end
              else
                exit;
           end;

      end;

    procedure tdef.write;
      begin
        writeword(indexnr);
        writesymref(typesym);
{$ifdef GDB}
        if globalnb = 0 then
          begin
            if assigned(owner) then
              globalnb := owner^.getnewtypecount
            else
              begin
                globalnb := PGlobalTypeCount^;
                Inc(PGlobalTypeCount^);
              end;
           end;
{$endif GDB}
      end;


    function tdef.size : longint;
      begin
         size:=savesize;
      end;


    function tdef.alignment : longint;
      begin
         { normal alignment by default }
         alignment:=0;
      end;


{$ifdef GDB}
   procedure tdef.set_globalnb;
     begin
         globalnb :=PGlobalTypeCount^;
         inc(PglobalTypeCount^);
     end;


    function tdef.stabstring : pchar;
      begin
      stabstring := strpnew('t'+numberstring+';');
      end;


    function tdef.numberstring : string;
      var table : psymtable;
      begin
      {formal def have no type !}
      if deftype = formaldef then
        begin
        numberstring := voiddef^.numberstring;
        exit;
        end;
      if (not assigned(typesym)) or (not typesym^.isusedinstab) then
        begin
           {set even if debuglist is not defined}
           if assigned(typesym) then
             typesym^.isusedinstab := true;
           if assigned(debuglist) and not is_def_stab_written then
             concatstabto(debuglist);
        end;
      if not (cs_gdb_dbx in aktglobalswitches) then
        begin
           if globalnb = 0 then
             set_globalnb;
           numberstring := tostr(globalnb);
        end
      else
        begin
           if globalnb = 0 then
             begin
                if assigned(owner) then
                  globalnb := owner^.getnewtypecount
                else
                  begin
                     globalnb := PGlobalTypeCount^;
                     Inc(PGlobalTypeCount^);
                  end;
             end;
           if assigned(typesym) then
             begin
                table := typesym^.owner;
                if table^.unitid > 0 then
                  numberstring := '('+tostr(table^.unitid)+','+tostr(typesym^.restype.def^.globalnb)+')'
                else
                  numberstring := tostr(globalnb);
                exit;
             end;
           numberstring := tostr(globalnb);
        end;
      end;


    function tdef.allstabstring : pchar;
    var stabchar : string[2];
        ss,st : pchar;
        sname : string;
        sym_line_no : longint;
      begin
      ss := stabstring;
      getmem(st,strlen(ss)+512);
      stabchar := 't';
      if deftype in tagtypes then
        stabchar := 'Tt';
      if assigned(typesym) then
        begin
           sname := typesym^.name;
           sym_line_no:=typesym^.fileinfo.line;
        end
      else
        begin
           sname := ' ';
           sym_line_no:=0;
        end;
      strpcopy(st,'"'+sname+':'+stabchar+numberstring+'=');
      strpcopy(strecopy(strend(st),ss),'",'+tostr(N_LSYM)+',0,'+tostr(sym_line_no)+',0');
      allstabstring := strnew(st);
      freemem(st,strlen(ss)+512);
      strdispose(ss);
      end;


    procedure tdef.concatstabto(asmlist : paasmoutput);
     var stab_str : pchar;
    begin
    if ((typesym = nil) or typesym^.isusedinstab or (cs_gdb_dbx in aktglobalswitches))
      and not is_def_stab_written then
      begin
      If cs_gdb_dbx in aktglobalswitches then
        begin
           { otherwise you get two of each def }
           If assigned(typesym) then
             begin
                if typesym^.typ=symconst.typesym then
                  typesym^.isusedinstab:=true;
                if (typesym^.owner = nil) or
                  ((typesym^.owner^.symtabletype = unitsymtable) and
                 punitsymtable(typesym^.owner)^.dbx_count_ok)  then
                begin
                   {with DBX we get the definition from the other objects }
                   is_def_stab_written := true;
                   exit;
                end;
             end;
        end;
      { to avoid infinite loops }
      is_def_stab_written := true;
      stab_str := allstabstring;
      asmlist^.concat(new(pai_stabs,init(stab_str)));
      end;
    end;
{$endif GDB}


    procedure tdef.deref;
      begin
        resolvesym(psym(typesym));
      end;


    { rtti generation }
    procedure tdef.generate_rtti;
      begin
         if not has_rtti then
          begin
            has_rtti:=true;
            getdatalabel(rtti_label);
            write_child_rtti_data;
            rttilist^.concat(new(pai_symbol,init(rtti_label,0)));
            write_rtti_data;
            rttilist^.concat(new(pai_symbol_end,init(rtti_label)));
          end;
      end;


    function tdef.get_rtti_label : string;
      begin
         generate_rtti;
         get_rtti_label:=rtti_label^.name;
      end;


    { init table handling }
    function tdef.needs_inittable : boolean;
      begin
         needs_inittable:=false;
      end;


    procedure tdef.generate_inittable;
      begin
         has_inittable:=true;
         getdatalabel(inittable_label);
         write_child_init_data;
         rttilist^.concat(new(pai_label,init(inittable_label)));
         write_init_data;
      end;


    procedure tdef.write_init_data;
      begin
         write_rtti_data;
      end;


    procedure tdef.write_child_init_data;
      begin
         write_child_rtti_data;
      end;


    function tdef.get_inittable_label : pasmlabel;
      begin
         if not(has_inittable) then
           generate_inittable;
         get_inittable_label:=inittable_label;
      end;


    procedure tdef.write_rtti_name;
      var
         str : string;
      begin
         { name }
         if assigned(typesym) then
           begin
              str:=typesym^.name;
              rttilist^.concat(new(pai_string,init(chr(length(str))+str)));
           end
         else
           rttilist^.concat(new(pai_string,init(#0)))
      end;


    { returns true, if the definition can be published }
    function tdef.is_publishable : boolean;
      begin
         is_publishable:=false;
      end;


    procedure tdef.write_rtti_data;
      begin
      end;


    procedure tdef.write_child_rtti_data;
      begin
      end;


   function tdef.is_intregable : boolean;

     begin
        is_intregable:=false;
        case deftype of
          pointerdef,
          enumdef,
          procvardef :
            is_intregable:=true;
          orddef :
            case porddef(@self)^.typ of
              bool8bit,bool16bit,bool32bit,
              u8bit,u16bit,u32bit,
              s8bit,s16bit,s32bit:
                is_intregable:=true;
            end;
          setdef:
            is_intregable:=is_smallset(@self);
        end;
     end;

   function tdef.is_fpuregable : boolean;

     begin
        is_fpuregable:=(deftype=floatdef) and not(pfloatdef(@self)^.typ in [f32bit,f16bit]);
     end;

{****************************************************************************
                               TSTRINGDEF
****************************************************************************}

    constructor tstringdef.shortinit(l : byte);
      begin
         tdef.init;
         string_typ:=st_shortstring;
         deftype:=stringdef;
         len:=l;
         savesize:=len+1;
      end;


    constructor tstringdef.shortload;
      begin
         tdef.load;
         string_typ:=st_shortstring;
         deftype:=stringdef;
         len:=readbyte;
         savesize:=len+1;
      end;


    constructor tstringdef.longinit(l : longint);
      begin
         tdef.init;
         string_typ:=st_longstring;
         deftype:=stringdef;
         len:=l;
         savesize:=target_os.size_of_pointer;
      end;


    constructor tstringdef.longload;
      begin
         tdef.load;
         deftype:=stringdef;
         string_typ:=st_longstring;
         len:=readlong;
         savesize:=target_os.size_of_pointer;
      end;


    constructor tstringdef.ansiinit(l : longint);
      begin
         tdef.init;
         string_typ:=st_ansistring;
         deftype:=stringdef;
         len:=l;
         savesize:=target_os.size_of_pointer;
      end;


    constructor tstringdef.ansiload;
      begin
         tdef.load;
         deftype:=stringdef;
         string_typ:=st_ansistring;
         len:=readlong;
         savesize:=target_os.size_of_pointer;
      end;


    constructor tstringdef.wideinit(l : longint);
      begin
         tdef.init;
         string_typ:=st_widestring;
         deftype:=stringdef;
         len:=l;
         savesize:=target_os.size_of_pointer;
      end;


    constructor tstringdef.wideload;
      begin
         tdef.load;
         deftype:=stringdef;
         string_typ:=st_widestring;
         len:=readlong;
         savesize:=target_os.size_of_pointer;
      end;


    function tstringdef.stringtypname:string;
      const
        typname:array[tstringtype] of string[8]=('',
          'SHORTSTR','LONGSTR','ANSISTR','WIDESTR'
        );
      begin
        stringtypname:=typname[string_typ];
      end;


    function tstringdef.size : longint;
      begin
        size:=savesize;
      end;


    procedure tstringdef.write;
      begin
         tdef.write;
         if string_typ=st_shortstring then
           writebyte(len)
         else
           writelong(len);
         case string_typ of
           st_shortstring : current_ppu^.writeentry(ibshortstringdef);
            st_longstring : current_ppu^.writeentry(iblongstringdef);
            st_ansistring : current_ppu^.writeentry(ibansistringdef);
            st_widestring : current_ppu^.writeentry(ibwidestringdef);
         end;
      end;


{$ifdef GDB}
    function tstringdef.stabstring : pchar;
      var
        bytest,charst,longst : string;
      begin
        case string_typ of
           st_shortstring:
             begin
               charst := typeglobalnumber('char');
               { this is what I found in stabs.texinfo but
                 gdb 4.12 for go32 doesn't understand that !! }
             {$IfDef GDBknowsstrings}
               stabstring := strpnew('n'+charst+';'+tostr(len));
             {$else}
               bytest := typeglobalnumber('byte');
               stabstring := strpnew('s'+tostr(len+1)+'length:'+bytest
                  +',0,8;st:ar'+bytest
                  +';1;'+tostr(len)+';'+charst+',8,'+tostr(len*8)+';;');
             {$EndIf}
             end;
           st_longstring:
             begin
               charst := typeglobalnumber('char');
               { this is what I found in stabs.texinfo but
                 gdb 4.12 for go32 doesn't understand that !! }
             {$IfDef GDBknowsstrings}
               stabstring := strpnew('n'+charst+';'+tostr(len));
             {$else}
               bytest := typeglobalnumber('byte');
               longst := typeglobalnumber('longint');
               stabstring := strpnew('s'+tostr(len+5)+'length:'+longst
                  +',0,32;dummy:'+bytest+',32,8;st:ar'+bytest
                  +';1;'+tostr(len)+';'+charst+',40,'+tostr(len*8)+';;');
             {$EndIf}
             end;
           st_ansistring:
             begin
               { an ansi string looks like a pchar easy !! }
               stabstring:=strpnew('*'+typeglobalnumber('char'));
             end;
           st_widestring:
             begin
               { an ansi string looks like a pchar easy !! }
               stabstring:=strpnew('*'+typeglobalnumber('char'));
             end;
      end;
    end;


    procedure tstringdef.concatstabto(asmlist : paasmoutput);
      begin
        inherited concatstabto(asmlist);
      end;
{$endif GDB}


    function tstringdef.needs_inittable : boolean;
      begin
         needs_inittable:=string_typ in [st_ansistring,st_widestring];
      end;

    function tstringdef.gettypename : string;

      const
         names : array[tstringtype] of string[20] = ('',
           'ShortString','LongString','AnsiString','WideString');

      begin
         gettypename:=names[string_typ];
      end;

    procedure tstringdef.write_rtti_data;
      begin
         case string_typ of
            st_ansistring:
              begin
                 rttilist^.concat(new(pai_const,init_8bit(tkAString)));
                 write_rtti_name;
              end;
            st_widestring:
              begin
                 rttilist^.concat(new(pai_const,init_8bit(tkWString)));
                 write_rtti_name;
              end;
            st_longstring:
              begin
                 rttilist^.concat(new(pai_const,init_8bit(tkLString)));
                 write_rtti_name;
              end;
            st_shortstring:
              begin
                 rttilist^.concat(new(pai_const,init_8bit(tkSString)));
                 write_rtti_name;
                 rttilist^.concat(new(pai_const,init_8bit(len)));
              end;
         end;
      end;


    function tstringdef.is_publishable : boolean;
      begin
         is_publishable:=true;
      end;


{****************************************************************************
                                 TENUMDEF
****************************************************************************}

    constructor tenumdef.init;
      begin
         tdef.init;
         deftype:=enumdef;
         minval:=0;
         maxval:=0;
         calcsavesize;
         has_jumps:=false;
         basedef:=nil;
         rangenr:=0;
         firstenum:=nil;
         correct_owner_symtable;
      end;

    constructor tenumdef.init_subrange(_basedef:penumdef;_min,_max:longint);
      begin
         tdef.init;
         deftype:=enumdef;
         minval:=_min;
         maxval:=_max;
         basedef:=_basedef;
         calcsavesize;
         has_jumps:=false;
         rangenr:=0;
         firstenum:=basedef^.firstenum;
         while assigned(firstenum) and (penumsym(firstenum)^.value<>minval) do
          firstenum:=firstenum^.nextenum;
         correct_owner_symtable;
      end;


    constructor tenumdef.load;
      begin
         tdef.load;
         deftype:=enumdef;
         basedef:=penumdef(readdefref);
         minval:=readlong;
         maxval:=readlong;
         savesize:=readlong;
         has_jumps:=false;
         firstenum:=Nil;
      end;


    procedure tenumdef.calcsavesize;
      begin
        if (aktpackenum=4) or (min<0) or (max>65535) then
         savesize:=4
        else
         if (aktpackenum=2) or (min<0) or (max>255) then
          savesize:=2
        else
         savesize:=1;
      end;


    procedure tenumdef.setmax(_max:longint);
      begin
        maxval:=_max;
        calcsavesize;
      end;


    procedure tenumdef.setmin(_min:longint);
      begin
        minval:=_min;
        calcsavesize;
      end;


    function tenumdef.min:longint;
      begin
        min:=minval;
      end;


    function tenumdef.max:longint;
      begin
        max:=maxval;
      end;


    procedure tenumdef.deref;
      begin
        inherited deref;
        resolvedef(pdef(basedef));
      end;


    destructor tenumdef.done;
      begin
        inherited done;
      end;


    procedure tenumdef.write;
      begin
         tdef.write;
         writedefref(basedef);
         writelong(min);
         writelong(max);
         writelong(savesize);
         current_ppu^.writeentry(ibenumdef);
      end;


    function tenumdef.getrangecheckstring : string;
      begin
         if (cs_create_smart in aktmoduleswitches) then
           getrangecheckstring:='R_'+current_module^.modulename^+tostr(rangenr)
         else
           getrangecheckstring:='R_'+tostr(rangenr);
      end;


    procedure tenumdef.genrangecheck;
      begin
         if rangenr=0 then
           begin
              { generate two constant for bounds }
              getlabelnr(rangenr);
              if (cs_create_smart in aktmoduleswitches) then
                datasegment^.concat(new(pai_symbol,initname_global(getrangecheckstring,8)))
              else
                datasegment^.concat(new(pai_symbol,initname(getrangecheckstring,8)));
              datasegment^.concat(new(pai_const,init_32bit(min)));
              datasegment^.concat(new(pai_const,init_32bit(max)));
           end;
      end;


{$ifdef GDB}
    function tenumdef.stabstring : pchar;
      var st,st2 : pchar;
          p : penumsym;
          s : string;
          memsize : word;
      begin
        memsize := memsizeinc;
        getmem(st,memsize);
        strpcopy(st,'e');
        p := firstenum;
        while assigned(p) do
          begin
            s :=p^.name+':'+tostr(p^.value)+',';
            { place for the ending ';' also }
            if (strlen(st)+length(s)+1<memsize) then
              strpcopy(strend(st),s)
            else
              begin
                getmem(st2,memsize+memsizeinc);
                strcopy(st2,st);
                freemem(st,memsize);
                st := st2;
                memsize := memsize+memsizeinc;
                strpcopy(strend(st),s);
              end;
            p := p^.nextenum;
          end;
        strpcopy(strend(st),';');
        stabstring := strnew(st);
        freemem(st,memsize);
      end;
{$endif GDB}


    procedure tenumdef.write_child_rtti_data;
      begin
         if assigned(basedef) then
           basedef^.get_rtti_label;
      end;


    procedure tenumdef.write_rtti_data;

      var
         hp : penumsym;

      begin
         rttilist^.concat(new(pai_const,init_8bit(tkEnumeration)));
         write_rtti_name;
         case savesize of
            1:
              rttilist^.concat(new(pai_const,init_8bit(otUByte)));
            2:
              rttilist^.concat(new(pai_const,init_8bit(otUWord)));
            4:
              rttilist^.concat(new(pai_const,init_8bit(otULong)));
         end;
         rttilist^.concat(new(pai_const,init_32bit(min)));
         rttilist^.concat(new(pai_const,init_32bit(max)));
         if assigned(basedef) then
           rttilist^.concat(new(pai_const_symbol,initname(basedef^.get_rtti_label)))
         else
           rttilist^.concat(new(pai_const,init_32bit(0)));
         hp:=firstenum;
         while assigned(hp) do
           begin
              rttilist^.concat(new(pai_const,init_8bit(length(hp^.name))));
              rttilist^.concat(new(pai_string,init(globals.lower(hp^.name))));
              hp:=hp^.nextenum;
           end;
         rttilist^.concat(new(pai_const,init_8bit(0)));
      end;


    function tenumdef.is_publishable : boolean;
      begin
         is_publishable:=true;
      end;

    function tenumdef.gettypename : string;

      begin
         gettypename:='<enumeration type>';
      end;

{****************************************************************************
                                 TORDDEF
****************************************************************************}

    constructor torddef.init(t : tbasetype;v,b : longint);
      begin
         inherited init;
         deftype:=orddef;
         low:=v;
         high:=b;
         typ:=t;
         rangenr:=0;
         setsize;
      end;


    constructor torddef.load;
      begin
         inherited load;
         deftype:=orddef;
         typ:=tbasetype(readbyte);
         low:=readlong;
         high:=readlong;
         rangenr:=0;
         setsize;
      end;


    procedure torddef.setsize;
      begin
         if typ=uauto then
           begin
              { generate a unsigned range if high<0 and low>=0 }
              if (low>=0) and (high<0) then
                begin
                   savesize:=4;
                   typ:=u32bit;
                end
              else if (low>=0) and (high<=255) then
                begin
                   savesize:=1;
                   typ:=u8bit;
                end
              else if (low>=-128) and (high<=127) then
                begin
                   savesize:=1;
                   typ:=s8bit;
                end
              else if (low>=0) and (high<=65536) then
                begin
                   savesize:=2;
                   typ:=u16bit;
                end
              else if (low>=-32768) and (high<=32767) then
                begin
                   savesize:=2;
                   typ:=s16bit;
                end
              else
                begin
                   savesize:=4;
                   typ:=s32bit;
                end;
           end
         else
           begin
             case typ of
                u8bit,s8bit,
                uchar,bool8bit:
                  savesize:=1;

                u16bit,s16bit,
                bool16bit,uwidechar:
                  savesize:=2;

                s32bit,u32bit,
                bool32bit:
                  savesize:=4;

                u64bit,s64bit:
                  savesize:=8;
             else
               savesize:=0;
             end;
           end;
       { there are no entrys for range checking }
         rangenr:=0;
      end;

    function torddef.getrangecheckstring : string;

      begin
         if (cs_create_smart in aktmoduleswitches) then
           getrangecheckstring:='R_'+current_module^.modulename^+tostr(rangenr)
         else
           getrangecheckstring:='R_'+tostr(rangenr);
      end;

    procedure torddef.genrangecheck;
      var
        rangechecksize : longint;
      begin
         if rangenr=0 then
           begin
              if low<=high then
               rangechecksize:=8
              else
               rangechecksize:=16;
              { generate two constant for bounds }
              getlabelnr(rangenr);
              if (cs_create_smart in aktmoduleswitches) then
                datasegment^.concat(new(pai_symbol,initname_global(getrangecheckstring,rangechecksize)))
              else
                datasegment^.concat(new(pai_symbol,initname(getrangecheckstring,rangechecksize)));
              if low<=high then
                begin
                   datasegment^.concat(new(pai_const,init_32bit(low)));
                   datasegment^.concat(new(pai_const,init_32bit(high)));
                end
              { for u32bit we need two bounds }
              else
                begin
                   datasegment^.concat(new(pai_const,init_32bit(low)));
                   datasegment^.concat(new(pai_const,init_32bit($7fffffff)));
                   datasegment^.concat(new(pai_const,init_32bit($80000000)));
                   datasegment^.concat(new(pai_const,init_32bit(high)));
                end;
           end;
      end;


    procedure torddef.write;
      begin
         tdef.write;
         writebyte(byte(typ));
         writelong(low);
         writelong(high);
         current_ppu^.writeentry(iborddef);
      end;


{$ifdef GDB}
    function torddef.stabstring : pchar;
      begin
        case typ of
            uvoid : stabstring := strpnew(numberstring+';');
         {GDB 4.12 for go32 doesn't like boolean as range for 0 to 1 !!!}
{$ifdef Use_integer_types_for_boolean}
         bool8bit,
        bool16bit,
        bool32bit : stabstring := strpnew('r'+numberstring+';0;255;');
{$else : not Use_integer_types_for_boolean}
         bool8bit : stabstring := strpnew('-21;');
        bool16bit : stabstring := strpnew('-22;');
        bool32bit : stabstring := strpnew('-23;');
        u64bit    : stabstring := strpnew('-32;');
        s64bit    : stabstring := strpnew('-31;');
{$endif not Use_integer_types_for_boolean}
         { u32bit : stabstring := strpnew('r'+
              s32bitdef^.numberstring+';0;-1;'); }
        else
          stabstring := strpnew('r'+s32bitdef^.numberstring+';'+tostr(low)+';'+tostr(high)+';');
        end;
      end;
{$endif GDB}


    procedure torddef.write_rtti_data;
      const
        trans : array[uchar..bool8bit] of byte =
          (otUByte,otUByte,otUWord,otULong,otSByte,otSWord,otSLong,otUByte);
      begin
         case typ of
            bool8bit:
              rttilist^.concat(new(pai_const,init_8bit(tkBool)));
            uchar:
              rttilist^.concat(new(pai_const,init_8bit(tkWChar)));
            uwidechar:
              rttilist^.concat(new(pai_const,init_8bit(tkChar)));
            else
              rttilist^.concat(new(pai_const,init_8bit(tkInteger)));
         end;
         write_rtti_name;
         rttilist^.concat(new(pai_const,init_8bit(byte(trans[typ]))));
         rttilist^.concat(new(pai_const,init_32bit(low)));
         rttilist^.concat(new(pai_const,init_32bit(high)));
      end;


    function torddef.is_publishable : boolean;
      begin
         is_publishable:=typ in [uchar..bool8bit];
      end;

    function torddef.gettypename : string;

      const
        names : array[tbasetype] of string[20] = ('<unknown type>',
          'untyped','Char','Byte','Word','DWord','ShortInt',
          'SmallInt','LongInt','Boolean','WordBool',
          'LongBool','QWord','Int64','WideChar');

      begin
         gettypename:=names[typ];
      end;

{****************************************************************************
                                TFLOATDEF
****************************************************************************}

    constructor tfloatdef.init(t : tfloattype);
      begin
         inherited init;
         deftype:=floatdef;
         typ:=t;
         setsize;
      end;


    constructor tfloatdef.load;
      begin
         inherited load;
         deftype:=floatdef;
         typ:=tfloattype(readbyte);
         setsize;
      end;


    procedure tfloatdef.setsize;
      begin
         case typ of
            f16bit : savesize:=2;
            f32bit,
           s32real : savesize:=4;
           s64real : savesize:=8;
           s80real : savesize:=extended_size;
           s64comp : savesize:=8;
         else
           savesize:=0;
         end;
      end;


    procedure tfloatdef.write;
      begin
         inherited write;
         writebyte(byte(typ));
         current_ppu^.writeentry(ibfloatdef);
      end;


{$ifdef GDB}
    function tfloatdef.stabstring : pchar;
      begin
         case typ of
            s32real,
            s64real : stabstring := strpnew('r'+
               s32bitdef^.numberstring+';'+tostr(savesize)+';0;');
            { for fixed real use longint instead to be able to }
            { debug something at least                         }
            f32bit:
              stabstring := s32bitdef^.stabstring;
            f16bit:
              stabstring := strpnew('r'+s32bitdef^.numberstring+';0;'+
                tostr($ffff)+';');
            { found this solution in stabsread.c from GDB v4.16 }
            s64comp : stabstring := strpnew('r'+
               s32bitdef^.numberstring+';-'+tostr(savesize)+';0;');
{$ifdef i386}
            { under dos at least you must give a size of twelve instead of 10 !! }
            { this is probably do to the fact that in gcc all is pushed in 4 bytes size }
            s80real : stabstring := strpnew('r'+s32bitdef^.numberstring+';12;0;');
{$endif i386}
            else
              internalerror(10005);
         end;
      end;
{$endif GDB}


    procedure tfloatdef.write_rtti_data;
      const
         {tfloattype = (s32real,s64real,s80real,s64bit,f16bit,f32bit);}
         translate : array[tfloattype] of byte =
           (ftSingle,ftDouble,ftExtended,ftComp,ftFixed16,ftFixed32);
      begin
         rttilist^.concat(new(pai_const,init_8bit(tkFloat)));
         write_rtti_name;
         rttilist^.concat(new(pai_const,init_8bit(translate[typ])));
      end;


    function tfloatdef.is_publishable : boolean;
      begin
         is_publishable:=true;
      end;

    function tfloatdef.gettypename : string;

      const
        names : array[tfloattype] of string[20] = (
          'Single','Double','Extended','Comp','Fixed','Fixed16');

      begin
         gettypename:=names[typ];
      end;

{****************************************************************************
                                TFILEDEF
****************************************************************************}

    constructor tfiledef.inittext;
      begin
         inherited init;
         deftype:=filedef;
         filetyp:=ft_text;
         typedfiletype.reset;
         setsize;
      end;


    constructor tfiledef.inituntyped;
      begin
         inherited init;
         deftype:=filedef;
         filetyp:=ft_untyped;
         typedfiletype.reset;
         setsize;
      end;


    constructor tfiledef.inittyped(const tt : ttype);
      begin
         inherited init;
         deftype:=filedef;
         filetyp:=ft_typed;
         typedfiletype:=tt;
         setsize;
      end;


    constructor tfiledef.inittypeddef(p : pdef);
      begin
         inherited init;
         deftype:=filedef;
         filetyp:=ft_typed;
         typedfiletype.setdef(p);
         setsize;
      end;


    constructor tfiledef.load;
      begin
         inherited load;
         deftype:=filedef;
         filetyp:=tfiletyp(readbyte);
         if filetyp=ft_typed then
           typedfiletype.load
         else
           typedfiletype.reset;
         setsize;
      end;


    procedure tfiledef.deref;
      begin
        inherited deref;
        if filetyp=ft_typed then
          typedfiletype.resolve;
      end;


    procedure tfiledef.setsize;
      begin
        case filetyp of
          ft_text :
            savesize:=572;
          ft_typed,
          ft_untyped :
            savesize:=316;
        end;
      end;


    procedure tfiledef.write;
      begin
         inherited write;
         writebyte(byte(filetyp));
         if filetyp=ft_typed then
           typedfiletype.write;
         current_ppu^.writeentry(ibfiledef);
      end;


{$ifdef GDB}
    function tfiledef.stabstring : pchar;
      begin
   {$IfDef GDBknowsfiles}
      case filetyp of
        ft_typed :
          stabstring := strpnew('d'+typedfiletype.def^.numberstring{+';'});
        ft_untyped :
          stabstring := strpnew('d'+voiddef^.numberstring{+';'});
        ft_text :
          stabstring := strpnew('d'+cchardef^.numberstring{+';'});
      end;
   {$Else}
      {based on
        FileRec = Packed Record
          Handle,
          Mode,
          RecSize   : longint;
          _private  : array[1..32] of byte;
          UserData  : array[1..16] of byte;
          name      : array[0..255] of char;
        End; }
      { the buffer part is still missing !! (PM) }
      { but the string could become too long !! }
      stabstring := strpnew('s'+tostr(savesize)+
                     'HANDLE:'+typeglobalnumber('longint')+',0,32;'+
                     'MODE:'+typeglobalnumber('longint')+',32,32;'+
                     'RECSIZE:'+typeglobalnumber('longint')+',64,32;'+
                     '_PRIVATE:ar'+typeglobalnumber('word')+';1;32;'+typeglobalnumber('byte')
                        +',96,256;'+
                     'USERDATA:ar'+typeglobalnumber('word')+';1;16;'+typeglobalnumber('byte')
                        +',352,128;'+
                     'NAME:ar'+typeglobalnumber('word')+';0;255;'+typeglobalnumber('char')
                        +',480,2048;;');
   {$EndIf}
      end;


    procedure tfiledef.concatstabto(asmlist : paasmoutput);
      begin
      { most file defs are unnamed !!! }
      if ((typesym = nil) or typesym^.isusedinstab or (cs_gdb_dbx in aktglobalswitches)) and
         not is_def_stab_written then
        begin
        if assigned(typedfiletype.def) then forcestabto(asmlist,typedfiletype.def);
        inherited concatstabto(asmlist);
        end;
      end;
{$endif GDB}

    function tfiledef.gettypename : string;

      begin
         case filetyp of
           ft_untyped:
             gettypename:='File';
           ft_typed:
             gettypename:='File Of '+typedfiletype.def^.typename;
           ft_text:
             gettypename:='Text'
         end;
      end;



{****************************************************************************
                               TPOINTERDEF
****************************************************************************}

    constructor tpointerdef.init(const tt : ttype);
      begin
        tdef.init;
        deftype:=pointerdef;
        pointertype:=tt;
        is_far:=false;
        savesize:=target_os.size_of_pointer;
      end;


    constructor tpointerdef.initfar(const tt : ttype);
      begin
        tdef.init;
        deftype:=pointerdef;
        pointertype:=tt;
        is_far:=true;
        savesize:=target_os.size_of_pointer;
      end;


    constructor tpointerdef.initdef(p : pdef);
      var
        t : ttype;
      begin
        t.setdef(p);
        tpointerdef.init(t);
      end;


    constructor tpointerdef.initfardef(p : pdef);
      var
        t : ttype;
      begin
        t.setdef(p);
        tpointerdef.initfar(t);
      end;



    constructor tpointerdef.load;
      begin
         tdef.load;
         deftype:=pointerdef;
         pointertype.load;
         is_far:=(readbyte<>0);
         savesize:=target_os.size_of_pointer;
      end;


    destructor tpointerdef.done;
      begin
        if assigned(pointertype.def) and
           (pointertype.def^.deftype=forwarddef) then
         begin
           dispose(pointertype.def,done);
           pointertype.reset;
         end;
        inherited done;
      end;


    procedure tpointerdef.deref;
      begin
        inherited deref;
        pointertype.resolve;
      end;


    procedure tpointerdef.write;
      begin
         inherited write;
         pointertype.write;
         writebyte(byte(is_far));
         current_ppu^.writeentry(ibpointerdef);
      end;


{$ifdef GDB}
    function tpointerdef.stabstring : pchar;
      begin
        stabstring := strpnew('*'+pointertype.def^.numberstring);
      end;


    procedure tpointerdef.concatstabto(asmlist : paasmoutput);
      var st,nb : string;
          sym_line_no : longint;
      begin
      if assigned(pointertype.def) and
         (pointertype.def^.deftype=forwarddef) then
        exit;

      if ( (typesym=nil) or typesym^.isusedinstab or (cs_gdb_dbx in aktglobalswitches)) and
         not is_def_stab_written then
        begin
        if assigned(pointertype.def) then
          if pointertype.def^.deftype in [recorddef,objectdef] then
            begin
            is_def_stab_written := true;
            nb:=pointertype.def^.numberstring;
            {to avoid infinite recursion in record with next-like fields }
            is_def_stab_written := false;
            if not pointertype.def^.is_def_stab_written then
              begin
              if assigned(pointertype.def^.typesym) then
                begin
                if assigned(typesym) then
                  begin
                     st := typesym^.name;
                     sym_line_no:=typesym^.fileinfo.line;
                  end
                else
                  begin
                     st := ' ';
                     sym_line_no:=0;
                  end;
                st := '"'+st+':t'+numberstring+'=*'+nb
                      +'=xs'+pointertype.def^.typesym^.name+':",'+tostr(N_LSYM)+',0,'+tostr(sym_line_no)+',0';
                asmlist^.concat(new(pai_stabs,init(strpnew(st))));
                end;
              end else inherited concatstabto(asmlist);
            is_def_stab_written := true;
            end else
            begin
            { p =^p1; p1=^p problem }
            is_def_stab_written := true;
            forcestabto(asmlist,pointertype.def);
            is_def_stab_written := false;
            inherited concatstabto(asmlist);
            end;
        end;
      end;
{$endif GDB}

    function tpointerdef.gettypename : string;

      begin
         gettypename:='^'+pointertype.def^.typename;
      end;

{****************************************************************************
                              TCLASSREFDEF
****************************************************************************}

    constructor tclassrefdef.init(def : pdef);
      begin
         inherited initdef(def);
         deftype:=classrefdef;
      end;


    constructor tclassrefdef.load;
      begin
         { be careful, tclassdefref inherits from tpointerdef }
         tdef.load;
         deftype:=classrefdef;
         pointertype.load;
         is_far:=false;
         savesize:=target_os.size_of_pointer;
      end;


    procedure tclassrefdef.write;
      begin
         { be careful, tclassdefref inherits from tpointerdef }
         tdef.write;
         pointertype.write;
         current_ppu^.writeentry(ibclassrefdef);
      end;


{$ifdef GDB}
    function tclassrefdef.stabstring : pchar;
      begin
         stabstring:=strpnew(pvmtdef^.numberstring+';');
      end;


    procedure tclassrefdef.concatstabto(asmlist : paasmoutput);
      begin
        inherited concatstabto(asmlist);
      end;
{$endif GDB}

    function tclassrefdef.gettypename : string;

      begin
         gettypename:='Class Of '+pointertype.def^.typename;
      end;


{***************************************************************************
                                   TSETDEF
***************************************************************************}

{ For i386 smallsets work,
  for m68k there are problems
  can be test by compiling with -dusesmallset PM }
{$ifdef i386}
{$define usesmallset}
{$endif i386}

    constructor tsetdef.init(s : pdef;high : longint);
      begin
         inherited init;
         deftype:=setdef;
         elementtype.setdef(s);
{$ifdef usesmallset}
         { small sets only working for i386 PM }
         if high<32 then
           begin
              settype:=smallset;
              savesize:=Sizeof(longint);
           end
         else
{$endif usesmallset}
         if high<256 then
           begin
              settype:=normset;
              savesize:=32;
           end
         else
{$ifdef testvarsets}
         if high<$10000 then
           begin
              settype:=varset;
              savesize:=4*((high+31) div 32);
           end
         else
{$endif testvarsets}
          Message(sym_e_ill_type_decl_set);
      end;


    constructor tsetdef.load;
      begin
         inherited load;
         deftype:=setdef;
         elementtype.load;
         settype:=tsettype(readbyte);
         case settype of
            normset : savesize:=32;
            varset : savesize:=readlong;
            smallset : savesize:=Sizeof(longint);
         end;
      end;


    destructor tsetdef.done;
      begin
        inherited done;
      end;


    procedure tsetdef.write;
      begin
         inherited write;
         elementtype.write;
         writebyte(byte(settype));
         if settype=varset then
           writelong(savesize);
         current_ppu^.writeentry(ibsetdef);
      end;


{$ifdef GDB}
    function tsetdef.stabstring : pchar;
      begin
         { For small sets write a longint, which can at least be seen
           in the current GDB's (PFV)
           this is obsolete with GDBPAS !!
           and anyhow creates problems with version 4.18!! PM
         if settype=smallset then
           stabstring := strpnew('r'+s32bitdef^.numberstring+';0;0xffffffff;')
         else }
           stabstring := strpnew('S'+elementtype.def^.numberstring);
      end;


    procedure tsetdef.concatstabto(asmlist : paasmoutput);
      begin
      if ( not assigned(typesym) or typesym^.isusedinstab or (cs_gdb_dbx in aktglobalswitches)) and
          not is_def_stab_written then
        begin
          if assigned(elementtype.def) then
            forcestabto(asmlist,elementtype.def);
          inherited concatstabto(asmlist);
        end;
      end;
{$endif GDB}


    procedure tsetdef.deref;
      begin
        inherited deref;
        elementtype.resolve;
      end;


    procedure tsetdef.write_rtti_data;
      begin
         rttilist^.concat(new(pai_const,init_8bit(tkSet)));
         write_rtti_name;
         rttilist^.concat(new(pai_const,init_8bit(otULong)));
         rttilist^.concat(new(pai_const_symbol,initname(elementtype.def^.get_rtti_label)));
      end;


    procedure tsetdef.write_child_rtti_data;
      begin
         elementtype.def^.get_rtti_label;
      end;


    function tsetdef.is_publishable : boolean;
      begin
         is_publishable:=settype=smallset;
      end;

    function tsetdef.gettypename : string;

      begin
         gettypename:='Set Of '+elementtype.def^.typename;
      end;


{***************************************************************************
                                 TFORMALDEF
***************************************************************************}

    constructor tformaldef.init;
      var
         stregdef : boolean;
      begin
         stregdef:=registerdef;
         registerdef:=false;
         inherited init;
         deftype:=formaldef;
         registerdef:=stregdef;
         { formaldef must be registered at unit level !! }
         if registerdef and assigned(current_module) then
            if assigned(current_module^.localsymtable) then
              psymtable(current_module^.localsymtable)^.registerdef(@self)
            else if assigned(current_module^.globalsymtable) then
              psymtable(current_module^.globalsymtable)^.registerdef(@self);
         savesize:=target_os.size_of_pointer;
      end;


    constructor tformaldef.load;
      begin
         inherited load;
         deftype:=formaldef;
         savesize:=target_os.size_of_pointer;
      end;


    procedure tformaldef.write;
      begin
         inherited write;
         current_ppu^.writeentry(ibformaldef);
      end;


{$ifdef GDB}
    function tformaldef.stabstring : pchar;
      begin
      stabstring := strpnew('formal'+numberstring+';');
      end;


    procedure tformaldef.concatstabto(asmlist : paasmoutput);
      begin
      { formaldef can't be stab'ed !}
      end;
{$endif GDB}

    function tformaldef.gettypename : string;

      begin
         gettypename:='Var';
      end;

{***************************************************************************
                           TARRAYDEF
***************************************************************************}

    constructor tarraydef.init(l,h : longint;rd : pdef);
      begin
         inherited init;
         deftype:=arraydef;
         lowrange:=l;
         highrange:=h;
         rangetype.setdef(rd);
         elementtype.reset;
         IsVariant:=false;
         IsConstructor:=false;
         IsArrayOfConst:=false;
         rangenr:=0;
      end;


    constructor tarraydef.load;
      begin
         inherited load;
         deftype:=arraydef;
         { the addresses are calculated later }
         elementtype.load;
         rangetype.load;
         lowrange:=readlong;
         highrange:=readlong;
         IsArrayOfConst:=boolean(readbyte);
         IsVariant:=false;
         IsConstructor:=false;
         rangenr:=0;
      end;


    function tarraydef.getrangecheckstring : string;
      begin
         if (cs_create_smart in aktmoduleswitches) then
           getrangecheckstring:='R_'+current_module^.modulename^+tostr(rangenr)
         else
           getrangecheckstring:='R_'+tostr(rangenr);
      end;


    procedure tarraydef.genrangecheck;
      begin
         if rangenr=0 then
           begin
              { generates the data for range checking }
              getlabelnr(rangenr);
              if (cs_create_smart in aktmoduleswitches) then
                datasegment^.concat(new(pai_symbol,initname_global(getrangecheckstring,8)))
              else
                datasegment^.concat(new(pai_symbol,initname(getrangecheckstring,8)));
              datasegment^.concat(new(pai_const,init_32bit(lowrange)));
              datasegment^.concat(new(pai_const,init_32bit(highrange)));
           end;
      end;


    procedure tarraydef.deref;
      begin
        inherited deref;
        elementtype.resolve;
        rangetype.resolve;
      end;


    procedure tarraydef.write;
      begin
         inherited write;
         elementtype.write;
         rangetype.write;
         writelong(lowrange);
         writelong(highrange);
         writebyte(byte(IsArrayOfConst));
         current_ppu^.writeentry(ibarraydef);
      end;


{$ifdef GDB}
    function tarraydef.stabstring : pchar;
      begin
      stabstring := strpnew('ar'+rangetype.def^.numberstring+';'
                    +tostr(lowrange)+';'+tostr(highrange)+';'+elementtype.def^.numberstring);
      end;


    procedure tarraydef.concatstabto(asmlist : paasmoutput);
      begin
      if (not assigned(typesym) or typesym^.isusedinstab or (cs_gdb_dbx in aktglobalswitches))
        and not is_def_stab_written then
        begin
        {when array are inserted they have no definition yet !!}
        if assigned(elementtype.def) then
          inherited concatstabto(asmlist);
        end;
      end;
{$endif GDB}


    function tarraydef.elesize : longint;
      begin
         elesize:=elementtype.def^.size;
      end;


    function tarraydef.size : longint;
      begin
        {Tarraydef.size may never be called for an open array!}
        if highrange<lowrange then
            internalerror(99080501);
        If (elesize>0) and
           (
            (highrange-lowrange = $7fffffff) or
            { () are needed around elesize-1 to avoid a possible
              integer overflow for elesize=1 !! PM }
            (($7fffffff div elesize + (elesize -1)) < (highrange - lowrange))
           ) Then
          Begin
            Message(sym_e_segment_too_large);
            size := 4
          End
        Else size:=(highrange-lowrange+1)*elesize;
      end;


    function tarraydef.alignment : longint;
      begin
         { alignment is the size of the elements }
         alignment:=elementtype.def^.size;
      end;


    function tarraydef.needs_inittable : boolean;
      begin
         needs_inittable:=elementtype.def^.needs_inittable;
      end;


    procedure tarraydef.write_child_rtti_data;
      begin
         elementtype.def^.get_rtti_label;
      end;


    procedure tarraydef.write_rtti_data;
      begin
         rttilist^.concat(new(pai_const,init_8bit(tkarray)));
         write_rtti_name;
         { size of elements }
         rttilist^.concat(new(pai_const,init_32bit(elementtype.def^.size)));
         { count of elements }
         rttilist^.concat(new(pai_const,init_32bit(highrange-lowrange+1)));
         { element type }
         rttilist^.concat(new(pai_const_symbol,initname(elementtype.def^.get_rtti_label)));
      end;

    function tarraydef.gettypename : string;

      begin
         if isarrayofconst or isConstructor then
           begin
             if isvariant then
               gettypename:='Array Of Const'
             else
               gettypename:='Array Of '+elementtype.def^.typename;
           end
         else if is_open_array(@self) then
           gettypename:='Array Of '+elementtype.def^.typename
         else
           begin
              if rangetype.def^.deftype=enumdef then
                gettypename:='Array['+rangetype.def^.typename+'] Of '+elementtype.def^.typename
              else
                gettypename:='Array['+tostr(lowrange)+'..'+
                  tostr(highrange)+'] Of '+elementtype.def^.typename
           end;
      end;

{***************************************************************************
                                  trecorddef
***************************************************************************}

    constructor trecorddef.init(p : psymtable);
      begin
         inherited init;
         deftype:=recorddef;
         symtable:=p;
         symtable^.defowner := @self;
         symtable^.dataalignment:=packrecordalignment[aktpackrecords];
      end;


    constructor trecorddef.load;
      var
         oldread_member : boolean;
      begin
         inherited load;
         deftype:=recorddef;
         savesize:=readlong;
         oldread_member:=read_member;
         read_member:=true;
         symtable:=new(psymtable,loadas(recordsymtable));
         read_member:=oldread_member;
         symtable^.defowner := @self;
      end;


    destructor trecorddef.done;
      begin
         if assigned(symtable) then
           dispose(symtable,done);
         inherited done;
      end;


    var
       binittable : boolean;

    procedure check_rec_inittable(s : pnamedindexobject);

      begin
         if (not binittable) and
            (psym(s)^.typ=varsym) and
            assigned(pvarsym(s)^.vartype.def) then
          begin
            if ((pvarsym(s)^.vartype.def^.deftype<>objectdef) or
               not(pobjectdef(pvarsym(s)^.vartype.def)^.is_class)) then
             binittable:=pvarsym(s)^.vartype.def^.needs_inittable;
          end;
      end;


    function trecorddef.needs_inittable : boolean;
      var
         oldb : boolean;
      begin
         { there are recursive calls to needs_rtti possible, }
         { so we have to change to old value how else should }
         { we do that ? check_rec_rtti can't be a nested     }
         { procedure of needs_rtti !                         }
         oldb:=binittable;
         binittable:=false;
         symtable^.foreach({$ifndef TP}@{$endif}check_rec_inittable);
         needs_inittable:=binittable;
         binittable:=oldb;
      end;


    procedure trecorddef.deref;
      var
         oldrecsyms : psymtable;
      begin
         inherited deref;
         oldrecsyms:=aktrecordsymtable;
         aktrecordsymtable:=symtable;
         { now dereference the definitions }
         symtable^.deref;
         aktrecordsymtable:=oldrecsyms;
      end;


    procedure trecorddef.write;
      var
         oldread_member : boolean;
      begin
         oldread_member:=read_member;
         read_member:=true;
         inherited write;
         writelong(savesize);
         current_ppu^.writeentry(ibrecorddef);
         self.symtable^.writeas;
         read_member:=oldread_member;
      end;

    function trecorddef.size:longint;
      begin
        size:=symtable^.datasize;
      end;


    function trecorddef.alignment:longint;
      begin
        alignment:=symtable^.dataalignment;
      end;

{$ifdef GDB}
    Const StabRecString : pchar = Nil;
          StabRecSize : longint = 0;
          RecOffset : Longint = 0;

    procedure addname(p : pnamedindexobject);
    var
      news, newrec : pchar;
      spec : string[3];
      size : longint;
    begin
    { static variables from objects are like global objects }
    if (sp_static in psym(p)^.symoptions) then
      exit;
    If psym(p)^.typ = varsym then
       begin
         if (sp_protected in psym(p)^.symoptions) then
           spec:='/1'
         else if (sp_private in psym(p)^.symoptions) then
           spec:='/0'
         else
           spec:='';
         { class fields are pointers PM }
         if not assigned(pvarsym(p)^.vartype.def) then
          writeln(pvarsym(p)^.name);
         if (pvarsym(p)^.vartype.def^.deftype=objectdef) and
            pobjectdef(pvarsym(p)^.vartype.def)^.is_class then
            spec:=spec+'*';
         size:=pvarsym(p)^.vartype.def^.size;
         { open arrays made overflows !! }
         if size>$fffffff then
           size:=$fffffff;
         newrec := strpnew(p^.name+':'+spec+pvarsym(p)^.vartype.def^.numberstring
                       +','+tostr(pvarsym(p)^.address*8)+','
                       +tostr(size*8)+';');
         if strlen(StabRecString) + strlen(newrec) >= StabRecSize-256 then
           begin
              getmem(news,stabrecsize+memsizeinc);
              strcopy(news,stabrecstring);
              freemem(stabrecstring,stabrecsize);
              stabrecsize:=stabrecsize+memsizeinc;
              stabrecstring:=news;
           end;
         strcat(StabRecstring,newrec);
         strdispose(newrec);
         {This should be used for case !!}
         RecOffset := RecOffset + pvarsym(p)^.vartype.def^.size;
       end;
    end;


    function trecorddef.stabstring : pchar;
      Var oldrec : pchar;
          oldsize : longint;
      begin
        oldrec := stabrecstring;
        oldsize:=stabrecsize;
        GetMem(stabrecstring,memsizeinc);
        stabrecsize:=memsizeinc;
        strpcopy(stabRecString,'s'+tostr(size));
        RecOffset := 0;
        symtable^.foreach({$ifndef TP}@{$endif}addname);
        { FPC doesn't want to convert a char to a pchar}
        { is this a bug ? }
        strpcopy(strend(StabRecString),';');
        stabstring := strnew(StabRecString);
        Freemem(stabrecstring,stabrecsize);
        stabrecstring := oldrec;
        stabrecsize:=oldsize;
      end;


    procedure trecorddef.concatstabto(asmlist : paasmoutput);
      begin
        if (not assigned(typesym) or typesym^.isusedinstab or (cs_gdb_dbx in aktglobalswitches)) and
           (not is_def_stab_written) then
          inherited concatstabto(asmlist);
      end;

{$endif GDB}

    var
       count : longint;

    procedure count_inittable_fields(sym : pnamedindexobject);{$ifndef fpc}far;{$endif}
      begin
         if ((psym(sym)^.typ=varsym) and
            pvarsym(sym)^.vartype.def^.needs_inittable)
            and ((pvarsym(sym)^.vartype.def^.deftype<>objectdef) or
                  (not pobjectdef(pvarsym(sym)^.vartype.def)^.is_class)) then
           inc(count);
      end;


    procedure count_fields(sym : pnamedindexobject);{$ifndef fpc}far;{$endif}
      begin
            inc(count);
      end;


    procedure write_field_inittable(sym : pnamedindexobject);{$ifndef fpc}far;{$endif}
      begin
         if ((psym(sym)^.typ=varsym) and
            pvarsym(sym)^.vartype.def^.needs_inittable) and
            ((pvarsym(sym)^.vartype.def^.deftype<>objectdef) or
             (not pobjectdef(pvarsym(sym)^.vartype.def)^.is_class)) then
           begin
              rttilist^.concat(new(pai_const_symbol,init(pvarsym(sym)^.vartype.def^.get_inittable_label)));
              rttilist^.concat(new(pai_const,init_32bit(pvarsym(sym)^.address)));
           end;
      end;


    procedure write_field_rtti(sym : pnamedindexobject);{$ifndef fpc}far;{$endif}
      begin
         rttilist^.concat(new(pai_const_symbol,initname(pvarsym(sym)^.vartype.def^.get_rtti_label)));
         rttilist^.concat(new(pai_const,init_32bit(pvarsym(sym)^.address)));
      end;


    procedure generate_child_inittable(sym:pnamedindexobject);{$ifndef fpc}far;{$endif}
      begin
         if (psym(sym)^.typ=varsym) and
            pvarsym(sym)^.vartype.def^.needs_inittable then
         { force inittable generation }
           pvarsym(sym)^.vartype.def^.get_inittable_label;
      end;


    procedure generate_child_rtti(sym : pnamedindexobject);{$ifndef fpc}far;{$endif}
      begin
         pvarsym(sym)^.vartype.def^.get_rtti_label;
      end;


    procedure trecorddef.write_child_rtti_data;
      begin
         symtable^.foreach({$ifndef TP}@{$endif}generate_child_rtti);
      end;


    procedure trecorddef.write_child_init_data;
      begin
         symtable^.foreach({$ifndef TP}@{$endif}generate_child_inittable);
      end;


    procedure trecorddef.write_rtti_data;
      begin
         rttilist^.concat(new(pai_const,init_8bit(tkrecord)));
         write_rtti_name;
         rttilist^.concat(new(pai_const,init_32bit(size)));
         count:=0;
         symtable^.foreach({$ifndef TP}@{$endif}count_fields);
         rttilist^.concat(new(pai_const,init_32bit(count)));
         symtable^.foreach({$ifndef TP}@{$endif}write_field_rtti);
      end;


    procedure trecorddef.write_init_data;
      begin
         rttilist^.concat(new(pai_const,init_8bit(14)));
         write_rtti_name;
         rttilist^.concat(new(pai_const,init_32bit(size)));
         count:=0;
         symtable^.foreach({$ifndef TP}@{$endif}count_inittable_fields);
         rttilist^.concat(new(pai_const,init_32bit(count)));
         symtable^.foreach({$ifndef TP}@{$endif}write_field_inittable);
      end;

    function trecorddef.gettypename : string;

      begin
         gettypename:='<record type>'
      end;


{***************************************************************************
                       TABSTRACTPROCDEF
***************************************************************************}

    constructor tabstractprocdef.init;
      begin
         inherited init;
         new(para,init);
         fpu_used:=0;
         proctypeoption:=potype_none;
         proccalloptions:=[];
         procoptions:=[];
         rettype.setdef(voiddef);
         symtablelevel:=0;
         savesize:=target_os.size_of_pointer;
      end;


    destructor tabstractprocdef.done;
      begin
         dispose(para,done);
         inherited done;
      end;


    procedure tabstractprocdef.concatpara(tt:ttype;vsp : tvarspez);
      var
        hp : pparaitem;
      begin
        new(hp,init);
        hp^.paratyp:=vsp;
        hp^.paratype:=tt;
        hp^.register:=R_NO;
        para^.insert(hp);
      end;


    { all functions returning in FPU are
      assume to use 2 FPU registers
      until the function implementation
      is processed   PM }
    procedure tabstractprocdef.test_if_fpu_result;
      begin
         if assigned(rettype.def) and is_fpu(rettype.def) then
           fpu_used:=2;
      end;


    procedure tabstractprocdef.deref;
      var
         hp : pparaitem;
      begin
         inherited deref;
         rettype.resolve;
         hp:=pparaitem(para^.first);
         while assigned(hp) do
          begin
            hp^.paratype.resolve;
            hp:=pparaitem(hp^.next);
          end;
      end;


    constructor tabstractprocdef.load;
      var
         hp : pparaitem;
         count,i : word;
      begin
         inherited load;
         new(para,init);
         rettype.load;
         fpu_used:=readbyte;
         proctypeoption:=tproctypeoption(readlong);
         readsmallset(proccalloptions);
         readsmallset(procoptions);
         count:=readword;
         savesize:=target_os.size_of_pointer;
         for i:=1 to count do
          begin
            new(hp,init);
            hp^.paratyp:=tvarspez(readbyte);
            { hp^.register:=tregister(readbyte); }
            hp^.register:=R_NO;
            hp^.paratype.load;
            para^.concat(hp);
          end;
      end;


    procedure tabstractprocdef.write;
      var
        hp : pparaitem;
      begin
         inherited write;
         rettype.write;
         current_ppu^.do_interface_crc:=false;
         writebyte(fpu_used);
         writelong(ord(proctypeoption));
         writesmallset(proccalloptions);
         writesmallset(procoptions);
         writeword(para^.count);
         hp:=pparaitem(para^.first);
         while assigned(hp) do
          begin
            writebyte(byte(hp^.paratyp));
            { writebyte(byte(hp^.register)); }
            hp^.paratype.write;
            hp:=pparaitem(hp^.next);
          end;
      end;


    function tabstractprocdef.para_size(alignsize:longint) : longint;
      var
         pdc : pparaitem;
         l : longint;
      begin
         l:=0;
         pdc:=pparaitem(para^.first);
         while assigned(pdc) do
          begin
            case pdc^.paratyp of
              vs_var   : inc(l,target_os.size_of_pointer);
              vs_value,
              vs_const : if push_addr_param(pdc^.paratype.def) then
                          inc(l,target_os.size_of_pointer)
                         else
                          inc(l,pdc^.paratype.def^.size);
            end;
            l:=align(l,alignsize);
            pdc:=pparaitem(pdc^.next);
          end;
         para_size:=l;
      end;


    function tabstractprocdef.demangled_paras : string;
      var
        s : string;
        hp : pparaitem;
      begin
        s:='(';
        hp:=pparaitem(para^.last);
        while assigned(hp) do
         begin
           if assigned(hp^.paratype.def^.typesym) then
             s:=s+hp^.paratype.def^.typesym^.name
           else if hp^.paratyp=vs_var then
             s:=s+'var'
           else if hp^.paratyp=vs_const then
             s:=s+'const';
           hp:=pparaitem(hp^.previous);
           if assigned(hp) then
            s:=s+',';
         end;
        s:=s+')';
        demangled_paras:=s;
      end;


    function tabstractprocdef.proccalloption2str : string;
      type
        tproccallopt=record
          mask : tproccalloption;
          str  : string[30];
        end;
      const
        proccallopts=12;
        proccallopt : array[1..proccallopts] of tproccallopt=(
           (mask:pocall_none;         str:''),
           (mask:pocall_clearstack;   str:'ClearStack'),
           (mask:pocall_leftright;    str:'LeftRight'),
           (mask:pocall_cdecl;        str:'Cdecl'),
           (mask:pocall_register;     str:'Register'),
           (mask:pocall_stdcall;      str:'StdCall'),
           (mask:pocall_safecall;     str:'SafeCall'),
           (mask:pocall_palmossyscall;str:'PalmOSSysCall'),
           (mask:pocall_system;       str:'System'),
           (mask:pocall_inline;       str:'Inline'),
           (mask:pocall_internproc;   str:'InternProc'),
           (mask:pocall_internconst;  str:'InternConst')
        );
      var
        s : string;
        i : longint;
        first : boolean;
      begin
        s:='';
        first:=true;
        for i:=1to proccallopts do
         if (proccallopt[i].mask in proccalloptions) then
          begin
            if first then
              first:=false
            else
              s:=s+';';
            s:=s+proccallopt[i].str;
          end;
        proccalloption2str:=s;
      end;


{$ifdef GDB}
    function tabstractprocdef.stabstring : pchar;
      begin
        stabstring := strpnew('abstractproc'+numberstring+';');
      end;


    procedure tabstractprocdef.concatstabto(asmlist : paasmoutput);
      begin
         if (not assigned(typesym) or typesym^.isusedinstab or (cs_gdb_dbx in aktglobalswitches))
            and not is_def_stab_written then
           begin
              if assigned(rettype.def) then forcestabto(asmlist,rettype.def);
              inherited concatstabto(asmlist);
           end;
      end;
{$endif GDB}


{***************************************************************************
                                  TPROCDEF
***************************************************************************}

    constructor tprocdef.init;
      begin
         inherited init;
         deftype:=procdef;
         _mangledname:=nil;
         nextoverloaded:=nil;
         fileinfo:=aktfilepos;
         extnumber:=-1;
         localst:=new(psymtable,init(localsymtable));
         parast:=new(psymtable,init(parasymtable));
         localst^.defowner:=@self;
         parast^.defowner:=@self;
         { this is used by insert
          to check same names in parast and localst }
         localst^.next:=parast;
         defref:=nil;
         crossref:=nil;
         lastwritten:=nil;
         refcount:=0;
         if (cs_browser in aktmoduleswitches) and make_ref then
          begin
            defref:=new(pref,init(defref,@tokenpos));
            inc(refcount);
          end;
         lastref:=defref;
       { first, we assume that all registers are used }
{$ifdef newcg}
         usedregisters:=[firstreg..lastreg];
{$else newcg}
{$ifdef i386}
         usedregisters:=$ff;
{$endif i386}
{$ifdef m68k}
         usedregisters:=$FFFF;
{$endif}
{$endif newcg}
         forwarddef:=true;
         interfacedef:=false;
         _class := nil;
         code:=nil;
         count:=false;
         is_used:=false;
      end;


    constructor tprocdef.load;
      var
         s : string;
      begin
         inherited load;
         deftype:=procdef;

{$ifdef newcg}
         readnormalset(usedregisters);
{$else newcg}
{$ifdef i386}
         usedregisters:=readbyte;
{$endif i386}
{$ifdef m68k}
         usedregisters:=readword;
{$endif}
{$endif newcg}
         s:=readstring;
         setstring(_mangledname,s);

         extnumber:=readlong;
         nextoverloaded:=pprocdef(readdefref);
         _class := pobjectdef(readdefref);
         readposinfo(fileinfo);

         if (cs_link_deffile in aktglobalswitches) and
            (tf_need_export in target_info.flags) and
            (po_exports in procoptions) then
           deffile.AddExport(mangledname);

         parast:=nil;
         localst:=nil;
         forwarddef:=false;
         interfacedef:=false;
         lastref:=nil;
         lastwritten:=nil;
         defref:=nil;
         refcount:=0;
         count:=true;
         is_used:=false;
      end;


Const local_symtable_index : longint = $8001;

    procedure tprocdef.load_references;
      var
        pos : tfileposinfo;
{$ifndef NOLOCALBROWSER}
        oldsymtablestack,
        st : psymtable;
{$endif ndef NOLOCALBROWSER}
        move_last : boolean;
      begin
        move_last:=lastwritten=lastref;
        while (not current_ppu^.endofentry) do
         begin
           readposinfo(pos);
           inc(refcount);
           lastref:=new(pref,init(lastref,@pos));
           lastref^.is_written:=true;
           if refcount=1 then
            defref:=lastref;
         end;
        if move_last then
          lastwritten:=lastref;
        if ((current_module^.flags and uf_local_browser)<>0)
           and is_in_current then
          begin
{$ifndef NOLOCALBROWSER}
             oldsymtablestack:=symtablestack;
             st:=aktlocalsymtable;
             new(parast,loadas(parasymtable));
             parast^.defowner:=@self;
             aktlocalsymtable:=parast;
             parast^.deref;
             parast^.next:=owner;
             parast^.load_browser;
             aktlocalsymtable:=st;
             new(localst,loadas(localsymtable));
             localst^.defowner:=@self;
             aktlocalsymtable:=localst;
             symtablestack:=parast;
             localst^.deref;
             localst^.next:=parast;
             localst^.load_browser;
             aktlocalsymtable:=st;
             symtablestack:=oldsymtablestack;
{$endif ndef NOLOCALBROWSER}
          end;
      end;


    function tprocdef.write_references : boolean;
      var
        ref : pref;
{$ifndef NOLOCALBROWSER}
        st : psymtable;
        pdo : pobjectdef;
{$endif ndef NOLOCALBROWSER}
        move_last : boolean;
      begin
        move_last:=lastwritten=lastref;
        if move_last and (((current_module^.flags and uf_local_browser)=0)
           or not is_in_current) then
          exit;
      { write address of this symbol }
        writedefref(@self);
      { write refs }
        if assigned(lastwritten) then
          ref:=lastwritten
        else
          ref:=defref;
        while assigned(ref) do
         begin
           if ref^.moduleindex=current_module^.unit_index then
             begin
                writeposinfo(ref^.posinfo);
                ref^.is_written:=true;
                if move_last then
                  lastwritten:=ref;
             end
           else if not ref^.is_written then
             move_last:=false
           else if move_last then
             lastwritten:=ref;
           ref:=ref^.nextref;
         end;
        current_ppu^.writeentry(ibdefref);
        write_references:=true;
        if ((current_module^.flags and uf_local_browser)<>0)
           and is_in_current then
          begin
{$ifndef NOLOCALBROWSER}
             pdo:=_class;
             if (owner^.symtabletype<>localsymtable) then
               while assigned(pdo) do
                 begin
                    if pdo^.symtable<>aktrecordsymtable then
                      begin
                         pdo^.symtable^.unitid:=local_symtable_index;
                         inc(local_symtable_index);
                      end;
                    pdo:=pdo^.childof;
                 end;

             { we need TESTLOCALBROWSER para and local symtables
               PPU files are then easier to read PM }
             if not assigned(parast) then
               parast:=new(psymtable,init(parasymtable));
             parast^.defowner:=@self;
             st:=aktlocalsymtable;
             aktlocalsymtable:=parast;
             parast^.writeas;
             parast^.unitid:=local_symtable_index;
             inc(local_symtable_index);
             parast^.write_browser;
             if not assigned(localst) then
               localst:=new(psymtable,init(localsymtable));
             localst^.defowner:=@self;
             aktlocalsymtable:=localst;
             localst^.writeas;
             localst^.unitid:=local_symtable_index;
             inc(local_symtable_index);
             localst^.write_browser;
             aktlocalsymtable:=st;
             { decrement for }
             local_symtable_index:=local_symtable_index-2;
             pdo:=_class;
             if (owner^.symtabletype<>localsymtable) then
               while assigned(pdo) do
                 begin
                    if pdo^.symtable<>aktrecordsymtable then
                      dec(local_symtable_index);
                    pdo:=pdo^.childof;
                 end;
{$endif ndef NOLOCALBROWSER}
          end;
      end;


{$ifdef BrowserLog}
    procedure tprocdef.add_to_browserlog;
      begin
         if assigned(defref) then
          begin
            browserlog.AddLog('***'+mangledname);
            browserlog.AddLogRefs(defref);
            if (current_module^.flags and uf_local_browser)<>0 then
              begin
                 if assigned(parast) then
                   parast^.writebrowserlog;
                 if assigned(localst) then
                   localst^.writebrowserlog;
              end;
          end;
      end;
{$endif BrowserLog}


    destructor tprocdef.done;
      begin
         if assigned(defref) then
           begin
             defref^.freechain;
             dispose(defref,done);
           end;
         if assigned(parast) then
           dispose(parast,done);
         if assigned(localst) and (localst^.symtabletype<>staticsymtable) then
           dispose(localst,done);
         if (pocall_inline in proccalloptions) and assigned(code) then
           disposetree(ptree(code));
         if (po_msgstr in procoptions) then
           strdispose(messageinf.str);
         if
{$ifdef tp}
         not(use_big) and
{$endif}
           assigned(_mangledname) then
           strdispose(_mangledname);
         inherited done;
      end;


    procedure tprocdef.write;
      begin
         inherited write;
         current_ppu^.do_interface_crc:=false;
   { set all registers to used for simplified compilation PM }
         if simplify_ppu then
           begin
{$ifdef newcg}
             usedregisters:=[firstreg..lastreg];
{$else newcg}
{$ifdef i386}
             usedregisters:=$ff;
{$endif i386}
{$ifdef m68k}
             usedregisters:=$ffff;
{$endif}
{$endif newcg}
           end;

{$ifdef newcg}
         writenormalset(usedregisters);
{$else newcg}
{$ifdef i386}
         writebyte(usedregisters);
{$endif i386}
{$ifdef m68k}
         writeword(usedregisters);
{$endif}
{$endif newcg}
         current_ppu^.do_interface_crc:=true;
         writestring(mangledname);
         writelong(extnumber);
         if (proctypeoption<>potype_operator) then
           writedefref(nextoverloaded)
         else
           begin
              { only write the overloads from the same unit }
              if assigned(nextoverloaded) and
                 (nextoverloaded^.owner=owner) then
                writedefref(nextoverloaded)
              else
                writedefref(nil);
           end;
         writedefref(_class);
         writeposinfo(fileinfo);
         if (pocall_inline in proccalloptions) then
           begin
              { we need to save
                - the para and the local symtable
                - the code ptree !! PM
               writesymtable(parast);
               writesymtable(localst);
               writeptree(ptree(code));
               }
           end;
         current_ppu^.writeentry(ibprocdef);
      end;


    function tprocdef.haspara:boolean;
      begin
        haspara:=assigned(aktprocsym^.definition^.parast^.symindex^.first);
      end;


{$ifdef GDB}
    procedure addparaname(p : psym);
      var vs : char;
      begin
      if pvarsym(p)^.varspez = vs_value then vs := '1'
        else vs := '0';
      strpcopy(strend(StabRecString),p^.name+':'+pvarsym(p)^.vartype.def^.numberstring+','+vs+';');
      end;


    function tprocdef.stabstring : pchar;
      var
          i : longint;
          oldrec : pchar;
      begin
      oldrec := stabrecstring;
      getmem(StabRecString,1024);
      strpcopy(StabRecString,'f'+rettype.def^.numberstring);
      i:=para^.count;
      if i>0 then
        begin
        strpcopy(strend(StabRecString),','+tostr(i)+';');
        (* confuse gdb !! PM
        if assigned(parast) then
          parast^.foreach({$ifndef TP}@{$endif}addparaname)
          else
          begin
          param := para1;
          i := 0;
          while assigned(param) do
            begin
            inc(i);
            if param^.paratyp = vs_value then vartyp := '1' else vartyp := '0';
            {Here we have lost the parameter names !!}
            {using lower case parameters }
            strpcopy(strend(stabrecstring),'p'+tostr(i)
               +':'+param^.paratype.def^.numberstring+','+vartyp+';');
            param := param^.next;
            end;
          end;   *)
        {strpcopy(strend(StabRecString),';');}
        end;
      stabstring := strnew(stabrecstring);
      freemem(stabrecstring,1024);
      stabrecstring := oldrec;
      end;


    procedure tprocdef.concatstabto(asmlist : paasmoutput);
      begin
      end;
{$endif GDB}


    procedure tprocdef.deref;
      begin
         inherited deref;
         resolvedef(pdef(nextoverloaded));
         resolvedef(pdef(_class));
      end;


    function tprocdef.mangledname : string;
{$ifdef tp}
      var
         oldpos : longint;
         s : string;
         b : byte;
{$endif tp}
      begin
{$ifndef Delphi}
{$ifdef tp}
         if use_big then
           begin
              symbolstream.seek(longint(_mangledname));
              symbolstream.read(b,1);
              symbolstream.read(s[1],b);
              s[0]:=chr(b);
              mangledname:=s;
           end
         else
{$endif}
{$endif Delphi}
          mangledname:=strpas(_mangledname);
         if count then
           is_used:=true;
      end;


    function tprocdef.procname: string;
      var
        s : string;
        l : longint;
      begin
         s:=mangledname;
         { delete leading $$'s }
         l:=pos('$$',s);
         while l<>0 do
           begin
              delete(s,1,l+1);
              l:=pos('$$',s);
           end;
         { delete leading _$'s }
         l:=pos('_$',s);
         while l<>0 do
           begin
              delete(s,1,l+1);
              l:=pos('_$',s);
           end;
         l:=pos('$',s);
         if l=0 then
          procname:=s
         else
          procname:=Copy(s,1,l-1);
      end;

{$IfDef GDB}
    function tprocdef.cplusplusmangledname : string;
      var
         s,s2 : string;
         param : pparaitem;
      begin
      s := typesym^.name;
      if _class <> nil then
        begin
        s2 := _class^.objname^;
        s := s+'__'+tostr(length(s2))+s2;
        end else s := s + '_';
      param := pparaitem(para^.first);
      while assigned(param) do
        begin
        s2 := param^.paratype.def^.typesym^.name;
        s := s+tostr(length(s2))+s2;
        param := pparaitem(param^.next);
        end;
      cplusplusmangledname:=s;
      end;
{$EndIf GDB}


    procedure tprocdef.setmangledname(const s : string);
      begin
         if {$ifdef tp}not(use_big) and{$endif} (assigned(_mangledname)) then
           strdispose(_mangledname);
         setstring(_mangledname,s);
         if assigned(parast) then
           begin
              stringdispose(parast^.name);
              parast^.name:=stringdup('args of '+s);
           end;
         if assigned(localst) then
           begin
              stringdispose(localst^.name);
              localst^.name:=stringdup('locals of '+s);
           end;
      end;


{***************************************************************************
                                 TPROCVARDEF
***************************************************************************}

    constructor tprocvardef.init;
      begin
         inherited init;
         deftype:=procvardef;
      end;


    constructor tprocvardef.load;
      begin
         inherited load;
         deftype:=procvardef;
      end;


    procedure tprocvardef.write;
      begin
         { here we cannot get a real good value so just give something }
         { plausible (PM) }
         { a more secure way would be
           to allways store in a temp }
         if is_fpu(rettype.def) then
           fpu_used:=2
         else
           fpu_used:=0;
         inherited write;
         current_ppu^.writeentry(ibprocvardef);
      end;


    function tprocvardef.size : longint;
      begin
         if (po_methodpointer in procoptions) then
           size:=2*target_os.size_of_pointer
         else
           size:=target_os.size_of_pointer;
      end;


{$ifdef GDB}
    function tprocvardef.stabstring : pchar;
      var
         nss : pchar;
        { i   : longint; }
      begin
        { i := para^.count; }
        getmem(nss,1024);
        { it is not a function but a function pointer !! (PM) }

        strpcopy(nss,'*f'+rettype.def^.numberstring{+','+tostr(i)}+';');
        { this confuses gdb !!
          we should use 'F' instead of 'f' but
          as we use c++ language mode
          it does not like that either
          Please do not remove this part
          might be used once
          gdb for pascal is ready PM }
        (*
        param := para1;
        i := 0;
        while assigned(param) do
          begin
          inc(i);
          if param^.paratyp = vs_value then vartyp := '1' else vartyp := '0';
          {Here we have lost the parameter names !!}
          pst := strpnew('p'+tostr(i)+':'+param^.paratype.def^.numberstring+','+vartyp+';');
          strcat(nss,pst);
          strdispose(pst);
          param := param^.next;
          end; *)
        {strpcopy(strend(nss),';');}
        stabstring := strnew(nss);
        freemem(nss,1024);
      end;


    procedure tprocvardef.concatstabto(asmlist : paasmoutput);
      begin
         if ( not assigned(typesym) or typesym^.isusedinstab or (cs_gdb_dbx in aktglobalswitches))
           and not is_def_stab_written then
           inherited concatstabto(asmlist);
         is_def_stab_written:=true;
      end;
{$endif GDB}


    procedure tprocvardef.write_rtti_data;
      var
         pdc : pparaitem;
         methodkind, paraspec : byte;
      begin
        if po_methodpointer in procoptions then
          begin
             { write method id and name }
             rttilist^.concat(new(pai_const,init_8bit(tkmethod)));
             write_rtti_name;

             { write kind of method (can only be function or procedure)}
             if rettype.def = pdef(voiddef) then    { ### typecast shoudln't be necessary! (sg) }
               methodkind := mkProcedure
             else
               methodkind := mkFunction;
             rttilist^.concat(new(pai_const,init_8bit(methodkind)));

             { get # of parameters }
             rttilist^.concat(new(pai_const,init_8bit(para^.count)));

             { write parameter info. The parameters must be written in reverse order
               if this method uses right to left parameter pushing! }
             if (pocall_leftright in proccalloptions) then
              pdc:=pparaitem(para^.last)
             else
              pdc:=pparaitem(para^.first);
             while assigned(pdc) do
               begin
                 case pdc^.paratyp of
                   vs_value: paraspec := 0;
                   vs_const: paraspec := pfConst;
                   vs_var  : paraspec := pfVar;
                 end;
                 { write flags for current parameter }
                 rttilist^.concat(new(pai_const,init_8bit(paraspec)));
                 { write name of current parameter ### how can I get this??? (sg)}
                 rttilist^.concat(new(pai_const,init_8bit(0)));

                 { write name of type of current parameter }
                 pdc^.paratype.def^.write_rtti_name;

                 if (pocall_leftright in proccalloptions) then
                  pdc:=pparaitem(pdc^.previous)
                 else
                  pdc:=pparaitem(pdc^.next);
               end;

             { write name of result type }
             rettype.def^.write_rtti_name;
          end;
      end;


    procedure tprocvardef.write_child_rtti_data;
      begin
         {!!!!!!!!}
      end;


    function tprocvardef.is_publishable : boolean;
      begin
         is_publishable:=(po_methodpointer in procoptions);
      end;


    function tprocvardef.gettypename : string;
      begin
         if assigned(rettype.def) and
            (rettype.def<>pdef(voiddef)) then
           gettypename:='<procedure variable type of function'+demangled_paras+
             ':'+rettype.def^.gettypename+';'+proccalloption2str+'>'
         else
           gettypename:='<procedure variable type of procedure'+demangled_paras+
             ';'+proccalloption2str+'>';
      end;


{***************************************************************************
                              TOBJECTDEF
***************************************************************************}

{$ifdef GDB}
    const
       vtabletype : word = 0;
       vtableassigned : boolean = false;
{$endif GDB}

   constructor tobjectdef.init(const n : string;c : pobjectdef);
     begin
        tdef.init;
        deftype:=objectdef;
        objectoptions:=[];
        childof:=nil;
        symtable:=new(psymtable,init(objectsymtable));
        symtable^.name := stringdup(n);
        { create space for vmt !! }
        vmt_offset:=0;
        symtable^.datasize:=0;
        symtable^.defowner:=@self;
        symtable^.dataalignment:=packrecordalignment[aktpackrecords];
        set_parent(c);
        objname:=stringdup(n);
     end;


    constructor tobjectdef.load;
      var
         oldread_member : boolean;
      begin
         tdef.load;
         deftype:=objectdef;
         savesize:=readlong;
         vmt_offset:=readlong;
         objname:=stringdup(readstring);
         childof:=pobjectdef(readdefref);
         readsmallset(objectoptions);
         has_rtti:=boolean(readbyte);

         oldread_member:=read_member;
         read_member:=true;
         symtable:=new(psymtable,loadas(objectsymtable));
         read_member:=oldread_member;

         symtable^.defowner:=@self;
         symtable^.name := stringdup(objname^);

         { handles the predefined class tobject  }
         { the last TOBJECT which is loaded gets }
         { it !                                  }
         if (childof=nil) and
            is_class and
            (objname^='TOBJECT') then
           class_tobject:=@self;
       end;


   destructor tobjectdef.done;
     begin
        if assigned(symtable) then
          dispose(symtable,done);
        if (oo_is_forward in objectoptions) then
          Message1(sym_e_class_forward_not_resolved,objname^);
        stringdispose(objname);
        tdef.done;
     end;


    procedure tobjectdef.write;
      var
         oldread_member : boolean;
      begin
         tdef.write;
         writelong(size);
         writelong(vmt_offset);
         writestring(objname^);
         writedefref(childof);
         writesmallset(objectoptions);
         writebyte(byte(has_rtti));
         current_ppu^.writeentry(ibobjectdef);

         oldread_member:=read_member;
         read_member:=true;
         symtable^.writeas;
         read_member:=oldread_member;
      end;


    procedure tobjectdef.deref;
      var
         oldrecsyms : psymtable;
      begin
         inherited deref;
         resolvedef(pdef(childof));
         oldrecsyms:=aktrecordsymtable;
         aktrecordsymtable:=symtable;
         symtable^.deref;
         aktrecordsymtable:=oldrecsyms;
      end;


    procedure tobjectdef.set_parent( c : pobjectdef);
      begin
        { nothing to do if the parent was not forward !}
        if assigned(childof) then
          exit;
        childof:=c;
        { some options are inherited !! }
        if assigned(c) then
          begin
             objectoptions:=objectoptions+(c^.objectoptions*
               [oo_has_virtual,oo_has_private,oo_has_protected,oo_has_constructor,oo_has_destructor]);
             { add the data of the anchestor class }
             inc(symtable^.datasize,c^.symtable^.datasize);
             if (oo_has_vmt in objectoptions) and
                (oo_has_vmt in c^.objectoptions) then
               dec(symtable^.datasize,target_os.size_of_pointer);
             { if parent has a vmt field then
               the offset is the same for the child PM }
             if (oo_has_vmt in c^.objectoptions) or is_class then
               begin
                  vmt_offset:=c^.vmt_offset;
{$ifdef INCLUDEOK}
                  include(objectoptions,oo_has_vmt);
{$else}
                  objectoptions:=objectoptions+[oo_has_vmt];
{$endif}
               end;
          end;
        savesize := symtable^.datasize;
      end;


   procedure tobjectdef.insertvmt;
     begin
        if (oo_has_vmt in objectoptions) then
          internalerror(12345)
        else
          begin
             { first round up to multiple of 4 }
             if (symtable^.dataalignment=2) then
               begin
                 if (symtable^.datasize and 1)<>0 then
                   inc(symtable^.datasize);
               end
             else
              if (symtable^.dataalignment>=4) then
               begin
                 if (symtable^.datasize mod 4) <> 0 then
                   inc(symtable^.datasize,4-(symtable^.datasize mod 4));
               end;
             vmt_offset:=symtable^.datasize;
             inc(symtable^.datasize,target_os.size_of_pointer);
{$ifdef INCLUDEOK}
             include(objectoptions,oo_has_vmt);
{$else}
             objectoptions:=objectoptions+[oo_has_vmt];
{$endif}
          end;
     end;


   procedure tobjectdef.check_forwards;
     begin
        symtable^.check_forwards;
        if (oo_is_forward in objectoptions) then
          begin
             { ok, in future, the forward can be resolved }
             Message1(sym_e_class_forward_not_resolved,objname^);
{$ifdef INCLUDEOK}
             exclude(objectoptions,oo_is_forward);
{$else}
             objectoptions:=objectoptions-[oo_is_forward];
{$endif}
          end;
     end;


   { true, if self inherits from d (or if they are equal) }
   function tobjectdef.is_related(d : pobjectdef) : boolean;
     var
        hp : pobjectdef;
     begin
        hp:=@self;
        while assigned(hp) do
          begin
             if hp=d then
               begin
                  is_related:=true;
                  exit;
               end;
             hp:=hp^.childof;
          end;
        is_related:=false;
     end;

   var
      sd : pprocdef;

   procedure _searchdestructor(sym : pnamedindexobject);{$ifndef fpc}far;{$endif}

     var
        p : pprocdef;

     begin
        { if we found already a destructor, then we exit }
        if assigned(sd) then
          exit;
        if psym(sym)^.typ=procsym then
          begin
             p:=pprocsym(sym)^.definition;
             while assigned(p) do
               begin
                  if p^.proctypeoption=potype_destructor then
                    begin
                       sd:=p;
                       exit;
                    end;
                  p:=p^.nextoverloaded;
               end;
          end;
     end;

   function tobjectdef.searchdestructor : pprocdef;

     var
        o : pobjectdef;

     begin
        searchdestructor:=nil;
        o:=@self;
        sd:=nil;
        while assigned(o) do
          begin
             symtable^.foreach({$ifndef TP}@{$endif}_searchdestructor);
             if assigned(sd) then
               begin
                  searchdestructor:=sd;
                  exit;
               end;
             o:=o^.childof;
          end;
     end;

    function tobjectdef.size : longint;
      begin
        if (oo_is_class in objectoptions) then
          size:=target_os.size_of_pointer
        else
          size:=symtable^.datasize;
      end;


    function tobjectdef.alignment:longint;
      begin
        alignment:=symtable^.dataalignment;
      end;


    function tobjectdef.vmtmethodoffset(index:longint):longint;
      begin
        { for offset of methods for classes, see rtl/inc/objpash.inc }
        if is_class then
         vmtmethodoffset:=(index+12)*target_os.size_of_pointer
        else
{$ifdef WITHDMT}
         vmtmethodoffset:=(index+4)*target_os.size_of_pointer;
{$else WITHDMT}
         vmtmethodoffset:=(index+3)*target_os.size_of_pointer;
{$endif WITHDMT}
      end;


    function tobjectdef.vmt_mangledname : string;
    {DM: I get a nil pointer on the owner name. I don't know if this
     mayhappen, and I have therefore fixed the problem by doing nil pointer
     checks.}
    var
      s1,s2:string;
    begin
        if not(oo_has_vmt in objectoptions) then
          Message1(parser_object_has_no_vmt,objname^);
        if owner^.name=nil then
          s1:=''
        else
          s1:=owner^.name^;
        if objname=nil then
          s2:=''
        else
          s2:=objname^;
        vmt_mangledname:='VMT_'+s1+'$_'+s2;
    end;


    function tobjectdef.rtti_name : string;
    var
      s1,s2:string;
    begin
       if owner^.name=nil then
         s1:=''
       else
         s1:=owner^.name^;
       if objname=nil then
         s2:=''
       else
         s2:=objname^;
       rtti_name:='RTTI_'+s1+'$_'+s2;
    end;


    function tobjectdef.is_class : boolean;
      begin
         is_class:=(oo_is_class in objectoptions);
      end;


{$ifdef GDB}
    procedure addprocname(p :pnamedindexobject);
    var virtualind,argnames : string;
        news, newrec : pchar;
        pd,ipd : pprocdef;
        lindex : longint;
        para : pparaitem;
        arglength : byte;
        sp : char;

    begin
      If psym(p)^.typ = procsym then
       begin
                pd := pprocsym(p)^.definition;
                { this will be used for full implementation of object stabs
                not yet done }
                ipd := pd;
                while assigned(ipd^.nextoverloaded) do ipd := ipd^.nextoverloaded;
                if (po_virtualmethod in pd^.procoptions) then
                   begin
                   lindex := pd^.extnumber;
                   {doesnt seem to be necessary
                   lindex := lindex or $80000000;}
                   virtualind := '*'+tostr(lindex)+';'+ipd^._class^.numberstring+';'
                   end else virtualind := '.';

                 { used by gdbpas to recognize constructor and destructors }
                 if (pd^.proctypeoption=potype_constructor) then
                   argnames:='__ct__'
                 else if (pd^.proctypeoption=potype_destructor) then
                   argnames:='__dt__'
                 else
                   argnames := '';

                { arguments are not listed here }
                {we don't need another definition}
                 para := pparaitem(pd^.para^.first);
                 while assigned(para) do
                   begin
                   if para^.paratype.def^.deftype = formaldef then
                     begin
                        if para^.paratyp=vs_var then
                          argnames := argnames+'3var'
                        else if para^.paratyp=vs_const then
                          argnames:=argnames+'5const';
                     end
                   else
                     begin
                     { if the arg definition is like (v: ^byte;..
                     there is no sym attached to data !!! }
                     if assigned(para^.paratype.def^.typesym) then
                       begin
                          arglength := length(para^.paratype.def^.typesym^.name);
                          argnames := argnames + tostr(arglength)+para^.paratype.def^.typesym^.name;
                       end
                     else
                       begin
                          argnames:=argnames+'11unnamedtype';
                       end;
                     end;
                   para := pparaitem(para^.next);
                   end;
                ipd^.is_def_stab_written := true;
                { here 2A must be changed for private and protected }
                { 0 is private 1 protected and 2 public }
                if (sp_private in psym(p)^.symoptions) then sp:='0'
                else if (sp_protected in psym(p)^.symoptions) then sp:='1'
                else sp:='2';
                newrec := strpnew(p^.name+'::'+ipd^.numberstring
                     +'=##'+pd^.rettype.def^.numberstring+';:'+argnames+';'+sp+'A'
                     +virtualind+';');
               { get spare place for a string at the end }
               if strlen(StabRecString) + strlen(newrec) >= StabRecSize-256 then
                 begin
                    getmem(news,stabrecsize+memsizeinc);
                    strcopy(news,stabrecstring);
                    freemem(stabrecstring,stabrecsize);
                    stabrecsize:=stabrecsize+memsizeinc;
                    stabrecstring:=news;
                 end;
               strcat(StabRecstring,newrec);
               {freemem(newrec,memsizeinc);    }
               strdispose(newrec);
               {This should be used for case !!}
               RecOffset := RecOffset + pd^.size;
       end;
    end;


    function tobjectdef.stabstring : pchar;
      var anc : pobjectdef;
          oldrec : pchar;
          oldrecsize : longint;
          str_end : string;
      begin
        oldrec := stabrecstring;
        oldrecsize:=stabrecsize;
        stabrecsize:=memsizeinc;
        GetMem(stabrecstring,stabrecsize);
        strpcopy(stabRecString,'s'+tostr(symtable^.datasize));
        if assigned(childof) then
          {only one ancestor not virtual, public, at base offset 0 }
          {       !1           ,    0       2         0    ,       }
          strpcopy(strend(stabrecstring),'!1,020,'+childof^.numberstring+';');
        {virtual table to implement yet}
        RecOffset := 0;
        symtable^.foreach({$ifndef TP}@{$endif}addname);
      if (oo_has_vmt in objectoptions) then
        if not assigned(childof) or not(oo_has_vmt in childof^.objectoptions) then
           begin
              strpcopy(strend(stabrecstring),'$vf'+numberstring+':'+typeglobalnumber('vtblarray')
                +','+tostr(vmt_offset*8)+';');
           end;
        symtable^.foreach({$ifndef TP}@{$endif}addprocname);
        if (oo_has_vmt in objectoptions) then
          begin
             anc := @self;
             while assigned(anc^.childof) and (oo_has_vmt in anc^.childof^.objectoptions) do
               anc := anc^.childof;
             str_end:=';~%'+anc^.numberstring+';';
          end
        else
          str_end:=';';
        strpcopy(strend(stabrecstring),str_end);
        stabstring := strnew(StabRecString);
        freemem(stabrecstring,stabrecsize);
        stabrecstring := oldrec;
        stabrecsize:=oldrecsize;
      end;
{$endif GDB}


    procedure tobjectdef.write_child_init_data;
      begin
         symtable^.foreach({$ifndef TP}@{$endif}generate_child_inittable);
      end;


    procedure tobjectdef.write_init_data;
      begin
         if is_class then
           rttilist^.concat(new(pai_const,init_8bit(tkclass)))
         else
           rttilist^.concat(new(pai_const,init_8bit(tkobject)));

         { generate the name }
         rttilist^.concat(new(pai_const,init_8bit(length(objname^))));
         rttilist^.concat(new(pai_string,init(objname^)));

         rttilist^.concat(new(pai_const,init_32bit(size)));
         count:=0;
         symtable^.foreach({$ifndef TP}@{$endif}count_inittable_fields);
         rttilist^.concat(new(pai_const,init_32bit(count)));
         symtable^.foreach({$ifndef TP}@{$endif}write_field_inittable);
      end;


    function tobjectdef.needs_inittable : boolean;
      var
         oldb : boolean;
      begin
         if is_class then
           needs_inittable:=false
         else
           begin
              { there are recursive calls to needs_inittable possible, }
              { so we have to change to old value how else should      }
              { we do that ? check_rec_rtti can't be a nested          }
              { procedure of needs_rtti !                              }
              oldb:=binittable;
              binittable:=false;
              symtable^.foreach({$ifndef TP}@{$endif}check_rec_inittable);
              needs_inittable:=binittable;
              binittable:=oldb;
           end;
      end;


    procedure count_published_properties(sym:pnamedindexobject);
      {$ifndef fpc}far;{$endif}
      begin
         if needs_prop_entry(psym(sym)) then
           inc(count);
      end;


    procedure write_property_info(sym : pnamedindexobject);{$ifndef fpc}far;{$endif}
      var
         proctypesinfo : byte;

      procedure writeproc(proc : psymlist; shiftvalue : byte);

        var
           typvalue : byte;
           hp : psymlistitem;
           address : longint;

        begin
           if not(assigned(proc) and assigned(proc^.firstsym))  then
             begin
                rttilist^.concat(new(pai_const,init_32bit(1)));
                typvalue:=3;
             end
           else if proc^.firstsym^.sym^.typ=varsym then
             begin
                address:=0;
                hp:=proc^.firstsym;
                while assigned(hp) do
                  begin
                     inc(address,pvarsym(hp^.sym)^.address);
                     hp:=hp^.next;
                  end;
                rttilist^.concat(new(pai_const,init_32bit(address)));
                typvalue:=0;
             end
           else
             begin
                if not(po_virtualmethod in pprocdef(proc^.def)^.procoptions) then
                  begin
                     rttilist^.concat(new(pai_const_symbol,initname(pprocdef(proc^.def)^.mangledname)));
                     typvalue:=1;
                  end
                else
                  begin
                     { virtual method, write vmt offset }
                     rttilist^.concat(new(pai_const,init_32bit(
                       pprocdef(proc^.def)^._class^.vmtmethodoffset(pprocdef(proc^.def)^.extnumber))));
                     typvalue:=2;
                  end;
             end;
           proctypesinfo:=proctypesinfo or (typvalue shl shiftvalue);
        end;

      begin
         if needs_prop_entry(psym(sym)) then
           case psym(sym)^.typ of
              varsym:
                begin
                   if not(pvarsym(sym)^.vartype.def^.deftype=objectdef) or
                     not(pobjectdef(pvarsym(sym)^.vartype.def)^.is_class) then
                     internalerror(1509992);
                   { access to implicit class property as field }
                   proctypesinfo:=(0 shl 0) or (0 shl 2) or (0 shl 4);
                   rttilist^.concat(new(pai_const_symbol,initname(pvarsym(sym)^.vartype.def^.get_rtti_label)));
                   rttilist^.concat(new(pai_const,init_32bit(pvarsym(sym)^.address)));
                   rttilist^.concat(new(pai_const,init_32bit(pvarsym(sym)^.address)));
                   { per default stored }
                   rttilist^.concat(new(pai_const,init_32bit(1)));
                   { index as well as ... }
                   rttilist^.concat(new(pai_const,init_32bit(0)));
                   { default value are zero }
                   rttilist^.concat(new(pai_const,init_32bit(0)));
                   rttilist^.concat(new(pai_const,init_16bit(count)));
                   inc(count);
                   rttilist^.concat(new(pai_const,init_8bit(proctypesinfo)));
                   rttilist^.concat(new(pai_const,init_8bit(length(pvarsym(sym)^.name))));
                   rttilist^.concat(new(pai_string,init(pvarsym(sym)^.name)));
                end;
              propertysym:
                begin
                   if ppo_indexed in ppropertysym(sym)^.propoptions then
                     proctypesinfo:=$40
                   else
                     proctypesinfo:=0;
                   rttilist^.concat(new(pai_const_symbol,initname(ppropertysym(sym)^.proptype.def^.get_rtti_label)));
                   writeproc(ppropertysym(sym)^.readaccess,0);
                   writeproc(ppropertysym(sym)^.writeaccess,2);
                   { isn't it stored ? }
                   if not(ppo_stored in ppropertysym(sym)^.propoptions) then
                     begin
                        rttilist^.concat(new(pai_const,init_32bit(0)));
                        proctypesinfo:=proctypesinfo or (3 shl 4);
                     end
                   else
                     writeproc(ppropertysym(sym)^.storedaccess,4);
                   rttilist^.concat(new(pai_const,init_32bit(ppropertysym(sym)^.index)));
                   rttilist^.concat(new(pai_const,init_32bit(ppropertysym(sym)^.default)));
                   rttilist^.concat(new(pai_const,init_16bit(count)));
                   inc(count);
                   rttilist^.concat(new(pai_const,init_8bit(proctypesinfo)));
                   rttilist^.concat(new(pai_const,init_8bit(length(ppropertysym(sym)^.name))));
                   rttilist^.concat(new(pai_string,init(ppropertysym(sym)^.name)));
                end;
              else internalerror(1509992);
           end;
      end;


    procedure generate_published_child_rtti(sym : pnamedindexobject);{$ifndef fpc}far;{$endif}
      begin
         if needs_prop_entry(psym(sym)) then
           case psym(sym)^.typ of
              varsym:
                pvarsym(sym)^.vartype.def^.get_rtti_label;
              propertysym:
                ppropertysym(sym)^.proptype.def^.get_rtti_label;
              else
                internalerror(1509991);
           end;
      end;


    procedure tobjectdef.write_child_rtti_data;
      begin
         symtable^.foreach({$ifndef TP}@{$endif}generate_published_child_rtti);
      end;


    procedure tobjectdef.generate_rtti;
      begin
         if not has_rtti then
          begin
            has_rtti:=true;
            getdatalabel(rtti_label);
            write_child_rtti_data;
            rttilist^.concat(new(pai_symbol,initname_global(rtti_name,0)));
            rttilist^.concat(new(pai_label,init(rtti_label)));
            write_rtti_data;
            rttilist^.concat(new(pai_symbol_end,initname(rtti_name)));
          end;
      end;


    function tobjectdef.next_free_name_index : longint;
      var
         i : longint;
      begin
         if assigned(childof) and (oo_can_have_published in childof^.objectoptions) then
           i:=childof^.next_free_name_index
         else
           i:=0;
         count:=0;
         symtable^.foreach({$ifndef TP}@{$endif}count_published_properties);
         next_free_name_index:=i+count;
      end;


    procedure tobjectdef.write_rtti_data;
      begin
         if is_class then
           rttilist^.concat(new(pai_const,init_8bit(tkclass)))
         else
           rttilist^.concat(new(pai_const,init_8bit(tkobject)));

         { generate the name }
         rttilist^.concat(new(pai_const,init_8bit(length(objname^))));
         rttilist^.concat(new(pai_string,init(objname^)));

         { write class type }
         rttilist^.concat(new(pai_const_symbol,initname(vmt_mangledname)));

         { write owner typeinfo }
         if assigned(childof) and (oo_can_have_published in childof^.objectoptions) then
           rttilist^.concat(new(pai_const_symbol,initname(childof^.get_rtti_label)))
         else
           rttilist^.concat(new(pai_const,init_32bit(0)));

         { count total number of properties }
         if assigned(childof) and (oo_can_have_published in childof^.objectoptions) then
           count:=childof^.next_free_name_index
         else
           count:=0;

         { write it }
         symtable^.foreach({$ifndef TP}@{$endif}count_published_properties);
         rttilist^.concat(new(pai_const,init_16bit(count)));

         { write unit name }
         if assigned(owner^.name) then
           begin
              rttilist^.concat(new(pai_const,init_8bit(length(owner^.name^))));
              rttilist^.concat(new(pai_string,init(owner^.name^)));
           end
         else
           rttilist^.concat(new(pai_const,init_8bit(0)));

         { write published properties count }
         count:=0;
         symtable^.foreach({$ifndef TP}@{$endif}count_published_properties);
         rttilist^.concat(new(pai_const,init_16bit(count)));

         { count is used to write nameindex   }
         { but we need an offset of the owner }
         { to give each property an own slot  }
         if assigned(childof) and (oo_can_have_published in childof^.objectoptions) then
           count:=childof^.next_free_name_index
         else
           count:=0;

         symtable^.foreach({$ifndef TP}@{$endif}write_property_info);
      end;


    function tobjectdef.is_publishable : boolean;
      begin
         is_publishable:=is_class;
      end;

    function  tobjectdef.get_rtti_label : string;

      begin
         generate_rtti;
         get_rtti_label:=rtti_name;
      end;


{****************************************************************************
                                TFORWARDDEF
****************************************************************************}

   constructor tforwarddef.init(const s:string;const pos : tfileposinfo);
     var
       oldregisterdef : boolean;
     begin
        { never register the forwarddefs, they are disposed at the
          end of the type declaration block }
        oldregisterdef:=registerdef;
        registerdef:=false;
        inherited init;
        registerdef:=oldregisterdef;
        deftype:=forwarddef;
        tosymname:=s;
        forwardpos:=pos;
     end;


    function tforwarddef.gettypename:string;
      begin
        gettypename:='unresolved forward to '+tosymname;
      end;


{****************************************************************************
                                  TERRORDEF
****************************************************************************}

   constructor terrordef.init;
     begin
        inherited init;
        deftype:=errordef;
     end;


{$ifdef GDB}
    function terrordef.stabstring : pchar;
      begin
         stabstring:=strpnew('error'+numberstring);
      end;
{$endif GDB}

    function terrordef.gettypename:string;

      begin
         gettypename:='<erroneous type>';
      end;

{
  $Log$
  Revision 1.195  2000-02-11 13:53:49  pierre
   * avoid stack overflow in tref.done (bug 846)

  Revision 1.194  2000/02/09 13:23:04  peter
    * log truncated

  Revision 1.193  2000/02/05 14:33:32  florian
    * fixed init table generation for classes and arrays

  Revision 1.192  2000/02/04 20:00:22  florian
    * an exception in a construcor calls now the destructor (this applies only
      to classes)

  Revision 1.191  2000/01/30 23:29:06  peter
    * fixed dup rtti writing for classes

  Revision 1.190  2000/01/28 23:17:53  florian
    * virtual XXXX; support for objects, only if -dWITHDMT is defined

  Revision 1.189  2000/01/26 12:02:29  peter
    * abstractprocdef.para_size needs alignment parameter
    * secondcallparan gets para_alignment size instead of dword_align

  Revision 1.188  2000/01/23 16:35:31  peter
    * localbrowser loading of absolute fixed. It needed a symtablestack
      which was not setup correctly

  Revision 1.187  2000/01/09 23:16:06  peter
    * added st_default stringtype
    * genstringconstnode extended with stringtype parameter using st_default
      will do the old behaviour

  Revision 1.186  2000/01/07 01:14:39  peter
    * updated copyright to 2000

  Revision 1.185  2000/01/03 19:26:03  peter
    * fixed resolving of ttypesym which are reference from object/record
      fields.

  Revision 1.184  1999/12/31 14:24:34  peter
    * fixed rtti generation for classes with no published section

  Revision 1.183  1999/12/23 12:19:42  peter
    * check_rec_inittable fix from sg

  Revision 1.182  1999/12/19 17:00:27  peter
    * has_rtti should be saved in the ppu for objects

  Revision 1.181  1999/12/18 14:55:21  florian
    * very basic widestring support

  Revision 1.180  1999/12/06 18:21:03  peter
    * support !ENVVAR for long commandlines
    * win32/go32v2 write short pathnames to link.res so c:\Program Files\ is
      finally supported as installdir.

  Revision 1.179  1999/12/01 12:42:33  peter
    * fixed bug 698
    * removed some notes about unused vars

  Revision 1.178  1999/12/01 10:26:38  pierre
   * restore the correct way for stabs of forward defs

  Revision 1.177  1999/11/30 10:40:54  peter
    + ttype, tsymlist

  Revision 1.176  1999/11/09 23:35:49  pierre
   + better reference pos for forward defs

  Revision 1.175  1999/11/07 23:57:36  pierre
   + higher level browser

  Revision 1.174  1999/11/06 14:34:26  peter
    * truncated log to 20 revs

}