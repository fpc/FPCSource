{
    $Id$
    Copyright (c) 1993-98 by Florian Klaempfl, Pierre Muller

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
         sym := nil;
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
         sym:=ptypesym(readsymref);
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
         while assigned(sym) do
           begin
              sym^.definition:=nil;
              sym:=sym^.synonym;
           end;

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
        if assigned(sym) then
         typename:=Upper(sym^.name)
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
              else if p^.symtabletype=objectsymtable then
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
        writesymref(sym);
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
      if (not assigned(sym)) or (not sym^.isusedinstab) then
        begin
           {set even if debuglist is not defined}
           if assigned(sym) then
             sym^.isusedinstab := true;
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
           if assigned(sym) then
             begin
                table := sym^.owner;
                if table^.unitid > 0 then
                  numberstring := '('+tostr(table^.unitid)+','
                  +tostr(sym^.definition^.globalnb)+')'
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
      if assigned(sym) then
        begin
           sname := sym^.name;
           sym_line_no:=sym^.fileinfo.line;
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
    if ((sym = nil) or sym^.isusedinstab or (cs_gdb_dbx in aktglobalswitches))
      and not is_def_stab_written then
      begin
      If cs_gdb_dbx in aktglobalswitches then
        begin
           { otherwise you get two of each def }
           If assigned(sym) then
             begin
                if sym^.typ=typesym then
                  sym^.isusedinstab:=true;
                if (sym^.owner = nil) or
                  ((sym^.owner^.symtabletype = unitsymtable) and
                 punitsymtable(sym^.owner)^.dbx_count_ok)  then
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
      if asmlist = debuglist then do_count_dbx := true;
      { count_dbx(stab_str); moved to GDB.PAS}
      asmlist^.concat(new(pai_stabs,init(stab_str)));
      end;
    end;
{$endif GDB}


    procedure tdef.deref;
      begin
      end;


    procedure tdef.symderef;
      begin
        resolvesym(psym(sym));
      end;


    { rtti generation }
    procedure tdef.generate_rtti;
      begin
         has_rtti:=true;
         getdatalabel(rtti_label);
         write_child_rtti_data;
         rttilist^.concat(new(pai_symbol,init(rtti_label,0)));
         write_rtti_data;
         rttilist^.concat(new(pai_symbol_end,init(rtti_label)));
      end;


    function tdef.get_rtti_label : string;
      begin
         if not(has_rtti) then
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
         if assigned(sym) then
           begin
              str:=sym^.name;
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
        is_fpuregable:=not(pfloatdef(@self)^.typ in [f32bit,f16bit]);
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
        typname:array[tstringtype] of string[8]=(
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
         names : array[tstringtype] of string[20] =
           ('ShortString','LongString','AnsiString','WideString');

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
         if (cs_smartlink in aktmoduleswitches) then
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
              if (cs_smartlink in aktmoduleswitches) then
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
                bool16bit:
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
         if (cs_smartlink in aktmoduleswitches) then
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
              if (cs_smartlink in aktmoduleswitches) then
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
          'LongBool','QWord','Int64');

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

    constructor tfiledef.init(ft : tfiletype;tas : pdef);
      begin
         inherited init;
         deftype:=filedef;
         filetype:=ft;
         typed_as:=tas;
         setsize;
      end;


    constructor tfiledef.load;
      begin
         inherited load;
         deftype:=filedef;
         filetype:=tfiletype(readbyte);
         if filetype=ft_typed then
           typed_as:=readdefref
         else
           typed_as:=nil;
         setsize;
      end;


    procedure tfiledef.deref;
      begin
         if filetype=ft_typed then
           resolvedef(typed_as);
      end;


    procedure tfiledef.setsize;
      begin
        case filetype of
          ft_text : savesize:=572;
         ft_typed,
       ft_untyped : savesize:=316;
        end;
      end;


    procedure tfiledef.write;
      begin
         inherited write;
         writebyte(byte(filetype));
         if filetype=ft_typed then
           writedefref(typed_as);
         current_ppu^.writeentry(ibfiledef);
      end;


{$ifdef GDB}
    function tfiledef.stabstring : pchar;
      begin
   {$IfDef GDBknowsfiles}
      case filetyp of
        ft_typed : stabstring := strpnew('d'+typed_as^.numberstring{+';'});
      ft_untyped : stabstring := strpnew('d'+voiddef^.numberstring{+';'});
         ft_text : stabstring := strpnew('d'+cchardef^.numberstring{+';'});
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
      if ((sym = nil) or sym^.isusedinstab or (cs_gdb_dbx in aktglobalswitches)) and
         not is_def_stab_written then
        begin
        if assigned(typed_as) then forcestabto(asmlist,typed_as);
        inherited concatstabto(asmlist);
        end;
      end;
{$endif GDB}

    function tfiledef.gettypename : string;

      begin
         case filetype of
           ft_untyped:
             gettypename:='File';
           ft_typed:
             gettypename:='File Of '+typed_as^.typename;
           ft_text:
             gettypename:='Text'
         end;
      end;



{****************************************************************************
                               TPOINTERDEF
****************************************************************************}

    constructor tpointerdef.init(def : pdef);
      begin
         inherited init;
         deftype:=pointerdef;
         definition:=def;
         is_far:=false;
         savesize:=target_os.size_of_pointer;
      end;


    constructor tpointerdef.initfar(def : pdef);
      begin
         inherited init;
         deftype:=pointerdef;
         definition:=def;
         is_far:=true;
         savesize:=target_os.size_of_pointer;
      end;


    constructor tpointerdef.load;
      begin
         inherited load;
         deftype:=pointerdef;
         { the real address in memory is calculated later (deref) }
         definition:=readdefref;
         is_far:=(readbyte<>0);
         savesize:=target_os.size_of_pointer;
      end;


    procedure tpointerdef.deref;
      begin
         resolvedef(definition);
      end;


    procedure tpointerdef.write;
      begin
         inherited write;
         writedefref(definition);
         writebyte(byte(is_far));
         current_ppu^.writeentry(ibpointerdef);
      end;


{$ifdef GDB}
    function tpointerdef.stabstring : pchar;
      begin
        stabstring := strpnew('*'+definition^.numberstring);
      end;


    procedure tpointerdef.concatstabto(asmlist : paasmoutput);
      var st,nb : string;
          sym_line_no : longint;
      begin
      if ( (sym=nil) or sym^.isusedinstab or (cs_gdb_dbx in aktglobalswitches)) and
         not is_def_stab_written then
        begin
        if assigned(definition) then
          if definition^.deftype in [recorddef,objectdef] then
            begin
            is_def_stab_written := true;
            {to avoid infinite recursion in record with next-like fields }
            nb := definition^.numberstring;
            is_def_stab_written := false;
            if not definition^.is_def_stab_written then
              begin
              if assigned(definition^.sym) then
                begin
                if assigned(sym) then
                  begin
                     st := sym^.name;
                     sym_line_no:=sym^.fileinfo.line;
                  end
                else
                  begin
                     st := ' ';
                     sym_line_no:=0;
                  end;
                st := '"'+st+':t'+numberstring+'=*'+definition^.numberstring
                      +'=xs'+definition^.sym^.name+':",'+tostr(N_LSYM)+',0,'+tostr(sym_line_no)+',0';
                if asmlist = debuglist then do_count_dbx := true;
                asmlist^.concat(new(pai_stabs,init(strpnew(st))));
                end;
              end else inherited concatstabto(asmlist);
            is_def_stab_written := true;
            end else
            begin
            { p =^p1; p1=^p problem }
            is_def_stab_written := true;
            forcestabto(asmlist,definition);
            is_def_stab_written := false;
            inherited concatstabto(asmlist);
            end;
        end;
      end;
{$endif GDB}

    function tpointerdef.gettypename : string;

      begin
         gettypename:='^'+definition^.typename;
      end;

{****************************************************************************
                              TCLASSREFDEF
****************************************************************************}

    constructor tclassrefdef.init(def : pdef);
      begin
         inherited init(def);
         deftype:=classrefdef;
         definition:=def;
         savesize:=target_os.size_of_pointer;
      end;


    constructor tclassrefdef.load;
      begin
         { be careful, tclassdefref inherits from tpointerdef }
         tdef.load;
         deftype:=classrefdef;
         definition:=readdefref;
         is_far:=false;
         savesize:=target_os.size_of_pointer;
      end;


    procedure tclassrefdef.write;
      begin
         { be careful, tclassdefref inherits from tpointerdef }
         tdef.write;
         writedefref(definition);
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
         gettypename:='Class Of '+definition^.typename;
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
         setof:=s;
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
         setof:=readdefref;
         settype:=tsettype(readbyte);
         case settype of
            normset : savesize:=32;
            varset : savesize:=readlong;
            smallset : savesize:=Sizeof(longint);
         end;
      end;


    procedure tsetdef.write;
      begin
         inherited write;
         writedefref(setof);
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
           stabstring := strpnew('S'+setof^.numberstring);
      end;


    procedure tsetdef.concatstabto(asmlist : paasmoutput);
      begin
      if ( not assigned(sym) or sym^.isusedinstab or (cs_gdb_dbx in aktglobalswitches)) and
          not is_def_stab_written then
        begin
          if assigned(setof) then
            forcestabto(asmlist,setof);
          inherited concatstabto(asmlist);
        end;
      end;
{$endif GDB}


    procedure tsetdef.deref;
      begin
         resolvedef(setof);
      end;


    procedure tsetdef.write_rtti_data;
      begin
         rttilist^.concat(new(pai_const,init_8bit(tkSet)));
         write_rtti_name;
         rttilist^.concat(new(pai_const,init_8bit(otULong)));
         rttilist^.concat(new(pai_const_symbol,initname(setof^.get_rtti_label)));
      end;


    procedure tsetdef.write_child_rtti_data;
      begin
         setof^.get_rtti_label;
      end;


    function tsetdef.is_publishable : boolean;
      begin
         is_publishable:=settype=smallset;
      end;

    function tsetdef.gettypename : string;

      begin
         gettypename:='Set Of '+setof^.typename;
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
         rangedef:=rd;
         definition:=nil;
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
         definition:=readdefref;
         rangedef:=readdefref;
         lowrange:=readlong;
         highrange:=readlong;
         IsArrayOfConst:=boolean(readbyte);
         IsVariant:=false;
         IsConstructor:=false;
         rangenr:=0;
      end;


    function tarraydef.getrangecheckstring : string;
      begin
         if (cs_smartlink in aktmoduleswitches) then
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
              if (cs_smartlink in aktmoduleswitches) then
                datasegment^.concat(new(pai_symbol,initname_global(getrangecheckstring,8)))
              else
                datasegment^.concat(new(pai_symbol,initname(getrangecheckstring,8)));
              datasegment^.concat(new(pai_const,init_32bit(lowrange)));
              datasegment^.concat(new(pai_const,init_32bit(highrange)));
           end;
      end;


    procedure tarraydef.deref;
      begin
         resolvedef(definition);
         resolvedef(rangedef);
      end;


    procedure tarraydef.write;
      begin
         inherited write;
         writedefref(definition);
         writedefref(rangedef);
         writelong(lowrange);
         writelong(highrange);
         writebyte(byte(IsArrayOfConst));
         current_ppu^.writeentry(ibarraydef);
      end;


{$ifdef GDB}
    function tarraydef.stabstring : pchar;
      begin
      stabstring := strpnew('ar'+rangedef^.numberstring+';'
                    +tostr(lowrange)+';'+tostr(highrange)+';'+definition^.numberstring);
      end;


    procedure tarraydef.concatstabto(asmlist : paasmoutput);
      begin
      if (not assigned(sym) or sym^.isusedinstab or (cs_gdb_dbx in aktglobalswitches))
        and not is_def_stab_written then
        begin
        {when array are inserted they have no definition yet !!}
        if assigned(definition) then
          inherited concatstabto(asmlist);
        end;
      end;
{$endif GDB}


    function tarraydef.elesize : longint;
      begin
         elesize:=definition^.size;
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
         alignment:=definition^.size;
      end;


    function tarraydef.needs_inittable : boolean;
      begin
         needs_inittable:=definition^.needs_inittable;
      end;


    procedure tarraydef.write_child_rtti_data;
      begin
         definition^.get_rtti_label;
      end;


    procedure tarraydef.write_rtti_data;
      begin
         rttilist^.concat(new(pai_const,init_8bit(13)));
         write_rtti_name;
         { size of elements }
         rttilist^.concat(new(pai_const,init_32bit(definition^.size)));
         { count of elements }
         rttilist^.concat(new(pai_const,init_32bit(highrange-lowrange+1)));
         { element type }
         rttilist^.concat(new(pai_const_symbol,initname(definition^.get_rtti_label)));
      end;

    function tarraydef.gettypename : string;

      begin
         if isarrayofconst or isConstructor then
           begin
             if isvariant then
               gettypename:='Array Of Const'
             else
               gettypename:='Array Of '+definition^.typename;
           end
         else if is_open_array(@self) then
           gettypename:='Array Of '+definition^.typename
         else
           begin
              if rangedef^.deftype=enumdef then
                gettypename:='Array['+rangedef^.typename+'] Of '+definition^.typename
              else
                gettypename:='Array['+tostr(lowrange)+'..'+
                  tostr(highrange)+'] Of '+definition^.typename
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
         if (psym(s)^.typ=varsym) and
            ((pvarsym(s)^.definition^.deftype<>objectdef) or
             not(pobjectdef(pvarsym(s)^.definition)^.is_class)) then
            binittable:=pvarsym(s)^.definition^.needs_inittable;
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
         if (pvarsym(p)^.definition^.deftype=objectdef) and
            pobjectdef(pvarsym(p)^.definition)^.is_class then
            spec:=spec+'*';
         size:=pvarsym(p)^.definition^.size;
         { open arrays made overflows !! }
         if size>$fffffff then
           size:=$fffffff;
         newrec := strpnew(p^.name+':'+spec+pvarsym(p)^.definition^.numberstring
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
         RecOffset := RecOffset + pvarsym(p)^.definition^.size;
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
        if (not assigned(sym) or sym^.isusedinstab or (cs_gdb_dbx in aktglobalswitches)) and
           (not is_def_stab_written) then
          inherited concatstabto(asmlist);
      end;

{$endif GDB}

    var
       count : longint;
    procedure count_inittable_fields(sym : pnamedindexobject);{$ifndef fpc}far;{$endif}
      begin
         if (psym(sym)^.typ=varsym) and
            (pvarsym(sym)^.definition^.needs_inittable) then
           inc(count);
      end;


    procedure count_fields(sym : pnamedindexobject);{$ifndef fpc}far;{$endif}
      begin
         inc(count);
      end;


    procedure write_field_inittable(sym : pnamedindexobject);{$ifndef fpc}far;{$endif}
      begin
         if (psym(sym)^.typ=varsym) and
            pvarsym(sym)^.definition^.needs_inittable then
           begin
              rttilist^.concat(new(pai_const_symbol,init(pvarsym(sym)^.definition^.get_inittable_label)));
              rttilist^.concat(new(pai_const,init_32bit(pvarsym(sym)^.address)));
           end;
      end;


    procedure write_field_rtti(sym : pnamedindexobject);{$ifndef fpc}far;{$endif}
      begin
         rttilist^.concat(new(pai_const_symbol,initname(pvarsym(sym)^.definition^.get_rtti_label)));
         rttilist^.concat(new(pai_const,init_32bit(pvarsym(sym)^.address)));
      end;


    procedure generate_child_inittable(sym:pnamedindexobject);{$ifndef fpc}far;{$endif}
      begin
         if (psym(sym)^.typ=varsym) and
            pvarsym(sym)^.definition^.needs_inittable then
         { force inittable generation }
           pvarsym(sym)^.definition^.get_inittable_label;
      end;


    procedure generate_child_rtti(sym : pnamedindexobject);{$ifndef fpc}far;{$endif}
      begin
         pvarsym(sym)^.definition^.get_rtti_label;
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

    procedure disposepdefcoll(var para1 : pdefcoll);
      var
         hp : pdefcoll;
      begin
         hp:=para1;
         while assigned(hp) do
           begin
              para1:=hp^.next;
              dispose(hp);
              hp:=para1;
           end;
      end;


    constructor tabstractprocdef.init;
      begin
         inherited init;
         para1:=nil;
         fpu_used:=0;
         proctypeoption:=potype_none;
         proccalloptions:=[];
         procoptions:=[];
         retdef:=voiddef;
         symtablelevel:=0;
         savesize:=target_os.size_of_pointer;
      end;


    destructor tabstractprocdef.done;
      begin
         disposepdefcoll(para1);
         inherited done;
      end;


    procedure tabstractprocdef.concatdef(p : pdef;vsp : tvarspez);
      var
         hp : pdefcoll;
      begin
         new(hp);
         hp^.paratyp:=vsp;
         hp^.datasym:=nil;
         hp^.data:=p;
         hp^.next:=para1;
         hp^.register:=R_NO;
         para1:=hp;
      end;


    procedure tabstractprocdef.concattypesym(p : ptypesym;vsp : tvarspez);
      var
         hp : pdefcoll;
      begin
         new(hp);
         hp^.paratyp:=vsp;
         hp^.datasym:=p;
         hp^.data:=p^.definition;
         hp^.next:=para1;
         hp^.register:=R_NO;
         para1:=hp;
      end;


    { all functions returning in FPU are
      assume to use 2 FPU registers
      until the function implementation
      is processed   PM }
    procedure tabstractprocdef.test_if_fpu_result;
      begin
         if assigned(retdef) and is_fpu(retdef) then
           fpu_used:=2;
      end;


    procedure tabstractprocdef.deref;
      var
         hp : pdefcoll;
      begin
         inherited deref;
         resolvedef(retdef);
         hp:=para1;
         while assigned(hp) do
           begin
              if assigned(hp^.datasym) then
               begin
                 resolvesym(psym(hp^.datasym));
                 hp^.data:=hp^.datasym^.definition;
               end
              else
               resolvedef(hp^.data);
              hp:=hp^.next;
           end;
      end;


    constructor tabstractprocdef.load;
      var
         last,hp : pdefcoll;
         count,i : word;
      begin
         inherited load;
         retdef:=readdefref;
         fpu_used:=readbyte;
         proctypeoption:=tproctypeoption(readlong);
         readsmallset(proccalloptions);
         readsmallset(procoptions);
         count:=readword;
         para1:=nil;
         savesize:=target_os.size_of_pointer;
         for i:=1 to count do
           begin
              new(hp);
              hp^.paratyp:=tvarspez(readbyte);
              { hp^.register:=tregister(readbyte); }
              hp^.register:=R_NO;
              hp^.data:=readdefref;
              hp^.datasym:=ptypesym(readsymref);
              hp^.next:=nil;
              if para1=nil then
                para1:=hp
              else
                last^.next:=hp;
              last:=hp;
           end;
      end;


    procedure tabstractprocdef.write;
      var
         count : word;
         hp : pdefcoll;
      begin
         inherited write;
         writedefref(retdef);
         current_ppu^.do_interface_crc:=false;
         writebyte(fpu_used);
         writelong(ord(proctypeoption));
         writesmallset(proccalloptions);
         writesmallset(procoptions);
         hp:=para1;
         count:=0;
         while assigned(hp) do
           begin
              inc(count);
              hp:=hp^.next;
           end;
         writeword(count);
         hp:=para1;
         while assigned(hp) do
           begin
              writebyte(byte(hp^.paratyp));
              { writebyte(byte(hp^.register)); }
              if assigned(hp^.datasym) then
               begin
                 writedefref(nil);
                 writesymref(psym(hp^.datasym));
               end
              else
               begin
                 writedefref(hp^.data);
                 writesymref(nil);
               end;
              hp:=hp^.next;
           end;
      end;


    function tabstractprocdef.para_size : longint;
      var
         pdc : pdefcoll;
         l : longint;
      begin
         l:=0;
         pdc:=para1;
         while assigned(pdc) do
          begin
            case pdc^.paratyp of
              vs_var   : inc(l,target_os.size_of_pointer);
              vs_value,
              vs_const : if push_addr_param(pdc^.data) then
                          inc(l,target_os.size_of_pointer)
                         else
                          inc(l,align(pdc^.data^.size,target_os.stackalignment));
            end;
            pdc:=pdc^.next;
          end;
         para_size:=l;
      end;


    function tabstractprocdef.demangled_paras : string;

      var s : string;

      procedure doconcat(p : pdefcoll);

        begin
           if assigned(p^.next) then
             doconcat(p^.next)
           else
             s:='(';
           if assigned(p^.data^.sym) then
             s:=s+p^.data^.sym^.name
           else if p^.paratyp=vs_var then
             s:=s+'var'
           else if p^.paratyp=vs_const then
             s:=s+'const';
           if p<>para1 then
             s:=s+','
           else
             s:=s+')';
        end;

      begin
        s:='';
        { a recursive solution is the easiest way to inverse the parameter }
        { collection                                                       }
        if assigned(para1) then
          doconcat(para1);
        demangled_paras:=s;
      end;


{$ifdef GDB}
    function tabstractprocdef.stabstring : pchar;
      begin
        stabstring := strpnew('abstractproc'+numberstring+';');
      end;


    procedure tabstractprocdef.concatstabto(asmlist : paasmoutput);
      begin
         if (not assigned(sym) or sym^.isusedinstab or (cs_gdb_dbx in aktglobalswitches))
            and not is_def_stab_written then
           begin
              if assigned(retdef) then forcestabto(asmlist,retdef);
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
        pdo : pobjectdef;
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
             pdo:=_class;
             new(parast,loadas(parasymtable));
             parast^.next:=owner;
             parast^.load_browser;
             new(localst,loadas(localsymtable));
             localst^.next:=parast;
             localst^.load_browser;
{$endif ndef NOLOCALBROWSER}
          end;
      end;


    function tprocdef.write_references : boolean;
      var
        ref : pref;
{$ifndef NOLOCALBROWSER}
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
             parast^.writeas;
             parast^.unitid:=local_symtable_index;
             inc(local_symtable_index);
             parast^.write_browser;
             if not assigned(localst) then
               localst:=new(psymtable,init(localsymtable));
             localst^.writeas;
             localst^.unitid:=local_symtable_index;
             inc(local_symtable_index);
             localst^.write_browser;
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
           dispose(defref,done);
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
           globals.strdispose(_mangledname);
         inherited done;
      end;


    procedure tprocdef.write;
      begin
         inherited write;
         current_ppu^.do_interface_crc:=false;
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
         writestring(mangledname);
         current_ppu^.do_interface_crc:=true;
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
      strpcopy(strend(StabRecString),p^.name+':'+pvarsym(p)^.definition^.numberstring+','+vs+';');
      end;


    function tprocdef.stabstring : pchar;
      var param : pdefcoll;
          i : word;
          oldrec : pchar;
      begin
      oldrec := stabrecstring;
      getmem(StabRecString,1024);
      param := para1;
      i := 0;
      while assigned(param) do
        begin
           inc(i);
           param := param^.next;
        end;
      strpcopy(StabRecString,'f'+retdef^.numberstring);
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
               +':'+param^.data^.numberstring+','+vartyp+';');
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
         param : pdefcoll;
      begin
      s := sym^.name;
      if _class <> nil then
        begin
        s2 := _class^.objname^;
        s := s+'__'+tostr(length(s2))+s2;
        end else s := s + '_';
      param := para1;
      while assigned(param) do
        begin
        s2 := param^.data^.sym^.name;
        s := s+tostr(length(s2))+s2;
        param := param^.next;
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
         if is_fpu(retdef) then
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
         i : word;
         param : pdefcoll;
      begin
        i := 0;
        param := para1;
        while assigned(param) do
          begin
          inc(i);
          param := param^.next;
          end;
        getmem(nss,1024);
        { it is not a function but a function pointer !! (PM) }

        strpcopy(nss,'*f'+retdef^.numberstring{+','+tostr(i)}+';');
        param := para1;
        i := 0;
        { this confuses gdb !!
          we should use 'F' instead of 'f' but
          as we use c++ language mode
          it does not like that either
          Please do not remove this part
          might be used once
          gdb for pascal is ready PM }
        (* while assigned(param) do
          begin
          inc(i);
          if param^.paratyp = vs_value then vartyp := '1' else vartyp := '0';
          {Here we have lost the parameter names !!}
          pst := strpnew('p'+tostr(i)+':'+param^.data^.numberstring+','+vartyp+';');
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
         if ( not assigned(sym) or sym^.isusedinstab or (cs_gdb_dbx in aktglobalswitches))
           and not is_def_stab_written then
           inherited concatstabto(asmlist);
         is_def_stab_written:=true;
      end;
{$endif GDB}


    procedure tprocvardef.write_rtti_data;
      var
         pdc, pdc2, pdcbefore : pdefcoll;
         methodkind, paracount, paraspec : byte;
      begin
        { write method id and name }
        rttilist^.concat(new(pai_const,init_8bit(tkmethod)));
        write_rtti_name;

        { write kind of method (can only be function or procedure)}
        if retdef = pdef(voiddef) then    { ### typecast shoudln't be necessary! (sg) }
          methodkind := mkProcedure
        else
          methodkind := mkFunction;
        rttilist^.concat(new(pai_const,init_8bit(methodkind)));

        { get # of parameters }
        paracount:=0;
        pdc:=para1;
        while assigned(pdc) do
          begin
            inc(paracount);
            pdc:=pdc^.next;
          end;
        rttilist^.concat(new(pai_const,init_8bit(paracount)));

        { write parameter info. The parameters must be written in reverse order
          if this method uses right to left parameter pushing! }
        pdc:=para1;
        if assigned(pdc) and not (pocall_leftright in proccalloptions) then
          while assigned(pdc^.next) do pdc := pdc^.next;

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
            pdc^.data^.write_rtti_name;
            if pocall_leftright in proccalloptions then
              pdc:=pdc^.next
            else
              begin
                { find previous argument }
                pdcbefore := nil;
                pdc2 := para1;
                while pdc2 <> pdc do
                  begin
                    pdcbefore := pdc2;
                    pdc2 := pdc2^.next;
                  end;
                pdc := pdcbefore;
              end;
          end;

         { write name of result type }
         retdef^.write_rtti_name;
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
         if assigned(retdef) then
           gettypename:='<procedure variable type of function'+demangled_paras+':'+retdef^.gettypename+'>'
         else
           gettypename:='<procedure variable type of procedure'+demangled_paras+'>';
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
         has_rtti:=true;
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
         vmtmethodoffset:=(index+3)*target_os.size_of_pointer;
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
        para : pdefcoll;
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
                { arguments are not listed here }
                {we don't need another definition}
                 para := pd^.para1;
                 { used by gdbpas to recognize constructor and destructors }
                 if (pd^.proctypeoption=potype_constructor) then
                   argnames:='__ct__'
                 else if (pd^.proctypeoption=potype_destructor) then
                   argnames:='__dt__'
                 else
                   argnames := '';

                 while assigned(para) do
                   begin
                   if para^.data^.deftype = formaldef then
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
                     if assigned(para^.data^.sym) then
                       begin
                          arglength := length(para^.data^.sym^.name);
                          argnames := argnames + tostr(arglength)+para^.data^.sym^.name;
                       end
                     else
                       begin
                          argnames:=argnames+'11unnamedtype';
                       end;
                     end;
                   para := para^.next;
                   end;
                ipd^.is_def_stab_written := true;
                { here 2A must be changed for private and protected }
                { 0 is private 1 protected and 2 public }
                if (sp_private in psym(p)^.symoptions) then sp:='0'
                else if (sp_protected in psym(p)^.symoptions) then sp:='1'
                else sp:='2';
                newrec := strpnew(p^.name+'::'+ipd^.numberstring
                     +'=##'+pd^.retdef^.numberstring+';:'+argnames+';'+sp+'A'
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


    procedure count_published_properties(sym:pnamedindexobject);
      {$ifndef fpc}far;{$endif}
      begin
         if (psym(sym)^.typ=propertysym) and
            (sp_published in psym(sym)^.symoptions) then
           inc(count);
      end;


    procedure write_property_info(sym : pnamedindexobject);{$ifndef fpc}far;{$endif}
      var
         proctypesinfo : byte;

      procedure writeproc(sym : ppropsymlist;def : pdef;shiftvalue : byte);
        var
           typvalue : byte;
        begin
           if not(assigned(sym) and assigned(sym^.sym))  then
             begin
                rttilist^.concat(new(pai_const,init_32bit(1)));
                typvalue:=3;
             end
           else if sym^.sym^.typ=varsym then
             begin
                rttilist^.concat(new(pai_const,init_32bit(
                  pvarsym(sym^.sym)^.address)));
                typvalue:=0;
             end
           else
             begin
                if not(po_virtualmethod in pprocdef(def)^.procoptions) then
                  begin
                     rttilist^.concat(new(pai_const_symbol,initname(pprocdef(def)^.mangledname)));
                     typvalue:=1;
                  end
                else
                  begin
                     { virtual method, write vmt offset }
                     rttilist^.concat(new(pai_const,init_32bit(
                       pprocdef(def)^._class^.vmtmethodoffset(pprocdef(def)^.extnumber))));
                     typvalue:=2;
                  end;
             end;
           proctypesinfo:=proctypesinfo or (typvalue shl shiftvalue);
        end;

      begin

         if (psym(sym)^.typ=propertysym) and
            (ppo_indexed in ppropertysym(sym)^.propoptions) then
           proctypesinfo:=$40
         else
           proctypesinfo:=0;
         if (psym(sym)^.typ=propertysym) and
            (sp_published in psym(sym)^.symoptions) then
           begin
              rttilist^.concat(new(pai_const_symbol,initname(ppropertysym(sym)^.proptype^.get_rtti_label)));
              writeproc(ppropertysym(sym)^.readaccesssym,ppropertysym(sym)^.readaccessdef,0);
              writeproc(ppropertysym(sym)^.writeaccesssym,ppropertysym(sym)^.writeaccessdef,2);
              { isn't it stored ? }
              if not(ppo_stored in ppropertysym(sym)^.propoptions) then
                begin
                   rttilist^.concat(new(pai_const,init_32bit(1)));
                   proctypesinfo:=proctypesinfo or (3 shl 4);
                end
              else
                writeproc(ppropertysym(sym)^.storedsym,ppropertysym(sym)^.storeddef,4);
              rttilist^.concat(new(pai_const,init_32bit(ppropertysym(sym)^.index)));
              rttilist^.concat(new(pai_const,init_32bit(ppropertysym(sym)^.default)));
              rttilist^.concat(new(pai_const,init_16bit(count)));
              inc(count);
              rttilist^.concat(new(pai_const,init_8bit(proctypesinfo)));
              rttilist^.concat(new(pai_const,init_8bit(length(ppropertysym(sym)^.name))));
              rttilist^.concat(new(pai_string,init(ppropertysym(sym)^.name)));
           end;
      end;


    procedure generate_published_child_rtti(sym : pnamedindexobject);{$ifndef fpc}far;{$endif}
      begin
         if (psym(sym)^.typ=propertysym) and
            (sp_published in psym(sym)^.symoptions) then
           ppropertysym(sym)^.proptype^.get_rtti_label;
      end;


    procedure tobjectdef.write_child_rtti_data;
      begin
         symtable^.foreach({$ifndef TP}@{$endif}generate_published_child_rtti);
      end;


    procedure tobjectdef.generate_rtti;
      begin
         has_rtti:=true;
         getdatalabel(rtti_label);
         write_child_rtti_data;
         rttilist^.concat(new(pai_symbol,initname_global(rtti_name,0)));
         rttilist^.concat(new(pai_label,init(rtti_label)));
         write_rtti_data;
         rttilist^.concat(new(pai_symbol_end,initname(rtti_name)));
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
         get_rtti_label:=rtti_name;
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
  Revision 1.157  1999-08-26 21:13:58  peter
    * array elementsize of 0 doesn't crash anymore

  Revision 1.156  1999/08/17 13:58:56  michael
  RTTI writing patch

  Revision 1.155  1999/08/16 16:26:04  pierre
   * error in stabs for tclassrefdef corrected

  Revision 1.154  1999/08/14 00:38:58  peter
    * hack to support property with record fields

  Revision 1.153  1999/08/13 21:33:11  peter
    * support for array constructors extended and more error checking

  Revision 1.152  1999/08/13 14:24:18  pierre
    + stabs for classes and classref working,
      a class still needs an ^ to get that content of it,
      but the class fields inside a class don't result into an
      infinite loop anymore!

  Revision 1.151  1999/08/12 14:31:20  peter
    * long line fix

  Revision 1.150  1999/08/11 08:56:53  michael
  * RTTI fix from Sebastian Guenther

  Revision 1.149  1999/08/10 13:22:08  pierre
   * vmtmethodoffset made cross target compatible

  Revision 1.148  1999/08/10 12:32:13  pierre
   * avoid overflow in tarraydef.size

  Revision 1.147  1999/08/09 22:19:55  peter
    * classes vmt changed to only positive addresses
    * sharedlib creation is working

  Revision 1.146  1999/08/07 14:21:00  florian
    * some small problems fixed

  Revision 1.145  1999/08/07 13:36:54  daniel
  * Recommitted the arraydef overflow bugfix.

  Revision 1.143  1999/08/06 11:13:30  peter
    * fixed message which was wrong styled

  Revision 1.142  1999/08/05 22:41:34  daniel
  *** empty log message ***

  Revision 1.141  1999/08/05 16:53:13  peter
    * V_Fatal=1, all other V_ are also increased
    * Check for local procedure when assigning procvar
    * fixed comment parsing because directives
    * oldtp mode directives better supported
    * added some messages to errore.msg

  Revision 1.140  1999/08/04 13:03:07  jonas
    * all tokens now start with an underscore
    * PowerPC compiles!!

  Revision 1.139  1999/08/03 22:03:14  peter
    * moved bitmask constants to sets
    * some other type/const renamings

  Revision 1.138  1999/08/02 21:29:02  florian
    * the main branch psub.pas is now used for
      newcg compiler

  Revision 1.137  1999/07/31 22:37:17  michael
  * Fix of initialization information generation

  Revision 1.136  1999/07/29 20:54:07  peter
    * write .size also

  Revision 1.135  1999/07/27 23:42:18  peter
    * indirect type referencing is now allowed

  Revision 1.134  1999/07/23 23:07:03  peter
    * fixed stabs for record which still used savesize

  Revision 1.133  1999/07/23 16:05:28  peter
    * alignment is now saved in the symtable
    * C alignment added for records
    * PPU version increased to solve .12 <-> .13 probs

  Revision 1.132  1999/07/18 14:47:32  florian
    * bug 487 fixed, (inc(<property>) isn't allowed)
    * more fixes to compile with Delphi

  Revision 1.131  1999/07/06 21:48:27  florian
    * a lot bug fixes:
       - po_external isn't any longer necessary for procedure compatibility
       - m_tp_procvar is in -Sd now available
       - error messages of procedure variables improved
       - return values with init./finalization fixed
       - data types with init./finalization aren't any longer allowed in variant
         record

  Revision 1.130  1999/06/22 16:24:44  pierre
   * local browser stuff corrected

  Revision 1.129  1999/06/02 22:44:21  pierre
   * previous wrong log corrected

  Revision 1.128  1999/06/02 22:25:52  pierre
  * changed $ifdef FPC @ into $ifndef TP

  Revision 1.127  1999/06/02 10:26:50  florian
    * corrected order of parameter type for -vb

  Revision 1.126  1999/06/02 10:11:50  florian
    * make cycle fixed i.e. compilation with 0.99.10
    * some fixes for qword
    * start of register calling conventions

  Revision 1.125  1999/06/01 14:45:56  peter
    * @procvar is now always needed for FPC

  Revision 1.124  1999/05/31 16:42:33  peter
    * interfacedef flag for procdef if it's defined in the interface, to
      make a difference with 'forward;' directive forwarddef. Fixes 253

  Revision 1.123  1999/05/27 19:45:02  peter
    * removed oldasm
    * plabel -> pasmlabel
    * -a switches to source writing automaticly
    * assembler readers OOPed
    * asmsymbol automaticly external
    * jumptables and other label fixes for asm readers

  Revision 1.122  1999/05/23 18:42:14  florian
    * better error recovering in typed constants
    * some problems with arrays of const fixed, some problems
      due my previous
       - the location type of array constructor is now LOC_MEM
       - the pushing of high fixed
       - parameter copying fixed
       - zero temp. allocation removed
    * small problem in the assembler writers fixed:
      ref to nil wasn't written correctly

  Revision 1.121  1999/05/21 13:55:19  peter
    * NEWLAB for label as symbol

  Revision 1.120  1999/05/20 22:22:43  pierre
    + added synonym filed for ttypesym
      allows a clean disposal of tdefs and related ttypesyms

  Revision 1.119  1999/05/19 16:48:26  florian
    * tdef.typename: returns a now a proper type name for the most types

  Revision 1.118  1999/05/19 12:08:11  florian
    * tobject wasn't set as default anchestor, was a problem with the new ppu
      handling

  Revision 1.117  1999/05/17 21:57:15  florian
    * new temporary ansistring handling

  Revision 1.116  1999/05/16 02:26:51  peter
    * fixed loading of classrefdef

  Revision 1.115  1999/05/14 17:52:26  peter
    * new deref code

  Revision 1.114  1999/05/13 21:59:41  peter
    * removed oldppu code
    * warning if objpas is loaded from uses
    * first things for new deref writing

  Revision 1.113  1999/05/12 00:19:58  peter
    * removed R_DEFAULT_SEG
    * uniform float names

  Revision 1.112  1999/05/08 19:52:35  peter
    + MessagePos() which is enhanced Message() function but also gets the
      position info
    * Removed comp warnings

  Revision 1.111  1999/05/07 11:06:37  florian
    * enumeration type names are now written in lowercase (rtti)

  Revision 1.110  1999/05/06 09:05:28  peter
    * generic write_float and str_float
    * fixed constant float conversions

  Revision 1.109  1999/05/05 10:05:56  florian
    * a delphi compiled compiler recompiles ppc

  Revision 1.108  1999/04/28 22:30:52  pierre
   * delete -> deleteindex in tdef.correct_owner_symtable

  Revision 1.107  1999/04/28 06:02:11  florian
    * changes of Bruessel:
       + message handler can now take an explicit self
       * typinfo fixed: sometimes the type names weren't written
       * the type checking for pointer comparisations and subtraction
         and are now more strict (was also buggy)
       * small bug fix to link.pas to support compiling on another
         drive
       * probable bug in popt386 fixed: call/jmp => push/jmp
         transformation didn't count correctly the jmp references
       + threadvar support
       * warning if ln/sqrt gets an invalid constant argument

  Revision 1.106  1999/04/26 18:30:01  peter
    * farpointerdef moved into pointerdef.is_far

  Revision 1.105  1999/04/26 13:31:47  peter
    * release storenumber,double_checksum

  Revision 1.104  1999/04/21 09:43:50  peter
    * storenumber works
    * fixed some typos in double_checksum
    + incompatible types type1 and type2 message (with storenumber)

  Revision 1.103  1999/04/19 09:28:20  peter
    * fixed crash when writing overload operator to ppu

  Revision 1.102  1999/04/17 22:01:28  pierre
   * typo error fix in STORENUMBER code

  Revision 1.101  1999/04/14 09:14:58  peter
    * first things to store the symbol/def number in the ppu

  Revision 1.100  1999/04/08 15:57:51  peter
    + subrange checking for readln()

  Revision 1.99  1999/04/07 15:39:32  pierre
    + double_checksum code added

  Revision 1.98  1999/03/06 17:24:16  peter
    * reset savesize in tdef.init

  Revision 1.97  1999/03/01 13:45:04  pierre
   + added staticppusymtable symtable type for local browsing

  Revision 1.96  1999/02/25 21:02:52  peter
    * ag386bin updates
    + coff writer

  Revision 1.95  1999/02/23 18:29:23  pierre
    * win32 compilation error fix
    + some work for local browser (not cl=omplete yet)

  Revision 1.94  1999/02/22 20:13:38  florian
    + first implementation of message keyword

  Revision 1.93  1999/02/22 13:07:07  pierre
    + -b and -bl options work !
    + cs_local_browser ($L+) is disabled if cs_browser ($Y+)
      is not enabled when quitting global section
    * local vars and procedures are not yet stored into PPU

  Revision 1.92  1999/02/17 10:14:20  peter
    * set the first enumsym also for subrange types

  Revision 1.91  1999/02/08 09:51:21  pierre
   * gdb info for local functions was wrong

  Revision 1.90  1999/01/26 09:57:29  pierre
   * open arrays stabs changed

  Revision 1.89  1999/01/22 17:29:30  pierre
   * overflow in addname for open arrays removed

  Revision 1.88  1999/01/20 14:18:39  pierre
    * bugs related to mangledname solved
      - linux external without name
      -external procs already used
      (added count and is_used boolean fiels in tprocvar)

  Revision 1.87  1999/01/19 10:56:05  pierre
   typeof(object) without vmt generates an error instead of an internalerror

  Revision 1.86  1999/01/12 14:25:32  peter
    + BrowserLog for browser.log generation
    + BrowserCol for browser info in TCollections
    * released all other UseBrowser

  Revision 1.85  1998/12/30 22:15:52  peter
    + farpointer type
    * absolutesym now also stores if its far

  Revision 1.84  1998/12/30 13:41:12  peter
    * released valuepara

  Revision 1.83  1998/12/21 14:03:08  pierre
   * procvar stabs correction

  Revision 1.82  1998/12/19 00:23:52  florian
    * ansistring memory leaks fixed

  Revision 1.81  1998/12/11 08:57:22  pierre
   * internal gdb types for booleans and 64bit integers

  Revision 1.80  1998/12/10 09:47:26  florian
    + basic operations with int64/qord (compiler with -dint64)
    + rtti of enumerations extended: names are now written

  Revision 1.79  1998/12/08 10:18:12  peter
    + -gh for heaptrc unit

  Revision 1.78  1998/12/08 09:06:30  pierre
   + constructor destructor info for gdbpas

  Revision 1.77  1998/12/01 23:37:39  pierre
   * function type problem for gdb fix

  Revision 1.76  1998/11/29 21:45:48  florian
    * problem with arrays with init tables fixed

  Revision 1.75  1998/11/29 12:45:59  peter
    * hack for arraydef.size overflow

  Revision 1.74  1998/11/27 14:50:47  peter
    + open strings, $P switch support

  Revision 1.73  1998/11/26 14:47:00  michael
  + Fixed RTTI constants

  Revision 1.72  1998/11/25 14:35:28  florian
    * writting of rtti for properties fixed

  Revision 1.71  1998/11/20 15:35:59  florian
    * problems with rtti fixed, hope it works

  Revision 1.70  1998/11/18 15:44:16  peter
    * VALUEPARA for tp7 compatible value parameters

  Revision 1.69  1998/11/10 17:54:56  peter
    * removed warning

  Revision 1.68  1998/11/05 23:34:36  peter
    * don't dispose staticsymtable (caused crash under tp7 after a fatal
      error)

  Revision 1.67  1998/11/05 12:02:56  peter
    * released useansistring
    * removed -Sv, its now available in fpc modes

  Revision 1.66  1998/10/26 22:58:22  florian
    * new introduded problem with classes fix, the parent class wasn't set
      correct, if the class was defined forward before

  Revision 1.65  1998/10/26 14:19:28  pierre
    + added options -lS and -lT for source and target os output
      (to have a easier way to test OS_SOURCE abd OS_TARGET in makefiles)
    * several problems with rtti data
      (type of sym was not checked)
      assumed to be varsym when they could be procsym or property syms !!

  Revision 1.64  1998/10/22 17:11:21  pierre
    + terminated the include exclude implementation for i386
    * enums inside records fixed

  Revision 1.63  1998/10/20 09:32:56  peter
    * removed some unused vars

  Revision 1.62  1998/10/20 08:06:58  pierre
    * several memory corruptions due to double freemem solved
      => never use p^.loc.location:=p^.left^.loc.location;
    + finally I added now by default
      that ra386dir translates global and unit symbols
    + added a first field in tsymtable and
      a nextsym field in tsym
      (this allows to obtain ordered type info for
      records and objects in gdb !)

  Revision 1.61  1998/10/19 08:55:05  pierre
    * wrong stabs info corrected once again !!
    + variable vmt offset with vmt field only if required
      implemented now !!!

  Revision 1.60  1998/10/16 13:12:53  pierre
    * added vmt_offsets in destructors code also !!!
    * vmt_offset code for m68k

  Revision 1.59  1998/10/16 08:51:51  peter
    + target_os.stackalignment
    + stack can be aligned at 2 or 4 byte boundaries

  Revision 1.58  1998/10/15 15:13:30  pierre
    + added oo_hasconstructor and oo_hasdestructor
      for objects options

  Revision 1.57  1998/10/14 15:54:20  pierre
    * smallsets are not entirely implemented for
      m68k added a ifdef usesmallset
      that is allways defined for i386
      (enables testing for m68k)

  Revision 1.56  1998/10/09 11:47:56  pierre
    * still more memory leaks fixes !!

  Revision 1.55  1998/10/06 17:16:55  pierre
    * some memory leaks fixed (thanks to Peter for heaptrc !)

  Revision 1.54  1998/10/05 21:33:28  peter
    * fixed 161,165,166,167,168

  Revision 1.53  1998/10/05 12:48:39  pierre
    * wrong handling of range check for arrays fixed

  Revision 1.52  1998/10/02 07:20:38  florian
    * range checking in units doesn't work if the units are smartlinked, fixed

  Revision 1.51  1998/09/25 12:01:41  florian
    * tobjectdef.symtable.datasize was set to savesize, this is wrong now
      because the symtable size is read from the ppu file

  Revision 1.50  1998/09/23 15:46:40  florian
    * problem with with and classes fixed

  Revision 1.49  1998/09/23 12:03:55  peter
    * overloading fix for array of const

  Revision 1.48  1998/09/22 15:37:23  peter
    + array of const start

  Revision 1.47  1998/09/21 15:46:01  michael
  Applied florians fix for check_rec_inittable

  Revision 1.46  1998/09/21 08:45:21  pierre
    + added vmt_offset in tobjectdef.write for fututre use
      (first steps to have objects without vmt if no virtual !!)
    + added fpu_used field for tabstractprocdef  :
      sets this level to 2 if the functions return with value in FPU
      (is then set to correct value at parsing of implementation)
      THIS MIGHT refuse some code with FPU expression too complex
      that were accepted before and even in some cases
      that don't overflow in fact
      ( like if f : float; is a forward that finally in implementation
       only uses one fpu register !!)
      Nevertheless I think that it will improve security on
      FPU operations !!
    * most other changes only for UseBrowser code
      (added symtable references for record and objects)
      local switch for refs to args and local of each function
      (static symtable still missing)
      UseBrowser still not stable and probably broken by
      the definition hash array !!

  Revision 1.45  1998/09/20 08:31:29  florian
    + bit 6 of tpropinfo.propprocs is set, if the property contains a
      constant index

  Revision 1.44  1998/09/19 15:23:58  florian
    * rtti for ordtypes corrected

  Revision 1.43  1998/09/18 17:12:40  florian
    * problem with writing of class references fixed

  Revision 1.42  1998/09/17 13:41:20  pierre
  sizeof(TPOINT) problem

  Revision 1.40.2.2  1998/09/17 08:42:33  pierre
  TPOINT sizeof fix

  Revision 1.41  1998/09/15 17:39:30  jonas
    + bugfix from bugfix branch

  Revision 1.40.2.1  1998/09/15 17:35:32  jonas
    * chenged string_typ in tstringdef.wideload from ansistring to widestring

  Revision 1.40  1998/09/09 15:34:00  peter
    * removed warnings

  Revision 1.39  1998/09/08 10:23:44  pierre
    * name field of filedef corrected

  Revision 1.38  1998/09/07 23:10:23  florian
    * a lot of stuff fixed regarding rtti and publishing of properties,
      basics should now work

  Revision 1.37  1998/09/07 19:33:24  florian
    + some stuff for property rtti added:
       - NameIndex of the TPropInfo record is now written correctly
       - the DEFAULT/NODEFAULT keyword is supported now
       - the default value and the storedsym/def are now written to
         the PPU fiel

  Revision 1.36  1998/09/07 17:37:01  florian
    * first fixes for published properties

  Revision 1.35  1998/09/06 22:42:02  florian
    + rtti genreation for properties added

  Revision 1.34  1998/09/04 18:15:02  peter
    * filedef updated

  Revision 1.33  1998/09/03 17:08:49  pierre
    * better lines for stabs
      (no scroll back to if before else part
      no return to case line at jump outside case)
    + source lines also if not in order

  Revision 1.32  1998/09/03 16:03:20  florian
    + rtti generation
    * init table generation changed

  Revision 1.31  1998/09/02 15:14:28  peter
    * enum packing changed from len to max

  Revision 1.30  1998/09/01 17:37:29  peter
    * removed debug writeln :(

  Revision 1.29  1998/09/01 12:53:25  peter
    + aktpackenum

  Revision 1.28  1998/09/01 07:54:22  pierre
    * UseBrowser a little updated (might still be buggy !!)
    * bug in psub.pas in function specifier removed
    * stdcall allowed in interface and in implementation
      (FPC will not yet complain if it is missing in either part
      because stdcall is only a dummy !!)

  Revision 1.27  1998/08/28 12:51:43  florian
    + ansistring to pchar type cast fixed

  Revision 1.26  1998/08/25 12:42:44  pierre
    * CDECL changed to CVAR for variables
      specifications are read in structures also
    + started adding GPC compatibility mode ( option  -Sp)
    * names changed to lowercase

  Revision 1.25  1998/08/23 21:04:38  florian
    + rtti generation for classes added
    + new/dispose do now also a call to INITIALIZE/FINALIZE, if necessaray

  Revision 1.24  1998/08/20 12:53:26  peter
    * object_options are always written for object syms

  Revision 1.23  1998/08/19 00:42:42  peter
    + subrange types for enums
    + checking for bounds type with ranges

  Revision 1.22  1998/08/17 10:10:10  peter
    - removed OLDPPU

  Revision 1.21  1998/08/10 14:50:28  peter
    + localswitches, moduleswitches, globalswitches splitting

  Revision 1.20  1998/07/18 22:54:30  florian
    * some ansi/wide/longstring support fixed:
       o parameter passing
       o returning as result from functions

  Revision 1.19  1998/07/14 14:47:05  peter
    * released NEWINPUT

  Revision 1.18  1998/07/10 10:51:04  peter
    * m68k updates

  Revision 1.16  1998/07/07 11:20:13  peter
    + NEWINPUT for a better inputfile and scanner object

  Revision 1.15  1998/06/24 14:48:37  peter
    * ifdef newppu -> ifndef oldppu

  Revision 1.14  1998/06/16 08:56:31  peter
    + targetcpu
    * cleaner pmodules for newppu

  Revision 1.13  1998/06/15 15:38:09  pierre
    * small bug in systems.pas corrected
    + operators in different units better hanlded

  Revision 1.12  1998/06/15 14:30:12  daniel

  * Reverted my changes.

  Revision 1.10  1998/06/13 00:10:16  peter
    * working browser and newppu
    * some small fixes against crashes which occured in bp7 (but not in
      fpc?!)

  Revision 1.9  1998/06/12 14:10:37  michael
  * Fixed wrong code for ansistring

  Revision 1.8  1998/06/11 10:11:58  peter
    * -gb works again

  Revision 1.7  1998/06/07 15:30:25  florian
    + first working rtti
    + data init/final. for local variables

  Revision 1.6  1998/06/05 14:37:37  pierre
    * fixes for inline for operators
    * inline procedure more correctly restricted

  Revision 1.5  1998/06/04 23:52:01  peter
    * m68k compiles
    + .def file creation moved to gendef.pas so it could also be used
      for win32

  Revision 1.4  1998/06/04 09:55:45  pierre
    * demangled name of procsym reworked to become independant of the mangling
      scheme

  Revision 1.3  1998/06/03 22:49:03  peter
    + wordbool,longbool
    * rename bis,von -> high,low
    * moved some systemunit loading/creating to psystem.pas

  Revision 1.2  1998/05/31 14:13:37  peter
    * fixed call bugs with assembler readers
    + OPR_SYMBOL to hold a symbol in the asm parser
    * fixed staticsymtable vars which were acessed through %ebp instead of
      name

  Revision 1.1  1998/05/27 19:45:09  peter
    * symtable.pas splitted into includefiles
    * symtable adapted for $ifndef OLDPPU

}
