{
    $Id$
    Copyright (c) 1993-98 by Florian Klaempfl, Pierre Muller

    Symbol table implementation for the defenitions

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
       tkUnknown       = 0;
       tkInteger       = 1;
       tkChar          = 2;
       tkEnumeration   = 3;
       tkFloat         = 4;
       tkSet           = 6;
       tkMethod        = 7;
       tkSString       = 8;
       tkString        = tkSString;
       tkLString       = 9;
       tkAString       = 10;
       tkWString       = 11;
       tkVariant       = 12;
       tkArray         = 13;
       tkRecord        = 14;
       tkInterface     = 15;
       tkClass         = 16;
       tkObject        = 17;
       tkWChar         = 18;
       tkBool          = 19;

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


    constructor tdef.init;
      begin
         deftype:=abstractdef;
         owner := nil;
         next := nil;
         sym := nil;
         indexnb := 0;
         if registerdef then
           symtablestack^.registerdef(@self);
         has_rtti:=false;
         has_inittable:=false;
{$ifdef GDB}
         is_def_stab_written := false;
         globalnb := 0;
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
{$endif GDB}
      end;


    constructor tdef.load;
      begin
         deftype:=abstractdef;
         indexnb := 0;
         sym := nil;
         owner := nil;
         next := nil;
         has_rtti:=false;
         has_inittable:=false;
{$ifdef GDB}
         is_def_stab_written := false;
         globalnb := 0;
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
{$endif GDB}
      end;


    destructor tdef.done;
      begin
{$ifdef GDB}
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
{$endif GDB}
      end;


    procedure tdef.write;
      begin
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
      if not use_dbx then
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
        name : string;
        sym_line_no : longint;
      begin
      ss := stabstring;
      getmem(st,strlen(ss)+512);
      stabchar := 't';
      if deftype in tagtypes then
        stabchar := 'Tt';
      if assigned(sym) then
        begin
           name := sym^.name;
           sym_line_no:=sym^.fileinfo.line;
        end
      else
        begin
           name := ' ';
           sym_line_no:=0;
        end;
      strpcopy(st,'"'+name+':'+stabchar+numberstring+'=');
      strpcopy(strecopy(strend(st),ss),'",'+tostr(N_LSYM)+',0,'+tostr(sym_line_no)+',0');
      allstabstring := strnew(st);
      freemem(st,strlen(ss)+512);
      strdispose(ss);
      end;


    procedure tdef.concatstabto(asmlist : paasmoutput);
     var stab_str : pchar;
    begin
    if ((sym = nil) or sym^.isusedinstab or use_dbx)
      and not is_def_stab_written then
      begin
      If use_dbx then
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


    { rtti generation }
    procedure tdef.generate_rtti;
      begin
         has_rtti:=true;
         getlabel(rtti_label);
         write_child_rtti_data;
         rttilist^.concat(new(pai_label,init(rtti_label)));
         write_rtti_data;
      end;


    function tdef.get_rtti_label : plabel;
      begin
         if not(has_rtti) then
           generate_rtti;
         get_rtti_label:=rtti_label;
      end;


    { init table handling }
    function tdef.needs_inittable : boolean;
      begin
         needs_inittable:=false;
      end;


    procedure tdef.generate_inittable;
      begin
         has_inittable:=true;
         getlabel(inittable_label);
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


    function tdef.get_inittable_label : plabel;
      begin
         if not(has_inittable) then
           generate_inittable;
         get_inittable_label:=inittable_label;
      end;


    procedure tdef.writename;
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


{****************************************************************************
                               TSTRINGDEF
****************************************************************************}

    constructor tstringdef.init(l : byte);
      begin
         tdef.init;
         string_typ:=st_shortstring;
         deftype:=stringdef;
         len:=l;
         savesize:=len+1;
      end;


    constructor tstringdef.load;
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
         savesize:=Sizeof(pointer);
      end;


    constructor tstringdef.longload;
      begin
         tdef.load;
         deftype:=stringdef;
         string_typ:=st_longstring;
         len:=readlong;
         savesize:=Sizeof(pointer);
      end;


    constructor tstringdef.ansiinit(l : longint);
      begin
         tdef.init;
         string_typ:=st_ansistring;
         deftype:=stringdef;
         len:=l;
         savesize:=sizeof(pointer);
      end;


    constructor tstringdef.ansiload;
      begin
         tdef.load;
         deftype:=stringdef;
         string_typ:=st_ansistring;
         len:=readlong;
         savesize:=sizeof(pointer);
      end;


    constructor tstringdef.wideinit(l : longint);
      begin
         tdef.init;
         string_typ:=st_widestring;
         deftype:=stringdef;
         len:=l;
         savesize:=sizeof(pointer);
      end;


    constructor tstringdef.wideload;
      begin
         tdef.load;
         deftype:=stringdef;
         string_typ:=st_widestring;
         len:=readlong;
         savesize:=sizeof(pointer);
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
           st_shortstring : current_ppu^.writeentry(ibstringdef);
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


    procedure tstringdef.write_rtti_data;
      begin
         case string_typ of
            st_ansistring:
              begin
                 rttilist^.concat(new(pai_const,init_8bit(tkAString)));
              end;
            st_widestring:
              begin
                 rttilist^.concat(new(pai_const,init_8bit(tkWString)));
              end;
            st_longstring:
              begin
                 rttilist^.concat(new(pai_const,init_8bit(tkLString)));
              end;
            st_shortstring:
              begin
                 rttilist^.concat(new(pai_const,init_8bit(tkSString)));
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
         first:=nil;
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
         first:=nil;
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
         first:=Nil;
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
        p := first;
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
            p := p^.next;
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
      begin
         rttilist^.concat(new(pai_const,init_8bit(tkEnumeration)));
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
           rttilist^.concat(new(pai_const,init_symbol(strpnew(lab2str(basedef^.get_rtti_label)))))
         else
           rttilist^.concat(new(pai_const,init_32bit(0)));
         {!!!!!!! Name list }
      end;


    function tenumdef.is_publishable : boolean;
      begin
         is_publishable:=true;
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
        uchar,bool8bit : savesize:=1;
         u16bit,s16bit,
             bool16bit : savesize:=2;
         s32bit,u32bit,
             bool32bit : savesize:=4;
             else
               savesize:=0;
             end;
           end;
       { there are no entrys for range checking }
         rangenr:=0;
      end;


    procedure torddef.genrangecheck;
      begin
         if rangenr=0 then
           begin
              { generate two constant for bounds }
              getlabelnr(rangenr);
              if (cs_smartlink in aktmoduleswitches) then
                datasegment^.concat(new(pai_symbol,init_global('R_'+current_module^.modulename^+tostr(rangenr))))
              else
                datasegment^.concat(new(pai_symbol,init('R_'+tostr(rangenr))));
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
                   inc(nextlabelnr);
                   if (cs_smartlink in aktmoduleswitches) then
                     datasegment^.concat(new(pai_symbol,init_global('R_'+current_module^.modulename^+tostr(rangenr+1))))
                   else
                     datasegment^.concat(new(pai_symbol,init('R_'+tostr(rangenr+1))));
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
         bool8bit,
        bool16bit,
        bool32bit : stabstring := strpnew('r'+numberstring+';0;255;');
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
         rttilist^.concat(new(pai_const,init_8bit(byte(trans[typ]))));
         rttilist^.concat(new(pai_const,init_32bit(low)));
         rttilist^.concat(new(pai_const,init_32bit(high)));
      end;


    function torddef.is_publishable : boolean;
      begin
         is_publishable:=typ in [uchar..bool8bit];
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
            s64bit : savesize:=8;
           s80real : savesize:=extended_size;
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
            s64bit : stabstring := strpnew('r'+
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
         translate : array[tfloattype] of byte =
           (ftFixed32,ftSingle,ftDouble,ftExtended,ftComp,ftFixed16);
      begin
         rttilist^.concat(new(pai_const,init_8bit(tkFloat)));
         rttilist^.concat(new(pai_const,init_8bit(translate[typ])));
      end;


    function tfloatdef.is_publishable : boolean;
      begin
         is_publishable:=true;
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
      if ((sym = nil) or sym^.isusedinstab or use_dbx) and not is_def_stab_written then
        begin
        if assigned(typed_as) then forcestabto(asmlist,typed_as);
        inherited concatstabto(asmlist);
        end;
      end;
{$endif GDB}


{****************************************************************************
                               TPOINTERDEF
****************************************************************************}

    constructor tpointerdef.init(def : pdef);
      begin
         inherited init;
         deftype:=pointerdef;
         definition:=def;
         savesize:=Sizeof(pointer);
      end;


    constructor tpointerdef.load;
      begin
         inherited load;
         deftype:=pointerdef;
         { the real address in memory is calculated later (deref) }
         definition:=readdefref;
         savesize:=Sizeof(pointer);
      end;


    procedure tpointerdef.deref;
      begin
         resolvedef(definition);
      end;


    procedure tpointerdef.write;
      begin
         inherited write;
         writedefref(definition);
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
      if ( (sym=nil) or sym^.isusedinstab or use_dbx) and not is_def_stab_written then
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
            forcestabto(asmlist,definition);
            inherited concatstabto(asmlist);
            end;
        end;
      end;
{$endif GDB}


{****************************************************************************
                              TCLASSREFDEF
****************************************************************************}

    constructor tclassrefdef.init(def : pdef);
      begin
         inherited init(def);
         deftype:=classrefdef;
         definition:=def;
         savesize:=Sizeof(pointer);
      end;


    constructor tclassrefdef.load;
      begin
         inherited load;
         deftype:=classrefdef;
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
         stabstring:=strpnew('');
      end;


    procedure tclassrefdef.concatstabto(asmlist : paasmoutput);
      begin
      end;
{$endif GDB}


{***************************************************************************
                                   TSETDEF
***************************************************************************}

    constructor tsetdef.init(s : pdef;high : longint);
      begin
         inherited init;
         deftype:=setdef;
         setof:=s;
         if high<32 then
           begin
              settype:=smallset;
              savesize:=Sizeof(longint);
           end
         else
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
         stabstring := strpnew('S'+setof^.numberstring);
      end;


    procedure tsetdef.concatstabto(asmlist : paasmoutput);
      begin
      if ( not assigned(sym) or sym^.isusedinstab or use_dbx) and
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
         rttilist^.concat(new(pai_const,init_8bit(otULong)));
         rttilist^.concat(new(pai_const,init_symbol(strpnew(lab2str(setof^.get_rtti_label)))));
      end;


    procedure tsetdef.write_child_rtti_data;
      begin
         setof^.get_rtti_label;
      end;


    function tsetdef.is_publishable : boolean;
      begin
         is_publishable:=settype=smallset;
      end;


{***************************************************************************
                                 TFORMALDEF
***************************************************************************}

    constructor tformaldef.init;
      begin
         inherited init;
         deftype:=formaldef;
         savesize:=Sizeof(pointer);
      end;


    constructor tformaldef.load;
      begin
         inherited load;
         deftype:=formaldef;
         savesize:=Sizeof(pointer);
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
         rangenr:=0;
      end;


    procedure tarraydef.genrangecheck;
      begin
         if rangenr=0 then
           begin
              { generates the data for range checking }
              getlabelnr(rangenr);
              datasegment^.concat(new(pai_symbol,init('R_'+tostr(rangenr))));
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
      if (not assigned(sym) or sym^.isusedinstab or use_dbx)
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
         size:=(highrange-lowrange+1)*elesize;
      end;


    function tarraydef.needs_inittable : boolean;
      begin
         needs_inittable:=definition^.needs_inittable;
      end;


    procedure tarraydef.write_child_rtti_table;
      begin
         definition^.get_rtti_label;
      end;


    procedure tarraydef.write_rtti_data;
      begin
         rttilist^.concat(new(pai_const,init_8bit(13)));
         writename;
         { size of elements }
         rttilist^.concat(new(pai_const,init_32bit(definition^.size)));
         { count of elements }
         rttilist^.concat(new(pai_const,init_32bit(highrange-lowrange+1)));
         { element type }
         rttilist^.concat(new(pai_const,init_symbol(strpnew(lab2str(definition^.get_rtti_label)))));
      end;


{***************************************************************************
                                  TRECDEF
***************************************************************************}

    constructor trecdef.init(p : psymtable);
      begin
         inherited init;
         deftype:=recorddef;
         symtable:=p;
         savesize:=symtable^.datasize;
         symtable^.defowner := @self;
      end;


    constructor trecdef.load;
      var
         oldread_member : boolean;
      begin
         inherited load;
         deftype:=recorddef;
         savesize:=readlong;
         oldread_member:=read_member;
         read_member:=true;
         symtable:=new(psymtable,loadasstruct(recordsymtable));
         read_member:=oldread_member;
         symtable^.defowner := @self;
      end;


    destructor trecdef.done;
      begin
         if assigned(symtable) then dispose(symtable,done);
         inherited done;
      end;


    var
       binittable : boolean;
    procedure check_rec_inittable(s : psym);
      begin
         if (s^.typ=varsym) and
            ((pvarsym(s)^.definition^.deftype<>objectdef)
              or not(pobjectdef(pvarsym(s)^.definition)^.isclass)) then
            binittable:=pvarsym(s)^.definition^.needs_inittable;
      end;


    function trecdef.needs_inittable : boolean;
      var
         oldb : boolean;
      begin
         { there are recursive calls to needs_rtti possible, }
         { so we have to change to old value how else should }
         { we do that ? check_rec_rtti can't be a nested     }
         { procedure of needs_rtti !                         }
         oldb:=binittable;
         binittable:=false;
         symtable^.foreach(check_rec_inittable);
         needs_inittable:=binittable;
         binittable:=oldb;
      end;


    procedure trecdef.deref;
      var
         hp : pdef;
         oldrecsyms : psymtable;
      begin
         oldrecsyms:=aktrecordsymtable;
         aktrecordsymtable:=symtable;
         { now dereference the definitions }
         hp:=symtable^.rootdef;
         while assigned(hp) do
           begin
              hp^.deref;
              { set owner }
              hp^.owner:=symtable;
              hp:=hp^.next;
           end;
         {$ifdef tp}
           symtable^.foreach(derefsym);
         {$else}
           symtable^.foreach(@derefsym);
         {$endif}
         aktrecordsymtable:=oldrecsyms;
      end;


    procedure trecdef.write;
      var
         oldread_member : boolean;
      begin
         oldread_member:=read_member;
         read_member:=true;
         inherited write;
         writelong(savesize);
         current_ppu^.writeentry(ibrecorddef);
         self.symtable^.writeasstruct;
         read_member:=oldread_member;
      end;


{$ifdef GDB}
    Const StabRecString : pchar = Nil;
          StabRecSize : longint = 0;
          RecOffset : Longint = 0;

    procedure addname(p : psym);
    var
      news, newrec : pchar;
    begin
    { static variables from objects are like global objects }
    if ((p^.properties and sp_static)<>0) then
      exit;
    If p^.typ = varsym then
       begin
       newrec := strpnew(p^.name+':'+pvarsym(p)^.definition^.numberstring
                     +','+tostr(pvarsym(p)^.address*8)+','
                     +tostr(pvarsym(p)^.definition^.size*8)+';');
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


    function trecdef.stabstring : pchar;
      Var oldrec : pchar;
          oldsize : longint;
      begin
        oldrec := stabrecstring;
        oldsize:=stabrecsize;
        GetMem(stabrecstring,memsizeinc);
        stabrecsize:=memsizeinc;
        strpcopy(stabRecString,'s'+tostr(savesize));
        RecOffset := 0;
        {$ifdef tp}
          symtable^.foreach(addname);
        {$else}
          symtable^.foreach(@addname);
        {$endif}
        { FPC doesn't want to convert a char to a pchar}
        { is this a bug ? }
        strpcopy(strend(StabRecString),';');
        stabstring := strnew(StabRecString);
        Freemem(stabrecstring,stabrecsize);
        stabrecstring := oldrec;
        stabrecsize:=oldsize;
      end;


    procedure trecdef.concatstabto(asmlist : paasmoutput);
      begin
        if (not assigned(sym) or sym^.isusedinstab or use_dbx) and
           (not is_def_stab_written) then
          inherited concatstabto(asmlist);
      end;

{$endif GDB}

    var
       count : longint;
    procedure count_inittable_fields(sym : psym);{$ifndef fpc}far;{$endif}
      begin
         if pvarsym(sym)^.definition^.needs_inittable then
           inc(count);
      end;


    procedure count_fields(sym : psym);{$ifndef fpc}far;{$endif}
      begin
         inc(count);
      end;


    procedure write_field_inittable(sym : psym);{$ifndef fpc}far;{$endif}
      begin
         if pvarsym(sym)^.definition^.needs_inittable then
           begin
              rttilist^.concat(new(pai_const,init_symbol(strpnew(lab2str(pvarsym(sym)^.definition^.get_inittable_label)))));
              rttilist^.concat(new(pai_const,init_32bit(pvarsym(sym)^.address)));
           end;
      end;


    procedure write_field_rtti(sym : psym);{$ifndef fpc}far;{$endif}
      begin
         rttilist^.concat(new(pai_const,init_symbol(strpnew(lab2str(pvarsym(sym)^.definition^.get_rtti_label)))));
         rttilist^.concat(new(pai_const,init_32bit(pvarsym(sym)^.address)));
      end;


    procedure generate_child_inittable(sym : psym);{$ifndef fpc}far;{$endif}
      begin
         if (sym^.typ=varsym) and pvarsym(sym)^.definition^.needs_inittable then
         { force inittable generation }
           pvarsym(sym)^.definition^.get_inittable_label;
      end;


    procedure generate_child_rtti(sym : psym);{$ifndef fpc}far;{$endif}
      begin
         pvarsym(sym)^.definition^.get_rtti_label;
      end;


    procedure trecdef.write_child_rtti_data;
      begin
         symtable^.foreach(generate_child_rtti);
      end;


    procedure trecdef.write_child_init_data;
      begin
         symtable^.foreach(generate_child_inittable);
      end;


    procedure trecdef.write_rtti_data;
      begin
         rttilist^.concat(new(pai_const,init_8bit(14)));
         writename;
         rttilist^.concat(new(pai_const,init_32bit(size)));
         count:=0;
         symtable^.foreach(count_fields);
         rttilist^.concat(new(pai_const,init_32bit(count)));
         symtable^.foreach(write_field_rtti);
      end;


    procedure trecdef.write_init_data;
      begin
         rttilist^.concat(new(pai_const,init_8bit(14)));
         writename;
         rttilist^.concat(new(pai_const,init_32bit(size)));
         count:=0;
         symtable^.foreach(count_inittable_fields);
         rttilist^.concat(new(pai_const,init_32bit(count)));
         symtable^.foreach(write_field_inittable);
      end;


{***************************************************************************
                       TABSTRACTPROCDEF
***************************************************************************}

    constructor tabstractprocdef.init;

      begin
         inherited init;
         para1:=nil;
         fpu_used:=0;
         options:=0;
         retdef:=voiddef;
         savesize:=Sizeof(pointer);
      end;



    destructor tabstractprocdef.done;
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
         inherited done;
      end;


    procedure tabstractprocdef.concatdef(p : pdef;vsp : tvarspez);
      var
         hp : pdefcoll;
      begin
         new(hp);
         hp^.paratyp:=vsp;
         hp^.data:=p;
         hp^.next:=para1;
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
         options:=readlong;
         count:=readword;
         para1:=nil;
         savesize:=Sizeof(pointer);
         for i:=1 to count do
           begin
              new(hp);
              hp^.paratyp:=tvarspez(readbyte);
              hp^.data:=readdefref;
              hp^.next:=nil;
              if para1=nil then
                para1:=hp
              else
                last^.next:=hp;
              last:=hp;
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
                vs_value :
                  l:=l+pdc^.data^.size+(pdc^.data^.size mod 2);
                vs_var :
                  l:=l+sizeof(pointer);
                vs_const :
                  if dont_copy_const_param(pdc^.data) then
                    l:=l+sizeof(pointer)
                  else
                    l:=l+pdc^.data^.size+(pdc^.data^.size mod 2);
                end;
              pdc:=pdc^.next;
           end;
         para_size:=l;
      end;


    procedure tabstractprocdef.write;
      var
         count : word;
         hp : pdefcoll;
      begin
         inherited write;
         writedefref(retdef);
         writebyte(fpu_used);
         writelong(options);
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
              writedefref(hp^.data);
              hp:=hp^.next;
           end;
      end;


    function tabstractprocdef.demangled_paras : string;
      var s : string;
          p : pdefcoll;
      begin
        s:='';
        p:=para1;
        if assigned(p) then
          begin
             s:=s+'(';
             while assigned(p) do
               begin
                  if assigned(p^.data^.sym) then
                    s:=s+p^.data^.sym^.name
                  else if p^.paratyp=vs_var then
                    s:=s+'var'
                  else if p^.paratyp=vs_const then
                    s:=s+'const';
                  p:=p^.next;
                  if assigned(p) then
                    s:=s+','
                  else
                    s:=s+')';
               end;
          end;
        demangled_paras:=s;
      end;


{$ifdef GDB}
    function tabstractprocdef.stabstring : pchar;
      begin
        stabstring := strpnew('abstractproc'+numberstring+';');
      end;


    procedure tabstractprocdef.concatstabto(asmlist : paasmoutput);
      begin
         if (not assigned(sym) or sym^.isusedinstab or use_dbx)
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
         extnumber:=-1;
         localst:=new(psymtable,init(localsymtable));
         parast:=new(psymtable,init(parasymtable));
         { this is used by insert
          to check same names in parast and localst }
         localst^.next:=parast;
{$ifdef UseBrowser}
         defref:=nil;
         lastwritten:=nil;
         refcount:=0;
         if (cs_browser in aktmoduleswitches) and make_ref then
          begin
            defref:=new(pref,init(defref,@tokenpos));
            inc(refcount);
          end;
         lastref:=defref;
{$endif UseBrowser}
       { first, we assume, that all registers are used }
{$ifdef i386}
         usedregisters:=$ff;
{$endif i386}
{$ifdef m68k}
         usedregisters:=$FFFF;
{$endif}
{$ifdef alpha}
         usedregisters_int:=$ffffffff;
         usedregisters_fpu:=$ffffffff;
{$endif alpha}
         forwarddef:=true;
         _class := nil;
         code:=nil;
      end;


    constructor tprocdef.load;
      var
         s : string;
      begin
         inherited load;
         deftype:=procdef;
{$ifdef i386}
         usedregisters:=readbyte;
{$endif i386}
{$ifdef m68k}
         usedregisters:=readword;
{$endif}
{$ifdef alpha}
         usedregisters_int:=readlong;
         usedregisters_fpu:=readlong;
{$endif alpha}

         s:=readstring;
         setstring(_mangledname,s);



         extnumber:=readlong;
         nextoverloaded:=pprocdef(readdefref);
         _class := pobjectdef(readdefref);

         if (cs_link_deffile in aktglobalswitches) and ((options and poexports)<>0) then
           deffile.AddExport(mangledname);

         parast:=nil;
         localst:=nil;
         forwarddef:=false;
{$ifdef UseBrowser}
         lastref:=nil;
         lastwritten:=nil;
         defref:=nil;
         refcount:=0;
{$endif UseBrowser}
      end;


{$ifdef UseBrowser}
    procedure tprocdef.load_references;
      var
        pos : tfileposinfo;
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
        if (current_module^.flags and uf_local_browser)<>0 then
          begin
             new(parast,load);
             parast^.load_browser;
             new(localst,load);
             localst^.load_browser;
          end;
      end;


    function tprocdef.write_references : boolean;
      var
        ref : pref;
        move_last : boolean;
      begin
        move_last:=lastwritten=lastref;
        if move_last and ((current_module^.flags and uf_local_browser)=0) then
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
        if (current_module^.flags and uf_local_browser)<>0 then
          begin
             { we need dummy para and local symtables
               PPU files are then easier to read PM }
             if not assigned(parast) then
               parast:=new(psymtable,init(parasymtable));
             parast^.write;
             parast^.write_browser;
             if not assigned(localst) then
               localst:=new(psymtable,init(localsymtable));
             localst^.write;
             localst^.write_browser;
          end;
      end;


    procedure tprocdef.add_to_browserlog;
      begin
         if assigned(defref) then
          begin
            Browse.AddLog('***'+mangledname);
            Browse.AddLogRefs(defref);
            if (current_module^.flags and uf_local_browser)<>0 then
              begin
                 if assigned(parast) then
                   parast^.writebrowserlog;
                 if assigned(localst) then
                   localst^.writebrowserlog;
              end;
          end;
      end;
{$endif UseBrowser}


    destructor tprocdef.done;
      begin
{$ifdef UseBrowser}
         if assigned(defref) then
           dispose(defref,done);
{$endif UseBrowser}
         if assigned(parast) then
           dispose(parast,done);
         if assigned(localst) then
           dispose(localst,done);
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
{$ifdef i386}
         writebyte(usedregisters);
{$endif i386}
{$ifdef m68k}
         writeword(usedregisters);
{$endif}
{$ifdef alpha}
         writelong(usedregisters_int);
         writelong(usedregisters_fpu);
{$endif alpha}
         writestring(mangledname);
         writelong(extnumber);
         if (options and pooperator) = 0 then
           writedefref(nextoverloaded)
         else
           begin
              { only write the overloads from the same unit }
              if nextoverloaded^.owner=owner then
                writedefref(nextoverloaded)
              else
                writedefref(nil);
           end;
         writedefref(_class);
         if (options and poinline) <> 0 then
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
          vartyp : char;
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
        if assigned(parast) then
          {$IfDef TP}
          parast^.foreach(addparaname)
          {$Else}
          parast^.foreach(@addparaname)
          {$EndIf}
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
          end;
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
          mangledname:=strpas(_mangledname);
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
        s2 := _class^.name^;
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
{$ifdef UseBrowser}
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
{$endif UseBrowser}
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
         if (options and pomethodpointer)=0 then
           size:=sizeof(pointer)
         else
           size:=2*sizeof(pointer);
      end;


{$ifdef GDB}
    function tprocvardef.stabstring : pchar;
      var
         nss : pchar;
         i : word;
         vartyp : char;
         pst : pchar;
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
        strpcopy(nss,'f'+retdef^.numberstring+','+tostr(i)+';');
        param := para1;
        i := 0;
        while assigned(param) do
          begin
          inc(i);
          if param^.paratyp = vs_value then vartyp := '1' else vartyp := '0';
          {Here we have lost the parameter names !!}
          pst := strpnew('p'+tostr(i)+':'+param^.data^.numberstring+','+vartyp+';');
          strcat(nss,pst);
          strdispose(pst);
          param := param^.next;
          end;
        {strpcopy(strend(nss),';');}
        stabstring := strnew(nss);
        freemem(nss,1024);
      end;


    procedure tprocvardef.concatstabto(asmlist : paasmoutput);
      begin
         if ( not assigned(sym) or sym^.isusedinstab or use_dbx)
           and not is_def_stab_written then
           inherited concatstabto(asmlist);
         is_def_stab_written:=true;
      end;
{$endif GDB}


    procedure tprocvardef.write_rtti_data;
      begin
         {!!!!!!!}
      end;


    procedure tprocvardef.write_child_rtti_data;
      begin
         {!!!!!!!!}
      end;


    function tprocvardef.is_publishable : boolean;
      begin
         is_publishable:=(options and pomethodpointer)<>0;
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
        childof:=c;
        options:=0;
        vmt_offset:=0;
        { some options are inherited !! }
        if assigned(c) then
          options:= c^.options and
                    (oo_hasvirtual or oo_hasprivate or
                     oo_hasprotected
                     {or oo_can_have_published treated in pdecl }
                     );
        { privatesyms:=new(psymtable,init(objectsymtable));
      protectedsyms:=new(psymtable,init(objectsymtable)); }
        publicsyms:=new(psymtable,init(objectsymtable));
        publicsyms^.name := stringdup(n);
        { add the data of the anchestor class }
        if assigned(childof) then
          begin
             publicsyms^.datasize:=
               publicsyms^.datasize-4+childof^.publicsyms^.datasize;
          end;
        name:=stringdup(n);
        savesize := publicsyms^.datasize;
        publicsyms^.defowner:=@self;
     end;


    constructor tobjectdef.load;
      var
         oldread_member : boolean;
      begin
         tdef.load;
         deftype:=objectdef;
         savesize:=readlong;
         vmt_offset:=readlong;
         name:=stringdup(readstring);
         childof:=pobjectdef(readdefref);
         options:=readlong;
         oldread_member:=read_member;
         read_member:=true;
         object_options:=true;
         publicsyms:=new(psymtable,loadasstruct(objectsymtable));
         object_options:=false;
         read_member:=oldread_member;
         publicsyms^.defowner:=@self;
         publicsyms^.datasize:=savesize;
         publicsyms^.name := stringdup(name^);

         { handles the predefined class tobject  }
         { the last TOBJECT which is loaded gets }
         { it !                                  }
         if (name^='TOBJECT') and not(cs_compilesystem in aktmoduleswitches) and
           isclass and (childof=pointer($ffffffff)) then
           class_tobject:=@self;
      end;


   procedure tobjectdef.check_forwards;
     begin
        publicsyms^.check_forwards;
        if (options and oo_isforward)<>0 then
          begin
             { ok, in future, the forward can be resolved }
             Message1(sym_e_class_forward_not_resolved,name^);
             options:=options and not(oo_isforward);
          end;
     end;


   destructor tobjectdef.done;
     begin
{!!!!
        if assigned(privatesyms) then
          dispose(privatesyms,done);
        if assigned(protectedsyms) then
          dispose(protectedsyms,done); }
        if assigned(publicsyms) then
          dispose(publicsyms,done);
        if (options and oo_isforward)<>0 then
         Message1(sym_e_class_forward_not_resolved,name^);
        stringdispose(name);
        tdef.done;
     end;


   { true, if self inherits from d (or if they are equal) }
   function tobjectdef.isrelated(d : pobjectdef) : boolean;
     var
        hp : pobjectdef;
     begin
        hp:=@self;
        while assigned(hp) do
          begin
             if hp=d then
               begin
                  isrelated:=true;
                  exit;
               end;
             hp:=hp^.childof;
          end;
        isrelated:=false;
     end;


   function tobjectdef.size : longint;
     begin
        if (options and oois_class)<>0 then
          size:=sizeof(pointer)

        else
          size:=publicsyms^.datasize;
     end;


    procedure tobjectdef.deref;
      var
         hp : pdef;
         oldrecsyms : psymtable;
      begin
         resolvedef(pdef(childof));
         oldrecsyms:=aktrecordsymtable;
         aktrecordsymtable:=publicsyms;
         { nun die Definitionen dereferenzieren }
         hp:=publicsyms^.rootdef;
         while assigned(hp) do
           begin
              hp^.deref;

              { set owner }
              hp^.owner:=publicsyms;

              hp:=hp^.next;
           end;
{$ifdef tp}
         publicsyms^.foreach(derefsym);
{$else}
         publicsyms^.foreach(@derefsym);
{$endif}
         aktrecordsymtable:=oldrecsyms;
      end;


    function tobjectdef.vmt_mangledname : string;
    {DM: I get a nil pointer on the owner name. I don't know if this
     mayhappen, and I have therefore fixed the problem by doing nil pointer
     checks.}
    var
      s1,s2:string;
    begin
        if owner^.name=nil then
            s1:=''
        else
            s1:=owner^.name^;
        if name=nil then
            s2:=''
        else
            s2:=name^;
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
       if name=nil then
         s2:=''
       else
         s2:=name^;
       rtti_name:='RTTI_'+s1+'$_'+s2;
    end;


    function tobjectdef.isclass : boolean;
      begin
         isclass:=(options and oois_class)<>0;
      end;


    procedure tobjectdef.write;
      var
         oldread_member : boolean;
      begin
         tdef.write;
         writelong(size);
         writelong(vmt_offset);
         writestring(name^);
         writedefref(childof);
         writelong(options);
         current_ppu^.writeentry(ibobjectdef);

         oldread_member:=read_member;
         read_member:=true;
         object_options:=true;
         publicsyms^.writeasstruct;
         object_options:=false;
         read_member:=oldread_member;
      end;


{$ifdef GDB}
    procedure addprocname(p :psym);
    var virtualind,argnames : string;
        news, newrec : pchar;
        pd,ipd : pprocdef;
        lindex : longint;
        para : pdefcoll;
        arglength : byte;
        sp : char;

    begin
      If p^.typ = procsym then
       begin
                pd := pprocsym(p)^.definition;
                { this will be used for full implementation of object stabs
                not yet done }
                ipd := pd;
                while assigned(ipd^.nextoverloaded) do ipd := ipd^.nextoverloaded;
                if (pd^.options and povirtualmethod) <> 0 then
                   begin
                   lindex := pd^.extnumber;
                   {doesnt seem to be necessary
                   lindex := lindex or $80000000;}
                   virtualind := '*'+tostr(lindex)+';'+ipd^._class^.numberstring+';'
                   end else virtualind := '.';
                { arguments are not listed here }
                {we don't need another definition}
                 para := pd^.para1;
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
                if (p^.properties and sp_private)<>0 then sp:='0'
                else if (p^.properties and sp_protected)<>0 then sp:='1'
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
        strpcopy(stabRecString,'s'+tostr(size));
        if assigned(childof) then
          {only one ancestor not virtual, public, at base offset 0 }
          {       !1           ,    0       2         0    ,       }
          strpcopy(strend(stabrecstring),'!1,020,'+childof^.numberstring+';');
        {virtual table to implement yet}
        RecOffset := 0;
      {$ifdef tp}
         publicsyms^.foreach(addname);
      {$else}
         publicsyms^.foreach(@addname);
      {$endif tp}
      if (options and oo_hasvirtual) <> 0 then
        if not assigned(childof) or ((childof^.options and oo_hasvirtual) = 0) then
           begin
              str_end:='$vf'+numberstring+':'+typeglobalnumber('vtblarray')+',0;';
              strpcopy(strend(stabrecstring),'$vf'+numberstring+':'+typeglobalnumber('vtblarray')+',0;');
           end;
        {$ifdef tp}
          publicsyms^.foreach(addprocname);
        {$else}
          publicsyms^.foreach(@addprocname);
        {$endif tp }
        if (options and oo_hasvirtual) <> 0  then
          begin
             anc := @self;
             while assigned(anc^.childof) and ((anc^.childof^.options and oo_hasvirtual) <> 0) do
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
      end;


    procedure tobjectdef.write_init_data;
      begin
         if isclass then
           rttilist^.concat(new(pai_const,init_8bit(tkclass)))
         else
           rttilist^.concat(new(pai_const,init_8bit(tkobject)));

         { generate the name }
         rttilist^.concat(new(pai_const,init_8bit(length(name^))));
         rttilist^.concat(new(pai_string,init(name^)));

         rttilist^.concat(new(pai_const,init_32bit(size)));
         count:=0;
         publicsyms^.foreach(count_inittable_fields);
         rttilist^.concat(new(pai_const,init_32bit(count)));
         publicsyms^.foreach(write_field_inittable);
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
         publicsyms^.foreach(check_rec_inittable);
         needs_inittable:=binittable;
         binittable:=oldb;
      end;


    procedure count_published_properties(sym : psym);{$ifndef fpc}far;{$endif}
      begin
         if (sym^.typ=propertysym) and ((sym^.properties and sp_published)<>0) then
           inc(count);
      end;


    procedure write_property_info(sym : psym);{$ifndef fpc}far;{$endif}
      var
         proctypesinfo : byte;

      procedure writeproc(sym : psym;def : pdef;shiftvalue : byte);
        var
           typvalue : byte;
        begin
           if not(assigned(sym)) then
             begin
                rttilist^.concat(new(pai_const,init_32bit(1)));
                typvalue:=3;
             end
           else if sym^.typ=varsym then
             begin
                rttilist^.concat(new(pai_const,init_32bit(
                  pvarsym(sym)^.address)));
                typvalue:=0;
             end
           else
             begin
                if (pprocdef(def)^.options and povirtualmethod)=0 then
                  begin
                     rttilist^.concat(new(pai_const,init_symbol(strpnew(pprocdef(def)^.mangledname))));
                     typvalue:=1;
                  end
                else
                  begin
                     { virtual method, write vmt offset }
                     rttilist^.concat(new(pai_const,init_32bit(pprocdef(def)^.extnumber*4+12)));
                     typvalue:=2;
                  end;
             end;
           proctypesinfo:=proctypesinfo or (typvalue shl shiftvalue);
        end;

      begin

         if (ppropertysym(sym)^.options and ppo_indexed)<>0 then
           proctypesinfo:=$40
         else
           proctypesinfo:=0;
         if (sym^.typ=propertysym) and ((sym^.properties and sp_published)<>0) then
           begin
              rttilist^.concat(new(pai_const,init_symbol(strpnew(lab2str(ppropertysym(sym)^.proptype^.get_rtti_label)))));
              writeproc(ppropertysym(sym)^.readaccesssym,ppropertysym(sym)^.readaccessdef,0);
              writeproc(ppropertysym(sym)^.writeaccesssym,ppropertysym(sym)^.writeaccessdef,2);
              { isn't it stored ? }
              if (ppropertysym(sym)^.options and ppo_stored)=0 then
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


    procedure generate_published_child_rtti(sym : psym);{$ifndef fpc}far;{$endif}
      begin
         if (sym^.typ=propertysym) and ((sym^.properties and sp_published)<>0) then
           ppropertysym(sym)^.proptype^.get_rtti_label;
      end;


    procedure tobjectdef.write_child_rtti_data;
      begin
         publicsyms^.foreach(generate_published_child_rtti);
      end;


    procedure tobjectdef.generate_rtti;
      begin
         has_rtti:=true;
         getlabel(rtti_label);
         write_child_rtti_data;
         rttilist^.concat(new(pai_symbol,init_global(rtti_name)));
         rttilist^.concat(new(pai_label,init(rtti_label)));
         write_rtti_data;
      end;


    function tobjectdef.next_free_name_index : longint;
      var
         i : longint;
      begin
         if assigned(childof) and ((childof^.options and oo_can_have_published)<>0) then
           i:=childof^.next_free_name_index
         else
           i:=0;
         count:=0;
         publicsyms^.foreach(count_published_properties);
         next_free_name_index:=i+count;
      end;


    procedure tobjectdef.write_rtti_data;
      begin
         if isclass then
           rttilist^.concat(new(pai_const,init_8bit(tkclass)))
         else
           rttilist^.concat(new(pai_const,init_8bit(tkobject)));

         { generate the name }
         rttilist^.concat(new(pai_const,init_8bit(length(name^))));
         rttilist^.concat(new(pai_string,init(name^)));

         { write class type }
         rttilist^.concat(new(pai_const,init_symbol(strpnew(vmt_mangledname))));

         { write owner typeinfo }
         if assigned(childof) and ((childof^.options and oo_can_have_published)<>0) then
           rttilist^.concat(new(pai_const,init_symbol(strpnew(lab2str(childof^.get_rtti_label)))))
         else
           rttilist^.concat(new(pai_const,init_32bit(0)));

         { write published properties count }
         count:=0;
         publicsyms^.foreach(count_published_properties);
         rttilist^.concat(new(pai_const,init_16bit(count)));

         { write unit name }
         if assigned(owner^.name) then
           begin
              rttilist^.concat(new(pai_const,init_8bit(length(owner^.name^))));
              rttilist^.concat(new(pai_string,init(owner^.name^)));
           end
         else
           rttilist^.concat(new(pai_const,init_8bit(0)));

         { count is used to write nameindex   }
         { but we need an offset of the owner }
         { to give each property an own slot  }
         if assigned(childof) and ((childof^.options and oo_can_have_published)<>0) then
           count:=childof^.next_free_name_index
         else
           count:=0;
         publicsyms^.foreach(write_property_info);
      end;


    function tobjectdef.is_publishable : boolean;
      begin
         is_publishable:=isclass;
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

{
  $Log$
  Revision 1.48  1998-09-22 15:37:23  peter
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
