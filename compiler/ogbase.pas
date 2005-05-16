{
    $Id: ogbase.pas,v 1.17 2005/02/14 17:13:06 peter Exp $
    Copyright (c) 1998-2002 by Peter Vreman

    Contains the base stuff for binary object file writers

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
unit ogbase;

{$i fpcdefs.inc}

interface
    uses
       { common }
       cclasses,
       { targets }
       systems,
       { outputwriters }
       owbase,owar,
       { assembler }
       aasmbase,aasmtai;

    type
       tobjectoutput = class
       protected
         { writer }
         FWriter    : tobjectwriter;
         function  writedata(data:TAsmObjectData):boolean;virtual;abstract;
       public
         constructor create(smart:boolean);
         destructor  destroy;override;
         function  newobjectdata(const n:string):TAsmObjectData;virtual;
         function  startobjectfile(const fn:string):boolean;
         function  writeobjectfile(data:TAsmObjectData):boolean;
         procedure exportsymbol(p:tasmsymbol);
         property Writer:TObjectWriter read FWriter;
       end;

       tobjectinput = class
       protected
         { reader }
         FReader    : tobjectreader;
       protected
         function  readobjectdata(data:TAsmObjectData):boolean;virtual;abstract;
       public
         constructor create;
         destructor  destroy;override;
         function  newobjectdata(const n:string):TAsmObjectData;virtual;
         function  readobjectfile(const fn:string;data:TAsmObjectData):boolean;virtual;
         property Reader:TObjectReader read FReader;
       end;

       texesection = class(tnamedindexitem)
       public
         available : boolean;
         secsymidx,
         datasize,
         datapos,
         memsize,
         mempos    : longint;
         flags     : cardinal;
         secdatalist : TLinkedList;
         constructor create(const n:string);
         destructor  destroy;override;
       end;

       texeoutput = class
       private
         procedure Sections_FixUpSymbol(s:tnamedindexitem;arg:pointer);
       protected
         { writer }
         FWriter : tobjectwriter;
         procedure MapObjectdata(var datapos:longint;var mempos:longint);
         function  writedata:boolean;virtual;abstract;
       public
         { info for each section }
         sections     : tdictionary;
         { global symbols }
         externalsyms : tsinglelist;
         commonsyms   : tsinglelist;
         globalsyms   : tdictionary;
         { list of all data of the object files to link }
         objdatalist  : tlinkedlist;
         constructor create;
         destructor  destroy;override;
         function  newobjectinput:tobjectinput;virtual;
         procedure GenerateExecutable(const fn:string);virtual;abstract;
         function  writeexefile(const fn:string):boolean;
         function  CalculateSymbols:boolean;
         procedure CalculateMemoryMap;virtual;abstract;
         procedure addobjdata(objdata:TAsmObjectData);
         procedure FixUpSymbols;
         procedure FixUpRelocations;
         procedure addglobalsym(const name:string;ofs:longint);
         property Writer:TObjectWriter read FWriter;
       end;

    var
      exeoutput : texeoutput;


implementation

    uses
      cutils,globtype,globals,verbose,fmodule,ogmap;



{****************************************************************************
                                tobjectoutput
****************************************************************************}

    constructor tobjectoutput.create(smart:boolean);
      begin
      { init writer }
        if smart and
           not(cs_asm_leave in aktglobalswitches) then
          FWriter:=tarobjectwriter.create(current_module.staticlibfilename^)
        else
          FWriter:=tobjectwriter.create;
      end;


    destructor tobjectoutput.destroy;
      begin
        FWriter.free;
      end;


    function tobjectoutput.newobjectdata(const n:string):TAsmObjectData;
      begin
        result:=TAsmObjectData.create(n);
      end;


    function tobjectoutput.startobjectfile(const fn:string):boolean;
      begin
        result:=false;
        { start the writer already, so the .a generation can initialize
          the position of the current objectfile }
        if not FWriter.createfile(fn) then
         Comment(V_Fatal,'Can''t create object '+fn);
        result:=true;
      end;


    function tobjectoutput.writeobjectfile(data:TAsmObjectData):boolean;
      begin
        if errorcount=0 then
         result:=writedata(data)
        else
         result:=true;
        { close the writer }
        FWriter.closefile;
      end;


    procedure tobjectoutput.exportsymbol(p:tasmsymbol);
      begin
        { export globals and common symbols, this is needed
          for .a files }
        if p.currbind in [AB_GLOBAL,AB_COMMON] then
         FWriter.writesym(p.name);
      end;


{****************************************************************************
                                texesection
****************************************************************************}

    constructor texesection.create(const n:string);
      begin
        inherited createname(n);
        mempos:=0;
        memsize:=0;
        datapos:=0;
        datasize:=0;
        secsymidx:=0;
        available:=false;
        flags:=0;
        secdatalist:=TLinkedList.Create;
      end;


    destructor texesection.destroy;
      begin
      end;


{****************************************************************************
                                texeoutput
****************************************************************************}

    constructor texeoutput.create;
      begin
        { init writer }
        FWriter:=tobjectwriter.create;
        { object files }
        objdatalist:=tlinkedlist.create;
        { symbols }
        globalsyms:=tdictionary.create;
        globalsyms.usehash;
        globalsyms.noclear:=true;
        externalsyms:=tsinglelist.create;
        commonsyms:=tsinglelist.create;
        sections:=tdictionary.create;
      end;


    destructor texeoutput.destroy;
      begin
        sections.free;
        globalsyms.free;
        externalsyms.free;
        commonsyms.free;
        objdatalist.free;
        FWriter.free;
      end;


    function texeoutput.newobjectinput:tobjectinput;
      begin
        result:=tobjectinput.create;
      end;


    function texeoutput.writeexefile(const fn:string):boolean;
      begin
        result:=false;
        if FWriter.createfile(fn) then
         begin
           { Only write the .o if there are no errors }
           if errorcount=0 then
             result:=writedata
           else
             result:=true;
           { close the writer }
           FWriter.closefile;
         end
        else
         Comment(V_Fatal,'Can''t create executable '+fn);
      end;


    procedure texeoutput.addobjdata(objdata:TAsmObjectData);
      begin
        objdatalist.concat(objdata);
      end;


    procedure texeoutput.MapObjectdata(var datapos:longint;var mempos:longint);
{$ifdef needrewrite}
      var
        sec : TSection;
        s   : TAsmSection;
        alignedpos : longint;
        objdata : TAsmObjectData;
      begin
        { calculate offsets of each objdata }
        for sec:=low(TSection) to high(TSection) do
         begin
           if sections[sec].available then
            begin
              { set start position of section }
              sections[sec].datapos:=datapos;
              sections[sec].mempos:=mempos;
              { update objectfiles }
              objdata:=TAsmObjectData(objdatalist.first);
              while assigned(objdata) do
               begin
                 s:=objdata.sects[sec];
                 if assigned(s) then
                  begin
                    { align section }
                    mempos:=align(mempos,$10);
                    if assigned(s.data) then
                     begin
                       alignedpos:=align(datapos,$10);
                       s.dataalignbytes:=alignedpos-datapos;
                       datapos:=alignedpos;
                     end;
                    { set position and size of this objectfile }
                    s.mempos:=mempos;
                    s.datapos:=datapos;
                    inc(mempos,s.datasize);
                    if assigned(s.data) then
                     inc(datapos,s.datasize);
                  end;
                 objdata:=TAsmObjectData(objdata.next);
               end;
              { calculate size of the section }
              sections[sec].datasize:=datapos-sections[sec].datapos;
              sections[sec].memsize:=mempos-sections[sec].mempos;
            end;
         end;
{$endif needrewrite}
      begin
      end;


    procedure texeoutput.Sections_FixUpSymbol(s:tnamedindexitem;arg:pointer);
{$ifdef needrewrite}
      var
        secdata : TAsmSection;
        hsym    : TAsmSymbol;
      begin
        with texesection(s) do
          begin
            if assigned(exemap) then
              exemap.AddMemoryMapExeSection(TExeSection(s));
            secdata:=TAsmSection(secdatalist.first);
            while assigned(secdata) do
             begin
               if assigned(exemap) then
                 exemap.AddMemoryMapObjectSection(secdata);
               hsym:=tasmsymbol(secdata.owner.symbols.first);
               while assigned(hsym) do
                 begin
                   { process only the symbols that are defined in this section
                     and are located in this module }
                   if ((hsym.section=secdata) or
                       ((secdata.sectype=sec_bss) and (hsym.section.sectype=sec_common))) then
                     begin
                       if hsym.currbind=AB_EXTERNAL then
                         internalerror(200206303);
                       inc(hsym.address,secdata.mempos);
                       if assigned(exemap) then
                         exemap.AddMemoryMapSymbol(hsym);
                     end;
                   hsym:=tasmsymbol(hsym.indexnext);
                 end;
               secdata:=TAsmSection(secdata.indexnext);
             end;
          end;
      end;
{$endif needrewrite}
      begin
      end;


    procedure texeoutput.FixUpSymbols;
      var
        sym : tasmsymbol;
      begin
        {
          Fixing up symbols is done in the following steps:
           1. Update addresses
           2. Update common references
           3. Update external references
        }
        { Step 1, Update addresses }
        if assigned(exemap) then
          exemap.AddMemoryMapHeader;
        sections.foreach(@sections_fixupsymbol,nil);
        { Step 2, Update commons }
        sym:=tasmsymbol(commonsyms.first);
        while assigned(sym) do
         begin
           if sym.currbind=AB_COMMON then
            begin
              { update this symbol }
              sym.currbind:=sym.altsymbol.currbind;
              sym.address:=sym.altsymbol.address;
              sym.size:=sym.altsymbol.size;
              sym.section:=sym.altsymbol.section;
              sym.typ:=sym.altsymbol.typ;
              sym.owner:=sym.altsymbol.owner;
            end;
           sym:=tasmsymbol(sym.listnext);
         end;
        { Step 3, Update externals }
        sym:=tasmsymbol(externalsyms.first);
        while assigned(sym) do
         begin
           if sym.currbind=AB_EXTERNAL then
            begin
              { update this symbol }
              sym.currbind:=sym.altsymbol.currbind;
              sym.address:=sym.altsymbol.address;
              sym.size:=sym.altsymbol.size;
              sym.section:=sym.altsymbol.section;
              sym.typ:=sym.altsymbol.typ;
              sym.owner:=sym.altsymbol.owner;
            end;
           sym:=tasmsymbol(sym.listnext);
         end;
      end;


    procedure texeoutput.FixUpRelocations;
      var
        objdata : TAsmObjectData;
      begin
        objdata:=TAsmObjectData(objdatalist.first);
        while assigned(objdata) do
         begin
           objdata.fixuprelocs;
           objdata:=TAsmObjectData(objdata.next);
         end;
      end;


    procedure texeoutput.addglobalsym(const name:string;ofs:longint);
      var
        sym : tasmsymbol;
      begin
        sym:=tasmsymbol(globalsyms.search(name));
        if not assigned(sym) then
         begin
           sym:=tasmsymbol.create(name,AB_GLOBAL,AT_FUNCTION);
           globalsyms.insert(sym);
         end;
        sym.currbind:=AB_GLOBAL;
        sym.address:=ofs;
      end;


    function TExeOutput.CalculateSymbols:boolean;
      var
        commonobjdata,
        objdata : TAsmObjectData;
        sym,p : tasmsymbol;
      begin
        commonobjdata:=nil;
        CalculateSymbols:=true;
        {
          The symbol calculation is done in 3 steps:
           1. register globals
              register externals
              register commons
           2. try to find commons, if not found then
              add to the globals (so externals can be resolved)
           3. try to find externals
        }
        { Step 1, Register symbols }
        objdata:=TAsmObjectData(objdatalist.first);
        while assigned(objdata) do
         begin
           sym:=tasmsymbol(objdata.symbols.first);
           while assigned(sym) do
            begin
              if not assigned(sym.owner) then
               internalerror(200206302);
              case sym.currbind of
                AB_GLOBAL :
                  begin
                    p:=tasmsymbol(globalsyms.search(sym.name));
                    if not assigned(p) then
                      globalsyms.insert(sym)
                    else
                      begin
                        Comment(V_Error,'Multiple defined symbol '+sym.name);
                        result:=false;
                      end;
                  end;
                AB_EXTERNAL :
                  externalsyms.insert(sym);
                AB_COMMON :
                  commonsyms.insert(sym);
              end;
              sym:=tasmsymbol(sym.indexnext);
            end;
           objdata:=TAsmObjectData(objdata.next);
         end;
        { Step 2, Match common symbols or add to the globals }
        sym:=tasmsymbol(commonsyms.first);
        while assigned(sym) do
         begin
           if sym.currbind=AB_COMMON then
            begin
              p:=tasmsymbol(globalsyms.search(sym.name));
              if assigned(p) then
               begin
                 if p.size<>sym.size then
                  internalerror(200206301);
               end
              else
               begin
                 { allocate new symbol in .bss and store it in the
                   *COMMON* module }
                 if not assigned(commonobjdata) then
                  begin
                    if assigned(exemap) then
                      exemap.AddCommonSymbolsHeader;
                    { create .bss section and add to list }
                    commonobjdata:=TAsmObjectData.create('*COMMON*');
                    commonobjdata.createsection(sec_bss,'',0,[aso_alloconly]);
                    addobjdata(commonobjdata);
                  end;
                 p:=TAsmSymbol.Create(sym.name,AB_GLOBAL,AT_FUNCTION);
                 commonobjdata.writesymbol(p);
                 if assigned(exemap) then
                   exemap.AddCommonSymbol(p);
                 { make this symbol available as a global }
                 globalsyms.insert(p);
               end;
              sym.altsymbol:=p;
            end;
           sym:=tasmsymbol(sym.listnext);
         end;
        { Step 3 }
        sym:=tasmsymbol(externalsyms.first);
        while assigned(sym) do
         begin
           if sym.currbind=AB_EXTERNAL then
            begin
              p:=tasmsymbol(globalsyms.search(sym.name));
              if assigned(p) then
               begin
                 sym.altsymbol:=p;
               end
              else
               begin
                 Comment(V_Error,'Undefined symbol: '+sym.name);
                 CalculateSymbols:=false;
               end;
            end;
           sym:=tasmsymbol(sym.listnext);
         end;
      end;


{****************************************************************************
                                tobjectinput
****************************************************************************}

    constructor tobjectinput.create;
      begin
        { init reader }
        FReader:=tobjectreader.create;
      end;


    destructor tobjectinput.destroy;
      begin
        FReader.free;
      end;


    function tobjectinput.newobjectdata(const n:string):TAsmObjectData;
      begin
        result:=TAsmObjectData.create(n);
      end;


    function tobjectinput.readobjectfile(const fn:string;data:TAsmObjectData):boolean;
      begin
        result:=false;
        { start the reader }
        if FReader.openfile(fn) then
         begin
           result:=readobjectdata(data);
           FReader.closefile;
         end;
      end;


end.
{
  $Log: ogbase.pas,v $
  Revision 1.17  2005/02/14 17:13:06  peter
    * truncate log

}
