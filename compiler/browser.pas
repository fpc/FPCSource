{
    $Id$
    Copyright (c) 1996-98 by Florian Klaempfl

    This unit implements a browser object

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
}

unit browser;

interface

uses globals,cobjects,files;

type
  pref = ^tref;
  tref = object
         nextref   : pref;
         posinfo : tfileposinfo;
         moduleindex : word;
         constructor init(ref : pref;pos : pfileposinfo);
         constructor load(var ref : pref;fileindex : word;line,column : longint);
         destructor done; virtual;
         function  get_file_line : string;
         end;

  { simple method to chain all refs }
  procedure add_new_ref(var ref : pref;pos : pfileposinfo);

  function get_source_file(moduleindex,fileindex : word) : pinputfile;

  { one big problem remains for overloaded procedure }
  { we should be able to separate them               }
  { this might be feasable in pass_1                 }

implementation

  uses scanner,verbose;

  constructor tref.init(ref :pref;pos : pfileposinfo);

    begin
       nextref:=nil;
       if ref<>nil then
          ref^.nextref:=@self;
       if assigned(pos) then
         posinfo:=pos^;
       if current_module<>nil then
         begin
            moduleindex:=current_module^.unit_index;
         end;
    end;

  constructor tref.load(var ref : pref;fileindex : word;line,column : longint);

    begin
       moduleindex:=current_module^.unit_index;
       if assigned(ref) then
         ref^.nextref:=@self;
       nextref:=nil;
       posinfo.fileindex:=fileindex;
       posinfo.line:=line;
       posinfo.column:=column;
       ref:=@self;
    end;

  destructor tref.done;

    var
       inputfile : pinputfile;
    begin
       inputfile:=get_source_file(moduleindex,posinfo.fileindex);
       if inputfile<>nil then
         dec(inputfile^.ref_count);
    end;

    function tref.get_file_line : string;

      var
         inputfile : pinputfile;
      begin
        get_file_line:='';
        inputfile:=get_source_file(moduleindex,posinfo.fileindex);
        if assigned(inputfile) then
          if Use_Rhide then
            get_file_line:=globals.lowercase(inputfile^.name^+inputfile^.ext^)
              +':'+tostr(posinfo.line)+':'+tostr(posinfo.column)+':'
          else
            get_file_line:=inputfile^.name^+inputfile^.ext^
              +'('+tostr(posinfo.line)+','+tostr(posinfo.column)+')'
        else
          if Use_Rhide then
            get_file_line:='file_unknown:'
              +tostr(posinfo.line)+':'+tostr(posinfo.column)+':'
          else
            get_file_line:='file_unknown('
              +tostr(posinfo.line)+','+tostr(posinfo.column)+')'
      end;

  procedure add_new_ref(var ref : pref;pos : pfileposinfo);

    var
       newref : pref;

    begin
       new(newref,init(ref,pos));
       ref:=newref;
    end;

    function get_source_file(moduleindex,fileindex : word) : pinputfile;

      var
         hp : pmodule;
         f : pinputfile;

      begin
         hp:=pmodule(loaded_units.first);
         while assigned(hp) and (hp^.unit_index<>moduleindex) do
           hp:=pmodule(hp^.next);
         get_source_file:=nil;
         if not assigned(hp) then
           exit;
         f:=pinputfile(hp^.sourcefiles.files);
         while assigned(f) do
           begin
              if f^.ref_index=fileindex then
                begin
                   get_source_file:=f;
                   exit;
                end;
              f:=pinputfile(f^._next);
           end;
      end;

end.
{
  $Log$
  Revision 1.3  1998-05-20 09:42:32  pierre
    + UseTokenInfo now default
    * unit in interface uses and implementation uses gives error now
    * only one error for unknown symbol (uses lastsymknown boolean)
      the problem came from the label code !
    + first inlined procedures and function work
      (warning there might be allowed cases were the result is still wrong !!)
    * UseBrower updated gives a global list of all position of all used symbols
      with switch -gb

  Revision 1.2  1998/04/30 15:59:39  pierre
    * GDB works again better :
      correct type info in one pass
    + UseTokenInfo for better source position
    * fixed one remaining bug in scanner for line counts
    * several little fixes

  Revision 1.1.1.1  1998/03/25 11:18:12  root
  * Restored version

  Revision 1.5  1998/03/10 16:27:36  pierre
    * better line info in stabs debug
    * symtabletype and lexlevel separated into two fields of tsymtable
    + ifdef MAKELIB for direct library output, not complete
    + ifdef CHAINPROCSYMS for overloaded seach across units, not fully
      working
    + ifdef TESTFUNCRET for setting func result in underfunction, not
      working

  Revision 1.4  1998/03/10 01:17:15  peter
    * all files have the same header
    * messages are fully implemented, EXTDEBUG uses Comment()
    + AG... files for the Assembler generation

  Revision 1.3  1998/03/02 01:48:06  peter
    * renamed target_DOS to target_GO32V1
    + new verbose system, merged old errors and verbose units into one new
      verbose.pas, so errors.pas is obsolete

  Revision 1.2  1998/02/13 10:34:37  daniel
  * Made Motorola version compilable.
  * Fixed optimizer

  Revision 1.1.1.1  1997/11/27 08:32:51  michael
  FPC Compiler CVS start
}

