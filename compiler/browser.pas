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

uses globals, files;

type
  pref = ^tref;
  tref = object
         nextref   : pref;
         inputfile : pinputfile;
         lineno    : longint;
         constructor init(ref : pref);
         constructor load(var ref : pref;fileindex : word;line : longint);
         destructor done; virtual;
         function  get_file_line : string;
         end;

  { simple method to chain all refs }
  procedure add_new_ref(var ref : pref);

  function get_source_file(index : word) : pinputfile;

  { one big problem remains for overloaded procedure }
  { we should be able to separate them               }
  { this might be feasable in pass_1                 }

implementation

  constructor tref.init(ref :pref);

    begin
       nextref:=nil;
       if ref<>nil then
          ref^.nextref:=@self;
       if current_module<>nil then
         begin
            inputfile:=current_module^.current_inputfile;
            if inputfile<>nil then
              begin
                 inc(inputfile^.ref_index);
                 lineno:=inputfile^.line_no;
              end
            else
              lineno:=0;
         end
       else
         begin
            inputfile:=nil;
            lineno:=0;
         end;
    end;

  constructor tref.load(var ref : pref;fileindex : word;line : longint);

    begin
       if assigned(ref) then
         ref^.nextref:=@self;
       nextref:=nil;
       inputfile:=get_source_file(fileindex);
       lineno:=line;
       ref:=@self;
    end;

  destructor tref.done;

    begin
       if inputfile<>nil then
         dec(inputfile^.ref_count);
    end;

    function tref.get_file_line : string;

      begin
        get_file_line:='';
        if inputfile=nil then exit;
{$ifdef USE_RHIDE}
        get_file_line:=lowercase(inputfile^.name^+inputfile^.ext^)+':'+tostr(lineno)+':'
{$else  USE_RHIDE}
        get_file_line:=inputfile^.name^+inputfile^.ext^+'('+tostr(lineno)+')'
{$endif USE_RHIDE}
      end;

  procedure add_new_ref(var ref : pref);

    var
       newref : pref;

    begin
       new(newref,init(ref));
       ref:=newref;
    end;

    function get_source_file(index : word) : pinputfile;

      var
         f : pinputfile;

      begin
         get_source_file:=nil;
         f:=pinputfile(current_module^.sourcefiles.files);
         while assigned(f) do
           begin
              if f^.ref_index=index then
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
  Revision 1.1  1998-03-25 11:18:12  root
  Initial revision

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

