{
    Copyright (c) 2008 by Jonas Maebe

    Collects all whole program optimization plugin units

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

unit wpo;

{$i fpcdefs.inc}

interface

uses
  { all units with whole program optimisation components }
  optvirt,optdead,compilerbase;


  procedure InitWpo(compiler: TCompilerBase);
  procedure DoneWpo(compiler: TCompilerBase);

implementation

  uses
    globals,
    comphook,compiler,
    wpobase, wpoinfo;

  { called after command line parameters have been parsed }
  procedure InitWpo(compiler: TCompilerBase);
    begin
      { always create so we don't have to litter the source with if-tests }
      tcompiler(compiler).wpoinfomanager:=twpoinfomanager.create(compiler);

      { register the classes we can/should potentially use }
      compiler.wpoinfomanager.registerwpocomponentclass(tprogdevirtinfo);
      compiler.wpoinfomanager.registerwpocomponentclass(twpodeadcodeinfofromexternallinker);

      { assign input/output feedback files }
      if (compiler.globals.wpofeedbackinput<>'') then
        compiler.wpoinfomanager.setwpoinputfile(compiler.globals.wpofeedbackinput);
      if (compiler.globals.wpofeedbackoutput<>'') then
        compiler.wpoinfomanager.setwpooutputfile(compiler.globals.wpofeedbackoutput);

      { parse input }
      compiler.wpoinfomanager.parseandcheckwpoinfo;

      { abort if error }
      if (compiler.verbose.codegenerror) then
        raise ECompilerAbort.Create;
    end;


  procedure DoneWpo(compiler: TCompilerBase);
    begin
      tcompiler(compiler).wpoinfomanager.free;
      tcompiler(compiler).wpoinfomanager:=nil;
      compiler.globals.wpofeedbackinput:='';
      compiler.globals.wpofeedbackoutput:='';
    end;


end.

