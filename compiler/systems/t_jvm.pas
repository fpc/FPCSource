{
    Copyright (c) 2010 by Jonas Maebe

    This unit implements support import,export,link routines
    for the JVM target

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
unit t_jvm;

{$i fpcdefs.inc}

interface


implementation

  uses
    sysutils,
    cutils,cfileutl,cclasses,
    verbose,systems,globtype,globals,
    symconst,script,
    fmodule,aasmbase,aasmtai,aasmdata,aasmcpu,cpubase,symsym,symdef,
    import,export,link,comprsrc,rescmn,i_jvm,
    cgutils,cgbase,cgobj,cpuinfo,ogbase;

  type
    timportlibjvm=class(timportlib)
      procedure generatelib;override;
    end;

    texportlibjvm=class(texportlib)
    end;

    tlinkerjvm=class(texternallinker)
      constructor Create;override;
      function  MakeExecutable:boolean;override;
      function  MakeSharedLibrary:boolean;override;
    end;



{*****************************************************************************
                             TIMPORTLIBJVM
*****************************************************************************}

    procedure timportlibjvm.generatelib;
      begin
      end;


{*****************************************************************************
                             TEXPORTLIBJVM
*****************************************************************************}

{*****************************************************************************
                              TLINKERJVM
*****************************************************************************}

Constructor  tlinkerjvm.Create;
begin
  Inherited Create;
end;


function  tlinkerjvm.MakeExecutable:boolean;
begin
  result:=true;
end;


Function  tlinkerjvm.MakeSharedLibrary:boolean;
begin
  result:=false;
end;


{*****************************************************************************
                                     Initialize
*****************************************************************************}

initialization
  RegisterLinker(ld_jvm, tlinkerjvm);
  RegisterImport(system_jvm_java32,timportlibjvm);
  RegisterExport(system_jvm_java32,texportlibjvm);
  RegisterTarget(system_jvm_java32_info);

  RegisterImport(system_jvm_android32,timportlibjvm);
  RegisterExport(system_jvm_android32,texportlibjvm);
  RegisterTarget(system_jvm_android32_info);

  RegisterRes(res_jvmraw_info,TJVMRawResourceFile);
end.
