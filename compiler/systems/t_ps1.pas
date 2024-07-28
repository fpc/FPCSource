{
    Copyright (c) 2024 by Kirill Kranz

    This unit implements support link routines
    for the (MIPSEL) PS1 target

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
unit t_ps1;

{$i fpcdefs.inc}

interface

  uses
    link;

  type


    TLinkerPS1=class(TExternalLinker)
    private
      Function  WriteResponseFile(isdll:boolean) : Boolean;
    public
      constructor Create;override;
      procedure SetDefaultInfo;override;
      function  MakeExecutable:boolean;override;
      procedure InitSysInitUnitName; override;
    end;


implementation

  uses
    SysUtils,
    cutils,cfileutl,cclasses,
    verbose,systems,globtype,globals,
    fmodule,
    aasmbase,
    ogelf,owar,
    i_ps1
    ;


Constructor TLinkerPS1.Create;
begin
  Inherited Create;
end;


procedure TLinkerPS1.SetDefaultInfo;
begin
  with Info do
   begin
     ExeCmd[1]:= 'ld $OPT $RES';
   end;
end;


Function TLinkerPS1.WriteResponseFile(isdll: boolean): Boolean;
begin
  result:= True;
end;


function TLinkerPS1.MakeExecutable: boolean;
begin
  result:= true;
end;

procedure TLinkerPS1.InitSysInitUnitName;
begin
  sysinitunit:='si_prc';
end;



initialization
  RegisterLinker(ld_ps1, TLinkerPS1);
  RegisterTarget(system_mipsel_ps1_info);
end.

