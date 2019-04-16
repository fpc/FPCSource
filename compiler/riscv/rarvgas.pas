{
    Copyright (c) 2019 by Jeppe Johansen

    Does the parsing for the RISC-V GNU AS styled inline assembler.

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
unit rarvgas;

{$I fpcdefs.inc}

  interface

    uses
      raatt,
      cpubase;

    type

      trvattreader = class(tattreader)
        function is_targetdirective(const s: string): boolean; override;
        procedure HandleTargetDirective; override;
      end;

  implementation

    uses
      { helpers }
      cutils,
      { global }
      globtype,globals,verbose,
      systems,
      { aasm }
      aasmbase,aasmtai,aasmdata,aasmcpu,
      { symtable }
      symconst,symsym,symdef,
      { parser }
      procinfo,
      rabase,rautils,
      cgbase,cgobj,cgrv
      ;

    function trvattreader.is_targetdirective(const s: string): boolean;
      begin
        case s of
          '.option':
            result:=true
          else
            Result:=inherited is_targetdirective(s);
        end;
      end;

    procedure trvattreader.HandleTargetDirective;
      var
        id: string;
      begin
        case actasmpattern of
          '.option':
            begin
              consume(AS_TARGET_DIRECTIVE);
              id:=actasmpattern;
              Consume(AS_ID);
              curList.concat(tai_directive.create(asd_option, lower(id)));
            end
          else
            inherited HandleTargetDirective;
        end;
      end;

end.

