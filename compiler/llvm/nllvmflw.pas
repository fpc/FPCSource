{
    Copyright (c) 2016 by Jonas Maebe

    Generate assembler for nodes that influence the flow for llvm

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
unit nllvmflw;

{$i fpcdefs.inc}

interface

    uses
      aasmbase,aasmdata,
      nflw, ncgflw, ncgnstfl;

    type
      tllvmlabelnode = class(tcglabelnode)
        function getasmlabel: tasmlabel; override;
      end;

      tllvmexceptionstatehandler = class(tcgexceptionstatehandler)
        class procedure new_exception(list: TAsmList; const t: texceptiontemps; out exceptstate: texceptionstate); override;
        class procedure emit_except_label(list: TAsmList; var exceptionstate: texceptionstate); override;
      end;


implementation

{*****************************************************************************
                             SecondLabel
*****************************************************************************}

    uses
      systems,
      symconst,symdef,llvmdef,
      cgbase,cgutils,hlcgobj,
      llvmbase,aasmllvm,
      procinfo,llvmpi;

    function tllvmlabelnode.getasmlabel: tasmlabel;
      begin
        { don't allocate global labels even if the label is accessed from
          another routine: we always have to refer to such labels using the
          blockaddress() construct, which works with local labels too.
          Additionally, LLVM does not support defining global labels in the
          middle of a routine -> jumping to such a label from assembler code
          from another function will not work anyway (have to handle that by
          passing a blockaddress as argument to an assembler block, although
          "some targets may provide defined semantics when using the value as
          the operand to an inline assembly") }
        if not(assigned(asmlabel)) then
          current_asmdata.getjumplabel(asmlabel);
        result:=asmlabel
      end;


{*****************************************************************************
                     tllvmexceptionstatehandler
*****************************************************************************}

    class procedure tllvmexceptionstatehandler.new_exception(list: TAsmList; const t: texceptiontemps; out exceptstate: texceptionstate);
      var
        landingpadlabel: TAsmLabel;
      begin
        inherited;
        { all calls inside the exception block have to be invokes instead,
          which refer to the exception label. We can't use the same label as the
          one used by the setjmp/longjmp, because only invoke operations are
          allowed to refer to a landingpad label -> create an extra label and
          emit:
            landingpadlabel:
              %reg = landingpad ..
            exceptstate.exceptionlabel:
              <exception handling code>
        }
        current_asmdata.getjumplabel(landingpadlabel);
        { for consistency checking when popping }
        tllvmprocinfo(current_procinfo).pushexceptlabel(exceptstate.exceptionlabel);
        tllvmprocinfo(current_procinfo).pushexceptlabel(landingpadlabel);
      end;


    class procedure tllvmexceptionstatehandler.emit_except_label(list: TAsmList; var exceptionstate: texceptionstate);
      var
        reg: tregister;
        clause: taillvm;
        exc: treference;
        landingpaddef: trecorddef;
      begin
        { prevent fallthrough into the landingpad, not allowed }
        hlcg.a_jmp_always(list,exceptionstate.exceptionlabel);
        hlcg.a_label(list,tllvmprocinfo(current_procinfo).CurrExceptLabel);
        { indicate that we will catch everything to LLVM's control flow
          analysis; our personality function will (for now) indicate that it
          doesn't actually want to handle any exceptions, so the stack unwinders
          will ignore us anyway (our own exceptions are still handled via
          setjmp/longjmp) }
        clause:=taillvm.exceptclause(
          la_catch,voidpointertype,nil,nil);
        { dummy register (for now): we use the same code as on other platforms
          to determine the exception type, our "personality function" won't
          return anything useful }
        reg:=hlcg.getintregister(list,u32inttype);
        { use packrecords 1 because we don't want padding (LLVM 4.0+ requires
          exactly two fields in this struct) }
        landingpaddef:=llvmgettemprecorddef([voidpointertype,u32inttype],
          1,
          targetinfos[target_info.system]^.alignment.recordalignmin,
          targetinfos[target_info.system]^.alignment.maxCrecordalign);
        list.concat(taillvm.landingpad(reg,landingpaddef,clause));
        { remove current exception label from the stack }
        tllvmprocinfo(current_procinfo).popexceptlabel(tllvmprocinfo(current_procinfo).CurrExceptLabel);
        { consistency check }
        tllvmprocinfo(current_procinfo).popexceptlabel(exceptionstate.exceptionlabel);
        inherited;

      end;



begin
  clabelnode:=tllvmlabelnode;
  cexceptionstatehandler:=tllvmexceptionstatehandler;
end.

