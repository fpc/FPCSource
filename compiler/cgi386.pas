{
    $Id$
    Copyright (c) 1993-98 by Florian Klaempfl

    This unit generates i386 (or better) assembler from the parse tree

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
{$ifdef TP}
  {$E+,F+,N+,D+,L+,Y+}
{$endif}
unit cgi386;
interface

uses
  tree;

{ produces assembler for the expression in variable p }
{ and produces an assembler node at the end           }
procedure generatecode(var p : ptree);

{ produces the actual code }
function do_secondpass(var p : ptree) : boolean;
procedure secondpass(var p : ptree);


{$ifdef test_dest_loc}

const
  { used to avoid temporary assignments }
  dest_loc_known : boolean = false;
  in_dest_loc    : boolean = false;
  dest_loc_tree  : ptree = nil;

var
  dest_loc : tlocation;

procedure mov_reg_to_dest(p : ptree; s : topsize; reg : tregister);

{$endif test_dest_loc}


implementation

   uses
     verbose,cobjects,systems,globals,files,
     symtable,types,aasm,scanner,
     pass_1,hcodegen,temp_gen
{$ifdef GDB}
     ,gdb
{$endif}
{$ifdef i386}
     ,i386,tgeni386,cgai386
     ,cg386con,cg386mat,cg386cnv,cg386set,cg386add
     ,cg386mem,cg386cal,cg386ld,cg386flw
{$endif}
     ;

{$ifdef test_dest_loc}
       procedure mov_reg_to_dest(p : ptree; s : topsize; reg : tregister);

         begin
            if (dest_loc.loc=LOC_CREGISTER) or (dest_loc.loc=LOC_REGISTER) then
              begin
                emit_reg_reg(A_MOV,s,reg,dest_loc.register);
                p^.location:=dest_loc;
                in_dest_loc:=true;
              end
            else
            if (dest_loc.loc=LOC_REFERENCE) or (dest_loc.loc=LOC_MEM) then
              begin
                exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,s,reg,newreference(dest_loc.reference))));
                p^.location:=dest_loc;
                in_dest_loc:=true;
              end
            else
              internalerror(20080);
         end;

{$endif test_dest_loc}


    procedure message(const t : tmsgconst);

      var
         olderrorcount : longint;

      begin
         if not(codegenerror) then
           begin
              olderrorcount:=status.errorcount;
              verbose.Message(t);
              codegenerror:=olderrorcount<>status.errorcount;
           end;
      end;

    procedure message1(const t : tmsgconst;const s : string);

      var
         olderrorcount : longint;

      begin
         if not(codegenerror) then
           begin
              olderrorcount:=status.errorcount;
              verbose.Message1(t,s);
              codegenerror:=olderrorcount<>status.errorcount;
           end;
      end;

    procedure message2(const t : tmsgconst;const s1,s2 : string);

      var
         olderrorcount : longint;

      begin
         if not(codegenerror) then
           begin
              olderrorcount:=status.errorcount;
              verbose.Message2(t,s1,s2);
              codegenerror:=olderrorcount<>status.errorcount;
           end;
      end;

    procedure message3(const t : tmsgconst;const s1,s2,s3 : string);

      var
         olderrorcount : longint;

      begin
         if not(codegenerror) then
           begin
              olderrorcount:=status.errorcount;
              verbose.Message3(t,s1,s2,s3);
              codegenerror:=olderrorcount<>status.errorcount;
           end;
      end;

{*****************************************************************************
                              SecondPass
*****************************************************************************}

    type
       secondpassproc = procedure(var p : ptree);

    procedure secondnothing(var p : ptree);
      begin
      end;


    procedure seconderror(var p : ptree);

      begin
         p^.error:=true;
         codegenerror:=true;
      end;


    procedure secondstatement(var p : ptree);
      var
         hp : ptree;
      begin
         hp:=p;
         while assigned(hp) do
          begin
            if assigned(hp^.right) then
             begin
               cleartempgen;
               secondpass(hp^.right);
             end;
            hp:=hp^.left;
          end;
      end;


    procedure secondblockn(var p : ptree);
      begin
      { do second pass on left node }
        if assigned(p^.left) then
         secondpass(p^.left);
      end;


    procedure secondasm(var p : ptree);
      begin
         exprasmlist^.concatlist(p^.p_asm);
         if not p^.object_preserved then
           maybe_loadesi;
       end;


     procedure secondpass(var p : ptree);
       const
         procedures : array[ttreetyp] of secondpassproc =
            (secondadd,secondadd,secondadd,secondmoddiv,secondadd,
             secondmoddiv,secondassignment,secondload,secondnothing,
             secondadd,secondadd,secondadd,secondadd,
             secondadd,secondadd,secondin,secondadd,
             secondadd,secondshlshr,secondshlshr,secondadd,
             secondadd,secondsubscriptn,secondderef,secondaddr,
             seconddoubleaddr,
             secondordconst,secondtypeconv,secondcalln,secondnothing,
             secondrealconst,secondfixconst,secondumminus,
             secondasm,secondvecn,
             secondstringconst,secondfuncret,secondselfn,
             secondnot,secondinline,secondniln,seconderror,
             secondnothing,secondhnewn,secondhdisposen,secondnewn,
             secondsimplenewdispose,secondnothing,secondsetcons,secondblockn,
             secondstatement,secondnothing,secondifn,secondbreakn,
             secondcontinuen,second_while_repeatn,second_while_repeatn,secondfor,
             secondexitn,secondwith,secondcase,secondlabel,
             secondgoto,secondsimplenewdispose,secondtryexcept,secondraise,
             secondnothing,secondtryfinally,secondis,secondas,seconderror,
             secondfail,secondadd,secondprocinline,
             secondnothing,secondloadvmt);
      var
         oldcodegenerror : boolean;
         oldswitches : Tcswitches;
         oldpos : tfileposinfo;

      begin
         oldcodegenerror:=codegenerror;
         oldswitches:=aktswitches;
{$ifdef NEWINPUT}
         oldpos:=aktfilepos;
         aktfilepos:=p^.fileinfo;
{$else}
         get_cur_file_pos(oldpos);
         set_cur_file_pos(p^.fileinfo);
{$endif NEWINPUT}

         codegenerror:=false;
         aktswitches:=p^.pragmas;
         if not(p^.error) then
           begin
              procedures[p^.treetype](p);
              p^.error:=codegenerror;
              codegenerror:=codegenerror or oldcodegenerror;
           end
         else
           codegenerror:=true;
         aktswitches:=oldswitches;
{$ifdef NEWINPUT}
         aktfilepos:=oldpos;
{$else}
         set_cur_file_pos(oldpos);
{$endif NEWINPUT}
      end;


    function do_secondpass(var p : ptree) : boolean;
      begin
         codegenerror:=false;
         if not(p^.error) then
           secondpass(p);
         do_secondpass:=codegenerror;
      end;



    var
       regvars : array[1..maxvarregs] of pvarsym;
       regvars_para : array[1..maxvarregs] of boolean;
       regvars_refs : array[1..maxvarregs] of longint;
       parasym : boolean;

    procedure searchregvars(p : psym);

      var
         i,j,k : longint;

      begin
         if (p^.typ=varsym) and ((pvarsym(p)^.var_options and vo_regable)<>0) then
           begin
              { walk through all momentary register variables }
              for i:=1 to maxvarregs do
                begin
                   { free register ? }
                   if regvars[i]=nil then
                     begin
                        regvars[i]:=pvarsym(p);
                        regvars_para[i]:=parasym;
                        break;
                     end;
                   { else throw out a variable ? }
                       j:=pvarsym(p)^.refs;
                   { parameter get a less value }
                   if parasym then
                     begin
                        if cs_littlesize in aktswitches  then
                          dec(j,1)
                        else
                          dec(j,100);
                     end;
                   if (j>regvars_refs[i]) and (j>0) then
                     begin
                        for k:=maxvarregs-1 downto i do
                          begin
                             regvars[k+1]:=regvars[k];
                             regvars_para[k+1]:=regvars_para[k];
                          end;
                        { calc the new refs
                        pvarsym(p)^.refs:=j; }
                        regvars[i]:=pvarsym(p);
                        regvars_para[i]:=parasym;
                        regvars_refs[i]:=j;
                        break;
                     end;
                end;
           end;
      end;

    procedure generatecode(var p : ptree);

      var
           { *pass modifies with every node aktlinenr and current_module^.current_inputfile, }
         { to constantly contain the right line numbers             }
           oldis : pinputfile;
         oldnr,i : longint;
         regsize : topsize;
         regi : tregister;
          hr : preference;

       label
         nextreg;

      begin
         cleartempgen;
{$ifndef NEWINPUT}
         oldis:=current_module^.current_inputfile;
         oldnr:=current_module^.current_inputfile^.line_no;
{$endif}
         { when size optimization only count occurrence }
         if cs_littlesize in aktswitches then
           t_times:=1
         else
           { reference for repetition is 100 }
           t_times:=100;
         { clear register count }
{$ifdef SUPPORT_MMX}
         for regi:=R_EAX to R_MM6 do
           begin
              reg_pushes[regi]:=0;
              is_reg_var[regi]:=false;
           end;
{$else SUPPORT_MMX}
         for regi:=R_EAX to R_EDI do
           begin
              reg_pushes[regi]:=0;
              is_reg_var[regi]:=false;
           end;
{$endif SUPPORT_MMX}
         use_esp_stackframe:=false;

         if not(do_firstpass(p)) then
           begin
              { max. optimizations     }
              { only if no asm is used }
              if (cs_maxoptimieren in aktswitches) and
                ((procinfo.flags and pi_uses_asm)=0) then
                begin
                   { can we omit the stack frame ? }
                   { conditions:
                     1. procedure (not main block)
                     2. no constructor or destructor
                     3. no call to other procedures
                     4. no interrupt handler
                   }
                   if assigned(aktprocsym) then
                     begin
                       if (aktprocsym^.definition^.options and
                        poconstructor+podestructor{+poinline}+pointerrupt=0) and
                        ((procinfo.flags and pi_do_call)=0) and (lexlevel>1) then
                       begin
                         { use ESP as frame pointer }
                         procinfo.framepointer:=R_ESP;
                         use_esp_stackframe:=true;

                         { calc parameter distance new }
                         dec(procinfo.framepointer_offset,4);
                         dec(procinfo.ESI_offset,4);

                         { is this correct ???}
                         { retoffset can be negativ for results in eax !! }
                         { the value should be decreased only if positive }
                         if procinfo.retoffset>=0 then
                           dec(procinfo.retoffset,4);

                         dec(procinfo.call_offset,4);
                         aktprocsym^.definition^.parast^.call_offset:=procinfo.call_offset;
                       end;
                     end;
                   if (p^.registers32<4) then
                       begin
                        for i:=1 to maxvarregs do
                          regvars[i]:=nil;
                        parasym:=false;
                      {$ifdef tp}
                        symtablestack^.foreach(searchregvars);
                      {$else}
                        symtablestack^.foreach(@searchregvars);
                      {$endif}
                        { copy parameter into a register ? }
                        parasym:=true;
                      {$ifdef tp}
                        symtablestack^.next^.foreach(searchregvars);
                      {$else}
                        symtablestack^.next^.foreach(@searchregvars);
                      {$endif}
                        { hold needed registers free }
                        for i:=maxvarregs downto maxvarregs-p^.registers32+1 do
                          regvars[i]:=nil;
                        { now assign register }
                        for i:=1 to maxvarregs-p^.registers32 do
                          begin
                             if assigned(regvars[i]) then
                               begin
                                  { it is nonsens, to copy the variable to }
                                  { a register because we need then much   }
                                  { pushes ?                               }
                                  if reg_pushes[varregs[i]]>=regvars[i]^.refs then
                                    begin
                                       regvars[i]:=nil;
                                       goto nextreg;
                                    end;

                                  { register is no longer available for }
                                  { expressions                         }
                                  { search the register which is the most }
                                  { unused                                }
                                  usableregs:=usableregs-[varregs[i]];
                                  is_reg_var[varregs[i]]:=true;
                                  dec(c_usableregs);

                                  { possibly no 32 bit register are needed }
                                  if  (regvars[i]^.definition^.deftype=orddef) and
                                      (porddef(regvars[i]^.definition)^.typ in [bool8bit,uchar,u8bit,s8bit]) then
                                    begin
                                       regvars[i]^.reg:=reg32toreg8(varregs[i]);
                                       regsize:=S_B;
                                    end
                                  else if  (regvars[i]^.definition^.deftype=orddef) and
                                           (porddef(regvars[i]^.definition)^.typ in [bool16bit,u16bit,s16bit]) then
                                    begin
                                       regvars[i]^.reg:=reg32toreg16(varregs[i]);
                                       regsize:=S_W;
                                    end
                                  else
                                    begin
                                       regvars[i]^.reg:=varregs[i];
                                       regsize:=S_L;
                                    end;
                                  { parameter must be load }
                                  if regvars_para[i] then
                                    begin
                                       { procinfo is there actual,      }
                                       { because we can't never be in a }
                                       { nested procedure               }
                                       { when loading parameter to reg  }
                                       new(hr);
                                       reset_reference(hr^);
                                       hr^.offset:=pvarsym(regvars[i])^.address+procinfo.call_offset;
                                       hr^.base:=procinfo.framepointer;
                                       procinfo.aktentrycode^.concat(new(pai386,op_ref_reg(A_MOV,regsize,
                                         hr,regvars[i]^.reg)));
                                       unused:=unused - [regvars[i]^.reg];
                                    end;
                                  { procedure uses this register }
                                  usedinproc:=usedinproc or ($80 shr byte(varregs[i]));
                               end;
                             nextreg:
                               { dummy }
                               regsize:=S_W;
                          end;
                        if (verbosity and v_debug)=v_debug then
                          begin
                             for i:=1 to maxvarregs do
                               begin
                                  if assigned(regvars[i]) then
                                   Message3(cg_d_register_weight,reg2str(regvars[i]^.reg),
                                           tostr(regvars[i]^.refs),regvars[i]^.name);
                               end;
                          end;
                     end;
                end;
              if assigned(aktprocsym) and
                 ((aktprocsym^.definition^.options and poinline)<>0) then
                make_const_global:=true;
              do_secondpass(p);

{$ifdef StoreFPULevel}
              procinfo.def^.fpu_used:=p^.registersfpu;
{$endif StoreFPULevel}
              { all registers can be used again }
              usableregs:=[R_EAX,R_EBX,R_ECX,R_EDX];
{$ifdef SUPPORT_MMX}
              usableregs:=usableregs+[R_MM0..R_MM6];
{$endif SUPPORT_MMX}
              c_usableregs:=4;
           end;
         procinfo.aktproccode^.concatlist(exprasmlist);
         make_const_global:=false;
{$ifndef NEWINPUT}
         current_module^.current_inputfile:=oldis;
         current_module^.current_inputfile^.line_no:=oldnr;
{$endif}
      end;

end.
{
  $Log$
  Revision 1.40  1998-07-07 11:19:52  peter
    + NEWINPUT for a better inputfile and scanner object

  Revision 1.39  1998/06/12 10:32:23  pierre
    * column problem hopefully solved
    + C vars declaration changed

  Revision 1.38  1998/06/09 16:01:37  pierre
    + added procedure directive parsing for procvars
      (accepted are popstack cdecl and pascal)
    + added C vars with the following syntax
      var C calias 'true_c_name';(can be followed by external)
      reason is that you must add the Cprefix

      which is target dependent

  Revision 1.37  1998/06/08 13:13:41  pierre
    + temporary variables now in temp_gen.pas unit
      because it is processor independent
    * mppc68k.bat modified to undefine i386 and support_mmx
      (which are defaults for i386)

  Revision 1.36  1998/06/05 17:49:54  peter
    * cleanup of cgai386

  Revision 1.35  1998/06/05 16:13:32  pierre
    * fix for real and string consts inside inlined procs

  Revision 1.34  1998/06/05 14:37:27  pierre
    * fixes for inline for operators
    * inline procedure more correctly restricted

  Revision 1.33  1998/06/04 23:51:37  peter
    * m68k compiles
    + .def file creation moved to gendef.pas so it could also be used
      for win32

  Revision 1.32  1998/06/04 09:55:35  pierre
    * demangled name of procsym reworked to become independant of the mangling scheme

  Come test_funcret improvements (not yet working)S: ----------------------------------------------------------------------

  Revision 1.31  1998/06/03 22:48:52  peter
    + wordbool,longbool
    * rename bis,von -> high,low
    * moved some systemunit loading/creating to psystem.pas

  Revision 1.30  1998/06/02 17:03:00  pierre
    *  with node corrected for objects
    * small bugs for SUPPORT_MMX fixed

  Revision 1.29  1998/06/01 16:50:18  peter
    + boolean -> ord conversion
    * fixed ord -> boolean conversion

  Revision 1.28  1998/05/28 17:26:47  peter
    * fixed -R switch, it didn't work after my previous akt/init patch
    * fixed bugs 110,130,136

  Revision 1.27  1998/05/25 17:11:38  pierre
    * firstpasscount bug fixed
      now all is already set correctly the first time
      under EXTDEBUG try -gp to skip all other firstpasses
      it works !!
    * small bug fixes
      - for smallsets with -dTESTSMALLSET
      - some warnings removed (by correcting code !)

  Revision 1.26  1998/05/23 01:21:03  peter
    + aktasmmode, aktoptprocessor, aktoutputformat
    + smartlink per module $SMARTLINK-/+ (like MMX) and moved to aktswitches
    + $LIBNAME to set the library name where the unit will be put in
    * splitted cgi386 a bit (codeseg to large for bp7)
    * nasm, tasm works again. nasm moved to ag386nsm.pas

  Revision 1.25  1998/05/21 19:33:31  peter
    + better procedure directive handling and only one table

  Revision 1.24  1998/05/20 09:42:33  pierre
    + UseTokenInfo now default
    * unit in interface uses and implementation uses gives error now
    * only one error for unknown symbol (uses lastsymknown boolean)
      the problem came from the label code !
    + first inlined procedures and function work
      (warning there might be allowed cases were the result is still wrong !!)
    * UseBrower updated gives a global list of all position of all used symbols
      with switch -gb

  Revision 1.23  1998/05/12 10:46:58  peter
    * moved printstatus to verb_def
    + V_Normal which is between V_Error and V_Warning and doesn't have a
      prefix like error: warning: and is included in V_Default
    * fixed some messages
    * first time parameter scan is only for -v and -T
    - removed old style messages

  Revision 1.22  1998/05/07 00:17:00  peter
    * smartlinking for sets
    + consts labels are now concated/generated in hcodegen
    * moved some cpu code to cga and some none cpu depended code from cga
      to tree and hcodegen and cleanup of hcodegen
    * assembling .. output reduced for smartlinking ;)

  Revision 1.21  1998/05/06 08:38:36  pierre
    * better position info with UseTokenInfo
      UseTokenInfo greatly simplified
    + added check for changed tree after first time firstpass
      (if we could remove all the cases were it happen
      we could skip all firstpass if firstpasscount > 1)
      Only with ExtDebug

  Revision 1.20  1998/05/01 16:38:44  florian
    * handling of private and protected fixed
    + change_keywords_to_tp implemented to remove
      keywords which aren't supported by tp
    * break and continue are now symbols of the system unit
    + widestring, longstring and ansistring type released

  Revision 1.19  1998/04/30 15:59:39  pierre
    * GDB works again better :
      correct type info in one pass
    + UseTokenInfo for better source position
    * fixed one remaining bug in scanner for line counts
    * several little fixes

  Revision 1.18  1998/04/29 10:33:48  pierre
    + added some code for ansistring (not complete nor working yet)
    * corrected operator overloading
    * corrected nasm output
    + started inline procedures
    + added starstarn : use ** for exponentiation (^ gave problems)
    + started UseTokenInfo cond to get accurate positions

  Revision 1.17  1998/04/27 23:10:27  peter
    + new scanner
    * $makelib -> if smartlink
    * small filename fixes pmodule.setfilename
    * moved import from files.pas -> import.pas

  Revision 1.16  1998/04/23 21:52:08  florian
    * fixes of Jonas applied

  Revision 1.15  1998/04/22 21:06:49  florian
    * last fixes before the release:
      - veryyyy slow firstcall fixed

  Revision 1.14  1998/04/21 10:16:47  peter
    * patches from strasbourg
    * objects is not used anymore in the fpc compiled version

  Revision 1.13  1998/04/14 23:27:02  florian
    + exclude/include with constant second parameter added

  Revision 1.12  1998/04/13 21:15:41  florian
    * error handling of pass_1 and cgi386 fixed
    * the following bugs fixed: 0117, 0118, 0119 and 0129, 0122 was already
      fixed, verified

  Revision 1.11  1998/04/13 08:42:51  florian
    * call by reference and call by value open arrays fixed

  Revision 1.10  1998/04/12 22:39:43  florian
    * problem with read access to properties solved
    * correct handling of hidding methods via virtual (COM)
    * correct result type of constructor calls (COM), the resulttype
      depends now on the type of the class reference

  Revision 1.9  1998/04/10 21:36:55  florian
    + some stuff to support method pointers (procedure of object) added
      (declaration, parameter handling)

  Revision 1.8  1998/04/09 22:16:33  florian
    * problem with previous REGALLOC solved
    * improved property support

  Revision 1.7  1998/04/09 14:28:05  jonas
    + basic k6 and 6x86 optimizing support (-O7 and -O8)

  Revision 1.6  1998/04/08 11:34:20  peter
    * nasm works (linux only tested)

  Revision 1.5  1998/04/07 22:45:04  florian
    * bug0092, bug0115 and bug0121 fixed
    + packed object/class/array

  Revision 1.4  1998/04/07 13:19:42  pierre
    * bugfixes for reset_gdb_info
      in MEM parsing for go32v2
      better external symbol creation
      support for rhgdb.exe (lowercase file names)
}
