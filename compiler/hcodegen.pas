{
    $Id$
    Copyright (c) 1996-98 by Florian Klaempfl

    This unit exports some help routines for the code generation

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
unit hcodegen;

  interface

    uses
      aasm,tree,symtable
{$ifdef i386}
      ,i386
{$endif}
{$ifdef m68k}
      ,m68k
{$endif}
      ;

    const
       pi_uses_asm  = $1;       { set, if the procedure uses asm }
       pi_is_global = $2;       { set, if the procedure is exported by an unit }
       pi_do_call   = $4;       { set, if the procedure does a call }
       pi_operator  = $8;       { set, if the procedure is an operator   }
       pi_C_import  = $10;      { set, if the procedure is an external C function }

    type
       pprocinfo = ^tprocinfo;
       tprocinfo = record
          { pointer to parent in nested procedures }
          parent : pprocinfo;
          { current class, if we are in a method }
          _class : pobjectdef;
          { return type }
          retdef : pdef;
          { the definition of the proc itself }
          def : pdef;
          { frame pointer offset }
          framepointer_offset : longint;
          { self pointer offset }
          ESI_offset : longint;
          { result value offset }
          retoffset : longint;

          { firsttemp position }
          firsttemp : longint;

          funcret_is_valid : boolean;

          { parameter offset }
          call_offset : longint;

          { some collected informations about the procedure }
          { see pi_xxxx above                               }
          flags : longint;

          { register used as frame pointer }
          framepointer : tregister;

          { true, if the procedure is exported by an unit }
          globalsymbol : boolean;

          { true, if the procedure should be exported (only OS/2) }
          exported : boolean;

          { code for the current procedure }
          aktproccode,aktentrycode,
          aktexitcode,aktlocaldata : paasmoutput;
          { local data is used for smartlink }
       end;

    var
       { info about the current sub routine }
       procinfo : tprocinfo;

       { labels for BREAK and CONTINUE }
       aktbreaklabel,aktcontinuelabel : plabel;

       { label when the result is true or false }
       truelabel,falselabel : plabel;

       { label to leave the sub routine }
       aktexitlabel : plabel;

       { also an exit label, only used we need to clear only the stack }
       aktexit2label : plabel;

       { only used in constructor for fail or if getmem fails }
       quickexitlabel : plabel;

       { Boolean, wenn eine loadn kein Assembler erzeugt hat }
       simple_loadn : boolean;

       { tries to hold the amount of times which the current tree is processed  }
       t_times : longint;

       { true, if an error while code generation occurs }
       codegenerror : boolean;

    { initialize respectively terminates the code generator }
    { for a new module or procedure                         }
    procedure codegen_doneprocedure;
    procedure codegen_donemodule;
    procedure codegen_newmodule;
    procedure codegen_newprocedure;



    { counts the labels }
    function case_count_labels(root : pcaserecord) : longint;
    { searches the highest label }
    function case_get_max(root : pcaserecord) : longint;
    { searches the lowest label }
    function case_get_min(root : pcaserecord) : longint;


    { concates/inserts the ASCII string to the data segment }
    procedure generate_ascii(const hs : string);
    procedure generate_ascii_insert(const hs : string);
    { concates/inserts the ASCII string from pchar to the data  segment }
    { WARNING : if hs has no #0 and strlen(hs)=length           }
    { the terminal zero is not written                          }
    procedure generate_pascii(hs : pchar;length : longint);
    procedure generate_pascii_insert(hs : pchar;length : longint);


    { convert/concats a label for constants in the consts section }
    function constlabel2str(l : plabel;ctype:tconsttype):string;
    function constlabelnb2str(pnb : longint;ctype:tconsttype):string;
    procedure concat_constlabel(p:plabel;ctype:tconsttype);


implementation

     uses
        systems,cobjects,globals,files,strings;

{*****************************************************************************
         initialize/terminate the codegen for procedure and modules
*****************************************************************************}

    procedure codegen_newprocedure;
      begin
         aktbreaklabel:=nil;
         aktcontinuelabel:=nil;
         { aktexitlabel:=0; is store in oldaktexitlabel
           so it must not be reset to zero before this storage !}
         { the type of this lists isn't important }
         { because the code of this lists is      }
         { copied to the code segment             }
         procinfo.aktentrycode:=new(paasmoutput,init);
         procinfo.aktexitcode:=new(paasmoutput,init);
         procinfo.aktproccode:=new(paasmoutput,init);
         procinfo.aktlocaldata:=new(paasmoutput,init);
      end;



    procedure codegen_doneprocedure;
      begin
         dispose(procinfo.aktentrycode,done);
         dispose(procinfo.aktexitcode,done);
         dispose(procinfo.aktproccode,done);
         dispose(procinfo.aktlocaldata,done);
      end;



    procedure codegen_newmodule;
      begin
         exprasmlist:=new(paasmoutput,init);
         datasegment:=new(paasmoutput,init);
         codesegment:=new(paasmoutput,init);
         bsssegment:=new(paasmoutput,init);
         debuglist:=new(paasmoutput,init);
         externals:=new(paasmoutput,init);
         internals:=new(paasmoutput,init);
         consts:=new(paasmoutput,init);
         rttilist:=new(paasmoutput,init);
         importssection:=nil;
         exportssection:=nil;
         resourcesection:=nil;
      end;



    procedure codegen_donemodule;
      begin
         dispose(exprasmlist,done);
         dispose(codesegment,done);
         dispose(bsssegment,done);
         dispose(datasegment,done);
         dispose(debuglist,done);
         dispose(externals,done);
         dispose(consts,done);
         dispose(rttilist,done);
         if assigned(importssection) then
          dispose(importssection,done);
         if assigned(exportssection) then
          dispose(exportssection,done);
         if assigned(resourcesection) then
          dispose(resourcesection,done);
      end;

        
{*****************************************************************************
                              Case Helpers
*****************************************************************************}

    function case_count_labels(root : pcaserecord) : longint;
      var
         _l : longint;

      procedure count(p : pcaserecord);
        begin
           inc(_l);
           if assigned(p^.less) then
             count(p^.less);
           if assigned(p^.greater) then
             count(p^.greater);
        end;

      begin
         _l:=0;
         count(root);
         case_count_labels:=_l;
      end;


    function case_get_max(root : pcaserecord) : longint;
      var
         hp : pcaserecord;
      begin
         hp:=root;
         while assigned(hp^.greater) do
           hp:=hp^.greater;
         case_get_max:=hp^._high;
      end;


    function case_get_min(root : pcaserecord) : longint;
      var
         hp : pcaserecord;
      begin
         hp:=root;
         while assigned(hp^.less) do
           hp:=hp^.less;
         case_get_min:=hp^._low;
      end;


{*****************************************************************************
                              String Helpers
*****************************************************************************}

    procedure generate_ascii(const hs : string);
      begin
         datasegment^.concat(new(pai_string,init(hs)))
      end;


    procedure generate_ascii_insert(const hs : string);
      begin
         datasegment^.insert(new(pai_string,init(hs)));
      end;


    function strnew(p : pchar;length : longint) : pchar;
      var
         pc : pchar;
      begin
         getmem(pc,length);
         move(p^,pc^,length);
         strnew:=pc;
      end;



    { concates the ASCII string from pchar to the const segment }
    procedure generate_pascii(hs : pchar;length : longint);
      var
         real_end,current_begin,current_end : pchar;
         c :char;
      begin
         if assigned(hs) then
           begin
              current_begin:=hs;
              real_end:=strend(hs);
              c:=hs[0];
              while length>32 do
                begin
                   { restore the char displaced }
                   current_begin[0]:=c;
                   current_end:=current_begin+32;
                   { store the char for next loop }
                   c:=current_end[0];
                   current_end[0]:=#0;
                   datasegment^.concat(new(pai_string,init_length_pchar(strnew(current_begin,32),32)));
                   length:=length-32;
                end;
              datasegment^.concat(new(pai_string,init_length_pchar(strnew(current_begin,length),length)));
           end;
      end;


    { inserts the ASCII string from pchar to the const segment }
    procedure generate_pascii_insert(hs : pchar;length : longint);
      var
         real_end,current_begin,current_end : pchar;
         c :char;
      begin
         if assigned(hs) then
           begin
              current_begin:=hs;
              real_end:=strend(hs);
              c:=hs[0];
              length:=longint(real_end)-longint(hs);
              while length>32 do
                begin
                   { restore the char displaced }
                   current_begin[0]:=c;
                   current_end:=current_begin+32;
                   { store the char for next loop }
                   c:=current_end[0];
                   current_end[0]:=#0;
                   datasegment^.insert(new(pai_string,init_length_pchar(strnew(current_begin,32),32)));
                   length:=length-32;
                end;
              datasegment^.insert(new(pai_string,init_length_pchar(strnew(current_begin,length),length)));
           end;
      end;


{*****************************************************************************
                              Const Helpers
*****************************************************************************}

    const
      consttypestr : array[tconsttype] of string[6]=
        ('ord','string','real','bool','int','char','set');

      { Peter this gives problems for my inlines !! }
      { we must use the number directly !!! (PM) }
    function constlabel2str(l : plabel;ctype:tconsttype):string;
      begin
        if smartlink or (current_module^.output_format in [of_nasm,of_obj]) then
         constlabel2str:='_$'+current_module^.unitname^+'$'+consttypestr[ctype]+'_const_'+tostr(l^.nb)
        else
         constlabel2str:=lab2str(l);
      end;

    function constlabelnb2str(pnb : longint;ctype:tconsttype):string;
      begin
        if smartlink or (current_module^.output_format in [of_nasm,of_obj]) then
         constlabelnb2str:='_$'+current_module^.unitname^+'$'+consttypestr[ctype]+'_const_'+tostr(pnb)
        else
         constlabelnb2str:=target_asm.labelprefix+tostr(pnb);
      end;


    procedure concat_constlabel(p:plabel;ctype:tconsttype);
      var
        s : string;
      begin
        if smartlink or (current_module^.output_format in [of_nasm,of_obj]) then
         begin
           s:='_$'+current_module^.unitname^+'$'+consttypestr[ctype]+'_const_'+tostr(p^.nb);
           if smartlink then
            begin
              consts^.concat(new(pai_cut,init));
              consts^.concat(new(pai_symbol,init_global(s)))
            end
           else
            consts^.concat(new(pai_symbol,init_global(s)));
         end
        else
         consts^.concat(new(pai_label,init(p)));
      end;

end.

{
  $Log$
  Revision 1.5  1998-05-20 09:42:34  pierre
    + UseTokenInfo now default
    * unit in interface uses and implementation uses gives error now
    * only one error for unknown symbol (uses lastsymknown boolean)
      the problem came from the label code !
    + first inlined procedures and function work
      (warning there might be allowed cases were the result is still wrong !!)
    * UseBrower updated gives a global list of all position of all used symbols
      with switch -gb

  Revision 1.4  1998/05/07 00:17:01  peter
    * smartlinking for sets
    + consts labels are now concated/generated in hcodegen
    * moved some cpu code to cga and some none cpu depended code from cga
      to tree and hcodegen and cleanup of hcodegen
    * assembling .. output reduced for smartlinking ;)

  Revision 1.3  1998/05/06 08:38:40  pierre
    * better position info with UseTokenInfo
      UseTokenInfo greatly simplified
    + added check for changed tree after first time firstpass
      (if we could remove all the cases were it happen
      we could skip all firstpass if firstpasscount > 1)
      Only with ExtDebug

  Revision 1.2  1998/04/29 10:33:53  pierre
    + added some code for ansistring (not complete nor working yet)
    * corrected operator overloading
    * corrected nasm output
    + started inline procedures
    + added starstarn : use ** for exponentiation (^ gave problems)
    + started UseTokenInfo cond to get accurate positions
}
