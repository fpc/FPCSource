{
    Copyright (c) 1998-2007 by Florian Klaempfl

    Type checking and register allocation for inline nodes

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
unit ninl;

{$i fpcdefs.inc}

interface

    uses
       node,htypechk,cpuinfo,symtype;

    {$i compinnr.inc}

    type
       tinlinenode = class(tunarynode)
          inlinenumber : byte;
          constructor create(number : byte;is_const:boolean;l : tnode);virtual;
          constructor ppuload(t:tnodetype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          function dogetcopy : tnode;override;
          function pass_1 : tnode;override;
          function pass_typecheck:tnode;override;
          function simplify: tnode;override;
          function docompare(p: tnode): boolean; override;

          { pack and unpack are changed into for-loops by the compiler }
          function first_pack_unpack: tnode; virtual;

          { All the following routines currently
            call compilerprocs, unless they are
            overriden in which case, the code
            generator handles them.
          }
          function first_pi: tnode ; virtual;
          function first_arctan_real: tnode; virtual;
          function first_abs_real: tnode; virtual;
          function first_sqr_real: tnode; virtual;
          function first_sqrt_real: tnode; virtual;
          function first_ln_real: tnode; virtual;
          function first_cos_real: tnode; virtual;
          function first_sin_real: tnode; virtual;
          function first_exp_real: tnode; virtual;
          function first_frac_real: tnode; virtual;
          function first_round_real: tnode; virtual;
          function first_trunc_real: tnode; virtual;
          function first_int_real: tnode; virtual;
          function first_abs_long: tnode; virtual;
        private
          function handle_str: tnode;
          function handle_reset_rewrite_typed: tnode;
          function handle_text_read_write(filepara,params:Ttertiarynode;var newstatement:Tnode):boolean;
          function handle_typed_read_write(filepara,params:Ttertiarynode;var newstatement:Tnode):boolean;
          function handle_read_write: tnode;
          function handle_val: tnode;
       end;
       tinlinenodeclass = class of tinlinenode;

    var
       cinlinenode : tinlinenodeclass;

   function geninlinenode(number : byte;is_const:boolean;l : tnode) : tinlinenode;

implementation

    uses
      verbose,globals,systems,constexp,
      globtype, cutils,
      symconst,symdef,symsym,symtable,paramgr,defutil,
      pass_1,
      ncal,ncon,ncnv,nadd,nld,nbas,nflw,nmem,nmat,nutils,
      cgbase,procinfo
      ;

   function geninlinenode(number : byte;is_const:boolean;l : tnode) : tinlinenode;

     begin
        geninlinenode:=cinlinenode.create(number,is_const,l);
     end;

{*****************************************************************************
                           TINLINENODE
*****************************************************************************}

    constructor tinlinenode.create(number : byte;is_const:boolean;l : tnode);

      begin
         inherited create(inlinen,l);
         if is_const then
           include(flags,nf_inlineconst);
         inlinenumber:=number;
      end;


    constructor tinlinenode.ppuload(t:tnodetype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
        inlinenumber:=ppufile.getbyte;
      end;


    procedure tinlinenode.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        ppufile.putbyte(inlinenumber);
      end;


    function tinlinenode.dogetcopy : tnode;
      var
         n : tinlinenode;
      begin
         n:=tinlinenode(inherited dogetcopy);
         n.inlinenumber:=inlinenumber;
         result:=n;
      end;


    function tinlinenode.handle_str : tnode;
      var
        lenpara,
        fracpara,
        newparas,
        tmppara,
        dest,
        source  : tcallparanode;
        procname: string;
        is_real,is_enum : boolean;
        rt : aint;

      begin
        result := cerrornode.create;

        { make sure we got at least two parameters (if we got only one, }
        { this parameter may not be encapsulated in a callparan)        }
        if not assigned(left) or
           (left.nodetype <> callparan) then
          begin
            CGMessage1(parser_e_wrong_parameter_size,'Str');
            exit;
          end;

        { get destination string }
        dest := tcallparanode(left);

        { get source para (number) }
        source := dest;
        while assigned(source.right) do
          source := tcallparanode(source.right);

        { destination parameter must be a normal (not a colon) parameter, this
          check is needed because str(v:len) also has 2 parameters }
        if (source=dest) or
           (cpf_is_colon_para in tcallparanode(dest).callparaflags) then
          begin
            CGMessage1(parser_e_wrong_parameter_size,'Str');
            exit;
          end;

        is_real:=(source.resultdef.typ = floatdef) or is_currency(source.resultdef);
        is_enum:=source.left.resultdef.typ=enumdef;

        if ((dest.left.resultdef.typ<>stringdef) and
            not(is_chararray(dest.left.resultdef))) or
           not(is_real or is_enum or
               (source.left.resultdef.typ=orddef)) then
          begin
            CGMessagePos(fileinfo,parser_e_illegal_expression);
            exit;
          end;

        { get len/frac parameters }
        lenpara := nil;
        fracpara := nil;
        if (cpf_is_colon_para in tcallparanode(dest.right).callparaflags) then
          begin
            lenpara := tcallparanode(dest.right);

            { we can let the callnode do the type checking of these parameters too, }
            { but then the error messages aren't as nice                            }
            if not is_integer(lenpara.resultdef) then
              begin
                CGMessagePos1(lenpara.fileinfo,
                  type_e_integer_expr_expected,lenpara.resultdef.typename);
                exit;
              end;
            if (cpf_is_colon_para in tcallparanode(lenpara.right).callparaflags) then
              begin
                { parameters are in reverse order! }
                fracpara := lenpara;
                lenpara := tcallparanode(lenpara.right);
                if not is_real then
                  begin
                    CGMessagePos(lenpara.fileinfo,parser_e_illegal_colon_qualifier);
                    exit
                  end;
                if not is_integer(lenpara.resultdef) then
                  begin
                    CGMessagePos1(lenpara.fileinfo,
                      type_e_integer_expr_expected,lenpara.resultdef.typename);
                    exit;
                  end;
              end;
          end;

        { generate the parameter list for the compilerproc }
        newparas := dest;

        { if we have a float parameter, insert the realtype, len and fracpara parameters }
        if is_real then
          begin
            { insert realtype parameter }
            if not is_currency(source.resultdef) then
              begin
                rt:=ord(tfloatdef(source.left.resultdef).floattype);
                newparas.right := ccallparanode.create(cordconstnode.create(
                  rt,s32inttype,true),newparas.right);
                tmppara:=tcallparanode(newparas.right);
              end
            else
              tmppara:=newparas;
            { if necessary, insert a fraction parameter }
            if not assigned(fracpara) then
              begin
                tmppara.right := ccallparanode.create(
                  cordconstnode.create(int64(-1),s32inttype,false),
                   tmppara.right);
                fracpara := tcallparanode(tmppara.right);
              end;
            { if necessary, insert a length para }
            if not assigned(lenpara) then
              fracpara.right := ccallparanode.create(
                cordconstnode.create(int64(-32767),s32inttype,false),
                   fracpara.right);
          end
        else if is_enum then
          begin
            {Insert a reference to the ord2string index.}
            newparas.right:=Ccallparanode.create(
              Caddrnode.create_internal(
                Crttinode.create(Tenumdef(source.left.resultdef),fullrtti,rdt_normal)
              ),
              newparas.right);
            {Insert a reference to the typinfo.}
            newparas.right:=Ccallparanode.create(
              Caddrnode.create_internal(
                Crttinode.create(Tenumdef(source.left.resultdef),fullrtti,rdt_ord2str)
              ),
              newparas.right);
            {Insert a type conversion from the enumeration to longint.}
            source.left:=Ctypeconvnode.create_internal(source.left,s32inttype);
            typecheckpass(source.left);

            { if necessary, insert a length para }
            if not assigned(lenpara) then
              Tcallparanode(Tcallparanode(newparas.right).right).right:=
                Ccallparanode.create(
                  cordconstnode.create(int64(-1),s32inttype,false),
                  Tcallparanode(Tcallparanode(newparas.right).right).right
                );
          end
        else
          { for a normal parameter, insert a only length parameter if one is missing }
          if not assigned(lenpara) then
            newparas.right := ccallparanode.create(cordconstnode.create(int64(-1),s32inttype,false),
              newparas.right);

        { remove the parameters from the original node so they won't get disposed, }
        { since they're reused                                                     }
        left := nil;

        { create procedure name }
        if is_chararray(dest.resultdef) then
          procname:='fpc_chararray_'
        else
          procname := 'fpc_' + tstringdef(dest.resultdef).stringtypname+'_';
        if is_real then
          if is_currency(source.resultdef) then
            procname := procname + 'currency'
          else
            procname := procname + 'float'
        else if is_enum then
          procname:=procname+'enum'
        else
          case torddef(source.resultdef).ordtype of
{$ifdef cpu64bit}
            u64bit:
              procname := procname + 'uint';
{$else}
            u32bit:
              procname := procname + 'uint';
            u64bit:
              procname := procname + 'qword';
            scurrency,
            s64bit:
              procname := procname + 'int64';
{$endif}
            else
              procname := procname + 'sint';
          end;

        { free the errornode we generated in the beginning }
        result.free;
        { create the call node, }
        result := ccallnode.createintern(procname,newparas);
      end;


    function tinlinenode.handle_reset_rewrite_typed: tnode;
      begin
        { since this is a "in_xxxx_typedfile" node, we can be sure we have  }
        { a typed file as argument and we don't have to check it again (JM) }

        { add the recsize parameter }
        { note: for some reason, the parameter of intern procedures with only one }
        {   parameter is gets lifted out of its original tcallparanode (see round }
        {   line 1306 of ncal.pas), so recreate a tcallparanode here (JM)         }
        left := ccallparanode.create(cordconstnode.create(
          tfiledef(left.resultdef).typedfiledef.size,s32inttype,true),
          ccallparanode.create(left,nil));
        { create the correct call }
        if inlinenumber=in_reset_typedfile then
          result := ccallnode.createintern('fpc_reset_typed',left)
        else
          result := ccallnode.createintern('fpc_rewrite_typed',left);
        { make sure left doesn't get disposed, since we use it in the new call }
        left := nil;
      end;


    procedure maybe_convert_to_string(var n: tnode);
      begin
        { stringconstnodes are arrays of char. It's much more }
        { efficient to write a constant string, so convert    }
        { either to shortstring or ansistring depending on    }
        { length                                              }
        if (n.nodetype=stringconstn) then
          if is_chararray(n.resultdef) then
            if (tstringconstnode(n).len<=255) then
              inserttypeconv(n,cshortstringtype)
            else
              inserttypeconv(n,cansistringtype)
          else if is_widechararray(n.resultdef) then
            inserttypeconv(n,cwidestringtype);
      end;


    function Tinlinenode.handle_text_read_write(filepara,params:Ttertiarynode;var newstatement:Tnode):boolean;

    {Read(ln)/write(ln) for text files.}

    const  procprefixes:array[boolean] of string[15]=('fpc_write_text_','fpc_read_text_');

    var error_para,is_real,special_handling,found_error,do_read:boolean;
        p1:Tnode;
        nextpara,
        indexpara,
        lenpara,
        para,
        fracpara:Tcallparanode;
        temp:Ttempcreatenode;
        readfunctype:Tdef;
        name:string[31];

    begin
      para:=Tcallparanode(params);
      found_error:=false;
      do_read:=inlinenumber in [in_read_x,in_readln_x,in_readstr_x];
      while assigned(para) do
        begin
          { is this parameter faulty? }
          error_para:=false;
          { is this parameter a real? }
          is_real:=false;
          { type used for the read(), this is used to check
            whether a temp is needed for range checking }
          readfunctype:=nil;

          { can't read/write types }
          if para.left.nodetype=typen then
            begin
              CGMessagePos(para.fileinfo,type_e_cant_read_write_type);
              error_para := true;
            end;

          { support writeln(procvar) }
          if para.left.resultdef.typ=procvardef then
            begin
              p1:=ccallnode.create_procvar(nil,para.left);
              typecheckpass(p1);
              para.left:=p1;
            end;

          if inlinenumber in [in_write_x,in_writeln_x] then
            { prefer strings to chararrays }
            maybe_convert_to_string(para.left);

          case para.left.resultdef.typ of
            stringdef :
              name:=procprefixes[do_read]+tstringdef(para.left.resultdef).stringtypname;
            pointerdef :
              begin
                if (not is_pchar(para.left.resultdef)) or do_read then
                  begin
                    CGMessagePos(para.fileinfo,type_e_cant_read_write_type);
                    error_para := true;
                  end
                else
                  name:=procprefixes[do_read]+'pchar_as_pointer';
              end;
            floatdef :
              begin
                is_real:=true;
                if Tfloatdef(para.left.resultdef).floattype=s64currency then
                  name := procprefixes[do_read]+'currency'
                else
                  begin
                    name := procprefixes[do_read]+'float';
                    readfunctype:=pbestrealtype^;
                  end;
              end;
            enumdef:
              begin
                name:=procprefixes[do_read]+'enum';
                readfunctype:=s32inttype;
              end;
            orddef :
              begin
                case Torddef(para.left.resultdef).ordtype of
{$ifdef cpu64bit}
                  s64bit,
{$endif cpu64bit}
                  s8bit,
                  s16bit,
                  s32bit :
                    begin
                      name := procprefixes[do_read]+'sint';
                      readfunctype:=sinttype;
                    end;
{$ifdef cpu64bit}
                  u64bit,
{$endif cpu64bit}
                  u8bit,
                  u16bit,
                  u32bit :
                    begin
                      name := procprefixes[do_read]+'uint';
                      readfunctype:=uinttype;
                    end;
                  uchar :
                    begin
                      name := procprefixes[do_read]+'char';
                      readfunctype:=cchartype;
                    end;
                  uwidechar :
                    begin
                      name := procprefixes[do_read]+'widechar';
                      readfunctype:=cwidechartype;
                    end;
{$ifndef cpu64bit}
                  s64bit :
                    begin
                      name := procprefixes[do_read]+'int64';
                      readfunctype:=s64inttype;
                    end;
                  u64bit :
                    begin
                      name := procprefixes[do_read]+'qword';
                      readfunctype:=u64inttype;
                    end;
{$endif cpu64bit}
                  scurrency:
                    begin
                      name := procprefixes[do_read]+'currency';
                      readfunctype:=s64currencytype;
                      is_real:=true;
                    end;
                  bool8bit,
                  bool16bit,
                  bool32bit,
                  bool64bit:
                    if do_read then
                      begin
                        CGMessagePos(para.fileinfo,type_e_cant_read_write_type);
                        error_para := true;
                      end
                    else
                      begin
                        name := procprefixes[do_read]+'boolean';
                        readfunctype:=booltype;
                      end
                  else
                    CGMessagePos(para.fileinfo,type_e_cant_read_write_type);
                    error_para := true;
                end;
              end;
            variantdef :
              name:=procprefixes[do_read]+'variant';
            arraydef :
              begin
                if is_chararray(para.left.resultdef) then
                  name := procprefixes[do_read]+'pchar_as_array'
                else
                  begin
                    CGMessagePos(para.fileinfo,type_e_cant_read_write_type);
                    error_para := true;
                  end
              end
            else
              CGMessagePos(para.fileinfo,type_e_cant_read_write_type);
              error_para := true;
          end;

          { check for length/fractional colon para's }
          fracpara:=nil;
          lenpara:=nil;
          indexpara:=nil;
          if assigned(para.right) and
             (cpf_is_colon_para in tcallparanode(para.right).callparaflags) then
            begin
              lenpara := tcallparanode(para.right);
              if assigned(lenpara.right) and
                 (cpf_is_colon_para in tcallparanode(lenpara.right).callparaflags) then
                fracpara:=tcallparanode(lenpara.right);
            end;
          { get the next parameter now already, because we're going }
          { to muck around with the pointers                        }
          if assigned(fracpara) then
            nextpara := tcallparanode(fracpara.right)
          else if assigned(lenpara) then
            nextpara := tcallparanode(lenpara.right)
          else
            nextpara := tcallparanode(para.right);

          { check if a fracpara is allowed }
          if assigned(fracpara) and not is_real then
            begin
              CGMessagePos(fracpara.fileinfo,parser_e_illegal_colon_qualifier);
              error_para := true;
            end
          else if assigned(lenpara) and do_read then
            begin
              { I think this is already filtered out by parsing, but I'm not sure (JM) }
              CGMessagePos(lenpara.fileinfo,parser_e_illegal_colon_qualifier);
              error_para := true;
            end;

          { adjust found_error }
          found_error := found_error or error_para;

          if not error_para then
            begin
              special_handling:=false;
              { create dummy frac/len para's if necessary }
              if not do_read then
                begin
                  { difference in default value for floats and the rest :( }
                  if not is_real then
                    begin
                      if not assigned(lenpara) then
                        lenpara := ccallparanode.create(
                          cordconstnode.create(0,s32inttype,false),nil)
                      else
                        { make sure we don't pass the successive }
                        { parameters too. We also already have a }
                        { reference to the next parameter in     }
                        { nextpara                               }
                        lenpara.right := nil;
                    end
                  else
                    begin
                      if not assigned(lenpara) then
                        lenpara := ccallparanode.create(
                          cordconstnode.create(int64(-32767),s32inttype,false),nil);
                      { also create a default fracpara if necessary }
                      if not assigned(fracpara) then
                        fracpara := ccallparanode.create(
                          cordconstnode.create(int64(-1),s32inttype,false),nil);
                      { add it to the lenpara }
                      lenpara.right := fracpara;
                      if not is_currency(para.left.resultdef) then
                        begin
                          { and add the realtype para (this also removes the link }
                          { to any parameters coming after it)                    }
                          fracpara.right := ccallparanode.create(
                              cordconstnode.create(ord(tfloatdef(para.left.resultdef).floattype),
                              s32inttype,true),nil);
                        end;
                    end;
                  if para.left.resultdef.typ=enumdef then
                    begin
                      {To write(ln) an enum we need a some extra parameters.}
                      {Insert a reference to the ord2string index.}
                      indexpara:=Ccallparanode.create(
                        Caddrnode.create_internal(
                          Crttinode.create(Tenumdef(para.left.resultdef),fullrtti,rdt_normal)
                        ),
                        nil);
                      {Insert a reference to the typinfo.}
                      indexpara:=Ccallparanode.create(
                        Caddrnode.create_internal(
                         Crttinode.create(Tenumdef(para.left.resultdef),fullrtti,rdt_ord2str)
                        ),
                        indexpara);
                      {Insert a type conversion to to convert the enum to longint.}
                      para.left:=Ctypeconvnode.create_internal(para.left,s32inttype);
                      typecheckpass(para.left);
                    end;
                end
              else
                begin
                  {To read(ln) an enum we need a an extra parameter.}
                  if para.left.resultdef.typ=enumdef then
                    begin
                      {Insert a reference to the string2ord index.}
                      indexpara:=Ccallparanode.create(Caddrnode.create_internal(
                        Crttinode.create(Tenumdef(para.left.resultdef),fullrtti,rdt_str2ord)
                      ),nil);
                      {Insert a type conversion to to convert the enum to longint.}
                      para.left:=Ctypeconvnode.create_internal(para.left,s32inttype);
                      typecheckpass(para.left);
                    end;
                  { special handling of reading small numbers, because the helpers  }
                  { expect a longint/card/bestreal var parameter. Use a temp. can't }
                  { use functions because then the call to FPC_IOCHECK destroys     }
                  { their result before we can store it                             }
                  if (readfunctype<>nil) and (para.left.resultdef<>readfunctype) then
                    special_handling:=true;
                end;
              if special_handling then
                begin
                  { since we're not going to pass the parameter as var-parameter }
                  { to the read function, manually check whether the parameter   }
                  { can be used as var-parameter (e.g., whether it isn't a       }
                  { property)                                                    }
                  valid_for_var(para.left,true);

                  { create the parameter list: the temp ... }
                  temp := ctempcreatenode.create(readfunctype,readfunctype.size,tt_persistent,false);
                  addstatement(Tstatementnode(newstatement),temp);

                  { ... and the file }
                  p1 := ccallparanode.create(ctemprefnode.create(temp),
                    filepara.getcopy);
                  Tcallparanode(Tcallparanode(p1).right).right:=indexpara;

                  { create the call to the helper }
                  addstatement(Tstatementnode(newstatement),
                    ccallnode.createintern(name,tcallparanode(p1)));

                  { assign the result to the original var (this automatically }
                  { takes care of range checking)                             }
                  addstatement(Tstatementnode(newstatement),
                    cassignmentnode.create(para.left,
                      ctemprefnode.create(temp)));

                  { release the temp location }
                  addstatement(Tstatementnode(newstatement),ctempdeletenode.create(temp));

                  { statement of para is used }
                  para.left := nil;

                  { free the enclosing tcallparanode, but not the }
                  { parameters coming after it                    }
                  para.right := nil;
                  para.free;
                end
              else
                { read of non s/u-8/16bit, or a write }
                begin
                  { add the filepara to the current parameter }
                  para.right := filepara.getcopy;
                  {Add the lenpara and the indexpara(s) (fracpara and realtype are
                   already linked with the lenpara if necessary).}
                  if indexpara=nil then
                    Tcallparanode(para.right).right:=lenpara
                  else
                    begin
                      if lenpara=nil then
                        Tcallparanode(para.right).right:=indexpara
                      else
                        begin
                          Tcallparanode(para.right).right:=lenpara;
                          lenpara.right:=indexpara;
                        end;
{                      indexpara.right:=lenpara;}
                    end;
                  { in case of writing a chararray, add whether it's }
                  { zero-based                                       }
                  if para.left.resultdef.typ=arraydef then
                    para := ccallparanode.create(cordconstnode.create(
                      ord(tarraydef(para.left.resultdef).lowrange=0),booltype,false),para);
                  { create the call statement }
                  addstatement(Tstatementnode(newstatement),
                    ccallnode.createintern(name,para));
                end
            end
          else
            { error_para = true }
            begin
              { free the parameter, since it isn't referenced anywhere anymore }
              para.right := nil;
              para.free;
              if assigned(lenpara) then
                begin
                  lenpara.right := nil;
                  lenpara.free;
                end;
              if assigned(fracpara) then
                begin
                  fracpara.right := nil;
                  fracpara.free;
                end;
            end;

          { process next parameter }
          para := nextpara;
        end;

      { if no error, add the write(ln)/read(ln) end calls }
      if not found_error then
        begin
          case inlinenumber of
            in_read_x,
            in_readstr_x:
              name:='fpc_read_end';
            in_write_x,
            in_writestr_x:
              name:='fpc_write_end';
            in_readln_x:
              name:='fpc_readln_end';
            in_writeln_x:
              name:='fpc_writeln_end';
          end;
          addstatement(Tstatementnode(newstatement),ccallnode.createintern(name,filepara));
        end;
      handle_text_read_write:=found_error;
    end;

    function Tinlinenode.handle_typed_read_write(filepara,params:Ttertiarynode;var newstatement:Tnode):boolean;

    {Read/write for typed files.}

    const  procprefixes:array[boolean] of string[15]=('fpc_typed_write','fpc_typed_read');
           procnamesdisplay:array[boolean,boolean] of string[8] = (('Write','Read'),('WriteStr','ReadStr'));

    var found_error,do_read,is_rwstr:boolean;
        para,nextpara:Tcallparanode;
        p1:Tnode;
        temp:Ttempcreatenode;

    begin
      found_error:=false;
      para:=Tcallparanode(params);
      do_read:=inlinenumber in [in_read_x,in_readln_x,in_readstr_x];
      is_rwstr := inlinenumber in [in_readstr_x,in_writestr_x];
      { add the typesize to the filepara }
      if filepara.resultdef.typ=filedef then
        filepara.right := ccallparanode.create(cordconstnode.create(
          tfiledef(filepara.resultdef).typedfiledef.size,s32inttype,true),nil);

      { check for "no parameters" (you need at least one extra para for typed files) }
      if not assigned(para) then
        begin
          CGMessage1(parser_e_wrong_parameter_size,procnamesdisplay[is_rwstr,do_read]);
          found_error := true;
        end;

      { process all parameters }
      while assigned(para) do
        begin
          { check if valid parameter }
          if para.left.nodetype=typen then
            begin
              CGMessagePos(para.left.fileinfo,type_e_cant_read_write_type);
              found_error := true;
            end;

          { support writeln(procvar) }
          if (para.left.resultdef.typ=procvardef) then
            begin
              p1:=ccallnode.create_procvar(nil,para.left);
              typecheckpass(p1);
              para.left:=p1;
            end;

          if filepara.resultdef.typ=filedef then
            inserttypeconv(para.left,tfiledef(filepara.resultdef).typedfiledef);

          if assigned(para.right) and
            (cpf_is_colon_para in tcallparanode(para.right).callparaflags) then
            begin
              CGMessagePos(para.right.fileinfo,parser_e_illegal_colon_qualifier);

              { skip all colon para's }
              nextpara := tcallparanode(tcallparanode(para.right).right);
              while assigned(nextpara) and (cpf_is_colon_para in nextpara.callparaflags) do
                nextpara := tcallparanode(nextpara.right);
              found_error := true;
            end
          else
            { get next parameter }
            nextpara := tcallparanode(para.right);

          { When we have a call, we have a problem: you can't pass the  }
          { result of a call as a formal const parameter. Solution:     }
          { assign the result to a temp and pass this temp as parameter }
          { This is not very efficient, but write(typedfile,x) is       }
          { already slow by itself anyway (no buffering) (JM)           }
          { Actually, thge same goes for every non-simple expression    }
          { (such as an addition, ...) -> put everything but load nodes }
          { into temps (JM)                                             }
          { of course, this must only be allowed for writes!!! (JM)     }
          if not(do_read) and (para.left.nodetype <> loadn) then
            begin
              { create temp for result }
              temp := ctempcreatenode.create(para.left.resultdef,
                para.left.resultdef.size,tt_persistent,false);
              addstatement(Tstatementnode(newstatement),temp);
              { assign result to temp }
              addstatement(Tstatementnode(newstatement),
               cassignmentnode.create(ctemprefnode.create(temp),
                 para.left));
              { replace (reused) paranode with temp }
              para.left := ctemprefnode.create(temp);
            end;
          { add fileparameter }
          para.right := filepara.getcopy;

          { create call statment                                             }
          { since the parameters are in the correct order, we have to insert }
          { the statements always at the end of the current block            }
          addstatement(Tstatementnode(newstatement),
            Ccallnode.createintern(procprefixes[do_read],para
          ));

          { if we used a temp, free it }
          if para.left.nodetype = temprefn then
            addstatement(Tstatementnode(newstatement),ctempdeletenode.create(temp));

          { process next parameter }
          para := nextpara;
        end;

      { free the file parameter }
      filepara.free;
      handle_typed_read_write:=found_error;
    end;

    function tinlinenode.handle_read_write: tnode;

      var
        filepara,
        nextpara,
        params   : tcallparanode;
        newstatement  : tstatementnode;
        newblock      : tblocknode;
        filetemp      : Ttempcreatenode;
        name          : string[31];
        textsym       : ttypesym;
        is_typed,
        do_read,
        is_rwstr,
        found_error   : boolean;
      begin
        filepara := nil;
        is_typed := false;
        filetemp := nil;
        do_read := inlinenumber in [in_read_x,in_readln_x,in_readstr_x];
        is_rwstr := inlinenumber in [in_readstr_x,in_writestr_x];

        { if we fail, we can quickly exit this way. We must generate something }
        { instead of the inline node, because firstpass will bomb with an      }
        { internalerror if it encounters a read/write                          }
        result := cerrornode.create;

        { reverse the parameters (needed to get the colon parameters in the }
        { correct order when processing write(ln)                           }
        left := reverseparameters(tcallparanode(left));

        if is_rwstr then
          begin
            filepara := tcallparanode(left);
            { needs at least two parameters: source/dest string + min. 1 value }
            if not(assigned(filepara)) or
               not(assigned(filepara.right)) then
              begin
                CGMessagePos1(fileinfo,parser_e_wrong_parameter_size,'ReadStr/WriteStr');
                exit;
              end
            else if (filepara.resultdef.typ <> stringdef) then
              begin
                { convert chararray to string, or give an appropriate error message }
                { (if you want to optimize to use shortstring, keep in mind that    }
                {  readstr internally always uses ansistring, and to account for    }
                {  chararrays with > 255 characters)                                }
                inserttypeconv(filepara.left,cansistringtype);
                filepara.resultdef:=filepara.left.resultdef;
                if codegenerror then
                  exit;
              end
          end
        else if assigned(left) then
          begin
            { check if we have a file parameter and if yes, what kind it is }
            filepara := tcallparanode(left);

            if (filepara.resultdef.typ=filedef) then
              begin
                if (tfiledef(filepara.resultdef).filetyp=ft_untyped) then
                  begin
                    CGMessagePos(fileinfo,type_e_no_read_write_for_untyped_file);
                    exit;
                  end
                else
                  begin
                    if (tfiledef(filepara.resultdef).filetyp=ft_typed) then
                      begin
                        if (inlinenumber in [in_readln_x,in_writeln_x]) then
                          begin
                            CGMessagePos(fileinfo,type_e_no_readln_writeln_for_typed_file);
                            exit;
                          end;
                        is_typed := true;
                      end
                  end;
              end
            else
              filepara := nil;
          end;

        { create a blocknode in which the successive write/read statements will be  }
        { put, since they belong together. Also create a dummy statement already to }
        { make inserting of additional statements easier                            }
        newblock:=internalstatements(newstatement);

        { if we don't have a filepara, create one containing the default }
        if not assigned(filepara) or
           is_rwstr then
          begin
            { since the input/output variables are threadvars loading them into
              a temp once is faster. Create a temp which will hold a pointer to the file }
            filetemp := ctempcreatenode.create(voidpointertype,voidpointertype.size,tt_persistent,true);
            addstatement(newstatement,filetemp);

            { make sure the resultdef of the temp (and as such of the }
            { temprefs coming after it) is set (necessary because the  }
            { temprefs will be part of the filepara, of which we need  }
            { the resultdef later on and temprefs can only be         }
            { typecheckpassed if the resultdef of the temp is known) }
            typecheckpass(tnode(filetemp));

            if not is_rwstr then
              begin
                { assign the address of the file to the temp }
                if do_read then
                  name := 'input'
                else
                  name := 'output';
                addstatement(newstatement,
                  cassignmentnode.create(ctemprefnode.create(filetemp),
                    ccallnode.createintern('fpc_get_'+name,nil)));
              end
            else
              begin
                if (do_read) then
                  name := 'fpc_setupreadstr_'
                else
                  name := 'fpc_setupwritestr_';
                name:=name+tstringdef(filepara.resultdef).stringtypname;
                { remove the source/destination string parameter from the }
                { parameter chain                                         }
                left:=filepara.right;
                filepara.right:=nil;
                { pass the source/destination string to the setup routine, which }
                { will store the string's address in the returned textrec        }
                addstatement(newstatement,
                  cassignmentnode.create(ctemprefnode.create(filetemp),
                    ccallnode.createintern(name,filepara)));
              end;

            { create a new fileparameter as follows: file_type(temp^)    }
            { (so that we pass the value and not the address of the temp }
            { to the read/write routine)                                 }
            textsym:=search_system_type('TEXT');
            filepara := ccallparanode.create(ctypeconvnode.create_internal(
              cderefnode.create(ctemprefnode.create(filetemp)),textsym.typedef),nil);
          end
        else
          { remove filepara from the parameter chain }
          begin
            left := filepara.right;
            filepara.right := nil;
            { the file para is a var parameter, but it must be valid already }
            set_varstate(filepara.left,vs_readwritten,[vsf_must_be_valid]);
            { check if we should make a temp to store the result of a complex }
            { expression (better heuristics, anyone?) (JM)                    }
            if (filepara.left.nodetype <> loadn) then
              begin
                { create a temp which will hold a pointer to the file }
                filetemp := ctempcreatenode.create(voidpointertype,voidpointertype.size,tt_persistent,true);

                { add it to the statements }
                addstatement(newstatement,filetemp);

                { make sure the resultdef of the temp (and as such of the }
                { temprefs coming after it) is set (necessary because the  }
                { temprefs will be part of the filepara, of which we need  }
                { the resultdef later on and temprefs can only be         }
                { typecheckpassed if the resultdef of the temp is known) }
                typecheckpass(tnode(filetemp));

                { assign the address of the file to the temp }
                addstatement(newstatement,
                  cassignmentnode.create(ctemprefnode.create(filetemp),
                    caddrnode.create_internal(filepara.left)));
                typecheckpass(newstatement.left);
                { create a new fileparameter as follows: file_type(temp^)    }
                { (so that we pass the value and not the address of the temp }
                { to the read/write routine)                                 }
                nextpara := ccallparanode.create(ctypeconvnode.create_internal(
                  cderefnode.create(ctemprefnode.create(filetemp)),filepara.left.resultdef),nil);

                { replace the old file para with the new one }
                filepara.left := nil;
                filepara.free;
                filepara := nextpara;
              end;
          end;

        { the resultdef of the filepara must be set since it's }
        { used below                                            }
        filepara.get_paratype;

        { now, filepara is nowhere referenced anymore, so we can safely dispose it }
        { if something goes wrong or at the end of the procedure                   }

        { we're going to reuse the paranodes, so make sure they don't get freed }
        { twice                                                                 }
        params:=Tcallparanode(left);
        left := nil;

        if is_typed then
          found_error:=handle_typed_read_write(filepara,Ttertiarynode(params),newstatement)
        else
          found_error:=handle_text_read_write(filepara,Ttertiarynode(params),newstatement);

        { if we found an error, simply delete the generated blocknode }
        if found_error then
          newblock.free
        else
          begin
            { deallocate the temp for the file para if we used one }
            if assigned(filetemp) then
              addstatement(newstatement,ctempdeletenode.create(filetemp));
            { otherwise return the newly generated block of instructions, }
            { but first free the errornode we generated at the beginning }
            result.free;
            result := newblock
          end;
      end;


    function tinlinenode.handle_val: tnode;
      var
        procname,
        suffix        : string[31];
        sourcepara,
        destpara,
        codepara,
        sizepara,
        newparas      : tcallparanode;
        orgcode,tc    : tnode;
        newstatement  : tstatementnode;
        newblock      : tblocknode;
        tempcode      : ttempcreatenode;
      begin
        { for easy exiting if something goes wrong }
        result := cerrornode.create;

        { check the amount of parameters }
        if not(assigned(left)) or
           not(assigned(tcallparanode(left).right)) then
         begin
           CGMessage1(parser_e_wrong_parameter_size,'Val');
           exit;
         end;

        { reverse parameters for easier processing }
        left := reverseparameters(tcallparanode(left));

        { get the parameters }
        tempcode := nil;
        orgcode := nil;
        sizepara := nil;
        sourcepara := tcallparanode(left);
        destpara := tcallparanode(sourcepara.right);
        codepara := tcallparanode(destpara.right);

        { check if codepara is valid }
        if assigned(codepara) and
           (
            (codepara.resultdef.typ <> orddef)
{$ifndef cpu64bit}
            or is_64bitint(codepara.resultdef)
{$endif cpu64bit}
            ) then
          begin
            CGMessagePos1(codepara.fileinfo,type_e_integer_expr_expected,codepara.resultdef.typename);
            exit;
          end;

        { check if dest para is valid }
        if not(destpara.resultdef.typ in [orddef,floatdef,enumdef]) then
          begin
            CGMessagePos(destpara.fileinfo,type_e_integer_or_real_expr_expected);
            exit;
          end;

        { we're going to reuse the exisiting para's, so make sure they }
        { won't be disposed                                            }
        left := nil;

        { create the blocknode which will hold the generated statements + }
        { an initial dummy statement                                      }

        newblock:=internalstatements(newstatement);

        { do we need a temp for code? Yes, if no code specified, or if  }
        { code is not a 32bit parameter (we already checked whether the }
        { the code para, if specified, was an orddef)                   }
        if not assigned(codepara) or
           (codepara.resultdef.size<>sinttype.size) then
          begin
            tempcode := ctempcreatenode.create(sinttype,sinttype.size,tt_persistent,false);
            addstatement(newstatement,tempcode);
            { set the resultdef of the temp (needed to be able to get }
            { the resultdef of the tempref used in the new code para) }
            typecheckpass(tnode(tempcode));
            { create a temp codepara, but save the original code para to }
            { assign the result to later on                              }
            if assigned(codepara) then
              begin
                orgcode := codepara.left;
                codepara.left := ctemprefnode.create(tempcode);
              end
            else
              codepara := ccallparanode.create(ctemprefnode.create(tempcode),nil);
            { we need its resultdef later on }
            codepara.get_paratype;
          end
        else if (torddef(codepara.resultdef).ordtype = torddef(sinttype).ordtype) then
          { because code is a var parameter, it must match types exactly    }
          { however, since it will return values in [0..255], both longints }
          { and cardinals are fine. Since the formal code para type is      }
          { longint, insert a typecoversion to longint for cardinal para's  }
          begin
            codepara.left := ctypeconvnode.create_internal(codepara.left,sinttype);
            { make it explicit, oterwise you may get a nonsense range }
            { check error if the cardinal already contained a value   }
            { > $7fffffff                                             }
            codepara.get_paratype;
          end;

        { create the procedure name }
        procname := 'fpc_val_';

        case destpara.resultdef.typ of
          orddef:
            begin
              case torddef(destpara.resultdef).ordtype of
{$ifdef cpu64bit}
                s64bit,
{$endif cpu64bit}
                s8bit,
                s16bit,
                s32bit:
                  begin
                    suffix := 'sint_';
                    { we also need a destsize para in this case }
                    sizepara := ccallparanode.create(cordconstnode.create
                      (destpara.resultdef.size,s32inttype,true),nil);
                  end;
{$ifdef cpu64bit}
                u64bit,
{$endif cpu64bit}
                u8bit,
                u16bit,
                u32bit:
                   suffix := 'uint_';
{$ifndef cpu64bit}
                s64bit: suffix := 'int64_';
                u64bit: suffix := 'qword_';
{$endif cpu64bit}
                scurrency: suffix := 'currency_';
                else
                  internalerror(200304225);
              end;
            end;
          floatdef:
            suffix:='real_';
          enumdef:
            begin
              suffix:='enum_';
              sizepara:=Ccallparanode.create(Caddrnode.create_internal(
                Crttinode.create(Tenumdef(destpara.resultdef),fullrtti,rdt_str2ord)
              ),nil);
            end;
        end;

        procname := procname + suffix;

        { play a trick to have tcallnode handle invalid source parameters: }
        { the shortstring-longint val routine by default                   }
        if (sourcepara.resultdef.typ = stringdef) then
          procname := procname + tstringdef(sourcepara.resultdef).stringtypname
        { zero-based arrays (of char) can be implicitely converted to ansistring }
        else if is_zero_based_array(sourcepara.resultdef) then
          procname := procname + 'ansistr'
        else
          procname := procname + 'shortstr';

        { set up the correct parameters for the call: the code para... }
        newparas := codepara;
        { and the source para }
        codepara.right := sourcepara;
        { sizepara either contains nil if none is needed (which is ok, since   }
        { then the next statement severes any possible links with other paras  }
        { that sourcepara may have) or it contains the necessary size para and }
        { its right field is nil                                               }
        sourcepara.right := sizepara;

        { create the call and assign the result to dest (val helpers are functions).
          Use a trick to prevent a type size mismatch warning to be generated by the
          assignment node. First convert implicitly to the resultdef. This will insert
          the range check. The Second conversion is done explicitly to hide the implicit conversion
          for the assignment node and therefor preventing the warning (PFV)

          The implicit conversion is avoided for enums because implicit conversion between
          longint (which is what fpc_val_enum_shortstr returns) and enumerations is not
          possible. (DM).}
        if destpara.resultdef.typ=enumdef then
          tc:=ccallnode.createintern(procname,newparas)
        else
          tc:=ctypeconvnode.create(ccallnode.createintern(procname,newparas),destpara.left.resultdef);
        addstatement(newstatement,cassignmentnode.create(
          destpara.left,ctypeconvnode.create_internal(tc,destpara.left.resultdef)));

        { dispose of the enclosing paranode of the destination }
        destpara.left := nil;
        destpara.right := nil;
        destpara.free;

        { check if we used a temp for code and whether we have to store }
        { it to the real code parameter                                 }
        if assigned(orgcode) then
          addstatement(newstatement,cassignmentnode.create(
              orgcode,
              ctypeconvnode.create_internal(
                ctemprefnode.create(tempcode),orgcode.resultdef)));

        { release the temp if we allocated one }
        if assigned(tempcode) then
          addstatement(newstatement,ctempdeletenode.create(tempcode));

        { free the errornode }
        result.free;
        { and return it }
        result := newblock;
      end;

{$maxfpuregisters 0}

    function getpi : bestreal;
      begin
      {$ifdef x86}
        { x86 has pi in hardware }
        result:=pi;
      {$else x86}
        {$ifdef cpuextended}
          result:=MathPiExtended.Value;
        {$else cpuextended}
          result:=MathPi.Value;
        {$endif cpuextended}
      {$endif x86}
      end;


    function tinlinenode.simplify: tnode;

      function do_lowhigh(def:tdef) : tnode;
        var
           v    : tconstexprint;
           enum : tenumsym;
           hp   : tnode;
        begin
           case def.typ of
             orddef:
               begin
                  set_varstate(left,vs_read,[]);
                  if inlinenumber=in_low_x then
                    v:=torddef(def).low
                  else
                    v:=torddef(def).high;
                  hp:=cordconstnode.create(v,def,true);
                  typecheckpass(hp);
                  do_lowhigh:=hp;
               end;
             enumdef:
               begin
                  set_varstate(left,vs_read,[]);
                  enum:=tenumsym(tenumdef(def).firstenum);
                  v:=tenumdef(def).maxval;
                  if inlinenumber=in_high_x then
                    while assigned(enum) and (enum.value <> v) do
                      enum:=enum.nextenum;
                  if not assigned(enum) then
                    internalerror(309993)
                  else
                    hp:=genenumnode(enum);
                  do_lowhigh:=hp;
               end;
           else
             internalerror(87);
           end;
        end;


      function getconstrealvalue : bestreal;
        begin
           case left.nodetype of
              ordconstn:
                getconstrealvalue:=tordconstnode(left).value;
              realconstn:
                getconstrealvalue:=trealconstnode(left).value_real;
              else
                internalerror(309992);
           end;
        end;


      procedure setconstrealvalue(r : bestreal);
        begin
           result:=crealconstnode.create(r,pbestrealtype^);
        end;


      function handle_ln_const(r : bestreal) : tnode;
        begin
          if r<=0.0 then
            if (cs_check_range in current_settings.localswitches) or
               (cs_check_overflow in current_settings.localswitches) then
               begin
                 result:=crealconstnode.create(0,pbestrealtype^);
                 CGMessage(type_e_wrong_math_argument)
               end
            else
              begin
                if r=0.0 then
                  result:=crealconstnode.create(MathQNaN.Value,pbestrealtype^)
                else
                  result:=crealconstnode.create(MathNegInf.Value,pbestrealtype^)
              end
          else
            result:=crealconstnode.create(ln(r),pbestrealtype^)
        end;


      function handle_sqrt_const(r : bestreal) : tnode;
        begin
          if r<0.0 then
            if (cs_check_range in current_settings.localswitches) or
               (cs_check_overflow in current_settings.localswitches) then
               begin
                 result:=crealconstnode.create(0,pbestrealtype^);
                 CGMessage(type_e_wrong_math_argument)
               end
            else
              result:=crealconstnode.create(MathQNaN.Value,pbestrealtype^)
          else
            result:=crealconstnode.create(sqrt(r),pbestrealtype^)
        end;


      var
        hp        : tnode;
        vl,vl2    : TConstExprInt;
        vr        : bestreal;
        checkrange: boolean;

      begin { simplify }
         result:=nil;
         { handle intern constant functions in separate case }
         if nf_inlineconst in flags then
          begin
            { no parameters? }
            if not assigned(left) then
              internalerror(200501231)
            else
             begin
               vl:=0;
               vl2:=0; { second parameter Ex: ptr(vl,vl2) }
               case left.nodetype of
                 realconstn :
                   begin
                     { Real functions are all handled with internproc below }
                     CGMessage1(type_e_integer_expr_expected,left.resultdef.typename)
                   end;
                 ordconstn :
                   vl:=tordconstnode(left).value;
                 callparan :
                   begin
                     { both exists, else it was not generated }
                     vl:=tordconstnode(tcallparanode(left).left).value;
                     vl2:=tordconstnode(tcallparanode(tcallparanode(left).right).left).value;
                   end;
                 else
                   CGMessage(parser_e_illegal_expression);
               end;
               case inlinenumber of
                 in_const_abs :
                   if vl.signed then
                     hp:=genintconstnode(abs(vl.svalue))
                   else
                     hp:=genintconstnode(vl.uvalue);
                 in_const_sqr:
                   if vl.signed then
                     hp:=genintconstnode(sqr(vl.svalue))
                   else
                     hp:=genintconstnode(sqr(vl.uvalue));
                 in_const_odd :
                   hp:=cordconstnode.create(qword(odd(int64(vl))),booltype,true);
                 in_const_swap_word :
                   hp:=cordconstnode.create((vl and $ff) shl 8+(vl shr 8),left.resultdef,true);
                 in_const_swap_long :
                   hp:=cordconstnode.create((vl and $ffff) shl 16+(vl shr 16),left.resultdef,true);
                 in_const_swap_qword :
                   hp:=cordconstnode.create((vl and $ffff) shl 32+(vl shr 32),left.resultdef,true);
                 in_const_ptr:
                   begin
                     {Don't construct pointers from negative values.}
                     if (vl.signed and (vl.svalue<0)) or (vl2.signed and (vl2.svalue<0)) then
                       cgmessage(parser_e_range_check_error);
                     hp:=cpointerconstnode.create((vl2.uvalue shl 4)+vl.uvalue,voidfarpointertype);
                   end
                 else
                   internalerror(88);
               end;
             end;
            if hp=nil then
              hp:=cerrornode.create;
            result:=hp;
          end
        else
          begin
            case inlinenumber of
              in_lo_long,
              in_hi_long,
              in_lo_qword,
              in_hi_qword,
              in_lo_word,
              in_hi_word :
                begin
                  if left.nodetype=ordconstn then
                    begin
                      case inlinenumber of
                        in_lo_word :
                          result:=cordconstnode.create(tordconstnode(left).value and $ff,u8inttype,true);
                        in_hi_word :
                          result:=cordconstnode.create(tordconstnode(left).value shr 8,u8inttype,true);
                        in_lo_long :
                          result:=cordconstnode.create(tordconstnode(left).value and $ffff,u16inttype,true);
                        in_hi_long :
                          result:=cordconstnode.create(tordconstnode(left).value shr 16,u16inttype,true);
                        in_lo_qword :
                          result:=cordconstnode.create(tordconstnode(left).value and $ffffffff,u32inttype,true);
                        in_hi_qword :
                          result:=cordconstnode.create(tordconstnode(left).value shr 32,u32inttype,true);
                      end;
                    end;
                end;
              in_ord_x:
                begin
                  case left.resultdef.typ of
                    orddef :
                      begin
                        case torddef(left.resultdef).ordtype of
                          bool8bit,
                          uchar:
                            begin
                              { change to byte() }
                              result:=ctypeconvnode.create_internal(left,u8inttype);
                              left:=nil;
                            end;
                          bool16bit,
                          uwidechar :
                            begin
                              { change to word() }
                              result:=ctypeconvnode.create_internal(left,u16inttype);
                              left:=nil;
                            end;
                          bool32bit :
                            begin
                              { change to dword() }
                              result:=ctypeconvnode.create_internal(left,u32inttype);
                              left:=nil;
                            end;
                          bool64bit :
                            begin
                              { change to qword() }
                              result:=ctypeconvnode.create_internal(left,u64inttype);
                              left:=nil;
                            end;
                          uvoid :
                            CGMessage1(type_e_ordinal_expr_expected,left.resultdef.typename);
                          else
                            begin
                              { all other orddef need no transformation }
                              result:=left;
                              left:=nil;
                            end;
                        end;
                      end;
                    enumdef :
                      begin
                        result:=ctypeconvnode.create_internal(left,s32inttype);
                        left:=nil;
                      end;
                    pointerdef :
                      begin
                        if m_mac in current_settings.modeswitches then
                          begin
                            result:=ctypeconvnode.create_internal(left,ptruinttype);
                            left:=nil;
                          end
                      end;
                  end;
(*
                  if (left.nodetype=ordconstn) then
                     begin
                       result:=cordconstnode.create(
                         tordconstnode(left).value,sinttype,true);
                     end
                   else if (m_mac in current_settings.modeswitches) and
                           (left.ndoetype=pointerconstn) then
                       result:=cordconstnode.create(
                         tpointerconstnode(left).value,ptruinttype,true);
*)
                end;
              in_chr_byte:
                begin
                   { convert to explicit char() }
                   result:=ctypeconvnode.create_internal(left,cchartype);
                   left:=nil;
                end;
              in_length_x:
                begin
                  case left.resultdef.typ of
                    stringdef :
                      begin
                        if (left.nodetype=stringconstn) then
                          begin
                            result:=cordconstnode.create(
                              tstringconstnode(left).len,sinttype,true);
                          end;
                      end;
                    orddef :
                      begin
                        { length of char is always one }
                        if is_char(left.resultdef) or
                           is_widechar(left.resultdef) then
                         begin
                           result:=cordconstnode.create(1,sinttype,false);
                         end
                      end;
                    arraydef :
                      begin
                        if not is_open_array(left.resultdef) and
                           not is_array_of_const(left.resultdef) and
                           not is_dynamic_array(left.resultdef) then
                          result:=cordconstnode.create(tarraydef(left.resultdef).highrange-
                            tarraydef(left.resultdef).lowrange+1,
                            sinttype,true);
                      end;
                  end;
                end;
              in_assigned_x:
                begin
                  if is_constnode(tcallparanode(left).left) or
                     (tcallparanode(left).left.nodetype = pointerconstn) then
                    begin
                      { let an add node figure it out }
                      result:=caddnode.create(unequaln,tcallparanode(left).left,cnilnode.create);
                      tcallparanode(left).left := nil;
                    end;
                end;
              in_pred_x,
              in_succ_x:
                begin
                  { only perform range checking if the result is an enum }
                  checkrange:=(resultdef.typ=enumdef);
   
                  if (left.nodetype=ordconstn) then
                   begin
                     if (inlinenumber=in_succ_x) then
                       result:=cordconstnode.create(tordconstnode(left).value+1,left.resultdef,checkrange)
                     else
                       result:=cordconstnode.create(tordconstnode(left).value-1,left.resultdef,checkrange);
                   end;
                end;
              in_low_x,
              in_high_x:
                begin
                  case left.resultdef.typ of
                    orddef,
                    enumdef:
                      begin
                        result:=do_lowhigh(left.resultdef);
                      end;
                    setdef:
                      begin
                        result:=do_lowhigh(tsetdef(left.resultdef).elementdef);
                      end;
                    arraydef:
                      begin
                        if (inlinenumber=in_low_x) then
                          begin
                            result:=cordconstnode.create(int64(tarraydef(
                             left.resultdef).lowrange),tarraydef(left.resultdef).rangedef,true);
                          end
                        else if not is_open_array(left.resultdef) and
                                not is_array_of_const(left.resultdef) and
                                not is_dynamic_array(left.resultdef) then
                          result:=cordconstnode.create(int64(tarraydef(left.resultdef).highrange),
                            tarraydef(left.resultdef).rangedef,true);
                      end;
                    stringdef:
                      begin
                        if inlinenumber=in_low_x then
                          begin
                            result:=cordconstnode.create(0,u8inttype,false);
                          end
                        else if not is_ansistring(left.resultdef) and
                                not is_widestring(left.resultdef) then
                          result:=cordconstnode.create(tstringdef(left.resultdef).len,u8inttype,true)
                      end;
                  end;
                end;
              in_exp_real :
                begin
                  if left.nodetype in [ordconstn,realconstn] then
                    begin
                      result:=crealconstnode.create(exp(getconstrealvalue),pbestrealtype^);
                      if (trealconstnode(result).value_real=MathInf.Value) and
                         ((cs_check_range in current_settings.localswitches) or
                          (cs_check_overflow in current_settings.localswitches)) then
                        begin
                          result:=crealconstnode.create(0,pbestrealtype^);
                          CGMessage(parser_e_range_check_error);
                        end;
                    end
                end;
              in_trunc_real :
                begin
                  if left.nodetype in [ordconstn,realconstn] then
                    begin
                      vr:=getconstrealvalue;
                      if (vr>=9223372036854775807.5) or (vr<=-9223372036854775808.5) then
                        begin
                          CGMessage(parser_e_range_check_error);
                          result:=cordconstnode.create(1,s64inttype,false)
                        end
                      else
                        result:=cordconstnode.create(trunc(vr),s64inttype,true)
                    end
                end;
              in_round_real :
                begin
                  if left.nodetype in [ordconstn,realconstn] then
                    begin
                      vr:=getconstrealvalue;
                      if (vr>=9223372036854775807.5) or (vr<=-9223372036854775808.5) then
                        begin
                          CGMessage(parser_e_range_check_error);
                          result:=cordconstnode.create(1,s64inttype,false)
                        end
                      else
                        result:=cordconstnode.create(round(vr),s64inttype,true)
                    end
                end;
              in_frac_real :
                begin
                  if left.nodetype in [ordconstn,realconstn] then
                    setconstrealvalue(frac(getconstrealvalue))
                end;
              in_int_real :
                begin
                  if left.nodetype in [ordconstn,realconstn] then
                    setconstrealvalue(int(getconstrealvalue));
                end;
              in_pi_real :
                 begin
                   if block_type=bt_const then
                     setconstrealvalue(getpi)
                 end;
              in_cos_real :
                begin
                  if left.nodetype in [ordconstn,realconstn] then
                    setconstrealvalue(cos(getconstrealvalue))
                end;
              in_sin_real :
                begin
                  if left.nodetype in [ordconstn,realconstn] then
                    setconstrealvalue(sin(getconstrealvalue))
                end;
              in_arctan_real :
                begin
                  if left.nodetype in [ordconstn,realconstn] then
                    setconstrealvalue(arctan(getconstrealvalue))
                end;
              in_abs_real :
                begin
                  if left.nodetype in [ordconstn,realconstn] then
                    setconstrealvalue(abs(getconstrealvalue))
                end;
              in_abs_long:
                begin
                  if left.nodetype=ordconstn then
                    begin
                      if tordconstnode(left).value<0 then
                        result:=cordconstnode.create((-tordconstnode(left).value),s32inttype,false)
                      else
                        result:=cordconstnode.create((tordconstnode(left).value),s32inttype,false);
                    end
                end;
              in_sqr_real :
                begin
                  if left.nodetype in [ordconstn,realconstn] then
                    setconstrealvalue(sqr(getconstrealvalue))
                end;
              in_sqrt_real :
                begin
                  if left.nodetype in [ordconstn,realconstn] then
                    begin
                      vr:=getconstrealvalue;
                      if vr<0.0 then
                        result:=handle_sqrt_const(vr)
                      else
                        setconstrealvalue(sqrt(vr));
                    end
                end;
              in_ln_real :
                begin
                  if left.nodetype in [ordconstn,realconstn] then
                   begin
                     vr:=getconstrealvalue;
                     if vr<=0.0 then
                       result:=handle_ln_const(vr)
                     else
                       setconstrealvalue(ln(vr));
                   end
                end;
              in_assert_x_y :
                begin
                  if not(cs_do_assertion in current_settings.localswitches) then
                    { we need a valid node, so insert a nothingn }
                    result:=cnothingnode.create;
                end;
            end;
          end;
      end;



    function tinlinenode.pass_typecheck:tnode;

      procedure setfloatresultdef;
        begin
          if (left.resultdef.typ=floatdef) and
            (tfloatdef(left.resultdef).floattype in [s32real,s64real,s80real,s128real]) then
            resultdef:=left.resultdef
          else
            begin
              if (left.nodetype <> ordconstn) then
                inserttypeconv(left,pbestrealtype^);
              resultdef:=pbestrealtype^;
            end;
        end;


      procedure handle_pack_unpack;
        var
          source, target, index: tcallparanode;
          unpackedarraydef, packedarraydef: tarraydef;
          tempindex: TConstExprInt;
        begin
          resultdef:=voidtype;

          unpackedarraydef := nil;
          packedarraydef := nil;
          source := tcallparanode(left);
          if (inlinenumber = in_unpack_x_y_z) then
            begin
              target := tcallparanode(source.right);
              index := tcallparanode(target.right);

              { source must be a packed array }
              if not is_packed_array(source.left.resultdef) then
                CGMessagePos2(source.left.fileinfo,type_e_got_expected_packed_array,'1',source.left.resultdef.GetTypeName)
              else
                packedarraydef := tarraydef(source.left.resultdef);
              { target can be any kind of array, as long as it's not packed }
              if (target.left.resultdef.typ <> arraydef) or
                 is_packed_array(target.left.resultdef) then
                CGMessagePos2(target.left.fileinfo,type_e_got_expected_unpacked_array,'2',target.left.resultdef.GetTypeName)
              else
                unpackedarraydef := tarraydef(target.left.resultdef);
            end
          else
            begin
              index := tcallparanode(source.right);
              target := tcallparanode(index.right);

              { source can be any kind of array, as long as it's not packed }
              if (source.left.resultdef.typ <> arraydef) or
                 is_packed_array(source.left.resultdef) then
                CGMessagePos2(source.left.fileinfo,type_e_got_expected_unpacked_array,'1',source.left.resultdef.GetTypeName)
              else
                unpackedarraydef := tarraydef(source.left.resultdef);
              { target must be a packed array }
              if not is_packed_array(target.left.resultdef) then
                CGMessagePos2(target.left.fileinfo,type_e_got_expected_packed_array,'3',target.left.resultdef.GetTypeName)
              else
                packedarraydef := tarraydef(target.left.resultdef);
            end;

          if assigned(unpackedarraydef) then
            begin
              { index must be compatible with the unpacked array's indextype }
              inserttypeconv(index.left,unpackedarraydef.rangedef);

              { range check at compile time if possible }
              if assigned(packedarraydef) and
                 (index.left.nodetype = ordconstn) and
                 not is_special_array(unpackedarraydef) then
                begin
                  testrange(unpackedarraydef,tordconstnode(index.left).value,false);
                  tempindex := tordconstnode(index.left).value + packedarraydef.highrange-packedarraydef.lowrange;
                  testrange(unpackedarraydef,tempindex,false);
                end;
            end;

          { source array is read and must be valid }
          set_varstate(source.left,vs_read,[vsf_must_be_valid]);
          { target array is written }
          valid_for_assignment(target.left,true);
          set_varstate(target.left,vs_written,[]);
          { index in the unpacked array is read and must be valid }
          set_varstate(index.left,vs_read,[vsf_must_be_valid]);
          { if the size of the arrays is 0 (array of empty records), }
          { do nothing                                               }
          if (source.resultdef.size = 0) then
            result:=cnothingnode.create;
        end;



      var
         hightree,
         hp        : tnode;
      begin
        result:=nil;
        { when handling writeln "left" contains no valid address }
        if assigned(left) then
          begin
            if left.nodetype=callparan then
              tcallparanode(left).get_paratype
            else
              typecheckpass(left);
          end;

        if not(nf_inlineconst in flags) then
          begin
            case inlinenumber of
              in_lo_long,
              in_hi_long,
              in_lo_qword,
              in_hi_qword,
              in_lo_word,
              in_hi_word :
                begin
                  { give warning for incompatibility with tp and delphi }
                  if (inlinenumber in [in_lo_long,in_hi_long,in_lo_qword,in_hi_qword]) and
                     ((m_tp7 in current_settings.modeswitches) or
                      (m_delphi in current_settings.modeswitches)) then
                    CGMessage(type_w_maybe_wrong_hi_lo);
                  set_varstate(left,vs_read,[vsf_must_be_valid]);
                  if not is_integer(left.resultdef) then
                    CGMessage1(type_e_integer_expr_expected,left.resultdef.typename);
                  case inlinenumber of
                    in_lo_word,
                    in_hi_word :
                      resultdef:=u8inttype;
                    in_lo_long,
                    in_hi_long :
                      resultdef:=u16inttype;
                    in_lo_qword,
                    in_hi_qword :
                      resultdef:=u32inttype;
                  end;
                end;

              in_sizeof_x:
                begin
                  { the constant evaluation of in_sizeof_x happens in pexpr where possible }
                  set_varstate(left,vs_read,[]);
                  if paramanager.push_high_param(vs_value,left.resultdef,current_procinfo.procdef.proccalloption) then
                   begin
                     hightree:=load_high_value_node(tparavarsym(tloadnode(left).symtableentry));
                     if assigned(hightree) then
                      begin
                        hp:=caddnode.create(addn,hightree,
                                         cordconstnode.create(1,sinttype,false));
                        if (left.resultdef.typ=arraydef) then
                          if not is_packed_array(tarraydef(left.resultdef)) then
                            begin
                              if (tarraydef(left.resultdef).elesize<>1) then
                                hp:=caddnode.create(muln,hp,cordconstnode.create(tarraydef(
                                  left.resultdef).elesize,sinttype,true));
                            end
                          else if (tarraydef(left.resultdef).elepackedbitsize <> 8) then
                            begin
                              { no packed open array support yet }
                              if (hp.nodetype <> ordconstn) then
                                internalerror(2006081511);
                              hp.free;
                              hp := cordconstnode.create(left.resultdef.size,sinttype,true);
{
                              hp:=
                                 ctypeconvnode.create_explicit(sinttype,
                                   cmoddivnode.create(divn,
                                     caddnode.create(addn,
                                       caddnode.create(muln,hp,cordconstnode.create(tarraydef(
                                         left.resultdef).elepackedbitsize,s64inttype,true)),
                                       cordconstnode.create(a,s64inttype,true)),
                                     cordconstnode.create(8,s64inttype,true)),
                                   sinttype);
}
                            end;
                        result:=hp;
                      end;
                   end
                  else
                   resultdef:=sinttype;
                end;

              in_typeof_x:
                begin
                  set_varstate(left,vs_read,[]);
                  resultdef:=voidpointertype;
                end;

              in_ord_x:
                begin
                   set_varstate(left,vs_read,[vsf_must_be_valid]);
                   case left.resultdef.typ of
                     orddef,
                     enumdef :
                       ;
                     pointerdef :
                       begin
                         if not(m_mac in current_settings.modeswitches) then
                           CGMessage1(type_e_ordinal_expr_expected,left.resultdef.typename);
                       end
                     else
                       CGMessage1(type_e_ordinal_expr_expected,left.resultdef.typename);
                   end;
                end;

             in_chr_byte:
               begin
                 set_varstate(left,vs_read,[vsf_must_be_valid]);
               end;

              in_length_x:
                begin
                  if ((left.resultdef.typ=arraydef) and
                      (not is_special_array(left.resultdef) or
                       is_open_array(left.resultdef))) or
                     (left.resultdef.typ=orddef) then
                    set_varstate(left,vs_read,[])
                  else
                    set_varstate(left,vs_read,[vsf_must_be_valid]);

                  case left.resultdef.typ of
                    variantdef:
                      begin
                        inserttypeconv(left,cansistringtype);
                      end;

                    stringdef :
                      begin
                        { we don't need string convertions here,  }
                        { except if from widestring to ansistring }
                        { and vice versa (that can change the     }
                        { length)                                 }
                        if (left.nodetype=typeconvn) and
                           (ttypeconvnode(left).left.resultdef.typ=stringdef) and
                           not(is_widestring(left.resultdef) xor
                               is_widestring(ttypeconvnode(left).left.resultdef)) then
                         begin
                           hp:=ttypeconvnode(left).left;
                           ttypeconvnode(left).left:=nil;
                           left.free;
                           left:=hp;
                         end;
                      end;
                    orddef :
                      begin
                        { will be handled in simplify }
                        if not is_char(left.resultdef) and
                           not is_widechar(left.resultdef) then
                          CGMessage(type_e_mismatch);
                      end;
                    pointerdef :
                      begin
                        if is_pchar(left.resultdef) then
                         begin
                            hp := ccallparanode.create(left,nil);
                            result := ccallnode.createintern('fpc_pchar_length',hp);
                            { make sure the left node doesn't get disposed, since it's }
                            { reused in the new node (JM)                              }
                            left:=nil;
                            exit;
                         end
                        else if is_pwidechar(left.resultdef) then
                         begin
                            hp := ccallparanode.create(left,nil);
                            result := ccallnode.createintern('fpc_pwidechar_length',hp);
                            { make sure the left node doesn't get disposed, since it's }
                            { reused in the new node (JM)                              }
                            left:=nil;
                            exit;
                         end
                        else
                         CGMessage(type_e_mismatch);
                      end;
                    arraydef :
                      begin
                        if is_open_array(left.resultdef) or
                           is_array_of_const(left.resultdef) then
                         begin
                           hightree:=load_high_value_node(tparavarsym(tloadnode(left).symtableentry));
                           if assigned(hightree) then
                             result:=caddnode.create(addn,hightree,
                               cordconstnode.create(1,sinttype,false));
                           exit;
                         end
                        else if is_dynamic_array(left.resultdef) then
                          begin
                            hp := ccallparanode.create(ctypeconvnode.create_internal(left,voidpointertype),nil);
                            result := ccallnode.createintern('fpc_dynarray_length',hp);
                            { make sure the left node doesn't get disposed, since it's }
                            { reused in the new node (JM)                              }
                            left:=nil;
                            exit;
                          end
                        else
                          begin
                            { will be handled in simplify }
                          end;
                      end
                    else
                      CGMessage(type_e_mismatch);
                  end;

                  { shortstring return an 8 bit value as the length
                    is the first byte of the string }
                  if is_shortstring(left.resultdef) then
                    resultdef:=u8inttype
                  else
                    resultdef:=sinttype;
                end;

              in_typeinfo_x:
                begin
                   set_varstate(left,vs_read,[vsf_must_be_valid]);
                   resultdef:=voidpointertype;
                end;

              in_assigned_x:
                begin
                  { the parser has already made sure the expression is valid }

                  { there could be a procvar, which is 2*sizeof(pointer), while we }
                  { must only check the first pointer -> can't just convert to an  }
                  { add node in all cases                                          }
                  set_varstate(tcallparanode(left).left,vs_read,[vsf_must_be_valid]);
                  resultdef:=booltype;
                end;

              in_ofs_x :
                internalerror(2000101001);

              in_seg_x :
                begin
                  set_varstate(left,vs_read,[]);
                  result:=cordconstnode.create(0,s32inttype,false);
                end;

              in_pred_x,
              in_succ_x:
                begin
                   set_varstate(left,vs_read,[vsf_must_be_valid]);
                   resultdef:=left.resultdef;
                   if not is_ordinal(resultdef) then
                     CGMessage(type_e_ordinal_expr_expected)
                   else
                     begin
                       if (resultdef.typ=enumdef) and
                          (tenumdef(resultdef).has_jumps) and
                          not(m_delphi in current_settings.modeswitches) then
                         CGMessage(type_e_succ_and_pred_enums_with_assign_not_possible);
                     end;
                end;

              in_initialize_x,
              in_finalize_x,
              in_setlength_x:
                begin
                  { inlined from pinline }
                  internalerror(200204231);
                end;

              in_inc_x,
              in_dec_x:
                begin
                  resultdef:=voidtype;
                  if assigned(left) then
                    begin
                       { first param must be var }
                       valid_for_var(tcallparanode(left).left,true);
                       set_varstate(tcallparanode(left).left,vs_readwritten,[vsf_must_be_valid]);

                       if (left.resultdef.typ in [enumdef,pointerdef]) or
                          is_ordinal(left.resultdef) or
                          is_currency(left.resultdef) then
                        begin
                          { value of left gets changed -> must be unique }
                          set_unique(tcallparanode(left).left);
                          { two paras ? }
                          if assigned(tcallparanode(left).right) then
                           begin
                             if is_integer(tcallparanode(left).right.resultdef) then
                               begin
                                 set_varstate(tcallparanode(tcallparanode(left).right).left,vs_read,[vsf_must_be_valid]);
                                 inserttypeconv_internal(tcallparanode(tcallparanode(left).right).left,tcallparanode(left).left.resultdef);
                                 if assigned(tcallparanode(tcallparanode(left).right).right) then
                                   { should be handled in the parser (JM) }
                                   internalerror(2006020901);
                               end
                             else
                               CGMessagePos(tcallparanode(left).right.fileinfo,type_e_ordinal_expr_expected);
                           end;
                        end
                       else
                        CGMessagePos(left.fileinfo,type_e_ordinal_expr_expected);
                    end
                  else
                    CGMessagePos(fileinfo,type_e_mismatch);
                end;

              in_read_x,
              in_readln_x,
              in_readstr_x,
              in_write_x,
              in_writeln_x,
              in_writestr_x :
                begin
                  result := handle_read_write;
                end;

              in_settextbuf_file_x :
                begin
                  resultdef:=voidtype;
                  { now we know the type of buffer }
                  hp:=ccallparanode.create(cordconstnode.create(
                     tcallparanode(left).left.resultdef.size,s32inttype,true),left);
                  result:=ccallnode.createintern('SETTEXTBUF',hp);
                  left:=nil;
                end;

              { the firstpass of the arg has been done in firstcalln ? }
              in_reset_typedfile,
              in_rewrite_typedfile :
                begin
                  result := handle_reset_rewrite_typed;
                end;

              in_str_x_string :
                begin
                  result:=handle_str;
                end;

              in_val_x :
                begin
                  result:=handle_val;
                end;

              in_include_x_y,
              in_exclude_x_y:
                begin
                  resultdef:=voidtype;
                  { the parser already checks whether we have two (and exactly two) }
                  { parameters (JM)                                                 }
                  { first param must be var }
                  valid_for_var(tcallparanode(left).left,true);
                  set_varstate(tcallparanode(left).left,vs_readwritten,[vsf_must_be_valid]);
                  { check type }
                  if (left.resultdef.typ=setdef) then
                    begin
                      { insert a type conversion       }
                      { to the type of the set elements  }
                      set_varstate(tcallparanode(tcallparanode(left).right).left,vs_read,[vsf_must_be_valid]);
                      inserttypeconv(tcallparanode(tcallparanode(left).right).left,
                        tsetdef(left.resultdef).elementdef);
                    end
                  else
                    CGMessage(type_e_mismatch);
                end;
              in_pack_x_y_z,
              in_unpack_x_y_z :
                begin
                  handle_pack_unpack;
                end;

              in_slice_x:
                begin
                  result:=nil;
                  resultdef:=tcallparanode(left).left.resultdef;
                  if (resultdef.typ <> arraydef) then
                    CGMessagePos(left.fileinfo,type_e_mismatch)
                  else if is_packed_array(resultdef) then
                    CGMessagePos2(left.fileinfo,type_e_got_expected_unpacked_array,'1',resultdef.typename);
                  if not(is_integer(tcallparanode(tcallparanode(left).right).left.resultdef)) then
                    CGMessagePos1(tcallparanode(left).right.fileinfo,
                      type_e_integer_expr_expected,
                      tcallparanode(tcallparanode(left).right).left.resultdef.typename);
                end;

              in_low_x,
              in_high_x:
                begin
                  case left.resultdef.typ of
                    orddef,
                    enumdef,
                    setdef:
                      ;
                    arraydef:
                      begin
                        if (inlinenumber=in_low_x) then
                          set_varstate(left,vs_read,[])
                        else
                         begin
                           if is_open_array(left.resultdef) or
                              is_array_of_const(left.resultdef) then
                            begin
                              set_varstate(left,vs_read,[]);
                              result:=load_high_value_node(tparavarsym(tloadnode(left).symtableentry));
                            end
                           else
                            if is_dynamic_array(left.resultdef) then
                              begin
                                set_varstate(left,vs_read,[vsf_must_be_valid]);
                                { can't use inserttypeconv because we need }
                                { an explicit type conversion (JM)         }
                                hp := ccallparanode.create(ctypeconvnode.create_internal(left,voidpointertype),nil);
                                result := ccallnode.createintern('fpc_dynarray_high',hp);
                                { make sure the left node doesn't get disposed, since it's }
                                { reused in the new node (JM)                              }
                                left:=nil;
                              end
                           else
                            begin
                              set_varstate(left,vs_read,[]);
                            end;
                         end;
                      end;
                    stringdef:
                      begin
                        if inlinenumber=in_low_x then
                         begin
                           set_varstate(left,vs_read,[]);
                         end
                        else
                         begin
                           if is_open_string(left.resultdef) then
                            begin
                              set_varstate(left,vs_read,[]);
                              result:=load_high_value_node(tparavarsym(tloadnode(left).symtableentry))
                            end
                           else if is_ansistring(left.resultdef) or
                                   is_widestring(left.resultdef) then
                             CGMessage(type_e_mismatch)
                         end;
                     end;
                    else
                      CGMessage(type_e_mismatch);
                  end;
                end;

              in_exp_real,
              in_frac_real,
              in_int_real,
              in_cos_real,
              in_sin_real,
              in_arctan_real,
              in_abs_real,
              in_ln_real :
                begin
                  set_varstate(left,vs_read,[vsf_must_be_valid]);
                  { converting an int64 to double on platforms without }
                  { extended can cause precision loss                  }
                  if not(left.nodetype in [ordconstn,realconstn]) then
                    inserttypeconv(left,pbestrealtype^);
                  resultdef:=pbestrealtype^;
                end;

              in_trunc_real,
              in_round_real :
                begin
                  set_varstate(left,vs_read,[vsf_must_be_valid]);
                  { for direct float rounding, no best real type cast should be necessary }
                  if not((left.resultdef.typ=floatdef) and
                         (tfloatdef(left.resultdef).floattype in [s32real,s64real,s80real,s128real])) and
                     { converting an int64 to double on platforms without }
                     { extended can cause precision loss                  }
                     not(left.nodetype in [ordconstn,realconstn]) then
                    inserttypeconv(left,pbestrealtype^);
                  resultdef:=s64inttype;
                end;

              in_pi_real :
                begin
                  resultdef:=pbestrealtype^;
                end;

              in_abs_long:
                begin
                  set_varstate(left,vs_read,[vsf_must_be_valid]);
                  inserttypeconv(left,s32inttype);
                  resultdef:=s32inttype;
                end;

              in_sqr_real,
              in_sqrt_real :
                begin
                  set_varstate(left,vs_read,[vsf_must_be_valid]);
                  setfloatresultdef;
                end;

{$ifdef SUPPORT_MMX}
              in_mmx_pcmpeqb..in_mmx_pcmpgtw:
                begin
                end;
{$endif SUPPORT_MMX}
{$ifdef SUPPORT_UNALIGNED}
              in_unaligned_x:
                begin
                  resultdef:=left.resultdef;
                end;
{$endif SUPPORT_UNALIGNED}
              in_assert_x_y :
                begin
                  resultdef:=voidtype;
                  if assigned(left) then
                    begin
                      set_varstate(tcallparanode(left).left,vs_read,[vsf_must_be_valid]);
                      { check type }
                      if is_boolean(left.resultdef) then
                        begin
                           set_varstate(tcallparanode(tcallparanode(left).right).left,vs_read,[vsf_must_be_valid]);
                           { must always be a string }
                           inserttypeconv(tcallparanode(tcallparanode(left).right).left,cshortstringtype);
                         end
                       else
                         CGMessage1(type_e_boolean_expr_expected,left.resultdef.typename);
                    end
                  else
                    CGMessage(type_e_mismatch);

                  if (cs_do_assertion in current_settings.localswitches) then
                    include(current_procinfo.flags,pi_do_call);
                end;
              in_prefetch_var,
              in_get_frame,
              in_get_caller_frame,
              in_get_caller_addr:
                begin
                  resultdef:=voidpointertype;
                end;
               else
                internalerror(8);
            end;
          end;

        if not assigned(result) and not
           codegenerror then
          result:=simplify;
      end;


    function tinlinenode.pass_1 : tnode;
      var
         hp,hpp,resultnode  : tnode;
         shiftconst: longint;
         tempnode: ttempcreatenode;
         newstatement: tstatementnode;
         newblock: tblocknode;

      begin
         result:=nil;
         { if we handle writeln; left contains no valid address }
         if assigned(left) then
           begin
              if left.nodetype=callparan then
                tcallparanode(left).firstcallparan
              else
                firstpass(left);
           end;

         { intern const should already be handled }
         if nf_inlineconst in flags then
          internalerror(200104044);
         case inlinenumber of
          in_lo_qword,
          in_hi_qword,
          in_lo_long,
          in_hi_long,
          in_lo_word,
          in_hi_word:
            begin
              shiftconst := 0;
              case inlinenumber of
                in_hi_qword:
                  shiftconst := 32;
                in_hi_long:
                  shiftconst := 16;
                in_hi_word:
                  shiftconst := 8;
              end;
              if shiftconst <> 0 then
                result := ctypeconvnode.create_internal(cshlshrnode.create(shrn,left,
                    cordconstnode.create(shiftconst,u32inttype,false)),resultdef)
              else
                result := ctypeconvnode.create_internal(left,resultdef);
              left := nil;
              firstpass(result);
            end;

          in_sizeof_x:
            begin
              expectloc:=LOC_REGISTER;
            end;

          in_typeof_x:
            begin
               expectloc:=LOC_REGISTER;
            end;

          in_length_x:
            begin
               if is_shortstring(left.resultdef) then
                expectloc:=left.expectloc
               else
                begin
                  { ansi/wide string }
                  expectloc:=LOC_REGISTER;
                end;
            end;

          in_typeinfo_x:
            begin
               expectloc:=LOC_REGISTER;
            end;

          in_assigned_x:
            begin
              expectloc := LOC_JUMP;
            end;

          in_pred_x,
          in_succ_x:
            begin
              expectloc:=LOC_REGISTER;
            end;

          in_setlength_x,
          in_initialize_x,
          in_finalize_x:
            begin
              expectloc:=LOC_VOID;
            end;

          in_inc_x,
          in_dec_x:
            begin
               expectloc:=LOC_VOID;

               { range/overflow checking doesn't work properly }
               { with the inc/dec code that's generated (JM)   }
               if (current_settings.localswitches * [cs_check_overflow,cs_check_range] <> []) and
                 { No overflow check for pointer operations, because inc(pointer,-1) will always
                   trigger an overflow. For uint32 it works because then the operation is done
                   in 64bit. Range checking is not applicable to pointers either }
                  (tcallparanode(left).left.resultdef.typ<>pointerdef) then
                 { convert to simple add (JM) }
                 begin
                   newblock := internalstatements(newstatement);
                   { extra parameter? }
                   if assigned(tcallparanode(left).right) then
                     begin
                       { Yes, use for add node }
                       hpp := tcallparanode(tcallparanode(left).right).left;
                       tcallparanode(tcallparanode(left).right).left := nil;
                       if assigned(tcallparanode(tcallparanode(left).right).right) then
                         CGMessage(parser_e_illegal_expression);
                     end
                   else
                     begin
                       { no, create constant 1 }
                       hpp := cordconstnode.create(1,tcallparanode(left).left.resultdef,false);
                     end;
                   typecheckpass(hpp);

                   if not((hpp.resultdef.typ=orddef) and
{$ifndef cpu64bit}
                          (torddef(hpp.resultdef).ordtype<>u32bit)) then
{$else not cpu64bit}
                          (torddef(hpp.resultdef).ordtype<>u64bit)) then
{$endif not cpu64bit}
                     inserttypeconv_internal(hpp,sinttype);
                   { make sure we don't call functions part of the left node twice (and generally }
                   { optimize the code generation)                                                }
                   if node_complexity(tcallparanode(left).left) > 1 then
                     begin
                       tempnode := ctempcreatenode.create(voidpointertype,voidpointertype.size,tt_persistent,true);
                       addstatement(newstatement,tempnode);
                       addstatement(newstatement,cassignmentnode.create(ctemprefnode.create(tempnode),
                         caddrnode.create_internal(tcallparanode(left).left.getcopy)));
                       hp := cderefnode.create(ctemprefnode.create(tempnode));
                       inserttypeconv_internal(hp,tcallparanode(left).left.resultdef);
                     end
                   else
                     begin
                       hp := tcallparanode(left).left.getcopy;
                       tempnode := nil;
                     end;

                   resultnode := hp.getcopy;
                   { avoid type errors from the addn/subn }
                   if not is_integer(resultnode.resultdef) then
                     begin
                       inserttypeconv_internal(hp,sinttype);
                       inserttypeconv_internal(hpp,sinttype);
                     end;

                   { addition/substraction depending on inc/dec }
                   if inlinenumber = in_inc_x then
                     hpp := caddnode.create(addn,hp,hpp)
                   else
                     hpp := caddnode.create(subn,hp,hpp);
                   { assign result of addition }
                   if not(is_integer(resultnode.resultdef)) then
                     inserttypeconv(hpp,torddef.create(
{$ifdef cpu64bit}
                       s64bit,
{$else cpu64bit}
                       s32bit,
{$endif cpu64bit}
                       get_min_value(resultnode.resultdef),
                       get_max_value(resultnode.resultdef)))
                   else
                     inserttypeconv(hpp,resultnode.resultdef);
                   { avoid any possible warnings }
                   inserttypeconv_internal(hpp,resultnode.resultdef);

                   addstatement(newstatement,cassignmentnode.create(resultnode,hpp));
                   { deallocate the temp }
                   if assigned(tempnode) then
                     addstatement(newstatement,ctempdeletenode.create(tempnode));
                   { firstpass it }
                   firstpass(newblock);
                   { return new node }
                   result := newblock;
                 end;
            end;

         in_include_x_y,
         in_exclude_x_y:
           begin
              expectloc:=LOC_VOID;
           end;

         in_pack_x_y_z,
         in_unpack_x_y_z:
           begin
             result:=first_pack_unpack;
           end;

         in_exp_real:
           begin
             result:= first_exp_real;
           end;

         in_round_real:
           begin
             result:= first_round_real;
           end;

         in_trunc_real:
           begin
             result:= first_trunc_real;
           end;

         in_int_real:
           begin
             result:= first_int_real;
           end;

         in_frac_real:
           begin
             result:= first_frac_real;
           end;

         in_cos_real:
           begin
             result:= first_cos_real;
           end;

         in_sin_real:
           begin
             result := first_sin_real;
           end;

         in_arctan_real:
           begin
             result := first_arctan_real;
           end;

         in_pi_real :
           begin
             result := first_pi;
           end;

         in_abs_real:
           begin
             result := first_abs_real;
           end;

         in_abs_long:
           begin
             result := first_abs_long;
           end;

         in_sqr_real:
           begin
             result := first_sqr_real;
           end;

         in_sqrt_real:
           begin
             result := first_sqrt_real;
           end;

         in_ln_real:
           begin
             result := first_ln_real;
           end;

{$ifdef SUPPORT_MMX}
         in_mmx_pcmpeqb..in_mmx_pcmpgtw:
           begin
           end;
{$endif SUPPORT_MMX}

         in_assert_x_y :
            begin
              expectloc:=LOC_VOID;
            end;

          in_low_x,
          in_high_x:
            internalerror(200104047);

          in_slice_x:
            internalerror(2005101501);

          in_ord_x,
          in_chr_byte:
            begin
               { should not happend as it's converted to typeconv }
               internalerror(200104045);
            end;

          in_ofs_x :
            internalerror(2000101001);

          in_seg_x :
            internalerror(200104046);

          in_settextbuf_file_x,
          in_reset_typedfile,
          in_rewrite_typedfile,
          in_str_x_string,
          in_val_x,
          in_read_x,
          in_readln_x,
          in_write_x,
          in_writeln_x :
            begin
              { should be handled by pass_typecheck }
              internalerror(200108234);
            end;
         in_get_frame:
            begin
              include(current_procinfo.flags,pi_needs_stackframe);
              expectloc:=LOC_CREGISTER;
            end;
         in_get_caller_frame:
            begin
              expectloc:=LOC_REGISTER;
            end;
         in_get_caller_addr:
            begin
              expectloc:=LOC_REGISTER;
            end;

         in_prefetch_var:
           begin
             expectloc:=LOC_VOID;
           end;
{$ifdef SUPPORT_UNALIGNED}
         in_unaligned_x:
           begin
             expectloc:=tcallparanode(left).left.expectloc;
           end;
{$endif SUPPORT_UNALIGNED}
          else
            internalerror(89);
          end;
       end;
{$maxfpuregisters default}

    function tinlinenode.docompare(p: tnode): boolean;
      begin
        docompare :=
          inherited docompare(p) and
          (inlinenumber = tinlinenode(p).inlinenumber);
      end;


    function tinlinenode.first_pi : tnode;
      begin
        result:=crealconstnode.create(getpi,pbestrealtype^);
      end;


     function tinlinenode.first_arctan_real : tnode;
      begin
        { create the call to the helper }
        { on entry left node contains the parameter }
        first_arctan_real := ccallnode.createintern('fpc_arctan_real',
                ccallparanode.create(left,nil));
        left := nil;
      end;

     function tinlinenode.first_abs_real : tnode;
      begin
        { create the call to the helper }
        { on entry left node contains the parameter }
        first_abs_real := ccallnode.createintern('fpc_abs_real',
                ccallparanode.create(left,nil));
        left := nil;
      end;

     function tinlinenode.first_sqr_real : tnode;
      begin
        { create the call to the helper }
        { on entry left node contains the parameter }
        first_sqr_real := ctypeconvnode.create(ccallnode.createintern('fpc_sqr_real',
                ccallparanode.create(left,nil)),resultdef);
        left := nil;
      end;

     function tinlinenode.first_sqrt_real : tnode;
      begin
        { create the call to the helper }
        { on entry left node contains the parameter }
        first_sqrt_real := ctypeconvnode.create(ccallnode.createintern('fpc_sqrt_real',
                ccallparanode.create(left,nil)),resultdef);
        left := nil;
      end;

     function tinlinenode.first_ln_real : tnode;
      begin
        { create the call to the helper }
        { on entry left node contains the parameter }
        first_ln_real := ccallnode.createintern('fpc_ln_real',
                ccallparanode.create(left,nil));
        left := nil;
      end;

     function tinlinenode.first_cos_real : tnode;
      begin
        { create the call to the helper }
        { on entry left node contains the parameter }
        first_cos_real := ccallnode.createintern('fpc_cos_real',
                ccallparanode.create(left,nil));
        left := nil;
      end;

     function tinlinenode.first_sin_real : tnode;
      begin
        { create the call to the helper }
        { on entry left node contains the parameter }
        first_sin_real := ccallnode.createintern('fpc_sin_real',
                ccallparanode.create(left,nil));
        left := nil;
      end;

     function tinlinenode.first_exp_real : tnode;
      begin
        { create the call to the helper }
        { on entry left node contains the parameter }
        result := ccallnode.createintern('fpc_exp_real',ccallparanode.create(left,nil));
        left := nil;
      end;

     function tinlinenode.first_int_real : tnode;
      begin
        { create the call to the helper }
        { on entry left node contains the parameter }
        result := ccallnode.createintern('fpc_int_real',ccallparanode.create(left,nil));
        left := nil;
      end;

     function tinlinenode.first_frac_real : tnode;
      begin
        { create the call to the helper }
        { on entry left node contains the parameter }
        result := ccallnode.createintern('fpc_frac_real',ccallparanode.create(left,nil));
        left := nil;
      end;

     function tinlinenode.first_round_real : tnode;
      begin
        { create the call to the helper }
        { on entry left node contains the parameter }
        result := ccallnode.createintern('fpc_round_real',ccallparanode.create(left,nil));
        left := nil;
      end;

     function tinlinenode.first_trunc_real : tnode;
      begin
        { create the call to the helper }
        { on entry left node contains the parameter }
        result := ccallnode.createintern('fpc_trunc_real',ccallparanode.create(left,nil));
        left := nil;
      end;

     function tinlinenode.first_abs_long : tnode;
      begin
        result:=nil;
      end;

     function tinlinenode.first_pack_unpack: tnode;
       var
         loopstatement    : tstatementnode;
         loop             : tblocknode;
         loopvar          : ttempcreatenode;
         tempnode,
         source,
         target,
         index,
         unpackednode,
         packednode,
         sourcevecindex,
         targetvecindex,
         loopbody         : tnode;
         temprangedef     : tdef;
         ulorange,
         uhirange,
         plorange,
         phirange          : TConstExprInt;
       begin
         { transform into a for loop which assigns the data of the (un)packed }
         { array to the other one                                             }
         source := left;
         if (inlinenumber = in_unpack_x_y_z) then
           begin
             target := tcallparanode(source).right;
             index := tcallparanode(target).right;
             packednode := tcallparanode(source).left;
             unpackednode := tcallparanode(target).left;
           end
         else
           begin
             index := tcallparanode(source).right;
             target := tcallparanode(index).right;
             packednode := tcallparanode(target).left;
             unpackednode := tcallparanode(source).left;
           end;
         source := tcallparanode(source).left;
         target := tcallparanode(target).left;
         index := tcallparanode(index).left;

         loop := internalstatements(loopstatement);
         loopvar := ctempcreatenode.create(
           tarraydef(packednode.resultdef).rangedef,
           tarraydef(packednode.resultdef).rangedef.size,
           tt_persistent,true);
         addstatement(loopstatement,loopvar);

         { For range checking: we have to convert to an integer type (in case the index type }
         { is an enum), add the index and loop variable together, convert the result         }
         { implicitly to an orddef with range equal to the rangedef to get range checking   }
         { and finally convert it explicitly back to the actual rangedef to avoid type      }
         { errors                                                                            }
         temprangedef:=nil;
         getrange(unpackednode.resultdef,ulorange,uhirange);
         getrange(packednode.resultdef,plorange,phirange);
         temprangedef:=torddef.create(torddef(sinttype).ordtype,ulorange,uhirange);
         sourcevecindex := ctemprefnode.create(loopvar);
         targetvecindex := ctypeconvnode.create_internal(index.getcopy,sinttype);
         targetvecindex := caddnode.create(subn,targetvecindex,cordconstnode.create(plorange,sinttype,true));
         targetvecindex := caddnode.create(addn,targetvecindex,ctemprefnode.create(loopvar));
         targetvecindex := ctypeconvnode.create(targetvecindex,temprangedef);
         targetvecindex := ctypeconvnode.create_explicit(targetvecindex,tarraydef(unpackednode.resultdef).rangedef);

         if (inlinenumber = in_pack_x_y_z) then
           begin
             { swap source and target vec indices }
             tempnode := sourcevecindex;
             sourcevecindex := targetvecindex;
             targetvecindex := tempnode;
           end;

         { create the assignment in the loop body }
         loopbody :=
           cassignmentnode.create(
             cvecnode.create(target.getcopy,targetvecindex),
             cvecnode.create(source.getcopy,sourcevecindex)
           );
         { create the actual for loop }
         tempnode := cfornode.create(
           ctemprefnode.create(loopvar),
           cinlinenode.create(in_low_x,false,packednode.getcopy),
           cinlinenode.create(in_high_x,false,packednode.getcopy),
           loopbody,
           false);
         addstatement(loopstatement,tempnode);
         { free the loop counter }
         addstatement(loopstatement,ctempdeletenode.create(loopvar));
         result := loop;
       end;

begin
   cinlinenode:=tinlinenode;
end.
