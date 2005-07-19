{
    Copyright (c) 1998-2002 by Florian Klaempfl

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
          function getcopy : tnode;override;
          function pass_1 : tnode;override;
          function det_resulttype:tnode;override;
          function docompare(p: tnode): boolean; override;
          { All the following routines currently
            call compilerproc's, unless they are
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
        private
          function handle_str: tnode;
          function handle_reset_rewrite_typed: tnode;
          function handle_read_write: tnode;
          function handle_val: tnode;
       end;
       tinlinenodeclass = class of tinlinenode;

    var
       cinlinenode : tinlinenodeclass;

   function geninlinenode(number : byte;is_const:boolean;l : tnode) : tinlinenode;

implementation

    uses
      verbose,globals,systems,
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


    function tinlinenode.getcopy : tnode;
      var
         n : tinlinenode;
      begin
         n:=tinlinenode(inherited getcopy);
         n.inlinenumber:=inlinenumber;
         result:=n;
      end;


      function tinlinenode.handle_str : tnode;
      var
        lenpara,
        fracpara,
        newparas,
        dest,
        source  : tcallparanode;
        procname: string;
        is_real : boolean;

      begin
        result := cerrornode.create;

        { make sure we got at least two parameters (if we got only one, }
        { this parameter may not be encapsulated in a callparan)        }
        if not assigned(left) or
           (left.nodetype <> callparan) then
          begin
            CGMessage(parser_e_wrong_parameter_size);
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
            CGMessage(parser_e_wrong_parameter_size);
            exit;
          end;

        is_real := source.resulttype.def.deftype = floatdef;

        if ((dest.left.resulttype.def.deftype<>stringdef) and
            not(is_chararray(dest.left.resulttype.def))) or
           not(is_real or
               (source.left.resulttype.def.deftype = orddef)) then
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
            if not is_integer(lenpara.resulttype.def) then
              begin
                CGMessagePos1(lenpara.fileinfo,
                  type_e_integer_expr_expected,lenpara.resulttype.def.typename);
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
                if not is_integer(lenpara.resulttype.def) then
                  begin
                    CGMessagePos1(lenpara.fileinfo,
                      type_e_integer_expr_expected,lenpara.resulttype.def.typename);
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
            newparas.right := ccallparanode.create(cordconstnode.create(
              ord(tfloatdef(source.left.resulttype.def).typ),s32inttype,true),
               newparas.right);
            { if necessary, insert a fraction parameter }
            if not assigned(fracpara) then
              begin
                tcallparanode(newparas.right).right := ccallparanode.create(
                  cordconstnode.create(-1,s32inttype,false),
                   tcallparanode(newparas.right).right);
                fracpara := tcallparanode(tcallparanode(newparas.right).right);
              end;
            { if necessary, insert a length para }
            if not assigned(lenpara) then
              fracpara.right := ccallparanode.create(
                cordconstnode.create(-32767,s32inttype,false),
                   fracpara.right);
          end
        else
          { for a normal parameter, insert a only length parameter if one is missing }
          if not assigned(lenpara) then
            newparas.right := ccallparanode.create(cordconstnode.create(-1,s32inttype,false),
              newparas.right);

        { remove the parameters from the original node so they won't get disposed, }
        { since they're reused                                                     }
        left := nil;

        { create procedure name }
        if is_chararray(dest.resulttype.def) then
          procname:='fpc_chararray_'
        else
          procname := 'fpc_' + tstringdef(dest.resulttype.def).stringtypname+'_';
        if is_real then
          procname := procname + 'float'
        else
          case torddef(source.resulttype.def).typ of
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
          tfiledef(left.resulttype.def).typedfiletype.def.size,s32inttype,true),
          ccallparanode.create(left,nil));
        { create the correct call }
        if inlinenumber=in_reset_typedfile then
          result := ccallnode.createintern('fpc_reset_typed',left)
        else
          result := ccallnode.createintern('fpc_rewrite_typed',left);
        { make sure left doesn't get disposed, since we use it in the new call }
        left := nil;
      end;


    function tinlinenode.handle_read_write: tnode;

      const
        procnames: array[boolean,boolean] of string[11] =
          (('write_text_','read_text_'),('typed_write','typed_read'));

      var
        filepara,
        lenpara,
        fracpara,
        nextpara,
        para          : tcallparanode;
        newstatement  : tstatementnode;
        newblock      : tblocknode;
        p1            : tnode;
        filetemp,
        temp          : ttempcreatenode;
        procprefix,
        name          : string[31];
        textsym       : ttypesym;
        readfunctype  : ttype;
        is_typed,
        do_read,
        is_real,
        error_para,
        found_error   : boolean;
      begin
        filepara := nil;
        is_typed := false;
        filetemp := nil;
        do_read := inlinenumber in [in_read_x,in_readln_x];
        { if we fail, we can quickly exit this way. We must generate something }
        { instead of the inline node, because firstpass will bomb with an      }
        { internalerror if it encounters a read/write                          }
        result := cerrornode.create;

        { reverse the parameters (needed to get the colon parameters in the }
        { correct order when processing write(ln)                           }
        left := reverseparameters(tcallparanode(left));

        if assigned(left) then
          begin
            { check if we have a file parameter and if yes, what kind it is }
            filepara := tcallparanode(left);

            if (filepara.resulttype.def.deftype=filedef) then
              begin
                if (tfiledef(filepara.resulttype.def).filetyp=ft_untyped) then
                  begin
                    CGMessagePos(fileinfo,type_e_no_read_write_for_untyped_file);
                    exit;
                  end
                else
                  begin
                    if (tfiledef(filepara.resulttype.def).filetyp=ft_typed) then
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
        if not assigned(filepara) then
          begin
            { since the input/output variables are threadvars loading them into
              a temp once is faster. Create a temp which will hold a pointer to the file }
            filetemp := ctempcreatenode.create(voidpointertype,voidpointertype.def.size,tt_persistent,true);
            addstatement(newstatement,filetemp);

            { make sure the resulttype of the temp (and as such of the }
            { temprefs coming after it) is set (necessary because the  }
            { temprefs will be part of the filepara, of which we need  }
            { the resulttype later on and temprefs can only be         }
            { resulttypepassed if the resulttype of the temp is known) }
            resulttypepass(tnode(filetemp));

            { assign the address of the file to the temp }
            if do_read then
              name := 'input'
            else
              name := 'output';
            addstatement(newstatement,
              cassignmentnode.create(ctemprefnode.create(filetemp),
                ccallnode.createintern('fpc_get_'+name,nil)));

            { create a new fileparameter as follows: file_type(temp^)    }
            { (so that we pass the value and not the address of the temp }
            { to the read/write routine)                                 }
            if not searchsystype('TEXT',textsym) then
              internalerror(200108313);
            filepara := ccallparanode.create(ctypeconvnode.create_internal(
              cderefnode.create(ctemprefnode.create(filetemp)),textsym.restype),nil);
          end
        else
          { remove filepara from the parameter chain }
          begin
            left := filepara.right;
            filepara.right := nil;
            { the file para is a var parameter, but it must be valid already }
            set_varstate(filepara.left,vs_used,[vsf_must_be_valid]);
            { check if we should make a temp to store the result of a complex }
            { expression (better heuristics, anyone?) (JM)                    }
            if (filepara.left.nodetype <> loadn) then
              begin
                { create a temp which will hold a pointer to the file }
                filetemp := ctempcreatenode.create(voidpointertype,voidpointertype.def.size,tt_persistent,true);

                { add it to the statements }
                addstatement(newstatement,filetemp);

                { make sure the resulttype of the temp (and as such of the }
                { temprefs coming after it) is set (necessary because the  }
                { temprefs will be part of the filepara, of which we need  }
                { the resulttype later on and temprefs can only be         }
                { resulttypepassed if the resulttype of the temp is known) }
                resulttypepass(tnode(filetemp));

                { assign the address of the file to the temp }
                addstatement(newstatement,
                  cassignmentnode.create(ctemprefnode.create(filetemp),
                    caddrnode.create_internal(filepara.left)));
                resulttypepass(newstatement.left);
                { create a new fileparameter as follows: file_type(temp^)    }
                { (so that we pass the value and not the address of the temp }
                { to the read/write routine)                                 }
                nextpara := ccallparanode.create(ctypeconvnode.create_internal(
                  cderefnode.create(ctemprefnode.create(filetemp)),filepara.left.resulttype),nil);

                { replace the old file para with the new one }
                filepara.left := nil;
                filepara.free;
                filepara := nextpara;
              end;
          end;

        { the resulttype of the filepara must be set since it's }
        { used below                                            }
        filepara.get_paratype;

        { now, filepara is nowhere referenced anymore, so we can safely dispose it }
        { if something goes wrong or at the end of the procedure                   }

        { choose the correct procedure prefix }
        procprefix := 'fpc_'+procnames[is_typed,do_read];

        { we're going to reuse the paranodes, so make sure they don't get freed }
        { twice                                                                 }
        para := tcallparanode(left);
        left := nil;

        { no errors found yet... }
        found_error := false;

        if is_typed then
          begin
            { add the typesize to the filepara }
            if filepara.resulttype.def.deftype=filedef then
              filepara.right := ccallparanode.create(cordconstnode.create(
                tfiledef(filepara.resulttype.def).typedfiletype.def.size,s32inttype,true),nil);

            { check for "no parameters" (you need at least one extra para for typed files) }
            if not assigned(para) then
              begin
                CGMessage(parser_e_wrong_parameter_size);
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
                if (para.left.resulttype.def.deftype=procvardef) then
                  begin
                    p1:=ccallnode.create_procvar(nil,para.left);
                    resulttypepass(p1);
                    para.left:=p1;
                  end;

                if filepara.resulttype.def.deftype=filedef then
                  inserttypeconv(para.left,tfiledef(filepara.resulttype.def).typedfiletype);

                if assigned(para.right) and
                   (cpf_is_colon_para in tcallparanode(para.right).callparaflags) then
                  begin
                    CGMessagePos(para.right.fileinfo,parser_e_illegal_colon_qualifier);

                    { skip all colon para's }
                    nextpara := tcallparanode(tcallparanode(para.right).right);
                    while assigned(nextpara) and
                          (cpf_is_colon_para in nextpara.callparaflags) do
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
                if not(do_read) and
                   (para.left.nodetype <> loadn) then
                  begin
                    { create temp for result }
                    temp := ctempcreatenode.create(para.left.resulttype,
                      para.left.resulttype.def.size,tt_persistent,false);
                    addstatement(newstatement,temp);
                    { assign result to temp }
                    addstatement(newstatement,
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
                addstatement(newstatement,ccallnode.createintern(procprefix,para));

                { if we used a temp, free it }
                if para.left.nodetype = temprefn then
                  addstatement(newstatement,ctempdeletenode.create(temp));

                { process next parameter }
                para := nextpara;
              end;

            { free the file parameter }
            filepara.free;
          end
        else
          { text read/write }
          begin
            while assigned(para) do
              begin
                { is this parameter faulty? }
                error_para := false;
                { is this parameter a real? }
                is_real:=false;
                { type used for the read(), this is used to check
                  whether a temp is needed for range checking }
                readfunctype.reset;

                { can't read/write types }
                if para.left.nodetype=typen then
                  begin
                    CGMessagePos(para.fileinfo,type_e_cant_read_write_type);
                    error_para := true;
                  end;

                { support writeln(procvar) }
                if (para.left.resulttype.def.deftype=procvardef) then
                  begin
                    p1:=ccallnode.create_procvar(nil,para.left);
                    resulttypepass(p1);
                    para.left:=p1;
                  end;

                { Currency will be written using the bestreal }
                if is_currency(para.left.resulttype.def) then
                  inserttypeconv(para.left,pbestrealtype^);

                case para.left.resulttype.def.deftype of
                  stringdef :
                    begin
                      name := procprefix+tstringdef(para.left.resulttype.def).stringtypname;
                    end;
                  pointerdef :
                    begin
                      if not is_pchar(para.left.resulttype.def) then
                        begin
                          CGMessagePos(para.fileinfo,type_e_cant_read_write_type);
                          error_para := true;
                        end
                      else
                        name := procprefix+'pchar_as_pointer';
                    end;
                  floatdef :
                    begin
                      is_real:=true;
                      name := procprefix+'float';
                      readfunctype:=pbestrealtype^;
                    end;
                  orddef :
                    begin
                      case torddef(para.left.resulttype.def).typ of
{$ifdef cpu64bit}
                        s64bit,
{$endif cpu64bit}
                        s8bit,
                        s16bit,
                        s32bit :
                          begin
                            name := procprefix+'sint';
                            readfunctype:=sinttype;
                          end;
{$ifdef cpu64bit}
                        u64bit,
{$endif cpu64bit}
                        u8bit,
                        u16bit,
                        u32bit :
                          begin
                            name := procprefix+'uint';
                            readfunctype:=uinttype;
                          end;
                        uchar :
                          begin
                            name := procprefix+'char';
                            readfunctype:=cchartype;
                          end;
                        uwidechar :
                          begin
                            name := procprefix+'widechar';
                            readfunctype:=cwidechartype;
                          end;
{$ifndef cpu64bit}
                        s64bit :
                          begin
                            name := procprefix+'int64';
                            readfunctype:=s64inttype;
                          end;
                        u64bit :
                          begin
                            name := procprefix+'qword';
                            readfunctype:=u64inttype;
                          end;
{$endif cpu64bit}
                        bool8bit,
                        bool16bit,
                        bool32bit :
                          begin
                            if do_read then
                              begin
                                CGMessagePos(para.fileinfo,type_e_cant_read_write_type);
                                error_para := true;
                              end
                            else
                              begin
                                name := procprefix+'boolean';
                                readfunctype:=booltype;
                              end;
                          end
                        else
                          begin
                            CGMessagePos(para.fileinfo,type_e_cant_read_write_type);
                            error_para := true;
                          end;
                      end;
                    end;
                  variantdef :
                    name:=procprefix+'variant';
                  arraydef :
                    begin
                      if is_chararray(para.left.resulttype.def) then
                        name := procprefix+'pchar_as_array'
                      else
                        begin
                          CGMessagePos(para.fileinfo,type_e_cant_read_write_type);
                          error_para := true;
                        end
                    end
                  else
                    begin
                      CGMessagePos(para.fileinfo,type_e_cant_read_write_type);
                      error_para := true;
                    end
                end;

                { check for length/fractional colon para's }
                fracpara := nil;
                lenpara := nil;
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
                    { create dummy frac/len para's if necessary }
                    if not do_read then
                      begin
                        { difference in default value for floats and the rest :( }
                        if not is_real then
                          begin
                            if not assigned(lenpara) then
                              lenpara := ccallparanode.create(
                                cordconstnode.create(0,sinttype,false),nil)
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
                                cordconstnode.create(-32767,sinttype,false),nil);
                            { also create a default fracpara if necessary }
                            if not assigned(fracpara) then
                              fracpara := ccallparanode.create(
                                cordconstnode.create(-1,sinttype,false),nil);
                            { add it to the lenpara }
                            lenpara.right := fracpara;
                            { and add the realtype para (this also removes the link }
                            { to any parameters coming after it)                    }
                            fracpara.right := ccallparanode.create(
                                cordconstnode.create(ord(tfloatdef(para.left.resulttype.def).typ),
                                sinttype,true),nil);
                          end;
                      end;

                    { special handling of reading small numbers, because the helpers  }
                    { expect a longint/card/bestreal var parameter. Use a temp. can't }
                    { use functions because then the call to FPC_IOCHECK destroys     }
                    { their result before we can store it                             }
                    if do_read and
                       assigned(readfunctype.def) and
                       (para.left.resulttype.def<>readfunctype.def) then
                      begin
                        { create the parameter list: the temp ... }
                        temp := ctempcreatenode.create(readfunctype,readfunctype.def.size,tt_persistent,false);
                        addstatement(newstatement,temp);

                        { ... and the file }
                        p1 := ccallparanode.create(ctemprefnode.create(temp),
                          filepara.getcopy);

                        { create the call to the helper }
                        addstatement(newstatement,
                          ccallnode.createintern(name,tcallparanode(p1)));

                        { assign the result to the original var (this automatically }
                        { takes care of range checking)                             }
                        addstatement(newstatement,
                          cassignmentnode.create(para.left,
                            ctemprefnode.create(temp)));

                        { release the temp location }
                        addstatement(newstatement,ctempdeletenode.create(temp));

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
                        { add the lenpara (fracpara and realtype are already linked }
                        { with it if necessary)                                     }
                        tcallparanode(para.right).right := lenpara;
                        { create the call statement }
                        addstatement(newstatement,
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
                  in_read_x:
                    name:='fpc_read_end';
                  in_write_x:
                    name:='fpc_write_end';
                  in_readln_x:
                    name:='fpc_readln_end';
                  in_writeln_x:
                    name:='fpc_writeln_end';
                end;
                addstatement(newstatement,ccallnode.createintern(name,filepara));
              end;
          end;

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
        orgcode       : tnode;
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
           CGMessage(parser_e_wrong_parameter_size);
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
            (codepara.resulttype.def.deftype <> orddef)
{$ifndef cpu64bit}
            or is_64bitint(codepara.resulttype.def)
{$endif cpu64bit}
            ) then
          begin
            CGMessagePos1(codepara.fileinfo,type_e_integer_expr_expected,codepara.resulttype.def.typename);
            exit;
          end;

        { check if dest para is valid }
        if not(destpara.resulttype.def.deftype in [orddef,floatdef]) then
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
           (codepara.resulttype.def.size<>sinttype.def.size) then
          begin
            tempcode := ctempcreatenode.create(sinttype,sinttype.def.size,tt_persistent,false);
            addstatement(newstatement,tempcode);
            { set the resulttype of the temp (needed to be able to get }
            { the resulttype of the tempref used in the new code para) }
            resulttypepass(tnode(tempcode));
            { create a temp codepara, but save the original code para to }
            { assign the result to later on                              }
            if assigned(codepara) then
              begin
                orgcode := codepara.left;
                codepara.left := ctemprefnode.create(tempcode);
              end
            else
              codepara := ccallparanode.create(ctemprefnode.create(tempcode),nil);
            { we need its resulttype later on }
            codepara.get_paratype;
          end
        else if (torddef(codepara.resulttype.def).typ = torddef(sinttype.def).typ) then
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

        case destpara.resulttype.def.deftype of
          orddef:
            begin
              case torddef(destpara.resulttype.def).typ of
{$ifdef cpu64bit}
                scurrency,
                s64bit,
{$endif cpu64bit}
                s8bit,
                s16bit,
                s32bit:
                  begin
                    suffix := 'sint_';
                    { we also need a destsize para in this case }
                    sizepara := ccallparanode.create(cordconstnode.create
                      (destpara.resulttype.def.size,s32inttype,true),nil);
                  end;
{$ifdef cpu64bit}
                u64bit,
{$endif cpu64bit}
                u8bit,
                u16bit,
                u32bit:
                   suffix := 'uint_';
{$ifndef cpu64bit}
                scurrency,
                s64bit: suffix := 'int64_';
                u64bit: suffix := 'qword_';
{$endif cpu64bit}
                else
                  internalerror(200304225);
              end;
            end;
          floatdef:
            begin
              suffix := 'real_';
            end;
        end;

        procname := procname + suffix;

        { play a trick to have tcallnode handle invalid source parameters: }
        { the shortstring-longint val routine by default                   }
        if (sourcepara.resulttype.def.deftype = stringdef) then
          procname := procname + tstringdef(sourcepara.resulttype.def).stringtypname
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

        { create the call and assign the result to dest  }
        { (val helpers are functions)                    }
        { the assignment will take care of rangechecking }
        addstatement(newstatement,cassignmentnode.create(
          destpara.left,ccallnode.createintern(procname,newparas)));

        { dispose of the enclosing paranode of the destination }
        destpara.left := nil;
        destpara.right := nil;
        destpara.free;

        { check if we used a temp for code and whether we have to store }
        { it to the real code parameter                                 }
        if assigned(orgcode) then
          addstatement(newstatement,cassignmentnode.create(
              orgcode,
              ctemprefnode.create(tempcode)));

        { release the temp if we allocated one }
        if assigned(tempcode) then
          addstatement(newstatement,ctempdeletenode.create(tempcode));

        { free the errornode }
        result.free;
        { and return it }
        result := newblock;
      end;


{$ifdef fpc}
{$maxfpuregisters 0}
{$endif fpc}

    function getpi : bestreal;
      begin
      {$ifdef x86}
        { x86 has pi in hardware }
        result:=pi;
      {$else x86}
        {$ifdef cpuextended}
          result:=extended(MathPiExtended);
        {$else cpuextended}
          result:=double(MathPi);
        {$endif cpuextended}
      {$endif x86}
      end;


    function tinlinenode.det_resulttype:tnode;

        function do_lowhigh(const t:ttype) : tnode;
        var
           v    : tconstexprint;
           enum : tenumsym;
           hp   : tnode;
        begin
           case t.def.deftype of
             orddef:
               begin
                  if inlinenumber=in_low_x then
                    v:=torddef(t.def).low
                  else
                    v:=torddef(t.def).high;
                  { low/high of torddef are longints, so we need special }
                  { handling for cardinal and 64bit types (JM)           }
                  { 1.0.x doesn't support int64($ffffffff) correct, it'll expand
                    to -1 instead of staying $ffffffff. Therefor we use $ffff with
                    shl twice (PFV) }
                  case torddef(t.def).typ of
                    s64bit,scurrency :
                      begin
                        if (inlinenumber=in_low_x) then
                          v := int64($80000000) shl 32
                        else
                          v := (int64($7fffffff) shl 32) or int64($ffff) shl 16 or int64($ffff)
                      end;
                    u64bit :
                      begin
                        { we have to use a dirty trick for high(qword),     }
                        { because it's bigger than high(tconstexprint) (JM) }
                        v := 0
                      end
                    else
                      begin
                        if not is_signed(t.def) then
                          v := cardinal(v);
                      end;
                  end;
                  hp:=cordconstnode.create(v,t,true);
                  resulttypepass(hp);
                  { fix high(qword) }
                  if (torddef(t.def).typ=u64bit) and
                     (inlinenumber = in_high_x) then
                    tordconstnode(hp).value := -1; { is the same as qword($ffffffffffffffff) }
                  do_lowhigh:=hp;
               end;
             enumdef:
               begin
                  enum:=tenumsym(tenumdef(t.def).firstenum);
                  v:=tenumdef(t.def).maxval;
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
              if (cs_check_range in aktlocalswitches) or
                 (cs_check_overflow in aktlocalswitches) then
                 begin
                   result:=crealconstnode.create(0,pbestrealtype^);
                   CGMessage(type_e_wrong_math_argument)
                 end
              else
                begin
                  if r=0.0 then
                    result:=crealconstnode.create(double(MathQNaN),pbestrealtype^)
                  else
                    result:=crealconstnode.create(double(MathNegInf),pbestrealtype^)
                end
            else
              result:=crealconstnode.create(ln(r),pbestrealtype^)
          end;


        function handle_sqrt_const(r : bestreal) : tnode;
          begin
            if r<0.0 then
              if (cs_check_range in aktlocalswitches) or
                 (cs_check_overflow in aktlocalswitches) then
                 begin
                   result:=crealconstnode.create(0,pbestrealtype^);
                   CGMessage(type_e_wrong_math_argument)
                 end
              else
                result:=crealconstnode.create(double(MathQNaN),pbestrealtype^)
            else
              result:=crealconstnode.create(sqrt(r),pbestrealtype^)
          end;


      var
         vl,vl2    : TConstExprInt;
         vr        : bestreal;
         hightree,
         hp        : tnode;
         srsym     : tsym;
         checkrange : boolean;
      label
         myexit;
      begin
         result:=nil;
         { if we handle writeln; left contains no valid address }
         if assigned(left) then
           begin
             if left.nodetype=callparan then
               tcallparanode(left).get_paratype
             else
               resulttypepass(left);
           end;
         inc(parsing_para_level);

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
                     CGMessage1(type_e_integer_expr_expected,left.resulttype.def.typename)
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
                   hp:=genintconstnode(abs(vl));
                 in_const_sqr :
                   hp:=genintconstnode(sqr(vl));
                 in_const_odd :
                   hp:=cordconstnode.create(byte(odd(vl)),booltype,true);
                 in_const_swap_word :
                   hp:=cordconstnode.create((vl and $ff) shl 8+(vl shr 8),left.resulttype,true);
                 in_const_swap_long :
                   hp:=cordconstnode.create((vl and $ffff) shl 16+(vl shr 16),left.resulttype,true);
                 in_const_swap_qword :
                   hp:=cordconstnode.create((vl and $ffff) shl 32+(vl shr 32),left.resulttype,true);
                 in_const_ptr :
                   hp:=cpointerconstnode.create((vl2 shl 4)+vl,voidfarpointertype);
                 else
                   internalerror(88);
               end;
             end;
            if hp=nil then
             hp:=cerrornode.create;
            result:=hp;
            goto myexit;
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
                  { give warning for incompatibility with tp and delphi }
                  if (inlinenumber in [in_lo_long,in_hi_long,in_lo_qword,in_hi_qword]) and
                     ((m_tp7 in aktmodeswitches) or
                      (m_delphi in aktmodeswitches)) then
                    CGMessage(type_w_maybe_wrong_hi_lo);
                  { constant folding }
                  if left.nodetype=ordconstn then
                   begin
                     case inlinenumber of
                       in_lo_word :
                         hp:=cordconstnode.create(tordconstnode(left).value and $ff,left.resulttype,true);
                       in_hi_word :
                         hp:=cordconstnode.create(tordconstnode(left).value shr 8,left.resulttype,true);
                       in_lo_long :
                         hp:=cordconstnode.create(tordconstnode(left).value and $ffff,left.resulttype,true);
                       in_hi_long :
                         hp:=cordconstnode.create(tordconstnode(left).value shr 16,left.resulttype,true);
                       in_lo_qword :
                         hp:=cordconstnode.create(tordconstnode(left).value and $ffffffff,left.resulttype,true);
                       in_hi_qword :
                         hp:=cordconstnode.create(tordconstnode(left).value shr 32,left.resulttype,true);
                     end;
                     result:=hp;
                     goto myexit;
                   end;
                  set_varstate(left,vs_used,[vsf_must_be_valid]);
                  if not is_integer(left.resulttype.def) then
                    CGMessage1(type_e_integer_expr_expected,left.resulttype.def.typename);
                  case inlinenumber of
                    in_lo_word,
                    in_hi_word :
                      resulttype:=u8inttype;
                    in_lo_long,
                    in_hi_long :
                      resulttype:=u16inttype;
                    in_lo_qword,
                    in_hi_qword :
                      resulttype:=u32inttype;
                  end;
                end;


              in_sizeof_x:
                begin
                  set_varstate(left,vs_used,[]);
                  if paramanager.push_high_param(vs_value,left.resulttype.def,current_procinfo.procdef.proccalloption) then
                   begin
                     hightree:=load_high_value_node(tparavarsym(tloadnode(left).symtableentry));
                     if assigned(hightree) then
                      begin
                        hp:=caddnode.create(addn,hightree,
                                         cordconstnode.create(1,sinttype,false));
                        if (left.resulttype.def.deftype=arraydef) and
                           (tarraydef(left.resulttype.def).elesize<>1) then
                          hp:=caddnode.create(muln,hp,cordconstnode.create(tarraydef(
                            left.resulttype.def).elesize,sinttype,true));
                        result:=hp;
                      end;
                   end
                  else
                   resulttype:=sinttype;
                end;

              in_typeof_x:
                begin
                  set_varstate(left,vs_used,[]);
                  resulttype:=voidpointertype;
                end;

              in_ord_x:
                begin
                   if (left.nodetype=ordconstn) then
                    begin
                      hp:=cordconstnode.create(
                         tordconstnode(left).value,sinttype,true);
                      result:=hp;
                      goto myexit;
                    end;
                   set_varstate(left,vs_used,[vsf_must_be_valid]);
                   case left.resulttype.def.deftype of
                     orddef :
                       begin
                         case torddef(left.resulttype.def).typ of
                           bool8bit,
                           uchar:
                             begin
                               { change to byte() }
                               hp:=ctypeconvnode.create_internal(left,u8inttype);
                               left:=nil;
                               result:=hp;
                             end;
                           bool16bit,
                           uwidechar :
                             begin
                               { change to word() }
                               hp:=ctypeconvnode.create_internal(left,u16inttype);
                               left:=nil;
                               result:=hp;
                             end;
                           bool32bit :
                             begin
                               { change to dword() }
                               hp:=ctypeconvnode.create_internal(left,u32inttype);
                               left:=nil;
                               result:=hp;
                             end;
                           uvoid :
                             CGMessage1(type_e_ordinal_expr_expected,left.resulttype.def.typename);
                           else
                             begin
                               { all other orddef need no transformation }
                               hp:=left;
                               left:=nil;
                               result:=hp;
                             end;
                         end;
                       end;
                     enumdef :
                       begin
                         hp:=ctypeconvnode.create_internal(left,s32inttype);
                         left:=nil;
                         result:=hp;
                       end;
                     pointerdef :
                       begin
                         if m_mac in aktmodeswitches then
                           begin
                             hp:=ctypeconvnode.create_internal(left,ptrinttype);
                             left:=nil;
                             result:=hp;
                           end
                         else
                           CGMessage1(type_e_ordinal_expr_expected,left.resulttype.def.typename);
                       end
                     else
                       CGMessage1(type_e_ordinal_expr_expected,left.resulttype.def.typename);
                   end;
                end;

              in_chr_byte:
                begin
                   { convert to explicit char() }
                   set_varstate(left,vs_used,[vsf_must_be_valid]);
                   hp:=ctypeconvnode.create_internal(left,cchartype);
                   left:=nil;
                   result:=hp;
                end;

              in_length_x:
                begin
                  set_varstate(left,vs_used,[vsf_must_be_valid]);

                  case left.resulttype.def.deftype of
                    stringdef :
                      begin
                        { we don't need string convertions here }
                        if (left.nodetype=typeconvn) and
                           (ttypeconvnode(left).left.resulttype.def.deftype=stringdef) then
                         begin
                           hp:=ttypeconvnode(left).left;
                           ttypeconvnode(left).left:=nil;
                           left.free;
                           left:=hp;
                         end;

                        { evaluates length of constant strings direct }
                        if (left.nodetype=stringconstn) then
                         begin
                           hp:=cordconstnode.create(
                             tstringconstnode(left).len,s32inttype,true);
                           result:=hp;
                           goto myexit;
                         end;
                      end;
                    orddef :
                      begin
                        { length of char is one allways }
                        if is_char(left.resulttype.def) or
                           is_widechar(left.resulttype.def) then
                         begin
                           hp:=cordconstnode.create(1,s32inttype,false);
                           result:=hp;
                           goto myexit;
                         end
                        else
                         CGMessage(type_e_mismatch);
                      end;
                    pointerdef :
                      begin
                        if is_pchar(left.resulttype.def) then
                         begin
                            hp := ccallparanode.create(left,nil);
                            result := ccallnode.createintern('fpc_pchar_length',hp);
                            { make sure the left node doesn't get disposed, since it's }
                            { reused in the new node (JM)                              }
                            left:=nil;
                            goto myexit;
                         end
                        else if is_pwidechar(left.resulttype.def) then
                         begin
                            hp := ccallparanode.create(left,nil);
                            result := ccallnode.createintern('fpc_pwidechar_length',hp);
                            { make sure the left node doesn't get disposed, since it's }
                            { reused in the new node (JM)                              }
                            left:=nil;
                            goto myexit;
                         end
                        else
                         CGMessage(type_e_mismatch);
                      end;
                    arraydef :
                      begin
                        if is_open_array(left.resulttype.def) or
                           is_array_of_const(left.resulttype.def) then
                         begin
                           hightree:=load_high_value_node(tparavarsym(tloadnode(left).symtableentry));
                           if assigned(hightree) then
                            begin
                              hp:=caddnode.create(addn,hightree,
                                                  cordconstnode.create(1,s32inttype,false));
                              result:=hp;
                            end;
                           goto myexit;
                         end
                        else
                         if not is_dynamic_array(left.resulttype.def) then
                          begin
                            hp:=cordconstnode.create(tarraydef(left.resulttype.def).highrange-
                                                      tarraydef(left.resulttype.def).lowrange+1,
                                                     s32inttype,true);
                            result:=hp;
                            goto myexit;
                          end
                        else
                          begin
                            hp := ccallparanode.create(ctypeconvnode.create_internal(left,voidpointertype),nil);
                            result := ccallnode.createintern('fpc_dynarray_length',hp);
                            { make sure the left node doesn't get disposed, since it's }
                            { reused in the new node (JM)                              }
                            left:=nil;
                            goto myexit;
                          end;
                      end;
                    else
                      CGMessage(type_e_mismatch);
                  end;

                  { shortstring return an 8 bit value as the length
                    is the first byte of the string }
                  if is_shortstring(left.resulttype.def) then
                   resulttype:=u8inttype
                  else
                   resulttype:=sinttype;
                end;

              in_typeinfo_x:
                begin
                   set_varstate(left,vs_used,[vsf_must_be_valid]);
                   resulttype:=voidpointertype;
                end;

              in_assigned_x:
                begin
                  { the parser has already made sure the expression is valid }

                  { handle constant expressions }
                  if is_constnode(tcallparanode(left).left) or
                     (tcallparanode(left).left.nodetype = pointerconstn) then
                    begin
                      { let an add node figure it out }
                      result := caddnode.create(unequaln,tcallparanode(left).left,cnilnode.create);
                      tcallparanode(left).left := nil;
                      { free left, because otherwise some code at 'myexit' tries  }
                      { to run get_paratype for it, which crashes since left.left }
                      { is now nil                                                }
                      left.free;
                      left := nil;
                      goto myexit;
                    end;
                  { otherwise handle separately, because there could be a procvar, which }
                  { is 2*sizeof(pointer), while we must only check the first pointer     }
                  set_varstate(tcallparanode(left).left,vs_used,[vsf_must_be_valid]);
                  resulttype:=booltype;
                end;

              in_ofs_x :
                internalerror(2000101001);

              in_seg_x :
                begin
                  set_varstate(left,vs_used,[]);
                  result:=cordconstnode.create(0,s32inttype,false);
                  goto myexit;
                end;

              in_pred_x,
              in_succ_x:
                begin
                   set_varstate(left,vs_used,[vsf_must_be_valid]);
                   resulttype:=left.resulttype;
                   if not is_ordinal(resulttype.def) then
                     CGMessage(type_e_ordinal_expr_expected)
                   else
                     begin
                       if (resulttype.def.deftype=enumdef) and
                          (tenumdef(resulttype.def).has_jumps) then
                         CGMessage(type_e_succ_and_pred_enums_with_assign_not_possible);
                     end;

                   { only if the result is an enum do we do range checking }
                   if (resulttype.def.deftype=enumdef) then
                     checkrange := true
                   else
                     checkrange := false;

                   { do constant folding after check for jumps }
                   if left.nodetype=ordconstn then
                    begin
                      if inlinenumber=in_succ_x then
                        result:=cordconstnode.create(tordconstnode(left).value+1,left.resulttype,checkrange)
                      else
                        result:=cordconstnode.create(tordconstnode(left).value-1,left.resulttype,checkrange);
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
                  resulttype:=voidtype;
                  if assigned(left) then
                    begin
                       { first param must be var }
                       valid_for_var(tcallparanode(left).left);
                       set_varstate(tcallparanode(left).left,vs_used,[vsf_must_be_valid]);

                       if (left.resulttype.def.deftype in [enumdef,pointerdef]) or
                          is_ordinal(left.resulttype.def) or
                          is_currency(left.resulttype.def) then
                        begin
                          { value of left gets changed -> must be unique }
                          set_unique(tcallparanode(left).left);
                          { two paras ? }
                          if assigned(tcallparanode(left).right) then
                           begin
                             set_varstate(tcallparanode(tcallparanode(left).right).left,vs_used,[vsf_must_be_valid]);
                             inserttypeconv_internal(tcallparanode(tcallparanode(left).right).left,tcallparanode(left).left.resulttype);
                             if assigned(tcallparanode(tcallparanode(left).right).right) then
                               CGMessage(parser_e_illegal_expression);
                           end;
                        end
                       else
                        CGMessage(type_e_ordinal_expr_expected);
                    end
                  else
                    CGMessage(type_e_mismatch);
                end;

              in_read_x,
              in_readln_x,
              in_write_x,
              in_writeln_x :
                begin
                  result := handle_read_write;
                end;

              in_settextbuf_file_x :
                begin
                  resulttype:=voidtype;
                  { now we know the type of buffer }
                  srsym:=searchsymonlyin(systemunit,'SETTEXTBUF');
                  hp:=ccallparanode.create(cordconstnode.create(
                     tcallparanode(left).left.resulttype.def.size,s32inttype,true),left);
                  result:=ccallnode.create(hp,tprocsym(srsym),systemunit,nil,[]);
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
                  result := handle_str;
                end;

              in_val_x :
                begin
                  result := handle_val;
                end;

              in_include_x_y,
              in_exclude_x_y:
                begin
                  resulttype:=voidtype;
                  { the parser already checks whether we have two (and exectly two) }
                  { parameters (JM)                                                 }
                  { first param must be var }
                  valid_for_var(tcallparanode(left).left);
                  set_varstate(tcallparanode(left).left,vs_used,[vsf_must_be_valid]);
                  { check type }
                  if (left.resulttype.def.deftype=setdef) then
                    begin
                      { insert a type conversion       }
                      { to the type of the set elements  }
                      set_varstate(tcallparanode(tcallparanode(left).right).left,vs_used,[vsf_must_be_valid]);
                      inserttypeconv(tcallparanode(tcallparanode(left).right).left,
                        tsetdef(left.resulttype.def).elementtype);
                    end
                  else
                    CGMessage(type_e_mismatch);
                end;

              in_low_x,
              in_high_x:
                begin
                  case left.resulttype.def.deftype of
                    orddef,
                    enumdef:
                      begin
                        result:=do_lowhigh(left.resulttype);
                      end;
                    setdef:
                      begin
                        result:=do_lowhigh(tsetdef(left.resulttype.def).elementtype);
                      end;
                    arraydef:
                      begin
                        if inlinenumber=in_low_x then
                         begin
                           result:=cordconstnode.create(tarraydef(
                            left.resulttype.def).lowrange,tarraydef(left.resulttype.def).rangetype,true);
                         end
                        else
                         begin
                           if is_open_array(left.resulttype.def) or
                              is_array_of_const(left.resulttype.def) then
                            begin
                              set_varstate(left,vs_used,[vsf_must_be_valid]);
                              result:=load_high_value_node(tparavarsym(tloadnode(left).symtableentry));
                            end
                           else
                            if is_dynamic_array(left.resulttype.def) then
                              begin
                                set_varstate(left,vs_used,[vsf_must_be_valid]);
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
                              result:=cordconstnode.create(tarraydef(
                               left.resulttype.def).highrange,tarraydef(left.resulttype.def).rangetype,true);
                            end;
                         end;
                      end;
                    stringdef:
                      begin
                        if inlinenumber=in_low_x then
                         begin
                           result:=cordconstnode.create(0,u8inttype,false);
                         end
                        else
                         begin
                           if is_open_string(left.resulttype.def) then
			     begin
                               set_varstate(left,vs_used,[vsf_must_be_valid]);
                               result:=load_high_value_node(tparavarsym(tloadnode(left).symtableentry))
			     end
                           else
                             result:=cordconstnode.create(tstringdef(left.resulttype.def).len,u8inttype,true);
                         end;
                     end;
                    else
                      CGMessage(type_e_mismatch);
                  end;
                end;

              in_exp_real :
                begin
                  if left.nodetype in [ordconstn,realconstn] then
                    begin
                      result:=crealconstnode.create(exp(getconstrealvalue),pbestrealtype^);
                      if (trealconstnode(result).value_real=double(MathInf)) and
                         ((cs_check_range in aktlocalswitches) or
                          (cs_check_overflow in aktlocalswitches)) then
                        begin
                          result:=crealconstnode.create(0,pbestrealtype^);
                          CGMessage(parser_e_range_check_error);
                        end;
                    end
                  else
                    begin
                      set_varstate(left,vs_used,[vsf_must_be_valid]);
                      inserttypeconv(left,pbestrealtype^);
                      resulttype:=pbestrealtype^;
                    end;
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
                  else
                    begin
                      set_varstate(left,vs_used,[vsf_must_be_valid]);
                      inserttypeconv(left,pbestrealtype^);
                      resulttype:=s64inttype;
                    end;
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
                  else
                    begin
                      set_varstate(left,vs_used,[vsf_must_be_valid]);
                      inserttypeconv(left,pbestrealtype^);
                      resulttype:=s64inttype;
                    end;
                end;

              in_frac_real :
                begin
                  if left.nodetype in [ordconstn,realconstn] then
                    setconstrealvalue(frac(getconstrealvalue))
                  else
                    begin
                      set_varstate(left,vs_used,[vsf_must_be_valid]);
                      inserttypeconv(left,pbestrealtype^);
                      resulttype:=pbestrealtype^;
                    end;
                end;

              in_int_real :
                begin
                  if left.nodetype in [ordconstn,realconstn] then
                    setconstrealvalue(int(getconstrealvalue))
                  else
                    begin
                      set_varstate(left,vs_used,[vsf_must_be_valid]);
                      inserttypeconv(left,pbestrealtype^);
                      resulttype:=pbestrealtype^;
                    end;
                end;

             in_pi_real :
                begin
                  if block_type=bt_const then
                     setconstrealvalue(getpi)
                  else
                     resulttype:=pbestrealtype^;
                end;

              in_cos_real :
                begin
                  if left.nodetype in [ordconstn,realconstn] then
                   setconstrealvalue(cos(getconstrealvalue))
                  else
                   begin
                     set_varstate(left,vs_used,[vsf_must_be_valid]);
                     inserttypeconv(left,pbestrealtype^);
                     resulttype:=pbestrealtype^;
                   end;
                end;

              in_sin_real :
                begin
                  if left.nodetype in [ordconstn,realconstn] then
                   setconstrealvalue(sin(getconstrealvalue))
                  else
                   begin
                     set_varstate(left,vs_used,[vsf_must_be_valid]);
                     inserttypeconv(left,pbestrealtype^);
                     resulttype:=pbestrealtype^;
                   end;
                end;

              in_arctan_real :
                begin
                  if left.nodetype in [ordconstn,realconstn] then
                   setconstrealvalue(arctan(getconstrealvalue))
                  else
                   begin
                     set_varstate(left,vs_used,[vsf_must_be_valid]);
                     inserttypeconv(left,pbestrealtype^);
                     resulttype:=pbestrealtype^;
                   end;
                end;

              in_abs_real :
                begin
                  if left.nodetype in [ordconstn,realconstn] then
                   setconstrealvalue(abs(getconstrealvalue))
                  else
                   begin
                     set_varstate(left,vs_used,[vsf_must_be_valid]);
                     inserttypeconv(left,pbestrealtype^);
                     resulttype:=pbestrealtype^;
                   end;
                end;

              in_sqr_real :
                begin
                  if left.nodetype in [ordconstn,realconstn] then
                   setconstrealvalue(sqr(getconstrealvalue))
                  else
                   begin
                     set_varstate(left,vs_used,[vsf_must_be_valid]);
                     inserttypeconv(left,pbestrealtype^);
                     resulttype:=pbestrealtype^;
                   end;
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
                  else
                   begin
                     set_varstate(left,vs_used,[vsf_must_be_valid]);
                     inserttypeconv(left,pbestrealtype^);
                     resulttype:=pbestrealtype^;
                   end;
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
                  else
                   begin
                     set_varstate(left,vs_used,[vsf_must_be_valid]);
                     inserttypeconv(left,pbestrealtype^);
                     resulttype:=pbestrealtype^;
                   end;
                end;

 {$ifdef SUPPORT_MMX}
              in_mmx_pcmpeqb..in_mmx_pcmpgtw:
                begin
                end;
 {$endif SUPPORT_MMX}
              in_prefetch_var:
                begin
                  resulttype:=voidtype;
                end;
              in_assert_x_y :
                begin
                  resulttype:=voidtype;
                  if assigned(left) then
                    begin
                       set_varstate(tcallparanode(left).left,vs_used,[vsf_must_be_valid]);
                       { check type }
                       if is_boolean(left.resulttype.def) then
                         begin
                            set_varstate(tcallparanode(tcallparanode(left).right).left,vs_used,[vsf_must_be_valid]);
                            { must always be a string }
                            inserttypeconv(tcallparanode(tcallparanode(left).right).left,cshortstringtype);
                         end
                       else
                         CGMessage1(type_e_boolean_expr_expected,left.resulttype.def.typename);
                    end
                  else
                    CGMessage(type_e_mismatch);

                  { We've checked the whole statement for correctness, now we
                    can remove it if assertions are off }
                  if not(cs_do_assertion in aktlocalswitches) then
                    begin
                      { we need a valid node, so insert a nothingn }
                      result:=cnothingnode.create;
                    end
                   else
                     include(current_procinfo.flags,pi_do_call);
                end;

               else
                internalerror(8);
            end;
          end;

      myexit:
        { Run get_paratype again to update maybe inserted typeconvs }
        if not codegenerror then
         begin
           if assigned(left) and
              (left.nodetype=callparan) then
            tcallparanode(left).get_paratype;
         end;
        dec(parsing_para_level);
      end;


    function tinlinenode.pass_1 : tnode;
      var
         hp,hpp  : tnode;
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
              left_max;
           end;

         inc(parsing_para_level);
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
                    cordconstnode.create(shiftconst,u32inttype,false)),resulttype)
              else
                result := ctypeconvnode.create_internal(left,resulttype);
              left := nil;
              firstpass(result);
            end;

          in_sizeof_x:
            begin
              if registersint<1 then
                 registersint:=1;
              expectloc:=LOC_REGISTER;
            end;

          in_typeof_x:
            begin
               if registersint<1 then
                 registersint:=1;
               expectloc:=LOC_REGISTER;
            end;

          in_length_x:
            begin
               if is_shortstring(left.resulttype.def) then
                expectloc:=left.expectloc
               else
                begin
                  { ansi/wide string }
                  if registersint<1 then
                   registersint:=1;
                  expectloc:=LOC_REGISTER;
                end;
            end;

          in_typeinfo_x:
            begin
               expectloc:=LOC_REGISTER;
               registersint:=1;
            end;

          in_assigned_x:
            begin
              expectloc := LOC_JUMP;
              registersint:=1;
            end;

          in_pred_x,
          in_succ_x:
            begin
              if is_64bit(resulttype.def) then
               begin
                 if (registersint<2) then
                  registersint:=2
               end
              else
               begin
                 if (registersint<1) then
                  registersint:=1;
               end;
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

               { check type }
               if
{$ifndef cpu64bit}
                  is_64bit(left.resulttype.def) or
{$endif cpu64bit}
                  { range/overflow checking doesn't work properly }
                  { with the inc/dec code that's generated (JM)   }
                  (
                   (((left.resulttype.def.deftype = orddef) and
                     not(is_char(left.resulttype.def)) and
                     not(is_boolean(left.resulttype.def))) or
                    (left.resulttype.def.deftype = pointerdef)) and
                   (aktlocalswitches * [cs_check_overflow,cs_check_range] <> [])
                  ) then
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
                       hpp := cordconstnode.create(1,tcallparanode(left).left.resulttype,false);
                     end;
                   if (tcallparanode(left).left.resulttype.def.deftype=pointerdef) then
                     inserttypeconv_internal(hpp,sinttype);
                   { make sure we don't call functions part of the left node twice (and generally }
                   { optimize the code generation)                                                }
                   if node_complexity(tcallparanode(left).left) > 1 then
                     begin
                       tempnode := ctempcreatenode.create(voidpointertype,voidpointertype.def.size,tt_persistent,true);
                       addstatement(newstatement,tempnode);
                       addstatement(newstatement,cassignmentnode.create(ctemprefnode.create(tempnode),
                         caddrnode.create_internal(tcallparanode(left).left.getcopy)));
                       hp := cderefnode.create(ctemprefnode.create(tempnode));
                       inserttypeconv_internal(hp,tcallparanode(left).left.resulttype);
                     end
                   else
                     begin
                       hp := tcallparanode(left).left.getcopy;
                       tempnode := nil;
                     end;
                   { addition/substraction depending on inc/dec }
                   if inlinenumber = in_inc_x then
                     hpp := caddnode.create(addn,hp,hpp)
                   else
                     hpp := caddnode.create(subn,hp,hpp);
                   { assign result of addition }
                   inserttypeconv_internal(hpp,hp.resulttype);
                   addstatement(newstatement,cassignmentnode.create(hp.getcopy,hpp));
                   { deallocate the temp }
                   if assigned(tempnode) then
                     addstatement(newstatement,ctempdeletenode.create(tempnode));
                   { firstpass it }
                   firstpass(newblock);
                   { return new node }
                   result := newblock;
                 end
               else if (left.resulttype.def.deftype in [enumdef,pointerdef]) or
                       is_ordinal(left.resulttype.def) then
                 begin
                    { two paras ? }
                    if assigned(tcallparanode(left).right) then
                      begin
                         { need we an additional register ? }
                         if not(is_constintnode(tcallparanode(tcallparanode(left).right).left)) and
                           (tcallparanode(tcallparanode(left).right).left.expectloc in [LOC_CREFERENCE,LOC_REFERENCE]) and
                           (tcallparanode(tcallparanode(left).right).left.registersint<=1) then
                           inc(registersint);

                         { do we need an additional register to restore the first parameter? }
                         if tcallparanode(tcallparanode(left).right).left.registersint>=registersint then
                           inc(registersint);
                      end;
                 end;
            end;

         in_include_x_y,
         in_exclude_x_y:
           begin
              expectloc:=LOC_VOID;

              registersint:=left.registersint;
              registersfpu:=left.registersfpu;
{$ifdef SUPPORT_MMX}
              registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}
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
              registersint:=left.registersint;
              registersfpu:=left.registersfpu;
{$ifdef SUPPORT_MMX}
              registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}
            end;

          in_low_x,
          in_high_x:
            internalerror(200104047);

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
              { should be handled by det_resulttype }
              internalerror(200108234);
            end;

         in_prefetch_var:
           begin
             expectloc:=LOC_VOID;
           end;

          else
            internalerror(8);
          end;
         dec(parsing_para_level);
       end;
{$ifdef fpc}
{$maxfpuregisters default}
{$endif fpc}

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
        first_sqr_real := ccallnode.createintern('fpc_sqr_real',
                ccallparanode.create(left,nil));
        left := nil;
      end;

     function tinlinenode.first_sqrt_real : tnode;
      begin
        { create the call to the helper }
        { on entry left node contains the parameter }
        first_sqrt_real := ccallnode.createintern('fpc_sqrt_real',
                ccallparanode.create(left,nil));
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

begin
   cinlinenode:=tinlinenode;
end.
