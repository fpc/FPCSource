{
    $Id$
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
       node,htypechk,cpuinfo,symppu;

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
      symbase,symconst,symtype,symdef,symsym,symtable,paramgr,defutil,defcmp,
      pass_1,
      ncal,ncon,ncnv,nadd,nld,nbas,nflw,nmem,nmat,
      cpubase,tgobj,cgbase
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
        newnode : tnode;
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
        is_real := source.resulttype.def.deftype = floatdef;

        if not assigned(dest) or
           ((dest.left.resulttype.def.deftype<>stringdef) and
            not(is_chararray(dest.left.resulttype.def))) or
           not(is_real or
               (source.left.resulttype.def.deftype = orddef)) then
          begin
            { the parser will give this message already because we }
            { return an errornode (JM)                             }
            { CGMessagePos(fileinfo,cg_e_illegal_expression);      }
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
              ord(tfloatdef(source.left.resulttype.def).typ),s32bittype,true),
               newparas.right);
            { if necessary, insert a fraction parameter }
            if not assigned(fracpara) then
              begin
                tcallparanode(newparas.right).right := ccallparanode.create(
                  cordconstnode.create(-1,s32bittype,false),
                   tcallparanode(newparas.right).right);
                fracpara := tcallparanode(tcallparanode(newparas.right).right);
              end;
            { if necessary, insert a length para }
            if not assigned(lenpara) then
              fracpara.right := ccallparanode.create(
                cordconstnode.create(-32767,s32bittype,false),
                   fracpara.right);
          end
        else
          { for a normal parameter, insert a only length parameter if one is missing }
          if not assigned(lenpara) then
            newparas.right := ccallparanode.create(cordconstnode.create(-1,s32bittype,false),
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
            u32bit:
              procname := procname + 'longword';
            u64bit:
              procname := procname + 'qword';
            s64bit:
              procname := procname + 'int64';
            else
              procname := procname + 'longint';
          end;

        { create the call node, }
        newnode := ccallnode.createintern(procname,newparas);
        { resulttypepass it }
        resulttypepass(newnode);

        { and return it (but first free the errornode we generated in the beginning) }
        result.free;
        result := newnode;
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
          tfiledef(left.resulttype.def).typedfiletype.def.size,s32bittype,true),
          ccallparanode.create(left,nil));
        { create the correct call }
        if inlinenumber=in_reset_typedfile then
          result := ccallnode.createintern('fpc_reset_typed',left)
        else
          result := ccallnode.createintern('fpc_rewrite_typed',left);
        firstpass(result);
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
        srsym         : tvarsym;
        tempowner     : tsymtable;
        restype       : ^ttype;
        is_typed,
        do_read,
        is_real,
        error_para,
        found_error,
        is_ordinal   : boolean;
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
            { retrieve the symbols for standard input/output handle }
            if do_read then
              name := 'INPUT'
            else
              name := 'OUTPUT';
            if not searchsysvar(name,srsym,tempowner) then
              internalerror(200108141);

            { since the input/output variables are threadvars loading them into
              a temp once is faster. Create a temp which will hold a pointer to the file }
            filetemp := ctempcreatenode.create(voidpointertype,voidpointertype.def.size,true);
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
                caddrnode.create(cloadnode.create(srsym,tempowner))));

            { create a new fileparameter as follows: file_type(temp^)    }
            { (so that we pass the value and not the address of the temp }
            { to the read/write routine)                                 }
            filepara := ccallparanode.create(ctypeconvnode.create_explicit(
              cderefnode.create(ctemprefnode.create(filetemp)),srsym.vartype),nil);
          end
        else
          { remove filepara from the parameter chain }
          begin
            left := filepara.right;
            filepara.right := nil;
            { the file para is a var parameter, but it must be valid already }
            set_varstate(filepara,true);
            { check if we should make a temp to store the result of a complex }
            { expression (better heuristics, anyone?) (JM)                    }
            if (filepara.left.nodetype <> loadn) then
              begin
                { create a temp which will hold a pointer to the file }
                filetemp := ctempcreatenode.create(voidpointertype,voidpointertype.def.size,true);

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
                    caddrnode.create(filepara.left)));
                resulttypepass(newstatement.left);
                { create a new fileparameter as follows: file_type(temp^)    }
                { (so that we pass the value and not the address of the temp }
                { to the read/write routine)                                 }
                nextpara := ccallparanode.create(ctypeconvnode.create_explicit(
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
            filepara.right := ccallparanode.create(cordconstnode.create(
              tfiledef(filepara.resulttype.def).typedfiletype.def.size,s32bittype,true),nil);

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
                    p1:=ccallnode.create(nil,nil,nil,nil);
                    tcallnode(p1).set_procvar(para.left);
                    resulttypepass(p1);
                    para.left:=p1;
                  end;

                if not equal_defs(para.left.resulttype.def,tfiledef(filepara.resulttype.def).typedfiletype.def) then
                  begin
                    CGMessagePos(para.left.fileinfo,type_e_mismatch);
                    found_error := true;
                  end;

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
                if (para.left.nodetype <> loadn) then
                  begin
                    { create temp for result }
                    temp := ctempcreatenode.create(para.left.resulttype,
                      para.left.resulttype.def.size,true);
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
                { is this parameter an ordinal? }
                is_ordinal := false;
                { is this parameter a real? }
                is_real:=false;

                { can't read/write types }
                if para.left.nodetype=typen then
                  begin
                    CGMessagePos(para.fileinfo,type_e_cant_read_write_type);
                    error_para := true;
                  end;

                { support writeln(procvar) }
                if (para.left.resulttype.def.deftype=procvardef) then
                  begin
                    p1:=ccallnode.create(nil,nil,nil,nil);
                    tcallnode(p1).set_procvar(para.left);
                    resulttypepass(p1);
                    para.left:=p1;
                  end;

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
                    end;
                  orddef :
                    begin
                      is_ordinal := true;
                      case torddef(para.left.resulttype.def).typ of
                        s8bit,s16bit,s32bit :
                          name := procprefix+'sint';
                        u8bit,u16bit,u32bit :
                          name := procprefix+'uint';
                        uchar :
                          name := procprefix+'char';
                        uwidechar :
                          name := procprefix+'widechar';
                        s64bit :
                          name := procprefix+'int64';
                        u64bit :
                          name := procprefix+'qword';
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
                              name := procprefix+'boolean'
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
                                cordconstnode.create(0,s32bittype,false),nil)
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
                                cordconstnode.create(-32767,s32bittype,false),nil);
                            { also create a default fracpara if necessary }
                            if not assigned(fracpara) then
                              fracpara := ccallparanode.create(
                                cordconstnode.create(-1,s32bittype,false),nil);
                            { add it to the lenpara }
                            lenpara.right := fracpara;
                            { and add the realtype para (this also removes the link }
                            { to any parameters coming after it)                    }
                            fracpara.right := ccallparanode.create(
                                cordconstnode.create(ord(tfloatdef(para.left.resulttype.def).typ),
                                s32bittype,true),nil);
                          end;
                      end;

                    if do_read and
                      ((is_ordinal and
                        (torddef(para.left.resulttype.def).typ in [s8bit,s16bit,u8bit,u16bit])
                       ) or
                       (is_real and
                        not equal_defs(para.left.resulttype.def,pbestrealtype^.def)
                       )
                      ) then
                      { special handling of reading small numbers, because the helpers  }
                      { expect a longint/card/bestreal var parameter. Use a temp. can't }
                      { use functions because then the call to FPC_IOCHECK destroys     }
                      { their result before we can store it                             }
                      begin
                        { get the resulttype of the var parameter of the helper }
                        if is_real then
                          restype := pbestrealtype
                        else if is_signed(para.left.resulttype.def) then
                          restype := @s32bittype
                        else
                          restype := @u32bittype;

                        { create the parameter list: the temp ... }
                        temp := ctempcreatenode.create(restype^,restype^.def.size,true);
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
           ((codepara.resulttype.def.deftype <> orddef) or
            is_64bitint(codepara.resulttype.def)) then
          begin
            CGMessagePos(codepara.fileinfo,type_e_mismatch);
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
           (torddef(codepara.resulttype.def).typ in [u8bit,u16bit,s8bit,s16bit]) then
          begin
            tempcode := ctempcreatenode.create(s32bittype,4,true);
            addstatement(newstatement,tempcode);
            { set the resulttype of the temp (needed to be able to get }
            { the resulttype of the tempref used in the new code para) }
            resulttypepass(tnode(tempcode));
            { create a temp codepara, but save the original code para to }
            { assign the result to later on                              }
            if assigned(codepara) then
              orgcode := codepara.left
            else
              codepara := ccallparanode.create(nil,nil);
            codepara.left := ctemprefnode.create(tempcode);
            { we need its resulttype later on }
            codepara.get_paratype;
          end
        else if (torddef(codepara.resulttype.def).typ = u32bit) then
          { because code is a var parameter, it must match types exactly    }
          { however, since it will return values in [0..255], both longints }
          { and cardinals are fine. Since the formal code para type is      }
          { longint, insert a typecoversion to longint for cardinal para's  }
          begin
            codepara.left := ctypeconvnode.create(codepara.left,s32bittype);
            { make it explicit, oterwise you may get a nonsense range }
            { check error if the cardinal already contained a value   }
            { > $7fffffff                                             }
            codepara.left.toggleflag(nf_explizit);
            codepara.get_paratype;
          end;

        { create the procedure name }
        procname := 'fpc_val_';

        case destpara.resulttype.def.deftype of
          orddef:
            begin
              case torddef(destpara.resulttype.def).typ of
                s8bit,s16bit,s32bit:
                  begin
                    suffix := 'sint_';
                    { we also need a destsize para in this case }
                    sizepara := ccallparanode.create(cordconstnode.create
                      (destpara.resulttype.def.size,s32bittype,true),nil);
                  end;
                u8bit,u16bit,u32bit:
                   suffix := 'uint_';
                s64bit: suffix := 'int64_';
                u64bit: suffix := 'qword_';
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
                  if is_signed(t.def) and
                     is_64bitint(t.def) then
                    if (inlinenumber=in_low_x) then
                      v := int64($80000000) shl 32
                    else
                      v := (int64($7fffffff) shl 32) or int64($ffff) shl 16 or int64($ffff)
                  else
                    if is_64bitint(t.def) then
                      { we have to use a dirty trick for high(qword),     }
                      { because it's bigger than high(tconstexprint) (JM) }
                      v := 0
                    else
                      if not is_signed(t.def) then
                        v := cardinal(v);
                  hp:=cordconstnode.create(v,t,true);
                  resulttypepass(hp);
                  { fix high(qword) }
                  if not is_signed(t.def) and
                     is_64bitint(t.def) and
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

      var
         vl,vl2    : TConstExprInt;
         vr        : bestreal;
         hightree,
         hp        : tnode;
         srsym     : tsym;
         isreal    : boolean;
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
             begin
               case inlinenumber of
                 in_const_pi :
                   hp:=crealconstnode.create(pi,pbestrealtype^);
                 else
                   internalerror(89);
               end;
             end
            else
             begin
               vl:=0;
               vl2:=0; { second parameter Ex: ptr(vl,vl2) }
               vr:=0;
               isreal:=false;
               case left.nodetype of
                 realconstn :
                   begin
                     isreal:=true;
                     vr:=trealconstnode(left).value_real;
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
                   CGMessage(cg_e_illegal_expression);
               end;
               case inlinenumber of
                 in_const_trunc :
                   begin
                     if isreal then
                       begin
                          if (vr>=2147483648.0) or (vr<=-2147483649.0) then
                            begin
                               CGMessage(parser_e_range_check_error);
                               hp:=cordconstnode.create(1,s32bittype,false)
                            end
                          else
                            hp:=cordconstnode.create(trunc(vr),s32bittype,true)
                       end
                     else
                      hp:=cordconstnode.create(trunc(vl),s32bittype,true);
                   end;
                 in_const_round :
                   begin
                     if isreal then
                       begin
                          if (vr>=2147483647.5) or (vr<=-2147483648.5) then
                            begin
                               CGMessage(parser_e_range_check_error);
                               hp:=cordconstnode.create(1,s32bittype,false)
                            end
                          else
                            hp:=cordconstnode.create(round(vr),s32bittype,true)
                       end
                     else
                      hp:=cordconstnode.create(round(vl),s32bittype,true);
                   end;
                 in_const_frac :
                   begin
                     if isreal then
                      hp:=crealconstnode.create(frac(vr),pbestrealtype^)
                     else
                      hp:=crealconstnode.create(frac(vl),pbestrealtype^);
                   end;
                 in_const_int :
                   begin
                     if isreal then
                      hp:=crealconstnode.create(int(vr),pbestrealtype^)
                     else
                      hp:=crealconstnode.create(int(vl),pbestrealtype^);
                   end;
                 in_const_abs :
                   begin
                     if isreal then
                      hp:=crealconstnode.create(abs(vr),pbestrealtype^)
                     else
                      hp:=cordconstnode.create(abs(vl),left.resulttype,true);
                   end;
                 in_const_sqr :
                   begin
                     if isreal then
                      hp:=crealconstnode.create(sqr(vr),pbestrealtype^)
                     else
                      hp:=cordconstnode.create(sqr(vl),left.resulttype,true);
                   end;
                 in_const_odd :
                   begin
                     if isreal then
                      CGMessage1(type_e_integer_expr_expected,left.resulttype.def.typename)
                     else
                      hp:=cordconstnode.create(byte(odd(vl)),booltype,true);
                   end;
                 in_const_swap_word :
                   begin
                     if isreal then
                      CGMessage1(type_e_integer_expr_expected,left.resulttype.def.typename)
                     else
                      hp:=cordconstnode.create((vl and $ff) shl 8+(vl shr 8),left.resulttype,true);
                   end;
                 in_const_swap_long :
                   begin
                     if isreal then
                      CGMessage(type_e_mismatch)
                     else
                      hp:=cordconstnode.create((vl and $ffff) shl 16+(vl shr 16),left.resulttype,true);
                   end;
                 in_const_swap_qword :
                   begin
                     if isreal then
                      CGMessage(type_e_mismatch)
                     else
                      hp:=cordconstnode.create((vl and $ffff) shl 32+(vl shr 32),left.resulttype,true);
                   end;
                 in_const_ptr :
                   begin
                     if isreal then
                      CGMessage(type_e_mismatch)
                     else
                      hp:=cpointerconstnode.create((vl2 shl 4)+vl,voidfarpointertype);
                   end;
                 in_const_sqrt :
                   begin
                     if isreal then
                       begin
                          if vr<0.0 then
                           CGMessage(type_e_wrong_math_argument)
                          else
                           hp:=crealconstnode.create(sqrt(vr),pbestrealtype^)
                       end
                     else
                       begin
                          if vl<0 then
                           CGMessage(type_e_wrong_math_argument)
                          else
                           hp:=crealconstnode.create(sqrt(vl),pbestrealtype^);
                       end;
                   end;
                 in_const_arctan :
                   begin
                     if isreal then
                      hp:=crealconstnode.create(arctan(vr),pbestrealtype^)
                     else
                      hp:=crealconstnode.create(arctan(vl),pbestrealtype^);
                   end;
                 in_const_cos :
                   begin
                     if isreal then
                      hp:=crealconstnode.create(cos(vr),pbestrealtype^)
                     else
                      hp:=crealconstnode.create(cos(vl),pbestrealtype^);
                   end;
                 in_const_sin :
                   begin
                     if isreal then
                      hp:=crealconstnode.create(sin(vr),pbestrealtype^)
                     else
                      hp:=crealconstnode.create(sin(vl),pbestrealtype^);
                   end;
                 in_const_exp :
                   begin
                     if isreal then
                      hp:=crealconstnode.create(exp(vr),pbestrealtype^)
                     else
                      hp:=crealconstnode.create(exp(vl),pbestrealtype^);
                   end;
                 in_const_ln :
                   begin
                     if isreal then
                       begin
                          if vr<=0.0 then
                           CGMessage(type_e_wrong_math_argument)
                          else
                           hp:=crealconstnode.create(ln(vr),pbestrealtype^)
                       end
                     else
                       begin
                          if vl<=0 then
                           CGMessage(type_e_wrong_math_argument)
                          else
                           hp:=crealconstnode.create(ln(vl),pbestrealtype^);
                       end;
                   end;
                 else
                   internalerror(88);
               end;
             end;
            if hp=nil then
             hp:=tnode.create(errorn);
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
                  set_varstate(left,true);
                  if not is_integer(left.resulttype.def) then
                   CGMessage(type_e_mismatch);
                  case inlinenumber of
                    in_lo_word,
                    in_hi_word :
                      resulttype:=u8bittype;
                    in_lo_long,
                    in_hi_long :
                      resulttype:=u16bittype;
                    in_lo_qword,
                    in_hi_qword :
                      resulttype:=u32bittype;
                  end;
                end;


              in_sizeof_x:
                begin
                  set_varstate(left,false);
                  if paramanager.push_high_param(left.resulttype.def,aktprocdef.proccalloption) then
                   begin
                     hightree:=load_high_value(tvarsym(tloadnode(left).symtableentry));
                     if assigned(hightree) then
                      begin
                        hp:=caddnode.create(addn,hightree,
                                         cordconstnode.create(1,s32bittype,false));
                        if (left.resulttype.def.deftype=arraydef) and
                           (tarraydef(left.resulttype.def).elesize<>1) then
                          hp:=caddnode.create(muln,hp,cordconstnode.create(tarraydef(
                            left.resulttype.def).elesize,s32bittype,true));
                        result:=hp;
                      end;
                   end
                  else
                   resulttype:=s32bittype;
                end;

              in_typeof_x:
                begin
                  set_varstate(left,false);
                  resulttype:=voidpointertype;
                end;

              in_ord_x:
                begin
                   if (left.nodetype=ordconstn) then
                    begin
                      hp:=cordconstnode.create(
                         tordconstnode(left).value,s32bittype,true);
                      result:=hp;
                      goto myexit;
                    end;
                   set_varstate(left,true);
                   case left.resulttype.def.deftype of
                     orddef :
                       begin
                         case torddef(left.resulttype.def).typ of
                           bool8bit,
                           uchar:
                             begin
                               { change to byte() }
                               hp:=ctypeconvnode.create(left,u8bittype);
                               left:=nil;
                               include(hp.flags,nf_explizit);
                               result:=hp;
                             end;
                           bool16bit,
                           uwidechar :
                             begin
                               { change to word() }
                               hp:=ctypeconvnode.create(left,u16bittype);
                               left:=nil;
                               include(hp.flags,nf_explizit);
                               result:=hp;
                             end;
                           bool32bit :
                             begin
                               { change to dword() }
                               hp:=ctypeconvnode.create(left,u32bittype);
                               left:=nil;
                               include(hp.flags,nf_explizit);
                               result:=hp;
                             end;
                           uvoid :
                             CGMessage(type_e_mismatch)
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
                         hp:=ctypeconvnode.create(left,s32bittype);
                         left:=nil;
                         include(hp.flags,nf_explizit);
                         result:=hp;
                       end;
                     else
                       CGMessage(type_e_mismatch);
                   end;
                end;

              in_chr_byte:
                begin
                   { convert to explicit char() }
                   set_varstate(left,true);
                   hp:=ctypeconvnode.create(left,cchartype);
                   include(hp.flags,nf_explizit);
                   left:=nil;
                   result:=hp;
                end;

              in_length_x:
                begin
                  set_varstate(left,true);

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
                             tstringconstnode(left).len,s32bittype,true);
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
                           hp:=cordconstnode.create(1,s32bittype,false);
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
                           hightree:=load_high_value(tvarsym(tloadnode(left).symtableentry));
                           if assigned(hightree) then
                            begin
                              hp:=caddnode.create(addn,hightree,
                                                  cordconstnode.create(1,s32bittype,false));
                              result:=hp;
                            end;
                           goto myexit;
                         end
                        else
                         if not is_dynamic_array(left.resulttype.def) then
                          begin
                            hp:=cordconstnode.create(tarraydef(left.resulttype.def).highrange-
                                                      tarraydef(left.resulttype.def).lowrange+1,
                                                     s32bittype,true);
                            result:=hp;
                            goto myexit;
                          end
                        else
                          begin
                            hp := ccallparanode.create(ctypeconvnode.create_explicit(left,voidpointertype),nil);
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
                   resulttype:=u8bittype
                  else
                   resulttype:=s32bittype;
                end;

              in_typeinfo_x:
                begin
                   set_varstate(left,true);
                   resulttype:=voidpointertype;
                end;

              in_assigned_x:
                begin
                   result := caddnode.create(unequaln,tcallparanode(left).left,cnilnode.create);
                   tcallparanode(left).left := nil;
                   { free left, because otherwise some code at 'myexit' tries  }
                   { to run get_paratype for it, which crashes since left.left }
                   { is now nil                                                }
                   left.free;
                   left := nil;
                   goto myexit;
                end;

              in_ofs_x :
                internalerror(2000101001);

              in_seg_x :
                begin
                  set_varstate(left,false);
                  hp:=cordconstnode.create(0,s32bittype,false);
                  result:=hp;
                  goto myexit;
                end;

              in_pred_x,
              in_succ_x:
                begin
                   set_varstate(left,true);
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
                       hp:=cordconstnode.create(tordconstnode(left).value+1,left.resulttype,checkrange)
                      else
                       hp:=cordconstnode.create(tordconstnode(left).value-1,left.resulttype,checkrange);
                      result:=hp;
                    end;
                end;

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
                       set_varstate(left,true);
                       if codegenerror then
                        exit;
                       { first param must be var }
                       valid_for_var(tcallparanode(left).left);

                       if (left.resulttype.def.deftype in [enumdef,pointerdef]) or
                          is_ordinal(left.resulttype.def) then
                        begin
                           { value of left gets changed -> must be unique }
                           { (bug 1735) (JM)                              }
                           set_unique(tcallparanode(left).left);
                           { two paras ? }
                          if assigned(tcallparanode(left).right) then
                           begin
                             if (aktlocalswitches *
                                   [cs_check_overflow,cs_check_range] = []) then
                               begin
                                 { insert a type conversion       }
                                 { the second param is always longint }
                                 if is_64bitint(left.resulttype.def) then
                                   if is_signed(left.resulttype.def) then
                                     inserttypeconv(tcallparanode(tcallparanode(left).right).left,cs64bittype)
                                   else
                                     inserttypeconv(tcallparanode(tcallparanode(left).right).left,cu64bittype)
                                 else
                                   if is_signed(left.resulttype.def) then
                                     inserttypeconv(tcallparanode(tcallparanode(left).right).left,s32bittype)
                                   else
                                     inserttypeconv(tcallparanode(tcallparanode(left).right).left,u32bittype);
                               end;

                             if assigned(tcallparanode(tcallparanode(left).right).right) then
                              CGMessage(cg_e_illegal_expression);
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
                     tcallparanode(left).left.resulttype.def.size,s32bittype,true),left);
                  hp:=ccallnode.create(hp,tprocsym(srsym),systemunit,nil);
                  left:=nil;
                  result:=hp;
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
                  set_varstate(left,true);
                  { first param must be var }
                  valid_for_var(tcallparanode(left).left);
                  { check type }
                  if (left.resulttype.def.deftype=setdef) then
                    begin
                      { insert a type conversion       }
                      { to the type of the set elements  }
                      inserttypeconv(tcallparanode(tcallparanode(left).right).left,
                        tsetdef(left.resulttype.def).elementtype);
                    end
                  else
                    CGMessage(type_e_mismatch);
                end;

              in_low_x,
              in_high_x:
                begin
                  set_varstate(left,false);
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
                              result:=load_high_value(tvarsym(tloadnode(left).symtableentry));
                            end
                           else
                            if is_dynamic_array(left.resulttype.def) then
                              begin
                                { can't use inserttypeconv because we need }
                                { an explicit type conversion (JM)         }
                                hp := ctypeconvnode.create(left,voidpointertype);
                                hp.toggleflag(nf_explizit);
                                hp := ccallparanode.create(hp,nil);
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
                           result:=cordconstnode.create(0,u8bittype,false);
                         end
                        else
                         begin
                           if is_open_string(left.resulttype.def) then
                            result:=load_high_value(tvarsym(tloadnode(left).symtableentry))
                           else
                            result:=cordconstnode.create(tstringdef(left.resulttype.def).len,u8bittype,true);
                         end;
                     end;
                    else
                      CGMessage(type_e_mismatch);
                  end;
                end;

             in_pi:
                begin
                  if block_type=bt_const then
                     setconstrealvalue(pi)
                  else
                     resulttype:=pbestrealtype^;
                end;
              in_cos_extended :
                begin
                  if left.nodetype in [ordconstn,realconstn] then
                   setconstrealvalue(cos(getconstrealvalue))
                  else
                   begin
                     set_varstate(left,true);
                     inserttypeconv(left,pbestrealtype^);
                     resulttype:=pbestrealtype^;
                   end;
                end;

              in_sin_extended :
                begin
                  if left.nodetype in [ordconstn,realconstn] then
                   setconstrealvalue(sin(getconstrealvalue))
                  else
                   begin
                     set_varstate(left,true);
                     inserttypeconv(left,pbestrealtype^);
                     resulttype:=pbestrealtype^;
                   end;
                end;

              in_arctan_extended :
                begin
                  if left.nodetype in [ordconstn,realconstn] then
                   setconstrealvalue(arctan(getconstrealvalue))
                  else
                   begin
                     set_varstate(left,true);
                     inserttypeconv(left,pbestrealtype^);
                     resulttype:=pbestrealtype^;
                   end;
                end;

              in_abs_extended :
                begin
                  if left.nodetype in [ordconstn,realconstn] then
                   setconstrealvalue(abs(getconstrealvalue))
                  else
                   begin
                     set_varstate(left,true);
                     inserttypeconv(left,pbestrealtype^);
                     resulttype:=pbestrealtype^;
                   end;
                end;

              in_sqr_extended :
                begin
                  if left.nodetype in [ordconstn,realconstn] then
                   setconstrealvalue(sqr(getconstrealvalue))
                  else
                   begin
                     set_varstate(left,true);
                     inserttypeconv(left,pbestrealtype^);
                     resulttype:=pbestrealtype^;
                   end;
                end;

              in_sqrt_extended :
                begin
                  if left.nodetype in [ordconstn,realconstn] then
                   begin
                     vr:=getconstrealvalue;
                     if vr<0.0 then
                       begin
                         CGMessage(type_e_wrong_math_argument);
                         setconstrealvalue(0);
                       end
                      else
                       setconstrealvalue(sqrt(vr));
                   end
                  else
                   begin
                     set_varstate(left,true);
                     inserttypeconv(left,pbestrealtype^);
                     resulttype:=pbestrealtype^;
                   end;
                end;

              in_ln_extended :
                begin
                  if left.nodetype in [ordconstn,realconstn] then
                   begin
                     vr:=getconstrealvalue;
                     if vr<=0.0 then
                       begin
                         CGMessage(type_e_wrong_math_argument);
                         setconstrealvalue(0);
                       end
                      else
                       setconstrealvalue(ln(vr));
                   end
                  else
                   begin
                     set_varstate(left,true);
                     inserttypeconv(left,pbestrealtype^);
                     resulttype:=pbestrealtype^;
                   end;
                end;

 {$ifdef SUPPORT_MMX}
              in_mmx_pcmpeqb..in_mmx_pcmpgtw:
                begin
                end;
 {$endif SUPPORT_MMX}

              in_assert_x_y :
                begin
                  resulttype:=voidtype;
                  if assigned(left) then
                    begin
                       set_varstate(left,true);
                       { check type }
                       if is_boolean(left.resulttype.def) then
                         begin
                            { must always be a string }
                            inserttypeconv(tcallparanode(tcallparanode(left).right).left,cshortstringtype);
                         end
                       else
                         CGMessage(type_e_mismatch);
                    end
                  else
                    CGMessage(type_e_mismatch);

                  { We've checked the whole statement for correctness, now we
                    can remove it if assertions are off }
                  if not(cs_do_assertion in aktlocalswitches) then
                   begin
                     { we need a valid node, so insert a nothingn }
                     result:=cnothingnode.create;
                   end;
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

      begin
         result:=nil;
         { if we handle writeln; left contains no valid address }
         if assigned(left) then
           begin
              if left.nodetype=callparan then
                tcallparanode(left).firstcallparan(false)
              else
                firstpass(left);
              left_max;
              location.loc:=left.location.loc;
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
                result := ctypeconvnode.create(cshlshrnode.create(shrn,left,
                    cordconstnode.create(shiftconst,u32bittype,false)),resulttype)
              else
                result := ctypeconvnode.create(left,resulttype);
              left := nil;
              include(result.flags,nf_explizit);
              firstpass(result);
            end;

          in_sizeof_x:
            begin
              if registers32<1 then
                 registers32:=1;
              location.loc:=LOC_REGISTER;
            end;

          in_typeof_x:
            begin
               if registers32<1 then
                 registers32:=1;
               location.loc:=LOC_REGISTER;
            end;

          in_ord_x,
          in_chr_byte:
            begin
               { should not happend as it's converted to typeconv }
               internalerror(200104045);
            end;


          in_length_x:
            begin
               if is_shortstring(left.resulttype.def) then
                location.loc:=LOC_REFERENCE
               else
                begin
                  { ansi/wide string }
                  if registers32<1 then
                   registers32:=1;
                  location.loc:=LOC_REGISTER;
                end;
            end;

          in_typeinfo_x:
            begin
               location.loc:=LOC_REGISTER;
               registers32:=1;
            end;

          in_assigned_x:
            begin
               { should be removed in resulttype pass }
               internalerror(2002080201);
            end;

          in_ofs_x :
            internalerror(2000101001);

          in_seg_x :
            internalerror(200104046);

          in_pred_x,
          in_succ_x:
            begin
              if is_64bitint(resulttype.def) then
               begin
                 if (registers32<2) then
                  registers32:=2
               end
              else
               begin
                 if (registers32<1) then
                  registers32:=1;
               end;
              location.loc:=LOC_REGISTER;
            end;

          in_setlength_x:
            begin
            end;

          in_finalize_x:
            begin
            end;

          in_inc_x,
          in_dec_x:
            begin
               { check type }
               if is_64bitint(left.resulttype.def) or
                  { range/overflow checking doesn't work properly }
                  { with the inc/dec code that's generated (JM)   }
                  ((left.resulttype.def.deftype = orddef) and
                   not(is_char(left.resulttype.def)) and
                   not(is_boolean(left.resulttype.def)) and
                   (aktlocalswitches *
                    [cs_check_overflow,cs_check_range] <> [])) then
                 { convert to simple add (JM) }
                 begin
                   { extra parameter? }
                   if assigned(tcallparanode(left).right) then
                     begin
                       { Yes, use for add node }
                       hpp := tcallparanode(tcallparanode(left).right).left;
                       tcallparanode(tcallparanode(left).right).left := nil;
                       if assigned(tcallparanode(tcallparanode(left).right).right) then
                         CGMessage(cg_e_illegal_expression);
                     end
                   else
                     { no, create constant 1 }
                     hpp := cordconstnode.create(1,s32bittype,false);
                   { addition/substraction depending on inc/dec }
                   if inlinenumber = in_inc_x then
                     hp := caddnode.create(addn,tcallparanode(left).left.getcopy,hpp)
                   else
                     hp := caddnode.create(subn,tcallparanode(left).left.getcopy,hpp);
                   { assign result of addition }
                   hpp := cassignmentnode.create(tcallparanode(left).left,hp);
                   tcallparanode(left).left := nil;
                   { firstpass it }
                   firstpass(hpp);
                   { return new node }
                   result := hpp;
                 end
               else if (left.resulttype.def.deftype in [enumdef,pointerdef]) or
                       is_ordinal(left.resulttype.def) then
                 begin
                    { two paras ? }
                    if assigned(tcallparanode(left).right) then
                      begin
                         { need we an additional register ? }
                         if not(is_constintnode(tcallparanode(tcallparanode(left).right).left)) and
                           (tcallparanode(tcallparanode(left).right).left.location.loc in [LOC_CREFERENCE,LOC_REFERENCE]) and
                           (tcallparanode(tcallparanode(left).right).left.registers32<=1) then
                           inc(registers32);

                         { do we need an additional register to restore the first parameter? }
                         if tcallparanode(tcallparanode(left).right).left.registers32>=registers32 then
                           inc(registers32);
                      end;
                 end;
            end;

          in_read_x,
          in_readln_x,
          in_write_x,
          in_writeln_x :
            begin
               { should be handled by det_resulttype }
               internalerror(200108234);
            end;
         in_settextbuf_file_x :
           internalerror(200104262);

         in_reset_typedfile,
         in_rewrite_typedfile :
           begin
              { should already be removed in det_resulttype (JM) }
              internalerror(200108236);
           end;

         in_str_x_string :
           begin
              { should already be removed in det_resulttype (JM) }
              internalerror(200108235);
           end;

         in_val_x :
           begin
              { should already be removed in det_resulttype (JM) }
              internalerror(200108242);
           end;

         in_include_x_y,
         in_exclude_x_y:
           begin
              registers32:=left.registers32;
              registersfpu:=left.registersfpu;
{$ifdef SUPPORT_MMX}
              registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}
           end;

         in_low_x,
         in_high_x:
          internalerror(200104047);

         in_cos_extended:
           begin
             result:= first_cos_real;
           end;

         in_sin_extended:
           begin
             result := first_sin_real;
           end;

         in_arctan_extended:
           begin
             result := first_arctan_real;
           end;

         in_pi:
           begin
             result := first_pi;
           end;

         in_abs_extended:
           begin
             result := first_abs_real;
           end;

         in_sqr_extended:
           begin
             result := first_sqr_real;
           end;

         in_sqrt_extended:
           begin
             result := first_sqrt_real;
           end;

         in_ln_extended:
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
              registers32:=left.registers32;
              registersfpu:=left.registersfpu;
{$ifdef SUPPORT_MMX}
              registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}
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
        result := crealconstnode.create(pi,pbestrealtype^);
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


begin
   cinlinenode:=tinlinenode;
end.
{
  $Log$
  Revision 1.104  2002-12-30 12:48:07  jonas
    * fixed web bug 2296

  Revision 1.103  2002/12/17 22:19:33  peter
    * fixed pushing of records>8 bytes with stdcall
    * simplified hightree loading

  Revision 1.102  2002/12/15 21:30:12  florian
    * tcallnode.paraitem introduced, all references to defcoll removed

  Revision 1.101  2002/11/27 20:04:39  peter
    * cdecl array of const fixes

  Revision 1.100  2002/11/27 15:33:47  peter
    * the never ending story of tp procvar hacks

  Revision 1.99  2002/11/27 02:37:13  peter
    * case statement inlining added
    * fixed inlining of write()
    * switched statementnode left and right parts so the statements are
      processed in the correct order when getcopy is used. This is
      required for tempnodes

  Revision 1.98  2002/11/25 17:43:19  peter
    * splitted defbase in defutil,symutil,defcmp
    * merged isconvertable and is_equal into compare_defs(_ext)
    * made operator search faster by walking the list only once

  Revision 1.97  2002/11/18 18:35:01  peter
    * Swap(QWord) constant support

  Revision 1.96  2002/11/18 17:31:57  peter
    * pass proccalloption to ret_in_xxx and push_xxx functions

  Revision 1.95  2002/11/16 17:59:31  peter
    * load threadvar input/output variable in temp

  Revision 1.94  2002/11/15 01:58:52  peter
    * merged changes from 1.0.7 up to 04-11
      - -V option for generating bug report tracing
      - more tracing for option parsing
      - errors for cdecl and high()
      - win32 import stabs
      - win32 records<=8 are returned in eax:edx (turned off by default)
      - heaptrc update
      - more info for temp management in .s file with EXTDEBUG

  Revision 1.93  2002/10/10 19:24:58  florian
    + write(ln) support for variants added

  Revision 1.92  2002/10/10 16:07:57  florian
    + several widestring/pwidechar related stuff added

  Revision 1.91  2002/10/05 14:21:08  peter
    * Length(PChar) supported

  Revision 1.90  2002/09/13 19:12:09  carl
    * only enumerations have range checking for succ/pred in const section

  Revision 1.89  2002/09/09 19:41:01  peter
    * check ranges for pred() and succ()

  Revision 1.88  2002/09/08 13:01:25  jonas
    * first_pi now just generates a constant, added missing calls to firstpass()

  Revision 1.87  2002/09/07 20:42:16  carl
    * cardinal -> longword

  Revision 1.86  2002/09/07 12:16:04  carl
    * second part bug report 1996 fix, testrange in cordconstnode
      only called if option is set (also make parsing a tiny faster)

  Revision 1.85  2002/09/02 19:24:42  peter
    * array of char support for Str()

  Revision 1.84  2002/08/19 19:36:43  peter
    * More fixes for cross unit inlining, all tnodes are now implemented
    * Moved pocall_internconst to po_internconst because it is not a
      calling type at all and it conflicted when inlining of these small
      functions was requested

  Revision 1.83  2002/08/02 07:44:31  jonas
    * made assigned() handling generic
    * add nodes now can also evaluate constant expressions at compile time
      that contain nil nodes

  Revision 1.82  2002/07/29 21:23:43  florian
    * more fixes for the ppc
    + wrappers for the tcnvnode.first_* stuff introduced

  Revision 1.81  2002/07/26 12:28:50  jonas
    * don't always convert the second argument of inc/dec to a longint, but
      to a type based on the first argument

  Revision 1.80  2002/07/25 18:00:19  carl
    + Resulttype for floats is now CPU independent (bestrealytype)
    + Generic version of some routines (call to RTL routines)
        : still untested.

  Revision 1.79  2002/07/20 11:57:54  florian
    * types.pas renamed to defbase.pas because D6 contains a types
      unit so this would conflicts if D6 programms are compiled
    + Willamette/SSE2 instructions to assembler added

  Revision 1.78  2002/07/11 14:41:28  florian
    * start of the new generic parameter handling

  Revision 1.77  2002/06/06 18:53:53  jonas
    * fixed fpu stack overflow in compiler when compiled with -Or

  Revision 1.76  2002/05/18 13:34:10  peter
    * readded missing revisions

  Revision 1.75  2002/05/16 19:46:38  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.73  2002/05/12 16:53:07  peter
    * moved entry and exitcode to ncgutil and cgobj
    * foreach gets extra argument for passing local data to the
      iterator function
    * -CR checks also class typecasts at runtime by changing them
      into as
    * fixed compiler to cycle with the -CR option
    * fixed stabs with elf writer, finally the global variables can
      be watched
    * removed a lot of routines from cga unit and replaced them by
      calls to cgobj
    * u32bit-s32bit updates for and,or,xor nodes. When one element is
      u32bit then the other is typecasted also to u32bit without giving
      a rangecheck warning/error.
    * fixed pascal calling method with reversing also the high tree in
      the parast, detected by tcalcst3 test

  Revision 1.72  2002/04/23 19:16:34  peter
    * add pinline unit that inserts compiler supported functions using
      one or more statements
    * moved finalize and setlength from ninl to pinline

  Revision 1.71  2002/04/02 17:11:29  peter
    * tlocation,treference update
    * LOC_CONSTANT added for better constant handling
    * secondadd splitted in multiple routines
    * location_force_reg added for loading a location to a register
      of a specified size
    * secondassignment parses now first the right and then the left node
      (this is compatible with Kylix). This saves a lot of push/pop especially
      with string operations
    * adapted some routines to use the new cg methods

  Revision 1.70  2002/03/31 20:26:34  jonas
    + a_loadfpu_* and a_loadmm_* methods in tcg
    * register allocation is now handled by a class and is mostly processor
      independent (+rgobj.pas and i386/rgcpu.pas)
    * temp allocation is now handled by a class (+tgobj.pas, -i386\tgcpu.pas)
    * some small improvements and fixes to the optimizer
    * some register allocation fixes
    * some fpuvaroffset fixes in the unary minus node
    * push/popusedregisters is now called rg.save/restoreusedregisters and
      (for i386) uses temps instead of push/pop's when using -Op3 (that code is
      also better optimizable)
    * fixed and optimized register saving/restoring for new/dispose nodes
    * LOC_FPU locations now also require their "register" field to be set to
      R_ST, not R_ST0 (the latter is used for LOC_CFPUREGISTER locations only)
    - list field removed of the tnode class because it's not used currently
      and can cause hard-to-find bugs

  Revision 1.69  2002/01/24 18:25:48  peter
   * implicit result variable generation for assembler routines
   * removed m_tp modeswitch, use m_tp7 or not(m_fpc) instead

  Revision 1.68  2002/01/19 11:53:56  peter
    * constant evaluation for assinged added

}
