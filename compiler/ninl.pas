{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

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

{$i defines.inc}

interface

    uses
       node,htypechk,cpuinfo;

    {$i compinnr.inc}

    type
       tinlinenode = class(tunarynode)
          inlinenumber : byte;
          constructor create(number : byte;is_const:boolean;l : tnode);virtual;
          function getcopy : tnode;override;
          function pass_1 : tnode;override;
          function det_resulttype:tnode;override;
          function docompare(p: tnode): boolean; override;
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
      symbase,symconst,symtype,symdef,symsym,symtable,types,
      pass_1,
      ncal,ncon,ncnv,nadd,nld,nbas,nflw,nmem,
      cpubase,tgcpu,cgbase
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
           (dest.left.resulttype.def.deftype<>stringdef) or
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
              ord(tfloatdef(source.left.resulttype.def).typ),s32bittype),newparas.right);
            { if necessary, insert a fraction parameter }
            if not assigned(fracpara) then
              begin
                tcallparanode(newparas.right).right := ccallparanode.create(
                  cordconstnode.create(-1,s32bittype),tcallparanode(newparas.right).right);
                fracpara := tcallparanode(tcallparanode(newparas.right).right);
              end;
            { if necessary, insert a length para }
            if not assigned(lenpara) then
              fracpara.right := ccallparanode.create(cordconstnode.create(-32767,s32bittype),
                fracpara.right);
          end
        else
          { for a normal parameter, insert a only length parameter if one is missing }
          if not assigned(lenpara) then
            newparas.right := ccallparanode.create(cordconstnode.create(-1,s32bittype),
              newparas.right);

        { remove the parameters from the original node so they won't get disposed, }
        { since they're reused                                                     }
        left := nil;

        { create procedure name }
        procname := 'fpc_' + lower(tstringdef(dest.resulttype.def).stringtypname)+'_';
        if is_real then
          procname := procname + 'float'
        else
          case torddef(source.resulttype.def).typ of
            u32bit:
              procname := procname + 'cardinal';
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
          tfiledef(left.resulttype.def).typedfiletype.def.size,s32bittype),
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
        newstatement := cstatementnode.create(nil,cnothingnode.create);
        newblock := cblocknode.create(newstatement);

        { if we don't have a filepara, create one containing the default }
        if not assigned(filepara) then
          begin

            { create a loadnode for the standard input/output handle }
            if do_read then
              name := 'INPUT'
            else
              name := 'OUTPUT';

            { if we are compiling the system unit, the systemunit symtable is nil. }
            { however, if we aren't compiling the system unit, another unit could  }
            { also have defined the INPUT or OUTPUT symbols. Therefore we need the }
            { separate cases (JM)                                                  }
            if not searchsysvar(name,srsym,tempowner) then
              internalerror(200108141);

            { create the file parameter }
            filepara := ccallparanode.create(cloadnode.create(srsym,tempowner),nil);
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
                newstatement.left := cstatementnode.create(nil,filetemp);
                newstatement := tstatementnode(newstatement.left);

                { make sure the resulttype of the temp (and as such of the }
                { temprefs coming after it) is set (necessary because the  }
                { temprefs will be part of the filepara, of which we need  }
                { the resulttype later on and temprefs can only be         }
                { resulttypepassed if the resulttype of the temp is known) }
                resulttypepass(tnode(filetemp));

                { assign the address of the file to the temp }
                newstatement.left := cstatementnode.create(nil,
                  cassignmentnode.create(ctemprefnode.create(filetemp),
                    caddrnode.create(filepara.left)));
                newstatement := tstatementnode(newstatement.left);
                resulttypepass(newstatement.right);
                { create a new fileparameter as follows: file_type(temp^)    }
                { (so that we pass the value and not the address of the temp }
                { to the read/write routine)                                 }
                nextpara := ccallparanode.create(ctypeconvnode.create(
                  cderefnode.create(ctemprefnode.create(filetemp)),filepara.left.resulttype),nil);
                { make sure the type conversion is explicit, otherwise this }
                { typecast won't work                                       }
                nextpara.left.toggleflag(nf_explizit);

                { replace the old file para with the new one }
                filepara.left := nil;
                filepara.free;
                filepara := nextpara;

                { the resulttype of the filepara must be set since it's }
                { used below                                            }
                filepara.get_paratype;
              end;
          end;

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
              tfiledef(filepara.resulttype.def).typedfiletype.def.size,s32bittype),nil);

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

                if not is_equal(para.left.resulttype.def,tfiledef(filepara.resulttype.def).typedfiletype.def) then
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
                if (para.left.nodetype = calln) then
                  begin
                    { create temp for result }
                    temp := ctempcreatenode.create(para.left.resulttype,
                      para.left.resulttype.def.size,true);
                    newstatement.left := cstatementnode.create(nil,temp);
                    { assign result to temp }
                    newstatement := tstatementnode(newstatement.left);
                    newstatement.left := cstatementnode.create(nil,
                      cassignmentnode.create(ctemprefnode.create(temp),
                      para.left));
                    newstatement := tstatementnode(newstatement.left);
                    { replace (reused) paranode with temp }
                    para.left := ctemprefnode.create(temp);
                  end;
                { add fileparameter }
                para.right := filepara.getcopy;

                { create call statment                                             }
                { since the parameters are in the correct order, we have to insert }
                { the statements always at the end of the current block            }
                newstatement.left := cstatementnode.create(nil,
                  ccallnode.createintern(procprefix,para));
                newstatement := tstatementnode(newstatement.left);

                { if we used a temp, free it }
                if para.left.nodetype = temprefn then
                  begin
                    newstatement.left := cstatementnode.create(nil,
                      ctempdeletenode.create(temp));
                    newstatement := tstatementnode(newstatement.left);
                  end;

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
                      name := procprefix+lower(tstringdef(para.left.resulttype.def).stringtypname);
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
                              lenpara := ccallparanode.create(cordconstnode.create(0,s32bittype),nil)
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
                                cordconstnode.create(-32767,s32bittype),nil);
                            { also create a default fracpara if necessary }
                            if not assigned(fracpara) then
                              fracpara := ccallparanode.create(
                                cordconstnode.create(-1,s32bittype),nil);
                            { add it to the lenpara }
                            lenpara.right := fracpara;
                            { and add the realtype para (this also removes the link }
                            { to any parameters coming after it)                    }
                            fracpara.right := ccallparanode.create(
                                cordconstnode.create(ord(tfloatdef(para.left.resulttype.def).typ),
                                s32bittype),nil);
                          end;
                      end;

                    if do_read and
                      ((is_ordinal and
                        (torddef(para.left.resulttype.def).typ in [s8bit,s16bit,u8bit,u16bit])
                       ) or
                       (is_real and
                        not is_equal(para.left.resulttype.def,pbestrealtype^.def)
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
                        newstatement.left := cstatementnode.create(nil,temp);
                        newstatement := tstatementnode(newstatement.left);

                        { ... and the file }
                        p1 := ccallparanode.create(ctemprefnode.create(temp),
                          filepara.getcopy);

                        { create the call to the helper }
                        newstatement.left := cstatementnode.create(nil,
                          ccallnode.createintern(name,tcallparanode(p1)));
                        newstatement := tstatementnode(newstatement.left);

                        { assign the result to the original var (this automatically }
                        { takes care of range checking)                             }
                        newstatement.left := cstatementnode.create(nil,
                          cassignmentnode.create(para.left,
                           ctemprefnode.create(temp)));
                        newstatement := tstatementnode(newstatement.left);

                        { release the temp location }
                        newstatement.left := cstatementnode.create(nil,
                          ctempdeletenode.create(temp));
                        newstatement := tstatementnode(newstatement.left);

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
                        newstatement.left := cstatementnode.create(nil,
                          ccallnode.createintern(name,para));
                        newstatement := tstatementnode(newstatement.left);
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
                    newstatement.left := ccallnode.createintern('fpc_read_end',filepara);
                  in_write_x:
                    newstatement.left := ccallnode.createintern('fpc_write_end',filepara);
                  in_readln_x:
                    newstatement.left := ccallnode.createintern('fpc_readln_end',filepara);
                  in_writeln_x:
                    newstatement.left := ccallnode.createintern('fpc_writeln_end',filepara);
                end;
                newstatement.left := cstatementnode.create(nil,newstatement.left);
                newstatement := tstatementnode(newstatement.left);
              end;
          end;

          { if we found an error, simply delete the generated blocknode }
          if found_error then
            newblock.free
          else
            begin
              { deallocate the temp for the file para if we used one }
              if assigned(filetemp) then
                begin
                  newstatement.left := cstatementnode.create(nil,
                    ctempdeletenode.create(filetemp));
                  newstatement := tstatementnode(newstatement.left);
                end;
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
        newstatement := cstatementnode.create(nil,cnothingnode.create);
        newblock := cblocknode.create(newstatement);

        { do we need a temp for code? Yes, if no code specified, or if  }
        { code is not a 32bit parameter (we already checked whether the }
        { the code para, if specified, was an orddef)                   }
        if not assigned(codepara) or
           (torddef(codepara.resulttype.def).typ in [u8bit,u16bit,s8bit,s16bit]) then
          begin
            tempcode := ctempcreatenode.create(s32bittype,4,true);
            newstatement.left := cstatementnode.create(nil,tempcode);
            newstatement := tstatementnode(newstatement.left);
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
                      (destpara.resulttype.def.size,s32bittype),nil);
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
          procname := procname + lower(tstringdef(sourcepara.resulttype.def).stringtypname)
        else procname := procname + 'shortstr';

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
        newstatement.left := cstatementnode.create(nil,cassignmentnode.create(
          destpara.left,ccallnode.createintern(procname,newparas)));
        newstatement := tstatementnode(newstatement.left);

        { dispose of the enclosing paranode of the destination }
        destpara.left := nil;
        destpara.right := nil;
        destpara.free;

        { check if we used a temp for code and whether we have to store }
        { it to the real code parameter                                 }
        if assigned(orgcode) then
          begin
            newstatement.left := cstatementnode.create(nil,cassignmentnode.create(
              orgcode,ctemprefnode.create(tempcode)));
            newstatement := tstatementnode(newstatement.left);
          end;

        { release the temp if we allocated one }
        if assigned(tempcode) then
          begin
            newstatement.left := cstatementnode.create(nil,
              ctempdeletenode.create(tempcode));
            newstatement := tstatementnode(newstatement.left);
          end;

        { free the errornode }
        result.free;
        { and return it }
        result := newblock;
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
                  hp:=cordconstnode.create(v,t);
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
         counter   : longint;
         ppn       : tcallparanode;
         dummycoll : tparaitem;
         vl,vl2    : longint;
         vr        : bestreal;
         hp        : tnode;
         srsym     : tsym;
         def       : tdef;
         isreal    : boolean;
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
                               hp:=cordconstnode.create(1,s32bittype)
                            end
                          else
                            hp:=cordconstnode.create(trunc(vr),s32bittype)
                       end
                     else
                      hp:=cordconstnode.create(trunc(vl),s32bittype);
                   end;
                 in_const_round :
                   begin
                     if isreal then
                       begin
                          if (vr>=2147483647.5) or (vr<=-2147483648.5) then
                            begin
                               CGMessage(parser_e_range_check_error);
                               hp:=cordconstnode.create(1,s32bittype)
                            end
                          else
                            hp:=cordconstnode.create(round(vr),s32bittype)
                       end
                     else
                      hp:=cordconstnode.create(round(vl),s32bittype);
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
                      hp:=cordconstnode.create(abs(vl),left.resulttype);
                   end;
                 in_const_sqr :
                   begin
                     if isreal then
                      hp:=crealconstnode.create(sqr(vr),pbestrealtype^)
                     else
                      hp:=cordconstnode.create(sqr(vl),left.resulttype);
                   end;
                 in_const_odd :
                   begin
                     if isreal then
                      CGMessage1(type_e_integer_expr_expected,left.resulttype.def.typename)
                     else
                      hp:=cordconstnode.create(byte(odd(vl)),booltype);
                   end;
                 in_const_swap_word :
                   begin
                     if isreal then
                      CGMessage1(type_e_integer_expr_expected,left.resulttype.def.typename)
                     else
                      hp:=cordconstnode.create((vl and $ff) shl 8+(vl shr 8),left.resulttype);
                   end;
                 in_const_swap_long :
                   begin
                     if isreal then
                      CGMessage(type_e_mismatch)
                     else
                      hp:=cordconstnode.create((vl and $ffff) shl 16+(vl shr 16),left.resulttype);
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
                     ((m_tp in aktmodeswitches) or
                      (m_delphi in aktmodeswitches)) then
                    CGMessage(type_w_maybe_wrong_hi_lo);
                  { constant folding }
                  if left.nodetype=ordconstn then
                   begin
                     case inlinenumber of
                       in_lo_word :
                         hp:=cordconstnode.create(tordconstnode(left).value and $ff,left.resulttype);
                       in_hi_word :
                         hp:=cordconstnode.create(tordconstnode(left).value shr 8,left.resulttype);
                       in_lo_long :
                         hp:=cordconstnode.create(tordconstnode(left).value and $ffff,left.resulttype);
                       in_hi_long :
                         hp:=cordconstnode.create(tordconstnode(left).value shr 16,left.resulttype);
                       in_lo_qword :
                         hp:=cordconstnode.create(tordconstnode(left).value and $ffffffff,left.resulttype);
                       in_hi_qword :
                         hp:=cordconstnode.create(tordconstnode(left).value shr 32,left.resulttype);
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
                      hp:=cordconstnode.create(tordconstnode(left).value,s32bittype);
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
                           hp:=cordconstnode.create(tstringconstnode(left).len,s32bittype);
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
                           hp:=cordconstnode.create(1,s32bittype);
                           result:=hp;
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
                           srsym:=searchsymonlyin(tloadnode(left).symtable,'high'+tvarsym(tloadnode(left).symtableentry).name);
                           hp:=caddnode.create(addn,cloadnode.create(tvarsym(srsym),tloadnode(left).symtable),
                                                    cordconstnode.create(1,s32bittype));
                           result:=hp;
                           goto myexit;
                         end
                        else
                         if not is_dynamic_array(left.resulttype.def) then
                          begin
                            hp:=cordconstnode.create(tarraydef(left.resulttype.def).highrange-
                                                      tarraydef(left.resulttype.def).lowrange+1,
                                                     s32bittype);
                            result:=hp;
                            goto myexit;
                          end
                        else
                          begin
                            { can't use inserttypeconv because we need }
                            { an explicit type conversion (JM)         }
                            hp := ctypeconvnode.create(left,voidpointertype);
                            hp.toggleflag(nf_explizit);
                            hp := ccallparanode.create(hp,nil);
                            result := ccallnode.createintern('fpc_dynarray_length',hp);
                            { make sure the left node doesn't get disposed, since it's }
                            { reused in the new node (JM)                              }
                            left:=nil;
                            resulttypepass(result);
                            exit;
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
                   set_varstate(left,true);
                   resulttype:=booltype;
                end;

              in_ofs_x :
                internalerror(2000101001);

              in_seg_x :
                begin
                  set_varstate(left,false);
                  hp:=cordconstnode.create(0,s32bittype);
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

                   { do constant folding after check for jumps }
                   if left.nodetype=ordconstn then
                    begin
                      if inlinenumber=in_succ_x then
                       hp:=cordconstnode.create(tordconstnode(left).value+1,left.resulttype)
                      else
                       hp:=cordconstnode.create(tordconstnode(left).value-1,left.resulttype);
                      result:=hp;
                    end;
                end;

              in_setlength_x:
                begin
                   resulttype:=voidtype;
                   if assigned(left) then
                     begin
                        ppn:=tcallparanode(left);
                        counter:=0;
                        { check type }
                        while assigned(ppn.right) do
                          begin
                             set_varstate(ppn.left,true);
                             inserttypeconv(ppn.left,s32bittype);
                             inc(counter);
                             ppn:=tcallparanode(ppn.right);
                          end;
                        { last param must be var }
                        valid_for_var(ppn.left);
                        set_varstate(ppn.left,false);
                        { first param must be a string or dynamic array ...}
                        if not((ppn.left.resulttype.def.deftype=stringdef) or
                           (is_dynamic_array(ppn.left.resulttype.def))) then
                          CGMessage(type_e_mismatch);

                        { only dynamic arrays accept more dimensions }
                        if (counter>1) then
                          if (not(is_dynamic_array(ppn.left.resulttype.def))) then
                            CGMessage(type_e_mismatch)
                          else
                            { check if the amount of dimensions is valid }
                            begin
                              def := tarraydef(ppn.left.resulttype.def).elementtype.def;
                              while counter > 1 do
                                begin
                                  if not(is_dynamic_array(def)) then
                                    begin
                                      CGMessage(parser_e_wrong_parameter_size);
                                      break;
                                    end;
                                  dec(counter);
                                  def := tarraydef(def).elementtype.def;
                                end;
                            end;

                       { convert shortstrings to openstring parameters }
                       { (generate the hightree) (JM)                  }
                       if (ppn.left.resulttype.def.deftype = stringdef) and
                          (tstringdef(ppn.left.resulttype.def).string_typ =
                            st_shortstring) then
                         begin
                           dummycoll:=tparaitem.create;
                           dummycoll.paratyp:=vs_var;
                           dummycoll.paratype:=openshortstringtype;
                           tcallparanode(ppn).insert_typeconv(dummycoll,false);
                           dummycoll.destroy;
                         end;
                     end
                   else
                     CGMessage(type_e_mismatch);
                end;

              in_finalize_x:
                begin
                   resulttype:=voidtype;
                   if assigned(left) and assigned(tcallparanode(left).left) then
                     begin
                        { first param must be var }
                        valid_for_var(tcallparanode(left).left);
                        set_varstate(tcallparanode(left).left,true);

                        { two parameters?, the last parameter must be a longint }
                        if assigned(tcallparanode(left).right) then
                         inserttypeconv(tcallparanode(tcallparanode(left).right).left,s32bittype);
                     end
                   else
                     CGMessage(type_e_mismatch);
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
                          { two paras ? }
                          if assigned(tcallparanode(left).right) then
                           begin
                             { insert a type conversion       }
                             { the second param is always longint }
                             inserttypeconv(tcallparanode(tcallparanode(left).right).left,s32bittype);

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
                  hp:=ccallparanode.create(cordconstnode.create(tcallparanode(left).left.resulttype.def.size,s32bittype),left);
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
                           result:=cordconstnode.create(tarraydef(left.resulttype.def).lowrange,tarraydef(left.resulttype.def).rangetype);
                         end
                        else
                         begin
                           if is_open_array(left.resulttype.def) or
                             is_array_of_const(left.resulttype.def) then
                            begin
                              srsym:=searchsymonlyin(tloadnode(left).symtable,'high'+tvarsym(tloadnode(left).symtableentry).name);
                              result:=cloadnode.create(tvarsym(srsym),tloadnode(left).symtable);
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
                                resulttypepass(result);
                              end
                           else
                            begin
                              result:=cordconstnode.create(tarraydef(left.resulttype.def).highrange,tarraydef(left.resulttype.def).rangetype);
                            end;
                         end;
                         resulttypepass(result);
                      end;
                    stringdef:
                      begin
                        if inlinenumber=in_low_x then
                         begin
                           hp:=cordconstnode.create(0,u8bittype);
                           resulttypepass(hp);
                           result:=hp;
                         end
                        else
                         begin
                           if is_open_string(left.resulttype.def) then
                            begin
                              srsym:=searchsymonlyin(tloadnode(left).symtable,'high'+tvarsym(tloadnode(left).symtableentry).name);
                              hp:=cloadnode.create(tvarsym(srsym),tloadnode(left).symtable);
                              resulttypepass(hp);
                              result:=hp;
                            end
                           else
                            begin
                              hp:=cordconstnode.create(tstringdef(left.resulttype.def).len,u8bittype);
                              resulttypepass(hp);
                              result:=hp;
                            end;
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
                    resulttype:=s80floattype;
                end;

              in_cos_extended :
                begin
                  if left.nodetype in [ordconstn,realconstn] then
                   setconstrealvalue(cos(getconstrealvalue))
                  else
                   begin
                     set_varstate(left,true);
                     inserttypeconv(left,s80floattype);
                     resulttype:=s80floattype;
                   end;
                end;

              in_sin_extended :
                begin
                  if left.nodetype in [ordconstn,realconstn] then
                   setconstrealvalue(sin(getconstrealvalue))
                  else
                   begin
                     set_varstate(left,true);
                     inserttypeconv(left,s80floattype);
                     resulttype:=s80floattype;
                   end;
                end;

              in_arctan_extended :
                begin
                  if left.nodetype in [ordconstn,realconstn] then
                   setconstrealvalue(arctan(getconstrealvalue))
                  else
                   begin
                     set_varstate(left,true);
                     inserttypeconv(left,s80floattype);
                     resulttype:=s80floattype;
                   end;
                end;

              in_abs_extended :
                begin
                  if left.nodetype in [ordconstn,realconstn] then
                   setconstrealvalue(abs(getconstrealvalue))
                  else
                   begin
                     set_varstate(left,true);
                     inserttypeconv(left,s80floattype);
                     resulttype:=s80floattype;
                   end;
                end;

              in_sqr_extended :
                begin
                  if left.nodetype in [ordconstn,realconstn] then
                   setconstrealvalue(sqr(getconstrealvalue))
                  else
                   begin
                     set_varstate(left,true);
                     inserttypeconv(left,s80floattype);
                     resulttype:=s80floattype;
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
                     inserttypeconv(left,s80floattype);
                     resulttype:=s80floattype;
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
                     inserttypeconv(left,s80floattype);
                     resulttype:=s80floattype;
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


{$ifdef fpc}
{$maxfpuregisters 0}
{$endif fpc}


    function tinlinenode.pass_1 : tnode;
      var
         srsym   : tsym;
         hp,hpp  : tnode;

      begin
         result:=nil;
         { if we handle writeln; left contains no valid address }
         if assigned(left) then
           begin
              if left.nodetype=callparan then
                tcallparanode(left).firstcallparan(nil,false)
              else
                firstpass(left);
              left_max;
              set_location(location,left.location);
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
              if registers32<1 then
                registers32:=1;
              location.loc:=LOC_REGISTER;
            end;

          in_sizeof_x:
            begin
              if push_high_param(left.resulttype.def) then
               begin
                 srsym:=searchsymonlyin(tloadnode(left).symtable,'high'+tvarsym(tloadnode(left).symtableentry).name);
                 hp:=caddnode.create(addn,cloadnode.create(tvarsym(srsym),tloadnode(left).symtable),
                                  cordconstnode.create(1,s32bittype));
                 if (left.resulttype.def.deftype=arraydef) and
                    (tarraydef(left.resulttype.def).elesize<>1) then
                   hp:=caddnode.create(muln,hp,cordconstnode.create(tarraydef(left.resulttype.def).elesize,s32bittype));
                 firstpass(hp);
                 result:=hp;
               end
              else
               begin
                 if registers32<1 then
                    registers32:=1;
                 location.loc:=LOC_REGISTER;
               end;
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
               location.loc:=LOC_FLAGS;
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
                     hpp := cordconstnode.create(1,s32bittype);
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
                           (tcallparanode(tcallparanode(left).right).left.location.loc in [LOC_MEM,LOC_REFERENCE]) and
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
             location.loc:=LOC_FPU;
             registers32:=left.registers32;
             registersfpu:=max(left.registersfpu,1);
{$ifdef SUPPORT_MMX}
             registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}
           end;

         in_sin_extended:
           begin
             location.loc:=LOC_FPU;
             registers32:=left.registers32;
             registersfpu:=max(left.registersfpu,1);
{$ifdef SUPPORT_MMX}
             registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}
           end;

         in_arctan_extended:
           begin
             location.loc:=LOC_FPU;
             registers32:=left.registers32;
             registersfpu:=max(left.registersfpu,2);
{$ifdef SUPPORT_MMX}
             registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}
           end;

         in_pi:
           begin
             location.loc:=LOC_FPU;
             registersfpu:=1;
           end;

         in_abs_extended:
           begin
             location.loc:=LOC_FPU;
             registers32:=left.registers32;
             registersfpu:=max(left.registersfpu,1);
{$ifdef SUPPORT_MMX}
             registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}
           end;

         in_sqr_extended:
           begin
             location.loc:=LOC_FPU;
             registers32:=left.registers32;
             registersfpu:=max(left.registersfpu,1);
{$ifdef SUPPORT_MMX}
             registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}
           end;

         in_sqrt_extended:
           begin
             location.loc:=LOC_FPU;
             registers32:=left.registers32;
             registersfpu:=max(left.registersfpu,1);
{$ifdef SUPPORT_MMX}
             registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}
           end;

         in_ln_extended:
           begin
             location.loc:=LOC_FPU;
             registers32:=left.registers32;
             registersfpu:=max(left.registersfpu,2);
{$ifdef SUPPORT_MMX}
             registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}
           end;

{$ifdef SUPPORT_MMX}
         in_mmx_pcmpeqb..in_mmx_pcmpgtw:
           begin
           end;
{$endif SUPPORT_MMX}

         in_assert_x_y :
            begin
              { We've checked the whole statement for correctness, now we
                can remove it if assertions are off }
              if not(cs_do_assertion in aktlocalswitches) then
               begin
                 { we need a valid node, so insert a nothingn }
                 result:=cnothingnode.create;
               end
              else
               begin
                 registers32:=left.registers32;
                 registersfpu:=left.registersfpu;
{$ifdef SUPPORT_MMX}
                 registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}
               end;
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


begin
   cinlinenode:=tinlinenode;
end.
{
  $Log$
  Revision 1.64  2001-12-03 14:21:34  jonas
    * fixed web bug 1693 (dynarray support for length)

  Revision 1.63  2001/10/24 16:17:36  jonas
    * fixed web bug 1621 (write(typed_file,function_call) works again)
    * allow write(typed_file,procvar_call) too (it was already allowed for
      text file writes)

  Revision 1.62  2001/09/30 16:16:28  jonas
    - removed unused units form uses-clause and unused local vars

  Revision 1.61  2001/09/24 16:09:55  jonas
    * check if amount of dimensions passed to setlength for dynamic arrays
      is correct

  Revision 1.60  2001/09/24 11:35:55  jonas
    * fix from Pavel V. Ozersk to accept multiple dimensions for setlength
      and dynamical arrays

  Revision 1.59  2001/09/17 21:29:12  peter
    * merged netbsd, fpu-overflow from fixes branch

  Revision 1.58  2001/09/05 15:19:43  jonas
    * the result of high/low nodes wasn't always resulttypepassed

  Revision 1.57  2001/09/04 14:32:45  jonas
    * simplified det_resulttype code for include/exclude
    * include/exclude doesn't use any helpers anymore in the i386 secondpass

  Revision 1.56  2001/09/04 11:38:55  jonas
    + searchsystype() and searchsystype() functions in symtable
    * changed ninl and nadd to use these functions
    * i386 set comparison functions now return their results in al instead
      of in the flags so that they can be sued as compilerprocs
    - removed all processor specific code from n386add.pas that has to do
      with set handling, it's now all done in nadd.pas
    * fixed fpc_set_contains_sets in genset.inc
    * fpc_set_in_byte is now coded inline in n386set.pas and doesn't use a
      helper anymore
    * some small fixes in compproc.inc/set.inc regarding the declaration of
      internal helper types (fpc_small_set and fpc_normal_set)

  Revision 1.55  2001/09/02 21:12:07  peter
    * move class of definitions into type section for delphi

  Revision 1.54  2001/08/28 13:24:46  jonas
    + compilerproc implementation of most string-related type conversions
    - removed all code from the compiler which has been replaced by
      compilerproc implementations (using $ifdef hascompilerproc is not
      necessary in the compiler)

  Revision 1.53  2001/08/27 11:04:41  jonas
    * avoid nonsense range error when using cardinal with value
      > high(longint) as code para with val()

  Revision 1.52  2001/08/26 13:36:40  florian
    * some cg reorganisation
    * some PPC updates

  Revision 1.51  2001/08/24 13:47:27  jonas
    * moved "reverseparameters" from ninl.pas to ncal.pas
    + support for non-persistent temps in ttempcreatenode.create, for use
      with typeconversion nodes

  Revision 1.50  2001/08/24 12:33:54  jonas
    * fixed big bug in handle_str that caused it to (almost) always call
      fpc_<stringtype>_longint
    * fixed small bug in handle_read_write that caused wrong warnigns about
      uninitialized vars with read(ln)
    + handle_val (processor independent val() handling)

  Revision 1.49  2001/08/23 14:28:35  jonas
    + tempcreate/ref/delete nodes (allows the use of temps in the
      resulttype and first pass)
    * made handling of read(ln)/write(ln) processor independent
    * moved processor independent handling for str and reset/rewrite-typed
      from firstpass to resulttype pass
    * changed names of helpers in text.inc to be generic for use as
      compilerprocs + added "iocheck" directive for most of them
    * reading of ordinals is done by procedures instead of functions
      because otherwise FPC_IOCHECK overwrote the result before it could
      be stored elsewhere (range checking still works)
    * compilerprocs can now be used in the system unit before they are
      implemented
    * added note to errore.msg that booleans can't be read using read/readln

  Revision 1.48  2001/08/13 15:39:52  jonas
    * made in_reset_typedfile/in_rewrite_typedfile handling processor
      independent

  Revision 1.47  2001/08/13 12:41:57  jonas
    * made code for str(x,y) completely processor independent

  Revision 1.46  2001/08/06 12:47:31  jonas
    * parameters to FPC_TYPED_WRITE can't be regvars (merged)

  Revision 1.45  2001/08/06 09:44:10  jonas
    + support for high(dynarray) using compilerproc (forgot to commit
      previously)

  Revision 1.44  2001/07/09 21:15:40  peter
    * Length made internal
    * Add array support for Length

  Revision 1.43  2001/07/08 21:00:15  peter
    * various widestring updates, it works now mostly without charset
      mapping supported

  Revision 1.42  2001/06/04 11:48:01  peter
    * better const to var checking

  Revision 1.41  2001/06/03 20:12:53  peter
    * changed int64($ffffffff) that is buggy under 1.0.x to expression
      with a shl

  Revision 1.40  2001/05/06 17:16:43  jonas
    + added warning about missing implementation for high(dynamic_array)

  Revision 1.39  2001/04/26 21:57:05  peter
    * moved code from firstpass to det_resulttype and remove extraneous
      calls to firstcallparan for in_str,in_write,in_val

  Revision 1.38  2001/04/21 12:03:11  peter
    * m68k updates merged from fixes branch

  Revision 1.37  2001/04/13 22:22:30  peter
    * call set_varstate for setlength
    * ptr returns pointerconstnode instead of ordconstnode

  Revision 1.36  2001/04/13 01:22:09  peter
    * symtable change to classes
    * range check generation and errors fixed, make cycle DEBUG=1 works
    * memory leaks fixed

  Revision 1.35  2001/04/05 21:02:13  peter
    * fixed fpu inline functions typeconvs

  Revision 1.34  2001/04/04 22:42:40  peter
    * move constant folding into det_resulttype

  Revision 1.33  2001/04/04 21:30:43  florian
    * applied several fixes to get the DD8 Delphi Unit compiled
     e.g. "forward"-interfaces are working now

  Revision 1.32  2001/04/02 21:20:31  peter
    * resulttype rewrite

  Revision 1.31  2001/03/23 00:16:07  florian
    + some stuff to compile FreeCLX added

  Revision 1.30  2001/03/12 12:47:46  michael
  + Patches from peter

  Revision 1.29  2001/03/03 12:38:08  jonas
    * fixed low() for signed types < 64bit

  Revision 1.28  2001/02/26 19:44:53  peter
    * merged generic m68k updates from fixes branch

  Revision 1.27  2001/02/22 11:24:40  jonas
    * fixed bug in previous fix (hopped over revision 1.26 because that one
      also removed the fix for high(cardinal))

  Revision 1.26  2001/02/21 20:50:59  peter
    * fix to compile again, but high(cardinal) with $R+ still fails!

  Revision 1.25  2001/02/21 12:57:46  jonas
    * fixed high/low for cardinal, int64 and qword

  Revision 1.24  2001/01/06 19:54:11  peter
    * merged fix for 1310

  Revision 1.23  2001/01/06 18:28:39  peter
    * fixed wrong notes about locals

  Revision 1.22  2000/12/31 11:14:10  jonas
    + implemented/fixed docompare() mathods for all nodes (not tested)
    + nopt.pas, nadd.pas, i386/n386opt.pas: optimized nodes for adding strings
      and constant strings/chars together
    * n386add.pas: don't copy temp strings (of size 256) to another temp string
      when adding

  Revision 1.21  2000/12/25 00:07:26  peter
    + new tlinkedlist class (merge of old tstringqueue,tcontainer and
      tlinkedlist objects)

  Revision 1.20  2000/12/17 14:35:41  peter
    * fixed crash with val()

  Revision 1.19  2000/11/29 00:30:33  florian
    * unused units removed from uses clause
    * some changes for widestrings

  Revision 1.18  2000/11/12 15:27:22  jonas
    * also don't do conversion for chars/booleans (hopefully final change :/)

  Revision 1.17  2000/11/11 21:08:13  jonas
    * don't do inc/dec to add/sub conversion for enums

  Revision 1.16  2000/11/11 16:18:35  peter
    * ptr returns farpointer

  Revision 1.15  2000/11/11 15:59:07  jonas
    * convert inc/dec to add/sub when range/overflow checking is on

  Revision 1.14  2000/11/09 17:46:54  florian
    * System.TypeInfo fixed
    + System.Finalize implemented
    + some new keywords for interface support added

  Revision 1.13  2000/11/04 16:48:32  florian
    * innr.inc renamed to make compiler compilation easier because the rtl contains
      a file of the same name

  Revision 1.12  2000/10/31 22:02:48  peter
    * symtable splitted, no real code changes

  Revision 1.11  2000/10/26 14:15:06  jonas
    * fixed setlength for shortstrings

  Revision 1.10  2000/10/21 18:16:11  florian
    * a lot of changes:
       - basic dyn. array support
       - basic C++ support
       - some work for interfaces done
       ....

  Revision 1.9  2000/10/15 08:38:46  jonas
    * added missing getcopy for previous addition

  Revision 1.8  2000/10/14 18:27:53  jonas
    * merged fix for inc/dec on 64bit types from tcinl

  Revision 1.7  2000/10/14 10:14:50  peter
    * moehrendorf oct 2000 rewrite

  Revision 1.6  2000/10/01 19:48:24  peter
    * lot of compile updates for cg11

  Revision 1.5  2000/09/28 19:49:52  florian
  *** empty log message ***

  Revision 1.4  2000/09/28 16:34:47  florian
  *** empty log message ***

  Revision 1.3  2000/09/27 21:33:22  florian
    * finally nadd.pas compiles

  Revision 1.2  2000/09/27 20:25:44  florian
    * more stuff fixed

  Revision 1.1  2000/09/26 14:59:34  florian
    * more conversion work done

}
