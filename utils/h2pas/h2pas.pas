
(* Yacc parser template (TP Yacc V3.0), V1.2 6-17-91 AG *)

(* global definitions: *)

program h2pas;

(*
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

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

 ****************************************************************************)



  uses
   {$ifdef go32v2}
   {$ifndef NOEXCP}
   dpmiexcp,
   {$endif NOEXCP}
   {$endif}
   {$IFDEF WIN32}
   SysUtils,
   {$else}
   strings,
   {$endif}
   options,scan,converu,lexlib,yacclib;

  type
     YYSTYPE = presobject;

  const
     INT_STR = 'longint';
     UINT_STR = 'cardinal';
     SHORT_STR = 'integer';
     USHORT_STR = 'word';
     CHAR_STR = 'char';
     { should we use byte or char for 'unsigned char' ?? }
     UCHAR_STR = 'byte';
     REAL_STR = 'real';

  var
     debug : boolean;
     hp,ph : presobject;
     extfile: text;  (* file for implementation headers extern procs *)
     IsExtern:boolean;
     must_write_packed_field : boolean;
     tempfile : text;
     No_pop:boolean;
     s,TN,PN : String;

(* $ define yydebug
 compile with -dYYDEBUG to get debugging info *)

  const
     (* number of a?b:c construction in one define *)
     if_nb : longint = 0;
     is_packed : boolean = false;
     is_procvar : boolean = false;

  var space_array : array [0..255] of byte;
      space_index : byte;

        procedure shift(space_number : byte);
          var
             i : byte;
          begin
             space_array[space_index]:=space_number;
             inc(space_index);
             for i:=1 to space_number do
               aktspace:=aktspace+' ';
          end;

        procedure popshift;
          begin
             dec(space_index);
             if space_index<0 then
               internalerror(20);
             dec(byte(aktspace[0]),space_array[space_index]);
          end;

    function str(i : longint) : string;
      var
         s : string;
      begin
         system.str(i,s);
         str:=s;
      end;

    function hexstr(i : cardinal) : string;

    const
      HexTbl : array[0..15] of char='0123456789ABCDEF';
    var
      str : string;
    begin
      str:='';
      while i<>0 do
        begin
           str:=hextbl[i and $F]+str;
           i:=i shr 4;
        end;
      if str='' then str:='0';
      hexstr:='$'+str;
    end;

    function uppercase(s : string) : string;
      var
         i : byte;
      begin
         for i:=1 to length(s) do
           s[i]:=UpCase(s[i]);
         uppercase:=s;
      end;

    procedure write_type_specifier(var outfile:text; p : presobject);forward;
    procedure write_p_a_def(var outfile:text; p,simple_type : presobject);forward;
    procedure write_ifexpr(var outfile:text; p : presobject);forward;
    procedure write_funexpr(var outfile:text; p : presobject);forward;

    procedure yymsg(const msg : string);
      begin
         writeln('line ',line_no,': ',msg);
      end;

    procedure write_packed_fields_info(var outfile:text; p : presobject; ph : string);

      var
         hp1,hp2,hp3 : presobject;
         is_sized : boolean;
         line : string;
         flag_index : longint;
         name : pchar;
         ps : byte;

      begin
         { write out the tempfile created }
         close(tempfile);
         reset(tempfile);
         is_sized:=false;
         flag_index:=0;
         writeln(outfile,aktspace,'const');
         shift(3);
         while not eof(tempfile) do
           begin
              readln(tempfile,line);
              ps:=pos('&',line);
              if ps>0 then
                line:=copy(line,1,ps-1)+ph+'_'+copy(line,ps+1,255);
              writeln(outfile,aktspace,line);
           end;
         close(tempfile);
         rewrite(tempfile);
         popshift;
         (* walk through all members *)
         hp1 := p^.p1;
         while assigned(hp1) do
           begin
              (* hp2 is t_memberdec *)
              hp2:=hp1^.p1;
              (*  hp3 is t_declist *)
              hp3:=hp2^.p2;
              while assigned(hp3) do
                begin
                   if assigned(hp3^.p1^.p3) and
                      (hp3^.p1^.p3^.typ = t_size_specifier) then
                     begin
                        is_sized:=true;
                        name:=hp3^.p1^.p2^.p;
                        { get function in interface }
                        write(outfile,aktspace,'function ',name);
                        write(outfile,'(var a : ',ph,') : ');
                        shift(2);
                        write_p_a_def(outfile,hp3^.p1^.p1,hp2^.p1);
                        writeln(outfile,';');
                        popshift;
                        { get function in implementation }
                        write(extfile,aktspace,'function ',name);
                        write(extfile,'(var a : ',ph,') : ');
                        shift(2);
                        write_p_a_def(extfile,hp3^.p1^.p1,hp2^.p1);
                        writeln(extfile,';');
                        writeln(extfile,aktspace,'begin');
                        shift(3);
                        write(extfile,aktspace,name,':=(a.flag',flag_index);
                        writeln(extfile,' and bm_',ph,'_',name,') shr bp_',ph,'_',name,';');
                        popshift;
                        writeln(extfile,aktspace,'end;');
                        popshift;
                        writeln(extfile);
                        { set function in interface }
                        write(outfile,aktspace,'procedure set_',name);
                        write(outfile,'(var a : ',ph,'; __',name,' : ');
                        shift(2);
                        write_p_a_def(outfile,hp3^.p1^.p1,hp2^.p1);
                        writeln(outfile,');');
                        popshift;
                        { set function in implementation }
                        write(extfile,aktspace,'procedure set_',name);
                        write(extfile,'(var a : ',ph,'; __',name,' : ');
                        shift(2);
                        write_p_a_def(extfile,hp3^.p1^.p1,hp2^.p1);
                        writeln(extfile,');');
                        writeln(extfile,aktspace,'begin');
                        shift(3);
                        write(extfile,aktspace,'a.flag',flag_index,':=');
                        write(extfile,'a.flag',flag_index,' or ');
                        writeln(extfile,'((__',name,' shl bp_',ph,'_',name,') and bm_',ph,'_',name,');');
                        popshift;
                        writeln(extfile,aktspace,'end;');
                        popshift;
                        writeln(extfile);
                     end
                   else if is_sized then
                     begin
                        is_sized:=false;
                        inc(flag_index);
                     end;
                   hp3:=hp3^.next;
                end;
              hp1:=hp1^.next;
           end;
         must_write_packed_field:=false;
         block_type:=bt_no;
      end;

    procedure write_expr(var outfile:text; p : presobject);
      begin
      if assigned(p) then
        begin
         case p^.typ of
            t_id,t_ifexpr : write(outfile,p^.p);
            t_funexprlist : write_funexpr(outfile,p);
            t_preop : begin
                         write(outfile,p^.p,'(');
                         write_expr(outfile,p^.p1);
                         write(outfile,')');
                         flush(outfile);
                      end;
            t_typespec : begin
                         write_type_specifier(outfile,p^.p1);
                         write(outfile,'(');
                         write_expr(outfile,p^.p2);
                         write(outfile,')');
                         flush(outfile);
                      end;
            t_bop : begin
                       if p^.p1^.typ<>t_id then
                         write(outfile,'(');
                       write_expr(outfile,p^.p1);
                       if p^.p1^.typ<>t_id then
                       write(outfile,')');
                       write(outfile,p^.p);
                       if p^.p2^.typ<>t_id then
                         write(outfile,'(');
                       write_expr(outfile,p^.p2);
                       if p^.p2^.typ<>t_id then
                         write(outfile,')');
                    flush(outfile);
                    end;
            else internalerror(2);
            end;
         end;
      end;

    procedure write_ifexpr(var outfile:text; p : presobject);
      begin
         flush(outfile);
         write(outfile,'if ');
         write_expr(outfile,p^.p1);
         writeln(outfile,' then');
         write(outfile,aktspace,'  ');
         write(outfile,p^.p);
         write(outfile,':=');
         write_expr(outfile,p^.p2);
         writeln(outfile);
         writeln(outfile,aktspace,'else');
         write(outfile,aktspace,'  ');
         write(outfile,p^.p);
         write(outfile,':=');
         write_expr(outfile,p^.p3);
         writeln(outfile,';');
         write(outfile,aktspace);
         flush(outfile);
      end;

    procedure write_all_ifexpr(var outfile:text; p : presobject);
      begin
      if assigned(p) then
        begin
           case p^.typ of
             t_id :;
             t_preop :
               write_all_ifexpr(outfile,p^.p1);
             t_bop :
               begin
                  write_all_ifexpr(outfile,p^.p1);
                  write_all_ifexpr(outfile,p^.p2);
               end;
             t_ifexpr :
               begin
                  write_all_ifexpr(outfile,p^.p1);
                  write_all_ifexpr(outfile,p^.p2);
                  write_all_ifexpr(outfile,p^.p3);
                  write_ifexpr(outfile,p);
               end;
             t_typespec :
                  write_all_ifexpr(outfile,p^.p2);
             t_funexprlist,
             t_exprlist :
               begin
                 if assigned(p^.p1) then
                   write_all_ifexpr(outfile,p^.p1);
                 if assigned(p^.next) then
                   write_all_ifexpr(outfile,p^.next);
               end
             else
               internalerror(6);
           end;
        end;
      end;

    procedure write_funexpr(var outfile:text; p : presobject);
      var
         i : longint;

      begin
      if assigned(p) then
        begin
           case p^.typ of
             t_ifexpr :
               write(outfile,p^.p);
             t_exprlist :
               begin
                  write_expr(outfile,p^.p1);
                  if assigned(p^.next) then
                    begin
                      write(outfile,',');
                      write_funexpr(outfile,p^.next);
                    end
               end;
             t_funcname :
               begin
                  shift(2);
                  if if_nb>0 then
                    begin
                       writeln(outfile,aktspace,'var');
                       write(outfile,aktspace,'   ');
                       for i:=1 to if_nb do
                         begin
                            write(outfile,'if_local',i);
                            if i<if_nb then
                              write(outfile,', ')
                            else
                              writeln(outfile,' : longint;');
                         end;
                       writeln(outfile,aktspace,'(* result types are not known *)');
                       if_nb:=0;
                    end;
                  writeln(outfile,aktspace,'begin');
                  shift(3);
                  write(outfile,aktspace);
                  write_all_ifexpr(outfile,p^.p2);
                  write_expr(outfile,p^.p1);
                  write(outfile,':=');
                  write_funexpr(outfile,p^.p2);
                  writeln(outfile,';');
                  popshift;
                  writeln(outfile,aktspace,'end;');
                  popshift;
                  flush(outfile);
               end;
             t_funexprlist :
               begin
                  if assigned(p^.p3) then
                    begin
                       write_type_specifier(outfile,p^.p3);
                       write(outfile,'(');
                    end;
                  if assigned(p^.p1) then
                    write_funexpr(outfile,p^.p1);
                  if assigned(p^.p2) then
                    begin
                      write(outfile,'(');
                      write_funexpr(outfile,p^.p2);
                      write(outfile,')');
                    end;
                  if assigned(p^.p3) then
                    write(outfile,')');
               end
             else internalerror(5);
           end;
        end;
      end;

     function ellipsisarg : presobject;
       begin
          ellipsisarg:=new(presobject,init_two(t_arg,nil,nil));
       end;

    const
       (* if in args *dname is replaced by pdname *)
       in_args : boolean = false;
       typedef_level : longint = 0;

    (* writes an argument list, where p is t_arglist *)

    procedure write_args(var outfile:text; p : presobject);
      var
         length,para : longint;
         old_in_args : boolean;
         varpara : boolean;

      begin
         para:=1;
         length:=0;
         old_in_args:=in_args;
         in_args:=true;
         write(outfile,'(');
         shift(2);

         (* walk through all arguments *)
         (* p must be of type t_arglist *)
         while assigned(p) do
           begin
              if p^.typ<>t_arglist then
                internalerror(10);
              (* is ellipsis ? *)
              if not assigned(p^.p1^.p1) and
                 not assigned(p^.p1^.next) then
                begin
                   { write(outfile,'...'); }
                   write(outfile,'args:array of const');
                   { if variable number of args we must allways pop }
                   no_pop:=false;
                end
              (* we need to correct this in the pp file after *)
              else
                begin
                   (* generate a call by reference parameter ?       *)
                   varpara:=usevarparas and assigned(p^.p1^.p2^.p1) and
                     ((p^.p1^.p2^.p1^.typ=t_pointerdef) or
                     (p^.p1^.p2^.p1^.typ=t_addrdef));
                   (* do not do it for char pointer !!               *)
                   (* para : pchar; and var para : char; are         *)
                   (* completely different in pascal                 *)
                   (* here we exclude all typename containing char   *)
                   (* is this a good method ??                       *)


                   if varpara and
                      (p^.p1^.p2^.p1^.typ=t_pointerdef) and
                      (p^.p1^.p2^.p1^.p1^.typ=t_id) and
                     (pos('CHAR',uppercase(p^.p1^.p2^.p1^.p1^.str))<>0) then
                     varpara:=false;
                   if varpara then
                     begin
                        write(outfile,'var ');
                        length:=length+4;
                     end;

                   (* write new type name *)
                   if assigned(p^.p1^.p2^.p2) then
                     begin
                        write(outfile,p^.p1^.p2^.p2^.p);
                        length:=length+p^.p1^.p2^.p2^.strlength;
                     end
                   else
                     begin
                        write(outfile,'_para',para);
                        { not exact but unimportant }
                        length:=length+6;
                     end;
                   write(outfile,':');
                   if varpara then
                     write_p_a_def(outfile,p^.p1^.p2^.p1^.p1,p^.p1^.p1)
                   else
                     write_p_a_def(outfile,p^.p1^.p2^.p1,p^.p1^.p1);

                end;
              p:=p^.next;
              if assigned(p) then
                begin
                   write(outfile,'; ');
                   { if length>40 then : too complicated to compute }
                   if (para mod 5) = 0 then
                     begin
                        writeln(outfile);
                        write(outfile,aktspace);
                     end;
                end;
              inc(para);
           end;
         write(outfile,')');
         flush(outfile);
         in_args:=old_in_args;
         popshift;
      end;

    procedure write_p_a_def(var outfile:text; p,simple_type : presobject);
      var
         i : longint;
         error : integer;
         constant : boolean;

      begin
         if not(assigned(p)) then
           begin
              write_type_specifier(outfile,simple_type);
              exit;
           end;
         case p^.typ of
            t_pointerdef : begin
                              (* procedure variable ? *)
                              if assigned(p^.p1) and (p^.p1^.typ=t_procdef) then
                                begin
                                   is_procvar:=true;
                                   (* distinguish between procedure and function *)
                                   if (simple_type^.typ=t_void) and (p^.p1^.p1=nil) then
                                     begin
                                        write(outfile,'procedure ');

                                        shift(10);
                                        (* write arguments *)
                                        if assigned(p^.p1^.p2) then
                                          write_args(outfile,p^.p1^.p2);
                                        flush(outfile);
                                        popshift;
                                     end
                                   else
                                     begin
                                        write(outfile,'function ');
                                        shift(9);
                                        (* write arguments *)
                                        if assigned(p^.p1^.p2) then
                                          write_args(outfile,p^.p1^.p2);
                                        write(outfile,':');
                                        flush(outfile);
                                        write_p_a_def(outfile,p^.p1^.p1,simple_type);
                                        popshift;
                                     end
                                end
                              else
                                begin
                                   (* generate "pointer" ? *)
                                   if (simple_type^.typ=t_void) and (p^.p1=nil) then
                                      begin
                                       write(outfile,'pointer');
                                       flush(outfile);
                                      end
                                   else
                                     begin
                                        if in_args then
                                          write(outfile,'p')
                                        else
                                          write(outfile,'^');
                                        flush(outfile);
                                        write_p_a_def(outfile,p^.p1,simple_type);
                                     end;
                                end;
                           end;
            t_arraydef : begin
                             constant:=false;
                             if p^.p2^.typ=t_id then
                               begin
                                  val(p^.p2^.str,i,error);
                                  if error=0 then
                                    begin
                                       dec(i);
                                       constant:=true;
                                    end;
                               end;
                             if not constant then
                               begin
                                  write(outfile,'array[0..(');
                                  write_expr(outfile,p^.p2);
                                  write(outfile,')-1] of ');
                               end
                             else
                               begin
                                  write(outfile,'array[0..',i,'] of ');
                               end;
                             flush(outfile);
                             write_p_a_def(outfile,p^.p1,simple_type);
                          end;
            else internalerror(1);
         end;
      end;

    procedure write_type_specifier(var outfile:text; p : presobject);
      var
         hp1,hp2,hp3,lastexpr : presobject;
         i,l,w : longint;
         error : integer;
         mask : cardinal;
         flag_index,current_power : longint;
         current_level : byte;
         is_sized : boolean;

      begin
         case p^.typ of
            t_id :
              write(outfile,p^.p);
            { what can we do with void defs  ? }
            t_void :
              write(outfile,'void');
            t_pointerdef :
              begin
                 write(outfile,'p');
                 write_type_specifier(outfile,p^.p1);
              end;
            t_enumdef :
              begin
                 if (typedef_level>1) and (p^.p1=nil) and
                    (p^.p2^.typ=t_id) then
                   begin
                      write(outfile,p^.p2^.p);
                   end
                 else
                 if not EnumToConst then
                   begin
                      write(outfile,'(');
                      hp1:=p^.p1;
                      w:=length(aktspace);
                      while assigned(hp1) do
                        begin
                           write(outfile,hp1^.p1^.p);
                           if assigned(hp1^.p2) then
                             begin
                                write(outfile,' := ');
                                write_expr(outfile,hp1^.p2);
                                w:=w+6;(* strlen(hp1^.p); *)
                             end;
                           w:=w+length(hp1^.p1^.str);
                           hp1:=hp1^.next;
                           if assigned(hp1) then
                             write(outfile,',');
                           if w>40 then
                             begin
                                 writeln(outfile);
                                 write(outfile,aktspace);
                                 w:=length(aktspace);
                             end;
                           flush(outfile);
                        end;
                      write(outfile,')');
                      flush(outfile);
                   end
                 else
                   begin
                      Writeln (outfile,' Longint;');
                      hp1:=p^.p1;
                      l:=0;
                      lastexpr:=nil;
                      Writeln (outfile,aktspace,'Const');
                      while assigned(hp1) do
                        begin
                           write (outfile,aktspace,hp1^.p1^.p,' = ');
                           if assigned(hp1^.p2) then
                             begin
                                write_expr(outfile,hp1^.p2);
                                writeln(outfile,';');
                                lastexpr:=hp1^.p2;
                                if lastexpr^.typ=t_id then
                                  begin
                                     val(lastexpr^.str,l,error);
                                     if error=0 then
                                       begin
                                          inc(l);
                                          lastexpr:=nil;
                                       end
                                     else
                                       l:=1;
                                  end
                                else
                                  l:=1;
                             end
                           else
                             begin
                                if assigned(lastexpr) then
                                  begin
                                     write(outfile,'(');
                                     write_expr(outfile,lastexpr);
                                     writeln(outfile,')+',l,';');
                                  end
                                else
                                  writeln (outfile,l,';');
                                inc(l);
                             end;
                           hp1:=hp1^.next;
                           flush(outfile);
                        end;
                      block_type:=bt_const;
                  end;
               end;
            t_structdef :
              begin
                 inc(typedef_level);
                 flag_index:=-1;
                 is_sized:=false;
                 current_level:=0;
                 if (typedef_level>1) and (p^.p1=nil) and
                    (p^.p2^.typ=t_id) then
                   begin
                      write(outfile,p^.p2^.p);
                   end
                 else
                   begin
                      writeln(outfile,'record');
                      shift(3);
                      hp1:=p^.p1;

                      (* walk through all members *)
                      while assigned(hp1) do
                        begin
                           (* hp2 is t_memberdec *)
                           hp2:=hp1^.p1;
                           (*  hp3 is t_declist *)
                           hp3:=hp2^.p2;
                           while assigned(hp3) do
                             begin
                                if not assigned(hp3^.p1^.p3) or
                                   (hp3^.p1^.p3^.typ <> t_size_specifier) then
                                  begin
                                     if is_sized then
                                       begin
                                          if current_level <= 16 then
                                            writeln(outfile,'word;')
                                          else if current_level <= 32 then
                                            writeln(outfile,'longint;')
                                          else
                                            internalerror(11);
                                          is_sized:=false;
                                       end;

                                     write(outfile,aktspace,hp3^.p1^.p2^.p);
                                     write(outfile,' : ');
                                     shift(2);
                                     write_p_a_def(outfile,hp3^.p1^.p1,hp2^.p1);
                                     popshift;
                                  end;
                                { size specifier  or default value ? }
                                if assigned(hp3^.p1^.p3) then
                                  begin
                                     { we could use mask to implement this }
                                     { because we need to respect the positions }
                                     if hp3^.p1^.p3^.typ = t_size_specifier then
                                       begin
                                          if not is_sized then
                                            begin
                                               current_power:=1;
                                               current_level:=0;
                                               inc(flag_index);
                                               write(outfile,aktspace,'flag',flag_index,' : ');
                                            end;
                                          must_write_packed_field:=true;
                                          is_sized:=true;
                                          { can it be something else than a constant ? }
                                          { it can be a macro !! }
                                          if hp3^.p1^.p3^.p1^.typ=t_id then
                                            begin
                                              val(hp3^.p1^.p3^.p1^.str,l,error);
                                              if error=0 then
                                                begin
                                                   mask:=0;
                                                   for i:=1 to l do
                                                     begin
                                                        mask:=mask+current_power;
                                                        current_power:=current_power*2;
                                                     end;
                                                   write(tempfile,'bm_&',hp3^.p1^.p2^.p);
                                                   writeln(tempfile,' = ',hexstr(mask),';');
                                                   write(tempfile,'bp_&',hp3^.p1^.p2^.p);
                                                   writeln(tempfile,' = ',current_level,';');
                                                   current_level:=current_level + l;
                                                   { go to next flag if 31 }
                                                   if current_level = 32 then
                                                     begin
                                                        write(outfile,'longint');
                                                        is_sized:=false;
                                                     end;
                                                end;
                                            end;

                                       end
                                     else if hp3^.p1^.p3^.typ = t_default_value then
                                       begin
                                          write(outfile,'{=');
                                          write_expr(outfile,hp3^.p1^.p3^.p1);
                                          write(outfile,' ignored}');
                                       end;
                                  end;
                                if not is_sized then
                                  begin
                                     if is_procvar then
                                       begin
                                          if not no_pop then
                                            begin
                                               write(outfile,';cdecl');
                                               no_pop:=true;
                                            end;
                                          is_procvar:=false;
                                       end;
                                     writeln(outfile,';');
                                  end;
                                hp3:=hp3^.next;
                             end;
                           hp1:=hp1^.next;
                        end;
                      if is_sized then
                        begin
                           if current_level <= 16 then
                             writeln(outfile,'word;')
                           else if current_level <= 32 then
                             writeln(outfile,'longint;')
                           else
                             internalerror(11);
                           is_sized:=false;
                        end;
                      popshift;
                      write(outfile,aktspace,'end');
                      flush(outfile);
                   end;
                 dec(typedef_level);
              end;
            t_uniondef :
              begin
                 if (typedef_level>1) and (p^.p1=nil) and
                    (p^.p2^.typ=t_id) then
                   begin
                      write(outfile,p^.p2^.p);
                   end
                 else
                   begin
                      inc(typedef_level);
                      writeln(outfile,'record');
                      shift(2);
                      writeln(outfile,aktspace,'case longint of');
                      shift(3);
                      l:=0;
                      hp1:=p^.p1;

                      (* walk through all members *)
                      while assigned(hp1) do
                        begin
                           (* hp2 is t_memberdec *)
                           hp2:=hp1^.p1;
                           (* hp3 is t_declist *)
                           hp3:=hp2^.p2;
                           while assigned(hp3) do
                             begin
                                write(outfile,aktspace,l,' : ( ');
                                write(outfile,hp3^.p1^.p2^.p,' : ');
                                shift(2);
                                write_p_a_def(outfile,hp3^.p1^.p1,hp2^.p1);
                                popshift;
                                writeln(outfile,' );');
                                hp3:=hp3^.next;
                                inc(l);
                             end;
                           hp1:=hp1^.next;
                        end;
                      popshift;
                      write(outfile,aktspace,'end');
                      popshift;
                      flush(outfile);
                      dec(typedef_level);
                   end;
              end;
            else
              internalerror(3);
         end;
      end;

    procedure write_def_params(var outfile:text; p : presobject);
      var
         hp1 : presobject;
      begin
         case p^.typ of
            t_enumdef : begin
                           hp1:=p^.p1;
                           while assigned(hp1) do
                             begin
                                write(outfile,hp1^.p1^.p);
                                hp1:=hp1^.next;
                                if assigned(hp1) then
                                  write(outfile,',')
                                else
                                  write(outfile);
                                flush(outfile);
                             end;
                           flush(outfile);
                        end;
         else internalerror(4);
         end;
      end;

const TYPEDEF = 257;
const DEFINE = 258;
const COLON = 259;
const SEMICOLON = 260;
const COMMA = 261;
const LKLAMMER = 262;
const RKLAMMER = 263;
const LECKKLAMMER = 264;
const RECKKLAMMER = 265;
const LGKLAMMER = 266;
const RGKLAMMER = 267;
const STRUCT = 268;
const UNION = 269;
const ENUM = 270;
const ID = 271;
const NUMBER = 272;
const CSTRING = 273;
const SHORT = 274;
const UNSIGNED = 275;
const LONG = 276;
const INT = 277;
const REAL = 278;
const _CHAR = 279;
const VOID = 280;
const _CONST = 281;
const _FAR = 282;
const _HUGE = 283;
const _NEAR = 284;
const _ASSIGN = 285;
const NEW_LINE = 286;
const SPACE_DEFINE = 287;
const EXTERN = 288;
const STDCALL = 289;
const CDECL = 290;
const CALLBACK = 291;
const PASCAL = 292;
const WINAPI = 293;
const APIENTRY = 294;
const WINGDIAPI = 295;
const SYS_TRAP = 296;
const _PACKED = 297;
const ELLIPSIS = 298;
const R_AND = 299;
const EQUAL = 300;
const UNEQUAL = 301;
const GT = 302;
const LT = 303;
const GTE = 304;
const LTE = 305;
const QUESTIONMARK = 306;
const _OR = 307;
const _AND = 308;
const _PLUS = 309;
const MINUS = 310;
const _SHR = 311;
const _SHL = 312;
const STAR = 313;
const _SLASH = 314;
const _NOT = 315;
const PSTAR = 316;
const P_AND = 317;
const POINT = 318;
const DEREF = 319;
const STICK = 320;

var yylval : YYSType;

function yylex : Integer; forward;

function yyparse : Integer;

var yystate, yysp, yyn : Integer;
    yys : array [1..yymaxdepth] of Integer;
    yyv : array [1..yymaxdepth] of YYSType;
    yyval : YYSType;

procedure yyaction ( yyruleno : Integer );
  (* local definitions: *)
begin
  (* actions: *)
  case yyruleno of
   1 : begin
         yyval := yyv[yysp-0];
       end;
   2 : begin
         writeln(outfile,'(* error ');
         writeln(outfile,prev_line);
         writeln(outfile,last_source_line);
         
       end;
   3 : begin
         if yydebug then writeln('declaration reduced at line ',line_no);
         if yydebug then writeln(outfile,'(* declaration reduced *)');
         
       end;
   4 : begin
         if yydebug then writeln('define declaration reduced at line ',line_no);
         if yydebug then writeln(outfile,'(* define declaration reduced *)');
         
       end;
   5 : begin
         if yydebug then writeln('declaration reduced at line ',line_no);
         
       end;
   6 : begin
         if yydebug then writeln('define declaration reduced at line ',line_no);
         
       end;
   7 : begin
         yyval:=new(presobject,init_id('extern')); 
       end;
   8 : begin
         yyval:=new(presobject,init_id('intern')); 
       end;
   9 : begin
         yyval:=new(presobject,init_id('no_pop')); 
       end;
  10 : begin
         yyval:=new(presobject,init_id('cdecl')); 
       end;
  11 : begin
         yyval:=new(presobject,init_id('no_pop')); 
       end;
  12 : begin
         yyval:=new(presobject,init_id('no_pop')); 
       end;
  13 : begin
         yyval:=new(presobject,init_id('no_pop')); 
       end;
  14 : begin
         yyval:=new(presobject,init_id('no_pop')); 
       end;
  15 : begin
         yyval:=new(presobject,init_id('no_pop')); 
       end;
  16 : begin
         yyval:=nil 
       end;
  17 : begin
         yyval:=yyv[yysp-1]; 
       end;
  18 : begin
         yyval:=nil; 
       end;
  19 : begin
         IsExtern:=false;
         (* by default we must pop the args pushed on stack *)
         no_pop:=false;
         (* writeln(outfile,'{ dec_specifier type_specifier declarator_list SEMICOLON}');
         
         if assigned(yyv[yysp-3]) then writeln(outfile,'{*$3}');
         if assigned(yyv[yysp-3])and assigned(yyv[yysp-3].p1)
         then writeln(outfile,'{*$3^.p1}');
         if assigned(yyv[yysp-3])and assigned(yyv[yysp-3]^.p1)and assigned(yyv[yysp-3]^.p1^.p1)
         then writeln(outfile,'{*$3^.p1^.p1}');
         *)
         
         if (assigned(yyv[yysp-2])and assigned(yyv[yysp-2]^.p1)and assigned(yyv[yysp-2]^.p1^.p1))
         and (yyv[yysp-2]^.p1^.p1^.typ=t_procdef) then
         begin
         If UseLib then
         IsExtern:=true
         else
         IsExtern:=assigned(yyv[yysp-5])and(yyv[yysp-5]^.str='extern');
         no_pop:=assigned(yyv[yysp-3]) and (yyv[yysp-3]^.str='no_pop');
         if block_type<>bt_func then
         writeln(outfile);
         
         block_type:=bt_func;
         write(outfile,aktspace);
         write(extfile,aktspace);
         (* distinguish between procedure and function *)
         if assigned(yyv[yysp-4]) then
         if (yyv[yysp-4]^.typ=t_void) and (yyv[yysp-2]^.p1^.p1^.p1=nil) then
         begin
         write(outfile,'procedure ',yyv[yysp-2]^.p1^.p2^.p);
         (* write arguments *)
         shift(10);
         if assigned(yyv[yysp-2]^.p1^.p1^.p2) then
         write_args(outfile,yyv[yysp-2]^.p1^.p1^.p2);
         write(extfile,'procedure ',yyv[yysp-2]^.p1^.p2^.p);
         (* write arguments *)
         if assigned(yyv[yysp-2]^.p1^.p1^.p2) then
         write_args(extfile,yyv[yysp-2]^.p1^.p1^.p2);
         end
         else
         begin
         write(outfile,'function ',yyv[yysp-2]^.p1^.p2^.p);
         write(extfile,'function ',yyv[yysp-2]^.p1^.p2^.p);
         
         shift(9);
         (* write arguments *)
         if assigned(yyv[yysp-2]^.p1^.p1^.p2) then
         write_args(outfile,yyv[yysp-2]^.p1^.p1^.p2);
         if assigned(yyv[yysp-2]^.p1^.p1^.p2) then
         write_args(extfile,yyv[yysp-2]^.p1^.p1^.p2);
         
         write(outfile,':');
         write(extfile,':');
         write_p_a_def(outfile,yyv[yysp-2]^.p1^.p1^.p1,yyv[yysp-4]);
         write_p_a_def(extfile,yyv[yysp-2]^.p1^.p1^.p1,yyv[yysp-4]);
         end;
         
         if assigned(yyv[yysp-1]) then
         write(outfile,';systrap ',yyv[yysp-1]^.p);
         
         (* No CDECL in interface for Uselib *)
         if IsExtern and (not no_pop) then
         begin
         write(outfile,';cdecl');
         write(extfile,';cdecl');
         end;
         popshift;
         if UseLib then
         begin
         if IsExtern then
         begin
         write (extfile,';external');
         If UseName then
         Write(extfile,' External_library name ''',yyv[yysp-2]^.p1^.p2^.p,'''');
         end;
         writeln(extfile,';');
         writeln(outfile,';');
         end
         else
         begin
         writeln(extfile,';');
         writeln(outfile,';');
         if not IsExtern then
         begin
         writeln(extfile,aktspace,'  begin');
         writeln(extfile,aktspace,'     { You must implemented this function }');
         writeln(extfile,aktspace,'  end;');
         end;
         end;
         IsExtern:=false;
         writeln(outfile);
         if Uselib then
         writeln(extfile);
         end
         else (* yyv[yysp-2]^.p1^.p1^.typ=t_procdef *)
         if assigned(yyv[yysp-2])and assigned(yyv[yysp-2]^.p1) then
         begin
         shift(2);
         if block_type<>bt_var then
         begin
         writeln(outfile);
         writeln(outfile,aktspace,'var');
         end;
         block_type:=bt_var;
         
         shift(3);
         
         IsExtern:=assigned(yyv[yysp-5])and(yyv[yysp-5]^.str='extern');
         (* walk through all declarations *)
         hp:=yyv[yysp-2];
         while assigned(hp) and assigned(hp^.p1) do
         begin
         (* write new var name *)
         if assigned(hp^.p1^.p2)and assigned(hp^.p1^.p2^.p)then
         write(outfile,aktspace,hp^.p1^.p2^.p);
         write(outfile,' : ');
         shift(2);
         (* write its type *)
         write_p_a_def(outfile,hp^.p1^.p1,yyv[yysp-4]);
         if assigned(hp^.p1^.p2)and assigned(hp^.p1^.p2^.p)then
         begin
         if isExtern then
         write(outfile,';cvar;external')
         else
         write(outfile,';cvar;export');
         write(outfile,hp^.p1^.p2^.p);
         end;
         writeln(outfile,''';');
         popshift;
         hp:=hp^.p2;
         end;
         popshift;
         popshift;
         end;
         if assigned(yyv[yysp-5])then  dispose(yyv[yysp-5],done);
         if assigned(yyv[yysp-4])then  dispose(yyv[yysp-4],done);
         if assigned(yyv[yysp-2])then  dispose(yyv[yysp-2],done);
         
       end;
  20 : begin
         
         if block_type<>bt_type then
         begin
         writeln(outfile);
         writeln(outfile,aktspace,'type');
         end;
         block_type:=bt_type;
         shift(3);
         (* write new type name *)
         TN:=strpas(yyv[yysp-1]^.p2^.p);
         if (yyv[yysp-1]^.typ=t_structdef) or (yyv[yysp-1]^.typ=t_uniondef) then
         begin
         PN:='P'+strpas(yyv[yysp-1]^.p2^.p);
         if PrependTypes then
         TN:='T'+TN;
         if UsePPointers then
         Writeln (outfile,aktspace,PN,' = ^',TN,';');
         end;
         write(outfile,aktspace,TN,' = ');
         shift(2);
         hp:=yyv[yysp-1];
         write_type_specifier(outfile,hp);
         popshift;
         (* enum_to_const can make a switch to const *)
         if block_type=bt_type then writeln(outfile,';');
         writeln(outfile);
         flush(outfile);
         popshift;
         if must_write_packed_field then
         write_packed_fields_info(outfile,hp,TN);
         if assigned(hp) then
         dispose(hp,done);
         
       end;
  21 : begin
         
         if block_type<>bt_type then
         begin
         writeln(outfile);
         writeln(outfile,aktspace,'type');
         end;
         block_type:=bt_type;
         
         no_pop:=assigned(yyv[yysp-2]) and (yyv[yysp-2]^.str='no_pop');
         shift(3);
         (* walk through all declarations *)
         hp:=yyv[yysp-1];
         ph:=nil;
         is_procvar:=false;
         while assigned(hp) do
         begin
         writeln(outfile);
         (* write new type name *)
         write(outfile,aktspace,hp^.p1^.p2^.p);
         write(outfile,' = ');
         shift(2);
         if assigned(ph) then
         write_p_a_def(outfile,hp^.p1^.p1,ph)
         else
         write_p_a_def(outfile,hp^.p1^.p1,yyv[yysp-3]);
         (* simple def ?
         keep the name for the other defs *)
         if (ph=nil) and (hp^.p1^.p1=nil) then
         ph:=hp^.p1^.p2;
         popshift;
         (* if no_pop it is normal fpc calling convention *)
         if is_procvar and
         (not no_pop) then
         write(outfile,';cdecl');
         writeln(outfile,';');
         flush(outfile);
         hp:=hp^.next;
         end;
         (* write tag name *)
         if assigned(ph) and
         ((yyv[yysp-3]^.typ=t_structdef) or
         (yyv[yysp-3]^.typ=t_enumdef) or
         (yyv[yysp-3]^.typ=t_uniondef)) and
         assigned(yyv[yysp-3]^.p2) then
         begin
         writeln(outfile);
         write(outfile,aktspace,yyv[yysp-3]^.p2^.p,' = ');
         if assigned(ph) then
         writeln(outfile,ph^.p,';')
         else
         begin
         write_p_a_def(outfile,hp^.p1^.p1,yyv[yysp-3]);
         writeln(outfile,';');
         end;
         end;
         popshift;
         if must_write_packed_field then
         if assigned(ph) then
         write_packed_fields_info(outfile,yyv[yysp-3],ph^.str)
         else if assigned(yyv[yysp-3]^.p2) then
         write_packed_fields_info(outfile,yyv[yysp-3],yyv[yysp-3]^.p2^.str);
         if assigned(yyv[yysp-3])then
         dispose(yyv[yysp-3],done);
         if assigned(yyv[yysp-2])then
         dispose(yyv[yysp-2],done);
         if assigned(yyv[yysp-1])then
         dispose(yyv[yysp-1],done);
         
       end;
  22 : begin
         
         if block_type<>bt_type then
         begin
         writeln(outfile);
         writeln(outfile,aktspace,'type');
         end;
         block_type:=bt_type;
         
         shift(3);
         (* write as pointer *)
         writeln(outfile);
         writeln(outfile,'(* generic typedef  *)');
         writeln(outfile,aktspace,yyv[yysp-1]^.p,' = pointer;');
         flush(outfile);
         popshift;
         if assigned(yyv[yysp-1])then
         dispose(yyv[yysp-1],done);
         
       end;
  23 : begin
         writeln(outfile,'in declaration at line ',line_no,' *)');
         aktspace:='';
         in_space_define:=0;
         in_define:=false;
         arglevel:=0;
         if_nb:=0;
         aktspace:='    ';
         space_index:=1;
         yyerrok;
       end;
  24 : begin
         
         writeln (outfile,aktspace,'{ was #define dname(params) def_expr }');
         writeln (extfile,aktspace,'{ was #define dname(params) def_expr }');
         if assigned(yyv[yysp-4]) then
         begin
         writeln (outfile,aktspace,'{ argument types are unknown }');
         writeln (extfile,aktspace,'{ argument types are unknown }');
         end;
         if not assigned(yyv[yysp-1]^.p3) then
         begin
         writeln(outfile,aktspace,'{ return type might be wrong }   ');
         writeln(extfile,aktspace,'{ return type might be wrong }   ');
         end;
         block_type:=bt_func;
         write(outfile,aktspace,'function ',yyv[yysp-6]^.p);
         write(extfile,aktspace,'function ',yyv[yysp-6]^.p);
         
         if assigned(yyv[yysp-4]) then
         begin
         write(outfile,'(');
         write(extfile,'(');
         ph:=new(presobject,init_one(t_enumdef,yyv[yysp-4]));
         write_def_params(outfile,ph);
         write_def_params(extfile,ph);
         if assigned(ph) then dispose(ph,done);
         ph:=nil;
         (* types are unknown *)
         write(outfile,' : longint)');
         write(extfile,' : longint)');
         end;
         if not assigned(yyv[yysp-1]^.p3) then
         begin
         writeln(outfile,' : longint;');
         writeln(outfile,aktspace,'  { return type might be wrong }   ');
         flush(outfile);
         writeln(extfile,' : longint;');
         writeln(extfile,aktspace,'  { return type might be wrong }   ');
         end
         else
         begin
         write(outfile,' : ');
         write_type_specifier(outfile,yyv[yysp-1]^.p3);
         writeln(outfile,';');
         flush(outfile);
         write(extfile,' : ');
         write_type_specifier(extfile,yyv[yysp-1]^.p3);
         writeln(extfile,';');
         end;
         writeln(outfile);
         flush(outfile);
         hp:=new(presobject,init_two(t_funcname,yyv[yysp-6],yyv[yysp-1]));
         write_funexpr(extfile,hp);
         writeln(extfile);
         flush(extfile);
         if assigned(hp)then dispose(hp,done);
         
       end;
  25 : begin
         
         writeln(outfile,'{$define ',yyv[yysp-2]^.p,'}');
         flush(outfile);
         if assigned(yyv[yysp-2])then
         dispose(yyv[yysp-2],done);
         
       end;
  26 : begin
         
         writeln(outfile,'{$define ',yyv[yysp-1]^.p,'}');
         flush(outfile);
         if assigned(yyv[yysp-1])then
         dispose(yyv[yysp-1],done);
         
       end;
  27 : begin
         
         if (yyv[yysp-1]^.typ=t_exprlist) and
         yyv[yysp-1]^.p1^.is_const and
         not assigned(yyv[yysp-1]^.next) then
         begin
         if block_type<>bt_const then
         begin
         writeln(outfile);
         writeln(outfile,aktspace,'const');
         end;
         block_type:=bt_const;
         
         aktspace:=aktspace+'   ';
         write(outfile,aktspace,yyv[yysp-3]^.p);
         write(outfile,' = ');
         flush(outfile);
         write_expr(outfile,yyv[yysp-1]^.p1);
         writeln(outfile,';');
         dec(byte(aktspace[0]),3);
         if assigned(yyv[yysp-3]) then
         dispose(yyv[yysp-3],done);
         if assigned(yyv[yysp-1]) then
         dispose(yyv[yysp-1],done);
         end
         else
         begin
         aktspace:=aktspace+'  ';
         writeln (outfile,aktspace,'{ was #define dname def_expr }');
         writeln (extfile,aktspace,'{ was #define dname def_expr }');
         block_type:=bt_func;
         write(outfile,aktspace,'function ',yyv[yysp-3]^.p);
         write(extfile,aktspace,'function ',yyv[yysp-3]^.p);
         if not assigned(yyv[yysp-1]^.p3) then
         begin
         writeln(outfile,' : longint;');
         writeln(outfile,aktspace,'  { return type might be wrong }');
         flush(outfile);
         writeln(extfile,' : longint;');
         writeln(extfile,aktspace,'  { return type might be wrong }');
         end
         else
         begin
         write(outfile,' : ');
         write_type_specifier(outfile,yyv[yysp-1]^.p3);
         writeln(outfile,';');
         flush(outfile);
         write(extfile,' : ');
         write_type_specifier(extfile,yyv[yysp-1]^.p3);
         writeln(extfile,';');
         end;
         writeln(outfile);
         flush(outfile);
         hp:=new(presobject,init_two(t_funcname,yyv[yysp-3],yyv[yysp-1]));
         write_funexpr(extfile,hp);
         dec(byte(aktspace[0]),2);
         dispose(hp,done);
         writeln(extfile);
         flush(extfile);
         end;
         
       end;
  28 : begin
         writeln(outfile,'in define line ',line_no,' *)');
         aktspace:='';
         in_space_define:=0;
         in_define:=false;
         arglevel:=0;
         if_nb:=0;
         aktspace:='    ';
         space_index:=1;
         
         yyerrok;
       end;
  29 : begin
         yyval:=yyv[yysp-1];
       end;
  30 : begin
         writeln(outfile,' in member_list *)');
         yyerrok;
         yyval:=nil;
         
       end;
  31 : begin
         yyval:=yyv[yysp-1];
       end;
  32 : begin
         writeln(outfile,' in enum_list *)');
         yyerrok;
         yyval:=nil;
         
       end;
  33 : begin
         
         if not is_packed then
         writeln(outfile,'{$PACKRECORDS 1}');
         is_packed:=true;
         yyval:=new(presobject,init_two(t_structdef,yyv[yysp-1],yyv[yysp-2]));
         
       end;
  34 : begin
         
         if is_packed then
         writeln(outfile,'{$PACKRECORDS 4}');
         is_packed:=false;
         yyval:=new(presobject,init_two(t_structdef,yyv[yysp-0],yyv[yysp-1]));
         
       end;
  35 : begin
         
         if not is_packed then
         writeln(outfile,'{$PACKRECORDS 1}');
         is_packed:=true;
         yyval:=new(presobject,init_two(t_uniondef,yyv[yysp-1],yyv[yysp-2]));
         
       end;
  36 : begin
         
         yyval:=new(presobject,init_two(t_uniondef,yyv[yysp-0],yyv[yysp-1]));
         
       end;
  37 : begin
         
         yyval:=new(presobject,init_two(t_uniondef,nil,yyv[yysp-0]));
         
       end;
  38 : begin
         
         yyval:=new(presobject,init_two(t_structdef,nil,yyv[yysp-0]));
         
       end;
  39 : begin
         
         yyval:=new(presobject,init_two(t_enumdef,yyv[yysp-0],yyv[yysp-1]));
         
       end;
  40 : begin
         
         yyval:=new(presobject,init_two(t_enumdef,nil,yyv[yysp-0]));
         
       end;
  41 : begin
         
         writeln(outfile,'(* Const before type ignored *)');
         yyval:=yyv[yysp-0];
         
       end;
  42 : begin
         
         if not is_packed then
         writeln(outfile,'{$PACKRECORDS 1}');
         is_packed:=true;
         yyval:=new(presobject,init_one(t_uniondef,yyv[yysp-1]));
         
       end;
  43 : begin
         
         yyval:=new(presobject,init_one(t_uniondef,yyv[yysp-0]));
         
       end;
  44 : begin
         
         if not is_packed then
         writeln(outfile,'{$PACKRECORDS 1}');
         is_packed:=true;
         yyval:=new(presobject,init_one(t_structdef,yyv[yysp-1]));
         
       end;
  45 : begin
         
         if is_packed then
         writeln(outfile,'{$PACKRECORDS 4}');
         is_packed:=false;
         yyval:=new(presobject,init_one(t_structdef,yyv[yysp-0]));
         
       end;
  46 : begin
         
         yyval:=new(presobject,init_one(t_enumdef,yyv[yysp-0]));
         
       end;
  47 : begin
         
         yyval:=yyv[yysp-0];
         
       end;
  48 : begin
         yyval:=yyv[yysp-0]; 
       end;
  49 : begin
         
         yyval:=new(presobject,init_one(t_memberdeclist,yyv[yysp-1]));
         yyval^.next:=yyv[yysp-0];
         
       end;
  50 : begin
         
         yyval:=new(presobject,init_one(t_memberdeclist,yyv[yysp-0]));
         
       end;
  51 : begin
         
         yyval:=new(presobject,init_two(t_memberdec,yyv[yysp-2],yyv[yysp-1]));
         
       end;
  52 : begin
         (*dname*)
         yyval:=new(presobject,init_id(act_token));
         
       end;
  53 : begin
         
         yyval:=new(presobject,init_id(INT_STR));
         
       end;
  54 : begin
         
         yyval:=new(presobject,init_id(UINT_STR));
         
       end;
  55 : begin
         
         yyval:=new(presobject,init_id(INT_STR));
         
       end;
  56 : begin
         
         yyval:=new(presobject,init_id(REAL_STR));
         
       end;
  57 : begin
         
         yyval:=new(presobject,init_id(INT_STR));
         
       end;
  58 : begin
         
         yyval:=new(presobject,init_id(UINT_STR));
         
       end;
  59 : begin
         
         yyval:=new(presobject,init_id(UINT_STR));
         
       end;
  60 : begin
         
         yyval:=new(presobject,init_id(UINT_STR));
         
       end;
  61 : begin
         
         yyval:=new(presobject,init_id(USHORT_STR));
         
       end;
  62 : begin
         
         yyval:=new(presobject,init_id(UCHAR_STR));
         
       end;
  63 : begin
         
         yyval:=new(presobject,init_no(t_void));
         
       end;
  64 : begin
         
         yyval:=new(presobject,init_id(SHORT_STR));
         
       end;
  65 : begin
         
         yyval:=new(presobject,init_id(CHAR_STR));
         
       end;
  66 : begin
         
         yyval:=yyv[yysp-0];
         
       end;
  67 : begin
         
         yyval:=yyv[yysp-0];
         
       end;
  68 : begin
         
         yyval:=yyv[yysp-2];
         hp:=yyv[yysp-2];
         while assigned(hp^.next) do
         hp:=hp^.next;
         hp^.next:=new(presobject,init_one(t_declist,yyv[yysp-0]));
         
       end;
  69 : begin
         
         writeln(outfile,' in declarator_list *)');
         yyval:=yyv[yysp-0];
         yyerrok;
         
       end;
  70 : begin
         
         writeln(outfile,' in declarator_list *)');
         yyerrok;
         
       end;
  71 : begin
         
         yyval:=new(presobject,init_one(t_declist,yyv[yysp-0]));
         
       end;
  72 : begin
         
         yyval:=new(presobject,init_two(t_arg,yyv[yysp-1],yyv[yysp-0]));
         
       end;
  73 : begin
         
         yyval:=new(presobject,init_two(t_arg,yyv[yysp-1],yyv[yysp-0]));
         
       end;
  74 : begin
         
         yyval:=new(presobject,init_two(t_arglist,yyv[yysp-0],nil));
         
       end;
  75 : begin
         
         yyval:=new(presobject,init_two(t_arglist,yyv[yysp-2],nil));
         yyval^.next:=yyv[yysp-0];
         
       end;
  76 : begin
         
         yyval:=new(presobject,init_two(t_arglist,ellipsisarg,nil));
         (*** ELLIPSIS PROBLEM ***)
         
       end;
  77 : begin
         yyval:=new(presobject,init_id('far'));
       end;
  78 : begin
         yyval:=new(presobject,init_id('near'));
       end;
  79 : begin
         yyval:=new(presobject,init_id('huge'));
       end;
  80 : begin
         
         writeln(outfile,'(* Const before declarator ignored *)');
         yyval:=yyv[yysp-0];
         
       end;
  81 : begin
         
         writeln(outfile,aktspace,'(* ',yyv[yysp-2]^.p,' ignored *)');
         dispose(yyv[yysp-2],done);
         hp:=yyv[yysp-0];
         yyval:=hp;
         while assigned(hp^.p1) do
         hp:=hp^.p1;
         hp^.p1:=new(presobject,init_one(t_pointerdef,nil));
         
       end;
  82 : begin
         
         (* %prec PSTAR     this was wrong!! *)
         hp:=yyv[yysp-0];
         yyval:=hp;
         while assigned(hp^.p1) do
         hp:=hp^.p1;
         hp^.p1:=new(presobject,init_one(t_pointerdef,nil));
         
       end;
  83 : begin
         
         hp:=yyv[yysp-0];
         yyval:=hp;
         while assigned(hp^.p1) do
         hp:=hp^.p1;
         hp^.p1:=new(presobject,init_one(t_addrdef,nil));
         
       end;
  84 : begin
         
         (*  size specifier supported *)
         hp:=new(presobject,init_one(t_size_specifier,yyv[yysp-0]));
         yyval:=new(presobject,init_three(t_dec,nil,yyv[yysp-2],hp));
         
       end;
  85 : begin
         
         writeln(outfile,'(* Warning : default value for ',yyv[yysp-2]^.p,' ignored *)');
         hp:=new(presobject,init_one(t_default_value,yyv[yysp-0]));
         yyval:=new(presobject,init_three(t_dec,nil,yyv[yysp-2],hp));
         
       end;
  86 : begin
         
         yyval:=new(presobject,init_two(t_dec,nil,yyv[yysp-0]));
         
       end;
  87 : begin
         
         hp:=yyv[yysp-3];
         yyval:=hp;
         while assigned(hp^.p1) do
         hp:=hp^.p1;
         hp^.p1:=new(presobject,init_two(t_procdef,nil,yyv[yysp-1]));
         
       end;
  88 : begin
         
         hp:=yyv[yysp-1];
         yyval:=hp;
         while assigned(hp^.p1) do
         hp:=hp^.p1;
         hp^.p1:=new(presobject,init_two(t_procdef,nil,nil));
         
       end;
  89 : begin
         
         hp:=yyv[yysp-3];
         yyval:=hp;
         while assigned(hp^.p1) do
         hp:=hp^.p1;
         hp^.p1:=new(presobject,init_two(t_arraydef,nil,yyv[yysp-1]));
         
       end;
  90 : begin
         yyval:=yyv[yysp-1]; 
       end;
  91 : begin
         yyval := yyv[yysp-1];
       end;
  92 : begin
         yyval := yyv[yysp-2];
       end;
  93 : begin
         
         writeln(outfile,'(* Const before abstract_declarator ignored *)');
         yyval:=yyv[yysp-0];
         
       end;
  94 : begin
         
         writeln(outfile,aktspace,'(* ',yyv[yysp-2]^.p,' ignored *)');
         dispose(yyv[yysp-2],done);
         hp:=yyv[yysp-0];
         yyval:=hp;
         while assigned(hp^.p1) do
         hp:=hp^.p1;
         hp^.p1:=new(presobject,init_one(t_pointerdef,nil));
         
       end;
  95 : begin
         
         hp:=yyv[yysp-0];
         yyval:=hp;
         while assigned(hp^.p1) do
         hp:=hp^.p1;
         hp^.p1:=new(presobject,init_one(t_pointerdef,nil));
         
       end;
  96 : begin
         
         hp:=yyv[yysp-3];
         yyval:=hp;
         while assigned(hp^.p1) do
         hp:=hp^.p1;
         hp^.p1:=new(presobject,init_two(t_procdef,nil,yyv[yysp-1]));
         
       end;
  97 : begin
         
         hp:=yyv[yysp-1];
         yyval:=hp;
         while assigned(hp^.p1) do
         hp:=hp^.p1;
         hp^.p1:=new(presobject,init_two(t_procdef,nil,nil));
         
       end;
  98 : begin
         
         hp:=yyv[yysp-3];
         yyval:=hp;
         while assigned(hp^.p1) do
         hp:=hp^.p1;
         hp^.p1:=new(presobject,init_two(t_arraydef,nil,yyv[yysp-1]));
         
       end;
  99 : begin
         yyval:=yyv[yysp-1]; 
       end;
 100 : begin
         
         yyval:=new(presobject,init_two(t_dec,nil,nil));
         
       end;
 101 : begin
         yyval:=yyv[yysp-0];
       end;
 102 : begin
         yyval:=new(presobject,init_bop(' = ',yyv[yysp-2],yyv[yysp-0]));
       end;
 103 : begin
         yyval:=new(presobject,init_bop(' <> ',yyv[yysp-2],yyv[yysp-0]));
       end;
 104 : begin
         yyval:=new(presobject,init_bop(' > ',yyv[yysp-2],yyv[yysp-0]));
       end;
 105 : begin
         yyval:=new(presobject,init_bop(' >= ',yyv[yysp-2],yyv[yysp-0]));
       end;
 106 : begin
         yyval:=new(presobject,init_bop(' < ',yyv[yysp-2],yyv[yysp-0]));
       end;
 107 : begin
         yyval:=new(presobject,init_bop(' <= ',yyv[yysp-2],yyv[yysp-0]));
       end;
 108 : begin
         yyval:=new(presobject,init_bop(' + ',yyv[yysp-2],yyv[yysp-0]));
       end;
 109 : begin
         yyval:=new(presobject,init_bop(' - ',yyv[yysp-2],yyv[yysp-0]));
       end;
 110 : begin
         yyval:=new(presobject,init_bop(' * ',yyv[yysp-2],yyv[yysp-0]));
       end;
 111 : begin
         yyval:=new(presobject,init_bop(' / ',yyv[yysp-2],yyv[yysp-0]));
       end;
 112 : begin
         yyval:=new(presobject,init_bop(' or ',yyv[yysp-2],yyv[yysp-0]));
       end;
 113 : begin
         yyval:=new(presobject,init_bop(' and ',yyv[yysp-2],yyv[yysp-0]));
       end;
 114 : begin
         yyval:=new(presobject,init_bop(' not ',yyv[yysp-2],yyv[yysp-0]));
       end;
 115 : begin
         yyval:=new(presobject,init_bop(' shl ',yyv[yysp-2],yyv[yysp-0]));
       end;
 116 : begin
         yyval:=new(presobject,init_bop(' shr ',yyv[yysp-2],yyv[yysp-0]));
       end;
 117 : begin
         yyv[yysp-0]^.p1:=yyv[yysp-2];
         yyval:=yyv[yysp-0];
         inc(if_nb);
         yyval^.p:=strpnew('if_local'+str(if_nb));
         
       end;
 118 : begin
         yyval:=yyv[yysp-0];
       end;
 119 : begin
         (* if A then B else C *)
         yyval:=new(presobject,init_three(t_ifexpr,nil,yyv[yysp-2],yyv[yysp-0]));
       end;
 120 : begin
         yyval:=yyv[yysp-0]; 
       end;
 121 : begin
         yyval:=nil;
       end;
 122 : begin
         
         yyval:=yyv[yysp-0];
         
       end;
 123 : begin
         
         (* remove L prefix for widestrings *)
         s:=act_token;
         if Win32headers and (s[1]='L') then
         delete(s,1,1);
         yyval:=new(presobject,init_id(''''+copy(s,2,length(s)-2)+''''));
         
       end;
 124 : begin
         
         yyval:=new(presobject,init_id(act_token));
         
       end;
 125 : begin
         
         yyval:=new(presobject,init_bop('.',yyv[yysp-2],yyv[yysp-0]));
         
       end;
 126 : begin
         
         yyval:=new(presobject,init_bop('^.',yyv[yysp-2],yyv[yysp-0]));
         
       end;
 127 : begin
         
         yyval:=new(presobject,init_preop('-',yyv[yysp-0]));
         
       end;
 128 : begin
         
         yyval:=new(presobject,init_preop('@',yyv[yysp-0]));
         
       end;
 129 : begin
         
         yyval:=new(presobject,init_preop(' not ',yyv[yysp-0]));
         
       end;
 130 : begin
         
         if assigned(yyv[yysp-0]) then
         yyval:=new(presobject,init_two(t_typespec,yyv[yysp-2],yyv[yysp-0]))
         else
         yyval:=yyv[yysp-2];
         
       end;
 131 : begin
         
         yyval:=new(presobject,init_two(t_typespec,yyv[yysp-2],yyv[yysp-0]));
         
       end;
 132 : begin
         
         hp:=new(presobject,init_one(t_pointerdef,yyv[yysp-3]));
         yyval:=new(presobject,init_two(t_typespec,hp,yyv[yysp-0]));
         
       end;
 133 : begin
         
         writeln(outfile,aktspace,'(* ',yyv[yysp-3]^.p,' ignored *)');
         dispose(yyv[yysp-3],done);
         write_type_specifier(outfile,yyv[yysp-4]);
         writeln(outfile,' ignored *)');
         hp:=new(presobject,init_one(t_pointerdef,yyv[yysp-4]));
         yyval:=new(presobject,init_two(t_typespec,hp,yyv[yysp-0]));
         
       end;
 134 : begin
         
         hp:=new(presobject,init_one(t_exprlist,yyv[yysp-3]));
         yyval:=new(presobject,init_three(t_funexprlist,hp,yyv[yysp-1],nil));
         
       end;
 135 : begin
         
         yyval:=yyv[yysp-1];
         
       end;
 136 : begin
         (*enum_element COMMA enum_list *)
         yyval:=yyv[yysp-2];
         yyval^.next:=yyv[yysp-0];
         
       end;
 137 : begin
         
         yyval:=yyv[yysp-0];
         
       end;
 138 : begin
         (* empty enum list *)
         yyval:=nil;
       end;
 139 : begin
         begin (*enum_element: dname _ASSIGN expr *)
         yyval:=new(presobject,init_two(t_enumlist,yyv[yysp-2],yyv[yysp-0]));
         end;
         
       end;
 140 : begin
         
         begin (*enum_element: dname*)
         yyval:=new(presobject,init_two(t_enumlist,yyv[yysp-0],nil));
         end;
         
       end;
 141 : begin
         
         if yyv[yysp-0]^.typ=t_funexprlist then
         yyval:=yyv[yysp-0]
         else
         yyval:=new(presobject,init_two(t_exprlist,yyv[yysp-0],nil));
         (* if here is a type specifier
         we know the return type *)
         if (yyv[yysp-0]^.typ=t_typespec) then
         yyval^.p3:=yyv[yysp-0]^.p1^.get_copy;
         
       end;
 142 : begin
         (*exprlist COMMA expr*)
         yyval:=yyv[yysp-2];
         yyv[yysp-2]^.next:=yyv[yysp-0];
         
       end;
 143 : begin
         
         yyval:=yyv[yysp-0];
         
       end;
 144 : begin
         (* empty expression list *)
         yyval:=nil; 
       end;
 145 : begin
         
         yyval:=new(presobject,init_one(t_exprlist,yyv[yysp-0]));
         
       end;
  end;
end(*yyaction*);

(* parse table: *)

type YYARec = record
                sym, act : Integer;
              end;
     YYRRec = record
                len, sym : Integer;
              end;

const

yynacts   = 1862;
yyngotos  = 297;
yynstates = 250;
yynrules  = 145;

yya : array [1..yynacts] of YYARec = (
{ 0: }
  ( sym: 256; act: 7 ),
  ( sym: 257; act: 8 ),
  ( sym: 258; act: 9 ),
  ( sym: 268; act: 10 ),
  ( sym: 269; act: 11 ),
  ( sym: 270; act: 12 ),
  ( sym: 288; act: 13 ),
  ( sym: 271; act: -8 ),
  ( sym: 274; act: -8 ),
  ( sym: 275; act: -8 ),
  ( sym: 276; act: -8 ),
  ( sym: 277; act: -8 ),
  ( sym: 278; act: -8 ),
  ( sym: 279; act: -8 ),
  ( sym: 280; act: -8 ),
  ( sym: 281; act: -8 ),
{ 1: }
  ( sym: 260; act: 14 ),
{ 2: }
  ( sym: 268; act: 20 ),
  ( sym: 269; act: 21 ),
  ( sym: 270; act: 22 ),
  ( sym: 271; act: 23 ),
  ( sym: 274; act: 24 ),
  ( sym: 275; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 277; act: 27 ),
  ( sym: 278; act: 28 ),
  ( sym: 279; act: 29 ),
  ( sym: 280; act: 30 ),
  ( sym: 281; act: 31 ),
{ 3: }
{ 4: }
{ 5: }
  ( sym: 256; act: 7 ),
  ( sym: 257; act: 8 ),
  ( sym: 258; act: 9 ),
  ( sym: 268; act: 10 ),
  ( sym: 269; act: 11 ),
  ( sym: 270; act: 12 ),
  ( sym: 288; act: 13 ),
  ( sym: 0; act: -1 ),
  ( sym: 271; act: -8 ),
  ( sym: 274; act: -8 ),
  ( sym: 275; act: -8 ),
  ( sym: 276; act: -8 ),
  ( sym: 277; act: -8 ),
  ( sym: 278; act: -8 ),
  ( sym: 279; act: -8 ),
  ( sym: 280; act: -8 ),
  ( sym: 281; act: -8 ),
{ 6: }
  ( sym: 0; act: 0 ),
{ 7: }
{ 8: }
  ( sym: 268; act: 20 ),
  ( sym: 269; act: 21 ),
  ( sym: 270; act: 22 ),
  ( sym: 271; act: 23 ),
  ( sym: 274; act: 24 ),
  ( sym: 275; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 277; act: 27 ),
  ( sym: 278; act: 28 ),
  ( sym: 279; act: 29 ),
  ( sym: 280; act: 30 ),
  ( sym: 281; act: 31 ),
{ 9: }
  ( sym: 271; act: 23 ),
{ 10: }
  ( sym: 271; act: 23 ),
{ 11: }
  ( sym: 271; act: 23 ),
{ 12: }
  ( sym: 271; act: 23 ),
{ 13: }
{ 14: }
{ 15: }
{ 16: }
{ 17: }
{ 18: }
  ( sym: 289; act: 42 ),
  ( sym: 290; act: 43 ),
  ( sym: 291; act: 44 ),
  ( sym: 292; act: 45 ),
  ( sym: 293; act: 46 ),
  ( sym: 294; act: 47 ),
  ( sym: 295; act: 48 ),
  ( sym: 256; act: -16 ),
  ( sym: 262; act: -16 ),
  ( sym: 271; act: -16 ),
  ( sym: 281; act: -16 ),
  ( sym: 282; act: -16 ),
  ( sym: 283; act: -16 ),
  ( sym: 284; act: -16 ),
  ( sym: 308; act: -16 ),
  ( sym: 313; act: -16 ),
{ 19: }
{ 20: }
  ( sym: 256; act: 50 ),
  ( sym: 266; act: 51 ),
  ( sym: 271; act: 23 ),
{ 21: }
  ( sym: 256; act: 50 ),
  ( sym: 266; act: 51 ),
  ( sym: 271; act: 23 ),
{ 22: }
  ( sym: 256; act: 54 ),
  ( sym: 266; act: 55 ),
  ( sym: 271; act: 23 ),
{ 23: }
{ 24: }
{ 25: }
  ( sym: 274; act: 56 ),
  ( sym: 276; act: 57 ),
  ( sym: 277; act: 58 ),
  ( sym: 279; act: 59 ),
  ( sym: 256; act: -60 ),
  ( sym: 261; act: -60 ),
  ( sym: 262; act: -60 ),
  ( sym: 263; act: -60 ),
  ( sym: 264; act: -60 ),
  ( sym: 271; act: -60 ),
  ( sym: 281; act: -60 ),
  ( sym: 282; act: -60 ),
  ( sym: 283; act: -60 ),
  ( sym: 284; act: -60 ),
  ( sym: 289; act: -60 ),
  ( sym: 290; act: -60 ),
  ( sym: 291; act: -60 ),
  ( sym: 292; act: -60 ),
  ( sym: 293; act: -60 ),
  ( sym: 294; act: -60 ),
  ( sym: 295; act: -60 ),
  ( sym: 308; act: -60 ),
  ( sym: 313; act: -60 ),
{ 26: }
  ( sym: 277; act: 60 ),
  ( sym: 256; act: -55 ),
  ( sym: 261; act: -55 ),
  ( sym: 262; act: -55 ),
  ( sym: 263; act: -55 ),
  ( sym: 264; act: -55 ),
  ( sym: 271; act: -55 ),
  ( sym: 281; act: -55 ),
  ( sym: 282; act: -55 ),
  ( sym: 283; act: -55 ),
  ( sym: 284; act: -55 ),
  ( sym: 289; act: -55 ),
  ( sym: 290; act: -55 ),
  ( sym: 291; act: -55 ),
  ( sym: 292; act: -55 ),
  ( sym: 293; act: -55 ),
  ( sym: 294; act: -55 ),
  ( sym: 295; act: -55 ),
  ( sym: 308; act: -55 ),
  ( sym: 313; act: -55 ),
{ 27: }
{ 28: }
{ 29: }
{ 30: }
{ 31: }
  ( sym: 268; act: 20 ),
  ( sym: 269; act: 21 ),
  ( sym: 270; act: 22 ),
  ( sym: 271; act: 23 ),
  ( sym: 274; act: 24 ),
  ( sym: 275; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 277; act: 27 ),
  ( sym: 278; act: 28 ),
  ( sym: 279; act: 29 ),
  ( sym: 280; act: 30 ),
  ( sym: 281; act: 31 ),
{ 32: }
{ 33: }
{ 34: }
  ( sym: 260; act: 62 ),
  ( sym: 286; act: 63 ),
{ 35: }
  ( sym: 289; act: 42 ),
  ( sym: 290; act: 43 ),
  ( sym: 291; act: 44 ),
  ( sym: 292; act: 45 ),
  ( sym: 293; act: 46 ),
  ( sym: 294; act: 47 ),
  ( sym: 295; act: 48 ),
  ( sym: 256; act: -16 ),
  ( sym: 262; act: -16 ),
  ( sym: 271; act: -16 ),
  ( sym: 281; act: -16 ),
  ( sym: 282; act: -16 ),
  ( sym: 283; act: -16 ),
  ( sym: 284; act: -16 ),
  ( sym: 308; act: -16 ),
  ( sym: 313; act: -16 ),
{ 36: }
  ( sym: 260; act: 65 ),
  ( sym: 256; act: -67 ),
  ( sym: 262; act: -67 ),
  ( sym: 271; act: -67 ),
  ( sym: 281; act: -67 ),
  ( sym: 282; act: -67 ),
  ( sym: 283; act: -67 ),
  ( sym: 284; act: -67 ),
  ( sym: 289; act: -67 ),
  ( sym: 290; act: -67 ),
  ( sym: 291; act: -67 ),
  ( sym: 292; act: -67 ),
  ( sym: 293; act: -67 ),
  ( sym: 294; act: -67 ),
  ( sym: 295; act: -67 ),
  ( sym: 308; act: -67 ),
  ( sym: 313; act: -67 ),
{ 37: }
  ( sym: 262; act: 66 ),
  ( sym: 286; act: 67 ),
  ( sym: 287; act: 68 ),
{ 38: }
  ( sym: 256; act: 50 ),
  ( sym: 266; act: 51 ),
  ( sym: 260; act: -38 ),
  ( sym: 261; act: -38 ),
  ( sym: 262; act: -38 ),
  ( sym: 263; act: -38 ),
  ( sym: 264; act: -38 ),
  ( sym: 271; act: -38 ),
  ( sym: 281; act: -38 ),
  ( sym: 282; act: -38 ),
  ( sym: 283; act: -38 ),
  ( sym: 284; act: -38 ),
  ( sym: 289; act: -38 ),
  ( sym: 290; act: -38 ),
  ( sym: 291; act: -38 ),
  ( sym: 292; act: -38 ),
  ( sym: 293; act: -38 ),
  ( sym: 294; act: -38 ),
  ( sym: 295; act: -38 ),
  ( sym: 308; act: -38 ),
  ( sym: 313; act: -38 ),
{ 39: }
  ( sym: 256; act: 50 ),
  ( sym: 266; act: 51 ),
  ( sym: 260; act: -37 ),
  ( sym: 261; act: -37 ),
  ( sym: 262; act: -37 ),
  ( sym: 263; act: -37 ),
  ( sym: 264; act: -37 ),
  ( sym: 271; act: -37 ),
  ( sym: 281; act: -37 ),
  ( sym: 282; act: -37 ),
  ( sym: 283; act: -37 ),
  ( sym: 284; act: -37 ),
  ( sym: 289; act: -37 ),
  ( sym: 290; act: -37 ),
  ( sym: 291; act: -37 ),
  ( sym: 292; act: -37 ),
  ( sym: 293; act: -37 ),
  ( sym: 294; act: -37 ),
  ( sym: 295; act: -37 ),
  ( sym: 308; act: -37 ),
  ( sym: 313; act: -37 ),
{ 40: }
  ( sym: 256; act: 54 ),
  ( sym: 266; act: 55 ),
  ( sym: 260; act: -40 ),
  ( sym: 261; act: -40 ),
  ( sym: 262; act: -40 ),
  ( sym: 263; act: -40 ),
  ( sym: 264; act: -40 ),
  ( sym: 271; act: -40 ),
  ( sym: 281; act: -40 ),
  ( sym: 282; act: -40 ),
  ( sym: 283; act: -40 ),
  ( sym: 284; act: -40 ),
  ( sym: 289; act: -40 ),
  ( sym: 290; act: -40 ),
  ( sym: 291; act: -40 ),
  ( sym: 292; act: -40 ),
  ( sym: 293; act: -40 ),
  ( sym: 294; act: -40 ),
  ( sym: 295; act: -40 ),
  ( sym: 308; act: -40 ),
  ( sym: 313; act: -40 ),
{ 41: }
  ( sym: 256; act: 76 ),
  ( sym: 262; act: 77 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 78 ),
  ( sym: 282; act: 79 ),
  ( sym: 283; act: 80 ),
  ( sym: 284; act: 81 ),
  ( sym: 308; act: 82 ),
  ( sym: 313; act: 83 ),
{ 42: }
{ 43: }
{ 44: }
{ 45: }
{ 46: }
{ 47: }
{ 48: }
{ 49: }
  ( sym: 297; act: 84 ),
  ( sym: 256; act: -45 ),
  ( sym: 261; act: -45 ),
  ( sym: 262; act: -45 ),
  ( sym: 263; act: -45 ),
  ( sym: 264; act: -45 ),
  ( sym: 271; act: -45 ),
  ( sym: 281; act: -45 ),
  ( sym: 282; act: -45 ),
  ( sym: 283; act: -45 ),
  ( sym: 284; act: -45 ),
  ( sym: 289; act: -45 ),
  ( sym: 290; act: -45 ),
  ( sym: 291; act: -45 ),
  ( sym: 292; act: -45 ),
  ( sym: 293; act: -45 ),
  ( sym: 294; act: -45 ),
  ( sym: 295; act: -45 ),
  ( sym: 308; act: -45 ),
  ( sym: 313; act: -45 ),
{ 50: }
{ 51: }
  ( sym: 268; act: 20 ),
  ( sym: 269; act: 21 ),
  ( sym: 270; act: 22 ),
  ( sym: 271; act: 23 ),
  ( sym: 274; act: 24 ),
  ( sym: 275; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 277; act: 27 ),
  ( sym: 278; act: 28 ),
  ( sym: 279; act: 29 ),
  ( sym: 280; act: 30 ),
  ( sym: 281; act: 31 ),
{ 52: }
  ( sym: 297; act: 89 ),
  ( sym: 256; act: -43 ),
  ( sym: 261; act: -43 ),
  ( sym: 262; act: -43 ),
  ( sym: 263; act: -43 ),
  ( sym: 264; act: -43 ),
  ( sym: 271; act: -43 ),
  ( sym: 281; act: -43 ),
  ( sym: 282; act: -43 ),
  ( sym: 283; act: -43 ),
  ( sym: 284; act: -43 ),
  ( sym: 289; act: -43 ),
  ( sym: 290; act: -43 ),
  ( sym: 291; act: -43 ),
  ( sym: 292; act: -43 ),
  ( sym: 293; act: -43 ),
  ( sym: 294; act: -43 ),
  ( sym: 295; act: -43 ),
  ( sym: 308; act: -43 ),
  ( sym: 313; act: -43 ),
{ 53: }
{ 54: }
{ 55: }
  ( sym: 271; act: 23 ),
  ( sym: 267; act: -138 ),
{ 56: }
{ 57: }
  ( sym: 277; act: 94 ),
  ( sym: 256; act: -59 ),
  ( sym: 261; act: -59 ),
  ( sym: 262; act: -59 ),
  ( sym: 263; act: -59 ),
  ( sym: 264; act: -59 ),
  ( sym: 271; act: -59 ),
  ( sym: 281; act: -59 ),
  ( sym: 282; act: -59 ),
  ( sym: 283; act: -59 ),
  ( sym: 284; act: -59 ),
  ( sym: 289; act: -59 ),
  ( sym: 290; act: -59 ),
  ( sym: 291; act: -59 ),
  ( sym: 292; act: -59 ),
  ( sym: 293; act: -59 ),
  ( sym: 294; act: -59 ),
  ( sym: 295; act: -59 ),
  ( sym: 308; act: -59 ),
  ( sym: 313; act: -59 ),
{ 58: }
{ 59: }
{ 60: }
{ 61: }
{ 62: }
{ 63: }
{ 64: }
  ( sym: 256; act: 76 ),
  ( sym: 262; act: 77 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 78 ),
  ( sym: 282; act: 79 ),
  ( sym: 283; act: 80 ),
  ( sym: 284; act: 81 ),
  ( sym: 308; act: 82 ),
  ( sym: 313; act: 83 ),
{ 65: }
{ 66: }
  ( sym: 271; act: 23 ),
  ( sym: 263; act: -138 ),
{ 67: }
{ 68: }
  ( sym: 262; act: 100 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 101 ),
  ( sym: 273; act: 102 ),
  ( sym: 286; act: 103 ),
  ( sym: 308; act: 104 ),
  ( sym: 310; act: 105 ),
  ( sym: 315; act: 106 ),
{ 69: }
  ( sym: 297; act: 107 ),
  ( sym: 256; act: -34 ),
  ( sym: 260; act: -34 ),
  ( sym: 261; act: -34 ),
  ( sym: 262; act: -34 ),
  ( sym: 263; act: -34 ),
  ( sym: 264; act: -34 ),
  ( sym: 271; act: -34 ),
  ( sym: 281; act: -34 ),
  ( sym: 282; act: -34 ),
  ( sym: 283; act: -34 ),
  ( sym: 284; act: -34 ),
  ( sym: 289; act: -34 ),
  ( sym: 290; act: -34 ),
  ( sym: 291; act: -34 ),
  ( sym: 292; act: -34 ),
  ( sym: 293; act: -34 ),
  ( sym: 294; act: -34 ),
  ( sym: 295; act: -34 ),
  ( sym: 308; act: -34 ),
  ( sym: 313; act: -34 ),
{ 70: }
  ( sym: 297; act: 108 ),
  ( sym: 256; act: -36 ),
  ( sym: 260; act: -36 ),
  ( sym: 261; act: -36 ),
  ( sym: 262; act: -36 ),
  ( sym: 263; act: -36 ),
  ( sym: 264; act: -36 ),
  ( sym: 271; act: -36 ),
  ( sym: 281; act: -36 ),
  ( sym: 282; act: -36 ),
  ( sym: 283; act: -36 ),
  ( sym: 284; act: -36 ),
  ( sym: 289; act: -36 ),
  ( sym: 290; act: -36 ),
  ( sym: 291; act: -36 ),
  ( sym: 292; act: -36 ),
  ( sym: 293; act: -36 ),
  ( sym: 294; act: -36 ),
  ( sym: 295; act: -36 ),
  ( sym: 308; act: -36 ),
  ( sym: 313; act: -36 ),
{ 71: }
{ 72: }
  ( sym: 313; act: 109 ),
{ 73: }
  ( sym: 262; act: 111 ),
  ( sym: 264; act: 112 ),
  ( sym: 260; act: -71 ),
  ( sym: 261; act: -71 ),
  ( sym: 296; act: -71 ),
{ 74: }
  ( sym: 261; act: 114 ),
  ( sym: 296; act: 115 ),
  ( sym: 260; act: -18 ),
{ 75: }
  ( sym: 259; act: 117 ),
  ( sym: 260; act: -86 ),
  ( sym: 261; act: -86 ),
  ( sym: 262; act: -86 ),
  ( sym: 263; act: -86 ),
  ( sym: 264; act: -86 ),
  ( sym: 296; act: -86 ),
{ 76: }
{ 77: }
  ( sym: 262; act: 77 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 78 ),
  ( sym: 282; act: 79 ),
  ( sym: 283; act: 80 ),
  ( sym: 284; act: 81 ),
  ( sym: 308; act: 82 ),
  ( sym: 313; act: 83 ),
{ 78: }
  ( sym: 262; act: 77 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 78 ),
  ( sym: 282; act: 79 ),
  ( sym: 283; act: 80 ),
  ( sym: 284; act: 81 ),
  ( sym: 308; act: 82 ),
  ( sym: 313; act: 83 ),
{ 79: }
{ 80: }
{ 81: }
{ 82: }
  ( sym: 262; act: 77 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 78 ),
  ( sym: 282; act: 79 ),
  ( sym: 283; act: 80 ),
  ( sym: 284; act: 81 ),
  ( sym: 308; act: 82 ),
  ( sym: 313; act: 83 ),
{ 83: }
  ( sym: 262; act: 77 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 78 ),
  ( sym: 282; act: 79 ),
  ( sym: 283; act: 80 ),
  ( sym: 284; act: 81 ),
  ( sym: 308; act: 82 ),
  ( sym: 313; act: 83 ),
{ 84: }
{ 85: }
  ( sym: 267; act: 123 ),
{ 86: }
  ( sym: 268; act: 20 ),
  ( sym: 269; act: 21 ),
  ( sym: 270; act: 22 ),
  ( sym: 271; act: 23 ),
  ( sym: 274; act: 24 ),
  ( sym: 275; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 277; act: 27 ),
  ( sym: 278; act: 28 ),
  ( sym: 279; act: 29 ),
  ( sym: 280; act: 30 ),
  ( sym: 281; act: 31 ),
  ( sym: 267; act: -50 ),
{ 87: }
  ( sym: 267; act: 125 ),
{ 88: }
  ( sym: 256; act: 76 ),
  ( sym: 262; act: 77 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 78 ),
  ( sym: 282; act: 79 ),
  ( sym: 283; act: 80 ),
  ( sym: 284; act: 81 ),
  ( sym: 308; act: 82 ),
  ( sym: 313; act: 83 ),
{ 89: }
{ 90: }
  ( sym: 267; act: 127 ),
{ 91: }
  ( sym: 261; act: 128 ),
  ( sym: 263; act: -137 ),
  ( sym: 267; act: -137 ),
{ 92: }
  ( sym: 267; act: 129 ),
{ 93: }
  ( sym: 285; act: 130 ),
  ( sym: 261; act: -140 ),
  ( sym: 263; act: -140 ),
  ( sym: 267; act: -140 ),
{ 94: }
{ 95: }
  ( sym: 260; act: 131 ),
  ( sym: 261; act: 114 ),
{ 96: }
  ( sym: 263; act: 132 ),
{ 97: }
  ( sym: 318; act: 133 ),
  ( sym: 319; act: 134 ),
  ( sym: 286; act: -141 ),
{ 98: }
  ( sym: 286; act: 135 ),
{ 99: }
  ( sym: 262; act: 136 ),
  ( sym: 259; act: -122 ),
  ( sym: 260; act: -122 ),
  ( sym: 261; act: -122 ),
  ( sym: 263; act: -122 ),
  ( sym: 264; act: -122 ),
  ( sym: 265; act: -122 ),
  ( sym: 267; act: -122 ),
  ( sym: 286; act: -122 ),
  ( sym: 296; act: -122 ),
  ( sym: 300; act: -122 ),
  ( sym: 301; act: -122 ),
  ( sym: 302; act: -122 ),
  ( sym: 303; act: -122 ),
  ( sym: 304; act: -122 ),
  ( sym: 305; act: -122 ),
  ( sym: 306; act: -122 ),
  ( sym: 307; act: -122 ),
  ( sym: 308; act: -122 ),
  ( sym: 309; act: -122 ),
  ( sym: 310; act: -122 ),
  ( sym: 311; act: -122 ),
  ( sym: 312; act: -122 ),
  ( sym: 313; act: -122 ),
  ( sym: 314; act: -122 ),
  ( sym: 315; act: -122 ),
  ( sym: 318; act: -122 ),
  ( sym: 319; act: -122 ),
{ 100: }
  ( sym: 262; act: 100 ),
  ( sym: 268; act: 20 ),
  ( sym: 269; act: 21 ),
  ( sym: 270; act: 22 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 101 ),
  ( sym: 273; act: 102 ),
  ( sym: 274; act: 24 ),
  ( sym: 275; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 277; act: 27 ),
  ( sym: 278; act: 28 ),
  ( sym: 279; act: 29 ),
  ( sym: 280; act: 30 ),
  ( sym: 281; act: 31 ),
  ( sym: 308; act: 104 ),
  ( sym: 310; act: 105 ),
  ( sym: 315; act: 106 ),
{ 101: }
{ 102: }
{ 103: }
{ 104: }
  ( sym: 262; act: 100 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 101 ),
  ( sym: 273; act: 102 ),
  ( sym: 308; act: 104 ),
  ( sym: 310; act: 105 ),
  ( sym: 315; act: 106 ),
{ 105: }
  ( sym: 262; act: 100 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 101 ),
  ( sym: 273; act: 102 ),
  ( sym: 308; act: 104 ),
  ( sym: 310; act: 105 ),
  ( sym: 315; act: 106 ),
{ 106: }
  ( sym: 262; act: 100 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 101 ),
  ( sym: 273; act: 102 ),
  ( sym: 308; act: 104 ),
  ( sym: 310; act: 105 ),
  ( sym: 315; act: 106 ),
{ 107: }
{ 108: }
{ 109: }
  ( sym: 262; act: 77 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 78 ),
  ( sym: 282; act: 79 ),
  ( sym: 283; act: 80 ),
  ( sym: 284; act: 81 ),
  ( sym: 308; act: 82 ),
  ( sym: 313; act: 83 ),
{ 110: }
{ 111: }
  ( sym: 263; act: 149 ),
  ( sym: 268; act: 20 ),
  ( sym: 269; act: 21 ),
  ( sym: 270; act: 22 ),
  ( sym: 271; act: 23 ),
  ( sym: 274; act: 24 ),
  ( sym: 275; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 277; act: 27 ),
  ( sym: 278; act: 28 ),
  ( sym: 279; act: 29 ),
  ( sym: 280; act: 150 ),
  ( sym: 281; act: 31 ),
  ( sym: 298; act: 151 ),
{ 112: }
  ( sym: 262; act: 100 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 101 ),
  ( sym: 273; act: 102 ),
  ( sym: 308; act: 104 ),
  ( sym: 310; act: 105 ),
  ( sym: 315; act: 106 ),
{ 113: }
  ( sym: 260; act: 154 ),
{ 114: }
  ( sym: 262; act: 77 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 78 ),
  ( sym: 282; act: 79 ),
  ( sym: 283; act: 80 ),
  ( sym: 284; act: 81 ),
  ( sym: 308; act: 82 ),
  ( sym: 313; act: 83 ),
{ 115: }
  ( sym: 262; act: 156 ),
{ 116: }
  ( sym: 262; act: 100 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 101 ),
  ( sym: 273; act: 102 ),
  ( sym: 308; act: 104 ),
  ( sym: 310; act: 105 ),
  ( sym: 315; act: 106 ),
{ 117: }
  ( sym: 262; act: 100 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 101 ),
  ( sym: 273; act: 102 ),
  ( sym: 308; act: 104 ),
  ( sym: 310; act: 105 ),
  ( sym: 315; act: 106 ),
{ 118: }
  ( sym: 261; act: 159 ),
  ( sym: 260; act: -70 ),
  ( sym: 296; act: -70 ),
{ 119: }
  ( sym: 262; act: 111 ),
  ( sym: 263; act: 160 ),
  ( sym: 264; act: 112 ),
{ 120: }
  ( sym: 262; act: 111 ),
  ( sym: 264; act: 112 ),
  ( sym: 260; act: -80 ),
  ( sym: 261; act: -80 ),
  ( sym: 263; act: -80 ),
  ( sym: 296; act: -80 ),
{ 121: }
  ( sym: 264; act: 112 ),
  ( sym: 260; act: -83 ),
  ( sym: 261; act: -83 ),
  ( sym: 262; act: -83 ),
  ( sym: 263; act: -83 ),
  ( sym: 296; act: -83 ),
{ 122: }
  ( sym: 262; act: 111 ),
  ( sym: 264; act: 112 ),
  ( sym: 260; act: -82 ),
  ( sym: 261; act: -82 ),
  ( sym: 263; act: -82 ),
  ( sym: 296; act: -82 ),
{ 123: }
{ 124: }
{ 125: }
{ 126: }
  ( sym: 260; act: 161 ),
  ( sym: 261; act: 114 ),
{ 127: }
{ 128: }
  ( sym: 271; act: 23 ),
  ( sym: 263; act: -138 ),
  ( sym: 267; act: -138 ),
{ 129: }
{ 130: }
  ( sym: 262; act: 100 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 101 ),
  ( sym: 273; act: 102 ),
  ( sym: 308; act: 104 ),
  ( sym: 310; act: 105 ),
  ( sym: 315; act: 106 ),
{ 131: }
{ 132: }
  ( sym: 287; act: 164 ),
{ 133: }
  ( sym: 262; act: 100 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 101 ),
  ( sym: 273; act: 102 ),
  ( sym: 308; act: 104 ),
  ( sym: 310; act: 105 ),
  ( sym: 315; act: 106 ),
{ 134: }
  ( sym: 262; act: 100 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 101 ),
  ( sym: 273; act: 102 ),
  ( sym: 308; act: 104 ),
  ( sym: 310; act: 105 ),
  ( sym: 315; act: 106 ),
{ 135: }
{ 136: }
  ( sym: 262; act: 100 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 101 ),
  ( sym: 273; act: 102 ),
  ( sym: 308; act: 104 ),
  ( sym: 310; act: 105 ),
  ( sym: 315; act: 106 ),
  ( sym: 263; act: -144 ),
{ 137: }
  ( sym: 318; act: 133 ),
  ( sym: 319; act: 134 ),
  ( sym: 259; act: -118 ),
  ( sym: 260; act: -118 ),
  ( sym: 261; act: -118 ),
  ( sym: 262; act: -118 ),
  ( sym: 263; act: -118 ),
  ( sym: 264; act: -118 ),
  ( sym: 265; act: -118 ),
  ( sym: 267; act: -118 ),
  ( sym: 286; act: -118 ),
  ( sym: 296; act: -118 ),
  ( sym: 300; act: -118 ),
  ( sym: 301; act: -118 ),
  ( sym: 302; act: -118 ),
  ( sym: 303; act: -118 ),
  ( sym: 304; act: -118 ),
  ( sym: 305; act: -118 ),
  ( sym: 306; act: -118 ),
  ( sym: 307; act: -118 ),
  ( sym: 308; act: -118 ),
  ( sym: 309; act: -118 ),
  ( sym: 310; act: -118 ),
  ( sym: 311; act: -118 ),
  ( sym: 312; act: -118 ),
  ( sym: 313; act: -118 ),
  ( sym: 314; act: -118 ),
  ( sym: 315; act: -118 ),
{ 138: }
  ( sym: 263; act: 170 ),
  ( sym: 300; act: -101 ),
  ( sym: 301; act: -101 ),
  ( sym: 302; act: -101 ),
  ( sym: 303; act: -101 ),
  ( sym: 304; act: -101 ),
  ( sym: 305; act: -101 ),
  ( sym: 306; act: -101 ),
  ( sym: 307; act: -101 ),
  ( sym: 308; act: -101 ),
  ( sym: 309; act: -101 ),
  ( sym: 310; act: -101 ),
  ( sym: 311; act: -101 ),
  ( sym: 312; act: -101 ),
  ( sym: 313; act: -101 ),
  ( sym: 314; act: -101 ),
  ( sym: 315; act: -101 ),
{ 139: }
  ( sym: 300; act: 171 ),
  ( sym: 301; act: 172 ),
  ( sym: 302; act: 173 ),
  ( sym: 303; act: 174 ),
  ( sym: 304; act: 175 ),
  ( sym: 305; act: 176 ),
  ( sym: 306; act: 177 ),
  ( sym: 307; act: 178 ),
  ( sym: 308; act: 179 ),
  ( sym: 309; act: 180 ),
  ( sym: 310; act: 181 ),
  ( sym: 311; act: 182 ),
  ( sym: 312; act: 183 ),
  ( sym: 313; act: 184 ),
  ( sym: 314; act: 185 ),
  ( sym: 315; act: 186 ),
{ 140: }
  ( sym: 263; act: 188 ),
  ( sym: 282; act: 79 ),
  ( sym: 283; act: 80 ),
  ( sym: 284; act: 81 ),
  ( sym: 313; act: 189 ),
{ 141: }
  ( sym: 262; act: 136 ),
  ( sym: 263; act: 190 ),
  ( sym: 282; act: -67 ),
  ( sym: 283; act: -67 ),
  ( sym: 284; act: -67 ),
  ( sym: 313; act: -67 ),
  ( sym: 300; act: -122 ),
  ( sym: 301; act: -122 ),
  ( sym: 302; act: -122 ),
  ( sym: 303; act: -122 ),
  ( sym: 304; act: -122 ),
  ( sym: 305; act: -122 ),
  ( sym: 306; act: -122 ),
  ( sym: 307; act: -122 ),
  ( sym: 308; act: -122 ),
  ( sym: 309; act: -122 ),
  ( sym: 310; act: -122 ),
  ( sym: 311; act: -122 ),
  ( sym: 312; act: -122 ),
  ( sym: 314; act: -122 ),
  ( sym: 315; act: -122 ),
  ( sym: 318; act: -122 ),
  ( sym: 319; act: -122 ),
{ 142: }
  ( sym: 318; act: 133 ),
  ( sym: 319; act: 134 ),
  ( sym: 259; act: -128 ),
  ( sym: 260; act: -128 ),
  ( sym: 261; act: -128 ),
  ( sym: 262; act: -128 ),
  ( sym: 263; act: -128 ),
  ( sym: 264; act: -128 ),
  ( sym: 265; act: -128 ),
  ( sym: 267; act: -128 ),
  ( sym: 286; act: -128 ),
  ( sym: 296; act: -128 ),
  ( sym: 300; act: -128 ),
  ( sym: 301; act: -128 ),
  ( sym: 302; act: -128 ),
  ( sym: 303; act: -128 ),
  ( sym: 304; act: -128 ),
  ( sym: 305; act: -128 ),
  ( sym: 306; act: -128 ),
  ( sym: 307; act: -128 ),
  ( sym: 308; act: -128 ),
  ( sym: 309; act: -128 ),
  ( sym: 310; act: -128 ),
  ( sym: 311; act: -128 ),
  ( sym: 312; act: -128 ),
  ( sym: 313; act: -128 ),
  ( sym: 314; act: -128 ),
  ( sym: 315; act: -128 ),
{ 143: }
  ( sym: 318; act: 133 ),
  ( sym: 319; act: 134 ),
  ( sym: 259; act: -127 ),
  ( sym: 260; act: -127 ),
  ( sym: 261; act: -127 ),
  ( sym: 262; act: -127 ),
  ( sym: 263; act: -127 ),
  ( sym: 264; act: -127 ),
  ( sym: 265; act: -127 ),
  ( sym: 267; act: -127 ),
  ( sym: 286; act: -127 ),
  ( sym: 296; act: -127 ),
  ( sym: 300; act: -127 ),
  ( sym: 301; act: -127 ),
  ( sym: 302; act: -127 ),
  ( sym: 303; act: -127 ),
  ( sym: 304; act: -127 ),
  ( sym: 305; act: -127 ),
  ( sym: 306; act: -127 ),
  ( sym: 307; act: -127 ),
  ( sym: 308; act: -127 ),
  ( sym: 309; act: -127 ),
  ( sym: 310; act: -127 ),
  ( sym: 311; act: -127 ),
  ( sym: 312; act: -127 ),
  ( sym: 313; act: -127 ),
  ( sym: 314; act: -127 ),
  ( sym: 315; act: -127 ),
{ 144: }
  ( sym: 318; act: 133 ),
  ( sym: 319; act: 134 ),
  ( sym: 259; act: -129 ),
  ( sym: 260; act: -129 ),
  ( sym: 261; act: -129 ),
  ( sym: 262; act: -129 ),
  ( sym: 263; act: -129 ),
  ( sym: 264; act: -129 ),
  ( sym: 265; act: -129 ),
  ( sym: 267; act: -129 ),
  ( sym: 286; act: -129 ),
  ( sym: 296; act: -129 ),
  ( sym: 300; act: -129 ),
  ( sym: 301; act: -129 ),
  ( sym: 302; act: -129 ),
  ( sym: 303; act: -129 ),
  ( sym: 304; act: -129 ),
  ( sym: 305; act: -129 ),
  ( sym: 306; act: -129 ),
  ( sym: 307; act: -129 ),
  ( sym: 308; act: -129 ),
  ( sym: 309; act: -129 ),
  ( sym: 310; act: -129 ),
  ( sym: 311; act: -129 ),
  ( sym: 312; act: -129 ),
  ( sym: 313; act: -129 ),
  ( sym: 314; act: -129 ),
  ( sym: 315; act: -129 ),
{ 145: }
  ( sym: 262; act: 111 ),
  ( sym: 264; act: 112 ),
  ( sym: 260; act: -81 ),
  ( sym: 261; act: -81 ),
  ( sym: 263; act: -81 ),
  ( sym: 296; act: -81 ),
{ 146: }
  ( sym: 263; act: 191 ),
{ 147: }
  ( sym: 261; act: 192 ),
  ( sym: 263; act: -74 ),
{ 148: }
  ( sym: 262; act: 196 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 197 ),
  ( sym: 282; act: 79 ),
  ( sym: 283; act: 80 ),
  ( sym: 284; act: 81 ),
  ( sym: 308; act: 82 ),
  ( sym: 313; act: 198 ),
  ( sym: 261; act: -100 ),
  ( sym: 263; act: -100 ),
  ( sym: 264; act: -100 ),
{ 149: }
{ 150: }
  ( sym: 263; act: 199 ),
  ( sym: 261; act: -63 ),
  ( sym: 262; act: -63 ),
  ( sym: 264; act: -63 ),
  ( sym: 271; act: -63 ),
  ( sym: 281; act: -63 ),
  ( sym: 282; act: -63 ),
  ( sym: 283; act: -63 ),
  ( sym: 284; act: -63 ),
  ( sym: 308; act: -63 ),
  ( sym: 313; act: -63 ),
{ 151: }
{ 152: }
{ 153: }
  ( sym: 265; act: 200 ),
  ( sym: 300; act: 171 ),
  ( sym: 301; act: 172 ),
  ( sym: 302; act: 173 ),
  ( sym: 303; act: 174 ),
  ( sym: 304; act: 175 ),
  ( sym: 305; act: 176 ),
  ( sym: 306; act: 177 ),
  ( sym: 307; act: 178 ),
  ( sym: 308; act: 179 ),
  ( sym: 309; act: 180 ),
  ( sym: 310; act: 181 ),
  ( sym: 311; act: 182 ),
  ( sym: 312; act: 183 ),
  ( sym: 313; act: 184 ),
  ( sym: 314; act: 185 ),
  ( sym: 315; act: 186 ),
{ 154: }
{ 155: }
  ( sym: 262; act: 111 ),
  ( sym: 264; act: 112 ),
  ( sym: 260; act: -68 ),
  ( sym: 261; act: -68 ),
  ( sym: 296; act: -68 ),
{ 156: }
  ( sym: 271; act: 23 ),
{ 157: }
  ( sym: 300; act: 171 ),
  ( sym: 301; act: 172 ),
  ( sym: 302; act: 173 ),
  ( sym: 303; act: 174 ),
  ( sym: 304; act: 175 ),
  ( sym: 305; act: 176 ),
  ( sym: 306; act: 177 ),
  ( sym: 307; act: 178 ),
  ( sym: 308; act: 179 ),
  ( sym: 309; act: 180 ),
  ( sym: 310; act: 181 ),
  ( sym: 311; act: 182 ),
  ( sym: 312; act: 183 ),
  ( sym: 313; act: 184 ),
  ( sym: 314; act: 185 ),
  ( sym: 315; act: 186 ),
  ( sym: 260; act: -85 ),
  ( sym: 261; act: -85 ),
  ( sym: 262; act: -85 ),
  ( sym: 263; act: -85 ),
  ( sym: 264; act: -85 ),
  ( sym: 296; act: -85 ),
{ 158: }
  ( sym: 300; act: 171 ),
  ( sym: 301; act: 172 ),
  ( sym: 302; act: 173 ),
  ( sym: 303; act: 174 ),
  ( sym: 304; act: 175 ),
  ( sym: 305; act: 176 ),
  ( sym: 306; act: 177 ),
  ( sym: 307; act: 178 ),
  ( sym: 308; act: 179 ),
  ( sym: 309; act: 180 ),
  ( sym: 310; act: 181 ),
  ( sym: 311; act: 182 ),
  ( sym: 312; act: 183 ),
  ( sym: 313; act: 184 ),
  ( sym: 314; act: 185 ),
  ( sym: 315; act: 186 ),
  ( sym: 260; act: -84 ),
  ( sym: 261; act: -84 ),
  ( sym: 262; act: -84 ),
  ( sym: 263; act: -84 ),
  ( sym: 264; act: -84 ),
  ( sym: 296; act: -84 ),
{ 159: }
  ( sym: 256; act: 76 ),
  ( sym: 262; act: 77 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 78 ),
  ( sym: 282; act: 79 ),
  ( sym: 283; act: 80 ),
  ( sym: 284; act: 81 ),
  ( sym: 308; act: 82 ),
  ( sym: 313; act: 83 ),
{ 160: }
{ 161: }
{ 162: }
{ 163: }
  ( sym: 300; act: 171 ),
  ( sym: 301; act: 172 ),
  ( sym: 302; act: 173 ),
  ( sym: 303; act: 174 ),
  ( sym: 304; act: 175 ),
  ( sym: 305; act: 176 ),
  ( sym: 306; act: 177 ),
  ( sym: 307; act: 178 ),
  ( sym: 308; act: 179 ),
  ( sym: 309; act: 180 ),
  ( sym: 310; act: 181 ),
  ( sym: 311; act: 182 ),
  ( sym: 312; act: 183 ),
  ( sym: 313; act: 184 ),
  ( sym: 314; act: 185 ),
  ( sym: 315; act: 186 ),
  ( sym: 261; act: -139 ),
  ( sym: 263; act: -139 ),
  ( sym: 267; act: -139 ),
{ 164: }
  ( sym: 262; act: 100 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 101 ),
  ( sym: 273; act: 102 ),
  ( sym: 308; act: 104 ),
  ( sym: 310; act: 105 ),
  ( sym: 315; act: 106 ),
{ 165: }
{ 166: }
{ 167: }
  ( sym: 261; act: 204 ),
  ( sym: 263; act: -143 ),
{ 168: }
  ( sym: 263; act: 205 ),
{ 169: }
  ( sym: 300; act: 171 ),
  ( sym: 301; act: 172 ),
  ( sym: 302; act: 173 ),
  ( sym: 303; act: 174 ),
  ( sym: 304; act: 175 ),
  ( sym: 305; act: 176 ),
  ( sym: 306; act: 177 ),
  ( sym: 307; act: 178 ),
  ( sym: 308; act: 179 ),
  ( sym: 309; act: 180 ),
  ( sym: 310; act: 181 ),
  ( sym: 311; act: 182 ),
  ( sym: 312; act: 183 ),
  ( sym: 313; act: 184 ),
  ( sym: 314; act: 185 ),
  ( sym: 315; act: 186 ),
  ( sym: 261; act: -145 ),
  ( sym: 263; act: -145 ),
{ 170: }
{ 171: }
  ( sym: 262; act: 100 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 101 ),
  ( sym: 273; act: 102 ),
  ( sym: 308; act: 104 ),
  ( sym: 310; act: 105 ),
  ( sym: 315; act: 106 ),
{ 172: }
  ( sym: 262; act: 100 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 101 ),
  ( sym: 273; act: 102 ),
  ( sym: 308; act: 104 ),
  ( sym: 310; act: 105 ),
  ( sym: 315; act: 106 ),
{ 173: }
  ( sym: 262; act: 100 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 101 ),
  ( sym: 273; act: 102 ),
  ( sym: 308; act: 104 ),
  ( sym: 310; act: 105 ),
  ( sym: 315; act: 106 ),
{ 174: }
  ( sym: 262; act: 100 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 101 ),
  ( sym: 273; act: 102 ),
  ( sym: 308; act: 104 ),
  ( sym: 310; act: 105 ),
  ( sym: 315; act: 106 ),
{ 175: }
  ( sym: 262; act: 100 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 101 ),
  ( sym: 273; act: 102 ),
  ( sym: 308; act: 104 ),
  ( sym: 310; act: 105 ),
  ( sym: 315; act: 106 ),
{ 176: }
  ( sym: 262; act: 100 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 101 ),
  ( sym: 273; act: 102 ),
  ( sym: 308; act: 104 ),
  ( sym: 310; act: 105 ),
  ( sym: 315; act: 106 ),
{ 177: }
  ( sym: 262; act: 100 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 101 ),
  ( sym: 273; act: 102 ),
  ( sym: 308; act: 104 ),
  ( sym: 310; act: 105 ),
  ( sym: 315; act: 106 ),
{ 178: }
  ( sym: 262; act: 100 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 101 ),
  ( sym: 273; act: 102 ),
  ( sym: 308; act: 104 ),
  ( sym: 310; act: 105 ),
  ( sym: 315; act: 106 ),
{ 179: }
  ( sym: 262; act: 100 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 101 ),
  ( sym: 273; act: 102 ),
  ( sym: 308; act: 104 ),
  ( sym: 310; act: 105 ),
  ( sym: 315; act: 106 ),
{ 180: }
  ( sym: 262; act: 100 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 101 ),
  ( sym: 273; act: 102 ),
  ( sym: 308; act: 104 ),
  ( sym: 310; act: 105 ),
  ( sym: 315; act: 106 ),
{ 181: }
  ( sym: 262; act: 100 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 101 ),
  ( sym: 273; act: 102 ),
  ( sym: 308; act: 104 ),
  ( sym: 310; act: 105 ),
  ( sym: 315; act: 106 ),
{ 182: }
  ( sym: 262; act: 100 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 101 ),
  ( sym: 273; act: 102 ),
  ( sym: 308; act: 104 ),
  ( sym: 310; act: 105 ),
  ( sym: 315; act: 106 ),
{ 183: }
  ( sym: 262; act: 100 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 101 ),
  ( sym: 273; act: 102 ),
  ( sym: 308; act: 104 ),
  ( sym: 310; act: 105 ),
  ( sym: 315; act: 106 ),
{ 184: }
  ( sym: 262; act: 100 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 101 ),
  ( sym: 273; act: 102 ),
  ( sym: 308; act: 104 ),
  ( sym: 310; act: 105 ),
  ( sym: 315; act: 106 ),
{ 185: }
  ( sym: 262; act: 100 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 101 ),
  ( sym: 273; act: 102 ),
  ( sym: 308; act: 104 ),
  ( sym: 310; act: 105 ),
  ( sym: 315; act: 106 ),
{ 186: }
  ( sym: 262; act: 100 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 101 ),
  ( sym: 273; act: 102 ),
  ( sym: 308; act: 104 ),
  ( sym: 310; act: 105 ),
  ( sym: 315; act: 106 ),
{ 187: }
  ( sym: 313; act: 223 ),
{ 188: }
  ( sym: 262; act: 100 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 101 ),
  ( sym: 273; act: 102 ),
  ( sym: 308; act: 104 ),
  ( sym: 310; act: 105 ),
  ( sym: 315; act: 106 ),
{ 189: }
  ( sym: 263; act: 225 ),
{ 190: }
  ( sym: 262; act: 100 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 101 ),
  ( sym: 273; act: 102 ),
  ( sym: 308; act: 104 ),
  ( sym: 310; act: 105 ),
  ( sym: 315; act: 106 ),
  ( sym: 259; act: -121 ),
  ( sym: 260; act: -121 ),
  ( sym: 261; act: -121 ),
  ( sym: 263; act: -121 ),
  ( sym: 264; act: -121 ),
  ( sym: 265; act: -121 ),
  ( sym: 267; act: -121 ),
  ( sym: 286; act: -121 ),
  ( sym: 296; act: -121 ),
  ( sym: 300; act: -121 ),
  ( sym: 301; act: -121 ),
  ( sym: 302; act: -121 ),
  ( sym: 303; act: -121 ),
  ( sym: 304; act: -121 ),
  ( sym: 305; act: -121 ),
  ( sym: 306; act: -121 ),
  ( sym: 307; act: -121 ),
  ( sym: 309; act: -121 ),
  ( sym: 311; act: -121 ),
  ( sym: 312; act: -121 ),
  ( sym: 313; act: -121 ),
  ( sym: 314; act: -121 ),
  ( sym: 318; act: -121 ),
  ( sym: 319; act: -121 ),
{ 191: }
{ 192: }
  ( sym: 268; act: 20 ),
  ( sym: 269; act: 21 ),
  ( sym: 270; act: 22 ),
  ( sym: 271; act: 23 ),
  ( sym: 274; act: 24 ),
  ( sym: 275; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 277; act: 27 ),
  ( sym: 278; act: 28 ),
  ( sym: 279; act: 29 ),
  ( sym: 280; act: 30 ),
  ( sym: 281; act: 31 ),
  ( sym: 298; act: 151 ),
{ 193: }
  ( sym: 313; act: 229 ),
{ 194: }
  ( sym: 262; act: 231 ),
  ( sym: 264; act: 232 ),
  ( sym: 261; act: -73 ),
  ( sym: 263; act: -73 ),
{ 195: }
  ( sym: 262; act: 111 ),
  ( sym: 264; act: 112 ),
  ( sym: 261; act: -72 ),
  ( sym: 263; act: -72 ),
{ 196: }
  ( sym: 262; act: 196 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 197 ),
  ( sym: 282; act: 79 ),
  ( sym: 283; act: 80 ),
  ( sym: 284; act: 81 ),
  ( sym: 308; act: 82 ),
  ( sym: 313; act: 198 ),
  ( sym: 263; act: -100 ),
  ( sym: 264; act: -100 ),
{ 197: }
  ( sym: 262; act: 196 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 197 ),
  ( sym: 282; act: 79 ),
  ( sym: 283; act: 80 ),
  ( sym: 284; act: 81 ),
  ( sym: 308; act: 82 ),
  ( sym: 313; act: 198 ),
  ( sym: 261; act: -100 ),
  ( sym: 263; act: -100 ),
  ( sym: 264; act: -100 ),
{ 198: }
  ( sym: 262; act: 196 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 197 ),
  ( sym: 282; act: 79 ),
  ( sym: 283; act: 80 ),
  ( sym: 284; act: 81 ),
  ( sym: 308; act: 82 ),
  ( sym: 313; act: 198 ),
  ( sym: 261; act: -100 ),
  ( sym: 263; act: -100 ),
  ( sym: 264; act: -100 ),
{ 199: }
{ 200: }
{ 201: }
  ( sym: 263; act: 236 ),
{ 202: }
{ 203: }
  ( sym: 286; act: 237 ),
{ 204: }
  ( sym: 262; act: 100 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 101 ),
  ( sym: 273; act: 102 ),
  ( sym: 308; act: 104 ),
  ( sym: 310; act: 105 ),
  ( sym: 315; act: 106 ),
  ( sym: 263; act: -144 ),
{ 205: }
{ 206: }
  ( sym: 306; act: 177 ),
  ( sym: 307; act: 178 ),
  ( sym: 308; act: 179 ),
  ( sym: 309; act: 180 ),
  ( sym: 310; act: 181 ),
  ( sym: 311; act: 182 ),
  ( sym: 312; act: 183 ),
  ( sym: 313; act: 184 ),
  ( sym: 314; act: 185 ),
  ( sym: 315; act: 186 ),
  ( sym: 259; act: -102 ),
  ( sym: 260; act: -102 ),
  ( sym: 261; act: -102 ),
  ( sym: 262; act: -102 ),
  ( sym: 263; act: -102 ),
  ( sym: 264; act: -102 ),
  ( sym: 265; act: -102 ),
  ( sym: 267; act: -102 ),
  ( sym: 286; act: -102 ),
  ( sym: 296; act: -102 ),
  ( sym: 300; act: -102 ),
  ( sym: 301; act: -102 ),
  ( sym: 302; act: -102 ),
  ( sym: 303; act: -102 ),
  ( sym: 304; act: -102 ),
  ( sym: 305; act: -102 ),
  ( sym: 318; act: -102 ),
  ( sym: 319; act: -102 ),
{ 207: }
  ( sym: 306; act: 177 ),
  ( sym: 307; act: 178 ),
  ( sym: 308; act: 179 ),
  ( sym: 309; act: 180 ),
  ( sym: 310; act: 181 ),
  ( sym: 311; act: 182 ),
  ( sym: 312; act: 183 ),
  ( sym: 313; act: 184 ),
  ( sym: 314; act: 185 ),
  ( sym: 315; act: 186 ),
  ( sym: 259; act: -103 ),
  ( sym: 260; act: -103 ),
  ( sym: 261; act: -103 ),
  ( sym: 262; act: -103 ),
  ( sym: 263; act: -103 ),
  ( sym: 264; act: -103 ),
  ( sym: 265; act: -103 ),
  ( sym: 267; act: -103 ),
  ( sym: 286; act: -103 ),
  ( sym: 296; act: -103 ),
  ( sym: 300; act: -103 ),
  ( sym: 301; act: -103 ),
  ( sym: 302; act: -103 ),
  ( sym: 303; act: -103 ),
  ( sym: 304; act: -103 ),
  ( sym: 305; act: -103 ),
  ( sym: 318; act: -103 ),
  ( sym: 319; act: -103 ),
{ 208: }
  ( sym: 306; act: 177 ),
  ( sym: 307; act: 178 ),
  ( sym: 308; act: 179 ),
  ( sym: 309; act: 180 ),
  ( sym: 310; act: 181 ),
  ( sym: 311; act: 182 ),
  ( sym: 312; act: 183 ),
  ( sym: 313; act: 184 ),
  ( sym: 314; act: 185 ),
  ( sym: 315; act: 186 ),
  ( sym: 259; act: -104 ),
  ( sym: 260; act: -104 ),
  ( sym: 261; act: -104 ),
  ( sym: 262; act: -104 ),
  ( sym: 263; act: -104 ),
  ( sym: 264; act: -104 ),
  ( sym: 265; act: -104 ),
  ( sym: 267; act: -104 ),
  ( sym: 286; act: -104 ),
  ( sym: 296; act: -104 ),
  ( sym: 300; act: -104 ),
  ( sym: 301; act: -104 ),
  ( sym: 302; act: -104 ),
  ( sym: 303; act: -104 ),
  ( sym: 304; act: -104 ),
  ( sym: 305; act: -104 ),
  ( sym: 318; act: -104 ),
  ( sym: 319; act: -104 ),
{ 209: }
  ( sym: 306; act: 177 ),
  ( sym: 307; act: 178 ),
  ( sym: 308; act: 179 ),
  ( sym: 309; act: 180 ),
  ( sym: 310; act: 181 ),
  ( sym: 311; act: 182 ),
  ( sym: 312; act: 183 ),
  ( sym: 313; act: 184 ),
  ( sym: 314; act: 185 ),
  ( sym: 315; act: 186 ),
  ( sym: 259; act: -106 ),
  ( sym: 260; act: -106 ),
  ( sym: 261; act: -106 ),
  ( sym: 262; act: -106 ),
  ( sym: 263; act: -106 ),
  ( sym: 264; act: -106 ),
  ( sym: 265; act: -106 ),
  ( sym: 267; act: -106 ),
  ( sym: 286; act: -106 ),
  ( sym: 296; act: -106 ),
  ( sym: 300; act: -106 ),
  ( sym: 301; act: -106 ),
  ( sym: 302; act: -106 ),
  ( sym: 303; act: -106 ),
  ( sym: 304; act: -106 ),
  ( sym: 305; act: -106 ),
  ( sym: 318; act: -106 ),
  ( sym: 319; act: -106 ),
{ 210: }
  ( sym: 306; act: 177 ),
  ( sym: 307; act: 178 ),
  ( sym: 308; act: 179 ),
  ( sym: 309; act: 180 ),
  ( sym: 310; act: 181 ),
  ( sym: 311; act: 182 ),
  ( sym: 312; act: 183 ),
  ( sym: 313; act: 184 ),
  ( sym: 314; act: 185 ),
  ( sym: 315; act: 186 ),
  ( sym: 259; act: -105 ),
  ( sym: 260; act: -105 ),
  ( sym: 261; act: -105 ),
  ( sym: 262; act: -105 ),
  ( sym: 263; act: -105 ),
  ( sym: 264; act: -105 ),
  ( sym: 265; act: -105 ),
  ( sym: 267; act: -105 ),
  ( sym: 286; act: -105 ),
  ( sym: 296; act: -105 ),
  ( sym: 300; act: -105 ),
  ( sym: 301; act: -105 ),
  ( sym: 302; act: -105 ),
  ( sym: 303; act: -105 ),
  ( sym: 304; act: -105 ),
  ( sym: 305; act: -105 ),
  ( sym: 318; act: -105 ),
  ( sym: 319; act: -105 ),
{ 211: }
  ( sym: 306; act: 177 ),
  ( sym: 307; act: 178 ),
  ( sym: 308; act: 179 ),
  ( sym: 309; act: 180 ),
  ( sym: 310; act: 181 ),
  ( sym: 311; act: 182 ),
  ( sym: 312; act: 183 ),
  ( sym: 313; act: 184 ),
  ( sym: 314; act: 185 ),
  ( sym: 315; act: 186 ),
  ( sym: 259; act: -107 ),
  ( sym: 260; act: -107 ),
  ( sym: 261; act: -107 ),
  ( sym: 262; act: -107 ),
  ( sym: 263; act: -107 ),
  ( sym: 264; act: -107 ),
  ( sym: 265; act: -107 ),
  ( sym: 267; act: -107 ),
  ( sym: 286; act: -107 ),
  ( sym: 296; act: -107 ),
  ( sym: 300; act: -107 ),
  ( sym: 301; act: -107 ),
  ( sym: 302; act: -107 ),
  ( sym: 303; act: -107 ),
  ( sym: 304; act: -107 ),
  ( sym: 305; act: -107 ),
  ( sym: 318; act: -107 ),
  ( sym: 319; act: -107 ),
{ 212: }
{ 213: }
  ( sym: 259; act: 239 ),
  ( sym: 300; act: 171 ),
  ( sym: 301; act: 172 ),
  ( sym: 302; act: 173 ),
  ( sym: 303; act: 174 ),
  ( sym: 304; act: 175 ),
  ( sym: 305; act: 176 ),
  ( sym: 306; act: 177 ),
  ( sym: 307; act: 178 ),
  ( sym: 308; act: 179 ),
  ( sym: 309; act: 180 ),
  ( sym: 310; act: 181 ),
  ( sym: 311; act: 182 ),
  ( sym: 312; act: 183 ),
  ( sym: 313; act: 184 ),
  ( sym: 314; act: 185 ),
  ( sym: 315; act: 186 ),
{ 214: }
  ( sym: 308; act: 179 ),
  ( sym: 309; act: 180 ),
  ( sym: 310; act: 181 ),
  ( sym: 311; act: 182 ),
  ( sym: 312; act: 183 ),
  ( sym: 313; act: 184 ),
  ( sym: 314; act: 185 ),
  ( sym: 315; act: 186 ),
  ( sym: 259; act: -112 ),
  ( sym: 260; act: -112 ),
  ( sym: 261; act: -112 ),
  ( sym: 262; act: -112 ),
  ( sym: 263; act: -112 ),
  ( sym: 264; act: -112 ),
  ( sym: 265; act: -112 ),
  ( sym: 267; act: -112 ),
  ( sym: 286; act: -112 ),
  ( sym: 296; act: -112 ),
  ( sym: 300; act: -112 ),
  ( sym: 301; act: -112 ),
  ( sym: 302; act: -112 ),
  ( sym: 303; act: -112 ),
  ( sym: 304; act: -112 ),
  ( sym: 305; act: -112 ),
  ( sym: 306; act: -112 ),
  ( sym: 307; act: -112 ),
  ( sym: 318; act: -112 ),
  ( sym: 319; act: -112 ),
{ 215: }
  ( sym: 309; act: 180 ),
  ( sym: 310; act: 181 ),
  ( sym: 311; act: 182 ),
  ( sym: 312; act: 183 ),
  ( sym: 313; act: 184 ),
  ( sym: 314; act: 185 ),
  ( sym: 315; act: 186 ),
  ( sym: 259; act: -113 ),
  ( sym: 260; act: -113 ),
  ( sym: 261; act: -113 ),
  ( sym: 262; act: -113 ),
  ( sym: 263; act: -113 ),
  ( sym: 264; act: -113 ),
  ( sym: 265; act: -113 ),
  ( sym: 267; act: -113 ),
  ( sym: 286; act: -113 ),
  ( sym: 296; act: -113 ),
  ( sym: 300; act: -113 ),
  ( sym: 301; act: -113 ),
  ( sym: 302; act: -113 ),
  ( sym: 303; act: -113 ),
  ( sym: 304; act: -113 ),
  ( sym: 305; act: -113 ),
  ( sym: 306; act: -113 ),
  ( sym: 307; act: -113 ),
  ( sym: 308; act: -113 ),
  ( sym: 318; act: -113 ),
  ( sym: 319; act: -113 ),
{ 216: }
  ( sym: 311; act: 182 ),
  ( sym: 312; act: 183 ),
  ( sym: 313; act: 184 ),
  ( sym: 314; act: 185 ),
  ( sym: 315; act: 186 ),
  ( sym: 259; act: -108 ),
  ( sym: 260; act: -108 ),
  ( sym: 261; act: -108 ),
  ( sym: 262; act: -108 ),
  ( sym: 263; act: -108 ),
  ( sym: 264; act: -108 ),
  ( sym: 265; act: -108 ),
  ( sym: 267; act: -108 ),
  ( sym: 286; act: -108 ),
  ( sym: 296; act: -108 ),
  ( sym: 300; act: -108 ),
  ( sym: 301; act: -108 ),
  ( sym: 302; act: -108 ),
  ( sym: 303; act: -108 ),
  ( sym: 304; act: -108 ),
  ( sym: 305; act: -108 ),
  ( sym: 306; act: -108 ),
  ( sym: 307; act: -108 ),
  ( sym: 308; act: -108 ),
  ( sym: 309; act: -108 ),
  ( sym: 310; act: -108 ),
  ( sym: 318; act: -108 ),
  ( sym: 319; act: -108 ),
{ 217: }
  ( sym: 311; act: 182 ),
  ( sym: 312; act: 183 ),
  ( sym: 313; act: 184 ),
  ( sym: 314; act: 185 ),
  ( sym: 315; act: 186 ),
  ( sym: 259; act: -109 ),
  ( sym: 260; act: -109 ),
  ( sym: 261; act: -109 ),
  ( sym: 262; act: -109 ),
  ( sym: 263; act: -109 ),
  ( sym: 264; act: -109 ),
  ( sym: 265; act: -109 ),
  ( sym: 267; act: -109 ),
  ( sym: 286; act: -109 ),
  ( sym: 296; act: -109 ),
  ( sym: 300; act: -109 ),
  ( sym: 301; act: -109 ),
  ( sym: 302; act: -109 ),
  ( sym: 303; act: -109 ),
  ( sym: 304; act: -109 ),
  ( sym: 305; act: -109 ),
  ( sym: 306; act: -109 ),
  ( sym: 307; act: -109 ),
  ( sym: 308; act: -109 ),
  ( sym: 309; act: -109 ),
  ( sym: 310; act: -109 ),
  ( sym: 318; act: -109 ),
  ( sym: 319; act: -109 ),
{ 218: }
  ( sym: 313; act: 184 ),
  ( sym: 314; act: 185 ),
  ( sym: 315; act: 186 ),
  ( sym: 259; act: -116 ),
  ( sym: 260; act: -116 ),
  ( sym: 261; act: -116 ),
  ( sym: 262; act: -116 ),
  ( sym: 263; act: -116 ),
  ( sym: 264; act: -116 ),
  ( sym: 265; act: -116 ),
  ( sym: 267; act: -116 ),
  ( sym: 286; act: -116 ),
  ( sym: 296; act: -116 ),
  ( sym: 300; act: -116 ),
  ( sym: 301; act: -116 ),
  ( sym: 302; act: -116 ),
  ( sym: 303; act: -116 ),
  ( sym: 304; act: -116 ),
  ( sym: 305; act: -116 ),
  ( sym: 306; act: -116 ),
  ( sym: 307; act: -116 ),
  ( sym: 308; act: -116 ),
  ( sym: 309; act: -116 ),
  ( sym: 310; act: -116 ),
  ( sym: 311; act: -116 ),
  ( sym: 312; act: -116 ),
  ( sym: 318; act: -116 ),
  ( sym: 319; act: -116 ),
{ 219: }
  ( sym: 313; act: 184 ),
  ( sym: 314; act: 185 ),
  ( sym: 315; act: 186 ),
  ( sym: 259; act: -115 ),
  ( sym: 260; act: -115 ),
  ( sym: 261; act: -115 ),
  ( sym: 262; act: -115 ),
  ( sym: 263; act: -115 ),
  ( sym: 264; act: -115 ),
  ( sym: 265; act: -115 ),
  ( sym: 267; act: -115 ),
  ( sym: 286; act: -115 ),
  ( sym: 296; act: -115 ),
  ( sym: 300; act: -115 ),
  ( sym: 301; act: -115 ),
  ( sym: 302; act: -115 ),
  ( sym: 303; act: -115 ),
  ( sym: 304; act: -115 ),
  ( sym: 305; act: -115 ),
  ( sym: 306; act: -115 ),
  ( sym: 307; act: -115 ),
  ( sym: 308; act: -115 ),
  ( sym: 309; act: -115 ),
  ( sym: 310; act: -115 ),
  ( sym: 311; act: -115 ),
  ( sym: 312; act: -115 ),
  ( sym: 318; act: -115 ),
  ( sym: 319; act: -115 ),
{ 220: }
  ( sym: 315; act: 186 ),
  ( sym: 259; act: -110 ),
  ( sym: 260; act: -110 ),
  ( sym: 261; act: -110 ),
  ( sym: 262; act: -110 ),
  ( sym: 263; act: -110 ),
  ( sym: 264; act: -110 ),
  ( sym: 265; act: -110 ),
  ( sym: 267; act: -110 ),
  ( sym: 286; act: -110 ),
  ( sym: 296; act: -110 ),
  ( sym: 300; act: -110 ),
  ( sym: 301; act: -110 ),
  ( sym: 302; act: -110 ),
  ( sym: 303; act: -110 ),
  ( sym: 304; act: -110 ),
  ( sym: 305; act: -110 ),
  ( sym: 306; act: -110 ),
  ( sym: 307; act: -110 ),
  ( sym: 308; act: -110 ),
  ( sym: 309; act: -110 ),
  ( sym: 310; act: -110 ),
  ( sym: 311; act: -110 ),
  ( sym: 312; act: -110 ),
  ( sym: 313; act: -110 ),
  ( sym: 314; act: -110 ),
  ( sym: 318; act: -110 ),
  ( sym: 319; act: -110 ),
{ 221: }
  ( sym: 315; act: 186 ),
  ( sym: 259; act: -111 ),
  ( sym: 260; act: -111 ),
  ( sym: 261; act: -111 ),
  ( sym: 262; act: -111 ),
  ( sym: 263; act: -111 ),
  ( sym: 264; act: -111 ),
  ( sym: 265; act: -111 ),
  ( sym: 267; act: -111 ),
  ( sym: 286; act: -111 ),
  ( sym: 296; act: -111 ),
  ( sym: 300; act: -111 ),
  ( sym: 301; act: -111 ),
  ( sym: 302; act: -111 ),
  ( sym: 303; act: -111 ),
  ( sym: 304; act: -111 ),
  ( sym: 305; act: -111 ),
  ( sym: 306; act: -111 ),
  ( sym: 307; act: -111 ),
  ( sym: 308; act: -111 ),
  ( sym: 309; act: -111 ),
  ( sym: 310; act: -111 ),
  ( sym: 311; act: -111 ),
  ( sym: 312; act: -111 ),
  ( sym: 313; act: -111 ),
  ( sym: 314; act: -111 ),
  ( sym: 318; act: -111 ),
  ( sym: 319; act: -111 ),
{ 222: }
  ( sym: 315; act: 186 ),
  ( sym: 259; act: -114 ),
  ( sym: 260; act: -114 ),
  ( sym: 261; act: -114 ),
  ( sym: 262; act: -114 ),
  ( sym: 263; act: -114 ),
  ( sym: 264; act: -114 ),
  ( sym: 265; act: -114 ),
  ( sym: 267; act: -114 ),
  ( sym: 286; act: -114 ),
  ( sym: 296; act: -114 ),
  ( sym: 300; act: -114 ),
  ( sym: 301; act: -114 ),
  ( sym: 302; act: -114 ),
  ( sym: 303; act: -114 ),
  ( sym: 304; act: -114 ),
  ( sym: 305; act: -114 ),
  ( sym: 306; act: -114 ),
  ( sym: 307; act: -114 ),
  ( sym: 308; act: -114 ),
  ( sym: 309; act: -114 ),
  ( sym: 310; act: -114 ),
  ( sym: 311; act: -114 ),
  ( sym: 312; act: -114 ),
  ( sym: 313; act: -114 ),
  ( sym: 314; act: -114 ),
  ( sym: 318; act: -114 ),
  ( sym: 319; act: -114 ),
{ 223: }
  ( sym: 263; act: 240 ),
{ 224: }
  ( sym: 318; act: 133 ),
  ( sym: 319; act: 134 ),
  ( sym: 259; act: -131 ),
  ( sym: 260; act: -131 ),
  ( sym: 261; act: -131 ),
  ( sym: 262; act: -131 ),
  ( sym: 263; act: -131 ),
  ( sym: 264; act: -131 ),
  ( sym: 265; act: -131 ),
  ( sym: 267; act: -131 ),
  ( sym: 286; act: -131 ),
  ( sym: 296; act: -131 ),
  ( sym: 300; act: -131 ),
  ( sym: 301; act: -131 ),
  ( sym: 302; act: -131 ),
  ( sym: 303; act: -131 ),
  ( sym: 304; act: -131 ),
  ( sym: 305; act: -131 ),
  ( sym: 306; act: -131 ),
  ( sym: 307; act: -131 ),
  ( sym: 308; act: -131 ),
  ( sym: 309; act: -131 ),
  ( sym: 310; act: -131 ),
  ( sym: 311; act: -131 ),
  ( sym: 312; act: -131 ),
  ( sym: 313; act: -131 ),
  ( sym: 314; act: -131 ),
  ( sym: 315; act: -131 ),
{ 225: }
  ( sym: 262; act: 100 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 101 ),
  ( sym: 273; act: 102 ),
  ( sym: 308; act: 104 ),
  ( sym: 310; act: 105 ),
  ( sym: 315; act: 106 ),
{ 226: }
{ 227: }
  ( sym: 318; act: 133 ),
  ( sym: 319; act: 134 ),
  ( sym: 259; act: -120 ),
  ( sym: 260; act: -120 ),
  ( sym: 261; act: -120 ),
  ( sym: 262; act: -120 ),
  ( sym: 263; act: -120 ),
  ( sym: 264; act: -120 ),
  ( sym: 265; act: -120 ),
  ( sym: 267; act: -120 ),
  ( sym: 286; act: -120 ),
  ( sym: 296; act: -120 ),
  ( sym: 300; act: -120 ),
  ( sym: 301; act: -120 ),
  ( sym: 302; act: -120 ),
  ( sym: 303; act: -120 ),
  ( sym: 304; act: -120 ),
  ( sym: 305; act: -120 ),
  ( sym: 306; act: -120 ),
  ( sym: 307; act: -120 ),
  ( sym: 308; act: -120 ),
  ( sym: 309; act: -120 ),
  ( sym: 310; act: -120 ),
  ( sym: 311; act: -120 ),
  ( sym: 312; act: -120 ),
  ( sym: 313; act: -120 ),
  ( sym: 314; act: -120 ),
  ( sym: 315; act: -120 ),
{ 228: }
{ 229: }
  ( sym: 262; act: 196 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 197 ),
  ( sym: 282; act: 79 ),
  ( sym: 283; act: 80 ),
  ( sym: 284; act: 81 ),
  ( sym: 308; act: 82 ),
  ( sym: 313; act: 198 ),
  ( sym: 261; act: -100 ),
  ( sym: 263; act: -100 ),
  ( sym: 264; act: -100 ),
{ 230: }
{ 231: }
  ( sym: 263; act: 149 ),
  ( sym: 268; act: 20 ),
  ( sym: 269; act: 21 ),
  ( sym: 270; act: 22 ),
  ( sym: 271; act: 23 ),
  ( sym: 274; act: 24 ),
  ( sym: 275; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 277; act: 27 ),
  ( sym: 278; act: 28 ),
  ( sym: 279; act: 29 ),
  ( sym: 280; act: 150 ),
  ( sym: 281; act: 31 ),
  ( sym: 298; act: 151 ),
{ 232: }
  ( sym: 262; act: 100 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 101 ),
  ( sym: 273; act: 102 ),
  ( sym: 308; act: 104 ),
  ( sym: 310; act: 105 ),
  ( sym: 315; act: 106 ),
{ 233: }
  ( sym: 262; act: 231 ),
  ( sym: 263; act: 245 ),
  ( sym: 264; act: 232 ),
{ 234: }
  ( sym: 262; act: 231 ),
  ( sym: 264; act: 232 ),
  ( sym: 261; act: -93 ),
  ( sym: 263; act: -93 ),
{ 235: }
  ( sym: 264; act: 232 ),
  ( sym: 261; act: -95 ),
  ( sym: 262; act: -95 ),
  ( sym: 263; act: -95 ),
{ 236: }
{ 237: }
{ 238: }
{ 239: }
  ( sym: 262; act: 100 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 101 ),
  ( sym: 273; act: 102 ),
  ( sym: 308; act: 104 ),
  ( sym: 310; act: 105 ),
  ( sym: 315; act: 106 ),
{ 240: }
  ( sym: 262; act: 100 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 101 ),
  ( sym: 273; act: 102 ),
  ( sym: 308; act: 104 ),
  ( sym: 310; act: 105 ),
  ( sym: 315; act: 106 ),
{ 241: }
  ( sym: 318; act: 133 ),
  ( sym: 319; act: 134 ),
  ( sym: 259; act: -132 ),
  ( sym: 260; act: -132 ),
  ( sym: 261; act: -132 ),
  ( sym: 262; act: -132 ),
  ( sym: 263; act: -132 ),
  ( sym: 264; act: -132 ),
  ( sym: 265; act: -132 ),
  ( sym: 267; act: -132 ),
  ( sym: 286; act: -132 ),
  ( sym: 296; act: -132 ),
  ( sym: 300; act: -132 ),
  ( sym: 301; act: -132 ),
  ( sym: 302; act: -132 ),
  ( sym: 303; act: -132 ),
  ( sym: 304; act: -132 ),
  ( sym: 305; act: -132 ),
  ( sym: 306; act: -132 ),
  ( sym: 307; act: -132 ),
  ( sym: 308; act: -132 ),
  ( sym: 309; act: -132 ),
  ( sym: 310; act: -132 ),
  ( sym: 311; act: -132 ),
  ( sym: 312; act: -132 ),
  ( sym: 313; act: -132 ),
  ( sym: 314; act: -132 ),
  ( sym: 315; act: -132 ),
{ 242: }
  ( sym: 262; act: 231 ),
  ( sym: 264; act: 232 ),
  ( sym: 261; act: -94 ),
  ( sym: 263; act: -94 ),
{ 243: }
  ( sym: 263; act: 248 ),
{ 244: }
  ( sym: 265; act: 249 ),
  ( sym: 300; act: 171 ),
  ( sym: 301; act: 172 ),
  ( sym: 302; act: 173 ),
  ( sym: 303; act: 174 ),
  ( sym: 304; act: 175 ),
  ( sym: 305; act: 176 ),
  ( sym: 306; act: 177 ),
  ( sym: 307; act: 178 ),
  ( sym: 308; act: 179 ),
  ( sym: 309; act: 180 ),
  ( sym: 310; act: 181 ),
  ( sym: 311; act: 182 ),
  ( sym: 312; act: 183 ),
  ( sym: 313; act: 184 ),
  ( sym: 314; act: 185 ),
  ( sym: 315; act: 186 ),
{ 245: }
{ 246: }
  ( sym: 307; act: 178 ),
  ( sym: 308; act: 179 ),
  ( sym: 309; act: 180 ),
  ( sym: 310; act: 181 ),
  ( sym: 311; act: 182 ),
  ( sym: 312; act: 183 ),
  ( sym: 313; act: 184 ),
  ( sym: 314; act: 185 ),
  ( sym: 315; act: 186 ),
  ( sym: 259; act: -119 ),
  ( sym: 260; act: -119 ),
  ( sym: 261; act: -119 ),
  ( sym: 262; act: -119 ),
  ( sym: 263; act: -119 ),
  ( sym: 264; act: -119 ),
  ( sym: 265; act: -119 ),
  ( sym: 267; act: -119 ),
  ( sym: 286; act: -119 ),
  ( sym: 296; act: -119 ),
  ( sym: 300; act: -119 ),
  ( sym: 301; act: -119 ),
  ( sym: 302; act: -119 ),
  ( sym: 303; act: -119 ),
  ( sym: 304; act: -119 ),
  ( sym: 305; act: -119 ),
  ( sym: 306; act: -119 ),
  ( sym: 318; act: -119 ),
  ( sym: 319; act: -119 ),
{ 247: }
  ( sym: 318; act: 133 ),
  ( sym: 319; act: 134 ),
  ( sym: 259; act: -133 ),
  ( sym: 260; act: -133 ),
  ( sym: 261; act: -133 ),
  ( sym: 262; act: -133 ),
  ( sym: 263; act: -133 ),
  ( sym: 264; act: -133 ),
  ( sym: 265; act: -133 ),
  ( sym: 267; act: -133 ),
  ( sym: 286; act: -133 ),
  ( sym: 296; act: -133 ),
  ( sym: 300; act: -133 ),
  ( sym: 301; act: -133 ),
  ( sym: 302; act: -133 ),
  ( sym: 303; act: -133 ),
  ( sym: 304; act: -133 ),
  ( sym: 305; act: -133 ),
  ( sym: 306; act: -133 ),
  ( sym: 307; act: -133 ),
  ( sym: 308; act: -133 ),
  ( sym: 309; act: -133 ),
  ( sym: 310; act: -133 ),
  ( sym: 311; act: -133 ),
  ( sym: 312; act: -133 ),
  ( sym: 313; act: -133 ),
  ( sym: 314; act: -133 ),
  ( sym: 315; act: -133 )
{ 248: }
{ 249: }
);

yyg : array [1..yyngotos] of YYARec = (
{ 0: }
  ( sym: -13; act: 1 ),
  ( sym: -7; act: 2 ),
  ( sym: -6; act: 3 ),
  ( sym: -5; act: 4 ),
  ( sym: -3; act: 5 ),
  ( sym: -2; act: 6 ),
{ 1: }
{ 2: }
  ( sym: -21; act: 15 ),
  ( sym: -19; act: 16 ),
  ( sym: -13; act: 17 ),
  ( sym: -11; act: 18 ),
  ( sym: -10; act: 19 ),
{ 3: }
{ 4: }
{ 5: }
  ( sym: -13; act: 1 ),
  ( sym: -7; act: 2 ),
  ( sym: -6; act: 32 ),
  ( sym: -5; act: 33 ),
{ 6: }
{ 7: }
  ( sym: -4; act: 34 ),
{ 8: }
  ( sym: -21; act: 15 ),
  ( sym: -19; act: 16 ),
  ( sym: -13; act: 17 ),
  ( sym: -11; act: 35 ),
  ( sym: -10; act: 36 ),
{ 9: }
  ( sym: -10; act: 37 ),
{ 10: }
  ( sym: -10; act: 38 ),
{ 11: }
  ( sym: -10; act: 39 ),
{ 12: }
  ( sym: -10; act: 40 ),
{ 13: }
{ 14: }
{ 15: }
{ 16: }
{ 17: }
{ 18: }
  ( sym: -8; act: 41 ),
{ 19: }
{ 20: }
  ( sym: -16; act: 49 ),
  ( sym: -10; act: 38 ),
{ 21: }
  ( sym: -16; act: 52 ),
  ( sym: -10; act: 39 ),
{ 22: }
  ( sym: -18; act: 53 ),
  ( sym: -10; act: 40 ),
{ 23: }
{ 24: }
{ 25: }
{ 26: }
{ 27: }
{ 28: }
{ 29: }
{ 30: }
{ 31: }
  ( sym: -21; act: 15 ),
  ( sym: -19; act: 16 ),
  ( sym: -13; act: 17 ),
  ( sym: -11; act: 61 ),
  ( sym: -10; act: 19 ),
{ 32: }
{ 33: }
{ 34: }
{ 35: }
  ( sym: -8; act: 64 ),
{ 36: }
{ 37: }
{ 38: }
  ( sym: -16; act: 69 ),
{ 39: }
  ( sym: -16; act: 70 ),
{ 40: }
  ( sym: -18; act: 71 ),
{ 41: }
  ( sym: -26; act: 72 ),
  ( sym: -22; act: 73 ),
  ( sym: -12; act: 74 ),
  ( sym: -10; act: 75 ),
{ 42: }
{ 43: }
{ 44: }
{ 45: }
{ 46: }
{ 47: }
{ 48: }
{ 49: }
{ 50: }
  ( sym: -4; act: 85 ),
{ 51: }
  ( sym: -21; act: 15 ),
  ( sym: -20; act: 86 ),
  ( sym: -19; act: 16 ),
  ( sym: -17; act: 87 ),
  ( sym: -13; act: 17 ),
  ( sym: -11; act: 88 ),
  ( sym: -10; act: 19 ),
{ 52: }
{ 53: }
{ 54: }
  ( sym: -4; act: 90 ),
{ 55: }
  ( sym: -35; act: 91 ),
  ( sym: -14; act: 92 ),
  ( sym: -10; act: 93 ),
{ 56: }
{ 57: }
{ 58: }
{ 59: }
{ 60: }
{ 61: }
{ 62: }
{ 63: }
{ 64: }
  ( sym: -26; act: 72 ),
  ( sym: -22; act: 73 ),
  ( sym: -12; act: 95 ),
  ( sym: -10; act: 75 ),
{ 65: }
{ 66: }
  ( sym: -35; act: 91 ),
  ( sym: -14; act: 96 ),
  ( sym: -10; act: 93 ),
{ 67: }
{ 68: }
  ( sym: -32; act: 97 ),
  ( sym: -15; act: 98 ),
  ( sym: -10; act: 99 ),
{ 69: }
{ 70: }
{ 71: }
{ 72: }
{ 73: }
  ( sym: -29; act: 110 ),
{ 74: }
  ( sym: -9; act: 113 ),
{ 75: }
  ( sym: -28; act: 116 ),
{ 76: }
  ( sym: -4; act: 118 ),
{ 77: }
  ( sym: -26; act: 72 ),
  ( sym: -22; act: 119 ),
  ( sym: -10; act: 75 ),
{ 78: }
  ( sym: -26; act: 72 ),
  ( sym: -22; act: 120 ),
  ( sym: -10; act: 75 ),
{ 79: }
{ 80: }
{ 81: }
{ 82: }
  ( sym: -26; act: 72 ),
  ( sym: -22; act: 121 ),
  ( sym: -10; act: 75 ),
{ 83: }
  ( sym: -26; act: 72 ),
  ( sym: -22; act: 122 ),
  ( sym: -10; act: 75 ),
{ 84: }
{ 85: }
{ 86: }
  ( sym: -21; act: 15 ),
  ( sym: -20; act: 86 ),
  ( sym: -19; act: 16 ),
  ( sym: -17; act: 124 ),
  ( sym: -13; act: 17 ),
  ( sym: -11; act: 88 ),
  ( sym: -10; act: 19 ),
{ 87: }
{ 88: }
  ( sym: -26; act: 72 ),
  ( sym: -22; act: 73 ),
  ( sym: -12; act: 126 ),
  ( sym: -10; act: 75 ),
{ 89: }
{ 90: }
{ 91: }
{ 92: }
{ 93: }
{ 94: }
{ 95: }
{ 96: }
{ 97: }
{ 98: }
{ 99: }
{ 100: }
  ( sym: -32; act: 137 ),
  ( sym: -30; act: 138 ),
  ( sym: -27; act: 139 ),
  ( sym: -21; act: 15 ),
  ( sym: -19; act: 16 ),
  ( sym: -13; act: 17 ),
  ( sym: -11; act: 140 ),
  ( sym: -10; act: 141 ),
{ 101: }
{ 102: }
{ 103: }
{ 104: }
  ( sym: -32; act: 142 ),
  ( sym: -10; act: 99 ),
{ 105: }
  ( sym: -32; act: 143 ),
  ( sym: -10; act: 99 ),
{ 106: }
  ( sym: -32; act: 144 ),
  ( sym: -10; act: 99 ),
{ 107: }
{ 108: }
{ 109: }
  ( sym: -26; act: 72 ),
  ( sym: -22; act: 145 ),
  ( sym: -10; act: 75 ),
{ 110: }
{ 111: }
  ( sym: -25; act: 146 ),
  ( sym: -23; act: 147 ),
  ( sym: -21; act: 15 ),
  ( sym: -19; act: 16 ),
  ( sym: -13; act: 17 ),
  ( sym: -11; act: 148 ),
  ( sym: -10; act: 19 ),
{ 112: }
  ( sym: -32; act: 137 ),
  ( sym: -30; act: 152 ),
  ( sym: -27; act: 153 ),
  ( sym: -10; act: 99 ),
{ 113: }
{ 114: }
  ( sym: -26; act: 72 ),
  ( sym: -22; act: 155 ),
  ( sym: -10; act: 75 ),
{ 115: }
{ 116: }
  ( sym: -32; act: 137 ),
  ( sym: -30; act: 152 ),
  ( sym: -27; act: 157 ),
  ( sym: -10; act: 99 ),
{ 117: }
  ( sym: -32; act: 137 ),
  ( sym: -30; act: 152 ),
  ( sym: -27; act: 158 ),
  ( sym: -10; act: 99 ),
{ 118: }
{ 119: }
  ( sym: -29; act: 110 ),
{ 120: }
  ( sym: -29; act: 110 ),
{ 121: }
  ( sym: -29; act: 110 ),
{ 122: }
  ( sym: -29; act: 110 ),
{ 123: }
{ 124: }
{ 125: }
{ 126: }
{ 127: }
{ 128: }
  ( sym: -35; act: 91 ),
  ( sym: -14; act: 162 ),
  ( sym: -10; act: 93 ),
{ 129: }
{ 130: }
  ( sym: -32; act: 137 ),
  ( sym: -30; act: 152 ),
  ( sym: -27; act: 163 ),
  ( sym: -10; act: 99 ),
{ 131: }
{ 132: }
{ 133: }
  ( sym: -32; act: 137 ),
  ( sym: -30; act: 152 ),
  ( sym: -27; act: 165 ),
  ( sym: -10; act: 99 ),
{ 134: }
  ( sym: -32; act: 137 ),
  ( sym: -30; act: 152 ),
  ( sym: -27; act: 166 ),
  ( sym: -10; act: 99 ),
{ 135: }
{ 136: }
  ( sym: -36; act: 167 ),
  ( sym: -34; act: 168 ),
  ( sym: -32; act: 137 ),
  ( sym: -30; act: 152 ),
  ( sym: -27; act: 169 ),
  ( sym: -10; act: 99 ),
{ 137: }
{ 138: }
{ 139: }
{ 140: }
  ( sym: -26; act: 187 ),
{ 141: }
{ 142: }
{ 143: }
{ 144: }
{ 145: }
  ( sym: -29; act: 110 ),
{ 146: }
{ 147: }
{ 148: }
  ( sym: -26; act: 193 ),
  ( sym: -24; act: 194 ),
  ( sym: -22; act: 195 ),
  ( sym: -10; act: 75 ),
{ 149: }
{ 150: }
{ 151: }
{ 152: }
{ 153: }
{ 154: }
{ 155: }
  ( sym: -29; act: 110 ),
{ 156: }
  ( sym: -10; act: 201 ),
{ 157: }
{ 158: }
{ 159: }
  ( sym: -26; act: 72 ),
  ( sym: -22; act: 73 ),
  ( sym: -12; act: 202 ),
  ( sym: -10; act: 75 ),
{ 160: }
{ 161: }
{ 162: }
{ 163: }
{ 164: }
  ( sym: -32; act: 97 ),
  ( sym: -15; act: 203 ),
  ( sym: -10; act: 99 ),
{ 165: }
{ 166: }
{ 167: }
{ 168: }
{ 169: }
{ 170: }
{ 171: }
  ( sym: -32; act: 137 ),
  ( sym: -30; act: 152 ),
  ( sym: -27; act: 206 ),
  ( sym: -10; act: 99 ),
{ 172: }
  ( sym: -32; act: 137 ),
  ( sym: -30; act: 152 ),
  ( sym: -27; act: 207 ),
  ( sym: -10; act: 99 ),
{ 173: }
  ( sym: -32; act: 137 ),
  ( sym: -30; act: 152 ),
  ( sym: -27; act: 208 ),
  ( sym: -10; act: 99 ),
{ 174: }
  ( sym: -32; act: 137 ),
  ( sym: -30; act: 152 ),
  ( sym: -27; act: 209 ),
  ( sym: -10; act: 99 ),
{ 175: }
  ( sym: -32; act: 137 ),
  ( sym: -30; act: 152 ),
  ( sym: -27; act: 210 ),
  ( sym: -10; act: 99 ),
{ 176: }
  ( sym: -32; act: 137 ),
  ( sym: -30; act: 152 ),
  ( sym: -27; act: 211 ),
  ( sym: -10; act: 99 ),
{ 177: }
  ( sym: -32; act: 137 ),
  ( sym: -31; act: 212 ),
  ( sym: -30; act: 152 ),
  ( sym: -27; act: 213 ),
  ( sym: -10; act: 99 ),
{ 178: }
  ( sym: -32; act: 137 ),
  ( sym: -30; act: 152 ),
  ( sym: -27; act: 214 ),
  ( sym: -10; act: 99 ),
{ 179: }
  ( sym: -32; act: 137 ),
  ( sym: -30; act: 152 ),
  ( sym: -27; act: 215 ),
  ( sym: -10; act: 99 ),
{ 180: }
  ( sym: -32; act: 137 ),
  ( sym: -30; act: 152 ),
  ( sym: -27; act: 216 ),
  ( sym: -10; act: 99 ),
{ 181: }
  ( sym: -32; act: 137 ),
  ( sym: -30; act: 152 ),
  ( sym: -27; act: 217 ),
  ( sym: -10; act: 99 ),
{ 182: }
  ( sym: -32; act: 137 ),
  ( sym: -30; act: 152 ),
  ( sym: -27; act: 218 ),
  ( sym: -10; act: 99 ),
{ 183: }
  ( sym: -32; act: 137 ),
  ( sym: -30; act: 152 ),
  ( sym: -27; act: 219 ),
  ( sym: -10; act: 99 ),
{ 184: }
  ( sym: -32; act: 137 ),
  ( sym: -30; act: 152 ),
  ( sym: -27; act: 220 ),
  ( sym: -10; act: 99 ),
{ 185: }
  ( sym: -32; act: 137 ),
  ( sym: -30; act: 152 ),
  ( sym: -27; act: 221 ),
  ( sym: -10; act: 99 ),
{ 186: }
  ( sym: -32; act: 137 ),
  ( sym: -30; act: 152 ),
  ( sym: -27; act: 222 ),
  ( sym: -10; act: 99 ),
{ 187: }
{ 188: }
  ( sym: -32; act: 224 ),
  ( sym: -10; act: 99 ),
{ 189: }
{ 190: }
  ( sym: -33; act: 226 ),
  ( sym: -32; act: 227 ),
  ( sym: -10; act: 99 ),
{ 191: }
{ 192: }
  ( sym: -25; act: 228 ),
  ( sym: -23; act: 147 ),
  ( sym: -21; act: 15 ),
  ( sym: -19; act: 16 ),
  ( sym: -13; act: 17 ),
  ( sym: -11; act: 148 ),
  ( sym: -10; act: 19 ),
{ 193: }
{ 194: }
  ( sym: -29; act: 230 ),
{ 195: }
  ( sym: -29; act: 110 ),
{ 196: }
  ( sym: -26; act: 193 ),
  ( sym: -24; act: 233 ),
  ( sym: -22; act: 119 ),
  ( sym: -10; act: 75 ),
{ 197: }
  ( sym: -26; act: 193 ),
  ( sym: -24; act: 234 ),
  ( sym: -22; act: 120 ),
  ( sym: -10; act: 75 ),
{ 198: }
  ( sym: -26; act: 193 ),
  ( sym: -24; act: 235 ),
  ( sym: -22; act: 122 ),
  ( sym: -10; act: 75 ),
{ 199: }
{ 200: }
{ 201: }
{ 202: }
{ 203: }
{ 204: }
  ( sym: -36; act: 167 ),
  ( sym: -34; act: 238 ),
  ( sym: -32; act: 137 ),
  ( sym: -30; act: 152 ),
  ( sym: -27; act: 169 ),
  ( sym: -10; act: 99 ),
{ 205: }
{ 206: }
{ 207: }
{ 208: }
{ 209: }
{ 210: }
{ 211: }
{ 212: }
{ 213: }
{ 214: }
{ 215: }
{ 216: }
{ 217: }
{ 218: }
{ 219: }
{ 220: }
{ 221: }
{ 222: }
{ 223: }
{ 224: }
{ 225: }
  ( sym: -32; act: 241 ),
  ( sym: -10; act: 99 ),
{ 226: }
{ 227: }
{ 228: }
{ 229: }
  ( sym: -26; act: 193 ),
  ( sym: -24; act: 242 ),
  ( sym: -22; act: 145 ),
  ( sym: -10; act: 75 ),
{ 230: }
{ 231: }
  ( sym: -25; act: 243 ),
  ( sym: -23; act: 147 ),
  ( sym: -21; act: 15 ),
  ( sym: -19; act: 16 ),
  ( sym: -13; act: 17 ),
  ( sym: -11; act: 148 ),
  ( sym: -10; act: 19 ),
{ 232: }
  ( sym: -32; act: 137 ),
  ( sym: -30; act: 152 ),
  ( sym: -27; act: 244 ),
  ( sym: -10; act: 99 ),
{ 233: }
  ( sym: -29; act: 230 ),
{ 234: }
  ( sym: -29; act: 230 ),
{ 235: }
  ( sym: -29; act: 230 ),
{ 236: }
{ 237: }
{ 238: }
{ 239: }
  ( sym: -32; act: 137 ),
  ( sym: -30; act: 152 ),
  ( sym: -27; act: 246 ),
  ( sym: -10; act: 99 ),
{ 240: }
  ( sym: -32; act: 247 ),
  ( sym: -10; act: 99 ),
{ 241: }
{ 242: }
  ( sym: -29; act: 230 )
{ 243: }
{ 244: }
{ 245: }
{ 246: }
{ 247: }
{ 248: }
{ 249: }
);

yyd : array [0..yynstates-1] of Integer = (
{ 0: } 0,
{ 1: } 0,
{ 2: } 0,
{ 3: } -6,
{ 4: } -5,
{ 5: } 0,
{ 6: } 0,
{ 7: } -2,
{ 8: } 0,
{ 9: } 0,
{ 10: } 0,
{ 11: } 0,
{ 12: } 0,
{ 13: } -7,
{ 14: } -20,
{ 15: } -66,
{ 16: } -48,
{ 17: } -47,
{ 18: } 0,
{ 19: } -67,
{ 20: } 0,
{ 21: } 0,
{ 22: } 0,
{ 23: } -52,
{ 24: } -64,
{ 25: } 0,
{ 26: } 0,
{ 27: } -53,
{ 28: } -56,
{ 29: } -65,
{ 30: } -63,
{ 31: } 0,
{ 32: } -4,
{ 33: } -3,
{ 34: } 0,
{ 35: } 0,
{ 36: } 0,
{ 37: } 0,
{ 38: } 0,
{ 39: } 0,
{ 40: } 0,
{ 41: } 0,
{ 42: } -9,
{ 43: } -10,
{ 44: } -11,
{ 45: } -12,
{ 46: } -13,
{ 47: } -14,
{ 48: } -15,
{ 49: } 0,
{ 50: } -2,
{ 51: } 0,
{ 52: } 0,
{ 53: } -46,
{ 54: } -2,
{ 55: } 0,
{ 56: } -61,
{ 57: } 0,
{ 58: } -54,
{ 59: } -62,
{ 60: } -57,
{ 61: } -41,
{ 62: } -23,
{ 63: } -28,
{ 64: } 0,
{ 65: } -22,
{ 66: } 0,
{ 67: } -26,
{ 68: } 0,
{ 69: } 0,
{ 70: } 0,
{ 71: } -39,
{ 72: } 0,
{ 73: } 0,
{ 74: } 0,
{ 75: } 0,
{ 76: } -2,
{ 77: } 0,
{ 78: } 0,
{ 79: } -77,
{ 80: } -79,
{ 81: } -78,
{ 82: } 0,
{ 83: } 0,
{ 84: } -44,
{ 85: } 0,
{ 86: } 0,
{ 87: } 0,
{ 88: } 0,
{ 89: } -42,
{ 90: } 0,
{ 91: } 0,
{ 92: } 0,
{ 93: } 0,
{ 94: } -58,
{ 95: } 0,
{ 96: } 0,
{ 97: } 0,
{ 98: } 0,
{ 99: } 0,
{ 100: } 0,
{ 101: } -124,
{ 102: } -123,
{ 103: } -25,
{ 104: } 0,
{ 105: } 0,
{ 106: } 0,
{ 107: } -33,
{ 108: } -35,
{ 109: } 0,
{ 110: } -88,
{ 111: } 0,
{ 112: } 0,
{ 113: } 0,
{ 114: } 0,
{ 115: } 0,
{ 116: } 0,
{ 117: } 0,
{ 118: } 0,
{ 119: } 0,
{ 120: } 0,
{ 121: } 0,
{ 122: } 0,
{ 123: } -30,
{ 124: } -49,
{ 125: } -29,
{ 126: } 0,
{ 127: } -32,
{ 128: } 0,
{ 129: } -31,
{ 130: } 0,
{ 131: } -21,
{ 132: } 0,
{ 133: } 0,
{ 134: } 0,
{ 135: } -27,
{ 136: } 0,
{ 137: } 0,
{ 138: } 0,
{ 139: } 0,
{ 140: } 0,
{ 141: } 0,
{ 142: } 0,
{ 143: } 0,
{ 144: } 0,
{ 145: } 0,
{ 146: } 0,
{ 147: } 0,
{ 148: } 0,
{ 149: } -91,
{ 150: } 0,
{ 151: } -76,
{ 152: } -101,
{ 153: } 0,
{ 154: } -19,
{ 155: } 0,
{ 156: } 0,
{ 157: } 0,
{ 158: } 0,
{ 159: } 0,
{ 160: } -90,
{ 161: } -51,
{ 162: } -136,
{ 163: } 0,
{ 164: } 0,
{ 165: } -125,
{ 166: } -126,
{ 167: } 0,
{ 168: } 0,
{ 169: } 0,
{ 170: } -135,
{ 171: } 0,
{ 172: } 0,
{ 173: } 0,
{ 174: } 0,
{ 175: } 0,
{ 176: } 0,
{ 177: } 0,
{ 178: } 0,
{ 179: } 0,
{ 180: } 0,
{ 181: } 0,
{ 182: } 0,
{ 183: } 0,
{ 184: } 0,
{ 185: } 0,
{ 186: } 0,
{ 187: } 0,
{ 188: } 0,
{ 189: } 0,
{ 190: } 0,
{ 191: } -87,
{ 192: } 0,
{ 193: } 0,
{ 194: } 0,
{ 195: } 0,
{ 196: } 0,
{ 197: } 0,
{ 198: } 0,
{ 199: } -92,
{ 200: } -89,
{ 201: } 0,
{ 202: } -69,
{ 203: } 0,
{ 204: } 0,
{ 205: } -134,
{ 206: } 0,
{ 207: } 0,
{ 208: } 0,
{ 209: } 0,
{ 210: } 0,
{ 211: } 0,
{ 212: } -117,
{ 213: } 0,
{ 214: } 0,
{ 215: } 0,
{ 216: } 0,
{ 217: } 0,
{ 218: } 0,
{ 219: } 0,
{ 220: } 0,
{ 221: } 0,
{ 222: } 0,
{ 223: } 0,
{ 224: } 0,
{ 225: } 0,
{ 226: } -130,
{ 227: } 0,
{ 228: } -75,
{ 229: } 0,
{ 230: } -97,
{ 231: } 0,
{ 232: } 0,
{ 233: } 0,
{ 234: } 0,
{ 235: } 0,
{ 236: } -17,
{ 237: } -24,
{ 238: } -142,
{ 239: } 0,
{ 240: } 0,
{ 241: } 0,
{ 242: } 0,
{ 243: } 0,
{ 244: } 0,
{ 245: } -99,
{ 246: } 0,
{ 247: } 0,
{ 248: } -96,
{ 249: } -98
);

yyal : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 17,
{ 2: } 18,
{ 3: } 30,
{ 4: } 30,
{ 5: } 30,
{ 6: } 47,
{ 7: } 48,
{ 8: } 48,
{ 9: } 60,
{ 10: } 61,
{ 11: } 62,
{ 12: } 63,
{ 13: } 64,
{ 14: } 64,
{ 15: } 64,
{ 16: } 64,
{ 17: } 64,
{ 18: } 64,
{ 19: } 80,
{ 20: } 80,
{ 21: } 83,
{ 22: } 86,
{ 23: } 89,
{ 24: } 89,
{ 25: } 89,
{ 26: } 112,
{ 27: } 132,
{ 28: } 132,
{ 29: } 132,
{ 30: } 132,
{ 31: } 132,
{ 32: } 144,
{ 33: } 144,
{ 34: } 144,
{ 35: } 146,
{ 36: } 162,
{ 37: } 179,
{ 38: } 182,
{ 39: } 203,
{ 40: } 224,
{ 41: } 245,
{ 42: } 254,
{ 43: } 254,
{ 44: } 254,
{ 45: } 254,
{ 46: } 254,
{ 47: } 254,
{ 48: } 254,
{ 49: } 254,
{ 50: } 274,
{ 51: } 274,
{ 52: } 286,
{ 53: } 306,
{ 54: } 306,
{ 55: } 306,
{ 56: } 308,
{ 57: } 308,
{ 58: } 328,
{ 59: } 328,
{ 60: } 328,
{ 61: } 328,
{ 62: } 328,
{ 63: } 328,
{ 64: } 328,
{ 65: } 337,
{ 66: } 337,
{ 67: } 339,
{ 68: } 339,
{ 69: } 347,
{ 70: } 368,
{ 71: } 389,
{ 72: } 389,
{ 73: } 390,
{ 74: } 395,
{ 75: } 398,
{ 76: } 405,
{ 77: } 405,
{ 78: } 413,
{ 79: } 421,
{ 80: } 421,
{ 81: } 421,
{ 82: } 421,
{ 83: } 429,
{ 84: } 437,
{ 85: } 437,
{ 86: } 438,
{ 87: } 451,
{ 88: } 452,
{ 89: } 461,
{ 90: } 461,
{ 91: } 462,
{ 92: } 465,
{ 93: } 466,
{ 94: } 470,
{ 95: } 470,
{ 96: } 472,
{ 97: } 473,
{ 98: } 476,
{ 99: } 477,
{ 100: } 505,
{ 101: } 523,
{ 102: } 523,
{ 103: } 523,
{ 104: } 523,
{ 105: } 530,
{ 106: } 537,
{ 107: } 544,
{ 108: } 544,
{ 109: } 544,
{ 110: } 552,
{ 111: } 552,
{ 112: } 566,
{ 113: } 573,
{ 114: } 574,
{ 115: } 582,
{ 116: } 583,
{ 117: } 590,
{ 118: } 597,
{ 119: } 600,
{ 120: } 603,
{ 121: } 609,
{ 122: } 615,
{ 123: } 621,
{ 124: } 621,
{ 125: } 621,
{ 126: } 621,
{ 127: } 623,
{ 128: } 623,
{ 129: } 626,
{ 130: } 626,
{ 131: } 633,
{ 132: } 633,
{ 133: } 634,
{ 134: } 641,
{ 135: } 648,
{ 136: } 648,
{ 137: } 656,
{ 138: } 684,
{ 139: } 701,
{ 140: } 717,
{ 141: } 722,
{ 142: } 745,
{ 143: } 773,
{ 144: } 801,
{ 145: } 829,
{ 146: } 835,
{ 147: } 836,
{ 148: } 838,
{ 149: } 849,
{ 150: } 849,
{ 151: } 860,
{ 152: } 860,
{ 153: } 860,
{ 154: } 877,
{ 155: } 877,
{ 156: } 882,
{ 157: } 883,
{ 158: } 905,
{ 159: } 927,
{ 160: } 936,
{ 161: } 936,
{ 162: } 936,
{ 163: } 936,
{ 164: } 955,
{ 165: } 962,
{ 166: } 962,
{ 167: } 962,
{ 168: } 964,
{ 169: } 965,
{ 170: } 983,
{ 171: } 983,
{ 172: } 990,
{ 173: } 997,
{ 174: } 1004,
{ 175: } 1011,
{ 176: } 1018,
{ 177: } 1025,
{ 178: } 1032,
{ 179: } 1039,
{ 180: } 1046,
{ 181: } 1053,
{ 182: } 1060,
{ 183: } 1067,
{ 184: } 1074,
{ 185: } 1081,
{ 186: } 1088,
{ 187: } 1095,
{ 188: } 1096,
{ 189: } 1103,
{ 190: } 1104,
{ 191: } 1135,
{ 192: } 1135,
{ 193: } 1148,
{ 194: } 1149,
{ 195: } 1153,
{ 196: } 1157,
{ 197: } 1167,
{ 198: } 1178,
{ 199: } 1189,
{ 200: } 1189,
{ 201: } 1189,
{ 202: } 1190,
{ 203: } 1190,
{ 204: } 1191,
{ 205: } 1199,
{ 206: } 1199,
{ 207: } 1227,
{ 208: } 1255,
{ 209: } 1283,
{ 210: } 1311,
{ 211: } 1339,
{ 212: } 1367,
{ 213: } 1367,
{ 214: } 1384,
{ 215: } 1412,
{ 216: } 1440,
{ 217: } 1468,
{ 218: } 1496,
{ 219: } 1524,
{ 220: } 1552,
{ 221: } 1580,
{ 222: } 1608,
{ 223: } 1636,
{ 224: } 1637,
{ 225: } 1665,
{ 226: } 1672,
{ 227: } 1672,
{ 228: } 1700,
{ 229: } 1700,
{ 230: } 1711,
{ 231: } 1711,
{ 232: } 1725,
{ 233: } 1732,
{ 234: } 1735,
{ 235: } 1739,
{ 236: } 1743,
{ 237: } 1743,
{ 238: } 1743,
{ 239: } 1743,
{ 240: } 1750,
{ 241: } 1757,
{ 242: } 1785,
{ 243: } 1789,
{ 244: } 1790,
{ 245: } 1807,
{ 246: } 1807,
{ 247: } 1835,
{ 248: } 1863,
{ 249: } 1863
);

yyah : array [0..yynstates-1] of Integer = (
{ 0: } 16,
{ 1: } 17,
{ 2: } 29,
{ 3: } 29,
{ 4: } 29,
{ 5: } 46,
{ 6: } 47,
{ 7: } 47,
{ 8: } 59,
{ 9: } 60,
{ 10: } 61,
{ 11: } 62,
{ 12: } 63,
{ 13: } 63,
{ 14: } 63,
{ 15: } 63,
{ 16: } 63,
{ 17: } 63,
{ 18: } 79,
{ 19: } 79,
{ 20: } 82,
{ 21: } 85,
{ 22: } 88,
{ 23: } 88,
{ 24: } 88,
{ 25: } 111,
{ 26: } 131,
{ 27: } 131,
{ 28: } 131,
{ 29: } 131,
{ 30: } 131,
{ 31: } 143,
{ 32: } 143,
{ 33: } 143,
{ 34: } 145,
{ 35: } 161,
{ 36: } 178,
{ 37: } 181,
{ 38: } 202,
{ 39: } 223,
{ 40: } 244,
{ 41: } 253,
{ 42: } 253,
{ 43: } 253,
{ 44: } 253,
{ 45: } 253,
{ 46: } 253,
{ 47: } 253,
{ 48: } 253,
{ 49: } 273,
{ 50: } 273,
{ 51: } 285,
{ 52: } 305,
{ 53: } 305,
{ 54: } 305,
{ 55: } 307,
{ 56: } 307,
{ 57: } 327,
{ 58: } 327,
{ 59: } 327,
{ 60: } 327,
{ 61: } 327,
{ 62: } 327,
{ 63: } 327,
{ 64: } 336,
{ 65: } 336,
{ 66: } 338,
{ 67: } 338,
{ 68: } 346,
{ 69: } 367,
{ 70: } 388,
{ 71: } 388,
{ 72: } 389,
{ 73: } 394,
{ 74: } 397,
{ 75: } 404,
{ 76: } 404,
{ 77: } 412,
{ 78: } 420,
{ 79: } 420,
{ 80: } 420,
{ 81: } 420,
{ 82: } 428,
{ 83: } 436,
{ 84: } 436,
{ 85: } 437,
{ 86: } 450,
{ 87: } 451,
{ 88: } 460,
{ 89: } 460,
{ 90: } 461,
{ 91: } 464,
{ 92: } 465,
{ 93: } 469,
{ 94: } 469,
{ 95: } 471,
{ 96: } 472,
{ 97: } 475,
{ 98: } 476,
{ 99: } 504,
{ 100: } 522,
{ 101: } 522,
{ 102: } 522,
{ 103: } 522,
{ 104: } 529,
{ 105: } 536,
{ 106: } 543,
{ 107: } 543,
{ 108: } 543,
{ 109: } 551,
{ 110: } 551,
{ 111: } 565,
{ 112: } 572,
{ 113: } 573,
{ 114: } 581,
{ 115: } 582,
{ 116: } 589,
{ 117: } 596,
{ 118: } 599,
{ 119: } 602,
{ 120: } 608,
{ 121: } 614,
{ 122: } 620,
{ 123: } 620,
{ 124: } 620,
{ 125: } 620,
{ 126: } 622,
{ 127: } 622,
{ 128: } 625,
{ 129: } 625,
{ 130: } 632,
{ 131: } 632,
{ 132: } 633,
{ 133: } 640,
{ 134: } 647,
{ 135: } 647,
{ 136: } 655,
{ 137: } 683,
{ 138: } 700,
{ 139: } 716,
{ 140: } 721,
{ 141: } 744,
{ 142: } 772,
{ 143: } 800,
{ 144: } 828,
{ 145: } 834,
{ 146: } 835,
{ 147: } 837,
{ 148: } 848,
{ 149: } 848,
{ 150: } 859,
{ 151: } 859,
{ 152: } 859,
{ 153: } 876,
{ 154: } 876,
{ 155: } 881,
{ 156: } 882,
{ 157: } 904,
{ 158: } 926,
{ 159: } 935,
{ 160: } 935,
{ 161: } 935,
{ 162: } 935,
{ 163: } 954,
{ 164: } 961,
{ 165: } 961,
{ 166: } 961,
{ 167: } 963,
{ 168: } 964,
{ 169: } 982,
{ 170: } 982,
{ 171: } 989,
{ 172: } 996,
{ 173: } 1003,
{ 174: } 1010,
{ 175: } 1017,
{ 176: } 1024,
{ 177: } 1031,
{ 178: } 1038,
{ 179: } 1045,
{ 180: } 1052,
{ 181: } 1059,
{ 182: } 1066,
{ 183: } 1073,
{ 184: } 1080,
{ 185: } 1087,
{ 186: } 1094,
{ 187: } 1095,
{ 188: } 1102,
{ 189: } 1103,
{ 190: } 1134,
{ 191: } 1134,
{ 192: } 1147,
{ 193: } 1148,
{ 194: } 1152,
{ 195: } 1156,
{ 196: } 1166,
{ 197: } 1177,
{ 198: } 1188,
{ 199: } 1188,
{ 200: } 1188,
{ 201: } 1189,
{ 202: } 1189,
{ 203: } 1190,
{ 204: } 1198,
{ 205: } 1198,
{ 206: } 1226,
{ 207: } 1254,
{ 208: } 1282,
{ 209: } 1310,
{ 210: } 1338,
{ 211: } 1366,
{ 212: } 1366,
{ 213: } 1383,
{ 214: } 1411,
{ 215: } 1439,
{ 216: } 1467,
{ 217: } 1495,
{ 218: } 1523,
{ 219: } 1551,
{ 220: } 1579,
{ 221: } 1607,
{ 222: } 1635,
{ 223: } 1636,
{ 224: } 1664,
{ 225: } 1671,
{ 226: } 1671,
{ 227: } 1699,
{ 228: } 1699,
{ 229: } 1710,
{ 230: } 1710,
{ 231: } 1724,
{ 232: } 1731,
{ 233: } 1734,
{ 234: } 1738,
{ 235: } 1742,
{ 236: } 1742,
{ 237: } 1742,
{ 238: } 1742,
{ 239: } 1749,
{ 240: } 1756,
{ 241: } 1784,
{ 242: } 1788,
{ 243: } 1789,
{ 244: } 1806,
{ 245: } 1806,
{ 246: } 1834,
{ 247: } 1862,
{ 248: } 1862,
{ 249: } 1862
);

yygl : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 7,
{ 2: } 7,
{ 3: } 12,
{ 4: } 12,
{ 5: } 12,
{ 6: } 16,
{ 7: } 16,
{ 8: } 17,
{ 9: } 22,
{ 10: } 23,
{ 11: } 24,
{ 12: } 25,
{ 13: } 26,
{ 14: } 26,
{ 15: } 26,
{ 16: } 26,
{ 17: } 26,
{ 18: } 26,
{ 19: } 27,
{ 20: } 27,
{ 21: } 29,
{ 22: } 31,
{ 23: } 33,
{ 24: } 33,
{ 25: } 33,
{ 26: } 33,
{ 27: } 33,
{ 28: } 33,
{ 29: } 33,
{ 30: } 33,
{ 31: } 33,
{ 32: } 38,
{ 33: } 38,
{ 34: } 38,
{ 35: } 38,
{ 36: } 39,
{ 37: } 39,
{ 38: } 39,
{ 39: } 40,
{ 40: } 41,
{ 41: } 42,
{ 42: } 46,
{ 43: } 46,
{ 44: } 46,
{ 45: } 46,
{ 46: } 46,
{ 47: } 46,
{ 48: } 46,
{ 49: } 46,
{ 50: } 46,
{ 51: } 47,
{ 52: } 54,
{ 53: } 54,
{ 54: } 54,
{ 55: } 55,
{ 56: } 58,
{ 57: } 58,
{ 58: } 58,
{ 59: } 58,
{ 60: } 58,
{ 61: } 58,
{ 62: } 58,
{ 63: } 58,
{ 64: } 58,
{ 65: } 62,
{ 66: } 62,
{ 67: } 65,
{ 68: } 65,
{ 69: } 68,
{ 70: } 68,
{ 71: } 68,
{ 72: } 68,
{ 73: } 68,
{ 74: } 69,
{ 75: } 70,
{ 76: } 71,
{ 77: } 72,
{ 78: } 75,
{ 79: } 78,
{ 80: } 78,
{ 81: } 78,
{ 82: } 78,
{ 83: } 81,
{ 84: } 84,
{ 85: } 84,
{ 86: } 84,
{ 87: } 91,
{ 88: } 91,
{ 89: } 95,
{ 90: } 95,
{ 91: } 95,
{ 92: } 95,
{ 93: } 95,
{ 94: } 95,
{ 95: } 95,
{ 96: } 95,
{ 97: } 95,
{ 98: } 95,
{ 99: } 95,
{ 100: } 95,
{ 101: } 103,
{ 102: } 103,
{ 103: } 103,
{ 104: } 103,
{ 105: } 105,
{ 106: } 107,
{ 107: } 109,
{ 108: } 109,
{ 109: } 109,
{ 110: } 112,
{ 111: } 112,
{ 112: } 119,
{ 113: } 123,
{ 114: } 123,
{ 115: } 126,
{ 116: } 126,
{ 117: } 130,
{ 118: } 134,
{ 119: } 134,
{ 120: } 135,
{ 121: } 136,
{ 122: } 137,
{ 123: } 138,
{ 124: } 138,
{ 125: } 138,
{ 126: } 138,
{ 127: } 138,
{ 128: } 138,
{ 129: } 141,
{ 130: } 141,
{ 131: } 145,
{ 132: } 145,
{ 133: } 145,
{ 134: } 149,
{ 135: } 153,
{ 136: } 153,
{ 137: } 159,
{ 138: } 159,
{ 139: } 159,
{ 140: } 159,
{ 141: } 160,
{ 142: } 160,
{ 143: } 160,
{ 144: } 160,
{ 145: } 160,
{ 146: } 161,
{ 147: } 161,
{ 148: } 161,
{ 149: } 165,
{ 150: } 165,
{ 151: } 165,
{ 152: } 165,
{ 153: } 165,
{ 154: } 165,
{ 155: } 165,
{ 156: } 166,
{ 157: } 167,
{ 158: } 167,
{ 159: } 167,
{ 160: } 171,
{ 161: } 171,
{ 162: } 171,
{ 163: } 171,
{ 164: } 171,
{ 165: } 174,
{ 166: } 174,
{ 167: } 174,
{ 168: } 174,
{ 169: } 174,
{ 170: } 174,
{ 171: } 174,
{ 172: } 178,
{ 173: } 182,
{ 174: } 186,
{ 175: } 190,
{ 176: } 194,
{ 177: } 198,
{ 178: } 203,
{ 179: } 207,
{ 180: } 211,
{ 181: } 215,
{ 182: } 219,
{ 183: } 223,
{ 184: } 227,
{ 185: } 231,
{ 186: } 235,
{ 187: } 239,
{ 188: } 239,
{ 189: } 241,
{ 190: } 241,
{ 191: } 244,
{ 192: } 244,
{ 193: } 251,
{ 194: } 251,
{ 195: } 252,
{ 196: } 253,
{ 197: } 257,
{ 198: } 261,
{ 199: } 265,
{ 200: } 265,
{ 201: } 265,
{ 202: } 265,
{ 203: } 265,
{ 204: } 265,
{ 205: } 271,
{ 206: } 271,
{ 207: } 271,
{ 208: } 271,
{ 209: } 271,
{ 210: } 271,
{ 211: } 271,
{ 212: } 271,
{ 213: } 271,
{ 214: } 271,
{ 215: } 271,
{ 216: } 271,
{ 217: } 271,
{ 218: } 271,
{ 219: } 271,
{ 220: } 271,
{ 221: } 271,
{ 222: } 271,
{ 223: } 271,
{ 224: } 271,
{ 225: } 271,
{ 226: } 273,
{ 227: } 273,
{ 228: } 273,
{ 229: } 273,
{ 230: } 277,
{ 231: } 277,
{ 232: } 284,
{ 233: } 288,
{ 234: } 289,
{ 235: } 290,
{ 236: } 291,
{ 237: } 291,
{ 238: } 291,
{ 239: } 291,
{ 240: } 295,
{ 241: } 297,
{ 242: } 297,
{ 243: } 298,
{ 244: } 298,
{ 245: } 298,
{ 246: } 298,
{ 247: } 298,
{ 248: } 298,
{ 249: } 298
);

yygh : array [0..yynstates-1] of Integer = (
{ 0: } 6,
{ 1: } 6,
{ 2: } 11,
{ 3: } 11,
{ 4: } 11,
{ 5: } 15,
{ 6: } 15,
{ 7: } 16,
{ 8: } 21,
{ 9: } 22,
{ 10: } 23,
{ 11: } 24,
{ 12: } 25,
{ 13: } 25,
{ 14: } 25,
{ 15: } 25,
{ 16: } 25,
{ 17: } 25,
{ 18: } 26,
{ 19: } 26,
{ 20: } 28,
{ 21: } 30,
{ 22: } 32,
{ 23: } 32,
{ 24: } 32,
{ 25: } 32,
{ 26: } 32,
{ 27: } 32,
{ 28: } 32,
{ 29: } 32,
{ 30: } 32,
{ 31: } 37,
{ 32: } 37,
{ 33: } 37,
{ 34: } 37,
{ 35: } 38,
{ 36: } 38,
{ 37: } 38,
{ 38: } 39,
{ 39: } 40,
{ 40: } 41,
{ 41: } 45,
{ 42: } 45,
{ 43: } 45,
{ 44: } 45,
{ 45: } 45,
{ 46: } 45,
{ 47: } 45,
{ 48: } 45,
{ 49: } 45,
{ 50: } 46,
{ 51: } 53,
{ 52: } 53,
{ 53: } 53,
{ 54: } 54,
{ 55: } 57,
{ 56: } 57,
{ 57: } 57,
{ 58: } 57,
{ 59: } 57,
{ 60: } 57,
{ 61: } 57,
{ 62: } 57,
{ 63: } 57,
{ 64: } 61,
{ 65: } 61,
{ 66: } 64,
{ 67: } 64,
{ 68: } 67,
{ 69: } 67,
{ 70: } 67,
{ 71: } 67,
{ 72: } 67,
{ 73: } 68,
{ 74: } 69,
{ 75: } 70,
{ 76: } 71,
{ 77: } 74,
{ 78: } 77,
{ 79: } 77,
{ 80: } 77,
{ 81: } 77,
{ 82: } 80,
{ 83: } 83,
{ 84: } 83,
{ 85: } 83,
{ 86: } 90,
{ 87: } 90,
{ 88: } 94,
{ 89: } 94,
{ 90: } 94,
{ 91: } 94,
{ 92: } 94,
{ 93: } 94,
{ 94: } 94,
{ 95: } 94,
{ 96: } 94,
{ 97: } 94,
{ 98: } 94,
{ 99: } 94,
{ 100: } 102,
{ 101: } 102,
{ 102: } 102,
{ 103: } 102,
{ 104: } 104,
{ 105: } 106,
{ 106: } 108,
{ 107: } 108,
{ 108: } 108,
{ 109: } 111,
{ 110: } 111,
{ 111: } 118,
{ 112: } 122,
{ 113: } 122,
{ 114: } 125,
{ 115: } 125,
{ 116: } 129,
{ 117: } 133,
{ 118: } 133,
{ 119: } 134,
{ 120: } 135,
{ 121: } 136,
{ 122: } 137,
{ 123: } 137,
{ 124: } 137,
{ 125: } 137,
{ 126: } 137,
{ 127: } 137,
{ 128: } 140,
{ 129: } 140,
{ 130: } 144,
{ 131: } 144,
{ 132: } 144,
{ 133: } 148,
{ 134: } 152,
{ 135: } 152,
{ 136: } 158,
{ 137: } 158,
{ 138: } 158,
{ 139: } 158,
{ 140: } 159,
{ 141: } 159,
{ 142: } 159,
{ 143: } 159,
{ 144: } 159,
{ 145: } 160,
{ 146: } 160,
{ 147: } 160,
{ 148: } 164,
{ 149: } 164,
{ 150: } 164,
{ 151: } 164,
{ 152: } 164,
{ 153: } 164,
{ 154: } 164,
{ 155: } 165,
{ 156: } 166,
{ 157: } 166,
{ 158: } 166,
{ 159: } 170,
{ 160: } 170,
{ 161: } 170,
{ 162: } 170,
{ 163: } 170,
{ 164: } 173,
{ 165: } 173,
{ 166: } 173,
{ 167: } 173,
{ 168: } 173,
{ 169: } 173,
{ 170: } 173,
{ 171: } 177,
{ 172: } 181,
{ 173: } 185,
{ 174: } 189,
{ 175: } 193,
{ 176: } 197,
{ 177: } 202,
{ 178: } 206,
{ 179: } 210,
{ 180: } 214,
{ 181: } 218,
{ 182: } 222,
{ 183: } 226,
{ 184: } 230,
{ 185: } 234,
{ 186: } 238,
{ 187: } 238,
{ 188: } 240,
{ 189: } 240,
{ 190: } 243,
{ 191: } 243,
{ 192: } 250,
{ 193: } 250,
{ 194: } 251,
{ 195: } 252,
{ 196: } 256,
{ 197: } 260,
{ 198: } 264,
{ 199: } 264,
{ 200: } 264,
{ 201: } 264,
{ 202: } 264,
{ 203: } 264,
{ 204: } 270,
{ 205: } 270,
{ 206: } 270,
{ 207: } 270,
{ 208: } 270,
{ 209: } 270,
{ 210: } 270,
{ 211: } 270,
{ 212: } 270,
{ 213: } 270,
{ 214: } 270,
{ 215: } 270,
{ 216: } 270,
{ 217: } 270,
{ 218: } 270,
{ 219: } 270,
{ 220: } 270,
{ 221: } 270,
{ 222: } 270,
{ 223: } 270,
{ 224: } 270,
{ 225: } 272,
{ 226: } 272,
{ 227: } 272,
{ 228: } 272,
{ 229: } 276,
{ 230: } 276,
{ 231: } 283,
{ 232: } 287,
{ 233: } 288,
{ 234: } 289,
{ 235: } 290,
{ 236: } 290,
{ 237: } 290,
{ 238: } 290,
{ 239: } 294,
{ 240: } 296,
{ 241: } 296,
{ 242: } 297,
{ 243: } 297,
{ 244: } 297,
{ 245: } 297,
{ 246: } 297,
{ 247: } 297,
{ 248: } 297,
{ 249: } 297
);

yyr : array [1..yynrules] of YYRRec = (
{ 1: } ( len: 1; sym: -2 ),
{ 2: } ( len: 0; sym: -4 ),
{ 3: } ( len: 2; sym: -3 ),
{ 4: } ( len: 2; sym: -3 ),
{ 5: } ( len: 1; sym: -3 ),
{ 6: } ( len: 1; sym: -3 ),
{ 7: } ( len: 1; sym: -7 ),
{ 8: } ( len: 0; sym: -7 ),
{ 9: } ( len: 1; sym: -8 ),
{ 10: } ( len: 1; sym: -8 ),
{ 11: } ( len: 1; sym: -8 ),
{ 12: } ( len: 1; sym: -8 ),
{ 13: } ( len: 1; sym: -8 ),
{ 14: } ( len: 1; sym: -8 ),
{ 15: } ( len: 1; sym: -8 ),
{ 16: } ( len: 0; sym: -8 ),
{ 17: } ( len: 4; sym: -9 ),
{ 18: } ( len: 0; sym: -9 ),
{ 19: } ( len: 6; sym: -5 ),
{ 20: } ( len: 2; sym: -5 ),
{ 21: } ( len: 5; sym: -5 ),
{ 22: } ( len: 3; sym: -5 ),
{ 23: } ( len: 3; sym: -5 ),
{ 24: } ( len: 8; sym: -6 ),
{ 25: } ( len: 4; sym: -6 ),
{ 26: } ( len: 3; sym: -6 ),
{ 27: } ( len: 5; sym: -6 ),
{ 28: } ( len: 3; sym: -6 ),
{ 29: } ( len: 3; sym: -16 ),
{ 30: } ( len: 3; sym: -16 ),
{ 31: } ( len: 3; sym: -18 ),
{ 32: } ( len: 3; sym: -18 ),
{ 33: } ( len: 4; sym: -13 ),
{ 34: } ( len: 3; sym: -13 ),
{ 35: } ( len: 4; sym: -13 ),
{ 36: } ( len: 3; sym: -13 ),
{ 37: } ( len: 2; sym: -13 ),
{ 38: } ( len: 2; sym: -13 ),
{ 39: } ( len: 3; sym: -13 ),
{ 40: } ( len: 2; sym: -13 ),
{ 41: } ( len: 2; sym: -11 ),
{ 42: } ( len: 3; sym: -11 ),
{ 43: } ( len: 2; sym: -11 ),
{ 44: } ( len: 3; sym: -11 ),
{ 45: } ( len: 2; sym: -11 ),
{ 46: } ( len: 2; sym: -11 ),
{ 47: } ( len: 1; sym: -11 ),
{ 48: } ( len: 1; sym: -11 ),
{ 49: } ( len: 2; sym: -17 ),
{ 50: } ( len: 1; sym: -17 ),
{ 51: } ( len: 3; sym: -20 ),
{ 52: } ( len: 1; sym: -10 ),
{ 53: } ( len: 1; sym: -21 ),
{ 54: } ( len: 2; sym: -21 ),
{ 55: } ( len: 1; sym: -21 ),
{ 56: } ( len: 1; sym: -21 ),
{ 57: } ( len: 2; sym: -21 ),
{ 58: } ( len: 3; sym: -21 ),
{ 59: } ( len: 2; sym: -21 ),
{ 60: } ( len: 1; sym: -21 ),
{ 61: } ( len: 2; sym: -21 ),
{ 62: } ( len: 2; sym: -21 ),
{ 63: } ( len: 1; sym: -21 ),
{ 64: } ( len: 1; sym: -21 ),
{ 65: } ( len: 1; sym: -21 ),
{ 66: } ( len: 1; sym: -19 ),
{ 67: } ( len: 1; sym: -19 ),
{ 68: } ( len: 3; sym: -12 ),
{ 69: } ( len: 4; sym: -12 ),
{ 70: } ( len: 2; sym: -12 ),
{ 71: } ( len: 1; sym: -12 ),
{ 72: } ( len: 2; sym: -23 ),
{ 73: } ( len: 2; sym: -23 ),
{ 74: } ( len: 1; sym: -25 ),
{ 75: } ( len: 3; sym: -25 ),
{ 76: } ( len: 1; sym: -25 ),
{ 77: } ( len: 1; sym: -26 ),
{ 78: } ( len: 1; sym: -26 ),
{ 79: } ( len: 1; sym: -26 ),
{ 80: } ( len: 2; sym: -22 ),
{ 81: } ( len: 3; sym: -22 ),
{ 82: } ( len: 2; sym: -22 ),
{ 83: } ( len: 2; sym: -22 ),
{ 84: } ( len: 3; sym: -22 ),
{ 85: } ( len: 3; sym: -22 ),
{ 86: } ( len: 1; sym: -22 ),
{ 87: } ( len: 4; sym: -22 ),
{ 88: } ( len: 2; sym: -22 ),
{ 89: } ( len: 4; sym: -22 ),
{ 90: } ( len: 3; sym: -22 ),
{ 91: } ( len: 2; sym: -29 ),
{ 92: } ( len: 3; sym: -29 ),
{ 93: } ( len: 2; sym: -24 ),
{ 94: } ( len: 3; sym: -24 ),
{ 95: } ( len: 2; sym: -24 ),
{ 96: } ( len: 4; sym: -24 ),
{ 97: } ( len: 2; sym: -24 ),
{ 98: } ( len: 4; sym: -24 ),
{ 99: } ( len: 3; sym: -24 ),
{ 100: } ( len: 0; sym: -24 ),
{ 101: } ( len: 1; sym: -27 ),
{ 102: } ( len: 3; sym: -30 ),
{ 103: } ( len: 3; sym: -30 ),
{ 104: } ( len: 3; sym: -30 ),
{ 105: } ( len: 3; sym: -30 ),
{ 106: } ( len: 3; sym: -30 ),
{ 107: } ( len: 3; sym: -30 ),
{ 108: } ( len: 3; sym: -30 ),
{ 109: } ( len: 3; sym: -30 ),
{ 110: } ( len: 3; sym: -30 ),
{ 111: } ( len: 3; sym: -30 ),
{ 112: } ( len: 3; sym: -30 ),
{ 113: } ( len: 3; sym: -30 ),
{ 114: } ( len: 3; sym: -30 ),
{ 115: } ( len: 3; sym: -30 ),
{ 116: } ( len: 3; sym: -30 ),
{ 117: } ( len: 3; sym: -30 ),
{ 118: } ( len: 1; sym: -30 ),
{ 119: } ( len: 3; sym: -31 ),
{ 120: } ( len: 1; sym: -33 ),
{ 121: } ( len: 0; sym: -33 ),
{ 122: } ( len: 1; sym: -32 ),
{ 123: } ( len: 1; sym: -32 ),
{ 124: } ( len: 1; sym: -32 ),
{ 125: } ( len: 3; sym: -32 ),
{ 126: } ( len: 3; sym: -32 ),
{ 127: } ( len: 2; sym: -32 ),
{ 128: } ( len: 2; sym: -32 ),
{ 129: } ( len: 2; sym: -32 ),
{ 130: } ( len: 4; sym: -32 ),
{ 131: } ( len: 4; sym: -32 ),
{ 132: } ( len: 5; sym: -32 ),
{ 133: } ( len: 6; sym: -32 ),
{ 134: } ( len: 4; sym: -32 ),
{ 135: } ( len: 3; sym: -32 ),
{ 136: } ( len: 3; sym: -14 ),
{ 137: } ( len: 1; sym: -14 ),
{ 138: } ( len: 0; sym: -14 ),
{ 139: } ( len: 3; sym: -35 ),
{ 140: } ( len: 1; sym: -35 ),
{ 141: } ( len: 1; sym: -15 ),
{ 142: } ( len: 3; sym: -34 ),
{ 143: } ( len: 1; sym: -34 ),
{ 144: } ( len: 0; sym: -34 ),
{ 145: } ( len: 1; sym: -36 )
);


const _error = 256; (* error token *)

function yyact(state, sym : Integer; var act : Integer) : Boolean;
  (* search action table *)
  var k : Integer;
  begin
    k := yyal[state];
    while (k<=yyah[state]) and (yya[k].sym<>sym) do inc(k);
    if k>yyah[state] then
      yyact := false
    else
      begin
        act := yya[k].act;
        yyact := true;
      end;
  end(*yyact*);

function yygoto(state, sym : Integer; var nstate : Integer) : Boolean;
  (* search goto table *)
  var k : Integer;
  begin
    k := yygl[state];
    while (k<=yygh[state]) and (yyg[k].sym<>sym) do inc(k);
    if k>yygh[state] then
      yygoto := false
    else
      begin
        nstate := yyg[k].act;
        yygoto := true;
      end;
  end(*yygoto*);

label parse, next, error, errlab, shift, reduce, accept, abort;

begin(*yyparse*)

  (* initialize: *)

  yystate := 0; yychar := -1; yynerrs := 0; yyerrflag := 0; yysp := 0;

{$ifdef yydebug}
  yydebug := true;
{$else}
  yydebug := false;
{$endif}

parse:

  (* push state and value: *)

  inc(yysp);
  if yysp>yymaxdepth then
    begin
      yyerror('yyparse stack overflow');
      goto abort;
    end;
  yys[yysp] := yystate; yyv[yysp] := yyval;

next:

  if (yyd[yystate]=0) and (yychar=-1) then
    (* get next symbol *)
    begin
      yychar := yylex; if yychar<0 then yychar := 0;
    end;

  if yydebug then writeln('state ', yystate, ', char ', yychar);

  (* determine parse action: *)

  yyn := yyd[yystate];
  if yyn<>0 then goto reduce; (* simple state *)

  (* no default action; search parse table *)

  if not yyact(yystate, yychar, yyn) then goto error
  else if yyn>0 then                      goto shift
  else if yyn<0 then                      goto reduce
  else                                    goto accept;

error:

  (* error; start error recovery: *)

  if yyerrflag=0 then yyerror('syntax error');

errlab:

  if yyerrflag=0 then inc(yynerrs);     (* new error *)

  if yyerrflag<=2 then                  (* incomplete recovery; try again *)
    begin
      yyerrflag := 3;
      (* uncover a state with shift action on error token *)
      while (yysp>0) and not ( yyact(yys[yysp], _error, yyn) and
                               (yyn>0) ) do
        begin
          if yydebug then
            if yysp>1 then
              writeln('error recovery pops state ', yys[yysp], ', uncovers ',
                      yys[yysp-1])
            else
              writeln('error recovery fails ... abort');
          dec(yysp);
        end;
      if yysp=0 then goto abort; (* parser has fallen from stack; abort *)
      yystate := yyn;            (* simulate shift on error *)
      goto parse;
    end
  else                                  (* no shift yet; discard symbol *)
    begin
      if yydebug then writeln('error recovery discards char ', yychar);
      if yychar=0 then goto abort; (* end of input; abort *)
      yychar := -1; goto next;     (* clear lookahead char and try again *)
    end;

shift:

  (* go to new state, clear lookahead character: *)

  yystate := yyn; yychar := -1; yyval := yylval;
  if yyerrflag>0 then dec(yyerrflag);

  goto parse;

reduce:

  (* execute action, pop rule from stack, and go to next state: *)

  if yydebug then writeln('reduce ', -yyn);

  yyflag := yyfnone; yyaction(-yyn);
  dec(yysp, yyr[-yyn].len);
  if yygoto(yys[yysp], yyr[-yyn].sym, yyn) then yystate := yyn;

  (* handle action calls to yyaccept, yyabort and yyerror: *)

  case yyflag of
    yyfaccept : goto accept;
    yyfabort  : goto abort;
    yyferror  : goto errlab;
  end;

  goto parse;

accept:

  yyparse := 0; exit;

abort:

  yyparse := 1; exit;

end(*yyparse*);


function yylex : Integer;
 begin
 yylex:=scan.yylex;
 end;

var r:integer; SS:string;

begin
   debug:=true;
   yydebug:=true;
   aktspace:='  ';
   block_type:=bt_no;
   IsExtern:=false;
   Assign(extfile,'ext.tmp'); rewrite(extfile);
   Assign(tempfile,'ext2.tmp'); rewrite(tempfile);
   r:=yyparse;
   if not(includefile) then
     begin
        writeln(outfile);
        writeln(outfile,'  implementation');
        writeln(outfile);
        writeln(outfile,'const External_library=''',libfilename,'''; {Setup as you need!}');
        writeln(outfile);
     end;
   reset(extfile);

   { here we have a problem if a line is longer than 255 chars !! }
   while not eof(extfile) do
    begin
    readln(extfile,SS);
    writeln(outfile,SS);
    end;

   writeln(outfile);

   if not(includefile) then
     writeln(outfile,'end.');

   close(extfile);
   erase(extfile);
   close(outfile);
   close(tempfile);
   erase(tempfile);
   close(textinfile);
end.

(*

 $Log$
 Revision 1.3  2000-02-09 16:44:15  peter
   * log truncated

 Revision 1.2  2000/01/07 16:46:04  daniel
   * copyright 2000

       #ifdef ID  to {$ifdef ID}
       #ifundef ID  to {$ifundef ID}
       #else to {$else}
       #define ID to {$define ID}
       #endif to {$endif}

      -"extern" fully handled . Adds proc/func + 'external _ExternalLibrary;'to
        implementation section
       you must assign _ExternalLibrary later.

      -"const" skips in func/proc arguments.

      changes in convert.y and scan.l
      - "convert" renamed to "h2pas"
      - Inserted the checking "IsAssigned(Pointer)" everywhere access to pointers
       It preserv from Acces Violation Errors.
      - A little remade for TP Lex and Yacc 4.01 -
           changed function "input" to "get_char"
      -!!! because of peculiarity TPLY4.01 you must create unit CONVERU.PAS by
       your hand! Cut const definitions from CONVERT.PAS and paste into CONVERU.PAS

 What need
   * handle struct a {  }; in the right way
   * all predefined C types
   * misplaced comments
   * handle functions without result
*)
