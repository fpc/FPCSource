
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
     SHORT_STR = 'smallint';
     UINT_STR = 'dword';
     USHORT_STR = 'word';
     CHAR_STR = 'char';
     { should we use byte or char for 'unsigned char' ?? }
     UCHAR_STR = 'byte';
     REAL_STR = 'double';

  var
     hp,ph    : presobject;
     extfile  : text;  (* file for implementation headers extern procs *)
     IsExtern : boolean;
     must_write_packed_field : boolean;
     tempfile : text;
     No_pop   : boolean;
     s,TN,PN  : String;

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
                       If removeUnderscore then
                         begin
                         Write (outfile,'para',para);
                         Length:=(Length+5);
                         end
                       else
                         begin
                         write(outfile,'_para',para);
                         length:=length+6;
                         end;
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
                                        write(outfile,'P');
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
                 write(outfile,'P');
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
         
         if not stripinfo then
         begin
         writeln(outfile,'(* error ');
         writeln(outfile,yyline);
         end;
         
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
         if not CompactMode then
         begin
         write(outfile,aktspace);
         if not IsExtern then
         write(extfile,aktspace);
         end;
         (* distinguish between procedure and function *)
         if assigned(yyv[yysp-4]) then
         if (yyv[yysp-4]^.typ=t_void) and (yyv[yysp-2]^.p1^.p1^.p1=nil) then
         begin
         shift(10);
         write(outfile,'procedure ',yyv[yysp-2]^.p1^.p2^.p);
         if assigned(yyv[yysp-2]^.p1^.p1^.p2) then
         write_args(outfile,yyv[yysp-2]^.p1^.p1^.p2);
         if not IsExtern then
         begin
         write(extfile,'procedure ',yyv[yysp-2]^.p1^.p2^.p);
         if assigned(yyv[yysp-2]^.p1^.p1^.p2) then
         write_args(extfile,yyv[yysp-2]^.p1^.p1^.p2);
         end;
         end
         else
         begin
         shift(9);
         write(outfile,'function ',yyv[yysp-2]^.p1^.p2^.p);
         if assigned(yyv[yysp-2]^.p1^.p1^.p2) then
         write_args(outfile,yyv[yysp-2]^.p1^.p1^.p2);
         write(outfile,':');
         write_p_a_def(outfile,yyv[yysp-2]^.p1^.p1^.p1,yyv[yysp-4]);
         if not IsExtern then
         begin
         write(extfile,'function ',yyv[yysp-2]^.p1^.p2^.p);
         if assigned(yyv[yysp-2]^.p1^.p1^.p2) then
         write_args(extfile,yyv[yysp-2]^.p1^.p1^.p2);
         write(extfile,':');
         write_p_a_def(extfile,yyv[yysp-2]^.p1^.p1^.p1,yyv[yysp-4]);
         end;
         end;
         if assigned(yyv[yysp-1]) then
         write(outfile,';systrap ',yyv[yysp-1]^.p);
         (* No CDECL in interface for Uselib *)
         if IsExtern and (not no_pop) then
         write(outfile,';cdecl');
         popshift;
         if UseLib then
         begin
         if IsExtern then
         begin
         write (outfile,';external');
         If UseName then
         Write(outfile,' External_library name ''',yyv[yysp-2]^.p1^.p2^.p,'''');
         end;
         writeln(outfile,';');
         end
         else
         begin
         writeln(extfile,';');
         writeln(outfile,';');
         if not IsExtern then
         begin
         writeln(extfile,aktspace,'begin');
         writeln(extfile,aktspace,'  { You must implemented this function }');
         writeln(extfile,aktspace,'end;');
         end;
         end;
         IsExtern:=false;
         if not compactmode then
         writeln(outfile);
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
         block_type:=bt_type;
         end;
         shift(3);
         (* write new type name *)
         TN:=strpas(yyv[yysp-1]^.p2^.p);
         if RemoveUnderScore and (length(tn)>1) and (tn[1]='_') then
         Delete(TN,1,1);
         if UsePPointers and
         ((yyv[yysp-1]^.typ=t_structdef) or (yyv[yysp-1]^.typ=t_uniondef)) then
         begin
         PN:='P'+TN;
         if PrependTypes then
         TN:='T'+TN;
         Writeln (outfile,aktspace,PN,' = ^',TN,';');
         end;
         write(outfile,aktspace,TN,' = ');
         shift(2);
         hp:=yyv[yysp-1];
         write_type_specifier(outfile,hp);
         popshift;
         (* enum_to_const can make a switch to const *)
         if block_type=bt_type then
         writeln(outfile,';');
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
         block_type:=bt_type;
         end;
         PN:=yyv[yysp-2]^.p;
         TN:=yyv[yysp-1]^.p;
         if RemoveUnderscore then
         begin
         if (length(pn)>1) and (PN[1]='_') then
         Delete(Pn,1,1);
         if (length(tn)>1) and (tN[1]='_') then
         Delete(tn,1,1);
         end;
         if Uppercase(tn)<>Uppercase(pn) then
         begin
         shift(3);
         writeln(outfile,aktspace,PN,' = ',TN,';');
         popshift;
         end;
         if assigned(yyv[yysp-2]) then
         dispose(yyv[yysp-2],done);
         if assigned(yyv[yysp-1]) then
         dispose(yyv[yysp-1],done);
         
       end;
  22 : begin
         
         if block_type<>bt_type then
         begin
         writeln(outfile);
         writeln(outfile,aktspace,'type');
         block_type:=bt_type;
         end;
         no_pop:=assigned(yyv[yysp-5]) and (yyv[yysp-5]^.str='no_pop');
         shift(3);
         (* walk through all declarations *)
         hp:=yyv[yysp-4];
         ph:=nil;
         if assigned(hp) then
         begin
         hp:=yyv[yysp-1];
         while assigned(hp^.p1) do
         hp:=hp^.p1;
         hp^.p1:=new(presobject,init_two(t_procdef,nil,yyv[yysp-1]));
         hp:=yyv[yysp-4];
         if assigned(hp^.p1) and assigned(hp^.p1^.p2) then
         begin
         writeln(outfile);
         (* write new type name *)
         write(outfile,aktspace,hp^.p1^.p2^.p);
         write(outfile,' = ');
         shift(2);
         if assigned(ph) then
         write_p_a_def(outfile,hp^.p1^.p1,ph)
         else
         write_p_a_def(outfile,hp^.p1^.p1,yyv[yysp-7]);
         (* simple def ? keep the name for the other defs *)
         if (ph=nil) and (hp^.p1^.p1=nil) then
         ph:=hp^.p1^.p2;
         popshift;
         (* if no_pop it is normal fpc calling convention *)
         if is_procvar and
         (not no_pop) then
         write(outfile,';cdecl');
         writeln(outfile,';');
         flush(outfile);
         end;
         end;
         popshift;
         if assigned(yyv[yysp-7])then
         dispose(yyv[yysp-7],done);
         if assigned(yyv[yysp-5])then
         dispose(yyv[yysp-5],done);
         if assigned(yyv[yysp-4])then (* disposes also yyv[yysp-1] *)
         dispose(yyv[yysp-4],done);
         
       end;
  23 : begin
         
         if block_type<>bt_type then
         begin
         writeln(outfile);
         writeln(outfile,aktspace,'type');
         block_type:=bt_type;
         end;
         
         no_pop:=assigned(yyv[yysp-2]) and (yyv[yysp-2]^.str='no_pop');
         shift(3);
         (* walk through all declarations *)
         hp:=yyv[yysp-1];
         ph:=nil;
         is_procvar:=false;
         while assigned(hp) do
         begin
         if assigned(hp^.p1) and assigned(hp^.p1^.p2) then
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
         (* simple def ? keep the name for the other defs *)
         if (ph=nil) and (hp^.p1^.p1=nil) then
         ph:=hp^.p1^.p2;
         popshift;
         (* if no_pop it is normal fpc calling convention *)
         if is_procvar and
         (not no_pop) then
         write(outfile,';cdecl');
         writeln(outfile,';');
         flush(outfile);
         end;
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
  24 : begin
         
         if block_type<>bt_type then
         begin
         writeln(outfile);
         writeln(outfile,aktspace,'type');
         block_type:=bt_type;
         end;
         shift(3);
         (* write as pointer *)
         writeln(outfile);
         writeln(outfile,'(* generic typedef  *)');
         writeln(outfile,aktspace,yyv[yysp-1]^.p,' = pointer;');
         flush(outfile);
         popshift;
         if assigned(yyv[yysp-1]) then
         dispose(yyv[yysp-1],done);
         
       end;
  25 : begin
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
  26 : begin
         
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
  27 : begin
         
         writeln(outfile,'{$define ',yyv[yysp-2]^.p,'}');
         flush(outfile);
         if assigned(yyv[yysp-2])then
         dispose(yyv[yysp-2],done);
         
       end;
  28 : begin
         
         writeln(outfile,'{$define ',yyv[yysp-1]^.p,'}');
         flush(outfile);
         if assigned(yyv[yysp-1])then
         dispose(yyv[yysp-1],done);
         
       end;
  29 : begin
         
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
  30 : begin
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
  31 : begin
         yyval:=yyv[yysp-1];
       end;
  32 : begin
         writeln(outfile,' in member_list *)');
         yyerrok;
         yyval:=nil;
         
       end;
  33 : begin
         yyval:=yyv[yysp-1];
       end;
  34 : begin
         writeln(outfile,' in enum_list *)');
         yyerrok;
         yyval:=nil;
         
       end;
  35 : begin
         
         if not is_packed then
         writeln(outfile,'{$PACKRECORDS 1}');
         is_packed:=true;
         yyval:=new(presobject,init_two(t_structdef,yyv[yysp-1],yyv[yysp-2]));
         
       end;
  36 : begin
         
         if is_packed then
         writeln(outfile,'{$PACKRECORDS 4}');
         is_packed:=false;
         yyval:=new(presobject,init_two(t_structdef,yyv[yysp-0],yyv[yysp-1]));
         
       end;
  37 : begin
         
         if not is_packed then
         writeln(outfile,'{$PACKRECORDS 1}');
         is_packed:=true;
         yyval:=new(presobject,init_two(t_uniondef,yyv[yysp-1],yyv[yysp-2]));
         
       end;
  38 : begin
         
         yyval:=new(presobject,init_two(t_uniondef,yyv[yysp-0],yyv[yysp-1]));
         
       end;
  39 : begin
         
         yyval:=new(presobject,init_two(t_uniondef,nil,yyv[yysp-0]));
         
       end;
  40 : begin
         
         yyval:=new(presobject,init_two(t_structdef,nil,yyv[yysp-0]));
         
       end;
  41 : begin
         
         yyval:=new(presobject,init_two(t_enumdef,yyv[yysp-0],yyv[yysp-1]));
         
       end;
  42 : begin
         
         yyval:=new(presobject,init_two(t_enumdef,nil,yyv[yysp-0]));
         
       end;
  43 : begin
         
         if not stripinfo then
         writeln(outfile,'(* Const before type ignored *)');
         yyval:=yyv[yysp-0];
         
       end;
  44 : begin
         
         if not is_packed then
         writeln(outfile,'{$PACKRECORDS 1}');
         is_packed:=true;
         yyval:=new(presobject,init_one(t_uniondef,yyv[yysp-1]));
         
       end;
  45 : begin
         
         yyval:=new(presobject,init_one(t_uniondef,yyv[yysp-0]));
         
       end;
  46 : begin
         
         if not is_packed then
         writeln(outfile,'{$PACKRECORDS 1}');
         is_packed:=true;
         yyval:=new(presobject,init_one(t_structdef,yyv[yysp-1]));
         
       end;
  47 : begin
         
         if is_packed then
         writeln(outfile,'{$PACKRECORDS 4}');
         is_packed:=false;
         yyval:=new(presobject,init_one(t_structdef,yyv[yysp-0]));
         
       end;
  48 : begin
         
         yyval:=new(presobject,init_one(t_enumdef,yyv[yysp-0]));
         
       end;
  49 : begin
         
         yyval:=yyv[yysp-0];
         
       end;
  50 : begin
         yyval:=yyv[yysp-0]; 
       end;
  51 : begin
         
         yyval:=new(presobject,init_one(t_memberdeclist,yyv[yysp-1]));
         yyval^.next:=yyv[yysp-0];
         
       end;
  52 : begin
         
         yyval:=new(presobject,init_one(t_memberdeclist,yyv[yysp-0]));
         
       end;
  53 : begin
         
         yyval:=new(presobject,init_two(t_memberdec,yyv[yysp-2],yyv[yysp-1]));
         
       end;
  54 : begin
         (*dname*)
         yyval:=new(presobject,init_id(act_token));
         
       end;
  55 : begin
         
         yyval:=new(presobject,init_id(INT_STR));
         
       end;
  56 : begin
         
         yyval:=new(presobject,init_id(UINT_STR));
         
       end;
  57 : begin
         
         yyval:=new(presobject,init_id(INT_STR));
         
       end;
  58 : begin
         
         yyval:=new(presobject,init_id(REAL_STR));
         
       end;
  59 : begin
         
         yyval:=new(presobject,init_id(INT_STR));
         
       end;
  60 : begin
         
         yyval:=new(presobject,init_id(UINT_STR));
         
       end;
  61 : begin
         
         yyval:=new(presobject,init_id(UINT_STR));
         
       end;
  62 : begin
         
         yyval:=new(presobject,init_id(UINT_STR));
         
       end;
  63 : begin
         
         yyval:=new(presobject,init_id(USHORT_STR));
         
       end;
  64 : begin
         
         yyval:=new(presobject,init_id(UCHAR_STR));
         
       end;
  65 : begin
         
         yyval:=new(presobject,init_no(t_void));
         
       end;
  66 : begin
         
         yyval:=new(presobject,init_id(SHORT_STR));
         
       end;
  67 : begin
         
         yyval:=new(presobject,init_id(CHAR_STR));
         
       end;
  68 : begin
         
         yyval:=yyv[yysp-0];
         
       end;
  69 : begin
         
         yyval:=yyv[yysp-0];
         tn:=yyval^.str;
         if removeunderscore and
         (length(tn)>1) and (tn[1]='_') then
         yyval^.setstr(Copy(tn,2,length(tn)-1));
         
       end;
  70 : begin
         
         yyval:=yyv[yysp-2];
         hp:=yyv[yysp-2];
         while assigned(hp^.next) do
         hp:=hp^.next;
         hp^.next:=new(presobject,init_one(t_declist,yyv[yysp-0]));
         
       end;
  71 : begin
         
         writeln(outfile,' in declarator_list *)');
         yyval:=yyv[yysp-0];
         yyerrok;
         
       end;
  72 : begin
         
         writeln(outfile,' in declarator_list *)');
         yyerrok;
         
       end;
  73 : begin
         
         yyval:=new(presobject,init_one(t_declist,yyv[yysp-0]));
         
       end;
  74 : begin
         
         yyval:=new(presobject,init_two(t_arg,yyv[yysp-1],yyv[yysp-0]));
         
       end;
  75 : begin
         
         hp:=new(presobject,init_one(t_pointerdef,yyv[yysp-2]));
         yyval:=new(presobject,init_two(t_arg,hp,yyv[yysp-0]));
         
       end;
  76 : begin
         
         yyval:=new(presobject,init_two(t_arg,yyv[yysp-1],yyv[yysp-0]));
         
       end;
  77 : begin
         
         yyval:=new(presobject,init_two(t_arglist,yyv[yysp-0],nil));
         
       end;
  78 : begin
         
         yyval:=new(presobject,init_two(t_arglist,yyv[yysp-2],nil));
         yyval^.next:=yyv[yysp-0];
         
       end;
  79 : begin
         
         yyval:=new(presobject,init_two(t_arglist,ellipsisarg,nil));
         (*** ELLIPSIS PROBLEM ***)
         
       end;
  80 : begin
         yyval:=new(presobject,init_id('far'));
       end;
  81 : begin
         yyval:=new(presobject,init_id('near'));
       end;
  82 : begin
         yyval:=new(presobject,init_id('huge'));
       end;
  83 : begin
         
         if not stripinfo then
         writeln(outfile,'(* Const before declarator ignored *)');
         yyval:=yyv[yysp-0];
         
       end;
  84 : begin
         
         if not stripinfo then
         writeln(outfile,aktspace,'(* ',yyv[yysp-2]^.p,' ignored *)');
         dispose(yyv[yysp-2],done);
         hp:=yyv[yysp-0];
         yyval:=hp;
         while assigned(hp^.p1) do
         hp:=hp^.p1;
         hp^.p1:=new(presobject,init_one(t_pointerdef,nil));
         
       end;
  85 : begin
         
         (* %prec PSTAR this was wrong!! *)
         hp:=yyv[yysp-0];
         yyval:=hp;
         while assigned(hp^.p1) do
         hp:=hp^.p1;
         hp^.p1:=new(presobject,init_one(t_pointerdef,nil));
         
       end;
  86 : begin
         
         hp:=yyv[yysp-0];
         yyval:=hp;
         while assigned(hp^.p1) do
         hp:=hp^.p1;
         hp^.p1:=new(presobject,init_one(t_addrdef,nil));
         
       end;
  87 : begin
         
         (*  size specifier supported *)
         hp:=new(presobject,init_one(t_size_specifier,yyv[yysp-0]));
         yyval:=new(presobject,init_three(t_dec,nil,yyv[yysp-2],hp));
         
       end;
  88 : begin
         
         if not stripinfo then
         writeln(outfile,'(* Warning : default value for ',yyv[yysp-2]^.p,' ignored *)');
         hp:=new(presobject,init_one(t_default_value,yyv[yysp-0]));
         yyval:=new(presobject,init_three(t_dec,nil,yyv[yysp-2],hp));
         
       end;
  89 : begin
         
         yyval:=new(presobject,init_two(t_dec,nil,yyv[yysp-0]));
         
       end;
  90 : begin
         
         hp:=yyv[yysp-3];
         yyval:=hp;
         while assigned(hp^.p1) do
         hp:=hp^.p1;
         hp^.p1:=new(presobject,init_two(t_procdef,nil,yyv[yysp-1]));
         
       end;
  91 : begin
         
         hp:=yyv[yysp-1];
         yyval:=hp;
         while assigned(hp^.p1) do
         hp:=hp^.p1;
         hp^.p1:=new(presobject,init_two(t_procdef,nil,nil));
         
       end;
  92 : begin
         
         hp:=yyv[yysp-3];
         yyval:=hp;
         while assigned(hp^.p1) do
         hp:=hp^.p1;
         hp^.p1:=new(presobject,init_two(t_arraydef,nil,yyv[yysp-1]));
         
       end;
  93 : begin
         yyval:=yyv[yysp-1]; 
       end;
  94 : begin
         yyval := yyv[yysp-1];
       end;
  95 : begin
         yyval := yyv[yysp-2];
       end;
  96 : begin
         
         if not stripinfo then
         writeln(outfile,'(* Const before abstract_declarator ignored *)');
         yyval:=yyv[yysp-0];
         
       end;
  97 : begin
         
         if not stripinfo then
         writeln(outfile,aktspace,'(* ',yyv[yysp-2]^.p,' ignored *)');
         dispose(yyv[yysp-2],done);
         hp:=yyv[yysp-0];
         yyval:=hp;
         while assigned(hp^.p1) do
         hp:=hp^.p1;
         hp^.p1:=new(presobject,init_one(t_pointerdef,nil));
         
       end;
  98 : begin
         
         hp:=yyv[yysp-0];
         yyval:=hp;
         while assigned(hp^.p1) do
         hp:=hp^.p1;
         hp^.p1:=new(presobject,init_one(t_pointerdef,nil));
         
       end;
  99 : begin
         
         hp:=yyv[yysp-3];
         yyval:=hp;
         while assigned(hp^.p1) do
         hp:=hp^.p1;
         hp^.p1:=new(presobject,init_two(t_procdef,nil,yyv[yysp-1]));
         
       end;
 100 : begin
         
         hp:=yyv[yysp-1];
         yyval:=hp;
         while assigned(hp^.p1) do
         hp:=hp^.p1;
         hp^.p1:=new(presobject,init_two(t_procdef,nil,nil));
         
       end;
 101 : begin
         
         hp:=yyv[yysp-3];
         yyval:=hp;
         while assigned(hp^.p1) do
         hp:=hp^.p1;
         hp^.p1:=new(presobject,init_two(t_arraydef,nil,yyv[yysp-1]));
         
       end;
 102 : begin
         yyval:=yyv[yysp-1]; 
       end;
 103 : begin
         
         yyval:=new(presobject,init_two(t_dec,nil,nil));
         
       end;
 104 : begin
         yyval:=yyv[yysp-0];
       end;
 105 : begin
         yyval:=new(presobject,init_bop(' = ',yyv[yysp-2],yyv[yysp-0]));
       end;
 106 : begin
         yyval:=new(presobject,init_bop(' <> ',yyv[yysp-2],yyv[yysp-0]));
       end;
 107 : begin
         yyval:=new(presobject,init_bop(' > ',yyv[yysp-2],yyv[yysp-0]));
       end;
 108 : begin
         yyval:=new(presobject,init_bop(' >= ',yyv[yysp-2],yyv[yysp-0]));
       end;
 109 : begin
         yyval:=new(presobject,init_bop(' < ',yyv[yysp-2],yyv[yysp-0]));
       end;
 110 : begin
         yyval:=new(presobject,init_bop(' <= ',yyv[yysp-2],yyv[yysp-0]));
       end;
 111 : begin
         yyval:=new(presobject,init_bop(' + ',yyv[yysp-2],yyv[yysp-0]));
       end;
 112 : begin
         yyval:=new(presobject,init_bop(' - ',yyv[yysp-2],yyv[yysp-0]));
       end;
 113 : begin
         yyval:=new(presobject,init_bop(' * ',yyv[yysp-2],yyv[yysp-0]));
       end;
 114 : begin
         yyval:=new(presobject,init_bop(' / ',yyv[yysp-2],yyv[yysp-0]));
       end;
 115 : begin
         yyval:=new(presobject,init_bop(' or ',yyv[yysp-2],yyv[yysp-0]));
       end;
 116 : begin
         yyval:=new(presobject,init_bop(' and ',yyv[yysp-2],yyv[yysp-0]));
       end;
 117 : begin
         yyval:=new(presobject,init_bop(' not ',yyv[yysp-2],yyv[yysp-0]));
       end;
 118 : begin
         yyval:=new(presobject,init_bop(' shl ',yyv[yysp-2],yyv[yysp-0]));
       end;
 119 : begin
         yyval:=new(presobject,init_bop(' shr ',yyv[yysp-2],yyv[yysp-0]));
       end;
 120 : begin
         yyv[yysp-0]^.p1:=yyv[yysp-2];
         yyval:=yyv[yysp-0];
         inc(if_nb);
         yyval^.p:=strpnew('if_local'+str(if_nb));
         
       end;
 121 : begin
         yyval:=yyv[yysp-0];
       end;
 122 : begin
         (* if A then B else C *)
         yyval:=new(presobject,init_three(t_ifexpr,nil,yyv[yysp-2],yyv[yysp-0]));
       end;
 123 : begin
         yyval:=yyv[yysp-0]; 
       end;
 124 : begin
         yyval:=nil;
       end;
 125 : begin
         
         yyval:=yyv[yysp-0];
         
       end;
 126 : begin
         
         (* remove L prefix for widestrings *)
         s:=act_token;
         if Win32headers and (s[1]='L') then
         delete(s,1,1);
         yyval:=new(presobject,init_id(''''+copy(s,2,length(s)-2)+''''));
         
       end;
 127 : begin
         
         yyval:=new(presobject,init_id(act_token));
         
       end;
 128 : begin
         
         yyval:=new(presobject,init_bop('.',yyv[yysp-2],yyv[yysp-0]));
         
       end;
 129 : begin
         
         yyval:=new(presobject,init_bop('^.',yyv[yysp-2],yyv[yysp-0]));
         
       end;
 130 : begin
         
         yyval:=new(presobject,init_preop('-',yyv[yysp-0]));
         
       end;
 131 : begin
         
         yyval:=new(presobject,init_preop('@',yyv[yysp-0]));
         
       end;
 132 : begin
         
         yyval:=new(presobject,init_preop(' not ',yyv[yysp-0]));
         
       end;
 133 : begin
         
         if assigned(yyv[yysp-0]) then
         yyval:=new(presobject,init_two(t_typespec,yyv[yysp-2],yyv[yysp-0]))
         else
         yyval:=yyv[yysp-2];
         
       end;
 134 : begin
         
         yyval:=new(presobject,init_two(t_typespec,yyv[yysp-2],yyv[yysp-0]));
         
       end;
 135 : begin
         
         hp:=new(presobject,init_one(t_pointerdef,yyv[yysp-3]));
         yyval:=new(presobject,init_two(t_typespec,hp,yyv[yysp-0]));
         
       end;
 136 : begin
         
         if not stripinfo then
         writeln(outfile,aktspace,'(* ',yyv[yysp-3]^.p,' ignored *)');
         dispose(yyv[yysp-3],done);
         write_type_specifier(outfile,yyv[yysp-4]);
         writeln(outfile,' ignored *)');
         hp:=new(presobject,init_one(t_pointerdef,yyv[yysp-4]));
         yyval:=new(presobject,init_two(t_typespec,hp,yyv[yysp-0]));
         
       end;
 137 : begin
         
         hp:=new(presobject,init_one(t_exprlist,yyv[yysp-3]));
         yyval:=new(presobject,init_three(t_funexprlist,hp,yyv[yysp-1],nil));
         
       end;
 138 : begin
         
         yyval:=yyv[yysp-1];
         
       end;
 139 : begin
         (*enum_element COMMA enum_list *)
         yyval:=yyv[yysp-2];
         yyval^.next:=yyv[yysp-0];
         
       end;
 140 : begin
         
         yyval:=yyv[yysp-0];
         
       end;
 141 : begin
         (* empty enum list *)
         yyval:=nil;
       end;
 142 : begin
         begin (*enum_element: dname _ASSIGN expr *)
         yyval:=new(presobject,init_two(t_enumlist,yyv[yysp-2],yyv[yysp-0]));
         end;
         
       end;
 143 : begin
         
         begin (*enum_element: dname*)
         yyval:=new(presobject,init_two(t_enumlist,yyv[yysp-0],nil));
         end;
         
       end;
 144 : begin
         
         if yyv[yysp-0]^.typ=t_funexprlist then
         yyval:=yyv[yysp-0]
         else
         yyval:=new(presobject,init_two(t_exprlist,yyv[yysp-0],nil));
         (* if here is a type specifier
         we know the return type *)
         if (yyv[yysp-0]^.typ=t_typespec) then
         yyval^.p3:=yyv[yysp-0]^.p1^.get_copy;
         
       end;
 145 : begin
         (*exprlist COMMA expr*)
         yyval:=yyv[yysp-2];
         yyv[yysp-2]^.next:=yyv[yysp-0];
         
       end;
 146 : begin
         
         yyval:=yyv[yysp-0];
         
       end;
 147 : begin
         (* empty expression list *)
         yyval:=nil; 
       end;
 148 : begin
         
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

yynacts   = 1939;
yyngotos  = 318;
yynstates = 263;
yynrules  = 148;

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
  ( sym: 268; act: 37 ),
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
  ( sym: 289; act: 43 ),
  ( sym: 290; act: 44 ),
  ( sym: 291; act: 45 ),
  ( sym: 292; act: 46 ),
  ( sym: 293; act: 47 ),
  ( sym: 294; act: 48 ),
  ( sym: 295; act: 49 ),
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
  ( sym: 256; act: 51 ),
  ( sym: 266; act: 52 ),
  ( sym: 271; act: 23 ),
{ 21: }
  ( sym: 256; act: 51 ),
  ( sym: 266; act: 52 ),
  ( sym: 271; act: 23 ),
{ 22: }
  ( sym: 256; act: 55 ),
  ( sym: 266; act: 56 ),
  ( sym: 271; act: 23 ),
{ 23: }
{ 24: }
{ 25: }
  ( sym: 274; act: 57 ),
  ( sym: 276; act: 58 ),
  ( sym: 277; act: 59 ),
  ( sym: 279; act: 60 ),
  ( sym: 256; act: -62 ),
  ( sym: 261; act: -62 ),
  ( sym: 262; act: -62 ),
  ( sym: 263; act: -62 ),
  ( sym: 264; act: -62 ),
  ( sym: 271; act: -62 ),
  ( sym: 281; act: -62 ),
  ( sym: 282; act: -62 ),
  ( sym: 283; act: -62 ),
  ( sym: 284; act: -62 ),
  ( sym: 289; act: -62 ),
  ( sym: 290; act: -62 ),
  ( sym: 291; act: -62 ),
  ( sym: 292; act: -62 ),
  ( sym: 293; act: -62 ),
  ( sym: 294; act: -62 ),
  ( sym: 295; act: -62 ),
  ( sym: 308; act: -62 ),
  ( sym: 313; act: -62 ),
{ 26: }
  ( sym: 277; act: 61 ),
  ( sym: 256; act: -57 ),
  ( sym: 261; act: -57 ),
  ( sym: 262; act: -57 ),
  ( sym: 263; act: -57 ),
  ( sym: 264; act: -57 ),
  ( sym: 271; act: -57 ),
  ( sym: 281; act: -57 ),
  ( sym: 282; act: -57 ),
  ( sym: 283; act: -57 ),
  ( sym: 284; act: -57 ),
  ( sym: 289; act: -57 ),
  ( sym: 290; act: -57 ),
  ( sym: 291; act: -57 ),
  ( sym: 292; act: -57 ),
  ( sym: 293; act: -57 ),
  ( sym: 294; act: -57 ),
  ( sym: 295; act: -57 ),
  ( sym: 308; act: -57 ),
  ( sym: 313; act: -57 ),
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
  ( sym: 260; act: 63 ),
  ( sym: 286; act: 64 ),
{ 35: }
  ( sym: 262; act: 66 ),
  ( sym: 289; act: 43 ),
  ( sym: 290; act: 44 ),
  ( sym: 291; act: 45 ),
  ( sym: 292; act: 46 ),
  ( sym: 293; act: 47 ),
  ( sym: 294; act: 48 ),
  ( sym: 295; act: 49 ),
  ( sym: 256; act: -16 ),
  ( sym: 271; act: -16 ),
  ( sym: 281; act: -16 ),
  ( sym: 282; act: -16 ),
  ( sym: 283; act: -16 ),
  ( sym: 284; act: -16 ),
  ( sym: 308; act: -16 ),
  ( sym: 313; act: -16 ),
{ 36: }
  ( sym: 260; act: 67 ),
  ( sym: 256; act: -69 ),
  ( sym: 262; act: -69 ),
  ( sym: 271; act: -69 ),
  ( sym: 281; act: -69 ),
  ( sym: 282; act: -69 ),
  ( sym: 283; act: -69 ),
  ( sym: 284; act: -69 ),
  ( sym: 289; act: -69 ),
  ( sym: 290; act: -69 ),
  ( sym: 291; act: -69 ),
  ( sym: 292; act: -69 ),
  ( sym: 293; act: -69 ),
  ( sym: 294; act: -69 ),
  ( sym: 295; act: -69 ),
  ( sym: 308; act: -69 ),
  ( sym: 313; act: -69 ),
{ 37: }
  ( sym: 256; act: 51 ),
  ( sym: 266; act: 52 ),
  ( sym: 271; act: 23 ),
{ 38: }
  ( sym: 262; act: 69 ),
  ( sym: 286; act: 70 ),
  ( sym: 287; act: 71 ),
{ 39: }
  ( sym: 256; act: 51 ),
  ( sym: 266; act: 52 ),
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
{ 40: }
  ( sym: 256; act: 51 ),
  ( sym: 266; act: 52 ),
  ( sym: 260; act: -39 ),
  ( sym: 261; act: -39 ),
  ( sym: 262; act: -39 ),
  ( sym: 263; act: -39 ),
  ( sym: 264; act: -39 ),
  ( sym: 271; act: -39 ),
  ( sym: 281; act: -39 ),
  ( sym: 282; act: -39 ),
  ( sym: 283; act: -39 ),
  ( sym: 284; act: -39 ),
  ( sym: 289; act: -39 ),
  ( sym: 290; act: -39 ),
  ( sym: 291; act: -39 ),
  ( sym: 292; act: -39 ),
  ( sym: 293; act: -39 ),
  ( sym: 294; act: -39 ),
  ( sym: 295; act: -39 ),
  ( sym: 308; act: -39 ),
  ( sym: 313; act: -39 ),
{ 41: }
  ( sym: 256; act: 55 ),
  ( sym: 266; act: 56 ),
  ( sym: 260; act: -42 ),
  ( sym: 261; act: -42 ),
  ( sym: 262; act: -42 ),
  ( sym: 263; act: -42 ),
  ( sym: 264; act: -42 ),
  ( sym: 271; act: -42 ),
  ( sym: 281; act: -42 ),
  ( sym: 282; act: -42 ),
  ( sym: 283; act: -42 ),
  ( sym: 284; act: -42 ),
  ( sym: 289; act: -42 ),
  ( sym: 290; act: -42 ),
  ( sym: 291; act: -42 ),
  ( sym: 292; act: -42 ),
  ( sym: 293; act: -42 ),
  ( sym: 294; act: -42 ),
  ( sym: 295; act: -42 ),
  ( sym: 308; act: -42 ),
  ( sym: 313; act: -42 ),
{ 42: }
  ( sym: 256; act: 79 ),
  ( sym: 262; act: 80 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 81 ),
  ( sym: 282; act: 82 ),
  ( sym: 283; act: 83 ),
  ( sym: 284; act: 84 ),
  ( sym: 308; act: 85 ),
  ( sym: 313; act: 86 ),
{ 43: }
{ 44: }
{ 45: }
{ 46: }
{ 47: }
{ 48: }
{ 49: }
{ 50: }
  ( sym: 297; act: 87 ),
  ( sym: 256; act: -47 ),
  ( sym: 261; act: -47 ),
  ( sym: 262; act: -47 ),
  ( sym: 263; act: -47 ),
  ( sym: 264; act: -47 ),
  ( sym: 271; act: -47 ),
  ( sym: 281; act: -47 ),
  ( sym: 282; act: -47 ),
  ( sym: 283; act: -47 ),
  ( sym: 284; act: -47 ),
  ( sym: 289; act: -47 ),
  ( sym: 290; act: -47 ),
  ( sym: 291; act: -47 ),
  ( sym: 292; act: -47 ),
  ( sym: 293; act: -47 ),
  ( sym: 294; act: -47 ),
  ( sym: 295; act: -47 ),
  ( sym: 308; act: -47 ),
  ( sym: 313; act: -47 ),
{ 51: }
{ 52: }
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
{ 53: }
  ( sym: 297; act: 92 ),
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
{ 54: }
{ 55: }
{ 56: }
  ( sym: 271; act: 23 ),
  ( sym: 267; act: -141 ),
{ 57: }
{ 58: }
  ( sym: 277; act: 97 ),
  ( sym: 256; act: -61 ),
  ( sym: 261; act: -61 ),
  ( sym: 262; act: -61 ),
  ( sym: 263; act: -61 ),
  ( sym: 264; act: -61 ),
  ( sym: 271; act: -61 ),
  ( sym: 281; act: -61 ),
  ( sym: 282; act: -61 ),
  ( sym: 283; act: -61 ),
  ( sym: 284; act: -61 ),
  ( sym: 289; act: -61 ),
  ( sym: 290; act: -61 ),
  ( sym: 291; act: -61 ),
  ( sym: 292; act: -61 ),
  ( sym: 293; act: -61 ),
  ( sym: 294; act: -61 ),
  ( sym: 295; act: -61 ),
  ( sym: 308; act: -61 ),
  ( sym: 313; act: -61 ),
{ 59: }
{ 60: }
{ 61: }
{ 62: }
{ 63: }
{ 64: }
{ 65: }
  ( sym: 256; act: 79 ),
  ( sym: 262; act: 80 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 81 ),
  ( sym: 282; act: 82 ),
  ( sym: 283; act: 83 ),
  ( sym: 284; act: 84 ),
  ( sym: 308; act: 85 ),
  ( sym: 313; act: 86 ),
{ 66: }
  ( sym: 289; act: 43 ),
  ( sym: 290; act: 44 ),
  ( sym: 291; act: 45 ),
  ( sym: 292; act: 46 ),
  ( sym: 293; act: 47 ),
  ( sym: 294; act: 48 ),
  ( sym: 295; act: 49 ),
  ( sym: 262; act: -16 ),
  ( sym: 271; act: -16 ),
  ( sym: 281; act: -16 ),
  ( sym: 282; act: -16 ),
  ( sym: 283; act: -16 ),
  ( sym: 284; act: -16 ),
  ( sym: 308; act: -16 ),
  ( sym: 313; act: -16 ),
{ 67: }
{ 68: }
  ( sym: 256; act: 51 ),
  ( sym: 266; act: 52 ),
  ( sym: 271; act: 23 ),
  ( sym: 262; act: -40 ),
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
{ 69: }
  ( sym: 271; act: 23 ),
  ( sym: 263; act: -141 ),
{ 70: }
{ 71: }
  ( sym: 262; act: 105 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 106 ),
  ( sym: 273; act: 107 ),
  ( sym: 286; act: 108 ),
  ( sym: 308; act: 109 ),
  ( sym: 310; act: 110 ),
  ( sym: 315; act: 111 ),
{ 72: }
  ( sym: 297; act: 112 ),
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
{ 73: }
  ( sym: 297; act: 113 ),
  ( sym: 256; act: -38 ),
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
{ 74: }
{ 75: }
  ( sym: 313; act: 114 ),
{ 76: }
  ( sym: 262; act: 116 ),
  ( sym: 264; act: 117 ),
  ( sym: 260; act: -73 ),
  ( sym: 261; act: -73 ),
  ( sym: 296; act: -73 ),
{ 77: }
  ( sym: 261; act: 119 ),
  ( sym: 296; act: 120 ),
  ( sym: 260; act: -18 ),
{ 78: }
  ( sym: 259; act: 122 ),
  ( sym: 260; act: -89 ),
  ( sym: 261; act: -89 ),
  ( sym: 262; act: -89 ),
  ( sym: 263; act: -89 ),
  ( sym: 264; act: -89 ),
  ( sym: 296; act: -89 ),
{ 79: }
{ 80: }
  ( sym: 262; act: 80 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 81 ),
  ( sym: 282; act: 82 ),
  ( sym: 283; act: 83 ),
  ( sym: 284; act: 84 ),
  ( sym: 308; act: 85 ),
  ( sym: 313; act: 86 ),
{ 81: }
  ( sym: 262; act: 80 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 81 ),
  ( sym: 282; act: 82 ),
  ( sym: 283; act: 83 ),
  ( sym: 284; act: 84 ),
  ( sym: 308; act: 85 ),
  ( sym: 313; act: 86 ),
{ 82: }
{ 83: }
{ 84: }
{ 85: }
  ( sym: 262; act: 80 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 81 ),
  ( sym: 282; act: 82 ),
  ( sym: 283; act: 83 ),
  ( sym: 284; act: 84 ),
  ( sym: 308; act: 85 ),
  ( sym: 313; act: 86 ),
{ 86: }
  ( sym: 262; act: 80 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 81 ),
  ( sym: 282; act: 82 ),
  ( sym: 283; act: 83 ),
  ( sym: 284; act: 84 ),
  ( sym: 308; act: 85 ),
  ( sym: 313; act: 86 ),
{ 87: }
{ 88: }
  ( sym: 267; act: 128 ),
{ 89: }
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
  ( sym: 267; act: -52 ),
{ 90: }
  ( sym: 267; act: 130 ),
{ 91: }
  ( sym: 256; act: 79 ),
  ( sym: 262; act: 80 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 81 ),
  ( sym: 282; act: 82 ),
  ( sym: 283; act: 83 ),
  ( sym: 284; act: 84 ),
  ( sym: 308; act: 85 ),
  ( sym: 313; act: 86 ),
{ 92: }
{ 93: }
  ( sym: 267; act: 132 ),
{ 94: }
  ( sym: 261; act: 133 ),
  ( sym: 263; act: -140 ),
  ( sym: 267; act: -140 ),
{ 95: }
  ( sym: 267; act: 134 ),
{ 96: }
  ( sym: 285; act: 135 ),
  ( sym: 261; act: -143 ),
  ( sym: 263; act: -143 ),
  ( sym: 267; act: -143 ),
{ 97: }
{ 98: }
  ( sym: 260; act: 136 ),
  ( sym: 261; act: 119 ),
{ 99: }
  ( sym: 262; act: 80 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 81 ),
  ( sym: 282; act: 82 ),
  ( sym: 283; act: 83 ),
  ( sym: 284; act: 84 ),
  ( sym: 308; act: 85 ),
  ( sym: 313; act: 86 ),
{ 100: }
  ( sym: 260; act: 138 ),
{ 101: }
  ( sym: 263; act: 139 ),
{ 102: }
  ( sym: 318; act: 140 ),
  ( sym: 319; act: 141 ),
  ( sym: 286; act: -144 ),
{ 103: }
  ( sym: 286; act: 142 ),
{ 104: }
  ( sym: 262; act: 143 ),
  ( sym: 259; act: -125 ),
  ( sym: 260; act: -125 ),
  ( sym: 261; act: -125 ),
  ( sym: 263; act: -125 ),
  ( sym: 264; act: -125 ),
  ( sym: 265; act: -125 ),
  ( sym: 267; act: -125 ),
  ( sym: 286; act: -125 ),
  ( sym: 296; act: -125 ),
  ( sym: 300; act: -125 ),
  ( sym: 301; act: -125 ),
  ( sym: 302; act: -125 ),
  ( sym: 303; act: -125 ),
  ( sym: 304; act: -125 ),
  ( sym: 305; act: -125 ),
  ( sym: 306; act: -125 ),
  ( sym: 307; act: -125 ),
  ( sym: 308; act: -125 ),
  ( sym: 309; act: -125 ),
  ( sym: 310; act: -125 ),
  ( sym: 311; act: -125 ),
  ( sym: 312; act: -125 ),
  ( sym: 313; act: -125 ),
  ( sym: 314; act: -125 ),
  ( sym: 315; act: -125 ),
  ( sym: 318; act: -125 ),
  ( sym: 319; act: -125 ),
{ 105: }
  ( sym: 262; act: 105 ),
  ( sym: 268; act: 20 ),
  ( sym: 269; act: 21 ),
  ( sym: 270; act: 22 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 106 ),
  ( sym: 273; act: 107 ),
  ( sym: 274; act: 24 ),
  ( sym: 275; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 277; act: 27 ),
  ( sym: 278; act: 28 ),
  ( sym: 279; act: 29 ),
  ( sym: 280; act: 30 ),
  ( sym: 281; act: 31 ),
  ( sym: 308; act: 109 ),
  ( sym: 310; act: 110 ),
  ( sym: 315; act: 111 ),
{ 106: }
{ 107: }
{ 108: }
{ 109: }
  ( sym: 262; act: 105 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 106 ),
  ( sym: 273; act: 107 ),
  ( sym: 308; act: 109 ),
  ( sym: 310; act: 110 ),
  ( sym: 315; act: 111 ),
{ 110: }
  ( sym: 262; act: 105 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 106 ),
  ( sym: 273; act: 107 ),
  ( sym: 308; act: 109 ),
  ( sym: 310; act: 110 ),
  ( sym: 315; act: 111 ),
{ 111: }
  ( sym: 262; act: 105 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 106 ),
  ( sym: 273; act: 107 ),
  ( sym: 308; act: 109 ),
  ( sym: 310; act: 110 ),
  ( sym: 315; act: 111 ),
{ 112: }
{ 113: }
{ 114: }
  ( sym: 262; act: 80 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 81 ),
  ( sym: 282; act: 82 ),
  ( sym: 283; act: 83 ),
  ( sym: 284; act: 84 ),
  ( sym: 308; act: 85 ),
  ( sym: 313; act: 86 ),
{ 115: }
{ 116: }
  ( sym: 263; act: 156 ),
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
  ( sym: 280; act: 157 ),
  ( sym: 281; act: 31 ),
  ( sym: 298; act: 158 ),
{ 117: }
  ( sym: 262; act: 105 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 106 ),
  ( sym: 273; act: 107 ),
  ( sym: 308; act: 109 ),
  ( sym: 310; act: 110 ),
  ( sym: 315; act: 111 ),
{ 118: }
  ( sym: 260; act: 161 ),
{ 119: }
  ( sym: 262; act: 80 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 81 ),
  ( sym: 282; act: 82 ),
  ( sym: 283; act: 83 ),
  ( sym: 284; act: 84 ),
  ( sym: 308; act: 85 ),
  ( sym: 313; act: 86 ),
{ 120: }
  ( sym: 262; act: 163 ),
{ 121: }
  ( sym: 262; act: 105 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 106 ),
  ( sym: 273; act: 107 ),
  ( sym: 308; act: 109 ),
  ( sym: 310; act: 110 ),
  ( sym: 315; act: 111 ),
{ 122: }
  ( sym: 262; act: 105 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 106 ),
  ( sym: 273; act: 107 ),
  ( sym: 308; act: 109 ),
  ( sym: 310; act: 110 ),
  ( sym: 315; act: 111 ),
{ 123: }
  ( sym: 261; act: 166 ),
  ( sym: 260; act: -72 ),
  ( sym: 296; act: -72 ),
{ 124: }
  ( sym: 262; act: 116 ),
  ( sym: 263; act: 167 ),
  ( sym: 264; act: 117 ),
{ 125: }
  ( sym: 262; act: 116 ),
  ( sym: 264; act: 117 ),
  ( sym: 260; act: -83 ),
  ( sym: 261; act: -83 ),
  ( sym: 263; act: -83 ),
  ( sym: 296; act: -83 ),
{ 126: }
  ( sym: 264; act: 117 ),
  ( sym: 260; act: -86 ),
  ( sym: 261; act: -86 ),
  ( sym: 262; act: -86 ),
  ( sym: 263; act: -86 ),
  ( sym: 296; act: -86 ),
{ 127: }
  ( sym: 262; act: 116 ),
  ( sym: 264; act: 117 ),
  ( sym: 260; act: -85 ),
  ( sym: 261; act: -85 ),
  ( sym: 263; act: -85 ),
  ( sym: 296; act: -85 ),
{ 128: }
{ 129: }
{ 130: }
{ 131: }
  ( sym: 260; act: 168 ),
  ( sym: 261; act: 119 ),
{ 132: }
{ 133: }
  ( sym: 271; act: 23 ),
  ( sym: 263; act: -141 ),
  ( sym: 267; act: -141 ),
{ 134: }
{ 135: }
  ( sym: 262; act: 105 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 106 ),
  ( sym: 273; act: 107 ),
  ( sym: 308; act: 109 ),
  ( sym: 310; act: 110 ),
  ( sym: 315; act: 111 ),
{ 136: }
{ 137: }
  ( sym: 262; act: 116 ),
  ( sym: 263; act: 171 ),
  ( sym: 264; act: 117 ),
{ 138: }
{ 139: }
  ( sym: 287; act: 172 ),
{ 140: }
  ( sym: 262; act: 105 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 106 ),
  ( sym: 273; act: 107 ),
  ( sym: 308; act: 109 ),
  ( sym: 310; act: 110 ),
  ( sym: 315; act: 111 ),
{ 141: }
  ( sym: 262; act: 105 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 106 ),
  ( sym: 273; act: 107 ),
  ( sym: 308; act: 109 ),
  ( sym: 310; act: 110 ),
  ( sym: 315; act: 111 ),
{ 142: }
{ 143: }
  ( sym: 262; act: 105 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 106 ),
  ( sym: 273; act: 107 ),
  ( sym: 308; act: 109 ),
  ( sym: 310; act: 110 ),
  ( sym: 315; act: 111 ),
  ( sym: 263; act: -147 ),
{ 144: }
  ( sym: 318; act: 140 ),
  ( sym: 319; act: 141 ),
  ( sym: 259; act: -121 ),
  ( sym: 260; act: -121 ),
  ( sym: 261; act: -121 ),
  ( sym: 262; act: -121 ),
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
  ( sym: 308; act: -121 ),
  ( sym: 309; act: -121 ),
  ( sym: 310; act: -121 ),
  ( sym: 311; act: -121 ),
  ( sym: 312; act: -121 ),
  ( sym: 313; act: -121 ),
  ( sym: 314; act: -121 ),
  ( sym: 315; act: -121 ),
{ 145: }
  ( sym: 263; act: 178 ),
  ( sym: 300; act: -104 ),
  ( sym: 301; act: -104 ),
  ( sym: 302; act: -104 ),
  ( sym: 303; act: -104 ),
  ( sym: 304; act: -104 ),
  ( sym: 305; act: -104 ),
  ( sym: 306; act: -104 ),
  ( sym: 307; act: -104 ),
  ( sym: 308; act: -104 ),
  ( sym: 309; act: -104 ),
  ( sym: 310; act: -104 ),
  ( sym: 311; act: -104 ),
  ( sym: 312; act: -104 ),
  ( sym: 313; act: -104 ),
  ( sym: 314; act: -104 ),
  ( sym: 315; act: -104 ),
{ 146: }
  ( sym: 300; act: 179 ),
  ( sym: 301; act: 180 ),
  ( sym: 302; act: 181 ),
  ( sym: 303; act: 182 ),
  ( sym: 304; act: 183 ),
  ( sym: 305; act: 184 ),
  ( sym: 306; act: 185 ),
  ( sym: 307; act: 186 ),
  ( sym: 308; act: 187 ),
  ( sym: 309; act: 188 ),
  ( sym: 310; act: 189 ),
  ( sym: 311; act: 190 ),
  ( sym: 312; act: 191 ),
  ( sym: 313; act: 192 ),
  ( sym: 314; act: 193 ),
  ( sym: 315; act: 194 ),
{ 147: }
  ( sym: 263; act: 196 ),
  ( sym: 282; act: 82 ),
  ( sym: 283; act: 83 ),
  ( sym: 284; act: 84 ),
  ( sym: 313; act: 197 ),
{ 148: }
  ( sym: 262; act: 143 ),
  ( sym: 263; act: 198 ),
  ( sym: 282; act: -69 ),
  ( sym: 283; act: -69 ),
  ( sym: 284; act: -69 ),
  ( sym: 313; act: -69 ),
  ( sym: 300; act: -125 ),
  ( sym: 301; act: -125 ),
  ( sym: 302; act: -125 ),
  ( sym: 303; act: -125 ),
  ( sym: 304; act: -125 ),
  ( sym: 305; act: -125 ),
  ( sym: 306; act: -125 ),
  ( sym: 307; act: -125 ),
  ( sym: 308; act: -125 ),
  ( sym: 309; act: -125 ),
  ( sym: 310; act: -125 ),
  ( sym: 311; act: -125 ),
  ( sym: 312; act: -125 ),
  ( sym: 314; act: -125 ),
  ( sym: 315; act: -125 ),
  ( sym: 318; act: -125 ),
  ( sym: 319; act: -125 ),
{ 149: }
  ( sym: 318; act: 140 ),
  ( sym: 319; act: 141 ),
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
{ 150: }
  ( sym: 318; act: 140 ),
  ( sym: 319; act: 141 ),
  ( sym: 259; act: -130 ),
  ( sym: 260; act: -130 ),
  ( sym: 261; act: -130 ),
  ( sym: 262; act: -130 ),
  ( sym: 263; act: -130 ),
  ( sym: 264; act: -130 ),
  ( sym: 265; act: -130 ),
  ( sym: 267; act: -130 ),
  ( sym: 286; act: -130 ),
  ( sym: 296; act: -130 ),
  ( sym: 300; act: -130 ),
  ( sym: 301; act: -130 ),
  ( sym: 302; act: -130 ),
  ( sym: 303; act: -130 ),
  ( sym: 304; act: -130 ),
  ( sym: 305; act: -130 ),
  ( sym: 306; act: -130 ),
  ( sym: 307; act: -130 ),
  ( sym: 308; act: -130 ),
  ( sym: 309; act: -130 ),
  ( sym: 310; act: -130 ),
  ( sym: 311; act: -130 ),
  ( sym: 312; act: -130 ),
  ( sym: 313; act: -130 ),
  ( sym: 314; act: -130 ),
  ( sym: 315; act: -130 ),
{ 151: }
  ( sym: 318; act: 140 ),
  ( sym: 319; act: 141 ),
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
{ 152: }
  ( sym: 262; act: 116 ),
  ( sym: 264; act: 117 ),
  ( sym: 260; act: -84 ),
  ( sym: 261; act: -84 ),
  ( sym: 263; act: -84 ),
  ( sym: 296; act: -84 ),
{ 153: }
  ( sym: 261; act: 199 ),
  ( sym: 263; act: -77 ),
{ 154: }
  ( sym: 263; act: 200 ),
{ 155: }
  ( sym: 262; act: 204 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 205 ),
  ( sym: 282; act: 82 ),
  ( sym: 283; act: 83 ),
  ( sym: 284; act: 84 ),
  ( sym: 308; act: 85 ),
  ( sym: 313; act: 206 ),
  ( sym: 261; act: -103 ),
  ( sym: 263; act: -103 ),
  ( sym: 264; act: -103 ),
{ 156: }
{ 157: }
  ( sym: 263; act: 207 ),
  ( sym: 261; act: -65 ),
  ( sym: 262; act: -65 ),
  ( sym: 264; act: -65 ),
  ( sym: 271; act: -65 ),
  ( sym: 281; act: -65 ),
  ( sym: 282; act: -65 ),
  ( sym: 283; act: -65 ),
  ( sym: 284; act: -65 ),
  ( sym: 308; act: -65 ),
  ( sym: 313; act: -65 ),
{ 158: }
{ 159: }
{ 160: }
  ( sym: 265; act: 208 ),
  ( sym: 300; act: 179 ),
  ( sym: 301; act: 180 ),
  ( sym: 302; act: 181 ),
  ( sym: 303; act: 182 ),
  ( sym: 304; act: 183 ),
  ( sym: 305; act: 184 ),
  ( sym: 306; act: 185 ),
  ( sym: 307; act: 186 ),
  ( sym: 308; act: 187 ),
  ( sym: 309; act: 188 ),
  ( sym: 310; act: 189 ),
  ( sym: 311; act: 190 ),
  ( sym: 312; act: 191 ),
  ( sym: 313; act: 192 ),
  ( sym: 314; act: 193 ),
  ( sym: 315; act: 194 ),
{ 161: }
{ 162: }
  ( sym: 262; act: 116 ),
  ( sym: 264; act: 117 ),
  ( sym: 260; act: -70 ),
  ( sym: 261; act: -70 ),
  ( sym: 296; act: -70 ),
{ 163: }
  ( sym: 271; act: 23 ),
{ 164: }
  ( sym: 300; act: 179 ),
  ( sym: 301; act: 180 ),
  ( sym: 302; act: 181 ),
  ( sym: 303; act: 182 ),
  ( sym: 304; act: 183 ),
  ( sym: 305; act: 184 ),
  ( sym: 306; act: 185 ),
  ( sym: 307; act: 186 ),
  ( sym: 308; act: 187 ),
  ( sym: 309; act: 188 ),
  ( sym: 310; act: 189 ),
  ( sym: 311; act: 190 ),
  ( sym: 312; act: 191 ),
  ( sym: 313; act: 192 ),
  ( sym: 314; act: 193 ),
  ( sym: 315; act: 194 ),
  ( sym: 260; act: -88 ),
  ( sym: 261; act: -88 ),
  ( sym: 262; act: -88 ),
  ( sym: 263; act: -88 ),
  ( sym: 264; act: -88 ),
  ( sym: 296; act: -88 ),
{ 165: }
  ( sym: 300; act: 179 ),
  ( sym: 301; act: 180 ),
  ( sym: 302; act: 181 ),
  ( sym: 303; act: 182 ),
  ( sym: 304; act: 183 ),
  ( sym: 305; act: 184 ),
  ( sym: 306; act: 185 ),
  ( sym: 307; act: 186 ),
  ( sym: 308; act: 187 ),
  ( sym: 309; act: 188 ),
  ( sym: 310; act: 189 ),
  ( sym: 311; act: 190 ),
  ( sym: 312; act: 191 ),
  ( sym: 313; act: 192 ),
  ( sym: 314; act: 193 ),
  ( sym: 315; act: 194 ),
  ( sym: 260; act: -87 ),
  ( sym: 261; act: -87 ),
  ( sym: 262; act: -87 ),
  ( sym: 263; act: -87 ),
  ( sym: 264; act: -87 ),
  ( sym: 296; act: -87 ),
{ 166: }
  ( sym: 256; act: 79 ),
  ( sym: 262; act: 80 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 81 ),
  ( sym: 282; act: 82 ),
  ( sym: 283; act: 83 ),
  ( sym: 284; act: 84 ),
  ( sym: 308; act: 85 ),
  ( sym: 313; act: 86 ),
{ 167: }
{ 168: }
{ 169: }
{ 170: }
  ( sym: 300; act: 179 ),
  ( sym: 301; act: 180 ),
  ( sym: 302; act: 181 ),
  ( sym: 303; act: 182 ),
  ( sym: 304; act: 183 ),
  ( sym: 305; act: 184 ),
  ( sym: 306; act: 185 ),
  ( sym: 307; act: 186 ),
  ( sym: 308; act: 187 ),
  ( sym: 309; act: 188 ),
  ( sym: 310; act: 189 ),
  ( sym: 311; act: 190 ),
  ( sym: 312; act: 191 ),
  ( sym: 313; act: 192 ),
  ( sym: 314; act: 193 ),
  ( sym: 315; act: 194 ),
  ( sym: 261; act: -142 ),
  ( sym: 263; act: -142 ),
  ( sym: 267; act: -142 ),
{ 171: }
  ( sym: 262; act: 211 ),
{ 172: }
  ( sym: 262; act: 105 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 106 ),
  ( sym: 273; act: 107 ),
  ( sym: 308; act: 109 ),
  ( sym: 310; act: 110 ),
  ( sym: 315; act: 111 ),
{ 173: }
{ 174: }
{ 175: }
  ( sym: 261; act: 213 ),
  ( sym: 263; act: -146 ),
{ 176: }
  ( sym: 263; act: 214 ),
{ 177: }
  ( sym: 300; act: 179 ),
  ( sym: 301; act: 180 ),
  ( sym: 302; act: 181 ),
  ( sym: 303; act: 182 ),
  ( sym: 304; act: 183 ),
  ( sym: 305; act: 184 ),
  ( sym: 306; act: 185 ),
  ( sym: 307; act: 186 ),
  ( sym: 308; act: 187 ),
  ( sym: 309; act: 188 ),
  ( sym: 310; act: 189 ),
  ( sym: 311; act: 190 ),
  ( sym: 312; act: 191 ),
  ( sym: 313; act: 192 ),
  ( sym: 314; act: 193 ),
  ( sym: 315; act: 194 ),
  ( sym: 261; act: -148 ),
  ( sym: 263; act: -148 ),
{ 178: }
{ 179: }
  ( sym: 262; act: 105 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 106 ),
  ( sym: 273; act: 107 ),
  ( sym: 308; act: 109 ),
  ( sym: 310; act: 110 ),
  ( sym: 315; act: 111 ),
{ 180: }
  ( sym: 262; act: 105 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 106 ),
  ( sym: 273; act: 107 ),
  ( sym: 308; act: 109 ),
  ( sym: 310; act: 110 ),
  ( sym: 315; act: 111 ),
{ 181: }
  ( sym: 262; act: 105 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 106 ),
  ( sym: 273; act: 107 ),
  ( sym: 308; act: 109 ),
  ( sym: 310; act: 110 ),
  ( sym: 315; act: 111 ),
{ 182: }
  ( sym: 262; act: 105 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 106 ),
  ( sym: 273; act: 107 ),
  ( sym: 308; act: 109 ),
  ( sym: 310; act: 110 ),
  ( sym: 315; act: 111 ),
{ 183: }
  ( sym: 262; act: 105 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 106 ),
  ( sym: 273; act: 107 ),
  ( sym: 308; act: 109 ),
  ( sym: 310; act: 110 ),
  ( sym: 315; act: 111 ),
{ 184: }
  ( sym: 262; act: 105 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 106 ),
  ( sym: 273; act: 107 ),
  ( sym: 308; act: 109 ),
  ( sym: 310; act: 110 ),
  ( sym: 315; act: 111 ),
{ 185: }
  ( sym: 262; act: 105 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 106 ),
  ( sym: 273; act: 107 ),
  ( sym: 308; act: 109 ),
  ( sym: 310; act: 110 ),
  ( sym: 315; act: 111 ),
{ 186: }
  ( sym: 262; act: 105 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 106 ),
  ( sym: 273; act: 107 ),
  ( sym: 308; act: 109 ),
  ( sym: 310; act: 110 ),
  ( sym: 315; act: 111 ),
{ 187: }
  ( sym: 262; act: 105 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 106 ),
  ( sym: 273; act: 107 ),
  ( sym: 308; act: 109 ),
  ( sym: 310; act: 110 ),
  ( sym: 315; act: 111 ),
{ 188: }
  ( sym: 262; act: 105 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 106 ),
  ( sym: 273; act: 107 ),
  ( sym: 308; act: 109 ),
  ( sym: 310; act: 110 ),
  ( sym: 315; act: 111 ),
{ 189: }
  ( sym: 262; act: 105 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 106 ),
  ( sym: 273; act: 107 ),
  ( sym: 308; act: 109 ),
  ( sym: 310; act: 110 ),
  ( sym: 315; act: 111 ),
{ 190: }
  ( sym: 262; act: 105 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 106 ),
  ( sym: 273; act: 107 ),
  ( sym: 308; act: 109 ),
  ( sym: 310; act: 110 ),
  ( sym: 315; act: 111 ),
{ 191: }
  ( sym: 262; act: 105 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 106 ),
  ( sym: 273; act: 107 ),
  ( sym: 308; act: 109 ),
  ( sym: 310; act: 110 ),
  ( sym: 315; act: 111 ),
{ 192: }
  ( sym: 262; act: 105 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 106 ),
  ( sym: 273; act: 107 ),
  ( sym: 308; act: 109 ),
  ( sym: 310; act: 110 ),
  ( sym: 315; act: 111 ),
{ 193: }
  ( sym: 262; act: 105 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 106 ),
  ( sym: 273; act: 107 ),
  ( sym: 308; act: 109 ),
  ( sym: 310; act: 110 ),
  ( sym: 315; act: 111 ),
{ 194: }
  ( sym: 262; act: 105 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 106 ),
  ( sym: 273; act: 107 ),
  ( sym: 308; act: 109 ),
  ( sym: 310; act: 110 ),
  ( sym: 315; act: 111 ),
{ 195: }
  ( sym: 313; act: 232 ),
{ 196: }
  ( sym: 262; act: 105 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 106 ),
  ( sym: 273; act: 107 ),
  ( sym: 308; act: 109 ),
  ( sym: 310; act: 110 ),
  ( sym: 315; act: 111 ),
{ 197: }
  ( sym: 263; act: 234 ),
{ 198: }
  ( sym: 262; act: 105 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 106 ),
  ( sym: 273; act: 107 ),
  ( sym: 308; act: 109 ),
  ( sym: 310; act: 110 ),
  ( sym: 315; act: 111 ),
  ( sym: 259; act: -124 ),
  ( sym: 260; act: -124 ),
  ( sym: 261; act: -124 ),
  ( sym: 263; act: -124 ),
  ( sym: 264; act: -124 ),
  ( sym: 265; act: -124 ),
  ( sym: 267; act: -124 ),
  ( sym: 286; act: -124 ),
  ( sym: 296; act: -124 ),
  ( sym: 300; act: -124 ),
  ( sym: 301; act: -124 ),
  ( sym: 302; act: -124 ),
  ( sym: 303; act: -124 ),
  ( sym: 304; act: -124 ),
  ( sym: 305; act: -124 ),
  ( sym: 306; act: -124 ),
  ( sym: 307; act: -124 ),
  ( sym: 309; act: -124 ),
  ( sym: 311; act: -124 ),
  ( sym: 312; act: -124 ),
  ( sym: 313; act: -124 ),
  ( sym: 314; act: -124 ),
  ( sym: 318; act: -124 ),
  ( sym: 319; act: -124 ),
{ 199: }
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
  ( sym: 298; act: 158 ),
{ 200: }
{ 201: }
  ( sym: 313; act: 238 ),
{ 202: }
  ( sym: 262; act: 240 ),
  ( sym: 264; act: 241 ),
  ( sym: 261; act: -76 ),
  ( sym: 263; act: -76 ),
{ 203: }
  ( sym: 262; act: 116 ),
  ( sym: 264; act: 117 ),
  ( sym: 261; act: -74 ),
  ( sym: 263; act: -74 ),
{ 204: }
  ( sym: 262; act: 204 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 205 ),
  ( sym: 282; act: 82 ),
  ( sym: 283; act: 83 ),
  ( sym: 284; act: 84 ),
  ( sym: 308; act: 85 ),
  ( sym: 313; act: 243 ),
  ( sym: 263; act: -103 ),
  ( sym: 264; act: -103 ),
{ 205: }
  ( sym: 262; act: 204 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 205 ),
  ( sym: 282; act: 82 ),
  ( sym: 283; act: 83 ),
  ( sym: 284; act: 84 ),
  ( sym: 308; act: 85 ),
  ( sym: 313; act: 243 ),
  ( sym: 261; act: -103 ),
  ( sym: 263; act: -103 ),
  ( sym: 264; act: -103 ),
{ 206: }
  ( sym: 262; act: 204 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 205 ),
  ( sym: 282; act: 82 ),
  ( sym: 283; act: 83 ),
  ( sym: 284; act: 84 ),
  ( sym: 308; act: 85 ),
  ( sym: 313; act: 243 ),
  ( sym: 261; act: -103 ),
  ( sym: 263; act: -103 ),
  ( sym: 264; act: -103 ),
{ 207: }
{ 208: }
{ 209: }
  ( sym: 263; act: 247 ),
{ 210: }
{ 211: }
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
  ( sym: 298; act: 158 ),
{ 212: }
  ( sym: 286; act: 249 ),
{ 213: }
  ( sym: 262; act: 105 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 106 ),
  ( sym: 273; act: 107 ),
  ( sym: 308; act: 109 ),
  ( sym: 310; act: 110 ),
  ( sym: 315; act: 111 ),
  ( sym: 263; act: -147 ),
{ 214: }
{ 215: }
  ( sym: 306; act: 185 ),
  ( sym: 307; act: 186 ),
  ( sym: 308; act: 187 ),
  ( sym: 309; act: 188 ),
  ( sym: 310; act: 189 ),
  ( sym: 311; act: 190 ),
  ( sym: 312; act: 191 ),
  ( sym: 313; act: 192 ),
  ( sym: 314; act: 193 ),
  ( sym: 315; act: 194 ),
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
{ 216: }
  ( sym: 306; act: 185 ),
  ( sym: 307; act: 186 ),
  ( sym: 308; act: 187 ),
  ( sym: 309; act: 188 ),
  ( sym: 310; act: 189 ),
  ( sym: 311; act: 190 ),
  ( sym: 312; act: 191 ),
  ( sym: 313; act: 192 ),
  ( sym: 314; act: 193 ),
  ( sym: 315; act: 194 ),
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
{ 217: }
  ( sym: 306; act: 185 ),
  ( sym: 307; act: 186 ),
  ( sym: 308; act: 187 ),
  ( sym: 309; act: 188 ),
  ( sym: 310; act: 189 ),
  ( sym: 311; act: 190 ),
  ( sym: 312; act: 191 ),
  ( sym: 313; act: 192 ),
  ( sym: 314; act: 193 ),
  ( sym: 315; act: 194 ),
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
{ 218: }
  ( sym: 306; act: 185 ),
  ( sym: 307; act: 186 ),
  ( sym: 308; act: 187 ),
  ( sym: 309; act: 188 ),
  ( sym: 310; act: 189 ),
  ( sym: 311; act: 190 ),
  ( sym: 312; act: 191 ),
  ( sym: 313; act: 192 ),
  ( sym: 314; act: 193 ),
  ( sym: 315; act: 194 ),
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
  ( sym: 318; act: -109 ),
  ( sym: 319; act: -109 ),
{ 219: }
  ( sym: 306; act: 185 ),
  ( sym: 307; act: 186 ),
  ( sym: 308; act: 187 ),
  ( sym: 309; act: 188 ),
  ( sym: 310; act: 189 ),
  ( sym: 311; act: 190 ),
  ( sym: 312; act: 191 ),
  ( sym: 313; act: 192 ),
  ( sym: 314; act: 193 ),
  ( sym: 315; act: 194 ),
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
  ( sym: 318; act: -108 ),
  ( sym: 319; act: -108 ),
{ 220: }
  ( sym: 306; act: 185 ),
  ( sym: 307; act: 186 ),
  ( sym: 308; act: 187 ),
  ( sym: 309; act: 188 ),
  ( sym: 310; act: 189 ),
  ( sym: 311; act: 190 ),
  ( sym: 312; act: 191 ),
  ( sym: 313; act: 192 ),
  ( sym: 314; act: 193 ),
  ( sym: 315; act: 194 ),
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
  ( sym: 318; act: -110 ),
  ( sym: 319; act: -110 ),
{ 221: }
{ 222: }
  ( sym: 259; act: 251 ),
  ( sym: 300; act: 179 ),
  ( sym: 301; act: 180 ),
  ( sym: 302; act: 181 ),
  ( sym: 303; act: 182 ),
  ( sym: 304; act: 183 ),
  ( sym: 305; act: 184 ),
  ( sym: 306; act: 185 ),
  ( sym: 307; act: 186 ),
  ( sym: 308; act: 187 ),
  ( sym: 309; act: 188 ),
  ( sym: 310; act: 189 ),
  ( sym: 311; act: 190 ),
  ( sym: 312; act: 191 ),
  ( sym: 313; act: 192 ),
  ( sym: 314; act: 193 ),
  ( sym: 315; act: 194 ),
{ 223: }
  ( sym: 308; act: 187 ),
  ( sym: 309; act: 188 ),
  ( sym: 310; act: 189 ),
  ( sym: 311; act: 190 ),
  ( sym: 312; act: 191 ),
  ( sym: 313; act: 192 ),
  ( sym: 314; act: 193 ),
  ( sym: 315; act: 194 ),
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
  ( sym: 318; act: -115 ),
  ( sym: 319; act: -115 ),
{ 224: }
  ( sym: 309; act: 188 ),
  ( sym: 310; act: 189 ),
  ( sym: 311; act: 190 ),
  ( sym: 312; act: 191 ),
  ( sym: 313; act: 192 ),
  ( sym: 314; act: 193 ),
  ( sym: 315; act: 194 ),
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
  ( sym: 318; act: -116 ),
  ( sym: 319; act: -116 ),
{ 225: }
  ( sym: 311; act: 190 ),
  ( sym: 312; act: 191 ),
  ( sym: 313; act: 192 ),
  ( sym: 314; act: 193 ),
  ( sym: 315; act: 194 ),
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
  ( sym: 318; act: -111 ),
  ( sym: 319; act: -111 ),
{ 226: }
  ( sym: 311; act: 190 ),
  ( sym: 312; act: 191 ),
  ( sym: 313; act: 192 ),
  ( sym: 314; act: 193 ),
  ( sym: 315; act: 194 ),
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
  ( sym: 308; act: -112 ),
  ( sym: 309; act: -112 ),
  ( sym: 310; act: -112 ),
  ( sym: 318; act: -112 ),
  ( sym: 319; act: -112 ),
{ 227: }
  ( sym: 313; act: 192 ),
  ( sym: 314; act: 193 ),
  ( sym: 315; act: 194 ),
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
  ( sym: 307; act: -119 ),
  ( sym: 308; act: -119 ),
  ( sym: 309; act: -119 ),
  ( sym: 310; act: -119 ),
  ( sym: 311; act: -119 ),
  ( sym: 312; act: -119 ),
  ( sym: 318; act: -119 ),
  ( sym: 319; act: -119 ),
{ 228: }
  ( sym: 313; act: 192 ),
  ( sym: 314; act: 193 ),
  ( sym: 315; act: 194 ),
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
  ( sym: 318; act: -118 ),
  ( sym: 319; act: -118 ),
{ 229: }
  ( sym: 315; act: 194 ),
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
  ( sym: 309; act: -113 ),
  ( sym: 310; act: -113 ),
  ( sym: 311; act: -113 ),
  ( sym: 312; act: -113 ),
  ( sym: 313; act: -113 ),
  ( sym: 314; act: -113 ),
  ( sym: 318; act: -113 ),
  ( sym: 319; act: -113 ),
{ 230: }
  ( sym: 315; act: 194 ),
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
{ 231: }
  ( sym: 315; act: 194 ),
  ( sym: 259; act: -117 ),
  ( sym: 260; act: -117 ),
  ( sym: 261; act: -117 ),
  ( sym: 262; act: -117 ),
  ( sym: 263; act: -117 ),
  ( sym: 264; act: -117 ),
  ( sym: 265; act: -117 ),
  ( sym: 267; act: -117 ),
  ( sym: 286; act: -117 ),
  ( sym: 296; act: -117 ),
  ( sym: 300; act: -117 ),
  ( sym: 301; act: -117 ),
  ( sym: 302; act: -117 ),
  ( sym: 303; act: -117 ),
  ( sym: 304; act: -117 ),
  ( sym: 305; act: -117 ),
  ( sym: 306; act: -117 ),
  ( sym: 307; act: -117 ),
  ( sym: 308; act: -117 ),
  ( sym: 309; act: -117 ),
  ( sym: 310; act: -117 ),
  ( sym: 311; act: -117 ),
  ( sym: 312; act: -117 ),
  ( sym: 313; act: -117 ),
  ( sym: 314; act: -117 ),
  ( sym: 318; act: -117 ),
  ( sym: 319; act: -117 ),
{ 232: }
  ( sym: 263; act: 252 ),
{ 233: }
  ( sym: 318; act: 140 ),
  ( sym: 319; act: 141 ),
  ( sym: 259; act: -134 ),
  ( sym: 260; act: -134 ),
  ( sym: 261; act: -134 ),
  ( sym: 262; act: -134 ),
  ( sym: 263; act: -134 ),
  ( sym: 264; act: -134 ),
  ( sym: 265; act: -134 ),
  ( sym: 267; act: -134 ),
  ( sym: 286; act: -134 ),
  ( sym: 296; act: -134 ),
  ( sym: 300; act: -134 ),
  ( sym: 301; act: -134 ),
  ( sym: 302; act: -134 ),
  ( sym: 303; act: -134 ),
  ( sym: 304; act: -134 ),
  ( sym: 305; act: -134 ),
  ( sym: 306; act: -134 ),
  ( sym: 307; act: -134 ),
  ( sym: 308; act: -134 ),
  ( sym: 309; act: -134 ),
  ( sym: 310; act: -134 ),
  ( sym: 311; act: -134 ),
  ( sym: 312; act: -134 ),
  ( sym: 313; act: -134 ),
  ( sym: 314; act: -134 ),
  ( sym: 315; act: -134 ),
{ 234: }
  ( sym: 262; act: 105 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 106 ),
  ( sym: 273; act: 107 ),
  ( sym: 308; act: 109 ),
  ( sym: 310; act: 110 ),
  ( sym: 315; act: 111 ),
{ 235: }
{ 236: }
  ( sym: 318; act: 140 ),
  ( sym: 319; act: 141 ),
  ( sym: 259; act: -123 ),
  ( sym: 260; act: -123 ),
  ( sym: 261; act: -123 ),
  ( sym: 262; act: -123 ),
  ( sym: 263; act: -123 ),
  ( sym: 264; act: -123 ),
  ( sym: 265; act: -123 ),
  ( sym: 267; act: -123 ),
  ( sym: 286; act: -123 ),
  ( sym: 296; act: -123 ),
  ( sym: 300; act: -123 ),
  ( sym: 301; act: -123 ),
  ( sym: 302; act: -123 ),
  ( sym: 303; act: -123 ),
  ( sym: 304; act: -123 ),
  ( sym: 305; act: -123 ),
  ( sym: 306; act: -123 ),
  ( sym: 307; act: -123 ),
  ( sym: 308; act: -123 ),
  ( sym: 309; act: -123 ),
  ( sym: 310; act: -123 ),
  ( sym: 311; act: -123 ),
  ( sym: 312; act: -123 ),
  ( sym: 313; act: -123 ),
  ( sym: 314; act: -123 ),
  ( sym: 315; act: -123 ),
{ 237: }
{ 238: }
  ( sym: 262; act: 204 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 205 ),
  ( sym: 282; act: 82 ),
  ( sym: 283; act: 83 ),
  ( sym: 284; act: 84 ),
  ( sym: 308; act: 85 ),
  ( sym: 313; act: 243 ),
  ( sym: 261; act: -103 ),
  ( sym: 263; act: -103 ),
  ( sym: 264; act: -103 ),
{ 239: }
{ 240: }
  ( sym: 263; act: 156 ),
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
  ( sym: 280; act: 157 ),
  ( sym: 281; act: 31 ),
  ( sym: 298; act: 158 ),
{ 241: }
  ( sym: 262; act: 105 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 106 ),
  ( sym: 273; act: 107 ),
  ( sym: 308; act: 109 ),
  ( sym: 310; act: 110 ),
  ( sym: 315; act: 111 ),
{ 242: }
  ( sym: 262; act: 240 ),
  ( sym: 263; act: 257 ),
  ( sym: 264; act: 241 ),
{ 243: }
  ( sym: 262; act: 204 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 205 ),
  ( sym: 282; act: 82 ),
  ( sym: 283; act: 83 ),
  ( sym: 284; act: 84 ),
  ( sym: 308; act: 85 ),
  ( sym: 313; act: 243 ),
  ( sym: 261; act: -103 ),
  ( sym: 263; act: -103 ),
  ( sym: 264; act: -103 ),
{ 244: }
  ( sym: 262; act: 240 ),
  ( sym: 264; act: 241 ),
  ( sym: 261; act: -96 ),
  ( sym: 263; act: -96 ),
{ 245: }
  ( sym: 264; act: 241 ),
  ( sym: 261; act: -98 ),
  ( sym: 262; act: -98 ),
  ( sym: 263; act: -98 ),
{ 246: }
  ( sym: 262; act: 116 ),
  ( sym: 264; act: 117 ),
  ( sym: 261; act: -75 ),
  ( sym: 263; act: -75 ),
{ 247: }
{ 248: }
  ( sym: 263; act: 258 ),
{ 249: }
{ 250: }
{ 251: }
  ( sym: 262; act: 105 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 106 ),
  ( sym: 273; act: 107 ),
  ( sym: 308; act: 109 ),
  ( sym: 310; act: 110 ),
  ( sym: 315; act: 111 ),
{ 252: }
  ( sym: 262; act: 105 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 106 ),
  ( sym: 273; act: 107 ),
  ( sym: 308; act: 109 ),
  ( sym: 310; act: 110 ),
  ( sym: 315; act: 111 ),
{ 253: }
  ( sym: 318; act: 140 ),
  ( sym: 319; act: 141 ),
  ( sym: 259; act: -135 ),
  ( sym: 260; act: -135 ),
  ( sym: 261; act: -135 ),
  ( sym: 262; act: -135 ),
  ( sym: 263; act: -135 ),
  ( sym: 264; act: -135 ),
  ( sym: 265; act: -135 ),
  ( sym: 267; act: -135 ),
  ( sym: 286; act: -135 ),
  ( sym: 296; act: -135 ),
  ( sym: 300; act: -135 ),
  ( sym: 301; act: -135 ),
  ( sym: 302; act: -135 ),
  ( sym: 303; act: -135 ),
  ( sym: 304; act: -135 ),
  ( sym: 305; act: -135 ),
  ( sym: 306; act: -135 ),
  ( sym: 307; act: -135 ),
  ( sym: 308; act: -135 ),
  ( sym: 309; act: -135 ),
  ( sym: 310; act: -135 ),
  ( sym: 311; act: -135 ),
  ( sym: 312; act: -135 ),
  ( sym: 313; act: -135 ),
  ( sym: 314; act: -135 ),
  ( sym: 315; act: -135 ),
{ 254: }
  ( sym: 262; act: 240 ),
  ( sym: 264; act: 241 ),
  ( sym: 261; act: -97 ),
  ( sym: 263; act: -97 ),
{ 255: }
  ( sym: 263; act: 261 ),
{ 256: }
  ( sym: 265; act: 262 ),
  ( sym: 300; act: 179 ),
  ( sym: 301; act: 180 ),
  ( sym: 302; act: 181 ),
  ( sym: 303; act: 182 ),
  ( sym: 304; act: 183 ),
  ( sym: 305; act: 184 ),
  ( sym: 306; act: 185 ),
  ( sym: 307; act: 186 ),
  ( sym: 308; act: 187 ),
  ( sym: 309; act: 188 ),
  ( sym: 310; act: 189 ),
  ( sym: 311; act: 190 ),
  ( sym: 312; act: 191 ),
  ( sym: 313; act: 192 ),
  ( sym: 314; act: 193 ),
  ( sym: 315; act: 194 ),
{ 257: }
{ 258: }
{ 259: }
  ( sym: 307; act: 186 ),
  ( sym: 308; act: 187 ),
  ( sym: 309; act: 188 ),
  ( sym: 310; act: 189 ),
  ( sym: 311; act: 190 ),
  ( sym: 312; act: 191 ),
  ( sym: 313; act: 192 ),
  ( sym: 314; act: 193 ),
  ( sym: 315; act: 194 ),
  ( sym: 259; act: -122 ),
  ( sym: 260; act: -122 ),
  ( sym: 261; act: -122 ),
  ( sym: 262; act: -122 ),
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
  ( sym: 318; act: -122 ),
  ( sym: 319; act: -122 ),
{ 260: }
  ( sym: 318; act: 140 ),
  ( sym: 319; act: 141 ),
  ( sym: 259; act: -136 ),
  ( sym: 260; act: -136 ),
  ( sym: 261; act: -136 ),
  ( sym: 262; act: -136 ),
  ( sym: 263; act: -136 ),
  ( sym: 264; act: -136 ),
  ( sym: 265; act: -136 ),
  ( sym: 267; act: -136 ),
  ( sym: 286; act: -136 ),
  ( sym: 296; act: -136 ),
  ( sym: 300; act: -136 ),
  ( sym: 301; act: -136 ),
  ( sym: 302; act: -136 ),
  ( sym: 303; act: -136 ),
  ( sym: 304; act: -136 ),
  ( sym: 305; act: -136 ),
  ( sym: 306; act: -136 ),
  ( sym: 307; act: -136 ),
  ( sym: 308; act: -136 ),
  ( sym: 309; act: -136 ),
  ( sym: 310; act: -136 ),
  ( sym: 311; act: -136 ),
  ( sym: 312; act: -136 ),
  ( sym: 313; act: -136 ),
  ( sym: 314; act: -136 ),
  ( sym: 315; act: -136 )
{ 261: }
{ 262: }
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
  ( sym: -23; act: 15 ),
  ( sym: -21; act: 16 ),
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
  ( sym: -23; act: 15 ),
  ( sym: -21; act: 16 ),
  ( sym: -13; act: 17 ),
  ( sym: -11; act: 35 ),
  ( sym: -10; act: 36 ),
{ 9: }
  ( sym: -10; act: 38 ),
{ 10: }
  ( sym: -10; act: 39 ),
{ 11: }
  ( sym: -10; act: 40 ),
{ 12: }
  ( sym: -10; act: 41 ),
{ 13: }
{ 14: }
{ 15: }
{ 16: }
{ 17: }
{ 18: }
  ( sym: -8; act: 42 ),
{ 19: }
{ 20: }
  ( sym: -18; act: 50 ),
  ( sym: -10; act: 39 ),
{ 21: }
  ( sym: -18; act: 53 ),
  ( sym: -10; act: 40 ),
{ 22: }
  ( sym: -20; act: 54 ),
  ( sym: -10; act: 41 ),
{ 23: }
{ 24: }
{ 25: }
{ 26: }
{ 27: }
{ 28: }
{ 29: }
{ 30: }
{ 31: }
  ( sym: -23; act: 15 ),
  ( sym: -21; act: 16 ),
  ( sym: -13; act: 17 ),
  ( sym: -11; act: 62 ),
  ( sym: -10; act: 19 ),
{ 32: }
{ 33: }
{ 34: }
{ 35: }
  ( sym: -8; act: 65 ),
{ 36: }
{ 37: }
  ( sym: -18; act: 50 ),
  ( sym: -10; act: 68 ),
{ 38: }
{ 39: }
  ( sym: -18; act: 72 ),
{ 40: }
  ( sym: -18; act: 73 ),
{ 41: }
  ( sym: -20; act: 74 ),
{ 42: }
  ( sym: -26; act: 75 ),
  ( sym: -14; act: 76 ),
  ( sym: -12; act: 77 ),
  ( sym: -10; act: 78 ),
{ 43: }
{ 44: }
{ 45: }
{ 46: }
{ 47: }
{ 48: }
{ 49: }
{ 50: }
{ 51: }
  ( sym: -4; act: 88 ),
{ 52: }
  ( sym: -23; act: 15 ),
  ( sym: -22; act: 89 ),
  ( sym: -21; act: 16 ),
  ( sym: -19; act: 90 ),
  ( sym: -13; act: 17 ),
  ( sym: -11; act: 91 ),
  ( sym: -10; act: 19 ),
{ 53: }
{ 54: }
{ 55: }
  ( sym: -4; act: 93 ),
{ 56: }
  ( sym: -35; act: 94 ),
  ( sym: -16; act: 95 ),
  ( sym: -10; act: 96 ),
{ 57: }
{ 58: }
{ 59: }
{ 60: }
{ 61: }
{ 62: }
{ 63: }
{ 64: }
{ 65: }
  ( sym: -26; act: 75 ),
  ( sym: -14; act: 76 ),
  ( sym: -12; act: 98 ),
  ( sym: -10; act: 78 ),
{ 66: }
  ( sym: -8; act: 99 ),
{ 67: }
{ 68: }
  ( sym: -18; act: 72 ),
  ( sym: -10; act: 100 ),
{ 69: }
  ( sym: -35; act: 94 ),
  ( sym: -16; act: 101 ),
  ( sym: -10; act: 96 ),
{ 70: }
{ 71: }
  ( sym: -32; act: 102 ),
  ( sym: -17; act: 103 ),
  ( sym: -10; act: 104 ),
{ 72: }
{ 73: }
{ 74: }
{ 75: }
{ 76: }
  ( sym: -29; act: 115 ),
{ 77: }
  ( sym: -9; act: 118 ),
{ 78: }
  ( sym: -28; act: 121 ),
{ 79: }
  ( sym: -4; act: 123 ),
{ 80: }
  ( sym: -26; act: 75 ),
  ( sym: -14; act: 124 ),
  ( sym: -10; act: 78 ),
{ 81: }
  ( sym: -26; act: 75 ),
  ( sym: -14; act: 125 ),
  ( sym: -10; act: 78 ),
{ 82: }
{ 83: }
{ 84: }
{ 85: }
  ( sym: -26; act: 75 ),
  ( sym: -14; act: 126 ),
  ( sym: -10; act: 78 ),
{ 86: }
  ( sym: -26; act: 75 ),
  ( sym: -14; act: 127 ),
  ( sym: -10; act: 78 ),
{ 87: }
{ 88: }
{ 89: }
  ( sym: -23; act: 15 ),
  ( sym: -22; act: 89 ),
  ( sym: -21; act: 16 ),
  ( sym: -19; act: 129 ),
  ( sym: -13; act: 17 ),
  ( sym: -11; act: 91 ),
  ( sym: -10; act: 19 ),
{ 90: }
{ 91: }
  ( sym: -26; act: 75 ),
  ( sym: -14; act: 76 ),
  ( sym: -12; act: 131 ),
  ( sym: -10; act: 78 ),
{ 92: }
{ 93: }
{ 94: }
{ 95: }
{ 96: }
{ 97: }
{ 98: }
{ 99: }
  ( sym: -26; act: 75 ),
  ( sym: -14; act: 137 ),
  ( sym: -10; act: 78 ),
{ 100: }
{ 101: }
{ 102: }
{ 103: }
{ 104: }
{ 105: }
  ( sym: -32; act: 144 ),
  ( sym: -30; act: 145 ),
  ( sym: -27; act: 146 ),
  ( sym: -23; act: 15 ),
  ( sym: -21; act: 16 ),
  ( sym: -13; act: 17 ),
  ( sym: -11; act: 147 ),
  ( sym: -10; act: 148 ),
{ 106: }
{ 107: }
{ 108: }
{ 109: }
  ( sym: -32; act: 149 ),
  ( sym: -10; act: 104 ),
{ 110: }
  ( sym: -32; act: 150 ),
  ( sym: -10; act: 104 ),
{ 111: }
  ( sym: -32; act: 151 ),
  ( sym: -10; act: 104 ),
{ 112: }
{ 113: }
{ 114: }
  ( sym: -26; act: 75 ),
  ( sym: -14; act: 152 ),
  ( sym: -10; act: 78 ),
{ 115: }
{ 116: }
  ( sym: -24; act: 153 ),
  ( sym: -23; act: 15 ),
  ( sym: -21; act: 16 ),
  ( sym: -15; act: 154 ),
  ( sym: -13; act: 17 ),
  ( sym: -11; act: 155 ),
  ( sym: -10; act: 19 ),
{ 117: }
  ( sym: -32; act: 144 ),
  ( sym: -30; act: 159 ),
  ( sym: -27; act: 160 ),
  ( sym: -10; act: 104 ),
{ 118: }
{ 119: }
  ( sym: -26; act: 75 ),
  ( sym: -14; act: 162 ),
  ( sym: -10; act: 78 ),
{ 120: }
{ 121: }
  ( sym: -32; act: 144 ),
  ( sym: -30; act: 159 ),
  ( sym: -27; act: 164 ),
  ( sym: -10; act: 104 ),
{ 122: }
  ( sym: -32; act: 144 ),
  ( sym: -30; act: 159 ),
  ( sym: -27; act: 165 ),
  ( sym: -10; act: 104 ),
{ 123: }
{ 124: }
  ( sym: -29; act: 115 ),
{ 125: }
  ( sym: -29; act: 115 ),
{ 126: }
  ( sym: -29; act: 115 ),
{ 127: }
  ( sym: -29; act: 115 ),
{ 128: }
{ 129: }
{ 130: }
{ 131: }
{ 132: }
{ 133: }
  ( sym: -35; act: 94 ),
  ( sym: -16; act: 169 ),
  ( sym: -10; act: 96 ),
{ 134: }
{ 135: }
  ( sym: -32; act: 144 ),
  ( sym: -30; act: 159 ),
  ( sym: -27; act: 170 ),
  ( sym: -10; act: 104 ),
{ 136: }
{ 137: }
  ( sym: -29; act: 115 ),
{ 138: }
{ 139: }
{ 140: }
  ( sym: -32; act: 144 ),
  ( sym: -30; act: 159 ),
  ( sym: -27; act: 173 ),
  ( sym: -10; act: 104 ),
{ 141: }
  ( sym: -32; act: 144 ),
  ( sym: -30; act: 159 ),
  ( sym: -27; act: 174 ),
  ( sym: -10; act: 104 ),
{ 142: }
{ 143: }
  ( sym: -36; act: 175 ),
  ( sym: -34; act: 176 ),
  ( sym: -32; act: 144 ),
  ( sym: -30; act: 159 ),
  ( sym: -27; act: 177 ),
  ( sym: -10; act: 104 ),
{ 144: }
{ 145: }
{ 146: }
{ 147: }
  ( sym: -26; act: 195 ),
{ 148: }
{ 149: }
{ 150: }
{ 151: }
{ 152: }
  ( sym: -29; act: 115 ),
{ 153: }
{ 154: }
{ 155: }
  ( sym: -26; act: 201 ),
  ( sym: -25; act: 202 ),
  ( sym: -14; act: 203 ),
  ( sym: -10; act: 78 ),
{ 156: }
{ 157: }
{ 158: }
{ 159: }
{ 160: }
{ 161: }
{ 162: }
  ( sym: -29; act: 115 ),
{ 163: }
  ( sym: -10; act: 209 ),
{ 164: }
{ 165: }
{ 166: }
  ( sym: -26; act: 75 ),
  ( sym: -14; act: 76 ),
  ( sym: -12; act: 210 ),
  ( sym: -10; act: 78 ),
{ 167: }
{ 168: }
{ 169: }
{ 170: }
{ 171: }
{ 172: }
  ( sym: -32; act: 102 ),
  ( sym: -17; act: 212 ),
  ( sym: -10; act: 104 ),
{ 173: }
{ 174: }
{ 175: }
{ 176: }
{ 177: }
{ 178: }
{ 179: }
  ( sym: -32; act: 144 ),
  ( sym: -30; act: 159 ),
  ( sym: -27; act: 215 ),
  ( sym: -10; act: 104 ),
{ 180: }
  ( sym: -32; act: 144 ),
  ( sym: -30; act: 159 ),
  ( sym: -27; act: 216 ),
  ( sym: -10; act: 104 ),
{ 181: }
  ( sym: -32; act: 144 ),
  ( sym: -30; act: 159 ),
  ( sym: -27; act: 217 ),
  ( sym: -10; act: 104 ),
{ 182: }
  ( sym: -32; act: 144 ),
  ( sym: -30; act: 159 ),
  ( sym: -27; act: 218 ),
  ( sym: -10; act: 104 ),
{ 183: }
  ( sym: -32; act: 144 ),
  ( sym: -30; act: 159 ),
  ( sym: -27; act: 219 ),
  ( sym: -10; act: 104 ),
{ 184: }
  ( sym: -32; act: 144 ),
  ( sym: -30; act: 159 ),
  ( sym: -27; act: 220 ),
  ( sym: -10; act: 104 ),
{ 185: }
  ( sym: -32; act: 144 ),
  ( sym: -31; act: 221 ),
  ( sym: -30; act: 159 ),
  ( sym: -27; act: 222 ),
  ( sym: -10; act: 104 ),
{ 186: }
  ( sym: -32; act: 144 ),
  ( sym: -30; act: 159 ),
  ( sym: -27; act: 223 ),
  ( sym: -10; act: 104 ),
{ 187: }
  ( sym: -32; act: 144 ),
  ( sym: -30; act: 159 ),
  ( sym: -27; act: 224 ),
  ( sym: -10; act: 104 ),
{ 188: }
  ( sym: -32; act: 144 ),
  ( sym: -30; act: 159 ),
  ( sym: -27; act: 225 ),
  ( sym: -10; act: 104 ),
{ 189: }
  ( sym: -32; act: 144 ),
  ( sym: -30; act: 159 ),
  ( sym: -27; act: 226 ),
  ( sym: -10; act: 104 ),
{ 190: }
  ( sym: -32; act: 144 ),
  ( sym: -30; act: 159 ),
  ( sym: -27; act: 227 ),
  ( sym: -10; act: 104 ),
{ 191: }
  ( sym: -32; act: 144 ),
  ( sym: -30; act: 159 ),
  ( sym: -27; act: 228 ),
  ( sym: -10; act: 104 ),
{ 192: }
  ( sym: -32; act: 144 ),
  ( sym: -30; act: 159 ),
  ( sym: -27; act: 229 ),
  ( sym: -10; act: 104 ),
{ 193: }
  ( sym: -32; act: 144 ),
  ( sym: -30; act: 159 ),
  ( sym: -27; act: 230 ),
  ( sym: -10; act: 104 ),
{ 194: }
  ( sym: -32; act: 144 ),
  ( sym: -30; act: 159 ),
  ( sym: -27; act: 231 ),
  ( sym: -10; act: 104 ),
{ 195: }
{ 196: }
  ( sym: -32; act: 233 ),
  ( sym: -10; act: 104 ),
{ 197: }
{ 198: }
  ( sym: -33; act: 235 ),
  ( sym: -32; act: 236 ),
  ( sym: -10; act: 104 ),
{ 199: }
  ( sym: -24; act: 153 ),
  ( sym: -23; act: 15 ),
  ( sym: -21; act: 16 ),
  ( sym: -15; act: 237 ),
  ( sym: -13; act: 17 ),
  ( sym: -11; act: 155 ),
  ( sym: -10; act: 19 ),
{ 200: }
{ 201: }
{ 202: }
  ( sym: -29; act: 239 ),
{ 203: }
  ( sym: -29; act: 115 ),
{ 204: }
  ( sym: -26; act: 201 ),
  ( sym: -25; act: 242 ),
  ( sym: -14; act: 124 ),
  ( sym: -10; act: 78 ),
{ 205: }
  ( sym: -26; act: 201 ),
  ( sym: -25; act: 244 ),
  ( sym: -14; act: 125 ),
  ( sym: -10; act: 78 ),
{ 206: }
  ( sym: -26; act: 201 ),
  ( sym: -25; act: 245 ),
  ( sym: -14; act: 246 ),
  ( sym: -10; act: 78 ),
{ 207: }
{ 208: }
{ 209: }
{ 210: }
{ 211: }
  ( sym: -24; act: 153 ),
  ( sym: -23; act: 15 ),
  ( sym: -21; act: 16 ),
  ( sym: -15; act: 248 ),
  ( sym: -13; act: 17 ),
  ( sym: -11; act: 155 ),
  ( sym: -10; act: 19 ),
{ 212: }
{ 213: }
  ( sym: -36; act: 175 ),
  ( sym: -34; act: 250 ),
  ( sym: -32; act: 144 ),
  ( sym: -30; act: 159 ),
  ( sym: -27; act: 177 ),
  ( sym: -10; act: 104 ),
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
{ 226: }
{ 227: }
{ 228: }
{ 229: }
{ 230: }
{ 231: }
{ 232: }
{ 233: }
{ 234: }
  ( sym: -32; act: 253 ),
  ( sym: -10; act: 104 ),
{ 235: }
{ 236: }
{ 237: }
{ 238: }
  ( sym: -26; act: 201 ),
  ( sym: -25; act: 254 ),
  ( sym: -14; act: 152 ),
  ( sym: -10; act: 78 ),
{ 239: }
{ 240: }
  ( sym: -24; act: 153 ),
  ( sym: -23; act: 15 ),
  ( sym: -21; act: 16 ),
  ( sym: -15; act: 255 ),
  ( sym: -13; act: 17 ),
  ( sym: -11; act: 155 ),
  ( sym: -10; act: 19 ),
{ 241: }
  ( sym: -32; act: 144 ),
  ( sym: -30; act: 159 ),
  ( sym: -27; act: 256 ),
  ( sym: -10; act: 104 ),
{ 242: }
  ( sym: -29; act: 239 ),
{ 243: }
  ( sym: -26; act: 201 ),
  ( sym: -25; act: 245 ),
  ( sym: -14; act: 127 ),
  ( sym: -10; act: 78 ),
{ 244: }
  ( sym: -29; act: 239 ),
{ 245: }
  ( sym: -29; act: 239 ),
{ 246: }
  ( sym: -29; act: 115 ),
{ 247: }
{ 248: }
{ 249: }
{ 250: }
{ 251: }
  ( sym: -32; act: 144 ),
  ( sym: -30; act: 159 ),
  ( sym: -27; act: 259 ),
  ( sym: -10; act: 104 ),
{ 252: }
  ( sym: -32; act: 260 ),
  ( sym: -10; act: 104 ),
{ 253: }
{ 254: }
  ( sym: -29; act: 239 )
{ 255: }
{ 256: }
{ 257: }
{ 258: }
{ 259: }
{ 260: }
{ 261: }
{ 262: }
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
{ 15: } -68,
{ 16: } -50,
{ 17: } -49,
{ 18: } 0,
{ 19: } -69,
{ 20: } 0,
{ 21: } 0,
{ 22: } 0,
{ 23: } -54,
{ 24: } -66,
{ 25: } 0,
{ 26: } 0,
{ 27: } -55,
{ 28: } -58,
{ 29: } -67,
{ 30: } -65,
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
{ 42: } 0,
{ 43: } -9,
{ 44: } -10,
{ 45: } -11,
{ 46: } -12,
{ 47: } -13,
{ 48: } -14,
{ 49: } -15,
{ 50: } 0,
{ 51: } -2,
{ 52: } 0,
{ 53: } 0,
{ 54: } -48,
{ 55: } -2,
{ 56: } 0,
{ 57: } -63,
{ 58: } 0,
{ 59: } -56,
{ 60: } -64,
{ 61: } -59,
{ 62: } -43,
{ 63: } -25,
{ 64: } -30,
{ 65: } 0,
{ 66: } 0,
{ 67: } -24,
{ 68: } 0,
{ 69: } 0,
{ 70: } -28,
{ 71: } 0,
{ 72: } 0,
{ 73: } 0,
{ 74: } -41,
{ 75: } 0,
{ 76: } 0,
{ 77: } 0,
{ 78: } 0,
{ 79: } -2,
{ 80: } 0,
{ 81: } 0,
{ 82: } -80,
{ 83: } -82,
{ 84: } -81,
{ 85: } 0,
{ 86: } 0,
{ 87: } -46,
{ 88: } 0,
{ 89: } 0,
{ 90: } 0,
{ 91: } 0,
{ 92: } -44,
{ 93: } 0,
{ 94: } 0,
{ 95: } 0,
{ 96: } 0,
{ 97: } -60,
{ 98: } 0,
{ 99: } 0,
{ 100: } 0,
{ 101: } 0,
{ 102: } 0,
{ 103: } 0,
{ 104: } 0,
{ 105: } 0,
{ 106: } -127,
{ 107: } -126,
{ 108: } -27,
{ 109: } 0,
{ 110: } 0,
{ 111: } 0,
{ 112: } -35,
{ 113: } -37,
{ 114: } 0,
{ 115: } -91,
{ 116: } 0,
{ 117: } 0,
{ 118: } 0,
{ 119: } 0,
{ 120: } 0,
{ 121: } 0,
{ 122: } 0,
{ 123: } 0,
{ 124: } 0,
{ 125: } 0,
{ 126: } 0,
{ 127: } 0,
{ 128: } -32,
{ 129: } -51,
{ 130: } -31,
{ 131: } 0,
{ 132: } -34,
{ 133: } 0,
{ 134: } -33,
{ 135: } 0,
{ 136: } -23,
{ 137: } 0,
{ 138: } -21,
{ 139: } 0,
{ 140: } 0,
{ 141: } 0,
{ 142: } -29,
{ 143: } 0,
{ 144: } 0,
{ 145: } 0,
{ 146: } 0,
{ 147: } 0,
{ 148: } 0,
{ 149: } 0,
{ 150: } 0,
{ 151: } 0,
{ 152: } 0,
{ 153: } 0,
{ 154: } 0,
{ 155: } 0,
{ 156: } -94,
{ 157: } 0,
{ 158: } -79,
{ 159: } -104,
{ 160: } 0,
{ 161: } -19,
{ 162: } 0,
{ 163: } 0,
{ 164: } 0,
{ 165: } 0,
{ 166: } 0,
{ 167: } -93,
{ 168: } -53,
{ 169: } -139,
{ 170: } 0,
{ 171: } 0,
{ 172: } 0,
{ 173: } -128,
{ 174: } -129,
{ 175: } 0,
{ 176: } 0,
{ 177: } 0,
{ 178: } -138,
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
{ 191: } 0,
{ 192: } 0,
{ 193: } 0,
{ 194: } 0,
{ 195: } 0,
{ 196: } 0,
{ 197: } 0,
{ 198: } 0,
{ 199: } 0,
{ 200: } -90,
{ 201: } 0,
{ 202: } 0,
{ 203: } 0,
{ 204: } 0,
{ 205: } 0,
{ 206: } 0,
{ 207: } -95,
{ 208: } -92,
{ 209: } 0,
{ 210: } -71,
{ 211: } 0,
{ 212: } 0,
{ 213: } 0,
{ 214: } -137,
{ 215: } 0,
{ 216: } 0,
{ 217: } 0,
{ 218: } 0,
{ 219: } 0,
{ 220: } 0,
{ 221: } -120,
{ 222: } 0,
{ 223: } 0,
{ 224: } 0,
{ 225: } 0,
{ 226: } 0,
{ 227: } 0,
{ 228: } 0,
{ 229: } 0,
{ 230: } 0,
{ 231: } 0,
{ 232: } 0,
{ 233: } 0,
{ 234: } 0,
{ 235: } -133,
{ 236: } 0,
{ 237: } -78,
{ 238: } 0,
{ 239: } -100,
{ 240: } 0,
{ 241: } 0,
{ 242: } 0,
{ 243: } 0,
{ 244: } 0,
{ 245: } 0,
{ 246: } 0,
{ 247: } -17,
{ 248: } 0,
{ 249: } -26,
{ 250: } -145,
{ 251: } 0,
{ 252: } 0,
{ 253: } 0,
{ 254: } 0,
{ 255: } 0,
{ 256: } 0,
{ 257: } -102,
{ 258: } -22,
{ 259: } 0,
{ 260: } 0,
{ 261: } -99,
{ 262: } -101
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
{ 39: } 185,
{ 40: } 206,
{ 41: } 227,
{ 42: } 248,
{ 43: } 257,
{ 44: } 257,
{ 45: } 257,
{ 46: } 257,
{ 47: } 257,
{ 48: } 257,
{ 49: } 257,
{ 50: } 257,
{ 51: } 277,
{ 52: } 277,
{ 53: } 289,
{ 54: } 309,
{ 55: } 309,
{ 56: } 309,
{ 57: } 311,
{ 58: } 311,
{ 59: } 331,
{ 60: } 331,
{ 61: } 331,
{ 62: } 331,
{ 63: } 331,
{ 64: } 331,
{ 65: } 331,
{ 66: } 340,
{ 67: } 355,
{ 68: } 355,
{ 69: } 372,
{ 70: } 374,
{ 71: } 374,
{ 72: } 382,
{ 73: } 403,
{ 74: } 424,
{ 75: } 424,
{ 76: } 425,
{ 77: } 430,
{ 78: } 433,
{ 79: } 440,
{ 80: } 440,
{ 81: } 448,
{ 82: } 456,
{ 83: } 456,
{ 84: } 456,
{ 85: } 456,
{ 86: } 464,
{ 87: } 472,
{ 88: } 472,
{ 89: } 473,
{ 90: } 486,
{ 91: } 487,
{ 92: } 496,
{ 93: } 496,
{ 94: } 497,
{ 95: } 500,
{ 96: } 501,
{ 97: } 505,
{ 98: } 505,
{ 99: } 507,
{ 100: } 515,
{ 101: } 516,
{ 102: } 517,
{ 103: } 520,
{ 104: } 521,
{ 105: } 549,
{ 106: } 567,
{ 107: } 567,
{ 108: } 567,
{ 109: } 567,
{ 110: } 574,
{ 111: } 581,
{ 112: } 588,
{ 113: } 588,
{ 114: } 588,
{ 115: } 596,
{ 116: } 596,
{ 117: } 610,
{ 118: } 617,
{ 119: } 618,
{ 120: } 626,
{ 121: } 627,
{ 122: } 634,
{ 123: } 641,
{ 124: } 644,
{ 125: } 647,
{ 126: } 653,
{ 127: } 659,
{ 128: } 665,
{ 129: } 665,
{ 130: } 665,
{ 131: } 665,
{ 132: } 667,
{ 133: } 667,
{ 134: } 670,
{ 135: } 670,
{ 136: } 677,
{ 137: } 677,
{ 138: } 680,
{ 139: } 680,
{ 140: } 681,
{ 141: } 688,
{ 142: } 695,
{ 143: } 695,
{ 144: } 703,
{ 145: } 731,
{ 146: } 748,
{ 147: } 764,
{ 148: } 769,
{ 149: } 792,
{ 150: } 820,
{ 151: } 848,
{ 152: } 876,
{ 153: } 882,
{ 154: } 884,
{ 155: } 885,
{ 156: } 896,
{ 157: } 896,
{ 158: } 907,
{ 159: } 907,
{ 160: } 907,
{ 161: } 924,
{ 162: } 924,
{ 163: } 929,
{ 164: } 930,
{ 165: } 952,
{ 166: } 974,
{ 167: } 983,
{ 168: } 983,
{ 169: } 983,
{ 170: } 983,
{ 171: } 1002,
{ 172: } 1003,
{ 173: } 1010,
{ 174: } 1010,
{ 175: } 1010,
{ 176: } 1012,
{ 177: } 1013,
{ 178: } 1031,
{ 179: } 1031,
{ 180: } 1038,
{ 181: } 1045,
{ 182: } 1052,
{ 183: } 1059,
{ 184: } 1066,
{ 185: } 1073,
{ 186: } 1080,
{ 187: } 1087,
{ 188: } 1094,
{ 189: } 1101,
{ 190: } 1108,
{ 191: } 1115,
{ 192: } 1122,
{ 193: } 1129,
{ 194: } 1136,
{ 195: } 1143,
{ 196: } 1144,
{ 197: } 1151,
{ 198: } 1152,
{ 199: } 1183,
{ 200: } 1196,
{ 201: } 1196,
{ 202: } 1197,
{ 203: } 1201,
{ 204: } 1205,
{ 205: } 1215,
{ 206: } 1226,
{ 207: } 1237,
{ 208: } 1237,
{ 209: } 1237,
{ 210: } 1238,
{ 211: } 1238,
{ 212: } 1251,
{ 213: } 1252,
{ 214: } 1260,
{ 215: } 1260,
{ 216: } 1288,
{ 217: } 1316,
{ 218: } 1344,
{ 219: } 1372,
{ 220: } 1400,
{ 221: } 1428,
{ 222: } 1428,
{ 223: } 1445,
{ 224: } 1473,
{ 225: } 1501,
{ 226: } 1529,
{ 227: } 1557,
{ 228: } 1585,
{ 229: } 1613,
{ 230: } 1641,
{ 231: } 1669,
{ 232: } 1697,
{ 233: } 1698,
{ 234: } 1726,
{ 235: } 1733,
{ 236: } 1733,
{ 237: } 1761,
{ 238: } 1761,
{ 239: } 1772,
{ 240: } 1772,
{ 241: } 1786,
{ 242: } 1793,
{ 243: } 1796,
{ 244: } 1807,
{ 245: } 1811,
{ 246: } 1815,
{ 247: } 1819,
{ 248: } 1819,
{ 249: } 1820,
{ 250: } 1820,
{ 251: } 1820,
{ 252: } 1827,
{ 253: } 1834,
{ 254: } 1862,
{ 255: } 1866,
{ 256: } 1867,
{ 257: } 1884,
{ 258: } 1884,
{ 259: } 1884,
{ 260: } 1912,
{ 261: } 1940,
{ 262: } 1940
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
{ 38: } 184,
{ 39: } 205,
{ 40: } 226,
{ 41: } 247,
{ 42: } 256,
{ 43: } 256,
{ 44: } 256,
{ 45: } 256,
{ 46: } 256,
{ 47: } 256,
{ 48: } 256,
{ 49: } 256,
{ 50: } 276,
{ 51: } 276,
{ 52: } 288,
{ 53: } 308,
{ 54: } 308,
{ 55: } 308,
{ 56: } 310,
{ 57: } 310,
{ 58: } 330,
{ 59: } 330,
{ 60: } 330,
{ 61: } 330,
{ 62: } 330,
{ 63: } 330,
{ 64: } 330,
{ 65: } 339,
{ 66: } 354,
{ 67: } 354,
{ 68: } 371,
{ 69: } 373,
{ 70: } 373,
{ 71: } 381,
{ 72: } 402,
{ 73: } 423,
{ 74: } 423,
{ 75: } 424,
{ 76: } 429,
{ 77: } 432,
{ 78: } 439,
{ 79: } 439,
{ 80: } 447,
{ 81: } 455,
{ 82: } 455,
{ 83: } 455,
{ 84: } 455,
{ 85: } 463,
{ 86: } 471,
{ 87: } 471,
{ 88: } 472,
{ 89: } 485,
{ 90: } 486,
{ 91: } 495,
{ 92: } 495,
{ 93: } 496,
{ 94: } 499,
{ 95: } 500,
{ 96: } 504,
{ 97: } 504,
{ 98: } 506,
{ 99: } 514,
{ 100: } 515,
{ 101: } 516,
{ 102: } 519,
{ 103: } 520,
{ 104: } 548,
{ 105: } 566,
{ 106: } 566,
{ 107: } 566,
{ 108: } 566,
{ 109: } 573,
{ 110: } 580,
{ 111: } 587,
{ 112: } 587,
{ 113: } 587,
{ 114: } 595,
{ 115: } 595,
{ 116: } 609,
{ 117: } 616,
{ 118: } 617,
{ 119: } 625,
{ 120: } 626,
{ 121: } 633,
{ 122: } 640,
{ 123: } 643,
{ 124: } 646,
{ 125: } 652,
{ 126: } 658,
{ 127: } 664,
{ 128: } 664,
{ 129: } 664,
{ 130: } 664,
{ 131: } 666,
{ 132: } 666,
{ 133: } 669,
{ 134: } 669,
{ 135: } 676,
{ 136: } 676,
{ 137: } 679,
{ 138: } 679,
{ 139: } 680,
{ 140: } 687,
{ 141: } 694,
{ 142: } 694,
{ 143: } 702,
{ 144: } 730,
{ 145: } 747,
{ 146: } 763,
{ 147: } 768,
{ 148: } 791,
{ 149: } 819,
{ 150: } 847,
{ 151: } 875,
{ 152: } 881,
{ 153: } 883,
{ 154: } 884,
{ 155: } 895,
{ 156: } 895,
{ 157: } 906,
{ 158: } 906,
{ 159: } 906,
{ 160: } 923,
{ 161: } 923,
{ 162: } 928,
{ 163: } 929,
{ 164: } 951,
{ 165: } 973,
{ 166: } 982,
{ 167: } 982,
{ 168: } 982,
{ 169: } 982,
{ 170: } 1001,
{ 171: } 1002,
{ 172: } 1009,
{ 173: } 1009,
{ 174: } 1009,
{ 175: } 1011,
{ 176: } 1012,
{ 177: } 1030,
{ 178: } 1030,
{ 179: } 1037,
{ 180: } 1044,
{ 181: } 1051,
{ 182: } 1058,
{ 183: } 1065,
{ 184: } 1072,
{ 185: } 1079,
{ 186: } 1086,
{ 187: } 1093,
{ 188: } 1100,
{ 189: } 1107,
{ 190: } 1114,
{ 191: } 1121,
{ 192: } 1128,
{ 193: } 1135,
{ 194: } 1142,
{ 195: } 1143,
{ 196: } 1150,
{ 197: } 1151,
{ 198: } 1182,
{ 199: } 1195,
{ 200: } 1195,
{ 201: } 1196,
{ 202: } 1200,
{ 203: } 1204,
{ 204: } 1214,
{ 205: } 1225,
{ 206: } 1236,
{ 207: } 1236,
{ 208: } 1236,
{ 209: } 1237,
{ 210: } 1237,
{ 211: } 1250,
{ 212: } 1251,
{ 213: } 1259,
{ 214: } 1259,
{ 215: } 1287,
{ 216: } 1315,
{ 217: } 1343,
{ 218: } 1371,
{ 219: } 1399,
{ 220: } 1427,
{ 221: } 1427,
{ 222: } 1444,
{ 223: } 1472,
{ 224: } 1500,
{ 225: } 1528,
{ 226: } 1556,
{ 227: } 1584,
{ 228: } 1612,
{ 229: } 1640,
{ 230: } 1668,
{ 231: } 1696,
{ 232: } 1697,
{ 233: } 1725,
{ 234: } 1732,
{ 235: } 1732,
{ 236: } 1760,
{ 237: } 1760,
{ 238: } 1771,
{ 239: } 1771,
{ 240: } 1785,
{ 241: } 1792,
{ 242: } 1795,
{ 243: } 1806,
{ 244: } 1810,
{ 245: } 1814,
{ 246: } 1818,
{ 247: } 1818,
{ 248: } 1819,
{ 249: } 1819,
{ 250: } 1819,
{ 251: } 1826,
{ 252: } 1833,
{ 253: } 1861,
{ 254: } 1865,
{ 255: } 1866,
{ 256: } 1883,
{ 257: } 1883,
{ 258: } 1883,
{ 259: } 1911,
{ 260: } 1939,
{ 261: } 1939,
{ 262: } 1939
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
{ 38: } 41,
{ 39: } 41,
{ 40: } 42,
{ 41: } 43,
{ 42: } 44,
{ 43: } 48,
{ 44: } 48,
{ 45: } 48,
{ 46: } 48,
{ 47: } 48,
{ 48: } 48,
{ 49: } 48,
{ 50: } 48,
{ 51: } 48,
{ 52: } 49,
{ 53: } 56,
{ 54: } 56,
{ 55: } 56,
{ 56: } 57,
{ 57: } 60,
{ 58: } 60,
{ 59: } 60,
{ 60: } 60,
{ 61: } 60,
{ 62: } 60,
{ 63: } 60,
{ 64: } 60,
{ 65: } 60,
{ 66: } 64,
{ 67: } 65,
{ 68: } 65,
{ 69: } 67,
{ 70: } 70,
{ 71: } 70,
{ 72: } 73,
{ 73: } 73,
{ 74: } 73,
{ 75: } 73,
{ 76: } 73,
{ 77: } 74,
{ 78: } 75,
{ 79: } 76,
{ 80: } 77,
{ 81: } 80,
{ 82: } 83,
{ 83: } 83,
{ 84: } 83,
{ 85: } 83,
{ 86: } 86,
{ 87: } 89,
{ 88: } 89,
{ 89: } 89,
{ 90: } 96,
{ 91: } 96,
{ 92: } 100,
{ 93: } 100,
{ 94: } 100,
{ 95: } 100,
{ 96: } 100,
{ 97: } 100,
{ 98: } 100,
{ 99: } 100,
{ 100: } 103,
{ 101: } 103,
{ 102: } 103,
{ 103: } 103,
{ 104: } 103,
{ 105: } 103,
{ 106: } 111,
{ 107: } 111,
{ 108: } 111,
{ 109: } 111,
{ 110: } 113,
{ 111: } 115,
{ 112: } 117,
{ 113: } 117,
{ 114: } 117,
{ 115: } 120,
{ 116: } 120,
{ 117: } 127,
{ 118: } 131,
{ 119: } 131,
{ 120: } 134,
{ 121: } 134,
{ 122: } 138,
{ 123: } 142,
{ 124: } 142,
{ 125: } 143,
{ 126: } 144,
{ 127: } 145,
{ 128: } 146,
{ 129: } 146,
{ 130: } 146,
{ 131: } 146,
{ 132: } 146,
{ 133: } 146,
{ 134: } 149,
{ 135: } 149,
{ 136: } 153,
{ 137: } 153,
{ 138: } 154,
{ 139: } 154,
{ 140: } 154,
{ 141: } 158,
{ 142: } 162,
{ 143: } 162,
{ 144: } 168,
{ 145: } 168,
{ 146: } 168,
{ 147: } 168,
{ 148: } 169,
{ 149: } 169,
{ 150: } 169,
{ 151: } 169,
{ 152: } 169,
{ 153: } 170,
{ 154: } 170,
{ 155: } 170,
{ 156: } 174,
{ 157: } 174,
{ 158: } 174,
{ 159: } 174,
{ 160: } 174,
{ 161: } 174,
{ 162: } 174,
{ 163: } 175,
{ 164: } 176,
{ 165: } 176,
{ 166: } 176,
{ 167: } 180,
{ 168: } 180,
{ 169: } 180,
{ 170: } 180,
{ 171: } 180,
{ 172: } 180,
{ 173: } 183,
{ 174: } 183,
{ 175: } 183,
{ 176: } 183,
{ 177: } 183,
{ 178: } 183,
{ 179: } 183,
{ 180: } 187,
{ 181: } 191,
{ 182: } 195,
{ 183: } 199,
{ 184: } 203,
{ 185: } 207,
{ 186: } 212,
{ 187: } 216,
{ 188: } 220,
{ 189: } 224,
{ 190: } 228,
{ 191: } 232,
{ 192: } 236,
{ 193: } 240,
{ 194: } 244,
{ 195: } 248,
{ 196: } 248,
{ 197: } 250,
{ 198: } 250,
{ 199: } 253,
{ 200: } 260,
{ 201: } 260,
{ 202: } 260,
{ 203: } 261,
{ 204: } 262,
{ 205: } 266,
{ 206: } 270,
{ 207: } 274,
{ 208: } 274,
{ 209: } 274,
{ 210: } 274,
{ 211: } 274,
{ 212: } 281,
{ 213: } 281,
{ 214: } 287,
{ 215: } 287,
{ 216: } 287,
{ 217: } 287,
{ 218: } 287,
{ 219: } 287,
{ 220: } 287,
{ 221: } 287,
{ 222: } 287,
{ 223: } 287,
{ 224: } 287,
{ 225: } 287,
{ 226: } 287,
{ 227: } 287,
{ 228: } 287,
{ 229: } 287,
{ 230: } 287,
{ 231: } 287,
{ 232: } 287,
{ 233: } 287,
{ 234: } 287,
{ 235: } 289,
{ 236: } 289,
{ 237: } 289,
{ 238: } 289,
{ 239: } 293,
{ 240: } 293,
{ 241: } 300,
{ 242: } 304,
{ 243: } 305,
{ 244: } 309,
{ 245: } 310,
{ 246: } 311,
{ 247: } 312,
{ 248: } 312,
{ 249: } 312,
{ 250: } 312,
{ 251: } 312,
{ 252: } 316,
{ 253: } 318,
{ 254: } 318,
{ 255: } 319,
{ 256: } 319,
{ 257: } 319,
{ 258: } 319,
{ 259: } 319,
{ 260: } 319,
{ 261: } 319,
{ 262: } 319
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
{ 37: } 40,
{ 38: } 40,
{ 39: } 41,
{ 40: } 42,
{ 41: } 43,
{ 42: } 47,
{ 43: } 47,
{ 44: } 47,
{ 45: } 47,
{ 46: } 47,
{ 47: } 47,
{ 48: } 47,
{ 49: } 47,
{ 50: } 47,
{ 51: } 48,
{ 52: } 55,
{ 53: } 55,
{ 54: } 55,
{ 55: } 56,
{ 56: } 59,
{ 57: } 59,
{ 58: } 59,
{ 59: } 59,
{ 60: } 59,
{ 61: } 59,
{ 62: } 59,
{ 63: } 59,
{ 64: } 59,
{ 65: } 63,
{ 66: } 64,
{ 67: } 64,
{ 68: } 66,
{ 69: } 69,
{ 70: } 69,
{ 71: } 72,
{ 72: } 72,
{ 73: } 72,
{ 74: } 72,
{ 75: } 72,
{ 76: } 73,
{ 77: } 74,
{ 78: } 75,
{ 79: } 76,
{ 80: } 79,
{ 81: } 82,
{ 82: } 82,
{ 83: } 82,
{ 84: } 82,
{ 85: } 85,
{ 86: } 88,
{ 87: } 88,
{ 88: } 88,
{ 89: } 95,
{ 90: } 95,
{ 91: } 99,
{ 92: } 99,
{ 93: } 99,
{ 94: } 99,
{ 95: } 99,
{ 96: } 99,
{ 97: } 99,
{ 98: } 99,
{ 99: } 102,
{ 100: } 102,
{ 101: } 102,
{ 102: } 102,
{ 103: } 102,
{ 104: } 102,
{ 105: } 110,
{ 106: } 110,
{ 107: } 110,
{ 108: } 110,
{ 109: } 112,
{ 110: } 114,
{ 111: } 116,
{ 112: } 116,
{ 113: } 116,
{ 114: } 119,
{ 115: } 119,
{ 116: } 126,
{ 117: } 130,
{ 118: } 130,
{ 119: } 133,
{ 120: } 133,
{ 121: } 137,
{ 122: } 141,
{ 123: } 141,
{ 124: } 142,
{ 125: } 143,
{ 126: } 144,
{ 127: } 145,
{ 128: } 145,
{ 129: } 145,
{ 130: } 145,
{ 131: } 145,
{ 132: } 145,
{ 133: } 148,
{ 134: } 148,
{ 135: } 152,
{ 136: } 152,
{ 137: } 153,
{ 138: } 153,
{ 139: } 153,
{ 140: } 157,
{ 141: } 161,
{ 142: } 161,
{ 143: } 167,
{ 144: } 167,
{ 145: } 167,
{ 146: } 167,
{ 147: } 168,
{ 148: } 168,
{ 149: } 168,
{ 150: } 168,
{ 151: } 168,
{ 152: } 169,
{ 153: } 169,
{ 154: } 169,
{ 155: } 173,
{ 156: } 173,
{ 157: } 173,
{ 158: } 173,
{ 159: } 173,
{ 160: } 173,
{ 161: } 173,
{ 162: } 174,
{ 163: } 175,
{ 164: } 175,
{ 165: } 175,
{ 166: } 179,
{ 167: } 179,
{ 168: } 179,
{ 169: } 179,
{ 170: } 179,
{ 171: } 179,
{ 172: } 182,
{ 173: } 182,
{ 174: } 182,
{ 175: } 182,
{ 176: } 182,
{ 177: } 182,
{ 178: } 182,
{ 179: } 186,
{ 180: } 190,
{ 181: } 194,
{ 182: } 198,
{ 183: } 202,
{ 184: } 206,
{ 185: } 211,
{ 186: } 215,
{ 187: } 219,
{ 188: } 223,
{ 189: } 227,
{ 190: } 231,
{ 191: } 235,
{ 192: } 239,
{ 193: } 243,
{ 194: } 247,
{ 195: } 247,
{ 196: } 249,
{ 197: } 249,
{ 198: } 252,
{ 199: } 259,
{ 200: } 259,
{ 201: } 259,
{ 202: } 260,
{ 203: } 261,
{ 204: } 265,
{ 205: } 269,
{ 206: } 273,
{ 207: } 273,
{ 208: } 273,
{ 209: } 273,
{ 210: } 273,
{ 211: } 280,
{ 212: } 280,
{ 213: } 286,
{ 214: } 286,
{ 215: } 286,
{ 216: } 286,
{ 217: } 286,
{ 218: } 286,
{ 219: } 286,
{ 220: } 286,
{ 221: } 286,
{ 222: } 286,
{ 223: } 286,
{ 224: } 286,
{ 225: } 286,
{ 226: } 286,
{ 227: } 286,
{ 228: } 286,
{ 229: } 286,
{ 230: } 286,
{ 231: } 286,
{ 232: } 286,
{ 233: } 286,
{ 234: } 288,
{ 235: } 288,
{ 236: } 288,
{ 237: } 288,
{ 238: } 292,
{ 239: } 292,
{ 240: } 299,
{ 241: } 303,
{ 242: } 304,
{ 243: } 308,
{ 244: } 309,
{ 245: } 310,
{ 246: } 311,
{ 247: } 311,
{ 248: } 311,
{ 249: } 311,
{ 250: } 311,
{ 251: } 315,
{ 252: } 317,
{ 253: } 317,
{ 254: } 318,
{ 255: } 318,
{ 256: } 318,
{ 257: } 318,
{ 258: } 318,
{ 259: } 318,
{ 260: } 318,
{ 261: } 318,
{ 262: } 318
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
{ 22: } ( len: 9; sym: -5 ),
{ 23: } ( len: 5; sym: -5 ),
{ 24: } ( len: 3; sym: -5 ),
{ 25: } ( len: 3; sym: -5 ),
{ 26: } ( len: 8; sym: -6 ),
{ 27: } ( len: 4; sym: -6 ),
{ 28: } ( len: 3; sym: -6 ),
{ 29: } ( len: 5; sym: -6 ),
{ 30: } ( len: 3; sym: -6 ),
{ 31: } ( len: 3; sym: -18 ),
{ 32: } ( len: 3; sym: -18 ),
{ 33: } ( len: 3; sym: -20 ),
{ 34: } ( len: 3; sym: -20 ),
{ 35: } ( len: 4; sym: -13 ),
{ 36: } ( len: 3; sym: -13 ),
{ 37: } ( len: 4; sym: -13 ),
{ 38: } ( len: 3; sym: -13 ),
{ 39: } ( len: 2; sym: -13 ),
{ 40: } ( len: 2; sym: -13 ),
{ 41: } ( len: 3; sym: -13 ),
{ 42: } ( len: 2; sym: -13 ),
{ 43: } ( len: 2; sym: -11 ),
{ 44: } ( len: 3; sym: -11 ),
{ 45: } ( len: 2; sym: -11 ),
{ 46: } ( len: 3; sym: -11 ),
{ 47: } ( len: 2; sym: -11 ),
{ 48: } ( len: 2; sym: -11 ),
{ 49: } ( len: 1; sym: -11 ),
{ 50: } ( len: 1; sym: -11 ),
{ 51: } ( len: 2; sym: -19 ),
{ 52: } ( len: 1; sym: -19 ),
{ 53: } ( len: 3; sym: -22 ),
{ 54: } ( len: 1; sym: -10 ),
{ 55: } ( len: 1; sym: -23 ),
{ 56: } ( len: 2; sym: -23 ),
{ 57: } ( len: 1; sym: -23 ),
{ 58: } ( len: 1; sym: -23 ),
{ 59: } ( len: 2; sym: -23 ),
{ 60: } ( len: 3; sym: -23 ),
{ 61: } ( len: 2; sym: -23 ),
{ 62: } ( len: 1; sym: -23 ),
{ 63: } ( len: 2; sym: -23 ),
{ 64: } ( len: 2; sym: -23 ),
{ 65: } ( len: 1; sym: -23 ),
{ 66: } ( len: 1; sym: -23 ),
{ 67: } ( len: 1; sym: -23 ),
{ 68: } ( len: 1; sym: -21 ),
{ 69: } ( len: 1; sym: -21 ),
{ 70: } ( len: 3; sym: -12 ),
{ 71: } ( len: 4; sym: -12 ),
{ 72: } ( len: 2; sym: -12 ),
{ 73: } ( len: 1; sym: -12 ),
{ 74: } ( len: 2; sym: -24 ),
{ 75: } ( len: 3; sym: -24 ),
{ 76: } ( len: 2; sym: -24 ),
{ 77: } ( len: 1; sym: -15 ),
{ 78: } ( len: 3; sym: -15 ),
{ 79: } ( len: 1; sym: -15 ),
{ 80: } ( len: 1; sym: -26 ),
{ 81: } ( len: 1; sym: -26 ),
{ 82: } ( len: 1; sym: -26 ),
{ 83: } ( len: 2; sym: -14 ),
{ 84: } ( len: 3; sym: -14 ),
{ 85: } ( len: 2; sym: -14 ),
{ 86: } ( len: 2; sym: -14 ),
{ 87: } ( len: 3; sym: -14 ),
{ 88: } ( len: 3; sym: -14 ),
{ 89: } ( len: 1; sym: -14 ),
{ 90: } ( len: 4; sym: -14 ),
{ 91: } ( len: 2; sym: -14 ),
{ 92: } ( len: 4; sym: -14 ),
{ 93: } ( len: 3; sym: -14 ),
{ 94: } ( len: 2; sym: -29 ),
{ 95: } ( len: 3; sym: -29 ),
{ 96: } ( len: 2; sym: -25 ),
{ 97: } ( len: 3; sym: -25 ),
{ 98: } ( len: 2; sym: -25 ),
{ 99: } ( len: 4; sym: -25 ),
{ 100: } ( len: 2; sym: -25 ),
{ 101: } ( len: 4; sym: -25 ),
{ 102: } ( len: 3; sym: -25 ),
{ 103: } ( len: 0; sym: -25 ),
{ 104: } ( len: 1; sym: -27 ),
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
{ 118: } ( len: 3; sym: -30 ),
{ 119: } ( len: 3; sym: -30 ),
{ 120: } ( len: 3; sym: -30 ),
{ 121: } ( len: 1; sym: -30 ),
{ 122: } ( len: 3; sym: -31 ),
{ 123: } ( len: 1; sym: -33 ),
{ 124: } ( len: 0; sym: -33 ),
{ 125: } ( len: 1; sym: -32 ),
{ 126: } ( len: 1; sym: -32 ),
{ 127: } ( len: 1; sym: -32 ),
{ 128: } ( len: 3; sym: -32 ),
{ 129: } ( len: 3; sym: -32 ),
{ 130: } ( len: 2; sym: -32 ),
{ 131: } ( len: 2; sym: -32 ),
{ 132: } ( len: 2; sym: -32 ),
{ 133: } ( len: 4; sym: -32 ),
{ 134: } ( len: 4; sym: -32 ),
{ 135: } ( len: 5; sym: -32 ),
{ 136: } ( len: 6; sym: -32 ),
{ 137: } ( len: 4; sym: -32 ),
{ 138: } ( len: 3; sym: -32 ),
{ 139: } ( len: 3; sym: -16 ),
{ 140: } ( len: 1; sym: -16 ),
{ 141: } ( len: 0; sym: -16 ),
{ 142: } ( len: 3; sym: -35 ),
{ 143: } ( len: 1; sym: -35 ),
{ 144: } ( len: 1; sym: -17 ),
{ 145: } ( len: 3; sym: -34 ),
{ 146: } ( len: 1; sym: -34 ),
{ 147: } ( len: 0; sym: -34 ),
{ 148: } ( len: 1; sym: -36 )
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
  line_no:=yylineno;
end;


var
  SS : string;
begin
{ Initialize }
  yydebug:=true;
  aktspace:='';
  block_type:=bt_no;
  IsExtern:=false;
{ Read commandline options }
  ProcessOptions;
  if not CompactMode then
   aktspace:='  ';
{ open input and output files }
  assign(yyinput, inputfilename);
  {$I-}
   reset(yyinput);
  {$I+}
  if ioresult<>0 then
   begin
     writeln('file ',inputfilename,' not found!');
     halt(1);
   end;
  assign(outfile, outputfilename);
  rewrite(outfile);
{ write unit header }
  if not includefile then
   begin
     writeln(outfile,'unit ',unitname,';');
     writeln(outfile,aktspace,'interface');
     writeln(outfile);
     writeln(outfile,'{ Automatically converted by H2Pas ',version,' from ',inputfilename,' }');
     writeln(outfile);
   end;
  if UseName then
   begin
     writeln(outfile,aktspace,'const');
     writeln(outfile,aktspace,'  External_library=''',libfilename,'''; {Setup as you need}');
     writeln(outfile);
   end;
  if UsePPointers then
   begin
     Writeln(outfile,aktspace,'{ Pointers to basic pascal types, inserted by h2pas conversion program.}');
     Writeln(outfile,aktspace,'Type');
     Writeln(outfile,aktspace,'  PLongint  = ^Longint;');
     Writeln(outfile,aktspace,'  PSmallInt = ^SmallInt;');
     Writeln(outfile,aktspace,'  PByte     = ^Byte;');
     Writeln(outfile,aktspace,'  PWord     = ^Word;');
     Writeln(outfile,aktspace,'  PDWord    = ^DWord;');
     Writeln(outfile,aktspace,'  PDouble   = ^Double;');
     Writeln(outfile);
   end;
  writeln(outfile,'{$PACKRECORDS C}');
  writeln(outfile);
{ Open tempfiles }
  Assign(extfile,'ext.tmp');
  rewrite(extfile);
  Assign(tempfile,'ext2.tmp');
  rewrite(tempfile);
{ Parse! }
  yyparse;
{ Write implementation if needed }
   if not(includefile) then
    begin
      writeln(outfile);
      writeln(outfile,aktspace,'implementation');
      writeln(outfile);
    end;
   { here we have a problem if a line is longer than 255 chars !! }
   reset(extfile);
   while not eof(extfile) do
    begin
      readln(extfile,SS);
      writeln(outfile,SS);
    end;
   { write end of file }
   writeln(outfile);
   if not(includefile) then
     writeln(outfile,'end.');
   { close and erase tempfiles }
   close(extfile);
   erase(extfile);
   close(outfile);
   close(tempfile);
   erase(tempfile);
end.

(*
 $Log$
 Revision 1.6  2000-04-01 14:16:31  peter
   * addition for another procvar style decl (not working correct yet)

 Revision 1.5  2000/03/28 06:56:31  michael
 + RemoveUNderscore now also does not add underscores to generated parameter names

 Revision 1.4  2000/03/27 21:39:20  peter
   + -S, -T, -c modes added
   * crash fixes
   * removed double opening of inputfile

 Revision 1.3  2000/02/09 16:44:15  peter
   * log truncated

 Revision 1.2  2000/01/07 16:46:05  daniel
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
