
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
     INT64_STR = 'int64';
     QWORD_STR = 'qword';
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
                      Writeln (outfile,copy(aktspace,1,length(aktspace)-2),'Const');
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
         if not(compactmode) then
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
         write(outfile,';cvar;external ''')
         else
         write(outfile,';cvar;export ''');
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
         if not(compactmode) then
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
         if not(compactmode) then
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
         if not(compactmode) then
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
         if not(compactmode) then
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
         if not(compactmode) then
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
         
         yyval:=new(presobject,init_id(INT64_STR));
         
       end;
  61 : begin
         
         yyval:=new(presobject,init_id(QWORD_STR));
         
       end;
  62 : begin
         
         yyval:=new(presobject,init_id(INT64_STR));
         
       end;
  63 : begin
         
         yyval:=new(presobject,init_id(QWORD_STR));
         
       end;
  64 : begin
         
         yyval:=new(presobject,init_id(UINT_STR));
         
       end;
  65 : begin
         
         yyval:=new(presobject,init_id(UINT_STR));
         
       end;
  66 : begin
         
         yyval:=new(presobject,init_id(UINT_STR));
         
       end;
  67 : begin
         
         yyval:=new(presobject,init_id(USHORT_STR));
         
       end;
  68 : begin
         
         yyval:=new(presobject,init_id(UCHAR_STR));
         
       end;
  69 : begin
         
         yyval:=new(presobject,init_no(t_void));
         
       end;
  70 : begin
         
         yyval:=new(presobject,init_id(SHORT_STR));
         
       end;
  71 : begin
         
         yyval:=new(presobject,init_id(CHAR_STR));
         
       end;
  72 : begin
         
         yyval:=yyv[yysp-0];
         
       end;
  73 : begin
         
         yyval:=yyv[yysp-0];
         tn:=yyval^.str;
         if removeunderscore and
         (length(tn)>1) and (tn[1]='_') then
         yyval^.setstr(Copy(tn,2,length(tn)-1));
         
       end;
  74 : begin
         
         yyval:=yyv[yysp-2];
         hp:=yyv[yysp-2];
         while assigned(hp^.next) do
         hp:=hp^.next;
         hp^.next:=new(presobject,init_one(t_declist,yyv[yysp-0]));
         
       end;
  75 : begin
         
         writeln(outfile,' in declarator_list *)');
         yyval:=yyv[yysp-0];
         yyerrok;
         
       end;
  76 : begin
         
         writeln(outfile,' in declarator_list *)');
         yyerrok;
         
       end;
  77 : begin
         
         yyval:=new(presobject,init_one(t_declist,yyv[yysp-0]));
         
       end;
  78 : begin
         
         yyval:=new(presobject,init_two(t_arg,yyv[yysp-1],yyv[yysp-0]));
         
       end;
  79 : begin
         
         hp:=new(presobject,init_one(t_pointerdef,yyv[yysp-2]));
         yyval:=new(presobject,init_two(t_arg,hp,yyv[yysp-0]));
         
       end;
  80 : begin
         
         yyval:=new(presobject,init_two(t_arg,yyv[yysp-1],yyv[yysp-0]));
         
       end;
  81 : begin
         
         yyval:=new(presobject,init_two(t_arglist,yyv[yysp-0],nil));
         
       end;
  82 : begin
         
         yyval:=new(presobject,init_two(t_arglist,yyv[yysp-2],nil));
         yyval^.next:=yyv[yysp-0];
         
       end;
  83 : begin
         
         yyval:=new(presobject,init_two(t_arglist,ellipsisarg,nil));
         (*** ELLIPSIS PROBLEM ***)
         
       end;
  84 : begin
         yyval:=new(presobject,init_id('far'));
       end;
  85 : begin
         yyval:=new(presobject,init_id('near'));
       end;
  86 : begin
         yyval:=new(presobject,init_id('huge'));
       end;
  87 : begin
         
         if not stripinfo then
         writeln(outfile,'(* Const before declarator ignored *)');
         yyval:=yyv[yysp-0];
         
       end;
  88 : begin
         
         if not stripinfo then
         writeln(outfile,aktspace,'(* ',yyv[yysp-2]^.p,' ignored *)');
         dispose(yyv[yysp-2],done);
         hp:=yyv[yysp-0];
         yyval:=hp;
         while assigned(hp^.p1) do
         hp:=hp^.p1;
         hp^.p1:=new(presobject,init_one(t_pointerdef,nil));
         
       end;
  89 : begin
         
         (* %prec PSTAR this was wrong!! *)
         hp:=yyv[yysp-0];
         yyval:=hp;
         while assigned(hp^.p1) do
         hp:=hp^.p1;
         hp^.p1:=new(presobject,init_one(t_pointerdef,nil));
         
       end;
  90 : begin
         
         hp:=yyv[yysp-0];
         yyval:=hp;
         while assigned(hp^.p1) do
         hp:=hp^.p1;
         hp^.p1:=new(presobject,init_one(t_addrdef,nil));
         
       end;
  91 : begin
         
         (*  size specifier supported *)
         hp:=new(presobject,init_one(t_size_specifier,yyv[yysp-0]));
         yyval:=new(presobject,init_three(t_dec,nil,yyv[yysp-2],hp));
         
       end;
  92 : begin
         
         if not stripinfo then
         writeln(outfile,'(* Warning : default value for ',yyv[yysp-2]^.p,' ignored *)');
         hp:=new(presobject,init_one(t_default_value,yyv[yysp-0]));
         yyval:=new(presobject,init_three(t_dec,nil,yyv[yysp-2],hp));
         
       end;
  93 : begin
         
         yyval:=new(presobject,init_two(t_dec,nil,yyv[yysp-0]));
         
       end;
  94 : begin
         
         hp:=yyv[yysp-3];
         yyval:=hp;
         while assigned(hp^.p1) do
         hp:=hp^.p1;
         hp^.p1:=new(presobject,init_two(t_procdef,nil,yyv[yysp-1]));
         
       end;
  95 : begin
         
         hp:=yyv[yysp-1];
         yyval:=hp;
         while assigned(hp^.p1) do
         hp:=hp^.p1;
         hp^.p1:=new(presobject,init_two(t_procdef,nil,nil));
         
       end;
  96 : begin
         
         hp:=yyv[yysp-3];
         yyval:=hp;
         while assigned(hp^.p1) do
         hp:=hp^.p1;
         hp^.p1:=new(presobject,init_two(t_arraydef,nil,yyv[yysp-1]));
         
       end;
  97 : begin
         yyval:=yyv[yysp-1]; 
       end;
  98 : begin
         yyval := yyv[yysp-1];
       end;
  99 : begin
         yyval := yyv[yysp-2];
       end;
 100 : begin
         
         if not stripinfo then
         writeln(outfile,'(* Const before abstract_declarator ignored *)');
         yyval:=yyv[yysp-0];
         
       end;
 101 : begin
         
         if not stripinfo then
         writeln(outfile,aktspace,'(* ',yyv[yysp-2]^.p,' ignored *)');
         dispose(yyv[yysp-2],done);
         hp:=yyv[yysp-0];
         yyval:=hp;
         while assigned(hp^.p1) do
         hp:=hp^.p1;
         hp^.p1:=new(presobject,init_one(t_pointerdef,nil));
         
       end;
 102 : begin
         
         hp:=yyv[yysp-0];
         yyval:=hp;
         while assigned(hp^.p1) do
         hp:=hp^.p1;
         hp^.p1:=new(presobject,init_one(t_pointerdef,nil));
         
       end;
 103 : begin
         
         hp:=yyv[yysp-3];
         yyval:=hp;
         while assigned(hp^.p1) do
         hp:=hp^.p1;
         hp^.p1:=new(presobject,init_two(t_procdef,nil,yyv[yysp-1]));
         
       end;
 104 : begin
         
         hp:=yyv[yysp-1];
         yyval:=hp;
         while assigned(hp^.p1) do
         hp:=hp^.p1;
         hp^.p1:=new(presobject,init_two(t_procdef,nil,nil));
         
       end;
 105 : begin
         
         hp:=yyv[yysp-3];
         yyval:=hp;
         while assigned(hp^.p1) do
         hp:=hp^.p1;
         hp^.p1:=new(presobject,init_two(t_arraydef,nil,yyv[yysp-1]));
         
       end;
 106 : begin
         yyval:=yyv[yysp-1]; 
       end;
 107 : begin
         
         yyval:=new(presobject,init_two(t_dec,nil,nil));
         
       end;
 108 : begin
         yyval:=yyv[yysp-0];
       end;
 109 : begin
         yyval:=new(presobject,init_bop(' = ',yyv[yysp-2],yyv[yysp-0]));
       end;
 110 : begin
         yyval:=new(presobject,init_bop(' <> ',yyv[yysp-2],yyv[yysp-0]));
       end;
 111 : begin
         yyval:=new(presobject,init_bop(' > ',yyv[yysp-2],yyv[yysp-0]));
       end;
 112 : begin
         yyval:=new(presobject,init_bop(' >= ',yyv[yysp-2],yyv[yysp-0]));
       end;
 113 : begin
         yyval:=new(presobject,init_bop(' < ',yyv[yysp-2],yyv[yysp-0]));
       end;
 114 : begin
         yyval:=new(presobject,init_bop(' <= ',yyv[yysp-2],yyv[yysp-0]));
       end;
 115 : begin
         yyval:=new(presobject,init_bop(' + ',yyv[yysp-2],yyv[yysp-0]));
       end;
 116 : begin
         yyval:=new(presobject,init_bop(' - ',yyv[yysp-2],yyv[yysp-0]));
       end;
 117 : begin
         yyval:=new(presobject,init_bop(' * ',yyv[yysp-2],yyv[yysp-0]));
       end;
 118 : begin
         yyval:=new(presobject,init_bop(' / ',yyv[yysp-2],yyv[yysp-0]));
       end;
 119 : begin
         yyval:=new(presobject,init_bop(' or ',yyv[yysp-2],yyv[yysp-0]));
       end;
 120 : begin
         yyval:=new(presobject,init_bop(' and ',yyv[yysp-2],yyv[yysp-0]));
       end;
 121 : begin
         yyval:=new(presobject,init_bop(' not ',yyv[yysp-2],yyv[yysp-0]));
       end;
 122 : begin
         yyval:=new(presobject,init_bop(' shl ',yyv[yysp-2],yyv[yysp-0]));
       end;
 123 : begin
         yyval:=new(presobject,init_bop(' shr ',yyv[yysp-2],yyv[yysp-0]));
       end;
 124 : begin
         yyv[yysp-0]^.p1:=yyv[yysp-2];
         yyval:=yyv[yysp-0];
         inc(if_nb);
         yyval^.p:=strpnew('if_local'+str(if_nb));
         
       end;
 125 : begin
         yyval:=yyv[yysp-0];
       end;
 126 : begin
         (* if A then B else C *)
         yyval:=new(presobject,init_three(t_ifexpr,nil,yyv[yysp-2],yyv[yysp-0]));
       end;
 127 : begin
         yyval:=yyv[yysp-0]; 
       end;
 128 : begin
         yyval:=nil;
       end;
 129 : begin
         
         yyval:=yyv[yysp-0];
         
       end;
 130 : begin
         
         (* remove L prefix for widestrings *)
         s:=act_token;
         if Win32headers and (s[1]='L') then
         delete(s,1,1);
         yyval:=new(presobject,init_id(''''+copy(s,2,length(s)-2)+''''));
         
       end;
 131 : begin
         
         yyval:=new(presobject,init_id(act_token));
         
       end;
 132 : begin
         
         yyval:=new(presobject,init_bop('.',yyv[yysp-2],yyv[yysp-0]));
         
       end;
 133 : begin
         
         yyval:=new(presobject,init_bop('^.',yyv[yysp-2],yyv[yysp-0]));
         
       end;
 134 : begin
         
         yyval:=new(presobject,init_preop('-',yyv[yysp-0]));
         
       end;
 135 : begin
         
         yyval:=new(presobject,init_preop('@',yyv[yysp-0]));
         
       end;
 136 : begin
         
         yyval:=new(presobject,init_preop(' not ',yyv[yysp-0]));
         
       end;
 137 : begin
         
         if assigned(yyv[yysp-0]) then
         yyval:=new(presobject,init_two(t_typespec,yyv[yysp-2],yyv[yysp-0]))
         else
         yyval:=yyv[yysp-2];
         
       end;
 138 : begin
         
         yyval:=new(presobject,init_two(t_typespec,yyv[yysp-2],yyv[yysp-0]));
         
       end;
 139 : begin
         
         hp:=new(presobject,init_one(t_pointerdef,yyv[yysp-3]));
         yyval:=new(presobject,init_two(t_typespec,hp,yyv[yysp-0]));
         
       end;
 140 : begin
         
         if not stripinfo then
         writeln(outfile,aktspace,'(* ',yyv[yysp-3]^.p,' ignored *)');
         dispose(yyv[yysp-3],done);
         write_type_specifier(outfile,yyv[yysp-4]);
         writeln(outfile,' ignored *)');
         hp:=new(presobject,init_one(t_pointerdef,yyv[yysp-4]));
         yyval:=new(presobject,init_two(t_typespec,hp,yyv[yysp-0]));
         
       end;
 141 : begin
         
         hp:=new(presobject,init_one(t_exprlist,yyv[yysp-3]));
         yyval:=new(presobject,init_three(t_funexprlist,hp,yyv[yysp-1],nil));
         
       end;
 142 : begin
         
         yyval:=yyv[yysp-1];
         
       end;
 143 : begin
         (*enum_element COMMA enum_list *)
         yyval:=yyv[yysp-2];
         yyval^.next:=yyv[yysp-0];
         
       end;
 144 : begin
         
         yyval:=yyv[yysp-0];
         
       end;
 145 : begin
         (* empty enum list *)
         yyval:=nil;
       end;
 146 : begin
         begin (*enum_element: dname _ASSIGN expr *)
         yyval:=new(presobject,init_two(t_enumlist,yyv[yysp-2],yyv[yysp-0]));
         end;
         
       end;
 147 : begin
         
         begin (*enum_element: dname*)
         yyval:=new(presobject,init_two(t_enumlist,yyv[yysp-0],nil));
         end;
         
       end;
 148 : begin
         
         if yyv[yysp-0]^.typ=t_funexprlist then
         yyval:=yyv[yysp-0]
         else
         yyval:=new(presobject,init_two(t_exprlist,yyv[yysp-0],nil));
         (* if here is a type specifier
         we know the return type *)
         if (yyv[yysp-0]^.typ=t_typespec) then
         yyval^.p3:=yyv[yysp-0]^.p1^.get_copy;
         
       end;
 149 : begin
         (*exprlist COMMA expr*)
         yyval:=yyv[yysp-2];
         yyv[yysp-2]^.next:=yyv[yysp-0];
         
       end;
 150 : begin
         
         yyval:=yyv[yysp-0];
         
       end;
 151 : begin
         (* empty expression list *)
         yyval:=nil; 
       end;
 152 : begin
         
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

yynacts   = 1981;
yyngotos  = 318;
yynstates = 267;
yynrules  = 152;

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
  ( sym: 256; act: -66 ),
  ( sym: 261; act: -66 ),
  ( sym: 262; act: -66 ),
  ( sym: 263; act: -66 ),
  ( sym: 264; act: -66 ),
  ( sym: 271; act: -66 ),
  ( sym: 281; act: -66 ),
  ( sym: 282; act: -66 ),
  ( sym: 283; act: -66 ),
  ( sym: 284; act: -66 ),
  ( sym: 289; act: -66 ),
  ( sym: 290; act: -66 ),
  ( sym: 291; act: -66 ),
  ( sym: 292; act: -66 ),
  ( sym: 293; act: -66 ),
  ( sym: 294; act: -66 ),
  ( sym: 295; act: -66 ),
  ( sym: 308; act: -66 ),
  ( sym: 313; act: -66 ),
{ 26: }
  ( sym: 276; act: 61 ),
  ( sym: 277; act: 62 ),
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
  ( sym: 260; act: 64 ),
  ( sym: 286; act: 65 ),
{ 35: }
  ( sym: 262; act: 67 ),
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
  ( sym: 260; act: 68 ),
  ( sym: 256; act: -73 ),
  ( sym: 262; act: -73 ),
  ( sym: 271; act: -73 ),
  ( sym: 281; act: -73 ),
  ( sym: 282; act: -73 ),
  ( sym: 283; act: -73 ),
  ( sym: 284; act: -73 ),
  ( sym: 289; act: -73 ),
  ( sym: 290; act: -73 ),
  ( sym: 291; act: -73 ),
  ( sym: 292; act: -73 ),
  ( sym: 293; act: -73 ),
  ( sym: 294; act: -73 ),
  ( sym: 295; act: -73 ),
  ( sym: 308; act: -73 ),
  ( sym: 313; act: -73 ),
{ 37: }
  ( sym: 256; act: 51 ),
  ( sym: 266; act: 52 ),
  ( sym: 271; act: 23 ),
{ 38: }
  ( sym: 262; act: 70 ),
  ( sym: 286; act: 71 ),
  ( sym: 287; act: 72 ),
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
  ( sym: 256; act: 80 ),
  ( sym: 262; act: 81 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 82 ),
  ( sym: 282; act: 83 ),
  ( sym: 283; act: 84 ),
  ( sym: 284; act: 85 ),
  ( sym: 308; act: 86 ),
  ( sym: 313; act: 87 ),
{ 43: }
{ 44: }
{ 45: }
{ 46: }
{ 47: }
{ 48: }
{ 49: }
{ 50: }
  ( sym: 297; act: 88 ),
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
  ( sym: 297; act: 93 ),
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
  ( sym: 267; act: -145 ),
{ 57: }
{ 58: }
  ( sym: 276; act: 98 ),
  ( sym: 277; act: 99 ),
  ( sym: 256; act: -65 ),
  ( sym: 261; act: -65 ),
  ( sym: 262; act: -65 ),
  ( sym: 263; act: -65 ),
  ( sym: 264; act: -65 ),
  ( sym: 271; act: -65 ),
  ( sym: 281; act: -65 ),
  ( sym: 282; act: -65 ),
  ( sym: 283; act: -65 ),
  ( sym: 284; act: -65 ),
  ( sym: 289; act: -65 ),
  ( sym: 290; act: -65 ),
  ( sym: 291; act: -65 ),
  ( sym: 292; act: -65 ),
  ( sym: 293; act: -65 ),
  ( sym: 294; act: -65 ),
  ( sym: 295; act: -65 ),
  ( sym: 308; act: -65 ),
  ( sym: 313; act: -65 ),
{ 59: }
{ 60: }
{ 61: }
  ( sym: 277; act: 100 ),
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
{ 62: }
{ 63: }
{ 64: }
{ 65: }
{ 66: }
  ( sym: 256; act: 80 ),
  ( sym: 262; act: 81 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 82 ),
  ( sym: 282; act: 83 ),
  ( sym: 283; act: 84 ),
  ( sym: 284; act: 85 ),
  ( sym: 308; act: 86 ),
  ( sym: 313; act: 87 ),
{ 67: }
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
{ 68: }
{ 69: }
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
{ 70: }
  ( sym: 271; act: 23 ),
  ( sym: 263; act: -145 ),
{ 71: }
{ 72: }
  ( sym: 262; act: 108 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 109 ),
  ( sym: 273; act: 110 ),
  ( sym: 286; act: 111 ),
  ( sym: 308; act: 112 ),
  ( sym: 310; act: 113 ),
  ( sym: 315; act: 114 ),
{ 73: }
  ( sym: 297; act: 115 ),
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
{ 74: }
  ( sym: 297; act: 116 ),
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
{ 75: }
{ 76: }
  ( sym: 313; act: 117 ),
{ 77: }
  ( sym: 262; act: 119 ),
  ( sym: 264; act: 120 ),
  ( sym: 260; act: -77 ),
  ( sym: 261; act: -77 ),
  ( sym: 296; act: -77 ),
{ 78: }
  ( sym: 261; act: 122 ),
  ( sym: 296; act: 123 ),
  ( sym: 260; act: -18 ),
{ 79: }
  ( sym: 259; act: 125 ),
  ( sym: 260; act: -93 ),
  ( sym: 261; act: -93 ),
  ( sym: 262; act: -93 ),
  ( sym: 263; act: -93 ),
  ( sym: 264; act: -93 ),
  ( sym: 296; act: -93 ),
{ 80: }
{ 81: }
  ( sym: 262; act: 81 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 82 ),
  ( sym: 282; act: 83 ),
  ( sym: 283; act: 84 ),
  ( sym: 284; act: 85 ),
  ( sym: 308; act: 86 ),
  ( sym: 313; act: 87 ),
{ 82: }
  ( sym: 262; act: 81 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 82 ),
  ( sym: 282; act: 83 ),
  ( sym: 283; act: 84 ),
  ( sym: 284; act: 85 ),
  ( sym: 308; act: 86 ),
  ( sym: 313; act: 87 ),
{ 83: }
{ 84: }
{ 85: }
{ 86: }
  ( sym: 262; act: 81 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 82 ),
  ( sym: 282; act: 83 ),
  ( sym: 283; act: 84 ),
  ( sym: 284; act: 85 ),
  ( sym: 308; act: 86 ),
  ( sym: 313; act: 87 ),
{ 87: }
  ( sym: 262; act: 81 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 82 ),
  ( sym: 282; act: 83 ),
  ( sym: 283; act: 84 ),
  ( sym: 284; act: 85 ),
  ( sym: 308; act: 86 ),
  ( sym: 313; act: 87 ),
{ 88: }
{ 89: }
  ( sym: 267; act: 131 ),
{ 90: }
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
{ 91: }
  ( sym: 267; act: 133 ),
{ 92: }
  ( sym: 256; act: 80 ),
  ( sym: 262; act: 81 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 82 ),
  ( sym: 282; act: 83 ),
  ( sym: 283; act: 84 ),
  ( sym: 284; act: 85 ),
  ( sym: 308; act: 86 ),
  ( sym: 313; act: 87 ),
{ 93: }
{ 94: }
  ( sym: 267; act: 135 ),
{ 95: }
  ( sym: 261; act: 136 ),
  ( sym: 263; act: -144 ),
  ( sym: 267; act: -144 ),
{ 96: }
  ( sym: 267; act: 137 ),
{ 97: }
  ( sym: 285; act: 138 ),
  ( sym: 261; act: -147 ),
  ( sym: 263; act: -147 ),
  ( sym: 267; act: -147 ),
{ 98: }
  ( sym: 277; act: 139 ),
  ( sym: 256; act: -63 ),
  ( sym: 261; act: -63 ),
  ( sym: 262; act: -63 ),
  ( sym: 263; act: -63 ),
  ( sym: 264; act: -63 ),
  ( sym: 271; act: -63 ),
  ( sym: 281; act: -63 ),
  ( sym: 282; act: -63 ),
  ( sym: 283; act: -63 ),
  ( sym: 284; act: -63 ),
  ( sym: 289; act: -63 ),
  ( sym: 290; act: -63 ),
  ( sym: 291; act: -63 ),
  ( sym: 292; act: -63 ),
  ( sym: 293; act: -63 ),
  ( sym: 294; act: -63 ),
  ( sym: 295; act: -63 ),
  ( sym: 308; act: -63 ),
  ( sym: 313; act: -63 ),
{ 99: }
{ 100: }
{ 101: }
  ( sym: 260; act: 140 ),
  ( sym: 261; act: 122 ),
{ 102: }
  ( sym: 262; act: 81 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 82 ),
  ( sym: 282; act: 83 ),
  ( sym: 283; act: 84 ),
  ( sym: 284; act: 85 ),
  ( sym: 308; act: 86 ),
  ( sym: 313; act: 87 ),
{ 103: }
  ( sym: 260; act: 142 ),
{ 104: }
  ( sym: 263; act: 143 ),
{ 105: }
  ( sym: 318; act: 144 ),
  ( sym: 319; act: 145 ),
  ( sym: 286; act: -148 ),
{ 106: }
  ( sym: 286; act: 146 ),
{ 107: }
  ( sym: 262; act: 147 ),
  ( sym: 259; act: -129 ),
  ( sym: 260; act: -129 ),
  ( sym: 261; act: -129 ),
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
  ( sym: 318; act: -129 ),
  ( sym: 319; act: -129 ),
{ 108: }
  ( sym: 262; act: 108 ),
  ( sym: 268; act: 20 ),
  ( sym: 269; act: 21 ),
  ( sym: 270; act: 22 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 109 ),
  ( sym: 273; act: 110 ),
  ( sym: 274; act: 24 ),
  ( sym: 275; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 277; act: 27 ),
  ( sym: 278; act: 28 ),
  ( sym: 279; act: 29 ),
  ( sym: 280; act: 30 ),
  ( sym: 281; act: 31 ),
  ( sym: 308; act: 112 ),
  ( sym: 310; act: 113 ),
  ( sym: 315; act: 114 ),
{ 109: }
{ 110: }
{ 111: }
{ 112: }
  ( sym: 262; act: 108 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 109 ),
  ( sym: 273; act: 110 ),
  ( sym: 308; act: 112 ),
  ( sym: 310; act: 113 ),
  ( sym: 315; act: 114 ),
{ 113: }
  ( sym: 262; act: 108 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 109 ),
  ( sym: 273; act: 110 ),
  ( sym: 308; act: 112 ),
  ( sym: 310; act: 113 ),
  ( sym: 315; act: 114 ),
{ 114: }
  ( sym: 262; act: 108 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 109 ),
  ( sym: 273; act: 110 ),
  ( sym: 308; act: 112 ),
  ( sym: 310; act: 113 ),
  ( sym: 315; act: 114 ),
{ 115: }
{ 116: }
{ 117: }
  ( sym: 262; act: 81 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 82 ),
  ( sym: 282; act: 83 ),
  ( sym: 283; act: 84 ),
  ( sym: 284; act: 85 ),
  ( sym: 308; act: 86 ),
  ( sym: 313; act: 87 ),
{ 118: }
{ 119: }
  ( sym: 263; act: 160 ),
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
  ( sym: 280; act: 161 ),
  ( sym: 281; act: 31 ),
  ( sym: 298; act: 162 ),
{ 120: }
  ( sym: 262; act: 108 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 109 ),
  ( sym: 273; act: 110 ),
  ( sym: 308; act: 112 ),
  ( sym: 310; act: 113 ),
  ( sym: 315; act: 114 ),
{ 121: }
  ( sym: 260; act: 165 ),
{ 122: }
  ( sym: 262; act: 81 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 82 ),
  ( sym: 282; act: 83 ),
  ( sym: 283; act: 84 ),
  ( sym: 284; act: 85 ),
  ( sym: 308; act: 86 ),
  ( sym: 313; act: 87 ),
{ 123: }
  ( sym: 262; act: 167 ),
{ 124: }
  ( sym: 262; act: 108 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 109 ),
  ( sym: 273; act: 110 ),
  ( sym: 308; act: 112 ),
  ( sym: 310; act: 113 ),
  ( sym: 315; act: 114 ),
{ 125: }
  ( sym: 262; act: 108 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 109 ),
  ( sym: 273; act: 110 ),
  ( sym: 308; act: 112 ),
  ( sym: 310; act: 113 ),
  ( sym: 315; act: 114 ),
{ 126: }
  ( sym: 261; act: 170 ),
  ( sym: 260; act: -76 ),
  ( sym: 296; act: -76 ),
{ 127: }
  ( sym: 262; act: 119 ),
  ( sym: 263; act: 171 ),
  ( sym: 264; act: 120 ),
{ 128: }
  ( sym: 262; act: 119 ),
  ( sym: 264; act: 120 ),
  ( sym: 260; act: -87 ),
  ( sym: 261; act: -87 ),
  ( sym: 263; act: -87 ),
  ( sym: 296; act: -87 ),
{ 129: }
  ( sym: 264; act: 120 ),
  ( sym: 260; act: -90 ),
  ( sym: 261; act: -90 ),
  ( sym: 262; act: -90 ),
  ( sym: 263; act: -90 ),
  ( sym: 296; act: -90 ),
{ 130: }
  ( sym: 262; act: 119 ),
  ( sym: 264; act: 120 ),
  ( sym: 260; act: -89 ),
  ( sym: 261; act: -89 ),
  ( sym: 263; act: -89 ),
  ( sym: 296; act: -89 ),
{ 131: }
{ 132: }
{ 133: }
{ 134: }
  ( sym: 260; act: 172 ),
  ( sym: 261; act: 122 ),
{ 135: }
{ 136: }
  ( sym: 271; act: 23 ),
  ( sym: 263; act: -145 ),
  ( sym: 267; act: -145 ),
{ 137: }
{ 138: }
  ( sym: 262; act: 108 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 109 ),
  ( sym: 273; act: 110 ),
  ( sym: 308; act: 112 ),
  ( sym: 310; act: 113 ),
  ( sym: 315; act: 114 ),
{ 139: }
{ 140: }
{ 141: }
  ( sym: 262; act: 119 ),
  ( sym: 263; act: 175 ),
  ( sym: 264; act: 120 ),
{ 142: }
{ 143: }
  ( sym: 287; act: 176 ),
{ 144: }
  ( sym: 262; act: 108 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 109 ),
  ( sym: 273; act: 110 ),
  ( sym: 308; act: 112 ),
  ( sym: 310; act: 113 ),
  ( sym: 315; act: 114 ),
{ 145: }
  ( sym: 262; act: 108 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 109 ),
  ( sym: 273; act: 110 ),
  ( sym: 308; act: 112 ),
  ( sym: 310; act: 113 ),
  ( sym: 315; act: 114 ),
{ 146: }
{ 147: }
  ( sym: 262; act: 108 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 109 ),
  ( sym: 273; act: 110 ),
  ( sym: 308; act: 112 ),
  ( sym: 310; act: 113 ),
  ( sym: 315; act: 114 ),
  ( sym: 263; act: -151 ),
{ 148: }
  ( sym: 318; act: 144 ),
  ( sym: 319; act: 145 ),
  ( sym: 259; act: -125 ),
  ( sym: 260; act: -125 ),
  ( sym: 261; act: -125 ),
  ( sym: 262; act: -125 ),
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
{ 149: }
  ( sym: 263; act: 182 ),
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
  ( sym: 311; act: -108 ),
  ( sym: 312; act: -108 ),
  ( sym: 313; act: -108 ),
  ( sym: 314; act: -108 ),
  ( sym: 315; act: -108 ),
{ 150: }
  ( sym: 300; act: 183 ),
  ( sym: 301; act: 184 ),
  ( sym: 302; act: 185 ),
  ( sym: 303; act: 186 ),
  ( sym: 304; act: 187 ),
  ( sym: 305; act: 188 ),
  ( sym: 306; act: 189 ),
  ( sym: 307; act: 190 ),
  ( sym: 308; act: 191 ),
  ( sym: 309; act: 192 ),
  ( sym: 310; act: 193 ),
  ( sym: 311; act: 194 ),
  ( sym: 312; act: 195 ),
  ( sym: 313; act: 196 ),
  ( sym: 314; act: 197 ),
  ( sym: 315; act: 198 ),
{ 151: }
  ( sym: 263; act: 200 ),
  ( sym: 282; act: 83 ),
  ( sym: 283; act: 84 ),
  ( sym: 284; act: 85 ),
  ( sym: 313; act: 201 ),
{ 152: }
  ( sym: 262; act: 147 ),
  ( sym: 263; act: 202 ),
  ( sym: 282; act: -73 ),
  ( sym: 283; act: -73 ),
  ( sym: 284; act: -73 ),
  ( sym: 313; act: -73 ),
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
  ( sym: 314; act: -129 ),
  ( sym: 315; act: -129 ),
  ( sym: 318; act: -129 ),
  ( sym: 319; act: -129 ),
{ 153: }
  ( sym: 318; act: 144 ),
  ( sym: 319; act: 145 ),
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
{ 154: }
  ( sym: 318; act: 144 ),
  ( sym: 319; act: 145 ),
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
{ 155: }
  ( sym: 318; act: 144 ),
  ( sym: 319; act: 145 ),
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
  ( sym: 315; act: -136 ),
{ 156: }
  ( sym: 262; act: 119 ),
  ( sym: 264; act: 120 ),
  ( sym: 260; act: -88 ),
  ( sym: 261; act: -88 ),
  ( sym: 263; act: -88 ),
  ( sym: 296; act: -88 ),
{ 157: }
  ( sym: 261; act: 203 ),
  ( sym: 263; act: -81 ),
{ 158: }
  ( sym: 263; act: 204 ),
{ 159: }
  ( sym: 262; act: 208 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 209 ),
  ( sym: 282; act: 83 ),
  ( sym: 283; act: 84 ),
  ( sym: 284; act: 85 ),
  ( sym: 308; act: 86 ),
  ( sym: 313; act: 210 ),
  ( sym: 261; act: -107 ),
  ( sym: 263; act: -107 ),
  ( sym: 264; act: -107 ),
{ 160: }
{ 161: }
  ( sym: 263; act: 211 ),
  ( sym: 261; act: -69 ),
  ( sym: 262; act: -69 ),
  ( sym: 264; act: -69 ),
  ( sym: 271; act: -69 ),
  ( sym: 281; act: -69 ),
  ( sym: 282; act: -69 ),
  ( sym: 283; act: -69 ),
  ( sym: 284; act: -69 ),
  ( sym: 308; act: -69 ),
  ( sym: 313; act: -69 ),
{ 162: }
{ 163: }
{ 164: }
  ( sym: 265; act: 212 ),
  ( sym: 300; act: 183 ),
  ( sym: 301; act: 184 ),
  ( sym: 302; act: 185 ),
  ( sym: 303; act: 186 ),
  ( sym: 304; act: 187 ),
  ( sym: 305; act: 188 ),
  ( sym: 306; act: 189 ),
  ( sym: 307; act: 190 ),
  ( sym: 308; act: 191 ),
  ( sym: 309; act: 192 ),
  ( sym: 310; act: 193 ),
  ( sym: 311; act: 194 ),
  ( sym: 312; act: 195 ),
  ( sym: 313; act: 196 ),
  ( sym: 314; act: 197 ),
  ( sym: 315; act: 198 ),
{ 165: }
{ 166: }
  ( sym: 262; act: 119 ),
  ( sym: 264; act: 120 ),
  ( sym: 260; act: -74 ),
  ( sym: 261; act: -74 ),
  ( sym: 296; act: -74 ),
{ 167: }
  ( sym: 271; act: 23 ),
{ 168: }
  ( sym: 300; act: 183 ),
  ( sym: 301; act: 184 ),
  ( sym: 302; act: 185 ),
  ( sym: 303; act: 186 ),
  ( sym: 304; act: 187 ),
  ( sym: 305; act: 188 ),
  ( sym: 306; act: 189 ),
  ( sym: 307; act: 190 ),
  ( sym: 308; act: 191 ),
  ( sym: 309; act: 192 ),
  ( sym: 310; act: 193 ),
  ( sym: 311; act: 194 ),
  ( sym: 312; act: 195 ),
  ( sym: 313; act: 196 ),
  ( sym: 314; act: 197 ),
  ( sym: 315; act: 198 ),
  ( sym: 260; act: -92 ),
  ( sym: 261; act: -92 ),
  ( sym: 262; act: -92 ),
  ( sym: 263; act: -92 ),
  ( sym: 264; act: -92 ),
  ( sym: 296; act: -92 ),
{ 169: }
  ( sym: 300; act: 183 ),
  ( sym: 301; act: 184 ),
  ( sym: 302; act: 185 ),
  ( sym: 303; act: 186 ),
  ( sym: 304; act: 187 ),
  ( sym: 305; act: 188 ),
  ( sym: 306; act: 189 ),
  ( sym: 307; act: 190 ),
  ( sym: 308; act: 191 ),
  ( sym: 309; act: 192 ),
  ( sym: 310; act: 193 ),
  ( sym: 311; act: 194 ),
  ( sym: 312; act: 195 ),
  ( sym: 313; act: 196 ),
  ( sym: 314; act: 197 ),
  ( sym: 315; act: 198 ),
  ( sym: 260; act: -91 ),
  ( sym: 261; act: -91 ),
  ( sym: 262; act: -91 ),
  ( sym: 263; act: -91 ),
  ( sym: 264; act: -91 ),
  ( sym: 296; act: -91 ),
{ 170: }
  ( sym: 256; act: 80 ),
  ( sym: 262; act: 81 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 82 ),
  ( sym: 282; act: 83 ),
  ( sym: 283; act: 84 ),
  ( sym: 284; act: 85 ),
  ( sym: 308; act: 86 ),
  ( sym: 313; act: 87 ),
{ 171: }
{ 172: }
{ 173: }
{ 174: }
  ( sym: 300; act: 183 ),
  ( sym: 301; act: 184 ),
  ( sym: 302; act: 185 ),
  ( sym: 303; act: 186 ),
  ( sym: 304; act: 187 ),
  ( sym: 305; act: 188 ),
  ( sym: 306; act: 189 ),
  ( sym: 307; act: 190 ),
  ( sym: 308; act: 191 ),
  ( sym: 309; act: 192 ),
  ( sym: 310; act: 193 ),
  ( sym: 311; act: 194 ),
  ( sym: 312; act: 195 ),
  ( sym: 313; act: 196 ),
  ( sym: 314; act: 197 ),
  ( sym: 315; act: 198 ),
  ( sym: 261; act: -146 ),
  ( sym: 263; act: -146 ),
  ( sym: 267; act: -146 ),
{ 175: }
  ( sym: 262; act: 215 ),
{ 176: }
  ( sym: 262; act: 108 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 109 ),
  ( sym: 273; act: 110 ),
  ( sym: 308; act: 112 ),
  ( sym: 310; act: 113 ),
  ( sym: 315; act: 114 ),
{ 177: }
{ 178: }
{ 179: }
  ( sym: 261; act: 217 ),
  ( sym: 263; act: -150 ),
{ 180: }
  ( sym: 263; act: 218 ),
{ 181: }
  ( sym: 300; act: 183 ),
  ( sym: 301; act: 184 ),
  ( sym: 302; act: 185 ),
  ( sym: 303; act: 186 ),
  ( sym: 304; act: 187 ),
  ( sym: 305; act: 188 ),
  ( sym: 306; act: 189 ),
  ( sym: 307; act: 190 ),
  ( sym: 308; act: 191 ),
  ( sym: 309; act: 192 ),
  ( sym: 310; act: 193 ),
  ( sym: 311; act: 194 ),
  ( sym: 312; act: 195 ),
  ( sym: 313; act: 196 ),
  ( sym: 314; act: 197 ),
  ( sym: 315; act: 198 ),
  ( sym: 261; act: -152 ),
  ( sym: 263; act: -152 ),
{ 182: }
{ 183: }
  ( sym: 262; act: 108 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 109 ),
  ( sym: 273; act: 110 ),
  ( sym: 308; act: 112 ),
  ( sym: 310; act: 113 ),
  ( sym: 315; act: 114 ),
{ 184: }
  ( sym: 262; act: 108 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 109 ),
  ( sym: 273; act: 110 ),
  ( sym: 308; act: 112 ),
  ( sym: 310; act: 113 ),
  ( sym: 315; act: 114 ),
{ 185: }
  ( sym: 262; act: 108 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 109 ),
  ( sym: 273; act: 110 ),
  ( sym: 308; act: 112 ),
  ( sym: 310; act: 113 ),
  ( sym: 315; act: 114 ),
{ 186: }
  ( sym: 262; act: 108 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 109 ),
  ( sym: 273; act: 110 ),
  ( sym: 308; act: 112 ),
  ( sym: 310; act: 113 ),
  ( sym: 315; act: 114 ),
{ 187: }
  ( sym: 262; act: 108 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 109 ),
  ( sym: 273; act: 110 ),
  ( sym: 308; act: 112 ),
  ( sym: 310; act: 113 ),
  ( sym: 315; act: 114 ),
{ 188: }
  ( sym: 262; act: 108 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 109 ),
  ( sym: 273; act: 110 ),
  ( sym: 308; act: 112 ),
  ( sym: 310; act: 113 ),
  ( sym: 315; act: 114 ),
{ 189: }
  ( sym: 262; act: 108 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 109 ),
  ( sym: 273; act: 110 ),
  ( sym: 308; act: 112 ),
  ( sym: 310; act: 113 ),
  ( sym: 315; act: 114 ),
{ 190: }
  ( sym: 262; act: 108 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 109 ),
  ( sym: 273; act: 110 ),
  ( sym: 308; act: 112 ),
  ( sym: 310; act: 113 ),
  ( sym: 315; act: 114 ),
{ 191: }
  ( sym: 262; act: 108 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 109 ),
  ( sym: 273; act: 110 ),
  ( sym: 308; act: 112 ),
  ( sym: 310; act: 113 ),
  ( sym: 315; act: 114 ),
{ 192: }
  ( sym: 262; act: 108 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 109 ),
  ( sym: 273; act: 110 ),
  ( sym: 308; act: 112 ),
  ( sym: 310; act: 113 ),
  ( sym: 315; act: 114 ),
{ 193: }
  ( sym: 262; act: 108 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 109 ),
  ( sym: 273; act: 110 ),
  ( sym: 308; act: 112 ),
  ( sym: 310; act: 113 ),
  ( sym: 315; act: 114 ),
{ 194: }
  ( sym: 262; act: 108 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 109 ),
  ( sym: 273; act: 110 ),
  ( sym: 308; act: 112 ),
  ( sym: 310; act: 113 ),
  ( sym: 315; act: 114 ),
{ 195: }
  ( sym: 262; act: 108 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 109 ),
  ( sym: 273; act: 110 ),
  ( sym: 308; act: 112 ),
  ( sym: 310; act: 113 ),
  ( sym: 315; act: 114 ),
{ 196: }
  ( sym: 262; act: 108 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 109 ),
  ( sym: 273; act: 110 ),
  ( sym: 308; act: 112 ),
  ( sym: 310; act: 113 ),
  ( sym: 315; act: 114 ),
{ 197: }
  ( sym: 262; act: 108 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 109 ),
  ( sym: 273; act: 110 ),
  ( sym: 308; act: 112 ),
  ( sym: 310; act: 113 ),
  ( sym: 315; act: 114 ),
{ 198: }
  ( sym: 262; act: 108 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 109 ),
  ( sym: 273; act: 110 ),
  ( sym: 308; act: 112 ),
  ( sym: 310; act: 113 ),
  ( sym: 315; act: 114 ),
{ 199: }
  ( sym: 313; act: 236 ),
{ 200: }
  ( sym: 262; act: 108 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 109 ),
  ( sym: 273; act: 110 ),
  ( sym: 308; act: 112 ),
  ( sym: 310; act: 113 ),
  ( sym: 315; act: 114 ),
{ 201: }
  ( sym: 263; act: 238 ),
{ 202: }
  ( sym: 262; act: 108 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 109 ),
  ( sym: 273; act: 110 ),
  ( sym: 308; act: 112 ),
  ( sym: 310; act: 113 ),
  ( sym: 315; act: 114 ),
  ( sym: 259; act: -128 ),
  ( sym: 260; act: -128 ),
  ( sym: 261; act: -128 ),
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
  ( sym: 309; act: -128 ),
  ( sym: 311; act: -128 ),
  ( sym: 312; act: -128 ),
  ( sym: 313; act: -128 ),
  ( sym: 314; act: -128 ),
  ( sym: 318; act: -128 ),
  ( sym: 319; act: -128 ),
{ 203: }
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
  ( sym: 298; act: 162 ),
{ 204: }
{ 205: }
  ( sym: 313; act: 242 ),
{ 206: }
  ( sym: 262; act: 244 ),
  ( sym: 264; act: 245 ),
  ( sym: 261; act: -80 ),
  ( sym: 263; act: -80 ),
{ 207: }
  ( sym: 262; act: 119 ),
  ( sym: 264; act: 120 ),
  ( sym: 261; act: -78 ),
  ( sym: 263; act: -78 ),
{ 208: }
  ( sym: 262; act: 208 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 209 ),
  ( sym: 282; act: 83 ),
  ( sym: 283; act: 84 ),
  ( sym: 284; act: 85 ),
  ( sym: 308; act: 86 ),
  ( sym: 313; act: 247 ),
  ( sym: 263; act: -107 ),
  ( sym: 264; act: -107 ),
{ 209: }
  ( sym: 262; act: 208 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 209 ),
  ( sym: 282; act: 83 ),
  ( sym: 283; act: 84 ),
  ( sym: 284; act: 85 ),
  ( sym: 308; act: 86 ),
  ( sym: 313; act: 247 ),
  ( sym: 261; act: -107 ),
  ( sym: 263; act: -107 ),
  ( sym: 264; act: -107 ),
{ 210: }
  ( sym: 262; act: 208 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 209 ),
  ( sym: 282; act: 83 ),
  ( sym: 283; act: 84 ),
  ( sym: 284; act: 85 ),
  ( sym: 308; act: 86 ),
  ( sym: 313; act: 247 ),
  ( sym: 261; act: -107 ),
  ( sym: 263; act: -107 ),
  ( sym: 264; act: -107 ),
{ 211: }
{ 212: }
{ 213: }
  ( sym: 263; act: 251 ),
{ 214: }
{ 215: }
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
  ( sym: 298; act: 162 ),
{ 216: }
  ( sym: 286; act: 253 ),
{ 217: }
  ( sym: 262; act: 108 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 109 ),
  ( sym: 273; act: 110 ),
  ( sym: 308; act: 112 ),
  ( sym: 310; act: 113 ),
  ( sym: 315; act: 114 ),
  ( sym: 263; act: -151 ),
{ 218: }
{ 219: }
  ( sym: 306; act: 189 ),
  ( sym: 307; act: 190 ),
  ( sym: 308; act: 191 ),
  ( sym: 309; act: 192 ),
  ( sym: 310; act: 193 ),
  ( sym: 311; act: 194 ),
  ( sym: 312; act: 195 ),
  ( sym: 313; act: 196 ),
  ( sym: 314; act: 197 ),
  ( sym: 315; act: 198 ),
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
{ 220: }
  ( sym: 306; act: 189 ),
  ( sym: 307; act: 190 ),
  ( sym: 308; act: 191 ),
  ( sym: 309; act: 192 ),
  ( sym: 310; act: 193 ),
  ( sym: 311; act: 194 ),
  ( sym: 312; act: 195 ),
  ( sym: 313; act: 196 ),
  ( sym: 314; act: 197 ),
  ( sym: 315; act: 198 ),
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
  ( sym: 306; act: 189 ),
  ( sym: 307; act: 190 ),
  ( sym: 308; act: 191 ),
  ( sym: 309; act: 192 ),
  ( sym: 310; act: 193 ),
  ( sym: 311; act: 194 ),
  ( sym: 312; act: 195 ),
  ( sym: 313; act: 196 ),
  ( sym: 314; act: 197 ),
  ( sym: 315; act: 198 ),
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
  ( sym: 318; act: -111 ),
  ( sym: 319; act: -111 ),
{ 222: }
  ( sym: 306; act: 189 ),
  ( sym: 307; act: 190 ),
  ( sym: 308; act: 191 ),
  ( sym: 309; act: 192 ),
  ( sym: 310; act: 193 ),
  ( sym: 311; act: 194 ),
  ( sym: 312; act: 195 ),
  ( sym: 313; act: 196 ),
  ( sym: 314; act: 197 ),
  ( sym: 315; act: 198 ),
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
  ( sym: 318; act: -113 ),
  ( sym: 319; act: -113 ),
{ 223: }
  ( sym: 306; act: 189 ),
  ( sym: 307; act: 190 ),
  ( sym: 308; act: 191 ),
  ( sym: 309; act: 192 ),
  ( sym: 310; act: 193 ),
  ( sym: 311; act: 194 ),
  ( sym: 312; act: 195 ),
  ( sym: 313; act: 196 ),
  ( sym: 314; act: 197 ),
  ( sym: 315; act: 198 ),
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
  ( sym: 318; act: -112 ),
  ( sym: 319; act: -112 ),
{ 224: }
  ( sym: 306; act: 189 ),
  ( sym: 307; act: 190 ),
  ( sym: 308; act: 191 ),
  ( sym: 309; act: 192 ),
  ( sym: 310; act: 193 ),
  ( sym: 311; act: 194 ),
  ( sym: 312; act: 195 ),
  ( sym: 313; act: 196 ),
  ( sym: 314; act: 197 ),
  ( sym: 315; act: 198 ),
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
  ( sym: 318; act: -114 ),
  ( sym: 319; act: -114 ),
{ 225: }
{ 226: }
  ( sym: 259; act: 255 ),
  ( sym: 300; act: 183 ),
  ( sym: 301; act: 184 ),
  ( sym: 302; act: 185 ),
  ( sym: 303; act: 186 ),
  ( sym: 304; act: 187 ),
  ( sym: 305; act: 188 ),
  ( sym: 306; act: 189 ),
  ( sym: 307; act: 190 ),
  ( sym: 308; act: 191 ),
  ( sym: 309; act: 192 ),
  ( sym: 310; act: 193 ),
  ( sym: 311; act: 194 ),
  ( sym: 312; act: 195 ),
  ( sym: 313; act: 196 ),
  ( sym: 314; act: 197 ),
  ( sym: 315; act: 198 ),
{ 227: }
  ( sym: 308; act: 191 ),
  ( sym: 309; act: 192 ),
  ( sym: 310; act: 193 ),
  ( sym: 311; act: 194 ),
  ( sym: 312; act: 195 ),
  ( sym: 313; act: 196 ),
  ( sym: 314; act: 197 ),
  ( sym: 315; act: 198 ),
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
  ( sym: 318; act: -119 ),
  ( sym: 319; act: -119 ),
{ 228: }
  ( sym: 309; act: 192 ),
  ( sym: 310; act: 193 ),
  ( sym: 311; act: 194 ),
  ( sym: 312; act: 195 ),
  ( sym: 313; act: 196 ),
  ( sym: 314; act: 197 ),
  ( sym: 315; act: 198 ),
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
  ( sym: 318; act: -120 ),
  ( sym: 319; act: -120 ),
{ 229: }
  ( sym: 311; act: 194 ),
  ( sym: 312; act: 195 ),
  ( sym: 313; act: 196 ),
  ( sym: 314; act: 197 ),
  ( sym: 315; act: 198 ),
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
  ( sym: 318; act: -115 ),
  ( sym: 319; act: -115 ),
{ 230: }
  ( sym: 311; act: 194 ),
  ( sym: 312; act: 195 ),
  ( sym: 313; act: 196 ),
  ( sym: 314; act: 197 ),
  ( sym: 315; act: 198 ),
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
  ( sym: 318; act: -116 ),
  ( sym: 319; act: -116 ),
{ 231: }
  ( sym: 313; act: 196 ),
  ( sym: 314; act: 197 ),
  ( sym: 315; act: 198 ),
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
  ( sym: 318; act: -123 ),
  ( sym: 319; act: -123 ),
{ 232: }
  ( sym: 313; act: 196 ),
  ( sym: 314; act: 197 ),
  ( sym: 315; act: 198 ),
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
  ( sym: 307; act: -122 ),
  ( sym: 308; act: -122 ),
  ( sym: 309; act: -122 ),
  ( sym: 310; act: -122 ),
  ( sym: 311; act: -122 ),
  ( sym: 312; act: -122 ),
  ( sym: 318; act: -122 ),
  ( sym: 319; act: -122 ),
{ 233: }
  ( sym: 315; act: 198 ),
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
{ 234: }
  ( sym: 315; act: 198 ),
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
  ( sym: 318; act: -118 ),
  ( sym: 319; act: -118 ),
{ 235: }
  ( sym: 315; act: 198 ),
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
  ( sym: 318; act: -121 ),
  ( sym: 319; act: -121 ),
{ 236: }
  ( sym: 263; act: 256 ),
{ 237: }
  ( sym: 318; act: 144 ),
  ( sym: 319; act: 145 ),
  ( sym: 259; act: -138 ),
  ( sym: 260; act: -138 ),
  ( sym: 261; act: -138 ),
  ( sym: 262; act: -138 ),
  ( sym: 263; act: -138 ),
  ( sym: 264; act: -138 ),
  ( sym: 265; act: -138 ),
  ( sym: 267; act: -138 ),
  ( sym: 286; act: -138 ),
  ( sym: 296; act: -138 ),
  ( sym: 300; act: -138 ),
  ( sym: 301; act: -138 ),
  ( sym: 302; act: -138 ),
  ( sym: 303; act: -138 ),
  ( sym: 304; act: -138 ),
  ( sym: 305; act: -138 ),
  ( sym: 306; act: -138 ),
  ( sym: 307; act: -138 ),
  ( sym: 308; act: -138 ),
  ( sym: 309; act: -138 ),
  ( sym: 310; act: -138 ),
  ( sym: 311; act: -138 ),
  ( sym: 312; act: -138 ),
  ( sym: 313; act: -138 ),
  ( sym: 314; act: -138 ),
  ( sym: 315; act: -138 ),
{ 238: }
  ( sym: 262; act: 108 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 109 ),
  ( sym: 273; act: 110 ),
  ( sym: 308; act: 112 ),
  ( sym: 310; act: 113 ),
  ( sym: 315; act: 114 ),
{ 239: }
{ 240: }
  ( sym: 318; act: 144 ),
  ( sym: 319; act: 145 ),
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
{ 241: }
{ 242: }
  ( sym: 262; act: 208 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 209 ),
  ( sym: 282; act: 83 ),
  ( sym: 283; act: 84 ),
  ( sym: 284; act: 85 ),
  ( sym: 308; act: 86 ),
  ( sym: 313; act: 247 ),
  ( sym: 261; act: -107 ),
  ( sym: 263; act: -107 ),
  ( sym: 264; act: -107 ),
{ 243: }
{ 244: }
  ( sym: 263; act: 160 ),
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
  ( sym: 280; act: 161 ),
  ( sym: 281; act: 31 ),
  ( sym: 298; act: 162 ),
{ 245: }
  ( sym: 262; act: 108 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 109 ),
  ( sym: 273; act: 110 ),
  ( sym: 308; act: 112 ),
  ( sym: 310; act: 113 ),
  ( sym: 315; act: 114 ),
{ 246: }
  ( sym: 262; act: 244 ),
  ( sym: 263; act: 261 ),
  ( sym: 264; act: 245 ),
{ 247: }
  ( sym: 262; act: 208 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 209 ),
  ( sym: 282; act: 83 ),
  ( sym: 283; act: 84 ),
  ( sym: 284; act: 85 ),
  ( sym: 308; act: 86 ),
  ( sym: 313; act: 247 ),
  ( sym: 261; act: -107 ),
  ( sym: 263; act: -107 ),
  ( sym: 264; act: -107 ),
{ 248: }
  ( sym: 262; act: 244 ),
  ( sym: 264; act: 245 ),
  ( sym: 261; act: -100 ),
  ( sym: 263; act: -100 ),
{ 249: }
  ( sym: 264; act: 245 ),
  ( sym: 261; act: -102 ),
  ( sym: 262; act: -102 ),
  ( sym: 263; act: -102 ),
{ 250: }
  ( sym: 262; act: 119 ),
  ( sym: 264; act: 120 ),
  ( sym: 261; act: -79 ),
  ( sym: 263; act: -79 ),
{ 251: }
{ 252: }
  ( sym: 263; act: 262 ),
{ 253: }
{ 254: }
{ 255: }
  ( sym: 262; act: 108 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 109 ),
  ( sym: 273; act: 110 ),
  ( sym: 308; act: 112 ),
  ( sym: 310; act: 113 ),
  ( sym: 315; act: 114 ),
{ 256: }
  ( sym: 262; act: 108 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 109 ),
  ( sym: 273; act: 110 ),
  ( sym: 308; act: 112 ),
  ( sym: 310; act: 113 ),
  ( sym: 315; act: 114 ),
{ 257: }
  ( sym: 318; act: 144 ),
  ( sym: 319; act: 145 ),
  ( sym: 259; act: -139 ),
  ( sym: 260; act: -139 ),
  ( sym: 261; act: -139 ),
  ( sym: 262; act: -139 ),
  ( sym: 263; act: -139 ),
  ( sym: 264; act: -139 ),
  ( sym: 265; act: -139 ),
  ( sym: 267; act: -139 ),
  ( sym: 286; act: -139 ),
  ( sym: 296; act: -139 ),
  ( sym: 300; act: -139 ),
  ( sym: 301; act: -139 ),
  ( sym: 302; act: -139 ),
  ( sym: 303; act: -139 ),
  ( sym: 304; act: -139 ),
  ( sym: 305; act: -139 ),
  ( sym: 306; act: -139 ),
  ( sym: 307; act: -139 ),
  ( sym: 308; act: -139 ),
  ( sym: 309; act: -139 ),
  ( sym: 310; act: -139 ),
  ( sym: 311; act: -139 ),
  ( sym: 312; act: -139 ),
  ( sym: 313; act: -139 ),
  ( sym: 314; act: -139 ),
  ( sym: 315; act: -139 ),
{ 258: }
  ( sym: 262; act: 244 ),
  ( sym: 264; act: 245 ),
  ( sym: 261; act: -101 ),
  ( sym: 263; act: -101 ),
{ 259: }
  ( sym: 263; act: 265 ),
{ 260: }
  ( sym: 265; act: 266 ),
  ( sym: 300; act: 183 ),
  ( sym: 301; act: 184 ),
  ( sym: 302; act: 185 ),
  ( sym: 303; act: 186 ),
  ( sym: 304; act: 187 ),
  ( sym: 305; act: 188 ),
  ( sym: 306; act: 189 ),
  ( sym: 307; act: 190 ),
  ( sym: 308; act: 191 ),
  ( sym: 309; act: 192 ),
  ( sym: 310; act: 193 ),
  ( sym: 311; act: 194 ),
  ( sym: 312; act: 195 ),
  ( sym: 313; act: 196 ),
  ( sym: 314; act: 197 ),
  ( sym: 315; act: 198 ),
{ 261: }
{ 262: }
{ 263: }
  ( sym: 307; act: 190 ),
  ( sym: 308; act: 191 ),
  ( sym: 309; act: 192 ),
  ( sym: 310; act: 193 ),
  ( sym: 311; act: 194 ),
  ( sym: 312; act: 195 ),
  ( sym: 313; act: 196 ),
  ( sym: 314; act: 197 ),
  ( sym: 315; act: 198 ),
  ( sym: 259; act: -126 ),
  ( sym: 260; act: -126 ),
  ( sym: 261; act: -126 ),
  ( sym: 262; act: -126 ),
  ( sym: 263; act: -126 ),
  ( sym: 264; act: -126 ),
  ( sym: 265; act: -126 ),
  ( sym: 267; act: -126 ),
  ( sym: 286; act: -126 ),
  ( sym: 296; act: -126 ),
  ( sym: 300; act: -126 ),
  ( sym: 301; act: -126 ),
  ( sym: 302; act: -126 ),
  ( sym: 303; act: -126 ),
  ( sym: 304; act: -126 ),
  ( sym: 305; act: -126 ),
  ( sym: 306; act: -126 ),
  ( sym: 318; act: -126 ),
  ( sym: 319; act: -126 ),
{ 264: }
  ( sym: 318; act: 144 ),
  ( sym: 319; act: 145 ),
  ( sym: 259; act: -140 ),
  ( sym: 260; act: -140 ),
  ( sym: 261; act: -140 ),
  ( sym: 262; act: -140 ),
  ( sym: 263; act: -140 ),
  ( sym: 264; act: -140 ),
  ( sym: 265; act: -140 ),
  ( sym: 267; act: -140 ),
  ( sym: 286; act: -140 ),
  ( sym: 296; act: -140 ),
  ( sym: 300; act: -140 ),
  ( sym: 301; act: -140 ),
  ( sym: 302; act: -140 ),
  ( sym: 303; act: -140 ),
  ( sym: 304; act: -140 ),
  ( sym: 305; act: -140 ),
  ( sym: 306; act: -140 ),
  ( sym: 307; act: -140 ),
  ( sym: 308; act: -140 ),
  ( sym: 309; act: -140 ),
  ( sym: 310; act: -140 ),
  ( sym: 311; act: -140 ),
  ( sym: 312; act: -140 ),
  ( sym: 313; act: -140 ),
  ( sym: 314; act: -140 ),
  ( sym: 315; act: -140 )
{ 265: }
{ 266: }
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
  ( sym: -11; act: 63 ),
  ( sym: -10; act: 19 ),
{ 32: }
{ 33: }
{ 34: }
{ 35: }
  ( sym: -8; act: 66 ),
{ 36: }
{ 37: }
  ( sym: -18; act: 50 ),
  ( sym: -10; act: 69 ),
{ 38: }
{ 39: }
  ( sym: -18; act: 73 ),
{ 40: }
  ( sym: -18; act: 74 ),
{ 41: }
  ( sym: -20; act: 75 ),
{ 42: }
  ( sym: -26; act: 76 ),
  ( sym: -14; act: 77 ),
  ( sym: -12; act: 78 ),
  ( sym: -10; act: 79 ),
{ 43: }
{ 44: }
{ 45: }
{ 46: }
{ 47: }
{ 48: }
{ 49: }
{ 50: }
{ 51: }
  ( sym: -4; act: 89 ),
{ 52: }
  ( sym: -23; act: 15 ),
  ( sym: -22; act: 90 ),
  ( sym: -21; act: 16 ),
  ( sym: -19; act: 91 ),
  ( sym: -13; act: 17 ),
  ( sym: -11; act: 92 ),
  ( sym: -10; act: 19 ),
{ 53: }
{ 54: }
{ 55: }
  ( sym: -4; act: 94 ),
{ 56: }
  ( sym: -35; act: 95 ),
  ( sym: -16; act: 96 ),
  ( sym: -10; act: 97 ),
{ 57: }
{ 58: }
{ 59: }
{ 60: }
{ 61: }
{ 62: }
{ 63: }
{ 64: }
{ 65: }
{ 66: }
  ( sym: -26; act: 76 ),
  ( sym: -14; act: 77 ),
  ( sym: -12; act: 101 ),
  ( sym: -10; act: 79 ),
{ 67: }
  ( sym: -8; act: 102 ),
{ 68: }
{ 69: }
  ( sym: -18; act: 73 ),
  ( sym: -10; act: 103 ),
{ 70: }
  ( sym: -35; act: 95 ),
  ( sym: -16; act: 104 ),
  ( sym: -10; act: 97 ),
{ 71: }
{ 72: }
  ( sym: -32; act: 105 ),
  ( sym: -17; act: 106 ),
  ( sym: -10; act: 107 ),
{ 73: }
{ 74: }
{ 75: }
{ 76: }
{ 77: }
  ( sym: -29; act: 118 ),
{ 78: }
  ( sym: -9; act: 121 ),
{ 79: }
  ( sym: -28; act: 124 ),
{ 80: }
  ( sym: -4; act: 126 ),
{ 81: }
  ( sym: -26; act: 76 ),
  ( sym: -14; act: 127 ),
  ( sym: -10; act: 79 ),
{ 82: }
  ( sym: -26; act: 76 ),
  ( sym: -14; act: 128 ),
  ( sym: -10; act: 79 ),
{ 83: }
{ 84: }
{ 85: }
{ 86: }
  ( sym: -26; act: 76 ),
  ( sym: -14; act: 129 ),
  ( sym: -10; act: 79 ),
{ 87: }
  ( sym: -26; act: 76 ),
  ( sym: -14; act: 130 ),
  ( sym: -10; act: 79 ),
{ 88: }
{ 89: }
{ 90: }
  ( sym: -23; act: 15 ),
  ( sym: -22; act: 90 ),
  ( sym: -21; act: 16 ),
  ( sym: -19; act: 132 ),
  ( sym: -13; act: 17 ),
  ( sym: -11; act: 92 ),
  ( sym: -10; act: 19 ),
{ 91: }
{ 92: }
  ( sym: -26; act: 76 ),
  ( sym: -14; act: 77 ),
  ( sym: -12; act: 134 ),
  ( sym: -10; act: 79 ),
{ 93: }
{ 94: }
{ 95: }
{ 96: }
{ 97: }
{ 98: }
{ 99: }
{ 100: }
{ 101: }
{ 102: }
  ( sym: -26; act: 76 ),
  ( sym: -14; act: 141 ),
  ( sym: -10; act: 79 ),
{ 103: }
{ 104: }
{ 105: }
{ 106: }
{ 107: }
{ 108: }
  ( sym: -32; act: 148 ),
  ( sym: -30; act: 149 ),
  ( sym: -27; act: 150 ),
  ( sym: -23; act: 15 ),
  ( sym: -21; act: 16 ),
  ( sym: -13; act: 17 ),
  ( sym: -11; act: 151 ),
  ( sym: -10; act: 152 ),
{ 109: }
{ 110: }
{ 111: }
{ 112: }
  ( sym: -32; act: 153 ),
  ( sym: -10; act: 107 ),
{ 113: }
  ( sym: -32; act: 154 ),
  ( sym: -10; act: 107 ),
{ 114: }
  ( sym: -32; act: 155 ),
  ( sym: -10; act: 107 ),
{ 115: }
{ 116: }
{ 117: }
  ( sym: -26; act: 76 ),
  ( sym: -14; act: 156 ),
  ( sym: -10; act: 79 ),
{ 118: }
{ 119: }
  ( sym: -24; act: 157 ),
  ( sym: -23; act: 15 ),
  ( sym: -21; act: 16 ),
  ( sym: -15; act: 158 ),
  ( sym: -13; act: 17 ),
  ( sym: -11; act: 159 ),
  ( sym: -10; act: 19 ),
{ 120: }
  ( sym: -32; act: 148 ),
  ( sym: -30; act: 163 ),
  ( sym: -27; act: 164 ),
  ( sym: -10; act: 107 ),
{ 121: }
{ 122: }
  ( sym: -26; act: 76 ),
  ( sym: -14; act: 166 ),
  ( sym: -10; act: 79 ),
{ 123: }
{ 124: }
  ( sym: -32; act: 148 ),
  ( sym: -30; act: 163 ),
  ( sym: -27; act: 168 ),
  ( sym: -10; act: 107 ),
{ 125: }
  ( sym: -32; act: 148 ),
  ( sym: -30; act: 163 ),
  ( sym: -27; act: 169 ),
  ( sym: -10; act: 107 ),
{ 126: }
{ 127: }
  ( sym: -29; act: 118 ),
{ 128: }
  ( sym: -29; act: 118 ),
{ 129: }
  ( sym: -29; act: 118 ),
{ 130: }
  ( sym: -29; act: 118 ),
{ 131: }
{ 132: }
{ 133: }
{ 134: }
{ 135: }
{ 136: }
  ( sym: -35; act: 95 ),
  ( sym: -16; act: 173 ),
  ( sym: -10; act: 97 ),
{ 137: }
{ 138: }
  ( sym: -32; act: 148 ),
  ( sym: -30; act: 163 ),
  ( sym: -27; act: 174 ),
  ( sym: -10; act: 107 ),
{ 139: }
{ 140: }
{ 141: }
  ( sym: -29; act: 118 ),
{ 142: }
{ 143: }
{ 144: }
  ( sym: -32; act: 148 ),
  ( sym: -30; act: 163 ),
  ( sym: -27; act: 177 ),
  ( sym: -10; act: 107 ),
{ 145: }
  ( sym: -32; act: 148 ),
  ( sym: -30; act: 163 ),
  ( sym: -27; act: 178 ),
  ( sym: -10; act: 107 ),
{ 146: }
{ 147: }
  ( sym: -36; act: 179 ),
  ( sym: -34; act: 180 ),
  ( sym: -32; act: 148 ),
  ( sym: -30; act: 163 ),
  ( sym: -27; act: 181 ),
  ( sym: -10; act: 107 ),
{ 148: }
{ 149: }
{ 150: }
{ 151: }
  ( sym: -26; act: 199 ),
{ 152: }
{ 153: }
{ 154: }
{ 155: }
{ 156: }
  ( sym: -29; act: 118 ),
{ 157: }
{ 158: }
{ 159: }
  ( sym: -26; act: 205 ),
  ( sym: -25; act: 206 ),
  ( sym: -14; act: 207 ),
  ( sym: -10; act: 79 ),
{ 160: }
{ 161: }
{ 162: }
{ 163: }
{ 164: }
{ 165: }
{ 166: }
  ( sym: -29; act: 118 ),
{ 167: }
  ( sym: -10; act: 213 ),
{ 168: }
{ 169: }
{ 170: }
  ( sym: -26; act: 76 ),
  ( sym: -14; act: 77 ),
  ( sym: -12; act: 214 ),
  ( sym: -10; act: 79 ),
{ 171: }
{ 172: }
{ 173: }
{ 174: }
{ 175: }
{ 176: }
  ( sym: -32; act: 105 ),
  ( sym: -17; act: 216 ),
  ( sym: -10; act: 107 ),
{ 177: }
{ 178: }
{ 179: }
{ 180: }
{ 181: }
{ 182: }
{ 183: }
  ( sym: -32; act: 148 ),
  ( sym: -30; act: 163 ),
  ( sym: -27; act: 219 ),
  ( sym: -10; act: 107 ),
{ 184: }
  ( sym: -32; act: 148 ),
  ( sym: -30; act: 163 ),
  ( sym: -27; act: 220 ),
  ( sym: -10; act: 107 ),
{ 185: }
  ( sym: -32; act: 148 ),
  ( sym: -30; act: 163 ),
  ( sym: -27; act: 221 ),
  ( sym: -10; act: 107 ),
{ 186: }
  ( sym: -32; act: 148 ),
  ( sym: -30; act: 163 ),
  ( sym: -27; act: 222 ),
  ( sym: -10; act: 107 ),
{ 187: }
  ( sym: -32; act: 148 ),
  ( sym: -30; act: 163 ),
  ( sym: -27; act: 223 ),
  ( sym: -10; act: 107 ),
{ 188: }
  ( sym: -32; act: 148 ),
  ( sym: -30; act: 163 ),
  ( sym: -27; act: 224 ),
  ( sym: -10; act: 107 ),
{ 189: }
  ( sym: -32; act: 148 ),
  ( sym: -31; act: 225 ),
  ( sym: -30; act: 163 ),
  ( sym: -27; act: 226 ),
  ( sym: -10; act: 107 ),
{ 190: }
  ( sym: -32; act: 148 ),
  ( sym: -30; act: 163 ),
  ( sym: -27; act: 227 ),
  ( sym: -10; act: 107 ),
{ 191: }
  ( sym: -32; act: 148 ),
  ( sym: -30; act: 163 ),
  ( sym: -27; act: 228 ),
  ( sym: -10; act: 107 ),
{ 192: }
  ( sym: -32; act: 148 ),
  ( sym: -30; act: 163 ),
  ( sym: -27; act: 229 ),
  ( sym: -10; act: 107 ),
{ 193: }
  ( sym: -32; act: 148 ),
  ( sym: -30; act: 163 ),
  ( sym: -27; act: 230 ),
  ( sym: -10; act: 107 ),
{ 194: }
  ( sym: -32; act: 148 ),
  ( sym: -30; act: 163 ),
  ( sym: -27; act: 231 ),
  ( sym: -10; act: 107 ),
{ 195: }
  ( sym: -32; act: 148 ),
  ( sym: -30; act: 163 ),
  ( sym: -27; act: 232 ),
  ( sym: -10; act: 107 ),
{ 196: }
  ( sym: -32; act: 148 ),
  ( sym: -30; act: 163 ),
  ( sym: -27; act: 233 ),
  ( sym: -10; act: 107 ),
{ 197: }
  ( sym: -32; act: 148 ),
  ( sym: -30; act: 163 ),
  ( sym: -27; act: 234 ),
  ( sym: -10; act: 107 ),
{ 198: }
  ( sym: -32; act: 148 ),
  ( sym: -30; act: 163 ),
  ( sym: -27; act: 235 ),
  ( sym: -10; act: 107 ),
{ 199: }
{ 200: }
  ( sym: -32; act: 237 ),
  ( sym: -10; act: 107 ),
{ 201: }
{ 202: }
  ( sym: -33; act: 239 ),
  ( sym: -32; act: 240 ),
  ( sym: -10; act: 107 ),
{ 203: }
  ( sym: -24; act: 157 ),
  ( sym: -23; act: 15 ),
  ( sym: -21; act: 16 ),
  ( sym: -15; act: 241 ),
  ( sym: -13; act: 17 ),
  ( sym: -11; act: 159 ),
  ( sym: -10; act: 19 ),
{ 204: }
{ 205: }
{ 206: }
  ( sym: -29; act: 243 ),
{ 207: }
  ( sym: -29; act: 118 ),
{ 208: }
  ( sym: -26; act: 205 ),
  ( sym: -25; act: 246 ),
  ( sym: -14; act: 127 ),
  ( sym: -10; act: 79 ),
{ 209: }
  ( sym: -26; act: 205 ),
  ( sym: -25; act: 248 ),
  ( sym: -14; act: 128 ),
  ( sym: -10; act: 79 ),
{ 210: }
  ( sym: -26; act: 205 ),
  ( sym: -25; act: 249 ),
  ( sym: -14; act: 250 ),
  ( sym: -10; act: 79 ),
{ 211: }
{ 212: }
{ 213: }
{ 214: }
{ 215: }
  ( sym: -24; act: 157 ),
  ( sym: -23; act: 15 ),
  ( sym: -21; act: 16 ),
  ( sym: -15; act: 252 ),
  ( sym: -13; act: 17 ),
  ( sym: -11; act: 159 ),
  ( sym: -10; act: 19 ),
{ 216: }
{ 217: }
  ( sym: -36; act: 179 ),
  ( sym: -34; act: 254 ),
  ( sym: -32; act: 148 ),
  ( sym: -30; act: 163 ),
  ( sym: -27; act: 181 ),
  ( sym: -10; act: 107 ),
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
{ 235: }
{ 236: }
{ 237: }
{ 238: }
  ( sym: -32; act: 257 ),
  ( sym: -10; act: 107 ),
{ 239: }
{ 240: }
{ 241: }
{ 242: }
  ( sym: -26; act: 205 ),
  ( sym: -25; act: 258 ),
  ( sym: -14; act: 156 ),
  ( sym: -10; act: 79 ),
{ 243: }
{ 244: }
  ( sym: -24; act: 157 ),
  ( sym: -23; act: 15 ),
  ( sym: -21; act: 16 ),
  ( sym: -15; act: 259 ),
  ( sym: -13; act: 17 ),
  ( sym: -11; act: 159 ),
  ( sym: -10; act: 19 ),
{ 245: }
  ( sym: -32; act: 148 ),
  ( sym: -30; act: 163 ),
  ( sym: -27; act: 260 ),
  ( sym: -10; act: 107 ),
{ 246: }
  ( sym: -29; act: 243 ),
{ 247: }
  ( sym: -26; act: 205 ),
  ( sym: -25; act: 249 ),
  ( sym: -14; act: 130 ),
  ( sym: -10; act: 79 ),
{ 248: }
  ( sym: -29; act: 243 ),
{ 249: }
  ( sym: -29; act: 243 ),
{ 250: }
  ( sym: -29; act: 118 ),
{ 251: }
{ 252: }
{ 253: }
{ 254: }
{ 255: }
  ( sym: -32; act: 148 ),
  ( sym: -30; act: 163 ),
  ( sym: -27; act: 263 ),
  ( sym: -10; act: 107 ),
{ 256: }
  ( sym: -32; act: 264 ),
  ( sym: -10; act: 107 ),
{ 257: }
{ 258: }
  ( sym: -29; act: 243 )
{ 259: }
{ 260: }
{ 261: }
{ 262: }
{ 263: }
{ 264: }
{ 265: }
{ 266: }
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
{ 15: } -72,
{ 16: } -50,
{ 17: } -49,
{ 18: } 0,
{ 19: } -73,
{ 20: } 0,
{ 21: } 0,
{ 22: } 0,
{ 23: } -54,
{ 24: } -70,
{ 25: } 0,
{ 26: } 0,
{ 27: } -55,
{ 28: } -58,
{ 29: } -71,
{ 30: } -69,
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
{ 57: } -67,
{ 58: } 0,
{ 59: } -56,
{ 60: } -68,
{ 61: } 0,
{ 62: } -59,
{ 63: } -43,
{ 64: } -25,
{ 65: } -30,
{ 66: } 0,
{ 67: } 0,
{ 68: } -24,
{ 69: } 0,
{ 70: } 0,
{ 71: } -28,
{ 72: } 0,
{ 73: } 0,
{ 74: } 0,
{ 75: } -41,
{ 76: } 0,
{ 77: } 0,
{ 78: } 0,
{ 79: } 0,
{ 80: } -2,
{ 81: } 0,
{ 82: } 0,
{ 83: } -84,
{ 84: } -86,
{ 85: } -85,
{ 86: } 0,
{ 87: } 0,
{ 88: } -46,
{ 89: } 0,
{ 90: } 0,
{ 91: } 0,
{ 92: } 0,
{ 93: } -44,
{ 94: } 0,
{ 95: } 0,
{ 96: } 0,
{ 97: } 0,
{ 98: } 0,
{ 99: } -64,
{ 100: } -60,
{ 101: } 0,
{ 102: } 0,
{ 103: } 0,
{ 104: } 0,
{ 105: } 0,
{ 106: } 0,
{ 107: } 0,
{ 108: } 0,
{ 109: } -131,
{ 110: } -130,
{ 111: } -27,
{ 112: } 0,
{ 113: } 0,
{ 114: } 0,
{ 115: } -35,
{ 116: } -37,
{ 117: } 0,
{ 118: } -95,
{ 119: } 0,
{ 120: } 0,
{ 121: } 0,
{ 122: } 0,
{ 123: } 0,
{ 124: } 0,
{ 125: } 0,
{ 126: } 0,
{ 127: } 0,
{ 128: } 0,
{ 129: } 0,
{ 130: } 0,
{ 131: } -32,
{ 132: } -51,
{ 133: } -31,
{ 134: } 0,
{ 135: } -34,
{ 136: } 0,
{ 137: } -33,
{ 138: } 0,
{ 139: } -61,
{ 140: } -23,
{ 141: } 0,
{ 142: } -21,
{ 143: } 0,
{ 144: } 0,
{ 145: } 0,
{ 146: } -29,
{ 147: } 0,
{ 148: } 0,
{ 149: } 0,
{ 150: } 0,
{ 151: } 0,
{ 152: } 0,
{ 153: } 0,
{ 154: } 0,
{ 155: } 0,
{ 156: } 0,
{ 157: } 0,
{ 158: } 0,
{ 159: } 0,
{ 160: } -98,
{ 161: } 0,
{ 162: } -83,
{ 163: } -108,
{ 164: } 0,
{ 165: } -19,
{ 166: } 0,
{ 167: } 0,
{ 168: } 0,
{ 169: } 0,
{ 170: } 0,
{ 171: } -97,
{ 172: } -53,
{ 173: } -143,
{ 174: } 0,
{ 175: } 0,
{ 176: } 0,
{ 177: } -132,
{ 178: } -133,
{ 179: } 0,
{ 180: } 0,
{ 181: } 0,
{ 182: } -142,
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
{ 200: } 0,
{ 201: } 0,
{ 202: } 0,
{ 203: } 0,
{ 204: } -94,
{ 205: } 0,
{ 206: } 0,
{ 207: } 0,
{ 208: } 0,
{ 209: } 0,
{ 210: } 0,
{ 211: } -99,
{ 212: } -96,
{ 213: } 0,
{ 214: } -75,
{ 215: } 0,
{ 216: } 0,
{ 217: } 0,
{ 218: } -141,
{ 219: } 0,
{ 220: } 0,
{ 221: } 0,
{ 222: } 0,
{ 223: } 0,
{ 224: } 0,
{ 225: } -124,
{ 226: } 0,
{ 227: } 0,
{ 228: } 0,
{ 229: } 0,
{ 230: } 0,
{ 231: } 0,
{ 232: } 0,
{ 233: } 0,
{ 234: } 0,
{ 235: } 0,
{ 236: } 0,
{ 237: } 0,
{ 238: } 0,
{ 239: } -137,
{ 240: } 0,
{ 241: } -82,
{ 242: } 0,
{ 243: } -104,
{ 244: } 0,
{ 245: } 0,
{ 246: } 0,
{ 247: } 0,
{ 248: } 0,
{ 249: } 0,
{ 250: } 0,
{ 251: } -17,
{ 252: } 0,
{ 253: } -26,
{ 254: } -149,
{ 255: } 0,
{ 256: } 0,
{ 257: } 0,
{ 258: } 0,
{ 259: } 0,
{ 260: } 0,
{ 261: } -106,
{ 262: } -22,
{ 263: } 0,
{ 264: } 0,
{ 265: } -103,
{ 266: } -105
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
{ 27: } 133,
{ 28: } 133,
{ 29: } 133,
{ 30: } 133,
{ 31: } 133,
{ 32: } 145,
{ 33: } 145,
{ 34: } 145,
{ 35: } 147,
{ 36: } 163,
{ 37: } 180,
{ 38: } 183,
{ 39: } 186,
{ 40: } 207,
{ 41: } 228,
{ 42: } 249,
{ 43: } 258,
{ 44: } 258,
{ 45: } 258,
{ 46: } 258,
{ 47: } 258,
{ 48: } 258,
{ 49: } 258,
{ 50: } 258,
{ 51: } 278,
{ 52: } 278,
{ 53: } 290,
{ 54: } 310,
{ 55: } 310,
{ 56: } 310,
{ 57: } 312,
{ 58: } 312,
{ 59: } 333,
{ 60: } 333,
{ 61: } 333,
{ 62: } 353,
{ 63: } 353,
{ 64: } 353,
{ 65: } 353,
{ 66: } 353,
{ 67: } 362,
{ 68: } 377,
{ 69: } 377,
{ 70: } 394,
{ 71: } 396,
{ 72: } 396,
{ 73: } 404,
{ 74: } 425,
{ 75: } 446,
{ 76: } 446,
{ 77: } 447,
{ 78: } 452,
{ 79: } 455,
{ 80: } 462,
{ 81: } 462,
{ 82: } 470,
{ 83: } 478,
{ 84: } 478,
{ 85: } 478,
{ 86: } 478,
{ 87: } 486,
{ 88: } 494,
{ 89: } 494,
{ 90: } 495,
{ 91: } 508,
{ 92: } 509,
{ 93: } 518,
{ 94: } 518,
{ 95: } 519,
{ 96: } 522,
{ 97: } 523,
{ 98: } 527,
{ 99: } 547,
{ 100: } 547,
{ 101: } 547,
{ 102: } 549,
{ 103: } 557,
{ 104: } 558,
{ 105: } 559,
{ 106: } 562,
{ 107: } 563,
{ 108: } 591,
{ 109: } 609,
{ 110: } 609,
{ 111: } 609,
{ 112: } 609,
{ 113: } 616,
{ 114: } 623,
{ 115: } 630,
{ 116: } 630,
{ 117: } 630,
{ 118: } 638,
{ 119: } 638,
{ 120: } 652,
{ 121: } 659,
{ 122: } 660,
{ 123: } 668,
{ 124: } 669,
{ 125: } 676,
{ 126: } 683,
{ 127: } 686,
{ 128: } 689,
{ 129: } 695,
{ 130: } 701,
{ 131: } 707,
{ 132: } 707,
{ 133: } 707,
{ 134: } 707,
{ 135: } 709,
{ 136: } 709,
{ 137: } 712,
{ 138: } 712,
{ 139: } 719,
{ 140: } 719,
{ 141: } 719,
{ 142: } 722,
{ 143: } 722,
{ 144: } 723,
{ 145: } 730,
{ 146: } 737,
{ 147: } 737,
{ 148: } 745,
{ 149: } 773,
{ 150: } 790,
{ 151: } 806,
{ 152: } 811,
{ 153: } 834,
{ 154: } 862,
{ 155: } 890,
{ 156: } 918,
{ 157: } 924,
{ 158: } 926,
{ 159: } 927,
{ 160: } 938,
{ 161: } 938,
{ 162: } 949,
{ 163: } 949,
{ 164: } 949,
{ 165: } 966,
{ 166: } 966,
{ 167: } 971,
{ 168: } 972,
{ 169: } 994,
{ 170: } 1016,
{ 171: } 1025,
{ 172: } 1025,
{ 173: } 1025,
{ 174: } 1025,
{ 175: } 1044,
{ 176: } 1045,
{ 177: } 1052,
{ 178: } 1052,
{ 179: } 1052,
{ 180: } 1054,
{ 181: } 1055,
{ 182: } 1073,
{ 183: } 1073,
{ 184: } 1080,
{ 185: } 1087,
{ 186: } 1094,
{ 187: } 1101,
{ 188: } 1108,
{ 189: } 1115,
{ 190: } 1122,
{ 191: } 1129,
{ 192: } 1136,
{ 193: } 1143,
{ 194: } 1150,
{ 195: } 1157,
{ 196: } 1164,
{ 197: } 1171,
{ 198: } 1178,
{ 199: } 1185,
{ 200: } 1186,
{ 201: } 1193,
{ 202: } 1194,
{ 203: } 1225,
{ 204: } 1238,
{ 205: } 1238,
{ 206: } 1239,
{ 207: } 1243,
{ 208: } 1247,
{ 209: } 1257,
{ 210: } 1268,
{ 211: } 1279,
{ 212: } 1279,
{ 213: } 1279,
{ 214: } 1280,
{ 215: } 1280,
{ 216: } 1293,
{ 217: } 1294,
{ 218: } 1302,
{ 219: } 1302,
{ 220: } 1330,
{ 221: } 1358,
{ 222: } 1386,
{ 223: } 1414,
{ 224: } 1442,
{ 225: } 1470,
{ 226: } 1470,
{ 227: } 1487,
{ 228: } 1515,
{ 229: } 1543,
{ 230: } 1571,
{ 231: } 1599,
{ 232: } 1627,
{ 233: } 1655,
{ 234: } 1683,
{ 235: } 1711,
{ 236: } 1739,
{ 237: } 1740,
{ 238: } 1768,
{ 239: } 1775,
{ 240: } 1775,
{ 241: } 1803,
{ 242: } 1803,
{ 243: } 1814,
{ 244: } 1814,
{ 245: } 1828,
{ 246: } 1835,
{ 247: } 1838,
{ 248: } 1849,
{ 249: } 1853,
{ 250: } 1857,
{ 251: } 1861,
{ 252: } 1861,
{ 253: } 1862,
{ 254: } 1862,
{ 255: } 1862,
{ 256: } 1869,
{ 257: } 1876,
{ 258: } 1904,
{ 259: } 1908,
{ 260: } 1909,
{ 261: } 1926,
{ 262: } 1926,
{ 263: } 1926,
{ 264: } 1954,
{ 265: } 1982,
{ 266: } 1982
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
{ 26: } 132,
{ 27: } 132,
{ 28: } 132,
{ 29: } 132,
{ 30: } 132,
{ 31: } 144,
{ 32: } 144,
{ 33: } 144,
{ 34: } 146,
{ 35: } 162,
{ 36: } 179,
{ 37: } 182,
{ 38: } 185,
{ 39: } 206,
{ 40: } 227,
{ 41: } 248,
{ 42: } 257,
{ 43: } 257,
{ 44: } 257,
{ 45: } 257,
{ 46: } 257,
{ 47: } 257,
{ 48: } 257,
{ 49: } 257,
{ 50: } 277,
{ 51: } 277,
{ 52: } 289,
{ 53: } 309,
{ 54: } 309,
{ 55: } 309,
{ 56: } 311,
{ 57: } 311,
{ 58: } 332,
{ 59: } 332,
{ 60: } 332,
{ 61: } 352,
{ 62: } 352,
{ 63: } 352,
{ 64: } 352,
{ 65: } 352,
{ 66: } 361,
{ 67: } 376,
{ 68: } 376,
{ 69: } 393,
{ 70: } 395,
{ 71: } 395,
{ 72: } 403,
{ 73: } 424,
{ 74: } 445,
{ 75: } 445,
{ 76: } 446,
{ 77: } 451,
{ 78: } 454,
{ 79: } 461,
{ 80: } 461,
{ 81: } 469,
{ 82: } 477,
{ 83: } 477,
{ 84: } 477,
{ 85: } 477,
{ 86: } 485,
{ 87: } 493,
{ 88: } 493,
{ 89: } 494,
{ 90: } 507,
{ 91: } 508,
{ 92: } 517,
{ 93: } 517,
{ 94: } 518,
{ 95: } 521,
{ 96: } 522,
{ 97: } 526,
{ 98: } 546,
{ 99: } 546,
{ 100: } 546,
{ 101: } 548,
{ 102: } 556,
{ 103: } 557,
{ 104: } 558,
{ 105: } 561,
{ 106: } 562,
{ 107: } 590,
{ 108: } 608,
{ 109: } 608,
{ 110: } 608,
{ 111: } 608,
{ 112: } 615,
{ 113: } 622,
{ 114: } 629,
{ 115: } 629,
{ 116: } 629,
{ 117: } 637,
{ 118: } 637,
{ 119: } 651,
{ 120: } 658,
{ 121: } 659,
{ 122: } 667,
{ 123: } 668,
{ 124: } 675,
{ 125: } 682,
{ 126: } 685,
{ 127: } 688,
{ 128: } 694,
{ 129: } 700,
{ 130: } 706,
{ 131: } 706,
{ 132: } 706,
{ 133: } 706,
{ 134: } 708,
{ 135: } 708,
{ 136: } 711,
{ 137: } 711,
{ 138: } 718,
{ 139: } 718,
{ 140: } 718,
{ 141: } 721,
{ 142: } 721,
{ 143: } 722,
{ 144: } 729,
{ 145: } 736,
{ 146: } 736,
{ 147: } 744,
{ 148: } 772,
{ 149: } 789,
{ 150: } 805,
{ 151: } 810,
{ 152: } 833,
{ 153: } 861,
{ 154: } 889,
{ 155: } 917,
{ 156: } 923,
{ 157: } 925,
{ 158: } 926,
{ 159: } 937,
{ 160: } 937,
{ 161: } 948,
{ 162: } 948,
{ 163: } 948,
{ 164: } 965,
{ 165: } 965,
{ 166: } 970,
{ 167: } 971,
{ 168: } 993,
{ 169: } 1015,
{ 170: } 1024,
{ 171: } 1024,
{ 172: } 1024,
{ 173: } 1024,
{ 174: } 1043,
{ 175: } 1044,
{ 176: } 1051,
{ 177: } 1051,
{ 178: } 1051,
{ 179: } 1053,
{ 180: } 1054,
{ 181: } 1072,
{ 182: } 1072,
{ 183: } 1079,
{ 184: } 1086,
{ 185: } 1093,
{ 186: } 1100,
{ 187: } 1107,
{ 188: } 1114,
{ 189: } 1121,
{ 190: } 1128,
{ 191: } 1135,
{ 192: } 1142,
{ 193: } 1149,
{ 194: } 1156,
{ 195: } 1163,
{ 196: } 1170,
{ 197: } 1177,
{ 198: } 1184,
{ 199: } 1185,
{ 200: } 1192,
{ 201: } 1193,
{ 202: } 1224,
{ 203: } 1237,
{ 204: } 1237,
{ 205: } 1238,
{ 206: } 1242,
{ 207: } 1246,
{ 208: } 1256,
{ 209: } 1267,
{ 210: } 1278,
{ 211: } 1278,
{ 212: } 1278,
{ 213: } 1279,
{ 214: } 1279,
{ 215: } 1292,
{ 216: } 1293,
{ 217: } 1301,
{ 218: } 1301,
{ 219: } 1329,
{ 220: } 1357,
{ 221: } 1385,
{ 222: } 1413,
{ 223: } 1441,
{ 224: } 1469,
{ 225: } 1469,
{ 226: } 1486,
{ 227: } 1514,
{ 228: } 1542,
{ 229: } 1570,
{ 230: } 1598,
{ 231: } 1626,
{ 232: } 1654,
{ 233: } 1682,
{ 234: } 1710,
{ 235: } 1738,
{ 236: } 1739,
{ 237: } 1767,
{ 238: } 1774,
{ 239: } 1774,
{ 240: } 1802,
{ 241: } 1802,
{ 242: } 1813,
{ 243: } 1813,
{ 244: } 1827,
{ 245: } 1834,
{ 246: } 1837,
{ 247: } 1848,
{ 248: } 1852,
{ 249: } 1856,
{ 250: } 1860,
{ 251: } 1860,
{ 252: } 1861,
{ 253: } 1861,
{ 254: } 1861,
{ 255: } 1868,
{ 256: } 1875,
{ 257: } 1903,
{ 258: } 1907,
{ 259: } 1908,
{ 260: } 1925,
{ 261: } 1925,
{ 262: } 1925,
{ 263: } 1953,
{ 264: } 1981,
{ 265: } 1981,
{ 266: } 1981
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
{ 66: } 60,
{ 67: } 64,
{ 68: } 65,
{ 69: } 65,
{ 70: } 67,
{ 71: } 70,
{ 72: } 70,
{ 73: } 73,
{ 74: } 73,
{ 75: } 73,
{ 76: } 73,
{ 77: } 73,
{ 78: } 74,
{ 79: } 75,
{ 80: } 76,
{ 81: } 77,
{ 82: } 80,
{ 83: } 83,
{ 84: } 83,
{ 85: } 83,
{ 86: } 83,
{ 87: } 86,
{ 88: } 89,
{ 89: } 89,
{ 90: } 89,
{ 91: } 96,
{ 92: } 96,
{ 93: } 100,
{ 94: } 100,
{ 95: } 100,
{ 96: } 100,
{ 97: } 100,
{ 98: } 100,
{ 99: } 100,
{ 100: } 100,
{ 101: } 100,
{ 102: } 100,
{ 103: } 103,
{ 104: } 103,
{ 105: } 103,
{ 106: } 103,
{ 107: } 103,
{ 108: } 103,
{ 109: } 111,
{ 110: } 111,
{ 111: } 111,
{ 112: } 111,
{ 113: } 113,
{ 114: } 115,
{ 115: } 117,
{ 116: } 117,
{ 117: } 117,
{ 118: } 120,
{ 119: } 120,
{ 120: } 127,
{ 121: } 131,
{ 122: } 131,
{ 123: } 134,
{ 124: } 134,
{ 125: } 138,
{ 126: } 142,
{ 127: } 142,
{ 128: } 143,
{ 129: } 144,
{ 130: } 145,
{ 131: } 146,
{ 132: } 146,
{ 133: } 146,
{ 134: } 146,
{ 135: } 146,
{ 136: } 146,
{ 137: } 149,
{ 138: } 149,
{ 139: } 153,
{ 140: } 153,
{ 141: } 153,
{ 142: } 154,
{ 143: } 154,
{ 144: } 154,
{ 145: } 158,
{ 146: } 162,
{ 147: } 162,
{ 148: } 168,
{ 149: } 168,
{ 150: } 168,
{ 151: } 168,
{ 152: } 169,
{ 153: } 169,
{ 154: } 169,
{ 155: } 169,
{ 156: } 169,
{ 157: } 170,
{ 158: } 170,
{ 159: } 170,
{ 160: } 174,
{ 161: } 174,
{ 162: } 174,
{ 163: } 174,
{ 164: } 174,
{ 165: } 174,
{ 166: } 174,
{ 167: } 175,
{ 168: } 176,
{ 169: } 176,
{ 170: } 176,
{ 171: } 180,
{ 172: } 180,
{ 173: } 180,
{ 174: } 180,
{ 175: } 180,
{ 176: } 180,
{ 177: } 183,
{ 178: } 183,
{ 179: } 183,
{ 180: } 183,
{ 181: } 183,
{ 182: } 183,
{ 183: } 183,
{ 184: } 187,
{ 185: } 191,
{ 186: } 195,
{ 187: } 199,
{ 188: } 203,
{ 189: } 207,
{ 190: } 212,
{ 191: } 216,
{ 192: } 220,
{ 193: } 224,
{ 194: } 228,
{ 195: } 232,
{ 196: } 236,
{ 197: } 240,
{ 198: } 244,
{ 199: } 248,
{ 200: } 248,
{ 201: } 250,
{ 202: } 250,
{ 203: } 253,
{ 204: } 260,
{ 205: } 260,
{ 206: } 260,
{ 207: } 261,
{ 208: } 262,
{ 209: } 266,
{ 210: } 270,
{ 211: } 274,
{ 212: } 274,
{ 213: } 274,
{ 214: } 274,
{ 215: } 274,
{ 216: } 281,
{ 217: } 281,
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
{ 235: } 287,
{ 236: } 287,
{ 237: } 287,
{ 238: } 287,
{ 239: } 289,
{ 240: } 289,
{ 241: } 289,
{ 242: } 289,
{ 243: } 293,
{ 244: } 293,
{ 245: } 300,
{ 246: } 304,
{ 247: } 305,
{ 248: } 309,
{ 249: } 310,
{ 250: } 311,
{ 251: } 312,
{ 252: } 312,
{ 253: } 312,
{ 254: } 312,
{ 255: } 312,
{ 256: } 316,
{ 257: } 318,
{ 258: } 318,
{ 259: } 319,
{ 260: } 319,
{ 261: } 319,
{ 262: } 319,
{ 263: } 319,
{ 264: } 319,
{ 265: } 319,
{ 266: } 319
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
{ 65: } 59,
{ 66: } 63,
{ 67: } 64,
{ 68: } 64,
{ 69: } 66,
{ 70: } 69,
{ 71: } 69,
{ 72: } 72,
{ 73: } 72,
{ 74: } 72,
{ 75: } 72,
{ 76: } 72,
{ 77: } 73,
{ 78: } 74,
{ 79: } 75,
{ 80: } 76,
{ 81: } 79,
{ 82: } 82,
{ 83: } 82,
{ 84: } 82,
{ 85: } 82,
{ 86: } 85,
{ 87: } 88,
{ 88: } 88,
{ 89: } 88,
{ 90: } 95,
{ 91: } 95,
{ 92: } 99,
{ 93: } 99,
{ 94: } 99,
{ 95: } 99,
{ 96: } 99,
{ 97: } 99,
{ 98: } 99,
{ 99: } 99,
{ 100: } 99,
{ 101: } 99,
{ 102: } 102,
{ 103: } 102,
{ 104: } 102,
{ 105: } 102,
{ 106: } 102,
{ 107: } 102,
{ 108: } 110,
{ 109: } 110,
{ 110: } 110,
{ 111: } 110,
{ 112: } 112,
{ 113: } 114,
{ 114: } 116,
{ 115: } 116,
{ 116: } 116,
{ 117: } 119,
{ 118: } 119,
{ 119: } 126,
{ 120: } 130,
{ 121: } 130,
{ 122: } 133,
{ 123: } 133,
{ 124: } 137,
{ 125: } 141,
{ 126: } 141,
{ 127: } 142,
{ 128: } 143,
{ 129: } 144,
{ 130: } 145,
{ 131: } 145,
{ 132: } 145,
{ 133: } 145,
{ 134: } 145,
{ 135: } 145,
{ 136: } 148,
{ 137: } 148,
{ 138: } 152,
{ 139: } 152,
{ 140: } 152,
{ 141: } 153,
{ 142: } 153,
{ 143: } 153,
{ 144: } 157,
{ 145: } 161,
{ 146: } 161,
{ 147: } 167,
{ 148: } 167,
{ 149: } 167,
{ 150: } 167,
{ 151: } 168,
{ 152: } 168,
{ 153: } 168,
{ 154: } 168,
{ 155: } 168,
{ 156: } 169,
{ 157: } 169,
{ 158: } 169,
{ 159: } 173,
{ 160: } 173,
{ 161: } 173,
{ 162: } 173,
{ 163: } 173,
{ 164: } 173,
{ 165: } 173,
{ 166: } 174,
{ 167: } 175,
{ 168: } 175,
{ 169: } 175,
{ 170: } 179,
{ 171: } 179,
{ 172: } 179,
{ 173: } 179,
{ 174: } 179,
{ 175: } 179,
{ 176: } 182,
{ 177: } 182,
{ 178: } 182,
{ 179: } 182,
{ 180: } 182,
{ 181: } 182,
{ 182: } 182,
{ 183: } 186,
{ 184: } 190,
{ 185: } 194,
{ 186: } 198,
{ 187: } 202,
{ 188: } 206,
{ 189: } 211,
{ 190: } 215,
{ 191: } 219,
{ 192: } 223,
{ 193: } 227,
{ 194: } 231,
{ 195: } 235,
{ 196: } 239,
{ 197: } 243,
{ 198: } 247,
{ 199: } 247,
{ 200: } 249,
{ 201: } 249,
{ 202: } 252,
{ 203: } 259,
{ 204: } 259,
{ 205: } 259,
{ 206: } 260,
{ 207: } 261,
{ 208: } 265,
{ 209: } 269,
{ 210: } 273,
{ 211: } 273,
{ 212: } 273,
{ 213: } 273,
{ 214: } 273,
{ 215: } 280,
{ 216: } 280,
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
{ 234: } 286,
{ 235: } 286,
{ 236: } 286,
{ 237: } 286,
{ 238: } 288,
{ 239: } 288,
{ 240: } 288,
{ 241: } 288,
{ 242: } 292,
{ 243: } 292,
{ 244: } 299,
{ 245: } 303,
{ 246: } 304,
{ 247: } 308,
{ 248: } 309,
{ 249: } 310,
{ 250: } 311,
{ 251: } 311,
{ 252: } 311,
{ 253: } 311,
{ 254: } 311,
{ 255: } 315,
{ 256: } 317,
{ 257: } 317,
{ 258: } 318,
{ 259: } 318,
{ 260: } 318,
{ 261: } 318,
{ 262: } 318,
{ 263: } 318,
{ 264: } 318,
{ 265: } 318,
{ 266: } 318
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
{ 61: } ( len: 4; sym: -23 ),
{ 62: } ( len: 2; sym: -23 ),
{ 63: } ( len: 3; sym: -23 ),
{ 64: } ( len: 3; sym: -23 ),
{ 65: } ( len: 2; sym: -23 ),
{ 66: } ( len: 1; sym: -23 ),
{ 67: } ( len: 2; sym: -23 ),
{ 68: } ( len: 2; sym: -23 ),
{ 69: } ( len: 1; sym: -23 ),
{ 70: } ( len: 1; sym: -23 ),
{ 71: } ( len: 1; sym: -23 ),
{ 72: } ( len: 1; sym: -21 ),
{ 73: } ( len: 1; sym: -21 ),
{ 74: } ( len: 3; sym: -12 ),
{ 75: } ( len: 4; sym: -12 ),
{ 76: } ( len: 2; sym: -12 ),
{ 77: } ( len: 1; sym: -12 ),
{ 78: } ( len: 2; sym: -24 ),
{ 79: } ( len: 3; sym: -24 ),
{ 80: } ( len: 2; sym: -24 ),
{ 81: } ( len: 1; sym: -15 ),
{ 82: } ( len: 3; sym: -15 ),
{ 83: } ( len: 1; sym: -15 ),
{ 84: } ( len: 1; sym: -26 ),
{ 85: } ( len: 1; sym: -26 ),
{ 86: } ( len: 1; sym: -26 ),
{ 87: } ( len: 2; sym: -14 ),
{ 88: } ( len: 3; sym: -14 ),
{ 89: } ( len: 2; sym: -14 ),
{ 90: } ( len: 2; sym: -14 ),
{ 91: } ( len: 3; sym: -14 ),
{ 92: } ( len: 3; sym: -14 ),
{ 93: } ( len: 1; sym: -14 ),
{ 94: } ( len: 4; sym: -14 ),
{ 95: } ( len: 2; sym: -14 ),
{ 96: } ( len: 4; sym: -14 ),
{ 97: } ( len: 3; sym: -14 ),
{ 98: } ( len: 2; sym: -29 ),
{ 99: } ( len: 3; sym: -29 ),
{ 100: } ( len: 2; sym: -25 ),
{ 101: } ( len: 3; sym: -25 ),
{ 102: } ( len: 2; sym: -25 ),
{ 103: } ( len: 4; sym: -25 ),
{ 104: } ( len: 2; sym: -25 ),
{ 105: } ( len: 4; sym: -25 ),
{ 106: } ( len: 3; sym: -25 ),
{ 107: } ( len: 0; sym: -25 ),
{ 108: } ( len: 1; sym: -27 ),
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
{ 121: } ( len: 3; sym: -30 ),
{ 122: } ( len: 3; sym: -30 ),
{ 123: } ( len: 3; sym: -30 ),
{ 124: } ( len: 3; sym: -30 ),
{ 125: } ( len: 1; sym: -30 ),
{ 126: } ( len: 3; sym: -31 ),
{ 127: } ( len: 1; sym: -33 ),
{ 128: } ( len: 0; sym: -33 ),
{ 129: } ( len: 1; sym: -32 ),
{ 130: } ( len: 1; sym: -32 ),
{ 131: } ( len: 1; sym: -32 ),
{ 132: } ( len: 3; sym: -32 ),
{ 133: } ( len: 3; sym: -32 ),
{ 134: } ( len: 2; sym: -32 ),
{ 135: } ( len: 2; sym: -32 ),
{ 136: } ( len: 2; sym: -32 ),
{ 137: } ( len: 4; sym: -32 ),
{ 138: } ( len: 4; sym: -32 ),
{ 139: } ( len: 5; sym: -32 ),
{ 140: } ( len: 6; sym: -32 ),
{ 141: } ( len: 4; sym: -32 ),
{ 142: } ( len: 3; sym: -32 ),
{ 143: } ( len: 3; sym: -16 ),
{ 144: } ( len: 1; sym: -16 ),
{ 145: } ( len: 0; sym: -16 ),
{ 146: } ( len: 3; sym: -35 ),
{ 147: } ( len: 1; sym: -35 ),
{ 148: } ( len: 1; sym: -17 ),
{ 149: } ( len: 3; sym: -34 ),
{ 150: } ( len: 1; sym: -34 ),
{ 151: } ( len: 0; sym: -34 ),
{ 152: } ( len: 1; sym: -36 )
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
  i : longint;
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
     writeln(outfile,'{ The following command line parameters were used:');
     for i:=1 to paramcount do
       writeln(outfile,'    ',paramstr(i));
     writeln(outfile,'}');
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
 Revision 1.7  2000-04-01 20:19:37  florian
   + implemented support for 64 bit int types
   + options are written now to output file
   * improved compact mode
   * fixed writing of variables

 Revision 1.6  2000/04/01 14:16:32  peter
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