
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
         writeln(outfile,'*)');
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
  23 : begin
         
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
  24 : begin
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
  25 : begin
         
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
  26 : begin
         
         writeln(outfile,'{$define ',yyv[yysp-2]^.p,'}');
         flush(outfile);
         if assigned(yyv[yysp-2])then
         dispose(yyv[yysp-2],done);
         
       end;
  27 : begin
         
         writeln(outfile,'{$define ',yyv[yysp-1]^.p,'}');
         flush(outfile);
         if assigned(yyv[yysp-1])then
         dispose(yyv[yysp-1],done);
         
       end;
  28 : begin
         
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
  29 : begin
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
  30 : begin
         yyval:=yyv[yysp-1];
       end;
  31 : begin
         writeln(outfile,' in member_list *)');
         yyerrok;
         yyval:=nil;
         
       end;
  32 : begin
         yyval:=yyv[yysp-1];
       end;
  33 : begin
         writeln(outfile,' in enum_list *)');
         yyerrok;
         yyval:=nil;
         
       end;
  34 : begin
         
         if not is_packed then
         writeln(outfile,'{$PACKRECORDS 1}');
         is_packed:=true;
         yyval:=new(presobject,init_two(t_structdef,yyv[yysp-1],yyv[yysp-2]));
         
       end;
  35 : begin
         
         if is_packed then
         writeln(outfile,'{$PACKRECORDS 4}');
         is_packed:=false;
         yyval:=new(presobject,init_two(t_structdef,yyv[yysp-0],yyv[yysp-1]));
         
       end;
  36 : begin
         
         if not is_packed then
         writeln(outfile,'{$PACKRECORDS 1}');
         is_packed:=true;
         yyval:=new(presobject,init_two(t_uniondef,yyv[yysp-1],yyv[yysp-2]));
         
       end;
  37 : begin
         
         yyval:=new(presobject,init_two(t_uniondef,yyv[yysp-0],yyv[yysp-1]));
         
       end;
  38 : begin
         
         yyval:=new(presobject,init_two(t_uniondef,nil,yyv[yysp-0]));
         
       end;
  39 : begin
         
         yyval:=new(presobject,init_two(t_structdef,nil,yyv[yysp-0]));
         
       end;
  40 : begin
         
         yyval:=new(presobject,init_two(t_enumdef,yyv[yysp-0],yyv[yysp-1]));
         
       end;
  41 : begin
         
         yyval:=new(presobject,init_two(t_enumdef,nil,yyv[yysp-0]));
         
       end;
  42 : begin
         
         if not stripinfo then
         writeln(outfile,'(* Const before type ignored *)');
         yyval:=yyv[yysp-0];
         
       end;
  43 : begin
         
         if not is_packed then
         writeln(outfile,'{$PACKRECORDS 1}');
         is_packed:=true;
         yyval:=new(presobject,init_one(t_uniondef,yyv[yysp-1]));
         
       end;
  44 : begin
         
         yyval:=new(presobject,init_one(t_uniondef,yyv[yysp-0]));
         
       end;
  45 : begin
         
         if not is_packed then
         writeln(outfile,'{$PACKRECORDS 1}');
         is_packed:=true;
         yyval:=new(presobject,init_one(t_structdef,yyv[yysp-1]));
         
       end;
  46 : begin
         
         if is_packed then
         writeln(outfile,'{$PACKRECORDS 4}');
         is_packed:=false;
         yyval:=new(presobject,init_one(t_structdef,yyv[yysp-0]));
         
       end;
  47 : begin
         
         yyval:=new(presobject,init_one(t_enumdef,yyv[yysp-0]));
         
       end;
  48 : begin
         
         yyval:=yyv[yysp-0];
         
       end;
  49 : begin
         yyval:=yyv[yysp-0]; 
       end;
  50 : begin
         
         yyval:=new(presobject,init_one(t_memberdeclist,yyv[yysp-1]));
         yyval^.next:=yyv[yysp-0];
         
       end;
  51 : begin
         
         yyval:=new(presobject,init_one(t_memberdeclist,yyv[yysp-0]));
         
       end;
  52 : begin
         
         yyval:=new(presobject,init_two(t_memberdec,yyv[yysp-2],yyv[yysp-1]));
         
       end;
  53 : begin
         (*dname*)
         yyval:=new(presobject,init_id(act_token));
         
       end;
  54 : begin
         
         yyval:=new(presobject,init_id(INT_STR));
         
       end;
  55 : begin
         
         yyval:=new(presobject,init_id(UINT_STR));
         
       end;
  56 : begin
         
         yyval:=new(presobject,init_id(INT_STR));
         
       end;
  57 : begin
         
         yyval:=new(presobject,init_id(REAL_STR));
         
       end;
  58 : begin
         
         yyval:=new(presobject,init_id(INT_STR));
         
       end;
  59 : begin
         
         yyval:=new(presobject,init_id(UINT_STR));
         
       end;
  60 : begin
         
         yyval:=new(presobject,init_id(UINT_STR));
         
       end;
  61 : begin
         
         yyval:=new(presobject,init_id(UINT_STR));
         
       end;
  62 : begin
         
         yyval:=new(presobject,init_id(USHORT_STR));
         
       end;
  63 : begin
         
         yyval:=new(presobject,init_id(UCHAR_STR));
         
       end;
  64 : begin
         
         yyval:=new(presobject,init_no(t_void));
         
       end;
  65 : begin
         
         yyval:=new(presobject,init_id(SHORT_STR));
         
       end;
  66 : begin
         
         yyval:=new(presobject,init_id(CHAR_STR));
         
       end;
  67 : begin
         
         yyval:=yyv[yysp-0];
         
       end;
  68 : begin
         
         yyval:=yyv[yysp-0];
         tn:=yyval^.str;
         if removeunderscore and
         (length(tn)>1) and (tn[1]='_') then
         yyval^.setstr(Copy(tn,2,length(tn)-1));
         
       end;
  69 : begin
         
         yyval:=yyv[yysp-2];
         hp:=yyv[yysp-2];
         while assigned(hp^.next) do
         hp:=hp^.next;
         hp^.next:=new(presobject,init_one(t_declist,yyv[yysp-0]));
         
       end;
  70 : begin
         
         writeln(outfile,' in declarator_list *)');
         yyval:=yyv[yysp-0];
         yyerrok;
         
       end;
  71 : begin
         
         writeln(outfile,' in declarator_list *)');
         yyerrok;
         
       end;
  72 : begin
         
         yyval:=new(presobject,init_one(t_declist,yyv[yysp-0]));
         
       end;
  73 : begin
         
         yyval:=new(presobject,init_two(t_arg,yyv[yysp-1],yyv[yysp-0]));
         
       end;
  74 : begin
         
         hp:=new(presobject,init_one(t_pointerdef,yyv[yysp-2]));
         yyval:=new(presobject,init_two(t_arg,hp,yyv[yysp-0]));
         
       end;
  75 : begin
         
         yyval:=new(presobject,init_two(t_arg,yyv[yysp-1],yyv[yysp-0]));
         
       end;
  76 : begin
         
         yyval:=new(presobject,init_two(t_arglist,yyv[yysp-0],nil));
         
       end;
  77 : begin
         
         yyval:=new(presobject,init_two(t_arglist,yyv[yysp-2],nil));
         yyval^.next:=yyv[yysp-0];
         
       end;
  78 : begin
         
         yyval:=new(presobject,init_two(t_arglist,ellipsisarg,nil));
         (*** ELLIPSIS PROBLEM ***)
         
       end;
  79 : begin
         yyval:=new(presobject,init_id('far'));
       end;
  80 : begin
         yyval:=new(presobject,init_id('near'));
       end;
  81 : begin
         yyval:=new(presobject,init_id('huge'));
       end;
  82 : begin
         
         if not stripinfo then
         writeln(outfile,'(* Const before declarator ignored *)');
         yyval:=yyv[yysp-0];
         
       end;
  83 : begin
         
         if not stripinfo then
         writeln(outfile,aktspace,'(* ',yyv[yysp-2]^.p,' ignored *)');
         dispose(yyv[yysp-2],done);
         hp:=yyv[yysp-0];
         yyval:=hp;
         while assigned(hp^.p1) do
         hp:=hp^.p1;
         hp^.p1:=new(presobject,init_one(t_pointerdef,nil));
         
       end;
  84 : begin
         
         (* %prec PSTAR this was wrong!! *)
         hp:=yyv[yysp-0];
         yyval:=hp;
         while assigned(hp^.p1) do
         hp:=hp^.p1;
         hp^.p1:=new(presobject,init_one(t_pointerdef,nil));
         
       end;
  85 : begin
         
         hp:=yyv[yysp-0];
         yyval:=hp;
         while assigned(hp^.p1) do
         hp:=hp^.p1;
         hp^.p1:=new(presobject,init_one(t_addrdef,nil));
         
       end;
  86 : begin
         
         (*  size specifier supported *)
         hp:=new(presobject,init_one(t_size_specifier,yyv[yysp-0]));
         yyval:=new(presobject,init_three(t_dec,nil,yyv[yysp-2],hp));
         
       end;
  87 : begin
         
         if not stripinfo then
         writeln(outfile,'(* Warning : default value for ',yyv[yysp-2]^.p,' ignored *)');
         hp:=new(presobject,init_one(t_default_value,yyv[yysp-0]));
         yyval:=new(presobject,init_three(t_dec,nil,yyv[yysp-2],hp));
         
       end;
  88 : begin
         
         yyval:=new(presobject,init_two(t_dec,nil,yyv[yysp-0]));
         
       end;
  89 : begin
         
         hp:=yyv[yysp-3];
         yyval:=hp;
         while assigned(hp^.p1) do
         hp:=hp^.p1;
         hp^.p1:=new(presobject,init_two(t_procdef,nil,yyv[yysp-1]));
         
       end;
  90 : begin
         
         hp:=yyv[yysp-1];
         yyval:=hp;
         while assigned(hp^.p1) do
         hp:=hp^.p1;
         hp^.p1:=new(presobject,init_two(t_procdef,nil,nil));
         
       end;
  91 : begin
         
         hp:=yyv[yysp-3];
         yyval:=hp;
         while assigned(hp^.p1) do
         hp:=hp^.p1;
         hp^.p1:=new(presobject,init_two(t_arraydef,nil,yyv[yysp-1]));
         
       end;
  92 : begin
         yyval:=yyv[yysp-1]; 
       end;
  93 : begin
         yyval := yyv[yysp-1];
       end;
  94 : begin
         yyval := yyv[yysp-2];
       end;
  95 : begin
         
         if not stripinfo then
         writeln(outfile,'(* Const before abstract_declarator ignored *)');
         yyval:=yyv[yysp-0];
         
       end;
  96 : begin
         
         if not stripinfo then
         writeln(outfile,aktspace,'(* ',yyv[yysp-2]^.p,' ignored *)');
         dispose(yyv[yysp-2],done);
         hp:=yyv[yysp-0];
         yyval:=hp;
         while assigned(hp^.p1) do
         hp:=hp^.p1;
         hp^.p1:=new(presobject,init_one(t_pointerdef,nil));
         
       end;
  97 : begin
         
         hp:=yyv[yysp-0];
         yyval:=hp;
         while assigned(hp^.p1) do
         hp:=hp^.p1;
         hp^.p1:=new(presobject,init_one(t_pointerdef,nil));
         
       end;
  98 : begin
         
         hp:=yyv[yysp-3];
         yyval:=hp;
         while assigned(hp^.p1) do
         hp:=hp^.p1;
         hp^.p1:=new(presobject,init_two(t_procdef,nil,yyv[yysp-1]));
         
       end;
  99 : begin
         
         hp:=yyv[yysp-1];
         yyval:=hp;
         while assigned(hp^.p1) do
         hp:=hp^.p1;
         hp^.p1:=new(presobject,init_two(t_procdef,nil,nil));
         
       end;
 100 : begin
         
         hp:=yyv[yysp-3];
         yyval:=hp;
         while assigned(hp^.p1) do
         hp:=hp^.p1;
         hp^.p1:=new(presobject,init_two(t_arraydef,nil,yyv[yysp-1]));
         
       end;
 101 : begin
         yyval:=yyv[yysp-1]; 
       end;
 102 : begin
         
         yyval:=new(presobject,init_two(t_dec,nil,nil));
         
       end;
 103 : begin
         yyval:=yyv[yysp-0];
       end;
 104 : begin
         yyval:=new(presobject,init_bop(' = ',yyv[yysp-2],yyv[yysp-0]));
       end;
 105 : begin
         yyval:=new(presobject,init_bop(' <> ',yyv[yysp-2],yyv[yysp-0]));
       end;
 106 : begin
         yyval:=new(presobject,init_bop(' > ',yyv[yysp-2],yyv[yysp-0]));
       end;
 107 : begin
         yyval:=new(presobject,init_bop(' >= ',yyv[yysp-2],yyv[yysp-0]));
       end;
 108 : begin
         yyval:=new(presobject,init_bop(' < ',yyv[yysp-2],yyv[yysp-0]));
       end;
 109 : begin
         yyval:=new(presobject,init_bop(' <= ',yyv[yysp-2],yyv[yysp-0]));
       end;
 110 : begin
         yyval:=new(presobject,init_bop(' + ',yyv[yysp-2],yyv[yysp-0]));
       end;
 111 : begin
         yyval:=new(presobject,init_bop(' - ',yyv[yysp-2],yyv[yysp-0]));
       end;
 112 : begin
         yyval:=new(presobject,init_bop(' * ',yyv[yysp-2],yyv[yysp-0]));
       end;
 113 : begin
         yyval:=new(presobject,init_bop(' / ',yyv[yysp-2],yyv[yysp-0]));
       end;
 114 : begin
         yyval:=new(presobject,init_bop(' or ',yyv[yysp-2],yyv[yysp-0]));
       end;
 115 : begin
         yyval:=new(presobject,init_bop(' and ',yyv[yysp-2],yyv[yysp-0]));
       end;
 116 : begin
         yyval:=new(presobject,init_bop(' not ',yyv[yysp-2],yyv[yysp-0]));
       end;
 117 : begin
         yyval:=new(presobject,init_bop(' shl ',yyv[yysp-2],yyv[yysp-0]));
       end;
 118 : begin
         yyval:=new(presobject,init_bop(' shr ',yyv[yysp-2],yyv[yysp-0]));
       end;
 119 : begin
         yyv[yysp-0]^.p1:=yyv[yysp-2];
         yyval:=yyv[yysp-0];
         inc(if_nb);
         yyval^.p:=strpnew('if_local'+str(if_nb));
         
       end;
 120 : begin
         yyval:=yyv[yysp-0];
       end;
 121 : begin
         (* if A then B else C *)
         yyval:=new(presobject,init_three(t_ifexpr,nil,yyv[yysp-2],yyv[yysp-0]));
       end;
 122 : begin
         yyval:=yyv[yysp-0]; 
       end;
 123 : begin
         yyval:=nil;
       end;
 124 : begin
         
         yyval:=yyv[yysp-0];
         
       end;
 125 : begin
         
         (* remove L prefix for widestrings *)
         s:=act_token;
         if Win32headers and (s[1]='L') then
         delete(s,1,1);
         yyval:=new(presobject,init_id(''''+copy(s,2,length(s)-2)+''''));
         
       end;
 126 : begin
         
         yyval:=new(presobject,init_id(act_token));
         
       end;
 127 : begin
         
         yyval:=new(presobject,init_bop('.',yyv[yysp-2],yyv[yysp-0]));
         
       end;
 128 : begin
         
         yyval:=new(presobject,init_bop('^.',yyv[yysp-2],yyv[yysp-0]));
         
       end;
 129 : begin
         
         yyval:=new(presobject,init_preop('-',yyv[yysp-0]));
         
       end;
 130 : begin
         
         yyval:=new(presobject,init_preop('@',yyv[yysp-0]));
         
       end;
 131 : begin
         
         yyval:=new(presobject,init_preop(' not ',yyv[yysp-0]));
         
       end;
 132 : begin
         
         if assigned(yyv[yysp-0]) then
         yyval:=new(presobject,init_two(t_typespec,yyv[yysp-2],yyv[yysp-0]))
         else
         yyval:=yyv[yysp-2];
         
       end;
 133 : begin
         
         yyval:=new(presobject,init_two(t_typespec,yyv[yysp-2],yyv[yysp-0]));
         
       end;
 134 : begin
         
         hp:=new(presobject,init_one(t_pointerdef,yyv[yysp-3]));
         yyval:=new(presobject,init_two(t_typespec,hp,yyv[yysp-0]));
         
       end;
 135 : begin
         
         if not stripinfo then
         writeln(outfile,aktspace,'(* ',yyv[yysp-3]^.p,' ignored *)');
         dispose(yyv[yysp-3],done);
         write_type_specifier(outfile,yyv[yysp-4]);
         writeln(outfile,' ignored *)');
         hp:=new(presobject,init_one(t_pointerdef,yyv[yysp-4]));
         yyval:=new(presobject,init_two(t_typespec,hp,yyv[yysp-0]));
         
       end;
 136 : begin
         
         hp:=new(presobject,init_one(t_exprlist,yyv[yysp-3]));
         yyval:=new(presobject,init_three(t_funexprlist,hp,yyv[yysp-1],nil));
         
       end;
 137 : begin
         
         yyval:=yyv[yysp-1];
         
       end;
 138 : begin
         (*enum_element COMMA enum_list *)
         yyval:=yyv[yysp-2];
         yyval^.next:=yyv[yysp-0];
         
       end;
 139 : begin
         
         yyval:=yyv[yysp-0];
         
       end;
 140 : begin
         (* empty enum list *)
         yyval:=nil;
       end;
 141 : begin
         begin (*enum_element: dname _ASSIGN expr *)
         yyval:=new(presobject,init_two(t_enumlist,yyv[yysp-2],yyv[yysp-0]));
         end;
         
       end;
 142 : begin
         
         begin (*enum_element: dname*)
         yyval:=new(presobject,init_two(t_enumlist,yyv[yysp-0],nil));
         end;
         
       end;
 143 : begin
         
         if yyv[yysp-0]^.typ=t_funexprlist then
         yyval:=yyv[yysp-0]
         else
         yyval:=new(presobject,init_two(t_exprlist,yyv[yysp-0],nil));
         (* if here is a type specifier
         we know the return type *)
         if (yyv[yysp-0]^.typ=t_typespec) then
         yyval^.p3:=yyv[yysp-0]^.p1^.get_copy;
         
       end;
 144 : begin
         (*exprlist COMMA expr*)
         yyval:=yyv[yysp-2];
         yyv[yysp-2]^.next:=yyv[yysp-0];
         
       end;
 145 : begin
         
         yyval:=yyv[yysp-0];
         
       end;
 146 : begin
         (* empty expression list *)
         yyval:=nil; 
       end;
 147 : begin
         
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

yynacts   = 1898;
yyngotos  = 306;
yynstates = 256;
yynrules  = 147;

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
{ 26: }
  ( sym: 277; act: 61 ),
  ( sym: 256; act: -56 ),
  ( sym: 261; act: -56 ),
  ( sym: 262; act: -56 ),
  ( sym: 263; act: -56 ),
  ( sym: 264; act: -56 ),
  ( sym: 271; act: -56 ),
  ( sym: 281; act: -56 ),
  ( sym: 282; act: -56 ),
  ( sym: 283; act: -56 ),
  ( sym: 284; act: -56 ),
  ( sym: 289; act: -56 ),
  ( sym: 290; act: -56 ),
  ( sym: 291; act: -56 ),
  ( sym: 292; act: -56 ),
  ( sym: 293; act: -56 ),
  ( sym: 294; act: -56 ),
  ( sym: 295; act: -56 ),
  ( sym: 308; act: -56 ),
  ( sym: 313; act: -56 ),
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
{ 36: }
  ( sym: 260; act: 66 ),
  ( sym: 256; act: -68 ),
  ( sym: 262; act: -68 ),
  ( sym: 271; act: -68 ),
  ( sym: 281; act: -68 ),
  ( sym: 282; act: -68 ),
  ( sym: 283; act: -68 ),
  ( sym: 284; act: -68 ),
  ( sym: 289; act: -68 ),
  ( sym: 290; act: -68 ),
  ( sym: 291; act: -68 ),
  ( sym: 292; act: -68 ),
  ( sym: 293; act: -68 ),
  ( sym: 294; act: -68 ),
  ( sym: 295; act: -68 ),
  ( sym: 308; act: -68 ),
  ( sym: 313; act: -68 ),
{ 37: }
  ( sym: 256; act: 51 ),
  ( sym: 266; act: 52 ),
  ( sym: 271; act: 23 ),
{ 38: }
  ( sym: 262; act: 68 ),
  ( sym: 286; act: 69 ),
  ( sym: 287; act: 70 ),
{ 39: }
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
{ 40: }
  ( sym: 256; act: 51 ),
  ( sym: 266; act: 52 ),
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
{ 41: }
  ( sym: 256; act: 55 ),
  ( sym: 266; act: 56 ),
  ( sym: 260; act: -41 ),
  ( sym: 261; act: -41 ),
  ( sym: 262; act: -41 ),
  ( sym: 263; act: -41 ),
  ( sym: 264; act: -41 ),
  ( sym: 271; act: -41 ),
  ( sym: 281; act: -41 ),
  ( sym: 282; act: -41 ),
  ( sym: 283; act: -41 ),
  ( sym: 284; act: -41 ),
  ( sym: 289; act: -41 ),
  ( sym: 290; act: -41 ),
  ( sym: 291; act: -41 ),
  ( sym: 292; act: -41 ),
  ( sym: 293; act: -41 ),
  ( sym: 294; act: -41 ),
  ( sym: 295; act: -41 ),
  ( sym: 308; act: -41 ),
  ( sym: 313; act: -41 ),
{ 42: }
  ( sym: 256; act: 78 ),
  ( sym: 262; act: 79 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 80 ),
  ( sym: 282; act: 81 ),
  ( sym: 283; act: 82 ),
  ( sym: 284; act: 83 ),
  ( sym: 308; act: 84 ),
  ( sym: 313; act: 85 ),
{ 43: }
{ 44: }
{ 45: }
{ 46: }
{ 47: }
{ 48: }
{ 49: }
{ 50: }
  ( sym: 297; act: 86 ),
  ( sym: 256; act: -46 ),
  ( sym: 261; act: -46 ),
  ( sym: 262; act: -46 ),
  ( sym: 263; act: -46 ),
  ( sym: 264; act: -46 ),
  ( sym: 271; act: -46 ),
  ( sym: 281; act: -46 ),
  ( sym: 282; act: -46 ),
  ( sym: 283; act: -46 ),
  ( sym: 284; act: -46 ),
  ( sym: 289; act: -46 ),
  ( sym: 290; act: -46 ),
  ( sym: 291; act: -46 ),
  ( sym: 292; act: -46 ),
  ( sym: 293; act: -46 ),
  ( sym: 294; act: -46 ),
  ( sym: 295; act: -46 ),
  ( sym: 308; act: -46 ),
  ( sym: 313; act: -46 ),
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
  ( sym: 297; act: 91 ),
  ( sym: 256; act: -44 ),
  ( sym: 261; act: -44 ),
  ( sym: 262; act: -44 ),
  ( sym: 263; act: -44 ),
  ( sym: 264; act: -44 ),
  ( sym: 271; act: -44 ),
  ( sym: 281; act: -44 ),
  ( sym: 282; act: -44 ),
  ( sym: 283; act: -44 ),
  ( sym: 284; act: -44 ),
  ( sym: 289; act: -44 ),
  ( sym: 290; act: -44 ),
  ( sym: 291; act: -44 ),
  ( sym: 292; act: -44 ),
  ( sym: 293; act: -44 ),
  ( sym: 294; act: -44 ),
  ( sym: 295; act: -44 ),
  ( sym: 308; act: -44 ),
  ( sym: 313; act: -44 ),
{ 54: }
{ 55: }
{ 56: }
  ( sym: 271; act: 23 ),
  ( sym: 267; act: -140 ),
{ 57: }
{ 58: }
  ( sym: 277; act: 96 ),
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
{ 59: }
{ 60: }
{ 61: }
{ 62: }
{ 63: }
{ 64: }
{ 65: }
  ( sym: 256; act: 78 ),
  ( sym: 262; act: 79 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 80 ),
  ( sym: 282; act: 81 ),
  ( sym: 283; act: 82 ),
  ( sym: 284; act: 83 ),
  ( sym: 308; act: 84 ),
  ( sym: 313; act: 85 ),
{ 66: }
{ 67: }
  ( sym: 256; act: 51 ),
  ( sym: 266; act: 52 ),
  ( sym: 271; act: 23 ),
  ( sym: 262; act: -39 ),
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
{ 68: }
  ( sym: 271; act: 23 ),
  ( sym: 263; act: -140 ),
{ 69: }
{ 70: }
  ( sym: 262; act: 103 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 104 ),
  ( sym: 273; act: 105 ),
  ( sym: 286; act: 106 ),
  ( sym: 308; act: 107 ),
  ( sym: 310; act: 108 ),
  ( sym: 315; act: 109 ),
{ 71: }
  ( sym: 297; act: 110 ),
  ( sym: 256; act: -35 ),
  ( sym: 260; act: -35 ),
  ( sym: 261; act: -35 ),
  ( sym: 262; act: -35 ),
  ( sym: 263; act: -35 ),
  ( sym: 264; act: -35 ),
  ( sym: 271; act: -35 ),
  ( sym: 281; act: -35 ),
  ( sym: 282; act: -35 ),
  ( sym: 283; act: -35 ),
  ( sym: 284; act: -35 ),
  ( sym: 289; act: -35 ),
  ( sym: 290; act: -35 ),
  ( sym: 291; act: -35 ),
  ( sym: 292; act: -35 ),
  ( sym: 293; act: -35 ),
  ( sym: 294; act: -35 ),
  ( sym: 295; act: -35 ),
  ( sym: 308; act: -35 ),
  ( sym: 313; act: -35 ),
{ 72: }
  ( sym: 297; act: 111 ),
  ( sym: 256; act: -37 ),
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
{ 73: }
{ 74: }
  ( sym: 313; act: 112 ),
{ 75: }
  ( sym: 262; act: 114 ),
  ( sym: 264; act: 115 ),
  ( sym: 260; act: -72 ),
  ( sym: 261; act: -72 ),
  ( sym: 296; act: -72 ),
{ 76: }
  ( sym: 261; act: 117 ),
  ( sym: 296; act: 118 ),
  ( sym: 260; act: -18 ),
{ 77: }
  ( sym: 259; act: 120 ),
  ( sym: 260; act: -88 ),
  ( sym: 261; act: -88 ),
  ( sym: 262; act: -88 ),
  ( sym: 263; act: -88 ),
  ( sym: 264; act: -88 ),
  ( sym: 296; act: -88 ),
{ 78: }
{ 79: }
  ( sym: 262; act: 79 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 80 ),
  ( sym: 282; act: 81 ),
  ( sym: 283; act: 82 ),
  ( sym: 284; act: 83 ),
  ( sym: 308; act: 84 ),
  ( sym: 313; act: 85 ),
{ 80: }
  ( sym: 262; act: 79 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 80 ),
  ( sym: 282; act: 81 ),
  ( sym: 283; act: 82 ),
  ( sym: 284; act: 83 ),
  ( sym: 308; act: 84 ),
  ( sym: 313; act: 85 ),
{ 81: }
{ 82: }
{ 83: }
{ 84: }
  ( sym: 262; act: 79 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 80 ),
  ( sym: 282; act: 81 ),
  ( sym: 283; act: 82 ),
  ( sym: 284; act: 83 ),
  ( sym: 308; act: 84 ),
  ( sym: 313; act: 85 ),
{ 85: }
  ( sym: 262; act: 79 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 80 ),
  ( sym: 282; act: 81 ),
  ( sym: 283; act: 82 ),
  ( sym: 284; act: 83 ),
  ( sym: 308; act: 84 ),
  ( sym: 313; act: 85 ),
{ 86: }
{ 87: }
  ( sym: 267; act: 126 ),
{ 88: }
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
  ( sym: 267; act: -51 ),
{ 89: }
  ( sym: 267; act: 128 ),
{ 90: }
  ( sym: 256; act: 78 ),
  ( sym: 262; act: 79 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 80 ),
  ( sym: 282; act: 81 ),
  ( sym: 283; act: 82 ),
  ( sym: 284; act: 83 ),
  ( sym: 308; act: 84 ),
  ( sym: 313; act: 85 ),
{ 91: }
{ 92: }
  ( sym: 267; act: 130 ),
{ 93: }
  ( sym: 261; act: 131 ),
  ( sym: 263; act: -139 ),
  ( sym: 267; act: -139 ),
{ 94: }
  ( sym: 267; act: 132 ),
{ 95: }
  ( sym: 285; act: 133 ),
  ( sym: 261; act: -142 ),
  ( sym: 263; act: -142 ),
  ( sym: 267; act: -142 ),
{ 96: }
{ 97: }
  ( sym: 260; act: 134 ),
  ( sym: 261; act: 117 ),
{ 98: }
  ( sym: 260; act: 135 ),
{ 99: }
  ( sym: 263; act: 136 ),
{ 100: }
  ( sym: 318; act: 137 ),
  ( sym: 319; act: 138 ),
  ( sym: 286; act: -143 ),
{ 101: }
  ( sym: 286; act: 139 ),
{ 102: }
  ( sym: 262; act: 140 ),
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
  ( sym: 308; act: -124 ),
  ( sym: 309; act: -124 ),
  ( sym: 310; act: -124 ),
  ( sym: 311; act: -124 ),
  ( sym: 312; act: -124 ),
  ( sym: 313; act: -124 ),
  ( sym: 314; act: -124 ),
  ( sym: 315; act: -124 ),
  ( sym: 318; act: -124 ),
  ( sym: 319; act: -124 ),
{ 103: }
  ( sym: 262; act: 103 ),
  ( sym: 268; act: 20 ),
  ( sym: 269; act: 21 ),
  ( sym: 270; act: 22 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 104 ),
  ( sym: 273; act: 105 ),
  ( sym: 274; act: 24 ),
  ( sym: 275; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 277; act: 27 ),
  ( sym: 278; act: 28 ),
  ( sym: 279; act: 29 ),
  ( sym: 280; act: 30 ),
  ( sym: 281; act: 31 ),
  ( sym: 308; act: 107 ),
  ( sym: 310; act: 108 ),
  ( sym: 315; act: 109 ),
{ 104: }
{ 105: }
{ 106: }
{ 107: }
  ( sym: 262; act: 103 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 104 ),
  ( sym: 273; act: 105 ),
  ( sym: 308; act: 107 ),
  ( sym: 310; act: 108 ),
  ( sym: 315; act: 109 ),
{ 108: }
  ( sym: 262; act: 103 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 104 ),
  ( sym: 273; act: 105 ),
  ( sym: 308; act: 107 ),
  ( sym: 310; act: 108 ),
  ( sym: 315; act: 109 ),
{ 109: }
  ( sym: 262; act: 103 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 104 ),
  ( sym: 273; act: 105 ),
  ( sym: 308; act: 107 ),
  ( sym: 310; act: 108 ),
  ( sym: 315; act: 109 ),
{ 110: }
{ 111: }
{ 112: }
  ( sym: 262; act: 79 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 80 ),
  ( sym: 282; act: 81 ),
  ( sym: 283; act: 82 ),
  ( sym: 284; act: 83 ),
  ( sym: 308; act: 84 ),
  ( sym: 313; act: 85 ),
{ 113: }
{ 114: }
  ( sym: 263; act: 153 ),
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
  ( sym: 280; act: 154 ),
  ( sym: 281; act: 31 ),
  ( sym: 298; act: 155 ),
{ 115: }
  ( sym: 262; act: 103 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 104 ),
  ( sym: 273; act: 105 ),
  ( sym: 308; act: 107 ),
  ( sym: 310; act: 108 ),
  ( sym: 315; act: 109 ),
{ 116: }
  ( sym: 260; act: 158 ),
{ 117: }
  ( sym: 262; act: 79 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 80 ),
  ( sym: 282; act: 81 ),
  ( sym: 283; act: 82 ),
  ( sym: 284; act: 83 ),
  ( sym: 308; act: 84 ),
  ( sym: 313; act: 85 ),
{ 118: }
  ( sym: 262; act: 160 ),
{ 119: }
  ( sym: 262; act: 103 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 104 ),
  ( sym: 273; act: 105 ),
  ( sym: 308; act: 107 ),
  ( sym: 310; act: 108 ),
  ( sym: 315; act: 109 ),
{ 120: }
  ( sym: 262; act: 103 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 104 ),
  ( sym: 273; act: 105 ),
  ( sym: 308; act: 107 ),
  ( sym: 310; act: 108 ),
  ( sym: 315; act: 109 ),
{ 121: }
  ( sym: 261; act: 163 ),
  ( sym: 260; act: -71 ),
  ( sym: 296; act: -71 ),
{ 122: }
  ( sym: 262; act: 114 ),
  ( sym: 263; act: 164 ),
  ( sym: 264; act: 115 ),
{ 123: }
  ( sym: 262; act: 114 ),
  ( sym: 264; act: 115 ),
  ( sym: 260; act: -82 ),
  ( sym: 261; act: -82 ),
  ( sym: 263; act: -82 ),
  ( sym: 296; act: -82 ),
{ 124: }
  ( sym: 264; act: 115 ),
  ( sym: 260; act: -85 ),
  ( sym: 261; act: -85 ),
  ( sym: 262; act: -85 ),
  ( sym: 263; act: -85 ),
  ( sym: 296; act: -85 ),
{ 125: }
  ( sym: 262; act: 114 ),
  ( sym: 264; act: 115 ),
  ( sym: 260; act: -84 ),
  ( sym: 261; act: -84 ),
  ( sym: 263; act: -84 ),
  ( sym: 296; act: -84 ),
{ 126: }
{ 127: }
{ 128: }
{ 129: }
  ( sym: 260; act: 165 ),
  ( sym: 261; act: 117 ),
{ 130: }
{ 131: }
  ( sym: 271; act: 23 ),
  ( sym: 263; act: -140 ),
  ( sym: 267; act: -140 ),
{ 132: }
{ 133: }
  ( sym: 262; act: 103 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 104 ),
  ( sym: 273; act: 105 ),
  ( sym: 308; act: 107 ),
  ( sym: 310; act: 108 ),
  ( sym: 315; act: 109 ),
{ 134: }
{ 135: }
{ 136: }
  ( sym: 287; act: 168 ),
{ 137: }
  ( sym: 262; act: 103 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 104 ),
  ( sym: 273; act: 105 ),
  ( sym: 308; act: 107 ),
  ( sym: 310; act: 108 ),
  ( sym: 315; act: 109 ),
{ 138: }
  ( sym: 262; act: 103 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 104 ),
  ( sym: 273; act: 105 ),
  ( sym: 308; act: 107 ),
  ( sym: 310; act: 108 ),
  ( sym: 315; act: 109 ),
{ 139: }
{ 140: }
  ( sym: 262; act: 103 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 104 ),
  ( sym: 273; act: 105 ),
  ( sym: 308; act: 107 ),
  ( sym: 310; act: 108 ),
  ( sym: 315; act: 109 ),
  ( sym: 263; act: -146 ),
{ 141: }
  ( sym: 318; act: 137 ),
  ( sym: 319; act: 138 ),
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
{ 142: }
  ( sym: 263; act: 174 ),
  ( sym: 300; act: -103 ),
  ( sym: 301; act: -103 ),
  ( sym: 302; act: -103 ),
  ( sym: 303; act: -103 ),
  ( sym: 304; act: -103 ),
  ( sym: 305; act: -103 ),
  ( sym: 306; act: -103 ),
  ( sym: 307; act: -103 ),
  ( sym: 308; act: -103 ),
  ( sym: 309; act: -103 ),
  ( sym: 310; act: -103 ),
  ( sym: 311; act: -103 ),
  ( sym: 312; act: -103 ),
  ( sym: 313; act: -103 ),
  ( sym: 314; act: -103 ),
  ( sym: 315; act: -103 ),
{ 143: }
  ( sym: 300; act: 175 ),
  ( sym: 301; act: 176 ),
  ( sym: 302; act: 177 ),
  ( sym: 303; act: 178 ),
  ( sym: 304; act: 179 ),
  ( sym: 305; act: 180 ),
  ( sym: 306; act: 181 ),
  ( sym: 307; act: 182 ),
  ( sym: 308; act: 183 ),
  ( sym: 309; act: 184 ),
  ( sym: 310; act: 185 ),
  ( sym: 311; act: 186 ),
  ( sym: 312; act: 187 ),
  ( sym: 313; act: 188 ),
  ( sym: 314; act: 189 ),
  ( sym: 315; act: 190 ),
{ 144: }
  ( sym: 263; act: 192 ),
  ( sym: 282; act: 81 ),
  ( sym: 283; act: 82 ),
  ( sym: 284; act: 83 ),
  ( sym: 313; act: 193 ),
{ 145: }
  ( sym: 262; act: 140 ),
  ( sym: 263; act: 194 ),
  ( sym: 282; act: -68 ),
  ( sym: 283; act: -68 ),
  ( sym: 284; act: -68 ),
  ( sym: 313; act: -68 ),
  ( sym: 300; act: -124 ),
  ( sym: 301; act: -124 ),
  ( sym: 302; act: -124 ),
  ( sym: 303; act: -124 ),
  ( sym: 304; act: -124 ),
  ( sym: 305; act: -124 ),
  ( sym: 306; act: -124 ),
  ( sym: 307; act: -124 ),
  ( sym: 308; act: -124 ),
  ( sym: 309; act: -124 ),
  ( sym: 310; act: -124 ),
  ( sym: 311; act: -124 ),
  ( sym: 312; act: -124 ),
  ( sym: 314; act: -124 ),
  ( sym: 315; act: -124 ),
  ( sym: 318; act: -124 ),
  ( sym: 319; act: -124 ),
{ 146: }
  ( sym: 318; act: 137 ),
  ( sym: 319; act: 138 ),
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
{ 147: }
  ( sym: 318; act: 137 ),
  ( sym: 319; act: 138 ),
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
{ 148: }
  ( sym: 318; act: 137 ),
  ( sym: 319; act: 138 ),
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
{ 149: }
  ( sym: 262; act: 114 ),
  ( sym: 264; act: 115 ),
  ( sym: 260; act: -83 ),
  ( sym: 261; act: -83 ),
  ( sym: 263; act: -83 ),
  ( sym: 296; act: -83 ),
{ 150: }
  ( sym: 263; act: 195 ),
{ 151: }
  ( sym: 261; act: 196 ),
  ( sym: 263; act: -76 ),
{ 152: }
  ( sym: 262; act: 200 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 201 ),
  ( sym: 282; act: 81 ),
  ( sym: 283; act: 82 ),
  ( sym: 284; act: 83 ),
  ( sym: 308; act: 84 ),
  ( sym: 313; act: 202 ),
  ( sym: 261; act: -102 ),
  ( sym: 263; act: -102 ),
  ( sym: 264; act: -102 ),
{ 153: }
{ 154: }
  ( sym: 263; act: 203 ),
  ( sym: 261; act: -64 ),
  ( sym: 262; act: -64 ),
  ( sym: 264; act: -64 ),
  ( sym: 271; act: -64 ),
  ( sym: 281; act: -64 ),
  ( sym: 282; act: -64 ),
  ( sym: 283; act: -64 ),
  ( sym: 284; act: -64 ),
  ( sym: 308; act: -64 ),
  ( sym: 313; act: -64 ),
{ 155: }
{ 156: }
{ 157: }
  ( sym: 265; act: 204 ),
  ( sym: 300; act: 175 ),
  ( sym: 301; act: 176 ),
  ( sym: 302; act: 177 ),
  ( sym: 303; act: 178 ),
  ( sym: 304; act: 179 ),
  ( sym: 305; act: 180 ),
  ( sym: 306; act: 181 ),
  ( sym: 307; act: 182 ),
  ( sym: 308; act: 183 ),
  ( sym: 309; act: 184 ),
  ( sym: 310; act: 185 ),
  ( sym: 311; act: 186 ),
  ( sym: 312; act: 187 ),
  ( sym: 313; act: 188 ),
  ( sym: 314; act: 189 ),
  ( sym: 315; act: 190 ),
{ 158: }
{ 159: }
  ( sym: 262; act: 114 ),
  ( sym: 264; act: 115 ),
  ( sym: 260; act: -69 ),
  ( sym: 261; act: -69 ),
  ( sym: 296; act: -69 ),
{ 160: }
  ( sym: 271; act: 23 ),
{ 161: }
  ( sym: 300; act: 175 ),
  ( sym: 301; act: 176 ),
  ( sym: 302; act: 177 ),
  ( sym: 303; act: 178 ),
  ( sym: 304; act: 179 ),
  ( sym: 305; act: 180 ),
  ( sym: 306; act: 181 ),
  ( sym: 307; act: 182 ),
  ( sym: 308; act: 183 ),
  ( sym: 309; act: 184 ),
  ( sym: 310; act: 185 ),
  ( sym: 311; act: 186 ),
  ( sym: 312; act: 187 ),
  ( sym: 313; act: 188 ),
  ( sym: 314; act: 189 ),
  ( sym: 315; act: 190 ),
  ( sym: 260; act: -87 ),
  ( sym: 261; act: -87 ),
  ( sym: 262; act: -87 ),
  ( sym: 263; act: -87 ),
  ( sym: 264; act: -87 ),
  ( sym: 296; act: -87 ),
{ 162: }
  ( sym: 300; act: 175 ),
  ( sym: 301; act: 176 ),
  ( sym: 302; act: 177 ),
  ( sym: 303; act: 178 ),
  ( sym: 304; act: 179 ),
  ( sym: 305; act: 180 ),
  ( sym: 306; act: 181 ),
  ( sym: 307; act: 182 ),
  ( sym: 308; act: 183 ),
  ( sym: 309; act: 184 ),
  ( sym: 310; act: 185 ),
  ( sym: 311; act: 186 ),
  ( sym: 312; act: 187 ),
  ( sym: 313; act: 188 ),
  ( sym: 314; act: 189 ),
  ( sym: 315; act: 190 ),
  ( sym: 260; act: -86 ),
  ( sym: 261; act: -86 ),
  ( sym: 262; act: -86 ),
  ( sym: 263; act: -86 ),
  ( sym: 264; act: -86 ),
  ( sym: 296; act: -86 ),
{ 163: }
  ( sym: 256; act: 78 ),
  ( sym: 262; act: 79 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 80 ),
  ( sym: 282; act: 81 ),
  ( sym: 283; act: 82 ),
  ( sym: 284; act: 83 ),
  ( sym: 308; act: 84 ),
  ( sym: 313; act: 85 ),
{ 164: }
{ 165: }
{ 166: }
{ 167: }
  ( sym: 300; act: 175 ),
  ( sym: 301; act: 176 ),
  ( sym: 302; act: 177 ),
  ( sym: 303; act: 178 ),
  ( sym: 304; act: 179 ),
  ( sym: 305; act: 180 ),
  ( sym: 306; act: 181 ),
  ( sym: 307; act: 182 ),
  ( sym: 308; act: 183 ),
  ( sym: 309; act: 184 ),
  ( sym: 310; act: 185 ),
  ( sym: 311; act: 186 ),
  ( sym: 312; act: 187 ),
  ( sym: 313; act: 188 ),
  ( sym: 314; act: 189 ),
  ( sym: 315; act: 190 ),
  ( sym: 261; act: -141 ),
  ( sym: 263; act: -141 ),
  ( sym: 267; act: -141 ),
{ 168: }
  ( sym: 262; act: 103 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 104 ),
  ( sym: 273; act: 105 ),
  ( sym: 308; act: 107 ),
  ( sym: 310; act: 108 ),
  ( sym: 315; act: 109 ),
{ 169: }
{ 170: }
{ 171: }
  ( sym: 261; act: 208 ),
  ( sym: 263; act: -145 ),
{ 172: }
  ( sym: 263; act: 209 ),
{ 173: }
  ( sym: 300; act: 175 ),
  ( sym: 301; act: 176 ),
  ( sym: 302; act: 177 ),
  ( sym: 303; act: 178 ),
  ( sym: 304; act: 179 ),
  ( sym: 305; act: 180 ),
  ( sym: 306; act: 181 ),
  ( sym: 307; act: 182 ),
  ( sym: 308; act: 183 ),
  ( sym: 309; act: 184 ),
  ( sym: 310; act: 185 ),
  ( sym: 311; act: 186 ),
  ( sym: 312; act: 187 ),
  ( sym: 313; act: 188 ),
  ( sym: 314; act: 189 ),
  ( sym: 315; act: 190 ),
  ( sym: 261; act: -147 ),
  ( sym: 263; act: -147 ),
{ 174: }
{ 175: }
  ( sym: 262; act: 103 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 104 ),
  ( sym: 273; act: 105 ),
  ( sym: 308; act: 107 ),
  ( sym: 310; act: 108 ),
  ( sym: 315; act: 109 ),
{ 176: }
  ( sym: 262; act: 103 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 104 ),
  ( sym: 273; act: 105 ),
  ( sym: 308; act: 107 ),
  ( sym: 310; act: 108 ),
  ( sym: 315; act: 109 ),
{ 177: }
  ( sym: 262; act: 103 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 104 ),
  ( sym: 273; act: 105 ),
  ( sym: 308; act: 107 ),
  ( sym: 310; act: 108 ),
  ( sym: 315; act: 109 ),
{ 178: }
  ( sym: 262; act: 103 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 104 ),
  ( sym: 273; act: 105 ),
  ( sym: 308; act: 107 ),
  ( sym: 310; act: 108 ),
  ( sym: 315; act: 109 ),
{ 179: }
  ( sym: 262; act: 103 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 104 ),
  ( sym: 273; act: 105 ),
  ( sym: 308; act: 107 ),
  ( sym: 310; act: 108 ),
  ( sym: 315; act: 109 ),
{ 180: }
  ( sym: 262; act: 103 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 104 ),
  ( sym: 273; act: 105 ),
  ( sym: 308; act: 107 ),
  ( sym: 310; act: 108 ),
  ( sym: 315; act: 109 ),
{ 181: }
  ( sym: 262; act: 103 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 104 ),
  ( sym: 273; act: 105 ),
  ( sym: 308; act: 107 ),
  ( sym: 310; act: 108 ),
  ( sym: 315; act: 109 ),
{ 182: }
  ( sym: 262; act: 103 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 104 ),
  ( sym: 273; act: 105 ),
  ( sym: 308; act: 107 ),
  ( sym: 310; act: 108 ),
  ( sym: 315; act: 109 ),
{ 183: }
  ( sym: 262; act: 103 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 104 ),
  ( sym: 273; act: 105 ),
  ( sym: 308; act: 107 ),
  ( sym: 310; act: 108 ),
  ( sym: 315; act: 109 ),
{ 184: }
  ( sym: 262; act: 103 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 104 ),
  ( sym: 273; act: 105 ),
  ( sym: 308; act: 107 ),
  ( sym: 310; act: 108 ),
  ( sym: 315; act: 109 ),
{ 185: }
  ( sym: 262; act: 103 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 104 ),
  ( sym: 273; act: 105 ),
  ( sym: 308; act: 107 ),
  ( sym: 310; act: 108 ),
  ( sym: 315; act: 109 ),
{ 186: }
  ( sym: 262; act: 103 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 104 ),
  ( sym: 273; act: 105 ),
  ( sym: 308; act: 107 ),
  ( sym: 310; act: 108 ),
  ( sym: 315; act: 109 ),
{ 187: }
  ( sym: 262; act: 103 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 104 ),
  ( sym: 273; act: 105 ),
  ( sym: 308; act: 107 ),
  ( sym: 310; act: 108 ),
  ( sym: 315; act: 109 ),
{ 188: }
  ( sym: 262; act: 103 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 104 ),
  ( sym: 273; act: 105 ),
  ( sym: 308; act: 107 ),
  ( sym: 310; act: 108 ),
  ( sym: 315; act: 109 ),
{ 189: }
  ( sym: 262; act: 103 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 104 ),
  ( sym: 273; act: 105 ),
  ( sym: 308; act: 107 ),
  ( sym: 310; act: 108 ),
  ( sym: 315; act: 109 ),
{ 190: }
  ( sym: 262; act: 103 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 104 ),
  ( sym: 273; act: 105 ),
  ( sym: 308; act: 107 ),
  ( sym: 310; act: 108 ),
  ( sym: 315; act: 109 ),
{ 191: }
  ( sym: 313; act: 227 ),
{ 192: }
  ( sym: 262; act: 103 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 104 ),
  ( sym: 273; act: 105 ),
  ( sym: 308; act: 107 ),
  ( sym: 310; act: 108 ),
  ( sym: 315; act: 109 ),
{ 193: }
  ( sym: 263; act: 229 ),
{ 194: }
  ( sym: 262; act: 103 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 104 ),
  ( sym: 273; act: 105 ),
  ( sym: 308; act: 107 ),
  ( sym: 310; act: 108 ),
  ( sym: 315; act: 109 ),
  ( sym: 259; act: -123 ),
  ( sym: 260; act: -123 ),
  ( sym: 261; act: -123 ),
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
  ( sym: 309; act: -123 ),
  ( sym: 311; act: -123 ),
  ( sym: 312; act: -123 ),
  ( sym: 313; act: -123 ),
  ( sym: 314; act: -123 ),
  ( sym: 318; act: -123 ),
  ( sym: 319; act: -123 ),
{ 195: }
{ 196: }
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
  ( sym: 298; act: 155 ),
{ 197: }
  ( sym: 313; act: 233 ),
{ 198: }
  ( sym: 262; act: 235 ),
  ( sym: 264; act: 236 ),
  ( sym: 261; act: -75 ),
  ( sym: 263; act: -75 ),
{ 199: }
  ( sym: 262; act: 114 ),
  ( sym: 264; act: 115 ),
  ( sym: 261; act: -73 ),
  ( sym: 263; act: -73 ),
{ 200: }
  ( sym: 262; act: 200 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 201 ),
  ( sym: 282; act: 81 ),
  ( sym: 283; act: 82 ),
  ( sym: 284; act: 83 ),
  ( sym: 308; act: 84 ),
  ( sym: 313; act: 238 ),
  ( sym: 263; act: -102 ),
  ( sym: 264; act: -102 ),
{ 201: }
  ( sym: 262; act: 200 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 201 ),
  ( sym: 282; act: 81 ),
  ( sym: 283; act: 82 ),
  ( sym: 284; act: 83 ),
  ( sym: 308; act: 84 ),
  ( sym: 313; act: 238 ),
  ( sym: 261; act: -102 ),
  ( sym: 263; act: -102 ),
  ( sym: 264; act: -102 ),
{ 202: }
  ( sym: 262; act: 200 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 201 ),
  ( sym: 282; act: 81 ),
  ( sym: 283; act: 82 ),
  ( sym: 284; act: 83 ),
  ( sym: 308; act: 84 ),
  ( sym: 313; act: 238 ),
  ( sym: 261; act: -102 ),
  ( sym: 263; act: -102 ),
  ( sym: 264; act: -102 ),
{ 203: }
{ 204: }
{ 205: }
  ( sym: 263; act: 242 ),
{ 206: }
{ 207: }
  ( sym: 286; act: 243 ),
{ 208: }
  ( sym: 262; act: 103 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 104 ),
  ( sym: 273; act: 105 ),
  ( sym: 308; act: 107 ),
  ( sym: 310; act: 108 ),
  ( sym: 315; act: 109 ),
  ( sym: 263; act: -146 ),
{ 209: }
{ 210: }
  ( sym: 306; act: 181 ),
  ( sym: 307; act: 182 ),
  ( sym: 308; act: 183 ),
  ( sym: 309; act: 184 ),
  ( sym: 310; act: 185 ),
  ( sym: 311; act: 186 ),
  ( sym: 312; act: 187 ),
  ( sym: 313; act: 188 ),
  ( sym: 314; act: 189 ),
  ( sym: 315; act: 190 ),
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
{ 211: }
  ( sym: 306; act: 181 ),
  ( sym: 307; act: 182 ),
  ( sym: 308; act: 183 ),
  ( sym: 309; act: 184 ),
  ( sym: 310; act: 185 ),
  ( sym: 311; act: 186 ),
  ( sym: 312; act: 187 ),
  ( sym: 313; act: 188 ),
  ( sym: 314; act: 189 ),
  ( sym: 315; act: 190 ),
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
{ 212: }
  ( sym: 306; act: 181 ),
  ( sym: 307; act: 182 ),
  ( sym: 308; act: 183 ),
  ( sym: 309; act: 184 ),
  ( sym: 310; act: 185 ),
  ( sym: 311; act: 186 ),
  ( sym: 312; act: 187 ),
  ( sym: 313; act: 188 ),
  ( sym: 314; act: 189 ),
  ( sym: 315; act: 190 ),
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
{ 213: }
  ( sym: 306; act: 181 ),
  ( sym: 307; act: 182 ),
  ( sym: 308; act: 183 ),
  ( sym: 309; act: 184 ),
  ( sym: 310; act: 185 ),
  ( sym: 311; act: 186 ),
  ( sym: 312; act: 187 ),
  ( sym: 313; act: 188 ),
  ( sym: 314; act: 189 ),
  ( sym: 315; act: 190 ),
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
{ 214: }
  ( sym: 306; act: 181 ),
  ( sym: 307; act: 182 ),
  ( sym: 308; act: 183 ),
  ( sym: 309; act: 184 ),
  ( sym: 310; act: 185 ),
  ( sym: 311; act: 186 ),
  ( sym: 312; act: 187 ),
  ( sym: 313; act: 188 ),
  ( sym: 314; act: 189 ),
  ( sym: 315; act: 190 ),
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
{ 215: }
  ( sym: 306; act: 181 ),
  ( sym: 307; act: 182 ),
  ( sym: 308; act: 183 ),
  ( sym: 309; act: 184 ),
  ( sym: 310; act: 185 ),
  ( sym: 311; act: 186 ),
  ( sym: 312; act: 187 ),
  ( sym: 313; act: 188 ),
  ( sym: 314; act: 189 ),
  ( sym: 315; act: 190 ),
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
{ 216: }
{ 217: }
  ( sym: 259; act: 245 ),
  ( sym: 300; act: 175 ),
  ( sym: 301; act: 176 ),
  ( sym: 302; act: 177 ),
  ( sym: 303; act: 178 ),
  ( sym: 304; act: 179 ),
  ( sym: 305; act: 180 ),
  ( sym: 306; act: 181 ),
  ( sym: 307; act: 182 ),
  ( sym: 308; act: 183 ),
  ( sym: 309; act: 184 ),
  ( sym: 310; act: 185 ),
  ( sym: 311; act: 186 ),
  ( sym: 312; act: 187 ),
  ( sym: 313; act: 188 ),
  ( sym: 314; act: 189 ),
  ( sym: 315; act: 190 ),
{ 218: }
  ( sym: 308; act: 183 ),
  ( sym: 309; act: 184 ),
  ( sym: 310; act: 185 ),
  ( sym: 311; act: 186 ),
  ( sym: 312; act: 187 ),
  ( sym: 313; act: 188 ),
  ( sym: 314; act: 189 ),
  ( sym: 315; act: 190 ),
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
  ( sym: 318; act: -114 ),
  ( sym: 319; act: -114 ),
{ 219: }
  ( sym: 309; act: 184 ),
  ( sym: 310; act: 185 ),
  ( sym: 311; act: 186 ),
  ( sym: 312; act: 187 ),
  ( sym: 313; act: 188 ),
  ( sym: 314; act: 189 ),
  ( sym: 315; act: 190 ),
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
  ( sym: 318; act: -115 ),
  ( sym: 319; act: -115 ),
{ 220: }
  ( sym: 311; act: 186 ),
  ( sym: 312; act: 187 ),
  ( sym: 313; act: 188 ),
  ( sym: 314; act: 189 ),
  ( sym: 315; act: 190 ),
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
  ( sym: 318; act: -110 ),
  ( sym: 319; act: -110 ),
{ 221: }
  ( sym: 311; act: 186 ),
  ( sym: 312; act: 187 ),
  ( sym: 313; act: 188 ),
  ( sym: 314; act: 189 ),
  ( sym: 315; act: 190 ),
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
{ 222: }
  ( sym: 313; act: 188 ),
  ( sym: 314; act: 189 ),
  ( sym: 315; act: 190 ),
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
{ 223: }
  ( sym: 313; act: 188 ),
  ( sym: 314; act: 189 ),
  ( sym: 315; act: 190 ),
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
  ( sym: 318; act: -117 ),
  ( sym: 319; act: -117 ),
{ 224: }
  ( sym: 315; act: 190 ),
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
  ( sym: 311; act: -112 ),
  ( sym: 312; act: -112 ),
  ( sym: 313; act: -112 ),
  ( sym: 314; act: -112 ),
  ( sym: 318; act: -112 ),
  ( sym: 319; act: -112 ),
{ 225: }
  ( sym: 315; act: 190 ),
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
{ 226: }
  ( sym: 315; act: 190 ),
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
  ( sym: 313; act: -116 ),
  ( sym: 314; act: -116 ),
  ( sym: 318; act: -116 ),
  ( sym: 319; act: -116 ),
{ 227: }
  ( sym: 263; act: 246 ),
{ 228: }
  ( sym: 318; act: 137 ),
  ( sym: 319; act: 138 ),
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
  ( sym: 315; act: -133 ),
{ 229: }
  ( sym: 262; act: 103 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 104 ),
  ( sym: 273; act: 105 ),
  ( sym: 308; act: 107 ),
  ( sym: 310; act: 108 ),
  ( sym: 315; act: 109 ),
{ 230: }
{ 231: }
  ( sym: 318; act: 137 ),
  ( sym: 319; act: 138 ),
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
  ( sym: 313; act: -122 ),
  ( sym: 314; act: -122 ),
  ( sym: 315; act: -122 ),
{ 232: }
{ 233: }
  ( sym: 262; act: 200 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 201 ),
  ( sym: 282; act: 81 ),
  ( sym: 283; act: 82 ),
  ( sym: 284; act: 83 ),
  ( sym: 308; act: 84 ),
  ( sym: 313; act: 238 ),
  ( sym: 261; act: -102 ),
  ( sym: 263; act: -102 ),
  ( sym: 264; act: -102 ),
{ 234: }
{ 235: }
  ( sym: 263; act: 153 ),
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
  ( sym: 280; act: 154 ),
  ( sym: 281; act: 31 ),
  ( sym: 298; act: 155 ),
{ 236: }
  ( sym: 262; act: 103 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 104 ),
  ( sym: 273; act: 105 ),
  ( sym: 308; act: 107 ),
  ( sym: 310; act: 108 ),
  ( sym: 315; act: 109 ),
{ 237: }
  ( sym: 262; act: 235 ),
  ( sym: 263; act: 251 ),
  ( sym: 264; act: 236 ),
{ 238: }
  ( sym: 262; act: 200 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 201 ),
  ( sym: 282; act: 81 ),
  ( sym: 283; act: 82 ),
  ( sym: 284; act: 83 ),
  ( sym: 308; act: 84 ),
  ( sym: 313; act: 238 ),
  ( sym: 261; act: -102 ),
  ( sym: 263; act: -102 ),
  ( sym: 264; act: -102 ),
{ 239: }
  ( sym: 262; act: 235 ),
  ( sym: 264; act: 236 ),
  ( sym: 261; act: -95 ),
  ( sym: 263; act: -95 ),
{ 240: }
  ( sym: 264; act: 236 ),
  ( sym: 261; act: -97 ),
  ( sym: 262; act: -97 ),
  ( sym: 263; act: -97 ),
{ 241: }
  ( sym: 262; act: 114 ),
  ( sym: 264; act: 115 ),
  ( sym: 261; act: -74 ),
  ( sym: 263; act: -74 ),
{ 242: }
{ 243: }
{ 244: }
{ 245: }
  ( sym: 262; act: 103 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 104 ),
  ( sym: 273; act: 105 ),
  ( sym: 308; act: 107 ),
  ( sym: 310; act: 108 ),
  ( sym: 315; act: 109 ),
{ 246: }
  ( sym: 262; act: 103 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 104 ),
  ( sym: 273; act: 105 ),
  ( sym: 308; act: 107 ),
  ( sym: 310; act: 108 ),
  ( sym: 315; act: 109 ),
{ 247: }
  ( sym: 318; act: 137 ),
  ( sym: 319; act: 138 ),
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
{ 248: }
  ( sym: 262; act: 235 ),
  ( sym: 264; act: 236 ),
  ( sym: 261; act: -96 ),
  ( sym: 263; act: -96 ),
{ 249: }
  ( sym: 263; act: 254 ),
{ 250: }
  ( sym: 265; act: 255 ),
  ( sym: 300; act: 175 ),
  ( sym: 301; act: 176 ),
  ( sym: 302; act: 177 ),
  ( sym: 303; act: 178 ),
  ( sym: 304; act: 179 ),
  ( sym: 305; act: 180 ),
  ( sym: 306; act: 181 ),
  ( sym: 307; act: 182 ),
  ( sym: 308; act: 183 ),
  ( sym: 309; act: 184 ),
  ( sym: 310; act: 185 ),
  ( sym: 311; act: 186 ),
  ( sym: 312; act: 187 ),
  ( sym: 313; act: 188 ),
  ( sym: 314; act: 189 ),
  ( sym: 315; act: 190 ),
{ 251: }
{ 252: }
  ( sym: 307; act: 182 ),
  ( sym: 308; act: 183 ),
  ( sym: 309; act: 184 ),
  ( sym: 310; act: 185 ),
  ( sym: 311; act: 186 ),
  ( sym: 312; act: 187 ),
  ( sym: 313; act: 188 ),
  ( sym: 314; act: 189 ),
  ( sym: 315; act: 190 ),
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
  ( sym: 318; act: -121 ),
  ( sym: 319; act: -121 ),
{ 253: }
  ( sym: 318; act: 137 ),
  ( sym: 319; act: 138 ),
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
  ( sym: 315; act: -135 )
{ 254: }
{ 255: }
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
  ( sym: -16; act: 50 ),
  ( sym: -10; act: 39 ),
{ 21: }
  ( sym: -16; act: 53 ),
  ( sym: -10; act: 40 ),
{ 22: }
  ( sym: -18; act: 54 ),
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
  ( sym: -21; act: 15 ),
  ( sym: -19; act: 16 ),
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
  ( sym: -16; act: 50 ),
  ( sym: -10; act: 67 ),
{ 38: }
{ 39: }
  ( sym: -16; act: 71 ),
{ 40: }
  ( sym: -16; act: 72 ),
{ 41: }
  ( sym: -18; act: 73 ),
{ 42: }
  ( sym: -26; act: 74 ),
  ( sym: -22; act: 75 ),
  ( sym: -12; act: 76 ),
  ( sym: -10; act: 77 ),
{ 43: }
{ 44: }
{ 45: }
{ 46: }
{ 47: }
{ 48: }
{ 49: }
{ 50: }
{ 51: }
  ( sym: -4; act: 87 ),
{ 52: }
  ( sym: -21; act: 15 ),
  ( sym: -20; act: 88 ),
  ( sym: -19; act: 16 ),
  ( sym: -17; act: 89 ),
  ( sym: -13; act: 17 ),
  ( sym: -11; act: 90 ),
  ( sym: -10; act: 19 ),
{ 53: }
{ 54: }
{ 55: }
  ( sym: -4; act: 92 ),
{ 56: }
  ( sym: -35; act: 93 ),
  ( sym: -14; act: 94 ),
  ( sym: -10; act: 95 ),
{ 57: }
{ 58: }
{ 59: }
{ 60: }
{ 61: }
{ 62: }
{ 63: }
{ 64: }
{ 65: }
  ( sym: -26; act: 74 ),
  ( sym: -22; act: 75 ),
  ( sym: -12; act: 97 ),
  ( sym: -10; act: 77 ),
{ 66: }
{ 67: }
  ( sym: -16; act: 71 ),
  ( sym: -10; act: 98 ),
{ 68: }
  ( sym: -35; act: 93 ),
  ( sym: -14; act: 99 ),
  ( sym: -10; act: 95 ),
{ 69: }
{ 70: }
  ( sym: -32; act: 100 ),
  ( sym: -15; act: 101 ),
  ( sym: -10; act: 102 ),
{ 71: }
{ 72: }
{ 73: }
{ 74: }
{ 75: }
  ( sym: -29; act: 113 ),
{ 76: }
  ( sym: -9; act: 116 ),
{ 77: }
  ( sym: -28; act: 119 ),
{ 78: }
  ( sym: -4; act: 121 ),
{ 79: }
  ( sym: -26; act: 74 ),
  ( sym: -22; act: 122 ),
  ( sym: -10; act: 77 ),
{ 80: }
  ( sym: -26; act: 74 ),
  ( sym: -22; act: 123 ),
  ( sym: -10; act: 77 ),
{ 81: }
{ 82: }
{ 83: }
{ 84: }
  ( sym: -26; act: 74 ),
  ( sym: -22; act: 124 ),
  ( sym: -10; act: 77 ),
{ 85: }
  ( sym: -26; act: 74 ),
  ( sym: -22; act: 125 ),
  ( sym: -10; act: 77 ),
{ 86: }
{ 87: }
{ 88: }
  ( sym: -21; act: 15 ),
  ( sym: -20; act: 88 ),
  ( sym: -19; act: 16 ),
  ( sym: -17; act: 127 ),
  ( sym: -13; act: 17 ),
  ( sym: -11; act: 90 ),
  ( sym: -10; act: 19 ),
{ 89: }
{ 90: }
  ( sym: -26; act: 74 ),
  ( sym: -22; act: 75 ),
  ( sym: -12; act: 129 ),
  ( sym: -10; act: 77 ),
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
{ 101: }
{ 102: }
{ 103: }
  ( sym: -32; act: 141 ),
  ( sym: -30; act: 142 ),
  ( sym: -27; act: 143 ),
  ( sym: -21; act: 15 ),
  ( sym: -19; act: 16 ),
  ( sym: -13; act: 17 ),
  ( sym: -11; act: 144 ),
  ( sym: -10; act: 145 ),
{ 104: }
{ 105: }
{ 106: }
{ 107: }
  ( sym: -32; act: 146 ),
  ( sym: -10; act: 102 ),
{ 108: }
  ( sym: -32; act: 147 ),
  ( sym: -10; act: 102 ),
{ 109: }
  ( sym: -32; act: 148 ),
  ( sym: -10; act: 102 ),
{ 110: }
{ 111: }
{ 112: }
  ( sym: -26; act: 74 ),
  ( sym: -22; act: 149 ),
  ( sym: -10; act: 77 ),
{ 113: }
{ 114: }
  ( sym: -25; act: 150 ),
  ( sym: -23; act: 151 ),
  ( sym: -21; act: 15 ),
  ( sym: -19; act: 16 ),
  ( sym: -13; act: 17 ),
  ( sym: -11; act: 152 ),
  ( sym: -10; act: 19 ),
{ 115: }
  ( sym: -32; act: 141 ),
  ( sym: -30; act: 156 ),
  ( sym: -27; act: 157 ),
  ( sym: -10; act: 102 ),
{ 116: }
{ 117: }
  ( sym: -26; act: 74 ),
  ( sym: -22; act: 159 ),
  ( sym: -10; act: 77 ),
{ 118: }
{ 119: }
  ( sym: -32; act: 141 ),
  ( sym: -30; act: 156 ),
  ( sym: -27; act: 161 ),
  ( sym: -10; act: 102 ),
{ 120: }
  ( sym: -32; act: 141 ),
  ( sym: -30; act: 156 ),
  ( sym: -27; act: 162 ),
  ( sym: -10; act: 102 ),
{ 121: }
{ 122: }
  ( sym: -29; act: 113 ),
{ 123: }
  ( sym: -29; act: 113 ),
{ 124: }
  ( sym: -29; act: 113 ),
{ 125: }
  ( sym: -29; act: 113 ),
{ 126: }
{ 127: }
{ 128: }
{ 129: }
{ 130: }
{ 131: }
  ( sym: -35; act: 93 ),
  ( sym: -14; act: 166 ),
  ( sym: -10; act: 95 ),
{ 132: }
{ 133: }
  ( sym: -32; act: 141 ),
  ( sym: -30; act: 156 ),
  ( sym: -27; act: 167 ),
  ( sym: -10; act: 102 ),
{ 134: }
{ 135: }
{ 136: }
{ 137: }
  ( sym: -32; act: 141 ),
  ( sym: -30; act: 156 ),
  ( sym: -27; act: 169 ),
  ( sym: -10; act: 102 ),
{ 138: }
  ( sym: -32; act: 141 ),
  ( sym: -30; act: 156 ),
  ( sym: -27; act: 170 ),
  ( sym: -10; act: 102 ),
{ 139: }
{ 140: }
  ( sym: -36; act: 171 ),
  ( sym: -34; act: 172 ),
  ( sym: -32; act: 141 ),
  ( sym: -30; act: 156 ),
  ( sym: -27; act: 173 ),
  ( sym: -10; act: 102 ),
{ 141: }
{ 142: }
{ 143: }
{ 144: }
  ( sym: -26; act: 191 ),
{ 145: }
{ 146: }
{ 147: }
{ 148: }
{ 149: }
  ( sym: -29; act: 113 ),
{ 150: }
{ 151: }
{ 152: }
  ( sym: -26; act: 197 ),
  ( sym: -24; act: 198 ),
  ( sym: -22; act: 199 ),
  ( sym: -10; act: 77 ),
{ 153: }
{ 154: }
{ 155: }
{ 156: }
{ 157: }
{ 158: }
{ 159: }
  ( sym: -29; act: 113 ),
{ 160: }
  ( sym: -10; act: 205 ),
{ 161: }
{ 162: }
{ 163: }
  ( sym: -26; act: 74 ),
  ( sym: -22; act: 75 ),
  ( sym: -12; act: 206 ),
  ( sym: -10; act: 77 ),
{ 164: }
{ 165: }
{ 166: }
{ 167: }
{ 168: }
  ( sym: -32; act: 100 ),
  ( sym: -15; act: 207 ),
  ( sym: -10; act: 102 ),
{ 169: }
{ 170: }
{ 171: }
{ 172: }
{ 173: }
{ 174: }
{ 175: }
  ( sym: -32; act: 141 ),
  ( sym: -30; act: 156 ),
  ( sym: -27; act: 210 ),
  ( sym: -10; act: 102 ),
{ 176: }
  ( sym: -32; act: 141 ),
  ( sym: -30; act: 156 ),
  ( sym: -27; act: 211 ),
  ( sym: -10; act: 102 ),
{ 177: }
  ( sym: -32; act: 141 ),
  ( sym: -30; act: 156 ),
  ( sym: -27; act: 212 ),
  ( sym: -10; act: 102 ),
{ 178: }
  ( sym: -32; act: 141 ),
  ( sym: -30; act: 156 ),
  ( sym: -27; act: 213 ),
  ( sym: -10; act: 102 ),
{ 179: }
  ( sym: -32; act: 141 ),
  ( sym: -30; act: 156 ),
  ( sym: -27; act: 214 ),
  ( sym: -10; act: 102 ),
{ 180: }
  ( sym: -32; act: 141 ),
  ( sym: -30; act: 156 ),
  ( sym: -27; act: 215 ),
  ( sym: -10; act: 102 ),
{ 181: }
  ( sym: -32; act: 141 ),
  ( sym: -31; act: 216 ),
  ( sym: -30; act: 156 ),
  ( sym: -27; act: 217 ),
  ( sym: -10; act: 102 ),
{ 182: }
  ( sym: -32; act: 141 ),
  ( sym: -30; act: 156 ),
  ( sym: -27; act: 218 ),
  ( sym: -10; act: 102 ),
{ 183: }
  ( sym: -32; act: 141 ),
  ( sym: -30; act: 156 ),
  ( sym: -27; act: 219 ),
  ( sym: -10; act: 102 ),
{ 184: }
  ( sym: -32; act: 141 ),
  ( sym: -30; act: 156 ),
  ( sym: -27; act: 220 ),
  ( sym: -10; act: 102 ),
{ 185: }
  ( sym: -32; act: 141 ),
  ( sym: -30; act: 156 ),
  ( sym: -27; act: 221 ),
  ( sym: -10; act: 102 ),
{ 186: }
  ( sym: -32; act: 141 ),
  ( sym: -30; act: 156 ),
  ( sym: -27; act: 222 ),
  ( sym: -10; act: 102 ),
{ 187: }
  ( sym: -32; act: 141 ),
  ( sym: -30; act: 156 ),
  ( sym: -27; act: 223 ),
  ( sym: -10; act: 102 ),
{ 188: }
  ( sym: -32; act: 141 ),
  ( sym: -30; act: 156 ),
  ( sym: -27; act: 224 ),
  ( sym: -10; act: 102 ),
{ 189: }
  ( sym: -32; act: 141 ),
  ( sym: -30; act: 156 ),
  ( sym: -27; act: 225 ),
  ( sym: -10; act: 102 ),
{ 190: }
  ( sym: -32; act: 141 ),
  ( sym: -30; act: 156 ),
  ( sym: -27; act: 226 ),
  ( sym: -10; act: 102 ),
{ 191: }
{ 192: }
  ( sym: -32; act: 228 ),
  ( sym: -10; act: 102 ),
{ 193: }
{ 194: }
  ( sym: -33; act: 230 ),
  ( sym: -32; act: 231 ),
  ( sym: -10; act: 102 ),
{ 195: }
{ 196: }
  ( sym: -25; act: 232 ),
  ( sym: -23; act: 151 ),
  ( sym: -21; act: 15 ),
  ( sym: -19; act: 16 ),
  ( sym: -13; act: 17 ),
  ( sym: -11; act: 152 ),
  ( sym: -10; act: 19 ),
{ 197: }
{ 198: }
  ( sym: -29; act: 234 ),
{ 199: }
  ( sym: -29; act: 113 ),
{ 200: }
  ( sym: -26; act: 197 ),
  ( sym: -24; act: 237 ),
  ( sym: -22; act: 122 ),
  ( sym: -10; act: 77 ),
{ 201: }
  ( sym: -26; act: 197 ),
  ( sym: -24; act: 239 ),
  ( sym: -22; act: 123 ),
  ( sym: -10; act: 77 ),
{ 202: }
  ( sym: -26; act: 197 ),
  ( sym: -24; act: 240 ),
  ( sym: -22; act: 241 ),
  ( sym: -10; act: 77 ),
{ 203: }
{ 204: }
{ 205: }
{ 206: }
{ 207: }
{ 208: }
  ( sym: -36; act: 171 ),
  ( sym: -34; act: 244 ),
  ( sym: -32; act: 141 ),
  ( sym: -30; act: 156 ),
  ( sym: -27; act: 173 ),
  ( sym: -10; act: 102 ),
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
{ 226: }
{ 227: }
{ 228: }
{ 229: }
  ( sym: -32; act: 247 ),
  ( sym: -10; act: 102 ),
{ 230: }
{ 231: }
{ 232: }
{ 233: }
  ( sym: -26; act: 197 ),
  ( sym: -24; act: 248 ),
  ( sym: -22; act: 149 ),
  ( sym: -10; act: 77 ),
{ 234: }
{ 235: }
  ( sym: -25; act: 249 ),
  ( sym: -23; act: 151 ),
  ( sym: -21; act: 15 ),
  ( sym: -19; act: 16 ),
  ( sym: -13; act: 17 ),
  ( sym: -11; act: 152 ),
  ( sym: -10; act: 19 ),
{ 236: }
  ( sym: -32; act: 141 ),
  ( sym: -30; act: 156 ),
  ( sym: -27; act: 250 ),
  ( sym: -10; act: 102 ),
{ 237: }
  ( sym: -29; act: 234 ),
{ 238: }
  ( sym: -26; act: 197 ),
  ( sym: -24; act: 240 ),
  ( sym: -22; act: 125 ),
  ( sym: -10; act: 77 ),
{ 239: }
  ( sym: -29; act: 234 ),
{ 240: }
  ( sym: -29; act: 234 ),
{ 241: }
  ( sym: -29; act: 113 ),
{ 242: }
{ 243: }
{ 244: }
{ 245: }
  ( sym: -32; act: 141 ),
  ( sym: -30; act: 156 ),
  ( sym: -27; act: 252 ),
  ( sym: -10; act: 102 ),
{ 246: }
  ( sym: -32; act: 253 ),
  ( sym: -10; act: 102 ),
{ 247: }
{ 248: }
  ( sym: -29; act: 234 )
{ 249: }
{ 250: }
{ 251: }
{ 252: }
{ 253: }
{ 254: }
{ 255: }
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
{ 15: } -67,
{ 16: } -49,
{ 17: } -48,
{ 18: } 0,
{ 19: } -68,
{ 20: } 0,
{ 21: } 0,
{ 22: } 0,
{ 23: } -53,
{ 24: } -65,
{ 25: } 0,
{ 26: } 0,
{ 27: } -54,
{ 28: } -57,
{ 29: } -66,
{ 30: } -64,
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
{ 54: } -47,
{ 55: } -2,
{ 56: } 0,
{ 57: } -62,
{ 58: } 0,
{ 59: } -55,
{ 60: } -63,
{ 61: } -58,
{ 62: } -42,
{ 63: } -24,
{ 64: } -29,
{ 65: } 0,
{ 66: } -23,
{ 67: } 0,
{ 68: } 0,
{ 69: } -27,
{ 70: } 0,
{ 71: } 0,
{ 72: } 0,
{ 73: } -40,
{ 74: } 0,
{ 75: } 0,
{ 76: } 0,
{ 77: } 0,
{ 78: } -2,
{ 79: } 0,
{ 80: } 0,
{ 81: } -79,
{ 82: } -81,
{ 83: } -80,
{ 84: } 0,
{ 85: } 0,
{ 86: } -45,
{ 87: } 0,
{ 88: } 0,
{ 89: } 0,
{ 90: } 0,
{ 91: } -43,
{ 92: } 0,
{ 93: } 0,
{ 94: } 0,
{ 95: } 0,
{ 96: } -59,
{ 97: } 0,
{ 98: } 0,
{ 99: } 0,
{ 100: } 0,
{ 101: } 0,
{ 102: } 0,
{ 103: } 0,
{ 104: } -126,
{ 105: } -125,
{ 106: } -26,
{ 107: } 0,
{ 108: } 0,
{ 109: } 0,
{ 110: } -34,
{ 111: } -36,
{ 112: } 0,
{ 113: } -90,
{ 114: } 0,
{ 115: } 0,
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
{ 126: } -31,
{ 127: } -50,
{ 128: } -30,
{ 129: } 0,
{ 130: } -33,
{ 131: } 0,
{ 132: } -32,
{ 133: } 0,
{ 134: } -22,
{ 135: } -21,
{ 136: } 0,
{ 137: } 0,
{ 138: } 0,
{ 139: } -28,
{ 140: } 0,
{ 141: } 0,
{ 142: } 0,
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
{ 153: } -93,
{ 154: } 0,
{ 155: } -78,
{ 156: } -103,
{ 157: } 0,
{ 158: } -19,
{ 159: } 0,
{ 160: } 0,
{ 161: } 0,
{ 162: } 0,
{ 163: } 0,
{ 164: } -92,
{ 165: } -52,
{ 166: } -138,
{ 167: } 0,
{ 168: } 0,
{ 169: } -127,
{ 170: } -128,
{ 171: } 0,
{ 172: } 0,
{ 173: } 0,
{ 174: } -137,
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
{ 191: } 0,
{ 192: } 0,
{ 193: } 0,
{ 194: } 0,
{ 195: } -89,
{ 196: } 0,
{ 197: } 0,
{ 198: } 0,
{ 199: } 0,
{ 200: } 0,
{ 201: } 0,
{ 202: } 0,
{ 203: } -94,
{ 204: } -91,
{ 205: } 0,
{ 206: } -70,
{ 207: } 0,
{ 208: } 0,
{ 209: } -136,
{ 210: } 0,
{ 211: } 0,
{ 212: } 0,
{ 213: } 0,
{ 214: } 0,
{ 215: } 0,
{ 216: } -119,
{ 217: } 0,
{ 218: } 0,
{ 219: } 0,
{ 220: } 0,
{ 221: } 0,
{ 222: } 0,
{ 223: } 0,
{ 224: } 0,
{ 225: } 0,
{ 226: } 0,
{ 227: } 0,
{ 228: } 0,
{ 229: } 0,
{ 230: } -132,
{ 231: } 0,
{ 232: } -77,
{ 233: } 0,
{ 234: } -99,
{ 235: } 0,
{ 236: } 0,
{ 237: } 0,
{ 238: } 0,
{ 239: } 0,
{ 240: } 0,
{ 241: } 0,
{ 242: } -17,
{ 243: } -25,
{ 244: } -144,
{ 245: } 0,
{ 246: } 0,
{ 247: } 0,
{ 248: } 0,
{ 249: } 0,
{ 250: } 0,
{ 251: } -101,
{ 252: } 0,
{ 253: } 0,
{ 254: } -98,
{ 255: } -100
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
{ 67: } 340,
{ 68: } 357,
{ 69: } 359,
{ 70: } 359,
{ 71: } 367,
{ 72: } 388,
{ 73: } 409,
{ 74: } 409,
{ 75: } 410,
{ 76: } 415,
{ 77: } 418,
{ 78: } 425,
{ 79: } 425,
{ 80: } 433,
{ 81: } 441,
{ 82: } 441,
{ 83: } 441,
{ 84: } 441,
{ 85: } 449,
{ 86: } 457,
{ 87: } 457,
{ 88: } 458,
{ 89: } 471,
{ 90: } 472,
{ 91: } 481,
{ 92: } 481,
{ 93: } 482,
{ 94: } 485,
{ 95: } 486,
{ 96: } 490,
{ 97: } 490,
{ 98: } 492,
{ 99: } 493,
{ 100: } 494,
{ 101: } 497,
{ 102: } 498,
{ 103: } 526,
{ 104: } 544,
{ 105: } 544,
{ 106: } 544,
{ 107: } 544,
{ 108: } 551,
{ 109: } 558,
{ 110: } 565,
{ 111: } 565,
{ 112: } 565,
{ 113: } 573,
{ 114: } 573,
{ 115: } 587,
{ 116: } 594,
{ 117: } 595,
{ 118: } 603,
{ 119: } 604,
{ 120: } 611,
{ 121: } 618,
{ 122: } 621,
{ 123: } 624,
{ 124: } 630,
{ 125: } 636,
{ 126: } 642,
{ 127: } 642,
{ 128: } 642,
{ 129: } 642,
{ 130: } 644,
{ 131: } 644,
{ 132: } 647,
{ 133: } 647,
{ 134: } 654,
{ 135: } 654,
{ 136: } 654,
{ 137: } 655,
{ 138: } 662,
{ 139: } 669,
{ 140: } 669,
{ 141: } 677,
{ 142: } 705,
{ 143: } 722,
{ 144: } 738,
{ 145: } 743,
{ 146: } 766,
{ 147: } 794,
{ 148: } 822,
{ 149: } 850,
{ 150: } 856,
{ 151: } 857,
{ 152: } 859,
{ 153: } 870,
{ 154: } 870,
{ 155: } 881,
{ 156: } 881,
{ 157: } 881,
{ 158: } 898,
{ 159: } 898,
{ 160: } 903,
{ 161: } 904,
{ 162: } 926,
{ 163: } 948,
{ 164: } 957,
{ 165: } 957,
{ 166: } 957,
{ 167: } 957,
{ 168: } 976,
{ 169: } 983,
{ 170: } 983,
{ 171: } 983,
{ 172: } 985,
{ 173: } 986,
{ 174: } 1004,
{ 175: } 1004,
{ 176: } 1011,
{ 177: } 1018,
{ 178: } 1025,
{ 179: } 1032,
{ 180: } 1039,
{ 181: } 1046,
{ 182: } 1053,
{ 183: } 1060,
{ 184: } 1067,
{ 185: } 1074,
{ 186: } 1081,
{ 187: } 1088,
{ 188: } 1095,
{ 189: } 1102,
{ 190: } 1109,
{ 191: } 1116,
{ 192: } 1117,
{ 193: } 1124,
{ 194: } 1125,
{ 195: } 1156,
{ 196: } 1156,
{ 197: } 1169,
{ 198: } 1170,
{ 199: } 1174,
{ 200: } 1178,
{ 201: } 1188,
{ 202: } 1199,
{ 203: } 1210,
{ 204: } 1210,
{ 205: } 1210,
{ 206: } 1211,
{ 207: } 1211,
{ 208: } 1212,
{ 209: } 1220,
{ 210: } 1220,
{ 211: } 1248,
{ 212: } 1276,
{ 213: } 1304,
{ 214: } 1332,
{ 215: } 1360,
{ 216: } 1388,
{ 217: } 1388,
{ 218: } 1405,
{ 219: } 1433,
{ 220: } 1461,
{ 221: } 1489,
{ 222: } 1517,
{ 223: } 1545,
{ 224: } 1573,
{ 225: } 1601,
{ 226: } 1629,
{ 227: } 1657,
{ 228: } 1658,
{ 229: } 1686,
{ 230: } 1693,
{ 231: } 1693,
{ 232: } 1721,
{ 233: } 1721,
{ 234: } 1732,
{ 235: } 1732,
{ 236: } 1746,
{ 237: } 1753,
{ 238: } 1756,
{ 239: } 1767,
{ 240: } 1771,
{ 241: } 1775,
{ 242: } 1779,
{ 243: } 1779,
{ 244: } 1779,
{ 245: } 1779,
{ 246: } 1786,
{ 247: } 1793,
{ 248: } 1821,
{ 249: } 1825,
{ 250: } 1826,
{ 251: } 1843,
{ 252: } 1843,
{ 253: } 1871,
{ 254: } 1899,
{ 255: } 1899
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
{ 66: } 339,
{ 67: } 356,
{ 68: } 358,
{ 69: } 358,
{ 70: } 366,
{ 71: } 387,
{ 72: } 408,
{ 73: } 408,
{ 74: } 409,
{ 75: } 414,
{ 76: } 417,
{ 77: } 424,
{ 78: } 424,
{ 79: } 432,
{ 80: } 440,
{ 81: } 440,
{ 82: } 440,
{ 83: } 440,
{ 84: } 448,
{ 85: } 456,
{ 86: } 456,
{ 87: } 457,
{ 88: } 470,
{ 89: } 471,
{ 90: } 480,
{ 91: } 480,
{ 92: } 481,
{ 93: } 484,
{ 94: } 485,
{ 95: } 489,
{ 96: } 489,
{ 97: } 491,
{ 98: } 492,
{ 99: } 493,
{ 100: } 496,
{ 101: } 497,
{ 102: } 525,
{ 103: } 543,
{ 104: } 543,
{ 105: } 543,
{ 106: } 543,
{ 107: } 550,
{ 108: } 557,
{ 109: } 564,
{ 110: } 564,
{ 111: } 564,
{ 112: } 572,
{ 113: } 572,
{ 114: } 586,
{ 115: } 593,
{ 116: } 594,
{ 117: } 602,
{ 118: } 603,
{ 119: } 610,
{ 120: } 617,
{ 121: } 620,
{ 122: } 623,
{ 123: } 629,
{ 124: } 635,
{ 125: } 641,
{ 126: } 641,
{ 127: } 641,
{ 128: } 641,
{ 129: } 643,
{ 130: } 643,
{ 131: } 646,
{ 132: } 646,
{ 133: } 653,
{ 134: } 653,
{ 135: } 653,
{ 136: } 654,
{ 137: } 661,
{ 138: } 668,
{ 139: } 668,
{ 140: } 676,
{ 141: } 704,
{ 142: } 721,
{ 143: } 737,
{ 144: } 742,
{ 145: } 765,
{ 146: } 793,
{ 147: } 821,
{ 148: } 849,
{ 149: } 855,
{ 150: } 856,
{ 151: } 858,
{ 152: } 869,
{ 153: } 869,
{ 154: } 880,
{ 155: } 880,
{ 156: } 880,
{ 157: } 897,
{ 158: } 897,
{ 159: } 902,
{ 160: } 903,
{ 161: } 925,
{ 162: } 947,
{ 163: } 956,
{ 164: } 956,
{ 165: } 956,
{ 166: } 956,
{ 167: } 975,
{ 168: } 982,
{ 169: } 982,
{ 170: } 982,
{ 171: } 984,
{ 172: } 985,
{ 173: } 1003,
{ 174: } 1003,
{ 175: } 1010,
{ 176: } 1017,
{ 177: } 1024,
{ 178: } 1031,
{ 179: } 1038,
{ 180: } 1045,
{ 181: } 1052,
{ 182: } 1059,
{ 183: } 1066,
{ 184: } 1073,
{ 185: } 1080,
{ 186: } 1087,
{ 187: } 1094,
{ 188: } 1101,
{ 189: } 1108,
{ 190: } 1115,
{ 191: } 1116,
{ 192: } 1123,
{ 193: } 1124,
{ 194: } 1155,
{ 195: } 1155,
{ 196: } 1168,
{ 197: } 1169,
{ 198: } 1173,
{ 199: } 1177,
{ 200: } 1187,
{ 201: } 1198,
{ 202: } 1209,
{ 203: } 1209,
{ 204: } 1209,
{ 205: } 1210,
{ 206: } 1210,
{ 207: } 1211,
{ 208: } 1219,
{ 209: } 1219,
{ 210: } 1247,
{ 211: } 1275,
{ 212: } 1303,
{ 213: } 1331,
{ 214: } 1359,
{ 215: } 1387,
{ 216: } 1387,
{ 217: } 1404,
{ 218: } 1432,
{ 219: } 1460,
{ 220: } 1488,
{ 221: } 1516,
{ 222: } 1544,
{ 223: } 1572,
{ 224: } 1600,
{ 225: } 1628,
{ 226: } 1656,
{ 227: } 1657,
{ 228: } 1685,
{ 229: } 1692,
{ 230: } 1692,
{ 231: } 1720,
{ 232: } 1720,
{ 233: } 1731,
{ 234: } 1731,
{ 235: } 1745,
{ 236: } 1752,
{ 237: } 1755,
{ 238: } 1766,
{ 239: } 1770,
{ 240: } 1774,
{ 241: } 1778,
{ 242: } 1778,
{ 243: } 1778,
{ 244: } 1778,
{ 245: } 1785,
{ 246: } 1792,
{ 247: } 1820,
{ 248: } 1824,
{ 249: } 1825,
{ 250: } 1842,
{ 251: } 1842,
{ 252: } 1870,
{ 253: } 1898,
{ 254: } 1898,
{ 255: } 1898
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
{ 99: } 99,
{ 100: } 99,
{ 101: } 99,
{ 102: } 99,
{ 103: } 99,
{ 104: } 107,
{ 105: } 107,
{ 106: } 107,
{ 107: } 107,
{ 108: } 109,
{ 109: } 111,
{ 110: } 113,
{ 111: } 113,
{ 112: } 113,
{ 113: } 116,
{ 114: } 116,
{ 115: } 123,
{ 116: } 127,
{ 117: } 127,
{ 118: } 130,
{ 119: } 130,
{ 120: } 134,
{ 121: } 138,
{ 122: } 138,
{ 123: } 139,
{ 124: } 140,
{ 125: } 141,
{ 126: } 142,
{ 127: } 142,
{ 128: } 142,
{ 129: } 142,
{ 130: } 142,
{ 131: } 142,
{ 132: } 145,
{ 133: } 145,
{ 134: } 149,
{ 135: } 149,
{ 136: } 149,
{ 137: } 149,
{ 138: } 153,
{ 139: } 157,
{ 140: } 157,
{ 141: } 163,
{ 142: } 163,
{ 143: } 163,
{ 144: } 163,
{ 145: } 164,
{ 146: } 164,
{ 147: } 164,
{ 148: } 164,
{ 149: } 164,
{ 150: } 165,
{ 151: } 165,
{ 152: } 165,
{ 153: } 169,
{ 154: } 169,
{ 155: } 169,
{ 156: } 169,
{ 157: } 169,
{ 158: } 169,
{ 159: } 169,
{ 160: } 170,
{ 161: } 171,
{ 162: } 171,
{ 163: } 171,
{ 164: } 175,
{ 165: } 175,
{ 166: } 175,
{ 167: } 175,
{ 168: } 175,
{ 169: } 178,
{ 170: } 178,
{ 171: } 178,
{ 172: } 178,
{ 173: } 178,
{ 174: } 178,
{ 175: } 178,
{ 176: } 182,
{ 177: } 186,
{ 178: } 190,
{ 179: } 194,
{ 180: } 198,
{ 181: } 202,
{ 182: } 207,
{ 183: } 211,
{ 184: } 215,
{ 185: } 219,
{ 186: } 223,
{ 187: } 227,
{ 188: } 231,
{ 189: } 235,
{ 190: } 239,
{ 191: } 243,
{ 192: } 243,
{ 193: } 245,
{ 194: } 245,
{ 195: } 248,
{ 196: } 248,
{ 197: } 255,
{ 198: } 255,
{ 199: } 256,
{ 200: } 257,
{ 201: } 261,
{ 202: } 265,
{ 203: } 269,
{ 204: } 269,
{ 205: } 269,
{ 206: } 269,
{ 207: } 269,
{ 208: } 269,
{ 209: } 275,
{ 210: } 275,
{ 211: } 275,
{ 212: } 275,
{ 213: } 275,
{ 214: } 275,
{ 215: } 275,
{ 216: } 275,
{ 217: } 275,
{ 218: } 275,
{ 219: } 275,
{ 220: } 275,
{ 221: } 275,
{ 222: } 275,
{ 223: } 275,
{ 224: } 275,
{ 225: } 275,
{ 226: } 275,
{ 227: } 275,
{ 228: } 275,
{ 229: } 275,
{ 230: } 277,
{ 231: } 277,
{ 232: } 277,
{ 233: } 277,
{ 234: } 281,
{ 235: } 281,
{ 236: } 288,
{ 237: } 292,
{ 238: } 293,
{ 239: } 297,
{ 240: } 298,
{ 241: } 299,
{ 242: } 300,
{ 243: } 300,
{ 244: } 300,
{ 245: } 300,
{ 246: } 304,
{ 247: } 306,
{ 248: } 306,
{ 249: } 307,
{ 250: } 307,
{ 251: } 307,
{ 252: } 307,
{ 253: } 307,
{ 254: } 307,
{ 255: } 307
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
{ 66: } 63,
{ 67: } 65,
{ 68: } 68,
{ 69: } 68,
{ 70: } 71,
{ 71: } 71,
{ 72: } 71,
{ 73: } 71,
{ 74: } 71,
{ 75: } 72,
{ 76: } 73,
{ 77: } 74,
{ 78: } 75,
{ 79: } 78,
{ 80: } 81,
{ 81: } 81,
{ 82: } 81,
{ 83: } 81,
{ 84: } 84,
{ 85: } 87,
{ 86: } 87,
{ 87: } 87,
{ 88: } 94,
{ 89: } 94,
{ 90: } 98,
{ 91: } 98,
{ 92: } 98,
{ 93: } 98,
{ 94: } 98,
{ 95: } 98,
{ 96: } 98,
{ 97: } 98,
{ 98: } 98,
{ 99: } 98,
{ 100: } 98,
{ 101: } 98,
{ 102: } 98,
{ 103: } 106,
{ 104: } 106,
{ 105: } 106,
{ 106: } 106,
{ 107: } 108,
{ 108: } 110,
{ 109: } 112,
{ 110: } 112,
{ 111: } 112,
{ 112: } 115,
{ 113: } 115,
{ 114: } 122,
{ 115: } 126,
{ 116: } 126,
{ 117: } 129,
{ 118: } 129,
{ 119: } 133,
{ 120: } 137,
{ 121: } 137,
{ 122: } 138,
{ 123: } 139,
{ 124: } 140,
{ 125: } 141,
{ 126: } 141,
{ 127: } 141,
{ 128: } 141,
{ 129: } 141,
{ 130: } 141,
{ 131: } 144,
{ 132: } 144,
{ 133: } 148,
{ 134: } 148,
{ 135: } 148,
{ 136: } 148,
{ 137: } 152,
{ 138: } 156,
{ 139: } 156,
{ 140: } 162,
{ 141: } 162,
{ 142: } 162,
{ 143: } 162,
{ 144: } 163,
{ 145: } 163,
{ 146: } 163,
{ 147: } 163,
{ 148: } 163,
{ 149: } 164,
{ 150: } 164,
{ 151: } 164,
{ 152: } 168,
{ 153: } 168,
{ 154: } 168,
{ 155: } 168,
{ 156: } 168,
{ 157: } 168,
{ 158: } 168,
{ 159: } 169,
{ 160: } 170,
{ 161: } 170,
{ 162: } 170,
{ 163: } 174,
{ 164: } 174,
{ 165: } 174,
{ 166: } 174,
{ 167: } 174,
{ 168: } 177,
{ 169: } 177,
{ 170: } 177,
{ 171: } 177,
{ 172: } 177,
{ 173: } 177,
{ 174: } 177,
{ 175: } 181,
{ 176: } 185,
{ 177: } 189,
{ 178: } 193,
{ 179: } 197,
{ 180: } 201,
{ 181: } 206,
{ 182: } 210,
{ 183: } 214,
{ 184: } 218,
{ 185: } 222,
{ 186: } 226,
{ 187: } 230,
{ 188: } 234,
{ 189: } 238,
{ 190: } 242,
{ 191: } 242,
{ 192: } 244,
{ 193: } 244,
{ 194: } 247,
{ 195: } 247,
{ 196: } 254,
{ 197: } 254,
{ 198: } 255,
{ 199: } 256,
{ 200: } 260,
{ 201: } 264,
{ 202: } 268,
{ 203: } 268,
{ 204: } 268,
{ 205: } 268,
{ 206: } 268,
{ 207: } 268,
{ 208: } 274,
{ 209: } 274,
{ 210: } 274,
{ 211: } 274,
{ 212: } 274,
{ 213: } 274,
{ 214: } 274,
{ 215: } 274,
{ 216: } 274,
{ 217: } 274,
{ 218: } 274,
{ 219: } 274,
{ 220: } 274,
{ 221: } 274,
{ 222: } 274,
{ 223: } 274,
{ 224: } 274,
{ 225: } 274,
{ 226: } 274,
{ 227: } 274,
{ 228: } 274,
{ 229: } 276,
{ 230: } 276,
{ 231: } 276,
{ 232: } 276,
{ 233: } 280,
{ 234: } 280,
{ 235: } 287,
{ 236: } 291,
{ 237: } 292,
{ 238: } 296,
{ 239: } 297,
{ 240: } 298,
{ 241: } 299,
{ 242: } 299,
{ 243: } 299,
{ 244: } 299,
{ 245: } 303,
{ 246: } 305,
{ 247: } 305,
{ 248: } 306,
{ 249: } 306,
{ 250: } 306,
{ 251: } 306,
{ 252: } 306,
{ 253: } 306,
{ 254: } 306,
{ 255: } 306
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
{ 22: } ( len: 5; sym: -5 ),
{ 23: } ( len: 3; sym: -5 ),
{ 24: } ( len: 3; sym: -5 ),
{ 25: } ( len: 8; sym: -6 ),
{ 26: } ( len: 4; sym: -6 ),
{ 27: } ( len: 3; sym: -6 ),
{ 28: } ( len: 5; sym: -6 ),
{ 29: } ( len: 3; sym: -6 ),
{ 30: } ( len: 3; sym: -16 ),
{ 31: } ( len: 3; sym: -16 ),
{ 32: } ( len: 3; sym: -18 ),
{ 33: } ( len: 3; sym: -18 ),
{ 34: } ( len: 4; sym: -13 ),
{ 35: } ( len: 3; sym: -13 ),
{ 36: } ( len: 4; sym: -13 ),
{ 37: } ( len: 3; sym: -13 ),
{ 38: } ( len: 2; sym: -13 ),
{ 39: } ( len: 2; sym: -13 ),
{ 40: } ( len: 3; sym: -13 ),
{ 41: } ( len: 2; sym: -13 ),
{ 42: } ( len: 2; sym: -11 ),
{ 43: } ( len: 3; sym: -11 ),
{ 44: } ( len: 2; sym: -11 ),
{ 45: } ( len: 3; sym: -11 ),
{ 46: } ( len: 2; sym: -11 ),
{ 47: } ( len: 2; sym: -11 ),
{ 48: } ( len: 1; sym: -11 ),
{ 49: } ( len: 1; sym: -11 ),
{ 50: } ( len: 2; sym: -17 ),
{ 51: } ( len: 1; sym: -17 ),
{ 52: } ( len: 3; sym: -20 ),
{ 53: } ( len: 1; sym: -10 ),
{ 54: } ( len: 1; sym: -21 ),
{ 55: } ( len: 2; sym: -21 ),
{ 56: } ( len: 1; sym: -21 ),
{ 57: } ( len: 1; sym: -21 ),
{ 58: } ( len: 2; sym: -21 ),
{ 59: } ( len: 3; sym: -21 ),
{ 60: } ( len: 2; sym: -21 ),
{ 61: } ( len: 1; sym: -21 ),
{ 62: } ( len: 2; sym: -21 ),
{ 63: } ( len: 2; sym: -21 ),
{ 64: } ( len: 1; sym: -21 ),
{ 65: } ( len: 1; sym: -21 ),
{ 66: } ( len: 1; sym: -21 ),
{ 67: } ( len: 1; sym: -19 ),
{ 68: } ( len: 1; sym: -19 ),
{ 69: } ( len: 3; sym: -12 ),
{ 70: } ( len: 4; sym: -12 ),
{ 71: } ( len: 2; sym: -12 ),
{ 72: } ( len: 1; sym: -12 ),
{ 73: } ( len: 2; sym: -23 ),
{ 74: } ( len: 3; sym: -23 ),
{ 75: } ( len: 2; sym: -23 ),
{ 76: } ( len: 1; sym: -25 ),
{ 77: } ( len: 3; sym: -25 ),
{ 78: } ( len: 1; sym: -25 ),
{ 79: } ( len: 1; sym: -26 ),
{ 80: } ( len: 1; sym: -26 ),
{ 81: } ( len: 1; sym: -26 ),
{ 82: } ( len: 2; sym: -22 ),
{ 83: } ( len: 3; sym: -22 ),
{ 84: } ( len: 2; sym: -22 ),
{ 85: } ( len: 2; sym: -22 ),
{ 86: } ( len: 3; sym: -22 ),
{ 87: } ( len: 3; sym: -22 ),
{ 88: } ( len: 1; sym: -22 ),
{ 89: } ( len: 4; sym: -22 ),
{ 90: } ( len: 2; sym: -22 ),
{ 91: } ( len: 4; sym: -22 ),
{ 92: } ( len: 3; sym: -22 ),
{ 93: } ( len: 2; sym: -29 ),
{ 94: } ( len: 3; sym: -29 ),
{ 95: } ( len: 2; sym: -24 ),
{ 96: } ( len: 3; sym: -24 ),
{ 97: } ( len: 2; sym: -24 ),
{ 98: } ( len: 4; sym: -24 ),
{ 99: } ( len: 2; sym: -24 ),
{ 100: } ( len: 4; sym: -24 ),
{ 101: } ( len: 3; sym: -24 ),
{ 102: } ( len: 0; sym: -24 ),
{ 103: } ( len: 1; sym: -27 ),
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
{ 118: } ( len: 3; sym: -30 ),
{ 119: } ( len: 3; sym: -30 ),
{ 120: } ( len: 1; sym: -30 ),
{ 121: } ( len: 3; sym: -31 ),
{ 122: } ( len: 1; sym: -33 ),
{ 123: } ( len: 0; sym: -33 ),
{ 124: } ( len: 1; sym: -32 ),
{ 125: } ( len: 1; sym: -32 ),
{ 126: } ( len: 1; sym: -32 ),
{ 127: } ( len: 3; sym: -32 ),
{ 128: } ( len: 3; sym: -32 ),
{ 129: } ( len: 2; sym: -32 ),
{ 130: } ( len: 2; sym: -32 ),
{ 131: } ( len: 2; sym: -32 ),
{ 132: } ( len: 4; sym: -32 ),
{ 133: } ( len: 4; sym: -32 ),
{ 134: } ( len: 5; sym: -32 ),
{ 135: } ( len: 6; sym: -32 ),
{ 136: } ( len: 4; sym: -32 ),
{ 137: } ( len: 3; sym: -32 ),
{ 138: } ( len: 3; sym: -14 ),
{ 139: } ( len: 1; sym: -14 ),
{ 140: } ( len: 0; sym: -14 ),
{ 141: } ( len: 3; sym: -35 ),
{ 142: } ( len: 1; sym: -35 ),
{ 143: } ( len: 1; sym: -15 ),
{ 144: } ( len: 3; sym: -34 ),
{ 145: } ( len: 1; sym: -34 ),
{ 146: } ( len: 0; sym: -34 ),
{ 147: } ( len: 1; sym: -36 )
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
 Revision 1.4  2000-03-27 21:39:19  peter
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
