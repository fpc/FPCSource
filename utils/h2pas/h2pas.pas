
(* Yacc parser template (TP Yacc V3.0), V1.2 6-17-91 AG *)

(* global definitions: *)

program h2pas;

(*
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
     SysUtils,classes,
     options,scan,converu,lexlib,yacclib;

   type
     YYSTYPE = presobject;

   const
     SHORT_STR  = 'smallint';
     USHORT_STR = 'word';
     INT_STR    = 'longint';
     UINT_STR   = 'dword';
     CHAR_STR   = 'char';
     UCHAR_STR  = 'byte'; { should we use byte or char for 'unsigned char' ?? }
     INT64_STR  = 'int64';
     QWORD_STR  = 'qword';
     REAL_STR   = 'double';
     WCHAR_STR  = 'widechar';

  var
     hp,ph    : presobject;
     implemfile  : text;  (* file for implementation headers extern procs *)
     IsExtern : boolean;
     NeedEllipsisOverload : boolean;
     must_write_packed_field : boolean;
     tempfile : text;
     No_pop   : boolean;
     s,TN,PN  : String;
     pointerprefix: boolean;
     freedynlibproc,
     loaddynlibproc : tstringlist;


(* $ define yydebug
 compile with -dYYDEBUG to get debugging info *)

  const
     (* number of a?b:c construction in one define *)
     if_nb : longint = 0;
     is_packed : boolean = false;
     is_procvar : boolean = false;

  var space_array : array [0..255] of byte;
      space_index : byte;

      { Used when PPointers is used - pointer type definitions }
      PTypeList : TStringList;


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
             delete(aktspace,1,space_array[space_index]);
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


    { This converts pascal reserved words to
      the correct syntax.
    }
    function FixId(const s:string):string;
    const
     maxtokens = 14;
     reservedid: array[1..maxtokens] of string[14] =
       (
         'CLASS',
         'DISPOSE',
         'FUNCTION',
         'FALSE',
         'LABEL',
         'NEW',
         'PROPERTY',
         'PROCEDURE',
         'RECORD',
         'REPEAT',
         'STRING',
         'TYPE',
         'TRUE',
         'UNTIL'
       );
      var
        b : boolean;
        up : string;
        i: integer;
      begin
        if s='' then
         begin
           FixId:='';
           exit;
         end;
        b:=false;
        up:=Uppercase(s);
        for i:=1 to maxtokens do
          begin
            if up=reservedid[i] then
               begin
                  b:=true;
                  break;
                end;
          end;
        if b then
         FixId:='_'+s
        else
         FixId:=s;
      end;



    function TypeName(const s:string):string;
      var
        i : longint;
      begin
        i:=1;
        if RemoveUnderScore and (length(s)>1) and (s[1]='_') then
         i:=2;
        if PrependTypes then
         TypeName:='T'+Copy(s,i,255)
        else
         TypeName:=Copy(s,i,255);
      end;


    function PointerName(const s:string):string;
      var
        i : longint;
      begin
        i:=1;
        if RemoveUnderScore and (length(s)>1) and (s[1]='_') then
         i:=2;
        if UsePPointers then
        begin
         PointerName:='P'+Copy(s,i,255);
         PTypeList.Add(PointerName);
        end
        else
         PointerName:=Copy(s,i,255);
        if PointerPrefix then
           PTypeList.Add('P'+s);
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
         writeln(outfile);
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
         writeln(outfile);
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
                        write(implemfile,aktspace,'function ',name);
                        write(implemfile,'(var a : ',ph,') : ');
                        if not compactmode then
                         shift(2);
                        write_p_a_def(implemfile,hp3^.p1^.p1,hp2^.p1);
                        writeln(implemfile,';');
                        writeln(implemfile,aktspace,'begin');
                        shift(3);
                        write(implemfile,aktspace,name,':=(a.flag',flag_index);
                        writeln(implemfile,' and bm_',ph,'_',name,') shr bp_',ph,'_',name,';');
                        popshift;
                        writeln(implemfile,aktspace,'end;');
                        if not compactmode then
                         popshift;
                        writeln(implemfile,'');
                        { set function in interface }
                        write(outfile,aktspace,'procedure set_',name);
                        write(outfile,'(var a : ',ph,'; __',name,' : ');
                        shift(2);
                        write_p_a_def(outfile,hp3^.p1^.p1,hp2^.p1);
                        writeln(outfile,');');
                        popshift;
                        { set function in implementation }
                        write(implemfile,aktspace,'procedure set_',name);
                        write(implemfile,'(var a : ',ph,'; __',name,' : ');
                        if not compactmode then
                         shift(2);
                        write_p_a_def(implemfile,hp3^.p1^.p1,hp2^.p1);
                        writeln(implemfile,');');
                        writeln(implemfile,aktspace,'begin');
                        shift(3);
                        write(implemfile,aktspace,'a.flag',flag_index,':=');
                        write(implemfile,'a.flag',flag_index,' or ');
                        writeln(implemfile,'((__',name,' shl bp_',ph,'_',name,') and bm_',ph,'_',name,');');
                        popshift;
                        writeln(implemfile,aktspace,'end;');
                        if not compactmode then
                         popshift;
                        writeln(implemfile,'');
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
            t_id,
            t_ifexpr :
              write(outfile,FixId(p^.p));
            t_funexprlist :
              write_funexpr(outfile,p);
             t_exprlist :
               begin
                 if assigned(p^.p1) then
                   write_expr(outfile,p^.p1);
                 if assigned(p^.next) then
                   begin
                     write(', ');
                     write_expr(outfile,p^.next);
                   end;
               end;
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
            t_arrayop :
                    begin
                      write_expr(outfile,p^.p1);
                      write(outfile,p^.p,'[');
                      write_expr(outfile,p^.p2);
                      write(outfile,']');
                      flush(outfile);
                    end;
            t_callop :
                    begin
                      write_expr(outfile,p^.p1);
                      write(outfile,p^.p,'(');
                      write_expr(outfile,p^.p2);
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
             t_callop,
             t_arrayop,
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
                  if not compactmode then
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
                  if not compactmode then
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
         len,para : longint;
         old_in_args : boolean;
         varpara : boolean;
         lastp : presobject;
         hs : string;
      begin
         NeedEllipsisOverload:=false;
         para:=1;
         len:=0;
         lastp:=nil;
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
                   write(outfile,'args:array of const');
                   (* if variable number of args we must allways pop *)
                   no_pop:=false;
                   (* Needs 2 declarations, also one without args, becuase
                      in C you can omit the second parameter. Default parameter
                      doesn't help as that isn't possible with array of const *)
                   NeedEllipsisOverload:=true;
                   (* Remove this para *)
                   if assigned(lastp) then
                    lastp^.next:=nil;
                   dispose(p,done);
                   (* leave the loop as p isnot valid anymore *)
                   break;
                end
              (* we need to correct this in the pp file after *)
              else
                begin
                   (* generate a call by reference parameter ?       *)

//                   varpara:=usevarparas and
//                            assigned(p^.p1^.p2^.p1) and
//                            (p^.p1^.p2^.p1^.typ in [t_addrdef,t_pointerdef]) and
//                            assigned(p^.p1^.p2^.p1^.p1) and
//                            (p^.p1^.p2^.p1^.p1^.typ<>t_procdef);
                   varpara:=usevarparas and
                            assigned(p^.p1^.p1) and
                            (p^.p1^.p1^.typ in [t_addrdef,t_pointerdef]) and
                            assigned(p^.p1^.p1^.p1) and
                            (p^.p1^.p1^.p1^.typ<>t_procdef);
                   (* do not do it for char pointer !!               *)
                   (* para : pchar; and var para : char; are         *)
                   (* completely different in pascal                 *)
                   (* here we exclude all typename containing char   *)
                   (* is this a good method ??                       *)
                   if varpara and
                      (p^.p1^.p1^.typ=t_pointerdef) and
                      (p^.p1^.p1^.p1^.typ=t_id) and
                      (pos('CHAR',uppercase(p^.p1^.p1^.p1^.str))<>0) then
                     varpara:=false;
                   if varpara then
                     begin
                        write(outfile,'var ');
                        inc(len,4);
                     end;

                   (* write new parameter name *)
                   if assigned(p^.p1^.p2^.p2) then
                     begin
                        hs:=FixId(p^.p1^.p2^.p2^.p);
                        write(outfile,hs);
                        inc(len,length(hs));
                     end
                   else
                     begin
                       If removeUnderscore then
                         begin
                           Write (outfile,'para',para);
                           inc(Len,5);
                         end
                       else
                         begin
                           write(outfile,'_para',para);
                           inc(Len,6);
                         end;
                     end;
                   write(outfile,':');
                   if varpara then
                   begin
                     write_p_a_def(outfile,p^.p1^.p2^.p1,p^.p1^.p1^.p1);
                   end
                   else
                     write_p_a_def(outfile,p^.p1^.p2^.p1,p^.p1^.p1);

                end;
              lastp:=p;
              p:=p^.next;
              if assigned(p) then
                begin
                   write(outfile,'; ');
                   { if len>40 then : too complicated to compute }
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
         pointerwritten,
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
                                       pointerwritten:=false;
                                       if (p^.p1=nil) and UsePPointers then
                                        begin
                                          if (simple_type^.typ=t_id) then
                                           begin
                                             write(outfile,PointerName(simple_type^.p));
                                             pointerwritten:=true;
                                           end
                                          { structure }
                                          else if (simple_type^.typ in [t_uniondef,t_structdef]) and
                                                  (simple_type^.p1=nil) and (simple_type^.p2^.typ=t_id) then
                                           begin
                                             write(outfile,PointerName(simple_type^.p2^.p));
                                             pointerwritten:=true;
                                           end;
                                        end;
                                      if not pointerwritten then
                                       begin
                                         if in_args then
                                         begin
                                          write(outfile,'P');
                                          pointerprefix:=true;
                                         end
                                         else
                                          write(outfile,'^');
                                         write_p_a_def(outfile,p^.p1,simple_type);
                                         pointerprefix:=false;
                                       end;
                                     end;
                                end;
                           end;
            t_arraydef : begin
                             constant:=false;
                             if assigned(p^.p2) then
                              begin
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
                              end
                             else
                              begin
                                (* open array *)
                                write(outfile,'array of ');
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
         current_power,
         mask : cardinal;
         flag_index : longint;
         current_level : byte;
         pointerwritten,
         is_sized : boolean;

      begin
         case p^.typ of
            t_id :
              begin
                if pointerprefix then
                   PTypeList.Add('P'+p^.str);
                if p^.intname then
                 write(outfile,p^.p)
                else
                 write(outfile,TypeName(p^.p));
              end;
            { what can we do with void defs  ? }
            t_void :
              write(outfile,'void');
            t_pointerdef :
              begin
                 pointerwritten:=false;
                 if (p^.p1^.typ=t_void) then
                  begin
                    write(outfile,'pointer');
                    pointerwritten:=true;
                  end
                 else
                  if UsePPointers then
                   begin
                     if (p^.p1^.typ=t_id) then
                      begin
                        write(outfile,PointerName(p^.p1^.p));
                        pointerwritten:=true;
                      end
                     { structure }
                     else if (p^.p1^.typ in [t_uniondef,t_structdef]) and
                             (p^.p1^.p1=nil) and (p^.p1^.p2^.typ=t_id) then
                      begin
                        write(outfile,PointerName(p^.p1^.p2^.p));
                        pointerwritten:=true;
                      end;
                   end;
                 if not pointerwritten then
                  begin
                    if in_args then
                    begin
                     write(outfile,'P');
                     pointerprefix:=true;
                    end
                    else
                     write(outfile,'^');
                    write_type_specifier(outfile,p^.p1);
                    pointerprefix:=false;
                  end;
              end;
            t_enumdef :
              begin
                 if (typedef_level>1) and (p^.p1=nil) and
                    (p^.p2^.typ=t_id) then
                   begin
                      if pointerprefix then
                        PTypeList.Add('P'+p^.p2^.str);
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
                 if ((in_args) or (typedef_level>1)) and
                    (p^.p1=nil) and (p^.p2^.typ=t_id) then
                   begin
                      if pointerprefix then
                        PTypeList.Add('P'+p^.p2^.str);
                     write(outfile,TypeName(p^.p2^.p));
                   end
                 else
                   begin
                      if packrecords then
                        writeln(outfile,'packed record')
                      else
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

                                     write(outfile,aktspace,FixId(hp3^.p1^.p2^.p));
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
                                                        inc(mask,current_power);
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
                 inc(typedef_level);
                 if (typedef_level>1) and (p^.p1=nil) and
                    (p^.p2^.typ=t_id) then
                   begin
                      write(outfile,p^.p2^.p);
                   end
                 else
                   begin
                      inc(typedef_level);
                      if packrecords then
                        writeln(outfile,'packed record')
                      else
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
                                write(outfile,FixId(hp3^.p1^.p2^.p),' : ');
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
                 dec(typedef_level);
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
                                write(outfile,FixId(hp1^.p1^.p));
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
const SIGNED = 321;

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
         
         yyval:=nil;
         
       end;
   3 : begin
         
         yyval:=nil;
         
       end;
   4 : begin
         
         writeln(outfile,'(* error ');
         writeln(outfile,yyline);
         
       end;
   5 : begin
         if yydebug then writeln('declaration reduced at line ',line_no);
         if yydebug then writeln(outfile,'(* declaration reduced *)');
         
       end;
   6 : begin
         if yydebug then writeln('define declaration reduced at line ',line_no);
         if yydebug then writeln(outfile,'(* define declaration reduced *)');
         
       end;
   7 : begin
         if yydebug then writeln('declaration reduced at line ',line_no);
         
       end;
   8 : begin
         if yydebug then writeln('define declaration reduced at line ',line_no);
         
       end;
   9 : begin
         yyval:=new(presobject,init_id('extern')); 
       end;
  10 : begin
         yyval:=new(presobject,init_id('intern')); 
       end;
  11 : begin
         yyval:=new(presobject,init_id('no_pop')); 
       end;
  12 : begin
         yyval:=new(presobject,init_id('cdecl')); 
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
         yyval:=new(presobject,init_id('no_pop')); 
       end;
  17 : begin
         yyval:=new(presobject,init_id('no_pop')); 
       end;
  18 : begin
         yyval:=nil 
       end;
  19 : begin
         yyval:=yyv[yysp-1]; 
       end;
  20 : begin
         yyval:=nil; 
       end;
  21 : begin
         
         IsExtern:=false;
         (* by default we must pop the args pushed on stack *)
         no_pop:=false;
         if (assigned(yyv[yysp-2])and assigned(yyv[yysp-2]^.p1)and assigned(yyv[yysp-2]^.p1^.p1))
         and (yyv[yysp-2]^.p1^.p1^.typ=t_procdef) then
         begin
         repeat
         If UseLib then
         IsExtern:=true
         else
         IsExtern:=assigned(yyv[yysp-5])and(yyv[yysp-5]^.str='extern');
         no_pop:=assigned(yyv[yysp-3]) and (yyv[yysp-3]^.str='no_pop');
         
         if (block_type<>bt_func) and not(createdynlib) then
         begin
         writeln(outfile);
         block_type:=bt_func;
         end;
         
         (* dyn. procedures must be put into a var block *)
         if createdynlib then
         begin
         if (block_type<>bt_var) then
         begin
         if not(compactmode) then
         writeln(outfile);
         writeln(outfile,aktspace,'var');
         block_type:=bt_var;
         end;
         shift(2);
         end;
         if not CompactMode then
         begin
         write(outfile,aktspace);
         if not IsExtern then
         write(implemfile,aktspace);
         end;
         (* distinguish between procedure and function *)
         if assigned(yyv[yysp-4]) then
         if (yyv[yysp-4]^.typ=t_void) and (yyv[yysp-2]^.p1^.p1^.p1=nil) then
         begin
         if createdynlib then
         begin
         write(outfile,yyv[yysp-2]^.p1^.p2^.p,' : procedure');
         end
         else
         begin
         shift(10);
         write(outfile,'procedure ',yyv[yysp-2]^.p1^.p2^.p);
         end;
         if assigned(yyv[yysp-2]^.p1^.p1^.p2) then
         write_args(outfile,yyv[yysp-2]^.p1^.p1^.p2);
         if createdynlib then
         begin
         loaddynlibproc.add('pointer('+yyv[yysp-2]^.p1^.p2^.p+'):=GetProcAddress(hlib,'''+yyv[yysp-2]^.p1^.p2^.p+''');');
         freedynlibproc.add(yyv[yysp-2]^.p1^.p2^.p+':=nil;');
         end
         else if not IsExtern then
         begin
         write(implemfile,'procedure ',yyv[yysp-2]^.p1^.p2^.p);
         if assigned(yyv[yysp-2]^.p1^.p1^.p2) then
         write_args(implemfile,yyv[yysp-2]^.p1^.p1^.p2);
         end;
         end
         else
         begin
         if createdynlib then
         begin
         write(outfile,yyv[yysp-2]^.p1^.p2^.p,' : function');
         end
         else
         begin
         shift(9);
         write(outfile,'function ',yyv[yysp-2]^.p1^.p2^.p);
         end;
         
         if assigned(yyv[yysp-2]^.p1^.p1^.p2) then
         write_args(outfile,yyv[yysp-2]^.p1^.p1^.p2);
         write(outfile,':');
         write_p_a_def(outfile,yyv[yysp-2]^.p1^.p1^.p1,yyv[yysp-4]);
         if createdynlib then
         begin
         loaddynlibproc.add('pointer('+yyv[yysp-2]^.p1^.p2^.p+'):=GetProcAddress(hlib,'''+yyv[yysp-2]^.p1^.p2^.p+''');');
         freedynlibproc.add(yyv[yysp-2]^.p1^.p2^.p+':=nil;');
         end
         else if not IsExtern then
         begin
         write(implemfile,'function ',yyv[yysp-2]^.p1^.p2^.p);
         if assigned(yyv[yysp-2]^.p1^.p1^.p2) then
         write_args(implemfile,yyv[yysp-2]^.p1^.p1^.p2);
         write(implemfile,':');
         write_p_a_def(implemfile,yyv[yysp-2]^.p1^.p1^.p1,yyv[yysp-4]);
         end;
         end;
         if assigned(yyv[yysp-1]) then
         write(outfile,';systrap ',yyv[yysp-1]^.p);
         (* No CDECL in interface for Uselib *)
         if IsExtern and (not no_pop) then
         write(outfile,';cdecl');
         popshift;
         if createdynlib then
         begin
         writeln(outfile,';');
         end
         else if UseLib then
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
         writeln(outfile,';');
         if not IsExtern then
         begin
         writeln(implemfile,';');
         writeln(implemfile,aktspace,'begin');
         writeln(implemfile,aktspace,'  { You must implement this function }');
         writeln(implemfile,aktspace,'end;');
         end;
         end;
         IsExtern:=false;
         if not(compactmode) and not(createdynlib) then
         writeln(outfile);
         until not NeedEllipsisOverload;
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
         if assigned(hp^.p1^.p2) and assigned(hp^.p1^.p2^.p) then
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
         write(outfile,';cvar;public');
         end;
         writeln(outfile,';');
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
  22 : begin
         
         if block_type<>bt_type then
         begin
         if not(compactmode) then
         writeln(outfile);
         writeln(outfile,aktspace,'type');
         block_type:=bt_type;
         end;
         shift(3);
         if ( yyv[yysp-1]^.p2  <> nil ) then
         begin
         (* write new type name *)
         TN:=TypeName(yyv[yysp-1]^.p2^.p);
         PN:=PointerName(yyv[yysp-1]^.p2^.p);
         (* define a Pointer type also for structs *)
         if UsePPointers and (Uppercase(tn)<>Uppercase(pn)) and
         assigned(yyv[yysp-1]) and (yyv[yysp-1]^.typ in [t_uniondef,t_structdef]) then
         writeln(outfile,aktspace,PN,' = ^',TN,';');
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
         dispose(hp,done)
         end
         else
         begin
         TN:=TypeName(yyv[yysp-1]^.str);
         PN:=PointerName(yyv[yysp-1]^.str);
         if UsePPointers then writeln(outfile,aktspace,PN,' = ^',TN,';');
         if PackRecords then
         writeln(outfile, aktspace, TN, ' = packed record')
         else
         writeln(outfile, aktspace, TN, ' = record');
         writeln(outfile, aktspace, '    {undefined structure}');
         writeln(outfile, aktspace, '  end;');
         writeln(outfile);
         popshift;
         end;
         
       end;
  23 : begin
         
         (* TYPEDEF STRUCT dname dname SEMICOLON *)
         if block_type<>bt_type then
         begin
         if not(compactmode) then
         writeln(outfile);
         writeln(outfile,aktspace,'type');
         block_type:=bt_type;
         end;
         PN:=TypeName(yyv[yysp-2]^.p);
         TN:=TypeName(yyv[yysp-1]^.p);
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
  24 : begin
         
         (* TYPEDEF type_specifier LKLAMMER dec_modifier declarator RKLAMMER maybe_space LKLAMMER argument_declaration_list RKLAMMER SEMICOLON *)
         if block_type<>bt_type then
         begin
         if not(compactmode) then
         writeln(outfile);
         writeln(outfile,aktspace,'type');
         block_type:=bt_type;
         end;
         no_pop:=assigned(yyv[yysp-7]) and (yyv[yysp-7]^.str='no_pop');
         shift(3);
         (* walk through all declarations *)
         hp:=yyv[yysp-6];
         if assigned(hp) then
         begin
         hp:=yyv[yysp-6];
         while assigned(hp^.p1) do
         hp:=hp^.p1;
         hp^.p1:=new(presobject,init_two(t_procdef,nil,yyv[yysp-2]));
         hp:=yyv[yysp-6];
         if assigned(hp^.p1) and assigned(hp^.p1^.p1) then
         begin
         writeln(outfile);
         (* write new type name *)
         write(outfile,aktspace,TypeName(hp^.p2^.p),' = ');
         shift(2);
         write_p_a_def(outfile,hp^.p1,yyv[yysp-9]);
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
         if assigned(yyv[yysp-9])then
         dispose(yyv[yysp-9],done);
         if assigned(yyv[yysp-7])then
         dispose(yyv[yysp-7],done);
         if assigned(yyv[yysp-6])then (* disposes also yyv[yysp-2] *)
         dispose(yyv[yysp-6],done);
         
       end;
  25 : begin
         
         (* TYPEDEF type_specifier dec_modifier declarator_list SEMICOLON *)
         if block_type<>bt_type then
         begin
         if not(compactmode) then
         writeln(outfile);
         writeln(outfile,aktspace,'type');
         block_type:=bt_type;
         end;
         no_pop:=assigned(yyv[yysp-2]) and (yyv[yysp-2]^.str='no_pop');
         shift(3);
         (* Get the name to write the type definition for, try
         to use the tag name first *)
         if assigned(yyv[yysp-3]^.p2) then
         begin
         ph:=yyv[yysp-3]^.p2;
         end
         else
         begin
         if not assigned(yyv[yysp-1]^.p1^.p2) then
         internalerror(4444);
         ph:=yyv[yysp-1]^.p1^.p2;
         end;
         (* write type definition *)
         is_procvar:=false;
         writeln(outfile);
         TN:=TypeName(ph^.p);
         PN:=PointerName(ph^.p);
         if UsePPointers and (Uppercase(tn)<>Uppercase(pn)) and
         assigned(yyv[yysp-3]) and (yyv[yysp-3]^.typ<>t_procdef) then
         writeln(outfile,aktspace,PN,' = ^',TN,';');
         (* write new type name *)
         write(outfile,aktspace,TN,' = ');
         shift(2);
         write_type_specifier(outfile,yyv[yysp-3]);
         popshift;
         (* if no_pop it is normal fpc calling convention *)
         if is_procvar and
         (not no_pop) then
         write(outfile,';cdecl');
         writeln(outfile,';');
         flush(outfile);
         (* write alias names, ph points to the name already used *)
         hp:=yyv[yysp-1];
         while assigned(hp) do
         begin
         if (hp<>ph) and assigned(hp^.p1^.p2) then
         begin
         PN:=TypeName(ph^.p);
         TN:=TypeName(hp^.p1^.p2^.p);
         if Uppercase(TN)<>Uppercase(PN) then
         begin
         write(outfile,aktspace,TN,' = ');
         write_p_a_def(outfile,hp^.p1^.p1,ph);
         writeln(outfile,';');
         PN:=PointerName(hp^.p1^.p2^.p);
         if UsePPointers and (Uppercase(tn)<>Uppercase(pn)) and
         assigned(yyv[yysp-3]) and (yyv[yysp-3]^.typ<>t_procdef) then
         writeln(outfile,aktspace,PN,' = ^',TN,';');
         end;
         end;
         hp:=hp^.next;
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
  26 : begin
         
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
  27 : begin
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
  28 : begin
         
         (* DEFINE dname LKLAMMER enum_list RKLAMMER para_def_expr NEW_LINE *)
         if not stripinfo then
         begin
         writeln (outfile,aktspace,'{ was #define dname(params) para_def_expr }');
         writeln (implemfile,aktspace,'{ was #define dname(params) para_def_expr }');
         if assigned(yyv[yysp-3]) then
         begin
         writeln (outfile,aktspace,'{ argument types are unknown }');
         writeln (implemfile,aktspace,'{ argument types are unknown }');
         end;
         if not assigned(yyv[yysp-1]^.p3) then
         begin
         writeln(outfile,aktspace,'{ return type might be wrong }   ');
         writeln(implemfile,aktspace,'{ return type might be wrong }   ');
         end;
         end;
         block_type:=bt_func;
         write(outfile,aktspace,'function ',yyv[yysp-5]^.p);
         write(implemfile,aktspace,'function ',yyv[yysp-5]^.p);
         
         if assigned(yyv[yysp-3]) then
         begin
         write(outfile,'(');
         write(implemfile,'(');
         ph:=new(presobject,init_one(t_enumdef,yyv[yysp-3]));
         write_def_params(outfile,ph);
         write_def_params(implemfile,ph);
         if assigned(ph) then dispose(ph,done);
         ph:=nil;
         (* types are unknown *)
         write(outfile,' : longint)');
         write(implemfile,' : longint)');
         end;
         if not assigned(yyv[yysp-1]^.p3) then
         begin
         writeln(outfile,' : longint;',aktspace,commentstr);
         writeln(implemfile,' : longint;');
         flush(outfile);
         end
         else
         begin
         write(outfile,' : ');
         write_type_specifier(outfile,yyv[yysp-1]^.p3);
         writeln(outfile,';',aktspace,commentstr);
         flush(outfile);
         write(implemfile,' : ');
         write_type_specifier(implemfile,yyv[yysp-1]^.p3);
         writeln(implemfile,';');
         end;
         writeln(outfile);
         flush(outfile);
         hp:=new(presobject,init_two(t_funcname,yyv[yysp-5],yyv[yysp-1]));
         write_funexpr(implemfile,hp);
         writeln(implemfile);
         flush(implemfile);
         if assigned(hp)then dispose(hp,done);
         
       end;
  29 : begin
         
         (* DEFINE dname SPACE_DEFINE NEW_LINE *)
         writeln(outfile,'{$define ',yyv[yysp-2]^.p,'}',aktspace,commentstr);
         flush(outfile);
         if assigned(yyv[yysp-2])then
         dispose(yyv[yysp-2],done);
         
       end;
  30 : begin
         
         writeln(outfile,'{$define ',yyv[yysp-1]^.p,'}',aktspace,commentstr);
         flush(outfile);
         if assigned(yyv[yysp-1])then
         dispose(yyv[yysp-1],done);
         
       end;
  31 : begin
         
         (* DEFINE dname SPACE_DEFINE def_expr NEW_LINE *)
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
         shift(3);
         write(outfile,aktspace,yyv[yysp-3]^.p);
         write(outfile,' = ');
         flush(outfile);
         write_expr(outfile,yyv[yysp-1]^.p1);
         writeln(outfile,';',aktspace,commentstr);
         popshift;
         if assigned(yyv[yysp-3]) then
         dispose(yyv[yysp-3],done);
         if assigned(yyv[yysp-1]) then
         dispose(yyv[yysp-1],done);
         end
         else
         begin
         if not stripinfo then
         begin
         writeln (outfile,aktspace,'{ was #define dname def_expr }');
         writeln (implemfile,aktspace,'{ was #define dname def_expr }');
         end;
         block_type:=bt_func;
         write(outfile,aktspace,'function ',yyv[yysp-3]^.p);
         write(implemfile,aktspace,'function ',yyv[yysp-3]^.p);
         shift(2);
         if not assigned(yyv[yysp-1]^.p3) then
         begin
         writeln(outfile,' : longint;');
         writeln(outfile,aktspace,'  { return type might be wrong }');
         flush(outfile);
         writeln(implemfile,' : longint;');
         writeln(implemfile,aktspace,'  { return type might be wrong }');
         end
         else
         begin
         write(outfile,' : ');
         write_type_specifier(outfile,yyv[yysp-1]^.p3);
         writeln(outfile,';',aktspace,commentstr);
         flush(outfile);
         write(implemfile,' : ');
         write_type_specifier(implemfile,yyv[yysp-1]^.p3);
         writeln(implemfile,';');
         end;
         writeln(outfile);
         flush(outfile);
         hp:=new(presobject,init_two(t_funcname,yyv[yysp-3],yyv[yysp-1]));
         write_funexpr(implemfile,hp);
         popshift;
         dispose(hp,done);
         writeln(implemfile);
         flush(implemfile);
         end;
         
       end;
  32 : begin
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
  33 : begin
         yyval:=yyv[yysp-1];
       end;
  34 : begin
         writeln(outfile,' in member_list *)');
         yyerrok;
         yyval:=nil;
         
       end;
  35 : begin
         yyval:=yyv[yysp-1];
       end;
  36 : begin
         writeln(outfile,' in enum_list *)');
         yyerrok;
         yyval:=nil;
         
       end;
  37 : begin
         
         if (not is_packed) and (not packrecords) then
         writeln(outfile,'{$PACKRECORDS 1}');
         is_packed:=true;
         yyval:=new(presobject,init_two(t_structdef,yyv[yysp-1],yyv[yysp-2]));
         
       end;
  38 : begin
         
         if (is_packed) and (not packrecords) then
         writeln(outfile,'{$PACKRECORDS 4}');
         is_packed:=false;
         yyval:=new(presobject,init_two(t_structdef,yyv[yysp-0],yyv[yysp-1]));
         
       end;
  39 : begin
         
         if (not is_packed) and (not packrecords) then
         writeln(outfile,'{$PACKRECORDS 1}');
         is_packed:=true;
         yyval:=new(presobject,init_two(t_uniondef,yyv[yysp-1],yyv[yysp-2]));
         
       end;
  40 : begin
         
         yyval:=new(presobject,init_two(t_uniondef,yyv[yysp-0],yyv[yysp-1]));
         
       end;
  41 : begin
         
         yyval:=yyv[yysp-0];
         
       end;
  42 : begin
         
         yyval:=yyv[yysp-0];
         
       end;
  43 : begin
         
         yyval:=new(presobject,init_two(t_enumdef,yyv[yysp-0],yyv[yysp-1]));
         
       end;
  44 : begin
         
         yyval:=yyv[yysp-0];
         
       end;
  45 : begin
         
         if not stripinfo then
         writeln(outfile,'(* Const before type ignored *)');
         yyval:=yyv[yysp-0];
         
       end;
  46 : begin
         
         if (not is_packed) and (not packrecords)then
         writeln(outfile,'{$PACKRECORDS 1}');
         is_packed:=true;
         yyval:=new(presobject,init_one(t_uniondef,yyv[yysp-1]));
         
       end;
  47 : begin
         
         yyval:=new(presobject,init_one(t_uniondef,yyv[yysp-0]));
         
       end;
  48 : begin
         
         if (not is_packed) and (not packrecords) then
         writeln(outfile,'{$PACKRECORDS 1}');
         is_packed:=true;
         yyval:=new(presobject,init_one(t_structdef,yyv[yysp-1]));
         
       end;
  49 : begin
         
         if (is_packed) and (not packrecords) then
         writeln(outfile,'{$PACKRECORDS 4}');
         is_packed:=false;
         yyval:=new(presobject,init_one(t_structdef,yyv[yysp-0]));
         
       end;
  50 : begin
         
         yyval:=new(presobject,init_one(t_enumdef,yyv[yysp-0]));
         
       end;
  51 : begin
         
         yyval:=yyv[yysp-0];
         
       end;
  52 : begin
         yyval:=yyv[yysp-0]; 
       end;
  53 : begin
         
         yyval:=new(presobject,init_one(t_memberdeclist,yyv[yysp-1]));
         yyval^.next:=yyv[yysp-0];
         
       end;
  54 : begin
         
         yyval:=new(presobject,init_one(t_memberdeclist,yyv[yysp-0]));
         
       end;
  55 : begin
         
         yyval:=new(presobject,init_two(t_memberdec,yyv[yysp-2],yyv[yysp-1]));
         
       end;
  56 : begin
         (*dname*)
         yyval:=new(presobject,init_id(act_token));
         
       end;
  57 : begin
         
         hp:=yyv[yysp-0];
         yyval:=hp;
         if assigned(hp) then
         begin
         s:=strpas(hp^.p);
         if s=UINT_STR then
         s:=INT_STR
         else if s=USHORT_STR then
         s:=SHORT_STR
         else if s=UCHAR_STR then
         s:=CHAR_STR
         else if s=QWORD_STR then
         s:=INT64_STR
         else
         s:='';
         if s<>'' then
         hp^.setstr(s);
         end;
         
       end;
  58 : begin
         
         hp:=yyv[yysp-0];
         yyval:=hp;
         if assigned(hp) then
         begin
         s:=strpas(hp^.p);
         if s=INT_STR then
         s:=UINT_STR
         else if s=SHORT_STR then
         s:=USHORT_STR
         else if s=CHAR_STR then
         s:=UCHAR_STR
         else if s=INT64_STR then
         s:=QWORD_STR
         else
         s:='';
         if s<>'' then
         hp^.setstr(s);
         end;
         
       end;
  59 : begin
         
         yyval:=new(presobject,init_intid(INT_STR));
         
       end;
  60 : begin
         
         yyval:=new(presobject,init_intid(INT_STR));
         
       end;
  61 : begin
         
         yyval:=new(presobject,init_intid(INT_STR));
         
       end;
  62 : begin
         
         yyval:=new(presobject,init_intid(INT64_STR));
         
       end;
  63 : begin
         
         yyval:=new(presobject,init_intid(INT64_STR));
         
       end;
  64 : begin
         
         yyval:=new(presobject,init_intid(SHORT_STR));
         
       end;
  65 : begin
         
         yyval:=new(presobject,init_intid(SHORT_STR));
         
       end;
  66 : begin
         
         yyval:=new(presobject,init_intid(REAL_STR));
         
       end;
  67 : begin
         
         yyval:=new(presobject,init_no(t_void));
         
       end;
  68 : begin
         
         yyval:=new(presobject,init_intid(CHAR_STR));
         
       end;
  69 : begin
         
         yyval:=new(presobject,init_intid(UINT_STR));
         
       end;
  70 : begin
         
         yyval:=yyv[yysp-0];
         
       end;
  71 : begin
         
         yyval:=yyv[yysp-0];
         tn:=yyval^.str;
         if removeunderscore and
         (length(tn)>1) and (tn[1]='_') then
         yyval^.setstr(Copy(tn,2,length(tn)-1));
         
       end;
  72 : begin
         
         yyval:=yyv[yysp-2];
         hp:=yyv[yysp-2];
         while assigned(hp^.next) do
         hp:=hp^.next;
         hp^.next:=new(presobject,init_one(t_declist,yyv[yysp-0]));
         
       end;
  73 : begin
         
         writeln(outfile,' in declarator_list *)');
         yyval:=yyv[yysp-0];
         yyerrok;
         
       end;
  74 : begin
         
         writeln(outfile,' in declarator_list *)');
         yyerrok;
         
       end;
  75 : begin
         
         yyval:=new(presobject,init_one(t_declist,yyv[yysp-0]));
         
       end;
  76 : begin
         
         yyval:=new(presobject,init_two(t_arg,yyv[yysp-1],yyv[yysp-0]));
         
       end;
  77 : begin
         
         (* type_specifier STAR declarator *)
         hp:=new(presobject,init_one(t_pointerdef,yyv[yysp-2]));
         yyval:=new(presobject,init_two(t_arg,hp,yyv[yysp-0]));
         
       end;
  78 : begin
         
         yyval:=new(presobject,init_two(t_arg,yyv[yysp-1],yyv[yysp-0]));
         
       end;
  79 : begin
         
         yyval:=new(presobject,init_two(t_arglist,yyv[yysp-0],nil));
         
       end;
  80 : begin
         
         yyval:=new(presobject,init_two(t_arglist,yyv[yysp-2],nil));
         yyval^.next:=yyv[yysp-0];
         
       end;
  81 : begin
         
         yyval:=new(presobject,init_two(t_arglist,ellipsisarg,nil));
         
       end;
  82 : begin
         
         yyval:=nil;
         
       end;
  83 : begin
         yyval:=new(presobject,init_id('far'));
       end;
  84 : begin
         yyval:=new(presobject,init_id('near'));
       end;
  85 : begin
         yyval:=new(presobject,init_id('huge'));
       end;
  86 : begin
         
         if not stripinfo then
         writeln(outfile,'(* Const before declarator ignored *)');
         yyval:=yyv[yysp-0];
         
       end;
  87 : begin
         
         if not stripinfo then
         writeln(outfile,aktspace,'(* ',yyv[yysp-2]^.p,' ignored *)');
         dispose(yyv[yysp-2],done);
         hp:=yyv[yysp-0];
         yyval:=hp;
         while assigned(hp^.p1) do
         hp:=hp^.p1;
         hp^.p1:=new(presobject,init_one(t_pointerdef,nil));
         
       end;
  88 : begin
         
         (* %prec PSTAR this was wrong!! *)
         hp:=yyv[yysp-0];
         yyval:=hp;
         while assigned(hp^.p1) do
         hp:=hp^.p1;
         hp^.p1:=new(presobject,init_one(t_pointerdef,nil));
         
       end;
  89 : begin
         
         hp:=yyv[yysp-0];
         yyval:=hp;
         while assigned(hp^.p1) do
         hp:=hp^.p1;
         hp^.p1:=new(presobject,init_one(t_addrdef,nil));
         
       end;
  90 : begin
         
         (*  size specifier supported *)
         hp:=new(presobject,init_one(t_size_specifier,yyv[yysp-0]));
         yyval:=new(presobject,init_three(t_dec,nil,yyv[yysp-2],hp));
         
       end;
  91 : begin
         
         if not stripinfo then
         writeln(outfile,'(* Warning : default value for ',yyv[yysp-2]^.p,' ignored *)');
         hp:=new(presobject,init_one(t_default_value,yyv[yysp-0]));
         yyval:=new(presobject,init_three(t_dec,nil,yyv[yysp-2],hp));
         
       end;
  92 : begin
         
         yyval:=new(presobject,init_two(t_dec,nil,yyv[yysp-0]));
         
       end;
  93 : begin
         
         hp:=yyv[yysp-3];
         yyval:=hp;
         while assigned(hp^.p1) do
         hp:=hp^.p1;
         hp^.p1:=new(presobject,init_two(t_procdef,nil,yyv[yysp-1]));
         
       end;
  94 : begin
         
         hp:=yyv[yysp-1];
         yyval:=hp;
         while assigned(hp^.p1) do
         hp:=hp^.p1;
         hp^.p1:=new(presobject,init_two(t_procdef,nil,nil));
         
       end;
  95 : begin
         
         hp:=yyv[yysp-3];
         yyval:=hp;
         while assigned(hp^.p1) do
         hp:=hp^.p1;
         hp^.p1:=new(presobject,init_two(t_arraydef,nil,yyv[yysp-1]));
         
       end;
  96 : begin
         
         (* this is translated into a pointer *)
         hp:=yyv[yysp-2];
         yyval:=hp;
         while assigned(hp^.p1) do
         hp:=hp^.p1;
         hp^.p1:=new(presobject,init_two(t_arraydef,nil,nil));
         
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
         
         (* this is translated into a pointer *)
         hp:=yyv[yysp-2];
         yyval:=hp;
         while assigned(hp^.p1) do
         hp:=hp^.p1;
         hp^.p1:=new(presobject,init_two(t_arraydef,nil,nil));
         
       end;
 107 : begin
         
         yyval:=yyv[yysp-1];
         
       end;
 108 : begin
         
         yyval:=new(presobject,init_two(t_dec,nil,nil));
         
       end;
 109 : begin
         yyval:=yyv[yysp-0];
       end;
 110 : begin
         yyval:=new(presobject,init_bop(' = ',yyv[yysp-2],yyv[yysp-0]));
       end;
 111 : begin
         yyval:=new(presobject,init_bop(' <> ',yyv[yysp-2],yyv[yysp-0]));
       end;
 112 : begin
         yyval:=new(presobject,init_bop(' > ',yyv[yysp-2],yyv[yysp-0]));
       end;
 113 : begin
         yyval:=new(presobject,init_bop(' >= ',yyv[yysp-2],yyv[yysp-0]));
       end;
 114 : begin
         yyval:=new(presobject,init_bop(' < ',yyv[yysp-2],yyv[yysp-0]));
       end;
 115 : begin
         yyval:=new(presobject,init_bop(' <= ',yyv[yysp-2],yyv[yysp-0]));
       end;
 116 : begin
         yyval:=new(presobject,init_bop(' + ',yyv[yysp-2],yyv[yysp-0]));
       end;
 117 : begin
         yyval:=new(presobject,init_bop(' - ',yyv[yysp-2],yyv[yysp-0]));
       end;
 118 : begin
         yyval:=new(presobject,init_bop(' * ',yyv[yysp-2],yyv[yysp-0]));
       end;
 119 : begin
         yyval:=new(presobject,init_bop(' / ',yyv[yysp-2],yyv[yysp-0]));
       end;
 120 : begin
         yyval:=new(presobject,init_bop(' or ',yyv[yysp-2],yyv[yysp-0]));
       end;
 121 : begin
         yyval:=new(presobject,init_bop(' and ',yyv[yysp-2],yyv[yysp-0]));
       end;
 122 : begin
         yyval:=new(presobject,init_bop(' not ',yyv[yysp-2],yyv[yysp-0]));
       end;
 123 : begin
         yyval:=new(presobject,init_bop(' shl ',yyv[yysp-2],yyv[yysp-0]));
       end;
 124 : begin
         yyval:=new(presobject,init_bop(' shr ',yyv[yysp-2],yyv[yysp-0]));
       end;
 125 : begin
         yyv[yysp-0]^.p1:=yyv[yysp-2];
         yyval:=yyv[yysp-0];
         inc(if_nb);
         yyval^.p:=strpnew('if_local'+str(if_nb));
         
       end;
 126 : begin
         yyval:=yyv[yysp-0];
       end;
 127 : begin
         (* if A then B else C *)
         yyval:=new(presobject,init_three(t_ifexpr,nil,yyv[yysp-2],yyv[yysp-0]));
       end;
 128 : begin
         yyval:=yyv[yysp-0]; 
       end;
 129 : begin
         yyval:=nil;
       end;
 130 : begin
         
         yyval:=yyv[yysp-0];
         
       end;
 131 : begin
         
         yyval:=yyv[yysp-0];
         
       end;
 132 : begin
         
         (* remove L prefix for widestrings *)
         s:=act_token;
         if Win32headers and (s[1]='L') then
         delete(s,1,1);
         yyval:=new(presobject,init_id(''''+copy(s,2,length(s)-2)+''''));
         
       end;
 133 : begin
         
         yyval:=new(presobject,init_id(act_token));
         
       end;
 134 : begin
         
         yyval:=new(presobject,init_bop('.',yyv[yysp-2],yyv[yysp-0]));
         
       end;
 135 : begin
         
         yyval:=new(presobject,init_bop('^.',yyv[yysp-2],yyv[yysp-0]));
         
       end;
 136 : begin
         
         yyval:=new(presobject,init_preop('-',yyv[yysp-0]));
         
       end;
 137 : begin
         
         yyval:=new(presobject,init_preop('@',yyv[yysp-0]));
         
       end;
 138 : begin
         
         yyval:=new(presobject,init_preop(' not ',yyv[yysp-0]));
         
       end;
 139 : begin
         
         if assigned(yyv[yysp-0]) then
         yyval:=new(presobject,init_two(t_typespec,yyv[yysp-2],yyv[yysp-0]))
         else
         yyval:=yyv[yysp-2];
         
       end;
 140 : begin
         
         yyval:=new(presobject,init_two(t_typespec,yyv[yysp-2],yyv[yysp-0]));
         
       end;
 141 : begin
         
         hp:=new(presobject,init_one(t_pointerdef,yyv[yysp-3]));
         yyval:=new(presobject,init_two(t_typespec,hp,yyv[yysp-0]));
         
       end;
 142 : begin
         
         if not stripinfo then
         writeln(outfile,aktspace,'(* ',yyv[yysp-3]^.p,' ignored *)');
         dispose(yyv[yysp-3],done);
         write_type_specifier(outfile,yyv[yysp-4]);
         writeln(outfile,' ignored *)');
         hp:=new(presobject,init_one(t_pointerdef,yyv[yysp-4]));
         yyval:=new(presobject,init_two(t_typespec,hp,yyv[yysp-0]));
         
       end;
 143 : begin
         
         hp:=new(presobject,init_one(t_exprlist,yyv[yysp-3]));
         yyval:=new(presobject,init_three(t_funexprlist,hp,yyv[yysp-1],nil));
         
       end;
 144 : begin
         
         yyval:=yyv[yysp-1];
         
       end;
 145 : begin
         
         yyval:=new(presobject,init_two(t_callop,yyv[yysp-5],yyv[yysp-1]));
         
       end;
 146 : begin
         
         yyval:=new(presobject,init_two(t_arrayop,yyv[yysp-3],yyv[yysp-1]));
         
       end;
 147 : begin
         (*enum_element COMMA enum_list *)
         yyval:=yyv[yysp-2];
         yyval^.next:=yyv[yysp-0];
         
       end;
 148 : begin
         
         yyval:=yyv[yysp-0];
         
       end;
 149 : begin
         (* empty enum list *)
         yyval:=nil;
       end;
 150 : begin
         begin (*enum_element: dname _ASSIGN expr *)
         yyval:=new(presobject,init_two(t_enumlist,yyv[yysp-2],yyv[yysp-0]));
         end;
         
       end;
 151 : begin
         
         begin (*enum_element: dname*)
         yyval:=new(presobject,init_two(t_enumlist,yyv[yysp-0],nil));
         end;
         
       end;
 152 : begin
         
         if yyv[yysp-0]^.typ=t_funexprlist then
         yyval:=yyv[yysp-0]
         else
         yyval:=new(presobject,init_two(t_exprlist,yyv[yysp-0],nil));
         (* if here is a type specifier
         we know the return type *)
         if (yyv[yysp-0]^.typ=t_typespec) then
         yyval^.p3:=yyv[yysp-0]^.p1^.get_copy;
         
       end;
 153 : begin
         
         yyval:=yyv[yysp-0];
         
       end;
 154 : begin
         
         yyval:=yyv[yysp-1]
         
       end;
 155 : begin
         (*exprlist COMMA expr*)
         yyval:=yyv[yysp-2];
         yyv[yysp-2]^.next:=yyv[yysp-0];
         
       end;
 156 : begin
         
         yyval:=yyv[yysp-0];
         
       end;
 157 : begin
         (* empty expression list *)
         yyval:=nil; 
       end;
 158 : begin
         
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

yynacts   = 2490;
yyngotos  = 389;
yynstates = 291;
yynrules  = 158;

yya : array [1..yynacts] of YYARec = (
{ 0: }
  ( sym: 256; act: 7 ),
  ( sym: 257; act: 8 ),
  ( sym: 258; act: 9 ),
  ( sym: 268; act: 10 ),
  ( sym: 269; act: 11 ),
  ( sym: 270; act: 12 ),
  ( sym: 288; act: 13 ),
  ( sym: 271; act: -10 ),
  ( sym: 274; act: -10 ),
  ( sym: 275; act: -10 ),
  ( sym: 276; act: -10 ),
  ( sym: 277; act: -10 ),
  ( sym: 278; act: -10 ),
  ( sym: 279; act: -10 ),
  ( sym: 280; act: -10 ),
  ( sym: 281; act: -10 ),
  ( sym: 321; act: -10 ),
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
  ( sym: 321; act: 32 ),
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
  ( sym: 271; act: -10 ),
  ( sym: 274; act: -10 ),
  ( sym: 275; act: -10 ),
  ( sym: 276; act: -10 ),
  ( sym: 277; act: -10 ),
  ( sym: 278; act: -10 ),
  ( sym: 279; act: -10 ),
  ( sym: 280; act: -10 ),
  ( sym: 281; act: -10 ),
  ( sym: 321; act: -10 ),
{ 6: }
  ( sym: 0; act: 0 ),
{ 7: }
{ 8: }
  ( sym: 268; act: 38 ),
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
  ( sym: 321; act: 32 ),
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
  ( sym: 289; act: 44 ),
  ( sym: 290; act: 45 ),
  ( sym: 291; act: 46 ),
  ( sym: 292; act: 47 ),
  ( sym: 293; act: 48 ),
  ( sym: 294; act: 49 ),
  ( sym: 295; act: 50 ),
  ( sym: 256; act: -18 ),
  ( sym: 262; act: -18 ),
  ( sym: 271; act: -18 ),
  ( sym: 281; act: -18 ),
  ( sym: 282; act: -18 ),
  ( sym: 283; act: -18 ),
  ( sym: 284; act: -18 ),
  ( sym: 308; act: -18 ),
  ( sym: 313; act: -18 ),
{ 19: }
{ 20: }
  ( sym: 256; act: 52 ),
  ( sym: 266; act: 53 ),
  ( sym: 271; act: 23 ),
{ 21: }
  ( sym: 256; act: 52 ),
  ( sym: 266; act: 53 ),
  ( sym: 271; act: 23 ),
{ 22: }
  ( sym: 256; act: 56 ),
  ( sym: 266; act: 57 ),
  ( sym: 271; act: 23 ),
{ 23: }
{ 24: }
  ( sym: 277; act: 58 ),
  ( sym: 256; act: -64 ),
  ( sym: 259; act: -64 ),
  ( sym: 260; act: -64 ),
  ( sym: 261; act: -64 ),
  ( sym: 262; act: -64 ),
  ( sym: 263; act: -64 ),
  ( sym: 264; act: -64 ),
  ( sym: 265; act: -64 ),
  ( sym: 267; act: -64 ),
  ( sym: 271; act: -64 ),
  ( sym: 281; act: -64 ),
  ( sym: 282; act: -64 ),
  ( sym: 283; act: -64 ),
  ( sym: 284; act: -64 ),
  ( sym: 286; act: -64 ),
  ( sym: 289; act: -64 ),
  ( sym: 290; act: -64 ),
  ( sym: 291; act: -64 ),
  ( sym: 292; act: -64 ),
  ( sym: 293; act: -64 ),
  ( sym: 294; act: -64 ),
  ( sym: 295; act: -64 ),
  ( sym: 296; act: -64 ),
  ( sym: 300; act: -64 ),
  ( sym: 301; act: -64 ),
  ( sym: 302; act: -64 ),
  ( sym: 303; act: -64 ),
  ( sym: 304; act: -64 ),
  ( sym: 305; act: -64 ),
  ( sym: 306; act: -64 ),
  ( sym: 307; act: -64 ),
  ( sym: 308; act: -64 ),
  ( sym: 309; act: -64 ),
  ( sym: 310; act: -64 ),
  ( sym: 311; act: -64 ),
  ( sym: 312; act: -64 ),
  ( sym: 313; act: -64 ),
  ( sym: 314; act: -64 ),
  ( sym: 315; act: -64 ),
  ( sym: 318; act: -64 ),
  ( sym: 319; act: -64 ),
{ 25: }
  ( sym: 274; act: 24 ),
  ( sym: 275; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 277; act: 27 ),
  ( sym: 278; act: 28 ),
  ( sym: 279; act: 29 ),
  ( sym: 280; act: 30 ),
  ( sym: 321; act: 32 ),
  ( sym: 256; act: -69 ),
  ( sym: 259; act: -69 ),
  ( sym: 260; act: -69 ),
  ( sym: 261; act: -69 ),
  ( sym: 262; act: -69 ),
  ( sym: 263; act: -69 ),
  ( sym: 264; act: -69 ),
  ( sym: 265; act: -69 ),
  ( sym: 267; act: -69 ),
  ( sym: 271; act: -69 ),
  ( sym: 281; act: -69 ),
  ( sym: 282; act: -69 ),
  ( sym: 283; act: -69 ),
  ( sym: 284; act: -69 ),
  ( sym: 286; act: -69 ),
  ( sym: 289; act: -69 ),
  ( sym: 290; act: -69 ),
  ( sym: 291; act: -69 ),
  ( sym: 292; act: -69 ),
  ( sym: 293; act: -69 ),
  ( sym: 294; act: -69 ),
  ( sym: 295; act: -69 ),
  ( sym: 296; act: -69 ),
  ( sym: 300; act: -69 ),
  ( sym: 301; act: -69 ),
  ( sym: 302; act: -69 ),
  ( sym: 303; act: -69 ),
  ( sym: 304; act: -69 ),
  ( sym: 305; act: -69 ),
  ( sym: 306; act: -69 ),
  ( sym: 307; act: -69 ),
  ( sym: 308; act: -69 ),
  ( sym: 309; act: -69 ),
  ( sym: 310; act: -69 ),
  ( sym: 311; act: -69 ),
  ( sym: 312; act: -69 ),
  ( sym: 313; act: -69 ),
  ( sym: 314; act: -69 ),
  ( sym: 315; act: -69 ),
  ( sym: 318; act: -69 ),
  ( sym: 319; act: -69 ),
{ 26: }
  ( sym: 276; act: 60 ),
  ( sym: 277; act: 61 ),
  ( sym: 256; act: -60 ),
  ( sym: 259; act: -60 ),
  ( sym: 260; act: -60 ),
  ( sym: 261; act: -60 ),
  ( sym: 262; act: -60 ),
  ( sym: 263; act: -60 ),
  ( sym: 264; act: -60 ),
  ( sym: 265; act: -60 ),
  ( sym: 267; act: -60 ),
  ( sym: 271; act: -60 ),
  ( sym: 281; act: -60 ),
  ( sym: 282; act: -60 ),
  ( sym: 283; act: -60 ),
  ( sym: 284; act: -60 ),
  ( sym: 286; act: -60 ),
  ( sym: 289; act: -60 ),
  ( sym: 290; act: -60 ),
  ( sym: 291; act: -60 ),
  ( sym: 292; act: -60 ),
  ( sym: 293; act: -60 ),
  ( sym: 294; act: -60 ),
  ( sym: 295; act: -60 ),
  ( sym: 296; act: -60 ),
  ( sym: 300; act: -60 ),
  ( sym: 301; act: -60 ),
  ( sym: 302; act: -60 ),
  ( sym: 303; act: -60 ),
  ( sym: 304; act: -60 ),
  ( sym: 305; act: -60 ),
  ( sym: 306; act: -60 ),
  ( sym: 307; act: -60 ),
  ( sym: 308; act: -60 ),
  ( sym: 309; act: -60 ),
  ( sym: 310; act: -60 ),
  ( sym: 311; act: -60 ),
  ( sym: 312; act: -60 ),
  ( sym: 313; act: -60 ),
  ( sym: 314; act: -60 ),
  ( sym: 315; act: -60 ),
  ( sym: 318; act: -60 ),
  ( sym: 319; act: -60 ),
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
  ( sym: 321; act: 32 ),
{ 32: }
  ( sym: 274; act: 24 ),
  ( sym: 275; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 277; act: 27 ),
  ( sym: 278; act: 28 ),
  ( sym: 279; act: 29 ),
  ( sym: 280; act: 30 ),
  ( sym: 321; act: 32 ),
{ 33: }
{ 34: }
{ 35: }
  ( sym: 260; act: 64 ),
  ( sym: 286; act: 65 ),
{ 36: }
  ( sym: 262; act: 67 ),
  ( sym: 289; act: 44 ),
  ( sym: 290; act: 45 ),
  ( sym: 291; act: 46 ),
  ( sym: 292; act: 47 ),
  ( sym: 293; act: 48 ),
  ( sym: 294; act: 49 ),
  ( sym: 295; act: 50 ),
  ( sym: 256; act: -18 ),
  ( sym: 271; act: -18 ),
  ( sym: 281; act: -18 ),
  ( sym: 282; act: -18 ),
  ( sym: 283; act: -18 ),
  ( sym: 284; act: -18 ),
  ( sym: 308; act: -18 ),
  ( sym: 313; act: -18 ),
{ 37: }
  ( sym: 260; act: 68 ),
  ( sym: 256; act: -71 ),
  ( sym: 262; act: -71 ),
  ( sym: 271; act: -71 ),
  ( sym: 281; act: -71 ),
  ( sym: 282; act: -71 ),
  ( sym: 283; act: -71 ),
  ( sym: 284; act: -71 ),
  ( sym: 289; act: -71 ),
  ( sym: 290; act: -71 ),
  ( sym: 291; act: -71 ),
  ( sym: 292; act: -71 ),
  ( sym: 293; act: -71 ),
  ( sym: 294; act: -71 ),
  ( sym: 295; act: -71 ),
  ( sym: 308; act: -71 ),
  ( sym: 313; act: -71 ),
{ 38: }
  ( sym: 256; act: 52 ),
  ( sym: 266; act: 53 ),
  ( sym: 271; act: 23 ),
{ 39: }
  ( sym: 262; act: 70 ),
  ( sym: 286; act: 71 ),
  ( sym: 287; act: 72 ),
{ 40: }
  ( sym: 256; act: 52 ),
  ( sym: 266; act: 53 ),
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
{ 41: }
  ( sym: 256; act: 52 ),
  ( sym: 266; act: 53 ),
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
  ( sym: 256; act: 56 ),
  ( sym: 266; act: 57 ),
  ( sym: 260; act: -44 ),
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
{ 43: }
  ( sym: 256; act: 80 ),
  ( sym: 262; act: 81 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 82 ),
  ( sym: 282; act: 83 ),
  ( sym: 283; act: 84 ),
  ( sym: 284; act: 85 ),
  ( sym: 308; act: 86 ),
  ( sym: 313; act: 87 ),
{ 44: }
{ 45: }
{ 46: }
{ 47: }
{ 48: }
{ 49: }
{ 50: }
{ 51: }
  ( sym: 297; act: 88 ),
  ( sym: 256; act: -49 ),
  ( sym: 261; act: -49 ),
  ( sym: 262; act: -49 ),
  ( sym: 263; act: -49 ),
  ( sym: 264; act: -49 ),
  ( sym: 271; act: -49 ),
  ( sym: 281; act: -49 ),
  ( sym: 282; act: -49 ),
  ( sym: 283; act: -49 ),
  ( sym: 284; act: -49 ),
  ( sym: 289; act: -49 ),
  ( sym: 290; act: -49 ),
  ( sym: 291; act: -49 ),
  ( sym: 292; act: -49 ),
  ( sym: 293; act: -49 ),
  ( sym: 294; act: -49 ),
  ( sym: 295; act: -49 ),
  ( sym: 308; act: -49 ),
  ( sym: 313; act: -49 ),
{ 52: }
{ 53: }
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
  ( sym: 321; act: 32 ),
{ 54: }
  ( sym: 297; act: 93 ),
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
{ 55: }
{ 56: }
{ 57: }
  ( sym: 271; act: 23 ),
  ( sym: 267; act: -149 ),
{ 58: }
{ 59: }
{ 60: }
  ( sym: 277; act: 98 ),
  ( sym: 256; act: -62 ),
  ( sym: 259; act: -62 ),
  ( sym: 260; act: -62 ),
  ( sym: 261; act: -62 ),
  ( sym: 262; act: -62 ),
  ( sym: 263; act: -62 ),
  ( sym: 264; act: -62 ),
  ( sym: 265; act: -62 ),
  ( sym: 267; act: -62 ),
  ( sym: 271; act: -62 ),
  ( sym: 281; act: -62 ),
  ( sym: 282; act: -62 ),
  ( sym: 283; act: -62 ),
  ( sym: 284; act: -62 ),
  ( sym: 286; act: -62 ),
  ( sym: 289; act: -62 ),
  ( sym: 290; act: -62 ),
  ( sym: 291; act: -62 ),
  ( sym: 292; act: -62 ),
  ( sym: 293; act: -62 ),
  ( sym: 294; act: -62 ),
  ( sym: 295; act: -62 ),
  ( sym: 296; act: -62 ),
  ( sym: 300; act: -62 ),
  ( sym: 301; act: -62 ),
  ( sym: 302; act: -62 ),
  ( sym: 303; act: -62 ),
  ( sym: 304; act: -62 ),
  ( sym: 305; act: -62 ),
  ( sym: 306; act: -62 ),
  ( sym: 307; act: -62 ),
  ( sym: 308; act: -62 ),
  ( sym: 309; act: -62 ),
  ( sym: 310; act: -62 ),
  ( sym: 311; act: -62 ),
  ( sym: 312; act: -62 ),
  ( sym: 313; act: -62 ),
  ( sym: 314; act: -62 ),
  ( sym: 315; act: -62 ),
  ( sym: 318; act: -62 ),
  ( sym: 319; act: -62 ),
{ 61: }
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
  ( sym: 289; act: 44 ),
  ( sym: 290; act: 45 ),
  ( sym: 291; act: 46 ),
  ( sym: 292; act: 47 ),
  ( sym: 293; act: 48 ),
  ( sym: 294; act: 49 ),
  ( sym: 295; act: 50 ),
  ( sym: 262; act: -18 ),
  ( sym: 271; act: -18 ),
  ( sym: 281; act: -18 ),
  ( sym: 282; act: -18 ),
  ( sym: 283; act: -18 ),
  ( sym: 284; act: -18 ),
  ( sym: 308; act: -18 ),
  ( sym: 313; act: -18 ),
{ 68: }
{ 69: }
  ( sym: 256; act: 52 ),
  ( sym: 266; act: 53 ),
  ( sym: 271; act: 23 ),
  ( sym: 262; act: -42 ),
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
{ 70: }
  ( sym: 271; act: 23 ),
  ( sym: 263; act: -149 ),
{ 71: }
{ 72: }
  ( sym: 262; act: 107 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 108 ),
  ( sym: 273; act: 109 ),
  ( sym: 274; act: 24 ),
  ( sym: 275; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 277; act: 27 ),
  ( sym: 278; act: 28 ),
  ( sym: 279; act: 29 ),
  ( sym: 280; act: 30 ),
  ( sym: 286; act: 110 ),
  ( sym: 308; act: 111 ),
  ( sym: 310; act: 112 ),
  ( sym: 315; act: 113 ),
  ( sym: 321; act: 32 ),
{ 73: }
  ( sym: 297; act: 114 ),
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
  ( sym: 297; act: 115 ),
  ( sym: 256; act: -40 ),
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
{ 75: }
{ 76: }
  ( sym: 313; act: 116 ),
{ 77: }
  ( sym: 262; act: 118 ),
  ( sym: 264; act: 119 ),
  ( sym: 260; act: -75 ),
  ( sym: 261; act: -75 ),
  ( sym: 296; act: -75 ),
{ 78: }
  ( sym: 261; act: 121 ),
  ( sym: 296; act: 122 ),
  ( sym: 260; act: -20 ),
{ 79: }
  ( sym: 259; act: 124 ),
  ( sym: 260; act: -92 ),
  ( sym: 261; act: -92 ),
  ( sym: 262; act: -92 ),
  ( sym: 263; act: -92 ),
  ( sym: 264; act: -92 ),
  ( sym: 296; act: -92 ),
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
  ( sym: 267; act: 130 ),
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
  ( sym: 321; act: 32 ),
  ( sym: 267; act: -54 ),
{ 91: }
  ( sym: 267; act: 132 ),
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
  ( sym: 267; act: 134 ),
{ 95: }
  ( sym: 261; act: 135 ),
  ( sym: 263; act: -148 ),
  ( sym: 267; act: -148 ),
{ 96: }
  ( sym: 267; act: 136 ),
{ 97: }
  ( sym: 285; act: 137 ),
  ( sym: 261; act: -151 ),
  ( sym: 263; act: -151 ),
  ( sym: 267; act: -151 ),
{ 98: }
{ 99: }
  ( sym: 260; act: 138 ),
  ( sym: 261; act: 121 ),
{ 100: }
  ( sym: 262; act: 81 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 82 ),
  ( sym: 282; act: 83 ),
  ( sym: 283; act: 84 ),
  ( sym: 284; act: 85 ),
  ( sym: 308; act: 86 ),
  ( sym: 313; act: 87 ),
{ 101: }
  ( sym: 260; act: 140 ),
{ 102: }
  ( sym: 263; act: 141 ),
{ 103: }
  ( sym: 318; act: 142 ),
  ( sym: 319; act: 143 ),
  ( sym: 263; act: -152 ),
  ( sym: 286; act: -152 ),
{ 104: }
{ 105: }
  ( sym: 286; act: 144 ),
{ 106: }
  ( sym: 262; act: 145 ),
  ( sym: 264; act: 146 ),
  ( sym: 259; act: -130 ),
  ( sym: 260; act: -130 ),
  ( sym: 261; act: -130 ),
  ( sym: 263; act: -130 ),
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
  ( sym: 318; act: -130 ),
  ( sym: 319; act: -130 ),
{ 107: }
  ( sym: 262; act: 107 ),
  ( sym: 268; act: 20 ),
  ( sym: 269; act: 21 ),
  ( sym: 270; act: 22 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 108 ),
  ( sym: 273; act: 109 ),
  ( sym: 274; act: 24 ),
  ( sym: 275; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 277; act: 27 ),
  ( sym: 278; act: 28 ),
  ( sym: 279; act: 29 ),
  ( sym: 280; act: 30 ),
  ( sym: 281; act: 31 ),
  ( sym: 308; act: 111 ),
  ( sym: 310; act: 112 ),
  ( sym: 313; act: 153 ),
  ( sym: 315; act: 113 ),
  ( sym: 321; act: 32 ),
{ 108: }
{ 109: }
{ 110: }
{ 111: }
  ( sym: 262; act: 107 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 108 ),
  ( sym: 273; act: 109 ),
  ( sym: 274; act: 24 ),
  ( sym: 275; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 277; act: 27 ),
  ( sym: 278; act: 28 ),
  ( sym: 279; act: 29 ),
  ( sym: 280; act: 30 ),
  ( sym: 308; act: 111 ),
  ( sym: 310; act: 112 ),
  ( sym: 315; act: 113 ),
  ( sym: 321; act: 32 ),
{ 112: }
  ( sym: 262; act: 107 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 108 ),
  ( sym: 273; act: 109 ),
  ( sym: 274; act: 24 ),
  ( sym: 275; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 277; act: 27 ),
  ( sym: 278; act: 28 ),
  ( sym: 279; act: 29 ),
  ( sym: 280; act: 30 ),
  ( sym: 308; act: 111 ),
  ( sym: 310; act: 112 ),
  ( sym: 315; act: 113 ),
  ( sym: 321; act: 32 ),
{ 113: }
  ( sym: 262; act: 107 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 108 ),
  ( sym: 273; act: 109 ),
  ( sym: 274; act: 24 ),
  ( sym: 275; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 277; act: 27 ),
  ( sym: 278; act: 28 ),
  ( sym: 279; act: 29 ),
  ( sym: 280; act: 30 ),
  ( sym: 308; act: 111 ),
  ( sym: 310; act: 112 ),
  ( sym: 315; act: 113 ),
  ( sym: 321; act: 32 ),
{ 114: }
{ 115: }
{ 116: }
  ( sym: 262; act: 81 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 82 ),
  ( sym: 282; act: 83 ),
  ( sym: 283; act: 84 ),
  ( sym: 284; act: 85 ),
  ( sym: 308; act: 86 ),
  ( sym: 313; act: 87 ),
{ 117: }
{ 118: }
  ( sym: 263; act: 161 ),
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
  ( sym: 280; act: 162 ),
  ( sym: 281; act: 31 ),
  ( sym: 298; act: 163 ),
  ( sym: 321; act: 32 ),
{ 119: }
  ( sym: 262; act: 107 ),
  ( sym: 265; act: 166 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 108 ),
  ( sym: 273; act: 109 ),
  ( sym: 274; act: 24 ),
  ( sym: 275; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 277; act: 27 ),
  ( sym: 278; act: 28 ),
  ( sym: 279; act: 29 ),
  ( sym: 280; act: 30 ),
  ( sym: 308; act: 111 ),
  ( sym: 310; act: 112 ),
  ( sym: 315; act: 113 ),
  ( sym: 321; act: 32 ),
{ 120: }
  ( sym: 260; act: 167 ),
{ 121: }
  ( sym: 262; act: 81 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 82 ),
  ( sym: 282; act: 83 ),
  ( sym: 283; act: 84 ),
  ( sym: 284; act: 85 ),
  ( sym: 308; act: 86 ),
  ( sym: 313; act: 87 ),
{ 122: }
  ( sym: 262; act: 169 ),
{ 123: }
  ( sym: 262; act: 107 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 108 ),
  ( sym: 273; act: 109 ),
  ( sym: 274; act: 24 ),
  ( sym: 275; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 277; act: 27 ),
  ( sym: 278; act: 28 ),
  ( sym: 279; act: 29 ),
  ( sym: 280; act: 30 ),
  ( sym: 308; act: 111 ),
  ( sym: 310; act: 112 ),
  ( sym: 315; act: 113 ),
  ( sym: 321; act: 32 ),
{ 124: }
  ( sym: 262; act: 107 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 108 ),
  ( sym: 273; act: 109 ),
  ( sym: 274; act: 24 ),
  ( sym: 275; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 277; act: 27 ),
  ( sym: 278; act: 28 ),
  ( sym: 279; act: 29 ),
  ( sym: 280; act: 30 ),
  ( sym: 308; act: 111 ),
  ( sym: 310; act: 112 ),
  ( sym: 315; act: 113 ),
  ( sym: 321; act: 32 ),
{ 125: }
  ( sym: 261; act: 172 ),
  ( sym: 260; act: -74 ),
  ( sym: 296; act: -74 ),
{ 126: }
  ( sym: 262; act: 118 ),
  ( sym: 263; act: 173 ),
  ( sym: 264; act: 119 ),
{ 127: }
  ( sym: 262; act: 118 ),
  ( sym: 264; act: 119 ),
  ( sym: 260; act: -86 ),
  ( sym: 261; act: -86 ),
  ( sym: 263; act: -86 ),
  ( sym: 296; act: -86 ),
{ 128: }
  ( sym: 264; act: 119 ),
  ( sym: 260; act: -89 ),
  ( sym: 261; act: -89 ),
  ( sym: 262; act: -89 ),
  ( sym: 263; act: -89 ),
  ( sym: 296; act: -89 ),
{ 129: }
  ( sym: 262; act: 118 ),
  ( sym: 264; act: 119 ),
  ( sym: 260; act: -88 ),
  ( sym: 261; act: -88 ),
  ( sym: 263; act: -88 ),
  ( sym: 296; act: -88 ),
{ 130: }
{ 131: }
{ 132: }
{ 133: }
  ( sym: 260; act: 174 ),
  ( sym: 261; act: 121 ),
{ 134: }
{ 135: }
  ( sym: 271; act: 23 ),
  ( sym: 263; act: -149 ),
  ( sym: 267; act: -149 ),
{ 136: }
{ 137: }
  ( sym: 262; act: 107 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 108 ),
  ( sym: 273; act: 109 ),
  ( sym: 274; act: 24 ),
  ( sym: 275; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 277; act: 27 ),
  ( sym: 278; act: 28 ),
  ( sym: 279; act: 29 ),
  ( sym: 280; act: 30 ),
  ( sym: 308; act: 111 ),
  ( sym: 310; act: 112 ),
  ( sym: 315; act: 113 ),
  ( sym: 321; act: 32 ),
{ 138: }
{ 139: }
  ( sym: 262; act: 118 ),
  ( sym: 263; act: 177 ),
  ( sym: 264; act: 119 ),
{ 140: }
{ 141: }
  ( sym: 287; act: 180 ),
  ( sym: 262; act: -3 ),
{ 142: }
  ( sym: 262; act: 107 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 108 ),
  ( sym: 273; act: 109 ),
  ( sym: 274; act: 24 ),
  ( sym: 275; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 277; act: 27 ),
  ( sym: 278; act: 28 ),
  ( sym: 279; act: 29 ),
  ( sym: 280; act: 30 ),
  ( sym: 308; act: 111 ),
  ( sym: 310; act: 112 ),
  ( sym: 315; act: 113 ),
  ( sym: 321; act: 32 ),
{ 143: }
  ( sym: 262; act: 107 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 108 ),
  ( sym: 273; act: 109 ),
  ( sym: 274; act: 24 ),
  ( sym: 275; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 277; act: 27 ),
  ( sym: 278; act: 28 ),
  ( sym: 279; act: 29 ),
  ( sym: 280; act: 30 ),
  ( sym: 308; act: 111 ),
  ( sym: 310; act: 112 ),
  ( sym: 315; act: 113 ),
  ( sym: 321; act: 32 ),
{ 144: }
{ 145: }
  ( sym: 262; act: 107 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 108 ),
  ( sym: 273; act: 109 ),
  ( sym: 274; act: 24 ),
  ( sym: 275; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 277; act: 27 ),
  ( sym: 278; act: 28 ),
  ( sym: 279; act: 29 ),
  ( sym: 280; act: 30 ),
  ( sym: 308; act: 111 ),
  ( sym: 310; act: 112 ),
  ( sym: 315; act: 113 ),
  ( sym: 321; act: 32 ),
  ( sym: 263; act: -157 ),
{ 146: }
  ( sym: 262; act: 107 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 108 ),
  ( sym: 273; act: 109 ),
  ( sym: 274; act: 24 ),
  ( sym: 275; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 277; act: 27 ),
  ( sym: 278; act: 28 ),
  ( sym: 279; act: 29 ),
  ( sym: 280; act: 30 ),
  ( sym: 308; act: 111 ),
  ( sym: 310; act: 112 ),
  ( sym: 315; act: 113 ),
  ( sym: 321; act: 32 ),
  ( sym: 265; act: -157 ),
{ 147: }
  ( sym: 318; act: 142 ),
  ( sym: 319; act: 143 ),
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
  ( sym: 307; act: -126 ),
  ( sym: 308; act: -126 ),
  ( sym: 309; act: -126 ),
  ( sym: 310; act: -126 ),
  ( sym: 311; act: -126 ),
  ( sym: 312; act: -126 ),
  ( sym: 313; act: -126 ),
  ( sym: 314; act: -126 ),
  ( sym: 315; act: -126 ),
{ 148: }
  ( sym: 263; act: 187 ),
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
  ( sym: 311; act: -109 ),
  ( sym: 312; act: -109 ),
  ( sym: 313; act: -109 ),
  ( sym: 314; act: -109 ),
  ( sym: 315; act: -109 ),
{ 149: }
  ( sym: 300; act: 188 ),
  ( sym: 301; act: 189 ),
  ( sym: 302; act: 190 ),
  ( sym: 303; act: 191 ),
  ( sym: 304; act: 192 ),
  ( sym: 305; act: 193 ),
  ( sym: 306; act: 194 ),
  ( sym: 307; act: 195 ),
  ( sym: 308; act: 196 ),
  ( sym: 309; act: 197 ),
  ( sym: 310; act: 198 ),
  ( sym: 311; act: 199 ),
  ( sym: 312; act: 200 ),
  ( sym: 313; act: 201 ),
  ( sym: 314; act: 202 ),
  ( sym: 315; act: 203 ),
{ 150: }
  ( sym: 263; act: -70 ),
  ( sym: 282; act: -70 ),
  ( sym: 283; act: -70 ),
  ( sym: 284; act: -70 ),
  ( sym: 313; act: -70 ),
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
  ( sym: 314; act: -131 ),
  ( sym: 315; act: -131 ),
  ( sym: 318; act: -131 ),
  ( sym: 319; act: -131 ),
{ 151: }
  ( sym: 263; act: 205 ),
  ( sym: 282; act: 83 ),
  ( sym: 283; act: 84 ),
  ( sym: 284; act: 85 ),
  ( sym: 313; act: 206 ),
{ 152: }
  ( sym: 262; act: 145 ),
  ( sym: 263; act: 207 ),
  ( sym: 264; act: 146 ),
  ( sym: 282; act: -71 ),
  ( sym: 283; act: -71 ),
  ( sym: 284; act: -71 ),
  ( sym: 313; act: -71 ),
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
  ( sym: 314; act: -130 ),
  ( sym: 315; act: -130 ),
  ( sym: 318; act: -130 ),
  ( sym: 319; act: -130 ),
{ 153: }
  ( sym: 262; act: 107 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 108 ),
  ( sym: 273; act: 109 ),
  ( sym: 274; act: 24 ),
  ( sym: 275; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 277; act: 27 ),
  ( sym: 278; act: 28 ),
  ( sym: 279; act: 29 ),
  ( sym: 280; act: 30 ),
  ( sym: 308; act: 111 ),
  ( sym: 310; act: 112 ),
  ( sym: 315; act: 113 ),
  ( sym: 321; act: 32 ),
{ 154: }
  ( sym: 318; act: 142 ),
  ( sym: 319; act: 143 ),
  ( sym: 259; act: -137 ),
  ( sym: 260; act: -137 ),
  ( sym: 261; act: -137 ),
  ( sym: 262; act: -137 ),
  ( sym: 263; act: -137 ),
  ( sym: 264; act: -137 ),
  ( sym: 265; act: -137 ),
  ( sym: 267; act: -137 ),
  ( sym: 286; act: -137 ),
  ( sym: 296; act: -137 ),
  ( sym: 300; act: -137 ),
  ( sym: 301; act: -137 ),
  ( sym: 302; act: -137 ),
  ( sym: 303; act: -137 ),
  ( sym: 304; act: -137 ),
  ( sym: 305; act: -137 ),
  ( sym: 306; act: -137 ),
  ( sym: 307; act: -137 ),
  ( sym: 308; act: -137 ),
  ( sym: 309; act: -137 ),
  ( sym: 310; act: -137 ),
  ( sym: 311; act: -137 ),
  ( sym: 312; act: -137 ),
  ( sym: 313; act: -137 ),
  ( sym: 314; act: -137 ),
  ( sym: 315; act: -137 ),
{ 155: }
  ( sym: 318; act: 142 ),
  ( sym: 319; act: 143 ),
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
  ( sym: 318; act: 142 ),
  ( sym: 319; act: 143 ),
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
{ 157: }
  ( sym: 262; act: 118 ),
  ( sym: 264; act: 119 ),
  ( sym: 260; act: -87 ),
  ( sym: 261; act: -87 ),
  ( sym: 263; act: -87 ),
  ( sym: 296; act: -87 ),
{ 158: }
  ( sym: 261; act: 209 ),
  ( sym: 263; act: -79 ),
{ 159: }
  ( sym: 263; act: 210 ),
{ 160: }
  ( sym: 262; act: 214 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 215 ),
  ( sym: 282; act: 83 ),
  ( sym: 283; act: 84 ),
  ( sym: 284; act: 85 ),
  ( sym: 308; act: 86 ),
  ( sym: 313; act: 216 ),
  ( sym: 261; act: -108 ),
  ( sym: 263; act: -108 ),
  ( sym: 264; act: -108 ),
{ 161: }
{ 162: }
  ( sym: 263; act: 217 ),
  ( sym: 261; act: -67 ),
  ( sym: 262; act: -67 ),
  ( sym: 264; act: -67 ),
  ( sym: 271; act: -67 ),
  ( sym: 281; act: -67 ),
  ( sym: 282; act: -67 ),
  ( sym: 283; act: -67 ),
  ( sym: 284; act: -67 ),
  ( sym: 308; act: -67 ),
  ( sym: 313; act: -67 ),
{ 163: }
{ 164: }
{ 165: }
  ( sym: 265; act: 218 ),
  ( sym: 300; act: 188 ),
  ( sym: 301; act: 189 ),
  ( sym: 302; act: 190 ),
  ( sym: 303; act: 191 ),
  ( sym: 304; act: 192 ),
  ( sym: 305; act: 193 ),
  ( sym: 306; act: 194 ),
  ( sym: 307; act: 195 ),
  ( sym: 308; act: 196 ),
  ( sym: 309; act: 197 ),
  ( sym: 310; act: 198 ),
  ( sym: 311; act: 199 ),
  ( sym: 312; act: 200 ),
  ( sym: 313; act: 201 ),
  ( sym: 314; act: 202 ),
  ( sym: 315; act: 203 ),
{ 166: }
{ 167: }
{ 168: }
  ( sym: 262; act: 118 ),
  ( sym: 264; act: 119 ),
  ( sym: 260; act: -72 ),
  ( sym: 261; act: -72 ),
  ( sym: 296; act: -72 ),
{ 169: }
  ( sym: 271; act: 23 ),
{ 170: }
  ( sym: 300; act: 188 ),
  ( sym: 301; act: 189 ),
  ( sym: 302; act: 190 ),
  ( sym: 303; act: 191 ),
  ( sym: 304; act: 192 ),
  ( sym: 305; act: 193 ),
  ( sym: 306; act: 194 ),
  ( sym: 307; act: 195 ),
  ( sym: 308; act: 196 ),
  ( sym: 309; act: 197 ),
  ( sym: 310; act: 198 ),
  ( sym: 311; act: 199 ),
  ( sym: 312; act: 200 ),
  ( sym: 313; act: 201 ),
  ( sym: 314; act: 202 ),
  ( sym: 315; act: 203 ),
  ( sym: 260; act: -91 ),
  ( sym: 261; act: -91 ),
  ( sym: 262; act: -91 ),
  ( sym: 263; act: -91 ),
  ( sym: 264; act: -91 ),
  ( sym: 296; act: -91 ),
{ 171: }
  ( sym: 300; act: 188 ),
  ( sym: 301; act: 189 ),
  ( sym: 302; act: 190 ),
  ( sym: 303; act: 191 ),
  ( sym: 304; act: 192 ),
  ( sym: 305; act: 193 ),
  ( sym: 306; act: 194 ),
  ( sym: 307; act: 195 ),
  ( sym: 308; act: 196 ),
  ( sym: 309; act: 197 ),
  ( sym: 310; act: 198 ),
  ( sym: 311; act: 199 ),
  ( sym: 312; act: 200 ),
  ( sym: 313; act: 201 ),
  ( sym: 314; act: 202 ),
  ( sym: 315; act: 203 ),
  ( sym: 260; act: -90 ),
  ( sym: 261; act: -90 ),
  ( sym: 262; act: -90 ),
  ( sym: 263; act: -90 ),
  ( sym: 264; act: -90 ),
  ( sym: 296; act: -90 ),
{ 172: }
  ( sym: 256; act: 80 ),
  ( sym: 262; act: 81 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 82 ),
  ( sym: 282; act: 83 ),
  ( sym: 283; act: 84 ),
  ( sym: 284; act: 85 ),
  ( sym: 308; act: 86 ),
  ( sym: 313; act: 87 ),
{ 173: }
{ 174: }
{ 175: }
{ 176: }
  ( sym: 300; act: 188 ),
  ( sym: 301; act: 189 ),
  ( sym: 302; act: 190 ),
  ( sym: 303; act: 191 ),
  ( sym: 304; act: 192 ),
  ( sym: 305; act: 193 ),
  ( sym: 306; act: 194 ),
  ( sym: 307; act: 195 ),
  ( sym: 308; act: 196 ),
  ( sym: 309; act: 197 ),
  ( sym: 310; act: 198 ),
  ( sym: 311; act: 199 ),
  ( sym: 312; act: 200 ),
  ( sym: 313; act: 201 ),
  ( sym: 314; act: 202 ),
  ( sym: 315; act: 203 ),
  ( sym: 261; act: -150 ),
  ( sym: 263; act: -150 ),
  ( sym: 267; act: -150 ),
{ 177: }
  ( sym: 287; act: 222 ),
  ( sym: 262; act: -3 ),
{ 178: }
  ( sym: 286; act: 223 ),
{ 179: }
  ( sym: 262; act: 224 ),
{ 180: }
  ( sym: 262; act: 107 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 108 ),
  ( sym: 273; act: 109 ),
  ( sym: 274; act: 24 ),
  ( sym: 275; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 277; act: 27 ),
  ( sym: 278; act: 28 ),
  ( sym: 279; act: 29 ),
  ( sym: 280; act: 30 ),
  ( sym: 308; act: 111 ),
  ( sym: 310; act: 112 ),
  ( sym: 315; act: 113 ),
  ( sym: 321; act: 32 ),
{ 181: }
{ 182: }
{ 183: }
  ( sym: 261; act: 226 ),
  ( sym: 263; act: -156 ),
  ( sym: 265; act: -156 ),
{ 184: }
  ( sym: 263; act: 227 ),
{ 185: }
  ( sym: 300; act: 188 ),
  ( sym: 301; act: 189 ),
  ( sym: 302; act: 190 ),
  ( sym: 303; act: 191 ),
  ( sym: 304; act: 192 ),
  ( sym: 305; act: 193 ),
  ( sym: 306; act: 194 ),
  ( sym: 307; act: 195 ),
  ( sym: 308; act: 196 ),
  ( sym: 309; act: 197 ),
  ( sym: 310; act: 198 ),
  ( sym: 311; act: 199 ),
  ( sym: 312; act: 200 ),
  ( sym: 313; act: 201 ),
  ( sym: 314; act: 202 ),
  ( sym: 315; act: 203 ),
  ( sym: 261; act: -158 ),
  ( sym: 263; act: -158 ),
  ( sym: 265; act: -158 ),
{ 186: }
  ( sym: 265; act: 228 ),
{ 187: }
{ 188: }
  ( sym: 262; act: 107 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 108 ),
  ( sym: 273; act: 109 ),
  ( sym: 274; act: 24 ),
  ( sym: 275; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 277; act: 27 ),
  ( sym: 278; act: 28 ),
  ( sym: 279; act: 29 ),
  ( sym: 280; act: 30 ),
  ( sym: 308; act: 111 ),
  ( sym: 310; act: 112 ),
  ( sym: 315; act: 113 ),
  ( sym: 321; act: 32 ),
{ 189: }
  ( sym: 262; act: 107 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 108 ),
  ( sym: 273; act: 109 ),
  ( sym: 274; act: 24 ),
  ( sym: 275; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 277; act: 27 ),
  ( sym: 278; act: 28 ),
  ( sym: 279; act: 29 ),
  ( sym: 280; act: 30 ),
  ( sym: 308; act: 111 ),
  ( sym: 310; act: 112 ),
  ( sym: 315; act: 113 ),
  ( sym: 321; act: 32 ),
{ 190: }
  ( sym: 262; act: 107 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 108 ),
  ( sym: 273; act: 109 ),
  ( sym: 274; act: 24 ),
  ( sym: 275; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 277; act: 27 ),
  ( sym: 278; act: 28 ),
  ( sym: 279; act: 29 ),
  ( sym: 280; act: 30 ),
  ( sym: 308; act: 111 ),
  ( sym: 310; act: 112 ),
  ( sym: 315; act: 113 ),
  ( sym: 321; act: 32 ),
{ 191: }
  ( sym: 262; act: 107 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 108 ),
  ( sym: 273; act: 109 ),
  ( sym: 274; act: 24 ),
  ( sym: 275; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 277; act: 27 ),
  ( sym: 278; act: 28 ),
  ( sym: 279; act: 29 ),
  ( sym: 280; act: 30 ),
  ( sym: 308; act: 111 ),
  ( sym: 310; act: 112 ),
  ( sym: 315; act: 113 ),
  ( sym: 321; act: 32 ),
{ 192: }
  ( sym: 262; act: 107 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 108 ),
  ( sym: 273; act: 109 ),
  ( sym: 274; act: 24 ),
  ( sym: 275; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 277; act: 27 ),
  ( sym: 278; act: 28 ),
  ( sym: 279; act: 29 ),
  ( sym: 280; act: 30 ),
  ( sym: 308; act: 111 ),
  ( sym: 310; act: 112 ),
  ( sym: 315; act: 113 ),
  ( sym: 321; act: 32 ),
{ 193: }
  ( sym: 262; act: 107 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 108 ),
  ( sym: 273; act: 109 ),
  ( sym: 274; act: 24 ),
  ( sym: 275; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 277; act: 27 ),
  ( sym: 278; act: 28 ),
  ( sym: 279; act: 29 ),
  ( sym: 280; act: 30 ),
  ( sym: 308; act: 111 ),
  ( sym: 310; act: 112 ),
  ( sym: 315; act: 113 ),
  ( sym: 321; act: 32 ),
{ 194: }
  ( sym: 262; act: 107 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 108 ),
  ( sym: 273; act: 109 ),
  ( sym: 274; act: 24 ),
  ( sym: 275; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 277; act: 27 ),
  ( sym: 278; act: 28 ),
  ( sym: 279; act: 29 ),
  ( sym: 280; act: 30 ),
  ( sym: 308; act: 111 ),
  ( sym: 310; act: 112 ),
  ( sym: 315; act: 113 ),
  ( sym: 321; act: 32 ),
{ 195: }
  ( sym: 262; act: 107 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 108 ),
  ( sym: 273; act: 109 ),
  ( sym: 274; act: 24 ),
  ( sym: 275; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 277; act: 27 ),
  ( sym: 278; act: 28 ),
  ( sym: 279; act: 29 ),
  ( sym: 280; act: 30 ),
  ( sym: 308; act: 111 ),
  ( sym: 310; act: 112 ),
  ( sym: 315; act: 113 ),
  ( sym: 321; act: 32 ),
{ 196: }
  ( sym: 262; act: 107 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 108 ),
  ( sym: 273; act: 109 ),
  ( sym: 274; act: 24 ),
  ( sym: 275; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 277; act: 27 ),
  ( sym: 278; act: 28 ),
  ( sym: 279; act: 29 ),
  ( sym: 280; act: 30 ),
  ( sym: 308; act: 111 ),
  ( sym: 310; act: 112 ),
  ( sym: 315; act: 113 ),
  ( sym: 321; act: 32 ),
{ 197: }
  ( sym: 262; act: 107 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 108 ),
  ( sym: 273; act: 109 ),
  ( sym: 274; act: 24 ),
  ( sym: 275; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 277; act: 27 ),
  ( sym: 278; act: 28 ),
  ( sym: 279; act: 29 ),
  ( sym: 280; act: 30 ),
  ( sym: 308; act: 111 ),
  ( sym: 310; act: 112 ),
  ( sym: 315; act: 113 ),
  ( sym: 321; act: 32 ),
{ 198: }
  ( sym: 262; act: 107 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 108 ),
  ( sym: 273; act: 109 ),
  ( sym: 274; act: 24 ),
  ( sym: 275; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 277; act: 27 ),
  ( sym: 278; act: 28 ),
  ( sym: 279; act: 29 ),
  ( sym: 280; act: 30 ),
  ( sym: 308; act: 111 ),
  ( sym: 310; act: 112 ),
  ( sym: 315; act: 113 ),
  ( sym: 321; act: 32 ),
{ 199: }
  ( sym: 262; act: 107 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 108 ),
  ( sym: 273; act: 109 ),
  ( sym: 274; act: 24 ),
  ( sym: 275; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 277; act: 27 ),
  ( sym: 278; act: 28 ),
  ( sym: 279; act: 29 ),
  ( sym: 280; act: 30 ),
  ( sym: 308; act: 111 ),
  ( sym: 310; act: 112 ),
  ( sym: 315; act: 113 ),
  ( sym: 321; act: 32 ),
{ 200: }
  ( sym: 262; act: 107 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 108 ),
  ( sym: 273; act: 109 ),
  ( sym: 274; act: 24 ),
  ( sym: 275; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 277; act: 27 ),
  ( sym: 278; act: 28 ),
  ( sym: 279; act: 29 ),
  ( sym: 280; act: 30 ),
  ( sym: 308; act: 111 ),
  ( sym: 310; act: 112 ),
  ( sym: 315; act: 113 ),
  ( sym: 321; act: 32 ),
{ 201: }
  ( sym: 262; act: 107 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 108 ),
  ( sym: 273; act: 109 ),
  ( sym: 274; act: 24 ),
  ( sym: 275; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 277; act: 27 ),
  ( sym: 278; act: 28 ),
  ( sym: 279; act: 29 ),
  ( sym: 280; act: 30 ),
  ( sym: 308; act: 111 ),
  ( sym: 310; act: 112 ),
  ( sym: 315; act: 113 ),
  ( sym: 321; act: 32 ),
{ 202: }
  ( sym: 262; act: 107 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 108 ),
  ( sym: 273; act: 109 ),
  ( sym: 274; act: 24 ),
  ( sym: 275; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 277; act: 27 ),
  ( sym: 278; act: 28 ),
  ( sym: 279; act: 29 ),
  ( sym: 280; act: 30 ),
  ( sym: 308; act: 111 ),
  ( sym: 310; act: 112 ),
  ( sym: 315; act: 113 ),
  ( sym: 321; act: 32 ),
{ 203: }
  ( sym: 262; act: 107 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 108 ),
  ( sym: 273; act: 109 ),
  ( sym: 274; act: 24 ),
  ( sym: 275; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 277; act: 27 ),
  ( sym: 278; act: 28 ),
  ( sym: 279; act: 29 ),
  ( sym: 280; act: 30 ),
  ( sym: 308; act: 111 ),
  ( sym: 310; act: 112 ),
  ( sym: 315; act: 113 ),
  ( sym: 321; act: 32 ),
{ 204: }
  ( sym: 313; act: 246 ),
{ 205: }
  ( sym: 262; act: 107 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 108 ),
  ( sym: 273; act: 109 ),
  ( sym: 274; act: 24 ),
  ( sym: 275; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 277; act: 27 ),
  ( sym: 278; act: 28 ),
  ( sym: 279; act: 29 ),
  ( sym: 280; act: 30 ),
  ( sym: 308; act: 111 ),
  ( sym: 310; act: 112 ),
  ( sym: 315; act: 113 ),
  ( sym: 321; act: 32 ),
{ 206: }
  ( sym: 263; act: 248 ),
{ 207: }
  ( sym: 262; act: 107 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 108 ),
  ( sym: 273; act: 109 ),
  ( sym: 274; act: 24 ),
  ( sym: 275; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 277; act: 27 ),
  ( sym: 278; act: 28 ),
  ( sym: 279; act: 29 ),
  ( sym: 280; act: 30 ),
  ( sym: 308; act: 111 ),
  ( sym: 310; act: 112 ),
  ( sym: 315; act: 113 ),
  ( sym: 321; act: 32 ),
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
  ( sym: 309; act: -129 ),
  ( sym: 311; act: -129 ),
  ( sym: 312; act: -129 ),
  ( sym: 313; act: -129 ),
  ( sym: 314; act: -129 ),
  ( sym: 318; act: -129 ),
  ( sym: 319; act: -129 ),
{ 208: }
  ( sym: 263; act: 251 ),
  ( sym: 318; act: 142 ),
  ( sym: 319; act: 143 ),
{ 209: }
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
  ( sym: 298; act: 163 ),
  ( sym: 321; act: 32 ),
  ( sym: 263; act: -82 ),
{ 210: }
{ 211: }
  ( sym: 313; act: 253 ),
{ 212: }
  ( sym: 262; act: 255 ),
  ( sym: 264; act: 256 ),
  ( sym: 261; act: -78 ),
  ( sym: 263; act: -78 ),
{ 213: }
  ( sym: 262; act: 118 ),
  ( sym: 264; act: 257 ),
  ( sym: 261; act: -76 ),
  ( sym: 263; act: -76 ),
{ 214: }
  ( sym: 262; act: 214 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 215 ),
  ( sym: 282; act: 83 ),
  ( sym: 283; act: 84 ),
  ( sym: 284; act: 85 ),
  ( sym: 308; act: 86 ),
  ( sym: 313; act: 260 ),
  ( sym: 263; act: -108 ),
  ( sym: 264; act: -108 ),
{ 215: }
  ( sym: 262; act: 214 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 215 ),
  ( sym: 282; act: 83 ),
  ( sym: 283; act: 84 ),
  ( sym: 284; act: 85 ),
  ( sym: 308; act: 86 ),
  ( sym: 313; act: 260 ),
  ( sym: 261; act: -108 ),
  ( sym: 263; act: -108 ),
  ( sym: 264; act: -108 ),
{ 216: }
  ( sym: 262; act: 214 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 215 ),
  ( sym: 282; act: 83 ),
  ( sym: 283; act: 84 ),
  ( sym: 284; act: 85 ),
  ( sym: 308; act: 86 ),
  ( sym: 313; act: 260 ),
  ( sym: 261; act: -108 ),
  ( sym: 263; act: -108 ),
  ( sym: 264; act: -108 ),
{ 217: }
{ 218: }
{ 219: }
  ( sym: 263; act: 265 ),
{ 220: }
{ 221: }
  ( sym: 262; act: 266 ),
{ 222: }
{ 223: }
{ 224: }
  ( sym: 262; act: 107 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 108 ),
  ( sym: 273; act: 109 ),
  ( sym: 274; act: 24 ),
  ( sym: 275; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 277; act: 27 ),
  ( sym: 278; act: 28 ),
  ( sym: 279; act: 29 ),
  ( sym: 280; act: 30 ),
  ( sym: 308; act: 111 ),
  ( sym: 310; act: 112 ),
  ( sym: 315; act: 113 ),
  ( sym: 321; act: 32 ),
{ 225: }
{ 226: }
  ( sym: 262; act: 107 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 108 ),
  ( sym: 273; act: 109 ),
  ( sym: 274; act: 24 ),
  ( sym: 275; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 277; act: 27 ),
  ( sym: 278; act: 28 ),
  ( sym: 279; act: 29 ),
  ( sym: 280; act: 30 ),
  ( sym: 308; act: 111 ),
  ( sym: 310; act: 112 ),
  ( sym: 315; act: 113 ),
  ( sym: 321; act: 32 ),
  ( sym: 263; act: -157 ),
  ( sym: 265; act: -157 ),
{ 227: }
{ 228: }
{ 229: }
  ( sym: 306; act: 194 ),
  ( sym: 307; act: 195 ),
  ( sym: 308; act: 196 ),
  ( sym: 309; act: 197 ),
  ( sym: 310; act: 198 ),
  ( sym: 311; act: 199 ),
  ( sym: 312; act: 200 ),
  ( sym: 313; act: 201 ),
  ( sym: 314; act: 202 ),
  ( sym: 315; act: 203 ),
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
{ 230: }
  ( sym: 306; act: 194 ),
  ( sym: 307; act: 195 ),
  ( sym: 308; act: 196 ),
  ( sym: 309; act: 197 ),
  ( sym: 310; act: 198 ),
  ( sym: 311; act: 199 ),
  ( sym: 312; act: 200 ),
  ( sym: 313; act: 201 ),
  ( sym: 314; act: 202 ),
  ( sym: 315; act: 203 ),
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
{ 231: }
  ( sym: 306; act: 194 ),
  ( sym: 307; act: 195 ),
  ( sym: 308; act: 196 ),
  ( sym: 309; act: 197 ),
  ( sym: 310; act: 198 ),
  ( sym: 311; act: 199 ),
  ( sym: 312; act: 200 ),
  ( sym: 313; act: 201 ),
  ( sym: 314; act: 202 ),
  ( sym: 315; act: 203 ),
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
{ 232: }
  ( sym: 306; act: 194 ),
  ( sym: 307; act: 195 ),
  ( sym: 308; act: 196 ),
  ( sym: 309; act: 197 ),
  ( sym: 310; act: 198 ),
  ( sym: 311; act: 199 ),
  ( sym: 312; act: 200 ),
  ( sym: 313; act: 201 ),
  ( sym: 314; act: 202 ),
  ( sym: 315; act: 203 ),
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
{ 233: }
  ( sym: 306; act: 194 ),
  ( sym: 307; act: 195 ),
  ( sym: 308; act: 196 ),
  ( sym: 309; act: 197 ),
  ( sym: 310; act: 198 ),
  ( sym: 311; act: 199 ),
  ( sym: 312; act: 200 ),
  ( sym: 313; act: 201 ),
  ( sym: 314; act: 202 ),
  ( sym: 315; act: 203 ),
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
{ 234: }
  ( sym: 306; act: 194 ),
  ( sym: 307; act: 195 ),
  ( sym: 308; act: 196 ),
  ( sym: 309; act: 197 ),
  ( sym: 310; act: 198 ),
  ( sym: 311; act: 199 ),
  ( sym: 312; act: 200 ),
  ( sym: 313; act: 201 ),
  ( sym: 314; act: 202 ),
  ( sym: 315; act: 203 ),
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
  ( sym: 318; act: -115 ),
  ( sym: 319; act: -115 ),
{ 235: }
{ 236: }
  ( sym: 259; act: 269 ),
  ( sym: 300; act: 188 ),
  ( sym: 301; act: 189 ),
  ( sym: 302; act: 190 ),
  ( sym: 303; act: 191 ),
  ( sym: 304; act: 192 ),
  ( sym: 305; act: 193 ),
  ( sym: 306; act: 194 ),
  ( sym: 307; act: 195 ),
  ( sym: 308; act: 196 ),
  ( sym: 309; act: 197 ),
  ( sym: 310; act: 198 ),
  ( sym: 311; act: 199 ),
  ( sym: 312; act: 200 ),
  ( sym: 313; act: 201 ),
  ( sym: 314; act: 202 ),
  ( sym: 315; act: 203 ),
{ 237: }
  ( sym: 308; act: 196 ),
  ( sym: 309; act: 197 ),
  ( sym: 310; act: 198 ),
  ( sym: 311; act: 199 ),
  ( sym: 312; act: 200 ),
  ( sym: 313; act: 201 ),
  ( sym: 314; act: 202 ),
  ( sym: 315; act: 203 ),
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
  ( sym: 318; act: -120 ),
  ( sym: 319; act: -120 ),
{ 238: }
  ( sym: 309; act: 197 ),
  ( sym: 310; act: 198 ),
  ( sym: 311; act: 199 ),
  ( sym: 312; act: 200 ),
  ( sym: 313; act: 201 ),
  ( sym: 314; act: 202 ),
  ( sym: 315; act: 203 ),
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
  ( sym: 318; act: -121 ),
  ( sym: 319; act: -121 ),
{ 239: }
  ( sym: 311; act: 199 ),
  ( sym: 312; act: 200 ),
  ( sym: 313; act: 201 ),
  ( sym: 314; act: 202 ),
  ( sym: 315; act: 203 ),
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
{ 240: }
  ( sym: 311; act: 199 ),
  ( sym: 312; act: 200 ),
  ( sym: 313; act: 201 ),
  ( sym: 314; act: 202 ),
  ( sym: 315; act: 203 ),
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
  ( sym: 318; act: -117 ),
  ( sym: 319; act: -117 ),
{ 241: }
  ( sym: 313; act: 201 ),
  ( sym: 314; act: 202 ),
  ( sym: 315; act: 203 ),
  ( sym: 259; act: -124 ),
  ( sym: 260; act: -124 ),
  ( sym: 261; act: -124 ),
  ( sym: 262; act: -124 ),
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
  ( sym: 318; act: -124 ),
  ( sym: 319; act: -124 ),
{ 242: }
  ( sym: 313; act: 201 ),
  ( sym: 314; act: 202 ),
  ( sym: 315; act: 203 ),
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
{ 243: }
  ( sym: 315; act: 203 ),
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
{ 244: }
  ( sym: 315; act: 203 ),
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
  ( sym: 313; act: -119 ),
  ( sym: 314; act: -119 ),
  ( sym: 318; act: -119 ),
  ( sym: 319; act: -119 ),
{ 245: }
  ( sym: 315; act: 203 ),
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
  ( sym: 318; act: -122 ),
  ( sym: 319; act: -122 ),
{ 246: }
  ( sym: 263; act: 270 ),
{ 247: }
  ( sym: 318; act: 142 ),
  ( sym: 319; act: 143 ),
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
  ( sym: 315; act: -140 ),
{ 248: }
  ( sym: 262; act: 107 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 108 ),
  ( sym: 273; act: 109 ),
  ( sym: 274; act: 24 ),
  ( sym: 275; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 277; act: 27 ),
  ( sym: 278; act: 28 ),
  ( sym: 279; act: 29 ),
  ( sym: 280; act: 30 ),
  ( sym: 308; act: 111 ),
  ( sym: 310; act: 112 ),
  ( sym: 315; act: 113 ),
  ( sym: 321; act: 32 ),
{ 249: }
{ 250: }
  ( sym: 318; act: 142 ),
  ( sym: 319; act: 143 ),
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
{ 251: }
  ( sym: 287; act: 222 ),
  ( sym: 262; act: -3 ),
{ 252: }
{ 253: }
  ( sym: 262; act: 214 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 215 ),
  ( sym: 282; act: 83 ),
  ( sym: 283; act: 84 ),
  ( sym: 284; act: 85 ),
  ( sym: 308; act: 86 ),
  ( sym: 313; act: 260 ),
  ( sym: 261; act: -108 ),
  ( sym: 263; act: -108 ),
  ( sym: 264; act: -108 ),
{ 254: }
{ 255: }
  ( sym: 263; act: 161 ),
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
  ( sym: 280; act: 162 ),
  ( sym: 281; act: 31 ),
  ( sym: 298; act: 163 ),
  ( sym: 321; act: 32 ),
{ 256: }
  ( sym: 262; act: 107 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 108 ),
  ( sym: 273; act: 109 ),
  ( sym: 274; act: 24 ),
  ( sym: 275; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 277; act: 27 ),
  ( sym: 278; act: 28 ),
  ( sym: 279; act: 29 ),
  ( sym: 280; act: 30 ),
  ( sym: 308; act: 111 ),
  ( sym: 310; act: 112 ),
  ( sym: 315; act: 113 ),
  ( sym: 321; act: 32 ),
{ 257: }
  ( sym: 262; act: 107 ),
  ( sym: 265; act: 277 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 108 ),
  ( sym: 273; act: 109 ),
  ( sym: 274; act: 24 ),
  ( sym: 275; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 277; act: 27 ),
  ( sym: 278; act: 28 ),
  ( sym: 279; act: 29 ),
  ( sym: 280; act: 30 ),
  ( sym: 308; act: 111 ),
  ( sym: 310; act: 112 ),
  ( sym: 315; act: 113 ),
  ( sym: 321; act: 32 ),
{ 258: }
  ( sym: 262; act: 255 ),
  ( sym: 263; act: 278 ),
  ( sym: 264; act: 256 ),
{ 259: }
  ( sym: 262; act: 118 ),
  ( sym: 263; act: 173 ),
  ( sym: 264; act: 257 ),
{ 260: }
  ( sym: 262; act: 214 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 215 ),
  ( sym: 282; act: 83 ),
  ( sym: 283; act: 84 ),
  ( sym: 284; act: 85 ),
  ( sym: 308; act: 86 ),
  ( sym: 313; act: 260 ),
  ( sym: 261; act: -108 ),
  ( sym: 263; act: -108 ),
  ( sym: 264; act: -108 ),
{ 261: }
  ( sym: 262; act: 255 ),
  ( sym: 264; act: 256 ),
  ( sym: 261; act: -100 ),
  ( sym: 263; act: -100 ),
{ 262: }
  ( sym: 262; act: 118 ),
  ( sym: 264; act: 257 ),
  ( sym: 261; act: -86 ),
  ( sym: 263; act: -86 ),
{ 263: }
  ( sym: 264; act: 256 ),
  ( sym: 261; act: -102 ),
  ( sym: 262; act: -102 ),
  ( sym: 263; act: -102 ),
{ 264: }
  ( sym: 262; act: 118 ),
  ( sym: 264; act: 257 ),
  ( sym: 261; act: -77 ),
  ( sym: 263; act: -77 ),
{ 265: }
{ 266: }
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
  ( sym: 298; act: 163 ),
  ( sym: 321; act: 32 ),
  ( sym: 263; act: -82 ),
{ 267: }
  ( sym: 263; act: 281 ),
{ 268: }
{ 269: }
  ( sym: 262; act: 107 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 108 ),
  ( sym: 273; act: 109 ),
  ( sym: 274; act: 24 ),
  ( sym: 275; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 277; act: 27 ),
  ( sym: 278; act: 28 ),
  ( sym: 279; act: 29 ),
  ( sym: 280; act: 30 ),
  ( sym: 308; act: 111 ),
  ( sym: 310; act: 112 ),
  ( sym: 315; act: 113 ),
  ( sym: 321; act: 32 ),
{ 270: }
  ( sym: 262; act: 107 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 108 ),
  ( sym: 273; act: 109 ),
  ( sym: 274; act: 24 ),
  ( sym: 275; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 277; act: 27 ),
  ( sym: 278; act: 28 ),
  ( sym: 279; act: 29 ),
  ( sym: 280; act: 30 ),
  ( sym: 308; act: 111 ),
  ( sym: 310; act: 112 ),
  ( sym: 315; act: 113 ),
  ( sym: 321; act: 32 ),
{ 271: }
  ( sym: 318; act: 142 ),
  ( sym: 319; act: 143 ),
  ( sym: 259; act: -141 ),
  ( sym: 260; act: -141 ),
  ( sym: 261; act: -141 ),
  ( sym: 262; act: -141 ),
  ( sym: 263; act: -141 ),
  ( sym: 264; act: -141 ),
  ( sym: 265; act: -141 ),
  ( sym: 267; act: -141 ),
  ( sym: 286; act: -141 ),
  ( sym: 296; act: -141 ),
  ( sym: 300; act: -141 ),
  ( sym: 301; act: -141 ),
  ( sym: 302; act: -141 ),
  ( sym: 303; act: -141 ),
  ( sym: 304; act: -141 ),
  ( sym: 305; act: -141 ),
  ( sym: 306; act: -141 ),
  ( sym: 307; act: -141 ),
  ( sym: 308; act: -141 ),
  ( sym: 309; act: -141 ),
  ( sym: 310; act: -141 ),
  ( sym: 311; act: -141 ),
  ( sym: 312; act: -141 ),
  ( sym: 313; act: -141 ),
  ( sym: 314; act: -141 ),
  ( sym: 315; act: -141 ),
{ 272: }
  ( sym: 262; act: 284 ),
{ 273: }
  ( sym: 262; act: 255 ),
  ( sym: 264; act: 256 ),
  ( sym: 261; act: -101 ),
  ( sym: 263; act: -101 ),
{ 274: }
  ( sym: 262; act: 118 ),
  ( sym: 264; act: 257 ),
  ( sym: 261; act: -87 ),
  ( sym: 263; act: -87 ),
{ 275: }
  ( sym: 263; act: 285 ),
{ 276: }
  ( sym: 265; act: 286 ),
  ( sym: 300; act: 188 ),
  ( sym: 301; act: 189 ),
  ( sym: 302; act: 190 ),
  ( sym: 303; act: 191 ),
  ( sym: 304; act: 192 ),
  ( sym: 305; act: 193 ),
  ( sym: 306; act: 194 ),
  ( sym: 307; act: 195 ),
  ( sym: 308; act: 196 ),
  ( sym: 309; act: 197 ),
  ( sym: 310; act: 198 ),
  ( sym: 311; act: 199 ),
  ( sym: 312; act: 200 ),
  ( sym: 313; act: 201 ),
  ( sym: 314; act: 202 ),
  ( sym: 315; act: 203 ),
{ 277: }
{ 278: }
{ 279: }
  ( sym: 262; act: 118 ),
  ( sym: 264; act: 257 ),
  ( sym: 261; act: -88 ),
  ( sym: 263; act: -88 ),
{ 280: }
  ( sym: 263; act: 287 ),
{ 281: }
{ 282: }
  ( sym: 307; act: 195 ),
  ( sym: 308; act: 196 ),
  ( sym: 309; act: 197 ),
  ( sym: 310; act: 198 ),
  ( sym: 311; act: 199 ),
  ( sym: 312; act: 200 ),
  ( sym: 313; act: 201 ),
  ( sym: 314; act: 202 ),
  ( sym: 315; act: 203 ),
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
  ( sym: 318; act: -127 ),
  ( sym: 319; act: -127 ),
{ 283: }
  ( sym: 318; act: 142 ),
  ( sym: 319; act: 143 ),
  ( sym: 259; act: -142 ),
  ( sym: 260; act: -142 ),
  ( sym: 261; act: -142 ),
  ( sym: 262; act: -142 ),
  ( sym: 263; act: -142 ),
  ( sym: 264; act: -142 ),
  ( sym: 265; act: -142 ),
  ( sym: 267; act: -142 ),
  ( sym: 286; act: -142 ),
  ( sym: 296; act: -142 ),
  ( sym: 300; act: -142 ),
  ( sym: 301; act: -142 ),
  ( sym: 302; act: -142 ),
  ( sym: 303; act: -142 ),
  ( sym: 304; act: -142 ),
  ( sym: 305; act: -142 ),
  ( sym: 306; act: -142 ),
  ( sym: 307; act: -142 ),
  ( sym: 308; act: -142 ),
  ( sym: 309; act: -142 ),
  ( sym: 310; act: -142 ),
  ( sym: 311; act: -142 ),
  ( sym: 312; act: -142 ),
  ( sym: 313; act: -142 ),
  ( sym: 314; act: -142 ),
  ( sym: 315; act: -142 ),
{ 284: }
  ( sym: 262; act: 107 ),
  ( sym: 271; act: 23 ),
  ( sym: 272; act: 108 ),
  ( sym: 273; act: 109 ),
  ( sym: 274; act: 24 ),
  ( sym: 275; act: 25 ),
  ( sym: 276; act: 26 ),
  ( sym: 277; act: 27 ),
  ( sym: 278; act: 28 ),
  ( sym: 279; act: 29 ),
  ( sym: 280; act: 30 ),
  ( sym: 308; act: 111 ),
  ( sym: 310; act: 112 ),
  ( sym: 315; act: 113 ),
  ( sym: 321; act: 32 ),
  ( sym: 263; act: -157 ),
{ 285: }
{ 286: }
{ 287: }
  ( sym: 260; act: 289 ),
{ 288: }
  ( sym: 263; act: 290 )
{ 289: }
{ 290: }
);

yyg : array [1..yyngotos] of YYARec = (
{ 0: }
  ( sym: -14; act: 1 ),
  ( sym: -8; act: 2 ),
  ( sym: -7; act: 3 ),
  ( sym: -6; act: 4 ),
  ( sym: -3; act: 5 ),
  ( sym: -2; act: 6 ),
{ 1: }
{ 2: }
  ( sym: -25; act: 15 ),
  ( sym: -23; act: 16 ),
  ( sym: -14; act: 17 ),
  ( sym: -12; act: 18 ),
  ( sym: -11; act: 19 ),
{ 3: }
{ 4: }
{ 5: }
  ( sym: -14; act: 1 ),
  ( sym: -8; act: 2 ),
  ( sym: -7; act: 33 ),
  ( sym: -6; act: 34 ),
{ 6: }
{ 7: }
  ( sym: -5; act: 35 ),
{ 8: }
  ( sym: -25; act: 15 ),
  ( sym: -23; act: 16 ),
  ( sym: -14; act: 17 ),
  ( sym: -12; act: 36 ),
  ( sym: -11; act: 37 ),
{ 9: }
  ( sym: -11; act: 39 ),
{ 10: }
  ( sym: -11; act: 40 ),
{ 11: }
  ( sym: -11; act: 41 ),
{ 12: }
  ( sym: -11; act: 42 ),
{ 13: }
{ 14: }
{ 15: }
{ 16: }
{ 17: }
{ 18: }
  ( sym: -9; act: 43 ),
{ 19: }
{ 20: }
  ( sym: -20; act: 51 ),
  ( sym: -11; act: 40 ),
{ 21: }
  ( sym: -20; act: 54 ),
  ( sym: -11; act: 41 ),
{ 22: }
  ( sym: -22; act: 55 ),
  ( sym: -11; act: 42 ),
{ 23: }
{ 24: }
{ 25: }
  ( sym: -25; act: 59 ),
{ 26: }
{ 27: }
{ 28: }
{ 29: }
{ 30: }
{ 31: }
  ( sym: -25; act: 15 ),
  ( sym: -23; act: 16 ),
  ( sym: -14; act: 17 ),
  ( sym: -12; act: 62 ),
  ( sym: -11; act: 19 ),
{ 32: }
  ( sym: -25; act: 63 ),
{ 33: }
{ 34: }
{ 35: }
{ 36: }
  ( sym: -9; act: 66 ),
{ 37: }
{ 38: }
  ( sym: -20; act: 51 ),
  ( sym: -11; act: 69 ),
{ 39: }
{ 40: }
  ( sym: -20; act: 73 ),
{ 41: }
  ( sym: -20; act: 74 ),
{ 42: }
  ( sym: -22; act: 75 ),
{ 43: }
  ( sym: -28; act: 76 ),
  ( sym: -15; act: 77 ),
  ( sym: -13; act: 78 ),
  ( sym: -11; act: 79 ),
{ 44: }
{ 45: }
{ 46: }
{ 47: }
{ 48: }
{ 49: }
{ 50: }
{ 51: }
{ 52: }
  ( sym: -5; act: 89 ),
{ 53: }
  ( sym: -25; act: 15 ),
  ( sym: -24; act: 90 ),
  ( sym: -23; act: 16 ),
  ( sym: -21; act: 91 ),
  ( sym: -14; act: 17 ),
  ( sym: -12; act: 92 ),
  ( sym: -11; act: 19 ),
{ 54: }
{ 55: }
{ 56: }
  ( sym: -5; act: 94 ),
{ 57: }
  ( sym: -37; act: 95 ),
  ( sym: -17; act: 96 ),
  ( sym: -11; act: 97 ),
{ 58: }
{ 59: }
{ 60: }
{ 61: }
{ 62: }
{ 63: }
{ 64: }
{ 65: }
{ 66: }
  ( sym: -28; act: 76 ),
  ( sym: -15; act: 77 ),
  ( sym: -13; act: 99 ),
  ( sym: -11; act: 79 ),
{ 67: }
  ( sym: -9; act: 100 ),
{ 68: }
{ 69: }
  ( sym: -20; act: 73 ),
  ( sym: -11; act: 101 ),
{ 70: }
  ( sym: -37; act: 95 ),
  ( sym: -17; act: 102 ),
  ( sym: -11; act: 97 ),
{ 71: }
{ 72: }
  ( sym: -34; act: 103 ),
  ( sym: -25; act: 104 ),
  ( sym: -19; act: 105 ),
  ( sym: -11; act: 106 ),
{ 73: }
{ 74: }
{ 75: }
{ 76: }
{ 77: }
  ( sym: -31; act: 117 ),
{ 78: }
  ( sym: -10; act: 120 ),
{ 79: }
  ( sym: -30; act: 123 ),
{ 80: }
  ( sym: -5; act: 125 ),
{ 81: }
  ( sym: -28; act: 76 ),
  ( sym: -15; act: 126 ),
  ( sym: -11; act: 79 ),
{ 82: }
  ( sym: -28; act: 76 ),
  ( sym: -15; act: 127 ),
  ( sym: -11; act: 79 ),
{ 83: }
{ 84: }
{ 85: }
{ 86: }
  ( sym: -28; act: 76 ),
  ( sym: -15; act: 128 ),
  ( sym: -11; act: 79 ),
{ 87: }
  ( sym: -28; act: 76 ),
  ( sym: -15; act: 129 ),
  ( sym: -11; act: 79 ),
{ 88: }
{ 89: }
{ 90: }
  ( sym: -25; act: 15 ),
  ( sym: -24; act: 90 ),
  ( sym: -23; act: 16 ),
  ( sym: -21; act: 131 ),
  ( sym: -14; act: 17 ),
  ( sym: -12; act: 92 ),
  ( sym: -11; act: 19 ),
{ 91: }
{ 92: }
  ( sym: -28; act: 76 ),
  ( sym: -15; act: 77 ),
  ( sym: -13; act: 133 ),
  ( sym: -11; act: 79 ),
{ 93: }
{ 94: }
{ 95: }
{ 96: }
{ 97: }
{ 98: }
{ 99: }
{ 100: }
  ( sym: -28; act: 76 ),
  ( sym: -15; act: 139 ),
  ( sym: -11; act: 79 ),
{ 101: }
{ 102: }
{ 103: }
{ 104: }
{ 105: }
{ 106: }
{ 107: }
  ( sym: -34; act: 147 ),
  ( sym: -32; act: 148 ),
  ( sym: -29; act: 149 ),
  ( sym: -25; act: 150 ),
  ( sym: -23; act: 16 ),
  ( sym: -14; act: 17 ),
  ( sym: -12; act: 151 ),
  ( sym: -11; act: 152 ),
{ 108: }
{ 109: }
{ 110: }
{ 111: }
  ( sym: -34; act: 154 ),
  ( sym: -25; act: 104 ),
  ( sym: -11; act: 106 ),
{ 112: }
  ( sym: -34; act: 155 ),
  ( sym: -25; act: 104 ),
  ( sym: -11; act: 106 ),
{ 113: }
  ( sym: -34; act: 156 ),
  ( sym: -25; act: 104 ),
  ( sym: -11; act: 106 ),
{ 114: }
{ 115: }
{ 116: }
  ( sym: -28; act: 76 ),
  ( sym: -15; act: 157 ),
  ( sym: -11; act: 79 ),
{ 117: }
{ 118: }
  ( sym: -26; act: 158 ),
  ( sym: -25; act: 15 ),
  ( sym: -23; act: 16 ),
  ( sym: -16; act: 159 ),
  ( sym: -14; act: 17 ),
  ( sym: -12; act: 160 ),
  ( sym: -11; act: 19 ),
{ 119: }
  ( sym: -34; act: 147 ),
  ( sym: -32; act: 164 ),
  ( sym: -29; act: 165 ),
  ( sym: -25; act: 104 ),
  ( sym: -11; act: 106 ),
{ 120: }
{ 121: }
  ( sym: -28; act: 76 ),
  ( sym: -15; act: 168 ),
  ( sym: -11; act: 79 ),
{ 122: }
{ 123: }
  ( sym: -34; act: 147 ),
  ( sym: -32; act: 164 ),
  ( sym: -29; act: 170 ),
  ( sym: -25; act: 104 ),
  ( sym: -11; act: 106 ),
{ 124: }
  ( sym: -34; act: 147 ),
  ( sym: -32; act: 164 ),
  ( sym: -29; act: 171 ),
  ( sym: -25; act: 104 ),
  ( sym: -11; act: 106 ),
{ 125: }
{ 126: }
  ( sym: -31; act: 117 ),
{ 127: }
  ( sym: -31; act: 117 ),
{ 128: }
  ( sym: -31; act: 117 ),
{ 129: }
  ( sym: -31; act: 117 ),
{ 130: }
{ 131: }
{ 132: }
{ 133: }
{ 134: }
{ 135: }
  ( sym: -37; act: 95 ),
  ( sym: -17; act: 175 ),
  ( sym: -11; act: 97 ),
{ 136: }
{ 137: }
  ( sym: -34; act: 147 ),
  ( sym: -32; act: 164 ),
  ( sym: -29; act: 176 ),
  ( sym: -25; act: 104 ),
  ( sym: -11; act: 106 ),
{ 138: }
{ 139: }
  ( sym: -31; act: 117 ),
{ 140: }
{ 141: }
  ( sym: -18; act: 178 ),
  ( sym: -4; act: 179 ),
{ 142: }
  ( sym: -34; act: 147 ),
  ( sym: -32; act: 164 ),
  ( sym: -29; act: 181 ),
  ( sym: -25; act: 104 ),
  ( sym: -11; act: 106 ),
{ 143: }
  ( sym: -34; act: 147 ),
  ( sym: -32; act: 164 ),
  ( sym: -29; act: 182 ),
  ( sym: -25; act: 104 ),
  ( sym: -11; act: 106 ),
{ 144: }
{ 145: }
  ( sym: -38; act: 183 ),
  ( sym: -36; act: 184 ),
  ( sym: -34; act: 147 ),
  ( sym: -32; act: 164 ),
  ( sym: -29; act: 185 ),
  ( sym: -25; act: 104 ),
  ( sym: -11; act: 106 ),
{ 146: }
  ( sym: -38; act: 183 ),
  ( sym: -36; act: 186 ),
  ( sym: -34; act: 147 ),
  ( sym: -32; act: 164 ),
  ( sym: -29; act: 185 ),
  ( sym: -25; act: 104 ),
  ( sym: -11; act: 106 ),
{ 147: }
{ 148: }
{ 149: }
{ 150: }
{ 151: }
  ( sym: -28; act: 204 ),
{ 152: }
{ 153: }
  ( sym: -34; act: 208 ),
  ( sym: -25; act: 104 ),
  ( sym: -11; act: 106 ),
{ 154: }
{ 155: }
{ 156: }
{ 157: }
  ( sym: -31; act: 117 ),
{ 158: }
{ 159: }
{ 160: }
  ( sym: -28; act: 211 ),
  ( sym: -27; act: 212 ),
  ( sym: -15; act: 213 ),
  ( sym: -11; act: 79 ),
{ 161: }
{ 162: }
{ 163: }
{ 164: }
{ 165: }
{ 166: }
{ 167: }
{ 168: }
  ( sym: -31; act: 117 ),
{ 169: }
  ( sym: -11; act: 219 ),
{ 170: }
{ 171: }
{ 172: }
  ( sym: -28; act: 76 ),
  ( sym: -15; act: 77 ),
  ( sym: -13; act: 220 ),
  ( sym: -11; act: 79 ),
{ 173: }
{ 174: }
{ 175: }
{ 176: }
{ 177: }
  ( sym: -4; act: 221 ),
{ 178: }
{ 179: }
{ 180: }
  ( sym: -34; act: 103 ),
  ( sym: -25; act: 104 ),
  ( sym: -19; act: 225 ),
  ( sym: -11; act: 106 ),
{ 181: }
{ 182: }
{ 183: }
{ 184: }
{ 185: }
{ 186: }
{ 187: }
{ 188: }
  ( sym: -34; act: 147 ),
  ( sym: -32; act: 164 ),
  ( sym: -29; act: 229 ),
  ( sym: -25; act: 104 ),
  ( sym: -11; act: 106 ),
{ 189: }
  ( sym: -34; act: 147 ),
  ( sym: -32; act: 164 ),
  ( sym: -29; act: 230 ),
  ( sym: -25; act: 104 ),
  ( sym: -11; act: 106 ),
{ 190: }
  ( sym: -34; act: 147 ),
  ( sym: -32; act: 164 ),
  ( sym: -29; act: 231 ),
  ( sym: -25; act: 104 ),
  ( sym: -11; act: 106 ),
{ 191: }
  ( sym: -34; act: 147 ),
  ( sym: -32; act: 164 ),
  ( sym: -29; act: 232 ),
  ( sym: -25; act: 104 ),
  ( sym: -11; act: 106 ),
{ 192: }
  ( sym: -34; act: 147 ),
  ( sym: -32; act: 164 ),
  ( sym: -29; act: 233 ),
  ( sym: -25; act: 104 ),
  ( sym: -11; act: 106 ),
{ 193: }
  ( sym: -34; act: 147 ),
  ( sym: -32; act: 164 ),
  ( sym: -29; act: 234 ),
  ( sym: -25; act: 104 ),
  ( sym: -11; act: 106 ),
{ 194: }
  ( sym: -34; act: 147 ),
  ( sym: -33; act: 235 ),
  ( sym: -32; act: 164 ),
  ( sym: -29; act: 236 ),
  ( sym: -25; act: 104 ),
  ( sym: -11; act: 106 ),
{ 195: }
  ( sym: -34; act: 147 ),
  ( sym: -32; act: 164 ),
  ( sym: -29; act: 237 ),
  ( sym: -25; act: 104 ),
  ( sym: -11; act: 106 ),
{ 196: }
  ( sym: -34; act: 147 ),
  ( sym: -32; act: 164 ),
  ( sym: -29; act: 238 ),
  ( sym: -25; act: 104 ),
  ( sym: -11; act: 106 ),
{ 197: }
  ( sym: -34; act: 147 ),
  ( sym: -32; act: 164 ),
  ( sym: -29; act: 239 ),
  ( sym: -25; act: 104 ),
  ( sym: -11; act: 106 ),
{ 198: }
  ( sym: -34; act: 147 ),
  ( sym: -32; act: 164 ),
  ( sym: -29; act: 240 ),
  ( sym: -25; act: 104 ),
  ( sym: -11; act: 106 ),
{ 199: }
  ( sym: -34; act: 147 ),
  ( sym: -32; act: 164 ),
  ( sym: -29; act: 241 ),
  ( sym: -25; act: 104 ),
  ( sym: -11; act: 106 ),
{ 200: }
  ( sym: -34; act: 147 ),
  ( sym: -32; act: 164 ),
  ( sym: -29; act: 242 ),
  ( sym: -25; act: 104 ),
  ( sym: -11; act: 106 ),
{ 201: }
  ( sym: -34; act: 147 ),
  ( sym: -32; act: 164 ),
  ( sym: -29; act: 243 ),
  ( sym: -25; act: 104 ),
  ( sym: -11; act: 106 ),
{ 202: }
  ( sym: -34; act: 147 ),
  ( sym: -32; act: 164 ),
  ( sym: -29; act: 244 ),
  ( sym: -25; act: 104 ),
  ( sym: -11; act: 106 ),
{ 203: }
  ( sym: -34; act: 147 ),
  ( sym: -32; act: 164 ),
  ( sym: -29; act: 245 ),
  ( sym: -25; act: 104 ),
  ( sym: -11; act: 106 ),
{ 204: }
{ 205: }
  ( sym: -34; act: 247 ),
  ( sym: -25; act: 104 ),
  ( sym: -11; act: 106 ),
{ 206: }
{ 207: }
  ( sym: -35; act: 249 ),
  ( sym: -34; act: 250 ),
  ( sym: -25; act: 104 ),
  ( sym: -11; act: 106 ),
{ 208: }
{ 209: }
  ( sym: -26; act: 158 ),
  ( sym: -25; act: 15 ),
  ( sym: -23; act: 16 ),
  ( sym: -16; act: 252 ),
  ( sym: -14; act: 17 ),
  ( sym: -12; act: 160 ),
  ( sym: -11; act: 19 ),
{ 210: }
{ 211: }
{ 212: }
  ( sym: -31; act: 254 ),
{ 213: }
  ( sym: -31; act: 117 ),
{ 214: }
  ( sym: -28; act: 211 ),
  ( sym: -27; act: 258 ),
  ( sym: -15; act: 259 ),
  ( sym: -11; act: 79 ),
{ 215: }
  ( sym: -28; act: 211 ),
  ( sym: -27; act: 261 ),
  ( sym: -15; act: 262 ),
  ( sym: -11; act: 79 ),
{ 216: }
  ( sym: -28; act: 211 ),
  ( sym: -27; act: 263 ),
  ( sym: -15; act: 264 ),
  ( sym: -11; act: 79 ),
{ 217: }
{ 218: }
{ 219: }
{ 220: }
{ 221: }
{ 222: }
{ 223: }
{ 224: }
  ( sym: -34; act: 103 ),
  ( sym: -25; act: 104 ),
  ( sym: -19; act: 267 ),
  ( sym: -11; act: 106 ),
{ 225: }
{ 226: }
  ( sym: -38; act: 183 ),
  ( sym: -36; act: 268 ),
  ( sym: -34; act: 147 ),
  ( sym: -32; act: 164 ),
  ( sym: -29; act: 185 ),
  ( sym: -25; act: 104 ),
  ( sym: -11; act: 106 ),
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
{ 239: }
{ 240: }
{ 241: }
{ 242: }
{ 243: }
{ 244: }
{ 245: }
{ 246: }
{ 247: }
{ 248: }
  ( sym: -34; act: 271 ),
  ( sym: -25; act: 104 ),
  ( sym: -11; act: 106 ),
{ 249: }
{ 250: }
{ 251: }
  ( sym: -4; act: 272 ),
{ 252: }
{ 253: }
  ( sym: -28; act: 211 ),
  ( sym: -27; act: 273 ),
  ( sym: -15; act: 274 ),
  ( sym: -11; act: 79 ),
{ 254: }
{ 255: }
  ( sym: -26; act: 158 ),
  ( sym: -25; act: 15 ),
  ( sym: -23; act: 16 ),
  ( sym: -16; act: 275 ),
  ( sym: -14; act: 17 ),
  ( sym: -12; act: 160 ),
  ( sym: -11; act: 19 ),
{ 256: }
  ( sym: -34; act: 147 ),
  ( sym: -32; act: 164 ),
  ( sym: -29; act: 276 ),
  ( sym: -25; act: 104 ),
  ( sym: -11; act: 106 ),
{ 257: }
  ( sym: -34; act: 147 ),
  ( sym: -32; act: 164 ),
  ( sym: -29; act: 165 ),
  ( sym: -25; act: 104 ),
  ( sym: -11; act: 106 ),
{ 258: }
  ( sym: -31; act: 254 ),
{ 259: }
  ( sym: -31; act: 117 ),
{ 260: }
  ( sym: -28; act: 211 ),
  ( sym: -27; act: 263 ),
  ( sym: -15; act: 279 ),
  ( sym: -11; act: 79 ),
{ 261: }
  ( sym: -31; act: 254 ),
{ 262: }
  ( sym: -31; act: 117 ),
{ 263: }
  ( sym: -31; act: 254 ),
{ 264: }
  ( sym: -31; act: 117 ),
{ 265: }
{ 266: }
  ( sym: -26; act: 158 ),
  ( sym: -25; act: 15 ),
  ( sym: -23; act: 16 ),
  ( sym: -16; act: 280 ),
  ( sym: -14; act: 17 ),
  ( sym: -12; act: 160 ),
  ( sym: -11; act: 19 ),
{ 267: }
{ 268: }
{ 269: }
  ( sym: -34; act: 147 ),
  ( sym: -32; act: 164 ),
  ( sym: -29; act: 282 ),
  ( sym: -25; act: 104 ),
  ( sym: -11; act: 106 ),
{ 270: }
  ( sym: -34; act: 283 ),
  ( sym: -25; act: 104 ),
  ( sym: -11; act: 106 ),
{ 271: }
{ 272: }
{ 273: }
  ( sym: -31; act: 254 ),
{ 274: }
  ( sym: -31; act: 117 ),
{ 275: }
{ 276: }
{ 277: }
{ 278: }
{ 279: }
  ( sym: -31; act: 117 ),
{ 280: }
{ 281: }
{ 282: }
{ 283: }
{ 284: }
  ( sym: -38; act: 183 ),
  ( sym: -36; act: 288 ),
  ( sym: -34; act: 147 ),
  ( sym: -32; act: 164 ),
  ( sym: -29; act: 185 ),
  ( sym: -25; act: 104 ),
  ( sym: -11; act: 106 )
{ 285: }
{ 286: }
{ 287: }
{ 288: }
{ 289: }
{ 290: }
);

yyd : array [0..yynstates-1] of Integer = (
{ 0: } 0,
{ 1: } 0,
{ 2: } 0,
{ 3: } -8,
{ 4: } -7,
{ 5: } 0,
{ 6: } 0,
{ 7: } -4,
{ 8: } 0,
{ 9: } 0,
{ 10: } 0,
{ 11: } 0,
{ 12: } 0,
{ 13: } -9,
{ 14: } -22,
{ 15: } -70,
{ 16: } -52,
{ 17: } -51,
{ 18: } 0,
{ 19: } -71,
{ 20: } 0,
{ 21: } 0,
{ 22: } 0,
{ 23: } -56,
{ 24: } 0,
{ 25: } 0,
{ 26: } 0,
{ 27: } -59,
{ 28: } -66,
{ 29: } -68,
{ 30: } -67,
{ 31: } 0,
{ 32: } 0,
{ 33: } -6,
{ 34: } -5,
{ 35: } 0,
{ 36: } 0,
{ 37: } 0,
{ 38: } 0,
{ 39: } 0,
{ 40: } 0,
{ 41: } 0,
{ 42: } 0,
{ 43: } 0,
{ 44: } -11,
{ 45: } -12,
{ 46: } -13,
{ 47: } -14,
{ 48: } -15,
{ 49: } -16,
{ 50: } -17,
{ 51: } 0,
{ 52: } -4,
{ 53: } 0,
{ 54: } 0,
{ 55: } -50,
{ 56: } -4,
{ 57: } 0,
{ 58: } -65,
{ 59: } -58,
{ 60: } 0,
{ 61: } -61,
{ 62: } -45,
{ 63: } -57,
{ 64: } -27,
{ 65: } -32,
{ 66: } 0,
{ 67: } 0,
{ 68: } -26,
{ 69: } 0,
{ 70: } 0,
{ 71: } -30,
{ 72: } 0,
{ 73: } 0,
{ 74: } 0,
{ 75: } -43,
{ 76: } 0,
{ 77: } 0,
{ 78: } 0,
{ 79: } 0,
{ 80: } -4,
{ 81: } 0,
{ 82: } 0,
{ 83: } -83,
{ 84: } -85,
{ 85: } -84,
{ 86: } 0,
{ 87: } 0,
{ 88: } -48,
{ 89: } 0,
{ 90: } 0,
{ 91: } 0,
{ 92: } 0,
{ 93: } -46,
{ 94: } 0,
{ 95: } 0,
{ 96: } 0,
{ 97: } 0,
{ 98: } -63,
{ 99: } 0,
{ 100: } 0,
{ 101: } 0,
{ 102: } 0,
{ 103: } 0,
{ 104: } -131,
{ 105: } 0,
{ 106: } 0,
{ 107: } 0,
{ 108: } -133,
{ 109: } -132,
{ 110: } -29,
{ 111: } 0,
{ 112: } 0,
{ 113: } 0,
{ 114: } -37,
{ 115: } -39,
{ 116: } 0,
{ 117: } -94,
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
{ 128: } 0,
{ 129: } 0,
{ 130: } -34,
{ 131: } -53,
{ 132: } -33,
{ 133: } 0,
{ 134: } -36,
{ 135: } 0,
{ 136: } -35,
{ 137: } 0,
{ 138: } -25,
{ 139: } 0,
{ 140: } -23,
{ 141: } 0,
{ 142: } 0,
{ 143: } 0,
{ 144: } -31,
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
{ 156: } 0,
{ 157: } 0,
{ 158: } 0,
{ 159: } 0,
{ 160: } 0,
{ 161: } -98,
{ 162: } 0,
{ 163: } -81,
{ 164: } -109,
{ 165: } 0,
{ 166: } -96,
{ 167: } -21,
{ 168: } 0,
{ 169: } 0,
{ 170: } 0,
{ 171: } 0,
{ 172: } 0,
{ 173: } -97,
{ 174: } -55,
{ 175: } -147,
{ 176: } 0,
{ 177: } 0,
{ 178: } 0,
{ 179: } 0,
{ 180: } 0,
{ 181: } -134,
{ 182: } -135,
{ 183: } 0,
{ 184: } 0,
{ 185: } 0,
{ 186: } 0,
{ 187: } -144,
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
{ 204: } 0,
{ 205: } 0,
{ 206: } 0,
{ 207: } 0,
{ 208: } 0,
{ 209: } 0,
{ 210: } -93,
{ 211: } 0,
{ 212: } 0,
{ 213: } 0,
{ 214: } 0,
{ 215: } 0,
{ 216: } 0,
{ 217: } -99,
{ 218: } -95,
{ 219: } 0,
{ 220: } -73,
{ 221: } 0,
{ 222: } -2,
{ 223: } -28,
{ 224: } 0,
{ 225: } -153,
{ 226: } 0,
{ 227: } -143,
{ 228: } -146,
{ 229: } 0,
{ 230: } 0,
{ 231: } 0,
{ 232: } 0,
{ 233: } 0,
{ 234: } 0,
{ 235: } -125,
{ 236: } 0,
{ 237: } 0,
{ 238: } 0,
{ 239: } 0,
{ 240: } 0,
{ 241: } 0,
{ 242: } 0,
{ 243: } 0,
{ 244: } 0,
{ 245: } 0,
{ 246: } 0,
{ 247: } 0,
{ 248: } 0,
{ 249: } -139,
{ 250: } 0,
{ 251: } 0,
{ 252: } -80,
{ 253: } 0,
{ 254: } -104,
{ 255: } 0,
{ 256: } 0,
{ 257: } 0,
{ 258: } 0,
{ 259: } 0,
{ 260: } 0,
{ 261: } 0,
{ 262: } 0,
{ 263: } 0,
{ 264: } 0,
{ 265: } -19,
{ 266: } 0,
{ 267: } 0,
{ 268: } -155,
{ 269: } 0,
{ 270: } 0,
{ 271: } 0,
{ 272: } 0,
{ 273: } 0,
{ 274: } 0,
{ 275: } 0,
{ 276: } 0,
{ 277: } -96,
{ 278: } -107,
{ 279: } 0,
{ 280: } 0,
{ 281: } -154,
{ 282: } 0,
{ 283: } 0,
{ 284: } 0,
{ 285: } -103,
{ 286: } -105,
{ 287: } 0,
{ 288: } 0,
{ 289: } -24,
{ 290: } -145
);

yyal : array [0..yynstates-1] of Integer = (
{ 0: } 1,
{ 1: } 18,
{ 2: } 19,
{ 3: } 32,
{ 4: } 32,
{ 5: } 32,
{ 6: } 50,
{ 7: } 51,
{ 8: } 51,
{ 9: } 64,
{ 10: } 65,
{ 11: } 66,
{ 12: } 67,
{ 13: } 68,
{ 14: } 68,
{ 15: } 68,
{ 16: } 68,
{ 17: } 68,
{ 18: } 68,
{ 19: } 84,
{ 20: } 84,
{ 21: } 87,
{ 22: } 90,
{ 23: } 93,
{ 24: } 93,
{ 25: } 135,
{ 26: } 184,
{ 27: } 227,
{ 28: } 227,
{ 29: } 227,
{ 30: } 227,
{ 31: } 227,
{ 32: } 240,
{ 33: } 248,
{ 34: } 248,
{ 35: } 248,
{ 36: } 250,
{ 37: } 266,
{ 38: } 283,
{ 39: } 286,
{ 40: } 289,
{ 41: } 310,
{ 42: } 331,
{ 43: } 352,
{ 44: } 361,
{ 45: } 361,
{ 46: } 361,
{ 47: } 361,
{ 48: } 361,
{ 49: } 361,
{ 50: } 361,
{ 51: } 361,
{ 52: } 381,
{ 53: } 381,
{ 54: } 394,
{ 55: } 414,
{ 56: } 414,
{ 57: } 414,
{ 58: } 416,
{ 59: } 416,
{ 60: } 416,
{ 61: } 458,
{ 62: } 458,
{ 63: } 458,
{ 64: } 458,
{ 65: } 458,
{ 66: } 458,
{ 67: } 467,
{ 68: } 482,
{ 69: } 482,
{ 70: } 499,
{ 71: } 501,
{ 72: } 501,
{ 73: } 517,
{ 74: } 538,
{ 75: } 559,
{ 76: } 559,
{ 77: } 560,
{ 78: } 565,
{ 79: } 568,
{ 80: } 575,
{ 81: } 575,
{ 82: } 583,
{ 83: } 591,
{ 84: } 591,
{ 85: } 591,
{ 86: } 591,
{ 87: } 599,
{ 88: } 607,
{ 89: } 607,
{ 90: } 608,
{ 91: } 622,
{ 92: } 623,
{ 93: } 632,
{ 94: } 632,
{ 95: } 633,
{ 96: } 636,
{ 97: } 637,
{ 98: } 641,
{ 99: } 641,
{ 100: } 643,
{ 101: } 651,
{ 102: } 652,
{ 103: } 653,
{ 104: } 657,
{ 105: } 657,
{ 106: } 658,
{ 107: } 686,
{ 108: } 706,
{ 109: } 706,
{ 110: } 706,
{ 111: } 706,
{ 112: } 721,
{ 113: } 736,
{ 114: } 751,
{ 115: } 751,
{ 116: } 751,
{ 117: } 759,
{ 118: } 759,
{ 119: } 774,
{ 120: } 790,
{ 121: } 791,
{ 122: } 799,
{ 123: } 800,
{ 124: } 815,
{ 125: } 830,
{ 126: } 833,
{ 127: } 836,
{ 128: } 842,
{ 129: } 848,
{ 130: } 854,
{ 131: } 854,
{ 132: } 854,
{ 133: } 854,
{ 134: } 856,
{ 135: } 856,
{ 136: } 859,
{ 137: } 859,
{ 138: } 874,
{ 139: } 874,
{ 140: } 877,
{ 141: } 877,
{ 142: } 879,
{ 143: } 894,
{ 144: } 909,
{ 145: } 909,
{ 146: } 925,
{ 147: } 941,
{ 148: } 969,
{ 149: } 986,
{ 150: } 1002,
{ 151: } 1024,
{ 152: } 1029,
{ 153: } 1053,
{ 154: } 1068,
{ 155: } 1096,
{ 156: } 1124,
{ 157: } 1152,
{ 158: } 1158,
{ 159: } 1160,
{ 160: } 1161,
{ 161: } 1172,
{ 162: } 1172,
{ 163: } 1183,
{ 164: } 1183,
{ 165: } 1183,
{ 166: } 1200,
{ 167: } 1200,
{ 168: } 1200,
{ 169: } 1205,
{ 170: } 1206,
{ 171: } 1228,
{ 172: } 1250,
{ 173: } 1259,
{ 174: } 1259,
{ 175: } 1259,
{ 176: } 1259,
{ 177: } 1278,
{ 178: } 1280,
{ 179: } 1281,
{ 180: } 1282,
{ 181: } 1297,
{ 182: } 1297,
{ 183: } 1297,
{ 184: } 1300,
{ 185: } 1301,
{ 186: } 1320,
{ 187: } 1321,
{ 188: } 1321,
{ 189: } 1336,
{ 190: } 1351,
{ 191: } 1366,
{ 192: } 1381,
{ 193: } 1396,
{ 194: } 1411,
{ 195: } 1426,
{ 196: } 1441,
{ 197: } 1456,
{ 198: } 1471,
{ 199: } 1486,
{ 200: } 1501,
{ 201: } 1516,
{ 202: } 1531,
{ 203: } 1546,
{ 204: } 1561,
{ 205: } 1562,
{ 206: } 1577,
{ 207: } 1578,
{ 208: } 1617,
{ 209: } 1620,
{ 210: } 1635,
{ 211: } 1635,
{ 212: } 1636,
{ 213: } 1640,
{ 214: } 1644,
{ 215: } 1654,
{ 216: } 1665,
{ 217: } 1676,
{ 218: } 1676,
{ 219: } 1676,
{ 220: } 1677,
{ 221: } 1677,
{ 222: } 1678,
{ 223: } 1678,
{ 224: } 1678,
{ 225: } 1693,
{ 226: } 1693,
{ 227: } 1710,
{ 228: } 1710,
{ 229: } 1710,
{ 230: } 1738,
{ 231: } 1766,
{ 232: } 1794,
{ 233: } 1822,
{ 234: } 1850,
{ 235: } 1878,
{ 236: } 1878,
{ 237: } 1895,
{ 238: } 1923,
{ 239: } 1951,
{ 240: } 1979,
{ 241: } 2007,
{ 242: } 2035,
{ 243: } 2063,
{ 244: } 2091,
{ 245: } 2119,
{ 246: } 2147,
{ 247: } 2148,
{ 248: } 2176,
{ 249: } 2191,
{ 250: } 2191,
{ 251: } 2219,
{ 252: } 2221,
{ 253: } 2221,
{ 254: } 2232,
{ 255: } 2232,
{ 256: } 2247,
{ 257: } 2262,
{ 258: } 2278,
{ 259: } 2281,
{ 260: } 2284,
{ 261: } 2295,
{ 262: } 2299,
{ 263: } 2303,
{ 264: } 2307,
{ 265: } 2311,
{ 266: } 2311,
{ 267: } 2326,
{ 268: } 2327,
{ 269: } 2327,
{ 270: } 2342,
{ 271: } 2357,
{ 272: } 2385,
{ 273: } 2386,
{ 274: } 2390,
{ 275: } 2394,
{ 276: } 2395,
{ 277: } 2412,
{ 278: } 2412,
{ 279: } 2412,
{ 280: } 2416,
{ 281: } 2417,
{ 282: } 2417,
{ 283: } 2445,
{ 284: } 2473,
{ 285: } 2489,
{ 286: } 2489,
{ 287: } 2489,
{ 288: } 2490,
{ 289: } 2491,
{ 290: } 2491
);

yyah : array [0..yynstates-1] of Integer = (
{ 0: } 17,
{ 1: } 18,
{ 2: } 31,
{ 3: } 31,
{ 4: } 31,
{ 5: } 49,
{ 6: } 50,
{ 7: } 50,
{ 8: } 63,
{ 9: } 64,
{ 10: } 65,
{ 11: } 66,
{ 12: } 67,
{ 13: } 67,
{ 14: } 67,
{ 15: } 67,
{ 16: } 67,
{ 17: } 67,
{ 18: } 83,
{ 19: } 83,
{ 20: } 86,
{ 21: } 89,
{ 22: } 92,
{ 23: } 92,
{ 24: } 134,
{ 25: } 183,
{ 26: } 226,
{ 27: } 226,
{ 28: } 226,
{ 29: } 226,
{ 30: } 226,
{ 31: } 239,
{ 32: } 247,
{ 33: } 247,
{ 34: } 247,
{ 35: } 249,
{ 36: } 265,
{ 37: } 282,
{ 38: } 285,
{ 39: } 288,
{ 40: } 309,
{ 41: } 330,
{ 42: } 351,
{ 43: } 360,
{ 44: } 360,
{ 45: } 360,
{ 46: } 360,
{ 47: } 360,
{ 48: } 360,
{ 49: } 360,
{ 50: } 360,
{ 51: } 380,
{ 52: } 380,
{ 53: } 393,
{ 54: } 413,
{ 55: } 413,
{ 56: } 413,
{ 57: } 415,
{ 58: } 415,
{ 59: } 415,
{ 60: } 457,
{ 61: } 457,
{ 62: } 457,
{ 63: } 457,
{ 64: } 457,
{ 65: } 457,
{ 66: } 466,
{ 67: } 481,
{ 68: } 481,
{ 69: } 498,
{ 70: } 500,
{ 71: } 500,
{ 72: } 516,
{ 73: } 537,
{ 74: } 558,
{ 75: } 558,
{ 76: } 559,
{ 77: } 564,
{ 78: } 567,
{ 79: } 574,
{ 80: } 574,
{ 81: } 582,
{ 82: } 590,
{ 83: } 590,
{ 84: } 590,
{ 85: } 590,
{ 86: } 598,
{ 87: } 606,
{ 88: } 606,
{ 89: } 607,
{ 90: } 621,
{ 91: } 622,
{ 92: } 631,
{ 93: } 631,
{ 94: } 632,
{ 95: } 635,
{ 96: } 636,
{ 97: } 640,
{ 98: } 640,
{ 99: } 642,
{ 100: } 650,
{ 101: } 651,
{ 102: } 652,
{ 103: } 656,
{ 104: } 656,
{ 105: } 657,
{ 106: } 685,
{ 107: } 705,
{ 108: } 705,
{ 109: } 705,
{ 110: } 705,
{ 111: } 720,
{ 112: } 735,
{ 113: } 750,
{ 114: } 750,
{ 115: } 750,
{ 116: } 758,
{ 117: } 758,
{ 118: } 773,
{ 119: } 789,
{ 120: } 790,
{ 121: } 798,
{ 122: } 799,
{ 123: } 814,
{ 124: } 829,
{ 125: } 832,
{ 126: } 835,
{ 127: } 841,
{ 128: } 847,
{ 129: } 853,
{ 130: } 853,
{ 131: } 853,
{ 132: } 853,
{ 133: } 855,
{ 134: } 855,
{ 135: } 858,
{ 136: } 858,
{ 137: } 873,
{ 138: } 873,
{ 139: } 876,
{ 140: } 876,
{ 141: } 878,
{ 142: } 893,
{ 143: } 908,
{ 144: } 908,
{ 145: } 924,
{ 146: } 940,
{ 147: } 968,
{ 148: } 985,
{ 149: } 1001,
{ 150: } 1023,
{ 151: } 1028,
{ 152: } 1052,
{ 153: } 1067,
{ 154: } 1095,
{ 155: } 1123,
{ 156: } 1151,
{ 157: } 1157,
{ 158: } 1159,
{ 159: } 1160,
{ 160: } 1171,
{ 161: } 1171,
{ 162: } 1182,
{ 163: } 1182,
{ 164: } 1182,
{ 165: } 1199,
{ 166: } 1199,
{ 167: } 1199,
{ 168: } 1204,
{ 169: } 1205,
{ 170: } 1227,
{ 171: } 1249,
{ 172: } 1258,
{ 173: } 1258,
{ 174: } 1258,
{ 175: } 1258,
{ 176: } 1277,
{ 177: } 1279,
{ 178: } 1280,
{ 179: } 1281,
{ 180: } 1296,
{ 181: } 1296,
{ 182: } 1296,
{ 183: } 1299,
{ 184: } 1300,
{ 185: } 1319,
{ 186: } 1320,
{ 187: } 1320,
{ 188: } 1335,
{ 189: } 1350,
{ 190: } 1365,
{ 191: } 1380,
{ 192: } 1395,
{ 193: } 1410,
{ 194: } 1425,
{ 195: } 1440,
{ 196: } 1455,
{ 197: } 1470,
{ 198: } 1485,
{ 199: } 1500,
{ 200: } 1515,
{ 201: } 1530,
{ 202: } 1545,
{ 203: } 1560,
{ 204: } 1561,
{ 205: } 1576,
{ 206: } 1577,
{ 207: } 1616,
{ 208: } 1619,
{ 209: } 1634,
{ 210: } 1634,
{ 211: } 1635,
{ 212: } 1639,
{ 213: } 1643,
{ 214: } 1653,
{ 215: } 1664,
{ 216: } 1675,
{ 217: } 1675,
{ 218: } 1675,
{ 219: } 1676,
{ 220: } 1676,
{ 221: } 1677,
{ 222: } 1677,
{ 223: } 1677,
{ 224: } 1692,
{ 225: } 1692,
{ 226: } 1709,
{ 227: } 1709,
{ 228: } 1709,
{ 229: } 1737,
{ 230: } 1765,
{ 231: } 1793,
{ 232: } 1821,
{ 233: } 1849,
{ 234: } 1877,
{ 235: } 1877,
{ 236: } 1894,
{ 237: } 1922,
{ 238: } 1950,
{ 239: } 1978,
{ 240: } 2006,
{ 241: } 2034,
{ 242: } 2062,
{ 243: } 2090,
{ 244: } 2118,
{ 245: } 2146,
{ 246: } 2147,
{ 247: } 2175,
{ 248: } 2190,
{ 249: } 2190,
{ 250: } 2218,
{ 251: } 2220,
{ 252: } 2220,
{ 253: } 2231,
{ 254: } 2231,
{ 255: } 2246,
{ 256: } 2261,
{ 257: } 2277,
{ 258: } 2280,
{ 259: } 2283,
{ 260: } 2294,
{ 261: } 2298,
{ 262: } 2302,
{ 263: } 2306,
{ 264: } 2310,
{ 265: } 2310,
{ 266: } 2325,
{ 267: } 2326,
{ 268: } 2326,
{ 269: } 2341,
{ 270: } 2356,
{ 271: } 2384,
{ 272: } 2385,
{ 273: } 2389,
{ 274: } 2393,
{ 275: } 2394,
{ 276: } 2411,
{ 277: } 2411,
{ 278: } 2411,
{ 279: } 2415,
{ 280: } 2416,
{ 281: } 2416,
{ 282: } 2444,
{ 283: } 2472,
{ 284: } 2488,
{ 285: } 2488,
{ 286: } 2488,
{ 287: } 2489,
{ 288: } 2490,
{ 289: } 2490,
{ 290: } 2490
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
{ 26: } 34,
{ 27: } 34,
{ 28: } 34,
{ 29: } 34,
{ 30: } 34,
{ 31: } 34,
{ 32: } 39,
{ 33: } 40,
{ 34: } 40,
{ 35: } 40,
{ 36: } 40,
{ 37: } 41,
{ 38: } 41,
{ 39: } 43,
{ 40: } 43,
{ 41: } 44,
{ 42: } 45,
{ 43: } 46,
{ 44: } 50,
{ 45: } 50,
{ 46: } 50,
{ 47: } 50,
{ 48: } 50,
{ 49: } 50,
{ 50: } 50,
{ 51: } 50,
{ 52: } 50,
{ 53: } 51,
{ 54: } 58,
{ 55: } 58,
{ 56: } 58,
{ 57: } 59,
{ 58: } 62,
{ 59: } 62,
{ 60: } 62,
{ 61: } 62,
{ 62: } 62,
{ 63: } 62,
{ 64: } 62,
{ 65: } 62,
{ 66: } 62,
{ 67: } 66,
{ 68: } 67,
{ 69: } 67,
{ 70: } 69,
{ 71: } 72,
{ 72: } 72,
{ 73: } 76,
{ 74: } 76,
{ 75: } 76,
{ 76: } 76,
{ 77: } 76,
{ 78: } 77,
{ 79: } 78,
{ 80: } 79,
{ 81: } 80,
{ 82: } 83,
{ 83: } 86,
{ 84: } 86,
{ 85: } 86,
{ 86: } 86,
{ 87: } 89,
{ 88: } 92,
{ 89: } 92,
{ 90: } 92,
{ 91: } 99,
{ 92: } 99,
{ 93: } 103,
{ 94: } 103,
{ 95: } 103,
{ 96: } 103,
{ 97: } 103,
{ 98: } 103,
{ 99: } 103,
{ 100: } 103,
{ 101: } 106,
{ 102: } 106,
{ 103: } 106,
{ 104: } 106,
{ 105: } 106,
{ 106: } 106,
{ 107: } 106,
{ 108: } 114,
{ 109: } 114,
{ 110: } 114,
{ 111: } 114,
{ 112: } 117,
{ 113: } 120,
{ 114: } 123,
{ 115: } 123,
{ 116: } 123,
{ 117: } 126,
{ 118: } 126,
{ 119: } 133,
{ 120: } 138,
{ 121: } 138,
{ 122: } 141,
{ 123: } 141,
{ 124: } 146,
{ 125: } 151,
{ 126: } 151,
{ 127: } 152,
{ 128: } 153,
{ 129: } 154,
{ 130: } 155,
{ 131: } 155,
{ 132: } 155,
{ 133: } 155,
{ 134: } 155,
{ 135: } 155,
{ 136: } 158,
{ 137: } 158,
{ 138: } 163,
{ 139: } 163,
{ 140: } 164,
{ 141: } 164,
{ 142: } 166,
{ 143: } 171,
{ 144: } 176,
{ 145: } 176,
{ 146: } 183,
{ 147: } 190,
{ 148: } 190,
{ 149: } 190,
{ 150: } 190,
{ 151: } 190,
{ 152: } 191,
{ 153: } 191,
{ 154: } 194,
{ 155: } 194,
{ 156: } 194,
{ 157: } 194,
{ 158: } 195,
{ 159: } 195,
{ 160: } 195,
{ 161: } 199,
{ 162: } 199,
{ 163: } 199,
{ 164: } 199,
{ 165: } 199,
{ 166: } 199,
{ 167: } 199,
{ 168: } 199,
{ 169: } 200,
{ 170: } 201,
{ 171: } 201,
{ 172: } 201,
{ 173: } 205,
{ 174: } 205,
{ 175: } 205,
{ 176: } 205,
{ 177: } 205,
{ 178: } 206,
{ 179: } 206,
{ 180: } 206,
{ 181: } 210,
{ 182: } 210,
{ 183: } 210,
{ 184: } 210,
{ 185: } 210,
{ 186: } 210,
{ 187: } 210,
{ 188: } 210,
{ 189: } 215,
{ 190: } 220,
{ 191: } 225,
{ 192: } 230,
{ 193: } 235,
{ 194: } 240,
{ 195: } 246,
{ 196: } 251,
{ 197: } 256,
{ 198: } 261,
{ 199: } 266,
{ 200: } 271,
{ 201: } 276,
{ 202: } 281,
{ 203: } 286,
{ 204: } 291,
{ 205: } 291,
{ 206: } 294,
{ 207: } 294,
{ 208: } 298,
{ 209: } 298,
{ 210: } 305,
{ 211: } 305,
{ 212: } 305,
{ 213: } 306,
{ 214: } 307,
{ 215: } 311,
{ 216: } 315,
{ 217: } 319,
{ 218: } 319,
{ 219: } 319,
{ 220: } 319,
{ 221: } 319,
{ 222: } 319,
{ 223: } 319,
{ 224: } 319,
{ 225: } 323,
{ 226: } 323,
{ 227: } 330,
{ 228: } 330,
{ 229: } 330,
{ 230: } 330,
{ 231: } 330,
{ 232: } 330,
{ 233: } 330,
{ 234: } 330,
{ 235: } 330,
{ 236: } 330,
{ 237: } 330,
{ 238: } 330,
{ 239: } 330,
{ 240: } 330,
{ 241: } 330,
{ 242: } 330,
{ 243: } 330,
{ 244: } 330,
{ 245: } 330,
{ 246: } 330,
{ 247: } 330,
{ 248: } 330,
{ 249: } 333,
{ 250: } 333,
{ 251: } 333,
{ 252: } 334,
{ 253: } 334,
{ 254: } 338,
{ 255: } 338,
{ 256: } 345,
{ 257: } 350,
{ 258: } 355,
{ 259: } 356,
{ 260: } 357,
{ 261: } 361,
{ 262: } 362,
{ 263: } 363,
{ 264: } 364,
{ 265: } 365,
{ 266: } 365,
{ 267: } 372,
{ 268: } 372,
{ 269: } 372,
{ 270: } 377,
{ 271: } 380,
{ 272: } 380,
{ 273: } 380,
{ 274: } 381,
{ 275: } 382,
{ 276: } 382,
{ 277: } 382,
{ 278: } 382,
{ 279: } 382,
{ 280: } 383,
{ 281: } 383,
{ 282: } 383,
{ 283: } 383,
{ 284: } 383,
{ 285: } 390,
{ 286: } 390,
{ 287: } 390,
{ 288: } 390,
{ 289: } 390,
{ 290: } 390
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
{ 25: } 33,
{ 26: } 33,
{ 27: } 33,
{ 28: } 33,
{ 29: } 33,
{ 30: } 33,
{ 31: } 38,
{ 32: } 39,
{ 33: } 39,
{ 34: } 39,
{ 35: } 39,
{ 36: } 40,
{ 37: } 40,
{ 38: } 42,
{ 39: } 42,
{ 40: } 43,
{ 41: } 44,
{ 42: } 45,
{ 43: } 49,
{ 44: } 49,
{ 45: } 49,
{ 46: } 49,
{ 47: } 49,
{ 48: } 49,
{ 49: } 49,
{ 50: } 49,
{ 51: } 49,
{ 52: } 50,
{ 53: } 57,
{ 54: } 57,
{ 55: } 57,
{ 56: } 58,
{ 57: } 61,
{ 58: } 61,
{ 59: } 61,
{ 60: } 61,
{ 61: } 61,
{ 62: } 61,
{ 63: } 61,
{ 64: } 61,
{ 65: } 61,
{ 66: } 65,
{ 67: } 66,
{ 68: } 66,
{ 69: } 68,
{ 70: } 71,
{ 71: } 71,
{ 72: } 75,
{ 73: } 75,
{ 74: } 75,
{ 75: } 75,
{ 76: } 75,
{ 77: } 76,
{ 78: } 77,
{ 79: } 78,
{ 80: } 79,
{ 81: } 82,
{ 82: } 85,
{ 83: } 85,
{ 84: } 85,
{ 85: } 85,
{ 86: } 88,
{ 87: } 91,
{ 88: } 91,
{ 89: } 91,
{ 90: } 98,
{ 91: } 98,
{ 92: } 102,
{ 93: } 102,
{ 94: } 102,
{ 95: } 102,
{ 96: } 102,
{ 97: } 102,
{ 98: } 102,
{ 99: } 102,
{ 100: } 105,
{ 101: } 105,
{ 102: } 105,
{ 103: } 105,
{ 104: } 105,
{ 105: } 105,
{ 106: } 105,
{ 107: } 113,
{ 108: } 113,
{ 109: } 113,
{ 110: } 113,
{ 111: } 116,
{ 112: } 119,
{ 113: } 122,
{ 114: } 122,
{ 115: } 122,
{ 116: } 125,
{ 117: } 125,
{ 118: } 132,
{ 119: } 137,
{ 120: } 137,
{ 121: } 140,
{ 122: } 140,
{ 123: } 145,
{ 124: } 150,
{ 125: } 150,
{ 126: } 151,
{ 127: } 152,
{ 128: } 153,
{ 129: } 154,
{ 130: } 154,
{ 131: } 154,
{ 132: } 154,
{ 133: } 154,
{ 134: } 154,
{ 135: } 157,
{ 136: } 157,
{ 137: } 162,
{ 138: } 162,
{ 139: } 163,
{ 140: } 163,
{ 141: } 165,
{ 142: } 170,
{ 143: } 175,
{ 144: } 175,
{ 145: } 182,
{ 146: } 189,
{ 147: } 189,
{ 148: } 189,
{ 149: } 189,
{ 150: } 189,
{ 151: } 190,
{ 152: } 190,
{ 153: } 193,
{ 154: } 193,
{ 155: } 193,
{ 156: } 193,
{ 157: } 194,
{ 158: } 194,
{ 159: } 194,
{ 160: } 198,
{ 161: } 198,
{ 162: } 198,
{ 163: } 198,
{ 164: } 198,
{ 165: } 198,
{ 166: } 198,
{ 167: } 198,
{ 168: } 199,
{ 169: } 200,
{ 170: } 200,
{ 171: } 200,
{ 172: } 204,
{ 173: } 204,
{ 174: } 204,
{ 175: } 204,
{ 176: } 204,
{ 177: } 205,
{ 178: } 205,
{ 179: } 205,
{ 180: } 209,
{ 181: } 209,
{ 182: } 209,
{ 183: } 209,
{ 184: } 209,
{ 185: } 209,
{ 186: } 209,
{ 187: } 209,
{ 188: } 214,
{ 189: } 219,
{ 190: } 224,
{ 191: } 229,
{ 192: } 234,
{ 193: } 239,
{ 194: } 245,
{ 195: } 250,
{ 196: } 255,
{ 197: } 260,
{ 198: } 265,
{ 199: } 270,
{ 200: } 275,
{ 201: } 280,
{ 202: } 285,
{ 203: } 290,
{ 204: } 290,
{ 205: } 293,
{ 206: } 293,
{ 207: } 297,
{ 208: } 297,
{ 209: } 304,
{ 210: } 304,
{ 211: } 304,
{ 212: } 305,
{ 213: } 306,
{ 214: } 310,
{ 215: } 314,
{ 216: } 318,
{ 217: } 318,
{ 218: } 318,
{ 219: } 318,
{ 220: } 318,
{ 221: } 318,
{ 222: } 318,
{ 223: } 318,
{ 224: } 322,
{ 225: } 322,
{ 226: } 329,
{ 227: } 329,
{ 228: } 329,
{ 229: } 329,
{ 230: } 329,
{ 231: } 329,
{ 232: } 329,
{ 233: } 329,
{ 234: } 329,
{ 235: } 329,
{ 236: } 329,
{ 237: } 329,
{ 238: } 329,
{ 239: } 329,
{ 240: } 329,
{ 241: } 329,
{ 242: } 329,
{ 243: } 329,
{ 244: } 329,
{ 245: } 329,
{ 246: } 329,
{ 247: } 329,
{ 248: } 332,
{ 249: } 332,
{ 250: } 332,
{ 251: } 333,
{ 252: } 333,
{ 253: } 337,
{ 254: } 337,
{ 255: } 344,
{ 256: } 349,
{ 257: } 354,
{ 258: } 355,
{ 259: } 356,
{ 260: } 360,
{ 261: } 361,
{ 262: } 362,
{ 263: } 363,
{ 264: } 364,
{ 265: } 364,
{ 266: } 371,
{ 267: } 371,
{ 268: } 371,
{ 269: } 376,
{ 270: } 379,
{ 271: } 379,
{ 272: } 379,
{ 273: } 380,
{ 274: } 381,
{ 275: } 381,
{ 276: } 381,
{ 277: } 381,
{ 278: } 381,
{ 279: } 382,
{ 280: } 382,
{ 281: } 382,
{ 282: } 382,
{ 283: } 382,
{ 284: } 389,
{ 285: } 389,
{ 286: } 389,
{ 287: } 389,
{ 288: } 389,
{ 289: } 389,
{ 290: } 389
);

yyr : array [1..yynrules] of YYRRec = (
{ 1: } ( len: 1; sym: -2 ),
{ 2: } ( len: 1; sym: -4 ),
{ 3: } ( len: 0; sym: -4 ),
{ 4: } ( len: 0; sym: -5 ),
{ 5: } ( len: 2; sym: -3 ),
{ 6: } ( len: 2; sym: -3 ),
{ 7: } ( len: 1; sym: -3 ),
{ 8: } ( len: 1; sym: -3 ),
{ 9: } ( len: 1; sym: -8 ),
{ 10: } ( len: 0; sym: -8 ),
{ 11: } ( len: 1; sym: -9 ),
{ 12: } ( len: 1; sym: -9 ),
{ 13: } ( len: 1; sym: -9 ),
{ 14: } ( len: 1; sym: -9 ),
{ 15: } ( len: 1; sym: -9 ),
{ 16: } ( len: 1; sym: -9 ),
{ 17: } ( len: 1; sym: -9 ),
{ 18: } ( len: 0; sym: -9 ),
{ 19: } ( len: 4; sym: -10 ),
{ 20: } ( len: 0; sym: -10 ),
{ 21: } ( len: 6; sym: -6 ),
{ 22: } ( len: 2; sym: -6 ),
{ 23: } ( len: 5; sym: -6 ),
{ 24: } ( len: 11; sym: -6 ),
{ 25: } ( len: 5; sym: -6 ),
{ 26: } ( len: 3; sym: -6 ),
{ 27: } ( len: 3; sym: -6 ),
{ 28: } ( len: 7; sym: -7 ),
{ 29: } ( len: 4; sym: -7 ),
{ 30: } ( len: 3; sym: -7 ),
{ 31: } ( len: 5; sym: -7 ),
{ 32: } ( len: 3; sym: -7 ),
{ 33: } ( len: 3; sym: -20 ),
{ 34: } ( len: 3; sym: -20 ),
{ 35: } ( len: 3; sym: -22 ),
{ 36: } ( len: 3; sym: -22 ),
{ 37: } ( len: 4; sym: -14 ),
{ 38: } ( len: 3; sym: -14 ),
{ 39: } ( len: 4; sym: -14 ),
{ 40: } ( len: 3; sym: -14 ),
{ 41: } ( len: 2; sym: -14 ),
{ 42: } ( len: 2; sym: -14 ),
{ 43: } ( len: 3; sym: -14 ),
{ 44: } ( len: 2; sym: -14 ),
{ 45: } ( len: 2; sym: -12 ),
{ 46: } ( len: 3; sym: -12 ),
{ 47: } ( len: 2; sym: -12 ),
{ 48: } ( len: 3; sym: -12 ),
{ 49: } ( len: 2; sym: -12 ),
{ 50: } ( len: 2; sym: -12 ),
{ 51: } ( len: 1; sym: -12 ),
{ 52: } ( len: 1; sym: -12 ),
{ 53: } ( len: 2; sym: -21 ),
{ 54: } ( len: 1; sym: -21 ),
{ 55: } ( len: 3; sym: -24 ),
{ 56: } ( len: 1; sym: -11 ),
{ 57: } ( len: 2; sym: -25 ),
{ 58: } ( len: 2; sym: -25 ),
{ 59: } ( len: 1; sym: -25 ),
{ 60: } ( len: 1; sym: -25 ),
{ 61: } ( len: 2; sym: -25 ),
{ 62: } ( len: 2; sym: -25 ),
{ 63: } ( len: 3; sym: -25 ),
{ 64: } ( len: 1; sym: -25 ),
{ 65: } ( len: 2; sym: -25 ),
{ 66: } ( len: 1; sym: -25 ),
{ 67: } ( len: 1; sym: -25 ),
{ 68: } ( len: 1; sym: -25 ),
{ 69: } ( len: 1; sym: -25 ),
{ 70: } ( len: 1; sym: -23 ),
{ 71: } ( len: 1; sym: -23 ),
{ 72: } ( len: 3; sym: -13 ),
{ 73: } ( len: 4; sym: -13 ),
{ 74: } ( len: 2; sym: -13 ),
{ 75: } ( len: 1; sym: -13 ),
{ 76: } ( len: 2; sym: -26 ),
{ 77: } ( len: 3; sym: -26 ),
{ 78: } ( len: 2; sym: -26 ),
{ 79: } ( len: 1; sym: -16 ),
{ 80: } ( len: 3; sym: -16 ),
{ 81: } ( len: 1; sym: -16 ),
{ 82: } ( len: 0; sym: -16 ),
{ 83: } ( len: 1; sym: -28 ),
{ 84: } ( len: 1; sym: -28 ),
{ 85: } ( len: 1; sym: -28 ),
{ 86: } ( len: 2; sym: -15 ),
{ 87: } ( len: 3; sym: -15 ),
{ 88: } ( len: 2; sym: -15 ),
{ 89: } ( len: 2; sym: -15 ),
{ 90: } ( len: 3; sym: -15 ),
{ 91: } ( len: 3; sym: -15 ),
{ 92: } ( len: 1; sym: -15 ),
{ 93: } ( len: 4; sym: -15 ),
{ 94: } ( len: 2; sym: -15 ),
{ 95: } ( len: 4; sym: -15 ),
{ 96: } ( len: 3; sym: -15 ),
{ 97: } ( len: 3; sym: -15 ),
{ 98: } ( len: 2; sym: -31 ),
{ 99: } ( len: 3; sym: -31 ),
{ 100: } ( len: 2; sym: -27 ),
{ 101: } ( len: 3; sym: -27 ),
{ 102: } ( len: 2; sym: -27 ),
{ 103: } ( len: 4; sym: -27 ),
{ 104: } ( len: 2; sym: -27 ),
{ 105: } ( len: 4; sym: -27 ),
{ 106: } ( len: 3; sym: -27 ),
{ 107: } ( len: 3; sym: -27 ),
{ 108: } ( len: 0; sym: -27 ),
{ 109: } ( len: 1; sym: -29 ),
{ 110: } ( len: 3; sym: -32 ),
{ 111: } ( len: 3; sym: -32 ),
{ 112: } ( len: 3; sym: -32 ),
{ 113: } ( len: 3; sym: -32 ),
{ 114: } ( len: 3; sym: -32 ),
{ 115: } ( len: 3; sym: -32 ),
{ 116: } ( len: 3; sym: -32 ),
{ 117: } ( len: 3; sym: -32 ),
{ 118: } ( len: 3; sym: -32 ),
{ 119: } ( len: 3; sym: -32 ),
{ 120: } ( len: 3; sym: -32 ),
{ 121: } ( len: 3; sym: -32 ),
{ 122: } ( len: 3; sym: -32 ),
{ 123: } ( len: 3; sym: -32 ),
{ 124: } ( len: 3; sym: -32 ),
{ 125: } ( len: 3; sym: -32 ),
{ 126: } ( len: 1; sym: -32 ),
{ 127: } ( len: 3; sym: -33 ),
{ 128: } ( len: 1; sym: -35 ),
{ 129: } ( len: 0; sym: -35 ),
{ 130: } ( len: 1; sym: -34 ),
{ 131: } ( len: 1; sym: -34 ),
{ 132: } ( len: 1; sym: -34 ),
{ 133: } ( len: 1; sym: -34 ),
{ 134: } ( len: 3; sym: -34 ),
{ 135: } ( len: 3; sym: -34 ),
{ 136: } ( len: 2; sym: -34 ),
{ 137: } ( len: 2; sym: -34 ),
{ 138: } ( len: 2; sym: -34 ),
{ 139: } ( len: 4; sym: -34 ),
{ 140: } ( len: 4; sym: -34 ),
{ 141: } ( len: 5; sym: -34 ),
{ 142: } ( len: 6; sym: -34 ),
{ 143: } ( len: 4; sym: -34 ),
{ 144: } ( len: 3; sym: -34 ),
{ 145: } ( len: 8; sym: -34 ),
{ 146: } ( len: 4; sym: -34 ),
{ 147: } ( len: 3; sym: -17 ),
{ 148: } ( len: 1; sym: -17 ),
{ 149: } ( len: 0; sym: -17 ),
{ 150: } ( len: 3; sym: -37 ),
{ 151: } ( len: 1; sym: -37 ),
{ 152: } ( len: 1; sym: -19 ),
{ 153: } ( len: 2; sym: -18 ),
{ 154: } ( len: 4; sym: -18 ),
{ 155: } ( len: 3; sym: -36 ),
{ 156: } ( len: 1; sym: -36 ),
{ 157: } ( len: 0; sym: -36 ),
{ 158: } ( len: 1; sym: -38 )
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

procedure WriteFileHeader(var headerfile: Text);
var
 i: integer;
 originalstr: string;
begin
{ write unit header }
  if not includefile then
   begin
     if createdynlib then
       writeln(headerfile,'{$mode objfpc}');
     writeln(headerfile,'unit ',unitname,';');
     writeln(headerfile,'interface');
     writeln(headerfile);
     writeln(headerfile,'{');
     writeln(headerfile,'  Automatically converted by H2Pas ',version,' from ',inputfilename);
     writeln(headerfile,'  The following command line parameters were used:');
     for i:=1 to paramcount do
       writeln(headerfile,'    ',paramstr(i));
     writeln(headerfile,'}');
     writeln(headerfile);
   end;
  if UseName then
   begin
     writeln(headerfile,aktspace,'const');
     writeln(headerfile,aktspace,'  External_library=''',libfilename,'''; {Setup as you need}');
     writeln(headerfile);
   end;
  if UsePPointers then
   begin
     Writeln(headerfile,aktspace,'{ Pointers to basic pascal types, inserted by h2pas conversion program.}');
     Writeln(headerfile,aktspace,'Type');
     Writeln(headerfile,aktspace,'  PLongint  = ^Longint;');
     Writeln(headerfile,aktspace,'  PSmallInt = ^SmallInt;');
     Writeln(headerfile,aktspace,'  PByte     = ^Byte;');
     Writeln(headerfile,aktspace,'  PWord     = ^Word;');
     Writeln(headerfile,aktspace,'  PDWord    = ^DWord;');
     Writeln(headerfile,aktspace,'  PDouble   = ^Double;');
     Writeln(headerfile);
   end;
  if PTypeList.count <> 0 then
   Writeln(headerfile,aktspace,'Type');
  for i:=0 to (PTypeList.Count-1) do
   begin
     originalstr:=copy(PTypelist[i],2,length(PTypeList[i]));
     Writeln(headerfile,aktspace,PTypeList[i],'  = ^',originalstr,';');
   end;
  if not packrecords then
   begin
      writeln(headerfile,'{$IFDEF FPC}');
      writeln(headerfile,'{$PACKRECORDS C}');
      writeln(headerfile,'{$ENDIF}');
   end;
  writeln(headerfile);
end;


var
  SS : string;
  i : longint;
  headerfile: Text;
  finaloutfile: Text;
begin
  pointerprefix:=false;
{ Initialize }
  PTypeList:=TStringList.Create;
  PTypeList.Sorted := true;
  PTypeList.Duplicates := dupIgnore;
  freedynlibproc:=TStringList.Create;
  loaddynlibproc:=TStringList.Create;
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
  { This is the intermediate output file }
  assign(outfile, 'ext3.tmp');
  {$I-}
  rewrite(outfile);
  {$I+}
  if ioresult<>0 then
   begin
     writeln('file ext3.tmp could not be created!');
     halt(1);
   end;
  writeln(outfile);
{ Open tempfiles }
  { This is where the implementation section of the unit shall be stored }
  Assign(implemfile,'ext.tmp');
  rewrite(implemfile);
  Assign(tempfile,'ext2.tmp');
  rewrite(tempfile);
{ Parse! }
  yyparse;
{ Write implementation if needed }
   if not(includefile) then
    begin
      writeln(outfile);
      writeln(outfile,'implementation');
      writeln(outfile);
    end;
   { here we have a problem if a line is longer than 255 chars !! }
   reset(implemfile);
   while not eof(implemfile) do
    begin
      readln(implemfile,SS);
      writeln(outfile,SS);
    end;

  if createdynlib then
    begin
      writeln(outfile,'  uses');
      writeln(outfile,'    SysUtils,');
      writeln(outfile,'{$ifdef Win32}');
      writeln(outfile,'    Windows;');
      writeln(outfile,'{$else}');
      writeln(outfile,'    DLLFuncs;');
      writeln(outfile,'{$endif win32}');
      writeln(outfile);
      writeln(outfile,'  var');
      writeln(outfile,'    hlib : thandle;');
      writeln(outfile);
      writeln(outfile);
      writeln(outfile,'  procedure Free',unitname,';');
      writeln(outfile,'    begin');
      writeln(outfile,'      FreeLibrary(hlib);');

      for i:=0 to (freedynlibproc.Count-1) do
        Writeln(outfile,'      ',freedynlibproc[i]);

      writeln(outfile,'    end;');
      writeln(outfile);
      writeln(outfile);
      writeln(outfile,'  procedure Load',unitname,'(lib : pchar);');
      writeln(outfile,'    begin');
      writeln(outfile,'      Free',unitname,';');
      writeln(outfile,'      hlib:=LoadLibrary(lib);');
      writeln(outfile,'      if hlib=0 then');
      writeln(outfile,'        raise Exception.Create(format(''Could not load library: %s'',[lib]));');
      writeln(outfile);
      for i:=0 to (loaddynlibproc.Count-1) do
        Writeln(outfile,'      ',loaddynlibproc[i]);
      writeln(outfile,'    end;');

      writeln(outfile);
      writeln(outfile);

      writeln(outfile,'initialization');
      writeln(outfile,'  Load',unitname,'(''',unitname,''');');
      writeln(outfile,'finalization');
      writeln(outfile,'  Free',unitname,';');
    end;

   { write end of file }
   writeln(outfile);
   if not(includefile) then
     writeln(outfile,'end.');
   { close and erase tempfiles }
  close(implemfile);
  erase(implemfile);
  close(tempfile);
  erase(tempfile);
  flush(outfile);

  {**** generate full file ****}
  assign(headerfile, 'ext4.tmp');
  {$I-}
  rewrite(headerfile);
  {$I+}
  if ioresult<>0 then
    begin
      writeln('file ext4.tmp could not be created!');
      halt(1);
  end;
  WriteFileHeader(HeaderFile);

  { Final output filename }
  assign(finaloutfile, outputfilename);
  {$I-}
  rewrite(finaloutfile);
  {$I+}
  if ioresult<>0 then
  begin
     writeln('file ',outputfilename,' could not be created!');
     halt(1);
  end;
  writeln(finaloutfile);

  { Read unit header file }
  reset(headerfile);
  while not eof(headerfile) do
    begin
      readln(headerfile,SS);
      writeln(finaloutfile,SS);
    end;
  { Read interface and implementation file }
  reset(outfile);
  while not eof(outfile) do
    begin
      readln(outfile,SS);
      writeln(finaloutfile,SS);
    end;

  close(HeaderFile);
  close(outfile);
  close(finaloutfile);
  erase(outfile);
  erase(headerfile);

  PTypeList.Free;
  freedynlibproc.free;
  loaddynlibproc.free;
end.
