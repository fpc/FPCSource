
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
{$ifdef Delphi}
     SysUtils,
{$else Delphi}
     strings,
{$endif Delphi}
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
     extfile  : text;  (* file for implementation headers extern procs *)
     IsExtern : boolean;
     NeedEllipsisOverload : boolean;
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


    function FixId(const s:string):string;
      var
        b : boolean;
        up : string;
      begin
        if s='' then
         begin
           FixId:='';
           exit;
         end;
        b:=false;
        up:=Uppercase(s);
        case up[1] of
          'C' : b:=(up='CLASS');
          'D' : b:=(up='DISPOSE');
          'F' : b:=(up='FUNCTION') or (up='FALSE');
          'N' : b:=(up='NEW');
          'P' : b:=(up='PROPERTY') or (up='PROCEDURE');
          'R' : b:=(up='RECORD');
          'S' : b:=(up='STRING');
          'T' : b:=(up='TYPE') or (up='TRUE');
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
         PointerName:='P'+Copy(s,i,255)
        else
         PointerName:=Copy(s,i,255);
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
                        if not compactmode then
                         shift(2);
                        write_p_a_def(extfile,hp3^.p1^.p1,hp2^.p1);
                        writeln(extfile,';');
                        writeln(extfile,aktspace,'begin');
                        shift(3);
                        write(extfile,aktspace,name,':=(a.flag',flag_index);
                        writeln(extfile,' and bm_',ph,'_',name,') shr bp_',ph,'_',name,';');
                        popshift;
                        writeln(extfile,aktspace,'end;');
                        if not compactmode then
                         popshift;
                        writeln(extfile,'');
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
                        if not compactmode then
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
                        if not compactmode then
                         popshift;
                        writeln(extfile,'');
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
         length,para : longint;
         old_in_args : boolean;
         varpara : boolean;
         lastp : presobject;
      begin
         NeedEllipsisOverload:=false;
         para:=1;
         length:=0;
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
                   varpara:=usevarparas and
                            assigned(p^.p1^.p2^.p1) and
                            (p^.p1^.p2^.p1^.typ in [t_addrdef,t_pointerdef]) and
                            assigned(p^.p1^.p2^.p1^.p1) and
                            (p^.p1^.p2^.p1^.p1^.typ<>t_procdef);
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
                           inc(Length,5);
                         end
                       else
                         begin
                           write(outfile,'_para',para);
                           inc(Length,6);
                         end;
                     end;
                   write(outfile,':');
                   if varpara then
                     write_p_a_def(outfile,p^.p1^.p2^.p1^.p1,p^.p1^.p1)
                   else
                     write_p_a_def(outfile,p^.p1^.p2^.p1,p^.p1^.p1);

                end;
              lastp:=p;
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
                                          write(outfile,'P')
                                         else
                                          write(outfile,'^');
                                         write_p_a_def(outfile,p^.p1,simple_type);
                                       end;
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
                     write(outfile,'P')
                    else
                     write(outfile,'^');
                    write_type_specifier(outfile,p^.p1);
                  end;
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
                 if ((in_args) or (typedef_level>1)) and
                    (p^.p1=nil) and (p^.p2^.typ=t_id) then
                   begin
                     write(outfile,TypeName(p^.p2^.p));
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
         writeln(outfile,';');
         if not IsExtern then
         begin
         writeln(extfile,';');
         writeln(extfile,aktspace,'begin');
         writeln(extfile,aktspace,'  { You must implemented this function }');
         writeln(extfile,aktspace,'end;');
         end;
         end;
         IsExtern:=false;
         if not compactmode then
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
         dispose(hp,done);

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
         writeln (extfile,aktspace,'{ was #define dname(params) para_def_expr }');
         if assigned(yyv[yysp-3]) then
         begin
         writeln (outfile,aktspace,'{ argument types are unknown }');
         writeln (extfile,aktspace,'{ argument types are unknown }');
         end;
         if not assigned(yyv[yysp-1]^.p3) then
         begin
         writeln(outfile,aktspace,'{ return type might be wrong }   ');
         writeln(extfile,aktspace,'{ return type might be wrong }   ');
         end;
         end;
         block_type:=bt_func;
         write(outfile,aktspace,'function ',yyv[yysp-5]^.p);
         write(extfile,aktspace,'function ',yyv[yysp-5]^.p);

         if assigned(yyv[yysp-3]) then
         begin
         write(outfile,'(');
         write(extfile,'(');
         ph:=new(presobject,init_one(t_enumdef,yyv[yysp-3]));
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
         writeln(extfile,' : longint;');
         flush(outfile);
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
         hp:=new(presobject,init_two(t_funcname,yyv[yysp-5],yyv[yysp-1]));
         write_funexpr(extfile,hp);
         writeln(extfile);
         flush(extfile);
         if assigned(hp)then dispose(hp,done);

       end;
  29 : begin

         (* DEFINE dname SPACE_DEFINE NEW_LINE *)
         writeln(outfile,'{$define ',yyv[yysp-2]^.p,'}');
         flush(outfile);
         if assigned(yyv[yysp-2])then
         dispose(yyv[yysp-2],done);

       end;
  30 : begin

         writeln(outfile,'{$define ',yyv[yysp-1]^.p,'}');
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
         writeln(outfile,';');
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
         writeln (extfile,aktspace,'{ was #define dname def_expr }');
         end;
         block_type:=bt_func;
         write(outfile,aktspace,'function ',yyv[yysp-3]^.p);
         write(extfile,aktspace,'function ',yyv[yysp-3]^.p);
         shift(2);
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
         popshift;
         dispose(hp,done);
         writeln(extfile);
         flush(extfile);
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

         if not is_packed then
         writeln(outfile,'{$PACKRECORDS 1}');
         is_packed:=true;
         yyval:=new(presobject,init_two(t_structdef,yyv[yysp-1],yyv[yysp-2]));

       end;
  38 : begin

         if is_packed then
         writeln(outfile,'{$PACKRECORDS 4}');
         is_packed:=false;
         yyval:=new(presobject,init_two(t_structdef,yyv[yysp-0],yyv[yysp-1]));

       end;
  39 : begin

         if not is_packed then
         writeln(outfile,'{$PACKRECORDS 1}');
         is_packed:=true;
         yyval:=new(presobject,init_two(t_uniondef,yyv[yysp-1],yyv[yysp-2]));

       end;
  40 : begin

         yyval:=new(presobject,init_two(t_uniondef,yyv[yysp-0],yyv[yysp-1]));

       end;
  41 : begin

         yyval:=new(presobject,init_two(t_uniondef,nil,yyv[yysp-0]));

       end;
  42 : begin

         yyval:=new(presobject,init_two(t_structdef,nil,yyv[yysp-0]));

       end;
  43 : begin

         yyval:=new(presobject,init_two(t_enumdef,yyv[yysp-0],yyv[yysp-1]));

       end;
  44 : begin

         yyval:=new(presobject,init_two(t_enumdef,nil,yyv[yysp-0]));

       end;
  45 : begin

         if not stripinfo then
         writeln(outfile,'(* Const before type ignored *)');
         yyval:=yyv[yysp-0];

       end;
  46 : begin

         if not is_packed then
         writeln(outfile,'{$PACKRECORDS 1}');
         is_packed:=true;
         yyval:=new(presobject,init_one(t_uniondef,yyv[yysp-1]));

       end;
  47 : begin

         yyval:=new(presobject,init_one(t_uniondef,yyv[yysp-0]));

       end;
  48 : begin

         if not is_packed then
         writeln(outfile,'{$PACKRECORDS 1}');
         is_packed:=true;
         yyval:=new(presobject,init_one(t_structdef,yyv[yysp-1]));

       end;
  49 : begin

         if is_packed then
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
         yyval:=new(presobject,init_id('far'));
       end;
  83 : begin
         yyval:=new(presobject,init_id('near'));
       end;
  84 : begin
         yyval:=new(presobject,init_id('huge'));
       end;
  85 : begin

         if not stripinfo then
         writeln(outfile,'(* Const before declarator ignored *)');
         yyval:=yyv[yysp-0];

       end;
  86 : begin

         if not stripinfo then
         writeln(outfile,aktspace,'(* ',yyv[yysp-2]^.p,' ignored *)');
         dispose(yyv[yysp-2],done);
         hp:=yyv[yysp-0];
         yyval:=hp;
         while assigned(hp^.p1) do
         hp:=hp^.p1;
         hp^.p1:=new(presobject,init_one(t_pointerdef,nil));

       end;
  87 : begin

         (* %prec PSTAR this was wrong!! *)
         hp:=yyv[yysp-0];
         yyval:=hp;
         while assigned(hp^.p1) do
         hp:=hp^.p1;
         hp^.p1:=new(presobject,init_one(t_pointerdef,nil));

       end;
  88 : begin

         hp:=yyv[yysp-0];
         yyval:=hp;
         while assigned(hp^.p1) do
         hp:=hp^.p1;
         hp^.p1:=new(presobject,init_one(t_addrdef,nil));

       end;
  89 : begin

         (*  size specifier supported *)
         hp:=new(presobject,init_one(t_size_specifier,yyv[yysp-0]));
         yyval:=new(presobject,init_three(t_dec,nil,yyv[yysp-2],hp));

       end;
  90 : begin

         if not stripinfo then
         writeln(outfile,'(* Warning : default value for ',yyv[yysp-2]^.p,' ignored *)');
         hp:=new(presobject,init_one(t_default_value,yyv[yysp-0]));
         yyval:=new(presobject,init_three(t_dec,nil,yyv[yysp-2],hp));

       end;
  91 : begin

         yyval:=new(presobject,init_two(t_dec,nil,yyv[yysp-0]));

       end;
  92 : begin

         hp:=yyv[yysp-3];
         yyval:=hp;
         while assigned(hp^.p1) do
         hp:=hp^.p1;
         hp^.p1:=new(presobject,init_two(t_procdef,nil,yyv[yysp-1]));

       end;
  93 : begin

         hp:=yyv[yysp-1];
         yyval:=hp;
         while assigned(hp^.p1) do
         hp:=hp^.p1;
         hp^.p1:=new(presobject,init_two(t_procdef,nil,nil));

       end;
  94 : begin

         hp:=yyv[yysp-3];
         yyval:=hp;
         while assigned(hp^.p1) do
         hp:=hp^.p1;
         hp^.p1:=new(presobject,init_two(t_arraydef,nil,yyv[yysp-1]));

       end;
  95 : begin
         yyval:=yyv[yysp-1];
       end;
  96 : begin
         yyval := yyv[yysp-1];
       end;
  97 : begin
         yyval := yyv[yysp-2];
       end;
  98 : begin

         if not stripinfo then
         writeln(outfile,'(* Const before abstract_declarator ignored *)');
         yyval:=yyv[yysp-0];

       end;
  99 : begin

         if not stripinfo then
         writeln(outfile,aktspace,'(* ',yyv[yysp-2]^.p,' ignored *)');
         dispose(yyv[yysp-2],done);
         hp:=yyv[yysp-0];
         yyval:=hp;
         while assigned(hp^.p1) do
         hp:=hp^.p1;
         hp^.p1:=new(presobject,init_one(t_pointerdef,nil));

       end;
 100 : begin

         hp:=yyv[yysp-0];
         yyval:=hp;
         while assigned(hp^.p1) do
         hp:=hp^.p1;
         hp^.p1:=new(presobject,init_one(t_pointerdef,nil));

       end;
 101 : begin

         hp:=yyv[yysp-3];
         yyval:=hp;
         while assigned(hp^.p1) do
         hp:=hp^.p1;
         hp^.p1:=new(presobject,init_two(t_procdef,nil,yyv[yysp-1]));

       end;
 102 : begin

         hp:=yyv[yysp-1];
         yyval:=hp;
         while assigned(hp^.p1) do
         hp:=hp^.p1;
         hp^.p1:=new(presobject,init_two(t_procdef,nil,nil));

       end;
 103 : begin

         hp:=yyv[yysp-3];
         yyval:=hp;
         while assigned(hp^.p1) do
         hp:=hp^.p1;
         hp^.p1:=new(presobject,init_two(t_arraydef,nil,yyv[yysp-1]));

       end;
 104 : begin
         yyval:=yyv[yysp-1];
       end;
 105 : begin

         yyval:=new(presobject,init_two(t_dec,nil,nil));

       end;
 106 : begin
         yyval:=yyv[yysp-0];
       end;
 107 : begin
         yyval:=new(presobject,init_bop(' = ',yyv[yysp-2],yyv[yysp-0]));
       end;
 108 : begin
         yyval:=new(presobject,init_bop(' <> ',yyv[yysp-2],yyv[yysp-0]));
       end;
 109 : begin
         yyval:=new(presobject,init_bop(' > ',yyv[yysp-2],yyv[yysp-0]));
       end;
 110 : begin
         yyval:=new(presobject,init_bop(' >= ',yyv[yysp-2],yyv[yysp-0]));
       end;
 111 : begin
         yyval:=new(presobject,init_bop(' < ',yyv[yysp-2],yyv[yysp-0]));
       end;
 112 : begin
         yyval:=new(presobject,init_bop(' <= ',yyv[yysp-2],yyv[yysp-0]));
       end;
 113 : begin
         yyval:=new(presobject,init_bop(' + ',yyv[yysp-2],yyv[yysp-0]));
       end;
 114 : begin
         yyval:=new(presobject,init_bop(' - ',yyv[yysp-2],yyv[yysp-0]));
       end;
 115 : begin
         yyval:=new(presobject,init_bop(' * ',yyv[yysp-2],yyv[yysp-0]));
       end;
 116 : begin
         yyval:=new(presobject,init_bop(' / ',yyv[yysp-2],yyv[yysp-0]));
       end;
 117 : begin
         yyval:=new(presobject,init_bop(' or ',yyv[yysp-2],yyv[yysp-0]));
       end;
 118 : begin
         yyval:=new(presobject,init_bop(' and ',yyv[yysp-2],yyv[yysp-0]));
       end;
 119 : begin
         yyval:=new(presobject,init_bop(' not ',yyv[yysp-2],yyv[yysp-0]));
       end;
 120 : begin
         yyval:=new(presobject,init_bop(' shl ',yyv[yysp-2],yyv[yysp-0]));
       end;
 121 : begin
         yyval:=new(presobject,init_bop(' shr ',yyv[yysp-2],yyv[yysp-0]));
       end;
 122 : begin
         yyv[yysp-0]^.p1:=yyv[yysp-2];
         yyval:=yyv[yysp-0];
         inc(if_nb);
         yyval^.p:=strpnew('if_local'+str(if_nb));

       end;
 123 : begin
         yyval:=yyv[yysp-0];
       end;
 124 : begin
         (* if A then B else C *)
         yyval:=new(presobject,init_three(t_ifexpr,nil,yyv[yysp-2],yyv[yysp-0]));
       end;
 125 : begin
         yyval:=yyv[yysp-0];
       end;
 126 : begin
         yyval:=nil;
       end;
 127 : begin

         yyval:=yyv[yysp-0];

       end;
 128 : begin

         yyval:=yyv[yysp-0];

       end;
 129 : begin

         (* remove L prefix for widestrings *)
         s:=act_token;
         if Win32headers and (s[1]='L') then
         delete(s,1,1);
         yyval:=new(presobject,init_id(''''+copy(s,2,length(s)-2)+''''));

       end;
 130 : begin

         yyval:=new(presobject,init_id(act_token));

       end;
 131 : begin

         yyval:=new(presobject,init_bop('.',yyv[yysp-2],yyv[yysp-0]));

       end;
 132 : begin

         yyval:=new(presobject,init_bop('^.',yyv[yysp-2],yyv[yysp-0]));

       end;
 133 : begin

         yyval:=new(presobject,init_preop('-',yyv[yysp-0]));

       end;
 134 : begin

         yyval:=new(presobject,init_preop('@',yyv[yysp-0]));

       end;
 135 : begin

         yyval:=new(presobject,init_preop(' not ',yyv[yysp-0]));

       end;
 136 : begin

         if assigned(yyv[yysp-0]) then
         yyval:=new(presobject,init_two(t_typespec,yyv[yysp-2],yyv[yysp-0]))
         else
         yyval:=yyv[yysp-2];

       end;
 137 : begin

         yyval:=new(presobject,init_two(t_typespec,yyv[yysp-2],yyv[yysp-0]));

       end;
 138 : begin

         hp:=new(presobject,init_one(t_pointerdef,yyv[yysp-3]));
         yyval:=new(presobject,init_two(t_typespec,hp,yyv[yysp-0]));

       end;
 139 : begin

         if not stripinfo then
         writeln(outfile,aktspace,'(* ',yyv[yysp-3]^.p,' ignored *)');
         dispose(yyv[yysp-3],done);
         write_type_specifier(outfile,yyv[yysp-4]);
         writeln(outfile,' ignored *)');
         hp:=new(presobject,init_one(t_pointerdef,yyv[yysp-4]));
         yyval:=new(presobject,init_two(t_typespec,hp,yyv[yysp-0]));

       end;
 140 : begin

         hp:=new(presobject,init_one(t_exprlist,yyv[yysp-3]));
         yyval:=new(presobject,init_three(t_funexprlist,hp,yyv[yysp-1],nil));

       end;
 141 : begin

         yyval:=yyv[yysp-1];

       end;
 142 : begin

         yyval:=new(presobject,init_two(t_callop,yyv[yysp-5],yyv[yysp-1]));

       end;
 143 : begin

         yyval:=new(presobject,init_two(t_arrayop,yyv[yysp-3],yyv[yysp-1]));

       end;
 144 : begin
         (*enum_element COMMA enum_list *)
         yyval:=yyv[yysp-2];
         yyval^.next:=yyv[yysp-0];

       end;
 145 : begin

         yyval:=yyv[yysp-0];

       end;
 146 : begin
         (* empty enum list *)
         yyval:=nil;
       end;
 147 : begin
         begin (*enum_element: dname _ASSIGN expr *)
         yyval:=new(presobject,init_two(t_enumlist,yyv[yysp-2],yyv[yysp-0]));
         end;

       end;
 148 : begin

         begin (*enum_element: dname*)
         yyval:=new(presobject,init_two(t_enumlist,yyv[yysp-0],nil));
         end;

       end;
 149 : begin

         if yyv[yysp-0]^.typ=t_funexprlist then
         yyval:=yyv[yysp-0]
         else
         yyval:=new(presobject,init_two(t_exprlist,yyv[yysp-0],nil));
         (* if here is a type specifier
         we know the return type *)
         if (yyv[yysp-0]^.typ=t_typespec) then
         yyval^.p3:=yyv[yysp-0]^.p1^.get_copy;

       end;
 150 : begin

         yyval:=yyv[yysp-0];

       end;
 151 : begin

         yyval:=yyv[yysp-1]

       end;
 152 : begin
         (*exprlist COMMA expr*)
         yyval:=yyv[yysp-2];
         yyv[yysp-2]^.next:=yyv[yysp-0];

       end;
 153 : begin

         yyval:=yyv[yysp-0];

       end;
 154 : begin
         (* empty expression list *)
         yyval:=nil;
       end;
 155 : begin

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

yynacts   = 2456;
yyngotos  = 380;
yynstates = 284;
yynrules  = 155;

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
  ( sym: 267; act: -146 ),
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
  ( sym: 263; act: -146 ),
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
  ( sym: 260; act: -91 ),
  ( sym: 261; act: -91 ),
  ( sym: 262; act: -91 ),
  ( sym: 263; act: -91 ),
  ( sym: 264; act: -91 ),
  ( sym: 296; act: -91 ),
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
  ( sym: 263; act: -145 ),
  ( sym: 267; act: -145 ),
{ 96: }
  ( sym: 267; act: 136 ),
{ 97: }
  ( sym: 285; act: 137 ),
  ( sym: 261; act: -148 ),
  ( sym: 263; act: -148 ),
  ( sym: 267; act: -148 ),
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
  ( sym: 263; act: -149 ),
  ( sym: 286; act: -149 ),
{ 104: }
{ 105: }
  ( sym: 286; act: 144 ),
{ 106: }
  ( sym: 262; act: 145 ),
  ( sym: 264; act: 146 ),
  ( sym: 259; act: -127 ),
  ( sym: 260; act: -127 ),
  ( sym: 261; act: -127 ),
  ( sym: 263; act: -127 ),
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
  ( sym: 318; act: -127 ),
  ( sym: 319; act: -127 ),
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
  ( sym: 260; act: 166 ),
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
  ( sym: 262; act: 168 ),
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
  ( sym: 261; act: 171 ),
  ( sym: 260; act: -74 ),
  ( sym: 296; act: -74 ),
{ 126: }
  ( sym: 262; act: 118 ),
  ( sym: 263; act: 172 ),
  ( sym: 264; act: 119 ),
{ 127: }
  ( sym: 262; act: 118 ),
  ( sym: 264; act: 119 ),
  ( sym: 260; act: -85 ),
  ( sym: 261; act: -85 ),
  ( sym: 263; act: -85 ),
  ( sym: 296; act: -85 ),
{ 128: }
  ( sym: 264; act: 119 ),
  ( sym: 260; act: -88 ),
  ( sym: 261; act: -88 ),
  ( sym: 262; act: -88 ),
  ( sym: 263; act: -88 ),
  ( sym: 296; act: -88 ),
{ 129: }
  ( sym: 262; act: 118 ),
  ( sym: 264; act: 119 ),
  ( sym: 260; act: -87 ),
  ( sym: 261; act: -87 ),
  ( sym: 263; act: -87 ),
  ( sym: 296; act: -87 ),
{ 130: }
{ 131: }
{ 132: }
{ 133: }
  ( sym: 260; act: 173 ),
  ( sym: 261; act: 121 ),
{ 134: }
{ 135: }
  ( sym: 271; act: 23 ),
  ( sym: 263; act: -146 ),
  ( sym: 267; act: -146 ),
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
  ( sym: 263; act: 176 ),
  ( sym: 264; act: 119 ),
{ 140: }
{ 141: }
  ( sym: 287; act: 179 ),
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
  ( sym: 263; act: -154 ),
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
  ( sym: 265; act: -154 ),
{ 147: }
  ( sym: 318; act: 142 ),
  ( sym: 319; act: 143 ),
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
{ 148: }
  ( sym: 263; act: 186 ),
  ( sym: 300; act: -106 ),
  ( sym: 301; act: -106 ),
  ( sym: 302; act: -106 ),
  ( sym: 303; act: -106 ),
  ( sym: 304; act: -106 ),
  ( sym: 305; act: -106 ),
  ( sym: 306; act: -106 ),
  ( sym: 307; act: -106 ),
  ( sym: 308; act: -106 ),
  ( sym: 309; act: -106 ),
  ( sym: 310; act: -106 ),
  ( sym: 311; act: -106 ),
  ( sym: 312; act: -106 ),
  ( sym: 313; act: -106 ),
  ( sym: 314; act: -106 ),
  ( sym: 315; act: -106 ),
{ 149: }
  ( sym: 300; act: 187 ),
  ( sym: 301; act: 188 ),
  ( sym: 302; act: 189 ),
  ( sym: 303; act: 190 ),
  ( sym: 304; act: 191 ),
  ( sym: 305; act: 192 ),
  ( sym: 306; act: 193 ),
  ( sym: 307; act: 194 ),
  ( sym: 308; act: 195 ),
  ( sym: 309; act: 196 ),
  ( sym: 310; act: 197 ),
  ( sym: 311; act: 198 ),
  ( sym: 312; act: 199 ),
  ( sym: 313; act: 200 ),
  ( sym: 314; act: 201 ),
  ( sym: 315; act: 202 ),
{ 150: }
  ( sym: 263; act: -70 ),
  ( sym: 282; act: -70 ),
  ( sym: 283; act: -70 ),
  ( sym: 284; act: -70 ),
  ( sym: 313; act: -70 ),
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
  ( sym: 314; act: -128 ),
  ( sym: 315; act: -128 ),
  ( sym: 318; act: -128 ),
  ( sym: 319; act: -128 ),
{ 151: }
  ( sym: 263; act: 204 ),
  ( sym: 282; act: 83 ),
  ( sym: 283; act: 84 ),
  ( sym: 284; act: 85 ),
  ( sym: 313; act: 205 ),
{ 152: }
  ( sym: 262; act: 145 ),
  ( sym: 263; act: 206 ),
  ( sym: 264; act: 146 ),
  ( sym: 282; act: -71 ),
  ( sym: 283; act: -71 ),
  ( sym: 284; act: -71 ),
  ( sym: 313; act: -71 ),
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
  ( sym: 314; act: -127 ),
  ( sym: 315; act: -127 ),
  ( sym: 318; act: -127 ),
  ( sym: 319; act: -127 ),
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
  ( sym: 318; act: 142 ),
  ( sym: 319; act: 143 ),
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
{ 156: }
  ( sym: 318; act: 142 ),
  ( sym: 319; act: 143 ),
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
{ 157: }
  ( sym: 262; act: 118 ),
  ( sym: 264; act: 119 ),
  ( sym: 260; act: -86 ),
  ( sym: 261; act: -86 ),
  ( sym: 263; act: -86 ),
  ( sym: 296; act: -86 ),
{ 158: }
  ( sym: 261; act: 208 ),
  ( sym: 263; act: -79 ),
{ 159: }
  ( sym: 263; act: 209 ),
{ 160: }
  ( sym: 262; act: 213 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 214 ),
  ( sym: 282; act: 83 ),
  ( sym: 283; act: 84 ),
  ( sym: 284; act: 85 ),
  ( sym: 308; act: 86 ),
  ( sym: 313; act: 215 ),
  ( sym: 261; act: -105 ),
  ( sym: 263; act: -105 ),
  ( sym: 264; act: -105 ),
{ 161: }
{ 162: }
  ( sym: 263; act: 216 ),
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
  ( sym: 265; act: 217 ),
  ( sym: 300; act: 187 ),
  ( sym: 301; act: 188 ),
  ( sym: 302; act: 189 ),
  ( sym: 303; act: 190 ),
  ( sym: 304; act: 191 ),
  ( sym: 305; act: 192 ),
  ( sym: 306; act: 193 ),
  ( sym: 307; act: 194 ),
  ( sym: 308; act: 195 ),
  ( sym: 309; act: 196 ),
  ( sym: 310; act: 197 ),
  ( sym: 311; act: 198 ),
  ( sym: 312; act: 199 ),
  ( sym: 313; act: 200 ),
  ( sym: 314; act: 201 ),
  ( sym: 315; act: 202 ),
{ 166: }
{ 167: }
  ( sym: 262; act: 118 ),
  ( sym: 264; act: 119 ),
  ( sym: 260; act: -72 ),
  ( sym: 261; act: -72 ),
  ( sym: 296; act: -72 ),
{ 168: }
  ( sym: 271; act: 23 ),
{ 169: }
  ( sym: 300; act: 187 ),
  ( sym: 301; act: 188 ),
  ( sym: 302; act: 189 ),
  ( sym: 303; act: 190 ),
  ( sym: 304; act: 191 ),
  ( sym: 305; act: 192 ),
  ( sym: 306; act: 193 ),
  ( sym: 307; act: 194 ),
  ( sym: 308; act: 195 ),
  ( sym: 309; act: 196 ),
  ( sym: 310; act: 197 ),
  ( sym: 311; act: 198 ),
  ( sym: 312; act: 199 ),
  ( sym: 313; act: 200 ),
  ( sym: 314; act: 201 ),
  ( sym: 315; act: 202 ),
  ( sym: 260; act: -90 ),
  ( sym: 261; act: -90 ),
  ( sym: 262; act: -90 ),
  ( sym: 263; act: -90 ),
  ( sym: 264; act: -90 ),
  ( sym: 296; act: -90 ),
{ 170: }
  ( sym: 300; act: 187 ),
  ( sym: 301; act: 188 ),
  ( sym: 302; act: 189 ),
  ( sym: 303; act: 190 ),
  ( sym: 304; act: 191 ),
  ( sym: 305; act: 192 ),
  ( sym: 306; act: 193 ),
  ( sym: 307; act: 194 ),
  ( sym: 308; act: 195 ),
  ( sym: 309; act: 196 ),
  ( sym: 310; act: 197 ),
  ( sym: 311; act: 198 ),
  ( sym: 312; act: 199 ),
  ( sym: 313; act: 200 ),
  ( sym: 314; act: 201 ),
  ( sym: 315; act: 202 ),
  ( sym: 260; act: -89 ),
  ( sym: 261; act: -89 ),
  ( sym: 262; act: -89 ),
  ( sym: 263; act: -89 ),
  ( sym: 264; act: -89 ),
  ( sym: 296; act: -89 ),
{ 171: }
  ( sym: 256; act: 80 ),
  ( sym: 262; act: 81 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 82 ),
  ( sym: 282; act: 83 ),
  ( sym: 283; act: 84 ),
  ( sym: 284; act: 85 ),
  ( sym: 308; act: 86 ),
  ( sym: 313; act: 87 ),
{ 172: }
{ 173: }
{ 174: }
{ 175: }
  ( sym: 300; act: 187 ),
  ( sym: 301; act: 188 ),
  ( sym: 302; act: 189 ),
  ( sym: 303; act: 190 ),
  ( sym: 304; act: 191 ),
  ( sym: 305; act: 192 ),
  ( sym: 306; act: 193 ),
  ( sym: 307; act: 194 ),
  ( sym: 308; act: 195 ),
  ( sym: 309; act: 196 ),
  ( sym: 310; act: 197 ),
  ( sym: 311; act: 198 ),
  ( sym: 312; act: 199 ),
  ( sym: 313; act: 200 ),
  ( sym: 314; act: 201 ),
  ( sym: 315; act: 202 ),
  ( sym: 261; act: -147 ),
  ( sym: 263; act: -147 ),
  ( sym: 267; act: -147 ),
{ 176: }
  ( sym: 287; act: 221 ),
  ( sym: 262; act: -3 ),
{ 177: }
  ( sym: 286; act: 222 ),
{ 178: }
  ( sym: 262; act: 223 ),
{ 179: }
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
{ 180: }
{ 181: }
{ 182: }
  ( sym: 261; act: 225 ),
  ( sym: 263; act: -153 ),
  ( sym: 265; act: -153 ),
{ 183: }
  ( sym: 263; act: 226 ),
{ 184: }
  ( sym: 300; act: 187 ),
  ( sym: 301; act: 188 ),
  ( sym: 302; act: 189 ),
  ( sym: 303; act: 190 ),
  ( sym: 304; act: 191 ),
  ( sym: 305; act: 192 ),
  ( sym: 306; act: 193 ),
  ( sym: 307; act: 194 ),
  ( sym: 308; act: 195 ),
  ( sym: 309; act: 196 ),
  ( sym: 310; act: 197 ),
  ( sym: 311; act: 198 ),
  ( sym: 312; act: 199 ),
  ( sym: 313; act: 200 ),
  ( sym: 314; act: 201 ),
  ( sym: 315; act: 202 ),
  ( sym: 261; act: -155 ),
  ( sym: 263; act: -155 ),
  ( sym: 265; act: -155 ),
{ 185: }
  ( sym: 265; act: 227 ),
{ 186: }
{ 187: }
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
  ( sym: 313; act: 245 ),
{ 204: }
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
{ 205: }
  ( sym: 263; act: 247 ),
{ 206: }
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
  ( sym: 259; act: -126 ),
  ( sym: 260; act: -126 ),
  ( sym: 261; act: -126 ),
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
  ( sym: 309; act: -126 ),
  ( sym: 311; act: -126 ),
  ( sym: 312; act: -126 ),
  ( sym: 313; act: -126 ),
  ( sym: 314; act: -126 ),
  ( sym: 318; act: -126 ),
  ( sym: 319; act: -126 ),
{ 207: }
  ( sym: 263; act: 250 ),
  ( sym: 318; act: 142 ),
  ( sym: 319; act: 143 ),
{ 208: }
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
{ 209: }
{ 210: }
  ( sym: 313; act: 252 ),
{ 211: }
  ( sym: 262; act: 254 ),
  ( sym: 264; act: 255 ),
  ( sym: 261; act: -78 ),
  ( sym: 263; act: -78 ),
{ 212: }
  ( sym: 262; act: 118 ),
  ( sym: 264; act: 119 ),
  ( sym: 261; act: -76 ),
  ( sym: 263; act: -76 ),
{ 213: }
  ( sym: 262; act: 213 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 214 ),
  ( sym: 282; act: 83 ),
  ( sym: 283; act: 84 ),
  ( sym: 284; act: 85 ),
  ( sym: 308; act: 86 ),
  ( sym: 313; act: 257 ),
  ( sym: 263; act: -105 ),
  ( sym: 264; act: -105 ),
{ 214: }
  ( sym: 262; act: 213 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 214 ),
  ( sym: 282; act: 83 ),
  ( sym: 283; act: 84 ),
  ( sym: 284; act: 85 ),
  ( sym: 308; act: 86 ),
  ( sym: 313; act: 257 ),
  ( sym: 261; act: -105 ),
  ( sym: 263; act: -105 ),
  ( sym: 264; act: -105 ),
{ 215: }
  ( sym: 262; act: 213 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 214 ),
  ( sym: 282; act: 83 ),
  ( sym: 283; act: 84 ),
  ( sym: 284; act: 85 ),
  ( sym: 308; act: 86 ),
  ( sym: 313; act: 257 ),
  ( sym: 261; act: -105 ),
  ( sym: 263; act: -105 ),
  ( sym: 264; act: -105 ),
{ 216: }
{ 217: }
{ 218: }
  ( sym: 263; act: 261 ),
{ 219: }
{ 220: }
  ( sym: 262; act: 262 ),
{ 221: }
{ 222: }
{ 223: }
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
{ 224: }
{ 225: }
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
  ( sym: 263; act: -154 ),
  ( sym: 265; act: -154 ),
{ 226: }
{ 227: }
{ 228: }
  ( sym: 306; act: 193 ),
  ( sym: 307; act: 194 ),
  ( sym: 308; act: 195 ),
  ( sym: 309; act: 196 ),
  ( sym: 310; act: 197 ),
  ( sym: 311; act: 198 ),
  ( sym: 312; act: 199 ),
  ( sym: 313; act: 200 ),
  ( sym: 314; act: 201 ),
  ( sym: 315; act: 202 ),
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
{ 229: }
  ( sym: 306; act: 193 ),
  ( sym: 307; act: 194 ),
  ( sym: 308; act: 195 ),
  ( sym: 309; act: 196 ),
  ( sym: 310; act: 197 ),
  ( sym: 311; act: 198 ),
  ( sym: 312; act: 199 ),
  ( sym: 313; act: 200 ),
  ( sym: 314; act: 201 ),
  ( sym: 315; act: 202 ),
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
{ 230: }
  ( sym: 306; act: 193 ),
  ( sym: 307; act: 194 ),
  ( sym: 308; act: 195 ),
  ( sym: 309; act: 196 ),
  ( sym: 310; act: 197 ),
  ( sym: 311; act: 198 ),
  ( sym: 312; act: 199 ),
  ( sym: 313; act: 200 ),
  ( sym: 314; act: 201 ),
  ( sym: 315; act: 202 ),
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
{ 231: }
  ( sym: 306; act: 193 ),
  ( sym: 307; act: 194 ),
  ( sym: 308; act: 195 ),
  ( sym: 309; act: 196 ),
  ( sym: 310; act: 197 ),
  ( sym: 311; act: 198 ),
  ( sym: 312; act: 199 ),
  ( sym: 313; act: 200 ),
  ( sym: 314; act: 201 ),
  ( sym: 315; act: 202 ),
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
{ 232: }
  ( sym: 306; act: 193 ),
  ( sym: 307; act: 194 ),
  ( sym: 308; act: 195 ),
  ( sym: 309; act: 196 ),
  ( sym: 310; act: 197 ),
  ( sym: 311; act: 198 ),
  ( sym: 312; act: 199 ),
  ( sym: 313; act: 200 ),
  ( sym: 314; act: 201 ),
  ( sym: 315; act: 202 ),
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
{ 233: }
  ( sym: 306; act: 193 ),
  ( sym: 307; act: 194 ),
  ( sym: 308; act: 195 ),
  ( sym: 309; act: 196 ),
  ( sym: 310; act: 197 ),
  ( sym: 311; act: 198 ),
  ( sym: 312; act: 199 ),
  ( sym: 313; act: 200 ),
  ( sym: 314; act: 201 ),
  ( sym: 315; act: 202 ),
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
{ 234: }
{ 235: }
  ( sym: 259; act: 265 ),
  ( sym: 300; act: 187 ),
  ( sym: 301; act: 188 ),
  ( sym: 302; act: 189 ),
  ( sym: 303; act: 190 ),
  ( sym: 304; act: 191 ),
  ( sym: 305; act: 192 ),
  ( sym: 306; act: 193 ),
  ( sym: 307; act: 194 ),
  ( sym: 308; act: 195 ),
  ( sym: 309; act: 196 ),
  ( sym: 310; act: 197 ),
  ( sym: 311; act: 198 ),
  ( sym: 312; act: 199 ),
  ( sym: 313; act: 200 ),
  ( sym: 314; act: 201 ),
  ( sym: 315; act: 202 ),
{ 236: }
  ( sym: 308; act: 195 ),
  ( sym: 309; act: 196 ),
  ( sym: 310; act: 197 ),
  ( sym: 311; act: 198 ),
  ( sym: 312; act: 199 ),
  ( sym: 313; act: 200 ),
  ( sym: 314; act: 201 ),
  ( sym: 315; act: 202 ),
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
  ( sym: 318; act: -117 ),
  ( sym: 319; act: -117 ),
{ 237: }
  ( sym: 309; act: 196 ),
  ( sym: 310; act: 197 ),
  ( sym: 311; act: 198 ),
  ( sym: 312; act: 199 ),
  ( sym: 313; act: 200 ),
  ( sym: 314; act: 201 ),
  ( sym: 315; act: 202 ),
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
  ( sym: 318; act: -118 ),
  ( sym: 319; act: -118 ),
{ 238: }
  ( sym: 311; act: 198 ),
  ( sym: 312; act: 199 ),
  ( sym: 313; act: 200 ),
  ( sym: 314; act: 201 ),
  ( sym: 315; act: 202 ),
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
  ( sym: 318; act: -113 ),
  ( sym: 319; act: -113 ),
{ 239: }
  ( sym: 311; act: 198 ),
  ( sym: 312; act: 199 ),
  ( sym: 313; act: 200 ),
  ( sym: 314; act: 201 ),
  ( sym: 315; act: 202 ),
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
  ( sym: 318; act: -114 ),
  ( sym: 319; act: -114 ),
{ 240: }
  ( sym: 313; act: 200 ),
  ( sym: 314; act: 201 ),
  ( sym: 315; act: 202 ),
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
  ( sym: 318; act: -121 ),
  ( sym: 319; act: -121 ),
{ 241: }
  ( sym: 313; act: 200 ),
  ( sym: 314; act: 201 ),
  ( sym: 315; act: 202 ),
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
  ( sym: 318; act: -120 ),
  ( sym: 319; act: -120 ),
{ 242: }
  ( sym: 315; act: 202 ),
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
  ( sym: 313; act: -115 ),
  ( sym: 314; act: -115 ),
  ( sym: 318; act: -115 ),
  ( sym: 319; act: -115 ),
{ 243: }
  ( sym: 315; act: 202 ),
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
{ 244: }
  ( sym: 315; act: 202 ),
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
  ( sym: 263; act: 266 ),
{ 246: }
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
{ 247: }
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
{ 248: }
{ 249: }
  ( sym: 318; act: 142 ),
  ( sym: 319; act: 143 ),
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
{ 250: }
  ( sym: 287; act: 221 ),
  ( sym: 262; act: -3 ),
{ 251: }
{ 252: }
  ( sym: 262; act: 213 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 214 ),
  ( sym: 282; act: 83 ),
  ( sym: 283; act: 84 ),
  ( sym: 284; act: 85 ),
  ( sym: 308; act: 86 ),
  ( sym: 313; act: 257 ),
  ( sym: 261; act: -105 ),
  ( sym: 263; act: -105 ),
  ( sym: 264; act: -105 ),
{ 253: }
{ 254: }
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
{ 255: }
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
{ 256: }
  ( sym: 262; act: 254 ),
  ( sym: 263; act: 272 ),
  ( sym: 264; act: 255 ),
{ 257: }
  ( sym: 262; act: 213 ),
  ( sym: 271; act: 23 ),
  ( sym: 281; act: 214 ),
  ( sym: 282; act: 83 ),
  ( sym: 283; act: 84 ),
  ( sym: 284; act: 85 ),
  ( sym: 308; act: 86 ),
  ( sym: 313; act: 257 ),
  ( sym: 261; act: -105 ),
  ( sym: 263; act: -105 ),
  ( sym: 264; act: -105 ),
{ 258: }
  ( sym: 262; act: 254 ),
  ( sym: 264; act: 255 ),
  ( sym: 261; act: -98 ),
  ( sym: 263; act: -98 ),
{ 259: }
  ( sym: 264; act: 255 ),
  ( sym: 261; act: -100 ),
  ( sym: 262; act: -100 ),
  ( sym: 263; act: -100 ),
{ 260: }
  ( sym: 262; act: 118 ),
  ( sym: 264; act: 119 ),
  ( sym: 261; act: -77 ),
  ( sym: 263; act: -77 ),
{ 261: }
{ 262: }
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
{ 263: }
  ( sym: 263; act: 274 ),
{ 264: }
{ 265: }
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
{ 266: }
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
{ 267: }
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
{ 268: }
  ( sym: 262; act: 277 ),
{ 269: }
  ( sym: 262; act: 254 ),
  ( sym: 264; act: 255 ),
  ( sym: 261; act: -99 ),
  ( sym: 263; act: -99 ),
{ 270: }
  ( sym: 263; act: 278 ),
{ 271: }
  ( sym: 265; act: 279 ),
  ( sym: 300; act: 187 ),
  ( sym: 301; act: 188 ),
  ( sym: 302; act: 189 ),
  ( sym: 303; act: 190 ),
  ( sym: 304; act: 191 ),
  ( sym: 305; act: 192 ),
  ( sym: 306; act: 193 ),
  ( sym: 307; act: 194 ),
  ( sym: 308; act: 195 ),
  ( sym: 309; act: 196 ),
  ( sym: 310; act: 197 ),
  ( sym: 311; act: 198 ),
  ( sym: 312; act: 199 ),
  ( sym: 313; act: 200 ),
  ( sym: 314; act: 201 ),
  ( sym: 315; act: 202 ),
{ 272: }
{ 273: }
  ( sym: 263; act: 280 ),
{ 274: }
{ 275: }
  ( sym: 307; act: 194 ),
  ( sym: 308; act: 195 ),
  ( sym: 309; act: 196 ),
  ( sym: 310; act: 197 ),
  ( sym: 311; act: 198 ),
  ( sym: 312; act: 199 ),
  ( sym: 313; act: 200 ),
  ( sym: 314; act: 201 ),
  ( sym: 315; act: 202 ),
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
  ( sym: 318; act: -124 ),
  ( sym: 319; act: -124 ),
{ 276: }
  ( sym: 318; act: 142 ),
  ( sym: 319; act: 143 ),
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
{ 277: }
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
  ( sym: 263; act: -154 ),
{ 278: }
{ 279: }
{ 280: }
  ( sym: 260; act: 282 ),
{ 281: }
  ( sym: 263; act: 283 )
{ 282: }
{ 283: }
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
  ( sym: -15; act: 167 ),
  ( sym: -11; act: 79 ),
{ 122: }
{ 123: }
  ( sym: -34; act: 147 ),
  ( sym: -32; act: 164 ),
  ( sym: -29; act: 169 ),
  ( sym: -25; act: 104 ),
  ( sym: -11; act: 106 ),
{ 124: }
  ( sym: -34; act: 147 ),
  ( sym: -32; act: 164 ),
  ( sym: -29; act: 170 ),
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
  ( sym: -17; act: 174 ),
  ( sym: -11; act: 97 ),
{ 136: }
{ 137: }
  ( sym: -34; act: 147 ),
  ( sym: -32; act: 164 ),
  ( sym: -29; act: 175 ),
  ( sym: -25; act: 104 ),
  ( sym: -11; act: 106 ),
{ 138: }
{ 139: }
  ( sym: -31; act: 117 ),
{ 140: }
{ 141: }
  ( sym: -18; act: 177 ),
  ( sym: -4; act: 178 ),
{ 142: }
  ( sym: -34; act: 147 ),
  ( sym: -32; act: 164 ),
  ( sym: -29; act: 180 ),
  ( sym: -25; act: 104 ),
  ( sym: -11; act: 106 ),
{ 143: }
  ( sym: -34; act: 147 ),
  ( sym: -32; act: 164 ),
  ( sym: -29; act: 181 ),
  ( sym: -25; act: 104 ),
  ( sym: -11; act: 106 ),
{ 144: }
{ 145: }
  ( sym: -38; act: 182 ),
  ( sym: -36; act: 183 ),
  ( sym: -34; act: 147 ),
  ( sym: -32; act: 164 ),
  ( sym: -29; act: 184 ),
  ( sym: -25; act: 104 ),
  ( sym: -11; act: 106 ),
{ 146: }
  ( sym: -38; act: 182 ),
  ( sym: -36; act: 185 ),
  ( sym: -34; act: 147 ),
  ( sym: -32; act: 164 ),
  ( sym: -29; act: 184 ),
  ( sym: -25; act: 104 ),
  ( sym: -11; act: 106 ),
{ 147: }
{ 148: }
{ 149: }
{ 150: }
{ 151: }
  ( sym: -28; act: 203 ),
{ 152: }
{ 153: }
  ( sym: -34; act: 207 ),
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
  ( sym: -28; act: 210 ),
  ( sym: -27; act: 211 ),
  ( sym: -15; act: 212 ),
  ( sym: -11; act: 79 ),
{ 161: }
{ 162: }
{ 163: }
{ 164: }
{ 165: }
{ 166: }
{ 167: }
  ( sym: -31; act: 117 ),
{ 168: }
  ( sym: -11; act: 218 ),
{ 169: }
{ 170: }
{ 171: }
  ( sym: -28; act: 76 ),
  ( sym: -15; act: 77 ),
  ( sym: -13; act: 219 ),
  ( sym: -11; act: 79 ),
{ 172: }
{ 173: }
{ 174: }
{ 175: }
{ 176: }
  ( sym: -4; act: 220 ),
{ 177: }
{ 178: }
{ 179: }
  ( sym: -34; act: 103 ),
  ( sym: -25; act: 104 ),
  ( sym: -19; act: 224 ),
  ( sym: -11; act: 106 ),
{ 180: }
{ 181: }
{ 182: }
{ 183: }
{ 184: }
{ 185: }
{ 186: }
{ 187: }
  ( sym: -34; act: 147 ),
  ( sym: -32; act: 164 ),
  ( sym: -29; act: 228 ),
  ( sym: -25; act: 104 ),
  ( sym: -11; act: 106 ),
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
  ( sym: -33; act: 234 ),
  ( sym: -32; act: 164 ),
  ( sym: -29; act: 235 ),
  ( sym: -25; act: 104 ),
  ( sym: -11; act: 106 ),
{ 194: }
  ( sym: -34; act: 147 ),
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
{ 204: }
  ( sym: -34; act: 246 ),
  ( sym: -25; act: 104 ),
  ( sym: -11; act: 106 ),
{ 205: }
{ 206: }
  ( sym: -35; act: 248 ),
  ( sym: -34; act: 249 ),
  ( sym: -25; act: 104 ),
  ( sym: -11; act: 106 ),
{ 207: }
{ 208: }
  ( sym: -26; act: 158 ),
  ( sym: -25; act: 15 ),
  ( sym: -23; act: 16 ),
  ( sym: -16; act: 251 ),
  ( sym: -14; act: 17 ),
  ( sym: -12; act: 160 ),
  ( sym: -11; act: 19 ),
{ 209: }
{ 210: }
{ 211: }
  ( sym: -31; act: 253 ),
{ 212: }
  ( sym: -31; act: 117 ),
{ 213: }
  ( sym: -28; act: 210 ),
  ( sym: -27; act: 256 ),
  ( sym: -15; act: 126 ),
  ( sym: -11; act: 79 ),
{ 214: }
  ( sym: -28; act: 210 ),
  ( sym: -27; act: 258 ),
  ( sym: -15; act: 127 ),
  ( sym: -11; act: 79 ),
{ 215: }
  ( sym: -28; act: 210 ),
  ( sym: -27; act: 259 ),
  ( sym: -15; act: 260 ),
  ( sym: -11; act: 79 ),
{ 216: }
{ 217: }
{ 218: }
{ 219: }
{ 220: }
{ 221: }
{ 222: }
{ 223: }
  ( sym: -34; act: 103 ),
  ( sym: -25; act: 104 ),
  ( sym: -19; act: 263 ),
  ( sym: -11; act: 106 ),
{ 224: }
{ 225: }
  ( sym: -38; act: 182 ),
  ( sym: -36; act: 264 ),
  ( sym: -34; act: 147 ),
  ( sym: -32; act: 164 ),
  ( sym: -29; act: 184 ),
  ( sym: -25; act: 104 ),
  ( sym: -11; act: 106 ),
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
{ 239: }
{ 240: }
{ 241: }
{ 242: }
{ 243: }
{ 244: }
{ 245: }
{ 246: }
{ 247: }
  ( sym: -34; act: 267 ),
  ( sym: -25; act: 104 ),
  ( sym: -11; act: 106 ),
{ 248: }
{ 249: }
{ 250: }
  ( sym: -4; act: 268 ),
{ 251: }
{ 252: }
  ( sym: -28; act: 210 ),
  ( sym: -27; act: 269 ),
  ( sym: -15; act: 157 ),
  ( sym: -11; act: 79 ),
{ 253: }
{ 254: }
  ( sym: -26; act: 158 ),
  ( sym: -25; act: 15 ),
  ( sym: -23; act: 16 ),
  ( sym: -16; act: 270 ),
  ( sym: -14; act: 17 ),
  ( sym: -12; act: 160 ),
  ( sym: -11; act: 19 ),
{ 255: }
  ( sym: -34; act: 147 ),
  ( sym: -32; act: 164 ),
  ( sym: -29; act: 271 ),
  ( sym: -25; act: 104 ),
  ( sym: -11; act: 106 ),
{ 256: }
  ( sym: -31; act: 253 ),
{ 257: }
  ( sym: -28; act: 210 ),
  ( sym: -27; act: 259 ),
  ( sym: -15; act: 129 ),
  ( sym: -11; act: 79 ),
{ 258: }
  ( sym: -31; act: 253 ),
{ 259: }
  ( sym: -31; act: 253 ),
{ 260: }
  ( sym: -31; act: 117 ),
{ 261: }
{ 262: }
  ( sym: -26; act: 158 ),
  ( sym: -25; act: 15 ),
  ( sym: -23; act: 16 ),
  ( sym: -16; act: 273 ),
  ( sym: -14; act: 17 ),
  ( sym: -12; act: 160 ),
  ( sym: -11; act: 19 ),
{ 263: }
{ 264: }
{ 265: }
  ( sym: -34; act: 147 ),
  ( sym: -32; act: 164 ),
  ( sym: -29; act: 275 ),
  ( sym: -25; act: 104 ),
  ( sym: -11; act: 106 ),
{ 266: }
  ( sym: -34; act: 276 ),
  ( sym: -25; act: 104 ),
  ( sym: -11; act: 106 ),
{ 267: }
{ 268: }
{ 269: }
  ( sym: -31; act: 253 ),
{ 270: }
{ 271: }
{ 272: }
{ 273: }
{ 274: }
{ 275: }
{ 276: }
{ 277: }
  ( sym: -38; act: 182 ),
  ( sym: -36; act: 281 ),
  ( sym: -34; act: 147 ),
  ( sym: -32; act: 164 ),
  ( sym: -29; act: 184 ),
  ( sym: -25; act: 104 ),
  ( sym: -11; act: 106 )
{ 278: }
{ 279: }
{ 280: }
{ 281: }
{ 282: }
{ 283: }
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
{ 83: } -82,
{ 84: } -84,
{ 85: } -83,
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
{ 104: } -128,
{ 105: } 0,
{ 106: } 0,
{ 107: } 0,
{ 108: } -130,
{ 109: } -129,
{ 110: } -29,
{ 111: } 0,
{ 112: } 0,
{ 113: } 0,
{ 114: } -37,
{ 115: } -39,
{ 116: } 0,
{ 117: } -93,
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
{ 161: } -96,
{ 162: } 0,
{ 163: } -81,
{ 164: } -106,
{ 165: } 0,
{ 166: } -21,
{ 167: } 0,
{ 168: } 0,
{ 169: } 0,
{ 170: } 0,
{ 171: } 0,
{ 172: } -95,
{ 173: } -55,
{ 174: } -144,
{ 175: } 0,
{ 176: } 0,
{ 177: } 0,
{ 178: } 0,
{ 179: } 0,
{ 180: } -131,
{ 181: } -132,
{ 182: } 0,
{ 183: } 0,
{ 184: } 0,
{ 185: } 0,
{ 186: } -141,
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
{ 204: } 0,
{ 205: } 0,
{ 206: } 0,
{ 207: } 0,
{ 208: } 0,
{ 209: } -92,
{ 210: } 0,
{ 211: } 0,
{ 212: } 0,
{ 213: } 0,
{ 214: } 0,
{ 215: } 0,
{ 216: } -97,
{ 217: } -94,
{ 218: } 0,
{ 219: } -73,
{ 220: } 0,
{ 221: } -2,
{ 222: } -28,
{ 223: } 0,
{ 224: } -150,
{ 225: } 0,
{ 226: } -140,
{ 227: } -143,
{ 228: } 0,
{ 229: } 0,
{ 230: } 0,
{ 231: } 0,
{ 232: } 0,
{ 233: } 0,
{ 234: } -122,
{ 235: } 0,
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
{ 248: } -136,
{ 249: } 0,
{ 250: } 0,
{ 251: } -80,
{ 252: } 0,
{ 253: } -102,
{ 254: } 0,
{ 255: } 0,
{ 256: } 0,
{ 257: } 0,
{ 258: } 0,
{ 259: } 0,
{ 260: } 0,
{ 261: } -19,
{ 262: } 0,
{ 263: } 0,
{ 264: } -152,
{ 265: } 0,
{ 266: } 0,
{ 267: } 0,
{ 268: } 0,
{ 269: } 0,
{ 270: } 0,
{ 271: } 0,
{ 272: } -104,
{ 273: } 0,
{ 274: } -151,
{ 275: } 0,
{ 276: } 0,
{ 277: } 0,
{ 278: } -101,
{ 279: } -103,
{ 280: } 0,
{ 281: } 0,
{ 282: } -24,
{ 283: } -142
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
{ 120: } 789,
{ 121: } 790,
{ 122: } 798,
{ 123: } 799,
{ 124: } 814,
{ 125: } 829,
{ 126: } 832,
{ 127: } 835,
{ 128: } 841,
{ 129: } 847,
{ 130: } 853,
{ 131: } 853,
{ 132: } 853,
{ 133: } 853,
{ 134: } 855,
{ 135: } 855,
{ 136: } 858,
{ 137: } 858,
{ 138: } 873,
{ 139: } 873,
{ 140: } 876,
{ 141: } 876,
{ 142: } 878,
{ 143: } 893,
{ 144: } 908,
{ 145: } 908,
{ 146: } 924,
{ 147: } 940,
{ 148: } 968,
{ 149: } 985,
{ 150: } 1001,
{ 151: } 1023,
{ 152: } 1028,
{ 153: } 1052,
{ 154: } 1067,
{ 155: } 1095,
{ 156: } 1123,
{ 157: } 1151,
{ 158: } 1157,
{ 159: } 1159,
{ 160: } 1160,
{ 161: } 1171,
{ 162: } 1171,
{ 163: } 1182,
{ 164: } 1182,
{ 165: } 1182,
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
{ 209: } 1633,
{ 210: } 1633,
{ 211: } 1634,
{ 212: } 1638,
{ 213: } 1642,
{ 214: } 1652,
{ 215: } 1663,
{ 216: } 1674,
{ 217: } 1674,
{ 218: } 1674,
{ 219: } 1675,
{ 220: } 1675,
{ 221: } 1676,
{ 222: } 1676,
{ 223: } 1676,
{ 224: } 1691,
{ 225: } 1691,
{ 226: } 1708,
{ 227: } 1708,
{ 228: } 1708,
{ 229: } 1736,
{ 230: } 1764,
{ 231: } 1792,
{ 232: } 1820,
{ 233: } 1848,
{ 234: } 1876,
{ 235: } 1876,
{ 236: } 1893,
{ 237: } 1921,
{ 238: } 1949,
{ 239: } 1977,
{ 240: } 2005,
{ 241: } 2033,
{ 242: } 2061,
{ 243: } 2089,
{ 244: } 2117,
{ 245: } 2145,
{ 246: } 2146,
{ 247: } 2174,
{ 248: } 2189,
{ 249: } 2189,
{ 250: } 2217,
{ 251: } 2219,
{ 252: } 2219,
{ 253: } 2230,
{ 254: } 2230,
{ 255: } 2245,
{ 256: } 2260,
{ 257: } 2263,
{ 258: } 2274,
{ 259: } 2278,
{ 260: } 2282,
{ 261: } 2286,
{ 262: } 2286,
{ 263: } 2300,
{ 264: } 2301,
{ 265: } 2301,
{ 266: } 2316,
{ 267: } 2331,
{ 268: } 2359,
{ 269: } 2360,
{ 270: } 2364,
{ 271: } 2365,
{ 272: } 2382,
{ 273: } 2382,
{ 274: } 2383,
{ 275: } 2383,
{ 276: } 2411,
{ 277: } 2439,
{ 278: } 2455,
{ 279: } 2455,
{ 280: } 2455,
{ 281: } 2456,
{ 282: } 2457,
{ 283: } 2457
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
{ 119: } 788,
{ 120: } 789,
{ 121: } 797,
{ 122: } 798,
{ 123: } 813,
{ 124: } 828,
{ 125: } 831,
{ 126: } 834,
{ 127: } 840,
{ 128: } 846,
{ 129: } 852,
{ 130: } 852,
{ 131: } 852,
{ 132: } 852,
{ 133: } 854,
{ 134: } 854,
{ 135: } 857,
{ 136: } 857,
{ 137: } 872,
{ 138: } 872,
{ 139: } 875,
{ 140: } 875,
{ 141: } 877,
{ 142: } 892,
{ 143: } 907,
{ 144: } 907,
{ 145: } 923,
{ 146: } 939,
{ 147: } 967,
{ 148: } 984,
{ 149: } 1000,
{ 150: } 1022,
{ 151: } 1027,
{ 152: } 1051,
{ 153: } 1066,
{ 154: } 1094,
{ 155: } 1122,
{ 156: } 1150,
{ 157: } 1156,
{ 158: } 1158,
{ 159: } 1159,
{ 160: } 1170,
{ 161: } 1170,
{ 162: } 1181,
{ 163: } 1181,
{ 164: } 1181,
{ 165: } 1198,
{ 166: } 1198,
{ 167: } 1203,
{ 168: } 1204,
{ 169: } 1226,
{ 170: } 1248,
{ 171: } 1257,
{ 172: } 1257,
{ 173: } 1257,
{ 174: } 1257,
{ 175: } 1276,
{ 176: } 1278,
{ 177: } 1279,
{ 178: } 1280,
{ 179: } 1295,
{ 180: } 1295,
{ 181: } 1295,
{ 182: } 1298,
{ 183: } 1299,
{ 184: } 1318,
{ 185: } 1319,
{ 186: } 1319,
{ 187: } 1334,
{ 188: } 1349,
{ 189: } 1364,
{ 190: } 1379,
{ 191: } 1394,
{ 192: } 1409,
{ 193: } 1424,
{ 194: } 1439,
{ 195: } 1454,
{ 196: } 1469,
{ 197: } 1484,
{ 198: } 1499,
{ 199: } 1514,
{ 200: } 1529,
{ 201: } 1544,
{ 202: } 1559,
{ 203: } 1560,
{ 204: } 1575,
{ 205: } 1576,
{ 206: } 1615,
{ 207: } 1618,
{ 208: } 1632,
{ 209: } 1632,
{ 210: } 1633,
{ 211: } 1637,
{ 212: } 1641,
{ 213: } 1651,
{ 214: } 1662,
{ 215: } 1673,
{ 216: } 1673,
{ 217: } 1673,
{ 218: } 1674,
{ 219: } 1674,
{ 220: } 1675,
{ 221: } 1675,
{ 222: } 1675,
{ 223: } 1690,
{ 224: } 1690,
{ 225: } 1707,
{ 226: } 1707,
{ 227: } 1707,
{ 228: } 1735,
{ 229: } 1763,
{ 230: } 1791,
{ 231: } 1819,
{ 232: } 1847,
{ 233: } 1875,
{ 234: } 1875,
{ 235: } 1892,
{ 236: } 1920,
{ 237: } 1948,
{ 238: } 1976,
{ 239: } 2004,
{ 240: } 2032,
{ 241: } 2060,
{ 242: } 2088,
{ 243: } 2116,
{ 244: } 2144,
{ 245: } 2145,
{ 246: } 2173,
{ 247: } 2188,
{ 248: } 2188,
{ 249: } 2216,
{ 250: } 2218,
{ 251: } 2218,
{ 252: } 2229,
{ 253: } 2229,
{ 254: } 2244,
{ 255: } 2259,
{ 256: } 2262,
{ 257: } 2273,
{ 258: } 2277,
{ 259: } 2281,
{ 260: } 2285,
{ 261: } 2285,
{ 262: } 2299,
{ 263: } 2300,
{ 264: } 2300,
{ 265: } 2315,
{ 266: } 2330,
{ 267: } 2358,
{ 268: } 2359,
{ 269: } 2363,
{ 270: } 2364,
{ 271: } 2381,
{ 272: } 2381,
{ 273: } 2382,
{ 274: } 2382,
{ 275: } 2410,
{ 276: } 2438,
{ 277: } 2454,
{ 278: } 2454,
{ 279: } 2454,
{ 280: } 2455,
{ 281: } 2456,
{ 282: } 2456,
{ 283: } 2456
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
{ 168: } 200,
{ 169: } 201,
{ 170: } 201,
{ 171: } 201,
{ 172: } 205,
{ 173: } 205,
{ 174: } 205,
{ 175: } 205,
{ 176: } 205,
{ 177: } 206,
{ 178: } 206,
{ 179: } 206,
{ 180: } 210,
{ 181: } 210,
{ 182: } 210,
{ 183: } 210,
{ 184: } 210,
{ 185: } 210,
{ 186: } 210,
{ 187: } 210,
{ 188: } 215,
{ 189: } 220,
{ 190: } 225,
{ 191: } 230,
{ 192: } 235,
{ 193: } 240,
{ 194: } 246,
{ 195: } 251,
{ 196: } 256,
{ 197: } 261,
{ 198: } 266,
{ 199: } 271,
{ 200: } 276,
{ 201: } 281,
{ 202: } 286,
{ 203: } 291,
{ 204: } 291,
{ 205: } 294,
{ 206: } 294,
{ 207: } 298,
{ 208: } 298,
{ 209: } 305,
{ 210: } 305,
{ 211: } 305,
{ 212: } 306,
{ 213: } 307,
{ 214: } 311,
{ 215: } 315,
{ 216: } 319,
{ 217: } 319,
{ 218: } 319,
{ 219: } 319,
{ 220: } 319,
{ 221: } 319,
{ 222: } 319,
{ 223: } 319,
{ 224: } 323,
{ 225: } 323,
{ 226: } 330,
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
{ 248: } 333,
{ 249: } 333,
{ 250: } 333,
{ 251: } 334,
{ 252: } 334,
{ 253: } 338,
{ 254: } 338,
{ 255: } 345,
{ 256: } 350,
{ 257: } 351,
{ 258: } 355,
{ 259: } 356,
{ 260: } 357,
{ 261: } 358,
{ 262: } 358,
{ 263: } 365,
{ 264: } 365,
{ 265: } 365,
{ 266: } 370,
{ 267: } 373,
{ 268: } 373,
{ 269: } 373,
{ 270: } 374,
{ 271: } 374,
{ 272: } 374,
{ 273: } 374,
{ 274: } 374,
{ 275: } 374,
{ 276: } 374,
{ 277: } 374,
{ 278: } 381,
{ 279: } 381,
{ 280: } 381,
{ 281: } 381,
{ 282: } 381,
{ 283: } 381
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
{ 167: } 199,
{ 168: } 200,
{ 169: } 200,
{ 170: } 200,
{ 171: } 204,
{ 172: } 204,
{ 173: } 204,
{ 174: } 204,
{ 175: } 204,
{ 176: } 205,
{ 177: } 205,
{ 178: } 205,
{ 179: } 209,
{ 180: } 209,
{ 181: } 209,
{ 182: } 209,
{ 183: } 209,
{ 184: } 209,
{ 185: } 209,
{ 186: } 209,
{ 187: } 214,
{ 188: } 219,
{ 189: } 224,
{ 190: } 229,
{ 191: } 234,
{ 192: } 239,
{ 193: } 245,
{ 194: } 250,
{ 195: } 255,
{ 196: } 260,
{ 197: } 265,
{ 198: } 270,
{ 199: } 275,
{ 200: } 280,
{ 201: } 285,
{ 202: } 290,
{ 203: } 290,
{ 204: } 293,
{ 205: } 293,
{ 206: } 297,
{ 207: } 297,
{ 208: } 304,
{ 209: } 304,
{ 210: } 304,
{ 211: } 305,
{ 212: } 306,
{ 213: } 310,
{ 214: } 314,
{ 215: } 318,
{ 216: } 318,
{ 217: } 318,
{ 218: } 318,
{ 219: } 318,
{ 220: } 318,
{ 221: } 318,
{ 222: } 318,
{ 223: } 322,
{ 224: } 322,
{ 225: } 329,
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
{ 247: } 332,
{ 248: } 332,
{ 249: } 332,
{ 250: } 333,
{ 251: } 333,
{ 252: } 337,
{ 253: } 337,
{ 254: } 344,
{ 255: } 349,
{ 256: } 350,
{ 257: } 354,
{ 258: } 355,
{ 259: } 356,
{ 260: } 357,
{ 261: } 357,
{ 262: } 364,
{ 263: } 364,
{ 264: } 364,
{ 265: } 369,
{ 266: } 372,
{ 267: } 372,
{ 268: } 372,
{ 269: } 373,
{ 270: } 373,
{ 271: } 373,
{ 272: } 373,
{ 273: } 373,
{ 274: } 373,
{ 275: } 373,
{ 276: } 373,
{ 277: } 380,
{ 278: } 380,
{ 279: } 380,
{ 280: } 380,
{ 281: } 380,
{ 282: } 380,
{ 283: } 380
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
{ 82: } ( len: 1; sym: -28 ),
{ 83: } ( len: 1; sym: -28 ),
{ 84: } ( len: 1; sym: -28 ),
{ 85: } ( len: 2; sym: -15 ),
{ 86: } ( len: 3; sym: -15 ),
{ 87: } ( len: 2; sym: -15 ),
{ 88: } ( len: 2; sym: -15 ),
{ 89: } ( len: 3; sym: -15 ),
{ 90: } ( len: 3; sym: -15 ),
{ 91: } ( len: 1; sym: -15 ),
{ 92: } ( len: 4; sym: -15 ),
{ 93: } ( len: 2; sym: -15 ),
{ 94: } ( len: 4; sym: -15 ),
{ 95: } ( len: 3; sym: -15 ),
{ 96: } ( len: 2; sym: -31 ),
{ 97: } ( len: 3; sym: -31 ),
{ 98: } ( len: 2; sym: -27 ),
{ 99: } ( len: 3; sym: -27 ),
{ 100: } ( len: 2; sym: -27 ),
{ 101: } ( len: 4; sym: -27 ),
{ 102: } ( len: 2; sym: -27 ),
{ 103: } ( len: 4; sym: -27 ),
{ 104: } ( len: 3; sym: -27 ),
{ 105: } ( len: 0; sym: -27 ),
{ 106: } ( len: 1; sym: -29 ),
{ 107: } ( len: 3; sym: -32 ),
{ 108: } ( len: 3; sym: -32 ),
{ 109: } ( len: 3; sym: -32 ),
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
{ 123: } ( len: 1; sym: -32 ),
{ 124: } ( len: 3; sym: -33 ),
{ 125: } ( len: 1; sym: -35 ),
{ 126: } ( len: 0; sym: -35 ),
{ 127: } ( len: 1; sym: -34 ),
{ 128: } ( len: 1; sym: -34 ),
{ 129: } ( len: 1; sym: -34 ),
{ 130: } ( len: 1; sym: -34 ),
{ 131: } ( len: 3; sym: -34 ),
{ 132: } ( len: 3; sym: -34 ),
{ 133: } ( len: 2; sym: -34 ),
{ 134: } ( len: 2; sym: -34 ),
{ 135: } ( len: 2; sym: -34 ),
{ 136: } ( len: 4; sym: -34 ),
{ 137: } ( len: 4; sym: -34 ),
{ 138: } ( len: 5; sym: -34 ),
{ 139: } ( len: 6; sym: -34 ),
{ 140: } ( len: 4; sym: -34 ),
{ 141: } ( len: 3; sym: -34 ),
{ 142: } ( len: 8; sym: -34 ),
{ 143: } ( len: 4; sym: -34 ),
{ 144: } ( len: 3; sym: -17 ),
{ 145: } ( len: 1; sym: -17 ),
{ 146: } ( len: 0; sym: -17 ),
{ 147: } ( len: 3; sym: -37 ),
{ 148: } ( len: 1; sym: -37 ),
{ 149: } ( len: 1; sym: -19 ),
{ 150: } ( len: 2; sym: -18 ),
{ 151: } ( len: 4; sym: -18 ),
{ 152: } ( len: 3; sym: -36 ),
{ 153: } ( len: 1; sym: -36 ),
{ 154: } ( len: 0; sym: -36 ),
{ 155: } ( len: 1; sym: -38 )
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
     writeln(outfile,'interface');
     writeln(outfile);
     writeln(outfile,'{');
     writeln(outfile,'  Automatically converted by H2Pas ',version,' from ',inputfilename);
     writeln(outfile,'  The following command line parameters were used:');
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
      writeln(outfile,'implementation');
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

{
  $Log$
  Revision 1.1.2.1  2001-04-08 12:23:13  peter
    * updated to more stable version in the main branch

  Revision 1.2  2000/12/27 21:59:58  peter
    * big update, it now converts much more files without serious errors
    + h2pas preprocessor to remove specific defines from a .h file
      (you need to run h2paspp manual)

  Revision 1.1  2000/07/13 10:16:23  michael
  + Initial import
}