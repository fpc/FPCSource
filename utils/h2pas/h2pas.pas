
(* Yacc parser template (TP Yacc V3.0), V1.2 6-17-91 AG *)

(* global definitions: *)

program h2pas;

(*
    $Id: h2pas.y,v 1.10 2005/02/20 11:09:41 florian Exp $
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

  {ctypes strings}
  const
    cint8_STR       = 'cint8';
    cuint8_STR      = 'cuint8';
    cchar_STR       = 'cchar';
    cschar_STR      = 'cschar';
    cuchar_STR      = 'cuchar';
    
    cint16_STR      = 'cint16';
    cuint16_STR     = 'cuint16';
    cshort_STR      = 'cshort';
    csshort_STR     = 'csshort';
    cushort_STR     = 'cushort';
    
    cint32_STR      = 'cint32';
    cuint32_STR     = 'cuint32';
    cint_STR        = 'cint';
    csint_STR       = 'csint';
    cuint_STR       = 'cuint';
    
    csigned_STR     = 'csigned';
    cunsigned_STR   = 'cunsigned';
    
    cint64_STR      = 'cint64';
    cuint64_STR     = 'cuint64';
    clonglong_STR   = 'clonglong';
    cslonglong_STR  = 'cslonglong';
    culonglong_STR  = 'culonglong';

    cbool_STR       = 'cbool';
    
    clong_STR       = 'clong';
    cslong_STR      = 'cslong';
    culong_STR      = 'culong';
    
    cfloat_STR      = 'cfloat';
    cdouble_STR     = 'cdouble';
    clongdouble_STR = 'clongdouble';
  
  const
    MAX_CTYPESARRAY = 25;
    CTypesArray : array [0..MAX_CTYPESARRAY] of string =
      (cint8_STR,     cuint8_STR, 
       cchar_STR,     cschar_STR,     cuchar_STR,
       cint16_STR,    cuint16_STR, 
       cshort_STR,    csshort_STR,    cushort_STR,
       csigned_STR,   cunsigned_STR, 
       cint32_STR,    cuint32_STR,    cint_STR,
       csint_STR,     cuint_STR,       
       cint64_STR,    cuint64_STR,
       clonglong_STR, cslonglong_STR, culonglong_STR,
       
       cbool_STR,     
       clong_STR,      cslong_STR,    culong_STR);  
    
    
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

    function IsACType(const s : String) : Boolean;
    var i : Integer;
    begin
      IsACType := True;
      for i := 0 to MAX_CTYPESARRAY do
      begin
        if s = CTypesArray[i] then
        begin
          Exit;
        end;
      end;
      IsACType := False;
    end;

    function PointerName(const s:string):string;
      var
        i : longint;
      begin
        if UseCTypesUnit then
        begin
          if IsACType(s) then
          begin
            PointerName := 'p'+s;
            exit;
          end;
        end;
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
            else
              begin
                writeln(ord(p^.typ));
                internalerror(2);
              end;
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
                  if UseCtypesUnit then
                  begin
                    if not IsACType(p^.p) then
                    begin
                      PTypeList.Add('P'+p^.str);
                    end;
                  end
                  else
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
                      if UseCTypesUnit and (IsACType(p^.p1^.p)=False) then
                        write(outfile,'P')
                      else
                        write(outfile,'p');
                      pointerprefix:=true;
                    end
                    else
                    begin
                      if UseCTypesUnit and (IsACType(p^.p1^.p)=False) then
                        write(outfile,'^')
                      else
                        write(outfile,'p');
                    end;
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
                        if UseCTypesUnit and (IsACType( p^.p2^.p )=False) then
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
                        if UseCTypesUnit and (IsACType(p^.p2^.str)=false) then
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


    procedure write_statement_block(var outfile:text; p : presobject);
      begin
        writeln(outfile,aktspace,'begin');
        while assigned(p) do
          begin
            shift(2);
            if assigned(p^.p1) then
              begin
                case p^.p1^.typ of
                  t_whilenode:
                    begin
                      write(outfile,aktspace,'while ');
                      write_expr(outfile,p^.p1^.p1);
                      writeln(outfile,' do');
                      shift(2);
                      write_statement_block(outfile,p^.p1^.p2);
                      popshift;
                    end;
                  else
                    begin
                      write(outfile,aktspace);
                      write_expr(outfile,p^.p1);
                      writeln(outfile,';');
                    end;
                end;
              end;
            p:=p^.next;
            popshift;
          end;
        writeln(outfile,aktspace,'end;');
      end;

const _WHILE = 257;
const _FOR = 258;
const _DO = 259;
const _GOTO = 260;
const _CONTINUE = 261;
const _BREAK = 262;
const TYPEDEF = 263;
const DEFINE = 264;
const COLON = 265;
const SEMICOLON = 266;
const COMMA = 267;
const LKLAMMER = 268;
const RKLAMMER = 269;
const LECKKLAMMER = 270;
const RECKKLAMMER = 271;
const LGKLAMMER = 272;
const RGKLAMMER = 273;
const STRUCT = 274;
const UNION = 275;
const ENUM = 276;
const ID = 277;
const NUMBER = 278;
const CSTRING = 279;
const SHORT = 280;
const UNSIGNED = 281;
const LONG = 282;
const INT = 283;
const REAL = 284;
const _CHAR = 285;
const VOID = 286;
const _CONST = 287;
const _FAR = 288;
const _HUGE = 289;
const _NEAR = 290;
const NEW_LINE = 291;
const SPACE_DEFINE = 292;
const EXTERN = 293;
const STDCALL = 294;
const CDECL = 295;
const CALLBACK = 296;
const PASCAL = 297;
const WINAPI = 298;
const APIENTRY = 299;
const WINGDIAPI = 300;
const SYS_TRAP = 301;
const _PACKED = 302;
const ELLIPSIS = 303;
const _ASSIGN = 304;
const R_AND = 305;
const EQUAL = 306;
const UNEQUAL = 307;
const GT = 308;
const LT = 309;
const GTE = 310;
const LTE = 311;
const QUESTIONMARK = 312;
const _OR = 313;
const _AND = 314;
const _PLUS = 315;
const MINUS = 316;
const _SHR = 317;
const _SHL = 318;
const STAR = 319;
const _SLASH = 320;
const _NOT = 321;
const PSTAR = 322;
const P_AND = 323;
const POINT = 324;
const DEREF = 325;
const STICK = 326;
const SIGNED = 327;

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
         yyval:=yyv[yysp-1]; 
       end;
  22 : begin
         yyval:=new(presobject,init_two(t_whilenode,yyv[yysp-2],yyv[yysp-0])); 
       end;
  23 : begin
         
         yyval:=new(presobject,init_one(t_statement_list,yyv[yysp-1]));
         yyval^.next:=yyv[yysp-0];
         
       end;
  24 : begin
         
         yyval:=new(presobject,init_one(t_statement_list,yyv[yysp-0]));
         
       end;
  25 : begin
         
         yyval:=new(presobject,init_one(t_statement_list,nil));
         
       end;
  26 : begin
         
         yyval:=new(presobject,init_one(t_statement_list,nil));
         
       end;
  27 : begin
         yyval:=yyv[yysp-1]; 
       end;
  28 : begin
         
         IsExtern:=false;
         (* by default we must pop the args pushed on stack *)
         no_pop:=false;
         if (assigned(yyv[yysp-1])and assigned(yyv[yysp-1]^.p1)and assigned(yyv[yysp-1]^.p1^.p1))
         and (yyv[yysp-1]^.p1^.p1^.typ=t_procdef) then
         begin
         repeat
         If UseLib then
         IsExtern:=true
         else
         IsExtern:=assigned(yyv[yysp-4])and(yyv[yysp-4]^.str='extern');
         no_pop:=assigned(yyv[yysp-2]) and (yyv[yysp-2]^.str='no_pop');
         
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
         if assigned(yyv[yysp-3]) then
         if (yyv[yysp-3]^.typ=t_void) and (yyv[yysp-1]^.p1^.p1^.p1=nil) then
         begin
         if createdynlib then
         begin
         write(outfile,yyv[yysp-1]^.p1^.p2^.p,' : procedure');
         end
         else
         begin
         shift(10);
         write(outfile,'procedure ',yyv[yysp-1]^.p1^.p2^.p);
         end;
         if assigned(yyv[yysp-1]^.p1^.p1^.p2) then
         write_args(outfile,yyv[yysp-1]^.p1^.p1^.p2);
         if createdynlib then
         begin
         loaddynlibproc.add('pointer('+yyv[yysp-1]^.p1^.p2^.p+'):=GetProcAddress(hlib,'''+yyv[yysp-1]^.p1^.p2^.p+''');');
         freedynlibproc.add(yyv[yysp-1]^.p1^.p2^.p+':=nil;');
         end
         else if not IsExtern then
         begin
         write(implemfile,'procedure ',yyv[yysp-1]^.p1^.p2^.p);
         if assigned(yyv[yysp-1]^.p1^.p1^.p2) then
         write_args(implemfile,yyv[yysp-1]^.p1^.p1^.p2);
         end;
         end
         else
         begin
         if createdynlib then
         begin
         write(outfile,yyv[yysp-1]^.p1^.p2^.p,' : function');
         end
         else
         begin
         shift(9);
         write(outfile,'function ',yyv[yysp-1]^.p1^.p2^.p);
         end;
         
         if assigned(yyv[yysp-1]^.p1^.p1^.p2) then
         write_args(outfile,yyv[yysp-1]^.p1^.p1^.p2);
         write(outfile,':');
         write_p_a_def(outfile,yyv[yysp-1]^.p1^.p1^.p1,yyv[yysp-3]);
         if createdynlib then
         begin
         loaddynlibproc.add('pointer('+yyv[yysp-1]^.p1^.p2^.p+'):=GetProcAddress(hlib,'''+yyv[yysp-1]^.p1^.p2^.p+''');');
         freedynlibproc.add(yyv[yysp-1]^.p1^.p2^.p+':=nil;');
         end
         else if not IsExtern then
         begin
         write(implemfile,'function ',yyv[yysp-1]^.p1^.p2^.p);
         if assigned(yyv[yysp-1]^.p1^.p1^.p2) then
         write_args(implemfile,yyv[yysp-1]^.p1^.p1^.p2);
         write(implemfile,':');
         write_p_a_def(implemfile,yyv[yysp-1]^.p1^.p1^.p1,yyv[yysp-3]);
         end;
         end;
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
         Write(outfile,' External_library name ''',yyv[yysp-1]^.p1^.p2^.p,'''');
         end;
         writeln(outfile,';');
         end
         else
         begin
         writeln(outfile,';');
         if not IsExtern then
         begin
         writeln(implemfile,';');
         shift(2);
         if yyv[yysp-0]^.typ=t_statement_list then
         write_statement_block(implemfile,yyv[yysp-0]);
         popshift;
         end;
         end;
         IsExtern:=false;
         if not(compactmode) and not(createdynlib) then
         writeln(outfile);
         until not NeedEllipsisOverload;
         end
         else (* yyv[yysp-1]^.p1^.p1^.typ=t_procdef *)
         if assigned(yyv[yysp-1])and assigned(yyv[yysp-1]^.p1) then
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
         
         IsExtern:=assigned(yyv[yysp-4])and(yyv[yysp-4]^.str='extern');
         (* walk through all declarations *)
         hp:=yyv[yysp-1];
         while assigned(hp) and assigned(hp^.p1) do
         begin
         (* write new var name *)
         if assigned(hp^.p1^.p2) and assigned(hp^.p1^.p2^.p) then
         write(outfile,aktspace,hp^.p1^.p2^.p);
         write(outfile,' : ');
         shift(2);
         (* write its type *)
         write_p_a_def(outfile,hp^.p1^.p1,yyv[yysp-3]);
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
         if assigned(yyv[yysp-4]) then
         dispose(yyv[yysp-4],done);
         if assigned(yyv[yysp-3]) then
         dispose(yyv[yysp-3],done);
         if assigned(yyv[yysp-2]) then
         dispose(yyv[yysp-2],done);
         if assigned(yyv[yysp-1]) then
         dispose(yyv[yysp-1],done);
         if assigned(yyv[yysp-0]) then
         dispose(yyv[yysp-0],done);
         
       end;
  29 : begin
         
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
  30 : begin
         
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
  31 : begin
         
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
  32 : begin
         
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
  33 : begin
         
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
  34 : begin
         
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
  35 : begin
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
  36 : begin
         
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
  37 : begin
         
         (* DEFINE dname SPACE_DEFINE NEW_LINE *)
         writeln(outfile,'{$define ',yyv[yysp-2]^.p,'}',aktspace,commentstr);
         flush(outfile);
         if assigned(yyv[yysp-2])then
         dispose(yyv[yysp-2],done);
         
       end;
  38 : begin
         
         writeln(outfile,'{$define ',yyv[yysp-1]^.p,'}',aktspace,commentstr);
         flush(outfile);
         if assigned(yyv[yysp-1])then
         dispose(yyv[yysp-1],done);
         
       end;
  39 : begin
         
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
  40 : begin
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
  41 : begin
         yyval:=yyv[yysp-1];
       end;
  42 : begin
         writeln(outfile,' in member_list *)');
         yyerrok;
         yyval:=nil;
         
       end;
  43 : begin
         yyval:=yyv[yysp-1];
       end;
  44 : begin
         writeln(outfile,' in enum_list *)');
         yyerrok;
         yyval:=nil;
         
       end;
  45 : begin
         
         if (not is_packed) and (not packrecords) then
         writeln(outfile,'{$PACKRECORDS 1}');
         is_packed:=true;
         yyval:=new(presobject,init_two(t_structdef,yyv[yysp-1],yyv[yysp-2]));
         
       end;
  46 : begin
         
         if (is_packed) and (not packrecords) then
         writeln(outfile,'{$PACKRECORDS 4}');
         is_packed:=false;
         yyval:=new(presobject,init_two(t_structdef,yyv[yysp-0],yyv[yysp-1]));
         
       end;
  47 : begin
         
         if (not is_packed) and (not packrecords) then
         writeln(outfile,'{$PACKRECORDS 1}');
         is_packed:=true;
         yyval:=new(presobject,init_two(t_uniondef,yyv[yysp-1],yyv[yysp-2]));
         
       end;
  48 : begin
         
         yyval:=new(presobject,init_two(t_uniondef,yyv[yysp-0],yyv[yysp-1]));
         
       end;
  49 : begin
         
         yyval:=yyv[yysp-0];
         
       end;
  50 : begin
         
         yyval:=yyv[yysp-0];
         
       end;
  51 : begin
         
         yyval:=new(presobject,init_two(t_enumdef,yyv[yysp-0],yyv[yysp-1]));
         
       end;
  52 : begin
         
         yyval:=yyv[yysp-0];
         
       end;
  53 : begin
         
         if not stripinfo then
         writeln(outfile,'(* Const before type ignored *)');
         yyval:=yyv[yysp-0];
         
       end;
  54 : begin
         
         if (not is_packed) and (not packrecords)then
         writeln(outfile,'{$PACKRECORDS 1}');
         is_packed:=true;
         yyval:=new(presobject,init_one(t_uniondef,yyv[yysp-1]));
         
       end;
  55 : begin
         
         yyval:=new(presobject,init_one(t_uniondef,yyv[yysp-0]));
         
       end;
  56 : begin
         
         if (not is_packed) and (not packrecords) then
         writeln(outfile,'{$PACKRECORDS 1}');
         is_packed:=true;
         yyval:=new(presobject,init_one(t_structdef,yyv[yysp-1]));
         
       end;
  57 : begin
         
         if (is_packed) and (not packrecords) then
         writeln(outfile,'{$PACKRECORDS 4}');
         is_packed:=false;
         yyval:=new(presobject,init_one(t_structdef,yyv[yysp-0]));
         
       end;
  58 : begin
         
         yyval:=new(presobject,init_one(t_enumdef,yyv[yysp-0]));
         
       end;
  59 : begin
         
         yyval:=yyv[yysp-0];
         
       end;
  60 : begin
         yyval:=yyv[yysp-0]; 
       end;
  61 : begin
         
         yyval:=new(presobject,init_one(t_memberdeclist,yyv[yysp-1]));
         yyval^.next:=yyv[yysp-0];
         
       end;
  62 : begin
         
         yyval:=new(presobject,init_one(t_memberdeclist,yyv[yysp-0]));
         
       end;
  63 : begin
         
         yyval:=new(presobject,init_two(t_memberdec,yyv[yysp-2],yyv[yysp-1]));
         
       end;
  64 : begin
         (*dname*)
         yyval:=new(presobject,init_id(act_token));
         
       end;
  65 : begin
         
         hp:=yyv[yysp-0];
         yyval:=hp;
         if assigned(hp) then
         begin
         s:=strpas(hp^.p);
         if UseCTypesUnit then
         begin          
         if s=cint_STR then
         s:=csint_STR
         else if s=cshort_STR then
         s:=csshort_STR
         else if s=cchar_STR then
         s:=cschar_STR
         else if s=clong_STR then
         s:=cslong_STR              
         else if s=clonglong_STR then
         s:=cslonglong_STR
         else
         s:='';
         end
         else
         begin
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
         end; 
         if s<>'' then
         hp^.setstr(s);
         end;
         
       end;
  66 : begin
         
         hp:=yyv[yysp-0];
         yyval:=hp;
         if assigned(hp) then
         begin
         s:=strpas(hp^.p);
         if UseCTypesUnit then
         begin
         if s=cint_STR then
         s:=cuint_STR
         else if s=cshort_STR then
         s:=cushort_STR
         else if s=cchar_STR then
         s:=cuchar_STR
         else if s=clong_STR then
         s:=culong_STR
         else if s=clonglong_STR then
         s:=culonglong_STR
         else
         s:='';          
         end
         else
         begin
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
         end;
         if s<>'' then
         hp^.setstr(s);
         end;
         
       end;
  67 : begin
         
         if UseCTypesUnit then
         yyval:=new(presobject,init_id(cint_STR)) 
         else       
         yyval:=new(presobject,init_intid(INT_STR));
         
       end;
  68 : begin
         
         if UseCTypesUnit then
         yyval:=new(presobject,init_id(clong_STR)) 
         else      
         yyval:=new(presobject,init_intid(INT_STR));
         
       end;
  69 : begin
         
         if UseCTypesUnit then
         yyval:=new(presobject,init_id(clong_STR)) 
         else    
         yyval:=new(presobject,init_intid(INT_STR));
         
       end;
  70 : begin
         
         if UseCTypesUnit then
         yyval:=new(presobject,init_id(clonglong_STR)) 
         else        
         yyval:=new(presobject,init_intid(INT64_STR));
         
       end;
  71 : begin
         
         if UseCTypesUnit then
         yyval:=new(presobject,init_id(clonglong_STR)) 
         else      
         yyval:=new(presobject,init_intid(INT64_STR));
         
       end;
  72 : begin
         
         if UseCTypesUnit then
         yyval:=new(presobject,init_id(cshort_STR)) 
         else      
         yyval:=new(presobject,init_intid(SHORT_STR));
         
       end;
  73 : begin
         
         if UseCTypesUnit then
         yyval:=new(presobject,init_id(csint_STR)) 
         else      
         yyval:=new(presobject,init_intid(SHORT_STR));
         
       end;
  74 : begin
         
         yyval:=new(presobject,init_intid(REAL_STR));
         
       end;
  75 : begin
         
         yyval:=new(presobject,init_no(t_void));
         
       end;
  76 : begin
         
         if UseCTypesUnit then
         yyval:=new(presobject,init_id(cchar_STR)) 
         else
         yyval:=new(presobject,init_intid(CHAR_STR));
         
       end;
  77 : begin
         
         if UseCTypesUnit then
         yyval:=new(presobject,init_id(cunsigned_STR)) 
         else     
         yyval:=new(presobject,init_intid(UINT_STR));
         
       end;
  78 : begin
         
         yyval:=yyv[yysp-0];
         
       end;
  79 : begin
         
         yyval:=yyv[yysp-0];
         tn:=yyval^.str;
         if removeunderscore and
         (length(tn)>1) and (tn[1]='_') then
         yyval^.setstr(Copy(tn,2,length(tn)-1));
         
       end;
  80 : begin
         
         yyval:=yyv[yysp-2];
         hp:=yyv[yysp-2];
         while assigned(hp^.next) do
         hp:=hp^.next;
         hp^.next:=new(presobject,init_one(t_declist,yyv[yysp-0]));
         
       end;
  81 : begin
         
         writeln(outfile,' in declarator_list *)');
         yyval:=yyv[yysp-0];
         yyerrok;
         
       end;
  82 : begin
         
         writeln(outfile,' in declarator_list *)');
         yyerrok;
         
       end;
  83 : begin
         
         yyval:=new(presobject,init_one(t_declist,yyv[yysp-0]));
         
       end;
  84 : begin
         
         yyval:=new(presobject,init_two(t_arg,yyv[yysp-1],yyv[yysp-0]));
         
       end;
  85 : begin
         
         (* type_specifier STAR declarator *)
         hp:=new(presobject,init_one(t_pointerdef,yyv[yysp-2]));
         yyval:=new(presobject,init_two(t_arg,hp,yyv[yysp-0]));
         
       end;
  86 : begin
         
         yyval:=new(presobject,init_two(t_arg,yyv[yysp-1],yyv[yysp-0]));
         
       end;
  87 : begin
         
         yyval:=new(presobject,init_two(t_arglist,yyv[yysp-0],nil));
         
       end;
  88 : begin
         
         yyval:=new(presobject,init_two(t_arglist,yyv[yysp-2],nil));
         yyval^.next:=yyv[yysp-0];
         
       end;
  89 : begin
         
         yyval:=new(presobject,init_two(t_arglist,ellipsisarg,nil));
         
       end;
  90 : begin
         
         yyval:=nil;
         
       end;
  91 : begin
         yyval:=new(presobject,init_id('far'));
       end;
  92 : begin
         yyval:=new(presobject,init_id('near'));
       end;
  93 : begin
         yyval:=new(presobject,init_id('huge'));
       end;
  94 : begin
         
         if not stripinfo then
         writeln(outfile,'(* Const before declarator ignored *)');
         yyval:=yyv[yysp-0];
         
       end;
  95 : begin
         
         if not stripinfo then
         writeln(outfile,aktspace,'(* ',yyv[yysp-2]^.p,' ignored *)');
         dispose(yyv[yysp-2],done);
         hp:=yyv[yysp-0];
         yyval:=hp;
         while assigned(hp^.p1) do
         hp:=hp^.p1;
         hp^.p1:=new(presobject,init_one(t_pointerdef,nil));
         
       end;
  96 : begin
         
         (* %prec PSTAR this was wrong!! *)
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
         hp^.p1:=new(presobject,init_one(t_addrdef,nil));
         
       end;
  98 : begin
         
         (*  size specifier supported *)
         hp:=new(presobject,init_one(t_size_specifier,yyv[yysp-0]));
         yyval:=new(presobject,init_three(t_dec,nil,yyv[yysp-2],hp));
         
       end;
  99 : begin
         
         if not stripinfo then
         writeln(outfile,'(* Warning : default value for ',yyv[yysp-2]^.p,' ignored *)');
         hp:=new(presobject,init_one(t_default_value,yyv[yysp-0]));
         yyval:=new(presobject,init_three(t_dec,nil,yyv[yysp-2],hp));
         
       end;
 100 : begin
         
         yyval:=new(presobject,init_two(t_dec,nil,yyv[yysp-0]));
         
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
         
         (* this is translated into a pointer *)
         hp:=yyv[yysp-2];
         yyval:=hp;
         while assigned(hp^.p1) do
         hp:=hp^.p1;
         hp^.p1:=new(presobject,init_two(t_arraydef,nil,nil));
         
       end;
 105 : begin
         
         yyval:=yyv[yysp-1];
         
       end;
 106 : begin
         yyval := yyv[yysp-1];
       end;
 107 : begin
         yyval := yyv[yysp-2];
       end;
 108 : begin
         
         if not stripinfo then
         writeln(outfile,'(* Const before abstract_declarator ignored *)');
         yyval:=yyv[yysp-0];
         
       end;
 109 : begin
         
         if not stripinfo then
         writeln(outfile,aktspace,'(* ',yyv[yysp-2]^.p,' ignored *)');
         dispose(yyv[yysp-2],done);
         hp:=yyv[yysp-0];
         yyval:=hp;
         while assigned(hp^.p1) do
         hp:=hp^.p1;
         hp^.p1:=new(presobject,init_one(t_pointerdef,nil));
         
       end;
 110 : begin
         
         hp:=yyv[yysp-0];
         yyval:=hp;
         while assigned(hp^.p1) do
         hp:=hp^.p1;
         hp^.p1:=new(presobject,init_one(t_pointerdef,nil));
         
       end;
 111 : begin
         
         hp:=yyv[yysp-3];
         yyval:=hp;
         while assigned(hp^.p1) do
         hp:=hp^.p1;
         hp^.p1:=new(presobject,init_two(t_procdef,nil,yyv[yysp-1]));
         
       end;
 112 : begin
         
         hp:=yyv[yysp-1];
         yyval:=hp;
         while assigned(hp^.p1) do
         hp:=hp^.p1;
         hp^.p1:=new(presobject,init_two(t_procdef,nil,nil));
         
       end;
 113 : begin
         
         hp:=yyv[yysp-3];
         yyval:=hp;
         while assigned(hp^.p1) do
         hp:=hp^.p1;
         hp^.p1:=new(presobject,init_two(t_arraydef,nil,yyv[yysp-1]));
         
       end;
 114 : begin
         
         (* this is translated into a pointer *)
         hp:=yyv[yysp-2];
         yyval:=hp;
         while assigned(hp^.p1) do
         hp:=hp^.p1;
         hp^.p1:=new(presobject,init_two(t_arraydef,nil,nil));
         
       end;
 115 : begin
         
         yyval:=yyv[yysp-1];
         
       end;
 116 : begin
         
         yyval:=new(presobject,init_two(t_dec,nil,nil));
         
       end;
 117 : begin
         yyval:=yyv[yysp-0]; 
       end;
 118 : begin
         yyval:=new(presobject,init_bop(':=',yyv[yysp-2],yyv[yysp-0])); 
       end;
 119 : begin
         yyval:=new(presobject,init_bop('=',yyv[yysp-2],yyv[yysp-0]));
       end;
 120 : begin
         yyval:=new(presobject,init_bop('<>',yyv[yysp-2],yyv[yysp-0]));
       end;
 121 : begin
         yyval:=new(presobject,init_bop('>',yyv[yysp-2],yyv[yysp-0]));
       end;
 122 : begin
         yyval:=new(presobject,init_bop('>=',yyv[yysp-2],yyv[yysp-0]));
       end;
 123 : begin
         yyval:=new(presobject,init_bop('<',yyv[yysp-2],yyv[yysp-0]));
       end;
 124 : begin
         yyval:=new(presobject,init_bop('<=',yyv[yysp-2],yyv[yysp-0]));
       end;
 125 : begin
         yyval:=new(presobject,init_bop('+',yyv[yysp-2],yyv[yysp-0]));
       end;
 126 : begin
         yyval:=new(presobject,init_bop('-',yyv[yysp-2],yyv[yysp-0]));
       end;
 127 : begin
         yyval:=new(presobject,init_bop('*',yyv[yysp-2],yyv[yysp-0]));
       end;
 128 : begin
         yyval:=new(presobject,init_bop('/',yyv[yysp-2],yyv[yysp-0]));
       end;
 129 : begin
         yyval:=new(presobject,init_bop(' or ',yyv[yysp-2],yyv[yysp-0]));
       end;
 130 : begin
         yyval:=new(presobject,init_bop(' and ',yyv[yysp-2],yyv[yysp-0]));
       end;
 131 : begin
         yyval:=new(presobject,init_bop(' not ',yyv[yysp-2],yyv[yysp-0]));
       end;
 132 : begin
         yyval:=new(presobject,init_bop(' shl ',yyv[yysp-2],yyv[yysp-0]));
       end;
 133 : begin
         yyval:=new(presobject,init_bop(' shr ',yyv[yysp-2],yyv[yysp-0]));
       end;
 134 : begin
         
         yyv[yysp-0]^.p1:=yyv[yysp-2];
         yyval:=yyv[yysp-0];
         inc(if_nb);
         yyval^.p:=strpnew('if_local'+str(if_nb));
         
       end;
 135 : begin
         yyval:=yyv[yysp-0];
       end;
 136 : begin
         (* if A then B else C *)
         yyval:=new(presobject,init_three(t_ifexpr,nil,yyv[yysp-2],yyv[yysp-0]));
       end;
 137 : begin
         yyval:=yyv[yysp-0]; 
       end;
 138 : begin
         yyval:=nil;
       end;
 139 : begin
         
         yyval:=yyv[yysp-0];
         
       end;
 140 : begin
         
         yyval:=yyv[yysp-0];
         
       end;
 141 : begin
         
         (* remove L prefix for widestrings *)
         s:=act_token;
         if Win32headers and (s[1]='L') then
         delete(s,1,1);
         yyval:=new(presobject,init_id(''''+copy(s,2,length(s)-2)+''''));
         
       end;
 142 : begin
         
         yyval:=new(presobject,init_id(act_token));
         
       end;
 143 : begin
         
         yyval:=new(presobject,init_bop('.',yyv[yysp-2],yyv[yysp-0]));
         
       end;
 144 : begin
         
         yyval:=new(presobject,init_bop('^.',yyv[yysp-2],yyv[yysp-0]));
         
       end;
 145 : begin
         
         yyval:=new(presobject,init_preop('-',yyv[yysp-0]));
         
       end;
 146 : begin
         
         yyval:=new(presobject,init_preop('@',yyv[yysp-0]));
         
       end;
 147 : begin
         
         yyval:=new(presobject,init_preop(' not ',yyv[yysp-0]));
         
       end;
 148 : begin
         
         if assigned(yyv[yysp-0]) then
         yyval:=new(presobject,init_two(t_typespec,yyv[yysp-2],yyv[yysp-0]))
         else
         yyval:=yyv[yysp-2];
         
       end;
 149 : begin
         
         yyval:=new(presobject,init_two(t_typespec,yyv[yysp-2],yyv[yysp-0]));
         
       end;
 150 : begin
         
         hp:=new(presobject,init_one(t_pointerdef,yyv[yysp-3]));
         yyval:=new(presobject,init_two(t_typespec,hp,yyv[yysp-0]));
         
       end;
 151 : begin
         
         if not stripinfo then
         writeln(outfile,aktspace,'(* ',yyv[yysp-3]^.p,' ignored *)');
         dispose(yyv[yysp-3],done);
         write_type_specifier(outfile,yyv[yysp-4]);
         writeln(outfile,' ignored *)');
         hp:=new(presobject,init_one(t_pointerdef,yyv[yysp-4]));
         yyval:=new(presobject,init_two(t_typespec,hp,yyv[yysp-0]));
         
       end;
 152 : begin
         
         hp:=new(presobject,init_one(t_exprlist,yyv[yysp-3]));
         yyval:=new(presobject,init_three(t_funexprlist,hp,yyv[yysp-1],nil));
         
       end;
 153 : begin
         
         yyval:=yyv[yysp-1];
         
       end;
 154 : begin
         
         yyval:=new(presobject,init_two(t_callop,yyv[yysp-5],yyv[yysp-1]));
         
       end;
 155 : begin
         
         yyval:=new(presobject,init_two(t_arrayop,yyv[yysp-3],yyv[yysp-1]));
         
       end;
 156 : begin
         (*enum_element COMMA enum_list *)
         yyval:=yyv[yysp-2];
         yyval^.next:=yyv[yysp-0];
         
       end;
 157 : begin
         
         yyval:=yyv[yysp-0];
         
       end;
 158 : begin
         (* empty enum list *)
         yyval:=nil;
       end;
 159 : begin
         begin (*enum_element: dname _ASSIGN expr *)
         yyval:=new(presobject,init_two(t_enumlist,yyv[yysp-2],yyv[yysp-0]));
         end;
         
       end;
 160 : begin
         
         begin (*enum_element: dname*)
         yyval:=new(presobject,init_two(t_enumlist,yyv[yysp-0],nil));
         end;
         
       end;
 161 : begin
         
         if yyv[yysp-0]^.typ=t_funexprlist then
         yyval:=yyv[yysp-0]
         else
         yyval:=new(presobject,init_two(t_exprlist,yyv[yysp-0],nil));
         (* if here is a type specifier
         we know the return type *)
         if (yyv[yysp-0]^.typ=t_typespec) then
         yyval^.p3:=yyv[yysp-0]^.p1^.get_copy;
         
       end;
 162 : begin
         
         yyval:=yyv[yysp-0];
         
       end;
 163 : begin
         
         yyval:=yyv[yysp-1]
         
       end;
 164 : begin
         (*exprlist COMMA expr*)
         yyval:=yyv[yysp-2];
         yyv[yysp-2]^.next:=yyv[yysp-0];
         
       end;
 165 : begin
         
         yyval:=yyv[yysp-0];
         
       end;
 166 : begin
         (* empty expression list *)
         yyval:=nil; 
       end;
 167 : begin
         
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

yynacts   = 2724;
yyngotos  = 421;
yynstates = 307;
yynrules  = 167;

yya : array [1..yynacts] of YYARec = (
{ 0: }
  ( sym: 256; act: 7 ),
  ( sym: 263; act: 8 ),
  ( sym: 264; act: 9 ),
  ( sym: 274; act: 10 ),
  ( sym: 275; act: 11 ),
  ( sym: 276; act: 12 ),
  ( sym: 293; act: 13 ),
  ( sym: 277; act: -10 ),
  ( sym: 280; act: -10 ),
  ( sym: 281; act: -10 ),
  ( sym: 282; act: -10 ),
  ( sym: 283; act: -10 ),
  ( sym: 284; act: -10 ),
  ( sym: 285; act: -10 ),
  ( sym: 286; act: -10 ),
  ( sym: 287; act: -10 ),
  ( sym: 327; act: -10 ),
{ 1: }
  ( sym: 266; act: 14 ),
{ 2: }
  ( sym: 274; act: 20 ),
  ( sym: 275; act: 21 ),
  ( sym: 276; act: 22 ),
  ( sym: 277; act: 23 ),
  ( sym: 280; act: 24 ),
  ( sym: 281; act: 25 ),
  ( sym: 282; act: 26 ),
  ( sym: 283; act: 27 ),
  ( sym: 284; act: 28 ),
  ( sym: 285; act: 29 ),
  ( sym: 286; act: 30 ),
  ( sym: 287; act: 31 ),
  ( sym: 327; act: 32 ),
{ 3: }
{ 4: }
{ 5: }
  ( sym: 256; act: 7 ),
  ( sym: 263; act: 8 ),
  ( sym: 264; act: 9 ),
  ( sym: 274; act: 10 ),
  ( sym: 275; act: 11 ),
  ( sym: 276; act: 12 ),
  ( sym: 293; act: 13 ),
  ( sym: 0; act: -1 ),
  ( sym: 277; act: -10 ),
  ( sym: 280; act: -10 ),
  ( sym: 281; act: -10 ),
  ( sym: 282; act: -10 ),
  ( sym: 283; act: -10 ),
  ( sym: 284; act: -10 ),
  ( sym: 285; act: -10 ),
  ( sym: 286; act: -10 ),
  ( sym: 287; act: -10 ),
  ( sym: 327; act: -10 ),
{ 6: }
  ( sym: 0; act: 0 ),
{ 7: }
{ 8: }
  ( sym: 274; act: 38 ),
  ( sym: 275; act: 21 ),
  ( sym: 276; act: 22 ),
  ( sym: 277; act: 23 ),
  ( sym: 280; act: 24 ),
  ( sym: 281; act: 25 ),
  ( sym: 282; act: 26 ),
  ( sym: 283; act: 27 ),
  ( sym: 284; act: 28 ),
  ( sym: 285; act: 29 ),
  ( sym: 286; act: 30 ),
  ( sym: 287; act: 31 ),
  ( sym: 327; act: 32 ),
{ 9: }
  ( sym: 277; act: 23 ),
{ 10: }
  ( sym: 277; act: 23 ),
{ 11: }
  ( sym: 277; act: 23 ),
{ 12: }
  ( sym: 277; act: 23 ),
{ 13: }
{ 14: }
{ 15: }
{ 16: }
{ 17: }
{ 18: }
  ( sym: 294; act: 44 ),
  ( sym: 295; act: 45 ),
  ( sym: 296; act: 46 ),
  ( sym: 297; act: 47 ),
  ( sym: 298; act: 48 ),
  ( sym: 299; act: 49 ),
  ( sym: 300; act: 50 ),
  ( sym: 256; act: -18 ),
  ( sym: 268; act: -18 ),
  ( sym: 277; act: -18 ),
  ( sym: 287; act: -18 ),
  ( sym: 288; act: -18 ),
  ( sym: 289; act: -18 ),
  ( sym: 290; act: -18 ),
  ( sym: 314; act: -18 ),
  ( sym: 319; act: -18 ),
{ 19: }
{ 20: }
  ( sym: 256; act: 52 ),
  ( sym: 272; act: 53 ),
  ( sym: 277; act: 23 ),
{ 21: }
  ( sym: 256; act: 52 ),
  ( sym: 272; act: 53 ),
  ( sym: 277; act: 23 ),
{ 22: }
  ( sym: 256; act: 56 ),
  ( sym: 272; act: 57 ),
  ( sym: 277; act: 23 ),
{ 23: }
{ 24: }
  ( sym: 283; act: 58 ),
  ( sym: 256; act: -72 ),
  ( sym: 265; act: -72 ),
  ( sym: 266; act: -72 ),
  ( sym: 267; act: -72 ),
  ( sym: 268; act: -72 ),
  ( sym: 269; act: -72 ),
  ( sym: 270; act: -72 ),
  ( sym: 271; act: -72 ),
  ( sym: 272; act: -72 ),
  ( sym: 273; act: -72 ),
  ( sym: 277; act: -72 ),
  ( sym: 287; act: -72 ),
  ( sym: 288; act: -72 ),
  ( sym: 289; act: -72 ),
  ( sym: 290; act: -72 ),
  ( sym: 291; act: -72 ),
  ( sym: 294; act: -72 ),
  ( sym: 295; act: -72 ),
  ( sym: 296; act: -72 ),
  ( sym: 297; act: -72 ),
  ( sym: 298; act: -72 ),
  ( sym: 299; act: -72 ),
  ( sym: 300; act: -72 ),
  ( sym: 301; act: -72 ),
  ( sym: 304; act: -72 ),
  ( sym: 306; act: -72 ),
  ( sym: 307; act: -72 ),
  ( sym: 308; act: -72 ),
  ( sym: 309; act: -72 ),
  ( sym: 310; act: -72 ),
  ( sym: 311; act: -72 ),
  ( sym: 312; act: -72 ),
  ( sym: 313; act: -72 ),
  ( sym: 314; act: -72 ),
  ( sym: 315; act: -72 ),
  ( sym: 316; act: -72 ),
  ( sym: 317; act: -72 ),
  ( sym: 318; act: -72 ),
  ( sym: 319; act: -72 ),
  ( sym: 320; act: -72 ),
  ( sym: 321; act: -72 ),
  ( sym: 324; act: -72 ),
  ( sym: 325; act: -72 ),
{ 25: }
  ( sym: 280; act: 24 ),
  ( sym: 281; act: 25 ),
  ( sym: 282; act: 26 ),
  ( sym: 283; act: 27 ),
  ( sym: 284; act: 28 ),
  ( sym: 285; act: 29 ),
  ( sym: 286; act: 30 ),
  ( sym: 327; act: 32 ),
  ( sym: 256; act: -77 ),
  ( sym: 265; act: -77 ),
  ( sym: 266; act: -77 ),
  ( sym: 267; act: -77 ),
  ( sym: 268; act: -77 ),
  ( sym: 269; act: -77 ),
  ( sym: 270; act: -77 ),
  ( sym: 271; act: -77 ),
  ( sym: 272; act: -77 ),
  ( sym: 273; act: -77 ),
  ( sym: 277; act: -77 ),
  ( sym: 287; act: -77 ),
  ( sym: 288; act: -77 ),
  ( sym: 289; act: -77 ),
  ( sym: 290; act: -77 ),
  ( sym: 291; act: -77 ),
  ( sym: 294; act: -77 ),
  ( sym: 295; act: -77 ),
  ( sym: 296; act: -77 ),
  ( sym: 297; act: -77 ),
  ( sym: 298; act: -77 ),
  ( sym: 299; act: -77 ),
  ( sym: 300; act: -77 ),
  ( sym: 301; act: -77 ),
  ( sym: 304; act: -77 ),
  ( sym: 306; act: -77 ),
  ( sym: 307; act: -77 ),
  ( sym: 308; act: -77 ),
  ( sym: 309; act: -77 ),
  ( sym: 310; act: -77 ),
  ( sym: 311; act: -77 ),
  ( sym: 312; act: -77 ),
  ( sym: 313; act: -77 ),
  ( sym: 314; act: -77 ),
  ( sym: 315; act: -77 ),
  ( sym: 316; act: -77 ),
  ( sym: 317; act: -77 ),
  ( sym: 318; act: -77 ),
  ( sym: 319; act: -77 ),
  ( sym: 320; act: -77 ),
  ( sym: 321; act: -77 ),
  ( sym: 324; act: -77 ),
  ( sym: 325; act: -77 ),
{ 26: }
  ( sym: 282; act: 60 ),
  ( sym: 283; act: 61 ),
  ( sym: 256; act: -68 ),
  ( sym: 265; act: -68 ),
  ( sym: 266; act: -68 ),
  ( sym: 267; act: -68 ),
  ( sym: 268; act: -68 ),
  ( sym: 269; act: -68 ),
  ( sym: 270; act: -68 ),
  ( sym: 271; act: -68 ),
  ( sym: 272; act: -68 ),
  ( sym: 273; act: -68 ),
  ( sym: 277; act: -68 ),
  ( sym: 287; act: -68 ),
  ( sym: 288; act: -68 ),
  ( sym: 289; act: -68 ),
  ( sym: 290; act: -68 ),
  ( sym: 291; act: -68 ),
  ( sym: 294; act: -68 ),
  ( sym: 295; act: -68 ),
  ( sym: 296; act: -68 ),
  ( sym: 297; act: -68 ),
  ( sym: 298; act: -68 ),
  ( sym: 299; act: -68 ),
  ( sym: 300; act: -68 ),
  ( sym: 301; act: -68 ),
  ( sym: 304; act: -68 ),
  ( sym: 306; act: -68 ),
  ( sym: 307; act: -68 ),
  ( sym: 308; act: -68 ),
  ( sym: 309; act: -68 ),
  ( sym: 310; act: -68 ),
  ( sym: 311; act: -68 ),
  ( sym: 312; act: -68 ),
  ( sym: 313; act: -68 ),
  ( sym: 314; act: -68 ),
  ( sym: 315; act: -68 ),
  ( sym: 316; act: -68 ),
  ( sym: 317; act: -68 ),
  ( sym: 318; act: -68 ),
  ( sym: 319; act: -68 ),
  ( sym: 320; act: -68 ),
  ( sym: 321; act: -68 ),
  ( sym: 324; act: -68 ),
  ( sym: 325; act: -68 ),
{ 27: }
{ 28: }
{ 29: }
{ 30: }
{ 31: }
  ( sym: 274; act: 20 ),
  ( sym: 275; act: 21 ),
  ( sym: 276; act: 22 ),
  ( sym: 277; act: 23 ),
  ( sym: 280; act: 24 ),
  ( sym: 281; act: 25 ),
  ( sym: 282; act: 26 ),
  ( sym: 283; act: 27 ),
  ( sym: 284; act: 28 ),
  ( sym: 285; act: 29 ),
  ( sym: 286; act: 30 ),
  ( sym: 287; act: 31 ),
  ( sym: 327; act: 32 ),
{ 32: }
  ( sym: 280; act: 24 ),
  ( sym: 281; act: 25 ),
  ( sym: 282; act: 26 ),
  ( sym: 283; act: 27 ),
  ( sym: 284; act: 28 ),
  ( sym: 285; act: 29 ),
  ( sym: 286; act: 30 ),
  ( sym: 327; act: 32 ),
{ 33: }
{ 34: }
{ 35: }
  ( sym: 266; act: 64 ),
  ( sym: 291; act: 65 ),
{ 36: }
  ( sym: 268; act: 67 ),
  ( sym: 294; act: 44 ),
  ( sym: 295; act: 45 ),
  ( sym: 296; act: 46 ),
  ( sym: 297; act: 47 ),
  ( sym: 298; act: 48 ),
  ( sym: 299; act: 49 ),
  ( sym: 300; act: 50 ),
  ( sym: 256; act: -18 ),
  ( sym: 277; act: -18 ),
  ( sym: 287; act: -18 ),
  ( sym: 288; act: -18 ),
  ( sym: 289; act: -18 ),
  ( sym: 290; act: -18 ),
  ( sym: 314; act: -18 ),
  ( sym: 319; act: -18 ),
{ 37: }
  ( sym: 266; act: 68 ),
  ( sym: 256; act: -79 ),
  ( sym: 268; act: -79 ),
  ( sym: 277; act: -79 ),
  ( sym: 287; act: -79 ),
  ( sym: 288; act: -79 ),
  ( sym: 289; act: -79 ),
  ( sym: 290; act: -79 ),
  ( sym: 294; act: -79 ),
  ( sym: 295; act: -79 ),
  ( sym: 296; act: -79 ),
  ( sym: 297; act: -79 ),
  ( sym: 298; act: -79 ),
  ( sym: 299; act: -79 ),
  ( sym: 300; act: -79 ),
  ( sym: 314; act: -79 ),
  ( sym: 319; act: -79 ),
{ 38: }
  ( sym: 256; act: 52 ),
  ( sym: 272; act: 53 ),
  ( sym: 277; act: 23 ),
{ 39: }
  ( sym: 268; act: 70 ),
  ( sym: 291; act: 71 ),
  ( sym: 292; act: 72 ),
{ 40: }
  ( sym: 256; act: 52 ),
  ( sym: 272; act: 53 ),
  ( sym: 266; act: -50 ),
  ( sym: 267; act: -50 ),
  ( sym: 268; act: -50 ),
  ( sym: 269; act: -50 ),
  ( sym: 270; act: -50 ),
  ( sym: 277; act: -50 ),
  ( sym: 287; act: -50 ),
  ( sym: 288; act: -50 ),
  ( sym: 289; act: -50 ),
  ( sym: 290; act: -50 ),
  ( sym: 294; act: -50 ),
  ( sym: 295; act: -50 ),
  ( sym: 296; act: -50 ),
  ( sym: 297; act: -50 ),
  ( sym: 298; act: -50 ),
  ( sym: 299; act: -50 ),
  ( sym: 300; act: -50 ),
  ( sym: 314; act: -50 ),
  ( sym: 319; act: -50 ),
{ 41: }
  ( sym: 256; act: 52 ),
  ( sym: 272; act: 53 ),
  ( sym: 266; act: -49 ),
  ( sym: 267; act: -49 ),
  ( sym: 268; act: -49 ),
  ( sym: 269; act: -49 ),
  ( sym: 270; act: -49 ),
  ( sym: 277; act: -49 ),
  ( sym: 287; act: -49 ),
  ( sym: 288; act: -49 ),
  ( sym: 289; act: -49 ),
  ( sym: 290; act: -49 ),
  ( sym: 294; act: -49 ),
  ( sym: 295; act: -49 ),
  ( sym: 296; act: -49 ),
  ( sym: 297; act: -49 ),
  ( sym: 298; act: -49 ),
  ( sym: 299; act: -49 ),
  ( sym: 300; act: -49 ),
  ( sym: 314; act: -49 ),
  ( sym: 319; act: -49 ),
{ 42: }
  ( sym: 256; act: 56 ),
  ( sym: 272; act: 57 ),
  ( sym: 266; act: -52 ),
  ( sym: 267; act: -52 ),
  ( sym: 268; act: -52 ),
  ( sym: 269; act: -52 ),
  ( sym: 270; act: -52 ),
  ( sym: 277; act: -52 ),
  ( sym: 287; act: -52 ),
  ( sym: 288; act: -52 ),
  ( sym: 289; act: -52 ),
  ( sym: 290; act: -52 ),
  ( sym: 294; act: -52 ),
  ( sym: 295; act: -52 ),
  ( sym: 296; act: -52 ),
  ( sym: 297; act: -52 ),
  ( sym: 298; act: -52 ),
  ( sym: 299; act: -52 ),
  ( sym: 300; act: -52 ),
  ( sym: 314; act: -52 ),
  ( sym: 319; act: -52 ),
{ 43: }
  ( sym: 256; act: 80 ),
  ( sym: 268; act: 81 ),
  ( sym: 277; act: 23 ),
  ( sym: 287; act: 82 ),
  ( sym: 288; act: 83 ),
  ( sym: 289; act: 84 ),
  ( sym: 290; act: 85 ),
  ( sym: 314; act: 86 ),
  ( sym: 319; act: 87 ),
{ 44: }
{ 45: }
{ 46: }
{ 47: }
{ 48: }
{ 49: }
{ 50: }
{ 51: }
  ( sym: 302; act: 88 ),
  ( sym: 256; act: -57 ),
  ( sym: 267; act: -57 ),
  ( sym: 268; act: -57 ),
  ( sym: 269; act: -57 ),
  ( sym: 270; act: -57 ),
  ( sym: 277; act: -57 ),
  ( sym: 287; act: -57 ),
  ( sym: 288; act: -57 ),
  ( sym: 289; act: -57 ),
  ( sym: 290; act: -57 ),
  ( sym: 294; act: -57 ),
  ( sym: 295; act: -57 ),
  ( sym: 296; act: -57 ),
  ( sym: 297; act: -57 ),
  ( sym: 298; act: -57 ),
  ( sym: 299; act: -57 ),
  ( sym: 300; act: -57 ),
  ( sym: 314; act: -57 ),
  ( sym: 319; act: -57 ),
{ 52: }
{ 53: }
  ( sym: 274; act: 20 ),
  ( sym: 275; act: 21 ),
  ( sym: 276; act: 22 ),
  ( sym: 277; act: 23 ),
  ( sym: 280; act: 24 ),
  ( sym: 281; act: 25 ),
  ( sym: 282; act: 26 ),
  ( sym: 283; act: 27 ),
  ( sym: 284; act: 28 ),
  ( sym: 285; act: 29 ),
  ( sym: 286; act: 30 ),
  ( sym: 287; act: 31 ),
  ( sym: 327; act: 32 ),
{ 54: }
  ( sym: 302; act: 93 ),
  ( sym: 256; act: -55 ),
  ( sym: 267; act: -55 ),
  ( sym: 268; act: -55 ),
  ( sym: 269; act: -55 ),
  ( sym: 270; act: -55 ),
  ( sym: 277; act: -55 ),
  ( sym: 287; act: -55 ),
  ( sym: 288; act: -55 ),
  ( sym: 289; act: -55 ),
  ( sym: 290; act: -55 ),
  ( sym: 294; act: -55 ),
  ( sym: 295; act: -55 ),
  ( sym: 296; act: -55 ),
  ( sym: 297; act: -55 ),
  ( sym: 298; act: -55 ),
  ( sym: 299; act: -55 ),
  ( sym: 300; act: -55 ),
  ( sym: 314; act: -55 ),
  ( sym: 319; act: -55 ),
{ 55: }
{ 56: }
{ 57: }
  ( sym: 277; act: 23 ),
  ( sym: 273; act: -158 ),
{ 58: }
{ 59: }
{ 60: }
  ( sym: 283; act: 98 ),
  ( sym: 256; act: -70 ),
  ( sym: 265; act: -70 ),
  ( sym: 266; act: -70 ),
  ( sym: 267; act: -70 ),
  ( sym: 268; act: -70 ),
  ( sym: 269; act: -70 ),
  ( sym: 270; act: -70 ),
  ( sym: 271; act: -70 ),
  ( sym: 272; act: -70 ),
  ( sym: 273; act: -70 ),
  ( sym: 277; act: -70 ),
  ( sym: 287; act: -70 ),
  ( sym: 288; act: -70 ),
  ( sym: 289; act: -70 ),
  ( sym: 290; act: -70 ),
  ( sym: 291; act: -70 ),
  ( sym: 294; act: -70 ),
  ( sym: 295; act: -70 ),
  ( sym: 296; act: -70 ),
  ( sym: 297; act: -70 ),
  ( sym: 298; act: -70 ),
  ( sym: 299; act: -70 ),
  ( sym: 300; act: -70 ),
  ( sym: 301; act: -70 ),
  ( sym: 304; act: -70 ),
  ( sym: 306; act: -70 ),
  ( sym: 307; act: -70 ),
  ( sym: 308; act: -70 ),
  ( sym: 309; act: -70 ),
  ( sym: 310; act: -70 ),
  ( sym: 311; act: -70 ),
  ( sym: 312; act: -70 ),
  ( sym: 313; act: -70 ),
  ( sym: 314; act: -70 ),
  ( sym: 315; act: -70 ),
  ( sym: 316; act: -70 ),
  ( sym: 317; act: -70 ),
  ( sym: 318; act: -70 ),
  ( sym: 319; act: -70 ),
  ( sym: 320; act: -70 ),
  ( sym: 321; act: -70 ),
  ( sym: 324; act: -70 ),
  ( sym: 325; act: -70 ),
{ 61: }
{ 62: }
{ 63: }
{ 64: }
{ 65: }
{ 66: }
  ( sym: 256; act: 80 ),
  ( sym: 268; act: 81 ),
  ( sym: 277; act: 23 ),
  ( sym: 287; act: 82 ),
  ( sym: 288; act: 83 ),
  ( sym: 289; act: 84 ),
  ( sym: 290; act: 85 ),
  ( sym: 314; act: 86 ),
  ( sym: 319; act: 87 ),
{ 67: }
  ( sym: 294; act: 44 ),
  ( sym: 295; act: 45 ),
  ( sym: 296; act: 46 ),
  ( sym: 297; act: 47 ),
  ( sym: 298; act: 48 ),
  ( sym: 299; act: 49 ),
  ( sym: 300; act: 50 ),
  ( sym: 268; act: -18 ),
  ( sym: 277; act: -18 ),
  ( sym: 287; act: -18 ),
  ( sym: 288; act: -18 ),
  ( sym: 289; act: -18 ),
  ( sym: 290; act: -18 ),
  ( sym: 314; act: -18 ),
  ( sym: 319; act: -18 ),
{ 68: }
{ 69: }
  ( sym: 256; act: 52 ),
  ( sym: 272; act: 53 ),
  ( sym: 277; act: 23 ),
  ( sym: 268; act: -50 ),
  ( sym: 287; act: -50 ),
  ( sym: 288; act: -50 ),
  ( sym: 289; act: -50 ),
  ( sym: 290; act: -50 ),
  ( sym: 294; act: -50 ),
  ( sym: 295; act: -50 ),
  ( sym: 296; act: -50 ),
  ( sym: 297; act: -50 ),
  ( sym: 298; act: -50 ),
  ( sym: 299; act: -50 ),
  ( sym: 300; act: -50 ),
  ( sym: 314; act: -50 ),
  ( sym: 319; act: -50 ),
{ 70: }
  ( sym: 277; act: 23 ),
  ( sym: 269; act: -158 ),
{ 71: }
{ 72: }
  ( sym: 268; act: 107 ),
  ( sym: 277; act: 23 ),
  ( sym: 278; act: 108 ),
  ( sym: 279; act: 109 ),
  ( sym: 280; act: 24 ),
  ( sym: 281; act: 25 ),
  ( sym: 282; act: 26 ),
  ( sym: 283; act: 27 ),
  ( sym: 284; act: 28 ),
  ( sym: 285; act: 29 ),
  ( sym: 286; act: 30 ),
  ( sym: 291; act: 110 ),
  ( sym: 314; act: 111 ),
  ( sym: 316; act: 112 ),
  ( sym: 321; act: 113 ),
  ( sym: 327; act: 32 ),
{ 73: }
  ( sym: 302; act: 114 ),
  ( sym: 256; act: -46 ),
  ( sym: 266; act: -46 ),
  ( sym: 267; act: -46 ),
  ( sym: 268; act: -46 ),
  ( sym: 269; act: -46 ),
  ( sym: 270; act: -46 ),
  ( sym: 277; act: -46 ),
  ( sym: 287; act: -46 ),
  ( sym: 288; act: -46 ),
  ( sym: 289; act: -46 ),
  ( sym: 290; act: -46 ),
  ( sym: 294; act: -46 ),
  ( sym: 295; act: -46 ),
  ( sym: 296; act: -46 ),
  ( sym: 297; act: -46 ),
  ( sym: 298; act: -46 ),
  ( sym: 299; act: -46 ),
  ( sym: 300; act: -46 ),
  ( sym: 314; act: -46 ),
  ( sym: 319; act: -46 ),
{ 74: }
  ( sym: 302; act: 115 ),
  ( sym: 256; act: -48 ),
  ( sym: 266; act: -48 ),
  ( sym: 267; act: -48 ),
  ( sym: 268; act: -48 ),
  ( sym: 269; act: -48 ),
  ( sym: 270; act: -48 ),
  ( sym: 277; act: -48 ),
  ( sym: 287; act: -48 ),
  ( sym: 288; act: -48 ),
  ( sym: 289; act: -48 ),
  ( sym: 290; act: -48 ),
  ( sym: 294; act: -48 ),
  ( sym: 295; act: -48 ),
  ( sym: 296; act: -48 ),
  ( sym: 297; act: -48 ),
  ( sym: 298; act: -48 ),
  ( sym: 299; act: -48 ),
  ( sym: 300; act: -48 ),
  ( sym: 314; act: -48 ),
  ( sym: 319; act: -48 ),
{ 75: }
{ 76: }
  ( sym: 319; act: 116 ),
{ 77: }
  ( sym: 268; act: 118 ),
  ( sym: 270; act: 119 ),
  ( sym: 266; act: -83 ),
  ( sym: 267; act: -83 ),
  ( sym: 272; act: -83 ),
  ( sym: 301; act: -83 ),
{ 78: }
  ( sym: 267; act: 122 ),
  ( sym: 272; act: 123 ),
  ( sym: 301; act: 124 ),
  ( sym: 266; act: -20 ),
{ 79: }
  ( sym: 265; act: 126 ),
  ( sym: 266; act: -100 ),
  ( sym: 267; act: -100 ),
  ( sym: 268; act: -100 ),
  ( sym: 269; act: -100 ),
  ( sym: 270; act: -100 ),
  ( sym: 272; act: -100 ),
  ( sym: 301; act: -100 ),
{ 80: }
{ 81: }
  ( sym: 268; act: 81 ),
  ( sym: 277; act: 23 ),
  ( sym: 287; act: 82 ),
  ( sym: 288; act: 83 ),
  ( sym: 289; act: 84 ),
  ( sym: 290; act: 85 ),
  ( sym: 314; act: 86 ),
  ( sym: 319; act: 87 ),
{ 82: }
  ( sym: 268; act: 81 ),
  ( sym: 277; act: 23 ),
  ( sym: 287; act: 82 ),
  ( sym: 288; act: 83 ),
  ( sym: 289; act: 84 ),
  ( sym: 290; act: 85 ),
  ( sym: 314; act: 86 ),
  ( sym: 319; act: 87 ),
{ 83: }
{ 84: }
{ 85: }
{ 86: }
  ( sym: 268; act: 81 ),
  ( sym: 277; act: 23 ),
  ( sym: 287; act: 82 ),
  ( sym: 288; act: 83 ),
  ( sym: 289; act: 84 ),
  ( sym: 290; act: 85 ),
  ( sym: 314; act: 86 ),
  ( sym: 319; act: 87 ),
{ 87: }
  ( sym: 268; act: 81 ),
  ( sym: 277; act: 23 ),
  ( sym: 287; act: 82 ),
  ( sym: 288; act: 83 ),
  ( sym: 289; act: 84 ),
  ( sym: 290; act: 85 ),
  ( sym: 314; act: 86 ),
  ( sym: 319; act: 87 ),
{ 88: }
{ 89: }
  ( sym: 273; act: 132 ),
{ 90: }
  ( sym: 274; act: 20 ),
  ( sym: 275; act: 21 ),
  ( sym: 276; act: 22 ),
  ( sym: 277; act: 23 ),
  ( sym: 280; act: 24 ),
  ( sym: 281; act: 25 ),
  ( sym: 282; act: 26 ),
  ( sym: 283; act: 27 ),
  ( sym: 284; act: 28 ),
  ( sym: 285; act: 29 ),
  ( sym: 286; act: 30 ),
  ( sym: 287; act: 31 ),
  ( sym: 327; act: 32 ),
  ( sym: 273; act: -62 ),
{ 91: }
  ( sym: 273; act: 134 ),
{ 92: }
  ( sym: 256; act: 80 ),
  ( sym: 268; act: 81 ),
  ( sym: 277; act: 23 ),
  ( sym: 287; act: 82 ),
  ( sym: 288; act: 83 ),
  ( sym: 289; act: 84 ),
  ( sym: 290; act: 85 ),
  ( sym: 314; act: 86 ),
  ( sym: 319; act: 87 ),
{ 93: }
{ 94: }
  ( sym: 273; act: 136 ),
{ 95: }
  ( sym: 267; act: 137 ),
  ( sym: 269; act: -157 ),
  ( sym: 273; act: -157 ),
{ 96: }
  ( sym: 273; act: 138 ),
{ 97: }
  ( sym: 304; act: 139 ),
  ( sym: 267; act: -160 ),
  ( sym: 269; act: -160 ),
  ( sym: 273; act: -160 ),
{ 98: }
{ 99: }
  ( sym: 266; act: 140 ),
  ( sym: 267; act: 122 ),
{ 100: }
  ( sym: 268; act: 81 ),
  ( sym: 277; act: 23 ),
  ( sym: 287; act: 82 ),
  ( sym: 288; act: 83 ),
  ( sym: 289; act: 84 ),
  ( sym: 290; act: 85 ),
  ( sym: 314; act: 86 ),
  ( sym: 319; act: 87 ),
{ 101: }
  ( sym: 266; act: 142 ),
{ 102: }
  ( sym: 269; act: 143 ),
{ 103: }
  ( sym: 324; act: 144 ),
  ( sym: 325; act: 145 ),
  ( sym: 269; act: -161 ),
  ( sym: 291; act: -161 ),
{ 104: }
{ 105: }
  ( sym: 291; act: 146 ),
{ 106: }
  ( sym: 268; act: 147 ),
  ( sym: 270; act: 148 ),
  ( sym: 265; act: -139 ),
  ( sym: 266; act: -139 ),
  ( sym: 267; act: -139 ),
  ( sym: 269; act: -139 ),
  ( sym: 271; act: -139 ),
  ( sym: 272; act: -139 ),
  ( sym: 273; act: -139 ),
  ( sym: 291; act: -139 ),
  ( sym: 301; act: -139 ),
  ( sym: 304; act: -139 ),
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
  ( sym: 316; act: -139 ),
  ( sym: 317; act: -139 ),
  ( sym: 318; act: -139 ),
  ( sym: 319; act: -139 ),
  ( sym: 320; act: -139 ),
  ( sym: 321; act: -139 ),
  ( sym: 324; act: -139 ),
  ( sym: 325; act: -139 ),
{ 107: }
  ( sym: 268; act: 107 ),
  ( sym: 274; act: 20 ),
  ( sym: 275; act: 21 ),
  ( sym: 276; act: 22 ),
  ( sym: 277; act: 23 ),
  ( sym: 278; act: 108 ),
  ( sym: 279; act: 109 ),
  ( sym: 280; act: 24 ),
  ( sym: 281; act: 25 ),
  ( sym: 282; act: 26 ),
  ( sym: 283; act: 27 ),
  ( sym: 284; act: 28 ),
  ( sym: 285; act: 29 ),
  ( sym: 286; act: 30 ),
  ( sym: 287; act: 31 ),
  ( sym: 314; act: 111 ),
  ( sym: 316; act: 112 ),
  ( sym: 319; act: 155 ),
  ( sym: 321; act: 113 ),
  ( sym: 327; act: 32 ),
{ 108: }
{ 109: }
{ 110: }
{ 111: }
  ( sym: 268; act: 107 ),
  ( sym: 277; act: 23 ),
  ( sym: 278; act: 108 ),
  ( sym: 279; act: 109 ),
  ( sym: 280; act: 24 ),
  ( sym: 281; act: 25 ),
  ( sym: 282; act: 26 ),
  ( sym: 283; act: 27 ),
  ( sym: 284; act: 28 ),
  ( sym: 285; act: 29 ),
  ( sym: 286; act: 30 ),
  ( sym: 314; act: 111 ),
  ( sym: 316; act: 112 ),
  ( sym: 321; act: 113 ),
  ( sym: 327; act: 32 ),
{ 112: }
  ( sym: 268; act: 107 ),
  ( sym: 277; act: 23 ),
  ( sym: 278; act: 108 ),
  ( sym: 279; act: 109 ),
  ( sym: 280; act: 24 ),
  ( sym: 281; act: 25 ),
  ( sym: 282; act: 26 ),
  ( sym: 283; act: 27 ),
  ( sym: 284; act: 28 ),
  ( sym: 285; act: 29 ),
  ( sym: 286; act: 30 ),
  ( sym: 314; act: 111 ),
  ( sym: 316; act: 112 ),
  ( sym: 321; act: 113 ),
  ( sym: 327; act: 32 ),
{ 113: }
  ( sym: 268; act: 107 ),
  ( sym: 277; act: 23 ),
  ( sym: 278; act: 108 ),
  ( sym: 279; act: 109 ),
  ( sym: 280; act: 24 ),
  ( sym: 281; act: 25 ),
  ( sym: 282; act: 26 ),
  ( sym: 283; act: 27 ),
  ( sym: 284; act: 28 ),
  ( sym: 285; act: 29 ),
  ( sym: 286; act: 30 ),
  ( sym: 314; act: 111 ),
  ( sym: 316; act: 112 ),
  ( sym: 321; act: 113 ),
  ( sym: 327; act: 32 ),
{ 114: }
{ 115: }
{ 116: }
  ( sym: 268; act: 81 ),
  ( sym: 277; act: 23 ),
  ( sym: 287; act: 82 ),
  ( sym: 288; act: 83 ),
  ( sym: 289; act: 84 ),
  ( sym: 290; act: 85 ),
  ( sym: 314; act: 86 ),
  ( sym: 319; act: 87 ),
{ 117: }
{ 118: }
  ( sym: 269; act: 163 ),
  ( sym: 274; act: 20 ),
  ( sym: 275; act: 21 ),
  ( sym: 276; act: 22 ),
  ( sym: 277; act: 23 ),
  ( sym: 280; act: 24 ),
  ( sym: 281; act: 25 ),
  ( sym: 282; act: 26 ),
  ( sym: 283; act: 27 ),
  ( sym: 284; act: 28 ),
  ( sym: 285; act: 29 ),
  ( sym: 286; act: 164 ),
  ( sym: 287; act: 31 ),
  ( sym: 303; act: 165 ),
  ( sym: 327; act: 32 ),
{ 119: }
  ( sym: 268; act: 107 ),
  ( sym: 271; act: 168 ),
  ( sym: 277; act: 23 ),
  ( sym: 278; act: 108 ),
  ( sym: 279; act: 109 ),
  ( sym: 280; act: 24 ),
  ( sym: 281; act: 25 ),
  ( sym: 282; act: 26 ),
  ( sym: 283; act: 27 ),
  ( sym: 284; act: 28 ),
  ( sym: 285; act: 29 ),
  ( sym: 286; act: 30 ),
  ( sym: 314; act: 111 ),
  ( sym: 316; act: 112 ),
  ( sym: 321; act: 113 ),
  ( sym: 327; act: 32 ),
{ 120: }
{ 121: }
  ( sym: 266; act: 169 ),
{ 122: }
  ( sym: 268; act: 81 ),
  ( sym: 277; act: 23 ),
  ( sym: 287; act: 82 ),
  ( sym: 288; act: 83 ),
  ( sym: 289; act: 84 ),
  ( sym: 290; act: 85 ),
  ( sym: 314; act: 86 ),
  ( sym: 319; act: 87 ),
{ 123: }
  ( sym: 257; act: 174 ),
  ( sym: 266; act: 175 ),
  ( sym: 268; act: 107 ),
  ( sym: 277; act: 23 ),
  ( sym: 278; act: 108 ),
  ( sym: 279; act: 109 ),
  ( sym: 280; act: 24 ),
  ( sym: 281; act: 25 ),
  ( sym: 282; act: 26 ),
  ( sym: 283; act: 27 ),
  ( sym: 284; act: 28 ),
  ( sym: 285; act: 29 ),
  ( sym: 286; act: 30 ),
  ( sym: 314; act: 111 ),
  ( sym: 316; act: 112 ),
  ( sym: 321; act: 113 ),
  ( sym: 327; act: 32 ),
  ( sym: 273; act: -26 ),
{ 124: }
  ( sym: 268; act: 176 ),
{ 125: }
  ( sym: 268; act: 107 ),
  ( sym: 277; act: 23 ),
  ( sym: 278; act: 108 ),
  ( sym: 279; act: 109 ),
  ( sym: 280; act: 24 ),
  ( sym: 281; act: 25 ),
  ( sym: 282; act: 26 ),
  ( sym: 283; act: 27 ),
  ( sym: 284; act: 28 ),
  ( sym: 285; act: 29 ),
  ( sym: 286; act: 30 ),
  ( sym: 314; act: 111 ),
  ( sym: 316; act: 112 ),
  ( sym: 321; act: 113 ),
  ( sym: 327; act: 32 ),
{ 126: }
  ( sym: 268; act: 107 ),
  ( sym: 277; act: 23 ),
  ( sym: 278; act: 108 ),
  ( sym: 279; act: 109 ),
  ( sym: 280; act: 24 ),
  ( sym: 281; act: 25 ),
  ( sym: 282; act: 26 ),
  ( sym: 283; act: 27 ),
  ( sym: 284; act: 28 ),
  ( sym: 285; act: 29 ),
  ( sym: 286; act: 30 ),
  ( sym: 314; act: 111 ),
  ( sym: 316; act: 112 ),
  ( sym: 321; act: 113 ),
  ( sym: 327; act: 32 ),
{ 127: }
  ( sym: 267; act: 179 ),
  ( sym: 266; act: -82 ),
  ( sym: 272; act: -82 ),
  ( sym: 301; act: -82 ),
{ 128: }
  ( sym: 268; act: 118 ),
  ( sym: 269; act: 180 ),
  ( sym: 270; act: 119 ),
{ 129: }
  ( sym: 268; act: 118 ),
  ( sym: 270; act: 119 ),
  ( sym: 266; act: -94 ),
  ( sym: 267; act: -94 ),
  ( sym: 269; act: -94 ),
  ( sym: 272; act: -94 ),
  ( sym: 301; act: -94 ),
{ 130: }
  ( sym: 270; act: 119 ),
  ( sym: 266; act: -97 ),
  ( sym: 267; act: -97 ),
  ( sym: 268; act: -97 ),
  ( sym: 269; act: -97 ),
  ( sym: 272; act: -97 ),
  ( sym: 301; act: -97 ),
{ 131: }
  ( sym: 268; act: 118 ),
  ( sym: 270; act: 119 ),
  ( sym: 266; act: -96 ),
  ( sym: 267; act: -96 ),
  ( sym: 269; act: -96 ),
  ( sym: 272; act: -96 ),
  ( sym: 301; act: -96 ),
{ 132: }
{ 133: }
{ 134: }
{ 135: }
  ( sym: 266; act: 181 ),
  ( sym: 267; act: 122 ),
{ 136: }
{ 137: }
  ( sym: 277; act: 23 ),
  ( sym: 269; act: -158 ),
  ( sym: 273; act: -158 ),
{ 138: }
{ 139: }
  ( sym: 268; act: 107 ),
  ( sym: 277; act: 23 ),
  ( sym: 278; act: 108 ),
  ( sym: 279; act: 109 ),
  ( sym: 280; act: 24 ),
  ( sym: 281; act: 25 ),
  ( sym: 282; act: 26 ),
  ( sym: 283; act: 27 ),
  ( sym: 284; act: 28 ),
  ( sym: 285; act: 29 ),
  ( sym: 286; act: 30 ),
  ( sym: 314; act: 111 ),
  ( sym: 316; act: 112 ),
  ( sym: 321; act: 113 ),
  ( sym: 327; act: 32 ),
{ 140: }
{ 141: }
  ( sym: 268; act: 118 ),
  ( sym: 269; act: 184 ),
  ( sym: 270; act: 119 ),
{ 142: }
{ 143: }
  ( sym: 292; act: 187 ),
  ( sym: 268; act: -3 ),
{ 144: }
  ( sym: 268; act: 107 ),
  ( sym: 277; act: 23 ),
  ( sym: 278; act: 108 ),
  ( sym: 279; act: 109 ),
  ( sym: 280; act: 24 ),
  ( sym: 281; act: 25 ),
  ( sym: 282; act: 26 ),
  ( sym: 283; act: 27 ),
  ( sym: 284; act: 28 ),
  ( sym: 285; act: 29 ),
  ( sym: 286; act: 30 ),
  ( sym: 314; act: 111 ),
  ( sym: 316; act: 112 ),
  ( sym: 321; act: 113 ),
  ( sym: 327; act: 32 ),
{ 145: }
  ( sym: 268; act: 107 ),
  ( sym: 277; act: 23 ),
  ( sym: 278; act: 108 ),
  ( sym: 279; act: 109 ),
  ( sym: 280; act: 24 ),
  ( sym: 281; act: 25 ),
  ( sym: 282; act: 26 ),
  ( sym: 283; act: 27 ),
  ( sym: 284; act: 28 ),
  ( sym: 285; act: 29 ),
  ( sym: 286; act: 30 ),
  ( sym: 314; act: 111 ),
  ( sym: 316; act: 112 ),
  ( sym: 321; act: 113 ),
  ( sym: 327; act: 32 ),
{ 146: }
{ 147: }
  ( sym: 268; act: 107 ),
  ( sym: 277; act: 23 ),
  ( sym: 278; act: 108 ),
  ( sym: 279; act: 109 ),
  ( sym: 280; act: 24 ),
  ( sym: 281; act: 25 ),
  ( sym: 282; act: 26 ),
  ( sym: 283; act: 27 ),
  ( sym: 284; act: 28 ),
  ( sym: 285; act: 29 ),
  ( sym: 286; act: 30 ),
  ( sym: 314; act: 111 ),
  ( sym: 316; act: 112 ),
  ( sym: 321; act: 113 ),
  ( sym: 327; act: 32 ),
  ( sym: 269; act: -166 ),
{ 148: }
  ( sym: 268; act: 107 ),
  ( sym: 277; act: 23 ),
  ( sym: 278; act: 108 ),
  ( sym: 279; act: 109 ),
  ( sym: 280; act: 24 ),
  ( sym: 281; act: 25 ),
  ( sym: 282; act: 26 ),
  ( sym: 283; act: 27 ),
  ( sym: 284; act: 28 ),
  ( sym: 285; act: 29 ),
  ( sym: 286; act: 30 ),
  ( sym: 314; act: 111 ),
  ( sym: 316; act: 112 ),
  ( sym: 321; act: 113 ),
  ( sym: 327; act: 32 ),
  ( sym: 271; act: -166 ),
{ 149: }
  ( sym: 324; act: 144 ),
  ( sym: 325; act: 145 ),
  ( sym: 265; act: -135 ),
  ( sym: 266; act: -135 ),
  ( sym: 267; act: -135 ),
  ( sym: 268; act: -135 ),
  ( sym: 269; act: -135 ),
  ( sym: 270; act: -135 ),
  ( sym: 271; act: -135 ),
  ( sym: 272; act: -135 ),
  ( sym: 273; act: -135 ),
  ( sym: 291; act: -135 ),
  ( sym: 301; act: -135 ),
  ( sym: 304; act: -135 ),
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
  ( sym: 316; act: -135 ),
  ( sym: 317; act: -135 ),
  ( sym: 318; act: -135 ),
  ( sym: 319; act: -135 ),
  ( sym: 320; act: -135 ),
  ( sym: 321; act: -135 ),
{ 150: }
  ( sym: 269; act: 194 ),
  ( sym: 304; act: -117 ),
  ( sym: 306; act: -117 ),
  ( sym: 307; act: -117 ),
  ( sym: 308; act: -117 ),
  ( sym: 309; act: -117 ),
  ( sym: 310; act: -117 ),
  ( sym: 311; act: -117 ),
  ( sym: 312; act: -117 ),
  ( sym: 313; act: -117 ),
  ( sym: 314; act: -117 ),
  ( sym: 315; act: -117 ),
  ( sym: 316; act: -117 ),
  ( sym: 317; act: -117 ),
  ( sym: 318; act: -117 ),
  ( sym: 319; act: -117 ),
  ( sym: 320; act: -117 ),
  ( sym: 321; act: -117 ),
{ 151: }
  ( sym: 269; act: -78 ),
  ( sym: 288; act: -78 ),
  ( sym: 289; act: -78 ),
  ( sym: 290; act: -78 ),
  ( sym: 319; act: -78 ),
  ( sym: 304; act: -140 ),
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
  ( sym: 316; act: -140 ),
  ( sym: 317; act: -140 ),
  ( sym: 318; act: -140 ),
  ( sym: 320; act: -140 ),
  ( sym: 321; act: -140 ),
  ( sym: 324; act: -140 ),
  ( sym: 325; act: -140 ),
{ 152: }
  ( sym: 269; act: 196 ),
  ( sym: 288; act: 83 ),
  ( sym: 289; act: 84 ),
  ( sym: 290; act: 85 ),
  ( sym: 319; act: 197 ),
{ 153: }
  ( sym: 304; act: 198 ),
  ( sym: 306; act: 199 ),
  ( sym: 307; act: 200 ),
  ( sym: 308; act: 201 ),
  ( sym: 309; act: 202 ),
  ( sym: 310; act: 203 ),
  ( sym: 311; act: 204 ),
  ( sym: 312; act: 205 ),
  ( sym: 313; act: 206 ),
  ( sym: 314; act: 207 ),
  ( sym: 315; act: 208 ),
  ( sym: 316; act: 209 ),
  ( sym: 317; act: 210 ),
  ( sym: 318; act: 211 ),
  ( sym: 319; act: 212 ),
  ( sym: 320; act: 213 ),
  ( sym: 321; act: 214 ),
{ 154: }
  ( sym: 268; act: 147 ),
  ( sym: 269; act: 215 ),
  ( sym: 270; act: 148 ),
  ( sym: 288; act: -79 ),
  ( sym: 289; act: -79 ),
  ( sym: 290; act: -79 ),
  ( sym: 319; act: -79 ),
  ( sym: 304; act: -139 ),
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
  ( sym: 316; act: -139 ),
  ( sym: 317; act: -139 ),
  ( sym: 318; act: -139 ),
  ( sym: 320; act: -139 ),
  ( sym: 321; act: -139 ),
  ( sym: 324; act: -139 ),
  ( sym: 325; act: -139 ),
{ 155: }
  ( sym: 268; act: 107 ),
  ( sym: 277; act: 23 ),
  ( sym: 278; act: 108 ),
  ( sym: 279; act: 109 ),
  ( sym: 280; act: 24 ),
  ( sym: 281; act: 25 ),
  ( sym: 282; act: 26 ),
  ( sym: 283; act: 27 ),
  ( sym: 284; act: 28 ),
  ( sym: 285; act: 29 ),
  ( sym: 286; act: 30 ),
  ( sym: 314; act: 111 ),
  ( sym: 316; act: 112 ),
  ( sym: 321; act: 113 ),
  ( sym: 327; act: 32 ),
{ 156: }
  ( sym: 324; act: 144 ),
  ( sym: 325; act: 145 ),
  ( sym: 265; act: -146 ),
  ( sym: 266; act: -146 ),
  ( sym: 267; act: -146 ),
  ( sym: 268; act: -146 ),
  ( sym: 269; act: -146 ),
  ( sym: 270; act: -146 ),
  ( sym: 271; act: -146 ),
  ( sym: 272; act: -146 ),
  ( sym: 273; act: -146 ),
  ( sym: 291; act: -146 ),
  ( sym: 301; act: -146 ),
  ( sym: 304; act: -146 ),
  ( sym: 306; act: -146 ),
  ( sym: 307; act: -146 ),
  ( sym: 308; act: -146 ),
  ( sym: 309; act: -146 ),
  ( sym: 310; act: -146 ),
  ( sym: 311; act: -146 ),
  ( sym: 312; act: -146 ),
  ( sym: 313; act: -146 ),
  ( sym: 314; act: -146 ),
  ( sym: 315; act: -146 ),
  ( sym: 316; act: -146 ),
  ( sym: 317; act: -146 ),
  ( sym: 318; act: -146 ),
  ( sym: 319; act: -146 ),
  ( sym: 320; act: -146 ),
  ( sym: 321; act: -146 ),
{ 157: }
  ( sym: 324; act: 144 ),
  ( sym: 325; act: 145 ),
  ( sym: 265; act: -145 ),
  ( sym: 266; act: -145 ),
  ( sym: 267; act: -145 ),
  ( sym: 268; act: -145 ),
  ( sym: 269; act: -145 ),
  ( sym: 270; act: -145 ),
  ( sym: 271; act: -145 ),
  ( sym: 272; act: -145 ),
  ( sym: 273; act: -145 ),
  ( sym: 291; act: -145 ),
  ( sym: 301; act: -145 ),
  ( sym: 304; act: -145 ),
  ( sym: 306; act: -145 ),
  ( sym: 307; act: -145 ),
  ( sym: 308; act: -145 ),
  ( sym: 309; act: -145 ),
  ( sym: 310; act: -145 ),
  ( sym: 311; act: -145 ),
  ( sym: 312; act: -145 ),
  ( sym: 313; act: -145 ),
  ( sym: 314; act: -145 ),
  ( sym: 315; act: -145 ),
  ( sym: 316; act: -145 ),
  ( sym: 317; act: -145 ),
  ( sym: 318; act: -145 ),
  ( sym: 319; act: -145 ),
  ( sym: 320; act: -145 ),
  ( sym: 321; act: -145 ),
{ 158: }
  ( sym: 324; act: 144 ),
  ( sym: 325; act: 145 ),
  ( sym: 265; act: -147 ),
  ( sym: 266; act: -147 ),
  ( sym: 267; act: -147 ),
  ( sym: 268; act: -147 ),
  ( sym: 269; act: -147 ),
  ( sym: 270; act: -147 ),
  ( sym: 271; act: -147 ),
  ( sym: 272; act: -147 ),
  ( sym: 273; act: -147 ),
  ( sym: 291; act: -147 ),
  ( sym: 301; act: -147 ),
  ( sym: 304; act: -147 ),
  ( sym: 306; act: -147 ),
  ( sym: 307; act: -147 ),
  ( sym: 308; act: -147 ),
  ( sym: 309; act: -147 ),
  ( sym: 310; act: -147 ),
  ( sym: 311; act: -147 ),
  ( sym: 312; act: -147 ),
  ( sym: 313; act: -147 ),
  ( sym: 314; act: -147 ),
  ( sym: 315; act: -147 ),
  ( sym: 316; act: -147 ),
  ( sym: 317; act: -147 ),
  ( sym: 318; act: -147 ),
  ( sym: 319; act: -147 ),
  ( sym: 320; act: -147 ),
  ( sym: 321; act: -147 ),
{ 159: }
  ( sym: 268; act: 118 ),
  ( sym: 270; act: 119 ),
  ( sym: 266; act: -95 ),
  ( sym: 267; act: -95 ),
  ( sym: 269; act: -95 ),
  ( sym: 272; act: -95 ),
  ( sym: 301; act: -95 ),
{ 160: }
  ( sym: 267; act: 217 ),
  ( sym: 269; act: -87 ),
{ 161: }
  ( sym: 269; act: 218 ),
{ 162: }
  ( sym: 268; act: 222 ),
  ( sym: 277; act: 23 ),
  ( sym: 287; act: 223 ),
  ( sym: 288; act: 83 ),
  ( sym: 289; act: 84 ),
  ( sym: 290; act: 85 ),
  ( sym: 314; act: 86 ),
  ( sym: 319; act: 224 ),
  ( sym: 267; act: -116 ),
  ( sym: 269; act: -116 ),
  ( sym: 270; act: -116 ),
{ 163: }
{ 164: }
  ( sym: 269; act: 225 ),
  ( sym: 267; act: -75 ),
  ( sym: 268; act: -75 ),
  ( sym: 270; act: -75 ),
  ( sym: 277; act: -75 ),
  ( sym: 287; act: -75 ),
  ( sym: 288; act: -75 ),
  ( sym: 289; act: -75 ),
  ( sym: 290; act: -75 ),
  ( sym: 314; act: -75 ),
  ( sym: 319; act: -75 ),
{ 165: }
{ 166: }
{ 167: }
  ( sym: 271; act: 226 ),
  ( sym: 304; act: 198 ),
  ( sym: 306; act: 199 ),
  ( sym: 307; act: 200 ),
  ( sym: 308; act: 201 ),
  ( sym: 309; act: 202 ),
  ( sym: 310; act: 203 ),
  ( sym: 311; act: 204 ),
  ( sym: 312; act: 205 ),
  ( sym: 313; act: 206 ),
  ( sym: 314; act: 207 ),
  ( sym: 315; act: 208 ),
  ( sym: 316; act: 209 ),
  ( sym: 317; act: 210 ),
  ( sym: 318; act: 211 ),
  ( sym: 319; act: 212 ),
  ( sym: 320; act: 213 ),
  ( sym: 321; act: 214 ),
{ 168: }
{ 169: }
{ 170: }
  ( sym: 268; act: 118 ),
  ( sym: 270; act: 119 ),
  ( sym: 266; act: -80 ),
  ( sym: 267; act: -80 ),
  ( sym: 272; act: -80 ),
  ( sym: 301; act: -80 ),
{ 171: }
  ( sym: 273; act: 227 ),
{ 172: }
  ( sym: 266; act: 228 ),
  ( sym: 304; act: 198 ),
  ( sym: 306; act: 199 ),
  ( sym: 307; act: 200 ),
  ( sym: 308; act: 201 ),
  ( sym: 309; act: 202 ),
  ( sym: 310; act: 203 ),
  ( sym: 311; act: 204 ),
  ( sym: 312; act: 205 ),
  ( sym: 313; act: 206 ),
  ( sym: 314; act: 207 ),
  ( sym: 315; act: 208 ),
  ( sym: 316; act: 209 ),
  ( sym: 317; act: 210 ),
  ( sym: 318; act: 211 ),
  ( sym: 319; act: 212 ),
  ( sym: 320; act: 213 ),
  ( sym: 321; act: 214 ),
{ 173: }
  ( sym: 257; act: 174 ),
  ( sym: 266; act: 175 ),
  ( sym: 268; act: 107 ),
  ( sym: 277; act: 23 ),
  ( sym: 278; act: 108 ),
  ( sym: 279; act: 109 ),
  ( sym: 280; act: 24 ),
  ( sym: 281; act: 25 ),
  ( sym: 282; act: 26 ),
  ( sym: 283; act: 27 ),
  ( sym: 284; act: 28 ),
  ( sym: 285; act: 29 ),
  ( sym: 286; act: 30 ),
  ( sym: 314; act: 111 ),
  ( sym: 316; act: 112 ),
  ( sym: 321; act: 113 ),
  ( sym: 327; act: 32 ),
  ( sym: 273; act: -24 ),
{ 174: }
  ( sym: 268; act: 230 ),
{ 175: }
{ 176: }
  ( sym: 277; act: 23 ),
{ 177: }
  ( sym: 304; act: 198 ),
  ( sym: 306; act: 199 ),
  ( sym: 307; act: 200 ),
  ( sym: 308; act: 201 ),
  ( sym: 309; act: 202 ),
  ( sym: 310; act: 203 ),
  ( sym: 311; act: 204 ),
  ( sym: 312; act: 205 ),
  ( sym: 313; act: 206 ),
  ( sym: 314; act: 207 ),
  ( sym: 315; act: 208 ),
  ( sym: 316; act: 209 ),
  ( sym: 317; act: 210 ),
  ( sym: 318; act: 211 ),
  ( sym: 319; act: 212 ),
  ( sym: 320; act: 213 ),
  ( sym: 321; act: 214 ),
  ( sym: 266; act: -99 ),
  ( sym: 267; act: -99 ),
  ( sym: 268; act: -99 ),
  ( sym: 269; act: -99 ),
  ( sym: 270; act: -99 ),
  ( sym: 272; act: -99 ),
  ( sym: 301; act: -99 ),
{ 178: }
  ( sym: 304; act: 198 ),
  ( sym: 306; act: 199 ),
  ( sym: 307; act: 200 ),
  ( sym: 308; act: 201 ),
  ( sym: 309; act: 202 ),
  ( sym: 310; act: 203 ),
  ( sym: 311; act: 204 ),
  ( sym: 312; act: 205 ),
  ( sym: 313; act: 206 ),
  ( sym: 314; act: 207 ),
  ( sym: 315; act: 208 ),
  ( sym: 316; act: 209 ),
  ( sym: 317; act: 210 ),
  ( sym: 318; act: 211 ),
  ( sym: 319; act: 212 ),
  ( sym: 320; act: 213 ),
  ( sym: 321; act: 214 ),
  ( sym: 266; act: -98 ),
  ( sym: 267; act: -98 ),
  ( sym: 268; act: -98 ),
  ( sym: 269; act: -98 ),
  ( sym: 270; act: -98 ),
  ( sym: 272; act: -98 ),
  ( sym: 301; act: -98 ),
{ 179: }
  ( sym: 256; act: 80 ),
  ( sym: 268; act: 81 ),
  ( sym: 277; act: 23 ),
  ( sym: 287; act: 82 ),
  ( sym: 288; act: 83 ),
  ( sym: 289; act: 84 ),
  ( sym: 290; act: 85 ),
  ( sym: 314; act: 86 ),
  ( sym: 319; act: 87 ),
{ 180: }
{ 181: }
{ 182: }
{ 183: }
  ( sym: 304; act: 198 ),
  ( sym: 306; act: 199 ),
  ( sym: 307; act: 200 ),
  ( sym: 308; act: 201 ),
  ( sym: 309; act: 202 ),
  ( sym: 310; act: 203 ),
  ( sym: 311; act: 204 ),
  ( sym: 312; act: 205 ),
  ( sym: 313; act: 206 ),
  ( sym: 314; act: 207 ),
  ( sym: 315; act: 208 ),
  ( sym: 316; act: 209 ),
  ( sym: 317; act: 210 ),
  ( sym: 318; act: 211 ),
  ( sym: 319; act: 212 ),
  ( sym: 320; act: 213 ),
  ( sym: 321; act: 214 ),
  ( sym: 267; act: -159 ),
  ( sym: 269; act: -159 ),
  ( sym: 273; act: -159 ),
{ 184: }
  ( sym: 292; act: 234 ),
  ( sym: 268; act: -3 ),
{ 185: }
  ( sym: 291; act: 235 ),
{ 186: }
  ( sym: 268; act: 236 ),
{ 187: }
  ( sym: 268; act: 107 ),
  ( sym: 277; act: 23 ),
  ( sym: 278; act: 108 ),
  ( sym: 279; act: 109 ),
  ( sym: 280; act: 24 ),
  ( sym: 281; act: 25 ),
  ( sym: 282; act: 26 ),
  ( sym: 283; act: 27 ),
  ( sym: 284; act: 28 ),
  ( sym: 285; act: 29 ),
  ( sym: 286; act: 30 ),
  ( sym: 314; act: 111 ),
  ( sym: 316; act: 112 ),
  ( sym: 321; act: 113 ),
  ( sym: 327; act: 32 ),
{ 188: }
{ 189: }
{ 190: }
  ( sym: 267; act: 238 ),
  ( sym: 269; act: -165 ),
  ( sym: 271; act: -165 ),
{ 191: }
  ( sym: 269; act: 239 ),
{ 192: }
  ( sym: 304; act: 198 ),
  ( sym: 306; act: 199 ),
  ( sym: 307; act: 200 ),
  ( sym: 308; act: 201 ),
  ( sym: 309; act: 202 ),
  ( sym: 310; act: 203 ),
  ( sym: 311; act: 204 ),
  ( sym: 312; act: 205 ),
  ( sym: 313; act: 206 ),
  ( sym: 314; act: 207 ),
  ( sym: 315; act: 208 ),
  ( sym: 316; act: 209 ),
  ( sym: 317; act: 210 ),
  ( sym: 318; act: 211 ),
  ( sym: 319; act: 212 ),
  ( sym: 320; act: 213 ),
  ( sym: 321; act: 214 ),
  ( sym: 267; act: -167 ),
  ( sym: 269; act: -167 ),
  ( sym: 271; act: -167 ),
{ 193: }
  ( sym: 271; act: 240 ),
{ 194: }
{ 195: }
  ( sym: 319; act: 241 ),
{ 196: }
  ( sym: 268; act: 107 ),
  ( sym: 277; act: 23 ),
  ( sym: 278; act: 108 ),
  ( sym: 279; act: 109 ),
  ( sym: 280; act: 24 ),
  ( sym: 281; act: 25 ),
  ( sym: 282; act: 26 ),
  ( sym: 283; act: 27 ),
  ( sym: 284; act: 28 ),
  ( sym: 285; act: 29 ),
  ( sym: 286; act: 30 ),
  ( sym: 314; act: 111 ),
  ( sym: 316; act: 112 ),
  ( sym: 321; act: 113 ),
  ( sym: 327; act: 32 ),
{ 197: }
  ( sym: 269; act: 243 ),
{ 198: }
  ( sym: 268; act: 107 ),
  ( sym: 277; act: 23 ),
  ( sym: 278; act: 108 ),
  ( sym: 279; act: 109 ),
  ( sym: 280; act: 24 ),
  ( sym: 281; act: 25 ),
  ( sym: 282; act: 26 ),
  ( sym: 283; act: 27 ),
  ( sym: 284; act: 28 ),
  ( sym: 285; act: 29 ),
  ( sym: 286; act: 30 ),
  ( sym: 314; act: 111 ),
  ( sym: 316; act: 112 ),
  ( sym: 321; act: 113 ),
  ( sym: 327; act: 32 ),
{ 199: }
  ( sym: 268; act: 107 ),
  ( sym: 277; act: 23 ),
  ( sym: 278; act: 108 ),
  ( sym: 279; act: 109 ),
  ( sym: 280; act: 24 ),
  ( sym: 281; act: 25 ),
  ( sym: 282; act: 26 ),
  ( sym: 283; act: 27 ),
  ( sym: 284; act: 28 ),
  ( sym: 285; act: 29 ),
  ( sym: 286; act: 30 ),
  ( sym: 314; act: 111 ),
  ( sym: 316; act: 112 ),
  ( sym: 321; act: 113 ),
  ( sym: 327; act: 32 ),
{ 200: }
  ( sym: 268; act: 107 ),
  ( sym: 277; act: 23 ),
  ( sym: 278; act: 108 ),
  ( sym: 279; act: 109 ),
  ( sym: 280; act: 24 ),
  ( sym: 281; act: 25 ),
  ( sym: 282; act: 26 ),
  ( sym: 283; act: 27 ),
  ( sym: 284; act: 28 ),
  ( sym: 285; act: 29 ),
  ( sym: 286; act: 30 ),
  ( sym: 314; act: 111 ),
  ( sym: 316; act: 112 ),
  ( sym: 321; act: 113 ),
  ( sym: 327; act: 32 ),
{ 201: }
  ( sym: 268; act: 107 ),
  ( sym: 277; act: 23 ),
  ( sym: 278; act: 108 ),
  ( sym: 279; act: 109 ),
  ( sym: 280; act: 24 ),
  ( sym: 281; act: 25 ),
  ( sym: 282; act: 26 ),
  ( sym: 283; act: 27 ),
  ( sym: 284; act: 28 ),
  ( sym: 285; act: 29 ),
  ( sym: 286; act: 30 ),
  ( sym: 314; act: 111 ),
  ( sym: 316; act: 112 ),
  ( sym: 321; act: 113 ),
  ( sym: 327; act: 32 ),
{ 202: }
  ( sym: 268; act: 107 ),
  ( sym: 277; act: 23 ),
  ( sym: 278; act: 108 ),
  ( sym: 279; act: 109 ),
  ( sym: 280; act: 24 ),
  ( sym: 281; act: 25 ),
  ( sym: 282; act: 26 ),
  ( sym: 283; act: 27 ),
  ( sym: 284; act: 28 ),
  ( sym: 285; act: 29 ),
  ( sym: 286; act: 30 ),
  ( sym: 314; act: 111 ),
  ( sym: 316; act: 112 ),
  ( sym: 321; act: 113 ),
  ( sym: 327; act: 32 ),
{ 203: }
  ( sym: 268; act: 107 ),
  ( sym: 277; act: 23 ),
  ( sym: 278; act: 108 ),
  ( sym: 279; act: 109 ),
  ( sym: 280; act: 24 ),
  ( sym: 281; act: 25 ),
  ( sym: 282; act: 26 ),
  ( sym: 283; act: 27 ),
  ( sym: 284; act: 28 ),
  ( sym: 285; act: 29 ),
  ( sym: 286; act: 30 ),
  ( sym: 314; act: 111 ),
  ( sym: 316; act: 112 ),
  ( sym: 321; act: 113 ),
  ( sym: 327; act: 32 ),
{ 204: }
  ( sym: 268; act: 107 ),
  ( sym: 277; act: 23 ),
  ( sym: 278; act: 108 ),
  ( sym: 279; act: 109 ),
  ( sym: 280; act: 24 ),
  ( sym: 281; act: 25 ),
  ( sym: 282; act: 26 ),
  ( sym: 283; act: 27 ),
  ( sym: 284; act: 28 ),
  ( sym: 285; act: 29 ),
  ( sym: 286; act: 30 ),
  ( sym: 314; act: 111 ),
  ( sym: 316; act: 112 ),
  ( sym: 321; act: 113 ),
  ( sym: 327; act: 32 ),
{ 205: }
  ( sym: 268; act: 107 ),
  ( sym: 277; act: 23 ),
  ( sym: 278; act: 108 ),
  ( sym: 279; act: 109 ),
  ( sym: 280; act: 24 ),
  ( sym: 281; act: 25 ),
  ( sym: 282; act: 26 ),
  ( sym: 283; act: 27 ),
  ( sym: 284; act: 28 ),
  ( sym: 285; act: 29 ),
  ( sym: 286; act: 30 ),
  ( sym: 314; act: 111 ),
  ( sym: 316; act: 112 ),
  ( sym: 321; act: 113 ),
  ( sym: 327; act: 32 ),
{ 206: }
  ( sym: 268; act: 107 ),
  ( sym: 277; act: 23 ),
  ( sym: 278; act: 108 ),
  ( sym: 279; act: 109 ),
  ( sym: 280; act: 24 ),
  ( sym: 281; act: 25 ),
  ( sym: 282; act: 26 ),
  ( sym: 283; act: 27 ),
  ( sym: 284; act: 28 ),
  ( sym: 285; act: 29 ),
  ( sym: 286; act: 30 ),
  ( sym: 314; act: 111 ),
  ( sym: 316; act: 112 ),
  ( sym: 321; act: 113 ),
  ( sym: 327; act: 32 ),
{ 207: }
  ( sym: 268; act: 107 ),
  ( sym: 277; act: 23 ),
  ( sym: 278; act: 108 ),
  ( sym: 279; act: 109 ),
  ( sym: 280; act: 24 ),
  ( sym: 281; act: 25 ),
  ( sym: 282; act: 26 ),
  ( sym: 283; act: 27 ),
  ( sym: 284; act: 28 ),
  ( sym: 285; act: 29 ),
  ( sym: 286; act: 30 ),
  ( sym: 314; act: 111 ),
  ( sym: 316; act: 112 ),
  ( sym: 321; act: 113 ),
  ( sym: 327; act: 32 ),
{ 208: }
  ( sym: 268; act: 107 ),
  ( sym: 277; act: 23 ),
  ( sym: 278; act: 108 ),
  ( sym: 279; act: 109 ),
  ( sym: 280; act: 24 ),
  ( sym: 281; act: 25 ),
  ( sym: 282; act: 26 ),
  ( sym: 283; act: 27 ),
  ( sym: 284; act: 28 ),
  ( sym: 285; act: 29 ),
  ( sym: 286; act: 30 ),
  ( sym: 314; act: 111 ),
  ( sym: 316; act: 112 ),
  ( sym: 321; act: 113 ),
  ( sym: 327; act: 32 ),
{ 209: }
  ( sym: 268; act: 107 ),
  ( sym: 277; act: 23 ),
  ( sym: 278; act: 108 ),
  ( sym: 279; act: 109 ),
  ( sym: 280; act: 24 ),
  ( sym: 281; act: 25 ),
  ( sym: 282; act: 26 ),
  ( sym: 283; act: 27 ),
  ( sym: 284; act: 28 ),
  ( sym: 285; act: 29 ),
  ( sym: 286; act: 30 ),
  ( sym: 314; act: 111 ),
  ( sym: 316; act: 112 ),
  ( sym: 321; act: 113 ),
  ( sym: 327; act: 32 ),
{ 210: }
  ( sym: 268; act: 107 ),
  ( sym: 277; act: 23 ),
  ( sym: 278; act: 108 ),
  ( sym: 279; act: 109 ),
  ( sym: 280; act: 24 ),
  ( sym: 281; act: 25 ),
  ( sym: 282; act: 26 ),
  ( sym: 283; act: 27 ),
  ( sym: 284; act: 28 ),
  ( sym: 285; act: 29 ),
  ( sym: 286; act: 30 ),
  ( sym: 314; act: 111 ),
  ( sym: 316; act: 112 ),
  ( sym: 321; act: 113 ),
  ( sym: 327; act: 32 ),
{ 211: }
  ( sym: 268; act: 107 ),
  ( sym: 277; act: 23 ),
  ( sym: 278; act: 108 ),
  ( sym: 279; act: 109 ),
  ( sym: 280; act: 24 ),
  ( sym: 281; act: 25 ),
  ( sym: 282; act: 26 ),
  ( sym: 283; act: 27 ),
  ( sym: 284; act: 28 ),
  ( sym: 285; act: 29 ),
  ( sym: 286; act: 30 ),
  ( sym: 314; act: 111 ),
  ( sym: 316; act: 112 ),
  ( sym: 321; act: 113 ),
  ( sym: 327; act: 32 ),
{ 212: }
  ( sym: 268; act: 107 ),
  ( sym: 277; act: 23 ),
  ( sym: 278; act: 108 ),
  ( sym: 279; act: 109 ),
  ( sym: 280; act: 24 ),
  ( sym: 281; act: 25 ),
  ( sym: 282; act: 26 ),
  ( sym: 283; act: 27 ),
  ( sym: 284; act: 28 ),
  ( sym: 285; act: 29 ),
  ( sym: 286; act: 30 ),
  ( sym: 314; act: 111 ),
  ( sym: 316; act: 112 ),
  ( sym: 321; act: 113 ),
  ( sym: 327; act: 32 ),
{ 213: }
  ( sym: 268; act: 107 ),
  ( sym: 277; act: 23 ),
  ( sym: 278; act: 108 ),
  ( sym: 279; act: 109 ),
  ( sym: 280; act: 24 ),
  ( sym: 281; act: 25 ),
  ( sym: 282; act: 26 ),
  ( sym: 283; act: 27 ),
  ( sym: 284; act: 28 ),
  ( sym: 285; act: 29 ),
  ( sym: 286; act: 30 ),
  ( sym: 314; act: 111 ),
  ( sym: 316; act: 112 ),
  ( sym: 321; act: 113 ),
  ( sym: 327; act: 32 ),
{ 214: }
  ( sym: 268; act: 107 ),
  ( sym: 277; act: 23 ),
  ( sym: 278; act: 108 ),
  ( sym: 279; act: 109 ),
  ( sym: 280; act: 24 ),
  ( sym: 281; act: 25 ),
  ( sym: 282; act: 26 ),
  ( sym: 283; act: 27 ),
  ( sym: 284; act: 28 ),
  ( sym: 285; act: 29 ),
  ( sym: 286; act: 30 ),
  ( sym: 314; act: 111 ),
  ( sym: 316; act: 112 ),
  ( sym: 321; act: 113 ),
  ( sym: 327; act: 32 ),
{ 215: }
  ( sym: 268; act: 107 ),
  ( sym: 277; act: 23 ),
  ( sym: 278; act: 108 ),
  ( sym: 279; act: 109 ),
  ( sym: 280; act: 24 ),
  ( sym: 281; act: 25 ),
  ( sym: 282; act: 26 ),
  ( sym: 283; act: 27 ),
  ( sym: 284; act: 28 ),
  ( sym: 285; act: 29 ),
  ( sym: 286; act: 30 ),
  ( sym: 314; act: 111 ),
  ( sym: 316; act: 112 ),
  ( sym: 321; act: 113 ),
  ( sym: 327; act: 32 ),
  ( sym: 265; act: -138 ),
  ( sym: 266; act: -138 ),
  ( sym: 267; act: -138 ),
  ( sym: 269; act: -138 ),
  ( sym: 270; act: -138 ),
  ( sym: 271; act: -138 ),
  ( sym: 272; act: -138 ),
  ( sym: 273; act: -138 ),
  ( sym: 291; act: -138 ),
  ( sym: 301; act: -138 ),
  ( sym: 304; act: -138 ),
  ( sym: 306; act: -138 ),
  ( sym: 307; act: -138 ),
  ( sym: 308; act: -138 ),
  ( sym: 309; act: -138 ),
  ( sym: 310; act: -138 ),
  ( sym: 311; act: -138 ),
  ( sym: 312; act: -138 ),
  ( sym: 313; act: -138 ),
  ( sym: 315; act: -138 ),
  ( sym: 317; act: -138 ),
  ( sym: 318; act: -138 ),
  ( sym: 319; act: -138 ),
  ( sym: 320; act: -138 ),
  ( sym: 324; act: -138 ),
  ( sym: 325; act: -138 ),
{ 216: }
  ( sym: 269; act: 264 ),
  ( sym: 324; act: 144 ),
  ( sym: 325; act: 145 ),
{ 217: }
  ( sym: 274; act: 20 ),
  ( sym: 275; act: 21 ),
  ( sym: 276; act: 22 ),
  ( sym: 277; act: 23 ),
  ( sym: 280; act: 24 ),
  ( sym: 281; act: 25 ),
  ( sym: 282; act: 26 ),
  ( sym: 283; act: 27 ),
  ( sym: 284; act: 28 ),
  ( sym: 285; act: 29 ),
  ( sym: 286; act: 30 ),
  ( sym: 287; act: 31 ),
  ( sym: 303; act: 165 ),
  ( sym: 327; act: 32 ),
  ( sym: 269; act: -90 ),
{ 218: }
{ 219: }
  ( sym: 319; act: 266 ),
{ 220: }
  ( sym: 268; act: 268 ),
  ( sym: 270; act: 269 ),
  ( sym: 267; act: -86 ),
  ( sym: 269; act: -86 ),
{ 221: }
  ( sym: 268; act: 118 ),
  ( sym: 270; act: 270 ),
  ( sym: 267; act: -84 ),
  ( sym: 269; act: -84 ),
{ 222: }
  ( sym: 268; act: 222 ),
  ( sym: 277; act: 23 ),
  ( sym: 287; act: 223 ),
  ( sym: 288; act: 83 ),
  ( sym: 289; act: 84 ),
  ( sym: 290; act: 85 ),
  ( sym: 314; act: 86 ),
  ( sym: 319; act: 273 ),
  ( sym: 269; act: -116 ),
  ( sym: 270; act: -116 ),
{ 223: }
  ( sym: 268; act: 222 ),
  ( sym: 277; act: 23 ),
  ( sym: 287; act: 223 ),
  ( sym: 288; act: 83 ),
  ( sym: 289; act: 84 ),
  ( sym: 290; act: 85 ),
  ( sym: 314; act: 86 ),
  ( sym: 319; act: 273 ),
  ( sym: 267; act: -116 ),
  ( sym: 269; act: -116 ),
  ( sym: 270; act: -116 ),
{ 224: }
  ( sym: 268; act: 222 ),
  ( sym: 277; act: 23 ),
  ( sym: 287; act: 223 ),
  ( sym: 288; act: 83 ),
  ( sym: 289; act: 84 ),
  ( sym: 290; act: 85 ),
  ( sym: 314; act: 86 ),
  ( sym: 319; act: 273 ),
  ( sym: 267; act: -116 ),
  ( sym: 269; act: -116 ),
  ( sym: 270; act: -116 ),
{ 225: }
{ 226: }
{ 227: }
{ 228: }
{ 229: }
{ 230: }
  ( sym: 268; act: 107 ),
  ( sym: 277; act: 23 ),
  ( sym: 278; act: 108 ),
  ( sym: 279; act: 109 ),
  ( sym: 280; act: 24 ),
  ( sym: 281; act: 25 ),
  ( sym: 282; act: 26 ),
  ( sym: 283; act: 27 ),
  ( sym: 284; act: 28 ),
  ( sym: 285; act: 29 ),
  ( sym: 286; act: 30 ),
  ( sym: 314; act: 111 ),
  ( sym: 316; act: 112 ),
  ( sym: 321; act: 113 ),
  ( sym: 327; act: 32 ),
{ 231: }
  ( sym: 269; act: 279 ),
{ 232: }
{ 233: }
  ( sym: 268; act: 280 ),
{ 234: }
{ 235: }
{ 236: }
  ( sym: 268; act: 107 ),
  ( sym: 277; act: 23 ),
  ( sym: 278; act: 108 ),
  ( sym: 279; act: 109 ),
  ( sym: 280; act: 24 ),
  ( sym: 281; act: 25 ),
  ( sym: 282; act: 26 ),
  ( sym: 283; act: 27 ),
  ( sym: 284; act: 28 ),
  ( sym: 285; act: 29 ),
  ( sym: 286; act: 30 ),
  ( sym: 314; act: 111 ),
  ( sym: 316; act: 112 ),
  ( sym: 321; act: 113 ),
  ( sym: 327; act: 32 ),
{ 237: }
{ 238: }
  ( sym: 268; act: 107 ),
  ( sym: 277; act: 23 ),
  ( sym: 278; act: 108 ),
  ( sym: 279; act: 109 ),
  ( sym: 280; act: 24 ),
  ( sym: 281; act: 25 ),
  ( sym: 282; act: 26 ),
  ( sym: 283; act: 27 ),
  ( sym: 284; act: 28 ),
  ( sym: 285; act: 29 ),
  ( sym: 286; act: 30 ),
  ( sym: 314; act: 111 ),
  ( sym: 316; act: 112 ),
  ( sym: 321; act: 113 ),
  ( sym: 327; act: 32 ),
  ( sym: 269; act: -166 ),
  ( sym: 271; act: -166 ),
{ 239: }
{ 240: }
{ 241: }
  ( sym: 269; act: 283 ),
{ 242: }
  ( sym: 324; act: 144 ),
  ( sym: 325; act: 145 ),
  ( sym: 265; act: -149 ),
  ( sym: 266; act: -149 ),
  ( sym: 267; act: -149 ),
  ( sym: 268; act: -149 ),
  ( sym: 269; act: -149 ),
  ( sym: 270; act: -149 ),
  ( sym: 271; act: -149 ),
  ( sym: 272; act: -149 ),
  ( sym: 273; act: -149 ),
  ( sym: 291; act: -149 ),
  ( sym: 301; act: -149 ),
  ( sym: 304; act: -149 ),
  ( sym: 306; act: -149 ),
  ( sym: 307; act: -149 ),
  ( sym: 308; act: -149 ),
  ( sym: 309; act: -149 ),
  ( sym: 310; act: -149 ),
  ( sym: 311; act: -149 ),
  ( sym: 312; act: -149 ),
  ( sym: 313; act: -149 ),
  ( sym: 314; act: -149 ),
  ( sym: 315; act: -149 ),
  ( sym: 316; act: -149 ),
  ( sym: 317; act: -149 ),
  ( sym: 318; act: -149 ),
  ( sym: 319; act: -149 ),
  ( sym: 320; act: -149 ),
  ( sym: 321; act: -149 ),
{ 243: }
  ( sym: 268; act: 107 ),
  ( sym: 277; act: 23 ),
  ( sym: 278; act: 108 ),
  ( sym: 279; act: 109 ),
  ( sym: 280; act: 24 ),
  ( sym: 281; act: 25 ),
  ( sym: 282; act: 26 ),
  ( sym: 283; act: 27 ),
  ( sym: 284; act: 28 ),
  ( sym: 285; act: 29 ),
  ( sym: 286; act: 30 ),
  ( sym: 314; act: 111 ),
  ( sym: 316; act: 112 ),
  ( sym: 321; act: 113 ),
  ( sym: 327; act: 32 ),
{ 244: }
  ( sym: 304; act: 198 ),
  ( sym: 306; act: 199 ),
  ( sym: 307; act: 200 ),
  ( sym: 308; act: 201 ),
  ( sym: 309; act: 202 ),
  ( sym: 310; act: 203 ),
  ( sym: 311; act: 204 ),
  ( sym: 312; act: 205 ),
  ( sym: 313; act: 206 ),
  ( sym: 314; act: 207 ),
  ( sym: 315; act: 208 ),
  ( sym: 316; act: 209 ),
  ( sym: 317; act: 210 ),
  ( sym: 318; act: 211 ),
  ( sym: 319; act: 212 ),
  ( sym: 320; act: 213 ),
  ( sym: 321; act: 214 ),
  ( sym: 265; act: -118 ),
  ( sym: 266; act: -118 ),
  ( sym: 267; act: -118 ),
  ( sym: 268; act: -118 ),
  ( sym: 269; act: -118 ),
  ( sym: 270; act: -118 ),
  ( sym: 271; act: -118 ),
  ( sym: 272; act: -118 ),
  ( sym: 273; act: -118 ),
  ( sym: 291; act: -118 ),
  ( sym: 301; act: -118 ),
  ( sym: 324; act: -118 ),
  ( sym: 325; act: -118 ),
{ 245: }
  ( sym: 312; act: 205 ),
  ( sym: 313; act: 206 ),
  ( sym: 314; act: 207 ),
  ( sym: 315; act: 208 ),
  ( sym: 316; act: 209 ),
  ( sym: 317; act: 210 ),
  ( sym: 318; act: 211 ),
  ( sym: 319; act: 212 ),
  ( sym: 320; act: 213 ),
  ( sym: 321; act: 214 ),
  ( sym: 265; act: -119 ),
  ( sym: 266; act: -119 ),
  ( sym: 267; act: -119 ),
  ( sym: 268; act: -119 ),
  ( sym: 269; act: -119 ),
  ( sym: 270; act: -119 ),
  ( sym: 271; act: -119 ),
  ( sym: 272; act: -119 ),
  ( sym: 273; act: -119 ),
  ( sym: 291; act: -119 ),
  ( sym: 301; act: -119 ),
  ( sym: 304; act: -119 ),
  ( sym: 306; act: -119 ),
  ( sym: 307; act: -119 ),
  ( sym: 308; act: -119 ),
  ( sym: 309; act: -119 ),
  ( sym: 310; act: -119 ),
  ( sym: 311; act: -119 ),
  ( sym: 324; act: -119 ),
  ( sym: 325; act: -119 ),
{ 246: }
  ( sym: 312; act: 205 ),
  ( sym: 313; act: 206 ),
  ( sym: 314; act: 207 ),
  ( sym: 315; act: 208 ),
  ( sym: 316; act: 209 ),
  ( sym: 317; act: 210 ),
  ( sym: 318; act: 211 ),
  ( sym: 319; act: 212 ),
  ( sym: 320; act: 213 ),
  ( sym: 321; act: 214 ),
  ( sym: 265; act: -120 ),
  ( sym: 266; act: -120 ),
  ( sym: 267; act: -120 ),
  ( sym: 268; act: -120 ),
  ( sym: 269; act: -120 ),
  ( sym: 270; act: -120 ),
  ( sym: 271; act: -120 ),
  ( sym: 272; act: -120 ),
  ( sym: 273; act: -120 ),
  ( sym: 291; act: -120 ),
  ( sym: 301; act: -120 ),
  ( sym: 304; act: -120 ),
  ( sym: 306; act: -120 ),
  ( sym: 307; act: -120 ),
  ( sym: 308; act: -120 ),
  ( sym: 309; act: -120 ),
  ( sym: 310; act: -120 ),
  ( sym: 311; act: -120 ),
  ( sym: 324; act: -120 ),
  ( sym: 325; act: -120 ),
{ 247: }
  ( sym: 312; act: 205 ),
  ( sym: 313; act: 206 ),
  ( sym: 314; act: 207 ),
  ( sym: 315; act: 208 ),
  ( sym: 316; act: 209 ),
  ( sym: 317; act: 210 ),
  ( sym: 318; act: 211 ),
  ( sym: 319; act: 212 ),
  ( sym: 320; act: 213 ),
  ( sym: 321; act: 214 ),
  ( sym: 265; act: -121 ),
  ( sym: 266; act: -121 ),
  ( sym: 267; act: -121 ),
  ( sym: 268; act: -121 ),
  ( sym: 269; act: -121 ),
  ( sym: 270; act: -121 ),
  ( sym: 271; act: -121 ),
  ( sym: 272; act: -121 ),
  ( sym: 273; act: -121 ),
  ( sym: 291; act: -121 ),
  ( sym: 301; act: -121 ),
  ( sym: 304; act: -121 ),
  ( sym: 306; act: -121 ),
  ( sym: 307; act: -121 ),
  ( sym: 308; act: -121 ),
  ( sym: 309; act: -121 ),
  ( sym: 310; act: -121 ),
  ( sym: 311; act: -121 ),
  ( sym: 324; act: -121 ),
  ( sym: 325; act: -121 ),
{ 248: }
  ( sym: 312; act: 205 ),
  ( sym: 313; act: 206 ),
  ( sym: 314; act: 207 ),
  ( sym: 315; act: 208 ),
  ( sym: 316; act: 209 ),
  ( sym: 317; act: 210 ),
  ( sym: 318; act: 211 ),
  ( sym: 319; act: 212 ),
  ( sym: 320; act: 213 ),
  ( sym: 321; act: 214 ),
  ( sym: 265; act: -123 ),
  ( sym: 266; act: -123 ),
  ( sym: 267; act: -123 ),
  ( sym: 268; act: -123 ),
  ( sym: 269; act: -123 ),
  ( sym: 270; act: -123 ),
  ( sym: 271; act: -123 ),
  ( sym: 272; act: -123 ),
  ( sym: 273; act: -123 ),
  ( sym: 291; act: -123 ),
  ( sym: 301; act: -123 ),
  ( sym: 304; act: -123 ),
  ( sym: 306; act: -123 ),
  ( sym: 307; act: -123 ),
  ( sym: 308; act: -123 ),
  ( sym: 309; act: -123 ),
  ( sym: 310; act: -123 ),
  ( sym: 311; act: -123 ),
  ( sym: 324; act: -123 ),
  ( sym: 325; act: -123 ),
{ 249: }
  ( sym: 312; act: 205 ),
  ( sym: 313; act: 206 ),
  ( sym: 314; act: 207 ),
  ( sym: 315; act: 208 ),
  ( sym: 316; act: 209 ),
  ( sym: 317; act: 210 ),
  ( sym: 318; act: 211 ),
  ( sym: 319; act: 212 ),
  ( sym: 320; act: 213 ),
  ( sym: 321; act: 214 ),
  ( sym: 265; act: -122 ),
  ( sym: 266; act: -122 ),
  ( sym: 267; act: -122 ),
  ( sym: 268; act: -122 ),
  ( sym: 269; act: -122 ),
  ( sym: 270; act: -122 ),
  ( sym: 271; act: -122 ),
  ( sym: 272; act: -122 ),
  ( sym: 273; act: -122 ),
  ( sym: 291; act: -122 ),
  ( sym: 301; act: -122 ),
  ( sym: 304; act: -122 ),
  ( sym: 306; act: -122 ),
  ( sym: 307; act: -122 ),
  ( sym: 308; act: -122 ),
  ( sym: 309; act: -122 ),
  ( sym: 310; act: -122 ),
  ( sym: 311; act: -122 ),
  ( sym: 324; act: -122 ),
  ( sym: 325; act: -122 ),
{ 250: }
  ( sym: 312; act: 205 ),
  ( sym: 313; act: 206 ),
  ( sym: 314; act: 207 ),
  ( sym: 315; act: 208 ),
  ( sym: 316; act: 209 ),
  ( sym: 317; act: 210 ),
  ( sym: 318; act: 211 ),
  ( sym: 319; act: 212 ),
  ( sym: 320; act: 213 ),
  ( sym: 321; act: 214 ),
  ( sym: 265; act: -124 ),
  ( sym: 266; act: -124 ),
  ( sym: 267; act: -124 ),
  ( sym: 268; act: -124 ),
  ( sym: 269; act: -124 ),
  ( sym: 270; act: -124 ),
  ( sym: 271; act: -124 ),
  ( sym: 272; act: -124 ),
  ( sym: 273; act: -124 ),
  ( sym: 291; act: -124 ),
  ( sym: 301; act: -124 ),
  ( sym: 304; act: -124 ),
  ( sym: 306; act: -124 ),
  ( sym: 307; act: -124 ),
  ( sym: 308; act: -124 ),
  ( sym: 309; act: -124 ),
  ( sym: 310; act: -124 ),
  ( sym: 311; act: -124 ),
  ( sym: 324; act: -124 ),
  ( sym: 325; act: -124 ),
{ 251: }
{ 252: }
  ( sym: 265; act: 285 ),
  ( sym: 304; act: 198 ),
  ( sym: 306; act: 199 ),
  ( sym: 307; act: 200 ),
  ( sym: 308; act: 201 ),
  ( sym: 309; act: 202 ),
  ( sym: 310; act: 203 ),
  ( sym: 311; act: 204 ),
  ( sym: 312; act: 205 ),
  ( sym: 313; act: 206 ),
  ( sym: 314; act: 207 ),
  ( sym: 315; act: 208 ),
  ( sym: 316; act: 209 ),
  ( sym: 317; act: 210 ),
  ( sym: 318; act: 211 ),
  ( sym: 319; act: 212 ),
  ( sym: 320; act: 213 ),
  ( sym: 321; act: 214 ),
{ 253: }
  ( sym: 314; act: 207 ),
  ( sym: 315; act: 208 ),
  ( sym: 316; act: 209 ),
  ( sym: 317; act: 210 ),
  ( sym: 318; act: 211 ),
  ( sym: 319; act: 212 ),
  ( sym: 320; act: 213 ),
  ( sym: 321; act: 214 ),
  ( sym: 265; act: -129 ),
  ( sym: 266; act: -129 ),
  ( sym: 267; act: -129 ),
  ( sym: 268; act: -129 ),
  ( sym: 269; act: -129 ),
  ( sym: 270; act: -129 ),
  ( sym: 271; act: -129 ),
  ( sym: 272; act: -129 ),
  ( sym: 273; act: -129 ),
  ( sym: 291; act: -129 ),
  ( sym: 301; act: -129 ),
  ( sym: 304; act: -129 ),
  ( sym: 306; act: -129 ),
  ( sym: 307; act: -129 ),
  ( sym: 308; act: -129 ),
  ( sym: 309; act: -129 ),
  ( sym: 310; act: -129 ),
  ( sym: 311; act: -129 ),
  ( sym: 312; act: -129 ),
  ( sym: 313; act: -129 ),
  ( sym: 324; act: -129 ),
  ( sym: 325; act: -129 ),
{ 254: }
  ( sym: 315; act: 208 ),
  ( sym: 316; act: 209 ),
  ( sym: 317; act: 210 ),
  ( sym: 318; act: 211 ),
  ( sym: 319; act: 212 ),
  ( sym: 320; act: 213 ),
  ( sym: 321; act: 214 ),
  ( sym: 265; act: -130 ),
  ( sym: 266; act: -130 ),
  ( sym: 267; act: -130 ),
  ( sym: 268; act: -130 ),
  ( sym: 269; act: -130 ),
  ( sym: 270; act: -130 ),
  ( sym: 271; act: -130 ),
  ( sym: 272; act: -130 ),
  ( sym: 273; act: -130 ),
  ( sym: 291; act: -130 ),
  ( sym: 301; act: -130 ),
  ( sym: 304; act: -130 ),
  ( sym: 306; act: -130 ),
  ( sym: 307; act: -130 ),
  ( sym: 308; act: -130 ),
  ( sym: 309; act: -130 ),
  ( sym: 310; act: -130 ),
  ( sym: 311; act: -130 ),
  ( sym: 312; act: -130 ),
  ( sym: 313; act: -130 ),
  ( sym: 314; act: -130 ),
  ( sym: 324; act: -130 ),
  ( sym: 325; act: -130 ),
{ 255: }
  ( sym: 317; act: 210 ),
  ( sym: 318; act: 211 ),
  ( sym: 319; act: 212 ),
  ( sym: 320; act: 213 ),
  ( sym: 321; act: 214 ),
  ( sym: 265; act: -125 ),
  ( sym: 266; act: -125 ),
  ( sym: 267; act: -125 ),
  ( sym: 268; act: -125 ),
  ( sym: 269; act: -125 ),
  ( sym: 270; act: -125 ),
  ( sym: 271; act: -125 ),
  ( sym: 272; act: -125 ),
  ( sym: 273; act: -125 ),
  ( sym: 291; act: -125 ),
  ( sym: 301; act: -125 ),
  ( sym: 304; act: -125 ),
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
  ( sym: 316; act: -125 ),
  ( sym: 324; act: -125 ),
  ( sym: 325; act: -125 ),
{ 256: }
  ( sym: 317; act: 210 ),
  ( sym: 318; act: 211 ),
  ( sym: 319; act: 212 ),
  ( sym: 320; act: 213 ),
  ( sym: 321; act: 214 ),
  ( sym: 265; act: -126 ),
  ( sym: 266; act: -126 ),
  ( sym: 267; act: -126 ),
  ( sym: 268; act: -126 ),
  ( sym: 269; act: -126 ),
  ( sym: 270; act: -126 ),
  ( sym: 271; act: -126 ),
  ( sym: 272; act: -126 ),
  ( sym: 273; act: -126 ),
  ( sym: 291; act: -126 ),
  ( sym: 301; act: -126 ),
  ( sym: 304; act: -126 ),
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
  ( sym: 316; act: -126 ),
  ( sym: 324; act: -126 ),
  ( sym: 325; act: -126 ),
{ 257: }
  ( sym: 319; act: 212 ),
  ( sym: 320; act: 213 ),
  ( sym: 321; act: 214 ),
  ( sym: 265; act: -133 ),
  ( sym: 266; act: -133 ),
  ( sym: 267; act: -133 ),
  ( sym: 268; act: -133 ),
  ( sym: 269; act: -133 ),
  ( sym: 270; act: -133 ),
  ( sym: 271; act: -133 ),
  ( sym: 272; act: -133 ),
  ( sym: 273; act: -133 ),
  ( sym: 291; act: -133 ),
  ( sym: 301; act: -133 ),
  ( sym: 304; act: -133 ),
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
  ( sym: 316; act: -133 ),
  ( sym: 317; act: -133 ),
  ( sym: 318; act: -133 ),
  ( sym: 324; act: -133 ),
  ( sym: 325; act: -133 ),
{ 258: }
  ( sym: 319; act: 212 ),
  ( sym: 320; act: 213 ),
  ( sym: 321; act: 214 ),
  ( sym: 265; act: -132 ),
  ( sym: 266; act: -132 ),
  ( sym: 267; act: -132 ),
  ( sym: 268; act: -132 ),
  ( sym: 269; act: -132 ),
  ( sym: 270; act: -132 ),
  ( sym: 271; act: -132 ),
  ( sym: 272; act: -132 ),
  ( sym: 273; act: -132 ),
  ( sym: 291; act: -132 ),
  ( sym: 301; act: -132 ),
  ( sym: 304; act: -132 ),
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
  ( sym: 316; act: -132 ),
  ( sym: 317; act: -132 ),
  ( sym: 318; act: -132 ),
  ( sym: 324; act: -132 ),
  ( sym: 325; act: -132 ),
{ 259: }
  ( sym: 321; act: 214 ),
  ( sym: 265; act: -127 ),
  ( sym: 266; act: -127 ),
  ( sym: 267; act: -127 ),
  ( sym: 268; act: -127 ),
  ( sym: 269; act: -127 ),
  ( sym: 270; act: -127 ),
  ( sym: 271; act: -127 ),
  ( sym: 272; act: -127 ),
  ( sym: 273; act: -127 ),
  ( sym: 291; act: -127 ),
  ( sym: 301; act: -127 ),
  ( sym: 304; act: -127 ),
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
  ( sym: 316; act: -127 ),
  ( sym: 317; act: -127 ),
  ( sym: 318; act: -127 ),
  ( sym: 319; act: -127 ),
  ( sym: 320; act: -127 ),
  ( sym: 324; act: -127 ),
  ( sym: 325; act: -127 ),
{ 260: }
  ( sym: 321; act: 214 ),
  ( sym: 265; act: -128 ),
  ( sym: 266; act: -128 ),
  ( sym: 267; act: -128 ),
  ( sym: 268; act: -128 ),
  ( sym: 269; act: -128 ),
  ( sym: 270; act: -128 ),
  ( sym: 271; act: -128 ),
  ( sym: 272; act: -128 ),
  ( sym: 273; act: -128 ),
  ( sym: 291; act: -128 ),
  ( sym: 301; act: -128 ),
  ( sym: 304; act: -128 ),
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
  ( sym: 316; act: -128 ),
  ( sym: 317; act: -128 ),
  ( sym: 318; act: -128 ),
  ( sym: 319; act: -128 ),
  ( sym: 320; act: -128 ),
  ( sym: 324; act: -128 ),
  ( sym: 325; act: -128 ),
{ 261: }
  ( sym: 321; act: 214 ),
  ( sym: 265; act: -131 ),
  ( sym: 266; act: -131 ),
  ( sym: 267; act: -131 ),
  ( sym: 268; act: -131 ),
  ( sym: 269; act: -131 ),
  ( sym: 270; act: -131 ),
  ( sym: 271; act: -131 ),
  ( sym: 272; act: -131 ),
  ( sym: 273; act: -131 ),
  ( sym: 291; act: -131 ),
  ( sym: 301; act: -131 ),
  ( sym: 304; act: -131 ),
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
  ( sym: 316; act: -131 ),
  ( sym: 317; act: -131 ),
  ( sym: 318; act: -131 ),
  ( sym: 319; act: -131 ),
  ( sym: 320; act: -131 ),
  ( sym: 324; act: -131 ),
  ( sym: 325; act: -131 ),
{ 262: }
{ 263: }
  ( sym: 324; act: 144 ),
  ( sym: 325; act: 145 ),
  ( sym: 265; act: -137 ),
  ( sym: 266; act: -137 ),
  ( sym: 267; act: -137 ),
  ( sym: 268; act: -137 ),
  ( sym: 269; act: -137 ),
  ( sym: 270; act: -137 ),
  ( sym: 271; act: -137 ),
  ( sym: 272; act: -137 ),
  ( sym: 273; act: -137 ),
  ( sym: 291; act: -137 ),
  ( sym: 301; act: -137 ),
  ( sym: 304; act: -137 ),
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
  ( sym: 316; act: -137 ),
  ( sym: 317; act: -137 ),
  ( sym: 318; act: -137 ),
  ( sym: 319; act: -137 ),
  ( sym: 320; act: -137 ),
  ( sym: 321; act: -137 ),
{ 264: }
  ( sym: 292; act: 234 ),
  ( sym: 268; act: -3 ),
{ 265: }
{ 266: }
  ( sym: 268; act: 222 ),
  ( sym: 277; act: 23 ),
  ( sym: 287; act: 223 ),
  ( sym: 288; act: 83 ),
  ( sym: 289; act: 84 ),
  ( sym: 290; act: 85 ),
  ( sym: 314; act: 86 ),
  ( sym: 319; act: 273 ),
  ( sym: 267; act: -116 ),
  ( sym: 269; act: -116 ),
  ( sym: 270; act: -116 ),
{ 267: }
{ 268: }
  ( sym: 269; act: 163 ),
  ( sym: 274; act: 20 ),
  ( sym: 275; act: 21 ),
  ( sym: 276; act: 22 ),
  ( sym: 277; act: 23 ),
  ( sym: 280; act: 24 ),
  ( sym: 281; act: 25 ),
  ( sym: 282; act: 26 ),
  ( sym: 283; act: 27 ),
  ( sym: 284; act: 28 ),
  ( sym: 285; act: 29 ),
  ( sym: 286; act: 164 ),
  ( sym: 287; act: 31 ),
  ( sym: 303; act: 165 ),
  ( sym: 327; act: 32 ),
{ 269: }
  ( sym: 268; act: 107 ),
  ( sym: 277; act: 23 ),
  ( sym: 278; act: 108 ),
  ( sym: 279; act: 109 ),
  ( sym: 280; act: 24 ),
  ( sym: 281; act: 25 ),
  ( sym: 282; act: 26 ),
  ( sym: 283; act: 27 ),
  ( sym: 284; act: 28 ),
  ( sym: 285; act: 29 ),
  ( sym: 286; act: 30 ),
  ( sym: 314; act: 111 ),
  ( sym: 316; act: 112 ),
  ( sym: 321; act: 113 ),
  ( sym: 327; act: 32 ),
{ 270: }
  ( sym: 268; act: 107 ),
  ( sym: 271; act: 291 ),
  ( sym: 277; act: 23 ),
  ( sym: 278; act: 108 ),
  ( sym: 279; act: 109 ),
  ( sym: 280; act: 24 ),
  ( sym: 281; act: 25 ),
  ( sym: 282; act: 26 ),
  ( sym: 283; act: 27 ),
  ( sym: 284; act: 28 ),
  ( sym: 285; act: 29 ),
  ( sym: 286; act: 30 ),
  ( sym: 314; act: 111 ),
  ( sym: 316; act: 112 ),
  ( sym: 321; act: 113 ),
  ( sym: 327; act: 32 ),
{ 271: }
  ( sym: 268; act: 268 ),
  ( sym: 269; act: 292 ),
  ( sym: 270; act: 269 ),
{ 272: }
  ( sym: 268; act: 118 ),
  ( sym: 269; act: 180 ),
  ( sym: 270; act: 270 ),
{ 273: }
  ( sym: 268; act: 222 ),
  ( sym: 277; act: 23 ),
  ( sym: 287; act: 223 ),
  ( sym: 288; act: 83 ),
  ( sym: 289; act: 84 ),
  ( sym: 290; act: 85 ),
  ( sym: 314; act: 86 ),
  ( sym: 319; act: 273 ),
  ( sym: 267; act: -116 ),
  ( sym: 269; act: -116 ),
  ( sym: 270; act: -116 ),
{ 274: }
  ( sym: 268; act: 268 ),
  ( sym: 270; act: 269 ),
  ( sym: 267; act: -108 ),
  ( sym: 269; act: -108 ),
{ 275: }
  ( sym: 268; act: 118 ),
  ( sym: 270; act: 270 ),
  ( sym: 267; act: -94 ),
  ( sym: 269; act: -94 ),
{ 276: }
  ( sym: 270; act: 269 ),
  ( sym: 267; act: -110 ),
  ( sym: 268; act: -110 ),
  ( sym: 269; act: -110 ),
{ 277: }
  ( sym: 268; act: 118 ),
  ( sym: 270; act: 270 ),
  ( sym: 267; act: -85 ),
  ( sym: 269; act: -85 ),
{ 278: }
  ( sym: 269; act: 294 ),
  ( sym: 304; act: 198 ),
  ( sym: 306; act: 199 ),
  ( sym: 307; act: 200 ),
  ( sym: 308; act: 201 ),
  ( sym: 309; act: 202 ),
  ( sym: 310; act: 203 ),
  ( sym: 311; act: 204 ),
  ( sym: 312; act: 205 ),
  ( sym: 313; act: 206 ),
  ( sym: 314; act: 207 ),
  ( sym: 315; act: 208 ),
  ( sym: 316; act: 209 ),
  ( sym: 317; act: 210 ),
  ( sym: 318; act: 211 ),
  ( sym: 319; act: 212 ),
  ( sym: 320; act: 213 ),
  ( sym: 321; act: 214 ),
{ 279: }
{ 280: }
  ( sym: 274; act: 20 ),
  ( sym: 275; act: 21 ),
  ( sym: 276; act: 22 ),
  ( sym: 277; act: 23 ),
  ( sym: 280; act: 24 ),
  ( sym: 281; act: 25 ),
  ( sym: 282; act: 26 ),
  ( sym: 283; act: 27 ),
  ( sym: 284; act: 28 ),
  ( sym: 285; act: 29 ),
  ( sym: 286; act: 30 ),
  ( sym: 287; act: 31 ),
  ( sym: 303; act: 165 ),
  ( sym: 327; act: 32 ),
  ( sym: 269; act: -90 ),
{ 281: }
  ( sym: 269; act: 296 ),
{ 282: }
{ 283: }
  ( sym: 268; act: 107 ),
  ( sym: 277; act: 23 ),
  ( sym: 278; act: 108 ),
  ( sym: 279; act: 109 ),
  ( sym: 280; act: 24 ),
  ( sym: 281; act: 25 ),
  ( sym: 282; act: 26 ),
  ( sym: 283; act: 27 ),
  ( sym: 284; act: 28 ),
  ( sym: 285; act: 29 ),
  ( sym: 286; act: 30 ),
  ( sym: 314; act: 111 ),
  ( sym: 316; act: 112 ),
  ( sym: 321; act: 113 ),
  ( sym: 327; act: 32 ),
{ 284: }
  ( sym: 324; act: 144 ),
  ( sym: 325; act: 145 ),
  ( sym: 265; act: -150 ),
  ( sym: 266; act: -150 ),
  ( sym: 267; act: -150 ),
  ( sym: 268; act: -150 ),
  ( sym: 269; act: -150 ),
  ( sym: 270; act: -150 ),
  ( sym: 271; act: -150 ),
  ( sym: 272; act: -150 ),
  ( sym: 273; act: -150 ),
  ( sym: 291; act: -150 ),
  ( sym: 301; act: -150 ),
  ( sym: 304; act: -150 ),
  ( sym: 306; act: -150 ),
  ( sym: 307; act: -150 ),
  ( sym: 308; act: -150 ),
  ( sym: 309; act: -150 ),
  ( sym: 310; act: -150 ),
  ( sym: 311; act: -150 ),
  ( sym: 312; act: -150 ),
  ( sym: 313; act: -150 ),
  ( sym: 314; act: -150 ),
  ( sym: 315; act: -150 ),
  ( sym: 316; act: -150 ),
  ( sym: 317; act: -150 ),
  ( sym: 318; act: -150 ),
  ( sym: 319; act: -150 ),
  ( sym: 320; act: -150 ),
  ( sym: 321; act: -150 ),
{ 285: }
  ( sym: 268; act: 107 ),
  ( sym: 277; act: 23 ),
  ( sym: 278; act: 108 ),
  ( sym: 279; act: 109 ),
  ( sym: 280; act: 24 ),
  ( sym: 281; act: 25 ),
  ( sym: 282; act: 26 ),
  ( sym: 283; act: 27 ),
  ( sym: 284; act: 28 ),
  ( sym: 285; act: 29 ),
  ( sym: 286; act: 30 ),
  ( sym: 314; act: 111 ),
  ( sym: 316; act: 112 ),
  ( sym: 321; act: 113 ),
  ( sym: 327; act: 32 ),
{ 286: }
  ( sym: 268; act: 299 ),
{ 287: }
  ( sym: 268; act: 268 ),
  ( sym: 270; act: 269 ),
  ( sym: 267; act: -109 ),
  ( sym: 269; act: -109 ),
{ 288: }
  ( sym: 268; act: 118 ),
  ( sym: 270; act: 270 ),
  ( sym: 267; act: -95 ),
  ( sym: 269; act: -95 ),
{ 289: }
  ( sym: 269; act: 300 ),
{ 290: }
  ( sym: 271; act: 301 ),
  ( sym: 304; act: 198 ),
  ( sym: 306; act: 199 ),
  ( sym: 307; act: 200 ),
  ( sym: 308; act: 201 ),
  ( sym: 309; act: 202 ),
  ( sym: 310; act: 203 ),
  ( sym: 311; act: 204 ),
  ( sym: 312; act: 205 ),
  ( sym: 313; act: 206 ),
  ( sym: 314; act: 207 ),
  ( sym: 315; act: 208 ),
  ( sym: 316; act: 209 ),
  ( sym: 317; act: 210 ),
  ( sym: 318; act: 211 ),
  ( sym: 319; act: 212 ),
  ( sym: 320; act: 213 ),
  ( sym: 321; act: 214 ),
{ 291: }
{ 292: }
{ 293: }
  ( sym: 268; act: 118 ),
  ( sym: 270; act: 270 ),
  ( sym: 267; act: -96 ),
  ( sym: 269; act: -96 ),
{ 294: }
  ( sym: 257; act: 174 ),
  ( sym: 266; act: 175 ),
  ( sym: 268; act: 107 ),
  ( sym: 277; act: 23 ),
  ( sym: 278; act: 108 ),
  ( sym: 279; act: 109 ),
  ( sym: 280; act: 24 ),
  ( sym: 281; act: 25 ),
  ( sym: 282; act: 26 ),
  ( sym: 283; act: 27 ),
  ( sym: 284; act: 28 ),
  ( sym: 285; act: 29 ),
  ( sym: 286; act: 30 ),
  ( sym: 314; act: 111 ),
  ( sym: 316; act: 112 ),
  ( sym: 321; act: 113 ),
  ( sym: 327; act: 32 ),
  ( sym: 273; act: -26 ),
{ 295: }
  ( sym: 269; act: 303 ),
{ 296: }
{ 297: }
  ( sym: 324; act: 144 ),
  ( sym: 325; act: 145 ),
  ( sym: 265; act: -151 ),
  ( sym: 266; act: -151 ),
  ( sym: 267; act: -151 ),
  ( sym: 268; act: -151 ),
  ( sym: 269; act: -151 ),
  ( sym: 270; act: -151 ),
  ( sym: 271; act: -151 ),
  ( sym: 272; act: -151 ),
  ( sym: 273; act: -151 ),
  ( sym: 291; act: -151 ),
  ( sym: 301; act: -151 ),
  ( sym: 304; act: -151 ),
  ( sym: 306; act: -151 ),
  ( sym: 307; act: -151 ),
  ( sym: 308; act: -151 ),
  ( sym: 309; act: -151 ),
  ( sym: 310; act: -151 ),
  ( sym: 311; act: -151 ),
  ( sym: 312; act: -151 ),
  ( sym: 313; act: -151 ),
  ( sym: 314; act: -151 ),
  ( sym: 315; act: -151 ),
  ( sym: 316; act: -151 ),
  ( sym: 317; act: -151 ),
  ( sym: 318; act: -151 ),
  ( sym: 319; act: -151 ),
  ( sym: 320; act: -151 ),
  ( sym: 321; act: -151 ),
{ 298: }
  ( sym: 313; act: 206 ),
  ( sym: 314; act: 207 ),
  ( sym: 315; act: 208 ),
  ( sym: 316; act: 209 ),
  ( sym: 317; act: 210 ),
  ( sym: 318; act: 211 ),
  ( sym: 319; act: 212 ),
  ( sym: 320; act: 213 ),
  ( sym: 321; act: 214 ),
  ( sym: 265; act: -136 ),
  ( sym: 266; act: -136 ),
  ( sym: 267; act: -136 ),
  ( sym: 268; act: -136 ),
  ( sym: 269; act: -136 ),
  ( sym: 270; act: -136 ),
  ( sym: 271; act: -136 ),
  ( sym: 272; act: -136 ),
  ( sym: 273; act: -136 ),
  ( sym: 291; act: -136 ),
  ( sym: 301; act: -136 ),
  ( sym: 304; act: -136 ),
  ( sym: 306; act: -136 ),
  ( sym: 307; act: -136 ),
  ( sym: 308; act: -136 ),
  ( sym: 309; act: -136 ),
  ( sym: 310; act: -136 ),
  ( sym: 311; act: -136 ),
  ( sym: 312; act: -136 ),
  ( sym: 324; act: -136 ),
  ( sym: 325; act: -136 ),
{ 299: }
  ( sym: 268; act: 107 ),
  ( sym: 277; act: 23 ),
  ( sym: 278; act: 108 ),
  ( sym: 279; act: 109 ),
  ( sym: 280; act: 24 ),
  ( sym: 281; act: 25 ),
  ( sym: 282; act: 26 ),
  ( sym: 283; act: 27 ),
  ( sym: 284; act: 28 ),
  ( sym: 285; act: 29 ),
  ( sym: 286; act: 30 ),
  ( sym: 314; act: 111 ),
  ( sym: 316; act: 112 ),
  ( sym: 321; act: 113 ),
  ( sym: 327; act: 32 ),
  ( sym: 269; act: -166 ),
{ 300: }
{ 301: }
{ 302: }
{ 303: }
  ( sym: 266; act: 305 ),
{ 304: }
  ( sym: 269; act: 306 )
{ 305: }
{ 306: }
);

yyg : array [1..yyngotos] of YYARec = (
{ 0: }
  ( sym: -18; act: 1 ),
  ( sym: -8; act: 2 ),
  ( sym: -7; act: 3 ),
  ( sym: -6; act: 4 ),
  ( sym: -3; act: 5 ),
  ( sym: -2; act: 6 ),
{ 1: }
{ 2: }
  ( sym: -29; act: 15 ),
  ( sym: -27; act: 16 ),
  ( sym: -18; act: 17 ),
  ( sym: -16; act: 18 ),
  ( sym: -11; act: 19 ),
{ 3: }
{ 4: }
{ 5: }
  ( sym: -18; act: 1 ),
  ( sym: -8; act: 2 ),
  ( sym: -7; act: 33 ),
  ( sym: -6; act: 34 ),
{ 6: }
{ 7: }
  ( sym: -5; act: 35 ),
{ 8: }
  ( sym: -29; act: 15 ),
  ( sym: -27; act: 16 ),
  ( sym: -18; act: 17 ),
  ( sym: -16; act: 36 ),
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
  ( sym: -24; act: 51 ),
  ( sym: -11; act: 40 ),
{ 21: }
  ( sym: -24; act: 54 ),
  ( sym: -11; act: 41 ),
{ 22: }
  ( sym: -26; act: 55 ),
  ( sym: -11; act: 42 ),
{ 23: }
{ 24: }
{ 25: }
  ( sym: -29; act: 59 ),
{ 26: }
{ 27: }
{ 28: }
{ 29: }
{ 30: }
{ 31: }
  ( sym: -29; act: 15 ),
  ( sym: -27; act: 16 ),
  ( sym: -18; act: 17 ),
  ( sym: -16; act: 62 ),
  ( sym: -11; act: 19 ),
{ 32: }
  ( sym: -29; act: 63 ),
{ 33: }
{ 34: }
{ 35: }
{ 36: }
  ( sym: -9; act: 66 ),
{ 37: }
{ 38: }
  ( sym: -24; act: 51 ),
  ( sym: -11; act: 69 ),
{ 39: }
{ 40: }
  ( sym: -24; act: 73 ),
{ 41: }
  ( sym: -24; act: 74 ),
{ 42: }
  ( sym: -26; act: 75 ),
{ 43: }
  ( sym: -32; act: 76 ),
  ( sym: -19; act: 77 ),
  ( sym: -17; act: 78 ),
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
  ( sym: -29; act: 15 ),
  ( sym: -28; act: 90 ),
  ( sym: -27; act: 16 ),
  ( sym: -25; act: 91 ),
  ( sym: -18; act: 17 ),
  ( sym: -16; act: 92 ),
  ( sym: -11; act: 19 ),
{ 54: }
{ 55: }
{ 56: }
  ( sym: -5; act: 94 ),
{ 57: }
  ( sym: -40; act: 95 ),
  ( sym: -21; act: 96 ),
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
  ( sym: -32; act: 76 ),
  ( sym: -19; act: 77 ),
  ( sym: -17; act: 99 ),
  ( sym: -11; act: 79 ),
{ 67: }
  ( sym: -9; act: 100 ),
{ 68: }
{ 69: }
  ( sym: -24; act: 73 ),
  ( sym: -11; act: 101 ),
{ 70: }
  ( sym: -40; act: 95 ),
  ( sym: -21; act: 102 ),
  ( sym: -11; act: 97 ),
{ 71: }
{ 72: }
  ( sym: -37; act: 103 ),
  ( sym: -29; act: 104 ),
  ( sym: -23; act: 105 ),
  ( sym: -11; act: 106 ),
{ 73: }
{ 74: }
{ 75: }
{ 76: }
{ 77: }
  ( sym: -34; act: 117 ),
{ 78: }
  ( sym: -15; act: 120 ),
  ( sym: -10; act: 121 ),
{ 79: }
  ( sym: -33; act: 125 ),
{ 80: }
  ( sym: -5; act: 127 ),
{ 81: }
  ( sym: -32; act: 76 ),
  ( sym: -19; act: 128 ),
  ( sym: -11; act: 79 ),
{ 82: }
  ( sym: -32; act: 76 ),
  ( sym: -19; act: 129 ),
  ( sym: -11; act: 79 ),
{ 83: }
{ 84: }
{ 85: }
{ 86: }
  ( sym: -32; act: 76 ),
  ( sym: -19; act: 130 ),
  ( sym: -11; act: 79 ),
{ 87: }
  ( sym: -32; act: 76 ),
  ( sym: -19; act: 131 ),
  ( sym: -11; act: 79 ),
{ 88: }
{ 89: }
{ 90: }
  ( sym: -29; act: 15 ),
  ( sym: -28; act: 90 ),
  ( sym: -27; act: 16 ),
  ( sym: -25; act: 133 ),
  ( sym: -18; act: 17 ),
  ( sym: -16; act: 92 ),
  ( sym: -11; act: 19 ),
{ 91: }
{ 92: }
  ( sym: -32; act: 76 ),
  ( sym: -19; act: 77 ),
  ( sym: -17; act: 135 ),
  ( sym: -11; act: 79 ),
{ 93: }
{ 94: }
{ 95: }
{ 96: }
{ 97: }
{ 98: }
{ 99: }
{ 100: }
  ( sym: -32; act: 76 ),
  ( sym: -19; act: 141 ),
  ( sym: -11; act: 79 ),
{ 101: }
{ 102: }
{ 103: }
{ 104: }
{ 105: }
{ 106: }
{ 107: }
  ( sym: -37; act: 149 ),
  ( sym: -35; act: 150 ),
  ( sym: -29; act: 151 ),
  ( sym: -27; act: 16 ),
  ( sym: -18; act: 17 ),
  ( sym: -16; act: 152 ),
  ( sym: -13; act: 153 ),
  ( sym: -11; act: 154 ),
{ 108: }
{ 109: }
{ 110: }
{ 111: }
  ( sym: -37; act: 156 ),
  ( sym: -29; act: 104 ),
  ( sym: -11; act: 106 ),
{ 112: }
  ( sym: -37; act: 157 ),
  ( sym: -29; act: 104 ),
  ( sym: -11; act: 106 ),
{ 113: }
  ( sym: -37; act: 158 ),
  ( sym: -29; act: 104 ),
  ( sym: -11; act: 106 ),
{ 114: }
{ 115: }
{ 116: }
  ( sym: -32; act: 76 ),
  ( sym: -19; act: 159 ),
  ( sym: -11; act: 79 ),
{ 117: }
{ 118: }
  ( sym: -30; act: 160 ),
  ( sym: -29; act: 15 ),
  ( sym: -27; act: 16 ),
  ( sym: -20; act: 161 ),
  ( sym: -18; act: 17 ),
  ( sym: -16; act: 162 ),
  ( sym: -11; act: 19 ),
{ 119: }
  ( sym: -37; act: 149 ),
  ( sym: -35; act: 166 ),
  ( sym: -29; act: 104 ),
  ( sym: -13; act: 167 ),
  ( sym: -11; act: 106 ),
{ 120: }
{ 121: }
{ 122: }
  ( sym: -32; act: 76 ),
  ( sym: -19; act: 170 ),
  ( sym: -11; act: 79 ),
{ 123: }
  ( sym: -37; act: 149 ),
  ( sym: -35; act: 166 ),
  ( sym: -29; act: 104 ),
  ( sym: -14; act: 171 ),
  ( sym: -13; act: 172 ),
  ( sym: -12; act: 173 ),
  ( sym: -11; act: 106 ),
{ 124: }
{ 125: }
  ( sym: -37; act: 149 ),
  ( sym: -35; act: 166 ),
  ( sym: -29; act: 104 ),
  ( sym: -13; act: 177 ),
  ( sym: -11; act: 106 ),
{ 126: }
  ( sym: -37; act: 149 ),
  ( sym: -35; act: 166 ),
  ( sym: -29; act: 104 ),
  ( sym: -13; act: 178 ),
  ( sym: -11; act: 106 ),
{ 127: }
{ 128: }
  ( sym: -34; act: 117 ),
{ 129: }
  ( sym: -34; act: 117 ),
{ 130: }
  ( sym: -34; act: 117 ),
{ 131: }
  ( sym: -34; act: 117 ),
{ 132: }
{ 133: }
{ 134: }
{ 135: }
{ 136: }
{ 137: }
  ( sym: -40; act: 95 ),
  ( sym: -21; act: 182 ),
  ( sym: -11; act: 97 ),
{ 138: }
{ 139: }
  ( sym: -37; act: 149 ),
  ( sym: -35; act: 166 ),
  ( sym: -29; act: 104 ),
  ( sym: -13; act: 183 ),
  ( sym: -11; act: 106 ),
{ 140: }
{ 141: }
  ( sym: -34; act: 117 ),
{ 142: }
{ 143: }
  ( sym: -22; act: 185 ),
  ( sym: -4; act: 186 ),
{ 144: }
  ( sym: -37; act: 149 ),
  ( sym: -35; act: 166 ),
  ( sym: -29; act: 104 ),
  ( sym: -13; act: 188 ),
  ( sym: -11; act: 106 ),
{ 145: }
  ( sym: -37; act: 149 ),
  ( sym: -35; act: 166 ),
  ( sym: -29; act: 104 ),
  ( sym: -13; act: 189 ),
  ( sym: -11; act: 106 ),
{ 146: }
{ 147: }
  ( sym: -41; act: 190 ),
  ( sym: -39; act: 191 ),
  ( sym: -37; act: 149 ),
  ( sym: -35; act: 166 ),
  ( sym: -29; act: 104 ),
  ( sym: -13; act: 192 ),
  ( sym: -11; act: 106 ),
{ 148: }
  ( sym: -41; act: 190 ),
  ( sym: -39; act: 193 ),
  ( sym: -37; act: 149 ),
  ( sym: -35; act: 166 ),
  ( sym: -29; act: 104 ),
  ( sym: -13; act: 192 ),
  ( sym: -11; act: 106 ),
{ 149: }
{ 150: }
{ 151: }
{ 152: }
  ( sym: -32; act: 195 ),
{ 153: }
{ 154: }
{ 155: }
  ( sym: -37; act: 216 ),
  ( sym: -29; act: 104 ),
  ( sym: -11; act: 106 ),
{ 156: }
{ 157: }
{ 158: }
{ 159: }
  ( sym: -34; act: 117 ),
{ 160: }
{ 161: }
{ 162: }
  ( sym: -32; act: 219 ),
  ( sym: -31; act: 220 ),
  ( sym: -19; act: 221 ),
  ( sym: -11; act: 79 ),
{ 163: }
{ 164: }
{ 165: }
{ 166: }
{ 167: }
{ 168: }
{ 169: }
{ 170: }
  ( sym: -34; act: 117 ),
{ 171: }
{ 172: }
{ 173: }
  ( sym: -37; act: 149 ),
  ( sym: -35; act: 166 ),
  ( sym: -29; act: 104 ),
  ( sym: -14; act: 229 ),
  ( sym: -13; act: 172 ),
  ( sym: -12; act: 173 ),
  ( sym: -11; act: 106 ),
{ 174: }
{ 175: }
{ 176: }
  ( sym: -11; act: 231 ),
{ 177: }
{ 178: }
{ 179: }
  ( sym: -32; act: 76 ),
  ( sym: -19; act: 77 ),
  ( sym: -17; act: 232 ),
  ( sym: -11; act: 79 ),
{ 180: }
{ 181: }
{ 182: }
{ 183: }
{ 184: }
  ( sym: -4; act: 233 ),
{ 185: }
{ 186: }
{ 187: }
  ( sym: -37; act: 103 ),
  ( sym: -29; act: 104 ),
  ( sym: -23; act: 237 ),
  ( sym: -11; act: 106 ),
{ 188: }
{ 189: }
{ 190: }
{ 191: }
{ 192: }
{ 193: }
{ 194: }
{ 195: }
{ 196: }
  ( sym: -37; act: 242 ),
  ( sym: -29; act: 104 ),
  ( sym: -11; act: 106 ),
{ 197: }
{ 198: }
  ( sym: -37; act: 149 ),
  ( sym: -35; act: 166 ),
  ( sym: -29; act: 104 ),
  ( sym: -13; act: 244 ),
  ( sym: -11; act: 106 ),
{ 199: }
  ( sym: -37; act: 149 ),
  ( sym: -35; act: 166 ),
  ( sym: -29; act: 104 ),
  ( sym: -13; act: 245 ),
  ( sym: -11; act: 106 ),
{ 200: }
  ( sym: -37; act: 149 ),
  ( sym: -35; act: 166 ),
  ( sym: -29; act: 104 ),
  ( sym: -13; act: 246 ),
  ( sym: -11; act: 106 ),
{ 201: }
  ( sym: -37; act: 149 ),
  ( sym: -35; act: 166 ),
  ( sym: -29; act: 104 ),
  ( sym: -13; act: 247 ),
  ( sym: -11; act: 106 ),
{ 202: }
  ( sym: -37; act: 149 ),
  ( sym: -35; act: 166 ),
  ( sym: -29; act: 104 ),
  ( sym: -13; act: 248 ),
  ( sym: -11; act: 106 ),
{ 203: }
  ( sym: -37; act: 149 ),
  ( sym: -35; act: 166 ),
  ( sym: -29; act: 104 ),
  ( sym: -13; act: 249 ),
  ( sym: -11; act: 106 ),
{ 204: }
  ( sym: -37; act: 149 ),
  ( sym: -35; act: 166 ),
  ( sym: -29; act: 104 ),
  ( sym: -13; act: 250 ),
  ( sym: -11; act: 106 ),
{ 205: }
  ( sym: -37; act: 149 ),
  ( sym: -36; act: 251 ),
  ( sym: -35; act: 166 ),
  ( sym: -29; act: 104 ),
  ( sym: -13; act: 252 ),
  ( sym: -11; act: 106 ),
{ 206: }
  ( sym: -37; act: 149 ),
  ( sym: -35; act: 166 ),
  ( sym: -29; act: 104 ),
  ( sym: -13; act: 253 ),
  ( sym: -11; act: 106 ),
{ 207: }
  ( sym: -37; act: 149 ),
  ( sym: -35; act: 166 ),
  ( sym: -29; act: 104 ),
  ( sym: -13; act: 254 ),
  ( sym: -11; act: 106 ),
{ 208: }
  ( sym: -37; act: 149 ),
  ( sym: -35; act: 166 ),
  ( sym: -29; act: 104 ),
  ( sym: -13; act: 255 ),
  ( sym: -11; act: 106 ),
{ 209: }
  ( sym: -37; act: 149 ),
  ( sym: -35; act: 166 ),
  ( sym: -29; act: 104 ),
  ( sym: -13; act: 256 ),
  ( sym: -11; act: 106 ),
{ 210: }
  ( sym: -37; act: 149 ),
  ( sym: -35; act: 166 ),
  ( sym: -29; act: 104 ),
  ( sym: -13; act: 257 ),
  ( sym: -11; act: 106 ),
{ 211: }
  ( sym: -37; act: 149 ),
  ( sym: -35; act: 166 ),
  ( sym: -29; act: 104 ),
  ( sym: -13; act: 258 ),
  ( sym: -11; act: 106 ),
{ 212: }
  ( sym: -37; act: 149 ),
  ( sym: -35; act: 166 ),
  ( sym: -29; act: 104 ),
  ( sym: -13; act: 259 ),
  ( sym: -11; act: 106 ),
{ 213: }
  ( sym: -37; act: 149 ),
  ( sym: -35; act: 166 ),
  ( sym: -29; act: 104 ),
  ( sym: -13; act: 260 ),
  ( sym: -11; act: 106 ),
{ 214: }
  ( sym: -37; act: 149 ),
  ( sym: -35; act: 166 ),
  ( sym: -29; act: 104 ),
  ( sym: -13; act: 261 ),
  ( sym: -11; act: 106 ),
{ 215: }
  ( sym: -38; act: 262 ),
  ( sym: -37; act: 263 ),
  ( sym: -29; act: 104 ),
  ( sym: -11; act: 106 ),
{ 216: }
{ 217: }
  ( sym: -30; act: 160 ),
  ( sym: -29; act: 15 ),
  ( sym: -27; act: 16 ),
  ( sym: -20; act: 265 ),
  ( sym: -18; act: 17 ),
  ( sym: -16; act: 162 ),
  ( sym: -11; act: 19 ),
{ 218: }
{ 219: }
{ 220: }
  ( sym: -34; act: 267 ),
{ 221: }
  ( sym: -34; act: 117 ),
{ 222: }
  ( sym: -32; act: 219 ),
  ( sym: -31; act: 271 ),
  ( sym: -19; act: 272 ),
  ( sym: -11; act: 79 ),
{ 223: }
  ( sym: -32; act: 219 ),
  ( sym: -31; act: 274 ),
  ( sym: -19; act: 275 ),
  ( sym: -11; act: 79 ),
{ 224: }
  ( sym: -32; act: 219 ),
  ( sym: -31; act: 276 ),
  ( sym: -19; act: 277 ),
  ( sym: -11; act: 79 ),
{ 225: }
{ 226: }
{ 227: }
{ 228: }
{ 229: }
{ 230: }
  ( sym: -37; act: 149 ),
  ( sym: -35; act: 166 ),
  ( sym: -29; act: 104 ),
  ( sym: -13; act: 278 ),
  ( sym: -11; act: 106 ),
{ 231: }
{ 232: }
{ 233: }
{ 234: }
{ 235: }
{ 236: }
  ( sym: -37; act: 103 ),
  ( sym: -29; act: 104 ),
  ( sym: -23; act: 281 ),
  ( sym: -11; act: 106 ),
{ 237: }
{ 238: }
  ( sym: -41; act: 190 ),
  ( sym: -39; act: 282 ),
  ( sym: -37; act: 149 ),
  ( sym: -35; act: 166 ),
  ( sym: -29; act: 104 ),
  ( sym: -13; act: 192 ),
  ( sym: -11; act: 106 ),
{ 239: }
{ 240: }
{ 241: }
{ 242: }
{ 243: }
  ( sym: -37; act: 284 ),
  ( sym: -29; act: 104 ),
  ( sym: -11; act: 106 ),
{ 244: }
{ 245: }
{ 246: }
{ 247: }
{ 248: }
{ 249: }
{ 250: }
{ 251: }
{ 252: }
{ 253: }
{ 254: }
{ 255: }
{ 256: }
{ 257: }
{ 258: }
{ 259: }
{ 260: }
{ 261: }
{ 262: }
{ 263: }
{ 264: }
  ( sym: -4; act: 286 ),
{ 265: }
{ 266: }
  ( sym: -32; act: 219 ),
  ( sym: -31; act: 287 ),
  ( sym: -19; act: 288 ),
  ( sym: -11; act: 79 ),
{ 267: }
{ 268: }
  ( sym: -30; act: 160 ),
  ( sym: -29; act: 15 ),
  ( sym: -27; act: 16 ),
  ( sym: -20; act: 289 ),
  ( sym: -18; act: 17 ),
  ( sym: -16; act: 162 ),
  ( sym: -11; act: 19 ),
{ 269: }
  ( sym: -37; act: 149 ),
  ( sym: -35; act: 166 ),
  ( sym: -29; act: 104 ),
  ( sym: -13; act: 290 ),
  ( sym: -11; act: 106 ),
{ 270: }
  ( sym: -37; act: 149 ),
  ( sym: -35; act: 166 ),
  ( sym: -29; act: 104 ),
  ( sym: -13; act: 167 ),
  ( sym: -11; act: 106 ),
{ 271: }
  ( sym: -34; act: 267 ),
{ 272: }
  ( sym: -34; act: 117 ),
{ 273: }
  ( sym: -32; act: 219 ),
  ( sym: -31; act: 276 ),
  ( sym: -19; act: 293 ),
  ( sym: -11; act: 79 ),
{ 274: }
  ( sym: -34; act: 267 ),
{ 275: }
  ( sym: -34; act: 117 ),
{ 276: }
  ( sym: -34; act: 267 ),
{ 277: }
  ( sym: -34; act: 117 ),
{ 278: }
{ 279: }
{ 280: }
  ( sym: -30; act: 160 ),
  ( sym: -29; act: 15 ),
  ( sym: -27; act: 16 ),
  ( sym: -20; act: 295 ),
  ( sym: -18; act: 17 ),
  ( sym: -16; act: 162 ),
  ( sym: -11; act: 19 ),
{ 281: }
{ 282: }
{ 283: }
  ( sym: -37; act: 297 ),
  ( sym: -29; act: 104 ),
  ( sym: -11; act: 106 ),
{ 284: }
{ 285: }
  ( sym: -37; act: 149 ),
  ( sym: -35; act: 166 ),
  ( sym: -29; act: 104 ),
  ( sym: -13; act: 298 ),
  ( sym: -11; act: 106 ),
{ 286: }
{ 287: }
  ( sym: -34; act: 267 ),
{ 288: }
  ( sym: -34; act: 117 ),
{ 289: }
{ 290: }
{ 291: }
{ 292: }
{ 293: }
  ( sym: -34; act: 117 ),
{ 294: }
  ( sym: -37; act: 149 ),
  ( sym: -35; act: 166 ),
  ( sym: -29; act: 104 ),
  ( sym: -14; act: 302 ),
  ( sym: -13; act: 172 ),
  ( sym: -12; act: 173 ),
  ( sym: -11; act: 106 ),
{ 295: }
{ 296: }
{ 297: }
{ 298: }
{ 299: }
  ( sym: -41; act: 190 ),
  ( sym: -39; act: 304 ),
  ( sym: -37; act: 149 ),
  ( sym: -35; act: 166 ),
  ( sym: -29; act: 104 ),
  ( sym: -13; act: 192 ),
  ( sym: -11; act: 106 )
{ 300: }
{ 301: }
{ 302: }
{ 303: }
{ 304: }
{ 305: }
{ 306: }
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
{ 14: } -30,
{ 15: } -78,
{ 16: } -60,
{ 17: } -59,
{ 18: } 0,
{ 19: } -79,
{ 20: } 0,
{ 21: } 0,
{ 22: } 0,
{ 23: } -64,
{ 24: } 0,
{ 25: } 0,
{ 26: } 0,
{ 27: } -67,
{ 28: } -74,
{ 29: } -76,
{ 30: } -75,
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
{ 55: } -58,
{ 56: } -4,
{ 57: } 0,
{ 58: } -73,
{ 59: } -66,
{ 60: } 0,
{ 61: } -69,
{ 62: } -53,
{ 63: } -65,
{ 64: } -35,
{ 65: } -40,
{ 66: } 0,
{ 67: } 0,
{ 68: } -34,
{ 69: } 0,
{ 70: } 0,
{ 71: } -38,
{ 72: } 0,
{ 73: } 0,
{ 74: } 0,
{ 75: } -51,
{ 76: } 0,
{ 77: } 0,
{ 78: } 0,
{ 79: } 0,
{ 80: } -4,
{ 81: } 0,
{ 82: } 0,
{ 83: } -91,
{ 84: } -93,
{ 85: } -92,
{ 86: } 0,
{ 87: } 0,
{ 88: } -56,
{ 89: } 0,
{ 90: } 0,
{ 91: } 0,
{ 92: } 0,
{ 93: } -54,
{ 94: } 0,
{ 95: } 0,
{ 96: } 0,
{ 97: } 0,
{ 98: } -71,
{ 99: } 0,
{ 100: } 0,
{ 101: } 0,
{ 102: } 0,
{ 103: } 0,
{ 104: } -140,
{ 105: } 0,
{ 106: } 0,
{ 107: } 0,
{ 108: } -142,
{ 109: } -141,
{ 110: } -37,
{ 111: } 0,
{ 112: } 0,
{ 113: } 0,
{ 114: } -45,
{ 115: } -47,
{ 116: } 0,
{ 117: } -102,
{ 118: } 0,
{ 119: } 0,
{ 120: } -28,
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
{ 131: } 0,
{ 132: } -42,
{ 133: } -61,
{ 134: } -41,
{ 135: } 0,
{ 136: } -44,
{ 137: } 0,
{ 138: } -43,
{ 139: } 0,
{ 140: } -33,
{ 141: } 0,
{ 142: } -31,
{ 143: } 0,
{ 144: } 0,
{ 145: } 0,
{ 146: } -39,
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
{ 161: } 0,
{ 162: } 0,
{ 163: } -106,
{ 164: } 0,
{ 165: } -89,
{ 166: } -117,
{ 167: } 0,
{ 168: } -104,
{ 169: } -29,
{ 170: } 0,
{ 171: } 0,
{ 172: } 0,
{ 173: } 0,
{ 174: } 0,
{ 175: } -25,
{ 176: } 0,
{ 177: } 0,
{ 178: } 0,
{ 179: } 0,
{ 180: } -105,
{ 181: } -63,
{ 182: } -156,
{ 183: } 0,
{ 184: } 0,
{ 185: } 0,
{ 186: } 0,
{ 187: } 0,
{ 188: } -143,
{ 189: } -144,
{ 190: } 0,
{ 191: } 0,
{ 192: } 0,
{ 193: } 0,
{ 194: } -153,
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
{ 210: } 0,
{ 211: } 0,
{ 212: } 0,
{ 213: } 0,
{ 214: } 0,
{ 215: } 0,
{ 216: } 0,
{ 217: } 0,
{ 218: } -101,
{ 219: } 0,
{ 220: } 0,
{ 221: } 0,
{ 222: } 0,
{ 223: } 0,
{ 224: } 0,
{ 225: } -107,
{ 226: } -103,
{ 227: } -27,
{ 228: } -21,
{ 229: } -23,
{ 230: } 0,
{ 231: } 0,
{ 232: } -81,
{ 233: } 0,
{ 234: } -2,
{ 235: } -36,
{ 236: } 0,
{ 237: } -162,
{ 238: } 0,
{ 239: } -152,
{ 240: } -155,
{ 241: } 0,
{ 242: } 0,
{ 243: } 0,
{ 244: } 0,
{ 245: } 0,
{ 246: } 0,
{ 247: } 0,
{ 248: } 0,
{ 249: } 0,
{ 250: } 0,
{ 251: } -134,
{ 252: } 0,
{ 253: } 0,
{ 254: } 0,
{ 255: } 0,
{ 256: } 0,
{ 257: } 0,
{ 258: } 0,
{ 259: } 0,
{ 260: } 0,
{ 261: } 0,
{ 262: } -148,
{ 263: } 0,
{ 264: } 0,
{ 265: } -88,
{ 266: } 0,
{ 267: } -112,
{ 268: } 0,
{ 269: } 0,
{ 270: } 0,
{ 271: } 0,
{ 272: } 0,
{ 273: } 0,
{ 274: } 0,
{ 275: } 0,
{ 276: } 0,
{ 277: } 0,
{ 278: } 0,
{ 279: } -19,
{ 280: } 0,
{ 281: } 0,
{ 282: } -164,
{ 283: } 0,
{ 284: } 0,
{ 285: } 0,
{ 286: } 0,
{ 287: } 0,
{ 288: } 0,
{ 289: } 0,
{ 290: } 0,
{ 291: } -104,
{ 292: } -115,
{ 293: } 0,
{ 294: } 0,
{ 295: } 0,
{ 296: } -163,
{ 297: } 0,
{ 298: } 0,
{ 299: } 0,
{ 300: } -111,
{ 301: } -113,
{ 302: } -22,
{ 303: } 0,
{ 304: } 0,
{ 305: } -32,
{ 306: } -154
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
{ 25: } 137,
{ 26: } 188,
{ 27: } 233,
{ 28: } 233,
{ 29: } 233,
{ 30: } 233,
{ 31: } 233,
{ 32: } 246,
{ 33: } 254,
{ 34: } 254,
{ 35: } 254,
{ 36: } 256,
{ 37: } 272,
{ 38: } 289,
{ 39: } 292,
{ 40: } 295,
{ 41: } 316,
{ 42: } 337,
{ 43: } 358,
{ 44: } 367,
{ 45: } 367,
{ 46: } 367,
{ 47: } 367,
{ 48: } 367,
{ 49: } 367,
{ 50: } 367,
{ 51: } 367,
{ 52: } 387,
{ 53: } 387,
{ 54: } 400,
{ 55: } 420,
{ 56: } 420,
{ 57: } 420,
{ 58: } 422,
{ 59: } 422,
{ 60: } 422,
{ 61: } 466,
{ 62: } 466,
{ 63: } 466,
{ 64: } 466,
{ 65: } 466,
{ 66: } 466,
{ 67: } 475,
{ 68: } 490,
{ 69: } 490,
{ 70: } 507,
{ 71: } 509,
{ 72: } 509,
{ 73: } 525,
{ 74: } 546,
{ 75: } 567,
{ 76: } 567,
{ 77: } 568,
{ 78: } 574,
{ 79: } 578,
{ 80: } 586,
{ 81: } 586,
{ 82: } 594,
{ 83: } 602,
{ 84: } 602,
{ 85: } 602,
{ 86: } 602,
{ 87: } 610,
{ 88: } 618,
{ 89: } 618,
{ 90: } 619,
{ 91: } 633,
{ 92: } 634,
{ 93: } 643,
{ 94: } 643,
{ 95: } 644,
{ 96: } 647,
{ 97: } 648,
{ 98: } 652,
{ 99: } 652,
{ 100: } 654,
{ 101: } 662,
{ 102: } 663,
{ 103: } 664,
{ 104: } 668,
{ 105: } 668,
{ 106: } 669,
{ 107: } 699,
{ 108: } 719,
{ 109: } 719,
{ 110: } 719,
{ 111: } 719,
{ 112: } 734,
{ 113: } 749,
{ 114: } 764,
{ 115: } 764,
{ 116: } 764,
{ 117: } 772,
{ 118: } 772,
{ 119: } 787,
{ 120: } 803,
{ 121: } 803,
{ 122: } 804,
{ 123: } 812,
{ 124: } 830,
{ 125: } 831,
{ 126: } 846,
{ 127: } 861,
{ 128: } 865,
{ 129: } 868,
{ 130: } 875,
{ 131: } 882,
{ 132: } 889,
{ 133: } 889,
{ 134: } 889,
{ 135: } 889,
{ 136: } 891,
{ 137: } 891,
{ 138: } 894,
{ 139: } 894,
{ 140: } 909,
{ 141: } 909,
{ 142: } 912,
{ 143: } 912,
{ 144: } 914,
{ 145: } 929,
{ 146: } 944,
{ 147: } 944,
{ 148: } 960,
{ 149: } 976,
{ 150: } 1006,
{ 151: } 1024,
{ 152: } 1047,
{ 153: } 1052,
{ 154: } 1069,
{ 155: } 1094,
{ 156: } 1109,
{ 157: } 1139,
{ 158: } 1169,
{ 159: } 1199,
{ 160: } 1206,
{ 161: } 1208,
{ 162: } 1209,
{ 163: } 1220,
{ 164: } 1220,
{ 165: } 1231,
{ 166: } 1231,
{ 167: } 1231,
{ 168: } 1249,
{ 169: } 1249,
{ 170: } 1249,
{ 171: } 1255,
{ 172: } 1256,
{ 173: } 1274,
{ 174: } 1292,
{ 175: } 1293,
{ 176: } 1293,
{ 177: } 1294,
{ 178: } 1318,
{ 179: } 1342,
{ 180: } 1351,
{ 181: } 1351,
{ 182: } 1351,
{ 183: } 1351,
{ 184: } 1371,
{ 185: } 1373,
{ 186: } 1374,
{ 187: } 1375,
{ 188: } 1390,
{ 189: } 1390,
{ 190: } 1390,
{ 191: } 1393,
{ 192: } 1394,
{ 193: } 1414,
{ 194: } 1415,
{ 195: } 1415,
{ 196: } 1416,
{ 197: } 1431,
{ 198: } 1432,
{ 199: } 1447,
{ 200: } 1462,
{ 201: } 1477,
{ 202: } 1492,
{ 203: } 1507,
{ 204: } 1522,
{ 205: } 1537,
{ 206: } 1552,
{ 207: } 1567,
{ 208: } 1582,
{ 209: } 1597,
{ 210: } 1612,
{ 211: } 1627,
{ 212: } 1642,
{ 213: } 1657,
{ 214: } 1672,
{ 215: } 1687,
{ 216: } 1728,
{ 217: } 1731,
{ 218: } 1746,
{ 219: } 1746,
{ 220: } 1747,
{ 221: } 1751,
{ 222: } 1755,
{ 223: } 1765,
{ 224: } 1776,
{ 225: } 1787,
{ 226: } 1787,
{ 227: } 1787,
{ 228: } 1787,
{ 229: } 1787,
{ 230: } 1787,
{ 231: } 1802,
{ 232: } 1803,
{ 233: } 1803,
{ 234: } 1804,
{ 235: } 1804,
{ 236: } 1804,
{ 237: } 1819,
{ 238: } 1819,
{ 239: } 1836,
{ 240: } 1836,
{ 241: } 1836,
{ 242: } 1837,
{ 243: } 1867,
{ 244: } 1882,
{ 245: } 1912,
{ 246: } 1942,
{ 247: } 1972,
{ 248: } 2002,
{ 249: } 2032,
{ 250: } 2062,
{ 251: } 2092,
{ 252: } 2092,
{ 253: } 2110,
{ 254: } 2140,
{ 255: } 2170,
{ 256: } 2200,
{ 257: } 2230,
{ 258: } 2260,
{ 259: } 2290,
{ 260: } 2320,
{ 261: } 2350,
{ 262: } 2380,
{ 263: } 2380,
{ 264: } 2410,
{ 265: } 2412,
{ 266: } 2412,
{ 267: } 2423,
{ 268: } 2423,
{ 269: } 2438,
{ 270: } 2453,
{ 271: } 2469,
{ 272: } 2472,
{ 273: } 2475,
{ 274: } 2486,
{ 275: } 2490,
{ 276: } 2494,
{ 277: } 2498,
{ 278: } 2502,
{ 279: } 2520,
{ 280: } 2520,
{ 281: } 2535,
{ 282: } 2536,
{ 283: } 2536,
{ 284: } 2551,
{ 285: } 2581,
{ 286: } 2596,
{ 287: } 2597,
{ 288: } 2601,
{ 289: } 2605,
{ 290: } 2606,
{ 291: } 2624,
{ 292: } 2624,
{ 293: } 2624,
{ 294: } 2628,
{ 295: } 2646,
{ 296: } 2647,
{ 297: } 2647,
{ 298: } 2677,
{ 299: } 2707,
{ 300: } 2723,
{ 301: } 2723,
{ 302: } 2723,
{ 303: } 2723,
{ 304: } 2724,
{ 305: } 2725,
{ 306: } 2725
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
{ 24: } 136,
{ 25: } 187,
{ 26: } 232,
{ 27: } 232,
{ 28: } 232,
{ 29: } 232,
{ 30: } 232,
{ 31: } 245,
{ 32: } 253,
{ 33: } 253,
{ 34: } 253,
{ 35: } 255,
{ 36: } 271,
{ 37: } 288,
{ 38: } 291,
{ 39: } 294,
{ 40: } 315,
{ 41: } 336,
{ 42: } 357,
{ 43: } 366,
{ 44: } 366,
{ 45: } 366,
{ 46: } 366,
{ 47: } 366,
{ 48: } 366,
{ 49: } 366,
{ 50: } 366,
{ 51: } 386,
{ 52: } 386,
{ 53: } 399,
{ 54: } 419,
{ 55: } 419,
{ 56: } 419,
{ 57: } 421,
{ 58: } 421,
{ 59: } 421,
{ 60: } 465,
{ 61: } 465,
{ 62: } 465,
{ 63: } 465,
{ 64: } 465,
{ 65: } 465,
{ 66: } 474,
{ 67: } 489,
{ 68: } 489,
{ 69: } 506,
{ 70: } 508,
{ 71: } 508,
{ 72: } 524,
{ 73: } 545,
{ 74: } 566,
{ 75: } 566,
{ 76: } 567,
{ 77: } 573,
{ 78: } 577,
{ 79: } 585,
{ 80: } 585,
{ 81: } 593,
{ 82: } 601,
{ 83: } 601,
{ 84: } 601,
{ 85: } 601,
{ 86: } 609,
{ 87: } 617,
{ 88: } 617,
{ 89: } 618,
{ 90: } 632,
{ 91: } 633,
{ 92: } 642,
{ 93: } 642,
{ 94: } 643,
{ 95: } 646,
{ 96: } 647,
{ 97: } 651,
{ 98: } 651,
{ 99: } 653,
{ 100: } 661,
{ 101: } 662,
{ 102: } 663,
{ 103: } 667,
{ 104: } 667,
{ 105: } 668,
{ 106: } 698,
{ 107: } 718,
{ 108: } 718,
{ 109: } 718,
{ 110: } 718,
{ 111: } 733,
{ 112: } 748,
{ 113: } 763,
{ 114: } 763,
{ 115: } 763,
{ 116: } 771,
{ 117: } 771,
{ 118: } 786,
{ 119: } 802,
{ 120: } 802,
{ 121: } 803,
{ 122: } 811,
{ 123: } 829,
{ 124: } 830,
{ 125: } 845,
{ 126: } 860,
{ 127: } 864,
{ 128: } 867,
{ 129: } 874,
{ 130: } 881,
{ 131: } 888,
{ 132: } 888,
{ 133: } 888,
{ 134: } 888,
{ 135: } 890,
{ 136: } 890,
{ 137: } 893,
{ 138: } 893,
{ 139: } 908,
{ 140: } 908,
{ 141: } 911,
{ 142: } 911,
{ 143: } 913,
{ 144: } 928,
{ 145: } 943,
{ 146: } 943,
{ 147: } 959,
{ 148: } 975,
{ 149: } 1005,
{ 150: } 1023,
{ 151: } 1046,
{ 152: } 1051,
{ 153: } 1068,
{ 154: } 1093,
{ 155: } 1108,
{ 156: } 1138,
{ 157: } 1168,
{ 158: } 1198,
{ 159: } 1205,
{ 160: } 1207,
{ 161: } 1208,
{ 162: } 1219,
{ 163: } 1219,
{ 164: } 1230,
{ 165: } 1230,
{ 166: } 1230,
{ 167: } 1248,
{ 168: } 1248,
{ 169: } 1248,
{ 170: } 1254,
{ 171: } 1255,
{ 172: } 1273,
{ 173: } 1291,
{ 174: } 1292,
{ 175: } 1292,
{ 176: } 1293,
{ 177: } 1317,
{ 178: } 1341,
{ 179: } 1350,
{ 180: } 1350,
{ 181: } 1350,
{ 182: } 1350,
{ 183: } 1370,
{ 184: } 1372,
{ 185: } 1373,
{ 186: } 1374,
{ 187: } 1389,
{ 188: } 1389,
{ 189: } 1389,
{ 190: } 1392,
{ 191: } 1393,
{ 192: } 1413,
{ 193: } 1414,
{ 194: } 1414,
{ 195: } 1415,
{ 196: } 1430,
{ 197: } 1431,
{ 198: } 1446,
{ 199: } 1461,
{ 200: } 1476,
{ 201: } 1491,
{ 202: } 1506,
{ 203: } 1521,
{ 204: } 1536,
{ 205: } 1551,
{ 206: } 1566,
{ 207: } 1581,
{ 208: } 1596,
{ 209: } 1611,
{ 210: } 1626,
{ 211: } 1641,
{ 212: } 1656,
{ 213: } 1671,
{ 214: } 1686,
{ 215: } 1727,
{ 216: } 1730,
{ 217: } 1745,
{ 218: } 1745,
{ 219: } 1746,
{ 220: } 1750,
{ 221: } 1754,
{ 222: } 1764,
{ 223: } 1775,
{ 224: } 1786,
{ 225: } 1786,
{ 226: } 1786,
{ 227: } 1786,
{ 228: } 1786,
{ 229: } 1786,
{ 230: } 1801,
{ 231: } 1802,
{ 232: } 1802,
{ 233: } 1803,
{ 234: } 1803,
{ 235: } 1803,
{ 236: } 1818,
{ 237: } 1818,
{ 238: } 1835,
{ 239: } 1835,
{ 240: } 1835,
{ 241: } 1836,
{ 242: } 1866,
{ 243: } 1881,
{ 244: } 1911,
{ 245: } 1941,
{ 246: } 1971,
{ 247: } 2001,
{ 248: } 2031,
{ 249: } 2061,
{ 250: } 2091,
{ 251: } 2091,
{ 252: } 2109,
{ 253: } 2139,
{ 254: } 2169,
{ 255: } 2199,
{ 256: } 2229,
{ 257: } 2259,
{ 258: } 2289,
{ 259: } 2319,
{ 260: } 2349,
{ 261: } 2379,
{ 262: } 2379,
{ 263: } 2409,
{ 264: } 2411,
{ 265: } 2411,
{ 266: } 2422,
{ 267: } 2422,
{ 268: } 2437,
{ 269: } 2452,
{ 270: } 2468,
{ 271: } 2471,
{ 272: } 2474,
{ 273: } 2485,
{ 274: } 2489,
{ 275: } 2493,
{ 276: } 2497,
{ 277: } 2501,
{ 278: } 2519,
{ 279: } 2519,
{ 280: } 2534,
{ 281: } 2535,
{ 282: } 2535,
{ 283: } 2550,
{ 284: } 2580,
{ 285: } 2595,
{ 286: } 2596,
{ 287: } 2600,
{ 288: } 2604,
{ 289: } 2605,
{ 290: } 2623,
{ 291: } 2623,
{ 292: } 2623,
{ 293: } 2627,
{ 294: } 2645,
{ 295: } 2646,
{ 296: } 2646,
{ 297: } 2676,
{ 298: } 2706,
{ 299: } 2722,
{ 300: } 2722,
{ 301: } 2722,
{ 302: } 2722,
{ 303: } 2723,
{ 304: } 2724,
{ 305: } 2724,
{ 306: } 2724
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
{ 79: } 79,
{ 80: } 80,
{ 81: } 81,
{ 82: } 84,
{ 83: } 87,
{ 84: } 87,
{ 85: } 87,
{ 86: } 87,
{ 87: } 90,
{ 88: } 93,
{ 89: } 93,
{ 90: } 93,
{ 91: } 100,
{ 92: } 100,
{ 93: } 104,
{ 94: } 104,
{ 95: } 104,
{ 96: } 104,
{ 97: } 104,
{ 98: } 104,
{ 99: } 104,
{ 100: } 104,
{ 101: } 107,
{ 102: } 107,
{ 103: } 107,
{ 104: } 107,
{ 105: } 107,
{ 106: } 107,
{ 107: } 107,
{ 108: } 115,
{ 109: } 115,
{ 110: } 115,
{ 111: } 115,
{ 112: } 118,
{ 113: } 121,
{ 114: } 124,
{ 115: } 124,
{ 116: } 124,
{ 117: } 127,
{ 118: } 127,
{ 119: } 134,
{ 120: } 139,
{ 121: } 139,
{ 122: } 139,
{ 123: } 142,
{ 124: } 149,
{ 125: } 149,
{ 126: } 154,
{ 127: } 159,
{ 128: } 159,
{ 129: } 160,
{ 130: } 161,
{ 131: } 162,
{ 132: } 163,
{ 133: } 163,
{ 134: } 163,
{ 135: } 163,
{ 136: } 163,
{ 137: } 163,
{ 138: } 166,
{ 139: } 166,
{ 140: } 171,
{ 141: } 171,
{ 142: } 172,
{ 143: } 172,
{ 144: } 174,
{ 145: } 179,
{ 146: } 184,
{ 147: } 184,
{ 148: } 191,
{ 149: } 198,
{ 150: } 198,
{ 151: } 198,
{ 152: } 198,
{ 153: } 199,
{ 154: } 199,
{ 155: } 199,
{ 156: } 202,
{ 157: } 202,
{ 158: } 202,
{ 159: } 202,
{ 160: } 203,
{ 161: } 203,
{ 162: } 203,
{ 163: } 207,
{ 164: } 207,
{ 165: } 207,
{ 166: } 207,
{ 167: } 207,
{ 168: } 207,
{ 169: } 207,
{ 170: } 207,
{ 171: } 208,
{ 172: } 208,
{ 173: } 208,
{ 174: } 215,
{ 175: } 215,
{ 176: } 215,
{ 177: } 216,
{ 178: } 216,
{ 179: } 216,
{ 180: } 220,
{ 181: } 220,
{ 182: } 220,
{ 183: } 220,
{ 184: } 220,
{ 185: } 221,
{ 186: } 221,
{ 187: } 221,
{ 188: } 225,
{ 189: } 225,
{ 190: } 225,
{ 191: } 225,
{ 192: } 225,
{ 193: } 225,
{ 194: } 225,
{ 195: } 225,
{ 196: } 225,
{ 197: } 228,
{ 198: } 228,
{ 199: } 233,
{ 200: } 238,
{ 201: } 243,
{ 202: } 248,
{ 203: } 253,
{ 204: } 258,
{ 205: } 263,
{ 206: } 269,
{ 207: } 274,
{ 208: } 279,
{ 209: } 284,
{ 210: } 289,
{ 211: } 294,
{ 212: } 299,
{ 213: } 304,
{ 214: } 309,
{ 215: } 314,
{ 216: } 318,
{ 217: } 318,
{ 218: } 325,
{ 219: } 325,
{ 220: } 325,
{ 221: } 326,
{ 222: } 327,
{ 223: } 331,
{ 224: } 335,
{ 225: } 339,
{ 226: } 339,
{ 227: } 339,
{ 228: } 339,
{ 229: } 339,
{ 230: } 339,
{ 231: } 344,
{ 232: } 344,
{ 233: } 344,
{ 234: } 344,
{ 235: } 344,
{ 236: } 344,
{ 237: } 348,
{ 238: } 348,
{ 239: } 355,
{ 240: } 355,
{ 241: } 355,
{ 242: } 355,
{ 243: } 355,
{ 244: } 358,
{ 245: } 358,
{ 246: } 358,
{ 247: } 358,
{ 248: } 358,
{ 249: } 358,
{ 250: } 358,
{ 251: } 358,
{ 252: } 358,
{ 253: } 358,
{ 254: } 358,
{ 255: } 358,
{ 256: } 358,
{ 257: } 358,
{ 258: } 358,
{ 259: } 358,
{ 260: } 358,
{ 261: } 358,
{ 262: } 358,
{ 263: } 358,
{ 264: } 358,
{ 265: } 359,
{ 266: } 359,
{ 267: } 363,
{ 268: } 363,
{ 269: } 370,
{ 270: } 375,
{ 271: } 380,
{ 272: } 381,
{ 273: } 382,
{ 274: } 386,
{ 275: } 387,
{ 276: } 388,
{ 277: } 389,
{ 278: } 390,
{ 279: } 390,
{ 280: } 390,
{ 281: } 397,
{ 282: } 397,
{ 283: } 397,
{ 284: } 400,
{ 285: } 400,
{ 286: } 405,
{ 287: } 405,
{ 288: } 406,
{ 289: } 407,
{ 290: } 407,
{ 291: } 407,
{ 292: } 407,
{ 293: } 407,
{ 294: } 408,
{ 295: } 415,
{ 296: } 415,
{ 297: } 415,
{ 298: } 415,
{ 299: } 415,
{ 300: } 422,
{ 301: } 422,
{ 302: } 422,
{ 303: } 422,
{ 304: } 422,
{ 305: } 422,
{ 306: } 422
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
{ 78: } 78,
{ 79: } 79,
{ 80: } 80,
{ 81: } 83,
{ 82: } 86,
{ 83: } 86,
{ 84: } 86,
{ 85: } 86,
{ 86: } 89,
{ 87: } 92,
{ 88: } 92,
{ 89: } 92,
{ 90: } 99,
{ 91: } 99,
{ 92: } 103,
{ 93: } 103,
{ 94: } 103,
{ 95: } 103,
{ 96: } 103,
{ 97: } 103,
{ 98: } 103,
{ 99: } 103,
{ 100: } 106,
{ 101: } 106,
{ 102: } 106,
{ 103: } 106,
{ 104: } 106,
{ 105: } 106,
{ 106: } 106,
{ 107: } 114,
{ 108: } 114,
{ 109: } 114,
{ 110: } 114,
{ 111: } 117,
{ 112: } 120,
{ 113: } 123,
{ 114: } 123,
{ 115: } 123,
{ 116: } 126,
{ 117: } 126,
{ 118: } 133,
{ 119: } 138,
{ 120: } 138,
{ 121: } 138,
{ 122: } 141,
{ 123: } 148,
{ 124: } 148,
{ 125: } 153,
{ 126: } 158,
{ 127: } 158,
{ 128: } 159,
{ 129: } 160,
{ 130: } 161,
{ 131: } 162,
{ 132: } 162,
{ 133: } 162,
{ 134: } 162,
{ 135: } 162,
{ 136: } 162,
{ 137: } 165,
{ 138: } 165,
{ 139: } 170,
{ 140: } 170,
{ 141: } 171,
{ 142: } 171,
{ 143: } 173,
{ 144: } 178,
{ 145: } 183,
{ 146: } 183,
{ 147: } 190,
{ 148: } 197,
{ 149: } 197,
{ 150: } 197,
{ 151: } 197,
{ 152: } 198,
{ 153: } 198,
{ 154: } 198,
{ 155: } 201,
{ 156: } 201,
{ 157: } 201,
{ 158: } 201,
{ 159: } 202,
{ 160: } 202,
{ 161: } 202,
{ 162: } 206,
{ 163: } 206,
{ 164: } 206,
{ 165: } 206,
{ 166: } 206,
{ 167: } 206,
{ 168: } 206,
{ 169: } 206,
{ 170: } 207,
{ 171: } 207,
{ 172: } 207,
{ 173: } 214,
{ 174: } 214,
{ 175: } 214,
{ 176: } 215,
{ 177: } 215,
{ 178: } 215,
{ 179: } 219,
{ 180: } 219,
{ 181: } 219,
{ 182: } 219,
{ 183: } 219,
{ 184: } 220,
{ 185: } 220,
{ 186: } 220,
{ 187: } 224,
{ 188: } 224,
{ 189: } 224,
{ 190: } 224,
{ 191: } 224,
{ 192: } 224,
{ 193: } 224,
{ 194: } 224,
{ 195: } 224,
{ 196: } 227,
{ 197: } 227,
{ 198: } 232,
{ 199: } 237,
{ 200: } 242,
{ 201: } 247,
{ 202: } 252,
{ 203: } 257,
{ 204: } 262,
{ 205: } 268,
{ 206: } 273,
{ 207: } 278,
{ 208: } 283,
{ 209: } 288,
{ 210: } 293,
{ 211: } 298,
{ 212: } 303,
{ 213: } 308,
{ 214: } 313,
{ 215: } 317,
{ 216: } 317,
{ 217: } 324,
{ 218: } 324,
{ 219: } 324,
{ 220: } 325,
{ 221: } 326,
{ 222: } 330,
{ 223: } 334,
{ 224: } 338,
{ 225: } 338,
{ 226: } 338,
{ 227: } 338,
{ 228: } 338,
{ 229: } 338,
{ 230: } 343,
{ 231: } 343,
{ 232: } 343,
{ 233: } 343,
{ 234: } 343,
{ 235: } 343,
{ 236: } 347,
{ 237: } 347,
{ 238: } 354,
{ 239: } 354,
{ 240: } 354,
{ 241: } 354,
{ 242: } 354,
{ 243: } 357,
{ 244: } 357,
{ 245: } 357,
{ 246: } 357,
{ 247: } 357,
{ 248: } 357,
{ 249: } 357,
{ 250: } 357,
{ 251: } 357,
{ 252: } 357,
{ 253: } 357,
{ 254: } 357,
{ 255: } 357,
{ 256: } 357,
{ 257: } 357,
{ 258: } 357,
{ 259: } 357,
{ 260: } 357,
{ 261: } 357,
{ 262: } 357,
{ 263: } 357,
{ 264: } 358,
{ 265: } 358,
{ 266: } 362,
{ 267: } 362,
{ 268: } 369,
{ 269: } 374,
{ 270: } 379,
{ 271: } 380,
{ 272: } 381,
{ 273: } 385,
{ 274: } 386,
{ 275: } 387,
{ 276: } 388,
{ 277: } 389,
{ 278: } 389,
{ 279: } 389,
{ 280: } 396,
{ 281: } 396,
{ 282: } 396,
{ 283: } 399,
{ 284: } 399,
{ 285: } 404,
{ 286: } 404,
{ 287: } 405,
{ 288: } 406,
{ 289: } 406,
{ 290: } 406,
{ 291: } 406,
{ 292: } 406,
{ 293: } 407,
{ 294: } 414,
{ 295: } 414,
{ 296: } 414,
{ 297: } 414,
{ 298: } 414,
{ 299: } 421,
{ 300: } 421,
{ 301: } 421,
{ 302: } 421,
{ 303: } 421,
{ 304: } 421,
{ 305: } 421,
{ 306: } 421
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
{ 21: } ( len: 2; sym: -12 ),
{ 22: } ( len: 5; sym: -12 ),
{ 23: } ( len: 2; sym: -14 ),
{ 24: } ( len: 1; sym: -14 ),
{ 25: } ( len: 1; sym: -14 ),
{ 26: } ( len: 0; sym: -14 ),
{ 27: } ( len: 3; sym: -15 ),
{ 28: } ( len: 5; sym: -6 ),
{ 29: } ( len: 6; sym: -6 ),
{ 30: } ( len: 2; sym: -6 ),
{ 31: } ( len: 5; sym: -6 ),
{ 32: } ( len: 11; sym: -6 ),
{ 33: } ( len: 5; sym: -6 ),
{ 34: } ( len: 3; sym: -6 ),
{ 35: } ( len: 3; sym: -6 ),
{ 36: } ( len: 7; sym: -7 ),
{ 37: } ( len: 4; sym: -7 ),
{ 38: } ( len: 3; sym: -7 ),
{ 39: } ( len: 5; sym: -7 ),
{ 40: } ( len: 3; sym: -7 ),
{ 41: } ( len: 3; sym: -24 ),
{ 42: } ( len: 3; sym: -24 ),
{ 43: } ( len: 3; sym: -26 ),
{ 44: } ( len: 3; sym: -26 ),
{ 45: } ( len: 4; sym: -18 ),
{ 46: } ( len: 3; sym: -18 ),
{ 47: } ( len: 4; sym: -18 ),
{ 48: } ( len: 3; sym: -18 ),
{ 49: } ( len: 2; sym: -18 ),
{ 50: } ( len: 2; sym: -18 ),
{ 51: } ( len: 3; sym: -18 ),
{ 52: } ( len: 2; sym: -18 ),
{ 53: } ( len: 2; sym: -16 ),
{ 54: } ( len: 3; sym: -16 ),
{ 55: } ( len: 2; sym: -16 ),
{ 56: } ( len: 3; sym: -16 ),
{ 57: } ( len: 2; sym: -16 ),
{ 58: } ( len: 2; sym: -16 ),
{ 59: } ( len: 1; sym: -16 ),
{ 60: } ( len: 1; sym: -16 ),
{ 61: } ( len: 2; sym: -25 ),
{ 62: } ( len: 1; sym: -25 ),
{ 63: } ( len: 3; sym: -28 ),
{ 64: } ( len: 1; sym: -11 ),
{ 65: } ( len: 2; sym: -29 ),
{ 66: } ( len: 2; sym: -29 ),
{ 67: } ( len: 1; sym: -29 ),
{ 68: } ( len: 1; sym: -29 ),
{ 69: } ( len: 2; sym: -29 ),
{ 70: } ( len: 2; sym: -29 ),
{ 71: } ( len: 3; sym: -29 ),
{ 72: } ( len: 1; sym: -29 ),
{ 73: } ( len: 2; sym: -29 ),
{ 74: } ( len: 1; sym: -29 ),
{ 75: } ( len: 1; sym: -29 ),
{ 76: } ( len: 1; sym: -29 ),
{ 77: } ( len: 1; sym: -29 ),
{ 78: } ( len: 1; sym: -27 ),
{ 79: } ( len: 1; sym: -27 ),
{ 80: } ( len: 3; sym: -17 ),
{ 81: } ( len: 4; sym: -17 ),
{ 82: } ( len: 2; sym: -17 ),
{ 83: } ( len: 1; sym: -17 ),
{ 84: } ( len: 2; sym: -30 ),
{ 85: } ( len: 3; sym: -30 ),
{ 86: } ( len: 2; sym: -30 ),
{ 87: } ( len: 1; sym: -20 ),
{ 88: } ( len: 3; sym: -20 ),
{ 89: } ( len: 1; sym: -20 ),
{ 90: } ( len: 0; sym: -20 ),
{ 91: } ( len: 1; sym: -32 ),
{ 92: } ( len: 1; sym: -32 ),
{ 93: } ( len: 1; sym: -32 ),
{ 94: } ( len: 2; sym: -19 ),
{ 95: } ( len: 3; sym: -19 ),
{ 96: } ( len: 2; sym: -19 ),
{ 97: } ( len: 2; sym: -19 ),
{ 98: } ( len: 3; sym: -19 ),
{ 99: } ( len: 3; sym: -19 ),
{ 100: } ( len: 1; sym: -19 ),
{ 101: } ( len: 4; sym: -19 ),
{ 102: } ( len: 2; sym: -19 ),
{ 103: } ( len: 4; sym: -19 ),
{ 104: } ( len: 3; sym: -19 ),
{ 105: } ( len: 3; sym: -19 ),
{ 106: } ( len: 2; sym: -34 ),
{ 107: } ( len: 3; sym: -34 ),
{ 108: } ( len: 2; sym: -31 ),
{ 109: } ( len: 3; sym: -31 ),
{ 110: } ( len: 2; sym: -31 ),
{ 111: } ( len: 4; sym: -31 ),
{ 112: } ( len: 2; sym: -31 ),
{ 113: } ( len: 4; sym: -31 ),
{ 114: } ( len: 3; sym: -31 ),
{ 115: } ( len: 3; sym: -31 ),
{ 116: } ( len: 0; sym: -31 ),
{ 117: } ( len: 1; sym: -13 ),
{ 118: } ( len: 3; sym: -35 ),
{ 119: } ( len: 3; sym: -35 ),
{ 120: } ( len: 3; sym: -35 ),
{ 121: } ( len: 3; sym: -35 ),
{ 122: } ( len: 3; sym: -35 ),
{ 123: } ( len: 3; sym: -35 ),
{ 124: } ( len: 3; sym: -35 ),
{ 125: } ( len: 3; sym: -35 ),
{ 126: } ( len: 3; sym: -35 ),
{ 127: } ( len: 3; sym: -35 ),
{ 128: } ( len: 3; sym: -35 ),
{ 129: } ( len: 3; sym: -35 ),
{ 130: } ( len: 3; sym: -35 ),
{ 131: } ( len: 3; sym: -35 ),
{ 132: } ( len: 3; sym: -35 ),
{ 133: } ( len: 3; sym: -35 ),
{ 134: } ( len: 3; sym: -35 ),
{ 135: } ( len: 1; sym: -35 ),
{ 136: } ( len: 3; sym: -36 ),
{ 137: } ( len: 1; sym: -38 ),
{ 138: } ( len: 0; sym: -38 ),
{ 139: } ( len: 1; sym: -37 ),
{ 140: } ( len: 1; sym: -37 ),
{ 141: } ( len: 1; sym: -37 ),
{ 142: } ( len: 1; sym: -37 ),
{ 143: } ( len: 3; sym: -37 ),
{ 144: } ( len: 3; sym: -37 ),
{ 145: } ( len: 2; sym: -37 ),
{ 146: } ( len: 2; sym: -37 ),
{ 147: } ( len: 2; sym: -37 ),
{ 148: } ( len: 4; sym: -37 ),
{ 149: } ( len: 4; sym: -37 ),
{ 150: } ( len: 5; sym: -37 ),
{ 151: } ( len: 6; sym: -37 ),
{ 152: } ( len: 4; sym: -37 ),
{ 153: } ( len: 3; sym: -37 ),
{ 154: } ( len: 8; sym: -37 ),
{ 155: } ( len: 4; sym: -37 ),
{ 156: } ( len: 3; sym: -21 ),
{ 157: } ( len: 1; sym: -21 ),
{ 158: } ( len: 0; sym: -21 ),
{ 159: } ( len: 3; sym: -40 ),
{ 160: } ( len: 1; sym: -40 ),
{ 161: } ( len: 1; sym: -23 ),
{ 162: } ( len: 2; sym: -22 ),
{ 163: } ( len: 4; sym: -22 ),
{ 164: } ( len: 3; sym: -39 ),
{ 165: } ( len: 1; sym: -39 ),
{ 166: } ( len: 0; sym: -39 ),
{ 167: } ( len: 1; sym: -41 )
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
     if UseCTypesUnit then
     begin
       writeln(headerfile,'uses');
       writeln(headerfile,'  ctypes;');
       writeln(headerfile);
     end;
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