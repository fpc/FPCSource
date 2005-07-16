{
    This unit implements basic regular expression support

    This file is part of the Free Pascal run time library.
    Copyright (c) 2000-2005 by Florian Klaempfl

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{ $define DEBUG}
{
  TODO:
     - correct backtracking, for example in (...)*
     - | support
     - getting substrings and using substrings with \1 etc.
     - test ^  and $
     - newline handling in DOS?
     - locals dependend upper/lowercase routines
     - extend the interface
}

{$mode objfpc}

unit regexpr;

  interface

    { the following declarions are only in the interface because }
    { some procedures return pregexprentry but programs which   }
    { use this unit shouldn't access this data structures        }
    type
       tcharset = set of char;
       tregexprentrytype = (ret_charset,ret_or,ret_startpattern,
          ret_endpattern,ret_illegalend,ret_backtrace,ret_startline,
          ret_endline);

       pregexprentry = ^tregexprentry;
       tregexprentry = record
          next,nextdestroy : pregexprentry;
          case typ : tregexprentrytype of
             ret_charset : (chars : tcharset;
                            elsepath : pregexprentry);
             ret_or : (alternative : pregexprentry);
       end;

       tregexprflag = (ref_singleline,ref_multiline,ref_caseinsensitive);
       tregexprflags = set of tregexprflag;

       TRegExprEngine = record
          Data : pregexprentry;
          DestroyList : pregexprentry;
          Flags : TRegExprFlags;
       end;

     const
        cs_allchars : tcharset = [#0..#255];
        cs_wordchars : tcharset = ['A'..'Z','a'..'z','_','0'..'9'];
        cs_newline : tcharset = [#10];
        cs_digits : tcharset = ['0'..'9'];
        cs_whitespace : tcharset = [' ',#9];

     var
        { these are initilized in the init section of the unit }
        cs_nonwordchars : tcharset;
        cs_nondigits : tcharset;
        cs_nonwhitespace : tcharset;

     { the following procedures can be used by units basing }
     { on the regexpr unit                                  }
     function GenerateRegExprEngine(regexpr : pchar;flags : tregexprflags) : TRegExprEngine;

     procedure DestroyRegExprEngine(var regexpr : TRegExprEngine);

     function RegExprPos(regexprengine : TRegExprEngine;p : pchar;var index,len : longint) : boolean;

  implementation

{$ifdef DEBUG}
     procedure writecharset(c : tcharset);

       var
          b : byte;

       begin
          for b:=0 to 255 do
            if chr(b) in c then
              write(chr(b));
          writeln;
       end;
{$endif DEBUG}

     function GenerateRegExprEngine(regexpr : pchar;flags : tregexprflags) : TRegExprEngine;

       var
          first : pregexprentry;

       procedure doregister(p : pregexprentry);

         begin
            p^.nextdestroy:=first;
            if not(assigned(first)) then
              first:=p;
         end;

       var
          currentpos : pchar;
          error : boolean;

       function readchars : tcharset;

         var
            c1 : char;

         begin
            readchars:=[];
            case currentpos^ of
               #0:
                  exit;
               '.':
                  begin
                     inc(currentpos);
                     readchars:=cs_allchars-cs_newline;
                  end;
               '\':
                  begin
                     inc(currentpos);
                     case currentpos^ of
                        #0:
                          begin
                             error:=true;
                             exit;
                          end;
                        't':
                           begin
                              inc(currentpos);
                              readchars:=[#9];
                           end;
                        'n':
                           begin
                              inc(currentpos);
                              readchars:=[#10];
                           end;
                        'r':
                           begin
                              inc(currentpos);
                              readchars:=[#13];
                           end;
                        'd':
                          begin
                             inc(currentpos);
                             readchars:=cs_digits;
                          end;
                        'D':
                          begin
                             inc(currentpos);
                             readchars:=cs_nondigits;
                          end;
                        's':
                          begin
                             inc(currentpos);
                             readchars:=cs_whitespace;
                          end;
                        'S':
                          begin
                             inc(currentpos);
                             readchars:=cs_nonwhitespace;
                          end;
                        'w':
                           begin
                              inc(currentpos);
                              readchars:=cs_wordchars;
                           end;
                        'W':
                           begin
                              inc(currentpos);
                              readchars:=cs_nonwordchars;
                           end;
                         else
                           begin //Some basic escaping...
                              readchars := [currentpos^];
                              inc (currentpos);
                              {error:=true;
                              exit;}
                           end;
                     end;
                  end;
               else
                 begin
                    if ref_caseinsensitive in flags then
                       c1:=upcase(currentpos^)
                    else
                       c1:=currentpos^;

                    inc(currentpos);
                    if currentpos^='-' then
                      begin
                         inc(currentpos);
                         if currentpos^=#0 then
                           begin
                              error:=true;
                              exit;
                           end;
                         if ref_caseinsensitive in flags then
                           readchars:=[c1..upcase(currentpos^)]
                         else
                           readchars:=[c1..currentpos^];
                         inc(currentpos);
                      end
                    else
                      readchars:=[c1];
                 end;
            end;
         end;


       function readcharset : tcharset;

         begin
            readcharset:=[];
            case currentpos^ of
               #0:
                  exit;
               '[':
                  begin
                     inc(currentpos);
                     while currentpos^<>']' do
                       begin
                          if currentpos^='^' then
                            begin
                               inc(currentpos);
                               readcharset:=readcharset+(cs_allchars-readchars);
                            end
                          else
                            readcharset:=readcharset+readchars;
                          if error or (currentpos^=#0) then
                            begin
                               error:=true;
                               exit;
                            end;
                       end;
                     inc(currentpos);
                  end;
               '^':
                  begin
                     inc(currentpos);
                     readcharset:=cs_allchars-readchars;
                  end;
               else
                  readcharset:=readchars;
            end;
         end;

       function parseregexpr(next,elsepath : pregexprentry) : pregexprentry;

         var
            hp,hp2,ep : pregexprentry;
            cs : tcharset;
            chaining : ^pregexprentry;

         begin
            chaining:=nil;
            parseregexpr:=nil;
            if error then
              exit;
            { this dummy allows us to redirect the elsepath later }
            new(ep);
            doregister(ep);
            ep^.typ:=ret_charset;
            ep^.chars:=[];
            ep^.elsepath:=elsepath;
            elsepath:=ep;
            while true do
              begin
                 if error then
                   exit;
                 case currentpos^ of
                    '(':
                       begin
                          inc(currentpos);
                          new(hp2);
                          doregister(hp2);
                          hp2^.typ:=ret_charset;
                          hp2^.chars:=[];
                          hp2^.elsepath:=next;
                          hp:=parseregexpr(hp2,ep);
                          if assigned(chaining) then
                            chaining^:=hp
                          else
                            parseregexpr:=hp;
                          chaining:=@hp2^.elsepath;
                          if currentpos^<>')' then
                            begin
                               error:=true;
                               exit;
                            end;
                          inc(currentpos);
                       end;
{
                    '|':
                       begin
{$ifdef DEBUG}
                          writeln('Creating backtrace entry');
{$endif DEBUG}
                          while currentpos^='|' do
                            begin
                              inc(currentpos);
                              if currentpos^=#0 then
                                begin
                                   error:=true;
                                   exit;
                                end;

                          doregister(hp2);
                          hp2^.typ:=ret_charset;
                          hp2^.chars:=[];
                          hp2^.elsepath:=next;

                              new(hp);
                              doregister(hp);
                              hp^.typ:=ret_backtrace;
                              hp^.elsepath:=parseregexpr();
                              hp^.next:=next;
                              if assigned(chaining) then
                                chaining^:=hp
                              else
                                parseregexpr:=hp;
                              chaining:=@hp^.elsepath;
                            end;
                          exit;
                       end;
}
                    ')':
                       exit;
                    '^':
                       begin
                          inc(currentpos);
                          new(hp);
                          doregister(hp);
                          hp^.typ:=ret_startline;
                          hp^.elsepath:=ep;
                          // hp^.next:=parseregexpr(ep);
                       end;
                    '$':
                       begin
                          inc(currentpos);
                          new(hp);
                          doregister(hp);
                          hp^.typ:=ret_endline;
                          hp^.elsepath:=ep;
                          // hp^.next:=parseregexpr(ep);
                       end;
                    #0:
                       exit;
                    else
                      begin
                         cs:=readcharset;
                         if error then
                           exit;
                         case currentpos^ of
                            '*':
                               begin
                                  inc(currentpos);
                                  new(hp);
                                  doregister(hp);
                                  hp^.typ:=ret_charset;
                                  hp^.chars:=cs;
                                  hp^.elsepath:=next;
                                  hp^.next:=hp;
                                  if assigned(chaining) then
                                    chaining^:=hp
                                  else
                                    parseregexpr:=hp;
                                  chaining:=@hp^.elsepath;
                               end;
                            '+':
                               begin
                                  inc(currentpos);
                                  new(hp);
                                  new(hp2);
                                  doregister(hp);
                                  doregister(hp2);
                                  hp^.typ:=ret_charset;
                                  hp2^.typ:=ret_charset;
                                  hp^.chars:=cs;
                                  hp2^.chars:=cs;
                                  hp^.elsepath:=elsepath;
                                  hp^.next:=hp2;
                                  hp2^.elsepath:=next;
                                  hp2^.next:=hp2;
                                  if assigned(chaining) then
                                    chaining^:=hp
                                  else
                                    parseregexpr:=hp;
                                  chaining:=@hp2^.elsepath;
                               end;
                            '?':
                               begin
                                  inc(currentpos);
                                  new(hp);
                                  { this is a dummy }
                                  new(hp2);
                                  doregister(hp);
                                  doregister(hp2);
                                  hp^.typ:=ret_charset;
                                  hp^.chars:=cs;
                                  hp^.next:=hp2;
                                  hp^.elsepath:=hp2;
                                  hp2^.typ:=ret_charset;
                                  hp2^.chars:=[];
                                  hp2^.elsepath:=next;
                                  if assigned(chaining) then
                                    chaining^:=hp
                                  else
                                    parseregexpr:=hp;
                                  chaining:=@hp2^.elsepath;
                               end;
                            else
                               begin
                                  new(hp);
                                  doregister(hp);
                                  hp^.typ:=ret_charset;
                                  hp^.chars:=cs;
                                  hp^.elsepath:=elsepath;
                                  hp^.next:=next;
                                  if assigned(chaining) then
                                    chaining^:=hp
                                  else
                                    parseregexpr:=hp;
                                  chaining:=@hp^.next;
                               end;
                         end;
                      end;
                 end;
              end;
         end;

       var
          endp : pregexprentry;

       begin
          GenerateRegExprEngine.Data:=nil;
          GenerateRegExprEngine.DestroyList:=nil;
          if regexpr=nil then
            exit;
          first:=nil;
          if (ref_singleline in flags) and (ref_multiline in flags) then
            exit;
          currentpos:=regexpr;
          error:=false;
          new(endp);
          doregister(endp);
          endp^.typ:=ret_illegalend;
          GenerateRegExprEngine.flags:=flags;
          GenerateRegExprEngine.Data:=parseregexpr(nil,endp);
          GenerateRegExprEngine.DestroyList:=first;
          if error or (currentpos^<>#0) then
            DestroyRegExprEngine(Result);
       end;

    procedure DestroyRegExprEngine(var regexpr : TRegExprEngine);

       var
          hp : pregexprentry;

       begin
          hp:=regexpr.DestroyList;
          while assigned(hp) do
            begin
               regexpr.DestroyList:=hp^.nextdestroy;
               dispose(hp);
               hp:=regexpr.DestroyList;
            end;
          regexpr.Data:=nil;
          regexpr.DestroyList:=nil;
       end;

     function RegExprPos(regexprengine : TRegExprEngine;p : pchar;var index,len : longint) : boolean;

       var
          lastpos : pchar;

       function dosearch(regexpr : pregexprentry;pos : pchar) : boolean;

         begin
            dosearch:=false;
            while true do
              begin
                 {$IFDEF Debug}
                 writeln(byte(regexpr^.typ));
                 {$ENDIF Debug}
                 case regexpr^.typ of
                    ret_endline:
                      begin
                         if ref_multiline in regexprengine.flags then
                           begin
                              if ((pos+1)^ in [#10,#0]) then
                                regexpr:=regexpr^.next
                              else
                                regexpr:=regexpr^.elsepath;
                           end
                         else
                           begin
                              if (pos+1)^=#0 then
                                regexpr:=regexpr^.next
                              else
                                regexpr:=regexpr^.elsepath;
                           end;
                      end;
                    ret_startline:
                      begin
                         if ref_multiline in regexprengine.flags then
                           begin
                              if (pos=p) or ((pos-1)^=#10) then
                                regexpr:=regexpr^.next
                              else
                                regexpr:=regexpr^.elsepath;
                           end
                         else
                           begin
                              if pos=p then
                                regexpr:=regexpr^.next
                              else
                                regexpr:=regexpr^.elsepath;
                           end;
                      end;
                    ret_charset:
                      begin
                         if (pos^ in regexpr^.chars) or
                           ((ref_caseinsensitive in regexprengine.flags) and
                            (upcase(pos^) in regexpr^.chars)) then
                           begin
{$ifdef DEBUG}
                              writeln('Found matching: ',pos^);
{$endif DEBUG}
                              regexpr:=regexpr^.next;
                              inc(pos);
                           end
                         else
                           begin
{$ifdef DEBUG}
                              writeln('Found unmatching: ',pos^);
{$endif DEBUG}
                              regexpr:=regexpr^.elsepath;
                           end;
                      end;
                    ret_backtrace:
                      begin
{$ifdef DEBUG}
                         writeln('Starting backtrace');
{$endif DEBUG}
                         if dosearch(regexpr^.next,pos) then
                           begin
                              dosearch:=true;
                              exit;
                           end
                         else if dosearch(regexpr^.elsepath,pos) then
                           begin
                              dosearch:=true;
                              exit;
                           end
                         else
                           exit;
                      end;
                 end;
                 lastpos:=pos;
                 if regexpr=nil then
                   begin
                      dosearch:=true;
                      exit;
                   end;
                 if regexpr^.typ=ret_illegalend then
                   exit;
                 if pos^=#0 then
                   exit;
              end;
         end;

       begin
          RegExprPos:=false;
          index:=0;
          len:=0;
          if regexprengine.Data=nil then
            exit;
          while p^<>#0 do
            begin
               if dosearch(regexprengine.Data,p) then
                 begin
                    len:=lastpos-p;
                    RegExprPos:=true;
                    exit;
                 end
               else
                 begin
                    inc(p);
                    inc(index);
                 end;
            end;
          index:=-1;
       end;

begin
   cs_nonwordchars:=cs_allchars-cs_wordchars;
   cs_nondigits:=cs_allchars-cs_digits;
   cs_nonwhitespace:=cs_allchars-cs_whitespace;
end.
