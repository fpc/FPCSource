{
    $Id$
    This program is part of the Free Pascal run time library.
    Copyright (c) 1998-2000 by Peter Vreman

    Show the differences between two .msg files

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
Program messagedif;

Uses
  Strings;

Type
  TEnum = String;
  TText = String;

  PMsg = ^TMsg;
  TMsg = Record
     Line,cnb : Longint;
     enum : TEnum;
     text : TText;
     comment : pchar;
     Next,Prev : PMsg;
     FileNext,
     Equivalent : PMsg;
   end;
Var
  OrgFileName,DiffFileName : String;
  OrgRoot,DiffRoot : PMsg;
  OrgFirst,DiffFirst : PMsg;
  Last : PMsg;

const
  NewFileName = 'new.msg';
  Is_interactive : boolean = false;

Procedure GetTranslation( p : PMsg);
var
   s : string;
   i,j : longint;
begin
  i:=pos('_',p^.text);
  if (i>0) and (i<=5) then
      Writeln(P^.Enum,' type  "',copy(p^.text,1,i-1),'" "',copy(p^.text,i+1,255),'"')
  else
    Writeln(P^.enum,' "',p^.text,'"');
  Readln(s);
  if s='' then
    begin
      Is_interactive:=false;
      exit;
    end;
  j:=pos('_',s);
  if (j>0) and (j<=5) then
    begin
      if copy(p^.text,1,i)<>copy(s,1,j) then
        Writeln('Different verbosity !!');
      p^.text:=s;
    end
  else
    p^.text:=copy(p^.text,1,i)+s;
end;

Function NewMsg (Var RM : PMsg; L : Longint; Const E : TEnum;Const T : TText;C : pchar;NbLn : longint) : PMsg;

Var
  P,R : PMsg;

begin
  New(P);
  with P^ do
    begin
    Line:=L;
    Text:=T;
    enum:=E;
    comment:=c;
    cnb:=NbLn;
    next:=Nil;
    prev:=Nil;
    filenext:=nil;
    equivalent:=nil;
    if assigned(last) then
      last^.FileNext:=P;
    last:=P;
    end;
  R:=RM;
  While (R<>Nil) and (UpCase(R^.enum)>UpCase(P^.Enum)) do
    begin
      P^.Prev:=R;
      R:=R^.next;
    end;
  if assigned(R) and (UpCase(R^.Enum)=UpCase(P^.Enum)) then
    Writeln('Error ',R^.Enum,' duplicate');
  P^.Next:=R;
  If R<>Nil then
    R^.Prev:=P;
  If P^.Prev<>Nil then
    P^.Prev^.Next:=P
  else
    RM:=P;
  NewMsg:=P;
end;

Procedure PrintList(const name : string;R : PMsg);
var
  P : PMsg;
  f : text;
begin
  P:=R;
  Assign(f,name);
  Rewrite(f);
  while assigned(P) do
    begin
      Writeln(f,UpCase(P^.Enum));
      P:=P^.Next;
    end;
  Close(f);
end;

Procedure Usage;

begin
  Writeln ('Usage : msgdif [-i] orgfile diffile');
  Writeln(' optional -i option allows to enter translated messages interactivly');
  Writeln('Generates ',NewFileName,' with updated messages');
  halt(1)
end;

Procedure ProcessOptions;
var
  i,count : longint;
begin
  count:=paramcount;
  if (count>0) and (UpCase(Paramstr(1))='-I') then
    begin
      dec(count);
      i:=1;
      Is_interactive:=true;
    end
  else
    begin
      i:=0;
      Is_interactive:=false;
    end;
  If Count<>2 then
    Usage;
  OrgfileName:=Paramstr(i+1);
  DiffFileName:=Paramstr(i+2);
  if (OrgFileName=NewFileName) or (DiffFileName=NewFileName) then
    begin
      Writeln('The file names must be different from ',NewFileName);
      Halt(1);
    end;
end;

Procedure ProcessFile (FileName : String; Var Root,First : PMsg);

Const
    ArrayLength = 65500;
Var F : Text;
    S,prevS : String;
    J,LineNo,Count,NbLn : Longint;
    chararray : array[0..ArrayLength] of char;
    currentindex : longint;
    c : pchar;
begin
  Assign(F,FileName);
  Reset(F);
  Write ('Processing: ',Filename,'...');
  LineNo:=0;
  NbLn:=0;
  Count:=0;
  currentindex:=0;
  Root:=Nil;
  First:=nil;
  Last:=nil;
  PrevS:='';
  While not eof(f) do
    begin
    Readln(F,S);
    Inc(LineNo);
    If (length(S)>0) and Not (S[1] in ['%','#']) Then
      begin
      J:=Pos('=',S);
      If j<1 then
        writeln (Filename,'(',LineNo,') : Invalid entry')
      else
        begin
        chararray[currentindex]:=#0;
        c:=strnew(@chararray);
        if PrevS<>'' then
          NewMsg(Root,LineNo,Copy(PrevS,1,Pos('=',PrevS)-1),
           Copy(PrevS,Pos('=',PrevS)+1,255),c,NbLn);
        currentindex:=0;
        NbLn:=0;
        PrevS:=S;
        if First=nil then
          First:=Root;
        Inc(Count);
        end;
      end
    else
      begin
        if currentindex+length(s)+1>ArrayLength then
          Writeln('Comment too long : over ',ArrayLength,' chars')
        else
          begin
            strpcopy(@chararray[currentindex],s+#10);
            inc(currentindex,length(s)+1);
            inc(NbLn);
          end;
      end;
    end;
  chararray[currentindex]:=#0;
  c:=strnew(@chararray);
  if PrevS<>'' then
    NewMsg(Root,LineNo,Copy(PrevS,1,Pos('=',PrevS)-1),
     Copy(PrevS,Pos('=',PrevS)+1,255),c,NbLn);
  Writeln (' Done. Read ',LineNo,' lines, got ',Count,' constants.');
  Close(f);
end;

Procedure ShowDiff (POrg,PDiff : PMsg);

Var
  count,orgcount,diffcount : longint;

Procedure NotFound (Org : Boolean; P : PMsg);

begin
  With P^ do
    If Org Then
      Writeln ('Not found in ',DiffFileName,' : ',Enum,' ',OrgFileName,'(',Line,')')
    else
      Writeln ('Extra in ',DiffFileName,'(',line,') : ',enum);
  if org then
    inc(orgcount)
  else
    inc(diffcount);
end;

begin
  orgcount:=0;
  diffcount:=0;
  count:=0;
  While (Porg<>Nil) and (PDiff<>Nil) do
    begin
//    Writeln (POrg^.enum,'<=>',PDiff^.Enum);
    If UpCase(Porg^.Enum)>UpCase(PDiff^.Enum) then
      begin
      NotFound (True,Porg);
      POrg:=POrg^.Next
      end
    else If UpCase(POrg^.enum)=UpCase(PDiff^.Enum)  then
      begin
      inc(count);
      POrg^.Equivalent:=PDiff;
      PDiff^.Equivalent:=POrg;
      POrg:=POrg^.Next;
      PDiff:=PDiff^.Next;
      end
    else
      begin
      NotFound (False,PDiff);
      PDiff:=PDiff^.Next
      end;
    end;
   While POrg<>Nil do
     begin
     NotFound(True,Porg);
     POrg:=pOrg^.Next;
     end;
   While PDiff<>Nil do
     begin
     NotFound(False,PDiff);
     PDiff:=PDiff^.Next;
     end;
   Writeln(count,' messages found in common to both files');
   Writeln(orgcount,' messages only in ',OrgFileName);
   Writeln(diffcount,' messages only in ',DiffFileName);
end;

procedure WriteReorderedFile(FileName : string;orgnext,diffnext : PMsg);
  var t,t2,t3 : text;
      i,ntcount : longint;
      s,s2,s3 : string;
      is_msg : boolean;
      nextdiffkept : pmsg;
  begin
     ntcount:=0;
     Assign(t,FileName);
     Rewrite(t);
     Writeln(t,'%%% Reordering of ',DiffFileName,' respective to ',OrgFileName);
     Writeln(t,'%%% Contains all comments from ',DiffFileName);
     Assign(t2,DiffFileName);
     Reset(t2);
     Assign(t3,OrgFileName);
     Reset(t3);
     i:=2;
     s:='';s3:='';
     nextdiffkept:=diffnext;
     while assigned(nextdiffkept) and (nextdiffkept^.equivalent=nil) do
       nextdiffkept:=nextdiffkept^.filenext;
     { First write the header of diff }
     repeat
       Readln(t2,s);
       is_msg:=(pos('=',s)>1) and (s[1]<>'%') and (s[1]<>'#');
       if not is_msg then
         begin
           Writeln(t,s);
           inc(i);
         end;
     until is_msg;
     { Write all messages in Org order }
     while assigned(orgnext) do
       begin
         if not assigned(orgnext^.equivalent) then
           begin
             { Insert a new error msg with the english comments }
             Writeln('New error ',orgnext^.enum,' added');
             If Is_interactive then
               GetTranslation(orgnext);
             Writeln(t,orgnext^.enum,'=',orgnext^.text);
             inc(i);
             Write(t,orgnext^.comment);
             inc(i,orgnext^.cnb);
           end
         else
           begin
             inc(i);
             if orgnext^.text=orgnext^.equivalent^.text then
               begin
                 Writeln(FileName,'(',i,') ',orgnext^.enum,' not translated');
                 If Is_interactive then
                   GetTranslation(orgnext^.equivalent);
                 if orgnext^.text=orgnext^.equivalent^.text then
                   inc(ntcount);
               end;
             s2:=orgnext^.text;
             s2:=upcase(copy(s2,1,pos('_',s2)));
             s3:=orgnext^.equivalent^.text;
             s3:=upcase(copy(s3,1,pos('_',s3)));
             { that are the conditions in verbose unit }
             if (length(s3)<5) and (s2<>s3) then
               begin
                 Writeln('Warning: different options for ',orgnext^.enum);
                 Writeln('in ',orgFileName,' : ',s2);
                 Writeln('in ',diffFileName,' : ',s3);
                 If Is_interactive then
                   begin
                     Write('Use ',OrgFileName,' verbosity ? [y/n] ');
                     Readln(s);
                     if UpCase(s)<>'N' then
                       orgnext^.equivalent^.text:=s2+copy(orgnext^.equivalent^.text,
                         length(s3)+1,255);
                   end;
               end;

             Writeln(t,orgnext^.enum,'=',orgnext^.equivalent^.text);
             if assigned(orgnext^.equivalent^.comment) and
               (strlen(orgnext^.equivalent^.comment)>0) then
               Write(t,orgnext^.equivalent^.comment)
             else if assigned(orgnext^.comment) and
               (strlen(orgnext^.comment)>0) then
               begin
                 Writeln('Comment from ',OrgFileName,' for enum ',orgnext^.enum,' added');
                 Write(t,orgnext^.comment);
               end;
             inc(i,orgnext^.equivalent^.cnb);
           end;
         orgnext:=orgnext^.filenext;
       end;

     while assigned(diffnext) do
       begin
         if not assigned(diffnext^.Equivalent) then
           begin
              { Skip removed enum in errore.msg}
              { maybe a renaming of an enum !}
              Writeln(diffnext^.enum,' commented out');
              Writeln(t,'%%% ',diffnext^.enum,'=',diffnext^.text);
              inc(i);
              Write(t,diffnext^.comment);
              inc(i,diffnext^.cnb);
           end;
         diffnext:=diffnext^.filenext;
       end;
     Close(t);
     Close(t2);
     Close(t3);
     Writeln(ntcount,' not translated items found');
  end;

begin
  ProcessOptions;
  ProcessFile(OrgFileName,orgroot,orgfirst);
  ProcessFile(DiffFileName,diffRoot,difffirst);
  PrintList('Org.lst',OrgRoot);
  PrintList('Diff.lst',DiffRoot);
  ShowDiff (OrgRoot,DiffRoot);
  WriteReorderedFile(NewFileName,orgfirst,difffirst);
end.
{
  $Log$
  Revision 1.12  2000-05-12 15:03:44  pierre
   + interactive mode for translation

  Revision 1.11  2000/05/12 08:47:25  pierre
    + add a warning if the error level is different in the two files
    + force to keep the order of orgfile

  Revision 1.10  2000/05/11 13:37:37  pierre
   * ordering bugs fixed

  Revision 1.9  2000/02/09 13:23:11  peter
    * log truncated

  Revision 1.8  2000/01/07 01:15:01  peter
    * updated copyright to 2000

}