{
    This program is part of the Free Pascal run time library.
    Copyright (c) 1998-2002 by Peter Vreman

    Show the differences between two .msg files

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ May be we need to compare a prefixes of option_help_pages too?
  Currently this is not performed }

Program messagedif;

{$h+} {Huge strings}

Uses
  Strings;

Type
  TEnum = String;
  TText = String;

  PMsg = ^TMsg;
  TMsg = Record
     Line, ctxt, cnb : Longint;
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
  Auto_verbosity : boolean = false;


Procedure GetTranslation( p : PMsg);
var
   s : string;
   i,j,k : longint;
begin
  i:=pos('_',p^.text);
  if i>0 then
    for j:=i+1 to Length(p^.text) do
      if p^.text[j]='_' then
        begin
          i:=j;
          break;
        end;
  if (i>0) and (i<=15) then
      Writeln(P^.Enum,' type  "',copy(p^.text,1,i-1),'" "',copy(p^.text,i+1,255),'"')
  else
    Writeln(P^.enum,' "',p^.text,'"');
  Writeln('Type translated error message in,');
  Writeln('Press return to keep it unchanged, or "q" to finish interactive mode');
  Readln(s);
  if s='' then
    exit;
  if s='q' then
    begin
      Is_interactive:=false;
      exit;
    end;
  j:=pos('_',s);
  if j>0 then
    for k:=j+1 to Length(s) do
      if s[j]='_' then
        begin
          j:=k;
          break;
        end;
  if (j>0) then
    begin
      if copy(p^.text,1,i)<>copy(s,1,j) then
        Writeln('Warning : different verbosity !!');
      p^.text:=s;
    end
  else
    p^.text:=copy(p^.text,1,i)+s;
end;

Function NewMsg (Var RM : PMsg; L : Longint; Const E : TEnum;Const T : TText;C : pchar;NbLn,TxtLn : longint) : PMsg;

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
    ctxt:=TxtLn;
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
  Writeln('Usage : msgdif [options] <org-file> <dif-file>');
  Writeln('Options:');
  Writeln('   -i    allow to enter translated messages interactively');
  Writeln('   -y1   use <org-file> verbosity (do not query acknowledge)');
  Writeln('');
  Writeln('Generates "',NewFileName,'" that contain the messages from <dif-file>');
  Writeln('with a new messages from <org-file>');
  Writeln('');
  Writeln('Example:');
  Writeln('  msgdif errore.msg errorr.msg');
  halt(1)
end;

Procedure ProcessOptions;
var
  i,count : longint;
begin
  Is_interactive:=false;
  Auto_verbosity:=false;

  count:=paramcount; i:=1;
  while (count>0) and (Paramstr(i)[1]='-') do
   case UpCase(Paramstr(i)[2]) of
     'I': begin
            Is_interactive:=true;
            dec(count); Inc(i);
          end;
     'Y': case Paramstr(i)[3] of
            '1': begin
                   Auto_verbosity:=true;
                   dec(count); Inc(i);
                 end;
          else
            Writeln ('Error: unknown option ', Paramstr(i));
            Usage;
          end;
   else
     Writeln ('Error: unknown option ', Paramstr(i));
     Usage;
   end;
  If Count<>2 then begin
    Writeln ('Error: there must be exactly two message files');
    Usage;
  end;

  OrgfileName:=Paramstr(i);
  DiffFileName:=Paramstr(i+1);
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
    J,LineNo,Count,NbLn,TxtLn : Longint;
    chararray : array[0..ArrayLength] of char;
    currentindex : longint;
    c : pchar;
    multiline : boolean;
begin
  Assign(F,FileName);
  Reset(F);
  Write ('Processing: ',Filename,'...');
  LineNo:=0;
  NbLn:=0;
  TxtLn:=0;
  Count:=0;
  currentindex:=0;
  Root:=Nil;
  First:=nil;
  Last:=nil;
  PrevS:='';
  multiline:=false;
  While not eof(f) do
    begin
    Readln(F,S);
    Inc(LineNo);
    If multiline then
      begin
        PrevS:=PrevS+#10+S; Inc(TxtLn);
        if (Length(S)<>0) and (S[1]=']') then
          multiline:=false;
      end
    else
    if (length(S)>0) and Not (S[1] in ['%','#']) Then
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
           Copy(PrevS,Pos('=',PrevS)+1,Length(PrevS)),c,NbLn,TxtLn)
        else
          StrDispose(c);
        currentindex:=0;
        NbLn:=0; TxtLn:=0;
        PrevS:=S; Inc(TxtLn);
        if S[j+7]='[' then multiline:=true;
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
     Copy(PrevS,Pos('=',PrevS)+1,Length(PrevS)),c,NbLn,TxtLn);
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

type TArgSet = set of 0..31;

function MsgToSet(const Msg, FileName: string; var R: TArgSet): Boolean;
  var
    i, j,l, num : integer;
    code : word;
  begin
    R:=[];
    MsgToSet:=false;
    for i:=1 to Length(Msg) do
      if Msg[i]='$' then
      begin
        j:=i+1; l:=length(msg)+1;
        while (j<l) and (Msg[j] in ['0'..'9']) do Inc(j);
        if j = l then
        begin
          val(copy(Msg,i+1,j-i-1),num,code);
          if num > high(TArgSet) then begin
            WriteLn('Error in ', FileName,': ', Msg);
            WriteLn(' number at position ', i);
            WriteLn(' must be LE ', high(TArgSet));
            Exit;
          end;
          R:=R+[num];
        end;
      end;
      MsgToSet:=true;
  end;


procedure CheckParm(const s1, s2: string);
  var
    R1, R2: TArgSet;
  begin
    if MsgToSet(s1,OrgFileName, R1) <> true then Exit;
    if MsgToSet(s2,DiffFileName,R2) <> true then Exit;
    if R1<>R2 then begin
      WriteLn('Error: set of arguments is different');
      WriteLn(' ',s1);
      WriteLn(' ',s2);
    end;
  end;

procedure WriteReorderedFile(FileName : string;orgnext,diffnext : PMsg);
  var t,t2,t3 : text;
      i,ntcount : longint;
      j : integer;
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
             inc(i,orgnext^.ctxt);
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
             j:=pos('_',copy(s2,7,20)) + 6;
             s2:=upcase(copy(s2,1,j));
             s3:=orgnext^.equivalent^.text;
             j:=pos('_',copy(s3,7,20)) + 6;
             s3:=upcase(copy(s3,1,j));
             { that are the conditions in verbose unit }
             if (length(s3)<12) and (s2<>s3) then
               begin
                 Writeln('Warning: different options for ',orgnext^.enum);
                 Writeln(' ',orgnext^.text);
                 Writeln(' ',orgnext^.equivalent^.text);
                 s:='N';
                 if Auto_verbosity then
                   s:='Y'
                 else
                 If Is_interactive then
                   begin
                     Write('Use ',s2,' verbosity ? [y/n] ');
                     Readln(s);
                   end;
                 if UpCase(s[1])='Y' then
                   begin
                     orgnext^.equivalent^.text:=s2+copy(orgnext^.equivalent^.text,
                       length(s3)+1,Length(orgnext^.equivalent^.text));
                     WriteLn(' Using ', s2);
                   end;
               end;

             CheckParm(orgnext^.text, orgnext^.equivalent^.text);

             Writeln(t,orgnext^.enum,'=',orgnext^.equivalent^.text);
             Dec(i); Inc(i,orgnext^.equivalent^.ctxt);
             if assigned(orgnext^.equivalent^.comment) and
               (strlen(orgnext^.equivalent^.comment)>0) then
             begin
               Write(t,orgnext^.equivalent^.comment);
               inc(i,orgnext^.equivalent^.cnb);
             end
             else if assigned(orgnext^.comment) and
               (strlen(orgnext^.comment)>0) then
               begin
                 Writeln('Comment from ',OrgFileName,' for enum ',orgnext^.enum,' added');
                 Write(t,orgnext^.comment);
                 inc(i,orgnext^.cnb);
               end;
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
              inc(i,diffnext^.ctxt);
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
  PrintList('org.lst',OrgRoot);
  PrintList('diff.lst',DiffRoot);
  ShowDiff (OrgRoot,DiffRoot);
  WriteReorderedFile(NewFileName,orgfirst,difffirst);
end.
