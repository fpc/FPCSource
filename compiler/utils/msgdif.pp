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

Type
  TEnum = String;
  TText = String;

  PMsg = ^TMsg;
  TMsg = Record
     Line : Longint;
     enum : TEnum;
     text : TText;
     Next,Prev : PMsg;
     FileNext,
     Equivalent : PMsg;
   end;
Var
  OrgFileName,DiffFileName : String;
  OrgRoot,DiffRoot : PMsg;
  OrgFirst,DiffFirst : PMsg;
  Last : PMsg;

Function NewMsg (Var RM : PMsg; L : Longint; Const E : TEnum;Const T : TText) : PMsg;

Var
  P,R : PMsg;

begin
  New(P);
  with P^ do
    begin
    Line:=L;
    Text:=T;
    enum:=E;
    next:=Nil;
    prev:=Nil;
    filenext:=nil;
    equivalent:=nil;
    if assigned(last) then
      last^.FileNext:=P;
    last:=P;
    end;
  R:=RM;
  While (R<>Nil) and (R^.enum<P^.Enum) do
    begin
    P^.Prev:=R;
    R:=R^.next;
    end;
  P^.Next:=R;
  If R<>Nil then
    R^.Prev:=P;
  If P^.Prev<>Nil then
    P^.Prev^.Next:=P
  else
    RM:=P;
  NewMsg:=P;
end;

Procedure Usage;

begin
  Writeln ('Usage : msgdif orgfile diffile');
  halt(1)
end;

Procedure ProcessOptions;

begin
  If ParamCount<>2 then
    Usage;
  OrgfileName:=Paramstr(1);
  DiffFileName:=Paramstr(2);
end;

Procedure ProcessFile (FileName : String; Var Root,First : PMsg);

Var F : Text;
    S : String;
    J,LineNo,Count : Longint;

begin
  Assign(F,FileName);
  Reset(F);
  Write ('Processing: ',Filename,'...');
  LineNo:=0;
  Count:=0;
  Root:=Nil;
  First:=nil;
  Last:=nil;
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
        NewMsg(Root,LineNo,Copy(S,1,J-1),Copy(S,j+1,255));
        if First=nil then
          First:=Root;
        Inc(Count);
        end;
      end;
    end;
  Writeln (' Done. Read ',LineNo,' lines, got ',Count,' constants.');
  Close(f);
end;

Procedure ShowDiff (POrg,PDiff : PMsg);

Procedure NotFound (Org : Boolean; P : PMsg);

begin
  With P^ do
    If Org Then
      Writeln ('Not found in ',DiffFileName,' : ',Enum,' ',OrgFileName,'(',Line,')')
    else
      Writeln ('Extra in ',DiffFileName,'(',line,') : ',enum)
end;

Var P : PMsg;
    count : longint;
begin
  count:=0;
  While (Porg<>Nil) and (PDiff<>Nil) do
    begin
//    Writeln (POrg^.enum,'<=>',PDiff^.Enum);
    If Porg^.Enum<PDiff^.Enum then
      begin
      NotFound (True,Porg);
      POrg:=POrg^.Next
      end
    else If POrg^.enum=PDiff^.Enum  then
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
   Writeln(count,' messages found in both files');
end;

procedure WriteReorderedFile(FileName : string;orgnext,diffnext : PMsg);
  var t,t2,t3 : text;
      i,i2,i3 : longint;
      s,s3 : string;
      CurrentMsg : PMsg;
      nextdiffkept : pmsg;
  begin
     Assign(t,FileName);
     Rewrite(t);
     Writeln(t,'%%% Reordering of ',DiffFileName,' respective to ',OrgFileName);
     Writeln(t,'%%% Contains all comments from ',DiffFileName);
     Assign(t2,DiffFileName);
     Reset(t2);
     Assign(t3,OrgFileName);
     Reset(t3);
     i:=2;i2:=0;i3:=0;
     s:='';s3:='';
     nextdiffkept:=diffnext;
     while assigned(nextdiffkept) and (nextdiffkept^.equivalent=nil) do
       nextdiffkept:=nextdiffkept^.filenext;
     While not eof(t2) do
       begin
          while assigned(orgnext) and assigned(nextdiffkept) and
             (orgnext^.enum<>nextdiffkept^.enum) and not(eof(t3)) do
             begin
                { Insert a new error msg with the english comments }
                while i3<orgnext^.line do
                  begin
                     readln(t3,s3);
                     inc(i3);
                  end;
                     writeln(t,s3);
                     inc(i);
                     readln(t3,s3);
                     inc(i3);
                while (s3<>'') and (s3[1] in ['#','%']) do
                  begin
                     writeln(t,s3);
                     inc(i);
                     readln(t3,s3);
                     inc(i3);
                  end;
                Writeln('New error ',orgnext^.enum,' added');
                orgnext:=orgnext^.filenext;
             end;
          if s='' then
            begin
               readln(t2,s);
               inc(i2);
            end;
          if assigned(orgnext) and
             assigned(diffnext) and (i2=diffnext^.line) then
            begin
               if assigned(diffnext^.Equivalent) then
                 begin
                    if diffnext^.equivalent<>orgnext then
                      Writeln('Problem inside WriteReorderedFile');
                    Writeln(t,s);
                    s:='';
                    inc(i);
                    readln(t2,s);
                    inc(i2);
                    while (s<>'') and (s[1] in ['#','%']) do
                      begin
                         writeln(t,s);
                         inc(i);
                         readln(t2,s);
                         inc(i2);
                      end;
                    if diffnext^.Equivalent^.Text=diffnext^.Text then
                      Writeln(diffnext^.Enum,': ',DiffFileName,'(',i2,') not translated');
                    Diffnext:=Diffnext^.FileNext;
                    nextdiffkept:=diffnext;
                    while assigned(nextdiffkept) and (nextdiffkept^.equivalent=nil) do
                      nextdiffkept:=nextdiffkept^.filenext;
                    Orgnext:=orgnext^.filenext;
                 end
               else
                 begin
                    { Skip removed enum in errore.msg}
                    { maybe a renaming of an enum !}
                    Writeln(diffnext^.enum,' commented out');
                    Writeln(t,'%%% ',s);
                    inc(i);
                    readln(t2,s);
                    inc(i2);
                    Diffnext:=Diffnext^.FileNext;
                    nextdiffkept:=diffnext;
                    while assigned(nextdiffkept) and (nextdiffkept^.equivalent=nil) do
                      nextdiffkept:=nextdiffkept^.filenext;
                    if assigned(diffnext) then
                      while (i2<diffnext^.line) do
                        begin
                           writeln(t,'%%% ',s);
                           inc(i);
                           readln(t2,s);
                           inc(i2);
                        end;
                 end;
            end
          else
            begin
               writeln(t,s);
               inc(i);
               s:='';
            end;
       end;
     Close(t);
     Close(t2);
     Close(t3);
  end;

begin
  ProcessOptions;
  ProcessFile(OrgFileName,orgroot,orgfirst);
  ProcessFile(DiffFileName,diffRoot,difffirst);
  ShowDiff (OrgRoot,DiffRoot);
  WriteReorderedFile('new.msg',orgfirst,difffirst);
end.
{
  $Log$
  Revision 1.9  2000-02-09 13:23:11  peter
    * log truncated

  Revision 1.8  2000/01/07 01:15:01  peter
    * updated copyright to 2000

}
