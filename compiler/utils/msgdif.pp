{
    $Id$
    This program is part of the Free Pascal run time library.
    Copyright (c) 1998 by Peter Vreman

    Show the differences between two .msg files

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
Program messagedif;

Type
  TEnum = String[40];
  PMsg = ^TMsg;
  TMsg = Record
     Line : Longint;
     enum : TEnum;
     Next,Prev : PMsg;
   end;
Var
  OrgFileName,DiffFileName : String;
  OrgRoot,DiffRoot : PMsg;

Function NewMsg (Var RM : PMsg; L : Longint; Const E : TEnum) : PMsg;

Var
  P,R : PMsg;

begin
  New(P);
  with P^ do
    begin
    Line:=L;
    enum:=E;
    next:=Nil;
    prev:=Nil;
    end;
  If RM=NIl then
    RM:=P
  else
    begin
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
        P^.Prev^.Next:=P;
    end;
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

Procedure ProcessFile (FileName : String; Var Root : PMsg);

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
        NewMsg(Root,LineNo,Copy(S,1,J-1));
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
      Writeln ('Not found in new : ',Enum,' (line ',Line,' in ',OrgFilename,')')
    else
      Writeln ('Extra in new : ',enum,' (Line',line,' in ',DiffFileName,')')
end;

Var P : PMsg;

begin
  While (Porg<>Nil) and (PDiff<>Nil) do
    If Porg^.Enum<PDiff^.Enum then
      begin
      NotFound (True,Porg);
      POrg:=POrg^.Next
      end
    else If POrg^.enum=PDiff^.Enum  then
      begin
      POrg:=POrg^.Next;
      PDiff:=PDiff^.Next
      end
    else
      begin
      NotFound (False,PDiff);
      PDiff:=PDiff^.Next
      end;
   While POrg<>Nil do
     begin
     NotFound(True,Porg);
     POrg:=pOrg^.Next;
     end;
   While PDiff<>Nil do
     begin
     NotFound(True,PDiff);
     PDiff:=PDiff^.Next;
     end;
end;

begin
  ProcessOptions;
  ProcessFile(OrgFileName,orgroot);
  ProcessFile(DiffFileName,diffRoot);
  ShowDiff (OrgRoot,DiffRoot);
end.
{
  $Log$
  Revision 1.1  1999-05-12 16:17:09  peter
    * init

}