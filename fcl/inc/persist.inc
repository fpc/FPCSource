{
    $Id$
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{****************************************************************************}
{*                             TPersistent                                  *}
{****************************************************************************}


procedure TPersistent.AssignError(Source: TPersistent);

Var SourceName : String;

begin
  If Source<>Nil then
    SourceName:=Source.ClassName
  else
    SourceName:='Nil';
  Writeln ('Error assigning to ',ClassName,' from : ',SourceName);
  raise EConvertError.CreateFmt (SAssignError,[SourceName,ClassName]);
end;



procedure TPersistent.AssignTo(Dest: TPersistent);


begin
  Dest.AssignError(Self);
end;


procedure TPersistent.DefineProperties(Filer: TFiler);

begin
end;


function  TPersistent.GetOwner: TPersistent;

begin
  Result:=Nil;
end;

destructor TPersistent.Destroy;

begin
  Inherited Destroy;
end;


procedure TPersistent.Assign(Source: TPersistent);

begin
  If Source<>Nil then
    Source.AssignTo(Self)
  else
    AssignError(Nil);
end;

function  TPersistent.GetNamePath: string;

Var OwnerName :String;

begin
 Result:=ClassNAme;
 If GetOwner<>Nil then
   begin
   OwnerName:=GetOwner.GetNamePath;
   If OwnerName<>'' then Result:=OwnerName+'.'+Result;
   end;
end;
{
  $Log$
  Revision 1.2  2000-07-13 11:32:59  michael
  + removed logs
 
}
