{
    $Id$
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1998 by the Free Pascal development team

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
  Revision 1.5  2000-01-06 01:20:33  peter
    * moved out of packages/ back to topdir

  Revision 1.1  2000/01/03 19:33:08  peter
    * moved to packages dir

  Revision 1.3  1998/10/02 22:41:29  michael
  + Added exceptions for error handling

  Revision 1.2  1998/05/27 12:22:14  michael
  + Implemented TPersistent

  Revision 1.1  1998/05/04 14:30:12  michael
  * Split file according to Class; implemented dummys for all methods, so unit compiles.

}
