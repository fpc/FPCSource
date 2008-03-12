{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 by Giulio Bernardi

    Internal class to support merging of resources

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit resmerger;

{$MODE OBJFPC}{$H+}

interface

uses
  Classes, SysUtils, resource,
  stringtableresource,

  //these two resource types know how to do it on their own... add them so that
  //they got registered
  groupiconresource, groupcursorresource;

type

  { TResourceMerger }

  TResourceMerger = class
  private
  protected
  public
    class function Merge(aRes1, aRes2 : TAbstractResource) : boolean;
  end;

implementation

{ TResourceMerger }

class function TResourceMerger.Merge(aRes1, aRes2: TAbstractResource): boolean;
var i : integer;
    sRes1, sRes2 : TStringTableResource;
begin
  Result:=false;
  if not (aRes1 is TStringTableResource) then exit;
  sRes1:=TStringTableResource(aRes1);
  sRes2:=TStringTableResource(aRes2);
  //check if merging is possible...
  for i:=sRes1.FirstID to sRes1.LastID do
    if (sRes1[i]<>'') and (sRes2[i]<>'') then exit; //if both are used, no merging is possible
  //ok, merge
  for i:=sRes1.FirstID to sRes1.LastID do
    if sRes2[i]<>'' then sRes1[i]:=sRes2[i];
  Result:=true;
end;

end.
