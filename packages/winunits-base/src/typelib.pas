unit typelib;

{$mode objfpc}{$H+}

{ Typelib import routines.

  Creates freepascal bindings for COM objects stored in .tlb, .dll, .exe or .olb files.

  Copyright (C) 2011 Ludo Brands

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

interface

uses
  Classes, SysUtils,comobj,activex,windows;

{
Reads type information from 'FileName' and converts it in a freepascal binding unit. The
contents of the unit is returned as the function result.
Returns in 'sUnitName' the unit name with '.pas' extension.
Appends to 'slDependencies' the filenames of the additional typelibs needed.
If bActiveX is true an ActiveXContainer descendant is created with the evenual OnEvent hooks
If bActiveX is false and an event source is found an TEventSink descendant is created with the OnEvent hooks

By default, the type library is extracted from the first resource of type ITypeLib.
To load a different type of library resource, append an integer index to 'FileName'.

Example:  C:\WINDOWS\system32\msvbvm60.dll\3
}
function ImportTypelib(FileName: WideString;var sUnitName:string;var slDependencies:TStringList;
  bActiveX,bPackage,bRemoveStructTag:boolean;var sPackageSource,sPackageRegUnitSource:String):string;


Type

  { TTypeLibImporter }

  TTypeLibImporter = Class(TComponent)
  private
    FActiveX: Boolean;
    FAppendVersionNumber: Boolean;
    FCreatePackage: Boolean;
    FDependencies: TStringList;
    FRemoveStructTag: Boolean;
    FUnitSource: TStringList;
    FPackageSource: TStringList;
    FPackageRegUnitSource: TStringList;
    FInputFileName: WideString;
    FOutputFileName: String;
    FUnitname: string;
    FUses : TStrings;
    FHeader : TStrings;
    FInterface : TStrings;
    FImplementation : TStrings;
    FTypes : TStrings;
    FDeclared : TStrings;
    FEventDisp : TStrings;
    FEventIID : TStrings;
    FEventSignatures: TStrings;
    FEventFunctions: TStrings;
    FEventProperties: TStrings;
    FEventImplementations: TStrings;
    FAXClasses: TStrings;
    FAXImages: TStrings;
    function GetDependencies: TStrings;
    function GetUnitSource: TStrings;
    function GetPackageSource: TStrings;
    function GetPackageRegUnitSource: TStrings;
    procedure SetActiveX(AValue: Boolean);
    procedure SetCreatePackage(AValue: Boolean);
    procedure SetOutputFileName(AValue: String);
    procedure SetRemoveStructTag(AValue: Boolean);
    procedure SetUnitName(AValue: string);
  Protected
    bIsCustomAutomatable,bIsInterface,bIsAutomatable,bIsExternalDecl,bIsUserDefined:boolean;
    // Construct unit from header, uses, interface,
    procedure BuildUnit; virtual;
    // Add to various parts of sources
    Procedure AddToUses(Const AUnit : String); virtual;
    Procedure AddToHeader(Const ALine : String; AllowDuplicate : Boolean = False);virtual;
    Procedure AddToHeader(Const Fmt : String; Args : Array of const; AllowDuplicate : Boolean = False);
    Procedure AddToInterface(Const ALine : String);virtual;
    Procedure AddToInterface(Const Fmt : String; Args : Array of const);
    Procedure AddToImplementation(Const ALine : String);virtual;
    Procedure AddToImplementation(Const Fmt : String; Args : Array of const);
    // utility functions
    function interfacedeclaration(iName, iDoc: string; TI: ITypeInfo; TA: LPTYPEATTR;
      bIsDispatch,bCreateEvents:boolean): string;
    function VarTypeIsAutomatable(ParamType: integer): boolean; virtual;
    function VarTypeToStr(ParamType: integer): string; virtual;
    function TypeToString(TI: ITypeInfo; TD: TYPEDESC): string; virtual;
    function ValidateID(id: string): boolean; virtual;
    function ValidateIDAgainstDeclared(id: string): boolean; virtual;
    function MakeValidId(id:string;out valid:string): boolean; virtual;
    function MakeValidIdAgainstDeclared(id:string;var valid:string): boolean;
    function RemoveTag(typename: string): string;
    // The actual routines that do the work.
    procedure CreateCoClasses(const TL: ITypeLib; TICount: Integer); virtual;
    procedure CreateForwards(const TL: ITypeLib; TICount: Integer); virtual;
    procedure CreateInterfaces(const TL: ITypeLib; TICount: Integer); virtual;
    procedure CreateRecordsUnionsAliases(const TL: ITypeLib; TICount: Integer); virtual;
    procedure CreateUnitHeader(const TL: ITypeLib; const LA: lpTLIBATTR); virtual;
    procedure ImportEnums(const TL: ITypeLib; TICount: Integer); virtual;
    procedure ImportGUIDs(const TL: ITypeLib; TICount: Integer); virtual;
    Procedure DoBuildPackage;virtual;
    Procedure DoImportTypelib;virtual;
    // For the benefit of descendents;
    Property UsesClause : TStrings read FUses;
    Property Header : TStrings read FHeader;
    Property InterfaceSection : TStrings Read FInterface;
    Property ImplementationSection : TStrings Read FImplementation;
  Public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    Procedure Execute;
    Property Dependencies : TStrings Read GetDependencies;
    Property UnitSource : TStrings Read GetUnitSource;
    Property PackageSource: TStrings Read GetPackageSource;
    Property PackageRegUnitSource: TStrings Read GetPackageRegUnitSource;
  Published
    // Create ActiveXContainer descendant: default false
    Property ActiveX : Boolean Read FActiveX write SetActiveX Default False;
    // Append version number to unit name.
    Property AppendVersionNumber : Boolean Read FAppendVersionNumber Write FAppendVersionNumber Default True;
    // Create lpk package for ActiveXContainer descendant: default false
    Property CreatePackage : Boolean read FCreatePackage write SetCreatePackage  Default False;
    // File to read typelib from.
    Property InputFileName : WideString Read FInputFileName Write FInputFileName;
    // If set, unit source will be written to this file.
    Property OutputFileName : String Read FOutputFileName Write SetOutputFileName;
    // Remove tag from struct names
    Property RemoveStructTag : Boolean read FRemoveStructTag write SetRemoveStructTag Default False;
    // Set automatically by OutputFileName or by Execute
    Property UnitName : string Read FUnitname Write SetUnitName;
  end;


implementation

Resourcestring
  SErrInvalidUnitName = 'Invalid unit name : %s';

function ImportTypelib(FileName: WideString;var sUnitName:string;var slDependencies:TStringList;
  bActiveX,bPackage,bRemoveStructTag:boolean;var sPackageSource,sPackageRegUnitSource:String):string;
var i:integer;
begin
  With TTypeLibImporter.Create(Nil) do
    try
      InputFileName:=FileName;
      ActiveX:=bActiveX;
      CreatePackage:=bPackage;
      RemoveStructTag:=bRemoveStructTag;
      Execute;
      Result:=UnitSource.Text;
      sUnitname:=UnitName;
      sPackageSource:=FPackageSource.Text;
      sPackageRegUnitSource:=FPackageRegUnitSource.Text;
      if Assigned(slDependencies) then
        begin  //add new dependencies
        for i:=0 to Dependencies.Count-1 do
          if slDependencies.IndexOf(Dependencies[i])=-1 then
            slDependencies.Add(Dependencies[i]);
        end;
    finally
      Free;
    end;
end;

function TTypeLibImporter.VarTypeIsAutomatable(ParamType:integer): boolean;

begin
result:=ParamType in [vt_i1,vt_ui1,vt_i2,vt_ui2,vt_i4,vt_ui4,vt_uint,
            vt_i8,VT_UI8,vt_bool,vt_r4,vt_r8,vt_cy,vt_date,
            VT_BSTR,VT_VARIANT,VT_DISPATCH,VT_UNKNOWN,vt_hresult,VT_INT,
            VT_LPWSTR,VT_LPSTR];
end;

function TTypeLibImporter.VarTypeToStr(ParamType:integer): string;

begin
  case ParamType of
    vt_empty : Result := 'Empty';
    vt_null : Result := 'Null';
    vt_i2 : Result := 'Smallint';
    vt_i4 : Result := 'Integer';
    vt_r4 : Result := 'Single';
    vt_r8 : Result := 'Double';
    vt_cy : Result := 'Currency';
    vt_date : Result := 'TDateTime';
    vt_bstr : Result := 'WideString';
    vt_dispatch  : Result := 'IDispatch';
    vt_error : Result := 'SCODE';
    vt_bool : Result := 'WordBool';
    vt_variant : Result := 'OleVariant';
    vt_unknown : Result := 'IUnknown';
    vt_i1  : Result := 'ShortInt';
    vt_ui1 : Result := 'Byte';
    vt_ui2 : Result := 'Word';
    vt_ui4  : Result := 'LongWord';
    vt_i8 : Result := 'Int64';
    VT_UI8: Result := 'QWord';
    vt_clsid : Result := 'TGUID';
    vt_void : Result := 'pointer';
    vt_ptr : Result := 'Pointer';
    vt_uint : Result := 'UInt';
    vt_userdefined : Result := 'User defined';
    vt_hresult : Result := 'HResult';
    VT_INT:Result:='SYSINT';
    VT_SAFEARRAY:Result:='PSafeArray';
    VT_LPWSTR:Result:='PWideChar';
    VT_LPSTR:Result:='PChar';
    VT_DECIMAL:Result:='TDecimal';
  else
    Result := 'Unknown (' + IntToStr(ParamType) + ')';
  end;
end;

function TTypeLibImporter.ValidateID(id:string):boolean;

const
  RESERVEDCNT=111;
  RESERVED:array[1..RESERVEDCNT] of string=
  ('absolute','and','array','asm','begin','break','case','const',
  'constructor','continue','destructor','div','do','downto','else','end',
  'file','for','function','goto','if','implementation','in','inherited',
  'inline','interface','label','mod','nil','not','object','of',
  'on','operator','or','packed','procedure','program','record','reintroduce',
  'repeat','self','set','shl','shr','string','then','to',
  'type','unit','until','uses','var','while','with','xor',
  'as','class','except','exports','finalization','finally','initialization',
  'is','library','on','property','raise','threadvar','try',
  'dispose','exit','false','new','true',
  'abs','arctan','boolean','char','cos','dispose','eof','eoln',
  'exp','false','input','integer','ln','maxint','new','odd',
  'ord','output','pack','page','pred','read','readln','real',
  'reset','rewrite','round','sin','sqr','sqrt','succ','text',
  'true','trunc','write','writeln');

var
  sl:string;
  i:integer;

begin
  sl:=lowercase(id);
  result:=true;
  for i:=1 to RESERVEDCNT do
    if sl= RESERVED[i] then
      begin
      result:=false;
      break;
      end;
end;

function TTypeLibImporter.ValidateIDAgainstDeclared(id: string): boolean;
var i:integer;
begin
  id:=lowercase(id);
  i:=FDeclared.Count-1;
  while i>=0 do
    begin
    if lowercase(FDeclared[i])=id then
      break;
    i:=i-1;
    end;
  result:=i<0;
end;

function TTypeLibImporter.MakeValidId(id:string;out valid:string): boolean;
begin
  result:= ValidateID(id);
  if result then
    valid:=id
  else
    valid:=id+'_';
end;

function TTypeLibImporter.MakeValidIdAgainstDeclared(id:string;var valid:string): boolean;
begin
  result:= ValidateIDAgainstDeclared(id) and ValidateID(id);
  if result then
    valid:=id
  else
    MakeValidIdAgainstDeclared(id+'_',valid);
end;

function TTypeLibImporter.RemoveTag(typename: string): string;
begin
  result:=typename;
  if FRemoveStructTag and (pos('tag',typename)>0) then
    if (copy(typename,1,3)='tag') then
      delete(result,1,3)
    else if (copy(typename,1,4)='_tag') then
      delete(result,2,3)
    else if (copy(typename,1,5)='__tag') then
      delete(result,3,3);
end;


function TTypeLibImporter.TypeToString(TI:ITypeInfo; TD:TYPEDESC):string;

var
  TIref: ITypeInfo;
  TARef:LPTYPEATTR;
  TLRef: ITypeLib;
  LARef: lpTLIBATTR;
  BstrName : WideString;
  il:LongWord;
  i,idims:integer;
  sl,sRefSrc,sKey:string;
  Handle:HKEY;
  bWasPointer:boolean;
begin
  result:='';
  bIsCustomAutomatable:=false;
  bIsInterface:=false;
  bIsExternalDecl:=false;
  bIsUserDefined:=false;
  if (TD.vt=vt_userdefined) or ((TD.vt=VT_PTR) and (TD.lptdesc^.vt=vt_userdefined)) then
    begin
    // interface references are dealt with now because they are pointers in fpc.
    // Recursive algorithm makes it difficult to remove a single preceding 'P' from the result.
    bIsUserDefined:=true;
    bWasPointer:=(TD.vt=VT_PTR);
    if bWasPointer then
      TD:=TD.lptdesc^;
    OleCheck(TI.GetRefTypeInfo(TD.hreftype,TIref));
    OleCheck(TIRef.GetDocumentation(DISPID_UNKNOWN, @BstrName, nil, nil, nil));
    OleCheck(TIRef.GetTypeAttr(TARef));
    bIsCustomAutomatable:=TARef^.typekind in [TKIND_DISPATCH,TKIND_INTERFACE,TKIND_ENUM,TKIND_COCLASS];
    if TARef^.typekind in [TKIND_RECORD,TKIND_UNION,TKIND_ALIAS] then
      BstrName:=RemoveTag(BstrName);
    MakeValidId(BstrName,result);
    if TARef^.typekind=TKIND_ALIAS then
      begin
      TypeToString(TIRef,TARef^.tdescAlias); //not interested in result, only bIsCustomAutomatable and bIsInterface
      bIsCustomAutomatable:=bIsAutomatable;
      end
    else
      bIsInterface:=TARef^.typekind in [TKIND_DISPATCH,TKIND_INTERFACE,TKIND_COCLASS] ;
    if bWasPointer and not bIsInterface then  // interfaces are pointers to interface in fpc
      result:='P'+result;
    OleCheck(TIRef.GetContainingTypeLib(TLRef,il));
    OleCheck(TLRef.GetDocumentation(-1, @BstrName, nil, nil, nil));
    OleCheck(TLRef.GetLibAttr(LARef));
    if FAppendVersionNumber then
      sl:=format('%s_%d_%d_TLB',[BstrName,LARef^.wMajorVerNum,LARef^.wMinorVerNum])
    else
      sl:=format('%s_TLB',[BstrName]);
    if (LowerCase(BstrName)='stdole') then // don't include, uses pre-defined stdole2.pas if V2
      begin
      bIsExternalDecl:=true;
      if lowercase(result)='guid' then
        result:='TGUID';
      if (LARef^.wMajorVerNum=2) and (FUses.IndexOf('stdole2')=-1) then
        begin
        AddToHeader('// Dependency: stdole v2 (stdole2.pas)');
        FUses.Add('stdole2');
        end;
      end
    else if (LowerCase(sl)<>LowerCase(UnitName)) and (FUses.IndexOf(sl)=-1) then
      begin  // add dependency
      // find source in registry key HKEY_CLASSES_ROOT\TypeLib\GUID\version\0\win32
      bIsExternalDecl:=true;
      il:=MAX_PATH;
      SetLength(sRefSrc,il);
      sKey:=format('\TypeLib\%s\%d.%d\0\win32',[GUIDToString(LARef^.GUID),LARef^.wMajorVerNum,LARef^.wMinorVerNum]);
      if (RegOpenKeyEx(HKEY_CLASSES_ROOT,pchar(sKey),0,KEY_READ,Handle) = ERROR_SUCCESS) then
        begin
        if RegQueryValue(Handle,nil,@sRefSrc[1],@il) = ERROR_SUCCESS then
          begin
          SetLength(sRefSrc,il-1);  // includes null terminator
          if not FDependencies.Find(sRefSrc,i) then
            FDependencies.Add(sRefSrc);
          end
        else
          sRefSrc:=GUIDToString(LARef^.GUID);
        RegCloseKey(Handle);
        end
      else
        sRefSrc:='DLL not registered in the system';
      AddToHeader('// Dependency: %s v%d.%d (%s)',[BstrName,LARef^.wMajorVerNum,LARef^.wMinorVerNum,sRefSrc]);
      FUses.Add(sl);
      TLRef.ReleaseTLibAttr(LARef);
      end;
    TIRef.ReleaseTypeAttr(TARef);
    end
  else if TD.vt=VT_PTR then //pointer type
    begin
    TD:=TD.lptdesc^;
    result:='P'+TypeToString(TI,TD);
    bIsAutomatable:=VarTypeIsAutomatable(TD.vt) or bIsCustomAutomatable;
    exit;
    end
  else if TD.vt=VT_CARRAY then //C type array
    begin
    // get array element type
    sl:=TypeToString(TI,TD.lpadesc^.tdescElem);
    // get dimensions
    idims:=TD.lpadesc^.cDims;
    result:='array[';
    // get boundaries for every dimension
    for i:=0 to idims-1 do
      result:=result+IntToStr(TD.lpadesc^.rgbounds[i].lLbound)+'..'+IntToStr(TD.lpadesc^.rgbounds[i].cElements - TD.lpadesc^.rgbounds[i].lLbound -1)+',';
    result[length(result)]:=']';
    result:=result + ' of '+sl;
    end
  else
    result:=VarTypeToStr(TD.vt);
  bIsAutomatable:=VarTypeIsAutomatable(TD.vt) or bIsCustomAutomatable;
end;

function TTypeLibImporter.interfacedeclaration(iName,iDoc:string;TI:ITypeInfo;TA:LPTYPEATTR;
  bIsDispatch,bCreateEvents:boolean):string;

type
  TPropertyDef=record
    idispid:integer;
    bput,bputref,bget:boolean;
    name,pname,prname,
    sgtype,sptype,sprtype,
    sorgname,
    sdoc,
    sParam,
    sDefault,
    sPutSuffix:string;
  end;

var
  RTIT: HREFTYPE;
  TIref: ITypeInfo;
  BstrName,BstrNameRef,BstrDocString : WideString;
  s,sl,sPropDispIntfc,sType,sConv,sFunc,sPar,sVarName,sMethodName,
  sPropParam,sPropParam2,sPropParam3:string;
  sEventSignatures,sEventFunctions,sEventProperties,sEventImplementations:string;
  i,j,k:integer;
  FD: lpFUNCDESC;
  BL : array[0..99] of TBstr;
  cnt:LongWord;
  TD: TYPEDESC;
  bPropHasParam,bIsFunction,bParamByRef:boolean;
  VD: lpVARDESC;
  aPropertyDefs:array of TPropertyDef;
  Propertycnt,iType:integer;

  function findProperty(ireqdispid:integer):integer;
  var i:integer;
  begin
    for i:=0 to Propertycnt-1 do
      if aPropertyDefs[i].idispid=ireqdispid then
        begin
        result:=i;
        exit;
        end;
    result:=Propertycnt;
    Propertycnt:=Propertycnt+1;
    with aPropertyDefs[result] do
      begin
      idispid:=ireqdispid;
      bput:=false;
      bputref:=false;
      bget:=false;
      name:='';
      pname:='';
      prname:='';
      sgtype:='';
      sptype:='';
      sprtype:='';
      sorgname:='';
      sdoc:='';
      sParam:='';
      sDefault:='';
      sPutSuffix:='';
      end;
  end;

  function GetName(i:integer):string;  //bug in Office10\MSacc.OLB _WizHook.Key
  begin
    result:='';
    if i<integer(cnt) then
      result:=BL[i];
    if result='' then //No name ?
      result:='Param'+inttostr(i);
  end;

begin
  FillMemory(@TD,Sizeof(TD),0);
  TD.vt:=VT_ILLEGAL;
  Propertycnt:=0;
  SetLength(aPropertyDefs,TA^.cFuncs+TA^.cVars);   // worst case, all functions getters or all setters
  sEventSignatures:='';
  sEventFunctions:='';
  sEventProperties:='';
  sEventImplementations:='';
  if not bIsDispatch then
    begin
    // find base class
    if TA^.cImplTypes>0 then
      begin
      OleCheck(TI.GetRefTypeOfImplType(0,RTIT));
      OleCheck(TI.GetRefTypeInfo(RTIT,TIref));
      OleCheck(TIRef.GetDocumentation(DISPID_UNKNOWN, @BstrNameRef, nil, nil, nil));
      MakeValidId(BstrNameRef,sl);
      s:=format(#13#10'// %s : %s'#13#10#13#10' %s = interface(%s)'#13#10,[iname,iDoc,iname,sl]);
      end
    else // no base class
      begin
      s:=format(#13#10'// %s : %s'#13#10#13#10' %s = interface'#13#10,[iname,iDoc,iname]);
      end;
    end
  else
    if (TA^.wTypeFlags and TYPEFLAG_FDUAL)=TYPEFLAG_FDUAL then
      s:=format(#13#10'// %s : %s'#13#10#13#10' %sDisp = dispinterface'#13#10,[iname,iDoc,iname])
    else
      s:=format(#13#10'// %s : %s'#13#10#13#10' %s = dispinterface'#13#10,[iname,iDoc,iname]);
  sPropDispIntfc:='';
  s:=s+format('   [''%s'']'#13#10,[GUIDToString(TA^.GUID)]);
  for j:=0 to TA^.cFuncs-1 do
    begin
    OleCheck(TI.GetFuncDesc(j,FD));
    OleCheck(TI.GetNames(FD^.memid,@BL,length(BL),cnt));
    // skip IUnknown and IDispatch methods
    sl:=lowercase(BL[0]);
    if (sl='queryinterface') or (sl='addref') or (sl='release') then  //IUnknown
      continue;
    if bIsDispatch and
      ((sl='gettypeinfocount') or (sl='gettypeinfo') or (sl='getidsofnames') or (sl='invoke')) then  //IDispatch
      continue;
    // get return type
    if bIsDispatch and ((FD^.invkind=INVOKE_PROPERTYGET) or (FD^.invkind=INVOKE_FUNC)) then
      begin
      sType:=TypeToString(TI,FD^.elemdescFunc.tdesc);
      iType:=FD^.elemdescFunc.tdesc.vt;
      end
    else
      if FD^.cParams>0 then
        begin
        sType:=TypeToString(TI,FD^.lprgelemdescParam[FD^.cParams-1].tdesc);
        iType:=FD^.lprgelemdescParam[FD^.cParams-1].tdesc.vt;
        if ((FD^.lprgelemdescParam[FD^.cParams-1].paramdesc.wParamFlags and (PARAMFLAG_FRETVAL or PARAMFLAG_FOUT)) <>0) then
          begin
          delete(sType,1,1); //out parameters are always defined as pointer
          if assigned(FD^.lprgelemdescParam[FD^.cParams-1].tdesc.lptdesc) then
            iType:=FD^.lprgelemdescParam[FD^.cParams-1].tdesc.lptdesc^.vt;
          end;
        end
      else
        if((FD^.invkind=INVOKE_PROPERTYGET) or (FD^.invkind=INVOKE_FUNC)) then
          begin
          sType:=TypeToString(TI,FD^.elemdescFunc.tdesc);
          iType:=FD^.elemdescFunc.tdesc.vt;
          end;
    //get calling convention
    if FD^.callconv=CC_STDCALL then
      begin
      if lowercase(BstrNameRef)='iunknown' then
        sConv:='stdcall'
      else
        sConv:='safecall';
      end
    else
      sConv:='cdecl';
    // get info
    OleCheck(TI.GetDocumentation(FD^.memid, @BstrName, @BstrDocString, nil, nil));
    case FD^.invkind of
      // build function/procedure
      INVOKE_FUNC :
        begin
        if not MakeValidId(BstrName,sMethodName) then
          AddToHeader('//  Warning: renamed method ''%s'' in %s to ''%s''',[BstrName,iname,sMethodName],True);
        bIsFunction:=(bIsDispatch and (FD^.elemdescFunc.tdesc.vt<>VT_VOID)) or
          (not bIsDispatch and (FD^.cParams>0) and ((FD^.lprgelemdescParam[FD^.cParams-1].paramdesc.wParamFlags and PARAMFLAG_FRETVAL ) <>0)) or
          (sConv<>'safecall');
        if bIsFunction then
          sFunc:=format('    // %s : %s '#13#10'   function %s(',[sMethodName,BstrDocString,sMethodName])
        else
          sFunc:=format('    // %s : %s '#13#10'   procedure %s(',[sMethodName,BstrDocString,sMethodName]);
        if bIsFunction and bIsDispatch and (not bIsAutomatable or (sType='POleVariant')) then
          begin
          AddToHeader('//  Warning: ''%s'' not automatable in %sdisp.%s',[stype,iname,BstrName],True);
          sType:='{!! '+sType+' !!} OleVariant';
          end;
        if bCreateEvents then
          begin
          sEventSignatures:=sEventSignatures+format('  T%s%s = procedure(Sender: TObject;',[iname,sMethodName]);
          sEventFunctions:=sEventFunctions+format('    FOn%s:T%s%s;'#13#10,[sMethodName,iname,sMethodName]);
          sEventProperties:=sEventProperties+format('    property On%s : T%s%s read FOn%s write FOn%s;'#13#10,
            [sMethodName,iname,sMethodName,sMethodName,sMethodName]);
          sEventImplementations:=sEventImplementations+
            format('    %d: if assigned(On%s) then'#13#10+
                   '          On%s(Self,',[FD^.memid,sMethodName,sMethodName]);
          end;
        // parameters
        for k:=0 to FD^.cParams-1 do
          begin
          if (FD^.lprgelemdescParam[k].paramdesc.wParamFlags and PARAMFLAG_FRETVAL ) <>0 then  //return type
            continue;
          sl:=TypeToString(TI,FD^.lprgelemdescParam[k].tdesc);
          bParamByRef:=(FD^.lprgelemdescParam[k].tdesc.vt=VT_PTR) and                         // by ref
            not((FD^.lprgelemdescParam[k].tdesc.lptdesc^.vt=VT_USERDEFINED) and bIsInterface);// but not pointer to interface
          if bParamByRef then
             delete(sl,1,1);
          if bIsDispatch and not bIsAutomatable then
            begin
            AddToHeader('//  Warning: ''%s'' not automatable in %sdisp.%s',[sl,iname,sMethodName],True);
            sl:='{!! '+sl+' !!} OleVariant';
            end;
          sPar:='';
          if bParamByRef then
            case FD^.lprgelemdescParam[k].paramdesc.wParamFlags and (PARAMFLAG_FIN or PARAMFLAG_FOUT) of
            PARAMFLAG_FIN or PARAMFLAG_FOUT:sPar:='var ';
            PARAMFLAG_FOUT:sPar:='out ';
            PARAMFLAG_FIN:sPar:='var '; //constref in safecall? TBD
            end;
          if not MakeValidId(GetName(k+1),sVarName) then
            AddToHeader('//  Warning: renamed parameter ''%s'' in %s.%s to ''%s''',[GetName(k+1),iname,sMethodName,sVarName],True);
          sPar:=sPar+format('%s:%s;',[sVarName,sl]);
          sFunc:=sFunc+sPar;
          if bCreateEvents then
            begin
            sEventSignatures:=sEventSignatures+sPar;
            //params are numbered last to first
            if bParamByRef and not(bIsDispatch and not bIsAutomatable) then
              begin
              if ((FD^.lprgelemdescParam[k].paramdesc.wParamFlags and (PARAMFLAG_FIN or PARAMFLAG_FOUT))<>0)
                and (FD^.lprgelemdescParam[k].tdesc.lptdesc^.vt=VT_PTR)
                and (FD^.lprgelemdescParam[k].tdesc.lptdesc^.lptdesc^.vt=VT_USERDEFINED) then
                //some casting needed for interfaces!!
                sEventImplementations:=sEventImplementations+format(' %s(Params.rgvarg[%d].byRef^),',[sl,FD^.cParams-1-k])
              else if  FD^.lprgelemdescParam[k].tdesc.lptdesc^.vt=VT_USERDEFINED then
                //some casting needed for enums!!
                sEventImplementations:=sEventImplementations+format(' %s(Params.rgvarg[%d].byRef^),',[sl,FD^.cParams-1-k])
              else if  FD^.lprgelemdescParam[k].tdesc.lptdesc^.vt=VT_VARIANT then
                //pvarVal^ results in Error: Can't take the address of constant expressions ?????
                sEventImplementations:=sEventImplementations+format(' OleVariant(Params.rgvarg[%d].byRef^),',[FD^.cParams-1-k])
              else if  FD^.lprgelemdescParam[k].tdesc.lptdesc^.vt=VT_BSTR then
                //pbstrVal^ results in Error: Can't take the address of constant expressions ?????
                sEventImplementations:=sEventImplementations+format(' WideString(Params.rgvarg[%d].byRef^),',[FD^.cParams-1-k])
              else
                begin
                case FD^.lprgelemdescParam[k].tdesc.lptdesc^.vt of
                  VT_UI1:       sl:='pbVal';
                  VT_UI2:       sl:='puiVal';
                  VT_UI4:       sl:='pulVal';
                  VT_UI8:       sl:='pullVal';
                  VT_I1:        sl:='pcVal';
                  VT_I2:        sl:='piVal';
                  VT_I4:        sl:='plVal';
                  VT_I8:        sl:='pllVal';
                  VT_R4:        sl:='pfltVal';
                  VT_R8:        sl:='pdblVal';
                  VT_BOOL:      sl:='pbool';
                  VT_ERROR:     sl:='pscode';
                  VT_CY:        sl:='pcyVal';
                  VT_DATE:      sl:='pdate';
//                  VT_BSTR:      sl:='pbstrVal';
                  VT_UNKNOWN:   sl:='punkVal';
                  VT_DISPATCH:  sl:='pdispVal';
                  VT_ARRAY:     sl:='pparray';
                  //VT_VARIANT:   sl:='pvarVal';
                  VT_INT:       sl:='pintVal';
                  VT_UINT:      sl:='puintVal';
                  VT_DECIMAL:   sl:='pdecVal';
                else
                  sl:='byRef';
                end;
                sEventImplementations:=sEventImplementations+format(' Params.rgvarg[%d].%s^,',[FD^.cParams-1-k,sl]);
                end;
              end
            else
              sEventImplementations:=sEventImplementations+format(' OleVariant(Params.rgvarg[%d]),',[FD^.cParams-1-k]);
            end;
          end;
        // finish interface and dispinterface
        if sFunc[length(sFunc)]=';' then
          sFunc[length(sFunc)]:=')'
        else  // no params
          delete(sFunc,length(sFunc),1);
        if bCreateEvents then
          begin
          sEventSignatures[length(sEventSignatures)]:=')';
          sEventSignatures:=sEventSignatures+' of object;'#13#10;
          sEventImplementations[length(sEventImplementations)]:=')';
          sEventImplementations:=sEventImplementations+';'#13#10;
          end;
        if bIsFunction then
          if (sConv<>'safecall') then
            sFunc:=sFunc+':HRESULT'
          else
            sFunc:=sFunc+format(':%s',[sType]);
        if bIsDispatch then
          s:=s+sFunc+format(';dispid %d;'#13#10,[FD^.memid])
        else
          s:=s+sFunc+format(';%s;'#13#10,[sConv]);
        end;
      INVOKE_PROPERTYGET,INVOKE_PROPERTYPUT,INVOKE_PROPERTYPUTREF :
        // build properties. Use separate string to group properties at end of interface declaration.
        begin
        if not MakeValidId(BstrName,sMethodName) then
          AddToHeader('//  Warning: renamed property ''%s'' in %s to ''%s''',[BstrName,iname,sMethodName]);
        bPropHasParam:=(((FD^.invkind=INVOKE_PROPERTYGET) and (FD^.cParams>0)) or (FD^.cParams>1))
            and ((FD^.lprgelemdescParam[0].paramdesc.wParamFlags and PARAMFLAG_FIN) = PARAMFLAG_FIN) ;
        if (FD^.memid=0) and  bPropHasParam then sl:=' default;' else sl:='';
        sPropParam:='';
        sPropParam2:='';
        if bPropHasParam then
          begin
          if not MakeValidId(GetName(1),sPropParam) then
            AddToHeader('//  Warning: renamed property index  ''%s'' in %s.%s to ''%s''',[GetName(1),iname,sMethodName,sPropParam]);
          sPropParam:=sPropParam+':'+TypeToString(TI,FD^.lprgelemdescParam[0].tdesc);
          end;
        if bIsDispatch then
          begin
          if (TD.vt<>VT_VOID) and (not bIsAutomatable or (sType='POleVariant')) then
            begin
            AddToHeader('//  Warning: ''%s'' not automatable in %s.%s',[stype,iname,BstrName]);
            sType:='{!! '+sType+' !!} OleVariant';
            end;
          if bPropHasParam then
            sPropParam:='['+sPropParam+']';
          i:=pos(format('dispid %d;',[FD^.memid]),sPropDispIntfc);
          if i<=0 then
            begin
            if FD^.invkind=INVOKE_PROPERTYGET then
              sType:=sType+'  readonly'
            else
              sType:=sType+' writeonly';
            sPropDispIntfc:=sPropDispIntfc+format('    // %s : %s '#13#10'   property %s%s:%s dispid %d;%s'#13#10,
              [BstrName,BstrDocString,sMethodName,sPropParam,sType,FD^.memid,sl]);
            end
          else //remove readonly or writeonly
            // make sure writeonly isn't delete twice (put and putref !!)
            if pos(format('only dispid %d;',[FD^.memid]),sPropDispIntfc)>0 then
              delete(sPropDispIntfc,i-11,10);   //10= length('  readonly')
          end
        else
          begin
          //getters/setters for interface, insert in interface as they come,
          //store in aPropertyDefs to create properties at the end
          if bPropHasParam then
            begin
            sPropParam2:='('+sPropParam+')';
            sPropParam3:=sPropParam+'; const par'+sMethodName;
            sPropParam:='['+sPropParam+']';
            end;
          if FD^.invkind=INVOKE_PROPERTYGET then
            begin
            s:=s+format('   function Get_%s%s : %s; %s;'#13#10,[sMethodName,sPropParam2,sType,sConv]);
            with aPropertyDefs[findProperty(FD^.memid)] do
              begin
              bget:=true;
              name:=sMethodName;
              sgtype:=sType;
              sorgname:=BstrName;
              sdoc:=BstrDocString;
              sParam:=sPropParam;
              sDefault:=sl;
              end;
            end
          else
            begin
            if not MakeValidId(GetName(1),sVarName) then
              AddToHeader('//  Warning: renamed parameter ''%s'' in %s.Set_%s to ''%s''',[GetName(1),iname,sMethodName,sVarName]);
            with aPropertyDefs[findProperty(FD^.memid)] do
              begin
              if FD^.invkind=INVOKE_PROPERTYPUT then
                begin
                sptype:=sType;
                bput:=true;
                if bputref then                  //disambiguate  multiple setter
                  sMethodName:=sMethodName+'_';
                pname:=sMethodName;
                end
              else
                begin
                sprtype:=sType;
                bputref:=true;
                if bput then                     //disambiguate  multiple setter
                  sMethodName:=sMethodName+'_';
                prname:=sMethodName;
                end;
              sorgname:=BstrName;
              sdoc:=BstrDocString;
              sParam:=sPropParam;
              sDefault:=sl;
              end;
            if bPropHasParam then
              s:=s+format('   procedure Set_%s(const %s:%s); %s;'#13#10,[sMethodName,sPropParam3,sType,sConv])
            else
              s:=s+format('   procedure Set_%s(const %s:%s); %s;'#13#10,[sMethodName,sVarName,sType,sConv]);
            end;
          end;
        end;
    end;
    TI.ReleaseFuncDesc(FD);
    end;
  for j:=0 to TA^.cVars-1 do
    begin  //read-write properties only
    if bIsDispatch then
      begin
      TI.GetVarDesc(j,VD);
      if assigned(VD) then
        begin
        TI.GetDocumentation(VD^.memId,@BstrName, @BstrDocString, nil, nil);
        if not MakeValidId(BstrName,sMethodName) then
          AddToHeader('//  Warning: renamed property ''%s'' in %s to ''%s''',[BstrName,iname,sMethodName]);
        sType:=TypeToString(TI,VD^.ElemdescVar.tdesc);
        sPropDispIntfc:=sPropDispIntfc+format('    // %s : %s '#13#10'   property %s:%s  dispid %d;'#13#10,
          [BstrName,BstrDocString,sMethodName,sType,VD^.memId]);
        end;
      end;
    end;
  if bCreateEvents then
    begin
    FEventDisp.Add(iname);
    FEventIID.Add(GUIDToString(TA^.GUID));
    FEventSignatures.Add(sEventSignatures);
    FEventFunctions.Add(sEventFunctions);
    FEventProperties.Add(sEventProperties);
    FEventImplementations.Add(sEventImplementations);
    end;
  if bIsDispatch then
    result:=s + sPropDispIntfc +'  end;'#13#10
  else
    begin
    // add interface properties
    for i:=0 to Propertycnt-1 do
      with aPropertyDefs[i] do
      if not bget then //setter only
        begin
        if bput then
          s:=s+format('    // %s : %s '#13#10'   property %s%s:%s write Set_%s;%s'#13#10,
            [sorgname,sdoc,pname,sParam,sptype,pname,sDefault])
        else
          s:=s+format('    // %s : %s '#13#10'   property %s%s:%s write Set_%s;%s'#13#10,
            [sorgname,sdoc,prname,sParam,sprtype,prname,sDefault]);
        end
      else if not (bput or bputref) then //getter only
        s:=s+format('    // %s : %s '#13#10'   property %s%s:%s read Get_%s;%s'#13#10,
          [sorgname,sdoc,name,sParam,sgtype,name,sDefault])
      else if bput and (sptype=sgtype) then //don't create property if no matching type.
        begin
        s:=s+format('    // %s : %s '#13#10'   property %s%s:%s read Get_%s write Set_%s;%s'#13#10,
          [sorgname,sdoc,name,sParam,sptype,name,pname,sDefault]);
        end
        else if bputref and (sprtype=sgtype) then //don't create property if no matching type.
          begin
          s:=s+format('    // %s : %s '#13#10'   property %s%s:%s read Get_%s write Set_%s;%s'#13#10,
            [sorgname,sdoc,name,sParam,sprtype,name,prname,sDefault]);
          end;
    result:=s+'  end;'#13#10;
    end;
end;

function TTypeLibImporter.GetDependencies: TStrings;
begin
  Result:=FDependencies;
end;

function TTypeLibImporter.GetUnitSource: TStrings;
begin
  Result:=FUnitSource;
end;

function TTypeLibImporter.GetPackageSource: TStrings;
begin
  Result:=FPackageSource;
end;

function TTypeLibImporter.GetPackageRegUnitSource: TStrings;
begin
   Result:=FPackageRegUnitSource;
end;

procedure TTypeLibImporter.SetActiveX(AValue: Boolean);
begin
  if FActiveX=AValue then Exit;
  FActiveX:=AValue;
  if not FActiveX then FCreatePackage:=false;
end;

procedure TTypeLibImporter.SetCreatePackage(AValue: Boolean);
begin
  if FCreatePackage=AValue then Exit;
  FCreatePackage:=AValue;
  if FCreatePackage then FActiveX:=true;
end;

Procedure TTypeLibImporter.ImportGUIDs(Const TL : ITypeLib; TICount : Integer);

Var
  i : integer;
  BstrName, BstrDocString, BstrHelpFile : WideString;
  dwHelpContext: DWORD;
  TI:ITypeInfo;
  TA:LPTYPEATTR;
  TIT: TYPEKIND;

begin
  //GUIDs
  for i:=0 to TIcount-1 do
    begin
    OleCheck(TL.GetTypeInfoType(i, TIT));
    OleCheck(TL.GetTypeInfo(i, TI));
    OleCheck(TL.GetDocumentation(i, @BstrName, @BstrDocString, @dwHelpContext, @BstrHelpFile));
    OleCheck(TI.GetTypeAttr(TA));
    case TIT of
      TKIND_DISPATCH,TKIND_INTERFACE:
        begin
        AddToInterface('  IID_%s : TGUID = ''%s'';',[BstrName,GUIDToString(TA^.GUID)]);
        FTypes.Add(BstrName);
        FDeclared.Add(BstrName);
        end;
      TKIND_COCLASS:
        begin
        AddToInterface('  CLASS_%s : TGUID = ''%s'';',[BstrName,GUIDToString(TA^.GUID)]);
        FTypes.Add(BstrName);
        FDeclared.Add(BstrName);
        end;
      end;
    TI.ReleaseTypeAttr(TA);
    end;
end;

procedure TTypeLibImporter.DoBuildPackage;
var
  i : integer;
  sl:string;
begin
  if FAXClasses.Count=0 then  //nothing to do
    exit;
  // create lpk
  FPackageSource.Clear;
  FPackageSource.Add('<?xml version="1.0"?>');
  FPackageSource.Add('<CONFIG>');
  FPackageSource.Add('  <Package Version="4">');
  FPackageSource.Add('    <PathDelim Value="\"/>');
  FPackageSource.Add(format('    <Name Value="%sP"/>',[unitname]));
  FPackageSource.Add('    <CompilerOptions>');
  FPackageSource.Add('      <Version Value="11"/>');
  FPackageSource.Add('      <PathDelim Value="\"/>');
  FPackageSource.Add('      <SearchPaths>');
  FPackageSource.Add('        <UnitOutputDirectory Value="lib\$(TargetCPU)-$(TargetOS)"/>');
  FPackageSource.Add('      </SearchPaths>');
  FPackageSource.Add('      <Other>');
  FPackageSource.Add('        <CompilerMessages>');
  FPackageSource.Add('          <UseMsgFile Value="True"/>');
  FPackageSource.Add('        </CompilerMessages>');
  FPackageSource.Add('        <CustomOptions Value="-Ur"/>');
  FPackageSource.Add('        <CompilerPath Value="$(CompPath)"/>');
  FPackageSource.Add('      </Other>');
  FPackageSource.Add('    </CompilerOptions>');
  FPackageSource.Add(format('    <Description Value="%s package"/>',[unitname]));
  FPackageSource.Add('    <Version Minor="1"/>');
  FPackageSource.Add('    <Files Count="2">');
  FPackageSource.Add('      <Item1>');
  FPackageSource.Add(format('        <Filename Value="%s.pas"/>',[unitname]));
  FPackageSource.Add(format('        <UnitName Value="%s"/>',[unitname]));
  FPackageSource.Add('      </Item1>');
  FPackageSource.Add('      <Item2>');
  FPackageSource.Add(format('        <Filename Value="%sPreg.pas"/>',[unitname]));
  FPackageSource.Add('        <HasRegisterProc Value="True"/>');
  FPackageSource.Add('        <AddToUsesPkgSection Value="False"/>');
  FPackageSource.Add(format('        <UnitName Value="%sPreg"/>',[unitname]));
  FPackageSource.Add('      </Item2>');
  FPackageSource.Add('    </Files>');
  FPackageSource.Add('    <Type Value="RunAndDesignTime"/>');
  FPackageSource.Add('    <RequiredPkgs Count="1">');
  FPackageSource.Add('      <Item1>');
  FPackageSource.Add('        <PackageName Value="LazActiveX"/>');
  FPackageSource.Add('      </Item1>');
  FPackageSource.Add('    </RequiredPkgs>');
  FPackageSource.Add('    <UsageOptions>');
  FPackageSource.Add('      <UnitPath Value="$(PkgOutDir)"/>');
  FPackageSource.Add('    </UsageOptions>');
  FPackageSource.Add('    <PublishOptions>');
  FPackageSource.Add('      <Version Value="2"/>');
  FPackageSource.Add('    </PublishOptions>');
  FPackageSource.Add('  </Package>');
  FPackageSource.Add('</CONFIG>');
  // create registration unit
  FPackageRegUnitSource.Clear;
  FPackageRegUnitSource.Add(format('unit %sPreg;',[unitname]));
  FPackageRegUnitSource.Add('');
  FPackageRegUnitSource.Add('interface');
  FPackageRegUnitSource.Add('');
  FPackageRegUnitSource.Add('uses');
  FPackageRegUnitSource.Add(format('  %s;',[unitname]));
  FPackageRegUnitSource.Add('');
  FPackageRegUnitSource.Add('procedure Register;');
  FPackageRegUnitSource.Add('');
  FPackageRegUnitSource.Add('implementation');
  FPackageRegUnitSource.Add('');
  FPackageRegUnitSource.Add('uses  classes,LResources;');
  FPackageRegUnitSource.Add('');
  FPackageRegUnitSource.Add('');
  FPackageRegUnitSource.Add('procedure Register;');
  FPackageRegUnitSource.Add('begin');
  sl:='';
  for i:=0 to FAXClasses.Count-1 do
    sl:=sl+FAXClasses[i]+',';
  sl[Length(sl)]:=']';
  FPackageRegUnitSource.Add(format('  RegisterComponents(''ActiveX'', [%s);',[sl]));
  FPackageRegUnitSource.Add('end;');
  FPackageRegUnitSource.Add('');
  FPackageRegUnitSource.Add('initialization');
  FPackageRegUnitSource.Add('');
  for i:=0 to FAXImages.Count-1 do
    PackageRegUnitSource.Add(FAXImages[i]);
  FPackageRegUnitSource.Add('');
  FPackageRegUnitSource.Add('end.');
end;

Procedure TTypeLibImporter.ImportEnums(Const TL : ITypeLib; TICount : Integer);

Var
  i,j : integer;
  sl ,senum: string;
  BstrName, BstrDocString, BstrHelpFile : WideString;
  dwHelpContext: DWORD;
  TI:ITypeInfo;
  TA:LPTYPEATTR;
  TIT: TYPEKIND;
  bDuplicate:boolean;
  VD: lpVARDESC;

begin
  //enums
  AddToInterface('');
  AddToInterface('//Enums');
  AddToInterface('');
  for i:=0 to TIcount-1 do
    begin
    OleCheck(TL.GetTypeInfoType(i, TIT));
    OleCheck(TL.GetTypeInfo(i, TI));
    OleCheck(TL.GetDocumentation(i, @BstrName, @BstrDocString, @dwHelpContext, @BstrHelpFile));
    OleCheck(TI.GetTypeAttr(TA));
    if TIT=TKIND_ENUM then
      begin
      bDuplicate:=false;
      if not MakeValidId(BstrName,senum) then
        AddToHeader('//  Warning: renamed enum type ''%s'' to ''%s''',[BstrName,senum],True);
      if (InterfaceSection.IndexOf(Format('  %s =LongWord;',[senum]))<>-1) then  // duplicate enums fe. AXVCL.dll 1.0
        begin
        senum:=senum+IntToStr(i); // index is unique in this typelib
        AddToHeader('//  Warning: duplicate enum ''%s''. Renamed to ''%s''. consts appended with %d',[BstrName,senum,i]);
        bDuplicate:=true;
        end;
      AddToInterface('Type');
      AddToInterface('  %s =LongWord;',[senum]);
      FTypes.Add(senum);
      FDeclared.Add(senum);
      AddToInterface('Const');
      for j:=0 to TA^.cVars-1 do
        begin
        TI.GetVarDesc(j,VD);
        if assigned(VD) then
          begin
          TI.GetDocumentation(VD^.memId,@BstrName, nil, nil, nil);
          if bDuplicate then
            sl:=BstrName+IntToStr(i)
          else
            sl:=BstrName;
          if assigned(VD^.lpvarValue) then
            begin
            if not MakeValidIdAgainstDeclared(sl,sl) then
              AddToHeader('//  Warning: renamed enum member ''%s'' of ''%s'' to ''%s''',[BstrName,senum,sl],True);
            AddToInterface('  %s = $%s;',[sl,IntToHex(PtrInt(VD^.lpvarValue^),16)]);
            FDeclared.Add(sl);
            end;
          end;
        end;
      end;
    TI.ReleaseTypeAttr(TA);
    end;
end;

Procedure TTypeLibImporter.CreateForwards(Const TL : ITypeLib; TICount : Integer);

Var
  i, j: integer;
  BstrName, BstrNameRef : WideString;
//  dwHelpContext: DWORD;
  TI, TIref:ITypeInfo;
  TA:LPTYPEATTR;
  TIT: TYPEKIND;
  RTIT : HREFTYPE;
  ITF:WINT;
  sl,slref:string;

begin
  // Forward declarations
  AddToInterface('//Forward declarations');
  AddToInterface('');
  AddToInterface('Type');
  for i:=0 to TIcount-1 do
    begin
    OleCheck(TL.GetTypeInfoType(i, TIT));
    OleCheck(TL.GetTypeInfo(i, TI));
    OleCheck(TL.GetDocumentation(i, @BstrName, nil, nil, nil));
    OleCheck(TI.GetTypeAttr(TA));
    if (TIT=TKIND_DISPATCH) then
      begin
      if not MakeValidId(BstrName,sl) then
        AddToHeader('//  Warning: renamed interface ''%s'' to ''%s''',[BstrName,sl],True);
      if (TA^.wTypeFlags and TYPEFLAG_FDUAL)=TYPEFLAG_FDUAL then
        begin
        AddToInterface(' %s = interface;',[sl]);
        AddToInterFace(' %sDisp = dispinterface;',[sl]);
        FDeclared.Add(sl+'disp');
        end
      else
        AddToInterface(' %s = dispinterface;',[sl]);
      end
    else if (TIT=TKIND_INTERFACE) then
      begin
      if not MakeValidId(BstrName,sl) then
        AddToHeader('//  Warning: renamed interface ''%s'' to ''%s''',[BstrName,sl],True);
      AddToInterface(' %s = interface;',[sl]);
      end;
    TI.ReleaseTypeAttr(TA);
    end;
  // Default interfaces for Co Classes
  AddToInterface('');
  AddToInterface('//Map CoClass to its default interface');
  AddToInterface('');
  for i:=0 to TIcount-1 do
    begin
    OleCheck(TL.GetTypeInfoType(i, TIT));
    OleCheck(TL.GetTypeInfo(i, TI));
    OleCheck(TL.GetDocumentation(i, @BstrName, nil, nil, nil));
    OleCheck(TI.GetTypeAttr(TA));
    if (TIT=TKIND_COCLASS) then
      begin //find default interface
      if not MakeValidId(BstrName,sl) then
        AddToHeader('//  Warning: renamed coclass ''%s'' to ''%s''',[BstrName,sl],True);
      for j:=0 to TA^.cImplTypes-1 do
        begin
        OleCheck(TI.GetImplTypeFlags(J,ITF));
        if (ITF and (IMPLTYPEFLAG_FDEFAULT or IMPLTYPEFLAG_FSOURCE))= IMPLTYPEFLAG_FDEFAULT then
          begin
          OleCheck(TI.GetRefTypeOfImplType(J,RTIT));
          OleCheck(TI.GetRefTypeInfo(RTIT,TIref));
          OleCheck(TIRef.GetDocumentation(DISPID_UNKNOWN, @BstrNameRef, nil, nil, nil));
          MakeValidId(BstrNameRef,slRef);
          AddToInterface(' %s = %s;',[sl,slRef]);
          break;
          end;
        end;
      end;
    TI.ReleaseTypeAttr(TA);
    end;
end;

Procedure TTypeLibImporter.CreateRecordsUnionsAliases(Const TL : ITypeLib; TICount : Integer);

Var
  i,j : integer;
  BstrName, BstrDocString, BstrHelpFile : WideString;
  dwHelpContext: DWORD;
  TI:ITypeInfo;
  TA:LPTYPEATTR;
  TIT: TYPEKIND;
  VD: lpVARDESC;
  slDeferredType,slDeferredPendingType,slDeferredDeclaration:TStrings;
  sl,sldeclaration,stype,smembername,srecordname:string;
  bIsDeferred:boolean;

  procedure ReleasePendingType(sPen:string);
  var k:integer;
    sDec,sTyp:string;
  begin
    k:=slDeferredPendingType.IndexOf(sPen);
    while (k>=0) do
      begin
      sDec:=slDeferredDeclaration[k];
      sTyp:=slDeferredType[k];
      slDeferredPendingType.Delete(k);
      slDeferredDeclaration.Delete(k);
      slDeferredType.Delete(k);
      // any other types pending for this declaration ? If yes, wait until all types declared.
      if slDeferredDeclaration.IndexOf(sDec)=-1 then
        begin
        AddToInterface(sDec);
        FTypes.Add(sTyp);
        FDeclared.Add(sTyp);
        ReleasePendingType(sTyp);
        end;
      k:=slDeferredPendingType.IndexOf(sPen);
      end;
  end;

begin
  //records, unions aliases
  AddToInterface('');
  AddToInterface('//records, unions, aliases');
  AddToInterface('');
  slDeferredType:=TStringList.Create;
  slDeferredPendingType:=TStringList.Create;
  slDeferredDeclaration:=TStringList.Create;
  try
  for i:=0 to TIcount-1 do
    begin
    bIsDeferred:=false;
    sldeclaration:='';
    OleCheck(TL.GetTypeInfoType(i, TIT));
    //s:=s+format('type %d'#13#10,[ord(TIT)]);
    OleCheck(TL.GetTypeInfo(i, TI));
    OleCheck(TL.GetDocumentation(i, @BstrName, @BstrDocString, @dwHelpContext, @BstrHelpFile));
    OleCheck(TI.GetTypeAttr(TA));
    case TIT of
      TKIND_RECORD,TKIND_UNION:
        begin
        if not MakeValidId(RemoveTag(BstrName),sRecordName) then
          AddToHeader('//  Warning: renamed record ''%s'' to ''%s''',[BstrName,sRecordName],True);
        AddToInterface(' P%s = ^%s;'#13#10,[sRecordName,sRecordName]);
        FTypes.Add('P'+sRecordName);
        FDeclared.Add('P'+sRecordName);
        ReleasePendingType('P'+sRecordName);
        if TIT=TKIND_RECORD then
          sldeclaration:=sldeclaration+format(' %s = packed record'#13#10,[sRecordName])
        else
          begin
          sldeclaration:=sldeclaration+format(' %s =  record'#13#10,[sRecordName]);
          sldeclaration:=sldeclaration+'    case Integer of'#13#10;
          end;
        for j:=0 to TA^.cVars-1 do
          begin
          TI.GetVarDesc(j,VD);
          TI.GetDocumentation(VD^.memId,@BstrName, @BstrDocString, @dwHelpContext, @BstrHelpFile);
          if not MakeValidId(BstrName,smemberName) then
            AddToHeader('//  Warning: renamed record member ''%s'' in %s to ''%s''',[BstrName,sRecordName,smemberName],True);
          stype:=TypeToString(TI, VD^.ElemdescVar.tdesc);
          if bIsUserDefined and not ValidateID(stype) then
            stype:=stype+'_';
          if bIsUserDefined and not bIsExternalDecl and (FTypes.IndexOf(stype)=-1) then //not defined yet, defer
            begin
            bIsDeferred:=true;
            slDeferredPendingType.Add(stype);
            slDeferredType.Add(sRecordName);
            end;
          if TIT=TKIND_RECORD then
            sldeclaration:=sldeclaration+format('     %s : %s;'#13#10,[smemberName,stype])
          else
            sldeclaration:=sldeclaration+format('     %d: (%s : %s);'#13#10,[j,smemberName,stype]);
          end;
        sldeclaration:=sldeclaration+' end;';
        if not bIsDeferred then
          begin
          AddToInterface(sldeclaration);
          FTypes.Add(sRecordName);
          FDeclared.Add(sRecordName);
          ReleasePendingType(sRecordName);
          end
        else
          for j:=slDeferredDeclaration.Count to slDeferredType.Count-1 do  // catch up on slDeferredType
            slDeferredDeclaration.Add(sldeclaration);
        end;
      TKIND_ALIAS:
        begin
        stype:=TypeToString(TI, TA^.tdescAlias);
        if bIsUserDefined and not ValidateID(stype) then
          stype:=stype+'_';
        if not MakeValidId(RemoveTag(BstrName),sRecordName) then
          AddToHeader('//  Warning: renamed alias ''%s'' to ''%s''',[BstrName,sRecordName],True);
        sl:=format(' %s = %s;',[sRecordName,stype]);
        if bIsUserDefined and not bIsExternalDecl and (FTypes.IndexOf(stype)=-1) then //not defined yet, defer
          begin
          slDeferredDeclaration.Add(sl);
          slDeferredPendingType.Add(stype);
          slDeferredType.Add(sRecordName);
          end
        else
          begin
          AddToInterface(sl);
          FTypes.Add(sRecordName);
          FDeclared.Add(sRecordName);
          ReleasePendingType(sRecordName);
          end;
        end;
      end;
    TI.ReleaseTypeAttr(TA);
    end;
  if slDeferredDeclaration.Count>1 then  // circular references
    begin
    AddToHeader('//  Error : the following type declarations have circular or unresolved references',True);
    AddToInterface('// circular or unresolved references start here');
    for j:=0 to slDeferredDeclaration.Count-1 do
      AddToHeader('//          %s',[slDeferredType[j]]);
    for j:=0 to slDeferredDeclaration.Count-1 do
      AddToInterface(slDeferredDeclaration[j]);
    end;
  finally
    slDeferredDeclaration.Free;
    slDeferredPendingType.Free;
    slDeferredType.Free;
  end;
end;

Procedure TTypeLibImporter.CreateInterfaces(Const TL : ITypeLib; TICount : Integer);

Var
  i : integer;
  BstrName, BstrDocString, BstrHelpFile, BstrNameRef : WideString;
  dwHelpContext : DWORD;
  TI,TIref,TIref2 : ITypeInfo;
  TA,TAref: LPTYPEATTR;
  //TAref2 : LPTYPEATTR;
  TIT : TYPEKIND;
  RTIT : HREFTYPE;
  sl: string;
  slDeclaredType,slDeferredType,slDeferredPendingType,slDeferredDeclaration: Tstrings;
  bDeferred:boolean;

  procedure ReleasePendingType(sPen:string);
  var k:integer;
    sDec,sTyp:string;
  begin
    slDeclaredType.Add(sPen);
    k:=slDeferredPendingType.IndexOf(sPen);
    while (k>=0) do
      begin
      sDec:=slDeferredDeclaration[k];
      sTyp:=slDeferredType[k];
      slDeferredPendingType.Delete(k);
      slDeferredDeclaration.Delete(k);
      slDeferredType.Delete(k);
      AddToInterface(sDec);
      ReleasePendingType(sTyp);
      k:=slDeferredPendingType.IndexOf(sPen);
      end;
  end;

begin
  // interface declarations
  slDeclaredType:=TStringList.Create;
  slDeferredType:=TStringList.Create;
  slDeferredPendingType:=TStringList.Create;
  slDeferredDeclaration:=TStringList.Create;
  slDeclaredType.Add('IDispatch');
  slDeclaredType.Add('IUnknown');
  try
    AddToInterface('');
    AddToInterface('//interface declarations');
    for i:=0 to TIcount-1 do
      begin
      OleCheck(TL.GetTypeInfoType(i, TIT));
      OleCheck(TL.GetTypeInfo(i, TI));
      OleCheck(TL.GetDocumentation(i, @BstrName, @BstrDocString, @dwHelpContext, @BstrHelpFile));
      if (TIT=TKIND_DISPATCH) or (TIT=TKIND_INTERFACE) then
        begin
        OleCheck(TI.GetTypeAttr(TA));
        bDeferred:=false;
        MakeValidId(BstrName,sl);
        if (TIT=TKIND_DISPATCH) then
          begin
          // get also TKIND_INTERFACE if dual interface
          if (TA^.wTypeFlags and TYPEFLAG_FDUAL)=TYPEFLAG_FDUAL then
            begin
            //get TKIND_INTERFACE
            OleCheck(TI.GetRefTypeOfImplType($ffffffff,RTIT));
            OleCheck(TI.GetRefTypeInfo(RTIT,TIref));
            //get its ancestor
            OleCheck(TIref.GetRefTypeOfImplType(0,RTIT));
            OleCheck(TIref.GetRefTypeInfo(RTIT,TIref2));
            OleCheck(TIRef2.GetDocumentation(DISPID_UNKNOWN, @BstrNameRef, nil, nil, nil));
            bDeferred:=slDeclaredType.IndexOf(BstrNameRef)<0;
            OleCheck(TIref.GetTypeAttr(TAref));
            if bDeferred then
              begin
              slDeferredType.Add(sl);
              slDeferredPendingType.Add(BstrNameRef);
              slDeferredDeclaration.Add(interfacedeclaration(sl,BstrDocString,TIref,TAref,false,false)+
                interfacedeclaration(sl,BstrDocString,TI,TA,true,false));
              end
            else
              begin
              AddToInterface(interfacedeclaration(sl,BstrDocString,TIref,TAref,false,false));
              AddToInterface(interfacedeclaration(sl,BstrDocString,TI,TA,true,false));
              ReleasePendingType(sl);
              end;
            TIref.ReleaseTypeAttr(TAref);
            end
          else
            AddToInterface(interfacedeclaration(sl,BstrDocString,TI,TA,true,true));
          end
        else
          begin
          if (TA^.cImplTypes>0) then
            begin
            //get ancestor
            OleCheck(TI.GetRefTypeOfImplType(0,RTIT));
            OleCheck(TI.GetRefTypeInfo(RTIT,TIref));
            OleCheck(TIRef.GetDocumentation(DISPID_UNKNOWN, @BstrNameRef, nil, nil, nil));
            bDeferred:=slDeclaredType.IndexOf(BstrNameRef)<0;
            end;
          if bDeferred then
            begin
            slDeferredType.Add(sl);
            slDeferredPendingType.Add(BstrNameRef);
            slDeferredDeclaration.Add(interfacedeclaration(sl,BstrDocString,TI,TA,false,false));
            end
          else
            begin
            AddToInterface(interfacedeclaration(sl,BstrDocString,TI,TA,false,false));
            ReleasePendingType(sl);
            end;
          end;
        TI.ReleaseTypeAttr(TA);
        end;
      end;
    for i:=0 to slDeferredPendingType.Count-1 do // should not happen
      begin
      AddToInterface('// should not happen');
      AddToInterface(slDeferredDeclaration[i]);
      end;
  finally
    slDeferredDeclaration.Free;
    slDeferredPendingType.Free;
    slDeferredType.Free;
    slDeclaredType.Free;
  end;
end;

Procedure TTypeLibImporter.CreateCoClasses(Const TL : ITypeLib; TICount : Integer);

Var
  i, j ,idx: integer;
  BstrName, BstrDocString, BstrHelpFile, BstrNameRef : WideString;
  dwHelpContext : DWORD;
  TI,TIref : ITypeInfo;
  TA,TARef : LPTYPEATTR;
  TIT : TYPEKIND;
  RTIT : HREFTYPE;
  sDefIntf, sDefEvents : string;
  ITF:WINT;
  RegHandle:HKEY;
  il,il2:LongWord;
  sRefSrc,sKey,sl:string;
  resHandle:hmodule;
  bmhandle:handle;
  pData:pByte;
  bIsDispatch:boolean;
begin
  //CoClasses
  AddToInterface('//CoClasses');
  AddToImplementation('implementation');
  AddToImplementation('');
  AddToImplementation('uses comobj;');
  AddToImplementation('');
  for i:=0 to TIcount-1 do
    begin
    OleCheck(TL.GetTypeInfoType(i, TIT));
    if TIT =TKIND_COCLASS then
      begin
      OleCheck(TL.GetTypeInfo(i, TI));
      OleCheck(TL.GetDocumentation(i, @BstrName, @BstrDocString, @dwHelpContext, @BstrHelpFile));
      OleCheck(TI.GetTypeAttr(TA));
      // get default interface and events.
      sDefEvents:='';
      bIsDispatch:=false;
      for j:=0 to TA^.cImplTypes-1 do
        begin
        OleCheck(TI.GetImplTypeFlags(J,ITF));
        if (ITF and IMPLTYPEFLAG_FDEFAULT)<>0 then
          begin
          OleCheck(TI.GetRefTypeOfImplType(J,RTIT));
          OleCheck(TI.GetRefTypeInfo(RTIT,TIref));
          OleCheck(TIRef.GetDocumentation(DISPID_UNKNOWN, @BstrNameRef, nil, nil, nil));
          if (ITF and IMPLTYPEFLAG_FSOURCE)<>0 then
            begin
            MakeValidId(BstrNameRef,sDefEvents);
            idx:=FEventDisp.IndexOf(sDefEvents);
            if idx<0 then // should not happen
              sDefEvents:=''
            else
              if FEventSignatures[idx]='' then //interface defined but no events
                sDefEvents:='';
            end
          else
            begin
            MakeValidId(BstrNameRef,sDefIntf);
            // is this a dispinterface?
            OleCheck(TIRef.GetTypeAttr(TARef));
            bIsDispatch:= TARef^.typekind=TKIND_DISPATCH;
            end;
          end;
        end;
      if bIsDispatch and (sDefEvents<>'') and (FEventSignatures[idx]<>'already defined') then //add event signatures
        begin
        AddToInterface(FEventSignatures[idx]);
        FEventSignatures[idx]:='already defined';  // add event signatures only once. Multiple coclasses can use same events
        AddToInterface('');
        end;
      AddToInterFace('  Co%s = Class',[BstrName]);
      AddToInterface('  Public');
      AddToInterface('    Class Function Create: %s;',[sDefIntf]);
      AddToInterFace('    Class Function CreateRemote(const MachineName: string): %s;',[sDefIntf]);
      AddToInterFace('  end;');
      AddToInterFace('');
      if FActiveX and bIsDispatch then
        begin
        if FUses.IndexOf('ActiveXContainer')<0 then
          FUses.Add('ActiveXContainer');
        AddToInterFace('  TAxc%s = Class(TActiveXContainer)',[BstrName]);
        AddToInterface('  Private');
        AddToInterface('    FServer:%s;',[sDefIntf]);
        if (sDefEvents<>'') then //add function variables
          begin
          if FUses.IndexOf('Eventsink')<0 then
            FUses.Add('EventSink');
          AddToInterface(FEventFunctions[idx]);
          AddToInterface('    FEventSink:TEventSink;',[sDefIntf]);
          AddToInterface('    procedure EventSinkInvoke(Sender: TObject; DispID: Integer;');
          AddToInterface('          const IID: TGUID; LocaleID: Integer; Flags: Word;');
          AddToInterface('          Params: tagDISPPARAMS; VarResult, ExcepInfo, ArgErr: Pointer);');
          end;
        AddToInterface('  Public');
        AddToInterface('    constructor Create(TheOwner: TComponent); override;');
        AddToInterface('    destructor Destroy; override;');
        AddToInterface('    property OleServer:%s read FServer;',[sDefIntf]);
        AddToInterFace('  Published');
        AddToInterFace('    property Align;');
        AddToInterFace('    property Anchors;');
        AddToInterFace('    property AutoSize;');
        AddToInterFace('    property BorderSpacing;');
        AddToInterFace('    property ChildSizing;');
        AddToInterFace('    property ClientHeight;');
        AddToInterFace('    property ClientWidth;');
        AddToInterFace('    property Constraints;');
        AddToInterFace('    property DockSite;');
        AddToInterFace('    property DragCursor;');
        AddToInterFace('    property DragKind;');
        AddToInterFace('    property DragMode;');
        AddToInterFace('    property Enabled;');
        AddToInterFace('    property ParentShowHint;');
        AddToInterFace('    property PopupMenu;');
        AddToInterFace('    property ShowHint;');
        AddToInterFace('    property TabOrder;');
        AddToInterFace('    property TabStop;');
        AddToInterFace('    property UseDockManager default True;');
        AddToInterFace('    property Visible;');
        AddToInterFace('    property OnContextPopup;');
        AddToInterFace('    property OnDockDrop;');
        AddToInterFace('    property OnDockOver;');
        AddToInterFace('    property OnDragDrop;');
        AddToInterFace('    property OnDragOver;');
        AddToInterFace('    property OnEndDock;');
        AddToInterFace('    property OnEndDrag;');
        AddToInterFace('    property OnEnter;');
        AddToInterFace('    property OnExit;');
        AddToInterFace('    property OnGetSiteInfo;');
        AddToInterFace('    property OnGetDockCaption;');
        AddToInterFace('    property OnResize;');
        AddToInterFace('    property OnStartDock;');
        AddToInterFace('    property OnStartDrag;');
        AddToInterFace('    property OnStatusText;');
        AddToInterFace('    property OnUnDock;');
        if (sDefEvents<>'') then
          AddToInterface(FEventProperties[idx]);
        AddToInterFace('    property Active;');
        AddToInterFace('  end;');
        AddToInterFace('');
        end
      else if bIsDispatch and (sDefEvents<>'') then //add event sink descendant
        begin
        if FUses.IndexOf('Eventsink')<0 then
          FUses.Add('EventSink');
        AddToInterFace('  TEvs%s = Class(TEventSink)',[BstrName]);
        AddToInterface('  Private');
        AddToInterface(FEventFunctions[idx]);
        AddToInterface('    fServer:%s;',[sDefIntf]);
        AddToInterface('    procedure EventSinkInvoke(Sender: TObject; DispID: Integer;');
        AddToInterface('          const IID: TGUID; LocaleID: Integer; Flags: Word;');
        AddToInterface('          Params: tagDISPPARAMS; VarResult, ExcepInfo, ArgErr: Pointer);');
        AddToInterface('  Public');
        AddToInterface('    constructor Create(TheOwner: TComponent); override;');
        AddToInterface('    property ComServer:%s read fServer;',[sDefIntf]);
        AddToInterface(FEventProperties[idx]);
        AddToInterFace('  end;');
        AddToInterFace('');
        end;
      AddToImplementation('Class Function Co%s.Create: %s;',[BstrName,sDefIntf]);
      AddToImplementation('begin');
      AddToImplementation('  Result := CreateComObject(CLASS_%s) as %s;',[BstrName,sDefIntf]);
      AddToImplementation('end;');
      AddToImplementation('');
      AddToImplementation('Class Function Co%s.CreateRemote(const MachineName: string): %s;',[BstrName,sDefIntf]);
      AddToImplementation('begin');
      AddToImplementation('  Result := CreateRemoteComObject(MachineName,CLASS_%s) as %s;',[BstrName,sDefIntf]);
      AddToImplementation('end;');
      AddToImplementation('');
      if FActiveX and bIsDispatch then
        begin
        AddToImplementation('constructor TAxc%s.Create(TheOwner: TComponent);',[BstrName]);
        AddToImplementation('begin');
        AddToImplementation('  inherited Create(TheOwner);');
        AddToImplementation('  FServer:=Co%s.Create;',[BstrName]);
        AddToImplementation('  ComServer:=FServer;',[BstrName]);
        if (sDefEvents<>'') then
          begin
          AddToImplementation('  FEventSink:=TEventSink.Create(Self);');
          AddToImplementation('  FEventSink.OnInvoke:=EventSinkInvoke;');
          AddToImplementation('  FEventSink.Connect(FServer,%s);',[FEventDisp[idx]]);
          end;
        AddToImplementation('end;');
        AddToImplementation('');
        AddToImplementation('destructor TAxc%s.Destroy;',[BstrName]);
        AddToImplementation('begin');
        if (sDefEvents<>'') then
          AddToImplementation('  FEventSink.Destroy;');
        AddToImplementation('  inherited destroy;');
        AddToImplementation('end;');
        AddToImplementation('');
        if (sDefEvents<>'') then
          begin
          AddToImplementation('procedure TAxc%s.EventSinkInvoke(Sender: TObject; DispID: Integer;',[BstrName]);
          AddToImplementation('  const IID: TGUID; LocaleID: Integer; Flags: Word; Params: tagDISPPARAMS;');
          AddToImplementation('  VarResult, ExcepInfo, ArgErr: Pointer);');
          AddToImplementation('begin');
          AddToImplementation('  case DispID of');
          AddToImplementation(FEventImplementations[idx]);
          AddToImplementation('  end;');
          AddToImplementation('end;');
          AddToImplementation('');
          end;
        end
      else if (sDefEvents<>'') and bIsDispatch then //add event implementations
        begin
        AddToImplementation('constructor TEvs%s.Create(TheOwner: TComponent);',[BstrName]);
        AddToImplementation('begin');
        AddToImplementation('  inherited Create(TheOwner);');
        AddToImplementation('  OnInvoke:=EventSinkInvoke;');
        AddToImplementation('  fServer:=Co%s.Create;',[BstrName]);
        AddToImplementation('  Connect(fServer,%s);',[FEventDisp[idx]]);
        AddToImplementation('end;');
        AddToImplementation('');
        AddToImplementation('procedure TEvs%s.EventSinkInvoke(Sender: TObject; DispID: Integer;',[BstrName]);
        AddToImplementation('  const IID: TGUID; LocaleID: Integer; Flags: Word; Params: tagDISPPARAMS;');
        AddToImplementation('  VarResult, ExcepInfo, ArgErr: Pointer);');
        AddToImplementation('begin');
        AddToImplementation('  case DispID of');
        AddToImplementation(FEventImplementations[idx]);
        AddToImplementation('  end;');
        AddToImplementation('end;');
        AddToImplementation('');
        end;
      if CreatePackage and bIsDispatch then
        begin
        //get image location from registry
        il:=MAX_PATH;
        SetLength(sRefSrc,il);
        sKey:=format('\CLSID\%s\ToolboxBitmap32',[GUIDToString(TA^.GUID)]);
        bmhandle:=0;
        if (RegOpenKeyEx(HKEY_CLASSES_ROOT,pchar(sKey),0,KEY_READ,RegHandle) = ERROR_SUCCESS) then
          begin
          if RegQueryValue(RegHandle,nil,@sRefSrc[1],@il) = ERROR_SUCCESS then
            begin
            SetLength(sRefSrc,il-1);  // includes null terminator
            sl:=trim(copy(sRefSrc,pos(',',sRefSrc)+1,length(sRefSrc))); //format: filename, id
            sRefSrc:=copy(sRefSrc,1,pos(',',sRefSrc)-1);
            //Load bitmap
            ResHandle:=LoadLibraryExA(pchar(sRefSrc),0,$00000022); //LOAD_LIBRARY_AS_IMAGE_RESOURCE or LOAD_LIBRARY_AS_DATAFILE
            if (ResHandle<>0) then
              begin
              bmhandle:=FindResource(ResHandle,makeintresource(StrToIntDef(sl,0)),RT_BITMAP);
              if bmhandle<>0 then
                begin
                // get pointer to raw bitmap data
                pData:=LockResource(LoadResource(ResHandle, bmhandle));
                // convert to ascii. These are small bitmaps, don't bother making smallest possible
                il:=SizeofResource(ResHandle, bmhandle);
                //pixel offset=size-16*2*bits/color  (16x16 bitmap)
                il2:=14+il-32*pbyte(pData+14)^;
                //bmp header
                sl:='#$42#$4D#'+IntTostr((14+il) mod 256) +'#'+IntTostr((14+il) div 256)
                  +'#0#0#0#0#0#0#'+IntTostr(il2 mod 256) +'#'+IntTostr(il2 div 256)
                  +'#0#0+'#13#10;
                while il>0 do
                  begin
                  sl:=sl+'#'+inttostr(pData^);
                  pData:=pData+1;
                  il:=il-1;
                  if ((il mod 16)=0) and (il>0) then
                    sl:=sl+'+'#13#10;
                  end;
                sl:=format('LazarusResources.Add(''TAxc%s'',''BMP'',['#13#10,[BstrName])
                    + sl + #13#10']);'#13#10;
                FAXImages.Add(sl);
                end;
              FreeLibrary(ResHandle);
              end;
            end
          else
          RegCloseKey(RegHandle);
          end;
        FAXClasses.Add(format('TAxc%s',[BstrName]));
        if FAXImages.Count<FAXClasses.Count then //couldn't load image for this coclass
          FAXImages.Add('');
        end;
      TI.ReleaseTypeAttr(TA);
      end;
    end;
end;

Procedure TTypeLibImporter.CreateUnitHeader(Const TL : ITypeLib; const LA: lpTLIBATTR);


Var
  BstrName, BstrDocString, BstrHelpFile : WideString;
  dwHelpContext: DWORD;

begin
  OleCheck(TL.GetDocumentation(-1, @BstrName, @BstrDocString, @dwHelpContext, @BstrHelpFile));
  if (UnitName='') then
    if FAppendVersionNumber then
      UnitName:=format('%s_%d_%d_TLB',[BstrName,LA^.wMajorVerNum,LA^.wMinorVerNum])
    else
      UnitName:=format('%s_TLB',[BstrName]);
  //header
  AddToHeader('Unit %s;',[UnitName],True);
  AddToHeader('',true);
  AddToHeader('//  Imported %s on %s from %s',[BstrName,DateTimeToStr(Now()),InputFilename],True);
  AddToHeader('',true);
  AddToHeader('{$mode delphi}{$H+}',true);
  AddToHeader('',true);
  AddToHeader('interface',true);
  AddToHeader('',true);
  FUses.Add('Windows');
  FUses.Add('ActiveX');
  FUses.Add('Classes');
  //FUses.Add('OleServer');
  FUses.Add('Variants');
  AddToInterface('Const');
  AddToInterface('  %sMajorVersion = %d;',[BstrName,LA^.wMajorVerNum]);
  AddToInterface('  %sMinorVersion = %d;',[BstrName,LA^.wMinorVerNum]);
  AddToInterface('  %sLCID = %x;',[BstrName,LA^.LCID]);
  AddToInterface('  LIBID_%s : TGUID = ''%s'';',[BstrName,GUIDToString(LA^.GUID)]);
  AddToInterface('');
end;

Procedure TTypeLibImporter.DoImportTypelib;

var
  TL: ITypelib;
  TIcount:integer;
  LA: lpTLIBATTR;

begin
  Header.Clear;
  InterfaceSection.Clear;
  TL:=nil;
  OleCheck(LoadTypeLib(PWidechar(InputFileName), TL ));
  OleCheck(TL.GetLibAttr(LA));
  try
    CreateUnitHeader(TL,LA);
    TIcount:=TL.GetTypeInfoCount;
    ImportGUIDs(TL,TICount);
    ImportEnums(TL,TICount);
    CreateForwards(TL,TICount);
    CreateRecordsUnionsAliases(TL,TICount);
    CreateInterFaces(TL,TICount);
    CreateCoClasses(TL,TICount);
  finally
    TL.ReleaseTLibAttr(LA);
  end;
  BuildUnit;
end;

procedure TTypeLibImporter.BuildUnit;

Var
  l : string;
  I : Integer;

begin
  UnitSource.AddStrings(Header);
  UnitSource.Add('Uses');
  L:='  ';
  For I:=0 to FUses.Count-1 do
    begin
    L:=L+FUses[i];
    If (I<Fuses.Count-1) then
      L:=L+','
    else
      L:=L+';';
    if (Length(L)>72) then
      begin
      UnitSource.Add(L);
      L:='  ';
      end;
    end;
  if (L<>'  ') then
    UnitSource.Add(L);
  UnitSource.addStrings(InterfaceSection);
  UnitSource.addStrings(ImplementationSection);
  UnitSource.Add('end.');
end;

{ TTypeLibImporter }

procedure TTypeLibImporter.SetOutputFileName(AValue: String);

Var
  UN : String;

begin
  if FOutputFileName=AValue then Exit;
  UN:=ChangeFileExt(ExtractFileName(AValue),'');
  if not IsValidIdent(UN) then
    Raise Exception.CreateFmt(SErrInvalidUnitName,[UN]);
  FOutputFileName:=AValue;
  SetUnitName(UN)
end;

procedure TTypeLibImporter.SetRemoveStructTag(AValue: Boolean);
begin
  if FRemoveStructTag=AValue then Exit;
  FRemoveStructTag:=AValue;
end;

procedure TTypeLibImporter.SetUnitName(AValue: string);
begin
  if FUnitname=AValue then Exit;
  if not IsValidIdent(AVAlue) then
    Raise Exception.CreateFmt(SErrInvalidUnitName,[AValue]);
  FUnitname:=AValue;
  if (OutputFileName<>'') then
    OutputFileName:=ExtractFilePath(OutputFileName)+UnitName+'.pas';
end;

procedure TTypeLibImporter.AddToUses(const AUnit: String);
begin
  If FUses.IndexOf(AUnit)=-1 then
    FUses.add(AUnit);
end;

procedure TTypeLibImporter.AddToHeader(const ALine: String;
  AllowDuplicate: Boolean);

begin
  If (AllowDuplicate) or (FHeader.IndexOf(ALine)=-1) then
    FHeader.Add(ALine);
end;

procedure TTypeLibImporter.AddToHeader(const Fmt: String; Args: array of const;
  AllowDuplicate: Boolean);
begin
  AddToheader(Format(Fmt,Args),AllowDuplicate)
end;

procedure TTypeLibImporter.AddToInterface(const ALine: String);
begin
  FInterface.Add(ALine);
end;

procedure TTypeLibImporter.AddToInterface(const Fmt: String;
  Args: array of const);
begin
  FInterface.Add(Format(Fmt,Args));
end;

procedure TTypeLibImporter.AddToImplementation(const ALine: String);
begin
  FImplementation.Add(ALine);
end;

procedure TTypeLibImporter.AddToImplementation(const Fmt: String;
  Args: array of const);
begin
  FImplementation.Add(Format(Fmt,Args));
end;

constructor TTypeLibImporter.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDependencies:=TStringList.Create;
  FUnitSource:=TStringList.Create;
  FPackageSource:=TStringList.Create;
  FPackageRegUnitSource:=TStringList.Create;
  FAppendVersionNumber:=true;
end;

destructor TTypeLibImporter.Destroy;
begin
  FreeAndNil(FDependencies);
  FreeAndNil(FUnitSource);
  FreeAndNil(FPackageSource);
  FreeAndNil(FPackageRegUnitSource);
  inherited Destroy;
end;

procedure TTypeLibImporter.Execute;
begin
  FDependencies.Clear;
  FUnitSource.Clear;
  FHeader:=TStringList.Create;
  FInterface:=TStringList.Create;
  FImplementation:=TStringList.Create;
  FUses:=TStringList.Create;
  FDeclared:=TStringList.Create;
  FTypes:=TStringList.Create;
  FEventDisp:=TStringList.Create;
  FEventIID:=TStringList.Create;
  FEventSignatures:=TStringList.Create;
  FEventFunctions:=TStringList.Create;
  FEventProperties:=TStringList.Create;
  FEventImplementations:=TStringList.Create;
  FAXClasses:=TStringList.Create;
  FAXImages:=TStringList.Create;
  try
    DoImportTypeLib;
    If (OutputFileName<>'') then
      UnitSource.SaveToFile(OutputFileName);
    If (CreatePackage) then
      DoBuildPackage;
  finally
    FreeAndNil(FAXImages);
    FreeAndNil(FAXClasses);
    FreeAndNil(FEventImplementations);
    FreeAndNil(FEventProperties);
    FreeAndNil(FEventFunctions);
    FreeAndNil(FEventSignatures);
    FreeAndNil(FEventIID);
    FreeAndNil(FEventDisp);
    FreeAndNil(FTypes);
    FreeAndNil(FDeclared);
    FreeAndNil(FUses);
    FreeAndNil(FInterface);
    FreeAndNil(FHeader);
    FreeAndNil(FImplementation);
  end;
end;

end.

