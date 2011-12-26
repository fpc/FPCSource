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

By default, the type library is extracted from the first resource of type ITypeLib.
To load a different type of library resource, append an integer index to 'FileName'.

Example:  C:\WINDOWS\system32\msvbvm60.dll\3
}
function ImportTypelib(FileName: WideString;var sUnitName:string;var slDependencies:TStringList):string;


Type

  { TTypeLibImporter }

  TTypeLibImporter = Class(TComponent)
  private
    FAppendVersionNumber: Boolean;
    FDependencies: TStringList;
    FUnitSource: TStringList;
    FInputFileName: WideString;
    FOutputFileName: String;
    FUnitname: string;
    FUses : TStrings;
    FHeader : TStrings;
    FInterface : TStrings;
    FImplementation : TStrings;
    procedure CreateCoClasses(const TL: ITypeLib; TICount: Integer);
    procedure CreateForwards(const TL: ITypeLib; TICount: Integer);
    procedure CreateInterfaces(const TL: ITypeLib; TICount: Integer);
    procedure CreateRecordsUnionsAliases(const TL: ITypeLib; TICount: Integer);
    procedure CreateUnitHeader(const TL: ITypeLib; const LA: lpTLIBATTR);
    function GetDependencies: TStrings;
    function GetUnitSource: TStrings;
    procedure ImportEnums(const TL: ITypeLib; TICount: Integer);
    procedure ImportGUIDs(const TL: ITypeLib; TICount: Integer);
    procedure SetOutputFileName(AValue: String);
    procedure SetUnitName(AValue: string);
  Protected
    bIsCustomAutomatable,bIsInterface,bIsAutomatable:boolean;
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
    function interfacedeclaration(iName, iDoc: string; TI: ITypeInfo; TA: LPTYPEATTR; bIsDispatch: boolean): string;
    function VarTypeIsAutomatable(ParamType: integer): boolean; virtual;
    function VarTypeToStr(ParamType: integer): string; virtual;
    function TypeToString(TI: ITypeInfo; TD: TYPEDESC): string; virtual;
    function ValidateID(id: string): boolean; virtual;
    // The actual routine that does the work.
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
  Published
    // Append version number to unit name.
    Property AppendVersionNumber : Boolean Read FAppendVersionNumber Write FAppendVersionNumber Default True;
    // File to read typelib from.
    Property InputFileName : WideString Read FInputFileName Write FInputFileName;
    // If set, unit source will be written to this file.
    Property OutputFileName : String Read FOutputFileName Write SetOutputFileName;
    // Set automatically by OutputFileName or by Execute
    Property UnitName : string Read FUnitname Write SetUnitName;
  end;


implementation

Resourcestring
  SErrInvalidUnitName = 'Invalid unit name : %s';

function ImportTypelib(FileName: WideString;var sUnitName:string;var slDependencies:TStringList):string;
var i:integer;
begin
  With TTypeLibImporter.Create(Nil) do
    try
      InputFileName:=FileName;
      Execute;
      Result:=UnitSource.Text;
      sUnitname:=UnitName+'.pas';
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
            VT_BSTR,VT_VARIANT,VT_DISPATCH,VT_UNKNOWN,vt_hresult,VT_INT];
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
  if (TD.vt=vt_userdefined) or ((TD.vt=VT_PTR) and (TD.lptdesc^.vt=vt_userdefined)) then
    begin
    // interface references are dealt with now because they are pointers in fpc.
    // Recursive algorithm makes it difficult to remove a single preceding 'P' from the result.
    bWasPointer:=(TD.vt=VT_PTR);
    if bWasPointer then
      TD:=TD.lptdesc^;
    OleCheck(TI.GetRefTypeInfo(TD.hreftype,TIref));
    OleCheck(TIRef.GetDocumentation(DISPID_UNKNOWN, @BstrName, nil, nil, nil));
    result:=BstrName;
    OleCheck(TIRef.GetTypeAttr(TARef));
    bIsCustomAutomatable:=TARef^.typekind in [TKIND_DISPATCH,TKIND_INTERFACE,TKIND_ENUM];
    if TARef^.typekind=TKIND_ALIAS then
      begin
      TypeToString(TIRef,TARef^.tdescAlias); //not interested in result, only bIsCustomAutomatable and bIsInterface
      bIsCustomAutomatable:=bIsAutomatable;
      end
    else
      bIsInterface:=TARef^.typekind in [TKIND_DISPATCH,TKIND_INTERFACE] ;
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
      if (LARef^.wMajorVerNum=2) and (FUses.IndexOf('stdole2')=-1) then
        begin
        AddToHeader('// Dependency: stdole v2 (stdole2.pas)');
        FUses.Add('stdole2');
        end;
      end
    else if (LowerCase(sl)<>LowerCase(UnitName)) and (FUses.IndexOf(sl)=-1) then
      begin  // add dependency
      // find source in registry key HKEY_CLASSES_ROOT\TypeLib\GUID\version\0\win32
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
        end;
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
    bIsAutomatable:=(VarTypeIsAutomatable(TD.vt) and (TD.vt<>VT_VARIANT)) or bIsCustomAutomatable;
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

function TTypeLibImporter.interfacedeclaration(iName,iDoc:string;TI:ITypeInfo;TA:LPTYPEATTR;bIsDispatch:boolean):string;

type
  TPropertyDef=record
    idispid:integer;
    bput,bget:boolean;
    iptype,igtype:integer;
    name,
    sptype,          // only used if iptype=igtype
    sorgname,
    sdoc,
    sParam,
    sDefault:string;
  end;

var
  RTIT: HREFTYPE;
  TIref: ITypeInfo;
  BstrName,BstrNameRef,BstrDocString : WideString;
  s,sl,sPropIntfc,sPropDispIntfc,sType,sConv,sFunc,sVarName,sMethodName,sPropParam,sPropParam2:string;
  i,j,k:integer;
  FD: lpFUNCDESC;
  BL : array[0..99] of TBstr;
  cnt:LongWord;
  TD: TYPEDESC;
  bPropHasParam,bIsFunction:boolean;
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
      bget:=false;
      name:='';
      iptype:=vt_empty;
      igtype:=vt_empty;
      sptype:='';
      sorgname:='';
      sdoc:='';
      sParam:='';
      sDefault:='';
      end;
  end;

begin
  Propertycnt:=0;
  SetLength(aPropertyDefs,TA^.cFuncs+TA^.cVars);   // worst case, all functions getters or all setters
  result:='TA^.cFuncs';
  if not bIsDispatch then
    begin
    // find base class
    if TA^.cImplTypes>0 then
      begin
      OleCheck(TI.GetRefTypeOfImplType(0,RTIT));
      OleCheck(TI.GetRefTypeInfo(RTIT,TIref));
      OleCheck(TIRef.GetDocumentation(DISPID_UNKNOWN, @BstrNameRef, nil, nil, nil));
      s:=format(#13#10'// %s : %s'#13#10#13#10' %s = interface(%s)'#13#10,[iname,iDoc,iname,BstrNameRef]);
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
  sPropIntfc:='';
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
        if ValidateID(BstrName) then
          sMethodName:=BstrName
        else
          begin
          sMethodName:=BstrName+'_';
          AddToHeader('//  Warning: renamed method ''%s'' in %s to ''%s''',[BstrName,iname,sMethodName],True);
          end;
        bIsFunction:=(bIsDispatch and (FD^.elemdescFunc.tdesc.vt<>VT_VOID)) or
          (not bIsDispatch and (FD^.cParams>0) and ((FD^.lprgelemdescParam[FD^.cParams-1].paramdesc.wParamFlags and PARAMFLAG_FRETVAL ) <>0));
        if bIsFunction then
          sFunc:=format('    // %s : %s '#13#10'   function %s(',[sMethodName,BstrDocString,sMethodName])
        else
          sFunc:=format('    // %s : %s '#13#10'   procedure %s(',[sMethodName,BstrDocString,sMethodName]);
        if bIsFunction and bIsDispatch and not bIsAutomatable then
          begin
          AddToHeader('//  Warning: ''%s'' not automatable in %sdisp.%s',[stype,iname,BstrName],True);
          sType:='{!! '+sType+' !!} OleVariant';
          end;
        // parameters
        for k:=0 to FD^.cParams-1 do
          begin
          if (FD^.lprgelemdescParam[k].paramdesc.wParamFlags and PARAMFLAG_FRETVAL ) <>0 then  //return type
            continue;
          sl:=TypeToString(TI,FD^.lprgelemdescParam[k].tdesc);
          if sMethodName='Clone' then
            sl:=sl;
          if (FD^.lprgelemdescParam[k].tdesc.vt=VT_PTR) and                                        // by ref
            not((FD^.lprgelemdescParam[k].tdesc.lptdesc^.vt=VT_USERDEFINED) and bIsInterface) then // but not pointer to interface
             delete(sl,1,1);
          if bIsDispatch and not bIsAutomatable then
            begin
            AddToHeader('//  Warning: ''%s'' not automatable in %sdisp.%s',[sl,iname,sMethodName],True);
            sl:='{!! '+sl+' !!} OleVariant';
            end;
          if (FD^.lprgelemdescParam[k].tdesc.vt=VT_PTR) and                                          // by ref
              not((FD^.lprgelemdescParam[k].tdesc.lptdesc^.vt=VT_USERDEFINED) and bIsInterface) then // but not pointer to interface
            case FD^.lprgelemdescParam[k].paramdesc.wParamFlags and (PARAMFLAG_FIN or PARAMFLAG_FOUT) of
            PARAMFLAG_FIN or PARAMFLAG_FOUT:sFunc:=sFunc+'var ';
            PARAMFLAG_FOUT:sFunc:=sFunc+'out ';
            PARAMFLAG_FIN:sFunc:=sFunc+'var '; //constref in safecall? TBD
            end;
          if ValidateID(BL[k+1]) then
            sVarName:=BL[k+1]
          else
            begin
            sVarName:=BL[k+1]+'_';
            AddToHeader('//  Warning: renamed parameter ''%s'' in %s.%s to ''%s'''#13#10,[BL[k+1],iname,sMethodName,sVarName],True);
            end;
          sFunc:=sFunc+format('%s:%s;',[sVarName,sl]);
          end;
        // finish interface and dispinterface
        if sFunc[length(sFunc)]=';' then
          sFunc[length(sFunc)]:=')'
        else  // no params
          delete(sFunc,length(sFunc),1);
        if bIsFunction then
          sFunc:=sFunc+format(':%s',[sType]);
        if bIsDispatch then
          s:=s+sFunc+format(';dispid %d;'#13#10,[FD^.memid])
        else
          s:=s+sFunc+format(';%s;'#13#10,[sConv]);
        end;
      INVOKE_PROPERTYGET,INVOKE_PROPERTYPUT,INVOKE_PROPERTYPUTREF :
        // build properties. Use separate string to group properties at end of interface declaration.
        begin
        if ValidateID(BstrName) then
          sMethodName:=BstrName
        else
          begin
          sMethodName:=BstrName+'_';
          AddToHeader('//  Warning: renamed property ''%s'' in %s to ''%s''',[BstrName,iname,sMethodName]);
          end;
        bPropHasParam:=(((FD^.invkind=INVOKE_PROPERTYGET) and (FD^.cParams>0)) or (FD^.cParams>1))
            and ((FD^.lprgelemdescParam[0].paramdesc.wParamFlags and PARAMFLAG_FIN) = PARAMFLAG_FIN) ;
        if (FD^.memid=0) and  bPropHasParam then sl:=' default;' else sl:='';
        sPropParam:='';
        sPropParam2:='';
        if bPropHasParam then
          begin
          sPropParam:=BL[1]+':'+TypeToString(TI,FD^.lprgelemdescParam[0].tdesc);
          end;
        if bIsDispatch then
          begin
          if (TD.vt<>VT_VOID) and not bIsAutomatable then
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
              sType:=sType+' readonly '
            else
              sType:=sType+' writeonly';
            sPropDispIntfc:=sPropDispIntfc+format('    // %s : %s '#13#10'   property %s%s:%s dispid %d;%s'#13#10,
              [BstrName,BstrDocString,sMethodName,sPropParam,sType,FD^.memid,sl]);
            end
          else //remove readonly or writeonly
            delete(sPropDispIntfc,i-11,10);   //10= length(' readonly ')
          end
        else
          begin
          //getters/setters for interface, insert in interface as they come,
          //store in aPropertyDefs to create properties at the end
          if bPropHasParam then
            begin
            sPropParam2:='('+sPropParam+')';
            sPropParam:='['+sPropParam+']';
            end;
          if FD^.invkind=INVOKE_PROPERTYGET then
            begin
            s:=s+format('   function Get_%s%s : %s; %s;'#13#10,[sMethodName,sPropParam2,sType,sConv]);
            with aPropertyDefs[findProperty(FD^.memid)] do
              begin
              bget:=true;
              name:=sMethodName;
              igtype:=itype;
              sptype:=sType;
              sorgname:=BstrName;
              sdoc:=BstrDocString;
              sParam:=sPropParam;
              sDefault:=sl;
              end;
            end
          else
            begin
            if ValidateID(BL[1]) then
              sVarName:=BL[1]
            else
              begin
              sVarName:=BL[1]+'_';
              AddToHeader('//  Warning: renamed parameter ''%s'' in %s.Set_%s to ''%s''',[BL[1],iname,sMethodName,sVarName]);
              end;
            s:=s+format('   procedure Set_%s(const %s:%s); %s;'#13#10,[sMethodName,sVarName,sType,sConv]);
            with aPropertyDefs[findProperty(FD^.memid)] do
              begin
              bput:=true;
              name:=sMethodName;
              iptype:=itype;
              sptype:=sType;
              sorgname:=BstrName;
              sdoc:=BstrDocString;
              sParam:=sPropParam;
              sDefault:=sl;
              end;
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
        if ValidateID(BstrName) then
          sMethodName:=BstrName
        else
          begin
          sMethodName:=BstrName+'_';
          AddToHeader('//  Warning: renamed property ''%s'' in %s to ''%s'''#13#10,[BstrName,iname,sMethodName]);
          end;
        sType:=TypeToString(TI,VD^.ElemdescVar.tdesc);
        sPropDispIntfc:=sPropDispIntfc+format('    // %s : %s '#13#10'   property %s:%s  dispid %d;'#13#10,
          [BstrName,BstrDocString,sMethodName,sType,VD^.memId]);
        end;
      end;
    end;
  if bIsDispatch then
    result:=s + sPropDispIntfc +'  end;'#13#10
  else
    begin
    // add interface properties
    for i:=0 to Propertycnt-1 do
      with aPropertyDefs[i] do
      if (iptype=igtype) or not bget or not bput then
        begin
        s:=s+format('    // %s : %s '#13#10'   property %s%s:%s',[sorgname,sdoc,name,sParam,sptype]);
        if bget then
          s:=s+format(' read Get_%s',[name]);
        if bput then
          s:=s+format(' write Set_%s',[name]);
        s:=s+format(';%s'#13#10,[sDefault]);
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
        end;
      TKIND_COCLASS:
        begin
        AddToInterface('  CLASS_%s : TGUID = ''%s'';',[BstrName,GUIDToString(TA^.GUID)]);
        end;
      end;
    TI.ReleaseTypeAttr(TA);
    end;
end;

Procedure TTypeLibImporter.ImportEnums(Const TL : ITypeLib; TICount : Integer);

Var
  i,j : integer;
  sl : string;
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
      sl:=BstrName;
      if (InterfaceSection.IndexOf(Format('  %s =TOleEnum;',[sl]))<>-1) then  // duplicate enums fe. AXVCL.dll 1.0
        begin
        sl:=sl+IntToStr(i); // index is unique in this typelib
        AddToHeader('//  Warning: duplicate enum ''%s''. Renamed to ''%s''. consts appended with %d',[BstrName,sl,i]);
        bDuplicate:=true;
        end;
      AddToInterface('Type');
      AddToInterface('  %s =TOleEnum;',[sl]);
      AddToInterface('Const');
      for j:=0 to TA^.cVars-1 do
        begin
        TI.GetVarDesc(j,VD);
        if assigned(VD) then
          begin
          TI.GetDocumentation(VD^.memId,@BstrName, nil, nil, nil);
          sl:=BstrName;
          if bDuplicate then
            sl:=sl+IntToStr(i);
          if assigned(VD^.lpvarValue) then
            AddToInterface('  %s = $%s;',[sl,IntToHex(PtrInt(VD^.lpvarValue^),16)]);
          end;
        end;
      end;
    TI.ReleaseTypeAttr(TA);
    end;
end;

Procedure TTypeLibImporter.CreateForwards(Const TL : ITypeLib; TICount : Integer);

Var
  i : integer;
  BstrName, BstrDocString, BstrHelpFile : WideString;
  dwHelpContext: DWORD;
  TI:ITypeInfo;
  TA:LPTYPEATTR;
  TIT: TYPEKIND;

begin
  // Forward declarations
  AddToInterface('//Forward declarations');
  AddToInterface('');
  AddToInterface('Type');
  for i:=0 to TIcount-1 do
    begin
    OleCheck(TL.GetTypeInfoType(i, TIT));
    OleCheck(TL.GetTypeInfo(i, TI));
    OleCheck(TL.GetDocumentation(i, @BstrName, @BstrDocString, @dwHelpContext, @BstrHelpFile));
    OleCheck(TI.GetTypeAttr(TA));
    if (TIT=TKIND_DISPATCH) then
      begin
      if (TA^.wTypeFlags and TYPEFLAG_FDUAL)=TYPEFLAG_FDUAL then
        begin
        AddToInterface(' %s = interface;',[BstrName]);
        AddToInterFace(' %sDisp = dispinterface;',[BstrName]);
        end
      else
        AddToInterface(' %s = dispinterface;',[BstrName]);
      end
    else if (TIT=TKIND_INTERFACE) then
      AddToInterface(' %s = interface;',[BstrName]);
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

begin
  //records, unions aliases
  AddToInterface('');
  AddToInterface('//records, unions, aliases');
  AddToInterface('');

  for i:=0 to TIcount-1 do
    begin
    OleCheck(TL.GetTypeInfoType(i, TIT));
    //s:=s+format('type %d'#13#10,[ord(TIT)]);
    OleCheck(TL.GetTypeInfo(i, TI));
    OleCheck(TL.GetDocumentation(i, @BstrName, @BstrDocString, @dwHelpContext, @BstrHelpFile));
    OleCheck(TI.GetTypeAttr(TA));
    case TIT of
      TKIND_RECORD:
        begin
        AddToInterface(' P%s = ^%s;',[BstrName,BstrName]);
        AddToInterface(' %s = packed record',[BstrName]);
        for j:=0 to TA^.cVars-1 do
          begin
          TI.GetVarDesc(j,VD);
          TI.GetDocumentation(VD^.memId,@BstrName, @BstrDocString, @dwHelpContext, @BstrHelpFile);
          AddToInterface('     %s : %s;',[BstrName,TypeToString(TI, VD^.ElemdescVar.tdesc)]);
          end;
        AddToInterface(' end;');
        end;
      TKIND_ALIAS:
        begin
        AddToInterface('     %s = %s;',[BstrName,TypeToString(TI, TA^.tdescAlias)]);
        end;
      TKIND_UNION:
        begin
        end;
      end;
    TI.ReleaseTypeAttr(TA);
    end;
end;

Procedure TTypeLibImporter.CreateInterfaces(Const TL : ITypeLib; TICount : Integer);

Var
  i : integer;
  BstrName, BstrDocString, BstrHelpFile : WideString;
  dwHelpContext : DWORD;
  TI,TIref : ITypeInfo;
  TA,TAref : LPTYPEATTR;
  TIT : TYPEKIND;
  RTIT : HREFTYPE;

begin
  // interface declarations
  AddToInterface('//interface declarations');
  for i:=0 to TIcount-1 do
    begin
    OleCheck(TL.GetTypeInfoType(i, TIT));
    OleCheck(TL.GetTypeInfo(i, TI));
    OleCheck(TL.GetDocumentation(i, @BstrName, @BstrDocString, @dwHelpContext, @BstrHelpFile));
    if (TIT=TKIND_DISPATCH) or (TIT=TKIND_INTERFACE) then
      begin
      OleCheck(TI.GetTypeAttr(TA));
      if (TIT=TKIND_DISPATCH) then
        begin
        // get also TKIND_INTERFACE if dual interface
        if (TA^.wTypeFlags and TYPEFLAG_FDUAL)=TYPEFLAG_FDUAL then
          begin
          OleCheck(TI.GetRefTypeOfImplType(-1,RTIT));
          OleCheck(TI.GetRefTypeInfo(RTIT,TIref));
          OleCheck(TIref.GetTypeAttr(TAref));
          AddToInterface(interfacedeclaration(BstrName,BstrDocString,TIref,TAref,false));
          TIref.ReleaseTypeAttr(TAref);
          end;
        AddToInterface(interfacedeclaration(BstrName,BstrDocString,TI,TA,true));
        end
      else
        AddToInterface(interfacedeclaration(BstrName,BstrDocString,TI,TA,false));
      TI.ReleaseTypeAttr(TA);
      end;
    end;
end;

Procedure TTypeLibImporter.CreateCoClasses(Const TL : ITypeLib; TICount : Integer);

Var
  i : integer;
  BstrName, BstrDocString, BstrHelpFile, BstrNameRef : WideString;
  dwHelpContext : DWORD;
  TI,TIref : ITypeInfo;
  TA : LPTYPEATTR;
  TIT : TYPEKIND;
  RTIT : HREFTYPE;

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
      OleCheck(TI.GetRefTypeOfImplType(0,RTIT));
      OleCheck(TI.GetRefTypeInfo(RTIT,TIref));
      OleCheck(TIRef.GetDocumentation(DISPID_UNKNOWN, @BstrNameRef, nil, nil, nil));
      AddToInterFace('  Co%s = Class',[BstrName]);
      AddToInterface('    Class Function Create: %s;',[BstrNameRef]);
      AddToInterFace('    Class Function CreateRemote(const MachineName: string): %s;',[BstrNameRef]);
      AddToInterFace('  end;');
      AddToInterFace('');
      AddToImplementation('Class Function Co%s.Create: %s;',[BstrName,BstrNameRef]);
      AddToImplementation('begin');
      AddToImplementation('  Result := CreateComObject(CLASS_%s) as %s;',[BstrName,BstrNameRef]);
      AddToImplementation('end;');
      AddToImplementation('');
      AddToImplementation('Class Function Co%s.CreateRemote(const MachineName: string): %s;',[BstrName,BstrNameRef]);
      AddToImplementation('begin');
      AddToImplementation('  Result := CreateRemoteComObject(MachineName,CLASS_%s) as %s;',[BstrName,BstrNameRef]);
      AddToImplementation('end;');
      AddToImplementation('');
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
  FUses.Add('OleServer');
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
  FAppendVersionNumber:=true;
end;

destructor TTypeLibImporter.Destroy;
begin
  FreeAndNil(FDependencies);
  FreeAndNil(FUnitSource);
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
  try
    DoImportTypeLib;
    If (OutputFileName<>'') then
      UnitSource.SaveToFile(OutputFileName);
  finally
    FreeAndNil(FUses);
    FreeAndNil(FInterface);
    FreeAndNil(FHeader);
    FreeAndNil(FImplementation);
  end;
end;

end.

