{
    Copyright (c) 1998-2002 by Peter Vreman

    This unit implements support import,export,link routines
    for the (i8086) Win16 target

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************
}
unit t_win16;

{$i fpcdefs.inc}

interface


implementation

    uses
       SysUtils,
       cutils,cfileutl,cclasses,
       globtype,globals,systems,verbose,cscript,
       import,export,fmodule,i_win16,
       link,aasmbase,cpuinfo,
       omfbase,ogbase,ogomf,owbase,owomflib,
       symconst,symdef,symsym;

    type

      { TImportLibWin16 }

      TImportLibWin16=class(timportlib)
      public
        procedure generatelib;override;
      end;

      { TExportLibWin16 }

      TExportLibWin16=class(texportlib)
      private
        EList: TFPList;
      public
        destructor Destroy;override;
        procedure preparelib(const s : string);override;
        procedure exportprocedure(hp : texported_item);override;
        procedure generatelib;override;
      end;

      { the (Open) Watcom linker }
      TExternalLinkerWin16WLink=class(texternallinker)
      private
         Function  WriteResponseFile(isdll:boolean) : Boolean;
      public
         constructor Create;override;
         procedure SetDefaultInfo;override;
         function  MakeExecutable:boolean;override;
      end;

{****************************************************************************
                               TImportLibWin16
****************************************************************************}


procedure TImportLibWin16.generatelib;
var
  ObjWriter: TOmfLibObjectWriter;
  ObjOutput: TOmfObjOutput;
  i,j: Integer;
  ImportLibrary: TImportLibrary;
  ImportSymbol: TImportSymbol;
  AsmPrefix: String;

  procedure AddImport(const dllname,afuncname,mangledname:string;ordnr:longint;isvar:boolean);
    begin
      ObjOutput.startObjectfile(mangledname);
      ObjOutput.WriteDllImport(dllname,afuncname,mangledname,ordnr,isvar);
      ObjOutput.Writer.closefile;
    end;

begin
  AsmPrefix:='imp'+Lower(current_module.modulename^);
  current_module.linkotherstaticlibs.add(current_module.importlibfilename,link_always);
  ObjWriter:=TOmfLibObjectWriter.CreateAr(current_module.importlibfilename,32);
  ObjOutput:=TOmfObjOutput.Create(ObjWriter);
  for i:=0 to current_module.ImportLibraryList.Count-1 do
    begin
      ImportLibrary:=TImportLibrary(current_module.ImportLibraryList[i]);
      for j:=0 to ImportLibrary.ImportSymbolList.Count-1 do
        begin
          ImportSymbol:=TImportSymbol(ImportLibrary.ImportSymbolList[j]);
          AddImport(ImportLibrary.Name,ImportSymbol.Name,ImportSymbol.MangledName,ImportSymbol.OrdNr,ImportSymbol.IsVar);
        end;
    end;
  ObjOutput.Free;
  ObjWriter.Free;
end;


{****************************************************************************
                               TExportLibWin16
****************************************************************************}

destructor TExportLibWin16.Destroy;
begin
  EList.Free;
  inherited Destroy;
end;

procedure TExportLibWin16.preparelib(const s: string);
begin
  if EList=nil then
    EList:=TFPList.Create;
end;

procedure TExportLibWin16.exportprocedure(hp: texported_item);
begin
  if (eo_index in hp.options) and ((hp.index<=0) or (hp.index>$ffff)) then
    begin
     message1(parser_e_export_invalid_index,tostr(hp.index));
     exit;
    end;
  EList.Add(hp);
end;

procedure TExportLibWin16.generatelib;
var
  ObjWriter: TObjectWriter;
  ObjOutput: TOmfObjOutput;
  RawRecord: TOmfRawRecord;
  Header: TOmfRecord_THEADR;
  i: Integer;
  hp: texported_item;
  ModEnd: TOmfRecord_MODEND;
  DllExport_COMENT: TOmfRecord_COMENT;
  expflag: Byte;
  internal_name: TSymStr;
begin
  if EList.Count=0 then
    exit;

  current_module.linkotherofiles.add(current_module.exportfilename,link_always);
  ObjWriter:=TObjectWriter.Create;
  ObjOutput:=TOmfObjOutput.Create(ObjWriter);
  ObjWriter.createfile(current_module.exportfilename);

  { write header record }
  RawRecord:=TOmfRawRecord.Create;
  Header:=TOmfRecord_THEADR.Create;
  Header.ModuleName:=current_module.exportfilename;
  Header.EncodeTo(RawRecord);
  RawRecord.WriteTo(ObjWriter);
  Header.Free;

  for i:=0 to EList.Count-1 do
    begin
      hp:=texported_item(EList[i]);

      { write EXPDEF record }
      DllExport_COMENT:=TOmfRecord_COMENT.Create;
      DllExport_COMENT.CommentClass:=CC_OmfExtension;
      expflag:=0;
      if eo_index in hp.options then
        expflag:=expflag or $80;
      if eo_resident in hp.options then
        expflag:=expflag or $40;
      if assigned(hp.sym) then
        case hp.sym.typ of
          staticvarsym:
            internal_name:=tstaticvarsym(hp.sym).mangledname;
          procsym:
            internal_name:=tprocdef(tprocsym(hp.sym).ProcdefList[0]).mangledname;
          else
            internalerror(2015092701);
        end
      else
        internal_name:=hp.name^;
      DllExport_COMENT.CommentString:=#2+Chr(expflag)+Chr(Length(hp.name^))+hp.name^+Chr(Length(internal_name))+internal_name;
      if eo_index in hp.options then
        DllExport_COMENT.CommentString:=DllExport_COMENT.CommentString+Chr(Byte(hp.index))+Chr(Byte(hp.index shr 8));
      DllExport_COMENT.EncodeTo(RawRecord);
      RawRecord.WriteTo(ObjWriter);
      DllExport_COMENT.Free;
    end;

  { write MODEND record }
  ModEnd:=TOmfRecord_MODEND.Create;
  ModEnd.EncodeTo(RawRecord);
  RawRecord.WriteTo(ObjWriter);
  ModEnd.Free;

  ObjWriter.closefile;
  ObjOutput.Free;
  ObjWriter.Free;
  RawRecord.Free;
end;


{****************************************************************************
                               TExternalLinkerWin16WLink
****************************************************************************}

function TExternalLinkerWin16WLink.WriteResponseFile(isdll: boolean): Boolean;
Var
  linkres  : TLinkRes;
  s        : string;
  i: Integer;
begin
  WriteResponseFile:=False;

  { Open link.res file }
  LinkRes:=TLinkRes.Create(outputexedir+Info.ResName,true);

  { Add all options to link.res instead of passing them via command line:
    DOS command line is limited to 126 characters! }

  LinkRes.Add('option quiet');

  if target_dbg.id in [dbg_dwarf2,dbg_dwarf3,dbg_dwarf4] then
    LinkRes.Add('debug dwarf')
  else if target_dbg.id=dbg_codeview then
    LinkRes.Add('debug codeview');
  if cs_link_separate_dbg_file in current_settings.globalswitches then
    LinkRes.Add('option symfile');

  { add objectfiles, start with prt0 always }
  case current_settings.x86memorymodel of
    mm_tiny:    LinkRes.Add('file ' + maybequoted(FindObjectFile('prt0t','',false)));
    mm_small:   LinkRes.Add('file ' + maybequoted(FindObjectFile('prt0s','',false)));
    mm_medium:  LinkRes.Add('file ' + maybequoted(FindObjectFile('prt0m','',false)));
    mm_compact: LinkRes.Add('file ' + maybequoted(FindObjectFile('prt0c','',false)));
    mm_large:   LinkRes.Add('file ' + maybequoted(FindObjectFile('prt0l','',false)));
    mm_huge:    LinkRes.Add('file ' + maybequoted(FindObjectFile('prt0h','',false)));
  end;
  while not ObjectFiles.Empty do
  begin
    s:=ObjectFiles.GetFirst;
    if s<>'' then
      LinkRes.Add('file ' + maybequoted(s));
  end;
  while not StaticLibFiles.Empty do
  begin
    s:=StaticLibFiles.GetFirst;
    if s<>'' then
      LinkRes.Add('library '+MaybeQuoted(s));
  end;
  LinkRes.Add('format windows');
  LinkRes.Add('option heapsize='+tostr(heapsize));
  if (cs_link_map in current_settings.globalswitches) then
    LinkRes.Add('option map='+maybequoted(ChangeFileExt(current_module.exefilename,'.map')));
  LinkRes.Add('name ' + maybequoted(current_module.exefilename));
  LinkRes.Add('option dosseg');

  { Write and Close response }
  linkres.writetodisk;
  LinkRes.Free;

  WriteResponseFile:=True;
end;

constructor TExternalLinkerWin16WLink.Create;
begin
  Inherited Create;
  { allow duplicated libs (PM) }
  SharedLibFiles.doubles:=true;
  StaticLibFiles.doubles:=true;
end;

procedure TExternalLinkerWin16WLink.SetDefaultInfo;
begin
  with Info do
   begin
     ExeCmd[1]:='wlink $OPT $RES';
   end;
end;

function TExternalLinkerWin16WLink.MakeExecutable: boolean;
var
  binstr,
  cmdstr  : TCmdStr;
  success : boolean;
begin
  if not(cs_link_nolink in current_settings.globalswitches) then
    Message1(exec_i_linking,current_module.exefilename);

  { Write used files and libraries and our own tlink script }
  WriteResponsefile(false);

  { Call linker }
  SplitBinCmd(Info.ExeCmd[1],binstr,cmdstr);
  Replace(cmdstr,'$RES','@'+maybequoted(outputexedir+Info.ResName));
  Replace(cmdstr,'$OPT',Info.ExtraOptions);
  success:=DoExec(FindUtil(utilsprefix+BinStr),cmdstr,true,false);

  { Remove ReponseFile }
  if (success) and not(cs_link_nolink in current_settings.globalswitches) then
    DeleteFile(outputexedir+Info.ResName);

  MakeExecutable:=success;   { otherwise a recursive call to link method }
end;


{*****************************************************************************
                                     Initialize
*****************************************************************************}

initialization
  RegisterLinker(ld_win16,TExternalLinkerWin16WLink);
  RegisterImport(system_i8086_win16,TImportLibWin16);
  RegisterExport(system_i8086_win16,TExportLibWin16);
  RegisterTarget(system_i8086_win16_info);
end.
