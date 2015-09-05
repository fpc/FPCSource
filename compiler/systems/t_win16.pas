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
       globtype,globals,systems,verbose,script,
       import,fmodule,i_win16,
       link,aasmbase,cpuinfo,
       omfbase,ogbase,ogomf,owomflib;

    type

      { TImportLibWin16 }

      TImportLibWin16=class(timportlib)
      public
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
    LinkRes.Add('debug dwarf');

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
  if (cs_link_map in current_settings.globalswitches) then
    LinkRes.Add('option map='+maybequoted(ChangeFileExt(current_module.exefilename,'.map')));
  LinkRes.Add('name ' + maybequoted(current_module.exefilename));

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
  RegisterTarget(system_i8086_win16_info);
end.
