{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by Michael Van Canneyt

    Create a HTML page with all SVG files and a rendering though fcl-svg side by side.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
program svggallery;

{$mode objfpc}{$H+}

uses sysutils, classes, math, fpimage, fpwritepng,
     fpreadpng, fpreadjpeg, fpreadgif, fpreadbmp,
     fpsvg.types, fpsvg.dom, fpsvg.read, fpsvg.render, fpsvg.soft,
     fpsvg.freetype,
{$IFDEF DARWIN}
     fpsvg.fonts.macos;
{$ENDIF}
{$IFDEF WINDOWS}
     fpsvg.fonts.windows;
{$ENDIF}
{$IF defined(UNIX) and not defined(DARWIN)}
     fpsvg.fonts.unix;
{$ENDIF}

const
  { Nothing is drawn wider or taller than this, however large the
    document says it is. A document saying no size at all is drawn at
    the second pair. }
  MaxWide = 640;
  MaxTall = 640;
  PlainWide = 320;
  PlainTall = 240;

type
  { What became of one document. }
  TDrawing = record
    Name    : String;
    Wide    : Integer;
    Tall    : Integer;
    Elements: Integer;
    Trouble : String;
  end;

var
  lIn, lOut, lPage: String;
  lNames: TStringList;
  lDrawings: array of TDrawing;
  lRenderer: TSVGRenderer;
  lFonts: TSVGFreeTypeProvider;
  lImages: TSVGFileImageResolver;
  lDocuments: TSVGFileDocumentResolver;
  lSheets: TSVGFileStyleSheetResolver;
  lFaces: TSVGFileFontResolver;
  I, lDrawn, lFailed: Integer;

// The text with the five characters a browser reads as markup written out.
function HtmlText(const aText: String): String;

begin
  Result := StringReplace(aText, '&', '&amp;', [rfReplaceAll]);
  Result := StringReplace(Result, '<', '&lt;', [rfReplaceAll]);
  Result := StringReplace(Result, '>', '&gt;', [rfReplaceAll]);
  Result := StringReplace(Result, '"', '&quot;', [rfReplaceAll]);
  Result := StringReplace(Result, '''', '&#39;', [rfReplaceAll]);
end;


// The path with the characters that cannot stand in an attribute escaped.
function UrlText(const aPath: String): String;

var
  I: Integer;

begin
  Result := '';
  for I := 1 to Length(aPath) do
    if aPath[I] in ['A'..'Z', 'a'..'z', '0'..'9', '-', '_', '.', '~', '/'] then
      Result := Result + aPath[I]
    else
      Result := Result + '%' + HexStr(Ord(aPath[I]), 2);
end;


// Every file of the directory whose name ends in svg, in order.
procedure Gather(const aDir: String; aNames: TStringList);

var
  lSearch: TSearchRec;

begin
  if FindFirst(IncludeTrailingPathDelimiter(aDir) + '*', faAnyFile,
       lSearch) <> 0 then
    Exit;
  try
    repeat
      if ((lSearch.Attr and faDirectory) = 0)
         and SameText(ExtractFileExt(lSearch.Name), '.svg') then
        aNames.Add(lSearch.Name);
    until FindNext(lSearch) <> 0;
  finally
    FindClose(lSearch);
  end;
  aNames.Sort;
end;


// Draws one document at its natural size, capped on the sizes set above.
function DrawOne(const aName: String): TDrawing;

var
  lDoc: TSVGDocument;
  lSoft: TSVGSoftBackend;
  lWriter: TFPWriterPNG;
  lScale: Double;

begin
  FillChar(Result, SizeOf(Result), 0);
  Result.Name := aName;
  lDoc := nil;
  lSoft := nil;
  lWriter := nil;
  try
    try
      lDoc := ReadSVGFile(IncludeTrailingPathDelimiter(lIn) + aName);
      lDoc.BaseURI := IncludeTrailingPathDelimiter(lIn) + aName;
      if not lRenderer.DocumentSize(lDoc, Result.Wide, Result.Tall)
         or (Result.Wide <= 0) or (Result.Tall <= 0) then
        begin
        Result.Wide := PlainWide;
        Result.Tall := PlainTall;
        end;
      lScale := 1;
      if Result.Wide > MaxWide then
        lScale := MaxWide / Result.Wide;
      if Result.Tall * lScale > MaxTall then
        lScale := MaxTall / Result.Tall;
      Result.Wide := Max(1, Round(Result.Wide * lScale));
      Result.Tall := Max(1, Round(Result.Tall * lScale));
      lSoft := TSVGSoftBackend.Create;
      lRenderer.RenderToFit(lDoc, lSoft, Result.Wide, Result.Tall);
      Result.Elements := lRenderer.ElementCount;
      lWriter := TFPWriterPNG.Create;
      lWriter.UseAlpha := True;
      lWriter.WordSized := False;
      lSoft.Image.SaveToFile(IncludeTrailingPathDelimiter(lOut)
        + ChangeFileExt(aName, '.png'), lWriter);
    except
      on E: Exception do
        Result.Trouble := E.Message;
    end;
  finally
    lWriter.Free;
    lSoft.Free;
    lDoc.Free;
  end;
end;


// Write the actual HTML with images side by side.
procedure WritePage(const aPath, aSVGHref: String);

var
  lHtml: TStringList;
  I: Integer;
  lPng, lSvg: String;

begin
  lHtml := TStringList.Create;
  try
    lHtml.Add('<!DOCTYPE html>');
    lHtml.Add('<html lang="en"><head><meta charset="utf-8">');
    lHtml.Add('<meta name="viewport" content="width=device-width,'
      + ' initial-scale=1">');
    lHtml.Add('<title>' + HtmlText(ExtractFileName(ExcludeTrailingPathDelimiter(
      lIn))) + ' drawn two ways</title>');
    lHtml.Add('<style>');
    lHtml.Add(':root { color-scheme: light dark;');
    lHtml.Add('  --ink: #1a1a1a; --dim: #666; --paper: #fff;');
    lHtml.Add('  --line: #e3e3e3; --panel: #fafafa; --bad: #b3261e; }');
    lHtml.Add('@media (prefers-color-scheme: dark) { :root {');
    lHtml.Add('  --ink: #e8e8e8; --dim: #9a9a9a; --paper: #16181c;');
    lHtml.Add('  --line: #2c2f34; --panel: #1c1f24; --bad: #ff8a80; } }');
    lHtml.Add('body { margin: 0; padding: 24px; background: var(--paper);');
    lHtml.Add('  color: var(--ink); font: 14px/1.5 system-ui, sans-serif; }');
    lHtml.Add('h1 { font-size: 20px; margin: 0 0 4px; }');
    lHtml.Add('p.sum { margin: 0 0 18px; color: var(--dim); }');
    lHtml.Add('table { border-collapse: collapse; width: 100%; }');
    lHtml.Add('th { text-align: left; font-weight: 600; color: var(--dim);');
    lHtml.Add('  padding: 6px 10px; border-bottom: 1px solid var(--line); }');
    lHtml.Add('td { padding: 10px; border-bottom: 1px solid var(--line);');
    lHtml.Add('  vertical-align: top; }');
    lHtml.Add('.name { width: 22%; }');
    lHtml.Add('.shot { width: 39%; }');
    lHtml.Add('.tn { font-family: ui-monospace, monospace;');
    lHtml.Add('  font-weight: 600; overflow-wrap: anywhere; }');
    lHtml.Add('.dim { color: var(--dim); }');
    lHtml.Add('.bad { color: var(--bad); }');
    // The chequer behind both pictures.
    lHtml.Add('img { max-width: 100%; height: auto; display: block;');
    lHtml.Add('  background-color: #fff; background-image:');
    lHtml.Add('    linear-gradient(45deg, #eee 25%, transparent 25%),');
    lHtml.Add('    linear-gradient(-45deg, #eee 25%, transparent 25%),');
    lHtml.Add('    linear-gradient(45deg, transparent 75%, #eee 75%),');
    lHtml.Add('    linear-gradient(-45deg, transparent 75%, #eee 75%);');
    lHtml.Add('  background-size: 16px 16px;');
    lHtml.Add('  background-position: 0 0, 0 8px, 8px -8px, -8px 0; }');
    lHtml.Add('</style></head><body>');
    lHtml.Add('<h1>' + HtmlText(lIn) + '</h1>');
    lHtml.Add(Format('<p class="sum">%d drawn, %d that would not draw. '
      + 'The left of each pair is by fcl-svg, the right is the document '
      + 'as drawn by the browser.</p>', [lDrawn, lFailed]));
    lHtml.Add('<table><thead><tr><th class="name">document</th>'
      + '<th class="shot">ours</th>'
      + '<th class="shot">the browser''s</th></tr></thead><tbody>');
    for I := 0 to High(lDrawings) do
      begin
      lHtml.Add('<tr>');
      lHtml.Add('<td class="name"><div class="tn">'
        + HtmlText(lDrawings[I].Name) + '</div>');
      if lDrawings[I].Trouble <> '' then
        lHtml.Add('<div class="bad">' + HtmlText(lDrawings[I].Trouble)
          + '</div>')
      else
        lHtml.Add(Format('<div class="dim">%d by %d, %d elements</div>',
          [lDrawings[I].Wide, lDrawings[I].Tall, lDrawings[I].Elements]));
      lHtml.Add('</td>');
      lPng := UrlText(ChangeFileExt(lDrawings[I].Name, '.png'));
      lSvg := aSVGHref + UrlText(lDrawings[I].Name);
      if lDrawings[I].Trouble = '' then
        lHtml.Add('<td class="shot"><a href="' + lPng + '">'
          + '<img loading="lazy" alt="ours" src="' + lPng + '"></a></td>')
      else
        lHtml.Add('<td class="shot dim">nothing drawn</td>');
      lHtml.Add('<td class="shot"><a href="' + lSvg + '">'
        + '<img loading="lazy" alt="the document" src="' + lSvg
        + '"></a></td>');
      lHtml.Add('</tr>');
      end;
    lHtml.Add('</tbody></table></body></html>');
    lHtml.SaveToFile(aPath);
  finally
    lHtml.Free;
  end;
end;


begin
  lIn := ParamStr(1);
  if lIn = '' then
    lIn := 'examples' + PathDelim + 'SVG';
  lOut := ParamStr(2);
  if lOut = '' then
    lOut := 'build' + PathDelim + 'gallery';
  if not DirectoryExists(lIn) then
    begin
    WriteLn('svggallery [svg dir] [out dir]');
    WriteLn('  ', lIn, ' is not a directory');
    Halt(2);
    end;
  ForceDirectories(lOut);
  lFonts := TSVGFreeTypeProvider.Create;
  lImages := TSVGFileImageResolver.Create(lIn);
  lDocuments := TSVGFileDocumentResolver.Create(lIn);
  lSheets := TSVGFileStyleSheetResolver.Create(lIn);
  lFaces := TSVGFileFontResolver.Create(lIn);
  lRenderer := TSVGRenderer.Create;
  lRenderer.Fonts := lFonts;
  lRenderer.Images := lImages;
  lRenderer.Documents := lDocuments;
  lRenderer.StyleSheets := lSheets;
  lRenderer.FontFiles := lFaces;
  lNames := TStringList.Create;
  try
    Gather(lIn, lNames);
    SetLength(lDrawings, lNames.Count);
    lDrawn := 0;
    lFailed := 0;
    for I := 0 to lNames.Count - 1 do
      begin
      lDrawings[I] := DrawOne(lNames[I]);
      if lDrawings[I].Trouble = '' then
        Inc(lDrawn)
      else
        begin
        Inc(lFailed);
        WriteLn(Format('  %s: %s', [lNames[I], lDrawings[I].Trouble]));
        end;
      end;
    lPage := IncludeTrailingPathDelimiter(lOut) + 'index.html';
    WritePage(lPage, UrlText(ExtractRelativepath(
      IncludeTrailingPathDelimiter(ExpandFileName(lOut)),
      IncludeTrailingPathDelimiter(ExpandFileName(lIn)))));
    WriteLn(Format('%d drawn, %d that would not draw, laid out in %s',
      [lDrawn, lFailed, lPage]));
  finally
    lNames.Free;
    lRenderer.Free;
    lFonts.Free;
  end;
end.
