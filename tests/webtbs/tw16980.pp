{ %norun }

{$mode delphi}
{$packset 4}
type
  TColorComponent = (ccRed, ccGreen, ccBlue, ccAlpha);
  TColorMask = set of TColorComponent;

  TGLStateCache = class
  private
    FColorWriteMask: array[0..15] of TColorMask;
    procedure SetColorWriteMask(Index: Integer; const Value: TColorMask);
  end;
  TGLuint = cardinal;
  tglboolean = boolean;

var
  glColorMaski: procedure(index: TGLuint; r: TGLboolean; g: TGLboolean;
                            b: TGLboolean; a: TGLboolean);{$IFDEF MSWINDOWS} stdcall; {$ENDIF} {$IFDEF UNIX} cdecl; {$ENDIF}

procedure TGLStateCache.SetColorWriteMask(Index: Integer;
  const Value: TColorMask);
begin
//  if FColorWriteMask[Index]<>Value then
  begin
    FColorWriteMask[Index] := Value;
    glColorMaski(Index, ccRed in Value, ccGreen in Value, ccBlue in Value,
                 ccAlpha in Value);
  end;
end;


begin
end.
