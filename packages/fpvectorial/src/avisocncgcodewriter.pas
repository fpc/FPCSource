{
Writes AvisoCNC G-Code

License: The same modified LGPL as the Free Pascal RTL
         See the file COPYING.modifiedLGPL for more details

AUTHORS: Felipe Monteiro de Carvalho
         Pedro Sol Pegorini L de Lima
}
unit avisocncgcodewriter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  fpvectorial;

type
  { TvAvisoCNCGCodeWriter }

  TvAvisoCNCGCodeWriter = class(TvCustomVectorialWriter)
  public
    { General reading methods }
    procedure WriteToStrings(AStrings: TStrings; AData: TvVectorialDocument); override;
  end;

implementation

{ TvGCodeVectorialWriter }

procedure TvAvisoCNCGCodeWriter.WriteToStrings(AStrings: TStrings;
  AData: TvVectorialDocument);
var
  i, j: Integer;
  Str: string;
  APath: TPath;
begin
  AStrings.Clear;

  AStrings.Add('M216 // Ligar monitor de carga');
  AStrings.Add('G28 // Ir rapidamente para posição inicial');
  AStrings.Add('G00');

  // itera por todos os itens
  for i := 0 to AData.GetPathCount - 1 do
  begin
    APath := AData.GetPath(i);

    // levanta a broca
    AStrings.Add('P01 // Sobe a cabeça de gravação');
    // vai para o ponto inicial
    AStrings.Add(Format('G01 X%f Y%f',
      [APath.Points[0].X, APath.Points[0].Y]));
    AStrings.Add('P02 // Abaixa a cabeça de gravação');

    for j := 1 to APath.Len - 1 do
    begin
      case APath.Points[j].SegmentType of
      st2DLine: AStrings.Add(Format('G01 X%f Y%f',
         [APath.Points[j].X, APath.Points[j].Y]));
      st3DLine: AStrings.Add(Format('G01 X%f Y%f Z%f',
         [APath.Points[j].X, APath.Points[j].Y, APath.Points[j].Z]));
      st2DBezier: AStrings.Add(Format('B02 X%f Y%f X%f Y%f X%f Y%f',
         [APath.Points[j].X2, APath.Points[j].Y2,
          APath.Points[j].X3, APath.Points[j].Y3,
          APath.Points[j].X, APath.Points[j].Y]));
      st3DBezier: AStrings.Add(Format('B03 X%f Y%f Z%f X%f Y%f Z%f X%f Y%f Z%f',
         [APath.Points[j].X2, APath.Points[j].Y2, APath.Points[j].Z2,
          APath.Points[j].X3, APath.Points[j].Y3, APath.Points[j].Z3,
          APath.Points[j].X, APath.Points[j].Y, APath.Points[j].Z]));
      end;
    end;
  end;

  AStrings.Add('P01 // Sobe a cabeça de gravação');
  AStrings.Add('M30 // Parar o programa e retornar para posição inicial');
  AStrings.Add('M215 // Desligar monitor de carga');
end;

initialization

  RegisterVectorialWriter(TvAvisoCNCGCodeWriter, vfGCodeAvisoCNCPrototipoV5);

end.

