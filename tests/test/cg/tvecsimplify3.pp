program tvecsimplify3;
{$H+}

const
  SSonnet: WideString = 'When forty winters shall besiege thy brow,'#10+
	'And dig deep trenches in thy beauty''s field,'#10+
	'Thy youth''s proud livery, so gazed on now,'#10+
	'Will be a tattered weed of small worth held.'#10+
	'Then being asked where all thy beauty lies,'#10+
	'Where all the treasure of thy lusty days,'#10+
	'To say within thine own deep-sunken eyes'#10+
	'Were an all-eating shame and thriftless praise.'#10+
	'How much more praise deserved thy beauty''s use'#10+
	'If thou couldst answer ''This fair child of mine'#10+
	'Shall sum my count, and make my old excuse'','#10+
	'Proving his beauty by succession thine.'#10+
	'This were to be new made when thou art old,'#10+
	'And see thy blood warm when thou feel''st it cold.';

function GetChar: WideChar;
begin
  GetChar := SSonnet[300];
end;

begin
  if GetChar() <> 'y' then
    Halt(1);
	
  WriteLn('ok');
end.