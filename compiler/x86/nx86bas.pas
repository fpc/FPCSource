{
    Copyright (c) 2024 by J. Gareth "Kit" Moreton

    This unit implements the x86-specific assembly node

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
unit nx86bas;

{$i fpcdefs.inc}

interface

  uses
    nbas, ncgbas, aasmtai;

  type
    Tx86AsmNode = class(TCGAsmNode)
{$ifdef DEBUG_NODE_XML}
      procedure XMLPrintNodeData(var T: Text); override;
    protected
      function XMLFormatOp(const Oper: POper): string; override;
      procedure XMLProcessInstruction(var T: Text; p: tai); override;
{$endif DEBUG_NODE_XML}
    end;

implementation

{$ifdef DEBUG_NODE_XML}
  uses
    cutils,
    cgutils,
    cgbase,
    cpubase,
    itcpugas,
    aasmcpu,
    verbose;
{$endif DEBUG_NODE_XML}

{$ifdef DEBUG_NODE_XML}
  function Tx86AsmNode.XMLFormatOp(const Oper: POper): string;
    begin
      case Oper^.typ of
        top_const:
          begin
            case Oper^.val of
              -15..15:
                Result := '$' + tostr(Oper^.val);
              $10..$FF:
                Result := '$0x' + hexstr(Oper^.val, 2);
              $100..$FFFF:
                Result := '$0x' + hexstr(Oper^.val, 4);
  {$ifdef CPU64}
              $10000..$FFFFFFFF:
                Result := '$0x' + hexstr(Oper^.val, 8);
              else
                Result := '$0x' + hexstr(Oper^.val, 16);
  {$else CPU64}
              else
                Result := '$0x' + hexstr(Oper^.val, 8);
  {$endif CPU64}
            end;
          end;

        top_ref:
          with Oper^.ref^ do
            begin
              if segment <> NR_NO then
                Result := gas_regname(segment) + ':'
              else
                Result := '';

              if Assigned(symbol) then
                begin
                  Result := Result + symbol.Name;
                  if offset > 0 then
                    Result := Result + '+';
                end;

              if offset <> 0 then
                Result := Result + tostr(offset)
              else
                Result := Result;

              if (base <> NR_NO) or (index <> NR_NO) then
                begin
                  Result := Result + '(';

                  if base <> NR_NO then
                    begin
                      Result := Result + gas_regname(base);
                      if index <> NR_NO then
                        Result := Result + ',';
                    end;

                  if index <> NR_NO then
                    Result := Result + gas_regname(index);

                  if scalefactor <> 0 then
                    Result := Result + ',' + tostr(scalefactor) + ')'
                  else
                    Result := Result + ')';
                end;
            end;
        else
          Result := inherited XMLFormatOp(Oper);
      end;
    end;


  procedure Tx86AsmNode.XMLProcessInstruction(var T: Text; p: tai);
    var
      ThisOp, ThisOper: string;
      X: Integer;
    begin
      if p.typ = ait_instruction then
        begin
          ThisOp := gas_op2str[taicpu(p).opcode] + cond2str[taicpu(p).condition];
          if gas_needsuffix[taicpu(p).opcode] <> AttSufNONE then
            ThisOp := ThisOp + gas_opsize2str[taicpu(p).opsize];

          { Pad the opcode with spaces so the succeeding operands are aligned }
          XMLPadString(ThisOp, 7);

          Write(T, PrintNodeIndention, '  ', ThisOp); { Extra indentation to account for label formatting }
          for X := 0 to taicpu(p).ops - 1 do
            begin
              Write(T, ' ');

              ThisOper := XMLFormatOp(taicpu(p).oper[X]);
              if X < taicpu(p).ops - 1 then
                begin
                  ThisOper := ThisOper + ',';

                  XMLPadString(ThisOper, 7);
                end;

              Write(T, ThisOper);
            end;
          WriteLn(T);
        end
      else
        inherited XMLProcessInstruction(T, p);
    end;


  procedure Tx86AsmNode.XMLPrintNodeData(var T: Text);
    var
      hp: tai;
    begin
      if not Assigned(p_asm) then
        Exit;

      hp := tai(p_asm.First);
      while Assigned(hp) do
        begin
          XMLProcessInstruction(T, hp);
          hp := tai(hp.Next);
        end;
    end;
{$endif DEBUG_NODE_XML}

initialization
  casmnode := Tx86AsmNode;

end.

