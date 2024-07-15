{
    Copyright (c) 2024 by J. Gareth "Kit" Moreton

    This unit implements the ARM and AArch64-specific assembly node

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
unit narmbas;

{$i fpcdefs.inc}

interface

  uses
    nbas, ncgbas, aasmtai;

  type
    TArmGenAsmNode = class(TCGAsmNode)
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
{$ifdef arm}
    agarmgas, { Needed for gas_shiftmode2str }
{$endif arm}
{$ifdef aarch64}
    agcpugas, { Needed for gas_shiftmode2str }
{$endif aarch64}
    verbose;
{$endif DEBUG_NODE_XML}

{$ifdef DEBUG_NODE_XML}
  function TArmGenAsmNode.XMLFormatOp(const Oper: POper): string;
  {$ifdef arm}
    var
      NotFirst: Boolean;
      ThisSupReg: TSuperRegister;
  {$endif arm}
    begin
      case Oper^.typ of
        top_const:
          begin
            case Oper^.val of
              -15..15:
                Result := '#' + tostr(Oper^.val);
              $10..$FF:
                Result := '#0x' + hexstr(Oper^.val, 2);
              $100..$FFFF:
                Result := '#0x' + hexstr(Oper^.val, 4);
  {$ifdef CPU32}
              else
                Result := '#0x' + hexstr(Oper^.val, 8);
  {$else CPU32}
              $10000..$FFFFFFFF:
                Result := '#0x' + hexstr(Oper^.val, 8);
              else
                Result := '#0x' + hexstr(Oper^.val, 16);
  {$endif CPU32}
            end;
          end;

        top_ref:
          with Oper^.ref^ do
            begin
              if Assigned(symbol) then
                begin
                  Result := symbol.Name;
                  if (offset <> 0) then
                    begin
                      if (offset < 0) then
                        Result := Result + ' - ' + tostr(-offset)
                      else
                        Result := Result + ' + ' + tostr(offset);
                    end;
                end
              else
                begin
                  if (base <> NR_NO) then
                    begin
                      Result := '[' + gas_regname(base);
                      if addressmode = AM_POSTINDEXED then
                        Result := Result + '], '
                      else if (offset <> 0) or (shiftmode <> SM_None) then
                        Result := Result + ', ';
                    end
                  else { Usually a special kind of reference used by ldm/stm instructions }
                    Result := '';

                  if index <> NR_NO then
                    Result := Result + gas_regname(index)
                  else if (offset <> 0) or (shiftmode <> SM_None) or (addressmode = AM_POSTINDEXED) then
                    Result := Result + '#' + tostr(offset);
{$ifdef arm}
                  if shiftmode = SM_RRX then
                    Result := Result + ', rrx' { Implicit value of 1 }
                  else
{$endif arm}
                  if shiftmode <> SM_None then
                    Result := Result + ', ' + gas_shiftmode2str[shiftmode] + ' #' + tostr(shiftimm);

                  if addressmode <> AM_POSTINDEXED then
                    begin
                      if (base <> NR_NO) then
                        Result := Result + ']';

                      if addressmode = AM_PREINDEXED then
                        Result := Result + '!';
                    end;
                end;
            end;
{$ifdef arm}
        top_regset:
          begin
            Result := '{';
            NotFirst := False;
            for ThisSupReg in Oper^.regset^ do
              begin
                if NotFirst then
                  Result := Result + ', ';
                Result := Result + gas_regname(newreg(Oper^.regtyp, ThisSupReg, Oper^.subreg));

                NotFirst := True;
              end;
            Result := Result + '}';
          end;

        top_specialreg:
          with Oper^ do
            begin
              Result := gas_regname(specialreg) + '_';
              if (srC in specialflags) then
                Result := Result + 'c';
              if (srX in specialflags) then
                Result := Result + 'x';
              if (srF in specialflags) then
                Result := Result + 'f';
              if (srS in specialflags) then
                Result := Result + 's';
            end;
{$endif arm}
{$ifdef aarch64}
        top_indexedreg:
          with Oper^ do
            Result := gas_regname(indexedreg)+'['+tostr(regindex)+']';
{$endif aarch64}
        top_conditioncode:
          Result := cond2str[Oper^.cc];

        top_realconst:
          Result := '#' + realtostr(Oper^.val_real);

        top_shifterop:
          with Oper^.shifterop^ do
            begin
{$ifdef arm}
              if shiftmode = SM_RRX then
                begin
                  Result := 'rrx'; { Implicit value of 1 }
                  Exit;
                end;
              Result := gas_shiftmode2str[shiftmode] + ' ';
              if rs <> NR_NO then
                Result := Result + gas_regname(rs)
              else
                Result := Result + '#' + tostr(shiftimm);
{$endif arm}
{$ifdef aarch64}
              Result := gas_shiftmode2str[shiftmode] + ' #' + tostr(shiftimm);
{$endif aarch64}
            end;
        else
          Result := inherited XMLFormatOp(Oper);
      end;
    end;


  procedure TArmGenAsmNode.XMLProcessInstruction(var T: Text; p: tai);
    var
      ThisOp, ThisOper: string;
      X: Integer;
    begin
      if p.typ = ait_instruction then
        begin
          ThisOp := gas_op2str[taicpu(p).opcode] + cond2str[taicpu(p).condition] + oppostfix2str[taicpu(p).oppostfix];

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

                  XMLPadString(ThisOper, 4);
                end;

              Write(T, ThisOper);
            end;
          WriteLn(T);
        end
      else
        inherited XMLProcessInstruction(T, p);
    end;


  procedure TArmGenAsmNode.XMLPrintNodeData(var T: Text);
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
  casmnode := TArmGenAsmNode;

end.

