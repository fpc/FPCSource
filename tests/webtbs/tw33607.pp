{$mode objfpc}{$H+}
{$modeSwitch advancedRecords}

type
   TRectangle = record
		   public
		   Left, Bottom: Integer;
		   Width, Height: Cardinal;

		function ScaleAround0(const Factor: Single): TRectangle;
		end;

function TRectangle.ScaleAround0(const Factor: Single): TRectangle;
begin
   if Width <= 0 then
   begin
      Result.Width  := Width;
      Result.Left   := Left;
   end else
      halt(3);

   Result.Height := Height;
   Result.Bottom := Bottom;
end;

function Rectangle(const Left, Bottom: Integer;
		   const Width, Height: Cardinal): TRectangle;
begin
   Rectangle.Left := Left;
   Rectangle.Bottom := Bottom;
   Rectangle.Width := Width;
   Rectangle.Height := Height;
end;

procedure test(c: qword);
begin
  if c<>0 then
    halt(2);
end;

var
   R, S	:  TRectangle;
begin
   R := Rectangle(10, 20, 0, 50);
   S := R.ScaleAround0(2);
   if s.width<>0 then
     halt(1);

  test(R.ScaleAround0(2).Width);
end.

