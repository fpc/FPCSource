program SimpleBGScroll;

{$mode objfpc}
{$H+}

uses
  ctypes, gba;

{$l build\r6502_portfont.bin.o}

{$include inc\r6502_portfont.bin.inc}

var
  MAPADDRESS: pcuint16;

const
  DELAY = 2;			                  // slow things down
  TILEWIDTH = 8;			              // how much to scroll
  ROW = 10;			                    // what row to place text at

// --------------------------------------------------------------------

var 
  palette: array [0..6] of cuint16;
  
// --------------------------------------------------------------------

const 
  message = '                                ' +
            'Hello, this is an example of an oldschool simple tile scroller ' +
            'not unlike how it was done in days of yore.  The ''@'' symbol ' +
            'at the top of your screen is intentional, to dispel the illusion ' +
            'of this scroller, to demonstrate the simple concept behind it. ' +
            'Check out the source to learn how it works.  It is very simple! ' +
            'This exercise brought to you by r6502...          ' +
            'Text is about to restart... ';



procedure updatescrolltext(idx: cuint32);
var
  i: integer;
  temppointer: pcuint16;
begin
  temppointer := pcuint16(MAPADDRESS + (ROW * 32));

  // write out a whole row of text to the map
  for i := 0 to 31 do
  begin
    // check for end of message so we can wrap around properly
    if (message[idx] = #0) then 
      idx := 0;

    // write a character - we subtract 32, because the font graphics
    // start at tile 0, but our text is in ascii (starting at 32 and up)
    // in other words, tile 0 is a space in our font, but in ascii a
    // space is 32 so we must account for that difference between the two.
    temppointer^ := Ord(message[idx]) - 32;
    inc(temppointer); 
    inc(idx);
  end;
end;


var
  i, scrollx, scrolldelay, textindex: integer;
  temppointer: pcuint16;

begin	
  MAPADDRESS := MAP_BASE_ADR(31);    // our base map address
  
  palette[0] := RGB8($40,$80,$c0);
  palette[1] := RGB8($FF,$FF,$FF);
  palette[2] := RGB8($F5,$FF,$FF);
  palette[3] := RGB8($DF,$FF,$F2);
  palette[4] := RGB8($CA,$FF,$E2);
  palette[5] := RGB8($B7,$FD,$D8);
  palette[6] := RGB8($2C,$4F,$8B);

  // Set up the interrupt handlers
  irqInit();
  // Enable Vblank Interrupt to allow VblankIntrWait
  irqEnable(IRQ_VBLANK);

  // Allow Interrupts
  REG_IME^ := 1;

  // load the palette for the background, 7 colors
  temppointer := BG_COLORS;
  
  for i := 0 to 6 do
  begin
    temppointer^ := cuint32(palette[i]); // u32 cast avoids u8 memory writing
    inc(temppointer);
  end;

  // load the font into gba video mem (48 characters, 4bit tiles)

  CpuFastSet(@r6502_portfont_bin, pcuint16(VRAM), (r6502_portfont_bin_size div 4) or COPY32);

  // clear screen map with tile 0 ('space' tile) (256x256 halfwords)

  //MAP_BASE_ADR(31) := nil;
  CpuFastSet( MAP_BASE_ADR(31), MAP_BASE_ADR(31), FILL or COPY32 or ($800 div 4));

  // set screen H and V scroll positions
  BG_OFFSET[0].x := 0; 
  BG_OFFSET[0].y := 0;

  // initialize our variables
  scrollx := 0;
  textindex := 0;
  scrolldelay := 0;

  // put the '@' symbol on the top of the screen to show how
  // the screen is only scrolling 7 pixels - to reveal the
  // illusion of how the scroller works
  pcuint16((MAPADDRESS + 1))^ := $20;	// 0x20 == '@'

  // draw a row of text from beginning of message
  updatescrolltext(0);

  // set the screen base to 31 (0x600F800) and char base to 0 (0x6000000)
  BGCTRL[0] := SCREEN_BASE(31);

  // screen mode & background to display
  SetMode( MODE_0 or BG0_ON );

  while true do 
  begin
    VBlankIntrWait();

    // check if we reached our delay
    if (scrolldelay = DELAY) then
    begin
      // yes, the delay is complete, so let's reset it
      scrolldelay := 0;

      // check if we reached our scrollcount
      if (scrollx = (TILEWIDTH-1)) then
      begin
        // yes, we've scrolled enough, so let's reset the count
        scrollx := 0;

        // check if we reached the end of our scrolltext
        // and if so we need to restart our index
        if (message[textindex] = #0) then 
          textindex := 0
        else 
          inc(textindex);

        // finally, let's update the scrolltext with the current text index
        updatescrolltext(textindex);
      end else 
        inc(scrollx);
    end else 
      inc(scrolldelay);

    // update the hardware horizontal scroll register
    BG_OFFSET[0].x := scrollx;
  end;
end.

