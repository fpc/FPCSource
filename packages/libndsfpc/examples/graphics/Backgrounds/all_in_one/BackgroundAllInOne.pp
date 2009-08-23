program AllInOne;
{$L build/RotBackgrounds.o}
{$L build/TextBackgrounds.o}
{$L build/Multilayer.o}

{$mode objfpc}

uses
  ctypes, nds9, basic, advanced, handmade, scrolling;

type
  Demo = record
    go: fp;
    name: pchar;
    description: pchar;
  end;
  PDemo = ^Demo;

  Category = record
    name: pchar;
    demos: PDemo;
    count: integer;
  end;
  PCategory = ^Category;

var
  basicExamples: array [0..12] of Demo = (
    (go: @HandMadeTiles;  name: 'Handmade Text 256x256';        description: 'Displays a handmade 256 x 256 Text background';),
    (go: @Text256x256;    name: 'Text 256x256';                 description: 'Displays a 256 x 256 Text background';),
    (go: @Text256x512;    name: 'Text 256x512';                 description: 'Displays a 256 x 512 Text background';),
    (go: @Text512x256;    name: 'Text 512x256';                 description: 'Displays a 512 x 256 Text background';),
    (go: @Text512x512;    name: 'Text 512x512';                 description: 'Displays a 512 x 512 Text background';),
    (go: @ExRot128x128;   name: 'Extended Rotation 128x128';    description: 'Displays a 128 x 128 Extended Rotation background';),
    (go: @ExRot256x256;   name: 'Extended Rotation 256x256';    description: 'Displays a 256 x 256 Extended Rotation background';),
    (go: @ExRot512x512;   name: 'Extended Rotation 512x512';    description: 'Displays a 512 x 512 Extended Rotation background';),
    (go: @ExRot1024x1024; name: 'Extended Rotation 1024x1024';  description: 'Displays a 1024 x 1024 Extended Rotation background';),
    (go: @Rot128x128;     name: 'Rotation 128x128';             description: 'Displays a 256 x 256 Rotation background';),
    (go: @Rot256x256;     name: 'Rotation 256x256';             description: 'Displays a 256 x 256 Rotation background';),
    (go: @Rot512x512;     name: 'Rotation 512x512';             description: 'Displays a 512 x 512 Rotation background';),
    (go: @Rot1024x1024;   name: 'Rotation 1024x1024';           description: 'Displays a 1024 x 1024 Rotation background';)
  );

  bitmapExamples: array [0..9] of Demo = (
    (go: @Bmp8_128x128;   name: '256 color 128x128';    description: 'Displays a 128 x 128 Bitmap background';),
    (go: @Bmp8_256x256;   name: '256 color 256x256';    description: 'Displays a 256 x 256 Bitmap background';),
    (go: @Bmp8_512x256;   name: '256 color  512x256';   description: 'Displays a 512 x 256 Bitmap background';),
    (go: @Bmp8_512x512;   name: '256 color  512x512';   description: 'Displays a 512 x 512 Bitmap background';),
    (go: @Bmp8_512x1024;  name: '256 color  512x1024';  description: 'Displays a 512 x 1024 Bitmap background';),
    (go: @Bmp8_1024x512;  name: '256 color  1024x512';  description: 'Displays a 1024 x 512 Bitmap background';),

    (go: @Bmp16_128x128; name: '16-bit color 128x128';  description: 'Displays a 128 x 128 Bitmap background';),
    (go: @Bmp16_256x256; name: '16-bit color  256x256'; description: 'Displays a 256 x 256 Bitmap background';),
    (go: @Bmp16_512x256; name: '16-bit color  512x256'; description: 'Displays a 512 x 256 Bitmap background';),
    (go: @Bmp16_512x512; name: '16-bit color  512x512'; description: 'Displays a 512 x 512 Bitmap background';)
  );

  scrollingExamples: array [0..6] of Demo = (
    (go: @scrollText;                 name: 'Text Backgrounds';             description: 'Hardware Scrolling of a Text background';),
    (go: @scrollRotation;             name: 'Rot Backgrounds';              description: 'Hardware Scrolling of a Rotation background';),
    (go: @scrollVertical;             name: 'Vertical Scrolling';           description: 'Scrolling a large map vertically';),
    (go: @scrollHorizontalText;       name: 'Horizontal Scrolling (Text)';  description: 'Scrolling a large map horzontally on a text background';),
    (go: @scrollHorizontalExRotation; name: 'Horizontal Scrolling (ExRot)'; description: 'Scrolling a large map horzontally on an extended rotation background';),
    (go: @scroll4wayText;             name: '4 Way Scrolling (Text)';       description: 'Scrolling a large map 4 ways on a text background';),
    (go: @scroll4wayExRotation;       name: '4 Way Scrolling (Rotation)';   description: 'Scrolling a large map 4 ways on a ex rotation background';)
  );

  advancedExamples: array [0..4] of Demo = (
    (go: @advMosaic;          name: 'Mosaic';               description: 'A demo of the Mosaic scaling';),
    (go: @advRotating;        name: 'Rotation';             description: '';),
    (go: @advScaling;         name: 'Scaling';              description: '';),
    (go: @advExtendedPalette; name: 'Extended Palette';     description: '';),
    (go: @advMultipleLayers;  name: 'Multiple Text Layers'; description: '';)
  );

  categories: array [0..3] of Category = (
    (name: 'Basic';     demos: @basicExamples;      count: sizeof(basicExamples) div sizeof(Demo);),
    (name: 'Bitmap';    demos: @bitmapExamples;     count: sizeof(bitmapExamples) div sizeof(Demo);),
    (name: 'Scrolling'; demos: @scrollingExamples;  count: sizeof(scrollingExamples) div sizeof(Demo);),
    (name: 'Advanced';  demos: @advancedExamples;   count: sizeof(advancedExamples) div sizeof(Demo);)
  );


var
  keys: integer;
  selectedCategory: integer = 0;
  selectedDemo: integer = 0;
  selected: boolean = false;
	catCount: integer;
  demoCount: integer = 0;
  ci: integer;
  di: integer;

begin

	while true do
	begin
		catCount := sizeof(categories) div sizeof(Category);
		demoCount := 0;

		videoSetModeSub(MODE_0_2D);
		consoleDemoInit();

		while not selected do
		begin
			scanKeys();

			keys := keysDown();

			if (keys and KEY_UP) <> 0 then dec(selectedCategory);
			if (keys and KEY_DOWN) <> 0 then inc(selectedCategory);
			if (keys and KEY_A) <> 0 then selected := true;

			if (selectedCategory < 0) then selectedCategory := catCount - 1;
			if (selectedCategory >= catCount) then selectedCategory := 0;

			swiWaitForVBlank();
			consoleClear();
			for ci := 0 to catCount - 1 do
			begin
        if ci = selectedCategory then
				  iprintf('%c%d: %s'#10, '*', ci + 1, categories[ci].name)
        else
				  iprintf('%c%d: %s'#10, ' ', ci + 1, categories[ci].name);
			end;
		end;

		selected := false;

		demoCount := categories[selectedCategory].count;

		while not (selected) do
		begin
			scanKeys();

			keys := keysDown();

			if (keys and KEY_UP) <> 0 then dec(selectedDemo);
			if (keys and KEY_DOWN) <> 0 then inc(selectedDemo);
			if (keys and KEY_A) <> 0 then selected := true;
			if (keys and KEY_B) <> 0 then break;

			if (selectedDemo < 0) then selectedDemo := demoCount - 1;
			if (selectedDemo >= demoCount) then selectedDemo := 0;

			swiWaitForVBlank();
			consoleClear();

			for di := 0 to demoCount - 1 do
			begin
        if di = selectedDemo then
				  iprintf('%c%d: %s'#10, '*', di + 1, categories[selectedCategory].demos[di].name)
        else
          iprintf('%c%d: %s'#10, ' ', di + 1, categories[selectedCategory].demos[di].name);
			end;
		end;

		if (selected) then
		begin
			consoleClear();
			iprintf('Use arrow keys to scroll'#10'Press ''B'' to exit');
			categories[selectedCategory].demos[selectedDemo].go();
		end;
	end;

end.

