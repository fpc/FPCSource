unit libmcgui;
interface
const
	NEGICON_A				= $20;
	NEGICON_B				= $10;

	MOUSE_LBUTTON			= $08;
	MOUSE_RBUTTON			= $04;

	MCGUI_INTERNAL_FONT		= 0;
	MCGUI_EXTERNAL_FONT		= 1;

type

// Texture Information Structure
	sMcGuiTexture = packed record
						addr : pointer;
	end;

// Memory Card Information Structure
	sMcGuiCards = packed record
							_file : array [0..20] of char;
							title : array [0..64] of char;
							frame : byte;
							block : byte;
							dataBytes : longint;
							iconAddr : pointer;
							dataAddr : pointer;
	end;

// BG Information Structure
	sMcGuiBg = packed record
						mode : smallint;
						scrollDirect : shortint;	// 0:Up 1:Up&Left 2:Left 3:Down&left 4:Down ...
						scrollSpeed : shortint;		// 0:no scroll 1:1/60 2:1/30 3:1/20
						timadr : pointer;
	end;

// Cursor Information Structure
	sMcGuiCursor = packed record
						mode : byte;
						r, g, b : byte;
	end;

// BGM,Sound Effects Information Structure
	Tbgm = packed record
					isbgm : longint;
					seq : pdword;
					vh : pdword;
					vb : pdword;
					SVOL : longint;
					isReverb : longint;
					reverbType : longint;
					reverbDepth : longint;
	end;

	Tse = packed record
					isse : longint;
					vh : pdword;
					vb : pdword;
					vol : longint;
					prog : longint;
					TONE_OK : longint;
					TONE_CANCEL : longint;
					TONE_CURSOR : longint;
					TONE_ERROR : longint;
	end;


	sMcGuiSnd = packed record
					MVOL : longint;
					bgm : Tbgm;
					se : Tse;
	end;


// Controller Related Information Structure

	TType = packed record
					flag : longint;
					BUTTON_OK : dword;
					BUTTON_CANCEL : dword;
	end;

	sMcGuiController = packed record
								buf : array [0..1] of byte;
								type1 : Ttype;
								type2 : Ttype;
								type3 : Ttype;
								type4 : Ttype;
	end;


// Memory Card Screen Configuration Structure
	McGuiEnv = packed record
						cards : sMcGuiCards;			// Memory Card Information
						bg : sMcGuiBg;					// BG Information
						controller : sMcGuiController;	// Controller Related Information
						sound : sMcGuiSnd;				// BGM Sound Effects Information
						texture : sMcGuiTexture;		// Texture Information
						cursor : sMcGuiCursor;			// Cursor Information
	end;
	PMcGuiEnv = ^McGuiEnv;


function McGuiSave(env:PMcGuiEnv): longint; external;
function McGuiLoad(env:PMcGuiEnv): longint; external;

function McGuiSetExternalFont(env: PMcGuiEnv; mode: longint): longint; external;

implementation

begin
end.