{
  Copyright (c) 1998 Erland Van Olmen (erlandvo@hotmail.com)
  Protected Mode Ready

  The Delay procedure replacement was provided by Frank Heckenbach:

>>  http://www.brain.uni-freiburg.de/~klaus/pascal/runerr200/download.html <<

  Here you have a little unit which does the same thing as most Crt & some
  system routines, plus some other things as drawing boxes. The code is far
  from optimal, but should be sufficiently fast for normal "stuff".

  This file was added to the MP source code so it would no longer suffer
  from the "runtime error 200" (division by zero / divide overflow) bug
  that occurs on machines faster than about 200 Mhz
  I didn't want to patch my crt unit but provide a replacement instead, which
  makes it a lot easier to recompile the program...


þþBugfixes/changes/additions since version 1.2 (first public release):

      - Made box routines faster and foolproof


BUGS:
-----

   - Not tested with monochrome adaptors, but should work!
   - VESA support may be a bit buggy... But VESA is not supported anymore
     so who cares hm?


FEATURES:
---------

   - Supports ALL possible text modes!
   - VESA compatible! (VESA *text* modes only of course!)
   - Compatible with both PASCAL & C-like strings!


******************************************************************************

Here are all the Crt-routines:

 - Procedures:

    ClrScr
    KeyPressed
    ReadKey
    TextBackground
    TextColor
    WhereY
    WhereX
    GotoXY
    AssignCrt*
    Delay
    DelLine*
    Sound*
    NoSound*
    ClrEol*
    InsLine*
    TextMode
    Window

    HighVideo
    LowVideo
    NormVideo    ; always sets attribute to 7 (LightGray on Black)

 - Variables:

    CheckBreak:  Boolean; (*)
    CheckEOF:    Boolean; (*)
    DirectVideo: Boolean; (*)
    CheckSnow:   Boolean; (**)
    LastMode:    Word;
    TextAttr:    Byte;
    WindMin:     Word;
    WindMax:     Word;

(*)  =  not implemented
(**) =  not implemented because useless.

   Coordinates are either 1,1 or 0,0 based, see the Crt unit for reference.
   (WhereX, WhereY, GotoXY, Window are all 1,1 based)

}

Unit NewCrt;

{****************************************************************************}

interface

Const
    EmptyNULLStr: Word = 0;
    MaxVideoModes = 1024; {1024 possible video modes should be enuf for inst.}
Type
    Boxtype = Array[1..8] Of Byte;

    ModeArrayType       = Array[0..MaxVideoModes-1] Of Word;
    ModeArrayTypePTR    = ^ModeArrayType;
    VgaInfoBlockTypePTR = ^VgaInfoBlockType;

    VgaInfoBlockType = Record
      VESASignature: Array[0..3] of Char; {; ==> 'VESA': 4 signature bytes    }
      VESAVersion  : Word;             { VESA version number                  }
      OEMStringPtr : ^string;          { Pointer to OEM string                }
      Capabilities : LongInt;          { capabilities of the video environment}
      VideoModePtr : ModeArrayTypePTR; { pointer to supported Super VGA modes }
      TotalMemory  : Word;             { Number of 64kb memory blocks on board}
      Reserved     : Array[0..236-1] of Byte; { Remainder of VgaInfoBlock     }
    End;

    ModeInfoBlockTypePTR = ^ModeInfoBlockType;
    ModeInfoBlockType = Record
{ mandatory information }
      ModeAttributes     : Word; { mode attributes                           }
      WinAAttributes     : Byte; { window A attributes                       }
      WinBAttributes     : Byte; { window B attributes                       }
      WinGranularity     : Word; { window granularity                        }
      WinSize            : Word; { window size                               }
      WinASegment        : Word; { window A start segment                    }
      WinBSegment        : Word; { window B start segment                    }
      WinFuncPtr         : Pointer;  { pointer to windor function            }
      BytesPerScanLine   : Word; { bytes per scan line                       }

{ formerly optional information: (mandatory from VESA v1.2+ on) }
      XResolution        : Word; { horizontal resolution                     }
      YResolution        : Word; { vertical resolution                       }
      XCharSize          : Byte; { character cell width                      }
      YCharSize          : Byte; { character cell height                     }
      NumberOfPlanes     : Byte; { number of memory planes                   }
      BitsPerPixel       : Byte; { bits per pixel                            }
      NumberOfBanks      : Byte; { number of banks                           }
      MemoryModel        : Byte; { memory model type                         }
      BankSize           : Byte; { bank size in kb                           }
      NumberOfImagePages : Byte; { number of images                          }
      Reserved           : Byte; { always 1 reserved for page function       }

{the "new Direct Color fields" below are valid only if the "MemoryModel" field
 is set to a 6 (Direct Color) or 7 (YUV). }
{ new Direct Color fields }
      RedMaskSize        : Byte; { 2size of direct color red mask in bits     }
      RedFieldPosition   : Byte; { bit position of LSB of red mask           }
      GreenMaskSize      : Byte; { size of direct color green mask in bits   }
      GreenFieldPosition : Byte; { bit position of LSB of green mask         }
      BlueMaskSize       : Byte; { size of direct color blue mask in bits    }
      BlueFieldPosition  : Byte; { bit position of LSB of blue mask          }
      RsvdMaskSize       : Byte; { size of direct color reserved mask in bits}
      DirectColorModeInfo: Byte; { Direct Color mode attributes              }
      Reserved_Arr       : array[0..216-1] of Byte;{ remainder of ModeInfoBlock}
    End;

CONST
{Vesa Text Video Modes: -----------------------------------------------------}
  VESA_80x60  = $108;
  VESA_132x25 = $109;
  VESA_132x43 = $10A;
  VESA_132x50 = $10B;
  VESA_132x60 = $10C;

{Bit test constants for the "ModeAttributes" field in the "ModeInfoBlockType"
 -like records: If an "AND" operation w/ the field and one of these constants
 gives true (non-zero value) the condition is satisfied ---------------------}

  ModeHWSupported   = $1;  { requested video mode is supported by HardWare   }
  Reserved          = $2;  { always 1 from VESA v1.2 on. If VESA version is  }
                           { below v1.2, this bit is set if the fields that  }
                           { come after the "BytesPerScanline"-field can be  }
                           { considered as valid.                            }
  OutputFnSupported = $4;  { Output functions are supported by BIOS          }
  ColorMode         = $8;  { If "AND" operation results in a 0, mode = Mono  }
  ModeType          = $10; { If "AND" operation results in a 0, -=> textmode }
{ All the other bits of this field are reserved in VESA version 1.2 }


{Bit test constants for the "WinAAttributes" and "WinBAttributes" field in the
 "ModeInfoBlockType"-like records: If an "AND" operation w/ the field and one
 of these constants gives true (non-zero value) the condition is satisfied --}
  WindowSupported   = $1;  { If bit isn't set (= 0), window isn't supported  }
  WindowReadable    = $2;  { If bit isn't set (= 0), window isn't readable   }
  WindowWriteable   = $4;  { If bit isn't set (= 0), window isn't writeable  }
{ All the other bits of this field are reserved in VESA version 1.2 }


{Here you have all the possible values for the "MemoryModel" field in the
 "ModeInfoBlockType"-like records: ------------------------------------------}
  Text_Mode       = $0; { Text mode                                          }
  CGAGFX          = $1; { CGA graphics                                       }
  HercGFX         = $2; { Hercules graphics                                  }
  FourPlanePlanar = $3; { 4-plane planar                                     }
  PackedPixel     = $4; { Packed pixel                                       }
  Non_chain4_256c = $5; { Non-chain 4, 256 color                             }
  DirectColor     = $6; { Direct Color  "new Direct Color fields" are valid  }
  YUV             = $7; { YUV           "new Direct Color fields" are valid  }
{ $8-$F  : to be defined by VESA (reserved) }
{ $10-$FF: to be defined by OEM             }

{Bit test values for the "DirectColorModeInfo" field: -----------------------}
{Only use the following in Direct Color Modes (see straight above.) }
  ProgrammableColorRamp = $1; { Color ramp is programmable                   }
  RsvdFieldBitsUsable   = $2; { Bits in "Rsvd" field are usable by the appl. }


{Here come some other constants: --------------------------------------------}
  Up    = $6;
  Down  = $7;

{to have the foregroundcolor blinking, just do: TextAttr:=TextAttr OR Blink; }
  Blink = 128;

{Foreground colors: }
  White        = 15;
  Yellow       = 14;
  LightMagenta = 13;
  LightRed     = 12;
  LightCyan    = 11;
  LightGreen   = 10;
  LightBlue    = 9 ;
  DarkGray     = 8 ;

{Background colors: }
  LightGray    = 7 ;
  Brown        = 6 ;
  Magenta      = 5 ;
  Red          = 4 ;
  Cyan         = 3 ;
  Green        = 2 ;
  Blue         = 1 ;
  Black        = 0 ;

  StandardVideo = Black shl 4 + LightGray; {LightGray on Black: standard vid.}

  Font8x8  = $12;
  Font8x14 = $11;
  Font8x16 = $14;

  ScanLines: Byte = 8;

{TextMode Constants: }
 {Constant ณ Value ณ Mode             ณ Adapter     }
 {อออออออออุอออออออุออออออออออออออออออุอออออออออออออ}
  BW40    = 0;   { ณ 40x25 B/W        ณ   CGA       }
  CO40    = 1;   { ณ 40x25 Color      ณ   CGA       }
  BW80    = 2;   { ณ 80x25 B/W        ณ   CGA       }
  CO80    = 3;   { ณ 80x25 Color      ณ   CGA       }
  Mono    = 7;   { ณ 80x25 B/W on MDA ณ   HGC       }
{  Font8x8 = 256;  ณ 43-/50-line mode ณ   EGA/VGA  ; use LoadFont or VESA }

{The below are for compatibility...}
  C40 = CO40;
  C80 = CO80;

  StandardCursor  = $1E1F;
  FineCursor      = $1F1F;
  BlockCursor     = $001F;
  HalfBlockCursor = $181F;
  NoCursor        = $201F;

  ASCIIZString = 1;
  PascalString = 2;

  DefMaxStrLen = 255;             { Default Maximum StringLength }
  MaxStrLen: Byte = DefMaxStrLen; { Maximum StringLength         }
{Here come the initialised data: --------------------------------------------}
  StringType: Byte = PascalString; { Default = use pascal strings }

  DirectVideo{ : Boolean}= True; { Use Direct-to-mem code? (False<>supported!}
  FilledBoxes  : Boolean = False;
  SetAttributes: Boolean = True;
  CursorTrace  : Boolean = True;
  VESAPresent  : Boolean = False; { VESA unavailable by default }

  Box1: Boxtype = (ord('ฺ'), ord('ฟ'), ord('ภ'), ord('ู'), ord('ฤ'), ord('ณ'), ord('ฤ'), ord('ณ'));
  Box2: Boxtype = (ord('ษ'), ord('ป'), ord('ศ'), ord('ผ'), ord('อ'), ord('บ'), ord('อ'), ord('บ'));
  Box3: Boxtype = (ord('ี'), ord('ธ'), ord('ิ'), ord('พ'), ord('อ'), ord('ณ'), ord('อ'), ord('ณ'));
  Box4: Boxtype = (ord('ึ'), ord('ท'), ord('ำ'), ord('ฝ'), ord('ฤ'), ord('บ'), ord('ฤ'), ord('บ'));
  Box5: Boxtype = (ord('Û'), ord('Û'), ord('Û'), ord('Û'), ord('฿'), ord('Û'), ord('Ü'), ord('Û'));
  Box6: Boxtype = (ord('Û'), ord('Û'), ord('Û'), ord('Û'), ord('Û'), ord('Û'), ord('Û'), ord('Û'));

VAR
{Here come the uninitialised data: ------------------------------------------}
  _VGAInfo : VgaInfoBlockType;      { for the VESA support: start }
  VGAInfo  : VgaInfoBlockTypePTR;
  _ModeInfo: ModeInfoBlockType;
  ModeInfo : ModeInfoBlockTypePTR;  { for the VESA support: end }

  TextAttr, LastMode, CurrentMode,
                             WhereX, WhereY: Byte; { these two are 1 - based }
  WindMin, WindMax,                                { these two are 0 - based }
                   VideoSEG, MaxRows, MaxColums : Word;

  ExtendedKeyboard: Boolean;

  MaxColumsCalc: Word;
{Here come the code: --------------------------------------------------------}

{VESA-specific functions: ---------------------------------------------------}
Procedure VESAGetInfo (VGAInfo: VgaInfoBlockTypePTR); { Init VESA, get misc info }
Function  VESAInitMode(Mode: Word): Boolean;
Function  VESAModeAvail(Mode: Word): Boolean;  { if a VESA mode is supported }
Function  VESAGetModeInfo(VESA_Mode: Word): Boolean; { true if record        }
                                                     { could be initialised  }

{Other functions: -----------------------------------------------------------}
Procedure Prepare4Mode; { Call it whenever you init'ed a special text mode.  }
Procedure TextMode(Mode: Integer); {Loads one of the text modes defined above}
Procedure LoadFont(Font: Byte);    {Loads one of the three standard fonts.
                                    call this proc immediately after TEXTMODE.
                                    Use Font8x8 to switch to mode 80x50      }
Procedure SetCursor(Cursor: Word); { sets/hides cursor; not too accurate...  }

Procedure GotoXY(X, Y: Byte);      { see the Crt unit                        }

Procedure Box  (X1, Y1, X2, Y2: word; BType: Boxtype); { Draws a "BType" box }
Procedure FBox (X1, Y1, X2, Y2: word);                 { Draws a "Fbox"      }
Procedure HBox (X1, Y1, X2, Y2: word; BType: Boxtype);
Procedure LBox (X1, Y1, X2, Y2: word; BType: Boxtype);

Procedure Clrscr;                           { see the Crt unit               }
Procedure ClearXY(X1, Y1, X2, Y2: Word);    { Custom clrscr                  }
Procedure TextColor(color: Byte);           { see the Crt unit               }
Procedure TextBackground(color: Byte);      { see the Crt unit               }
Procedure NormVideo;
Procedure HighVideo;
Procedure LowVideo;

function  getc(x, y: word): Byte;        { get a char                     }
Procedure putc(x, y: word; sign: Char);  { put a char                     }
Procedure print  (s: string);
Procedure println(s: string);
Procedure printArg(x, y, StrLen: word); { Print a list of Argument bytes  }
Procedure printXY(X, Y: word; s: string);

Function I2DStr(IntVal: LongInt): String;
Function I2HStr(IntVal: LongInt; ChRet: Byte): String;
Function I2HStr2(IntVal: LongInt; ChRet: Byte): String;{Integer==>Hex-String.}

Procedure Scroll(Direction: Byte; NrLin: Byte); { Scrolls window             }
Procedure Window(X1, Y1, X2, Y2: Byte); { see Window from the Crt unit       }

Function Readkey: Char;                 { see ReadKey from the Crt unit      }
Function Keypressed: Boolean;           { see KeyPressed from the Crt unit   }

Procedure Delay(MilliSec: Word);        { see the Crt unit }

{****************************************************************************}
{****************************************************************************}
{****************************************************************************}
{****************************************************************************}

implementation

Uses
{$IFDEF DPMI}
  WinAPI, HardWare, Strings, WinDos,
{$ELSE}
  Dos,
{$ENDIF}
  NewDelay;

CONST
  VESAFnPresent = $4F;
  VESAFnNr      = $4F;
  VESAFailed    = $100;

VAR
  OldTextAttr  : Byte;


{****************************************************************************}
{****************************************************************************}
{********* CURSOR-RELATED FUNCTIONS/PROCEDURES: *****************************}
{****************************************************************************}

Procedure GotoXY(X, Y: Byte);
var
{$IFDEF DPMI}
  regs: TRegisters;
{$ELSE}
  regs: Registers;
{$ENDIF}
Begin
  WhereX:=X; WhereY:=Y;
  If Not Cursortrace then exit;
  regs.ah:=$02;
  regs.bh:=$0;         {assume video page 0}

  regs.dl:=X+(WindMin AND $FF)-1;
  regs.dh:=Y+(WindMin shr 8  )-1;

  regs.ds:=0; regs.es:=0; { for compatibility with protected mode }
  Intr($10, regs);
End;

{****************************************************************************}

Procedure SetCursor(Cursor: Word);
var
{$IFDEF DPMI}
  regs: TRegisters;
{$ELSE}
  regs: Registers;
{$ENDIF}
Begin
  regs.ah:=1;
  regs.cx:=cursor;
  regs.ds:=0; regs.es:=0; { for compatibility with protected mode }
  Intr($10, regs);
End;

{****************************************************************************}
{****************************************************************************}
{********* OUTPUT-RELATED FUNCTIONS/PROCEDURES: *****************************}
{****************************************************************************}
{****************************************************************************}

Procedure VESAGetInfo(VGAInfo: VgaInfoBlockTypePTR);
VAR
  Status  : Word;
{$IFDEF DPMI}
  AllocOK : Boolean;
  p       : PPointer;
  rm, pm  : pointer;
  regs    : DPMIRegisters;
{$ELSE}
  regs    : TRegisters;
{$ENDIF}

BEGIN
  regs.ax:=VESAFnNr shl 8;
{$IFDEF DPMI}
  AllocOK:=AllocateDos(p, SizeOf(VgaInfoBlockType));
  If not AllocOK then begin VESAPresent:=False; exit; end;
  ClearDOS(p, SizeOf(VgaInfoBlockType));
  regs.es:=LongRec(LongInt(p.rm)).hi; regs.di:=LongRec(LongInt(p.rm)).lo;
  regs.ds:=0;
  Intr386($10, 0, regs);
  Move(PPOINTER(p).pm^, VGAInfo^, SizeOf(VgaInfoBlockType));
  FreeDOS(p, SizeOf(VgaInfoBlockType)); { free the valuable real mode memory }
{ convert the real-mode pointers to protected-mode pointers & copy data: ----}
  If GetMappedDPMIPtr(pm, VGAInfo^.OEMStringPtr, 127) then
    Begin
      GetMem(VGAInfo^.OEMStringPtr, 127);
      StrLCopy(PChar(VGAInfo^.OEMStringPtr), PChar(pm), 127-1);
      FreeMappedDPMIPtr(pm);
    end
  Else VGAInfo^.OEMStringPtr:=addr(EmptyNULLStr);
  If GetMappedDPMIPtr(pm, VGAInfo^.VideoModePtr, SizeOf(ModeArrayType)) then
    Begin
      GetMem(VGAInfo^.VideoModePtr, SizeOf(ModeArrayType));
      Move(pm^, VGAInfo^.VideoModePtr^, SizeOf(ModeArrayType));
      FreeMappedDPMIPtr(pm);
    end
  Else VGAInfo^.VideoModePtr:=nil;
{$ELSE}
  regs.es:=Seg(VGAInfo^); regs.di:=Ofs(VGAInfo^);
  Intr($10, regs);
{$ENDIF}
  status:=regs.ax;
  If Boolean(Status AND VESAFailed) or (Lo(Status)<>VESAFnPresent) or
    ((VGAInfo^.VESASignature[0]<>'V') or (VGAInfo^.VESASignature[1]<>'E') or
     (VGAInfo^.VESASignature[2]<>'S') or (VGAInfo^.VESASignature[3]<>'A'))
     then VESAPresent:=False
  Else VESAPresent:=True;
END;

{****************************************************************************}

Function VESAInitMode(Mode: Word): Boolean;
VAR
   Status, ChkMode: Word;
   regs: TRegisters;
BEGIN
  If not VesaPresent then Begin VESAInitMode:=False; exit; end;
  regs.ax:=VESAFnNr shl 8 + 2;
  regs.bx:=Mode;
  regs.es:=0; regs.ds:=0;        { for compatibility with protected mode }
  Intr($10, regs);
  status:=regs.ax;
  If (Lo(Status)<>VESAFnNr) or Boolean(Status AND VESAFailed) then
                                         Begin VESAInitMode:=False; exit; end;
  regs.ax:=VESAFnNr shl 8 + 3;
  regs.es:=0; regs.ds:=0;        { for compatibility with protected mode }
  Intr($10, regs);
  status:=regs.ax;
  ChkMode:=regs.bx;
{And now verify if the mode was succesfully initialised: --------------------
  ASM
    mov   ax, VESAFnNr shl 8 + 3 { VESA fn identifier + Get SVGA mode fn: 3
    int   10h                    { Call (VESA) BIOS: ax = status
    mov   [Status], ax
    mov   [ChkMode], bx          { SVGA Mode
  END;}

  Prepare4Mode;
  If (Lo(Status)<>VESAFnNr) or Boolean(Status AND VESAFailed) or (ChkMode<>Mode)
                     then Begin VESAInitMode:=False; exit; end;
 Case Mode of
   VESA_80x60 ,
   VESA_132x25,
   VESA_132x43,
   VESA_132x50,
   VESA_132x60: Scanlines:=8;
   Else; ScanLines:=8; { ?? }
 End;
 VESAInitMode:=True;
END;

{****************************************************************************}

Function VESAModeAvail(Mode: Word): Boolean;
VAR
   I: Word;
   ModeFound: Boolean;
BEGIN
  If Not VESAPresent then begin VESAModeAvail:=False; exit; end;
  I:=0; ModeFound:=False;
  Repeat
    If VGAInfo^.VideoModePtr^[I] = Mode then ModeFound:=True; Inc(I);
  Until (I>=MaxVideoModes) or ModeFound or (VGAInfo^.VideoModePtr^[I-1]=$FFFF);
  VESAModeAvail:=ModeFound;
END;

{****************************************************************************}

Function VESAGetModeInfo(VESA_Mode: Word): Boolean; { true if record         }
VAR                                                 { correctly initialised  }
  tmp: ModeInfoBlockTypePTR;
  I, Status: Word;
  ModeFound: Boolean;
{$IFDEF DPMI}
  AllocOK : Boolean;
  p       : PPointer;
  rm, pm  : pointer;
  regs    : DPMIRegisters;
{$ELSE}
  regs    : TRegisters;
{$ENDIF}
BEGIN
  If Not VESAPresent then              begin VESAGetModeInfo:=False; exit; end;
  If Not VESAModeAvail(VESA_Mode) then begin VESAGetModeInfo:=False; exit; end;

  regs.ax:=VESAFnNr shl 8 + 1;
  regs.cx:=VESA_Mode;
{$IFDEF DPMI}
  AllocOK:=AllocateDos(p, SizeOf(ModeInfoBlockType));
  If not AllocOK then begin VESAGetModeInfo:=False; exit; end;
  ClearDOS(p, SizeOf(ModeInfoBlockType));
  regs.es:=LongRec(LongInt(p.rm)).hi; regs.di:=LongRec(LongInt(p.rm)).lo;
  regs.ds:=0;
  Intr386($10, 0, regs);
  Move(PPOINTER(p).pm^, ModeInfo^, SizeOf(ModeInfoBlockType));
  FreeDOS(p, SizeOf(ModeInfoBlockType));
{  remap the real mode pointer to the VESA function to a protected mode
   pointer... like that's ever gonna work, haha                              }
  If GetMappedDPMIPtr(pm, ModeInfo^.WinFuncPtr, $FFFF) then
    Begin
      ModeInfo^.WinFuncPtr:=pm;
      FreeMappedDPMIPtr(pm);
    end
  Else ModeInfo^.WinFuncPtr:=nil;
{$ELSE}
  regs.es:=Seg(ModeInfo^); regs.di:=Ofs(ModeInfo^);
  Intr($10, regs);
{$ENDIF}
  status:=regs.ax;
  If (Lo(Status)<>VESAFnNr) or Boolean(Status AND VESAFailed)
                                  then begin VESAGetModeInfo:=False; exit; end;
  VESAGetModeInfo:=True;
END;

{****************************************************************************}
{****************************************************************************}
{****************************************************************************}
{****************************************************************************}

{Call this procedure RIGHT AFTER you initialised a new (text) mode via the
 BIOS. The procedure initialises some variables needed by the functions of the
 unit. If you omit a call to this function, the procedures in this unit will
 NOT work properly! You can Also call this procedure right after an initiali-
 sation of a custom (non-standard) text mode (example: card-dedicated 132*60).
}
Procedure Prepare4Mode;
BEGIN
  MaxColums:=Word(Ptr(Seg0040, $4a)^);   { Get the actual display width in colums }
  MaxRows  :=Byte(Ptr(Seg0040, $84)^)+1; { Get the actual display Height in Rows  }
  Case MaxRows of
    25: Scanlines:=16;
    28: Scanlines:=14;
    43, 50: Scanlines:=8;
  End;
  MaxColumsCalc:=MaxColums shl 1;   { For video memory index calculations    }
  Window(1, 1, MaxColums, MaxRows); { Init window coords                     }
{Get the actual video mode. Is it Mono? ==> change VideoSEG. !!!NOT TESTED!!!}
  If Byte(Ptr(Seg0040, $49)^) = Mono then VideoSEG:=SegB000
  Else                                    VideoSEG:=SegB800;
  ClrScr;
END;

{****************************************************************************}

Procedure TextMode(Mode: Integer);
VAR
  P: ^Byte;
  regs: TRegisters;
Begin
  LastMode:=Byte(Ptr(Seg0040, $49)^);
{Call BIOS to init the video mode: ------------------------------------------}
  regs.ax:=Mode;
  regs.es:=0; regs.ds:=0;        { for compatibility with protected mode }
  Intr($10, regs);
  CurrentMode:=Mode;
  If Mode = (CO80 + Font8x8) then LoadFont(Font8x8) else LoadFont(Font8x16);
  Prepare4Mode; { !IMPORTANT! Don't remove! }
End;

{****************************************************************************}

Procedure Window(X1, Y1, X2, Y2: Byte);
BEGIN
  If (X1 = 0)         or
     (X2 = 0)         or
     (Y1 = 0)         or
     (Y2 = 0)         or
     (X1 > MaxColums) or
     (X2 > MaxColums) or
     (Y1 > MaxRows)   or
     (Y2 > MaxRows)   or
     (X1 > X2)        or
     (Y1 > Y2)           then exit;  { exit for compatibility with Crt unit  }

  WindMin:=Integer(Y1 - 1) shl 8 + X1 - 1;
  WindMax:=Integer(Y2 - 1) shl 8 + X2 - 1;
  GotoXY(1, 1);                      { again for compatibility with Crt unit }
END;

{****************************************************************************

Procedure Shadow(X1, Y1, X2, Y2: Byte);
Var
   I: word;
BEGIN
  inc(X1, 2); inc(X2, 2); inc(Y1); inc(Y2);
  For I:=Y1 to Y2 do Mem[VideoSEG: I *160+(X2-1)*2+1]:=7;
  For I:=Y1 to Y2 do Mem[VideoSEG: I *160+X2*2+1]:=7;
  For I:=X1 to X2 do Mem[VideoSEG: Y2*160+I *2+1]:=7;
END;



Procedure RmShadow(X1, Y1, X2, Y2: Byte);
VAR
   I: word;
BEGIN
  inc(X1, 2); inc(X2, 2); inc(Y1); inc(Y2);
  For I:=Y1 to Y2 do Mem[VideoSEG: I *160+(X2-1)*2+1]:=23;
  For I:=Y1 to Y2 do Mem[VideoSEG: I *160+X2*2+1]:=23;
  For I:=X1 to X2 do Mem[VideoSEG: Y2*160+I *2+1]:=23;
END;

*****************************************************************************}

function getc(x, y: word): Byte;
BEGIN
  getc:=Mem[VideoSEG: MaxColumsCalc * y + x shl 1];
END;

{****************************************************************************}

Procedure putc(x, y: word; sign: Char);
BEGIN
  x:=MaxColumsCalc * y + x shl 1;
  Mem[VideoSEG: x    ]:=ord(sign);
  Mem[VideoSEG: x + 1]:=TextAttr;
END; {Procedure putc}

{****************************************************************************}

Procedure print(s: string);
VAR
   I, Len, d: Word;
BEGIN
  If (StringType=PascalString) then
              Begin Len:=Length(S); If Len>MaxStrLen then Len:=MaxStrLen; end;
  If (StringType=PascalString) then
    For I:=1 to Len do
    begin
      If (Lo(WindMin)+WhereX-1)>Lo(WindMax) then begin Inc(WhereY);   WhereX:=1;   end;
      If (Hi(WindMin)+WhereY-1)>Hi(WindMax) then begin Scroll(Up, 1); dec(WhereY); end;
      d:=(MaxColumsCalc * (Hi(WindMin)+WhereY-1))+(Lo(WindMin)+WhereX-1) shl 1;
      Mem[VideoSEG: d]:=Ord(S[I]);
      If SetAttributes then Mem[VideoSEG: d + 1]:=TextAttr;
      Inc(WhereX);
    end
  Else If (StringType=ASCIIZString) then
    begin
      I:=0;
      While (Ord(S[I])<>0) and (I<=MaxStrLen) do
      begin
        If (Lo(WindMin)+WhereX-1)>Lo(WindMax) then begin Inc(WhereY);   WhereX:=1;   end;
        If (Hi(WindMin)+WhereY-1)>Hi(WindMax) then begin Scroll(Up, 1); dec(WhereY); end;
        d:=(MaxColumsCalc * (Hi(WindMin)+WhereY-1))+(Lo(WindMin)+WhereX-1) shl 1;
        Mem[VideoSEG: d]:=Ord(S[I]);
        If SetAttributes then Mem[VideoSEG: d + 1]:=TextAttr;
        Inc(WhereX);
        Inc(I);
      end;
    end;
  If CursorTrace then GotoXY(WhereX, WhereY);
END;

{****************************************************************************}

Procedure Println(s: string);
BEGIN
  Print(s);
  Inc(WhereY);
  If (Hi(WindMin)+WhereY-1)>Hi(WindMax) then begin Scroll(Up, 1); dec(WhereY); end;
  WhereX:=1;
  If CursorTrace then GotoXY(WhereX, WhereY);
END;

{****************************************************************************}
{ absolute coords, 1,1 based }
Procedure PrintXY(X, Y: word; s: string);
VAR
   I, d, d2, Len, limit: word;
BEGIN
  If (X>MaxColums) or (Y>MaxRows) then exit;
  Dec(X); Dec(Y);
  If (StringType=PascalString) then Len:=Length(S)
  Else begin Len:=0; While Ord(S[Len])<>0 do Inc(Len); end;
  If Len>MaxStrLen then Len:=MaxStrLen;
  limit:=MaxColumsCalc * MaxRows;                 {bijv. 80*2*25 = 4000}
  d    :=MaxColumsCalc * Y + X shl 1;             {startaddr 0 based }
  if (d + (Len shl 1)) > Limit then Len:=(limit-d) shr 1;
  d2:=d + 1;
  If (StringType=PascalString) then
       For I:=1 to Len   do begin Mem[VideoSEG: d]:=Ord(S[I]); Inc(d, 2); end
  Else If (StringType=ASCIIZString) then
       For I:=0 to Len-1 do begin Mem[VideoSEG: d]:=Ord(S[I]); Inc(d, 2); end;
  If SetAttributes then
    For I:=1 to Len do begin Mem[VideoSEG: d2]:=TextAttr; Inc(d2, 2); end;
END;

{****************************************************************************}
{ absolute coords, 1,1 based }
Procedure printArg(x, y, StrLen: word);
VAR
   I, I2: word;
BEGIN
  Dec(x); Dec(y);
  I2:=(MaxColumsCalc * y) + (x shl 1) + 1;
  For I:=1 to StrLen do begin Mem[VideoSEG: I2]:=TextAttr; Inc(I2, 2); end;
END;

{****************************************************************************}

Function I2HStr(IntVal: LongInt; ChRet: Byte): String;
CONST
     HexStr: array[0..15] of Char = '0123456789ABCDEF';
VAR
   I: Byte;
   Dummy: String;
BEGIN
  For I:=0 to 7 do Dummy[ChRet-I]:=HexStr[(Byte(IntVal SHR (I SHL 2)) AND $0F)];
  Dummy[0]:=Chr(CHRet);
 I2HStr:=Dummy;
END;

{****************************************************************************}

Function I2HStr2(IntVal: LongInt; ChRet: Byte): String;
BEGIN
  I2HStr2:='0'+I2HStr(IntVal, ChRet)+'h';
END;

{****************************************************************************}

Function I2DStr(IntVal: LongInt): String;
VAR
  D1  : String;
  sign: LongInt;
Begin
  sign:=IntVal; If IntVal<0 then IntVal:=-IntVal;
  D1:='';
  Repeat
    D1:=Chr(Ord('0')+(IntVal mod 10))+D1; IntVal:=IntVal div 10;
  Until IntVal=0;
  If Sign<0 then D1:='-'+D1;
  I2DStr:=D1;
End;

{****************************************************************************}
{ deze procedure is zero-based }
Procedure ClearXY(X1, Y1, X2, Y2: Word); {maxwaarde voor color: 7}
Var
   I, I2: Word;
BEGIN
  If (X1 > MaxColums-1) or
     (X2 > MaxColums-1) or
     (Y1 > MaxRows  -1) or
     (Y2 > MaxRows  -1) or
     (X1 > X2)          or
     (Y1 > Y2)             then exit;
  For I2:=Y1 to Y2 do For I:=X1 to X2 do
                              Mem[VideoSEG: I2*MaxColumsCalc+I*2  ]:=32;
  If SetAttriButes then For I2:=Y1 to Y2 do For I:=X1 to X2 do
                              Mem[VideoSEG: I2*MaxColumsCalc+I*2+1]:=TextAttr;
END;

{****************************************************************************}

Procedure ClrScr;
BEGIN
  CLearXY(Lo(WindMin), Hi(WindMin), Lo(WindMax), Hi(WindMax));
  GotoXY(1, 1);
END;

{****************************************************************************}

Procedure TextColor(Color: Byte);
Begin
  TextAttr:=(TextAttr and 112) or (Color and (255-112));
End;

{****************************************************************************}

Procedure Textbackground(color: Byte);
Begin
  TextAttr:=(TextAttr and (255-112)) or ((Color and $F) shl 4);
End;

{****************************************************************************}

Procedure NormVideo;
Begin
  TextAttr:=7;
End;

{****************************************************************************}

Procedure HighVideo;
Begin
  TextAttr:=TextAttr or $8;
End;

{****************************************************************************}

Procedure LowVideo;
Begin
  TextAttr:=TextAttr and ($FF-$8);
End;

{****************************************************************************}

Procedure Scroll(Direction: Byte; NrLin: Byte);
Var
  regs: TRegisters;
Begin
  regs.ah:=Direction;
  regs.al:=NrLin;
  regs.cx:=WindMin;
  regs.dx:=WindMax;
  regs.bh:=TextAttr;
  regs.es:=0; regs.ds:=0;        { for compatibility with protected mode }
  Intr($10, regs);
End;

{****************************************************************************}
{****************************************************************************}
{*********  I/O ROUTINES: ***************************************************}
{****************************************************************************}

Function ReadKey : Char;
var
  regs: TRegisters;
Begin
  regs.ah:=$08;
  MsDos(regs);
  ReadKey:=chr(regs.al);
End;

{****************************************************************************}

Function KeyPressed : Boolean;
var
  regs: TRegisters;
Begin
  regs.ah:=$0B;
  MsDos(regs);
  If regs.al<>0 then KeyPressed:=true
  Else               KeyPressed:=false;
End;

{****************************************************************************
{****************************************************************************}
{*********  BOX ROUTINES: ***************************************************}
{****************************************************************************}
{****************************************************************************}
{ this procedure is 1,1 based }
Procedure HBox(X1, Y1, X2, Y2: word; BType: Boxtype);
VAR
   X, Y, d1, d2, xa, xb, ya, yb, xo: word;
   BColor, _TextAttr, bg, fc: Byte;
BEGIN
  Dec(X1); Dec(X2); Dec(Y1); Dec(Y2);
  If (X1 > MaxColums-1) or
     (X2 > MaxColums-1) or
     (Y1 > MaxRows  -1) or
     (Y2 > MaxRows  -1) or
     (X1 >= X2)         or
     (Y1 >= Y2)            then exit;

  If FilledBoxes then clearXY(X1+1, Y1+1, X2-1, Y2-1);

  bg:=TextAttr AND $70; { keep bits: 6,5,4: background color}
  fc:=TextAttr AND $F;  { fc = foreground color }
  If fc<>White then BColor:=Black Else BColor:=DARKGRAY;

  xa:=X1 shl 1; xb:=X2 shl 1; ya:=Y1 * MaxColumsCalc; yb:=Y2 * MaxColumsCalc;

  xo:=ya + xa; d1:=xo; d2:=yb + xa;
  For X:=X1+1 to X2-1 do
      begin
        inc(d1, 2); mem[VideoSEG: d1]:=BType[5];              {'ฤ'}
        inc(d2, 2); mem[VideoSEG: d2]:=BType[7];
      end;

  d1:=xo; d2:=ya + xb;
  For Y:=Y1+1 to Y2-1 do
      begin
        inc(d1, MaxColumsCalc); mem[VideoSEG: d1]:=BType[6];  {'ณ'}
        inc(d2, MaxColumsCalc); mem[VideoSEG: d2]:=BType[8];
      end;

  _TextAttr:=bg+fc;
  d1:=xo + 1;
  For X:=X1 to X2-1 do begin mem[VideoSEG: d1]:=_TextAttr; Inc(d1, 2); end;

  d1:=xo + 1;
  For Y:=Y1 to Y2 do
              begin mem[VideoSEG: d1]:=_TextAttr; Inc(d1, MaxColumsCalc); end;

  _TextAttr:=bg+BColor;
  d1:=yb + xa + 1;
  For X:=X1+1 to X2 do begin Inc(d1, 2); mem[VideoSEG: d1]:=_TextAttr; end;

  d1:=ya + xb + 1;
  For Y:=Y1 to Y2 do
              begin mem[VideoSEG: d1]:=_TextAttr; Inc(d1, MaxColumsCalc); end;

  mem[VideoSEG: xo     ]:=BType[1]; {'ฺ'}
  mem[VideoSEG: ya + xb]:=BType[2]; {'ฟ'}
  mem[VideoSEG: yb + xa]:=BType[3]; {'ภ'}
  mem[VideoSEG: yb + xb]:=BType[4]; {'ู'}
END;

{****************************************************************************}
{ this procedure is 1,1 based }
Procedure LBox(X1, Y1, X2, Y2: word; BType: Boxtype);
VAR
   X, Y, d1, d2, xa, xb, ya, yb, xo: word;
   BColor, _TextAttr, bg, fc: Byte;
BEGIN
  Dec(X1); Dec(X2); Dec(Y1); Dec(Y2);
  If (X1 > MaxColums-1) or
     (X2 > MaxColums-1) or
     (Y1 > MaxRows  -1) or
     (Y2 > MaxRows  -1) or
     (X1 >= X2)         or
     (Y1 >= Y2)            then exit;

  If FilledBoxes then clearXY(X1+1, Y1+1, X2-1, Y2-1);

  bg:=TextAttr AND $70; { keep bits: 6,5,4: background color}
  fc:=TextAttr AND $F;  { fc = foreground color }
  If fc<>White then BColor:=Black Else BColor:=DARKGRAY;

  xa:=X1 shl 1; xb:=X2 shl 1; ya:=Y1 * MaxColumsCalc; yb:=Y2 * MaxColumsCalc;

  xo:=ya + xa; d1:=xo; d2:=yb + xa;
  For X:=X1+1 to X2-1 do
      begin
        inc(d1, 2); mem[VideoSEG: d1]:=BType[5];              {'ฤ'}
        inc(d2, 2); mem[VideoSEG: d2]:=BType[7];
      end;

  d1:=xo; d2:=ya + xb;
  For Y:=Y1+1 to Y2-1 do
      begin
        inc(d1, MaxColumsCalc); mem[VideoSEG: d1]:=BType[6];  {'ณ'}
        inc(d2, MaxColumsCalc); mem[VideoSEG: d2]:=BType[8];
      end;

  _TextAttr:=bg+BColor;
  d1:=xo + 1;
  For X:=X1 to X2-1 do begin mem[VideoSEG: d1]:=_TextAttr; Inc(d1, 2); end;

  d1:=xo + 1;
  For Y:=Y1 to Y2 do
              begin mem[VideoSEG: d1]:=_TextAttr; Inc(d1, MaxColumsCalc); end;

  _TextAttr:=bg+fc;
  d1:=yb + xa + 1;
  For X:=X1+1 to X2 do begin Inc(d1, 2); mem[VideoSEG: d1]:=_TextAttr; end;

  d1:=ya + xb + 1;
  For Y:=Y1 to Y2 do
              begin mem[VideoSEG: d1]:=_TextAttr; Inc(d1, MaxColumsCalc); end;

  mem[VideoSEG: xo     ]:=BType[1]; {'ฺ'}
  mem[VideoSEG: ya + xb]:=BType[2]; {'ฟ'}
  mem[VideoSEG: yb + xa]:=BType[3]; {'ภ'}
  mem[VideoSEG: yb + xb]:=BType[4]; {'ู'}
END;

{****************************************************************************}
{ this procedure is 1,1 based }
Procedure Box(X1, Y1, X2, Y2: word; BType: Boxtype);
VAR
   X, Y, d1, d2, xa, xb, ya, yb, xo: word;
BEGIN
  Dec(X1); Dec(X2); Dec(Y1); Dec(Y2);
  If (X1 > MaxColums-1) or
     (X2 > MaxColums-1) or
     (Y1 > MaxRows  -1) or
     (Y2 > MaxRows  -1) or
     (X1 >= X2)         or
     (Y1 >= Y2)            then exit;

  If FilledBoxes then clearXY(X1+1, Y1+1, X2-1, Y2-1);

  xa:=X1 shl 1; xb:=X2 shl 1; ya:=Y1 * MaxColumsCalc; yb:=Y2 * MaxColumsCalc;

  xo:=ya + xa; d1:=xo; d2:=yb + xa;
  For X:=X1+1 to X2-1 do
      begin
        inc(d1, 2); mem[VideoSEG: d1]:=BType[5];              {'ฤ'}
        inc(d2, 2); mem[VideoSEG: d2]:=BType[7];
      end;

  d1:=xo; d2:=ya + xb;
  For Y:=Y1+1 to Y2-1 do
      begin
        inc(d1, MaxColumsCalc); mem[VideoSEG: d1]:=BType[6];  {'ณ'}
        inc(d2, MaxColumsCalc); mem[VideoSEG: d2]:=BType[8];
      end;

  If SetAttributes then
  Begin
    d1:=xo + 1; d2:=yb + xa + 1;
    For X:=X1 to X2 do
      begin
        mem[VideoSEG: d1]:=TextAttr; Inc(d1, 2);
        mem[VideoSEG: d2]:=TextAttr; Inc(d2, 2);
      end;

    d1:=xo + 1; d2:=ya + xb + 1;
    For Y:=Y1+1 to Y2-1 do
      begin
        Inc(d1, MaxColumsCalc); mem[VideoSEG: d1]:=TextAttr;
        Inc(d2, MaxColumsCalc); mem[VideoSEG: d2]:=TextAttr;
      end;
  End;

  mem[VideoSEG: xo     ]:=BType[1]; {'ฺ'}
  mem[VideoSEG: ya + xb]:=BType[2]; {'ฟ'}
  mem[VideoSEG: yb + xa]:=BType[3]; {'ภ'}
  mem[VideoSEG: yb + xb]:=BType[4]; {'ู'}
END;

{****************************************************************************}
{ this procedure is 1,1 based }
Procedure FBox(X1, Y1, X2, Y2: word);
VAR
   X, Y, d1, d2, xa, xb, ya, yb, xo: word;
BEGIN
  Dec(X1); Dec(X2); Dec(Y1); Dec(Y2);
  If (X1 > MaxColums-1) or
     (X2 > MaxColums-1) or
     (Y1 > MaxRows  -1) or
     (Y2 > MaxRows  -1) or
     (X1 >= X2)         or
     (Y1 >= Y2)            then exit;

  If FilledBoxes then clearXY(X1, Y1, X2, Y2);

  xa:=X1 shl 1; xb:=X2 shl 1; ya:=Y1 * MaxColumsCalc; yb:=Y2 * MaxColumsCalc;

  xo:=ya + xa; d1:=xo; d2:=yb + xa;
  For X:=X1+1 to X2-1 do
      begin
        inc(d1, 2); mem[VideoSEG: d1]:=223;   {'฿'}
        inc(d2, 2); mem[VideoSEG: d2]:=220;   {'Ü'}
      end;

  d1:=xo; d2:=ya + xb;
  For Y:=Y1 to Y2 do
      begin
        mem[VideoSEG: d1]:=219; inc(d1, MaxColumsCalc); {'Û'}
        mem[VideoSEG: d2]:=219; inc(d2, MaxColumsCalc);
      end;

  If SetAttributes and not FilledBoxes then
  Begin
    d1:=xo + 1; d2:=yb + xa + 1;
    For X:=X1+1 to X2-1 do
      begin
        Inc(d1, 2); mem[VideoSEG: d1]:=TextAttr;
        Inc(d2, 2); mem[VideoSEG: d2]:=TextAttr;
      end;

    d1:=xo + 1; d2:=ya + xb + 1;
    For Y:=Y1 to Y2 do
      begin
        mem[VideoSEG: d1]:=TextAttr; Inc(d1, MaxColumsCalc);
        mem[VideoSEG: d2]:=TextAttr; Inc(d2, MaxColumsCalc);
      end;
  End;
END;

{****************************************************************************}
{****************************************************************************}

Procedure LoadFont(Font: Byte);
VAR
  regs: TRegisters;
Begin
  regs.al:=Font;
  regs.ah:=$11;
  regs.bl:=0;
  regs.es:=0; regs.ds:=0;        { for compatibility with protected mode }
  Intr($10, regs);
  Prepare4Mode;
End;

{****************************************************************************}
{****************************************************************************}

Procedure Delay(MilliSec: Word); {assembler;}
BEGIN
  NDelay(MilliSec);
END;

{****************************************************************************}
{****************************************************************************}
{*********** END OF UNIT: INITIALIZE DEFAULTS. ******************************}
{****************************************************************************}
{****************************************************************************}

{Initialise the unit's variables: -------------------------------------------}
BEGIN
  LastMode:=Byte(Ptr(Seg0040, $49)^);
  ExtendedKeyboard:=(Byte(Ptr(Seg0040, $96)^) AND $10) <> 0;
  CurrentMode:=LastMode;
{ DEBUG TEMP !!! *** DISABLED causes WinXP BSOD ;) }
{ Prepare for VESA Support --------------------------------------------------}
  VGAInfo :=@_VGAInfo;
  VESAGetInfo(VGAInfo);
  ModeInfo:=@_ModeInfo;

{Final initialising: --------------------------------------------------------}
  Prepare4Mode;
  NormVideo;

{Read Current cursor position: ----------------------------------------------
  ASM
    mov   es, [VideoSeg]
    mov   ah, 03h
    xor   bh, bh
    int   10h
    mov   [WhereX], dl
    mov   [WhereY], dh

{Now read current attribute at cursor position: -----------------------------
    mov   al, dh
    xor   ah, ah
    mov   bx, [MaxColumsCalc]
    mul   bx
    xor   dh, dh
    add   ax, dx
    mov   si, ax
    mov   al, [es:si+1]  ; we need the ATTRIBUTE BYTE, not the character!
    mov   [OldTextAttr], al
  END;
}
END.

