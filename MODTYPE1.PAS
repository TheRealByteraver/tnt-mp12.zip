{
  Copyright (c) 1998 Erland Van Olmen (erlandvo@hotmail.com)
  Protected Mode Ready

ўўBugfixes/changes/additions since version 1.0 (first public release):

      - None

ўўBugfixes/changes/additions since version 1.1 (second public release):

      - Made some small modifications for the WAV writer, and the now
        fully functional DPMI version of the player. Nothing important.
}
Unit MODType1;

interface

{This constants are needed for the type definitions: ------------------------}
Const
  PlayerVersion = '1.2';

  MaxWord  = $FFFF;
  MaxAlloc = MaxWord-$1F;

Type
{These type definitions are needed by the MOD loader & replay routines: -----}
  TSampleInfo = Record
    Name        : String[22];
    Length      : LongInt;
    RepeatOffset: LongInt;
    RepeatLength: LongInt;
    RepeatSample: Boolean;
    Used        : Boolean;
    Volume,
    FineTune,
    OldFineTune : ShortInt;
  end;

  TModInfo = Record
    NrChannels     : Byte;
    SongTitle      : String[20];
    ModFileName    : String;
    NrSamples      : Byte;
    NrPatterns     : Byte;
    MaxSamples     : Byte;   { 15 or 31 }
    CustomRepeat   : Boolean;
    RepeatModule   : Boolean;
    FinishedReplay : Boolean;
    SongLength     : Byte;
    RestartPosition: Byte;
    PatternTable   : Array[0..127] of Byte;
    Tag            : String[4];
    Samples        : Array[1..31] of TSampleInfo;
  end;

  TChannels = Record
{General channel info: ------------------------------------------------------}
    Muted,
    Replay         : Boolean;
    Period         : Word;
    Pan,
    Instr,
    Note,
    Vol            : Byte;
{Needed for the effect engine: ----------------------------------------------}
    Effect,
    EffectArg,
    LastEffect,
    LastEffectArg  : Byte;
    SmpOffset,
    ArpeggioPeriod : Word;
    ArpeggioCount,
    arp1,
    arp2           : Byte;
    DestPeriod     : Word;
    LastSlideRate  : Byte;
    Glissando      : Boolean;
    VibratoCount,
    VibratoWave,
    LastVibrato,
    TremoloCount,
    TremoloWave,
    LastTremolo,
    DelayCount     : Byte;
    DelayFlag      : Boolean;
    RetrigCount    : Byte;
    UpdateLine,                                 { Needed for the ptn loop FX }
    Looping        : Boolean;
    LoopStartLine,
    LoopCounter    : Byte;
  End;

{These types are needed for the Drivers: ------------------------------------}
  TOutputDevice = Record
    SWDetected: Boolean;    { If the device was actually detected            }
    HWDetected: Boolean;    { If the device was actually detected            }
    SubType   : Word;       { DSP version (for example)                      }
    ID        : Word;       { Just a number                                  }
    Name      : String[30]; { Name of the device (ex: 'SoundBlaster 1.0'     }
    MEnvStr   : String[20]; { Main      Environment variable of the device   }
    SEnvStr   : String[20]; { Secondary Environment variable of the device   }
    MPort     : Word;       { Main      I/O Port (^- not always available)   }
    SPort     : Word;       { Secondary I/O Port    (not always available)   }
    MIRQ      : Byte;       { Main      IRQ                                  }
    SIRQ      : Byte;       { Secondary IRQ         (not always available)   }
    MDMA      : Byte;       { Main      DMA Channel                          }
    SDMA      : Byte;       { Secondary DMA Channel (not always available)   }

{And this is the "code": first, some misc procedures }
{*} LoadMODSample: Function(var F: File; SampleNr: Byte)            : Word;
    HW_Detect    : Function                                         : Boolean;
{*} OpenCard     : Function(NrVoices: Byte)                         : Word;
{*} CloseCard    : Function                                         : Word;
{*} StartReplay  : Function                                         : Word;
{*} StopReplay   : Function                                         : Word;
{*} SetBPM       : Function                                         : Word;
{And now the replay-related procedures }
{*} VSetMode     : Function(Voice, Mode: Byte)                      : Word;
    VGetMode     : Function(Voice: Byte)                            : Byte;
{*} VPlay        : Function(Voice, Sample: Byte; Offset: Word)      : Word;
{*} VSetPanning  : Function(Voice, Panning: Byte)                   : Word;
{*} VSetVolume   : Function(Voice, Volume: Byte)                    : Word;
{*} VSetFrequency: Function(Voice: Byte; Frequency: LongInt)        : Word;
  end;
{ * -=> these functions return NoError if everything went fine. If an error
        occured, a non-zero value is returned.                               }

  TByteBuffer = Array[0..MaxAlloc-1] of Byte;
  PByteBuffer = ^TByteBuffer;
  TIntBuffer  = Array[0..MaxAlloc div 2 - 1] of Integer;
  PIntBuffer  = ^TIntBuffer;

{And now the constants: -----------------------------------------------------}
Const
  PeriodRec: Array[1..8*12] of Word = (
   4*1712,4*1616,4*1524,4*1440,4*1356,4*1280,4*1208,4*1140,4*1076,4*1016, 4*960, 4*906,
   2*1712,2*1616,2*1524,2*1440,2*1356,2*1280,2*1208,2*1140,2*1076,2*1016, 2*960, 2*906,
     1712,  1616,  1524,  1440,  1356,  1280,  1208,  1140,  1076,  1016,   960,   906,
      856,   808,   762,   720,   678,   640,   604,   570,   538,   508,   480,   453,
      428,   404,   381,   360,   339,   320,   302,   285,   269,   254,   240,   226,
      214,   202,   190,   180,   170,   160,   151,   143,   135,   127,   120,   113,
      107,   101,    95,    90,    85,    80,    75,    71,    67,    63,    60,    56,
       53,    50,    47,    45,    42,    40,    37,    35,    33,    31,    30,    28);

{ PeriodTable (Finetunes 0..$F (or 0..7, -8..-1)): --------------------------}
{ 14 Notes are included here w/ finetunes
           C-1 .. B-1 ; standard octave
           C-2 .. B-2 ; standard octave
           C-3 .. B-3 ; standard octave
           C-4 .. B-4 ; eXtended octave                                      }
  PeriodTable: Array[0..14*8] of Word = (
    960,954,948,940,934,926,920,914,
    907,900,894,887,881,875,868,862,856,850,844,838,832,826,820,814,
    808,802,796,791,785,779,774,768,762,757,752,746,741,736,730,725,
    720,715,709,704,699,694,689,684,678,675,670,665,660,655,651,646,
    640,636,632,628,623,619,614,610,604,601,597,592,588,584,580,575,
    570,567,563,559,555,551,547,543,538,535,532,528,524,520,516,513,
    508,505,502,498,494,491,487,484,480,477,474,470,467,463,460,457,
    453,450,447,443,440,437,434,431,428);

{Extended vs Standard octaves: ----------------------------------------------}
  NormalMaxPeriod   = 907;
  NormalMinPeriod   = 108;
  ExtendedMaxPeriod = 4*1712+100;
  ExtendedMinPeriod = 28    -5;
  MaxPeriod  : Word = ExtendedMaxPeriod;
  MinPeriod  : Word = ExtendedMinPeriod;
  ExtendedOctaves: Boolean = True;          { Default = Use extended octaves }

{This table is used for the Vibrato & Tremolo FX: ---------------------------}
{ The table is taken from the GUSMOD unit by Mc.Kriss/Extravaganza. Thanks!
  VibratoTable: Array [0..2,0..63] of ShortInt = (
    ( 00, 06, 12, 19, 24, 30, 36, 41, 45, 49, 53, 56, 59, 61, 63, 64 ,
      64, 64, 63, 61, 59, 56, 53, 49, 45, 41, 36, 30, 24, 19, 12, 06 ,
      00,-06,-12,-19,-24,-30,-36,-41,-45,-49,-53,-56,-59,-61,-63,-64 ,
     -64,-64,-63,-61,-59,-56,-53,-49,-45,-41,-36,-30,-24,-19,-12,-06),
    (-63,-61,-59,-57,-55,-53,-51,-49,-47,-45,-43,-41,-39,-37,-35,-33 ,
     -31,-29,-27,-25,-23,-21,-19,-17,-15,-13,-11,-09,-07,-05,-03,-01 ,
      01, 03, 05, 07, 09, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31 ,
      33, 35, 37, 39, 41, 43, 45, 47, 49, 51, 53, 55, 57, 59, 61, 63),
    (-64,-64,-64,-64,-64,-64,-64,-64,-64,-64,-64,-64,-64,-64,-64,-64 ,
     -64,-64,-64,-64,-64,-64,-64,-64,-64,-64,-64,-64,-64,-64,-64,-64 ,
      64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64 ,
      64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64)
  );
}
{Taken from CapaMOD:}
  VibratoTable: Array [0..2, 0..255] of Integer = (
   (0,6,13,19,25,31,38,44                   ,
    50,56,62,68,74,80,86,92                 ,
    98,104,109,115,121,126,132,137          ,
    142,147,152,157,162,167,172,177         ,
    181,185,190,194,198,202,206,209         ,
    213,216,220,223,226,229,231,234         ,
    237,239,241,243,245,247,248,250         ,
    251,252,253,254,255,255,256,256         ,
    256,256,256,255,255,254,253,252         ,
    251,250,248,247,245,243,241,239         ,
    237,234,231,229,226,223,220,216         ,
    213,209,206,202,198,194,190,185         ,
    181,177,172,167,162,157,152,147         ,
    142,137,132,126,121,115,109,104         ,
    98,92,86,80,74,68,62,56                 ,
    50,44,38,31,25,19,13,6                  ,
    0,-6,-13,-19,-25,-31,-38,-44            ,
    -50,-56,-62,-68,-74,-80,-86,-92         ,
    -98,-104,-109,-115,-121,-126,-132,-137  ,
    -142,-147,-152,-157,-162,-167,-172,-177 ,
    -181,-185,-190,-194,-198,-202,-206,-209 ,
    -213,-216,-220,-223,-226,-229,-231,-234 ,
    -237,-239,-241,-243,-245,-247,-248,-250 ,
    -251,-252,-253,-254,-255,-255,-256,-256 ,
    -256,-256,-256,-255,-255,-254,-253,-252 ,
    -251,-250,-248,-247,-245,-243,-241,-239 ,
    -237,-234,-231,-229,-226,-223,-220,-216 ,
    -213,-209,-206,-202,-198,-194,-190,-185 ,
    -181,-177,-172,-167,-162,-157,-152,-147 ,
    -142,-137,-132,-126,-121,-115,-109,-104 ,
    -98,-92,-86,-80,-74,-68,-62,-56         ,
    -50,-44,-38,-31,-25,-19,-13,-6         ),

   (256,254,252,250,248,246,244,242         ,
    240,238,236,234,232,230,228,226         ,
    224,222,220,218,216,214,212,210         ,
    208,206,204,202,200,198,196,194         ,
    192,190,188,186,184,182,180,178         ,
    176,174,172,170,168,166,164,162         ,
    160,158,156,154,152,150,148,146         ,
    144,142,140,138,136,134,132,130         ,
    128,126,124,122,120,118,116,114         ,
    112,110,108,106,104,102,100,98          ,
    96,94,92,90,88,86,84,82                 ,
    80,78,76,74,72,70,68,66                 ,
    64,62,60,58,56,54,52,50                 ,
    48,46,44,42,40,38,36,34                 ,
    32,30,28,26,24,22,20,18                 ,
    16,14,12,10,8,6,4,2                     ,
    0,-2,-4,-6,-8,-10,-12,-14               ,
    -16,-18,-20,-22,-24,-26,-28,-30         ,
    -32,-34,-36,-38,-40,-42,-44,-46         ,
    -48,-50,-52,-54,-56,-58,-60,-62         ,
    -64,-66,-68,-70,-72,-74,-76,-78         ,
    -80,-82,-84,-86,-88,-90,-92,-94         ,
    -96,-98,-100,-102,-104,-106,-108,-110   ,
    -112,-114,-116,-118,-120,-122,-124,-126 ,
    -128,-130,-132,-134,-136,-138,-140,-142 ,
    -144,-146,-148,-150,-152,-154,-156,-158 ,
    -160,-162,-164,-166,-168,-170,-172,-174 ,
    -176,-178,-180,-182,-184,-186,-188,-190 ,
    -192,-194,-196,-198,-200,-202,-204,-206 ,
    -208,-210,-212,-214,-216,-218,-220,-222 ,
    -224,-226,-228,-230,-232,-234,-236,-238 ,
    -240,-242,-244,-246,-248,-250,-252,-254),

   (256,256,256,256,256,256,256,256         ,
    256,256,256,256,256,256,256,256         ,
    256,256,256,256,256,256,256,256         ,
    256,256,256,256,256,256,256,256         ,
    256,256,256,256,256,256,256,256         ,
    256,256,256,256,256,256,256,256         ,
    256,256,256,256,256,256,256,256         ,
    256,256,256,256,256,256,256,256         ,
    256,256,256,256,256,256,256,256         ,
    256,256,256,256,256,256,256,256         ,
    256,256,256,256,256,256,256,256         ,
    256,256,256,256,256,256,256,256         ,
    256,256,256,256,256,256,256,256         ,
    256,256,256,256,256,256,256,256         ,
    256,256,256,256,256,256,256,256         ,
    256,256,256,256,256,256,256,256         ,
    -256,-256,-256,-256,-256,-256,-256,-256 ,
    -256,-256,-256,-256,-256,-256,-256,-256 ,
    -256,-256,-256,-256,-256,-256,-256,-256 ,
    -256,-256,-256,-256,-256,-256,-256,-256 ,
    -256,-256,-256,-256,-256,-256,-256,-256 ,
    -256,-256,-256,-256,-256,-256,-256,-256 ,
    -256,-256,-256,-256,-256,-256,-256,-256 ,
    -256,-256,-256,-256,-256,-256,-256,-256 ,
    -256,-256,-256,-256,-256,-256,-256,-256 ,
    -256,-256,-256,-256,-256,-256,-256,-256 ,
    -256,-256,-256,-256,-256,-256,-256,-256 ,
    -256,-256,-256,-256,-256,-256,-256,-256 ,
    -256,-256,-256,-256,-256,-256,-256,-256 ,
    -256,-256,-256,-256,-256,-256,-256,-256 ,
    -256,-256,-256,-256,-256,-256,-256,-256 ,
    -256,-256,-256,-256,-256,-256,-256,-256));

{Loader Errors: -------------------------------------------------------------}
{ (+128 to avoid "interference" w/ the errors returned by IOResult)          }
  NoError         = 0    ; { No error, everything goes fine                  }
  WrongExtension  = 1+128; { Extension was wrong                             }
  WrongFileName   = 2+128; { File does not exists                            }
  WrongFile       = 3+128; { File is not a real AMIGA Module                 }
  OutOfMemory     = 4+128; { Module contains too much data; out of Memory!   }
  OutOfGusMemory  = 5+128; { Samples are too big: out of GUS Memory!         }
  SampleLoadError = 6+128; { I/O Error whilst loading samples!               }
  OutOfSampleData = 7+128; { I/O Error whilst loading samples!               }
  AbortedByUser   = 8+128; { Error occured, and user decided to abort loading}
  PP20CorruptMod  = 9+128; { This MOD is a PP20 compressed Module            }
  ToHighPatternNr =10+128; { Detected pattern nr > 127 in pattern seq. table }
  NoPatterns      =11+128; { No patterns to play                             }
  PP20DecompErr   =12+128; { Cannot decompress PP20 compressed module        }
  DecompNotFound  =13+128; { Couldn't find PP2MOD.EXE                        }

{ Here come some Device-ID constants: ---------------------------------------}
  MaxDevices  = 8; { total nr of devices               }
  NODevice    = 0; { none                              }
  GUSDevice   = 1; { GRAVIS UltraSound                 }
  SB100Device = 2; { SoundBlaster 1.0                  }
  SB200Device = 3; { SoundBlaster 2.0                  }
  SB201Device = 4; { SoundBlaster 2.01                 }
  SBPRODevice = 5; { SoundBlaster PRO                  }
  SB16Device  = 6; { SoundBlaster 16 (ASP)             }
  WavDevice   = 7; { No replay, Write WAV file instead }

  DeviceNames: Array[0..MaxDevices-1] of String[30] = (
    'No Device',
    'GRAVIS UltraSound',
    'SoundBlaster 1.xx',
    'SoundBlaster 2.0',
    'SoundBlaster 2.1',
    'SoundBlaster PRO',
    'SoundBlaster 16',
    'Mod-to-Wav file writer'
  );

  GlobalVolume: Word = 70; {GlobalVolume In Percent}
{
  NOFilter      = 0;
  SBPROHW       = 1;
  Interpolative = 2;
}
{Flags for MAIN Program: ----------------------------------------------------}
  Loaded : Boolean = False;                { If Music is playing             }
  Playing: Boolean = False;                { If File was correctly loaded    }
{  Filter : Byte    = NOFilter;}

VAR
  PlayDevice : TOutputDevice;
  ModInfo    : TModInfo;
  Channels   : array[1..32]  Of TChannels;
  Patterns   : Array[0..127] Of PByteBuffer;
  FreqTable  : Array[ExtendedMinPeriod..ExtendedMaxPeriod] Of LongInt;
  WaitState  : Boolean;             { Set to true whenever a line was played }
  Timing     : Record
                 Count,
                 Speed,
                 PatternDelay: Word;    { for pattern delay effect }
                 Bpm         : Byte;
                 VBlank      : Boolean; { if true effect Fxx is always tempo }
               End;

{This makes some information available to the main program: -----------------}
  PatternInfo: Record
                 Line,
                 Track,
                 Pattern: Byte;
                 PatternData: PByteBuffer;
                 ActLine: Array[1..32] of Record
                                            Note,
                                            Sample,
                                            Effect,
                                            EffectArg: Byte;
                                          End;
               End;

Function ErrStr(Error: Word): String;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}
{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

implementation

Uses
  NewCrt;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}
{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

Function ErrStr(Error: Word): String;
BEGIN
  Case Error of
    NoError        : ErrStr:='No error detected.';
{
 Code Г Meaning
ЭЭЭЭЭиЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭЭ
  2  Г File not found
  3  Г Path not found
  5  Г Access denied
  6  Г Invalid handle
  8  Г Not enough memory
 10  Г Invalid environment
 11  Г Invalid format
 18  Г No more files

    FileNotFound   : ErrStr:='File not found';
    PathNotFound   : ErrStr:='Path not found';
    AccessDenied   : ErrStr:='Access Denied';
    InValidHandle  : ErrStr:='Invalid Handle';
}
    WrongExtension : ErrStr:='The extension of the filespec is wrong. Loading Aborted By User.';
    WrongFileName  : ErrStr:='The specified file doesn''t exist.';
    WrongFile      : ErrStr:='The specified file is not a Real AMIGA Module.';
    OutOfMemory    : ErrStr:='The song is too big. Out of heap Memory. Try the DPMI version of MP!';
    OutOfGusMemory : ErrStr:='The samples are too big. Out of GUS Memory.';
    SampleLoadError: ErrStr:='I/O Error while loading samples! Corrupt MOD File!';
    AbortedByUser  : ErrStr:='Loading aborted by user, exiting. Bye Bye!';
    PP20CorruptMod : ErrStr:='This module is compressed with PowerPacker 2.0. Use the DPMI version of MP!';
    ToHighPatternNr: ErrStr:='The pattern sequence table contains a number higher than 127!';
    NoPatterns     : ErrStr:='There are no patterns (there is no partiture for replay)!';
    PP20DecompErr  : ErrStr:='Cannot decompress PP20 compressed module! Please decompress manually.';
    DecompNotFound : ErrStr:='Couldn''t find the PP2MOD.EXE decompressing tool.';
    else; ErrStr:='Unknown error ' + I2DStr(Error) + '! Contact me!!!';
  End;
END;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}
{$F+}
Procedure EmptyProc;
BEGIN
  Writeln('Fatal Error: Empty function was called. Halting.');
  Halt;
END;
{$F-}
{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

BEGIN
{  Timing.PatternDelay:=0;}
  With PlayDevice do
  Begin
    SubType:=0;
    ID     :=MODType1.NoDevice;
    Name   :=DeviceNames[ID];
    MEnvStr:='';
    SEnvStr:='';
    MPort  :=0;
    SPort  :=0;
    MIRQ   :=0;
    SIRQ   :=0;
    MDMA   :=0;
    SDMA   :=0;
    HWDetected:=False;
    SWDetected:=True;
  End;

  Timing.VBlank:=False;
  Addr(PlayDevice.LoadMODSample):=Addr(EmptyProc);
  Addr(PlayDevice.HW_Detect    ):=Addr(EmptyProc);
  Addr(PlayDevice.OpenCard     ):=Addr(EmptyProc);
  Addr(PlayDevice.CloseCard    ):=Addr(EmptyProc);
  Addr(PlayDevice.StartReplay  ):=Addr(EmptyProc);
  Addr(PlayDevice.StopReplay   ):=Addr(EmptyProc);
  Addr(PlayDevice.SetBPM       ):=Addr(EmptyProc);
  Addr(PlayDevice.VSetMode     ):=Addr(EmptyProc);
  Addr(PlayDevice.VGetMode     ):=Addr(EmptyProc);
  Addr(PlayDevice.VPlay        ):=Addr(EmptyProc);
  Addr(PlayDevice.VSetPanning  ):=Addr(EmptyProc);
  Addr(PlayDevice.VSetVolume   ):=Addr(EmptyProc);
  Addr(PlayDevice.VSetFrequency):=Addr(EmptyProc);
END.

