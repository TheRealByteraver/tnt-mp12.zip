{
  Copyright (c) 1998 Erland Van Olmen (erlandvo@hotmail.com)
  Protected Mode Ready

  Known Bugs:

      - Sometimes the program displays garbage, and then goes on.
        I have no idea why.
        You can stop the output by hitting the pause key. This might
        come in handy, because the garbage output causes annoying beeps
        sometimes.
        --==>>> this still happens when aevias.mod is repeated
                also happens more easily with the BP mixer (xenolog1.mod)
                Apparently the pattern data gets corrupted, The code that
                displays the pattern data is now protected and displays
                'ERR' for wrong note and '!!' for wrong instrument
           --==>>> this bug should now be fixed!
      - The custom control break handler doesn't work properly with
        the DPMI version.
      - The DOSShell function doesn't work with SB replay (replay stops)
      - On exit, the DPMI version returns control to command.com instead
        of the default COMSPEC. This is really annoying (it prevents use in
        batch files for instance), I don't know why it happens though.


ўўBugfixes/changes/additions since version 1.0 (first public release):

      - Added custom control break handler, control break should exit
        the program properly now.
      - Channels 1 - 8 can now be muted by pressing '1' .. '8' respectively
      - Press 'i' to toggle Interpolation on SoundBlaster
      - Included "-D" command-line option (Shell to DOS) for the DPMI
        (protected Mode) version.


ўўBugfixes/changes/additions since version 1.1 (2nd public release):

      - modified slightly so it will display ok with 8 octaves
      - removed "-d" command-line option in the DPMI version. You can now
        shell to DOS at anytime by pressing "d", in the DPMI version of
        course.
      - Replaced usage of Borland Crt Unit with an own replacement in order
        to avoid the runtime error 200 problem on > 200MHz machines
      - Added SB Support for the DPMI version
      - Made general player appearance a bit nicer
      - changed key 'i' to Backspace (filters can now be cycled just like
        in Cubic Player):
         None / SB PRO LowPass / Linear interpolation / Cubic interpolation
      - Volume amplify can be modified during replay now using + and - :
        the value will turn red when clipping goes over 2.5% of the time
        (1 out of 40 samples has it's wave "cut off")
      - GUS driver is more stable due to removal of all interrupt chain code


ўўWork In Progress:

      - Make the DPMI version more stable (make DOS shell for SB work etc)
        (DOSSHELL for SB works in dosbox, not in win xp dos emu)

}
{$I-,N+,E-,R-}
Uses
  Shitty1,  { for the MIDAS-alike SETUP system                               }
  NewCrt,   { for some screen output                                         }
  Dos,      { for Control-Break Handler                                      }
  Replay,   { get the MOD replay routines                                    }
  MODType1, { Information about the MODFormat                                }
  ModLoad,  { the MOD loader                                                 }
  GUS_DRV1, { Choose the GUS & SB drivers among the 36 drivers available ;-) }
  SB_DRV1;

CONST
  NoteStrings: Array[0..8*12] of String[3] = ('ФФФ',
     'Cљ0','C#0','Dљ0','D#0','Eљ0','Fљ0','F#0','Gљ0','G#0','Aљ0','A#0','Bљ0',
     'Cљ1','C#1','Dљ1','D#1','Eљ1','Fљ1','F#1','Gљ1','G#1','Aљ1','A#1','Bљ1',
     'Cљ2','C#2','Dљ2','D#2','Eљ2','Fљ2','F#2','Gљ2','G#2','Aљ2','A#2','Bљ2',
     'Cљ3','C#3','Dљ3','D#3','Eљ3','Fљ3','F#3','Gљ3','G#3','Aљ3','A#3','Bљ3',
     'Cљ4','C#4','Dљ4','D#4','Eљ4','Fљ4','F#4','Gљ4','G#4','Aљ4','A#4','Bљ4',
     'Cљ5','C#5','Dљ5','D#5','Eљ5','Fљ5','F#5','Gљ5','G#5','Aљ5','A#5','Bљ5',
     'Cљ6','C#6','Dљ6','D#6','Eљ6','Fљ6','F#6','Gљ6','G#6','Aљ6','A#6','Bљ6',
     'Cљ7','C#7','Dљ7','D#7','Eљ7','Fљ7','F#7','Gљ7','G#7','Aљ7','A#7','Bљ7');

  FilterStrings: Array[0..3] of String[30] = (
    'No Filter           ',
    'SB PRO HW Low Pass  ',
    'Linr. Interpolation ',
    'Cubic Interpolation ');

{Error constants: -----------------------------------------------------------}
  NoArguments        = 0; { No command line options at all specified         }
  UnKnownOption      = 1; { Specified option does not exist                  }
  NoModule           = 2; { The module to be replayed is not specified       }
  WrongVolume        = 3; { The specified master volume has an invalid range }
  WrongVBoost        = 4; { The SB volume boost isn't specified as it should }
  WrongMixRate       = 5; { The specified mixrate has an invalid range       }
  WrongStartTrack    = 6; { The specified track to start at is invalid       }
  WrongFastDOverride = 7; { The soundcard/Base port specified does not exist }
  WrongGlobalPanning = 8; { The specified global panning has an invalid range}
  WrongNReduction    = 9; { The specified Noisereduction is invalid          }
  ToManyFilesToLoad  = 10;{ More than one .MOD file specified                }
  WrongAmplify       = 11;{ Wrong amplification specification                }

  OverrideDetection: Boolean = False;
  AmplifySpecified : Boolean = False;

{ Color Constants: ----------------------------------------------------------}
  WBoxBckGnd   = LightGray;
  WBoxFrGnd    = WBoxBckGnd+$8;
  WBckGnd      = WBoxBckGnd;
  WFrGnd       = White;
  WFrGndHL     = Yellow;

  Box1YDelta = 13;

  SB_Amp         : Word = 24;
  Filter         : Byte = LIFilter;
  GlobalPanning  : Byte = 48-1;


VAR
  Limit,
  Chn            : Byte;
  Wait           : Char;
  ModFileName    : String;
  OldBreakHandler: Procedure;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

Function DetectDevice: Boolean;
VAR
  Res1, Res2: Boolean;
BEGIN
  If GUS_DRV1.GUSDevice.SWDetected then PlayDevice:=GUS_DRV1.GUSDevice Else
  If SB_DRV1.SBDevice.SWDetected   then PlayDevice:=SB_DRV1.SBDevice   Else
  Begin
    PLAYDevice.ID:=NoDevice;
    PLAYDevice.SWDetected:=False;
  End;

  If OverrideDetection then
    Begin
      GiveDeviceInfo;
      If PLAYDevice.ID=NoDevice then Begin DetectDevice:=False; exit; End
      Else                           Begin DetectDevice:=True;  exit; End;
    End
  Else If WriteWav then SixteenbitMix:=True;

{Perform HardWare Detection: ------------------------------------------------
  Res1:=False; Res2:=False;
  Res1:=GUS_DRV1.HW_Detect;
  if Res1 then PLAYDevice:=GUSDevice;
  if Res2 then
  Else
   Begin
     Res2:=False;
     Res1:=SB_DRV1.HW_Detect;
     if Res1 then Res2:=SB_DRV1.HW_Check;
     if Res2 then PLAYDevice.ID:=SB100Device
     Else PLAYDevice.ID:=NODevice;
   End;
}
  PLAYDevice.SWDetected:=PLAYDevice.ID <> NoDevice;
  PLAYDevice.HWDetected:=PLAYDevice.SWDetected;
  DetectDevice:=PLAYDevice.SWDetected;
END;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}
{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

Procedure ResetTMode;
BEGIN
  TextMode(CO80+Font8x8);
  NormVideo; Window(1, 1, 80, 50); ClrScr;
END;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

Procedure SetExitScreen;
BEGIN
  ResetTMode;
  printLn('MOD Player '+PlayerVersion+' by BYTERAVER/TNT (The Next Temptation). (c) February 1998');
  printLn('Found bugs/want some more info? Read MPxx.DOC.');
  printLn('Last modifications in January 2009.');
  printLn('Have fun! Bye.');
  printLn('');
  printLn('Feel free to contact me for any reason at erlandvo@hotmail.com.');
  printLn('I will be pleased to help you!');
END;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

Procedure WrongArgs(error: Byte);
BEGIN
  ResetTMode;
  printLn('');
  If Error = NoArguments then
    Begin
{$IFDEF DPMI}
      printLn('TNT/NO-iD''s MOD player by ByteRaver. Protected Mode (DPMI) version.');
{$ELSE}
      printLn('TNT/NO-iD''s MOD player by ByteRaver. Real Mode version.');
{$ENDIF}
      printLn('Usage: MPxx  [OPTIONS]  <MUSICF[.MOD|.WOW]>  [OPTIONS]');
      printLn('Where OPTIONS can be: ');
      printLn('');
      printLn('(*)  -Mxx : Mixing Rate (in kHz). Valid values are: 8/9/10../42/43/44kHz.');
{      printLn('    -Txxx: Track to start at (in patterntable): 1..128');}
      printLn('     -Vxxx: Master Volume in % (Range: 1..100)');
      printLn('(*)  -Fx  : Use filter 1..3: SBPRO HW LP / Linear / Cubic Interpolation');
      printLn('(*)  -Axxx: Amplify. Min = 8 (divide by 2), Max = 255 (multiply by 16).');
      printLn('     -O   : Override SoundCard detection (default =  AutoDetect Device). ');
      printLn('     -B   : VBlank timing; use this for old MOD''s that play at the wrong speed.');
      printLn('(**) -Pxx : Set global panning. Same as in Cubic Player. Range: 0..15');
      printLn('     -W[:Fn]: Write to a WAV file (specify the name after ":")');
      printLn('');
      printLn('(*)   = Only affects SoundBlaster & compatible cards');
      printLn('(**)  = Only affects stereo soundcards');
      printLn('You MUST separate the options by a space & "-"');
      printLn('');
      printLn('Feel free to contact me for any reason at erlandvo@hotmail.com.');
      printLn('I will be pleased to help you!');
    End
  Else
    Begin
      Case error of
        UnKnownOption     : printLn('One of the options you specified doesn''t exist.');
        NoModule          : printLn('You forgot to specify a .MOD File to load.');
        WrongVolume       : printLn('The master volume you specified couldn''t be interpreted or it''s range is'+
                                            '        invalid.');
        WrongVBoost       : printLn('You must use "mp xx.mod -Vxx -v100" for correct `volume boost''');
        WrongMixRate      : printLn('The mixrate you specified couldn''t be interpreted or it''s range is invalid.');
        WrongStartTrack   : begin
                              printLn('The track to start at you specified couldn''t be interpreted or it''s range is ');
                              printLn('invalid.');
                            end;
        WrongFastDOverride: printLn('The soundcard/BasePort you specified couldn''t be interpreted or it''s range is'+
                                            '   invalid.');
        WrongGlobalPanning: printLn('The global panning you specified couldn''t be interpreted or it''s range is'+
                                            '       invalid.');
        ToManyFilesToLoad : begin
                              printLn('It seems you are trying to load TWO MOD''s at once!');
                              printLn('Remember to put a ''/'' or ''-'' before each option!');
                            end;
        WrongAmplify      : printLn('The Amplify you specified couldn''t be interpreted or it''s range is invalid.');
        else; printLn('Internal Error! Contact the author!');
      end;
    End;
  Halt;
END;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}
{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

Procedure GetOptions;
VAR
   I, I2: Byte;
   Code, temp: Integer;
   Mix: Word;
   Str2: String;
   Options: Array[1..32] of String;
   FileSpecified, GlobalVSpecified: Boolean;
BEGIN
  FileSpecified:=False;
  GlobalVSpecified:=False;
  If (Paramcount<1) then WrongArgs(NoArguments);

  For I:=1 to Paramcount do
  BEGIN
    Options[I]:=ParamStr(I);
    If (Options[I][1]='/') or (Options[I][1]='-') then
    begin
      Case Upcase(Options[I][2]) Of
        '?', 'H': WrongArgs(NoArguments);
        'O': OverrideDetection:=True;
        'B': Timing.VBlank:=True;
        'F': Begin
               Case Options[I][3] of
                 '1': Filter:=HWFilter;
                 '2': Filter:=LIFilter;
                 '3': Filter:=CIFilter;
                 else; Filter:=NoFilter;
               End;
             End;
        'V': begin
               If not GlobalVSpecified then
                 Begin
                   GlobalVSpecified:=True;
                   Str2:='';
                   for I2:=1 to Length(Options[I])-2 do Str2:=Str2+Options[I][I2+2];
                   val(Str2, GlobalVolume, code);
                   If (GlobalVolume>100) or (GlobalVolume<1) or (code<>0) then WrongArgs(WrongVolume);
                 end
               Else
{secret}         Begin
                   Str2:='';
                   for I2:=1 to Length(Options[I])-2 do Str2:=Str2+Options[I][I2+2];
                   val(Str2, temp, code);
                   If (temp=100) then SBVolumeBoost:=True
                   Else WrongArgs(WrongVBoost);
                 end
             end;
{
        'T': begin
               Str2:='';
               for I2:=1 to Length(Options[I])-2 do Str2:=Str2+Options[I][I2+2];
               val(Str2, StartTrack, code);
               If (code<>0) or (StartTrack>128) or (StartTrack<1) then WrongArgs(WrongStartTrack);
               Dec(StartTrack);
             end;
}
        'A': begin
               Str2:='';
               for I2:=1 to Length(Options[I])-2 do Str2:=Str2+Options[I][I2+2];
               val(Str2, SB_Amp, code);
               If (code<>0) or (SB_Amp<8) or (SB_Amp>255) then WrongArgs(WrongAmplify);
               Amplifyspecified:=True;
             end;
        'M': begin
               Str2:='';
               for I2:=1 to Length(Options[I])-2 do Str2:=Str2+Options[I][I2+2];
               val(Str2, Mix, code);
               If (code<>0) or (Mix>44) or (Mix<8) then WrongArgs(WrongMixRate);
               Case Mix Of
                 12: MixRate:=11905;
                 14: MixRate:=13889;
                 16: MixRate:=15873;
                 18: MixRate:=17857;
                 22: MixRate:=21739;
                 24: MixRate:=23810;
                 26: MixRate:=25641;
                 28: MixRate:=27778;
                 32: MixRate:=32258;
                 36: MixRate:=35714;
                 40: MixRate:=40000;
                 44: MixRate:=45454;
                 else MixRate:=Word(Mix)*1000;
               end;
             end;
        'P': begin
               Str2:='';
               for I2:=1 to Length(Options[I])-2 do Str2:=Str2+Options[I][I2+2];
               val(str2, GlobalPanning, code);
               if (code<>0) or (GlobalPanning>15) then WrongArgs(WrongGlobalPanning);
               GlobalPanning:=GlobalPanning*$10;
             end;
        'W': begin
               WAVFilename:='default.wav'; {temp! ***** eeeee}
               WriteWAV:=True;
               ModInfo.RepeatModule:=False;
             end;
        Else WrongArgs(UnKnownOption);
      end;
    End
    Else Begin
           If FileSpecified then WrongArgs(ToManyFilesToLoad); {file already specified?}
           ModFileName:=Options[I];
           FileSpecified:=True;
         End;
  END;
  If Not FileSpecified then WrongArgs(NoModule);
  If (MixRate<=23000) OR
     WriteWAV         then StereoReplay:=True;
END;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

Function Bool2BStr(Val: Boolean): String;
Begin
  If Val then Bool2BStr:='True' else Bool2BStr:='False';
End;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

Procedure DrawMain;
VAR
  BoxStyle: Boxtype;
Begin
  ResetTMode;
  FilledBoxes:=True;
  BoxStyle:=Box1;
  TextColor(WBoxFrGnd); TextBackground(WBoxBckGnd);
  HBox  (1, 1, MaxColums  , MaxRows        , BoxStyle);
  LBox  (2, 2, MaxColums-1, 2+Box1YDelta   , BoxStyle);
  LBox  (2, 2, MaxColums-1, 2+Box1YDelta   , BoxStyle);
  Window(3, 3, MaxColums-2, 2+Box1YDelta-1);
  LBox  (2, 3+Box1YDelta, MaxColums-1, MaxRows-1, BoxStyle);

  TextColor(WFrGnd); TextBackground(WBckGnd);
  println(ModInfo.ModFileName);
  println('');
  println('"'+ ModInfo.SongTitle + '"');
  println('Channels: ' + I2DStr(ModInfo.NrChannels));
  println('Samples : ' + I2DStr(ModInfo.NrSamples )
                 + '/' + I2DStr(ModInfo.MaxSamples));
  println('Patterns: ' + I2DStr(ModInfo.NrPatterns));
  println('Position: ' + '1/' + I2DStr(ModInfo.SongLength));
  println('Loop Pos: ' + I2DStr(ModInfo.RestartPosition));
  println('Mod Tag : "'+ ModInfo.Tag + '"');
  println('Panning (/|*) :');
  println('Amplify (-|+) :');
  print  ('Filter (BkSp) :');

  GotoXY(37, WhereY);
{$IFDEF DPMI}
  print('Hit <D> to shell to OS or <ESC> to exit.');
{$ELSE}
  print('Hit <ESC> to exit.');
{$ENDIF}
  If ModInfo.NrChannels < 8 then Limit:=ModInfo.NrChannels
  Else                           Limit:=8;

  Window(2+2, 2+Box1YDelta+2, MaxColums-2, MaxRows-2);
End;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

Procedure ShowPatternLine;
Begin
  TextBackGround(WBckGnd);
  TextColor(WFrGndHL);
  PrintXY(13, 9, I2DStr(PatternInfo.Track+1) + '/'+I2DStr(ModInfo.SongLength)+'      ');
  TextColor(WFrGnd); println('');
  If PatternInfo.line=0 then TextBackGround(Magenta) Else TextBackGround(WBckGnd);
  TextColor(LightRed); print(I2HStr(PatternInfo.line , 2));
  TextColor(WFrGnd);
{  print(I2HStr(PatternInfo.Track, 2) + '/'); print(I2HStr(ModInfo.SongLength-1, 2));}
  For chn:=1 to Limit do With PatternInfo.ActLine[Chn] do
    Begin
      If Channels[Chn].Muted Then TextBackground(Red)
      Else If PatternInfo.line=0 then TextBackGround(Magenta)
           Else TextBackGround(WBckGnd);
      TextColor(WFrGnd); print('н');
      If (Note>=0) and (Note<=96) then print(NoteStrings[Note])
      Else print('ERR');
{      If Note<>0 then Begin TextColor(White); print(NoteStrings[Note]); End
      Else print('ФФФ'); TextColor(WFrGnd);}
      If sample<>0 then
         If Sample<=ModInfo.MaxSamples then
            Begin TextColor(LightBlue); print(I2HStr(sample, 2)); End
         Else print('!!')
      Else print('ФФ');
      TextColor(WFrGnd);
      If not ((effect=0) and (effectarg=0)) then
        Begin
          TextColor(Cyan);
          If effect>=$E0 then
            Begin print(I2HStr(effect, 2)); print(I2HStr(effectArg, 1)); End
          Else
            Begin print(I2HStr(effect, 1)); print(I2HStr(effectArg, 2)); End;
        End
      Else print('ФФФ');
    End;
End;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

{$F+,S-}
Procedure BreakHandler; Interrupt;
BEGIN
  ResetTMode;
  If Playing then With PlayDevice do Begin StopReplay; CloseCard; End;
  SetIntVec($1B, @OldBreakHandler);
  OldBreakHandler; {call old control break handler}
  Halt;
END;
{$F-,S+}

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}
{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

VAR
  I           : Byte;
  SB_Detected,
  GUS_Detected: Boolean;
  Ans         : Char;
  SatCheck    : Word;

Begin
  GetIntVec($1B, @OldBreakHandler);   { Install Custom control-break Handler }
  SetIntVec($1B, @BreakHandler);

  TextMode(CO80+Font8x8); ClrScr; NormVideo;

  GetOptions;
  SetFilter(Filter);

  If not (DetectDevice or WriteWAV) Then
  Begin
    TextMode(CO80+Font8x8); NormVideo; ClrScr;
    printLn('You need a GUS, SB or compatible soundcard to run this program.');
    SetIntVec($1B, @OldBreakHandler);
    Halt;
  End;

  Filter:=GetFilter;

  TextMode(CO80+Font8x8); NormVideo; ClrScr;
  If (PLAYDevice.OpenCard(4)<>1) then
    Begin
      println('Sound Card initialisation failure... Exiting!');
      SetIntVec($1B, @OldBreakHandler);
      Halt;
    End;

  I:=LoadMod(ModFileName);

  If Not (I in [NoError, OutOfSampleData]) then
    Begin
      printLn(ErrStr(I));
      SetIntVec($1B, @OldBreakHandler);
      Halt;
    End;

  If I=OutOfSampleData Then
    Begin
      print  ('Module is corrupt; some sample data is missing, it might sound a bit strange! ;)');
      printLn('Hit any key to Continue.');
      ReadKey;
    End;

  If not AmplifySpecified then { this should do most of the time }
  Begin
    SB_Amp:=Word(MODInfo.NrChannels)*7;
    If Not StereoReplay then SB_Amp:=(SB_Amp*3) div 5;
    If SixteenBitMix then SB_Amp:=(SB_Amp*5) div 7; {use the extra resolution}
    If SB_Amp<20 then SB_Amp:=20; { for 1 - 3 channel modules ;) }
  End;

  PLAYDevice.OpenCard(ModInfo.NrChannels);
  If PlayDevice.ID = GUSDevice.ID then StereoReplay:=True
  Else SetAmplify(SB_Amp);
  DrawMain;
{ Do not uncomment; works only w/ GUS
  With PlayDevice do
  Begin
    VSetPanning(1, 128); VSetVolume(1, 64); VSetFrequency(1, 8363*2);
  End;
  For I:=1 to ModInfo.MaxSamples do
  If ModInfo.Samples[I].Length>2 then
    Begin
      write  ('Smp ', I:2);
      write  (': Len '   , ModInfo.Samples[I].Length      :5);
      write  ('; RepOfs ', ModInfo.Samples[I].RepeatOffset:5);
      write  ('; RepLen ', ModInfo.Samples[I].RepeatLength:5);
      write  ('; Fn '    , ModInfo.Samples[I].FineTune    :2);
      writeLn(' "'       , ModInfo.Samples[I].Name, '"');
      PlayDevice.VPlay(1, I, 0);
      ReadKey;
    End
  Else
    Begin
      PlayDevice.VSetMode(0, 3);
      writeLn('Smp ', I:2, ' is not used', '"':36, ModInfo.Samples[I].Name, '"');
    End;
  With PlayDevice do
          Begin VSetPanning(1, 7); VSetVolume(1, 0); VSetFrequency(1, 0); End;
{}
  SatCheck:=DefMBufLn div (Ord(StereoReplay) + 1 + Ord(SixteenbitMix) + 1);

  With PlayDevice do
  Begin
    If not WriteWAV then
    Begin
      GotoXY(1, 1);
      println('Hit any key to start replay...');
      ReadKey;
    End;

    SetGlobalPanning(Globalpanning);
    StartReplay;
    Wait:='?';

    If Not StereoReplay then TextColor(WFrGnd) Else TextColor(WFrGndHL);
    printXY(19, Box1YDelta-1, I2DStr(GlobalPanning)+'   ');

    If PlayDevice.ID<>GUSDevice.ID then TextColor(WFrGndHL)
    Else                                TextColor(WFrGnd);
    printXY(19, Box1YDelta  , I2DStr(SB_Amp       )+'   ');
    printXY(19, Box1YDelta+1, FilterStrings[Filter]);

    Repeat
      If KeyPressed then
      Begin
        Wait:=UpCase(ReadKey);
        Case Wait of
          '1'..'8':
               Channels[Ord(Wait)-48].Muted:=NOT Channels[Ord(Wait)-48].Muted;
          '/': If StereoReplay then
               Begin
                 If Globalpanning<(255-15) then Inc(Globalpanning, 16)
                 Else Globalpanning:=255;
                 TextBackGround(WBckGnd); TextColor(WFrGndHL);
                 printXY(19, Box1YDelta-1, I2DStr(GlobalPanning)+'   ');
                 SetGlobalPanning(Globalpanning);
               End;
          '*': If StereoReplay then
               Begin
                 If Globalpanning>15 then Dec(Globalpanning, 16)
                 Else Globalpanning:=0;
                 TextBackGround(WBckGnd); TextColor(WFrGndHL);
                 printXY(19, Box1YDelta-1, I2DStr(GlobalPanning)+'   ');
                 SetGlobalPanning(Globalpanning);
               End;
          '+': If PlayDevice.ID<>GUSDevice.ID then
               Begin
                 Inc(SB_Amp, MODInfo.NrChannels div 2);
                 If SB_Amp>255 then SB_Amp:=255;
                 SetAmplify(SB_Amp);
               End;
          '-': If PlayDevice.ID<>GUSDevice.ID then
               Begin
                 If SB_Amp>(MODInfo.NrChannels div 2 + 8) then
                                Dec(SB_Amp, MODInfo.NrChannels div 2)
                 Else SB_Amp:=8;
                 SetAmplify(SB_Amp);
               End;
          chr(8) { BACKSPACE }
             : If PlayDevice.ID<>GUSDevice.ID then
               Begin
                 Inc(Filter); If Filter>MxFilter then Filter:=NoFilter;
                 SetFilter(Filter);
                 TextBackGround(WBckGnd); TextColor(WFrGndHL);
                 printXY(19, Box1YDelta+1, FilterStrings[Filter]);
               End;
{$IFDEF DPMI}
          'D': Begin
                 ResetTMode;
                 printLn('Type "exit", <ENTER> to get back to MP.'); printLn('');
                 SwapVectors;
                 Exec(GetEnv('COMSPEC'), '');
                 SwapVectors;
                 If DosError <> 0 Then
                 Begin
                   printLn('Couldn''t shell to OS for some reason, sorry.');
                   printLn('DosError = '+ I2HStr(DosError, 2) + ', Hit any key to continue.');
                   ReadKey;
                 End;
                 DrawMain;
                 TextBackGround(WBckGnd); TextColor(WFrGndHL);
                 printXY(19, Box1YDelta-1, I2DStr(GlobalPanning)+'   ');
                 printXY(19, Box1YDelta, I2DStr((SB_Amp*100) div 16)+'%  ');
                 printXY(19, Box1YDelta+1, FilterStrings[Filter]);
               End;
{$ENDIF}
        End;
      End;
      If WaitState then
      Begin
        WaitState:=False;
        ShowPatternLine;
        TextBackGround(WBckGnd);
        If Saturation < (SatCheck div 40) then
             TextColor(WFrGndHL)
        Else TextColor(LightRed);
{        printXY(19, Box1YDelta, I2DStr(SB_Amp)+'  ');}
        printXY(19, Box1YDelta, I2DStr((SB_Amp*100) div 16)+'%  ');
      End;
    Until (Wait=chr(27)) or ModInfo.FinishedReplay; { 27 = ESC }
    StopReplay;
    CloseCard;
  End;
  SetExitScreen;
  SetIntVec($1B, @OldBreakHandler); { Restore old break handler }
End.
