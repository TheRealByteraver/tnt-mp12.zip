{
  These are the MOD replay routines.
  Protected Mode Ready
  Copyright (c) 1998 Erland Van Olmen (erlandvo@hotmail.com)

  Known Bugs:

      - Vibrato Fx is buggy :(
      - No tremolo fx
      - no glissando control (fx $E3)
      - no nested pattern loops (I'm not going to 'fix' that - sorry)


ўўBugfixes/changes/additions since version 1.0 (first public release):

      - Fx $9 past sample loop end jump bug (whole note is now ignored)
      - Fixed song repeat a bit
      - Fixed nested pattern loops
      - FX $E6 (pattern loop) and FX $D (pattern break) combination fixed.
        Loop has now precedence over pattern break when on the same row.
        !! Note that FT2 always breaks a pattern when effect $D occurs, no
        matter if a loop has been specified or not. Impulse tracker to the
        contrary works the same as MP.


ўўBugfixes/changes/additions since version 1.1 (2nd public release):

      - Fixed bug in patterndelay effect when a set tempo (set speed) effect
        occured in the same line in a higher channel.
      - Increased nr of octaves from 5 to 8, many thanks must go to
        Mr.H / Triton who was kind enough to supply me with formulas and
        quite some additional information.
        Note that because of FastTracker 2.08 saving routines, only 7 octaves
        are available in MOD files (the lowest octave can't be saved)
        This is actually a limitation of the .MOD format. Not FT2's fault ;)
      - Fixed major bug in pattern loop effect that did not work anymore
        because of another bugfix in version 1.1.
      - changed pattern-depack code to make it compatible with the new
        packing scheme introduced because of the extra octaves, it is now
        quite less memory efficient, but hey, use the dpmi version eh?
      - Fixed the Vibrato effect, finally (the waveform was retriggered each
        line instead of each new note), but it still doesn't sound right
      - Removed some redundant safety checks that were already done in the
        loader (hopefully ;))
      - Fixed Pattern delay + fine volume / portamento effects combination
      - Changed Pattern Break + Position Jump combination behaviour (it will
        now jump to the same pattern that it is playing)
}

{$I-,N+,E-}
Unit Replay;

interface

Procedure UpDateNotes;
Procedure UpdateMultipleStepsEffects;
Procedure ResetSong;
Procedure SetGlobalPanning(Soften: Byte);


{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}
{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

implementation

Uses
  MODType1, NewCrt;

Procedure UpDateTrackEffects; Forward;

Const
{Some Panning constants for the Stereo initialisations: ---------------------}
  DefaultGlobalPanning = $40;
  GlobalPanning: Byte  = DefaultGlobalPanning;
  LeftPan  = 0;
  RightPan = 255;
  InverseChannels: Boolean = False;           { If the channels are inversed }

VAR
  NewTempo   : Byte;                {        Needed for pattern delay effect }
  DepackCount: Word;                { Needed for real-time pattern depacking }
{VAR's for the replay procedures: -------------------------------------------}
  TrackInfo  : Record
                 Pattern,                            { Actual pattern        }
                 Line,                               { Actual Line           }
                 Track     : Byte;                   { Actual Track position }
               End;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}
{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

Procedure SetPtnOfs(Ptn, Line: Byte);
VAR
  Pack,
  NCnt: Byte;
BEGIN
  DepackCount:=0; TrackInfo.line:=0;
  If (Line = 0) or (Line>63) then exit;
  While (TrackInfo.line < Line) do
  begin
    For NCnt:=1 to ModInfo.NrChannels do
    Begin
      Pack:=Patterns[Ptn]^[DepackCount]; Inc(DepackCount);
      If (Pack AND $80)=0 then Inc(DepackCount, 3)
      Else
        Begin
          If (Pack AND $1)<>0 Then Inc(DepackCount);
          If (Pack AND $2)<>0 Then Inc(DepackCount);
          If (Pack AND $4)<>0 Then Inc(DepackCount);
          If (Pack AND $8)<>0 Then Inc(DepackCount);
        End;
    End;
    Inc(TrackInfo.Line);
  End;
END;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

Procedure ResetSong;
VAR
  Chn,
  I   : Byte;
BEGIN
  DepackCount:=0;
  With Timing do Begin Count:=0; Speed:=6; BPM:=125; End;
  PlayDevice.SetBPM;
  With TrackInfo do
           Begin Line:=0; Track:=0; Pattern:=ModInfo.PatternTable[Track]; End;

  For Chn:=1 to ModInfo.NrChannels do
    With Channels[Chn] do
    Begin
      Muted          :=False;
      Replay         :=False;
      Period         :=0;
      Instr          :=0;
      Note           :=0;
      Vol            :=0;
{Needed for the effect engine: ----------------------------------------------}
      Effect         :=0;
      EffectArg      :=0;
      LastEffect     :=0;
      LastEffectArg  :=0;
      SmpOffset      :=0;
      ArpeggioCount  :=0;
      arp1           :=0;
      arp2           :=0;
      DestPeriod     :=0;
      LastSlideRate  :=0;
      Glissando      :=False;
      VibratoCount   :=0;
      VibratoWave    :=0;
      LastVibrato    :=0;
      TremoloCount   :=0;
      TremoloWave    :=0;
      LastTremolo    :=0;
      DelayCount     :=0;
      RetrigCount    :=0;
    End;
{ Reset finetunes }
  For I:=1 to ModInfo.MaxSamples do with ModInfo.Samples[I] do
                                                        Finetune:=OldFinetune;
  For Chn:=0 to 7 do
  begin
    Channels[Chn shl 2+1].pan:=LeftPan;
    Channels[Chn shl 2+2].pan:=RightPan;
    Channels[Chn shl 2+3].pan:=RightPan;
    Channels[Chn shl 2+4].pan:=LeftPan;
  end;
END;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

Procedure SetGlobalPanning(Soften: Byte);
BEGIN
  If Soften>127 then InverseChannels:=True Else InverseChannels:=False;
  GlobalPanning:=Soften;
END;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

Procedure SetBalances;
VAR
  Chn,
  Temp,
  FPan: Byte;
BEGIN
  If InverseChannels then
    Begin
      Temp:=255-GlobalPanning;
      For Chn:=1 to ModInfo.NrChannels do
        Begin
          FPan:=(Channels[Chn].Pan*(255-2*Temp)) div 255+Temp;
          PlayDevice.VSetPanning(chn, 255-FPan);
        End;
    End
  Else
    Begin
      Temp:=GlobalPanning;
      For Chn:=1 to ModInfo.NrChannels do
        Begin
          FPan:=(Channels[Chn].Pan*(255-2*Temp)) div 255+Temp;
          PlayDevice.VSetPanning(chn, FPan);
        End;
    End;
END;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

Procedure RepeatSong;
VAR
  Chn: Byte;
BEGIN
  DepackCount:=0;
  If ModInfo.CustomRepeat then
    With TrackInfo do
    Begin
      Line   :=0;
      Track  :=ModInfo.RestartPosition;
      Pattern:=ModInfo.PatternTable[Track];
    End
  Else
    Begin
      With TrackInfo do
      Begin
        Line   :=0;
        Track  :=0;
        Pattern:=ModInfo.PatternTable[Track];
        With PlayDevice do For Chn:=1 to ModInfo.NrChannels do
                       Begin VSetFrequency(Chn, 0); VSetVolume(Chn, 0); End;
      End;

      With Timing do
      Begin
{        If Speed<=6 then Count:=6-Speed Else Inc(PatternDelay, Speed);}
        Speed:=6; BPM:=125;
      End;
      PlayDevice.SetBPM;

      For Chn:=1 to ModInfo.NrChannels do
      With Channels[Chn] do
      Begin
        Period         :=0;
        Instr          :=0;
        Note           :=0;
{        Vol            :=0;}
{Needed for the effect engine: ----------------------------------------------}
        Effect         :=0;
        EffectArg      :=0;
        LastEffect     :=0;
        LastEffectArg  :=0;
        SmpOffset      :=0;
        ArpeggioCount  :=0;
        arp1           :=0;
        arp2           :=0;
        DestPeriod     :=0;
        LastSlideRate  :=0;
        Glissando      :=False;
        VibratoCount   :=0;
        VibratoWave    :=0;
        LastVibrato    :=0;
        TremoloCount   :=0;
        TremoloWave    :=0;
        LastTremolo    :=0;
        DelayCount     :=0;
        RetrigCount    :=0;
      End;
{ Reset finetunes }
      For chn:=1 to ModInfo.MaxSamples do with ModInfo.Samples[chn] do
                                                        Finetune:=OldFinetune;
      For Chn:=0 to 7 do
      Begin
        Channels[Chn shl 2+1].pan:=LeftPan;
        Channels[Chn shl 2+2].pan:=RightPan;
        Channels[Chn shl 2+3].pan:=RightPan;
        Channels[Chn shl 2+4].pan:=LeftPan;
      End;
    End;
END;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

Function Note2Period(Note, Finetune: ShortInt): Word;
VAR
  i, j, k, l: LongInt;
BEGIN
  If Note>0 then Dec(Note);
  i:=Word(Note mod 12) shl 3 + ((Finetune + 128) shr 4);
  j:=(Byte(Finetune) and $F) shl 1;
  k:=Note div 12 + 2;
  l:=PeriodTable[8+i];
  Note2Period:=(l shl 5 + (PeriodTable[9+i] - l) * j) shr k;
END;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

Function Period2Freq(Period: Word): LongInt; {fn should be ok}
BEGIN
{ If Period<>0 then Period2Freq:=(8363*1712) div (Period)
  Else Period2Freq:=0; }
  If Period<>0 then
    Period2Freq:=7093789 div (Period shl 1)
{    Period2Freq:=Round((7093789.2 */2.12* ) / (Period shl 1))}
  Else Period2Freq:=0;
END;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

Procedure UpDateNotes;
VAR
   dummy    : Word;
   Pack,
   Chn      : Byte;
   HoldLine : Boolean;
   TempData : Record
                Note,
                Inst    : Byte;
                NewInst,
                NewNote,
                InstOk,
                NoteOk  : Boolean;
              End;
BEGIN
  NewTempo:=Timing.Speed;
  HoldLine:=False;
  With PatternInfo do
  Begin
    Line:=TrackInfo.line; Track:=TrackInfo.Track; Pattern:=TrackInfo.Pattern;
    PatternData:=Patterns[Pattern];
  End;

  For Chn:=1 to ModInfo.NrChannels do
  With Channels[Chn] do
  Begin
    UpdateLine:=True;
    With TempData do
      Begin NewInst:=False; NewNote:=False; InstOk:=False; NoteOk:=False; End;
{First depack note info: ----------------------------------------------------}
    LastEffect:=Effect; LastEffectArg:=EffectArg;
    Pack:=Patterns[TrackInfo.Pattern]^[DepackCount]; Inc(DepackCount);
    If (Pack AND $80)<>0 Then
      Begin
        Effect:=0; EffectArg:=0; TempData.Note:=0;
        If (Pack AND $1)<>0 Then
          Begin
            TempData.Note:=Patterns[TrackInfo.Pattern]^[DepackCount]; Inc(DepackCount);
            If (VibratoWave and 4) = 0 then VibratoCount:=0; {***}
          End;
        If (Pack AND $2)<>0 Then
          Begin
            TempData.Inst:=Patterns[TrackInfo.Pattern]^[DepackCount];
            Inc(DepackCount);
            If TempData.Inst<>Instr then
                      begin TempData.NewInst:=True; Instr:=TempData.Inst; end;
{validity checking is done in loader, removed it here: }
{            If (Instr>0) and (Instr<=ModInfo.MaxSamples) then}
                                                        TempData.InstOk:=True;
{            If not TempData.InstOk then TempData.NewInst:=False;}
            PatternInfo.ActLine[Chn].Sample:=Instr;
            SmpOffset:=0;
            Vol:=ModInfo.Samples[Instr].Volume;
          End
        Else
          Begin
            PatternInfo.ActLine[Chn].Sample:=0;
            TempData.Inst:=0;
{validity checking is done in loader, removed it here: }
{            If (Instr>0) and (Instr<=ModInfo.MaxSamples) then} TempData.InstOk:=True;
          End;
        If (Pack AND $4)<>0 Then
          Begin
            Effect:=Patterns[TrackInfo.Pattern]^[DepackCount]; Inc(DepackCount);
          End;
        If (Pack AND $8)<>0 Then
          Begin
            EffectArg:=Patterns[TrackInfo.Pattern]^[DepackCount]; Inc(DepackCount);
          End;
      End
    Else
      Begin
        TempData.Note:=Pack AND $7F;
        TempData.Inst:=Patterns[TrackInfo.Pattern]^[DepackCount]; Inc(DepackCount);
        Effect       :=Patterns[TrackInfo.Pattern]^[DepackCount]; Inc(DepackCount);
        EffectArg    :=Patterns[TrackInfo.Pattern]^[DepackCount]; Inc(DepackCount);

        If TempData.Inst<>Instr then TempData.NewInst:=True; Instr:=TempData.Inst;
{validity checking is done in loader, removed it here: }
{        If (Instr>0) and (Instr<=ModInfo.MaxSamples) then }TempData.InstOk:=True;
{        If not TempData.InstOk then TempData.NewInst:=False;}
        PatternInfo.ActLine[Chn].Sample:=Instr;
        SmpOffset:=0;
        Vol:=ModInfo.Samples[Instr].Volume;
      End;
    If (LastEffect=$10) and (Effect<>$10) then Period:=ArpeggioPeriod;
{Make pattern data available to the main program: ---------------------------}
    PatternInfo.ActLine[Chn].Note:=TempData.Note;
    If (Effect shr 4)=$E then
      Begin
        PatternInfo.ActLine[Chn].Effect:=Effect shr 4;
        PatternInfo.ActLine[Chn].EffectArg:=Effect shl 4 + EffectArg;
      End
    Else
      Begin
        PatternInfo.ActLine[Chn].Effect:=Effect;
        PatternInfo.ActLine[Chn].EffectArg:=EffectArg;
      End;

    If Effect=$ED then DelayFlag:=True Else DelayFlag:=False;

    If (TempData.Note>=1) and (TempData.Note<=8*12) then
      begin
        TempData.NewNote:=True; TempData.NoteOk:=True;
{for the $3 & $5 effects: ---------------------------------------------------}
        DestPeriod:=Note2Period(TempData.Note, ModInfo.Samples[Instr].FineTune);

        If (Effect<>$3) and (Effect<>$5) then
                           begin Note:=TempData.Note; Period:=DestPeriod; end;
      end
    Else If (Note>=1) and (Note<=8*12) then TempData.NoteOk:=True;

    Replay:=False;
    If TempData.NewNote and TempData.NoteOk then
    Begin
      If TempData.InstOk then Replay:=True;
      if ((Effect=$3) or (Effect=$5)) and (not TempData.NewInst) then Replay:=False;
    End;

{UPDATE EFFECTS (IMMEDIATE EFFECTS) -----------------------------------------}
    Case Effect of
{ARPEGGIO: ------------------------------------------------------------------}
      $10: If EffectArg<>0 then
          begin
            Arp1:=EffectArg shr 4; Arp2:=EffectArg and $F; ArpeggioCount:=0;
            If LastEffect<>$10 then ArpeggioPeriod:=Period;
          end;
{TONE PORTAMENTO: -----------------------------------------------------------}
      $3: If EffectArg<>0 then LastSlideRate:=EffectArg;
{VIBRATO: -------------------------------------------------------------------}
      $4: Begin
            If (EffectArg and $F ) = 0 then EffectArg:=EffectArg or (LastVibrato and $F);
            If (EffectArg and $F0) = 0 then EffectArg:=EffectArg or (LastVibrato and $F0);
            LastVibrato:=EffectArg;
          End;
{TREMOLO: -------------------------------------------------------------------}
      $7: Begin
            If (EffectArg and $F ) = 0 then EffectArg:=EffectArg or (LastTremolo and $F);
            If (EffectArg and $F0) = 0 then EffectArg:=EffectArg or (LastTremolo and $F0);
            LastTremolo:=EffectArg;
          End;
{SET FINE PANNING: ----------------------------------------------------------}
      $8: Pan:=EffectArg;
{SET SAMPLE OFFSET: ( No checking, is done in loader! ) ---------------------}
      $9: If (Word(EffectArg) shl 8)<ModInfo.Samples[Instr].Length then
                                             SmpOffset:=Word(EffectArg) shl 8
          Else Replay:=False;
{POSITION JUMP: -------------------------------------------------------------}
      $B,
{PATTERN BREAK: -------------------------------------------------------------}
      $D: UpDateLine:=False;
{SET VOLUME: ----------------------------------------------------------------}
      $C: Vol:=EffectArg; { No checking, is done in loader! }
{SET SPEED/BPM: -------------------------------------------------------------}
      $F: If Timing.VBlank then NewTempo:=EffectArg
          Else If EffectArg<$20 then NewTempo:=EffectArg;

{EXTENDED EFFECTS: ==========================================================}
{FINE SLIDE UP: -------------------------------------------------------------}
      $E1: If Period>EffectArg+MinPeriod then Dec(Period, EffectArg)
           Else Period:=MinPeriod;
{FINE SLIDE DOWN: -----------------------------------------------------------}
      $E2: If Period+EffectArg<MaxPeriod then Inc(Period, EffectArg)
           Else Period:=MaxPeriod;
{SET GLISSANDO CONTROL: -----------------------------------------------------
      $E3: Glissando  :=Boolean(EffectArg);
{SET VIBRATO WAVEFORM: ------------------------------------------------------}
      $E4: VibratoWave:=EffectArg and $0F; { and $0F ??? }
{SET FINETUNE: --------------------------------------------------------------}
      $E5: begin
{             ModInfo.Samples[Instr].FineTune:=ShortInt(EffectArg shl 4);  {?}
             Period:=Note2Period(Note, ShortInt(EffectArg shl 4));
           end;
{SET TREMOLO WAVEFORM: ------------------------------------------------------}
      $E7: TremoloWave:=EffectArg;
{SET ROUGH PANNING: ---------------------------------------------------------}
      $E8: Pan:=EffectArg shl 4;
{SAMPLE RETRIG: -------------------------------------------------------------}
      $E9: If EffectArg>0 then RetrigCount:=EffectArg Else Effect:=0;
{FINE VOLUME SLIDE UP: ------------------------------------------------------}
      $EA: Begin Inc(Vol, EffectArg); If Vol>64 then Vol:=64; end;
{FINE VOLUME SLIDE DOWN: ----------------------------------------------------}
      $EB: If Vol>=EffectArg then dec(Vol, EffectArg) Else Vol:=0;
{SAMPLE CUT: ----------------------------------------------------------------}
      $EC: If EffectArg = 0 then vol:=0;
{SAMPLE DELAY: --------------------------------------------------------------}
      $ED: DelayCount:=EffectArg;
    end;

    With PlayDevice do
    Begin
      If Not DelayFlag then
      Begin
        If (Effect<>$3) and (Effect<>$5) and (Period<>0) then
                                      VSetFrequency(Chn, Period2Freq(Period));
        If Replay then VPlay(Chn, Instr, SmpOffset);
        If Muted  then VSetVolume(Chn, 0) Else VSetVolume(Chn, Vol);
      End;
    End;
    SmpOffset:=0;
  End;

  SetBalances;
  UpDateTrackEffects;

  For chn:=1 to ModInfo.NrChannels do
   If Not Channels[Chn].UpdateLine then HoldLine:=True;
{UpDate Track Position: -----------------------------------------------------}
  If Not HoldLine then With TrackInfo do
    Begin
      If Line>=63 then
        Begin
          For chn:=1 to ModInfo.NrChannels do Channels[Chn].LoopStartLine:=0;
          If Track>=ModInfo.SongLength-1 then
            Begin
              If ModInfo.RepeatModule then RepeatSong
              Else ModInfo.FinishedReplay:=True;
            End
          Else Inc(Track);
          DepackCount:=0; Line:=0; Pattern:=ModInfo.PatternTable[Track];
        End
      Else Inc(Line);
    End;
  WaitState:=True;
END;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

Procedure UpDateTrackEffects;
VAR
  Temp,
  Chn             : Byte;
  HoldLine,
  PtnBreak_Occured: Boolean;
BEGIN
  PtnBreak_Occured:=False; HoldLine:=False;
  For Chn:=1 to ModInfo.NrChannels do
                                 If Channels[Chn].Looping then HoldLine:=True;
  For Chn:=1 to ModInfo.NrChannels do
  With Channels[Chn] do
  Begin
    Case Effect of
{POSITION JUMP: -------------------------------------------------------------}
      $B: begin
            PtnBreak_Occured:=true;
            If (EffectArg<=ModInfo.SongLength) and (TrackInfo.Track<=EffectArg) then
              begin
                TrackInfo.Track  :=EffectArg;
                TrackInfo.Pattern:=ModInfo.PatternTable[TrackInfo.Track];
                TrackInfo.Line   :=0;
                DepackCount:=0;
              end
            else begin
                   If ModInfo.RepeatModule then { RepeatSong }
                     Begin
                       TrackInfo.Track  :=EffectArg;
                       TrackInfo.Pattern:=ModInfo.PatternTable[TrackInfo.Track];
                       TrackInfo.Line   :=0;
                       DepackCount:=0;
                     End
                   Else ModInfo.FinishedReplay:=True;
                 end;
          end;
{PATTERN BREAK: -------------------------------------------------------------}
      $D: If {(Not PtnBreak_Occured) And} (Not HoldLine) then
          begin
            If TrackInfo.Track<ModInfo.SongLength-1 then
              begin
                If (Not PtnBreak_Occured) then
                begin
                  Inc(TrackInfo.Track);
                  TrackInfo.Pattern:=ModInfo.PatternTable[TrackInfo.Track];
                end;
                PtnBreak_Occured:=true;
                DepackCount:=0;
                SetPtnOfs(TrackInfo.pattern, (EffectArg shr 4) * 10 + EffectArg AND $F);
              end
            Else
              Begin
                If ModInfo.RepeatModule then
                  begin
                    RepeatSong;           { Next line might not be right ;-) }
                    SetPtnOfs(TrackInfo.pattern, 0{(EffectArg shr 4) * 10 + EffectArg AND $F});
                  end
                Else ModInfo.FinishedReplay:=True;
              End;
          end;
{PATTERN LOOP: --------------------------------------------------------------}
      $E6: If EffectArg = 0 then
             begin If not Looping then LoopStartLine:=TrackInfo.line end
           Else
             begin
               If Looping then
                 begin
                   If LoopCounter = 0 then
                               Begin Looping:=False; LoopStartLine:=255; end
                   Else
                     begin
                       UpdateLine:=false; Dec(LoopCounter);
                       SetPtnOfs(TrackInfo.Pattern, LoopStartLine);
                     end;
                 end
               Else
                 If LoopStartLine<>255 then
                 begin
                   UpDateLine:=False; Looping:=true;
                   LoopCounter:=EffectArg-1;
                   SetPtnOfs(TrackInfo.Pattern, LoopStartLine);
                 end;
             end;
{PATTERN DELAY: -------------------------------------------------------------}
      $EE: If EffectArg>0 then Timing.PatternDelay:=Word(EffectArg+1)*NewTempo;
{SET SPEED / SET BPM: -------------------------------------------------------}
      $F: If EffectArg<$20 then
               begin If EffectArg>0 then Timing.Speed:=EffectArg; end
          Else If Timing.VBlank then Timing.Speed:=EffectArg
               Else begin Timing.Bpm:=EffectArg; PlayDevice.SetBPM; end;
    end;
  end;
END;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

Procedure UpdateMultipleStepsEffects;
VAR
  Chn,
  dummy     : Byte;
  Diff1,             { for tone portamento, if glissando = true }
  Diff2,
  TempPeriod: Word;
  s, p, a, d: LongInt;

BEGIN
{PATTERN DELAY: -------------------------------------------------------------}
  If Timing.PatternDelay>1 then
  Begin
    If (Timing.PatternDelay mod Timing.Speed) = 0 then
      For Chn:=1 to ModInfo.NrChannels do
      With Channels[Chn] do
      Begin
        Case Effect of
{EXTENDED EFFECTS: ==========================================================}
{FINE SLIDE UP: -------------------------------------------------------------}
          $E1: If Period>EffectArg+MinPeriod then Dec(Period, EffectArg)
               Else Period:=MinPeriod;
{FINE SLIDE DOWN: -----------------------------------------------------------}
          $E2: If Period+EffectArg<MaxPeriod then Inc(Period, EffectArg)
               Else Period:=MaxPeriod;
{SAMPLE RETRIG: -------------------------------------------------------------}
          $E9: If EffectArg>0 then RetrigCount:=EffectArg Else Effect:=0;
{FINE VOLUME SLIDE UP: ------------------------------------------------------}
          $EA: Begin Inc(Vol, EffectArg); If Vol>64 then Vol:=64; end;
{FINE VOLUME SLIDE DOWN: ----------------------------------------------------}
          $EB: If Vol>=EffectArg then dec(Vol, EffectArg) Else Vol:=0;
        End;
      End;
    Dec(Timing.PatternDelay);
  End
  Else Timing.PatternDelay:=0;

  For Chn:=1 to ModInfo.NrChannels do
  With Channels[Chn] do
  Begin
    Case Effect of
{VOLUME SLIDE: --------------------------------------------------------------}
      $5,
      $6,
      $A: begin
            dummy:=EffectArg shr 4;
            If dummy<>0 then
              Begin
                Inc(Vol, dummy);
                If Vol>64 then Vol:=64;
              end
            Else
              begin
                dummy:=EffectArg and $F;
                If Vol>dummy then Dec(Vol, dummy)
                Else Vol:=0;
              end;
          end;
    end;

    Case Effect of
{ARPEGGIO: ------------------------------------------------------------------}
     $10: begin
            case (ArpeggioCount mod 3) of

              0: Period:=Note2Period(Note     , ModInfo.Samples[Instr].FineTune);
              1: Period:=Note2Period(Note+arp1, ModInfo.Samples[Instr].FineTune);
              2: Period:=Note2Period(Note+arp2, ModInfo.Samples[Instr].FineTune);
            end;
            Inc(ArpeggioCount);
          end;
{PITCH SLIDE UP: ------------------------------------------------------------}
      $1: begin
            If Period>EffectArg then Dec(Period, EffectArg)
            else Period:=MinPeriod;
            If Period<MinPeriod then Period:=MinPeriod;
          end;
{PITCH SLIDE DOWN: ----------------------------------------------------------}
      $2: begin
            Inc(Period, EffectArg);
            If Period>MaxPeriod then Period:=MaxPeriod;
          end;
{TONE PORTAMENTO: -----------------------------------------------------------}
      $3,
      $5: If Glissando then
            Begin
              If Period<DestPeriod then
                Begin
                  Inc(Period, LastSlideRate);
                  If Period>DestPeriod then Period:=DestPeriod;
                  TempPeriod:=Period;
                  If TempPeriod<>DestPeriod then
                  Begin
                    Dummy:=1;
                    While (Period<=
                      Note2Period(Dummy, ModInfo.Samples[Instr].FineTune))
                                                 and (Dummy<8*12) do Inc(Dummy);
                    If Dummy>1 then
                         Diff1:=Period-Note2Period(Dummy-1, ModInfo.Samples[Instr].FineTune)
                    Else Diff1:=Period-MinPeriod;
                    Diff2:=Note2Period(Dummy, ModInfo.Samples[Instr].FineTune) - Period;
                    If Diff2>Diff1 then TempPeriod:=Period-Diff1
                    Else                TempPeriod:=Period+Diff2;
                  End;
                end
              Else If Period>DestPeriod then
                begin
                  If Period<LastSlideRate then Period:=DestPeriod
                  Else Dec(Period, LastSlideRate);
                  If Period<DestPeriod then Period:=DestPeriod;
                  TempPeriod:=Period;
                  If TempPeriod<>DestPeriod then
                  Begin
                    Dummy:=1;
                    While (Period<=Note2Period(Dummy, ModInfo.Samples[Instr].FineTune))
                           and (Dummy<8*12) do Inc(Dummy);
                    If Dummy>1 then
                         Diff1:=Period-Note2Period(Dummy-1, ModInfo.Samples[Instr].FineTune)
                    Else Diff1:=Period-MinPeriod;
                    Diff2:=Note2Period(Dummy, ModInfo.Samples[Instr].FineTune) - Period;
                    If Diff2>Diff1 then TempPeriod:=Period+Diff2
                    Else                TempPeriod:=Period-Diff1;
                  End;
                end;

              PlayDevice.VsetFrequency(chn, Period2Freq(TempPeriod));
            End
          Else
            Begin
              If Period<DestPeriod then
                Begin
                  Inc(Period, LastSlideRate);
                  If Period>DestPeriod then Period:=DestPeriod;
                end
              Else If Period>DestPeriod then
                begin
                  If Period<LastSlideRate then Period:=DestPeriod
                  Else Dec(Period, LastSlideRate);
                  If Period<DestPeriod then Period:=DestPeriod;
                end;
              PlayDevice.VsetFrequency(chn, Period2Freq(Period));
            End;
{VIBRATO: -------------------------------------------------------------------
Vibrato means to "oscillate the sample pitch using a  particular waveform
with amplitude yyyy (notes-no!), such that (xxxx * speed)/64  full
oscillations occur in the line".
1 x 8 / 64 = 1 oscillatie
8 lijnen = 8 x 8 / 64 = voor 1 oscillatie
1 oscillatie = 256 ticks

8 x 8 / 64 = 1  effArg 8 speed 8 = 1 oscillatie / lijn
           = 256 progress / tick = 256 / 8 = 32


}
      $4,
      $6: Begin { scale of vibratotable = 256 }
            VibratoCount:=VibratoCount and $FF;
            s:=LongInt(VibratoTable[VibratoWave, VibratoCount]);
            Dummy:=(LastVibrato and $F);
            s:=(s * LongInt(Dummy)) div 128;     { signed divide! -128..+127 }
            TempPeriod:=LongInt(Period)+s;
            if TempPeriod>MaxPeriod then TempPeriod:=MaxPeriod;
            if TempPeriod<MinPeriod then TempPeriod:=MinPeriod;
            PlayDevice.VSetFrequency(chn, Period2Freq(TempPeriod));
            Inc(VibratoCount, (LastVibrato shr 4) shl 2);
          End;

{EXTENDED EFFECTS: ==========================================================}
{RETRIG SAMPLE: -------------------------------------------------------------}
     $E9: begin
            Inc(RetrigCount);
            If (RetrigCount mod EffectArg) = 0 then PlayDevice.VPlay(Chn, instr, 0);
          End;
{SAMPLE CUT: ----------------------------------------------------------------}
     $EC: If Timing.Count>EffectArg then vol:=0;
{SAMPLE DELAY: --------------------------------------------------------------}
     $ED: begin
            If DelayCount=Timing.Count then
            Begin
              DelayFlag:=False;
              If Replay then PlayDevice.VPlay(Chn, Instr, 0);
            End;
          end;
    End;

    With PlayDevice do
    Begin
      If Not DelayFlag then
      Begin
        If (Effect<>$3) and (Effect<>$5) and
           (Effect<>$4) and (Effect<>$6) and (Period<>0) then
                                      VSetFrequency(Chn, Period2Freq(Period));
        If not Muted then VSetVolume(Chn, Vol)
        Else              VSetVolume(Chn, 0);
      End;
    End;
  end;
  SetBalances;
END;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}
{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

BEGIN
  ModInfo.RepeatModule  :=True;
  ModInfo.FinishedReplay:=False;
  DepackCount:=0;
END.
