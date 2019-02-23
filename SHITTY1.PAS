{
  Copyright (c) 1998 Erland Van Olmen (erlandvo@hotmail.com)
  Protected Mode Ready
}
{$I-,N+,E-,R-}
Unit Shitty1;

interface

Procedure GiveDeviceInfo;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}
{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

implementation


Uses
  Dos,       { for register stuff }
  NewCrt,    { some output        }
  Shitty2,   { extra output stuff }
  MODType1,  { some info          }
  GUS_DRV1,  { GUS Driver         }
  SB_DRV1;   { SB  driver         }

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}
{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

Procedure GiveDeviceInfo;
Const
     MaxX   = 80;
     MaxY   = 25;
     Width  = 65;
     Height = 17;
     BGC    = Blue;
VAR
   Menu: MenuType;
   DetailedOptions: Boolean;

{лллллллллллллллллллллллллллллллллллллллл}

Procedure SoundCard_Menu;
VAR
  I: Byte;
BEGIN
  TextMode(CO80); TextBackGround(BGC); ClrScr; SetCursor(NoCursor);
  InitMenu(Menu);
  Menu.X1:=(MaxX-Width ) shr 1 + 1;
  Menu.Y1:=(MaxY-Height) shr 1 + 1;
  Menu.X2:=(MaxX+Width ) shr 1 + 1;
  Menu.Y2:=(MaxY+Height) shr 1;

  With Menu do
  Begin
    Case PLAYDevice.ID of
      MODType1.NoDevice   : DefaultT:=5;
      MODType1.GUSDevice  : DefaultT:=0;
      MODType1.SB100Device: DefaultT:=1;
      MODType1.SB200Device: DefaultT:=2;
      MODType1.SB201Device: DefaultT:=3;
      MODType1.SBPRODevice: DefaultT:=4;
      MODType1.SB16Device : DefaultT:=1;
    End;

    If WriteWav then DefaultT:=5;

    BarPos:=DefaultT;
    Title:='Select Sound Card';
    Topics[0]:='GRAVIS UltraSound (GF1 Synth)';           TAvail[0]:=True;
    Topics[1]:='SoundBlaster 1.xx or clone (DSP v1.xx )'; TAvail[1]:=True;
    Topics[2]:='SoundBlaster 1.5           (DSP v2.00 )'; TAvail[2]:=True;
    Topics[3]:='SoundBlaster 2.0           (DSP v2.01+)'; TAvail[3]:=True;
    Topics[4]:='SoundBlaster PRO           (DSP v3.xx )'; TAvail[4]:=True;
    Topics[5]:='Mod-to-WAV file writer                 '; TAvail[5]:=True;
{    Topics[6]:='SoundBlaster 16            (DSP v4.xx )'; TAvail[6]:=True;}
{    Topics[7]:='SoundBlaster AWE 32 +      (DSP v?.?? )'; TAvail[7]:=True;}
  End;

  DrawNiceBox(Menu, BGC); DrawMenu(Menu);
  TextBackGround(BGC); TextColor(White); Gotoxy(1, 2);
  println('             MP '+PlayerVersion+' ModPlayer, (c) 1997 - 2009 by ByteRaver/TNT.');
  println('                              SoundCard Setup:');
  Gotoxy(12, MaxY-2);
  println('Use arrows to select, Enter to confirm or Esc to cancel');
END;

{лллллллллллллллллллллллллллллллллллллллл}

Procedure BasePort_Menu;
VAR
  I: Byte;
BEGIN
  If PlayDevice.ID=MODType1.WAVDevice then exit;
  InitMenu(Menu);
  With Menu do
  Begin
    If PlayDevice.ID=NoDevice then DefaultT:=1
    Else DefaultT:=((PlayDevice.MPort-$200) shr 4)-1;
    BarPos:=DefaultT;
    Title:='Select Sound Card I/O Port Address';
    Topics[0]:='210h'; TAvail[0]:=True;
    Topics[1]:='220h'; TAvail[1]:=True;
    Topics[2]:='230h'; TAvail[2]:=True;
    Topics[3]:='240h'; TAvail[3]:=True;
    Topics[4]:='250h'; TAvail[4]:=True;
    Topics[5]:='260h'; TAvail[5]:=True;
    Topics[6]:='270h'; TAvail[6]:=True;
    Topics[7]:='280h'; TAvail[7]:=True;
  End;
  DrawMenu(Menu);
END;

{лллллллллллллллллллллллллллллллллллллллл}

Procedure IRQ_Menu;
VAR
  I, IRQ: Byte;
BEGIN
  Case PLAYDevice.ID of
    MODType1.WAVDevice  : exit;
    MODType1.SB100Device,
    MODType1.SB200Device,
    MODType1.SB201Device,
    MODType1.SBPRODevice,
    MODType1.SB16Device : IRQ:=SB_DRV1.SBDevice.MIRQ;
    MODType1.GUSDevice  : IRQ:=GUS_DRV1.GUSDevice.MIRQ;
   Else; IRQ:=5;
  End;

  InitMenu(Menu);

  With Menu do
  Begin
    Case IRQ of
      2 : DefaultT:=0;
      3 : DefaultT:=1;
      4 : DefaultT:=2;
      5 : DefaultT:=3;
      7 : DefaultT:=4;
      9 : DefaultT:=5;
      10: DefaultT:=6;
      11: DefaultT:=7;
      12: DefaultT:=8;
      15: DefaultT:=9;
    End;

    BarPos:=DefaultT;
    Title:='Select Sound Card IRQ Number';
    Topics[0]:='IRQ  2'; TAvail[0]:=True;
    Topics[1]:='IRQ  3'; TAvail[1]:=True;
    Topics[2]:='IRQ  4'; TAvail[2]:=True;
    Topics[3]:='IRQ  5'; TAvail[3]:=True;
    Topics[4]:='IRQ  7'; TAvail[4]:=True;
{Temp. disabled: }
    Topics[5]:='IRQ  9';
    Topics[6]:='IRQ 10';
    Topics[7]:='IRQ 11';
    Topics[8]:='IRQ 12';
    Topics[9]:='IRQ 15';
  End;
  DrawMenu(Menu);
END;

{лллллллллллллллллллллллллллллллллллллллл}

Procedure DMA_Menu;
VAR
  I: Byte;
BEGIN
  InitMenu(Menu);
  With Menu do
  Begin
    Case PLAYDevice.ID of
      MODType1.SB100Device,
      MODType1.SB200Device,
      MODType1.SB201Device,
      MODType1.SBPRODevice,
      MODType1.SB16Device : DefaultT:=SB_DRV1.SBDevice.MDMA;
      MODType1.GUSDevice  : DefaultT:=GUS_DRV1.GUSDevice.MDMA;
     Else; DefaultT:=1;
    End;

  BarPos:=DefaultT;
  Title:='Select Sound Card DMA Channel Number';
  Topics[0]:='0 ( 8-bit)'; {TAvail[0]:=True;}
  Topics[1]:='1 ( 8-bit)'; TAvail[1]:=True;
  Topics[2]:='2 ( 8-bit)'; {TAvail[2]:=True;}
  Topics[3]:='3 ( 8-bit)'; {TAvail[3]:=True;}
{Temp. disabled: }
  Topics[4]:='4 (16-bit)';
  Topics[5]:='5 (16-bit)';
  Topics[6]:='6 (16-bit)';
  Topics[7]:='7 (16-bit)';
{ // 16-bit DMA not supported yet!
    If PLAYDevice.ID >= SB16Device then For I:=4 to 7 do TAvail[I]:=True;}
{ DMA channel 1 is hardcoded right now }
  End;
  DrawMenu(Menu);
END;

{лллллллллллллллллллллллллллллллллллллллл}

Procedure MixRate_Menu;
VAR
  I: Byte;
BEGIN
  InitMenu(Menu);
  With Menu do
  Begin
    If MixRate>43000 then DefaultT:=9 Else
    If MixRate>39000 then DefaultT:=8 Else
    If MixRate>35000 then DefaultT:=7 Else
    If MixRate>31000 then DefaultT:=6 Else
    If MixRate>27000 then DefaultT:=5 Else
    If MixRate>23000 then DefaultT:=4 Else
    If MixRate>21000 then DefaultT:=3 Else
    If MixRate>17000 then DefaultT:=2 Else
    If MixRate>13000 then DefaultT:=1 Else DefaultT:=0;
    If (PLAYDevice.ID <= MODType1.SB200Device) AND (MixRate>22050) then DefaultT:=3;

    BarPos:=DefaultT;
    Title:='Select Mixing Rate';
    Topics[0]:='10000Hz'; TAvail[0]:=True;
    Topics[1]:='14000Hz'; TAvail[1]:=True;
    Topics[2]:='18000Hz'; TAvail[2]:=True;
    Topics[3]:='22000Hz'; TAvail[3]:=True;
    Topics[4]:='24000Hz';
    Topics[5]:='28000Hz';
    Topics[6]:='32000Hz';
    Topics[7]:='36000Hz';
    Topics[8]:='40000Hz';
    Topics[9]:='44000Hz';
    If (PLAYDevice.ID > MODType1.SB200Device) then For I:=4 to 9 do TAvail[I]:=True;
  End;
  DrawMenu(Menu);
END;

{лллллллллллллллллллллллллллллллллллллллл}

Procedure DetailedOptions_Menu;
VAR
  I: Byte;
BEGIN
  InitMenu(Menu);
  With Menu do
  Begin
    DefaultT:=1; {temporary}
    BarPos:=DefaultT;
    Title:='Detailed Options?';
    Topics[0]:='Go playing the music right now'; TAvail[0]:=True;
    Topics[1]:='Go on for detailed options'; TAvail[1]:=True;
  End;
  DrawMenu(Menu);
END;

{лллллллллллллллллллллллллллллллллллллллл}

Procedure Volume_Menu;
VAR
  I: Byte;
BEGIN
  InitMenu(Menu);
  With Menu do
  Begin
    DefaultT:=4; {temporary}
    BarPos:=DefaultT;
    Title:='Select Output Volume';
    Topics[0]:=' 20%'; TAvail[0]:=True;
    Topics[1]:=' 40%'; TAvail[1]:=True;
    Topics[2]:=' 60%'; TAvail[2]:=True;
    Topics[3]:=' 80%'; TAvail[3]:=True;
    Topics[4]:='100%'; TAvail[4]:=True;
    Topics[5]:='120% (amplify)';
    Topics[6]:='140%';
    Topics[7]:='160% (amplify, SB Volume Boost)';
    If (PLAYDevice.ID=MODType1.SB100Device) or (PLAYDevice.ID=MODType1.SB200Device) or
       (PLAYDevice.ID=MODType1.SB201Device) or (PLAYDevice.ID=MODType1.SBPRODevice) or
       (PLAYDevice.ID=MODType1.SB16Device)  then TAvail[7]:=True;
  End;
  DrawMenu(Menu);
END;

{лллллллллллллллллллллллллллллллллллллллл}

Procedure SBModeMenu_Menu;
VAR
  I: Byte;
BEGIN
  InitMenu(Menu);
  With Menu do
  Begin
    Case PlayDevice.ID of
      MODType1.GUSDevice  : DefaultT:=5;
      MODType1.SB100Device,
      MODType1.SB200Device,
      MODType1.SB201Device: DefaultT:=3;
      MODType1.SBPRODevice: If MixRate<22050 then DefaultT:=8
                            Else                  DefaultT:=3;
      Else; DefaultT:=0;
    End;
    If WriteWav then DefaultT:=9;

    BarPos:=DefaultT;
    Title:='Select SB Output Type';
    Topics[0]:=' 8-bit Mono';                          TAvail[0]:=True;
    Topics[1]:=' 8-bit Mono   , hardware filter';      TAvail[1]:=True;
    Topics[2]:=' 8-bit Mono   , Linear Interpolation'; TAvail[2]:=True;
    Topics[3]:=' 8-bit Mono   , Cubic  Interpolation'; TAvail[3]:=True;
    Topics[4]:='16-bit Mono   , Cubic  Interpolation';
    Topics[5]:=' 8-bit Stereo ';                       TAvail[5]:=True;
    Topics[6]:=' 8-bit Stereo , hardware filter';      TAvail[6]:=True;
    Topics[7]:=' 8-bit Stereo , Linear Interpolation'; TAvail[7]:=True;
    Topics[8]:=' 8-bit Stereo , Cubic  Interpolation'; TAvail[8]:=True;
    Topics[9]:='16-bit Stereo , Cubic  Interpolation';
    If WriteWav then Begin TAvail[4]:=True; TAvail[9]:=True; end;
  End;
  DrawMenu(Menu);
END;

{лллллллллллллллллллллллллллллллллллллллл}
{лллллллллллллллллллллллллллллллллллллллл}

VAR
  Answer: Char;
  I, I2: Word;
  Choice: Byte;

  Port, IRQ, DMA: Word;
Label ExitOK;

BEGIN
  SoundCard_Menu; Choice:=GetChoice(Menu);
  WriteWAV:=False;
  Case Choice of
    0: Begin PlayDevice:=GUS_DRV1.GUSDevice;PLAYDevice.ID:=MODType1.GUSDevice;   End;
    1: Begin PlayDevice:=SB_DRV1.SBDevice;  PLAYDevice.ID:=MODType1.SB100Device; End;
    2: Begin PlayDevice:=SB_DRV1.SBDevice;  PLAYDevice.ID:=MODType1.SB200Device; End;
    3: Begin PlayDevice:=SB_DRV1.SBDevice;  PLAYDevice.ID:=MODType1.SB201Device; End;
    4: Begin PlayDevice:=SB_DRV1.SBDevice;  PLAYDevice.ID:=MODType1.SBPRODevice; End;
    5: Begin
         PlayDevice:=SB_DRV1.SBDevice;
         WriteWAV:=True;                ModInfo.RepeatModule:=False;
       End;
    else; begin PLAYDevice.ID:=NODevice; exit; end;
  end;

  If not WriteWAV then
    Begin
      BasePort_Menu;
      Choice:=GetChoice(Menu);
      If Choice<>NoTopic then Port:=$200+(Choice+1)*$10
      else begin PLAYDevice.ID:=NODevice; exit; end;

      If (PLAYDevice.ID=MODType1.SB100Device) or (PLAYDevice.ID=MODType1.SB200Device) or
         (PLAYDevice.ID=MODType1.SB201Device) or (PLAYDevice.ID=MODType1.SBPRODevice) or
         (PLAYDevice.ID=MODType1.SB16Device) then
      Begin
        IRQ_Menu; Choice:=GetChoice(Menu);
        Case Choice of
          0: IRQ:=2 ;
          1: IRQ:=3 ;
          2: IRQ:=4 ;
          3: IRQ:=5 ;
          4: IRQ:=7 ;
          5: IRQ:=9 ;
          6: IRQ:=10;
          7: IRQ:=11;
          8: IRQ:=12;
          9: IRQ:=15;
          else; begin PLAYDevice.ID:=NODevice; exit; end;
        End;

        DMA_Menu; Choice:=GetChoice(Menu);
        If Choice=NoTopic then begin PLAYDevice.ID:=NODevice; exit; end
        Else DMA:=Choice;
      End;
    End;


  If (PLAYDevice.ID=MODType1.SB100Device) or (PLAYDevice.ID=MODType1.SB200Device) or
     (PLAYDevice.ID=MODType1.SB201Device) or (PLAYDevice.ID=MODType1.SBPRODevice) or
     (PLAYDevice.ID=MODType1.SB16Device)  or WriteWav then
  Begin
    MixRate_Menu; Choice:=GetChoice(Menu);
    Case Choice of
      0: MixRate:=10000;
      1: MixRate:=13889;
      2: MixRate:=17857;
      3: MixRate:=21739;
      4: MixRate:=23810;
      5: MixRate:=27778;
      6: MixRate:=32258;
      7: MixRate:=35714;
      8: MixRate:=40000;
      9: MixRate:=45454;
      else; begin PLAYDevice.ID:=MODType1.NODevice; exit; end;
    End;
  End;

{ they can get on my nerves thes two...!
  DetailedOptions_Menu; Choice:=GetChoice(Menu);
  If Choice <> NoTopic then DetailedOptions:=Boolean(Choice)
  Else DetailedOptions:=False;
  If NOT DetailedOptions then goto ExitOK;

  Volume_Menu; Choice:=GetChoice(Menu);
  If Choice=NoTopic then begin PLAYDevice.ID:=NODevice; exit; end;
  If Choice<=4 then GlobalVolume:=(Choice+1)*20 - 1;
  If Choice =7 then Begin SBVolumeBoost:=True; GlobalVolume:=99; End;
}
  If (PLAYDevice.ID=SB100Device) or (PLAYDevice.ID=SB200Device) or
     (PLAYDevice.ID=SB201Device) or (PLAYDevice.ID=SBPRODevice) or
     (PLAYDevice.ID=SB16Device)  or WriteWav then
  Begin
    SBModeMenu_Menu; Choice:=GetChoice(Menu);
    If Choice=NoTopic then begin PLAYDevice.ID:=NODevice; exit; end;

    Case Choice of
      0: Begin SetFilter(NoFilter); StereoReplay:=False; End;
      1: Begin SetFilter(HWFilter); StereoReplay:=False; End;
      2: Begin SetFilter(LIFilter); StereoReplay:=False; End;
      3: Begin SetFilter(CIFilter); StereoReplay:=False; End;
      4: Begin SetFilter(CIFilter); StereoReplay:=False; SixteenBitMix:=True; End;
      5: Begin SetFilter(NoFilter); StereoReplay:=True;  End;
      6: Begin SetFilter(HWFilter); StereoReplay:=True;  End;
      7: Begin SetFilter(LIFilter); StereoReplay:=True;  End;
      8: Begin SetFilter(CIFilter); StereoReplay:=True;  End;
      9: Begin { 16 bit stereo with cubic interpolation }
           SetFilter(CIFilter);     StereoReplay:=True;  SixteenBitMix:=True;
           {WriteWAV:=True;            ModInfo.RepeatModule:=False;}
         End;
    End;
    If Not WriteWAV then
      If (PLAYDevice.ID=SBPRODevice) AND (MixRate>21739) then MixRate:=21739;
  End;

ExitOK:
  Case PlayDevice.ID of
    MODType1.GUSDevice  : With GUS_DRV1.GUSDevice do Begin MPort:=Port; MIRQ:=IRQ; MDMA:=DMA; End;
    MODType1.SB100Device,
    MODType1.SB200Device,
    MODType1.SB201Device,
    MODType1.SBPRODevice,
    MODType1.SB16Device : With SB_DRV1.SBDevice   do Begin MPort:=Port; MIRQ:=IRQ; MDMA:=DMA; End;
  End;

  Normvideo;
  ClrScr;
  SetCursor(StandardCursor);
END;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

END.