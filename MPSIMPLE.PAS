
Uses
  Shitty1,  { for the MIDAS-alike SETUP system                               }
  Dos,      { for Control-Break Handler                                      }
  Replay,   { get the MOD replay routines                                    }
  MODType1, { Information about the MODFormat                                }
  ModLoad,  { the MOD loader                                                 }
  GUS_DRV1, { Choose the GUS & SB drivers among the 36 drivers available ;-) }
  SB_DRV1;

VAR
  OldBreakHandler: Procedure;

Procedure SetExitScreen;
BEGIN
  WriteLn;
  WriteLn('MOD Player by BYTERAVER/TNT (The Next Temptation). (c) Jan 1999');
  WriteLn('Found bugs/want some more info? Read MPxx.DOC.');
  WriteLn('Have fun! Bye.');
  WriteLn;
  WriteLn('Feel free to contact me for any reason at erlandvo@hotmail.com.');
  WriteLn('I will be pleased to help you!');
END;

{$F+}
Procedure BreakHandler; Interrupt;
BEGIN
  WriteLn;
  If Playing then With PlayDevice do Begin StopReplay; CloseCard; End;
  SetIntVec($1B, @OldBreakHandler);
  OldBreakHandler; {call old control break handler}
  Halt;
END;
{$F-}

VAR
  I             : Integer;
  SB_Detected,
  GUS_Detected  : Boolean;
BEGIN
  GetIntVec($1B, @OldBreakHandler);   { Install Custom control-break Handler }
  SetIntVec($1B, @BreakHandler);

  If ParamCount<>1 then
  Begin
    WriteLn('You need to specify a .MOD file. Nothing More, Nothing Less.');
    Halt;
  End;

  SB_Detected :=SB_DRV1.SBDevice.SWDetected;
  GUS_Detected:=GUS_DRV1.GUSDevice.SWDetected;
  If Not (SB_Detected or GUS_Detected) then
    Begin WriteLn('You need a GUS or SB card to run this proggy.'); Halt; End;
  If SB_Detected and not GUS_Detected then PLAYDevice:=SB_DRV1.SBDevice;
  If GUS_Detected and not SB_Detected then PLAYDevice:=GUS_DRV1.GUSDevice;
  If SB_Detected and GUS_Detected then PLAYDevice:=GUS_DRV1.GUSDevice;
  If (PLAYDevice.OpenCard(4)<>1) then
    Begin Writeln('Sound Card initialisation failure... Exiting!'); Halt; End;

  I:=LoadMod(paramstr(1));
  If I<>NoError then Begin WriteLn(ErrStr(I)); Halt; End;


  Filter:=Interpolative;
  MixRate:=40000;
  If MixRate<=23000 then StereoReplay:=True;        {Temporary, For SB PRO...}

{Start the music! -----------------------------------------------------------}
  With PlayDevice do Begin OpenCard(ModInfo.NrChannels); StartReplay; End;
{$IFDEF DPMI}
  WriteLn('Type "exit"+<enter> to return to program and stop replay.');
  Swapvectors;
  Exec(GetEnv('COMSPEC', '');
  Swapvectors;
  If DosError<>0 then
    Begin
      WriteLn('couldn''t shell to DOS for some reason. Hit <Enter> to exit.');
      Readln;
    End;
{$ELSE}
  WriteLn('Hit enter to exit.');
  Readln;
{$ENDIF}
  PlayDevice.StopReplay;
  PlayDevice.CloseCard;
END.
