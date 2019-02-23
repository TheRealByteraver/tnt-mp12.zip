{
  This is the powerful ;-) MOD loader: you're final solution for that
  nasty file xAytGFp!.MOD that just can't be played by ANY of your other
  players ;-).

  Copyright (c) 1998, Erland Van Olmen (erlandvo@hotmail.com)
  Protected Mode Ready

  Features of this loader:
  ў SoundCard Independent
  ў Tested successfully with over 3000 MODs
  ў Support for up to 8 octaves
  ў Loads all existant MOD formats: 15/31 instruments, 1..32 channels
    (TakeTracker 1,2,3,5,7,9,11,13,15 channel MODs are supported, too)
  ў Loads .WOW modules, too (8 & 4 channel type)
  ў Built-in cleaner: unused patterns and unused samples are not loaded
                      to save (GUS) memory
  ў Patterns are repacked: average compression is 64% (the patterns take
    about 36% of their original size). This can save up to 768kB of memory
    (32 channels, 128 patterns without effects or instrument specs ;-) ).
    (The most memory I ever saved was when loading DOPE.MOD: 267kB)
    --> Note that the pattern repacking thing is now less efficient due to
    the addition of the 3 extra octaves since the version 1.2
  ў Detects MODs that are compressed with PowerPacker (other MODPlayers
    just consider this files as plain corrupt or might even crash!)
    The Protected Mode version of the player executes the decompressing
    tool and then loads the module.
  ў Detects 31-instruments MOD that have a screwed ID field. The nr of
    channels is calculated using the size of a pattern. Other players
    will consider the file as garbage or play it as an 15-instrument file
    (which will sound totally wrong of course)
  ў Support for StarTrekker 8 channel modules has been added, but this has
    NOT been tested thoroughfully! If you have a GENUINE 8 channel Module
    created with Startrekker, contact me!
  ў Corrupt modules will be loaded, too, and be played as good as possible


  Known Bugs:

      - None


ўўBugfixes/changes/additions since version 1.0 (first public release):

      - None


ўўBugfixes/changes/additions since version 1.1 (2nd public release):

      - The MOD loader is now compatible with FastTracker II MOD's
        that use 8 octaves.
      - changed pattern-repack code to make it compatible with the new
        packing scheme introduced because of the extra octaves
      - The loader now loads modules where more than one sample is
        missing (corrupt modules) instead of giving the error message
        "module is corrupt"
      - Removed one floating point calculation
      - You can now also load .WOW modules without specifying the extension.
      - Fixed small bug that caused an incorrect I/O error when trying to
        unpack PP20 packed modules in existing "c:\temp" directory
}

{DEFINE DEBUGINFO}
{$X+,R-,Q-,S-,I-,G+,N+,E-}
Unit ModLoad;

interface

Function LoadMOD(FileName: String): Word;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}
{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

implementation

Uses
{$IFDEF DPMI}
  Dos,
{$ENDIF}
  NewCrt,
  MODType1;

Type
{These structures ( = records) represent exactly the layout of a MOD file: --}
  TSampleHeader = packed record
    Name        : Array[0..21] of Char;
    Length      : Word;         { Big-End Word; * 2 = samplelength in bytes. }
    FineTune    : Byte;         { This is in fact a signed nibble            }
    LinearVolume: Byte;
    RepeatOffset: Word;         { Big-End Word; * 2 = RepeatOffset in bytes. }
    RepeatLength: Word;         { Big-End Word; * 2 = RepeatLength in bytes. }
  end;

  THeaderNST = packed Record           { NST header layout }
    SongTitle      : Array[0..19 ] of Char;
    Samples        : Array[1..15 ] of TSampleHeader;
    SongLength     : Byte;
    RestartPosition: Byte;
    PatternTable   : Array[0..127] of Byte;
  end;

  THeaderMK = packed Record            { M.K. header layout }
    SongTitle      : Array[0..19 ] of Char;
    Samples        : Array[1..31 ] of TSampleHeader;
    SongLength     : Byte;
    RestartPosition: Byte;
    PatternTable   : Array[0..127] of Byte;
    Tag            : Array[0..3  ] of Char;
  end;

  TTag = Record
    Tag       : String[4];
    NrChannels: Byte;
  end;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

Const
{Miscellaneous constants: ---------------------------------------------------}
  ReadOnly = 0;           { MUST be here! Don't use the Built-in Constant!   }

  NO_MOD = 0;
  NT_MOD = 1;
  MK_MOD = 2;

  TagOffset     = 1080;
  UnknownModule = 255;
  MaxTags       = 5;
  Tags: Array[1..MaxTags+4] of TTag = (
    (Tag: 'N.T.'; NrChannels: 4 ),   { this tag does not exist }
    (Tag: 'M.K.'; NrChannels: 4 ),
    (Tag: 'M!K!'; NrChannels: 4 ),
    (Tag: 'OCTA'; NrChannels: 8 ),   { this tag may not exist  }
    (Tag: 'CD81'; NrChannels: 8 ),
    (Tag: 'TDZ' ; NrChannels: 3 ),   { NrChannels = max channels }
    (Tag: 'CHN' ; NrChannels: 9 ),   { NrChannels = max channels }
    (Tag: 'CH'  ; NrChannels: 32),   { NrChannels = max channels }
    (Tag: 'FLT' ; NrChannels: 8 ));  { NrChannels = max channels }

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

VAR
  MODFile           : File;                          { global... }
  PatternDataOffset,
  SampleDataOffset  : LongInt;
  PatternSize       : Word;
  TrashedID,
  WOWModule,
  PP20Decompress,
  StarTrekkerModule : Boolean;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

Function GetExtension(FName: String): String;
VAR
  I, I2, Len: Word;
  Dum: String;
BEGIN
  Len:=Ord(FName[0]); I:=Len; I2:=0;
  While (FName[I]<>'.') and (I>0) do begin Dec(I); Inc(I2); end;
  If (I=0) or (I2>3) then Dum:='' Else Dum[0]:=Chr(I2);
  For I:=1 to I2 do Dum[I]:=UpCase(FName[Len-I2+I]);
 GetExtension:=Dum;
END;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

{This fn returns true if the file FName exists. (The file may be read-only)  }
Function IfExists(FName: String): Boolean;
VAR
   F: File;
   d, tmpFM: Byte;
Begin
  tmpFM:=FileMode;
  FileMode:=ReadOnly;
  Assign(F, FName); Reset(F, 1);
  FileMode:=tmpFM;
  If IOResult<>0 then IfExists:=False Else IfExists:=True;
  Close(F); d:=IOResult;
End;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

Function GetExactFileName: Boolean;
Const
  StandardExt = 'MOD';
  WOWExt      = 'WOW';
VAR
  Ext: String;
BEGIN
  GetExactFileName:=False;
  If IfExists(ModInfo.ModFileName) then
    Begin GetExactFileName:=True; Exit; End;
  With ModInfo do
  Begin
    Ext:=GetExtension(ModFileName);
    If Ext='' then
      Begin
        If ModFileName[Length(ModFileName)]<>'.' then
                                                 ModFileName:=ModFileName+'.';
        If IfExists(ModFileName+StandardExt) then
          Begin
            ModFileName:=ModFileName+StandardExt; GetExactFileName:=True;
          End
        Else If IfExists(ModFileName+WOWExt) then
             Begin
               ModFileName:=ModFileName+WOWExt; GetExactFileName:=True;
               WowModule:=True;
             End;
{
        If ModFileName[Length(ModFileName)]='.' then
             ModFileName:=ModFileName    +StandardExt
        Else ModFileName:=ModFileName+'.'+StandardExt;
        If IfExists(ModFileName) then GetExactFileName:=True;
}
      End
    Else
      Begin
        If ModFileName[Length(ModFileName)]='.' then
          If IfExists(ModFileName+StandardExt) then
            Begin
              ModFileName:=ModFileName+StandardExt; GetExactFileName:=True;
            End
          Else If IfExists(ModFileName+WOWExt) then
               Begin
                 ModFileName:=ModFileName+WOWExt; GetExactFileName:=True;
                 WowModule:=True;
               End;
{
        If ModFileName[Length(ModFileName)]='.' then
             ModFileName:=ModFileName    +StandardExt;
        If IfExists(ModFileName) then GetExactFileName:=True;
}
      End;
  End;
END;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

Function OpenFile: Boolean; { True if successful }
VAR
  DummyB: Byte;
BEGIN
  OpenFile:=False;
{ Read only File Access: in Read/Write mode (default), you'll get an I/O
  error if you try to read a Read-Only file (on CD-ROMS for example): -------}
  FileMode:=ReadOnly;
  Assign(MODFile, MODInfo.ModFileName); Reset(MODFile, 1); DummyB:=IOResult;
  If DummyB=0 then OpenFile:=True;
END;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

Function GetMODType: Byte;  { Returns: 255 if unknown or WOW, else nr of chn }
VAR
  RTag: Array [1..4] of Char;
  Tag : String[4];
  I   : Byte;
BEGIN
  GetMODType:=UnknownModule;
  Seek(MODFile, TagOffset); BlockRead(MODFile, RTag, SizeOf(RTag));
  Reset(MODFile, 1); If IOResult<>0 then exit;
  For I:=1 to 4 do ModInfo.Tag[I]:=RTag[I]; ModInfo.Tag[0]:=chr(4);
  For I:=1 to 4 do Tag[I]:=RTag[I]; Tag[0]:=chr(4);
  For I:=3 to MaxTags do {first tag does not exists}
    If Tag=Tags[I].Tag then
      Begin GetMODType:=Tags[I].NrChannels; {ModInfo.Tag:=Tags[I].Tag;} Exit; End;

{Is this an "M.K." module? YES? -=> check for .WOW: -------------------------}
  If Tag=Tags[2].Tag then
    If GetExtension(ModInfo.ModFileName)='WOW' Then
         Begin WOWModule:=True;  GetMODType:=255; Exit; End
    Else Begin                   GetMODType:=4;   Exit; End;

{Is This a TDZx file? -------------------------------------------------------}
  For I:=1 to 3 do Tag[I]:=RTag[I]; Tag[0]:=chr(3);
  If Tag=Tags[MaxTags+1].Tag then
  Begin
    I:=Ord(RTag[4])-48; If (I>=1) and (I<=3) then GetMODType:=I; Exit;
  End;

{Is This a FLTx file? -------------------------------------------------------}
  For I:=1 to 3 do Tag[I]:=RTag[I]; Tag[0]:=chr(3);
  If Tag=Tags[MaxTags+4].Tag then
  Begin
    I:=Ord(RTag[4])-48;
    If (I=4) then Begin GetMODType:=I; Exit; End;
    If (I=8) then Begin GetMODType:=I; StarTrekkerModule:=True; End
    Else GetMODType:=255;
    Exit;
  End;

{Is This a xCHN file? -------------------------------------------------------}
  For I:=1 to 3 do Tag[I]:=RTag[I+1]; Tag[0]:=chr(3);
  If Tag=Tags[MaxTags+2].Tag then
  Begin
    I:=Ord(RTag[1])-48; If ((I>=2) and (I<=9)) then GetMODType:=I; Exit;
  End;

{Is This a xxCH file? -------------------------------------------------------}
  For I:=1 to 2 do Tag[I]:=RTag[I+2]; Tag[0]:=chr(2);
  If Tag=Tags[MaxTags+3].Tag then
  Begin
    I:=(Ord(RTag[1])-48)*10; Inc(I, Ord(RTag[2])-48);
    If (I>=10) and (I<=32) then GetMODType:=I;
  End;
END;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

Function CheckPP20: Boolean;
CONST
  IDString: String[4] = 'PP20';
VAR
  I       : Byte;
  MODTitle: Array[1..4] of Char;
BEGIN
  CheckPP20:=False;
  Reset(MODFile, 1); BlockRead(MODFile, MODTitle, SizeOf(MODTitle));
  Reset(MODFile, 1); If IOResult<>0 then exit;
  CheckPP20:=True;
  For I:=1 to 4 do
    If MODTitle[I]<>IDString[I] Then CheckPP20:=False;
END;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

{This function checks if the string consists of characters that are commonly
 used in the SampleName-Field.
 Return: nr of non-common char's --------------------------------------------}
Function IfAscii(Test: Array of char): Byte;
VAR
   I, I2: Word;
BEGIN
  I2:=0; I:=0;
  For I:=0 to 21 do
    If (Ord(Test[I]) in [1..12, 15..31, 127..195, 197..204, 206..254]) then
                                                                      Inc(I2);
  IfAscii:=I2;
END;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

Function CheckSNames: Byte;
Const
  LIMIT  = 8;
VAR
  I        : Byte;
  Cnt, Cnt2: Word;
  HeaderMK : THeaderMK;
BEGIN
  CheckSNames:=255;
  Reset(MODFile, 1); BlockRead(MODFile, HeaderMK, SizeOf(HeaderMK));
  Reset(MODFile, 1); If IOResult<>0 then exit;

{Are sample 16-31 names ASCII? YES? => M.K., NO? => N.T., maybe.
 Are sample 1 -15 names ASCII? YES? => N.T., NO? => File is not a module.----}
  Cnt:=0; Cnt2:=0;
  For I:=1  to 15 do Inc(Cnt , IfASCII(HeaderMK.Samples[I].Name));
  For I:=16 to 31 do Inc(Cnt2, IfASCII(HeaderMK.Samples[I].Name));
  If Cnt2>LIMIT then                     { File is not a 31-instr }
    If Cnt>LIMIT then CheckSNames:=NO_MOD{ File is not a 15-instr }
    Else CheckSNames:=NT_MOD
  Else CheckSNames:=MK_MOD;
END;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

Function GetNrChn: Byte;    { nr of chn if successful (4 or 8), 255 if not }
VAR
  ModFileSize,
  TotalSize  : LongInt;
  I, NrChn   : Byte;
BEGIN
  GetNrChn:=255;
  ModFileSize:=FileSize(MODFile);
  If ModFileSize<SizeOf(THeaderMK) then Exit;
  Dec(ModFileSize, SizeOf(THeaderMK));
  TotalSize:=0;
  For I:=1 to ModInfo.MaxSamples do Inc(TotalSize, ModInfo.Samples[I].Length);
  If TotalSize>ModFileSize then Exit;
  Dec(ModFileSize, TotalSize);
  GetNrChn:=((ModFileSize+10) div ModInfo.NrPatterns) shr 8;
END;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

Function InitHeader: Word;  { 0 = NoError }
VAR
  I, I2    : Byte;
  HeaderMK : THeaderMK ;
  HeaderNST: THeaderNST;
  PatternA,
  PatternB : LongInt;

  ModFileSize,
  AllPatternSize,
  TotalSampleLength : LongInt;
BEGIN
  InitHeader:=255;
  TotalSampleLength      :=0;
  SampleDataOffset       :=0;
  PatternDataOffset      :=0;
  ModInfo.NrSamples      :=0;
  ModInfo.NrPatterns     :=0;
  ModInfo.SongLength     :=0;
  ModInfo.RestartPosition:=0;
  ModInfo.SongTitle      :='';
  ModInfo.CustomRepeat   :=False;
  For I:=0 to 127 do ModInfo.PatternTable[I]:=0;
{Calculating the size of an individual pattern: -----------------------------}
  PatternSize:=Word(ModInfo.NrChannels) SHL 8;

  Reset(MODFile, 1); If IOResult<>0 then exit;
  ModInfo.SongTitle[0]:=chr(20);
  Case ModInfo.MaxSamples of
    15: Begin
          PatternDataOffset:=SizeOf(HeaderNST);
          BlockRead(MODFile, HeaderNST, SizeOf(HeaderNST));
          For I:=1 to 20 do ModInfo.SongTitle[I]:=HeaderNST.SongTitle[I-1];
          ModInfo.SongLength     :=HeaderNST.SongLength;
          ModInfo.RestartPosition:=HeaderNST.RestartPosition;
          If (ModInfo.RestartPosition<ModInfo.SongLength) and
             (ModInfo.RestartPosition<>0) then ModInfo.CustomRepeat:=True;
          For I:=0 to 127 do ModInfo.PatternTable[I]:=HeaderNST.PatternTable[I];
          For I2:=1 to 15 do
          Begin
            For I:=1 to 22 do ModInfo.Samples[I2].Name[I]:=HeaderNST.Samples[I2].Name[I-1];
            ModInfo.Samples[I2].Name[0]:=chr(22);
            ModInfo.Samples[I2].Length      :=LongInt(Swap(HeaderNST.Samples[I2].Length)      ) SHL 1;
            ModInfo.Samples[I2].RepeatOffset:=LongInt(Swap(HeaderNST.Samples[I2].RepeatOffset)) SHL 1;
            ModInfo.Samples[I2].RepeatLength:=LongInt(Swap(HeaderNST.Samples[I2].RepeatLength)) SHL 1;
            ModInfo.Samples[I2].Used        :=False;
            ModInfo.Samples[I2].RepeatSample:=ModInfo.Samples[I2].RepeatLength>2;
            ModInfo.Samples[I2].Volume      :=HeaderNST.Samples[I2].LinearVolume;
            If ModInfo.Samples[I2].Volume>64 then ModInfo.Samples[I2].Volume:=64;
            Inc(TotalSampleLength, ModInfo.Samples[I2].Length);
            If ModInfo.Samples[I2].Length>2 then Inc(ModInfo.NrSamples);
            If HeaderNST.Samples[I2].FineTune>$F then
                 ModInfo.Samples[I2].FineTune:=0
            Else ModInfo.Samples[I2].FineTune:=HeaderNST.Samples[I2].FineTune;
            {}
            ModInfo.Samples[I2].FineTune:=Byte(ModInfo.Samples[I2].FineTune) SHL 4;
            {}
            ModInfo.Samples[I2].OldFineTune:=ModInfo.Samples[I2].FineTune;
          End;

          For I2:=16 to 31 do
          Begin
            ModInfo.Samples[I2].Name:='';
            ModInfo.Samples[I2].Length      :=0;
            ModInfo.Samples[I2].RepeatOffset:=0;
            ModInfo.Samples[I2].RepeatLength:=0;
            ModInfo.Samples[I2].FineTune    :=0;
            ModInfo.Samples[I2].OldFineTune :=0;
            ModInfo.Samples[I2].Used        :=False;
            ModInfo.Samples[I2].RepeatSample:=False;
          End;

        End;
    31: Begin
          PatternDataOffset:=SizeOf(HeaderMK);
          BlockRead(MODFile, HeaderMK , SizeOf(HeaderMK));
          For I:=1 to 20 do ModInfo.SongTitle[I]:=HeaderMK.SongTitle[I-1];
          ModInfo.SongLength     :=HeaderMK.SongLength;
          ModInfo.RestartPosition:=HeaderMK.RestartPosition;
          If (ModInfo.RestartPosition<ModInfo.SongLength) and
             (ModInfo.RestartPosition<>0) then ModInfo.CustomRepeat:=True;
          For I:=0 to 127 do ModInfo.PatternTable[I]:=HeaderMK.PatternTable[I];
          For I2:=1 to 31 do
          Begin
            For I:=1 to 22 do ModInfo.Samples[I2].Name[I]:=HeaderMK.Samples[I2].Name[I-1];
            ModInfo.Samples[I2].Name[0]:=chr(22);
            ModInfo.Samples[I2].Length      :=LongInt(Swap(HeaderMK.Samples[I2].Length)      ) SHL 1;
            ModInfo.Samples[I2].RepeatOffset:=LongInt(Swap(HeaderMK.Samples[I2].RepeatOffset)) SHL 1;
            ModInfo.Samples[I2].RepeatLength:=LongInt(Swap(HeaderMK.Samples[I2].RepeatLength)) SHL 1;
            ModInfo.Samples[I2].Used        :=False;
            ModInfo.Samples[I2].RepeatSample:=ModInfo.Samples[I2].RepeatLength>2;
            ModInfo.Samples[I2].Volume      :=HeaderMK.Samples[I2].LinearVolume;
            If ModInfo.Samples[I2].Volume>64 then ModInfo.Samples[I2].Volume:=64;
            Inc(TotalSampleLength, ModInfo.Samples[I2].Length);
            If ModInfo.Samples[I2].Length>2 then Inc(ModInfo.NrSamples);

            If HeaderMK.Samples[I2].FineTune>$F then
                 ModInfo.Samples[I2].FineTune:=0
            Else ModInfo.Samples[I2].FineTune:=HeaderMK.Samples[I2].FineTune;
            {}
            ModInfo.Samples[I2].FineTune:=Byte(ModInfo.Samples[I2].FineTune) SHL 4;
            {}
            ModInfo.Samples[I2].OldFineTune:=ModInfo.Samples[I2].FineTune;
          End;

        End;
   else; exit;
  end;

{Check sample headers: ------------------------------------------------------}
  For I2:=1 to ModInfo.MaxSamples Do
  With ModInfo do
  Begin
    If Samples[I2].RepeatOffset>Samples[I2].Length Then
      Begin
        Samples[I2].RepeatOffset:=0;
        Samples[I2].RepeatLength:=0;
        Samples[I2].RepeatSample:=False;
      End;
    If Samples[I2].RepeatOffset+Samples[I2].RepeatLength>Samples[I2].Length Then
       Samples[I2].RepeatLength:=Samples[I2].Length-Samples[I2].RepeatOffset;
  End;

{Checking the NrPtnPlay Value: ----------------------------------------------}
  If ModInfo.SongLength>128 then ModInfo.SongLength:=128;
{Checking the RestartPosition value: ----------------------------------------}
  If (ModInfo.RestartPosition>=ModInfo.SongLength) or
     (ModInfo.RestartPosition>=127               ) then
                                                   ModInfo.RestartPosition:=0;
  If StarTrekkerModule then
    For I:=0 to 127 do ModInfo.PatternTable[I]:=ModInfo.PatternTable[I] shr 1;

  PatternA:=0; PatternB:=0;
{Now we're going to calc. the nr of patterns STORED in the song. NOT EASY!!!!
 THE thing to find is the offset of the SampleData, then we just divide the
 total size taken by the patterns by the size of one pattern. ---------------}

{Looking for the fileSize of the ModFile: -----------------------------------}
  ModFileSize:=FileSize(MODFile);

  If Not (WOWModule or TrashedID) then
  Begin
{Calculating the number of total patterns with first method: ----------------
 (substracting the size of the samples and the header of the module from the
  size of the file, then divide this value by the size of one pattern. This is
  dangerous because some (mostly ripped) mods are VERY big, that is:
  full of garbage. (Modules ripped by Sound Studio 2.2 for example.)         }

    AllPatternSize:=ModFileSize-PatternDataOffset-TotalSampleLength;
{    PatternA:=Round(AllPatternSize/PatternSize);}
    PatternA:=AllPatternSize div PatternSize;
    If       (AllPatternSize mod PatternSize) > (PatternSize div 2) then
                                                                Inc(PatternA);
  End;
{Calculating the number of total patterns with second method: ---------------
 (by scanning the pattern sequence table for the highest value. This is dange-
 rous (also!), 'coz sometimes modules contain unused patterns. If the last
 pattern in the file is an unused one, the following method may fail. BUT
 fortunately it seems that Trackers always set the highest pattern nr in the
 sequence Table after the song-sequence.                                     }

  For I:=0 to 127 do                        { scan ALL patterns positions... }
    If ModInfo.PatternTable[I]>127 then
      Begin InitHeader:=ToHighPatternNr; Exit; End
    Else
      If ModInfo.PatternTable[I]>PatternB then
                                            PatternB:=ModInfo.PatternTable[I];
  Inc(PatternB); { <=- because the patterns are numbered from 0 }
  If WOWModule or TrashedID then PatternA:=PatternB;

  If ((PatternA>64) and (PatternB<=64)) then PatternA:=PatternB;

{Now evaluate! What are we going to do now? ;-) -----------------------------}
  If PatternA=PatternB then ModInfo.NrPatterns:=PatternA     { MOD is clean! }
  Else
    Begin  { There's garbage, or unused patterns AFTER the last used pattern }
      If PatternB<PatternA then ModInfo.NrPatterns:=PatternB {???}
      Else  { module is corrupt! (too few data, sample data or Pattern data) }
        If ((PatternB>64) and (ModInfo.Tag<>Tags[3].Tag)) then
                                             ModInfo.NrPatterns:=PatternA
        Else                                 ModInfo.NrPatterns:=PatternB;
    End;

  If ModInfo.NrPatterns=0 then Begin InitHeader:=NoPatterns; Exit; End;

  If WOWModule then
  Begin
    I:=GetNrChn;
    If (I=255) or ((I<>4) and (I<>8)) then { assume M.K. 4chn (?) }
         Begin ModInfo.NrChannels:=4; PatternSize:=1024; End
    Else Begin ModInfo.NrChannels:=I; PatternSize:=Word(I) shl 8; End;
  End;
  If TrashedID then
  Begin
    I:=GetNrChn;
    If (I<1) or (I>32) then Exit;
    ModInfo.NrChannels:=I; PatternSize:=Word(I) shl 8;
  End;

  AllPatternSize  :=LongInt(ModInfo.NrPatterns)*PatternSize;
  SampleDataOffset:=PatternDataOffset+AllPatternSize;

{Make a last check of the pattern sequence table: ---------------------------}
  For I:=0 to 127 do If ModInfo.PatternTable[I]>ModInfo.NrPatterns-1 then
                                                   ModInfo.PatternTable[I]:=0;
  InitHeader:=NoError;
END;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}
{fn returns non-zero if error occured...}
Function ConvertStarTrekkerPattern(Pattern: PByteBuffer): Byte;
VAR
  Buffer: PByteBuffer;
  I, I2 : Word;
BEGIN
  ConvertStarTrekkerPattern:=255;
  If MaxAvail<2048 then exit; GetMem(Buffer, 2048); { get memory   }
  For I:=0 to 2048-1 do Buffer^[I]:=Pattern^[I];    { copy pattern }
  For I:=0 to 64-1 do
  Begin
    For I2:=0 to 4*4-1 do Pattern^[I*8*4+I2    ]:=Buffer^[I*4*4+I2];
    For I2:=0 to 4*4-1 do Pattern^[I*8*4+4*4+I2]:=Buffer^[I*4*4+I2+1024];
  End;
  FreeMem(Buffer, 2048);
  ConvertStarTrekkerPattern:=0;
END;
{ This function is handy to convert 8CHN MODs to FLT8 MODs. Only needed for
  testing.
Function ConvertToStarTrekkerPattern(Pattern: PByteBuffer): Byte;
VAR
  Buffer: PByteBuffer;
  I, I2 : Word;
BEGIN
  ConvertToStarTrekkerPattern:=255;
  If MaxAvail<2048 then exit; GetMem(Buffer, 2048);
  For I:=0 to 2048-1 do Buffer^[I]:=Pattern^[I];
  For I:=0 to 64-1 do
  Begin
    For I2:=0 to 4*4-1 do Pattern^[I*4*4+I2     ]:=Buffer^[I*8*4+I2    ];
    For I2:=0 to 4*4-1 do Pattern^[I*4*4+I2+1024]:=Buffer^[I*8*4+4*4+I2];
  End;
  FreeMem(Buffer, 2048);
  ConvertToStarTrekkerPattern:=0;
END;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

Function LoadPatterns: Word; { 0 -=> no error }
VAR
  I          : Byte;
  Buffer     : Array[0..32*4*64-1] of Byte;
  PatternUsed: Array[0..127] of Boolean;
  FreeMemory : LongInt;
  ExtraP,
  Period,
  RepackCount,
  NoteCount  : Word;
  Pack,
  Sample,
  Note,
  Effect,
  EffectArg,
  DummyB1,
  DummyB2    : Byte;
  NewNote,
  NewFX,
  NewFXArg,
  NewSample,
  Check      : Boolean;

BEGIN
  LoadPatterns:=255;
  Seek(MODFile, PatternDataOffset); I:=IOResult;
  If I<>0 then Begin LoadPatterns:=I; Exit; End;

  For I:=0 to 127 do PatternUsed[I]:=False;
  For I:=0 to ModInfo.SongLength-1 do PatternUsed[ModInfo.PatternTable[I]]:=True;

  For I:=0 to ModInfo.NrPatterns-1 do
  If PatternUsed[I] then
    Begin
      BlockRead(MODFile, Buffer, PatternSize);
      If StarTrekkerModule then ConvertStarTrekkerPattern(@Buffer);
      RepackCount:=0;
      For NoteCount:=0 to Word(ModInfo.NrChannels)*64-1 do
        Begin
{Decoding pattern info: -----------------------------------------------------}
          DummyB1  :=Buffer[NoteCount shl 2  ];
          Period   :=Buffer[NoteCount shl 2+1];
          DummyB2  :=Buffer[NoteCount shl 2+2];
          EffectArg:=Buffer[NoteCount shl 2+3];
          ExtraP   :=((DummyB1 and $F0) + (DummyB2 shr 4));
          Sample   :=ExtraP AND $1F;
          ExtraP   :=(ExtraP SHR 5) SHL 12;
          Inc(Period, (Word(DummyB1 and $0F) shl 8)+ExtraP);
          Effect   :=DummyB2 and $0F;

{Recoding pattern info: -----------------------------------------------------}
          Note:=0; Check:=False;
          If Period>0 then
            Begin
              If Period<MinPeriod Then Period:=MinPeriod;
              If Period>MaxPeriod Then Period:=MaxPeriod;
              Repeat
                Inc(Note);
                If Period >= PeriodRec[Note] then Check:=true
                Else If (Note >= 8*12) then begin Note:=0; Check:=True; end;
              Until Check;
              If (Note > 8*12{-1}) then Note:=0;
            End;
{Remap some effect numbers & fix some buggy effect specifications: ----------}
          Case Effect of
            $0: If EffectArg<>0 then Effect:=$10;{ change arpeggio effect nr }
            $5: If EffectArg= 0 then Effect:=$3;   { vol. slide w/o argument }
            $6: If EffectArg= 0 then Effect:=$4;   { vol. slide w/o argument }
            $9,       { these effects have no meaning when their argument is }
            $A,       { zero; some are even illegal without argument         }
            $F: If EffectArg= 0 then Effect:=0;     { No speed zero, useless }
            $B: If EffectArg>ModInfo.SongLength-1 then EffectArg:=ModInfo.RestartPosition;
            $C: If EffectArg>64 then EffectArg:=64;
            $E: begin
                  Effect   :=$E0+(EffectArg shr 4);
                  EffectArg:=EffectArg and $0F;
                  Case Effect of
                    $E0: Begin Effect:=0; EffectArg:=0; End;
                    $E1,
                    $E2,
                    $EA,
                    $EB,
                    $ED: If EffectArg=0 then Effect:=0;
                  End;
                end;
          end;
{Analysing part: ------------------------------------------------------------}
          If (Sample>0) and (Sample<=ModInfo.MaxSamples)
             and (ModInfo.Samples[Sample].Length>2) then
                                           ModInfo.Samples[Sample].Used:=True;
{Re-packing part: -----------------------------------------------------------}
          If (Sample>0) AND (Sample<=ModInfo.MaxSamples)
                          then NewSample:=True Else NewSample:=False;
          If Note     <>0 then NewNote  :=True Else NewNote  :=False;
          If Effect   <>0 then NewFX    :=True Else NewFX    :=False;
          If EffectArg<>0 then NewFXArg :=True Else NewFXArg :=False;
          If NewNote AND NewSample AND NewFX AND NewFXArg Then
            Begin
              Buffer[RepackCount]:=Note;      Inc(RepackCount);
              Buffer[RepackCount]:=Sample;    Inc(RepackCount);
              Buffer[RepackCount]:=Effect;    Inc(RepackCount);
              Buffer[RepackCount]:=EffectArg; Inc(RepackCount);
            End
          Else
            Begin
              Pack:=$80;
              If NewNote   Then Pack:=Pack OR $1;
              If NewSample Then Pack:=Pack OR $2;
              If NewFX     Then Pack:=Pack OR $4;
              If NewFXArg  Then Pack:=Pack OR $8;
              Buffer[RepackCount]:=Pack; Inc(RepackCount);
              If NewNote   Then Begin Buffer[RepackCount]:=Note;      Inc(RepackCount); End;
              If NewSample Then Begin Buffer[RepackCount]:=Sample;    Inc(RepackCount); End;
              If NewFX     Then Begin Buffer[RepackCount]:=Effect;    Inc(RepackCount); End;
              If NewFXArg  Then Begin Buffer[RepackCount]:=EffectArg; Inc(RepackCount); End;
            End;
        End;
{ Do we have enough memory? Check: ------------------------------------------}
      FreeMemory:=MaxAvail-8;
      If FreeMemory<RepackCount then
                                   Begin LoadPatterns:=OutOfMemory; Exit; End;
      Buffer[RepackCount]:=0;
      GetMem(Patterns[I], RepackCount+1);
{Move the repacked pattern from buffer (stack) to heap memory: --------------}
      Move(Buffer, Patterns[I]^, RepackCount);
    End
  Else
    Seek(MODFile, FilePos(MODFile)+PatternSize);

  LoadPatterns:=NoError;
END;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}
{ The DMPI version of this fn spawns the PowerPacker decompressing tool }
Function LoadPP20Module: Word;
VAR
  FNBackUp,
  CurDirStr,
  TempFileStr,
  PP20FileStr,
  PP20ArgsStr,
  TempDirStr : String;
  TempFile   : File;
  Check      : Boolean;
  I          : Word;
BEGIN
  LoadPP20Module:=0;
  FNBackUp:=MODInfo.MODFileName;
{$IFDEF DPMI}
      Begin
        PrintLn('The following program has not been written by the author of MP.');
        PrintLn('If the decompressing utility makes your system hang, then try');
        PrintLn('to decompress the file manually.');
        PrintLn('');
{Find TEMP directory: -------------------------------------------------------}
        GetDir(0, CurDirStr); { save original directory }
        TempDirStr:=GetEnv('TEMP');
        If Length(TempDirStr) = 0 then
          Begin
            TempDirStr:='c:\temp'; MkDir(TempDirStr);
            I:=IOResult;  { Clear I/O flag if directory should exist already }
          End;
        ChDir(TempDirStr);
        TempFileStr:=TempDirStr + '\tempfile.mod';
        Assign(TempFile, TempFileStr);
        Rewrite(TempFile); Close(TempFile); Erase(TempFile); { perform check }
        ChDir(CurDirStr);
        If IOResult<>0 then Begin LoadPP20Module:=PP20DecompErr; exit; End;
{Find pp2mod.exe decompressing tool: ----------------------------------------}
        PP20FileStr:=ParamStr(0); I:=Length(PP20FileStr); Check:=False;
        Repeat
          If PP20FileStr[I]<>'\' then Dec(I) Else Check:=True;
        Until check or (I=0);
        PP20FileStr[0]:=chr(I); { cut the ">mp xxx.mod -mxx" part of cmd str }
{Append "pp2mod.exe source_file destination_file" part: ---------------------}
        PP20FileStr:=PP20FileStr+'pp2mod.exe';
        PP20ArgsStr:=MODInfo.MODFileName+' '+TempFileStr;
{Check existence of pp2mod.exe: ---------------------------------------------}
        Assign(TempFile, PP20FileStr); Reset(TempFile, 1); Close(TempFile);
        If IOResult<>0 Then Begin LoadPP20Module:=DecompNotFound; Exit; End;
{Execute the decompressor: --------------------------------------------------}
        SwapVectors; Exec(PP20FileStr, PP20ArgsStr); SwapVectors;
{Check if decompressed file has been written: -------------------------------}
        Assign(TempFile, TempFileStr); Reset(TempFile); Close(TempFile);
        If (IOResult<>0) Then Begin LoadPP20Module:=PP20DecompErr; exit; End;
{Load decompressed module: --------------------------------------------------}
        PrintLn('');
        I:=LoadMOD(TempFileStr);
        MODInfo.MODFileName:=FNBackUp;
        Assign(TempFile, TempFileStr); Erase(TempFile);    { wipe temp. file }
        LoadPP20Module:=I;
        Exit;
      End;
{$ELSE}
      Begin LoadPP20Module:=PP20CorruptMod; exit; End;
{$ENDIF}
END;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

Function LoadMOD(FileName: String): Word;
VAR
  I,
  DummyB     : Byte;
  DummyW     : Word;
BEGIN
  WOWModule:=False; StarTrekkerModule:=False;
  TrashedID:=False; PP20Decompress:=False;
  With ModInfo do
  Begin
    MODFileName:=FileName;
    If not GetExactFileName then begin LoadMod:=WrongFileName; exit; end;
{Looking for the Mod: -------------------------------------------------------}
    If not OpenFile         then begin LoadMod:=WrongFileName; exit; end;
{What for MOD is this? ------------------------------------------------------}
    DummyB:=GetMODType;
    If DummyB<>UnknownModule then
      Begin MaxSamples:=31; NrChannels:=DummyB; End
    Else
      If Not WOWModule Then
        Begin
          DummyB:=CheckSNames;
          Case DummyB of { Check if this file is compressed with PowerPacker:}
            NO_MOD: If CheckPP20 then PP20Decompress:=True
                    Else              Begin LoadMod:=WrongFile;      exit; End;
            NT_MOD: Begin MaxSamples:=15; NrChannels:=4; Tag:=Tags[1].Tag; End;
            MK_MOD: Begin MaxSamples:=31; Tag:='----'; TrashedID:=TRUE;    End;
          End;
        End
      Else
        Begin MaxSamples:=31; Tag:='WOW'; End;
{Check if the file is compressed, decompress it before loading if so! -------}
    If PP20Decompress Then Begin LoadMOD:=LoadPP20Module; Exit; End;
{Now read the Header: -------------------------------------------------------}
    DummyB:=InitHeader;
    If      DummyB=255      then begin LoadMod:=WrongFile; Exit; end
    Else If DummyB<>NoError then begin LoadMod:=DummyB;    Exit; end;
{Now Load the patterns: -----------------------------------------------------}
    DummyB:=LoadPatterns;
    If      DummyB=255      then begin LoadMod:=WrongFile; Exit; end
    Else If DummyB<>NoError then begin LoadMod:=DummyB;    Exit; end;
  End;
{Now Load the samples: ------------------------------------------------------}
  Seek(MODFile, SampleDataOffset);
  For I:=1 to ModInfo.MaxSamples do
  Begin
    If ModInfo.Samples[I].Used then
      Begin
        DummyB:=PlayDevice.LoadMODSample(MODFile, I);
{Exit if not a Premature EOF error (? not really needed this one...) --------}
        If NOT (DummyB in [0, OutOfSampleData]) then
                                             Begin LoadMod:=DummyB; Exit; End;
        If ModInfo.Samples[I].RepeatSample then
          Begin
            dummyw:=ModInfo.Samples[I].RepeatOffset+ModInfo.Samples[I].RepeatLength;
            If dummyw>ModInfo.Samples[I].length then
              Begin
                dummyw:=ModInfo.Samples[I].length;
                ModInfo.Samples[I].RepeatSample:=False;
                ModInfo.Samples[I].RepeatOffset:=0;
                ModInfo.Samples[I].RepeatLength:=0;
              End;
            ModInfo.Samples[I].length:=dummyw;
          End
        Else
          Begin
            ModInfo.Samples[I].RepeatOffset:=0;
            ModInfo.Samples[I].RepeatLength:=0;
          End;
      End
    Else
      Seek(MODFile, FilePos(MODFile)+ModInfo.Samples[I].length);
  End;
{$IFDEF DEBUGINFO}
  Print('we are at ', FileSize(MODFile)-FilePos(MODFile), ' bytes from the EOF.'); Readln;
{$ENDIF}
  Close(MODFile); Loaded:=True;
  LoadMOD:=NoError;
END;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}
{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

END.
