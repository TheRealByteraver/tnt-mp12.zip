{
   ---------------------------------------
   Gravis Ultrasound driver - Version 2.11
   ---------------------------------------

   Copyright <C> 1993,1994 Ingenuity Software
   Copyright <C> 1993,1994 Advanced Gravis

   This driver unit can be used with either Turbo Pascal 6 or
   Turbo/Borland Pascal 7.  This unit was written in it's entirety
   by Kurt Kennett of Ingenuity Software (kurt_kennett@gravis.com).

   To use this unit, simply include "UltraDrv" in your USES statement
   at the top of your program.

   Please see the main UltraSound SDK manual for programming information.

   Version 7 users may also make use of the included TPH Help file by
   adding the file to the list of help files under their 'Help','Files..'
   menu item in the IDE.

   Please note that this unit will NOT compile under protected mode.
   Currently protected mode is not supported.

   Advanced Gravis, Ingenuity Software, and FORTE technologies
   disclaim all warranties with regards to this software, including
   any implied warranties of merchantibility and fitness.  In no
   event shall Advanced Gravis, Ingenuity Software, or FORTE
   technologies be liable for any damages whatsoever rising out of
   or in result of the use or performance of this software.

   Any additions or modifications to a copy of this file are a
   violation of copyright.  This file may not be distributed, in
   whole or in part, without the prior written consent of Advanced
   Gravis.

   Any additions or modifications to a copy of this file will NOT
   be supported by Advanced Gravis technical support.
}

UNIT UltraDrv;


{$IFDEF DPMI}
{* * * * Protected mode is NOT currently supported.}
{$ENDIF}

{$I-,N+,E-}
INTERFACE


TYPE
  PFV          = PROCEDURE;
  INT_PROC     = PROCEDURE(Voice : INTEGER);
  WORD_PROC    = PROCEDURE(MIDI_Send : WORD);
  TWOWORD_PROC = PROCEDURE(MIDI_Status : WORD; MIDI_Data : WORD);

  Ultra_CFG = RECORD
    Base_Port      : WORD;
    Dram_DMA_Chan  : WORD;
    ADC_DMA_Chan   : WORD;
    GF1_IRQ_Num    : WORD;
    MIDI_IRQ_Num   : WORD;
   END;

CONST
  { Common frequencies FOR sounds }
  Khz_11 = 11025;
  Khz_22 = 22050;
  Khz_44 = 44100;


  { Volume Control bit Values (OR together TO form Volume control BYTE) }
  Loop_Volume           = 08;
  Bi_Directional_Volume = 16;
  Enable_Volume_Handler = 32;


  { Voice Control bit Values (OR together TO form Voice control BYTE) }
  Voice_Data_16Bit        = 04;
  Loop_Voice              = 08;
  Bi_Directional_Voice    = 16;
  Enable_VoiceEnd_Handler = 32;


  { DMA control bit values (OR together TO form DMA control BYTE) }
  DMA_Data_16Bit = 64;
  Convert_Data   = 128;


  { ULTRASND environment variable readable OR NOT }
  Ultra_Installed : BOOLEAN = FALSE;


VAR
  Ultra_Config  : Ultra_CFG;
  UltraOk       : BOOLEAN;
  UltraError    : INTEGER;
  UltraErrorStr : STRING;


{ Base routines }

FUNCTION  UltraCalcRate(StartV   : WORD;
                        EndV     : WORD;
                        Mil_Secs : LONGINT) : BYTE;

FUNCTION  UltraClose : BOOLEAN;

FUNCTION  UltraDownLoad(DataPtr  : POINTER;
                        Control  : BYTE;
                        DRAM_Loc : LONGINT;
                        Len      : WORD;
                        Wait     : BOOLEAN) : BOOLEAN;

FUNCTION  UltraDRAMDMABusy : BOOLEAN;

FUNCTION  UltraGoRecord(Control : BYTE) : BOOLEAN;

PROCEDURE UltraGoVoice(Voice : INTEGER;
                       VMode : BYTE);

PROCEDURE UltraDisableLineIn;

PROCEDURE UltraDisableMicIn;

PROCEDURE UltraDisableOutput;

PROCEDURE UltraEnableLineIn;

PROCEDURE UltraEnableMicIn;

PROCEDURE UltraEnableOutput;

FUNCTION  UltraGetLineIn : BOOLEAN;

FUNCTION  UltraGetMicIn : BOOLEAN;

FUNCTION  UltraGetOutput : BOOLEAN;

PROCEDURE UltraDRAMTcHandler(VAR Handler : PFV);

PROCEDURE UltraMIDIXMitHandler(VAR Handler : WORD_PROC);

PROCEDURE UltraMIDIRecvHandler(VAR Handler : TWOWORD_PROC);

PROCEDURE UltraTimer1Handler(VAR Handler : PFV);

PROCEDURE UltraTimer2Handler(VAR Handler : PFV);

PROCEDURE UltraWaveHandler(VAR Handler : INT_PROC);

PROCEDURE UltraVolumeHandler(VAR Handler : INT_PROC);

PROCEDURE UltraRecordHandler(VAR Handler : PFV);

PROCEDURE UltraAuxHandler(VAR Handler : PFV);

FUNCTION  UltraMaxAvail : LONGINT;

FUNCTION  UltraMaxAlloc : LONGINT;

FUNCTION  UltraMemAvail : LONGINT;

FUNCTION  UltraMemAlloc(    Size     : LONGINT;
                        VAR Location : LONGINT) : BOOLEAN;

FUNCTION  UltraMemFree(Size     : LONGINT;
                       Location : LONGINT) : BOOLEAN;

PROCEDURE UltraMIDIDisableRecv;

PROCEDURE UltraMIDIDisableXmit;

PROCEDURE UltraMIDIEnableRecv;

PROCEDURE UltraMIDIEnableXmit;

FUNCTION  UltraMIDIRecv : BYTE;

PROCEDURE UltraMIDIReset;

FUNCTION  UltraMIDIStatus : BYTE;

PROCEDURE UltraMIDIXmit(data : BYTE);

FUNCTION  UltraOpen(VAR Config : Ultra_CFG;
                        Voices : INTEGER) : BOOLEAN;

FUNCTION  UltraPeekData(PPort   : INTEGER;
                        Address : LONGINT) : BYTE;

FUNCTION  UltraPing(PPort : WORD) : BOOLEAN;

PROCEDURE UltraPokeData(PPort   : INTEGER;
                        Address : LONGINT;
                        Data    : BYTE);

FUNCTION  UltraPrimeRecord(PC_PTR  : POINTER;
                           Size    : WORD;
                           RRepeat : BOOLEAN) : BOOLEAN;

FUNCTION  UltraPrimeVoice(Voice    : INTEGER;
                          VBegin   : LONGINT;
                          VStart   : LONGINT;
                          VEnd     : LONGINT;
                          VMode    : BYTE) : BYTE;

FUNCTION  UltraProbe(PPort : WORD) : BOOLEAN;

PROCEDURE UltraRampVolume(Voice  : INTEGER;
                          StartV : WORD;
                          EndV   : WORD;
                          VRate  : BYTE;
                          VMode  : BYTE);

FUNCTION  UltraReadRecordPosition : WORD;

FUNCTION  UltraReadVoice(Voice : INTEGER) : LONGINT;

FUNCTION  UltraReadVolume(Voice : INTEGER) : WORD;

FUNCTION  UltraRecordData(PC_PTR : POINTER;
                          Control : BYTE;
                          Size    : WORD;
                          Wait    : BOOLEAN;
                          RRepeat : BOOLEAN) : BOOLEAN;

FUNCTION  UltraRecordDMABusy : BOOLEAN;

FUNCTION  UltraReset(Voices : INTEGER) : BOOLEAN;

PROCEDURE UltraSetBalance(Voice : INTEGER;
                          Data  : BYTE);

PROCEDURE UltraSetFrequency(Voice     : INTEGER;
                            Speed_Khz : LONGINT);

PROCEDURE UltraSetLoopMode(Voice : INTEGER;
                           VMode : BYTE);

PROCEDURE UltraSetRecordFrequency(Rate : LONGINT);

PROCEDURE UltraSetVoice(Voice    : INTEGER;
                        Location : LONGINT);

PROCEDURE UltraSetVoiceEnd(Voice    : INTEGER;
                           VEnd     : LONGINT);

PROCEDURE UltraSetVolume(Voice  : INTEGER;
                         Volume : WORD);

FUNCTION  UltraSizeDRAM : INTEGER;

PROCEDURE UltraStartTimer(Timer : INTEGER;
                          Time  : BYTE);

PROCEDURE UltraStartVoice(Voice  : INTEGER;
                          VBegin : LONGINT;
                          VStart : LONGINT;
                          VEnd   : LONGINT;
                          VMode  : BYTE);

PROCEDURE UltraStopTimer(Timer : INTEGER);

PROCEDURE UltraStopVoice(Voice : INTEGER);

PROCEDURE UltraStopVolume(Voice : INTEGER);

FUNCTION  UltraTimerStopped(Timer : INTEGER) : BOOLEAN;

PROCEDURE UltraTrimJoystick(JoyVal : BYTE);

FUNCTION  UltraUpLoad(DataPtr  : POINTER;
                      Control  : BYTE;
                      DRAM_Loc : LONGINT;
                      Len      : WORD;
                      Wait     : BOOLEAN) : BOOLEAN;

PROCEDURE UltraVectorVolume(Voice : INTEGER;
                            VEnd  : WORD;
                            VRate : BYTE;
                            VMode : BYTE);

PROCEDURE UltraVersion(VAR Major : BYTE;
                       VAR Minor : BYTE);

FUNCTION  UltraVersionStr : STRING;

FUNCTION  UltraVoiceStopped(Voice : INTEGER) : BOOLEAN;

FUNCTION  UltraVolumeStopped(Voice : INTEGER) : BOOLEAN;

PROCEDURE UltraWaitDRAMDMA;

PROCEDURE UltraWaitRecordDMA;

PROCEDURE UltraVoiceOff(Voice : INTEGER;
                        VEnd  : BOOLEAN);

PROCEDURE UltraVoiceOn(Voice : INTEGER;
                       VBegin : LONGINT;
                       Start_Loop : LONGINT;
                       END_Loop : LONGINT;
                       Control  : BYTE;
                       Freq     : LONGINT);

PROCEDURE UltraSetLinearVolume(Voice : INTEGER;
                               INDEX : INTEGER);

FUNCTION  UltraReadLinearVolume(Voice : INTEGER) : INTEGER;

PROCEDURE UltraRampLinearVolume(Voice     : INTEGER;
                                Start_Idx : WORD;
                                END_Idx   : WORD;
                                Msecs     : LONGINT;
                                VMode     : BYTE);

PROCEDURE UltraVectorLinearVolume(Voice   : INTEGER;
                                  END_Idx : WORD;
                                  VRate   : BYTE;
                                  VMode   : BYTE);

PROCEDURE UltraClearVoices;

FUNCTION  UltraAllocVoice(VAR Voice_Num : INTEGER) : BOOLEAN;

PROCEDURE UltraFreeVoice(Voice_Num : INTEGER);



{ 3-D routines }

IMPLEMENTATION

USES
  NewCrt,
  DOS;

CONST
  { Error codes }
  ULTRA_OK             = 1;
  BAD_NUM_OF_Voices    = 2;
  NO_MEMORY            = 3;
  CORRUPT_MEM          = 4;
  NO_Ultra             = 5;
  DMA_BUSY             = 6;
  BAD_DMA_ADDR         = 7;
  BAD_ENV_VAR          = 8;
  VOICE_NOT_VALID      = 9;
  NO_FREE_VOICES       = 10;
  VOICE_ALREADY_USED   = 11;
  FILE_NOT_FOUND       = 12;
  INSUFFICIENT_GUS_MEM = 13;
  ErrorStrings : ARRAY[1..13] OF STRING[40] = ('No Error.',
                                          {02} 'Bad # of Voices',
                                          {03} 'No Memory!',
                                          {04} 'Memory Structures Corrupt',
                                          {05} 'No UltraSound found',
                                          {06} 'DMA is busy',
                                          {07} 'BAD DMA Address used',
                                          {08} 'ULTRASND Environment Variable Unreadable',
                                          {09} 'Voice Specified is out of range',
                                          {10} 'No more free Voices',
                                          {11} 'Specified Voice already in use',
                                          {12} 'File NOT Found',
                                          {13} 'Insufficient GUS Memory');

  JOYSTICK_TIMER  = $201;  (* 201 *)
  JOYSTICK_DATA   = $201;  (* 201 *)

  GF1_MIDI_CTRL   = $100;  (* 3X0 *)
  GF1_MIDI_DATA   = $101;  (* 3X1 *)

  GF1_PAGE        = $102;  (* 3X2 *)
  GF1_REG_SELECT  = $103;  (* 3X3 *)
  GF1_DATA_LOW    = $104;  (* 3X4 *)
  GF1_DATA_HI     = $105;  (* 3X5 *)
  GF1_IRQ_STAT    = $006;  (* 2X6 *)
  GF1_DRAM        = $107;  (* 3X7 *)

  GF1_MIX_CTRL    = $000;  (* 2X0 *)
  GF1_TIMER_CTRL  = $008;  (* 2X8 *)
  GF1_TIMER_DATA  = $009;  (* 2X9 *)
  GF1_IRQ_CTRL    = $00B;  (* 2XB *)

  (* The GF1 Hardware clock. *)
  Clock_Rate      = 9878400;

  (* Mixer control bits. *)
  ENABLE_LINE		= $01;
  ENABLE_DAC		= $02;
  ENABLE_MIC		= $04;

  (* interrupt controller 1 *)
  CNTRL_8259		= $21;
  OCR_8259		= $20;
  EOI			= $20;
  REARM3		= $2F3;
  REARM5		= $2F5;

  (* interrupt controller 2 *)
  CNTRL_M_8259	        = $21;
  CNTRL_M2_8259	        = $A1;
  OCR_2_8259		= $A0;

  (* DMA *)
  DMA_CONTROL		= $41;
  SET_DMA_ADDRESS	= $42;
  SET_DRAM_LOW		= $43;
  SET_DRAM_HIGH		= $44;

  (* TIMER *)
  TIMER_CONTROL		= $45;
  TIMER1		= $46;
  TIMER2		= $47;

  (* SAMPLING *)
  SET_SAMPLE_RATE	= $48;
  SAMPLE_CONTROL	= $49;

  (* MISC *)
  SET_JOYSTICK		= $4B;
  MASTER_RESET		= $4C;

  (* Voice register mapping. *)
  SET_CONTROL		= $00;
  SET_FREQUENCY		= $01;
  SET_START_HIGH	= $02;
  SET_START_LOW		= $03;
  SET_END_HIGH		= $04;
  SET_END_LOW		= $05;
  SET_VOLUME_RATE	= $06;
  SET_VOLUME_START	= $07;
  SET_VOLUME_END	= $08;
  SET_VOLUME		= $09;
  SET_ACC_HIGH		= $0a;
  SET_ACC_LOW		= $0b;
  SET_BALANCE		= $0c;
  SET_VOLUME_CONTROL 	= $0d;
  SET_VOICES		= $0e;

  GET_CONTROL		= $80;
  GET_FREQUENCY		= $81;
  GET_START_HIGH	= $82;
  GET_START_LOW		= $83;
  GET_END_HIGH		= $84;
  GET_END_LOW		= $85;
  GET_VOLUME_RATE	= $86;
  GET_VOLUME_START	= $87;
  GET_VOLUME_END	= $88;
  GET_VOLUME		= $89;
  GET_ACC_HIGH		= $8a;
  GET_ACC_LOW		= $8b;
  GET_BALANCE		= $8c;
  GET_VOLUME_CONTROL 	= $8d;
  GET_VOICES		= $8e;
  GET_IRQV		= $8f;

  (* MIDI *)
  MIDI_RESET	        = $03;
  MIDI_ENABLE_XMIT	= $20;
  MIDI_ENABLE_RCV	= $80;
  MIDI_RCV_FULL		= $01;
  MIDI_XMIT_EMPTY	= $02;
  MIDI_FRAME_ERR	= $10;
  MIDI_OVERRUN		= $20;
  MIDI_IRQ_PEND		= $80;

  (* JOYSTICK *)
  JOY_POSITION		= $0f;
  JOY_BUTTONS		= $f0;

  (* GF1_IRQ_STATUS (PORT 3X6) *)
  MIDI_TX_IRQ		= $01;
  MIDI_RX_IRQ		= $02;
  GF1_TIMER1_IRQ	= $04;
  GF1_TIMER2_IRQ	= $08;
  WAVETABLE_IRQ		= $20;
  ENVELOPE_IRQ		= $40;
  DMA_TC_IRQ		= $80;

  (* GF1_MIX_CTRL (PORT 2X0) *)
  ENABLE_LINE_IN	= $01;
  ENABLE_OUTPUT		= $02;
  ENABLE_MIC_IN		= $04;
  ENABLE_GF1_IRQ	= $08;
  GF122			= $10;
  ENABLE_MIDI_LOOP	= $20;
  SELECT_GF1_REG	= $40;

  (* DMA control register *)
  DMA_ENABLE		= $01;
  DMA_READ		= $02;
  DMA_WIDTH_16		= $04;
  DMA_RATE		= $18;
  DMA_IRQ_ENABLE	= $20;
  DMA_IRQ_PENDING	= $40;
  DMA_DATA_16		= $40;
  DMA_TWOS_COMP		= $80;

  (* These are the xfer rate bits ... *)
  DMA_R0		= $00;
  DMA_R1		= $08;
  DMA_R2		= $10;
  DMA_R3		= $18;

  (* SAMPLE control register *)
  ENABLE_ADC		= $01;
  ADC_MODE		= $02;
  ADC_DMA_WIDTH		= $04;
  ADC_IRQ_ENABLE	= $20;
  ADC_IRQ_PENDING	= $40;
  ADC_TWOS_COMP		= $80;

  (* RESET control register *)
  GF1_MASTER_RESET	= $01;
  GF1_OUTPUT_ENABLE	= $02;
  GF1_MASTER_IRQ	= $04;

  (* Voice specific registers *)
  VOICE_STOPPED		= $01;
  STOP_VOICE		= $02;
  VC_DATA_TYPE		= $04;
  VC_LOOP_ENABLE	= $08;
  VC_BI_LOOP		= $10;
  VC_WAVE_IRQ		= $20;
  VC_DIRECT		= $40;
  VC_IRQ_PENDING	= $80;

  (* Volume specific registers *)
  VL_RATE_MANTISSA	= $3f;
  VL_RATE_RANGE		= $C0;
  VL_START_MANT	        = $0F;
  VL_START_EXP		= $F0;
  VL_END_MANT		= $0F;
  VL_END_EXP		= $F0;

  (* Volume control register *)
  VOLUME_STOPPED	= $01;
  STOP_VOLUME		= $02;
  VC_ROLLOVER           = $04;
  VL_LOOP_ENABLE	= $08;
  VL_BI_LOOP		= $10;
  VL_WAVE_IRQ		= $20;
  VL_DIRECT		= $40;
  VL_IRQ_PENDING	= $80;

  (* Voice IRQ *)
  VOICE_VOLUME_IRQ	= $40;
  VOICE_WAVE_IRQ	= $80;

  (* Memory / Misc *)
  DMA_AUTO_INIT   = $01;
  DMA_16          = $40;
  DMA_8           = $00;
  DMA_CVT_2       = $80;
  DMA_NO_CVT      = $00;
  USE_ROLLOVER    = $01;

  ULTRA_PRESENT	  = $0001;
  DRAM_DMA_BUSY   = $0002;
  ADC_DMA_BUSY    = $0004;
  DRAM_DMA_NOWAIT = $0008;
  ADC_DMA_NOWAIT  = $0010;

  READ_DMA        = 1;
  WRITE_DMA       = 2;
  INDEF_READ      = 3;
  INDEF_WRITE     = 4;

  (* DMA Controler #1 (8-bit controller) *)
  DMA1_STAT	= $08;
  DMA1_WCMD	= $08;
  DMA1_WREQ	= $09;
  DMA1_SNGL	= $0A;
  DMA1_MODE	= $0B;
  DMA1_CLRFF	= $0C;
  DMA1_MCLR	= $0D;
  DMA1_CLRM	= $0E;
  DMA1_WRTALL	= $0F;
  (* DMA Controler #2 (16-bit controller) *)
  DMA2_STAT	= $D0;
  DMA2_WCMD	= $D0;
  DMA2_WREQ	= $D2;
  DMA2_SNGL	= $D4;
  DMA2_MODE	= $D6;
  DMA2_CLRFF	= $D8;
  DMA2_MCLR	= $DA;
  DMA2_CLRM	= $DC;
  DMA2_WRTALL	= $DE;

  DMA0_ADDR	= $00;
  DMA0_CNT	= $01;
  DMA1_ADDR	= $02;
  DMA1_CNT	= $03;
  DMA2_ADDR	= $04;
  DMA2_CNT	= $05;
  DMA3_ADDR	= $06;
  DMA3_CNT	= $07;
  DMA4_ADDR	= $C0;
  DMA4_CNT	= $C2;
  DMA5_ADDR	= $C4;
  DMA5_CNT	= $C6;
  DMA6_ADDR	= $C8;
  DMA6_CNT	= $CA;
  DMA7_ADDR	= $CC;
  DMA7_CNT	= $CE;
  DMA0_PAGE	= $87;
  DMA1_PAGE	= $83;
  DMA2_PAGE	= $81;
  DMA3_PAGE	= $82;
  DMA4_PAGE	= $8F;
  DMA5_PAGE	= $8B;
  DMA6_PAGE	= $89;
  DMA7_PAGE	= $8A;

  MAX_DMA       = 7;
  DMA_DECREMENT	= $20;

  (* Bits FOR dma flags location *)
  DMA_USED	= $0001;
  DMA_PENDING   = $0002;
  TWO_FLAG	= $0004;
  REV_FLAG	= $0008;
  CALIB_COUNT   = $0010;

  (* IRQ constants *)
  MAX_IRQ       = 16;
  IRQ_UNAVAIL   = $0000;
  IRQ_AVAIL     = $0001;
  IRQ_USED      = $0002;
  OCR1          = $20;
  IMR1          = $21;
  OCR2          = $A0;
  IMR2          = $A1;

  { For memory control scheme }
  MaxNumBanks = 4;
  BlockSizeK  = 256;
  OneK        = 1024;
  UMemInited  : BOOLEAN = FALSE;


TYPE
  ULTRA_DATA = RECORD
     Flags               : WORD;
     Base_Port           : WORD;
     DRAM_DMA_Chan       : WORD;
     ADC_DMA_Chan        : WORD;
     GF1_IRQ_Num         : WORD;
     MIDI_IRQ_Num        : WORD;
     Old_GF1_Vec         : PFV; { Interrupt }
     Old_MIDI_Vec        : PFV; { Interrupt }
     MIDI_XMit_Func      : WORD_Proc;
     MIDI_Recv_Func      : TwoWord_Proc;
     Timer1_Func         : PFV;
     Timer2_Func         : PFV;
     WaveTable_Func      : Int_Proc;
     Volume_Func         : Int_Proc;
     DRAM_DMA_TC_Func    : PFV;
     RECORD_DMA_TC_Func  : PFV;
     Aux_IRQ_Func        : PFV;
     Mix_Image           : BYTE;
     Voices              : BYTE;
     Image_MIDI          : BYTE;
     Used_Voices         : LONGINT;
     Timer_Ctrl          : BYTE;
     Timer_Mask          : BYTE;
     MIDI_Data           : INTEGER;
     MIDI_Control        : INTEGER;
     Voice_Select        : INTEGER;
     Reg_Select          : INTEGER;
     Data_LOW            : INTEGER;
     Data_Hi             : INTEGER;
     IRQ_Status          : INTEGER;
     DRAM_Data           : INTEGER;
     Mix_Control         : INTEGER;
     IRQ_Control         : INTEGER;
     Timer_Control       : INTEGER;
     Timer_Data          : INTEGER;
     Ultra_ErrNo         : INTEGER;
     GF1_SEMA4           : INTEGER;
     IRQ_Pending         : INTEGER;
   END;


  DMA_Entry = RECORD
    Flags       : WORD;
    Latch       : WORD;
    DMA_Disable : WORD;
    DMA_Enable  : WORD;
    Page        : WORD;
    Addr        : WORD;
    Count       : WORD;
    SINGLE      : WORD;
    Mode        : WORD;
    Clear_FF    : WORD;
    WriteTrans  : WORD;
    ReadTrans   : WORD;
    Cur_Mode    : BYTE;

    Cur_Page    : WORD;
    Cur_Addr    : WORD;
    Amnt_Sent   : WORD;
    Cur_Size    : WORD;
    Nxt_Page    : WORD;
    Nxt_Addr    : WORD;
    Nxt_Size    : WORD;

    Cur_Control : BYTE;
   END;

  IRQ_ENTRY = RECORD
     Latch    : BYTE;
     Mask     : BYTE;
     Spec_EOI : BYTE;
     OCR      : BYTE;
     IMR      : BYTE;
   END;

  { FOR new memory control scheme }
  PNode = ^Node;
  Node = RECORD
    StartLoc : LONGINT;
    EndLoc   : LONGINT;
    Prev,
    Next     : PNode;
   END;

  FreeList = RECORD
    BaseOffset : LONGINT;
    List       : PNode;
   END;


CONST
  IMAGE_MIDI : BYTE = 0;
  _GF1_Volumes : ARRAY[0..511] OF WORD = ($0000,
       $0700, $07FF, $0880, $08FF, $0940, $0980, $09C0, $09FF, $0A20,
       $0A40, $0A60, $0A80, $0AA0, $0AC0, $0AE0, $0AFF, $0B10, $0B20,
       $0B30, $0B40, $0B50, $0B60, $0B70, $0B80, $0B90, $0BA0, $0BB0,
       $0BC0, $0BD0, $0BE0, $0BF0, $0BFF, $0C08, $0C10, $0C18, $0C20,
       $0C28, $0C30, $0C38, $0C40, $0C48, $0C50, $0C58, $0C60, $0C68,
       $0C70, $0C78, $0C80, $0C88, $0C90, $0C98, $0CA0, $0CA8, $0CB0,
       $0CB8, $0CC0, $0CC8, $0CD0, $0CD8, $0CE0, $0CE8, $0CF0, $0CF8,
       $0CFF, $0D04, $0D08, $0D0C, $0D10, $0D14, $0D18, $0D1C, $0D20,
       $0D24, $0D28, $0D2C, $0D30, $0D34, $0D38, $0D3C, $0D40, $0D44,
       $0D48, $0D4C, $0D50, $0D54, $0D58, $0D5C, $0D60, $0D64, $0D68,
       $0D6C, $0D70, $0D74, $0D78, $0D7C, $0D80, $0D84, $0D88, $0D8C,
       $0D90, $0D94, $0D98, $0D9C, $0DA0, $0DA4, $0DA8, $0DAC, $0DB0,
       $0DB4, $0DB8, $0DBC, $0DC0, $0DC4, $0DC8, $0DCC, $0DD0, $0DD4,
       $0DD8, $0DDC, $0DE0, $0DE4, $0DE8, $0DEC, $0DF0, $0DF4, $0DF8,
       $0DFC, $0DFF, $0E02, $0E04, $0E06, $0E08, $0E0A, $0E0C, $0E0E,
       $0E10, $0E12, $0E14, $0E16, $0E18, $0E1A, $0E1C, $0E1E, $0E20,
       $0E22, $0E24, $0E26, $0E28, $0E2A, $0E2C, $0E2E, $0E30, $0E32,
       $0E34, $0E36, $0E38, $0E3A, $0E3C, $0E3E, $0E40, $0E42, $0E44,
       $0E46, $0E48, $0E4A, $0E4C, $0E4E, $0E50, $0E52, $0E54, $0E56,
       $0E58, $0E5A, $0E5C, $0E5E, $0E60, $0E62, $0E64, $0E66, $0E68,
       $0E6A, $0E6C, $0E6E, $0E70, $0E72, $0E74, $0E76, $0E78, $0E7A,
       $0E7C, $0E7E, $0E80, $0E82, $0E84, $0E86, $0E88, $0E8A, $0E8C,
       $0E8E, $0E90, $0E92, $0E94, $0E96, $0E98, $0E9A, $0E9C, $0E9E,
       $0EA0, $0EA2, $0EA4, $0EA6, $0EA8, $0EAA, $0EAC, $0EAE, $0EB0,
       $0EB2, $0EB4, $0EB6, $0EB8, $0EBA, $0EBC, $0EBE, $0EC0, $0EC2,
       $0EC4, $0EC6, $0EC8, $0ECA, $0ECC, $0ECE, $0ED0, $0ED2, $0ED4,
       $0ED6, $0ED8, $0EDA, $0EDC, $0EDE, $0EE0, $0EE2, $0EE4, $0EE6,
       $0EE8, $0EEA, $0EEC, $0EEE, $0EF0, $0EF2, $0EF4, $0EF6, $0EF8,
       $0EFA, $0EFC, $0EFE, $0EFF, $0F01, $0F02, $0F03, $0F04, $0F05,
       $0F06, $0F07, $0F08, $0F09, $0F0A, $0F0B, $0F0C, $0F0D, $0F0E,
       $0F0F, $0F10, $0F11, $0F12, $0F13, $0F14, $0F15, $0F16, $0F17,
       $0F18, $0F19, $0F1A, $0F1B, $0F1C, $0F1D, $0F1E, $0F1F, $0F20,
       $0F21, $0F22, $0F23, $0F24, $0F25, $0F26, $0F27, $0F28, $0F29,
       $0F2A, $0F2B, $0F2C, $0F2D, $0F2E, $0F2F, $0F30, $0F31, $0F32,
       $0F33, $0F34, $0F35, $0F36, $0F37, $0F38, $0F39, $0F3A, $0F3B,
       $0F3C, $0F3D, $0F3E, $0F3F, $0F40, $0F41, $0F42, $0F43, $0F44,
       $0F45, $0F46, $0F47, $0F48, $0F49, $0F4A, $0F4B, $0F4C, $0F4D,
       $0F4E, $0F4F, $0F50, $0F51, $0F52, $0F53, $0F54, $0F55, $0F56,
       $0F57, $0F58, $0F59, $0F5A, $0F5B, $0F5C, $0F5D, $0F5E, $0F5F,
       $0F60, $0F61, $0F62, $0F63, $0F64, $0F65, $0F66, $0F67, $0F68,
       $0F69, $0F6A, $0F6B, $0F6C, $0F6D, $0F6E, $0F6F, $0F70, $0F71,
       $0F72, $0F73, $0F74, $0F75, $0F76, $0F77, $0F78, $0F79, $0F7A,
       $0F7B, $0F7C, $0F7D, $0F7E, $0F7F, $0F80, $0F81, $0F82, $0F83,
       $0F84, $0F85, $0F86, $0F87, $0F88, $0F89, $0F8A, $0F8B, $0F8C,
       $0F8D, $0F8E, $0F8F, $0F90, $0F91, $0F92, $0F93, $0F94, $0F95,
       $0F96, $0F97, $0F98, $0F99, $0F9A, $0F9B, $0F9C, $0F9D, $0F9E,
       $0F9F, $0FA0, $0FA1, $0FA2, $0FA3, $0FA4, $0FA5, $0FA6, $0FA7,
       $0FA8, $0FA9, $0FAA, $0FAB, $0FAC, $0FAD, $0FAE, $0FAF, $0FB0,
       $0FB1, $0FB2, $0FB3, $0FB4, $0FB5, $0FB6, $0FB7, $0FB8, $0FB9,
       $0FBA, $0FBB, $0FBC, $0FBD, $0FBE, $0FBF, $0FC0, $0FC1, $0FC2,
       $0FC3, $0FC4, $0FC5, $0FC6, $0FC7, $0FC8, $0FC9, $0FCA, $0FCB,
       $0FCC, $0FCD, $0FCE, $0FCF, $0FD0, $0FD1, $0FD2, $0FD3, $0FD4,
       $0FD5, $0FD6, $0FD7, $0FD8, $0FD9, $0FDA, $0FDB, $0FDC, $0FDD,
       $0FDE, $0FDF, $0FE0, $0FE1, $0FE2, $0FE3, $0FE4, $0FE5, $0FE6,
       $0FE7, $0FE8, $0FE9, $0FEA, $0FEB, $0FEC, $0FED, $0FEE, $0FEF,
       $0FF0, $0FF1, $0FF2, $0FF3, $0FF4, $0FF5, $0FF6, $0FF7, $0FF8,
       $0FF9, $0FFA, $0FFB, $0FFC, $0FFD, $0FFE, $0FFF);

  VOL_Rates : ARRAY[0..18] OF WORD = (23 , { 14 voices }
                                      24 , { 15 voices }
                                      26 , { 16 voices }
                                      28 , { 17 voices }
                                      29 , { 18 voices }
                                      31 , { 19 voices }
                                      32 , { 20 voices }
                                      34 , { 21 voices }
                                      36 , { 22 voices }
                                      37 , { 23 voices }
                                      39 , { 24 voices }
                                      40 , { 25 voices }
                                      42 , { 26 voices }
                                      44 , { 27 voices }
                                      45 , { 28 voices }
                                      47 , { 29 voices }
                                      49 , { 30 voices }
                                      50 , { 31 voices }
                                      52); { 32 voices }

  Freq_Divisor : ARRAY[0..18] OF WORD = (44100,   { 14 active voices }
                                         41160,   { 15 active voices }
                                         38587,   { 16 active voices }
                                         36317,   { 17 active voices }
                                         34300,   { 18 active voices }
                                         32494,   { 19 active voices }
                                         30870,   { 20 active voices }
                                         29400,   { 21 active voices }
                                         28063,   { 22 active voices }
                                         26843,   { 23 active voices }
                                         25725,   { 24 active voices }
                                         24696,   { 25 active voices }
                                         23746,   { 26 active voices }
                                         22866,   { 27 active voices }
                                         22050,   { 28 active voices }
                                         21289,   { 29 active voices }
                                         20580,   { 30 active voices }
                                         19916,   { 31 active voices }
                                         19293);  { 32 active voices }

  _GF1_IRQ  : ARRAY[0..MAX_IRQ-1] OF IRQ_ENTRY =
              ((Latch    : 0;     {  0 }
                Mask     : $FE;
                Spec_EOI : $60;
                OCR      : OCR1;
                IMR      : IMR1),
               (Latch    : 0;     {  1 }
                Mask     : $FD;
                Spec_EOI : $61;
                OCR      : OCR1;
                IMR      : IMR1),
               (Latch    : 1;     {  2 }
                Mask     : $FB;
                Spec_EOI : $62;
                OCR      : OCR1;
                IMR      : IMR1),
               (Latch    : 3;     {  3 }
                Mask     : $F7;
                Spec_EOI : $63;
                OCR      : OCR1;
                IMR      : IMR1),
               (Latch    : 0;     {  4 }
                Mask     : $EF;
                Spec_EOI : $64;
                OCR      : OCR1;
                IMR      : IMR1),
               (Latch    : 2;     {  5 }
                Mask     : $DF;
                Spec_EOI : $65;
                OCR      : OCR1;
                IMR      : IMR1),
               (Latch    : 0;     {  6 }
                Mask     : $BF;
                Spec_EOI : $66;
                OCR      : OCR1;
                IMR      : IMR1),
               (Latch    : 4;     {  7 }
                Mask     : $7F;
                Spec_EOI : $67;
                OCR      : OCR1;
                IMR      : IMR1),
               (Latch    : 0;     {  8 }
                Mask     : $FE;
                Spec_EOI : $60;
                OCR      : OCR2;
                IMR      : IMR2),
               (Latch    : 0;     {  9 }
                Mask     : $FD;
                Spec_EOI : $61;
                OCR      : OCR2;
                IMR      : IMR2),
               (Latch    : 0;     { 10 }
                Mask     : $FB;
                Spec_EOI : $62;
                OCR      : OCR2;
                IMR      : IMR2),
               (Latch    : 5;     { 11 }
                Mask     : $F7;
                Spec_EOI : $63;
                OCR      : OCR2;
                IMR      : IMR2),
               (Latch    : 6;     { 12 }
                Mask     : $EF;
                Spec_EOI : $64;
                OCR      : OCR2;
                IMR      : IMR2),
               (Latch    : 0;     { 13 }
                Mask     : $DF;
                Spec_EOI : $65;
                OCR      : OCR2;
                IMR      : IMR2),
               (Latch    : 0;     { 14 }
                Mask     : $BF;
                Spec_EOI : $66;
                OCR      : OCR2;
                IMR      : IMR2),
               (Latch    : 7;     { 15 }
                Mask     : $7F;
                Spec_EOI : $67;
                OCR      : OCR2;
                IMR      : IMR2));

  _GF1_DMA  : ARRAY[0..MAX_DMA-1] OF DMA_ENTRY =
              ((Flags       : 0;           { Channel 1 }
                Latch       : 1;
                DMA_Disable : $05;
                DMA_Enable  : $01;
                Page        : DMA1_PAGE;
                Addr        : DMA1_ADDR;
                Count       : DMA1_CNT;
                SINGLE      : DMA1_SNGL;
                Mode        : DMA1_MODE;
                Clear_FF    : DMA1_CLRFF;
                WriteTrans  : $49;
                ReadTrans   : $45),
               (Flags       : 0;           { Channel 2 }
                Latch       : 0;
                DMA_Disable : $06;
                DMA_Enable  : $02;
                Page        : DMA2_PAGE;
                Addr        : DMA2_ADDR;
                Count       : DMA2_CNT;
                SINGLE      : DMA1_SNGL;
                Mode        : DMA1_MODE;
                Clear_FF    : DMA1_CLRFF;
                WriteTrans  : $4A;
                ReadTrans   : $46),
               (Flags       : 0;           { Channel 3 }
                Latch       : 2;
                DMA_Disable : $07;
                DMA_Enable  : $03;
                Page        : DMA3_PAGE;
                Addr        : DMA3_ADDR;
                Count       : DMA3_CNT;
                SINGLE      : DMA1_SNGL;
                Mode        : DMA1_MODE;
                Clear_FF    : DMA1_CLRFF;
                WriteTrans  : $4B;
                ReadTrans   : $47),
               (Flags       : 0;           { Channel 4 }
                Latch       : 0;
                DMA_Disable : $04;
                DMA_Enable  : $00;
                Page        : DMA4_PAGE;
                Addr        : DMA4_ADDR;
                Count       : DMA4_CNT;
                SINGLE      : DMA2_SNGL;
                Mode        : DMA2_MODE;
                Clear_FF    : DMA2_CLRFF;
                WriteTrans  : $48;
                ReadTrans   : $44),
               (Flags       : 0;           { Channel 5 }
                Latch       : 3;
                DMA_Disable : $05;
                DMA_Enable  : $01;
                Page        : DMA5_PAGE;
                Addr        : DMA5_ADDR;
                Count       : DMA5_CNT;
                SINGLE      : DMA2_SNGL;
                Mode        : DMA2_MODE;
                Clear_FF    : DMA2_CLRFF;
                WriteTrans  : $49;
                ReadTrans   : $45),
               (Flags       : 0;           { Channel 6 }
                Latch       : 4;
                DMA_Disable : $06;
                DMA_Enable  : $02;
                Page        : DMA6_PAGE;
                Addr        : DMA6_ADDR;
                Count       : DMA6_CNT;
                SINGLE      : DMA2_SNGL;
                Mode        : DMA2_MODE;
                Clear_FF    : DMA2_CLRFF;
                WriteTrans  : $4A;
                ReadTrans   : $46),
               (Flags       : 0;           { Channel 7 }
                Latch       : 5;
                DMA_Disable : $07;
                DMA_Enable  : $03;
                Page        : DMA7_PAGE;
                Addr        : DMA7_ADDR;
                Count       : DMA7_CNT;
                SINGLE      : DMA2_SNGL;
                Mode        : DMA2_MODE;
                Clear_FF    : DMA2_CLRFF;
                WriteTrans  : $4B;
                ReadTrans   : $47));


{ ---------------------------------------------------------- }

VAR
  GUSData   : ULTRA_DATA;
  UMemBlock : ARRAY[1..MaxNumBanks] OF FreeList;
  UMemStruc : LONGINT;


{ ---------------------------------------------------------- }

PROCEDURE ClearError;
  BEGIN
    UltraOk := TRUE;
    UltraError := ULTRA_OK;
    UltraErrorStr := ErrorStrings[ULTRA_OK];
  END;

PROCEDURE Default_Proc; FAR;
  BEGIN
    { Dummy PROCEDURE that does NOThing }
  END;

PROCEDURE Default_Int_Proc(V : INTEGER); FAR;
  BEGIN
    { Dummy PROCEDURE that does NOThing }
  END;

PROCEDURE Default_WORD_Proc(M : WORD); FAR;
  BEGIN
    { Dummy PROCEDURE that does NOThing }
  END;

PROCEDURE Default_TwoWord_Proc(MS : WORD; MD : WORD); FAR;
  BEGIN
    { Dummy PROCEDURE that does NOThing }
  END;

{ ---------------------------------------------------------- }

FUNCTION Make_MS_WORD(x : WORD) : LONGINT;
  BEGIN
    Make_MS_WORD := (LONGINT(X) SHL 16);
  END;

FUNCTION LSW(x : LONGINT) : WORD;
  BEGIN
    LSW := WORD(X);
  END;

FUNCTION MSW(x : LONGINT) : WORD;
  BEGIN
    MSW := WORD(X SHR 16);
  END;

FUNCTION MSB(x : WORD) : BYTE;
  BEGIN
    MSB := BYTE(X SHR 8);
  END;

FUNCTION LSB(x : WORD) : BYTE;
  BEGIN
    LSB := BYTE(x);
  END;

(* Make GF1 address FOR direct chip i/o. *)
FUNCTION Addr_HIGH(x : LONGINT) : WORD;
  BEGIN
    Addr_HIGH := WORD(LONGINT(X SHR LONGINT(7)) AND LONGINT($1FFF));
  END;

FUNCTION Addr_LOW(x : LONGINT) : WORD;
  BEGIN
    Addr_LOW := WORD(LONGINT(X AND LONGINT($7F)) SHL 9);
  END;

FUNCTION UltraPeekData(PPort   : INTEGER;
                       Address : LONGINT) : BYTE;
  BEGIN
    ASM
      PUSHF
      CLI
    END;
    PORT[PPort+GF1_REG_Select] := SET_DRAM_LOW;   (* 16 bits *)
    PORTW[PPort+GF1_DATA_LOW] := WORD(Address);
    PORT[PPort+GF1_REG_Select] := SET_DRAM_HIGH;  (* 8 bits *)
    PORT[PPort+GF1_DATA_HI] := BYTE(Address SHR 16);
    UltraPeekData := PORT[PPort+GF1_DRAM];
    ASM
      POPF
    END;
    ClearError;
  END;


PROCEDURE UltraPokeData(PPort   : INTEGER;
                        Address : LONGINT;
                        Data    : BYTE);
  BEGIN
    PORT[PPort+GF1_REG_Select] := SET_DRAM_LOW;   (* 16 bits *)
    PORTW[PPort+GF1_DATA_LOW] := WORD(Address);
    PORT[PPort+GF1_REG_Select] := SET_DRAM_HIGH;  (* 8 bits *)
    PORT[PPort+GF1_DATA_HI] := BYTE(Address SHR 16);
    PORT[PPort+GF1_DRAM] := Data;
    ClearError;
  END;


PROCEDURE UltraVersion(VAR Major : BYTE;
                       VAR Minor : BYTE);
  BEGIN
    Major := 2;
    Minor := 11;
    ClearError;
  END;

FUNCTION UltraVersionStr : STRING;
  VAR
    Maj, Min : BYTE;
    SVer, SVer2 : STRING[4];
  BEGIN
    UltraVersion(Maj, Min);
    Str(Maj, SVer);
    Str(Min, SVer2);
    IF length(SVer2) < 2 THEN
      SVer2 := '0' + SVer2;
    UltraVersionStr := SVer + '.' + SVer2 + 'P';
    { Error cleared by UltraVersion }
  END;


PROCEDURE GF1_Delay;
{ This FUNCTION is used as a 1.6*3 microsecond (OR longer) delay.
  This is needed when trying TO change any OF the 'self-modifying'
  bits in the voice registers. }
  VAR
    I : INTEGER;
    M : BYTE;
  BEGIN
    FOR I := 0 TO 6 DO
      M := PORT[GUSData.DRAM_data];
  END;


FUNCTION Make_Physical_Address(LOW  : WORD;
                               HIGH : WORD;
                               Mode : BYTE) : LONGINT;
{ This FUNCTION will convert the value read from the GF1 registers
  back TO a 'real' address. }
  VAR
    Lower_16,
    Upper_16 : WORD;
    Ret_Address,
    Bit_19_20 : LONGINT;
  BEGIN
    Upper_16 := HIGH SHR 9;
    Lower_16 := ((HIGH AND $01FF) SHL 7) OR ((LOW SHR 9) AND $007F);

    Ret_Address := Make_MS_WORD(Upper_16) OR LONGINT(Lower_16);

    IF (Mode AND VC_DATA_TYPE) <> 0 THEN
      BEGIN
        Bit_19_20 := Ret_Address AND $C0000;
        Ret_Address := Ret_Address SHL 1;
        Ret_Address := Ret_Address AND $3FFFF;
        Ret_Address := Ret_Address OR Bit_19_20;
      END;

    Make_Physical_Address := Ret_Address;
  END;



FUNCTION Convert_TO_16Bit(Address : LONGINT) : LONGINT;
{ This FUNCTION will translate the address IF the dma channel
  is a 16 bit channel. This translation is NOT necessary FOR
  an 8 bit dma channel. }
  VAR
    Hold_Address : LONGINT;
  BEGIN
    Hold_Address := Address;

    { Convert TO 16 translated address }
    Address := Address SHR 1;

    { Zero out bit 17 }
    Address := Address AND $0001FFFF;

    { Reset Buts 18 AND 19 }
    Address := Address OR (Hold_Address AND $000C0000);

    Convert_TO_16Bit := Address;
  END;


FUNCTION ParseToHex(VAR FromStr : STRING; VAR ToWord : WORD) : BOOLEAN;
  (* Take the first number found.  Disregard ',' AND ' ' chars *)
  BEGIN
    ParseToHex := FALSE;
    WHILE ((fromstr[1] = ' ') OR
           (fromstr[1] = ',') OR
           (fromstr[1] = '$')) AND
           (length(fromstr)<>0) DO
      delete(fromstr,1,1);
    IF (ord(fromstr[1]) > 47) AND
       (ord(fromstr[1]) < 58) AND
       (fromstr <> '') THEN
      BEGIN
        ParseToHex := TRUE;  (* a number was found *)
        ToWord := 0;
        WHILE (ord(fromstr[1]) > 47) AND
              (ord(fromstr[1]) < 58) AND
              (FromStr <> '') DO
          BEGIN
            ToWord := (ToWord * $10) + (Ord(FromStr[1]) - Ord('0'));
            delete(FromStr,1,1);
          END;
        ParseToHex := TRUE;
      END;
  END;

FUNCTION ParseToNum(VAR FromStr : STRING; VAR ToWord : WORD) : BOOLEAN;
  (* Take the first number found.  Disregard ',' AND ' ' chars *)
  VAR
    StripStr : STRING[10];
    Code     : INTEGER;
  BEGIN
    ParseToNum := FALSE;
    WHILE ((fromstr[1] = ' ') OR
           (fromstr[1] = ',')) AND
           (length(fromstr)<>0) DO
      delete(fromstr,1,1);
    IF (ord(fromstr[1]) > 47) AND
       (ord(fromstr[1]) < 58) AND
       (fromstr <> '') THEN
      BEGIN
        ParseToNum := TRUE;  (* a number was found *)
        StripStr := '';
        WHILE (ord(fromstr[1]) > 47) AND
              (ord(fromstr[1]) < 58) AND
              (FromStr <> '') DO
          BEGIN
            StripStr := StripStr + FromStr[1];
            delete(FromStr,1,1);
          END;
        Val(StripStr, ToWord, Code);
        IF Code=0 THEN
          ParseToNum := TRUE;
      END;
  END;

FUNCTION UltraGetCfg(VAR Config : ULTRA_CFG) : BOOLEAN;
  VAR
    EnvStr : STRING;
  BEGIN
    UltraGetCfg := FALSE;
    UltraOk := FALSE;
    UltraError := BAD_ENV_VAR;
    UltraErrorStr := ErrorStrings[BAD_ENV_VAR];
    WITH Config DO
      BEGIN
        Base_Port     := $220;
        DRAM_DMA_Chan :=   01;
        ADC_DMA_Chan  :=   01;
        GF1_IRQ_Num   :=   11;
        MIDI_IRQ_Num  :=   05;
      END;
    EnvStr := GetEnv('ULTRASND');
    IF EnvStr <> '' THEN
      IF ParseToHex(EnvStr, Config.Base_Port) AND
         ParseToNum(EnvStr, Config.DRAM_DMA_Chan) AND
         ParseToNum(EnvStr, Config.ADC_DMA_Chan) AND
         ParseToNum(EnvStr, Config.GF1_IRQ_Num) AND
         ParseToNum(EnvStr, Config.MIDI_IRQ_Num) THEN
        BEGIN
          UltraGetCfg := TRUE;
          ClearError;
        END;
  END;


PROCEDURE UltraStartDRAMDMA(Control : BYTE);
  BEGIN
    { Make sure only right bits are on.  Ignore other bits }
    Control := Control AND (DMA_READ OR DMA_16 OR DMA_CVT_2);

    { Set the transfer speed }
    Control := Control OR DMA_R0;

    { Enable AND turn on IRQ }
    Control := Control OR (DMA_ENABLE OR DMA_IRQ_ENABLE);

    { IF it's a 16 bit dma channel, set the flag bit }
    IF GUSData.DRAM_DMA_CHAN >= 4 THEN
      Control := Control OR DMA_Width_16;

    { DMA transfer begins here }
    ASM
      PUSHF
      CLI
    END;
    PORT[GUSData.Reg_Select] := DMA_Control;
    PORT[GUSData.Data_Hi] := Control;
    ASM
      POPF
    END;
  END;


PROCEDURE UltraStartRecordDMA(Control : BYTE);
  BEGIN
    { Make sure only right bits are on.  Ignore others }
    Control := Control AND (ADC_Mode OR ADC_Twos_COMP);

    { Enable AND turn on IRQ }
    Control := Control OR (ENABLE_ADC OR ADC_IRQ_ENABLE);

    { IF it's a 16 bit dma channel, set the flag bit }
    IF GUSData.ADC_DMA_Chan >= 4 THEN
      Control := Control OR ADC_DMA_Width;

    { DMA transfer begins here }
    ASM
      PUSHF;
      CLI;
    END;
    PORT[GUSData.Reg_Select] := Sample_Control;
    PORT[GUSData.Data_Hi] := Control;
    ASM
      POPF;
    END;
  END;


PROCEDURE UltraDMANext(VAR TDma      : DMA_Entry;
                           Recording : BOOLEAN);
  VAR
    TCount : WORD;
  BEGIN
    { Shut off RollOver }
    TDMA.Flags := TDMA.Flags AND (NOT TWO_FLAG);

    { Save the amount Transferred }
    TDMA.Amnt_Sent := TDMA.Cur_Size;

    { Set up the next buffer }
    TDMA.Cur_Size := TDMA.Nxt_Size;

    TCount := TDMA.Nxt_Size; { From TCount := TDMA.Nxt_Size-1; KWK 13MAR94 }
    PORT[TDMA.SINGLE] := TDMA.DMA_Disable;
    PORT[TDMA.Clear_FF] := 0;
    PORT[TDMA.Addr] := (TDMA.Nxt_Addr AND $FF);
    PORT[TDMA.Addr] := ((TDMA.Nxt_Addr SHR 8) AND $FF);
    PORT[TDMA.Page] := TDMA.Nxt_Page;
    PORT[TDMA.Mode] := TDMA.Cur_Mode;
    PORT[TDMA.Clear_FF] := 0;
    PORT[TDMA.Count] := (TCount AND $0FF);
    PORT[TDMA.Count] := ((TCount SHR 8) AND $0FF);
    PORT[TDMA.SINGLE] := TDMA.DMA_Enable;

    { Now start the transfer }
    IF Recording THEN
      UltraStartRecordDMA(TDMA.Cur_Control)
    ELSE
      UltraStartDRAMDMA(TDMA.Cur_Control);
  END;


FUNCTION PrimeDMA(PC_PTR  : POINTER;
                  TType   : INTEGER;
                  Size    : WORD;
                  Channel : WORD) : INTEGER;
  VAR
    TDMAPtr : ^DMA_Entry;
    EPtr    : POINTER;
    S_20Bit : LONGINT;
    E_20Bit : LONGINT;
    S_SEG,
    S_Off,
    E_SEG,
    E_Off   : WORD;
    S_Page,
    S_Addr,
    E_Page,
    E_Addr  : WORD;
    TCount  : WORD;
  BEGIN
    TDmaPtr := @_GF1_DMA[Channel-1];

    IF (TDMAPtr^.Flags AND DMA_Pending) <> 0 THEN
      BEGIN
        PrimeDMA := DMA_BUSY;
        EXIT;
      END;

    { Set DMA access active }
    TDMAPtr^.Flags := TDMAPtr^.Flags OR DMA_Pending;
    TDMAPtr^.Flags := TDMAPtr^.Flags OR CALIB_COUNT;

    { Convert PC address TO a 20 bit physical address that the DMA
      controller needs }
    S_SEG := SEG(PC_PTR^);
    S_Off := Ofs(PC_PTR^);
    S_20Bit := LONGINT(LONGINT(S_SEG) SHL 4);
    INC(S_20Bit, S_Off);

    E_20Bit := S_20Bit+Size-1;
    S_Page := WORD((S_20Bit AND $FFFF0000) SHR 16);
    E_Page := WORD((E_20Bit AND $FFFF0000) SHR 16);

    { IF 16-Bit Transfer, THEN address, count, AND size are divided by 2 }
    IF Channel >= 4 THEN
      BEGIN
        S_20Bit := S_20Bit SHR 1;
        E_20Bit := E_20Bit SHR 1;
        Size := Size SHR 1;
      END;

    S_Addr := WORD(S_20Bit AND $0000FFFF);
    E_Addr := WORD(E_20Bit AND $0000FFFF);

    { In case the buffer goes over a page, save the data FOR the irq
      handler TO use TO finish sending the data }

    IF S_Page <> E_Page THEN
      BEGIN
        TDMAPtr^.Flags := TDMAPtr^.Flags OR TWO_FLAG;
        TDMAPtr^.Nxt_Page := E_Page;
        IF Channel >= 4 THEN
          BEGIN
            IF (TDmaPtr^.Nxt_Page AND $01) <> 0 THEN
              TDmaPtr^.Nxt_Addr := $8000
            ELSE
              TDmaPtr^.Nxt_Addr := 0;
            E_Addr := E_Addr AND $7FFF;
          END
        ELSE
          TDMAPtr^.Nxt_Addr := 0;
        TDmaPtr^.Nxt_Size := E_Addr;
        { Alter amount TO send }
        Size := Size - E_Addr - 1; { Only supposed to send this much }
      END
    ELSE
      TDMAPtr^.Flags := TDMAPtr^.Flags AND (NOT TWO_FLAG);

    IF (TType = INDEF_READ) AND
       ((TDMAPtr^.Flags AND TWO_FLAG) <> 0) THEN
      BEGIN
        PrimeDMA := BAD_DMA_ADDR;
        UltraOk := FALSE;
        UltraError := BAD_DMA_ADDR;
        UltraErrorStr := ErrorStrings[BAD_DMA_ADDR];
        EXIT;
      END;

    { Init the amount transferred, show how big this part OF the buffer is }
    TDMAPtr^.Cur_Page  := S_Page;
    TDMAPtr^.Cur_Addr  := S_Addr;
    TDMAPtr^.Amnt_Sent := 0;
    TDMAPtr^.Cur_Size  := Size;

    TCount := Size-1;

    CASE TType OF
      READ_DMA:
        TDMAPtr^.Cur_Mode := TDMAPtr^.ReadTrans;
      WRITE_DMA:
        TDMAPtr^.Cur_Mode := TDMAPtr^.WriteTrans;
        { all we DO is kick off the gf1 sample control TO restart the dma
          recording.  Turn on auto init: }
      INDEF_READ:
        TDMAPtr^.Cur_Mode := TDMAPtr^.ReadTrans OR $10;
      INDEF_WRITE:
        TDMAPtr^.Cur_Mode := TDMAPtr^.WriteTrans OR $10;
     END;

    PORT[TDMAPtr^.SINGLE] := TDMAPtr^.DMA_Disable;              { Disable the channel }
    PORT[TDMAPtr^.Mode] := TDMAPtr^.Cur_Mode;                   { Set the mode }
    PORT[TDMAPtr^.Clear_FF] := 0;                               { Clear f/f }
    PORT[TDMAPtr^.Addr] := LSB(TDMAPtr^.Cur_Addr);              { Least Sig BYTE OF Address }
    PORT[TDMAPtr^.Addr] := MSB(TDMAPtr^.Cur_Addr);              { Most Sig BYTE OF Address }
    PORT[TDMAPtr^.Page] := TDMAPtr^.Cur_Page;                   { Page # }
    PORT[TDMAPtr^.Clear_FF] := 0;                               { Clear f/f }
    PORT[TDMAPtr^.Count] := LSB(TCount);                        { Count Least Sig BYTE }
    PORT[TDMAPtr^.Count] := MSB(TCount);                        { Count Most Sig BYTE }
    PORT[TDMAPtr^.SINGLE] := TDMAPtr^.DMA_Enable;               { Enable the channel }

    PrimeDMA := ULTRA_OK;
  END;


FUNCTION UltraDRAMDMABusy : BOOLEAN;
  BEGIN
    UltraDRAMDMABusy := (_GF1_DMA[GUSData.DRAM_DMA_Chan-1].Flags AND DMA_Pending) <> 0;
    ClearError;
  END;


PROCEDURE UltraWaitDRAMDMA;
  BEGIN
    GUSData.Flags := GUSData.Flags AND (NOT DRAM_DMA_NOWAIT);

    ASM
      PUSHF;   { Save the current IRQ state }
      STI;     { Allow Interrupts TO happen }
    END;
    { Wait FOR the IRQ TO clear this }
    WHILE (GUSData.Flags AND DRAM_DMA_BUSY) <> 0 DO;
    ASM
      POPF;    { Restore IRQ state }
    END;
  END;


{ This FUNCTION will prime the pc's dma controller FOR sending TO
  OR receiving data from the UltraSound. The last thing it does is
  start the transfer. It either waits FOR it TO complete OR it
  could return immediately ...

  NOTE: This example will NOT attempt TO explain how the pc's
        dma controller works. IF you want TO know more than you
        can by looking at this code, there are lots OF documents
        available that go into great detail. }
FUNCTION UltraDMADRAM_64K(PC_PTR    : POINTER;   { Buffer in RAM }
                          Size      : WORD;      { # OF bytes TO send/recv }
                          Ultra_PTR : LONGINT;   { Physical Ultrasound Addr }
                          Control   : BYTE;      { read OR write dram }
                          Wait      : BOOLEAN) : INTEGER;
  VAR
    TDMAPtr      : ^DMA_ENTRY;
    DRAM_Address : LONGINT;
    TType        : INTEGER;
    Ret          : INTEGER;
  BEGIN
    TDMAPtr := @_GF1_DMA[GUSData.DRAM_DMA_Chan-1];

    IF (Control AND DMA_Read) <> 0 THEN
      BEGIN
        IF (Control AND DMA_AUTO_INIT) <> 0 THEN
          TType := INDEF_READ
        ELSE
          TType := READ_DMA;
      END
    ELSE
      BEGIN
        IF (Control AND DMA_AUTO_INIT) <> 0 THEN
          TType := INDEF_WRITE
        ELSE
          TType := WRITE_DMA;
      END;

    Control := Control AND (NOT DMA_AUTO_INIT);

    Ret := PrimeDMA(PC_PTR, TType, Size, GUSData.DRAM_DMA_Chan);

    IF Ret <> ULTRA_OK THEN
      BEGIN
        UltraDMADRAM_64K := Ret;
        EXIT;
      END;

    { IF Its a 16 bit DMA Channel, an extra translation is necessary }
    IF (GUSData.DRAM_DMA_Chan >= 4) THEN
      DRAM_Address := Convert_TO_16Bit(Ultra_PTR)
    ELSE
      DRAM_Address := Ultra_PTR;

    { Only use the upper 16 bits. This means that an 8 bit xfer MUST   }
    { start on a 16 BYTE boundary AND a 16 bit xfer MUST start on a 32 }
    { BYTE boundary. }

    ASM
      PUSHF
      CLI
    END;
    PORT[GUSData.Reg_Select] := SET_DMA_ADDRESS;
    PORTW[GUSData.Data_LOW] := WORD(DRAM_Address SHR 4);
    ASM
      POPF
    END;

    { Set flag that irq handler clears when the xfer is complete }
    GUSData.Flags := GUSData.Flags OR DRAM_DMA_BUSY;

    { Now tell GF1 TO start xfer ... }
    TDMAPtr^.Cur_Control := Control;
    UltraStartDRAMDMA(Control);

    IF Wait THEN
      UltraWaitDRAMDMA
    ELSE
      GUSData.Flags := GUSData.Flags OR DRAM_DMA_NOWAIT;

    UltraDMADRAM_64k := ULTRA_OK;
  END;


{ This FUNCTION will download/upload up TO 64K OF data into/out the
  dram on the ultrasound card. Since the card canNOT DMA
  across 256K boundaries, this routine will split up the xfer .... }
FUNCTION UltraDMADRAM(PC_PTR    : POINTER;   { POINTER TO PC's Buffer }
                      Size      : WORD;      { Size OF the transfer   }
                      Ultra_PTR : LONGINT;   { Physical GUS Address   }
                      Control   : BYTE;      { Read OR write Dram     }
                      Wait      : BOOLEAN) : INTEGER;
  CONST
    Two_Flag    : BOOLEAN = FALSE;
  VAR
    TEnd : LONGINT;
    Start_Page,
    END_Page    : LONGINT;
    Start_Size  : WORD;
    Nxt_PC_PTR  : POINTER;
    Nxt_PTR     : LONGINT;
    Nxt_Size    : WORD;
    Ret         : INTEGER;
  BEGIN
    TEnd := Ultra_PTR + LONGINT(SIZE) - 1;

    { Isolate the 256k page }
    Start_Page := Ultra_PTR SHR 18;
    END_Page := TEnd SHR 18;

    IF Start_Page <> END_Page THEN
      BEGIN
        { Where second part goes into DRAM }
        Nxt_PTR := END_Page SHL 18;

        { How much TO put there }
        Nxt_Size := TEnd - Nxt_PTR;

        { How much in first page OF DRAM }
        Start_Size := Size - Nxt_Size;

        { Where in PC Ram is second part }
        Nxt_PC_PTR := PTR(SEG(PC_PTR^),Ofs(PC_PTR^)+Start_Size);
        Two_Flag := TRUE;
      END
    ELSE
      Start_Size := Size;

    Ret := UltraDMADRAM_64K(PC_PTR, Start_Size, Ultra_PTR, Control, wait);
    IF Ret <> ULTRA_OK THEN
      BEGIN
        UltraDMADRAM := Ret;
        EXIT;
      END;

    { Now send the second half IF there is one }
    IF Two_Flag THEN
      BEGIN
        IF Wait THEN
          UltraWaitDRAMDMA;
        Ret := UltraDMADram_64K(Nxt_PC_PTR, Nxt_Size, Nxt_PTR, Control, wait);
        IF Ret <> ULTRA_OK THEN
          BEGIN
            UltraDMADRAM := Ret;
            EXIT;
          END;
      END;

    UltraDMADRAM := ULTRA_OK;
  END;


FUNCTION UltraDownLoad(DataPtr  : POINTER;
                       Control  : BYTE;
                       DRAM_Loc : LONGINT;
                       Len      : WORD;
                       Wait     : BOOLEAN) : BOOLEAN;
  VAR
    RetCode : INTEGER;
  BEGIN
    Control := Control AND (NOT DMA_READ);
    RetCode := UltraDMADRAM(DataPtr, Len, DRAM_Loc, Control, Wait);
    IF RetCode=ULTRA_OK THEN
      BEGIN
        ClearError;
        UltraDownLoad := TRUE;
      END
    ELSE
      BEGIN
        UltraDownLoad := FALSE;
        UltraOk := FALSE;
        UltraError := RetCode;
        UltraErrorStr := ErrorStrings[RetCode];
      END;
  END;


FUNCTION UltraUpLoad(DataPtr  : POINTER;
                     Control  : BYTE;
                     DRAM_Loc : LONGINT;
                     Len      : WORD;
                     Wait     : BOOLEAN) : BOOLEAN;
  VAR
    RetCode : INTEGER;
  BEGIN
    Control := Control OR DMA_READ;
    RetCode := UltraDMADRAM(DataPtr, Len, DRAM_Loc, Control, Wait);
    IF RetCode=ULTRA_OK THEN
      BEGIN
        ClearError;
        UltraUpLoad := TRUE;
      END
    ELSE
      BEGIN
        UltraUpLoad := FALSE;
        UltraOk := FALSE;
        UltraError := RetCode;
        UltraErrorStr := ErrorStrings[RetCode];
      END;

  END;

{ These procedures take THEN handler given TO them AND make it the current
  handler.  The variable is THEN modified TO hold the old handler's address }
PROCEDURE UltraDRAMTcHandler(VAR Handler : PFV);
  VAR
    Old_Handler : PFV;
  BEGIN
    Old_Handler := GUSData.DRAM_DMA_TC_Func;
    GUSData.DRAM_DMA_TC_Func := Handler;
    Handler := Old_Handler;
  END;

PROCEDURE UltraMIDIXMitHandler(VAR Handler : WORD_Proc);
  VAR
    Old_Handler : WORD_Proc;
  BEGIN
    Old_Handler := GUSData.MIDI_XMIT_Func;
    GUSData.MIDI_XMit_Func := Handler;
    Handler := Old_Handler;
  END;

PROCEDURE UltraMIDIRecvHandler(VAR Handler : TwoWord_Proc);
  VAR
    Old_Handler : TwoWord_Proc;
  BEGIN
    Old_Handler := GUSData.MIDI_Recv_Func;
    GUSData.MIDI_Recv_Func := Handler;
    Handler := Old_Handler;
  END;

PROCEDURE UltraTimer1Handler(VAR Handler : PFV);
  VAR
    Old_Handler : PFV;
  BEGIN
    Old_Handler := GUSData.Timer1_Func;
    GUSData.Timer1_Func := Handler;
    Handler := Old_Handler;
  END;

PROCEDURE UltraTimer2Handler(VAR Handler : PFV);
  VAR
    Old_Handler : PFV;
  BEGIN
    Old_Handler := GUSData.Timer2_Func;
    GUSData.Timer2_Func := Handler;
    Handler := Old_Handler;
  END;

PROCEDURE UltraWaveHandler(VAR Handler : Int_Proc);
  VAR
    Old_Handler : Int_Proc;
  BEGIN
    Old_Handler := GUSData.WaveTable_Func;
    GUSData.WaveTable_Func := Handler;
    Handler := Old_Handler;
  END;

PROCEDURE UltraVolumeHandler(VAR Handler : Int_Proc);
  VAR
    Old_Handler : Int_Proc;
  BEGIN
    Old_Handler := GUSData.Volume_Func;
    GUSData.Volume_Func := Handler;
    Handler := Old_Handler;
  END;

PROCEDURE UltraRecordHandler(VAR Handler : PFV);
  VAR
    Old_Handler : PFV;
  BEGIN
    Old_Handler := GUSData.RECORD_DMA_TC_Func;
    GUSData.RECORD_DMA_TC_Func := Handler;
    Handler := Old_Handler;
  END;

PROCEDURE UltraAuxHandler(VAR Handler : PFV);
  VAR
    Old_Handler : PFV;
  BEGIN
    Old_Handler := GUSData.Aux_Irq_Func;
    GUSData.Aux_IRQ_Func := Handler;
    Handler := Old_Handler;
  END;


FUNCTION UltraPing(PPort : WORD) : BOOLEAN;
  VAR
    Val0,
    Val1      : BYTE;
    Save_Val0 : BYTE;
    Save_Val1 : BYTE;
  BEGIN
    { Save Current Values }
    Save_Val0 := UltraPeekData(PPort, 0);
    Save_Val1 := UltraPeekData(PPort, 1);

    UltraPokeData(PPort, 0, $AA);
    UltraPokeData(PPort, 1, $55);

    Val0 := UltraPeekData(PPort, 0);
    Val1 := UltraPeekData(PPort, 1);

    { Restore data TO old values }
    UltraPokeData(PPort, 0, Save_Val0);
    UltraPokeData(PPort, 1, Save_Val1);

    IF (Val0 = $AA) AND
       (Val1 = $55) THEN
      BEGIN
        UltraPing := TRUE;
        ClearError;
      END
    ELSE
      BEGIN
        UltraPing := FALSE;
        UltraOk := FALSE;
        UltraError := No_Ultra;
        UltraErrorStr := ErrorStrings[NO_ULTRA];
      END;
  END;

FUNCTION UltraProbe(PPort : WORD) : BOOLEAN;
  BEGIN
    GUSData.Base_Port := PPort;
    GUSData.MIDI_Data := PPort + GF1_MIDI_Data;
    GUSData.MIDI_Control := PPort + GF1_MIDI_CTRL;
    GUSData.Voice_Select := PPort + GF1_PAGE;
    GUSData.Reg_Select := PPort + GF1_REG_SELECT;
    GUSData.Data_LOW := PPort + GF1_Data_LOW;
    GUSData.Data_Hi := PPort + GF1_Data_HI;
    GUSData.IRQ_Status := PPort + GF1_IRQ_STAT;
    GUSData.DRAM_Data := PPort + GF1_DRAM;
    GUSData.Mix_Control := PPort + GF1_MIX_CTRL;
    GUSData.Timer_Control := PPort + GF1_TIMER_CTRL;
    GUSData.Timer_Data := PPort+ GF1_TIMER_DATA;
    GUSData.IRQ_Control := PPort + GF1_IRQ_CTRL;

    { Pull a Reset }
    PORT[GUSData.Reg_Select] := MASTER_RESET;
    PORT[GUSData.Data_Hi] := $00;

    GF1_Delay;
    GF1_Delay;

    { Release the reset }
    PORT[GUSData.Reg_Select] := MASTER_RESET;
    PORT[GUSData.Data_Hi] := GF1_MASTER_RESET;

    GF1_Delay;
    GF1_Delay;

    UltraProbe := UltraPing(PPort);
  END;

PROCEDURE UltraSetInterface(DRAM : INTEGER;   { DRAM DMA Channel }
                            ADC  : INTEGER;   { ADC DMA Channel }
                            GF1  : INTEGER;   { GF1 IRQ # }
                            MIDI : INTEGER);  { MIDI IRQ # }
  CONST
    DRAM_DMA : BYTE = 0;
    ADC_DMA  : BYTE = 0;
  VAR
    GF1_IRQ,
    MIDI_IRQ,
    IRQ_Control,
    DMA_Control,
    Mix_Image    : BYTE;
  BEGIN
    ASM
      PUSHF
      CLI
    END;
    { Don't need TO check FOR 0 irq #.  Its latch entry = 0 }
    GF1_IRQ := _GF1_IRQ[GF1].Latch;

    MIDI_IRQ := _GF1_IRQ[MIDI].Latch;

    MIDI_IRQ := MIDI_IRQ SHL 3;

    { Set Latch TO 0 IF DMA channel is 0.  This
      takes this channel off line }
    IF DRAM <> 0 THEN
      DRAM_DMA := _GF1_DMA[DRAM-1].Latch;

    IF ADC <> 0 THEN
      ADC_DMA := _GF1_DMA[ADC-1].Latch;

    ADC_DMA := ADC_DMA SHL 3;

    IRQ_Control := 0;
    DMA_Control := 0;

    { Init Image TO everything disabled }
    Mix_Image := GUSData.Mix_Image;

    IRQ_Control := IRQ_Control OR GF1_IRQ;

    IF (GF1 = MIDI) AND (GF1 <> 0) THEN
      IRQ_Control := IRQ_Control OR $40
    ELSE
      IRQ_Control := IRQ_Control OR MIDI_IRQ;

    DMA_Control := DMA_Control OR DRAM_DMA;
    IF (DRAM = ADC) AND (DRAM <> 0) THEN
      DMA_Control := DMA_Control OR $40
    ELSE
      DMA_Control := DMA_Control OR ADC_DMA;

    { Set up FOR Digital ASIC }
    PORT[GUSData.Base_Port+$0F] := 5;
    PORT[GUSData.Mix_Control] := Mix_image;
    PORT[GUSData.IRQ_Control] := 0;
    PORT[GUSData.Base_Port+$0F] := 0;

    { First DO DMA Control Register }
    PORT[GUSData.Mix_Control] := Mix_Image;
    PORT[GUSData.IRQ_Control] := DMA_Control OR $80;

    { IRQ Control Register}
    PORT[GUSData.Mix_Control] := Mix_Image OR $40;
    PORT[GUSData.IRQ_Control] := IRQ_Control;

    { First DO DMA Control Register }
    PORT[GUSData.Mix_Control] := Mix_Image;
    PORT[GUSData.IRQ_Control] := DMA_Control;

    { IRQ Control Register }
    PORT[GUSData.Mix_Control] := Mix_Image OR $40;
    PORT[GUSData.IRQ_Control] := IRQ_Control;

    { IRQ Control, enable IRQ.
      Just TO lock out writes TO irq/dma register }
    PORT[GUSData.Voice_Select] := 0;

    { Enable output AND IRQ, disable line AND mic imput }
    Mix_Image := Mix_Image OR $09;
    PORT[GUSData.Mix_Control] := Mix_Image;

    { Just TO lock out write TO irq/dma register }
    PORT[GUSData.Voice_Select] := 0;

    { Put image back }
    GUSData.Mix_Image := Mix_Image;
    ASM
      POPF
    END;
  END;


FUNCTION UltraGetOutput : BOOLEAN;
  BEGIN
    UltraGetOutput := (GUSData.Mix_Image AND ENABLE_OUTPUT) = 0;
  END;

PROCEDURE UltraEnableOutput;
  BEGIN
    GUSData.Mix_Image := GUSData.Mix_Image AND (NOT ENABLE_OUTPUT);
    PORT[GUSData.Mix_Control] := GUSData.Mix_Image;
  END;

PROCEDURE UltraDisableOutput;
  BEGIN
    GUSData.Mix_Image := GUSData.Mix_Image OR ENABLE_OUTPUT;
    PORT[GUSData.Mix_Control] := GUSData.Mix_Image;
  END;

FUNCTION UltraGetLineIn : BOOLEAN;
  BEGIN
    UltraGetLineIn := (GUSData.Mix_Image AND ENABLE_LINE_IN) = 0;
  END;

PROCEDURE UltraEnableLineIn;
  BEGIN
    GUSData.Mix_Image := GUSData.Mix_Image AND (NOT ENABLE_LINE_IN);
    PORT[GUSData.Mix_Control] := GUSData.Mix_Image;
  END;

PROCEDURE UltraDisableLineIn;
  BEGIN
    GUSData.Mix_Image := GUSData.Mix_Image OR ENABLE_LINE_IN;
    PORT[GUSData.Mix_Control] := GUSData.Mix_Image;
  END;

FUNCTION UltraGetMicIn : BOOLEAN;
  BEGIN
    UltraGetMicIn := (GUSData.Mix_Image AND ENABLE_MIC_IN) <> 0;
  END;

PROCEDURE UltraEnableMicIn;
  BEGIN
    GUSData.Mix_Image := GUSData.Mix_Image OR ENABLE_MIC_IN;
    PORT[GUSData.Mix_Control] := GUSData.Mix_Image;
  END;

PROCEDURE UltraDisableMicIn;
  BEGIN
    GUSData.Mix_Image := GUSData.Mix_Image AND (NOT ENABLE_MIC_IN);
    PORT[GUSData.Mix_Control] := GUSData.Mix_Image;
  END;


FUNCTION UltraReset(Voices : INTEGER) : BOOLEAN;
  VAR
    V         : BYTE;
    Select,
    Data_LOW,
    Data_Hi : INTEGER;
  BEGIN
    IF (Voices < 14) OR (Voices > 32) THEN
      BEGIN
        UltraReset := FALSE;
        UltraOk := FALSE;
        UltraError := Bad_Num_OF_Voices;
        UltraErrorStr := ErrorStrings[BAD_NUM_OF_VOICES];
        EXIT;
      END;

    GUSData.Voices := Voices;
    GUSData.Timer_Ctrl := 0;
    GUSData.Timer_Mask := 0;

    Select := GUSData.Reg_Select;
    Data_LOW := GUSData.Data_LOW;
    Data_Hi := GUSData.Data_Hi;

    { Set these TO zero so the they don't get summed in FOR voices that are
      NOT running. IF their volumes are NOT at zero, whatever value they
      are pointing at, will get summed into the output. By setting that
      location TO 0, that voice will have no contribution TO the output
      (2 locations are done in case voice is set TO 16 bits ... ) }
    UltraPokeData(GUSData.BASE_PORT,0,0);
    UltraPokeData(GUSData.BASE_PORT,1,0);

    ASM
      PUSHF
      CLI
    END;
    { Pull a register-level reset on the card. }
    PORT[Select] := MASTER_RESET;
    PORT[Data_Hi] := $00;

    FOR V := 0 TO 9 DO
      GF1_Delay;

    PORT[Select] := MASTER_RESET;
    PORT[Data_Hi] := GF1_MASTER_RESET;

    FOR V := 0 TO 9 DO
      GF1_Delay;

    { Reset the MIDI PORT }
    PORT[GUSData.MIDI_Control] := MIDI_RESET;
    FOR V := 0 TO 9 DO
      GF1_Delay;
    PORT[GUSData.MIDI_Control] := $00;

    { Clear all interrupts }
    PORT[Select] := DMA_Control;
    PORT[Data_Hi] := $00;
    PORT[Select] := TIMER_Control;
    PORT[Data_Hi] := $00;
    PORT[Select] := SAMPLE_Control;
    PORT[Data_Hi] := $00;

    { Set the number OF active voices }
    PORT[Select] := SET_VOICES;
    PORT[Data_Hi] := Lo((Voices-1) OR $C0);

    { Clear interrupts on voices }
    { Reading the status ports will clear the irqs }
    V := PORT[GUSData.IRQ_Status];
    PORT[Select] := DMA_Control;
    V := PORT[Data_Hi];
    PORT[Select] := SAMPLE_Control;
    V := PORT[Data_Hi];
    PORT[select] := GET_IRQV;
    V := PORT[Data_Hi];

    FOR V := 0 TO Voices-1 DO
      BEGIN
        { Select the proper voice }
        PORT[GUSData.Voice_Select] := V;

        { Stop the voice AND volume }
        PORT[Select] := SET_Control;
        PORT[Data_Hi] := (VOICE_STOPPED OR STOP_VOICE);
        PORT[Select] := SET_VOLUME_CONTROL;
        PORT[Data_Hi] := (VOLUME_STOPPED OR STOP_VOLUME);

        { Wait 4.8 Microseconds OR more }
        GF1_Delay;

        { Initialize each voice specific registers.  This is NOT really
          necessary, but is nice FOR completeness sake.  Each application
          will set up these TO whatever values it needs }
        PORT[Select] := SET_FREQUENCY;
        PORTW[Data_LOW] := $0400;

        PORT[Select] := SET_START_HIGH;
        PORTW[Data_LOW] := $0;

        PORT[Select] := SET_START_LOW;
        PORTW[Data_LOW] := $0;

        PORT[Select] := SET_END_HIGH;
        PORTW[Data_LOW] := $0;

        PORT[Select] := SET_END_LOW;
        PORTW[Data_LOW] := $0;

        PORT[Select] := SET_VOLUME_RATE;
        PORT[Data_HI] := $01;

        PORT[Select] := SET_VOLUME_START;
        PORT[Data_HI] := $10;

        PORT[Select] := SET_VOLUME_END;
        PORT[Data_HI] := $E0;

        PORT[Select] := SET_VOLUME;
        PORTW[Data_LOW] := $0000;

        PORT[Select] := SET_ACC_HIGH;
        PORTW[Data_LOW] := $0;

        PORT[Select] := SET_ACC_LOW;
        PORTW[Data_LOW] := $0;

        PORT[Select] := SET_BALANCE;
        PORT[Data_Hi] := $07;

      END;

    V := PORT[GUSData.IRQ_STATUS];
    PORT[Select] := DMA_CONTROL;
    V := PORT[Data_Hi];
    PORT[Select] := SAMPLE_CONTROL;
    V := PORT[DATA_Hi];
    PORT[Select] := GET_IRQV;
    V := PORT[Data_Hi];

    { Set up GF1 Chip FOR interrupts AND enable DACs }
    PORT[Select] := MASTER_RESET;
    PORT[Data_Hi] := (GF1_MASTER_RESET OR GF1_OUTPUT_ENABLE OR GF1_MASTER_IRQ);
    { Restore the IRQ state }
    ASM
      POPF
    END;

    UltraReset := TRUE;
    ClearError;
  END;


PROCEDURE UltraStopVoice(Voice : INTEGER);
  VAR
    Data : BYTE;
  BEGIN
    { Save the current IRQ State }
    ASM
      PUSHF
      CLI
    END;
    { Select the proper voice }
    PORT[GUSData.Voice_Select] := Lo(Voice);

    { turn off the roll over bit first }
    PORT[GUSData.Reg_Select] := GET_VOLUME_CONTROL;
    Data := PORT[GUSData.Data_Hi];
    Data := Data AND (NOT VC_ROLLOVER);
    PORT[GUSData.Reg_Select] := SET_VOLUME_CONTROL;
    PORT[GUSData.Data_Hi] := Data;
    GF1_Delay;
    PORT[GUSData.Data_Hi] := Data;

    { Now stop the voice }
    PORT[GUSData.Reg_Select] := GET_CONTROL;
    Data := PORT[GUSData.Data_Hi];
    Data := Data AND (NOT VC_WAVE_IRQ); { Disable the IRQs  }
    Data := Data OR (VOICE_STOPPED OR STOP_VOICE);
    PORT[GUSData.Reg_Select] := SET_Control;
    PORT[GUSData.Data_Hi] := Data;
    GF1_DELAY;
    PORT[GUSData.Data_hi] := Data;
    { Restore the IRQ state }
    ASM
      POPF
    END;
  END;


PROCEDURE GF1_SetVect(INT_Number : BYTE;
                      ISR        : POINTER);
  BEGIN
    ASM
      push DS
      lds  DX, isr
      mov  AH, $25
      mov  AL, INT_Number
      int  $21
      pop  DS
    END;
  END;

FUNCTION GF1_GetVect(INT_Number : BYTE) : POINTER;
  VAR
    Regs : Registers;
  BEGIN
    ASM
      push DS
      mov  AH, $35
      mov  AL, INT_Number
      int  $21
      mov  DX, ES
      mov  AX, BX
      pop  DS
    END;
    GF1_GetVect := PTR(Regs.DX,Regs.AX);
  END;


PROCEDURE ResetIRQHandlers(GF1_IRQ  : BYTE;
                           MIDI_IRQ : BYTE);
  VAR
    Temp_IRQ : INTEGER;
  BEGIN
    Temp_IRQ := GF1_IRQ;

    IF (Temp_IRQ <> 0) THEN
      BEGIN
        IF GF1_IRQ > 7 THEN
          GF1_IRQ := GF1_IRQ + $68
        ELSE
          GF1_IRQ := GF1_IRQ + $08;
        GF1_SetVect(GF1_IRQ, @GUSData.Old_GF1_Vec);
      END;

    IF ((Temp_IRQ <> MIDI_IRQ) AND (MIDI_IRQ <> 0)) THEN
      BEGIN
        IF MIDI_IRQ > 7 THEN
          MIDI_IRQ := MIDI_IRQ + $68
        ELSE
          MIDI_IRQ := MIDI_IRQ + $08;
        GF1_SetVect(MIDI_IRQ, @GUSData.Old_MIDI_Vec);
      END
  END;

PROCEDURE SetIRQs(GF1_IRQ  : BYTE;
                  MIDI_IRQ : BYTE);
  VAR
    Val : BYTE;
  BEGIN
    IF (GF1_IRQ <> 0) THEN
      BEGIN
        { Unmask GF1 Interrupt }
        Val := PORT[_GF1_IRQ[GF1_IRQ].IMR];
        Val := Val AND _GF1_IRQ[GF1_IRQ].Mask;
        PORT[_GF1_IRQ[GF1_IRQ].IMR] := Val;

        { Send a specific EOI in case OF pending interrupt }
        PORT[_GF1_IRQ[GF1_IRQ].OCR] := _GF1_IRQ[GF1_IRQ].Spec_EOI;
      END;

    IF (MIDI_IRQ <> GF1_IRQ) AND (MIDI_IRQ <> 0) THEN
      BEGIN
        { Unmask MIDI Interrupt }
        Val := PORT[_GF1_IRQ[MIDI_IRQ].IMR];
        Val := Val AND _GF1_IRQ[MIDI_IRQ].Mask;
        PORT[_GF1_IRQ[MIDI_IRQ].IMR] := Val;

        { Send a specific EOI in case OF pending interrupt }
        PORT[_GF1_IRQ[MIDI_IRQ].OCR] := _GF1_IRQ[MIDI_IRQ].Spec_EOI;
      END;

    IF (MIDI_IRQ > 7) OR (GF1_IRQ > 7) THEN
      BEGIN
        { Unmask IRQ 2 from first controller IF using 2nd controller }
        Val := PORT[_GF1_IRQ[2].IMR];
        Val := Val AND _GF1_IRQ[2].Mask;
        PORT[_GF1_IRQ[2].IMR] := Val;

        { Send a specific EOI in case OF pending interrupt }
        PORT[_GF1_IRQ[2].OCR] := _GF1_IRQ[2].Spec_EOI;
      END;
  END;

PROCEDURE ResetIRQs(GF1_IRQ  : BYTE;
                    MIDI_IRQ : BYTE);
  VAR
    Val : BYTE;
  BEGIN
    { Unmask GF1 Interrupt }
    IF (GF1_IRQ <> 2) AND (GF1_IRQ <> 0) THEN
      BEGIN
        { Turn mask bit back on ... }
        Val := PORT[_GF1_IRQ[GF1_IRQ].IMR];
        Val := Val OR (NOT _GF1_IRQ[GF1_IRQ].Mask);
        PORT[_GF1_IRQ[GF1_IRQ].IMR] := Val;
      END;

    { Unmask MIDI Interrupt }
    IF (MIDI_IRQ <> 2) AND (MIDI_IRQ <> 0) THEN
      BEGIN
        { Turn mask bit back on ... }
        Val := PORT[_GF1_IRQ[MIDI_IRQ].IMR];
        Val := Val OR (NOT _GF1_IRQ[MIDI_IRQ].Mask);
        PORT[_GF1_IRQ[MIDI_IRQ].IMR] := Val;
      END;
  END;

PROCEDURE Handle_DMA_Tc;
  VAR
    Regs    : Registers;
    TDMAPtr : ^DMA_Entry;
    Val     : BYTE;
  BEGIN
    { first check TO see IF we need TO service DRAM DMA terminal count }
    { NOTE: This read also clears the pending IRQ }
    ASM
      PUSHF
      CLI
    END;
    PORT[GUSData.REG_Select] := DMA_Control;
    Val := PORT[GUSData.Data_Hi];
    ASM
      POPF
    END;

    IF (Val AND DMA_IRQ_PENDING) <> 0 THEN
      BEGIN
        TDMAPtr := @_GF1_DMA[GUSData.DRAM_DMA_CHAN-1];
        IF (TDMAPtr^.Flags AND TWO_FLAG) <> 0 THEN
          UltraDMANext(TDMAPtr^, FALSE)  { Handle cross over page }
        ELSE
          BEGIN
            TDMAPtr^.Flags := TDMAPtr^.Flags AND (NOT DMA_PENDING);
            { Show Foreground task It's done }
            GUSData.Flags := GUSData.Flags AND (NOT DRAM_DMA_BUSY);
            { Accumulate totals }
            TDMAPtr^.Amnt_Sent := TDMAPtr^.Amnt_Sent + TDMAPtr^.Cur_Size;
            { Call Playback processing FUNCTION }
            GUSData.DRAM_DMA_Tc_Func;
          END;
      END;

    { Now check recording's terminal count}
    { NOTe: This read also clears the pending irq }
    ASM
      PUSHF
      CLI
    END;
    PORT[GUSData.REG_Select] := SAMPLE_CONTROL;
    Val := PORT[GUSData.Data_Hi];
    ASM
      POPF
    END;

    IF (Val AND ADC_IRQ_PENDING) <> 0 THEN
      BEGIN
        TDMAPtr := @_GF1_DMA[GUSData.ADC_DMA_Chan-1];
        IF (TDMAPtr^.Flags AND TWO_FLAG) <> 0 THEN
          UltraDMANext(TDMAPtr^, TRUE)   { Handle cross over page }
        ELSE
          BEGIN
            TDMAPtr^.Flags := TDMAPtr^.Flags AND (NOT DMA_PENDING);
            { Show Foreground task it's done }
            GUSData.Flags := GUSData.Flags AND (NOT ADC_DMA_BUSY);
            { Accumulate totals }
            TDMAPtr^.Amnt_Sent := TDMAPtr^.Amnt_Sent + TDMAPtr^.Cur_Size;
            { Call ADC processing FUNCTION }
            GUSData.RECORD_DMA_Tc_Func;
          END;
      END;
  END;


PROCEDURE Handle_Voice;
  VAR
    Temp1,
    Temp2,
    IRQ_Source     : BYTE;
    Voice          : WORD;
    Wave_Ignore,
    Volume_Ignore,
    Voice_Bit      : LONGINT;
  BEGIN
    { Clear the ignore flags.  These flags are needed because we get lots
      OF 'double' interrupts.  This will only allow one interrupt per voice }
    Wave_Ignore := 0;
    Volume_Ignore := 0;

    { The GF1 has a FIFO OF all pending wave table IRQs.  You should stay
      here AND service all pending waveform IRQs before returning }
    WHILE TRUE DO
      BEGIN
        ASM
          PUSHF
          CLI
        END;
        PORT[GUSData.Reg_Select] := GET_IRQV;
        IRQ_Source := PORT[GUSData.Data_Hi];
        ASM
          POPF
        END;

        { Pick off the voice # }
        Voice := IRQ_Source AND $1F;

        { Isolate the IRQ bits }
        IRQ_Source := IRQ_Source AND (VOICE_VOLUME_IRQ OR VOICE_WAVE_IRQ);

        { negative logic }
        IF IRQ_Source = (VOICE_VOLUME_IRQ OR VOICE_WAVE_IRQ) THEN
          { No pending IRQs left }
          EXIT;

        Voice_Bit := LONGINT(1) SHL Voice;
        { See IF any waveform IRQs first }
        IF (IRQ_Source AND VOICE_WAVE_IRQ) = 0 THEN
          IF (Wave_Ignore AND Voice_Bit) = 0 THEN
            BEGIN
              Wave_Ignore := Wave_Ignore OR Voice_Bit;

              PORT[GUSData.Voice_Select] := Lo(Voice);
              PORT[GUSData.Reg_Select] := GET_CONTROL;
              Temp1 := PORT[GUSData.Data_Hi];

              PORT[GUSData.Reg_Select] := GET_VOLUME_CONTROL;
              Temp2 := PORT[GUSData.Data_Hi];

              { IF either looping is on OR Rollover is on, don't stop voice }
              IF ((temp1 AND VC_Loop_Enable) OR (Temp2 AND VC_ROLLOVER)) = 0 THEN
                UltraStopVoice(Voice);

              { Call waveform Processing FUNCTION }
              GUSData.WaveTable_Func(Voice);
            END;

        IF (IRQ_Source AND VOICE_VOLUME_IRQ) = 0 THEN
          IF (Volume_Ignore AND Voice_Bit) = 0 THEN
            BEGIN
              Volume_Ignore := Volume_Ignore OR Voice_Bit;

              PORT[GUSData.Voice_Select] := Lo(Voice);
              PORT[GUSData.Reg_Select] := GET_VOLUME_CONTROL;
              Temp1 := PORT[GUSData.Data_Hi];

              { IF volume looping is enabled, don't stop it }
              IF (Temp1 AND VL_LOOP_ENABLE) = 0 THEN
                UltraStopVolume(Voice);

              { Call envelope Processing FUNCTION }
              GUSData.Volume_Func(Voice);
            END;
      END;
  END;


PROCEDURE GF1_Handler;
  CONST
    IRQ_Source  : BYTE = 0;  { Don't put these on the stack - speed }
    MIDI_Status : WORD = 0;
    Data        : WORD = 0;
  BEGIN
    { IRQs that will be vectored here:
        1: DAC Sampling (PC DMA)
        2: Voice Volume Envelope
        3: Wave table END
        4: DMA TO DRAM
        5: Possible MIDI }

    { Handle ALL irqs that may be pending }

    WHILE TRUE DO
      BEGIN
        { First, find out who has an interrupt pending }
        IRQ_Source := PORT[GUSData.IRQ_Status];
        IF IRQ_Source=0 THEN
          BEGIN
            GUSData.Aux_Irq_Func;  { aux interrupt handler (ultramax) }
            EXIT; { No more IRQs }
          END;

        IF (IRQ_Source AND DMA_Tc_IRQ) <> 0 THEN
          Handle_DMA_Tc;

        IF (IRQ_Source AND (MIDI_TX_IRQ OR MIDI_RX_IRQ)) <> 0 THEN
          BEGIN
            MIDI_Status := PORT[GUSData.MIDI_Control];

            IF (IRQ_Source AND MIDI_TX_IRQ) <> 0 THEN
              GUSData.MIDI_XMIT_Func(MIDI_Status);

            IF (IRQ_Source AND MIDI_RX_IRQ) <> 0 THEN
              BEGIN
                { Reading data will clear IRQ }
                Data := PORT[GUSData.MIDI_Data];
                { Cal MIDI receive data processing FUNCTION }
                GUSData.MIDI_Recv_Func(MIDI_STATUS, Data);
              END;
          END;

        IF (IRQ_Source AND GF1_TIMER1_IRQ) <> 0 THEN
          BEGIN
            ASM
              PUSHF
              CLI
            END;
            PORT[GUSData.Reg_Select] := TIMER_CONTROL;
            PORT[GUSData.Data_Hi] := (GUSData.TIMER_Ctrl AND (NOT $04));

            PORT[GUSData.Reg_Select] := TIMER_CONTROL;
            PORT[GUSData.Data_Hi] := GUSData.TIMER_Ctrl;

            GUSData.Timer1_Func;
            ASM
              POPF
            END;
          END;

        IF (IRQ_Source AND GF1_TIMER2_IRQ) <> 0 THEN
          BEGIN
            ASM
              PUSHF
              CLI
            END;
            PORT[GUSData.Reg_Select] := TIMER_CONTROL;
            PORT[GUSData.Data_Hi] := (GUSData.TIMER_Ctrl AND (NOT $08));

            PORT[GUSData.Reg_Select] := TIMER_CONTROL;
            PORT[GUSData.Data_Hi] := GUSData.TIMER_Ctrl;

            GUSData.Timer2_Func;
            ASM
              POPF
            END;
          END;

        IF (IRQ_Source AND (WAVETABLE_IRQ OR ENVELOPE_IRQ)) <> 0 THEN
          Handle_Voice;

      END;
  END;


PROCEDURE GF1_IRQ_Handler; INTERRUPT;
  CONST
    IRQ_Num : INTEGER = 0;  { Make this static - dont put on stack }
  BEGIN
    IRQ_Num := GUSData.GF1_IRQ_Num;

    { Clear PC's interrupt controller(s) }
    PORT[_GF1_IRQ[IRQ_Num].OCR] := _GF1_IRQ[IRQ_Num].Spec_EOI;

    { Gotta send EOI TO BOTH Controllers }
    IF IRQ_Num > 7 THEN
      PORT[OCR1] := EOI;

    { Go TO handler }
    GF1_Handler;
  END;


PROCEDURE MIDI_IRQ_Handler; INTERRUPT;
  CONST
    IRQ_Num : INTEGER = 0; { Make this static - dont put on stack }
  BEGIN
    IRQ_Num := GUSData.MIDI_IRQ_Num;

    { Clear PC's interrupt controller(s) }
    PORT[_GF1_IRQ[IRQ_Num].OCR] := _GF1_IRQ[IRQ_Num].Spec_EOI;

    { Gotta send EOI TO BOTH Controllers }
    IF IRQ_Num > 7 THEN
      PORT[OCR1] := EOI;

    { Go TO handler }
    GF1_Handler;
  END;


PROCEDURE SetIRQHandlers(GF1_IRQ  : BYTE;
                         MIDI_IRQ : BYTE);
  VAR
    Temp_IRQ : BYTE;
    PPoint   : POINTER;
  BEGIN
    Temp_IRQ := GF1_IRQ;

    IF (Temp_IRQ <> 0) THEN
      BEGIN
        IF GF1_IRQ > 7 THEN
          GF1_IRQ := GF1_IRQ + $68
        ELSE
          GF1_IRQ := GF1_IRQ + $08;

        { Shove PROCEDURE address into os variable }
        PPoint := GF1_GetVect(GF1_IRQ);
        Move(PPoint, GUSData.Old_GF1_Vec, SizeOf(PPoint));

        GF1_SetVect(GF1_IRQ, @GF1_IRQ_Handler);
      END;

    IF ((Temp_IRQ <> MIDI_IRQ) AND (MIDI_IRQ <> 0)) THEN
      BEGIN
        IF MIDI_IRQ > 7 THEN
          MIDI_IRQ := MIDI_IRQ + $68
        ELSE
          MIDI_IRQ := MIDI_IRQ + $08;

        { Shove PROCEDURE address into os variable }
        PPoint := GF1_GetVect(MIDI_IRQ);
        Move(PPoint, GUSData.Old_MIDI_Vec, SizeOf(PPoint));

        GF1_SetVect(MIDI_IRQ, @MIDI_IRQ_Handler);
      END
  END;


{ This FUNCTION returns the # OF K found on the card }
FUNCTION UltraSizeDRAM : INTEGER;
  VAR
    I,
    Loc       : LONGINT;
    Val,
    Save0,
    Save1     : BYTE;
    Base_Port : WORD;
    BreakOut  : BOOLEAN;
  BEGIN
    UltraSizeDRAM := 0;

    Base_Port := GUSData.Base_Port;

    { Save First Location }
    Save0 := UltraPeekData(Base_Port, 0);

    { See IF there is first block there ... }
    UltraPokeData(Base_Port, 0, $AA);
    IF (UltraPeekData(Base_Port,0) <> $AA) THEN
      EXIT;

    { Now zero it out so that I can check FOR mirroring }
    UltraPokeData(Base_Port, 0, $00);

    BreakOut := FALSE;
    I := 0;
    WHILE (NOT BreakOut) AND
          (I < 1023) DO
      BEGIN
        INC(I);

        { Check FOR mirroring }
        Val := UltraPeekData(Base_Port, 0);
        IF Val = 0 THEN
          BEGIN
            Loc := LONGINT(I) SHL 10;

            { Save Location so its a non-destructive sizing }
            Save1 := UltraPeekData(Base_Port, Loc);

            UltraPokeData(Base_Port, Loc, $AA);
            IF (UltraPeekData(Base_Port, Loc) <> $AA) THEN
              BreakOut := TRUE
            ELSE
              UltraPokeData(Base_Port, Loc, Save1);
          END
        ELSE
          BreakOut := TRUE;
      END;

    { Now restore location zero ... }
    UltraPokeData(Base_Port,0,Save0);
    UltraSizeDRAM := I;
  END;


{ ------------------------------------------------------------------------- }
{ This is the new memory control system by Kurt Kennett                     }
{ ------------------------------------------------------------------------- }

PROCEDURE UltraMemInit;
  VAR
    Runner : BYTE;
    Maker  : PNode;
  BEGIN
    {
    UMemStruc := SizeOf(UMemBlock);
    report heap space used only. }
    FillChar(UMemBlock, SizeOf(UMemBlock), 0);
    FOR Runner := 0 TO 3 DO
      WITH UMemBlock[Runner+1] DO
        BEGIN
          BaseOffset := Runner * (BlockSizeK*OneK);
          New(List);
          INC(UMemStruc, SizeOf(List^));
          FillChar(List^, SizeOf(List^), 0);
          List^.EndLoc := BlockSizeK*OneK-1;
          List^.Next   := NIL;
          List^.Prev   := NIL;
        END;
    UMemInited := TRUE;
  END;

PROCEDURE UltraMemClose;
  VAR
    Runner : BYTE;
    Killer,
    MList  : PNode;
  BEGIN
    FOR Runner := 1 TO 4 DO
      BEGIN
        MList := UMemBlock[Runner].List;
        WHILE MList <> NIL DO
          BEGIN
            Killer := MList;
            MList := MList^.Next;
            DEC(UMemStruc, SizeOf(Killer^));
            Dispose(Killer);
          END;
      END;
  END;

FUNCTION UltraMaxAvail : LONGINT;
  VAR
    Largest : LONGINT;
    MList   : PNode;
    Count   : BYTE;
  BEGIN
    Largest := 0;
    FOR Count := 1 TO 4 DO
      BEGIN
        MList := UMemBlock[Count].List;
        WHILE MList <> NIL DO
          BEGIN
            IF (MList^.EndLoc-MList^.StartLoc+1) > Largest THEN
              Largest := (MList^.EndLoc-MList^.StartLoc+1);
            MList := MList^.Next;
          END;
      END;
    UltraMaxAvail := Largest;
  END;

FUNCTION UltraMaxAlloc : LONGINT;
  BEGIN
    UltraMaxAlloc := UltraMaxAvail;
  END;

FUNCTION UltraMemAvail : LONGINT;
  VAR
    MemCount : LONGINT;
    MList    : PNode;
    Count    : BYTE;
  BEGIN
    MemCount := 0;
    FOR Count := 1 TO 4 DO
      BEGIN
        MList := UMemBlock[Count].List;
        WHILE MList <> NIL DO
          BEGIN
            INC(MemCount, (MList^.EndLoc-MList^.StartLoc+1));
            MList := MList^.Next;
          END;
      END;
    UltraMemAvail := MemCount;
  END;

FUNCTION UltraMemAlloc(    Size     : LONGINT;
                       VAR Location : LONGINT) : BOOLEAN;
  VAR
    BlockUse : BYTE;
    BlockEnd : LONGINT;
    MList    : PNode;
  BEGIN
    UltraMemAlloc := FALSE;
    IF NOT UMemInited THEN
      BEGIN
        { Memory structures NOT initialized }
        Location := -1;
        EXIT;
      END;
    IF (Size MOD 32) <> 0 THEN
      INC(Size, 32 - (Size MOD 32)); { bring size up TO a 32 BYTE boundary }
    IF Size > BlockSizeK*OneK THEN
      BEGIN
        { Size is bigger than allowed sample size }
        Location := -1;
        EXIT;
      END;
    BlockUse := 1;
    WHILE BlockUse <= MaxNumBanks DO
      BEGIN
        MList := UMemBlock[BlockUse].List;
        { Scan the list FOR a free space TO use }
        WHILE MList <> NIL DO
          BEGIN
            IF (MList^.EndLoc-MList^.StartLoc+1) >= Size THEN
              BEGIN
                { Set the END OF the block TO allocate }
                BlockEnd := MList^.StartLoc + Size - 1;

                { Prepare the location parameter WITH the actual address }
                Location := UMemBlock[BlockUse].BaseOffset + MList^.StartLoc;

                { Set the free block's new start position }
                MList^.StartLoc := BlockEnd+1;

                { EXIT WITH a TRUE result }
                UltraMemAlloc := TRUE;
                ClearError;
                EXIT;
              END;
            MList := MList^.Next;
          END;
        INC(BlockUse);
      END;

    { No memory left FOR block OF specified size }
    UltraOk := FALSE;
    UltraError := INSUFFICIENT_GUS_MEM;
    UltraErrorStr := ErrorStrings[INSUFFICIENT_GUS_MEM];
  END;


FUNCTION UltraMemFree(    Size : LONGINT;
                      Location : LONGINT) : BOOLEAN;
  VAR
    BlockUse : BYTE;
    NextNext,
    MList,
    Temp     : PNode;
  BEGIN
    UltraMemFree := FALSE;
    IF NOT UMemInited THEN
      BEGIN
        { Memory structures NOT initialized }
        EXIT;
      END;
    IF (Size MOD 32) <> 0 THEN
      INC(Size, 32 - (Size MOD 32)); { bring size up TO a 32 BYTE boundary }
    IF Size > BlockSizeK*OneK THEN
      BEGIN
        { Size is bigger than allowed sample size }
        EXIT;
      END;

    { find appropriate block }
    BlockUse := BYTE((Location DIV (BlockSizeK*OneK))+1);

    { create a temporary block at the specified position }
    New(Temp);
    INC(UMemStruc, SizeOf(Temp^));
    FillChar(Temp^, SizeOf(Temp^), 0);
    Temp^.StartLoc := Location - UMemBlock[BlockUse].BaseOffset;
    Temp^.EndLoc := Temp^.StartLoc + Size - 1;

    { Insert it at the correct position in the list }
    MList := UMemBlock[BlockUse].List;
    WHILE (MList <> NIL) AND
          (MList^.StartLoc < Temp^.StartLoc) DO
      MList := MList^.Next;
    IF MList= NIL THEN
      BEGIN
        DEC(UMemStruc, SizeOf(Temp^));
        Dispose(Temp);
        { Inappropriate location passed TO freemem }
        EXIT;
      END;
    Temp^.Next := MList;
    Temp^.Prev := MList^.Prev;
    IF Temp^.Prev <> NIL THEN
      Temp^.Prev^.Next := Temp;
    MList^.Prev := Temp;
    IF Temp^.StartLoc < UMemBlock[BlockUse].List^.StartLoc THEN
      UMemBlock[BlockUse].List := Temp;

    { merge consecutive blocks }
    MList := UMemBlock[BlockUse].List;
    WHILE MList <> NIL DO
      BEGIN
        IF MList^.Next <> NIL THEN
          BEGIN
            IF (MList^.Next^.StartLoc-1) = MList^.EndLoc THEN
              BEGIN
                MList^.EndLoc := MList^.Next^.EndLoc;
                NextNext := MList^.Next^.Next;
                DEC(UMemStruc,SizeOf(MList^.Next^));
                Dispose(MList^.Next);
                MList^.Next := NextNext;
                IF NextNext <> NIL THEN
                  NextNext^.Prev := MList;
              END
            ELSE
              MList := MList^.Next;
          END
        ELSE
          MList := MList^.Next;
      END;

    UltraMemFree := TRUE;
  END;


FUNCTION UltraOpen(VAR Config : ULTRA_CFG;
                       Voices : INTEGER) : BOOLEAN;
  VAR
    Temp : BOOLEAN;
  BEGIN
    GUSData.Base_Port := Config.Base_Port;
    GUSData.DRAM_DMA_Chan := Config.DRAM_DMA_Chan;
    GUSData.ADC_DMA_Chan := Config.ADC_DMA_Chan;
    GUSData.GF1_IRQ_Num := Config.GF1_IRQ_Num;
    GUSData.MIDI_IRQ_Num := Config.MIDI_IRQ_Num;
    GUSData.Mix_Image := $0B;
    GUSData.Voices := Voices;

    Temp := UltraProbe(GUSData.Base_Port);
    IF NOT Temp THEN
      BEGIN
        UltraOpen := FALSE;
        EXIT;
      END;

    UltraDisableLineIn;
    UltraDisableMicIn;
    UltraDisableOutput;

    Temp := UltraReset(Voices);
    IF NOT Temp THEN
      BEGIN
        UltraOpen := FALSE;
        EXIT;
      END;

    UltraSetInterface(GUSData.DRAM_DMA_Chan,
                      GUSData.ADC_DMA_Chan,
                      GUSData.GF1_IRQ_Num,
                      GUSData.MIDI_IRQ_Num);

    SetIRQHandlers(GUSData.GF1_IRQ_Num,
                   GUSData.MIDI_IRQ_Num);

    SetIRQs(GUSData.GF1_IRQ_Num,
            GUSData.MIDI_IRQ_Num);

    UltraEnableOutput;

    UltraMemInit;

    UltraOpen := TRUE;
    ClearError;
  END;


FUNCTION UltraClose : BOOLEAN;
  BEGIN
    UltraDisableOutput;
    UltraDisableLineIn;
    UltraDisableMicIn;
    UltraReset(14);
    ResetIRQs(GUSData.GF1_IRQ_Num,
              GUSData.MIDI_IRQ_Num);
    ResetIRQHandlers(GUSData.GF1_IRQ_Num,
                     GUSData.MIDI_IRQ_Num);
    UltraMemClose;
    UltraClose := TRUE;
    ClearError;
  END;


PROCEDURE Setup_GUSData;
  VAR
    Proc_Addr         : PFV;
    IntProc_Addr      : Int_Proc;
    WordProc_Addr     : WORD_Proc;
    TwoWord_Proc_Addr : TwoWord_Proc;
  BEGIN
    Proc_Addr := Default_Proc;
    IntProc_Addr := Default_Int_Proc;
    WordProc_Addr := Default_WORD_Proc;
    TwoWord_Proc_Addr := Default_TwoWord_Proc;
    FillChar(GUSData, SizeOf(GUSData), 0);
    WITH GUSData DO
      BEGIN
        Flags              := ULTRA_PRESENT;
        Base_Port          := 220;
        DRAM_DMA_Chan      := 1;
        ADC_DMA_Chan       := 1;
        GF1_IRQ_Num        := 11;
        MIDI_IRQ_Num       := 5;
        Old_GF1_Vec        := Proc_Addr; { TO be reset.  FOR tp6.0 compatiblity }
        Old_MIDI_Vec       := Proc_Addr; { TO be reset.  FOR tp6.0 compatiblity }
        MIDI_XMit_Func     := WordProc_Addr;
        MIDI_Recv_Func     := TwoWord_Proc_Addr;
        Timer1_Func        := Proc_Addr;
        Timer2_Func        := Proc_Addr;
        WaveTable_Func     := IntProc_Addr;
        Volume_Func        := IntProc_Addr;
        DRAM_DMA_TC_Func   := Proc_Addr;
        RECORD_DMA_TC_Func := Proc_Addr;
        Aux_IRQ_Func       := Proc_Addr;
        Mix_Image          := $08;
        Voices             := 32;
        Image_MIDI         := 0;
        Used_Voices        := 0;
      END;
  END;


PROCEDURE UltraStopVolume(Voice : INTEGER);
  VAR
    VlMode : BYTE;
  BEGIN
    ASM
      PUSHF
      CLI
    END;
    { Select the proper voice }
    PORT[GUSData.Voice_Select] := Lo(Voice);

    PORT[GUSData.Reg_Select] := GET_VOLUME_CONTROL;
    VlMode := PORT[GUSData.Data_Hi];
    VlMode := VLMode OR (VOLUME_STOPPED OR STOP_VOLUME);

    PORT[GUSData.Reg_Select] := SET_VOLUME_CONTROL;
    PORT[GUSData.Data_Hi] := VlMode;
    GF1_Delay;
    PORT[GUSData.Data_Hi] := VlMode;
    ASM
      POPF
    END;
  END;


PROCEDURE UltraSetVolume(Voice  : INTEGER;
                         Volume : WORD);
  BEGIN
    Volume := Volume SHL 4;

    ASM
      PUSHF
      CLI
    END;
    PORT[GUSData.Voice_Select] := Lo(Voice);
    PORT[GUSData.Reg_Select] := SET_VOLUME;
    PORTW[GUSData.Data_LOW] := Volume;
    ASM
      POPF
    END;
  END;


PROCEDURE UltraRampVolume(Voice  : INTEGER;
                          StartV : WORD;    { Linear }
                          EndV   : WORD;    { Linear }
                          VRate  : BYTE;
                          VMode  : BYTE);   { Looping, etc }
  VAR
    BeginV : WORD;
    VlMode : BYTE;
  BEGIN
    IF StartV=EndV THEN  { Don't bother if we don't have to }
      Exit;

    { IF the start volume is greater than the END volume, flip them AND
      turn on decreasing volume. NOTe that the GF1 requires that the
      programmed start volume MUST be less than OR equal TO the END
      volume. }

    { Don't let bat bits thru ... }
    VMode := VMode AND (NOT (VL_IRQ_PENDING OR VC_ROLLOVER OR STOP_VOLUME OR VOLUME_STOPPED));

    BeginV := StartV;

    IF (StartV > EndV) THEN
      BEGIN
        { Flip the start AND END volumes AND set mode decreasing }
        StartV := EndV;
        EndV := BeginV;
        VMode := VMode OR VC_DIRECT; { Set decreasing }
      END;

    { looping below 64 OR greater than 4032 can cause strange things }
    IF StartV < 64 THEN
      StartV := 64;

    IF EndV > 4032 THEN
      EndV := 4032;

    ASM
      PUSHF
      CLI
    END;
    PORT[GUSData.Voice_Select] := Lo(Voice);
    PORT[GUSData.Reg_Select] := SET_VOLUME_RATE;
    PORT[GUSData.Data_Hi] := VRate;
    PORT[GUSData.Reg_Select] := SET_VOLUME_START;
    PORT[GUSData.Data_Hi] := BYTE(StartV SHR 4);
    PORT[GUSData.Reg_Select] := SET_VOLUME_END;
    PORT[GUSData.Data_Hi] := BYTE(EndV SHR 4);

    { Also must set the current volume TO the StartV volume }
    UltraSetVolume(Voice, BeginV);

    PORT[GUSData.Reg_Select] := GET_VOLUME_CONTROL;
    VlMode := PORT[GUSData.Data_Hi];
    IF (VlMode AND VC_ROLLOVER) <> 0 THEN
      VMode := VMode OR VC_ROLLOVER;

    { Start 'er up!! }
    PORT[GUSData.Reg_Select] := SET_VOLUME_CONTROL;
    PORT[GUSData.Data_Hi] := VMode;
    GF1_DELAY;
    PORT[GUSData.Data_Hi] := VMode;
    ASM
      POPF
    END;
  END;


FUNCTION UltraReadVolume(Voice : INTEGER) : WORD;
  VAR
    Volume : WORD;
  BEGIN
    ASM
      PUSHF
      CLI
    END;
    { Make sure we are talking TO the proper voice }
    PORT[GUSData.Voice_Select] := Lo(Voice);

    PORT[GUSData.Reg_Select] := GET_VOLUME;
    Volume := PORTW[GUSData.Data_LOW];
    Volume := Volume SHR 4;
    ASM
      POPF
    END;

    UltraReadVolume := Volume;
  END;


PROCEDURE UltraVectorVolume(Voice : INTEGER;
                            VEnd  : WORD;
                            VRate : BYTE;
                            VMode : BYTE);
  VAR
    Cur_Vol : WORD;
  BEGIN
    UltraStopVolume(Voice);
    Cur_Vol := UltraReadVolume(Voice);
    UltraRampVolume(Voice, Cur_Vol, VEnd, VRate, VMode);
  END;


FUNCTION UltraCalcRate(StartV   : WORD;
                       EndV     : WORD;
                       Mil_Secs : LONGINT) : BYTE;
  VAR
    Gap,
    Mic_Secs   : LONGINT;
    I,
    Range,
    Increment,
    Value      : WORD;
    Rate_Val   : BYTE;
  BEGIN
    IF StartV > EndV THEN
      Gap := StartV - EndV
    ELSE
      Gap := EndV - StartV;

    { Long INTEGER division. SLOW. }
    Mic_Secs := (Mil_Secs * 1000) DIV Gap;

    { We now have the # OF microseconds FOR each update TO go from
      A TO B in X milliseconds. See what the best fit is in the table }
    Range := 4;
    Value := Vol_Rates[GUSData.Voices-14];

    I := 0;
    WHILE I < 3 DO
      BEGIN
        IF (Mic_Secs < Value) THEN
          BEGIN
            Range := I;
            I := 3;
          END
        ELSE
          Value := (Value SHL 3);
        INC(I);
      END;

    IF Range=4 THEN
      BEGIN
        Range := 3;
        Increment := 1;
      END
    ELSE
      { Calculate increment value }
      Increment := WORD((Value + (Value SHR 1)) DIV Mic_Secs);

    Rate_Val := BYTE(Range SHL 6);

    Rate_Val := Rate_Val OR (Increment AND $3F);

    UltraCalcRate := Rate_Val;
    ClearError;
  END;


PROCEDURE UltraSetLinearVolume(Voice : INTEGER;
                               INDEX : INTEGER);
  VAR
    Volume : WORD;
  BEGIN
    INDEX := (INDEX AND $01FF);  { only 0 to 511 please }

    { Use the table TO get a volume }
    Volume := _GF1_Volumes[INDEX];
    UltraSetVolume(Voice, Volume);
  END;


FUNCTION UltraReadLinearVolume(Voice : INTEGER) : INTEGER;
  VAR
    Volume : WORD;
    INDEX  : WORD;
  BEGIN
    Volume := UltraReadVolume(Voice);
    INDEX := 1;
    WHILE (INDEX < 511) AND
          (_GF1_Volumes[INDEX] < Volume) DO
      INC(INDEX);
    UltraReadLinearVolume := INDEX;
  END;


PROCEDURE UltraRampLinearVolume(Voice     : INTEGER;
                                Start_Idx : WORD;    { Linear Start Volume }
                                END_Idx   : WORD;    { Linear END Volume }
                                Msecs     : LONGINT;
                                VMode     : BYTE);
  VAR
    StartV : WORD;
    EndV   : WORD;
    VRate  : BYTE;
  BEGIN
    { Ramp from start TO END in x milliseconds }
    StartV := _GF1_Volumes[Start_Idx];
    EndV := _GF1_Volumes[END_Idx];

    { Calculate a rate TO get from start TO END in msec millisecs }
    VRate := UltraCalcRate(StartV, EndV, MSecs);

    { Ramp the sucker }
    UltraRampVolume(Voice, StartV, EndV, VRate, VMode);
  END;

PROCEDURE UltraVectorLinearVolume(Voice   : INTEGER;
                                  END_Idx : WORD;  { Linear END Volume }
                                  VRate   : BYTE;  { 0-63 rate }
                                  VMode   : BYTE);
  VAR
    Cur_Vol : WORD;
    EndV    : WORD;
  BEGIN
    UltraStopVolume(Voice);
    Cur_Vol := UltraReadVolume(Voice);

    EndV := _GF1_Volumes[(END_Idx AND $1FF)];

    UltraRampVolume(Voice, Cur_Vol, EndV, VRate, VMode);
  END;


FUNCTION UltraVolumeStopped(Voice : INTEGER) : BOOLEAN;
  VAR
    VMode : BYTE;
  BEGIN
    ASM
      PUSHF
      CLI
    END;
    { Make sure we are talking TO the proper voice }
    PORT[GUSData.Voice_Select] := Lo(Voice);
    PORT[GUSData.Reg_Select] := GET_VOLUME_CONTROL;

    { Check the volume ramp bits }
    VMode := PORT[GUSData.Data_Hi];
    ASM
      POPF
    END;

    UltraVolumeStopped := (VMode AND (VOLUME_STOPPED OR STOP_VOLUME)) <> 0;
  END;


PROCEDURE UltraSetFrequency(Voice     : INTEGER;
                            Speed_Khz : LONGINT);
  VAR
    Fc   : LONGINT;
    Temp : LONGINT;
  BEGIN
    { FC is calculated based on the # OF active voices }

    Temp := Freq_Divisor[GUSData.Voices-14];

    Fc := ((Speed_Khz SHL 9) + (Temp SHR 1)) DIV Temp;

    Fc := Fc SHL 1;

{    Fc:=Round(Speed_kHz/(Freq_Divisor[GUSData.Voices-14]/1000));}

    ASM
      PUSHF
      CLI
    END;
    PORT[GUSData.Voice_Select] := Lo(Voice);
    PORT[GUSData.Reg_Select] := SET_FREQUENCY;
    PORTW[GUSData.Data_LOW] := Fc;
    ASM
      POPF
    END;
  END;

FUNCTION UltraPrimeVoice(Voice    : INTEGER;
                         VBegin   : LONGINT;  { Phys start loc }
                         VStart   : LONGINT;  { Phys loop start loc }
                         VEnd     : LONGINT;  { Phys loop END loc }
                         VMode    : BYTE) : BYTE; { Looping, etc. }
  VAR
    VlMode    : BYTE;
    Temp,
    PBeginLoc,
    PEndLoc,
    PStartLoc : LONGINT;
  BEGIN
    IF VStart > VEnd THEN
      BEGIN
        { Start > END, so flip AND turn on decrementing addressed }
        Temp := VStart;
        VStart := VEnd;
        VEnd := Temp;
        VMode := VMode OR VC_DIRECT;
      END;

    IF (VMode AND VC_DATA_TYPE) <> 0 THEN
      BEGIN
        { 16 bit data, so must convert addresses }
        PBeginLoc := Convert_TO_16Bit(VBegin);
        PStartLoc := Convert_TO_16Bit(VStart);
        PEndLoc := Convert_TO_16Bit(VEnd);
      END
    ELSE
      BEGIN
        PBeginLoc := VBegin;
        PStartLoc := VStart;
        PEndLoc := VEnd;
      END;

    ASM
      PUSHF
      CLI
    END;
    { Make sure we are talking TO the proper voice }
    PORT[GUSData.Voice_Select] := Lo(Voice);

    { Set/Reset the rollover bit as per user request }
    PORT[GUSData.Reg_Select] := GET_VOLUME_CONTROL;
    VlMode := PORT[GUSData.Data_Hi];
    IF (VMode AND USE_ROLLOVER) <> 0 THEN
      VLMode := VLMode OR VC_ROLLOVER
    ELSE
      VLMode := VLMode AND (NOT VC_ROLLOVER);
    PORT[GUSData.Reg_Select] := SET_VOLUME_CONTROL;
    PORT[GUSData.Data_Hi] := VlMode;
    GF1_Delay;
    PORT[GUSData.Data_Hi] := VlMode;

    { First set accumulator TO beginning OF data }
    PORT[GUSData.Reg_Select] := SET_ACC_LOW;
    PORTW[GUSData.Data_LOW] := ADDR_LOW(PBeginLoc);
    PORT[GUSData.Reg_Select] := SET_ACC_HIGH;
    PORTW[GUSData.Data_LOW] := ADDR_HIGH(PBeginLoc);

    { Set start loop address OF buffer }
    PORT[GUSData.Reg_Select] := SET_START_HIGH;
    PORTW[GUSData.Data_LOW] := ADDR_HIGH(PStartLoc);
    PORT[GUSData.Reg_Select] := SET_START_LOW;
    PORTW[GUSData.Data_LOW] := ADDR_LOW(PStartLoc);

    { Set END address OF buffer }
    PORT[GUSData.Reg_Select] := SET_END_HIGH;
    PORTW[GUSData.Data_LOW] := ADDR_HIGH(PEndLoc);
    PORT[GUSData.Reg_Select] := SET_END_LOW;
    PORTW[GUSData.Data_LOW] := ADDR_LOW(PEndLoc);
    ASM
      POPF
    END;

    UltraPrimeVoice := VMode;
  END;


PROCEDURE UltraGoVoice(Voice : INTEGER;
                       VMode : BYTE);
  BEGIN
    ASM
      PUSHF
      CLI
    END;
    PORT[GUSData.Voice_Select] := Lo(Voice);
    { Turn 'stop' bits off .. }
    VMode := VMode AND (NOT (VOICE_STOPPED OR STOP_VOICE));

    { NOTE: no irq's from the voice ... }
    PORT[GUSData.Reg_Select] := SET_CONTROL;
    PORT[GUSData.Data_Hi] := VMode;

    GF1_Delay;

    PORT[GUSData.Data_Hi] := VMode;
    ASM
      POPF
    END;
  END;

{ This FUNCTION will start playing a wave out OF DRAM. It assumes
  the playback rate, volume & balance have been set up before ... }

PROCEDURE UltraStartVoice(Voice  : INTEGER;
                          VBegin : LONGINT;
                          VStart : LONGINT;
                          VEnd   : LONGINT;
                          VMode  : BYTE);
  BEGIN
    VMode := UltraPrimeVoice(Voice, VBegin, VStart, VEnd, VMode);
    UltraGoVoice(Voice, VMode);
  END;


PROCEDURE UltraSetLoopMode(Voice : INTEGER;
                           VMode : BYTE);
  VAR
    Data : BYTE;
  BEGIN
    ASM
      PUSHF
      CLI
    END;
    PORT[GUSData.Voice_Select] := Lo(Voice);

    { set/reset the rollover bit as per user request }
    PORT[GUSData.Reg_Select] := GET_VOLUME_CONTROL;
    VMode := PORT[GUSData.Data_Hi];

    IF (VMode AND USE_ROLLOVER) <> 0 THEN
      VMode := VMode OR VC_ROLLOVER
    ELSE
      VMode := VMode AND (NOT VC_ROLLOVER);
    PORT[GUSData.Reg_Select] := SET_VOLUME_CONTROL;
    PORT[GUSData.Data_Hi] := VMode;
    GF1_Delay;
    PORT[GUSData.Data_Hi] := VMode;

    PORT[GUSData.Reg_Select] := GET_CONTROL;
    Data := PORT[GUSData.Data_Hi];

    { Isolate the loop bits AND make sure no bad bits are passed }
    Data := Data AND (NOT (VC_WAVE_IRQ OR VC_BI_LOOP OR VC_LOOP_ENABLE));
    Data := Data AND (VC_WAVE_IRQ OR VC_BI_LOOP OR VC_LOOP_ENABLE);

    { Turn on proper bits }
    Data := Data OR VMode;

    PORT[GUSData.Reg_Select] := SET_CONTROL;
    PORT[GUSData.Data_Hi] := Data;

    GF1_Delay;

    PORT[GUSData.Reg_Select] := SET_CONTROL;
    PORT[GUSData.Data_Hi] := Data;
    ASM
      POPF
    END;
  END;


PROCEDURE UltraVoiceOn(Voice : INTEGER;
                       VBegin : LONGINT;
                       Start_Loop : LONGINT;
                       END_Loop : LONGINT;
                       Control  : BYTE;
                       Freq     : LONGINT);
  BEGIN
    UltraSetFrequency(Voice, Freq);
    UltraStartVoice(Voice, VBegin, Start_Loop, END_Loop, Control);

    { NOTE: TO play a sample backwards, 'BEGIN' should be set TO address
      that you want it TO start at, start must be a lower address than the
      END. Also turn on the decreasing addresses bit in the control BYTE.
      Now the sample will BEGIN playing at BEGIN, WITH decreasing addresses
      thru the END location until it hits the start location. It will THEN
      loop back TO the END position, (IF looping enabled) }
  END;


PROCEDURE UltraVoiceOff(Voice : INTEGER;
                        VEnd  : BOOLEAN);
  BEGIN
    IF VEnd THEN
      UltraSetLoopMode(Voice, $00)
    ELSE
      UltraStopVoice(Voice);
  END;


PROCEDURE UltraSetBalance(Voice : INTEGER;
                          Data  : BYTE);
  VAR
    Temp : BYTE;
  BEGIN
    { Make sure balance is between 0 AND 15 }
    Temp := Data AND $0F;

    ASM
      PUSHF
      CLI
    END;
    PORT[GUSData.Voice_Select] := Lo(Voice);
    PORT[GUSData.Reg_Select] := SET_BALANCE;
    PORT[GUSData.Data_Hi] := Temp;
    ASM
      POPF
    END;
  END;


FUNCTION UltraReadVoice(Voice : INTEGER) : LONGINT;
  VAR
    Count_LOW  : WORD;
    Count_HIGH : WORD;
    Acc        : LONGINT;
    VMode      : BYTE;
  BEGIN
    ASM
      PUSHF
      CLI
    END;
    PORT[GUSData.Voice_Select] := Lo(Voice);

    PORT[GUSData.Reg_Select] := GET_ACC_HIGH;
    Count_HIGH := PORTW[GUSData.Data_LOW];
    PORT[GUSData.Reg_Select] := GET_ACC_LOW;
    Count_LOW := PORTW[GUSData.Data_LOW];

    { Convert from the ultrasound's format TO a physical address }
    PORT[GUSData.Reg_Select] := GET_CONTROL;
    VMode := PORT[GUSData.Data_Hi];
    ASM
      POPF
    END;

    Acc := Make_Physical_Address(Count_LOW, Count_HIGH, VMode);

    UltraReadVoice := Acc AND $000FFFFF;  { 20 bits only please }
  END;


FUNCTION UltraRecordDMABusy : BOOLEAN;
  BEGIN
    UltraRecordDMABusy := (_GF1_DMA[GUSData.ADC_DMA_CHAN-1].Flags AND DMA_Pending) <> 0;
  END;

PROCEDURE UltraWaitRecordDMA;
  BEGIN
    GUSData.Flags := GUSData.Flags AND (NOT ADC_DMA_NOWAIT);

    ASM
      PUSHF
      STI
    END;
    { Wait FOR IRQs TO clear this }
    WHILE (GUSData.Flags AND ADC_DMA_BUSY) <> 0 DO;
    ASM
      POPF
    END;
  END;

PROCEDURE UltraSetRecordFrequency(Rate : LONGINT);
  VAR
    adsr : BYTE;
  BEGIN
    { first calculate as IF its FOR a record. }
    Adsr := BYTE(((CLOCK_RATE SHR 4) DIV Rate)) - 2;

    ASM
      PUSHF
      CLI
    END;
    PORT[GUSData.Reg_Select] := SET_SAMPLE_RATE;
    PORT[GUSData.Data_Hi] := Adsr;
    ASM
      POPF
    END;
  END;

FUNCTION UltraPrimeRecord(PC_PTR  : POINTER;
                          Size    : WORD;
                          RRepeat : BOOLEAN) : BOOLEAN;
  VAR
    Vmode : INTEGER;
  BEGIN
    IF RRepeat THEN
      VMode := INDEF_Read
    ELSE
      VMode := READ_DMA;

    { Make sure the channel is NOT busy (recording OR playback) }
    UltraPrimeRecord := (PrimeDMA(PC_PTR, VMode, Size, GUSData.ADC_DMA_Chan) = ULTRA_OK);
  END;

FUNCTION UltraGoRecord(Control : BYTE) : BOOLEAN;
  VAR
    TDMAPtr : ^DMA_ENTRY;
  BEGIN
    TDMAPtr := @_GF1_DMA[GUSData.Adc_DMA_Chan-1];

    { Set the flag that the IRQ handler clears when xfer is complete }
    GUSData.Flags := GUSData.Flags OR ADC_DMA_BUSY;

    { Now tell the GF1 TO start the xfer }
    TDMAPtr^.Cur_Control := Control;

    UltraStartRecordDMA(Control);

    UltraGoRecord := TRUE;
    ClearError;
  END;


FUNCTION UltraRecordData(PC_PTR : POINTER;
                         Control : BYTE;
                         Size    : WORD;
                         Wait    : BOOLEAN;
                         RRepeat : BOOLEAN) : BOOLEAN;
  BEGIN
    IF NOT UltraPrimeRecord(PC_PTR, Size, RRepeat) THEN
      BEGIN
        UltraRecordData := FALSE;
        EXIT;
      END;

    UltraGoRecord(Control);

    IF Wait THEN
      UltraWaitRecordDMA
    ELSE
      GUSData.Flags := GUSData.Flags OR ADC_DMA_NOWAIT;

    UltraRecordData := TRUE;
    ClearError;
  END;


FUNCTION GetRecordDMAPos(Chan_Num : INTEGER) : WORD;
  CONST
    Threshold : WORD    = 30;
    I         : INTEGER =  5;
  VAR
    TDMAPtr      : ^DMA_ENTRY;
    Val1, Val2   : WORD;
    LOW1, LOW2   : WORD;
    HIGH1, HIGH2 : WORD;
    DumpOut      : BOOLEAN;
  BEGIN
    TDMAPtr := @_GF1_DMA[Chan_Num-1];

    ASM
      PUSHF
      CLI
    END;

    IF (TDMAPtr^.Flags AND CALIB_COUNT) <> 0 THEN
      BEGIN
        { This code is necessary to accomodate a virtualized DMA controller.
          If something (like emm386) virtualizes the DMA controller, we have
          to adjust our threshold point to accomodate the rollover problem
          in the DMA controller. The problem is when you read the low byte
          and the high byte rolls over before we get a chance to read it.
          A virtualized DMA controller aggravates the problem since it will
          take longer between reads ... }

        TDMAPtr^.Flags := TDMAPtr^.Flags AND (NOT CALIB_COUNT);

        WHILE I > 0 DO
          BEGIN
            Port[TDMAPtr^.Clear_FF] := 0;
            LOW1  := WORD(Port[TDMAPtr^.Count]);
            HIGH1 := WORD(Port[TDMAPtr^.Count]);
            LOW2  := WORD(Port[TDMAPtr^.Count]);
            HIGH2 := WORD(Port[TDMAPtr^.Count]);
            IF (HIGH1 = HIGH2) THEN
              BEGIN
                Threshold := ((LOW1 - LOW2) DIV 2) + 2;
                I := 0;
              END
            ELSE
              DEC(I);
          END;
      END;

    Val2 := 1;

    DumpOut := FALSE;
    WHILE NOT DumpOut DO
      BEGIN
        Port[TDMAPtr^.Clear_FF] := 0;
        LOW1  := WORD(Port[TDMAPtr^.Count]);
        HIGH1 := WORD(Port[TDMAPtr^.Count]);

        Val1 := (HIGH1 SHL 8) + LOW1;

        { If count is not about to roll over, use this count
          or if count equals 0xffff (dma complete) use this count }
        IF ((LOW1 > threshold) AND (LOW1 <> $FF)) OR
           (Val1 = $FFFF) THEN
          Val2 := Val1;

        IF Val2 = Val1 THEN
          DumpOut := TRUE
        ELSE
          Val2 := Val1;
      END;

    ASM
      POPF
    END;

    GetRecordDMAPos := Val2;
  END;


FUNCTION UltraReadRecordPosition : WORD;
  VAR
    TDMAPtr    : ^DMA_ENTRY;
    Actual_DMA : WORD;
    This_Size  : WORD;
    Total_Size : WORD;
  BEGIN
    TDMAPtr := @_GF1_DMA[GUSData.ADC_DMA_Chan-1];

    Actual_DMA := GetRecordDMAPos(GUSData.ADC_DMA_Chan);

    { Since it counts backwards, subtract this from the size OF the transfer }
    This_Size := TDMAPtr^.Cur_Size - Actual_DMA;

    { Now add in the amount sent (In case it crosses page) }
    Total_Size := TDMAPtr^.AMNT_Sent + This_Size;

    IF GUSData.ADC_DMA_CHAN >= 4 THEN
      Total_Size := Total_Size SHL 1;

    UltraReadRecordPosition := Total_Size;
  END;


PROCEDURE UltraSetVoice(Voice    : INTEGER;
                        Location : LONGINT);  { Physical start location }
  VAR
    Data     : BYTE;
  BEGIN
    ASM
      PUSHF
      CLI
    END;
    { Make sure we are talking TO the proper voice }
    PORT[GUSData.Voice_Select] := Lo(Voice);

    PORT[GUSData.Reg_Select] := GET_CONTROL;
    Data := PORT[GUSData.Data_Hi];

    IF (Data AND VC_DATA_TYPE) <> 0 THEN
      Location := Convert_TO_16Bit(Location);

    { First set accumulator TO beginning OF data }
    PORT[GUSData.Reg_Select] := SET_ACC_HIGH;
    PORTW[GUSData.Data_LOW] := ADDR_HIGH(Location);
    PORT[GUSData.Reg_Select] := SET_ACC_LOW;
    PORTW[GUSData.Data_LOW] := ADDR_LOW(Location);
    ASM
      POPF
    END;
  END;


PROCEDURE UltraSetVoiceEnd(Voice    : INTEGER;
                           VEnd     : LONGINT);  { Physical END location }
  VAR
    Data     : BYTE;
  BEGIN
    ASM
      PUSHF
      CLI
    END;
    { Make sure we are talking TO the proper voice }
    PORT[GUSData.Voice_Select] := Lo(Voice);

    PORT[GUSData.Reg_Select] := GET_CONTROL;
    Data := PORT[GUSData.Data_Hi];

    IF (Data AND VC_DATA_TYPE) <> 0 THEN
      VEnd := Convert_TO_16Bit(VEnd);

    { Make sure we are talking TO the proper voice }
    PORT[GUSData.Voice_Select] := Lo(Voice);

    { Set END address OF buffer }
    PORT[GUSData.Reg_Select] := SET_END_LOW;
    PORTW[GUSData.Data_LOW]  := ADDR_LOW(VEnd);
    PORT[GUSData.Reg_Select] := SET_END_HIGH;
    PORTW[GUSData.Data_LOW]  := ADDR_HIGH(VEnd);

    { turn 'stop' bits off }
    Data := Data AND (NOT (VC_IRQ_PENDING OR VOICE_STOPPED OR STOP_VOICE));

    PORT[GUSData.Reg_Select] := SET_CONTROL;
    PORT[GUSData.Data_Hi] := Data;
    GF1_Delay;
    PORT[GUSData.Reg_Select] := SET_CONTROL;
    PORT[GUSData.Data_Hi] := Data;
    ASM
      POPF
    END;
  END;


PROCEDURE UltraTrimJoystick(JoyVal : BYTE);
  BEGIN
    PORT[GUSData.Reg_Select] := SET_JOYSTICK;
    PORT[GUSData.Data_Hi] := JoyVal;
  END;


FUNCTION UltraVoiceStopped(Voice : INTEGER) : BOOLEAN;
  VAR
    VMode : BYTE;
  BEGIN
    ASM
      PUSHF
      CLI
    END;
    { Make sure we are talking TO the proper voice }
    PORT[GUSData.Voice_Select] := Lo(Voice);
    PORT[GUSData.Reg_Select] := GET_CONTROL;

    VMode := PORT[GUSData.Data_Hi];
    ASM
      POPF
    END;

    UltraVoiceStopped := ((VMode AND (VOICE_STOPPED OR STOP_VOICE)) <> 0);
  END;


PROCEDURE UltraMIDIXmit(data : BYTE);
  BEGIN
    PORT[GUSData.MIDI_Data] := Data;
  END;

FUNCTION UltraMIDIRecv : BYTE;
  BEGIN
    UltraMIDIRecv := PORT[GUSData.MIDI_Data];
  END;

FUNCTION UltraMIDIStatus : BYTE;
  BEGIN
    UltraMIDIStatus := PORT[GUSData.MIDI_Control];
  END;

PROCEDURE UltraMIDIEnableRecv;
  BEGIN
    GUSData.Image_MIDI := GUSData.Image_MIDI OR MIDI_ENABLE_RCV;
    PORT[GUSData.MIDI_Control] := GUSData.Image_MIDI;
  END;

PROCEDURE UltraMIDIEnableXmit;
  BEGIN
    GUSData.Image_MIDI := GUSData.Image_MIDI OR MIDI_ENABLE_XMIT;
    PORT[GUSData.MIDI_Control] := GUSData.Image_MIDI;
  END;

PROCEDURE UltraMIDIDisableRecv;
  BEGIN
    GUSData.Image_MIDI := GUSData.Image_MIDI AND (NOT MIDI_ENABLE_RCV);
    PORT[GUSData.MIDI_Control] := GUSData.Image_MIDI;
  END;

PROCEDURE UltraMIDIDisableXmit;
  BEGIN
    GUSData.Image_MIDI := GUSData.Image_MIDI AND (NOT MIDI_ENABLE_XMIT);
    PORT[GUSData.MIDI_Control] := GUSData.Image_MIDI;
  END;

PROCEDURE UltraMIDIReset;
  BEGIN
    GUSData.Image_MIDI := 0;
    PORT[GUSData.MIDI_Control] := MIDI_RESET;
    GF1_Delay;
    PORT[GUSData.MIDI_Control] := $00;
  END;


PROCEDURE UltraStartTimer(Timer : INTEGER;
                          Time  : BYTE);
  VAR
    Temp : BYTE;
  BEGIN
    IF Timer=1 THEN
      BEGIN
        GUSData.Timer_Ctrl := GUSData.Timer_Ctrl OR $04;
        GUSData.Timer_Mask := GUSData.Timer_Mask OR $01;
        Temp := Timer1;
      END
    ELSE
      BEGIN
        GUSData.Timer_Ctrl := GUSData.Timer_Ctrl OR $08;
        GUSData.Timer_Mask := GUSData.Timer_Mask OR $02;
        Temp := Timer2;
      END;

    ASM
      PUSHF
      CLI
    END;
    Time := BYTE(256 - time);
    PORT[GUSData.Reg_Select] := Temp;
    PORT[GUSData.Data_Hi] := Time;

    PORT[GUSData.Reg_Select] := TIMER_CONTROL;
    PORT[GUSData.Data_Hi] := GUSData.Timer_Ctrl;

    PORT[GUSData.Timer_Control] := $04;
    PORT[GUSData.Timer_Data] := GUSData.Timer_Mask;
    ASM
      POPF
    END;
  END;


PROCEDURE UltraStopTimer(Timer : INTEGER);
  BEGIN
    IF Timer=1 THEN
      BEGIN
        GUSData.Timer_Ctrl := GUSData.Timer_Ctrl AND (NOT $04);
        GUSData.Timer_Mask := GUSData.Timer_Mask AND (NOT $01);
      END
    ELSE
      BEGIN
        GUSData.Timer_Ctrl := GUSData.Timer_Ctrl AND (NOT $08);
        GUSData.Timer_Mask := GUSData.Timer_Mask AND (NOT $02);
      END;

    ASM
      PUSHF
      CLI
    END;
    PORT[GUSData.Reg_Select] := TIMER_CONTROL;
    PORT[GUSData.Data_Hi] := GUSData.Timer_Ctrl;

    PORT[GUSData.Timer_Control] := $04;
    PORT[GUSData.Timer_Data] := (GUSData.Timer_Mask OR $80);
    ASM
      POPF
    END;
  END;

FUNCTION UltraTimerStopped(Timer : INTEGER) : BOOLEAN;
  VAR
    Temp   : BYTE;
  BEGIN
    IF Timer = 1 THEN
      Temp := $40
    ELSE
      Temp := $20;

    UltraTimerStopped := ((PORT[GUSData.Timer_Control] AND Temp) <> 0);
  END;

{ ------------------------------------------------------------------------- }

PROCEDURE UltraClearVoices;
  BEGIN
    GUSData.Used_Voices := 0;
  END;

FUNCTION UltraAllocVoice(VAR Voice_Num : INTEGER) : BOOLEAN;
  VAR
    I : LONGINT;
  BEGIN
    UltraAllocVoice := FALSE;

    IF Voice_Num >= GUSData.Voices THEN
      BEGIN
        UltraOk := FALSE;
        UltraError := VOICE_NOT_VALID;
        UltraErrorStr := ErrorStrings[VOICE_NOT_VALID];
        EXIT;
      END;

    IF Voice_Num = -1 THEN
      BEGIN
        I := 0;
        WHILE (I < 32) AND
              (Voice_Num = -1) DO
          BEGIN
            IF (GUSData.Used_Voices AND LONGINT(1 SHL I)) = 0 THEN
              Voice_Num := I;
            INC(I);
          END;
        IF I=32 THEN
          BEGIN
            UltraOk := FALSE;
            UltraError := NO_FREE_VOICES;
            UltraErrorStr := ErrorStrings[NO_FREE_VOICES];
            EXIT;
          END;
      END;

    IF (GUSData.Used_Voices AND LONGINT(1 SHL Voice_Num)) = 0 THEN
      GUSData.Used_Voices := GUSData.Used_Voices OR LONGINT(1 SHL Voice_Num)
    ELSE
      BEGIN
        UltraOk := FALSE;
        UltraError := VOICE_ALREADY_USED;
        UltraErrorStr := ErrorStrings[VOICE_ALREADY_USED];
        EXIT;
      END;

    UltraAllocVoice := TRUE;
  END;

PROCEDURE UltraFreeVoice(Voice_Num : INTEGER);
  BEGIN
    GUSData.Used_Voices := GUSData.Used_Voices AND (NOT (LONGINT(1 SHL Voice_Num)));
  END;

FUNCTION  UltraVoicesMax : INTEGER;
  BEGIN
    UltraVoicesMax := GUSData.Voices;
  END;

{ ------------------------------------------------------------------------- }

BEGIN
  UltraOk := FALSE;
  UltraErrorStr := '';
  Setup_GUSData;
  IF UltraGetCfg(Ultra_Config) THEN
    Ultra_Installed := TRUE;
END.
