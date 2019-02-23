{
  Copyright (c) 1998 Erland Van Olmen (erlandvo@hotmail.com)
  Protected Mode Ready

  DPMI routines and records by M. G. Ricken found on the internet:

   ---->  http://www.superscalar.org/gptricks/gp_386a.html <----


ўўBugfixes/changes/additions since version 1.0 (first public release):

      - None


ўўBugfixes/changes/additions since version 1.1 (2nd public release):

      - Added M. G. Ricken's routines for proper DPMI support (thanks!!!)
}

{$I-,N+,E-}
Unit HardWare;

interface

Type
{$IFDEF DPMI}
  DPMIRegisters = record    { registers for DPMI int calls }
    case byte of
      1 : (EDI,ESI,EBP,EReserved,
           EBX,EDX,ECX,EAX:longint;
           Flags,ES,DS,FS,GS,
           IP,CS,SP,SS:word);
      2 : (DI,XDI,SI,XSI,BP,XBP,Reserved,
           XReserved,BX,XBX,DX,XDX,CX,XCX,
           AX,XAX:word);
      3 : (DIL,DIH,XDIL,XDIH,
           SIL,SIH,XSIL,XSIH,
           BPL,BPH,XBPL,XBPH,
           ReservedL,ReservedH,
           XReservedL,XReservedH,
           BL,BH,XBL,XBH,
           DL,DH,XDL,XDH,
           CL,CH,XCL,XCH,
           AL,AH,XAL,XAH,
           FlagsL,FlagsH,
           ESL,ESH,DSL,DSH,FSL,FSH,
           GSL,GSH,IPL,IPH,CSL,CSH,
           SPL,SPH,SSL,SSH:byte);
    end;

  LONGREC  = record
    lo,hi: word;     { LONGINT typecast }
  end;
{this is the original - we use the other one for the asm mixing routine
  _PPointer = record
    rm,pm: pointer;
  end;               }

  PPointer = record
    pm, rm: pointer;  { real/protected mode pointer }
  end;

  PtrRec = record
    Ofs, Seg: Word;
  end;
{$ENDIF}

  ByteBuffer  = Array[0..$FFF0-1] of Byte;
  PByteBuffer = ^ByteBuffer;

  TDMARec = Record
   EightBit           : Boolean;
   DMAChannel,
   ModeByte,
   ControlByte,
   ControlByteMask    : Byte;
   DMABuffer          : PByteBuffer;
   DMAAddrPort,
   DMACountPort,
   DMAPagePort,
   DMAMaskReg,
   DMAClearReg,
   DMAModeReg         : Byte;
   TransferLength,
   page               : Integer;
{   _go32_dpmi_seginfo SegInfo;}
   masksave           : Word;
   phys               : LongInt; { !!! should really be a DWORD !!! }
  end;

Const
{ Control Byte bit definitions }
  DemandMode         = 0;      { Mode Selection Bits 7:6               }
  SingleMode         = 64;
  BlockMode          = 128;
  CascadeMode        = 192;
  AddressDecrement   = 32;     { Address Increment/Decrement bit 5     }
  AddressIncrement   = 0;
  AutoInit           = 16;     { AutoInitialization enable bit 4       }
  SingleCycle        = 0;
  VerifyTransfer     = 0;      { Transfer Type bits 3:2                }
  WriteTransfer      = 4;
  ReadTransfer       = 8;
{ Channel Bits 1:0                      }

  BUFFSIZE           = 8192;
  HALFBUFFSIZE       = 4096;

{IRQ Bitfield constants (for the PIC = Interrupt controller): ---------------}
  Master_PIC = $20;       { Base Address of Master-PIC                       }
  Slave_PIC  = $A0;       { Base Address of slave-PIC                        }
  IRQ_Mask   = $01;       { Offset to Mask Port                              }

{IRQ program vectors from device-interrupts: --------------------------------}
  Master_FIRST_Vector = $08;
  Slave_FIRST_Vector  = $70;


{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}
{$IFDEF DPMI}
Function  Intr386(const int_number: byte; const stack_words: word;
                                              var regs: DPMIRegisters) : word;
function  AllocateDOS(var   p; const size: word): boolean;
procedure FreeDOS    (const p; const size: word);
procedure ClearDOS   (var   p; const size: word);
procedure CopyPPointer(const source; var dest);
function  GetMappedDPMIPtr (var   ProtPtr; const RealPtr:pointer;
                                                   const Size:word) : boolean;
function  FreeMappedDPMIPtr(const ProtPtr: pointer) : boolean;
Function  GetDMABuffer(var DMABuffer: PPointer; Size: WORD): Boolean;
{$ELSE}
Function  GetDMABuffer(var DMABuffer:  Pointer; Size: WORD): Boolean;
{$ENDIF}

Procedure irq_Enable  (iIRQ : Word);
Procedure irq_Disable (iIRQ : Word);
Procedure TimerSpeedup(Speed: Word);

Procedure DMAInit                (DMA: TDMARec);
Procedure DMADeInit              (DMA: TDMARec);
Procedure DMASetControlByteMask  (DMA: TDMARec; ModeSelect, AIncDec, AIBit, TransferB: Byte);

Procedure DMASetControlByte      (DMA: TDMARec);
Procedure DMASetChannel          (DMA: TDMARec; Channel: Byte);
Procedure DMASetPorts            (DMA: TDMARec);
Procedure DMAEnableChannel       (DMA: TDMARec);
Procedure DMADisableChannel      (DMA: TDMARec);
Procedure DMAClearFlipFlop       (DMA: TDMARec);
Procedure DMASetTransferLength   (DMA: TDMARec; Buflen: Integer);
{Procedure SetBufferInfo          (DMA: TDMARec);}
{Procedure DMAAllocateBuffer      (DMA: TDMARec);}
{Function  MK_FP(unsigned long,unsigned long): Pointer;}

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

implementation
{$IFDEF DPMI}
Uses
  WinApi;
{$ENDIF}

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}
{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

{$IFDEF DPMI}
{ Make Real Mode interrupt while in Protected Mode.                          }
Function Intr386(const int_number: byte; const stack_words: word;
                  var regs: DPMIRegisters) : word; assembler;
asm
  xor bx,bx           {                             clear bx }
  mov bl,int_number   {                load interrupt number }
  mov cx,stack_words  {                      get stack words }
  les di,regs         {       load effective address of regs }
  mov ax,0300h        {                 function number 300h }
  int 31h             {                       call interrupt }
  jc @pexit           {    if error jump to end of procedure }
  xor ax,ax           {                 clear ax; successful }
 @pexit:              {                     end of procedure }
end;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}
{  Allocate DOS memory in Protected Mode.                     }
function  AllocateDOS(var p; const size:word) : boolean;
var   l : longint;               { dummy for segment/selector }
begin
   l:=GlobalDosAlloc(size); AllocateDOS:=l<>0;
                                            { allocate memory }
   PPOINTER(p).rm:=Ptr(LONGREC(l).hi,0);
                                   { create real mode pointer }
   PPOINTER(p).pm:=Ptr(LONGREC(l).lo,0);
                              { create protected mode pointer }
end;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}
{  Free DOS memory in Protected Mode                          }
procedure FreeDOS(const p; const size:word);
begin
   GlobalDosFree(PTRREC(PPOINTER(p).pm).seg);   { free memory }
end;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}
{  Clear DOS memory.                                          }
procedure ClearDOS(var p; const size:word);
begin
   FillChar(PPOINTER(p).pm^,size,0);
                             { fill at protected mode pointer }
end;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

procedure CopyPPointer(const source; var dest);
{ Copy pointers. THIS DOES NOT COPY THE DATA SOURCE POINTS
  TO TO DEST!!! This merely does dest:=source, as if source
  and dest were pointers.                                     }
begin
   PPOINTER(dest).rm:=PPOINTER(source).rm;
   PPOINTER(dest).pm:=PPOINTER(source).pm;
end;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

function GetMappedDPMIPtr(var ProtPtr;
                          const RealPtr:pointer;
                          const Size:word) : boolean;assembler;
{ Get a Protected Mode pointer to a Real Mode address.      }
asm
   xor ax,ax        { Get an LDT descriptor & selector for it }
   mov cx,1
   int 31h
   jc @@Error
   xchg ax,bx
   xor ax,ax                 { Set descriptor to real address }
   mov dx,RealPtr.Word[2]
   mov al,dh
   mov cl,4
   shr ax,cl
   shl dx,cl
   xchg ax,cx
   mov ax,7
   int 31h
   jc @@Error
   mov ax,8              { Set descriptor to limit Size bytes }
   xor cx,cx
   mov dx,Size
   add dx,RealPtr.Word[0]
   jnc @@1
   xor dx,dx
   dec dx
@@1:
   int 31h
   jc @@Error
   cld                      { Save selector:offset in ProtPtr }
   les di,ProtPtr
   mov ax,RealPtr.Word[0]
   stosw
   xchg ax,bx
   stosw
   mov ax,1
   jmp @@Exit
@@Error:
   xor ax,ax
@@Exit:
end;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

function FreeMappedDPMIPtr(const ProtPtr:pointer) : boolean;
         assembler;
{ Free selector given by GetMappedDPMIPtr.                  }
asm
   mov ax,1
   mov bx,ProtPtr.Word[2]
   int 31h
   mov ax,0
   jc @@Error
   inc ax
@@Error:
end;
{$ENDIF}

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}
{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}
{Returns true if everything went OK. }
{$IFDEF DPMI}
Function  GetDMABuffer(var DMABuffer: PPointer; Size: WORD): Boolean;
{$ELSE}
Function  GetDMABuffer(var DMABuffer:  Pointer; Size: WORD): Boolean;
{$ENDIF}
VAR
  Page          : Byte;
  PageOffset,
  MemHoleSize   : Word;
  LinAddr       : LongInt;
  MemHole       : Pointer;
{$IFDEF DPMI}
  rseg, rofs    : LongInt;
{$ENDIF}
BEGIN
{$IFDEF DPMI}
  If Not AllocateDos(DMABuffer, Size*2) then
                                         Begin GetDMABuffer:=false; exit; end;
  LinAddr   :=LongInt(LongRec(DMABuffer.rm).hi) Shl 4;
  Page      :=LongRec(LinAddr).Hi and $F;
  PageOffset:=LongRec(LinAddr).Lo;
{  println('');
  println('LinAddr    : '+I2HStr(LinAddr    , 8));
  println('Page       : '+I2HStr(Page       , 4));
  println('Page Offset: '+I2HStr(PageOffset , 8));
  readkey;}
  If ($FFFF-PageOffset) < Size Then
    Begin Inc(Page); PageOffset:=0; LinAddr:=LongInt(Page) shl 16;
{      println('Page incremented!'); readkey; }
    End;
  rseg        :=LinAddr shr 4;
  rofs        :=LongRec(LinAddr).Lo and $F;
  DMABuffer.rm:=pointer(rseg shl 16 + rofs);
  If Not GetMappedDPMIPtr(DMABuffer.pm, DMABuffer.rm, Size) then
                                         Begin GetDMABuffer:=false; exit; end;
{$ELSE}
  GetDMABuffer:=False; MemHole:=Nil; MemHoleSize:=0;
  If MaxAvail<Size then exit; { check free mem size }
  LinAddr    :=LongInt(Seg(HeapPtr^)) shl 4 + Ofs(HeapPtr^);
  Page       :=LinAddr shr 16;
  PageOffset :=LinAddr AND $FFFF;
  If ($FFFF-PageOffset)<Size Then
    Begin
      MemHoleSize:=$FFFF-PageOffset;
      GetMem(MemHole, MemHoleSize);
      If MaxAvail<Size then
        Begin FreeMem(MemHole, MemHoleSize); GetDMABuffer:=false; exit; End;
    End;
  GetMem(DMABuffer, Size);
  If MemHoleSize>0 then FreeMem(MemHole, MemHoleSize);
{$ENDIF}
  GetDMABuffer:=True;
END;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}
{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

Procedure irq_Enable(iIRQ: Word);
VAR
  iPort: Word;
BEGIN
  if iIRQ <= 7 then iPort:=MASTER_PIC Else iPort:=SLAVE_PIC;
  Inc(iPort, IRQ_MASK);             { Choose Mask port                       }
  iIRQ:=iIRQ AND $0007;             { define PIC-interrupt number (0-7)      }
  Port[iPort]:=(Port[iPort] AND ((Word(1) SHL iIRQ) XOR $FFFF));
END;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

Procedure irq_Disable(iIRQ: Word);
VAR
  iPort: Word;
BEGIN
  if iIRQ <= 7 then iPort:=MASTER_PIC Else iPort:=SLAVE_PIC;
  Inc(iPort, IRQ_MASK);             { Choose Mask port                       }
  iIRQ:=iIRQ AND $0007;             { define PIC-interrupt number (0-7)      }
  Port[iPort]:=Port[iPort] OR (Word(1) SHL iIRQ);
END;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

Procedure TimerSpeedup(Speed: Word);
Begin
  Port[$43]:=$36;
  Port[$40]:=Speed AND $FF;
  Port[$40]:=Speed SHR 8;
{
  mov    al, 036h
  out    043h, al
  mov    ax, [Speed]
  out    040h, al
  mov    al, ah
  out    040h, al
}
End;

{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}
{лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл}

Procedure DMAInit(DMA: TDMARec);
Begin
  With DMA do
  Begin
    DMAChannel          :=100;
    ModeByte            :=0;
    ControlByte         :=0;
    ControlByteMask     :=0;
    DMAAddrPort         :=0;
    DMACountPort        :=0;
    DMAPagePort         :=0;
    DMAMaskReg          :=0;
    DMAClearReg         :=0;
    DMAModeReg          :=0;
    EightBit            :=True;
  End;
{  AllocateDMABuffer();}
End;

Procedure DMADeInit(DMA: TDMARec);
Begin
 { _go32_dpmi_free_dos_memory(&SegInfo); }
End;

{use GetDMABuffer instead? }
{
Procedure  DMA::AllocateDMABuffer()
Begin
  SegInfo.size=((BUFFSIZE*2)+15)/16;
 _go32_dpmi_allocate_dos_memory(&SegInfo);
 phys=SegInfo.rm_segment<<4;

 if((phys>>16)!=((phys+BUFFSIZE)>>16))
 Begin phys+=BUFFSIZE;
  cout<<"Hit Page Division!\n";  //doing page checking right here
 End;
 page=(long)(phys>>16);
 memset((unsigned char *)MK_FP(phys>>4,0),0,BUFFSIZE);
End;
}

Procedure DMASetChannel(DMA: TDMARec; Channel: Byte);
Begin
  If Channel > 7 then exit
  Else DMA.DMAChannel:=Channel;
  DMASetPorts(DMA);
End;

Procedure DMASetPorts(DMA: TDMARec);
Begin
  With DMA do
  Begin
    Case DMAChannel of
      0: begin DMAAddrPort:=$00; DMACountPort:=$01; DMAPagePort:=$87; end;
      1: begin DMAAddrPort:=$02; DMACountPort:=$03; DMAPagePort:=$83; end;
      2: begin DMAAddrPort:=$04; DMACountPort:=$05; DMAPagePort:=$81; end;
      3: begin DMAAddrPort:=$06; DMACountPort:=$07; DMAPagePort:=$82; end;
     { 16 bit channels }
      4: begin DMAAddrPort:=$C0; DMACountPort:=$C2; DMAPagePort:=$8F; end;
      5: begin DMAAddrPort:=$C4; DMACountPort:=$C6; DMAPagePort:=$8B; end;
      6: begin DMAAddrPort:=$C8; DMACountPort:=$CA; DMAPagePort:=$89; end;
      7: begin DMAAddrPort:=$CC; DMACountPort:=$CE; DMAPagePort:=$8A; end;
     else; Begin writeln('Invalid DMA Channel!'); exit; End;
    end;

    If DMAChannel < 4 then
           Begin DMAMaskReg:=$0A; DMAClearReg:=$0C; DMAModeReg:=$0B; End
    Else   Begin { 16 bit channels }
             DMAMaskReg:=$D4; DMAClearReg:=$D8; DMAModeReg:=$D6;
             Dec(DMAChannel, 4); EightBit:=False;
           End;
  End;
End;


Procedure DMASetControlByteMask(DMA: TDMARec; ModeSelect, AIncDec, AIBit, TransferB: Byte);
Begin
  With DMA do
  Begin
    ControlByteMask:=ModeSelect+AIncDec+AIBit+TransferB;
    Inc(ControlByteMask, DMAChannel);
  End;
End;


Procedure DMASetControlByte(DMA: TDMARec);
Begin
  With DMA do
  Begin
    ControlByte:=ControlByte or ControlByteMask;
    Port[DMAModeReg]:=ControlByte;
  End;
End;


Procedure DMAEnableChannel(DMA: TDMARec);
Var
  Mask: Byte;
Begin
  With DMA do
  Begin
    Mask:=DMAChannel;
    Port[DMAMaskReg]:=Mask;
  End;
End;


Procedure DMADisableChannel(DMA: TDMARec);
Var
  Mask: Byte;
Begin
  With DMA do
  Begin
    Mask:=DMAChannel or $4;
    Port[DMAMaskReg]:=Mask;
  End;
End;


Procedure DMAClearFlipFlop(DMA: TDMARec);
Begin
  Port[DMA.DMAClearReg]:=0;
End;


Procedure DMASetTransferLength(DMA: TDMARec; Buflen: Integer);
Begin
  With DMA do
  Begin
    TransferLength:=BufLen;
    Port[DMACountPort]:=Lo(Buflen-1); { Low  byte of buffersize }
    Port[DMACountPort]:=Hi(Buflen-1); { High byte of buffersize }
  End;
End;


Procedure DMASetBufferInfo(DMA: TDMARec);
Var
  offs: LongInt;
Begin
  With DMA do
  Begin
    offs:=phys and $FFFF;
    If Not EightBit then offs:=(phys shr 1) and $FFFF;
    Port[DMAPagePort]:=page;
    Port[DMAAddrPort]:=offs and $FF;
    Port[DMAAddrPort]:=offs shr 8;
  End;
End;

{
void * DMA::MK_FP(unsigned long seg, unsigned long ofs)
begin
  if(!(_crt0_startup_flags & _CRT0_FLAG_NEARPTR))
   if(!__djgpp_nearptr_enable())
     return (void*)0;
   return (void *)(seg*16+ofs+__djgpp_conventional_base);
end;
}

Procedure SetupDMA(DMA: TDMARec; Channel: Byte);
Begin
  With DMA do
  Begin
    asm cli end;
    DMASetChannel(DMA, Channel);
    DMADisableChannel(DMA);       { Disable DMA channel while programming it }
    DMASetControlByteMask(DMA, DemandMode, AddressIncrement, SingleCycle, WriteTransfer);
{ Put into 8-bit Single Cycle mode }
    DMASetControlByte(DMA);
    DMAClearFlipFlop(DMA);
    DMASetBufferInfo(DMA);
    DMASetTransferLength(DMA, BUFFSIZE);
    asm sti end;
    DMAEnableChannel(DMA);
  End;
End;


END.