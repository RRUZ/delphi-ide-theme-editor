unit KOLDetours;
//    purpose: Interception of methods, procedures and functions
//     Author: KOL version Thaddy de Koning
//             Original author is Erik van Bilsen.
//             Published first in Delphi Magazine 101.

interface

{$IFDEF CPUX64}
  Sorry, this unit only can be used  in 32 bits mode
{$ENDIF}

uses Windows, Sysutils;

function InterceptCreate(const TargetProc, InterceptProc: Pointer): Pointer;
{ Redirects the procedure TargetProc to InterceptProc. Returns the address of a
  trampoline function, which can be used to call the original routine from
  your Intercept. Also, the trampoline function must be passed to the
  InterceptRemove function to restore the original routine again. The Intercept
  and trampoline routines must have the exact same signature as TargetProc for
  the Intercept to work. Example:

    var
      TrampolineMessageBox: function(hWnd: HWND; lpText, lpCaption: PChar;
        uType: UINT): Integer; stdcall;

    function InterceptMessageBox(hWnd: HWND; lpText, lpCaption: PChar;
      uType: UINT): Integer; stdcall;
    var
      S: String;
    begin
      S := UpperCase(lpText);
      Result := TrampolineMessageBox(hWnd,PChar(S),lpCaption,uType);
    end;

    @TrampolineMessageBox := InterceptCreate(@MessageBox,@InterceptMessageBox);
    MessageBox(0,'Intercept test','Test',MB_OK);
    // Will display the message 'Intercept test' in upper case
    InterceptRemove(@TrampolineMessageBox,@InterceptMessageBox);
    MessageBox(0,'Intercept test','Test',MB_OK);
    // Will display the message 'Intercept test' in original case }

function InterceptRemove(var Trampoline: Pointer; const InterceptProc: Pointer): Boolean;
{ Restores the Intercept. You must pass the trampoline as returned by the
  InterceptCreate function and the Intercept routine. This trampoline will be freed
  and set to nil }


{$IFDEF VER120}
type
  PPointer = ^Pointer;
{$ENDIF}

const
  SizeOfJmp = 5;
  { Size of an assembly JMP instruction in bytes (1 byte for the OpCode and
    4 bytes for the address displacement) }
  TrampolineSize = 32;
  { The amount of bytes allocated for a trampoline function. A trampoline
    should be large enough to accomodate at least 5 bytes from the original
    routine, and 5 bytes for an unconditional JMP instruction. A single
    assembly instruction can take up 17 bytes: up to 4 bytes for an instruction
    prefix + up to 3 bytes for the instruction opcode + 1 optional ModR/M byte
    + 1 optional SIB-byte + up to 4 address displacement bytes + up to 4
    immediate data bytes (see Intel's IA-32 instruction reference for details).
    Since at least 5 bytes from the original routine have to be copied to the
    trampoline, and the fifth byte could start a new 17-byte instruction, this
    means a maximum of 4+17=21 bytes could be needed. Including the 5 bytes for
    an unconditional JMP, we should allocate 26 bytes minimum. We use 32 bytes
    here to be on the safe side. }

const // Machine code values for certain assembly opcodes
  opJmpIndirect = $25FF; // jmp dword ptr[<Address>]  jmp dword ptr [$12345678]
  opJmpRelative = $E9;   // jmp <Displacement>        jmp -$00001234
  opJmpEax      = $E0FF; // jmp eax
  opRetPop      = $C2;   // ret <Value>               ret $1234
  opRet         = $C3;   // ret
  opPreES       = $26;   // ES-prefix (e.g. jmp dword ptr ES:[$12345678])
  opPreCS       = $2E;   // CS-prefix
  opPreSS       = $36;   // SS-prefix
  opPreDS       = $3E;   // DS-prefix
  opPreFS       = $64;   // FS-prefix
  opPreGS       = $65;   // GS-prefix
  opInt3        = $CC;   // int 3 (debug breakpoint)
  //
  opMovECX      = $0D8B; // mov ECX (mov ECX, EAX)

type
  PIntercepts=^TIntercepts;
  TIntercepts = class(Tobject)
  { Helper class for creating Intercepts }
  private
    FUse16BitAddress: Boolean;
    FUse16BitOperand: Boolean;
    FScratch: array [0..63] of Byte;
    { Scratch area for copying assembly instructions }
    FProcess: THandle;
    { Handle to the current process }
  protected
    function InsertIntercept(const TargetProc, Trampoline, InterceptProc: Pointer): Boolean;
    { Is called by CreateIntercept. The first few instructions of TargetProc are
      copied to the Trampoline. TargetProc is adjusted by inserting an
      unconditional JMP instruction to the InterceptProc. The Trampoline is
      extended with an unconditional JMP instruction to the remainder of
      TargetProc }
    function CopyInstruction(Src, Dst: Pointer): Pointer;
    { Copies the assembly instruction at Src to Dst. Returns the address of the
      next instruction in Src. Dst can be nil. In that case the instruction is
      not actually copied and only the address of the next instruction is
      returned. This can be helpful to calculate the number of bytes an
      instruction takes. }
    procedure AdjustTarget(const Src, Dst: Pointer; const OpCodeSize,
      TargetOffset: Integer);
    { Adjusts the relative target of the instruction at Dst. Src points to the
      source instruction and Dst to the already copied destination instruction.
      OpCodeSize must be the fixed size of the OpCode and TargetOffset the
      offset from the OpCode where the relative target is located.
      Example:
        Src: $00005000 jmp -$1000
      At address $00005000 is an instruction that jumps $1000 bytes backwards,
      that is to address $00004000. When this instruction is copied to Dst
      at address $00007000:
        Dst: $00007000 jmp -$1000
      than it jumps to address $00006000, which is incorrect. The instruction is
      adjusted so it jumps to the right address:
        Dst: $00007000 jmp -$3000 }
    function SetPermission(const Code: Pointer; const Size: Integer;
      const Permission: Longword): Longword;
    { Change the access protection of a piece of assembly code. Code should
      point to the code and be Size bytes in size. Permission should be a
      constant representing the requested permission (see the Windows API help
      on VirtualProtect). Returns the old permission value }
    function InsertJump(Code, Target: Pointer; Size: Integer): Boolean;
    { Adds an unconditional jump at address Code to jump to Target. If Size is
      larger than the space needed for the jump (5 bytes), then the remainder
      of the Code is filled with "int 3" instructions. Returns False if the
      jump could not be inserted (size < 5) }

    property Use16BitAddress: Boolean read FUse16BitAddress write FUse16BitAddress;
    { Indicates if the current instruction uses a 16-bit address (if False, a
      32-bit address is used) }
    property Use16BitOperand: Boolean read FUse16BitOperand write FUse16BitOperand;
    { Indicates if the current instruction uses a 16-bit operand (if False, a
      32-bit operand is used}
  public
    function CreateIntercept(TargetProc, InterceptProc: Pointer): Pointer;
    { Redirects TargetProc to InterceptProc and returns a trampoline routine }
    function RemoveIntercept(Trampoline, InterceptProc: Pointer): Boolean;
    { Restores a Intercept }

    class function GetFinalCode(const Proc: Pointer; const SkipJumps: Boolean = False): Pointer;
    { Examines the routine Proc. Returns the address where the actual code of
      Proc is located. }
  end;
  
function NewIntercepts:TIntercepts;

type
  POpCodeEntry = ^TOpCodeEntry;

  TCopyInstructionProc = function (const Intercepts: PIntercepts;
    Entry: POpCodeEntry; Src, Dst: Pointer): Pointer;
  { Procedural type used to copy assembly instructions }

  TOpCodeEntry = packed record
  { Contains information about an assembly OpCode }
    Size32: Byte; // Fixed size of OpCode with 32-bit operand
    Size16: Byte; // Fixed size of OpCode with 16-bit operand
    ModOfs: Byte; // Offset to ModR/M byte
    RelOfs: Byte; // Offset to relative target
    UseAdr: Boolean; // Use address-size attribute instead of operand-size attribute
    Copy  : TCopyInstructionProc; // Function used to copy instruction
  end;



{ Forward declarations of TCopyInstructionProc functions used in the
  OneByteOpCodes and TwoByteOpCodes arrays }

function CopyNormal(const Intercepts: PIntercepts; Entry: POpCodeEntry;
  Src, Dst: Pointer): Pointer; forward;
{ Default function for copying instructions }

function Copy2ByteOpCode(const Intercepts: PIntercepts; Entry: POpCodeEntry;
  Src, Dst: Pointer): Pointer; forward;
{ Function for copying 2 byte long OpCodes. The first OpCode byte is $0F }

function CopyWithPrefix(const Intercepts: PIntercepts; Entry: POpCodeEntry;
  Src, Dst: Pointer): Pointer; forward;
{ Function for copying instructions that contain prefix bytes }

function CopyWith16BitOperand(const Intercepts: PIntercepts; Entry: POpCodeEntry;
  Src, Dst: Pointer): Pointer; forward;
{ Function for copying instructions that contain a prefix instructing to use
  16-bit operands }

function CopyWith16BitAddress(const Intercepts: PIntercepts; Entry: POpCodeEntry;
  Src, Dst: Pointer): Pointer; forward;
{ Function for copying instructions that contain a prefix instructing to use
  16-bit addresses }

function CopyOpF6(const Intercepts: PIntercepts; Entry: POpCodeEntry;
  Src, Dst: Pointer): Pointer; forward;
{ Function for copying instructions with OpCode $F6 (test, div, idiv, mul,
  imul, not and neg using a 8-bit operand) }

function CopyOpF7(const Intercepts: PIntercepts; Entry: POpCodeEntry;
  Src, Dst: Pointer): Pointer; forward;
{ Function for copying instructions with OpCode $F7 (test, div, idiv, mul,
  imul, not and neg using a 16/32-bit operand) }

function CopyInvalid(const Intercepts: PIntercepts; Entry: POpCodeEntry;
  Src, Dst: Pointer): Pointer; forward;
{ Called when trying to copy an invalid OpCode. Just skips the OpCode. }

const
  OneByteOpCodes: array [0..255] of TOpCodeEntry = (
    // Information about OpCodes of 1 byte
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // ADD /r
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // ADD /r
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // ADD /r
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // ADD /r
    (Size32: 2; Size16: 2; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // ADD ib
    (Size32: 5; Size16: 3; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // ADD iw
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // PUSH
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // POP
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // OR /r
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // OR /r
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // OR /r
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // OR /r
    (Size32: 2; Size16: 2; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // OR ib
    (Size32: 5; Size16: 3; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // OR iw
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // PUSH
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: Copy2ByteOpCode     ), // Extension Ops
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // ADC /r
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // ADC /r
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // ADC /r
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // ADC /r
    (Size32: 2; Size16: 2; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // ADC ib
    (Size32: 5; Size16: 3; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // ADC id
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // PUSH
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // POP
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // SBB /r
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // SBB /r
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // SBB /r
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // SBB /r
    (Size32: 2; Size16: 2; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // SBB ib
    (Size32: 5; Size16: 3; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // SBB id
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // PUSH
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // POP
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // AND /r
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // AND /r
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // AND /r
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // AND /r
    (Size32: 2; Size16: 2; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // AND ib
    (Size32: 5; Size16: 3; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // AND id
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyWithPrefix      ), // ES prefix
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // DAA
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // SUB /r
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // SUB /r
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // SUB /r
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // SUB /r
    (Size32: 2; Size16: 2; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // SUB ib
    (Size32: 5; Size16: 3; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // SUB id
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyWithPrefix      ), // CS prefix
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // DAS
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // XOR /r
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // XOR /r
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // XOR /r
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // XOR /r
    (Size32: 2; Size16: 2; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // XOR ib
    (Size32: 5; Size16: 3; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // XOR id
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyWithPrefix      ), // SS prefix
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // AAA
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // CMP /r
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // CMP /r
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // CMP /r
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // CMP /r
    (Size32: 2; Size16: 2; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // CMP ib
    (Size32: 5; Size16: 3; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // CMP id
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyWithPrefix      ), // DS prefix
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // AAS
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // INC
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // INC
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // INC
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // INC
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // INC
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // INC
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // INC
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // INC
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // DEC
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // DEC
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // DEC
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // DEC
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // DEC
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // DEC
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // DEC
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // DEC
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // PUSH
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // PUSH
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // PUSH
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // PUSH
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // PUSH
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // PUSH
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // PUSH
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // PUSH
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // POP
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // POP
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // POP
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // POP
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // POP
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // POP
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // POP
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // POP
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // PUSHAD
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // POPAD
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // BOUND /r
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // ARPL /r
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyWithPrefix      ), // FS prefix
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyWithPrefix      ), // GS prefix
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyWith16BitOperand), // Operand Prefix
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyWith16BitAddress), // Address Prefix
    (Size32: 5; Size16: 3; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // PUSH
    (Size32: 6; Size16: 4; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), //
    (Size32: 2; Size16: 2; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // PUSH
    (Size32: 3; Size16: 3; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // IMUL /r ib
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // INS
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // INS
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // OUTS/OUTSB
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // OUTS/OUTSW
    (Size32: 2; Size16: 2; ModOfs: 0; RelOfs: 1; UseAdr: False; Copy: CopyNormal          ), // JO
    (Size32: 2; Size16: 2; ModOfs: 0; RelOfs: 1; UseAdr: False; Copy: CopyNormal          ), // JNO
    (Size32: 2; Size16: 2; ModOfs: 0; RelOfs: 1; UseAdr: False; Copy: CopyNormal          ), // JB/JC/JNAE
    (Size32: 2; Size16: 2; ModOfs: 0; RelOfs: 1; UseAdr: False; Copy: CopyNormal          ), // JAE/JNB/JNC
    (Size32: 2; Size16: 2; ModOfs: 0; RelOfs: 1; UseAdr: False; Copy: CopyNormal          ), // JE/JZ
    (Size32: 2; Size16: 2; ModOfs: 0; RelOfs: 1; UseAdr: False; Copy: CopyNormal          ), // JNE/JNZ
    (Size32: 2; Size16: 2; ModOfs: 0; RelOfs: 1; UseAdr: False; Copy: CopyNormal          ), // JBE/JNA
    (Size32: 2; Size16: 2; ModOfs: 0; RelOfs: 1; UseAdr: False; Copy: CopyNormal          ), // JA/JNBE
    (Size32: 2; Size16: 2; ModOfs: 0; RelOfs: 1; UseAdr: False; Copy: CopyNormal          ), // JS
    (Size32: 2; Size16: 2; ModOfs: 0; RelOfs: 1; UseAdr: False; Copy: CopyNormal          ), // JNS
    (Size32: 2; Size16: 2; ModOfs: 0; RelOfs: 1; UseAdr: False; Copy: CopyNormal          ), // JP/JPE
    (Size32: 2; Size16: 2; ModOfs: 0; RelOfs: 1; UseAdr: False; Copy: CopyNormal          ), // JNP/JPO
    (Size32: 2; Size16: 2; ModOfs: 0; RelOfs: 1; UseAdr: False; Copy: CopyNormal          ), // JL/JNGE
    (Size32: 2; Size16: 2; ModOfs: 0; RelOfs: 1; UseAdr: False; Copy: CopyNormal          ), // JGE/JNL
    (Size32: 2; Size16: 2; ModOfs: 0; RelOfs: 1; UseAdr: False; Copy: CopyNormal          ), // JLE/JNG
    (Size32: 2; Size16: 2; ModOfs: 0; RelOfs: 1; UseAdr: False; Copy: CopyNormal          ), // JG/JNLE
    (Size32: 3; Size16: 3; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // ADC/2 ib, etc.s
    (Size32: 6; Size16: 4; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), //
    (Size32: 2; Size16: 2; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // MOV al,x
    (Size32: 3; Size16: 3; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // ADC/2 ib, etc.
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // TEST /r
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // TEST /r
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // XCHG /r @todo
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // XCHG /r @todo
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // MOV /r
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // MOV /r
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // MOV /r
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // MOV /r
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // MOV /r
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // LEA /r
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // MOV /r
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // POP /0
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // NOP
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // XCHG
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // XCHG
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // XCHG
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // XCHG
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // XCHG
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // XCHG
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // XCHG
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // CWDE
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // CDQ
    (Size32: 7; Size16: 5; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // CALL cp
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // WAIT/FWAIT
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // PUSHFD
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // POPFD
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // SAHF
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // LAHF
    (Size32: 5; Size16: 3; ModOfs: 0; RelOfs: 0; UseAdr: True ; Copy: CopyNormal          ), // MOV
    (Size32: 5; Size16: 3; ModOfs: 0; RelOfs: 0; UseAdr: True ; Copy: CopyNormal          ), // MOV
    (Size32: 5; Size16: 3; ModOfs: 0; RelOfs: 0; UseAdr: True ; Copy: CopyNormal          ), // MOV
    (Size32: 5; Size16: 3; ModOfs: 0; RelOfs: 0; UseAdr: True ; Copy: CopyNormal          ), // MOV
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // MOVS
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // MOVS/MOVSD
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // CMPS/CMPSB
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // CMPS/CMPSW
    (Size32: 2; Size16: 2; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // TEST
    (Size32: 5; Size16: 3; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // TEST
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // STOS/STOSB
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // STOS/STOSW
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // LODS/LODSB
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // LODS/LODSW
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // SCAS/SCASB
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // SCAS/SCASD
    (Size32: 2; Size16: 2; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // MOV B0+rb
    (Size32: 2; Size16: 2; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // MOV B0+rb
    (Size32: 2; Size16: 2; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // MOV B0+rb
    (Size32: 2; Size16: 2; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // MOV B0+rb
    (Size32: 2; Size16: 2; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // MOV B0+rb
    (Size32: 2; Size16: 2; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // MOV B0+rb
    (Size32: 2; Size16: 2; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // MOV B0+rb
    (Size32: 2; Size16: 2; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // MOV B0+rb
    (Size32: 5; Size16: 3; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // MOV B8+rb
    (Size32: 5; Size16: 3; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // MOV B8+rb
    (Size32: 5; Size16: 3; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // MOV B8+rb
    (Size32: 5; Size16: 3; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // MOV B8+rb
    (Size32: 5; Size16: 3; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // MOV B8+rb
    (Size32: 5; Size16: 3; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // MOV B8+rb
    (Size32: 5; Size16: 3; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // MOV B8+rb
    (Size32: 5; Size16: 3; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // MOV B8+rb
    (Size32: 3; Size16: 3; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // RCL/2 ib, etc.
    (Size32: 3; Size16: 3; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // RCL/2 ib, etc.
    (Size32: 3; Size16: 3; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // RET
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // RET
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // LES
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // LDS
    (Size32: 3; Size16: 3; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // MOV
    (Size32: 6; Size16: 4; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // MOV
    (Size32: 4; Size16: 4; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // ENTER
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // LEAVE
    (Size32: 3; Size16: 3; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // RET
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // RET
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // INT 3
    (Size32: 2; Size16: 2; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // INT ib
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // INTO
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // IRET
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // RCL/2, etc.
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // RCL/2, etc.
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // RCL/2, etc.
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // RCL/2, etc.
    (Size32: 2; Size16: 2; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // AAM
    (Size32: 2; Size16: 2; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // AAD
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), //
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // XLAT/XLATB
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // FADD, etc.
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // F2XM1, etc.
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // FLADD, etc.
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // FCLEX, etc.
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // FADD/0, etc.
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // FFREE, etc.
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // FADDP, etc.
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // FBLD/4, etc.
    (Size32: 2; Size16: 2; ModOfs: 0; RelOfs: 1; UseAdr: False; Copy: CopyNormal          ), // LOOPNE cb
    (Size32: 2; Size16: 2; ModOfs: 0; RelOfs: 1; UseAdr: False; Copy: CopyNormal          ), // LOOPE cb
    (Size32: 2; Size16: 2; ModOfs: 0; RelOfs: 1; UseAdr: False; Copy: CopyNormal          ), // LOOP cb
    (Size32: 2; Size16: 2; ModOfs: 0; RelOfs: 1; UseAdr: False; Copy: CopyNormal          ), // JCXZ/JECXZ
    (Size32: 2; Size16: 2; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // IN ib
    (Size32: 2; Size16: 2; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // IN id
    (Size32: 2; Size16: 2; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // OUT ib
    (Size32: 2; Size16: 2; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // OUT ib
    (Size32: 5; Size16: 3; ModOfs: 0; RelOfs: 1; UseAdr: False; Copy: CopyNormal          ), // CALL cd
    (Size32: 5; Size16: 3; ModOfs: 0; RelOfs: 1; UseAdr: False; Copy: CopyNormal          ), // JMP cd
    (Size32: 7; Size16: 5; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // JMP cp
    (Size32: 2; Size16: 2; ModOfs: 0; RelOfs: 1; UseAdr: False; Copy: CopyNormal          ), // JMP cb
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // IN ib
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // IN id
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // OUT
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // OUT
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyWithPrefix      ), // LOCK prefix
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), //
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyWithPrefix      ), // REPNE prefix
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyWithPrefix      ), // REPE prefix
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // HLT
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // CMC
    (Size32: 0; Size16: 0; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyOpF6            ), // TEST/0, DIV/6
    (Size32: 0; Size16: 0; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyOpF7            ), // TEST/0, DIV/6
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // CLC
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // STC
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // CLI
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // STI
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // CLD
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // STD
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // DEC/1,INC/0
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ));// CALL/2

const
  TwoByteOpCodes: array [0..255] of TOpCodeEntry = (
    // Information about OpCodes of 2 bytes
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // LLDT/2, etc.
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // INVLPG/7, etc.
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // LAR/r
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // LSL/r
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _04
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _05
    (Size32: 2; Size16: 2; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // CLTS
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _07
    (Size32: 2; Size16: 2; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // INVD
    (Size32: 2; Size16: 2; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // WBINVD
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _0A
    (Size32: 2; Size16: 2; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // UD2
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _0C
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _0D
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _0E
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _0F
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _10
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _11
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _12
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _13
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _14
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _15
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _16
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _17
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _18
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _19
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _1A
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _1B
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _1C
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _1D
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _1E
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _1F
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // MOV/r
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // MOV/r
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // MOV/r
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // MOV/r
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _24
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _25
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _26
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _27
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _28
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _29
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _2A
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _2B
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _2C
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _2D
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _2E
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _2F
    (Size32: 2; Size16: 2; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // WRMSR
    (Size32: 2; Size16: 2; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // RDTSC
    (Size32: 2; Size16: 2; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // RDMSR
    (Size32: 2; Size16: 2; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // RDPMC
    (Size32: 2; Size16: 2; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // SYSENTER
    (Size32: 2; Size16: 2; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // SYSEXIT
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _36
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _37
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _38
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _39
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _3A
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _3B
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _3C
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _3D
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _3E
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _3F
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // CMOVO (0F 40)
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // CMOVNO (0F 41)
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // CMOVB & CMOVNE (0F 42)
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // CMOVAE & CMOVNB (0F 43)
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // CMOVE & CMOVZ (0F 44)
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // CMOVNE & CMOVNZ (0F 45)
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // CMOVBE & CMOVNA (0F 46)
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // CMOVA & CMOVNBE (0F 47)
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // CMOVS (0F 48)
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // CMOVNS (0F 49)
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // CMOVP & CMOVPE (0F 4A)
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // CMOVNP & CMOVPO (0F 4B)
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // CMOVL & CMOVNGE (0F 4C)
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // CMOVGE & CMOVNL (0F 4D)
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // CMOVLE & CMOVNG (0F 4E)
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // CMOVG & CMOVNLE (0F 4F)
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _50
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _51
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _52
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _53
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _54
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _55
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _56
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _57
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _58
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _59
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _5A
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _5B
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _5C
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _5D
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _5E
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _5F
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // PUNPCKLBW/r
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _61
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // PUNPCKLWD/r
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // PACKSSWB/r
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // PCMPGTB/r
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // PCMPGTW/r
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // PCMPGTD/r
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // PACKUSWB/r
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // PUNPCKHBW/r
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // PUNPCKHWD/r
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // PUNPCKHDQ/r
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // PACKSSDW/r
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _6C
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _6D
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // MOVD/r
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // MOV/r
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _70
    (Size32: 3; Size16: 3; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // PSLLW/6 ib,PSRAW/4 ib,PSRLW/2 ib
    (Size32: 3; Size16: 3; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // PSLLD/6 ib,PSRAD/4 ib,PSRLD/2 ib
    (Size32: 3; Size16: 3; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // PSLLQ/6 ib,PSRLQ/2 ib
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // PCMPEQB/r
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // PCMPEQW/r
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // PCMPEQD/r
    (Size32: 2; Size16: 2; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // EMMS
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _78
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _79
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _7A
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _7B
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _7C
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _7D
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // MOVD/r
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // MOV/r
    (Size32: 5; Size16: 3; ModOfs: 0; RelOfs: 1; UseAdr: False; Copy: CopyNormal          ), // JO
    (Size32: 5; Size16: 3; ModOfs: 0; RelOfs: 1; UseAdr: False; Copy: CopyNormal          ), // JNO
    (Size32: 5; Size16: 3; ModOfs: 0; RelOfs: 1; UseAdr: False; Copy: CopyNormal          ), // JB,JC,JNAE
    (Size32: 5; Size16: 3; ModOfs: 0; RelOfs: 1; UseAdr: False; Copy: CopyNormal          ), // JAE,JNB,JNC
    (Size32: 5; Size16: 3; ModOfs: 0; RelOfs: 1; UseAdr: False; Copy: CopyNormal          ), // JE,JZ,JZ
    (Size32: 5; Size16: 3; ModOfs: 0; RelOfs: 1; UseAdr: False; Copy: CopyNormal          ), // JNE,JNZ
    (Size32: 5; Size16: 3; ModOfs: 0; RelOfs: 1; UseAdr: False; Copy: CopyNormal          ), // JBE,JNA
    (Size32: 5; Size16: 3; ModOfs: 0; RelOfs: 1; UseAdr: False; Copy: CopyNormal          ), // JA,JNBE
    (Size32: 5; Size16: 3; ModOfs: 0; RelOfs: 1; UseAdr: False; Copy: CopyNormal          ), // JS
    (Size32: 5; Size16: 3; ModOfs: 0; RelOfs: 1; UseAdr: False; Copy: CopyNormal          ), // JNS
    (Size32: 5; Size16: 3; ModOfs: 0; RelOfs: 1; UseAdr: False; Copy: CopyNormal          ), // JP,JPE
    (Size32: 5; Size16: 3; ModOfs: 0; RelOfs: 1; UseAdr: False; Copy: CopyNormal          ), // JNP,JPO
    (Size32: 5; Size16: 3; ModOfs: 0; RelOfs: 1; UseAdr: False; Copy: CopyNormal          ), // JL,NGE
    (Size32: 5; Size16: 3; ModOfs: 0; RelOfs: 1; UseAdr: False; Copy: CopyNormal          ), // JGE,JNL
    (Size32: 5; Size16: 3; ModOfs: 0; RelOfs: 1; UseAdr: False; Copy: CopyNormal          ), // JLE,JNG
    (Size32: 5; Size16: 3; ModOfs: 0; RelOfs: 1; UseAdr: False; Copy: CopyNormal          ), // JG,JNLE
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // CMOVO (0F 40)
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // CMOVNO (0F 41)
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // CMOVB & CMOVC & CMOVNAE (0F 42)
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // CMOVAE & CMOVNB & CMOVNC (0F 43)
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // CMOVE & CMOVZ (0F 44)
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // CMOVNE & CMOVNZ (0F 45)
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // CMOVBE & CMOVNA (0F 46)
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // CMOVA & CMOVNBE (0F 47)
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // CMOVS (0F 48)
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // CMOVNS (0F 49)
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // CMOVP & CMOVPE (0F 4A)
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // CMOVNP & CMOVPO (0F 4B)
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // CMOVL & CMOVNGE (0F 4C)
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // CMOVGE & CMOVNL (0F 4D)
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // CMOVLE & CMOVNG (0F 4E)
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // CMOVG & CMOVNLE (0F 4F)
    (Size32: 2; Size16: 2; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // PUSH
    (Size32: 2; Size16: 2; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // POP
    (Size32: 2; Size16: 2; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // CPUID
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // BT  (0F A3)
    (Size32: 3; Size16: 3; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // SHLD
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // SHLD
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _A6
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _A7
    (Size32: 2; Size16: 2; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // PUSH
    (Size32: 2; Size16: 2; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // POP
    (Size32: 2; Size16: 2; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // RSM
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // BTS (0F AB)
    (Size32: 3; Size16: 3; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // SHRD
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // SHRD
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // FXRSTOR/1,FXSAVE/0
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // IMUL (0F AF)
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // CMPXCHG (0F B0)
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // CMPXCHG (0F B1)
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // LSS/r
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // BTR (0F B3)
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // LFS/r
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // LGS/r
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // MOVZX/r
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // MOVZX/r
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _B8
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _B9
    (Size32: 3; Size16: 3; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // BT & BTC & BTR & BTS (0F BA)
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // BTC (0F BB)
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // BSF (0F BC)
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // BSR (0F BD)
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // MOVSX/r
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // MOVSX/r
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // XADD/r
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // XADD/r
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _C2
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _C3
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _C4
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _C5
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _C6
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // CMPXCHG8B (0F C7)
    (Size32: 2; Size16: 2; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // BSWAP 0F C8 + rd
    (Size32: 2; Size16: 2; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // BSWAP 0F C8 + rd
    (Size32: 2; Size16: 2; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // BSWAP 0F C8 + rd
    (Size32: 2; Size16: 2; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // BSWAP 0F C8 + rd
    (Size32: 2; Size16: 2; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // BSWAP 0F C8 + rd
    (Size32: 2; Size16: 2; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // BSWAP 0F C8 + rd
    (Size32: 2; Size16: 2; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // BSWAP 0F C8 + rd
    (Size32: 2; Size16: 2; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // BSWAP 0F C8 + rd
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _D0
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // PSRLW/r
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // PSRLD/r
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // PSRLQ/r
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _D4
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // PMULLW/r
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _D6
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _D7
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // PSUBUSB/r
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // PSUBUSW/r
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _DA
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // PAND/r
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // PADDUSB/r
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // PADDUSW/r
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _DE
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // PANDN/r
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _E0
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // PSRAW/r
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // PSRAD/r
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _E3
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _E4
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // PMULHW/r
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _E6
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _E7
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // PSUBB/r
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // PSUBW/r
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _EA
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // POR/r
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // PADDSB/r
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // PADDSW/r
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _EE
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // PXOR/r
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _F0
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // PSLLW/r
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // PSLLD/r
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // PSLLQ/r
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _F4
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // PMADDWD/r
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _F6
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _F7
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // PSUBB/r
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // PSUBW/r
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // PSUBD/r
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ), // _FB
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // PADDB/r
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // PADDW/r
    (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0; UseAdr: False; Copy: CopyNormal          ), // PADDD/r
    (Size32: 1; Size16: 1; ModOfs: 0; RelOfs: 0; UseAdr: False; Copy: CopyInvalid         ));// _FF

const
  ModRMSizes: array [0..255] of ShortInt = (
    { Contains the number of extra bytes for each possible ModR/M byte.
      A negative value means that the instruction also uses a SIB-byte }
    0,0,0,0, -1,4,0,0,  0,0,0,0, -1,4,0,0,
    0,0,0,0, -1,4,0,0,  0,0,0,0, -1,4,0,0,
    0,0,0,0, -1,4,0,0,  0,0,0,0, -1,4,0,0,
    0,0,0,0, -1,4,0,0,  0,0,0,0, -1,4,0,0,
    1,1,1,1,  2,1,1,1,  1,1,1,1,  2,1,1,1,
    1,1,1,1,  2,1,1,1,  1,1,1,1,  2,1,1,1,
    1,1,1,1,  2,1,1,1,  1,1,1,1,  2,1,1,1,
    1,1,1,1,  2,1,1,1,  1,1,1,1,  2,1,1,1,
    4,4,4,4,  5,4,4,4,  4,4,4,4,  5,4,4,4,
    4,4,4,4,  5,4,4,4,  4,4,4,4,  5,4,4,4,
    4,4,4,4,  5,4,4,4,  4,4,4,4,  5,4,4,4,
    4,4,4,4,  5,4,4,4,  4,4,4,4,  5,4,4,4,
    0,0,0,0,  0,0,0,0,  0,0,0,0,  0,0,0,0,
    0,0,0,0,  0,0,0,0,  0,0,0,0,  0,0,0,0,
    0,0,0,0,  0,0,0,0,  0,0,0,0,  0,0,0,0,
    0,0,0,0,  0,0,0,0,  0,0,0,0,  0,0,0,0);

implementation


{ TCopyInstructionProc functions used for copying instructions: }

function CopyNormal(const Intercepts: PIntercepts; Entry: POpCodeEntry;
  Src, Dst: Pointer): Pointer;
{ Default function for copying instructions }
var
  FixedSize, Size, ModRMSize: Integer;
  ModRM, SIB: Byte;
  P: PByte;
begin
  { Determine the number of fixed bytes used by the instruction }
  if Entry.UseAdr then
    if Intercepts.Use16BitAddress then
      FixedSize := Entry.Size16
    else
      FixedSize := Entry.Size32
  else
    if Intercepts.Use16BitOperand then
      FixedSize := Entry.Size16
    else
      FixedSize := Entry.Size32;
  { Determine the size of the instruction }
  Size := FixedSize;
  if Entry.ModOfs > 0 then begin
    { The instruction uses a ModR/M byte }
    P := Src;
    Inc(P,Entry.ModOfs);
    ModRM := P^;
    { Determine extra bytes used by ModR/M }
    ModRMSize := ModRMSizes[ModRM];
    if ModRMSize < 0 then begin
      { The instruction also uses a SIB-byte }
      ModRMSize := -ModRMSize;
      Inc(P);
      SIB := P^;
      { If the Base of the SIB-byte (lower 3 bits) have value 5, the instruction
        may use 1 or 4 extra bytes, depending on ModR/M }
      if (SIB and $07) = 5 then
        case (ModRM and $C0) of
          $00, $80: Inc(Size,4);
          $40     : Inc(Size);
        end;
    end;
    Inc(Size,ModRMSize);
  end;
  { Copy the calculated number of bytes }
  Move(Src^,Dst^,Size);
  if Entry.RelOfs <> 0 then
    { The instruction uses a relative offset, for example "je -500". We cannot
      just copy this instruction to a new address, because the code at the
      new address -500 bytes is incorrect (see comment on AdjustTarget method) }
    Intercepts.AdjustTarget(Src,Dst,FixedSize,Entry.RelOfs);
  Result := Src;
  Inc(PByte(Result),Size);
end;

function Copy2ByteOpCode(const Intercepts: PIntercepts; Entry: POpCodeEntry;
  Src, Dst: Pointer): Pointer;
{ Function for copying 2 byte long OpCodes. The first OpCode byte is $0F }
begin
  { Copy the first OpCode byte (that is, the $0F prefix) }
  CopyNormal(Intercepts,Entry,Src,Dst);
  Inc(PByte(Src)); Inc(PByte(Dst));
  { Copy the remainder using the TwoByteOpCodes instruction table }
  Entry := @TwoByteOpCodes[PByte(Src)^];
  Result := Entry.Copy(Intercepts,Entry,Src,Dst);
end;

function CopyWithPrefix(const Intercepts: PIntercepts; Entry: POpCodeEntry;
  Src, Dst: Pointer): Pointer;
{ Function for copying instructions that contain prefix bytes }
begin
  { Copy the prefix byte the normal way }
  CopyNormal(Intercepts,Entry,Src,Dst);
  Inc(PByte(Src)); Inc(PByte(Dst));
  { Copy the remainder using the normal OpCode instruction table }
  Entry := @OneByteOpCodes[PByte(Src)^];
  Result := Entry.Copy(Intercepts,Entry,Src,Dst);
end;

function CopyWith16BitOperand(const Intercepts: PIntercepts; Entry: POpCodeEntry;
  Src, Dst: Pointer): Pointer;
{ Function for copying instructions that contain a prefix instructing to use
  16-bit operands }
begin
  Intercepts.Use16BitOperand := True;
  Result := CopyWithPrefix(Intercepts,Entry,Src,Dst);
end;

function CopyWith16BitAddress(const Intercepts: PIntercepts; Entry: POpCodeEntry;
  Src, Dst: Pointer): Pointer;
{ Function for copying instructions that contain a prefix instructing to use
  16-bit addresses }
begin
  Intercepts.Use16BitAddress := True;
  Result := CopyWithPrefix(Intercepts,Entry,Src,Dst);
end;

function CopyOpF6(const Intercepts: PIntercepts; Entry: POpCodeEntry;
  Src, Dst: Pointer): Pointer;
{ Function for copying instructions with OpCode $F6 (test, div, idiv, mul,
  imul, not and neg using a 8-bit operand) }
const
  E1: TOpCodeEntry = (Size32: 3; Size16: 3; ModOfs: 1; RelOfs: 0;
                      UseAdr: False; Copy: CopyNormal);
  E2: TOpCodeEntry = (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0;
                      UseAdr: False; Copy: CopyNormal);
begin
  { $F6-OpCodes are 2 or 3 bytes in size, depending on bits 3-5 of the ModR/M
    byte. If these bits are all 0, the instruction is 3 bytes, otherwise it
    is 2 bytes in size }
  Inc(PByte(Src));
  if PByte(Src)^ and $38 = 0 then
    Entry := @E1
  else
    Entry := @E2;
  Dec(PByte(Src));
  Result := Entry.Copy(Intercepts,Entry,Src,Dst);
end;

function CopyOpF7(const Intercepts: PIntercepts; Entry: POpCodeEntry;
  Src, Dst: Pointer): Pointer;
{ Function for copying instructions with OpCode $F7 (test, div, idiv, mul,
  imul, not and neg using a16/32-bit operand) }
const
  E1: TOpCodeEntry = (Size32: 6; Size16: 4; ModOfs: 1; RelOfs: 0;
                      UseAdr: False; Copy: CopyNormal);
  E2: TOpCodeEntry = (Size32: 2; Size16: 2; ModOfs: 1; RelOfs: 0;
                      UseAdr: False; Copy: CopyNormal);
begin
  { $F6-OpCodes are 2, 4 or 6 bytes in size, depending on bits 3-5 of the ModR/M
    byte. If these bits are all 0, the instruction is 6 bytes in 32-bit mode or
    4 bytes in 16-bit mode. Otherwise it is 2 bytes in size }
  Inc(PByte(Src));
  if PByte(Src)^ and $38 = 0 then
    Entry := @E1
  else
    Entry := @E2;
  Dec(PByte(Src));
  Result := Entry.Copy(Intercepts,Entry,Src,Dst);
end;

function CopyInvalid(const Intercepts: PIntercepts; Entry: POpCodeEntry;
  Src, Dst: Pointer): Pointer;
{ Called when trying to copy an invalid OpCode. Just skips the OpCode. }
begin
  Assert(False,'Invalid OpCode');
  Result := Src;
  Inc(PByte(Result));
end;

{ PIntercepts }

procedure TIntercepts.AdjustTarget(const Src, Dst: Pointer; const OpCodeSize,
  TargetOffset: Integer);
var
  TargetSize, OrigOffset, NewOffset: Integer;
  TargetAddr: Pointer;
begin
  { Example:
      Src: $00005000 jmp -$1000   --->    Dst: $00007000 jmp -$1000
    Dst must be adjusted to:
      Dst: $00007000 jmp -$3000
    The parameters are:
      Src: $00005000
      Dst: $00007000
      OpCodeSize: 5 (size of a relative JMP instruction)
      TargetOffset: 1 (the target is located 1 byte after the OpCode)
    First, calculate then target size (the size in bytes of the relative
    target) and the corresponding address of the target ($00007001 in this
    example). }
  TargetSize := OpCodeSize - TargetOffset;
  TargetAddr := Dst;
  Inc(PByte(TargetAddr),TargetOffset);
  { Get the original target value (-$1000 in the example). This depends on the
    size of the target, either 1, 2 or 4 bytes. }
  case TargetSize of
    1: OrigOffset := PShortInt(TargetAddr)^;
    2: OrigOffset := PSmallInt(TargetAddr)^;
    4: OrigOffset := PLongInt(TargetAddr)^;
  else
    begin
      Assert(False);
      OrigOffset := 0;
    end;
  end;
  { Calculate the new target value based on the original value and the
    difference between the Src and Dst location. In the example, the new value
    should be -$1000 - ($00007000 - $00005000) = -$3000.
    The new value could potentionally fall outside the range of the target.
    This is checked with assertions. }
  NewOffset := OrigOffset - (Integer(Dst) - Integer(Src));
  case TargetSize of
    1: begin
         Assert((NewOffset >= Low(ShortInt)) and (NewOffset <= High(ShortInt)));
         PShortInt(TargetAddr)^ := NewOffset;
       end;
    2: begin
         Assert((NewOffset >= Low(SmallInt)) and (NewOffset <= High(SmallInt)));
         PSmallInt(TargetAddr)^ := NewOffset;
       end;
    4: PLongInt(TargetAddr)^ := NewOffset;
  end;
end;

function TIntercepts.CopyInstruction(Src, Dst: Pointer): Pointer;
var
  Entry: POpCodeEntry;
begin
  Assert(Assigned(Src));
  if not Assigned(Dst) then
    Dst := @FScratch;
  { Default to 32-bit operands and addresses }
  FUse16BitAddress := False;
  FUse16BitOperand := False;
  { Get information about the OpCode at Src }
  Entry := @OneByteOpCodes[PByte(Src)^];
  Result := Entry.Copy(@Self,Entry,Src,Dst);
end;

function NewIntercepts:TIntercepts;
begin
  Result := TIntercepts.Create;
  Result.FProcess := GetCurrentProcess;
end;

function TIntercepts.CreateIntercept(TargetProc, InterceptProc: Pointer): Pointer;
begin
  Assert(Assigned(TargetProc) and Assigned(InterceptProc));
  Result := VirtualAlloc(nil,TrampolineSize,MEM_COMMIT,PAGE_EXECUTE_READWRITE);
  TargetProc := GetFinalCode(TargetProc);
  InterceptProc := GetFinalCode(InterceptProc);
  if not InsertIntercept(TargetProc,Result,InterceptProc) then
  begin
    FreeMem(Result);
    Result := nil;
  end;
end;

class function TIntercepts.GetFinalCode(const Proc: Pointer;
  const SkipJumps: Boolean): Pointer;
begin
  Assert(Assigned(Proc));
  Result := Proc;
  if Assigned(Result) then begin
    if PWord(Result)^ = opJmpIndirect then begin
      { The assembly code of Proc has the following format:
          jmp dword ptr [<ImportTableEntry>]
        This means that Proc uses a DLL import table. We need to get the
        address of the actual routine.
        First, retrieve the address of the import table entry by dereferencing
        the pointer after the JMP instruction (<ImportTableEntry>) }
      Inc(PWord(Result));
      Result := PPointer(Result)^;
      { Next, derefence this pointer again to get to the actual implementation.
        That is, the address at the import table entry, points to the actual
        implementation }
      Result := PPointer(Result)^;
    end else if (PByte(Result)^ = opJmpRelative) and SkipJumps then begin
      { The assembly code of Proc has the following format:
          jmp <RelativeDisplacement>
        When this method is called with SkipJumps=True, we need to get the
        actual address this instruction jumps to. To get this address, we
        need to increment the pointer <RelativeDisplacement> bytes, plus
        the size of the JMP-instruction itself (which is 5 bytes).
        Note that you need to typecast the Result to a PByte in order to
        advance the pointer a certain number of bytes. }
      Inc(PByte(Result));
      Inc(PByte(Result),PInteger(Result)^ + SizeOfJmp);
    end;
  end;
end;

function TIntercepts.InsertIntercept(const TargetProc, Trampoline,
  InterceptProc: Pointer): Boolean;
var
  P, Q: Pointer;
  BytesToCopy, BytesCopied: Integer;
  OrigTrampolineAccess, OrigTargetProcAccess: Longword;
begin
  Result := False;
  P := TargetProc;
  BytesToCopy := 0;
  while BytesToCopy < SizeOfJmp do
  begin
    { Calculate the size of the assembly instructions to be copied to the
      trampoline by dummy-copying assembly instructions until at least 5 bytes
      (SizeOfJmp) have been processed }
    Q := P;
    P := CopyInstruction(P,nil);
    BytesToCopy := Integer(P) - Integer(TargetProc);
    { When the OpCode is a JMP or RET instruction, we do not need to copy
      more instructions (as they will never be executed) }
    if PByte(Q)^ in [opJmpRelative,opRetPop,opRet] then
      Break;
    if (PWord(Q)^ = opJmpEax) or (PWord(Q)^ = opJmpIndirect) then
      Break;
    if PByte(Q)^ in [opPreES,opPreCS,opPreSS,opPreDS,opPreFS,opPreGS] then begin
      Inc(PByte(Q));
      if PWord(Q)^ = opJmpIndirect then
        Break;
    end;
  end;
  { We need at least 5 bytes to insert a JMP instruction }
  if BytesToCopy < SizeOfJmp then
    Exit
  { Don't copy beyond the trampoline }
  else if BytesToCopy > (TrampolineSize - SizeOfJmp - 1) then
    Exit;
  { Enable write access to original routine and trampoline }
  OrigTargetProcAccess := 0;
  OrigTrampolineAccess :=
    SetPermission(Trampoline,TrampolineSize,PAGE_EXECUTE_READWRITE);
  try
    OrigTargetProcAccess :=
      SetPermission(TargetProc,BytesToCopy,PAGE_EXECUTE_READWRITE);
    { Copy first few original instructions to trampoline }
    BytesCopied := 0;
    P := TargetProc;
    Q := Trampoline;
    while BytesCopied < BytesToCopy do begin
      P := CopyInstruction(P,Q);
      BytesCopied := Integer(P) - Integer(TargetProc);
      Q := Trampoline;
      Inc(PByte(Q),BytesCopied);
    end;
    if BytesCopied <> BytesToCopy then
      Exit;
    { Add an unconditional jump to the trampoline (Q) to jump to the remainder
      of the original routine (P) }
    if not InsertJump(Q,P,SizeOfJmp) then
      Exit;
    { Set the last byte of the trampoline to the size of the copied code. This
      byte is later used by RemoveIntercept the restore the situation. }
    Q := Trampoline;
    Inc(PByte(Q),TrampolineSize - 1);
    PByte(Q)^ := BytesCopied;
    { Put an unconditional jump at the beginning of to the original routine
      (TargetProc) to jump to the Intercept routine (InterceptProc) }
    Result := InsertJump(TargetProc,InterceptProc,BytesCopied);
  finally
    { Restore permissions }
    SetPermission(Trampoline,TrampolineSize,OrigTrampolineAccess);
    SetPermission(TargetProc,BytesToCopy,OrigTargetProcAccess);
  end;
end;

function TIntercepts.InsertJump(Code, Target: Pointer;
  Size: Integer): Boolean;
begin
  Result := (Size >= SizeOfJmp);
  if Result then begin
    { Add a relative JMP instruction (jmp <Displacement>) }
    PByte(Code)^ := opJmpRelative;
    Inc(PByte(Code));
    { Set the <displacement> to Target - (Code + SizeOfJmp) }
    PInteger(Code)^ := Integer(Target) - (Integer(Code) - 1 + SizeOfJmp);
    Inc(PInteger(Code));
    Dec(Size,SizeOfJmp);
    { Fill the remainder of Code with "int 3" instructions. These instructions
      should never be executed, but in case they do anyway, "int 3" is used
      as a debug breakpoint so control is passed to the debugger }
    while Size > 0 do begin
      PByte(Code)^ := opInt3;
      Inc(PByte(Code));
      Dec(Size);
    end;
  end;
end;

function TIntercepts.RemoveIntercept(Trampoline,
  InterceptProc: Pointer): Boolean;
var
  CodeSize, Target, BytesCopied: Integer;
  OrigProcAccess: Longword;
  P, Q, Src, Dst: PByte;
begin
  Trampoline := GetFinalCode(Trampoline,True);
  InterceptProc := GetFinalCode(InterceptProc);
  { Retrieve codesize from last byte of trampoline }
  P := Trampoline;
  Inc(P,TrampolineSize - 1);
  CodeSize := P^;
  Result := (CodeSize > 0) and (CodeSize < TrampolineSize - 1);
  if Result then begin
    { The last instruction of the trampoline must be a JMP to the remainder of
      the original routine}
    P := Trampoline;
    Inc(P,CodeSize);
    Result := (P^ = opJmpRelative);
    if Result then begin
      { Retrieve jump target }
      Inc(P);
      Target := PInteger(P)^;
      { Set P to the beginning of the original routine }
      P := Trampoline;
      Inc(P,Target + SizeOfJmp);
      { The original routine must start with a JMP to the Intercept }
      Result := (P^ = opJmpRelative);
      if Result then begin
        { Retrieve jump target }
        Inc(P);
        Target := PInteger(P)^;
        Dec(P);
        { Check that the target points to the Intercept }
        Q := P;
        Inc(Q,Target + SizeOfJmp);
        Result := (Q = InterceptProc);
        if Result then begin
          { Enable write access to original routine to remove Intercept }
          OrigProcAccess := SetPermission(P,CodeSize,PAGE_EXECUTE_READWRITE);
          try
            { Copy instructions from trampoline back to original routine }
            BytesCopied := 0;
            Src := Trampoline;
            Dst := P;
            while BytesCopied < CodeSize do begin
              Src := CopyInstruction(Src,Dst);
              BytesCopied := Integer(Src) - Integer(Trampoline);
              Dst := P;
              Inc(Dst,BytesCopied);
            end;
            Result := (BytesCopied = CodeSize);
          finally
            SetPermission(P,CodeSize,OrigProcAccess);
          end;
        end;
      end;
    end;
  end;
end;

function TIntercepts.SetPermission(const Code: Pointer; const Size: Integer;
  const Permission: Longword): Longword;
begin
  Assert(Assigned(Code) and (Size > 0));
  { Flush the instruction cache so changes to the code page are effective
    immediately }
  if Permission <> 0 then
    if FlushInstructionCache(FProcess,Code,Size) then
      VirtualProtect(Code,Size,Permission,Longword(Result));
end;

{ Main Intercepts functions }

function InterceptCreate(const TargetProc, InterceptProc: Pointer): Pointer;
var
  Intercepts: TIntercepts;
begin
  if Assigned(TargetProc) and Assigned(InterceptProc) then
  begin
    Intercepts := NewIntercepts;
    try
      Result := Intercepts.CreateIntercept(TargetProc,InterceptProc);
    finally
      Intercepts.Free;
    end;
  end
  else
    Result := nil;
end;

function InterceptRemove(var Trampoline: Pointer; const InterceptProc: Pointer): Boolean;
var
  Intercepts: TIntercepts;
begin
  if Assigned(Trampoline) and Assigned(InterceptProc) then
  begin
    Intercepts := NewIntercepts;
    try
      Result := Intercepts.RemoveIntercept(Trampoline,InterceptProc);
      VirtualFree(Trampoline, TrampolineSize, MEM_RELEASE);
      Trampoline := nil;
    finally
      Intercepts.Free;
    end;
  end
  else
    Result := False;
end;

end.
