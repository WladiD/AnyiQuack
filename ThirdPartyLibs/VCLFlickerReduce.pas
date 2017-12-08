{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J-,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
(*************************************************************************

Copyright (c) 2006-2007 Andreas Hausladen (http://unvclx.sourceforge.net)


This software is provided 'as-is', without any express or implied
warranty. In no event will the author be held liable for any damages
arising from the use of this software.

Permission is granted to anyone to use this software for any purpose,
including commercial applications, and to alter it and redistribute it
freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented, you must 
     not claim that you wrote the original software. If you use this 
     software in a product, an acknowledgment in the product documentation 
     would be appreciated but is not required.

  2. Altered source versions must be plainly marked as such, and must not 
     be misrepresented as being the original software.

  3. This notice may not be removed or altered from any source distribution.

*************************************************************************)

(*************************************************************************
History:
2008-05-16:
  - Complete rewrite due to new information
2007-01-31:
  - Complete rewrite
2006-11-21:
  - fixed region bugs
  - fixed wrong DefaultHandler call
  - fixed parent background painting
  - added region exclusion for sub-controls of transparent control children
2006-10-03:
  - fixed RangeChecks disabled
*************************************************************************)

unit VCLFlickerReduce;

{$D-} // no debugging

{$IFDEF CONDITIONALEXPRESSIONS}
 {$IF RTLVersion >= 15.00}
  {$DEFINE HAS_THEMES_UNIT}
 {$IFEND}
 {$IF CompilerVersion >= 17.00}
  {$DEFINE NEW_WM_PAINT}
 {$IFEND}
{$ENDIF CONDITIONALEXPRESSIONS}

interface

uses
  Windows, Messages, SysUtils, Classes,
  {$IFDEF HAS_THEMES_UNIT}
  Themes,
  {$ENDIF HAS_THEMES_UNIT}
  Graphics, Controls, Forms, StdCtrls, ExtCtrls, ComCtrls;

implementation

uses
  Types;

// ------- BEGIN Memory manipulation functions ----------

type
  PPointer = ^Pointer; // Delphi 5

type
  TXRedirCode = packed record
    Jump: Byte;
    Offset: Integer;
  end;

  TRedirectCode = packed record
    RealProc: Pointer;
    Count: Integer;
    case Byte of
      0: (Code: TXRedirCode);
      1: (Code2: Int64);
  end;

function WriteProtectedMemory(BaseAddress, Buffer: Pointer; Size: NativeUInt;
  out WrittenBytes: NativeUInt): Boolean;
var
  OldProt: Cardinal;
begin
  VirtualProtect(BaseAddress, Size, PAGE_EXECUTE_READWRITE, OldProt);
  Result := WriteProcessMemory(GetCurrentProcess, BaseAddress, Buffer, Size, WrittenBytes);
  VirtualProtect(BaseAddress, Size, OldProt, nil);
  FlushInstructionCache(GetCurrentProcess, BaseAddress, WrittenBytes);
end;

function ReadProtectedMemory(BaseAddress, Buffer: Pointer; Size: NativeUInt;
  out ReadBytes: NativeUInt): Boolean;
begin
  Result := ReadProcessMemory(GetCurrentProcess, BaseAddress, Buffer, Size, ReadBytes);
end;

function GetActualAddr(Proc: Pointer): Pointer;
type
  PWin9xDebugThunk = ^TWin9xDebugThunk;
  TWin9xDebugThunk = packed record
    Push: Byte;
    Addr: Pointer;
    Jump: Byte;
    Offset: Integer;
  end;

  PAbsoluteIndirectJmp = ^TAbsoluteIndirectJmp;
  TAbsoluteIndirectJmp = packed record
    OpCode: Word;
    Addr: ^Pointer;
  end;

  function IsWin9xDebugThunk(P: PWin9xDebugThunk): Boolean;
  begin
    Result := (P.Push = $68) and (P.Jump = $E9);
  end;

begin
  if (Proc <> nil) and not IsBadReadPtr(Proc, 6) then
  begin
    if (SysUtils.Win32Platform <> VER_PLATFORM_WIN32_NT) and IsWin9xDebugThunk(Proc) then
      Proc := PWin9xDebugThunk(Proc).Addr;
    if (PAbsoluteIndirectJmp(Proc).OpCode = $25FF) then
      Result := PAbsoluteIndirectJmp(Proc).Addr^
    else
      Result := Proc;
  end
  else
    Result := nil;
end;

procedure CodeRedirectEx(Proc: Pointer; NewProc: Pointer; out Data: TRedirectCode);
type
  PPointer = ^Pointer;
  TRelocationRec = packed record
    Jump: Word;
    Address: PPointer;
  end;

var
  OldProtect: Cardinal;
begin
  if Proc = nil then
  begin
    Data.RealProc := nil;
    Exit;
  end;
  if Data.Count = 0 then // do not overwrite an already backuped code
  begin
    Proc := GetActualAddr(Proc);
    if VirtualProtect(Proc, SizeOf(Data.Code) + 1, PAGE_EXECUTE_READWRITE, OldProtect) then
    begin
      Data.RealProc := Proc;
      Data.Code2 := Int64(Proc^);
      TXRedirCode(Proc^).Jump := $E9;
      TXRedirCode(Proc^).Offset := Integer(NewProc) - Integer(Proc) - (SizeOf(Data.Code));
      VirtualProtect(Proc, SizeOf(Data.Code) + 1, OldProtect, @OldProtect);
    end;
  end;
  Inc(Data.Count);
end;

function CodeRedirect(Proc: Pointer; NewProc: Pointer): TRedirectCode;
begin
  Result.Count := 0;
  Result.RealProc := nil;
  CodeRedirectEx(Proc, NewProc, Result);
end;

procedure CodeRestore(var Data: TRedirectCode);
var
  n: NativeUInt;
begin
  if (Data.RealProc <> nil) and (Data.Count = 1) then
    WriteProtectedMemory(Data.RealProc, @Data.Code, SizeOf(Data.Code), n);
  Dec(Data.Count);
end;

function GetDynamicMethod(AClass: TClass; Index: Integer): Pointer; assembler;
asm
  call System.@FindDynaClass
end;

// ------- END Memory manipulation functions ----------

type
  TOpenWinControl = class(TWinControl);

{$IFNDEF NEW_WM_PAINT} // Delphi 2007 has this function in the VCL
procedure WinControlPaint(Control: TWinControl; var Message: TWMPaint);
var
  DC, MemDC: HDC;
  MemBitmap, OldBitmap: HBITMAP;
  PS: TPaintStruct;
begin
  if not Control.DoubleBuffered or (Message.DC <> 0) then
  begin
    if not (csCustomPaint in Control.ControlState) and (Control.ControlCount = 0) then
      Control.DefaultHandler(Message)
    else
      TOpenWinControl(Control).PaintHandler(Message);
  end
  else
  begin
    DC := BeginPaint(Control.Handle, PS);
    MemBitmap := CreateCompatibleBitmap(DC, PS.rcPaint.Right - PS.rcPaint.Left,
      PS.rcPaint.Bottom - PS.rcPaint.Top);
    MemDC := CreateCompatibleDC(DC);
    OldBitmap := SelectObject(MemDC, MemBitmap);
    SetWindowOrgEx(MemDC, PS.rcPaint.Left, PS.rcPaint.Top, nil);
    try
      Control.Perform(WM_ERASEBKGND, MemDC, MemDC);
      Message.DC := MemDC;
      WinControlPaint(Control, Message);
      Message.DC := 0;
      BitBlt(DC, PS.rcPaint.Left, PS.rcPaint.Top,
        PS.rcPaint.Right - PS.rcPaint.Left,
        PS.rcPaint.Bottom - PS.rcPaint.Top,
        MemDC,
        PS.rcPaint.Left, PS.rcPaint.Top,
        SRCCOPY);
    finally
      EndPaint(Control.Handle, PS);
      SelectObject(MemDC, OldBitmap);
      DeleteDC(MemDC);
      DeleteObject(MemBitmap);
    end;
  end;
end;
{$ENDIF ~NEW_WM_PAINT}

procedure InvalidateSyncPaintControls(Control: TWinControl);
var
  I: Integer;
  Ctrl: TWinControl;
  ChildR: TRect;
begin
  with Control do
  begin
    if HandleAllocated then
    begin
      if GetUpdateRect(Handle, ChildR, False) then
      begin
        if not (csOpaque in ControlStyle) then
          InvalidateRect(Handle, @ChildR, True);

        {$IFDEF HAS_THEMES_UNIT}
        if StyleServices.Enabled then
          for I := 0 to ControlCount - 1 do
            if Controls[I].Visible and (Controls[I] is TWinControl) then
            begin
              Ctrl := TWinControl(Controls[i]);
              if Ctrl.HandleAllocated and (csParentBackground in Ctrl.ControlStyle) then
                if GetUpdateRect(Ctrl.Handle, ChildR, False) then
                  InvalidateSyncPaintControls(Ctrl);
            end;
        {$ENDIF HAS_THEMES_UNIT}
      end;
    end;
  end;
end;

procedure WinControlSetBounds(Self: TOpenWinControl; ALeft, ATop, AWidth, AHeight: Integer);
var
  WindowPlacement: TWindowPlacement;
  R: TRect;
begin
  with Self do
  begin
    if (ALeft <> Left) or (ATop <> Top) or
       (AWidth <> Width) or (AHeight <> Height) then
    begin
      if HandleAllocated and not IsIconic(Handle) then
      begin
        R := BoundsRect;
        SetWindowPos(Handle, 0, ALeft, ATop, AWidth, AHeight,
          SWP_NOZORDER or SWP_NOACTIVATE or SWP_DEFERERASE);
        if Parent <> nil then
          InvalidateSyncPaintControls(Parent);
        R := BoundsRect;
      end
      else
      begin
        R := Rect(ALeft, ATop, ALeft + AWidth, ATop + AHeight);
        if HandleAllocated then
        begin
          WindowPlacement.Length := SizeOf(WindowPlacement);
          GetWindowPlacement(Handle, @WindowPlacement);
          WindowPlacement.rcNormalPosition := BoundsRect;
          SetWindowPlacement(Handle, @WindowPlacement);
        end;
      end;
      UpdateBoundsRect(R);
      RequestAlign;
    end;
  end;
end;

var
  {$IFNDEF NEW_WM_PAINT}
  WinControlWMPaintHook: TRedirectCode;
  {$ENDIF ~NEW_WM_PAINT}
  WinControlSetBoundsHook: TRedirectCode;

initialization
  WinControlSetBoundsHook := CodeRedirect(@TWinControl.SetBounds, @WinControlSetBounds);
  {$IFNDEF NEW_WM_PAINT}
//  WinControlWMPaintHook := CodeRedirect(GetDynamicMethod(TWinControl, WM_PAINT), @WinControlPaint);
  {$ENDIF ~NEW_WM_PAINT}

finalization
  CodeRestore(WinControlSetBoundsHook);
  {$IFNDEF NEW_WM_PAINT}
//  CodeRestore(WinControlWMPaintHook);
  {$ENDIF ~NEW_WM_PAINT}

end.
