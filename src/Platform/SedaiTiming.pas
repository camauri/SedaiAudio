{*
 * Sedai Audio Foundation - Cross-Platform High-Precision Timing
 *
 * TSedaiTiming provides microsecond-accurate timing abstraction
 * using platform-native APIs for maximum precision.
 *
 * Platform implementations:
 * - Windows: QueryPerformanceCounter / QueryPerformanceFrequency
 * - Linux: clock_gettime(CLOCK_MONOTONIC)
 * - macOS: mach_absolute_time
 * - Fallback: FPC GetTickCount64
 *
 * (c) 2025 Artiforge - Licensed under GPL-3.0
 *}
unit SedaiTiming;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  {$IFDEF UNIX}
    {$IFDEF DARWIN}
    // macOS uses mach_absolute_time
    {$ELSE}
    // Linux uses clock_gettime
    BaseUnix, Unix,
    {$ENDIF}
  {$ENDIF}
  SysUtils;

type
  { TSedaiTiming }
  // Cross-platform high-precision timing class
  // All methods are class methods for easy access without instantiation
  TSedaiTiming = class
  private class var
    FInitialized: Boolean;
    FFrequency: Int64;           // Ticks per second
    {$IFDEF DARWIN}
    FMachTimebaseInfo: record
      Numer: UInt32;
      Denom: UInt32;
    end;
    {$ENDIF}
  public
    // Initialize the timing subsystem (called automatically on first use)
    class procedure Initialize;

    // Get current high-precision tick count
    class function GetTicks: Int64;

    // Get timer frequency (ticks per second)
    class function GetFrequency: Int64;

    // Convert ticks to microseconds
    class function TicksToMicroseconds(ATicks: Int64): Int64;

    // Convert microseconds to ticks
    class function MicrosecondsToTicks(AMicroseconds: Int64): Int64;

    // Convert ticks to milliseconds
    class function TicksToMilliseconds(ATicks: Int64): Int64;

    // Convert milliseconds to ticks
    class function MillisecondsToTicks(AMilliseconds: Int64): Int64;

    // Get elapsed microseconds since a start tick
    class function GetElapsedMicroseconds(AStartTicks: Int64): Int64;

    // Get elapsed milliseconds since a start tick
    class function GetElapsedMilliseconds(AStartTicks: Int64): Int64;

    // High-precision sleep (hybrid: OS sleep + busy-wait for precision)
    // Note: For very short sleeps (<2ms), uses pure busy-wait
    class procedure PreciseSleep(AMicroseconds: Int64);

    // Simple busy-wait (100% CPU but maximum precision)
    class procedure BusyWait(AMicroseconds: Int64);

    // Get current time in seconds (high precision)
    class function GetTimeSeconds: Double;

    // Properties
    class property Initialized: Boolean read FInitialized;
    class property Frequency: Int64 read FFrequency;
  end;

// Convenience functions (procedural interface)
function SedaiGetTicks: Int64; inline;
function SedaiTicksToMicroseconds(ATicks: Int64): Int64; inline;
function SedaiMicrosecondsToTicks(AMicroseconds: Int64): Int64; inline;
function SedaiGetElapsedMicroseconds(AStartTicks: Int64): Int64; inline;
procedure SedaiPreciseSleep(AMicroseconds: Int64); inline;

implementation

{$IFDEF DARWIN}
// macOS: mach_absolute_time
function mach_absolute_time: UInt64; cdecl; external 'libc.dylib';
function mach_timebase_info(info: Pointer): Integer; cdecl; external 'libc.dylib';
{$ENDIF}

{$IFDEF UNIX}
{$IFNDEF DARWIN}
// Linux: clock_gettime is in BaseUnix/Unix units
const
  CLOCK_MONOTONIC = 1;
{$ENDIF}
{$ENDIF}

{ TSedaiTiming }

class procedure TSedaiTiming.Initialize;
{$IFDEF WINDOWS}
var
  LFreq: Int64;
{$ENDIF}
{$IFDEF UNIX}
{$IFNDEF DARWIN}
var
  LTimeSpec: TTimeSpec;
{$ENDIF}
{$ENDIF}
begin
  if FInitialized then Exit;

  {$IFDEF WINDOWS}
  // Windows: QueryPerformanceFrequency
  if QueryPerformanceFrequency(LFreq) then
    FFrequency := LFreq
  else
    FFrequency := 1000;  // Fallback to milliseconds
  {$ENDIF}

  {$IFDEF DARWIN}
  // macOS: mach_absolute_time with timebase info
  mach_timebase_info(@FMachTimebaseInfo);
  // Frequency = 1e9 * denom / numer (nanoseconds)
  FFrequency := 1000000000;  // We work in nanoseconds internally
  {$ENDIF}

  {$IFDEF UNIX}
  {$IFNDEF DARWIN}
  // Linux: clock_gettime returns nanoseconds
  FFrequency := 1000000000;  // Nanoseconds
  // Verify clock is available
  if clock_gettime(CLOCK_MONOTONIC, @LTimeSpec) <> 0 then
    FFrequency := 1000;  // Fallback
  {$ENDIF}
  {$ENDIF}

  // Fallback for unknown platforms
  {$IFNDEF WINDOWS}
  {$IFNDEF UNIX}
  FFrequency := 1000;  // Millisecond precision fallback
  {$ENDIF}
  {$ENDIF}

  FInitialized := True;
end;

class function TSedaiTiming.GetTicks: Int64;
{$IFDEF WINDOWS}
var
  LTicks: Int64;
{$ENDIF}
{$IFDEF DARWIN}
var
  LMachTime: UInt64;
{$ENDIF}
{$IFDEF UNIX}
{$IFNDEF DARWIN}
var
  LTimeSpec: TTimeSpec;
{$ENDIF}
{$ENDIF}
begin
  if not FInitialized then Initialize;

  {$IFDEF WINDOWS}
  QueryPerformanceCounter(LTicks);
  Result := LTicks;
  {$ENDIF}

  {$IFDEF DARWIN}
  LMachTime := mach_absolute_time;
  // Convert to nanoseconds using timebase
  Result := (LMachTime * FMachTimebaseInfo.Numer) div FMachTimebaseInfo.Denom;
  {$ENDIF}

  {$IFDEF UNIX}
  {$IFNDEF DARWIN}
  if clock_gettime(CLOCK_MONOTONIC, @LTimeSpec) = 0 then
    Result := Int64(LTimeSpec.tv_sec) * 1000000000 + LTimeSpec.tv_nsec
  else
    Result := GetTickCount64 * 1000000;  // Fallback to ms * 1e6
  {$ENDIF}
  {$ENDIF}

  // Fallback
  {$IFNDEF WINDOWS}
  {$IFNDEF UNIX}
  Result := GetTickCount64;
  {$ENDIF}
  {$ENDIF}
end;

class function TSedaiTiming.GetFrequency: Int64;
begin
  if not FInitialized then Initialize;
  Result := FFrequency;
end;

class function TSedaiTiming.TicksToMicroseconds(ATicks: Int64): Int64;
begin
  if not FInitialized then Initialize;

  {$IFDEF WINDOWS}
  // Windows: ticks * 1000000 / frequency
  Result := (ATicks * 1000000) div FFrequency;
  {$ELSE}
  // Unix/macOS: ticks are in nanoseconds, divide by 1000
  Result := ATicks div 1000;
  {$ENDIF}
end;

class function TSedaiTiming.MicrosecondsToTicks(AMicroseconds: Int64): Int64;
begin
  if not FInitialized then Initialize;

  {$IFDEF WINDOWS}
  // Windows: microseconds * frequency / 1000000
  Result := (AMicroseconds * FFrequency) div 1000000;
  {$ELSE}
  // Unix/macOS: microseconds * 1000 = nanoseconds
  Result := AMicroseconds * 1000;
  {$ENDIF}
end;

class function TSedaiTiming.TicksToMilliseconds(ATicks: Int64): Int64;
begin
  if not FInitialized then Initialize;

  {$IFDEF WINDOWS}
  Result := (ATicks * 1000) div FFrequency;
  {$ELSE}
  // Unix/macOS: ticks are in nanoseconds
  Result := ATicks div 1000000;
  {$ENDIF}
end;

class function TSedaiTiming.MillisecondsToTicks(AMilliseconds: Int64): Int64;
begin
  if not FInitialized then Initialize;

  {$IFDEF WINDOWS}
  Result := (AMilliseconds * FFrequency) div 1000;
  {$ELSE}
  // Unix/macOS: milliseconds * 1000000 = nanoseconds
  Result := AMilliseconds * 1000000;
  {$ENDIF}
end;

class function TSedaiTiming.GetElapsedMicroseconds(AStartTicks: Int64): Int64;
begin
  Result := TicksToMicroseconds(GetTicks - AStartTicks);
end;

class function TSedaiTiming.GetElapsedMilliseconds(AStartTicks: Int64): Int64;
begin
  Result := TicksToMilliseconds(GetTicks - AStartTicks);
end;

class procedure TSedaiTiming.PreciseSleep(AMicroseconds: Int64);
var
  LStartTicks: Int64;
  LTargetTicks: Int64;
  LRemainingMicros: Int64;
begin
  if AMicroseconds <= 0 then Exit;
  if not FInitialized then Initialize;

  LStartTicks := GetTicks;
  LTargetTicks := LStartTicks + MicrosecondsToTicks(AMicroseconds);

  // For longer waits (>2ms), use OS sleep for most of the time
  // This saves CPU while maintaining precision for the final portion
  if AMicroseconds > 2000 then
  begin
    LRemainingMicros := AMicroseconds - 1500;  // Leave 1.5ms for busy-wait

    {$IFDEF WINDOWS}
    // Windows Sleep() has ~15ms resolution by default
    // With timeBeginPeriod(1), it can be 1ms
    Sleep(LRemainingMicros div 1000);
    {$ELSE}
    // Unix: usleep or nanosleep
    {$IFDEF UNIX}
    fpNanoSleep(LRemainingMicros * 1000);  // Convert to nanoseconds
    {$ELSE}
    Sleep(LRemainingMicros div 1000);
    {$ENDIF}
    {$ENDIF}
  end;

  // Busy-wait for the remaining time (precision portion)
  while GetTicks < LTargetTicks do
    ; // Spin
end;

class procedure TSedaiTiming.BusyWait(AMicroseconds: Int64);
var
  LTargetTicks: Int64;
begin
  if AMicroseconds <= 0 then Exit;
  if not FInitialized then Initialize;

  LTargetTicks := GetTicks + MicrosecondsToTicks(AMicroseconds);

  while GetTicks < LTargetTicks do
    ; // Spin (100% CPU)
end;

class function TSedaiTiming.GetTimeSeconds: Double;
begin
  if not FInitialized then Initialize;
  Result := GetTicks / FFrequency;
end;

// ============================================================================
// CONVENIENCE FUNCTIONS
// ============================================================================

function SedaiGetTicks: Int64;
begin
  Result := TSedaiTiming.GetTicks;
end;

function SedaiTicksToMicroseconds(ATicks: Int64): Int64;
begin
  Result := TSedaiTiming.TicksToMicroseconds(ATicks);
end;

function SedaiMicrosecondsToTicks(AMicroseconds: Int64): Int64;
begin
  Result := TSedaiTiming.MicrosecondsToTicks(AMicroseconds);
end;

function SedaiGetElapsedMicroseconds(AStartTicks: Int64): Int64;
begin
  Result := TSedaiTiming.GetElapsedMicroseconds(AStartTicks);
end;

procedure SedaiPreciseSleep(AMicroseconds: Int64);
begin
  TSedaiTiming.PreciseSleep(AMicroseconds);
end;

initialization
  TSedaiTiming.FInitialized := False;
  TSedaiTiming.FFrequency := 1000;

end.
