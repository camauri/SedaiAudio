{*
 * Sedai Audio Foundation - Engine (Part/Instrument host)
 *
 * TSAFEngine is the multitimbral host of the Part/Instrument architecture:
 *
 *   TSAFEngine
 *    +- Part = instrument -> VoiceManager(universal voices) -> Mixer channel
 *    +- Part ...                                                     |
 *    +- ...                                                    MasterBus -> out
 *
 * Each Part renders to its own stereo-interleaved buffer; the engine feeds those
 * buffers, one per mixer channel, into a TSedaiMixer whose master bus produces
 * the final stereo output (with the master limiter).
 *
 * Fixed capacity: the part table and the scratch-buffer/pointer arrays are sized
 * once (MAX_ENGINE_PARTS) at construction, so AddPart never reallocates them.
 * That keeps the audio-thread RenderBlock free of races against a concurrent
 * AddPart on the control thread (on x86-64, the single FPartCount increment
 * after the slot write is safely ordered).
 *
 * (c) 2025 Artiforge - Licensed under GPL-3.0
 *}
unit SedaiEngine;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SedaiAudioTypes,
  SedaiMixer, SedaiMixerChannel, SedaiBus,
  SedaiVoiceManager, SedaiPart;

const
  // One mixer channel per part; the mixer supports up to MAX_MIXER_CHANNELS.
  MAX_ENGINE_PARTS = 64;
  // Per-part scratch buffer is pre-sized (control thread) to this many frames so
  // the audio thread never reallocates in RenderBlock for normal block sizes.
  ENGINE_PREALLOC_FRAMES = 8192;

type
  { TSAFEngine }
  TSAFEngine = class
  private
    FParts: array of TSAFPart;     // length MAX_ENGINE_PARTS; first FPartCount used
    FPartCount: Integer;
    FMixer: TSedaiMixer;
    FSampleRate: Cardinal;

    // Per-part stereo scratch buffers + the pointer array handed to the mixer.
    FPartBuffers: array of array of Single;   // outer length MAX_ENGINE_PARTS
    FInputs: array of PSingle;                // length MAX_ENGINE_PARTS

    procedure EnsureBuffers(AFrameCount: Integer);

  public
    constructor Create(ASampleRate: Cardinal = SEDAI_DEFAULT_SAMPLE_RATE);
    destructor Destroy; override;

    procedure SetSampleRate(AValue: Cardinal);

    // Add a Part. A matching mixer channel is created in the same slot index
    // (part i <-> channel i), routed straight to the master bus. Returns nil if
    // the engine is at capacity.
    function AddPart(const AName: string = '';
      AMaxVoices: Integer = DEFAULT_MAX_VOICES): TSAFPart;

    function PartCount: Integer;
    function GetPart(AIndex: Integer): TSAFPart;
    function GetChannel(APartIndex: Integer): TSedaiMixerChannel;
    function GetMasterBus: TSedaiMasterBus;

    // Render the full mix (all parts -> mixer -> master) into AOutput,
    // stereo interleaved L/R.
    procedure RenderBlock(AOutput: PSingle; AFrameCount: Integer);

    // Master volume convenience (dB; the master bus is the authority).
    procedure SetMasterVolumeDb(ADb: Single);

    // Live total of active voices across all parts. Each part is sized
    // independently (TSAFPart.Polyphony); the engine's total polyphony is just
    // the sum of the parts' pools, bounded only by the hardware.
    function TotalActiveVoices: Integer;

    property SampleRate: Cardinal read FSampleRate;
    property Mixer: TSedaiMixer read FMixer;
  end;

implementation

constructor TSAFEngine.Create(ASampleRate: Cardinal);
begin
  inherited Create;

  FMixer := TSedaiMixer.Create;
  // Set the mixer sample rate up front so channels added later inherit it.
  FMixer.SetSampleRate(ASampleRate);
  FSampleRate := ASampleRate;

  // Fixed-capacity tables (no reallocation during playback).
  SetLength(FParts, MAX_ENGINE_PARTS);
  SetLength(FPartBuffers, MAX_ENGINE_PARTS);
  SetLength(FInputs, MAX_ENGINE_PARTS);
  FPartCount := 0;
end;

destructor TSAFEngine.Destroy;
var
  I: Integer;
begin
  for I := 0 to FPartCount - 1 do
    FParts[I].Free;
  SetLength(FParts, 0);

  FMixer.Free;
  inherited Destroy;
end;

procedure TSAFEngine.SetSampleRate(AValue: Cardinal);
var
  I: Integer;
begin
  FSampleRate := AValue;
  FMixer.SetSampleRate(AValue);  // propagates to channels + master bus
  for I := 0 to FPartCount - 1 do
    FParts[I].SetSampleRate(AValue);
end;

function TSAFEngine.AddPart(const AName: string; AMaxVoices: Integer): TSAFPart;
var
  Part: TSAFPart;
  Ch: TSedaiMixerChannel;
  Idx: Integer;
begin
  Result := nil;
  if FPartCount >= MAX_ENGINE_PARTS then Exit;

  Part := TSAFPart.Create(AMaxVoices);
  Part.SetSampleRate(FSampleRate);
  if AName <> '' then
    Part.Name := AName;

  Idx := FPartCount;
  FParts[Idx] := Part;

  // Pre-size this part's scratch buffer here (control thread) so RenderBlock on
  // the audio thread never has to allocate for normal block sizes.
  SetLength(FPartBuffers[Idx], ENGINE_PREALLOC_FRAMES * 2);

  // One mixer channel per part, in the same slot index (AddChannel fills
  // sequentially), routed to master by default (OutputBus = -1).
  Ch := FMixer.AddChannel(Part.Name);
  if Ch <> nil then
    Ch.ChannelType := ctInstrument;

  // Publish the new part last: the audio thread reads FPartCount to bound its
  // loop, so the slot write above is visible before the count increments.
  Inc(FPartCount);
  Result := Part;
end;

function TSAFEngine.PartCount: Integer;
begin
  Result := FPartCount;
end;

function TSAFEngine.GetPart(AIndex: Integer): TSAFPart;
begin
  if (AIndex >= 0) and (AIndex < FPartCount) then
    Result := FParts[AIndex]
  else
    Result := nil;
end;

function TSAFEngine.GetChannel(APartIndex: Integer): TSedaiMixerChannel;
begin
  // Part i maps to mixer channel slot i.
  Result := FMixer.GetChannel(APartIndex);
end;

function TSAFEngine.GetMasterBus: TSedaiMasterBus;
begin
  Result := FMixer.GetMasterBus;
end;

procedure TSAFEngine.EnsureBuffers(AFrameCount: Integer);
var
  I, Need: Integer;
begin
  Need := AFrameCount * 2;  // stereo interleaved
  for I := 0 to FPartCount - 1 do
    if Length(FPartBuffers[I]) < Need then
      SetLength(FPartBuffers[I], Need);
end;

procedure TSAFEngine.RenderBlock(AOutput: PSingle; AFrameCount: Integer);
var
  I, Count: Integer;
begin
  if AFrameCount <= 0 then Exit;

  Count := FPartCount;  // snapshot once for this block
  EnsureBuffers(AFrameCount);

  // Render each part into its own buffer and collect the pointers fresh each
  // block (managed-array storage may have moved between calls).
  for I := 0 to Count - 1 do
  begin
    FParts[I].RenderBlock(@FPartBuffers[I][0], AFrameCount);
    FInputs[I] := @FPartBuffers[I][0];
  end;

  FMixer.ProcessBlock(FInputs, AOutput, AFrameCount);
end;

procedure TSAFEngine.SetMasterVolumeDb(ADb: Single);
begin
  FMixer.GetMasterBus.Volume := ADb;
end;

function TSAFEngine.TotalActiveVoices: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to FPartCount - 1 do
    Inc(Result, FParts[I].ActiveVoiceCount);
end;

end.
