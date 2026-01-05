{*
 * Sedai Audio Foundation - Voice Manager
 *
 * TSedaiVoiceManager provides polyphonic voice allocation with
 * configurable voice stealing (oldest, quietest, priority-based),
 * mono mode with legato, and unison mode with detune spread.
 *
 * (c) 2024 Artiforge - Licensed under GPL-3.0
 *}
unit SedaiVoiceManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, SedaiAudioTypes, SedaiAudioObject, SedaiSignalNode,
  SedaiVoice;

const
  DEFAULT_MAX_VOICES = 16;
  MAX_UNISON_VOICES = 8;

type
  // Procedural type for voice configuration callback
  TVoiceConfigProc = procedure(AVoice: TSedaiVoice) of object;

  { TSedaiVoiceManager }
  // Polyphonic voice manager with stealing and unison modes
  TSedaiVoiceManager = class(TSedaiSignalNode)
  private
    // Voice pool
    FVoices: array of TSedaiVoice;
    FMaxVoices: Integer;
    FActiveVoiceCount: Integer;

    // Voice stealing
    FStealPolicy: TVoiceStealPolicy;
    FNoteAge: Cardinal;           // Global note age counter

    // Mono mode
    FMonoMode: Boolean;
    FLegatoMode: Boolean;
    FNoteStack: array of record
      Note: Byte;
      Velocity: Single;
    end;
    FNoteStackSize: Integer;

    // Unison mode
    FUnisonEnabled: Boolean;
    FUnisonVoices: Integer;       // Number of unison voices (1-8)
    FUnisonDetune: Single;        // Detune spread in cents
    FUnisonStereoSpread: Single;  // Stereo spread (0-1)

    // Glide/Portamento
    FGlideEnabled: Boolean;
    FGlideTime: Single;           // Portamento time in seconds
    FGlideAlways: Boolean;        // Glide between all notes (vs. legato only)

    // Output mixing
    FMixBuffer: array of Single;
    FMixBufferSize: Integer;

    function FindFreeVoice: Integer;
    function FindStealVoice: Integer;
    function FindVoiceByNote(ANote: Byte): Integer;
    function GetActiveVoiceCount: Integer;

    procedure AllocateMixBuffer(AFrameCount: Integer);
    procedure PushNote(ANote: Byte; AVelocity: Single);
    procedure RemoveNote(ANote: Byte);
    function PopNote(out ANote: Byte; out AVelocity: Single): Boolean;

    procedure SetMaxVoices(AValue: Integer);
    procedure SetUnisonVoices(AValue: Integer);
    procedure SetUnisonDetune(AValue: Single);
    procedure SetGlideTime(AValue: Single);

  protected
    procedure SampleRateChanged; override;

  public
    constructor Create; override;
    destructor Destroy; override;

    // Reset all voices
    procedure Reset; override;

    // ========================================================================
    // NOTE CONTROL
    // ========================================================================

    // Handle note on
    procedure NoteOn(ANote: Byte; AVelocity: Single);

    // Handle note off
    procedure NoteOff(ANote: Byte);

    // Handle all notes off
    procedure AllNotesOff;

    // Handle all sound off (immediate)
    procedure AllSoundOff;

    // ========================================================================
    // MIDI CONTROL
    // ========================================================================

    // Handle pitch bend
    procedure SetPitchBend(AValue: Single);

    // Handle mod wheel
    procedure SetModWheel(AValue: Single);

    // Handle sustain pedal
    procedure SetSustainPedal(AValue: Boolean);

    // ========================================================================
    // PROCESSING
    // ========================================================================

    // Process a block of audio
    procedure ProcessBlock(AInput, AOutput: PSingle; AFrameCount: Integer); override;

    // ========================================================================
    // VOICE ACCESS
    // ========================================================================

    // Get voice by index (for parameter setup)
    function GetVoice(AIndex: Integer): TSedaiVoice;

    // Configure all voices with same parameters
    procedure ConfigureAllVoices(AConfigProc: TVoiceConfigProc);

    // ========================================================================
    // PROPERTIES
    // ========================================================================

    // Voice pool
    property MaxVoices: Integer read FMaxVoices write SetMaxVoices;
    property ActiveVoiceCount: Integer read GetActiveVoiceCount;

    // Voice stealing
    property StealPolicy: TVoiceStealPolicy read FStealPolicy write FStealPolicy;

    // Mono mode
    property MonoMode: Boolean read FMonoMode write FMonoMode;
    property LegatoMode: Boolean read FLegatoMode write FLegatoMode;

    // Unison
    property UnisonEnabled: Boolean read FUnisonEnabled write FUnisonEnabled;
    property UnisonVoices: Integer read FUnisonVoices write SetUnisonVoices;
    property UnisonDetune: Single read FUnisonDetune write SetUnisonDetune;
    property UnisonStereoSpread: Single read FUnisonStereoSpread write FUnisonStereoSpread;

    // Glide
    property GlideEnabled: Boolean read FGlideEnabled write FGlideEnabled;
    property GlideTime: Single read FGlideTime write SetGlideTime;
    property GlideAlways: Boolean read FGlideAlways write FGlideAlways;
  end;

implementation

{ TSedaiVoiceManager }

constructor TSedaiVoiceManager.Create;
var
  I: Integer;
begin
  inherited Create;

  FMaxVoices := DEFAULT_MAX_VOICES;
  FActiveVoiceCount := 0;
  FNoteAge := 0;

  // Create voice pool
  SetLength(FVoices, FMaxVoices);
  for I := 0 to FMaxVoices - 1 do
    FVoices[I] := TSedaiVoice.Create;

  // Default settings
  FStealPolicy := vspOldest;
  FMonoMode := False;
  FLegatoMode := False;

  // Note stack for mono mode
  SetLength(FNoteStack, 16);
  FNoteStackSize := 0;

  // Unison
  FUnisonEnabled := False;
  FUnisonVoices := 1;
  FUnisonDetune := 10.0;  // 10 cents default
  FUnisonStereoSpread := 1.0;

  // Glide
  FGlideEnabled := False;
  FGlideTime := 0.1;
  FGlideAlways := False;

  // Mix buffer
  FMixBufferSize := 0;
end;

destructor TSedaiVoiceManager.Destroy;
var
  I: Integer;
begin
  for I := 0 to High(FVoices) do
    FVoices[I].Free;

  SetLength(FVoices, 0);
  SetLength(FNoteStack, 0);
  SetLength(FMixBuffer, 0);

  inherited Destroy;
end;

procedure TSedaiVoiceManager.Reset;
var
  I: Integer;
begin
  inherited Reset;

  for I := 0 to High(FVoices) do
    FVoices[I].Reset;

  FActiveVoiceCount := 0;
  FNoteStackSize := 0;
  FNoteAge := 0;
end;

procedure TSedaiVoiceManager.SampleRateChanged;
var
  I: Integer;
begin
  inherited;

  for I := 0 to High(FVoices) do
    FVoices[I].SetSampleRate(FSampleRate);
end;

procedure TSedaiVoiceManager.SetMaxVoices(AValue: Integer);
var
  I, OldMax: Integer;
begin
  if AValue < 1 then
    AValue := 1
  else if AValue > 64 then
    AValue := 64;

  if AValue = FMaxVoices then
    Exit;

  OldMax := FMaxVoices;
  FMaxVoices := AValue;

  // Resize voice pool
  if AValue > OldMax then
  begin
    SetLength(FVoices, AValue);
    for I := OldMax to AValue - 1 do
    begin
      FVoices[I] := TSedaiVoice.Create;
      FVoices[I].SetSampleRate(FSampleRate);
    end;
  end
  else
  begin
    for I := AValue to OldMax - 1 do
      FVoices[I].Free;
    SetLength(FVoices, AValue);
  end;
end;

procedure TSedaiVoiceManager.SetUnisonVoices(AValue: Integer);
begin
  if AValue < 1 then
    AValue := 1
  else if AValue > MAX_UNISON_VOICES then
    AValue := MAX_UNISON_VOICES;

  FUnisonVoices := AValue;
end;

procedure TSedaiVoiceManager.SetUnisonDetune(AValue: Single);
begin
  if AValue < 0.0 then
    AValue := 0.0
  else if AValue > 100.0 then
    AValue := 100.0;

  FUnisonDetune := AValue;
end;

procedure TSedaiVoiceManager.SetGlideTime(AValue: Single);
var
  I: Integer;
begin
  if AValue < 0.0 then
    AValue := 0.0
  else if AValue > 10.0 then
    AValue := 10.0;

  FGlideTime := AValue;

  // Update all voices
  for I := 0 to High(FVoices) do
    FVoices[I].GlideTime := AValue;
end;

function TSedaiVoiceManager.FindFreeVoice: Integer;
var
  I: Integer;
begin
  Result := -1;

  for I := 0 to High(FVoices) do
  begin
    if not FVoices[I].IsActive then
    begin
      Result := I;
      Exit;
    end;
  end;
end;

function TSedaiVoiceManager.FindStealVoice: Integer;
var
  I: Integer;
  OldestAge: Cardinal;
  QuietestLevel: Single;
  LowestPriority: Integer;
  BestVoice: Integer;
begin
  Result := -1;
  BestVoice := 0;

  case FStealPolicy of
    vspOldest:
      begin
        OldestAge := 0;
        for I := 0 to High(FVoices) do
        begin
          if FVoices[I].CanSteal and (FVoices[I].Age > OldestAge) then
          begin
            OldestAge := FVoices[I].Age;
            BestVoice := I;
          end;
        end;
        Result := BestVoice;
      end;

    vspQuietest:
      begin
        QuietestLevel := 2.0;  // Above max
        for I := 0 to High(FVoices) do
        begin
          if FVoices[I].CanSteal and (FVoices[I].GetCurrentLevel < QuietestLevel) then
          begin
            QuietestLevel := FVoices[I].GetCurrentLevel;
            BestVoice := I;
          end;
        end;
        Result := BestVoice;
      end;

    vspLowestPriority:
      begin
        LowestPriority := MaxInt;
        for I := 0 to High(FVoices) do
        begin
          if FVoices[I].CanSteal and (FVoices[I].StealPriority < LowestPriority) then
          begin
            LowestPriority := FVoices[I].StealPriority;
            BestVoice := I;
          end;
        end;
        Result := BestVoice;
      end;

    vspNone:
      Result := -1;  // No stealing
  end;
end;

function TSedaiVoiceManager.FindVoiceByNote(ANote: Byte): Integer;
var
  I: Integer;
begin
  Result := -1;

  for I := 0 to High(FVoices) do
  begin
    if FVoices[I].IsActive and (FVoices[I].Note = ANote) then
    begin
      Result := I;
      Exit;
    end;
  end;
end;

function TSedaiVoiceManager.GetActiveVoiceCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to High(FVoices) do
  begin
    if FVoices[I].IsActive then
      Inc(Result);
  end;
end;

procedure TSedaiVoiceManager.AllocateMixBuffer(AFrameCount: Integer);
var
  RequiredSize: Integer;
begin
  RequiredSize := AFrameCount * 2;  // Stereo
  if RequiredSize > FMixBufferSize then
  begin
    SetLength(FMixBuffer, RequiredSize);
    FMixBufferSize := RequiredSize;
  end;
end;

procedure TSedaiVoiceManager.PushNote(ANote: Byte; AVelocity: Single);
begin
  if FNoteStackSize < Length(FNoteStack) then
  begin
    FNoteStack[FNoteStackSize].Note := ANote;
    FNoteStack[FNoteStackSize].Velocity := AVelocity;
    Inc(FNoteStackSize);
  end;
end;

procedure TSedaiVoiceManager.RemoveNote(ANote: Byte);
var
  I, J: Integer;
begin
  for I := 0 to FNoteStackSize - 1 do
  begin
    if FNoteStack[I].Note = ANote then
    begin
      // Shift remaining notes
      for J := I to FNoteStackSize - 2 do
        FNoteStack[J] := FNoteStack[J + 1];
      Dec(FNoteStackSize);
      Exit;
    end;
  end;
end;

function TSedaiVoiceManager.PopNote(out ANote: Byte; out AVelocity: Single): Boolean;
begin
  if FNoteStackSize > 0 then
  begin
    Dec(FNoteStackSize);
    ANote := FNoteStack[FNoteStackSize].Note;
    AVelocity := FNoteStack[FNoteStackSize].Velocity;
    Result := True;
  end
  else
    Result := False;
end;

procedure TSedaiVoiceManager.NoteOn(ANote: Byte; AVelocity: Single);
var
  VoiceIndex: Integer;
  I, VoicesToUse: Integer;
  DetuneStep, Detune: Single;
  PanStep, Pan: Single;
  IsLegato: Boolean;
begin
  Inc(FNoteAge);

  if FMonoMode then
  begin
    // Mono mode: use single voice with note stack
    IsLegato := FLegatoMode and (FNoteStackSize > 0);
    PushNote(ANote, AVelocity);

    VoiceIndex := 0;
    if IsLegato then
    begin
      // Legato: just change note, don't retrigger
      FVoices[VoiceIndex].Note := ANote;
      FVoices[VoiceIndex].Velocity := AVelocity;
    end
    else
    begin
      // Retrigger
      if FGlideEnabled and FGlideAlways then
        FVoices[VoiceIndex].GlideTime := FGlideTime
      else if FGlideEnabled and (FNoteStackSize > 1) then
        FVoices[VoiceIndex].GlideTime := FGlideTime
      else
        FVoices[VoiceIndex].GlideTime := 0;

      FVoices[VoiceIndex].NoteOn(ANote, AVelocity);
    end;
  end
  else if FUnisonEnabled and (FUnisonVoices > 1) then
  begin
    // Unison mode: spread notes across multiple voices
    VoicesToUse := FUnisonVoices;
    DetuneStep := FUnisonDetune / (VoicesToUse - 1);
    PanStep := FUnisonStereoSpread * 2.0 / (VoicesToUse - 1);

    for I := 0 to VoicesToUse - 1 do
    begin
      VoiceIndex := FindFreeVoice;
      if VoiceIndex < 0 then
        VoiceIndex := FindStealVoice;

      if VoiceIndex >= 0 then
      begin
        // Calculate detune for this unison voice
        if VoicesToUse > 1 then
          Detune := -FUnisonDetune / 2.0 + I * DetuneStep
        else
          Detune := 0;

        // Calculate pan
        if VoicesToUse > 1 then
          Pan := -FUnisonStereoSpread + I * PanStep
        else
          Pan := 0;

        FVoices[VoiceIndex].SetOscillatorDetune(0, Detune);
        FVoices[VoiceIndex].Pan := Pan;
        FVoices[VoiceIndex].StealPriority := I;  // First voice highest priority
        FVoices[VoiceIndex].NoteOn(ANote, AVelocity);
      end;
    end;
  end
  else
  begin
    // Normal polyphonic mode
    VoiceIndex := FindFreeVoice;
    if VoiceIndex < 0 then
      VoiceIndex := FindStealVoice;

    if VoiceIndex >= 0 then
    begin
      if FGlideEnabled and FGlideAlways then
        FVoices[VoiceIndex].GlideTime := FGlideTime
      else
        FVoices[VoiceIndex].GlideTime := 0;

      FVoices[VoiceIndex].NoteOn(ANote, AVelocity);
    end;
  end;
end;

procedure TSedaiVoiceManager.NoteOff(ANote: Byte);
var
  I: Integer;
  PrevNote: Byte;
  PrevVelocity: Single;
begin
  if FMonoMode then
  begin
    // Remove note from stack
    RemoveNote(ANote);

    // Check if we should switch to previous note (legato)
    if FLegatoMode and (FNoteStackSize > 0) then
    begin
      PrevNote := FNoteStack[FNoteStackSize - 1].Note;
      PrevVelocity := FNoteStack[FNoteStackSize - 1].Velocity;

      // Glide to previous note
      if FGlideEnabled then
        FVoices[0].GlideTime := FGlideTime;

      FVoices[0].Note := PrevNote;
    end
    else if FNoteStackSize = 0 then
    begin
      // No more notes, release voice
      FVoices[0].NoteOff;
    end;
  end
  else
  begin
    // Release all voices playing this note
    for I := 0 to High(FVoices) do
    begin
      if FVoices[I].IsActive and (FVoices[I].Note = ANote) then
        FVoices[I].NoteOff;
    end;
  end;
end;

procedure TSedaiVoiceManager.AllNotesOff;
var
  I: Integer;
begin
  for I := 0 to High(FVoices) do
  begin
    if FVoices[I].IsActive then
      FVoices[I].NoteOff;
  end;

  FNoteStackSize := 0;
end;

procedure TSedaiVoiceManager.AllSoundOff;
var
  I: Integer;
begin
  for I := 0 to High(FVoices) do
    FVoices[I].Kill;

  FNoteStackSize := 0;
end;

procedure TSedaiVoiceManager.SetPitchBend(AValue: Single);
var
  I: Integer;
begin
  for I := 0 to High(FVoices) do
    FVoices[I].PitchBend := AValue;
end;

procedure TSedaiVoiceManager.SetModWheel(AValue: Single);
begin
  // Modwheel typically controls LFO depth or filter
  // Implementation depends on synth design
end;

procedure TSedaiVoiceManager.SetSustainPedal(AValue: Boolean);
begin
  // Sustain pedal handling
  // When released, send note off to sustained notes
end;

procedure TSedaiVoiceManager.ProcessBlock(AInput, AOutput: PSingle; AFrameCount: Integer);
var
  I, J: Integer;
  SampleCount: Integer;
begin
  SampleCount := AFrameCount * 2;  // Stereo

  // Clear output buffer
  for I := 0 to SampleCount - 1 do
    AOutput[I] := 0.0;

  // Allocate mix buffer
  AllocateMixBuffer(AFrameCount);

  // Process each active voice and mix
  for I := 0 to High(FVoices) do
  begin
    if FVoices[I].IsActive then
    begin
      // Clear mix buffer
      for J := 0 to SampleCount - 1 do
        FMixBuffer[J] := 0.0;

      // Process voice into mix buffer
      FVoices[I].ProcessBlock(nil, @FMixBuffer[0], AFrameCount);

      // Add to output
      for J := 0 to SampleCount - 1 do
        AOutput[J] := AOutput[J] + FMixBuffer[J];
    end;
  end;

  // Simple limiting to prevent clipping
  for I := 0 to SampleCount - 1 do
  begin
    if AOutput[I] > 1.0 then
      AOutput[I] := 1.0
    else if AOutput[I] < -1.0 then
      AOutput[I] := -1.0;
  end;
end;

function TSedaiVoiceManager.GetVoice(AIndex: Integer): TSedaiVoice;
begin
  if (AIndex >= 0) and (AIndex <= High(FVoices)) then
    Result := FVoices[AIndex]
  else
    Result := nil;
end;

procedure TSedaiVoiceManager.ConfigureAllVoices(AConfigProc: TVoiceConfigProc);
var
  I: Integer;
begin
  for I := 0 to High(FVoices) do
    AConfigProc(FVoices[I]);
end;

end.
