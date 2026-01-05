{*
 * Sedai Audio Foundation - Mixer Channel
 *
 * TSedaiMixerChannel provides a complete mixing console channel
 * with gain, pan, mute/solo, insert effects, and aux sends.
 *
 * (c) 2024 Artiforge - Licensed under GPL-3.0
 *}
unit SedaiMixerChannel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, SedaiAudioTypes, SedaiSignalNode, SedaiFilter, SedaiEffect;

const
  MAX_INSERTS = 8;
  MAX_AUX_SENDS = 8;

type
  // Channel type
  TChannelType = (
    ctAudio,              // Standard audio channel
    ctInstrument,         // Instrument/VSTi channel
    ctGroup,              // Group/bus channel
    ctAux,                // Aux return channel
    ctMaster              // Master output channel
  );

  // Metering mode
  TMeteringMode = (
    mmPeak,               // Peak metering
    mmRMS,                // RMS metering
    mmVU,                 // VU-style (slower)
    mmPPM                 // PPM broadcast standard
  );

  // Aux send configuration
  TAuxSend = record
    Level: Single;        // Send level (0-1)
    PreFader: Boolean;    // Pre/post fader
    Enabled: Boolean;
    Pan: Single;          // Send pan (-1 to +1)
  end;

  // Forward declaration
  TSedaiMixerChannel = class;

  { TSedaiMixerChannel }
  // Complete mixing console channel
  TSedaiMixerChannel = class(TSedaiSignalProcessor)
  private
    FChannelType: TChannelType;
    FName: string;
    FIndex: Integer;

    // Gain staging
    FInputGain: Single;           // Pre-insert gain (dB)
    FVolume: Single;              // Fader volume (dB)
    FPan: Single;                 // -1 (L) to +1 (R)

    // Mute/Solo
    FMuted: Boolean;
    FSoloed: Boolean;
    FSoloSafe: Boolean;           // Ignore solo on other channels
    FSoloActive: Boolean;         // External solo affects this channel

    // Phase
    FPhaseInvert: array[0..1] of Boolean;

    // Insert effects
    FInserts: array[0..MAX_INSERTS-1] of TSedaiEffect;
    FInsertBypass: array[0..MAX_INSERTS-1] of Boolean;
    FInsertCount: Integer;

    // Aux sends
    FAuxSends: array[0..MAX_AUX_SENDS-1] of TAuxSend;

    // Metering
    FMeteringMode: TMeteringMode;
    FPeakL, FPeakR: Single;
    FRMSL, FRMSRL: Single;
    FRMSAccumL, FRMSAccumR: Single;
    FRMSSamples: Integer;
    FMeterDecay: Single;
    FClippedL, FClippedR: Boolean;

    // Output routing
    FOutputBus: Integer;          // Index of destination bus (-1 = master)

    // Smoothing
    FVolumeSmooth: Single;
    FPanSmooth: Single;
    FSmoothCoeff: Single;

    // Internal
    FTempBuffer: array of Single;

    procedure SetVolume(AValue: Single);
    procedure SetPan(AValue: Single);
    procedure SetInputGain(AValue: Single);
    procedure UpdateSmoothing;
    procedure UpdateMeters(ALeft, ARight: Single);

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Reset; override;
    procedure SampleRateChanged; override;

    // Insert effect management
    function AddInsert(AEffect: TSedaiEffect): Integer;
    procedure RemoveInsert(AIndex: Integer);
    procedure ClearInserts;
    procedure SetInsertBypass(AIndex: Integer; ABypass: Boolean);
    function GetInsert(AIndex: Integer): TSedaiEffect;

    // Aux send management
    procedure SetAuxSend(AAuxIndex: Integer; ALevel: Single; APreFader: Boolean = False);
    procedure SetAuxSendPan(AAuxIndex: Integer; APan: Single);
    procedure SetAuxSendEnabled(AAuxIndex: Integer; AEnabled: Boolean);
    function GetAuxSend(AAuxIndex: Integer): TAuxSend;

    // Get aux send output (call after ProcessBlock)
    procedure GetAuxSendOutput(AAuxIndex: Integer; AOutput: PSingle;
                                AFrameCount: Integer; const AInput: PSingle);

    // Process audio
    procedure ProcessBlock(AInput, AOutput: PSingle; AFrameCount: Integer); override;

    // Metering
    procedure ResetMeters;
    procedure GetMeterLevels(out ALeft, ARight: Single);
    function IsClipping: Boolean;

    // Properties - Configuration
    property ChannelType: TChannelType read FChannelType write FChannelType;
    property Name: string read FName write FName;
    property Index: Integer read FIndex write FIndex;
    property OutputBus: Integer read FOutputBus write FOutputBus;

    // Properties - Gain staging
    property InputGain: Single read FInputGain write SetInputGain;
    property Volume: Single read FVolume write SetVolume;
    property Pan: Single read FPan write SetPan;

    // Properties - Mute/Solo
    property Muted: Boolean read FMuted write FMuted;
    property Soloed: Boolean read FSoloed write FSoloed;
    property SoloSafe: Boolean read FSoloSafe write FSoloSafe;
    property SoloActive: Boolean read FSoloActive write FSoloActive;

    // Properties - Phase
    property PhaseInvertL: Boolean read FPhaseInvert[0] write FPhaseInvert[0];
    property PhaseInvertR: Boolean read FPhaseInvert[1] write FPhaseInvert[1];

    // Properties - Metering
    property MeteringMode: TMeteringMode read FMeteringMode write FMeteringMode;
    property PeakL: Single read FPeakL;
    property PeakR: Single read FPeakR;

    // Properties - Insert count
    property InsertCount: Integer read FInsertCount;
  end;

implementation

{ TSedaiMixerChannel }

constructor TSedaiMixerChannel.Create;
var
  I: Integer;
begin
  inherited Create;

  FChannelType := ctAudio;
  FName := 'Channel';
  FIndex := 0;

  FInputGain := 0.0;    // 0 dB
  FVolume := 0.0;       // 0 dB
  FPan := 0.0;          // Center

  FMuted := False;
  FSoloed := False;
  FSoloSafe := False;
  FSoloActive := False;

  FPhaseInvert[0] := False;
  FPhaseInvert[1] := False;

  FInsertCount := 0;
  for I := 0 to MAX_INSERTS - 1 do
  begin
    FInserts[I] := nil;
    FInsertBypass[I] := False;
  end;

  for I := 0 to MAX_AUX_SENDS - 1 do
  begin
    FAuxSends[I].Level := 0.0;
    FAuxSends[I].PreFader := False;
    FAuxSends[I].Enabled := False;
    FAuxSends[I].Pan := 0.0;
  end;

  FMeteringMode := mmPeak;
  FPeakL := 0.0;
  FPeakR := 0.0;
  FRMSL := 0.0;
  FRMSRL := 0.0;
  FRMSAccumL := 0.0;
  FRMSAccumR := 0.0;
  FRMSSamples := 0;
  FMeterDecay := 0.9995;
  FClippedL := False;
  FClippedR := False;

  FOutputBus := -1;     // Master

  FVolumeSmooth := 1.0;
  FPanSmooth := 0.0;
  FSmoothCoeff := 0.999;
end;

destructor TSedaiMixerChannel.Destroy;
begin
  ClearInserts;
  SetLength(FTempBuffer, 0);
  inherited Destroy;
end;

procedure TSedaiMixerChannel.Reset;
begin
  inherited Reset;

  FVolumeSmooth := DecibelToLinear(FVolume);
  FPanSmooth := FPan;
  ResetMeters;
end;

procedure TSedaiMixerChannel.SampleRateChanged;
var
  I: Integer;
  Rate: Cardinal;
begin
  inherited SampleRateChanged;

  Rate := SampleRate;

  // Update insert sample rates
  for I := 0 to MAX_INSERTS - 1 do
    if Assigned(FInserts[I]) then
      FInserts[I].SetSampleRate(Rate);

  // Calculate smoothing coefficient for ~10ms response
  if Rate > 0 then
    FSmoothCoeff := Exp(-1.0 / (0.01 * Rate))
  else
    FSmoothCoeff := 0.999;

  // Calculate meter decay (~300ms for peaks)
  if Rate > 0 then
    FMeterDecay := Exp(-1.0 / (0.3 * Rate))
  else
    FMeterDecay := 0.9995;
end;

procedure TSedaiMixerChannel.SetVolume(AValue: Single);
begin
  if AValue < -96.0 then AValue := -96.0;
  if AValue > 12.0 then AValue := 12.0;
  FVolume := AValue;
end;

procedure TSedaiMixerChannel.SetPan(AValue: Single);
begin
  if AValue < -1.0 then AValue := -1.0;
  if AValue > 1.0 then AValue := 1.0;
  FPan := AValue;
end;

procedure TSedaiMixerChannel.SetInputGain(AValue: Single);
begin
  if AValue < -24.0 then AValue := -24.0;
  if AValue > 24.0 then AValue := 24.0;
  FInputGain := AValue;
end;

procedure TSedaiMixerChannel.UpdateSmoothing;
begin
  // Smooth volume changes
  FVolumeSmooth := FSmoothCoeff * FVolumeSmooth +
                   (1.0 - FSmoothCoeff) * DecibelToLinear(FVolume);

  // Smooth pan changes
  FPanSmooth := FSmoothCoeff * FPanSmooth +
                (1.0 - FSmoothCoeff) * FPan;
end;

procedure TSedaiMixerChannel.UpdateMeters(ALeft, ARight: Single);
var
  AbsL, AbsR: Single;
begin
  AbsL := Abs(ALeft);
  AbsR := Abs(ARight);

  case FMeteringMode of
    mmPeak:
      begin
        // Fast attack, slow decay
        if AbsL > FPeakL then
          FPeakL := AbsL
        else
          FPeakL := FPeakL * FMeterDecay;

        if AbsR > FPeakR then
          FPeakR := AbsR
        else
          FPeakR := FPeakR * FMeterDecay;
      end;

    mmRMS:
      begin
        // Accumulate for RMS calculation
        FRMSAccumL := FRMSAccumL + AbsL * AbsL;
        FRMSAccumR := FRMSAccumR + AbsR * AbsR;
        Inc(FRMSSamples);

        // Update RMS every ~50ms
        if FRMSSamples >= Round(FSampleRate * 0.05) then
        begin
          FRMSL := Sqrt(FRMSAccumL / FRMSSamples);
          FRMSRL := Sqrt(FRMSAccumR / FRMSSamples);
          FPeakL := FRMSL;
          FPeakR := FRMSRL;
          FRMSAccumL := 0.0;
          FRMSAccumR := 0.0;
          FRMSSamples := 0;
        end;
      end;

    mmVU, mmPPM:
      begin
        // Slower response for VU/PPM
        FPeakL := FPeakL * 0.9997 + AbsL * 0.0003;
        FPeakR := FPeakR * 0.9997 + AbsR * 0.0003;
      end;
  end;

  // Clip detection
  if AbsL > 1.0 then FClippedL := True;
  if AbsR > 1.0 then FClippedR := True;
end;

function TSedaiMixerChannel.AddInsert(AEffect: TSedaiEffect): Integer;
var
  I: Integer;
begin
  Result := -1;

  // Find first empty slot
  for I := 0 to MAX_INSERTS - 1 do
  begin
    if FInserts[I] = nil then
    begin
      FInserts[I] := AEffect;
      FInserts[I].SetSampleRate(FSampleRate);
      FInsertBypass[I] := False;
      Inc(FInsertCount);
      Result := I;
      Exit;
    end;
  end;
end;

procedure TSedaiMixerChannel.RemoveInsert(AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= MAX_INSERTS) then Exit;
  if FInserts[AIndex] = nil then Exit;

  FInserts[AIndex] := nil;
  FInsertBypass[AIndex] := False;
  Dec(FInsertCount);
end;

procedure TSedaiMixerChannel.ClearInserts;
var
  I: Integer;
begin
  for I := 0 to MAX_INSERTS - 1 do
  begin
    FInserts[I] := nil;
    FInsertBypass[I] := False;
  end;
  FInsertCount := 0;
end;

procedure TSedaiMixerChannel.SetInsertBypass(AIndex: Integer; ABypass: Boolean);
begin
  if (AIndex >= 0) and (AIndex < MAX_INSERTS) then
    FInsertBypass[AIndex] := ABypass;
end;

function TSedaiMixerChannel.GetInsert(AIndex: Integer): TSedaiEffect;
begin
  if (AIndex >= 0) and (AIndex < MAX_INSERTS) then
    Result := FInserts[AIndex]
  else
    Result := nil;
end;

procedure TSedaiMixerChannel.SetAuxSend(AAuxIndex: Integer; ALevel: Single; APreFader: Boolean);
begin
  if (AAuxIndex < 0) or (AAuxIndex >= MAX_AUX_SENDS) then Exit;

  if ALevel < 0 then ALevel := 0;
  if ALevel > 1 then ALevel := 1;

  FAuxSends[AAuxIndex].Level := ALevel;
  FAuxSends[AAuxIndex].PreFader := APreFader;
  FAuxSends[AAuxIndex].Enabled := ALevel > 0;
end;

procedure TSedaiMixerChannel.SetAuxSendPan(AAuxIndex: Integer; APan: Single);
begin
  if (AAuxIndex < 0) or (AAuxIndex >= MAX_AUX_SENDS) then Exit;

  if APan < -1 then APan := -1;
  if APan > 1 then APan := 1;

  FAuxSends[AAuxIndex].Pan := APan;
end;

procedure TSedaiMixerChannel.SetAuxSendEnabled(AAuxIndex: Integer; AEnabled: Boolean);
begin
  if (AAuxIndex >= 0) and (AAuxIndex < MAX_AUX_SENDS) then
    FAuxSends[AAuxIndex].Enabled := AEnabled;
end;

function TSedaiMixerChannel.GetAuxSend(AAuxIndex: Integer): TAuxSend;
begin
  if (AAuxIndex >= 0) and (AAuxIndex < MAX_AUX_SENDS) then
    Result := FAuxSends[AAuxIndex]
  else
  begin
    Result.Level := 0;
    Result.PreFader := False;
    Result.Enabled := False;
    Result.Pan := 0;
  end;
end;

procedure TSedaiMixerChannel.GetAuxSendOutput(AAuxIndex: Integer; AOutput: PSingle;
  AFrameCount: Integer; const AInput: PSingle);
var
  I: Integer;
  Level, PanL, PanR: Single;
  Left, Right: Single;
begin
  if (AAuxIndex < 0) or (AAuxIndex >= MAX_AUX_SENDS) then Exit;
  if not FAuxSends[AAuxIndex].Enabled then Exit;

  Level := FAuxSends[AAuxIndex].Level;
  if Level <= 0 then Exit;

  // Calculate pan
  PanL := Cos((FAuxSends[AAuxIndex].Pan + 1.0) * PI * 0.25);
  PanR := Sin((FAuxSends[AAuxIndex].Pan + 1.0) * PI * 0.25);

  for I := 0 to AFrameCount - 1 do
  begin
    Left := AInput[I * 2] * Level;
    Right := AInput[I * 2 + 1] * Level;

    AOutput[I * 2] := AOutput[I * 2] + Left * PanL;
    AOutput[I * 2 + 1] := AOutput[I * 2 + 1] + Right * PanR;
  end;
end;

procedure TSedaiMixerChannel.ProcessBlock(AInput, AOutput: PSingle; AFrameCount: Integer);
var
  I, J: Integer;
  Left, Right: Single;
  InputGainLin: Single;
  PanL, PanR: Single;
begin
  // Ensure temp buffer
  if Length(FTempBuffer) < AFrameCount * 2 then
    SetLength(FTempBuffer, AFrameCount * 2);

  // Calculate input gain
  InputGainLin := DecibelToLinear(FInputGain);

  // Copy input with gain and phase
  for I := 0 to AFrameCount - 1 do
  begin
    Left := AInput[I * 2] * InputGainLin;
    Right := AInput[I * 2 + 1] * InputGainLin;

    // Phase invert
    if FPhaseInvert[0] then Left := -Left;
    if FPhaseInvert[1] then Right := -Right;

    FTempBuffer[I * 2] := Left;
    FTempBuffer[I * 2 + 1] := Right;
  end;

  // Process inserts
  for J := 0 to MAX_INSERTS - 1 do
  begin
    if Assigned(FInserts[J]) and not FInsertBypass[J] then
    begin
      // Copy to output, process, copy back
      for I := 0 to AFrameCount * 2 - 1 do
        AOutput[I] := FTempBuffer[I];

      FInserts[J].ProcessBlock(@FTempBuffer[0], AOutput, AFrameCount);

      for I := 0 to AFrameCount * 2 - 1 do
        FTempBuffer[I] := AOutput[I];
    end;
  end;

  // Apply fader, pan, mute
  for I := 0 to AFrameCount - 1 do
  begin
    UpdateSmoothing;

    Left := FTempBuffer[I * 2];
    Right := FTempBuffer[I * 2 + 1];

    // Check mute
    if FMuted or FSoloActive then
    begin
      AOutput[I * 2] := 0.0;
      AOutput[I * 2 + 1] := 0.0;
      Continue;
    end;

    // Apply volume
    Left := Left * FVolumeSmooth;
    Right := Right * FVolumeSmooth;

    // Apply pan (constant power)
    PanL := Cos((FPanSmooth + 1.0) * PI * 0.25);
    PanR := Sin((FPanSmooth + 1.0) * PI * 0.25);

    Left := Left * PanL;
    Right := Right * PanR;

    // Output
    AOutput[I * 2] := Left;
    AOutput[I * 2 + 1] := Right;

    // Update meters
    UpdateMeters(Left, Right);
  end;
end;

procedure TSedaiMixerChannel.ResetMeters;
begin
  FPeakL := 0.0;
  FPeakR := 0.0;
  FRMSL := 0.0;
  FRMSRL := 0.0;
  FRMSAccumL := 0.0;
  FRMSAccumR := 0.0;
  FRMSSamples := 0;
  FClippedL := False;
  FClippedR := False;
end;

procedure TSedaiMixerChannel.GetMeterLevels(out ALeft, ARight: Single);
begin
  ALeft := FPeakL;
  ARight := FPeakR;
end;

function TSedaiMixerChannel.IsClipping: Boolean;
begin
  Result := FClippedL or FClippedR;
end;

end.
