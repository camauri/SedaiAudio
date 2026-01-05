{*
 * Sedai Audio Foundation - Mixer
 *
 * TSedaiMixer provides a complete mixing console with multiple
 * channels, groups, aux buses, and master output.
 *
 * (c) 2024 Artiforge - Licensed under GPL-3.0
 *}
unit SedaiMixer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, SedaiAudioTypes, SedaiAudioObject, SedaiSignalNode,
  SedaiMixerChannel, SedaiBus, SedaiEffect;

const
  MAX_MIXER_CHANNELS = 64;
  MAX_GROUP_BUSES = 16;
  MAX_AUX_BUSES = 8;

type
  { TSedaiMixer }
  // Complete mixing console
  TSedaiMixer = class(TSedaiAudioObject)
  private
    // Channels
    FChannels: array[0..MAX_MIXER_CHANNELS-1] of TSedaiMixerChannel;
    FChannelCount: Integer;

    // Buses
    FGroupBuses: array[0..MAX_GROUP_BUSES-1] of TSedaiGroupBus;
    FGroupBusCount: Integer;

    FAuxBuses: array[0..MAX_AUX_BUSES-1] of TSedaiAuxBus;
    FAuxBusCount: Integer;

    FMasterBus: TSedaiMasterBus;

    // Solo state
    FSoloActive: Boolean;
    FSoloCount: Integer;

    // Processing buffers
    FChannelOutput: array of Single;
    FAuxSendBuffer: array of Single;
    FGroupBuffer: array of Single;
    FMasterBuffer: array of Single;
    FMixerBlockSize: Integer;

    procedure UpdateSoloState;
    procedure EnsureBufferSize(AFrameCount: Integer);

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Reset; override;
    procedure SampleRateChanged; override;

    // Channel management
    function AddChannel(AName: string = ''): TSedaiMixerChannel;
    procedure RemoveChannel(AIndex: Integer);
    function GetChannel(AIndex: Integer): TSedaiMixerChannel;

    // Group bus management
    function AddGroupBus(AName: string = ''): TSedaiGroupBus;
    procedure RemoveGroupBus(AIndex: Integer);
    function GetGroupBus(AIndex: Integer): TSedaiGroupBus;

    // Aux bus management
    function AddAuxBus(AName: string = ''): TSedaiAuxBus;
    procedure RemoveAuxBus(AIndex: Integer);
    function GetAuxBus(AIndex: Integer): TSedaiAuxBus;

    // Master bus
    function GetMasterBus: TSedaiMasterBus;

    // Solo management
    procedure SetChannelSolo(AIndex: Integer; ASolo: Boolean);
    procedure ClearAllSolos;
    function IsAnySoloed: Boolean;

    // Mute management
    procedure MuteChannel(AIndex: Integer; AMute: Boolean);
    procedure MuteAllChannels;
    procedure UnmuteAllChannels;

    // Process all channels and buses
    procedure ProcessBlock(AInputs: array of PSingle; AOutput: PSingle;
                           AFrameCount: Integer);

    // Process single channel (for real-time input)
    procedure ProcessChannel(AChannelIndex: Integer; AInput, AOutput: PSingle;
                             AFrameCount: Integer);

    // Properties
    property ChannelCount: Integer read FChannelCount;
    property GroupBusCount: Integer read FGroupBusCount;
    property AuxBusCount: Integer read FAuxBusCount;
    property SoloActive: Boolean read FSoloActive;
    property MasterBus: TSedaiMasterBus read FMasterBus;
  end;

implementation

{ TSedaiMixer }

constructor TSedaiMixer.Create;
var
  I: Integer;
begin
  inherited Create;

  // Initialize channel array
  FChannelCount := 0;
  for I := 0 to MAX_MIXER_CHANNELS - 1 do
    FChannels[I] := nil;

  // Initialize group buses
  FGroupBusCount := 0;
  for I := 0 to MAX_GROUP_BUSES - 1 do
    FGroupBuses[I] := nil;

  // Initialize aux buses
  FAuxBusCount := 0;
  for I := 0 to MAX_AUX_BUSES - 1 do
    FAuxBuses[I] := nil;

  // Create master bus
  FMasterBus := TSedaiMasterBus.Create;

  FSoloActive := False;
  FSoloCount := 0;
  FMixerBlockSize := 0;
end;

destructor TSedaiMixer.Destroy;
var
  I: Integer;
begin
  // Free channels
  for I := 0 to MAX_MIXER_CHANNELS - 1 do
    if Assigned(FChannels[I]) then
      FChannels[I].Free;

  // Free group buses
  for I := 0 to MAX_GROUP_BUSES - 1 do
    if Assigned(FGroupBuses[I]) then
      FGroupBuses[I].Free;

  // Free aux buses
  for I := 0 to MAX_AUX_BUSES - 1 do
    if Assigned(FAuxBuses[I]) then
      FAuxBuses[I].Free;

  // Free master bus
  FMasterBus.Free;

  // Free buffers
  SetLength(FChannelOutput, 0);
  SetLength(FAuxSendBuffer, 0);
  SetLength(FGroupBuffer, 0);
  SetLength(FMasterBuffer, 0);

  inherited Destroy;
end;

procedure TSedaiMixer.Reset;
var
  I: Integer;
begin
  inherited Reset;

  for I := 0 to MAX_MIXER_CHANNELS - 1 do
    if Assigned(FChannels[I]) then
      FChannels[I].Reset;

  for I := 0 to MAX_GROUP_BUSES - 1 do
    if Assigned(FGroupBuses[I]) then
      FGroupBuses[I].Reset;

  for I := 0 to MAX_AUX_BUSES - 1 do
    if Assigned(FAuxBuses[I]) then
      FAuxBuses[I].Reset;

  FMasterBus.Reset;
end;

procedure TSedaiMixer.SampleRateChanged;
var
  I: Integer;
  Rate: Cardinal;
begin
  inherited SampleRateChanged;
  Rate := SampleRate;

  for I := 0 to MAX_MIXER_CHANNELS - 1 do
    if Assigned(FChannels[I]) then
      FChannels[I].SetSampleRate(Rate);

  for I := 0 to MAX_GROUP_BUSES - 1 do
    if Assigned(FGroupBuses[I]) then
      FGroupBuses[I].SetSampleRate(Rate);

  for I := 0 to MAX_AUX_BUSES - 1 do
    if Assigned(FAuxBuses[I]) then
      FAuxBuses[I].SetSampleRate(Rate);

  FMasterBus.SetSampleRate(Rate);
end;

procedure TSedaiMixer.UpdateSoloState;
var
  I: Integer;
begin
  FSoloCount := 0;

  // Count soloed channels
  for I := 0 to MAX_MIXER_CHANNELS - 1 do
    if Assigned(FChannels[I]) and FChannels[I].Soloed then
      Inc(FSoloCount);

  FSoloActive := FSoloCount > 0;

  // Update solo active flag on each channel
  for I := 0 to MAX_MIXER_CHANNELS - 1 do
  begin
    if Assigned(FChannels[I]) then
    begin
      if FSoloActive then
        // If solo is active, mute channels that aren't soloed (unless solo-safe)
        FChannels[I].SoloActive := not FChannels[I].Soloed and not FChannels[I].SoloSafe
      else
        FChannels[I].SoloActive := False;
    end;
  end;
end;

procedure TSedaiMixer.EnsureBufferSize(AFrameCount: Integer);
begin
  if FMixerBlockSize < AFrameCount then
  begin
    FMixerBlockSize := AFrameCount;
    SetLength(FChannelOutput, AFrameCount * 2);
    SetLength(FAuxSendBuffer, AFrameCount * 2);
    SetLength(FGroupBuffer, AFrameCount * 2);
    SetLength(FMasterBuffer, AFrameCount * 2);
  end;
end;

function TSedaiMixer.AddChannel(AName: string): TSedaiMixerChannel;
var
  I: Integer;
begin
  Result := nil;

  // Find empty slot
  for I := 0 to MAX_MIXER_CHANNELS - 1 do
  begin
    if FChannels[I] = nil then
    begin
      FChannels[I] := TSedaiMixerChannel.Create;
      FChannels[I].SetSampleRate(FSampleRate);
      FChannels[I].Index := I;

      if AName = '' then
        FChannels[I].Name := Format('Channel %d', [I + 1])
      else
        FChannels[I].Name := AName;

      Inc(FChannelCount);
      Result := FChannels[I];
      Exit;
    end;
  end;
end;

procedure TSedaiMixer.RemoveChannel(AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= MAX_MIXER_CHANNELS) then Exit;
  if FChannels[AIndex] = nil then Exit;

  FChannels[AIndex].Free;
  FChannels[AIndex] := nil;
  Dec(FChannelCount);

  UpdateSoloState;
end;

function TSedaiMixer.GetChannel(AIndex: Integer): TSedaiMixerChannel;
begin
  if (AIndex >= 0) and (AIndex < MAX_MIXER_CHANNELS) then
    Result := FChannels[AIndex]
  else
    Result := nil;
end;

function TSedaiMixer.AddGroupBus(AName: string): TSedaiGroupBus;
var
  I: Integer;
begin
  Result := nil;

  for I := 0 to MAX_GROUP_BUSES - 1 do
  begin
    if FGroupBuses[I] = nil then
    begin
      FGroupBuses[I] := TSedaiGroupBus.Create;
      FGroupBuses[I].SetSampleRate(FSampleRate);
      FGroupBuses[I].Index := I;

      if AName = '' then
        FGroupBuses[I].Name := Format('Group %d', [I + 1])
      else
        FGroupBuses[I].Name := AName;

      Inc(FGroupBusCount);
      Result := FGroupBuses[I];
      Exit;
    end;
  end;
end;

procedure TSedaiMixer.RemoveGroupBus(AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= MAX_GROUP_BUSES) then Exit;
  if FGroupBuses[AIndex] = nil then Exit;

  FGroupBuses[AIndex].Free;
  FGroupBuses[AIndex] := nil;
  Dec(FGroupBusCount);
end;

function TSedaiMixer.GetGroupBus(AIndex: Integer): TSedaiGroupBus;
begin
  if (AIndex >= 0) and (AIndex < MAX_GROUP_BUSES) then
    Result := FGroupBuses[AIndex]
  else
    Result := nil;
end;

function TSedaiMixer.AddAuxBus(AName: string): TSedaiAuxBus;
var
  I: Integer;
begin
  Result := nil;

  for I := 0 to MAX_AUX_BUSES - 1 do
  begin
    if FAuxBuses[I] = nil then
    begin
      FAuxBuses[I] := TSedaiAuxBus.Create;
      FAuxBuses[I].SetSampleRate(FSampleRate);
      FAuxBuses[I].Index := I;

      if AName = '' then
        FAuxBuses[I].Name := Format('Aux %d', [I + 1])
      else
        FAuxBuses[I].Name := AName;

      Inc(FAuxBusCount);
      Result := FAuxBuses[I];
      Exit;
    end;
  end;
end;

procedure TSedaiMixer.RemoveAuxBus(AIndex: Integer);
begin
  if (AIndex < 0) or (AIndex >= MAX_AUX_BUSES) then Exit;
  if FAuxBuses[AIndex] = nil then Exit;

  FAuxBuses[AIndex].Free;
  FAuxBuses[AIndex] := nil;
  Dec(FAuxBusCount);
end;

function TSedaiMixer.GetAuxBus(AIndex: Integer): TSedaiAuxBus;
begin
  if (AIndex >= 0) and (AIndex < MAX_AUX_BUSES) then
    Result := FAuxBuses[AIndex]
  else
    Result := nil;
end;

function TSedaiMixer.GetMasterBus: TSedaiMasterBus;
begin
  Result := FMasterBus;
end;

procedure TSedaiMixer.SetChannelSolo(AIndex: Integer; ASolo: Boolean);
begin
  if (AIndex < 0) or (AIndex >= MAX_MIXER_CHANNELS) then Exit;
  if FChannels[AIndex] = nil then Exit;

  FChannels[AIndex].Soloed := ASolo;
  UpdateSoloState;
end;

procedure TSedaiMixer.ClearAllSolos;
var
  I: Integer;
begin
  for I := 0 to MAX_MIXER_CHANNELS - 1 do
    if Assigned(FChannels[I]) then
      FChannels[I].Soloed := False;

  UpdateSoloState;
end;

function TSedaiMixer.IsAnySoloed: Boolean;
begin
  Result := FSoloActive;
end;

procedure TSedaiMixer.MuteChannel(AIndex: Integer; AMute: Boolean);
begin
  if (AIndex < 0) or (AIndex >= MAX_MIXER_CHANNELS) then Exit;
  if FChannels[AIndex] = nil then Exit;

  FChannels[AIndex].Muted := AMute;
end;

procedure TSedaiMixer.MuteAllChannels;
var
  I: Integer;
begin
  for I := 0 to MAX_MIXER_CHANNELS - 1 do
    if Assigned(FChannels[I]) then
      FChannels[I].Muted := True;
end;

procedure TSedaiMixer.UnmuteAllChannels;
var
  I: Integer;
begin
  for I := 0 to MAX_MIXER_CHANNELS - 1 do
    if Assigned(FChannels[I]) then
      FChannels[I].Muted := False;
end;

procedure TSedaiMixer.ProcessBlock(AInputs: array of PSingle; AOutput: PSingle;
  AFrameCount: Integer);
var
  I, J, K: Integer;
  OutputBus: Integer;
  AuxSend: TAuxSend;
begin
  EnsureBufferSize(AFrameCount);

  // Clear all bus summing buffers
  for I := 0 to MAX_GROUP_BUSES - 1 do
    if Assigned(FGroupBuses[I]) then
      FGroupBuses[I].ClearSum(AFrameCount);

  for I := 0 to MAX_AUX_BUSES - 1 do
    if Assigned(FAuxBuses[I]) then
      FAuxBuses[I].ClearSum(AFrameCount);

  FMasterBus.ClearSum(AFrameCount);

  // Clear aux send buffer
  for I := 0 to AFrameCount * 2 - 1 do
    FAuxSendBuffer[I] := 0.0;

  // Process each channel
  for I := 0 to MAX_MIXER_CHANNELS - 1 do
  begin
    if not Assigned(FChannels[I]) then Continue;

    // Get input for this channel
    if (I >= 0) and (I <= High(AInputs)) and (AInputs[I] <> nil) then
    begin
      // Process channel
      FChannels[I].ProcessBlock(AInputs[I], @FChannelOutput[0], AFrameCount);

      // Get aux sends (before or after fader depending on config)
      for J := 0 to MAX_AUX_BUSES - 1 do
      begin
        if Assigned(FAuxBuses[J]) then
        begin
          AuxSend := FChannels[I].GetAuxSend(J);
          if AuxSend.Enabled then
          begin
            // Clear send buffer
            for K := 0 to AFrameCount * 2 - 1 do
              FAuxSendBuffer[K] := 0.0;

            // Get send output
            if AuxSend.PreFader then
              FChannels[I].GetAuxSendOutput(J, @FAuxSendBuffer[0], AFrameCount, AInputs[I])
            else
              FChannels[I].GetAuxSendOutput(J, @FAuxSendBuffer[0], AFrameCount, @FChannelOutput[0]);

            // Add to aux bus
            FAuxBuses[J].AddToSum(@FAuxSendBuffer[0], AFrameCount);
          end;
        end;
      end;

      // Route channel output to appropriate bus
      OutputBus := FChannels[I].OutputBus;

      if OutputBus = -1 then
        // Direct to master
        FMasterBus.AddToSum(@FChannelOutput[0], AFrameCount)
      else if (OutputBus >= 0) and (OutputBus < MAX_GROUP_BUSES) and
              Assigned(FGroupBuses[OutputBus]) then
        // To group bus
        FGroupBuses[OutputBus].AddToSum(@FChannelOutput[0], AFrameCount);
    end;
  end;

  // Process aux buses and route to master
  for I := 0 to MAX_AUX_BUSES - 1 do
  begin
    if Assigned(FAuxBuses[I]) then
    begin
      FAuxBuses[I].ProcessBlock(nil, @FAuxSendBuffer[0], AFrameCount);
      FMasterBus.AddToSum(@FAuxSendBuffer[0], AFrameCount);
    end;
  end;

  // Process group buses
  for I := 0 to MAX_GROUP_BUSES - 1 do
  begin
    if Assigned(FGroupBuses[I]) then
    begin
      FGroupBuses[I].ProcessBlock(nil, @FGroupBuffer[0], AFrameCount);

      OutputBus := FGroupBuses[I].OutputBus;

      if OutputBus = -1 then
        FMasterBus.AddToSum(@FGroupBuffer[0], AFrameCount)
      else if (OutputBus >= 0) and (OutputBus < MAX_GROUP_BUSES) and
              (OutputBus <> I) and Assigned(FGroupBuses[OutputBus]) then
        FGroupBuses[OutputBus].AddToSum(@FGroupBuffer[0], AFrameCount);
    end;
  end;

  // Process master bus
  FMasterBus.ProcessBlock(nil, AOutput, AFrameCount);
end;

procedure TSedaiMixer.ProcessChannel(AChannelIndex: Integer; AInput, AOutput: PSingle;
  AFrameCount: Integer);
begin
  if (AChannelIndex < 0) or (AChannelIndex >= MAX_MIXER_CHANNELS) then Exit;
  if not Assigned(FChannels[AChannelIndex]) then Exit;

  FChannels[AChannelIndex].ProcessBlock(AInput, AOutput, AFrameCount);
end;

end.
