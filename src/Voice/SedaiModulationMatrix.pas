{*
 * Sedai Audio Foundation - Modulation Matrix
 *
 * TSedaiModulationMatrix provides centralized modulation routing
 * between sources (envelopes, LFOs, velocity, mod wheel, etc.) and
 * destinations (oscillator pitch, filter cutoff, amplitude, etc.).
 *
 * (c) 2024 Artiforge - Licensed under GPL-3.0
 *}
unit SedaiModulationMatrix;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SedaiAudioTypes, SedaiAudioObject;

const
  MAX_MOD_SLOTS = 16;             // Maximum modulation routing slots
  MAX_MOD_SOURCES = 32;           // Maximum modulation sources
  MAX_MOD_DESTINATIONS = 32;      // Maximum modulation destinations

type
  // Modulation source types
  TModSourceType = (
    mstNone,
    mstEnvelope1,
    mstEnvelope2,
    mstEnvelope3,
    mstLFO1,
    mstLFO2,
    mstVelocity,
    mstKeytrack,       // Note number mapped to modulation
    mstAftertouch,
    mstModWheel,
    mstPitchBend,
    mstExpression,
    mstBreath,
    mstRandom,         // Random value per note
    mstCustom1,
    mstCustom2,
    mstCustom3,
    mstCustom4
  );

  // Modulation destination types
  TModDestType = (
    mdtNone,
    mdtOsc1Pitch,
    mdtOsc2Pitch,
    mdtOsc3Pitch,
    mdtOscAllPitch,
    mdtOsc1PulseWidth,
    mdtOsc2PulseWidth,
    mdtOsc3PulseWidth,
    mdtOsc1Level,
    mdtOsc2Level,
    mdtOsc3Level,
    mdtFilterCutoff,
    mdtFilterResonance,
    mdtFilterEnvAmount,
    mdtAmplitude,
    mdtPan,
    mdtLFO1Rate,
    mdtLFO2Rate,
    mdtLFO1Depth,
    mdtLFO2Depth,
    mdtCustom1,
    mdtCustom2,
    mdtCustom3,
    mdtCustom4
  );

  { TModulationSlot }
  // Single modulation routing
  TModulationSlot = record
    Enabled: Boolean;
    Source: TModSourceType;
    Destination: TModDestType;
    Amount: Single;               // -1.0 to +1.0
    Bipolar: Boolean;             // True = -1..+1, False = 0..+1
    Curve: TParameterCurve;       // Shaping curve
  end;

  { TSedaiModulationMatrix }
  // Centralized modulation routing
  TSedaiModulationMatrix = class(TSedaiAudioObject)
  private
    // Modulation slots
    FSlots: array[0..MAX_MOD_SLOTS-1] of TModulationSlot;
    FSlotCount: Integer;

    // Source values (updated each sample/block)
    FSourceValues: array[TModSourceType] of Single;

    // Destination modulation amounts (accumulated)
    FDestinationMod: array[TModDestType] of Single;

    // Custom source/destination names
    FCustomSourceNames: array[0..3] of string;
    FCustomDestNames: array[0..3] of string;

    function GetSlot(AIndex: Integer): TModulationSlot;
    procedure SetSlot(AIndex: Integer; const AValue: TModulationSlot);
    function ApplyCurve(AValue: Single; ACurve: TParameterCurve): Single;

  public
    constructor Create; override;

    // Reset all modulation
    procedure Reset; override;

    // ========================================================================
    // SLOT MANAGEMENT
    // ========================================================================

    // Add a new modulation routing
    function AddSlot(ASource: TModSourceType; ADest: TModDestType;
                     AAmount: Single; ABipolar: Boolean = True): Integer;

    // Remove a modulation slot
    procedure RemoveSlot(AIndex: Integer);

    // Clear all slots
    procedure ClearAllSlots;

    // Enable/disable slot
    procedure SetSlotEnabled(AIndex: Integer; AEnabled: Boolean);

    // Set slot amount
    procedure SetSlotAmount(AIndex: Integer; AAmount: Single);

    // Get number of active slots
    function GetActiveSlotCount: Integer;

    // ========================================================================
    // SOURCE VALUES
    // ========================================================================

    // Set a source value (call from modulators)
    procedure SetSourceValue(ASource: TModSourceType; AValue: Single);

    // Get current source value
    function GetSourceValue(ASource: TModSourceType): Single;

    // ========================================================================
    // DESTINATION VALUES
    // ========================================================================

    // Calculate modulation for all destinations
    procedure Process;

    // Get modulation amount for destination
    function GetModulation(ADest: TModDestType): Single;

    // Get total modulated value (base + modulation)
    function GetModulatedValue(ADest: TModDestType; ABaseValue: Single): Single;

    // ========================================================================
    // CUSTOM NAMES
    // ========================================================================

    // Set custom source name
    procedure SetCustomSourceName(AIndex: Integer; const AName: string);

    // Set custom destination name
    procedure SetCustomDestName(AIndex: Integer; const AName: string);

    // Get source name
    function GetSourceName(ASource: TModSourceType): string;

    // Get destination name
    function GetDestName(ADest: TModDestType): string;

    // ========================================================================
    // PROPERTIES
    // ========================================================================

    property Slots[AIndex: Integer]: TModulationSlot read GetSlot write SetSlot;
    property SlotCount: Integer read FSlotCount;
  end;

implementation

{ TSedaiModulationMatrix }

constructor TSedaiModulationMatrix.Create;
var
  I: Integer;
  S: TModSourceType;
  D: TModDestType;
begin
  inherited Create;

  FSlotCount := 0;

  // Initialize all slots
  for I := 0 to MAX_MOD_SLOTS - 1 do
  begin
    FSlots[I].Enabled := False;
    FSlots[I].Source := mstNone;
    FSlots[I].Destination := mdtNone;
    FSlots[I].Amount := 0.0;
    FSlots[I].Bipolar := True;
    FSlots[I].Curve := pcLinear;
  end;

  // Initialize source values
  for S := Low(TModSourceType) to High(TModSourceType) do
    FSourceValues[S] := 0.0;

  // Initialize destination modulation
  for D := Low(TModDestType) to High(TModDestType) do
    FDestinationMod[D] := 0.0;

  // Default custom names
  for I := 0 to 3 do
  begin
    FCustomSourceNames[I] := Format('Custom Src %d', [I + 1]);
    FCustomDestNames[I] := Format('Custom Dst %d', [I + 1]);
  end;
end;

procedure TSedaiModulationMatrix.Reset;
var
  S: TModSourceType;
  D: TModDestType;
begin
  inherited Reset;

  // Reset all source values
  for S := Low(TModSourceType) to High(TModSourceType) do
    FSourceValues[S] := 0.0;

  // Reset destination modulation
  for D := Low(TModDestType) to High(TModDestType) do
    FDestinationMod[D] := 0.0;
end;

function TSedaiModulationMatrix.GetSlot(AIndex: Integer): TModulationSlot;
begin
  if (AIndex >= 0) and (AIndex < MAX_MOD_SLOTS) then
    Result := FSlots[AIndex]
  else
  begin
    Result.Enabled := False;
    Result.Source := mstNone;
    Result.Destination := mdtNone;
    Result.Amount := 0.0;
    Result.Bipolar := True;
    Result.Curve := pcLinear;
  end;
end;

procedure TSedaiModulationMatrix.SetSlot(AIndex: Integer; const AValue: TModulationSlot);
begin
  if (AIndex >= 0) and (AIndex < MAX_MOD_SLOTS) then
    FSlots[AIndex] := AValue;
end;

function TSedaiModulationMatrix.ApplyCurve(AValue: Single; ACurve: TParameterCurve): Single;
begin
  case ACurve of
    pcLinear:
      Result := AValue;

    pcLogarithmic:
      begin
        if AValue >= 0 then
          Result := Sqrt(AValue)
        else
          Result := -Sqrt(-AValue);
      end;

    pcExponential:
      begin
        if AValue >= 0 then
          Result := AValue * AValue
        else
          Result := -(AValue * AValue);
      end;

    else
      Result := AValue;
  end;
end;

function TSedaiModulationMatrix.AddSlot(ASource: TModSourceType; ADest: TModDestType;
                                        AAmount: Single; ABipolar: Boolean): Integer;
var
  I: Integer;
begin
  Result := -1;

  // Find first empty slot
  for I := 0 to MAX_MOD_SLOTS - 1 do
  begin
    if not FSlots[I].Enabled then
    begin
      FSlots[I].Enabled := True;
      FSlots[I].Source := ASource;
      FSlots[I].Destination := ADest;
      FSlots[I].Amount := AAmount;
      FSlots[I].Bipolar := ABipolar;
      FSlots[I].Curve := pcLinear;
      Inc(FSlotCount);
      Result := I;
      Exit;
    end;
  end;
end;

procedure TSedaiModulationMatrix.RemoveSlot(AIndex: Integer);
begin
  if (AIndex >= 0) and (AIndex < MAX_MOD_SLOTS) and FSlots[AIndex].Enabled then
  begin
    FSlots[AIndex].Enabled := False;
    FSlots[AIndex].Source := mstNone;
    FSlots[AIndex].Destination := mdtNone;
    Dec(FSlotCount);
  end;
end;

procedure TSedaiModulationMatrix.ClearAllSlots;
var
  I: Integer;
begin
  for I := 0 to MAX_MOD_SLOTS - 1 do
  begin
    FSlots[I].Enabled := False;
    FSlots[I].Source := mstNone;
    FSlots[I].Destination := mdtNone;
  end;
  FSlotCount := 0;
end;

procedure TSedaiModulationMatrix.SetSlotEnabled(AIndex: Integer; AEnabled: Boolean);
begin
  if (AIndex >= 0) and (AIndex < MAX_MOD_SLOTS) then
  begin
    if FSlots[AIndex].Enabled <> AEnabled then
    begin
      FSlots[AIndex].Enabled := AEnabled;
      if AEnabled then
        Inc(FSlotCount)
      else
        Dec(FSlotCount);
    end;
  end;
end;

procedure TSedaiModulationMatrix.SetSlotAmount(AIndex: Integer; AAmount: Single);
begin
  if (AIndex >= 0) and (AIndex < MAX_MOD_SLOTS) then
  begin
    if AAmount < -1.0 then
      AAmount := -1.0
    else if AAmount > 1.0 then
      AAmount := 1.0;
    FSlots[AIndex].Amount := AAmount;
  end;
end;

function TSedaiModulationMatrix.GetActiveSlotCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to MAX_MOD_SLOTS - 1 do
  begin
    if FSlots[I].Enabled then
      Inc(Result);
  end;
end;

procedure TSedaiModulationMatrix.SetSourceValue(ASource: TModSourceType; AValue: Single);
begin
  FSourceValues[ASource] := AValue;
end;

function TSedaiModulationMatrix.GetSourceValue(ASource: TModSourceType): Single;
begin
  Result := FSourceValues[ASource];
end;

procedure TSedaiModulationMatrix.Process;
var
  I: Integer;
  D: TModDestType;
  SourceVal, ModVal: Single;
begin
  // Clear all destination modulation
  for D := Low(TModDestType) to High(TModDestType) do
    FDestinationMod[D] := 0.0;

  // Process each active slot
  for I := 0 to MAX_MOD_SLOTS - 1 do
  begin
    if FSlots[I].Enabled and (FSlots[I].Source <> mstNone) and
       (FSlots[I].Destination <> mdtNone) then
    begin
      // Get source value
      SourceVal := FSourceValues[FSlots[I].Source];

      // Convert to unipolar if needed
      if not FSlots[I].Bipolar then
        SourceVal := (SourceVal + 1.0) * 0.5;

      // Apply curve
      SourceVal := ApplyCurve(SourceVal, FSlots[I].Curve);

      // Calculate modulation amount
      ModVal := SourceVal * FSlots[I].Amount;

      // Accumulate to destination
      FDestinationMod[FSlots[I].Destination] :=
        FDestinationMod[FSlots[I].Destination] + ModVal;
    end;
  end;
end;

function TSedaiModulationMatrix.GetModulation(ADest: TModDestType): Single;
begin
  Result := FDestinationMod[ADest];
end;

function TSedaiModulationMatrix.GetModulatedValue(ADest: TModDestType;
                                                  ABaseValue: Single): Single;
begin
  Result := ABaseValue + FDestinationMod[ADest];
end;

procedure TSedaiModulationMatrix.SetCustomSourceName(AIndex: Integer; const AName: string);
begin
  if (AIndex >= 0) and (AIndex <= 3) then
    FCustomSourceNames[AIndex] := AName;
end;

procedure TSedaiModulationMatrix.SetCustomDestName(AIndex: Integer; const AName: string);
begin
  if (AIndex >= 0) and (AIndex <= 3) then
    FCustomDestNames[AIndex] := AName;
end;

function TSedaiModulationMatrix.GetSourceName(ASource: TModSourceType): string;
begin
  case ASource of
    mstNone: Result := 'None';
    mstEnvelope1: Result := 'Envelope 1';
    mstEnvelope2: Result := 'Envelope 2';
    mstEnvelope3: Result := 'Envelope 3';
    mstLFO1: Result := 'LFO 1';
    mstLFO2: Result := 'LFO 2';
    mstVelocity: Result := 'Velocity';
    mstKeytrack: Result := 'Key Track';
    mstAftertouch: Result := 'Aftertouch';
    mstModWheel: Result := 'Mod Wheel';
    mstPitchBend: Result := 'Pitch Bend';
    mstExpression: Result := 'Expression';
    mstBreath: Result := 'Breath';
    mstRandom: Result := 'Random';
    mstCustom1: Result := FCustomSourceNames[0];
    mstCustom2: Result := FCustomSourceNames[1];
    mstCustom3: Result := FCustomSourceNames[2];
    mstCustom4: Result := FCustomSourceNames[3];
    else Result := 'Unknown';
  end;
end;

function TSedaiModulationMatrix.GetDestName(ADest: TModDestType): string;
begin
  case ADest of
    mdtNone: Result := 'None';
    mdtOsc1Pitch: Result := 'Osc 1 Pitch';
    mdtOsc2Pitch: Result := 'Osc 2 Pitch';
    mdtOsc3Pitch: Result := 'Osc 3 Pitch';
    mdtOscAllPitch: Result := 'All Osc Pitch';
    mdtOsc1PulseWidth: Result := 'Osc 1 PW';
    mdtOsc2PulseWidth: Result := 'Osc 2 PW';
    mdtOsc3PulseWidth: Result := 'Osc 3 PW';
    mdtOsc1Level: Result := 'Osc 1 Level';
    mdtOsc2Level: Result := 'Osc 2 Level';
    mdtOsc3Level: Result := 'Osc 3 Level';
    mdtFilterCutoff: Result := 'Filter Cutoff';
    mdtFilterResonance: Result := 'Filter Reso';
    mdtFilterEnvAmount: Result := 'Filter Env';
    mdtAmplitude: Result := 'Amplitude';
    mdtPan: Result := 'Pan';
    mdtLFO1Rate: Result := 'LFO 1 Rate';
    mdtLFO2Rate: Result := 'LFO 2 Rate';
    mdtLFO1Depth: Result := 'LFO 1 Depth';
    mdtLFO2Depth: Result := 'LFO 2 Depth';
    mdtCustom1: Result := FCustomDestNames[0];
    mdtCustom2: Result := FCustomDestNames[1];
    mdtCustom3: Result := FCustomDestNames[2];
    mdtCustom4: Result := FCustomDestNames[3];
    else Result := 'Unknown';
  end;
end;

end.
