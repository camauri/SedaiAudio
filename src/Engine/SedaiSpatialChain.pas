{*
 * Sedai Audio Foundation - Spatial Chain (body -> auto-space -> room)
 *
 * TSAFSpatialChain is a small convenience that wires the spatial/radiation layers
 * onto a TSAFEngine and OWNS the effect objects it creates (the mixer channels and
 * the master bus reference their inserts but do NOT free them). It turns the
 * "il tubo + spazio + sala" chain into one-liners:
 *
 *   chain := TSAFSpatialChain.Create;
 *   chain.SpatializePart(engine, partIdx, bodyViolin, 0.5, 0.5, 0.5); // C then D
 *   chain.SetRoom(engine, roomMedium, 0.25);                          // shared reverb
 *   ...
 *   engine.Free;   // free the engine FIRST (stops referencing the inserts)
 *   chain.Free;    // then the chain frees the effects
 *
 * Per-Part order is body (C) then auto-space (D) -- the physical chain: the body
 * colours/rings near-field, then the room spatialises. The room (reverb) is a
 * SINGLE shared insert on the master bus (one space for the whole mix), matching
 * "space belongs to the mix, shared", not per-preset.
 *
 * Lifetime: free the chain AFTER the engine (or after you stop rendering) so the
 * mixer never dereferences a freed insert.
 *
 * (c) 2026 Artiforge - Licensed under GPL-3.0
 *}
unit SedaiSpatialChain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  SedaiEffect, SedaiEngine, SedaiMixerChannel, SedaiBus,
  SedaiBodyResonator, SedaiAutoSpace, SedaiReverb;

type
  TSAFRoomSize = (roomSmall, roomMedium, roomLarge, roomPlate);

  { TSAFSpatialChain }
  TSAFSpatialChain = class
  private
    FEffects: array of TSedaiEffect;   // owned; freed on destroy
    procedure Track(AEffect: TSedaiEffect);
  public
    destructor Destroy; override;

    // Body/radiation (C) insert on a part's mixer channel. Returns the effect so
    // the caller can tweak it live (Width, LoadBody, SetMode...). nil if the part
    // index is invalid or the channel is full.
    function AddBody(AEngine: TSAFEngine; APartIndex: Integer;
      AKind: TBodyKind; AWidth: Single): TSedaiBodyResonator;

    // Auto-space (D) insert on a part's mixer channel.
    function AddAutoSpace(AEngine: TSAFEngine; APartIndex: Integer;
      AWidth, ASize: Single): TSedaiAutoSpace;

    // Convenience: body (C) then auto-space (D) on one part, in physical order.
    procedure SpatializePart(AEngine: TSAFEngine; APartIndex: Integer;
      AKind: TBodyKind; ABodyWidth, ASpaceWidth, ASpaceSize: Single);

    // Shared room: a single reverb insert on the master bus (whole mix).
    function SetRoom(AEngine: TSAFEngine; ASize: TSAFRoomSize; AMix: Single): TSedaiReverb;
  end;

implementation

destructor TSAFSpatialChain.Destroy;
var
  I: Integer;
begin
  for I := 0 to High(FEffects) do
    FEffects[I].Free;
  SetLength(FEffects, 0);
  inherited Destroy;
end;

procedure TSAFSpatialChain.Track(AEffect: TSedaiEffect);
var
  n: Integer;
begin
  n := Length(FEffects);
  SetLength(FEffects, n + 1);
  FEffects[n] := AEffect;
end;

function TSAFSpatialChain.AddBody(AEngine: TSAFEngine; APartIndex: Integer;
  AKind: TBodyKind; AWidth: Single): TSedaiBodyResonator;
var
  ch: TSedaiMixerChannel;
  br: TSedaiBodyResonator;
begin
  Result := nil;
  if AEngine = nil then Exit;
  ch := AEngine.GetChannel(APartIndex);
  if ch = nil then Exit;
  br := TSedaiBodyResonator.Create;
  br.SetSampleRate(AEngine.SampleRate);
  br.LoadBody(AKind);
  br.Width := AWidth;
  if ch.AddInsert(br) < 0 then    // channel full: don't leak
  begin
    br.Free;
    Exit;
  end;
  Track(br);
  Result := br;
end;

function TSAFSpatialChain.AddAutoSpace(AEngine: TSAFEngine; APartIndex: Integer;
  AWidth, ASize: Single): TSedaiAutoSpace;
var
  ch: TSedaiMixerChannel;
  sp: TSedaiAutoSpace;
begin
  Result := nil;
  if AEngine = nil then Exit;
  ch := AEngine.GetChannel(APartIndex);
  if ch = nil then Exit;
  sp := TSedaiAutoSpace.Create;
  sp.SetSampleRate(AEngine.SampleRate);
  sp.Width := AWidth;
  sp.Size := ASize;
  if ch.AddInsert(sp) < 0 then
  begin
    sp.Free;
    Exit;
  end;
  Track(sp);
  Result := sp;
end;

procedure TSAFSpatialChain.SpatializePart(AEngine: TSAFEngine; APartIndex: Integer;
  AKind: TBodyKind; ABodyWidth, ASpaceWidth, ASpaceSize: Single);
begin
  // Body first (inserted at the lower slot -> processed first), then auto-space.
  if AKind <> bodyNone then
    AddBody(AEngine, APartIndex, AKind, ABodyWidth);
  AddAutoSpace(AEngine, APartIndex, ASpaceWidth, ASpaceSize);
end;

function TSAFSpatialChain.SetRoom(AEngine: TSAFEngine; ASize: TSAFRoomSize; AMix: Single): TSedaiReverb;
var
  mb: TSedaiMasterBus;
  rv: TSedaiReverb;
begin
  Result := nil;
  if AEngine = nil then Exit;
  mb := AEngine.GetMasterBus;
  if mb = nil then Exit;
  rv := TSedaiReverb.Create;
  rv.SetSampleRate(AEngine.SampleRate);
  case ASize of
    roomSmall:  rv.LoadPresetSmallRoom;
    roomMedium: rv.LoadPresetMediumHall;
    roomLarge:  rv.LoadPresetLargeHall;
    roomPlate:  rv.LoadPresetPlate;
  end;
  rv.Mix := AMix;   // dry/wet: keep the mix mostly dry, add a touch of space
  if mb.AddInsert(rv) < 0 then
  begin
    rv.Free;
    Exit;
  end;
  Track(rv);
  Result := rv;
end;

end.
