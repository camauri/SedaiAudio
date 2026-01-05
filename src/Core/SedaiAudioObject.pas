{*
 * Sedai Audio Foundation - Audio Object Base Class
 *
 * TSedaiAudioObject is the root base class for all audio objects in the
 * Sedai Audio Foundation library. It provides common functionality for
 * identification, lifecycle management, and configuration.
 *
 * (c) 2024 Artiforge - Licensed under GPL-3.0
 *}
unit SedaiAudioObject;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SedaiAudioTypes;

type
  { TSedaiAudioObject }
  // Root base class for all audio objects
  TSedaiAudioObject = class
  private
    FID: TGUID;
    FName: string;
    FEnabled: Boolean;
    FSuspended: Boolean;
    FParent: TSedaiAudioObject;
    FProcessingLoad: Single;  // CPU usage estimate (0-1)

  protected
    FSampleRate: Cardinal;
    FBlockSize: Integer;
    // Called when sample rate changes - override to recalculate coefficients
    procedure SampleRateChanged; virtual;

    // Called when block size changes - override to reallocate buffers
    procedure BlockSizeChanged; virtual;

    // Set the parent object
    procedure SetParent(AValue: TSedaiAudioObject); virtual;

  public
    constructor Create; virtual;
    destructor Destroy; override;

    // ========================================================================
    // LIFECYCLE METHODS
    // ========================================================================

    // Initialize the object - call after creation and configuration
    procedure Init; virtual;

    // Reset the object to initial state (clear buffers, reset phase, etc.)
    procedure Reset; virtual;

    // Main processing method - override in subclasses
    procedure Process; virtual; abstract;

    // Temporarily disable processing without full reset
    procedure Suspend; virtual;

    // Re-enable processing after suspend
    procedure Resume; virtual;

    // ========================================================================
    // CONFIGURATION
    // ========================================================================

    // Set sample rate (calls SampleRateChanged if value changes)
    procedure SetSampleRate(AValue: Cardinal);

    // Set block size (calls BlockSizeChanged if value changes)
    procedure SetBlockSize(AValue: Integer);

    // ========================================================================
    // UTILITY METHODS
    // ========================================================================

    // Get estimated CPU usage (0-1)
    function GetProcessingLoad: Single; virtual;

    // Generate a new unique ID
    procedure RegenerateID;

    // Check if object is currently processing (enabled and not suspended)
    function IsActive: Boolean; inline;

    // ========================================================================
    // PROPERTIES
    // ========================================================================

    // Unique identifier for serialization and routing
    property ID: TGUID read FID;

    // Human-readable name
    property Name: string read FName write FName;

    // Enable/disable processing
    property Enabled: Boolean read FEnabled write FEnabled;

    // Suspended state (temporarily disabled)
    property Suspended: Boolean read FSuspended;

    // Parent object in hierarchy (nil if root)
    property Parent: TSedaiAudioObject read FParent write SetParent;

    // Operating sample rate
    property SampleRate: Cardinal read FSampleRate;

    // Processing block size
    property BlockSize: Integer read FBlockSize;
  end;

implementation

{ TSedaiAudioObject }

constructor TSedaiAudioObject.Create;
begin
  inherited Create;

  // Generate unique ID
  CreateGUID(FID);

  // Default values
  FName := '';
  FEnabled := True;
  FSuspended := False;
  FParent := nil;
  FSampleRate := SEDAI_DEFAULT_SAMPLE_RATE;
  FBlockSize := SEDAI_DEFAULT_BLOCK_SIZE;
  FProcessingLoad := 0.0;
end;

destructor TSedaiAudioObject.Destroy;
begin
  // Detach from parent
  FParent := nil;

  inherited Destroy;
end;

procedure TSedaiAudioObject.Init;
begin
  // Base implementation does nothing
  // Override in subclasses to perform initialization after configuration
end;

procedure TSedaiAudioObject.Reset;
begin
  // Base implementation resets processing load
  FProcessingLoad := 0.0;

  // Override in subclasses to reset state
end;

procedure TSedaiAudioObject.Suspend;
begin
  FSuspended := True;
end;

procedure TSedaiAudioObject.Resume;
begin
  FSuspended := False;
end;

procedure TSedaiAudioObject.SampleRateChanged;
begin
  // Override in subclasses to recalculate sample-rate-dependent values
end;

procedure TSedaiAudioObject.BlockSizeChanged;
begin
  // Override in subclasses to reallocate buffers
end;

procedure TSedaiAudioObject.SetParent(AValue: TSedaiAudioObject);
begin
  // Prevent circular references
  if AValue = Self then
    Exit;

  FParent := AValue;
end;

procedure TSedaiAudioObject.SetSampleRate(AValue: Cardinal);
begin
  if AValue <> FSampleRate then
  begin
    FSampleRate := AValue;
    SampleRateChanged;
  end;
end;

procedure TSedaiAudioObject.SetBlockSize(AValue: Integer);
begin
  if AValue <> FBlockSize then
  begin
    FBlockSize := AValue;
    BlockSizeChanged;
  end;
end;

function TSedaiAudioObject.GetProcessingLoad: Single;
begin
  Result := FProcessingLoad;
end;

procedure TSedaiAudioObject.RegenerateID;
begin
  CreateGUID(FID);
end;

function TSedaiAudioObject.IsActive: Boolean; inline;
begin
  Result := FEnabled and not FSuspended;
end;

end.
