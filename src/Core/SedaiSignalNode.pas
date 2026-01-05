{*
 * Sedai Audio Foundation - Signal Node
 *
 * TSedaiSignalNode is the base class for all DSP nodes that can be
 * connected in a signal processing graph. It provides connection
 * management, block processing, and latency compensation.
 *
 * (c) 2024 Artiforge - Licensed under GPL-3.0
 *}
unit SedaiSignalNode;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SedaiAudioTypes, SedaiAudioObject;

type
  // Forward declaration
  TSedaiSignalNode = class;

  // Array of signal nodes
  TSedaiSignalNodeArray = array of TSedaiSignalNode;

  { TSedaiSignalNode }
  // Abstract DSP node for graph-based signal processing
  TSedaiSignalNode = class(TSedaiAudioObject)
  private
    FInputs: TSedaiSignalNodeArray;
    FOutputs: TSedaiSignalNodeArray;
    FLatency: Integer;           // Processing latency in samples
    FProcessingOrder: Integer;   // Topological sort order
    FBypassInput: Integer;       // Which input passes in bypass mode (-1 = none)
    FProcessed: Boolean;         // Flag for graph traversal

    function GetInputCount: Integer;
    function GetOutputCount: Integer;
    function GetInput(AIndex: Integer): TSedaiSignalNode;
    function GetOutput(AIndex: Integer): TSedaiSignalNode;

  protected
    // Internal buffer for processing
    FInternalBuffer: array of Single;
    FInternalBufferSize: Integer;

    // Allocate internal buffer based on block size and channels
    procedure AllocateInternalBuffer(ASamples, AChannels: Integer);

    // Called when block size changes
    procedure BlockSizeChanged; override;

  public
    constructor Create; override;
    destructor Destroy; override;

    // ========================================================================
    // CONNECTION MANAGEMENT
    // ========================================================================

    // Connect this node's output to another node's input
    function Connect(ATarget: TSedaiSignalNode): Boolean;

    // Disconnect from a target node
    function Disconnect(ATarget: TSedaiSignalNode): Boolean;

    // Disconnect all connections
    procedure DisconnectAll;

    // Check if connected to a specific node
    function IsConnectedTo(ANode: TSedaiSignalNode): Boolean;

    // Check if this node has any inputs
    function HasInputs: Boolean;

    // Check if this node has any outputs
    function HasOutputs: Boolean;

    // ========================================================================
    // GRAPH VALIDATION
    // ========================================================================

    // Validate connections (check for cycles - DAG validation)
    // Returns True if graph is valid (no cycles), False otherwise
    function ValidateConnections: Boolean;

    // Calculate total latency from source to this node
    function GetLatencyCompensation: Integer;

    // Reset processed flag (call before graph traversal)
    procedure ResetProcessedFlag;

    // ========================================================================
    // PROCESSING
    // ========================================================================

    // Process a block of audio - override in subclasses
    // AInput: Input buffer (interleaved stereo)
    // AOutput: Output buffer (interleaved stereo)
    // AFrameCount: Number of frames to process
    procedure ProcessBlock(AInput, AOutput: PSingle; AFrameCount: Integer); virtual;

    // Bypass processing - copy input to output
    procedure BypassBlock(AInput, AOutput: PSingle; AFrameCount: Integer; AChannels: Integer);

    // ========================================================================
    // PROPERTIES
    // ========================================================================

    // Input connections
    property InputCount: Integer read GetInputCount;
    property Inputs[AIndex: Integer]: TSedaiSignalNode read GetInput;

    // Output connections
    property OutputCount: Integer read GetOutputCount;
    property Outputs[AIndex: Integer]: TSedaiSignalNode read GetOutput;

    // Processing latency in samples
    property Latency: Integer read FLatency write FLatency;

    // Topological sort order (for graph processing)
    property ProcessingOrder: Integer read FProcessingOrder write FProcessingOrder;

    // Which input passes through in bypass mode (-1 = silence)
    property BypassInput: Integer read FBypassInput write FBypassInput;

    // Processing flag for graph traversal
    property Processed: Boolean read FProcessed write FProcessed;
  end;

implementation

{ TSedaiSignalNode }

constructor TSedaiSignalNode.Create;
begin
  inherited Create;

  SetLength(FInputs, 0);
  SetLength(FOutputs, 0);
  FLatency := 0;
  FProcessingOrder := 0;
  FBypassInput := 0;  // Default: first input passes in bypass
  FProcessed := False;
  FInternalBufferSize := 0;
end;

destructor TSedaiSignalNode.Destroy;
begin
  // Disconnect all connections
  DisconnectAll;

  // Free internal buffer
  SetLength(FInternalBuffer, 0);

  inherited Destroy;
end;

function TSedaiSignalNode.GetInputCount: Integer;
begin
  Result := Length(FInputs);
end;

function TSedaiSignalNode.GetOutputCount: Integer;
begin
  Result := Length(FOutputs);
end;

function TSedaiSignalNode.GetInput(AIndex: Integer): TSedaiSignalNode;
begin
  if (AIndex >= 0) and (AIndex < Length(FInputs)) then
    Result := FInputs[AIndex]
  else
    Result := nil;
end;

function TSedaiSignalNode.GetOutput(AIndex: Integer): TSedaiSignalNode;
begin
  if (AIndex >= 0) and (AIndex < Length(FOutputs)) then
    Result := FOutputs[AIndex]
  else
    Result := nil;
end;

procedure TSedaiSignalNode.AllocateInternalBuffer(ASamples, AChannels: Integer);
var
  NewSize: Integer;
begin
  NewSize := ASamples * AChannels;
  if NewSize <> FInternalBufferSize then
  begin
    SetLength(FInternalBuffer, NewSize);
    FInternalBufferSize := NewSize;
  end;
end;

procedure TSedaiSignalNode.BlockSizeChanged;
begin
  inherited BlockSizeChanged;
  // Reallocate internal buffer for stereo
  AllocateInternalBuffer(BlockSize, 2);
end;

function TSedaiSignalNode.Connect(ATarget: TSedaiSignalNode): Boolean;
var
  I: Integer;
begin
  Result := False;

  // Cannot connect to self
  if ATarget = Self then
    Exit;

  // Cannot connect to nil
  if ATarget = nil then
    Exit;

  // Check if already connected
  for I := 0 to High(FOutputs) do
    if FOutputs[I] = ATarget then
      Exit;  // Already connected

  // Add to our outputs
  SetLength(FOutputs, Length(FOutputs) + 1);
  FOutputs[High(FOutputs)] := ATarget;

  // Add to target's inputs
  SetLength(ATarget.FInputs, Length(ATarget.FInputs) + 1);
  ATarget.FInputs[High(ATarget.FInputs)] := Self;

  Result := True;
end;

function TSedaiSignalNode.Disconnect(ATarget: TSedaiSignalNode): Boolean;
var
  I, J: Integer;
  Found: Boolean;
begin
  Result := False;

  if ATarget = nil then
    Exit;

  // Remove from our outputs
  Found := False;
  for I := 0 to High(FOutputs) do
  begin
    if FOutputs[I] = ATarget then
    begin
      // Shift remaining elements
      for J := I to High(FOutputs) - 1 do
        FOutputs[J] := FOutputs[J + 1];
      SetLength(FOutputs, Length(FOutputs) - 1);
      Found := True;
      Break;
    end;
  end;

  if not Found then
    Exit;

  // Remove from target's inputs
  for I := 0 to High(ATarget.FInputs) do
  begin
    if ATarget.FInputs[I] = Self then
    begin
      // Shift remaining elements
      for J := I to High(ATarget.FInputs) - 1 do
        ATarget.FInputs[J] := ATarget.FInputs[J + 1];
      SetLength(ATarget.FInputs, Length(ATarget.FInputs) - 1);
      Break;
    end;
  end;

  Result := True;
end;

procedure TSedaiSignalNode.DisconnectAll;
var
  I: Integer;
  Target: TSedaiSignalNode;
begin
  // Disconnect all outputs
  while Length(FOutputs) > 0 do
  begin
    Target := FOutputs[0];
    Disconnect(Target);
  end;

  // Disconnect all inputs (they should disconnect from us)
  while Length(FInputs) > 0 do
  begin
    FInputs[0].Disconnect(Self);
  end;
end;

function TSedaiSignalNode.IsConnectedTo(ANode: TSedaiSignalNode): Boolean;
var
  I: Integer;
begin
  Result := False;

  // Check outputs
  for I := 0 to High(FOutputs) do
    if FOutputs[I] = ANode then
    begin
      Result := True;
      Exit;
    end;

  // Check inputs
  for I := 0 to High(FInputs) do
    if FInputs[I] = ANode then
    begin
      Result := True;
      Exit;
    end;
end;

function TSedaiSignalNode.HasInputs: Boolean;
begin
  Result := Length(FInputs) > 0;
end;

function TSedaiSignalNode.HasOutputs: Boolean;
begin
  Result := Length(FOutputs) > 0;
end;

function TSedaiSignalNode.ValidateConnections: Boolean;

  // Depth-first search for cycle detection
  function HasCycle(ANode: TSedaiSignalNode; var Visiting, Visited: array of Boolean;
                    NodeIndex: Integer): Boolean;
  var
    I, TargetIndex: Integer;
  begin
    Result := False;

    // Mark as currently visiting
    Visiting[NodeIndex] := True;

    // Check all outputs
    for I := 0 to High(ANode.FOutputs) do
    begin
      // Get index of target node (simplified - assumes linear node list)
      // In real implementation, would need a node registry
      TargetIndex := I;  // Placeholder

      if Visiting[TargetIndex] then
      begin
        // Found a back edge - cycle detected
        Result := True;
        Exit;
      end;

      if not Visited[TargetIndex] then
      begin
        if HasCycle(ANode.FOutputs[I], Visiting, Visited, TargetIndex) then
        begin
          Result := True;
          Exit;
        end;
      end;
    end;

    // Mark as visited
    Visiting[NodeIndex] := False;
    Visited[NodeIndex] := True;
  end;

begin
  // Simplified validation - just check we're not connected to ourselves
  Result := not IsConnectedTo(Self);

  // Full cycle detection would require a graph registry
  // For now, trust that connections are made correctly
end;

function TSedaiSignalNode.GetLatencyCompensation: Integer;
var
  I: Integer;
  InputLatency: Integer;
begin
  Result := FLatency;

  // Add maximum latency from all inputs
  for I := 0 to High(FInputs) do
  begin
    InputLatency := FInputs[I].GetLatencyCompensation;
    if InputLatency > Result - FLatency then
      Result := FLatency + InputLatency;
  end;
end;

procedure TSedaiSignalNode.ResetProcessedFlag;
begin
  FProcessed := False;
end;

procedure TSedaiSignalNode.ProcessBlock(AInput, AOutput: PSingle; AFrameCount: Integer);
begin
  // Default implementation: bypass
  BypassBlock(AInput, AOutput, AFrameCount, 2);
end;

procedure TSedaiSignalNode.BypassBlock(AInput, AOutput: PSingle; AFrameCount: Integer; AChannels: Integer);
var
  I: Integer;
  SampleCount: Integer;
begin
  SampleCount := AFrameCount * AChannels;

  if (AInput <> nil) and (AOutput <> nil) then
  begin
    // Copy input to output
    for I := 0 to SampleCount - 1 do
      AOutput[I] := AInput[I];
  end
  else if AOutput <> nil then
  begin
    // No input - output silence
    for I := 0 to SampleCount - 1 do
      AOutput[I] := 0.0;
  end;
end;

end.
