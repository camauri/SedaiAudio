{*
 * Sedai Audio Foundation - Compressed Audio Decoder Interface
 *
 * TSedaiAudioDecoder is the common abstract base for the streaming, pure
 * Pascal decoders of the compressed formats (FLAC, MP3, OGG Vorbis). Each
 * concrete decoder reads from a TStream it does NOT own (the owning
 * TSedaiAudioFileReader keeps and frees the stream) and produces interleaved
 * float frames at its native channel count, matching the WAV/AIFF paths.
 *
 * (c) 2024 Artiforge - Licensed under GPL-3.0
 *}
unit SedaiAudioDecoder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  { TSedaiAudioDecoder }
  // Abstract base for compressed-format streaming decoders. The decoder does
  // not own AStream; ReadFrames yields interleaved float samples (FChannels
  // per frame). Position is tracked in whole sample frames.
  TSedaiAudioDecoder = class
  protected
    FStream: TStream;            // borrowed, not owned
    FSampleRate: Integer;
    FChannels: Integer;
    FBitsPerSample: Integer;     // source bit depth (for scaling/reporting)
    FTotalFrames: Int64;         // 0 if unknown (e.g. streamed)
    FPosition: Int64;            // next frame index to be returned
    FSeekable: Boolean;
    FLastError: string;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    // Bind to a stream (positioned at start) and parse headers. Returns False
    // and sets LastError on failure. The decoder never frees AStream.
    function OpenStream(AStream: TStream): Boolean; virtual; abstract;

    // Decode up to AFrameCount frames into ABuffer (interleaved, Channels per
    // frame). Returns the number of frames actually produced (0 at EOF).
    function ReadFrames(ABuffer: PSingle; AFrameCount: Integer): Integer; virtual; abstract;

    // Seek to absolute frame AFrame. Returns False if not seekable / failed.
    function Seek(AFrame: Int64): Boolean; virtual; abstract;

    property SampleRate: Integer read FSampleRate;
    property Channels: Integer read FChannels;
    property BitsPerSample: Integer read FBitsPerSample;
    property TotalFrames: Int64 read FTotalFrames;
    property Position: Int64 read FPosition;
    property Seekable: Boolean read FSeekable;
    property LastError: string read FLastError;
  end;

implementation

constructor TSedaiAudioDecoder.Create;
begin
  inherited Create;
  FStream := nil;
  FSampleRate := 0;
  FChannels := 0;
  FBitsPerSample := 0;
  FTotalFrames := 0;
  FPosition := 0;
  FSeekable := False;
  FLastError := '';
end;

destructor TSedaiAudioDecoder.Destroy;
begin
  // FStream is borrowed; never freed here.
  inherited Destroy;
end;

end.
