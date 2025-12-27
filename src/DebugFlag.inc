//
// Sedai Audio Foundation - Debug Configuration
// Copyright (C) 2025 Maurizio Cammalleri
//
// This file controls debug output for the entire library.
// Include this file at the beginning of each unit after the mode directive.
//
// Usage:
//   Include DebugFlag.inc
//
// Available defines:
//   SEDAI_DEBUG        - Master debug switch (enables all debug output)
//   SEDAI_DEBUG_AUDIO  - Audio system debug (callbacks, buffer processing)
//   SEDAI_DEBUG_MIDI   - MIDI system debug (parsing, sequencing, playback)
//   SEDAI_DEBUG_SYNTH  - Synthesis debug (voice allocation, processing)
//   SEDAI_DEBUG_LOADER - Wavetable loader debug (file loading, cache)
//
// To enable debug output, uncomment the desired defines below.
// To disable, comment them out (default state).
//

// ============================================================================
// MASTER DEBUG SWITCH
// ============================================================================
// Uncomment to enable ALL debug output
{.$DEFINE SEDAI_DEBUG}

// ============================================================================
// INDIVIDUAL DEBUG SWITCHES
// ============================================================================
// These can be enabled independently for targeted debugging

// Audio system: callbacks, buffer processing, SDL interaction
// {$DEFINE SEDAI_DEBUG_AUDIO}

// MIDI system: file parsing, sequencing, note events, timing
// {$DEFINE SEDAI_DEBUG_MIDI}

// Synthesis: voice allocation, ADSR, processors
// {$DEFINE SEDAI_DEBUG_SYNTH}

// Wavetable loader: file loading, format detection, cache
// {$DEFINE SEDAI_DEBUG_LOADER}

// ============================================================================
// AUTOMATIC PROPAGATION
// ============================================================================
// If master switch is enabled, enable all individual switches
{$IFDEF SEDAI_DEBUG}
  {$DEFINE SEDAI_DEBUG_AUDIO}
  {$DEFINE SEDAI_DEBUG_MIDI}
  {$DEFINE SEDAI_DEBUG_SYNTH}
  {$DEFINE SEDAI_DEBUG_LOADER}
{$ENDIF}
