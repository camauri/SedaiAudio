{ ============================================================================
  Unit: SedaiAudioSDL2Dyn (runtime-loaded SDL2 audio bindings)

  Purpose: let SedaiAudioFoundation call SDL2's audio API without creating a
  STATIC import of the SDL2 library in host executables. A static import
  makes the OS loader map SDL2 - and its dependency tree - on EVERY launch
  of the host (measured at several Mcycles), even for runs that never play
  a note. The audio DEVICE is already opened lazily; this makes the LIBRARY
  itself lazy too.

  Same pattern as the host's SedaiSDL2Dyn: procedural variables carrying the
  very same names/signatures as the static externals in sdl2.pas. A unit
  listing SedaiAudioSDL2Dyn AFTER SDL2 in its uses clause resolves the calls
  to these pointers (later unit wins); types/constants keep coming from
  sdl2.pas and cost no import. EnsureAudioSDL2Bound loads the library (via
  the binding's per-platform SDL_LibName) and binds everything, once; it
  must be called on the audio init entry points BEFORE any wrapped call -
  every teardown/queue path is behind a successful init, so binding there
  covers them all.

  ⚠️ If audio code starts calling an SDL function NOT declared below, the
  name silently resolves to the static external again and the import
  returns: after touching SDL calls here, check the host's import table
  (objdump -p sb.exe | grep -A 10 SDL2).
  ============================================================================ }

unit SedaiAudioSDL2Dyn;

{$mode objfpc}{$H+}

interface

uses
  ctypes, SDL2;

var
  { same names/signatures as the static externals in sdl2.pas }
  SDL_Init: function(flags: TSDL_Init): cint; cdecl;
  SDL_Quit: procedure; cdecl;
  SDL_InitSubSystem: function(flags: TSDL_Init): cint; cdecl;
  SDL_QuitSubSystem: procedure(flags: TSDL_Init); cdecl;
  SDL_GetError: function: PAnsiChar; cdecl;
  SDL_Delay: procedure(ms: cuint32); cdecl;
  SDL_OpenAudioDevice: function(device: PAnsiChar; iscapture: cint; desired: PSDL_AudioSpec;
                                obtained: PSDL_AudioSpec; allowed_changes: cint): TSDL_AudioDeviceID; cdecl;
  SDL_CloseAudioDevice: procedure(dev: TSDL_AudioDeviceID); cdecl;
  SDL_PauseAudioDevice: procedure(dev: TSDL_AudioDeviceID; pause_on: cint); cdecl;
  SDL_LockAudioDevice: procedure(dev: TSDL_AudioDeviceID); cdecl;
  SDL_UnlockAudioDevice: procedure(dev: TSDL_AudioDeviceID); cdecl;
  SDL_QueueAudio: function(dev: TSDL_AudioDeviceID; data: Pointer; len: cuint32): cint; cdecl;
  SDL_GetQueuedAudioSize: function(dev: TSDL_AudioDeviceID): cuint32; cdecl;
  SDL_ClearQueuedAudio: procedure(dev: TSDL_AudioDeviceID); cdecl;

  { Window and events. Not audio, but bound here for the same reason the audio
    calls are: a host must start and say something useful on a machine without
    SDL2, rather than fail to load. Used by the live tools to read a keyboard
    properly — a terminal reports key-down and never key-up, so a note can be
    struck but never held. }
  SDL_CreateWindow: function(title: PAnsiChar; x, y, w, h: cint;
                             flags: cuint32): PSDL_Window; cdecl;
  SDL_DestroyWindow: procedure(window: PSDL_Window); cdecl;
  SDL_RaiseWindow: procedure(window: PSDL_Window); cdecl;
  SDL_PollEvent: function(event: PSDL_Event): cint32; cdecl;

{ Load SDL2 and bind every pointer above. Idempotent and cheap after the first call.
  Returns False when the library cannot be loaded - callers treat that as "no audio". }
function EnsureAudioSDL2Bound: Boolean;

implementation

uses
  SysUtils, dynlibs;

var
  GSDL2: TLibHandle = NilHandle;
  GTried: Boolean = False;

function EnsureAudioSDL2Bound: Boolean;
begin
  if GTried then Exit(GSDL2 <> NilHandle);
  GTried := True;

  GSDL2 := LoadLibrary(SDL_LibName);
  if GSDL2 = NilHandle then Exit(False);

  Pointer(SDL_Init) := GetProcedureAddress(GSDL2, 'SDL_Init');
  Pointer(SDL_Quit) := GetProcedureAddress(GSDL2, 'SDL_Quit');
  Pointer(SDL_InitSubSystem) := GetProcedureAddress(GSDL2, 'SDL_InitSubSystem');
  Pointer(SDL_QuitSubSystem) := GetProcedureAddress(GSDL2, 'SDL_QuitSubSystem');
  Pointer(SDL_GetError) := GetProcedureAddress(GSDL2, 'SDL_GetError');
  Pointer(SDL_Delay) := GetProcedureAddress(GSDL2, 'SDL_Delay');
  Pointer(SDL_OpenAudioDevice) := GetProcedureAddress(GSDL2, 'SDL_OpenAudioDevice');
  Pointer(SDL_CloseAudioDevice) := GetProcedureAddress(GSDL2, 'SDL_CloseAudioDevice');
  Pointer(SDL_PauseAudioDevice) := GetProcedureAddress(GSDL2, 'SDL_PauseAudioDevice');
  Pointer(SDL_LockAudioDevice) := GetProcedureAddress(GSDL2, 'SDL_LockAudioDevice');
  Pointer(SDL_UnlockAudioDevice) := GetProcedureAddress(GSDL2, 'SDL_UnlockAudioDevice');
  Pointer(SDL_QueueAudio) := GetProcedureAddress(GSDL2, 'SDL_QueueAudio');
  Pointer(SDL_GetQueuedAudioSize) := GetProcedureAddress(GSDL2, 'SDL_GetQueuedAudioSize');
  Pointer(SDL_ClearQueuedAudio) := GetProcedureAddress(GSDL2, 'SDL_ClearQueuedAudio');

  Pointer(SDL_CreateWindow) := GetProcedureAddress(GSDL2, 'SDL_CreateWindow');
  Pointer(SDL_DestroyWindow) := GetProcedureAddress(GSDL2, 'SDL_DestroyWindow');
  Pointer(SDL_RaiseWindow) := GetProcedureAddress(GSDL2, 'SDL_RaiseWindow');
  Pointer(SDL_PollEvent) := GetProcedureAddress(GSDL2, 'SDL_PollEvent');

  Result := True;
end;

end.
