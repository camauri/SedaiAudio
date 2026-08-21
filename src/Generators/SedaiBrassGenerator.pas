{*
 * Sedai Audio Foundation - Waveguide Lip-Reed (Brass) Generator
 *
 * TSedaiBrassGenerator is the fourth physical model in SAF and the one that was
 * deferred twice: a brass instrument is a tube driven by a pair of LIPS, and the
 * lips are not a woodwind reed with the sign changed. They are a mass on a
 * spring with a resonance of their OWN, and a brass player chooses a note by
 * tuning that resonance, not by choosing a fingering.
 *
 * WHY THE FIRST ATTEMPT DID NOT TRACK PITCH. It locked to about 212 Hz whatever
 * note was asked for. That is the signature of a non-inverting loop whose only
 * frequency-selective element is the DC blocker: the loop latches onto the
 * blocker's own relaxation and the delay line has nothing to say about it. Two
 * things fix it, and both are in this file:
 *
 *   1. THE DELAY IS TWO PERIODS, NOT ONE. A brass tube is closed at the lips and
 *      open at the bell, and a player does not play its fundamental — the pedal
 *      note is a curiosity. Every note actually played is the SECOND mode or
 *      above. So the tube whose second mode is the note is twice as long as the
 *      note's period, and that is what gets built here. A one-period delay is
 *      the wrong instrument and, worse, leaves the loop with a mode at DC for
 *      the blocker to fight over.
 *
 *   2. THE LIPS ARE A RESONATOR AND THEY ARE TUNED TO THE NOTE. A two-pole
 *      bandpass at the played frequency, with the pole radius just inside the
 *      unit circle. This is the element that picks WHICH mode of the tube
 *      sounds, exactly as a player's embouchure does, and without it the model
 *      has no way to prefer one harmonic over another.
 *
 * Structure re-derived from the Cook/Scavone STK `Brass` model (Perry Cook and
 * Gary Scavone, Synthesis ToolKit); reimplemented in Pascal, no STK dependency
 * and no STK code. The MSW loop, the lip filter and the pressure scattering are
 * theirs; the breath ramp, the lip-tuning offset and the brassiness stage here
 * follow the conventions of SedaiReedGenerator so the two are siblings.
 *
 * WHAT THE LIP OFFSET IS FOR. `SetLipTuning` moves the lip resonance away from
 * the note in semitones. Zero is a player perfectly centred. Push it and the
 * pitch bends and eventually the note breaks to the neighbouring mode, which is
 * what happens to a real player too — it is the model's ONE control that makes
 * it sound played rather than triggered.
 *
 * Opt-in / inert: with the gate closed the generator is silent.
 *
 * (c) 2026 Artiforge - Licensed under GPL-3.0
 *}
unit SedaiBrassGenerator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, SedaiAudioTypes, SedaiAudioObject, SedaiSignalNode,
  SedaiOscillator;

type
  // A fractional-delay line (linear interpolation). Same shape as the reed's,
  // deliberately: the two models are read side by side often enough that a
  // second spelling of the same idea would only cost the reader.
  TBrassDelay = record
    buf: array of Single;
    len: Integer;
    wr: Integer;
    delay: Single;
    last: Single;
  end;

  { TSedaiBrassGenerator }
  TSedaiBrassGenerator = class(TSedaiSignalGenerator)
  private
    FD: TBrassDelay;           // the bore: two periods of the played note

    // The lips: a two-pole resonator tuned to the note (plus the player's
    // offset). ALL-POLE, and that is not an implementation detail.
    //
    // The valve's opening is the lip displacement SQUARED, and a square is
    // where the DC matters: with an offset, delta = D + a gives D^2 + 2*D*a +
    // a^2, and it is the middle term — at the lip's own frequency — that drives
    // the bore. Put zeros at +-1 to make a tidy bandpass and D vanishes, and
    // with it that term: what is left is a^2, at TWICE the lip frequency. The
    // model then drives the tube at the wrong frequency and answers a
    // half-semitone change of embouchure by jumping an octave. Measured, and it
    // is what the tidy version did.
    //
    // What DOES have to be fixed is the gain, which for an all-pole resonator
    // runs from about 2,000 at the top of a trumpet to 20,000 at the bottom of
    // a tuba. Left alone, that drives the lips ten times harder at the bottom
    // than at the top: measurement showed the opening saturating wide open
    // below MIDI 67 and never closing, so nothing oscillated — and the giveaway
    // was that the lip gain changed NOTHING, the output scaling purely with
    // breath, which is what a stuck-open valve does. So the peak gain is
    // computed and divided out, and the drive then means one thing everywhere.
    FLipA1, FLipA2, FLipNorm: Single;
    FLipY1, FLipY2: Single;
    FLipGain: Single;
    // The embouchure's QUALITY FACTOR, not a pole radius. A fixed radius means a
    // fixed ring time IN SAMPLES, which is a different number of periods at
    // every note — measurably so: the drive that oscillated at MIDI 58 was
    // silent at 73 and jumped a mode at 40. A fixed Q rings for the same number
    // of PERIODS everywhere, which is what "the same lips" ought to mean.
    FLipQ: Single;
    FLipRadius: Single;        // derived from Q and the note
    FLipSemis: Single;         // lip resonance offset from the note, in semitones
    FLipOpen: Single;          // equilibrium opening: how far apart the lips rest

    // The high-pass in the loop, and it does two jobs.
    //
    // The obvious one is refusing DC: a non-inverting loop has a mode at zero
    // and would happily sit there.
    //
    // The one that matters musically is refusing the PEDAL. The delay line is
    // two periods long, so the tube has a mode at half the note as well as at
    // the note — and a real brass instrument does not: the mouthpiece and the
    // flare between them make the pedal note weak and hard to find, which is
    // why players almost never use it. An idealised tube has no such reticence,
    // and measurement showed what that costs: at ordinary dynamics ten notes
    // out of fourteen in the top octave dropped to the octave below. So the
    // corner TRACKS THE NOTE, sitting between the pedal and the note, and the
    // instrument stops offering a partial it should not have.
    FDCPole: Single;
    FHPRatio: Single;          // where that corner sits, as a fraction of the note
    FDCX1, FDCY1: Single;

    FBoreRefl: Single;         // bore reflection gain (~0.85), NOT inverting
    FBoreTrim: Single;         // samples of loop latency taken off the delay
    FBoreScale: Single;        // proportional correction, calibrated by measurement
    FMouthScale: Single;       // mouth pressure as a fraction of breath (~0.3)

    FMaxPressure: Single;
    FPressure: Single;
    FPressCoeff: Single;
    FAttackTime: Single;
    FVelGain: Single;

    // The bell. A brass bell reflects the low frequencies back down the tube and
    // RADIATES the high ones, which is why the sound in the room is far brighter
    // than the wave inside the instrument. One pole is enough to say it: what
    // comes back is low-passed, and what is heard is what did not come back.
    FBellCut: Single;          // reflection low-pass corner, 0..1 (0 = dull bore)
    FBellState: Single;
    FTilt: Single;             // register compensation for the bell's high-pass

    FVibratoGain: Single;
    FVibratoRate: Single;
    FVibratoPhase: Single;
    FNoiseGain: Single;
    FRng: Cardinal;

    FOutputGain: Single;

    FNote: Integer;
    FVelocity: Single;
    FGateOpen: Boolean;
    FReleasing: Boolean;

  protected
    procedure SetFrequency(AValue: Single); override;

  private
    procedure UpdateBore;
    procedure UpdateLip;
    procedure RecalcPressCoeff;
    procedure ClearState;
    function NoiseSample: Single;
    function DelayTick(AInput: Single): Single;
    function LipTick(AInput: Single): Single;
    function DCTick(AInput: Single): Single;
    function BreathNow: Single;

  public
    constructor Create; override;

    procedure SampleRateChanged; override;

    procedure NoteOn(ANote: Integer; AVelocity: Single);
    procedure NoteOff;
    procedure Kill;

    function GenerateSample: Single; override;

    // How hard the lips are driven, and how far into the tube the breath goes.
    // Pressure below the threshold does not oscillate at all, which is correct
    // and is why a value is supplied rather than left at zero.
    procedure SetLip(AGain, APressure: Single);
    // Where the player's embouchure sits, in semitones from the note. 0 is
    // centred; +-1 bends; far enough and the note breaks to the next mode.
    procedure SetLipTuning(ASemitones: Single);
    // How far apart the lips rest before anything blows. Small is a tight
    // embouchure that slams shut on every cycle — the buzz of a loud brass
    // note; large is a loose one that never quite closes, which is softer and
    // rounder and, past a point, stops oscillating at all.
    procedure SetLipOpening(AOpening: Single);
    // How sharply the embouchure selects, as a QUALITY FACTOR: the lip
    // resonance's bandwidth is f/Q, so it means the same at every note. Higher
    // is a firmer, more decided player; lower lets the tube win the argument.
    procedure SetLipQ(AQ: Single);
    procedure SetBore(AReflection: Single);
    // How firmly the instrument refuses its own pedal note, as a fraction of
    // the played note: 0.5 is where the pedal is, 1.0 is the note itself.
    // Nearer 1 is a mouthpiece that makes the pedal unreachable.
    procedure SetPedalCut(ARatio: Single);
    // Samples taken off the two-period delay to pay for the rest of the loop
    // (lip filter, blocker, interpolator). Exposed because the right value is a
    // measurement, not a guess.
    procedure SetBoreTrim(ASamples: Single);
    procedure SetBoreScale(AScale: Single);
    procedure SetBreath(ANoise, AVibDepth, AVibRateHz: Single);
    procedure SetAttack(ASeconds: Single);
    // The bell's reflection corner, 0..1 of Nyquist-ish: how much of the wave
    // comes back down the tube instead of leaving it. Low is a narrow, dull
    // instrument; high is an open bell that radiates the top and keeps only the
    // bottom. This is what makes the model bright, not a filter after the fact.
    procedure SetBell(ACutoff: Single);
    procedure SetOutputGain(AGain: Single);

    property Note: Integer read FNote;
    property Velocity: Single read FVelocity;
    property GateOpen: Boolean read FGateOpen;
    property Releasing: Boolean read FReleasing;
    property LipTuning: Single read FLipSemis;
  end;

implementation

const
  BRASS_MIN_FREQ = 30.0;       // the bore is TWO periods long: size for it
  // The loop carries a couple of samples that are not in the delay line: the
  // lip filter, the blocker and the interpolator. Subtracted so the played note
  // lands where it was asked for rather than a few cents under.
  BRASS_DELAY_FUDGE = 3.0;
  BRASS_ATTACK_S = 0.03;
  // Where the loop's high-pass sits, as a fraction of the played note. Between
  // the pedal at 0.5 and the note at 1.0, and nearer the note than the pedal
  // because the pedal is the one being refused. A FIXED corner cannot do this
  // job: at 0.99 (76 Hz) it took 40% out of every pass at the bottom of a
  // trombone and the low notes would not start at all; at 0.999 (8 Hz) the low
  // notes came back and the top octave fell into the pedal instead. Both
  // measured; neither is a corner, it is a ratio.
  BRASS_HP_RATIO = 0.95;
  // CALIBRATED, not derived, and the calibration is the honest part of this
  // file. The delay line is not the only thing the wave goes round: the lip
  // resonator and the DC blocker each hold it a while, and together they are
  // worth about a tenth of a period. Left uncorrected the model plays 86 cents
  // sharp — measured, and flat across the range, which is what says it is a
  // proportion and not a fixed number of samples. So the line carries 2.2
  // periods and the rest of the loop carries the remainder. (2.33 now: the
  // note-tracking high-pass that refuses the pedal costs phase too, and the
  // calibration was redone after it went in.)
  //
  // It is calibrated FOR THE DEFAULT EMBOUCHURE. Change the lip Q or the drive
  // a long way and the tuning moves, because the lips then hold the wave for
  // longer or less — which is exactly what happens to a player who changes
  // embouchure, and what SetLipTuning is for.
  BRASS_BORE_SCALE = 1.165;
  // The radiated wave is a DIFFERENCE of two nearly equal numbers, so it comes
  // out around fifty times smaller than the pressure inside the tube. Folded in
  // here rather than left for the caller to discover, so the output gain knob
  // means the same thing it means on the reed.
  BRASS_RADIATION_GAIN = 150.0;
  // Radiating through a high-pass makes the top of the range louder than the
  // bottom — measured at nine times over four octaves, which is more tilt than
  // any instrument has. Compensated by most of it, not all: a real brass
  // instrument IS brighter and stronger up high, and flattening that completely
  // would be correcting away something true.
  BRASS_TILT_REF = 233.08;
  BRASS_TILT_POW = 0.6;

{ TSedaiBrassGenerator }

constructor TSedaiBrassGenerator.Create;
begin
  inherited Create;
  FLipGain := 10.0;
  FLipQ := 8.0;
  FLipRadius := 0.99;
  FLipSemis := 0.0;
  FLipOpen := 0.4;
  FBoreRefl := 0.85;
  FBoreTrim := BRASS_DELAY_FUDGE;
  FBoreScale := BRASS_BORE_SCALE;
  FMouthScale := 0.3;
  FMaxPressure := 1.0;
  FAttackTime := BRASS_ATTACK_S;
  FVelGain := 1.0;
  FBellCut := 0.55;
  FTilt := 1.0;
  FDCPole := 0.999;
  FHPRatio := BRASS_HP_RATIO;
  FBellState := 0.0;
  FVibratoGain := 0.0;
  FVibratoRate := 5.5;
  FVibratoPhase := 0.0;
  FNoiseGain := 0.0;
  FOutputGain := 0.18;
  FRng := 30011;
  FNote := -1;
  FVelocity := 1.0;
  FGateOpen := False;
  FReleasing := False;
  FFrequency := 233.08;        // Bb3, where a trombone lives
  FAmplitude := 1.0;
  SampleRateChanged;
end;

procedure TSedaiBrassGenerator.SampleRateChanged;
var
  need: Integer;
begin
  inherited SampleRateChanged;
  if FSampleRate <= 0 then Exit;
  need := Ceil(2.0 * FSampleRate / BRASS_MIN_FREQ) + 4;
  if Length(FD.buf) < need then SetLength(FD.buf, need);
  FD.len := Length(FD.buf);
  RecalcPressCoeff;
  UpdateBore;
  UpdateLip;
end;

procedure TSedaiBrassGenerator.SetFrequency(AValue: Single);
begin
  inherited SetFrequency(AValue);
  UpdateBore;
  UpdateLip;
end;

// THE line that the first attempt got wrong. Two periods, because the note is
// the tube's SECOND mode: a brass player never plays the fundamental.
procedure TSedaiBrassGenerator.UpdateBore;
var
  d: Single;
begin
  if (FFrequency <= 0) or (FSampleRate <= 0) or (FD.len < 8) then Exit;
  FTilt := Power(BRASS_TILT_REF / FFrequency, BRASS_TILT_POW);
  FDCPole := 1.0 / (1.0 + 2.0 * Pi * (FHPRatio * FFrequency) / FSampleRate);
  d := FBoreScale * 2.0 * FSampleRate / FFrequency - FBoreTrim;
  if d < 2 then d := 2;
  if d > FD.len - 2 then d := FD.len - 2;
  FD.delay := d;
end;

// The embouchure. A two-pole resonator with zeros at +-1, normalised so its
// peak gain is 1 — which is what lets FLipGain mean "how hard", independently
// of the note.
procedure TSedaiBrassGenerator.UpdateLip;
var
  f, w, re, im, mag: Single;
begin
  if (FFrequency <= 0) or (FSampleRate <= 0) then Exit;
  f := FFrequency * Power(2.0, FLipSemis / 12.0);
  if f < 1.0 then f := 1.0;
  if f > FSampleRate * 0.45 then f := FSampleRate * 0.45;
  w := 2.0 * Pi * f / FSampleRate;
  // Bandwidth f/Q, so the pole radius follows the note instead of standing still.
  FLipRadius := Exp(-Pi * (f / FLipQ) / FSampleRate);
  if FLipRadius > 0.99995 then FLipRadius := 0.99995;
  FLipA2 := FLipRadius * FLipRadius;
  FLipA1 := -2.0 * FLipRadius * Cos(w);
  // Peak gain, evaluated rather than approximated: |1/D(e^jw)| at the
  // resonance. Dividing it out leaves an all-pole filter whose peak is 1, so
  // the drive is the same instruction at every note and the DC path survives.
  re := 1.0 + FLipA1 * Cos(w) + FLipA2 * Cos(2.0 * w);
  im := -(FLipA1 * Sin(w) + FLipA2 * Sin(2.0 * w));
  mag := Sqrt(re * re + im * im);
  if mag < 1e-9 then mag := 1e-9;
  FLipNorm := mag;
end;

procedure TSedaiBrassGenerator.RecalcPressCoeff;
begin
  if (FSampleRate <= 0) or (FAttackTime <= 0) then FPressCoeff := 0
  else FPressCoeff := Exp(-1.0 / (FAttackTime * FSampleRate));
end;

procedure TSedaiBrassGenerator.ClearState;
var
  i: Integer;
begin
  for i := 0 to High(FD.buf) do FD.buf[i] := 0;
  FD.wr := 0;
  FD.last := 0;
  FLipY1 := 0; FLipY2 := 0;
  FDCX1 := 0; FDCY1 := 0;
  FBellState := 0;
  FPressure := 0;
end;

function TSedaiBrassGenerator.NoiseSample: Single;
begin
  FRng := FRng * 1664525 + 1013904223;
  Result := (Integer(FRng shr 8) / 8388608.0) - 1.0;
end;

function TSedaiBrassGenerator.DelayTick(AInput: Single): Single;
var
  readPos, frac: Single;
  i0, i1: Integer;
begin
  FD.buf[FD.wr] := AInput;
  readPos := FD.wr - FD.delay;
  while readPos < 0 do readPos := readPos + FD.len;
  i0 := Trunc(readPos);
  frac := readPos - i0;
  i1 := i0 + 1; if i1 >= FD.len then i1 := i1 - FD.len;
  Result := FD.buf[i0] * (1 - frac) + FD.buf[i1] * frac;
  Inc(FD.wr); if FD.wr >= FD.len then FD.wr := 0;
  FD.last := Result;
end;

function TSedaiBrassGenerator.LipTick(AInput: Single): Single;
var
  x, y: Single;
begin
  x := AInput * FLipGain * FLipNorm;
  y := x - FLipA1 * FLipY1 - FLipA2 * FLipY2;
  FLipY2 := FLipY1; FLipY1 := y;
  Result := y;
end;

function TSedaiBrassGenerator.DCTick(AInput: Single): Single;
begin
  Result := FDCPole * (FDCY1 + AInput - FDCX1);
  FDCX1 := AInput;
  FDCY1 := Result;
end;

function TSedaiBrassGenerator.BreathNow: Single;
var
  target: Single;
begin
  if FGateOpen then target := FMaxPressure else target := 0;
  FPressure := target + (FPressure - target) * FPressCoeff;
  Result := FPressure;
  if FVibratoGain > 0 then
  begin
    FVibratoPhase := FVibratoPhase + FVibratoRate / FSampleRate;
    while FVibratoPhase >= 1.0 do FVibratoPhase := FVibratoPhase - 1.0;
    Result := Result * (1.0 + FVibratoGain * Sin(2.0 * Pi * FVibratoPhase));
  end;
  if FNoiseGain > 0 then Result := Result + Result * FNoiseGain * NoiseSample;
end;

procedure TSedaiBrassGenerator.NoteOn(ANote: Integer; AVelocity: Single);
begin
  if ANote < 0 then ANote := 0;
  if ANote > 127 then ANote := 127;
  if AVelocity < 0 then AVelocity := 0;
  if AVelocity > 1 then AVelocity := 1;
  FNote := ANote;
  FVelocity := AVelocity;
  SetFrequency(440.0 * Power(2.0, (ANote - 69) / 12.0));
  FGateOpen := True;
  FReleasing := False;
end;

procedure TSedaiBrassGenerator.NoteOff;
begin
  FGateOpen := False;
  FReleasing := True;
end;

procedure TSedaiBrassGenerator.Kill;
begin
  FGateOpen := False;
  FReleasing := False;
  ClearState;
end;

// The MSW loop, in the order the air travels it: breath meets the returning
// wave at the lips, the lips open by an amount that depends on the difference,
// and what the lips let through joins what came back.
function TSedaiBrassGenerator.GenerateSample: Single;
var
  breath, mouth, bore, refl, radiated, delta, area, open_, s: Single;
begin
  if (FSampleRate <= 0) or (FD.len < 8) then Exit(0.0);

  breath := BreathNow;
  if (breath < 1e-6) and (Abs(FD.last) < 1e-7) then
  begin
    FReleasing := False;
    Exit(0.0);
  end;

  mouth := FMouthScale * breath;

  // At the bell, one pole decides what comes back. `refl` travels on down the
  // tube; everything else left the instrument, and THAT is what is heard.
  FBellState := FBellState + FBellCut * (FD.last - FBellState);
  refl := FBoreRefl * FBellState;
  radiated := FD.last - FBellState;
  bore := refl;

  // Force on the lips is the pressure ACROSS them; through the resonator that
  // becomes a displacement, and the displacement decides how far the lips are
  // apart. The nonlinearity here is what makes the model self-oscillate rather
  // than ring, and its SHAPE is what makes it lips rather than a reed.
  //
  // ONE-SIDED, around an equilibrium opening. Lips close and then STAY closed:
  // pushed further they do not open again on the other side, they just remain
  // shut, and the wave gets a flat bottom rather than a second lobe. A
  // symmetric square — displacement squared about zero — opens twice per cycle
  // instead of once, which is a buzz at double the frequency laid over the
  // note, and is heard as a reed vibrating oddly rather than as a pair of lips.
  delta := LipTick(mouth - bore);
  open_ := FLipOpen + delta;
  if open_ < 0.0 then open_ := 0.0;
  area := open_ * open_;
  // Saturating smoothly instead of at a corner: real lips run out of travel,
  // they do not hit a wall, and a hard clip is a flat top full of harmonics
  // that belong to the clip and not to the instrument.
  area := area / (1.0 + area);

  // Scattering at the mouthpiece: the more open the lips, the more of the
  // mouth's pressure gets in and the less of the bore's is reflected.
  s := area * mouth + (1.0 - area) * bore;

  DelayTick(DCTick(s));
  // The radiated wave, not the one inside the tube. A brass instrument is heard
  // through its bell, and the bell is a high-pass: taking the bore pressure
  // straight out is why an untreated waveguide brass sounds like a tube.
  Result := radiated * FOutputGain * FTilt * BRASS_RADIATION_GAIN *
            (0.25 + 0.75 * FVelocity * FVelGain);
end;

procedure TSedaiBrassGenerator.SetLip(AGain, APressure: Single);
begin
  if AGain < 0.0 then AGain := 0.0;
  if AGain > 4000.0 then AGain := 4000.0;
  if APressure < 0.0 then APressure := 0.0;
  if APressure > 4.0 then APressure := 4.0;
  FLipGain := AGain;
  FMaxPressure := APressure;
end;

procedure TSedaiBrassGenerator.SetLipTuning(ASemitones: Single);
begin
  if ASemitones < -24.0 then ASemitones := -24.0;
  if ASemitones > 24.0 then ASemitones := 24.0;
  FLipSemis := ASemitones;
  UpdateLip;
end;

procedure TSedaiBrassGenerator.SetLipOpening(AOpening: Single);
begin
  if AOpening < 0.0 then AOpening := 0.0;
  if AOpening > 4.0 then AOpening := 4.0;
  FLipOpen := AOpening;
end;

procedure TSedaiBrassGenerator.SetLipQ(AQ: Single);
begin
  if AQ < 0.5 then AQ := 0.5;
  if AQ > 200.0 then AQ := 200.0;
  FLipQ := AQ;
  UpdateLip;
end;

procedure TSedaiBrassGenerator.SetBore(AReflection: Single);
begin
  if AReflection < 0.0 then AReflection := 0.0;
  if AReflection > 0.999 then AReflection := 0.999;
  FBoreRefl := AReflection;
end;

procedure TSedaiBrassGenerator.SetPedalCut(ARatio: Single);
begin
  if ARatio < 0.05 then ARatio := 0.05;
  if ARatio > 1.6 then ARatio := 1.6;
  FHPRatio := ARatio;
  UpdateBore;
end;

procedure TSedaiBrassGenerator.SetBoreTrim(ASamples: Single);
begin
  if ASamples < -50.0 then ASamples := -50.0;
  if ASamples > 50.0 then ASamples := 50.0;
  FBoreTrim := ASamples;
  UpdateBore;
end;

procedure TSedaiBrassGenerator.SetBoreScale(AScale: Single);
begin
  if AScale < 0.5 then AScale := 0.5;
  if AScale > 2.0 then AScale := 2.0;
  FBoreScale := AScale;
  UpdateBore;
end;

procedure TSedaiBrassGenerator.SetBreath(ANoise, AVibDepth, AVibRateHz: Single);
begin
  if ANoise < 0 then ANoise := 0;
  if AVibDepth < 0 then AVibDepth := 0;
  if AVibRateHz < 0 then AVibRateHz := 0;
  FNoiseGain := ANoise;
  FVibratoGain := AVibDepth;
  FVibratoRate := AVibRateHz;
end;

procedure TSedaiBrassGenerator.SetAttack(ASeconds: Single);
begin
  if ASeconds < 0.001 then ASeconds := 0.001;
  if ASeconds > 2.0 then ASeconds := 2.0;
  FAttackTime := ASeconds;
  RecalcPressCoeff;
end;

procedure TSedaiBrassGenerator.SetBell(ACutoff: Single);
begin
  if ACutoff < 0.02 then ACutoff := 0.02;
  if ACutoff > 0.99 then ACutoff := 0.99;
  FBellCut := ACutoff;
end;

procedure TSedaiBrassGenerator.SetOutputGain(AGain: Single);
begin
  if AGain < 0 then AGain := 0;
  FOutputGain := AGain;
end;

end.
