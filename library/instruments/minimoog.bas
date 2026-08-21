'' I tre Minimoog come UNA FAMIGLIA.
''
'' Fino a ieri erano tre file .patch che ripetevano la stessa architettura --
'' tre oscillatori, due filtri in cascata, due inviluppi, il blocco della
'' velocity -- e differivano in tredici numeri e tre presenze opzionali. Qui la
'' struttura e' scritta una volta e i tre sono quei numeri.
''
'' Il criterio di riuscita non e' che "suoni bene": e' che le patch generate
'' rendano IDENTICHE, campione per campione, a quelle scritte a mano.
''
''   sb library/instruments/minimoog.bas bass  > library/patches/moog_bass.patch
''
'' ⚠️ LA CREAZIONE DEI MODULI STA TUTTA IN Build, E DEVE RESTARCI. Misurato su
'' sb 2.0: un campo puntatore assegnato dentro un metodo VIRTUALE viene
'' sovrascritto quando un metodo virtuale DIVERSO assegna un altro campo
'' puntatore — tutti finiscono per puntare all'ultimo oggetto creato. Assegnare
'' da un metodo non virtuale e' sicuro, e assegnarli tutti dentro UN solo metodo
'' virtuale pure. Percio' qui Build alloca e le giunture cablano soltanto: un
'' figlio che ridefinisce BuildFilter cambia il filtro, non lo alloca.
''
'' L'ORDINE DELLE CHIAMATE CONTA. Un ingresso somma le sue sorgenti nell'ordine
'' in cui sono dichiarate, quindi la velocity sul cutoff deve venire prima del
'' contour dell'inviluppo: solo cosi' -1.0 + 1.0 fa esattamente zero a velocity
'' piena e lo strumento resta identico a com'era.
#include "library/instruments/saf.bas"

Type MoogVoice Extends Object
  Public:
    '' --- i parametri: e' qui che i tre differiscono ---
    Nm        As String
    BaseFreq  As Double        '' altezza degli oscillatori 1 e 2
    Freq3     As Double        '' il terzo: sub-ottava sul basso e sul lead
    Shape3    As String
    Pw3       As Double        '' larghezza d'impulso, se Shape3 e' pulse
    Det2      As Double        '' scordatura in volt: il battimento e' il corpo
    Det3      As Double
    Cutoff    As Double
    Res1      As Double
    Res2      As Double
    Contour   As Double        '' quanto l'inviluppo apre il filtro
    Mix1      As Double
    Mix2      As Double
    Mix3      As Double
    AmpGain   As Double
    FA As Double : FD As Double : FS As Double : FR As Double
    AA As Double : AD As Double : AS_ As Double : AR As Double
    HasGlide  As Integer
    GlideTime As Double
    HasVib    As Integer
    VibRate   As Double
    VibDepth  As Double
    HasDrive  As Integer
    DriveAmt  As Double
    DriveTone As Double
    DriveMix  As Double
    '' 0 = nessuna riga `voices`: la polifonia la decide chi suona. Le tre
    '' patch scritte a mano non la dichiarano, e questo deve restare vero.
    Voices    As Integer

    '' --- i moduli ---
    Kbd  As Note Ptr
    Gld  As Glide Ptr
    O1   As Osc Ptr
    O2   As Osc Ptr
    O3   As Osc Ptr
    Flt1 As Filter Ptr
    Flt2 As Filter Ptr
    FEnv As Env Ptr
    AEnv As Env Ptr
    Vib  As Lfo Ptr
    Vamp As Amp Ptr
    Drv  As Sdist Ptr
    Vvca As Ring Ptr

    Declare Virtual Sub BuildKeyboard()
    Declare Virtual Sub BuildSources()
    Declare Virtual Sub BuildFilter()
    Declare Virtual Sub BuildAmp()
    Declare Sub Build()
End Type

Constructor MoogVoice()
  This.Nm = "moog.voice"
  This.Shape3 = "saw" : This.Pw3 = 0.0
  This.HasGlide = 0 : This.HasVib = 0 : This.HasDrive = 0
  This.Voices = 0
End Constructor

Sub MoogVoice.Build()
  safVoices = This.Voices

  '' --- allocazione, tutta qui e in quest'ordine ---
  '' ⚠️ OGNI `New` STA FUORI DA QUALSIASI BLOCCO. Misurato su sb 2.0: un campo
  '' puntatore assegnato dentro un If (o dentro un For, o in un metodo virtuale
  '' diverso dagli altri) viene sovrascritto dall'assegnazione successiva, e
  '' finisce per puntare all'ultimo oggetto creato. Percio' i moduli opzionali
  '' si allocano sempre e solo la DICHIARAZIONE e' condizionale: un oggetto mai
  '' Init-ato non compare nella patch, e costa una manciata di byte.
  This.Kbd = New Note : This.Kbd->Init("note")
  This.Gld = New Glide
  If This.HasGlide <> 0 Then This.Gld->Init("gl")
  This.O1 = New Osc : This.O1->Init("osc1")
  This.O2 = New Osc : This.O2->Init("osc2")
  This.O3 = New Osc : This.O3->Init("osc3")
  This.Flt1 = New Filter : This.Flt1->Init("f1")
  This.Flt2 = New Filter : This.Flt2->Init("f2")
  This.FEnv = New Env : This.FEnv->Init("fenv")
  This.AEnv = New Env : This.AEnv->Init("aenv")
  This.Vib = New Lfo
  If This.HasVib <> 0 Then This.Vib->Init("vib")
  This.Vamp = New Amp : This.Vamp->Init("amp")
  This.Drv = New Sdist
  If This.HasDrive <> 0 Then This.Drv->Init("drv")
  This.Vvca = New Ring : This.Vvca->Init("vvca")

  '' --- e le giunture, che ora cablano soltanto ---
  This.BuildKeyboard()
  This.BuildSources()
  This.BuildFilter()
  This.BuildAmp()
  SafOutput("amp.out")
End Sub

'' La tastiera, e il portamento se questo strumento ne ha uno.
Sub MoogVoice.BuildKeyboard()
  If This.HasGlide <> 0 Then
    This.Gld->KeyN("time", This.GlideTime)
    This.Gld->In.Add(This.Kbd->Pitch)
  End If
End Sub

'' Tre oscillatori scordati, sommati sul primo filtro. Le quote sono parametri
'' e non 1/3: su un Minimoog il bilanciamento fra i tre E' parte del suono.
Sub MoogVoice.BuildSources()
  This.O1->Key("shape", "saw") : This.O1->KeyN("freq", This.BaseFreq)
  This.O2->Key("shape", "saw") : This.O2->KeyN("freq", This.BaseFreq)
  This.O3->Key("shape", This.Shape3) : This.O3->KeyN("freq", This.Freq3)
End Sub

Sub MoogVoice.BuildFilter()
  This.Flt1->Key("mode", "lowpass")
  This.Flt1->KeyN("cutoff", This.Cutoff) : This.Flt1->KeyN("res", This.Res1)
  This.Flt2->Key("mode", "lowpass")
  This.Flt2->KeyN("cutoff", This.Cutoff) : This.Flt2->KeyN("res", This.Res2)
End Sub

Sub MoogVoice.BuildAmp()
  This.FEnv->KeyN("a", This.FA) : This.FEnv->KeyN("d", This.FD)
  This.FEnv->KeyN("s", This.FS) : This.FEnv->KeyN("r", This.FR)
  This.AEnv->KeyN("a", This.AA) : This.AEnv->KeyN("d", This.AD)
  This.AEnv->KeyN("s", This.AS_) : This.AEnv->KeyN("r", This.AR)

  If This.HasVib <> 0 Then
    This.Vib->Key("shape", "sine") : This.Vib->KeyN("rate", This.VibRate)
  End If
  If This.HasDrive <> 0 Then
    This.Drv->KeyN("drive", This.DriveAmt) : This.Drv->KeyN("tone", This.DriveTone)
    This.Drv->Mix.Value = This.DriveMix
  End If

  '' --- velocity: PRIMA di tutto il resto sui cutoff ---
  This.Vvca->B.Value = 0.25
  This.Vvca->B.Add(This.Kbd->Vel, 0.75)
  This.Vvca->A.Add(This.AEnv->Out)
  This.Flt1->Cutoff.Value = -1.0
  This.Flt1->Cutoff.Add(This.Kbd->Vel, 1.0)
  This.Flt2->Cutoff.Value = -1.0
  This.Flt2->Cutoff.Add(This.Kbd->Vel, 1.0)

  '' --- la tastiera agli oscillatori, diretta o attraverso il portamento ---
  If This.HasGlide <> 0 Then
    This.O1->Pitch.Add(This.Gld->Out)
    This.O2->Pitch.Add(This.Gld->Out)
    This.O3->Pitch.Add(This.Gld->Out)
  Else
    This.O1->Pitch.Add(This.Kbd->Pitch)
    This.O2->Pitch.Add(This.Kbd->Pitch)
    This.O3->Pitch.Add(This.Kbd->Pitch)
  End If
  If This.Det2 <> 0.0 Then This.O2->Pitch.Value = This.Det2
  If This.Det3 <> 0.0 Then This.O3->Pitch.Value = This.Det3
  If This.Pw3 <> 0.0 Then This.O3->Pw.Value = This.Pw3
  If This.HasVib <> 0 Then
    This.O1->Pitch.Add(This.Vib->Out, This.VibDepth)
    This.O2->Pitch.Add(This.Vib->Out, This.VibDepth)
  End If

  This.Flt1->In.Add(This.O1->Out, This.Mix1)
  This.Flt1->In.Add(This.O2->Out, This.Mix2)
  This.Flt1->In.Add(This.O3->Out, This.Mix3)

  This.FEnv->Gate.Add(This.Kbd->Gate)
  This.AEnv->Gate.Add(This.Kbd->Gate)
  This.Flt1->Cutoff.Add(This.FEnv->Out, This.Contour)
  This.Flt2->Cutoff.Add(This.FEnv->Out, This.Contour)
  This.Flt2->In.Add(This.Flt1->Out)

  If This.HasDrive <> 0 Then
    This.Drv->In.Add(This.Flt2->Out)
    This.Vamp->In.Add(This.Drv->Out)
  Else
    This.Vamp->In.Add(This.Flt2->Out)
  End If
  This.Vamp->Gain.Add(This.Vvca->Out, This.AmpGain)
End Sub

'' ---------------------------------------------------------------------------
'' I tre. Ognuno e' un costruttore: dichiara i parametri e basta, la struttura
'' la realizza Build. Nessuno di loro ridefinisce una giuntura, perche' non ne
'' hanno bisogno -- differiscono in numeri, e i numeri stanno nel costruttore.
'' ---------------------------------------------------------------------------

Type MoogBass Extends MoogVoice
End Type

Constructor MoogBass()
  This.Nm = "moog.bass"
  This.BaseFreq = 55.0
  This.Freq3 = 27.5 : This.Shape3 = "square"     '' la sub-ottava e' il peso
  This.Det2 = 0.0026                             '' ~4.5 cent sopra
  This.Cutoff = 230.0 : This.Res1 = 0.62 : This.Res2 = 0.30
  This.Contour = 2.30                            '' il "contour amount"
  This.Mix1 = 0.42 : This.Mix2 = 0.38 : This.Mix3 = 0.30
  This.AmpGain = 0.26
  This.FA = 0.001 : This.FD = 0.34 : This.FS = 0.10 : This.FR = 0.12
  This.AA = 0.002 : This.AD = 0.60 : This.AS_ = 0.55 : This.AR = 0.14
  This.HasGlide = 1 : This.GlideTime = 0.006     '' corto: e' articolazione
  This.HasDrive = 1
  This.DriveAmt = 1.6 : This.DriveTone = 0.45 : This.DriveMix = 0.35
End Constructor

Type MoogBrass Extends MoogVoice
End Type

Constructor MoogBrass()
  This.Nm = "moog.brass"
  This.BaseFreq = 110.0
  This.Freq3 = 110.0 : This.Shape3 = "pulse" : This.Pw3 = 0.32
  This.Det2 = 0.0035
  This.Cutoff = 380.0 : This.Res1 = 0.42 : This.Res2 = 0.22
  This.Contour = 2.75                            '' l'attacco lento e' il fiato
  This.Mix1 = 0.38 : This.Mix2 = 0.36 : This.Mix3 = 0.26
  This.AmpGain = 0.24
  This.FA = 0.075 : This.FD = 0.45 : This.FS = 0.55 : This.FR = 0.22
  This.AA = 0.020 : This.AD = 0.20 : This.AS_ = 0.90 : This.AR = 0.25
End Constructor

Type MoogLead Extends MoogVoice
End Type

Constructor MoogLead()
  This.Nm = "moog.lead"
  This.BaseFreq = 220.0
  This.Freq3 = 110.0 : This.Shape3 = "saw"
  This.Det2 = 0.0042 : This.Det3 = -0.0030
  This.Cutoff = 520.0 : This.Res1 = 0.55 : This.Res2 = 0.28
  This.Contour = 2.60
  This.Mix1 = 0.36 : This.Mix2 = 0.34 : This.Mix3 = 0.30
  This.AmpGain = 0.24
  This.FA = 0.004 : This.FD = 0.55 : This.FS = 0.42 : This.FR = 0.25
  This.AA = 0.008 : This.AD = 0.30 : This.AS_ = 0.85 : This.AR = 0.28
  This.HasGlide = 1 : This.GlideTime = 0.045     '' lungo: meta' della firma
  This.HasVib = 1 : This.VibRate = 5.2 : This.VibDepth = 0.0022
End Constructor

'' ---------------------------------------------------------------------------
'' Quale dei tre, dalla riga di comando.
'' ---------------------------------------------------------------------------
Dim As MoogVoice Ptr v
Dim As String which
which = Command(1)
If which = "" Then which = "bass"

If which = "bass" Then
  v = New MoogBass
  safTitle = "MINIMOOG — il basso. Sub-ottava quadra, filtro stretto, un filo di drive."
ElseIf which = "brass" Then
  v = New MoogBrass
  safTitle = "MINIMOOG — gli ottoni. L'attacco lento sul cutoff E' il fiato."
ElseIf which = "lead" Then
  v = New MoogLead
  safTitle = "MINIMOOG — il lead. Portamento lungo e un vibrato appena percettibile."
Else
  Print "#ERRORE: uso: sb minimoog.bas [bass|brass|lead]"
  End
End If

v->Build()
SafEmit()
