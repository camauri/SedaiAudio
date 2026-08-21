'' Una FAMIGLIA di strumenti, non una trascrizione.
''
'' Qui non c'e' niente di rigido: il numero di oscillatori e' un NUMERO, la
'' scordatura si calcola, la quota nel mix si calcola, e un figlio cambia una
'' sola giuntura senza sapere cosa ha deciso il padre. Il .patch che esce e'
'' piatto e stupido, come deve essere — l'intelligenza sta qui.
'' ⚠️ Gli oscillatori si costruiscono E si cablano DENTRO la stessa
'' iterazione, e Src(i) non viene piu' riletto dopo il ciclo: un puntatore
'' assegnato dentro un blocco non sopravvive all'assegnazione successiva
'' (vedi README).
#include "library/instruments/saf.bas"

Type MoogVoice Extends Object
  Public:
    Nm       As String
    Count    As Integer          '' quanti oscillatori: un numero, non un sottotipo
    Detune   As Double           '' centesimi, distribuiti fra loro
    BaseFreq As Double
    Kbd  As Note Ptr
    Src(0 To 15) As Osc Ptr
    Filt As Filter Ptr
    AEnv As Env Ptr
    Vamp As Amp Ptr
    Declare Virtual Sub BuildSources()
    Declare Virtual Sub BuildFilter()
    Declare Virtual Sub BuildAmp()
    Declare Sub Build()
End Type

Constructor MoogVoice()
  This.Nm       = "moog.voice"
  This.Count    = 2
  This.Detune   = 10.0
  This.BaseFreq = 55.0
End Constructor

'' Spezzato lungo le giunture dove qualcuno vorra' intervenire.
Sub MoogVoice.Build()
  This.Kbd  = New Note   : This.Kbd->Init("note")
  This.Filt = New Filter : This.Filt->Init("filt")
  This.AEnv = New Env    : This.AEnv->Init("aenv")
  This.Vamp = New Amp    : This.Vamp->Init("amp")
  This.BuildSources()
  This.BuildFilter()
  This.BuildAmp()
  SafOutput("amp.out")
End Sub

Sub MoogVoice.BuildSources()
  Dim As Integer i
  Dim As Double cents, share
  '' la quota si calcola da se': un figlio che aggiunge un oscillatore non deve
  '' sapere cosa aveva deciso il padre
  share = 1.0 / This.Count
  For i = 0 To This.Count - 1
    cents = 0.0
    If This.Count > 1 Then cents = This.Detune * (i / (This.Count - 1) - 0.5)
    This.Src(i) = New Osc
    This.Src(i)->Init("osc" + Str(i))
    This.Src(i)->Key("shape", "saw")
    This.Src(i)->KeyN("freq", This.BaseFreq)
    This.Src(i)->Pitch.Value = cents / 1200.0      '' volt per ottava
    This.Src(i)->Pitch.Add(This.Kbd->Pitch)
    This.Filt->In.Add(This.Src(i)->Out, share)
  Next
End Sub

Sub MoogVoice.BuildFilter()
  This.Filt->Key("mode", "lowpass")
  This.Filt->KeyN("cutoff", 520.0)
  This.Filt->Res.Value = 0.60
End Sub

Sub MoogVoice.BuildAmp()
  This.AEnv->KeyN("a", 0.004) : This.AEnv->KeyN("d", 0.220)
  This.AEnv->KeyN("s", 0.0)   : This.AEnv->KeyN("r", 0.150)
  This.AEnv->Gate.Add(This.Kbd->Gate)
  This.Vamp->In.Add(This.Filt->Out)
  This.Vamp->Gain.Add(This.AEnv->Out, 0.90)
End Sub

'' Un basso: cambia UNA giuntura, l'inviluppo sul cutoff, che e' tutta
'' l'articolazione. Non disfa niente del padre.
Type MoogBass Extends MoogVoice
  Public:
    FEnv As Env Ptr
    Declare Override Sub BuildFilter()
End Type

Constructor MoogBass()
  This.Nm       = "moog.bass"
  This.Count    = 3
  This.Detune   = 8.0
  This.BaseFreq = 55.0
End Constructor

Sub MoogBass.BuildFilter()
  Base.BuildFilter()
  This.FEnv = New Env : This.FEnv->Init("fenv")
  This.FEnv->KeyN("a", 0.001) : This.FEnv->KeyN("d", 0.34)
  This.FEnv->KeyN("s", 0.10)  : This.FEnv->KeyN("r", 0.12)
  This.FEnv->Gate.Add(This.Kbd->Gate)
  This.Filt->Cutoff.Add(This.FEnv->Out, 2.30)
End Sub

Dim As MoogVoice Ptr v
v = New MoogBass
safTitle = "Minimoog bass, generato: " + Str(v->Count) + " oscillatori scordati di " + Str(v->Detune) + " centesimi"
safVoices = 6
v->Build()
SafEmit()
