'' Un organo a nove tiranti.
''
'' Il punto di questo file: le nove barre sono un CICLO, non nove blocchi
'' copiati. In un .patch scritto a mano sarebbero quarantacinque righe da tenere
'' allineate; qui cambiare la registrazione e' cambiare nove numeri in una riga.
''
'' ⚠️ Una variabile non puo' chiamarsi come un TIPO della libreria: `note`,
'' `env` e `amp` sono tipi, quindi la tastiera qui si chiama `kbd` e
'' l'amplificatore `vamp`. Il traduttore automatico premette `m_` per lo
'' stesso motivo.
#include "library/instruments/saf.bas"

Dim As Note Ptr kbd
Dim As Env Ptr  aenv
Dim As Amp Ptr  vamp
Dim As Osc Ptr  bar(0 To 8)

'' i nove rapporti armonici di un organo a tiranti, la sub-ottava per prima
Dim As Double ratio(0 To 8) = {0.5, 1.5, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 8.0}
'' 888000000 — la registrazione classica, piena di corpo
Dim As Double level(0 To 8) = {0.8, 0.8, 0.8, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0}

safTitle = "Organo a tiranti: nove sinusoidi, nessun filtro. Il timbro E' la miscela."
safVoices = 8

kbd  = New Note : kbd->Init("note")
vamp = New Amp  : vamp->Init("amp")
aenv = New Env  : aenv->Init("aenv")
aenv->KeyN("a", 0.004) : aenv->KeyN("d", 0.0)
aenv->KeyN("s", 1.0)   : aenv->KeyN("r", 0.01)

Dim As Integer i
For i = 0 To 8
  '' una barra a zero non si costruisce nemmeno: il .patch resta pulito
  If level(i) > 0.0 Then
    bar(i) = New Osc
    bar(i)->Init("bar" + Str(i))
    bar(i)->Key("shape", "sine")
    bar(i)->KeyN("freq", 110.0 * ratio(i))
    bar(i)->Pitch.Add(kbd->Pitch)
    vamp->In.Add(bar(i)->Out, level(i))
  End If
Next

aenv->Gate.Add(kbd->Gate)
vamp->Gain.Add(aenv->Out, 0.9)

SafOutput("amp.out")
SafEmit()
