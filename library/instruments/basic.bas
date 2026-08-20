'' basic.patch — lifted from the .patch by patch_bas.
'' Round trip:  sb <this> > out.patch  must render identically.
#include "library/instruments/saf.bas"

Dim As Note Ptr m_note
Dim As Osc Ptr m_osc1
Dim As Filter Ptr m_filt
Dim As Env Ptr m_env1
Dim As Amp Ptr m_amp
Dim As Ring Ptr m_vvca

m_note = New Note : m_note->Init("note")
m_osc1 = New Osc : m_osc1->Init("osc1")
m_filt = New Filter : m_filt->Init("filt")
m_env1 = New Env : m_env1->Init("env1")
m_amp = New Amp : m_amp->Init("amp")
m_vvca = New Ring : m_vvca->Init("vvca")

m_osc1->Key("shape", "saw")
m_osc1->Key("freq", "110")
m_filt->Key("mode", "lowpass")
m_filt->Key("cutoff", "800")
m_env1->Key("a", "0.005")
m_env1->Key("d", "0.15")
m_env1->Key("s", "0.6")
m_env1->Key("r", "0.3")
m_filt->Res.Value = 0.4
m_vvca->B.Value = 0.25
m_vvca->B.Add(m_note->Vel, 0.75)
m_vvca->A.Add(m_env1->Out)
m_filt->Cutoff.Value = -1.5
m_filt->Cutoff.Add(m_note->Vel, 1.5)
m_osc1->Pitch.Add(m_note->Pitch)
m_env1->Gate.Add(m_note->Gate)
m_filt->In.Add(m_osc1->Out)
m_amp->In.Add(m_filt->Out)
m_amp->Gain.Add(m_vvca->Out, 0.3)
SafOutput("amp.out")

SafEmit()
