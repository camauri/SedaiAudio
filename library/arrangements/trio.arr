# Un piccolo assieme su un palco, visti da chi sta seduto in mezzo alla sala.
#
# Un .patch dice cosa E' uno strumento. Questo dice DOVE STA e quanto pesa
# contro gli altri: sono cose dell'arrangiamento, non dello strumento, e uno
# strumento che avesse gia' deciso di stare "a sinistra" non si potrebbe piu'
# girare.
#
# Le coordinate sono in metri, con -Z davanti all'ascoltatore.

rate    48000
listen  at=0,1.2,0  facing=0,0,-1

part    bass  = ../patches/moog_bass.patch   channel=2  voices=4
part    brass = ../patches/moog_brass.patch  channel=3  voices=6
part    lead  = ../patches/moog_lead.patch   channel=5  voices=1
part    pad   = ../patches/poly.patch         channel=4  voices=6

place   bass  at=-2.5,0.4,4.0
place   brass at=0.0,1.0,5.5
place   lead  at=2.2,1.0,3.5
place   pad   at=-0.8,1.4,7.0

gain    bass  = 0.9
gain    brass = 0.8
gain    lead  = 0.7
gain    pad   = 0.6
