# ENPC eduroam reference

The ENPC NetworkManager profile is named `eduroam ENPC` and is retained as a fallback, with autoconnect disabled. IWD has only one `eduroam.8021x` slot; the system feature provisions UPJV there, not ENPC.

If switching later, the previously working ENPC settings were PEAP, inner MSCHAPV2, identity `nicolas.graves@enpc.fr`, server domain `radet.enpc.fr`, and the HARICA TLS RSA Root CA 2021. The password is in `passage`; do not put it in this repository or a Guix store file. Reconfiguring the system restores the UPJV IWD profile.
