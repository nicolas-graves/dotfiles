(define-module (system connections)
  #:use-module (system connections-utils)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services configuration)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix build utils))

(define-public services
  (cons*
   (service
    system-connections-service-type
    (system-connections-configuration
     (config
      `((,(getenv "ID_2c8a22d9_8845_4c37_abdc_53d8b4def1f5")
          ((connection
                       ((id . ,(getenv "ID_2c8a22d9_8845_4c37_abdc_53d8b4def1f5"))
                        (uuid . "2c8a22d9-8845-4c37-abdc-53d8b4def1f5")
                        (type . wifi)))
           (wifi
                 ((mode . infrastructure)
                  (seen-bssids . "6C:38:A1:03:F6:28;")
                  (ssid . ,(getenv "ID_2c8a22d9_8845_4c37_abdc_53d8b4def1f5"))))
           (wifi-security
                          ((key-mgmt . wpa-psk)
                           (psk-flags . 1)))
           (ipv4 ((method . auto)))
           (ipv6 ((addr-gen-mode . stable-privacy)
                    (method . auto)))))
         (,(getenv "ID_48d6ad3c_a415_4f46_b4db_6722f30d6ce4")
          ((connection
                       ((id . ,(getenv "ID_48d6ad3c_a415_4f46_b4db_6722f30d6ce4"))
                        (uuid . "48d6ad3c-a415-4f46-b4db-6722f30d6ce4")
                        (type . wifi)))
           (wifi
                 ((mode . infrastructure)
                  (ssid . ,(getenv "ID_48d6ad3c_a415_4f46_b4db_6722f30d6ce4"))))
           (wifi-security
                          ((auth-alg . open)
                           (key-mgmt . wpa-psk)
                           (psk . ,(getenv "PSK_48d6ad3c_a415_4f46_b4db_6722f30d6ce4"))))
           (ipv4 ((method . auto)))
           (ipv6 ((addr-gen-mode . stable-privacy)
                    (method . auto)))))
         (,(getenv "ID_6aa57df9_ce9f_4fa7_a0de_c13f9120b392")
          ((connection
                       ((id . ,(getenv "ID_6aa57df9_ce9f_4fa7_a0de_c13f9120b392"))
                        (uuid . "6aa57df9-ce9f-4fa7-a0de-c13f9120b392")
                        (type . wifi)))
           (wifi
                 ((mode . infrastructure)
                  (ssid . ,(getenv "ID_6aa57df9_ce9f_4fa7_a0de_c13f9120b392"))))
           (wifi-security
                          ((auth-alg . open)
                           (key-mgmt . wpa-psk)
                           (psk . ,(getenv "PSK_6aa57df9_ce9f_4fa7_a0de_c13f9120b392"))))
           (ipv4 ((method . auto)))
           (ipv6 ((addr-gen-mode . stable-privacy)
                    (method . auto))))
          )
         (,(getenv "ID_100deaa3_5775_46f2_ba53_1641889f5934")
          ((connection
                       ((id . ,(getenv "ID_100deaa3_5775_46f2_ba53_1641889f5934"))
                        (uuid . "100deaa3-5775-46f2-ba53-1641889f5934")
                        (type . wifi)))
           (wifi
                 ((mode . infrastructure)
                  (ssid . ,(getenv "ID_100deaa3_5775_46f2_ba53_1641889f5934"))))
           (wifi-security
                          ((auth-alg . open)
                           (key-mgmt . wpa-psk)
                           (psk . ,(getenv "PSK_100deaa3_5775_46f2_ba53_1641889f5934"))))
           (ipv4 ((method . auto)))
           (ipv6 ((addr-gen-mode . stable-privacy)
                    (method . auto)))))
         (,(getenv "ID_9f73c581_611a_4bef_9832_8e9e644e362e")
          ((connection
                       ((id . ,(getenv "ID_9f73c581_611a_4bef_9832_8e9e644e362e"))
                        (uuid . "9f73c581-611a-4bef-9832-8e9e644e362e")
                        (type . ethernet)))
           ;; (ethernet . (#~""))
           (ipv4
                 ((may-fail . false)
                  (method . auto)))
           (ipv6
                 ((addr-gen-mode . stable-privacy)
                  (method . disabled)))))
         (,(getenv "ID_dfacb629_a107_4714_a4f6_7d6bf2e661f0")
          ((connection
                       ((id . ,(getenv "ID_dfacb629_a107_4714_a4f6_7d6bf2e661f0"))
                        (uuid . "dfacb629-a107-4714-a4f6-7d6bf2e661f0")
                        (type . ethernet)))
           ;; (ethernet . (#~""))
           ("802-1x"
                   ((eap . "ttls;")
                    (identity . ,(getenv "IDENTITY_dfacb629_a107_4714_a4f6_7d6bf2e661f0"))
                    (password . ,(getenv "PASS_dfacb629_a107_4714_a4f6_7d6bf2e661f0"))
                    (phase2-autheap . mschapv2)))
           (ipv4 ((method . auto)))
           (ipv6 ((addr-gen-mode . stable-privacy)
                    (method . auto)))))
         (,(getenv "ID_8f03eb94_be5c_4d44_a6f7_f2c8290d4552")
          ((connection
                       ((id . ,(getenv "ID_8f03eb94_be5c_4d44_a6f7_f2c8290d4552"))
                        (uuid . "8f03eb94-be5c-4d44-a6f7-f2c8290d4552")
                        (type . wifi)))
           (wifi
                 ((mode . infrastructure)
                  (ssid . ,(getenv "ID_8f03eb94_be5c_4d44_a6f7_f2c8290d4552"))))
           (ipv4 ((method . auto)))
           (ipv6 ((addr-gen-mode . stable-privacy)
                    (method . auto)))))
         (,(getenv "ID_dfb8c014_f4a0_4484_ac0e_48a5f7ad1b28")
          ((connection
                       ((id . ,(getenv "ID_dfb8c014_f4a0_4484_ac0e_48a5f7ad1b28"))
                        (uuid . "dfb8c014-f4a0-4484-ac0e-48a5f7ad1b28")
                        (type . wifi)
                        (interface-name . wlp2s0)))
           (wifi
                 ((mode . infrastructure)
                  (ssid . ,(getenv "ID_dfb8c014_f4a0_4484_ac0e_48a5f7ad1b28"))))
           (wifi-security
                          ((auth-alg . open)
                           (key-mgmt . wpa-psk)
                           (psk . ,(getenv "PSK_dfb8c014_f4a0_4484_ac0e_48a5f7ad1b28"))))
           (ipv4 ((method . auto)))
           (ipv6 ((addr-gen-mode . stable-privacy)
                    (method . auto)))))
         (,(getenv "ID_61201506_ff48_4e37_9089_083bfb0384b0")
          ((connection
                       ((id . ,(getenv "ID_61201506_ff48_4e37_9089_083bfb0384b0"))
                        (uuid . "61201506-ff48-4e37-9089-083bfb0384b0")
                        (type . wifi)))
           (wifi
                 ((mode . infrastructure)
                  (ssid . ,(getenv "ID_61201506_ff48_4e37_9089_083bfb0384b0"))))
           (wifi-security ((key-mgmt . wpa-eap)))
           ("802-1x"
                   ((eap . "peap;")
                    (identity . ,(getenv "IDENTITY_61201506_ff48_4e37_9089_083bfb0384b0"))
                    (password . ,(getenv "PASS_61201506_ff48_4e37_9089_083bfb0384b0"))
                    (phase2-auth . mschapv2)))
           (ipv4 ((method . auto)))
           (ipv6 ((addr-gen-mode . stable-privacy)
                    (method . auto)))))
         (,(getenv "ID_76db17b3_2394_43e5_b6a2_2f43cce96f7f")
          ((connection
                       ((id . ,(getenv "ID_76db17b3_2394_43e5_b6a2_2f43cce96f7f"))
                        (uuid . "76db17b3-2394-43e5-b6a2-2f43cce96f7f")
                        (type . ethernet)
                        (autoconnect . false)))
           (ethernet ((mac-address . ,(getenv "MAC_76db17b3_2394_43e5_b6a2_2f43cce96f7f"))))
           (ipv4
                 ((address1 . "192.168.66.66/24")
                  (dns-priority . 100)
                  (method . shared)))
           (ipv6
                 ((addr-gen-mode . stable-privacy)
                  (method . ignore)))))
         (,(getenv "ID_a8c5c86f_544b_4069_b239_d222924f4399")
          ((connection
                       ((id . ,(getenv "ID_a8c5c86f_544b_4069_b239_d222924f4399"))
                        (uuid . "a8c5c86f-544b-4069-b239-d222924f4399")
                        (type . wifi)))
           (wifi
                 ((mode . infrastructure)
                  (seen-bssids . "B0:B2:8F:F7:5A:C0;")
                  (ssid . ,(getenv "ID_a8c5c86f_544b_4069_b239_d222924f4399"))))
           (wifi-security
                          ((key-mgmt . wpa-psk)
                           (psk-flags . 1)))
           (ipv4 ((method . auto)))
           (ipv6 ((addr-gen-mode . stable-privacy)
                    (method . auto)))))
         (,(getenv "ID_c4da4633_20c3_4b07_b3fe_1bbd45fbd4a8")
          ((connection
                       ((id . ,(getenv "ID_c4da4633_20c3_4b07_b3fe_1bbd45fbd4a8"))
                        (uuid . "c4da4633-20c3-4b07-b3fe-1bbd45fbd4a8")
                        (type . wifi)))
           (wifi
                 ((mode . infrastructure)
                  (seen-bssids . "56:03:31:1D:4C:98;E4:9E:12:DF:52:C8;")
                  (ssid . ,(getenv "ID_c4da4633_20c3_4b07_b3fe_1bbd45fbd4a8"))))
           (wifi-security
                          ((key-mgmt . wpa-psk)
                           (psk-flags . 1)))
           (ipv4 ((method . auto)))
           (ipv6 ((addr-gen-mode . stable-privacy)
                    (method . auto)))))
         (,(getenv "ID_e525ce2a_05f7_45d0_9cce_22ca44d9eaac")
          ((connection
                       ((id . ,(getenv "ID_e525ce2a_05f7_45d0_9cce_22ca44d9eaac"))
                        (uuid . "e525ce2a-05f7-45d0-9cce-22ca44d9eaac")
                        (type . wifi)))
           (wifi
                 ((mode . infrastructure)
                  (ssid . ,(getenv "ID_e525ce2a_05f7_45d0_9cce_22ca44d9eaac"))))
           (wifi-security
                          (( auth-alg . open)
                           (key-mgmt . wpa-psk)
                           (psk . ,(getenv "PSK_e525ce2a_05f7_45d0_9cce_22ca44d9eaac"))))
           (ipv4 ((method . auto)))
           (ipv6 ((addr-gen-mode . stable-privacy)
                    (method . auto)))))
         (,(getenv "ID_49d7ccb9_e56f_49b5_8b4d_bf154ccf03a4")
          ((connection
                       ((id . ,(getenv "ID_49d7ccb9_e56f_49b5_8b4d_bf154ccf03a4"))
                        (uuid . "49d7ccb9-e56f-49b5-8b4d-bf154ccf03a4")
                        (type . wifi)))
           (wifi
                 ((mode . infrastructure)
                  (seen-bssids . "02:3C:9B:B3:39:8C;9A:75:C7:A0:68:32;")
                  (ssid . ,(getenv "ID_49d7ccb9_e56f_49b5_8b4d_bf154ccf03a4"))))
           (wifi-security
                          ((key-mgmt . wpa-psk)
                           (psk-flags . 1)))
           (ipv4 ((method . auto)))
           (ipv6 ((addr-gen-mode . stable-privacy)
                    (method . auto)))))
         (,(getenv "ID_2aa8f21b_ce79_42f9_8475_82c1f3f6a091")
          ((connection
                       ((id . ,(getenv "ID_2aa8f21b_ce79_42f9_8475_82c1f3f6a091"))
                        (uuid . "2aa8f21b-ce79-42f9-8475-82c1f3f6a091")
                        (type . wifi)))
           (wifi
                 ((mode . infrastructure)
                  (ssid . ,(getenv "ID_2aa8f21b_ce79_42f9_8475_82c1f3f6a091"))))
           (wifi-security
                          ((auth-alg . open)
                           (key-mgmt . wpa-psk)
                           (psk . ,(getenv "PSK_2aa8f21b_ce79_42f9_8475_82c1f3f6a091"))))
           (ipv4 ((method . auto)))
           (ipv6 ((addr-gen-mode . stable-privacy)
                    (method . auto)))))
         (,(getenv "ID_9dceec52_08b0_4b60_8254_0cfb386d8e19")
          ((connection
                       ((id . ,(getenv "ID_9dceec52_08b0_4b60_8254_0cfb386d8e19"))
                        (uuid . "9dceec52-08b0-4b60-8254-0cfb386d8e19")
                        (type . wifi)))
           (wifi
                 ((mode . infrastructure)
                  (ssid . ,(getenv "ID_9dceec52_08b0_4b60_8254_0cfb386d8e19"))))
           (wifi-security
	                  ((auth-alg . open)
                           (key-mgmt . wpa-psk)
                           (psk . ,(getenv "PSK_9dceec52_08b0_4b60_8254_0cfb386d8e19"))))
           (ipv4 ((method . auto)))
           (ipv6 ((addr-gen-mode . stable-privacy)
                    (method . auto)))))
         (,(getenv "ID_dba6f528_451f_440e_953b_c9d2ebae61d4")
          ((connection
	               ((id . ,(getenv "ID_dba6f528_451f_440e_953b_c9d2ebae61d4"))
                        (uuid . "dba6f528-451f-440e-953b-c9d2ebae61d4")
                        (type . wifi)))
           (wifi
                 ((mode . infrastructure)
                  (seen-bssids . "78:94:B4:DC:16:30;")
                  (ssid . ,(getenv "ID_dba6f528_451f_440e_953b_c9d2ebae61d4"))))
           (wifi-security
                          ((key-mgmt . wpa-psk)
                           (psk . ,(getenv "PSK_dba6f528_451f_440e_953b_c9d2ebae61d4"))))
           (ipv4 ((method . auto)))
           (ipv6 ((addr-gen-mode . stable-privacy)
                    (method . auto)))))
         (,(getenv "ID_b525f3dd_d9f3_45cf_b822_7ea42b902198")
          ((connection
	               ((id . ,(getenv "ID_b525f3dd_d9f3_45cf_b822_7ea42b902198"))
                        (uuid . "b525f3dd-d9f3-45cf-b822-7ea42b902198")
                        (type . wifi)))
           (wifi
                 ((mode . infrastructure)
                  (seen-bssids . "44:A6:1E:4D:74:B7;")
                  (ssid . ,(getenv "ID_b525f3dd_d9f3_45cf_b822_7ea42b902198"))))
           (wifi-security
	                  ((key-mgmt . wpa-psk)
                           (psk-flags . 1)))
           (ipv4 ((method . auto)))
           (ipv6 ((addr-gen-mode . stable-privacy)
                    (method . auto)))))
         (,(getenv "ID_53d43f11_72c3_4443_b9d3_ab28bb490826")
          ((connection
	               ((id . ,(getenv "ID_53d43f11_72c3_4443_b9d3_ab28bb490826"))
                        (uuid . "53d43f11-72c3-4443-b9d3-ab28bb490826")
                        (type . wifi)))
           (wifi
	         ((mode . infrastructure)
                  (seen-bssids . "44:A6:1E:07:9C:C3;")
                  (ssid . ,(getenv "ID_53d43f11_72c3_4443_b9d3_ab28bb490826"))))
           (wifi-security
	                  ((key-mgmt . wpa-psk)
                           (psk-flags . 1)))
           (ipv4 ((method . auto)))
           (ipv6 ((addr-gen-mode . stable-privacy)
                    (method . auto)))))
         (,(getenv "ID_7db52c41_bec4_4763_977d_873e07377fc3")
          (  (connection
	                 ((id . ,(getenv "ID_7db52c41_bec4_4763_977d_873e07377fc3"))
                          (uuid . "7db52c41-bec4-4763-977d-873e07377fc3")
                          (type . wifi)))
             (wifi
	           ((mode . infrastructure)
                    (ssid . ,(getenv "ID_7db52c41_bec4_4763_977d_873e07377fc3"))))
             (wifi-security
	                    ((auth-alg . open)
                             (key-mgmt . wpa-psk)
                             (psk . ,(getenv "PSK_7db52c41_bec4_4763_977d_873e07377fc3"))))
             (ipv4 ((method . auto)))
             (ipv6 ((addr-gen-mode . stable-privacy)
                      (method . auto)))
             ))
         (,(getenv "ID_85a2f17b_39f4_4ff9_8914_0b175a266913")
          ((connection
	               ((id . ,(getenv "ID_85a2f17b_39f4_4ff9_8914_0b175a266913"))
                        (uuid . "85a2f17b-39f4-4ff9-8914-0b175a266913")
                        (type . wifi)
                        (autoconnect . false)))
           (wifi
	         ((mode . infrastructure)
                  (ssid . ,(getenv "ID_85a2f17b_39f4_4ff9_8914_0b175a266913"))))
           (wifi-security
                          ((auth-alg . open)
                           (key-mgmt . wpa-psk)
                           (psk . ,(getenv "PSK_85a2f17b_39f4_4ff9_8914_0b175a266913"))))
           (ipv4 ((method . auto)))
           (ipv6 ((addr-gen-mode . stable-privacy)
                    (method . auto)))))
         (,(getenv "ID_e7d0c5aa_92fe_4686_bd54_0bc447ddf775")
          ((connection
	               ((id . ,(getenv "ID_e7d0c5aa_92fe_4686_bd54_0bc447ddf775"))
                        (uuid . "e7d0c5aa-92fe-4686-bd54-0bc447ddf775")
                        (type . wifi)
                        (autoconnect . false)))
           (wifi
	         ((mode . infrastructure)
                  (ssid . ,(getenv "ID_e7d0c5aa_92fe_4686_bd54_0bc447ddf775"))))
           (wifi-security ((key-mgmt . wpa-eap)))
           ("802-1x"
	           ((eap . "ttls;")
                    (identity . ,(getenv "IDENTITY_e7d0c5aa_92fe_4686_bd54_0bc447ddf775"))
                    (password . ,(getenv "PASS_e7d0c5aa_92fe_4686_bd54_0bc447ddf775"))
                    (phase2-auth . mschapv2)))
           (ipv4 ((method . auto)))
           (ipv6 ((addr-gen-mode . stable-privacy)
                    (method . auto)))))
         (,(getenv "ID_92c2cea4_f8c1_4ff3_a71d_9512309a09ba")
          ((connection
                       ((id . ,(getenv "ID_92c2cea4_f8c1_4ff3_a71d_9512309a09ba"))
                        (uuid . "92c2cea4-f8c1-4ff3-a71d-9512309a09ba")
                        (type . wifi)))
           (wifi
                 ((mode . infrastructure)
                  (seen-bssids . "C0:10:B1:0B:1F:AC;EE:F3:18:BA:16:7D;")
                  (ssid . ,(getenv "ID_92c2cea4_f8c1_4ff3_a71d_9512309a09ba"))))
           (wifi-security
	                  ((key-mgmt . wpa-psk)
                           (psk . ,(getenv "PSK_92c2cea4_f8c1_4ff3_a71d_9512309a09ba"))))
           (ipv4 ((method . auto)))
           (ipv6 ((addr-gen-mode . stable-privacy)
                    (method . auto)))))
         (,(getenv "ID_a15a6d2f_627f_4ee5_9754_294fa1f7cd9d")
          ((connection
                       ((id . ,(getenv "ID_a15a6d2f_627f_4ee5_9754_294fa1f7cd9d"))
                        (uuid . "a15a6d2f-627f-4ee5-9754-294fa1f7cd9d")
                        (type . wifi)))
           (wifi
                 ((mode . infrastructure)
                  (ssid . ,(getenv "ID_a15a6d2f_627f_4ee5_9754_294fa1f7cd9d"))))
           (ipv4 ((method . auto)))
           (ipv6 ((addr-gen-mode . stable-privacy)
                    (method . auto)))))
         (,(getenv "ID_95f259c3_80d3_490c_a4f7_08987a46a1ff")
          ((connection
                       ((id . ,(getenv "ID_95f259c3_80d3_490c_a4f7_08987a46a1ff"))
                        (uuid . "95f259c3-80d3-490c-a4f7-08987a46a1ff")
                        (type . wifi)))
           (wifi
                 ((mode . infrastructure)
                  (seen-bssids . "E0:CE:C3:D1:45:EC;")
                  (ssid . ,(getenv "ID_95f259c3_80d3_490c_a4f7_08987a46a1ff"))))
           (wifi-security
	                  ((key-mgmt . wpa-psk)
                           (psk-flags . 1)))
           (ipv4 ((method . auto)))
           (ipv6 ((addr-gen-mode . stable-privacy)
                    (method . auto)))))
         (,(getenv "ID_8213663d_a88f_430c_804f_916e97238692")
          ((connection
	               ((id . ,(getenv "ID_8213663d_a88f_430c_804f_916e97238692"))
                        (uuid . "8213663d-a88f-430c-804f-916e97238692")
                        (type . wifi)))
           (wifi
                 ((mode . infrastructure)
                  (ssid . ,(getenv "ID_8213663d_a88f_430c_804f_916e97238692"))))
           (wifi-security
	                  ((auth-alg . open)
                           (key-mgmt . wpa-psk)
                           (psk . ,(getenv "PSK_8213663d_a88f_430c_804f_916e97238692"))))
           (ipv4 ((method . auto)))
           (ipv6 ((addr-gen-mode . stable-privacy)
                    (method . auto)))))
         (,(getenv "ID_8f03eb94_be5c_4d44_a6f7_f2c8290d4552")
          ((connection
                       ((id . ,(getenv "ID_8f03eb94_be5c_4d44_a6f7_f2c8290d4552"))
                        (uuid . "8f03eb94-be5c-4d44-a6f7-f2c8290d4552")
                        (type . wifi)))
           (wifi
                 ((mode . infrastructure)
                  (ssid . ,(getenv "ID_8f03eb94_be5c_4d44_a6f7_f2c8290d4552"))))
           (wifi-security ((key-mgmt . wpa-eap)))
           ("802-1x"
                   ((eap . "ttls;")
                    (identity . ,(getenv "IDENTITY_8f03eb94_be5c_4d44_a6f7_f2c8290d4552"))
                    (password . ,(getenv "PASS_8f03eb94_be5c_4d44_a6f7_f2c8290d4552"))
                    (phase2-auth . mschapv2)))
           (ipv4 ((method . auto)))
           (ipv6 ((addr-gen-mode . stable-privacy)
                    (method . auto)))))
         (,(getenv "ID_683a3c4d_9d63_444f_819d_91f9ad512cdc")
          ((connection
                       ((id . ,(getenv "ID_683a3c4d_9d63_444f_819d_91f9ad512cdc"))
                        (uuid . "683a3c4d-9d63-444f-819d-91f9ad512cdc")
                        (type . wifi)))
           (wifi
                 ((mode . infrastructure)
                  (ssid . ,(getenv "ID_683a3c4d_9d63_444f_819d_91f9ad512cdc"))))
           (wifi-security
                          ((key-mgmt . wpa-psk)
                           (psk . ,(getenv "PSK_683a3c4d_9d63_444f_819d_91f9ad512cdc"))))
           (ipv4 ((method . auto)))
           (ipv6 ((addr-gen-mode . stable-privacy)
                    (method . auto)))))
         (,(getenv "ID_6be2746c_812f_4779_ba57_6f28de5ba145")
          ((connection
                       ((id . ,(getenv "ID_6be2746c_812f_4779_ba57_6f28de5ba145"))
                        (uuid . "6be2746c-812f-4779-ba57-6f28de5ba145")
                        (type . wifi)))
           (wifi
                 ((mode . infrastructure)
                  (seen-bssids . "CC:2D:1B:3C:5C:86;")
                  (ssid . ,(getenv "ID_6be2746c_812f_4779_ba57_6f28de5ba145"))))
           (wifi-security
                          ((key-mgmt . wpa-psk)
                           (psk-flags . 1)))
           (ipv4 ((method . auto)))
           (ipv6 ((addr-gen-mode . stable-privacy)
                    (method . auto)))))
         (,(getenv "ID_9cf97c0d_e5f4_4e52_a532_b4acbb1cf492")
          ((connection
                       ((id . ,(getenv "ID_9cf97c0d_e5f4_4e52_a532_b4acbb1cf492"))
                        (uuid . "9cf97c0d-e5f4-4e52-a532-b4acbb1cf492")
                        (type . wifi)))
           (wifi
	         ((mode . infrastructure)
                  (ssid . ,(getenv "ID_9cf97c0d_e5f4_4e52_a532_b4acbb1cf492"))))
           (ipv4 ((method . auto)))
           (ipv6 ((addr-gen-mode . stable-privacy)
                    (method . auto)))))
         (,(getenv "ID_62d31516_d06a_4a2b_b240_5b39866eace8")
          ((connection
	               ((id . ,(getenv "ID_62d31516_d06a_4a2b_b240_5b39866eace8"))
                        (uuid . "62d31516-d06a-4a2b-b240-5b39866eace8")
                        (type . wifi)))
           (wifi
                 ((mode . infrastructure)
                  (ssid . ,(getenv "ID_62d31516_d06a_4a2b_b240_5b39866eace8"))))
           (wifi-security
                          ((auth-alg . open)
                           (key-mgmt . wpa-psk)
                           (psk . ,(getenv "PSK_62d31516_d06a_4a2b_b240_5b39866eace8"))))
           (ipv4 ((method . auto)))
           (ipv6 ((addr-gen-mode . stable-privacy)
                    (method . auto)))))
         (,(getenv "ID_d5d5ed62_5a95_47e9_8c80_2aba90d8cab1")
          ((connection
                       ((id . ,(getenv "ID_d5d5ed62_5a95_47e9_8c80_2aba90d8cab1"))
                        (uuid . "d5d5ed62-5a95-47e9-8c80-2aba90d8cab1")
                        (type . wifi)))
           (wifi
                 ((mode . infrastructure)
                  (ssid . ,(getenv "ID_d5d5ed62_5a95_47e9_8c80_2aba90d8cab1"))))
           (wifi-security
                          ((auth-alg . open)
                           (key-mgmt . wpa-psk)
                           (psk . ,(getenv "PSK_d5d5ed62_5a95_47e9_8c80_2aba90d8cab1"))))
           (ipv4 ((method . auto)))
           (ipv6 ((addr-gen-mode . stable-privacy)
                    (method . auto)))))))
     ))))
