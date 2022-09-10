(define-module (dots services vnstat)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages admin)
  #:use-module (dots packages vnstat)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (guix modules)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module ((guix store) #:select (text-file))
  #:use-module ((guix utils) #:select (version-major))
  #:use-module ((guix packages) #:select (package-version))
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (gnu home services utils)
  #:export (vnstat-configuration
            vnstat-configuration?
            vnstat-configuration-name
            vnstat-configuration-day-format
            vnstat-configuration-month-format
            vnstat-configuration-top-format
            vnstat-configuration-unit-mode
            vnstat-configuration-rate-unit
            vnstat-configuration-rate-unit-mode
            vnstat-configuration-output-style
            vnstat-configuration-estimate-bar-visible
            vnstat-configuration-default-decimals
            vnstat-configuration-hourly-decimals
            vnstat-configuration-hourly-section-style
            vnstat-configuration-sampletime
            vnstat-configuration-query-mode
            vnstat-configuration-list-hours
            vnstat-configuration-list-days
            vnstat-configuration-list-months
            vnstat-configuration-list-years
            vnstat-configuration-list-top
            vnstat-configuration-interface-match-method
            vnstat-configuration-daemon-user
            vnstat-configuration-daemon-group
            vnstat-configuration-bandwidth-detection
            vnstat-configuration-max-bandwidth
            vnstat-configuration-hourly-days
            vnstat-configuration-daily-days
            vnstat-configuration-monthly-months
            vnstat-configuration-yearly-years
            vnstat-configuration-top-day-entries
            vnstat-configuration-update-interval
            vnstat-configuration-poll-interval
            vnstat-configuration-save-interval
            vnstat-configuration-offline-save-interval
            vnstat-configuration-rescan-database-on-save
            vnstat-configuration-always-add-new-interfaces
            vnstat-configuration-month-rotate
            vnstat-configuration-month-rotate-affects-years
            vnstat-configuration-check-disk-space
            vnstat-configuration-boot-variation
            vnstat-configuration-trafficless-entries
            vnstat-configuration-time-sync-wait
            vnstat-configuration-bandwidth-detection-interval
            vnstat-configuration-save-on-status-change
            vnstat-configuration-use-logging
            vnstat-configuration-create-dirs
            vnstat-configuration-update-file-owner
            vnstat-configuration-log-file
            vnstat-configuration-pid-file
            vnstat-configuration-database-write-ahead-logging
            vnstat-configuration-database-synchronous
            vnstat-configuration-header-format
            vnstat-configuration-hourly-rate
            vnstat-configuration-summary-rate
            vnstat-configuration-transparent-bg
            vnstat-configuration-large-fonts
            vnstat-configuration-line-spacing-adjustment
            vnstat-configuration-image-scale
            vnstat-configuration-hourly-graph-mode
            vnstat-configuration-summary-graph
            vnstat-configuration-estimate-style
            vnstat-configuration-bar-column-shows-rate
            vnstat-configuration-c-background
            vnstat-configuration-c-edge
            vnstat-configuration-c-header
            vnstat-configuration-c-header-title
            vnstat-configuration-c-header-date
            vnstat-configuration-c-text
            vnstat-configuration-c-line
            vnstat-configuration-c-line-l
            vnstat-configuration-c-rx
            vnstat-configuration-c-tx
            vnstat-configuration-c-rx-d
            vnstat-configuration-c-tx-d

            vnstat-service-type))

;; WIP configuration for vnstat for the guix distribution

(define-record-type* <vnstat-configuration>
  vnstat-configuration make-vnstat-configuration
  vnstat-configuration?
  (package                     vnstat-configuration-package ;file-like
                               (default vnstat))
  (name                        vnstat-configuration-name
                               (default "default"))
  (database-dir                vnstat-configuration-databasedir ;location of database directory
                               (default "/var/lib/vnstat"))
  (locale                      vnstat-configuration-locale ;(LC_ALL) ("-" = use system locale)
                               (default "-"))
  ;; date output formats for -d, -m, -t and -w
  (day-format                  vnstat-configuration-day-format
                               (default "%Y-%m-%d"))
  (month-format                vnstat-configuration-month-format
                               (default "%Y-%m"))
  (top-format                  vnstat-configuration-top-format
                               (default "%Y-%m-%d"))
  ;; characters used for visuals
  (RX-character                vnstat-configuration-RX-character
                               (default "%"))
  (TX-character                vnstat-configuration-TX-character
                               (default ":"))
  (RX-hour-character           vnstat-configuration-RX-hour-character
                               (default "r"))
  (TX-hour-character           vnstat-configuration-TX-hour-character
                               (default "t"))
  ;; how units are prefixed when traffic is shown
  ;; 0 = IEC standard prefixes (KiB/MiB/GiB...)
  ;; 1 = old style binary prefixes (KB/MB/GB...)
  ;; 2 = SI decimal prefixes (kB/MB/GB...)
  (unit-mode                   vnstat-configuration-unit-mode
                               (default 0))
  ;; used rate unit (0 = bytes, 1 = bits)
  (rate-unit                   vnstat-configuration-rate-unit
                               (default 1))
  ;; how units are prefixed when traffic rate is shown in bits
  ;; 0 = IEC binary prefixes (Kibit/s...)
  ;; 1 = SI decimal prefixes (kbit/s...)
  (rate-unit-mode              vnstat-configuration-rate-unit-mode
                               (default 1))
  ;; output style
  ;; 0 = minimal & narrow, 1 = bar column visible
  ;; 2 = same as 1 except rate in summary
  ;; 3 = rate column visible
  (output-style                vnstat-configuration-output-style
                               (default 3))
  (estimate-bar-visible        vnstat-configuration-estimate-bar-visible
                               (default 1))
  ;; number of decimals to use in outputs
  (default-decimals            vnstat-configuration-default-decimals
                               (default 2))
  (hourly-decimals             vnstat-configuration-hourly-decimals
                               (default 1))
  ;; spacer for separating hourly sections (0 = none, 1 = '|', 2 = '][', 3 = '[ ]')
  (hourly-section-style        vnstat-configuration-hourly-section-style
                               (default 2))
  ;; how many seconds should sampling for -tr take by default
  (sampletime                  vnstat-configuration-sampletime
                               (default 5))
  ;; default query mode
  ;; 0 = normal, 1 = days, 2 = months, 3 = top, 5 = short
  ;; 7 = hours, 8 = xml, 9 = one line, 10 = json
  (query-mode                  vnstat-configuration-query-mode
                               (default 0))
  ;; default list output entry limits (0 = all)
  (list5-mins                  vnstat-configuration-list5-mins
                               (default 24))
  (list-hours                  vnstat-configuration-list-hours
                               (default 24))
  (list-days                   vnstat-configuration-list-days
                               (default 30))
  (list-months                 vnstat-configuration-list-months
                               (default 12))
  (list-years                  vnstat-configuration-list-years
                               (default 0))
  (list-top                    vnstat-configuration-list-top
                               (default 10))
  ;; how to match interface given for query to interface in database
  ;; 0 = case sensitive exact match to interface name
  ;; 1 = method 0 followed by case sensitive exact match of alias
  ;; 2 = method 1 followed by case insensitive exact match of alias
  ;; 3 = method 2 followed by case insensitive beginning match of alias
  (interface-match-method      vnstat-configuration-interface-match-method
                               (default 3))
  ;; vnstatd
  ;; switch to given user when started as root (leave empty to disable)
  (daemon-user                 vnstat-configuration-daemon-user
                               (default "vnstat"))
  ;; switch to given group when started as root (leave empty to disable)
  (daemon-group                vnstat-configuration-daemon-group
                               (default "vnstat"))
  ;; try to detect interface maximum bandwidth, 0 = disable feature
  ;; MaxBandwidth will be used as fallback value when enabled
  (bandwidth-detection         vnstat-configuration-bandwidth-detection
                               (default 1))
  ;; maximum bandwidth (Mbit) for all interfaces, 0 = disable feature
  ;; (unless interface specific limit is given)
  (max-bandwidth               vnstat-configuration-max-bandwidth
                               (default 1000))
  ;; interface specific limits
  ;;  example 8Mbit limit for eth0 (remove # to activate):
  ;; #MaxBWeth0 8
  ;; data retention durations (-1 = unlimited, 0 = feature disabled)
  (5minute-hours                vnstat-configuration-5minute-hours
                                (default 48))
  (hourly-days                  vnstat-configuration-hourly-days
                                (default 4))
  (daily-days                   vnstat-configuration-daily-days
                                (default 62))
  (monthly-months               vnstat-configuration-monthly-months
                                (default 25))
  (yearly-years                 vnstat-configuration-yearly-years
                                (default -1))
  (top-day-entries              vnstat-configuration-top-day-entries
                                (default 20))
  ;; how often (in seconds) interface data is updated
  (update-interval              vnstat-configuration-update-interval
                                (default 20))
  ;; how often (in seconds) interface status changes are checked
  (poll-interval                vnstat-configuration-poll-interval
                                (default 5))
  ;; how often (in minutes) data is saved to database
  (save-interval                vnstat-configuration-save-interval
                                (default 5))
  ;; how often (in minutes) data is saved when all interface are offline
  (offline-save-interval        vnstat-configuration-offline-save-interval
                                (default 30))
  ;; rescan database after save for new interfaces to be monitored (1 = enabled, 0 = disabled)
  (rescan-database-on-save      vnstat-configuration-rescan-database-on-save
                                (default 1))
  ;; automatically start monitoring all interfaces not found in the database
  ;; (1 = enabled, 0 = disabled)
  (always-add-new-interfaces    vnstat-configuration-always-add-new-interfaces
                                (default 0))
  ;; on which day should months change
  (month-rotate                 vnstat-configuration-month-rotate
                                (default 1))
  (month-rotate-affects-years   vnstat-configuration-month-rotate-affects-years
                                (default 0))
  ;; filesystem disk space check (1 = enabled, 0 = disabled)
  (check-disk-space             vnstat-configuration-check-disk-space
                                (default 1))
  ;; how much the boot time can variate between updates (seconds)
  (boot-variation               vnstat-configuration-boot-variation
                                (default 15))
  ;; create database entries even when there is no traffic (1 = enabled, 0 = disabled)
  (trafficless-entries          vnstat-configuration-trafficless-entries
                                (default 1))
  ;; how many minutes to wait during daemon startup for system clock to
  ;; sync time if most recent database update appears to be in the future
  (time-sync-wait               vnstat-configuration-time-sync-wait
                                (default 5))
  ;; how often (in minutes) bandwidth detection is done when
  ;; BandwidthDetection is enabled (0 = disabled)
  (bandwidth-detection-interval vnstat-configuration-bandwidth-detection-interval
                                (default 5))
  ;; force data save when interface status changes (1 = enabled, 0 = disabled)
  (save-on-status-change        vnstat-configuration-save-on-status-change
                                (default 1))
  ;; enable / disable logging (0 = disabled, 1 = logfile, 2 = syslog)
  (use-logging                  vnstat-configuration-use-logging
                                (default 2))
  ;; create dirs if needed (1 = enabled, 0 = disabled)
  (create-dirs                  vnstat-configuration-create-dirs
                                (default 1))
  ;; update ownership of files if needed (1 = enabled, 0 = disabled)
  (update-file-owner            vnstat-configuration-update-file-owner
                                (default 1))
  ;; file used for logging if UseLogging is set to 1
  (log-file                     vnstat-configuration-log-file
                                (default "/var/log/vnstat/vnstat.log"))
  ;; file used as daemon pid / lock file
  (pid-file                     vnstat-configuration-pid-file
                                (default "/var/run/vnstat/vnstat.pid"))
  ;; 1 = 64-bit, 0 = 32-bit, -1 = old style logic, -2 = automatic detection
  (64bit-interface-counters     vnstat-configuration-64bit-interface-counters
                                (default -2))
  ;; use SQLite Write-Ahead Logging mode (1 = enabled, 0 = disabled)
  (database-write-ahead-logging vnstat-configuration-database-write-ahead-logging
                                (default 0))
  ;; change the setting of the SQLite "synchronous" flag
  ;; (-1 = auto, 0 = off, 1, = normal, 2 = full, 3 = extra)
  (database-synchronous         vnstat-configuration-database-synchronous
                                (default -1))
  ;; database uses UTC instead of local timezone (1 = enabled, 0 = disabled)
  (use-UTC                      vnstat-configuration-use-UTC
                                (default 0))
  ;; # vnstati
  ;; title timestamp format
  (header-format                vnstat-configuration-header-format
                                (default "%Y-%m-%d %H:%M"))
  ;; show hours with rate (1 = enabled, 0 = disabled)
  (hourly-rate                  vnstat-configuration-hourly-rate
                                (default 1))
  ;; show rate in summary (1 = enabled, 0 = disabled)
  (summary-rate                 vnstat-configuration-summary-rate
                                (default 1))
  ;; transparent background (1 = enabled, 0 = disabled)
  (transparent-bg               vnstat-configuration-transparent-bg
                                (default 0))
  ;; image size control
  (large-fonts                  vnstat-configuration-large-fonts
                                (default 0))
  (line-spacing-adjustment      vnstat-configuration-line-spacing-adjustment
                                (default 0))
  (image-scale                  vnstat-configuration-image-scale
                                (default 100))
  ;; 5 minutes graph size control
  (5-minute-graph-result-count  vnstat-configuration-5-minute-graph-result-count
                                (default 576))
  (5-minute-graph-height        vnstat-configuration-5-minute-graph-height
                                (default 300))
  ;; hourly graph mode (0 = 24 hour sliding window, 1 = begins from midnight)
  (hourly-graph-mode            vnstat-configuration-hourly-graph-mode
                                (default 0))
  ;; horizontal/vertical summary graph (0 = hours, 1 = 5 minutes)
  (summary-graph                vnstat-configuration-summary-graph
                                (default 0))
  ;; traffic estimate bar style
  ;; (0 = not shown, 1 = continuation of existing bar, 2 = separate bar)
  (estimate-style               vnstat-configuration-estimate-style
                                (default 1))
  ;; bar column in list outputs shows rate if OutputStyle is 3
  ;; (1 = enabled, 0 = disabled)
  (bar-column-shows-rate        vnstat-configuration-bar-column-shows-rate
                                (default 0))
  ;; image colors
  (c-background                 vnstat-configuration-c-background
                                (default "FFFFFF"))
  (c-edge                       vnstat-configuration-c-edge
                                (default "AEAEAE"))
  (c-header                     vnstat-configuration-c-header
                                (default "606060"))
  (c-header-title               vnstat-configuration-c-header-title
                                (default "FFFFFF"))
  (c-header-date                vnstat-configuration-c-header-date
                                (default "FFFFFF"))
  (c-text                       vnstat-configuration-c-text
                                (default "000000"))
  (c-line                       vnstat-configuration-c-line
                                (default "B0B0B0"))
  (c-line-l                     vnstat-configuration-c-line-l
                                (default "-"))
  (c-rx                         vnstat-configuration-c-rx
                                (default "92CF00"))
  (c-tx                         vnstat-configuration-c-tx
                                (default "606060"))
  (c-rx-d                       vnstat-configuration-c-rx-d
                                (default "-"))
  (c-tx-d                       vnstat-configuration-c-tx-d
                                (default "-")))

(define %vnstat-accounts
  (list (user-group
         (name "vnstat")
         (system? #t))
        (user-account
         (name "vnstat")
         (group "vnstat")
         (system? #t)
         (comment "Vnstat User")
         (home-directory "/var/vnstat")
         (shell (file-append shadow "/sbin/nologin")))))

;; (define (serialize-string field-name val)
;;   (format #f "XDG_~a_DIR=\"~a\"\n"
;;           (object->camel-case-string field-name) val))

(define vnstat-shepherd-service
  (match-lambda
    (($ <vnstat-configuration>
        package name databasedir locale day-format month-format top-format
        RX-character TX-character RX-hour-character TX-hour-character unit-mode
        rate-unit rate-unit-mode output-style estimate-bar-visible default-decimals
        hourly-decimals hourly-section-style sampletime query-mode list5-mins list-hours
        list-days list-months list-years list-top interface-match-method daemon-user
        daemon-group bandwidth-detection max-bandwidth 5minute-hours hourly-days
        daily-days monthly-months yearly-years top-day-entries update-interval
        poll-interval save-interval offline-save-interval rescan-database-on-save
        always-add-new-interfaces month-rotate month-rotate-affects-years
        check-disk-space boot-variation trafficless-entries time-sync-wait
        bandwidth-detection-interval save-on-status-change use-logging create-dirs
        update-file-owner log-file pid-file 64bit-interface-counters
        database-write-ahead-logging database-synchronous use-UTC header-format
        hourly-rate summary-rate transparent-bg large-fonts line-spacing-adjustment
        image-scale 5-minute-graph-result-count 5-minute-graph-height hourly-graph-mode
        summary-graph estimate-style bar-column-shows-rate c-background c-edge c-header
        c-header-title c-header-date c-text c-line c-line-l c-rx c-x c-rx-d c-tx-d)
     (list (shepherd-service
            (provision (list (symbol-append 'vnstat- (string->symbol name))))
            (documentation (string-append "The Vnstat Network Traffix Monitor"
                                          " (" name ")"))
            (requirement '(networking))
            (start #~(make-forkexec-constructor
                      (list #$(file-append package "/sbin/vnstatd")
                            "-n"
                            "-g" #$daemon-group
                            "-u" #$daemon-user
                            "-p" #$pid-file)
                      ;; Vnstat will drop privileges to the "vnstat" user when
                      ;; it exists.  Not passing #:user here allows the service
                      ;; to bind to ports < 1024.
                      #:pid-file (if (string-prefix? "/" #$name)
                                     (string-append #$name "/_.pid")
                                     (string-append "/var/vnstat/" #$name "/_.pid"))))
            (stop #~(make-kill-destructor)))))))

(define vnstat-service-type
  (service-type
   (name 'vnstat)
   (description "Run the Vnstat network traffic monitor.")
   (extensions
    (list (service-extension account-service-type
                             (const %vnstat-accounts))
          (service-extension shepherd-root-service-type
                             vnstat-shepherd-service)))
   (default-value
     (vnstat-configuration))))
