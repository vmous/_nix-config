;;;;;; secrets.el
;;;;
;; File containing sensitive information. All variables are published as empty
;; strings. Documentation provided in order to be easily reproducible in case of
;; loss.

;; org-gcal
(defconst jazzy/secrets/org/gcal/client-id ""
  "The Google Calendar client id to be used by org gcal. To generate or find out the value of this variable check the installation guide at https://github.com/myuhe/org-gcal.el.")
(defconst jazzy/secrets/org/gcal/client-secret ""
  "The Google Calendar client secret to be used by org gcal. To generate or find out the value of this variable check the installation guide at https://github.com/myuhe/org-gcal.el.")
(defconst jazzy/secrets/org/gcal/calendar-primary ""
  "The primary Google Calendar to be used by org-gcal. Most of the time, this value is your GMail e-mail address.")
(defconst jazzy/secrets/org/gcal/url-primary ""
  "The URL of the primary Google Calendar to be used by org-gcal. To find the URL value for any calendar follow this path: Google Calendar -> Gear on top right -> Settings -> Settings for my calendars (on the left pane) -> Calendar name -> Integrate Calendar (on the main pane) -> Secret address in iCal format.")
(defconst jazzy/secrets/org/gcal/url-de-holidays ""
  "Google Calendar containing holidays for Germany. To find the URL value for any calendar follow this path: Google Calendar -> Gear on top right -> Settings -> Settings for my calendars (on the left pane) -> Calendar name -> Integrate Calendar (on the main pane) -> Secret address in iCal format.")
(defconst jazzy/secrets/org/gcal/url-gr-holidays ""
  "Google Calendar containing holidays for Greece. To find the URL value for any calendar follow this path: Google Calendar -> Gear on top right -> Settings -> Settings for my calendars (on the left pane) -> Calendar name -> Integrate Calendar (on the main pane) -> Secret address in iCal format.")
(defconst jazzy/secrets/org/gcal/url-gr-names ""
  "Google Calendar containing name days for greek names. To find the URL value for any calendar follow this path: Google Calendar -> Gear on top right -> Settings -> Settings for my calendars (on the left pane) -> Calendar name -> Integrate Calendar (on the main pane) -> Secret address in iCal format.")

;; system-name
(defconst jazzy/secrets/system-name/macosx ""
  "The system name of my Mac OS X system. Verify with 'C-h v system-name RET'.")
(defconst jazzy/secrets/system-name/linux ""
  "The system name of my personal Linux system. Verify with 'C-h v system-name RET'.")
(defconst jazzy/secrets/system-name/amznlinux ""
  "The system name of my personal Linux system. Verify with 'C-h v system-name RET'.")
