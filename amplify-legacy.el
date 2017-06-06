(require 'amplify-core)
(require 'amplify-upgrade)

;; Monto legacy
(amplify/defprocess brokers
  (concat
   ;; Change dir because Monto loads the settings from a file relative
   ;; to the "current" directory:
   "cd " spoofax/amplify-current-dir "; "
   ;; Start the binary:
   spoofax/amplify-current-dir
   "monto-" spoofax/amplify-current-version "-osx broker"))

(amplify/defprocess brokers-debug
  (concat
   ;; Change dir because Monto loads the settings from a file relative
   ;; to the "current" directory:
   "cd " spoofax/amplify-current-dir "; "
   ;; Start the binary:
   spoofax/amplify-current-dir
   "monto-" spoofax/amplify-current-version "-osx-dbg broker"))
