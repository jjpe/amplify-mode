(require 'amplify-core)

;; Monto legacy
(amplify/defprocess brokers
  (concat
   ;; Change dir because Monto loads the settings from a file relative
   ;; to the "current" directory:
   "cd " spoofax/amplify-current-dir "; "
   ;; Start the binary:
   spoofax/amplify-current-dir
   "monto-" spoofax/amplify-current-version "-" amplify/current-os " broker"))

(amplify/defprocess brokers-debug
  (concat
   ;; Change dir because Monto loads the settings from a file relative
   ;; to the "current" directory:
   "cd " spoofax/amplify-current-dir "; "
   ;; Start the binary:
   spoofax/amplify-current-dir
   "monto-" spoofax/amplify-current-version "-" amplify/current-os "-dbg broker"))

(provide 'amplify-legacy)
;;; amplify-legacy ends here
