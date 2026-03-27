;;; sd-proof-manager.el  -*- lexical-binding:t -*-

(cl-defstruct proof-manager
  setup-hook
  current-goal
  current-hypotheses)

(provide 'sd-proof-manager)
