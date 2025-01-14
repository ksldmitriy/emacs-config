(require 'toml)

 (let ((local-config
       (toml:read-from-file
        (expand-file-name "local-config.toml" user-emacs-directory))))
  (message (format "config: %s" local-config)))
