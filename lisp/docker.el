(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

(setenv "DOCKER_TLS_VERIFY" "1")
(setenv "DOCKER_HOST" "tcp://allocsoc.net:2376")
(setenv "DOCKER_CERT_PATH" "/Users/jordicoscolla/.docker/machine/machines/cloud")
(setenv "DOCKER_MACHINE_NAME" "cloud")
